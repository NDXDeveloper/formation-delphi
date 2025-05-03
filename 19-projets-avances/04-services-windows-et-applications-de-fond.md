# 19.4 Services Windows et applications de fond

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Les services Windows sont des applications spéciales qui s'exécutent en arrière-plan, sans interface utilisateur visible. Ils sont parfaits pour des tâches qui doivent fonctionner en permanence, même lorsque aucun utilisateur n'est connecté à l'ordinateur. Dans ce chapitre, nous allons apprendre à créer des services Windows avec Delphi et explorer comment développer des applications qui travaillent en arrière-plan.

## Cas d'utilisation typiques

Les services Windows sont idéaux pour de nombreux scénarios :

- Surveillance de systèmes
- Tâches planifiées récurrentes
- Gestion de bases de données
- Communication réseau continue
- Synchronisation de données
- Traitement de données en arrière-plan
- Serveurs d'applications

## Prérequis

- Delphi 11 Alexandria ou Delphi 12 Athens
- Connaissance de base de Delphi et du langage Object Pascal
- Droits administratifs sur votre système Windows (pour installer et tester les services)

## 1. Comprendre les services Windows

### Qu'est-ce qu'un service Windows ?

Un service Windows est un programme qui démarre automatiquement avec Windows et s'exécute en arrière-plan. Contrairement aux applications normales, les services :

- N'ont pas d'interface utilisateur traditionnelle
- Peuvent démarrer avant qu'un utilisateur ne se connecte
- Continuent à s'exécuter même après la déconnexion de l'utilisateur
- Sont gérés via le gestionnaire de services Windows
- Peuvent être configurés pour redémarrer automatiquement en cas d'échec

### Cycle de vie d'un service

Un service Windows possède plusieurs états :

1. **Installation** : Enregistrement du service auprès de Windows
2. **Démarrage** : Initialisation du service
3. **Exécution** : Phase principale où le service effectue ses tâches
4. **Pause** : État temporaire où le service est inactif mais prêt à reprendre
5. **Arrêt** : Nettoyage des ressources et fin d'exécution
6. **Désinstallation** : Suppression du service du système

### Types de services Windows

Il existe principalement deux types de services :

- **Services interactifs** : Peuvent interagir avec le bureau (rarement utilisés dans les versions récentes de Windows)
- **Services non-interactifs** : S'exécutent sans accès au bureau (le type le plus courant)

## 2. Création d'un service Windows simple

Commençons par créer un service Windows basique qui écrira des entrées dans un fichier journal à intervalles réguliers.

### 2.1 Créer un nouveau projet de service

1. Lancez Delphi et sélectionnez **Fichier > Nouveau > Autre**.
2. Dans la boîte de dialogue, allez dans **Delphi Projects > Service** et choisissez **Service Application**.
3. Cliquez sur **OK**.

Delphi créera un projet de service avec une unité contenant une classe de service prête à être personnalisée.

### 2.2 Personnaliser le service

L'unité de service créée par Delphi ressemble à ceci :

```pascal
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  Service1: TService1;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := True;
end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Stopped := True;
end;
```

Maintenant, modifions cette unité pour notre service de journalisation :

1. Renommez l'unité en `LoggerServiceUnit` et la classe en `TLoggerService`.
2. Configurez les propriétés du service dans l'éditeur de formulaire :
   - **Name** : `LoggerService`
   - **DisplayName** : `Service de journalisation`
   - **Description** : `Service qui écrit des entrées dans un journal à intervalles réguliers`

3. Ajoutez un `TTimer` depuis la palette de composants et configurez-le :
   - **Name** : `LogTimer`
   - **Interval** : `60000` (1 minute)
   - **Enabled** : `False`

4. Modifiez le code comme suit :

```pascal
unit LoggerServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TLoggerService = class(TService)
    LogTimer: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure LogTimerTimer(Sender: TObject);
  private
    FLogFile: string;
    procedure WriteToLog(const Message: string);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  LoggerService: TLoggerService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  LoggerService.Controller(CtrlCode);
end;

function TLoggerService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TLoggerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Définir le chemin du fichier journal
  FLogFile := ExtractFilePath(ParamStr(0)) + 'service_log.txt';

  // Écrire un message au démarrage
  WriteToLog('Service démarré à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Démarrer le timer
  LogTimer.Enabled := True;

  Started := True;
end;

procedure TLoggerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  // Arrêter le timer
  LogTimer.Enabled := False;

  // Écrire un message d'arrêt
  WriteToLog('Service arrêté à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  Stopped := True;
end;

procedure TLoggerService.LogTimerTimer(Sender: TObject);
begin
  // Écrire un message périodique
  WriteToLog('Entrée de journal à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
end;

procedure TLoggerService.WriteToLog(const Message: string);
var
  LogStream: TStreamWriter;
begin
  try
    // Créer ou ouvrir le fichier en mode append
    if FileExists(FLogFile) then
      LogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8)
    else
      LogStream := TStreamWriter.Create(FLogFile, False, TEncoding.UTF8);

    try
      LogStream.WriteLine(Message);
    finally
      LogStream.Free;
    end;
  except
    on E: Exception do
      // Dans un service réel, il faudrait gérer cette erreur correctement
      // car on ne peut pas afficher de boîte de dialogue
  end;
end;
```

5. Double-cliquez sur l'événement `OnTimer` du `LogTimer` et associez-le à la méthode `LogTimerTimer`.

### 2.3 Configuration du projet de service

Modifions maintenant le fichier projet pour configurer correctement notre service :

```pascal
program LoggerService;

uses
  Vcl.SvcMgr,
  LoggerServiceUnit in 'LoggerServiceUnit.pas' {LoggerService: TService};

{$R *.res}

begin
  // Windows 2003 Server nécessite StartServiceCtrlDispatcher pour être
  // appelé avant l'initialisation de CoInitialize
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TLoggerService, LoggerService);
  Application.Run;
end.
```

### 2.4 Compilation du service

Compilez le projet en appuyant sur **F9** ou en sélectionnant **Projet > Compiler LoggerService**.

## 3. Installation et gestion d'un service Windows

Une fois le service compilé, nous devons l'installer dans le système Windows.

### 3.1 Installation du service

Pour installer un service, vous devez exécuter l'application avec le paramètre `/install`. Cela nécessite des droits administratifs :

1. Ouvrez une invite de commande avec des droits administratifs
2. Naviguez jusqu'au dossier contenant votre fichier `.exe` compilé
3. Exécutez la commande : `LoggerService.exe /install`

Si tout se passe bien, vous verrez un message confirmant l'installation du service.

### 3.2 Gestion du service via le gestionnaire de services Windows

Une fois installé, vous pouvez gérer votre service via le gestionnaire de services Windows :

1. Appuyez sur **Win+R**, tapez `services.msc` et appuyez sur Entrée
2. Localisez votre service "Service de journalisation" dans la liste
3. Utilisez le menu contextuel (clic droit) pour :
   - Démarrer le service
   - Arrêter le service
   - Redémarrer le service
   - Modifier les propriétés du service

### 3.3 Désinstallation du service

Pour désinstaller le service :

1. Assurez-vous que le service est arrêté
2. Ouvrez une invite de commande avec des droits administratifs
3. Exécutez : `LoggerService.exe /uninstall`

### 3.4 Déboguer un service Windows

Le débogage des services peut être compliqué car ils s'exécutent en contexte système. Voici quelques approches :

#### Option 1 : Mode application

Modifiez votre projet pour qu'il puisse s'exécuter comme une application normale pendant le développement :

```pascal
program LoggerService;

uses
  Vcl.SvcMgr,
  Vcl.Forms,
  LoggerServiceUnit in 'LoggerServiceUnit.pas' {LoggerService: TService};

{$R *.res}

begin
  // Vérifier les paramètres pour déterminer le mode d'exécution
  if (ParamCount > 0) and
     ((ParamStr(1) = '/install') or (ParamStr(1) = '/uninstall')) then
  begin
    // Mode service
    if not Application.DelayInitialize or Application.Installing then
      Application.Initialize;
    Application.CreateForm(TLoggerService, LoggerService);
    Application.Run;
  end
  else
  begin
    // Mode application pour le débogage
    Application.Initialize;
    Application.CreateForm(TLoggerService, LoggerService);
    // Simuler le démarrage du service
    LoggerService.ServiceStart(LoggerService, True);

    // Créer une simple form pour maintenir l'application en vie
    with TForm.Create(Application) do
    begin
      Caption := 'Service en mode débogage';
      Width := 400;
      Height := 150;

      with TButton.Create(Application) do
      begin
        Parent := TWinControl(Application.MainForm);
        Caption := 'Arrêter';
        Left := 150;
        Top := 50;
        Width := 100;
        OnClick := procedure(Sender: TObject)
        begin
          LoggerService.ServiceStop(LoggerService, True);
          Application.Terminate;
        end;
      end;

      Application.Run;
      Free;
    end;
  end;
end.
```

#### Option 2 : Journalisation extensive

Utilisez une journalisation détaillée dans toutes les parties critiques du service pour pouvoir analyser son comportement après exécution.

## 4. Fonctionnalités avancées des services

Explorons maintenant quelques fonctionnalités plus avancées pour nos services.

### 4.1 Gestion de la pause et de la reprise

Nous pouvons ajouter la prise en charge des commandes de pause et de reprise :

```pascal
procedure TLoggerService.ServicePause(Sender: TService; var Paused: Boolean);
begin
  // Mettre en pause les activités du service
  LogTimer.Enabled := False;

  WriteToLog('Service mis en pause à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  Paused := True;
end;

procedure TLoggerService.ServiceContinue(Sender: TService; var Continued: Boolean);
begin
  // Reprendre les activités du service
  LogTimer.Enabled := True;

  WriteToLog('Service repris à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  Continued := True;
end;
```

### 4.2 Gestion de l'arrêt du système

Nous pouvons également gérer correctement l'arrêt du système :

```pascal
procedure TLoggerService.ServiceShutdown(Sender: TService);
begin
  // Nettoyer les ressources lors de l'arrêt du système
  LogTimer.Enabled := False;

  WriteToLog('Service arrêté suite à l''arrêt du système à ' +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
end;
```

### 4.3 Configuration et paramètres du service

Pour rendre notre service plus flexible, ajoutons des paramètres configurables :

```pascal
type
  TLoggerService = class(TService)
    // ... autres déclarations
  private
    FLogFile: string;
    FLogInterval: Integer;
    procedure WriteToLog(const Message: string);
    procedure LoadSettings;
    procedure SaveSettings;
  public
    // ... autres méthodes
  end;

procedure TLoggerService.LoadSettings;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('SOFTWARE\YourCompany\LoggerService', False) then
    begin
      if Registry.ValueExists('LogFile') then
        FLogFile := Registry.ReadString('LogFile')
      else
        FLogFile := ExtractFilePath(ParamStr(0)) + 'service_log.txt';

      if Registry.ValueExists('LogInterval') then
        FLogInterval := Registry.ReadInteger('LogInterval')
      else
        FLogInterval := 60000; // 1 minute par défaut

      Registry.CloseKey;
    end
    else
    begin
      // Utiliser les valeurs par défaut
      FLogFile := ExtractFilePath(ParamStr(0)) + 'service_log.txt';
      FLogInterval := 60000;
    end;
  finally
    Registry.Free;
  end;

  // Appliquer les paramètres
  LogTimer.Interval := FLogInterval;
end;

procedure TLoggerService.SaveSettings;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('SOFTWARE\YourCompany\LoggerService', True) then
    begin
      Registry.WriteString('LogFile', FLogFile);
      Registry.WriteInteger('LogInterval', FLogInterval);
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TLoggerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Charger les paramètres
  LoadSettings;

  // Écrire un message au démarrage
  WriteToLog('Service démarré à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Démarrer le timer
  LogTimer.Enabled := True;

  Started := True;
end;
```

### 4.4 Communication avec le service

Il est souvent utile de pouvoir communiquer avec un service en cours d'exécution. Voici comment implémenter une communication simple basée sur des messages Windows :

```pascal
const
  WM_USER_SERVICE = WM_USER + 1;

  // Commandes personnalisées
  CMD_UPDATE_INTERVAL = 1;
  CMD_WRITE_CUSTOM_LOG = 2;

type
  TLoggerService = class(TService)
    // ... autres déclarations
  private
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
    // ... autres méthodes
  public
    // ... autres méthodes
  end;

procedure TLoggerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Charger les paramètres
  LoadSettings;

  // Créer une fenêtre cachée pour recevoir des messages
  FWindowHandle := AllocateHWnd(WndProc);

  // ... reste du code de démarrage
end;

procedure TLoggerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  // Libérer la fenêtre cachée
  if FWindowHandle <> 0 then
  begin
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;

  // ... reste du code d'arrêt
end;

procedure TLoggerService.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_USER_SERVICE then
  begin
    case Msg.WParam of
      CMD_UPDATE_INTERVAL:
        begin
          // Mettre à jour l'intervalle du timer
          FLogInterval := Msg.LParam;
          LogTimer.Interval := FLogInterval;
          SaveSettings;
          WriteToLog('Intervalle mis à jour à ' + IntToStr(FLogInterval) + ' ms');
        end;

      CMD_WRITE_CUSTOM_LOG:
        begin
          // Écrire un message personnalisé
          if Msg.LParam <> 0 then
          begin
            WriteToLog(PChar(Msg.LParam));
            // Libérer la mémoire allouée par l'expéditeur
            FreeMem(Pointer(Msg.LParam));
          end;
        end;
    end;
    Msg.Result := 1; // Message traité
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;
```

## 5. Création d'une application cliente pour le service

Pour interagir avec notre service, créons une simple application cliente :

1. Créez une nouvelle application VCL (Fichier > Nouveau > Application VCL)

2. Ajoutez les contrôles suivants au formulaire principal :
   - Un `TButton` nommé `btnUpdateInterval` avec Caption "Mettre à jour l'intervalle"
   - Un `TEdit` nommé `edtInterval` avec Text "60000"
   - Un `TButton` nommé `btnCustomLog` avec Caption "Écrire message personnalisé"
   - Un `TEdit` nommé `edtCustomMessage` avec Text "Message personnalisé"
   - Un `TButton` nommé `btnStartService` avec Caption "Démarrer le service"
   - Un `TButton` nommé `btnStopService` avec Caption "Arrêter le service"

3. Ajoutez le code suivant :

```pascal
unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Winapi.WinSvc;

type
  TMainForm = class(TForm)
    btnUpdateInterval: TButton;
    edtInterval: TEdit;
    btnCustomLog: TButton;
    edtCustomMessage: TEdit;
    btnStartService: TButton;
    btnStopService: TButton;
    procedure btnUpdateIntervalClick(Sender: TObject);
    procedure btnCustomLogClick(Sender: TObject);
    procedure btnStartServiceClick(Sender: TObject);
    procedure btnStopServiceClick(Sender: TObject);
  private
    function FindServiceWindow: HWND;
    function StartService(const ServiceName: string): Boolean;
    function StopService(const ServiceName: string): Boolean;
  public
    { Public declarations }
  end;

const
  SERVICE_NAME = 'LoggerService';
  SERVICE_WINDOW_CLASS = 'TServiceWindowClass';

  WM_USER_SERVICE = WM_USER + 1;
  CMD_UPDATE_INTERVAL = 1;
  CMD_WRITE_CUSTOM_LOG = 2;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function TMainForm.FindServiceWindow: HWND;
var
  WindowClass: array[0..255] of Char;
begin
  Result := FindWindow(nil, 'LoggerService');
  if Result = 0 then
  begin
    // Essayer de trouver par classe
    StrPCopy(WindowClass, SERVICE_WINDOW_CLASS);
    Result := FindWindow(WindowClass, nil);
  end;
end;

function TMainForm.StartService(const ServiceName: string): Boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: TServiceStatus;
  Timeout: Cardinal;
begin
  Result := False;

  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
  begin
    ShowMessage('Erreur lors de l''ouverture du gestionnaire de services.' +
      ' Assurez-vous d''exécuter l''application en tant qu''administrateur.');
    Exit;
  end;

  try
    // Ouvrir le service
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    if Service = 0 then
    begin
      ShowMessage('Erreur lors de l''ouverture du service ' + ServiceName);
      Exit;
    end;

    try
      // Vérifier si le service est déjà en cours d'exécution
      if not QueryServiceStatus(Service, Status) then
      begin
        ShowMessage('Impossible d''obtenir le statut du service');
        Exit;
      end;

      // Si le service est déjà en cours d'exécution, retourner succès
      if Status.dwCurrentState = SERVICE_RUNNING then
      begin
        Result := True;
        Exit;
      end;

      // Démarrer le service
      if not StartService(Service, 0, nil) then
      begin
        ShowMessage('Erreur lors du démarrage du service');
        Exit;
      end;

      // Attendre que le service démarre (avec timeout)
      Timeout := GetTickCount + 10000; // 10 secondes de timeout

      repeat
        // Vérifier périodiquement le statut
        if not QueryServiceStatus(Service, Status) then
          Break;

        if Status.dwCurrentState = SERVICE_RUNNING then
        begin
          Result := True;
          Break;
        end;

        // Éviter de consommer trop de CPU
        Sleep(100);
      until GetTickCount > Timeout;

      if not Result then
        ShowMessage('Le service n''a pas démarré dans le délai imparti');
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function TMainForm.StopService(const ServiceName: string): Boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: TServiceStatus;
  Timeout: Cardinal;
begin
  Result := False;

  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
  begin
    ShowMessage('Erreur lors de l''ouverture du gestionnaire de services.' +
      ' Assurez-vous d''exécuter l''application en tant qu''administrateur.');
    Exit;
  end;

  try
    // Ouvrir le service
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    if Service = 0 then
    begin
      ShowMessage('Erreur lors de l''ouverture du service ' + ServiceName);
      Exit;
    end;

    try
      // Vérifier si le service est en cours d'exécution
      if not QueryServiceStatus(Service, Status) then
      begin
        ShowMessage('Impossible d''obtenir le statut du service');
        Exit;
      end;

      // Si le service n'est pas en cours d'exécution, retourner succès
      if Status.dwCurrentState = SERVICE_STOPPED then
      begin
        Result := True;
        Exit;
      end;

      // Arrêter le service
      if not ControlService(Service, SERVICE_CONTROL_STOP, Status) then
      begin
        ShowMessage('Erreur lors de l''arrêt du service');
        Exit;
      end;

      // Attendre que le service s'arrête (avec timeout)
      Timeout := GetTickCount + 10000; // 10 secondes de timeout

      repeat
        // Vérifier périodiquement le statut
        if not QueryServiceStatus(Service, Status) then
          Break;

        if Status.dwCurrentState = SERVICE_STOPPED then
        begin
          Result := True;
          Break;
        end;

        // Éviter de consommer trop de CPU
        Sleep(100);
      until GetTickCount > Timeout;

      if not Result then
        ShowMessage('Le service ne s''est pas arrêté dans le délai imparti');
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure TMainForm.btnStartServiceClick(Sender: TObject);
begin
  if StartService(SERVICE_NAME) then
    ShowMessage('Service démarré avec succès')
  else
    ShowMessage('Échec du démarrage du service');
end;

procedure TMainForm.btnStopServiceClick(Sender: TObject);
begin
  if StopService(SERVICE_NAME) then
    ShowMessage('Service arrêté avec succès')
  else
    ShowMessage('Échec de l''arrêt du service');
end;

procedure TMainForm.btnUpdateIntervalClick(Sender: TObject);
var
  Interval: Integer;
  ServiceWindow: HWND;
begin
  // Valider l'intervalle
  try
    Interval := StrToInt(edtInterval.Text);
    if (Interval < 1000) or (Interval > 3600000) then // entre 1s et 1h
    begin
      ShowMessage('L''intervalle doit être compris entre 1000 et 3600000 ms');
      Exit;
    end;
  except
    ShowMessage('Veuillez entrer un intervalle valide');
    Exit;
  end;

  // Trouver la fenêtre du service
  ServiceWindow := FindServiceWindow;
  if ServiceWindow = 0 then
  begin
    ShowMessage('Service non trouvé ou non démarré');
    Exit;
  end;

  // Envoyer le message de mise à jour de l'intervalle
  if SendMessage(ServiceWindow, WM_USER_SERVICE, CMD_UPDATE_INTERVAL, Interval) <> 1 then
    ShowMessage('Erreur lors de l''envoi du message au service')
  else
    ShowMessage('Intervalle mis à jour avec succès');
end;

procedure TMainForm.btnCustomLogClick(Sender: TObject);
var
  CustomMessage: PChar;
  MessageLen: Integer;
  ServiceWindow: HWND;
begin
  // Vérifier si le message est vide
  if Trim(edtCustomMessage.Text) = '' then
  begin
    ShowMessage('Veuillez entrer un message');
    Exit;
  end;

  // Trouver la fenêtre du service
  ServiceWindow := FindServiceWindow;
  if ServiceWindow = 0 then
  begin
    ShowMessage('Service non trouvé ou non démarré');
    Exit;
  end;

  // Allouer de la mémoire pour le message
  MessageLen := Length(edtCustomMessage.Text) + 1;
  CustomMessage := AllocMem(MessageLen * SizeOf(Char));
  try
    StrPCopy(CustomMessage, edtCustomMessage.Text);

    // Envoyer le message personnalisé
    if SendMessage(ServiceWindow, WM_USER_SERVICE, CMD_WRITE_CUSTOM_LOG, LPARAM(CustomMessage)) <> 1 then
    begin
      ShowMessage('Erreur lors de l''envoi du message au service');
      // Libérer la mémoire en cas d'échec
      FreeMem(CustomMessage);
    end
    else
      ShowMessage('Message envoyé avec succès au service');
  except
    FreeMem(CustomMessage);
    raise;
  end;
end;

end.
```

## 6. Bonnes pratiques pour les services Windows

### 6.1 Gestion des erreurs

Les services ne peuvent pas afficher de boîtes de dialogue ou interagir avec l'utilisateur. Il est donc essentiel de mettre en place une gestion d'erreurs robuste :

- Utilisez toujours des blocs try-except et try-finally
- Journalisez toutes les erreurs dans un fichier journal ou dans le journal d'événements Windows
- Implémentez un mécanisme de reprise après erreur

```pascal
procedure TLoggerService.WriteToEventLog(const Msg: string; EventType: DWord);
var
  EventSource: THandle;
  StringPtrs: array[0..0] of PChar;
begin
  // Ouvrir ou créer une source d'événement
  EventSource := RegisterEventSource(nil, PChar(Name));
  if EventSource <> 0 then
  begin
    try
      StringPtrs[0] := PChar(Msg);
      // Écrire dans le journal d'événements de Windows
      ReportEvent(
        EventSource,            // handle de la source
        EventType,              // type d'événement (EVENTLOG_ERROR_TYPE, EVENTLOG_WARNING_TYPE, etc.)
        0,                      // catégorie
        0,                      // ID d'événement
        nil,                    // SID utilisateur
        1,                      // nombre de chaînes
        0,                      // taille des données binaires
        @StringPtrs,            // tableau de chaînes
        nil                     // données binaires
      );
    finally
      DeregisterEventSource(EventSource);
    end;
  end;
end;

procedure TLoggerService.HandleException(E: Exception);
begin
  // Enregistrer l'erreur dans notre fichier journal
  WriteToLog('ERREUR: ' + E.Message);

  // Enregistrer également dans le journal d'événements Windows
  WriteToEventLog('Erreur dans le service ' + DisplayName + ': ' + E.Message,
    EVENTLOG_ERROR_TYPE);
end;
```

### 6.2 Gestion des ressources

Les services peuvent s'exécuter pendant de longues périodes. Il est important de gérer correctement les ressources :

- Libérez toutes les ressources allouées
- Évitez les fuites de mémoire
- Utilisez des mécanismes périodiques de nettoyage

```pascal
procedure TLoggerService.PerformPeriodicCleanup;
var
  LogFileSize: Int64;
  BackupFileName: string;
begin
  try
    // Vérifier la taille du fichier journal
    if FileExists(FLogFile) then
    begin
      LogFileSize := TFile.GetSize(FLogFile);

      // Si le fichier dépasse 5 Mo, faire une rotation
      if LogFileSize > 5 * 1024 * 1024 then
      begin
        // Créer un nom pour la sauvegarde basé sur la date
        BackupFileName := ChangeFileExt(FLogFile,
          FormatDateTime('_yyyymmdd_hhnnss', Now) + ExtractFileExt(FLogFile));

        // Déplacer le fichier actuel vers la sauvegarde
        if TFile.Exists(BackupFileName) then
          TFile.Delete(BackupFileName);

        TFile.Move(FLogFile, BackupFileName);

        WriteToLog('Rotation du fichier journal. Ancien fichier sauvegardé en: ' + BackupFileName);
      end;
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;
```

### 6.3 Sécurité des services

Les services Windows s'exécutent généralement avec des privilèges élevés. La sécurité est donc primordiale :

- Utilisez le principe du moindre privilège (exécutez votre service avec un compte disposant uniquement des droits nécessaires)
- Sécurisez les fichiers et ressources utilisés par le service
- Validez toutes les entrées, en particulier celles provenant de sources externes

```pascal
procedure TLoggerService.InitializeWithSecureDefaults;
var
  LogDirectory: string;
  SecurityAttributes: TSecurityAttributes;
  SecurityDescriptor: TSecurityDescriptor;
  ACL: TACL;
begin
  // Initialiser le descripteur de sécurité
  InitializeSecurityDescriptor(@SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);

  // Configurer les permissions (code simplifié)
  // Dans un environnement de production, vous devriez configurer des ACL détaillées
  SetSecurityDescriptorDacl(@SecurityDescriptor, True, nil, False);

  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.lpSecurityDescriptor := @SecurityDescriptor;
  SecurityAttributes.bInheritHandle := False;

  // Créer un répertoire sécurisé pour les fichiers du service
  LogDirectory := ExtractFilePath(ParamStr(0)) + 'Logs';
  if not DirectoryExists(LogDirectory) then
    CreateDirectory(PChar(LogDirectory), @SecurityAttributes);

  // Définir le chemin du fichier journal
  FLogFile := LogDirectory + '\service_log.txt';
end;
```

## 7. Applications Windows en arrière-plan

En plus des services Windows, vous pouvez également créer des applications qui s'exécutent en arrière-plan. Ces applications sont des programmes Windows normaux qui minimisent dans la zone de notification (systray) au lieu de la barre des tâches.

### 7.1 Création d'une application avec icône dans le systray

Créons une application qui s'exécute en arrière-plan et affiche une icône dans la zone de notification :

1. Créez une nouvelle application VCL (Fichier > Nouveau > Application VCL)

2. Ajoutez un composant `TTrayIcon` depuis la palette

3. Configurez les propriétés du `TTrayIcon` :
   - **Name** : `TrayIcon1`
   - **Visible** : `True`
   - **Hint** : `Application en arrière-plan`
   - **Icon** : Choisissez une icône appropriée

4. Ajoutez un composant `TPopupMenu` et associez-le à la propriété `PopupMenu` du `TTrayIcon`

5. Ajoutez des éléments au menu contextuel :
   - "Afficher" avec le gestionnaire d'événement `ShowMainForm`
   - "Démarrer la surveillance" avec le gestionnaire d'événement `StartMonitoring`
   - "Arrêter la surveillance" avec le gestionnaire d'événement `StopMonitoring`
   - "-" (séparateur)
   - "Quitter" avec le gestionnaire d'événement `QuitApplication`

6. Modifiez le code du formulaire comme suit :

```pascal
unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    miShow: TMenuItem;
    miStartMonitoring: TMenuItem;
    miStopMonitoring: TMenuItem;
    Separator1: TMenuItem;
    miQuit: TMenuItem;
    StatusMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure miStartMonitoringClick(Sender: TObject);
    procedure miStopMonitoringClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FMonitoringActive: Boolean;
    FMonitoringTimer: TTimer;
    procedure StartMonitoring;
    procedure StopMonitoring;
    procedure AddLogMessage(const Msg: string);
    procedure MonitoringTimerEvent(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser le timer mais ne pas l'activer
  FMonitoringTimer := TTimer.Create(Self);
  FMonitoringTimer.Interval := 5000; // 5 secondes
  FMonitoringTimer.Enabled := False;
  FMonitoringTimer.OnTimer := MonitoringTimerEvent;

  FMonitoringActive := False;

  // Configurer le mémo pour les journaux
  StatusMemo.Clear;
  AddLogMessage('Application démarrée à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Lors de la tentative de fermeture, minimiser plutôt que fermer
  if not (fsShiftState in KeyboardStateToShiftState) then
  begin
    CanClose := False;
    Hide;
    TrayIcon1.ShowBalloonHint('Application minimisée',
      'L''application continue à s''exécuter en arrière-plan. ' +
      'Cliquez sur l''icône pour la restaurer.', btInfo, 10);
  end;
end;

procedure TMainForm.miShowClick(Sender: TObject);
begin
  // Afficher la fenêtre principale
  Show;
  WindowState := wsNormal;
  Application.BringToFront;
end;

procedure TMainForm.miStartMonitoringClick(Sender: TObject);
begin
  StartMonitoring;
end;

procedure TMainForm.miStopMonitoringClick(Sender: TObject);
begin
  StopMonitoring;
end;

procedure TMainForm.miQuitClick(Sender: TObject);
begin
  // Quitter l'application
  StopMonitoring;
  Application.Terminate;
end;

procedure TMainForm.StartMonitoring;
begin
  if not FMonitoringActive then
  begin
    FMonitoringTimer.Enabled := True;
    FMonitoringActive := True;
    AddLogMessage('Surveillance démarrée');

    // Mettre à jour l'état du menu
    miStartMonitoring.Enabled := False;
    miStopMonitoring.Enabled := True;

    // Notifier l'utilisateur
    TrayIcon1.ShowBalloonHint('Surveillance active',
      'La surveillance en arrière-plan est maintenant active.',
      btInfo, 5);
  end;
end;

procedure TMainForm.StopMonitoring;
begin
  if FMonitoringActive then
  begin
    FMonitoringTimer.Enabled := False;
    FMonitoringActive := False;
    AddLogMessage('Surveillance arrêtée');

    // Mettre à jour l'état du menu
    miStartMonitoring.Enabled := True;
    miStopMonitoring.Enabled := False;
  end;
end;

procedure TMainForm.AddLogMessage(const Msg: string);
begin
  StatusMemo.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + Msg);
end;

procedure TMainForm.MonitoringTimerEvent(Sender: TObject);
var
  CPUUsage: Double;
  MemoryStatus: TMemoryStatusEx;
begin
  try
    // Simuler la surveillance des ressources système
    // Dans une application réelle, vous utiliseriez les API appropriées

    // Exemple simplifié pour l'utilisation de la mémoire
    MemoryStatus.dwLength := SizeOf(MemoryStatus);
    if GlobalMemoryStatusEx(MemoryStatus) then
    begin
      AddLogMessage(Format('Mémoire utilisée: %.1f%%', [MemoryStatus.dwMemoryLoad * 1.0]));
    end;

    // Simuler une valeur aléatoire pour l'utilisation du CPU
    CPUUsage := Random * 100;
    AddLogMessage(Format('CPU utilisé: %.1f%%', [CPUUsage]));

    // Vérifier si des alertes sont nécessaires
    if CPUUsage > 90 then
    begin
      TrayIcon1.ShowBalloonHint('Alerte - CPU élevé',
        Format('Utilisation du CPU à %.1f%%', [CPUUsage]),
        btWarning, 10);
    end;

    if MemoryStatus.dwMemoryLoad > 90 then
    begin
      TrayIcon1.ShowBalloonHint('Alerte - Mémoire faible',
        Format('Utilisation de la mémoire à %.1f%%', [MemoryStatus.dwMemoryLoad * 1.0]),
        btWarning, 10);
    end;
  except
    on E: Exception do
      AddLogMessage('Erreur lors de la surveillance: ' + E.Message);
  end;
end;

// Dans le fichier .dpr, ajoutez ce code pour minimiser l'application au démarrage
initialization
  Application.ShowMainForm := False;
  Application.Title := 'Application en arrière-plan';
end.
```

### 7.2 Démarrage automatique avec Windows

Pour qu'une application s'exécute automatiquement au démarrage de Windows, nous pouvons ajouter une entrée dans le registre :

```pascal
procedure RegisterAutoStart(const AppName, ExePath: string; Enable: Boolean);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', True) then
    begin
      if Enable then
        Reg.WriteString(AppName, ExePath)
      else if Reg.ValueExists(AppName) then
        Reg.DeleteValue(AppName);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TMainForm.chkAutoStartClick(Sender: TObject);
begin
  RegisterAutoStart('MonApplicationArrierePlan',
    Application.ExeName,
    chkAutoStart.Checked);
end;
```

### 7.3 Contrôle de processus multiples

Pour éviter que plusieurs instances de votre application ne s'exécutent simultanément :

```pascal
function IsApplicationAlreadyRunning: Boolean;
var
  MutexHandle: THandle;
  MutexName: string;
begin
  MutexName := 'Global\MyBackgroundAppMutex';
  MutexHandle := CreateMutex(nil, True, PChar(MutexName));
  Result := (MutexHandle <> 0) and (GetLastError = ERROR_ALREADY_EXISTS);

  if Result then
    // Une autre instance est déjà en cours d'exécution
    CloseHandle(MutexHandle);
  // Sinon, gardez le mutex ouvert pour signaler que cette instance est en cours d'exécution
end;

// Dans le fichier projet (.dpr)
begin
  if IsApplicationAlreadyRunning then
  begin
    Application.MessageBox(
      'Une instance de cette application est déjà en cours d''exécution.',
      'Application déjà démarrée',
      MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Application.Initialize;
  // ... reste du code normal
end.
```

## 8. Applications périodiques avec le Planificateur de tâches Windows

Une alternative aux services Windows pour les tâches périodiques est d'utiliser le Planificateur de tâches Windows (Task Scheduler).

### 8.1 Création d'une application console pour le Planificateur de tâches

Créons une application simple qui sera exécutée périodiquement par le Planificateur de tâches :

1. Créez une nouvelle application console (Fichier > Nouveau > Console Application)

2. Modifiez le code comme suit :

```pascal
program PeriodicTask;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.IOUtils;

var
  LogFile: string;

procedure WriteToLog(const Message: string);
var
  LogStream: TStreamWriter;
begin
  try
    if FileExists(LogFile) then
      LogStream := TStreamWriter.Create(LogFile, True, TEncoding.UTF8)
    else
      LogStream := TStreamWriter.Create(LogFile, False, TEncoding.UTF8);

    try
      LogStream.WriteLine(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now) + Message);
    finally
      LogStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur d''écriture dans le fichier journal: ' + E.Message);
  end;
end;

procedure ProcessFiles(const SourceDir, DestDir: string);
var
  Files: TStringDynArray;
  FileName: string;
  DestFileName: string;
begin
  // Vérifier si les répertoires existent
  if not DirectoryExists(SourceDir) then
  begin
    WriteToLog('Le répertoire source n''existe pas: ' + SourceDir);
    Exit;
  end;

  if not DirectoryExists(DestDir) then
  begin
    try
      TDirectory.CreateDirectory(DestDir);
      WriteToLog('Répertoire de destination créé: ' + DestDir);
    except
      on E: Exception do
      begin
        WriteToLog('Erreur lors de la création du répertoire de destination: ' + E.Message);
        Exit;
      end;
    end;
  end;

  try
    // Obtenir la liste des fichiers
    Files := TDirectory.GetFiles(SourceDir, '*.txt');
    WriteToLog(Format('Trouvé %d fichiers à traiter', [Length(Files)]));

    // Traiter chaque fichier
    for FileName in Files do
    begin
      DestFileName := TPath.Combine(DestDir,
        ChangeFileExt(ExtractFileName(FileName),
          FormatDateTime('_yyyymmdd', Now) + ExtractFileExt(FileName)));

      try
        // Copier le fichier avec un nouveau nom
        TFile.Copy(FileName, DestFileName, True);

        // Après une copie réussie, supprimer le fichier source
        TFile.Delete(FileName);

        WriteToLog(Format('Fichier traité: %s -> %s',
          [ExtractFileName(FileName), ExtractFileName(DestFileName)]));
      except
        on E: Exception do
          WriteToLog(Format('Erreur lors du traitement du fichier %s: %s',
            [ExtractFileName(FileName), E.Message]));
      end;
    end;
  except
    on E: Exception do
      WriteToLog('Erreur lors du traitement des fichiers: ' + E.Message);
  end;
end;

var
  SourceDirectory: string;
  DestinationDirectory: string;

begin
  try
    WriteLn('Tâche périodique démarrée à ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Définir le chemin du fichier journal
    LogFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'task_log.txt');

    WriteToLog('Tâche périodique démarrée');

    // Définir les répertoires source et destination
    if ParamCount >= 2 then
    begin
      SourceDirectory := ParamStr(1);
      DestinationDirectory := ParamStr(2);
    end
    else
    begin
      // Utiliser des valeurs par défaut
      SourceDirectory := TPath.Combine(ExtractFilePath(ParamStr(0)), 'source');
      DestinationDirectory := TPath.Combine(ExtractFilePath(ParamStr(0)), 'destination');
    end;

    WriteToLog(Format('Répertoire source: %s', [SourceDirectory]));
    WriteToLog(Format('Répertoire destination: %s', [DestinationDirectory]));

    // Traiter les fichiers
    ProcessFiles(SourceDirectory, DestinationDirectory);

    WriteToLog('Tâche périodique terminée');
    WriteLn('Tâche périodique terminée à ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Pause pour voir les résultats si exécuté manuellement
    if DebugHook <> 0 then
    begin
      WriteLn('Appuyez sur Entrée pour fermer la fenêtre...');
      ReadLn;
    end;
  except
    on E: Exception do
    begin
      WriteToLog('Erreur fatale: ' + E.Message);
      WriteLn('Erreur: ', E.Message);
      if DebugHook <> 0 then
      begin
        WriteLn('Appuyez sur Entrée pour fermer la fenêtre...');
        ReadLn;
      end;
    end;
  end;
end.
```

### 8.2 Configuration dans le Planificateur de tâches Windows

Pour configurer notre application dans le Planificateur de tâches Windows :

1. Ouvrez le Planificateur de tâches (tapez "Task Scheduler" ou "Planificateur de tâches" dans le menu Démarrer)
2. Cliquez sur "Créer une tâche de base..." dans le volet Actions
3. Suivez l'assistant :
   - Nommez la tâche "TraitementFichiersPériodique"
   - Choisissez quand exécuter la tâche (quotidiennement, hebdomadairement, etc.)
   - Définissez l'heure de démarrage
   - Action : "Démarrer un programme"
   - Programme : Chemin vers votre fichier .exe
   - Arguments : Chemin_Source Chemin_Destination (si nécessaire)
   - Finalisez la tâche

## 9. Création d'une bibliothèque de surveillance système

Créons maintenant une bibliothèque réutilisable pour la surveillance système, qui pourra être utilisée dans des services ou des applications en arrière-plan.

### 9.1 Unité de surveillance système

Créez une nouvelle unité nommée `SystemMonitorUnit.pas` :

```pascal
unit SystemMonitorUnit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Diagnostics,
  Winapi.Windows, Winapi.PsAPI;

type
  TCPUUsage = record
    TotalUsage: Double;      // Utilisation totale en pourcentage
    CoreUsages: TArray<Double>; // Utilisation par cœur
  end;

  TMemoryInfo = record
    TotalPhysical: UInt64;   // Mémoire physique totale en octets
    AvailablePhysical: UInt64; // Mémoire physique disponible en octets
    MemoryUsage: Double;     // Pourcentage d'utilisation
    TotalPageFile: UInt64;   // Taille totale du fichier d'échange
    AvailablePageFile: UInt64; // Espace disponible dans le fichier d'échange
  end;

  TDiskInfo = record
    DriveLetter: string;     // Lettre du lecteur
    TotalSize: UInt64;       // Taille totale en octets
    FreeSpace: UInt64;       // Espace libre en octets
    UsagePercent: Double;    // Pourcentage d'utilisation
  end;

  TSystemMonitor = class
  private
    FProcessorCount: Integer;
    FLastCPUSample: TArray<Int64>;
    FLastIdleSample: TArray<Int64>;
    FLastCheckTime: TDateTime;
    FDisks: TList<string>;

    function GetCPUUsageForCore(CoreIndex: Integer): Double;
    function GetCPUTotalUsage: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateSamples;

    function GetCPUUsage: TCPUUsage;
    function GetMemoryInfo: TMemoryInfo;
    function GetDiskInfo(const DriveLetter: string = ''): TArray<TDiskInfo>;
    function GetProcessList: TArray<string>;

    property ProcessorCount: Integer read FProcessorCount;
  end;

implementation

constructor TSystemMonitor.Create;
var
  SystemInfo: TSystemInfo;
  Drives: Integer;
  i: Integer;
  DriveLetter: Char;
begin
  inherited Create;

  // Obtenir le nombre de processeurs
  GetSystemInfo(SystemInfo);
  FProcessorCount := SystemInfo.dwNumberOfProcessors;

  // Initialiser les tableaux pour les échantillons CPU
  SetLength(FLastCPUSample, FProcessorCount);
  SetLength(FLastIdleSample, FProcessorCount);

  // Obtenir les lecteurs disponibles
  FDisks := TList<string>.Create;
  Drives := GetLogicalDrives;
  for i := 0 to 25 do
  begin
    if (Drives and (1 shl i)) <> 0 then
    begin
      DriveLetter := Char(i + Ord('A'));
      FDisks.Add(DriveLetter + ':');
    end;
  end;

  // Initialiser avec un premier échantillon
  UpdateSamples;
end;

destructor TSystemMonitor.Destroy;
begin
  FDisks.Free;
  inherited;
end;

procedure TSystemMonitor.UpdateSamples;
var
  i: Integer;
  SystemTimes: TArray<FILETIME>;
  IdleTime: Int64;
  KernelTime: Int64;
  UserTime: Int64;
begin
  // Obtenir les temps système pour chaque processeur
  SetLength(SystemTimes, 3); // IdleTime, KernelTime, UserTime

  for i := 0 to FProcessorCount - 1 do
  begin
    if GetSystemTimes(SystemTimes[0], SystemTimes[1], SystemTimes[2]) then
    begin
      // Convertir les temps en Int64
      IdleTime := Int64(SystemTimes[0].dwLowDateTime) or (Int64(SystemTimes[0].dwHighDateTime) shl 32);
      KernelTime := Int64(SystemTimes[1].dwLowDateTime) or (Int64(SystemTimes[1].dwHighDateTime) shl 32);
      UserTime := Int64(SystemTimes[2].dwLowDateTime) or (Int64(SystemTimes[2].dwHighDateTime) shl 32);

      // Le temps kernel inclut le temps d'inactivité, donc on le soustrait
      KernelTime := KernelTime - IdleTime;

      // Stocker les nouveaux échantillons
      FLastCPUSample[i] := KernelTime + UserTime;
      FLastIdleSample[i] := IdleTime;
    end;
  end;

  FLastCheckTime := Now;
end;

function TSystemMonitor.GetCPUUsageForCore(CoreIndex: Integer): Double;
var
  CurrentCPUSample: TArray<Int64>;
  CurrentIdleSample: TArray<Int64>;
  IdleDiff: Int64;
  TotalDiff: Int64;
  SystemTimes: TArray<FILETIME>;
  IdleTime: Int64;
  KernelTime: Int64;
  UserTime: Int64;
begin
  Result := 0;

  if (CoreIndex < 0) or (CoreIndex >= FProcessorCount) then
    Exit;

  // Obtenir le temps actuel
  SetLength(SystemTimes, 3); // IdleTime, KernelTime, UserTime

  if GetSystemTimes(SystemTimes[0], SystemTimes[1], SystemTimes[2]) then
  begin
    // Convertir les temps en Int64
    IdleTime := Int64(SystemTimes[0].dwLowDateTime) or (Int64(SystemTimes[0].dwHighDateTime) shl 32);
    KernelTime := Int64(SystemTimes[1].dwLowDateTime) or (Int64(SystemTimes[1].dwHighDateTime) shl 32);
    UserTime := Int64(SystemTimes[2].dwLowDateTime) or (Int64(SystemTimes[2].dwHighDateTime) shl 32);

    // Le temps kernel inclut le temps d'inactivité, donc on le soustrait
    KernelTime := KernelTime - IdleTime;

    // Calculer les différences depuis le dernier échantillon
    IdleDiff := IdleTime - FLastIdleSample[CoreIndex];
    TotalDiff := (KernelTime + UserTime) - FLastCPUSample[CoreIndex];

    if TotalDiff > 0 then
      Result := 100 - (IdleDiff * 100 / TotalDiff);
  end;
end;

function TSystemMonitor.GetCPUTotalUsage: Double;
var
  UsageSum: Double;
  i: Integer;
begin
  UsageSum := 0;

  for i := 0 to FProcessorCount - 1 do
    UsageSum := UsageSum + GetCPUUsageForCore(i);

  Result := UsageSum / FProcessorCount;
end;

function TSystemMonitor.GetCPUUsage: TCPUUsage;
var
  i: Integer;
begin
  Result.TotalUsage := GetCPUTotalUsage;

  SetLength(Result.CoreUsages, FProcessorCount);
  for i := 0 to FProcessorCount - 1 do
    Result.CoreUsages[i] := GetCPUUsageForCore(i);
end;

function TSystemMonitor.GetMemoryInfo: TMemoryInfo;
var
  MemoryStatus: TMemoryStatusEx;
begin
  MemoryStatus.dwLength := SizeOf(MemoryStatus);

  if GlobalMemoryStatusEx(MemoryStatus) then
  begin
    Result.TotalPhysical := MemoryStatus.ullTotalPhys;
    Result.AvailablePhysical := MemoryStatus.ullAvailPhys;
    Result.MemoryUsage := MemoryStatus.dwMemoryLoad;
    Result.TotalPageFile := MemoryStatus.ullTotalPageFile;
    Result.AvailablePageFile := MemoryStatus.ullAvailPageFile;
  end
  else
  begin
    // En cas d'erreur, initialiser à zéro
    Result.TotalPhysical := 0;
    Result.AvailablePhysical := 0;
    Result.MemoryUsage := 0;
    Result.TotalPageFile := 0;
    Result.AvailablePageFile := 0;
  end;
end;

function TSystemMonitor.GetDiskInfo(const DriveLetter: string): TArray<TDiskInfo>;
var
  TotalSpace, FreeSpace: UInt64;
  i: Integer;
  DriveToCheck: string;
  ResultList: TList<TDiskInfo>;
  Info: TDiskInfo;
begin
  ResultList := TList<TDiskInfo>.Create;
  try
    // Si aucun lecteur spécifié, vérifier tous les lecteurs
    if DriveLetter = '' then
    begin
      for i := 0 to FDisks.Count - 1 do
      begin
        DriveToCheck := FDisks[i];

        if GetDiskFreeSpaceEx(PChar(DriveToCheck + '\'), FreeSpace, TotalSpace, nil) then
        begin
          Info.DriveLetter := DriveToCheck;
          Info.TotalSize := TotalSpace;
          Info.FreeSpace := FreeSpace;

          if TotalSpace > 0 then
            Info.UsagePercent := 100 * (1 - (FreeSpace / TotalSpace))
          else
            Info.UsagePercent := 0;

          ResultList.Add(Info);
        end;
      end;
    end
    else
    begin
      // Vérifier un lecteur spécifique
      DriveToCheck := DriveLetter;
      if DriveToCheck[Length(DriveToCheck)] <> ':' then
        DriveToCheck := DriveToCheck + ':';

      if GetDiskFreeSpaceEx(PChar(DriveToCheck + '\'), FreeSpace, TotalSpace, nil) then
      begin
        Info.DriveLetter := DriveToCheck;
        Info.TotalSize := TotalSpace;
        Info.FreeSpace := FreeSpace;

        if TotalSpace > 0 then
          Info.UsagePercent := 100 * (1 - (FreeSpace / TotalSpace))
        else
          Info.UsagePercent := 0;

        ResultList.Add(Info);
      end;
    end;

    // Convertir la liste en tableau
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TSystemMonitor.GetProcessList: TArray<string>;
var
  ProcessEntry: TProcessEntry32;
  Snapshot: THandle;
  ProcessList: TList<string>;
begin
  ProcessList := TList<string>.Create;
  try
    Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if Snapshot <> INVALID_HANDLE_VALUE then
    begin
      try
        ProcessEntry.dwSize := SizeOf(ProcessEntry);
        if Process32First(Snapshot, ProcessEntry) then
        begin
          repeat
            ProcessList.Add(string(ProcessEntry.szExeFile));
          until not Process32Next(Snapshot, ProcessEntry);
        end;
      finally
        CloseHandle(Snapshot);
      end;
    end;

    Result := ProcessList.ToArray;
  finally
    ProcessList.Free;
  end;
end;

end.
```

### 9.2 Utilisation de la bibliothèque dans un service

Maintenant, utilisons cette bibliothèque dans notre service de journalisation pour surveiller les ressources système :

```pascal
unit SystemMonitorServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls,
  SystemMonitorUnit;

type
  TSystemMonitorService = class(TService)
    MonitorTimer: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure MonitorTimerTimer(Sender: TObject);
  private
    FSystemMonitor: TSystemMonitor;
    FLogFile: string;
    FAlertThresholdCPU: Double;
    FAlertThresholdMemory: Double;
    FAlertThresholdDisk: Double;

    procedure WriteToLog(const Message: string);
    procedure CheckAndSendAlerts;
    procedure LoadSettings;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  SystemMonitorService: TSystemMonitorService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SystemMonitorService.Controller(CtrlCode);
end;

function TSystemMonitorService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSystemMonitorService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Créer l'instance du moniteur système
  FSystemMonitor := TSystemMonitor.Create;

  // Charger les paramètres
  LoadSettings;

  // Définir le chemin du fichier journal
  FLogFile := ExtractFilePath(ParamStr(0)) + 'system_monitor.log';

  // Écrire un message au démarrage
  WriteToLog('Service de surveillance système démarré à ' +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Effectuer une première vérification
  MonitorTimerTimer(nil);

  // Démarrer le timer
  MonitorTimer.Enabled := True;

  Started := True;
end;

procedure TSystemMonitorService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  // Arrêter le timer
  MonitorTimer.Enabled := False;

  // Libérer les ressources
  if Assigned(FSystemMonitor) then
  begin
    FSystemMonitor.Free;
    FSystemMonitor := nil;
  end;

  // Écrire un message d'arrêt
  WriteToLog('Service de surveillance système arrêté à ' +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  Stopped := True;
end;

procedure TSystemMonitorService.LoadSettings;
var
  Registry: TRegistry;
begin
  // Définir des valeurs par défaut
  FAlertThresholdCPU := 90;    // Alerte si CPU > 90%
  FAlertThresholdMemory := 85; // Alerte si mémoire > 85%
  FAlertThresholdDisk := 90;   // Alerte si disque > 90%

  // Charger les paramètres depuis le registre
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('SOFTWARE\YourCompany\SystemMonitorService', False) then
    begin
      if Registry.ValueExists('AlertThresholdCPU') then
        FAlertThresholdCPU := Registry.ReadFloat('AlertThresholdCPU');

      if Registry.ValueExists('AlertThresholdMemory') then
        FAlertThresholdMemory := Registry.ReadFloat('AlertThresholdMemory');

      if Registry.ValueExists('AlertThresholdDisk') then
        FAlertThresholdDisk := Registry.ReadFloat('AlertThresholdDisk');

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSystemMonitorService.WriteToLog(const Message: string);
var
  LogStream: TStreamWriter;
begin
  try
    // Créer ou ouvrir le fichier en mode append
    if FileExists(FLogFile) then
      LogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8)
    else
      LogStream := TStreamWriter.Create(FLogFile, False, TEncoding.UTF8);

    try
      LogStream.WriteLine(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now) + Message);
    finally
      LogStream.Free;
    end;
  except
    on E: Exception do
      // Écrire dans le journal d'événements Windows en cas d'erreur
      LogEvent(Format('Erreur d''écriture dans le fichier journal: %s', [E.Message]),
        EVENTLOG_ERROR_TYPE, 0, 0);
  end;
end;

procedure TSystemMonitorService.MonitorTimerTimer(Sender: TObject);
var
  CPUUsage: TCPUUsage;
  MemoryInfo: TMemoryInfo;
  DiskInfoArray: TArray<TDiskInfo>;
  DiskInfo: TDiskInfo;
  i: Integer;
  ProcessList: TArray<string>;
  LogMessage: string;
begin
  try
    // Mettre à jour les échantillons
    FSystemMonitor.UpdateSamples;

    // Obtenir l'utilisation du CPU
    CPUUsage := FSystemMonitor.GetCPUUsage;
    LogMessage := Format('CPU Total: %.1f%%', [CPUUsage.TotalUsage]);
    WriteToLog(LogMessage);

    // Obtenir l'information sur la mémoire
    MemoryInfo := FSystemMonitor.GetMemoryInfo;
    LogMessage := Format('Mémoire: %.1f%% utilisée (%.2f GB / %.2f GB)',
      [MemoryInfo.MemoryUsage,
       (MemoryInfo.TotalPhysical - MemoryInfo.AvailablePhysical) / (1024*1024*1024),
       MemoryInfo.TotalPhysical / (1024*1024*1024)]);
    WriteToLog(LogMessage);

    // Obtenir l'information sur les disques
    DiskInfoArray := FSystemMonitor.GetDiskInfo();
    for DiskInfo in DiskInfoArray do
    begin
      LogMessage := Format('Disque %s: %.1f%% utilisé (%.2f GB / %.2f GB)',
        [DiskInfo.DriveLetter,
         DiskInfo.UsagePercent,
         (DiskInfo.TotalSize - DiskInfo.FreeSpace) / (1024*1024*1024),
         DiskInfo.TotalSize / (1024*1024*1024)]);
      WriteToLog(LogMessage);
    end;

    // Vérifier les seuils d'alerte
    CheckAndSendAlerts;

    // Tous les 10 cycles (selon la fréquence du timer), lister les processus en cours
    if (GetTickCount div MonitorTimer.Interval) mod 10 = 0 then
    begin
      ProcessList := FSystemMonitor.GetProcessList;
      WriteToLog('Liste des processus en cours d''exécution:');
      for i := 0 to Min(19, Length(ProcessList) - 1) do // Limiter à 20 processus pour éviter un log trop volumineux
        WriteToLog(' - ' + ProcessList[i]);

      if Length(ProcessList) > 20 then
        WriteToLog(Format(' - ... et %d autres processus', [Length(ProcessList) - 20]));
    end;
  except
    on E: Exception do
      WriteToLog('Erreur lors de la surveillance système: ' + E.Message);
  end;
end;

procedure TSystemMonitorService.CheckAndSendAlerts;
var
  CPUUsage: TCPUUsage;
  MemoryInfo: TMemoryInfo;
  DiskInfoArray: TArray<TDiskInfo>;
  DiskInfo: TDiskInfo;
  AlertMessage: string;
  HasAlert: Boolean;
begin
  HasAlert := False;
  AlertMessage := 'ALERTE SYSTÈME - ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) + sLineBreak;

  // Vérifier l'utilisation du CPU
  CPUUsage := FSystemMonitor.GetCPUUsage;
  if CPUUsage.TotalUsage > FAlertThresholdCPU then
  begin
    AlertMessage := AlertMessage + Format('- CPU utilisé à %.1f%% (seuil: %.1f%%)' + sLineBreak,
      [CPUUsage.TotalUsage, FAlertThresholdCPU]);
    HasAlert := True;
  end;

  // Vérifier l'utilisation de la mémoire
  MemoryInfo := FSystemMonitor.GetMemoryInfo;
  if MemoryInfo.MemoryUsage > FAlertThresholdMemory then
  begin
    AlertMessage := AlertMessage + Format('- Mémoire utilisée à %.1f%% (seuil: %.1f%%)' + sLineBreak,
      [MemoryInfo.MemoryUsage, FAlertThresholdMemory]);
    HasAlert := True;
  end;

  // Vérifier l'utilisation des disques
  DiskInfoArray := FSystemMonitor.GetDiskInfo();
  for DiskInfo in DiskInfoArray do
  begin
    if DiskInfo.UsagePercent > FAlertThresholdDisk then
    begin
      AlertMessage := AlertMessage + Format('- Disque %s utilisé à %.1f%% (seuil: %.1f%%)' + sLineBreak,
        [DiskInfo.DriveLetter, DiskInfo.UsagePercent, FAlertThresholdDisk]);
      HasAlert := True;
    end;
  end;

  // Si au moins une alerte, enregistrer et notifier
  if HasAlert then
  begin
    // Écrire dans le journal
    WriteToLog('!!! ' + AlertMessage);

    // Dans une application réelle, vous pourriez envoyer un email,
    // un SMS ou utiliser d'autres moyens de notification

    // Écrire également dans le journal d'événements Windows
    LogEvent(AlertMessage, EVENTLOG_WARNING_TYPE, 0, 0);
  end;
end;
```

## 10. Projet avancé : Service de synchronisation de fichiers

Maintenant, créons un projet plus complet : un service qui surveille un dossier et synchronise les fichiers modifiés vers un emplacement réseau.

### 10.1 Structure du service

```pascal
unit FileSyncServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls,
  System.IOUtils, System.Generics.Collections, System.Generics.Defaults,
  System.SyncObjs, Winapi.ActiveX;

type
  TFileOperation = (foCreate, foModify, foDelete, foRename);

  TFileChangeInfo = class
  private
    FFilePath: string;
    FOperation: TFileOperation;
    FTimestamp: TDateTime;
    FNewPath: string; // Pour les opérations de renommage
  public
    constructor Create(const AFilePath: string; AOperation: TFileOperation);
    property FilePath: string read FFilePath;
    property Operation: TFileOperation read FOperation;
    property Timestamp: TDateTime read FTimestamp;
    property NewPath: string read FNewPath write FNewPath;
  end;

  TFileSyncService = class(TService)
    SyncTimer: TTimer;
    DirectoryWatchTimer: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure SyncTimerTimer(Sender: TObject);
    procedure DirectoryWatchTimerTimer(Sender: TObject);
  private
    FSourceDirectory: string;
    FDestinationDirectory: string;
    FExcludePatterns: TArray<string>;
    FFileQueue: TThreadedQueue<TFileChangeInfo>;
    FLastFileHash: TDictionary<string, string>;
    FWatchHandles: TDictionary<string, THandle>;
    FLogFile: string;
    FSyncRunning: Boolean;
    FCriticalSection: TCriticalSection;

    procedure WriteToLog(const Message: string);
    procedure LoadSettings;
    function ShouldExcludeFile(const FilePath: string): Boolean;
    procedure ProcessFileChange(FileInfo: TFileChangeInfo);
    procedure SetupDirectoryWatchers;
    procedure CleanupDirectoryWatchers;
    function GetFileHash(const FilePath: string): string;
    procedure SynchronizeFile(const SourceFile, DestFile: string);
    procedure DeleteDestinationFile(const DestFile: string);
    procedure WatchDirectory(const Directory: string);
    procedure ProcessDirectoryChanges(const WatchHandle: THandle);
  public
    function GetServiceController: TServiceController; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FileSyncService: TFileSyncService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FileSyncService.Controller(CtrlCode);
end;

function TFileSyncService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

constructor TFileSyncService.Create(AOwner: TComponent);
begin
  inherited;
  FFileQueue := TThreadedQueue<TFileChangeInfo>.Create(1000, 10, 1000);
  FLastFileHash := TDictionary<string, string>.Create;
  FWatchHandles := TDictionary<string, THandle>.Create;
  FCriticalSection := TCriticalSection.Create;
  FSyncRunning := False;
end;

destructor TFileSyncService.Destroy;
begin
  CleanupDirectoryWatchers;
  FFileQueue.Free;
  FLastFileHash.Free;
  FWatchHandles.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TFileSyncService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Initialiser COM pour le thread principal
  CoInitialize(nil);

  // Charger les paramètres
  LoadSettings;

  // Définir le chemin du fichier journal
  FLogFile := ExtractFilePath(ParamStr(0)) + 'file_sync.log';

  // Écrire un message au démarrage
  WriteToLog('Service de synchronisation de fichiers démarré à ' +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Vérifier si les répertoires existent
  if not DirectoryExists(FSourceDirectory) then
  begin
    WriteToLog('ERREUR: Le répertoire source n''existe pas: ' + FSourceDirectory);
    try
      TDirectory.CreateDirectory(FSourceDirectory);
      WriteToLog('Répertoire source créé: ' + FSourceDirectory);
    except
      on E: Exception do
      begin
        WriteToLog('Impossible de créer le répertoire source: ' + E.Message);
        Started := False;
        Exit;
      end;
    end;
  end;

  if not DirectoryExists(FDestinationDirectory) then
  begin
    WriteToLog('ERREUR: Le répertoire de destination n''existe pas: ' + FDestinationDirectory);
    try
      TDirectory.CreateDirectory(FDestinationDirectory);
      WriteToLog('Répertoire de destination créé: ' + FDestinationDirectory);
    except
      on E: Exception do
      begin
        WriteToLog('Impossible de créer le répertoire de destination: ' + E.Message);
        Started := False;
        Exit;
      end;
    end;
  end;

  // Configurer la surveillance des répertoires
  SetupDirectoryWatchers;

  // Effectuer une synchronisation initiale
  SyncTimerTimer(nil);

  // Démarrer les timers
  SyncTimer.Enabled := True;
  DirectoryWatchTimer.Enabled := True;

  Started := True;
end;

procedure TFileSyncService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  // Arrêter les timers
  SyncTimer.Enabled := False;
  DirectoryWatchTimer.Enabled := False;

  // Nettoyer les surveillances de répertoires
  CleanupDirectoryWatchers;

  // Vider la file d'attente
  while FFileQueue.QueueSize > 0 do
  begin
    var FileInfo := FFileQueue.PopItem;
    if FileInfo <> nil then
      FileInfo.Free;
  end;

  // Écrire un message d'arrêt
  WriteToLog('Service de synchronisation de fichiers arrêté à ' +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Libérer COM
  CoUninitialize;

  Stopped := True;
end;

procedure TFileSyncService.LoadSettings;
var
  Registry: TRegistry;
  ExcludeString: string;
begin
  // Définir des valeurs par défaut
  FSourceDirectory := 'C:\SyncSource';
  FDestinationDirectory := 'C:\SyncDestination';
  FExcludePatterns := ['.tmp', '.bak', '~$*', 'Thumbs.db'];

  // Charger les paramètres depuis le registre
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('SOFTWARE\YourCompany\FileSyncService', False) then
    begin
      if Registry.ValueExists('SourceDirectory') then
        FSourceDirectory := Registry.ReadString('SourceDirectory');

      if Registry.ValueExists('DestinationDirectory') then
        FDestinationDirectory := Registry.ReadString('DestinationDirectory');

      if Registry.ValueExists('ExcludePatterns') then
      begin
        ExcludeString := Registry.ReadString('ExcludePatterns');
        FExcludePatterns := ExcludeString.Split([';']);
      end;

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TFileSyncService.WriteToLog(const Message: string);
var
  LogStream: TStreamWriter;
begin
  try
    // Créer ou ouvrir le fichier en mode append
    if FileExists(FLogFile) then
      LogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8)
    else
      LogStream := TStreamWriter.Create(FLogFile, False, TEncoding.UTF8);

    try
      LogStream.WriteLine(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now) + Message);
    finally
      LogStream.Free;
    end;
  except
    on E: Exception do
      // Écrire dans le journal d'événements Windows en cas d'erreur
      LogEvent(Format('Erreur d''écriture dans le fichier journal: %s', [E.Message]),
        EVENTLOG_ERROR_TYPE, 0, 0);
  end;
end;
```

### 10.2 Implémentation des fonctions de surveillance et synchronisation

```pascal
procedure TFileSyncService.SetupDirectoryWatchers;
begin
  // Surveiller le répertoire principal
  WatchDirectory(FSourceDirectory);

  // Surveiller également les sous-répertoires
  for var Dir in TDirectory.GetDirectories(FSourceDirectory, '*', TSearchOption.soAllDirectories) do
    WatchDirectory(Dir);
end;

procedure TFileSyncService.WatchDirectory(const Directory: string);
var
  WatchHandle: THandle;
begin
  // Éviter de surveiller deux fois le même répertoire
  if FWatchHandles.ContainsKey(Directory) then
    Exit;

  // Créer un handle de surveillance pour le répertoire
  WatchHandle := FindFirstChangeNotification(
    PChar(Directory),
    True, // Inclure les sous-répertoires
    FILE_NOTIFY_CHANGE_FILE_NAME or   // Création/suppression/renommage de fichiers
    FILE_NOTIFY_CHANGE_DIR_NAME or    // Création/suppression/renommage de répertoires
    FILE_NOTIFY_CHANGE_LAST_WRITE     // Modification de fichiers
  );

  if WatchHandle <> INVALID_HANDLE_VALUE then
  begin
    // Stocker le handle pour ce répertoire
    FWatchHandles.Add(Directory, WatchHandle);
    WriteToLog('Surveillance configurée pour le répertoire: ' + Directory);
  end
  else
    WriteToLog('Erreur lors de la configuration de la surveillance pour: ' + Directory);
end;

procedure TFileSyncService.CleanupDirectoryWatchers;
var
  Directory: string;
  WatchHandle: THandle;
begin
  for Directory in FWatchHandles.Keys do
  begin
    if FWatchHandles.TryGetValue(Directory, WatchHandle) then
    begin
      if WatchHandle <> INVALID_HANDLE_VALUE then
        FindCloseChangeNotification(WatchHandle);
    end;
  end;

  FWatchHandles.Clear;
end;

procedure TFileSyncService.DirectoryWatchTimerTimer(Sender: TObject);
var
  Directory: string;
  WatchHandle: THandle;
  WaitResult: DWORD;
begin
  // Vérifier chaque répertoire surveillé
  for Directory in FWatchHandles.Keys.ToArray do
  begin
    if FWatchHandles.TryGetValue(Directory, WatchHandle) then
    begin
      if WatchHandle <> INVALID_HANDLE_VALUE then
      begin
        // Vérifier si des changements ont été détectés
        WaitResult := WaitForSingleObject(WatchHandle, 0);

        if WaitResult = WAIT_OBJECT_0 then
        begin
          // Des changements détectés, traiter les modifications
          ProcessDirectoryChanges(Directory);

          // Réinitialiser la notification
          if not FindNextChangeNotification(WatchHandle) then
          begin
            WriteToLog('Erreur lors de la réinitialisation de la surveillance pour: ' + Directory);

            // Fermer et recréer le handle de surveillance
            FindCloseChangeNotification(WatchHandle);
            WatchDirectory(Directory);
          end;
        end;
      end;
    end;
  end;

  // Vérifier si de nouveaux sous-répertoires ont été créés
  try
    for var Dir in TDirectory.GetDirectories(FSourceDirectory, '*', TSearchOption.soAllDirectories) do
    begin
      if not FWatchHandles.ContainsKey(Dir) then
        WatchDirectory(Dir);
    end;
  except
    on E: Exception do
      WriteToLog('Erreur lors de la vérification des nouveaux sous-répertoires: ' + E.Message);
  end;
end;

procedure TFileSyncService.ProcessDirectoryChanges(const Directory: string);
var
  Files: TArray<string>;
  File: string;
  RelativePath: string;
  SourceFile: string;
  DestFile: string;
  CurrentHash: string;
  StoredHash: string;
  FileInfo: TFileChangeInfo;
begin
  try
    // Obtenir tous les fichiers du répertoire
    Files := TDirectory.GetFiles(Directory, '*.*', TSearchOption.soTopDirectoryOnly);

    // Vérifier chaque fichier pour les modifications
    for File in Files do
    begin
      if ShouldExcludeFile(File) then
        Continue;

      CurrentHash := GetFileHash(File);

      // Calculer les chemins relatifs
      RelativePath := File.Substring(FSourceDirectory.Length).TrimStart([PathDelim]);
      SourceFile := File;
      DestFile := TPath.Combine(FDestinationDirectory, RelativePath);

      // Vérifier si le fichier existe dans notre cache
      if FLastFileHash.TryGetValue(SourceFile, StoredHash) then
      begin
        // Le fichier existe, vérifier s'il a été modifié
        if CurrentHash <> StoredHash then
        begin
          // Fichier modifié
          FileInfo := TFileChangeInfo.Create(SourceFile, foModify);
          FFileQueue.PushItem(FileInfo);

          // Mettre à jour le hash
          FLastFileHash[SourceFile] := CurrentHash;

          WriteToLog('Fichier modifié détecté: ' + SourceFile);
        end;
      end
      else
      begin
        // Nouveau fichier
        FileInfo := TFileChangeInfo.Create(SourceFile, foCreate);
        FFileQueue.PushItem(FileInfo);

        // Stocker le hash
        FLastFileHash.Add(SourceFile, CurrentHash);

        WriteToLog('Nouveau fichier détecté: ' + SourceFile);
      end;
    end;

    // Vérifier les fichiers supprimés
    var FilesCopy := FLastFileHash.Keys.ToArray;
    for SourceFile in FilesCopy do
    begin
      if SourceFile.StartsWith(Directory) and not FileExists(SourceFile) then
      begin
        // Fichier supprimé
        RelativePath := SourceFile.Substring(FSourceDirectory.Length).TrimStart([PathDelim]);
        DestFile := TPath.Combine(FDestinationDirectory, RelativePath);

        FileInfo := TFileChangeInfo.Create(DestFile, foDelete);
        FFileQueue.PushItem(FileInfo);

        // Retirer du cache
        FLastFileHash.Remove(SourceFile);

        WriteToLog('Fichier supprimé détecté: ' + SourceFile);
      end;
    end;
  except
    on E: Exception do
      WriteToLog('Erreur lors du traitement des changements de répertoire: ' + E.Message);
  end;
end;

function TFileSyncService.ShouldExcludeFile(const FilePath: string): Boolean;
var
  FileName: string;
  Pattern: string;
begin
  FileName := ExtractFileName(FilePath);

  for Pattern in FExcludePatterns do
  begin
    // Gestion des jokers (* et ?)
    if MatchesMask(FileName, Pattern) then
      Exit(True);
  end;

  Result := False;
end;

function TFileSyncService.GetFileHash(const FilePath: string): string;
var
  FileStream: TFileStream;
  MD5Context: TIdHashMessageDigest5;
  HashBytes: TIdBytes;
begin
  Result := '';

  if not FileExists(FilePath) then
    Exit;

  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    try
      MD5Context := TIdHashMessageDigest5.Create;
      try
        HashBytes := MD5Context.HashStream(FileStream);
        Result := MD5Context.HashBytesToHex(HashBytes);
      finally
        MD5Context.Free;
      end;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteToLog('Erreur lors du calcul du hash pour ' + FilePath + ': ' + E.Message);
      // En cas d'erreur, utiliser une valeur unique basée sur la date/heure
      Result := FormatDateTime('yyyymmddhhnnsszzz', Now);
    end;
  end;
end;

procedure TFileSyncService.SyncTimerTimer(Sender: TObject);
var
  FileInfo: TFileChangeInfo;
begin
  // Éviter l'exécution simultanée
  if FSyncRunning then
    Exit;

  FCriticalSection.Enter;
  try
    FSyncRunning := True;

    // Traiter tous les changements en attente
    while FFileQueue.QueueSize > 0 do
    begin
      if FFileQueue.PopItem(FileInfo, 100) then
      begin
        try
          ProcessFileChange(FileInfo);
        finally
          FileInfo.Free;
        end;
      end;
    end;
  finally
    FSyncRunning := False;
    FCriticalSection.Leave;
  end;
end;

procedure TFileSyncService.ProcessFileChange(FileInfo: TFileChangeInfo);
var
  RelativePath: string;
  DestFile: string;
begin
  try
    case FileInfo.Operation of
      foCreate, foModify:
        begin
          // Calculer le chemin de destination
          RelativePath := FileInfo.FilePath.Substring(FSourceDirectory.Length).TrimStart([PathDelim]);
          DestFile := TPath.Combine(FDestinationDirectory, RelativePath);

          // Synchroniser le fichier
          SynchronizeFile(FileInfo.FilePath, DestFile);
        end;

      foDelete:
        begin
          // Pour les suppressions, FileInfo.FilePath contient déjà le chemin de destination
          DeleteDestinationFile(FileInfo.FilePath);
        end;

      foRename:
        begin
          // Pour les renommages, supprimer l'ancien fichier et créer le nouveau
          DeleteDestinationFile(FileInfo.FilePath);

          // Puis synchroniser le nouveau
          RelativePath := FileInfo.NewPath.Substring(FSourceDirectory.Length).TrimStart([PathDelim]);
          DestFile := TPath.Combine(FDestinationDirectory, RelativePath);

          SynchronizeFile(FileInfo.NewPath, DestFile);
        end;
    end;
  except
    on E: Exception do
      WriteToLog('Erreur lors du traitement du changement de fichier: ' + E.Message);
  end;
end;

procedure TFileSyncService.SynchronizeFile(const SourceFile, DestFile: string);
var
  DestDir: string;
begin
  if not FileExists(SourceFile) then
  begin
    WriteToLog('Erreur: Fichier source inexistant: ' + SourceFile);
    Exit;
  end;

  // Créer le répertoire de destination s'il n'existe pas
  DestDir := ExtractFilePath(DestFile);
  if not DirectoryExists(DestDir) then
  begin
    try
      ForceDirectories(DestDir);
      WriteToLog('Répertoire de destination créé: ' + DestDir);
    except
      on E: Exception do
      begin
        WriteToLog('Erreur lors de la création du répertoire de destination: ' + E.Message);
        Exit;
      end;
    end;
  end;

  // Copier le fichier
  try
    TFile.Copy(SourceFile, DestFile, True);
    WriteToLog('Fichier synchronisé: ' + SourceFile + ' -> ' + DestFile);

    // Copier également les attributs et la date de modification
    TFile.SetAttributes(DestFile, TFile.GetAttributes(SourceFile));
    TFile.SetLastWriteTime(DestFile, TFile.GetLastWriteTime(SourceFile));
  except
    on E: Exception do
      WriteToLog('Erreur lors de la copie du fichier: ' + E.Message);
  end;
end;

procedure TFileSyncService.DeleteDestinationFile(const DestFile: string);
begin
  if FileExists(DestFile) then
  begin
    try
      TFile.Delete(DestFile);
      WriteToLog('Fichier supprimé: ' + DestFile);
    except
      on E: Exception do
        WriteToLog('Erreur lors de la suppression du fichier: ' + E.Message);
    end;
  end
  else
    WriteToLog('Fichier à supprimer introuvable: ' + DestFile);
end;

{ TFileChangeInfo }

constructor TFileChangeInfo.Create(const AFilePath: string; AOperation: TFileOperation);
begin
  inherited Create;
  FFilePath := AFilePath;
  FOperation := AOperation;
  FTimestamp := Now;
  FNewPath := '';
end;
```

### 10.3 Tests et débogage du service de synchronisation

Pour tester notre service de synchronisation de fichiers, nous pouvons créer une application de test qui simule les opérations et affiche les résultats :

```pascal
program TestFileSyncService;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Winapi.Windows;

var
  SourceDir, DestDir: string;

procedure WriteLog(const Msg: string);
begin
  Writeln(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
end;

function CreateSampleFile(const Directory, FileName, Content: string): string;
var
  FilePath: string;
  StreamWriter: TStreamWriter;
begin
  FilePath := TPath.Combine(Directory, FileName);

  StreamWriter := TStreamWriter.Create(FilePath, False, TEncoding.UTF8);
  try
    StreamWriter.Write(Content);
  finally
    StreamWriter.Free;
  end;

  Result := FilePath;
  WriteLog('Fichier créé: ' + FilePath);
end;

procedure ModifySampleFile(const FilePath, NewContent: string);
var
  StreamWriter: TStreamWriter;
begin
  if FileExists(FilePath) then
  begin
    StreamWriter := TStreamWriter.Create(FilePath, False, TEncoding.UTF8);
    try
      StreamWriter.Write(NewContent);
    finally
      StreamWriter.Free;
    end;

    WriteLog('Fichier modifié: ' + FilePath);
  end
  else
    WriteLog('Erreur: Fichier à modifier inexistant: ' + FilePath);
end;

procedure DeleteSampleFile(const FilePath: string);
begin
  if FileExists(FilePath) then
  begin
    TFile.Delete(FilePath);
    WriteLog('Fichier supprimé: ' + FilePath);
  end
  else
    WriteLog('Erreur: Fichier à supprimer inexistant: ' + FilePath);
end;

procedure CreateNestedDirectories(const BaseDir: string);
var
  SubDir1, SubDir2: string;
begin
  SubDir1 := TPath.Combine(BaseDir, 'Niveau1');
  SubDir2 := TPath.Combine(SubDir1, 'Niveau2');

  if not DirectoryExists(SubDir1) then
  begin
    TDirectory.CreateDirectory(SubDir1);
    WriteLog('Répertoire créé: ' + SubDir1);
  end;

  if not DirectoryExists(SubDir2) then
  begin
    TDirectory.CreateDirectory(SubDir2);
    WriteLog('Répertoire créé: ' + SubDir2);
  end;
end;

procedure TestFileOperations;
var
  File1, File2, File3: string;
begin
  // Créer des fichiers test
  File1 := CreateSampleFile(SourceDir, 'test1.txt', 'Contenu du fichier test 1');
  File2 := CreateSampleFile(SourceDir, 'test2.txt', 'Contenu du fichier test 2');

  // Créer un fichier dans un sous-répertoire
  CreateNestedDirectories(SourceDir);
  File3 := CreateSampleFile(TPath.Combine(SourceDir, 'Niveau1\Niveau2'),
    'test3.txt', 'Contenu du fichier test 3');

  // Attendre que le service détecte et traite les nouveaux fichiers
  WriteLog('Attente pour la détection des nouveaux fichiers...');
  Sleep(5000);

  // Modifier un fichier
  WriteLog('Modification d''un fichier...');
  ModifySampleFile(File1, 'Contenu modifié du fichier test 1');

  // Attendre que le service détecte et traite les modifications
  WriteLog('Attente pour la détection des modifications...');
  Sleep(5000);

  // Supprimer un fichier
  WriteLog('Suppression d''un fichier...');
  DeleteSampleFile(File2);

  // Attendre que le service détecte et traite les suppressions
  WriteLog('Attente pour la détection des suppressions...');
  Sleep(5000);

  // Vérifier les résultats
  WriteLog('Vérification des résultats:');

  var DestFile1 := TPath.Combine(DestDir, 'test1.txt');
  var DestFile2 := TPath.Combine(DestDir, 'test2.txt');
  var DestFile3 := TPath.Combine(DestDir, 'Niveau1\Niveau2\test3.txt');

  if FileExists(DestFile1) then
  begin
    var Content := TFile.ReadAllText(DestFile1);
    WriteLog('- Fichier 1 synchronisé correctement: ' + BoolToStr(Content = 'Contenu modifié du fichier test 1', True));
  end
  else
    WriteLog('- Fichier 1 non synchronisé.');

  WriteLog('- Fichier 2 supprimé correctement: ' + BoolToStr(not FileExists(DestFile2), True));

  if FileExists(DestFile3) then
  begin
    var Content := TFile.ReadAllText(DestFile3);
    WriteLog('- Fichier 3 synchronisé correctement: ' + BoolToStr(Content = 'Contenu du fichier test 3', True));
  end
  else
    WriteLog('- Fichier 3 non synchronisé.');
end;

begin
  try
    Writeln('Test du service de synchronisation de fichiers');
    Writeln('--------------------------------------------');

    // Définir les répertoires source et destination pour les tests
    SourceDir := 'C:\SyncTest\Source';
    DestDir := 'C:\SyncTest\Destination';

    // Créer les répertoires s'ils n'existent pas
    if not DirectoryExists(SourceDir) then
    begin
      TDirectory.CreateDirectory(SourceDir);
      WriteLog('Répertoire source créé: ' + SourceDir);
    end;

    if not DirectoryExists(DestDir) then
    begin
      TDirectory.CreateDirectory(DestDir);
      WriteLog('Répertoire destination créé: ' + DestDir);
    end;

    // Tester les opérations de fichiers
    TestFileOperations;

    Writeln('--------------------------------------------');
    Writeln('Tests terminés. Appuyez sur Entrée pour quitter...');
    Readln;
  except
    on E: Exception do
    begin
      Writeln('Erreur: ' + E.Message);
      Readln;
    end;
  end;
end.
```

## 11. Applications en tâche de fond avec tray icon et service combinés

Pour certaines applications, il peut être utile de combiner un service Windows avec une interface utilisateur minimale dans la zone de notification (tray icon). Voyons comment mettre en œuvre cette approche hybride.

### 11.1 Structure du projet

Nous allons créer :
1. Un service Windows pour les opérations en arrière-plan
2. Une application Windows classique avec une icône dans le systray
3. Un mécanisme de communication entre les deux

### 11.2 Implémentation du mécanisme de communication

```pascal
unit InterprocessCommUnit;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.Messages,
  System.JSON;

const
  WM_APP_COMMUNICATION = WM_USER + 1000;

  // Commandes
  CMD_GET_STATUS = 1;
  CMD_SET_CONFIG = 2;
  CMD_REQUEST_SYNC = 3;
  CMD_PAUSE_SERVICE = 4;
  CMD_RESUME_SERVICE = 5;

type
  TResponseCallback = reference to procedure(const Response: string);

  TInterprocessComm = class
  private
    FWindowHandle: HWND;
    FServiceWindowName: string;
    FCallback: TResponseCallback;

    procedure WndProc(var Message: TMessage);
  public
    constructor Create(const ServiceWindowName: string);
    destructor Destroy; override;

    function SendCommand(Command: Integer; const Data: string = ''): Boolean;
    function SendCommandWithResponse(Command: Integer; const Data: string;
      const Callback: TResponseCallback): Boolean;

    property ServiceWindowName: string read FServiceWindowName write FServiceWindowName;
  end;

implementation

constructor TInterprocessComm.Create(const ServiceWindowName: string);
begin
  inherited Create;
  FServiceWindowName := ServiceWindowName;

  // Créer une fenêtre cachée pour recevoir les réponses
  FWindowHandle := AllocateHWnd(WndProc);
end;

destructor TInterprocessComm.Destroy;
begin
  if FWindowHandle <> 0 then
    DeallocateHWnd(FWindowHandle);

  inherited;
end;

procedure TInterprocessComm.WndProc(var Message: TMessage);
var
  Response: string;
begin
  if Message.Msg = WM_APP_COMMUNICATION then
  begin
    // Récupérer la réponse (passée par copydata)
    if Message.LParam <> 0 then
    begin
      Response := PChar(Message.LParam);

      // Exécuter le callback avec la réponse
      if Assigned(FCallback) then
        FCallback(Response);

      // Libérer la mémoire allouée par l'expéditeur
      FreeMem(Pointer(Message.LParam));
    end;

    Message.Result := 1; // Message traité
  end
  else
    Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.WParam, Message.LParam);
end;

function TInterprocessComm.SendCommand(Command: Integer; const Data: string): Boolean;
var
  ServiceWindow: HWND;
  CopyData: TCopyDataStruct;
  DataStr: string;
  DataPtr: PChar;
  JsonObj: TJSONObject;
begin
  Result := False;

  // Chercher la fenêtre du service
  ServiceWindow := FindWindow(nil, PChar(FServiceWindowName));
  if ServiceWindow = 0 then
    Exit;

  // Préparer les données au format JSON
  JsonObj := TJSONObject.Create;
  try
    JsonObj.AddPair('command', TJSONNumber.Create(Command));

    if Data <> '' then
      JsonObj.AddPair('data', Data);

    DataStr := JsonObj.ToJSON;
  finally
    JsonObj.Free;
  end;

  // Copier les données dans un buffer
  DataPtr := StrNew(PChar(DataStr));

  // Configurer la structure CopyData
  CopyData.dwData := 0;
  CopyData.cbData := (Length(DataStr) + 1) * SizeOf(Char);
  CopyData.lpData := DataPtr;

  // Envoyer le message
  try
    Result := SendMessage(ServiceWindow, WM_COPYDATA, FWindowHandle, LPARAM(@CopyData)) = 1;
  finally
    StrDispose(DataPtr);
  end;
end;

function TInterprocessComm.SendCommandWithResponse(Command: Integer; const Data: string;
  const Callback: TResponseCallback): Boolean;
begin
  // Stocker le callback pour le traitement ultérieur
  FCallback := Callback;

  // Envoyer la commande
  Result := SendCommand(Command, Data);

  // Si l'envoi a échoué, le callback ne sera jamais appelé
  if not Result then
    FCallback := nil;
end;

end.
```

### 11.3 Implémentation du service avec fenêtre de communication

```pascal
unit HybridServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls,
  System.JSON;

const
  SERVICE_WINDOW_NAME = 'HybridServiceWindow';
  WM_APP_COMMUNICATION = WM_USER + 1000;

  // Commandes
  CMD_GET_STATUS = 1;
  CMD_SET_CONFIG = 2;
  CMD_REQUEST_SYNC = 3;
  CMD_PAUSE_SERVICE = 4;
  CMD_RESUME_SERVICE = 5;

type
  THybridService = class(TService)
    ServiceTimer: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceTimerTimer(Sender: TObject);
  private
    FWindowHandle: HWND;
    FLogFile: string;
    FIsPaused: Boolean;

    procedure CreateCommunicationWindow;
    procedure WndProc(var Message: TMessage);
    procedure WriteToLog(const Message: string);
    procedure HandleCommand(const Command: Integer; const Data: string;
      const ResponseWindow: HWND);
    procedure SendResponse(const Window: HWND; const Response: string);
    function GetStatusJSON: string;
  public
    function GetServiceController: TServiceController; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  HybridService: THybridService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  HybridService.Controller(CtrlCode);
end;

function THybridService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

constructor THybridService.Create(AOwner: TComponent);
begin
  inherited;
  FIsPaused := False;
end;

destructor THybridService.Destroy;
begin
  if FWindowHandle <> 0 then
    DeallocateHWnd(FWindowHandle);

  inherited;
end;

procedure THybridService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Créer la fenêtre de communication
  CreateCommunicationWindow;

  // Définir le chemin du fichier journal
  FLogFile := ExtractFilePath(ParamStr(0)) + 'hybrid_service.log';

  // Écrire un message au démarrage
  WriteToLog('Service hybride démarré à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Démarrer le timer
  ServiceTimer.Enabled := True;

  Started := True;
end;

procedure THybridService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  // Arrêter le timer
  ServiceTimer.Enabled := False;

  // Écrire un message d'arrêt
  WriteToLog('Service hybride arrêté à ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  // Libérer la fenêtre de communication
  if FWindowHandle <> 0 then
  begin
    DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
  end;

  Stopped := True;
end;

procedure THybridService.CreateCommunicationWindow;
begin
  // Créer une fenêtre cachée pour la communication interprocessus
  FWindowHandle := AllocateHWnd(WndProc);
  SetWindowText(FWindowHandle, PChar(SERVICE_WINDOW_NAME));

  WriteToLog('Fenêtre de communication créée');
end;

procedure THybridService.WndProc(var Message: TMessage);
var
  CopyData: PCopyDataStruct;
  Command: Integer;
  Data: string;
  JsonObj: TJSONObject;
begin
  if Message.Msg = WM_COPYDATA then
  begin
    // Récupérer les données copiées
    CopyData := PCopyDataStruct(Message.LParam);

    // Extraire la commande et les données du JSON
    try
      JsonObj := TJSONObject.ParseJSONValue(PChar(CopyData.lpData)) as TJSONObject;
      if JsonObj <> nil then
      begin
        try
          Command := JsonObj.GetValue<Integer>('command');

          if JsonObj.TryGetValue<string>('data', Data) then
            // La donnée existe
          else
            Data := '';

          // Traiter la commande
          HandleCommand(Command, Data, Message.WParam);
        finally
          JsonObj.Free;
        end;
      end;
    except
      on E: Exception do
        WriteToLog('Erreur lors du traitement du message: ' + E.Message);
    end;

    Message.Result := 1; // Message traité
  end
  else
    Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure THybridService.HandleCommand(const Command: Integer; const Data: string;
  const ResponseWindow: HWND);
var
  Response: string;
begin
  Response := '';

  try
    case Command of
      CMD_GET_STATUS:
        begin
          // Renvoyer l'état actuel du service
          Response := GetStatusJSON;
          WriteToLog('Statut demandé par l''application cliente');
        end;

      CMD_SET_CONFIG:
        begin
          // Appliquer une nouvelle configuration
          WriteToLog('Nouvelle configuration reçue: ' + Data);

          // Dans une application réelle, vous analyseriez le JSON et appliqueriez les changements
          Response := '{"success": true, "message": "Configuration mise à jour"}';
        end;

      CMD_REQUEST_SYNC:
        begin
          // Lancer une synchronisation immédiate
          WriteToLog('Synchronisation immédiate demandée');

          // Simuler une tâche immédiate
          ServiceTimerTimer(nil);

          Response := '{"success": true, "message": "Synchronisation lancée"}';
        end;

      CMD_PAUSE_SERVICE:
        begin
          // Mettre en pause les opérations du service
          FIsPaused := True;
          WriteToLog('Service mis en pause');

          Response := '{"success": true, "message": "Service mis en pause"}';
        end;

      CMD_RESUME_SERVICE:
        begin
          // Reprendre les opérations du service
          FIsPaused := False;
          WriteToLog('Service repris');

          Response := '{"success": true, "message": "Service repris"}';
        end;
    end;
  except
    on E: Exception do
    begin
      WriteToLog('Erreur lors du traitement de la commande: ' + E.Message);
      Response := Format('{"success": false, "error": "%s"}', [E.Message]);
    end;
  end;

  // Envoyer la réponse si nécessaire
  if (Response <> '') and (ResponseWindow <> 0) then
    SendResponse(ResponseWindow, Response);
end;

procedure THybridService.SendResponse(const Window: HWND; const Response: string);
var
  ResponsePtr: PChar;
begin
  // Allouer de la mémoire pour la réponse
  ResponsePtr := StrNew(PChar(Response));

  // Envoyer la réponse à la fenêtre cliente
  SendMessage(Window, WM_APP_COMMUNICATION, FWindowHandle, LPARAM(ResponsePtr));

  // Note: Ne pas libérer ResponsePtr ici, le destinataire s'en chargera
end;

function THybridService.GetStatusJSON: string;
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  try
    JsonObj.AddPair('running', TJSONBool.Create(ServiceTimer.Enabled));
    JsonObj.AddPair('paused', TJSONBool.Create(FIsPaused));
    JsonObj.AddPair('startTime', FormatDateTime('yyyy-mm-dd hh:nn:ss', StartTime));
    JsonObj.AddPair('lastActivity', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Ajouter d'autres informations d'état pertinentes

    Result := JsonObj.ToJSON;
  finally
    JsonObj.Free;
  end;
end;

procedure THybridService.WriteToLog(const Message: string);
var
  LogStream: TStreamWriter;
begin
  try
    // Créer ou ouvrir le fichier en mode append
    if FileExists(FLogFile) then
      LogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8)
    else
      LogStream := TStreamWriter.Create(FLogFile, False, TEncoding.UTF8);

    try
      LogStream.WriteLine(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now) + Message);
    finally
      LogStream.Free;
    end;
  except
    on E: Exception do
      // Écrire dans le journal d'événements Windows en cas d'erreur
      LogEvent(Format('Erreur d''écriture dans le fichier journal: %s', [E.Message]),
        EVENTLOG_ERROR_TYPE, 0, 0);
  end;
end;

procedure THybridService.ServiceTimerTimer(Sender: TObject);
begin
  // Ignorer si le service est en pause
  if FIsPaused then
    Exit;

  try
    // Effectuer la tâche périodique
    WriteToLog('Exécution de la tâche périodique');

    // Dans une application réelle, vous effectueriez ici le traitement
  except
    on E: Exception do
      WriteToLog('Erreur lors de l''exécution de la tâche périodique: ' + E.Message);
  end;
end;
```

### 11.4 Implémentation de l'application cliente avec icône systray

```pascal
unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls,
  Vcl.StdCtrls, System.JSON, InterprocessCommUnit;

type
  TMainForm = class(TForm)
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    miStatus: TMenuItem;
    miSeparator1: TMenuItem;
    miPauseResume: TMenuItem;
    miSyncNow: TMenuItem;
    miSeparator2: TMenuItem;
    miSettings: TMenuItem;
    miExit: TMenuItem;
    StatusMemo: TMemo;
    btnRefreshStatus: TButton;
    btnSyncNow: TButton;
    btnTogglePause: TButton;
    btnSettings: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miStatusClick(Sender: TObject);
    procedure miPauseResumeClick(Sender: TObject);
    procedure miSyncNowClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure btnRefreshStatusClick(Sender: TObject);
    procedure btnSyncNowClick(Sender: TObject);
    procedure btnTogglePauseClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FInterprocessComm: TInterprocessComm;
    FIsPaused: Boolean;

    procedure UpdateStatus;
    procedure DisplayStatus(const StatusJSON: string);
    procedure ShowError(const ErrorMessage: string);
    procedure InstallService;
    procedure UninstallService;
    function IsServiceInstalled: Boolean;
    function IsServiceRunning: Boolean;

  public
    { Public declarations }
  end;

const
  SERVICE_NAME = 'HybridService';
  SERVICE_WINDOW_NAME = 'HybridServiceWindow';

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser la communication inter-processus
  FInterprocessComm := TInterprocessComm.Create(SERVICE_WINDOW_NAME);

  FIsPaused := False;

  // Mettre à jour les éléments de l'interface
  UpdateStatus;

  // Définir le comportement initial du formulaire
  Width := 600;
  Height := 450;
  Caption := 'Hybrid Service Manager';

  // Configurer le mémo de statut
  StatusMemo.Clear;
  StatusMemo.Lines.Add('Connexion au service...');

  // Configurer le tray icon
  TrayIcon1.Hint := 'Hybrid Service Manager';
  TrayIcon1.Visible := True;

  // Vérifier l'état du service
  if not IsServiceInstalled then
  begin
    StatusMemo.Lines.Add('Le service n''est pas installé.');
    if Application.MessageBox('Le service n''est pas installé. Voulez-vous l''installer maintenant ?',
      'Installation du service', MB_YESNO or MB_ICONQUESTION) = IDYES then
    begin
      InstallService;
    end;
  end
  else if not IsServiceRunning then
  begin
    StatusMemo.Lines.Add('Le service est installé mais n''est pas en cours d''exécution.');
    if Application.MessageBox('Le service n''est pas en cours d''exécution. Voulez-vous le démarrer maintenant ?',
      'Démarrage du service', MB_YESNO or MB_ICONQUESTION) = IDYES then
    begin
      // Démarrer le service
      var SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
      if SCManager <> 0 then
      begin
        try
          var Service := OpenService(SCManager, PChar(SERVICE_NAME), SERVICE_START);
          if Service <> 0 then
          begin
            try
              if StartService(Service, 0, nil) then
                StatusMemo.Lines.Add('Service démarré avec succès.')
              else
                StatusMemo.Lines.Add('Erreur lors du démarrage du service: ' + SysErrorMessage(GetLastError));
            finally
              CloseServiceHandle(Service);
            end;
          end
          else
            StatusMemo.Lines.Add('Erreur lors de l''ouverture du service: ' + SysErrorMessage(GetLastError));
        finally
          CloseServiceHandle(SCManager);
        end;
      end
      else
        StatusMemo.Lines.Add('Erreur lors de l''ouverture du gestionnaire de services: ' + SysErrorMessage(GetLastError));
    end;
  end
  else
  begin
    // Mettre à jour le statut
    UpdateStatus;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer la communication inter-processus
  FInterprocessComm.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Minimiser plutôt que fermer si on n'utilise pas la touche Shift
  if not (GetKeyState(VK_SHIFT) < 0) then
  begin
    CanClose := False;
    Hide;
    TrayIcon1.ShowBalloonHint('Application minimisée',
      'L''application continue à s''exécuter en arrière-plan. ' +
      'Cliquez sur l''icône pour la restaurer.', btInfo, 10);
  end;
end;

procedure TMainForm.UpdateStatus;
begin
  // Vérifier l'état du service via la communication inter-processus
  FInterprocessComm.SendCommandWithResponse(CMD_GET_STATUS, '',
    procedure(const Response: string)
    begin
      // Mettre à jour l'interface dans le thread principal
      TThread.Queue(nil,
        procedure
        begin
          DisplayStatus(Response);
        end);
    end);
end;

procedure TMainForm.DisplayStatus(const StatusJSON: string);
var
  JsonObj: TJSONObject;
  IsRunning, IsPaused: Boolean;
  StartTime, LastActivity: string;
begin
  try
    JsonObj := TJSONObject.ParseJSONValue(StatusJSON) as TJSONObject;
    if JsonObj <> nil then
    begin
      try
        IsRunning := JsonObj.GetValue<Boolean>('running');
        IsPaused := JsonObj.GetValue<Boolean>('paused');
        StartTime := JsonObj.GetValue<string>('startTime');
        LastActivity := JsonObj.GetValue<string>('lastActivity');

        // Mettre à jour le mémo de statut
        StatusMemo.Clear;
        StatusMemo.Lines.Add('État du service:');
        StatusMemo.Lines.Add('- En cours d''exécution: ' + BoolToStr(IsRunning, True));
        StatusMemo.Lines.Add('- En pause: ' + BoolToStr(IsPaused, True));
        StatusMemo.Lines.Add('- Démarré le: ' + StartTime);
        StatusMemo.Lines.Add('- Dernière activité: ' + LastActivity);

        // Mettre à jour l'état de pause
        FIsPaused := IsPaused;

        // Mettre à jour le menu contextuel
        if FIsPaused then
        begin
          miPauseResume.Caption := 'Reprendre le service';
          btnTogglePause.Caption := 'Reprendre';
        end
        else
        begin
          miPauseResume.Caption := 'Mettre en pause le service';
          btnTogglePause.Caption := 'Mettre en pause';
        end;
      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
      ShowError('Erreur lors de l''analyse du statut: ' + E.Message);
  end;
end;

procedure TMainForm.ShowError(const ErrorMessage: string);
begin
  StatusMemo.Lines.Add('ERREUR: ' + ErrorMessage);

  // Afficher également une notification
  TrayIcon1.ShowBalloonHint('Erreur', ErrorMessage, btError, 5);
end;

procedure TMainForm.btnRefreshStatusClick(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TMainForm.btnSyncNowClick(Sender: TObject);
begin
  miSyncNowClick(Sender);
end;

procedure TMainForm.btnTogglePauseClick(Sender: TObject);
begin
  miPauseResumeClick(Sender);
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  miSettingsClick(Sender);
end;

procedure TMainForm.miStatusClick(Sender: TObject);
begin
  // Afficher la fenêtre principale
  Show;
  WindowState := wsNormal;
  Application.BringToFront;

  // Mettre à jour le statut
  UpdateStatus;
end;

procedure TMainForm.miPauseResumeClick(Sender: TObject);
var
  Command: Integer;
begin
  if FIsPaused then
    Command := CMD_RESUME_SERVICE
  else
    Command := CMD_PAUSE_SERVICE;

  // Envoyer la commande au service
  FInterprocessComm.SendCommandWithResponse(Command, '',
    procedure(const Response: string)
    begin
      // Mettre à jour l'interface dans le thread principal
      TThread.Queue(nil,
        procedure
        var
          JsonObj: TJSONObject;
          Success: Boolean;
          Message: string;
        begin
          try
            JsonObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
            if JsonObj <> nil then
            begin
              try
                Success := JsonObj.GetValue<Boolean>('success');
                Message := JsonObj.GetValue<string>('message');

                if Success then
                begin
                  StatusMemo.Lines.Add(Message);

                  // Inverser l'état de pause
                  FIsPaused := not FIsPaused;

                  // Mettre à jour le menu contextuel
                  if FIsPaused then
                  begin
                    miPauseResume.Caption := 'Reprendre le service';
                    btnTogglePause.Caption := 'Reprendre';
                  end
                  else
                  begin
                    miPauseResume.Caption := 'Mettre en pause le service';
                    btnTogglePause.Caption := 'Mettre en pause';
                  end;

                  // Mettre à jour le statut complet
                  UpdateStatus;
                end
                else
                  ShowError('Erreur: ' + Message);
              finally
                JsonObj.Free;
              end;
            end;
          except
            on E: Exception do
              ShowError('Erreur lors de l''analyse de la réponse: ' + E.Message);
          end;
        end);
    end);
end;

procedure TMainForm.miSyncNowClick(Sender: TObject);
begin
  // Envoyer la commande de synchronisation immédiate
  FInterprocessComm.SendCommandWithResponse(CMD_REQUEST_SYNC, '',
    procedure(const Response: string)
    begin
      // Mettre à jour l'interface dans le thread principal
      TThread.Queue(nil,
        procedure
        var
          JsonObj: TJSONObject;
          Success: Boolean;
          Message: string;
        begin
          try
            JsonObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
            if JsonObj <> nil then
            begin
              try
                Success := JsonObj.GetValue<Boolean>('success');
                Message := JsonObj.GetValue<string>('message');

                if Success then
                begin
                  StatusMemo.Lines.Add(Message);

                  // Notification
                  TrayIcon1.ShowBalloonHint('Synchronisation',
                    'La synchronisation a été lancée avec succès.', btInfo, 5);
                end
                else
                  ShowError('Erreur: ' + Message);
              finally
                JsonObj.Free;
              end;
            end;
          except
            on E: Exception do
              ShowError('Erreur lors de l''analyse de la réponse: ' + E.Message);
          end;
        end);
    end);
end;

procedure TMainForm.miSettingsClick(Sender: TObject);
begin
  // Dans une application réelle, vous afficheriez ici une boîte de dialogue de configuration
  ShowMessage('Cette fonctionnalité ouvrira une boîte de dialogue de configuration dans une application réelle.');

  // Exemple de configuration JSON à envoyer au service
  var ConfigJSON := '{"interval": 300, "logLevel": "debug", "autoSync": true}';

  FInterprocessComm.SendCommandWithResponse(CMD_SET_CONFIG, ConfigJSON,
    procedure(const Response: string)
    begin
      // Mettre à jour l'interface dans le thread principal
      TThread.Queue(nil,
        procedure
        var
          JsonObj: TJSONObject;
          Success: Boolean;
          Message: string;
        begin
          try
            JsonObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
            if JsonObj <> nil then
            begin
              try
                Success := JsonObj.GetValue<Boolean>('success');
                Message := JsonObj.GetValue<string>('message');

                if Success then
                begin
                  StatusMemo.Lines.Add('Configuration mise à jour: ' + Message);
                end
                else
                  ShowError('Erreur lors de la mise à jour de la configuration: ' + Message);
              finally
                JsonObj.Free;
              end;
            end;
          except
            on E: Exception do
              ShowError('Erreur lors de l''analyse de la réponse: ' + E.Message);
          end;
        end);
    end);
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  // Confirmer la fermeture
  if Application.MessageBox('Voulez-vous vraiment quitter l''application ?',
    'Confirmation', MB_YESNO or MB_ICONQUESTION) = IDYES then
  begin
    // Fermer l'application
    Application.Terminate;
  end;
end;

procedure TMainForm.InstallService;
var
  ExePath: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: string;
  CommandLinePtr: PChar;
begin
  // Obtenir le chemin du service
  ExePath := ChangeFileExt(Application.ExeName, '') + 'Service.exe';

  if not FileExists(ExePath) then
  begin
    ShowError('Fichier du service introuvable: ' + ExePath);
    Exit;
  end;

  // Préparer la commande d'installation
  CommandLine := Format('"%s" /install', [ExePath]);
  CommandLinePtr := PChar(CommandLine);

  // Initialiser les structures
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));

  // Lancer le processus d'installation
  if CreateProcess(nil, CommandLinePtr, nil, nil, False,
    CREATE_NO_WINDOW, nil, nil, StartupInfo, ProcessInfo) then
  begin
    // Attendre la fin du processus
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

    // Obtenir le code de sortie
    var ExitCode: DWORD;
    if GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) and (ExitCode = 0) then
      StatusMemo.Lines.Add('Service installé avec succès.')
    else
      StatusMemo.Lines.Add('Erreur lors de l''installation du service. Code: ' + IntToStr(ExitCode));

    // Fermer les handles
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
    ShowError('Erreur lors du lancement du processus d''installation: ' + SysErrorMessage(GetLastError));
end;

procedure TMainForm.UninstallService;
var
  ExePath: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: string;
  CommandLinePtr: PChar;
begin
  // Obtenir le chemin du service
  ExePath := ChangeFileExt(Application.ExeName, '') + 'Service.exe';

  if not FileExists(ExePath) then
  begin
    ShowError('Fichier du service introuvable: ' + ExePath);
    Exit;
  end;

  // Préparer la commande de désinstallation
  CommandLine := Format('"%s" /uninstall', [ExePath]);
  CommandLinePtr := PChar(CommandLine);

  // Initialiser les structures
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));

  // Lancer le processus de désinstallation
  if CreateProcess(nil, CommandLinePtr, nil, nil, False,
    CREATE_NO_WINDOW, nil, nil, StartupInfo, ProcessInfo) then
  begin
    // Attendre la fin du processus
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

    // Obtenir le code de sortie
    var ExitCode: DWORD;
    if GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) and (ExitCode = 0) then
      StatusMemo.Lines.Add('Service désinstallé avec succès.')
    else
      StatusMemo.Lines.Add('Erreur lors de la désinstallation du service. Code: ' + IntToStr(ExitCode));

    // Fermer les handles
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
    ShowError('Erreur lors du lancement du processus de désinstallation: ' + SysErrorMessage(GetLastError));
end;

function TMainForm.IsServiceInstalled: Boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
begin
  Result := False;

  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      // Essayer d'ouvrir le service
      Service := OpenService(SCManager, PChar(SERVICE_NAME), SERVICE_QUERY_STATUS);
      if Service <> 0 then
      begin
        Result := True;
        CloseServiceHandle(Service);
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

function TMainForm.IsServiceRunning: Boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  Result := False;

  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      // Essayer d'ouvrir le service
      Service := OpenService(SCManager, PChar(SERVICE_NAME), SERVICE_QUERY_STATUS);
      if Service <> 0 then
      begin
        try
          // Vérifier l'état du service
          if QueryServiceStatus(Service, ServiceStatus) then
            Result := ServiceStatus.dwCurrentState = SERVICE_RUNNING;
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

end.
```

## 12. Outils d'administration et de surveillance de services

Pour compléter notre exploration des services Windows, créons quelques outils utiles pour administrer et surveiller les services.

### 12.1 Outil de gestion multi-services

Voici un exemple d'application qui permet de gérer plusieurs services à la fois :

```pascal
unit ServiceManagerFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Winapi.WinSvc, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TServiceInfo = record
    Name: string;
    DisplayName: string;
    Description: string;
    Status: DWORD;
    StatusText: string;
    Startup: DWORD;
    StartupText: string;
    ProcessID: DWORD;
    Dependencies: TArray<string>;
  end;

  TServiceManagerForm = class(TForm)
    Panel1: TPanel;
    btnRefresh: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnPause: TButton;
    btnResume: TButton;
    Panel2: TPanel;
    ListView1: TListView;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    mnuView: TMenuItem;
    mnuRefresh: TMenuItem;
    mnuTools: TMenuItem;
    mnuSettings: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnResumeClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuRefreshClick(Sender: TObject);
    procedure mnuSettingsClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    FSelectedService: string;
    procedure UpdateControlsState;
    function GetServicesList: TArray<TServiceInfo>;
    function ControlService(const ServiceName: string; const Control: DWORD;
      var StatusText: string): Boolean;
    function GetServiceDescription(const ServiceName: string): string;
    function GetServiceDependencies(const ServiceName: string): TArray<string>;
  public
    { Public declarations }
  end;

var
  ServiceManagerForm: TServiceManagerForm;

implementation

{$R *.dfm}

procedure TServiceManagerForm.FormCreate(Sender: TObject);
begin
  // Configurer le ListView
  ListView1.ViewStyle := vsReport;

  // Ajouter les colonnes
  ListView1.Columns.Add.Caption := 'Service';
  ListView1.Columns.Add.Caption := 'Nom d''affichage';
  ListView1.Columns.Add.Caption := 'État';
  ListView1.Columns.Add.Caption := 'Démarrage';
  ListView1.Columns.Add.Caption := 'PID';
  ListView1.Columns.Add.Caption := 'Description';

  // Ajuster les largeurs des colonnes
  ListView1.Columns[0].Width := 150;
  ListView1.Columns[1].Width := 200;
  ListView1.Columns[2].Width := 100;
  ListView1.Columns[3].Width := 100;
  ListView1.Columns[4].Width := 60;
  ListView1.Columns[5].Width := 300;

  // Activer la sélection de ligne complète
  ListView1.RowSelect := True;

  // Définir le gestionnaire de sélection
  ListView1.OnSelectItem := ListView1SelectItem;

  // Initialiser l'état des contrôles
  FSelectedService := '';
  UpdateControlsState;

  // Charger la liste des services
  btnRefreshClick(nil);
end;

procedure TServiceManagerForm.btnRefreshClick(Sender: TObject);
var
  Services: TArray<TServiceInfo>;
  Service: TServiceInfo;
  Item: TListItem;
  ImageIndex: Integer;
begin
  // Sauvegarder le service sélectionné
  var SelectedService := FSelectedService;

  // Effacer la liste
  ListView1.Items.Clear;

  // Obtenir la liste des services
  Services := GetServicesList;

  // Remplir la liste
  for Service in Services do
  begin
    Item := ListView1.Items.Add;
    Item.Caption := Service.Name;
    Item.SubItems.Add(Service.DisplayName);
    Item.SubItems.Add(Service.StatusText);
    Item.SubItems.Add(Service.StartupText);

    if Service.ProcessID <> 0 then
      Item.SubItems.Add(IntToStr(Service.ProcessID))
    else
      Item.SubItems.Add('');

    Item.SubItems.Add(Service.Description);

    // Définir l'image selon l'état
    case Service.Status of
      SERVICE_RUNNING: ImageIndex := 0; // Vert
      SERVICE_STOPPED: ImageIndex := 1; // Rouge
      SERVICE_PAUSED: ImageIndex := 2;  // Jaune
      else ImageIndex := 3; // Gris
    end;

    Item.ImageIndex := ImageIndex;

    // Stocker le nom du service dans les données
    Item.Data := Pointer(StrNew(PChar(Service.Name)));

    // Restaurer la sélection si possible
    if Service.Name = SelectedService then
      Item.Selected := True;
  end;

  // Mettre à jour la barre d'état
  StatusBar1.SimpleText := Format('Services trouvés: %d', [Length(Services)]);

  // Mettre à jour l'état des contrôles
  UpdateControlsState;
end;

function TServiceManagerForm.GetServicesList: TArray<TServiceInfo>;
var
  SCManager: SC_HANDLE;
  BytesNeeded, ServicesReturned: DWORD;
  Buffer: PBYTE;
  ServiceStatus: LPENUM_SERVICE_STATUS;
  i: Integer;
  Service: SC_HANDLE;
  ServiceStatusProcess: TServiceStatusProcess;
  ServiceConfig: LPQUERY_SERVICE_CONFIG;
  ServiceConfigSize: DWORD;
  ServicesList: TList<TServiceInfo>;
begin
  ServicesList := TList<TServiceInfo>.Create;
  try
    // Ouvrir le gestionnaire de services
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
    if SCManager = 0 then
      raise Exception.Create('Erreur lors de l''ouverture du gestionnaire de services: ' +
        SysErrorMessage(GetLastError));

    try
      // Obtenir la taille du buffer nécessaire
      EnumServicesStatusEx(
        SCManager,
        SC_ENUM_PROCESS_INFO,
        SERVICE_WIN32,
        SERVICE_STATE_ALL,
        nil,
        0,
        BytesNeeded,
        ServicesReturned,
        nil,
        nil);

      // Allouer le buffer
      GetMem(Buffer, BytesNeeded);
      try
        // Énumérer les services
        if EnumServicesStatusEx(
            SCManager,
            SC_ENUM_PROCESS_INFO,
            SERVICE_WIN32,
            SERVICE_STATE_ALL,
            Buffer,
            BytesNeeded,
            BytesNeeded,
            ServicesReturned,
            nil,
            nil) then
        begin
          ServiceStatus := LPENUM_SERVICE_STATUS_PROCESS(Buffer);

          for i := 0 to ServicesReturned - 1 do
          begin
            var Info: TServiceInfo;
            Info.Name := ServiceStatus^.lpServiceName;
            Info.DisplayName := ServiceStatus^.lpDisplayName;
            Info.Status := ServiceStatus^.ServiceStatusProcess.dwCurrentState;
            Info.ProcessID := ServiceStatus^.ServiceStatusProcess.dwProcessId;

            // Définir le texte d'état
            case Info.Status of
              SERVICE_STOPPED: Info.StatusText := 'Arrêté';
              SERVICE_START_PENDING: Info.StatusText := 'Démarrage...';
              SERVICE_STOP_PENDING: Info.StatusText := 'Arrêt...';
              SERVICE_RUNNING: Info.StatusText := 'En cours d''exécution';
              SERVICE_CONTINUE_PENDING: Info.StatusText := 'Reprise...';
              SERVICE_PAUSE_PENDING: Info.StatusText := 'Mise en pause...';
              SERVICE_PAUSED: Info.StatusText := 'En pause';
              else Info.StatusText := 'Inconnu';
            end;

            // Obtenir des informations supplémentaires
            Service := OpenService(SCManager, PChar(Info.Name),
              SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);

            if Service <> 0 then
            begin
              try
                // Obtenir la taille du buffer pour la configuration
                QueryServiceConfig(Service, nil, 0, ServiceConfigSize);

                // Allouer le buffer
                GetMem(ServiceConfig, ServiceConfigSize);
                try
                  if QueryServiceConfig(Service, ServiceConfig, ServiceConfigSize, ServiceConfigSize) then
                  begin
                    // Obtenir le type de démarrage
                    Info.Startup := ServiceConfig^.dwStartType;

                    // Définir le texte de démarrage
                    case Info.Startup of
                      SERVICE_AUTO_START: Info.StartupText := 'Automatique';
                      SERVICE_DEMAND_START: Info.StartupText := 'Manuel';
                      SERVICE_DISABLED: Info.StartupText := 'Désactivé';
                      SERVICE_BOOT_START: Info.StartupText := 'Au démarrage';
                      SERVICE_SYSTEM_START: Info.StartupText := 'Système';
                      else Info.StartupText := 'Inconnu';
                    end;
                  end;
                finally
                  FreeMem(ServiceConfig);
                end;

                // Obtenir la description
                Info.Description := GetServiceDescription(Info.Name);

                // Obtenir les dépendances
                Info.Dependencies := GetServiceDependencies(Info.Name);
              finally
                CloseServiceHandle(Service);
              end;
            end;

            // Ajouter le service à la liste
            ServicesList.Add(Info);

            // Passer au service suivant
            Inc(ServiceStatus);
          end;
        end;
      finally
        FreeMem(Buffer);
      end;
    finally
      CloseServiceHandle(SCManager);
    end;

    // Trier la liste par nom
    ServicesList.Sort(
      function(const Left, Right: TServiceInfo): Integer
      begin
        Result := CompareText(Left.DisplayName, Right.DisplayName);
      end);

    Result := ServicesList.ToArray;
  finally
    ServicesList.Free;
  end;
end;

function TServiceManagerForm.GetServiceDescription(const ServiceName: string): string;
var
  SCManager, Service: SC_HANDLE;
  DescriptionBuffer: PBYTE;
  BytesNeeded: DWORD;
  ServiceDescription: LPSERVICE_DESCRIPTION;
begin
  Result := '';

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_QUERY_CONFIG);
      if Service <> 0 then
      begin
        try
          // Obtenir la taille du buffer nécessaire
          QueryServiceConfig2(Service, SERVICE_CONFIG_DESCRIPTION, nil, 0, BytesNeeded);

          if GetLastError = ERROR_INSUFFICIENT_BUFFER then
          begin
            // Allouer le buffer
            GetMem(DescriptionBuffer, BytesNeeded);
            try
              // Obtenir la description
              if QueryServiceConfig2(Service, SERVICE_CONFIG_DESCRIPTION,
                DescriptionBuffer, BytesNeeded, BytesNeeded) then
              begin
                ServiceDescription := LPSERVICE_DESCRIPTION(DescriptionBuffer);
                if Assigned(ServiceDescription^.lpDescription) then
                  Result := ServiceDescription^.lpDescription;
              end;
            finally
              FreeMem(DescriptionBuffer);
            end;
          end;
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

function TServiceManagerForm.GetServiceDependencies(const ServiceName: string): TArray<string>;
var
  SCManager, Service: SC_HANDLE;
  Buffer: PBYTE;
  BytesNeeded: DWORD;
  Dependencies: PChar;
  DependenciesList: TList<string>;
begin
  DependenciesList := TList<string>.Create;
  try
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
    if SCManager <> 0 then
    begin
      try
        Service := OpenService(SCManager, PChar(ServiceName), SERVICE_QUERY_CONFIG);
        if Service <> 0 then
        begin
          try
            // Obtenir la taille du buffer nécessaire
            QueryServiceConfig(Service, nil, 0, BytesNeeded);

            if GetLastError = ERROR_INSUFFICIENT_BUFFER then
            begin
              // Allouer le buffer
              GetMem(Buffer, BytesNeeded);
              try
                // Obtenir la configuration
                if QueryServiceConfig(Service, LPQUERY_SERVICE_CONFIG(Buffer),
                  BytesNeeded, BytesNeeded) then
                begin
                  Dependencies := LPQUERY_SERVICE_CONFIG(Buffer)^.lpDependencies;

                  // Parcourir les dépendances (double NULL-terminated)
                  if Dependencies <> nil then
                  begin
                    while Dependencies^ <> #0 do
                    begin
                      DependenciesList.Add(Dependencies);

                      // Passer à la prochaine chaîne
                      Inc(Dependencies, Length(Dependencies) + 1);
                    end;
                  end;
                end;
              finally
                FreeMem(Buffer);
              end;
            end;
          finally
            CloseServiceHandle(Service);
          end;
        end;
      finally
        CloseServiceHandle(SCManager);
      end;
    end;

    Result := DependenciesList.ToArray;
  finally
    DependenciesList.Free;
  end;
end;

procedure TServiceManagerForm.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item.Data) then
    FSelectedService := PChar(Item.Data)
  else if not Selected and (FSelectedService = PChar(Item.Data)) then
    FSelectedService := '';

  UpdateControlsState;
end;

procedure TServiceManagerForm.UpdateControlsState;
var
  CanStart, CanStop, CanPause, CanResume: Boolean;
  SCManager, Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  CanStart := False;
  CanStop := False;
  CanPause := False;
  CanResume := False;

  if FSelectedService <> '' then
  begin
    // Obtenir l'état actuel du service
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
    if SCManager <> 0 then
    begin
      try
        Service := OpenService(SCManager, PChar(FSelectedService),
          SERVICE_QUERY_STATUS or SERVICE_START or SERVICE_STOP or
          SERVICE_PAUSE_CONTINUE);

        if Service <> 0 then
        begin
          try
            if QueryServiceStatus(Service, ServiceStatus) then
            begin
              case ServiceStatus.dwCurrentState of
                SERVICE_STOPPED:
                  begin
                    CanStart := True;
                    CanStop := False;
                    CanPause := False;
                    CanResume := False;
                  end;
                SERVICE_RUNNING:
                  begin
                    CanStart := False;
                    CanStop := True;
                    CanPause := (ServiceStatus.dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE) <> 0;
                    CanResume := False;
                  end;
                SERVICE_PAUSED:
                  begin
                    CanStart := False;
                    CanStop := True;
                    CanPause := False;
                    CanResume := True;
                  end;
                else
                  begin
                    // État en transition, désactiver tous les boutons
                    CanStart := False;
                    CanStop := False;
                    CanPause := False;
                    CanResume := False;
                  end;
              end;
            end;
          finally
            CloseServiceHandle(Service);
          end;
        end;
      finally
        CloseServiceHandle(SCManager);
      end;
    end;
  end;

  // Mettre à jour l'état des boutons
  btnStart.Enabled := CanStart;
  btnStop.Enabled := CanStop;
  btnPause.Enabled := CanPause;
  btnResume.Enabled := CanResume;
end;

procedure TServiceManagerForm.btnStartClick(Sender: TObject);
var
  StatusText: string;
begin
  if FSelectedService <> '' then
  begin
    if ControlService(FSelectedService, SERVICE_CONTROL_START, StatusText) then
      StatusBar1.SimpleText := 'Service démarré: ' + FSelectedService
    else
      StatusBar1.SimpleText := 'Erreur lors du démarrage du service: ' + StatusText;

    // Rafraîchir la liste
    Sleep(1000); // Attendre un peu pour que le service change d'état
    btnRefreshClick(nil);
  end;
end;

procedure TServiceManagerForm.btnStopClick(Sender: TObject);
var
  StatusText: string;
begin
  if FSelectedService <> '' then
  begin
    // Confirmer l'arrêt
    if MessageDlg('Voulez-vous vraiment arrêter le service "' + FSelectedService + '" ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if ControlService(FSelectedService, SERVICE_CONTROL_STOP, StatusText) then
        StatusBar1.SimpleText := 'Service arrêté: ' + FSelectedService
      else
        StatusBar1.SimpleText := 'Erreur lors de l''arrêt du service: ' + StatusText;

      // Rafraîchir la liste
      Sleep(1000); // Attendre un peu pour que le service change d'état
      btnRefreshClick(nil);
    end;
  end;
end;

procedure TServiceManagerForm.btnPauseClick(Sender: TObject);
var
  StatusText: string;
begin
  if FSelectedService <> '' then
  begin
    if ControlService(FSelectedService, SERVICE_CONTROL_PAUSE, StatusText) then
      StatusBar1.SimpleText := 'Service mis en pause: ' + FSelectedService
    else
      StatusBar1.SimpleText := 'Erreur lors de la mise en pause du service: ' + StatusText;

    // Rafraîchir la liste
    Sleep(500); // Attendre un peu pour que le service change d'état
    btnRefreshClick(nil);
  end;
end;

procedure TServiceManagerForm.btnResumeClick(Sender: TObject);
var
  StatusText: string;
begin
  if FSelectedService <> '' then
  begin
    if ControlService(FSelectedService, SERVICE_CONTROL_CONTINUE, StatusText) then
      StatusBar1.SimpleText := 'Service repris: ' + FSelectedService
    else
      StatusBar1.SimpleText := 'Erreur lors de la reprise du service: ' + StatusText;

    // Rafraîchir la liste
    Sleep(500); // Attendre un peu pour que le service change d'état
    btnRefreshClick(nil);
  end;
end;

function TServiceManagerForm.ControlService(const ServiceName: string; const Control: DWORD;
  var StatusText: string): Boolean;
var
  SCManager, Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  Result := False;
  StatusText := '';

  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager = 0 then
  begin
    StatusText := 'Erreur lors de l''ouverture du gestionnaire de services: ' +
      SysErrorMessage(GetLastError);
    Exit;
  end;

  try
    // Ouvrir le service avec les droits nécessaires
    if Control = SERVICE_CONTROL_START then
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_START)
    else
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_STOP or
        SERVICE_PAUSE_CONTINUE);

    if Service = 0 then
    begin
      StatusText := 'Erreur lors de l''ouverture du service: ' +
        SysErrorMessage(GetLastError);
      Exit;
    end;

    try
      // Exécuter la commande
      if Control = SERVICE_CONTROL_START then
      begin
        if not StartService(Service, 0, nil) then
        begin
          StatusText := 'Erreur lors du démarrage du service: ' +
            SysErrorMessage(GetLastError);
          Exit;
        end;
      end
      else
      begin
        if not ControlService(Service, Control, ServiceStatus) then
        begin
          StatusText := 'Erreur lors du contrôle du service: ' +
            SysErrorMessage(GetLastError);
          Exit;
        end;
      end;

      Result := True;
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure TServiceManagerForm.mnuAboutClick(Sender: TObject);
begin
  MessageDlg('Gestionnaire de services' + sLineBreak +
    'Version 1.0' + sLineBreak +
    'Développé avec Delphi', mtInformation, [mbOK], 0);
end;

procedure TServiceManagerForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TServiceManagerForm.mnuRefreshClick(Sender: TObject);
begin
  btnRefreshClick(Sender);
end;

procedure TServiceManagerForm.mnuSettingsClick(Sender: TObject);
begin
  // Dans une application réelle, vous afficheriez ici une boîte de dialogue de configuration
  ShowMessage('Cette fonctionnalité ouvrira une boîte de dialogue de configuration dans une application réelle.');
end;
```

### 12.2 Création d'un moniteur de logs pour les services

Pour faciliter le débogage et la surveillance des services, créons un outil qui permet de visualiser les logs de plusieurs services :

```pascal
unit LogMonitorFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.Generics.Collections, System.IOUtils, Vcl.Menus;

type
  TLogType = (ltInfo, ltWarning, ltError, ltDebug);

  TLogEntry = class
  private
    FTimestamp: TDateTime;
    FService: string;
    FMessage: string;
    FLogType: TLogType;
  public
    constructor Create(const ATimestamp: TDateTime; const AService, AMessage: string;
      ALogType: TLogType);

    property Timestamp: TDateTime read FTimestamp;
    property Service: string read FService;
    property Message: string read FMessage;
    property LogType: TLogType read FLogType;
  end;

  TLogMonitorSettings = class
  private
    FLogDirectories: TList<string>;
    FAutoRefresh: Boolean;
    FRefreshInterval: Integer;
    FMaxEntries: Integer;
    FFilterTypes: TList<TLogType>;
    FFilterServices: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromRegistry;
    procedure SaveToRegistry;

    function ShouldShowLog(const LogEntry: TLogEntry): Boolean;

    property LogDirectories: TList<string> read FLogDirectories;
    property AutoRefresh: Boolean read FAutoRefresh write FAutoRefresh;
    property RefreshInterval: Integer read FRefreshInterval write FRefreshInterval;
    property MaxEntries: Integer read FMaxEntries write FMaxEntries;
    property FilterTypes: TList<TLogType> read FFilterTypes;
    property FilterServices: TList<string> read FFilterServices;
  end;

  TLogMonitorForm = class(TForm)
    Panel1: TPanel;
    cboServices: TComboBox;
    Label1: TLabel;
    btnRefresh: TButton;
    chkAutoRefresh: TCheckBox;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    ListView1: TListView;
    tmrRefresh: TTimer;
    PopupMenu1: TPopupMenu;
    mnuCopySelected: TMenuItem;
    mnuCopyAll: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuClear: TMenuItem;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCopy: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuView: TMenuItem;
    mnuRefresh: TMenuItem;
    mnuAutoRefresh: TMenuItem;
    mnuTools: TMenuItem;
    mnuSettings: TMenuItem;
    mnuFilter: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure mnuCopySelectedClick(Sender: TObject);
    procedure mnuCopyAllClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuSettingsClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuFilterClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cboServicesChange(Sender: TObject);
  private
    FSettings: TLogMonitorSettings;
    FLogEntries: TObjectList<TLogEntry>;
    FLastRefreshTime: TDateTime;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure RefreshLogs;
    procedure UpdateListView;
    procedure UpdateControls;
    function ParseLogFile(const FileName: string; const ServiceName: string): TArray<TLogEntry>;
    function DetectLogType(const LogLine: string): TLogType;
  public
    { Public declarations }
  end;

var
  LogMonitorForm: TLogMonitorForm;

implementation

{$R *.dfm}

{ TLogEntry }

constructor TLogEntry.Create(const ATimestamp: TDateTime; const AService, AMessage: string;
  ALogType: TLogType);
begin
  inherited Create;
  FTimestamp := ATimestamp;
  FService := AService;
  FMessage := AMessage;
  FLogType := ALogType;
end;

{ TLogMonitorSettings }

constructor TLogMonitorSettings.Create;
begin
  inherited;
  FLogDirectories := TList<string>.Create;
  FFilterTypes := TList<TLogType>.Create;
  FFilterServices := TList<string>.Create;

  // Valeurs par défaut
  FAutoRefresh := True;
  FRefreshInterval := 5000; // 5 secondes
  FMaxEntries := 1000;

  // Inclure tous les types de log par défaut
  FFilterTypes.Add(ltInfo);
  FFilterTypes.Add(ltWarning);
  FFilterTypes.Add(ltError);
  FFilterTypes.Add(ltDebug);

  // Ajouter des répertoires de log par défaut
  FLogDirectories.Add(ExtractFilePath(Application.ExeName));

  // Charger les paramètres
  LoadFromRegistry;
end;

destructor TLogMonitorSettings.Destroy;
begin
  FLogDirectories.Free;
  FFilterTypes.Free;
  FFilterServices.Free;
  inherited;
end;

procedure TLogMonitorSettings.LoadFromRegistry;
var
  Registry: TRegistry;
  Directories: TStringList;
  ServicesList: TStringList;
  TypesList: TStringList;
  i: Integer;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey('Software\YourCompany\LogMonitor', False) then
    begin
      if Registry.ValueExists('AutoRefresh') then
        FAutoRefresh := Registry.ReadBool('AutoRefresh');

      if Registry.ValueExists('RefreshInterval') then
        FRefreshInterval := Registry.ReadInteger('RefreshInterval');

      if Registry.ValueExists('MaxEntries') then
        FMaxEntries := Registry.ReadInteger('MaxEntries');

      if Registry.ValueExists('LogDirectories') then
      begin
        Directories := TStringList.Create;
        try
          Directories.Text := Registry.ReadString('LogDirectories');

          FLogDirectories.Clear;
          for i := 0 to Directories.Count - 1 do
          begin
            if DirectoryExists(Directories[i]) then
              FLogDirectories.Add(Directories[i]);
          end;
        finally
          Directories.Free;
        end;
      end;

      if Registry.ValueExists('FilterServices') then
      begin
        ServicesList := TStringList.Create;
        try
          ServicesList.Text := Registry.ReadString('FilterServices');

          FFilterServices.Clear;
          for i := 0 to ServicesList.Count - 1 do
            FFilterServices.Add(ServicesList[i]);
        finally
          ServicesList.Free;
        end;
      end;

      if Registry.ValueExists('FilterTypes') then
      begin
        TypesList := TStringList.Create;
        try
          TypesList.Text := Registry.ReadString('FilterTypes');

          FFilterTypes.Clear;
          for i := 0 to TypesList.Count - 1 do
          begin
            case StrToIntDef(TypesList[i], 0) of
              0: FFilterTypes.Add(ltInfo);
              1: FFilterTypes.Add(ltWarning);
              2: FFilterTypes.Add(ltError);
              3: FFilterTypes.Add(ltDebug);
            end;
          end;
        finally
          TypesList.Free;
        end;
      end;

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;

  // S'assurer qu'il y a au moins un répertoire
  if FLogDirectories.Count = 0 then
    FLogDirectories.Add(ExtractFilePath(Application.ExeName));

  // S'assurer qu'il y a au moins un type de log
  if FFilterTypes.Count = 0 then
  begin
    FFilterTypes.Add(ltInfo);
    FFilterTypes.Add(ltWarning);
    FFilterTypes.Add(ltError);
  end;
end;

procedure TLogMonitorSettings.SaveToRegistry;
var
  Registry: TRegistry;
  Directories: TStringList;
  ServicesList: TStringList;
  TypesList: TStringList;
  i: Integer;
  LogType: TLogType;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey('Software\YourCompany\LogMonitor', True) then
    begin
      Registry.WriteBool('AutoRefresh', FAutoRefresh);
      Registry.WriteInteger('RefreshInterval', FRefreshInterval);
      Registry.WriteInteger('MaxEntries', FMaxEntries);

      // Sauvegarder les répertoires de log
      Directories := TStringList.Create;
      try
        for i := 0 to FLogDirectories.Count - 1 do
          Directories.Add(FLogDirectories[i]);

        Registry.WriteString('LogDirectories', Directories.Text);
      finally
        Directories.Free;
      end;

      // Sauvegarder les services filtrés
      ServicesList := TStringList.Create;
      try
        for i := 0 to FFilterServices.Count - 1 do
          ServicesList.Add(FFilterServices[i]);

        Registry.WriteString('FilterServices', ServicesList.Text);
      finally
        ServicesList.Free;
      end;

      // Sauvegarder les types de log filtrés
      TypesList := TStringList.Create;
      try
        for LogType in FFilterTypes do
          TypesList.Add(IntToStr(Ord(LogType)));

        Registry.WriteString('FilterTypes', TypesList.Text);
      finally
        TypesList.Free;
      end;

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

function TLogMonitorSettings.ShouldShowLog(const LogEntry: TLogEntry): Boolean;
begin
  // Vérifier si le type de log est filtré
  Result := FFilterTypes.Contains(LogEntry.LogType);

  // Vérifier si le service est filtré
  if Result and (FFilterServices.Count > 0) then
    Result := Result and FFilterServices.Contains(LogEntry.Service);
end;

{ TLogMonitorForm }

procedure TLogMonitorForm.FormCreate(Sender: TObject);
begin
  // Créer les objets
  FSettings := TLogMonitorSettings.Create;
  FLogEntries := TObjectList<TLogEntry>.Create(True); // Posséder les objets

  // Configurer le ListView
  ListView1.ViewStyle := vsReport;

  // Ajouter les colonnes
  ListView1.Columns.Add.Caption := 'Heure';
  ListView1.Columns.Add.Caption := 'Service';
  ListView1.Columns.Add.Caption := 'Type';
  ListView1.Columns.Add.Caption := 'Message';

  // Ajuster les largeurs des colonnes
  ListView1.Columns[0].Width := 120;
  ListView1.Columns[1].Width := 100;
  ListView1.Columns[2].Width := 70;
  ListView1.Columns[3].Width := 500;

  // Charger les paramètres
  LoadSettings;

  // Mettre à jour les contrôles
  UpdateControls;

  // Rafraîchir les logs
  RefreshLogs;

  // Démarrer le timer si nécessaire
  tmrRefresh.Enabled := FSettings.AutoRefresh;
end;

procedure TLogMonitorForm.FormDestroy(Sender: TObject);
begin
  // Sauvegarder les paramètres
  SaveSettings;

  // Libérer les objets
  FSettings.Free;
  FLogEntries.Free;
end;

procedure TLogMonitorForm.LoadSettings;
begin
  // Mettre à jour les contrôles selon les paramètres
  chkAutoRefresh.Checked := FSettings.AutoRefresh;
  tmrRefresh.Interval := FSettings.RefreshInterval;

  // Rafraîchir la liste des services
  cboServices.Items.Clear;
  cboServices.Items.Add('(Tous les services)');

  // Initialiser la sélection
  cboServices.ItemIndex := 0;
end;

procedure TLogMonitorForm.SaveSettings;
begin
  // Mettre à jour les paramètres
  FSettings.AutoRefresh := chkAutoRefresh.Checked;

  // Sauvegarder dans le registre
  FSettings.SaveToRegistry;
end;

procedure TLogMonitorForm.RefreshLogs;
var
  LogDir: string;
  Files: TArray<string>;
  FileName: string;
  ServiceName: string;
  LogEntries: TArray<TLogEntry>;
  Entry: TLogEntry;
  i: Integer;
begin
  // Mémoriser l'heure du rafraîchissement
  FLastRefreshTime := Now;

  // Pour chaque répertoire configuré
  for LogDir in FSettings.LogDirectories do
  begin
    if DirectoryExists(LogDir) then
    begin
      // Trouver tous les fichiers log
      Files := TDirectory.GetFiles(LogDir, '*.log', TSearchOption.soTopDirectoryOnly);

      for FileName in Files do
      begin
        // Extraire le nom du service à partir du nom de fichier
        ServiceName := ChangeFileExt(ExtractFileName(FileName), '');

        // Si le service est filtré et n'est pas dans la liste, passer au suivant
        if (FSettings.FilterServices.Count > 0) and
           not FSettings.FilterServices.Contains(ServiceName) then
          Continue;

        // Analyser le fichier log
        LogEntries := ParseLogFile(FileName, ServiceName);

        // Ajouter les entrées à la liste principale
        for Entry in LogEntries do
        begin
          if FSettings.ShouldShowLog(Entry) then
            FLogEntries.Add(Entry);
        end;
      end;
    end;
  end;

  // Limiter le nombre d'entrées
  while FLogEntries.Count > FSettings.MaxEntries do
    FLogEntries.Delete(0); // Supprimer les plus anciennes entrées

  // Trier par timestamp (plus récent d'abord)
  FLogEntries.Sort(
    function(const Left, Right: TLogEntry): Integer
    begin
      // Tri inversé pour avoir les plus récentes en haut
      Result := CompareDateTime(Right.Timestamp, Left.Timestamp);
    end);

  // Mettre à jour l'affichage
  UpdateListView;

  // Mettre à jour la barre d'état
  StatusBar1.SimpleText := Format('Entrées de log: %d - Dernier rafraîchissement: %s',
    [FLogEntries.Count, FormatDateTime('dd/mm/yyyy hh:nn:ss', FLastRefreshTime)]);

  // Mettre à jour la liste des services dans le combobox
  var ServicesSet := TDictionary<string, Boolean>.Create;
  try
    // Collecter les services uniques
    for i := 0 to FLogEntries.Count - 1 do
      ServicesSet.AddOrSetValue(FLogEntries[i].Service, True);

    // Sauvegarder la sélection actuelle
    var SelectedService := cboServices.Text;

    // Effacer et reconstruire la liste
    cboServices.Items.Clear;
    cboServices.Items.Add('(Tous les services)');

    for ServiceName in ServicesSet.Keys do
      cboServices.Items.Add(ServiceName);

    // Restaurer la sélection
    var Index := cboServices.Items.IndexOf(SelectedService);
    if Index >= 0 then
      cboServices.ItemIndex := Index
    else
      cboServices.ItemIndex := 0;
  finally
    ServicesSet.Free;
  end;
end;

function TLogMonitorForm.ParseLogFile(const FileName: string; const ServiceName: string): TArray<TLogEntry>;
var
  Lines: TArray<string>;
  LogLine: string;
  LogType: TLogType;
  Timestamp: TDateTime;
  Message: string;
  EntryList: TList<TLogEntry>;
  i, StartPos: Integer;
  TimestampStr: string;
begin
  EntryList := TList<TLogEntry>.Create;
  try
    // Lire toutes les lignes du fichier
    if FileExists(FileName) then
    begin
      Lines := TFile.ReadAllLines(FileName);

      for LogLine in Lines do
      begin
        // Ignorer les lignes vides
        if Trim(LogLine) = '' then
          Continue;

        // Extraire l'horodatage
        TimestampStr := '';
        StartPos := 0;

        // Chercher le format d'horodatage [yyyy-mm-dd hh:nn:ss]
        if (Length(LogLine) > 20) and (LogLine[1] = '[') then
        begin
          i := 2;
          while (i < Length(LogLine)) and (LogLine[i] <> ']') do
          begin
            TimestampStr := TimestampStr + LogLine[i];
            Inc(i);
          end;

          if (i < Length(LogLine)) and (LogLine[i] = ']') then
            StartPos := i + 1;
        end;

        // Essayer de convertir l'horodatage
        if not TryStrToDateTime(TimestampStr, Timestamp) then
          Timestamp := Now; // Utiliser l'heure actuelle si l'analyse échoue

        // Extraire le message
        Message := Trim(Copy(LogLine, StartPos, Length(LogLine)));

        // Déterminer le type de log
        LogType := DetectLogType(LogLine);

        // Créer une nouvelle entrée de log
        EntryList.Add(TLogEntry.Create(Timestamp, ServiceName, Message, LogType));
      end;
    end;

    Result := EntryList.ToArray;
  finally
    EntryList.Free;
  end;
end;

function TLogMonitorForm.DetectLogType(const LogLine: string): TLogType;
var
  LowerLine: string;
begin

LowerLine := LowerCase(LogLine);

  // Détection simple basée sur des mots-clés
  if (Pos('error', LowerLine) > 0) or
     (Pos('exception', LowerLine) > 0) or
     (Pos('failed', LowerLine) > 0) or
     (Pos('erreur', LowerLine) > 0) or
     (Pos('échec', LowerLine) > 0) then
    Result := ltError
  else if (Pos('warning', LowerLine) > 0) or
          (Pos('attention', LowerLine) > 0) or
          (Pos('avertissement', LowerLine) > 0) then
    Result := ltWarning
  else if (Pos('debug', LowerLine) > 0) or
          (Pos('trace', LowerLine) > 0) or
          (Pos('verbose', LowerLine) > 0) then
    Result := ltDebug
  else
    Result := ltInfo;
end;

procedure TLogMonitorForm.UpdateListView;
var
  i: Integer;
  Entry: TLogEntry;
  Item: TListItem;
  SelectedService: string;
  Filter: Boolean;
begin
  // Obtenir le service sélectionné
  if cboServices.ItemIndex > 0 then
    SelectedService := cboServices.Text
  else
    SelectedService := '';

  // Mettre à jour le ListView
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    for i := 0 to FLogEntries.Count - 1 do
    begin
      Entry := FLogEntries[i];

      // Appliquer le filtre de service si nécessaire
      Filter := (SelectedService = '') or (Entry.Service = SelectedService);

      if Filter then
      begin
        Item := ListView1.Items.Add;
        Item.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', Entry.Timestamp);
        Item.SubItems.Add(Entry.Service);

        // Texte du type de log
        case Entry.LogType of
          ltInfo: Item.SubItems.Add('Info');
          ltWarning: Item.SubItems.Add('Attention');
          ltError: Item.SubItems.Add('Erreur');
          ltDebug: Item.SubItems.Add('Debug');
        end;

        Item.SubItems.Add(Entry.Message);

        // Couleur selon le type
        case Entry.LogType of
          ltWarning: Item.ImageIndex := 1; // Jaune
          ltError: Item.ImageIndex := 2;   // Rouge
          ltDebug: Item.ImageIndex := 3;   // Gris
          else Item.ImageIndex := 0;       // Vert (Info)
        end;
      end;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TLogMonitorForm.UpdateControls;
begin
  // Mettre à jour les contrôles selon les paramètres
  chkAutoRefresh.Checked := FSettings.AutoRefresh;
  tmrRefresh.Enabled := FSettings.AutoRefresh;
end;

procedure TLogMonitorForm.btnClearClick(Sender: TObject);
begin
  mnuClearClick(Sender);
end;

procedure TLogMonitorForm.btnRefreshClick(Sender: TObject);
begin
  RefreshLogs;
end;

procedure TLogMonitorForm.cboServicesChange(Sender: TObject);
begin
  // Mettre à jour l'affichage selon le service sélectionné
  UpdateListView;
end;

procedure TLogMonitorForm.chkAutoRefreshClick(Sender: TObject);
begin
  // Mettre à jour le paramètre
  FSettings.AutoRefresh := chkAutoRefresh.Checked;

  // Activer/désactiver le timer
  tmrRefresh.Enabled := FSettings.AutoRefresh;
end;

procedure TLogMonitorForm.tmrRefreshTimer(Sender: TObject);
begin
  // Rafraîchir les logs
  RefreshLogs;
end;

procedure TLogMonitorForm.mnuAboutClick(Sender: TObject);
begin
  MessageDlg('Moniteur de Logs' + sLineBreak +
    'Version 1.0' + sLineBreak +
    'Développé avec Delphi', mtInformation, [mbOK], 0);
end;

procedure TLogMonitorForm.mnuClearClick(Sender: TObject);
begin
  // Vider la liste des entrées
  FLogEntries.Clear;

  // Mettre à jour l'affichage
  UpdateListView;

  // Mettre à jour la barre d'état
  StatusBar1.SimpleText := 'Entrées effacées';
end;

procedure TLogMonitorForm.mnuCopyAllClick(Sender: TObject);
var
  Text: TStringList;
  i: Integer;
  Item: TListItem;
begin
  Text := TStringList.Create;
  try
    // Copier toutes les entrées
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      Item := ListView1.Items[i];
      Text.Add(Format('%s | %s | %s | %s',
        [Item.Caption, Item.SubItems[0], Item.SubItems[1], Item.SubItems[2]]));
    end;

    // Copier dans le presse-papiers
    Clipboard.AsText := Text.Text;

    // Mettre à jour la barre d'état
    StatusBar1.SimpleText := Format('%d entrées copiées dans le presse-papiers', [ListView1.Items.Count]);
  finally
    Text.Free;
  end;
end;

procedure TLogMonitorForm.mnuCopySelectedClick(Sender: TObject);
var
  Text: TStringList;
  i: Integer;
  Item: TListItem;
begin
  Text := TStringList.Create;
  try
    // Copier les entrées sélectionnées
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      Item := ListView1.Items[i];
      if Item.Selected then
      begin
        Text.Add(Format('%s | %s | %s | %s',
          [Item.Caption, Item.SubItems[0], Item.SubItems[1], Item.SubItems[2]]));
      end;
    end;

    // Copier dans le presse-papiers
    if Text.Count > 0 then
    begin
      Clipboard.AsText := Text.Text;

      // Mettre à jour la barre d'état
      StatusBar1.SimpleText := Format('%d entrées copiées dans le presse-papiers', [Text.Count]);
    end;
  finally
    Text.Free;
  end;
end;

procedure TLogMonitorForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TLogMonitorForm.mnuFilterClick(Sender: TObject);
var
  Dialog: TForm;
  chkInfo, chkWarning, chkError, chkDebug: TCheckBox;
  lbServices: TListBox;
  btnOK, btnCancel: TButton;
  i: Integer;
begin
  // Créer une boîte de dialogue pour les filtres
  Dialog := TForm.Create(Self);
  try
    Dialog.Caption := 'Filtres';
    Dialog.Width := 400;
    Dialog.Height := 400;
    Dialog.Position := poMainFormCenter;
    Dialog.BorderStyle := bsDialog;

    // Ajouter les contrôles pour les types de log
    TLabel.Create(Dialog).Parent := Dialog;
    with TLabel(Dialog.Components[0]) do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 16;
      Caption := 'Types de log à afficher :';
    end;

    chkInfo := TCheckBox.Create(Dialog);
    with chkInfo do
    begin
      Parent := Dialog;
      Left := 32;
      Top := 40;
      Width := 120;
      Height := 17;
      Caption := 'Information';
      Checked := FSettings.FilterTypes.Contains(ltInfo);
    end;

    chkWarning := TCheckBox.Create(Dialog);
    with chkWarning do
    begin
      Parent := Dialog;
      Left := 32;
      Top := 64;
      Width := 120;
      Height := 17;
      Caption := 'Avertissement';
      Checked := FSettings.FilterTypes.Contains(ltWarning);
    end;

    chkError := TCheckBox.Create(Dialog);
    with chkError do
    begin
      Parent := Dialog;
      Left := 32;
      Top := 88;
      Width := 120;
      Height := 17;
      Caption := 'Erreur';
      Checked := FSettings.FilterTypes.Contains(ltError);
    end;

    chkDebug := TCheckBox.Create(Dialog);
    with chkDebug do
    begin
      Parent := Dialog;
      Left := 32;
      Top := 112;
      Width := 120;
      Height := 17;
      Caption := 'Debug';
      Checked := FSettings.FilterTypes.Contains(ltDebug);
    end;

    // Ajouter les contrôles pour les services
    TLabel.Create(Dialog).Parent := Dialog;
    with TLabel(Dialog.Components[5]) do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 152;
      Caption := 'Services à afficher :';
    end;

    lbServices := TListBox.Create(Dialog);
    with lbServices do
    begin
      Parent := Dialog;
      Left := 32;
      Top := 176;
      Width := 320;
      Height := 150;
      Items.AddStrings(cboServices.Items);
      MultiSelect := True;

      // Sélectionner les services filtrés
      for i := 0 to Items.Count - 1 do
      begin
        if (Items[i] = '(Tous les services)') or
           FSettings.FilterServices.Contains(Items[i]) then
          Selected[i] := True;
      end;
    end;

    // Ajouter les boutons
    btnOK := TButton.Create(Dialog);
    with btnOK do
    begin
      Parent := Dialog;
      Left := 216;
      Top := 336;
      Width := 75;
      Height := 25;
      Caption := 'OK';
      Default := True;
      ModalResult := mrOK;
    end;

    btnCancel := TButton.Create(Dialog);
    with btnCancel do
    begin
      Parent := Dialog;
      Left := 304;
      Top := 336;
      Width := 75;
      Height := 25;
      Caption := 'Annuler';
      Cancel := True;
      ModalResult := mrCancel;
    end;

    // Afficher la boîte de dialogue
    if Dialog.ShowModal = mrOK then
    begin
      // Mettre à jour les filtres
      FSettings.FilterTypes.Clear;

      if chkInfo.Checked then
        FSettings.FilterTypes.Add(ltInfo);

      if chkWarning.Checked then
        FSettings.FilterTypes.Add(ltWarning);

      if chkError.Checked then
        FSettings.FilterTypes.Add(ltError);

      if chkDebug.Checked then
        FSettings.FilterTypes.Add(ltDebug);

      // Mettre à jour les services filtrés
      FSettings.FilterServices.Clear;

      for i := 0 to lbServices.Items.Count - 1 do
      begin
        if lbServices.Selected[i] and (lbServices.Items[i] <> '(Tous les services)') then
          FSettings.FilterServices.Add(lbServices.Items[i]);
      end;

      // Rafraîchir les logs
      RefreshLogs;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TLogMonitorForm.mnuSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  // Sélectionner tous les éléments
  for i := 0 to ListView1.Items.Count - 1 do
    ListView1.Items[i].Selected := True;
end;

procedure TLogMonitorForm.mnuSettingsClick(Sender: TObject);
var
  Dialog: TForm;
  edtInterval: TEdit;
  edtMaxEntries: TEdit;
  btnBrowse: TButton;
  mmoDirectories: TMemo;
  btnOK, btnCancel: TButton;
  DirDialog: TFileOpenDialog;
begin
  // Créer une boîte de dialogue pour les paramètres
  Dialog := TForm.Create(Self);
  try
    Dialog.Caption := 'Paramètres';
    Dialog.Width := 400;
    Dialog.Height := 400;
    Dialog.Position := poMainFormCenter;
    Dialog.BorderStyle := bsDialog;

    // Ajouter les contrôles pour l'intervalle de rafraîchissement
    TLabel.Create(Dialog).Parent := Dialog;
    with TLabel(Dialog.Components[0]) do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 16;
      Caption := 'Intervalle de rafraîchissement (ms) :';
    end;

    edtInterval := TEdit.Create(Dialog);
    with edtInterval do
    begin
      Parent := Dialog;
      Left := 240;
      Top := 13;
      Width := 80;
      Height := 21;
      Text := IntToStr(FSettings.RefreshInterval);
    end;

    // Ajouter les contrôles pour le nombre maximum d'entrées
    TLabel.Create(Dialog).Parent := Dialog;
    with TLabel(Dialog.Components[2]) do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 48;
      Caption := 'Nombre maximum d''entrées :';
    end;

    edtMaxEntries := TEdit.Create(Dialog);
    with edtMaxEntries do
    begin
      Parent := Dialog;
      Left := 240;
      Top := 45;
      Width := 80;
      Height := 21;
      Text := IntToStr(FSettings.MaxEntries);
    end;

    // Ajouter les contrôles pour les répertoires de log
    TLabel.Create(Dialog).Parent := Dialog;
    with TLabel(Dialog.Components[4]) do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 80;
      Caption := 'Répertoires de log :';
    end;

    mmoDirectories := TMemo.Create(Dialog);
    with mmoDirectories do
    begin
      Parent := Dialog;
      Left := 16;
      Top := 104;
      Width := 304;
      Height := 200;
      ScrollBars := ssVertical;

      // Remplir avec les répertoires existants
      for var Dir in FSettings.LogDirectories do
        Lines.Add(Dir);
    end;

    btnBrowse := TButton.Create(Dialog);
    with btnBrowse do
    begin
      Parent := Dialog;
      Left := 328;
      Top := 104;
      Width := 56;
      Height := 25;
      Caption := '...';
      OnClick :=
        procedure(Sender: TObject)
        begin
          DirDialog := TFileOpenDialog.Create(Dialog);
          try
            DirDialog.Options := [fdoPickFolders, fdoPathMustExist];
            DirDialog.Title := 'Sélectionner un répertoire de log';

            if DirDialog.Execute then
              mmoDirectories.Lines.Add(DirDialog.FileName);
          finally
            DirDialog.Free;
          end;
        end;
    end;

    // Ajouter les boutons
    btnOK := TButton.Create(Dialog);
    with btnOK do
    begin
      Parent := Dialog;
      Left := 216;
      Top := 336;
      Width := 75;
      Height := 25;
      Caption := 'OK';
      Default := True;
      ModalResult := mrOK;
    end;

    btnCancel := TButton.Create(Dialog);
    with btnCancel do
    begin
      Parent := Dialog;
      Left := 304;
      Top := 336;
      Width := 75;
      Height := 25;
      Caption := 'Annuler';
      Cancel := True;
      ModalResult := mrCancel;
    end;

    // Afficher la boîte de dialogue
    if Dialog.ShowModal = mrOK then
    begin
      // Mettre à jour les paramètres
      FSettings.RefreshInterval := StrToIntDef(edtInterval.Text, 5000);
      FSettings.MaxEntries := StrToIntDef(edtMaxEntries.Text, 1000);

      // Mettre à jour les répertoires
      FSettings.LogDirectories.Clear;

      for var Dir in mmoDirectories.Lines do
      begin
        if DirectoryExists(Dir) then
          FSettings.LogDirectories.Add(Dir);
      end;

      // Mettre à jour le timer
      tmrRefresh.Interval := FSettings.RefreshInterval;

      // Sauvegarder les paramètres
      FSettings.SaveToRegistry;

      // Rafraîchir les logs
      RefreshLogs;
    end;
  finally
    Dialog.Free;
  end;
end;
```

## 13. Bonnes pratiques et conseils pour les services Windows

### 13.1 Considérations de sécurité

Les services Windows s'exécutent généralement avec des privilèges système élevés, ce qui peut poser des risques de sécurité. Voici quelques bonnes pratiques à suivre :

#### Principe du moindre privilège

Configurez vos services pour s'exécuter avec un compte utilisateur dédié disposant uniquement des privilèges nécessaires, plutôt qu'avec le compte système.

```pascal
// Comment spécifier un compte utilisateur lors de la création d'un service
procedure InstallService(const ServiceName, DisplayName, Description, ExecutablePath: string;
  const Username, Password: string);
var
  SCManager, Service: SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCManager <> 0 then
  begin
    try
      Service := CreateService(
        SCManager,
        PChar(ServiceName),
        PChar(DisplayName),
        SERVICE_ALL_ACCESS,
        SERVICE_WIN32_OWN_PROCESS,
        SERVICE_AUTO_START,
        SERVICE_ERROR_NORMAL,
        PChar(ExecutablePath),
        nil, nil, nil,
        PChar(Username),  // Spécifier le compte utilisateur ici
        PChar(Password)   // Et le mot de passe ici
      );

      if Service <> 0 then
      begin
        try
          // Définir la description
          TService.SetDescription(SCManager, ServiceName, Description);
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;
```

#### Sécurisation des données sensibles

Ne stockez jamais de données sensibles en texte clair dans les fichiers de configuration, le registre ou les journaux :

```pascal
// Utiliser la protection des données Windows (DPAPI)
procedure SecureStorePassword(const Password: string; const FileName: string);
var
  PasswordBytes, EncryptedBytes: TBytes;
  DataBlob, EncryptedBlob: DATA_BLOB;
  Stream: TFileStream;
begin
  // Convertir le mot de passe en octets
  PasswordBytes := TEncoding.Unicode.GetBytes(Password);

  // Configurer le blob de données
  DataBlob.cbData := Length(PasswordBytes);
  DataBlob.pbData := @PasswordBytes[0];

  // Chiffrer les données
  if CryptProtectData(
      @DataBlob,
      'Password',
      nil,
      nil,
      nil,
      0,
      @EncryptedBlob) then
  begin
    try
      // Convertir le blob chiffré en tableau d'octets
      SetLength(EncryptedBytes, EncryptedBlob.cbData);
      Move(EncryptedBlob.pbData^, EncryptedBytes[0], EncryptedBlob.cbData);

      // Enregistrer dans un fichier
      Stream := TFileStream.Create(FileName, fmCreate);
      try
        Stream.WriteBuffer(EncryptedBytes[0], Length(EncryptedBytes));
      finally
        Stream.Free;
      end;
    finally
      // Libérer la mémoire allouée par CryptProtectData
      LocalFree(HLOCAL(EncryptedBlob.pbData));
    end;
  end;
end;

function SecureRetrievePassword(const FileName: string): string;
var
  EncryptedBytes, PasswordBytes: TBytes;
  DataBlob, DecryptedBlob: DATA_BLOB;
  Stream: TFileStream;
  FileSize: Int64;
begin
  Result := '';

  if not FileExists(FileName) then
    Exit;

  // Lire le fichier chiffré
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    FileSize := Stream.Size;
    SetLength(EncryptedBytes, FileSize);
    Stream.ReadBuffer(EncryptedBytes[0], FileSize);
  finally
    Stream.Free;
  end;

  // Configurer le blob chiffré
  DataBlob.cbData := Length(EncryptedBytes);
  DataBlob.pbData := @EncryptedBytes[0];

  // Déchiffrer les données
  if CryptUnprotectData(
      @DataBlob,
      nil,
      nil,
      nil,
      nil,
      0,
      @DecryptedBlob) then
  begin
    try
      // Convertir le blob déchiffré en chaîne
      SetLength(PasswordBytes, DecryptedBlob.cbData);
      Move(DecryptedBlob.pbData^, PasswordBytes[0], DecryptedBlob.cbData);
      Result := TEncoding.Unicode.GetString(PasswordBytes);
    finally
      // Libérer la mémoire allouée par CryptUnprotectData
      LocalFree(HLOCAL(DecryptedBlob.pbData));
    end;
  end;
end;
```

#### Validation des entrées

Validez toujours les entrées, en particulier celles provenant de sources externes :

```pascal
function IsFileSafe(const FileName: string): Boolean;
begin
  // Vérifier si le chemin contient des caractères suspects
  Result := not (
    (Pos('..', FileName) > 0) or
    (Pos('\\\', FileName) > 0) or
    (Pos(':', FileName) > 2) or  // Autoriser seulement le préfixe de lecteur (ex: C:)
    (Pos('*', FileName) > 0) or
    (Pos('?', FileName) > 0) or
    (Pos('<', FileName) > 0) or
    (Pos('>', FileName) > 0) or
    (Pos('|', FileName) > 0) or
    (Pos('"', FileName) > 0)
  );
end;
```

### 13.2 Journalisation et diagnostic

Une journalisation efficace est cruciale pour le diagnostic des problèmes dans les services :

#### Utilisation du journal d'événements Windows

```pascal
procedure LogToEventLog(const ServiceName, Message: string; EventType: DWord);
var
  EventSource: THandle;
  Strings: array[0..0] of PChar;
begin
  EventSource := RegisterEventSource(nil, PChar(ServiceName));
  if EventSource <> 0 then
  begin
    try
      Strings[0] := PChar(Message);
      ReportEvent(
        EventSource,
        EventType,
        0,
        0,
        nil,
        1,
        0,
        @Strings,
        nil
      );
    finally
      DeregisterEventSource(EventSource);
    end;
  end;
end;
```

#### Rotation des fichiers journaux

Évitez que vos fichiers journaux ne grossissent indéfiniment :

```pascal
procedure RotateLogFile(const LogFileName: string; const MaxSizeBytes: Int64);
var
  ArchiveFileName: string;
begin
  if FileExists(LogFileName) and (TFile.GetSize(LogFileName) > MaxSizeBytes) then
  begin
    // Créer un nom pour le fichier d'archives
    ArchiveFileName := ChangeFileExt(LogFileName,
      FormatDateTime('_yyyymmdd_hhnnss', Now) + ExtractFileExt(LogFileName));

    // Déplacer le fichier actuel vers les archives
    TFile.Move(LogFileName, ArchiveFileName);

    // Supprimer les anciens fichiers d'archives si nécessaire
    var LogDir := ExtractFilePath(LogFileName);
    var BaseName := ChangeFileExt(ExtractFileName(LogFileName), '');
    var Extension := ExtractFileExt(LogFileName);
    var OldFiles := TDirectory.GetFiles(LogDir, BaseName + '_*' + Extension);

    // Trier par date (du plus récent au plus ancien)
    TArray.Sort<string>(OldFiles,
      function(const Left, Right: string): Integer
      begin
        Result := CompareStr(Right, Left);
      end);

    // Garder seulement les 5 fichiers les plus récents
    for var i := 5 to Length(OldFiles) - 1 do
      TFile.Delete(OldFiles[i]);
  end;
end;
```

### 13.3 Récupération après erreur

Les services doivent pouvoir se remettre des erreurs sans intervention manuelle :

#### Paramètres de récupération Windows

Configurez les services pour redémarrer automatiquement en cas d'échec :

```pascal
procedure ConfigureServiceRecovery(const ServiceName: string);
var
  SCManager, Service: SC_HANDLE;
  Actions: array[0..2] of SC_ACTION;
  Config: SERVICE_FAILURE_ACTIONS;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_CHANGE_CONFIG);
      if Service <> 0 then
      begin
        try
          // Premier échec : redémarrer après 1 minute
          Actions[0].Type := SC_ACTION_RESTART;
          Actions[0].Delay := 60000; // 1 minute

          // Deuxième échec : redémarrer après 5 minutes
          Actions[1].Type := SC_ACTION_RESTART;
          Actions[1].Delay := 300000; // 5 minutes

          // Troisième échec et suivants : redémarrer après 15 minutes
          Actions[2].Type := SC_ACTION_RESTART;
          Actions[2].Delay := 900000; // 15 minutes

          // Configurer les actions de récupération
          ZeroMemory(@Config, SizeOf(Config));
          Config.dwResetPeriod := 86400; // 1 jour
          Config.lpRebootMsg := nil;
          Config.lpCommand := nil;
          Config.cActions := Length(Actions);
          Config.lpsaActions := @Actions[0];

          ChangeServiceConfig2(Service, SERVICE_CONFIG_FAILURE_ACTIONS, @Config);
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;
```

#### Mécanismes de reprise interne

Implémentez des mécanismes de reprise dans votre code pour les opérations critiques :

```pascal
procedure SafeExecuteOperation(const Operation: TProc; const MaxRetries: Integer);
var
  Retries: Integer;
  Success: Boolean;
begin
  Retries := 0;
  Success := False;

  while (not Success) and (Retries < MaxRetries) do
  begin
    try
      Operation();
      Success := True;
    except
      on E: Exception do
      begin
        Inc(Retries);
        LogToEventLog('MonService',
          Format('Erreur lors de l''opération (tentative %d/%d): %s',
            [Retries, MaxRetries, E.Message]),
          EVENTLOG_WARNING_TYPE);

        // Attendre avant de réessayer (avec temps d'attente exponentiel)
        Sleep(1000 * (1 shl Retries));
      end;
    end;
  end;

  if not Success then
    LogToEventLog('MonService',
      Format('Échec définitif de l''opération après %d tentatives', [MaxRetries]),
      EVENTLOG_ERROR_TYPE);
end;
```

### 13.4 Performance et ressources

Les services doivent être conçus pour fonctionner efficacement pendant de longues périodes :

#### Gestion de la mémoire

Évitez les fuites de mémoire :

```pascal
// Exemple de pool d'objets réutilisables pour éviter les allocations fréquentes
type
  TObjectPool<T: class, constructor> = class
  private
    FItems: TObjectList<T>;
    FMaxItems: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(const MaxItems: Integer);
    destructor Destroy; override;

    function Acquire: T;
    procedure Release(const Item: T);
  end;

constructor TObjectPool<T>.Create(const MaxItems: Integer);
begin
  inherited Create;
  FItems := TObjectList<T>.Create(False); // Ne pas posséder les objets
  FMaxItems := MaxItems;
  FLock := TCriticalSection.Create;
end;

destructor TObjectPool<T>.Destroy;
begin
  // Libérer tous les objets du pool
  FLock.Enter;
  try
    for var i := 0 to FItems.Count - 1 do
      FItems[i].Free;

    FItems.Clear;
    FItems.Free;
  finally
    FLock.Leave;
    FLock.Free;
  end;

  inherited;
end;

function TObjectPool<T>.Acquire: T;
begin
  FLock.Enter;
  try
    if FItems.Count > 0 then
    begin
      // Réutiliser un objet existant
      Result := FItems.Last;
      FItems.Delete(FItems.Count - 1);
    end
    else
    begin
      // Créer un nouvel objet
      Result := T.Create;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TObjectPool<T>.Release(const Item: T);
begin
  if Item = nil then
    Exit;

  FLock.Enter;
  try
    // Limiter la taille du pool
    if FItems.Count < FMaxItems then
      FItems.Add(Item)
    else
      Item.Free;
  finally
    FLock.Leave;
  end;
end;
```

#### Utilisation du CPU et optimisation

Minimisez l'utilisation du CPU pour les opérations de longue durée :

```pascal
procedure ProcessLargeFile(const FileName: string; const ProcessChunk: TProc<TBytes>);
const
  ChunkSize = 1024 * 1024; // 1 Mo par bloc
var
  Stream: TFileStream;
  Buffer: TBytes;
  BytesRead: Integer;
  StartTime: Cardinal;
begin
  if not FileExists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, ChunkSize);

    // Traiter le fichier par morceaux
    while Stream.Position < Stream.Size do
    begin
      StartTime := GetTickCount;

      // Lire un morceau du fichier
      BytesRead := Stream.Read(Buffer[0], ChunkSize);

      if BytesRead > 0 then
      begin
        // Ajuster la taille du buffer si nécessaire
        if BytesRead < ChunkSize then
          SetLength(Buffer, BytesRead);

        // Traiter le morceau
        ProcessChunk(Buffer);
      end;

      // Céder du temps CPU pour ne pas monopoliser le processeur
      if GetTickCount - StartTime < 100 then // Si le traitement a pris moins de 100 ms
        Sleep(1); // Céder du temps aux autres processus
    end;
  finally
    Stream.Free;
  end;
end;
```

### 13.5 Gestion des dépendances

Gérez efficacement les dépendances entre services :

```pascal
// Vérifier si un service dépendant est démarré
function IsServiceDependencyRunning(const DependentServiceName: string): Boolean;
var
  SCManager, Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  Result := False;

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      Service := OpenService(SCManager, PChar(DependentServiceName), SERVICE_QUERY_STATUS);
      if Service <> 0 then
      begin
        try
          if QueryServiceStatus(Service, ServiceStatus) then
            Result := ServiceStatus.dwCurrentState = SERVICE_RUNNING;
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

// Attendre qu'un service dépendant soit démarré
function WaitForDependentService(const DependentServiceName: string;
  const TimeoutMs: Integer): Boolean;
var
  StartTime: Cardinal;
begin
  Result := False;
  StartTime := GetTickCount;

  repeat
    if IsServiceDependencyRunning(DependentServiceName) then
    begin
      Result := True;
      Break;
    end;

    Sleep(100);  // Vérifier toutes les 100 ms
  until GetTickCount - StartTime > TimeoutMs;
end;
```

## 14. Projet avancé : Système de surveillance et de gestion à distance

Pour conclure ce chapitre, créons un projet complet de surveillance de système qui combine les concepts et techniques que nous avons appris. Ce système sera composé de trois parties :

1. Un service Windows pour la collecte des données et la surveillance
2. Une application cliente pour la visualisation et la gestion
3. Un composant de communication pour l'interaction entre le service et le client

### 14.1 Structure du projet

Voici la structure de notre système :

#### Service de surveillance (MonitoringService)
- Collecte des informations système (CPU, mémoire, disque, réseau)
- Surveillance des journaux d'événements Windows
- Surveillance des processus et services
- Génération d'alertes basées sur des règles configurables
- API de communication pour les clients distants

#### Application cliente (MonitoringClient)
- Interface utilisateur pour visualiser les données collectées
- Tableaux de bord et graphiques en temps réel
- Configuration du service et des règles d'alerte
- Journalisation et historique des alertes

#### Bibliothèque de communication (MonitoringComm)
- Protocole de communication client-service
- Sécurisation des communications
- Mécanismes de découverte automatique

### 14.2 Implémentation

Nous n'allons pas présenter le code complet de ce projet, car il serait beaucoup trop volumineux. Cependant, voici quelques extraits clés pour vous aider à comprendre l'architecture et l'implémentation :

#### Collecte des données système (dans le service)

```pascal
// Classe pour collecter les informations système
type
  TSystemMonitor = class
  private
    FLastCPUSample: Int64;
    FLastIdleSample: Int64;
    FLastCheckTime: TDateTime;

    function GetCPUUsage: Double;
    function GetMemoryInfo: TMemoryInfo;
    function GetDiskInfo: TArray<TDiskInfo>;
    function GetNetworkInfo: TArray<TNetworkInfo>;
    function GetProcessesList: TArray<TProcessInfo>;
    function GetServicesList: TArray<TServiceInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    function CollectSystemSnapshot: TSystemSnapshot;
    procedure StartPeriodicCollection(Interval: Integer);
    procedure StopPeriodicCollection;
  end;
```

#### Génération d'alertes basées sur des règles

```pascal
// Système de règles et d'alertes
type
  TAlertRule = class
  private
    FName: string;
    FDescription: string;
    FEnabled: Boolean;
    FCondition: TAlertCondition;
    FSeverity: TAlertSeverity;
  public
    constructor Create;
    destructor Destroy; override;

    function EvaluateSnapshot(const Snapshot: TSystemSnapshot): Boolean;
    function GenerateAlert(const Snapshot: TSystemSnapshot): TAlert;

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Condition: TAlertCondition read FCondition write FCondition;
    property Severity: TAlertSeverity read FSeverity write FSeverity;
  end;

  TAlertManager = class
  private
    FRules: TObjectList<TAlertRule>;
    FAlerts: TObjectList<TAlert>;
    FOnAlertGenerated: TAlertEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadRulesFromConfig(const ConfigFile: string);
    procedure SaveRulesToConfig(const ConfigFile: string);
    procedure ProcessSnapshot(const Snapshot: TSystemSnapshot);
    procedure AddRule(const Rule: TAlertRule);
    procedure ClearAlerts;

    property Rules: TObjectList<TAlertRule> read FRules;
    property Alerts: TObjectList<TAlert> read FAlerts;
    property OnAlertGenerated: TAlertEvent read FOnAlertGenerated write FOnAlertGenerated;
  end;
```

#### Communication service-client

```pascal
// Interface de communication entre le service et le client
type
  IMonitoringComm = interface
    ['{F8A5D6E8-1B7A-4D9C-9C18-3D2A9B6F8E5D}']
    // Côté service
    procedure StartListening(Port: Integer);
    procedure StopListening;
    procedure BroadcastSnapshot(const Snapshot: TSystemSnapshot);
    procedure BroadcastAlert(const Alert: TAlert);

    // Côté client
    function Connect(const ServerAddress: string; Port: Integer): Boolean;
    procedure Disconnect;
    function RequestLatestSnapshot: TSystemSnapshot;
    function RequestAlertHistory(Count: Integer): TArray<TAlert>;
    function RequestSystemConfig: TSystemConfig;
    function SendSystemConfig(const Config: TSystemConfig): Boolean;

    // Événements
    property OnClientConnected: TClientEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TClientEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnSnapshotReceived: TSnapshotEvent read FOnSnapshotReceived write FOnSnapshotReceived;
    property OnAlertReceived: TAlertEvent read FOnAlertReceived write FOnAlertReceived;
  end;

  // Implémentation TCP/IP
  TMonitoringCommTCP = class(TInterfacedObject, IMonitoringComm)
  private
    FServer: TIdTCPServer;
    FClient: TIdTCPClient;
    FIsServer: Boolean;
    FConnected: Boolean;
    FClients: TList<TIdTCPConnection>;
    FSecureProtocol: Boolean;

    // Événements
    FOnClientConnected: TClientEvent;
    FOnClientDisconnected: TClientEvent;
    FOnSnapshotReceived: TSnapshotEvent;
    FOnAlertReceived: TAlertEvent;

    procedure ServerExecute(AContext: TIdContext);
    procedure HandleClientRequest(AContext: TIdContext; const Command: string);
    function SerializeSnapshot(const Snapshot: TSystemSnapshot): string;
    function DeserializeSnapshot(const Data: string): TSystemSnapshot;
    function SerializeAlert(const Alert: TAlert): string;
    function DeserializeAlert(const Data: string): TAlert;
  public
    constructor Create(IsServer: Boolean; SecureProtocol: Boolean = True);
    destructor Destroy; override;

    // Implémentation de l'interface IMonitoringComm
    procedure StartListening(Port: Integer);
    procedure StopListening;
    procedure BroadcastSnapshot(const Snapshot: TSystemSnapshot);
    procedure BroadcastAlert(const Alert: TAlert);
    function Connect(const ServerAddress: string; Port: Integer): Boolean;
    procedure Disconnect;
    function RequestLatestSnapshot: TSystemSnapshot;
    function RequestAlertHistory(Count: Integer): TArray<TAlert>;
    function RequestSystemConfig: TSystemConfig;
    function SendSystemConfig(const Config: TSystemConfig): Boolean;

    // Propriétés
    property OnClientConnected: TClientEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TClientEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnSnapshotReceived: TSnapshotEvent read FOnSnapshotReceived write FOnSnapshotReceived;
    property OnAlertReceived: TAlertEvent read FOnAlertReceived write FOnAlertReceived;
  end;
```

#### Interface utilisateur du client

```pascal
// Formulaire principal du client de surveillance
type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    tabDashboard: TTabSheet;
    tabProcesses: TTabSheet;
    tabServices: TTabSheet;
    tabAlerts: TTabSheet;
    tabSettings: TTabSheet;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuConnect: TMenuItem;
    mnuDisconnect: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuExit: TMenuItem;
    mnuView: TMenuItem;
    mnuRefresh: TMenuItem;
    mnuTools: TMenuItem;
    mnuOptions: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    pnlCPU: TPanel;
    pnlMemory: TPanel;
    pnlDisk: TPanel;
    pnlNetwork: TPanel;
    CPUChart: TChart;
    MemoryChart: TChart;
    DiskChart: TChart;
    NetworkChart: TChart;
    lvProcesses: TListView;
    lvServices: TListView;
    lvAlerts: TListView;
    tmrRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuConnectClick(Sender: TObject);
    procedure mnuDisconnectClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuRefreshClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
  private
    FComm: IMonitoringComm;
    FLastSnapshot: TSystemSnapshot;
    FConnected: Boolean;

    procedure UpdateDashboard(const Snapshot: TSystemSnapshot);
    procedure UpdateProcessesList(const Snapshot: TSystemSnapshot);
    procedure UpdateServicesList(const Snapshot: TSystemSnapshot);
    procedure HandleNewAlert(const Alert: TAlert);
    procedure InitializeCharts;
    procedure UpdateCharts(const Snapshot: TSystemSnapshot);
    procedure UpdateStatusBar;
  public
    { Public declarations }
  end;
```

## 15. Résumé et bonnes pratiques

### 15.1 Quand utiliser les services Windows

Les services Windows sont idéaux pour :

- Les tâches qui doivent s'exécuter indépendamment des sessions utilisateur
- Les applications serveur qui doivent fonctionner en permanence
- Les tâches d'arrière-plan comme la surveillance du système, la sauvegarde ou la synchronisation
- Les applications qui doivent démarrer automatiquement avec Windows, avant la connexion des utilisateurs

### 15.2 Quand utiliser les applications de fond avec TrayIcon

Les applications de fond avec icône de notification sont plus adaptées pour :

- Les applications qui doivent interagir régulièrement avec l'utilisateur
- Les utilitaires qui nécessitent une interface utilisateur accessible
- Les applications qui n'ont pas besoin des privilèges élevés des services
- Les applications qui peuvent être arrêtées temporairement sans conséquences

### 15.3 Quand utiliser les tâches planifiées Windows

Les tâches planifiées sont préférables pour :

- Les tâches périodiques qui ne nécessitent pas d'exécution continue
- Les scripts ou programmes qui doivent s'exécuter à des moments précis
- Les tâches qui nécessitent des conditions spécifiques pour s'exécuter (ex: inactivité, connexion à une alimentation)
- Les tâches de maintenance qui n'ont pas besoin de s'exécuter en permanence

### 15.4 Liste de contrôle pour les services Windows

Avant de déployer un service Windows en production, vérifiez les points suivants :

✅ Le service gère correctement le démarrage, l'arrêt, la pause et la reprise
✅ Le service fonctionne avec le minimum de privilèges nécessaires
✅ Les erreurs sont correctement journalisées et gérées
✅ Une journalisation adéquate est mise en place pour le diagnostic
✅ Les données sensibles sont sécurisées
✅ Le service libère correctement toutes les ressources à la fermeture
✅ Des mécanismes de récupération sont configurés pour les échecs
✅ Le service gère élégamment les dépendances avec d'autres services
✅ La documentation inclut les instructions d'installation et de configuration
✅ Des tests de charge ont été effectués pour vérifier les performances à long terme

## Conclusion

Les services Windows et les applications de fond sont des outils puissants pour développer des solutions qui fonctionnent de manière transparente et continue sur les systèmes Windows. En utilisant Delphi, vous pouvez créer rapidement des services robustes et des applications de fond efficaces.

Ce chapitre vous a présenté les concepts fondamentaux et avancés des services Windows, ainsi que des exemples pratiques pour vous aider à développer vos propres solutions. Vous avez appris à créer, configurer, déboguer et optimiser des services, ainsi qu'à implémenter des applications de fond avec des fonctionnalités avancées de communication et de surveillance.

En appliquant les bonnes pratiques et les techniques présentées dans ce chapitre, vous serez en mesure de concevoir des solutions d'arrière-plan fiables, efficaces et sécurisées pour répondre aux besoins variés de vos clients ou de votre organisation.

> **Note** : Ce tutoriel utilise Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria. Les fonctionnalités spécifiques à Delphi 12 sont marquées comme telles.


## Exercices pratiques

Pour renforcer votre compréhension des services Windows et des applications de fond, voici quelques exercices pratiques :

### Exercice 1 : Service de sauvegarde automatique
Créez un service Windows qui surveille un répertoire spécifique et copie automatiquement les nouveaux fichiers ou les fichiers modifiés vers un emplacement de sauvegarde. Le service doit :
- Permettre la configuration des répertoires source et destination
- Journaliser toutes les opérations de sauvegarde
- Gérer les erreurs de manière appropriée
- Fournir une application cliente pour configurer le service

### Exercice 2 : Moniteur de performances
Créez une application de fond qui surveille les performances du système (CPU, mémoire, disque) et affiche des notifications lorsque certains seuils sont dépassés. L'application doit :
- S'exécuter dans la zone de notification
- Afficher des graphiques de l'utilisation des ressources
- Permettre à l'utilisateur de configurer les seuils d'alerte
- Enregistrer un historique des données de performance

### Exercice 3 : Service de synchronisation de fichiers
Créez un service qui synchronise des fichiers entre plusieurs ordinateurs sur un réseau local. Le service doit :
- Détecter automatiquement les autres instances sur le réseau
- Comparer les fichiers pour identifier les différences
- Synchroniser uniquement les fichiers modifiés
- Gérer les conflits de synchronisation
- Fournir une interface utilisateur pour configurer et surveiller la synchronisation

Ces exercices vous aideront à appliquer les concepts et techniques présentés dans ce chapitre, et à développer vos compétences dans la création de services Windows et d'applications de fond avec Delphi.

⏭️ [Applications cloud et SaaS](19-projets-avances/05-applications-cloud-et-saas.md)
