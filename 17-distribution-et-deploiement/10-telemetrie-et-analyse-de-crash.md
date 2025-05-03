# 17.10 Télémétrie et analyse de crash

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Imaginez que vous venez de publier une nouvelle version de votre application Delphi. Quelques jours plus tard, un utilisateur vous contacte en signalant un problème, mais il n'arrive pas à expliquer clairement les circonstances du bug. Comment savoir ce qui s'est réellement passé ? Comment découvrir si d'autres utilisateurs rencontrent le même problème sans qu'ils vous le signalent ? C'est là qu'interviennent la télémétrie et l'analyse de crash.

Dans ce chapitre, nous allons découvrir comment collecter des informations précieuses sur le comportement de votre application en production, détecter automatiquement les erreurs, et analyser ces données pour améliorer continuellement la qualité de votre logiciel. Nous aborderons ces concepts techniques de manière accessible, avec des exemples concrets pour les développeurs Delphi débutants.

## Qu'est-ce que la télémétrie et l'analyse de crash ?

### La télémétrie

La **télémétrie** consiste à collecter des données sur l'utilisation de votre application : quelles fonctionnalités sont utilisées, combien de temps, sur quels types d'appareils, etc. Ces données permettent de comprendre comment les utilisateurs interagissent réellement avec votre logiciel.

### L'analyse de crash

L'**analyse de crash** se concentre sur la collecte d'informations détaillées lorsque votre application rencontre une erreur inattendue (une exception non gérée, par exemple). Ces informations permettent de reproduire et corriger les problèmes que les utilisateurs rencontrent dans le monde réel.

## Pourquoi implémenter ces fonctionnalités ?

L'ajout de télémétrie et d'analyse de crash à votre application Delphi offre de nombreux avantages :

1. **Détection précoce des problèmes** : Identifiez les bugs avant que trop d'utilisateurs ne soient affectés
2. **Amélioration ciblée** : Concentrez vos efforts sur les fonctionnalités les plus utilisées
3. **Décisions basées sur des données** : Orientez le développement selon l'usage réel, pas seulement selon les retours explicites
4. **Support client amélioré** : Disposez d'informations précises lorsqu'un utilisateur signale un problème
5. **Qualité logicielle** : Augmentez la fiabilité de votre application en corrigeant les problèmes réels

## Considérations éthiques et légales

Avant d'implémenter ces fonctionnalités, il est essentiel de respecter la vie privée des utilisateurs et les réglementations en vigueur :

### RGPD et autres réglementations

La collecte de données utilisateur est soumise à des règles strictes, notamment le Règlement Général sur la Protection des Données (RGPD) en Europe :

1. **Consentement explicite** : Informez les utilisateurs et obtenez leur accord avant de collecter des données
2. **Anonymisation** : Évitez de collecter des informations permettant d'identifier personnellement les utilisateurs
3. **Finalité** : Ne collectez que les données nécessaires à l'amélioration de l'application
4. **Sécurité** : Protégez les données collectées contre les accès non autorisés
5. **Droit à l'oubli** : Permettez aux utilisateurs de supprimer leurs données

### Transparence et confiance

Au-delà des obligations légales, la transparence renforce la confiance des utilisateurs :

1. **Politique de confidentialité claire** : Expliquez quelles données sont collectées et pourquoi
2. **Options de désactivation** : Permettez aux utilisateurs de désactiver la télémétrie
3. **Bénéfices communiqués** : Montrez comment ces données améliorent leur expérience

## Implémentation de la télémétrie dans une application Delphi

Voyons maintenant comment ajouter la télémétrie à votre application Delphi, étape par étape.

### Étape 1 : Conception d'un système de télémétrie simple

Commençons par créer une unité de base pour la télémétrie :

```pascal
unit uTelemetry;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Generics.Collections;

type
  TTelemetryEvent = record
    EventName: string;
    EventTime: TDateTime;
    Properties: TDictionary<string, string>;
  end;

  TTelemetryManager = class
  private
    FEnabled: Boolean;
    FAnonymousID: string;
    FEvents: TList<TTelemetryEvent>;
    FServerURL: string;
    FSendInterval: Integer; // en minutes
    FSendTimer: TTimer;

    procedure SendEvents(Sender: TObject);
    function EventToJSON(const Event: TTelemetryEvent): TJSONObject;
  public
    constructor Create(const ServerURL: string; SendInterval: Integer = 30);
    destructor Destroy; override;

    procedure Initialize;
    procedure TrackEvent(const EventName: string); overload;
    procedure TrackEvent(const EventName: string; const Properties: TDictionary<string, string>); overload;
    procedure SetEnabled(Value: Boolean);

    property Enabled: Boolean read FEnabled write SetEnabled;
    property AnonymousID: string read FAnonymousID;
  end;

var
  // Instance globale pour faciliter l'utilisation
  Telemetry: TTelemetryManager;

implementation

uses
  System.Hash, System.IniFiles, System.IOUtils;

{ TTelemetryManager }

constructor TTelemetryManager.Create(const ServerURL: string; SendInterval: Integer);
begin
  inherited Create;
  FServerURL := ServerURL;
  FSendInterval := SendInterval;
  FEvents := TList<TTelemetryEvent>.Create;

  // Créer un timer pour envoyer périodiquement les événements
  FSendTimer := TTimer.Create(nil);
  FSendTimer.Interval := FSendInterval * 60 * 1000; // Convertir en millisecondes
  FSendTimer.OnTimer := SendEvents;
  FSendTimer.Enabled := False;
end;

destructor TTelemetryManager.Destroy;
begin
  FSendTimer.Free;
  FEvents.Free;
  inherited;
end;

procedure TTelemetryManager.Initialize;
var
  IniFile: TIniFile;
  ConfigFile: string;
begin
  // Générer un ID anonyme unique pour cet utilisateur
  ConfigFile := TPath.Combine(TPath.GetDocumentsPath, 'MaApplication\config.ini');

  if not TDirectory.Exists(ExtractFileDir(ConfigFile)) then
    TDirectory.CreateDirectory(ExtractFileDir(ConfigFile));

  IniFile := TIniFile.Create(ConfigFile);
  try
    FAnonymousID := IniFile.ReadString('Telemetry', 'AnonymousID', '');

    // Si pas d'ID, en générer un nouveau
    if FAnonymousID = '' then
    begin
      FAnonymousID := THashMD5.GetHashString(
        Format('%s%s%d', [GetComputerName, DateTimeToStr(Now), Random(10000)]));
      IniFile.WriteString('Telemetry', 'AnonymousID', FAnonymousID);
    end;

    // Lire la préférence utilisateur pour la télémétrie
    FEnabled := IniFile.ReadBool('Telemetry', 'Enabled', True);
  finally
    IniFile.Free;
  end;

  // Activer le timer si la télémétrie est activée
  FSendTimer.Enabled := FEnabled;
end;

procedure TTelemetryManager.TrackEvent(const EventName: string);
begin
  TrackEvent(EventName, nil);
end;

procedure TTelemetryManager.TrackEvent(const EventName: string;
  const Properties: TDictionary<string, string>);
var
  Event: TTelemetryEvent;
begin
  if not FEnabled then
    Exit;

  Event.EventName := EventName;
  Event.EventTime := Now;

  if Assigned(Properties) then
    Event.Properties := TDictionary<string, string>.Create(Properties)
  else
    Event.Properties := TDictionary<string, string>.Create;

  FEvents.Add(Event);
end;

procedure TTelemetryManager.SetEnabled(Value: Boolean);
var
  IniFile: TIniFile;
  ConfigFile: string;
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  FSendTimer.Enabled := FEnabled;

  // Sauvegarder la préférence
  ConfigFile := TPath.Combine(TPath.GetDocumentsPath, 'MaApplication\config.ini');
  IniFile := TIniFile.Create(ConfigFile);
  try
    IniFile.WriteBool('Telemetry', 'Enabled', FEnabled);
  finally
    IniFile.Free;
  end;

  // Si la télémétrie est désactivée, vider la liste d'événements
  if not FEnabled then
    FEvents.Clear;
end;

function TTelemetryManager.EventToJSON(const Event: TTelemetryEvent): TJSONObject;
var
  Obj: TJSONObject;
  PropPair: TPair<string, string>;
begin
  Obj := TJSONObject.Create;

  Obj.AddPair('event_name', Event.EventName);
  Obj.AddPair('event_time', DateTimeToStr(Event.EventTime));
  Obj.AddPair('anonymous_id', FAnonymousID);
  Obj.AddPair('app_version', GetAppVersionStr);

  // Ajouter les propriétés personnalisées
  for PropPair in Event.Properties do
    Obj.AddPair(PropPair.Key, PropPair.Value);

  Result := Obj;
end;

procedure TTelemetryManager.SendEvents(Sender: TObject);
var
  I: Integer;
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  JSONArray: TJSONArray;
  EventsCopy: TList<TTelemetryEvent>;
begin
  if (FEvents.Count = 0) or (not FEnabled) then
    Exit;

  // Créer une copie des événements pour éviter les problèmes de concurrence
  EventsCopy := TList<TTelemetryEvent>.Create;
  try
    for I := 0 to FEvents.Count - 1 do
      EventsCopy.Add(FEvents[I]);

    // Créer un tableau JSON avec tous les événements
    JSONArray := TJSONArray.Create;
    try
      for I := 0 to EventsCopy.Count - 1 do
        JSONArray.AddElement(EventToJSON(EventsCopy[I]));

      // Envoyer les données au serveur
      HTTP := THTTPClient.Create;
      try
        try
          Response := HTTP.Post(FServerURL,
            TStringStream.Create(JSONArray.ToString, TEncoding.UTF8));

          // Si l'envoi réussit, vider la liste d'événements originale
          if Response.StatusCode = 200 then
          begin
            for I := 0 to EventsCopy.Count - 1 do
              EventsCopy[I].Properties.Free;

            FEvents.Clear;
          end;
        except
          // En cas d'erreur, simplement ignorer et réessayer plus tard
          // Les événements restent dans la liste
        end;
      finally
        HTTP.Free;
      end;
    finally
      JSONArray.Free;
    end;
  finally
    // Nettoyer la copie (mais pas les dictionnaires de propriétés qui sont encore dans la liste originale)
    EventsCopy.Free;
  end;
end;

initialization
  Telemetry := TTelemetryManager.Create('https://api.votreapp.com/telemetry');

finalization
  Telemetry.Free;

end.
```

### Étape 2 : Intégration dans votre application

Utilisez cette unité dans votre application pour suivre les événements importants :

```pascal
uses
  uTelemetry;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser la télémétrie
  Telemetry.Initialize;

  // Suivre le démarrage de l'application
  Telemetry.TrackEvent('AppStart');
end;

procedure TMainForm.btnGenerateReportClick(Sender: TObject);
var
  Properties: TDictionary<string, string>;
begin
  // Générer le rapport...

  // Suivre l'événement avec des propriétés
  Properties := TDictionary<string, string>.Create;
  try
    Properties.Add('report_type', cbReportType.Text);
    Properties.Add('date_range', DateToStr(dtpStart.Date) + ' to ' + DateToStr(dtpEnd.Date));

    Telemetry.TrackEvent('ReportGenerated', Properties);
  finally
    Properties.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Suivre la fermeture de l'application
  Telemetry.TrackEvent('AppClose');
end;
```

### Étape 3 : Demande de consentement

Ajoutez une boîte de dialogue pour obtenir le consentement de l'utilisateur :

```pascal
procedure TMainForm.ShowTelemetryConsent;
var
  Response: Integer;
begin
  Response := MessageDlg(
    'Pour nous aider à améliorer cette application, nous aimerions collecter des données ' +
    'anonymes sur son utilisation. Aucune information personnelle ne sera collectée. ' +
    'Acceptez-vous de partager ces données ?',
    mtConfirmation, [mbYes, mbNo], 0);

  Telemetry.Enabled := (Response = mrYes);

  // Suivre la réponse elle-même
  if Telemetry.Enabled then
    Telemetry.TrackEvent('ConsentAccepted')
  else
    // Ne pas suivre le refus pour respecter le choix de l'utilisateur
    ; // Ne rien faire
end;
```

### Étape 4 : Création d'un backend simple

Du côté serveur, vous aurez besoin d'un point d'API pour recevoir les données. Voici un exemple simple avec PHP :

```php
<?php
// telemetry.php - Point d'entrée pour la télémétrie
header('Content-Type: application/json');

// Récupérer les données JSON envoyées
$jsonData = file_get_contents('php://input');
$events = json_decode($jsonData, true);

if (!$events) {
    http_response_code(400);
    echo json_encode(['status' => 'error', 'message' => 'Invalid JSON data']);
    exit;
}

// Ouvrir le fichier de log (ou utiliser une base de données dans un cas réel)
$logFile = 'telemetry_log_' . date('Y-m-d') . '.json';
$fp = fopen($logFile, 'a');

if (!$fp) {
    http_response_code(500);
    echo json_encode(['status' => 'error', 'message' => 'Could not open log file']);
    exit;
}

// Ajouter chaque événement au fichier
foreach ($events as $event) {
    // Ajouter un timestamp côté serveur
    $event['server_time'] = date('Y-m-d H:i:s');

    // Écrire l'événement dans le fichier
    fwrite($fp, json_encode($event) . "\n");
}

fclose($fp);

// Répondre avec succès
http_response_code(200);
echo json_encode(['status' => 'success', 'count' => count($events)]);
?>
```

## Implémentation de l'analyse de crash

Maintenant, abordons l'autre aspect important : la capture et l'analyse des erreurs imprévues (crashes).

### Étape 1 : Création d'un gestionnaire d'exceptions global

```pascal
unit uCrashHandler;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Net.HttpClient,
  Winapi.Windows, System.Zip;

type
  TCrashReporter = class
  private
    FServerURL: string;
    FApplicationName: string;
    FApplicationVersion: string;
    FEnabled: Boolean;
    FCrashLogPath: string;

    function CreateMinidump(const ExceptionInfo: TExceptionRecord): string;
    function CollectSystemInfo: string;
    function CollectApplicationState: string;
  public
    constructor Create(const ServerURL, AppName, AppVersion: string);

    procedure Initialize;
    procedure HandleException(Sender: TObject; E: Exception);
    function SendCrashReport(const CrashInfo, UserEmail, UserComments: string): Boolean;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  CrashReporter: TCrashReporter;

implementation

uses
  System.Diagnostics, System.JSON, Winapi.PsAPI;

{ TCrashReporter }

constructor TCrashReporter.Create(const ServerURL, AppName, AppVersion: string);
begin
  inherited Create;
  FServerURL := ServerURL;
  FApplicationName := AppName;
  FApplicationVersion := AppVersion;
  FCrashLogPath := TPath.Combine(TPath.GetDocumentsPath, 'MaApplication\Crashes');

  if not TDirectory.Exists(FCrashLogPath) then
    TDirectory.CreateDirectory(FCrashLogPath);
end;

procedure TCrashReporter.Initialize;
begin
  // Installer le gestionnaire d'exceptions
  Application.OnException := HandleException;
end;

// La fonction pour écrire un minidump (nécessite les API Windows)
function TCrashReporter.CreateMinidump(const ExceptionInfo: TExceptionRecord): string;
var
  DumpFileName: string;
  DumpFile: TFileStream;
  MiniDumpWriteDump: function(
    hProcess: THandle;
    ProcessId: DWORD;
    hFile: THandle;
    DumpType: DWORD;
    ExceptionParam: Pointer;
    UserStreamParam: Pointer;
    CallbackParam: Pointer): BOOL; stdcall;
  DbgHelp: HMODULE;
begin
  DumpFileName := TPath.Combine(FCrashLogPath,
    Format('%s_crash_%s.dmp', [FApplicationName, FormatDateTime('yyyymmdd_hhnnss', Now)]));

  Result := DumpFileName;

  // Charger la DLL DbgHelp
  DbgHelp := LoadLibrary('dbghelp.dll');
  if DbgHelp = 0 then
    Exit;

  try
    @MiniDumpWriteDump := GetProcAddress(DbgHelp, 'MiniDumpWriteDump');
    if not Assigned(MiniDumpWriteDump) then
      Exit;

    DumpFile := TFileStream.Create(DumpFileName, fmCreate);
    try
      // Créer le minidump
      MiniDumpWriteDump(
        GetCurrentProcess,
        GetCurrentProcessId,
        DumpFile.Handle,
        MiniDumpNormal, // Ou d'autres options pour plus de détails
        @ExceptionInfo,
        nil,
        nil);
    finally
      DumpFile.Free;
    end;
  finally
    FreeLibrary(DbgHelp);
  end;
end;

// Collecter des informations sur le système
function TCrashReporter.CollectSystemInfo: string;
var
  Info: TStringList;
  OSVersion: TOSVersion;
  MemoryStatus: TMemoryStatusEx;
begin
  Info := TStringList.Create;
  try
    // Informations sur le système d'exploitation
    OSVersion := TOSVersion.Create;
    Info.Add('[OS]');
    Info.Add(Format('Version=%d.%d.%d', [OSVersion.Major, OSVersion.Minor, OSVersion.Build]));
    Info.Add(Format('Platform=%s', [TOSVersion.ToString(OSVersion.Platform)]));

    // Informations sur la mémoire
    MemoryStatus.dwLength := SizeOf(MemoryStatus);
    if GlobalMemoryStatusEx(MemoryStatus) then
    begin
      Info.Add('[Memory]');
      Info.Add(Format('TotalPhysical=%d MB', [MemoryStatus.ullTotalPhys div (1024*1024)]));
      Info.Add(Format('AvailablePhysical=%d MB', [MemoryStatus.ullAvailPhys div (1024*1024)]));
      Info.Add(Format('MemoryLoad=%d%%', [MemoryStatus.dwMemoryLoad]));
    end;

    // Informations sur le processeur
    Info.Add('[CPU]');
    Info.Add(Format('ProcessorCount=%d', [TThread.ProcessorCount]));

    Result := Info.Text;
  finally
    Info.Free;
  end;
end;

// Collecter des informations sur l'état de l'application
function TCrashReporter.CollectApplicationState: string;
var
  Info: TStringList;
begin
  Info := TStringList.Create;
  try
    // Informations sur l'application
    Info.Add('[Application]');
    Info.Add(Format('Name=%s', [FApplicationName]));
    Info.Add(Format('Version=%s', [FApplicationVersion]));
    Info.Add(Format('ExePath=%s', [ParamStr(0)]));
    Info.Add(Format('CommandLine=%s', [GetCommandLine]));

    // Ajouter des informations sur les formes actives, l'état des connexions, etc.

    Result := Info.Text;
  finally
    Info.Free;
  end;
end;

// Gestionnaire d'exceptions principal
procedure TCrashReporter.HandleException(Sender: TObject; E: Exception);
var
  CrashInfo: TStringList;
  CrashLogFile: string;
  ExceptionInfo: TExceptionRecord;
  MiniDumpFile: string;
  CallStack: string;
  I: Integer;
begin
  if not FEnabled then
  begin
    // Si le gestionnaire de crash est désactivé, utiliser le comportement par défaut
    if Assigned(Application.OnException) then
      Application.OnException(Sender, E);
    Exit;
  end;

  // Préparer les informations sur le crash
  CrashInfo := TStringList.Create;
  try
    CrashInfo.Add('[Exception]');
    CrashInfo.Add(Format('Type=%s', [E.ClassName]));
    CrashInfo.Add(Format('Message=%s', [E.Message]));
    CrashInfo.Add(Format('Time=%s', [DateTimeToStr(Now)]));

    // Obtenir la pile d'appels
    if E is EExternal then
    begin
      FillChar(ExceptionInfo, SizeOf(ExceptionInfo), 0);
      ExceptionInfo.ExceptionCode := EExternal(E).ExceptionRecord.ExceptionCode;
      ExceptionInfo.ExceptionAddress := EExternal(E).ExceptionRecord.ExceptionAddress;

      MiniDumpFile := CreateMinidump(ExceptionInfo);
      CrashInfo.Add(Format('MiniDump=%s', [ExtractFileName(MiniDumpFile)]));
    end;

    // Ajouter les informations système et d'application
    CrashInfo.Add(CollectSystemInfo);
    CrashInfo.Add(CollectApplicationState);

    // Enregistrer le rapport de crash
    CrashLogFile := TPath.Combine(FCrashLogPath,
      Format('%s_crash_%s.log', [FApplicationName, FormatDateTime('yyyymmdd_hhnnss', Now)]));
    CrashInfo.SaveToFile(CrashLogFile);

    // Montrer un dialogue à l'utilisateur
    with TTaskDialog.Create(nil) do
    try
      Caption := 'Erreur inattendue';
      Title := 'L''application a rencontré un problème';
      Text := Format('Message d''erreur : %s', [E.Message]);
      CommonButtons := [tcbClose];
      MainIcon := tdiError;

      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := 'Envoyer un rapport d''erreur';
        ModalResult := 1000;
      end;

      if Execute then
      begin
        if ModalResult = 1000 then
        begin
          // Montrer un formulaire pour collecter plus d'informations et envoyer le rapport
          ShowCrashReportDialog(CrashLogFile, MiniDumpFile);
        end;
      end;
    finally
      Free;
    end;
  finally
    CrashInfo.Free;
  end;
end;

// Envoyer le rapport de crash au serveur
function TCrashReporter.SendCrashReport(const CrashInfo, UserEmail, UserComments: string): Boolean;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  ZipFile: string;
  ZipArchive: TZipFile;
  Files: TArray<string>;
  JSONObj: TJSONObject;
begin
  Result := False;

  // Créer un zip avec tous les fichiers du rapport
  ZipFile := TPath.Combine(FCrashLogPath, 'crash_report.zip');

  if FileExists(ZipFile) then
    DeleteFile(ZipFile);

  ZipArchive := TZipFile.Create;
  try
    ZipArchive.Open(ZipFile, zmWrite);

    // Ajouter les fichiers de crash au zip
    Files := TDirectory.GetFiles(FCrashLogPath, '*.log');
    for var FileName in Files do
      ZipArchive.Add(FileName);

    Files := TDirectory.GetFiles(FCrashLogPath, '*.dmp');
    for var FileName in Files do
      ZipArchive.Add(FileName);

    ZipArchive.Close;

    // Préparer l'objet JSON avec les métadonnées
    JSONObj := TJSONObject.Create;
    try
      JSONObj.AddPair('app_name', FApplicationName);
      JSONObj.AddPair('app_version', FApplicationVersion);
      JSONObj.AddPair('crash_info', CrashInfo);
      JSONObj.AddPair('user_email', UserEmail);
      JSONObj.AddPair('user_comments', UserComments);

      // Envoyer le fichier zip au serveur avec les métadonnées
      HTTP := THTTPClient.Create;
      try
        HTTP.CustomHeaders['X-Crash-Metadata'] := JSONObj.ToString;

        try
          Response := HTTP.Post(FServerURL, TFileStream.Create(ZipFile, fmOpenRead));
          Result := Response.StatusCode = 200;
        except
          // En cas d'erreur, simplement retourner False
        end;
      finally
        HTTP.Free;
      end;
    finally
      JSONObj.Free;
    end;
  finally
    ZipArchive.Free;
    if FileExists(ZipFile) then
      DeleteFile(ZipFile);
  end;
end;

// Formulaire pour collecter les commentaires de l'utilisateur et envoyer le rapport
procedure TCrashReporter.ShowCrashReportDialog(const CrashLogFile, MiniDumpFile: string);
var
  Form: TForm;
  // Implémentation du formulaire...
begin
  // Code pour afficher un formulaire et collecter les commentaires utilisateur
  // ...
end;

initialization
  CrashReporter := TCrashReporter.Create(
    'https://api.votreapp.com/crash-reports',
    'MaApplication',
    GetAppVersionStr);

finalization
  CrashReporter.Free;

end.
```

### Étape 2 : Intégration du gestionnaire de crash dans votre application

```pascal
uses
  uCrashHandler;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser le gestionnaire de crash
  CrashReporter.Enabled := True;
  CrashReporter.Initialize;

  // Reste du code d'initialisation...
end;

// Pour tester le gestionnaire de crash (à ne pas inclure en production !)
procedure TMainForm.btnTestCrashClick(Sender: TObject);
var
  Ptr: PInteger;
begin
  // Provoquer une violation d'accès mémoire
  Ptr := nil;
  Ptr^ := 0; // Ceci va provoquer une exception EAccessViolation
end;
```

### Étape 3 : Backend pour les rapports de crash

Comme pour la télémétrie, vous aurez besoin d'un point d'API pour recevoir les rapports de crash :

```php
<?php
// crash_reports.php - Point d'entrée pour les rapports de crash
header('Content-Type: application/json');

// Vérifier si un fichier a été téléchargé
if (!isset($_FILES['crash_report']) || $_FILES['crash_report']['error'] !== UPLOAD_ERR_OK) {
    http_response_code(400);
    echo json_encode(['status' => 'error', 'message' => 'No file uploaded']);
    exit;
}

// Récupérer les métadonnées
$metadata = json_decode($_SERVER['HTTP_X_CRASH_METADATA'] ?? '{}', true);
$appName = $metadata['app_name'] ?? 'unknown';
$appVersion = $metadata['app_version'] ?? 'unknown';

// Créer un dossier pour stocker les rapports de crash
$crashDir = 'crash_reports/' . $appName . '/' . $appVersion . '/' . date('Ymd_His');
if (!is_dir($crashDir)) {
    mkdir($crashDir, 0777, true);
}

// Déplacer le fichier zip téléchargé
$targetFile = $crashDir . '/crash_report.zip';
if (!move_uploaded_file($_FILES['crash_report']['tmp_name'], $targetFile)) {
    http_response_code(500);
    echo json_encode(['status' => 'error', 'message' => 'Failed to save file']);
    exit;
}


// Enregistrer les métadonnées dans un fichier JSON
$metadataFile = $crashDir . '/metadata.json';
file_put_contents($metadataFile, json_encode($metadata, JSON_PRETTY_PRINT));

// Extraire le contenu du zip pour analyse
$zip = new ZipArchive();
if ($zip->open($targetFile) === TRUE) {
    $zip->extractTo($crashDir);
    $zip->close();

    // Option : analyser automatiquement les fichiers de crash
    // analyzeMinidump($crashDir);
}

// Option : envoyer une notification
$to = 'developpeur@votreentreprise.com';
$subject = "Nouveau rapport de crash - $appName $appVersion";
$message = "Un nouveau rapport de crash a été reçu.\n\n";
$message .= "Application: $appName\n";
$message .= "Version: $appVersion\n";
$message .= "Date: " . date('Y-m-d H:i:s') . "\n";

if (!empty($metadata['user_email'])) {
    $message .= "Email utilisateur: " . $metadata['user_email'] . "\n";
}

if (!empty($metadata['user_comments'])) {
    $message .= "Commentaires utilisateur: " . $metadata['user_comments'] . "\n";
}

mail($to, $subject, $message);

// Répondre avec succès
http_response_code(200);
echo json_encode(['status' => 'success', 'message' => 'Crash report received']);
?>
```

## Analyse des données de télémétrie et de crash

Maintenant que vous collectez des données, comment les utiliser efficacement pour améliorer votre application ? Voici quelques approches :

### Analyse de la télémétrie

#### 1. Tableau de bord simple pour la télémétrie

Voici un exemple de tableau de bord PHP simple pour visualiser vos données de télémétrie :

```php
<?php
// dashboard.php - Tableau de bord de télémétrie simple
// Remarque : Dans un environnement de production, ajoutez une authentification !

// Fonction pour charger les événements de télémétrie
function loadTelemetryEvents($daysBack = 7) {
    $events = [];
    $startDate = new DateTime();
    $startDate->modify("-$daysBack days");

    for ($i = 0; $i <= $daysBack; $i++) {
        $date = clone $startDate;
        $date->modify("+$i days");
        $logFile = 'telemetry_log_' . $date->format('Y-m-d') . '.json';

        if (file_exists($logFile)) {
            $lines = file($logFile, FILE_IGNORE_NEW_LINES);
            foreach ($lines as $line) {
                $event = json_decode($line, true);
                if ($event) {
                    $events[] = $event;
                }
            }
        }
    }

    return $events;
}

// Charger les événements
$events = loadTelemetryEvents(30); // Derniers 30 jours

// Préparer les données pour les graphiques
$eventCounts = [];
$userCounts = [];
$versionDistribution = [];
$uniqueUsers = [];

foreach ($events as $event) {
    $date = substr($event['event_time'], 0, 10);
    $anonymousId = $event['anonymous_id'];
    $version = $event['app_version'];
    $eventName = $event['event_name'];

    // Compter par type d'événement
    if (!isset($eventCounts[$eventName])) {
        $eventCounts[$eventName] = 0;
    }
    $eventCounts[$eventName]++;

    // Compter par date
    if (!isset($userCounts[$date])) {
        $userCounts[$date] = [];
    }
    $userCounts[$date][$anonymousId] = true;

    // Distribution des versions
    if (!isset($versionDistribution[$version])) {
        $versionDistribution[$version] = 0;
    }
    $versionDistribution[$version]++;

    // Utilisateurs uniques
    $uniqueUsers[$anonymousId] = true;
}

// Trier les données
ksort($userCounts);
arsort($eventCounts);
arsort($versionDistribution);

// Calculer le nombre d'utilisateurs uniques par jour
foreach ($userCounts as $date => $users) {
    $userCounts[$date] = count($users);
}
?>

<!DOCTYPE html>
<html>
<head>
    <title>Tableau de bord de télémétrie</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .dashboard { display: flex; flex-wrap: wrap; }
        .chart-container { width: 48%; margin-bottom: 20px; }
        h1, h2 { color: #333; }
    </style>
</head>
<body>
    <h1>Tableau de bord de télémétrie</h1>

    <div class="summary">
        <p>Nombre total d'événements : <?= count($events) ?></p>
        <p>Utilisateurs uniques : <?= count($uniqueUsers) ?></p>
        <p>Période : <?= array_key_first($userCounts) ?> à <?= array_key_last($userCounts) ?></p>
    </div>

    <div class="dashboard">
        <div class="chart-container">
            <h2>Utilisateurs par jour</h2>
            <canvas id="usersChart"></canvas>
        </div>

        <div class="chart-container">
            <h2>Types d'événements</h2>
            <canvas id="eventsChart"></canvas>
        </div>

        <div class="chart-container">
            <h2>Distribution des versions</h2>
            <canvas id="versionsChart"></canvas>
        </div>
    </div>

    <script>
        // Graphique des utilisateurs par jour
        new Chart(document.getElementById('usersChart'), {
            type: 'line',
            data: {
                labels: <?= json_encode(array_keys($userCounts)) ?>,
                datasets: [{
                    label: 'Utilisateurs uniques',
                    data: <?= json_encode(array_values($userCounts)) ?>,
                    borderColor: 'rgb(75, 192, 192)',
                    tension: 0.1
                }]
            }
        });

        // Graphique des types d'événements
        new Chart(document.getElementById('eventsChart'), {
            type: 'bar',
            data: {
                labels: <?= json_encode(array_keys($eventCounts)) ?>,
                datasets: [{
                    label: 'Nombre d\'occurrences',
                    data: <?= json_encode(array_values($eventCounts)) ?>,
                    backgroundColor: 'rgba(54, 162, 235, 0.5)'
                }]
            }
        });

        // Graphique de distribution des versions
        new Chart(document.getElementById('versionsChart'), {
            type: 'pie',
            data: {
                labels: <?= json_encode(array_keys($versionDistribution)) ?>,
                datasets: [{
                    data: <?= json_encode(array_values($versionDistribution)) ?>,
                    backgroundColor: [
                        'rgba(255, 99, 132, 0.5)',
                        'rgba(54, 162, 235, 0.5)',
                        'rgba(255, 206, 86, 0.5)',
                        'rgba(75, 192, 192, 0.5)',
                        'rgba(153, 102, 255, 0.5)'
                    ]
                }]
            }
        });
    </script>
</body>
</html>
```

#### 2. Questions auxquelles la télémétrie peut répondre

Une bonne analyse de télémétrie permet de répondre à des questions comme :

- **Quelles fonctionnalités sont les plus utilisées ?** → Priorisez les améliorations
- **Quelles fonctionnalités sont rarement utilisées ?** → Simplifiez ou supprimez
- **Quand mes utilisateurs utilisent-ils l'application ?** → Planifiez les mises à jour
- **Quel est mon taux de conservation d'utilisateurs ?** → Mesurez l'engagement
- **Quelles versions sont encore utilisées ?** → Décidez quand abandonner le support

### Analyse des rapports de crash

#### 1. Interface d'analyse des crash

Voici un exemple simple d'interface pour analyser les rapports de crash :

```php
<?php
// crash_analyzer.php - Interface d'analyse des rapports de crash
// Remarque : Dans un environnement de production, ajoutez une authentification !

// Fonction pour lister tous les dossiers de rapports de crash
function listCrashReports() {
    $reports = [];
    $baseDir = 'crash_reports';

    if (!is_dir($baseDir)) {
        return $reports;
    }

    // Parcourir les applications
    $appDirs = glob("$baseDir/*", GLOB_ONLYDIR);
    foreach ($appDirs as $appDir) {
        $appName = basename($appDir);

        // Parcourir les versions
        $versionDirs = glob("$appDir/*", GLOB_ONLYDIR);
        foreach ($versionDirs as $versionDir) {
            $version = basename($versionDir);

            // Parcourir les rapports individuels
            $reportDirs = glob("$versionDir/*", GLOB_ONLYDIR);
            foreach ($reportDirs as $reportDir) {
                $timestamp = basename($reportDir);
                $metadataFile = "$reportDir/metadata.json";

                if (file_exists($metadataFile)) {
                    $metadata = json_decode(file_get_contents($metadataFile), true);

                    // Lire le fichier log pour obtenir le type d'exception
                    $exceptionType = 'Unknown';
                    $exceptionMessage = '';
                    $logFiles = glob("$reportDir/*.log");
                    if (!empty($logFiles)) {
                        $logContent = file_get_contents($logFiles[0]);

                        // Extraire le type d'exception
                        if (preg_match('/Type=(.+)/', $logContent, $matches)) {
                            $exceptionType = $matches[1];
                        }

                        // Extraire le message d'exception
                        if (preg_match('/Message=(.+)/', $logContent, $matches)) {
                            $exceptionMessage = $matches[1];
                        }
                    }

                    $reports[] = [
                        'app' => $appName,
                        'version' => $version,
                        'timestamp' => $timestamp,
                        'exception_type' => $exceptionType,
                        'exception_message' => $exceptionMessage,
                        'user_email' => $metadata['user_email'] ?? '',
                        'user_comments' => $metadata['user_comments'] ?? '',
                        'path' => $reportDir
                    ];
                }
            }
        }
    }

    // Trier par timestamp décroissant
    usort($reports, function($a, $b) {
        return strcmp($b['timestamp'], $a['timestamp']);
    });

    return $reports;
}

// Charger les rapports
$reports = listCrashReports();

// Regrouper par type d'exception
$exceptionGroups = [];
foreach ($reports as $report) {
    $type = $report['exception_type'];
    if (!isset($exceptionGroups[$type])) {
        $exceptionGroups[$type] = 0;
    }
    $exceptionGroups[$type]++;
}
arsort($exceptionGroups);

// Vérifier si on demande le détail d'un rapport
$selectedReport = null;
if (isset($_GET['report'])) {
    foreach ($reports as $report) {
        if ($report['path'] === $_GET['report']) {
            $selectedReport = $report;
            break;
        }
    }
}
?>

<!DOCTYPE html>
<html>
<head>
    <title>Analyseur de rapports de crash</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        th { background-color: #4CAF50; color: white; }
        .summary { margin-bottom: 20px; }
        .detail-container { margin-top: 20px; padding: 10px; border: 1px solid #ddd; }
        pre { background-color: #f5f5f5; padding: 10px; overflow: auto; }
    </style>
</head>
<body>
    <h1>Analyseur de rapports de crash</h1>

    <div class="summary">
        <h2>Résumé</h2>
        <p>Nombre total de rapports : <?= count($reports) ?></p>

        <h3>Types d'exceptions</h3>
        <table>
            <tr>
                <th>Type</th>
                <th>Nombre</th>
                <th>Pourcentage</th>
            </tr>
            <?php foreach ($exceptionGroups as $type => $count): ?>
            <tr>
                <td><?= htmlspecialchars($type) ?></td>
                <td><?= $count ?></td>
                <td><?= round(($count / count($reports)) * 100, 1) ?>%</td>
            </tr>
            <?php endforeach; ?>
        </table>
    </div>

    <h2>Liste des rapports</h2>
    <table>
        <tr>
            <th>Date</th>
            <th>Application</th>
            <th>Version</th>
            <th>Exception</th>
            <th>Message</th>
            <th>Action</th>
        </tr>
        <?php foreach ($reports as $report): ?>
        <tr>
            <td><?= str_replace('_', ' ', $report['timestamp']) ?></td>
            <td><?= htmlspecialchars($report['app']) ?></td>
            <td><?= htmlspecialchars($report['version']) ?></td>
            <td><?= htmlspecialchars($report['exception_type']) ?></td>
            <td><?= htmlspecialchars(substr($report['exception_message'], 0, 50)) ?></td>
            <td>
                <a href="?report=<?= urlencode($report['path']) ?>">Détails</a>
            </td>
        </tr>
        <?php endforeach; ?>
    </table>

    <?php if ($selectedReport): ?>
    <div class="detail-container">
        <h2>Détails du rapport</h2>
        <h3>Informations générales</h3>
        <table>
            <tr>
                <th>Application</th>
                <td><?= htmlspecialchars($selectedReport['app']) ?></td>
            </tr>
            <tr>
                <th>Version</th>
                <td><?= htmlspecialchars($selectedReport['version']) ?></td>
            </tr>
            <tr>
                <th>Date</th>
                <td><?= str_replace('_', ' ', $selectedReport['timestamp']) ?></td>
            </tr>
            <tr>
                <th>Type d'exception</th>
                <td><?= htmlspecialchars($selectedReport['exception_type']) ?></td>
            </tr>
            <tr>
                <th>Message</th>
                <td><?= htmlspecialchars($selectedReport['exception_message']) ?></td>
            </tr>
            <?php if (!empty($selectedReport['user_email'])): ?>
            <tr>
                <th>Email utilisateur</th>
                <td><?= htmlspecialchars($selectedReport['user_email']) ?></td>
            </tr>
            <?php endif; ?>
            <?php if (!empty($selectedReport['user_comments'])): ?>
            <tr>
                <th>Commentaires</th>
                <td><?= htmlspecialchars($selectedReport['user_comments']) ?></td>
            </tr>
            <?php endif; ?>
        </table>

        <h3>Fichier journal</h3>
        <?php
        $logFiles = glob($selectedReport['path'] . "/*.log");
        if (!empty($logFiles)):
            $logContent = file_get_contents($logFiles[0]);
        ?>
        <pre><?= htmlspecialchars($logContent) ?></pre>
        <?php else: ?>
        <p>Aucun fichier journal trouvé.</p>
        <?php endif; ?>

        <h3>Fichiers disponibles</h3>
        <ul>
            <?php
            $files = glob($selectedReport['path'] . "/*");
            foreach ($files as $file):
                if (is_file($file)):
            ?>
            <li>
                <a href="download.php?file=<?= urlencode($file) ?>"><?= basename($file) ?></a>
                (<?= round(filesize($file) / 1024) ?> KB)
            </li>
            <?php
                endif;
            endforeach;
            ?>
        </ul>
    </div>
    <?php endif; ?>
</body>
</html>
```

#### 2. Analyse des tendances de crash

Pour tirer le maximum des rapports de crash, cherchez à identifier :

- **Les patterns récurrents** : Mêmes exceptions dans différents contextes
- **Les corrélations avec des versions** : Problèmes introduits dans une version spécifique
- **Les problèmes matériels ou OS spécifiques** : Crashes uniquement sur certaines configurations
- **Les sections de code problématiques** : Points chauds dans votre application

## Solutions commerciales et bibliothèques tierces

Développer un système complet de télémétrie et d'analyse de crash peut être complexe. Heureusement, il existe des solutions prêtes à l'emploi :

### Bibliothèques Delphi pour la télémétrie

1. **EurekaLog** : Solution complète de gestion des exceptions et de reporting
   - [https://www.eurekalog.com/](https://www.eurekalog.com/)
   - Capture détaillée des exceptions
   - Envoi automatique des rapports
   - Débogage post-mortem

2. **MadExcept** : Autre solution populaire
   - [http://madshi.net/madExceptDescription.htm](http://madshi.net/madExceptDescription.htm)
   - Rapports de crash détaillés
   - Support du débogage à distance

3. **LogBox** : Pour la télémétrie et la journalisation
   - [https://github.com/jkour/LogBox](https://github.com/jkour/LogBox)
   - Open-source
   - Plusieurs destinations de journalisation

### Services tiers d'analyse de crash et de télémétrie

1. **AppCenter** de Microsoft
   - [https://appcenter.ms/](https://appcenter.ms/)
   - Analytics et rapports de crash
   - Distributions de tests

2. **Bugsnag**
   - [https://www.bugsnag.com/](https://www.bugsnag.com/)
   - Suivi des erreurs en temps réel
   - Priorisation des bugs

3. **Firebase Crashlytics**
   - [https://firebase.google.com/products/crashlytics](https://firebase.google.com/products/crashlytics)
   - Rapports de crash détaillés
   - Gratuit, mais orienté mobile

Pour intégrer ces services tiers, vous devrez généralement utiliser leurs API et bibliothèques. La plupart fournissent des exemples d'intégration pour différents langages.

## Respect de la vie privée et bonnes pratiques

### Checklist de conformité

Pour s'assurer que votre collecte de données est éthique et légale :

1. **Transparence**
   - Informez clairement vos utilisateurs sur les données collectées
   - Expliquez comment ces données sont utilisées

2. **Consentement**
   - Obtenez un consentement explicite avant la collecte
   - Option de désactivation facilement accessible

3. **Minimisation des données**
   - Ne collectez que ce dont vous avez réellement besoin
   - Évitez les informations personnelles sensibles

4. **Sécurité**
   - Chiffrez les données en transit (HTTPS)
   - Protégez l'accès à votre backend d'analyse

### Exemple de boîte de dialogue de consentement améliorée

```pascal
function ShowPrivacyConsentDialog: Boolean;
var
  TaskDialog: TTaskDialog;
  Result: Integer;
begin
  TaskDialog := TTaskDialog.Create(nil);
  try
    TaskDialog.Caption := 'Confidentialité des données';
    TaskDialog.Title := 'Aider à améliorer l''application';
    TaskDialog.Text :=
      'Pour nous aider à améliorer cette application, nous aimerions collecter ' +
      'des données anonymes sur son utilisation et d''éventuels problèmes rencontrés.' + sLineBreak + sLineBreak +
      'Aucune information personnelle identifiable ne sera collectée. ' +
      'Les données seront uniquement utilisées pour améliorer la qualité et la stabilité de l''application.';

    TaskDialog.ExpandedText :=
      'Données collectées : ' + sLineBreak +
      '- Fonctionnalités utilisées' + sLineBreak +
      '- Performance de l''application' + sLineBreak +
      '- Erreurs et crashes' + sLineBreak +
      '- Informations système (OS, résolution)' + sLineBreak + sLineBreak +
      'Vous pouvez modifier ce choix à tout moment dans Paramètres > Confidentialité.';

    TaskDialog.ExpandButtonCaption := 'Plus d''informations';
    TaskDialog.CommonButtons := [];
    TaskDialog.MainIcon := tdiInformation;
    TaskDialog.FooterText := 'Votre vie privée est importante pour nous.';

    // Ajouter des boutons personnalisés
    with TTaskDialogButtonItem(TaskDialog.Buttons.Add) do
    begin
      Caption := 'J''accepte';
      ModalResult := mrYes;
    end;

    with TTaskDialogButtonItem(TaskDialog.Buttons.Add) do
    begin
      Caption := 'Je refuse';
      ModalResult := mrNo;
    end;

    // Ajouter un lien vers la politique de confidentialité complète
    TaskDialog.FooterIcon := tdiInformation;
    TaskDialog.FooterText :=
      '<a href="https://www.votreapp.com/privacy">Consulter notre politique de confidentialité complète</a>';

    Result := TaskDialog.Execute;
    Result := (Result = mrYes);
  finally
    TaskDialog.Free;
  end;
end;
```

### Gestion des préférences utilisateur

Ajoutez un écran de paramètres permettant aux utilisateurs de gérer leurs préférences de confidentialité :

```pascal
procedure TFormSettings.UpdatePrivacyControls;
begin
  chkTelemetry.Checked := Telemetry.Enabled;
  chkCrashReporting.Checked := CrashReporter.Enabled;
end;

procedure TFormSettings.chkTelemetryClick(Sender: TObject);
begin
  Telemetry.Enabled := chkTelemetry.Checked;
end;

procedure TFormSettings.chkCrashReportingClick(Sender: TObject);
begin
  CrashReporter.Enabled := chkCrashReporting.Checked;
end;

procedure TFormSettings.btnDeleteDataClick(Sender: TObject);
begin
  if MessageDlg('Êtes-vous sûr de vouloir supprimer toutes vos données de télémétrie ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Envoyer une demande de suppression au serveur
    DeleteTelemetryData(Telemetry.AnonymousID);
    ShowMessage('Vos données ont été supprimées.');
  end;
end;

procedure TFormSettings.lnkPrivacyPolicyClick(Sender: TObject);
begin
  // Ouvrir la politique de confidentialité
  ShellExecute(0, 'open', 'https://www.votreapp.com/privacy', nil, nil, SW_SHOWNORMAL);
end;
```

## Exercice pratique : Implémentation de télémétrie de base

Pour mettre en pratique ce que vous avez appris, suivez ces étapes pour ajouter une télémétrie simple à votre application Delphi :

1. Créez une unité `uSimpleTelemetry` basée sur l'exemple simplifié suivant :

```pascal
unit uSimpleTelemetry;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.JSON;

type
  TSimpleTelemetry = class
  private
    FEnabled: Boolean;
    FEventsLog: TStringList;
    FLogFile: string;
    FAnonymousID: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure TrackEvent(const EventName: string);
    procedure SaveEvents;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  SimpleTelemetry: TSimpleTelemetry;

implementation

uses
  System.IOUtils, System.Hash;

{ TSimpleTelemetry }

constructor TSimpleTelemetry.Create;
begin
  inherited Create;
  FEventsLog := TStringList.Create;
  FLogFile := TPath.Combine(TPath.GetDocumentsPath, 'VotreApp\telemetry.log');

  // Créer le dossier si nécessaire
  if not TDirectory.Exists(ExtractFilePath(FLogFile)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogFile));
end;

destructor TSimpleTelemetry.Destroy;
begin
  SaveEvents;
  FEventsLog.Free;
  inherited;
end;

procedure TSimpleTelemetry.Initialize;
var
  IniFile: TIniFile;
  ConfigFile: string;
begin
  // Charger ou générer l'ID anonyme
  ConfigFile := TPath.Combine(ExtractFilePath(FLogFile), 'config.ini');

  IniFile := TIniFile.Create(ConfigFile);
  try
    FAnonymousID := IniFile.ReadString('Telemetry', 'AnonymousID', '');

    if FAnonymousID = '' then
    begin
      FAnonymousID := THashMD5.GetHashString(
        Format('%s%s%d', [GetComputerName, DateTimeToStr(Now), Random(10000)]));
      IniFile.WriteString('Telemetry', 'AnonymousID', FAnonymousID);
    end;

    FEnabled := IniFile.ReadBool('Telemetry', 'Enabled', True);
  finally
    IniFile.Free;
  end;

  // Charger les événements existants
  if FileExists(FLogFile) then
    FEventsLog.LoadFromFile(FLogFile);
end;

procedure TSimpleTelemetry.TrackEvent(const EventName: string);
var
  Event: TJSONObject;
begin
  if not FEnabled then
    Exit;

  Event := TJSONObject.Create;
  try
    Event.AddPair('event', EventName);
    Event.AddPair('time', DateTimeToStr(Now));
    Event.AddPair('id', FAnonymousID);

    FEventsLog.Add(Event.ToString);
  finally
    Event.Free;
  end;

  // Sauvegarder périodiquement
  if FEventsLog.Count mod 10 = 0 then
    SaveEvents;
end;

procedure TSimpleTelemetry.SaveEvents;
begin
  if FEventsLog.Count > 0 then
    FEventsLog.SaveToFile(FLogFile);
end;

initialization
  SimpleTelemetry := TSimpleTelemetry.Create;

finalization
  SimpleTelemetry.Free;

end.
```

2. Intégrez cette unité dans votre application :

```pascal
uses
  uSimpleTelemetry;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SimpleTelemetry.Initialize;
  SimpleTelemetry.TrackEvent('AppStart');

  // Afficher une boîte de dialogue de consentement si c'est la première utilisation
  // ...
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SimpleTelemetry.TrackEvent('AppClose');
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  // Exécuter l'action de sauvegarde...

  SimpleTelemetry.TrackEvent('FileSaved');
end;
```

3. Créez un outil simple pour analyser les données collectées :

```pascal
procedure TAnalyzerForm.btnAnalyzeClick(Sender: TObject);
var
  LogFile: string;
  Events: TStringList;
  EventCounts: TDictionary<string, Integer>;
  Line, EventName: string;
  JsonObj: TJSONObject;
begin
  LogFile := TPath.Combine(TPath.GetDocumentsPath, 'VotreApp\telemetry.log');

  if not FileExists(LogFile) then
  begin
    ShowMessage('Aucun fichier de télémétrie trouvé.');
    Exit;
  end;

  Events := TStringList.Create;
  EventCounts := TDictionary<string, Integer>.Create;
  try
    Events.LoadFromFile(LogFile);

    // Compter les types d'événements
    for Line in Events do
    begin
      try
        JsonObj := TJSONObject.ParseJSONValue(Line) as TJSONObject;
        if Assigned(JsonObj) then
        begin
          try
            EventName := JsonObj.GetValue<string>('event');

            if not EventCounts.ContainsKey(EventName) then
              EventCounts.Add(EventName, 0);

            EventCounts[EventName] := EventCounts[EventName]+ 1;
          finally
            JsonObj.Free;
          end;
        end;
      except
        // Ignorer les lignes mal formatées
        Continue;
      end;
    end;

    // Afficher les résultats
    memResults.Clear;
    memResults.Lines.Add('Analyse de télémétrie:');
    memResults.Lines.Add('----------------------');
    memResults.Lines.Add(Format('Nombre total d''événements: %d', [Events.Count]));
    memResults.Lines.Add('');
    memResults.Lines.Add('Distribution des événements:');

    for var Pair in EventCounts do
    begin
      memResults.Lines.Add(Format('- %s: %d (%d%%)', [
        Pair.Key,
        Pair.Value,
        Round((Pair.Value / Events.Count) * 100)
      ]));
    end;

    // Optionnel: Créer un graphique avec la distribution
    CreateEventDistributionChart(EventCounts);

  finally
    Events.Free;
    EventCounts.Free;
  end;
end;

// Procédure auxiliaire pour créer un graphique
procedure TAnalyzerForm.CreateEventDistributionChart(const EventCounts: TDictionary<string, Integer>);
begin
  // Exemple d'implémentation avec un composant TChart
  // (nécessite le composant TeeChart ou similaire)
  {
  Chart1.Series[0].Clear;

  for var Pair in EventCounts do
    Chart1.Series[0].Add(Pair.Value, Pair.Key);

  Chart1.Title.Text.Text := 'Distribution des événements';
  }
end;
```

## Exercice avancé : Gestionnaire de crash simplifié

Pour aller plus loin, créez un gestionnaire de crash simplifié qui sauvegarde des informations lorsqu'une exception non gérée se produit :

```pascal
unit uSimpleCrashHandler;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, System.IOUtils;

type
  TSimpleCrashHandler = class
  private
    FEnabled: Boolean;
    FCrashLogPath: string;
    FPreviousExceptionHandler: TExceptionEvent;

    function GetSystemInfo: string;
  public
    constructor Create;

    procedure Initialize;
    procedure HandleException(Sender: TObject; E: Exception);

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  SimpleCrashHandler: TSimpleCrashHandler;

implementation

uses
  Winapi.Windows;

{ TSimpleCrashHandler }

constructor TSimpleCrashHandler.Create;
begin
  inherited Create;
  FEnabled := True;
  FCrashLogPath := TPath.Combine(TPath.GetDocumentsPath, 'VotreApp\Crashes');

  if not TDirectory.Exists(FCrashLogPath) then
    TDirectory.CreateDirectory(FCrashLogPath);
end;

procedure TSimpleCrashHandler.Initialize;
begin
  // Sauvegarder le gestionnaire précédent et installer le nôtre
  FPreviousExceptionHandler := Application.OnException;
  Application.OnException := HandleException;
end;

function TSimpleCrashHandler.GetSystemInfo: string;
var
  Info: TStringList;
  OSVersionInfo: TOSVersionInfo;
begin
  Info := TStringList.Create;
  try
    // OS version
    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    if GetVersionEx(OSVersionInfo) then
    begin
      Info.Add(Format('Windows Version: %d.%d Build %d', [
        OSVersionInfo.dwMajorVersion,
        OSVersionInfo.dwMinorVersion,
        OSVersionInfo.dwBuildNumber
      ]));
    end;

    // Mémoire
    Info.Add(Format('Mémoire physique: %d MB', [GetPhysicalMemory div (1024*1024)]));

    // Information sur l'application
    Info.Add(Format('Delphi Version: %s', [DelphiVersion]));
    Info.Add(Format('Exécutable: %s', [ParamStr(0)]));
    Info.Add(Format('Version: %s', [GetAppVersionStr]));

    Result := Info.Text;
  finally
    Info.Free;
  end;
end;

procedure TSimpleCrashHandler.HandleException(Sender: TObject; E: Exception);
var
  CrashInfo: TStringList;
  CrashLogFile: string;
  FormName: string;
begin
  // Si désactivé, utiliser le gestionnaire précédent
  if not FEnabled then
  begin
    if Assigned(FPreviousExceptionHandler) then
      FPreviousExceptionHandler(Sender, E);
    Exit;
  end;

  // Déterminer le formulaire d'où vient l'exception
  if Sender is TComponent then
  begin
    if Sender is TForm then
      FormName := TForm(Sender).Name
    else if Assigned(TComponent(Sender).Owner) and (TComponent(Sender).Owner is TForm) then
      FormName := TForm(TComponent(Sender).Owner).Name
    else
      FormName := 'Unknown';
  end
  else
    FormName := 'Unknown';

  // Créer le rapport de crash
  CrashInfo := TStringList.Create;
  try
    CrashInfo.Add('===== RAPPORT DE CRASH =====');
    CrashInfo.Add(Format('Date/Heure: %s', [DateTimeToStr(Now)]));
    CrashInfo.Add(Format('Exception: %s', [E.ClassName]));
    CrashInfo.Add(Format('Message: %s', [E.Message]));
    CrashInfo.Add(Format('Formulaire: %s', [FormName]));
    CrashInfo.Add('');
    CrashInfo.Add('===== INFORMATIONS SYSTÈME =====');
    CrashInfo.Add(GetSystemInfo);

    // Sauvegarder dans un fichier
    CrashLogFile := TPath.Combine(FCrashLogPath,
      Format('crash_%s.log', [FormatDateTime('yyyymmdd_hhnnss', Now)]));
    CrashInfo.SaveToFile(CrashLogFile);

    // Afficher un message à l'utilisateur
    MessageDlg(
      Format('L''application a rencontré une erreur: %s'#13#10 +
             'Un rapport a été créé dans %s'#13#10 +
             'Veuillez contacter le support technique.',
             [E.Message, ExtractFileName(CrashLogFile)]),
      mtError, [mbOK], 0);

    // Si c'est une erreur grave, fermer l'application
    if E is EAccessViolation then
      Application.Terminate;

  finally
    CrashInfo.Free;
  end;
end;

initialization
  SimpleCrashHandler := TSimpleCrashHandler.Create;

finalization
  SimpleCrashHandler.Free;

end.
```

Ensuite, intégrez ce gestionnaire dans votre application :

```pascal
uses
  uSimpleCrashHandler;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser le gestionnaire de crash
  SimpleCrashHandler.Initialize;

  // Reste du code...
end;
```

## Conclusion : L'importance des données pour l'amélioration continue

La télémétrie et l'analyse de crash sont des outils essentiels dans le cycle de développement moderne des applications Delphi. Ils vous permettent de :

1. **Prendre des décisions basées sur des données** plutôt que sur des suppositions
2. **Améliorer proactivement** votre application avant que les problèmes ne deviennent critiques
3. **Optimiser les fonctionnalités** qui comptent réellement pour vos utilisateurs
4. **Réduire les coûts de support** en identifiant et résolvant les problèmes rapidement
5. **Offrir une meilleure expérience utilisateur** en corrigeant les bugs avant qu'ils ne soient signalés

En implémentant ces techniques dans votre application Delphi, vous créez un cycle vertueux d'amélioration continue qui bénéficie à la fois à vos utilisateurs et à votre équipe de développement.

### Points clés à retenir

- **Commencez simplement** : Vous n'avez pas besoin d'un système complexe pour obtenir des informations utiles
- **Respectez la vie privée** : Soyez transparent et obtenez le consentement de vos utilisateurs
- **Analysez régulièrement** : Les données n'ont de valeur que si vous les utilisez
- **Intégrez dans votre processus** : Faites de l'analyse des données une partie de votre cycle de développement
- **Équilibrez coûts et bénéfices** : Utilisez des solutions tierces quand cela a du sens

En appliquant ces principes, vous transformerez votre application Delphi d'un produit statique en un système qui évolue et s'améliore continuellement en fonction des besoins réels de vos utilisateurs.

## Ressources supplémentaires

### Documentation

- [Documentation officielle de Delphi sur les exceptions](https://docwiki.embarcadero.com/RADStudio/en/Exceptions)
- [Guide RGPD pour les développeurs](https://gdpr.eu/developers/)

### Outils

- [EurekaLog](https://www.eurekalog.com/)
- [MadExcept](http://madshi.net/madExceptDescription.htm)
- [DUnit/DUnitX](https://github.com/VSoftTechnologies/DUnitX) pour les tests qui peuvent aider à prévenir les crashes

### Articles et tutoriels

- "Exception Handling in Delphi" par Nick Hodges
- "Building Robust Applications with Delphi" par Marco Cantù
- "Modern Error Handling Patterns" sur le blog Embarcadero

---

Ce chapitre vous a présenté les concepts fondamentaux de la télémétrie et de l'analyse de crash dans les applications Delphi. En appliquant ces techniques, vous serez en mesure de créer des applications plus fiables et mieux adaptées aux besoins de vos utilisateurs. Dans les prochains chapitres, nous explorerons d'autres aspects avancés du développement avec Delphi qui vous permettront de créer des applications professionnelles de haute qualité.

⏭️ [Architecture et bonnes pratiques](18-architecture-et-bonnes-pratiques/README.md)
