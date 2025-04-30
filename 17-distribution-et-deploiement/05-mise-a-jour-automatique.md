# 17.5 Mise à jour automatique

## Introduction

Imaginez que vous venez de corriger un bug important ou d'ajouter une fonctionnalité très attendue à votre application Delphi. Comment faire en sorte que tous vos utilisateurs en profitent rapidement ? La réponse est simple : un système de **mise à jour automatique**.

Une mise à jour automatique permet à votre application de vérifier périodiquement si une nouvelle version est disponible, de la télécharger et de l'installer, le tout avec un minimum d'intervention de l'utilisateur. Cette fonctionnalité améliore considérablement l'expérience utilisateur et garantit que vos clients utilisent toujours la dernière version de votre logiciel.

Dans ce chapitre, nous allons découvrir comment implémenter un système de mise à jour automatique dans vos applications Delphi, des concepts de base jusqu'à une solution complète.

## Concepts fondamentaux

Un système de mise à jour automatique comporte généralement ces éléments essentiels :

1. **Vérification des mises à jour** : Déterminer si une nouvelle version est disponible
2. **Téléchargement** : Récupérer les fichiers de mise à jour
3. **Installation** : Appliquer la mise à jour au logiciel existant
4. **Interface utilisateur** : Informer l'utilisateur et gérer ses choix

![Schéma du processus de mise à jour](https://placeholder-image.com/auto-update-process.png)

## Approches pour les mises à jour automatiques

Il existe plusieurs façons d'implémenter des mises à jour automatiques. Nous allons examiner trois approches, de la plus simple à la plus avancée :

### 1. Approche simple : Remplacement complet

Cette méthode consiste à télécharger un nouvel installateur complet et à l'exécuter.

**Avantages :**
- Facile à implémenter
- Fiable car elle utilise votre installateur standard

**Inconvénients :**
- Téléchargement plus volumineux
- Nécessite de quitter l'application

### 2. Approche intermédiaire : Mise à jour par fichiers différentiels

Cette approche ne télécharge que les fichiers qui ont changé.

**Avantages :**
- Téléchargements plus petits et plus rapides
- Processus de mise à jour plus rapide

**Inconvénients :**
- Plus complexe à implémenter
- Nécessite un mécanisme pour comparer les versions des fichiers

### 3. Approche avancée : Mise à jour à chaud (Hot Update)

Cette méthode permet de mettre à jour l'application pendant qu'elle s'exécute.

**Avantages :**
- Expérience utilisateur optimale
- Pas besoin de redémarrer l'application (pour certaines mises à jour)

**Inconvénients :**
- Complexité technique élevée
- Ne fonctionne pas pour tous les types de modifications

## Implémentation d'une solution simple

Commençons par créer un système de mise à jour automatique simple mais fonctionnel. Cette implémentation vérifiera si une nouvelle version est disponible, téléchargera le nouvel installateur et l'exécutera.

### Étape 1 : Structure du serveur de mise à jour

Vous aurez besoin d'un serveur web pour héberger :

1. Un fichier XML ou JSON contenant les informations de version
2. Les fichiers d'installation

Voici un exemple de structure de fichier `version.xml` :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<update>
  <version>2.1.5</version>
  <url>https://www.monsite.com/downloads/MonApp_Setup_2.1.5.exe</url>
  <notes>
    - Correction du bug d'affichage des rapports
    - Amélioration des performances de recherche
    - Nouvelle fonctionnalité d'exportation PDF
  </notes>
  <minVersion>2.0.0</minVersion>
  <forceUpdate>false</forceUpdate>
</update>
```

Placez ce fichier sur votre serveur web à une URL fixe, par exemple :
`https://www.monsite.com/updates/version.xml`

### Étape 2 : Création de la classe de mise à jour dans Delphi

Créons une classe pour gérer le processus de mise à jour :

```pascal
unit uUpdater;

interface

uses
  System.Classes, System.SysUtils, System.Net.HttpClient,
  System.Net.URLClient, Xml.XMLDoc, Xml.XMLIntf, System.IOUtils;

type
  TUpdateInfo = record
    CurrentVersion: string;
    NewVersion: string;
    DownloadURL: string;
    ReleaseNotes: string;
    MinVersion: string;
    ForceUpdate: Boolean;
    UpdateAvailable: Boolean;
  end;

  TUpdateCallback = procedure(const UpdateInfo: TUpdateInfo) of object;

  TAutoUpdater = class
  private
    FUpdateURL: string;
    FCurrentVersion: string;
    FTempFilePath: string;
    FOnUpdateAvailable: TUpdateCallback;
    FOnNoUpdateAvailable: TNotifyEvent;
    FOnDownloadProgress: TProc<Int64, Int64>;
    FOnDownloadComplete: TProc<string>;
    FOnError: TProc<string>;

    function CompareVersions(Version1, Version2: string): Integer;
    function DownloadFile(const URL, DestinationFile: string): Boolean;
  public
    constructor Create(const UpdateURL, CurrentVersion: string);
    destructor Destroy; override;

    function CheckForUpdates: TUpdateInfo;
    procedure DownloadUpdate(const UpdateInfo: TUpdateInfo);
    procedure InstallUpdate;

    property OnUpdateAvailable: TUpdateCallback read FOnUpdateAvailable write FOnUpdateAvailable;
    property OnNoUpdateAvailable: TNotifyEvent read FOnNoUpdateAvailable write FOnNoUpdateAvailable;
    property OnDownloadProgress: TProc<Int64, Int64> read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadComplete: TProc<string> read FOnDownloadComplete write FOnDownloadComplete;
    property OnError: TProc<string> read FOnError write FOnError;
  end;

implementation

uses
  Winapi.Windows, Winapi.ShellAPI;

{ TAutoUpdater }

constructor TAutoUpdater.Create(const UpdateURL, CurrentVersion: string);
begin
  inherited Create;
  FUpdateURL := UpdateURL;
  FCurrentVersion := CurrentVersion;
  FTempFilePath := TPath.Combine(TPath.GetTempPath, 'update_download.exe');
end;

destructor TAutoUpdater.Destroy;
begin
  if FileExists(FTempFilePath) then
    TFile.Delete(FTempFilePath);
  inherited;
end;

function TAutoUpdater.CompareVersions(Version1, Version2: string): Integer;
var
  V1, V2: TArray<string>;
  I, Num1, Num2: Integer;
begin
  V1 := Version1.Split(['.']);
  V2 := Version2.Split(['.']);

  Result := 0;
  for I := 0 to Min(Length(V1), Length(V2)) - 1 do
  begin
    Num1 := StrToIntDef(V1[I], 0);
    Num2 := StrToIntDef(V2[I], 0);

    if Num1 < Num2 then
      Exit(-1)
    else if Num1 > Num2 then
      Exit(1);
  end;

  if Length(V1) < Length(V2) then
    Result := -1
  else if Length(V1) > Length(V2) then
    Result := 1;
end;

function TAutoUpdater.CheckForUpdates: TUpdateInfo;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
begin
  // Initialiser le résultat
  Result.CurrentVersion := FCurrentVersion;
  Result.UpdateAvailable := False;

  try
    HTTP := THTTPClient.Create;
    try
      Response := HTTP.Get(FUpdateURL);

      if Response.StatusCode = 200 then
      begin
        XMLDoc := TXMLDocument.Create(nil);
        XMLDoc.LoadFromXML(Response.ContentAsString);
        XMLDoc.Active := True;

        RootNode := XMLDoc.DocumentElement;

        Result.NewVersion := RootNode.ChildValues['version'];
        Result.DownloadURL := RootNode.ChildValues['url'];
        Result.ReleaseNotes := RootNode.ChildValues['notes'];
        Result.MinVersion := RootNode.ChildValues['minVersion'];
        Result.ForceUpdate := LowerCase(RootNode.ChildValues['forceUpdate']) = 'true';

        // Comparer les versions
        Result.UpdateAvailable := CompareVersions(FCurrentVersion, Result.NewVersion) < 0;

        // Vérifier si la version actuelle est inférieure à la version minimale requise
        if (Result.MinVersion <> '') and (CompareVersions(FCurrentVersion, Result.MinVersion) < 0) then
          Result.ForceUpdate := True;

        if Result.UpdateAvailable and Assigned(FOnUpdateAvailable) then
          FOnUpdateAvailable(Result)
        else if not Result.UpdateAvailable and Assigned(FOnNoUpdateAvailable) then
          FOnNoUpdateAvailable(Self);
      end
      else if Assigned(FOnError) then
        FOnError('Erreur HTTP: ' + Response.StatusCode.ToString);
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError('Erreur: ' + E.Message);
  end;
end;

function TAutoUpdater.DownloadFile(const URL, DestinationFile: string): Boolean;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
begin
  Result := False;

  try
    HTTP := THTTPClient.Create;
    try
      // Configurer le callback de progression si nécessaire
      if Assigned(FOnDownloadProgress) then
      begin
        HTTP.OnReceiveData := procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
        begin
          FOnDownloadProgress(AContentLength, AReadCount);
        end;
      end;

      if FileExists(DestinationFile) then
        TFile.Delete(DestinationFile);

      FileStream := TFileStream.Create(DestinationFile, fmCreate);
      try
        Response := HTTP.Get(URL, FileStream);
        Result := Response.StatusCode = 200;
      finally
        FileStream.Free;
      end;

      if Result and Assigned(FOnDownloadComplete) then
        FOnDownloadComplete(DestinationFile)
      else if not Result and Assigned(FOnError) then
        FOnError('Erreur de téléchargement: ' + Response.StatusCode.ToString);
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Erreur: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TAutoUpdater.DownloadUpdate(const UpdateInfo: TUpdateInfo);
begin
  if DownloadFile(UpdateInfo.DownloadURL, FTempFilePath) then
  begin
    if Assigned(FOnDownloadComplete) then
      FOnDownloadComplete(FTempFilePath);
  end;
end;

procedure TAutoUpdater.InstallUpdate;
var
  ExecuteInfo: TShellExecuteInfo;
begin
  if not FileExists(FTempFilePath) then
  begin
    if Assigned(FOnError) then
      FOnError('Fichier d''installation introuvable');
    Exit;
  end;

  // Préparer l'exécution de l'installateur
  FillChar(ExecuteInfo, SizeOf(ExecuteInfo), 0);
  ExecuteInfo.cbSize := SizeOf(ExecuteInfo);
  ExecuteInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ExecuteInfo.lpFile := PChar(FTempFilePath);
  ExecuteInfo.nShow := SW_SHOW;

  // Exécuter l'installateur
  if ShellExecuteEx(@ExecuteInfo) then
  begin
    // Fermer l'application actuelle pour permettre la mise à jour
    ExitProcess(0);
  end
  else if Assigned(FOnError) then
    FOnError('Erreur lors du lancement de l''installateur');
end;

end.
```

### Étape 3 : Intégration dans le formulaire principal

Maintenant, intégrons notre système de mise à jour dans le formulaire principal de l'application :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, uUpdater;

type
  TfrmMain = class(TForm)
    btnCheckUpdates: TButton;
    mmoLog: TMemo;
    pbDownload: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckUpdatesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FUpdater: TAutoUpdater;
    FUpdateInfo: TUpdateInfo;

    procedure OnUpdateAvailable(const UpdateInfo: TUpdateInfo);
    procedure OnNoUpdateAvailable(Sender: TObject);
    procedure OnDownloadProgress(TotalSize, DownloadedSize: Int64);
    procedure OnDownloadComplete(const FilePath: string);
    procedure OnError(const ErrorMessage: string);
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  APP_VERSION = '2.0.5';
  UPDATE_URL = 'https://www.monsite.com/updates/version.xml';

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'Mon Application - v' + APP_VERSION;

  // Initialiser l'updater
  FUpdater := TAutoUpdater.Create(UPDATE_URL, APP_VERSION);
  FUpdater.OnUpdateAvailable := OnUpdateAvailable;
  FUpdater.OnNoUpdateAvailable := OnNoUpdateAvailable;
  FUpdater.OnDownloadProgress := OnDownloadProgress;
  FUpdater.OnDownloadComplete := OnDownloadComplete;
  FUpdater.OnError := OnError;

  pbDownload.Visible := False;

  // Vérification automatique au démarrage (optionnel)
  // FUpdater.CheckForUpdates;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FUpdater.Free;
end;

procedure TfrmMain.btnCheckUpdatesClick(Sender: TObject);
begin
  mmoLog.Lines.Add('Vérification des mises à jour...');
  FUpdateInfo := FUpdater.CheckForUpdates;
end;

procedure TfrmMain.OnUpdateAvailable(const UpdateInfo: TUpdateInfo);
var
  Response: Integer;
begin
  mmoLog.Lines.Add('Nouvelle version disponible: ' + UpdateInfo.NewVersion);
  mmoLog.Lines.Add('Notes de version:');
  mmoLog.Lines.Add(UpdateInfo.ReleaseNotes);

  // Stocker les informations de mise à jour
  FUpdateInfo := UpdateInfo;

  if UpdateInfo.ForceUpdate then
    Response := mrYes
  else
    Response := MessageDlg('Une nouvelle version (' + UpdateInfo.NewVersion +
                          ') est disponible. Voulez-vous la télécharger et l''installer maintenant?',
                          mtConfirmation, [mbYes, mbNo], 0);

  if Response = mrYes then
  begin
    mmoLog.Lines.Add('Téléchargement de la mise à jour...');
    pbDownload.Visible := True;
    pbDownload.Position := 0;

    // Commencer le téléchargement
    FUpdater.DownloadUpdate(UpdateInfo);
  end;
end;

procedure TfrmMain.OnNoUpdateAvailable(Sender: TObject);
begin
  mmoLog.Lines.Add('Votre application est à jour!');

  // Pas nécessaire de faire apparaître une boîte de dialogue si appelé au démarrage
  if not (TComponent(Sender) is TAutoUpdater) or
     (TComponent(Sender) = btnCheckUpdates) then
    MessageDlg('Votre application est déjà à jour!', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.OnDownloadProgress(TotalSize, DownloadedSize: Int64);
begin
  if TotalSize > 0 then
  begin
    pbDownload.Max := 100;
    pbDownload.Position := Round((DownloadedSize / TotalSize) * 100);
    Application.ProcessMessages; // Permet à l'interface de se mettre à jour
  end;
end;

procedure TfrmMain.OnDownloadComplete(const FilePath: string);
var
  Response: Integer;
begin
  mmoLog.Lines.Add('Téléchargement terminé!');
  pbDownload.Visible := False;

  if FUpdateInfo.ForceUpdate then
    Response := mrYes
  else
    Response := MessageDlg('La mise à jour a été téléchargée. Voulez-vous l''installer maintenant? ' +
                          'L''application va se fermer pendant l''installation.',
                          mtConfirmation, [mbYes, mbNo], 0);

  if Response = mrYes then
  begin
    mmoLog.Lines.Add('Installation de la mise à jour...');
    FUpdater.InstallUpdate;
    // L'application se fermera ici si l'installation démarre
  end;
end;

procedure TfrmMain.OnError(const ErrorMessage: string);
begin
  mmoLog.Lines.Add('Erreur: ' + ErrorMessage);
end;

end.
```

### Étape 4 : Mise en place de la vérification périodique (optionnel)

Pour vérifier automatiquement les mises à jour à intervalle régulier (par exemple, une fois par jour), vous pouvez ajouter un minuteur :

```pascal
// Ajouter dans la section private de TfrmMain
tmrCheckUpdate: TTimer;

// Dans FormCreate, après l'initialisation de l'updater
tmrCheckUpdate := TTimer.Create(Self);
tmrCheckUpdate.Interval := 24 * 60 * 60 * 1000; // 24 heures en millisecondes
tmrCheckUpdate.OnTimer := CheckUpdateTimer;
tmrCheckUpdate.Enabled := True;

// Ajouter cette méthode
procedure TfrmMain.CheckUpdateTimer(Sender: TObject);
begin
  FUpdater.CheckForUpdates;
end;
```

## Solutions plus avancées

### Mise à jour différentielle

Pour implémenter des mises à jour différentielles qui ne téléchargent que les fichiers modifiés, vous aurez besoin de :

1. Un fichier de manifeste sur le serveur qui liste tous les fichiers avec leurs versions/hash
2. Un mécanisme côté client pour comparer les fichiers locaux avec ceux du manifeste
3. Une logique pour télécharger et remplacer seulement les fichiers nécessaires

Voici un exemple simplifié de fichier manifeste (`manifest.xml`) :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<manifest version="2.1.5">
  <file>
    <path>app.exe</path>
    <version>2.1.5</version>
    <hash>a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6</hash>
    <url>https://www.monsite.com/downloads/app_2_1_5.exe</url>
  </file>
  <file>
    <path>data/config.dat</path>
    <version>1.0.3</version>
    <hash>q7r8s9t0u1v2w3x4y5z6a7b8c9d0e1f2</hash>
    <url>https://www.monsite.com/downloads/config_1_0_3.dat</url>
  </file>
  <!-- Autres fichiers... -->
</manifest>
```

### Bibliothèques tierces

Plusieurs bibliothèques Delphi peuvent vous aider à implémenter des mises à jour automatiques plus avancées :

1. **TMS Updater** : Une solution commerciale complète pour les mises à jour automatiques
   - [https://www.tmssoftware.com/site/tmspack.asp](https://www.tmssoftware.com/site/tmspack.asp)

2. **DevExpress** : Offre des composants pour la mise à jour automatique
   - [https://www.devexpress.com/](https://www.devexpress.com/)

3. **UniGUI Auto-Updater** : Pour les applications web UniGUI
   - [https://www.unigui.com/](https://www.unigui.com/)

4. **Sparkle pour Delphi** : Un portage open-source du système de mise à jour Sparkle de macOS
   - [https://github.com/delawarefw/sparkle-delphi](https://github.com/delawarefw/sparkle-delphi)

## Considérations importantes pour la mise à jour automatique

### Sécurité

1. **Utilisation du HTTPS** : Toujours utiliser HTTPS pour les téléchargements afin d'éviter les attaques "man-in-the-middle"

2. **Validation de la signature** : Vérifier la signature numérique des fichiers téléchargés

```pascal
// Exemple simplifié de vérification de signature
function VerifyFileSignature(const FilePath: string): Boolean;
var
  WinTrust: TWinTrust;
begin
  // Utilisation de l'API WinVerifyTrust pour vérifier la signature
  // Code à implémenter selon la documentation Microsoft
  // ...
end;
```

3. **Hachage des fichiers** : Vérifier l'intégrité par des hachages SHA-256

```pascal
function CalculateFileHash(const FilePath: string): string;
var
  IdHash: TIdHashSHA256;
  FileStream: TFileStream;
begin
  IdHash := TIdHashSHA256.Create;
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Result := IdHash.HashStreamAsHex(FileStream);
  finally
    FileStream.Free;
    IdHash.Free;
  end;
end;
```

### Gestion des erreurs

1. **Reprise de téléchargement** : Permettre la reprise des téléchargements interrompus

2. **Restauration** : Prévoir un mécanisme de restauration si la mise à jour échoue

```pascal
procedure BackupApplication(const BackupPath: string);
begin
  // Créer une sauvegarde avant la mise à jour
  // ...
end;

procedure RestoreFromBackup(const BackupPath: string);
begin
  // Restaurer l'application depuis la sauvegarde en cas d'échec
  // ...
end;
```

3. **Journalisation** : Enregistrer les actions et erreurs pour le diagnostic

### Expérience utilisateur

1. **Options de configuration** : Permettre à l'utilisateur de contrôler les mises à jour

```pascal
procedure ConfigureUpdateSettings;
var
  Form: TfrmUpdateSettings;
begin
  Form := TfrmUpdateSettings.Create(nil);
  try
    // Configurer les options (fréquence, automatique/manuel, etc.)
    if Form.ShowModal = mrOK then
    begin
      // Sauvegarder les préférences
    end;
  finally
    Form.Free;
  end;
end;
```

2. **Planification** : Proposer des heures de mise à jour qui minimisent les interruptions

3. **Notifications non intrusives** : Utiliser des notifications discrètes pour informer des mises à jour disponibles

```pascal
procedure ShowUpdateNotification(const Version: string);
var
  TrayIcon: TTrayIcon;
begin
  TrayIcon := TTrayIcon.Create(Self);
  TrayIcon.Visible := True;
  TrayIcon.BalloonTitle := 'Mise à jour disponible';
  TrayIcon.BalloonHint := 'La version ' + Version + ' est disponible. Cliquez pour mettre à jour.';
  TrayIcon.ShowBalloonHint;
end;
```

## Mise à jour via Microsoft Store ou plates-formes similaires

Si votre application est distribuée via le Microsoft Store ou d'autres plateformes similaires, le processus de mise à jour est généralement géré par la plateforme elle-même. Dans ce cas, vous n'avez pas besoin d'implémenter votre propre système.

## Exemple complet : Formulaire de paramètres de mise à jour

Voici un exemple de formulaire pour configurer les préférences de mise à jour :

```pascal
unit UpdateSettings;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.IniFiles;

type
  TUpdateFrequency = (ufDaily, ufWeekly, ufMonthly, ufNever);

  TfrmUpdateSettings = class(TForm)
    rbDaily: TRadioButton;
    rbWeekly: TRadioButton;
    rbMonthly: TRadioButton;
    rbNever: TRadioButton;
    chkAutoInstall: TCheckBox;
    chkStartup: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    lblFrequency: TLabel;
    lblOptions: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    function GetUpdateFrequency: TUpdateFrequency;
    procedure SetUpdateFrequency(Value: TUpdateFrequency);
    function GetAutoInstall: Boolean;
    procedure SetAutoInstall(Value: Boolean);
    function GetCheckOnStartup: Boolean;
    procedure SetCheckOnStartup(Value: Boolean);
    procedure LoadSettings;
    procedure SaveSettings;
  public
    property UpdateFrequency: TUpdateFrequency read GetUpdateFrequency write SetUpdateFrequency;
    property AutoInstall: Boolean read GetAutoInstall write SetAutoInstall;
    property CheckOnStartup: Boolean read GetCheckOnStartup write SetCheckOnStartup;
  end;

implementation

{$R *.dfm}

const
  INI_FILE = 'settings.ini';
  INI_SECTION = 'Updates';

procedure TfrmUpdateSettings.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

procedure TfrmUpdateSettings.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

function TfrmUpdateSettings.GetUpdateFrequency: TUpdateFrequency;
begin
  if rbDaily.Checked then
    Result := ufDaily
  else if rbWeekly.Checked then
    Result := ufWeekly
  else if rbMonthly.Checked then
    Result := ufMonthly
  else
    Result := ufNever;
end;

procedure TfrmUpdateSettings.SetUpdateFrequency(Value: TUpdateFrequency);
begin
  case Value of
    ufDaily: rbDaily.Checked := True;
    ufWeekly: rbWeekly.Checked := True;
    ufMonthly: rbMonthly.Checked := True;
    ufNever: rbNever.Checked := True;
  end;
end;

function TfrmUpdateSettings.GetAutoInstall: Boolean;
begin
  Result := chkAutoInstall.Checked;
end;

procedure TfrmUpdateSettings.SetAutoInstall(Value: Boolean);
begin
  chkAutoInstall.Checked := Value;
end;

function TfrmUpdateSettings.GetCheckOnStartup: Boolean;
begin
  Result := chkStartup.Checked;
end;

procedure TfrmUpdateSettings.SetCheckOnStartup(Value: Boolean);
begin
  chkStartup.Checked := Value;
end;

procedure TfrmUpdateSettings.LoadSettings;
var
  IniFile: TIniFile;
  Frequency: Integer;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + INI_FILE);
  try
    Frequency := IniFile.ReadInteger(INI_SECTION, 'Frequency', Ord(ufWeekly));
    UpdateFrequency := TUpdateFrequency(Frequency);
    AutoInstall := IniFile.ReadBool(INI_SECTION, 'AutoInstall', False);
    CheckOnStartup := IniFile.ReadBool(INI_SECTION, 'CheckOnStartup', True);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmUpdateSettings.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + INI_FILE);
  try
    IniFile.WriteInteger(INI_SECTION, 'Frequency', Ord(UpdateFrequency));
    IniFile.WriteBool(INI_SECTION, 'AutoInstall', AutoInstall);
    IniFile.WriteBool(INI_SECTION, 'CheckOnStartup', CheckOnStartup);
  finally
    IniFile.Free;
  end;
end;

end.
```

## Exercice pratique : Mise à jour en action

Suivez ces étapes pour implémenter un système de mise à jour automatique dans votre application Delphi existante :

1. Créez un fichier `version.xml` et hébergez-le sur un serveur web accessible
2. Implémentez la classe `TAutoUpdater` décrite dans ce chapitre
3. Intégrez le code de vérification des mises à jour dans votre formulaire principal
4. Testez le système en modifiant le fichier de version sur le serveur
5. Créez une nouvelle version de votre application et testez le processus complet de mise à jour

## Mise à jour pour des applications en cours d'exécution

Pour les applications qui doivent rester en service pendant la mise à jour, voici une approche utilisant un processus externe d'installation :

```pascal
procedure LaunchUpdaterProcess;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CurrentDir, CmdLine: string;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);

  CurrentDir := ExtractFilePath(ParamStr(0));

  // Crée une ligne de commande pour l'installateur avec des paramètres spéciaux
  CmdLine := Format('"%s" /silent /closeapp="%s" /pid=%d',
             [FTempFilePath, ExtractFileName(ParamStr(0)), GetCurrentProcessId]);

  // Démarre le processus de mise à jour
  if CreateProcess(nil, PChar(CmdLine), nil, nil, False,
                  CREATE_DEFAULT_ERROR_MODE, nil,
                  PChar(CurrentDir), StartupInfo, ProcessInfo) then
  begin
    // Ferme les handles mais laisse le processus s'exécuter
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);

    // Ferme l'application actuelle après un court délai
    // pour permettre au processus de mise à jour de démarrer
    Sleep(1000);
    Application.Terminate;
  end
  else if Assigned(FOnError) then
    FOnError('Erreur lors du lancement du processus de mise à jour: ' +
             IntToStr(GetLastError));
end;
```

## Organisation d'un système de mise à jour pour plusieurs applications

Si vous développez plusieurs applications, vous pouvez centraliser votre système de mise à jour :

### 1. Structure du serveur

```
/updates/
  /app1/
    version.xml
    files/
  /app2/
    version.xml
    files/
```

### 2. Classe générique de mise à jour

```pascal
TGenericUpdater = class(TAutoUpdater)
public
  class function CreateForApp(const AppID: string; const CurrentVersion: string): TGenericUpdater;
end;

// Implémentation
class function TGenericUpdater.CreateForApp(const AppID: string; const CurrentVersion: string): TGenericUpdater;
begin
  Result := TGenericUpdater.Create(Format('https://www.monsite.com/updates/%s/version.xml', [AppID]), CurrentVersion);
end;
```

## Bonnes pratiques de versionnement

Pour que votre système de mise à jour fonctionne efficacement, adoptez une stratégie de versionnement cohérente :

### Versionnement sémantique (recommandé)

Format : `MAJEUR.MINEUR.CORRECTIF`

- **MAJEUR** : changements incompatibles avec les versions précédentes
- **MINEUR** : ajouts de fonctionnalités rétrocompatibles
- **CORRECTIF** : corrections de bugs rétrocompatibles

Exemple : `2.5.3`

### Où stocker la version de votre application

1. **Ressources de l'exécutable** : Dans les informations de version de l'exécutable

```pascal
{$R *.res}  // Inclut un fichier de ressources avec les infos de version
```

2. **Constante globale** : Dans une unité dédiée

```pascal
unit uVersion;

interface

const
  APP_VERSION = '2.1.5';
  APP_VERSION_MAJOR = 2;
  APP_VERSION_MINOR = 1;
  APP_VERSION_PATCH = 5;

implementation

end.
```

## Création d'un service Windows de mise à jour

Pour les applications critiques nécessitant des mises à jour même sans utilisateur connecté, vous pouvez créer un service Windows :

```pascal
// Extrait de code pour un service de mise à jour
procedure TUpdateService.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    // Vérifier les mises à jour périodiquement
    CheckForUpdates;

    // Attendre l'intervalle configuré
    ServiceThread.WaitFor(UpdateInterval);
  end;
end;

procedure TUpdateService.CheckForUpdates;
var
  Updater: TAutoUpdater;
  UpdateInfo: TUpdateInfo;
begin
  Updater := TAutoUpdater.Create(UPDATE_URL, GetInstalledVersion);
  try
    UpdateInfo := Updater.CheckForUpdates;

    if UpdateInfo.UpdateAvailable then
    begin
      // Journaliser l'information
      LogMessage('Mise à jour disponible: ' + UpdateInfo.NewVersion);

      // Télécharger la mise à jour
      if Updater.DownloadUpdate(UpdateInfo) then
      begin
        // Planifier l'installation à une heure appropriée
        ScheduleInstallation(UpdateInfo);
      end;
    end;
  finally
    Updater.Free;
  end;
end;
```

## Interface utilisateur moderne pour les mises à jour

Pour améliorer l'expérience utilisateur, envisagez une interface plus attrayante pour les notifications de mise à jour :

### Exemple avec une notification animée

```pascal
procedure ShowModernUpdateNotification(const UpdateInfo: TUpdateInfo);
var
  NotifyForm: TfrmUpdateNotify;
begin
  NotifyForm := TfrmUpdateNotify.Create(nil);
  try
    NotifyForm.lblVersion.Caption := 'Version ' + UpdateInfo.NewVersion + ' disponible';
    NotifyForm.mmoNotes.Text := UpdateInfo.ReleaseNotes;

    // Positionner en bas à droite de l'écran
    NotifyForm.Left := Screen.Width - NotifyForm.Width - 20;
    NotifyForm.Top := Screen.Height;

    // Afficher avec animation de montée
    NotifyForm.Show;
    NotifyForm.AnimateIn;

    // La forme se fermera d'elle-même après un délai
    // ou l'utilisateur peut cliquer pour la fermer/mettre à jour
  except
    NotifyForm.Free;
    raise;
  end;
end;
```

## Statistiques et télémetrie (optionnel)

Pour suivre l'adoption de vos mises à jour, vous pouvez ajouter un système de télémetrie basique :

```pascal
procedure ReportUpdateStatus(const AppID, FromVersion, ToVersion: string; Success: Boolean);
var
  HTTP: THTTPClient;
  Params: TStringList;
begin
  HTTP := THTTPClient.Create;
  Params := TStringList.Create;
  try
    // Préparer les données
    Params.Add('app_id=' + AppID);
    Params.Add('from_version=' + FromVersion);
    Params.Add('to_version=' + ToVersion);
    Params.Add('success=' + BoolToStr(Success, True));
    Params.Add('system=' + GetSystemInfo);

    // Envoyer de façon asynchrone pour ne pas bloquer
    TThread.CreateAnonymousThread(procedure
    begin
      try
        HTTP.Post('https://www.monsite.com/update_stats.php', Params);
      except
        // Ignorer les erreurs - la télémetrie ne doit jamais interrompre l'application
      end;
    end).Start;
  finally
    Params.Free;
    HTTP.Free;
  end;
end;
```

## Conclusion

L'implémentation d'un système de mise à jour automatique est un investissement qui améliore considérablement l'expérience de vos utilisateurs et vous permet de déployer rapidement des corrections et des améliorations.

Les approches présentées dans ce chapitre vont d'une solution simple mais fonctionnelle à des implémentations plus avancées. Choisissez celle qui correspond le mieux à vos besoins et aux attentes de vos utilisateurs.

N'oubliez pas que la transparence est essentielle : informez toujours vos utilisateurs des mises à jour disponibles et donnez-leur le contrôle sur quand et comment ces mises à jour sont installées, sauf si des problèmes critiques de sécurité nécessitent une mise à jour forcée.

Dans la prochaine section, nous aborderons le déploiement de vos applications Delphi sur différentes plateformes, ce qui étendra encore davantage la portée de vos logiciels.
