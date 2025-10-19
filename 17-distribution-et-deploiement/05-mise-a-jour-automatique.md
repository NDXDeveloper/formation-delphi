🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.5 Mise à jour automatique

## Introduction

Imaginez que vous découvrez un bug critique dans votre application ou que vous souhaitez ajouter une nouvelle fonctionnalité. Comment faire pour que vos utilisateurs bénéficient rapidement de ces améliorations ? Devez-vous les contacter un par un pour leur dire de télécharger la nouvelle version ? Bien sûr que non !

La **mise à jour automatique** (ou *auto-update*) est un mécanisme qui permet à votre application de se mettre à jour elle-même, automatiquement ou avec un minimum d'intervention de l'utilisateur. C'est devenu un standard dans l'industrie du logiciel, et vos utilisateurs s'attendent à ce que votre application puisse se maintenir à jour facilement.

## Pourquoi implémenter un système de mise à jour ?

### 1. Correction rapide des bugs

Quand vous découvrez un bug, vous pouvez le corriger et distribuer la mise à jour immédiatement. Sans système de mise à jour, beaucoup d'utilisateurs continueront d'utiliser la version buggée pendant des mois.

### 2. Nouvelles fonctionnalités

Vous pouvez enrichir votre application au fil du temps et vos utilisateurs en bénéficient automatiquement.

### 3. Sécurité

Les failles de sécurité doivent être corrigées rapidement. Un système de mise à jour permet de déployer des patches de sécurité en quelques heures.

### 4. Expérience utilisateur

Les utilisateurs apprécient les applications qui se maintiennent à jour sans effort de leur part. C'est un signe de qualité et de maintenance active.

### 5. Support simplifié

Si tous vos utilisateurs ont des versions récentes, votre support est simplifié. Vous n'avez pas à gérer des dizaines de versions différentes.

### 6. Contrôle des versions en circulation

Vous gardez le contrôle des versions utilisées et pouvez même forcer la mise à jour si une version devient trop obsolète ou dangereuse.

## Types de stratégies de mise à jour

Il existe plusieurs approches pour gérer les mises à jour :

### 1. Notification simple

**Fonctionnement** :
- L'application vérifie si une nouvelle version existe
- Si oui, elle affiche un message à l'utilisateur
- L'utilisateur doit télécharger et installer manuellement

**Avantages** :
- Simple à implémenter
- Contrôle total pour l'utilisateur

**Inconvénients** :
- Beaucoup d'utilisateurs ignorent les notifications
- Nécessite une action manuelle

### 2. Téléchargement automatique avec installation manuelle

**Fonctionnement** :
- L'application détecte une nouvelle version
- Elle télécharge automatiquement la mise à jour
- Elle demande à l'utilisateur de fermer l'application pour installer

**Avantages** :
- Gain de temps pour l'utilisateur
- Mise à jour déjà prête à installer

**Inconvénients** :
- Consomme de la bande passante sans prévenir
- Installation toujours manuelle

### 3. Mise à jour automatique complète

**Fonctionnement** :
- Détection, téléchargement et installation automatiques
- L'utilisateur est simplement informé
- Peut se faire au démarrage ou en arrière-plan

**Avantages** :
- Expérience optimale
- Taux de mise à jour très élevé

**Inconvénients** :
- Complexe à implémenter
- Nécessite des droits administrateur (parfois)

### 4. Mise à jour forcée

**Fonctionnement** :
- L'application refuse de démarrer si elle n'est pas à jour
- Force le téléchargement et l'installation

**Avantages** :
- 100% des utilisateurs sont à jour
- Idéal pour les applications cloud qui nécessitent une version précise

**Inconvénients** :
- Peut frustrer les utilisateurs
- Problématique si le serveur de mise à jour est inaccessible

**Recommandation pour débutants** : Commencez par la stratégie 2 (téléchargement automatique avec installation manuelle), puis évoluez vers la 3.

## Principes de base d'un système de mise à jour

### Architecture générale

Un système de mise à jour comprend plusieurs éléments :

```
[Application Client]
       ↓
   Vérification
       ↓
[Serveur de mises à jour] (fichier XML/JSON avec infos version)
       ↓
   Comparaison
       ↓
   Nouvelle version ?
       ↓ Oui
   Téléchargement
       ↓
[Fichier de mise à jour] (installateur ou fichiers)
       ↓
   Installation
       ↓
   Redémarrage
```

### Composants nécessaires

1. **Module de vérification** : Code dans votre application qui vérifie les mises à jour
2. **Fichier de version** : Fichier sur un serveur (XML, JSON) contenant les informations de la dernière version
3. **Fichiers de mise à jour** : L'installateur ou les fichiers à mettre à jour
4. **Module d'installation** : Code qui installe la mise à jour

### Informations à gérer

**Côté serveur** (fichier version.json) :
- Numéro de la dernière version
- URL de téléchargement
- Taille du fichier
- Notes de version (changelog)
- Signature/Hash pour vérifier l'intégrité
- Version minimum requise

**Côté application** :
- Version actuelle de l'application
- Date de dernière vérification
- Paramètres de mise à jour (fréquence, automatique ou non)

## Implémentation d'un système de mise à jour simple

### Étape 1 : Créer le fichier de version sur le serveur

Créez un fichier `version.json` que vous hébergerez sur votre serveur web :

```json
{
  "version": "1.2.0",
  "release_date": "2025-01-20",
  "download_url": "https://monsite.com/downloads/MonApp_Setup_1.2.0.exe",
  "file_size": 15728640,
  "file_hash": "SHA256:a1b2c3d4e5f6...",
  "min_version": "1.0.0",
  "required": false,
  "changelog": [
    "Correction du bug de synchronisation",
    "Nouvelle fonctionnalité d'export PDF",
    "Améliorations de performance"
  ]
}
```

Placez ce fichier à une URL fixe, par exemple :
`https://monsite.com/updates/version.json`

### Étape 2 : Ajouter la gestion des versions dans votre application

Dans Delphi, définissez les informations de version :

```pascal
unit AppVersion;

interface

uses
  System.SysUtils, System.Classes;

type
  TVersionInfo = record
    Major: Integer;
    Minor: Integer;
    Release: Integer;
    Build: Integer;
    function ToString: string;
    function CompareWith(Other: TVersionInfo): Integer;
    class function FromString(const VersionStr: string): TVersionInfo; static;
  end;

const
  APP_VERSION: TVersionInfo = (Major: 1; Minor: 1; Release: 0; Build: 0);

implementation

function TVersionInfo.ToString: string;
begin
  Result := Format('%d.%d.%d', [Major, Minor, Release]);
end;

function TVersionInfo.CompareWith(Other: TVersionInfo): Integer;
begin
  // Retourne : -1 si inférieur, 0 si égal, 1 si supérieur
  if Major <> Other.Major then
    Result := Major - Other.Major
  else if Minor <> Other.Minor then
    Result := Minor - Other.Minor
  else if Release <> Other.Release then
    Result := Release - Other.Release
  else
    Result := Build - Other.Build;
end;

class function TVersionInfo.FromString(const VersionStr: string): TVersionInfo;
var
  Parts: TArray<string>;
begin
  Parts := VersionStr.Split(['.']);
  Result.Major := StrToIntDef(Parts[0], 0);
  Result.Minor := StrToIntDef(Parts[1], 0);
  Result.Release := StrToIntDef(Parts[2], 0);
  if Length(Parts) > 3 then
    Result.Build := StrToIntDef(Parts[3], 0)
  else
    Result.Build := 0;
end;

end.
```

### Étape 3 : Créer le module de vérification des mises à jour

```pascal
unit UpdateChecker;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.JSON,
  AppVersion;

type
  TUpdateInfo = record
    Available: Boolean;
    Version: TVersionInfo;
    DownloadURL: string;
    FileSize: Int64;
    FileHash: string;
    IsRequired: Boolean;
    ChangeLog: TArray<string>;
  end;

  TUpdateChecker = class
  private
    FUpdateURL: string;
    FHttpClient: THTTPClient;
  public
    constructor Create(const UpdateURL: string);
    destructor Destroy; override;

    function CheckForUpdates: TUpdateInfo;
  end;

implementation

constructor TUpdateChecker.Create(const UpdateURL: string);
begin
  inherited Create;
  FUpdateURL := UpdateURL;
  FHttpClient := THTTPClient.Create;
end;

destructor TUpdateChecker.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TUpdateChecker.CheckForUpdates: TUpdateInfo;
var
  Response: IHTTPResponse;
  JsonStr: string;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  i: Integer;
begin
  // Initialisation
  Result.Available := False;

  try
    // Télécharger le fichier de version
    Response := FHttpClient.Get(FUpdateURL);

    if Response.StatusCode = 200 then
    begin
      JsonStr := Response.ContentAsString;
      JsonObj := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;

      try
        // Parser les informations
        Result.Version := TVersionInfo.FromString(
          JsonObj.GetValue<string>('version')
        );

        Result.DownloadURL := JsonObj.GetValue<string>('download_url');
        Result.FileSize := JsonObj.GetValue<Int64>('file_size');
        Result.FileHash := JsonObj.GetValue<string>('file_hash');
        Result.IsRequired := JsonObj.GetValue<Boolean>('required');

        // Changelog
        JsonArray := JsonObj.GetValue<TJSONArray>('changelog');
        SetLength(Result.ChangeLog, JsonArray.Count);
        for i := 0 to JsonArray.Count - 1 do
          Result.ChangeLog[i] := JsonArray.Items[i].Value;

        // Vérifier si une mise à jour est disponible
        Result.Available := Result.Version.CompareWith(APP_VERSION) > 0;

      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur, pas de mise à jour disponible
      Result.Available := False;
    end;
  end;
end;

end.
```

### Étape 4 : Créer l'interface utilisateur de mise à jour

Créez un formulaire `TFormUpdate` :

```pascal
unit FormUpdate;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, UpdateChecker;

type
  TFormUpdate = class(TForm)
    LabelTitle: TLabel;
    LabelCurrentVersion: TLabel;
    LabelNewVersion: TLabel;
    MemoChangelog: TMemo;
    ProgressBar: TProgressBar;
    ButtonDownload: TButton;
    ButtonLater: TButton;
    PanelProgress: TPanel;
    LabelProgress: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonLaterClick(Sender: TObject);
  private
    FUpdateInfo: TUpdateInfo;
    FDownloadThread: TThread;
    procedure ShowUpdateInfo;
    procedure DownloadUpdate;
    procedure OnDownloadProgress(Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
    procedure OnDownloadComplete(Sender: TObject);
  public
    class function ShowUpdateDialog(const UpdateInfo: TUpdateInfo): Boolean;
  end;

implementation

uses
  System.Net.HttpClient, AppVersion, System.IOUtils, Winapi.ShellAPI;

{$R *.dfm}

procedure TFormUpdate.FormCreate(Sender: TObject);
begin
  PanelProgress.Visible := False;
end;

class function TFormUpdate.ShowUpdateDialog(const UpdateInfo: TUpdateInfo): Boolean;
var
  Form: TFormUpdate;
begin
  Form := TFormUpdate.Create(nil);
  try
    Form.FUpdateInfo := UpdateInfo;
    Form.ShowUpdateInfo;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TFormUpdate.ShowUpdateInfo;
var
  i: Integer;
  ChangelogText: string;
begin
  // Afficher les informations de version
  LabelCurrentVersion.Caption := 'Version actuelle : ' + APP_VERSION.ToString;
  LabelNewVersion.Caption := 'Nouvelle version : ' + FUpdateInfo.Version.ToString;

  // Afficher le changelog
  ChangelogText := '';
  for i := 0 to Length(FUpdateInfo.ChangeLog) - 1 do
    ChangelogText := ChangelogText + '• ' + FUpdateInfo.ChangeLog[i] + sLineBreak;
  MemoChangelog.Text := ChangelogText;

  // Mise à jour requise ?
  if FUpdateInfo.IsRequired then
  begin
    ButtonLater.Enabled := False;
    LabelTitle.Caption := 'Mise à jour requise';
  end
  else
  begin
    ButtonLater.Enabled := True;
    LabelTitle.Caption := 'Mise à jour disponible';
  end;
end;

procedure TFormUpdate.ButtonDownloadClick(Sender: TObject);
begin
  ButtonDownload.Enabled := False;
  ButtonLater.Enabled := False;
  PanelProgress.Visible := True;
  DownloadUpdate;
end;

procedure TFormUpdate.ButtonLaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormUpdate.DownloadUpdate;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
  TempPath, FileName: string;
begin
  // Créer un dossier temporaire
  TempPath := TPath.GetTempPath + 'MonAppUpdate\';
  ForceDirectories(TempPath);

  FileName := TempPath + 'setup.exe';

  // Télécharger dans un thread séparé
  FDownloadThread := TThread.CreateAnonymousThread(
    procedure
    var
      HttpClient: THTTPClient;
      FileStream: TFileStream;
    begin
      HttpClient := THTTPClient.Create;
      try
        // Gérer la progression
        HttpClient.OnReceiveData := OnDownloadProgress;

        // Télécharger le fichier
        FileStream := TFileStream.Create(FileName, fmCreate);
        try
          HttpClient.Get(FUpdateInfo.DownloadURL, FileStream);
        finally
          FileStream.Free;
        end;

        // Téléchargement terminé
        TThread.Synchronize(nil,
          procedure
          begin
            OnDownloadComplete(nil);

            // Lancer l'installateur
            ShellExecute(0, 'open', PChar(FileName), '/SILENT', nil, SW_SHOWNORMAL);

            // Fermer l'application
            Application.Terminate;
          end
        );
      finally
        HttpClient.Free;
      end;
    end
  );

  FDownloadThread.FreeOnTerminate := True;
  FDownloadThread.Start;
end;

procedure TFormUpdate.OnDownloadProgress(Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
var
  Percentage: Integer;
begin
  if ContentLength > 0 then
  begin
    Percentage := Round((ReadCount / ContentLength) * 100);

    TThread.Synchronize(nil,
      procedure
      begin
        ProgressBar.Position := Percentage;
        LabelProgress.Caption := Format('Téléchargement... %d%%', [Percentage]);
      end
    );
  end;
end;

procedure TFormUpdate.OnDownloadComplete(Sender: TObject);
begin
  LabelProgress.Caption := 'Téléchargement terminé !';
  ProgressBar.Position := 100;
end;

end.
```

### Étape 5 : Intégrer la vérification au démarrage

Dans votre formulaire principal :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Vérifier les mises à jour au démarrage
  CheckForUpdatesAsync;
end;

procedure TMainForm.CheckForUpdatesAsync;
begin
  TTask.Run(
    procedure
    var
      Checker: TUpdateChecker;
      UpdateInfo: TUpdateInfo;
    begin
      Checker := TUpdateChecker.Create('https://monsite.com/updates/version.json');
      try
        UpdateInfo := Checker.CheckForUpdates;

        if UpdateInfo.Available then
        begin
          // Afficher la boîte de dialogue dans le thread principal
          TThread.Synchronize(nil,
            procedure
            begin
              TFormUpdate.ShowUpdateDialog(UpdateInfo);
            end
          );
        end;
      finally
        Checker.Free;
      end;
    end
  );
end;
```

## Solutions existantes pour Delphi

Au lieu de tout coder vous-même, vous pouvez utiliser des composants existants :

### 1. TMS Web Update

**Description** : Composant commercial de TMS Software

**Avantages** :
- Interface graphique complète
- Support de multiples serveurs
- Mise à jour différentielle (seuls les fichiers modifiés)
- Détection automatique des dépendances

**Inconvénients** :
- Payant (~100-200€)

**Site** : https://www.tmssoftware.com/

### 2. Inno Setup avec InnoSetup Downloader

**Description** : Plugin gratuit pour Inno Setup

**Avantages** :
- Totalement gratuit
- Téléchargement de fichiers additionnels
- Mise à jour via installateur

**Inconvénients** :
- Moins flexible qu'une solution personnalisée
- Nécessite Inno Setup

### 3. AppUpdate Component

**Description** : Composant open source

**Avantages** :
- Gratuit et open source
- Simple d'utilisation
- Personnalisable

**Inconvénients** :
- Maintenance parfois irrégulière
- Documentation limitée

**GitHub** : Recherchez "Delphi auto update" sur GitHub

### 4. Winsparkle (pour Windows)

**Description** : Portage Windows du système Sparkle (macOS)

**Avantages** :
- Utilisé par de nombreuses applications
- Bien testé et fiable
- Support des deltas (mises à jour partielles)

**Inconvénients** :
- En C++, nécessite un wrapper pour Delphi
- Configuration initiale complexe

**Site** : https://winsparkle.org/

### 5. Solution maison recommandée

Pour débuter, créez votre propre système simple :
- Fichier JSON pour les versions
- Code Delphi de vérification
- Téléchargement et lancement d'installateur

**Avantages** :
- Contrôle total
- Pas de dépendances
- Apprentissage utile

## Bonnes pratiques

### 1. Vérifier périodiquement, pas à chaque démarrage

❌ **Mauvais** : Vérifier à chaque démarrage
- Ralentit le démarrage
- Consomme de la bande passante
- Agace les utilisateurs

✅ **Bon** : Vérifier tous les X jours
```pascal
procedure TMainForm.CheckForUpdatesIfNeeded;
var
  LastCheck: TDateTime;
  DaysSinceLastCheck: Integer;
begin
  // Lire la date de dernière vérification
  LastCheck := ReadLastCheckDate;
  DaysSinceLastCheck := DaysBetween(Now, LastCheck);

  // Vérifier seulement si plus de 7 jours
  if DaysSinceLastCheck >= 7 then
  begin
    CheckForUpdatesAsync;
    SaveLastCheckDate(Now);
  end;
end;
```

### 2. Permettre la vérification manuelle

Ajoutez toujours une option dans le menu :
```
Menu Aide → Rechercher des mises à jour
```

Cela permet aux utilisateurs de vérifier quand ils le souhaitent.

### 3. Ne pas bloquer l'interface

Faites toujours la vérification et le téléchargement en arrière-plan (avec TTask ou TThread) pour ne pas geler l'interface.

### 4. Gérer les erreurs réseau

```pascal
function TUpdateChecker.CheckForUpdates: TUpdateInfo;
begin
  Result.Available := False;

  try
    // Code de vérification...
  except
    on E: ENetException do
    begin
      // Erreur réseau : mode silencieux, pas d'alerte
      Exit;
    end;
    on E: Exception do
    begin
      // Autre erreur : log mais pas d'alerte
      LogError('Erreur mise à jour : ' + E.Message);
      Exit;
    end;
  end;
end;
```

Ne pas afficher d'erreur si le serveur est inaccessible. L'utilisateur ne doit pas être dérangé.

### 5. Vérifier l'intégrité du téléchargement

Utilisez un hash (SHA256) pour vérifier que le fichier téléchargé n'est pas corrompu :

```pascal
uses
  System.Hash;

function VerifyFileHash(const FileName, ExpectedHash: string): Boolean;
var
  FileStream: TFileStream;
  Hash: string;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Hash := THashSHA2.GetHashString(FileStream);
    Result := SameText(Hash, ExpectedHash);
  finally
    FileStream.Free;
  end;
end;
```

### 6. Sauvegarder les préférences utilisateur

Avant la mise à jour, sauvegardez :
- Configuration
- Données utilisateur
- Préférences

Après la mise à jour, restaurez automatiquement.

### 7. Fournir un changelog visible

Les utilisateurs veulent savoir ce qui change :

```
Version 1.2.0 (20 janvier 2025)
─────────────────────────────────
✓ Correction du bug de synchronisation
✓ Nouvelle fonctionnalité d'export PDF
✓ Amélioration des performances (30% plus rapide)
✓ Interface modernisée
```

### 8. Permettre de reporter la mise à jour

Sauf si c'est une mise à jour critique, laissez l'utilisateur reporter :

```pascal
if not UpdateInfo.IsRequired then
begin
  if MessageDlg('Une mise à jour est disponible. Installer maintenant ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;
end;
```

### 9. Gérer les versions minimales

Si une version est trop ancienne, forcez la mise à jour :

```pascal
// Dans version.json
{
  "version": "2.0.0",
  "min_version": "1.5.0",
  "required": true
}

// Dans le code
if APP_VERSION.CompareWith(MinVersion) < 0 then
begin
  ShowMessage('Votre version est trop ancienne. La mise à jour est obligatoire.');
  // Forcer la mise à jour
end;
```

### 10. Tester le mécanisme de mise à jour

Testez régulièrement votre système de mise à jour :
- Sur différentes versions de Windows
- Avec et sans droits admin
- Avec connexion lente
- Avec serveur inaccessible

## Sécurité des mises à jour

### 1. Utiliser HTTPS

**Toujours** utiliser HTTPS pour :
- Le fichier de version (version.json)
- Le téléchargement de la mise à jour

Cela empêche les attaques "man-in-the-middle" où quelqu'un pourrait remplacer votre mise à jour par un malware.

```pascal
// ✓ Bon
const UPDATE_URL = 'https://monsite.com/updates/version.json';

// ✗ Mauvais
const UPDATE_URL = 'http://monsite.com/updates/version.json';  // Non sécurisé !
```

### 2. Vérifier la signature du fichier téléchargé

Le fichier de mise à jour doit être signé numériquement :

```pascal
function VerifySignature(const FileName: string): Boolean;
var
  WinTrustData: TWinTrustData;
  FileInfo: TWinTrustFileInfo;
begin
  // Code de vérification de signature Windows
  // ... (complexe, utilisez une bibliothèque)
  Result := True; // Simplification
end;
```

### 3. Utiliser des hashes

Incluez le hash SHA256 du fichier dans version.json :

```json
{
  "version": "1.2.0",
  "download_url": "https://...",
  "file_hash": "SHA256:a1b2c3d4e5f6789..."
}
```

Vérifiez le hash après téléchargement avant d'exécuter.

### 4. Ne jamais exécuter du code non vérifié

Ne téléchargez et n'exécutez jamais de code arbitraire. Seuls des installateurs signés doivent être lancés.

### 5. Limiter les permissions

Si possible, effectuez la mise à jour sans droits administrateur. Utilisez un dossier utilisateur pour le téléchargement temporaire.

## Gestion des canaux de mise à jour

Pour les applications professionnelles, proposez différents canaux :

### Canal Stable

- Versions testées et stables
- Mises à jour moins fréquentes
- Pour les utilisateurs finaux

### Canal Beta

- Nouvelles fonctionnalités avant tout le monde
- Plus de bugs possibles
- Pour les early adopters

### Canal Dev/Alpha

- Derniers développements
- Instable
- Pour les testeurs

**Implémentation** :

```json
// version-stable.json
{
  "channel": "stable",
  "version": "1.2.0"
}

// version-beta.json
{
  "channel": "beta",
  "version": "1.3.0-beta"
}
```

```pascal
type
  TUpdateChannel = (ucStable, ucBeta, ucDev);

function GetUpdateURL(Channel: TUpdateChannel): string;
begin
  case Channel of
    ucStable: Result := 'https://monsite.com/updates/version-stable.json';
    ucBeta:   Result := 'https://monsite.com/updates/version-beta.json';
    ucDev:    Result := 'https://monsite.com/updates/version-dev.json';
  end;
end;
```

## Mises à jour différentielles (avancé)

Pour les grandes applications, télécharger l'installateur complet à chaque fois peut être lourd.

### Principe

Au lieu de télécharger 50 MB, téléchargez seulement les 2 MB qui ont changé.

### Solutions

**1. Fichiers patch binaires**
- Utilisez des outils comme `bsdiff` pour créer des patches
- Appliquez-les avec `bspatch`

**2. Mise à jour fichier par fichier**
```json
{
  "version": "1.2.0",
  "files": [
    {
      "name": "MonApp.exe",
      "url": "https://...",
      "hash": "..."
    },
    {
      "name": "config.dll",
      "url": "https://...",
      "hash": "..."
    }
  ]
}
```

Téléchargez et remplacez seulement les fichiers modifiés.

## Déploiement silencieux en entreprise

Pour les environnements d'entreprise, permettez les installations silencieuses :

```pascal
// Paramètres ligne de commande
if ParamStr(1) = '/update' then
begin
  // Mode mise à jour silencieuse
  SilentUpdate;
  Exit;
end;

procedure SilentUpdate;
begin
  // Pas d'interface
  // Télécharger et installer automatiquement
  // Logger les résultats dans un fichier
end;
```

Lancement :
```cmd
MonApp.exe /update
```

## Problèmes courants et solutions

### L'utilisateur n'a pas les droits administrateur

**Problème** : L'installation nécessite des droits admin

**Solutions** :
- Installez dans un dossier utilisateur (`%LOCALAPPDATA%`)
- Utilisez ClickOnce pour les applications .NET
- Demandez l'élévation seulement si nécessaire

### La mise à jour échoue car le fichier est en cours d'utilisation

**Problème** : Impossible de remplacer `MonApp.exe` pendant qu'il tourne

**Solution** : Utilisez un "updater" externe

```pascal
// MonApp.exe détecte une mise à jour
// Lance Updater.exe avec les paramètres
ShellExecute(0, 'open', 'Updater.exe',
  PChar('install "' + SetupFile + '"'), nil, SW_SHOWNORMAL);

// Ferme MonApp.exe
Application.Terminate;

// Updater.exe installe la mise à jour
// Puis relance MonApp.exe
```

### Le serveur de mise à jour est inaccessible

**Problème** : Pas de connexion Internet ou serveur down

**Solution** : Gestion d'erreur silencieuse
```pascal
try
  CheckForUpdates;
except
  // Log l'erreur mais ne dérange pas l'utilisateur
  on E: Exception do
    LogError('Vérification mise à jour impossible : ' + E.Message);
end;
```

### La mise à jour est bloquée par le pare-feu/antivirus

**Problème** : Téléchargement bloqué

**Solutions** :
- Signez votre installateur
- Utilisez HTTPS
- Testez avec différents antivirus
- Fournissez une mise à jour manuelle alternative

### Plusieurs instances de l'application sont ouvertes

**Problème** : Impossible de mettre à jour si plusieurs instances tournent

**Solution** : Détectez et fermez toutes les instances
```pascal
function CloseAllInstances: Boolean;
var
  hWindow: HWND;
begin
  repeat
    hWindow := FindWindow('TMainForm', nil);
    if hWindow <> 0 then
    begin
      SendMessage(hWindow, WM_CLOSE, 0, 0);
      Sleep(100);
    end;
  until hWindow = 0;
  Result := True;
end;
```

## Tableau de bord de mise à jour (facultatif)

Pour les applications professionnelles, créez un tableau de bord qui montre :

- Nombre de téléchargements par version
- Taux d'adoption de chaque version
- Erreurs de mise à jour rencontrées
- Versions encore en circulation

Cela vous aide à :
- Identifier les problèmes rapidement
- Savoir quand arrêter le support d'anciennes versions
- Mesurer le succès des mises à jour

**Outils** :
- Google Analytics
- Télémétrie personnalisée
- Bases de données de statistiques

## Checklist d'implémentation

Avant de déployer votre système de mise à jour :

- [ ] Fichier de version accessible via HTTPS
- [ ] Format de version cohérent (X.Y.Z)
- [ ] Vérification en arrière-plan (ne bloque pas l'UI)
- [ ] Téléchargement avec barre de progression
- [ ] Vérification de l'intégrité (hash)
- [ ] Signature de l'installateur
- [ ] Gestion des erreurs réseau
- [ ] Option "Télécharger plus tard"
- [ ] Changelog visible pour l'utilisateur
- [ ] Test sur différentes versions de Windows
- [ ] Test avec connexion lente
- [ ] Test avec serveur inaccessible
- [ ] Documentation pour les utilisateurs
- [ ] Plan de rollback en cas de problème
- [ ] Journalisation des mises à jour

## Exemple d'intégration complète

Voici comment intégrer tout cela dans votre application principale :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Menus,
  UpdateChecker, AppVersion;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuHelp: TMenuItem;
    MenuCheckUpdates: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure MenuCheckUpdatesClick(Sender: TObject);
  private
    procedure CheckForUpdatesIfNeeded;
    procedure CheckForUpdatesAsync(ShowNoUpdateMessage: Boolean = False);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Threading, System.DateUtils, System.IniFiles, FormUpdate;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Vérifier les mises à jour au démarrage (si nécessaire)
  CheckForUpdatesIfNeeded;
end;

procedure TMainForm.CheckForUpdatesIfNeeded;
var
  IniFile: TIniFile;
  LastCheck: TDateTime;
  DaysSinceLastCheck: Integer;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Lire la date de dernière vérification
    LastCheck := IniFile.ReadDateTime('Updates', 'LastCheck', 0);
    DaysSinceLastCheck := DaysBetween(Now, LastCheck);

    // Vérifier seulement tous les 7 jours
    if (LastCheck = 0) or (DaysSinceLastCheck >= 7) then
    begin
      CheckForUpdatesAsync(False);
      IniFile.WriteDateTime('Updates', 'LastCheck', Now);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.CheckForUpdatesAsync(ShowNoUpdateMessage: Boolean);
begin
  TTask.Run(
    procedure
    var
      Checker: TUpdateChecker;
      UpdateInfo: TUpdateInfo;
    begin
      Checker := TUpdateChecker.Create('https://monsite.com/updates/version.json');
      try
        UpdateInfo := Checker.CheckForUpdates;

        TThread.Synchronize(nil,
          procedure
          begin
            if UpdateInfo.Available then
            begin
              // Mise à jour disponible
              TFormUpdate.ShowUpdateDialog(UpdateInfo);
            end
            else if ShowNoUpdateMessage then
            begin
              // Vérification manuelle : informer qu'il n'y a pas de mise à jour
              ShowMessage('Votre application est à jour.');
            end;
          end
        );
      finally
        Checker.Free;
      end;
    end
  );
end;

procedure TMainForm.MenuCheckUpdatesClick(Sender: TObject);
begin
  // Vérification manuelle
  CheckForUpdatesAsync(True);
end;

end.
```

## Conclusion

La mise à jour automatique est un élément essentiel d'une application moderne. Elle permet de :

- **Distribuer rapidement** les corrections de bugs et nouvelles fonctionnalités
- **Améliorer la sécurité** en déployant des patches rapidement
- **Simplifier le support** en gardant tous les utilisateurs à jour
- **Offrir une meilleure expérience** avec des mises à jour transparentes

**Points clés à retenir** :

1. Commencez simple : fichier JSON + téléchargement + installation
2. Vérifiez en arrière-plan, ne bloquez jamais l'interface
3. Utilisez HTTPS et vérifiez l'intégrité des fichiers
4. Permettez à l'utilisateur de reporter (sauf urgence)
5. Testez abondamment votre système de mise à jour
6. Signez vos installateurs
7. Gérez gracieusement les erreurs réseau
8. Fournissez un changelog clair

Avec un bon système de mise à jour, vous maintenez une relation continue avec vos utilisateurs et assurez que votre application reste performante, sécurisée et à jour. Dans la prochaine section, nous verrons comment déployer votre application sur différentes plateformes (Windows, macOS, Linux, mobile).

⏭️ [Déploiement sur différentes plateformes](/17-distribution-et-deploiement/06-deploiement-sur-differentes-plateformes.md)
