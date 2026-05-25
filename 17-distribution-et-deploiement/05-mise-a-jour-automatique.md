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
// Helper local : compare deux Integer en retournant -1/0/1.
// ⚠ Renvoyer `A - B` directement risquerait l'overflow si A et B sont
//   très éloignés (cas théorique pour des versions, mais bonne habitude).

  function CmpInt(A, B: Integer): Integer;
  begin
    if A < B then      Result := -1
    else if A > B then Result := 1
    else               Result := 0;
  end;

begin
  Result := CmpInt(Major, Other.Major);
  if Result <> 0 then Exit;
  Result := CmpInt(Minor, Other.Minor);
  if Result <> 0 then Exit;
  Result := CmpInt(Release, Other.Release);
  if Result <> 0 then Exit;
  Result := CmpInt(Build, Other.Build);
end;

class function TVersionInfo.FromString(const VersionStr: string): TVersionInfo;  
var  
  Parts: TArray<string>;
begin
  // ⚠ Toujours initialiser le record car les champs ne sont PAS effacés
  //   automatiquement pour un record local non managé.
  Result.Major   := 0;
  Result.Minor   := 0;
  Result.Release := 0;
  Result.Build   := 0;

  // ⚠ Toujours vérifier Length(Parts) AVANT d'indexer : une chaîne vide
  //   ou tronquée (« 1 », « 1.2 », « ») provoquerait sinon une
  //   « Index out of bounds » sur Parts[1] ou Parts[2].
  Parts := VersionStr.Split(['.']);
  if Length(Parts) > 0 then Result.Major   := StrToIntDef(Parts[0], 0);
  if Length(Parts) > 1 then Result.Minor   := StrToIntDef(Parts[1], 0);
  if Length(Parts) > 2 then Result.Release := StrToIntDef(Parts[2], 0);
  if Length(Parts) > 3 then Result.Build   := StrToIntDef(Parts[3], 0);
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
  JsonStr, VersionStr: string;
  JsonValue: TJSONValue;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  i: Integer;
begin
  // Initialisation
  Result.Available := False;
  Result.IsRequired := False;
  Result.FileSize := 0;

  try
    // Télécharger le fichier de version
    Response := FHttpClient.Get(FUpdateURL);

    if Response.StatusCode = 200 then
    begin
      JsonStr := Response.ContentAsString;
      // ⚠ ParseJSONValue peut retourner nil (JSON invalide) ou un type
      //   autre que TJSONObject (un array, un nombre, etc. si la page
      //   reçue n'est pas un objet). Faire un `as TJSONObject` direct
      //   lèverait alors EInvalidCast. On teste donc le type avant.
      JsonValue := TJSONObject.ParseJSONValue(JsonStr);
      if not (JsonValue is TJSONObject) then
      begin
        JsonValue.Free;  // Libère même si nil (Free est nil-safe).
        Exit;
      end;
      JsonObj := TJSONObject(JsonValue);
      try
        // (JsonObj est forcément non-nil ici grâce au test ci-dessus.)

        // Parser les informations avec TryGetValue (ne lève PAS d'exception
        // si une clé est absente — retourne juste False).
        if not JsonObj.TryGetValue<string>('version', VersionStr) then
          Exit;
        Result.Version := TVersionInfo.FromString(VersionStr);

        JsonObj.TryGetValue<string>('download_url', Result.DownloadURL);
        JsonObj.TryGetValue<Int64>('file_size', Result.FileSize);
        JsonObj.TryGetValue<string>('file_hash', Result.FileHash);
        JsonObj.TryGetValue<Boolean>('required', Result.IsRequired);

        // Changelog (facultatif)
        if JsonObj.TryGetValue<TJSONArray>('changelog', JsonArray) then
        begin
          SetLength(Result.ChangeLog, JsonArray.Count);
          for i := 0 to JsonArray.Count - 1 do
            Result.ChangeLog[i] := JsonArray.Items[i].Value;
        end;

        // Vérifier si une mise à jour est disponible
        Result.Available := Result.Version.CompareWith(APP_VERSION) > 0;

      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur réseau ou JSON, pas de mise à jour proposée.
      // En production, logger l'erreur pour diagnostic.
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
  System.Net.HttpClient, AppVersion, System.IOUtils, Vcl.Dialogs,
  Winapi.ShellAPI;

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
  TempPath, FileName: string;
begin
  // Créer un sous-dossier temporaire propre à l'app (TOCTOU : éviter
  // `TPath.GetTempPath` directement, qui est partagé entre toutes les apps
  // de l'utilisateur — un attaquant pourrait pré-créer `setup.exe`).
  // En production, ajouter un nombre aléatoire au PID pour rendre le nom
  // de répertoire moins prédictible :
  //   TempPath := ... + IntToHex(GetCurrentProcessId, 8) + '_' + ...random...;
  TempPath := TPath.Combine(TPath.GetTempPath,
                            'MonAppUpdate_' + IntToHex(GetCurrentProcessId, 8));
  ForceDirectories(TempPath);

  FileName := TPath.Combine(TempPath, 'setup.exe');
  // Capturer TempPath dans une variable locale réutilisable par le ShellExecute
  // plus bas (5e paramètre = lpDirectory : éviter `nil` qui hériterait du
  // working dir parent — souvent Program Files, lecture seule).

  // Télécharger dans un thread séparé
  FDownloadThread := TThread.CreateAnonymousThread(
    procedure
    var
      HttpClient: THTTPClient;
      FileStream: TFileStream;
      HashOK: Boolean;
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

        // ⚠ SÉCURITÉ : vérifier l'intégrité du fichier téléchargé AVANT
        //   de l'exécuter. Le hash attendu vient du JSON serveur signé
        //   (via TLS) ; un fichier corrompu ou substitué (MITM, miroir
        //   compromis) ne doit pas être lancé. Voir helper plus bas
        //   `VerifyFileHash(FileName, ExpectedHash)` qui retourne Boolean.
        HashOK := (FUpdateInfo.FileHash <> '') and
                  VerifyFileHash(FileName, FUpdateInfo.FileHash);
        // Pour une vérification supplémentaire, valider aussi la signature
        // Authenticode du binaire (cf section 16.9 + WinVerifyTrust).

        // Téléchargement terminé
        TThread.Synchronize(nil,
          procedure
          begin
            OnDownloadComplete(nil);

            if not HashOK then
            begin
              ShowMessage('Mise à jour rejetée : intégrité non vérifiée.' +
                          sLineBreak + 'Le fichier téléchargé ne correspond ' +
                          'pas au hash attendu. Veuillez réessayer plus tard.');
              DeleteFile(FileName);
              Exit;
            end;

            // Lancer l'installateur (Inno Setup en mode silencieux).
            // 5e paramètre (lpDirectory) : TempPath, pas nil — sinon le
            // working dir hérité pourrait être Program Files (lecture seule).
            // ⚠ ShellExecute retourne une HINSTANCE : <= 32 = erreur
            //   (ERROR_FILE_NOT_FOUND, ERROR_ACCESS_DENIED, etc.).
            //   Ne terminer l'application QUE si l'installateur a bien démarré.
            if NativeInt(ShellExecute(0, 'open', PChar(FileName), '/SILENT',
                                      PChar(ExtractFilePath(FileName)),
                                      SW_SHOWNORMAL)) <= 32 then
            begin
              ShowMessage('Impossible de lancer l''installateur de mise à jour. ' +
                          'L''application va continuer à fonctionner ; ' +
                          'réessayez plus tard.');
              Exit;
            end;

            // L'installateur a été lancé : fermer l'application pour qu'il
            // puisse remplacer le binaire.
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

// `VerifyFileHash` est défini plus bas dans la section « 5. Vérifier
// l'intégrité du téléchargement » — il compare le hash SHA-256 du
// fichier avec un hash attendu (Boolean).

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

### 3. Composants open source Delphi

Plusieurs projets communautaires existent sur GitHub. Recherchez avec les mots-clés `delphi auto-update`, `delphi updater`, `delphi self-update`. Vérifiez avant adoption :

- **Date du dernier commit** : projet maintenu activement ?
- **Compatibilité Delphi 13 Florence** : code récent et testé sur RAD Studio actuel ?
- **Sécurité** : vérification d'intégrité (hash) et de signature (Authenticode) intégrée ?
- **Licence** : MIT, MPL, GPL — compatible avec votre projet ?

À évaluer cas par cas — la qualité varie énormément.

### 4. Winsparkle (pour Windows)

**Description** : portage Windows du système Sparkle (macOS, créé par Andy Matuschak).

**Avantages** :
- Utilisé par de nombreuses applications (Audacity, KeePass, etc.).
- Bien testé et fiable.
- Format d'« *appcast* » RSS standardisé.
- Support des signatures Ed25519 pour vérifier l'authenticité des mises à jour.

**Inconvénients** :
- En C/C++, nécessite un wrapper pour Delphi (DLL chargée dynamiquement, FFI sur quelques fonctions C — réalisable mais demande du travail).
- Pas de support officiel Delphi.

**Site** : https://winsparkle.org/

### 5. Sparkle (pour macOS)

Sur **macOS**, Sparkle est la référence absolue (à peu près obligatoire pour distribuer hors App Store) :
- Intégration avec la notarisation Apple.
- Support EdDSA signatures depuis Sparkle 2.
- Apparence native macOS (Sonoma 14, Sequoia 15, Tahoe 26).
- Téléchargements deltas pour réduire la taille des mises à jour.

À utiliser si vous distribuez votre app Delphi macOS hors App Store. Comme pour Winsparkle, c'est une bibliothèque C/Objective-C : il vous faudra écrire un mince wrapper Delphi (chargement dynamique via `dlopen`/`LibLoad`, FFI Pascal sur quelques fonctions exportées).

### 6. Velopack (alternative moderne)

**Velopack** (https://velopack.io/) est l'évolution moderne de Squirrel.Windows :
- Cross-platform (Windows + macOS + Linux).
- Mises à jour différentielles (deltas) ultra-rapides.
- Pas de droits administrateur requis (install per-user).
- API CLI, intégrable avec n'importe quel langage incluant Delphi.
- Open source MIT.

Émergeant en 2024-2026 comme solution générique recommandée pour le desktop.

### 7. Solution maison recommandée

Pour débuter, créez votre propre système simple :
- Fichier JSON pour les versions.
- Code Delphi de vérification (cf exemple ci-dessus).
- Téléchargement et lancement d'installateur (Inno Setup en mode `/SILENT`).

**Avantages** :
- Contrôle total.
- Pas de dépendances externes.
- Apprentissage utile.

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
uses
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.URLClient;

function TUpdateChecker.CheckForUpdates: TUpdateInfo;  
begin  
  Result.Available := False;

  try
    // Code de vérification...
  except
    // ⚠ Les exceptions réseau de System.Net.HttpClient sont :
    //   - ENetHTTPClientException : erreur du client HTTP (TLS, DNS, refus…)
    //   - ENetURIException        : URL invalide
    //   Capter ces deux suffit pour le mode « silencieux » réseau.
    on E: ENetHTTPClientException do
    begin
      // Erreur réseau / TLS / DNS : silence, l'utilisateur ne doit pas
      // être dérangé pour une simple absence de connectivité.
      Exit;
    end;
    on E: ENetURIException do
    begin
      Exit;
    end;
    on E: Exception do
    begin
      // Autre erreur (parsing JSON, hash, etc.) : logger pour diagnostic
      // mais ne pas afficher de boîte d'erreur.
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
  System.SysUtils, System.Hash;

function VerifyFileHash(const FileName, ExpectedHash: string): Boolean;  
var  
  FileStream: TFileStream;
  Hash, NormalizedExpected: string;
begin
  // ⚠ Le JSON serveur peut préfixer le hash par "SHA256:" (convention
  //   répandue pour préciser l'algorithme). On normalise pour ne comparer
  //   que la partie hexadécimale, insensible à la casse.
  NormalizedExpected := ExpectedHash;
  if NormalizedExpected.StartsWith('SHA256:', True) then
    Delete(NormalizedExpected, 1, Length('SHA256:'));
  NormalizedExpected := Trim(NormalizedExpected);

  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Hash := THashSHA2.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;

  Result := (NormalizedExpected <> '') and SameText(Hash, NormalizedExpected);
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

Si une version est trop ancienne (faille de sécurité, format de données incompatible…), forcez la mise à jour :

```json
// Dans version.json
{
  "version": "2.0.0",
  "min_version": "1.5.0",
  "required": true
}
```

```pascal
// Dans le code, après avoir reçu et parsé le JSON :
var
  MinVersionStr: string;
  MinVersion: TVersionInfo;
begin
  // ⚠ Récupérer min_version du JSON avant utilisation (le champ peut
  //   ne pas être présent — fournir une valeur par défaut "0.0.0").
  if not JsonObj.TryGetValue<string>('min_version', MinVersionStr) then
    MinVersionStr := '0.0.0';
  MinVersion := TVersionInfo.FromString(MinVersionStr);

  if APP_VERSION.CompareWith(MinVersion) < 0 then
  begin
    ShowMessage('Votre version est trop ancienne. La mise à jour est obligatoire.');
    // Forcer la mise à jour : empêcher l'utilisateur de continuer
    // sans installer (ButtonLater.Enabled := False ; Result.IsRequired := True).
  end;
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

Le fichier de mise à jour doit être signé numériquement, et l'application doit **vérifier la signature avant de l'exécuter** (cf section 16.9 du chapitre Sécurité pour le détail) :

```pascal
uses
  Winapi.Windows, Winapi.WinTrust, Winapi.SoftPub;

function VerifierSignatureAuthenticode(const AFichier: string): Boolean;  
var  
  FileInfo: WINTRUST_FILE_INFO;
  TrustData: WINTRUST_DATA;
  Action: TGUID;
  Status: HRESULT;
  FichierW: WideString;
begin
  Action := WINTRUST_ACTION_GENERIC_VERIFY_V2;
  FichierW := AFichier;

  FillChar(FileInfo, SizeOf(FileInfo), 0);
  FileInfo.cbStruct := SizeOf(FileInfo);
  FileInfo.pcwszFilePath := PWideChar(FichierW);

  FillChar(TrustData, SizeOf(TrustData), 0);
  TrustData.cbStruct := SizeOf(TrustData);
  TrustData.dwUIChoice := WTD_UI_NONE;
  TrustData.fdwRevocationChecks := WTD_REVOKE_WHOLECHAIN;
  TrustData.dwUnionChoice := WTD_CHOICE_FILE;
  TrustData.pFile := @FileInfo;
  TrustData.dwStateAction := WTD_STATEACTION_VERIFY;

  Status := WinVerifyTrust(INVALID_HANDLE_VALUE, Action, @TrustData);
  Result := Status = ERROR_SUCCESS;

  // Libérer l'état interne
  TrustData.dwStateAction := WTD_STATEACTION_CLOSE;
  WinVerifyTrust(INVALID_HANDLE_VALUE, Action, @TrustData);
end;
```

Cette fonction utilise l'API Windows `WinVerifyTrust` qui :
1. valide la signature cryptographique du fichier ;
2. vérifie la chaîne du certificat jusqu'à une CA de confiance ;
3. vérifie la révocation (CRL/OCSP) ;
4. valide l'horodatage RFC 3161 (pour les certificats expirés).

**Encore mieux** : combinez avec un *pinning* du certificat (cf chapitre 16.4) pour n'accepter que les mises à jour signées par **votre** certificat précis, pas n'importe quel certificat valide.

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
- **Installation per-user dans `%LOCALAPPDATA%\Programs\MonApp\`** : pas de droits admin requis. Configurable dans Inno Setup via `PrivilegesRequired=lowest` et `DefaultDirName={localappdata}\Programs\MonApp`.
- **MSIX** (cf section 17.8) : par défaut, installation per-user sans élévation.
- **Demandez l'élévation seulement si nécessaire** (ex : écriture dans Program Files, service Windows à enregistrer).
- ~~ClickOnce~~ : technologie spécifique à .NET, **non applicable aux applications Delphi natives**.

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
  System.Threading, System.DateUtils, System.IniFiles, System.IOUtils,
  FormUpdate;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);  
begin  
  // Vérifier les mises à jour au démarrage (si nécessaire)
  CheckForUpdatesIfNeeded;
end;

procedure TMainForm.CheckForUpdatesIfNeeded;  
var  
  IniPath: string;
  IniFile: TIniFile;
  LastCheck: TDateTime;
  DaysSinceLastCheck: Integer;
begin
  // ⚠ NE PAS écrire à côté de l'EXE — Program Files est lecture seule
  //   pour un utilisateur standard. La virtualisation UAC redirigerait
  //   silencieusement vers VirtualStore et la valeur ne serait jamais
  //   relue. On stocke donc dans le dossier de l'utilisateur.
  //
  // ⚠ Précision sur `TPath.GetHomePath` :
  //   - Windows : retourne `%USERPROFILE%` (ex : `C:\Users\<user>`).
  //   - macOS / Linux : retourne `~` (home utilisateur).
  //   Si vous voulez cibler PRÉCISÉMENT `%APPDATA%\Roaming` sur Windows,
  //   utilisez : `TPath.Combine(TPath.GetHomePath, 'AppData\Roaming\MonApp\…')`
  //   ou `GetEnvironmentVariable('APPDATA')`.
  IniPath := TPath.Combine(
    TPath.GetHomePath,            // %USERPROFILE% sous Windows
    'MonApp\settings.ini');
  ForceDirectories(ExtractFilePath(IniPath));

  IniFile := TIniFile.Create(IniPath);
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
