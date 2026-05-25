🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.10 Télémétrie et analyse de crash

## Introduction

Imaginez que vous avez distribué votre application à 10 000 utilisateurs. Un jour, vous recevez un email : "Votre application plante quand je clique sur ce bouton". Mais quand vous testez, tout fonctionne parfaitement chez vous. Comment savoir ce qui s'est vraiment passé ?

C'est exactement le problème que résolvent la **télémétrie** et l'**analyse de crash**. Ces technologies permettent à votre application de vous "raconter" ce qui se passe chez vos utilisateurs, vous aidant à :

- Détecter les bugs que vous n'avez jamais rencontrés
- Comprendre comment les utilisateurs utilisent vraiment votre application
- Identifier les fonctionnalités les plus populaires
- Mesurer les performances réelles
- Corriger les problèmes avant que les utilisateurs ne s'en plaignent

Dans cette section finale du chapitre sur la distribution et le déploiement, nous explorerons comment surveiller vos applications Delphi en production.

## Qu'est-ce que la télémétrie ?

### Définition simple

La **télémétrie** (du grec "mesure à distance") est la collecte automatique de données sur le fonctionnement de votre application.

**Analogie** : C'est comme la "boîte noire" d'un avion. Elle enregistre en continu ce qui se passe pour pouvoir analyser plus tard en cas de problème.

### Types de données collectées

La télémétrie peut inclure :

**1. Données d'utilisation**
- Fonctionnalités utilisées
- Fréquence d'utilisation
- Durée des sessions
- Parcours utilisateur (où ils cliquent)

**2. Données techniques**
- Version de l'application
- Système d'exploitation
- Résolution d'écran
- Langue et région

**3. Données de performance**
- Temps de démarrage
- Temps de réponse
- Utilisation de la mémoire
- Utilisation du CPU

**4. Erreurs et exceptions**
- Crashs et plantages
- Exceptions non gérées
- Erreurs capturées
- Stack traces (traces d'exécution)

### Télémétrie vs Analytics

| Télémétrie | Analytics |
|------------|-----------|
| Données techniques | Données comportementales |
| Performance, erreurs | Parcours, conversions |
| Pour développeurs | Pour business/marketing |
| Détecte les bugs | Optimise l'expérience |

**Exemple** :
- **Télémétrie** : "L'application a planté 50 fois aujourd'hui sur la fonction Export"
- **Analytics** : "80% des utilisateurs n'utilisent jamais la fonction Export"

Les deux sont complémentaires !

## Pourquoi implémenter la télémétrie ?

### 1. Détection proactive des problèmes

**Sans télémétrie** :
```
Utilisateur 1 : Crash → Frustré, abandonne  
Utilisateur 2 : Crash → Frustré, abandonne  
Utilisateur 3 : Crash → Envoie un email vague  
Vous : Découvrez le problème 2 semaines plus tard  
```

**Avec télémétrie** :
```
Utilisateurs 1, 2, 3 : Crash → Rapport automatique  
Vous : Alerte instantanée, stack trace détaillée  
Vous : Correction en 2 heures, patch envoyé  
```

### 2. Comprendre l'utilisation réelle

Vous pensez que la fonctionnalité X est essentielle, mais la télémétrie révèle que personne ne l'utilise. Vous pouvez alors :
- Améliorer son accessibilité
- Ou la supprimer pour simplifier l'application

### 3. Prioriser les corrections

Quel bug corriger en premier ?

**Sans données** : Vous devinez

**Avec télémétrie** :
```
Bug A : 5 utilisateurs affectés (0,05%)  
Bug B : 500 utilisateurs affectés (5%)  
Bug C : 5000 utilisateurs affectés (50%)  
```

Clairement, Bug C en priorité !

### 4. Valider les hypothèses

Vous pensez que la nouvelle fonctionnalité va plaire ?

**Télémétrie avant/après** :
```
Avant : Engagement 10 min/jour  
Après : Engagement 15 min/jour (+50%)  
```

Succès confirmé par les données !

### 5. Support client amélioré

**Client** : "L'application plante tout le temps !"

**Vous (sans télémétrie)** : "Pouvez-vous nous donner plus de détails ?"

**Vous (avec télémétrie)** : "Je vois que vous avez eu 3 crashs la semaine dernière, tous liés au module X sur Windows 11. Nous avons identifié le problème et un correctif arrive demain."

Réponse professionnelle et rassurante !

## Qu'est-ce que l'analyse de crash ?

### Définition

L'**analyse de crash** est un sous-ensemble de la télémétrie qui se concentre spécifiquement sur les plantages de l'application.

Quand votre application plante, un **rapport de crash** est généré contenant :

1. **Exception** : Quelle erreur s'est produite
2. **Stack trace** : Où dans le code l'erreur est survenue
3. **Contexte** : État de l'application au moment du crash
4. **Environnement** : OS, version, mémoire disponible, etc.

### Anatomie d'un rapport de crash

```
=== Crash Report ===
Date: 2026-04-15 14:32:15  
Application: MonApp v1.2.0  
OS: Windows 11 24H2 (Build 26100)  
Exception: EAccessViolation  
Message: Access violation at address 00405C7A. Read of address 00000000.  

Stack Trace:
  MonApp.exe  TMainForm.Button1Click  (MainForm.pas:145)
  MonApp.exe  TControl.Click          (Vcl.Controls.pas:7265)
  MonApp.exe  TButton.Click           (Vcl.StdCtrls.pas:4523)
  MonApp.exe  TWinControl.WndProc     (Vcl.Controls.pas:10156)

Context:
  Current User: <ID anonymisé : 8e2f...c3d1>
  Last Action: Export to PDF
  Memory Usage: 245 MB / 8 GB
  Active Form: TMainForm
```

> ⚠️ **Ne jamais inclure de nom d'utilisateur réel dans un rapport de crash** — utilisez toujours un identifiant anonymisé (hash machine, GUID local). Pour les rapports RGPD-compatibles, voir la section « Données à anonymiser » plus bas.

Avec ces informations, vous pouvez identifier et corriger le problème rapidement.

## Implémenter la télémétrie basique dans Delphi

### Étape 1 : Créer une unité de télémétrie

```pascal
unit Telemetry;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient;

type
  TTelemetryEventType = (teAppStart, teAppClose, teFeatureUsed, teError, teCrash);

  TTelemetry = class
  private
    FServerURL: string;
    FAppVersion: string;
    FUserID: string;
    FEnabled: Boolean;
    FHttpClient: THTTPClient;

    procedure SendData(const EventData: TJSONObject);
    function GetSystemInfo: TJSONObject;
  public
    constructor Create(const ServerURL, AppVersion: string);
    destructor Destroy; override;

    procedure TrackEvent(EventType: TTelemetryEventType; const EventName: string;
      const Data: TJSONObject = nil);
    procedure TrackError(const ErrorMessage, StackTrace: string);
    procedure TrackCrash(E: Exception);

    property Enabled: Boolean read FEnabled write FEnabled;
    property UserID: string read FUserID write FUserID;
  end;

var
  Telemetry: TTelemetry;

implementation

uses
  System.SysUtils, System.DateUtils, System.IOUtils,
  System.IniFiles, System.Classes;
  // ⚠ Le code de TTelemetry est volontairement **cross-platform** (RTL
  //   uniquement, pas d'appel `Winapi.Windows`). Si vous avez besoin
  //   d'API Windows spécifiques (ex : `GetComputerName`), ajoutez
  //   `{$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}` au début du uses.
  //
  // ⚠ `System.SysInfo` n'existe pas dans la RTL Delphi.
  //   - `TOSVersion` (OS) : System.SysUtils.
  //   - `TPath`, `ForceDirectories` : System.IOUtils.
  //   - `TIniFile` : System.IniFiles.
  //   - `CreateGUID`, `GUIDToString` : System.SysUtils.
  //   - `TTimeZone` (UTC) : System.DateUtils.
  //   - `TThread`, `TStringStream` : System.Classes.
  //   - `THashMD5` / `THashSHA2` (si besoin) : System.Hash (non utilisé ici
  //     depuis qu'on a un GUID v4 pour l'identifiant utilisateur).

// ⚠ Helper privé : garantit un ID unique par installation, stocké côté
//   utilisateur, sans informations identifiantes (juste un GUID v4
//   aléatoire). Déclaré AVANT le constructeur qui l'utilise — Pascal
//   exige que les identifiants soient déclarés avant leur usage.
function GetOrCreateAnonymousID: string;  
var  
  IniPath: string;
  Ini: TIniFile;
  ID: TGUID;
begin
  IniPath := TPath.Combine(TPath.GetHomePath, 'MonApp\telemetry.ini');
  ForceDirectories(ExtractFilePath(IniPath));

  Ini := TIniFile.Create(IniPath);
  try
    Result := Ini.ReadString('Telemetry', 'AnonymousID', '');
    if Result = '' then
    begin
      // Premier lancement : génère un GUID aléatoire et le persiste.
      CreateGUID(ID);
      Result := GUIDToString(ID);
      Ini.WriteString('Telemetry', 'AnonymousID', Result);
    end;
  finally
    Ini.Free;
  end;
end;

constructor TTelemetry.Create(const ServerURL, AppVersion: string);  
begin  
  inherited Create;
  FServerURL := ServerURL;
  FAppVersion := AppVersion;
  // ⚠ RGPD : la télémétrie doit être en OPT-IN explicite. On démarre
  //   DÉSACTIVÉE ; le code appelant doit activer `Enabled := True`
  //   APRÈS avoir obtenu le consentement utilisateur (voir le dialog
  //   de consentement plus bas dans ce chapitre).
  FEnabled := False;
  FHttpClient := THTTPClient.Create;

  // Générer (ou recharger) un ID utilisateur anonyme stable.
  // ⚠ NE PAS hasher `TOSVersion.Name + Architecture` — tous les utilisateurs
  //   d'un même OS auraient le MÊME ID (sans rapport avec un identifiant
  //   utilisateur unique). Utiliser un GUID généré une fois et persisté
  //   dans le dossier utilisateur (cf `GetOrCreateAnonymousID` ci-dessus)
  //   pour rester stable entre lancements ET unique par installation.
  FUserID := GetOrCreateAnonymousID;
end;

destructor TTelemetry.Destroy;  
begin  
  FHttpClient.Free;
  inherited;
end;

function TTelemetry.GetSystemInfo: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('os', TOSVersion.ToString);
  Result.AddPair('os_version', TOSVersion.Major.ToString + '.' + TOSVersion.Minor.ToString);
  Result.AddPair('architecture', TOSVersion.Architecture.ToString);
  Result.AddPair('app_version', FAppVersion);
  Result.AddPair('user_id', FUserID);
  // ⚠ Pour la télémétrie, TOUJOURS en UTC avec suffixe « Z » (ISO 8601).
  //   `Now` retourne l'heure locale — agrégeant des données de plusieurs
  //   fuseaux horaires, on se retrouve sinon avec des « pics d'erreurs
  //   à 14h » qui sont en réalité différents instants UTC.
  Result.AddPair('timestamp',
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                   TTimeZone.Local.ToUniversalTime(Now)));
end;

procedure TTelemetry.SendData(const EventData: TJSONObject);  
var  
  JsonString: string;
begin
  if not FEnabled then Exit;

  // ⚠ Capturer la string dans une variable locale AVANT de la passer au
  //   thread, pour ne pas dépendre de EventData (qui sera libéré par
  //   l'appelant après le retour de cette procédure).
  try
    JsonString := EventData.ToString;
  except
    Exit;
  end;

  // Envoi asynchrone pour ne pas bloquer l'application
  TThread.CreateAnonymousThread(
    procedure
    var
      Stream: TStringStream;
    begin
      try
        // ⚠ `TStringStream.Create(JsonString)` doit être LIBÉRÉ
        //   explicitement (try/finally) — sinon fuite mémoire à chaque
        //   envoi (et il y en a beaucoup en télémétrie).
        // ⚠ `FHttpClient` est partagé entre le thread principal et ce
        //   thread anonyme. THTTPClient n'est PAS thread-safe pour les
        //   opérations concurrentes. En production, créer une instance
        //   par thread, OU sérialiser les envois via une queue + un
        //   seul thread d'envoi dédié.
        Stream := TStringStream.Create(JsonString, TEncoding.UTF8);
        try
          FHttpClient.Post(FServerURL, Stream, nil);
        finally
          Stream.Free;
        end;
      except
        // Ignorer les erreurs d'envoi de télémétrie
        // pour ne pas perturber l'application.
      end;
    end
  ).Start;
end;

procedure TTelemetry.TrackEvent(EventType: TTelemetryEventType;
  const EventName: string; const Data: TJSONObject);
var
  EventData: TJSONObject;
  EventTypeName: string;
begin
  EventData := GetSystemInfo;
  try
    case EventType of
      teAppStart:    EventTypeName := 'app_start';
      teAppClose:    EventTypeName := 'app_close';
      teFeatureUsed: EventTypeName := 'feature_used';
      teError:       EventTypeName := 'error';
      teCrash:       EventTypeName := 'crash';
    end;

    EventData.AddPair('event_type', EventTypeName);
    EventData.AddPair('event_name', EventName);

    // ⚠ AddPair adopte le TJSONObject : `Data` devient propriété d'EventData
    //   et sera libéré lors du Free d'EventData ci-dessous. L'appelant NE
    //   doit PAS libérer `Data` après avoir appelé TrackEvent.
    if Assigned(Data) then
      EventData.AddPair('data', Data);

    SendData(EventData);
  finally
    // ✓ SendData sérialise EventData.ToString avant l'envoi async, donc
    //   on peut libérer EventData ici sans risquer de casser le thread.
    EventData.Free;
  end;
end;

procedure TTelemetry.TrackError(const ErrorMessage, StackTrace: string);  
var  
  ErrorData: TJSONObject;
begin
  ErrorData := TJSONObject.Create;
  // ⚠ Pas de Free ici : ErrorData est adopté par TrackEvent (cf note
  //   plus haut). Si TrackEvent levait une exception AVANT l'AddPair,
  //   il faudrait un try/except pour libérer ErrorData — mais c'est
  //   très improbable ici.
  ErrorData.AddPair('message', ErrorMessage);
  ErrorData.AddPair('stack_trace', StackTrace);

  TrackEvent(teError, 'error_occurred', ErrorData);
end;

procedure TTelemetry.TrackCrash(E: Exception);  
var  
  CrashData: TJSONObject;
begin
  CrashData := TJSONObject.Create;
  CrashData.AddPair('exception_class', E.ClassName);
  CrashData.AddPair('exception_message', E.Message);
  // ⚠ `E.StackTrace` retourne une chaîne VIDE par défaut dans Delphi.
  //   La RTL Delphi n'inclut pas de mécanisme natif pour capturer les
  //   stack traces. Pour avoir une vraie stack trace utilisable, vous
  //   DEVEZ installer l'un de ces outils :
  //   - madExcept (commercial, le plus puissant) — affecte
  //     automatiquement `Exception.StackTrace` après initialisation.
  //   - EurekaLog (commercial) — idem.
  //   - JclDebug (gratuit, JEDI) — appel manuel via
  //     `JclLastExceptStackList`.
  //   Sans l'un de ces outils, `stack_trace` sera vide.
  CrashData.AddPair('stack_trace', E.StackTrace);

  TrackEvent(teCrash, 'application_crash', CrashData);
end;

end.
```

### Étape 2 : Initialiser la télémétrie

Dans votre projet principal (.dpr) :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  System.SysUtils,
  Telemetry in 'Telemetry.pas',
  MainForm in 'MainForm.pas' {FormMain};

{$R *.res}

begin
  // Initialiser la télémétrie (créée DÉSACTIVÉE pour respecter le RGPD ;
  // le FormCreate du MainForm activera Telemetry.Enabled après recueil
  // du consentement utilisateur).
  Telemetry := TTelemetry.Create('https://telemetry.monapp.com/api/events', '1.0.0');
  try
    try
      // Démarrage de l'application — sans effet tant que le consentement
      // n'a pas été donné (Enabled = False par défaut).
      Telemetry.TrackEvent(teAppStart, 'application_started');

      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TFormMain, FormMain);
      Application.Run;

      // Fermeture normale
      Telemetry.TrackEvent(teAppClose, 'application_closed');
    except
      on E: Exception do
      begin
        // Crash de l'application : envoyer le rapport puis remonter.
        Telemetry.TrackCrash(E);
        raise;
      end;
    end;
  finally
    // ✓ try/finally pour libérer Telemetry même si une exception remonte.
    Telemetry.Free;
  end;
end.
```

### Étape 3 : Tracker les événements importants

Dans vos formulaires :

```pascal
procedure TMainForm.ButtonExportClick(Sender: TObject);  
begin  
  // Tracker l'utilisation de la fonctionnalité
  Telemetry.TrackEvent(teFeatureUsed, 'export_pdf');

  try
    ExportToPDF;
  except
    on E: Exception do
    begin
      Telemetry.TrackError(E.Message, E.StackTrace);
      ShowMessage('Erreur lors de l''export : ' + E.Message);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);  
var  
  SessionData: TJSONObject;
begin
  // Tracker l'ouverture du formulaire principal
  SessionData := TJSONObject.Create;
  SessionData.AddPair('screen_resolution',
    IntToStr(Screen.Width) + 'x' + IntToStr(Screen.Height));

  Telemetry.TrackEvent(teFeatureUsed, 'main_form_opened', SessionData);
end;
```

### Étape 4 : Gestionnaire d'exceptions global

Pour capturer toutes les exceptions non gérées :

```pascal
// ⚠ Une seule procédure FormCreate par formulaire ! Si vous avez déjà
//   ajouté du code dans FormCreate (cf Étape 3), AJOUTEZ la ligne
//   `Application.OnException := ApplicationException;` dans le FormCreate
//   existant — ne créez PAS une seconde procédure du même nom.
procedure TMainForm.FormCreate(Sender: TObject);  
begin  
  Application.OnException := ApplicationException;
  // ... reste du code FormCreate (consentement RGPD, tracking, etc.)
end;

procedure TMainForm.ApplicationException(Sender: TObject; E: Exception);  
begin  
  // Logger l'exception
  Telemetry.TrackError(E.Message, E.StackTrace);

  // Afficher à l'utilisateur
  ShowMessage('Une erreur est survenue : ' + E.Message);
end;
```

## Solutions de télémétrie professionnelles

Au lieu de construire votre propre système, utilisez des solutions existantes :

### 1. Sentry

**Sentry** est la solution la plus populaire pour le tracking d'erreurs.

**Avantages** :
- Gratuit jusqu'à 5000 événements/mois
- Excellent support multi-plateformes
- Interface web moderne
- Groupement intelligent des erreurs
- Alertes en temps réel

**Intégration avec Delphi** :

> ⚠️ **Pas de SDK Sentry officiel pour Delphi** en 2026 (le SDK officiel est en C/C++/JS/Python/Java/.NET, etc.). Trois options :  
> 1. **API REST Sentry « Envelope endpoint »** : POST JSON multipart sur l'endpoint régional `https://oXXXXXX.ingest.sentry.io/api/<project_id>/envelope/` avec en-tête `X-Sentry-Auth`. C'est l'API moderne (l'ancien endpoint `/store/` reste accepté mais est en mode legacy). Documenté sur https://develop.sentry.dev/sdk/envelopes/.  
> 2. **Wrapper communautaire** : chercher `delphi-sentry` ou similaire sur GitHub (qualité variable, vérifier maintenance et compatibilité Delphi 13).  
> 3. **Bridge via `sentry-native` (C/C++)** : charger la DLL native Sentry et exposer ses fonctions via FFI Delphi. Plus complexe mais robuste, et bénéficie automatiquement des mises à jour du SDK officiel.

```pascal
// Pseudocode d'usage si vous écrivez ou utilisez un wrapper Sentry :
uses
  MyDelphiSentryWrapper;  // Votre wrapper interne, pas une unité officielle.

procedure InitializeSentry;  
begin  
  SentryInit('https://your-key@sentry.io/project-id');
  SentrySetEnvironment('production');
  SentrySetRelease('MonApp@1.0.0');
end;

procedure TrackException(E: Exception);  
begin  
  SentryCaptureException(E);
end;
```

**Site** : https://sentry.io/

### 2. Raygun

**Raygun** se spécialise dans le crash reporting et le monitoring.

**Avantages** :
- Interface très claire
- Support Delphi via API REST
- Real User Monitoring (RUM)
- Déploiement tracking

**Intégration** :

```pascal
procedure SendToRaygun(E: Exception);  
var  
  HttpClient: THTTPClient;
  JsonPayload: TJSONObject;
  Stream: TStringStream;
begin
  JsonPayload := TJSONObject.Create;
  HttpClient := THTTPClient.Create;
  Stream := nil;
  try
    JsonPayload.AddPair('occurredOn',
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                     TTimeZone.Local.ToUniversalTime(Now)));
    // Détails de l'erreur
    // ... (construction du JSON selon API Raygun)

    HttpClient.CustomHeaders['X-ApiKey'] := 'YOUR_API_KEY';
    HttpClient.ContentType := 'application/json';
    // ⚠ Le TStringStream créé doit être LIBÉRÉ explicitement (try/finally),
    //   sinon fuite mémoire à chaque crash signalé.
    Stream := TStringStream.Create(JsonPayload.ToString, TEncoding.UTF8);
    HttpClient.Post('https://api.raygun.io/entries', Stream, nil);
  finally
    Stream.Free;
    HttpClient.Free;
    JsonPayload.Free;
  end;
end;
```

**Site** : https://raygun.com/

### 3. Application Insights (Microsoft)

**Application Insights** fait partie d'Azure Monitor.

**Avantages** :
- Intégration Azure parfaite
- Télémétrie complète (performance + erreurs)
- Tableaux de bord puissants
- Gratuit jusqu'à 5 GB/mois

**Intégration** :

```pascal
uses
  System.Net.HttpClient, System.JSON;

procedure SendToAppInsights(const EventName: string; Data: TJSONObject);  
var  
  HttpClient: THTTPClient;
  Payload: TJSONObject;
  Stream: TStringStream;
begin
  Payload := TJSONObject.Create;
  HttpClient := THTTPClient.Create;
  Stream := nil;
  try
    Payload.AddPair('name', 'Microsoft.ApplicationInsights.Event');
    Payload.AddPair('time',
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                     TTimeZone.Local.ToUniversalTime(Now)));
    // ... Configuration selon l'API Application Insights

    HttpClient.ContentType := 'application/json';
    // ⚠ Le TStringStream doit être LIBÉRÉ explicitement (try/finally).
    Stream := TStringStream.Create(Payload.ToString, TEncoding.UTF8);
    // ⚠ `dc.services.visualstudio.com` est l'endpoint LEGACY (depuis 2014).
    //   Depuis 2022-2023, Microsoft recommande l'endpoint RÉGIONAL extrait
    //   de la « Connection String » :
    //     IngestionEndpoint=https://westeurope-5.in.applicationinsights.azure.com/
    //   Récupérez-le dans le portail Azure → Application Insights → Vue
    //   d'ensemble → Connection String. L'ancien endpoint fonctionne encore
    //   mais sera dépriécié à terme.
    HttpClient.Post(
      'https://westeurope-5.in.applicationinsights.azure.com/v2.1/track',
      Stream,
      nil
    );
  finally
    Stream.Free;
    HttpClient.Free;
    Payload.Free;
  end;
end;
```

**Site** : https://azure.microsoft.com/services/monitor/

### 4. Google Analytics 4 (GA4) — pour applications desktop

> 🚨 **Universal Analytics (UA) déprécié** : depuis le 1ᵉʳ juillet 2023, Google a **arrêté** la collecte de données via Universal Analytics (`tid=UA-XXXXX-Y`, endpoint `/collect`). Les propriétés UA ont été définitivement supprimées en juillet 2024. **Toute intégration GA basée sur `UA-XXXXX-Y` ne fonctionne plus en 2026** — il faut migrer vers **GA4** (Measurement Protocol GA4) avec un Measurement ID `G-XXXXXXXXXX` et un *API secret*.

Bien que conçu pour le web, GA4 peut tracker les applications desktop via le **Measurement Protocol GA4**.

**Avantages** :
- Gratuit (jusqu'à 10 M d'événements/mois sur la version standard).
- Très répandu.
- Excellent pour les métriques d'utilisation.

**Inconvénients** :
- Moins adapté aux crashes (préférer Sentry pour le crash reporting).
- Configuration plus complexe que GA UA (Measurement ID + API Secret).
- Privacy : l'envoi de données à Google nécessite un consentement RGPD explicite et une politique de confidentialité claire.

**Intégration GA4 (2026)** :

```pascal
// ⚠ Cette procédure prend `ClientID` en paramètre — si vous l'écrivez
//   comme méthode de TTelemetry, vous pourrez accéder à FUserID directement.
//   Comme fonction libre (illustrée ici), il faut passer le client ID.
procedure TrackGA4Event(const ClientID, EventName: string);  
var  
  HttpClient: THTTPClient;
  Url: string;
  PayloadStr: string;
  Stream: TStringStream;
begin
  // ⚠ GA4 utilise un Measurement ID (G-XXXXXXXXXX) + un API Secret
  //   créé dans Admin → Data Streams → Measurement Protocol API secrets.
  //   Le `client_id` doit être un identifiant stable (GUID, cf
  //   `GetOrCreateAnonymousID` plus haut), PAS l'email de l'utilisateur.
  Url := 'https://www.google-analytics.com/mp/collect' +
         '?measurement_id=G-XXXXXXXXXX' +
         '&api_secret=VOTRE_API_SECRET';

  // Body JSON GA4 (différent du format key=value de UA).
  // ⚠ Échapper les caractères spéciaux JSON si EventName/ClientID viennent
  //   de l'utilisateur (utiliser TJSONObject.ToString plutôt que Format
  //   en production).
  PayloadStr := Format(
    '{"client_id":"%s","events":[{"name":"%s","params":{}}]}',
    [ClientID, EventName]);

  HttpClient := THTTPClient.Create;
  try
    Stream := TStringStream.Create(PayloadStr, TEncoding.UTF8);
    try
      HttpClient.ContentType := 'application/json';
      HttpClient.Post(Url, Stream, nil);
    finally
      Stream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

> ⚠️ **Politique RGPD obligatoire** : utiliser GA4 implique l'envoi de données vers Google (États-Unis). Conformité RGPD : consentement explicite + politique de confidentialité mentionnant Google Analytics + accord DPF (Data Privacy Framework, juillet 2023). Pour les apps européennes très soucieuses de la vie privée, préférer des alternatives auto-hébergées comme **Matomo** ou **PostHog** (cf chapitre 16.8).

### Tableau comparatif

| Solution | Coût gratuit | Focus | Support Delphi | Complexité |
|----------|--------------|-------|----------------|------------|
| **Sentry** | 5K événements/mois | Erreurs | API REST | Moyenne |
| **Raygun** | 14 jours trial | Crashes | API REST | Facile |
| **Application Insights** | 5 GB/mois | Performance + Erreurs | API REST | Moyenne |
| **Google Analytics** | Illimité | Utilisation | API HTTP | Facile |
| **Solution maison** | Gratuit (infra) | Sur mesure | Native | Difficile |

## Respect de la vie privée et RGPD

### Obligations légales

Si vous collectez des données sur des utilisateurs européens (ou simplement résidant en UE/EEE/UK), vous devez respecter le **RGPD** (Règlement Général sur la Protection des Données).

**Principes clés** :

1. **Consentement RGPD-conforme** : l'opt-in doit être **libre** (refus possible sans perte de fonctionnalité), **éclairé** (information claire AVANT le clic), **spécifique** (par finalité — télémétrie d'usage ≠ télémétrie de crash) et **univoque** (case **non** pré-cochée par défaut). Sauf si la collecte est **strictement nécessaire au fonctionnement** (par ex. télémétrie de sécurité critique), auquel cas le consentement n'est pas requis mais l'information reste obligatoire.
2. **Transparence** : expliquez clairement quelles données sont collectées, pour quelles finalités, qui y a accès et pour combien de temps elles sont conservées.
3. **Minimisation** : collectez **uniquement** ce qui est strictement nécessaire à la finalité déclarée. Pas de « on collecte tout au cas où ».
4. **Sécurité** : chiffrement en transit (HTTPS obligatoire) et au repos, contrôle d'accès strict.
5. **Droit à l'oubli** : permettez la suppression des données sur demande de l'utilisateur, dans un délai raisonnable (1 mois max).
6. **Droit à la portabilité** : permettez l'export des données collectées dans un format standard (JSON, CSV).

> ⚠️ **Sanctions RGPD** : jusqu'à **20 millions d'euros ou 4 % du chiffre d'affaires mondial** (le plus élevé des deux) en cas de manquement caractérisé. Sans aller jusqu'aux sanctions, la CNIL en France émet régulièrement des mises en demeure publiques qui nuisent à la réputation.

### Implémentation du consentement

```pascal
uses
  System.SysUtils, System.IOUtils, System.IniFiles, Vcl.Dialogs;

function TMainForm.GetPrivacyIniPath: string;  
begin  
  // ⚠ NE PAS écrire à côté de l'EXE — Program Files est lecture seule
  //   pour un utilisateur standard ; UAC virtualiserait silencieusement
  //   l'écriture vers VirtualStore et la valeur ne serait pas relue.
  Result := TPath.Combine(TPath.GetHomePath, 'MonApp\privacy.ini');
  ForceDirectories(ExtractFilePath(Result));
end;

// ⚠ Rappel : une SEULE procédure `FormCreate` par formulaire. Le bloc
//   ci-dessous montre la logique de consentement RGPD ; **intégrez-la
//   dans votre FormCreate existant**, à la suite des autres initialisations
//   (cf Étape 3 et Étape 4 plus haut).
procedure TMainForm.FormCreate(Sender: TObject);  
var  
  IniFile: TIniFile;
  TelemetryConsent: Boolean;
begin
  IniFile := TIniFile.Create(GetPrivacyIniPath);
  try
    // Vérifier si l'utilisateur a déjà donné son consentement
    if not IniFile.ValueExists('Privacy', 'TelemetryConsent') then
    begin
      // Première utilisation : demander le consentement
      if ShowConsentDialog then
      begin
        IniFile.WriteBool('Privacy', 'TelemetryConsent', True);
        Telemetry.Enabled := True;
      end
      else
      begin
        IniFile.WriteBool('Privacy', 'TelemetryConsent', False);
        Telemetry.Enabled := False;
      end;
    end
    else
    begin
      // Utiliser la préférence sauvegardée
      TelemetryConsent := IniFile.ReadBool('Privacy', 'TelemetryConsent', False);
      Telemetry.Enabled := TelemetryConsent;
    end;
  finally
    IniFile.Free;
  end;
end;

function TMainForm.ShowConsentDialog: Boolean;  
begin  
  Result := MessageDlg(
    'Pour améliorer notre application, nous aimerions collecter des données ' +
    'd''utilisation anonymes (fonctionnalités utilisées, erreurs rencontrées). ' +
    'Aucune donnée personnelle n''est collectée. ' +
    sLineBreak + sLineBreak +
    'Acceptez-vous de partager ces informations ?',
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes;
end;
```

### Données à anonymiser

**Ne collectez JAMAIS** :
- ❌ Noms réels des utilisateurs
- ❌ Adresses email
- ❌ Numéros de téléphone
- ❌ Adresses IP complètes
- ❌ Données de localisation précises
- ❌ Données sensibles (médicales, financières)

**Utilisez** :
- ✅ ID anonymes (GUID v4 généré aléatoirement à la première exécution puis persisté ; cf. helper `GetOrCreateAnonymousID` plus haut). **À éviter** : un hash du nom de machine — dans un parc d'entreprise les noms type `PC-JDUPONT-001` sont identifiants.
- ✅ Pays/Région (pas de ville précise — la géolocalisation à la ville commence à être considérée comme donnée personnelle en jurisprudence européenne).
- ✅ Versions de logiciels (OS, app).
- ✅ Métriques techniques agrégées (temps moyen, comptage).

### Option de désactivation

Permettez toujours aux utilisateurs de désactiver la télémétrie :

```pascal
procedure TFormOptions.CheckBoxTelemetryClick(Sender: TObject);  
var  
  IniFile: TIniFile;
begin
  // Réutilise GetPrivacyIniPath (cf section précédente) pour cibler
  // AppData et non Program Files.
  IniFile := TIniFile.Create(MainForm.GetPrivacyIniPath);
  try
    IniFile.WriteBool('Privacy', 'TelemetryConsent', CheckBoxTelemetry.Checked);
    Telemetry.Enabled := CheckBoxTelemetry.Checked;

    if CheckBoxTelemetry.Checked then
      ShowMessage('Télémétrie activée. Merci de nous aider à améliorer l''application !')
    else
      ShowMessage('Télémétrie désactivée. Aucune donnée ne sera envoyée.');
  finally
    IniFile.Free;
  end;
end;
```

## Analyse et utilisation des données

### Dashboard de télémétrie

Créez un tableau de bord pour visualiser les données :

**Métriques clés à suivre** :

1. **Santé de l'application**
   - Taux de crash (crashes / sessions)
   - Erreurs les plus fréquentes
   - Versions affectées

2. **Utilisation**
   - Utilisateurs actifs quotidiens (DAU)
   - Utilisateurs actifs mensuels (MAU)
   - Durée moyenne des sessions
   - Fonctionnalités les plus utilisées

3. **Performance**
   - Temps de démarrage
   - Temps de réponse moyen
   - Utilisation mémoire

4. **Adoption**
   - Nouvelles installations
   - Mises à jour effectuées
   - Taux de rétention

### Exemple de rapport hebdomadaire

```
=== Rapport Télémétrie - Semaine 3 ===

📊 Utilisation
- Utilisateurs actifs : 1,245 (+12%)
- Sessions totales : 8,934
- Durée moyenne : 23 minutes

⚠️ Erreurs
- Total erreurs : 47 (-35%)
- Crashes : 3 (-50%)
- Top erreur : "Database connection timeout" (15 occurrences)

🚀 Performance
- Temps de démarrage : 2.3s (stable)
- Mémoire moyenne : 156 MB (-8 MB)

💡 Insights
- La fonctionnalité "Export Excel" est utilisée par 67% des utilisateurs
- Windows 11 représente maintenant 78% des utilisateurs
- Bug #234 corrigé : 0 occurrence cette semaine !
```

### Actions basées sur les données

**Exemple de décisions guidées par la télémétrie** :

1. **Crash fréquent détecté**
   → Priorité haute pour correction  
   → Patch d'urgence si critique

2. **Fonctionnalité jamais utilisée**
   → Améliorer visibilité  
   → Ou supprimer pour simplifier

3. **Performance dégradée sur certaines configs**
   → Optimisation ciblée  
   → Avertissement pour configs non supportées

4. **Adoption lente d'une nouvelle version**
   → Communication renforcée  
   → Vérifier problèmes de mise à jour

## Bonnes pratiques de télémétrie

### 1. Commencer simple

❌ **Mauvais** : Tracker 100 événements dès le départ

✅ **Bon** : Commencer avec 5-10 événements clés
```
- Démarrage/Fermeture application
- Crashes et erreurs
- 2-3 fonctionnalités importantes
```

Ajoutez progressivement selon les besoins.

### 2. Envoi asynchrone

La télémétrie ne doit **jamais** ralentir l'application :

```pascal
procedure TTelemetry.SendData(const EventData: TJSONObject);  
var  
  JsonString: string;
begin
  // ⚠ Capturer la string AVANT le thread : EventData peut être libéré
  //   par l'appelant avant que le thread ne s'exécute.
  JsonString := EventData.ToString;

  // ✓ Bon : Envoi dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    var
      Stream: TStringStream;
    begin
      try
        // ⚠ Le TStringStream doit être LIBÉRÉ explicitement.
        Stream := TStringStream.Create(JsonString, TEncoding.UTF8);
        try
          FHttpClient.Post(FServerURL, Stream, nil);
        finally
          Stream.Free;
        end;
      except
        // Ignorer les erreurs : la télémétrie ne doit jamais
        // perturber l'application.
      end;
    end
  ).Start;
end;
```

### 3. Gestion des erreurs silencieuse

Les erreurs de télémétrie ne doivent **jamais** affecter l'utilisateur :

```pascal
try
  Telemetry.TrackEvent(teFeatureUsed, 'export_pdf');
except
  // Ignorer silencieusement
  // Ne jamais afficher d'erreur à l'utilisateur
end;
```

### 4. Limiter la taille des données

Envoyez uniquement ce qui est nécessaire :

```pascal
// ✗ Mauvais : Trop de détails
EventData.AddPair('full_document_content', Memo1.Text); // Peut être énorme !

// ✓ Bon : Métriques utiles
EventData.AddPair('document_length', IntToStr(Length(Memo1.Text)));  
EventData.AddPair('document_type', DetectDocumentType(Memo1.Text));  
```

### 5. Sampling pour les événements fréquents

Pour les événements très fréquents, utilisez le sampling :

```pascal
uses System.SysUtils;

// Helper : usage mémoire de l'application en mégaoctets.
// ⚠ Utilise FastMM (gestionnaire mémoire par défaut de Delphi). Pour
//   d'autres allocateurs, adapter ou utiliser GetProcessMemoryInfo
//   (Win32 PSAPI) qui donne la mémoire totale du processus.
function GetMemoryUsedMB: Cardinal;  
var  
  Status: TMemoryManagerState;
  Total, Block: NativeUInt;
  i: Integer;
begin
  GetMemoryManagerState(Status);
  Total := Status.TotalAllocatedMediumBlockSize +
           Status.TotalAllocatedLargeBlockSize;
  for i := 0 to High(Status.SmallBlockTypeStates) do
    Total := Total + Status.SmallBlockTypeStates[i].UseableBlockSize *
                     Status.SmallBlockTypeStates[i].AllocatedBlockCount;
  Result := Total div (1024 * 1024);
end;

procedure TMainForm.TimerPerformanceTimer(Sender: TObject);  
begin  
  // Envoyer les métriques de performance seulement 1 fois sur 100.
  // ⚠ `Randomize` doit avoir été appelé une fois au démarrage de l'app
  //   (sinon `Random` produit toujours la MÊME séquence — donc les
  //   sampling de plusieurs utilisateurs seraient corrélés).
  if Random(100) = 0 then
    Telemetry.TrackEvent(teFeatureUsed, 'memory_usage',
      TJSONObject.Create.AddPair('memory_mb',
                                 TJSONNumber.Create(GetMemoryUsedMB)));
end;
```

### 6. Contexte riche pour les erreurs

Quand une erreur survient, capturez le contexte :

```pascal
// ⚠ Variante ENRICHIE de TrackError (par rapport à la signature de base
//   définie plus haut avec (ErrorMessage, StackTrace)). On choisit ici de
//   collecter le contexte directement dans la méthode plutôt que de le
//   passer en paramètre — adaptez selon votre besoin.
procedure TTelemetry.TrackErrorWithContext(const ErrorMessage, StackTrace: string);  
var  
  ErrorData: TJSONObject;
  ActiveForm: TForm;
  ActiveCtrl: TWinControl;
begin
  ErrorData := TJSONObject.Create;
  ErrorData.AddPair('message', ErrorMessage);
  ErrorData.AddPair('stack_trace', StackTrace);

  // ⚠ Screen.ActiveForm et Screen.ActiveControl peuvent être nil
  //   (ex : crash au démarrage, avant la première fenêtre). Toujours
  //   vérifier avant de déréférencer.
  ActiveForm := Screen.ActiveForm;
  if Assigned(ActiveForm) then
    ErrorData.AddPair('active_form', ActiveForm.ClassName);

  ActiveCtrl := Screen.ActiveControl;
  if Assigned(ActiveCtrl) then
    ErrorData.AddPair('focused_control', ActiveCtrl.Name);

  ErrorData.AddPair('last_action', FLastUserAction);
  ErrorData.AddPair('session_duration', IntToStr(GetSessionDuration));

  TrackEvent(teError, 'error_occurred', ErrorData);
end;
```

Plus vous avez de contexte, plus facile sera le débogage.

### 7. Versioning des événements

Quand vous changez la structure des événements, versionnez :

```pascal
EventData.AddPair('telemetry_version', '2.0');
```

Cela permet de gérer l'évolution du schéma de données.

### 8. Mode offline

Gérez le cas où l'utilisateur n'a pas Internet :

```pascal
type
  TTelemetry = class
  private
    FOfflineQueue: TList<string>;  // ⚠ Stocke des strings JSON,
                                   //   pas des TJSONObject (durée de vie
                                   //   incertaine, propriétaire ambigu).
    procedure SendQueuedEvents;
  public
    procedure SendData(const EventData: TJSONObject);
  end;

procedure TTelemetry.SendData(const EventData: TJSONObject);  
var  
  JsonString: string;
begin
  // Sérialiser AVANT d'entrer dans le thread anonyme.
  JsonString := EventData.ToString;

  TThread.CreateAnonymousThread(
    procedure
    var
      Stream: TStringStream;
    begin
      try
        // ⚠ TStringStream libéré explicitement.
        Stream := TStringStream.Create(JsonString, TEncoding.UTF8);
        try
          FHttpClient.Post(FServerURL, Stream, nil);
        finally
          Stream.Free;
        end;

        // Si succès et qu'il y a des événements en attente
        if FOfflineQueue.Count > 0 then
          SendQueuedEvents;
      except
        // Échec : mettre en file d'attente (la string JSON, pas le
        // TJSONObject — l'appelant garde la propriété de ce dernier).
        TMonitor.Enter(FOfflineQueue);
        try
          FOfflineQueue.Add(JsonString);
        finally
          TMonitor.Exit(FOfflineQueue);
        end;
      end;
    end
  ).Start;
end;
```

### 9. Politique de rétention

Définissez combien de temps garder les données :

```
- Données brutes : 30 jours
- Données agrégées : 1 an
- Métriques clés : Indéfiniment
```

Cela réduit les coûts de stockage et respecte la vie privée.

### 10. Tests et validation

Testez votre télémétrie avant de déployer. `FServerURL` étant privé, on choisit  
l'URL à la construction (et pas par affectation depuis l'extérieur de l'unité) :  

```pascal
const
{$IFDEF DEBUG}
  // En développement : serveur local pour ne pas polluer la prod.
  TELEMETRY_URL = 'http://localhost:5000/telemetry';
{$ELSE}
  // En production : serveur réel (HTTPS impératif — cf section 17.5).
  TELEMETRY_URL = 'https://telemetry.monapp.com/api/events';
{$ENDIF}

Telemetry := TTelemetry.Create(TELEMETRY_URL, '1.0.0');
```

## Stack traces détaillées

Pour obtenir des stack traces utiles, utilisez des outils comme :

### 1. madExcept

**madExcept** est un outil commercial très puissant.

**Avantages** :
- Stack traces détaillées avec noms de fichiers et numéros de ligne
- Dialog d'erreur personnalisable
- Envoi automatique des rapports
- Support email, HTTP, FTP

**Intégration** : Simple, via l'IDE Delphi

**Site** : http://www.madshi.net/

### 2. JclDebug (JEDI Code Library)

**JclDebug** est open source et gratuit.

**Avantages** :
- Gratuit
- Stack traces avec numéros de ligne
- Léger

**Inconvénients** :
- Configuration plus complexe
- Moins de fonctionnalités que madExcept

**Intégration** :

```pascal
uses
  JclDebug;

procedure ShowExceptionDialog(E: Exception);  
var  
  StackInfo: TJclStackInfoList;
  i: Integer;
  Msg: string;
begin
  Msg := E.Message + sLineBreak + sLineBreak + 'Stack Trace:' + sLineBreak;

  StackInfo := JclLastExceptStackList();
  if Assigned(StackInfo) then
  begin
    for i := 0 to StackInfo.Count - 1 do
      Msg := Msg + StackInfo.Items[i].GetLocationInfoStr + sLineBreak;
  end;

  ShowMessage(Msg);
end;
```

### 3. EurekaLog

**EurekaLog** est un autre outil commercial populaire.

**Avantages** :
- Interface riche
- Dialog d'exception moderne
- Support multi-plateformes
- Bug tracker intégré

**Site** : https://www.eurekalog.com/

## Serveur de télémétrie simple

Si vous voulez héberger votre propre serveur, voici un exemple **minimal de prototype** avec Node.js :

```javascript
// server.js - Serveur de télémétrie minimal (PROTOTYPE, NON-PROD).
const express = require('express');  
const fs = require('fs');  
const app = express();  

app.use(express.json({ limit: '64kb' }));  // Limite payload : anti-DoS.

// ⚠ Authentification minimale : exiger un en-tête X-API-Key.
//   En prod : utiliser HMAC ou JWT, jamais une clé statique en clair.
const API_KEY = process.env.TELEMETRY_API_KEY;  
app.use('/api/events', (req, res, next) => {  
  if (!API_KEY || req.headers['x-api-key'] !== API_KEY)
    return res.status(401).send('Unauthorized');
  next();
});

// Endpoint pour recevoir les événements
app.post('/api/events', (req, res) => {
  const event = req.body;
  const logEntry = JSON.stringify(event) + '\n';

  // ⚠ `appendFileSync` bloque l'event loop. Pour la prod, utiliser
  //   `fs.appendFile` (async) ou un vrai logger (pino, winston) avec
  //   buffer/batch, OU mieux : pousser dans une queue (Redis, NATS,
  //   Kafka) et traiter en aval.
  fs.appendFileSync('telemetry.log', logEntry);

  if (event.event_type === 'crash') {
    console.error('CRASH DETECTED:', event);
    // Envoyer une alerte (email, Slack, etc.).
  }
  res.status(200).send('OK');
});

// Endpoint pour visualiser les stats (basique)
app.get('/dashboard', (req, res) => {
  // ⚠ `readFileSync` charge TOUT le fichier en RAM à chaque requête.
  //   Au-delà de quelques milliers de lignes, ça devient inutilisable.
  //   En prod : indexer dans PostgreSQL/ClickHouse, ou utiliser
  //   Grafana + Loki/Elasticsearch pour la visualisation.
  const logs = fs.readFileSync('telemetry.log', 'utf8');
  const events = logs.split('\n')
    .filter(line => line.trim())
    .map(line => JSON.parse(line));

  res.json({
    total_events: events.length,
    crashes: events.filter(e => e.event_type === 'crash').length,
    errors:   events.filter(e => e.event_type === 'error').length,
    active_users: new Set(events.map(e => e.user_id)).size
  });
});

app.listen(5000, () => {
  console.log('Serveur de télémétrie (prototype) démarré sur le port 5000');
});
```

**Démarrage** :
```bash
npm install express  
export TELEMETRY_API_KEY="cle-secrete-aleatoire"  
node server.js  
```

> 🚨 **À ne PAS utiliser tel quel en production** : ce script est un point de départ pédagogique. Pour une vraie installation :  
> - **HTTPS obligatoire** (terminate TLS via Nginx/Caddy/Cloudflare devant Node).  
> - **Authentification robuste** (HMAC avec rotation, JWT, OAuth — pas une simple clé statique).  
> - **Rate limiting** par IP / par client_id (express-rate-limit).  
> - **Stockage scalable** (PostgreSQL avec table partitionnée, ClickHouse pour analytics, ou service managé).  
> - **Validation stricte** du JSON entrant (`ajv`, `zod`).  
> - **Sauvegardes** régulières.

Votre application Delphi peut maintenant envoyer des événements à `https://telemetry.monapp.com/api/events` avec l'en-tête `X-API-Key`.

## Problèmes courants et solutions

### L'envoi de télémétrie ralentit l'application

**Cause** : Envoi synchrone

**Solution** : Toujours envoyer dans un thread séparé (voir exemples ci-dessus)

### Trop de données collectées

**Cause** : Tracking trop d'événements

**Solution** :
- Limiter aux événements essentiels
- Utiliser le sampling pour les événements fréquents
- Agréger localement avant envoi

### Utilisateurs bloquent la télémétrie

**Cause** : Pare-feu, antivirus, pas de consentement

**Solution** :
- Gérer gracieusement l'échec (mode offline)
- Ne jamais afficher d'erreur à l'utilisateur
- Respecter le choix de l'utilisateur

### Difficile d'analyser les données

**Cause** : Pas d'outil de visualisation

**Solution** :
- Utiliser une solution existante (Sentry, Raygun)
- Ou créer un dashboard simple (Grafana, Excel)
- Automatiser les rapports hebdomadaires

### Stack traces inutiles (juste des adresses mémoire)

**Cause** : pas de symboles de débogage côté binaire ou côté serveur de télémétrie.

**Solution** :
- Activer le **fichier MAP détaillé** dans `Projet → Options → Édition de liens` (cf section 17.1). Ce fichier ne grossit PAS l'EXE distribué — c'est un fichier séparé à archiver.
- Installer **madExcept / EurekaLog** (commercial) OU **JclDebug** (gratuit) qui exploitent le MAP pour produire des stack traces avec noms de fonctions et numéros de ligne.
- **Archiver les fichiers `.map`** avec chaque build (par version). Sans le MAP correspondant à la version qui a crashé, vous ne pouvez pas symboliser l'adresse.
- **Convertir le `.map` en `.pdb`** avec [map2pdb d'Anders Melander](https://github.com/andersmelander/map2pdb) si vous voulez utiliser WinDbg, Visual Studio Profiler ou Sentry pour la symbolisation. (Le projet a migré de Bitbucket vers GitHub ; le Bitbucket d'origine n'est plus mis à jour.)

## Checklist d'implémentation

Avant de déployer la télémétrie :

- [ ] Système de télémétrie implémenté
- [ ] Événements clés identifiés et trackés
- [ ] Gestion des exceptions globale configurée
- [ ] Envoi asynchrone (thread séparé)
- [ ] Gestion des erreurs silencieuse
- [ ] Consentement utilisateur implémenté
- [ ] Option de désactivation disponible
- [ ] Politique de confidentialité rédigée
- [ ] Données anonymisées (pas d'infos personnelles)
- [ ] Stack traces détaillées (madExcept/JclDebug)
- [ ] Serveur de télémétrie prêt
- [ ] Dashboard de visualisation configuré
- [ ] Alertes pour événements critiques
- [ ] Tests en environnement dev/staging
- [ ] Documentation pour l'équipe

## Conclusion

La télémétrie et l'analyse de crash transforment votre relation avec vos utilisateurs. Au lieu de deviner ce qui se passe, vous **savez** :

- Quels bugs affectent le plus d'utilisateurs
- Quelles fonctionnalités sont réellement utilisées
- Comment les utilisateurs parcourent votre application
- Où les performances doivent être améliorées

**Points clés à retenir** :

1. **Télémétrie = Visibilité** : Voir ce qui se passe en production
2. **Crash reports = Débogage rapide** : Corriger avant que ça devienne critique
3. **Consentement obligatoire** : Respecter la vie privée (RGPD)
4. **Asynchrone et silencieux** : Ne jamais perturber l'utilisateur
5. **Contexte riche** : Plus d'infos = débogage plus facile
6. **Solutions existantes** : Sentry, Raygun, Application Insights
7. **Commencer simple** : 5-10 événements clés, puis évoluer
8. **Agir sur les données** : La télémétrie n'a de valeur que si vous l'utilisez

Avec la télémétrie bien implémentée, vous pouvez :
- **Détecter** les problèmes en quelques minutes
- **Corriger** avant que les utilisateurs ne se plaignent
- **Améliorer** continuellement votre application
- **Prioriser** les développements selon l'usage réel
- **Offrir** une meilleure expérience utilisateur

La télémétrie ferme la boucle du cycle de développement : vous créez, déployez, surveillez, apprenez, et améliorez. C'est l'étape finale qui transforme une bonne application en une **excellente** application.

---

**Félicitations !** Vous avez terminé le chapitre 17 sur la Distribution et le Déploiement. Vous maîtrisez maintenant tous les aspects nécessaires pour distribuer professionnellement vos applications Delphi, de la compilation Release jusqu'au monitoring en production. Votre application est prête à rencontrer le monde !

⏭️ [Architecture et bonnes pratiques](/18-architecture-et-bonnes-pratiques/README.md)
