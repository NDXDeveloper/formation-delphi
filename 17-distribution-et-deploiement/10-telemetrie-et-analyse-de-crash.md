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
Date: 2025-01-20 14:32:15  
Application: MonApp v1.2.0  
OS: Windows 11 22H2 (Build 22621)  
Exception: EAccessViolation  
Message: Access violation at address 00405C7A. Read of address 00000000.  

Stack Trace:
  MonApp.exe  TMainForm.Button1Click  (MainForm.pas:145)
  MonApp.exe  TControl.Click          (Vcl.Controls.pas:7265)
  MonApp.exe  TButton.Click           (Vcl.StdCtrls.pas:4523)
  MonApp.exe  TWinControl.WndProc     (Vcl.Controls.pas:10156)

Context:
  Current User: Jean.Dupont
  Last Action: Export to PDF
  Memory Usage: 245 MB / 8 GB
  Active Form: TMainForm
```

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
  Winapi.Windows, System.SysInfo;

constructor TTelemetry.Create(const ServerURL, AppVersion: string);  
begin  
  inherited Create;
  FServerURL := ServerURL;
  FAppVersion := AppVersion;
  FEnabled := True;
  FHttpClient := THTTPClient.Create;

  // Générer un ID utilisateur anonyme basé sur le nom de la machine
  FUserID := THashMD5.GetHashString(TOSVersion.Name + '-' + TOSVersion.Architecture.ToString);
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
  Result.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
end;

procedure TTelemetry.SendData(const EventData: TJSONObject);  
var  
  Response: IHTTPResponse;
  JsonString: string;
begin
  if not FEnabled then Exit;

  try
    JsonString := EventData.ToString;

    // Envoi asynchrone pour ne pas bloquer l'application
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          FHttpClient.Post(FServerURL, TStringStream.Create(JsonString), nil);
        except
          // Ignorer les erreurs d'envoi de télémétrie
          // pour ne pas perturber l'application
        end;
      end
    ).Start;
  except
    // Ignorer silencieusement les erreurs de télémétrie
  end;
end;

procedure TTelemetry.TrackEvent(EventType: TTelemetryEventType;
  const EventName: string; const Data: TJSONObject);
var
  EventData: TJSONObject;
  EventTypeName: string;
begin
  EventData := GetSystemInfo;

  case EventType of
    teAppStart: EventTypeName := 'app_start';
    teAppClose: EventTypeName := 'app_close';
    teFeatureUsed: EventTypeName := 'feature_used';
    teError: EventTypeName := 'error';
    teCrash: EventTypeName := 'crash';
  end;

  EventData.AddPair('event_type', EventTypeName);
  EventData.AddPair('event_name', EventName);

  if Assigned(Data) then
    EventData.AddPair('data', Data);

  SendData(EventData);
end;

procedure TTelemetry.TrackError(const ErrorMessage, StackTrace: string);  
var  
  ErrorData: TJSONObject;
begin
  ErrorData := TJSONObject.Create;
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
  // Initialiser la télémétrie
  Telemetry := TTelemetry.Create('https://telemetry.monapp.com/api/events', '1.0.0');

  try
    // Démarrage de l'application
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
      // Crash de l'application
      Telemetry.TrackCrash(E);
      raise;
    end;
  end;

  Telemetry.Free;
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
procedure TMainForm.FormCreate(Sender: TObject);  
begin  
  Application.OnException := ApplicationException;
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

```pascal
uses
  SentryClient; // Via bibliothèque tierce

procedure InitializeSentry;  
begin  
  SentryInit('https://your-key@sentry.io/project-id');
  SentrySetEnvironment('production');
  SentrySetRelease('1.0.0');
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
begin
  JsonPayload := TJSONObject.Create;
  JsonPayload.AddPair('occurredOn', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  // Détails de l'erreur
  // ... (construction du JSON selon API Raygun)

  HttpClient := THTTPClient.Create;
  try
    HttpClient.CustomHeaders['X-ApiKey'] := 'YOUR_API_KEY';
    HttpClient.Post('https://api.raygun.io/entries',
      TStringStream.Create(JsonPayload.ToString), nil);
  finally
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
begin
  Payload := TJSONObject.Create;
  Payload.AddPair('name', 'Microsoft.ApplicationInsights.Event');
  Payload.AddPair('time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', TTimeZone.Local.ToUniversalTime(Now)));
  // ... Configuration selon l'API Application Insights

  HttpClient := THTTPClient.Create;
  try
    HttpClient.Post(
      'https://dc.services.visualstudio.com/v2/track',
      TStringStream.Create(Payload.ToString),
      nil
    );
  finally
    HttpClient.Free;
    Payload.Free;
  end;
end;
```

**Site** : https://azure.microsoft.com/services/monitor/

### 4. Google Analytics (pour applications desktop)

Bien que conçu pour le web, GA peut tracker les applications desktop.

**Avantages** :
- Gratuit
- Très répandu
- Excellent pour les métriques d'utilisation

**Inconvénients** :
- Moins adapté aux crashes
- Nécessite adaptation

**Intégration** :

```pascal
procedure TrackGAEvent(const Category, Action, Label: string; Value: Integer = 0);  
var  
  HttpClient: THTTPClient;
  Params: string;
begin
  Params := Format(
    'v=1&tid=UA-XXXXX-Y&cid=%s&t=event&ec=%s&ea=%s&el=%s&ev=%d',
    [FUserID, Category, Action, Label, Value]
  );

  HttpClient := THTTPClient.Create;
  try
    HttpClient.Post('https://www.google-analytics.com/collect',
      TStringStream.Create(Params), nil);
  finally
    HttpClient.Free;
  end;
end;
```

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

Si vous collectez des données sur des utilisateurs européens, vous devez respecter le **RGPD** (Règlement Général sur la Protection des Données).

**Principes clés** :

1. **Consentement** : L'utilisateur doit accepter la télémétrie
2. **Transparence** : Expliquez quelles données sont collectées
3. **Minimisation** : Collectez uniquement ce qui est nécessaire
4. **Sécurité** : Protégez les données collectées
5. **Droit à l'oubli** : Permettez la suppression des données

### Implémentation du consentement

```pascal
procedure TMainForm.FormCreate(Sender: TObject);  
var  
  IniFile: TIniFile;
  TelemetryConsent: Boolean;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
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
- ✅ ID anonymes (hash du nom de machine)
- ✅ Pays/Région (pas de ville précise)
- ✅ Versions de logiciels
- ✅ Métriques techniques agrégées

### Option de désactivation

Permettez toujours aux utilisateurs de désactiver la télémétrie :

```pascal
procedure TFormOptions.CheckBoxTelemetryClick(Sender: TObject);  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
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
begin  
  // ✓ Bon : Envoi dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FHttpClient.Post(FServerURL, TStringStream.Create(EventData.ToString), nil);
      except
        // Ignorer les erreurs
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
procedure TMainForm.TimerPerformanceTimer(Sender: TObject);  
begin  
  // Envoyer les métriques de performance seulement 1 fois sur 100
  if Random(100) = 0 then
  begin
    Telemetry.TrackEvent(teFeatureUsed, 'memory_usage',
      TJSONObject.Create.AddPair('memory_mb', IntToStr(GetMemoryUsed)));
  end;
end;
```

### 6. Contexte riche pour les erreurs

Quand une erreur survient, capturez le contexte :

```pascal
procedure TTelemetry.TrackError(const ErrorMessage: string);  
var  
  ErrorData: TJSONObject;
begin
  ErrorData := TJSONObject.Create;
  ErrorData.AddPair('message', ErrorMessage);
  ErrorData.AddPair('active_form', Screen.ActiveForm.ClassName);
  ErrorData.AddPair('focused_control', Screen.ActiveControl.Name);
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
    FOfflineQueue: TList<TJSONObject>;
    procedure SendQueuedEvents;
  public
    procedure SendData(const EventData: TJSONObject);
  end;

procedure TTelemetry.SendData(const EventData: TJSONObject);  
begin  
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FHttpClient.Post(FServerURL, TStringStream.Create(EventData.ToString), nil);

        // Si succès et qu'il y a des événements en attente
        if FOfflineQueue.Count > 0 then
          SendQueuedEvents;
      except
        // Échec : mettre en file d'attente
        TMonitor.Enter(FOfflineQueue);
        try
          FOfflineQueue.Add(EventData);
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

Testez votre télémétrie avant de déployer :

```pascal
{$IFDEF DEBUG}
  // En développement : Logger localement
  Telemetry.FServerURL := 'http://localhost:5000/telemetry';
{$ELSE}
  // En production : Serveur réel
  Telemetry.FServerURL := 'https://telemetry.monapp.com/api/events';
{$ENDIF}
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

Si vous voulez héberger votre propre serveur, voici un exemple minimal avec Node.js :

```javascript
// server.js - Serveur de télémétrie simple
const express = require('express');  
const fs = require('fs');  
const app = express();  

app.use(express.json());

// Endpoint pour recevoir les événements
app.post('/api/events', (req, res) => {
  const event = req.body;

  // Logger dans un fichier
  const logEntry = JSON.stringify(event) + '\n';
  fs.appendFileSync('telemetry.log', logEntry);

  // Analyser les crashes pour alerte
  if (event.event_type === 'crash') {
    console.error('CRASH DETECTED:', event);
    // Envoyer une alerte (email, Slack, etc.)
  }

  res.status(200).send('OK');
});

// Endpoint pour visualiser les stats
app.get('/dashboard', (req, res) => {
  const logs = fs.readFileSync('telemetry.log', 'utf8');
  const events = logs.split('\n')
    .filter(line => line.trim())
    .map(line => JSON.parse(line));

  const stats = {
    total_events: events.length,
    crashes: events.filter(e => e.event_type === 'crash').length,
    errors: events.filter(e => e.event_type === 'error').length,
    active_users: new Set(events.map(e => e.user_id)).size
  };

  res.json(stats);
});

app.listen(5000, () => {
  console.log('Serveur de télémétrie démarré sur le port 5000');
});
```

**Démarrage** :
```bash
npm install express  
node server.js  
```

Votre application Delphi peut maintenant envoyer des événements à `http://localhost:5000/api/events`.

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

**Cause** : Pas de symboles de débogage

**Solution** :
- Compiler avec informations de débogage
- Utiliser madExcept, JclDebug ou EurekaLog
- Garder les fichiers .map pour chaque version

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
