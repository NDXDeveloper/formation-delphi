🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.6 Audit de sécurité

## Introduction

L'audit de sécurité est l'ensemble des processus qui permettent de surveiller, enregistrer et analyser les événements de sécurité dans votre application. C'est votre système de surveillance qui vous alerte en cas de problème et vous aide à comprendre ce qui s'est passé après un incident.

**Analogie du monde réel** : Pensez à l'audit de sécurité comme aux caméras de surveillance et au registre des visiteurs dans un bâtiment. Ils ne empêchent pas directement les intrusions, mais ils permettent de :
- Savoir qui est entré, quand et où
- Détecter les comportements suspects
- Reconstituer ce qui s'est passé après un incident
- Dissuader les comportements malveillants

### Pourquoi l'audit est-il crucial ?

**Détection rapide** : Repérer une attaque en cours avant qu'elle ne cause trop de dégâts

**Investigation** : Comprendre comment une faille a été exploitée

**Conformité** : Respecter les réglementations (RGPD, ISO 27001, etc.)

**Preuve légale** : Fournir des preuves en cas de procédure judiciaire

**Amélioration continue** : Identifier les faiblesses pour les corriger

## Les principes de base de la journalisation

### Que faut-il journaliser ?

**Événements de sécurité** :
- Connexions et déconnexions (réussies et échouées)
- Tentatives d'accès à des ressources protégées
- Modifications de droits et permissions
- Changements de mots de passe
- Création/suppression d'utilisateurs

**Événements métier critiques** :
- Transactions financières
- Modifications de données sensibles
- Suppressions importantes
- Exports de données
- Changements de configuration

**Événements suspects** :
- Tentatives d'injection SQL
- Tentatives d'accès non autorisé répétées
- Erreurs inhabituelles
- Pics d'activité anormaux

### Que NE FAUT-IL PAS journaliser ?

**❌ Données sensibles** :
- Mots de passe (même hashés)
- Numéros de cartes bancaires
- Données personnelles non nécessaires
- Clés de chiffrement
- Tokens d'authentification complets

```pascal
// ❌ MAUVAIS - Journalise des données sensibles
LogEvent('Connexion utilisateur : ' + Username + ' avec mot de passe : ' + Password);

// ✅ BON - Journalise l'événement sans les données sensibles
LogEvent('Connexion utilisateur : ' + Username + ' - Statut : succès');
```

## Implémentation d'un système de journalisation

### Structure d'un log

Un bon log contient au minimum :
1. **Timestamp** : Quand l'événement s'est produit
2. **Niveau** : Gravité (Info, Warning, Error, Critical)
3. **Source** : Qui ou quoi a généré l'événement
4. **Message** : Description de l'événement
5. **Contexte** : Informations additionnelles (IP, utilisateur, etc.)

### Classe de logging simple

```pascal
unit UnitLogger;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs;

type
  TNiveauLog = (nlDebug, nlInfo, nlWarning, nlError, nlCritical);

  TLogger = class
  private
    class var FInstance: TLogger;
    class var FLock: TCriticalSection;
    FFichierLog: string;
    procedure EcrireDansFichier(const ALigne: string);
  public
    class constructor Create;
    class destructor Destroy;
    class function Instance: TLogger;

    procedure Log(ANiveau: TNiveauLog; const AMessage: string;
                  const AContexte: string = '');
    procedure Debug(const AMessage: string; const AContexte: string = '');
    procedure Info(const AMessage: string; const AContexte: string = '');
    procedure Warning(const AMessage: string; const AContexte: string = '');
    procedure Error(const AMessage: string; const AContexte: string = '');
    procedure Critical(const AMessage: string; const AContexte: string = '');
  end;

implementation

class constructor TLogger.Create;  
begin  
  FLock := TCriticalSection.Create;
  FInstance := TLogger.Create;
  FInstance.FFichierLog := ChangeFileExt(ParamStr(0), '.log');
end;

class destructor TLogger.Destroy;  
begin  
  FInstance.Free;
  FLock.Free;
end;

class function TLogger.Instance: TLogger;  
begin  
  Result := FInstance;
end;

procedure TLogger.EcrireDansFichier(const ALigne: string);  
var  
  Fichier: TextFile;
begin
  FLock.Enter;
  try
    AssignFile(Fichier, FFichierLog);
    try
      if FileExists(FFichierLog) then
        Append(Fichier)
      else
        Rewrite(Fichier);

      WriteLn(Fichier, ALigne);
    finally
      CloseFile(Fichier);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TLogger.Log(ANiveau: TNiveauLog; const AMessage: string;
                       const AContexte: string);
var
  NiveauStr: string;
  Ligne: string;
begin
  case ANiveau of
    nlDebug: NiveauStr := 'DEBUG';
    nlInfo: NiveauStr := 'INFO';
    nlWarning: NiveauStr := 'WARNING';
    nlError: NiveauStr := 'ERROR';
    nlCritical: NiveauStr := 'CRITICAL';
  end;

  Ligne := Format('[%s] [%s] %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    NiveauStr,
    AMessage
  ]);

  if AContexte <> '' then
    Ligne := Ligne + ' | ' + AContexte;

  EcrireDansFichier(Ligne);
end;

procedure TLogger.Debug(const AMessage: string; const AContexte: string);  
begin  
  Log(nlDebug, AMessage, AContexte);
end;

procedure TLogger.Info(const AMessage: string; const AContexte: string);  
begin  
  Log(nlInfo, AMessage, AContexte);
end;

procedure TLogger.Warning(const AMessage: string; const AContexte: string);  
begin  
  Log(nlWarning, AMessage, AContexte);
end;

procedure TLogger.Error(const AMessage: string; const AContexte: string);  
begin  
  Log(nlError, AMessage, AContexte);
end;

procedure TLogger.Critical(const AMessage: string; const AContexte: string);  
begin  
  Log(nlCritical, AMessage, AContexte);
end;

end.
```

### Utilisation du logger

```pascal
uses
  UnitLogger;

// Journaliser une connexion réussie
procedure JournaliserConnexion(const AUsername: string);  
begin  
  TLogger.Instance.Info('Connexion utilisateur',
                        Format('Username: %s, IP: %s', [AUsername, ObtenirIPClient]));
end;

// Journaliser une tentative échouée
procedure JournaliserEchecConnexion(const AUsername: string);  
begin  
  TLogger.Instance.Warning('Échec de connexion',
                           Format('Username: %s, IP: %s', [AUsername, ObtenirIPClient]));
end;

// Journaliser une erreur
procedure JournaliserErreurBD(const AMessage: string);  
begin  
  TLogger.Instance.Error('Erreur base de données', AMessage);
end;

// Journaliser un événement critique
procedure JournaliserSecuriteCompromise;  
begin  
  TLogger.Instance.Critical('Tentative d''injection SQL détectée',
                             'IP: ' + ObtenirIPClient);
end;

// Exemple dans une application
procedure TFormLogin.BtnConnexionClick(Sender: TObject);  
var  
  Username: string;
begin
  Username := EditUsername.Text;

  if VerifierIdentifiants(Username, EditPassword.Text) then
  begin
    JournaliserConnexion(Username);
    ShowMessage('Connexion réussie');
  end
  else
  begin
    JournaliserEchecConnexion(Username);
    ShowMessage('Identifiants incorrects');
  end;
end;
```

## Journalisation en base de données

Pour des analyses plus avancées, stockez les logs en base de données :

### Structure de table de logs

```sql
CREATE TABLE LogsSecurite (
    ID BIGINT PRIMARY KEY AUTO_INCREMENT,
    DateHeure DATETIME DEFAULT CURRENT_TIMESTAMP,
    Niveau ENUM('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL') NOT NULL,
    Categorie VARCHAR(50),
    Message TEXT NOT NULL,
    IDUtilisateur INT,
    AdresseIP VARCHAR(45),
    UserAgent VARCHAR(255),
    URL VARCHAR(500),
    Duree INT,  -- Durée de l'opération en ms
    Contexte JSON,
    INDEX idx_date (DateHeure),
    INDEX idx_niveau (Niveau),
    INDEX idx_utilisateur (IDUtilisateur),
    INDEX idx_ip (AdresseIP)
);

-- Table dédiée pour les événements de connexion
CREATE TABLE LogsConnexions (
    ID BIGINT PRIMARY KEY AUTO_INCREMENT,
    DateHeure DATETIME DEFAULT CURRENT_TIMESTAMP,
    Username VARCHAR(100) NOT NULL,
    AdresseIP VARCHAR(45),
    Reussi BOOLEAN NOT NULL,
    RaisonEchec VARCHAR(255),
    INDEX idx_username (Username),
    INDEX idx_date (DateHeure),
    INDEX idx_ip (AdresseIP)
);

-- Table pour les modifications de données sensibles
CREATE TABLE LogsAudit (
    ID BIGINT PRIMARY KEY AUTO_INCREMENT,
    DateHeure DATETIME DEFAULT CURRENT_TIMESTAMP,
    IDUtilisateur INT NOT NULL,
    Action ENUM('CREATE', 'UPDATE', 'DELETE', 'READ') NOT NULL,
    TableCible VARCHAR(50) NOT NULL,
    IDEnregistrement INT NOT NULL,
    AnciennesValeurs JSON,
    NouvellesValeurs JSON,
    INDEX idx_utilisateur (IDUtilisateur),
    INDEX idx_table (TableCible),
    INDEX idx_date (DateHeure)
);
```

### Implémentation du logger en base de données

```pascal
type
  TLoggerBD = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure LogSecurite(ANiveau: string; const AMessage: string;
                          AIDUtilisateur: Integer; const AIP: string);
    procedure LogConnexion(const AUsername, AIP: string; AReussi: Boolean;
                           const ARaison: string = '');
    procedure LogAudit(AIDUtilisateur: Integer; const AAction, ATable: string;
                       AIDEnregistrement: Integer; const AAncien, ANouveau: string);
  end;

constructor TLoggerBD.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TLoggerBD.LogSecurite(ANiveau: string; const AMessage: string;
                                 AIDUtilisateur: Integer; const AIP: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsSecurite (Niveau, Message, IDUtilisateur, AdresseIP) ' +
      'VALUES (:Niveau, :Message, :IDUser, :IP)';
    Query.ParamByName('Niveau').AsString := ANiveau;
    Query.ParamByName('Message').AsString := AMessage;
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('IP').AsString := AIP;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TLoggerBD.LogConnexion(const AUsername, AIP: string; AReussi: Boolean;
                                  const ARaison: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsConnexions (Username, AdresseIP, Reussi, RaisonEchec) ' +
      'VALUES (:Username, :IP, :Reussi, :Raison)';
    Query.ParamByName('Username').AsString := AUsername;
    Query.ParamByName('IP').AsString := AIP;
    Query.ParamByName('Reussi').AsBoolean := AReussi;
    Query.ParamByName('Raison').AsString := ARaison;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TLoggerBD.LogAudit(AIDUtilisateur: Integer; const AAction, ATable: string;
                              AIDEnregistrement: Integer; const AAncien, ANouveau: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsAudit (IDUtilisateur, Action, TableCible, IDEnregistrement, ' +
      '                       AnciennesValeurs, NouvellesValeurs) ' +
      'VALUES (:IDUser, :Action, :Table, :ID, :Ancien, :Nouveau)';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('Action').AsString := AAction;
    Query.ParamByName('Table').AsString := ATable;
    Query.ParamByName('ID').AsInteger := AIDEnregistrement;
    Query.ParamByName('Ancien').AsString := AAncien;
    Query.ParamByName('Nouveau').AsString := ANouveau;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// Utilisation
procedure ModifierUtilisateur(AID: Integer; const ANouveauNom: string);  
var  
  AncienNom: string;
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Récupérer l'ancienne valeur
    Query.SQL.Text := 'SELECT Nom FROM Users WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := AID;
    Query.Open;
    AncienNom := Query.FieldByName('Nom').AsString;
    Query.Close;

    // Effectuer la modification
    Query.SQL.Text := 'UPDATE Users SET Nom = :Nom WHERE ID = :ID';
    Query.ParamByName('Nom').AsString := ANouveauNom;
    Query.ParamByName('ID').AsInteger := AID;
    Query.ExecSQL;

    // Journaliser la modification
    LoggerBD.LogAudit(UtilisateurConnecteID, 'UPDATE', 'Users', AID,
                      Format('{"Nom":"%s"}', [AncienNom]),
                      Format('{"Nom":"%s"}', [ANouveauNom]));
  finally
    Query.Free;
  end;
end;
```

## Analyse des logs

### Requêtes d'analyse utiles

```sql
-- Tentatives de connexion échouées dans les dernières 24h
SELECT Username, AdresseIP, COUNT(*) as NbTentatives, MAX(DateHeure) as DerniereTentative  
FROM LogsConnexions  
WHERE Reussi = FALSE AND DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR)  
GROUP BY Username, AdresseIP  
HAVING NbTentatives > 5  
ORDER BY NbTentatives DESC;  

-- Activités suspectes (même IP, plusieurs comptes)
SELECT AdresseIP, COUNT(DISTINCT Username) as NbComptes, COUNT(*) as NbConnexions  
FROM LogsConnexions  
WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 1 HOUR)  
GROUP BY AdresseIP  
HAVING NbComptes > 3;  

-- Événements critiques récents
SELECT DateHeure, Message, AdresseIP, IDUtilisateur  
FROM LogsSecurite  
WHERE Niveau = 'CRITICAL' AND DateHeure >= DATE_SUB(NOW(), INTERVAL 7 DAY)  
ORDER BY DateHeure DESC;  

-- Modifications de données sensibles
SELECT u.Username, la.Action, la.TableCible, la.DateHeure,
       la.AnciennesValeurs, la.NouvellesValeurs
FROM LogsAudit la  
JOIN Users u ON la.IDUtilisateur = u.ID  
WHERE la.TableCible IN ('Users', 'Transactions', 'Configurations')  
ORDER BY la.DateHeure DESC  
LIMIT 100;  

-- Pic d'activité inhabituel
SELECT DATE_FORMAT(DateHeure, '%Y-%m-%d %H:00:00') as Heure,
       COUNT(*) as NbEvenements
FROM LogsSecurite  
WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 7 DAY)  
GROUP BY Heure  
HAVING NbEvenements > 1000  
ORDER BY NbEvenements DESC;  
```

### Tableau de bord de sécurité

```pascal
type
  TDashboardSecurite = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    function ObtenirStatistiques24h: string;
    function ObtenirTentativesEchouees: TDataSet;
    function ObtenirEvenementsCritiques: TDataSet;
    function ObtenirIPsSuspectes: TDataSet;
  end;

constructor TDashboardSecurite.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

function TDashboardSecurite.ObtenirStatistiques24h: string;  
var  
  Query: TFDQuery;
  Stats: TStringList;
begin
  Query := TFDQuery.Create(nil);
  Stats := TStringList.Create;
  try
    Query.Connection := FConnection;

    // Nombre total de connexions
    Query.SQL.Text :=
      'SELECT COUNT(*) as Total FROM LogsConnexions ' +
      'WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR)';
    Query.Open;
    Stats.Add('Connexions totales : ' + Query.FieldByName('Total').AsString);
    Query.Close;

    // Connexions échouées
    Query.SQL.Text :=
      'SELECT COUNT(*) as Total FROM LogsConnexions ' +
      'WHERE Reussi = FALSE AND DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR)';
    Query.Open;
    Stats.Add('Connexions échouées : ' + Query.FieldByName('Total').AsString);
    Query.Close;

    // Événements critiques
    Query.SQL.Text :=
      'SELECT COUNT(*) as Total FROM LogsSecurite ' +
      'WHERE Niveau = ''CRITICAL'' AND DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR)';
    Query.Open;
    Stats.Add('Événements critiques : ' + Query.FieldByName('Total').AsString);
    Query.Close;

    Result := Stats.Text;
  finally
    Stats.Free;
    Query.Free;
  end;
end;

function TDashboardSecurite.ObtenirTentativesEchouees: TDataSet;  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  Query.Connection := FConnection;
  Query.SQL.Text :=
    'SELECT Username, AdresseIP, COUNT(*) as NbTentatives, ' +
    '       MAX(DateHeure) as DerniereTentative ' +
    'FROM LogsConnexions ' +
    'WHERE Reussi = FALSE AND DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR) ' +
    'GROUP BY Username, AdresseIP ' +
    'HAVING NbTentatives >= 3 ' +
    'ORDER BY NbTentatives DESC';
  Query.Open;
  Result := Query;
end;

function TDashboardSecurite.ObtenirEvenementsCritiques: TDataSet;  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  Query.Connection := FConnection;
  Query.SQL.Text :=
    'SELECT DateHeure, Message, AdresseIP, IDUtilisateur ' +
    'FROM LogsSecurite ' +
    'WHERE Niveau = ''CRITICAL'' AND DateHeure >= DATE_SUB(NOW(), INTERVAL 7 DAY) ' +
    'ORDER BY DateHeure DESC ' +
    'LIMIT 50';
  Query.Open;
  Result := Query;
end;

function TDashboardSecurite.ObtenirIPsSuspectes: TDataSet;  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  Query.Connection := FConnection;
  Query.SQL.Text :=
    'SELECT AdresseIP, COUNT(DISTINCT Username) as NbComptesDifferents, ' +
    '       COUNT(*) as NbTentatives ' +
    'FROM LogsConnexions ' +
    'WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 1 HOUR) ' +
    'GROUP BY AdresseIP ' +
    'HAVING NbComptesDifferents > 3 OR NbTentatives > 20 ' +
    'ORDER BY NbTentatives DESC';
  Query.Open;
  Result := Query;
end;
```

## Alertes automatiques

### Système d'alertes

```pascal
type
  TNiveauAlerte = (naInfo, naAttention, naCritique);

  TSystemeAlertes = class
  private
    FConnection: TFDConnection;
    procedure EnvoyerEmail(const ADestinataire, ASujet, ACorps: string);
    procedure EnvoyerSMS(const ANumero, AMessage: string);
  public
    constructor Create(AConnection: TFDConnection);
    procedure VerifierAlertes;
    procedure AlerterTentativesEchoueesExcessives;
    procedure AlerterActiviteSuspecte;
    procedure AlerterEvenementCritique(const AMessage: string);
  end;

constructor TSystemeAlertes.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TSystemeAlertes.EnvoyerEmail(const ADestinataire, ASujet, ACorps: string);  
begin  
  // Implémentation de l'envoi d'email
  // Utiliser Indy (TIdSMTP) ou un service comme SendGrid
end;

procedure TSystemeAlertes.EnvoyerSMS(const ANumero, AMessage: string);  
begin  
  // Implémentation de l'envoi de SMS
  // Utiliser un service comme Twilio, Nexmo, etc.
end;

procedure TSystemeAlertes.AlerterTentativesEchoueesExcessives;  
var  
  Query: TFDQuery;
  Message: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT Username, AdresseIP, COUNT(*) as NbTentatives ' +
      'FROM LogsConnexions ' +
      'WHERE Reussi = FALSE AND DateHeure >= DATE_SUB(NOW(), INTERVAL 15 MINUTE) ' +
      'GROUP BY Username, AdresseIP ' +
      'HAVING NbTentatives > 10';
    Query.Open;

    if not Query.IsEmpty then
    begin
      while not Query.Eof do
      begin
        Message := Format('ALERTE : %d tentatives de connexion échouées pour %s depuis %s',
          [Query.FieldByName('NbTentatives').AsInteger,
           Query.FieldByName('Username').AsString,
           Query.FieldByName('AdresseIP').AsString]);

        EnvoyerEmail('security@monentreprise.com',
                     'Alerte Sécurité : Tentatives de connexion',
                     Message);

        TLogger.Instance.Critical('Alerte déclenchée', Message);
        Query.Next;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSystemeAlertes.AlerterActiviteSuspecte;  
var  
  Query: TFDQuery;
  Message: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Détecter une IP qui essaie plusieurs comptes
    Query.SQL.Text :=
      'SELECT AdresseIP, COUNT(DISTINCT Username) as NbComptes ' +
      'FROM LogsConnexions ' +
      'WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 10 MINUTE) ' +
      'GROUP BY AdresseIP ' +
      'HAVING NbComptes > 5';
    Query.Open;

    if not Query.IsEmpty then
    begin
      while not Query.Eof do
      begin
        Message := Format('ALERTE : IP suspecte %s a tenté d''accéder à %d comptes différents',
          [Query.FieldByName('AdresseIP').AsString,
           Query.FieldByName('NbComptes').AsInteger]);

        EnvoyerEmail('security@monentreprise.com',
                     'Alerte Sécurité : Activité suspecte',
                     Message);

        // Pour les alertes critiques, envoyer aussi un SMS
        EnvoyerSMS('+33612345678', Message);

        TLogger.Instance.Critical('Activité suspecte détectée', Message);
        Query.Next;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSystemeAlertes.AlerterEvenementCritique(const AMessage: string);  
begin  
  EnvoyerEmail('security@monentreprise.com',
               'ALERTE CRITIQUE',
               'Événement critique détecté : ' + AMessage);
  EnvoyerSMS('+33612345678', 'ALERTE: ' + AMessage);
  TLogger.Instance.Critical('Alerte critique déclenchée', AMessage);
end;

procedure TSystemeAlertes.VerifierAlertes;  
begin  
  // Exécuter toutes les vérifications
  AlerterTentativesEchoueesExcessives;
  AlerterActiviteSuspecte;
  // Ajouter d'autres vérifications selon les besoins
end;

// Utilisation avec un timer
procedure TFormPrincipal.TimerAlerteTimer(Sender: TObject);  
begin  
  SystemeAlertes.VerifierAlertes;
end;
```

## Rotation et archivage des logs

### Pourquoi faire de la rotation ?

Les logs peuvent devenir **très volumineux** rapidement. Il faut :
- Archiver les anciens logs
- Nettoyer régulièrement
- Maintenir les performances

### Implémentation de la rotation

```pascal
type
  TGestionnaireLogsarchive = class
  private
    FConnection: TFDConnection;
    FRepertoireArchive: string;
  public
    constructor Create(AConnection: TFDConnection; const ARepertoire: string);
    procedure ArchiverLogsAnciens(AJoursConservation: Integer);
    procedure NettoyerLogsArchives(AMoisConservation: Integer);
    procedure ExporterLogsEnFichier(const ANomFichier: string; ADateDebut, ADateFin: TDateTime);
  end;

constructor TGestionnaireLogsArchive.Create(AConnection: TFDConnection;
                                             const ARepertoire: string);
begin
  inherited Create;
  FConnection := AConnection;
  FRepertoireArchive := ARepertoire;

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(FRepertoireArchive) then
    ForceDirectories(FRepertoireArchive);
end;

procedure TGestionnaireLogsArchive.ArchiverLogsAnciens(AJoursConservation: Integer);  
var  
  Query: TFDQuery;
  DateLimite: TDateTime;
  NomFichierArchive: string;
  Fichier: TextFile;
begin
  DateLimite := Now - AJoursConservation;
  NomFichierArchive := TPath.Combine(FRepertoireArchive,
    Format('logs_archive_%s.txt', [FormatDateTime('yyyymmdd', DateLimite)]));

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Exporter les logs à archiver
    Query.SQL.Text :=
      'SELECT * FROM LogsSecurite WHERE DateHeure < :DateLimite ORDER BY DateHeure';
    Query.ParamByName('DateLimite').AsDateTime := DateLimite;
    Query.Open;

    if not Query.IsEmpty then
    begin
      AssignFile(Fichier, NomFichierArchive);
      Rewrite(Fichier);
      try
        // Écrire l'en-tête
        WriteLn(Fichier, 'ID;DateHeure;Niveau;Message;IDUtilisateur;AdresseIP');

        // Écrire les données
        while not Query.Eof do
        begin
          WriteLn(Fichier, Format('%d;%s;%s;%s;%d;%s',
            [Query.FieldByName('ID').AsInteger,
             DateTimeToStr(Query.FieldByName('DateHeure').AsDateTime),
             Query.FieldByName('Niveau').AsString,
             Query.FieldByName('Message').AsString,
             Query.FieldByName('IDUtilisateur').AsInteger,
             Query.FieldByName('AdresseIP').AsString]));
          Query.Next;
        end;
      finally
        CloseFile(Fichier);
      end;

      // Supprimer les logs archivés de la base
      Query.Close;
      Query.SQL.Text := 'DELETE FROM LogsSecurite WHERE DateHeure < :DateLimite';
      Query.ParamByName('DateLimite').AsDateTime := DateLimite;
      Query.ExecSQL;

      TLogger.Instance.Info('Logs archivés',
        Format('Fichier: %s, Date limite: %s', [NomFichierArchive, DateTimeToStr(DateLimite)]));
    end;
  finally
    Query.Free;
  end;
end;

procedure TGestionnaireLogsArchive.NettoyerLogsArchives(AMoisConservation: Integer);  
var  
  SearchRec: TSearchRec;
  DateLimite: TDateTime;
  CheminFichier: string;
begin
  DateLimite := Now - (AMoisConservation * 30);

  if FindFirst(TPath.Combine(FRepertoireArchive, '*.txt'), faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        CheminFichier := TPath.Combine(FRepertoireArchive, SearchRec.Name);

        // Supprimer les fichiers plus anciens que la limite
        if FileDateToDateTime(SearchRec.Time) < DateLimite then
        begin
          DeleteFile(CheminFichier);
          TLogger.Instance.Info('Archive supprimée', CheminFichier);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TGestionnaireLogsArchive.ExporterLogsEnFichier(const ANomFichier: string;
                                                          ADateDebut, ADateFin: TDateTime);
var
  Query: TFDQuery;
  Fichier: TextFile;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT * FROM LogsSecurite ' +
      'WHERE DateHeure BETWEEN :DateDebut AND :DateFin ' +
      'ORDER BY DateHeure';
    Query.ParamByName('DateDebut').AsDateTime := ADateDebut;
    Query.ParamByName('DateFin').AsDateTime := ADateFin;
    Query.Open;

    AssignFile(Fichier, ANomFichier);
    Rewrite(Fichier);
    try
      WriteLn(Fichier, 'Rapport de logs - Période du ' +
                       DateToStr(ADateDebut) + ' au ' + DateToStr(ADateFin));
      WriteLn(Fichier, '');
      WriteLn(Fichier, 'Date/Heure | Niveau | Message | Utilisateur | IP');
      WriteLn(Fichier, StringOfChar('-', 80));

      while not Query.Eof do
      begin
        WriteLn(Fichier, Format('%s | %s | %s | %d | %s',
          [DateTimeToStr(Query.FieldByName('DateHeure').AsDateTime),
           Query.FieldByName('Niveau').AsString,
           Query.FieldByName('Message').AsString,
           Query.FieldByName('IDUtilisateur').AsInteger,
           Query.FieldByName('AdresseIP').AsString]));
        Query.Next;
      end;
    finally
      CloseFile(Fichier);
    end;
  finally
    Query.Free;
  end;
end;

// Tâche planifiée quotidienne
procedure TFormPrincipal.TimerArchivageTimer(Sender: TObject);  
begin  
  // Archiver les logs de plus de 90 jours
  GestionnaireArchive.ArchiverLogsAnciens(90);

  // Supprimer les archives de plus de 12 mois
  GestionnaireArchive.NettoyerLogsArchives(12);
end;
```

## Tests de sécurité

### Types de tests

**1. Tests de pénétration (Pentesting)**
- Simulation d'attaques réelles
- Identification des vulnérabilités
- Test de la résistance du système

**2. Scan de vulnérabilités**
- Analyse automatisée
- Détection de failles connues
- Vérification des configurations

**3. Revue de code**
- Analyse manuelle du code
- Recherche de patterns dangereux
- Vérification des bonnes pratiques

### Checklist d'audit de sécurité

```pascal
type
  TResultatAudit = record
    Description: string;
    Statut: Boolean;
    Gravite: string;
    Recommandation: string;
  end;

  TAuditSecurite = class
  private
    FConnection: TFDConnection;
    FResultats: TList<TResultatAudit>;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    procedure ExecuterAudit;
    procedure VerifierRequetesParametrees;
    procedure VerifierGestionMotsDePasse;
    procedure VerifierJournalisation;
    procedure VerifierConfigurationBD;
    procedure GenererRapport(const ANomFichier: string);
  end;

constructor TAuditSecurite.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
  FResultats := TList<TResultatAudit>.Create;
end;

destructor TAuditSecurite.Destroy;  
begin  
  FResultats.Free;
  inherited;
end;

procedure TAuditSecurite.VerifierRequetesParametrees;  
var  
  Resultat: TResultatAudit;
begin
  Resultat.Description := 'Vérification des requêtes paramétrées';

  // Cette vérification nécessite une analyse de code
  // Pour l'exemple, on suppose qu'elle est faite
  Resultat.Statut := True;
  Resultat.Gravite := 'CRITIQUE';
  Resultat.Recommandation := 'Toutes les requêtes doivent utiliser des paramètres';

  FResultats.Add(Resultat);
end;

procedure TAuditSecurite.VerifierGestionMotsDePasse;  
var  
  Query: TFDQuery;
  Resultat: TResultatAudit;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    Resultat.Description := 'Vérification du hashage des mots de passe';

    // Vérifier qu'aucun mot de passe n'est stocké en clair
    Query.SQL.Text := 'SELECT COUNT(*) as Total FROM Users WHERE LENGTH(Password) < 32';
    Query.Open;

    if Query.FieldByName('Total').AsInteger > 0 then
    begin
      Resultat.Statut := False;
      Resultat.Gravite := 'CRITIQUE';
      Resultat.Recommandation := 'Des mots de passe semblent stockés en clair ou mal hashés';
    end
    else
    begin
      Resultat.Statut := True;
      Resultat.Gravite := 'OK';
      Resultat.Recommandation := 'Les mots de passe sont correctement hashés';
    end;

    FResultats.Add(Resultat);
  finally
    Query.Free;
  end;
end;

procedure TAuditSecurite.VerifierJournalisation;  
var  
  Query: TFDQuery;
  Resultat: TResultatAudit;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    Resultat.Description := 'Vérification de la journalisation';

    // Vérifier qu'il y a bien des logs récents
    Query.SQL.Text :=
      'SELECT COUNT(*) as Total FROM LogsSecurite ' +
      'WHERE DateHeure >= DATE_SUB(NOW(), INTERVAL 24 HOUR)';
    Query.Open;

    if Query.FieldByName('Total').AsInteger > 0 then
    begin
      Resultat.Statut := True;
      Resultat.Gravite := 'OK';
      Resultat.Recommandation := 'La journalisation fonctionne correctement';
    end
    else
    begin
      Resultat.Statut := False;
      Resultat.Gravite := 'ÉLEVÉ';
      Resultat.Recommandation := 'Aucun log récent trouvé. Vérifier le système de journalisation';
    end;

    FResultats.Add(Resultat);
  finally
    Query.Free;
  end;
end;

procedure TAuditSecurite.VerifierConfigurationBD;  
var  
  Query: TFDQuery;
  Resultat: TResultatAudit;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Vérifier la version de MySQL
    Resultat.Description := 'Version de la base de données';
    Query.SQL.Text := 'SELECT VERSION() as Version';
    Query.Open;

    Resultat.Statut := True;
    Resultat.Gravite := 'INFO';
    Resultat.Recommandation := 'Version: ' + Query.FieldByName('Version').AsString;
    FResultats.Add(Resultat);
    Query.Close;

    // Vérifier que SSL est activé
    Resultat.Description := 'Connexion SSL à la base de données';
    Query.SQL.Text := 'SHOW STATUS LIKE "Ssl_cipher"';
    Query.Open;

    if Query.FieldByName('Value').AsString <> '' then
    begin
      Resultat.Statut := True;
      Resultat.Gravite := 'OK';
      Resultat.Recommandation := 'SSL activé: ' + Query.FieldByName('Value').AsString;
    end
    else
    begin
      Resultat.Statut := False;
      Resultat.Gravite := 'MOYEN';
      Resultat.Recommandation := 'La connexion à la base n''utilise pas SSL';
    end;

    FResultats.Add(Resultat);
  finally
    Query.Free;
  end;
end;

procedure TAuditSecurite.ExecuterAudit;  
begin  
  FResultats.Clear;

  VerifierRequetesParametrees;
  VerifierGestionMotsDePasse;
  VerifierJournalisation;
  VerifierConfigurationBD;
end;

procedure TAuditSecurite.GenererRapport(const ANomFichier: string);  
var  
  Fichier: TextFile;
  Resultat: TResultatAudit;
  i: Integer;
begin
  AssignFile(Fichier, ANomFichier);
  Rewrite(Fichier);
  try
    WriteLn(Fichier, '=== RAPPORT D''AUDIT DE SÉCURITÉ ===');
    WriteLn(Fichier, 'Date: ' + DateTimeToStr(Now));
    WriteLn(Fichier, '');
    WriteLn(Fichier, StringOfChar('=', 80));
    WriteLn(Fichier, '');

    for i := 0 to FResultats.Count - 1 do
    begin
      Resultat := FResultats[i];
      WriteLn(Fichier, Format('%d. %s', [i + 1, Resultat.Description]));
      WriteLn(Fichier, '   Statut: ' + IfThen(Resultat.Statut, 'OK', 'ÉCHEC'));
      WriteLn(Fichier, '   Gravité: ' + Resultat.Gravite);
      WriteLn(Fichier, '   Recommandation: ' + Resultat.Recommandation);
      WriteLn(Fichier, '');
    end;

    WriteLn(Fichier, StringOfChar('=', 80));
    WriteLn(Fichier, 'Fin du rapport');
  finally
    CloseFile(Fichier);
  end;
end;
```

## Conformité RGPD et traçabilité

### Exigences RGPD pour l'audit

Le RGPD impose de pouvoir :
- Prouver le consentement
- Tracer les accès aux données personnelles
- Répondre aux demandes d'accès
- Prouver la suppression des données

### Implémentation de la traçabilité RGPD

```pascal
type
  TTracabiliteRGPD = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure LogAccesDonneesPersonnelles(AIDUtilisateur, AIDPersonneConcernee: Integer;
                                           const ATypeAcces: string);
    procedure LogConsentement(AIDPersonne: Integer; const ATypeTraitement: string;
                               AConsenti: Boolean);
    procedure GenererRapportAcces(AIDPersonne: Integer; const ANomFichier: string);
    procedure GenererRapportSuppressions(ADateDebut, ADateFin: TDateTime;
                                          const ANomFichier: string);
  end;

constructor TTracabiliteRGPD.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TTracabiliteRGPD.LogAccesDonneesPersonnelles(AIDUtilisateur,
                                                         AIDPersonneConcernee: Integer;
                                                         const ATypeAcces: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsRGPD (IDUtilisateur, IDPersonneConcernee, TypeAcces, DateHeure) ' +
      'VALUES (:IDUser, :IDPersonne, :Type, NOW())';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('IDPersonne').AsInteger := AIDPersonneConcernee;
    Query.ParamByName('Type').AsString := ATypeAcces;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TTracabiliteRGPD.LogConsentement(AIDPersonne: Integer;
                                            const ATypeTraitement: string;
                                            AConsenti: Boolean);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO Consentements (IDPersonne, TypeTraitement, Consenti, DateHeure) ' +
      'VALUES (:IDPersonne, :Type, :Consenti, NOW())';
    Query.ParamByName('IDPersonne').AsInteger := AIDPersonne;
    Query.ParamByName('Type').AsString := ATypeTraitement;
    Query.ParamByName('Consenti').AsBoolean := AConsenti;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TTracabiliteRGPD.GenererRapportAcces(AIDPersonne: Integer;
                                                const ANomFichier: string);
var
  Query: TFDQuery;
  Fichier: TextFile;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT lr.DateHeure, u.Username, lr.TypeAcces ' +
      'FROM LogsRGPD lr ' +
      'JOIN Users u ON lr.IDUtilisateur = u.ID ' +
      'WHERE lr.IDPersonneConcernee = :IDPersonne ' +
      'ORDER BY lr.DateHeure DESC';
    Query.ParamByName('IDPersonne').AsInteger := AIDPersonne;
    Query.Open;

    AssignFile(Fichier, ANomFichier);
    Rewrite(Fichier);
    try
      WriteLn(Fichier, '=== RAPPORT D''ACCÈS AUX DONNÉES PERSONNELLES ===');
      WriteLn(Fichier, 'ID Personne: ' + IntToStr(AIDPersonne));
      WriteLn(Fichier, 'Date du rapport: ' + DateTimeToStr(Now));
      WriteLn(Fichier, '');
      WriteLn(Fichier, 'Date/Heure | Utilisateur | Type d''accès');
      WriteLn(Fichier, StringOfChar('-', 60));

      while not Query.Eof do
      begin
        WriteLn(Fichier, Format('%s | %s | %s',
          [DateTimeToStr(Query.FieldByName('DateHeure').AsDateTime),
           Query.FieldByName('Username').AsString,
           Query.FieldByName('TypeAcces').AsString]));
        Query.Next;
      end;
    finally
      CloseFile(Fichier);
    end;
  finally
    Query.Free;
  end;
end;
```

## Bonnes pratiques d'audit

### ✅ À faire

**1. Journaliser de manière cohérente**
```pascal
// Structure standardisée pour tous les logs
TLogger.Instance.Info('ACTION',
  Format('User:%s, IP:%s, Resource:%s', [Username, IP, Resource]));
```

**2. Inclure le contexte**
```pascal
// Toujours inclure qui, quoi, quand, où
LogEvent('User:' + Username + ', IP:' + IP + ', Action:Login, Result:Success');
```

**3. Protéger les logs**
```pascal
// Logs en lecture seule pour les utilisateurs normaux
// Accès restreint aux administrateurs
```

**4. Surveiller activement**
```pascal
// Ne pas juste collecter, mais analyser
TimerSurveillance.Enabled := True;  
TimerSurveillance.Interval := 300000; // Toutes les 5 minutes  
```

**5. Archiver régulièrement**
```pascal
// Rotation automatique pour éviter la saturation
ArchiverLogsAnciens(90); // Tous les 90 jours
```

### ❌ À éviter

**1. Ne pas logger les données sensibles**
```pascal
// ❌ DANGEREUX
Log('Password: ' + Password);

// ✅ BON
Log('Password changed successfully');
```

**2. Ne pas ignorer les logs**
```pascal
// Les logs ne servent à rien si personne ne les lit
AnalyserLogsQuotidiennement;
```

**3. Ne pas tout journaliser**
```pascal
// Trop de logs = bruit, difficile de trouver l'important
// Logger seulement ce qui est pertinent
```

## Checklist d'audit de sécurité

Avant le déploiement :

### Journalisation
- [ ] Tous les événements de sécurité sont journalisés
- [ ] Les connexions (succès et échecs) sont tracées
- [ ] Les modifications de données sensibles sont auditées
- [ ] Les logs incluent timestamp, utilisateur, IP
- [ ] Aucune donnée sensible n'est dans les logs

### Surveillance
- [ ] Système d'alertes configuré
- [ ] Détection des tentatives d'intrusion
- [ ] Monitoring des anomalies
- [ ] Dashboard de sécurité fonctionnel
- [ ] Notifications automatiques activées

### Archivage
- [ ] Rotation des logs configurée
- [ ] Archives sécurisées et backupées
- [ ] Politique de rétention définie
- [ ] Suppression automatique des logs obsolètes

### Conformité
- [ ] Traçabilité RGPD en place
- [ ] Rapports d'audit générables
- [ ] Consentements tracés
- [ ] Procédure de réponse aux demandes d'accès

### Tests
- [ ] Audit de sécurité réalisé
- [ ] Vulnérabilités identifiées et corrigées
- [ ] Tests de pénétration effectués
- [ ] Revue de code de sécurité complétée

## Résumé des points essentiels

✅ **Impératifs d'audit** :
- Journaliser TOUS les événements de sécurité
- Inclure contexte complet (qui, quoi, quand, où)
- Ne JAMAIS logger de données sensibles
- Surveiller activement, pas seulement collecter
- Alertes automatiques pour événements critiques
- Rotation et archivage réguliers
- Tests et audits périodiques

❌ **Erreurs d'audit fatales** :
- Ne pas journaliser les accès
- Logger des mots de passe ou tokens
- Ignorer les logs collectés
- Pas de système d'alertes
- Logs non protégés
- Pas d'archivage (saturation)
- Négliger les tests de sécurité

🎯 **Objectifs d'un bon audit** :
- **Détection** : Repérer les incidents rapidement
- **Investigation** : Comprendre ce qui s'est passé
- **Prévention** : Identifier les faiblesses avant exploitation
- **Conformité** : Respecter les obligations légales
- **Amélioration** : Apprendre et renforcer continuellement

## Aller plus loin

**Sections complémentaires** :
- **16.7** : Stockage sécurisé des identifiants
- **16.8** : GDPR et confidentialité des données
- **16.10** : Sécurité des applications mobiles

**Outils recommandés** :
- ELK Stack (Elasticsearch, Logstash, Kibana) : Analyse de logs avancée
- Splunk : Plateforme de monitoring et analyse
- Graylog : Gestion centralisée des logs
- OSSEC : Système de détection d'intrusions

**Normes et frameworks** :
- ISO 27001 : Management de la sécurité
- NIST Cybersecurity Framework
- CIS Controls : Contrôles de sécurité critiques

L'audit de sécurité n'est pas une option, c'est une obligation. Sans logs et surveillance, vous êtes aveugle face aux attaques. Investissez du temps dans un bon système d'audit, c'est votre meilleure assurance en cas d'incident.

⏭️ [Stockage sécurisé des identifiants](/16-securite-des-applications/07-stockage-securise-des-identifiants.md)
