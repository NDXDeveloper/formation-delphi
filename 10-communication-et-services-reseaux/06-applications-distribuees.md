🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.6 Applications distribuées

## Introduction aux applications distribuées

### Qu'est-ce qu'une application distribuée ?

Une **application distribuée** est un système informatique dont les composants sont situés sur différents ordinateurs en réseau, qui communiquent et coordonnent leurs actions pour accomplir un objectif commun.

**Analogie simple :**
Imaginez une chaîne de restaurants franchisés. Chaque restaurant (nœud) fonctionne de manière autonome, mais tous partagent des informations (menu, prix, stocks) et peuvent se coordonner pour des commandes groupées ou des transferts de personnel. C'est le principe d'un système distribué.

**Différence avec le client-serveur classique :**

**Client-Serveur traditionnel :**
```
Client 1 ──┐
Client 2 ──┼──→ Serveur Central ──→ Base de données
Client 3 ──┘
```
- Un serveur central
- Les clients dépendent du serveur
- Point de défaillance unique

**Système distribué :**
```
┌─────────┐     ┌─────────┐     ┌─────────┐
│ Nœud 1  │←───→│ Nœud 2  │←───→│ Nœud 3  │
│ (DB+App)│     │ (DB+App)│     │ (DB+App)│
└─────────┘     └─────────┘     └─────────┘
     ↕              ↕              ↕
┌─────────┐     ┌─────────┐     ┌─────────┐
│Client A │     │Client B │     │Client C │
└─────────┘     └─────────┘     └─────────┘
```
- Plusieurs serveurs (nœuds)
- Chaque nœud peut fonctionner indépendamment
- Résilience et redondance

### Pourquoi utiliser une architecture distribuée ?

**1. Haute disponibilité**
- Si un serveur tombe, les autres continuent
- Pas d'interruption de service
- Résilience aux pannes

**2. Performance et scalabilité**
- Répartition de la charge sur plusieurs serveurs
- Traitement parallèle
- Ajout de serveurs selon les besoins

**3. Géolocalisation**
- Serveurs proches des utilisateurs
- Latence réduite
- Conformité réglementaire (données locales)

**4. Tolérance aux pannes**
- Réplication des données
- Basculement automatique (failover)
- Continuité de service

**5. Isolation des défaillances**
- Une panne n'affecte qu'une partie du système
- Dégradation gracieuse
- Maintenance sans interruption

### Défis des systèmes distribués

**1. Complexité**
- Plus difficile à concevoir et développer
- Débogage complexe
- Tests plus élaborés

**2. Cohérence des données**
- Synchronisation entre nœuds
- Gestion des conflits
- Transactions distribuées

**3. Latence réseau**
- Communication entre nœuds peut être lente
- Timeouts à gérer
- Optimisation nécessaire

**4. Sécurité**
- Plus de points d'entrée à sécuriser
- Chiffrement des communications
- Authentification distribuée

**5. Gestion des versions**
- Déploiement coordonné
- Compatibilité entre versions
- Migration progressive

## Concepts fondamentaux

### Le théorème CAP

Le **théorème CAP** énonce qu'un système distribué ne peut garantir simultanément que deux des trois propriétés suivantes :

**C - Consistency (Cohérence)**
- Tous les nœuds voient les mêmes données au même moment
- Une lecture retourne toujours la dernière écriture

**A - Availability (Disponibilité)**
- Chaque requête reçoit une réponse (succès ou erreur)
- Le système répond même si certains nœuds sont en panne

**P - Partition Tolerance (Tolérance au partitionnement)**
- Le système continue de fonctionner malgré des coupures réseau
- Résistance aux problèmes de communication

**Choix typiques :**

```
┌────────────┬──────────────┬──────────────────────┐
│   Type     │  Priorité    │     Exemple          │
├────────────┼──────────────┼──────────────────────┤
│    CP      │  C + P       │  Bases bancaires     │
│            │  (pas A)     │  (cohérence critique)│
├────────────┼──────────────┼──────────────────────┤
│    AP      │  A + P       │  Réseaux sociaux     │
│            │  (pas C)     │  (disponibilité++)   │
├────────────┼──────────────┼──────────────────────┤
│    CA      │  C + A       │  SGBD traditionnels  │
│            │  (pas P)     │  (monosite)          │
└────────────┴──────────────┴──────────────────────┘
```

### Modèles de cohérence

**Cohérence forte (Strong Consistency)**
- Toutes les lectures voient la dernière écriture
- Garantie absolue mais coûteuse en performance
- Exemple : systèmes bancaires

**Cohérence éventuelle (Eventual Consistency)**
- Les données convergent vers la cohérence avec le temps
- Performance optimale
- Exemple : DNS, caches

**Cohérence causale (Causal Consistency)**
- Les opérations liées causalement sont ordonnées
- Compromis entre forte et éventuelle
- Exemple : réseaux sociaux

### Réplication et sharding

**Réplication (Duplication des données)**
```
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Serveur 1│       │ Serveur 2│       │ Serveur 3│
│ ┌──────┐ │       │ ┌──────┐ │       │ ┌──────┐ │
│ │Copie │ │◄─────►│ │Copie │ │◄─────►│ │Copie │ │
│ │ A+B+C│ │       │ │ A+B+C│ │       │ │ A+B+C│ │
│ └──────┘ │       │ └──────┘ │       │ └──────┘ │
└──────────┘       └──────────┘       └──────────┘
```
- Même données sur plusieurs serveurs
- Haute disponibilité
- Tolérance aux pannes

**Sharding (Partitionnement des données)**
```
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Serveur 1│       │ Serveur 2│       │ Serveur 3│
│ ┌──────┐ │       │ ┌──────┐ │       │ ┌──────┐ │
│ │  A   │ │       │ │  B   │ │       │ │  C   │ │
│ └──────┘ │       │ └──────┘ │       │ └──────┘ │
└──────────┘       └──────────┘       └──────────┘
```
- Données réparties sur plusieurs serveurs
- Scalabilité horizontale
- Améliore les performances

## Architectures d'applications distribuées

### Architecture Maître-Esclave (Master-Slave)

**Structure :**
```
           ┌─────────────┐
           │   Maître    │
           │  (Écriture) │
           └──────┬──────┘
                  │
        ┌─────────┼─────────┐
        │         │         │
   ┌────▼───┐ ┌───▼───┐ ┌───▼───┐
   │Esclave1│ │Esclave│ │Esclave│
   │(Lecture│ │  2    │ │  3    │
   └────────┘ └───────┘ └───────┘
```

**Caractéristiques :**
- Un serveur maître gère les écritures
- Plusieurs esclaves pour les lectures
- Réplication asynchrone ou synchrone

**Implémentation avec Delphi :**

```pascal
unit MasterSlaveManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FireDAC.Comp.Client;

type
  TServerRole = (srMaster, srSlave);

  TServerInfo = record
    Host: string;
    Port: Integer;
    Role: TServerRole;
    Active: Boolean;
  end;

  TMasterSlaveManager = class
  private
    FMasterServer: TServerInfo;
    FSlaveServers: TList<TServerInfo>;
    FCurrentSlaveIndex: Integer;
    function GetNextSlave: TServerInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMaster(const Host: string; Port: Integer);
    procedure AddSlave(const Host: string; Port: Integer);

    function GetConnectionForWrite: TFDConnection;
    function GetConnectionForRead: TFDConnection;

    procedure SyncToSlaves;
  end;

implementation

constructor TMasterSlaveManager.Create;
begin
  inherited;
  FSlaveServers := TList<TServerInfo>.Create;
  FCurrentSlaveIndex := -1;
end;

destructor TMasterSlaveManager.Destroy;
begin
  FSlaveServers.Free;
  inherited;
end;

procedure TMasterSlaveManager.AddMaster(const Host: string; Port: Integer);
begin
  FMasterServer.Host := Host;
  FMasterServer.Port := Port;
  FMasterServer.Role := srMaster;
  FMasterServer.Active := True;
end;

procedure TMasterSlaveManager.AddSlave(const Host: string; Port: Integer);
var
  Slave: TServerInfo;
begin
  Slave.Host := Host;
  Slave.Port := Port;
  Slave.Role := srSlave;
  Slave.Active := True;

  FSlaveServers.Add(Slave);
end;

function TMasterSlaveManager.GetNextSlave: TServerInfo;
begin
  // Round-robin : répartition circulaire des lectures
  Inc(FCurrentSlaveIndex);
  if FCurrentSlaveIndex >= FSlaveServers.Count then
    FCurrentSlaveIndex := 0;

  Result := FSlaveServers[FCurrentSlaveIndex];
end;

function TMasterSlaveManager.GetConnectionForWrite: TFDConnection;
begin
  // Les écritures vont toujours vers le maître
  Result := TFDConnection.Create(nil);
  Result.Params.Values['Server'] := FMasterServer.Host;
  Result.Params.Values['Port'] := IntToStr(FMasterServer.Port);
  Result.Params.Values['Database'] := 'mabase';
  Result.Connected := True;
end;

function TMasterSlaveManager.GetConnectionForRead: TFDConnection;
var
  Slave: TServerInfo;
begin
  // Les lectures sont réparties sur les esclaves
  if FSlaveServers.Count > 0 then
    Slave := GetNextSlave
  else
    Slave := FMasterServer; // Fallback sur le maître

  Result := TFDConnection.Create(nil);
  Result.Params.Values['Server'] := Slave.Host;
  Result.Params.Values['Port'] := IntToStr(Slave.Port);
  Result.Params.Values['Database'] := 'mabase';
  Result.Connected := True;
end;

procedure TMasterSlaveManager.SyncToSlaves;
var
  Slave: TServerInfo;
begin
  // Déclencher la synchronisation vers les esclaves
  // (La réplication MySQL/MariaDB se fait automatiquement)
  // Cette méthode peut vérifier l'état de la réplication
  for Slave in FSlaveServers do
  begin
    // Vérifier le lag de réplication
    // Envoyer des heartbeats
    // etc.
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  MSManager: TMasterSlaveManager;
  ConnWrite, ConnRead: TFDConnection;
  Query: TFDQuery;
begin
  MSManager := TMasterSlaveManager.Create;
  try
    // Configuration
    MSManager.AddMaster('192.168.1.10', 3306);
    MSManager.AddSlave('192.168.1.11', 3306);
    MSManager.AddSlave('192.168.1.12', 3306);

    // Écriture (sur le maître)
    ConnWrite := MSManager.GetConnectionForWrite;
    try
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := ConnWrite;
        Query.SQL.Text := 'INSERT INTO clients (nom) VALUES (:nom)';
        Query.ParamByName('nom').AsString := 'Nouveau Client';
        Query.ExecSQL;
      finally
        Query.Free;
      end;
    finally
      ConnWrite.Free;
    end;

    // Lecture (sur un esclave)
    ConnRead := MSManager.GetConnectionForRead;
    try
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := ConnRead;
        Query.SQL.Text := 'SELECT * FROM clients';
        Query.Open;
        // Utiliser les données...
      finally
        Query.Free;
      end;
    finally
      ConnRead.Free;
    end;

  finally
    MSManager.Free;
  end;
end;
```

### Architecture Pair-à-Pair (Peer-to-Peer)

**Structure :**
```
┌────────┐     ┌────────┐
│ Nœud 1 │◄───►│ Nœud 2 │
└───┬────┘     └─────┬──┘
    │                │
    │    ┌────────┐  │
    └───►│ Nœud 3 │◄─
         └───┬────┘
             │
        ┌────▼───┐
        │ Nœud 4 │
        └────────┘
```

**Caractéristiques :**
- Tous les nœuds sont égaux
- Pas de serveur central
- Auto-organisation

**Exemple simplifié :**

```pascal
type
  TPeerNode = class
  private
    FNodeID: string;
    FConnectedPeers: TList<TPeerNode>;
  public
    constructor Create(const NodeID: string);
    destructor Destroy; override;

    procedure ConnectToPeer(Peer: TPeerNode);
    procedure BroadcastMessage(const Message: string);
    procedure ReceiveMessage(const FromNodeID, Message: string);
  end;

constructor TPeerNode.Create(const NodeID: string);
begin
  inherited Create;
  FNodeID := NodeID;
  FConnectedPeers := TList<TPeerNode>.Create;
end;

destructor TPeerNode.Destroy;
begin
  FConnectedPeers.Free;
  inherited;
end;

procedure TPeerNode.ConnectToPeer(Peer: TPeerNode);
begin
  if not FConnectedPeers.Contains(Peer) then
  begin
    FConnectedPeers.Add(Peer);
    Peer.FConnectedPeers.Add(Self); // Connexion bidirectionnelle
  end;
end;

procedure TPeerNode.BroadcastMessage(const Message: string);
var
  Peer: TPeerNode;
begin
  // Envoyer à tous les pairs connectés
  for Peer in FConnectedPeers do
    Peer.ReceiveMessage(FNodeID, Message);
end;

procedure TPeerNode.ReceiveMessage(const FromNodeID, Message: string);
begin
  // Traiter le message reçu
  WriteLn(Format('[%s] Message de %s: %s', [FNodeID, FromNodeID, Message]));
end;
```

### Architecture Multi-Maître (Multi-Master)

**Structure :**
```
┌──────────┐         ┌──────────┐
│ Maître 1 │◄───────►│ Maître 2 │
│ (R/W)    │         │ (R/W)    │
└─────┬────┘         └────┬─────┘
      │                   │
      └───────┬───────────┘
              │
         ┌────▼─────┐
         │ Maître 3 │
         │  (R/W)   │
         └──────────┘
```

**Caractéristiques :**
- Plusieurs serveurs acceptent les écritures
- Synchronisation bidirectionnelle
- Gestion des conflits nécessaire

**Gestion des conflits :**

```pascal
type
  TConflictResolution = (crLastWriteWins, crFirstWriteWins, crManual);

  TDistributedRecord = record
    ID: Integer;
    Data: string;
    Version: Integer;
    LastModified: TDateTime;
    ModifiedBy: string;
    ServerID: string;
  end;

  TConflictResolver = class
  public
    class function ResolveConflict(const Record1, Record2: TDistributedRecord;
      Resolution: TConflictResolution): TDistributedRecord;
  end;

class function TConflictResolver.ResolveConflict(
  const Record1, Record2: TDistributedRecord;
  Resolution: TConflictResolution): TDistributedRecord;
begin
  case Resolution of
    crLastWriteWins:
      begin
        // Le dernier modifié gagne
        if Record1.LastModified > Record2.LastModified then
          Result := Record1
        else
          Result := Record2;
      end;

    crFirstWriteWins:
      begin
        // Le premier modifié gagne
        if Record1.LastModified < Record2.LastModified then
          Result := Record1
        else
          Result := Record2;
      end;

    crManual:
      begin
        // Nécessite une intervention humaine
        raise Exception.Create('Conflit nécessitant une résolution manuelle');
      end;
  end;
end;
```

### Architecture par événements (Event-Driven)

**Structure :**
```
┌──────────┐    Événements    ┌─────────────┐
│Producteur│─────────────────►│ Bus Événmt. │
└──────────┘                  └──────┬──────┘
                                     │
                    ┌────────────────┼────────────────┐
                    │                │                │
              ┌─────▼────┐    ┌──────▼───┐    ┌───────▼──┐
              │Consumer 1│    │Consumer 2│    │Consumer 3│
              └──────────┘    └──────────┘    └──────────┘
```

**Implémentation avec Message Queue :**

```pascal
unit EventBus;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TEventHandler = reference to procedure(const EventData: string);

  TEventBus = class
  private
    FSubscribers: TDictionary<string, TList<TEventHandler>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subscribe(const EventType: string; Handler: TEventHandler);
    procedure Unsubscribe(const EventType: string; Handler: TEventHandler);
    procedure Publish(const EventType, EventData: string);
  end;

implementation

constructor TEventBus.Create;
begin
  inherited;
  FSubscribers := TDictionary<string, TList<TEventHandler>>.Create;
end;

destructor TEventBus.Destroy;
var
  List: TList<TEventHandler>;
begin
  for List in FSubscribers.Values do
    List.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TEventBus.Subscribe(const EventType: string; Handler: TEventHandler);
var
  Handlers: TList<TEventHandler>;
begin
  if not FSubscribers.TryGetValue(EventType, Handlers) then
  begin
    Handlers := TList<TEventHandler>.Create;
    FSubscribers.Add(EventType, Handlers);
  end;

  Handlers.Add(Handler);
end;

procedure TEventBus.Unsubscribe(const EventType: string; Handler: TEventHandler);
var
  Handlers: TList<TEventHandler>;
begin
  if FSubscribers.TryGetValue(EventType, Handlers) then
    Handlers.Remove(Handler);
end;

procedure TEventBus.Publish(const EventType, EventData: string);
var
  Handlers: TList<TEventHandler>;
  Handler: TEventHandler;
begin
  if FSubscribers.TryGetValue(EventType, Handlers) then
  begin
    for Handler in Handlers do
    begin
      try
        Handler(EventData);
      except
        // Logger l'erreur mais continuer
      end;
    end;
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  EventBus: TEventBus;
begin
  EventBus := TEventBus.Create;
  try
    // S'abonner aux événements
    EventBus.Subscribe('UserCreated',
      procedure(const Data: string)
      begin
        WriteLn('Service Email: Envoi email de bienvenue à ' + Data);
      end);

    EventBus.Subscribe('UserCreated',
      procedure(const Data: string)
      begin
        WriteLn('Service Analytics: Nouvel utilisateur ' + Data);
      end);

    // Publier un événement
    EventBus.Publish('UserCreated', 'jean.dupont@example.com');

  finally
    EventBus.Free;
  end;
end;
```

## Synchronisation des données

### Synchronisation bidirectionnelle

**Algorithme de synchronisation :**

```pascal
type
  TSyncStatus = (ssNotSynced, ssSyncing, ssSynced, ssConflict);

  TSyncRecord = record
    TableName: string;
    RecordID: Integer;
    LocalVersion: Integer;
    RemoteVersion: Integer;
    Status: TSyncStatus;
    LastSyncTime: TDateTime;
  end;

  TDataSynchronizer = class
  private
    FLocalConnection: TFDConnection;
    FRemoteConnection: TFDConnection;
    function GetLocalVersion(const TableName: string; RecordID: Integer): Integer;
    function GetRemoteVersion(const TableName: string; RecordID: Integer): Integer;
    procedure PushToRemote(const TableName: string; RecordID: Integer);
    procedure PullFromRemote(const TableName: string; RecordID: Integer);
  public
    constructor Create(LocalConn, RemoteConn: TFDConnection);

    procedure SynchronizeTable(const TableName: string);
    procedure SynchronizeAll;
    function GetSyncStatus(const TableName: string): TList<TSyncRecord>;
  end;

procedure TDataSynchronizer.SynchronizeTable(const TableName: string);
var
  Query: TFDQuery;
  RecordID, LocalVer, RemoteVer: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FLocalConnection;
    Query.SQL.Text := Format('SELECT id, version FROM %s', [TableName]);
    Query.Open;

    while not Query.Eof do
    begin
      RecordID := Query.FieldByName('id').AsInteger;
      LocalVer := Query.FieldByName('version').AsInteger;
      RemoteVer := GetRemoteVersion(TableName, RecordID);

      if LocalVer > RemoteVer then
      begin
        // Local plus récent -> Pousser vers le serveur
        PushToRemote(TableName, RecordID);
      end
      else if RemoteVer > LocalVer then
      begin
        // Serveur plus récent -> Tirer du serveur
        PullFromRemote(TableName, RecordID);
      end;
      // Sinon, déjà synchronisé

      Query.Next;
    end;

  finally
    Query.Free;
  end;
end;

procedure TDataSynchronizer.PushToRemote(const TableName: string; RecordID: Integer);
var
  LocalQuery, RemoteQuery: TFDQuery;
  FieldName: string;
begin
  LocalQuery := TFDQuery.Create(nil);
  RemoteQuery := TFDQuery.Create(nil);
  try
    // Lire les données locales
    LocalQuery.Connection := FLocalConnection;
    LocalQuery.SQL.Text := Format('SELECT * FROM %s WHERE id = :id', [TableName]);
    LocalQuery.ParamByName('id').AsInteger := RecordID;
    LocalQuery.Open;

    if not LocalQuery.IsEmpty then
    begin
      // Mettre à jour sur le serveur distant
      RemoteQuery.Connection := FRemoteConnection;
      RemoteQuery.SQL.Text := Format(
        'UPDATE %s SET nom = :nom, email = :email, version = :version WHERE id = :id',
        [TableName]);

      RemoteQuery.ParamByName('id').AsInteger := RecordID;
      RemoteQuery.ParamByName('nom').AsString := LocalQuery.FieldByName('nom').AsString;
      RemoteQuery.ParamByName('email').AsString := LocalQuery.FieldByName('email').AsString;
      RemoteQuery.ParamByName('version').AsInteger := LocalQuery.FieldByName('version').AsInteger;
      RemoteQuery.ExecSQL;
    end;

  finally
    LocalQuery.Free;
    RemoteQuery.Free;
  end;
end;
```

### Synchronisation incrémentale

**Synchroniser seulement les changements :**

```pascal
type
  TChangeLog = record
    ID: Integer;
    TableName: string;
    RecordID: Integer;
    Operation: string; // 'INSERT', 'UPDATE', 'DELETE'
    ChangeDate: TDateTime;
    Synced: Boolean;
  end;

  TIncrementalSync = class
  private
    FConnection: TFDConnection;
    procedure LogChange(const TableName: string; RecordID: Integer;
      const Operation: string);
  public
    procedure SyncChanges(RemoteConnection: TFDConnection);
    function GetPendingChanges: TList<TChangeLog>;
  end;

procedure TIncrementalSync.LogChange(const TableName: string;
  RecordID: Integer; const Operation: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO change_log (table_name, record_id, operation, change_date, synced) ' +
      'VALUES (:table, :id, :op, :date, 0)';

    Query.ParamByName('table').AsString := TableName;
    Query.ParamByName('id').AsInteger := RecordID;
    Query.ParamByName('op').AsString := Operation;
    Query.ParamByName('date').AsDateTime := Now;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TIncrementalSync.SyncChanges(RemoteConnection: TFDConnection);
var
  Changes: TList<TChangeLog>;
  Change: TChangeLog;
  Query: TFDQuery;
begin
  Changes := GetPendingChanges;
  try
    for Change in Changes do
    begin
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := RemoteConnection;

        case Change.Operation of
          'INSERT':
            begin
              // Copier l'enregistrement vers le serveur distant
              // ...
            end;

          'UPDATE':
            begin
              // Mettre à jour sur le serveur distant
              // ...
            end;

          'DELETE':
            begin
              // Supprimer sur le serveur distant
              Query.SQL.Text := Format(
                'DELETE FROM %s WHERE id = :id', [Change.TableName]);
              Query.ParamByName('id').AsInteger := Change.RecordID;
              Query.ExecSQL;
            end;
        end;

        // Marquer comme synchronisé
        MarkAsSynced(Change.ID);

      finally
        Query.Free;
      end;
    end;
  finally
    Changes.Free;
  end;
end;
```

## Load Balancing (Répartition de charge)

### Round Robin simple

**Répartir les requêtes équitablement :**

```pascal
type
  TLoadBalancer = class
  private
    FServers: TList<string>;
    FCurrentIndex: Integer;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddServer(const ServerURL: string);
    function GetNextServer: string;
    procedure RemoveServer(const ServerURL: string);
  end;

constructor TLoadBalancer.Create;
begin
  inherited;
  FServers := TList<string>.Create;
  FCurrentIndex := -1;
  FLock := TCriticalSection.Create;
end;

destructor TLoadBalancer.Destroy;
begin
  FServers.Free;
  FLock.Free;
  inherited;
end;

procedure TLoadBalancer.AddServer(const ServerURL: string);
begin
  FLock.Enter;
  try
    if not FServers.Contains(ServerURL) then
      FServers.Add(ServerURL);
  finally
    FLock.Leave;
  end;
end;

function TLoadBalancer.GetNextServer: string;
begin
  FLock.Enter;
  try
    if FServers.Count = 0 then
      raise Exception.Create('Aucun serveur disponible');

    Inc(FCurrentIndex);
    if FCurrentIndex >= FServers.Count then
      FCurrentIndex := 0;

    Result := FServers[FCurrentIndex];
  finally
    FLock.Leave;
  end;
end;
```

### Weighted Round Robin

**Répartition pondérée selon la capacité :**

```pascal
type
  TServerConfig = record
    URL: string;
    Weight: Integer;  // Poids (capacité relative)
    Active: Boolean;
  end;

  TWeightedLoadBalancer = class
  private
    FServers: TList<TServerConfig>;
    FCurrentWeight: Integer;
    function GetWeightedServer: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddServer(const URL: string; Weight: Integer);
    function GetNextServer: string;
  end;

function TWeightedLoadBalancer.GetWeightedServer: string;
var
  i, TotalWeight, RandomWeight: Integer;
  Server: TServerConfig;
begin
  TotalWeight := 0;

  // Calculer le poids total
  for Server in FServers do
    if Server.Active then
      Inc(TotalWeight, Server.Weight);

  if TotalWeight = 0 then
    raise Exception.Create('Aucun serveur actif');

  // Sélection pondérée aléatoire
  RandomWeight := Random(TotalWeight);

  for i := 0 to FServers.Count - 1 do
  begin
    Server := FServers[i];
    if Server.Active then
    begin
      Dec(RandomWeight, Server.Weight);
      if RandomWeight < 0 then
      begin
        Result := Server.URL;
        Exit;
      end;
    end;
  end;
end;
```

### Health Check

**Vérifier la santé des serveurs :**

```pascal
type
  TServerHealth = (shUnknown, shHealthy, shDegraded, shDown);

  THealthCheck = class
  private
    FServerURL: string;
    FLastCheck: TDateTime;
    FHealth: TServerHealth;
    FResponseTime: Integer; // ms
  public
    function CheckHealth: TServerHealth;
    property ServerURL: string read FServerURL write FServerURL;
    property Health: TServerHealth read FHealth;
    property ResponseTime: Integer read FResponseTime;
  end;

function THealthCheck.CheckHealth: TServerHealth;
var
  HTTPClient: THTTPClient;
  StartTime: TDateTime;
  Response: IHTTPResponse;
begin
  HTTPClient := THTTPClient.Create;
  try
    HTTPClient.ConnectionTimeout := 3000; // 3 secondes

    StartTime := Now;

    try
      Response := HTTPClient.Get(FServerURL + '/health');

      FResponseTime := MilliSecondsBetween(Now, StartTime);

      if Response.StatusCode = 200 then
      begin
        if FResponseTime < 100 then
          FHealth := shHealthy
        else if FResponseTime < 500 then
          FHealth := shDegraded
        else
          FHealth := shDown;
      end
      else
        FHealth := shDown;

    except
      FHealth := shDown;
      FResponseTime := -1;
    end;

  finally
    HTTPClient.Free;
  end;

  FLastCheck := Now;
  Result := FHealth;
end;
```

## Gestion du cache distribué

### Implémentation simple d'un cache

```pascal
type
  TCacheEntry = record
    Key: string;
    Value: string;
    ExpiryTime: TDateTime;
  end;

  TDistributedCache = class
  private
    FCache: TDictionary<string, TCacheEntry>;
    FLock: TCriticalSection;
    FDefaultTTL: Integer; // Time To Live en secondes
  public
    constructor Create(DefaultTTL: Integer = 300);
    destructor Destroy; override;

    procedure Put(const Key, Value: string; TTL: Integer = 0);
    function Get(const Key: string; out Value: string): Boolean;
    procedure Delete(const Key: string);
    procedure Clear;
    procedure CleanupExpired;
  end;

constructor TDistributedCache.Create(DefaultTTL: Integer);
begin
  inherited Create;
  FCache := TDictionary<string, TCacheEntry>.Create;
  FLock := TCriticalSection.Create;
  FDefaultTTL := DefaultTTL;
end;

destructor TDistributedCache.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited;
end;

procedure TDistributedCache.Put(const Key, Value: string; TTL: Integer);
var
  Entry: TCacheEntry;
  ActualTTL: Integer;
begin
  FLock.Enter;
  try
    if TTL <= 0 then
      ActualTTL := FDefaultTTL
    else
      ActualTTL := TTL;

    Entry.Key := Key;
    Entry.Value := Value;
    Entry.ExpiryTime := IncSecond(Now, ActualTTL);

    FCache.AddOrSetValue(Key, Entry);
  finally
    FLock.Leave;
  end;
end;

function TDistributedCache.Get(const Key: string; out Value: string): Boolean;
var
  Entry: TCacheEntry;
begin
  FLock.Enter;
  try
    Result := FCache.TryGetValue(Key, Entry);

    if Result then
    begin
      // Vérifier l'expiration
      if Now > Entry.ExpiryTime then
      begin
        FCache.Remove(Key);
        Result := False;
      end
      else
        Value := Entry.Value;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TDistributedCache.CleanupExpired;
var
  Key: string;
  Entry: TCacheEntry;
  ExpiredKeys: TList<string>;
begin
  ExpiredKeys := TList<string>.Create;
  try
    FLock.Enter;
    try
      // Identifier les clés expirées
      for Entry in FCache.Values do
      begin
        if Now > Entry.ExpiryTime then
          ExpiredKeys.Add(Entry.Key);
      end;

      // Supprimer les clés expirées
      for Key in ExpiredKeys do
        FCache.Remove(Key);

    finally
      FLock.Leave;
    end;
  finally
    ExpiredKeys.Free;
  end;
end;
```

### Cache avec invalidation

```pascal
type
  TCacheWithInvalidation = class(TDistributedCache)
  private
    FInvalidationSubscribers: TList<TProc<string>>;
  public
    constructor Create(DefaultTTL: Integer = 300);
    destructor Destroy; override;

    procedure SubscribeToInvalidation(Callback: TProc<string>);
    procedure InvalidateKey(const Key: string);
    procedure InvalidatePattern(const Pattern: string);
  end;

procedure TCacheWithInvalidation.InvalidateKey(const Key: string);
var
  Subscriber: TProc<string>;
begin
  Delete(Key);

  // Notifier tous les abonnés
  for Subscriber in FInvalidationSubscribers do
  begin
    try
      Subscriber(Key);
    except
      // Ignorer les erreurs de notification
    end;
  end;
end;

procedure TCacheWithInvalidation.InvalidatePattern(const Pattern: string);
var
  Key: string;
  KeysToInvalidate: TList<string>;
begin
  KeysToInvalidate := TList<string>.Create;
  try
    FLock.Enter;
    try
      // Trouver toutes les clés correspondant au pattern
      for Key in FCache.Keys do
      begin
        if TRegEx.IsMatch(Key, Pattern) then
          KeysToInvalidate.Add(Key);
      end;
    finally
      FLock.Leave;
    end;

    // Invalider chaque clé
    for Key in KeysToInvalidate do
      InvalidateKey(Key);

  finally
    KeysToInvalidate.Free;
  end;
end;
```

## Transactions distribuées

### Two-Phase Commit (2PC)

**Protocole de validation en deux phases :**

```pascal
type
  TTransactionPhase = (tpPrepare, tpCommit, tpAbort);
  TParticipantStatus = (psReady, psCommitted, psAborted);

  TDistributedTransaction = class
  private
    FParticipants: TList<TFDConnection>;
    FTransactionID: string;
    function PrepareAll: Boolean;
    procedure CommitAll;
    procedure AbortAll;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParticipant(Connection: TFDConnection);
    function Execute(Operations: TProc): Boolean;
  end;

constructor TDistributedTransaction.Create;
begin
  inherited;
  FParticipants := TList<TFDConnection>.Create;
  FTransactionID := TGUID.NewGuid.ToString;
end;

destructor TDistributedTransaction.Destroy;
begin
  FParticipants.Free;
  inherited;
end;

procedure TDistributedTransaction.AddParticipant(Connection: TFDConnection);
begin
  FParticipants.Add(Connection);
end;

function TDistributedTransaction.PrepareAll: Boolean;
var
  Conn: TFDConnection;
begin
  Result := True;

  // Phase 1 : Préparer tous les participants
  for Conn in FParticipants do
  begin
    try
      Conn.StartTransaction;
      // Les opérations sont exécutées
      // Mais pas encore committées
    except
      Result := False;
      Break;
    end;
  end;
end;

procedure TDistributedTransaction.CommitAll;
var
  Conn: TFDConnection;
begin
  // Phase 2 : Valider tous les participants
  for Conn in FParticipants do
  begin
    try
      Conn.Commit;
    except
      // Logger l'erreur mais continuer
      // (état incohérent possible)
    end;
  end;
end;

procedure TDistributedTransaction.AbortAll;
var
  Conn: TFDConnection;
begin
  // Annuler tous les participants
  for Conn in FParticipants do
  begin
    try
      Conn.Rollback;
    except
      // Logger l'erreur
    end;
  end;
end;

function TDistributedTransaction.Execute(Operations: TProc): Boolean;
begin
  Result := False;

  try
    // Exécuter les opérations dans le contexte transactionnel
    Operations();

    // Préparer tous les participants
    if PrepareAll then
    begin
      // Si tous sont prêts, valider
      CommitAll;
      Result := True;
    end
    else
    begin
      // Si un échoue, tout annuler
      AbortAll;
    end;

  except
    on E: Exception do
    begin
      AbortAll;
      raise;
    end;
  end;
end;
```

**Utilisation :**

```pascal
var
  DistTx: TDistributedTransaction;
  Conn1, Conn2: TFDConnection;
begin
  Conn1 := TFDConnection.Create(nil); // Serveur 1
  Conn2 := TFDConnection.Create(nil); // Serveur 2

  DistTx := TDistributedTransaction.Create;
  try
    DistTx.AddParticipant(Conn1);
    DistTx.AddParticipant(Conn2);

    if DistTx.Execute(
      procedure
      var
        Query1, Query2: TFDQuery;
      begin
        // Opération sur serveur 1
        Query1 := TFDQuery.Create(nil);
        try
          Query1.Connection := Conn1;
          Query1.SQL.Text := 'UPDATE compte SET solde = solde - 100 WHERE id = 1';
          Query1.ExecSQL;
        finally
          Query1.Free;
        end;

        // Opération sur serveur 2
        Query2 := TFDQuery.Create(nil);
        try
          Query2.Connection := Conn2;
          Query2.SQL.Text := 'UPDATE compte SET solde = solde + 100 WHERE id = 2';
          Query2.ExecSQL;
        finally
          Query2.Free;
        end;
      end) then
      ShowMessage('Transaction distribuée réussie')
    else
      ShowMessage('Transaction distribuée annulée');

  finally
    DistTx.Free;
    Conn1.Free;
    Conn2.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Idempotence

**Rendre les opérations idempotentes :**

```pascal
// ✅ Bon - Opération idempotente
procedure UpdateUserEmail(UserID: Integer; const NewEmail: string);
begin
  // Peut être exécuté plusieurs fois avec le même résultat
  Query.SQL.Text := 'UPDATE users SET email = :email WHERE id = :id';
  Query.ParamByName('id').AsInteger := UserID;
  Query.ParamByName('email').AsString := NewEmail;
  Query.ExecSQL;
end;

// ❌ Éviter - Non idempotent
procedure IncrementCounter(UserID: Integer);
begin
  // Exécuté 2 fois = résultat différent
  Query.SQL.Text := 'UPDATE users SET counter = counter + 1 WHERE id = :id';
  Query.ParamByName('id').AsInteger := UserID;
  Query.ExecSQL;
end;
```

### 2. Eventual Consistency

**Accepter la cohérence éventuelle quand approprié :**

```pascal
// Pour des données non critiques
procedure UpdateViewCount(ArticleID: Integer);
begin
  // Mise à jour asynchrone, cohérence éventuelle acceptable
  TTask.Run(
    procedure
    begin
      Query.SQL.Text := 'UPDATE articles SET views = views + 1 WHERE id = :id';
      Query.ParamByName('id').AsInteger := ArticleID;
      Query.ExecSQL;
    end);
end;
```

### 3. Circuit Breaker

**Éviter les cascades de pannes :**

```pascal
type
  TCircuitState = (csClose, csOpen, csHalfOpen);

  TCircuitBreaker = class
  private
    FState: TCircuitState;
    FFailureCount: Integer;
    FFailureThreshold: Integer;
    FTimeout: Integer;
    FLastFailureTime: TDateTime;
  public
    constructor Create(FailureThreshold: Integer = 5; Timeout: Integer = 60);
    function Execute(Operation: TFunc<Boolean>): Boolean;
  end;

function TCircuitBreaker.Execute(Operation: TFunc<Boolean>): Boolean;
begin
  case FState of
    csOpen:
      begin
        // Vérifier si le timeout est écoulé
        if SecondsBetween(Now, FLastFailureTime) >= FTimeout then
          FState := csHalfOpen
        else
          raise Exception.Create('Circuit ouvert');
      end;

    csHalfOpen:
      begin
        // Essayer une requête
        try
          Result := Operation();
          if Result then
          begin
            FState := csClosed;
            FFailureCount := 0;
          end
          else
          begin
            FState := csOpen;
            FLastFailureTime := Now;
          end;
        except
          FState := csOpen;
          FLastFailureTime := Now;
          raise;
        end;
      end;

    csClosed:
      begin
        try
          Result := Operation();
          FFailureCount := 0;
        except
          Inc(FFailureCount);
          if FFailureCount >= FFailureThreshold then
          begin
            FState := csOpen;
            FLastFailureTime := Now;
          end;
          raise;
        end;
      end;
  end;
end;
```

### 4. Heartbeat et monitoring

**Surveiller la santé du système :**

```pascal
type
  THeartbeatMonitor = class
  private
    FNodes: TDictionary<string, TDateTime>;
    FTimeout: Integer; // secondes
  public
    constructor Create(Timeout: Integer = 30);
    destructor Destroy; override;

    procedure RegisterHeartbeat(const NodeID: string);
    function IsNodeAlive(const NodeID: string): Boolean;
    function GetDeadNodes: TArray<string>;
  end;

procedure THeartbeatMonitor.RegisterHeartbeat(const NodeID: string);
begin
  FNodes.AddOrSetValue(NodeID, Now);
end;

function THeartbeatMonitor.IsNodeAlive(const NodeID: string): Boolean;
var
  LastHeartbeat: TDateTime;
begin
  if FNodes.TryGetValue(NodeID, LastHeartbeat) then
    Result := SecondsBetween(Now, LastHeartbeat) < FTimeout
  else
    Result := False;
end;
```

### 5. Versioning des messages

**Gérer la compatibilité :**

```pascal
type
  TMessage = class
  private
    FVersion: Integer;
    FData: TJSONObject;
  public
    constructor Create(Version: Integer);
    destructor Destroy; override;

    property Version: Integer read FVersion;
    property Data: TJSONObject read FData;
  end;

function ParseMessage(const JSONStr: string): TMessage;
var
  JSON: TJSONObject;
  Version: Integer;
begin
  JSON := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  try
    Version := JSON.GetValue<Integer>('version');

    case Version of
      1: Result := ParseMessageV1(JSON);
      2: Result := ParseMessageV2(JSON);
    else
      raise Exception.Create('Version de message non supportée');
    end;
  finally
    JSON.Free;
  end;
end;
```

## Résumé

### Points clés des applications distribuées

✅ **Concepts fondamentaux :**
- Système réparti sur plusieurs nœuds
- CAP : Cohérence, Disponibilité, Tolérance au partitionnement
- Réplication et sharding
- Cohérence forte vs éventuelle

✅ **Architectures :**
- **Maître-Esclave** : Écritures sur maître, lectures sur esclaves
- **Pair-à-Pair** : Tous les nœuds égaux
- **Multi-Maître** : Plusieurs nœuds en écriture
- **Event-Driven** : Communication par événements

✅ **Synchronisation :**
- Bidirectionnelle avec gestion de conflits
- Incrémentale (seulement les changements)
- Versioning des données

✅ **Performance :**
- Load balancing (Round Robin, Weighted)
- Health checks
- Cache distribué
- Pagination

✅ **Transactions :**
- Two-Phase Commit (2PC)
- Gestion des erreurs distribuées
- Idempotence

✅ **Bonnes pratiques :**
- Circuit Breaker
- Heartbeat monitoring
- Versioning des messages
- Eventual consistency
- Logging distribué

Les applications distribuées offrent haute disponibilité et scalabilité, au prix d'une complexité accrue. Elles sont essentielles pour les systèmes modernes à grande échelle !

⏭️ [OAuth2 et authentification moderne](/10-communication-et-services-reseaux/07-oauth2-et-authentification-moderne.md)
