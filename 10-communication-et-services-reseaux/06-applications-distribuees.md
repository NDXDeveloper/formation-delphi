# 10.6 Applications distribuées

## Introduction

Les applications distribuées sont des systèmes informatiques où différents composants logiciels s'exécutent sur plusieurs ordinateurs interconnectés par un réseau. Dans ce chapitre, nous allons explorer comment Delphi peut être utilisé pour développer de telles applications.

## Qu'est-ce qu'une application distribuée ?

Une application distribuée se compose de plusieurs éléments qui fonctionnent ensemble mais sont répartis sur différentes machines :

- **Composants clients** : interfaces utilisateur et logique de présentation
- **Composants serveurs** : logique métier et accès aux données
- **Middleware** : communication entre les différents composants

![Architecture d'une application distribuée](https://via.placeholder.com/800x400)

## Avantages des applications distribuées

- **Scalabilité** : possibilité d'ajouter des serveurs pour gérer plus d'utilisateurs
- **Disponibilité** : meilleure résilience en cas de défaillance d'un composant
- **Performance** : répartition de la charge de travail
- **Flexibilité** : mise à jour des composants individuellement

## Technologies de Delphi pour les applications distribuées

Delphi offre plusieurs mécanismes pour créer des applications distribuées :

### 1. DataSnap

DataSnap est une technologie de Delphi qui permet de créer facilement des applications multi-niveaux en séparant la couche de données de la couche présentation :

```pascal
// Côté serveur - Création d'une méthode serveur
procedure TServerMethods.SaluerClient(const Nom: string): string;
begin
  Result := 'Bonjour ' + Nom + '!';
end;

// Côté client - Appel de la méthode serveur
var
  Resultat: string;
begin
  Resultat := ServerMethodsClient.SaluerClient('Jean');
  ShowMessage(Resultat); // Affiche "Bonjour Jean!"
end;
```

### 2. Enterprise Beans (EJB)

Delphi peut communiquer avec des EJB Java via le modèle COM :

```pascal
// Exemple de communication avec EJB
var
  MonBean: IMonBean;
begin
  MonBean := GetEJBReference('MonBean');
  MonBean.ExecuterAction('MaCommande');
end;
```

### 3. Apache Thrift

Thrift est un framework RPC qui permet à Delphi de communiquer avec des services écrits dans d'autres langages :

```pascal
// Utilisation de Thrift avec Delphi
var
  Client: TMonServiceClient;
begin
  Client := TMonServiceClient.Create(Transport, Protocol);
  try
    Client.EnvoyerMessage('Bonjour depuis Delphi!');
  finally
    Client.Free;
  end;
end;
```

## Création d'une application distribuée simple

### Étape 1 : Créer un serveur DataSnap

1. Créez un nouveau projet **Application DataSnap Server** dans Delphi
2. Ajoutez des méthodes serveur qui exposeront vos fonctionnalités :

```pascal
unit ServerMethodsUnit;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth;

type
  TServerMethods = class(TDSServerModule)
  public
    function AdditionnerDeuxNombres(A, B: Integer): Integer;
    function ObtenirDateHeure: TDateTime;
  end;

implementation

function TServerMethods.AdditionnerDeuxNombres(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TServerMethods.ObtenirDateHeure: TDateTime;
begin
  Result := Now;
end;

end.
```

### Étape 2 : Créer un client DataSnap

1. Créez un nouveau projet **Application VCL** dans Delphi
2. Ajoutez une connexion au serveur DataSnap :

```pascal
procedure TForm1.ConnecterAuServeur;
var
  Connection: TSQLConnection;
begin
  Connection := TSQLConnection.Create(nil);
  try
    Connection.DriverName := 'DataSnap';
    Connection.Params.Values['HostName'] := 'localhost';
    Connection.Params.Values['Port'] := '211';
    Connection.Connected := True;

    // Création du proxy client
    FServerMethods := TServerMethods1Client.Create(Connection);
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

### Étape 3 : Utiliser les méthodes serveur depuis le client

```pascal
procedure TForm1.btnAdditionnerClick(Sender: TObject);
var
  A, B, Resultat: Integer;
begin
  A := StrToIntDef(edtNombre1.Text, 0);
  B := StrToIntDef(edtNombre2.Text, 0);

  Resultat := FServerMethods.AdditionnerDeuxNombres(A, B);

  lblResultat.Caption := 'Résultat : ' + IntToStr(Resultat);
end;

procedure TForm1.btnDateHeureClick(Sender: TObject);
var
  DateHeure: TDateTime;
begin
  DateHeure := FServerMethods.ObtenirDateHeure;

  lblDateHeure.Caption := 'Date et heure du serveur : ' +
                          DateTimeToStr(DateHeure);
end;
```

## Patterns d'architecture pour applications distribuées

### 1. Architecture n-tier

L'architecture n-tier divise l'application en plusieurs couches :

- **Couche présentation** : interfaces utilisateur
- **Couche logique métier** : règles de gestion
- **Couche données** : accès aux bases de données

![Architecture n-tier](https://via.placeholder.com/800x300)

### 2. Architecture orientée services (SOA)

SOA permet de construire des applications sous forme de services indépendants et réutilisables :

```pascal
// Exemple de service SOA avec Delphi
type
  IServiceCommande = interface
    ['{GUID}']
    function CreerCommande(ClientID: Integer; Produits: TArray<TProduit>): Integer;
    function AnnulerCommande(CommandeID: Integer): Boolean;
    function StatutCommande(CommandeID: Integer): TStatutCommande;
  end;
```

### 3. Architecture microservices

Les microservices décomposent une application en petits services autonomes :

```pascal
// Exemple de microservice avec Delphi REST
type
  [ServiceContract]
  IMicroserviceInventaire = interface(IInvokable)
    ['{GUID}']
    [OperationContract]
    function VerifierStock(ProduitID: Integer): Integer;

    [OperationContract]
    function ReserverStock(ProduitID, Quantite: Integer): Boolean;
  end;
```

## Gestion des erreurs dans les applications distribuées

La gestion des erreurs est cruciale dans les applications distribuées car les défaillances peuvent survenir à différents niveaux :

```pascal
procedure TClientForm.ExecuterOperationDistante;
begin
  try
    // Tentative d'opération distante
    FServiceClient.OperationDistante;
  except
    on E: EServiceIndisponible do
      GererIndisponibiliteService;
    on E: ETimeoutException do
      GererTimeoutReseau;
    on E: Exception do
      GererErreurGenerique(E);
  end;
end;
```

## Considérations pour les applications distribuées

### 1. Sécurité

- Authentification des clients
- Chiffrement des communications
- Autorisation par rôle

```pascal
// Exemple d'authentification DataSnap
procedure TServerContainer.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TServerMethods1;
end;

procedure TServerMethods1.DSAuthenticationManager1UserAuthenticate(
  Sender: TObject; const Protocol, Context, User, Password: string;
  var valid: Boolean; UserRoles: TStrings);
begin
  valid := (User = 'admin') and (Password = 'secret');
  if valid then
    UserRoles.Add('Administrator');
end;
```

### 2. Performances

- Éviter les appels distants inutiles
- Traiter les données par lots
- Cache local des données

```pascal
// Exemple de cache des données
type
  TProduitsCache = class
  private
    FProduits: TDictionary<Integer, TProduit>;
    FDateDerniereMiseAJour: TDateTime;
  public
    function ObtenirProduit(ID: Integer): TProduit;
    procedure MettreAJourCache;
  end;
```

### 3. Tolérance aux pannes

- Détection des défaillances
- Reconnexion automatique
- Circuit breaker pattern

```pascal
// Exemple de circuit breaker
type
  TCircuitBreaker = class
  private
    FEtatOuvert: Boolean;
    FCompteurEchecs: Integer;
    FDateDerniereErreur: TDateTime;
    const SEUIL_ECHECS = 3;
    const DELAI_RECUPERATION = 60; // secondes
  public
    function ExecuterOperation(Operation: TFunc<Boolean>): Boolean;
  end;
```

## Outils de surveillance et de diagnostic

Pour les applications distribuées, il est important de mettre en place des outils de surveillance :

```pascal
// Exemple de journalisation
procedure TClientDistribue.LogAction(const Action, Details: string);
begin
  TLogger.Instance.Log(Format('[%s] %s - %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Action, Details]));
end;
```

## Exercice pratique : Création d'une application de chat distribué

Pour mettre en pratique les concepts abordés, créons une application de chat simple utilisant l'architecture distribuée :

1. Créez un serveur DataSnap qui gère les utilisateurs et les messages
2. Créez un client qui se connecte au serveur et envoie/reçoit des messages
3. Implémentez une interface utilisateur pour afficher les messages

## Conclusion

Les applications distribuées avec Delphi permettent de créer des systèmes évolutifs et robustes. Elles séparent logiquement les différentes parties de l'application, facilitant ainsi la maintenance et les évolutions futures.

Pour aller plus loin, vous pouvez explorer des technologies comme REST, GraphQL, ou MQTT pour créer des applications distribuées encore plus flexibles et performantes.

## Ressources supplémentaires

- Documentation officielle de DataSnap
- Exemples de projets DataSnap inclus dans Delphi
- Tutoriels vidéo sur les applications distribuées avec Delphi

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria.*
