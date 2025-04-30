# 18.11 Microservices et architecture distribuée

## Introduction

Dans les chapitres précédents, nous avons vu comment structurer une application Delphi monolithique, où tous les composants fonctionnent au sein d'un même processus. Mais qu'en est-il si votre application devient si complexe qu'elle devient difficile à maintenir ? Ou si vous avez besoin de faire évoluer certaines parties indépendamment des autres ? Ou encore si vous souhaitez répartir la charge sur plusieurs serveurs ?

C'est là qu'interviennent les architectures distribuées et les microservices. Dans ce chapitre, nous allons explorer ces concepts et voir comment les mettre en œuvre avec Delphi, en gardant une approche accessible pour les débutants.

## Qu'est-ce qu'une architecture distribuée ?

Une architecture distribuée est un système informatique dans lequel les différents composants sont répartis sur plusieurs ordinateurs ou serveurs, mais fonctionnent ensemble comme un tout cohérent. Ces composants communiquent entre eux via un réseau.

Imaginez une entreprise avec plusieurs départements (comptabilité, ressources humaines, production, etc.). Chaque département peut fonctionner de manière relativement autonome, mais ils doivent communiquer et collaborer pour que l'entreprise fonctionne dans son ensemble. Une architecture distribuée suit le même principe.

## Qu'est-ce que les microservices ?

Les microservices représentent une approche spécifique d'architecture distribuée. Dans ce modèle, une application est décomposée en petits services indépendants, chacun ayant une responsabilité bien définie et pouvant être développé, déployé et mis à l'échelle séparément.

### Comparaison avec l'architecture monolithique

Pour mieux comprendre, comparons les deux approches :

**Architecture monolithique** :
- Une seule application qui gère toutes les fonctionnalités
- Tout le code est déployé ensemble
- Si une partie échoue, toute l'application peut être affectée
- Mise à l'échelle de l'application entière

**Architecture microservices** :
- Plusieurs petits services, chacun avec sa propre responsabilité
- Chaque service peut être déployé indépendamment
- Si un service échoue, les autres peuvent continuer à fonctionner
- Chaque service peut être mis à l'échelle selon ses besoins

## Avantages et inconvénients des microservices

### Avantages

1. **Évolutivité indépendante** : Vous pouvez faire évoluer certaines parties de votre application sans toucher aux autres.
2. **Équipes autonomes** : Différentes équipes peuvent travailler sur différents services.
3. **Résilience** : La défaillance d'un service n'entraîne pas nécessairement celle de l'ensemble du système.
4. **Adaptabilité technologique** : Chaque service peut utiliser la technologie la plus adaptée à son objectif.
5. **Déploiement plus facile** : Les déploiements peuvent être plus petits et plus fréquents.

### Inconvénients

1. **Complexité accrue** : La gestion de plusieurs services interagissant ensemble est plus complexe.
2. **Overhead de communication** : Les services doivent communiquer via un réseau, ce qui introduit une latence.
3. **Gestion des transactions** : Maintenir la cohérence des données entre services peut être difficile.
4. **Surveillance et débogage** : Suivre un problème à travers plusieurs services peut être complexe.
5. **Coûts d'infrastructure** : Plus de services signifie généralement plus de ressources informatiques.

## Quand utiliser les microservices ?

Les microservices ne sont pas une solution universelle. Ils conviennent particulièrement aux :

- Applications complexes avec de nombreuses fonctionnalités
- Systèmes nécessitant une grande évolutivité
- Équipes de développement importantes travaillant en parallèle
- Applications ayant des besoins de performance très variés selon les fonctionnalités

Pour les applications plus simples, une architecture monolithique est souvent plus appropriée et plus facile à gérer.

## Mise en œuvre d'une architecture microservices avec Delphi

Delphi offre plusieurs mécanismes pour créer des applications distribuées. Explorons les principales approches :

### 1. Services REST avec Delphi

Les API REST sont l'une des méthodes les plus populaires pour créer des microservices, et Delphi les supporte très bien.

#### Création d'un service REST simple

```pascal
// Serveur REST basique avec Delphi
program MicroserviceServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Jhonson, // Pour le support JSON
  System.JSON;

begin
  // Middleware pour gérer le JSON
  THorse.Use(Jhonson);

  // Définir une route GET
  THorse.Get('/api/clients',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      Clients: TJSONArray;
      Client: TJSONObject;
    begin
      Clients := TJSONArray.Create;

      // Exemple : ajouter quelques clients
      Client := TJSONObject.Create;
      Client.AddPair('id', TJSONNumber.Create(1));
      Client.AddPair('name', 'Dupont SA');
      Client.AddPair('email', 'contact@dupont.com');
      Clients.AddElement(Client);

      Client := TJSONObject.Create;
      Client.AddPair('id', TJSONNumber.Create(2));
      Client.AddPair('name', 'Martin Inc');
      Client.AddPair('email', 'info@martin.com');
      Clients.AddElement(Client);

      // Renvoyer la réponse JSON
      Res.Send<TJSONArray>(Clients);
    end);

  // Définir une route POST
  THorse.Post('/api/clients',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      Client: TJSONObject;
    begin
      Client := Req.Body<TJSONObject>.Clone as TJSONObject;

      // Dans un cas réel, vous enregistreriez le client dans une base de données
      // Cet exemple simule juste une réponse
      Client.AddPair('id', TJSONNumber.Create(Random(1000) + 3));

      // Renvoyer le client créé
      Res.Status(201); // Created
      Res.Send<TJSONObject>(Client);
    end);

  // Démarrer le serveur
  THorse.Listen(9000,
    procedure
    begin
      Writeln('Microservice clients en écoute sur le port 9000...');
    end);
end.
```

> **Note** : Cet exemple utilise la bibliothèque Horse, un framework web moderne pour Delphi. Vous pouvez l'installer via GetIt Package Manager dans l'IDE Delphi.

#### Appel du service depuis un client

```pascal
procedure TMainForm.GetClientsButtonClick(Sender: TObject);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  i: Integer;
begin
  RESTClient := TRESTClient.Create('http://localhost:9000');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.Resource := 'api/clients';

    // Exécuter la requête
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // Analyser la réponse JSON
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);

      if JSONValue is TJSONArray then
      begin
        JSONArray := JSONValue as TJSONArray;

        // Afficher les clients dans un mémo
        MemoClients.Clear;
        for i := 0 to JSONArray.Count - 1 do
        begin
          MemoClients.Lines.Add(
            Format('ID: %s, Nom: %s, Email: %s',
              [JSONArray.Items[i].GetValue<string>('id'),
               JSONArray.Items[i].GetValue<string>('name'),
               JSONArray.Items[i].GetValue<string>('email')])
          );
        end;
      end;

      JSONValue.Free;
    end
    else
      ShowMessage('Erreur: ' + RESTResponse.StatusText);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### 2. DataSnap pour les architectures distribuées

DataSnap est une technologie Delphi mature pour créer des applications distribuées. Elle supporte différentes approches, y compris REST, mais offre également des fonctionnalités plus avancées comme les appels de méthodes distantes.

#### Création d'un serveur DataSnap

```pascal
// Serveur DataSnap
unit ServerMethods;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth;

type
  TClientService = class(TDSServerModule)
  private
    // Champs privés
  public
    function GetClients: TJSONArray;
    function AddClient(const AName, AEmail: string): TJSONObject;
    function GetClientById(const AId: Integer): TJSONObject;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.JSON;

function TClientService.GetClients: TJSONArray;
var
  Client: TJSONObject;
begin
  // Dans un cas réel, vous récupéreriez les données depuis une base de données
  Result := TJSONArray.Create;

  Client := TJSONObject.Create;
  Client.AddPair('id', TJSONNumber.Create(1));
  Client.AddPair('name', 'Dupont SA');
  Client.AddPair('email', 'contact@dupont.com');
  Result.AddElement(Client);

  Client := TJSONObject.Create;
  Client.AddPair('id', TJSONNumber.Create(2));
  Client.AddPair('name', 'Martin Inc');
  Client.AddPair('email', 'info@martin.com');
  Result.AddElement(Client);
end;

function TClientService.AddClient(const AName, AEmail: string): TJSONObject;
begin
  // Dans un cas réel, vous inséreriez dans une base de données
  Result := TJSONObject.Create;
  Result.AddPair('id', TJSONNumber.Create(Random(1000) + 3));
  Result.AddPair('name', AName);
  Result.AddPair('email', AEmail);
end;

function TClientService.GetClientById(const AId: Integer): TJSONObject;
begin
  // Simuler la recherche d'un client
  if AId = 1 then
  begin
    Result := TJSONObject.Create;
    Result.AddPair('id', TJSONNumber.Create(1));
    Result.AddPair('name', 'Dupont SA');
    Result.AddPair('email', 'contact@dupont.com');
  end
  else if AId = 2 then
  begin
    Result := TJSONObject.Create;
    Result.AddPair('id', TJSONNumber.Create(2));
    Result.AddPair('name', 'Martin Inc');
    Result.AddPair('email', 'info@martin.com');
  end
  else
    Result := TJSONObject.Create; // Client non trouvé
end;

end.
```

#### Configuration du serveur DataSnap

```pascal
// Unité du serveur
unit ServerContainer;

interface

uses
  System.SysUtils, System.Classes,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Datasnap.DSAuth, IPPeerServer, IndyPeerImpl;

type
  TServerContainer = class(TDataModule)
    DSServer: TDSServer;
    DSAuthenticationManager: TDSAuthenticationManager;
    DSServerClass: TDSServerClass;
    procedure DSServerClassGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
  private
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  ServerMethods;

procedure TServerContainer.DSServerClassGetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := ServerMethods.TClientService;
end;

constructor TServerContainer.Create(AOwner: TComponent);
begin
  inherited;
  DSServer.Start;
end;

destructor TServerContainer.Destroy;
begin
  DSServer.Stop;
  inherited;
end;

end.
```

#### Utilisation du client DataSnap

```pascal
procedure TMainForm.GetClientsButtonClick(Sender: TObject);
var
  ClientModule: TClientModule;
  ClientsJson: TJSONArray;
  i: Integer;
begin
  ClientModule := TClientModule.Create(nil);
  try
    // Configurer la connexion
    ClientModule.DSRestConnection1.Host := 'localhost';
    ClientModule.DSRestConnection1.Port := 8080;

    try
      // Appeler la méthode distante
      ClientsJson := ClientModule.ServerMethods1Client.GetClients;

      try
        // Afficher les clients
        MemoClients.Clear;
        for i := 0 to ClientsJson.Count - 1 do
        begin
          MemoClients.Lines.Add(
            Format('ID: %s, Nom: %s, Email: %s',
              [ClientsJson.Items[i].GetValue<string>('id'),
               ClientsJson.Items[i].GetValue<string>('name'),
               ClientsJson.Items[i].GetValue<string>('email')])
          );
        end;
      finally
        ClientsJson.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    end;
  finally
    ClientModule.Free;
  end;
end;
```

### 3. gRPC avec Delphi (approche moderne)

gRPC est un framework RPC (Remote Procedure Call) moderne développé par Google. Il est performant, multiplateforme et de plus en plus utilisé pour les microservices. Depuis Delphi 10.4 ou avec des composants tiers, vous pouvez l'utiliser dans vos projets.

#### Définition du service avec Protocol Buffers (.proto)

```protobuf
syntax = "proto3";

package clients;

// Service de gestion des clients
service ClientService {
  // Obtenir tous les clients
  rpc GetClients(GetClientsRequest) returns (GetClientsResponse);

  // Ajouter un nouveau client
  rpc AddClient(AddClientRequest) returns (Client);

  // Obtenir un client par son ID
  rpc GetClientById(GetClientByIdRequest) returns (Client);
}

// Message vide pour la requête GetClients
message GetClientsRequest {}

// Réponse contenant une liste de clients
message GetClientsResponse {
  repeated Client clients = 1;
}

// Requête pour ajouter un client
message AddClientRequest {
  string name = 1;
  string email = 2;
}

// Requête pour obtenir un client par ID
message GetClientByIdRequest {
  int32 id = 1;
}

// Structure d'un client
message Client {
  int32 id = 1;
  string name = 2;
  string email = 3;
}
```

> **Note** : L'utilisation de gRPC avec Delphi peut nécessiter des composants tiers ou des configurations spécifiques selon votre version de Delphi. Des solutions comme DelphiGrpc ou des wrappers sont disponibles.

## Architecture d'une application basée sur les microservices

Voyons maintenant comment structurer une application complète basée sur les microservices. Prenons l'exemple d'une application de gestion commerciale :

### Identification des microservices

1. **Service de gestion des clients** : Gère les informations des clients
2. **Service de gestion des produits** : Gère le catalogue de produits
3. **Service de commandes** : Gère les commandes des clients
4. **Service de facturation** : Gère la facturation et les paiements
5. **Service de notification** : Envoie des notifications (emails, SMS, etc.)
6. **Service d'authentification** : Gère l'authentification et les autorisations
7. **Application cliente** : Interface utilisateur (desktop ou web)

### Structure du projet

```
MonProjetMicroservices/
  ├── SharedLibs/                      # Bibliothèques partagées
  │   ├── ClientModels/                # Modèles de données clients
  │   ├── ProductModels/               # Modèles de données produits
  │   └── Common/                      # Utilitaires communs
  │
  ├── ClientService/                   # Service de gestion des clients
  │   ├── ClientService.dproj          # Projet du service
  │   ├── API/                         # Contrôleurs d'API
  │   ├── Domain/                      # Logique métier
  │   └── Data/                        # Accès aux données
  │
  ├── ProductService/                  # Service de gestion des produits
  │   ├── ProductService.dproj
  │   ├── API/
  │   ├── Domain/
  │   └── Data/
  │
  ├── OrderService/                    # Service de commandes
  │   ├── OrderService.dproj
  │   ├── API/
  │   ├── Domain/
  │   └── Data/
  │
  ├── InvoiceService/                  # Service de facturation
  │   ├── InvoiceService.dproj
  │   ├── API/
  │   ├── Domain/
  │   └── Data/
  │
  ├── NotificationService/             # Service de notification
  │   ├── NotificationService.dproj
  │   ├── API/
  │   ├── EmailProvider/
  │   └── SMSProvider/
  │
  ├── AuthService/                     # Service d'authentification
  │   ├── AuthService.dproj
  │   ├── API/
  │   ├── Auth/
  │   └── Users/
  │
  ├── ClientApp/                       # Application cliente desktop
  │   ├── ClientApp.dproj
  │   ├── Forms/
  │   ├── ViewModels/
  │   └── Services/                    # Clients pour les microservices
  │
  └── APIGateway/                      # Gateway pour centraliser les accès API
      └── APIGateway.dproj
```

### Communication entre les services

Pour permettre aux services de communiquer entre eux, plusieurs approches sont possibles :

#### 1. Communication synchrone via REST ou gRPC

Les services s'appellent directement via des API REST ou gRPC. Par exemple, le service de commandes pourrait appeler le service de clients pour obtenir les informations d'un client.

```pascal
// Dans le service de commandes
function TOrderService.GetCustomerInfo(const ACustomerId: Integer): TCustomerInfo;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
begin
  RESTClient := TRESTClient.Create('http://client-service:9001');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.Resource := Format('api/clients/%d', [ACustomerId]);

    // Exécuter la requête
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // Analyser la réponse JSON
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        Result.Id := JSONValue.GetValue<Integer>('id');
        Result.Name := JSONValue.GetValue<string>('name');
        Result.Email := JSONValue.GetValue<string>('email');
      finally
        JSONValue.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur lors de la récupération du client (Code: %d)',
        [RESTResponse.StatusCode]);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

#### 2. Communication asynchrone via un message broker

Les services communiquent via un système de messagerie comme RabbitMQ ou Apache Kafka. Cette approche est plus découplée et plus résiliente.

```pascal
// Dans le service de commandes, après la création d'une commande
procedure TOrderProcessor.PublishOrderCreatedEvent(const AOrder: TOrder);
var
  RabbitMQ: TRabbitMQPublisher;
  Message: TJSONObject;
begin
  RabbitMQ := TRabbitMQPublisher.Create('localhost', 5672, 'guest', 'guest');
  try
    // Créer le message d'événement
    Message := TJSONObject.Create;
    Message.AddPair('event_type', 'order_created');
    Message.AddPair('order_id', TJSONNumber.Create(AOrder.Id));
    Message.AddPair('customer_id', TJSONNumber.Create(AOrder.CustomerId));
    Message.AddPair('total_amount', TJSONNumber.Create(AOrder.TotalAmount));

    // Publier le message sur l'exchange 'orders'
    RabbitMQ.PublishMessage('orders', '', Message.ToString);
  finally
    Message.Free;
    RabbitMQ.Free;
  end;
end;

// Dans le service de notification, abonnement aux événements
procedure TNotificationService.SubscribeToOrderEvents;
var
  RabbitMQ: TRabbitMQConsumer;
begin
  RabbitMQ := TRabbitMQConsumer.Create('localhost', 5672, 'guest', 'guest');

  // S'abonner à l'exchange 'orders' avec la queue 'notification_service'
  RabbitMQ.SubscribeQueue('notification_service', 'orders', '',
    procedure(const AMessage: string)
    var
      JSONValue: TJSONValue;
      EventType: string;
      OrderId, CustomerId: Integer;
    begin
      JSONValue := TJSONObject.ParseJSONValue(AMessage);
      try
        EventType := JSONValue.GetValue<string>('event_type');

        if EventType = 'order_created' then
        begin
          OrderId := JSONValue.GetValue<Integer>('order_id');
          CustomerId := JSONValue.GetValue<Integer>('customer_id');

          // Envoyer un email de confirmation au client
          SendOrderConfirmationEmail(OrderId, CustomerId);
        end;
      finally
        JSONValue.Free;
      end;
    end);

  // Démarrer la consommation des messages
  RabbitMQ.Start;
end;
```

> **Note** : Pour utiliser RabbitMQ avec Delphi, vous aurez besoin d'une bibliothèque cliente comme DelphiRabbitMQ ou équivalent.

### API Gateway : le point d'entrée unique

Pour simplifier l'accès aux microservices depuis l'application cliente, on utilise souvent un API Gateway (passerelle d'API). C'est un service qui agit comme un point d'entrée unique et peut gérer :

- Routage des requêtes vers les bons services
- Authentification et autorisation
- Limitation de débit
- Mise en cache
- Agrégation de données

```pascal
// Exemple simplifié d'un API Gateway avec Horse
program APIGateway;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Jhonson,
  Horse.JWT, // Pour l'authentification JWT
  Horse.Compression, // Pour la compression
  Horse.Logger, // Pour la journalisation
  RESTClient;

begin
  // Middlewares
  THorse.Use(Compression()); // Compression des réponses
  THorse.Use(Jhonson()); // Support JSON
  THorse.Use(JWT('mon_secret')); // Vérification des tokens JWT
  THorse.Use(Logger('logs')); // Journalisation

  // Route pour les clients - redirige vers le service client
  THorse.Get('/api/clients',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      Response: string;
    begin
      // Appel au service client
      Response := CallClientService('GET', '/api/clients', '');
      Res.Send(Response);
    end);

  THorse.Get('/api/clients/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      ID: string;
      Response: string;
    begin
      ID := Req.Params['id'];
      // Appel au service client
      Response := CallClientService('GET', '/api/clients/' + ID, '');
      Res.Send(Response);
    end);

  // Route pour les produits - redirige vers le service produit
  THorse.Get('/api/products',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      Response: string;
    begin
      // Appel au service produit
      Response := CallProductService('GET', '/api/products', '');
      Res.Send(Response);
    end);

  // Route pour les commandes - redirige vers le service commande
  THorse.Post('/api/orders',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      Body: string;
      Response: string;
    begin
      Body := Req.Body;
      // Appel au service commande
      Response := CallOrderService('POST', '/api/orders', Body);
      Res.Send(Response);
    end);

  // Démarrer le gateway
  THorse.Listen(8080,
    procedure
    begin
      Writeln('API Gateway en écoute sur le port 8080...');
    end);
end.

// Fonction utilitaire pour appeler le service client
function CallClientService(const Method, Resource, Body: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('http://client-service:9001');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // Configurer la méthode
    if Method = 'GET' then
      RESTRequest.Method := TRESTRequestMethod.rmGET
    else if Method = 'POST' then
      RESTRequest.Method := TRESTRequestMethod.rmPOST
    else if Method = 'PUT' then
      RESTRequest.Method := TRESTRequestMethod.rmPUT
    else if Method = 'DELETE' then
      RESTRequest.Method := TRESTRequestMethod.rmDELETE;

    RESTRequest.Resource := Resource;

    if (Method = 'POST') or (Method = 'PUT') then
      RESTRequest.Body.JSONText := Body;

    // Exécuter la requête
    RESTRequest.Execute;

    Result := RESTResponse.Content;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

// Fonctions similaires pour les autres services...
```

## Déploiement et orchestration des microservices

Le déploiement de microservices peut être complexe. Heureusement, des outils comme Docker et Kubernetes peuvent grandement simplifier ce processus.

### Mise en conteneur avec Docker

Docker permet de créer des conteneurs légers pour chaque microservice, garantissant qu'ils s'exécutent de manière cohérente dans n'importe quel environnement.

Exemple de Dockerfile pour un microservice Delphi :

```dockerfile
# Étape de construction
FROM delphibase/delphi:10.4-sydney AS builder

# Copier les fichiers source
COPY . /app
WORKDIR /app

# Compiler l'application
RUN msbuild ClientService.dproj /t:Build /p:Config=Release

# Étape de déploiement
FROM ubuntu:20.04

# Copier l'exécutable compilé
COPY --from=builder /app/Win64/Release/ClientService /app/

# Exposer le port
EXPOSE 9001

# Démarrer le service
CMD ["/app/ClientService"]
```

### Orchestration avec Docker Compose

Pour un développement local, Docker Compose permet de définir et d'exécuter plusieurs conteneurs ensemble.

Exemple de fichier docker-compose.yml :

```yaml
version: '3'

services:
  client-service:
    build:
      context: ./ClientService
    ports:
      - "9001:9001"
    depends_on:
      - db
    environment:
      - DB_HOST=db
      - DB_USER=postgres
      - DB_PASSWORD=postgres
      - DB_NAME=clients

  product-service:
    build:
      context: ./ProductService
    ports:
      - "9002:9002"
    depends_on:
      - db
    environment:
      - DB_HOST=db
      - DB_USER=postgres
      - DB_PASSWORD=postgres
      - DB_NAME=products

  order-service:
    build:
      context: ./OrderService
    ports:
      - "9003:9003"
    depends_on:
      - db
      - rabbitmq
    environment:
      - DB_HOST=db
      - DB_USER=postgres
      - DB_PASSWORD=postgres
      - DB_NAME=orders
      - RABBITMQ_HOST=rabbitmq

  notification-service:
    build:
      context: ./NotificationService
    depends_on:
      - rabbitmq
    environment:
      - RABBITMQ_HOST=rabbitmq
      - SMTP_HOST=smtp.example.com
      - SMTP_PORT=587
      - SMTP_USER=user
      - SMTP_PASSWORD=password

  api-gateway:
    build:
      context: ./APIGateway
    ports:
      - "8080:8080"
    depends_on:
      - client-service
      - product-service
      - order-service

  db:
    image: postgres:13
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    volumes:
      - postgres-data:/var/lib/postgresql/data

  rabbitmq:
    image: rabbitmq:3-management
    ports:
      - "5672:5672"  # AMQP port
      - "15672:15672"  # Management UI

volumes:
  postgres-data:
```

### Orchestration avec Kubernetes

Pour les environnements de production, Kubernetes offre des fonctionnalités avancées pour l'orchestration de conteneurs.

Exemple de fichier de déploiement Kubernetes (client-service.yaml) :

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: client-service
spec:
  replicas: 3  # Exécuter 3 instances pour la haute disponibilité
  selector:
    matchLabels:
      app: client-service
  template:
    metadata:
      labels:
        app: client-service
    spec:
      containers:
      - name: client-service
        image: my-registry/client-service:latest
        ports:
        - containerPort: 9001
        env:
        - name: DB_HOST
          value: postgres
        - name: DB_USER
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: username
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: password
        - name: DB_NAME
          value: clients
        resources:
          limits:
            cpu: "500m"
            memory: "512Mi"
          requests:
            cpu: "100m"
            memory: "256Mi"
        livenessProbe:
          httpGet:
            path: /health
            port: 9001
          initialDelaySeconds: 30
          periodSeconds: 10
---
apiVersion: v1
kind: Service
metadata:
  name: client-service
spec:
  selector:
    app: client-service
  ports:
  - port: 9001
    targetPort: 9001
  type: ClusterIP
```

## Bonnes pratiques pour la conception de microservices avec Delphi

Pour réussir votre architecture microservices avec Delphi, voici quelques bonnes pratiques à suivre :

### 1. Dimensionnement approprié des services

Ne créez pas des services trop petits (nano-services) ni trop grands (mini-monolithes). Chaque service devrait être responsable d'une capacité métier cohérente.

**Bon dimensionnement** :
- Service de gestion des clients
- Service de gestion des produits
- Service de commandes

**Mauvais dimensionnement (trop petit)** :
- Service de validation d'adresse email
- Service de calcul de TVA
- Service de formatage de nom de client

### 2. Indépendance des données

Chaque service devrait idéalement posséder ses propres données. Si des services partagent une base de données, vous perdez l'indépendance et l'évolutivité.

```pascal
// Approche correcte: chaque service a sa propre base de données
// Service client
procedure TClientRepository.GetClient(const AId: Integer; out AClient: TClient);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection; // Connexion à la BD des clients
    Query.SQL.Text := 'SELECT * FROM Clients WHERE Id = :Id';
    Query.ParamByName('Id').AsInteger := AId;
    Query.Open;

    if not Query.IsEmpty then
    begin
      AClient.Id := Query.FieldByName('Id').AsInteger;
      AClient.Name := Query.FieldByName('Name').AsString;
      AClient.Email := Query.FieldByName('Email').AsString;
    end;
  finally
    Query.Free;
  end;
end;

// Service commande - obtient les données client via API
procedure TOrderService.GetClientInfo(const AClientId: Integer; out AClientInfo: TClientInfo);
var
  Response: IResponse;
begin
  Response := FHttpClient.Get(Format('http://client-service:9001/api/clients/%d', [AClientId]));
  if Response.StatusCode = 200 then
  begin
    var JSONObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
    try
      AClientInfo.Id := JSONObj.GetValue<Integer>('id');
      AClientInfo.Name := JSONObj.GetValue<string>('name');
      AClientInfo.Email := JSONObj.GetValue<string>('email');
    finally
      JSONObj.Free;
    end;
  end;
end;
```

### 3. Contrats d'API stables

Définissez des contrats d'API clairs et évitez de les modifier fréquemment. Utilisez le versionnement des API pour gérer les évolutions.

```pascal
// API versionnée
THorse.Get('/api/v1/clients',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Implémentation de la V1
  end);

THorse.Get('/api/v2/clients',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Implémentation de la V2 avec des fonctionnalités supplémentaires
  end);
```

### 4. Résilience et tolérance aux pannes

Les microservices doivent être conçus pour être résilients face aux défaillances d'autres services.

```pascal
function TOrderService.GetClientInfo(const AClientId: Integer): TClientInfo;
var
  Retries: Integer;
  Success: Boolean;
  Response: IResponse;
begin
  // Initialisation par défaut en cas d'échec
  Result.Id := AClientId;
  Result.Name := 'Client inconnu';
  Result.Email := '';

  Retries := 0;
  Success := False;

  while (not Success) and (Retries < 3) do
  begin
    try
      Response := FHttpClient.Get(Format('http://client-service:9001/api/clients/%d', [AClientId]));
      Success := Response.StatusCode = 200;

      if Success then
      begin
        var JSONObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
        try
          Result.Id := JSONObj.GetValue<Integer>('id');
          Result.Name := JSONObj.GetValue<string>('name');
          Result.Email := JSONObj.GetValue<string>('email');
        finally
          JSONObj.Free;
        end;
      end;
    except
      // En cas d'erreur, on réessaie
      Inc(Retries);
      Sleep(500 * Retries); // Backoff exponentiel
    end;
  end;

  // Utilisation d'un circuit breaker pour éviter d'appeler un service défaillant
  if not Success then
    FCircuitBreaker.RecordFailure('client-service');

  // Si nous avons des données en cache, on les utilise en cas d'échec
  if (not Success) and FCacheManager.HasData(Format('client:%d', [AClientId])) then
    Result := FCacheManager.GetData<TClientInfo>(Format('client:%d', [AClientId]));
end;
```

### 5. Surveillance et journalisation centralisées

Mettez en place une infrastructure de surveillance et de journalisation pour suivre le comportement de vos microservices.

```pascal
// Middleware de journalisation avec ELK (Elasticsearch, Logstash, Kibana)
THorse.Use(
  procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  var
    StartTime: TDateTime;
    ElapsedMS: Integer;
    LogEntry: TJSONObject;
  begin
    StartTime := Now;

    // Exécuter le gestionnaire de route
    Next();

    // Calculer le temps de réponse
    ElapsedMS := MilliSecondsBetween(Now, StartTime);

    // Créer l'entrée de journal
    LogEntry := TJSONObject.Create;
    try
      LogEntry.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', Now));
      LogEntry.AddPair('service', 'client-service');
      LogEntry.AddPair('method', Req.MethodAsString);
      LogEntry.AddPair('path', Req.Path);
      LogEntry.AddPair('status', TJSONNumber.Create(Res.Status));
      LogEntry.AddPair('elapsed_ms', TJSONNumber.Create(ElapsedMS));

      // Envoyer le journal à Logstash
      TLogstashSender.Instance.Send(LogEntry.ToString);
    finally
      LogEntry.Free;
    end;
  end);
```

### 6. Tests automatisés

Testez chaque microservice indépendamment et mettez en place des tests d'intégration pour vérifier leurs interactions.

```pascal
procedure TClientServiceTests.TestGetClient;
var
  Response: IResponse;
  JSONObj: TJSONObject;
begin
  // Préparer les données de test
  SetupTestClient(1, 'Test Client', 'test@example.com');

  // Appeler le service
  Response := FHttpClient.Get('http://localhost:9001/api/clients/1');

  // Vérifier la réponse
  Assert.AreEqual(200, Response.StatusCode);

  JSONObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
  try
    Assert.AreEqual(1, JSONObj.GetValue<Integer>('id'));
    Assert.AreEqual('Test Client', JSONObj.GetValue<string>('name'));
    Assert.AreEqual('test@example.com', JSONObj.GetValue<string>('email'));
  finally
    JSONObj.Free;
  end;
end;

procedure TOrderServiceIntegrationTests.TestCreateOrderWithClientInfo;
var
  OrderRequest, Response: TJSONObject;
  ClientResponse: IResponse;
begin
  // Configurer un client de test dans le service client
  SetupTestClient(99, 'Integration Test', 'integration@example.com');

  // Vérifier que le client existe
  ClientResponse := FHttpClient.Get('http://localhost:9001/api/clients/99');
  Assert.AreEqual(200, ClientResponse.StatusCode);

  // Créer une commande pour ce client
  OrderRequest := TJSONObject.Create;
  try
    OrderRequest.AddPair('client_id', TJSONNumber.Create(99));
    OrderRequest.AddPair('items', TJSONArray.Create(
      TJSONObject.Create
        .AddPair('product_id', TJSONNumber.Create(1))
        .AddPair('quantity', TJSONNumber.Create(2))));

    Response := TJSONObject.ParseJSONValue(
      FHttpClient.Post('http://localhost:9003/api/orders', OrderRequest.ToString).Content) as TJSONObject;

    try
      Assert.IsNotNull(Response);
      Assert.IsTrue(Response.GetValue<Integer>('id') > 0);
      Assert.AreEqual(99, Response.GetValue<Integer>('client_id'));

      // Vérifier que le service des commandes a bien récupéré les infos client
      Assert.AreEqual('Integration Test', Response.GetValue<string>('client_name'));
    finally
      Response.Free;
    end;
  finally
    OrderRequest.Free;
  end;
end;
```

## Défis courants et solutions

Passons en revue certains défis courants rencontrés lors de la mise en œuvre d'une architecture microservices et leurs solutions potentielles.

### 1. Cohérence des données

Dans une architecture microservices, maintenir la cohérence des données entre services peut être difficile.

**Solution** : Utiliser le pattern "Saga" pour les transactions distribuées.

```pascal
// Gestionnaire de saga pour une commande
type
  TOrderSaga = class
  private
    FOrderId: Integer;
    FClientId: Integer;
    FPaymentId: Integer;
    FShipmentId: Integer;
    FState: string; // 'Started', 'PaymentProcessed', 'Completed', 'Failed'

    procedure CompensatePayment;
    procedure CompensateInventory;
  public
    constructor Create(AOrderId, AClientId: Integer);

    // Étapes de la saga
    function ProcessPayment: Boolean;
    function ReserveInventory: Boolean;
    function CreateShipment: Boolean;

    // Gestion de l'état global
    procedure Execute;
    procedure Compensate;
  end;

procedure TOrderSaga.Execute;
begin
  FState := 'Started';

  // Exécuter les étapes de la saga
  if not ProcessPayment then
  begin
    FState := 'Failed';
    Compensate;
    Exit;
  end;

  FState := 'PaymentProcessed';

  if not ReserveInventory then
  begin
    FState := 'Failed';
    Compensate;
    Exit;
  end;

  if not CreateShipment then
  begin
    FState := 'Failed';
    Compensate;
    Exit;
  end;

  FState := 'Completed';
end;

procedure TOrderSaga.Compensate;
begin
  case FState of
    'PaymentProcessed':
      CompensatePayment;
    'InventoryReserved':
      begin
        CompensateInventory;
        CompensatePayment;
      end;
  end;
end;
```

### 2. Complexité de la découverte de services

Dans un environnement dynamique, les services doivent pouvoir se découvrir mutuellement.

**Solution** : Utiliser un service de registre et de découverte comme Consul ou etcd.

```pascal
// Enregistrement d'un service auprès de Consul
procedure TClientService.RegisterWithConsul;
var
  Registration: TJSONObject;
  Response: IResponse;
begin
  Registration := TJSONObject.Create;
  try
    Registration.AddPair('ID', 'client-service-1');
    Registration.AddPair('Name', 'client-service');
    Registration.AddPair('Address', GetHostAddress);
    Registration.AddPair('Port', TJSONNumber.Create(9001));

    // Définir un check de santé
    var Check := TJSONObject.Create;
    Check.AddPair('HTTP', Format('http://%s:9001/health', [GetHostAddress]));
    Check.AddPair('Interval', '10s');
    Registration.AddPair('Check', Check);

    // Enregistrer auprès de Consul
    Response := FHttpClient.Put('http://consul:8500/v1/agent/service/register', Registration.ToString);

    if Response.StatusCode <> 200 then
      raise Exception.Create('Erreur lors de l''enregistrement auprès de Consul: ' + Response.Content);
  finally
    Registration.Free;
  end;
end;

// Découverte d'un service depuis Consul
function TServiceDiscovery.GetServiceAddress(const AServiceName: string): string;
var
  Response: IResponse;
  JSONValue: TJSONValue;
  Services: TJSONArray;
begin
  Result := '';

  Response := FHttpClient.Get(Format('http://consul:8500/v1/health/service/%s?passing=true', [AServiceName]));

  if Response.StatusCode = 200 then
  begin
    JSONValue := TJSONObject.ParseJSONValue(Response.Content);
    try
      if (JSONValue is TJSONArray) and (TJSONArray(JSONValue).Count > 0) then
      begin
        Services := JSONValue as TJSONArray;

        // Prendre le premier service disponible (on pourrait implémenter un load balancing ici)
        var Service := Services.Items[0].GetValue<TJSONObject>('Service');
        var Address := Service.GetValue<string>('Address');
        var Port := Service.GetValue<Integer>('Port');

        Result := Format('%s:%d', [Address, Port]);
      end;
    finally
      JSONValue.Free;
    end;
  end;
end;
```

### 3. Déploiement et scaling complexes

La gestion d'un grand nombre de microservices peut devenir complexe.

**Solution** : Utiliser des outils d'orchestration comme Kubernetes et des pratiques DevOps.

```yaml
# Autoscaling avec Kubernetes
apiVersion: autoscaling/v2beta2
kind: HorizontalPodAutoscaler
metadata:
  name: order-service-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: order-service
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

### 4. Gestion de la sécurité

La sécurité devient plus complexe avec des services distribués.

**Solution** : Mettre en œuvre l'authentification et l'autorisation centralisées.

```pascal
// Middleware d'authentification JWT pour Horse
procedure ConfigureJWTAuth;
begin
  THorse.Use(
    HorseJWT('your_secret_key',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      var
        Token: string;
      begin
        // Extraire le token du header Authorization
        Token := Req.Headers['Authorization'];
        if Token.StartsWith('Bearer ') then
          Token := Token.Substring(7);

        // Vérifier et décoder le token
        try
          // Si la vérification échoue, une exception sera levée
          var Claims := TJWT.Verify(Token, 'your_secret_key');

          // Stocker les informations de l'utilisateur dans la requête
          Req.Session.Add('user_id', Claims.Subject);
          Req.Session.Add('user_role', Claims.Claims.GetValue<string>('role'));

          // Continuer le traitement
          Next();
        except
          on E: Exception do
          begin
            Res.Status(401);
            Res.Send('{"error": "Unauthorized - Invalid token"}');
          end;
        end;
      end));
end;

// Middleware d'autorisation
procedure AuthorizeAdmin(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Session.ContainsKey('user_role') and (Req.Session['user_role'] = 'admin') then
    Next()
  else
  begin
    Res.Status(403);
    Res.Send('{"error": "Forbidden - Admin access required"}');
  end;
end;

// Application des middlewares sur des routes
THorse.Get('/api/admin/users', AuthorizeAdmin,
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Traitement réservé aux administrateurs
  end);
```

## Migration d'une application monolithique vers des microservices

La migration d'une application monolithique Delphi existante vers une architecture microservices est généralement un processus progressif.

### Approche étrangler (Strangler Pattern)

Cette approche consiste à remplacer progressivement les fonctionnalités du monolithe par des microservices.

1. **Identifier les fonctionnalités** à extraire en priorité.
2. **Créer une façade API** devant le monolithe.
3. **Extraire progressivement** les fonctionnalités en microservices.
4. **Rediriger le trafic** de la façade vers les nouveaux microservices.
5. **Réduire progressivement** le monolithe.

```pascal
// Façade API qui redirige vers le monolithe ou les microservices
THorse.Get('/api/clients/:id',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    ClientId: Integer;
    UseNewService: Boolean;
  begin
    ClientId := Req.Params['id'].ToInteger;

    // Décider si on utilise le nouveau microservice ou le monolithe
    UseNewService := ShouldUseNewClientService(ClientId);

    if UseNewService then
    begin
      // Rediriger vers le nouveau microservice
      var Response := CallClientService('GET', '/api/clients/' + ClientId.ToString, '');
      Res.Send(Response);
    end
    else
    begin
      // Utiliser le monolithe
      var Client := MonolithicApp.GetClient(ClientId);
      Res.Send(ClientToJSON(Client));
    end;
  end);

// Fonction pour décider si on utilise le nouveau service
function ShouldUseNewClientService(AClientId: Integer): Boolean;
begin
  // Stratégies possibles:
  // 1. Basé sur l'ID (migration progressive par plages d'IDs)
  // Result := AClientId > 1000;

  // 2. Basé sur un pourcentage (canary release)
  // Result := Random(100) < 20; // 20% vers le nouveau service

  // 3. Basé sur des fonctionnalités (feature flags)
  Result := FFeatureFlags.IsEnabled('use-new-client-service');
end;
```

### Conseils pour la migration

1. **Commencez petit** : Choisissez une fonctionnalité périphérique avec peu de dépendances.
2. **Définissez des limites claires** : Identifiez les limites entre les différentes parties de votre application.
3. **Mettez en place une API Gateway** tôt dans le processus.
4. **Utilisez des bases de données séparées** pour les nouveaux microservices.
5. **Automatisez les tests** pour détecter rapidement les régressions.
6. **Surveillez attentivement** les performances pendant la transition.

## Conclusion

L'architecture microservices offre de nombreux avantages, mais elle introduit également une complexité supplémentaire. Pour les applications Delphi, elle peut être particulièrement avantageuse lorsque vous avez besoin de faire évoluer indépendamment différentes parties de votre application, ou lorsque vous avez une équipe importante travaillant sur le même projet.

Les technologies Delphi comme DataSnap, REST ou même les approches plus modernes comme gRPC permettent de mettre en œuvre efficacement des architectures distribuées. Cependant, il est important de bien comprendre les compromis et de ne pas adopter cette architecture sans une raison valable.

Pour les applications plus simples ou pour les équipes plus petites, une architecture monolithique bien conçue et modulaire peut être plus appropriée. N'oubliez pas que la complexité introduite par les microservices ne se justifie que si elle apporte des avantages concrets à votre cas d'utilisation spécifique.

En résumé, les microservices sont un outil puissant dans votre boîte à outils d'architecte, mais comme tout outil, ils doivent être utilisés à bon escient. Évaluez soigneusement vos besoins avant de vous lancer dans cette aventure, et avancez progressivement en commençant par les fondamentaux.

## Ressources complémentaires

- [Horse Framework](https://github.com/HashLoad/horse) - Un framework web minimaliste pour Delphi
- [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) - Un framework MVC pour Delphi avec support REST
- [Consul](https://www.consul.io/) - Outil de découverte de services
- [Docker](https://www.docker.com/) - Plateforme de conteneurisation
- [Kubernetes](https://kubernetes.io/) - Orchestrateur de conteneurs
- [RabbitMQ](https://www.rabbitmq.com/) - Broker de messages
- [JWT](https://jwt.io/) - JSON Web Tokens pour l'authentification
