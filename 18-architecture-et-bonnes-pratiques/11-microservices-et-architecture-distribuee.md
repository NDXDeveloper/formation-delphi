🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.11 Microservices et architecture distribuée

## Introduction

Dans cette section, nous allons découvrir les concepts de microservices et d'architecture distribuée, et comment les implémenter avec Delphi. Ces approches modernes permettent de créer des applications évolutives, maintenables et résilientes.

## Qu'est-ce qu'une architecture distribuée ?

### Définition simple

Une **architecture distribuée** est une approche où une application est divisée en plusieurs composants qui s'exécutent sur différentes machines ou processus, et qui communiquent entre eux via un réseau.

**Analogie** : Imaginez une grande entreprise. Au lieu d'avoir une seule personne qui fait tout, vous avez différents départements (comptabilité, ventes, ressources humaines) qui travaillent ensemble mais de manière autonome.

### Architecture monolithique vs distribuée

#### Architecture monolithique traditionnelle

Dans une application monolithique :
- Tout le code est dans une seule application
- Une seule base de données
- Déploiement complet à chaque mise à jour
- Si une partie plante, toute l'application peut être affectée

#### Architecture distribuée

Dans une architecture distribuée :
- Le code est séparé en plusieurs services indépendants
- Chaque service peut avoir sa propre base de données
- Déploiement indépendant de chaque service
- Si un service plante, les autres continuent de fonctionner

## Qu'est-ce qu'un microservice ?

### Définition

Un **microservice** est un petit service autonome qui :
- Accomplit une fonction métier spécifique
- Peut être développé, déployé et maintenu indépendamment
- Communique avec d'autres services via des API (généralement REST ou messages)
- Possède sa propre base de données si nécessaire

### Caractéristiques principales

**1. Responsabilité unique**
Chaque microservice se concentre sur une seule fonctionnalité métier.

Exemple : Dans une application e-commerce :
- Service de gestion des utilisateurs
- Service de catalogue de produits
- Service de panier d'achat
- Service de paiement
- Service de livraison

**2. Indépendance**
Chaque service peut être développé dans une technologie différente si nécessaire, et déployé séparément.

**3. Communication via API**
Les microservices communiquent principalement via HTTP/REST ou systèmes de messages.

**4. Résilience**
Si un service tombe en panne, les autres continuent de fonctionner (avec dégradation gracieuse).

## Avantages et inconvénients

### Avantages des microservices

**Évolutivité**
- Possibilité de faire évoluer uniquement les services qui en ont besoin
- Ajout de ressources ciblé sur les parties critiques

**Maintenabilité**
- Code plus petit et plus facile à comprendre
- Équipes peuvent travailler indépendamment sur différents services
- Mises à jour sans arrêter toute l'application

**Flexibilité technologique**
- Possibilité d'utiliser différentes technologies selon les besoins
- Facilite l'adoption de nouvelles technologies progressivement

**Déploiement continu**
- Mises à jour fréquentes et sans risque
- Rollback facile en cas de problème

### Inconvénients et défis

**Complexité accrue**
- Gestion de multiples services au lieu d'une seule application
- Nécessite une bonne infrastructure de monitoring

**Communication réseau**
- Latence due aux appels réseau entre services
- Gestion des pannes de réseau

**Gestion des données**
- Cohérence des données entre services
- Transactions distribuées complexes

**DevOps requis**
- Nécessite une bonne automatisation du déploiement
- Monitoring et logging distribués

## Implémentation avec Delphi

### Services REST avec Delphi

Delphi offre plusieurs approches pour créer des microservices REST.

#### Utilisation de TRESTServer

Delphi permet de créer facilement des serveurs REST qui exposent des API.

**Composants clés** :
- `TRESTServer` : Serveur HTTP qui gère les requêtes REST
- `TRESTRouter` : Routage des requêtes vers les bonnes méthodes
- `TRESTRequest` et `TRESTResponse` : Gestion des requêtes et réponses

#### Frameworks recommandés

**1. Horse (Framework Web léger)**

[Horse](https://github.com/HashLoad/horse) est un framework web open source pour Delphi inspiré de **Express.js** (Node.js). Il simplifie radicalement la création d'API REST.

Avantages :
- Syntaxe ultra-simple et lisible
- Système de middlewares (CORS, JWT, logger, etc.)
- Indépendant du backend HTTP (peut tourner sur Indy, DaemonAPI ou WebBroker)
- Compatible Windows et Linux
- Communauté active (LinkApi / HashLoad)

**2. DelphiMVCFramework (DMVC)**

[DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) — souvent abrégé **DMVCFramework** — par Daniele Teti, est un framework MVC complet pour applications web et services REST.

Avantages :
- Pattern MVC bien structuré (Controllers, Models, Views)
- Support de templates côté serveur (Mustache, TemplatePro)
- Génération automatique de documentation API (Swagger / OpenAPI)
- Sérialisation JSON puissante (jsondataobjects)
- Support de JWT, Server-Sent Events, WebSockets

**3. mORMot 2**

[mORMot 2](https://github.com/synopse/mORMot2) (Synopse) est un framework SOA complet et très performant. Plus complexe qu'Horse mais extrêmement riche.

Avantages :
- Performance exceptionnelle (l'un des serveurs HTTP les plus rapides du marché)
- ORM intégré (compatible avec la plupart des SGBD)
- Authentification, sérialisation, RPC, micro-services
- Mode interface-based publishing (le serveur expose des interfaces Delphi)

**4. Mars-Curiosity**

[Mars](https://github.com/andrea-magni/MARS) (Andrea Magni) est un framework REST pour Delphi inspiré de **JAX-RS** (Java).

Avantages :
- Architecture à base d'attributs (`[Path('/users')]`, `[GET]`)
- Support de l'injection de dépendances
- Intégration FireDAC

### Exemple concret : un microservice REST avec Horse

Voici un microservice complet d'API utilisateurs en moins de 50 lignes :

```pascal
program UsersService;

{$APPTYPE CONSOLE}

uses
  Horse,                       // Framework web
  Horse.Jhonson,               // Middleware JSON
  System.JSON,
  System.SysUtils;

procedure GetUsers(Req: THorseRequest; Res: THorseResponse);  
var  
  Users: TJSONArray;
begin
  Users := TJSONArray.Create;
  Users.Add(TJSONObject.Create
    .AddPair('id', '1')
    .AddPair('name', 'Alice'));
  Users.Add(TJSONObject.Create
    .AddPair('id', '2')
    .AddPair('name', 'Bob'));
  Res.Send<TJSONArray>(Users);
end;

procedure GetUserById(Req: THorseRequest; Res: THorseResponse);  
var  
  ID: string;
  User: TJSONObject;
begin
  ID := Req.Params['id'];
  User := TJSONObject.Create
    .AddPair('id', ID)
    .AddPair('name', 'User ' + ID);
  Res.Send<TJSONObject>(User);
end;

procedure HealthCheck(Req: THorseRequest; Res: THorseResponse);  
begin  
  Res.Send<TJSONObject>(
    TJSONObject.Create
      .AddPair('status', 'ok')
      .AddPair('service', 'users')
      .AddPair('version', '1.0.0'));
end;

begin
  // Middleware JSON activé pour toutes les routes
  THorse.Use(Jhonson);

  // Définition des routes
  THorse.Get('/health',     HealthCheck);
  THorse.Get('/users',      GetUsers);
  THorse.Get('/users/:id',  GetUserById);

  // Démarrage du serveur sur le port 9000
  THorse.Listen(9000,
    procedure(Horse: THorse)
    begin
      Writeln('Users microservice listening on port ', Horse.Port);
    end);
end.
```

**Test du service avec `curl`** :
```bash
curl http://localhost:9000/health
# → {"status":"ok","service":"users","version":"1.0.0"}

curl http://localhost:9000/users
# → [{"id":"1","name":"Alice"},{"id":"2","name":"Bob"}]

curl http://localhost:9000/users/42
# → {"id":"42","name":"User 42"}
```

> 💡 **Gestion mémoire** : grâce au middleware `Jhonson`, Horse sérialise l'objet JSON ET le libère automatiquement après l'envoi de la réponse. Vous n'avez donc pas besoin d'appeler `Users.Free` ou `User.Free` manuellement — sinon vous obtiendriez un double-free.

Ce squelette se complète facilement avec :
- Un **middleware JWT** (`Horse.Jwt`) pour sécuriser certaines routes
- Un **middleware CORS** (`Horse.CORS`) pour les appels cross-origin
- Un **middleware Logger** (`Horse.HandleException`) pour logger toutes les requêtes
- L'accès à la base via FireDAC (idéalement encapsulé dans un Repository)

Installation des frameworks : la méthode moderne en 2026 passe par **Boss** (https://github.com/HashLoad/boss), le package manager open source dédié à Delphi, ou par **GetIt Package Manager** intégré à RAD Studio 13.

### Structure d'un microservice Delphi

#### Organisation typique d'un projet

```
MonMicroservice/
├── src/
│   ├── Controllers/       # Points d'entrée API
│   ├── Models/            # Modèles de données
│   ├── Services/          # Logique métier
│   ├── Repositories/      # Accès aux données
│   └── Utils/             # Utilitaires
├── config/                # Configuration
├── tests/                 # Tests unitaires
└── docs/                  # Documentation
```

#### Principes de conception

**Séparation des préoccupations**

Chaque couche a une responsabilité claire :
- **Controllers** : Gèrent les requêtes HTTP, validation des entrées
- **Services** : Contiennent la logique métier
- **Repositories** : Accès et manipulation des données
- **Models** : Structures de données

**Exemple conceptuel** :

Un client fait une requête GET pour obtenir un produit :
1. Le **Controller** reçoit la requête
2. Il appelle le **Service** approprié
3. Le Service utilise le **Repository** pour accéder aux données
4. Le Repository interroge la base de données
5. Les données remontent dans le sens inverse
6. Le Controller retourne une réponse JSON au client

## Communication entre microservices

### Approches de communication

#### 1. Communication synchrone (REST)

**Principe** : Un service appelle directement un autre service et attend la réponse.

**Utilisation en Delphi** :
- Utilisation de `TRESTClient` pour effectuer des appels HTTP
- Sérialisation/désérialisation JSON avec les classes natives ou bibliothèques

**Avantages** :
- Simple à implémenter
- Réponse immédiate
- Facile à déboguer

**Inconvénients** :
- Couplage entre services
- Si un service est lent, il ralentit les autres
- Gestion des pannes délicate

#### 2. Communication asynchrone (Messages)

**Principe** : Les services communiquent via un système de messages (broker). Un service envoie un message sans attendre de réponse immédiate.

**Technologies courantes** :
- RabbitMQ
- Apache Kafka
- Redis Pub/Sub

**Avantages** :
- Découplage des services
- Meilleure résilience
- Gestion de la charge avec files d'attente

**Inconvénients** :
- Plus complexe à mettre en place
- Infrastructure supplémentaire nécessaire

### Patterns de communication

#### API Gateway

**Concept** : Point d'entrée unique qui route les requêtes vers les microservices appropriés.

**Rôle** :
- Authentification centralisée
- Routage des requêtes
- Agrégation de réponses
- Rate limiting
- Cache

**Avec Delphi** : Vous pouvez créer un service Delphi qui agit comme API Gateway en utilisant Horse ou DMVC.

#### Service Discovery

**Problème** : Comment un service trouve-t-il les autres services dans un environnement dynamique ?

**Solutions** :
- Consul
- Eureka
- Etcd

Les services s'enregistrent automatiquement et peuvent découvrir les autres services disponibles.

## Gestion des données

### Base de données par service

**Principe fondamental** : Chaque microservice devrait avoir sa propre base de données.

**Pourquoi ?**
- Indépendance : Modifications du schéma sans affecter les autres
- Évolutivité : Choix du type de base adapté (SQL, NoSQL)
- Isolation : Pas de couplage via la base de données

**Avec Delphi et FireDAC** :
Chaque microservice peut utiliser FireDAC pour se connecter à sa propre base :
- MySQL/MariaDB pour un service
- PostgreSQL pour un autre
- MongoDB pour un troisième

### Cohérence des données

#### Problème

Comment maintenir la cohérence quand les données sont réparties sur plusieurs bases ?

#### Pattern : Saga

**Définition** : Une saga est une séquence de transactions locales. Si une étape échoue, des transactions compensatoires annulent les changements.

**Exemple : Commande e-commerce**

1. Service Commande : Créer la commande
2. Service Paiement : Débiter le client
3. Service Stock : Réserver les produits
4. Service Livraison : Créer l'expédition

Si l'étape 3 échoue :
- Compensation étape 2 : Rembourser le client
- Compensation étape 1 : Annuler la commande

#### Event Sourcing

**Concept** : Au lieu de stocker l'état actuel, on stocke tous les événements qui ont conduit à cet état.

**Avantages** :
- Historique complet
- Possibilité de reconstruire l'état à n'importe quel moment
- Audit trail naturel

## Sécurité dans les microservices

### Authentification et autorisation

#### JWT (JSON Web Tokens)

**Principe** : Token signé contenant les informations d'authentification, échangé entre services.

**Workflow typique** :
1. L'utilisateur s'authentifie auprès d'un service d'authentification
2. Il reçoit un JWT
3. Ce token est inclus dans chaque requête aux autres services
4. Chaque service valide le token

**Implémentation en Delphi** :
La bibliothèque de référence est **delphi-jose-jwt** de Paolo Rossi (https://github.com/paolo-rossi/delphi-jose-jwt). Elle implémente la suite **JOSE** complète :
- **JWT** (JSON Web Token) — le token lui-même
- **JWS** (JSON Web Signature) — signature HMAC (HS256/384/512) ou RSA (RS256/384/512) ou ECDSA (ES256/384/512)
- **JWK** (JSON Web Key) — représentation des clés
- **JWA** (JSON Web Algorithms) — catalogue d'algorithmes

Pour Horse, le middleware `Horse.JWT` (du même écosystème HashLoad) encapsule directement delphi-jose-jwt :

```pascal
uses Horse, Horse.JWT, JOSE.Core.Builder, JOSE.Core.JWT;

THorse.Use(HorseJWT('MaCleSecrete'));  // Toutes les routes derrière sont protégées
```

#### OAuth2

Protocole d'autorisation standard pour les API.

**Scénarios** :
- Délégation d'accès à des services tiers
- Single Sign-On (SSO)

### Sécurisation des communications

**HTTPS obligatoire**
Toutes les communications entre services doivent être chiffrées.

**Certificats mutuels (mTLS)**
Pour les communications service-à-service, l'utilisation de certificats clients/serveurs ajoute une couche de sécurité.

## Monitoring et observabilité

### Importance du monitoring

Dans une architecture distribuée, il est crucial de :
- Savoir si tous les services fonctionnent
- Identifier rapidement les problèmes
- Comprendre les flux de requêtes

### Logging distribué

**Problème** : Les logs sont éparpillés sur plusieurs services.

**Solution** : Centralisation des logs avec des outils comme :
- ELK Stack (Elasticsearch, Logstash, Kibana)
- Graylog
- Splunk

**Avec Delphi** :
- Utiliser des bibliothèques de logging (Log4D, Spring4D)
- Envoyer les logs vers un collecteur centralisé
- Format structuré (JSON) pour faciliter l'analyse

### Tracing distribué

**Concept** : Suivre une requête à travers tous les services qu'elle traverse.

**Correlation ID** : Identifiant unique propagé à travers tous les services pour une même requête.

**Outils** :
- Jaeger
- Zipkin
- OpenTelemetry

### Health Checks

Chaque service doit exposer un endpoint de health check :
- `/health` : Statut du service (up/down)
- `/ready` : Le service est-il prêt à traiter des requêtes ?

**Implémentation simple en Delphi** :
Un endpoint qui retourne un JSON avec le statut du service et de ses dépendances (base de données, services externes).

## Déploiement et orchestration

### Conteneurisation avec Docker

**Principe** : Chaque microservice est empaqueté dans un conteneur Docker.

**Avantages** :
- Environnement isolé et reproductible
- Facilite le déploiement
- Portabilité entre environnements

**Delphi et Docker** : Compilez votre service en cible **Linux64** (binaires ELF via la toolchain LLVM intégrée) depuis Delphi — possible depuis Delphi 10.2 Tokyo en Enterprise/Architect. Pour une interface graphique Linux, l'add-on tiers **FMXLinux** (KSDev) est nécessaire ; pour un microservice console/REST en revanche, la RTL standard suffit. Empaquetez ensuite dans une image Docker minimale.

**Exemple de Dockerfile pour un microservice Horse (Linux64)** :

```dockerfile
# Étape 1 : image de base légère
FROM debian:bookworm-slim

# Bibliothèques runtime nécessaires aux binaires Delphi Linux
# (libstdc++, libc6, libgcc — souvent déjà présentes — et OpenSSL pour HTTPS)
RUN apt-get update && apt-get install -y --no-install-recommends \
        libssl3 \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Étape 2 : copie du binaire compilé
WORKDIR /app  
COPY ./Linux64/Release/UsersService /app/UsersService  
RUN chmod +x /app/UsersService  

# Métadonnées
EXPOSE 9000  
HEALTHCHECK --interval=30s --timeout=3s \  
  CMD curl -fsS http://localhost:9000/health || exit 1

# Utilisateur non-root pour la sécurité
RUN useradd -m -s /bin/false appuser  
USER appuser  

CMD ["/app/UsersService"]
```

**Construction et lancement** :

```bash
# Compiler en Linux64 depuis Delphi (ou via msbuild)
msbuild UsersService.dproj /p:Platform=Linux64 /p:Config=Release

# Construire l'image
docker build -t users-service:1.0.0 .

# Lancer le conteneur
docker run -d -p 9000:9000 --name users users-service:1.0.0

# Tester
curl http://localhost:9000/health
```

**Compose multi-services** (`docker-compose.yml`) :

```yaml
services:
  users:
    image: users-service:1.0.0
    ports: ["9000:9000"]
  orders:
    image: orders-service:1.0.0
    ports: ["9001:9001"]
    depends_on: [users, postgres]
  postgres:
    image: postgres:17-alpine
    environment:
      POSTGRES_PASSWORD_FILE: /run/secrets/db_pwd
    secrets: [db_pwd]
secrets:
  db_pwd:
    file: ./secrets/db_password.txt
```

### Orchestration avec Kubernetes

**Kubernetes (K8s)** : Plateforme d'orchestration de conteneurs qui gère :
- Déploiement automatisé
- Scaling automatique
- Répartition de charge
- Auto-healing (redémarrage automatique)
- Rolling updates

### CI/CD pour microservices

**Intégration continue (CI)** :
- Tests automatiques à chaque commit
- Build automatique

**Déploiement continu (CD)** :
- Déploiement automatique en production
- Possibilité de rollback rapide

**Outils** :
- GitLab CI/CD
- Jenkins
- GitHub Actions

## Patterns et bonnes pratiques

### Circuit Breaker

**Problème** : Un service défaillant peut ralentir tout le système.

**Solution** : Détection automatique des pannes et court-circuitage des appels vers un service défaillant.

**États** :
- **Fermé** : Fonctionnement normal
- **Ouvert** : Service inaccessible, retour immédiat d'erreur
- **Semi-ouvert** : Test périodique pour voir si le service est de nouveau disponible

```
        ┌──────────────────┐
        │     FERMÉ        │  Appels OK, on compte les échecs
        │    (Closed)      │
        └───────┬──────────┘
                │ Seuil d'échecs atteint
                ↓
        ┌──────────────────┐
        │     OUVERT       │  Tous les appels échouent immédiatement
        │     (Open)       │  Pendant une durée fixée (ex. 30s)
        └───────┬──────────┘
                │ Délai expiré
                ↓
        ┌──────────────────┐
        │   SEMI-OUVERT    │  Un appel test passe :
        │   (Half-Open)    │  Succès → FERMÉ, Échec → OUVERT
        └──────────────────┘
```

**Squelette Delphi** :

```pascal
type
  TCircuitState = (csClosed, csOpen, csHalfOpen);

  TCircuitBreaker = class
  private
    FState: TCircuitState;
    FFailureCount: Integer;
    FFailureThreshold: Integer;     // ex: 5 échecs avant ouverture
    FOpenedAt: TDateTime;
    FCooldownSec: Integer;          // ex: 30 secondes
    procedure RecordSuccess;
    procedure RecordFailure;
    function ShouldAttemptReset: Boolean;
  public
    constructor Create(FailureThreshold: Integer = 5; CooldownSec: Integer = 30);
    function Execute(Op: TFunc<Boolean>): Boolean;
  end;

function TCircuitBreaker.Execute(Op: TFunc<Boolean>): Boolean;  
begin  
  if (FState = csOpen) and not ShouldAttemptReset then
    raise Exception.Create('Circuit ouvert — appel court-circuité');

  if FState = csOpen then
    FState := csHalfOpen;     // Tentative de test

  try
    Result := Op();
    if Result then RecordSuccess else RecordFailure;
  except
    RecordFailure;
    raise;
  end;
end;
```

Pour une implémentation complète, voir des librairies comme **Spring4D** ou **mORMot** qui proposent des composants de résilience.

### Retry Pattern

**Principe** : Réessayer automatiquement une opération qui a échoué, avec un délai exponentiel (« exponential backoff ») entre les tentatives, et idéalement un **jitter** (aléatoire) pour éviter le « thundering herd ».

**Attention** : Ne pas abuser pour éviter d'aggraver un problème. **Ne retentez que les erreurs transitoires** (timeout réseau, 503 Service Unavailable…), JAMAIS les erreurs logiques (400 Bad Request, 401 Unauthorized…).

**Exemple Delphi** :

```pascal
type
  TRetryableOperation = reference to function: Boolean;

function ExecuteWithRetry(Op: TRetryableOperation;
  MaxAttempts: Integer = 3; InitialDelayMs: Integer = 100): Boolean;
var
  Attempt: Integer;
  Delay: Integer;
  JitterMax: Integer;
begin
  Delay := InitialDelayMs;
  for Attempt := 1 to MaxAttempts do
  begin
    try
      if Op() then
        Exit(True);            // Succès → on sort
    except
      on E: Exception do
        if Attempt = MaxAttempts then
          raise;                // Dernière tentative : on propage
    end;

    // Backoff exponentiel + jitter aléatoire de ±20 %
    // ⚠ Random(0) lève une exception : on garantit JitterMax >= 1.
    JitterMax := Delay div 5;
    if JitterMax < 1 then JitterMax := 1;
    Sleep(Delay + Random(JitterMax));
    Delay := Delay * 2;         // 100ms → 200ms → 400ms → 800ms…
  end;
  Result := False;
end;

// Utilisation :
// ExecuteWithRetry(function: Boolean begin Result := CallRemoteAPI; end);
```

> 🔒 **Thread-safety** : la fonction `Random` de la RTL Delphi utilise un état partagé via la variable globale `RandSeed`. Dans un environnement multi-thread (typique des microservices), appelez `Randomize` une seule fois au démarrage, ou mieux, utilisez `System.Hash.THashBobJenkins` pour des nombres pseudo-aléatoires thread-safe, ou `BCryptGenRandom` (Windows) pour de la vraie entropie.

### Timeout

**Règle** : Toujours définir des timeouts pour les appels entre services.

**Pourquoi ?** : Éviter qu'un service lent bloque tout le système.

### Versioning d'API

**Importance** : Permet de faire évoluer les services sans casser les clients existants.

**Approches** :
- Versioning dans l'URL : `/api/v1/users`, `/api/v2/users`
- Versioning via headers HTTP
- Gestion rétrocompatible des changements

### Cache distribué

**Utilisation** : Redis ou Memcached pour partager un cache entre services.

**Bénéfices** :
- Réduction de la charge sur les bases de données
- Amélioration des performances
- Partage d'état entre instances

## Quand utiliser les microservices ?

### Cas favorables

**Applications complexes et en croissance**
- Application avec de nombreuses fonctionnalités
- Équipes multiples travaillant sur des domaines différents

**Besoins d'évolutivité**
- Certaines parties de l'application nécessitent plus de ressources
- Trafic variable selon les fonctionnalités

**Cycles de release indépendants**
- Besoin de déployer des fonctionnalités sans affecter le reste
- Mise à jour fréquente de certaines parties

### Cas où rester monolithique

**Petites applications**
Si votre application est simple et petite, un monolithe est plus approprié.

**Équipe réduite**
Une petite équipe aura du mal à gérer de nombreux microservices.

**Début de projet**
Il est souvent préférable de commencer par un monolithe et migrer progressivement vers des microservices si nécessaire.

**Manque de compétences DevOps**
Les microservices nécessitent une infrastructure et des compétences spécifiques.

## Migration progressive

### Approche Strangler Fig

**Concept** : Pattern popularisé par Martin Fowler (2004) inspiré du **figuier étrangleur**, un arbre tropical qui pousse autour d'un hôte jusqu'à le remplacer complètement. Appliqué au logiciel : on entoure progressivement un monolithe de microservices jusqu'à le remplacer.

**Étapes** :
1. Identifier une fonctionnalité à extraire
2. Créer un nouveau microservice
3. Router les nouvelles requêtes vers le microservice
4. Migrer progressivement les données
5. Supprimer l'ancien code du monolithe

**Schéma d'évolution** :

```
Phase 1 :    [Monolithe complet]
                    ↓
Phase 2 :    [Monolithe] + [µService 1] (via routeur)
                    ↓
Phase 3 :    [Monolithe réduit] + [µService 1] + [µService 2]
                    ↓
Phase N :    [µService 1] + [µService 2] + ... + [µService N]
             (le monolithe a disparu, « étranglé »)
```

**Avantages** :
- Migration **sans Big Bang** : pas d'arrêt prolongé
- Possibilité de **rollback** facile à chaque étape
- Validation continue en production
- Acquisition progressive des compétences DevOps par l'équipe

**Outil clé** : un **routeur de requêtes** (reverse proxy comme Nginx, Traefik ou un service API Gateway) qui décide pour chaque endpoint s'il route vers le monolithe ou vers le microservice extrait.

### Modularité dans le monolithe

Avant de passer aux microservices, assurez-vous que votre code monolithique est bien structuré en modules avec des interfaces claires. Cette étape s'appelle parfois **« monolithe modulaire »** (modular monolith) — c'est une architecture intermédiaire très valable en soi, et un excellent tremplin si vous décidez plus tard d'extraire des microservices.

## Outils et ressources pour Delphi

### Frameworks web Delphi

- **Horse** : Simple et léger, syntaxe à la Express.js
- **DelphiMVCFramework (DMVC)** : Complet avec pattern MVC, Swagger intégré
- **mORMot 2** : Hautes performances, SOA, ORM intégré
- **Mars-Curiosity** : REST avancé avec injection de dépendances

### Bibliothèques utiles

- **Spring4D** : Injection de dépendances et patterns (https://bitbucket.org/sglienke/spring4d)
- **DUnitX** : Tests unitaires (https://github.com/VSoftTechnologies/DUnitX)
- **delphi-jose-jwt** (Paolo Rossi) : JWT pour authentification
- **Boss** : Package manager pour Delphi (équivalent npm pour Node.js)
- **GetIt Package Manager** : intégré à RAD Studio 13 Florence

### Communauté et documentation

- **Embarcadero DocWiki** : https://docwiki.embarcadero.com
- **Delphi-PRAXiS** : forum communautaire de référence
- **r/delphi** : communauté Reddit
- **GitHub** : tag `delphi`, nombreux exemples open source
- **Blogs** : Marco Cantù, Daniele Teti, Andrea Magni, Paolo Rossi, etc.

## Conclusion

Les microservices et l'architecture distribuée représentent une évolution majeure dans la conception d'applications. Bien qu'elles apportent de la complexité, elles offrent aussi une flexibilité et une évolutivité précieuses pour les applications d'envergure.

**Points clés à retenir** :

1. **Les microservices ne sont pas toujours la solution** : Évaluez vos besoins réels
2. **Commencez simple** : Pas besoin de tout distribuer dès le départ
3. **L'infrastructure compte** : Investissez dans le monitoring et l'automatisation
4. **Delphi est adapté** : Avec les bons frameworks, Delphi peut créer d'excellents microservices
5. **Pensez communication et résilience** : Ce sont les piliers d'une architecture distribuée réussie

L'architecture distribuée avec Delphi ouvre de nouvelles perspectives pour créer des applications modernes, scalables et maintenables, tout en bénéficiant de la puissance et de la maturité de l'écosystème Delphi.

⏭️ [Projets avancés](/19-projets-avances/README.md)
