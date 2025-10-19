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

Horse est un framework web populaire pour Delphi qui simplifie la création d'API REST.

Avantages :
- Syntaxe simple et intuitive
- Middleware support
- Documentation claire
- Active communauté

**2. Mars-Curiosity**

Framework REST pour Delphi inspiré de JAX-RS (Java).

Avantages :
- Architecture par attributs
- Support de l'injection de dépendances
- Intégration FireDAC

**3. DMVC (Delphi MVC Framework)**

Framework MVC complet pour applications web et services REST.

Avantages :
- Pattern MVC bien structuré
- Support de templates
- Générateur de documentation API

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
Plusieurs bibliothèques Delphi existent pour gérer les JWT (jose-jwt, delphi-jose-jwt).

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

**Delphi et Docker** :
Vous pouvez créer des images Docker pour vos applications Delphi Linux ou Windows.

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

### Retry Pattern

**Principe** : Réessayer automatiquement une opération qui a échoué, avec un délai exponentiel entre les tentatives.

**Attention** : Ne pas abuser pour éviter d'aggraver un problème.

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

**Concept** : Remplacer progressivement un monolithe par des microservices.

**Étapes** :
1. Identifier une fonctionnalité à extraire
2. Créer un nouveau microservice
3. Router les nouvelles requêtes vers le microservice
4. Migrer progressivement les données
5. Supprimer l'ancien code du monolithe

### Modularité dans le monolithe

Avant de passer aux microservices, assurez-vous que votre code monolithique est bien structuré en modules avec des interfaces claires.

## Outils et ressources pour Delphi

### Frameworks web Delphi

- **Horse** : Simple et léger
- **DMVC Framework** : Complet avec pattern MVC
- **Mars-Curiosity** : REST avancé avec DI

### Bibliothèques utiles

- **mORMot 2** : Framework complet pour serveurs hautes performances
- **Spring4D** : Injection de dépendances et patterns
- **DUnitX** : Tests unitaires

### Communauté et documentation

- Forums Delphi : Entraide et conseils
- GitHub : Nombreux exemples de projets
- Blogs spécialisés : Retours d'expérience

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
