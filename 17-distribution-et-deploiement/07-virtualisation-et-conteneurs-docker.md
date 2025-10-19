🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.7 Virtualisation et conteneurs Docker

## Introduction

Imaginez que vous vouliez tester votre application sur différentes versions de Windows, ou que vous développiez un service web qui doit fonctionner de manière identique sur votre machine de développement, sur le serveur de test et en production. Comment garantir que l'environnement sera toujours le même partout ?

C'est là qu'interviennent la **virtualisation** et les **conteneurs**. Ces technologies permettent de créer des environnements isolés et reproductibles pour exécuter vos applications. Bien que Delphi soit principalement utilisé pour créer des applications desktop Windows, ces concepts deviennent de plus en plus importants, notamment pour :

- Les services web et API REST développés avec Delphi
- Les applications serveur et services Windows
- Les applications Linux avec FMXLinux
- Les environnements de test automatisés
- Le déploiement dans le cloud

## Qu'est-ce que la virtualisation ?

### Définition simple

La **virtualisation** consiste à créer une version virtuelle d'un ordinateur complet (système d'exploitation, mémoire, disque dur) à l'intérieur de votre ordinateur physique.

**Analogie** : C'est comme avoir un ordinateur dans un ordinateur, chacun avec son propre système d'exploitation et ses propres applications, mais partageant le matériel physique.

### Comment ça fonctionne ?

```
[Ordinateur Physique]
    ↓
[Hyperviseur] (VirtualBox, VMware, Hyper-V)
    ↓
[Machine Virtuelle 1]  [Machine Virtuelle 2]  [Machine Virtuelle 3]
  Windows 10            Windows 11              Linux Ubuntu
```

Chaque **machine virtuelle (VM)** est complètement isolée et possède :
- Son propre système d'exploitation
- Sa propre mémoire allouée
- Son propre disque dur virtuel
- Ses propres applications

### Avantages de la virtualisation

**1. Isolation complète**
- Une VM qui plante n'affecte pas les autres
- Idéal pour tester des configurations dangereuses

**2. Environnements multiples**
- Testez sur Windows 7, 10, 11 sans avoir plusieurs PC
- Testez différentes versions de serveurs

**3. Snapshots (instantanés)**
- Sauvegardez l'état exact d'une VM
- Revenez en arrière en cas de problème

**4. Portabilité**
- Copiez une VM complète d'une machine à une autre
- Partagez des environnements de test avec votre équipe

### Inconvénients

**1. Consommation de ressources**
- Chaque VM nécessite sa propre allocation de mémoire (2-4 Go minimum)
- Occupe beaucoup d'espace disque (20-50 Go par VM)

**2. Performance**
- Légère baisse de performance par rapport à une machine physique
- Démarrage plus lent (1-2 minutes par VM)

**3. Complexité**
- Nécessite de gérer plusieurs systèmes d'exploitation
- Configuration réseau parfois délicate

### Solutions de virtualisation populaires

#### VirtualBox (Gratuit)
- **Éditeur** : Oracle
- **Plateformes** : Windows, macOS, Linux
- **Avantages** : Gratuit, simple, bien documenté
- **Idéal pour** : Tests et développement

#### VMware Workstation (Payant) / VMware Player (Gratuit)
- **Éditeur** : VMware (Broadcom)
- **Avantages** : Performances excellentes, fonctionnalités avancées
- **Idéal pour** : Environnements professionnels

#### Hyper-V (Gratuit avec Windows)
- **Éditeur** : Microsoft
- **Avantages** : Intégré à Windows, performant
- **Inconvénient** : Uniquement sur Windows Pro/Entreprise
- **Idéal pour** : Développeurs Windows

## Utiliser la virtualisation pour tester des applications Delphi

### Cas d'usage typiques

#### 1. Tester sur différentes versions de Windows

Créez des VM pour :
- Windows 10 (différentes versions : 21H2, 22H2)
- Windows 11
- Windows Server 2019/2022

**Avantage** : Vérifiez que votre application fonctionne partout sans posséder plusieurs PC.

#### 2. Environnement de test "propre"

**Problème** : Votre machine de développement a beaucoup de logiciels installés (Delphi, drivers, composants). Votre application fonctionnera-t-elle sur une machine utilisateur "vierge" ?

**Solution** : Testez dans une VM fraîchement installée qui simule une vraie machine utilisateur.

#### 3. Tests de compatibilité

Testez votre application avec :
- Différentes versions de .NET Framework
- Différentes versions de drivers de base de données
- Différentes configurations régionales (langue, format de date)

### Exemple pratique : Créer une VM de test Windows

**Avec VirtualBox** :

1. **Télécharger VirtualBox**
   - Site : https://www.virtualbox.org/
   - Gratuit et open source

2. **Télécharger une image Windows**
   - Microsoft propose des VM de développement gratuites (90 jours)
   - Site : https://developer.microsoft.com/windows/downloads/virtual-machines/

3. **Créer la VM**
   ```
   - Cliquez sur "Nouvelle"
   - Nom : "Windows 11 Test"
   - Type : Microsoft Windows
   - Version : Windows 11 (64-bit)
   - Mémoire : 4096 Mo (4 Go minimum)
   - Disque dur : 50 Go
   ```

4. **Installer Windows**
   - Démarrez la VM
   - Suivez l'assistant d'installation Windows

5. **Installer les Guest Additions**
   - Améliore les performances
   - Permet le partage de dossiers
   - Menu : Périphériques → Insérer l'image CD des Additions invité

6. **Créer un dossier partagé**
   - VM → Configuration → Dossiers partagés
   - Ajoutez le dossier de votre projet Delphi
   - Accessible depuis `\\VBOXSVR\NomDuPartage` dans Windows

7. **Tester votre application**
   - Copiez votre installateur dans la VM
   - Installez et testez
   - Vérifiez qu'il n'y a pas de DLL manquantes

8. **Créer un snapshot**
   - Menu : Machine → Prendre un instantané
   - Nom : "Installation propre de Windows"
   - Vous pourrez revenir à cet état à tout moment

## Qu'est-ce que Docker et les conteneurs ?

### Différence avec la virtualisation

Si la virtualisation crée des ordinateurs complets virtuels, les **conteneurs** créent des environnements isolés qui partagent le même système d'exploitation.

**Comparaison** :

```
VIRTUALISATION                    CONTENEURISATION
──────────────                    ────────────────
[Ordinateur Physique]             [Ordinateur Physique]
    ↓                                 ↓
[Hyperviseur]                     [Système d'exploitation]
    ↓                                 ↓
[VM1: OS complet]                 [Docker Engine]
[VM2: OS complet]                     ↓
[VM3: OS complet]                 [Conteneur 1] [Conteneur 2] [Conteneur 3]
                                  (partagent le même OS)
```

### Avantages des conteneurs

**1. Légers**
- Démarrage en quelques secondes (vs minutes pour une VM)
- Occupent beaucoup moins d'espace (Mo vs Go)
- Partagent le noyau du système hôte

**2. Portables**
- "Build once, run anywhere" (compilez une fois, exécutez partout)
- Même comportement en développement et en production

**3. Reproductibles**
- L'environnement est défini dans un fichier (Dockerfile)
- Facile à versionner et partager

**4. Efficaces**
- Plusieurs dizaines de conteneurs sur une machine
- Consommation de ressources minimale

### Inconvénients des conteneurs

**1. Partage du noyau OS**
- Les conteneurs Linux ne peuvent tourner que sur Linux (ou via WSL2 sur Windows)
- Les conteneurs Windows ne peuvent tourner que sur Windows Server

**2. Isolation moindre**
- Moins isolé qu'une VM complète
- Partage de ressources avec l'hôte

**3. Complexité initiale**
- Courbe d'apprentissage pour Docker
- Concepts nouveaux (images, conteneurs, volumes, réseaux)

## Docker : Les concepts de base

### Qu'est-ce que Docker ?

**Docker** est la plateforme la plus populaire pour créer et gérer des conteneurs. C'est devenu un standard dans l'industrie.

### Concepts clés

#### Image

Une **image** est un modèle en lecture seule contenant :
- Un système d'exploitation de base
- Votre application
- Toutes les dépendances nécessaires

**Analogie** : C'est comme un DVD d'installation, mais pour un conteneur.

#### Conteneur

Un **conteneur** est une instance en cours d'exécution d'une image.

**Analogie** : Si l'image est le DVD, le conteneur est le programme installé et en cours d'exécution.

#### Dockerfile

Un **Dockerfile** est un fichier texte contenant les instructions pour construire une image.

**Exemple simple** :
```dockerfile
FROM ubuntu:22.04
COPY MonApplication /app/
CMD ["/app/MonApplication"]
```

#### Docker Hub

**Docker Hub** est un registre public d'images Docker. Vous pouvez :
- Télécharger des images existantes (MySQL, Nginx, Ubuntu, etc.)
- Publier vos propres images

### Installation de Docker

#### Sur Windows

**Docker Desktop pour Windows** :

1. **Télécharger** : https://www.docker.com/products/docker-desktop/
2. **Installer** : Suivez l'assistant
3. **Activer WSL2** : Docker l'utilisera pour exécuter des conteneurs Linux
4. **Redémarrer** : Docker sera disponible après redémarrage

**Configuration minimale** :
- Windows 10/11 Pro, Entreprise ou Éducation (64-bit)
- 4 Go de RAM
- Virtualisation activée dans le BIOS

#### Sur Linux

```bash
# Ubuntu/Debian
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER
```

Redémarrez votre session pour que les changements prennent effet.

### Vérifier l'installation

```bash
docker --version
# Docker version 24.0.7, build afdd53b

docker run hello-world
# Si tout fonctionne, vous verrez un message de bienvenue
```

## Applications Delphi et Docker

### Pourquoi utiliser Docker avec Delphi ?

Docker est particulièrement utile pour certains types d'applications Delphi :

#### 1. Services Web et API REST

Si vous développez des services web avec **WebBroker** ou **DataSnap** :
- Déployez facilement sur n'importe quel serveur Linux
- Scalabilité horizontale (plusieurs instances)
- Isolation des différentes versions

#### 2. Applications serveur Linux (FMXLinux)

Avec Delphi 13 et FMXLinux :
- Créez des applications graphiques Linux
- Déployez-les dans des conteneurs
- Testez sur différentes distributions

#### 3. Services Windows (background)

Services Windows sans interface graphique :
- Traitement de données
- Tâches planifiées
- Intégration de systèmes

#### 4. Bases de données et environnements de test

Docker est excellent pour créer des environnements de test :
- MySQL, PostgreSQL, SQL Server dans des conteneurs
- Environnements isolés pour chaque développeur
- Supprimez et recréez en quelques secondes

### Conteneuriser une application serveur Delphi (Linux)

Voici un exemple complet de déploiement d'un service REST Delphi dans Docker.

#### Étape 1 : Créer l'application Delphi

**Application console simple qui expose une API REST** :

```pascal
program RestService;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.HTTPApp,
  IdHTTPWebBrokerBridge;

procedure StartServer;
var
  Server: TIdHTTPWebBrokerBridge;
begin
  Server := TIdHTTPWebBrokerBridge.Create(nil);
  try
    Server.DefaultPort := 8080;
    Server.Active := True;
    WriteLn('Serveur démarré sur le port 8080');
    WriteLn('Appuyez sur Entrée pour arrêter');
    ReadLn;
  finally
    Server.Free;
  end;
end;

begin
  try
    StartServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

#### Étape 2 : Compiler pour Linux

1. **Ajoutez la plateforme Linux** dans Delphi
2. **Compilez** en mode Release pour `Linux 64-bit`
3. Récupérez l'exécutable : `Linux64/Release/RestService`

#### Étape 3 : Créer le Dockerfile

Créez un fichier nommé `Dockerfile` (sans extension) :

```dockerfile
# Utiliser Ubuntu 22.04 comme base
FROM ubuntu:22.04

# Mettre à jour les paquets
RUN apt-get update && apt-get install -y \
    libgtk-3-0 \
    libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

# Créer le répertoire de l'application
WORKDIR /app

# Copier l'exécutable compilé
COPY Linux64/Release/RestService /app/RestService

# Rendre l'exécutable
RUN chmod +x /app/RestService

# Exposer le port 8080
EXPOSE 8080

# Démarrer l'application
CMD ["/app/RestService"]
```

#### Étape 4 : Construire l'image Docker

```bash
# Dans le dossier contenant le Dockerfile et l'exécutable
docker build -t mon-service-rest:1.0 .

# Explication :
# - build : construire une image
# - -t mon-service-rest:1.0 : nommer l'image avec un tag de version
# - . : utiliser le dossier actuel comme contexte
```

#### Étape 5 : Exécuter le conteneur

```bash
docker run -d -p 8080:8080 --name mon-service mon-service-rest:1.0

# Explication :
# - run : créer et démarrer un conteneur
# - -d : mode détaché (arrière-plan)
# - -p 8080:8080 : mapper le port 8080 du conteneur au port 8080 de l'hôte
# - --name mon-service : donner un nom au conteneur
# - mon-service-rest:1.0 : nom de l'image à utiliser
```

#### Étape 6 : Vérifier que ça fonctionne

```bash
# Voir les conteneurs en cours d'exécution
docker ps

# Voir les logs du conteneur
docker logs mon-service

# Tester l'API
curl http://localhost:8080/api/test
```

#### Étape 7 : Gérer le conteneur

```bash
# Arrêter le conteneur
docker stop mon-service

# Démarrer le conteneur
docker start mon-service

# Redémarrer le conteneur
docker restart mon-service

# Supprimer le conteneur
docker rm mon-service

# Supprimer l'image
docker rmi mon-service-rest:1.0
```

### Docker Compose : Orchestrer plusieurs conteneurs

**Docker Compose** permet de gérer plusieurs conteneurs qui travaillent ensemble.

#### Exemple : Service Delphi + Base de données MySQL

**Fichier `docker-compose.yml`** :

```yaml
version: '3.8'

services:
  # Service web Delphi
  api:
    build: .
    ports:
      - "8080:8080"
    depends_on:
      - database
    environment:
      - DB_HOST=database
      - DB_PORT=3306
      - DB_NAME=myapp
      - DB_USER=root
      - DB_PASSWORD=secret
    restart: unless-stopped

  # Base de données MySQL
  database:
    image: mysql:8.0
    environment:
      - MYSQL_ROOT_PASSWORD=secret
      - MYSQL_DATABASE=myapp
    volumes:
      - mysql_data:/var/lib/mysql
    ports:
      - "3306:3306"
    restart: unless-stopped

# Volumes persistants
volumes:
  mysql_data:
```

**Utilisation** :

```bash
# Démarrer tous les services
docker-compose up -d

# Voir l'état des services
docker-compose ps

# Voir les logs
docker-compose logs -f

# Arrêter tous les services
docker-compose down

# Arrêter et supprimer les volumes (données)
docker-compose down -v
```

**Avantages** :
- Configuration centralisée
- Démarrage de tout l'environnement en une commande
- Réseau automatique entre conteneurs
- Gestion simplifiée

## Cas d'usage pratiques pour développeurs Delphi

### 1. Environnement de développement avec base de données

Au lieu d'installer MySQL sur votre machine :

```yaml
# docker-compose.yml
version: '3.8'

services:
  mysql:
    image: mysql:8.0
    environment:
      MYSQL_ROOT_PASSWORD: dev
      MYSQL_DATABASE: myapp_dev
    ports:
      - "3306:3306"
    volumes:
      - ./sql:/docker-entrypoint-initdb.d
      - mysql_dev_data:/var/lib/mysql

volumes:
  mysql_dev_data:
```

**Utilisation** :
```bash
docker-compose up -d

# Votre application Delphi se connecte à localhost:3306
# Données persistantes même après arrêt
# Facile à supprimer et recréer pour des tests
```

### 2. Tests d'intégration automatisés

Créez un environnement de test complet :

```yaml
version: '3.8'

services:
  test-db:
    image: mysql:8.0
    environment:
      MYSQL_ROOT_PASSWORD: test
      MYSQL_DATABASE: test_db
    tmpfs:
      - /var/lib/mysql  # Données en RAM, très rapide

  test-redis:
    image: redis:7
    tmpfs:
      - /data

  test-api:
    build:
      context: .
      dockerfile: Dockerfile.test
    depends_on:
      - test-db
      - test-redis
    command: ["./run-tests.sh"]
```

**Lancement des tests** :
```bash
docker-compose up --abort-on-container-exit
```

Tout l'environnement de test est créé, les tests sont exécutés, puis tout est détruit automatiquement.

### 3. Environnements multiples (dev, staging, prod)

Utilisez des fichiers de configuration différents :

```bash
# Développement
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up

# Staging
docker-compose -f docker-compose.yml -f docker-compose.staging.yml up

# Production
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up
```

Chaque environnement a sa propre configuration mais partage la base commune.

### 4. Service de traitement par lot

Application Delphi qui traite des fichiers :

```dockerfile
FROM ubuntu:22.04

# Dépendances
RUN apt-get update && apt-get install -y libgtk-3-0 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY BatchProcessor /app/

# Volume pour les fichiers à traiter
VOLUME ["/data"]

CMD ["/app/BatchProcessor", "/data"]
```

**Exécution** :
```bash
docker run -v /chemin/vers/fichiers:/data mon-batch-processor
```

Les fichiers sont traités puis le conteneur s'arrête automatiquement.

## Bonnes pratiques Docker pour Delphi

### 1. Images de base légères

Utilisez des images de base minimales :

```dockerfile
# ✗ Lourd
FROM ubuntu:22.04

# ✓ Plus léger
FROM debian:bookworm-slim

# ✓ Encore plus léger (si compatibles)
FROM alpine:3.18
```

**Alpine Linux** est très léger (~5 Mo) mais peut nécessiter des ajustements pour les applications Delphi.

### 2. Multi-stage builds

Réduisez la taille de l'image finale :

```dockerfile
# Stage 1 : Build (avec toutes les dépendances)
FROM ubuntu:22.04 AS builder
RUN apt-get update && apt-get install -y build-tools
COPY source/ /build/
RUN compile-app

# Stage 2 : Runtime (seulement ce qui est nécessaire)
FROM ubuntu:22.04
COPY --from=builder /build/app /app/
CMD ["/app/app"]
```

L'image finale ne contient que l'exécutable, pas les outils de build.

### 3. Ne pas exécuter en root

Pour des raisons de sécurité, créez un utilisateur :

```dockerfile
FROM ubuntu:22.04

# Créer un utilisateur non-root
RUN useradd -m -u 1000 appuser

WORKDIR /app
COPY MonApp /app/
RUN chown -R appuser:appuser /app

# Passer à cet utilisateur
USER appuser

CMD ["/app/MonApp"]
```

### 4. Gérer les logs correctement

Les conteneurs doivent écrire sur stdout/stderr :

```pascal
// Dans votre application Delphi
procedure LogMessage(const Msg: string);
begin
  // Écrire sur la sortie standard pour Docker
  WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
end;
```

Docker capture automatiquement ces logs : `docker logs mon-conteneur`

### 5. Variables d'environnement pour la configuration

Ne codez pas en dur les paramètres :

```pascal
// Lire depuis les variables d'environnement
var
  DBHost, DBUser, DBPassword: string;
begin
  DBHost := GetEnvironmentVariable('DB_HOST');
  DBUser := GetEnvironmentVariable('DB_USER');
  DBPassword := GetEnvironmentVariable('DB_PASSWORD');
end;
```

```bash
docker run -e DB_HOST=192.168.1.100 \
           -e DB_USER=admin \
           -e DB_PASSWORD=secret \
           mon-app
```

### 6. Santé du conteneur (health checks)

Ajoutez des vérifications de santé :

```dockerfile
FROM ubuntu:22.04

COPY MonApp /app/
EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl -f http://localhost:8080/health || exit 1

CMD ["/app/MonApp"]
```

Docker pourra détecter si votre application ne répond plus.

### 7. Volumes pour les données persistantes

Les données importantes doivent être dans des volumes :

```bash
docker run -v mon_volume:/app/data mon-app

# Ou un dossier local
docker run -v /chemin/local:/app/data mon-app
```

Sinon, toutes les données sont perdues quand le conteneur est supprimé.

## Docker dans le CI/CD

Docker s'intègre parfaitement dans les pipelines d'intégration continue.

### Exemple avec GitLab CI

**.gitlab-ci.yml** :

```yaml
stages:
  - build
  - test
  - deploy

variables:
  IMAGE_TAG: $CI_REGISTRY_IMAGE:$CI_COMMIT_SHORT_SHA

build:
  stage: build
  script:
    - docker build -t $IMAGE_TAG .
    - docker push $IMAGE_TAG

test:
  stage: test
  script:
    - docker run --rm $IMAGE_TAG /app/run-tests.sh

deploy:
  stage: deploy
  script:
    - docker pull $IMAGE_TAG
    - docker stop mon-app || true
    - docker rm mon-app || true
    - docker run -d --name mon-app -p 8080:8080 $IMAGE_TAG
  only:
    - main
```

**Flux** :
1. Code committé sur Git
2. GitLab CI construit l'image Docker
3. Tests exécutés dans un conteneur
4. Si succès, déploiement automatique

## Déploiement dans le cloud avec Docker

Docker facilite le déploiement sur diverses plateformes cloud.

### Services cloud supportant Docker

#### 1. AWS (Amazon Web Services)

**Amazon ECS (Elastic Container Service)** :
- Service managé pour conteneurs
- Pas de gestion de serveurs
- Scalabilité automatique

**Amazon Fargate** :
- Encore plus simple qu'ECS
- Paiement à l'utilisation

#### 2. Azure (Microsoft)

**Azure Container Instances** :
- Déploiement rapide de conteneurs
- Facturation à la seconde

**Azure Kubernetes Service (AKS)** :
- Pour les applications complexes nécessitant orchestration

#### 3. Google Cloud Platform

**Google Cloud Run** :
- Conteneurs serverless
- Scaling automatique de 0 à N instances
- Paiement à l'utilisation

**Google Kubernetes Engine (GKE)** :
- Kubernetes managé par Google

#### 4. DigitalOcean

**App Platform** :
- Simple et abordable
- Parfait pour débuter

### Exemple : Déployer sur Google Cloud Run

```bash
# 1. Construire l'image
docker build -t gcr.io/mon-projet/mon-app:v1 .

# 2. Pousser vers Google Container Registry
docker push gcr.io/mon-projet/mon-app:v1

# 3. Déployer sur Cloud Run
gcloud run deploy mon-app \
  --image gcr.io/mon-projet/mon-app:v1 \
  --platform managed \
  --region europe-west1 \
  --allow-unauthenticated

# Votre application est en ligne avec une URL HTTPS !
```

**Avantages** :
- HTTPS automatique
- Scaling automatique
- Paiement uniquement quand utilisé
- URL publique fournie

## Limitations de Docker pour les applications Delphi

### Applications avec interface graphique (GUI)

Docker est principalement conçu pour les applications serveur. Pour les applications GUI Delphi (VCL ou FMX) :

**Problèmes** :
- Pas d'affichage graphique par défaut dans les conteneurs
- Conteneurs Linux pour applications Linux uniquement
- Conteneurs Windows possibles mais moins courants

**Solutions** :
- **VNC** : Accès distant à l'interface graphique
- **X11 forwarding** : Redirection de l'affichage (Linux)
- **Serveur RDP** : Bureau à distance Windows

Mais dans la pratique, **Docker n'est pas idéal pour les applications desktop graphiques**.

### Applications Windows Desktop

Les conteneurs Windows existent mais sont moins populaires :

**Limitations** :
- Nécessitent Windows Server comme hôte
- Images beaucoup plus volumineuses (plusieurs Go)
- Moins de support communautaire
- Pas disponibles sur tous les services cloud

**Recommandation** : Pour les applications desktop Windows, privilégiez les installateurs classiques et la virtualisation pour les tests.

### Performance

Les conteneurs ajoutent une légère surcharge :
- Généralement négligeable pour les services web
- Peut être notable pour des calculs intensifs

## Kubernetes : L'étape suivante

Si vous gérez de nombreux conteneurs, **Kubernetes** (K8s) est la plateforme d'orchestration de référence.

### Qu'est-ce que Kubernetes ?

**Kubernetes** gère automatiquement :
- Déploiement de conteneurs sur plusieurs serveurs
- Répartition de charge
- Redémarrage automatique en cas de crash
- Mise à l'échelle automatique
- Mises à jour progressives (rolling updates)

### Quand utiliser Kubernetes ?

**Utilisez Kubernetes si** :
- Vous avez de nombreux microservices
- Vous avez besoin de haute disponibilité
- Vous gérez un trafic variable nécessitant du scaling

**N'utilisez pas Kubernetes si** :
- Vous avez une seule application simple
- Vous débutez avec les conteneurs
- Votre équipe est petite

**Courbe d'apprentissage** : Kubernetes est complexe. Commencez par maîtriser Docker avant d'aborder Kubernetes.

## Tableau comparatif : VM vs Conteneurs vs Natif

| Aspect | Machine Virtuelle | Conteneur Docker | Installation Native |
|--------|-------------------|------------------|---------------------|
| **Démarrage** | 1-2 minutes | 1-2 secondes | Instantané |
| **Taille** | 20-50 Go | 100 Mo - 2 Go | Variable |
| **Isolation** | Complète | Partielle | Aucune |
| **Performance** | -10% à -20% | -2% à -5% | 100% |
| **Portabilité** | Moyenne | Excellente | Faible |
| **Ressources** | Élevées | Faibles | Minimales |
| **Gestion** | Complexe | Moyenne | Simple |
| **Cas d'usage** | Tests multi-OS | Services, APIs | Applications desktop |

## Checklist pour conteneuriser une application Delphi

Avant de conteneuriser votre application :

- [ ] Application sans interface graphique ou service web
- [ ] Peut fonctionner sur Linux (si conteneur Linux)
- [ ] Configuration via variables d'environnement
- [ ] Logs envoyés sur stdout/stderr
- [ ] Données stockées dans volumes ou BDD externe
- [ ] Port d'écoute configurable
- [ ] Gestion propre de l'arrêt (SIGTERM)
- [ ] Dépendances identifiées et documentées
- [ ] Testé dans un environnement isolé
- [ ] Documentation du Dockerfile créée

## Problèmes courants et solutions

### "Docker is not running"

**Cause** : Docker Desktop n'est pas démarré

**Solution** :
```bash
# Windows : Lancer Docker Desktop depuis le menu Démarrer
# Linux :
sudo systemctl start docker
```

### "Cannot connect to Docker daemon"

**Cause** : Permissions insuffisantes (Linux)

**Solution** :
```bash
sudo usermod -aG docker $USER
# Déconnectez-vous et reconnectez-vous
```

### "Port already in use"

**Cause** : Le port est déjà utilisé par un autre processus

**Solution** :
```bash
# Utiliser un port différent
docker run -p 8081:8080 mon-app

# Ou arrêter le processus qui utilise le port
# Windows :
netstat -ano | findstr :8080
taskkill /PID <PID> /F

# Linux :
sudo lsof -i :8080
kill <PID>
```

### Image trop volumineuse

**Cause** : Trop de fichiers ou dépendances inutiles

**Solutions** :
```dockerfile
# Utiliser .dockerignore (comme .gitignore)
# Créer le fichier .dockerignore :
*.log
temp/
*.bak

# Nettoyer dans le Dockerfile
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Utiliser multi-stage builds
```

### Application plante dans le conteneur mais pas en local

**Cause** : Différences d'environnement

**Solutions** :
- Vérifiez les dépendances système avec `ldd`
- Ajoutez des logs détaillés
- Testez dans un environnement similaire au conteneur
- Utilisez `docker exec -it conteneur /bin/bash` pour explorer

## Ressources pour aller plus loin

### Documentation officielle

- **Docker** : https://docs.docker.com/
- **Docker Compose** : https://docs.docker.com/compose/
- **Kubernetes** : https://kubernetes.io/docs/

### Tutoriels recommandés

- **Docker for Beginners** : Play with Docker (play-with-docker.com)
- **Katacoda** : Scénarios interactifs gratuits
- **YouTube** : Chaînes comme TechWorld with Nana

### Outils utiles

- **Docker Desktop** : Interface graphique pour Docker
- **Portainer** : Interface web pour gérer Docker
- **Dive** : Explorer les couches des images Docker
- **Lazydocker** : Interface terminal pour Docker

## Conclusion

La virtualisation et les conteneurs sont des outils puissants pour le déploiement moderne d'applications. Bien que Delphi soit principalement utilisé pour des applications desktop Windows (où ces technologies sont moins critiques), elles deviennent essentielles pour :

- **Services web et API** développés avec Delphi
- **Applications serveur Linux** avec FMXLinux
- **Environnements de test** reproductibles
- **Déploiement cloud** moderne
- **CI/CD** automatisé

**Points clés à retenir** :

1. **Virtualisation (VMs)** : Idéale pour tester sur différents OS, isolation complète
2. **Docker (conteneurs)** : Parfait pour services web, APIs, environnements de développement
3. **Pas pour GUI desktop** : Docker n'est pas adapté aux applications graphiques traditionnelles
4. **Commencez simple** : VirtualBox pour les tests, Docker pour les services
5. **Cloud-ready** : Docker facilite le déploiement sur AWS, Azure, Google Cloud
6. **Kubernetes** : Seulement si vous avez des besoins complexes d'orchestration

Avec ces technologies, vous pouvez moderniser le déploiement de vos applications Delphi et les intégrer dans des architectures cloud modernes. Dans la prochaine section, nous explorerons les formats MSI et la distribution via le Windows Store.

⏭️ [MSI et Windows Store](/17-distribution-et-deploiement/08-msi-et-windows-store.md)
