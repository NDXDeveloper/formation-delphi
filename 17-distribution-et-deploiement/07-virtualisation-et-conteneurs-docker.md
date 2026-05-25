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
- Testez sur Windows 10 22H2, Windows 11 23H2/24H2, Windows Server 2019/2022/2025 sans posséder plusieurs PC.
- (Windows 7/8 ne sont plus pertinents en 2026 : fin de support Microsoft en 2020/2023, à éviter sauf cas de maintenance long terme.)

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
- **Éditeur** : Microsoft.
- **Avantages** : intégré à Windows, performant, supporté nativement par Docker Desktop et WSL2.
- **Inconvénient** : Hyper-V « complet » (gestionnaire de VM) uniquement sur Windows **Pro / Entreprise / Education**. Sur Windows Home, seul le sous-ensemble nécessaire à WSL2 est disponible — vous pouvez quand même faire tourner Docker Desktop.
- **Idéal pour** : développeurs Windows Pro+ qui veulent éviter d'installer un hyperviseur tiers.

## Utiliser la virtualisation pour tester des applications Delphi

### Cas d'usage typiques

#### 1. Tester sur différentes versions de Windows

Créez des VM pour :
- **Windows 10 22H2** (encore présent chez beaucoup d'utilisateurs ; fin de support gratuit Microsoft le 14 octobre 2025 — beaucoup d'utilisateurs y resteront néanmoins en 2026).
- **Windows 11** : 23H2 et 24H2 — cible principale en 2026.
- **Windows Server** : 2019, 2022, 2025 selon votre cible serveur.

**Avantage** : Vérifiez que votre application fonctionne partout sans posséder plusieurs PC.

#### 2. Environnement de test "propre"

**Problème** : Votre machine de développement a beaucoup de logiciels installés (Delphi, drivers, composants). Votre application fonctionnera-t-elle sur une machine utilisateur "vierge" ?

**Solution** : Testez dans une VM fraîchement installée qui simule une vraie machine utilisateur.

#### 3. Tests de compatibilité

Testez votre application avec :
- Différentes versions de **.NET** si pertinent : .NET Framework 4.8/4.8.1 (maintenance) **OU** .NET 8 LTS / .NET 10 LTS (versions modernes). Le .NET Framework historique est en mode maintenance — .NET 8+ est désormais le standard pour le nouveau code .NET interop.
- Différentes versions de drivers de base de données (FireDAC, MySQL connector, etc.).
- Différentes configurations régionales (langue, format de date, séparateur décimal — un nombre `1,5` en français devient `1.5` en anglais : source classique de bugs).

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
   - Mémoire : 8192 Mo (8 Go recommandés pour Windows 11 ; 4 Go = limite stricte
                       Microsoft mais expérience dégradée)
   - Processeurs : 2 vCPU minimum (4 recommandés)
   - Disque dur : 80 Go (Windows 11 + Office + Delphi runtime occupent ~50 Go)
   - TPM 2.0 émulé et Secure Boot : requis par Windows 11 (VirtualBox 7+ supporte)
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

#### Docker Hub et registres alternatifs

**Docker Hub** est le registre public historique d'images Docker. Vous pouvez :
- Télécharger des images existantes (MySQL, Nginx, Ubuntu, etc.).
- Publier vos propres images (compte gratuit = 1 repo privé, illimité en public).

> ⚠️ **Limites de pull Docker Hub (depuis 2020, renforcées en 2024)** :  
> - **Anonyme** : 100 pulls / 6 heures (par IP). Les CI/CD partagés sur des IP publiques peuvent atteindre cette limite rapidement.  
> - **Compte Free** : 200 pulls / 6 heures.  
> - **Compte Pro** ($9/mois) : illimité.

**Alternatives modernes** (souvent plus généreuses) :
- **GitHub Container Registry** (`ghcr.io`) : gratuit illimité pour les repos GitHub publics, intégré à GitHub Actions via `${{ secrets.GITHUB_TOKEN }}`.
- **GitLab Container Registry** : intégré à GitLab CI, généreux sur les quotas.
- **Quay.io** (Red Hat) : longtemps gratuit pour open source, support des images signées (Cosign).
- **Amazon ECR Public** : gratuit pour les images publiques (limites de pull élevées).
- **Cloud privés** : Azure Container Registry, Google Artifact Registry (cf section 17.7 cloud).

### Installation de Docker

#### Sur Windows

**Docker Desktop pour Windows** :

1. **Télécharger** : https://www.docker.com/products/docker-desktop/
2. **Installer** : suivez l'assistant.
3. **Activer WSL2** : Docker Desktop l'utilise par défaut pour exécuter des conteneurs Linux (le mode Hyper-V est conservé pour les conteneurs Windows).
4. **Redémarrer** : Docker sera disponible après redémarrage.

**Configuration minimale (Docker Desktop 4.x en 2026)** :
- **Windows 10 22H2 ou Windows 11** — toutes éditions (Home, Pro, Enterprise, Education). Le requis « Pro/Enterprise » est obsolète depuis l'intégration WSL2.
- **WSL2** activé (gratuit, fourni avec Windows).
- 4 Go de RAM minimum (8 Go recommandés).
- **Virtualisation activée dans le BIOS/UEFI** (VT-x sur Intel, AMD-V sur AMD).

> ⚠️ **Licence commerciale Docker Desktop (2021+)** : Docker Desktop reste gratuit pour usage personnel, éducation, projets open source et **petites entreprises (< 250 salariés ET < 10 M $ de revenu annuel)**. Au-delà, une **souscription payante** est requise (Docker Pro/Team/Business — environ 5 à 24 $/mois/utilisateur en 2026). Les alternatives libres : **Rancher Desktop**, **Podman Desktop**, **Docker CE en CLI sur Linux/WSL2**.

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
# Docker version 27.x ou 28.x en 2026 (Docker CE est en cycle de release rapide).

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
  System.SysUtils, System.SyncObjs,
  {$IFDEF LINUX}Posix.Signal, Posix.SysTypes,{$ENDIF}
  Web.HTTPApp,
  IdHTTPWebBrokerBridge;

var
  GTerminateEvent: TEvent;

{$IFDEF LINUX}
// ⚠ Indispensable pour Docker : `docker stop` envoie SIGTERM puis attend
//   ~10 secondes avant SIGKILL. Sans handler, le `ReadLn` bloquerait
//   et le conteneur serait tué brutalement (connexions HTTP coupées,
//   pas de flush des logs, etc.). On capture SIGTERM/SIGINT pour
//   débloquer la boucle principale et faire un arrêt propre.
procedure HandleSignal(SigNum: Integer); cdecl;  
begin  
  GTerminateEvent.SetEvent;
end;

procedure InstallSignalHandlers;  
var  
  Action: sigaction_t;
begin
  FillChar(Action, SizeOf(Action), 0);
  Action.sa_handler := @HandleSignal;
  sigaction(SIGTERM, @Action, nil);  // docker stop
  sigaction(SIGINT,  @Action, nil);  // Ctrl+C
end;
{$ENDIF}

procedure StartServer;  
var  
  Server: TIdHTTPWebBrokerBridge;
begin
  Server := TIdHTTPWebBrokerBridge.Create(nil);
  try
    Server.DefaultPort := 8080;
    Server.Active := True;
    WriteLn('Serveur démarré sur le port 8080');

    // Attente bloquante de la demande d'arrêt.
    {$IFDEF LINUX}
    GTerminateEvent.WaitFor(INFINITE);
    WriteLn('Signal d''arrêt reçu, fermeture propre...');
    {$ELSE}
    WriteLn('Appuyez sur Entrée pour arrêter');
    ReadLn;
    {$ENDIF}
  finally
    Server.Free;
  end;
end;

begin
  GTerminateEvent := TEvent.Create(nil, True, False, '');
  try
    {$IFDEF LINUX}InstallSignalHandlers;{$ENDIF}
    try
      StartServer;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    GTerminateEvent.Free;
  end;
end.
```

> 💡 **Pourquoi capturer SIGTERM ?** Sur Docker Linux, `docker stop` envoie d'abord SIGTERM, attend (par défaut 10 s, ajustable via `--time`), puis SIGKILL si le processus ne s'est pas terminé. Un service Delphi qui ignore SIGTERM se fait donc tuer brutalement, sans flush des logs, sans drainer les requêtes HTTP en cours. Pour orchestrer proprement avec Kubernetes ou ECS, gérer SIGTERM est indispensable.

#### Étape 2 : Compiler pour Linux

1. **Ajoutez la plateforme Linux** dans Delphi
2. **Compilez** en mode Release pour `Linux 64-bit`
3. Récupérez l'exécutable : `Linux64/Release/RestService`

#### Étape 3 : Créer le Dockerfile

Créez un fichier nommé `Dockerfile` (sans extension) :

```dockerfile
# Utiliser Ubuntu 22.04 LTS comme base (alternative : debian:bookworm-slim).
FROM ubuntu:22.04

# ⚠ Dépendances pour un service Delphi CONSOLE (TIdHTTPWebBrokerBridge) :
#   - ca-certificates : pour la vérification TLS des appels sortants (BDD, API).
#   - libssl3        : OpenSSL 3 (TLS pour HTTPS).
#   - libcurl4       : utilisé par les composants HTTP modernes de la RTL.
#   N'ajoutez PAS libgtk-3-0 ici — GTK est nécessaire UNIQUEMENT pour les
#   apps FMXLinux GUI, pas pour un service console (gain ~50 Mo).
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libssl3 \
    libcurl4 \
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
# ⚠ La directive `version:` est obsolète depuis Docker Compose v2 (2021).
# Docker Compose v2+ ignore ce champ. Le format ci-dessous est compatible
# avec Compose v2 et v3+.

services:
  # Service web Delphi
  api:
    build: .
    ports:
      - "8080:8080"
    depends_on:
      database:
        condition: service_healthy
    # ⚠ En PROD, NE PAS écrire les mots de passe directement dans le YAML
    #   (qui finit généralement dans Git). Deux approches :
    #   1. Fichier .env (ajouté au .gitignore) : `DB_PASSWORD=...`
    #      puis `${DB_PASSWORD}` dans le YAML.
    #   2. Docker secrets (mode swarm/k8s) : `secrets:` + montage en
    #      `/run/secrets/db_password`, lu depuis l'app au démarrage.
    environment:
      - DB_HOST=database
      - DB_PORT=3306
      - DB_NAME=myapp
      - DB_USER=root
      - DB_PASSWORD=${DB_PASSWORD:?DB_PASSWORD doit être défini}
    restart: unless-stopped

  # Base de données MySQL
  database:
    image: mysql:8.4   # 8.4 LTS est la version LTS active en 2026
    environment:
      - MYSQL_ROOT_PASSWORD=${DB_PASSWORD:?DB_PASSWORD doit être défini}
      - MYSQL_DATABASE=myapp
    volumes:
      - mysql_data:/var/lib/mysql
    # ⚠ NE PAS exposer le port 3306 à l'hôte en production ; conserver
    # uniquement la communication interne entre services. Décommentez
    # `ports:` ci-dessous uniquement pour le développement local.
    # ports:
    #   - "3306:3306"
    healthcheck:
      test: ["CMD", "mysqladmin", "ping", "-h", "localhost"]
      interval: 5s
      timeout: 5s
      retries: 10
    restart: unless-stopped

# Volumes persistants
volumes:
  mysql_data:
```

> 💡 **`depends_on: condition: service_healthy`** : permet d'attendre que MySQL soit réellement prêt (et pas seulement démarré) avant de lancer le service `api`. Évite les erreurs « connexion refusée » au démarrage initial du stack.

**Utilisation** :

```bash
# ⚠ En 2026, la commande recommandée est `docker compose` (SANS tiret,
#   c'est un plugin intégré au CLI docker depuis Compose v2). L'ancienne
#   commande `docker-compose` (avec tiret, Compose v1 standalone) est
#   dépréciée et n'est plus livrée par défaut avec Docker Desktop.
#   Les exemples ci-dessous utilisent la commande moderne.

# Démarrer tous les services
docker compose up -d

# Voir l'état des services
docker compose ps

# Voir les logs
docker compose logs -f

# Arrêter tous les services
docker compose down

# Arrêter et supprimer les volumes (données)
docker compose down -v
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
# (directive `version:` obsolète depuis Compose v2 — omise)

services:
  mysql:
    image: mysql:8.4    # 8.4 LTS, supportée jusqu'en 2032
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
docker compose up -d

# Votre application Delphi se connecte à localhost:3306
# Données persistantes même après arrêt
# Facile à supprimer et recréer pour des tests
```

### 2. Tests d'intégration automatisés

Créez un environnement de test complet :

```yaml
# (directive `version:` omise — obsolète depuis Compose v2)

services:
  test-db:
    image: mysql:8.4
    environment:
      MYSQL_ROOT_PASSWORD: test
      MYSQL_DATABASE: test_db
    tmpfs:
      - /var/lib/mysql  # Données en RAM, très rapide

  test-redis:
    image: redis:7-alpine
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
docker compose up --abort-on-container-exit
```

Tout l'environnement de test est créé, les tests sont exécutés, puis tout est détruit automatiquement.

### 3. Environnements multiples (dev, staging, prod)

Utilisez des fichiers de configuration différents :

```bash
# Développement
docker compose -f docker-compose.yml -f docker-compose.dev.yml up

# Staging
docker compose -f docker-compose.yml -f docker-compose.staging.yml up

# Production
docker compose -f docker-compose.yml -f docker-compose.prod.yml up
```

Chaque environnement a sa propre configuration mais partage la base commune.

### 4. Service de traitement par lot

Application Delphi console qui traite des fichiers :

```dockerfile
FROM ubuntu:22.04

# ⚠ Pour une app CONSOLE Delphi (sans interface FMX), pas besoin de GTK.
# N'installer que les libs réellement utilisées : SSL si appels HTTPS,
# curl si client REST natif, libcrypto si chiffrement custom, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates libssl3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app  
COPY BatchProcessor /app/  
RUN chmod +x /app/BatchProcessor  

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
# ✗ Lourd (~70 Mo)
FROM ubuntu:22.04

# ✓ Plus léger (~30 Mo)
FROM debian:bookworm-slim

# ⚠ Très léger (~5 Mo) MAIS incompatible avec Delphi
FROM alpine:3.20
```

> 🚨 **Alpine Linux est INCOMPATIBLE avec les binaires Delphi standards** : Alpine utilise la bibliothèque C **musl libc** au lieu de **glibc**. Or les compilateurs Delphi (dcclinux64) produisent du code lié à glibc. Une application Delphi placée dans Alpine échouera typiquement avec une erreur du type `Error loading shared libraries: libc.so.6: cannot open shared object file`. Pour minimiser la taille avec une app Delphi, utilisez plutôt :  
> - **`debian:bookworm-slim`** (~30 Mo, glibc).  
> - **`ubuntu:24.04`** ou **`ubuntu:22.04`** (~70 Mo, glibc, plus de paquets disponibles).  
> - **`gcr.io/distroless/cc-debian12`** (~20 Mo, glibc, image distroless de Google sans shell ni gestionnaire de paquets).

### 2. Multi-stage builds

Réduisez la taille de l'image finale :

```dockerfile
# ⚠ Pour Delphi : le compilateur Delphi (dcclinux64) ne tourne PAS dans
#   un conteneur Linux — il est exécuté côté Windows via PAServer. Le
#   multi-stage est donc surtout utile pour les binaires Delphi déjà
#   compilés (qu'on copie depuis l'hôte), accompagnés d'éventuelles
#   ressources qu'on traite dans le stage builder (compilation de scripts,
#   tests, etc.).
# Exemple : utiliser le stage builder pour bundler les dépendances :

# Stage 1 : Build (avec outils de packaging)
FROM ubuntu:22.04 AS builder
# ⚠ `build-tools` n'existe PAS comme paquet Debian/Ubuntu standard.
#   Le paquet correct est `build-essential` (gcc, make, etc.).
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential ca-certificates && \
    rm -rf /var/lib/apt/lists/*
COPY source/ /build/  
RUN cd /build && make  

# Stage 2 : Runtime (seulement ce qui est nécessaire)
FROM debian:bookworm-slim  
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
uses System.SysUtils, System.DateUtils;

// Dans votre application Delphi (service console pour Docker)
procedure LogMessage(const Msg: string);  
begin  
  // ⚠ TOUJOURS logger en UTC (suffixe « Z », format ISO 8601) : les
  //   conteneurs tournent souvent dans des fuseaux différents et l'agrégation
  //   centralisée (Loki, ELK, CloudWatch) suppose UTC.
  WriteLn(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                         TTimeZone.Local.ToUniversalTime(Now)) +
          ' - ' + Msg);
  // ⚠ stdout est bufferisé par défaut. Sans Flush(), les logs peuvent
  //   être perdus si le conteneur est tué brutalement. Pour Docker,
  //   forcer le flush après chaque ligne critique.
  Flush(Output);
end;
```

Docker capture automatiquement ces logs : `docker logs mon-conteneur`.

> 💡 **Niveau structuré (JSON)** : pour les pipelines de log centralisé (Loki, Elasticsearch), préférez du **JSON structuré** plutôt que du texte libre. Un parseur côté collecteur extraira directement les champs (level, msg, request_id, etc.) :  
>  
> ```pascal  
> WriteLn(Format('{"ts":"%s","level":"%s","msg":"%s"}',
>   [UtcTimestamp, Level, JsonEscape(Msg)]));
> Flush(Output);
> ```

### 5. Variables d'environnement pour la configuration

Ne codez pas en dur les paramètres :

```pascal
uses
  System.SysUtils;  // Pour GetEnvironmentVariable cross-platform.

// Lire depuis les variables d'environnement (fonctionne identiquement
// sur Windows et Linux car on utilise la version System.SysUtils,
// pas Winapi.Windows).
var
  DBHost, DBUser, DBPassword: string;
begin
  DBHost     := GetEnvironmentVariable('DB_HOST');
  DBUser     := GetEnvironmentVariable('DB_USER');
  DBPassword := GetEnvironmentVariable('DB_PASSWORD');

  // ⚠ GetEnvironmentVariable retourne '' si la variable n'est PAS définie
  // (n'émet pas d'exception). Vérifier la présence si elle est obligatoire :
  if DBHost = '' then
    raise Exception.Create('Variable d''environnement DB_HOST requise.');
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

# ⚠ `curl` n'est pas installé par défaut dans les images slim. Si vous
# utilisez `debian:bookworm-slim` ou une image distroless, installez-le
# explicitement OU utilisez `wget` / une simple commande sans dépendance.
RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

COPY MonApp /app/  
EXPOSE 8080  

HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

CMD ["/app/MonApp"]
```

> 💡 **Alternative sans curl** : si vous voulez éviter d'ajouter curl, votre app Delphi peut écrire un fichier témoin (`/tmp/healthy`) et le healthcheck vérifie sa présence : `CMD test -f /tmp/healthy || exit 1`.

Docker pourra détecter si votre application ne répond plus et la marquer comme `unhealthy` — utile pour les orchestrateurs (Kubernetes, Docker Swarm, ECS) qui peuvent alors la redémarrer automatiquement.

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

# GitLab CI fournit automatiquement les variables $CI_REGISTRY,
# $CI_REGISTRY_USER, $CI_REGISTRY_PASSWORD pour son container registry
# intégré. Sans `docker login`, le `docker push` échouerait.
.docker_login: &docker_login
  - echo "$CI_REGISTRY_PASSWORD" | docker login -u "$CI_REGISTRY_USER"
    --password-stdin "$CI_REGISTRY"

build:
  stage: build
  script:
    - *docker_login
    - docker build -t $IMAGE_TAG .
    - docker push $IMAGE_TAG

test:
  stage: test
  script:
    - *docker_login
    - docker pull $IMAGE_TAG
    - docker run --rm $IMAGE_TAG /app/run-tests.sh

deploy:
  stage: deploy
  script:
    - *docker_login
    - docker pull $IMAGE_TAG
    - docker stop mon-app || true
    - docker rm mon-app || true
    - docker run -d --name mon-app -p 8080:8080 $IMAGE_TAG
  only:
    - main
```

> 💡 **`--password-stdin`** : passer le mot de passe via stdin évite qu'il apparaisse dans `ps -ef` / la liste des processus pendant la durée du login. Pratique standard de sécurité.

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

> ⚠️ **Container Registry (`gcr.io`) a été arrêté le 18 mars 2025** par Google. Le service successeur est **Artifact Registry** (`pkg.dev`). Les URLs `gcr.io` continuent à fonctionner pour les images déjà hébergées, mais toute nouvelle image doit utiliser Artifact Registry. Référence : `https://cloud.google.com/artifact-registry/docs/transition/transition-from-gcr`.

```bash
# Prérequis : créer un dépôt Artifact Registry au préalable :
#   gcloud artifacts repositories create mon-repo \
#       --repository-format=docker --location=europe-west1

# 1. Construire l'image (URL Artifact Registry, pas gcr.io déprécié)
docker build -t europe-west1-docker.pkg.dev/mon-projet/mon-repo/mon-app:v1 .

# 2. S'authentifier auprès d'Artifact Registry
gcloud auth configure-docker europe-west1-docker.pkg.dev

# 3. Pousser vers Artifact Registry
docker push europe-west1-docker.pkg.dev/mon-projet/mon-repo/mon-app:v1

# 4. Déployer sur Cloud Run
gcloud run deploy mon-app \
  --image europe-west1-docker.pkg.dev/mon-projet/mon-repo/mon-app:v1 \
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
- En **développement** : Windows 10/11 Pro/Enterprise/Education avec Docker Desktop en mode « Windows containers » (Hyper-V isolation activée).
- En **production** : Windows Server 2019/2022/2025 reste l'hôte standard ; quelques services cloud (Azure ACI/AKS, AWS ECS Windows) supportent les Windows containers.
- Images beaucoup plus volumineuses (`mcr.microsoft.com/windows/servercore:ltsc2022` fait ~2 Go ; `nanoserver` ~250 Mo mais incompatible avec la plupart des DLL Win32 et donc avec Delphi).
- Communauté restreinte par rapport aux conteneurs Linux.

**Recommandation** : Pour les applications desktop Windows, privilégiez les installateurs classiques et la virtualisation (cf début du chapitre) pour les tests. Les conteneurs Windows ne sont vraiment utiles que pour des **services Windows headless** modernes (IIS, .NET, Delphi service compilé en console).

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

- **Docker for Beginners** : Play with Docker (play-with-docker.com).
- **Killercoda** (https://killercoda.com/) : successeur communautaire de Katacoda — Katacoda **fermé en juin 2022** par O'Reilly.
- **YouTube** : chaînes comme *TechWorld with Nana*, *NetworkChuck*, *Bret Fisher Docker*.

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
