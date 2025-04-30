# 17.7 Virtualisation et conteneurs Docker

## Introduction

Dans le monde du développement moderne, la virtualisation et les conteneurs sont devenus des technologies incontournables pour le déploiement d'applications. Si vous avez entendu parler de Docker et vous demandez comment l'utiliser avec vos applications Delphi, ce chapitre est fait pour vous. Nous allons explorer comment ces technologies peuvent simplifier le déploiement, améliorer la portabilité et faciliter la mise à l'échelle de vos applications Delphi.

## Qu'est-ce que la virtualisation et les conteneurs ?

Commençons par comprendre ces concepts fondamentaux :

### Virtualisation traditionnelle

La **virtualisation** consiste à créer une version virtuelle (plutôt que réelle) d'une ressource informatique comme un système d'exploitation, un serveur, un périphérique de stockage ou des ressources réseau.

![Comparaison Machine Virtuelle vs Conteneur](https://placeholder-image.com/vm-vs-container.png)

- **Machine virtuelle (VM)** : C'est comme un ordinateur complet qui fonctionne à l'intérieur de votre ordinateur. Elle possède son propre système d'exploitation, ses propres pilotes et applications.
- **Avantages des VMs** : Isolation complète, compatibilité avec n'importe quel système d'exploitation
- **Inconvénients des VMs** : Lourdes (plusieurs Go), démarrage lent, utilisation inefficace des ressources

### Conteneurs

Les **conteneurs** sont une forme de virtualisation plus légère qui partage le système d'exploitation de l'hôte, mais exécute l'application dans un environnement isolé.

- **Conteneur** : Package léger qui contient l'application et toutes ses dépendances, mais partage le noyau du système d'exploitation hôte
- **Avantages des conteneurs** : Légers (quelques Mo), démarrage rapide, efficaces en ressources
- **Inconvénients des conteneurs** : Isolation moins complète, limités aux systèmes d'exploitation compatibles

## Qu'est-ce que Docker ?

**Docker** est la plateforme de conteneurisation la plus populaire qui permet de créer, déployer et exécuter des applications dans des conteneurs.

### Concepts clés de Docker

- **Image Docker** : Modèle en lecture seule avec le système d'exploitation, l'application et les dépendances
- **Conteneur Docker** : Instance en cours d'exécution d'une image
- **Dockerfile** : Script de commandes pour construire une image
- **Docker Hub** : Référentiel en ligne pour partager des images Docker
- **Docker Compose** : Outil pour définir et exécuter des applications multi-conteneurs

## Pourquoi utiliser Docker avec Delphi ?

L'utilisation de Docker avec vos applications Delphi présente plusieurs avantages :

1. **Déploiement cohérent** : "Ça marche sur ma machine" devient "Ça marche partout"
2. **Isolation** : Chaque application dans son propre environnement, sans conflits
3. **Mise à l'échelle facile** : Déployez plusieurs instances rapidement
4. **Configuration simplifiée** : Pas besoin d'installer des dépendances sur chaque serveur
5. **Intégration aux pipelines CI/CD** : Automatisation plus facile des tests et du déploiement

## Types d'applications Delphi adaptées à Docker

Les applications Delphi les plus adaptées à la conteneurisation sont :

- **Applications console**
- **Services et démons**
- **Applications serveur**
- **API REST et services web**
- **Applications de traitement par lots**

Les applications avec interface graphique (GUI) sont moins adaptées aux conteneurs, bien que ce soit techniquement possible avec certaines configurations.

## Prérequis pour utiliser Docker avec Delphi

Avant de commencer, assurez-vous d'avoir :

1. **Docker Desktop** installé sur votre machine de développement :
   - [Télécharger Docker pour Windows](https://www.docker.com/products/docker-desktop)
   - [Télécharger Docker pour macOS](https://www.docker.com/products/docker-desktop)
   - Pour Linux, suivez les [instructions d'installation](https://docs.docker.com/engine/install/)

2. **Delphi** avec support pour la compilation Linux (si vous souhaitez créer des conteneurs Linux)
   - Delphi 10.2 Tokyo ou plus récent pour Linux

3. **Connaissances de base** sur la ligne de commande et les scripts

## Création d'une application Delphi compatible Docker

Commençons par créer une application Delphi simple adaptée à la conteneurisation.

### Étape 1 : Créer une application Delphi console

1. Ouvrez Delphi et créez un nouveau projet **Console Application**
2. Ajoutez le code suivant :

```pascal
program DockerDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

begin
  try
    WriteLn('Application Delphi dans Docker');
    WriteLn('-------------------------------');
    WriteLn('Heure actuelle : ' + TimeToStr(Now));
    WriteLn('Nom d''hôte     : ' + GetEnvironmentVariable('HOSTNAME'));
    WriteLn('-------------------------------');
    WriteLn('Appuyez sur Entrée pour quitter...');
    ReadLn;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
```

3. Compilez l'application pour **Win64** en mode **Release**

### Étape 2 : Préparer l'application pour Docker

Pour rendre votre application Delphi adaptée aux conteneurs Docker :

1. Évitez les dépendances aux ressources locales (registre Windows, fichiers locaux, etc.)
2. Utilisez des variables d'environnement pour la configuration
3. Gérez correctement la journalisation (sortie console ou fichiers dans des volumes montés)
4. Prévoyez un mécanisme d'arrêt propre (interceptez les signaux SIGTERM)

Pour notre exemple simple, l'application est déjà adaptée.

## Création d'un conteneur Docker Windows pour une application Delphi

Nous allons d'abord explorer la création d'un conteneur Docker Windows, plus facile pour commencer avec des applications Delphi existantes.

### Étape 1 : Créer un Dockerfile

Créez un fichier nommé `Dockerfile` (sans extension) dans le même dossier que votre exécutable compilé avec le contenu suivant :

```dockerfile
# Utilise une image Windows Server Core comme base
FROM mcr.microsoft.com/windows/servercore:ltsc2019

# Métadonnées de l'image
LABEL maintainer="Votre Nom <votre.email@exemple.com>"
LABEL version="1.0"
LABEL description="Application Delphi de démonstration Docker"

# Crée un dossier pour l'application
WORKDIR /app

# Copie l'exécutable Delphi dans le conteneur
COPY Win64/Release/DockerDemo.exe .

# Commande à exécuter quand le conteneur démarre
CMD ["DockerDemo.exe"]
```

### Étape 2 : Construire l'image Docker

Ouvrez une invite de commande ou PowerShell dans le dossier de votre projet et exécutez :

```powershell
docker build -t delphi-docker-demo .
```

Cette commande va construire une image Docker nommée `delphi-docker-demo` en utilisant les instructions du Dockerfile.

### Étape 3 : Exécuter le conteneur

Une fois l'image construite, vous pouvez exécuter un conteneur avec :

```powershell
docker run -it delphi-docker-demo
```

Le flag `-it` permet d'avoir un terminal interactif, nécessaire pour notre application qui attend une entrée utilisateur.

## Création d'un conteneur Docker Linux pour une application Delphi

Les conteneurs Linux sont beaucoup plus légers et plus largement utilisés que les conteneurs Windows. Voyons comment adapter notre application Delphi pour Linux.

### Étape 1 : Compiler pour Linux

1. Dans Delphi, changez la plateforme cible pour **Linux 64-bit**
2. Modifiez légèrement le code pour retirer la pause à la fin (les conteneurs Linux fonctionnent généralement sans interaction) :

```pascal
program DockerDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

begin
  try
    WriteLn('Application Delphi dans Docker (Linux)');
    WriteLn('--------------------------------------');
    WriteLn('Heure actuelle : ' + TimeToStr(Now));
    WriteLn('Nom d''hôte     : ' + GetEnvironmentVariable('HOSTNAME'));
    WriteLn('--------------------------------------');
    // Pas de ReadLn pour les conteneurs Linux
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
```

3. Compilez l'application pour Linux

### Étape 2 : Créer un Dockerfile pour Linux

```dockerfile
# Utilise une image Debian comme base
FROM debian:bullseye-slim

# Métadonnées de l'image
LABEL maintainer="Votre Nom <votre.email@exemple.com>"
LABEL version="1.0"
LABEL description="Application Delphi Linux de démonstration Docker"

# Installation des dépendances (bibliothèques nécessaires pour les apps Delphi Linux)
RUN apt-get update && apt-get install -y \
    libcurl4 \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# Crée un dossier pour l'application
WORKDIR /app

# Copie l'exécutable Delphi dans le conteneur
COPY Linux64/Release/DockerDemo .

# Rend l'exécutable exécutable (permissions)
RUN chmod +x DockerDemo

# Commande à exécuter quand le conteneur démarre
CMD ["./DockerDemo"]
```

### Étape 3 : Construire et exécuter l'image Linux

```bash
docker build -t delphi-linux-demo .
docker run delphi-linux-demo
```

## Création d'une application Delphi complète avec Docker

Maintenant, créons une application plus réaliste : un serveur REST Delphi qui pourrait faire partie d'une architecture microservices.

### Étape 1 : Créer une application serveur REST

1. Créez un nouveau projet **Console Application**
2. Ajoutez les unités `Horse` via GetIt Package Manager (ou utilisez `Indy` ou `mORMot`)
3. Voici un exemple simple utilisant Horse :

```pascal
program DelphiRestServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Get('/info',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(Format('{"server":"Delphi REST API","time":"%s","hostname":"%s"}',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
         GetEnvironmentVariable('HOSTNAME')]));
    end);

  WriteLn('Serveur Delphi REST démarré sur le port 9000');
  THorse.Listen(9000);
end.
```

4. Compilez pour Linux

### Étape 2 : Dockerfile pour le serveur REST

```dockerfile
FROM debian:bullseye-slim

LABEL maintainer="Votre Nom <votre.email@exemple.com>"
LABEL version="1.0"
LABEL description="Serveur REST Delphi dans Docker"

# Installation des dépendances
RUN apt-get update && apt-get install -y \
    libcurl4 \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY Linux64/Release/DelphiRestServer .
RUN chmod +x DelphiRestServer

# Expose le port 9000
EXPOSE 9000

# Définir un point d'entrée
ENTRYPOINT ["./DelphiRestServer"]
```

### Étape 3 : Construire et exécuter le serveur REST

```bash
docker build -t delphi-rest-server .
docker run -p 9000:9000 delphi-rest-server
```

Le paramètre `-p 9000:9000` mappe le port 9000 du conteneur au port 9000 de votre machine locale.

Vous pouvez maintenant tester le serveur en ouvrant `http://localhost:9000/ping` dans votre navigateur.

## Gestion des données avec Docker et Delphi

Les applications réelles ont souvent besoin de gérer des données. Voici comment gérer la persistance des données avec Docker :

### Volumes Docker

Les volumes Docker permettent de stocker des données en dehors du conteneur, ce qui les rend persistantes même si le conteneur est supprimé.

```bash
docker run -p 9000:9000 -v delphi-data:/app/data delphi-rest-server
```

Cela crée un volume nommé `delphi-data` et le monte dans le dossier `/app/data` du conteneur.

### Gestion des fichiers de configuration

Vous pouvez utiliser des variables d'environnement pour configurer votre application Delphi :

```bash
docker run -p 9000:9000 -e DATABASE_HOST=db.example.com -e API_KEY=secret123 delphi-rest-server
```

Dans votre code Delphi, récupérez ces valeurs avec `GetEnvironmentVariable` :

```pascal
var
  DBHost: string;
  ApiKey: string;
begin
  DBHost := GetEnvironmentVariable('DATABASE_HOST');
  ApiKey := GetEnvironmentVariable('API_KEY');
  // Utilisation de ces valeurs...
end;
```

## Docker Compose pour orchestrer plusieurs services

Pour les applications plus complexes, vous aurez souvent besoin de plusieurs conteneurs (par exemple, votre application Delphi + une base de données). Docker Compose permet de définir et gérer ces applications multi-conteneurs.

### Exemple de fichier docker-compose.yml

Créez un fichier `docker-compose.yml` :

```yaml
version: '3'

services:
  api:
    build: .
    ports:
      - "9000:9000"
    environment:
      - DATABASE_HOST=db
      - DATABASE_USER=myuser
      - DATABASE_PASSWORD=mypassword
    depends_on:
      - db
    volumes:
      - app-data:/app/data

  db:
    image: mariadb:10.5
    environment:
      - MYSQL_ROOT_PASSWORD=rootpassword
      - MYSQL_DATABASE=mydatabase
      - MYSQL_USER=myuser
      - MYSQL_PASSWORD=mypassword
    volumes:
      - db-data:/var/lib/mysql

volumes:
  app-data:
  db-data:
```

### Utilisation de Docker Compose

Pour lancer l'ensemble de l'application :

```bash
docker-compose up
```

Pour l'arrêter :

```bash
docker-compose down
```

## Bonnes pratiques pour Delphi et Docker

### 1. Gardez vos images légères

- Utilisez des images de base minimales (debian-slim, alpine)
- N'incluez que les dépendances nécessaires
- Supprimez les fichiers temporaires

### 2. Utilisez la mise en cache des couches Docker

Les Dockerfiles sont exécutés par couches, et Docker met en cache les couches inchangées. Organisez votre Dockerfile pour maximiser cette mise en cache :

```dockerfile
# Les dépendances changent rarement - mise en cache efficace
COPY dependencies.txt .
RUN install_dependencies.sh

# Le code source change souvent - placez-le après les dépendances
COPY source/ .
RUN compile_source.sh
```

### 3. Implémentez une gestion correcte des signaux

Les conteneurs peuvent recevoir des signaux de terminaison (SIGTERM). Assurez-vous que votre application Delphi les gère correctement :

```pascal
procedure HandleSignals(Signal: Integer); cdecl;
begin
  WriteLn('Signal reçu : ', Signal);
  // Nettoyage et arrêt propre

  // Signal au système que nous avons terminé
  ExitProcess(0);
end;

begin
  // Installation des gestionnaires de signaux
  signal(SIGTERM, @HandleSignals);
  signal(SIGINT, @HandleSignals);

  // Reste du programme...
end;
```

### 4. Utilisez la journalisation adaptée aux conteneurs

Les conteneurs s'attendent à ce que les applications journalisent sur stdout/stderr :

```pascal
procedure Log(const Msg: string);
begin
  WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
end;
```

### 5. Créez des builds spécifiques pour Docker

Compilez spécifiquement pour l'environnement Docker plutôt que d'utiliser des builds génériques.

## Déploiement en production

Lorsque vous êtes prêt à déployer votre application Delphi conteneurisée en production, vous avez plusieurs options :

### 1. Docker Swarm

Docker Swarm est une solution d'orchestration intégrée à Docker :

```bash
# Initialiser un swarm sur votre serveur
docker swarm init

# Déployer votre application en tant que stack
docker stack deploy -c docker-compose.yml myapp
```

### 2. Kubernetes

Pour les déploiements à plus grande échelle, Kubernetes est souvent préféré :

```bash
# Déployer sur Kubernetes
kubectl apply -f kubernetes-deployment.yml
```

### 3. Services cloud de conteneurs

Vous pouvez également utiliser des services cloud comme :

- AWS ECS (Elastic Container Service)
- Azure Container Instances
- Google Cloud Run

## Déboguer une application Delphi dans Docker

Le débogage d'applications dans des conteneurs peut être délicat. Voici quelques approches :

### 1. Journalisation détaillée

Ajoutez des journaux détaillés à votre application et utilisez `docker logs` pour les visualiser :

```bash
docker logs mon-conteneur-delphi
```

### 2. Exécution interactive

Lancez votre conteneur en mode interactif :

```bash
docker run -it --entrypoint /bin/bash delphi-app
```

### 3. Débogage à distance

Pour Linux, vous pouvez configurer un débogage à distance avec GDB.

## Exercice pratique : Conteneuriser une application Delphi existante

Suivez ces étapes pour conteneuriser une application Delphi existante :

1. Identifiez une application Delphi console ou serveur que vous avez déjà développée
2. Adaptez-la pour qu'elle fonctionne bien dans un conteneur (configuration par variables d'environnement, journalisation adéquate)
3. Créez un Dockerfile adapté à votre application
4. Construisez et testez l'image Docker
5. Déployez-la localement, puis en production si applicable

## Conclusion

La conteneurisation avec Docker offre de nombreux avantages pour les applications Delphi, en particulier pour les applications serveur, les services de traitement et les APIs. Bien que la courbe d'apprentissage puisse sembler raide au début, les bénéfices en termes de déploiement, de gestion et d'évolutivité en valent largement la peine.

Les conteneurs représentent l'avenir du déploiement d'applications, et avec les connaissances acquises dans ce chapitre, vous êtes maintenant prêt à moderniser votre workflow de développement Delphi et à profiter de tous les avantages de la conteneurisation.

Dans la prochaine section, nous explorerons le déploiement d'applications Delphi sur le Microsoft Store, une autre avenue moderne pour distribuer vos applications aux utilisateurs finaux.
