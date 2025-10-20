🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23. Conception d'applications Web avec Delphi

## Introduction générale

Aujourd'hui, le web est omniprésent. Que ce soit pour consulter ses emails, faire ses achats, gérer son entreprise ou se divertir, une grande partie de nos interactions numériques passe par des applications web accessibles depuis un navigateur.

En tant que développeur Delphi, vous vous demandez peut-être : **"Puis-je utiliser mes compétences Delphi pour créer des applications web ?"** La réponse est un **oui** retentissant !

Delphi offre plusieurs solutions pour concevoir des applications web, chacune avec ses propres caractéristiques et cas d'usage. Ce chapitre vous guidera à travers les différentes approches disponibles dans l'écosystème Delphi.

## Pourquoi développer des applications web avec Delphi ?

### Valoriser vos compétences existantes

Si vous maîtrisez déjà Delphi et l'Object Pascal, vous possédez un atout considérable :

- **Pas de nouveau langage à apprendre** : Vous continuez à travailler avec le Pascal que vous connaissez
- **Réutilisation du code** : Votre logique métier existante peut souvent être réemployée
- **Environnement familier** : L'IDE Delphi que vous utilisez quotidiennement
- **Productivité immédiate** : Pas de courbe d'apprentissage majeure

### Étendre la portée de vos applications

Les applications web offrent des avantages uniques :

- **Accessibilité universelle** : Utilisable depuis n'importe quel appareil avec un navigateur
- **Pas d'installation cliente** : Les utilisateurs accèdent directement via une URL
- **Mises à jour centralisées** : Une seule mise à jour profite à tous les utilisateurs
- **Multi-plateforme naturel** : Windows, macOS, Linux, smartphones, tablettes
- **Collaboration facilitée** : Plusieurs utilisateurs peuvent travailler simultanément

### Répondre aux attentes modernes

Les entreprises et les utilisateurs s'attendent aujourd'hui à :

- Pouvoir accéder aux applications depuis n'importe où
- Travailler sur différents appareils de manière transparente
- Collaborer en temps réel
- Bénéficier d'interfaces modernes et réactives

## Les différentes approches du web avec Delphi

Delphi propose plusieurs chemins pour créer des applications web. Voici une vue d'ensemble :

### 1. Applications web côté serveur

Dans cette approche, votre application Delphi s'exécute sur un serveur et génère dynamiquement les pages web envoyées aux navigateurs clients.

**Caractéristiques :**
- Le code Delphi reste sur le serveur
- Génération dynamique de HTML
- Gestion des sessions utilisateur
- Modèle traditionnel client-serveur

**Technologies Delphi :**
- IntraWeb
- WebBroker
- Applications CGI/ISAPI

### 2. Applications web côté client

Ici, votre code Delphi est transformé en JavaScript et s'exécute directement dans le navigateur de l'utilisateur.

**Caractéristiques :**
- L'application fonctionne dans le navigateur
- Transcompilation Pascal → JavaScript
- Interface utilisateur moderne et réactive
- Architecture similaire aux applications JavaScript actuelles

**Technologies Delphi :**
- TMS Web Core
- Smart Mobile Studio (anciennement)

### 3. Services web et API REST

Cette approche consiste à créer des services web (backend) avec Delphi, qui communiquent avec n'importe quel frontend web.

**Caractéristiques :**
- Séparation frontend / backend
- Architecture moderne et scalable
- Exposition de données via API REST
- Le frontend peut être créé avec n'importe quelle technologie

**Technologies Delphi :**
- RAD Server (EMS - Enterprise Mobility Services)
- DataSnap
- Frameworks tiers (Horse, MARS, etc.)

### 4. Applications hybrides

Combinaison de plusieurs approches pour tirer le meilleur de chaque monde.

**Exemple typique :**
- Frontend moderne (TMS Web Core ou framework JavaScript)
- Backend Delphi (services REST)
- Base de données gérée par Delphi

## Concepts fondamentaux du web

Avant de plonger dans les technologies spécifiques, comprenons quelques concepts essentiels du développement web.

### Le modèle client-serveur

```
┌─────────────────┐                    ┌─────────────────┐
│   Navigateur    │  ← Requête HTTP    │   Serveur Web   │
│    (Client)     │  ─────────────→    │                 │
│                 │                    │                 │
│                 │  ← Réponse HTML    │                 │
│                 │  ←─────────────    │                 │
└─────────────────┘                    └─────────────────┘
```

**Le navigateur (client) :**
- Affiche les pages web
- Interprète HTML, CSS et JavaScript
- Envoie des requêtes au serveur

**Le serveur :**
- Reçoit les requêtes des clients
- Traite la logique métier
- Génère et envoie les réponses

### Les technologies web de base

Même si Delphi vous permet de les abstraire en grande partie, il est utile de connaître les briques fondamentales du web :

**HTML (HyperText Markup Language)**
- Structure et contenu des pages web
- Balises pour organiser le texte, les images, les liens, etc.
- C'est le "squelette" d'une page web

**CSS (Cascading Style Sheets)**
- Apparence visuelle des pages
- Couleurs, polices, mise en page, animations
- C'est la "peau" qui habille le squelette HTML

**JavaScript**
- Interactivité et comportement dynamique
- Réagit aux actions de l'utilisateur
- Communique avec le serveur sans recharger la page
- C'est le "système nerveux" qui rend la page vivante

**HTTP (HyperText Transfer Protocol)**
- Protocole de communication entre client et serveur
- Méthodes : GET (récupérer), POST (envoyer), PUT (modifier), DELETE (supprimer)
- C'est le "langage" que parlent le navigateur et le serveur

### Architecture web moderne

Les applications web modernes suivent souvent une architecture en trois couches :

```
┌─────────────────────────────────────────┐
│         Couche Présentation             │
│  (Interface utilisateur dans navigateur)│
└──────────────┬──────────────────────────┘
               │
               │ API REST / HTTP
               │
┌──────────────┴──────────────────────────┐
│         Couche Métier                   │
│  (Logique applicative sur serveur)      │
└──────────────┬──────────────────────────┘
               │
               │ SQL / Requêtes
               │
┌──────────────┴──────────────────────────┐
│         Couche Données                  │
│  (Base de données)                       │
└─────────────────────────────────────────┘
```

Cette séparation offre plusieurs avantages :
- **Modularité** : Chaque couche peut évoluer indépendamment
- **Réutilisabilité** : Le backend peut servir plusieurs frontends
- **Scalabilité** : Possibilité de dimensionner chaque couche selon les besoins
- **Maintenabilité** : Code mieux organisé et plus facile à maintenir

## Comparaison des approches Delphi

### Côté serveur (IntraWeb, WebBroker)

**Avantages :**
- Code et logique métier protégés sur le serveur
- Familier pour les développeurs VCL
- Contrôle total de l'application
- Pas de JavaScript à écrire

**Inconvénients :**
- Nécessite un serveur d'application Delphi
- Performance dépendante du réseau
- Moins réactif qu'une application client
- Coûts d'hébergement plus élevés

**Idéal pour :**
- Applications intranet d'entreprise
- Tableaux de bord de gestion
- Applications avec logique complexe côté serveur
- Migration d'applications VCL existantes

### Côté client (TMS Web Core)

**Avantages :**
- Application moderne et réactive
- Fonctionne hors ligne (avec PWA)
- Hébergement simple et économique
- Excellente expérience utilisateur

**Inconvénients :**
- Code JavaScript visible (moins sécurisé)
- Nécessite un backend séparé pour les données
- Courbe d'apprentissage pour les concepts web
- Dépendance aux capacités du navigateur

**Idéal pour :**
- Applications web publiques
- SaaS (Software as a Service)
- Applications nécessitant réactivité
- Progressive Web Apps (PWA)

### Services REST (RAD Server, DataSnap)

**Avantages :**
- Architecture moderne et flexible
- Frontend indépendant de la technologie
- Excellente scalabilité
- Réutilisation du backend pour mobile/web/desktop

**Inconvénients :**
- Nécessite de créer deux applications (frontend + backend)
- Plus complexe à mettre en place initialement
- Gestion de l'authentification et sécurité à prévoir

**Idéal pour :**
- Applications d'entreprise à grande échelle
- Systèmes multi-clients (web + mobile + desktop)
- Microservices
- API publiques ou partenaires

## Évolution du développement web avec Delphi

### Les débuts

Dans les années 90 et début 2000, le développement web avec Delphi se limitait principalement à :
- CGI (Common Gateway Interface)
- ISAPI (Internet Server API)
- WebBroker

Ces technologies, bien que fonctionnelles, étaient assez basiques et nécessitaient beaucoup de code HTML manuel.

### L'ère RAD

Avec l'introduction d'IntraWeb et plus tard de DataSnap, Delphi a apporté sa philosophie RAD (Rapid Application Development) au web :
- Développement visuel
- Composants réutilisables
- Génération automatique de code
- Productivité accrue

### L'approche moderne

Aujourd'hui, Delphi s'adapte aux standards web modernes :
- **TMS Web Core** : Applications Single Page (SPA)
- **RAD Server** : API REST natives
- **Support PWA** : Applications web progressives
- **WebSockets** : Communication temps réel
- Intégration avec frameworks JavaScript modernes

## Choisir la bonne approche

Le choix de la technologie dépend de plusieurs facteurs :

### Questions à se poser

1. **Qui sont vos utilisateurs ?**
   - Intranet d'entreprise → Côté serveur
   - Public large → Côté client ou hybride

2. **Quels sont vos besoins de sécurité ?**
   - Code ultra-protégé → Côté serveur
   - Standard web → Côté client acceptable

3. **Quel est votre budget d'hébergement ?**
   - Limité → Côté client (hébergement statique)
   - Confortable → Côté serveur possible

4. **Quelle expérience utilisateur visez-vous ?**
   - Application réactive → Côté client
   - Application standard → Côté serveur suffisant

5. **Devez-vous supporter le mode hors ligne ?**
   - Oui → TMS Web Core avec PWA
   - Non → Toutes les options possibles

6. **Avez-vous déjà une application Delphi à migrer ?**
   - Application VCL → IntraWeb facilite la transition
   - Nouvelle application → TMS Web Core pour moderne

### Matrice de décision simplifiée

| Besoin principal | Solution recommandée |
|------------------|---------------------|
| Migration VCL rapide | IntraWeb |
| Application intranet | IntraWeb ou WebBroker |
| Application web moderne | TMS Web Core |
| SaaS / Application publique | TMS Web Core + Services REST |
| Backend pour mobile | RAD Server / DataSnap |
| Tableau de bord temps réel | TMS Web Core + WebSockets |
| API publique | RAD Server |

## Prérequis techniques

### Connaissances nécessaires

Pour développer efficacement des applications web avec Delphi, il est utile (mais pas obligatoire) de connaître :

**Essentiels :**
- Object Pascal (bien sûr !)
- Bases de données (SQL)
- Concepts client-serveur

**Recommandés :**
- Notions de HTML/CSS (compréhension de base)
- HTTP et REST (principes fondamentaux)
- JSON (format d'échange de données)

**Optionnels mais utiles :**
- JavaScript (pour personnalisations avancées)
- Sécurité web (HTTPS, authentification)
- Outils de développement navigateur (Chrome DevTools, Firefox Developer Tools)

### Environnement de développement

**Pour commencer le développement web avec Delphi, vous aurez besoin de :**

1. **Delphi** (version récente recommandée)
   - Community Edition acceptable pour débuter
   - Professional ou supérieur pour IntraWeb inclus

2. **Navigateur web moderne**
   - Chrome, Firefox, Edge ou Safari
   - Avec outils de développement activés

3. **Serveur web (selon l'approche)**
   - IIS (Windows)
   - Apache (multi-plateforme)
   - Nginx (léger et performant)
   - Serveur de développement intégré (IntraWeb)

4. **Composants additionnels (optionnels)**
   - IntraWeb (inclus ou standalone)
   - TMS Web Core (licence commerciale)
   - RAD Server (licence Enterprise)

## Structure de ce chapitre

Dans les sections suivantes, nous explorerons en détail :

**23.1 - Introduction à IntraWeb et TMS Web Core**
- Présentation approfondie de ces deux frameworks
- Comparaison détaillée
- Premiers pas avec chacun

**23.2 - Applications Web basées sur VCL**
- Migration d'applications existantes
- Techniques et astuces

**23.3 - Création de services REST avec Delphi**
- Architecture REST
- Implémentation avec Delphi
- Sécurisation et déploiement

**23.4 - Utilisation de WebBroker et DataSnap**
- Technologies historiques mais toujours pertinentes
- Cas d'usage spécifiques

**23.5 - Développement de sites Web dynamiques**
- Génération de contenu
- Templates et moteurs de rendu

**23.6 - Intégration avec des frameworks JavaScript**
- Communication entre Delphi et JavaScript
- Hybridation des technologies

**23.7 - Progressive Web Apps (PWA)**
- Transformer votre application web en PWA
- Mode hors ligne et notifications

**23.8 - WebAssembly et Delphi**
- Futur du web avec WebAssembly
- Perspectives pour Delphi

**23.9 - WebStencils : intégration côté serveur améliorée**
- Nouvelles approches de templating
- Performances optimisées

## Conclusion de l'introduction

Le développement web avec Delphi n'est plus une option exotique, mais une réalité mature et productive. Que vous choisissiez une approche côté serveur traditionnelle, une solution client moderne, ou une architecture distribuée avec services REST, Delphi vous offre les outils pour créer des applications web professionnelles.

L'avantage majeur reste votre expertise existante : vous pouvez créer des applications web sans abandonner l'écosystème Delphi et en capitalisant sur vos années d'expérience avec l'Object Pascal.

Dans les sections qui suivent, nous allons explorer concrètement chacune de ces approches, avec des explications claires et des exemples pratiques pour vous permettre de choisir et maîtriser la technologie qui correspond le mieux à vos besoins.

Prêt à faire le grand saut dans le web avec Delphi ? Commençons par découvrir IntraWeb et TMS Web Core dans la section suivante !

⏭️ [Introduction à Intraweb et TMS Web Core](/23-conception-dapplications-web-avec-delphi/01-introduction-a-intraweb-et-tms-web-core.md)
