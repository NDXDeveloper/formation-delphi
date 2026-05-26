🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.7 Progressive Web Apps (PWA)

## Introduction

Les **Progressive Web Apps (PWA)** représentent l'évolution moderne des applications web. Elles combinent le meilleur des applications web (accessibilité via URL, pas d'installation via store) et des applications natives (fonctionnement hors ligne, installation sur l'écran d'accueil, notifications push).

**Analogie :** Une PWA est comme un caméléon qui se comporte comme une application native tout en restant une application web.

Imaginez que vous créez une application web et que, sans effort supplémentaire majeur, vos utilisateurs peuvent :
- L'installer sur leur téléphone ou ordinateur comme une vraie application
- L'utiliser même sans connexion Internet
- Recevoir des notifications
- Avoir une expérience fluide et rapide

C'est exactement ce que permettent les PWA !

## Qu'est-ce qu'une Progressive Web App ?

### Définition

Une PWA est une **application web** qui utilise des technologies modernes pour offrir une **expérience similaire à une application native**.

### Les caractéristiques d'une PWA

**1. Progressive (Progressive)**
- Fonctionne pour tous les utilisateurs
- S'améliore progressivement selon les capacités du navigateur
- Expérience de base même sur navigateurs anciens

**2. Responsive (Adaptative)**
- S'adapte à toutes les tailles d'écran
- Fonctionne sur desktop, mobile, tablette
- Interface fluide et cohérente

**3. Connectivity Independent (Indépendante de la connexion)**
- Fonctionne hors ligne ou avec connexion lente
- Mise en cache intelligente des ressources
- Synchronisation en arrière-plan

**4. App-like (Semblable à une app native)**
- Navigation fluide sans rechargement de page
- Interactions rapides et réactives
- Plein écran possible

**5. Fresh (Toujours à jour)**
- Mise à jour automatique grâce aux Service Workers
- Contenu toujours frais
- Pas de version obsolète

**6. Safe (Sécurisée)**
- HTTPS obligatoire
- Communications chiffrées
- Pas d'intermédiaire malveillant

**7. Discoverable (Découvrable)**
- Indexable par les moteurs de recherche
- Référencement SEO normal
- Pas besoin d'App Store

**8. Re-engageable (Engagement utilisateur)**
- Notifications push
- Icône sur l'écran d'accueil
- Retour utilisateur facilité

**9. Installable (Installable)**
- Ajout à l'écran d'accueil
- Lancement en plein écran
- Apparence d'application native

**10. Linkable (Partageable)**
- Simple URL à partager
- Pas d'installation complexe
- Accès instantané

### Comparaison avec les autres types d'applications

| Caractéristique | Site Web classique | PWA | Application Native |
|----------------|-------------------|-----|-------------------|
| **Installation** | Aucune | Optionnelle | Obligatoire (Store) |
| **Hors ligne** | ❌ Non | ✅ Oui | ✅ Oui |
| **Notifications** | ❌ Non | ✅ Oui | ✅ Oui |
| **Performance** | Variable | Bonne | Excellente |
| **Mise à jour** | Instantanée | Automatique | Manuelle (Store) |
| **Découvrabilité** | ✅ SEO | ✅ SEO | ❌ Store uniquement |
| **Développement** | Web standard | Web standard | Natif par plateforme |
| **Coût** | Faible | Faible/Moyen | Élevé |

## Les technologies clés d'une PWA

### 1. Le Manifest (manifeste.json)

Le fichier manifeste est un fichier JSON qui décrit votre application.

**manifest.json :**
```json
{
  "name": "Mon Application Delphi",
  "short_name": "MonApp",
  "description": "Une PWA créée avec Delphi",
  "start_url": "/",
  "display": "standalone",
  "background_color": "#ffffff",
  "theme_color": "#2196F3",
  "orientation": "portrait-primary",
  "icons": [
    {
      "src": "/images/icon-72x72.png",
      "sizes": "72x72",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-96x96.png",
      "sizes": "96x96",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-128x128.png",
      "sizes": "128x128",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-144x144.png",
      "sizes": "144x144",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-152x152.png",
      "sizes": "152x152",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-192x192.png",
      "sizes": "192x192",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-384x384.png",
      "sizes": "384x384",
      "type": "image/png",
      "purpose": "any maskable"
    },
    {
      "src": "/images/icon-512x512.png",
      "sizes": "512x512",
      "type": "image/png",
      "purpose": "any maskable"
    }
  ]
}
```

**Explication des propriétés :**

- **name** : Nom complet de l'application (affiché lors de l'installation)
- **short_name** : Nom court (affiché sous l'icône)
- **description** : Description de l'application
- **start_url** : URL de démarrage de l'application
- **display** : Mode d'affichage
  - `standalone` : comme une app native (recommandé)
  - `fullscreen` : plein écran complet
  - `minimal-ui` : interface minimale
  - `browser` : dans le navigateur normal
- **background_color** : Couleur de fond au démarrage
- **theme_color** : Couleur du thème (barre d'état, etc.)
- **orientation** : Orientation préférée
- **icons** : Icônes de l'application (différentes tailles)

💡 **Astuce icônes** : `"purpose": "any maskable"` annonce à
Android/Chrome que l'icône peut être utilisée à la fois en mode  
classique (any) ET en mode adaptatif (maskable). Pour un rendu  
optimal, fournir **deux séries d'icônes distinctes** : une *any*  
sans padding (l'icône occupe tout le carré) et une *maskable* avec  
un padding de ~10 % autour (la *safe zone* pour que les masques  
circulaires/squircle d'Android ne coupent pas le logo). Le validateur  
[maskable.app](https://maskable.app/editor) aide à concevoir des
icônes maskable.

**Inclure le manifeste dans votre HTML :**
```html
<link rel="manifest" href="/manifest.json">
```

### 2. Les Service Workers

Un **Service Worker** est un script JavaScript qui fonctionne en arrière-plan, séparé de votre page web. Il intercepte les requêtes réseau et gère le cache.

**Schéma de fonctionnement :**
```
┌─────────────────┐
│  Application    │
│  Web            │
└────────┬────────┘
         │
         │ Requêtes
         ↓
┌────────────────────┐
│  Service Worker    │ → Cache Storage
│  (Intercepteur)    │
└────────┬───────────┘
         │
         │ Si pas en cache
         ↓
┌────────────────────┐
│  Réseau / Serveur  │
└────────────────────┘
```

**service-worker.js (exemple basique) :**
```javascript
// Nom du cache
const CACHE_NAME = 'mon-app-v1';

// Fichiers à mettre en cache
const urlsToCache = [
  '/',
  '/index.html',
  '/css/style.css',
  '/js/app.js',
  '/images/logo.png'
];

// Installation du Service Worker
self.addEventListener('install', function(event) {
  console.log('Service Worker: Installation');

  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(function(cache) {
        console.log('Service Worker: Mise en cache des fichiers');
        return cache.addAll(urlsToCache);
      })
  );
});

// Activation du Service Worker
self.addEventListener('activate', function(event) {
  console.log('Service Worker: Activation');

  // Nettoyer les anciens caches
  event.waitUntil(
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          if (cacheName !== CACHE_NAME) {
            console.log('Service Worker: Suppression ancien cache', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});

// Interception des requêtes
self.addEventListener('fetch', function(event) {
  // ⚠️ Le Cache API n'accepte QUE les requêtes GET. Pour POST/PUT/DELETE,
  //    on laisse le navigateur gérer normalement (sans cache).
  if (event.request.method !== 'GET') {
    return;
  }

  event.respondWith(
    caches.match(event.request)
      .then(function(response) {
        // Retourner depuis le cache si disponible
        if (response) {
          console.log('Service Worker: Réponse depuis le cache', event.request.url);
          return response;
        }

        // Sinon, aller chercher sur le réseau
        console.log('Service Worker: Requête réseau', event.request.url);
        return fetch(event.request).then(function(response) {
          // Mettre en cache la nouvelle ressource (statut 200 seulement)
          if (response && response.status === 200 && response.type === 'basic') {
            const responseToCache = response.clone();
            caches.open(CACHE_NAME).then(function(cache) {
              cache.put(event.request, responseToCache);
            });
          }
          return response;
        });
      })
  );
});
```

> 💡 `response.type === 'basic'` évite de cacher les **réponses opaques**  
> (CORS sans `Access-Control-Allow-Origin`) qui apparaissent avec une  
> taille de 0 octet une fois en cache et peuvent corrompre l'expérience  
> hors ligne.

**Enregistrer le Service Worker (dans votre page HTML) :**
```html
<script>
  // Vérifier que les Service Workers sont supportés
  if ('serviceWorker' in navigator) {
    window.addEventListener('load', function() {
      navigator.serviceWorker.register('/service-worker.js')
        .then(function(registration) {
          console.log('Service Worker enregistré avec succès:', registration.scope);
        })
        .catch(function(error) {
          console.log('Échec enregistrement Service Worker:', error);
        });
    });
  }
</script>
```

### 3. HTTPS obligatoire

Les PWA nécessitent **HTTPS** pour des raisons de sécurité.

**Exceptions :**
- `localhost` (pour le développement)
- `127.0.0.1` (pour le développement)

**Pour obtenir HTTPS :**
- Certificat Let's Encrypt (gratuit)
- Certificats payants
- Services cloud avec HTTPS inclus

## Créer une PWA avec TMS Web Core

TMS Web Core est la solution Delphi la plus adaptée pour créer des PWA car le code est transcompilé en JavaScript et s'exécute dans le navigateur.

### Structure du projet

```
MonProjetPWA/
├── Project1.dpr              (Projet principal Delphi)
├── Unit1.pas                 (Unité principale)
├── Unit1.dfm                 (Formulaire)
└── PWA/                      (Dossier pour fichiers PWA)
    ├── manifest.json
    ├── service-worker.js
    └── icons/
        ├── icon-192x192.png
        └── icon-512x512.png
```

### Code Delphi pour PWA

```pascal
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.ExtCtrls;

type
  TForm1 = class(TWebForm)
    WebPanel1: TWebPanel;
    WebLabel1: TWebLabel;
    WebButton1: TWebButton;
    WebButton2: TWebButton;
    WebMemo1: TWebMemo;

    procedure WebFormCreate(Sender: TObject);
    procedure WebButton1Click(Sender: TObject);
    procedure WebButton2Click(Sender: TObject);
  private
    FIsOnline: Boolean;
    procedure CheckOnlineStatus;
    procedure RegisterServiceWorker;
    procedure CheckInstallPrompt;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);  
begin  
  WebLabel1.Caption := 'Ma PWA Delphi';
  WebButton1.Caption := 'Vérifier connexion';
  WebButton2.Caption := 'Installer l''application';

  // Enregistrer le Service Worker
  RegisterServiceWorker;

  // Vérifier le statut en ligne
  CheckOnlineStatus;

  // Gérer le bouton d'installation
  CheckInstallPrompt;

  // Écouter les changements de connexion
  asm
    window.addEventListener('online', function() {
      Form1.FIsOnline = true;
      Form1.CheckOnlineStatus();
    });

    window.addEventListener('offline', function() {
      Form1.FIsOnline = false;
      Form1.CheckOnlineStatus();
    });
  end;
end;

procedure TForm1.RegisterServiceWorker;  
begin  
  asm
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.register('/service-worker.js')
        .then(function(registration) {
          console.log('Service Worker enregistré:', registration.scope);
        })
        .catch(function(error) {
          console.log('Erreur Service Worker:', error);
        });
    }
  end;
end;

procedure TForm1.CheckOnlineStatus;  
var  
  IsOnline: Boolean;
begin
  asm
    IsOnline = navigator.onLine;
  end;

  FIsOnline := IsOnline;

  if FIsOnline then
  begin
    WebMemo1.Lines.Add('✅ Vous êtes en ligne');
    WebPanel1.Color := clLime;
  end
  else
  begin
    WebMemo1.Lines.Add('❌ Vous êtes hors ligne');
    WebPanel1.Color := clRed;
  end;
end;

procedure TForm1.WebButton1Click(Sender: TObject);  
begin  
  CheckOnlineStatus;
end;

procedure TForm1.CheckInstallPrompt;  
begin  
  asm
    var deferredPrompt;

    window.addEventListener('beforeinstallprompt', function(e) {
      // Empêcher l'affichage automatique
      e.preventDefault();
      deferredPrompt = e;

      // Afficher notre bouton d'installation
      Form1.WebButton2.Visible = true;
    });

    // Gérer le clic sur le bouton d'installation
    Form1.WebButton2.onclick = function() {
      if (deferredPrompt) {
        // Afficher le prompt d'installation
        deferredPrompt.prompt();

        deferredPrompt.userChoice.then(function(choiceResult) {
          if (choiceResult.outcome === 'accepted') {
            console.log('PWA installée');
            Form1.WebMemo1.Lines.Add('✅ Application installée !');
          } else {
            console.log('Installation refusée');
            Form1.WebMemo1.Lines.Add('❌ Installation annulée');
          }
          deferredPrompt = null;
        });
      }
    };

    // Détecter si déjà installée
    window.addEventListener('appinstalled', function() {
      console.log('PWA installée');
      Form1.WebButton2.Visible = false;
      Form1.WebMemo1.Lines.Add('✅ Application installée avec succès !');
    });
  end;
end;

procedure TForm1.WebButton2Click(Sender: TObject);  
begin  
  // Le clic est géré dans le code JavaScript ci-dessus
end;

end.
```

### Configuration du manifeste pour TMS Web Core

**Dans le projet TMS Web Core :**

1. Créer le fichier `manifest.json` dans le dossier du projet
2. L'inclure dans le HTML généré

**Project HTML Template :**
```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="Ma PWA Delphi">
  <meta name="theme-color" content="#2196F3">

  <title>Ma PWA Delphi</title>

  <!-- Manifeste PWA -->
  <link rel="manifest" href="manifest.json">

  <!-- Icônes pour iOS -->
  <link rel="apple-touch-icon" href="icons/icon-192x192.png">
  <!-- Versions iOS Safari : balise apple-* historique + sa variante
       standardisée mobile-web-app-capable (W3C). Mettre les deux pour
       compatibilité maximale. -->
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black">
  <meta name="apple-mobile-web-app-title" content="MonApp">

  <!-- Splash screen pour iOS -->
  <link rel="apple-touch-startup-image" href="icons/icon-512x512.png">
</head>
<body>
  <!-- Votre application TMS Web Core sera injectée ici -->
</body>
</html>
```

## Stratégies de mise en cache

### Stratégie 1 : Cache First (Cache d'abord)

Parfait pour : Assets statiques (CSS, JS, images)

```javascript
self.addEventListener('fetch', function(event) {
  if (event.request.method !== 'GET') return;  // Cache API = GET only

  event.respondWith(
    caches.match(event.request)
      .then(function(response) {
        // Retourner depuis le cache, sinon réseau
        return response || fetch(event.request);
      })
  );
});
```

### Stratégie 2 : Network First (Réseau d'abord)

Parfait pour : API, données dynamiques

```javascript
self.addEventListener('fetch', function(event) {
  // ⚠️ Cache.put() N'accepte QUE les requêtes GET — un POST/PUT/DELETE
  //    déclenchera "Request method 'POST' is unsupported". On laisse
  //    donc passer les requêtes non-GET sans les toucher.
  if (event.request.method !== 'GET') {
    return;  // Le navigateur gère la requête normalement
  }

  event.respondWith(
    fetch(event.request)
      .then(function(response) {
        // Mettre en cache pour utilisation hors ligne — uniquement les
        // réponses 200 OK : ne pas cacher les 404 ou les redirections opaques.
        if (response && response.status === 200) {
          const responseToCache = response.clone();
          caches.open(CACHE_NAME).then(function(cache) {
            cache.put(event.request, responseToCache);
          });
        }
        return response;
      })
      .catch(function() {
        // Si le réseau échoue, utiliser le cache
        return caches.match(event.request);
      })
  );
});
```

### Stratégie 3 : Stale While Revalidate

Parfait pour : Équilibre entre fraîcheur et rapidité

```javascript
self.addEventListener('fetch', function(event) {
  if (event.request.method !== 'GET') return;

  event.respondWith(
    caches.open(CACHE_NAME).then(function(cache) {
      return cache.match(event.request).then(function(cachedResponse) {
        // Lancer la requête réseau en parallèle
        const fetchPromise = fetch(event.request).then(function(networkResponse) {
          // Ne cacher QUE les réponses 200 basic (ni 404, ni opaques)
          if (networkResponse && networkResponse.status === 200 &&
              networkResponse.type === 'basic') {
            cache.put(event.request, networkResponse.clone());
          }
          return networkResponse;
        });

        // Retourner le cache immédiatement, ou attendre le réseau
        return cachedResponse || fetchPromise;
      });
    })
  );
});
```

### Stratégie 4 : Cache avec délai réseau

Parfait pour : Expérience utilisateur optimale

```javascript
self.addEventListener('fetch', function(event) {
  if (event.request.method !== 'GET') return;

  event.respondWith(
    new Promise(function(resolve, reject) {
      const timeoutId = setTimeout(function() {
        // Après 500ms, basculer sur le cache (si présent ; sinon Response
        // sera undefined et le navigateur affichera une erreur réseau).
        caches.match(event.request).then(function(cached) {
          if (cached) resolve(cached);
          // Sinon, on laisse le fetch en cours continuer
        });
      }, 500);

      fetch(event.request).then(function(response) {
        clearTimeout(timeoutId);
        resolve(response);
      }).catch(function(err) {
        clearTimeout(timeoutId);
        // Réseau KO → essayer le cache, sinon propager l'erreur.
        caches.match(event.request).then(function(cached) {
          if (cached) resolve(cached); else reject(err);
        });
      });
    })
  );
});
```

## Synchronisation en arrière-plan

### Background Sync API

Permet de différer des actions jusqu'à ce que l'utilisateur ait une connexion stable.

> ⚠️ **Compatibilité** : Background Sync est disponible dans Chrome,  
> Edge, Opera et la plupart des navigateurs Chromium. **Pas supporté  
> dans Firefox ni Safari** (iOS inclus, quel que soit le pays). Pour  
> une application multi-navigateurs, prévoir un fallback (retry au  
> prochain chargement de page via `online` event listener).

**Exemple : Envoyer des données en différé**

```javascript
// Dans votre application — vérifier le support avant
if ('serviceWorker' in navigator && 'SyncManager' in window) {
  navigator.serviceWorker.ready.then(function(registration) {
    return registration.sync.register('envoi-donnees');
  });
}

// Dans le Service Worker
self.addEventListener('sync', function(event) {
  if (event.tag === 'envoi-donnees') {
    event.waitUntil(
      // Récupérer les données en attente depuis IndexedDB
      getDonneesEnAttente().then(function(donnees) {
        // Envoyer au serveur — Content-Type explicite pour le backend
        return fetch('/api/sync', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(donnees)
        });
        // Si fetch rejette (réseau toujours KO), le navigateur retentera
        // automatiquement plus tard avec backoff.
      })
    );
  }
});
```

## Stockage local pour PWA

### LocalStorage (simple mais limité)

> 💡 **Caractéristiques importantes** :  
> - Quota : **~5-10 Mo par origin** selon le navigateur (à comparer  
>   aux centaines de Mo de IndexedDB)  
> - Stockage **synchrone** — bloque le thread principal de l'UI. À éviter  
>   pour de gros objets ou des opérations fréquentes  
> - Valeurs **toujours en string** — un `JSON.stringify` est nécessaire  
>   pour stocker des objets  
> - **Pas chiffré** : ne JAMAIS y stocker tokens d'auth, mots de passe ou  
>   données sensibles (cf. avertissement JWT du chapitre 23.6)

```pascal
// TMS Web Core - Sauvegarder des données
procedure TForm1.SaveData;  
begin  
  asm
    localStorage.setItem('userName', 'Jean Dupont');
    localStorage.setItem('lastVisit', new Date().toISOString());
  end;
end;

// Récupérer des données
procedure TForm1.LoadData;  
var  
  UserName: string;
begin
  asm
    UserName = localStorage.getItem('userName') || 'Invité';
  end;

  WebLabel1.Caption := 'Bonjour ' + UserName;
end;
```

### IndexedDB (base de données locale)

Plus puissant que localStorage, permet de stocker des objets complexes.

```javascript
// Variable globale pour conserver la référence à la base de données.
// (IndexedDB est asynchrone : l'ouverture est différée.)
let db = null;

// Ouvrir la base de données
const request = indexedDB.open('MaBaseDonnees', 1);

request.onupgradeneeded = function(event) {
  const upgradeDb = event.target.result;

  // Créer un object store (uniquement à la création ou changement de version)
  const objectStore = upgradeDb.createObjectStore('clients',
    { keyPath: 'id', autoIncrement: true });
  objectStore.createIndex('nom', 'nom', { unique: false });
};

request.onerror = function(event) {
  console.error('Erreur IndexedDB :', event.target.error);
};

request.onsuccess = function(event) {
  db = event.target.result;   // ⚠️ Stocker pour usage ultérieur
  console.log('IndexedDB prête');

  // Ajouter un premier exemple
  const tx = db.transaction(['clients'], 'readwrite');
  tx.objectStore('clients').add({
    nom: 'Dupont',
    prenom: 'Jean',
    email: 'jean@example.com'
  });
};

// Lire des données — à n'appeler qu'APRÈS que `db` soit défini (onsuccess).
function lireClients() {
  if (!db) {
    console.warn('IndexedDB pas encore initialisée');
    return;
  }
  const transaction = db.transaction(['clients'], 'readonly');
  const objectStore = transaction.objectStore('clients');
  const req = objectStore.getAll();

  req.onsuccess = function() {
    console.log('Clients:', req.result);
  };
}
```

## Notifications Push

Les notifications push permettent de ré-engager les utilisateurs.

### Configuration des notifications

> ⚠️ **Demander la permission uniquement sur un clic utilisateur** : depuis  
> Chrome 80, Firefox 72 et Safari, appeler `Notification.requestPermission()`  
> au chargement de la page est refusé silencieusement (ou pénalise le site  
> dans le « Notification Permissions UI »). Toujours déclencher la demande  
> en réponse à une action explicite (clic sur un bouton « Activer les  
> notifications »).

```javascript
// À appeler depuis un gestionnaire de clic, pas au chargement de la page !
btnEnableNotifications.addEventListener('click', () => {
  Notification.requestPermission().then(function(permission) {
    if (permission === 'granted') {
      console.log('Permission accordée');
      afficherNotification();
    }
  });
});

// Afficher une notification
function afficherNotification() {
  const options = {
    body: 'Vous avez un nouveau message',
    icon: '/images/icon-192x192.png',
    badge: '/images/badge-72x72.png',
    vibrate: [200, 100, 200],
    data: { url: '/messages' },
    actions: [
      { action: 'ouvrir', title: 'Ouvrir' },
      { action: 'fermer', title: 'Fermer' }
    ]
  };

  navigator.serviceWorker.ready.then(function(registration) {
    registration.showNotification('Nouvelle notification', options);
  });
}

// Gérer les clics sur les notifications (dans le Service Worker)
self.addEventListener('notificationclick', function(event) {
  event.notification.close();

  if (event.action === 'ouvrir') {
    clients.openWindow(event.notification.data.url);
  }
});
```

### Dans TMS Web Core

```pascal
procedure TForm1.DemanderPermissionNotifications;  
begin  
  asm
    if ('Notification' in window) {
      Notification.requestPermission().then(function(permission) {
        if (permission === 'granted') {
          console.log('Permission notifications accordée');
          Form1.WebMemo1.Lines.Add('✅ Notifications activées');
        }
      });
    }
  end;
end;

procedure TForm1.EnvoyerNotification;  
begin  
  asm
    if (Notification.permission === 'granted') {
      navigator.serviceWorker.ready.then(function(registration) {
        registration.showNotification('Ma PWA Delphi', {
          body: 'Ceci est une notification de test',
          icon: '/icons/icon-192x192.png',
          badge: '/icons/badge-72x72.png'
        });
      });
    }
  end;
end;
```

## Détection de l'installation

### Savoir si l'application est installée

```pascal
procedure TForm1.CheckIfInstalled;  
var  
  IsInstalled: Boolean;
begin
  asm
    // Vérifier le mode d'affichage
    IsInstalled = (window.matchMedia('(display-mode: standalone)').matches) ||
                  (window.navigator.standalone) ||
                  document.referrer.includes('android-app://');
  end;

  if IsInstalled then
  begin
    WebLabel1.Caption := '✅ Application installée';
    WebButton2.Visible := False; // Cacher le bouton d'installation
  end
  else
  begin
    WebLabel1.Caption := 'Application web';
  end;
end;
```

## Tests et débogage

### Chrome DevTools

**1. Application Tab**
- Voir le manifeste
- Inspecter le Service Worker
- Vérifier le cache
- Tester les notifications

**2. Lighthouse**
- Auditer votre PWA (Performance, Accessibility, Best Practices, SEO)
- Recommandations d'amélioration détaillées
- 💡 Depuis Chrome DevTools 109 (2023), Lighthouse n'a plus de
  catégorie « PWA » avec score 0–100 séparée — les vérifications PWA
  apparaissent maintenant comme **« Installable »** et **« PWA Optimized »**
  dans le rapport principal, sous forme d'audits booléens (réussi/échec).

**Commande :**
```
F12 → Lighthouse → cocher les catégories voulues → Analyze page load
```

### Simuler le mode hors ligne

**Chrome DevTools :**
1. F12
2. Network tab
3. Cocher "Offline"
4. Recharger la page

### Tester l'installation

**Desktop (Chrome) :**
- Icône + dans la barre d'adresse
- Menu → Installer...

**Mobile :**
- Menu → Ajouter à l'écran d'accueil

## Déploiement d'une PWA

### Checklist de déploiement

✅ **HTTPS activé** (obligatoire)  
✅ **manifest.json** présent et valide  
✅ **Service Worker** enregistré  
✅ **Icônes** de toutes tailles (192x192 et 512x512 minimum)  
✅ **start_url** correct  
✅ **Responsive design** fonctionnel  
✅ **Mode hors ligne** opérationnel

### Configuration serveur

**Pour servir correctement le manifeste :**

**Apache (.htaccess) :**
```apache
# Manifeste
<Files "manifest.json">
  Header set Content-Type "application/manifest+json"
</Files>

# Service Worker
<Files "service-worker.js">
  Header set Service-Worker-Allowed "/"
  Header set Content-Type "text/javascript"
</Files>
```

**Nginx :**
```nginx
location /manifest.json {
    types { } default_type "application/manifest+json";
}

location /service-worker.js {
    types { } default_type "text/javascript";
    add_header Service-Worker-Allowed "/";
}
```

> 💡 Depuis la **RFC 9239** (2022), le type MIME standard pour JavaScript  
> est `text/javascript`. `application/javascript` et  
> `application/x-javascript` sont **dépréciés** mais restent acceptés  
> par tous les navigateurs.

### Hébergement recommandé

**Options gratuites :**
- **Netlify** : Déploiement simple, HTTPS auto, CDN global
- **Vercel** : Optimisé pour applications web modernes
- **GitHub Pages** : Simple mais nécessite configuration HTTPS
- **Firebase Hosting** : Excellent pour PWA

**Options payantes :**
- **AWS CloudFront + S3** : Scalable et performant
- **Google Cloud Storage + Load Balancer**
- **Azure Static Web Apps**

## Bonnes pratiques

### 1. Icônes de qualité

Créer des icônes pour toutes les tailles nécessaires :

```
icon-72x72.png  
icon-96x96.png  
icon-128x128.png  
icon-144x144.png  
icon-152x152.png  
icon-192x192.png  
icon-384x384.png  
icon-512x512.png  
```

**Conseils :**
- Format PNG avec transparence
- Design simple et reconnaissable
- Contraste élevé
- Test sur fond clair et foncé

### 2. Gestion des versions du cache

```javascript
// Incrémenter la version à chaque mise à jour
const CACHE_NAME = 'mon-app-v2'; // v1 → v2

// Supprimer les anciens caches
self.addEventListener('activate', function(event) {
  event.waitUntil(
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          if (cacheName !== CACHE_NAME) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});
```

### 3. Informer l'utilisateur des mises à jour

```javascript
// À placer côté PAGE (pas dans le Service Worker).
// L'événement `controllerchange` est émis sur `navigator.serviceWorker`
// quand un nouveau Service Worker prend le contrôle (après skipWaiting()).
let refreshing = false;  
navigator.serviceWorker.addEventListener('controllerchange', function() {  
  if (refreshing) return;       // éviter une boucle de reload
  refreshing = true;
  if (confirm('Nouvelle version disponible ! Recharger maintenant ?')) {
    window.location.reload();
  }
});

// Pour forcer l'activation immédiate du nouveau SW (sinon il attendrait
// que tous les onglets de l'origine soient fermés), appeler skipWaiting
// dans le handler 'install' du Service Worker :
//
//   self.addEventListener('install', (event) => {
//     self.skipWaiting();
//     // ... event.waitUntil(precache(...))
//   });
```

### 4. Analytics pour PWA

Suivre l'utilisation de votre PWA :

```javascript
// Dans le Service Worker — bonne pratique : différer les appels analytics
// jusqu'au retour en ligne (sinon ils échouent silencieusement).

// 1) Événement d'installation : enregistrer l'install (à envoyer dès
//    que possible, pas pendant le 'install' qui doit rester rapide).
self.addEventListener('activate', function(event) {
  event.waitUntil(
    fetch('/analytics/install').catch(() => {}) // best-effort
  );
});

// 2) Détection des requêtes hors ligne :
//    ⚠️ NE PAS appeler fetch('/analytics/...') depuis l'événement fetch
//    lui-même — ça déclencherait une boucle infinie (chaque appel
//    analytics est lui-même un fetch). Préférer un message côté page
//    via `postMessage` quand le SW détecte une réponse depuis le cache.
self.addEventListener('fetch', function(event) {
  // Exclure les URL analytics du traitement pour éviter la récursion.
  if (event.request.url.includes('/analytics/')) return;

  // Logique de cache habituelle ici…
});
```

### 5. Feedback utilisateur

```pascal
procedure TForm1.AfficherStatutPWA;  
begin  
  if FIsOnline then
    ShowMessage('✅ Connecté - Données synchronisées')
  else
    ShowMessage('⚠️ Hors ligne - Vos modifications seront synchronisées à la prochaine connexion');
end;
```

## Limitations et considérations

### Limitations techniques

❌ **iOS Safari** : Support PWA en progression mais incomplet. Depuis
  **iOS 16.4** (mars 2023), les notifications push fonctionnent UNIQUEMENT
  pour les PWA *installées sur l'écran d'accueil*. ⚠️ Dans l'**Union
  Européenne**, depuis iOS 17.4 (DMA), Apple a temporairement retiré le
  support PWA standalone : les PWA s'ouvrent comme des onglets Safari
  classiques, sans notifications push. Pas de Background Sync, ni de
  Background Fetch, ni de Periodic Sync sur iOS quel que soit le pays.  
❌ **Stockage** : Peut être effacé par le système si espace faible  
❌ **API natives** : Accès limité au matériel (vs app native)  
❌ **App Stores** : Pas de présence naturelle dans les stores (mais possible via wrapper)

### Considérations de sécurité

✅ **HTTPS obligatoire** : Coût et configuration  
✅ **CORS** : À configurer correctement  
✅ **Content Security Policy** : Restrictions à prévoir  
✅ **Données sensibles** : Ne jamais stocker en clair localement

### Quand choisir une PWA ?

**✅ Parfait pour :**
- Applications web qui bénéficieraient du mode hors ligne
- Éviter les frais et contraintes des App Stores
- Déploiement et mises à jour rapides
- Budget limité (un seul code pour toutes plateformes)

**❌ Moins adapté pour :**
- Applications nécessitant accès matériel avancé
- Jeux 3D performants
- Applications nécessitant Background Sync sur iOS (non supporté)
- Applications déployées en UE qui dépendent fortement de l'expérience standalone iOS (cf. DMA)
- Applications nécessitant intégration profonde avec l'OS

## Exemple complet : Application de notes PWA

### Structure de l'application

**Fonctionnalités :**
- Créer, modifier, supprimer des notes
- Fonctionne hors ligne
- Synchronisation automatique
- Installation possible

**Architecture :**
```
NotesApp/
├── index.html
├── manifest.json
├── service-worker.js
├── css/
│   └── style.css
├── js/
│   └── app.js (généré par TMS Web Core)
├── icons/
│   └── ... (toutes les tailles)
└── api/
    └── (Backend Delphi)
```

### Code TMS Web Core (simplifié)

```pascal
unit NotesApp;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.Storage;

type
  TNoteItem = record
    ID: string;
    Title: string;
    Content: string;
    CreatedAt: TDateTime;
    IsSynced: Boolean;
  end;

  TFormNotes = class(TWebForm)
    procedure WebFormCreate(Sender: TObject);
  private
    FNotes: array of TNoteItem;
    procedure LoadNotes;
    procedure SaveNotes;
    procedure SyncWithServer;
  public
    { Public declarations }
  end;

var
  FormNotes: TFormNotes;  // instance globale référencée depuis JS

implementation

procedure TFormNotes.WebFormCreate(Sender: TObject);  
begin  
  RegisterServiceWorker;
  LoadNotes;

  // Synchroniser quand on revient en ligne — le nom de l'instance JS
  // correspond à la variable globale ci-dessus (FormNotes, pas Form1).
  asm
    window.addEventListener('online', function() {
      FormNotes.SyncWithServer();
    });
  end;
end;

procedure TFormNotes.LoadNotes;  
var  
  StoredData: string;
begin
  // Charger depuis localStorage
  asm
    StoredData = localStorage.getItem('notes') || '[]';
  end;

  // Parser le JSON et remplir FNotes
  // ...
end;

procedure TFormNotes.SaveNotes;  
var  
  JSONData: string;
begin
  // Convertir FNotes en JSON
  // JSONData := ...

  // Sauvegarder localement
  asm
    localStorage.setItem('notes', JSONData);
  end;
end;

procedure TFormNotes.SyncWithServer;  
begin  
  // Envoyer les notes non synchronisées au serveur
  // via API REST Delphi
end;

end.
```

## Conclusion

Les Progressive Web Apps représentent **l'avenir des applications web**. Avec Delphi, et particulièrement TMS Web Core, vous pouvez créer des PWA professionnelles qui offrent :

✅ **Expérience native** sans les contraintes des app stores  
✅ **Fonctionnement hors ligne** pour une disponibilité maximale  
✅ **Installation facile** sur tous les appareils  
✅ **Mises à jour automatiques** sans intervention utilisateur  
✅ **Un seul code** pour toutes les plateformes

**Points clés à retenir :**

1. **Manifeste + Service Worker = PWA** : Les deux éléments essentiels
2. **HTTPS obligatoire** : Pas de compromis sur la sécurité
3. **Cache intelligent** : Choisir la bonne stratégie selon le contenu
4. **Expérience progressive** : L'app doit fonctionner même sans toutes les fonctionnalités PWA
5. **Tests approfondis** : Lighthouse et tests manuels sur différents appareils

Les PWA avec Delphi vous permettent de moderniser vos applications existantes ou de créer de nouvelles applications web compétitives, tout en conservant la puissance et la productivité de l'écosystème Delphi.

Dans les sections suivantes, nous explorerons d'autres aspects avancés du développement web avec Delphi, notamment WebAssembly et les techniques de templating serveur avancées.

⏭️ [WebAssembly et Delphi](/23-conception-dapplications-web-avec-delphi/08-webassembly-et-delphi.md)
