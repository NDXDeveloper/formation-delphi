# 23.7 Progressive Web Apps (PWA)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux Progressive Web Apps

Les Progressive Web Apps (PWA) représentent une évolution majeure dans le développement web. Ce sont des applications web qui offrent une expérience utilisateur similaire à celle des applications natives, tout en conservant les avantages des sites web traditionnels. En intégrant les capacités des PWA à vos applications Delphi, vous pouvez créer des expériences utilisateur modernes et engageantes.

![Note] Cette section ne nécessite pas de connaissances approfondies en JavaScript, mais une compréhension de base des concepts web sera utile. Les explications sont conçues pour être accessibles aux débutants.

## Pourquoi adopter les PWA ?

Les PWA offrent plusieurs avantages qui les rendent attractives tant pour les développeurs que pour les utilisateurs :

1. **Installation facile** - Les utilisateurs peuvent "installer" votre application sur leur appareil sans passer par un app store
2. **Fonctionnement hors ligne** - Les PWA peuvent fonctionner sans connexion internet
3. **Chargement rapide** - Grâce à la mise en cache des ressources
4. **Notifications push** - Possibilité d'envoyer des notifications même lorsque l'application n'est pas ouverte
5. **Mise à jour automatique** - Les utilisateurs ont toujours la dernière version
6. **Responsive** - Fonctionnent sur tous les appareils (desktop, mobile, tablette)
7. **Sécurisées** - Nécessitent HTTPS, garantissant une connexion sécurisée

## Composants clés d'une PWA

Pour transformer une application web en PWA, vous avez besoin de trois éléments essentiels :

1. **Service Worker** - Un script JavaScript qui agit comme un proxy réseau, permettant le fonctionnement hors ligne et la mise en cache
2. **Manifest Web** - Un fichier JSON qui décrit votre application (nom, icônes, couleurs, etc.)
3. **HTTPS** - Une connexion sécurisée est obligatoire pour les PWA

## Créer une PWA avec Delphi

Voyons comment transformer une application web Delphi existante en PWA. Nous utiliserons comme exemple le site web dynamique créé dans la section 23.5.

### Étape 1 : Créer le fichier manifest

Le manifest web est un fichier JSON qui fournit des informations sur votre application. Créez un fichier nommé `manifest.json` dans un dossier `www` de votre projet Delphi :

```json
{
  "name": "Mon Application Delphi",
  "short_name": "DelphiApp",
  "description": "Une Progressive Web App créée avec Delphi",
  "start_url": "/",
  "display": "standalone",
  "background_color": "#ffffff",
  "theme_color": "#4a6da7",
  "icons": [
    {
      "src": "/images/icon-192x192.png",
      "sizes": "192x192",
      "type": "image/png"
    },
    {
      "src": "/images/icon-512x512.png",
      "sizes": "512x512",
      "type": "image/png"
    }
  ]
}
```

Assurez-vous de créer les fichiers d'icônes correspondants et de les placer dans le dossier `www/images`.

### Étape 2 : Créer le Service Worker

Le Service Worker est un script JavaScript qui permet le fonctionnement hors ligne et d'autres fonctionnalités des PWA. Créez un fichier nommé `service-worker.js` dans le dossier `www` :

```javascript
// Nom du cache et ressources à mettre en cache immédiatement
const CACHE_NAME = 'delphi-pwa-v1';
const RESOURCES_TO_PRECACHE = [
  '/',
  '/index.html',
  '/styles.css',
  '/scripts/main.js',
  '/manifest.json',
  '/images/logo.png',
  '/images/icon-192x192.png',
  '/images/icon-512x512.png'
];

// Installation du Service Worker
self.addEventListener('install', event => {
  console.log('Service Worker: Installation en cours...');

  // Préchargement des ressources
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => {
        console.log('Service Worker: Mise en cache des fichiers essentiels');
        return cache.addAll(RESOURCES_TO_PRECACHE);
      })
      .then(() => self.skipWaiting()) // Forcer l'activation immédiate
  );
});

// Activation du Service Worker
self.addEventListener('activate', event => {
  console.log('Service Worker: Activé');

  // Nettoyage des anciens caches
  event.waitUntil(
    caches.keys().then(cacheNames => {
      return Promise.all(
        cacheNames.map(cacheName => {
          if (cacheName !== CACHE_NAME) {
            console.log('Service Worker: Suppression de l\'ancien cache', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    })
  );

  return self.clients.claim(); // Prendre le contrôle immédiat
});

// Stratégie de mise en cache : Cache First, puis réseau
self.addEventListener('fetch', event => {
  console.log('Service Worker: Récupération de ressource', event.request.url);

  event.respondWith(
    caches.match(event.request)
      .then(cachedResponse => {
        // Renvoyer la réponse mise en cache si elle existe
        if (cachedResponse) {
          return cachedResponse;
        }

        // Sinon, récupérer depuis le réseau
        return fetch(event.request)
          .then(response => {
            // Ne pas mettre en cache les réponses d'API (facultatif)
            if (!event.request.url.includes('/api/')) {
              // Cloner la réponse car elle ne peut être utilisée qu'une fois
              const responseToCache = response.clone();

              caches.open(CACHE_NAME)
                .then(cache => {
                  cache.put(event.request, responseToCache);
                });
            }

            return response;
          })
          .catch(error => {
            console.error('Erreur de récupération:', error);

            // Si la requête concerne une image, vous pourriez renvoyer une image de remplacement
            if (event.request.url.match(/\.(jpg|jpeg|png|gif|svg)$/)) {
              return caches.match('/images/offline-image.png');
            }

            // Pour les pages HTML, renvoyez une page hors ligne
            if (event.request.headers.get('accept').includes('text/html')) {
              return caches.match('/offline.html');
            }

            // Sinon, laissez l'erreur se propager
            throw error;
          });
      })
  );
});
```

### Étape 3 : Créer une page hors ligne

Pour améliorer l'expérience utilisateur lorsque l'application est hors ligne, créez un fichier `offline.html` dans le dossier `www` :

```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Mon Application Delphi - Hors ligne</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      margin: 0;
      background-color: #f5f5f5;
    }

    .offline-container {
      text-align: center;
      padding: 2rem;
      border-radius: 8px;
      background-color: white;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      max-width: 90%;
      width: 500px;
    }

    h1 {
      color: #4a6da7;
    }

    .icon {
      font-size: 3rem;
      margin-bottom: 1rem;
    }
  </style>
</head>
<body>
  <div class="offline-container">
    <div class="icon">📶</div>
    <h1>Vous êtes hors ligne</h1>
    <p>Impossible de se connecter à Internet. Vérifiez votre connexion et réessayez.</p>
    <p>Certaines fonctionnalités de l'application restent disponibles en mode hors ligne.</p>
    <button onclick="window.location.reload()">Réessayer</button>
  </div>
</body>
</html>
```

### Étape 4 : Modifier votre modèle HTML principal

Ajoutez les liens vers le manifest et enregistrez le Service Worker dans votre modèle HTML principal. Dans votre fichier `WebModuleUnit1.pas`, modifiez la méthode `GeneratePageHTML` :

```delphi
function TWebModule1.GeneratePageHTML(const Title, Content: string): string;
begin
  Result :=
    '<!DOCTYPE html>' + #13#10 +
    '<html lang="fr">' + #13#10 +
    '<head>' + #13#10 +
    '  <meta charset="UTF-8">' + #13#10 +
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' + #13#10 +
    '  <meta name="theme-color" content="#4a6da7">' + #13#10 +
    '  <link rel="manifest" href="/manifest.json">' + #13#10 +
    '  <link rel="apple-touch-icon" href="/images/icon-192x192.png">' + #13#10 +
    '  <title>' + Title + '</title>' + #13#10 +
    '  <style>' + #13#10 +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' + #13#10 +
    '    header { background-color: #4a6da7; color: white; padding: 10px 20px; margin-bottom: 20px; }' + #13#10 +
    '    nav { margin-bottom: 20px; }' + #13#10 +
    '    nav a { margin-right: 15px; color: #4a6da7; text-decoration: none; }' + #13#10 +
    '    nav a:hover { text-decoration: underline; }' + #13#10 +
    '    .content { padding: 20px; background-color: #f5f5f5; border-radius: 5px; }' + #13#10 +
    '    footer { margin-top: 20px; text-align: center; color: #666; font-size: 0.8em; }' + #13#10 +
    '  </style>' + #13#10 +
    '</head>' + #13#10 +
    '<body>' + #13#10 +
    '  <header>' + #13#10 +
    '    <h1>' + Title + '</h1>' + #13#10 +
    '  </header>' + #13#10 +
    '  <nav>' + #13#10 +
    '    <a href="/">Accueil</a>' + #13#10 +
    '    <a href="/produits">Produits</a>' + #13#10 +
    '    <a href="/contact">Contact</a>' + #13#10 +
    '    <a href="/apropos">À propos</a>' + #13#10 +
    '  </nav>' + #13#10 +
    '  <div class="content">' + #13#10 +
    Content + #13#10 +
    '  </div>' + #13#10 +
    '  <footer>' + #13#10 +
    '    <p>&copy; ' + FormatDateTime('yyyy', Now) + ' - Mon Site Web Dynamique avec Delphi</p>' + #13#10 +
    '  </footer>' + #13#10 +
    '  <script>' + #13#10 +
    '    // Enregistrement du Service Worker' + #13#10 +
    '    if ("serviceWorker" in navigator) {' + #13#10 +
    '      window.addEventListener("load", function() {' + #13#10 +
    '        navigator.serviceWorker.register("/service-worker.js")' + #13#10 +
    '          .then(function(registration) {' + #13#10 +
    '            console.log("Service Worker enregistré avec succès:", registration.scope);' + #13#10 +
    '          })' + #13#10 +
    '          .catch(function(error) {' + #13#10 +
    '            console.log("Échec de l''enregistrement du Service Worker:", error);' + #13#10 +
    '          });' + #13#10 +
    '      });' + #13#10 +
    '    }' + #13#10 +
    '  </script>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;
```

### Étape 5 : Servir les fichiers nécessaires

Assurez-vous que votre application Delphi peut servir tous les fichiers nécessaires à la PWA. Ajoutez des actions pour servir le manifest, le Service Worker et la page hors ligne :

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  Action: TWebActionItem;
begin
  // ... actions existantes ...

  // Ajouter une action pour servir le manifest
  Action := Actions.Add;
  Action.Name := 'ActionServeManifest';
  Action.PathInfo := '/manifest.json';
  Action.MethodType := mtGet;
  Action.OnAction := ActionServeManifestAction;

  // Ajouter une action pour servir le Service Worker
  Action := Actions.Add;
  Action.Name := 'ActionServeServiceWorker';
  Action.PathInfo := '/service-worker.js';
  Action.MethodType := mtGet;
  Action.OnAction := ActionServeServiceWorkerAction;

  // Ajouter une action pour servir la page hors ligne
  Action := Actions.Add;
  Action.Name := 'ActionServeOfflinePage';
  Action.PathInfo := '/offline.html';
  Action.MethodType := mtGet;
  Action.OnAction := ActionServeOfflinePageAction;
end;

procedure TWebModule1.ActionServeManifestAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FilePath: string;
  FileContent: TStringList;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\manifest.json';

  if FileExists(FilePath) then
  begin
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FilePath);
      Response.ContentType := 'application/json';
      Response.Content := FileContent.Text;
    finally
      FileContent.Free;
    end;
  end
  else
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain';
    Response.Content := 'Manifest non trouvé';
  end;

  Handled := True;
end;

procedure TWebModule1.ActionServeServiceWorkerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FilePath: string;
  FileContent: TStringList;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\service-worker.js';

  if FileExists(FilePath) then
  begin
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FilePath);
      Response.ContentType := 'application/javascript';
      Response.Content := FileContent.Text;
    finally
      FileContent.Free;
    end;
  end
  else
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain';
    Response.Content := 'Service Worker non trouvé';
  end;

  Handled := True;
end;

procedure TWebModule1.ActionServeOfflinePageAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FilePath: string;
  FileContent: TStringList;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\offline.html';

  if FileExists(FilePath) then
  begin
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FilePath);
      Response.ContentType := 'text/html';
      Response.Content := FileContent.Text;
    finally
      FileContent.Free;
    end;
  end
  else
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain';
    Response.Content := 'Page hors ligne non trouvée';
  end;

  Handled := True;
end;
```

### Étape 6 : Configurer HTTPS

Les PWA nécessitent une connexion HTTPS. Pour les environnements de développement, vous pouvez utiliser des certificats auto-signés, mais pour la production, vous aurez besoin d'un certificat SSL valide.

Pour le développement local, voici comment configurer un certificat auto-signé avec Delphi :

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  // ... code existant ...

  // Configurer HTTPS pour le développement
  if Server is TIdHTTPWebBrokerBridge then
  begin
    with TIdHTTPWebBrokerBridge(Server) do
    begin
      // Créer le contexte SSL
      IOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Server);
      with IOHandler as TIdServerIOHandlerSSLOpenSSL do
      begin
        SSLOptions.CertFile := ExtractFilePath(ParamStr(0)) + 'cert\server.crt';
        SSLOptions.KeyFile := ExtractFilePath(ParamStr(0)) + 'cert\server.key';
        SSLOptions.RootCertFile := ExtractFilePath(ParamStr(0)) + 'cert\root.pem';
        SSLOptions.Method := sslvTLSv1_2;
        SSLOptions.SSLVersions := [sslvTLSv1_2];
      end;
    end;
  end;
end;
```

Pour générer les certificats auto-signés, vous pouvez utiliser OpenSSL :

```bash
openssl genrsa -out server.key 2048
openssl req -new -key server.key -out server.csr
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
```

## Fonctionnalités avancées des PWA

### 1. Notifications Push

Les notifications push permettent d'informer les utilisateurs même lorsque l'application n'est pas ouverte. Voici comment les mettre en place :

1. **Demander la permission** - Ajoutez ce code JavaScript dans votre page principale :

```javascript
// Demander la permission d'envoyer des notifications push
function requestNotificationPermission() {
  if ('Notification' in window) {
    Notification.requestPermission().then(permission => {
      if (permission === 'granted') {
        console.log('Permission des notifications accordée');
        // Vous pouvez maintenant enregistrer l'abonnement push
        subscribeToPushNotifications();
      } else {
        console.log('Permission des notifications refusée');
      }
    });
  }
}

// Ajouter un bouton dans l'interface pour demander la permission
const notificationButton = document.createElement('button');
notificationButton.textContent = 'Activer les notifications';
notificationButton.onclick = requestNotificationPermission;
document.querySelector('header').appendChild(notificationButton);
```

2. **Gérer les notifications dans le Service Worker** - Ajoutez ce code à votre `service-worker.js` :

```javascript
// Écouter les événements push
self.addEventListener('push', event => {
  console.log('Notification push reçue', event);

  // Récupérer les données de la notification
  const data = event.data.json();

  // Afficher la notification
  const options = {
    body: data.body,
    icon: '/images/icon-192x192.png',
    badge: '/images/badge-72x72.png',
    data: {
      url: data.url || '/'
    }
  };

  event.waitUntil(
    self.registration.showNotification(data.title, options)
  );
});

// Gérer le clic sur une notification
self.addEventListener('notificationclick', event => {
  console.log('Notification cliquée', event);

  event.notification.close();

  // Ouvrir l'URL spécifiée dans les données de la notification
  event.waitUntil(
    clients.openWindow(event.notification.data.url)
  );
});
```

3. **Envoyer des notifications depuis Delphi** - Créez un endpoint pour envoyer des notifications :

```delphi
procedure TWebModule1.ActionEnvoyerNotificationAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Titre, Corps, URL, JSONPayload: string;
  HTTPClient: THTTPClient;
  RequestContent, ResponseContent: TStringStream;
begin
  // Récupérer les paramètres de la requête
  Titre := Request.ContentFields.Values['titre'];
  Corps := Request.ContentFields.Values['corps'];
  URL := Request.ContentFields.Values['url'];

  if (Titre = '') or (Corps = '') then
  begin
    Response.StatusCode := 400;
    Response.Content := '{"error": "Titre et corps sont requis"}';
    Response.ContentType := 'application/json';
    Handled := True;
    Exit;
  end;

  // Créer la charge utile JSON pour Web Push
  JSONPayload := Format('{"title":"%s","body":"%s","url":"%s"}',
                        [Titre, Corps, URL]);

  // Envoyer la notification via un service Web Push (exemple avec FCM)
  HTTPClient := THTTPClient.Create;
  RequestContent := TStringStream.Create(
    Format('{"message":{"topic":"all","notification":{"title":"%s","body":"%s"},"webpush":{"fcm_options":{"link":"%s"}}}}',
           [Titre, Corps, URL]),
    TEncoding.UTF8
  );
  ResponseContent := TStringStream.Create('', TEncoding.UTF8);

  try
    HTTPClient.AddHeader('Authorization', 'key=VOTRE_CLE_FCM');
    HTTPClient.AddHeader('Content-Type', 'application/json');

    if HTTPClient.Post('https://fcm.googleapis.com/fcm/send', RequestContent, ResponseContent) then
    begin
      Response.StatusCode := 200;
      Response.Content := '{"success": true, "message": "Notification envoyée"}';
    end
    else
    begin
      Response.StatusCode := 500;
      Response.Content := '{"error": "Échec de l''envoi de la notification"}';
    end;
  finally
    HTTPClient.Free;
    RequestContent.Free;
    ResponseContent.Free;
  end;

  Response.ContentType := 'application/json';
  Handled := True;
end;
```

### 2. Synchronisation en arrière-plan

La synchronisation en arrière-plan permet à votre application de synchroniser des données même lorsqu'elle n'est pas active :

```javascript
// Dans votre script principal
function enregistrerSynchronisation() {
  if ('serviceWorker' in navigator && 'SyncManager' in window) {
    navigator.serviceWorker.ready
      .then(registration => {
        // Enregistrer une tâche de synchronisation
        return registration.sync.register('sync-donnees');
      })
      .then(() => {
        console.log('Synchronisation en arrière-plan enregistrée');
      })
      .catch(error => {
        console.error('Erreur lors de l\'enregistrement de la synchronisation:', error);
      });
  }
}

// Dans service-worker.js
self.addEventListener('sync', event => {
  if (event.tag === 'sync-donnees') {
    event.waitUntil(
      // Votre logique de synchronisation ici
      syncData()
    );
  }
});

async function syncData() {
  // Récupérer les données en attente de IndexedDB
  const db = await openDatabase();
  const pendingData = await getDataFromIndexedDB(db);

  // Envoyer les données au serveur
  for (const item of pendingData) {
    try {
      await fetch('/api/sync', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(item)
      });

      // Supprimer l'élément synchronisé
      await removeDataFromIndexedDB(db, item.id);
    } catch (error) {
      console.error('Erreur de synchronisation:', error);
      // Laisser l'élément pour une tentative future
    }
  }
}
```

## Tester votre PWA

Pour tester si votre application répond aux critères d'une PWA, vous pouvez utiliser l'outil Lighthouse intégré à Google Chrome :

1. Ouvrez Chrome DevTools (F12)
2. Allez dans l'onglet "Lighthouse"
3. Cochez la catégorie "Progressive Web App"
4. Cliquez sur "Generate report"

Lighthouse vous donnera un score et des recommandations pour améliorer votre PWA.

## Déploiement de votre PWA

Pour déployer votre PWA Delphi en production, vous devrez :

1. **Acquérir un certificat SSL valide** - Utilisez Let's Encrypt ou un fournisseur commercial
2. **Configurer votre serveur Delphi avec HTTPS**
3. **Optimiser les ressources** - Minifier le JavaScript et le CSS
4. **Mettre en place un CDN** (facultatif) pour servir plus rapidement les ressources statiques

## Cas d'usage pratiques

### 1. Application de gestion d'inventaire

Une PWA de gestion d'inventaire peut être particulièrement utile pour :
- Permettre aux employés de scanner des produits même sans connexion
- Synchroniser les données une fois la connexion rétablie
- Recevoir des notifications pour les stocks faibles

### 2. Outil de suivi de projet

Une PWA de suivi de projet peut offrir :
- Un accès aux tâches et échéances même hors ligne
- Des notifications pour les échéances imminentes
- Une interface réactive fonctionnant sur tous les appareils

## Avantages et limites des PWA avec Delphi

### Avantages

- Utilise vos connaissances Delphi existantes
- Déploiement simplifié (pas d'app store)
- Une seule base de code pour plusieurs plateformes
- Maintenance plus facile

### Limites

- Accès limité à certaines fonctionnalités natives des appareils
- Performances potentiellement inférieures aux applications natives
- Support variable selon les navigateurs (particulièrement Safari sur iOS)

## Conclusion

Les Progressive Web Apps offrent un excellent compromis entre les applications web traditionnelles et les applications natives. En combinant la puissance et la robustesse de Delphi côté serveur avec les technologies web modernes côté client, vous pouvez créer des applications qui fonctionnent sur tous les appareils, même hors ligne.

Cette approche est particulièrement intéressante pour les développeurs Delphi qui souhaitent élargir leur portée sans avoir à apprendre totalement de nouvelles technologies. En suivant les étapes de ce tutoriel, vous pouvez transformer vos applications web Delphi existantes en PWA modernes et engageantes.

N'oubliez pas que le monde des PWA évolue rapidement, alors restez à jour sur les dernières fonctionnalités et meilleures pratiques pour tirer le meilleur parti de cette technologie.

⏭️ [WebAssembly et Delphi](23-conception-dapplications-web-avec-delphi/08-webassembly-et-delphi.md)
