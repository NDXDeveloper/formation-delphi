# 19.6 Applications PWA (Progressive Web Apps) avec Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux PWA

Les Progressive Web Apps (PWA) représentent une approche moderne du développement web qui permet de créer des applications offrant une expérience utilisateur proche de celle des applications natives, tout en conservant les avantages des sites web. Avec Delphi, vous pouvez développer des PWA performantes qui fonctionnent sur différentes plateformes.

## Pourquoi créer une PWA ?

Les PWA offrent plusieurs avantages par rapport aux applications web traditionnelles :

- **Installation sur l'écran d'accueil** sans passer par un app store
- **Fonctionnement hors ligne** grâce au cache local
- **Chargement rapide** même sur connexions réseau faibles
- **Notifications push** pour engager les utilisateurs
- **Mise à jour automatique** sans intervention de l'utilisateur
- **Adaptation responsive** à différentes tailles d'écran

## Outils pour créer des PWA avec Delphi

Delphi propose deux principales approches pour développer des PWA :

1. **TMS WEB Core** - Une extension puissante pour Delphi permettant de créer des applications web avec Object Pascal
2. **Intraweb** - Un framework intégré pour le développement d'applications web

## Création d'une PWA avec TMS WEB Core

### Prérequis

- Delphi 11 Alexandria ou Delphi 12 Athens
- TMS WEB Core installé (disponible via GetIt Package Manager)
- Connaissance de base du HTML, CSS et JavaScript

### Étapes de création d'une PWA basique

#### 1. Création du projet

1. Lancez Delphi et sélectionnez **Fichier → Nouveau → TMS Web Project**
2. Choisissez le modèle **PWA Application**
3. Donnez un nom à votre projet et cliquez sur **OK**

Delphi génère automatiquement les fichiers nécessaires, notamment :
- Le fichier principal de l'application
- Un manifeste web (manifest.json)
- Un service worker de base
- Des icônes par défaut

#### 2. Configuration du manifeste web

Le manifeste web est un fichier JSON qui décrit votre application. Modifiez le fichier `manifest.json` généré :

```json
{
  "name": "Mon Application PWA Delphi",
  "short_name": "PWADELPHI",
  "start_url": "./index.html",
  "display": "standalone",
  "background_color": "#FFFFFF",
  "theme_color": "#4D90FE",
  "icons": [
    {
      "src": "icons/icon-72x72.png",
      "sizes": "72x72",
      "type": "image/png"
    },
    {
      "src": "icons/icon-192x192.png",
      "sizes": "192x192",
      "type": "image/png"
    },
    {
      "src": "icons/icon-512x512.png",
      "sizes": "512x512",
      "type": "image/png"
    }
  ]
}
```

#### 3. Comprendre le Service Worker

Le service worker est un script JavaScript qui s'exécute en arrière-plan et permet notamment la mise en cache et le fonctionnement hors ligne. Voici un exemple simplifié :

```javascript
// service-worker.js
const CACHE_NAME = 'mon-pwa-delphi-v1';
const URLS_TO_CACHE = [
  './',
  './index.html',
  './css/style.css',
  './js/app.js',
  './images/logo.png'
];

// Installation du service worker et mise en cache des ressources
self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => {
        return cache.addAll(URLS_TO_CACHE);
      })
  );
});

// Gestion des requêtes réseau
self.addEventListener('fetch', (event) => {
  event.respondWith(
    caches.match(event.request)
      .then((response) => {
        // Retourne la ressource en cache si disponible, sinon fetch
        return response || fetch(event.request);
      })
  );
});
```

#### 4. Enregistrement du Service Worker

Dans votre code Pascal, vous devez enregistrer le service worker. Placez ce code dans l'événement `OnCreate` ou `OnReady` de votre formulaire :

```pascal
procedure TForm1.WebFormCreate(Sender: TObject);
begin
  // Enregistrement du service worker
  asm
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker
        .register('./service-worker.js')
        .then(function(registration) {
          console.log('Service Worker enregistré avec succès:', registration.scope);
        })
        .catch(function(error) {
          console.log('Échec de l\'enregistrement du Service Worker:', error);
        });
    }
  end;
end;
```

### Fonctionnalités avancées pour votre PWA

#### Fonctionnement hors ligne

Pour rendre votre application fonctionnelle hors ligne, implémentez une logique de détection de la connexion :

```pascal
procedure TForm1.CheckConnectivity;
begin
  asm
    window.addEventListener('online', function() {
      pas.Unit1.Form1.HandleOnline();
    });

    window.addEventListener('offline', function() {
      pas.Unit1.Form1.HandleOffline();
    });
  end;
end;

procedure TForm1.HandleOnline;
begin
  // Code à exécuter quand la connexion est rétablie
  lblStatus.Caption := 'En ligne';
  lblStatus.Font.Color := clGreen;
  // Synchroniser les données locales avec le serveur...
end;

procedure TForm1.HandleOffline;
begin
  // Code à exécuter quand la connexion est perdue
  lblStatus.Caption := 'Hors ligne';
  lblStatus.Font.Color := clRed;
  // Activer le mode hors ligne...
end;
```

#### Stockage local des données

Utilisez le stockage local pour sauvegarder les données lorsque l'utilisateur est hors ligne :

```pascal
// Fonction pour sauvegarder les données localement
procedure TForm1.SaveToLocalStorage(const Key, Value: string);
begin
  asm
    localStorage.setItem(Key, Value);
  end;
end;

// Fonction pour récupérer les données du stockage local
function TForm1.GetFromLocalStorage(const Key: string): string;
var
  Result: string;
begin
  asm
    Result = localStorage.getItem(Key) || '';
  end;
end;

// Exemple d'utilisation
procedure TForm1.btnSaveClick(Sender: TObject);
begin
  SaveToLocalStorage('user_data', edtUserData.Text);
  ShowMessage('Données sauvegardées localement');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edtUserData.Text := GetFromLocalStorage('user_data');
end;
```

#### Notifications Push

Pour implémenter les notifications push, vous devez d'abord demander la permission à l'utilisateur :

```pascal
procedure TForm1.RequestNotificationPermission;
begin
  asm
    if ('Notification' in window) {
      Notification.requestPermission()
        .then(function(permission) {
          if (permission === 'granted') {
            console.log('Permission accordée pour les notifications');
            // Enregistrer pour des notifications push...
          }
        });
    }
  end;
end;

procedure TForm1.ShowNotification(const Title, Body: string);
begin
  asm
    if ('Notification' in window && Notification.permission === 'granted') {
      new Notification(Title, {
        body: Body,
        icon: './icons/icon-72x72.png'
      });
    }
  end;
end;
```

## Test et déploiement

### Test local

1. Compilez votre application en utilisant **Build → Compiler le projet**
2. Lancez le serveur de développement intégré via **Run → Run without debugging**
3. Ouvrez un navigateur moderne (Chrome, Edge, Firefox) et accédez à l'URL locale (généralement http://localhost:8000)

### Audit de votre PWA

Pour vérifier que votre application est bien une PWA conforme, utilisez l'outil Lighthouse dans Chrome :

1. Ouvrez les DevTools (F12)
2. Accédez à l'onglet "Lighthouse"
3. Cochez l'option "Progressive Web App"
4. Cliquez sur "Generate report"

Suivez les recommandations pour améliorer votre score.

### Déploiement

Pour déployer votre PWA, vous devez l'héberger sur un serveur web avec HTTPS :

1. Compilez votre projet en mode Release
2. Copiez tous les fichiers du dossier `\public_html\` de votre projet sur votre serveur web
3. Assurez-vous que votre serveur est configuré pour servir les bons types MIME, notamment pour les fichiers `.json` et `.js`
4. Vérifiez que le service worker est correctement enregistré en production

## Exemple concret : Application de prise de notes

Voici un exemple simple de création d'une application de prise de notes PWA avec TMS WEB Core :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TWebForm)
    edtNoteTitle: TWebEdit;
    memoNoteContent: TWebMemo;
    btnSaveNote: TWebButton;
    btnLoadNotes: TWebButton;
    lstNotes: TWebListBox;
    procedure WebFormCreate(Sender: TObject);
    procedure btnSaveNoteClick(Sender: TObject);
    procedure btnLoadNotesClick(Sender: TObject);
    procedure lstNotesClick(Sender: TObject);
  private
    procedure RegisterServiceWorker;
    procedure SaveNote(const Title, Content: string);
    procedure LoadNotes;
    procedure LoadNoteContent(const Title: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  RegisterServiceWorker;
  LoadNotes;
end;

procedure TForm1.RegisterServiceWorker;
begin
  asm
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker
        .register('./service-worker.js')
        .then(function(registration) {
          console.log('Service Worker enregistré avec succès:', registration.scope);
        })
        .catch(function(error) {
          console.log('Échec de l\'enregistrement du Service Worker:', error);
        });
    }
  end;
end;

procedure TForm1.SaveNote(const Title, Content: string);
begin
  if (Title = '') then
  begin
    ShowMessage('Veuillez saisir un titre pour la note');
    Exit;
  end;

  asm
    // Sauvegarde de la note individuelle
    localStorage.setItem('note_' + Title, Content);

    // Récupération et mise à jour de la liste des notes
    let notesList = JSON.parse(localStorage.getItem('notes_list') || '[]');
    if (!notesList.includes(Title)) {
      notesList.push(Title);
      localStorage.setItem('notes_list', JSON.stringify(notesList));
    }
  end;

  LoadNotes;
  ShowMessage('Note sauvegardée avec succès');
end;

procedure TForm1.LoadNotes;
var
  NotesArray: TJSArray;
  I: Integer;
begin
  lstNotes.Clear;

  asm
    @NotesArray = JSON.parse(localStorage.getItem('notes_list') || '[]');
  end;

  for I := 0 to NotesArray.Length - 1 do
  begin
    lstNotes.Items.Add(String(NotesArray[I]));
  end;
end;

procedure TForm1.LoadNoteContent(const Title: string);
var
  Content: string;
begin
  asm
    @Content = localStorage.getItem('note_' + Title) || '';
  end;

  edtNoteTitle.Text := Title;
  memoNoteContent.Lines.Text := Content;
end;

procedure TForm1.btnSaveNoteClick(Sender: TObject);
begin
  SaveNote(edtNoteTitle.Text, memoNoteContent.Lines.Text);
end;

procedure TForm1.btnLoadNotesClick(Sender: TObject);
begin
  LoadNotes;
end;

procedure TForm1.lstNotesClick(Sender: TObject);
begin
  if lstNotes.ItemIndex >= 0 then
  begin
    LoadNoteContent(lstNotes.Items[lstNotes.ItemIndex]);
  end;
end;

end.
```

## Bonnes pratiques pour les PWA avec Delphi

1. **Design responsive** : Utilisez les contrôles responsives de TMS WEB Core pour s'adapter à toutes les tailles d'écran
2. **Performances** : Minimisez le JavaScript généré en optimisant votre code Pascal
3. **Expérience hors ligne** : Planifiez votre application pour fonctionner sans connexion internet
4. **Mise en cache intelligente** : Ne mettez pas en cache les données qui changent fréquemment
5. **Interface utilisateur native** : Utilisez des contrôles qui respectent les conventions de la plateforme
6. **Tests croisés** : Testez sur différents navigateurs et appareils

## Limitations actuelles

- Les PWA ont encore quelques limitations par rapport aux applications natives, notamment pour l'accès à certaines fonctionnalités matérielles
- iOS a une prise en charge plus limitée des PWA que Android
- Les notifications push ne sont pas entièrement supportées sur tous les navigateurs

## Conclusion

Les Progressive Web Apps offrent un excellent compromis entre applications web et applications natives. Avec Delphi et TMS WEB Core, vous pouvez créer des PWA performantes en utilisant vos connaissances en Object Pascal. Cette approche est particulièrement adaptée pour :

- Moderniser des applications Delphi existantes en leur donnant une présence web
- Créer des applications disponibles sur toutes les plateformes sans gérer plusieurs bases de code
- Proposer une expérience utilisateur rapide et fluide, même sur des connexions lentes

En suivant les bonnes pratiques PWA et en exploitant les outils Delphi, vous pouvez développer des applications web modernes qui offrent une expérience proche des applications natives, tout en bénéficiant des avantages du web.

## Ressources supplémentaires

- Documentation TMS WEB Core : [https://www.tmssoftware.com/site/tmswebcore.asp](https://www.tmssoftware.com/site/tmswebcore.asp)
- Guide Google sur les PWA : [https://web.dev/progressive-web-apps/](https://web.dev/progressive-web-apps/)
- Forum Delphi sur les PWA : [https://forums.embarcadero.com/](https://forums.embarcadero.com/)

---

_Note: Certains exemples avancés nécessitent Delphi 12 Athens ou supérieur._

⏭️ [Projets d'intelligence artificielle et machine learning](/19-projets-avances/07-projets-dintelligence-artificielle-et-machine-learning.md)
