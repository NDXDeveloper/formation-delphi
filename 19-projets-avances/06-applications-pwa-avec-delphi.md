🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.6 Applications PWA (Progressive Web Apps) avec Delphi

## Introduction

Bienvenue dans l'univers fascinant des Progressive Web Apps (PWA) ! Dans ce chapitre, vous allez découvrir comment créer des applications web modernes qui se comportent comme des applications natives, le tout en utilisant Delphi.

### Qu'est-ce qu'une PWA ?

Une **PWA** (Progressive Web App) est une application web qui offre une expérience similaire à une application mobile native, mais qui fonctionne dans un navigateur web.

**Analogie simple** : Imaginez que vous ouvrez un site web dans votre navigateur, et qu'il vous propose de l'installer sur votre téléphone ou ordinateur comme une vraie application. Une fois installée, elle fonctionne même sans connexion internet. C'est ça, une PWA !

#### Exemples de PWA célèbres

Vous utilisez peut-être déjà des PWA sans le savoir :
- 📱 **Twitter Lite** : Version légère de Twitter
- 📰 **Flipboard** : Lecteur d'actualités
- 🛒 **Alibaba** : Commerce en ligne
- 🎵 **Spotify Web Player** : Lecteur de musique
- 📧 **Gmail** (mode offline)

### Caractéristiques d'une PWA

**Une PWA doit être** :

✅ **Progressive** : Fonctionne pour tous les utilisateurs, quel que soit le navigateur
✅ **Responsive** : S'adapte à toutes les tailles d'écran (mobile, tablette, desktop)
✅ **Offline** : Fonctionne sans connexion internet
✅ **App-like** : Look & feel d'une application native
✅ **Fresh** : Toujours à jour grâce aux service workers
✅ **Safe** : Fonctionne uniquement en HTTPS
✅ **Discoverable** : Identifiable comme "application" par les moteurs de recherche
✅ **Installable** : Peut être ajoutée à l'écran d'accueil
✅ **Linkable** : Partageable via une simple URL

### Pourquoi créer une PWA ?

**Avantages pour les développeurs** :
✅ **Un seul code** : Fonctionne sur mobile, tablette, desktop
✅ **Pas de store** : Pas besoin de passer par l'App Store ou Play Store
✅ **Mises à jour instantanées** : Pas d'approbation nécessaire
✅ **Plus facile à maintenir** : Qu'une application native
✅ **SEO friendly** : Indexable par Google

**Avantages pour les utilisateurs** :
✅ **Installation rapide** : Pas de téléchargement lourd
✅ **Moins d'espace** : Plus léger qu'une app native
✅ **Fonctionne offline** : Accessible sans internet
✅ **Toujours à jour** : Pas besoin de mettre à jour manuellement
✅ **Accessible partout** : Via une simple URL

### Objectifs de ce chapitre

À la fin de ce tutoriel, vous serez capable de :

✅ Comprendre l'architecture des PWA
✅ Créer une PWA avec TMS Web Core et Delphi
✅ Implémenter le mode offline
✅ Créer un manifest d'application
✅ Gérer les service workers
✅ Permettre l'installation de l'app
✅ Ajouter des notifications push
✅ Déployer votre PWA
✅ Optimiser les performances

### Prérequis

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Notions de HTML/CSS (basique)
- ✅ Compréhension du web

**Outils nécessaires** :
- ✅ Delphi 13 Florence
- ✅ TMS Web Core (inclus dans Delphi)
- ✅ Navigateur moderne (Chrome, Edge, Firefox)
- ✅ Serveur web (IIS, Apache, ou serveur de dev)
- ✅ Certificat SSL pour HTTPS

### Durée estimée

**12 à 18 heures** de travail, réparties ainsi :
- Compréhension des concepts PWA : 2-3 heures
- Configuration et premier projet : 2-3 heures
- Développement de l'application : 4-6 heures
- Implémentation offline et service workers : 2-3 heures
- Tests et déploiement : 2-3 heures

---

## Partie 1 : Comprendre les PWA

### 1.1 Architecture d'une PWA

Une PWA repose sur trois piliers techniques :

```
┌─────────────────────────────────────────┐
│         Application Web (HTML/CSS/JS)   │
│         Créée avec TMS Web Core         │
└──────────────┬──────────────────────────┘
               │
               ↓
┌─────────────────────────────────────────┐
│         Web App Manifest                │
│         (manifest.json)                 │
│   - Nom, icônes, couleurs               │
│   - Mode d'affichage                    │
└──────────────┬──────────────────────────┘
               │
               ↓
┌─────────────────────────────────────────┐
│         Service Worker                  │
│         (sw.js)                         │
│   - Cache des ressources                │
│   - Fonctionnement offline              │
│   - Notifications push                  │
└─────────────────────────────────────────┘
```

#### Les trois piliers

**1. L'application web** : Votre interface créée avec TMS Web Core

**2. Le Manifest** : Fichier JSON qui décrit votre application
```json
{
  "name": "Mon Application",
  "short_name": "MonApp",
  "start_url": "/",
  "display": "standalone",
  "icons": [...]
}
```

**3. Le Service Worker** : Script JavaScript qui fonctionne en arrière-plan
- Cache les ressources
- Permet le mode offline
- Gère les notifications

### 1.2 Cycle de vie d'une PWA

```
[Première visite] → [Téléchargement] → [Installation SW] → [Utilisation]
                                              ↓
                               [Cache des ressources]
                                              ↓
[Visite suivante] → [Chargement depuis cache] → [Mise à jour en arrière-plan]
                                              ↓
                               [Fonctionne offline]
```

**Étapes** :

1. **Première visite** : L'utilisateur accède à votre URL
2. **Chargement** : Le navigateur charge les fichiers
3. **Installation** : Le service worker s'installe
4. **Cache** : Les ressources sont mises en cache
5. **Prompt d'installation** : Le navigateur propose d'installer l'app
6. **Utilisation offline** : L'app fonctionne sans internet

### 1.3 PWA vs Application native vs Site web

| Caractéristique | Site Web | PWA | App Native |
|-----------------|----------|-----|------------|
| Installation | Non | Oui (légère) | Oui (lourde) |
| Icône écran accueil | Non | Oui | Oui |
| Fonctionne offline | Non | Oui | Oui |
| Notifications push | Limitées | Oui | Oui |
| Accès hardware | Limité | Moyen | Total |
| Store requis | Non | Non | Oui |
| Mises à jour | Automatiques | Automatiques | Manuelles |
| Taille | N/A | ~1-5 MB | 50-200 MB |
| Développement | Un code | Un code | Code par plateforme |

---

## Partie 2 : Configuration de l'environnement

### 2.1 Installation de TMS Web Core

**TMS Web Core** est inclus dans Delphi et permet de créer des applications web en Pascal.

**Vérification de l'installation** :

1. Ouvrez Delphi 13 Florence
2. Menu **Tools → GetIt Package Manager**
3. Recherchez "TMS Web Core"
4. Installez si nécessaire

**Première utilisation** :

1. **File → New → Other...**
2. **TMS Web → TMS Web Application**
3. Sélectionnez **Progressive Web Application**
4. Nommez votre projet : `ShoppingList` (notre projet exemple)

### 2.2 Structure d'un projet PWA

Delphi génère automatiquement :

```
ShoppingList/
├── Source/
│   └── ShoppingList.dpr          (Projet principal)
├── Forms/
│   ├── UMainForm.pas              (Formulaire principal)
│   └── UMainForm.dfm
├── Web/
│   ├── index.html                 (Page HTML)
│   ├── manifest.json              (Manifest PWA)
│   ├── sw.js                      (Service Worker)
│   └── offline.html               (Page offline)
├── Images/
│   └── icons/                     (Icônes PWA)
└── Output/
    └── Debug/                     (Fichiers compilés)
```

### 2.3 Comprendre TMS Web Core

**TMS Web Core** compile du Pascal en JavaScript moderne :

**Code Pascal** :
```pascal
procedure TForm1.ButtonClick(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;
```

**Devient JavaScript** :
```javascript
function ButtonClick() {
  alert('Hello World!');
}
```

**Avantages** :
- ✅ Écrire en Pascal familier
- ✅ Compilation vers JS optimisé
- ✅ Support complet PWA
- ✅ Composants visuels

---

## Partie 3 : Création de l'application

### 3.1 Notre projet : Liste de courses PWA

Nous allons créer une **liste de courses** qui :
- Permet d'ajouter/supprimer des articles
- Fonctionne offline
- Se synchronise quand la connexion revient
- S'installe comme une app native
- Envoie des notifications

### 3.2 Interface utilisateur

**Formulaire principal (UMainForm.pas)** :

```pascal
unit UMainForm;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.ExtCtrls, WEBLib.Lists;

type
  TShoppingItem = class
  public
    ID: string;
    Name: string;
    Quantity: Integer;
    Checked: Boolean;
    CreatedAt: TDateTime;
  end;

  TMainForm = class(TWebForm)
    PanelHeader: TWebPanel;
    LabelTitle: TWebLabel;
    PanelInput: TWebPanel;
    EditItem: TWebEdit;
    EditQuantity: TWebEdit;
    ButtonAdd: TWebButton;
    ListBoxItems: TWebListBox;
    PanelFooter: TWebPanel;
    LabelStatus: TWebLabel;
    ButtonInstall: TWebButton;

    procedure WebFormCreate(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ListBoxItemsClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure WebFormShow(Sender: TObject);
  private
    FItems: TObjectList<TShoppingItem>;
    FDeferredPrompt: JSValue;

    procedure LoadItems;
    procedure SaveItems;
    procedure AddItem(const AName: string; AQuantity: Integer);
    procedure DeleteItem(const AID: string);
    procedure ToggleItem(const AID: string);
    procedure UpdateList;
    procedure CheckOnlineStatus;
    procedure RegisterServiceWorker;
    procedure SetupInstallPrompt;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  WEBLib.Storage, System.Generics.Collections;

{$R *.dfm}

procedure TMainForm.WebFormCreate(Sender: TObject);
begin
  FItems := TObjectList<TShoppingItem>.Create(True);

  // Charger les données
  LoadItems;
  UpdateList;

  // Configurer PWA
  RegisterServiceWorker;
  SetupInstallPrompt;

  // Vérifier le statut en ligne
  CheckOnlineStatus;
end;

procedure TMainForm.WebFormShow(Sender: TObject);
begin
  EditItem.SetFocus;
end;

procedure TMainForm.LoadItems;
var
  Storage: TLocalStorage;
  JSONData: string;
  JSONArray: TJSONArray;
  I: Integer;
  Item: TShoppingItem;
  JSONItem: TJSONObject;
begin
  Storage := TLocalStorage.Create;
  try
    JSONData := Storage.GetValue('shoppingList');

    if JSONData <> '' then
    begin
      JSONArray := TJSONObject.ParseJSONValue(JSONData) as TJSONArray;
      try
        for I := 0 to JSONArray.Count - 1 do
        begin
          JSONItem := JSONArray.Items[I] as TJSONObject;

          Item := TShoppingItem.Create;
          Item.ID := JSONItem.GetValue<string>('id');
          Item.Name := JSONItem.GetValue<string>('name');
          Item.Quantity := JSONItem.GetValue<Integer>('quantity');
          Item.Checked := JSONItem.GetValue<Boolean>('checked');
          Item.CreatedAt := StrToDateTime(JSONItem.GetValue<string>('createdAt'));

          FItems.Add(Item);
        end;
      finally
        JSONArray.Free;
      end;
    end;
  finally
    Storage.Free;
  end;
end;

procedure TMainForm.SaveItems;
var
  Storage: TLocalStorage;
  JSONArray: TJSONArray;
  Item: TShoppingItem;
  JSONItem: TJSONObject;
begin
  Storage := TLocalStorage.Create;
  try
    JSONArray := TJSONArray.Create;
    try
      for Item in FItems do
      begin
        JSONItem := TJSONObject.Create;
        JSONItem.AddPair('id', Item.ID);
        JSONItem.AddPair('name', Item.Name);
        JSONItem.AddPair('quantity', TJSONNumber.Create(Item.Quantity));
        JSONItem.AddPair('checked', TJSONBool.Create(Item.Checked));
        JSONItem.AddPair('createdAt', DateTimeToStr(Item.CreatedAt));

        JSONArray.AddElement(JSONItem);
      end;

      Storage.SetValue('shoppingList', JSONArray.ToString);
    finally
      JSONArray.Free;
    end;
  finally
    Storage.Free;
  end;
end;

procedure TMainForm.AddItem(const AName: string; AQuantity: Integer);
var
  Item: TShoppingItem;
begin
  if AName.Trim.IsEmpty then
    Exit;

  Item := TShoppingItem.Create;
  Item.ID := TGUID.NewGuid.ToString;
  Item.Name := AName.Trim;
  Item.Quantity := AQuantity;
  Item.Checked := False;
  Item.CreatedAt := Now;

  FItems.Add(Item);

  SaveItems;
  UpdateList;

  // Vider les champs
  EditItem.Text := '';
  EditQuantity.Text := '1';
  EditItem.SetFocus;
end;

procedure TMainForm.ButtonAddClick(Sender: TObject);
var
  Quantity: Integer;
begin
  if not TryStrToInt(EditQuantity.Text, Quantity) then
    Quantity := 1;

  AddItem(EditItem.Text, Quantity);
end;

procedure TMainForm.UpdateList;
var
  Item: TShoppingItem;
  DisplayText: string;
begin
  ListBoxItems.Items.BeginUpdate;
  try
    ListBoxItems.Items.Clear;

    for Item in FItems do
    begin
      DisplayText := Format('%s (x%d)', [Item.Name, Item.Quantity]);

      if Item.Checked then
        DisplayText := '✓ ' + DisplayText
      else
        DisplayText := '○ ' + DisplayText;

      ListBoxItems.Items.AddObject(DisplayText, Item);
    end;
  finally
    ListBoxItems.Items.EndUpdate;
  end;

  // Mettre à jour le compteur
  LabelStatus.Caption := Format('%d articles', [FItems.Count]);
end;

procedure TMainForm.ListBoxItemsClick(Sender: TObject);
var
  Item: TShoppingItem;
begin
  if ListBoxItems.ItemIndex < 0 then
    Exit;

  Item := TShoppingItem(ListBoxItems.Items.Objects[ListBoxItems.ItemIndex]);

  // Option : clic simple = toggle, clic long = supprimer
  // Pour simplifier, on va afficher un menu
  if window.confirm('Supprimer cet article ?') then
  begin
    DeleteItem(Item.ID);
  end
  else
  begin
    ToggleItem(Item.ID);
  end;
end;

procedure TMainForm.DeleteItem(const AID: string);
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    if FItems[I].ID = AID then
    begin
      FItems.Delete(I);
      Break;
    end;
  end;

  SaveItems;
  UpdateList;
end;

procedure TMainForm.ToggleItem(const AID: string);
var
  Item: TShoppingItem;
begin
  for Item in FItems do
  begin
    if Item.ID = AID then
    begin
      Item.Checked := not Item.Checked;
      Break;
    end;
  end;

  SaveItems;
  UpdateList;
end;

procedure TMainForm.CheckOnlineStatus;
begin
  asm
    if (navigator.onLine) {
      this.LabelStatus.SetCaption(this.LabelStatus.GetCaption() + ' - En ligne');
    } else {
      this.LabelStatus.SetCaption(this.LabelStatus.GetCaption() + ' - Hors ligne');
    }

    // Écouter les changements de statut
    window.addEventListener('online', function() {
      console.log('Application en ligne');
    });

    window.addEventListener('offline', function() {
      console.log('Application hors ligne');
    });
  end;
end;

procedure TMainForm.RegisterServiceWorker;
begin
  asm
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.register('/sw.js')
        .then(function(registration) {
          console.log('Service Worker enregistré:', registration);
        })
        .catch(function(error) {
          console.log('Erreur Service Worker:', error);
        });
    }
  end;
end;

procedure TMainForm.SetupInstallPrompt;
begin
  asm
    var self = this;

    // Capturer l'événement beforeinstallprompt
    window.addEventListener('beforeinstallprompt', function(e) {
      e.preventDefault();
      self.FDeferredPrompt = e;

      // Afficher le bouton d'installation
      self.ButtonInstall.SetVisible(true);
    });

    // Gérer l'installation réussie
    window.addEventListener('appinstalled', function() {
      console.log('PWA installée avec succès');
      self.ButtonInstall.SetVisible(false);
    });
  end;
end;

procedure TMainForm.ButtonInstallClick(Sender: TObject);
begin
  asm
    var self = this;

    if (self.FDeferredPrompt) {
      // Afficher le prompt d'installation
      self.FDeferredPrompt.prompt();

      // Attendre la réponse de l'utilisateur
      self.FDeferredPrompt.userChoice.then(function(choiceResult) {
        if (choiceResult.outcome === 'accepted') {
          console.log('Installation acceptée');
        } else {
          console.log('Installation refusée');
        }

        self.FDeferredPrompt = null;
      });
    }
  end;
end;

end.
```

### 3.3 Design de l'interface (CSS)

TMS Web Core permet d'ajouter du CSS personnalisé :

```css
/* styles.css */

:root {
  --primary-color: #4CAF50;
  --secondary-color: #45a049;
  --background: #f5f5f5;
  --card-background: #ffffff;
  --text-color: #333333;
  --border-color: #dddddd;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
               'Helvetica Neue', Arial, sans-serif;
  margin: 0;
  padding: 0;
  background: var(--background);
}

.header {
  background: var(--primary-color);
  color: white;
  padding: 20px;
  text-align: center;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.header h1 {
  margin: 0;
  font-size: 24px;
  font-weight: 600;
}

.input-panel {
  background: var(--card-background);
  padding: 20px;
  margin: 20px;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.input-group {
  display: flex;
  gap: 10px;
  margin-bottom: 10px;
}

input[type="text"],
input[type="number"] {
  flex: 1;
  padding: 12px;
  border: 1px solid var(--border-color);
  border-radius: 4px;
  font-size: 16px;
}

button {
  background: var(--primary-color);
  color: white;
  border: none;
  padding: 12px 24px;
  border-radius: 4px;
  font-size: 16px;
  cursor: pointer;
  transition: background 0.3s;
}

button:hover {
  background: var(--secondary-color);
}

button:active {
  transform: scale(0.98);
}

.list-container {
  margin: 20px;
}

.list-item {
  background: var(--card-background);
  padding: 15px;
  margin-bottom: 10px;
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  display: flex;
  align-items: center;
  gap: 10px;
  cursor: pointer;
  transition: transform 0.2s;
}

.list-item:hover {
  transform: translateX(5px);
}

.list-item.checked {
  opacity: 0.6;
  text-decoration: line-through;
}

.footer {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
  background: var(--card-background);
  padding: 15px;
  text-align: center;
  border-top: 1px solid var(--border-color);
  box-shadow: 0 -2px 4px rgba(0,0,0,0.1);
}

.install-button {
  background: #2196F3;
  margin-top: 10px;
}

.install-button:hover {
  background: #1976D2;
}

/* Mode sombre */
@media (prefers-color-scheme: dark) {
  :root {
    --background: #121212;
    --card-background: #1e1e1e;
    --text-color: #ffffff;
    --border-color: #333333;
  }
}

/* Responsive */
@media (max-width: 600px) {
  .input-group {
    flex-direction: column;
  }

  input[type="number"] {
    width: 100px;
  }
}
```

---

## Partie 4 : Web App Manifest

### 4.1 Création du manifest.json

Le fichier **manifest.json** décrit votre PWA :

```json
{
  "name": "Shopping List - Ma liste de courses",
  "short_name": "Shopping List",
  "description": "Gérez votre liste de courses facilement, même hors ligne",
  "start_url": "/",
  "scope": "/",
  "display": "standalone",
  "orientation": "portrait-primary",
  "theme_color": "#4CAF50",
  "background_color": "#ffffff",
  "lang": "fr-FR",
  "dir": "ltr",
  "categories": ["shopping", "lifestyle", "productivity"],
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
  ],
  "screenshots": [
    {
      "src": "/images/screenshot1.png",
      "sizes": "540x720",
      "type": "image/png"
    },
    {
      "src": "/images/screenshot2.png",
      "sizes": "540x720",
      "type": "image/png"
    }
  ],
  "shortcuts": [
    {
      "name": "Nouvelle liste",
      "short_name": "Nouvelle",
      "description": "Créer une nouvelle liste de courses",
      "url": "/new",
      "icons": [
        {
          "src": "/images/new-icon.png",
          "sizes": "96x96"
        }
      ]
    }
  ],
  "prefer_related_applications": false
}
```

### 4.2 Explication des propriétés

**Propriétés essentielles** :

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `name` | Nom complet de l'app | "Shopping List - Ma liste..." |
| `short_name` | Nom court (écran accueil) | "Shopping List" |
| `start_url` | URL de démarrage | "/" |
| `display` | Mode d'affichage | "standalone" |
| `theme_color` | Couleur de thème | "#4CAF50" |
| `background_color` | Couleur de fond | "#ffffff" |
| `icons` | Icônes de l'app | Array d'objets |

**Modes d'affichage** :

- **fullscreen** : Plein écran, sans interface navigateur
- **standalone** : App indépendante (recommandé)
- **minimal-ui** : Barre d'adresse minimale
- **browser** : Navigateur normal

### 4.3 Génération des icônes

Vous avez besoin d'icônes en plusieurs tailles. Utilisez des outils en ligne :

**Outils recommandés** :
- [PWA Asset Generator](https://github.com/onderceylan/pwa-asset-generator)
- [RealFaviconGenerator](https://realfavicongenerator.net/)
- [PWA Builder](https://www.pwabuilder.com/)

**Commande avec PWA Asset Generator** :

```bash
npx pwa-asset-generator logo.png ./images/icons --manifest manifest.json
```

**Tailles requises** :
- 72x72, 96x96, 128x128, 144x144, 152x152, 192x192, 384x384, 512x512

### 4.4 Lier le manifest dans index.html

```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Shopping List</title>

  <!-- Manifest PWA -->
  <link rel="manifest" href="/manifest.json">

  <!-- Thème -->
  <meta name="theme-color" content="#4CAF50">

  <!-- Apple -->
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
  <meta name="apple-mobile-web-app-title" content="Shopping List">
  <link rel="apple-touch-icon" href="/images/icon-152x152.png">

  <!-- Styles -->
  <link rel="stylesheet" href="styles.css">

  <!-- Application compilée par TMS Web Core -->
  <script src="ShoppingList.js"></script>
</head>
<body>
  <div id="app"></div>
</body>
</html>
```

---

## Partie 5 : Service Worker

### 5.1 Qu'est-ce qu'un Service Worker ?

Un **Service Worker** est un script JavaScript qui :
- S'exécute en arrière-plan
- Intercepte les requêtes réseau
- Met en cache les ressources
- Permet le mode offline
- Gère les notifications push

**Analogie** : C'est comme un "proxy" entre votre app et internet.

### 5.2 Création du Service Worker (sw.js)

```javascript
// sw.js - Service Worker pour Shopping List PWA

const CACHE_NAME = 'shopping-list-v1';
const OFFLINE_URL = '/offline.html';

// Fichiers à mettre en cache
const CACHE_FILES = [
  '/',
  '/index.html',
  '/ShoppingList.js',
  '/styles.css',
  '/manifest.json',
  '/offline.html',
  '/images/icon-192x192.png',
  '/images/icon-512x512.png'
];

// Installation du Service Worker
self.addEventListener('install', (event) => {
  console.log('[SW] Installation...');

  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => {
        console.log('[SW] Mise en cache des fichiers');
        return cache.addAll(CACHE_FILES);
      })
      .then(() => {
        console.log('[SW] Installation terminée');
        return self.skipWaiting();
      })
  );
});

// Activation du Service Worker
self.addEventListener('activate', (event) => {
  console.log('[SW] Activation...');

  event.waitUntil(
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          if (cacheName !== CACHE_NAME) {
            console.log('[SW] Suppression ancien cache:', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    }).then(() => {
      console.log('[SW] Activation terminée');
      return self.clients.claim();
    })
  );
});

// Interception des requêtes (stratégie Cache First)
self.addEventListener('fetch', (event) => {
  // Ignorer les requêtes non-GET
  if (event.request.method !== 'GET') {
    return;
  }

  event.respondWith(
    caches.match(event.request)
      .then((cachedResponse) => {
        if (cachedResponse) {
          console.log('[SW] Réponse depuis cache:', event.request.url);
          return cachedResponse;
        }

        // Pas en cache, faire la requête réseau
        return fetch(event.request)
          .then((response) => {
            // Ne pas mettre en cache les réponses invalides
            if (!response || response.status !== 200 || response.type !== 'basic') {
              return response;
            }

            // Cloner la réponse
            const responseToCache = response.clone();

            // Mettre en cache
            caches.open(CACHE_NAME)
              .then((cache) => {
                cache.put(event.request, responseToCache);
              });

            return response;
          })
          .catch((error) => {
            console.log('[SW] Erreur réseau:', error);

            // Si navigation HTML, retourner page offline
            if (event.request.mode === 'navigate') {
              return caches.match(OFFLINE_URL);
            }

            return new Response('Contenu non disponible offline', {
              status: 503,
              statusText: 'Service Unavailable'
            });
          });
      })
  );
});

// Gestion des messages du client
self.addEventListener('message', (event) => {
  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }

  if (event.data && event.data.type === 'CLEAR_CACHE') {
    caches.delete(CACHE_NAME).then(() => {
      console.log('[SW] Cache vidé');
    });
  }
});

// Synchronisation en arrière-plan
self.addEventListener('sync', (event) => {
  if (event.tag === 'sync-shopping-list') {
    event.waitUntil(
      syncShoppingList()
    );
  }
});

async function syncShoppingList() {
  console.log('[SW] Synchronisation de la liste...');

  // Récupérer les données locales
  // Envoyer au serveur si nécessaire
  // Cette fonctionnalité nécessite un backend

  return Promise.resolve();
}

// Notifications Push
self.addEventListener('push', (event) => {
  console.log('[SW] Notification push reçue');

  const options = {
    body: event.data ? event.data.text() : 'Nouvelle notification',
    icon: '/images/icon-192x192.png',
    badge: '/images/badge-72x72.png',
    vibrate: [200, 100, 200],
    data: {
      dateOfArrival: Date.now(),
      primaryKey: 1
    },
    actions: [
      {
        action: 'open',
        title: 'Ouvrir',
        icon: '/images/open-icon.png'
      },
      {
        action: 'close',
        title: 'Fermer',
        icon: '/images/close-icon.png'
      }
    ]
  };

  event.waitUntil(
    self.registration.showNotification('Shopping List', options)
  );
});

// Clic sur notification
self.addEventListener('notificationclick', (event) => {
  console.log('[SW] Clic sur notification');

  event.notification.close();

  if (event.action === 'open') {
    event.waitUntil(
      clients.openWindow('/')
    );
  }
});
```

### 5.3 Stratégies de cache

**1. Cache First (Cache d'abord)** :
```javascript
// Utilisé dans notre exemple
caches.match(request)
  .then(response => response || fetch(request))
```
**Bon pour** : Ressources statiques (images, CSS, JS)

**2. Network First (Réseau d'abord)** :
```javascript
fetch(request)
  .catch(() => caches.match(request))
```
**Bon pour** : Données dynamiques avec fallback

**3. Stale While Revalidate** :
```javascript
caches.match(request)
  .then(response => {
    const fetchPromise = fetch(request).then(networkResponse => {
      cache.put(request, networkResponse.clone());
      return networkResponse;
    });
    return response || fetchPromise;
  })
```
**Bon pour** : Équilibre entre rapidité et fraîcheur

### 5.4 Page offline.html

Créez une page pour le mode offline :

```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Hors ligne - Shopping List</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      margin: 0;
      background: #f5f5f5;
      text-align: center;
      padding: 20px;
    }

    .offline-container {
      max-width: 400px;
    }

    .offline-icon {
      font-size: 80px;
      margin-bottom: 20px;
    }

    h1 {
      color: #333;
      margin-bottom: 10px;
    }

    p {
      color: #666;
      line-height: 1.6;
    }

    button {
      background: #4CAF50;
      color: white;
      border: none;
      padding: 12px 24px;
      border-radius: 4px;
      font-size: 16px;
      cursor: pointer;
      margin-top: 20px;
    }

    button:hover {
      background: #45a049;
    }
  </style>
</head>
<body>
  <div class="offline-container">
    <div class="offline-icon">📡</div>
    <h1>Vous êtes hors ligne</h1>
    <p>
      Impossible de charger cette page en ce moment.
      Vérifiez votre connexion internet et réessayez.
    </p>
    <p>
      Vos données locales sont toujours disponibles et seront
      synchronisées dès que vous serez reconnecté.
    </p>
    <button onclick="window.location.reload()">
      Réessayer
    </button>
  </div>
</body>
</html>
```

---

## Partie 6 : Fonctionnalités avancées

### 6.1 Détection de l'état de connexion

Ajoutez dans votre application :

```pascal
procedure TMainForm.MonitorConnectionStatus;
begin
  asm
    var self = this;

    function updateStatus() {
      if (navigator.onLine) {
        self.LabelStatus.SetCaption('En ligne ✓');
        self.LabelStatus.SetElementClassName('status online');

        // Déclencher une synchronisation
        self.SyncWithServer();
      } else {
        self.LabelStatus.SetCaption('Hors ligne ○');
        self.LabelStatus.SetElementClassName('status offline');
      }
    }

    // État initial
    updateStatus();

    // Écouter les changements
    window.addEventListener('online', updateStatus);
    window.addEventListener('offline', updateStatus);
  end;
end;
```

### 6.2 Synchronisation en arrière-plan

```pascal
procedure TMainForm.EnableBackgroundSync;
begin
  asm
    if ('serviceWorker' in navigator && 'SyncManager' in window) {
      navigator.serviceWorker.ready.then(function(registration) {
        return registration.sync.register('sync-shopping-list');
      }).then(function() {
        console.log('Synchronisation planifiée');
      }).catch(function(error) {
        console.log('Erreur de synchronisation:', error);
      });
    }
  end;
end;
```

### 6.3 Notifications Push

**Demander la permission** :

```pascal
procedure TMainForm.RequestNotificationPermission;
begin
  asm
    if ('Notification' in window) {
      Notification.requestPermission().then(function(permission) {
        if (permission === 'granted') {
          console.log('Permission notifications accordée');

          // Afficher une notification de test
          new Notification('Shopping List', {
            body: 'Les notifications sont activées !',
            icon: '/images/icon-192x192.png',
            badge: '/images/badge-72x72.png',
            vibrate: [200, 100, 200]
          });
        }
      });
    }
  end;
end;
```

**Envoyer une notification locale** :

```pascal
procedure TMainForm.ShowNotification(const ATitle, ABody: string);
begin
  asm
    if ('Notification' in window && Notification.permission === 'granted') {
      if ('serviceWorker' in navigator) {
        navigator.serviceWorker.ready.then(function(registration) {
          registration.showNotification(ATitle, {
            body: ABody,
            icon: '/images/icon-192x192.png',
            badge: '/images/badge-72x72.png',
            vibrate: [200, 100, 200],
            tag: 'shopping-list-notification',
            renotify: true,
            requireInteraction: false,
            actions: [
              {
                action: 'view',
                title: 'Voir'
              },
              {
                action: 'close',
                title: 'Fermer'
              }
            ]
          });
        });
      }
    }
  end;
end;

// Utilisation
procedure TMainForm.ButtonAddClick(Sender: TObject);
begin
  AddItem(EditItem.Text, StrToIntDef(EditQuantity.Text, 1));

  // Notifier l'ajout
  ShowNotification('Article ajouté',
    Format('%s a été ajouté à votre liste', [EditItem.Text]));
end;
```

### 6.4 Partage natif

```pascal
procedure TMainForm.ShareList;
var
  ListText: string;
  Item: TShoppingItem;
begin
  // Créer le texte à partager
  ListText := 'Ma liste de courses:' + #13#10;
  for Item in FItems do
  begin
    ListText := ListText + Format('- %s (x%d)', [Item.Name, Item.Quantity]) + #13#10;
  end;

  // Utiliser l'API de partage native
  asm
    if (navigator.share) {
      navigator.share({
        title: 'Ma liste de courses',
        text: ListText,
        url: window.location.href
      }).then(function() {
        console.log('Liste partagée avec succès');
      }).catch(function(error) {
        console.log('Erreur de partage:', error);
      });
    } else {
      // Fallback : copier dans le presse-papiers
      navigator.clipboard.writeText(ListText).then(function() {
        alert('Liste copiée dans le presse-papiers');
      });
    }
  end;
end;
```

### 6.5 Mode d'installation

```pascal
procedure TMainForm.SetupInstallPrompt;
begin
  asm
    var self = this;
    var deferredPrompt;

    // Capturer l'événement
    window.addEventListener('beforeinstallprompt', function(e) {
      e.preventDefault();
      deferredPrompt = e;

      // Afficher le bouton d'installation
      self.ButtonInstall.SetVisible(true);

      console.log('Prompt d\'installation disponible');
    });

    // Gérer le clic sur le bouton
    self.ButtonInstall.SetOnClick(function() {
      if (deferredPrompt) {
        deferredPrompt.prompt();

        deferredPrompt.userChoice.then(function(choiceResult) {
          if (choiceResult.outcome === 'accepted') {
            console.log('PWA installée');
            self.ShowNotification('Installation réussie',
              'Shopping List a été installée sur votre appareil');
          } else {
            console.log('Installation refusée');
          }

          deferredPrompt = null;
          self.ButtonInstall.SetVisible(false);
        });
      }
    });

    // Détecter si déjà installée
    window.addEventListener('appinstalled', function() {
      console.log('PWA installée');
      self.ButtonInstall.SetVisible(false);

      self.ShowNotification('Bienvenue !',
        'Shopping List est maintenant installée sur votre appareil');
    });

    // Vérifier si l'app est lancée en mode standalone
    if (window.matchMedia('(display-mode: standalone)').matches) {
      console.log('Lancée en mode standalone');
      self.ButtonInstall.SetVisible(false);
    }
  end;
end;
```

---

## Partie 7 : Tests et débogage

### 7.1 Tester localement

**Option 1 : Serveur de développement TMS Web Core**

1. Dans Delphi : **Run → Run**
2. L'application s'ouvre dans le navigateur
3. URL : `http://localhost:8000`

**Option 2 : Serveur HTTP simple**

```bash
# Python
python -m http.server 8000

# Node.js
npx http-server -p 8000

# PHP
php -S localhost:8000
```

### 7.2 HTTPS requis

Les PWA nécessitent HTTPS (sauf localhost).

**Pour les tests locaux** :

1. **Utiliser localhost** : Fonctionne en HTTP
2. **Certificat auto-signé** :
```bash
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes
```

3. **Tunnel HTTPS** : Utilisez ngrok
```bash
ngrok http 8000
```

### 7.3 Outils de débogage

**Chrome DevTools** :

1. Ouvrez les DevTools (F12)
2. Onglet **Application**
3. Sections importantes :
   - **Manifest** : Vérifier le manifest.json
   - **Service Workers** : État du SW
   - **Cache Storage** : Contenu du cache
   - **Local Storage** : Données locales

**Lighthouse** :

1. DevTools → Onglet **Lighthouse**
2. Sélectionnez **Progressive Web App**
3. Cliquez **Generate report**
4. Score cible : >90/100

### 7.4 Checklist de test PWA

#### Manifest
- [ ] Fichier manifest.json présent et valide
- [ ] Toutes les icônes sont accessibles
- [ ] `theme_color` et `background_color` définis
- [ ] `display: standalone` configuré

#### Service Worker
- [ ] Service Worker enregistré avec succès
- [ ] Ressources mises en cache
- [ ] Fonctionne offline
- [ ] Stratégie de cache appropriée

#### Installation
- [ ] Prompt d'installation s'affiche
- [ ] App s'installe correctement
- [ ] Icône apparaît sur l'écran d'accueil
- [ ] Lance en mode standalone

#### Fonctionnalité
- [ ] Toutes les fonctions marchent online
- [ ] Fonctions essentielles marchent offline
- [ ] Synchronisation fonctionne
- [ ] Notifications fonctionnent

#### Performance
- [ ] Temps de chargement < 3s
- [ ] Lighthouse score > 90
- [ ] Responsive sur tous les appareils
- [ ] Pas d'erreurs console

---

## Partie 8 : Déploiement

### 8.1 Compilation pour production

Dans Delphi :

1. **Project → Options**
2. **Compiler → Optimization** : Activé
3. **Build → Release**
4. Les fichiers sont dans `Output/Release/`

### 8.2 Hébergement statique

**Options gratuites** :

#### Netlify

1. Créez un compte sur [netlify.com](https://www.netlify.com)
2. Glissez-déposez votre dossier `Output/Release`
3. Netlify génère une URL HTTPS automatiquement
4. Configuration automatique HTTPS

#### Vercel

```bash
# Installer Vercel CLI
npm i -g vercel

# Déployer
cd Output/Release
vercel
```

#### GitHub Pages

1. Créez un repo GitHub
2. Uploadez les fichiers dans `docs/`
3. **Settings → Pages** : Activez GitHub Pages
4. Utilisez une action pour le déploiement

**github-pages.yml** :
```yaml
name: Deploy PWA

on:
  push:
    branches: [ main ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./Output/Release
```

### 8.3 Configuration du serveur

**Fichier .htaccess (Apache)** :

```apache
# Activer la compression
<IfModule mod_deflate.c>
  AddOutputFilterByType DEFLATE text/html text/css text/javascript application/javascript application/json
</IfModule>

# Headers de cache
<IfModule mod_expires.c>
  ExpiresActive On
  ExpiresByType text/html "access plus 0 seconds"
  ExpiresByType application/javascript "access plus 1 year"
  ExpiresByType text/css "access plus 1 year"
  ExpiresByType image/png "access plus 1 year"
  ExpiresByType image/jpeg "access plus 1 year"
</IfModule>

# HTTPS redirect
RewriteEngine On
RewriteCond %{HTTPS} off
RewriteRule ^(.*)$ https://%{HTTP_HOST}%{REQUEST_URI} [L,R=301]

# Service Worker
<Files "sw.js">
  Header set Service-Worker-Allowed "/"
  Header set Cache-Control "no-cache"
</Files>

# Manifest
<Files "manifest.json">
  Header set Content-Type "application/manifest+json"
  Header set Cache-Control "no-cache"
</Files>
```

**Configuration Nginx** :

```nginx
server {
  listen 80;
  server_name votre-domaine.com;
  return 301 https://$server_name$request_uri;
}

server {
  listen 443 ssl http2;
  server_name votre-domaine.com;

  ssl_certificate /path/to/cert.pem;
  ssl_certificate_key /path/to/key.pem;

  root /var/www/shopping-list;
  index index.html;

  # Compression
  gzip on;
  gzip_types text/plain text/css application/json application/javascript;

  # Cache des assets
  location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg)$ {
    expires 1y;
    add_header Cache-Control "public, immutable";
  }

  # Pas de cache pour le SW et manifest
  location ~* (sw\.js|manifest\.json)$ {
    add_header Cache-Control "no-cache";
  }

  # SPA routing
  location / {
    try_files $uri $uri/ /index.html;
  }
}
```

### 8.4 Domaine personnalisé

1. **Acheter un domaine** : Namecheap, OVH, etc.
2. **Configurer DNS** :
```
A     @     185.199.108.153
A     @     185.199.109.153
CNAME www   votre-site.netlify.app
```
3. **Activer HTTPS** : Let's Encrypt (gratuit)

---

## Partie 9 : Optimisation et bonnes pratiques

### 9.1 Performance

**Optimiser le chargement** :

```html
<!-- Précharger les ressources critiques -->
<link rel="preload" href="ShoppingList.js" as="script">
<link rel="preload" href="styles.css" as="style">

<!-- Lazy loading des images -->
<img src="image.jpg" loading="lazy" alt="Description">

<!-- Defer pour les scripts non critiques -->
<script src="analytics.js" defer></script>
```

**Minification** :

```bash
# CSS
npx csso styles.css -o styles.min.css

# JavaScript (déjà fait par TMS Web Core)
```

### 9.2 Accessibilité

```html
<!-- ARIA labels -->
<button aria-label="Ajouter un article">+</button>

<!-- Contraste suffisant -->
<!-- Ratio minimum 4.5:1 pour texte normal -->

<!-- Navigation au clavier -->
<input type="text" tabindex="1">
<button tabindex="2">Ajouter</button>
```

```pascal
// Gérer les raccourcis clavier
procedure TMainForm.SetupKeyboardShortcuts;
begin
  asm
    document.addEventListener('keydown', function(e) {
      // Ctrl+N : Nouveau
      if (e.ctrlKey && e.key === 'n') {
        e.preventDefault();
        self.EditItem.SetFocus();
      }

      // Entrée : Ajouter
      if (e.key === 'Enter' && document.activeElement === self.EditItem.FElement) {
        e.preventDefault();
        self.ButtonAddClick(null);
      }
    });
  end;
end;
```

### 9.3 Sécurité

```html
<!-- Content Security Policy -->
<meta http-equiv="Content-Security-Policy"
      content="default-src 'self';
               script-src 'self' 'unsafe-inline';
               style-src 'self' 'unsafe-inline';">

<!-- Permissions Policy -->
<meta http-equiv="Permissions-Policy"
      content="geolocation=(), microphone=(), camera=()">
```

### 9.4 SEO

```html
<!-- Meta tags essentiels -->
<meta name="description" content="Gérez votre liste de courses facilement">
<meta name="keywords" content="liste, courses, shopping, offline">
<meta name="author" content="Votre Nom">

<!-- Open Graph -->
<meta property="og:title" content="Shopping List">
<meta property="og:description" content="Votre liste de courses">
<meta property="og:image" content="/images/og-image.png">
<meta property="og:url" content="https://votre-site.com">

<!-- Twitter Card -->
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="Shopping List">
<meta name="twitter:description" content="Gérez vos courses">
<meta name="twitter:image" content="/images/twitter-image.png">
```

### 9.5 Analytics

```pascal
// Intégrer Google Analytics
procedure TMainForm.InitAnalytics;
begin
  asm
    // Google Analytics 4
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    gtag('config', 'G-XXXXXXXXXX');

    // Tracker les événements PWA
    window.addEventListener('appinstalled', function() {
      gtag('event', 'pwa_installed');
    });

    // Tracker l'utilisation
    gtag('event', 'page_view', {
      page_title: 'Shopping List',
      page_location: window.location.href
    });
  end;
end;
```

---

## Partie 10 : Maintenance et évolution

### 10.1 Gestion des versions

**Stratégie de versioning** :

```javascript
// sw.js
const VERSION = '1.2.0';
const CACHE_NAME = `shopping-list-v${VERSION}`;

// Notifier l'utilisateur des mises à jour
self.addEventListener('activate', (event) => {
  // Envoyer un message à tous les clients
  event.waitUntil(
    clients.matchAll().then((clients) => {
      clients.forEach((client) => {
        client.postMessage({
          type: 'NEW_VERSION',
          version: VERSION
        });
      });
    })
  );
});
```

**Dans l'application** :

```pascal
procedure TMainForm.ListenForUpdates;
begin
  asm
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.addEventListener('message', function(event) {
        if (event.data.type === 'NEW_VERSION') {
          // Afficher une notification
          if (confirm('Nouvelle version disponible. Recharger ?')) {
            window.location.reload();
          }
        }
      });
    }
  end;
end;
```

### 10.2 Monitoring

```pascal
// Envoyer les erreurs à un service
procedure TMainForm.SetupErrorTracking;
begin
  asm
    window.addEventListener('error', function(event) {
      // Envoyer à Sentry, Rollbar, etc.
      fetch('/api/log-error', {
        method: 'POST',
        body: JSON.stringify({
          message: event.message,
          stack: event.error.stack,
          userAgent: navigator.userAgent,
          timestamp: new Date().toISOString()
        })
      });
    });

    // Erreurs de Service Worker
    navigator.serviceWorker.addEventListener('error', function(event) {
      console.error('Erreur Service Worker:', event);
    });
  end;
end;
```

### 10.3 A/B Testing

```pascal
procedure TMainForm.SetupABTesting;
var
  Variant: string;
begin
  // Déterminer la variante (A ou B)
  asm
    var variant = Math.random() < 0.5 ? 'A' : 'B';
    localStorage.setItem('variant', variant);
    Variant = variant;
  end;

  if Variant = 'A' then
    SetupVariantA
  else
    SetupVariantB;
end;
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous avez créé une PWA complète avec Delphi. Vous maîtrisez maintenant :

✅ **Concepts PWA** : Architecture et fonctionnement
✅ **TMS Web Core** : Développement web avec Delphi
✅ **Manifest** : Configuration de l'application
✅ **Service Workers** : Cache et mode offline
✅ **Installation** : App installable sur tous les appareils
✅ **Notifications** : Push notifications
✅ **Déploiement** : Mise en production HTTPS
✅ **Optimisation** : Performance et SEO

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Créer des PWA professionnelles
🎯 Implémenter le mode offline
🎯 Gérer le cache intelligemment
🎯 Déployer sur le web
🎯 Optimiser les performances
🎯 Maintenir et faire évoluer votre PWA

### Avantages des PWA avec Delphi

**Pour vous, développeur** :
- Un seul code pour toutes les plateformes
- Langage Pascal familier
- Pas de dépendance aux stores
- Déploiement instantané

**Pour vos utilisateurs** :
- Installation légère
- Fonctionne offline
- Toujours à jour
- Performances natives

### Prochaines étapes

**Améliorations suggérées** :
1. **Backend** : API pour synchronisation
2. **Partage** : Listes partagées en équipe
3. **Photos** : Ajouter des images aux produits
4. **Scanner** : Code-barres avec caméra
5. **Export** : PDF, Email
6. **Multilingue** : Support i18n

### Ressources complémentaires

**Documentation** :
- [PWA Documentation MDN](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps)
- [TMS Web Core Docs](https://www.tmssoftware.com/site/tmswebcore.asp)
- [Google PWA Guide](https://web.dev/progressive-web-apps/)

**Outils** :
- [Lighthouse](https://developers.google.com/web/tools/lighthouse)
- [Workbox](https://developers.google.com/web/tools/workbox) (Service Worker)
- [PWA Builder](https://www.pwabuilder.com/)

**Communautés** :
- TMS Web Core Forum
- PWA Community on Twitter
- Reddit r/PWA

### Message final

Les Progressive Web Apps représentent le futur du web. Avec Delphi et TMS Web Core, vous pouvez créer des applications web modernes qui rivalisent avec les applications natives, tout en gardant la simplicité et la puissance du langage Pascal.

Vos PWA fonctionnent partout : mobile, tablette, desktop, sur tous les systèmes d'exploitation, sans passer par les stores, avec des mises à jour instantanées.

**Bon développement de PWA avec Delphi !** 🚀📱💻

---

⏭️ [Projets d'intelligence artificielle et machine learning](/19-projets-avances/07-projets-dintelligence-artificielle-et-machine-learning.md)
