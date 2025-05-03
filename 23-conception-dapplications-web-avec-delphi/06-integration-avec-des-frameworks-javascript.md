# 23.6 Intégration avec des frameworks JavaScript

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Nous avons vu comment créer des sites web dynamiques avec Delphi, mais pour développer des interfaces utilisateur web modernes et interactives, l'intégration avec des frameworks JavaScript populaires peut être très avantageuse. Dans cette section, nous allons explorer comment combiner la puissance de Delphi côté serveur avec des frameworks JavaScript côté client pour créer des applications web complètes.

![Note] Ce chapitre nécessite des connaissances de base en JavaScript et en développement web. Cependant, nous avons structuré le contenu pour qu'il soit aussi accessible que possible aux débutants.

## Pourquoi intégrer des frameworks JavaScript ?

Les frameworks JavaScript modernes offrent de nombreux avantages :

- **Interfaces utilisateur réactives** : Mise à jour dynamique sans rechargement de page
- **Expérience utilisateur améliorée** : Animations fluides, transitions et interactions riches
- **Développement modulaire** : Organisation du code en composants réutilisables
- **Écosystème riche** : Nombreuses bibliothèques et extensions disponibles
- **Performances optimisées** : Rendu efficace avec des techniques comme le DOM virtuel

## Les frameworks JavaScript populaires

Avant de commencer, voici un aperçu des frameworks JavaScript les plus populaires que vous pourriez intégrer avec Delphi :

### React

Développé par Facebook, React est une bibliothèque pour construire des interfaces utilisateur à base de composants. React utilise un DOM virtuel pour optimiser les performances de rendu.

### Vue.js

Vue.js est un framework progressif qui se concentre sur la couche vue. Il est conçu pour être adopté de manière incrémentale et s'intègre facilement dans d'autres projets.

### Angular

Développé par Google, Angular est un framework complet qui inclut tout ce dont vous avez besoin pour créer des applications web complexes, avec une architecture basée sur les composants.

### Svelte

Svelte adopte une approche différente en effectuant le travail à la compilation plutôt qu'à l'exécution, ce qui se traduit par des applications plus légères et plus rapides.

## Approches d'intégration avec Delphi

Il existe plusieurs façons d'intégrer Delphi avec des frameworks JavaScript :

1. **API REST Delphi + Application JavaScript séparée**
2. **Delphi comme serveur de données avec rendu côté client**
3. **Composants web intégrés directement dans les applications Delphi**
4. **TMS WEB Core avec interaction JavaScript**

Dans ce tutoriel, nous allons nous concentrer sur les deux premières approches, qui sont les plus courantes et les plus flexibles.

## 1. Création d'une API REST avec Delphi

La première étape consiste à créer une API REST avec Delphi qui servira de backend à notre application JavaScript.

### Étape 1 : Créer un projet WebBroker

Commençons par créer une API REST simple :

1. Sélectionnez **Fichier** > **Nouveau** > **Autres** > **Delphi Projects** > **Web Server Application**
2. Choisissez **WebBroker Application** et **Stand Alone Application**
3. Cliquez sur **OK**

### Étape 2 : Configurer le module Web pour une API REST

Modifiez le fichier `WebModuleUnit1.pas` pour ajouter des actions qui renverront des données au format JSON :

```delphi
unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.JSON;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    procedure ConfigureCORS(Response: TWebResponse);
  public
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule1.ConfigureCORS(Response: TWebResponse);
begin
  // Configurer les en-têtes CORS pour permettre les requêtes cross-origin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  Response.SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.StatusCode := 404;
  Response.Content := '{"error": "Route not found"}';
  Response.ContentType := 'application/json';
  ConfigureCORS(Response);
  Handled := True;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  Action: TWebActionItem;
begin
  // Ajouter une action pour obtenir la liste des produits
  Action := Actions.Add;
  Action.Name := 'ActionGetProduits';
  Action.PathInfo := '/api/produits';
  Action.MethodType := mtGet;
  Action.OnAction := ActionGetProduitsAction;

  // Ajouter une action pour obtenir un produit spécifique
  Action := Actions.Add;
  Action.Name := 'ActionGetProduit';
  Action.PathInfo := '/api/produits/:id';
  Action.MethodType := mtGet;
  Action.OnAction := ActionGetProduitAction;

  // Ajouter d'autres actions selon vos besoins...
end;

procedure TWebModule1.ActionGetProduitsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
begin
  // Créer un tableau JSON pour les produits
  JSONArray := TJSONArray.Create;

  try
    // Ajouter quelques produits fictifs
    // En pratique, ces données viendraient d'une base de données

    // Produit 1
    JSONObj := TJSONObject.Create;
    JSONObj.AddPair('id', TJSONNumber.Create(1));
    JSONObj.AddPair('nom', 'Écran 24 pouces');
    JSONObj.AddPair('description', 'Écran HD avec une excellente qualité d''image.');
    JSONObj.AddPair('prix', TJSONNumber.Create(199.99));
    JSONObj.AddPair('categorie', 'Informatique');
    JSONArray.AddElement(JSONObj);

    // Produit 2
    JSONObj := TJSONObject.Create;
    JSONObj.AddPair('id', TJSONNumber.Create(2));
    JSONObj.AddPair('nom', 'Clavier mécanique');
    JSONObj.AddPair('description', 'Clavier gaming avec rétroéclairage RGB.');
    JSONObj.AddPair('prix', TJSONNumber.Create(89.99));
    JSONObj.AddPair('categorie', 'Informatique');
    JSONArray.AddElement(JSONObj);

    // Produit 3
    JSONObj := TJSONObject.Create;
    JSONObj.AddPair('id', TJSONNumber.Create(3));
    JSONObj.AddPair('nom', 'Casque audio');
    JSONObj.AddPair('description', 'Casque avec réduction de bruit active.');
    JSONObj.AddPair('prix', TJSONNumber.Create(149.99));
    JSONObj.AddPair('categorie', 'Audio');
    JSONArray.AddElement(JSONObj);

    // Configurer la réponse
    Response.ContentType := 'application/json';
    Response.Content := JSONArray.ToString;
    ConfigureCORS(Response);
  finally
    // Le JSON sera libéré automatiquement avec la réponse
    JSONArray.Owned := False;
  end;

  Handled := True;
end;

procedure TWebModule1.ActionGetProduitAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  IdStr: string;
  Id: Integer;
  JSONObj: TJSONObject;
begin
  // Extraire l'ID du produit de l'URL
  IdStr := Request.PathInfo;
  IdStr := StringReplace(IdStr, '/api/produits/', '', [rfIgnoreCase]);

  if TryStrToInt(IdStr, Id) then
  begin
    // Créer un objet JSON pour le produit
    JSONObj := TJSONObject.Create;

    try
      // En pratique, ces données viendraient d'une base de données
      case Id of
        1:
          begin
            JSONObj.AddPair('id', TJSONNumber.Create(1));
            JSONObj.AddPair('nom', 'Écran 24 pouces');
            JSONObj.AddPair('description', 'Écran HD avec une excellente qualité d''image.');
            JSONObj.AddPair('prix', TJSONNumber.Create(199.99));
            JSONObj.AddPair('categorie', 'Informatique');
          end;
        2:
          begin
            JSONObj.AddPair('id', TJSONNumber.Create(2));
            JSONObj.AddPair('nom', 'Clavier mécanique');
            JSONObj.AddPair('description', 'Clavier gaming avec rétroéclairage RGB.');
            JSONObj.AddPair('prix', TJSONNumber.Create(89.99));
            JSONObj.AddPair('categorie', 'Informatique');
          end;
        3:
          begin
            JSONObj.AddPair('id', TJSONNumber.Create(3));
            JSONObj.AddPair('nom', 'Casque audio');
            JSONObj.AddPair('description', 'Casque avec réduction de bruit active.');
            JSONObj.AddPair('prix', TJSONNumber.Create(149.99));
            JSONObj.AddPair('categorie', 'Audio');
          end;
        else
          begin
            Response.StatusCode := 404;
            Response.Content := '{"error": "Produit non trouvé"}';
            Response.ContentType := 'application/json';
            ConfigureCORS(Response);
            Handled := True;
            Exit;
          end;
      end;

      // Configurer la réponse
      Response.ContentType := 'application/json';
      Response.Content := JSONObj.ToString;
      ConfigureCORS(Response);
    finally
      // Le JSON sera libéré automatiquement avec la réponse
      JSONObj.Owned := False;
    end;
  end
  else
  begin
    Response.StatusCode := 400;
    Response.Content := '{"error": "ID de produit invalide"}';
    Response.ContentType := 'application/json';
    ConfigureCORS(Response);
  end;

  Handled := True;
end;

end.
```

### Étape 3 : Tester l'API REST

1. Compilez et exécutez votre application Delphi
2. Ouvrez un navigateur et accédez à `http://localhost:8080/api/produits`
3. Vous devriez voir une liste de produits au format JSON
4. Essayez également `http://localhost:8080/api/produits/1` pour voir un produit spécifique

## 2. Intégration avec React

Maintenant que notre API REST est fonctionnelle, créons une application React simple qui consommera cette API.

### Étape 1 : Préparer l'environnement React

Si vous n'avez jamais utilisé React auparavant, vous aurez besoin de Node.js et npm installés sur votre système. Ensuite, vous pouvez créer une nouvelle application React :

```bash
npx create-react-app boutique-delphi
cd boutique-delphi
```

### Étape 2 : Créer des composants React

Modifiez le fichier `src/App.js` pour créer une application React simple qui se connecte à notre API Delphi :

```jsx
import React, { useState, useEffect } from 'react';
import './App.css';

function App() {
  const [produits, setProduits] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    // Charger les produits depuis l'API Delphi
    fetch('http://localhost:8080/api/produits')
      .then(response => {
        if (!response.ok) {
          throw new Error('Erreur réseau lors de la récupération des produits');
        }
        return response.json();
      })
      .then(data => {
        setProduits(data);
        setLoading(false);
      })
      .catch(error => {
        setError(error.message);
        setLoading(false);
      });
  }, []);

  // Afficher un message de chargement
  if (loading) {
    return <div className="App">Chargement des produits...</div>;
  }

  // Afficher une erreur si nécessaire
  if (error) {
    return <div className="App">Erreur : {error}</div>;
  }

  return (
    <div className="App">
      <header className="App-header">
        <h1>Boutique en ligne</h1>
      </header>
      <main>
        <h2>Nos produits</h2>
        <div className="produits-grid">
          {produits.map(produit => (
            <div key={produit.id} className="produit-card">
              <h3>{produit.nom}</h3>
              <p>{produit.description}</p>
              <p className="prix">{produit.prix.toFixed(2)} €</p>
              <p className="categorie">Catégorie : {produit.categorie}</p>
              <button>Ajouter au panier</button>
            </div>
          ))}
        </div>
      </main>
    </div>
  );
}

export default App;
```

### Étape 3 : Ajouter du style

Ajoutez du style à votre application React en modifiant le fichier `src/App.css` :

```css
.App {
  font-family: Arial, sans-serif;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

.App-header {
  background-color: #4a6da7;
  color: white;
  padding: 20px;
  margin-bottom: 20px;
  border-radius: 5px;
}

.produits-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 20px;
}

.produit-card {
  border: 1px solid #ddd;
  border-radius: 5px;
  padding: 20px;
  box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
}

.produit-card h3 {
  margin-top: 0;
  color: #4a6da7;
}

.prix {
  font-size: 1.2em;
  font-weight: bold;
  color: #4a6da7;
}

.categorie {
  color: #666;
  font-style: italic;
}

button {
  background-color: #4a6da7;
  color: white;
  border: none;
  padding: 10px 15px;
  border-radius: 3px;
  cursor: pointer;
  font-size: 1em;
}

button:hover {
  background-color: #3a5d97;
}
```

### Étape 4 : Lancer l'application React

Dans le terminal, exécutez la commande suivante pour démarrer l'application React :

```bash
npm start
```

Votre navigateur devrait s'ouvrir automatiquement à l'adresse `http://localhost:3000`, affichant la liste des produits récupérés depuis votre API Delphi.

## 3. Intégration avec Vue.js

Voyons maintenant comment intégrer Delphi avec Vue.js, un framework JavaScript plus léger et plus facile à apprendre pour les débutants.

### Étape 1 : Créer un fichier HTML avec Vue.js

Pour commencer simplement, nous allons utiliser Vue.js directement via un CDN. Créez un dossier `vue-app` et à l'intérieur, un fichier `index.html` :

```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Boutique Vue.js + Delphi</title>
  <script src="https://cdn.jsdelivr.net/npm/vue@2.6.14/dist/vue.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/axios/dist/axios.min.js"></script>
  <style>
    body {
      font-family: Arial, sans-serif;
      max-width: 1200px;
      margin: 0 auto;
      padding: 20px;
    }
    header {
      background-color: #4a6da7;
      color: white;
      padding: 20px;
      margin-bottom: 20px;
      border-radius: 5px;
    }
    .produits-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
      gap: 20px;
    }
    .produit-card {
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 20px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
    }
    .produit-card h3 {
      margin-top: 0;
      color: #4a6da7;
    }
    .prix {
      font-size: 1.2em;
      font-weight: bold;
      color: #4a6da7;
    }
    .categorie {
      color: #666;
      font-style: italic;
    }
    button {
      background-color: #4a6da7;
      color: white;
      border: none;
      padding: 10px 15px;
      border-radius: 3px;
      cursor: pointer;
      font-size: 1em;
    }
    button:hover {
      background-color: #3a5d97;
    }
    .loader {
      text-align: center;
      padding: 30px;
      font-size: 1.2em;
    }
    .error {
      color: red;
      padding: 20px;
      background-color: #ffeeee;
      border-radius: 5px;
      margin-bottom: 20px;
    }
  </style>
</head>
<body>
  <div id="app">
    <header>
      <h1>Boutique en ligne</h1>
    </header>

    <div v-if="erreur" class="error">
      <p>{{ erreur }}</p>
    </div>

    <div v-if="chargement" class="loader">
      Chargement des produits...
    </div>

    <main v-else>
      <h2>Nos produits</h2>
      <div class="produits-grid">
        <div v-for="produit in produits" :key="produit.id" class="produit-card">
          <h3>{{ produit.nom }}</h3>
          <p>{{ produit.description }}</p>
          <p class="prix">{{ produit.prix.toFixed(2) }} €</p>
          <p class="categorie">Catégorie : {{ produit.categorie }}</p>
          <button @click="ajouterAuPanier(produit)">Ajouter au panier</button>
        </div>
      </div>

      <div v-if="panier.length > 0" style="margin-top: 30px; padding: 20px; background-color: #f5f5f5; border-radius: 5px;">
        <h2>Votre panier</h2>
        <ul>
          <li v-for="(item, index) in panier" :key="index">
            {{ item.nom }} - {{ item.prix.toFixed(2) }} €
            <button style="padding: 2px 5px; margin-left: 10px;" @click="supprimerDuPanier(index)">×</button>
          </li>
        </ul>
        <p><strong>Total : {{ totalPanier.toFixed(2) }} €</strong></p>
        <button>Passer commande</button>
      </div>
    </main>
  </div>

  <script>
    new Vue({
      el: '#app',
      data: {
        produits: [],
        panier: [],
        chargement: true,
        erreur: null
      },
      computed: {
        totalPanier() {
          return this.panier.reduce((total, item) => total + item.prix, 0);
        }
      },
      methods: {
        chargerProduits() {
          axios.get('http://localhost:8080/api/produits')
            .then(response => {
              this.produits = response.data;
              this.chargement = false;
            })
            .catch(error => {
              this.erreur = 'Erreur lors du chargement des produits : ' + error.message;
              this.chargement = false;
            });
        },
        ajouterAuPanier(produit) {
          this.panier.push({...produit});
        },
        supprimerDuPanier(index) {
          this.panier.splice(index, 1);
        }
      },
      mounted() {
        this.chargerProduits();
      }
    });
  </script>
</body>
</html>
```

### Étape 2 : Servir l'application Vue.js

Pour tester notre application Vue.js, nous avons plusieurs options :

#### Option 1 : Ouvrir directement le fichier HTML

Vous pouvez simplement ouvrir le fichier `index.html` dans votre navigateur. Cependant, certains navigateurs bloquent les requêtes AJAX vers des fichiers locaux pour des raisons de sécurité.

#### Option 2 : Utiliser un serveur web simple

Une meilleure option est d'utiliser un serveur web simple. Si vous avez Node.js installé, vous pouvez utiliser `http-server` :

```bash
npm install -g http-server
cd vue-app
http-server
```

Puis accédez à `http://localhost:8080` dans votre navigateur.

## 4. Intégration avec Delphi comme serveur de fichiers statiques

Pour que l'intégration soit complète, nous pouvons configurer notre application Delphi pour servir également les fichiers statiques de notre application frontend.

Ajoutez une action pour servir les fichiers statiques de l'application Vue.js :

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  Action: TWebActionItem;
begin
  // ... autres actions existantes ...

  // Ajouter une action pour servir le fichier index.html
  Action := Actions.Add;
  Action.Name := 'ActionServeRoot';
  Action.PathInfo := '/';
  Action.MethodType := mtGet;
  Action.OnAction := ActionServeRootAction;

  // Ajouter une action pour servir les fichiers statiques
  Action := Actions.Add;
  Action.Name := 'ActionServeStatic';
  Action.PathInfo := '/static/*';
  Action.MethodType := mtGet;
  Action.OnAction := ActionServeStaticAction;
end;

procedure TWebModule1.ActionServeRootAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FilePath: string;
  FileContent: TStringList;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\index.html';

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
    Response.Content := 'Fichier index.html non trouvé';
  end;

  Handled := True;
end;

procedure TWebModule1.ActionServeStaticAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FileName, FilePath, FileExt: string;
begin
  // Extraire le nom du fichier de l'URL
  FileName := StringReplace(Request.PathInfo, '/static/', '', [rfIgnoreCase]);

  // Vérifier que le nom de fichier est valide (éviter les attaques par traversée de répertoire)
  if (Pos('..', FileName) > 0) or (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0) then
  begin
    Response.StatusCode := 403; // Forbidden
    Response.ContentType := 'text/plain';
    Response.Content := 'Accès refusé';
    Handled := True;
    Exit;
  end;

  // Chemin complet vers le fichier
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\static\' + FileName;

  if not FileExists(FilePath) then
  begin
    Response.StatusCode := 404; // Not Found
    Response.ContentType := 'text/plain';
    Response.Content := 'Fichier non trouvé';
    Handled := True;
    Exit;
  end;

  // Déterminer le type MIME en fonction de l'extension
  FileExt := LowerCase(ExtractFileExt(FileName));
  if FileExt = '.css' then
    Response.ContentType := 'text/css'
  else if FileExt = '.js' then
    Response.ContentType := 'application/javascript'
  else if FileExt = '.png' then
    Response.ContentType := 'image/png'
  else if FileExt = '.jpg' or FileExt = '.jpeg' then
    Response.ContentType := 'image/jpeg'
  else if FileExt = '.gif' then
    Response.ContentType := 'image/gif'
  else if FileExt = '.svg' then
    Response.ContentType := 'image/svg+xml'
  else
    Response.ContentType := 'application/octet-stream';

  // Lire le fichier et l'envoyer comme réponse
  try
    Response.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    Response.FreeContentStream := True;
  except
    on E: Exception do
    begin
      Response.StatusCode := 500; // Internal Server Error
      Response.ContentType := 'text/plain';
      Response.Content := 'Erreur lors de la lecture du fichier';
    end;
  end;

  Handled := True;
end;
```

Avec cette configuration, vous pouvez regrouper votre application frontend et backend dans un seul exécutable Delphi.

## 5. Intégration avec TMS WEB Core

TMS WEB Core est une extension pour Delphi qui permet de développer des applications web directement dans l'IDE Delphi, en utilisant le langage Pascal, qui est ensuite transpilé en JavaScript.

### Utilisation de TMS WEB Core avec des frameworks JavaScript

TMS WEB Core peut être utilisé conjointement avec des frameworks JavaScript existants. Voici comment intégrer React dans une application TMS WEB Core :

1. Créez un nouveau projet TMS Web Application dans Delphi
2. Dans le fichier `Unit1.pas`, ajoutez une méthode pour initialiser React :

```delphi
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs;

type
  TForm1 = class(TWebForm)
    procedure WebFormCreate(Sender: TObject);
  private
    procedure InitReact;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  // Initialiser React une fois que le DOM est prêt
  InitReact;
end;

procedure TForm1.InitReact;
begin
  // Créer un élément div pour React
  asm
    // Créer un élément div pour React
    var reactRoot = document.createElement('div');
    reactRoot.id = 'react-root';
    document.body.appendChild(reactRoot);

    // Importer React et ReactDOM (assurez-vous d'avoir inclus les scripts dans votre page)
    // Note: Cela suppose que vous avez déjà ajouté les scripts React dans votre HTML

    // Définir un composant React simple
    function App() {
      const [count, setCount] = React.useState(0);

      return React.createElement('div', null, [
        React.createElement('h1', null, 'TMS Web Core + React'),
        React.createElement('p', null, `Compteur: ${count}`),
        React.createElement('button', { onClick: () => setCount(count + 1) }, 'Incrémenter')
      ]);
    }

    // Rendre le composant React
    ReactDOM.render(
      React.createElement(App),
      document.getElementById('react-root')
    );
  end;
end;

end.
```

Avant cela, vous devrez vous assurer que les bibliothèques React sont disponibles. Vous pouvez les ajouter dans le fichier HTML principal de votre application.

## Bonnes pratiques pour l'intégration avec des frameworks JavaScript

1. **Séparation des responsabilités** : Utilisez Delphi pour ce qu'il fait le mieux (backend, accès aux données) et les frameworks JavaScript pour l'interface utilisateur.

2. **API clairement définies** : Concevez des API REST bien structurées avec une documentation claire.

3. **Gestion de la sécurité** : Implémentez CORS correctement, utilisez HTTPS en production et sécurisez vos API avec une authentification appropriée.

4. **Performance** : Optimisez les requêtes API pour minimiser les allers-retours entre le client et le serveur.

5. **Gestion d'état** : Choisissez judicieusement où stocker l'état de l'application (côté client vs côté serveur).

6. **Déploiement unifié** : Envisagez de packager ensemble l'application frontend et backend pour simplifier le déploiement.

## Techniques avancées d'intégration

### Utilisation de WebSockets pour les communications en temps réel

Les API REST sont excellentes pour la plupart des communications client-serveur, mais pour les applications nécessitant des mises à jour en temps réel (chat, notifications, tableaux de bord en direct), les WebSockets sont plus appropriés.

Delphi peut facilement implémenter des serveurs WebSocket avec des bibliothèques comme sgcWebSockets. Voici un exemple simple :

```delphi
unit WebSocketServer;

interface

uses
  System.SysUtils, System.Classes, sgcWebSocket_Server, sgcWebSocket, sgcBase_Classes;

type
  TWebSocketServerModule = class(TDataModule)
    WSServer: TsgcWebSocketServer;
    procedure WSServerConnect(Connection: TsgcWSConnection);
    procedure WSServerDisconnect(Connection: TsgcWSConnection);
    procedure WSServerMessage(Connection: TsgcWSConnection; const Text: string);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure BroadcastMessage(const Text: string);
  end;

var
  WebSocketServerModule: TWebSocketServerModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebSocketServerModule.DataModuleCreate(Sender: TObject);
begin
  WSServer.Port := 8081;
  WSServer.Active := True;
end;

procedure TWebSocketServerModule.WSServerConnect(Connection: TsgcWSConnection);
begin
  // Informer tout le monde qu'un nouveau client s'est connecté
  BroadcastMessage('{"type": "connection", "message": "Un nouvel utilisateur s''est connecté"}');
end;

procedure TWebSocketServerModule.WSServerDisconnect(Connection: TsgcWSConnection);
begin
  // Informer tout le monde qu'un client s'est déconnecté
  BroadcastMessage('{"type": "connection", "message": "Un utilisateur s''est déconnecté"}');
end;

procedure TWebSocketServerModule.WSServerMessage(Connection: TsgcWSConnection;
  const Text: string);
begin
  // Retransmettre le message à tous les clients
  BroadcastMessage('{"type": "message", "content": ' + Text + '}');
end;

procedure TWebSocketServerModule.BroadcastMessage(const Text: string);
begin
  WSServer.Broadcast(Text);
end;

end.
```

Côté client (JavaScript), vous pouvez vous connecter au serveur WebSocket :

```javascript
// Établir une connexion WebSocket
const socket = new WebSocket('ws://localhost:8081');

// Gestion des événements de connexion
socket.onopen = (event) => {
  console.log('Connecté au serveur WebSocket');
};

// Gestion des messages entrants
socket.onmessage = (event) => {
  const data = JSON.parse(event.data);

  if (data.type === 'connection') {
    // Afficher un message de connexion
    console.log(data.message);
  } else if (data.type === 'message') {
    // Traiter un message normal
    displayMessage(data.content);
  }
};

// Gestion des erreurs
socket.onerror = (error) => {
  console.error('Erreur WebSocket:', error);
};

// Gestion de la fermeture de connexion
socket.onclose = (event) => {
  console.log('Déconnecté du serveur WebSocket');
};

// Fonction pour envoyer un message
function sendMessage(message) {
  if (socket.readyState === WebSocket.OPEN) {
    socket.send(JSON.stringify({ message }));
  }
}
```

### Intégration avec des bibliothèques de visualisation de données

Les frameworks JavaScript comme D3.js, Chart.js ou Plotly sont excellents pour visualiser des données. Vous pouvez les combiner avec Delphi pour créer des tableaux de bord puissants :

1. Récupérez les données via une API REST Delphi
2. Visualisez-les côté client avec D3.js

Exemple avec Chart.js :

```delphi
// Côté Delphi : endpoint pour récupérer des statistiques de ventes
procedure TWebModule1.ActionGetStatistiquesVentesAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
begin
  // Créer un tableau JSON pour les données
  JSONArray := TJSONArray.Create;

  try
    // En pratique, ces données viendraient d'une requête SQL
    for var Mois := 1 to 12 do
    begin
      JSONObj := TJSONObject.Create;
      JSONObj.AddPair('mois', FormatDateTime('mmmm', EncodeDate(2023, Mois, 1)));

      // Simule des valeurs de ventes aléatoires
      JSONObj.AddPair('ventes', TJSONNumber.Create(Random(10000) + 5000));
      JSONObj.AddPair('objectif', TJSONNumber.Create(8000));

      JSONArray.AddElement(JSONObj);
    end;

    Response.ContentType := 'application/json';
    Response.Content := JSONArray.ToString;
    ConfigureCORS(Response);
  finally
    JSONArray.Owned := False;
  end;

  Handled := True;
end;
```

```javascript
// Côté JavaScript : afficher un graphique avec Chart.js
async function afficherGraphiqueVentes() {
  try {
    // Récupérer les données depuis l'API Delphi
    const response = await fetch('http://localhost:8080/api/statistiques-ventes');
    const donnees = await response.json();

    // Préparer les données pour Chart.js
    const mois = donnees.map(item => item.mois);
    const ventes = donnees.map(item => item.ventes);
    const objectifs = donnees.map(item => item.objectif);

    // Créer le graphique
    const ctx = document.getElementById('graphique-ventes').getContext('2d');
    new Chart(ctx, {
      type: 'line',
      data: {
        labels: mois,
        datasets: [
          {
            label: 'Ventes',
            data: ventes,
            backgroundColor: 'rgba(74, 109, 167, 0.2)',
            borderColor: 'rgba(74, 109, 167, 1)',
            borderWidth: 2
          },
          {
            label: 'Objectifs',
            data: objectifs,
            backgroundColor: 'rgba(255, 99, 132, 0.2)',
            borderColor: 'rgba(255, 99, 132, 1)',
            borderWidth: 2,
            borderDash: [5, 5]
          }
        ]
      },
      options: {
        responsive: true,
        scales: {
          y: {
            beginAtZero: true
          }
        }
      }
    });
  } catch (error) {
    console.error('Erreur lors du chargement des données:', error);
  }
}

// Appeler la fonction lorsque la page est chargée
document.addEventListener('DOMContentLoaded', afficherGraphiqueVentes);
```

## Cas d'usage pratiques

### 1. Tableau de bord administratif

Un cas d'usage classique est un tableau de bord administratif où :
- Delphi fournit une API REST pour accéder aux données métier
- Un framework JavaScript (comme React ou Vue.js) crée une interface utilisateur riche et interactive

### 2. Application mobile hybride

Vous pouvez utiliser Delphi pour le backend et un framework comme Ionic (basé sur Angular) pour créer une application mobile hybride :
- L'API Delphi expose les fonctionnalités métier
- L'application Ionic consomme ces API et s'exécute sur iOS et Android

### 3. Modernisation d'applications existantes

Si vous avez une application Delphi existante, vous pouvez la moderniser progressivement :
1. Exposez certaines fonctionnalités via des API REST
2. Créez de nouvelles interfaces utilisateur avec des frameworks JavaScript modernes
3. Intégrez ces nouvelles interfaces dans votre application existante

## Conclusion

L'intégration de Delphi avec des frameworks JavaScript ouvre de nombreuses possibilités pour développer des applications web modernes. En combinant la robustesse de Delphi côté serveur avec la richesse des frameworks JavaScript côté client, vous pouvez créer des applications complètes qui tirent parti des forces de chaque technologie.

Les approches présentées dans ce tutoriel ne sont que le début. À mesure que vous vous familiarisez avec ces concepts, vous pourrez explorer des intégrations plus avancées et créer des applications web sophistiquées avec Delphi.

⏭️ [Progressive Web Apps (PWA)](/23-conception-dapplications-web-avec-delphi/07-progressive-web-apps.md)
