🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.1 Appels REST et API Web (TRESTClient)

## Introduction aux API REST

### Qu'est-ce qu'une API REST ?

Une **API REST** (Representational State Transfer) est un moyen standardisé pour permettre à des applications de communiquer entre elles via Internet. Imaginez-la comme un serveur dans un restaurant : vous passez une commande (requête), et le serveur vous apporte ce que vous avez demandé (réponse).

Les API REST utilisent le protocole HTTP, le même qui permet de naviguer sur le web. Elles sont devenues le standard pour échanger des données entre applications modernes.

### Les principes de base

**Les méthodes HTTP principales :**

- **GET** : Récupérer des données (comme lire une page web)
- **POST** : Créer de nouvelles données (comme soumettre un formulaire)
- **PUT** : Modifier des données existantes
- **DELETE** : Supprimer des données

**Le format des données :**

Les API REST échangent généralement des données au format **JSON** (JavaScript Object Notation), qui est facile à lire et à manipuler.

Exemple de données JSON :
```json
{
  "id": 1,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@example.com"
}
```

## Les composants REST dans Delphi

Delphi intègre des composants puissants pour travailler avec les API REST, regroupés dans la palette **REST Client**.

### Les composants essentiels

1. **TRESTClient** : Le client qui établit la connexion avec l'API
2. **TRESTRequest** : La requête que vous envoyez à l'API
3. **TRESTResponse** : La réponse renvoyée par l'API

Ces trois composants fonctionnent ensemble pour effectuer des appels REST.

## Configuration de base

### Étape 1 : Ajouter les composants

Sur votre formulaire, ajoutez depuis la palette **REST Client** :

- Un composant `TRESTClient`
- Un composant `TRESTRequest`
- Un composant `TRESTResponse`

### Étape 2 : Lier les composants

Dans l'Inspecteur d'objets :

1. Sélectionnez `RESTRequest1`
2. Dans la propriété `Client`, choisissez `RESTClient1`
3. Dans la propriété `Response`, choisissez `RESTResponse1`

Vos composants sont maintenant connectés et prêts à communiquer.

## Premier appel REST simple

### Récupérer des données avec GET

Imaginons que vous voulez récupérer des informations depuis une API publique. Voici comment procéder :

**Configuration du TRESTClient :**

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Définir l'URL de base de l'API
  RESTClient1.BaseURL := 'https://jsonplaceholder.typicode.com';
end;
```

**Effectuer une requête GET :**

```pascal
procedure TForm1.ButtonGetDataClick(Sender: TObject);  
begin  
  // Configurer la requête
  RESTRequest1.Method := rmGET;
  RESTRequest1.Resource := 'users/1';

  // Exécuter la requête
  RESTRequest1.Execute;

  // Afficher la réponse
  Memo1.Text := RESTResponse1.Content;
end;
```

**Explication du code :**

- `BaseURL` : L'adresse de base de l'API
- `Resource` : Le chemin spécifique que vous voulez interroger
- `Method` : Le type de requête (ici GET)
- `Execute` : Lance l'appel à l'API
- `Content` : Le contenu de la réponse au format texte

## Analyser les réponses JSON

### Utiliser TJSONObject

Delphi propose la classe `TJSONObject` pour manipuler facilement les données JSON.

```pascal
uses
  System.JSON;

procedure TForm1.ButtonParseJSONClick(Sender: TObject);  
var  
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Nom, Email: string;
begin
  // Exécuter la requête
  RESTRequest1.Execute;

  // Parser la réponse JSON
  JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire les valeurs
      Nom := JSONObject.GetValue<string>('name');
      Email := JSONObject.GetValue<string>('email');

      // Afficher les résultats
      ShowMessage('Nom: ' + Nom + #13#10 + 'Email: ' + Email);
    end;
  finally
    JSONValue.Free;
  end;
end;
```

**Points importants :**

- Toujours libérer la mémoire avec `Free`
- Vérifier que le parsing a réussi avant d'utiliser les données
- Utiliser `GetValue<Type>` pour extraire des valeurs typées

## Envoyer des données avec POST

### Créer une nouvelle ressource

Pour envoyer des données à une API, vous utilisez la méthode POST :

```pascal
procedure TForm1.ButtonCreateUserClick(Sender: TObject);  
var  
  JSONObject: TJSONObject;
begin
  // Créer un objet JSON avec les données
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', 'Marie Martin');
    JSONObject.AddPair('email', 'marie.martin@example.com');
    JSONObject.AddPair('username', 'mmartin');

    // Configurer la requête POST
    RESTRequest1.Method := rmPOST;
    RESTRequest1.Resource := 'users';

    // Ajouter le corps de la requête
    RESTRequest1.ClearBody;
    RESTRequest1.AddBody(JSONObject.ToString, ctAPPLICATION_JSON);

    // Exécuter
    RESTRequest1.Execute;

    // Vérifier le résultat
    if RESTResponse1.StatusCode = 201 then
      ShowMessage('Utilisateur créé avec succès!')
    else
      ShowMessage('Erreur: ' + RESTResponse1.StatusText);

  finally
    JSONObject.Free;
  end;
end;
```

**Nouveaux concepts :**

- `AddPair` : Ajoute des paires clé-valeur au JSON
- `ClearBody` : Nettoie le corps de la requête précédente
- `AddBody` : Ajoute le contenu à envoyer
- `StatusCode` : Le code de statut HTTP (201 = création réussie)

## Gérer les paramètres d'URL

### Paramètres de requête (Query Parameters)

Les paramètres de requête s'ajoutent à l'URL après un `?` :

```pascal
procedure TForm1.ButtonSearchClick(Sender: TObject);  
begin  
  RESTRequest1.Method := rmGET;
  RESTRequest1.Resource := 'posts';

  // Effacer les paramètres précédents
  RESTRequest1.Params.Clear;

  // Ajouter des paramètres
  RESTRequest1.AddParameter('userId', '1', pkGETorPOST);
  RESTRequest1.AddParameter('_limit', '5', pkGETorPOST);

  RESTRequest1.Execute;

  Memo1.Text := RESTResponse1.Content;
end;
```

Cela générera l'URL : `posts?userId=1&_limit=5`

### Paramètres dans le chemin

Vous pouvez aussi inclure des paramètres directement dans le chemin :

```pascal
procedure TForm1.ButtonGetUserClick(Sender: TObject);  
var  
  UserID: string;
begin
  UserID := Edit1.Text;

  RESTRequest1.Method := rmGET;
  RESTRequest1.Resource := 'users/{id}';

  // Remplacer {id} par la valeur
  RESTRequest1.Params.Clear;
  RESTRequest1.AddParameter('id', UserID, pkURLSEGMENT);

  RESTRequest1.Execute;
end;
```

## Gérer les en-têtes HTTP

### Ajouter des en-têtes personnalisés

Certaines API nécessitent des en-têtes spécifiques, comme une clé d'authentification :

```pascal
procedure TForm1.ConfigurerEnTetes;  
begin  
  // Ajouter un en-tête d'authentification
  RESTRequest1.Params.AddHeader('Authorization', 'Bearer votre-token-ici');

  // Ajouter un en-tête personnalisé
  RESTRequest1.Params.AddHeader('X-Custom-Header', 'valeur');

  // Définir le type de contenu
  RESTRequest1.Params.AddHeader('Content-Type', 'application/json');
end;
```

**Types d'en-têtes courants :**

- `Authorization` : Pour l'authentification
- `Content-Type` : Type de données envoyées
- `Accept` : Type de données acceptées en retour
- `User-Agent` : Identifie votre application

## Gestion des erreurs

### Vérifier les codes de statut

Il est essentiel de vérifier si la requête s'est bien déroulée :

```pascal
procedure TForm1.ButtonAppelSecuriseClick(Sender: TObject);  
begin  
  try
    RESTRequest1.Execute;

    // Vérifier le code de statut
    case RESTResponse1.StatusCode of
      200: ShowMessage('Succès !');
      201: ShowMessage('Ressource créée !');
      400: ShowMessage('Requête invalide');
      401: ShowMessage('Non autorisé - vérifiez vos identifiants');
      404: ShowMessage('Ressource non trouvée');
      500: ShowMessage('Erreur serveur');
    else
      ShowMessage('Code de statut: ' + IntToStr(RESTResponse1.StatusCode));
    end;

  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  end;
end;
```

**Codes HTTP importants :**

- **2xx** : Succès (200 OK, 201 Created)
- **3xx** : Redirection
- **4xx** : Erreur client (400 Bad Request, 404 Not Found)
- **5xx** : Erreur serveur (500 Internal Server Error)

## Authentification

### Authentification basique

Certaines API utilisent l'authentification HTTP basique :

```pascal
procedure TForm1.ConfigurerAuthBasique;  
begin  
  RESTClient1.Authenticator := THTTPBasicAuthenticator.Create(
    'nom_utilisateur',
    'mot_de_passe'
  );
end;
```

### Authentification par token Bearer

Plus courant dans les API modernes :

```pascal
procedure TForm1.ConfigurerAuthToken;  
var  
  OAuth2: TOAuth2Authenticator;
begin
  OAuth2 := TOAuth2Authenticator.Create(nil);
  OAuth2.AccessToken := 'votre_token_jwt_ici';
  RESTClient1.Authenticator := OAuth2;
end;
```

## Timeout et performances

### Configurer le timeout

Pour éviter que votre application ne se bloque indéfiniment :

```pascal
procedure TForm1.ConfigurerTimeout;  
begin  
  // Timeout de connexion en millisecondes (5 secondes)
  RESTClient1.ConnectTimeout := 5000;

  // Timeout de lecture (10 secondes)
  RESTClient1.ReadTimeout := 10000;
end;
```

## Exemple complet : Application météo

Voici un exemple complet d'utilisation d'une API REST pour récupérer la météo :

```pascal
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    ButtonGetWeather: TButton;
    EditCity: TEdit;
    MemoResult: TMemo;
    LabelCity: TLabel;
    procedure ButtonGetWeatherClick(Sender: TObject);
  private
    procedure AfficherMeteo(const JSONResponse: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonGetWeatherClick(Sender: TObject);  
var  
  Ville: string;
begin
  Ville := EditCity.Text;

  if Ville.IsEmpty then
  begin
    ShowMessage('Veuillez entrer un nom de ville');
    Exit;
  end;

  try
    // Configuration de la requête
    RESTClient1.BaseURL := 'https://api.openweathermap.org/data/2.5';
    RESTRequest1.Method := rmGET;
    RESTRequest1.Resource := 'weather';

    // Paramètres
    RESTRequest1.Params.Clear;
    RESTRequest1.AddParameter('q', Ville, pkGETorPOST);
    RESTRequest1.AddParameter('appid', 'VOTRE_CLE_API', pkGETorPOST);
    RESTRequest1.AddParameter('units', 'metric', pkGETorPOST);
    RESTRequest1.AddParameter('lang', 'fr', pkGETorPOST);

    // Exécution
    RESTRequest1.Execute;

    // Traitement de la réponse
    if RESTResponse1.StatusCode = 200 then
      AfficherMeteo(RESTResponse1.Content)
    else
      ShowMessage('Erreur: ' + RESTResponse1.StatusText);

  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  end;
end;

procedure TForm1.AfficherMeteo(const JSONResponse: string);  
var  
  JSONValue: TJSONValue;
  JSONObject, MainObject: TJSONObject;
  Temperature, Ressenti: Double;
  Description, VilleNom: string;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONResponse);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire les informations
      VilleNom := JSONObject.GetValue<string>('name');

      MainObject := JSONObject.GetValue<TJSONObject>('main');
      Temperature := MainObject.GetValue<Double>('temp');
      Ressenti := MainObject.GetValue<Double>('feels_like');

      // Afficher dans le Memo
      MemoResult.Lines.Clear;
      MemoResult.Lines.Add('Météo pour : ' + VilleNom);
      MemoResult.Lines.Add('');
      MemoResult.Lines.Add('Température : ' + FormatFloat('0.0', Temperature) + '°C');
      MemoResult.Lines.Add('Ressenti : ' + FormatFloat('0.0', Ressenti) + '°C');
    end;
  finally
    JSONValue.Free;
  end;
end;

end.
```

## Bonnes pratiques

### 1. Gestion de la mémoire

Toujours libérer les objets JSON créés :

```pascal
JSONObject := TJSONObject.Create;  
try  
  // Utilisation
finally
  JSONObject.Free;
end;
```

### 2. Ne jamais exposer les clés API

Ne mettez jamais vos clés API directement dans le code. Utilisez plutôt :

- Des fichiers de configuration
- Des variables d'environnement
- Un système de gestion des secrets

### 3. Gérer les timeouts

Toujours configurer des timeouts pour éviter les blocages :

```pascal
RESTClient1.ConnectTimeout := 5000;  
RESTClient1.ReadTimeout := 10000;  
```

### 4. Logger les erreurs

Conservez une trace des erreurs pour faciliter le débogage :

```pascal
try
  RESTRequest1.Execute;
except
  on E: Exception do
  begin
    // Logger l'erreur
    MemoLog.Lines.Add(DateTimeToStr(Now) + ' - ' + E.Message);
    raise;
  end;
end;
```

### 5. Utiliser HTTPS

Privilégiez toujours les connexions sécurisées (HTTPS) pour protéger les données :

```pascal
RESTClient1.BaseURL := 'https://api.example.com';  // HTTPS, pas HTTP
```

## Débogage

### Inspecter les requêtes et réponses

Pour comprendre ce qui se passe, affichez les détails :

```pascal
procedure TForm1.DeboguerRequete;  
begin  
  RESTRequest1.Execute;

  Memo1.Lines.Add('=== REQUÊTE ===');
  Memo1.Lines.Add('URL: ' + RESTRequest1.GetFullRequestURL);
  Memo1.Lines.Add('Méthode: ' + RESTRequest1.Method.ToString);
  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== RÉPONSE ===');
  Memo1.Lines.Add('Code: ' + IntToStr(RESTResponse1.StatusCode));
  Memo1.Lines.Add('Texte: ' + RESTResponse1.StatusText);
  Memo1.Lines.Add('Contenu: ' + RESTResponse1.Content);
end;
```

## Ressources complémentaires

### API publiques pour s'entraîner

- **JSONPlaceholder** (https://jsonplaceholder.typicode.com) : API de test gratuite
- **OpenWeatherMap** (https://openweathermap.org/api) : Données météo
- **REST Countries** (https://restcountries.com) : Informations sur les pays
- **The Cat API** (https://thecatapi.com) : Images de chats aléatoires

### Documentation Delphi

Consultez la documentation officielle Embarcadero pour plus de détails sur les composants REST.

## Conclusion

Les composants REST de Delphi (`TRESTClient`, `TRESTRequest`, `TRESTResponse`) offrent une solution puissante et intuitive pour communiquer avec des API Web. Avec ces bases, vous pouvez maintenant :

- Récupérer des données depuis des API
- Envoyer des informations
- Gérer l'authentification
- Traiter les réponses JSON
- Gérer les erreurs correctement

Les API REST sont au cœur de nombreuses applications modernes, et Delphi facilite grandement leur intégration dans vos projets.

⏭️ [Manipulation de JSON et XML](/10-communication-et-services-reseaux/02-manipulation-de-json-et-xml.md)
