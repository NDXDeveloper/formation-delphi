# 14.7 Liaison avec des API REST tierces

## Introduction

Les API REST (Representational State Transfer) sont aujourd'hui la méthode la plus répandue pour permettre à différentes applications de communiquer entre elles via Internet. Ces API permettent à votre application Delphi d'interagir avec une multitude de services en ligne : réseaux sociaux, services météo, systèmes de paiement, bases de données, services cloud, et bien plus encore.

Dans ce chapitre, nous allons découvrir comment intégrer des API REST tierces dans vos applications Delphi. Nous commencerons par les concepts de base, puis nous verrons plusieurs exemples concrets qui vous permettront de maîtriser cette technique essentielle pour le développement d'applications modernes.

## Qu'est-ce qu'une API REST ?

Une API REST est une interface qui permet à différentes applications de communiquer selon un ensemble de règles prédéfinies. Les principales caractéristiques d'une API REST sont :

1. **Sans état (Stateless)** : Chaque requête contient toutes les informations nécessaires, le serveur ne conserve pas d'information sur l'état du client entre les requêtes.
2. **Architecture client-serveur** : Séparation claire entre le client (votre application) et le serveur (l'API).
3. **Utilisation des méthodes HTTP** : GET (lecture), POST (création), PUT (mise à jour), DELETE (suppression).
4. **Format des données** : Généralement JSON ou XML pour l'échange d'informations.
5. **Ressources accessibles via URLs** : Chaque ressource est identifiée par une URL unique.

## Les composants REST de Delphi

Delphi propose plusieurs composants pour interagir avec les API REST. Les plus utilisés sont ceux de la bibliothèque REST Client, disponible depuis Delphi XE5 :

- **TRESTClient** : Gère la connexion avec le serveur REST
- **TRESTRequest** : Représente une requête spécifique
- **TRESTResponse** : Contient la réponse du serveur
- **TRESTResponseDataSetAdapter** : Permet de lier la réponse à un DataSet

## Premier exemple : Utilisation d'une API météo

Commençons par un exemple simple : récupérer les informations météorologiques d'une ville en utilisant l'API OpenWeatherMap.

### Étape 1 : Obtenir une clé API

La plupart des API REST nécessitent une clé API pour l'authentification. Pour OpenWeatherMap :

1. Créez un compte gratuit sur [OpenWeatherMap](https://openweathermap.org/api)
2. Obtenez votre clé API (gratuite pour un usage limité)

### Étape 2 : Configurez votre interface

Créez un nouveau projet VCL et ajoutez les composants suivants à votre formulaire :

- Un TEdit (nommé `EditCity`) pour saisir le nom de la ville
- Un TButton (nommé `ButtonGetWeather`) pour déclencher la requête
- Un TMemo (nommé `MemoResult`) pour afficher les résultats
- Un TLabel (nommé `LabelStatus`) pour afficher l'état de la requête

Dans la section uses, ajoutez :

```pascal
uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, System.JSON;
```

### Étape 3 : Implémentez la requête API

Ajoutez ce code au clic du bouton :

```pascal
procedure TForm1.ButtonGetWeatherClick(Sender: TObject);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
  City: string;
  ApiKey: string;
  Temperature, FeelsLike: Double;
  WeatherDescription, CountryCode: string;
  Humidity: Integer;
begin
  // Paramètres
  City := EditCity.Text;
  if City = '' then
  begin
    ShowMessage('Veuillez entrer le nom d''une ville');
    Exit;
  end;

  // Votre clé API OpenWeatherMap
  ApiKey := 'VOTRE_CLE_API_ICI';

  // Afficher l'état
  LabelStatus.Caption := 'Récupération des données...';
  Application.ProcessMessages;

  // Création des composants REST
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration du client
    RESTClient.BaseURL := 'https://api.openweathermap.org/data/2.5/weather';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;

    // Paramètres de la requête
    RESTRequest.Params.Clear;
    RESTRequest.Params.AddItem('q', City, pkGETorPOST);
    RESTRequest.Params.AddItem('appid', ApiKey, pkGETorPOST);
    RESTRequest.Params.AddItem('units', 'metric', pkGETorPOST); // Pour avoir les températures en Celsius
    RESTRequest.Params.AddItem('lang', 'fr', pkGETorPOST);      // Pour avoir les descriptions en français

    // Exécution de la requête
    try
      RESTRequest.Execute;

      // Vérification du code de réponse HTTP
      if RESTResponse.StatusCode = 200 then
      begin
        // Analyse de la réponse JSON
        JsonObj := RESTResponse.JSONValue as TJSONObject;

        // Extraction des données
        Temperature := JsonObj.GetValue<TJSONObject>('main').GetValue<Double>('temp');
        FeelsLike := JsonObj.GetValue<TJSONObject>('main').GetValue<Double>('feels_like');
        Humidity := JsonObj.GetValue<TJSONObject>('main').GetValue<Integer>('humidity');
        WeatherDescription := JsonObj.GetValue<TJSONArray>('weather').Items[0].GetValue<string>('description');
        CountryCode := JsonObj.GetValue<TJSONObject>('sys').GetValue<string>('country');

        // Affichage des résultats
        MemoResult.Lines.Clear;
        MemoResult.Lines.Add('Météo à ' + City + ', ' + CountryCode);
        MemoResult.Lines.Add('----------------------------');
        MemoResult.Lines.Add('Température: ' + FormatFloat('0.0', Temperature) + '°C');
        MemoResult.Lines.Add('Ressenti: ' + FormatFloat('0.0', FeelsLike) + '°C');
        MemoResult.Lines.Add('Humidité: ' + IntToStr(Humidity) + '%');
        MemoResult.Lines.Add('Conditions: ' + WeatherDescription);

        LabelStatus.Caption := 'Données récupérées avec succès';
      end
      else
      begin
        // Gestion des erreurs
        MemoResult.Lines.Clear;
        MemoResult.Lines.Add('Erreur: ' + IntToStr(RESTResponse.StatusCode));
        MemoResult.Lines.Add(RESTResponse.StatusText);

        if RESTResponse.StatusCode = 404 then
          MemoResult.Lines.Add('Ville non trouvée')
        else if RESTResponse.StatusCode = 401 then
          MemoResult.Lines.Add('Clé API invalide');

        LabelStatus.Caption := 'Erreur lors de la récupération des données';
      end;
    except
      on E: Exception do
      begin
        MemoResult.Lines.Clear;
        MemoResult.Lines.Add('Exception: ' + E.Message);
        LabelStatus.Caption := 'Erreur: ' + E.Message;
      end;
    end;
  finally
    // Libération des ressources
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

> **Important** : Remplacez `'VOTRE_CLE_API_ICI'` par votre clé API obtenue sur OpenWeatherMap.

### Étape 4 : Testez votre application

Compilez et exécutez votre application. Entrez le nom d'une ville (par exemple "Paris") et cliquez sur le bouton. Vous devriez voir les informations météorologiques s'afficher dans le mémo.

## Utilisation des composants non-visuels

Pour une approche plus organisée, vous pouvez utiliser les composants non-visuels de la palette REST Client :

1. Ajoutez ces composants à votre formulaire :
   - Un TRESTClient (nommé `RESTClient1`)
   - Un TRESTRequest (nommé `RESTRequest1`)
   - Un TRESTResponse (nommé `RESTResponse1`)

2. Configurez les propriétés dans l'Object Inspector :
   - RESTClient1.BaseURL : `https://api.openweathermap.org/data/2.5/weather`
   - RESTRequest1.Client : `RESTClient1`
   - RESTRequest1.Response : `RESTResponse1`
   - RESTRequest1.Method : `rmGET`

3. Ajoutez les paramètres dans RESTRequest1.Params :
   - Nom: `q`, Valeur: laissez vide (sera défini dans le code), Type: `pkGETorPOST`
   - Nom: `appid`, Valeur: votre clé API, Type: `pkGETorPOST`
   - Nom: `units`, Valeur: `metric`, Type: `pkGETorPOST`
   - Nom: `lang`, Valeur: `fr`, Type: `pkGETorPOST`

4. Modifiez le code du bouton :

```pascal
procedure TForm1.ButtonGetWeatherClick(Sender: TObject);
var
  JsonObj: TJSONObject;
  City: string;
  Temperature, FeelsLike: Double;
  WeatherDescription, CountryCode: string;
  Humidity: Integer;
begin
  // Paramètres
  City := EditCity.Text;
  if City = '' then
  begin
    ShowMessage('Veuillez entrer le nom d''une ville');
    Exit;
  end;

  // Mise à jour du paramètre ville
  RESTRequest1.Params.ParameterByName('q').Value := City;

  // Afficher l'état
  LabelStatus.Caption := 'Récupération des données...';
  Application.ProcessMessages;

  // Exécution de la requête
  try
    RESTRequest1.Execute;

    // Vérification du code de réponse HTTP
    if RESTResponse1.StatusCode = 200 then
    begin
      // Analyse de la réponse JSON
      JsonObj := RESTResponse1.JSONValue as TJSONObject;

      // Extraction et affichage des données (comme dans l'exemple précédent)
      // ...

      LabelStatus.Caption := 'Données récupérées avec succès';
    end
    else
    begin
      // Gestion des erreurs (comme dans l'exemple précédent)
      // ...
    end;
  except
    on E: Exception do
    begin
      MemoResult.Lines.Clear;
      MemoResult.Lines.Add('Exception: ' + E.Message);
      LabelStatus.Caption := 'Erreur: ' + E.Message;
    end;
  end;
end;
```

Cette approche rend le code plus lisible et simplifie la configuration de l'API.

## Authentification aux API REST

Les API REST utilisent différentes méthodes d'authentification. Voici les plus courantes et comment les implémenter dans Delphi :

### 1. Clé API dans l'URL ou les paramètres

C'est la méthode la plus simple, comme celle utilisée dans notre exemple OpenWeatherMap.

```pascal
RESTRequest.Params.AddItem('api_key', 'VOTRE_CLE_API', pkGETorPOST);
```

### 2. Authentification Basic

L'authentification HTTP Basic envoie un nom d'utilisateur et un mot de passe encodés en Base64 dans l'en-tête de la requête.

```pascal
RESTClient.Authenticator := THTTPBasicAuthenticator.Create('username', 'password');
```

### 3. Authentification avec jeton (Bearer Token)

De nombreuses API modernes utilisent des jetons JWT (JSON Web Tokens) ou OAuth.

```pascal
// Ajout d'un en-tête d'autorisation
RESTRequest.Params.AddHeader('Authorization', 'Bearer ' + YourAccessToken);
```

### 4. OAuth 2.0

Pour OAuth 2.0, le processus est plus complexe et implique généralement plusieurs étapes :

1. Redirection de l'utilisateur vers une page d'authentification
2. Récupération d'un code d'autorisation
3. Échange du code contre un jeton d'accès
4. Utilisation du jeton pour les requêtes API

Voici un exemple simplifié d'obtention d'un jeton OAuth 2.0 :

```pascal
procedure TForm1.GetOAuthToken;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
  TokenEndpoint: string;
  ClientID, ClientSecret, Code, RedirectURI: string;
begin
  // Paramètres OAuth (à obtenir auprès du fournisseur de l'API)
  TokenEndpoint := 'https://api.exemple.com/oauth/token';
  ClientID := 'votre_client_id';
  ClientSecret := 'votre_client_secret';
  Code := 'code_autorisation'; // Obtenu après la redirection de l'utilisateur
  RedirectURI := 'http://localhost/callback';

  // Création des composants REST
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration
    RESTClient.BaseURL := TokenEndpoint;
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Paramètres de la requête
    RESTRequest.Params.Clear;
    RESTRequest.Params.AddItem('grant_type', 'authorization_code', pkFORMDATA);
    RESTRequest.Params.AddItem('client_id', ClientID, pkFORMDATA);
    RESTRequest.Params.AddItem('client_secret', ClientSecret, pkFORMDATA);
    RESTRequest.Params.AddItem('code', Code, pkFORMDATA);
    RESTRequest.Params.AddItem('redirect_uri', RedirectURI, pkFORMDATA);

    // Exécution
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JsonObj := RESTResponse.JSONValue as TJSONObject;

      // Extraction du jeton
      FAccessToken := JsonObj.GetValue<string>('access_token');
      FTokenType := JsonObj.GetValue<string>('token_type');
      FExpiresIn := JsonObj.GetValue<Integer>('expires_in');

      ShowMessage('Jeton obtenu avec succès');
    end
    else
    begin
      ShowMessage('Erreur lors de l''obtention du jeton: ' +
                  IntToStr(RESTResponse.StatusCode) + ' ' +
                  RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

## Traitement des réponses JSON

La plupart des API REST renvoient des données au format JSON. Delphi fournit plusieurs classes pour manipuler facilement ce format :

### Utilisation de TJSONObject et TJSONArray

Le moyen le plus direct est d'utiliser les classes `TJSONObject` et `TJSONArray` :

```pascal
var
  JsonObj: TJSONObject;
  JsonArr: TJSONArray;
  i: Integer;
begin
  // Supposons que RESTResponse contient une réponse JSON
  JsonObj := RESTResponse.JSONValue as TJSONObject;

  // Accéder à une propriété simple
  ShowMessage('Nom: ' + JsonObj.GetValue<string>('name'));

  // Accéder à une propriété imbriquée
  ShowMessage('Ville: ' + JsonObj.GetValue<TJSONObject>('address').GetValue<string>('city'));

  // Parcourir un tableau JSON
  JsonArr := JsonObj.GetValue<TJSONArray>('items');
  for i := 0 to JsonArr.Count - 1 do
  begin
    Memo1.Lines.Add(JsonArr.Items[i].GetValue<string>('title'));
  end;
end;
```

### Utilisation de JSONToObject

Vous pouvez aussi convertir directement un JSON en objet Delphi :

```pascal
type
  TAddress = class
    street: string;
    city: string;
    zipcode: string;
  end;

  TPerson = class
    name: string;
    age: Integer;
    address: TAddress;
  end;

var
  Person: TPerson;
begin
  // Conversion du JSON en objet
  Person := TJson.JsonToObject<TPerson>(RESTResponse.Content);
  try
    ShowMessage('Nom: ' + Person.name);
    ShowMessage('Ville: ' + Person.address.city);
  finally
    Person.Free;
  end;
end;
```

Pour utiliser cette fonctionnalité, ajoutez `REST.Json` à la clause `uses`.

## Gestion des erreurs et des cas particuliers

### Codes de statut HTTP

Les API REST utilisent des codes de statut HTTP pour indiquer le résultat d'une requête :

- **2xx** : Succès (200 OK, 201 Created, etc.)
- **4xx** : Erreur client (400 Bad Request, 401 Unauthorized, 404 Not Found, etc.)
- **5xx** : Erreur serveur (500 Internal Server Error, 503 Service Unavailable, etc.)

```pascal
procedure TForm1.HandleAPIResponse;
begin
  case RESTResponse.StatusCode of
    200..299:
      ProcessSuccessResponse;

    400:
      ShowMessage('Requête invalide. Vérifiez les paramètres.');

    401:
      begin
        ShowMessage('Authentification requise.');
        // Redirection vers l'écran de connexion
        RefreshToken;
      end;

    404:
      ShowMessage('Ressource non trouvée.');

    429:
      begin
        ShowMessage('Trop de requêtes. Veuillez réessayer plus tard.');
        // Implémentation d'un délai avant nouvelle tentative
      end;

    500..599:
      ShowMessage('Erreur serveur. Veuillez réessayer plus tard.');

    else
      ShowMessage('Erreur inconnue: ' + IntToStr(RESTResponse.StatusCode));
  end;
end;
```

### Gestion des délais d'attente

Les requêtes réseau peuvent parfois prendre du temps ou échouer. Configurez des délais d'attente appropriés :

```pascal
RESTClient.ConnectTimeout := 5000;  // 5 secondes pour la connexion
RESTClient.ReadTimeout := 10000;    // 10 secondes pour la lecture
```

### Mise en œuvre des nouvelles tentatives

Pour les requêtes importantes, il est courant d'implémenter un mécanisme de nouvelle tentative :

```pascal
function TForm1.ExecuteWithRetry(Request: TRESTRequest; MaxRetries: Integer = 3): Boolean;
var
  RetryCount: Integer;
  WaitTime: Integer;
begin
  Result := False;
  RetryCount := 0;
  WaitTime := 1000; // Délai initial d'une seconde

  while (RetryCount < MaxRetries) and (not Result) do
  begin
    try
      Request.Execute;

      // Considérer comme réussi si le code est 2xx
      Result := (Request.Response.StatusCode >= 200) and
                (Request.Response.StatusCode < 300);

      // Si c'est une erreur 429 (trop de requêtes) ou une erreur serveur, réessayer
      if (not Result) and
         ((Request.Response.StatusCode = 429) or
          (Request.Response.StatusCode >= 500)) then
      begin
        Inc(RetryCount);
        if RetryCount < MaxRetries then
        begin
          // Attendre avec backoff exponentiel (1s, 2s, 4s, etc.)
          Sleep(WaitTime);
          WaitTime := WaitTime * 2;
        end;
      end
      else
      begin
        // Autres erreurs, ne pas réessayer
        Break;
      end;
    except
      on E: Exception do
      begin
        // Exception lors de la requête (problème réseau, etc.)
        Inc(RetryCount);
        if RetryCount < MaxRetries then
        begin
          Sleep(WaitTime);
          WaitTime := WaitTime * 2;
        end;
      end;
    end;
  end;
end;
```

## Requêtes asynchrones

Pour maintenir la réactivité de votre interface utilisateur, il est préférable d'effectuer les requêtes API en arrière-plan :

```pascal
procedure TForm1.ButtonAsyncGetWeatherClick(Sender: TObject);
begin
  // Désactiver le bouton pendant le chargement
  ButtonAsyncGetWeather.Enabled := False;
  LabelStatus.Caption := 'Récupération des données...';

  // Démarrer une tâche en arrière-plan
  TTask.Run(
    procedure
    var
      RESTClient: TRESTClient;
      RESTRequest: TRESTRequest;
      RESTResponse: TRESTResponse;
      JsonObj: TJSONObject;
      City, WeatherDesc, CountryCode: string;
      Temp: Double;
    begin
      RESTClient := TRESTClient.Create(nil);
      RESTRequest := TRESTRequest.Create(nil);
      RESTResponse := TRESTResponse.Create(nil);

      try
        // Configuration REST (comme dans les exemples précédents)
        RESTClient.BaseURL := 'https://api.openweathermap.org/data/2.5/weather';
        RESTRequest.Client := RESTClient;
        RESTRequest.Response := RESTResponse;
        RESTRequest.Method := rmGET;

        // Récupérer le nom de la ville depuis le thread principal
        TThread.Synchronize(TThread.Current,
          procedure
          begin
            City := EditCity.Text;
          end
        );

        // Paramètres de la requête
        RESTRequest.Params.Clear;
        RESTRequest.Params.AddItem('q', City, pkGETorPOST);
        RESTRequest.Params.AddItem('appid', 'VOTRE_CLE_API_ICI', pkGETorPOST);
        RESTRequest.Params.AddItem('units', 'metric', pkGETorPOST);

        // Exécution de la requête
        RESTRequest.Execute;

        // Traitement de la réponse
        if RESTResponse.StatusCode = 200 then
        begin
          JsonObj := RESTResponse.JSONValue as TJSONObject;

          // Extraction des données
          Temp := JsonObj.GetValue<TJSONObject>('main').GetValue<Double>('temp');
          WeatherDesc := JsonObj.GetValue<TJSONArray>('weather').Items[0].GetValue<string>('description');
          CountryCode := JsonObj.GetValue<TJSONObject>('sys').GetValue<string>('country');

          // Mise à jour de l'interface utilisateur dans le thread principal
          TThread.Synchronize(TThread.Current,
            procedure
            begin
              MemoResult.Lines.Clear;
              MemoResult.Lines.Add('Météo à ' + City + ', ' + CountryCode);
              MemoResult.Lines.Add('----------------------------');
              MemoResult.Lines.Add('Température: ' + FormatFloat('0.0', Temp) + '°C');
              MemoResult.Lines.Add('Conditions: ' + WeatherDesc);

              LabelStatus.Caption := 'Données récupérées avec succès';
              ButtonAsyncGetWeather.Enabled := True;
            end
          );
        end
        else
        begin
          // Gestion des erreurs
          TThread.Synchronize(TThread.Current,
            procedure
            begin
              MemoResult.Lines.Clear;
              MemoResult.Lines.Add('Erreur: ' + IntToStr(RESTResponse.StatusCode));
              MemoResult.Lines.Add(RESTResponse.StatusText);

              LabelStatus.Caption := 'Erreur lors de la récupération des données';
              ButtonAsyncGetWeather.Enabled := True;
            end
          );
        end;
      finally
        RESTClient.Free;
        RESTRequest.Free;
        RESTResponse.Free;
      end;
    end
  );
end;
```

Pour utiliser cette fonctionnalité, ajoutez `System.Threading` à la clause `uses`.

## Exemple pratique : Intégration avec l'API GitHub

Voici un exemple plus complet qui montre comment récupérer la liste des dépôts d'un utilisateur GitHub :

```pascal
procedure TForm1.ButtonGetRepositoriesClick(Sender: TObject);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonArray: TJSONArray;
  JsonValue: TJSONValue;
  Username: string;
  i: Integer;
begin
  Username := EditUsername.Text;
  if Username = '' then
  begin
    ShowMessage('Veuillez entrer un nom d''utilisateur GitHub');
    Exit;
  end;

  LabelStatus.Caption := 'Récupération des dépôts...';
  ListBoxRepositories.Clear;
  Application.ProcessMessages;

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration
    RESTClient.BaseURL := 'https://api.github.com';
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Resource := 'users/{username}/repos';

    // Paramètres
    RESTRequest.Params.Clear;
    RESTRequest.AddParameter('username', Username, pkURLSEGMENT);
    RESTRequest.AddParameter('sort', 'updated', pkGETorPOST);
    RESTRequest.AddParameter('per_page', '100', pkGETorPOST);

    // Ajouter un en-tête User-Agent (requis par l'API GitHub)
    RESTRequest.Params.AddHeader('User-Agent', 'Delphi-App');

    // Exécution
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // La réponse est un tableau JSON
      JsonArray := RESTResponse.JSONValue as TJSONArray;

      for i := 0 to JsonArray.Count - 1 do
      begin
        JsonValue := JsonArray.Items[i];

        // Ajouter chaque dépôt à la liste
        ListBoxRepositories.Items.Add(
          JsonValue.GetValue<string>('name') + ' - ' +
          JsonValue.GetValue<string>('description')
        );

        // Stocker l'URL du dépôt dans les données de l'item
        ListBoxRepositories.Items.Objects[ListBoxRepositories.Items.Count - 1] :=
          TObject(StrNew(PChar(JsonValue.GetValue<string>('html_url'))));
      end;

      LabelStatus.Caption := Format('%d dépôts trouvés pour %s',
                                   [JsonArray.Count, Username]);
    end
    else
    begin
      if RESTResponse.StatusCode = 404 then
        ShowMessage('Utilisateur non trouvé')
      else
        ShowMessage('Erreur: ' + IntToStr(RESTResponse.StatusCode) + ' ' +
                    RESTResponse.StatusText);

      LabelStatus.Caption := 'Erreur lors de la récupération des dépôts';
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

procedure TForm1.ListBoxRepositoriesClick(Sender: TObject);
var
  URL: string;
begin
  if ListBoxRepositories.ItemIndex >= 0 then
  begin
    // Récupérer l'URL stockée dans les données de l'item
    URL := PChar(ListBoxRepositories.Items.Objects[ListBoxRepositories.ItemIndex]);

    // Ouvrir l'URL dans le navigateur par défaut
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  // Libérer la mémoire allouée pour les URLs
  for i := 0 to ListBoxRepositories.Items.Count - 1 do
  begin
    if ListBoxRepositories.Items.Objects[i] <> nil then
      StrDispose(PChar(ListBoxRepositories.Items.Objects[i]));
  end;
end;
```

## Meilleures pratiques pour l'intégration des API REST

### 1. Sécurité

- Ne stockez jamais les clés API ou identifiants dans votre code source
- Utilisez HTTPS pour toutes les communications API
- Implémentez un stockage sécurisé pour les jetons d'accès et les informations d'identification
- Utilisez un système de chiffrement pour protéger les données sensibles

Voici un exemple de stockage sécurisé des clés API :

```pascal
// Dans une unité séparée (SecurityUtils.pas)
unit SecurityUtils;

interface

function GetApiKey(const ServiceName: string): string;
procedure SaveApiKey(const ServiceName, ApiKey: string);

implementation

uses
  System.SysUtils, System.IOUtils, System.Hash;

const
  ENCRYPTION_KEY = 'YourSecretEncryptionKey'; // À remplacer par une clé sécurisée

// Fonction simplifiée de chiffrement/déchiffrement
// Dans une application réelle, utilisez une bibliothèque de cryptographie robuste
function SimpleEncrypt(const Text, Key: string): string;
var
  i, KeyLen: Integer;
begin
  Result := '';
  KeyLen := Length(Key);
  for i := 1 to Length(Text) do
    Result := Result + Char(Ord(Text[i]) xor Ord(Key[((i-1) mod KeyLen) + 1]));

  // Encoder en Base64 pour un stockage plus facile
  Result := TNetEncoding.Base64.Encode(Result);
end;

function SimpleDecrypt(const Text, Key: string): string;
var
  Decoded: string;
  i, KeyLen: Integer;
begin
  // Décoder le Base64
  try
    Decoded := TNetEncoding.Base64.Decode(Text);
  except
    Result := '';
    Exit;
  end;

  Result := '';
  KeyLen := Length(Key);
  for i := 1 to Length(Decoded) do
    Result := Result + Char(Ord(Decoded[i]) xor Ord(Key[((i-1) mod KeyLen) + 1]));
end;

function GetConfigFilePath: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'AppConfig.dat');
end;

function GetApiKey(const ServiceName: string): string;
var
  Lines: TStringList;
  i: Integer;
  Line, Service, Key: string;
begin
  Result := '';

  if not FileExists(GetConfigFilePath) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(GetConfigFilePath);

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      if Pos('=', Line) > 0 then
      begin
        Service := Copy(Line, 1, Pos('=', Line) - 1);
        Key := Copy(Line, Pos('=', Line) + 1, Length(Line));

        if SameText(Service, ServiceName) then
        begin
          Result := SimpleDecrypt(Key, ENCRYPTION_KEY);
          Break;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure SaveApiKey(const ServiceName, ApiKey: string);
var
  Lines: TStringList;
  EncryptedKey: string;
  i, Index: Integer;
  Line, Service: string;
  Found: Boolean;
begin
  EncryptedKey := SimpleEncrypt(ApiKey, ENCRYPTION_KEY);
  Found := False;

  Lines := TStringList.Create;
  try
    if FileExists(GetConfigFilePath) then
      Lines.LoadFromFile(GetConfigFilePath);

    // Chercher si le service existe déjà
    Index := -1;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      if Pos('=', Line) > 0 then
      begin
        Service := Copy(Line, 1, Pos('=', Line) - 1);

        if SameText(Service, ServiceName) then
        begin
          Index := i;
          Found := True;
          Break;
        end;
      end;
    end;

    // Mettre à jour ou ajouter
    if Found then
      Lines[Index] := ServiceName + '=' + EncryptedKey
    else
      Lines.Add(ServiceName + '=' + EncryptedKey);

    Lines.SaveToFile(GetConfigFilePath);
  finally
    Lines.Free;
  end;
end;

end.
```

Utilisation dans votre application :

```pascal
uses
  SecurityUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
  ApiKey: string;
begin
  // Récupérer la clé API stockée
  ApiKey := GetApiKey('OpenWeatherMap');

  // Si pas de clé stockée, demander à l'utilisateur
  if ApiKey = '' then
  begin
    ApiKey := InputBox('Configuration', 'Veuillez entrer votre clé API OpenWeatherMap:', '');
    if ApiKey <> '' then
      SaveApiKey('OpenWeatherMap', ApiKey);
  end;

  // Utiliser la clé API
  FApiKey := ApiKey;
end;
```

### 2. Organisation du code

- Créez une classe d'encapsulation pour chaque API
- Séparez la logique d'appel API de l'interface utilisateur
- Utilisez un modèle de conception comme le Repository Pattern

Exemple de classe d'encapsulation pour l'API météo :

```pascal
// Dans une unité WeatherAPI.pas
unit WeatherAPI;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, System.JSON;

type
  TWeatherInfo = record
    City, CountryCode: string;
    Temperature, FeelsLike: Double;
    Humidity: Integer;
    Description: string;
  end;

  TWeatherAPI = class
  private
    FApiKey: string;
    FBaseURL: string;
    FLastError: string;

    function ExecuteRequest(const Resource: string; const Params: array of const): TJSONValue;
  public
    constructor Create(const ApiKey: string);
    destructor Destroy; override;

    function GetCurrentWeather(const City: string; out WeatherInfo: TWeatherInfo): Boolean;
    function GetForecast(const City: string; DaysCount: Integer): TJSONArray;

    property LastError: string read FLastError;
  end;

implementation

constructor TWeatherAPI.Create(const ApiKey: string);
begin
  inherited Create;
  FApiKey := ApiKey;
  FBaseURL := 'https://api.openweathermap.org/data/2.5';
end;

destructor TWeatherAPI.Destroy;
begin
  inherited;
end;

function TWeatherAPI.ExecuteRequest(const Resource: string; const Params: array of const): TJSONValue;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  FullResource: string;
  i: Integer;
begin
  Result := nil;
  FLastError := '';

  // Construire le chemin de ressource avec les paramètres
  FullResource := Resource;
  for i := 0 to High(Params) div 2 do
  begin
    FullResource := StringReplace(
      FullResource,
      '{' + string(Params[i*2]) + '}',
      string(Params[i*2+1]),
      [rfReplaceAll]
    );
  end;

  RESTClient := TRESTClient.Create(FBaseURL);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := FullResource;
    RESTRequest.Method := rmGET;

    // Ajouter les paramètres communs
    RESTRequest.Params.AddItem('appid', FApiKey, pkGETorPOST);
    RESTRequest.Params.AddItem('units', 'metric', pkGETorPOST);
    RESTRequest.Params.AddItem('lang', 'fr', pkGETorPOST);

    try
      RESTRequest.Execute;

      if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
      begin
        // Cloner la valeur JSON pour la renvoyer (le propriétaire sera libéré)
        Result := RESTResponse.JSONValue.Clone as TJSONValue;
      end
      else
      begin
        FLastError := Format('Erreur %d: %s', [RESTResponse.StatusCode, RESTResponse.StatusText]);

        // Essayer d'extraire un message d'erreur plus détaillé si disponible
        if Assigned(RESTResponse.JSONValue) then
        begin
          try
            var ErrorObj := RESTResponse.JSONValue as TJSONObject;
            if ErrorObj.TryGetValue<string>('message', var ErrorMsg) then
              FLastError := FLastError + ' - ' + ErrorMsg;
          except
            // Ignorer les erreurs de parsing
          end;
        end;
      end;
    except
      on E: Exception do
        FLastError := 'Exception: ' + E.Message;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TWeatherAPI.GetCurrentWeather(const City: string; out WeatherInfo: TWeatherInfo): Boolean;
var
  JsonObj: TJSONObject;
begin
  Result := False;

  // Initialiser la structure de sortie
  FillChar(WeatherInfo, SizeOf(WeatherInfo), 0);

  // Exécuter la requête
  JsonObj := ExecuteRequest('weather?q={city}', ['city', City]) as TJSONObject;

  if Assigned(JsonObj) then
  begin
    try
      // Extraire les données
      WeatherInfo.City := City;

      if JsonObj.TryGetValue<TJSONObject>('main', var MainObj) then
      begin
        MainObj.TryGetValue<Double>('temp', WeatherInfo.Temperature);
        MainObj.TryGetValue<Double>('feels_like', WeatherInfo.FeelsLike);
        MainObj.TryGetValue<Integer>('humidity', WeatherInfo.Humidity);
      end;

      if JsonObj.TryGetValue<TJSONArray>('weather', var WeatherArr) and (WeatherArr.Count > 0) then
        WeatherArr.Items[0].TryGetValue<string>('description', WeatherInfo.Description);

      if JsonObj.TryGetValue<TJSONObject>('sys', var SysObj) then
        SysObj.TryGetValue<string>('country', WeatherInfo.CountryCode);

      Result := True;
    finally
      JsonObj.Free;
    end;
  end;
end;

function TWeatherAPI.GetForecast(const City: string; DaysCount: Integer): TJSONArray;
var
  JsonObj: TJSONObject;
begin
  Result := nil;

  // Exécuter la requête
  JsonObj := ExecuteRequest('forecast/daily?q={city}&cnt={count}',
                           ['city', City, 'count', IntToStr(DaysCount)]) as TJSONObject;

  if Assigned(JsonObj) then
  begin
    try
      // Extraire le tableau de prévisions
      if JsonObj.TryGetValue<TJSONArray>('list', var ForecastList) then
        Result := ForecastList.Clone as TJSONArray;
    finally
      JsonObj.Free;
    end;
  end;
end;

end.
```

Utilisation dans votre formulaire :

```pascal
uses
  WeatherAPI, SecurityUtils;

procedure TForm1.ButtonGetWeatherClick(Sender: TObject);
var
  WeatherAPI: TWeatherAPI;
  WeatherInfo: TWeatherInfo;
  City: string;
begin
  City := EditCity.Text;
  if City = '' then
  begin
    ShowMessage('Veuillez entrer le nom d''une ville');
    Exit;
  end;

  LabelStatus.Caption := 'Récupération des données...';
  Application.ProcessMessages;

  WeatherAPI := TWeatherAPI.Create(GetApiKey('OpenWeatherMap'));
  try
    if WeatherAPI.GetCurrentWeather(City, WeatherInfo) then
    begin
      MemoResult.Lines.Clear;
      MemoResult.Lines.Add('Météo à ' + WeatherInfo.City + ', ' + WeatherInfo.CountryCode);
      MemoResult.Lines.Add('----------------------------');
      MemoResult.Lines.Add('Température: ' + FormatFloat('0.0', WeatherInfo.Temperature) + '°C');
      MemoResult.Lines.Add('Ressenti: ' + FormatFloat('0.0', WeatherInfo.FeelsLike) + '°C');
      MemoResult.Lines.Add('Humidité: ' + IntToStr(WeatherInfo.Humidity) + '%');
      MemoResult.Lines.Add('Conditions: ' + WeatherInfo.Description);

      LabelStatus.Caption := 'Données récupérées avec succès';
    end
    else
    begin
      MemoResult.Lines.Clear;
      MemoResult.Lines.Add('Erreur: ' + WeatherAPI.LastError);
      LabelStatus.Caption := 'Erreur lors de la récupération des données';
    end;
  finally
    WeatherAPI.Free;
  end;
end;
```

### 3. Gestion du cache

La mise en cache des résultats d'API peut améliorer les performances et réduire la consommation de données :

```pascal
// Ajout à la classe TWeatherAPI
private
  FCacheEnabled: Boolean;
  FCacheDuration: Integer; // en minutes

  function GetCacheFilePath(const Resource, Parameters: string): string;
  function TryGetFromCache(const Resource, Parameters: string): TJSONValue;
  procedure SaveToCache(const Resource, Parameters: string; const Value: TJSONValue);

public
  property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
  property CacheDuration: Integer read FCacheDuration write FCacheDuration;
```

Implémentation :

```pascal
function TWeatherAPI.GetCacheFilePath(const Resource, Parameters: string): string;
var
  CacheDir, CacheFileName: string;
  Hash: string;
begin
  // Créer un nom de fichier basé sur la requête
  Hash := THashMD5.GetHashString(Resource + Parameters + FApiKey);

  CacheDir := TPath.Combine(TPath.GetDocumentsPath, 'APICache');
  if not DirectoryExists(CacheDir) then
    ForceDirectories(CacheDir);

  CacheFileName := Hash + '.json';
  Result := TPath.Combine(CacheDir, CacheFileName);
end;

function TWeatherAPI.TryGetFromCache(const Resource, Parameters: string): TJSONValue;
var
  CacheFile: string;
  FileContent: string;
  FileAge: Integer;
begin
  Result := nil;

  if not FCacheEnabled then
    Exit;

  CacheFile := GetCacheFilePath(Resource, Parameters);

  if FileExists(CacheFile) then
  begin
    // Vérifier l'âge du fichier
    FileAge := MinutesBetween(Now, FileDateToDateTime(FileGetDate(CacheFile)));

    if FileAge <= FCacheDuration then
    begin
      // Lire le contenu du fichier cache
      with TStringList.Create do
      try
        LoadFromFile(CacheFile);
        FileContent := Text;

        // Parser le JSON
        Result := TJSONObject.ParseJSONValue(FileContent);
      except
        // En cas d'erreur de lecture ou de parsing, ignorer le cache
        Result := nil;
      end;
    end;
  end;
end;

procedure TWeatherAPI.SaveToCache(const Resource, Parameters: string; const Value: TJSONValue);
var
  CacheFile: string;
begin
  if not FCacheEnabled or not Assigned(Value) then
    Exit;

  CacheFile := GetCacheFilePath(Resource, Parameters);

  // Sauvegarder le contenu JSON dans le fichier cache
  try
    with TStringList.Create do
    try
      Text := Value.ToString;
      SaveToFile(CacheFile);
    finally
      Free;
    end;
  except
    // Ignorer les erreurs d'écriture de cache
  end;
end;

function TWeatherAPI.ExecuteRequest(const Resource: string; const Params: array of const): TJSONValue;
var
  ParametersStr: string;
  i: Integer;
begin
  // Construire une chaîne de paramètres pour le cache
  ParametersStr := '';
  for i := 0 to High(Params) div 2 do
    ParametersStr := ParametersStr + string(Params[i*2]) + '=' + string(Params[i*2+1]) + '&';

  // Essayer de récupérer depuis le cache
  Result := TryGetFromCache(Resource, ParametersStr);

  if Assigned(Result) then
    Exit;

  // Si pas en cache, exécuter la requête normale
  Result := DoExecuteRequest(Resource, Params);

  // Sauvegarder dans le cache si succès
  if Assigned(Result) then
    SaveToCache(Resource, ParametersStr, Result);
end;
```

### 4. Gestion des limites de débit (Rate Limiting)

De nombreuses API publiques imposent des limites au nombre de requêtes que vous pouvez effectuer. Une bonne gestion de ces limites est essentielle :

```pascal
// Ajout à la classe TWeatherAPI
private
  FRateLimitRemaining: Integer;
  FRateLimitReset: TDateTime;

  procedure UpdateRateLimitInfo(Response: TRESTResponse);
  function IsRateLimited: Boolean;

// Dans la méthode ExecuteRequest
if IsRateLimited then
begin
  FLastError := Format('Limite de requêtes atteinte. Réessayez après %s',
                       [FormatDateTime('hh:nn:ss', FRateLimitReset)]);
  Exit(nil);
end;

// Après l'exécution de la requête
UpdateRateLimitInfo(RESTResponse);
```

Implémentation :

```pascal
procedure TWeatherAPI.UpdateRateLimitInfo(Response: TRESTResponse);
begin
  // Récupérer les informations de limite de débit des en-têtes HTTP
  // Note: les noms exacts des en-têtes varient selon l'API

  // Exemple pour GitHub API
  if Response.Headers.ContainsKey('X-RateLimit-Remaining') then
    FRateLimitRemaining := StrToIntDef(Response.Headers.Values['X-RateLimit-Remaining'], 0);

  if Response.Headers.ContainsKey('X-RateLimit-Reset') then
  begin
    var UnixTime := StrToIntDef(Response.Headers.Values['X-RateLimit-Reset'], 0);
    if UnixTime > 0 then
      FRateLimitReset := UnixToDateTime(UnixTime);
  end;
end;

function TWeatherAPI.IsRateLimited: Boolean;
begin
  Result := (FRateLimitRemaining <= 0) and (FRateLimitReset > Now);
end;
```

### 5. Journalisation (Logging)

La journalisation des requêtes API peut être précieuse pour le débogage et la surveillance :

```pascal
// Dans une unité ApiLogger.pas
unit ApiLogger;

interface

type
  TApiLogLevel = (llInfo, llWarning, llError);

procedure LogApiCall(const ApiName, Method, Url, RequestBody: string);
procedure LogApiResponse(const ApiName: string; StatusCode: Integer;
                        const ResponseBody: string);
procedure LogApiError(const ApiName, ErrorMessage: string);

implementation

uses
  System.SysUtils, System.IOUtils, System.DateUtils;

const
  LOG_FOLDER = 'ApiLogs';

procedure EnsureLogFolderExists;
var
  LogPath: string;
begin
  LogPath := TPath.Combine(TPath.GetDocumentsPath, LOG_FOLDER);
  if not DirectoryExists(LogPath) then
    ForceDirectories(LogPath);
end;

function GetLogFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, LOG_FOLDER,
                         'ApiLog_' + FormatDateTime('yyyy-mm-dd', Now) + '.log');
end;

procedure WriteToLog(const LogEntry: string);
var
  LogFile: TextFile;
  FileName: string;
begin
  EnsureLogFolderExists;
  FileName := GetLogFileName;

  AssignFile(LogFile, FileName);
  try
    if FileExists(FileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' - ' + LogEntry);
  finally
    CloseFile(LogFile);
  end;
end;

procedure LogApiCall(const ApiName, Method, Url, RequestBody: string);
begin
  WriteToLog(Format('[%s] Request: %s %s', [ApiName, Method, Url]));
  if RequestBody <> '' then
    WriteToLog(Format('[%s] Request Body: %s', [ApiName, RequestBody]));
end;

procedure LogApiResponse(const ApiName: string; StatusCode: Integer;
                        const ResponseBody: string);
begin
  WriteToLog(Format('[%s] Response: Status %d', [ApiName, StatusCode]));

  // Limiter la taille du corps de réponse dans le log
  if Length(ResponseBody) > 1000 then
    WriteToLog(Format('[%s] Response Body: %s... (tronqué)',
              [ApiName, Copy(ResponseBody, 1, 1000)]))
  else
    WriteToLog(Format('[%s] Response Body: %s', [ApiName, ResponseBody]));
end;

procedure LogApiError(const ApiName, ErrorMessage: string);
begin
  WriteToLog(Format('[%s] ERROR: %s', [ApiName, ErrorMessage]));
end;

end.
```

Utilisation dans la classe API :

```pascal
uses
  ApiLogger;

// Dans ExecuteRequest
LogApiCall('WeatherAPI', 'GET', FBaseURL + FullResource, '');

// Après la requête
if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
  LogApiResponse('WeatherAPI', RESTResponse.StatusCode, RESTResponse.Content)
else
  LogApiError('WeatherAPI', FLastError);
```

### 6. Tests unitaires

Il est recommandé de tester vos intégrations API :

```pascal
procedure TWeatherAPITests.TestGetCurrentWeather;
var
  WeatherAPI: TWeatherAPI;
  WeatherInfo: TWeatherInfo;
  Result: Boolean;
begin
  // Créer une instance de l'API avec la clé de test
  WeatherAPI := TWeatherAPI.Create(TestApiKey);
  try
    // Activer le mock pour les tests
    WeatherAPI.UseMockResponses := True;

    // Tester avec une ville valide
    Result := WeatherAPI.GetCurrentWeather('Paris', WeatherInfo);

    // Vérifier les résultats
    CheckTrue(Result, 'La requête devrait réussir');
    CheckEquals('Paris', WeatherInfo.City, 'La ville devrait être correcte');
    CheckEquals('FR', WeatherInfo.CountryCode, 'Le code pays devrait être correct');
    Check(WeatherInfo.Temperature > -50, 'La température devrait être réaliste');
    Check(WeatherInfo.Temperature < 60, 'La température devrait être réaliste');

    // Tester avec une ville invalide
    Result := WeatherAPI.GetCurrentWeather('VilleInexistante123', WeatherInfo);
    CheckFalse(Result, 'La requête devrait échouer pour une ville invalide');
    Check(WeatherAPI.LastError <> '', 'Un message d''erreur devrait être présent');
  finally
    WeatherAPI.Free;
  end;
end;
```

## Exemples d'intégration d'API populaires

### 1. Google Maps

```pascal
procedure TForm1.ShowMapForAddress(const Address: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
  Location: TJSONObject;
  Lat, Lng: Double;
  MapHTML: string;
begin
  RESTClient := TRESTClient.Create('https://maps.googleapis.com/maps/api/geocode/json');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;

    RESTRequest.Params.AddItem('address', Address, pkGETorPOST);
    RESTRequest.Params.AddItem('key', GetApiKey('GoogleMaps'), pkGETorPOST);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      JsonObj := RESTResponse.JSONValue as TJSONObject;

      if JsonObj.GetValue<string>('status') = 'OK' then
      begin
        // Obtenir les coordonnées
        Location := JsonObj.GetValue<TJSONArray>('results').Items[0]
                          .GetValue<TJSONObject>('geometry')
                          .GetValue<TJSONObject>('location');

        Lat := Location.GetValue<Double>('lat');
        Lng := Location.GetValue<Double>('lng');

        // Générer le HTML pour afficher la carte
        MapHTML :=
          '<!DOCTYPE html>' +
          '<html>' +
          '<head>' +
          '  <meta charset="UTF-8">' +
          '  <style>html, body, #map {height: 100%; margin: 0; padding: 0;}</style>' +
          '  <script src="https://maps.googleapis.com/maps/api/js?key=' + GetApiKey('GoogleMaps') + '"></script>' +
          '  <script>' +
          '    function initMap() {' +
          '      var location = {lat: ' + FormatFloat('0.000000', Lat) + ', lng: ' + FormatFloat('0.000000', Lng) + '};' +
          '      var map = new google.maps.Map(document.getElementById("map"), {' +
          '        zoom: 14,' +
          '        center: location' +
          '      });' +
          '      var marker = new google.maps.Marker({' +
          '        position: location,' +
          '        map: map,' +
          '        title: "' + Address + '"' +
          '      });' +
          '    }' +
          '  </script>' +
          '</head>' +
          '<body onload="initMap()">' +
          '  <div id="map"></div>' +
          '</body>' +
          '</html>';

        // Afficher la carte dans un navigateur intégré
        WebBrowser1.Navigate('about:blank');
        (WebBrowser1.Document as IHTMLDocument2).write(MapHTML);
        (WebBrowser1.Document as IHTMLDocument2).close;
      end
      else
      begin
        ShowMessage('Erreur: ' + JsonObj.GetValue<string>('status'));
      end;
    end
    else
    begin
      ShowMessage('Erreur: ' + IntToStr(RESTResponse.StatusCode));
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### 2. API Twitter (X)

```pascal
procedure TForm1.PostTweet(const Message: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  OAuth1Authenticator: TOAuth1Authenticator;
  JsonObj: TJSONObject;
begin
  RESTClient := TRESTClient.Create('https://api.twitter.com/2/tweets');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  OAuth1Authenticator := TOAuth1Authenticator.Create(nil);

  try
    // Configuration OAuth1
    OAuth1Authenticator.ConsumerKey := GetApiKey('TwitterConsumerKey');
    OAuth1Authenticator.ConsumerSecret := GetApiKey('TwitterConsumerSecret');
    OAuth1Authenticator.Token := GetApiKey('TwitterToken');
    OAuth1Authenticator.TokenSecret := GetApiKey('TwitterTokenSecret');

    RESTClient.Authenticator := OAuth1Authenticator;
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Corps de la requête (format JSON)
    JsonObj := TJSONObject.Create;
    try
      JsonObj.AddPair('text', Message);
      RESTRequest.Body.Add(JsonObj.ToString);
      RESTRequest.Params.AddHeader('Content-Type', 'application/json');
    finally
      JsonObj.Free;
    end;

    // Exécution
    RESTRequest.Execute;

    // Traitement de la réponse
    if (RESTResponse.StatusCode = 201) or (RESTResponse.StatusCode = 200) then
      ShowMessage('Tweet publié avec succès !')
    else
      ShowMessage('Erreur lors de la publication: ' + RESTResponse.Content);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    OAuth1Authenticator.Free;
  end;
end;
```

### 3. API PayPal

PayPal est l'un des services de paiement en ligne les plus populaires. Voici comment intégrer son API REST dans votre application Delphi :

```pascal
type
  TPayPalAPI = class
  private
    FClientID: string;
    FClientSecret: string;
    FIsSandbox: Boolean;
    FAccessToken: string;
    FTokenExpiration: TDateTime;

    function GetBaseURL: string;
    function GetAccessToken: string;
    function IsTokenValid: Boolean;
    function RefreshAccessToken: Boolean;
  public
    constructor Create(const ClientID, ClientSecret: string; IsSandbox: Boolean = True);

    // Méthodes principales
    function CreatePayment(Amount: Double; const Currency, Description: string): string;
    function ExecutePayment(const PaymentID, PayerID: string): Boolean;
    function GetPaymentDetails(const PaymentID: string): TJSONObject;

    // Propriétés
    property IsSandbox: Boolean read FIsSandbox write FIsSandbox;
  end;

constructor TPayPalAPI.Create(const ClientID, ClientSecret: string; IsSandbox: Boolean = True);
begin
  inherited Create;

  FClientID := ClientID;
  FClientSecret := ClientSecret;
  FIsSandbox := IsSandbox;
  FAccessToken := '';
end;

function TPayPalAPI.GetBaseURL: string;
begin
  if FIsSandbox then
    Result := 'https://api.sandbox.paypal.com'
  else
    Result := 'https://api.paypal.com';
end;

function TPayPalAPI.IsTokenValid: Boolean;
begin
  // Vérifier si le token existe et n'est pas expiré
  Result := (FAccessToken <> '') and (FTokenExpiration > Now);
end;

function TPayPalAPI.GetAccessToken: string;
begin
  // Si le token n'est pas valide, on le rafraîchit
  if not IsTokenValid then
    RefreshAccessToken;

  Result := FAccessToken;
end;

function TPayPalAPI.RefreshAccessToken: Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
  BasicAuth: string;
  Expiration: Integer;
begin
  Result := False;
  FAccessToken := '';

  RESTClient := TRESTClient.Create(GetBaseURL + '/v1/oauth2/token');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Authentication Basic (ClientID:ClientSecret)
    BasicAuth := TNetEncoding.Base64.Encode(FClientID + ':' + FClientSecret);
    RESTRequest.Params.AddHeader('Authorization', 'Basic ' + BasicAuth);

    // Corps de la requête
    RESTRequest.AddParameter('grant_type', 'client_credentials', pkFORMDATA);

    // Exécution
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JsonObj := RESTResponse.JSONValue as TJSONObject;

      FAccessToken := JsonObj.GetValue<string>('access_token');
      Expiration := JsonObj.GetValue<Integer>('expires_in');

      // Convertir la durée d'expiration en TDateTime
      FTokenExpiration := Now + (Expiration / SecsPerDay);

      Result := True;
    end
    else
    begin
      // Journaliser l'erreur
      LogApiError('PayPalAPI', 'Erreur d''authentification: ' + RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TPayPalAPI.CreatePayment(Amount: Double; const Currency, Description: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj, PaymentObject: TJSONObject;
  RedirectURL, PaymentID, ApprovalUrl: string;
  Links: TJSONArray;
  i: Integer;
begin
  Result := '';

  // S'assurer d'avoir un token valide
  if GetAccessToken = '' then
    Exit;

  RESTClient := TRESTClient.Create(GetBaseURL + '/v1/payments/payment');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // En-tête d'autorisation
    RESTRequest.Params.AddHeader('Authorization', 'Bearer ' + FAccessToken);
    RESTRequest.Params.AddHeader('Content-Type', 'application/json');

    // Création de l'objet JSON pour la requête
    PaymentObject := TJSONObject.Create;
    try
      // Configuration du paiement
      PaymentObject.AddPair('intent', 'sale');

      // Information sur l'acheteur
      var Payer := TJSONObject.Create;
      Payer.AddPair('payment_method', 'paypal');
      PaymentObject.AddPair('payer', Payer);

      // Configuration des transactions
      var Transactions := TJSONArray.Create;
      var Transaction := TJSONObject.Create;

      var Amount := TJSONObject.Create;
      Amount.AddPair('total', FormatFloat('0.00', Amount));
      Amount.AddPair('currency', Currency);

      var ItemList := TJSONObject.Create;
      var Items := TJSONArray.Create;
      var Item := TJSONObject.Create;
      Item.AddPair('name', Description);
      Item.AddPair('quantity', '1');
      Item.AddPair('price', FormatFloat('0.00', Amount));
      Item.AddPair('currency', Currency);
      Items.Add(Item);
      ItemList.AddPair('items', Items);

      Transaction.AddPair('amount', Amount);
      Transaction.AddPair('description', Description);
      Transaction.AddPair('item_list', ItemList);

      Transactions.Add(Transaction);
      PaymentObject.AddPair('transactions', Transactions);

      // URLs de redirection
      var RedirectUrls := TJSONObject.Create;
      RedirectUrls.AddPair('return_url', 'http://localhost:8080/success');
      RedirectUrls.AddPair('cancel_url', 'http://localhost:8080/cancel');
      PaymentObject.AddPair('redirect_urls', RedirectUrls);

      // Ajouter le corps de la requête
      RESTRequest.Body.Add(PaymentObject.ToString);

      // Exécution de la requête
      RESTRequest.Execute;

      // Traitement de la réponse
      if RESTResponse.StatusCode = 201 then // Created
      begin
        JsonObj := RESTResponse.JSONValue as TJSONObject;

        // Récupérer l'ID du paiement
        PaymentID := JsonObj.GetValue<string>('id');

        // Trouver l'URL d'approbation
        Links := JsonObj.GetValue<TJSONArray>('links');
        for i := 0 to Links.Count - 1 do
        begin
          var Link := Links.Items[i] as TJSONObject;
          if Link.GetValue<string>('rel') = 'approval_url' then
          begin
            ApprovalUrl := Link.GetValue<string>('href');
            Break;
          end;
        end;

        // Ouvrir le navigateur pour que l'utilisateur approuve le paiement
        if ApprovalUrl <> '' then
        begin
          ShellExecute(0, 'open', PChar(ApprovalUrl), nil, nil, SW_SHOWNORMAL);
          Result := PaymentID;
        end;
      end
      else
      begin
        // Journaliser l'erreur
        LogApiError('PayPalAPI', 'Erreur de création de paiement: ' +
                   RESTResponse.StatusText + ' - ' + RESTResponse.Content);
      end;
    finally
      PaymentObject.Free;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TPayPalAPI.ExecutePayment(const PaymentID, PayerID: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
begin
  Result := False;

  // S'assurer d'avoir un token valide
  if GetAccessToken = '' then
    Exit;

  RESTClient := TRESTClient.Create(GetBaseURL + '/v1/payments/payment/' + PaymentID + '/execute');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // En-tête d'autorisation
    RESTRequest.Params.AddHeader('Authorization', 'Bearer ' + FAccessToken);
    RESTRequest.Params.AddHeader('Content-Type', 'application/json');

    // Corps de la requête
    JsonObj := TJSONObject.Create;
    try
      JsonObj.AddPair('payer_id', PayerID);
      RESTRequest.Body.Add(JsonObj.ToString);

      // Exécution de la requête
      RESTRequest.Execute;

      // Traitement de la réponse
      Result := (RESTResponse.StatusCode = 200);

      if not Result then
      begin
        // Journaliser l'erreur
        LogApiError('PayPalAPI', 'Erreur d''exécution de paiement: ' +
                   RESTResponse.StatusText + ' - ' + RESTResponse.Content);
      end;
    finally
      JsonObj.Free;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

Pour utiliser cette classe dans votre application, vous aurez besoin de :

1. Créer un compte développeur PayPal
2. Créer une application dans la console développeur PayPal pour obtenir les identifiants ClientID et ClientSecret
3. Configurer un petit serveur local pour intercepter les callbacks (ou utiliser un service comme ngrok)

Voici un exemple d'utilisation :

```pascal
procedure TForm1.ButtonPayWithPayPalClick(Sender: TObject);
var
  PayPalAPI: TPayPalAPI;
  PaymentID: string;
begin
  PayPalAPI := TPayPalAPI.Create(
    GetApiKey('PayPalClientID'),
    GetApiKey('PayPalClientSecret'),
    True // Utiliser le bac à sable pour les tests
  );

  try
    PaymentID := PayPalAPI.CreatePayment(
      StrToFloat(EditAmount.Text),
      'EUR',
      'Achat de ' + EditProductName.Text
    );

    if PaymentID <> '' then
    begin
      ShowMessage('Paiement créé avec succès. ID: ' + PaymentID);

      // Stocker l'ID du paiement pour l'utiliser lors du retour
      FCurrentPaymentID := PaymentID;
    end
    else
    begin
      ShowMessage('Erreur lors de la création du paiement.');
    end;
  finally
    PayPalAPI.Free;
  end;
end;

// Cette méthode serait appelée lorsque l'utilisateur revient après approbation
procedure TForm1.HandlePayPalCallback(const PayerID: string);
var
  PayPalAPI: TPayPalAPI;
begin
  if FCurrentPaymentID = '' then
  begin
    ShowMessage('Aucun paiement en cours.');
    Exit;
  end;

  PayPalAPI := TPayPalAPI.Create(
    GetApiKey('PayPalClientID'),
    GetApiKey('PayPalClientSecret'),
    True
  );

  try
    if PayPalAPI.ExecutePayment(FCurrentPaymentID, PayerID) then
    begin
      ShowMessage('Paiement effectué avec succès !');
      // Mettre à jour l'état de la commande dans votre système
      UpdateOrderStatus(FCurrentOrderID, 'PAID');
    end
    else
    begin
      ShowMessage('Erreur lors de l''exécution du paiement.');
    end;
  finally
    PayPalAPI.Free;
    FCurrentPaymentID := '';
  end;
end;
```

### 4. API Stripe

Stripe est une autre plateforme de paiement populaire. Voici comment intégrer son API REST :

```pascal
type
  TStripeAPI = class
  private
    FApiKey: string;
    FIsTest: Boolean;

    function GetBaseURL: string;
    function ExecuteRequest(const Method, Resource: string;
                           const Params: TStrings = nil): TJSONValue;
  public
    constructor Create(const ApiKey: string; IsTest: Boolean = True);

    // Méthodes principales
    function CreatePaymentIntent(Amount: Integer; const Currency, Description: string): string;
    function CreateCheckoutSession(Amount: Integer; const Currency, Description: string): string;
    function GetPaymentIntent(const PaymentIntentID: string): TJSONObject;
    function RefundPayment(const PaymentIntentID: string): Boolean;

    // Propriétés
    property IsTest: Boolean read FIsTest write FIsTest;
  end;

constructor TStripeAPI.Create(const ApiKey: string; IsTest: Boolean = True);
begin
  inherited Create;

  FApiKey := ApiKey;
  FIsTest := IsTest;
end;

function TStripeAPI.GetBaseURL: string;
begin
  Result := 'https://api.stripe.com/v1';
end;

function TStripeAPI.ExecuteRequest(const Method, Resource: string;
                                  const Params: TStrings = nil): TJSONValue;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := nil;

  RESTClient := TRESTClient.Create(GetBaseURL + Resource);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // Définir la méthode
    if SameText(Method, 'GET') then
      RESTRequest.Method := rmGET
    else if SameText(Method, 'POST') then
      RESTRequest.Method := rmPOST
    else if SameText(Method, 'DELETE') then
      RESTRequest.Method := rmDELETE
    else
      RESTRequest.Method := rmGET;

    // Authentification avec l'API key
    RESTClient.Authenticator := THTTPBasicAuthenticator.Create(FApiKey, '');

    // Ajouter les paramètres si présents
    if Assigned(Params) and (Params.Count > 0) then
    begin
      for var I := 0 to Params.Count - 1 do
      begin
        var Name := Params.Names[I];
        var Value := Params.ValueFromIndex[I];

        if RESTRequest.Method in [rmGET, rmDELETE] then
          RESTRequest.AddParameter(Name, Value, pkGETorPOST)
        else
          RESTRequest.AddParameter(Name, Value, pkFORMDATA);
      end;
    end;

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      Result := RESTResponse.JSONValue.Clone as TJSONValue;
    end
    else
    begin
      // Journaliser l'erreur
      LogApiError('StripeAPI', 'Erreur HTTP ' + IntToStr(RESTResponse.StatusCode) +
                 ': ' + RESTResponse.StatusText + ' - ' + RESTResponse.Content);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TStripeAPI.CreatePaymentIntent(Amount: Integer; const Currency, Description: string): string;
var
  Params: TStringList;
  JsonObj: TJSONObject;
begin
  Result := '';

  Params := TStringList.Create;
  try
    // L'amount doit être en centimes (ex: 1000 pour 10,00 €)
    Params.Values['amount'] := IntToStr(Amount);
    Params.Values['currency'] := Currency;
    Params.Values['description'] := Description;

    // Ajouter d'autres paramètres optionnels si nécessaire

    JsonObj := ExecuteRequest('POST', '/payment_intents', Params) as TJSONObject;

    if Assigned(JsonObj) then
    begin
      try
        // Récupérer l'ID de l'intention de paiement
        Result := JsonObj.GetValue<string>('id');

        // Le client secret est nécessaire pour compléter le paiement côté client
        var ClientSecret := JsonObj.GetValue<string>('client_secret');

        // Stocker le client secret pour l'utiliser plus tard
        // ...

      finally
        JsonObj.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TStripeAPI.CreateCheckoutSession(Amount: Integer; const Currency, Description: string): string;
var
  Params: TStringList;
  JsonObj: TJSONObject;
begin
  Result := '';

  Params := TStringList.Create;
  try
    // Configuration des articles
    Params.Values['line_items[0][price_data][currency]'] := Currency;
    Params.Values['line_items[0][price_data][product_data][name]'] := Description;
    Params.Values['line_items[0][price_data][unit_amount]'] := IntToStr(Amount);
    Params.Values['line_items[0][quantity]'] := '1';

    // Mode de paiement
    Params.Values['mode'] := 'payment';

    // URLs de redirection
    Params.Values['success_url'] := 'http://localhost:8080/success?session_id={CHECKOUT_SESSION_ID}';
    Params.Values['cancel_url'] := 'http://localhost:8080/cancel';

    JsonObj := ExecuteRequest('POST', '/checkout/sessions', Params) as TJSONObject;

    if Assigned(JsonObj) then
    begin
      try
        // Récupérer l'URL de la page de paiement Stripe
        var Url := JsonObj.GetValue<string>('url');

        // Ouvrir l'URL dans le navigateur
        if Url <> '' then
        begin
          ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);

          // Retourner l'ID de la session
          Result := JsonObj.GetValue<string>('id');
        end;
      finally
        JsonObj.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TStripeAPI.GetPaymentIntent(const PaymentIntentID: string): TJSONObject;
begin
  Result := ExecuteRequest('GET', '/payment_intents/' + PaymentIntentID) as TJSONObject;
end;

function TStripeAPI.RefundPayment(const PaymentIntentID: string): Boolean;
var
  Params: TStringList;
  JsonObj: TJSONObject;
begin
  Result := False;

  Params := TStringList.Create;
  try
    Params.Values['payment_intent'] := PaymentIntentID;

    JsonObj := ExecuteRequest('POST', '/refunds', Params) as TJSONObject;

    if Assigned(JsonObj) then
    begin
      try
        // Vérifier si le remboursement a été effectué
        Result := JsonObj.GetValue<string>('status') = 'succeeded';
      finally
        JsonObj.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;
```

Utilisation dans votre application :

```pascal
procedure TForm1.ButtonStripeCheckoutClick(Sender: TObject);
var
  StripeAPI: TStripeAPI;
  Amount: Integer;
  SessionID: string;
begin
  // Convertir le montant en centimes (ex: 10,00 € -> 1000)
  Amount := Round(StrToFloat(EditAmount.Text) * 100);

  StripeAPI := TStripeAPI.Create(GetApiKey('StripeSecretKey'), True);
  try
    // Créer une session Checkout pour le paiement
    SessionID := StripeAPI.CreateCheckoutSession(
      Amount,
      'EUR',
      'Achat de ' + EditProductName.Text
    );

    if SessionID <> '' then
    begin
      // Stocker l'ID de session pour vérification ultérieure
      FCurrentSessionID := SessionID;

      // La redirection vers la page de paiement Stripe est gérée dans CreateCheckoutSession
    end
    else
    begin
      ShowMessage('Erreur lors de la création de la session de paiement.');
    end;
  finally
    StripeAPI.Free;
  end;
end;

// Cette méthode serait appelée lorsque l'utilisateur revient après le paiement
procedure TForm1.HandleStripeCallback(const SessionID: string);
var
  StripeAPI: TStripeAPI;
  PaymentIntentID: string;
  PaymentIntent: TJSONObject;
  Status: string;
begin
  if SessionID <> FCurrentSessionID then
  begin
    ShowMessage('Session de paiement invalide.');
    Exit;
  end;

  StripeAPI := TStripeAPI.Create(GetApiKey('StripeSecretKey'), True);
  try
    // Récupérer les détails de la session de paiement
    var SessionObj := StripeAPI.ExecuteRequest('GET', '/checkout/sessions/' + SessionID) as TJSONObject;

    if Assigned(SessionObj) then
    begin
      try
        // Récupérer l'ID de l'intention de paiement
        PaymentIntentID := SessionObj.GetValue<string>('payment_intent');

        // Récupérer les détails de l'intention de paiement
        PaymentIntent := StripeAPI.GetPaymentIntent(PaymentIntentID);

        if Assigned(PaymentIntent) then
        begin
          try
            // Vérifier le statut du paiement
            Status := PaymentIntent.GetValue<string>('status');

            if Status = 'succeeded' then
            begin
              ShowMessage('Paiement effectué avec succès !');
              // Mettre à jour l'état de la commande dans votre système
              UpdateOrderStatus(FCurrentOrderID, 'PAID');
            end
            else
            begin
              ShowMessage('Le paiement n''a pas été complété. Statut: ' + Status);
            end;
          finally
            PaymentIntent.Free;
          end;
        end;
      finally
        SessionObj.Free;
      end;
    end;
  finally
    StripeAPI.Free;
    FCurrentSessionID := '';
  end;
end;
```

## Sécurité et conformité des API

### Protection des données sensibles

Lorsque vous travaillez avec des API de paiement ou d'autres services manipulant des données sensibles, vous devez vous conformer à diverses normes de sécurité comme PCI DSS (Payment Card Industry Data Security Standard) :

```pascal
procedure SecurePaymentProcessing;
begin
  // Règles de sécurité pour le traitement des paiements :

  // 1. Ne jamais stocker les numéros de carte complets
  // 2. Utiliser TLS pour toutes les communications
  // 3. Limiter l'accès aux données de paiement
  // 4. Journaliser les accès mais pas les données sensibles
  // 5. Utiliser des jetons plutôt que des numéros de carte
  // 6. Chiffrer les données stockées

  // Exemple: utiliser un jeton de paiement au lieu d'un numéro de carte
  var StripeAPI := TStripeAPI.Create(GetApiKey('StripeSecretKey'));
  try
    // Créer un jeton pour la carte
    var TokenParams := TStringList.Create;
    try
      // Ne PAS stocker ces informations !
      TokenParams.Values['card[number]'] := '4242424242424242'; // Carte de test Stripe
      TokenParams.Values['card[exp_month]'] := '12';
      TokenParams.Values['card[exp_year]'] := '2025';
      TokenParams.Values['card[cvc]'] := '123';

      var TokenObj := StripeAPI.ExecuteRequest('POST', '/tokens', TokenParams) as TJSONObject;

      if Assigned(TokenObj) then
      begin
        try
          // Utiliser le jeton pour le paiement (sécurité améliorée)
          var TokenID := TokenObj.GetValue<string>('id');

          // Créer la charge avec le jeton
          var ChargeParams := TStringList.Create;
          try
            ChargeParams.Values['amount'] := '1000'; // 10,00 €
            ChargeParams.Values['currency'] := 'eur';
            ChargeParams.Values['source'] := TokenID;
            ChargeParams.Values['description'] := 'Paiement sécurisé';

            StripeAPI.ExecuteRequest('POST', '/charges', ChargeParams);
          finally
            ChargeParams.Free;
          end;
        finally
          TokenObj.Free;
        end;
      end;
    finally
      TokenParams.Free;
    end;
  finally
    StripeAPI.Free;
  end;
end;
```

### GDPR et autres réglementations

Pour être conforme au RGPD (Règlement Général sur la Protection des Données) et autres réglementations similaires :

```pascal
procedure EnsureGDPRCompliance;
begin
  // 1. Obtenir le consentement explicite
  if not UserHasConsented('api_data_processing') then
  begin
    if MessageDlg('Cette application va envoyer des données à des services externes. ' +
                 'Acceptez-vous le traitement de vos données conformément à notre politique de confidentialité ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      SaveUserConsent('api_data_processing', True);
    end
    else
    begin
      ShowMessage('Sans votre consentement, certaines fonctionnalités ne seront pas disponibles.');
      Exit;
    end;
  end;

  // 2. Minimiser les données envoyées
  var MinimalUserData := TJSONObject.Create;
  try
    // Envoyer uniquement les données nécessaires, pas tout le profil
    MinimalUserData.AddPair('language', GetUserLanguage);
    MinimalUserData.AddPair('country', GetUserCountry);

    // 3. Journaliser le traitement des données (pour montrer la conformité)
    LogDataProcessing('Envoi de données minimales à l''API', MinimalUserData.ToString);

    SendToAPI(MinimalUserData);
  finally
    MinimalUserData.Free;
  end;

  // 4. Offrir un moyen d'exporter les données
  // 5. Offrir un moyen d'effacer les données
  // 6. Mettre en place des politiques de rétention des données
end;
```

## Conclusion

L'intégration d'API REST tierces ouvre un monde de possibilités pour vos applications Delphi, permettant d'ajouter des fonctionnalités avancées comme les paiements en ligne, les réseaux sociaux, la géolocalisation et bien plus encore.

Voici les points clés à retenir :

1. **Comprendre les bases** : Maîtrisez les concepts REST (méthodes HTTP, codes d'état, formats de données)
2. **Utilisez les bons outils** : Delphi propose des composants puissants comme TRESTClient pour interagir avec les API
3. **Organisez votre code** : Créez des classes d'encapsulation pour chaque API afin de maintenir votre code propre et modulaire
4. **Gérez les erreurs** : Implémentez une gestion robuste des erreurs avec nouvelles tentatives si nécessaire
5. **Sécurisez les données sensibles** : Ne stockez jamais les clés API ou les jetons dans votre code source
6. **Respectez les limites** : Tenez compte des limites de débit des API et mettez en place une mise en cache appropriée
7. **Tests approfondis** : Testez votre intégration sous différentes conditions (connexion lente, erreurs serveur, etc.)

En suivant ces meilleures pratiques, vous pourrez intégrer de manière fiable et efficace n'importe quelle API REST dans vos applications Delphi.

N'oubliez pas que chaque API a ses propres particularités et documentation. Consultez toujours la documentation officielle de l'API que vous souhaitez intégrer pour connaître les détails spécifiques à cette API.

## Ressources complémentaires

- **Documentation Delphi** : La documentation officielle des composants REST dans Delphi est une ressource précieuse.
  - Recherchez "REST Client Library" dans l'aide de Delphi
  - Documentation en ligne sur DocWiki d'Embarcadero

- **Sites de documentation d'API** :
  - [Swagger](https://swagger.io/) et [OpenAPI](https://www.openapis.org/) - Standards pour la documentation d'API
  - [Postman](https://www.postman.com/) - Un outil puissant pour tester et explorer les API

- **Sites de test d'API** :
  - [JSON Placeholder](https://jsonplaceholder.typicode.com/) - Une API REST fictive pour les tests
  - [ReqRes](https://reqres.in/) - Un service d'API REST de test

- **Sites d'apprentissage** :
  - [REST API Tutorial](https://restfulapi.net/) - Concepts REST et bonnes pratiques
  - [Mozilla Developer Network](https://developer.mozilla.org/fr/docs/Web/HTTP) - Documentation sur HTTP et REST

- **Outils de débogage** :
  - [Fiddler](https://www.telerik.com/fiddler) - Proxy de débogage HTTP
  - [Charles Proxy](https://www.charlesproxy.com/) - Alternative à Fiddler
  - [Wireshark](https://www.wireshark.org/) - Pour une analyse réseau plus approfondie

## Exemples de projets complets

Pour mieux comprendre l'intégration d'API, voici quelques idées de projets simples que vous pourriez réaliser pour pratiquer :

### 1. Application météo multiville

```pascal
// Unité principale (MainForm.pas)
unit MainForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON, Vcl.ComCtrls,
  System.ImageList, Vcl.ImgList, WeatherAPI, SecurityUtils;

type
  TFrmMain = class(TForm)
    PnlTop: TPanel;
    LblTitle: TLabel;
    EdtCity: TEdit;
    BtnAddCity: TButton;
    PnlBottom: TPanel;
    LblStatus: TLabel;
    PageControl: TPageControl;
    ImgWeather: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure BtnAddCityClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWeatherAPI: TWeatherAPI;
    FCityList: TStringList;

    procedure LoadCityList;
    procedure SaveCityList;
    procedure RefreshAllWeatherData;
    function CreateCityTab(const City: string): TTabSheet;
    procedure UpdateCityWeather(TabSheet: TTabSheet; const City: string);
    function GetWeatherIconIndex(const WeatherCode: string): Integer;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

// Implémentation ici...

// Exemple de méthode pour rafraîchir les données météo
procedure TFrmMain.RefreshAllWeatherData;
var
  i: Integer;
begin
  LblStatus.Caption := 'Mise à jour des données...';
  Application.ProcessMessages;

  for i := 0 to PageControl.PageCount - 1 do
  begin
    var TabSheet := PageControl.Pages[i];
    var City := TabSheet.Caption;

    UpdateCityWeather(TabSheet, City);
  end;

  LblStatus.Caption := 'Données mises à jour: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
end;

// Exemple de méthode pour mettre à jour la météo d'une ville
procedure TFrmMain.UpdateCityWeather(TabSheet: TTabSheet; const City: string);
var
  WeatherInfo: TWeatherInfo;
  LblTemp, LblCondition, LblHumidity, LblUpdate: TLabel;
  ImgCondition: TImage;
begin
  // Trouver les contrôles dans l'onglet
  LblTemp := TabSheet.FindComponent('LblTemp' + City) as TLabel;
  LblCondition := TabSheet.FindComponent('LblCondition' + City) as TLabel;
  LblHumidity := TabSheet.FindComponent('LblHumidity' + City) as TLabel;
  LblUpdate := TabSheet.FindComponent('LblUpdate' + City) as TLabel;
  ImgCondition := TabSheet.FindComponent('ImgCondition' + City) as TImage;

  if FWeatherAPI.GetCurrentWeather(City, WeatherInfo) then
  begin
    // Mettre à jour les contrôles avec les nouvelles données
    LblTemp.Caption := FormatFloat('0.0°C', WeatherInfo.Temperature);
    LblCondition.Caption := WeatherInfo.Description;
    LblHumidity.Caption := 'Humidité: ' + IntToStr(WeatherInfo.Humidity) + '%';
    LblUpdate.Caption := 'Dernière mise à jour: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);

    // Mettre à jour l'icône météo
    var IconIndex := GetWeatherIconIndex(WeatherInfo.WeatherCode);
    ImgWeather.GetBitmap(IconIndex, ImgCondition.Picture.Bitmap);
  end
  else
  begin
    LblTemp.Caption := 'N/A';
    LblCondition.Caption := 'Erreur: ' + FWeatherAPI.LastError;
    LblHumidity.Caption := '';
    LblUpdate.Caption := 'Échec de la mise à jour: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
  end;
end;
```

### 2. Client GitHub simple

```pascal
// Unité principale (GitHubExplorer.pas)
unit GitHubExplorer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON, GitHubAPI, SecurityUtils;

type
  TFrmGitHubExplorer = class(TForm)
    PnlTop: TPanel;
    LblUsername: TLabel;
    EdtUsername: TEdit;
    BtnSearch: TButton;
    PageControl: TPageControl;
    TabRepos: TTabSheet;
    TabFollowers: TTabSheet;
    LvRepos: TListView;
    LvFollowers: TListView;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure LvReposDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGitHubAPI: TGitHubAPI;

    procedure LoadUserData(const Username: string);
    procedure LoadRepositories(const Username: string);
    procedure LoadFollowers(const Username: string);
    procedure DisplayApiError(const ErrorMessage: string);
  public
    { Public declarations }
  end;

var
  FrmGitHubExplorer: TFrmGitHubExplorer;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.ShellAPI;

// Implémentation ici...

// Exemple de méthode pour charger les dépôts
procedure TFrmGitHubExplorer.LoadRepositories(const Username: string);
var
  Repositories: TJSONArray;
  i: Integer;
  Item: TListItem;
begin
  LvRepos.Items.Clear;
  StatusBar.SimpleText := 'Chargement des dépôts...';
  Application.ProcessMessages;

  Repositories := FGitHubAPI.GetUserRepositories(Username);

  if Assigned(Repositories) then
  begin
    try
      for i := 0 to Repositories.Count - 1 do
      begin
        var RepoObj := Repositories.Items[i] as TJSONObject;

        Item := LvRepos.Items.Add;
        Item.Caption := RepoObj.GetValue<string>('name');
        Item.SubItems.Add(RepoObj.GetValue<string>('description'));
        Item.SubItems.Add(IntToStr(RepoObj.GetValue<Integer>('stargazers_count')));
        Item.SubItems.Add(RepoObj.GetValue<string>('language'));
        Item.SubItems.Add(FormatDateTime('yyyy-mm-dd',
                         ISO8601ToDate(RepoObj.GetValue<string>('updated_at'))));

        // Stocker l'URL dans les données
        Item.Data := Pointer(TObject(StrNew(PChar(RepoObj.GetValue<string>('html_url')))));
      end;

      StatusBar.SimpleText := IntToStr(Repositories.Count) + ' dépôts trouvés';
    finally
      Repositories.Free;
    end;
  end
  else
  begin
    DisplayApiError(FGitHubAPI.LastError);
  end;
end;
```

### 3. Gestionnaire de tâches avec API Backend

```pascal
// Unité principale (TaskManager.pas)
unit TaskManager;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON, TaskAPI, SecurityUtils;

type
  TTaskStatus = (tsNew, tsInProgress, tsCompleted);

  TTask = class
    ID: Integer;
    Title: string;
    Description: string;
    DueDate: TDateTime;
    Status: TTaskStatus;

    constructor Create; overload;
    constructor Create(const ATitle, ADescription: string;
                      ADueDate: TDateTime; AStatus: TTaskStatus); overload;
  end;

  TFrmTaskManager = class(TForm)
    PnlTop: TPanel;
    LblTitle: TLabel;
    PnlBottom: TPanel;
    StatusBar: TStatusBar;
    PnlLeft: TPanel;
    LvTasks: TListView;
    PnlRight: TPanel;
    LblTaskTitle: TLabel;
    EdtTaskTitle: TEdit;
    LblTaskDescription: TLabel;
    MemoTaskDescription: TMemo;
    LblDueDate: TLabel;
    DatePickerDue: TDateTimePicker;
    CmbStatus: TComboBox;
    LblStatus: TLabel;
    BtnNew: TButton;
    BtnSave: TButton;
    BtnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnNewClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure LvTasksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FTaskAPI: TTaskAPI;
    FCurrentTask: TTask;

    procedure LoadTasks;
    procedure DisplayTask(Task: TTask);
    procedure ClearTaskForm;
    procedure UpdateTaskList;
    procedure DisplayApiError(const ErrorMessage: string);
    function StatusToString(Status: TTaskStatus): string;
    function StringToStatus(const StatusStr: string): TTaskStatus;
  public
    { Public declarations }
  end;

var
  FrmTaskManager: TFrmTaskManager;

implementation

{$R *.dfm}

// Implémentation ici...

// Exemple de méthode pour sauvegarder une tâche
procedure TFrmTaskManager.BtnSaveClick(Sender: TObject);
var
  Success: Boolean;
begin
  if EdtTaskTitle.Text = '' then
  begin
    ShowMessage('Le titre de la tâche est obligatoire.');
    EdtTaskTitle.SetFocus;
    Exit;
  end;

  if not Assigned(FCurrentTask) then
    FCurrentTask := TTask.Create;

  // Mettre à jour les données de la tâche
  FCurrentTask.Title := EdtTaskTitle.Text;
  FCurrentTask.Description := MemoTaskDescription.Text;
  FCurrentTask.DueDate := DatePickerDue.Date;
  FCurrentTask.Status := StringToStatus(CmbStatus.Text);

  StatusBar.SimpleText := 'Enregistrement de la tâche...';
  Application.ProcessMessages;

  if FCurrentTask.ID = 0 then
    Success := FTaskAPI.CreateTask(FCurrentTask)
  else
    Success := FTaskAPI.UpdateTask(FCurrentTask);

  if Success then
  begin
    StatusBar.SimpleText := 'Tâche enregistrée avec succès';
    LoadTasks;
    ClearTaskForm;
  end
  else
  begin
    DisplayApiError(FTaskAPI.LastError);
  end;
end;
```

## Pour aller plus loin

### Gestion des APIs avec authentification OAuth 2.0

L'OAuth 2.0 est un protocole d'autorisation complexe utilisé par de nombreux services populaires (Google, Facebook, Twitter, Microsoft, etc.). Voici un exemple plus complet d'intégration avec OAuth 2.0 :

```pascal
type
  TOAuth2Flow = (ofAuthorizationCode, ofImplicit, ofPasswordCredentials, ofClientCredentials);

  TOAuth2API = class
  private
    FClientID: string;
    FClientSecret: string;
    FRedirectURI: string;
    FAuthorizationEndpoint: string;
    FTokenEndpoint: string;
    FScope: string;
    FAccessToken: string;
    FRefreshToken: string;
    FTokenExpirationTime: TDateTime;
    FFlow: TOAuth2Flow;
    FLastError: string;

    function BuildAuthorizationURL: string;
    function ExchangeCodeForToken(const Code: string): Boolean;
    function RefreshAccessToken: Boolean;
    function IsTokenValid: Boolean;
    function GetAccessToken: string;
  public
    constructor Create(const ClientID, ClientSecret, RedirectURI,
                      AuthEndpoint, TokenEndpoint: string;
                      Flow: TOAuth2Flow = ofAuthorizationCode);

    function StartAuthorization: Boolean;
    function HandleRedirect(const URL: string): Boolean;
    function ExecuteRequest(const URL: string; Method: TRESTRequestMethod = rmGET;
                           const Body: string = ''): TJSONValue;

    property AccessToken: string read GetAccessToken;
    property LastError: string read FLastError;
  end;

constructor TOAuth2API.Create(const ClientID, ClientSecret, RedirectURI,
                            AuthEndpoint, TokenEndpoint: string;
                            Flow: TOAuth2Flow = ofAuthorizationCode);
begin
  inherited Create;

  FClientID := ClientID;
  FClientSecret := ClientSecret;
  FRedirectURI := RedirectURI;
  FAuthorizationEndpoint := AuthEndpoint;
  FTokenEndpoint := TokenEndpoint;
  FFlow := Flow;

  FAccessToken := '';
  FRefreshToken := '';
  FTokenExpirationTime := 0;
end;

function TOAuth2API.BuildAuthorizationURL: string;
begin
  Result := FAuthorizationEndpoint +
            '?client_id=' + TNetEncoding.URL.Encode(FClientID) +
            '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectURI) +
            '&response_type=code' +
            '&scope=' + TNetEncoding.URL.Encode(FScope) +
            '&state=' + TNetEncoding.URL.Encode(GenerateRandomState);
end;

function TOAuth2API.StartAuthorization: Boolean;
var
  AuthURL: string;
begin
  Result := False;

  // Construire l'URL d'autorisation
  AuthURL := BuildAuthorizationURL;

  // Ouvrir le navigateur pour l'autorisation
  ShellExecute(0, 'open', PChar(AuthURL), nil, nil, SW_SHOWNORMAL);

  // À ce stade, l'utilisateur est redirigé vers le site d'autorisation
  // Le résultat sera traité dans HandleRedirect
  Result := True;
end;

function TOAuth2API.HandleRedirect(const URL: string): Boolean;
var
  QueryParams: TStringList;
  Code, Error, State: string;
begin
  Result := False;

  // Extraire les paramètres de l'URL de redirection
  QueryParams := ExtractURLParameters(URL);
  try
    // Vérifier les erreurs
    if QueryParams.Values['error'] <> '' then
    begin
      FLastError := 'Erreur d''autorisation: ' + QueryParams.Values['error'];
      if QueryParams.Values['error_description'] <> '' then
        FLastError := FLastError + ' - ' + QueryParams.Values['error_description'];

      Exit;
    end;

    // Vérifier le code d'autorisation
    Code := QueryParams.Values['code'];
    if Code = '' then
    begin
      FLastError := 'Code d''autorisation manquant dans la réponse';
      Exit;
    end;

    // Vérifier l'état pour la protection CSRF
    State := QueryParams.Values['state'];
    if State <> GetStoredState then
    begin
      FLastError := 'État invalide, possible tentative CSRF';
      Exit;
    end;

    // Échanger le code contre un jeton d'accès
    Result := ExchangeCodeForToken(Code);
  finally
    QueryParams.Free;
  end;
end;

function TOAuth2API.ExchangeCodeForToken(const Code: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
begin
  Result := False;

  RESTClient := TRESTClient.Create(FTokenEndpoint);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Paramètres pour l'échange du code
    RESTRequest.Params.AddItem('grant_type', 'authorization_code', pkFORMDATA);
    RESTRequest.Params.AddItem('code', Code, pkFORMDATA);
    RESTRequest.Params.AddItem('redirect_uri', FRedirectURI, pkFORMDATA);
    RESTRequest.Params.AddItem('client_id', FClientID, pkFORMDATA);
    RESTRequest.Params.AddItem('client_secret', FClientSecret, pkFORMDATA);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JsonObj := RESTResponse.JSONValue as TJSONObject;

      // Stocker les jetons
      FAccessToken := JsonObj.GetValue<string>('access_token');

      if JsonObj.TryGetValue<string>('refresh_token', FRefreshToken) then
        SaveRefreshToken(FRefreshToken); // Stockage sécurisé

      if JsonObj.TryGetValue<Integer>('expires_in', var ExpiresIn) then
        FTokenExpirationTime := Now + (ExpiresIn / SecsPerDay);

      Result := True;
    end
    else
    begin
      FLastError := 'Erreur lors de l''échange du code: ' + RESTResponse.Content;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TOAuth2API.RefreshAccessToken: Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonObj: TJSONObject;
  StoredRefreshToken: string;
begin
  Result := False;

  // Récupérer le refresh token stocké de manière sécurisée
  StoredRefreshToken := GetStoredRefreshToken;
  if StoredRefreshToken = '' then
  begin
    FLastError := 'Aucun jeton de rafraîchissement disponible';
    Exit;
  end;

  RESTClient := TRESTClient.Create(FTokenEndpoint);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Paramètres pour le rafraîchissement du jeton
    RESTRequest.Params.AddItem('grant_type', 'refresh_token', pkFORMDATA);
    RESTRequest.Params.AddItem('refresh_token', StoredRefreshToken, pkFORMDATA);
    RESTRequest.Params.AddItem('client_id', FClientID, pkFORMDATA);
    RESTRequest.Params.AddItem('client_secret', FClientSecret, pkFORMDATA);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JsonObj := RESTResponse.JSONValue as TJSONObject;

      // Mettre à jour les jetons
      FAccessToken := JsonObj.GetValue<string>('access_token');

      if JsonObj.TryGetValue<string>('refresh_token', var NewRefreshToken) then
      begin
        FRefreshToken := NewRefreshToken;
        SaveRefreshToken(FRefreshToken);
      end;

      if JsonObj.TryGetValue<Integer>('expires_in', var ExpiresIn) then
        FTokenExpirationTime := Now + (ExpiresIn / SecsPerDay);

      Result := True;
    end
    else
    begin
      FLastError := 'Erreur lors du rafraîchissement du jeton: ' + RESTResponse.Content;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

function TOAuth2API.IsTokenValid: Boolean;
begin
  // Vérifier si le jeton est présent et non expiré
  Result := (FAccessToken <> '') and (FTokenExpirationTime > Now);
end;

function TOAuth2API.GetAccessToken: string;
begin
  // Si le jeton est expiré mais qu'un refresh token est disponible, rafraîchir le jeton
  if (not IsTokenValid) and (GetStoredRefreshToken <> '') then
    RefreshAccessToken;

  Result := FAccessToken;
end;

function TOAuth2API.ExecuteRequest(const URL: string; Method: TRESTRequestMethod = rmGET;
                                 const Body: string = ''): TJSONValue;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := nil;

  // Vérifier si nous avons un jeton valide
  if GetAccessToken = '' then
  begin
    FLastError := 'Aucun jeton d''accès valide disponible';
    Exit;
  end;

  RESTClient := TRESTClient.Create(URL);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := Method;

    // Ajouter le jeton d'autorisation
    RESTRequest.Params.AddHeader('Authorization', 'Bearer ' + FAccessToken);

    // Ajouter le corps si présent
    if Body <> '' then
    begin
      RESTRequest.Body.Add(Body);
      RESTRequest.Params.AddHeader('Content-Type', 'application/json');
    end;

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      // Cloner la valeur pour la retourner
      Result := RESTResponse.JSONValue.Clone as TJSONValue;
    end
    else if RESTResponse.StatusCode = 401 then
    begin
      // Jeton expiré, tenter de le rafraîchir et réessayer
      if RefreshAccessToken then
      begin
        // Mettre à jour l'en-tête d'autorisation
        RESTRequest.Params.ParameterByName('Authorization').Value := 'Bearer ' + FAccessToken;

        // Réessayer la requête
        RESTRequest.Execute;

        if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
          Result := RESTResponse.JSONValue.Clone as TJSONValue
        else
          FLastError := 'Erreur après rafraîchissement du jeton: ' + RESTResponse.Content;
      end;
    end
    else
    begin
      FLastError := 'Erreur HTTP ' + IntToStr(RESTResponse.StatusCode) + ': ' + RESTResponse.Content;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### Utilisation de l'API OAuth 2.0 avec Google

Voici un exemple d'utilisation de l'API OAuth 2.0 avec Google :

```pascal
procedure TFrmGoogleIntegration.ButtonGoogleAuthClick(Sender: TObject);
begin
  GoogleOAuth := TOAuth2API.Create(
    GetApiKey('GoogleClientID'),
    GetApiKey('GoogleClientSecret'),
    'http://localhost:8080/oauth-callback',
    'https://accounts.google.com/o/oauth2/v2/auth',
    'https://oauth2.googleapis.com/token'
  );

  // Définir la portée pour accéder au profil et aux emails
  GoogleOAuth.Scope := 'profile email';

  // Démarrer le processus d'autorisation
  if GoogleOAuth.StartAuthorization then
    StatusLabel.Caption := 'Autorisation en cours...'
  else
    ShowMessage('Erreur: ' + GoogleOAuth.LastError);
end;

// Cette méthode serait appelée par votre serveur local lors de la redirection
procedure TFrmGoogleIntegration.HandleOAuthCallback(const URL: string);
begin
  if GoogleOAuth.HandleRedirect(URL) then
  begin
    StatusLabel.Caption := 'Authentification réussie !';

    // Récupérer les informations du profil
    var UserInfo := GoogleOAuth.ExecuteRequest('https://www.googleapis.com/oauth2/v2/userinfo');

    if Assigned(UserInfo) then
    begin
      try
        LabelName.Caption := 'Nom: ' + UserInfo.GetValue<string>('name');
        LabelEmail.Caption := 'Email: ' + UserInfo.GetValue<string>('email');

        // Afficher l'image de profil
        var PictureURL := UserInfo.GetValue<string>('picture');
        LoadImageFromURL(PictureURL, ImageProfile.Picture);

        // Activer les fonctionnalités Google
        EnableGoogleFeatures;
      finally
        UserInfo.Free;
      end;
    end;
  end
  else
  begin
    StatusLabel.Caption := 'Erreur d''authentification: ' + GoogleOAuth.LastError;
  end;
end;
```

### Création d'un mini-serveur HTTP pour OAuth

Pour gérer les redirections OAuth, vous pouvez créer un petit serveur HTTP local :

```pascal
unit OAuthServer;

interface

uses
  System.SysUtils, System.Classes, IdHTTPServer, IdCustomHTTPServer,
  IdContext, IdGlobal;

type
  TOAuthCallbackEvent = procedure(const URL: string) of object;

  TOAuthServer = class
  private
    FHTTPServer: TIdHTTPServer;
    FPort: Integer;
    FOnCallback: TOAuthCallbackEvent;

    procedure HTTPServerCommandGet(AContext: TIdContext;
                                  ARequestInfo: TIdHTTPRequestInfo;
                                  AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(Port: Integer = 8080);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Port: Integer read FPort write FPort;
    property OnCallback: TOAuthCallbackEvent read FOnCallback write FOnCallback;
  end;

implementation

constructor TOAuthServer.Create(Port: Integer = 8080);
begin
  inherited Create;

  FPort := Port;

  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := FPort;
  FHTTPServer.OnCommandGet := HTTPServerCommandGet;
end;

destructor TOAuthServer.Destroy;
begin
  Stop;
  FHTTPServer.Free;

  inherited;
end;

procedure TOAuthServer.Start;
begin
  if not FHTTPServer.Active then
  begin
    FHTTPServer.Bindings.Clear;
    FHTTPServer.Bindings.Add.Port := FPort;
    FHTTPServer.Active := True;
  end;
end;

procedure TOAuthServer.Stop;
begin
  if FHTTPServer.Active then
    FHTTPServer.Active := False;
end;

procedure TOAuthServer.HTTPServerCommandGet(AContext: TIdContext;
                                          ARequestInfo: TIdHTTPRequestInfo;
                                          AResponseInfo: TIdHTTPResponseInfo);
var
  FullUrl: string;
  HtmlResponse: string;
begin
  // Construire l'URL complète avec les paramètres
  FullUrl := 'http://localhost:' + IntToStr(FPort) + ARequestInfo.URI;
  if ARequestInfo.QueryParams <> '' then
    FullUrl := FullUrl + '?' + ARequestInfo.QueryParams;

  // Pages spécifiques
  if ARequestInfo.URI = '/oauth-callback' then
  begin
    // Préparer une réponse HTML pour l'utilisateur
    HtmlResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head>' +
      '  <meta charset="UTF-8">' +
      '  <title>Authentification réussie</title>' +
      '  <style>' +
      '    body { font-family: Arial, sans-serif; text-align: center; margin: 40px; }' +
      '    .success { color: green; }' +
      '    .container { max-width: 600px; margin: 0 auto; }' +
      '  </style>' +
      '</head>' +
      '<body>' +
      '  <div class="container">' +
      '    <h1 class="success">Authentification réussie !</h1>' +
      '    <p>Vous pouvez maintenant fermer cette fenêtre et retourner à l\'application.</p>' +
      '  </div>' +
      '  <script>window.close();</script>' +
      '</body>' +
      '</html>';

    // Déclencher l'événement de callback
    if Assigned(FOnCallback) then
      FOnCallback(FullUrl);
  end
  else if ARequestInfo.URI = '/success' then
  begin
    HtmlResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head>' +
      '  <meta charset="UTF-8">' +
      '  <title>Opération réussie</title>' +
      '  <style>' +
      '    body { font-family: Arial, sans-serif; text-align: center; margin: 40px; }' +
      '    .success { color: green; }' +
      '    .container { max-width: 600px; margin: 0 auto; }' +
      '  </style>' +
      '</head>' +
      '<body>' +
      '  <div class="container">' +
      '    <h1 class="success">Opération réussie !</h1>' +
      '    <p>Vous pouvez maintenant fermer cette fenêtre et retourner à l\'application.</p>' +
      '  </div>' +
      '  <script>window.close();</script>' +
      '</body>' +
      '</html>';

    // Déclencher l'événement de callback
    if Assigned(FOnCallback) then
      FOnCallback(FullUrl);
  end
  else if ARequestInfo.URI = '/cancel' then
  begin
    HtmlResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head>' +
      '  <meta charset="UTF-8">' +
      '  <title>Opération annulée</title>' +
      '  <style>' +
      '    body { font-family: Arial, sans-serif; text-align: center; margin: 40px; }' +
      '    .cancel { color: #cc0000; }' +
      '    .container { max-width: 600px; margin: 0 auto; }' +
      '  </style>' +
      '</head>' +
      '<body>' +
      '  <div class="container">' +
      '    <h1 class="cancel">Opération annulée</h1>' +
      '    <p>Vous avez annulé l\'opération. Vous pouvez maintenant fermer cette fenêtre.</p>' +
      '  </div>' +
      '  <script>window.close();</script>' +
      '</body>' +
      '</html>';

    // Déclencher l'événement de callback
    if Assigned(FOnCallback) then
      FOnCallback(FullUrl);
  end
  else
  begin
    // Page par défaut pour les autres requêtes
    HtmlResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head>' +
      '  <meta charset="UTF-8">' +
      '  <title>Serveur OAuth</title>' +
      '</head>' +
      '<body>' +
      '  <h1>Serveur OAuth</h1>' +
      '  <p>Ce serveur est utilisé pour gérer les redirections OAuth.</p>' +
      '</body>' +
      '</html>';
  end;

  // Envoyer la réponse HTML
  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.ContentText := HtmlResponse;
end;
```

### Utilisation du serveur OAuth

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer et démarrer le serveur OAuth
  FOAuthServer := TOAuthServer.Create(8080);
  FOAuthServer.OnCallback := HandleOAuthCallback;
  FOAuthServer.Start;

  // ... autres initialisations ...
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Arrêter le serveur OAuth
  if Assigned(FOAuthServer) then
  begin
    FOAuthServer.Stop;
    FOAuthServer.Free;
  end;

  // ... autres nettoyages ...
end;

procedure TForm1.HandleOAuthCallback(const URL: string);
begin
  // Traiter la redirection OAuth
  if GoogleOAuth.HandleRedirect(URL) then
  begin
    // Authentification réussie, charger les données de l'utilisateur
    LoadUserProfile;
  end
  else
  begin
    // Afficher l'erreur
    ShowMessage('Erreur d''authentification: ' + GoogleOAuth.LastError);
  end;
end;
```

## Architecture avancée pour les API REST

Pour des projets plus importants, il est recommandé d'utiliser une architecture plus structurée. Voici un exemple d'architecture en couches pour l'intégration d'API REST :

### 1. Couche de base (APIBase.pas)

```pascal
unit APIBase;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON;

type
  TAPIResult<T> = record
    Success: Boolean;
    Data: T;
    ErrorMessage: string;
    ErrorCode: Integer;
  end;

  IAPILogger = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure LogRequest(const Method, URL, Body: string);
    procedure LogResponse(const URL: string; StatusCode: Integer; const Body: string);
    procedure LogError(const URL: string; const ErrorMessage: string);
  end;

  TAPIBase = class
  private
    FBaseURL: string;
    FHeaders: TStringList;
    FLogger: IAPILogger;
    FLastError: string;
    FRetryCount: Integer;
    FRetryDelay: Integer;

    function ExecuteRequest(const Resource: string; Method: TRESTRequestMethod;
                           const Params: TStrings = nil;
                           const Body: string = ''): TJSONValue;
    function TryExecuteWithRetry(Request: TRESTRequest): Boolean;
  protected
    function PrepareRequest(const Resource: string): string; virtual;
    procedure AddAuthHeaders(Request: TRESTRequest); virtual;
    function HandleResponse(Response: TRESTResponse): TJSONValue; virtual;
  public
    constructor Create(const BaseURL: string);
    destructor Destroy; override;

    // Méthodes génériques REST
    function Get(const Resource: string; const Params: TStrings = nil): TJSONValue;
    function Post(const Resource: string; const Body: string; const Params: TStrings = nil): TJSONValue;
    function Put(const Resource: string; const Body: string; const Params: TStrings = nil): TJSONValue;
    function Delete(const Resource: string; const Params: TStrings = nil): TJSONValue;

    // Propriétés
    property BaseURL: string read FBaseURL write FBaseURL;
    property Headers: TStringList read FHeaders;
    property Logger: IAPILogger read FLogger write FLogger;
    property LastError: string read FLastError;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    property RetryDelay: Integer read FRetryDelay write FRetryDelay;
  end;

implementation

// Implémentation...
```

### 2. Couche service spécifique (WeatherService.pas)

```pascal
unit WeatherService;

interface

uses
  System.SysUtils, System.Classes, System.JSON, APIBase;

type
  TWeatherInfo = record
    City, CountryCode: string;
    Temperature, FeelsLike: Double;
    Humidity: Integer;
    WeatherCode, Description: string;
  end;

  TWeatherService = class(TAPIBase)
  private
    FApiKey: string;
  protected
    function PrepareRequest(const Resource: string): string; override;
  public
    constructor Create(const ApiKey: string);

    function GetCurrentWeather(const City: string): TAPIResult<TWeatherInfo>;
    function GetForecast(const City: string; Days: Integer): TAPIResult<TArray<TWeatherInfo>>;
  end;

implementation

constructor TWeatherService.Create(const ApiKey: string);
begin
  inherited Create('https://api.openweathermap.org/data/2.5');

  FApiKey := ApiKey;
  FRetryCount := 3;
  FRetryDelay := 1000;
end;

function TWeatherService.PrepareRequest(const Resource: string): string;
begin
  // Ajouter l'API key à toutes les requêtes
  Result := inherited PrepareRequest(Resource);

  if Pos('?', Result) > 0 then
    Result := Result + '&appid=' + FApiKey
  else
    Result := Result + '?appid=' + FApiKey;

  // Ajouter les paramètres communs
  Result := Result + '&units=metric&lang=fr';
end;

function TWeatherService.GetCurrentWeather(const City: string): TAPIResult<TWeatherInfo>;
var
  Params: TStringList;
  JsonObj: TJSONObject;
begin
  Result.Success := False;

  Params := TStringList.Create;
  try
    Params.Values['q'] := City;

    JsonObj := Get('weather', Params) as TJSONObject;

    if Assigned(JsonObj) then
    begin
      try
        Result.Success := True;

        // Remplir la structure TWeatherInfo avec les données JSON
        Result.Data.City := City;

        if JsonObj.TryGetValue<TJSONObject>('main', var MainObj) then
        begin
          MainObj.TryGetValue<Double>('temp', Result.Data.Temperature);
          MainObj.TryGetValue<Double>('feels_like', Result.Data.FeelsLike);
          MainObj.TryGetValue<Integer>('humidity', Result.Data.Humidity);
        end;

        if JsonObj.TryGetValue<TJSONArray>('weather', var WeatherArr) and
           (WeatherArr.Count > 0) then
        begin
          var WeatherObj := WeatherArr.Items[0] as TJSONObject;
          WeatherObj.TryGetValue<string>('description', Result.Data.Description);
          WeatherObj.TryGetValue<string>('icon', Result.Data.WeatherCode);
        end;

        if JsonObj.TryGetValue<TJSONObject>('sys', var SysObj) then
          SysObj.TryGetValue<string>('country', Result.Data.CountryCode);
      finally
        JsonObj.Free;
      end;
    end
    else
    begin
      Result.ErrorMessage := FLastError;
    end;
  finally
    Params.Free;
  end;
end;
```

### 3. Couche présentation (WeatherViewModel.pas)

```pascal
unit WeatherViewModel;

interface

uses
  System.SysUtils, System.Classes, System.Observable, WeatherService, APIBase;

type
  TWeatherViewModel = class(TObservable)
  private
    FWeatherService: TWeatherService;
    FCity: string;
    FTemperature: Double;
    FDescription: string;
    FHumidity: Integer;
    FIsLoading: Boolean;
    FErrorMessage: string;

    procedure SetCity(const Value: string);
    procedure SetTemperature(const Value: Double);
    procedure SetDescription(const Value: string);
    procedure SetHumidity(const Value: Integer);
    procedure SetIsLoading(const Value: Boolean);
    procedure SetErrorMessage(const Value: string);
  public
    constructor Create(const ApiKey: string);
    destructor Destroy; override;

    procedure LoadWeather;

    property City: string read FCity write SetCity;
    property Temperature: Double read FTemperature write SetTemperature;
    property Description: string read FDescription write SetDescription;
    property Humidity: Integer read FHumidity write SetHumidity;
    property IsLoading: Boolean read FIsLoading write SetIsLoading;
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
  end;

implementation

uses
  System.Threading;

constructor TWeatherViewModel.Create(const ApiKey: string);
begin
  inherited Create;

  FWeatherService := TWeatherService.Create(ApiKey);
end;

destructor TWeatherViewModel.Destroy;
begin
  FWeatherService.Free;

  inherited;
end;

procedure TWeatherViewModel.SetCity(const Value: string);
begin
  if FCity <> Value then
  begin
    FCity := Value;
    NotifyObservers('City');
  end;
end;

procedure TWeatherViewModel.SetTemperature(const Value: Double);
begin
  if FTemperature <> Value then
  begin
    FTemperature := Value;
    NotifyObservers('Temperature');
  end;
end;

procedure TWeatherViewModel.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    NotifyObservers('Description');
  end;
end;

procedure TWeatherViewModel.SetHumidity(const Value: Integer);
begin
  if FHumidity <> Value then
  begin
    FHumidity := Value;
    NotifyObservers('Humidity');
  end;
end;

procedure TWeatherViewModel.SetIsLoading(const Value: Boolean);
begin
  if FIsLoading <> Value then
  begin
    FIsLoading := Value;
    NotifyObservers('IsLoading');
  end;
end;

procedure TWeatherViewModel.SetErrorMessage(const Value: string);
begin
  if FErrorMessage <> Value then
  begin
    FErrorMessage := Value;
    NotifyObservers('ErrorMessage');
  end;
end;

procedure TWeatherViewModel.LoadWeather;
begin
  if FCity = '' then
  begin
    ErrorMessage := 'Veuillez entrer le nom d''une ville';
    Exit;
  end;

  // Mettre à jour l'interface pour indiquer le chargement
  IsLoading := True;
  ErrorMessage := '';

  // Exécuter en arrière-plan
  TTask.Run(
    procedure
    var
      Result: TAPIResult<TWeatherInfo>;
    begin
      try
        // Appeler le service météo
        Result := FWeatherService.GetCurrentWeather(FCity);

        // Mettre à jour le modèle de vue sur le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            IsLoading := False;

            if Result.Success then
            begin
              Temperature := Result.Data.Temperature;
              Description := Result.Data.Description;
              Humidity := Result.Data.Humidity;
              ErrorMessage := '';
            end
            else
            begin
              ErrorMessage := Result.ErrorMessage;
            end;
          end
        );
      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              IsLoading := False;
              ErrorMessage := 'Exception: ' + E.Message;
            end
          );
        end;
      end;
    end
  );
end;
```

### 4. Couche vue (WeatherForm.pas)

```pascal
unit WeatherForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, WeatherViewModel, System.Observable;

type
  TFrmWeather = class(TForm, IObserver)
  private
    { Private declarations }
    FViewModel: TWeatherViewModel;

    procedure UpdateView;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Implémentation de IObserver
    procedure UpdateObserver(const ID: string; const Subject: IObservable);
  end;

var
  FrmWeather: TFrmWeather;

implementation

{$R *.dfm}

constructor TFrmWeather.Create(AOwner: TComponent);
begin
  inherited;

  // Créer le ViewModel
  FViewModel := TWeatherViewModel.Create(GetApiKey('OpenWeatherMap'));
  FViewModel.AddObserver(Self);

  // Initialiser l'interface
  UpdateView;
end;

destructor TFrmWeather.Destroy;
begin
  FViewModel.RemoveObserver(Self);
  FViewModel.Free;

  inherited;
end;

procedure TFrmWeather.UpdateObserver(const ID: string; const Subject: IObservable);
begin
  // Mettre à jour l'interface lorsque le modèle change
  UpdateView;
end;

procedure TFrmWeather.UpdateView;
begin
  // Mettre à jour les contrôles de l'interface
  EditCity.Text := FViewModel.City;

  if FViewModel.IsLoading then
  begin
    LabelStatus.Caption := 'Chargement...';
    LabelTemperature.Caption := '';
    LabelDescription.Caption := '';
    LabelHumidity.Caption := '';
  end
  else
  begin
    if FViewModel.ErrorMessage <> '' then
    begin
      LabelStatus.Caption := 'Erreur: ' + FViewModel.ErrorMessage;
      LabelTemperature.Caption := '';
      LabelDescription.Caption := '';
      LabelHumidity.Caption := '';
    end
    else
    begin
      LabelStatus.Caption := 'Météo à ' + FViewModel.City;
      LabelTemperature.Caption := FormatFloat('0.0°C', FViewModel.Temperature);
      LabelDescription.Caption := FViewModel.Description;
      LabelHumidity.Caption := 'Humidité: ' + IntToStr(FViewModel.Humidity) + '%';
    end;
  end;

  // Activer/désactiver le bouton selon l'état
  ButtonGetWeather.Enabled := not FViewModel.IsLoading;
end;

procedure TFrmWeather.ButtonGetWeatherClick(Sender: TObject);
begin
  // Mettre à jour le modèle
  FViewModel.City := EditCity.Text;

  // Déclencher le chargement
  FViewModel.LoadWeather;
end;
```

## Conclusion et bonnes pratiques finales

Maintenant que nous avons exploré en profondeur l'intégration des API REST dans Delphi, voici quelques conseils finaux pour vous aider à réussir vos projets :

### Résumé des meilleures pratiques

1. **Architecture** : Utilisez une architecture en couches (modèle-vue-contrôleur ou modèle-vue-vue modèle) pour séparer l'interface utilisateur, la logique métier et l'accès aux API.

2. **Sécurité** :
   - Protégez les identifiants et les jetons d'API
   - Utilisez HTTPS pour toutes les communications
   - Évitez de stocker les informations sensibles en clair
   - Implémentez une authentification robuste

3. **Performance** :
   - Utilisez des requêtes asynchrones pour maintenir la réactivité de l'interface
   - Implémentez la mise en cache des réponses
   - Regroupez les requêtes lorsque c'est possible
   - Utilisez des techniques comme la pagination pour les grandes quantités de données

4. **Robustesse** :
   - Implémentez des mécanismes de nouvelle tentative avec backoff exponentiel
   - Gérez correctement les erreurs et les cas limites
   - Validez toutes les entrées et sorties
   - Testez avec différentes conditions réseau

5. **Maintenance** :
   - Documentez vos intégrations d'API
   - Utilisez des classes d'encapsulation pour chaque API
   - Centralisez la configuration (endpoints, clés, etc.)
   - Mettez en place des journaux détaillés pour le débogage

6. **Conformité** :
   - Respectez les conditions d'utilisation des API
   - Suivez les exigences de protection des données (RGPD, etc.)
   - Respectez les limites de débit (rate limits)
   - Affichez les attributions requises

### Ressources supplémentaires

Pour continuer à développer vos compétences en intégration d'API REST avec Delphi, consultez ces ressources :

- **Blogs et forums** :
  - [Embarcadero Blog](https://blogs.embarcadero.com/)
  - [DelphiFeeds](https://www.delphifeeds.com/)
  - [Stack Overflow - Tag Delphi](https://stackoverflow.com/questions/tagged/delphi)

- **GitHub** :
  - Recherchez "Delphi REST" ou "Delphi API" pour trouver des projets et exemples

- **Livres** :
  - "Delphi Cookbook" par Daniele Spinetti (contient des recettes pour les API REST)
  - "Hands-On Design Patterns with Delphi" par Primož Gabrijelčič

- **Outils** :
  - [Postman](https://www.postman.com/) - Pour tester les API
  - [Fiddler](https://www.telerik.com/fiddler) - Pour analyser le trafic HTTP
  - [Swagger UI](https://swagger.io/tools/swagger-ui/) - Pour explorer et tester les API documentées en OpenAPI

En suivant ces principes et en utilisant les outils appropriés, vous serez en mesure d'intégrer efficacement n'importe quelle API REST dans vos applications Delphi, ouvrant ainsi un monde de possibilités pour vos projets.

N'oubliez pas que l'intégration d'API est un processus itératif - commencez par des intégrations simples, puis affinez et améliorez au fil du temps. Bonne programmation !
