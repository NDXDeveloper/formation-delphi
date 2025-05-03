# 14.5 Intégration avec des services tiers

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Dans le monde moderne du développement logiciel, une application rarement fonctionne de manière isolée. L'intégration avec des services tiers permet d'enrichir considérablement vos applications Delphi en y ajoutant des fonctionnalités comme le stockage cloud, la géolocalisation, les paiements en ligne, l'authentification sociale, ou encore l'intelligence artificielle.

Ce chapitre vous guidera à travers les différentes méthodes pour intégrer des services tiers à vos applications Delphi, avec des exemples concrets et accessibles.

## Qu'est-ce qu'un service tiers ?

Un service tiers est une application, une API ou une plateforme externe à votre application, généralement accessible via Internet, qui fournit des fonctionnalités spécifiques que vous pouvez utiliser dans votre logiciel. Ces services sont généralement proposés par d'autres entreprises et peuvent être gratuits ou payants.

Exemples de services tiers populaires :
- Services d'authentification (Google, Facebook, Twitter)
- Services de stockage cloud (Dropbox, Google Drive, OneDrive)
- Services de paiement (PayPal, Stripe)
- Services de cartographie (Google Maps, OpenStreetMap)
- Plateformes d'analyse (Google Analytics)
- Services de messagerie (Twilio, SendGrid)

## Méthodes d'intégration

### 1. Intégration via API REST

La méthode la plus courante pour intégrer des services tiers est l'utilisation d'API REST. Une API REST utilise les méthodes HTTP (GET, POST, PUT, DELETE) pour interagir avec un service web.

#### Exemple : Utilisation de l'API REST OpenWeatherMap

Voici un exemple simple d'intégration du service météorologique OpenWeatherMap :

```pascal
procedure TForm1.ButtonGetWeatherClick(Sender: TObject);
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JsonObj: TJSONObject;
  ApiKey: string;
  City: string;
  Temperature: Double;
  WeatherDescription: string;
begin
  // Paramètres de l'API
  ApiKey := 'votre_clé_api_ici'; // Obtenir une clé sur openweathermap.org
  City := EditCity.Text;

  try
    // Création des composants REST
    Client := TRESTClient.Create(nil);
    Request := TRESTRequest.Create(nil);
    Response := TRESTResponse.Create(nil);

    try
      // Configuration des composants
      Client.BaseURL := 'https://api.openweathermap.org/data/2.5';
      Request.Client := Client;
      Request.Response := Response;

      // Configuration de la requête
      Request.Method := rmGET;
      Request.Resource := 'weather';

      // Ajout des paramètres
      Request.AddParameter('q', City);
      Request.AddParameter('appid', ApiKey);
      Request.AddParameter('units', 'metric'); // Pour avoir la température en Celsius

      // Exécution de la requête
      Request.Execute;

      // Traitement de la réponse
      if Response.StatusCode = 200 then
      begin
        JsonObj := Response.JSONValue as TJSONObject;

        // Extraction des données
        Temperature := JsonObj.GetValue<TJSONObject>('main').GetValue<Double>('temp');
        WeatherDescription := JsonObj.GetValue<TJSONArray>('weather')
                               .Items[0].GetValue<string>('description');

        // Affichage des résultats
        LabelWeather.Caption := Format(
          'Météo à %s : %.1f°C, %s',
          [City, Temperature, WeatherDescription]
        );
      end
      else
      begin
        ShowMessage('Erreur : ' + Response.StatusText);
      end;
    finally
      // Libération des ressources
      Client.Free;
      Request.Free;
      Response.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

Pour utiliser ce code, vous devez ajouter les unités suivantes :

```pascal
uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, REST.Client, REST.Types, System.JSON;
```

> **Note** : Vous devez créer un compte sur [OpenWeatherMap](https://openweathermap.org) pour obtenir une clé API gratuite.

### 2. Intégration via SDK (Software Development Kit)

Certains services fournissent des SDK spécifiques pour Delphi, ce qui facilite grandement l'intégration.

#### Installation d'un SDK tiers

Pour installer un SDK tiers dans Delphi :

1. Téléchargez le SDK depuis le site officiel du service ou depuis GitHub
2. Décompressez les fichiers dans un dossier
3. Dans Delphi, allez dans **Outils > Options > Bibliothèque**
4. Ajoutez le chemin vers les dossiers du SDK dans les chemins de bibliothèque
5. Pour les composants visuels, installez le package en allant dans **Composant > Installer des packages**

#### Exemple : Utilisation du SDK Dropbox

Si un SDK Dropbox pour Delphi est disponible (fictif pour cet exemple) :

```pascal
procedure TForm1.ButtonUploadClick(Sender: TObject);
var
  DropboxClient: TDropboxClient;
  FileToUpload: string;
  DestinationPath: string;
  Response: TDropboxResponse;
begin
  // Initialisation du client Dropbox
  DropboxClient := TDropboxClient.Create('votre_token_acces');

  try
    // Paramètres pour l'upload
    FileToUpload := 'C:\Documents\rapport.pdf';
    DestinationPath := '/Documents/rapport.pdf';

    // Upload du fichier
    Response := DropboxClient.UploadFile(FileToUpload, DestinationPath);

    // Vérification du résultat
    if Response.Success then
      ShowMessage('Fichier uploadé avec succès !')
    else
      ShowMessage('Erreur lors de l''upload : ' + Response.ErrorMessage);
  finally
    DropboxClient.Free;
  end;
end;
```

### 3. Intégration via WebView

Pour les services qui ne proposent pas d'API ou de SDK, vous pouvez utiliser un composant WebView pour intégrer directement leur interface web.

#### Exemple : Intégration de Google Maps via WebView

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  HTML: string;
begin
  // HTML pour intégrer Google Maps
  HTML :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <style>' +
    '    html, body, #map {' +
    '      height: 100%;' +
    '      margin: 0;' +
    '      padding: 0;' +
    '    }' +
    '  </style>' +
    '  <script src="https://maps.googleapis.com/maps/api/js?key=VOTRE_CLE_API"></script>' +
    '  <script>' +
    '    function initMap() {' +
    '      var paris = {lat: 48.8566, lng: 2.3522};' +
    '      var map = new google.maps.Map(document.getElementById("map"), {' +
    '        zoom: 12,' +
    '        center: paris' +
    '      });' +
    '      var marker = new google.maps.Marker({' +
    '        position: paris,' +
    '        map: map,' +
    '        title: "Paris"' +
    '      });' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body onload="initMap()">' +
    '  <div id="map"></div>' +
    '</body>' +
    '</html>';

  // Affichage de la carte dans le WebBrowser
  WebBrowser1.Navigate('about:blank');
  while WebBrowser1.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  (WebBrowser1.Document as IHTMLDocument2).write(HTML);
end;
```

Pour utiliser ce code, vous devez ajouter un composant TWebBrowser à votre formulaire et inclure les unités suivantes :

```pascal
uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.OleCtrls, SHDocVw, MSHTML;
```

> **Note** : Pour une application moderne, considérez l'utilisation du composant TEdgeBrowser disponible dans les versions récentes de Delphi.

## 4. Authentification et autorisation

La plupart des services tiers nécessitent une forme d'authentification. Les méthodes courantes sont :

### Authentification par clé API

C'est la méthode la plus simple : vous recevez une clé API que vous incluez dans chaque requête.

```pascal
Request.AddParameter('api_key', 'votre_clé_api_ici');
```

### Authentification OAuth 2.0

OAuth 2.0 est un protocole d'autorisation standard utilisé par de nombreux services (Google, Facebook, Twitter, etc.).

#### Exemple simplifié d'authentification OAuth 2.0

```pascal
procedure TForm1.ButtonAuthClick(Sender: TObject);
var
  AuthURL: string;
begin
  // Construction de l'URL d'authentification
  AuthURL := 'https://accounts.google.com/o/oauth2/auth' +
             '?client_id=VOTRE_CLIENT_ID' +
             '&redirect_uri=http://localhost:8080/callback' +
             '&response_type=code' +
             '&scope=profile email' +
             '&state=random_state_string';

  // Ouverture du navigateur pour l'authentification
  ShellExecute(0, 'open', PChar(AuthURL), nil, nil, SW_SHOWNORMAL);

  // Dans une application réelle, vous devriez :
  // 1. Démarrer un petit serveur web local pour recevoir le code d'autorisation
  // 2. Échanger ce code contre un token d'accès
  // 3. Utiliser ce token pour les requêtes API
end;
```

Pour une implémentation complète, vous auriez besoin de gérer un serveur local pour recevoir le callback et échanger le code d'autorisation contre un token d'accès.

## Exemples pratiques d'intégration de services populaires

### Intégration de PayPal pour les paiements

```pascal
procedure TForm1.ButtonPayWithPayPalClick(Sender: TObject);
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JsonObj: TJSONObject;
  Token: string;
  PaymentID: string;
  ApprovalURL: string;
begin
  // Création du token d'accès (simplifiée)
  Client := TRESTClient.Create('https://api.sandbox.paypal.com/v1/oauth2/token');
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    Request.Client := Client;
    Request.Response := Response;
    Request.Method := rmPOST;

    // Authentification Basic avec les identifiants Client
    Client.Authenticator := THTTPBasicAuthenticator.Create('votre_client_id', 'votre_secret');

    // Paramètres de la requête
    Request.AddParameter('grant_type', 'client_credentials');
    Request.Execute;

    // Extraction du token
    Token := Response.JSONValue.GetValue<string>('access_token');

    // Création d'un paiement
    Client.BaseURL := 'https://api.sandbox.paypal.com/v1';
    Client.Authenticator := nil;

    Request.Resource := 'payments/payment';
    Request.Method := rmPOST;
    Request.AddHeader('Authorization', 'Bearer ' + Token);
    Request.AddHeader('Content-Type', 'application/json');

    // Création du corps de la requête (simplifié)
    Request.Body.Add('{' +
      '"intent": "sale",' +
      '"payer": {' +
        '"payment_method": "paypal"' +
      '},' +
      '"transactions": [{' +
        '"amount": {' +
          '"total": "30.00",' +
          '"currency": "EUR"' +
        '},' +
        '"description": "Achat sur Mon Application"' +
      '}],' +
      '"redirect_urls": {' +
        '"return_url": "http://localhost:8080/success",' +
        '"cancel_url": "http://localhost:8080/cancel"' +
      '}' +
    '}');

    Request.Execute;

    // Traiter la réponse
    JsonObj := Response.JSONValue as TJSONObject;
    PaymentID := JsonObj.GetValue<string>('id');

    // Trouver l'URL d'approbation
    var Links := JsonObj.GetValue<TJSONArray>('links');
    for var i := 0 to Links.Count - 1 do
    begin
      var Link := Links.Items[i] as TJSONObject;
      if Link.GetValue<string>('rel') = 'approval_url' then
      begin
        ApprovalURL := Link.GetValue<string>('href');
        break;
      end;
    end;

    // Rediriger l'utilisateur vers la page de paiement PayPal
    ShellExecute(0, 'open', PChar(ApprovalURL), nil, nil, SW_SHOWNORMAL);

  finally
    Client.Free;
    Request.Free;
    Response.Free;
  end;
end;
```

### Intégration de Firebase pour l'authentification

```pascal
procedure TForm1.ButtonFirebaseLoginClick(Sender: TObject);
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JsonBody: TJSONObject;
  JsonResponse: TJSONObject;
  Email, Password: string;
  IdToken, RefreshToken: string;
begin
  Email := EditEmail.Text;
  Password := EditPassword.Text;

  Client := TRESTClient.Create(nil);
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    // Configuration des composants
    Client.BaseURL := 'https://identitytoolkit.googleapis.com/v1';
    Request.Client := Client;
    Request.Response := Response;

    // Configuration de la requête
    Request.Method := rmPOST;
    Request.Resource := 'accounts:signInWithPassword';

    // Ajout des paramètres
    Request.AddParameter('key', 'votre_cle_api_firebase');

    // Création du corps de la requête
    JsonBody := TJSONObject.Create;
    try
      JsonBody.AddPair('email', Email);
      JsonBody.AddPair('password', Password);
      JsonBody.AddPair('returnSecureToken', TJSONTrue.Create);

      Request.Body.Add(JsonBody.ToString);
      Request.Execute;

      // Traitement de la réponse
      if Response.StatusCode = 200 then
      begin
        JsonResponse := Response.JSONValue as TJSONObject;

        // Extraction des tokens
        IdToken := JsonResponse.GetValue<string>('idToken');
        RefreshToken := JsonResponse.GetValue<string>('refreshToken');

        // Stockage des tokens (à faire de manière sécurisée dans une application réelle)
        // ...

        ShowMessage('Connexion réussie !');

        // Navigation vers l'écran principal
        // ...
      end
      else
      begin
        ShowMessage('Erreur de connexion : ' + Response.Content);
      end;
    finally
      JsonBody.Free;
    end;
  finally
    Client.Free;
    Request.Free;
    Response.Free;
  end;
end;
```

## Bonnes pratiques pour l'intégration de services tiers

### 1. Sécurité

- Ne stockez jamais les clés API ou secrets dans votre code source
- Utilisez des variables d'environnement ou un fichier de configuration chiffré
- Utilisez HTTPS pour toutes les communications avec les services tiers
- Vérifiez et validez toutes les données reçues des services externes

```pascal
// Mauvaise pratique (à éviter)
const
  API_KEY = 'abc123secret';

// Bonne pratique
function GetApiKey: string;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    Result := IniFile.ReadString('API', 'Key', '');
    // Idéalement, déchiffrer la clé ici
  finally
    IniFile.Free;
  end;
end;
```

### 2. Gestion des erreurs

- Gérez toujours les erreurs de connexion et de réponse
- Implémentez des mécanismes de nouvelle tentative avec backoff exponentiel
- Fournissez des messages d'erreur clairs à l'utilisateur

```pascal
function TryAPIRequest(MaxRetries: Integer = 3): Boolean;
var
  RetryCount: Integer;
  DelayMs: Integer;
begin
  Result := False;
  RetryCount := 0;
  DelayMs := 1000; // Délai initial de 1 seconde

  while (RetryCount < MaxRetries) and (not Result) do
  begin
    try
      // Tentative de requête API
      Request.Execute;

      if Response.StatusCode = 200 then
      begin
        Result := True;
        Exit;
      end
      else if (Response.StatusCode >= 500) or (Response.StatusCode = 429) then
      begin
        // Erreur serveur ou limitation de débit, on réessaye
        Inc(RetryCount);
        Sleep(DelayMs);
        DelayMs := DelayMs * 2; // Backoff exponentiel
      end
      else
      begin
        // Autre type d'erreur, on abandonne
        ShowMessage('Erreur API : ' + Response.StatusText);
        Exit;
      end;
    except
      on E: Exception do
      begin
        Inc(RetryCount);
        Sleep(DelayMs);
        DelayMs := DelayMs * 2;

        if RetryCount >= MaxRetries then
          ShowMessage('Erreur de connexion après ' + IntToStr(MaxRetries) + ' tentatives : ' + E.Message);
      end;
    end;
  end;
end;
```

### 3. Performances

- Minimisez le nombre de requêtes au service tiers
- Mettez en cache les réponses lorsque c'est possible
- Utilisez des requêtes asynchrones pour ne pas bloquer l'interface utilisateur

```pascal
procedure TForm1.ButtonGetDataAsyncClick(Sender: TObject);
begin
  // Désactiver le bouton pendant le chargement
  ButtonGetDataAsync.Enabled := False;
  LabelStatus.Caption := 'Chargement en cours...';

  // Lancer la requête en arrière-plan
  TTask.Run(
    procedure
    var
      Client: TRESTClient;
      Request: TRESTRequest;
      Response: TRESTResponse;
      JsonData: TJSONValue;
    begin
      Client := TRESTClient.Create('https://api.exemple.com');
      Request := TRESTRequest.Create(nil);
      Response := TRESTResponse.Create(nil);

      try
        Request.Client := Client;
        Request.Response := Response;
        Request.Method := rmGET;
        Request.Resource := 'data';
        Request.Execute;

        JsonData := Response.JSONValue;

        // Mise à jour de l'interface sur le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            // Traiter les données...
            Memo1.Lines.Text := JsonData.ToString;

            ButtonGetDataAsync.Enabled := True;
            LabelStatus.Caption := 'Données chargées avec succès';
          end
        );
      finally
        Client.Free;
        Request.Free;
        Response.Free;
      end;
    end
  );
end;
```

Pour utiliser ce code, vous devez ajouter l'unité `System.Threading` à la clause `uses`.

### 4. Conformité aux conditions d'utilisation

- Respectez les quotas et limites d'utilisation des API
- Lisez attentivement les conditions d'utilisation du service
- Vérifiez si vous avez besoin d'afficher des attributions (ex: "Propulsé par Google Maps")

## Tests et débogage

### Utilisation d'outils de test d'API

Des outils comme Postman, Insomnia ou curl peuvent être très utiles pour tester les API avant de les intégrer dans votre code :

```bash
# Exemple de test d'API avec curl
curl -X GET "https://api.exemple.com/data" -H "Authorization: Bearer votre_token"
```

### Journalisation des requêtes et réponses

Pour faciliter le débogage, enregistrez les détails des requêtes et des réponses :

```pascal
procedure LogApiRequest(const Request: TRESTRequest; const Response: TRESTResponse);
var
  LogFile: TextFile;
  LogFileName: string;
begin
  LogFileName := ExtractFilePath(Application.ExeName) + 'api_log.txt';

  AssignFile(LogFile, LogFileName);

  if FileExists(LogFileName) then
    Append(LogFile)
  else
    Rewrite(LogFile);

  try
    WriteLn(LogFile, '--- ' + DateTimeToStr(Now) + ' ---');
    WriteLn(LogFile, 'URL: ' + Request.GetFullRequestURL);
    WriteLn(LogFile, 'Method: ' + Request.Method.ToString);

    WriteLn(LogFile, 'Headers:');
    for var Header in Request.Params do
      if Header.Kind = pkHTTPHEADER then
        WriteLn(LogFile, '  ' + Header.Name + ': ' + Header.Value);

    WriteLn(LogFile, 'Body: ' + Request.Body.ToString);

    WriteLn(LogFile, 'Response Status: ' + IntToStr(Response.StatusCode) + ' ' + Response.StatusText);
    WriteLn(LogFile, 'Response: ' + Response.Content);
    WriteLn(LogFile, '-------------------');
  finally
    CloseFile(LogFile);
  end;
end;
```

## Conclusion

L'intégration de services tiers dans vos applications Delphi ouvre un monde de possibilités, vous permettant d'ajouter rapidement des fonctionnalités avancées sans avoir à tout développer vous-même. Que ce soit pour l'authentification, le stockage cloud, les paiements, ou d'autres services spécialisés, Delphi offre tous les outils nécessaires pour une intégration harmonieuse.

Les points clés à retenir :

1. Utilisez les API REST pour la plupart des intégrations modernes
2. Considérez les SDK officiels lorsqu'ils sont disponibles
3. Utilisez les WebViews pour les services sans API accessible
4. Gérez correctement l'authentification et la sécurité
5. Implémentez une gestion robuste des erreurs
6. Respectez les bonnes pratiques de performance

En suivant ces principes, vous pourrez enrichir considérablement vos applications Delphi tout en profitant des services et infrastructures développés par des tiers.

## Ressources complémentaires

- Documentation officielle des services que vous intégrez
- Communauté Delphi et forums pour des exemples spécifiques
- Sites comme GitHub pour trouver des composants et bibliothèques d'intégration existants

N'oubliez pas que chaque service a ses propres particularités et que cette introduction n'est qu'un point de départ. Consultez toujours la documentation officielle du service que vous souhaitez intégrer pour les détails spécifiques à ce service.

⏭️ [Liaisons avec d'autres langages](/14-utilisation-dapi-et-bibliotheques-externes/06-liaisons-avec-dautres-langages.md)
