🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.5 Intégration avec des services tiers

## Introduction aux services tiers

### Qu'est-ce qu'un service tiers ?

Un **service tiers** est une application ou un service web externe que vous pouvez utiliser dans votre propre application. C'est comme utiliser les services d'un expert : au lieu de tout créer vous-même, vous faites appel à des spécialistes.

**Exemples courants :**
- **Google Maps** : Afficher des cartes et calculer des itinéraires
- **Stripe/PayPal** : Traiter des paiements en ligne
- **Twilio** : Envoyer des SMS
- **SendGrid** : Envoyer des emails en masse
- **OpenAI** : Intelligence artificielle et traitement du langage
- **Weather API** : Obtenir des prévisions météo
- **Firebase** : Base de données en temps réel et authentification

### Pourquoi utiliser des services tiers ?

**Gain de temps** : Pas besoin de réinventer la roue. Des fonctionnalités complexes sont prêtes à l'emploi.

**Expertise** : Utiliser des solutions développées et maintenues par des spécialistes.

**Mise à l'échelle** : Les services tiers gèrent la montée en charge pour vous.

**Coût** : Souvent moins cher que de développer et maintenir soi-même.

**Mises à jour** : Les services sont constamment améliorés sans effort de votre part.

### Types d'intégration

**API REST** : Le plus courant, communication via HTTP avec JSON.

**API SOAP** : Plus ancien, utilise XML (encore utilisé dans certaines entreprises).

**WebHooks** : Le service vous envoie des notifications quand quelque chose se passe.

**SDK** : Bibliothèques spécifiques fournies par le service.

**OAuth** : Authentification via un service tiers (Se connecter avec Google, Facebook...).

## Authentification avec les services tiers

### Types d'authentification

#### API Key (Clé API)

La méthode la plus simple : une chaîne unique qui identifie votre application.

```pascal
uses
  System.Net.HttpClient;

procedure AppelerAPIAvecCle;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    URL := 'https://api.example.com/data?apikey=VOTRE_CLE_API';
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
      ShowMessage(Response.ContentAsString)
    else
      ShowMessage('Erreur: ' + IntToStr(Response.StatusCode));
  finally
    HttpClient.Free;
  end;
end;
```

**Important :** Ne jamais inclure votre clé API directement dans le code source. Utilisez un fichier de configuration ou des variables d'environnement.

#### Bearer Token

Un token qui est envoyé dans l'en-tête HTTP :

```pascal
procedure AppelerAPIAvecToken;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    // Ajouter le token dans l'en-tête
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer VOTRE_TOKEN');

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);

    if Response.StatusCode = 200 then
      ShowMessage(Response.ContentAsString);
  finally
    HttpClient.Free;
  end;
end;
```

#### Basic Authentication

Authentification avec nom d'utilisateur et mot de passe :

```pascal
uses
  System.NetEncoding;

function CreerHeaderBasicAuth(const Username, Password: string): string;  
var  
  Credentials: string;
begin
  Credentials := Username + ':' + Password;
  Result := 'Basic ' + TNetEncoding.Base64.Encode(Credentials);
end;

procedure AppelerAPIAvecBasicAuth;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization',
      CreerHeaderBasicAuth('utilisateur', 'motdepasse'));

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);

    ShowMessage(Response.ContentAsString);
  finally
    HttpClient.Free;
  end;
end;
```

### Stockage sécurisé des identifiants

```pascal
// Mauvaise pratique : Ne JAMAIS faire cela
const
  API_KEY = 'ma_cle_secrete_123';  // Visible dans le code !

// Bonne pratique : Utiliser un fichier de configuration
uses
  System.IniFiles;

function ChargerCleAPI: string;  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    Result := IniFile.ReadString('API', 'Key', '');
    if Result = '' then
      raise Exception.Create('Clé API non configurée');
  finally
    IniFile.Free;
  end;
end;

// Fichier config.ini :
// [API]
// Key=votre_cle_ici
```

## Services de géolocalisation

### API Google Maps

#### Obtenir les coordonnées d'une adresse (Geocoding)

```pascal
uses
  System.Net.HttpClient, System.JSON;

function GeocoderAdresse(const Adresse: string; out Latitude, Longitude: Double): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse, JSONLocation: TJSONObject;
  JSONResults: TJSONArray;
  URL, APIKey: string;
begin
  Result := False;
  APIKey := ChargerCleAPI; // Charger depuis config

  // URL encoder l'adresse
  URL := Format('https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s',
    [TNetEncoding.URL.Encode(Adresse), APIKey]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        if JSONResponse.GetValue<string>('status') = 'OK' then
        begin
          JSONResults := JSONResponse.GetValue<TJSONArray>('results');
          if JSONResults.Count > 0 then
          begin
            JSONLocation := (JSONResults.Items[0] as TJSONObject)
              .GetValue<TJSONObject>('geometry')
              .GetValue<TJSONObject>('location');

            Latitude := JSONLocation.GetValue<Double>('lat');
            Longitude := JSONLocation.GetValue<Double>('lng');
            Result := True;
          end;
        end;
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonGeocoderClick(Sender: TObject);  
var  
  Lat, Lng: Double;
begin
  if GeocoderAdresse('Tour Eiffel, Paris', Lat, Lng) then
    ShowMessage(Format('Latitude: %.6f, Longitude: %.6f', [Lat, Lng]))
  else
    ShowMessage('Adresse non trouvée');
end;
```

#### Calculer la distance entre deux points

```pascal
function CalculerDistanceGoogleMaps(const Origine, Destination: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse: TJSONObject;
  JSONRows, JSONElements: TJSONArray;
  URL, APIKey: string;
begin
  Result := '';
  APIKey := ChargerCleAPI;

  URL := Format('https://maps.googleapis.com/maps/api/distancematrix/json?origins=%s&destinations=%s&key=%s',
    [TNetEncoding.URL.Encode(Origine),
     TNetEncoding.URL.Encode(Destination),
     APIKey]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        JSONRows := JSONResponse.GetValue<TJSONArray>('rows');
        JSONElements := (JSONRows.Items[0] as TJSONObject).GetValue<TJSONArray>('elements');

        Result := (JSONElements.Items[0] as TJSONObject)
          .GetValue<TJSONObject>('distance')
          .GetValue<string>('text');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

### API OpenStreetMap (Alternative gratuite)

```pascal
function GeocoderAvecOpenStreetMap(const Adresse: string;
  out Latitude, Longitude: Double): Boolean;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  URL: string;
begin
  Result := False;

  URL := 'https://nominatim.openstreetmap.org/search?format=json&q=' +
    TNetEncoding.URL.Encode(Adresse);

  HttpClient := THTTPClient.Create;
  try
    // OpenStreetMap demande un User-Agent
    HttpClient.UserAgent := 'MonApplicationDelphi/1.0';

    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONArray := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONArray;
      try
        if JSONArray.Count > 0 then
        begin
          JSONObject := JSONArray.Items[0] as TJSONObject;
          Latitude := StrToFloat(JSONObject.GetValue<string>('lat'));
          Longitude := StrToFloat(JSONObject.GetValue<string>('lon'));
          Result := True;
        end;
      finally
        JSONArray.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Services météo

### API OpenWeatherMap

```pascal
type
  TMeteoInfo = record
    Ville: string;
    Temperature: Double;
    Description: string;
    Humidite: Integer;
    VitesseVent: Double;
  end;

function ObtenirMeteo(const Ville: string): TMeteoInfo;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse, JSONMain, JSONWeather, JSONWind: TJSONObject;
  JSONWeatherArray: TJSONArray;
  URL, APIKey: string;
begin
  APIKey := ChargerCleAPI;

  URL := Format('https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric&lang=fr',
    [TNetEncoding.URL.Encode(Ville), APIKey]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result.Ville := JSONResponse.GetValue<string>('name');

        JSONMain := JSONResponse.GetValue<TJSONObject>('main');
        Result.Temperature := JSONMain.GetValue<Double>('temp');
        Result.Humidite := JSONMain.GetValue<Integer>('humidity');

        JSONWeatherArray := JSONResponse.GetValue<TJSONArray>('weather');
        JSONWeather := JSONWeatherArray.Items[0] as TJSONObject;
        Result.Description := JSONWeather.GetValue<string>('description');

        JSONWind := JSONResponse.GetValue<TJSONObject>('wind');
        Result.VitesseVent := JSONWind.GetValue<Double>('speed');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonMeteoClick(Sender: TObject);  
var  
  Meteo: TMeteoInfo;
begin
  Meteo := ObtenirMeteo('Paris');
  Memo1.Lines.Add('Ville: ' + Meteo.Ville);
  Memo1.Lines.Add(Format('Température: %.1f°C', [Meteo.Temperature]));
  Memo1.Lines.Add('Description: ' + Meteo.Description);
  Memo1.Lines.Add(Format('Humidité: %d%%', [Meteo.Humidite]));
  Memo1.Lines.Add(Format('Vent: %.1f m/s', [Meteo.VitesseVent]));
end;
```

### Prévisions sur plusieurs jours

```pascal
function ObtenirPrevisions(const Ville: string; NbJours: Integer): TArray<TMeteoInfo>;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse, JSONItem, JSONMain, JSONWeather: TJSONObject;
  JSONList, JSONWeatherArray: TJSONArray;
  URL, APIKey: string;
  I: Integer;
begin
  APIKey := ChargerCleAPI;

  URL := Format('https://api.openweathermap.org/data/2.5/forecast?q=%s&appid=%s&units=metric&lang=fr',
    [TNetEncoding.URL.Encode(Ville), APIKey]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        JSONList := JSONResponse.GetValue<TJSONArray>('list');

        // Limiter au nombre de jours demandés (8 prévisions par jour)
        SetLength(Result, Min(JSONList.Count, NbJours * 8));

        for I := 0 to High(Result) do
        begin
          JSONItem := JSONList.Items[I] as TJSONObject;

          Result[I].Ville := Ville;

          JSONMain := JSONItem.GetValue<TJSONObject>('main');
          Result[I].Temperature := JSONMain.GetValue<Double>('temp');
          Result[I].Humidite := JSONMain.GetValue<Integer>('humidity');

          JSONWeatherArray := JSONItem.GetValue<TJSONArray>('weather');
          JSONWeather := JSONWeatherArray.Items[0] as TJSONObject;
          Result[I].Description := JSONWeather.GetValue<string>('description');
        end;
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Services de paiement

### Stripe (Traitement de paiements)

#### Créer un paiement

```pascal
uses
  System.Net.HttpClient, System.JSON;

function CreerPaiementStripe(Montant: Integer; const Devise, Token: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  PostData: TStringStream;
  SecretKey: string;
begin
  Result := False;
  SecretKey := ChargerCleAPI; // Clé secrète Stripe

  HttpClient := THTTPClient.Create;
  PostData := TStringStream.Create('');
  try
    // En-têtes
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + SecretKey);

    // Données POST
    PostData.WriteString(Format('amount=%d&currency=%s&source=%s',
      [Montant, Devise, Token]));
    PostData.Position := 0;

    // Envoyer la requête
    Response := HttpClient.Post('https://api.stripe.com/v1/charges',
      PostData, nil, Headers);

    Result := Response.StatusCode = 200;

    if Result then
      ShowMessage('Paiement réussi !')
    else
      ShowMessage('Erreur paiement: ' + Response.ContentAsString);
  finally
    PostData.Free;
    HttpClient.Free;
  end;
end;

// Utilisation (montant en centimes)
procedure TForm1.ButtonPayerClick(Sender: TObject);  
begin  
  // 2500 centimes = 25.00 EUR
  if CreerPaiementStripe(2500, 'eur', 'tok_visa') then
    ShowMessage('Paiement de 25€ effectué');
end;
```

#### Vérifier le statut d'un paiement

```pascal
function VerifierStatutPaiement(const ChargeID: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  JSONResponse: TJSONObject;
  SecretKey: string;
begin
  Result := 'inconnu';
  SecretKey := ChargerCleAPI;

  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + SecretKey);

    Response := HttpClient.Get('https://api.stripe.com/v1/charges/' + ChargeID,
      nil, Headers);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := JSONResponse.GetValue<string>('status');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

### PayPal (Alternative)

```pascal
function ObtenirTokenPayPal: string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  PostData: TStringStream;
  JSONResponse: TJSONObject;
  ClientID, Secret: string;
begin
  ClientID := 'votre_client_id';
  Secret := 'votre_secret';

  HttpClient := THTTPClient.Create;
  PostData := TStringStream.Create('grant_type=client_credentials');
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization',
      CreerHeaderBasicAuth(ClientID, Secret));

    Response := HttpClient.Post(
      'https://api.sandbox.paypal.com/v1/oauth2/token',
      PostData, nil, Headers);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := JSONResponse.GetValue<string>('access_token');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    PostData.Free;
    HttpClient.Free;
  end;
end;
```

## Services de communication

### Twilio (Envoi de SMS)

```pascal
function EnvoyerSMS(const NumeroDestinataire, Message: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  PostData: TStringStream;
  AccountSID, AuthToken, NumeroTwilio: string;
  URL: string;
begin
  Result := False;

  AccountSID := 'votre_account_sid';
  AuthToken := 'votre_auth_token';
  NumeroTwilio := '+33123456789'; // Votre numéro Twilio

  URL := Format('https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json',
    [AccountSID]);

  HttpClient := THTTPClient.Create;
  PostData := TStringStream.Create('');
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization',
      CreerHeaderBasicAuth(AccountSID, AuthToken));

    PostData.WriteString(Format('From=%s&To=%s&Body=%s',
      [NumeroTwilio, NumeroDestinataire, TNetEncoding.URL.Encode(Message)]));
    PostData.Position := 0;

    Response := HttpClient.Post(URL, PostData, nil, Headers);
    Result := Response.StatusCode = 201;

    if Result then
      ShowMessage('SMS envoyé avec succès')
    else
      ShowMessage('Erreur: ' + Response.ContentAsString);
  finally
    PostData.Free;
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonEnvoyerSMSClick(Sender: TObject);  
begin  
  EnvoyerSMS('+33612345678', 'Bonjour depuis Delphi !');
end;
```

### SendGrid (Envoi d'emails en masse)

```pascal
function EnvoyerEmailSendGrid(const Destinataire, Sujet, Contenu: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  JSONBody, JSONPersonalization, JSONContent: TJSONObject;
  JSONPersonalizations, JSONTo, JSONContents: TJSONArray;
  PostData: TStringStream;
  APIKey: string;
begin
  Result := False;
  APIKey := ChargerCleAPI;

  // Construire le JSON
  JSONBody := TJSONObject.Create;
  try
    // Personnalisations
    JSONPersonalizations := TJSONArray.Create;
    JSONPersonalization := TJSONObject.Create;
    JSONTo := TJSONArray.Create;
    JSONTo.Add(TJSONObject.Create.AddPair('email', Destinataire));
    JSONPersonalization.AddPair('to', JSONTo);
    JSONPersonalizations.Add(JSONPersonalization);
    JSONBody.AddPair('personalizations', JSONPersonalizations);

    // Expéditeur
    JSONBody.AddPair('from', TJSONObject.Create.AddPair('email', 'votre@email.com'));

    // Sujet
    JSONBody.AddPair('subject', Sujet);

    // Contenu
    JSONContents := TJSONArray.Create;
    JSONContent := TJSONObject.Create;
    JSONContent.AddPair('type', 'text/plain');
    JSONContent.AddPair('value', Contenu);
    JSONContents.Add(JSONContent);
    JSONBody.AddPair('content', JSONContents);

    // Envoyer
    HttpClient := THTTPClient.Create;
    PostData := TStringStream.Create(JSONBody.ToString, TEncoding.UTF8);
    try
      SetLength(Headers, 2);
      Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + APIKey);
      Headers[1] := TNetHeader.Create('Content-Type', 'application/json');

      Response := HttpClient.Post('https://api.sendgrid.com/v3/mail/send',
        PostData, nil, Headers);

      Result := Response.StatusCode = 202;
    finally
      PostData.Free;
      HttpClient.Free;
    end;
  finally
    JSONBody.Free;
  end;
end;
```

## Intelligence Artificielle

### OpenAI (ChatGPT)

```pascal
function InterrogerChatGPT(const Question: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  JSONRequest, JSONResponse, JSONChoice, JSONMessage: TJSONObject;
  JSONMessages, JSONChoices: TJSONArray;
  PostData: TStringStream;
  APIKey: string;
begin
  Result := '';
  APIKey := ChargerCleAPI;

  // Construire la requête JSON
  JSONRequest := TJSONObject.Create;
  try
    JSONRequest.AddPair('model', 'gpt-3.5-turbo');

    JSONMessages := TJSONArray.Create;
    JSONMessage := TJSONObject.Create;
    JSONMessage.AddPair('role', 'user');
    JSONMessage.AddPair('content', Question);
    JSONMessages.Add(JSONMessage);
    JSONRequest.AddPair('messages', JSONMessages);

    JSONRequest.AddPair('temperature', TJSONNumber.Create(0.7));

    HttpClient := THTTPClient.Create;
    PostData := TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8);
    try
      SetLength(Headers, 2);
      Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + APIKey);
      Headers[1] := TNetHeader.Create('Content-Type', 'application/json');

      Response := HttpClient.Post('https://api.openai.com/v1/chat/completions',
        PostData, nil, Headers);

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          JSONChoices := JSONResponse.GetValue<TJSONArray>('choices');
          JSONChoice := JSONChoices.Items[0] as TJSONObject;
          JSONMessage := JSONChoice.GetValue<TJSONObject>('message');
          Result := JSONMessage.GetValue<string>('content');
        finally
          JSONResponse.Free;
        end;
      end;
    finally
      PostData.Free;
      HttpClient.Free;
    end;
  finally
    JSONRequest.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonChatGPTClick(Sender: TObject);  
var  
  Question, Reponse: string;
begin
  Question := EditQuestion.Text;
  Reponse := InterrogerChatGPT(Question);
  Memo1.Lines.Add('Q: ' + Question);
  Memo1.Lines.Add('R: ' + Reponse);
  Memo1.Lines.Add('---');
end;
```

### Génération d'images avec DALL-E

```pascal
function GenererImageDALLE(const Description: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  JSONRequest, JSONResponse, JSONData: TJSONObject;
  JSONDataArray: TJSONArray;
  PostData: TStringStream;
  APIKey, URLImage: string;
begin
  Result := '';
  APIKey := ChargerCleAPI;

  JSONRequest := TJSONObject.Create;
  try
    JSONRequest.AddPair('prompt', Description);
    JSONRequest.AddPair('n', TJSONNumber.Create(1));
    JSONRequest.AddPair('size', '1024x1024');

    HttpClient := THTTPClient.Create;
    PostData := TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8);
    try
      SetLength(Headers, 2);
      Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + APIKey);
      Headers[1] := TNetHeader.Create('Content-Type', 'application/json');

      Response := HttpClient.Post('https://api.openai.com/v1/images/generations',
        PostData, nil, Headers);

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          JSONDataArray := JSONResponse.GetValue<TJSONArray>('data');
          JSONData := JSONDataArray.Items[0] as TJSONObject;
          Result := JSONData.GetValue<string>('url');
        finally
          JSONResponse.Free;
        end;
      end;
    finally
      PostData.Free;
      HttpClient.Free;
    end;
  finally
    JSONRequest.Free;
  end;
end;
```

## Services de stockage cloud

### Dropbox

```pascal
function TelechargerVersDropbox(const CheminLocal: string; const CheminDropbox: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  FileStream: TFileStream;
  Token: string;
begin
  Result := False;
  Token := ChargerCleAPI;

  if not FileExists(CheminLocal) then
  begin
    ShowMessage('Fichier introuvable');
    Exit;
  end;

  HttpClient := THTTPClient.Create;
  FileStream := TFileStream.Create(CheminLocal, fmOpenRead);
  try
    SetLength(Headers, 3);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + Token);
    Headers[1] := TNetHeader.Create('Content-Type', 'application/octet-stream');
    Headers[2] := TNetHeader.Create('Dropbox-API-Arg',
      Format('{"path":"%s","mode":"add","autorename":true}', [CheminDropbox]));

    Response := HttpClient.Post('https://content.dropboxapi.com/2/files/upload',
      FileStream, nil, Headers);

    Result := Response.StatusCode = 200;

    if Result then
      ShowMessage('Fichier téléchargé sur Dropbox')
    else
      ShowMessage('Erreur: ' + Response.ContentAsString);
  finally
    FileStream.Free;
    HttpClient.Free;
  end;
end;
```

### Google Drive

```pascal
// Note: Nécessite OAuth 2.0 (voir section suivante)
function TelechargerVersGoogleDrive(const CheminLocal, NomFichier: string;
  const AccessToken: string): Boolean;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  MultipartData: TMultipartFormData;
begin
  Result := False;

  HttpClient := THTTPClient.Create;
  MultipartData := TMultipartFormData.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + AccessToken);

    MultipartData.AddField('name', NomFichier);
    MultipartData.AddFile('file', CheminLocal);

    Response := HttpClient.Post(
      'https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart',
      MultipartData, nil, Headers);

    Result := Response.StatusCode = 200;
  finally
    MultipartData.Free;
    HttpClient.Free;
  end;
end;
```

## OAuth 2.0 (Authentification tierce)

### Principe de base

OAuth 2.0 permet à vos utilisateurs de se connecter via Google, Facebook, Microsoft, etc. sans partager leur mot de passe avec votre application.

**Flux simplifié :**
1. Rediriger l'utilisateur vers le service (Google, Facebook...)
2. L'utilisateur autorise votre application
3. Le service renvoie un code d'autorisation
4. Échanger ce code contre un token d'accès
5. Utiliser le token pour accéder aux données

### Exemple avec Google OAuth

```pascal
type
  TOAuthHelper = class
  private
    FClientID: string;
    FClientSecret: string;
    FRedirectURI: string;
  public
    constructor Create(const ClientID, ClientSecret, RedirectURI: string);
    function GenererURLAutorisation: string;
    function EchangerCodeContreToken(const Code: string): string;
    function ObtenirInfoUtilisateur(const AccessToken: string): TJSONObject;
  end;

constructor TOAuthHelper.Create(const ClientID, ClientSecret, RedirectURI: string);  
begin  
  FClientID := ClientID;
  FClientSecret := ClientSecret;
  FRedirectURI := RedirectURI;
end;

function TOAuthHelper.GenererURLAutorisation: string;  
begin  
  Result := 'https://accounts.google.com/o/oauth2/v2/auth?' +
    'client_id=' + FClientID +
    '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectURI) +
    '&response_type=code' +
    '&scope=email%20profile';
end;

function TOAuthHelper.EchangerCodeContreToken(const Code: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  PostData: TStringStream;
  JSONResponse: TJSONObject;
begin
  Result := '';

  HttpClient := THTTPClient.Create;
  PostData := TStringStream.Create('');
  try
    PostData.WriteString(Format('code=%s&client_id=%s&client_secret=%s&redirect_uri=%s&grant_type=authorization_code',
      [Code, FClientID, FClientSecret, TNetEncoding.URL.Encode(FRedirectURI)]));
    PostData.Position := 0;

    Response := HttpClient.Post('https://oauth2.googleapis.com/token', PostData);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := JSONResponse.GetValue<string>('access_token');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    PostData.Free;
    HttpClient.Free;
  end;
end;

function TOAuthHelper.ObtenirInfoUtilisateur(const AccessToken: string): TJSONObject;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  Result := nil;

  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + AccessToken);

    Response := HttpClient.Get('https://www.googleapis.com/oauth2/v2/userinfo',
      nil, Headers);

    if Response.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
  finally
    HttpClient.Free;
  end;
end;
```

## Gestion des erreurs et limites de taux

### Gérer les codes d'erreur HTTP

```pascal
procedure TraiterReponseHTTP(Response: IHTTPResponse);  
begin  
  case Response.StatusCode of
    200..299:
      ShowMessage('Succès');

    400:
      ShowMessage('Requête invalide');

    401:
      ShowMessage('Non autorisé - vérifiez vos identifiants');

    403:
      ShowMessage('Accès interdit');

    404:
      ShowMessage('Ressource introuvable');

    429:
      ShowMessage('Trop de requêtes - attendez avant de réessayer');

    500..599:
      ShowMessage('Erreur serveur');
  else
    ShowMessage('Erreur inconnue: ' + IntToStr(Response.StatusCode));
  end;
end;
```

### Respecter les limites de taux (Rate Limiting)

```pascal
type
  TRateLimiter = class
  private
    FDerniereRequete: TDateTime;
    FDelaiMinimum: Integer; // en millisecondes
  public
    constructor Create(DelaiMinimumMS: Integer);
    procedure Attendre;
  end;

constructor TRateLimiter.Create(DelaiMinimumMS: Integer);  
begin  
  FDelaiMinimum := DelaiMinimumMS;
  FDerniereRequete := 0;
end;

procedure TRateLimiter.Attendre;  
var  
  Ecoule, AAttendre: Integer;
begin
  if FDerniereRequete > 0 then
  begin
    Ecoule := MilliSecondsBetween(Now, FDerniereRequete);
    if Ecoule < FDelaiMinimum then
    begin
      AAttendre := FDelaiMinimum - Ecoule;
      Sleep(AAttendre);
    end;
  end;
  FDerniereRequete := Now;
end;

// Utilisation
var
  Limiter: TRateLimiter;
begin
  Limiter := TRateLimiter.Create(1000); // 1 seconde entre chaque requête
  try
    Limiter.Attendre;
    // Faire la première requête

    Limiter.Attendre;
    // Faire la deuxième requête
  finally
    Limiter.Free;
  end;
end;
```

### Retry avec backoff exponentiel

```pascal
function RequeteAvecRetry(const URL: string; MaxTentatives: Integer): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Tentative: Integer;
  Delai: Integer;
begin
  Result := '';
  Delai := 1000; // Commencer avec 1 seconde

  HttpClient := THTTPClient.Create;
  try
    for Tentative := 1 to MaxTentatives do
    begin
      try
        Response := HttpClient.Get(URL);

        if Response.StatusCode = 200 then
        begin
          Result := Response.ContentAsString;
          Exit; // Succès !
        end
        else if Response.StatusCode = 429 then
        begin
          // Trop de requêtes, attendre plus longtemps
          Sleep(Delai);
          Delai := Delai * 2; // Backoff exponentiel
        end
        else
          Break; // Autre erreur, arrêter

      except
        on E: Exception do
        begin
          if Tentative = MaxTentatives then
            raise; // Relancer l'exception à la dernière tentative
          Sleep(Delai);
          Delai := Delai * 2;
        end;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Bonnes pratiques

### Créer une classe wrapper réutilisable

```pascal
type
  TServiceAPIBase = class
  private
    FAPIKey: string;
    FBaseURL: string;
    FHttpClient: THTTPClient;
  protected
    function Get(const Endpoint: string): string;
    function Post(const Endpoint: string; const Data: string): string;
    function CreerHeaders: TNetHeaders; virtual;
  public
    constructor Create(const BaseURL, APIKey: string);
    destructor Destroy; override;
  end;

constructor TServiceAPIBase.Create(const BaseURL, APIKey: string);  
begin  
  FBaseURL := BaseURL;
  FAPIKey := APIKey;
  FHttpClient := THTTPClient.Create;
end;

destructor TServiceAPIBase.Destroy;  
begin  
  FHttpClient.Free;
  inherited;
end;

function TServiceAPIBase.CreerHeaders: TNetHeaders;  
begin  
  SetLength(Result, 1);
  Result[0] := TNetHeader.Create('Authorization', 'Bearer ' + FAPIKey);
end;

function TServiceAPIBase.Get(const Endpoint: string): string;  
var  
  Response: IHTTPResponse;
begin
  Response := FHttpClient.Get(FBaseURL + Endpoint, nil, CreerHeaders);
  Result := Response.ContentAsString;
end;

// Utilisation :
type
  TMonServiceAPI = class(TServiceAPIBase)
  public
    function ObtenirDonnees: string;
  end;

function TMonServiceAPI.ObtenirDonnees: string;  
begin  
  Result := Get('/data');
end;
```

### Logging des requêtes

```pascal
procedure LogRequete(const Methode, URL: string; StatusCode: Integer);  
var  
  Log: TStringList;
begin
  Log := TStringList.Create;
  try
    if FileExists('api_log.txt') then
      Log.LoadFromFile('api_log.txt');

    Log.Add(Format('[%s] %s %s - Status: %d',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Methode, URL, StatusCode]));

    Log.SaveToFile('api_log.txt');
  finally
    Log.Free;
  end;
end;
```

### Cache des résultats

```pascal
type
  TCacheAPI = class
  private
    FCache: TDictionary<string, string>;
    FDureeCache: Integer; // en secondes
  public
    constructor Create(DureeCacheSecondes: Integer);
    destructor Destroy; override;
    function Obtenir(const Cle: string): string;
    procedure Stocker(const Cle, Valeur: string);
    procedure Vider;
  end;

// Utilisation pour éviter trop de requêtes
function ObtenirAvecCache(const URL: string): string;  
begin  
  Result := Cache.Obtenir(URL);
  if Result = '' then
  begin
    Result := FaireRequeteHTTP(URL);
    Cache.Stocker(URL, Result);
  end;
end;
```

## Résumé

L'intégration avec des services tiers ouvre des possibilités infinies pour vos applications.

**Points clés :**

1. **API REST** est le standard actuel avec JSON
2. **Authentification** : API Keys, Bearer Tokens, OAuth 2.0
3. **Sécurité** : Ne jamais coder les clés en dur, utilisez des fichiers de configuration
4. **Gestion d'erreurs** : Vérifiez toujours les codes HTTP
5. **Rate Limiting** : Respectez les limites des services
6. **Retry** : Implémentez des mécanismes de nouvelle tentative
7. **Cache** : Réduisez les appels avec un cache intelligent
8. **Logging** : Enregistrez toutes les requêtes pour le débogage
9. **Encapsulation** : Créez des wrappers réutilisables
10. **Documentation** : Consultez toujours la documentation officielle de l'API

Les services tiers évoluent constamment. Consultez leur documentation officielle pour les dernières fonctionnalités et bonnes pratiques.

⏭️ [Liaisons avec d'autres langages](/14-utilisation-dapi-et-bibliotheques-externes/06-liaisons-avec-dautres-langages.md)
