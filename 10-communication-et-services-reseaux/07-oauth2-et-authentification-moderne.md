🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.7 OAuth2 et authentification moderne

## Introduction à l'authentification moderne

### Qu'est-ce que l'authentification ?

L'**authentification** est le processus qui permet de vérifier l'identité d'un utilisateur. C'est la réponse à la question : "Qui êtes-vous ?".

**Analogie simple :**
Imaginez l'entrée d'un immeuble sécurisé. Vous montrez votre carte d'identité au gardien (authentification), qui vérifie votre identité. Une fois identifié, vous recevez un badge visiteur (token) qui vous permet d'accéder aux différents étages autorisés (autorisation).

### Méthodes d'authentification

**1. Authentification basique (Basic Auth)**
```
Username: jean.dupont
Password: MotDePasse123
```
- Simple mais peu sécurisée
- Mot de passe transmis à chaque requête
- Pas de contrôle granulaire des permissions

**2. Authentification par session**
```
1. Login → Serveur crée une session
2. Cookie de session stocké
3. Cookie envoyé à chaque requête
```
- Nécessite état côté serveur
- Difficile à scaler
- Problèmes avec les applications mobiles

**3. Authentification moderne (OAuth2 + JWT)**
```
1. Login → Serveur génère un token
2. Token stocké côté client
3. Token envoyé dans les en-têtes HTTP
```
- Sans état (stateless)
- Scalable
- Adapté aux API et applications mobiles

## Qu'est-ce qu'OAuth2 ?

### Définition

**OAuth2** (Open Authorization 2.0) est un protocole d'autorisation qui permet à une application d'accéder aux ressources d'un utilisateur sur un autre service, sans partager le mot de passe.

**Exemple concret :**
Vous voulez utiliser une application de gestion de photos qui peut publier sur votre compte Instagram. Au lieu de donner votre mot de passe Instagram à l'application (dangereux !), OAuth2 permet à Instagram de donner un "permis temporaire" à l'application, limité uniquement à la publication de photos.

### Les acteurs d'OAuth2

**1. Resource Owner (Propriétaire de la ressource)**
- L'utilisateur qui possède les données
- Exemple : Vous

**2. Client (L'application)**
- L'application qui veut accéder aux données
- Exemple : Votre application Delphi

**3. Authorization Server (Serveur d'autorisation)**
- Serveur qui authentifie l'utilisateur et délivre les tokens
- Exemple : Serveur Google OAuth

**4. Resource Server (Serveur de ressources)**
- Serveur qui héberge les données protégées
- Exemple : API Google Drive

**Schéma de fonctionnement :**
```
┌──────────────┐
│  Utilisateur │ (Resource Owner)
└──────┬───────┘
       │ 1. Veut utiliser l'application
       ▼
┌──────────────────┐
│  Application     │ (Client)
│  Delphi          │
└────────┬─────────┘
         │ 2. Demande autorisation
         ▼
┌──────────────────┐
│  Google OAuth    │ (Authorization Server)
│  Login           │
└────────┬─────────┘
         │ 3. Utilisateur s'authentifie et autorise
         │ 4. Renvoie un token d'accès
         ▼
┌──────────────────┐
│  Application     │
│  Delphi          │
└────────┬─────────┘
         │ 5. Utilise le token pour accéder
         ▼
┌──────────────────┐
│  Google Drive    │ (Resource Server)
│  API             │
└──────────────────┘
```

### Les flux OAuth2 (Grant Types)

OAuth2 définit plusieurs "flux" selon le type d'application :

**1. Authorization Code Flow (Recommandé pour applications web/desktop)**
```
User → Login → Auth Server renvoie CODE → App échange CODE contre TOKEN
```
- Le plus sécurisé
- Nécessite un client secret
- Le token n'est jamais exposé au navigateur

**2. Implicit Flow (Applications JavaScript - déprécié)**
```
User → Login → Auth Server renvoie directement TOKEN
```
- Moins sécurisé
- Token exposé dans l'URL
- Ne plus utiliser (remplacé par PKCE)

**3. Client Credentials Flow (Machine-to-Machine)**
```
App → Envoie Client ID + Secret → Reçoit TOKEN
```
- Pas d'utilisateur impliqué
- Pour services backend
- Communication serveur à serveur

**4. Resource Owner Password Flow (À éviter)**
```
User → Donne username/password à l'app → App obtient TOKEN
```
- L'application connaît le mot de passe
- Utilisé seulement si on contrôle les deux services

**5. Authorization Code + PKCE (Moderne, pour mobiles/desktop)**
```
User → Login avec code challenge → Vérifie code verifier → TOKEN
```
- Sécurisé sans client secret
- Parfait pour applications publiques
- Recommandé pour applications mobiles/desktop

## Les tokens OAuth2

### Access Token

Le **token d'accès** est une clé temporaire qui permet d'accéder aux ressources protégées.

**Exemple de token :**
```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkplYW4gRHVwb250IiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
```

**Caractéristiques :**
- Durée de vie courte (15 minutes à 1 heure)
- Contient les permissions (scopes)
- Envoyé dans chaque requête API
- Ne doit jamais être partagé

### Refresh Token

Le **token de rafraîchissement** permet d'obtenir un nouveau token d'accès sans redemander à l'utilisateur de se connecter.

**Caractéristiques :**
- Durée de vie longue (jours, semaines, mois)
- Stocké de manière sécurisée
- Utilisé uniquement pour obtenir de nouveaux access tokens
- Peut être révoqué

**Cycle de vie :**
```
1. Login → Access Token (1h) + Refresh Token (30 jours)
2. Utilise Access Token pendant 1h
3. Access Token expire
4. Utilise Refresh Token → Nouveau Access Token
5. Répéter jusqu'à expiration du Refresh Token
```

### JWT (JSON Web Tokens)

**JWT** est un format standard pour les tokens. C'est un objet JSON encodé et signé.

**Structure d'un JWT :**
```
Header.Payload.Signature

Header (type et algorithme):
{
  "alg": "HS256",
  "typ": "JWT"
}

Payload (données):
{
  "sub": "1234567890",
  "name": "Jean Dupont",
  "email": "jean.dupont@example.com",
  "exp": 1735689600,
  "iat": 1735686000,
  "scopes": ["read:profile", "write:posts"]
}

Signature (vérification d'intégrité):
HMACSHA256(
  base64UrlEncode(header) + "." +
  base64UrlEncode(payload),
  secret
)
```

## Implémentation OAuth2 dans Delphi

### Composants nécessaires

Delphi ne fournit pas de composants OAuth2 natifs, mais vous pouvez :
- Utiliser des composants tiers (TMS Security System, etc.)
- Implémenter vous-même avec TNetHTTPClient
- Utiliser des bibliothèques open source

### Implémenter OAuth2 manuellement

**Configuration OAuth2 :**

```pascal
unit OAuth2Config;

interface

type
  TOAuth2Config = class
  public
    // Configuration du provider
    ClientID: string;
    ClientSecret: string;
    RedirectURI: string;
    AuthorizationEndpoint: string;
    TokenEndpoint: string;
    Scopes: string;

    // Tokens
    AccessToken: string;
    RefreshToken: string;
    TokenType: string;
    ExpiresIn: Integer;
    ExpiresAt: TDateTime;

    // Méthodes
    function IsTokenExpired: Boolean;
    procedure SaveTokens(const Access, Refresh: string; ExpiresIn: Integer);
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

function TOAuth2Config.IsTokenExpired: Boolean;
begin
  Result := (AccessToken.IsEmpty) or (Now >= ExpiresAt);
end;

procedure TOAuth2Config.SaveTokens(const Access, Refresh: string; ExpiresIn: Integer);
begin
  AccessToken := Access;
  RefreshToken := Refresh;
  Self.ExpiresIn := ExpiresIn;
  ExpiresAt := IncSecond(Now, ExpiresIn - 60); // 1 minute de marge
end;

end.
```

### Authorization Code Flow

**Étape 1 : Rediriger vers la page d'autorisation**

```pascal
unit OAuth2Flow;

interface

uses
  System.SysUtils, System.Classes, OAuth2Config, Winapi.ShellAPI;

type
  TOAuth2Flow = class
  private
    FConfig: TOAuth2Config;
    FState: string; // Protection CSRF
    function GenerateState: string;
  public
    constructor Create(Config: TOAuth2Config);

    procedure StartAuthorizationFlow;
    function HandleCallback(const AuthorizationCode: string): Boolean;
    function RefreshAccessToken: Boolean;
  end;

implementation

uses
  System.NetEncoding, System.Net.HttpClient, System.JSON;

constructor TOAuth2Flow.Create(Config: TOAuth2Config);
begin
  inherited Create;
  FConfig := Config;
end;

function TOAuth2Flow.GenerateState: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

procedure TOAuth2Flow.StartAuthorizationFlow;
var
  AuthURL: string;
begin
  // Générer un state pour CSRF protection
  FState := GenerateState;

  // Construire l'URL d'autorisation
  AuthURL := FConfig.AuthorizationEndpoint +
    '?client_id=' + TNetEncoding.URL.Encode(FConfig.ClientID) +
    '&redirect_uri=' + TNetEncoding.URL.Encode(FConfig.RedirectURI) +
    '&response_type=code' +
    '&scope=' + TNetEncoding.URL.Encode(FConfig.Scopes) +
    '&state=' + FState;

  // Ouvrir dans le navigateur
  ShellExecute(0, 'open', PChar(AuthURL), nil, nil, SW_SHOWNORMAL);

  // L'utilisateur va s'authentifier dans le navigateur
  // Le serveur OAuth redirigera vers RedirectURI avec le code
end;

function TOAuth2Flow.HandleCallback(const AuthorizationCode: string): Boolean;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  RequestBody: TStringList;
  JSONResponse: TJSONObject;
  AccessToken, RefreshToken: string;
  ExpiresIn: Integer;
begin
  Result := False;

  HTTPClient := THTTPClient.Create;
  RequestBody := TStringList.Create;
  try
    // Préparer les paramètres
    RequestBody.Add('grant_type=authorization_code');
    RequestBody.Add('code=' + TNetEncoding.URL.Encode(AuthorizationCode));
    RequestBody.Add('redirect_uri=' + TNetEncoding.URL.Encode(FConfig.RedirectURI));
    RequestBody.Add('client_id=' + TNetEncoding.URL.Encode(FConfig.ClientID));
    RequestBody.Add('client_secret=' + TNetEncoding.URL.Encode(FConfig.ClientSecret));

    // Envoyer la requête
    HTTPClient.ContentType := 'application/x-www-form-urlencoded';
    Response := HTTPClient.Post(FConfig.TokenEndpoint, RequestBody);

    if Response.StatusCode = 200 then
    begin
      // Parser la réponse JSON
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        AccessToken := JSONResponse.GetValue<string>('access_token');
        ExpiresIn := JSONResponse.GetValue<Integer>('expires_in');

        // Le refresh token est optionnel
        if JSONResponse.TryGetValue<string>('refresh_token', RefreshToken) then
          FConfig.SaveTokens(AccessToken, RefreshToken, ExpiresIn)
        else
          FConfig.SaveTokens(AccessToken, '', ExpiresIn);

        Result := True;
      finally
        JSONResponse.Free;
      end;
    end;

  finally
    RequestBody.Free;
    HTTPClient.Free;
  end;
end;

function TOAuth2Flow.RefreshAccessToken: Boolean;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  RequestBody: TStringList;
  JSONResponse: TJSONObject;
  NewAccessToken, NewRefreshToken: string;
  ExpiresIn: Integer;
begin
  Result := False;

  if FConfig.RefreshToken.IsEmpty then
    Exit;

  HTTPClient := THTTPClient.Create;
  RequestBody := TStringList.Create;
  try
    // Préparer les paramètres
    RequestBody.Add('grant_type=refresh_token');
    RequestBody.Add('refresh_token=' + TNetEncoding.URL.Encode(FConfig.RefreshToken));
    RequestBody.Add('client_id=' + TNetEncoding.URL.Encode(FConfig.ClientID));
    RequestBody.Add('client_secret=' + TNetEncoding.URL.Encode(FConfig.ClientSecret));

    // Envoyer la requête
    HTTPClient.ContentType := 'application/x-www-form-urlencoded';
    Response := HTTPClient.Post(FConfig.TokenEndpoint, RequestBody);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        NewAccessToken := JSONResponse.GetValue<string>('access_token');
        ExpiresIn := JSONResponse.GetValue<Integer>('expires_in');

        // Certains providers renvoient un nouveau refresh token
        if JSONResponse.TryGetValue<string>('refresh_token', NewRefreshToken) then
          FConfig.SaveTokens(NewAccessToken, NewRefreshToken, ExpiresIn)
        else
          FConfig.SaveTokens(NewAccessToken, FConfig.RefreshToken, ExpiresIn);

        Result := True;
      finally
        JSONResponse.Free;
      end;
    end;

  finally
    RequestBody.Free;
    HTTPClient.Free;
  end;
end;

end.
```

### Serveur local pour le callback

Pour recevoir le code d'autorisation, vous devez créer un serveur HTTP local :

```pascal
unit LocalCallbackServer;

interface

uses
  System.SysUtils, System.Classes, IdHTTPServer, IdContext,
  IdCustomHTTPServer, System.SyncObjs;

type
  TCallbackReceived = procedure(const Code, State: string) of object;

  TLocalCallbackServer = class
  private
    FHTTPServer: TIdHTTPServer;
    FPort: Integer;
    FAuthorizationCode: string;
    FState: string;
    FEvent: TEvent;
    FOnCallbackReceived: TCallbackReceived;

    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(Port: Integer = 8080);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    function WaitForCallback(TimeoutMs: Integer = 60000): Boolean;

    property AuthorizationCode: string read FAuthorizationCode;
    property State: string read FState;
    property OnCallbackReceived: TCallbackReceived read FOnCallbackReceived write FOnCallbackReceived;
  end;

implementation

uses
  System.NetEncoding, IdGlobal;

constructor TLocalCallbackServer.Create(Port: Integer);
begin
  inherited Create;
  FPort := Port;
  FEvent := TEvent.Create(nil, True, False, '');

  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := FPort;
  FHTTPServer.OnCommandGet := HTTPServerCommandGet;
end;

destructor TLocalCallbackServer.Destroy;
begin
  Stop;
  FHTTPServer.Free;
  FEvent.Free;
  inherited;
end;

procedure TLocalCallbackServer.Start;
begin
  FEvent.ResetEvent;
  FAuthorizationCode := '';
  FState := '';
  FHTTPServer.Active := True;
end;

procedure TLocalCallbackServer.Stop;
begin
  FHTTPServer.Active := False;
end;

procedure TLocalCallbackServer.HTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Code, State: string;
  HTMLResponse: string;
begin
  // Extraire le code et le state de l'URL
  Code := ARequestInfo.Params.Values['code'];
  State := ARequestInfo.Params.Values['state'];

  if not Code.IsEmpty then
  begin
    FAuthorizationCode := Code;
    FState := State;

    // Page de succès
    HTMLResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head><title>Authentification réussie</title></head>' +
      '<body>' +
      '<h1>Authentification réussie !</h1>' +
      '<p>Vous pouvez fermer cette fenêtre et retourner à l''application.</p>' +
      '</body>' +
      '</html>';

    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := HTMLResponse;
    AResponseInfo.ResponseNo := 200;

    // Déclencher l'événement
    if Assigned(FOnCallbackReceived) then
      FOnCallbackReceived(Code, State);

    FEvent.SetEvent;
  end
  else
  begin
    // Erreur
    HTMLResponse :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head><title>Erreur</title></head>' +
      '<body>' +
      '<h1>Erreur d''authentification</h1>' +
      '<p>Erreur: ' + ARequestInfo.Params.Values['error'] + '</p>' +
      '</body>' +
      '</html>';

    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := HTMLResponse;
    AResponseInfo.ResponseNo := 400;
  end;
end;

function TLocalCallbackServer.WaitForCallback(TimeoutMs: Integer): Boolean;
begin
  Result := FEvent.WaitFor(TimeoutMs) = wrSignaled;
end;

end.
```

### Utiliser l'API avec le token

**Classe pour les requêtes authentifiées :**

```pascal
unit OAuth2APIClient;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient,
  OAuth2Config, OAuth2Flow;

type
  TOAuth2APIClient = class
  private
    FConfig: TOAuth2Config;
    FFlow: TOAuth2Flow;
    FHTTPClient: THTTPClient;

    procedure EnsureValidToken;
  public
    constructor Create(Config: TOAuth2Config; Flow: TOAuth2Flow);
    destructor Destroy; override;

    function Get(const URL: string): string;
    function Post(const URL, Body: string): string;
    function Put(const URL, Body: string): string;
    function Delete(const URL: string): string;
  end;

implementation

constructor TOAuth2APIClient.Create(Config: TOAuth2Config; Flow: TOAuth2Flow);
begin
  inherited Create;
  FConfig := Config;
  FFlow := Flow;
  FHTTPClient := THTTPClient.Create;
end;

destructor TOAuth2APIClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

procedure TOAuth2APIClient.EnsureValidToken;
begin
  // Vérifier si le token est expiré
  if FConfig.IsTokenExpired then
  begin
    // Essayer de rafraîchir
    if not FFlow.RefreshAccessToken then
      raise Exception.Create('Token expiré et impossible de rafraîchir');
  end;
end;

function TOAuth2APIClient.Get(const URL: string): string;
var
  Response: IHTTPResponse;
begin
  EnsureValidToken;

  // Ajouter le token d'accès dans l'en-tête Authorization
  FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FConfig.AccessToken;

  Response := FHTTPClient.Get(URL);

  if Response.StatusCode = 200 then
    Result := Response.ContentAsString
  else
    raise Exception.CreateFmt('Erreur API: %d - %s',
      [Response.StatusCode, Response.StatusText]);
end;

function TOAuth2APIClient.Post(const URL, Body: string): string;
var
  Response: IHTTPResponse;
  Stream: TStringStream;
begin
  EnsureValidToken;

  FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FConfig.AccessToken;
  FHTTPClient.ContentType := 'application/json';

  Stream := TStringStream.Create(Body, TEncoding.UTF8);
  try
    Response := FHTTPClient.Post(URL, Stream);

    if (Response.StatusCode >= 200) and (Response.StatusCode < 300) then
      Result := Response.ContentAsString
    else
      raise Exception.CreateFmt('Erreur API: %d - %s',
        [Response.StatusCode, Response.StatusText]);
  finally
    Stream.Free;
  end;
end;

function TOAuth2APIClient.Put(const URL, Body: string): string;
var
  Response: IHTTPResponse;
  Stream: TStringStream;
begin
  EnsureValidToken;

  FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FConfig.AccessToken;
  FHTTPClient.ContentType := 'application/json';

  Stream := TStringStream.Create(Body, TEncoding.UTF8);
  try
    Response := FHTTPClient.Put(URL, Stream);

    if (Response.StatusCode >= 200) and (Response.StatusCode < 300) then
      Result := Response.ContentAsString
    else
      raise Exception.CreateFmt('Erreur API: %d - %s',
        [Response.StatusCode, Response.StatusText]);
  finally
    Stream.Free;
  end;
end;

function TOAuth2APIClient.Delete(const URL: string): string;
var
  Response: IHTTPResponse;
begin
  EnsureValidToken;

  FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FConfig.AccessToken;

  Response := FHTTPClient.Delete(URL);

  if (Response.StatusCode >= 200) and (Response.StatusCode < 300) then
    Result := Response.ContentAsString
  else
    raise Exception.CreateFmt('Erreur API: %d - %s',
      [Response.StatusCode, Response.StatusText]);
end;

end.
```

## Intégration avec des providers populaires

### Google OAuth2

**Configuration :**

```pascal
procedure ConfigurerGoogleOAuth(Config: TOAuth2Config);
begin
  // Obtenir ces valeurs depuis Google Cloud Console
  Config.ClientID := 'VOTRE_CLIENT_ID.apps.googleusercontent.com';
  Config.ClientSecret := 'VOTRE_CLIENT_SECRET';
  Config.RedirectURI := 'http://localhost:8080/callback';

  // Endpoints Google
  Config.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/v2/auth';
  Config.TokenEndpoint := 'https://oauth2.googleapis.com/token';

  // Scopes (permissions demandées)
  Config.Scopes := 'openid email profile';
  // Pour Google Drive: 'https://www.googleapis.com/auth/drive.readonly'
  // Pour Gmail: 'https://www.googleapis.com/auth/gmail.readonly'
end;
```

**Utilisation :**

```pascal
var
  Config: TOAuth2Config;
  Flow: TOAuth2Flow;
  CallbackServer: TLocalCallbackServer;
  APIClient: TOAuth2APIClient;
  UserInfo: string;
begin
  Config := TOAuth2Config.Create;
  Flow := TOAuth2Flow.Create(Config);
  CallbackServer := TLocalCallbackServer.Create(8080);
  try
    // Configuration
    ConfigurerGoogleOAuth(Config);

    // Démarrer le serveur local
    CallbackServer.Start;

    // Lancer le flux OAuth2
    Flow.StartAuthorizationFlow;

    // Attendre le callback
    if CallbackServer.WaitForCallback(60000) then
    begin
      // Échanger le code contre un token
      if Flow.HandleCallback(CallbackServer.AuthorizationCode) then
      begin
        ShowMessage('Authentification réussie !');

        // Utiliser l'API
        APIClient := TOAuth2APIClient.Create(Config, Flow);
        try
          UserInfo := APIClient.Get('https://www.googleapis.com/oauth2/v2/userinfo');
          Memo1.Text := UserInfo;
        finally
          APIClient.Free;
        end;
      end;
    end
    else
      ShowMessage('Timeout d''authentification');

  finally
    CallbackServer.Free;
    Flow.Free;
    Config.Free;
  end;
end;
```

### Microsoft OAuth2 (Azure AD)

**Configuration :**

```pascal
procedure ConfigurerMicrosoftOAuth(Config: TOAuth2Config);
var
  TenantID: string;
begin
  TenantID := 'common'; // ou votre Tenant ID spécifique

  Config.ClientID := 'VOTRE_APPLICATION_ID';
  Config.ClientSecret := 'VOTRE_CLIENT_SECRET';
  Config.RedirectURI := 'http://localhost:8080/callback';

  // Endpoints Microsoft
  Config.AuthorizationEndpoint :=
    Format('https://login.microsoftonline.com/%s/oauth2/v2.0/authorize', [TenantID]);
  Config.TokenEndpoint :=
    Format('https://login.microsoftonline.com/%s/oauth2/v2.0/token', [TenantID]);

  // Scopes
  Config.Scopes := 'openid email profile User.Read';
end;
```

### Facebook OAuth2

**Configuration :**

```pascal
procedure ConfigurerFacebookOAuth(Config: TOAuth2Config);
begin
  Config.ClientID := 'VOTRE_APP_ID';
  Config.ClientSecret := 'VOTRE_APP_SECRET';
  Config.RedirectURI := 'http://localhost:8080/callback';

  // Endpoints Facebook
  Config.AuthorizationEndpoint := 'https://www.facebook.com/v12.0/dialog/oauth';
  Config.TokenEndpoint := 'https://graph.facebook.com/v12.0/oauth/access_token';

  // Scopes
  Config.Scopes := 'email public_profile';
end;
```

### GitHub OAuth2

**Configuration :**

```pascal
procedure ConfigurerGitHubOAuth(Config: TOAuth2Config);
begin
  Config.ClientID := 'VOTRE_CLIENT_ID';
  Config.ClientSecret := 'VOTRE_CLIENT_SECRET';
  Config.RedirectURI := 'http://localhost:8080/callback';

  // Endpoints GitHub
  Config.AuthorizationEndpoint := 'https://github.com/login/oauth/authorize';
  Config.TokenEndpoint := 'https://github.com/login/oauth/access_token';

  // Scopes
  Config.Scopes := 'user repo';
end;
```

## JWT : Manipulation des tokens

### Décoder un JWT

```pascal
unit JWTHelper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding;

type
  TJWTPayload = record
    Subject: string;        // sub
    Name: string;          // name
    Email: string;         // email
    IssuedAt: TDateTime;   // iat
    ExpiresAt: TDateTime;  // exp
    Scopes: TArray<string>;
  end;

  TJWTHelper = class
  public
    class function DecodeJWT(const Token: string): TJWTPayload;
    class function IsJWTExpired(const Token: string): Boolean;
    class function GetJWTExpiration(const Token: string): TDateTime;
  end;

implementation

uses
  System.DateUtils;

class function TJWTHelper.DecodeJWT(const Token: string): TJWTPayload;
var
  Parts: TArray<string>;
  PayloadEncoded, PayloadJSON: string;
  JSONObject: TJSONObject;
  ScopesArray: TJSONArray;
  i: Integer;
  UnixTime: Int64;
begin
  // JWT format: Header.Payload.Signature
  Parts := Token.Split(['.']);

  if Length(Parts) <> 3 then
    raise Exception.Create('JWT invalide');

  // Décoder le payload (partie 2)
  PayloadEncoded := Parts[1];

  // Ajouter le padding si nécessaire
  while (Length(PayloadEncoded) mod 4) <> 0 do
    PayloadEncoded := PayloadEncoded + '=';

  // Décoder Base64URL
  PayloadJSON := TNetEncoding.Base64.Decode(PayloadEncoded);

  // Parser JSON
  JSONObject := TJSONObject.ParseJSONValue(PayloadJSON) as TJSONObject;
  try
    // Extraire les champs
    if JSONObject.TryGetValue<string>('sub', Result.Subject) then;
    if JSONObject.TryGetValue<string>('name', Result.Name) then;
    if JSONObject.TryGetValue<string>('email', Result.Email) then;

    // Convertir les timestamps Unix en TDateTime
    if JSONObject.TryGetValue<Int64>('iat', UnixTime) then
      Result.IssuedAt := UnixToDateTime(UnixTime);

    if JSONObject.TryGetValue<Int64>('exp', UnixTime) then
      Result.ExpiresAt := UnixToDateTime(UnixTime);

    // Extraire les scopes (peut être un string ou un tableau)
    if JSONObject.TryGetValue<TJSONArray>('scopes', ScopesArray) then
    begin
      SetLength(Result.Scopes, ScopesArray.Count);
      for i := 0 to ScopesArray.Count - 1 do
        Result.Scopes[i] := ScopesArray.Items[i].Value;
    end;

  finally
    JSONObject.Free;
  end;
end;

class function TJWTHelper.IsJWTExpired(const Token: string): Boolean;
var
  ExpiresAt: TDateTime;
begin
  ExpiresAt := GetJWTExpiration(Token);
  Result := Now >= ExpiresAt;
end;

class function TJWTHelper.GetJWTExpiration(const Token: string): TDateTime;
var
  Payload: TJWTPayload;
begin
  Payload := DecodeJWT(Token);
  Result := Payload.ExpiresAt;
end;

end.
```

### Créer un JWT (côté serveur)

```pascal
unit JWTGenerator;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.Hash;

type
  TJWTGenerator = class
  private
    class function Base64URLEncode(const Input: string): string;
    class function CreateSignature(const Header, Payload, Secret: string): string;
  public
    class function GenerateJWT(const UserID, Name, Email: string;
      const Scopes: TArray<string>; const Secret: string;
      ExpiresInSeconds: Integer = 3600): string;
  end;

implementation

uses
  System.DateUtils;

class function TJWTGenerator.Base64URLEncode(const Input: string): string;
begin
  Result := TNetEncoding.Base64.Encode(Input);
  // Convertir Base64 standard en Base64URL
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

class function TJWTGenerator.CreateSignature(const Header, Payload, Secret: string): string;
var
  Data: string;
  Hash: string;
begin
  Data := Header + '.' + Payload;
  Hash := THashSHA2.GetHMAC(Data, Secret, SHA256);
  Result := Base64URLEncode(Hash);
end;

class function TJWTGenerator.GenerateJWT(const UserID, Name, Email: string;
  const Scopes: TArray<string>; const Secret: string;
  ExpiresInSeconds: Integer): string;
var
  Header, Payload, Signature: string;
  HeaderJSON, PayloadJSON: TJSONObject;
  ScopesArray: TJSONArray;
  Scope: string;
  IssuedAt, ExpiresAt: Int64;
begin
  // Créer le header
  HeaderJSON := TJSONObject.Create;
  try
    HeaderJSON.AddPair('alg', 'HS256');
    HeaderJSON.AddPair('typ', 'JWT');
    Header := Base64URLEncode(HeaderJSON.ToString);
  finally
    HeaderJSON.Free;
  end;

  // Timestamps Unix
  IssuedAt := DateTimeToUnix(Now);
  ExpiresAt := DateTimeToUnix(IncSecond(Now, ExpiresInSeconds));

  // Créer le payload
  PayloadJSON := TJSONObject.Create;
  try
    PayloadJSON.AddPair('sub', UserID);
    PayloadJSON.AddPair('name', Name);
    PayloadJSON.AddPair('email', Email);
    PayloadJSON.AddPair('iat', TJSONNumber.Create(IssuedAt));
    PayloadJSON.AddPair('exp', TJSONNumber.Create(ExpiresAt));

    // Ajouter les scopes
    ScopesArray := TJSONArray.Create;
    for Scope in Scopes do
      ScopesArray.Add(Scope);
    PayloadJSON.AddPair('scopes', ScopesArray);

    Payload := Base64URLEncode(PayloadJSON.ToString);
  finally
    PayloadJSON.Free;
  end;

  // Créer la signature
  Signature := CreateSignature(Header, Payload, Secret);

  // Assembler le JWT
  Result := Header + '.' + Payload + '.' + Signature;
end;

end.
```

## Stockage sécurisé des tokens

### Utiliser le gestionnaire d'identifiants Windows

```pascal
unit SecureTokenStorage;

interface

uses
  System.SysUtils, Winapi.Windows;

type
  TSecureTokenStorage = class
  public
    class procedure SaveToken(const ServiceName, Token: string);
    class function LoadToken(const ServiceName: string): string;
    class procedure DeleteToken(const ServiceName: string);
  end;

implementation

uses
  System.NetEncoding;

class procedure TSecureTokenStorage.SaveToken(const ServiceName, Token: string);
var
  Credential: CREDENTIAL;
  TokenBytes: TBytes;
begin
  TokenBytes := TEncoding.UTF8.GetBytes(Token);

  ZeroMemory(@Credential, SizeOf(Credential));
  Credential.Type_ := CRED_TYPE_GENERIC;
  Credential.TargetName := PChar('DelphiOAuth2:' + ServiceName);
  Credential.CredentialBlobSize := Length(TokenBytes);
  Credential.CredentialBlob := @TokenBytes[0];
  Credential.Persist := CRED_PERSIST_LOCAL_MACHINE;

  if not CredWrite(@Credential, 0) then
    RaiseLastOSError;
end;

class function TSecureTokenStorage.LoadToken(const ServiceName: string): string;
var
  Credential: PCREDENTIAL;
  TokenBytes: TBytes;
begin
  Result := '';

  if CredRead(PChar('DelphiOAuth2:' + ServiceName), CRED_TYPE_GENERIC, 0, Credential) then
  begin
    try
      SetLength(TokenBytes, Credential.CredentialBlobSize);
      Move(Credential.CredentialBlob^, TokenBytes[0], Credential.CredentialBlobSize);
      Result := TEncoding.UTF8.GetString(TokenBytes);
    finally
      CredFree(Credential);
    end;
  end;
end;

class procedure TSecureTokenStorage.DeleteToken(const ServiceName: string);
begin
  CredDelete(PChar('DelphiOAuth2:' + ServiceName), CRED_TYPE_GENERIC, 0);
end;

end.
```

### Chiffrement des tokens

```pascal
unit TokenEncryption;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding;

type
  TTokenEncryption = class
  private
    class function XOREncrypt(const Data, Key: string): string;
  public
    class function EncryptToken(const Token, Password: string): string;
    class function DecryptToken(const EncryptedToken, Password: string): string;
  end;

implementation

uses
  System.Hash;

class function TTokenEncryption.XOREncrypt(const Data, Key: string): string;
var
  i: Integer;
  DataBytes, KeyBytes, ResultBytes: TBytes;
begin
  DataBytes := TEncoding.UTF8.GetBytes(Data);
  KeyBytes := TEncoding.UTF8.GetBytes(Key);
  SetLength(ResultBytes, Length(DataBytes));

  for i := 0 to High(DataBytes) do
    ResultBytes[i] := DataBytes[i] xor KeyBytes[i mod Length(KeyBytes)];

  Result := TNetEncoding.Base64.EncodeBytesToString(ResultBytes);
end;

class function TTokenEncryption.EncryptToken(const Token, Password: string): string;
var
  Key: string;
begin
  // Créer une clé à partir du mot de passe
  Key := THashSHA2.GetHashString(Password);

  // Chiffrer avec XOR (simple, pour exemple)
  // En production, utiliser AES ou autre algorithme robuste
  Result := XOREncrypt(Token, Key);
end;

class function TTokenEncryption.DecryptToken(const EncryptedToken, Password: string): string;
var
  Key: string;
begin
  Key := THashSHA2.GetHashString(Password);
  Result := XOREncrypt(EncryptedToken, Key); // XOR est réversible
end;

end.
```

## Sécurité et bonnes pratiques

### 1. Toujours utiliser HTTPS

```pascal
// ✅ Bon
Config.AuthorizationEndpoint := 'https://accounts.google.com/oauth2/auth';

// ❌ Jamais HTTP en production
Config.AuthorizationEndpoint := 'http://accounts.google.com/oauth2/auth';
```

### 2. Valider le state (protection CSRF)

```pascal
procedure TOAuth2Flow.StartAuthorizationFlow;
begin
  FState := GenerateRandomState;
  // Stocker le state
  SaveState(FState);

  // Ajouter à l'URL
  AuthURL := AuthURL + '&state=' + FState;
end;

function ValidateCallback(const ReceivedState: string): Boolean;
var
  ExpectedState: string;
begin
  ExpectedState := LoadState;
  Result := ReceivedState = ExpectedState;

  if not Result then
    raise Exception.Create('CSRF détecté : state invalide');
end;
```

### 3. Utiliser PKCE pour les applications publiques

```pascal
type
  TPKCEHelper = class
  public
    class function GenerateCodeVerifier: string;
    class function GenerateCodeChallenge(const Verifier: string): string;
  end;

class function TPKCEHelper.GenerateCodeVerifier: string;
var
  i: Integer;
const
  Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~';
begin
  SetLength(Result, 128);
  for i := 1 to 128 do
    Result[i] := Chars[Random(Length(Chars)) + 1];
end;

class function TPKCEHelper.GenerateCodeChallenge(const Verifier: string): string;
var
  Hash: string;
begin
  // SHA256 du verifier
  Hash := THashSHA2.GetHashString(Verifier, SHA256);

  // Base64URL encode
  Result := TNetEncoding.Base64.Encode(Hash);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;
```

### 4. Ne jamais logger les tokens

```pascal
// ❌ Mauvais - Ne jamais logger les tokens
procedure LogAPICall(const URL, Token: string);
begin
  WriteLn('Calling: ' + URL + ' with token: ' + Token); // DANGEREUX!
end;

// ✅ Bon - Logger sans le token
procedure LogAPICall(const URL: string);
begin
  WriteLn('Calling: ' + URL + ' [token hidden]');
end;
```

### 5. Révoquer les tokens

```pascal
function RevokeToken(const Token: string; const RevokeEndpoint: string): Boolean;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  RequestBody: TStringList;
begin
  HTTPClient := THTTPClient.Create;
  RequestBody := TStringList.Create;
  try
    RequestBody.Add('token=' + TNetEncoding.URL.Encode(Token));

    HTTPClient.ContentType := 'application/x-www-form-urlencoded';
    Response := HTTPClient.Post(RevokeEndpoint, RequestBody);

    Result := Response.StatusCode = 200;
  finally
    RequestBody.Free;
    HTTPClient.Free;
  end;
end;
```

### 6. Limiter les scopes

```pascal
// ❌ Demander trop de permissions
Config.Scopes := 'email profile read write delete admin';

// ✅ Demander seulement ce qui est nécessaire
Config.Scopes := 'email profile';
```

### 7. Gérer l'expiration gracieusement

```pascal
function ExecuteAPICall(const URL: string): string;
var
  Retries: Integer;
begin
  Retries := 0;

  while Retries < 3 do
  begin
    try
      Result := APIClient.Get(URL);
      Break; // Succès
    except
      on E: Exception do
      begin
        if (E.Message.Contains('401')) or (E.Message.Contains('expired')) then
        begin
          // Token expiré, rafraîchir
          if Flow.RefreshAccessToken then
          begin
            Inc(Retries);
            Continue; // Réessayer
          end
          else
            raise Exception.Create('Impossible de rafraîchir le token');
        end
        else
          raise; // Autre erreur
      end;
    end;
  end;
end;
```

### 8. Configurer les timeouts

```pascal
HTTPClient.ConnectionTimeout := 10000; // 10 secondes
HTTPClient.ResponseTimeout := 30000;   // 30 secondes
```

## Exemple complet d'application

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  OAuth2Config, OAuth2Flow, OAuth2APIClient, LocalCallbackServer;

type
  TFormMain = class(TForm)
    ButtonLogin: TButton;
    ButtonLogout: TButton;
    ButtonGetProfile: TButton;
    MemoInfo: TMemo;
    LabelStatus: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonLogoutClick(Sender: TObject);
    procedure ButtonGetProfileClick(Sender: TObject);
  private
    FConfig: TOAuth2Config;
    FFlow: TOAuth2Flow;
    FCallbackServer: TLocalCallbackServer;
    FAPIClient: TOAuth2APIClient;

    procedure UpdateUI;
    procedure ConfigureGoogleOAuth;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.JSON, SecureTokenStorage;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConfig := TOAuth2Config.Create;
  FFlow := TOAuth2Flow.Create(FConfig);
  FCallbackServer := TLocalCallbackServer.Create(8080);

  ConfigureGoogleOAuth;

  // Essayer de charger un token existant
  FConfig.AccessToken := TSecureTokenStorage.LoadToken('GoogleAccessToken');
  FConfig.RefreshToken := TSecureTokenStorage.LoadToken('GoogleRefreshToken');

  UpdateUI;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FAPIClient.Free;
  FCallbackServer.Free;
  FFlow.Free;
  FConfig.Free;
end;

procedure TFormMain.ConfigureGoogleOAuth;
begin
  FConfig.ClientID := 'VOTRE_CLIENT_ID.apps.googleusercontent.com';
  FConfig.ClientSecret := 'VOTRE_CLIENT_SECRET';
  FConfig.RedirectURI := 'http://localhost:8080/callback';
  FConfig.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/v2/auth';
  FConfig.TokenEndpoint := 'https://oauth2.googleapis.com/token';
  FConfig.Scopes := 'openid email profile';
end;

procedure TFormMain.ButtonLoginClick(Sender: TObject);
begin
  MemoInfo.Lines.Add('Démarrage de l''authentification...');

  FCallbackServer.Start;
  FFlow.StartAuthorizationFlow;

  // Attendre le callback dans un thread
  TThread.CreateAnonymousThread(
    procedure
    begin
      if FCallbackServer.WaitForCallback(120000) then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            if FFlow.HandleCallback(FCallbackServer.AuthorizationCode) then
            begin
              MemoInfo.Lines.Add('Authentification réussie !');

              // Sauvegarder les tokens
              TSecureTokenStorage.SaveToken('GoogleAccessToken', FConfig.AccessToken);
              TSecureTokenStorage.SaveToken('GoogleRefreshToken', FConfig.RefreshToken);

              UpdateUI;
            end
            else
              MemoInfo.Lines.Add('Échec de l''authentification');
          end);
      end
      else
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            MemoInfo.Lines.Add('Timeout d''authentification');
          end);
      end;

      FCallbackServer.Stop;
    end).Start;
end;

procedure TFormMain.ButtonLogoutClick(Sender: TObject);
begin
  FConfig.AccessToken := '';
  FConfig.RefreshToken := '';

  TSecureTokenStorage.DeleteToken('GoogleAccessToken');
  TSecureTokenStorage.DeleteToken('GoogleRefreshToken');

  MemoInfo.Lines.Add('Déconnecté');
  UpdateUI;
end;

procedure TFormMain.ButtonGetProfileClick(Sender: TObject);
var
  UserInfo: string;
  JSONObject: TJSONObject;
  Name, Email: string;
begin
  if FAPIClient = nil then
    FAPIClient := TOAuth2APIClient.Create(FConfig, FFlow);

  try
    UserInfo := FAPIClient.Get('https://www.googleapis.com/oauth2/v2/userinfo');

    JSONObject := TJSONObject.ParseJSONValue(UserInfo) as TJSONObject;
    try
      Name := JSONObject.GetValue<string>('name');
      Email := JSONObject.GetValue<string>('email');

      MemoInfo.Lines.Add('');
      MemoInfo.Lines.Add('Profil utilisateur :');
      MemoInfo.Lines.Add('Nom: ' + Name);
      MemoInfo.Lines.Add('Email: ' + Email);
    finally
      JSONObject.Free;
    end;

  except
    on E: Exception do
      MemoInfo.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TFormMain.UpdateUI;
var
  IsAuthenticated: Boolean;
begin
  IsAuthenticated := not FConfig.AccessToken.IsEmpty;

  ButtonLogin.Enabled := not IsAuthenticated;
  ButtonLogout.Enabled := IsAuthenticated;
  ButtonGetProfile.Enabled := IsAuthenticated;

  if IsAuthenticated then
    LabelStatus.Caption := 'Connecté'
  else
    LabelStatus.Caption := 'Non connecté';
end;

end.
```

## Résumé

### Points clés OAuth2

✅ **Concepts fondamentaux :**
- **OAuth2** = protocole d'autorisation standard
- **Access Token** = clé temporaire pour accéder aux ressources
- **Refresh Token** = permet de renouveler l'access token
- **JWT** = format standard pour les tokens

✅ **Flux OAuth2 :**
- **Authorization Code** : Le plus sécurisé, pour web/desktop
- **PKCE** : Pour applications publiques (mobile/desktop)
- **Client Credentials** : Machine-to-machine
- Éviter Implicit et Password flows

✅ **Implémentation Delphi :**
- Serveur local pour recevoir le callback
- TNetHTTPClient pour les requêtes
- Gestion automatique du refresh
- Stockage sécurisé des tokens

✅ **Providers populaires :**
- Google, Microsoft, Facebook, GitHub
- Configuration spécifique par provider
- Scopes différents selon les besoins

✅ **Sécurité :**
- Toujours HTTPS
- Valider le state (CSRF)
- Utiliser PKCE quand possible
- Ne jamais logger les tokens
- Révoquer les tokens à la déconnexion
- Limiter les scopes au minimum

✅ **Bonnes pratiques :**
- Rafraîchir automatiquement les tokens
- Gérer gracieusement les expirations
- Chiffrer les tokens en stockage
- Configurer des timeouts
- Utiliser le gestionnaire d'identifiants système

OAuth2 est le standard moderne pour l'authentification et l'autorisation, offrant sécurité et flexibilité pour les applications modernes !

⏭️ [GraphQL et nouvelles API](/10-communication-et-services-reseaux/08-graphql-et-nouvelles-api.md)
