# 10.7 OAuth2 et authentification moderne

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'authentification est un aspect crucial de toute application moderne, en particulier pour celles qui accèdent à des services externes ou qui nécessitent une sécurité renforcée. Dans ce chapitre, nous allons explorer OAuth2, le standard d'authentification moderne le plus répandu, et voir comment l'implémenter dans vos applications Delphi.

## Qu'est-ce que OAuth2 ?

OAuth2 (Open Authorization 2.0) est un protocole d'autorisation qui permet à une application d'accéder à des ressources protégées au nom d'un utilisateur sans que celui-ci ait à partager ses identifiants (nom d'utilisateur et mot de passe) avec l'application.

![Flux OAuth2](https://via.placeholder.com/800x400)

### Principaux acteurs dans OAuth2

- **Utilisateur** : La personne qui souhaite utiliser votre application
- **Client** : Votre application Delphi
- **Serveur d'autorisation** : Service qui authentifie l'utilisateur (par exemple, Google, Microsoft)
- **Serveur de ressources** : Service qui détient les données que votre application souhaite accéder

### Avantages d'OAuth2

- Pas besoin de stocker les mots de passe des utilisateurs
- Accès délimité par des "scopes" (portées)
- Possibilité de révoquer l'accès sans changer le mot de passe
- Compatibilité avec de nombreux services populaires
- Support de l'authentification multifacteur

## Types de flux OAuth2

OAuth2 propose plusieurs types de flux selon vos besoins :

### 1. Flux d'autorisation (Authorization Code Flow)

Le plus sécurisé, idéal pour les applications Delphi desktop :

```pascal
// Exemple de code pour démarrer le flux d'autorisation
procedure TAuthForm.DemanderAutorisation;
var
  URL: string;
begin
  // Construction de l'URL d'autorisation
  URL := 'https://auth.service.com/oauth2/authorize?' +
         'client_id=' + ClientID +
         '&redirect_uri=' + EncodeURIComponent(RedirectURI) +
         '&response_type=code' +
         '&scope=' + EncodeURIComponent('profile email');

  // Ouvrir le navigateur avec cette URL
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;
```

### 2. Flux implicite (Implicit Flow)

Simplifié, pour les applications sans backend :

```pascal
// Exemple de construction d'URL pour le flux implicite
URL := 'https://auth.service.com/oauth2/authorize?' +
       'client_id=' + ClientID +
       '&redirect_uri=' + EncodeURIComponent(RedirectURI) +
       '&response_type=token' +
       '&scope=' + EncodeURIComponent('profile');
```

### 3. Flux d'identification de client (Client Credentials Flow)

Pour les communications entre serveurs :

```pascal
// Exemple de demande d'accès avec identifiants client
procedure TOAuth2Client.ObtenirTokenClientCredentials;
var
  RESTClient: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(nil);
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    RESTClient.BaseURL := 'https://auth.service.com';
    Request.Client := RESTClient;
    Request.Response := Response;

    Request.Method := TRESTRequestMethod.rmPOST;
    Request.Resource := 'oauth2/token';

    Request.Params.AddItem('grant_type', 'client_credentials', TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_id', FClientID, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_secret', FClientSecret, TRESTRequestParameterKind.pkFORMDATA);

    Request.Execute;

    if Response.StatusCode = 200 then
    begin
      // Traitement du token reçu
      ParseTokenResponse(Response.Content);
    end;
  finally
    RESTClient.Free;
    Request.Free;
    Response.Free;
  end;
end;
```

## Implémentation d'OAuth2 dans une application Delphi

Voyons étape par étape comment implémenter OAuth2 dans une application Delphi :

### Étape 1 : Inscription de votre application

Avant de commencer, vous devez inscrire votre application auprès du fournisseur d'identité (Google, Microsoft, etc.) pour obtenir un `client_id` et un `client_secret`.

### Étape 2 : Configuration des composants

Créez une nouvelle application Delphi et ajoutez les composants nécessaires :

```pascal
// Dans votre formulaire principal
private
  FOAuth2Client: TOAuth2Client; // Classe personnalisée pour gérer OAuth2
  FRESTClient: TRESTClient;
  FAccessToken: string;
```

### Étape 3 : Création d'une classe OAuth2 personnalisée

```pascal
unit OAuth2Client;

interface

uses
  System.SysUtils, System.Classes, REST.Client, REST.Types, System.JSON;

type
  TOAuth2Client = class
  private
    FClientID: string;
    FClientSecret: string;
    FRedirectURI: string;
    FAccessToken: string;
    FRefreshToken: string;
    FExpiresIn: Integer;
    FTokenType: string;
    FScope: string;
    FOnTokenReceived: TNotifyEvent;

    procedure ParseTokenResponse(const JsonResponse: string);
  public
    constructor Create(const AClientID, AClientSecret, ARedirectURI: string);

    procedure DemanderAutorisation(const Scope: string = 'profile');
    function EchangerCodeContreToken(const Code: string): Boolean;
    function RefreshAccessToken: Boolean;

    property AccessToken: string read FAccessToken;
    property RefreshToken: string read FRefreshToken;
    property ExpiresIn: Integer read FExpiresIn;
    property TokenType: string read FTokenType;
    property Scope: string read FScope;
    property OnTokenReceived: TNotifyEvent read FOnTokenReceived write FOnTokenReceived;
  end;

implementation

uses
  Winapi.ShellAPI, Winapi.Windows, System.NetEncoding;

{ TOAuth2Client }

constructor TOAuth2Client.Create(const AClientID, AClientSecret, ARedirectURI: string);
begin
  FClientID := AClientID;
  FClientSecret := AClientSecret;
  FRedirectURI := ARedirectURI;
end;

procedure TOAuth2Client.DemanderAutorisation(const Scope: string);
var
  URL: string;
begin
  // Construction de l'URL d'autorisation
  URL := 'https://auth.service.com/oauth2/authorize?' +
         'client_id=' + FClientID +
         '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectURI) +
         '&response_type=code' +
         '&scope=' + TNetEncoding.URL.Encode(Scope);

  // Ouvrir le navigateur avec l'URL d'autorisation
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

function TOAuth2Client.EchangerCodeContreToken(const Code: string): Boolean;
var
  RESTClient: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
begin
  Result := False;

  RESTClient := TRESTClient.Create(nil);
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    RESTClient.BaseURL := 'https://auth.service.com';
    Request.Client := RESTClient;
    Request.Response := Response;

    Request.Method := TRESTRequestMethod.rmPOST;
    Request.Resource := 'oauth2/token';

    Request.Params.AddItem('grant_type', 'authorization_code', TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('code', Code, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_id', FClientID, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_secret', FClientSecret, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('redirect_uri', FRedirectURI, TRESTRequestParameterKind.pkFORMDATA);

    try
      Request.Execute;

      if Response.StatusCode = 200 then
      begin
        ParseTokenResponse(Response.Content);
        Result := True;

        if Assigned(FOnTokenReceived) then
          FOnTokenReceived(Self);
      end;
    except
      // Gestion des erreurs
    end;
  finally
    RESTClient.Free;
    Request.Free;
    Response.Free;
  end;
end;

function TOAuth2Client.RefreshAccessToken: Boolean;
var
  RESTClient: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
begin
  Result := False;

  if FRefreshToken = '' then
    Exit;

  RESTClient := TRESTClient.Create(nil);
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    RESTClient.BaseURL := 'https://auth.service.com';
    Request.Client := RESTClient;
    Request.Response := Response;

    Request.Method := TRESTRequestMethod.rmPOST;
    Request.Resource := 'oauth2/token';

    Request.Params.AddItem('grant_type', 'refresh_token', TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('refresh_token', FRefreshToken, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_id', FClientID, TRESTRequestParameterKind.pkFORMDATA);
    Request.Params.AddItem('client_secret', FClientSecret, TRESTRequestParameterKind.pkFORMDATA);

    try
      Request.Execute;

      if Response.StatusCode = 200 then
      begin
        ParseTokenResponse(Response.Content);
        Result := True;

        if Assigned(FOnTokenReceived) then
          FOnTokenReceived(Self);
      end;
    except
      // Gestion des erreurs
    end;
  finally
    RESTClient.Free;
    Request.Free;
    Response.Free;
  end;
end;

procedure TOAuth2Client.ParseTokenResponse(const JsonResponse: string);
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.ParseJSONValue(JsonResponse) as TJSONObject;

  try
    if JsonObj.TryGetValue<string>('access_token', FAccessToken) then
    begin
      JsonObj.TryGetValue<string>('refresh_token', FRefreshToken);
      JsonObj.TryGetValue<Integer>('expires_in', FExpiresIn);
      JsonObj.TryGetValue<string>('token_type', FTokenType);
      JsonObj.TryGetValue<string>('scope', FScope);
    end;
  finally
    JsonObj.Free;
  end;
end;

end.
```

### Étape 4 : Création du formulaire pour la redirection

```pascal
unit RedirectForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls;

type
  TRedirectForm = class(TForm)
    WebBrowser1: TWebBrowser;
    procedure WebBrowser1NavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure FormCreate(Sender: TObject);
  private
    FRedirectURI: string;
    FCode: string;
  public
    function GetAuthCode(const AuthURL, RedirectURI: string): string;
  end;

implementation

{$R *.dfm}

function TRedirectForm.GetAuthCode(const AuthURL, RedirectURI: string): string;
begin
  FRedirectURI := RedirectURI;
  FCode := '';

  WebBrowser1.Navigate(AuthURL);
  ShowModal;

  Result := FCode;
end;

procedure TRedirectForm.FormCreate(Sender: TObject);
begin
  WebBrowser1.Align := alClient;
end;

procedure TRedirectForm.WebBrowser1NavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
var
  CurrentURL: string;
  CodePos: Integer;
begin
  CurrentURL := VarToStr(URL);

  // Vérifier si on est redirigé vers notre URI de redirection
  if Pos(FRedirectURI, CurrentURL) = 1 then
  begin
    // Extraire le code d'autorisation de l'URL
    CodePos := Pos('code=', CurrentURL);
    if CodePos > 0 then
    begin
      FCode := Copy(CurrentURL, CodePos + 5, Length(CurrentURL));

      // Si le code contient d'autres paramètres, on les supprime
      CodePos := Pos('&', FCode);
      if CodePos > 0 then
        FCode := Copy(FCode, 1, CodePos - 1);

      ModalResult := mrOk;
    end;
  end;
end;

end.
```

### Étape 5 : Utilisation dans votre application principale

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, REST.Client, REST.Types,
  OAuth2Client, RedirectForm;

type
  TForm1 = class(TForm)
    btnLogin: TButton;
    btnGetData: TButton;
    memoResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnGetDataClick(Sender: TObject);
  private
    FOAuth2Client: TOAuth2Client;
    FRESTClient: TRESTClient;
    procedure OnTokenReceived(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Remplacer par vos valeurs obtenues lors de l'inscription de votre application
  FOAuth2Client := TOAuth2Client.Create('votre_client_id', 'votre_client_secret', 'http://localhost/callback');
  FOAuth2Client.OnTokenReceived := OnTokenReceived;

  FRESTClient := TRESTClient.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOAuth2Client.Free;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
var
  AuthURL: string;
  RedirectForm: TRedirectForm;
  Code: string;
begin
  // Construire l'URL d'autorisation (remplacer par votre service)
  AuthURL := 'https://auth.service.com/oauth2/authorize?' +
             'client_id=' + FOAuth2Client.ClientID +
             '&redirect_uri=' + TNetEncoding.URL.Encode(FOAuth2Client.RedirectURI) +
             '&response_type=code' +
             '&scope=' + TNetEncoding.URL.Encode('profile email');

  // Créer un formulaire de redirection avec un WebBrowser
  RedirectForm := TRedirectForm.Create(Self);
  try
    Code := RedirectForm.GetAuthCode(AuthURL, FOAuth2Client.RedirectURI);

    // Si un code a été obtenu, l'échanger contre un token
    if Code <> '' then
      FOAuth2Client.EchangerCodeContreToken(Code);
  finally
    RedirectForm.Free;
  end;
end;

procedure TForm1.btnGetDataClick(Sender: TObject);
var
  Request: TRESTRequest;
  Response: TRESTResponse;
begin
  if FOAuth2Client.AccessToken = '' then
  begin
    ShowMessage('Vous devez d''abord vous connecter!');
    Exit;
  end;

  Response := TRESTResponse.Create(Self);
  Request := TRESTRequest.Create(Self);

  try
    FRESTClient.BaseURL := 'https://api.service.com';
    Request.Client := FRESTClient;
    Request.Response := Response;

    Request.Method := TRESTRequestMethod.rmGET;
    Request.Resource := 'user/profile';

    // Ajouter le token d'accès dans l'en-tête Authorization
    Request.Params.AddItem('Authorization', 'Bearer ' + FOAuth2Client.AccessToken,
                          TRESTRequestParameterKind.pkHTTPHEADER);

    Request.Execute;

    if Response.StatusCode = 200 then
      memoResult.Text := Response.Content
    else
      memoResult.Text := 'Erreur: ' + IntToStr(Response.StatusCode) + ' - ' + Response.Content;
  finally
    Request.Free;
    Response.Free;
  end;
end;

procedure TForm1.OnTokenReceived(Sender: TObject);
begin
  memoResult.Lines.Add('Token reçu avec succès!');
  memoResult.Lines.Add('Access Token: ' + FOAuth2Client.AccessToken);
  memoResult.Lines.Add('Expire dans: ' + IntToStr(FOAuth2Client.ExpiresIn) + ' secondes');
  memoResult.Lines.Add('Scope: ' + FOAuth2Client.Scope);
end;

end.
```

## Utilisation d'OAuth2 avec des services populaires

### Google OAuth2

```pascal
// Configuration pour Google OAuth2
FOAuth2Client := TOAuth2Client.Create(
  '123456789-abcdef.apps.googleusercontent.com',  // Client ID
  'abcdefg-secret',                              // Client Secret
  'http://localhost:8080/callback'               // URI de redirection
);

// URL d'autorisation Google
AuthURL := 'https://accounts.google.com/o/oauth2/v2/auth?' +
           'client_id=' + FOAuth2Client.ClientID +
           '&redirect_uri=' + TNetEncoding.URL.Encode(FOAuth2Client.RedirectURI) +
           '&response_type=code' +
           '&scope=' + TNetEncoding.URL.Encode('https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email');
```

### Microsoft OAuth2

```pascal
// Configuration pour Microsoft OAuth2
FOAuth2Client := TOAuth2Client.Create(
  'appid-from-azure-portal',                    // Client ID
  'secret-from-azure-portal',                   // Client Secret
  'http://localhost:8080/callback'              // URI de redirection
);

// URL d'autorisation Microsoft
AuthURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize?' +
           'client_id=' + FOAuth2Client.ClientID +
           '&redirect_uri=' + TNetEncoding.URL.Encode(FOAuth2Client.RedirectURI) +
           '&response_type=code' +
           '&scope=' + TNetEncoding.URL.Encode('User.Read');
```

## OpenID Connect

OpenID Connect est une couche d'identité construite sur OAuth2 qui permet non seulement l'autorisation mais aussi l'authentification.

```pascal
// URL d'autorisation OpenID Connect
AuthURL := 'https://accounts.google.com/o/oauth2/v2/auth?' +
           'client_id=' + FOAuth2Client.ClientID +
           '&redirect_uri=' + TNetEncoding.URL.Encode(FOAuth2Client.RedirectURI) +
           '&response_type=code' +
           '&scope=' + TNetEncoding.URL.Encode('openid profile email') +
           '&nonce=' + GenerateRandomNonce();  // Important pour la sécurité
```

## Bonnes pratiques de sécurité

### État (State)

L'utilisation du paramètre `state` est recommandée pour prévenir les attaques CSRF :

```pascal
// Génération d'un état aléatoire
function GenerateRandomState: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

// Ajout du paramètre state à l'URL d'autorisation
FState := GenerateRandomState;
AuthURL := AuthURL + '&state=' + FState;

// Vérification du state lors de la redirection
if ExtractedState <> FState then
  raise Exception.Create('Erreur de sécurité: state invalide');
```

### PKCE (Proof Key for Code Exchange)

Pour les applications publiques, utilisez PKCE pour renforcer la sécurité :

```pascal
// Génération d'un code_verifier
FCodeVerifier := GenerateRandomString(64);

// Calcul du code_challenge
FCodeChallenge := THashSHA256.GetHashString(FCodeVerifier);
FCodeChallenge := TNetEncoding.Base64URL.Encode(FCodeChallenge);

// Ajout du code_challenge à l'URL d'autorisation
AuthURL := AuthURL +
  '&code_challenge=' + FCodeChallenge +
  '&code_challenge_method=S256';

// Inclusion du code_verifier lors de l'échange du code
Request.Params.AddItem('code_verifier', FCodeVerifier, TRESTRequestParameterKind.pkFORMDATA);
```

## Stockage sécurisé des tokens

```pascal
// Exemple d'utilisation du Credential Manager de Windows
function StoreCredential(const CredentialName, Username, Password: string): Boolean;
var
  Credential: PCREDENTIALW;
  CredSize: Cardinal;
begin
  CredSize := SizeOf(CREDENTIALW);
  Credential := AllocMem(CredSize);
  try
    Credential.Type_ := CRED_TYPE_GENERIC;
    Credential.TargetName := PChar(CredentialName);
    Credential.UserName := PChar(Username);
    Credential.CredentialBlobSize := Length(Password) * SizeOf(Char);
    Credential.CredentialBlob := PByte(PChar(Password));
    Credential.Persist := CRED_PERSIST_LOCAL_MACHINE;

    Result := CredWrite(Credential, 0);
  finally
    FreeMem(Credential);
  end;
end;

// Stocker le token d'accès
StoreCredential('MyApp.AccessToken', 'OAuth2User', FOAuth2Client.AccessToken);
```

## Authentification unique (SSO)

Pour implémenter l'authentification unique dans plusieurs applications Delphi :

```pascal
// Vérifier si un utilisateur est déjà connecté
function IsUserLoggedIn: Boolean;
var
  Credential: PCREDENTIALW;
  Read: Boolean;
begin
  Credential := nil;
  Read := CredRead('MyApp.AccessToken', CRED_TYPE_GENERIC, 0, Credential);
  if Read and (Credential <> nil) then
  begin
    // Vérifier si le token est encore valide
    FAccessToken := PChar(Credential.CredentialBlob);
    Result := ValidateToken(FAccessToken);
    CredFree(Credential);
  end
  else
    Result := False;
end;
```

## Exercice pratique : Intégration avec Google OAuth2

Voyons comment intégrer Google OAuth2 dans une application Delphi :

1. Inscrivez votre application dans la console Google Developer
2. Créez une application Delphi avec les composants TButton, TMemo, TWebBrowser
3. Implémentez l'authentification OAuth2 en utilisant les exemples ci-dessus
4. Récupérez et affichez le profil utilisateur depuis l'API Google

## Dépannage et erreurs courantes

### Erreur "redirect_uri_mismatch"

```
Vérifiez que l'URI de redirection dans votre code correspond exactement
à celle configurée dans la console développeur du service.
```

### Erreur "invalid_client"

```
Vérifiez que le client_id et client_secret sont corrects
et correspondent aux valeurs fournies par le service d'authentification.
```

### Erreur "invalid_grant"

```
Cette erreur peut survenir si :
- Le code d'autorisation a déjà été utilisé
- Le code est expiré (généralement après 10 minutes)
- Le code n'est pas valide pour l'URI de redirection utilisée
```

## Conclusion

L'authentification OAuth2 est essentielle pour les applications modernes qui interagissent avec des services externes. Avec Delphi, vous pouvez facilement implémenter cette méthode d'authentification sécurisée pour offrir à vos utilisateurs une expérience fluide et conforme aux standards actuels.

Pour aller plus loin, explorez d'autres fonctionnalités comme la gestion des tokens expirés, l'implémentation de la déconnexion, ou l'intégration avec d'autres fournisseurs d'identité comme Facebook, Twitter ou GitHub.

## Ressources supplémentaires

- Documentation officielle OAuth2
- Exemples de code sur GitHub
- Tutoriels vidéo sur l'authentification moderne avec Delphi

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria.*

⏭️ [GraphQL et nouvelles API](/10-communication-et-services-reseaux/08-graphql-et-nouvelles-api.md)

