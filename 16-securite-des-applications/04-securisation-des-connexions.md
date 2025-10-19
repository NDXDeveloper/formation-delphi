🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.4 Sécurisation des connexions

## Introduction

Dans un monde où les applications communiquent constamment via Internet ou des réseaux locaux, sécuriser ces connexions est devenu absolument critique. Chaque donnée transmise sans protection peut être interceptée, lue ou modifiée par un attaquant.

**Analogie du monde réel** : Imaginez envoyer une carte postale (HTTP) versus envoyer une lettre dans une enveloppe scellée (HTTPS). Avec la carte postale, n'importe qui peut lire votre message pendant le transit. Avec l'enveloppe scellée, seul le destinataire peut ouvrir et lire le contenu.

### Pourquoi sécuriser les connexions ?

**Les dangers d'une connexion non sécurisée** :
- **Interception** : Un attaquant peut lire toutes les données échangées
- **Modification** : Les données peuvent être altérées en transit (attaque man-in-the-middle)
- **Usurpation** : Quelqu'un peut se faire passer pour le serveur légitime
- **Vol d'identifiants** : Mots de passe et tokens transmis en clair

**Données sensibles à protéger** :
- Identifiants de connexion (login, mot de passe)
- Informations personnelles (nom, adresse, email)
- Données bancaires
- Sessions utilisateur (cookies, tokens)
- Documents confidentiels

## Les protocoles de sécurité

### HTTP vs HTTPS

**HTTP (HyperText Transfer Protocol)** : Le protocole de base du web, mais **NON sécurisé**.

```
Client ──[Données en clair]──> Serveur
       ←─[Réponse en clair]──

⚠️ Problème : Tout le monde peut lire les données !
```

**HTTPS (HTTP Secure)** : HTTP avec une couche de chiffrement SSL/TLS.

```
Client ──[Données chiffrées]──> Serveur
       ←─[Réponse chiffrée]──

✅ Sécurisé : Seuls le client et le serveur peuvent déchiffrer
```

**Différences visuelles** :
```
HTTP  : http://example.com  (pas de cadenas dans le navigateur)
HTTPS : https://example.com (cadenas 🔒 dans le navigateur)
```

### SSL et TLS

**SSL (Secure Sockets Layer)** : Ancien protocole de sécurité, maintenant obsolète.

**TLS (Transport Layer Security)** : Remplaçant moderne de SSL, c'est ce qu'on utilise aujourd'hui.

**Versions de TLS** :
- TLS 1.0 : ❌ Déprécié, vulnérable
- TLS 1.1 : ❌ Déprécié, ne plus utiliser
- TLS 1.2 : ✅ Bon, encore largement utilisé
- TLS 1.3 : ✅ Meilleur, plus rapide et plus sûr

**Note importante** : Même si on dit souvent "SSL", on parle en réalité de TLS aujourd'hui. Les deux termes sont utilisés de manière interchangeable par habitude.

## Comment fonctionne HTTPS/TLS

### Le processus de connexion sécurisée (handshake)

Quand vous vous connectez à un site en HTTPS, voici ce qui se passe en coulisses :

**Étape 1 : Hello**
```
Client → Serveur : "Bonjour, je supporte TLS 1.2 et 1.3,
                    voici les algorithmes de chiffrement que je connais"

Serveur → Client : "Bonjour, utilisons TLS 1.3 et
                    l'algorithme AES-256 pour chiffrer"
```

**Étape 2 : Échange de certificat**
```
Serveur → Client : "Voici mon certificat qui prouve mon identité"

Client vérifie :
- Le certificat est-il valide ?
- Est-il bien pour ce domaine ?
- Est-il signé par une autorité de confiance ?
```

**Étape 3 : Génération des clés**
```
Client et Serveur génèrent ensemble une clé de session unique
Cette clé servira à chiffrer toutes les communications
```

**Étape 4 : Communication sécurisée**
```
Client ←→ Serveur : Toutes les données sont maintenant chiffrées
                    avec la clé de session
```

### Schéma simplifié

```
┌────────┐                                    ┌────────┐
│ Client │                                    │Serveur │
└───┬────┘                                    └───┬────┘
    │                                             │
    │ 1. ClientHello (TLS 1.3, algorithmes)       │
    ├─────────────────────────────────────────────>
    │                                             │
    │ 2. ServerHello + Certificat                 │
    <─────────────────────────────────────────────┤
    │                                             │
    │ 3. Vérification certificat                  │
    │    Génération clé session                   │
    │                                             │
    │ 4. Communication chiffrée                   │
    ├────────────────────────────────────────────>
    <─────────────────────────────────────────────┤
    │                                             │
```

## Les certificats SSL/TLS

### Qu'est-ce qu'un certificat ?

Un certificat numérique est comme une carte d'identité pour un site web. Il prouve que :
- Le site est bien celui qu'il prétend être
- Les communications peuvent être chiffrées en toute sécurité

**Contenu d'un certificat** :
- Nom de domaine (ex: www.monsite.com)
- Nom de l'organisation
- Clé publique pour le chiffrement
- Date de validité (début et fin)
- Signature de l'autorité de certification

### Autorités de certification (CA)

Les **Certificate Authorities** sont des organisations de confiance qui vérifient l'identité des sites web et signent leurs certificats.

**Principales CA** :
- Let's Encrypt (gratuit, automatisé)
- DigiCert
- GlobalSign
- Sectigo
- GoDaddy

**Hiérarchie de confiance** :
```
Certificat racine (CA racine)
    └─ Certificat intermédiaire (CA intermédiaire)
        └─ Certificat du site (votre site web)
```

### Types de certificats

**1. DV (Domain Validation)** : Le plus simple et rapide
- Vérifie uniquement que vous possédez le domaine
- Gratuit avec Let's Encrypt
- Suffisant pour la plupart des sites

**2. OV (Organization Validation)** : Validation de l'organisation
- Vérifie l'existence légale de l'entreprise
- Affiche le nom de l'organisation
- Pour sites d'entreprise

**3. EV (Extended Validation)** : Validation étendue
- Vérification approfondie de l'organisation
- Barre d'adresse verte dans certains navigateurs
- Pour sites de banques, e-commerce important

**4. Wildcard** : Pour sous-domaines
- Protège *.monsite.com
- Ex: www.monsite.com, api.monsite.com, shop.monsite.com

### Obtenir un certificat avec Let's Encrypt

Let's Encrypt fournit des certificats gratuits, automatisés et reconnus par tous les navigateurs.

**Avantages** :
- ✅ Gratuit
- ✅ Automatique (renouvellement auto)
- ✅ Reconnu universellement
- ✅ Simple à configurer

**Installation avec Certbot** (sur le serveur) :
```bash
# Installation de Certbot
sudo apt-get install certbot

# Obtenir un certificat
sudo certbot certonly --webroot -w /var/www/html -d monsite.com -d www.monsite.com

# Renouvellement automatique (crontab)
0 3 * * * certbot renew --quiet
```

## Implémentation HTTPS en Delphi

### 1. Requêtes HTTPS avec TRESTClient

La manière la plus simple et moderne pour les API REST :

```pascal
uses
  REST.Client, REST.Types, System.JSON, System.SysUtils;

procedure AppelerAPISecurisee;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  // Créer les composants
  RESTClient := TRESTClient.Create('https://api.monsite.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // Configurer la requête
    RESTRequest.Resource := 'users/{id}';
    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.AddParameter('id', '123', TRESTRequestParameterKind.pkURLSEGMENT);

    // Ajouter un token d'authentification
    RESTRequest.AddAuthParameter('Authorization', 'Bearer VotreTokenJWT',
                                  TRESTRequestParameterKind.pkHTTPHEADER,
                                  [TRESTRequestParameterOption.poDoNotEncode]);

    // Exécuter (automatiquement en HTTPS)
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ShowMessage('Données reçues : ' + RESTResponse.Content);
    end
    else
    begin
      ShowMessage('Erreur ' + IntToStr(RESTResponse.StatusCode) +
                  ' : ' + RESTResponse.StatusText);
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

// Exemple avec POST de données JSON
procedure EnvoyerDonneesJSON;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONBody: TJSONObject;
begin
  RESTClient := TRESTClient.Create('https://api.monsite.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  JSONBody := TJSONObject.Create;
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := 'users';

    // Créer le corps JSON
    JSONBody.AddPair('nom', 'Dupont');
    JSONBody.AddPair('email', 'dupont@example.com');

    // Ajouter le corps à la requête
    RESTRequest.AddBody(JSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 201 then
      ShowMessage('Utilisateur créé avec succès')
    else
      ShowMessage('Erreur : ' + RESTResponse.Content);
  finally
    JSONBody.Free;
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

### 2. Connexions HTTPS avec Indy

Pour des connexions HTTP plus personnalisées :

```pascal
uses
  IdHTTP, IdSSLOpenSSL, System.SysUtils;

function TelechargerPageHTTPS(const AURL: string): string;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configurer SSL/TLS
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;

    // Options de vérification du certificat
    SSLHandler.SSLOptions.VerifyMode := [sslvrfPeer];
    SSLHandler.SSLOptions.VerifyDepth := 2;

    // Assigner le handler SSL au client HTTP
    HTTP.IOHandler := SSLHandler;

    // Effectuer la requête HTTPS
    Result := HTTP.Get(AURL);
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;

// Utilisation
procedure TForm1.BtnTelechargerClick(Sender: TObject);
var
  Contenu: string;
begin
  try
    Contenu := TelechargerPageHTTPS('https://www.example.com/api/data');
    Memo1.Lines.Text := Contenu;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

// POST avec HTTPS
procedure EnvoyerDonneesHTTPS(const AURL, ADonnees: string);
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Stream: TStringStream;
  Reponse: string;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Stream := TStringStream.Create(ADonnees, TEncoding.UTF8);
  try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;
    HTTP.IOHandler := SSLHandler;

    // Configurer les headers
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.CustomHeaders.AddValue('Authorization', 'Bearer VotreToken');

    // Envoyer les données
    Reponse := HTTP.Post(AURL, Stream);
    ShowMessage('Réponse : ' + Reponse);
  finally
    Stream.Free;
    HTTP.Free;
    SSLHandler.Free;
  end;
end;
```

### 3. Gestion des erreurs SSL/TLS

```pascal
procedure RequeteHTTPSAvecGestionErreurs;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSLHandler;

    try
      HTTP.Get('https://api.example.com/data');
    except
      on E: EIdHTTPProtocolException do
      begin
        // Erreur HTTP (404, 500, etc.)
        ShowMessage('Erreur HTTP ' + IntToStr(E.ErrorCode) + ': ' + E.Message);
      end;
      on E: EIdOSSLCouldNotLoadSSLLibrary do
      begin
        // Bibliothèques SSL manquantes
        ShowMessage('Erreur SSL : Les bibliothèques OpenSSL sont manquantes.' + sLineBreak +
                    'Veuillez installer libeay32.dll et ssleay32.dll');
      end;
      on E: EIdSSLProtocolException do
      begin
        // Problème de certificat ou protocole SSL
        ShowMessage('Erreur SSL/TLS : ' + E.Message + sLineBreak +
                    'Vérifiez le certificat du serveur.');
      end;
      on E: Exception do
      begin
        // Autre erreur
        ShowMessage('Erreur : ' + E.Message);
      end;
    end;
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;
```

### 4. Vérification des certificats

```pascal
uses
  IdSSLOpenSSL, IdX509;

procedure TForm1.VerifierCertificat;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Événement de vérification du certificat
    SSLHandler.OnVerifyPeer := SSLVerifyPeerEvent;

    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.VerifyMode := [sslvrfPeer, sslvrfFailIfNoPeerCert];
    HTTP.IOHandler := SSLHandler;

    HTTP.Get('https://www.example.com');
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;

function TForm1.SSLVerifyPeerEvent(Certificate: TIdX509; AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  // Afficher les informations du certificat
  Memo1.Lines.Add('Certificat reçu :');
  Memo1.Lines.Add('  Sujet : ' + Certificate.Subject.OneLine);
  Memo1.Lines.Add('  Émetteur : ' + Certificate.Issuer.OneLine);
  Memo1.Lines.Add('  Valide du : ' + DateTimeToStr(Certificate.notBefore));
  Memo1.Lines.Add('  Valide jusqu''au : ' + DateTimeToStr(Certificate.notAfter));

  // Vérifier la validité
  if not AOk then
  begin
    case AError of
      10: Memo1.Lines.Add('  ⚠️ Certificat expiré');
      18: Memo1.Lines.Add('  ⚠️ Certificat auto-signé');
      19: Memo1.Lines.Add('  ⚠️ CA auto-signée dans la chaîne');
      20: Memo1.Lines.Add('  ⚠️ Impossible de vérifier le certificat');
    else
      Memo1.Lines.Add('  ⚠️ Erreur de vérification : ' + IntToStr(AError));
    end;
  end;

  // En production : rejeter si non valide
  Result := AOk;

  // En développement : vous pouvez accepter (avec avertissement)
  // Result := True; // ATTENTION : Dangereux en production !
end;
```

## Sécurisation des connexions aux bases de données

### MySQL/MariaDB avec SSL

```pascal
procedure ConfigurerConnexionMySQLSSL;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=mysql.example.com');
  FDConnection1.Params.Add('Port=3306');
  FDConnection1.Params.Add('Database=mabase');
  FDConnection1.Params.Add('User_Name=utilisateur');
  FDConnection1.Params.Add('Password=motdepasse');

  // Activer SSL/TLS
  FDConnection1.Params.Add('UseSSL=True');

  // Spécifier les certificats (optionnel mais recommandé)
  FDConnection1.Params.Add('SSLCert=C:\certs\client-cert.pem');
  FDConnection1.Params.Add('SSLKey=C:\certs\client-key.pem');
  FDConnection1.Params.Add('SSLCA=C:\certs\ca-cert.pem');

  // Vérifier le certificat du serveur
  FDConnection1.Params.Add('SSLVerify=True');

  try
    FDConnection1.Connected := True;
    ShowMessage('Connexion sécurisée établie');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;

// Vérifier si la connexion est chiffrée
procedure VerifierChiffrementConnexion;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SHOW STATUS LIKE "Ssl_cipher"';
    Query.Open;

    if not Query.IsEmpty then
    begin
      if Query.FieldByName('Value').AsString <> '' then
        ShowMessage('Connexion chiffrée avec : ' + Query.FieldByName('Value').AsString)
      else
        ShowMessage('⚠️ Connexion NON chiffrée !');
    end;
  finally
    Query.Free;
  end;
end;
```

### PostgreSQL avec SSL

```pascal
procedure ConfigurerConnexionPostgreSQLSSL;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=PG');
  FDConnection1.Params.Add('Server=postgres.example.com');
  FDConnection1.Params.Add('Database=mabase');
  FDConnection1.Params.Add('User_Name=utilisateur');
  FDConnection1.Params.Add('Password=motdepasse');

  // Mode SSL
  // require : exige SSL
  // verify-ca : vérifie le certificat CA
  // verify-full : vérifie CA + nom d'hôte
  FDConnection1.Params.Add('SSLMode=require');

  // Certificats
  FDConnection1.Params.Add('SSLCert=client.crt');
  FDConnection1.Params.Add('SSLKey=client.key');
  FDConnection1.Params.Add('SSLRootCert=root.crt');

  FDConnection1.Connected := True;
end;
```

### SQL Server avec encryption

```pascal
procedure ConfigurerConnexionSQLServerChiffree;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MSSQL');
  FDConnection1.Params.Add('Server=sqlserver.example.com');
  FDConnection1.Params.Add('Database=mabase');

  // Authentification Windows ou SQL
  FDConnection1.Params.Add('OSAuthent=No');
  FDConnection1.Params.Add('User_Name=utilisateur');
  FDConnection1.Params.Add('Password=motdepasse');

  // Chiffrement activé
  FDConnection1.Params.Add('Encrypt=yes');

  // Faire confiance au certificat du serveur (développement uniquement)
  // En production, utilisez un certificat valide
  FDConnection1.Params.Add('TrustServerCertificate=no');

  FDConnection1.Connected := True;
end;
```

## Authentification par token (JWT)

Les JSON Web Tokens (JWT) sont une méthode moderne et sécurisée d'authentification pour les API.

### Structure d'un JWT

Un JWT se compose de 3 parties séparées par des points :

```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

│                                      │                                                    │                                        │
└────────── Header ────────────────────┴────────────────── Payload ─────────────────────────┴──────────── Signature ─────────────────┘
```

**Header** : Type de token et algorithme de signature
```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

**Payload** : Les données (claims)
```json
{
  "sub": "1234567890",
  "name": "John Doe",
  "iat": 1516239022,
  "exp": 1516242622
}
```

**Signature** : Garantit l'intégrité
```
HMACSHA256(
  base64UrlEncode(header) + "." +
  base64UrlEncode(payload),
  secret
)
```

### Utilisation de JWT dans Delphi

```pascal
uses
  REST.Client, System.JSON;

// Se connecter et obtenir un JWT
function SeConnecterEtObtenirToken(const ALogin, APassword: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONBody: TJSONObject;
  JSONResponse: TJSONObject;
begin
  Result := '';

  RESTClient := TRESTClient.Create('https://api.example.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  JSONBody := TJSONObject.Create;
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := 'auth/login';

    // Créer le corps de la requête
    JSONBody.AddPair('username', ALogin);
    JSONBody.AddPair('password', APassword);
    RESTRequest.AddBody(JSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter
    RESTRequest.Execute;

    // Extraire le token de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      try
        Result := JSONResponse.GetValue<string>('token');
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    JSONBody.Free;
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

// Utiliser le token pour accéder à une API protégée
procedure AccederAPIProtegee(const AToken: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://api.example.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'api/protected-data';

    // Ajouter le token JWT dans le header
    RESTRequest.AddAuthParameter('Authorization', 'Bearer ' + AToken,
                                  TRESTRequestParameterKind.pkHTTPHEADER,
                                  [TRESTRequestParameterOption.poDoNotEncode]);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
      ShowMessage('Données : ' + RESTResponse.Content)
    else if RESTResponse.StatusCode = 401 then
      ShowMessage('Token invalide ou expiré')
    else
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

// Exemple d'utilisation complète
procedure TForm1.BtnConnexionSecuriseClick(Sender: TObject);
var
  Token: string;
begin
  // 1. Se connecter et obtenir le token
  Token := SeConnecterEtObtenirToken(EditLogin.Text, EditPassword.Text);

  if Token <> '' then
  begin
    ShowMessage('Connexion réussie !');
    // 2. Stocker le token (en mémoire ou de manière sécurisée)
    FTokenJWT := Token;
    // 3. Utiliser le token pour les requêtes suivantes
    AccederAPIProtegee(Token);
  end
  else
    ShowMessage('Échec de connexion');
end;
```

## OAuth 2.0

OAuth 2.0 est le standard pour permettre aux utilisateurs de se connecter via des services tiers (Google, Facebook, Microsoft, etc.).

### Flux OAuth 2.0 simplifié

```
1. Utilisateur clique "Se connecter avec Google"
       ↓
2. Redirection vers Google avec client_id
       ↓
3. Utilisateur s'authentifie sur Google
       ↓
4. Google renvoie un code d'autorisation
       ↓
5. Échange du code contre un access_token
       ↓
6. Utilisation du token pour accéder aux API
```

### Implémentation OAuth en Delphi

```pascal
uses
  IdHTTP, IdSSLOpenSSL, System.JSON, System.NetEncoding;

type
  TOAuth2Manager = class
  private
    FClientID: string;
    FClientSecret: string;
    FRedirectURI: string;
    FAccessToken: string;
  public
    constructor Create(const AClientID, AClientSecret, ARedirectURI: string);
    function ObtenirURLAutorisation: string;
    function EchangerCodeConteToken(const ACode: string): Boolean;
    function AccederRessource(const AURL: string): string;
    property AccessToken: string read FAccessToken;
  end;

constructor TOAuth2Manager.Create(const AClientID, AClientSecret, ARedirectURI: string);
begin
  inherited Create;
  FClientID := AClientID;
  FClientSecret := AClientSecret;
  FRedirectURI := ARedirectURI;
end;

function TOAuth2Manager.ObtenirURLAutorisation: string;
begin
  // Construire l'URL d'autorisation
  Result := 'https://accounts.google.com/o/oauth2/v2/auth' +
            '?client_id=' + TNetEncoding.URL.Encode(FClientID) +
            '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectURI) +
            '&response_type=code' +
            '&scope=openid%20email%20profile';
end;

function TOAuth2Manager.EchangerCodeConteToken(const ACode: string): Boolean;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Params: TStringStream;
  Response: string;
  JSON: TJSONObject;
begin
  Result := False;
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSLHandler;
    HTTP.Request.ContentType := 'application/x-www-form-urlencoded';

    // Préparer les paramètres
    Params := TStringStream.Create(
      'code=' + TNetEncoding.URL.Encode(ACode) +
      '&client_id=' + TNetEncoding.URL.Encode(FClientID) +
      '&client_secret=' + TNetEncoding.URL.Encode(FClientSecret) +
      '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectURI) +
      '&grant_type=authorization_code'
    );
    try
      // Échanger le code contre un token
      Response := HTTP.Post('https://oauth2.googleapis.com/token', Params);

      // Extraire le token de la réponse
      JSON := TJSONObject.ParseJSONValue(Response) as TJSONObject;
      try
        if JSON.TryGetValue<string>('access_token', FAccessToken) then
          Result := True;
      finally
        JSON.Free;
      end;
    finally
      Params.Free;
    end;
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;

function TOAuth2Manager.AccederRessource(const AURL: string): string;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSLHandler;

    // Ajouter le token dans le header
    HTTP.Request.CustomHeaders.AddValue('Authorization', 'Bearer ' + FAccessToken);

    // Accéder à la ressource
    Result := HTTP.Get(AURL);
  finally
    HTTP.Free;
    SSLHandler.Free;
  end;
end;
```

## VPN et tunneling

Pour des communications ultra-sécurisées, notamment en entreprise :

### Concepts

**VPN (Virtual Private Network)** : Crée un tunnel chiffré entre votre application et le réseau distant.

```
Application Delphi → VPN Client → Internet (chiffré) → VPN Server → Réseau d'entreprise
```

**Avantages** :
- Tout le trafic est chiffré
- Masque l'origine des connexions
- Permet d'accéder aux ressources internes

### SSH Tunneling

Pour sécuriser une connexion à une base de données distante :

```
Application → Tunnel SSH local:3307 → SSH Server → MySQL Server:3306
```

**Configuration dans Delphi** :
```pascal
// Utiliser un client SSH comme composant (Indy SSH ou bibliothèque tierce)
// Établir le tunnel SSH d'abord
EtablirTunnelSSH('user@server.com', 'password', 3307, 'localhost', 3306);

// Puis se connecter à MySQL via le tunnel local
FDConnection1.Params.Add('Server=localhost');
FDConnection1.Params.Add('Port=3307'); // Port local du tunnel
FDConnection1.Connected := True;
```

## Pinning de certificat

Pour renforcer la sécurité, vous pouvez "épingler" un certificat spécifique.

**Concept** : Au lieu de faire confiance à n'importe quel certificat valide, on ne fait confiance qu'à UN certificat spécifique.

```pascal
procedure TForm1.VerifierCertificatEpingle(Certificate: TIdX509; var Accept: Boolean);
const
  EMPREINTE_ATTENDUE = 'A1:B2:C3:D4:E5:F6:...; // SHA-256 du certificat
var
  EmpreinteCertificat: string;
begin
  // Calculer l'empreinte du certificat reçu
  EmpreinteCertificat := CalculerEmpreinteSHA256(Certificate);

  // Vérifier qu'elle correspond
  Accept := (EmpreinteCertificat = EMPREINTE_ATTENDUE);

  if not Accept then
    ShowMessage('⚠️ ALERTE : Certificat non reconnu ! Possible attaque MITM.');
end;
```

**Usage** : Principalement pour les applications mobiles communiquant avec votre propre API.

## Bonnes pratiques

### ✅ À faire

**1. Toujours utiliser HTTPS en production**
```pascal
// ✅ BON
RESTClient.BaseURL := 'https://api.monsite.com';

// ❌ MAUVAIS en production
RESTClient.BaseURL := 'http://api.monsite.com';
```

**2. Utiliser TLS 1.2 ou supérieur**
```pascal
// ✅ BON
SSLHandler.SSLOptions.Method := sslvTLSv1_2;

// ❌ MAUVAIS
SSLHandler.SSLOptions.Method := sslvSSLv3; // Vulnérable
```

**3. Vérifier les certificats**
```pascal
// ✅ BON - Vérifier le certificat
SSLHandler.SSLOptions.VerifyMode := [sslvrfPeer];

// ❌ MAUVAIS - Accepter n'importe quel certificat
SSLHandler.SSLOptions.VerifyMode := [];
```

**4. Protéger les tokens**
```pascal
// ✅ BON - Token dans le header
Request.AddAuthParameter('Authorization', 'Bearer ' + Token,
                          TRESTRequestParameterKind.pkHTTPHEADER);

// ❌ MAUVAIS - Token dans l'URL (visible dans les logs)
Request.Resource := 'api/data?token=' + Token;
```

**5. Gérer l'expiration des tokens**
```pascal
// Vérifier avant chaque requête
if TokenExpire then
begin
  Token := RafraichirToken(RefreshToken);
  SauvegarderToken(Token);
end;
```

### ❌ À éviter

**1. Ne jamais désactiver la vérification SSL en production**
```pascal
// ❌ TRÈS DANGEREUX
SSLHandler.OnVerifyPeer := function(Cert: TIdX509; AOk: Boolean;
  ADepth, AError: Integer): Boolean
begin
  Result := True; // Accepte tout !
end;
```

**2. Ne pas stocker les tokens en clair**
```pascal
// ❌ MAUVAIS
IniFile.WriteString('Auth', 'Token', Token);

// ✅ BON
IniFile.WriteString('Auth', 'Token', ChiffrerToken(Token));
```

**3. Ne pas ignorer les erreurs SSL**
```pascal
// ❌ MAUVAIS
try
  HTTP.Get(URL);
except
  // Ignorer silencieusement
end;

// ✅ BON
try
  HTTP.Get(URL);
except
  on E: EIdSSLProtocolException do
  begin
    LoggerErreur('Erreur SSL : ' + E.Message);
    raise; // Propager l'exception
  end;
end;
```

**4. Ne pas mélanger HTTP et HTTPS**
```pascal
// ❌ MAUVAIS - Mélange de protocoles
PageHTTPS := 'https://monsite.com';
ImageHTTP := 'http://monsite.com/logo.png'; // Non sécurisé !

// ✅ BON - Tout en HTTPS
PageHTTPS := 'https://monsite.com';
ImageHTTPS := 'https://monsite.com/logo.png';
```

## Tester la sécurité de vos connexions

### Outils de test

**1. SSL Labs** (https://www.ssllabs.com/ssltest/)
- Teste la configuration SSL/TLS de votre serveur
- Note de A à F
- Recommandations de sécurité

**2. OWASP ZAP**
- Scanner de sécurité web
- Détecte les vulnérabilités
- Teste les connexions HTTPS

**3. Wireshark**
- Capture le trafic réseau
- Vérifie que les données sont bien chiffrées
- Analyse les protocoles

### Vérification dans Delphi

```pascal
procedure TesterConnexionSecurisee;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  InfosConnexion: TStringList;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  InfosConnexion := TStringList.Create;
  try
    SSLHandler.OnStatusInfo := procedure(const Msg: string)
    begin
      InfosConnexion.Add(Msg);
    end;

    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSLHandler;

    HTTP.Get('https://www.google.com');

    // Afficher les informations de connexion
    Memo1.Lines.Add('=== Informations de connexion ===');
    Memo1.Lines.AddStrings(InfosConnexion);
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Version TLS : ' +
      IntToStr(Ord(SSLHandler.SSLSocket.SSLVersion)));
    Memo1.Lines.Add('Cipher : ' + SSLHandler.SSLSocket.Cipher.Name);
  finally
    InfosConnexion.Free;
    HTTP.Free;
    SSLHandler.Free;
  end;
end;
```

## Checklist de sécurité des connexions

Avant de déployer votre application :

- [ ] Toutes les connexions utilisent HTTPS
- [ ] TLS 1.2 ou 1.3 est configuré
- [ ] Les certificats sont valides et vérifiés
- [ ] Les tokens sont transmis dans les headers, pas dans les URL
- [ ] Les tokens sont stockés de manière sécurisée
- [ ] L'expiration des tokens est gérée
- [ ] Les erreurs SSL/TLS sont correctement loguées
- [ ] Les connexions aux bases de données sont chiffrées
- [ ] Pas de données sensibles dans les logs
- [ ] Les certificats auto-signés sont refusés en production
- [ ] Un système de rafraîchissement des tokens existe
- [ ] Les bibliothèques SSL sont à jour

## Résumé des points essentiels

✅ **Impératifs de sécurité** :
- Utilisez TOUJOURS HTTPS, jamais HTTP pour des données sensibles
- TLS 1.2 minimum, TLS 1.3 idéalement
- Vérifiez les certificats en production
- Protégez les tokens JWT avec des headers sécurisés
- Chiffrez les connexions aux bases de données
- Gérez correctement l'expiration et le rafraîchissement des tokens

❌ **Erreurs fatales** :
- Accepter n'importe quel certificat en production
- Transmettre des identifiants en clair
- Ignorer les erreurs SSL/TLS
- Stocker des tokens non chiffrés
- Utiliser des versions obsolètes de SSL/TLS
- Mélanger HTTP et HTTPS dans la même application

🔒 **Protection maximale** :
- HTTPS + Certificat valide
- JWT avec expiration courte
- Refresh tokens pour renouveler
- Rate limiting côté serveur
- Logging des tentatives suspectes
- Monitoring actif des connexions

## Aller plus loin

Dans les sections suivantes et complémentaires :
- **16.5** : Protection contre les vulnérabilités (injection, XSS, CSRF)
- **16.6** : Audit de sécurité et journalisation
- **16.9** : Signature numérique et validation

**Ressources utiles** :
- Documentation SSL/TLS : https://www.openssl.org/docs/
- Let's Encrypt : https://letsencrypt.org/
- JWT.io : Pour décoder et valider les JWT
- SSL Labs : Pour tester vos serveurs

La sécurisation des connexions est la base d'une application moderne et fiable. Ne la négligez jamais, car c'est souvent le maillon faible que les attaquants exploitent en premier.

⏭️ [Protection contre les vulnérabilités courantes](/16-securite-des-applications/05-protection-contre-les-vulnerabilites-courantes.md)
