🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.7 Liaison avec des API REST tierces

## Introduction aux API REST

### Qu'est-ce qu'une API REST ?

**REST** (Representational State Transfer) est un style d'architecture pour les services web. Une **API REST** est comme un menu de restaurant : elle liste toutes les actions disponibles (endpoints) et comment les demander.

Imaginez que vous voulez commander au restaurant :
- **GET** : "Montrez-moi le menu" (lire des données)
- **POST** : "Je commande un plat" (créer des données)
- **PUT** : "Modifiez ma commande" (mettre à jour des données)
- **DELETE** : "Annulez ma commande" (supprimer des données)

### Caractéristiques de REST

**Sans état (Stateless)** : Chaque requête contient toutes les informations nécessaires. Le serveur ne garde pas de "mémoire" des requêtes précédentes.

**Basé sur HTTP** : Utilise les méthodes HTTP standard (GET, POST, PUT, DELETE).

**Format de données** : Généralement JSON, parfois XML.

**URL significatives** : Les URLs reflètent la structure des ressources (ex: `/api/utilisateurs/123`).

**Codes de statut HTTP** : Utilise les codes standard (200 = OK, 404 = Non trouvé, etc.).

### Pourquoi utiliser des API REST ?

- **Universelles** : Fonctionnent sur toutes les plateformes et tous les langages
- **Simples** : Basées sur HTTP, faciles à comprendre et utiliser
- **Flexibles** : Séparation claire entre client et serveur
- **Scalables** : Sans état, faciles à mettre à l'échelle
- **Standardisées** : Conventions communes dans toute l'industrie

## Les méthodes HTTP

### GET : Récupérer des données

La méthode GET est utilisée pour **lire** des données sans les modifier.

```pascal
uses
  System.Net.HttpClient;

function RecupererUtilisateur(UserID: Integer): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  URL := Format('https://api.example.com/users/%d', [UserID]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
      Result := Response.ContentAsString
    else
      raise Exception.CreateFmt('Erreur: %d - %s',
        [Response.StatusCode, Response.StatusText]);
  finally
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  JSON: string;
begin
  JSON := RecupererUtilisateur(123);
  Memo1.Lines.Text := JSON;
end;
```

### GET avec paramètres de requête

Les paramètres sont ajoutés à l'URL après un `?` :

```pascal
function RechercherUtilisateurs(const Nom: string; Age: Integer): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  // URL avec paramètres: /users?name=Jean&age=30
  URL := Format('https://api.example.com/users?name=%s&age=%d',
    [TNetEncoding.URL.Encode(Nom), Age]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;

// Alternative avec TURLBuilder (plus propre)
function RechercherUtilisateursV2(const Nom: string; Age: Integer): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: TURI;
begin
  URL := TURI.Create('https://api.example.com/users');

  // Ajouter les paramètres
  URL := URL.AddParameter('name', Nom);
  URL := URL.AddParameter('age', IntToStr(Age));

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL.ToString);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

### POST : Créer des données

POST envoie des données au serveur pour créer une nouvelle ressource.

```pascal
uses
  System.JSON;

function CreerUtilisateur(const Nom, Email: string; Age: Integer): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest: TJSONObject;
  PostData: TStringStream;
  Headers: TNetHeaders;
begin
  Result := False;

  // Créer le JSON
  JSONRequest := TJSONObject.Create;
  try
    JSONRequest.AddPair('name', Nom);
    JSONRequest.AddPair('email', Email);
    JSONRequest.AddPair('age', TJSONNumber.Create(Age));

    // Créer le stream avec le JSON
    PostData := TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8);
    try
      HttpClient := THTTPClient.Create;
      try
        // Définir les en-têtes
        SetLength(Headers, 1);
        Headers[0] := TNetHeader.Create('Content-Type', 'application/json');

        // Envoyer la requête POST
        Response := HttpClient.Post('https://api.example.com/users',
          PostData, nil, Headers);

        // 201 Created = succès
        Result := Response.StatusCode = 201;

        if Result then
          ShowMessage('Utilisateur créé: ' + Response.ContentAsString)
        else
          ShowMessage('Erreur: ' + IntToStr(Response.StatusCode));
      finally
        HttpClient.Free;
      end;
    finally
      PostData.Free;
    end;
  finally
    JSONRequest.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonCreerClick(Sender: TObject);  
begin  
  CreerUtilisateur('Jean Dupont', 'jean@example.com', 30);
end;
```

### PUT : Mettre à jour des données

PUT remplace complètement une ressource existante.

```pascal
function MettreAJourUtilisateur(UserID: Integer; const Nom, Email: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest: TJSONObject;
  PostData: TStringStream;
  Headers: TNetHeaders;
  URL: string;
begin
  Result := False;

  URL := Format('https://api.example.com/users/%d', [UserID]);

  JSONRequest := TJSONObject.Create;
  try
    JSONRequest.AddPair('name', Nom);
    JSONRequest.AddPair('email', Email);

    PostData := TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8);
    try
      HttpClient := THTTPClient.Create;
      try
        SetLength(Headers, 1);
        Headers[0] := TNetHeader.Create('Content-Type', 'application/json');

        Response := HttpClient.Put(URL, PostData, nil, Headers);

        Result := Response.StatusCode = 200;
      finally
        HttpClient.Free;
      end;
    finally
      PostData.Free;
    end;
  finally
    JSONRequest.Free;
  end;
end;
```

### PATCH : Mise à jour partielle

PATCH modifie seulement certains champs (pas toute la ressource).

```pascal
function ModifierEmail(UserID: Integer; const NouvelEmail: string): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest: TJSONObject;
  PostData: TStringStream;
  Headers: TNetHeaders;
  URL: string;
begin
  URL := Format('https://api.example.com/users/%d', [UserID]);

  JSONRequest := TJSONObject.Create;
  try
    // On ne change que l'email
    JSONRequest.AddPair('email', NouvelEmail);

    PostData := TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8);
    try
      HttpClient := THTTPClient.Create;
      try
        SetLength(Headers, 1);
        Headers[0] := TNetHeader.Create('Content-Type', 'application/json');

        Response := HttpClient.Patch(URL, PostData, nil, Headers);

        Result := Response.StatusCode = 200;
      finally
        HttpClient.Free;
      end;
    finally
      PostData.Free;
    end;
  finally
    JSONRequest.Free;
  end;
end;
```

### DELETE : Supprimer des données

DELETE supprime une ressource.

```pascal
function SupprimerUtilisateur(UserID: Integer): Boolean;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  URL := Format('https://api.example.com/users/%d', [UserID]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Delete(URL);

    // 204 No Content = suppression réussie
    Result := (Response.StatusCode = 200) or (Response.StatusCode = 204);

    if Result then
      ShowMessage('Utilisateur supprimé')
    else
      ShowMessage('Erreur: ' + IntToStr(Response.StatusCode));
  finally
    HttpClient.Free;
  end;
end;
```

## En-têtes HTTP

### En-têtes courants

Les en-têtes HTTP contiennent des métadonnées sur la requête ou la réponse.

```pascal
function RequeteAvecHeaders: string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 5);

    // Content-Type : Type de données envoyées
    Headers[0] := TNetHeader.Create('Content-Type', 'application/json');

    // Accept : Type de données acceptées en réponse
    Headers[1] := TNetHeader.Create('Accept', 'application/json');

    // Authorization : Authentification
    Headers[2] := TNetHeader.Create('Authorization', 'Bearer MON_TOKEN');

    // User-Agent : Identification du client
    Headers[3] := TNetHeader.Create('User-Agent', 'MonApp/1.0');

    // Custom header : En-tête personnalisé
    Headers[4] := TNetHeader.Create('X-Custom-Header', 'Valeur');

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

### Lire les en-têtes de réponse

```pascal
procedure AfficherHeadersReponse;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Header: TNetHeader;
begin
  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get('https://api.example.com/data');

    Memo1.Lines.Add('En-têtes de réponse:');
    Memo1.Lines.Add('---');

    for Header in Response.Headers do
      Memo1.Lines.Add(Header.Name + ': ' + Header.Value);

    // Accéder à un en-tête spécifique
    Memo1.Lines.Add('---');
    Memo1.Lines.Add('Content-Type: ' + Response.HeaderValue['Content-Type']);
  finally
    HttpClient.Free;
  end;
end;
```

## Codes de statut HTTP

### Comprendre les codes de statut

Les codes HTTP indiquent le résultat de la requête :

**2xx - Succès**
- 200 OK : Requête réussie
- 201 Created : Ressource créée avec succès
- 204 No Content : Succès sans contenu à retourner

**3xx - Redirection**
- 301 Moved Permanently : Ressource déplacée définitivement
- 302 Found : Redirection temporaire
- 304 Not Modified : Contenu non modifié (cache)

**4xx - Erreur client**
- 400 Bad Request : Requête invalide
- 401 Unauthorized : Authentification requise
- 403 Forbidden : Accès interdit
- 404 Not Found : Ressource introuvable
- 429 Too Many Requests : Trop de requêtes

**5xx - Erreur serveur**
- 500 Internal Server Error : Erreur serveur
- 502 Bad Gateway : Erreur de passerelle
- 503 Service Unavailable : Service temporairement indisponible

### Gérer les codes de statut

```pascal
procedure TraiterReponseHTTP(Response: IHTTPResponse);  
begin  
  case Response.StatusCode of
    200:
      ShowMessage('Succès: ' + Response.ContentAsString);

    201:
      ShowMessage('Ressource créée avec succès');

    204:
      ShowMessage('Opération réussie (pas de contenu)');

    400:
      raise Exception.Create('Requête invalide: ' + Response.ContentAsString);

    401:
      raise Exception.Create('Authentification requise');

    403:
      raise Exception.Create('Accès interdit');

    404:
      raise Exception.Create('Ressource introuvable');

    429:
      ShowMessage('Trop de requêtes, veuillez patienter');

    500..599:
      raise Exception.Create('Erreur serveur: ' + Response.StatusText);
  else
    raise Exception.CreateFmt('Code inattendu: %d', [Response.StatusCode]);
  end;
end;
```

## Authentification REST

### API Key dans l'URL

```pascal
function RequeteAvecAPIKey(const APIKey: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  URL := 'https://api.example.com/data?api_key=' + APIKey;

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

### API Key dans l'en-tête

```pascal
function RequeteAvecAPIKeyHeader(const APIKey: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('X-API-Key', APIKey);

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

### Bearer Token (OAuth 2.0)

```pascal
function RequeteAvecBearerToken(const Token: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Bearer ' + Token);

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

### Basic Authentication

```pascal
uses
  System.NetEncoding;

function RequeteAvecBasicAuth(const Username, Password: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Headers: TNetHeaders;
  Credentials: string;
begin
  // Encoder en Base64 : "username:password"
  Credentials := TNetEncoding.Base64.Encode(Username + ':' + Password);

  HttpClient := THTTPClient.Create;
  try
    SetLength(Headers, 1);
    Headers[0] := TNetHeader.Create('Authorization', 'Basic ' + Credentials);

    Response := HttpClient.Get('https://api.example.com/data', nil, Headers);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;
```

## Traitement du JSON

### Parser une réponse JSON simple

```pascal
uses
  System.JSON;

procedure ParserUtilisateur(const JSON: string);  
var  
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Nom, Email: string;
  Age: Integer;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSON);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire les valeurs
      Nom := JSONObject.GetValue<string>('name');
      Email := JSONObject.GetValue<string>('email');
      Age := JSONObject.GetValue<Integer>('age');

      ShowMessage(Format('Utilisateur: %s, %d ans, %s', [Nom, Age, Email]));
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Parser un tableau JSON

```pascal
procedure ParserListeUtilisateurs(const JSON: string);  
var  
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
  Nom: string;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSON);
  try
    if JSONValue is TJSONArray then
    begin
      JSONArray := JSONValue as TJSONArray;

      Memo1.Lines.Clear;
      Memo1.Lines.Add('Liste des utilisateurs:');

      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Items[I] as TJSONObject;
        Nom := JSONObject.GetValue<string>('name');
        Memo1.Lines.Add(Format('%d. %s', [I + 1, Nom]));
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Parser un JSON imbriqué

```pascal
// JSON: {"user": {"name": "Jean", "address": {"city": "Paris"}}}
procedure ParserJSONImbrique(const JSON: string);  
var  
  JSONRoot, JSONUser, JSONAddress: TJSONObject;
  Nom, Ville: string;
begin
  JSONRoot := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  try
    JSONUser := JSONRoot.GetValue<TJSONObject>('user');
    Nom := JSONUser.GetValue<string>('name');

    JSONAddress := JSONUser.GetValue<TJSONObject>('address');
    Ville := JSONAddress.GetValue<string>('city');

    ShowMessage(Format('%s habite à %s', [Nom, Ville]));
  finally
    JSONRoot.Free;
  end;
end;
```

### Gérer les valeurs null

```pascal
function ObtenirValeurOuDefaut(JSONObject: TJSONObject;
  const Key, Default: string): string;
var
  Value: TJSONValue;
begin
  Value := JSONObject.GetValue(Key);

  if (Value <> nil) and not (Value is TJSONNull) then
    Result := Value.Value
  else
    Result := Default;
end;

// Utilisation
procedure ParserAvecDefauts(const JSON: string);  
var  
  JSONObject: TJSONObject;
  Nom, Email: string;
begin
  JSONObject := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  try
    Nom := ObtenirValeurOuDefaut(JSONObject, 'name', 'Inconnu');
    Email := ObtenirValeurOuDefaut(JSONObject, 'email', 'Non fourni');

    ShowMessage(Format('Nom: %s, Email: %s', [Nom, Email]));
  finally
    JSONObject.Free;
  end;
end;
```

## Pagination

### Pagination par offset/limit

```pascal
function RecupererUtilisateursAvecPagination(Page, ParPage: Integer): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
  Offset: Integer;
begin
  Offset := (Page - 1) * ParPage;
  URL := Format('https://api.example.com/users?limit=%d&offset=%d',
    [ParPage, Offset]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);
    Result := Response.ContentAsString;
  finally
    HttpClient.Free;
  end;
end;

// Utilisation : récupérer la page 2 avec 10 éléments par page
procedure TForm1.Button1Click(Sender: TObject);  
var  
  JSON: string;
begin
  JSON := RecupererUtilisateursAvecPagination(2, 10);
  Memo1.Lines.Text := JSON;
end;
```

### Pagination par numéro de page

```pascal
function RecupererPage(NumeroPage, TaillePage: Integer): string;  
var  
  URL: string;
begin
  URL := Format('https://api.example.com/users?page=%d&per_page=%d',
    [NumeroPage, TaillePage]);

  Result := FaireRequeteGET(URL);
end;
```

### Pagination avec curseur

```pascal
type
  TResultatPagine = record
    Donnees: string;
    ProchainCurseur: string;
    APlusDePages: Boolean;
  end;

function RecupererAvecCurseur(const Curseur: string = ''): TResultatPagine;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse: TJSONObject;
  URL: string;
begin
  URL := 'https://api.example.com/users';
  if Curseur <> '' then
    URL := URL + '?cursor=' + Curseur;

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);

    JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Result.Donnees := JSONResponse.GetValue<TJSONArray>('data').ToString;
      Result.ProchainCurseur := JSONResponse.GetValue<string>('next_cursor');
      Result.APlusDePages := Result.ProchainCurseur <> '';
    finally
      JSONResponse.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Récupérer toutes les pages
procedure RecupererToutesLesPages;  
var  
  Resultat: TResultatPagine;
  Curseur: string;
begin
  Curseur := '';

  repeat
    Resultat := RecupererAvecCurseur(Curseur);

    // Traiter les données
    Memo1.Lines.Add('--- Page ---');
    Memo1.Lines.Add(Resultat.Donnees);

    Curseur := Resultat.ProchainCurseur;
  until not Resultat.APlusDePages;
end;
```

## Filtrage et tri

### Filtres simples

```pascal
function RechercherUtilisateurs(const Criteres: string): string;  
var  
  URL: string;
begin
  // Filtrer par critère
  URL := Format('https://api.example.com/users?status=%s',
    [TNetEncoding.URL.Encode(Criteres)]);

  Result := FaireRequeteGET(URL);
end;

// Utilisation
procedure TForm1.ButtonRechercherClick(Sender: TObject);  
var  
  Resultat: string;
begin
  Resultat := RechercherUtilisateurs('active');
  Memo1.Lines.Text := Resultat;
end;
```

### Filtres multiples

```pascal
type
  TFiltresUtilisateur = record
    Statut: string;
    AgeMin: Integer;
    AgeMax: Integer;
    Ville: string;
  end;

function RechercherAvecFiltres(const Filtres: TFiltresUtilisateur): string;  
var  
  URL: TURI;
begin
  URL := TURI.Create('https://api.example.com/users');

  if Filtres.Statut <> '' then
    URL := URL.AddParameter('status', Filtres.Statut);

  if Filtres.AgeMin > 0 then
    URL := URL.AddParameter('age_min', IntToStr(Filtres.AgeMin));

  if Filtres.AgeMax > 0 then
    URL := URL.AddParameter('age_max', IntToStr(Filtres.AgeMax));

  if Filtres.Ville <> '' then
    URL := URL.AddParameter('city', Filtres.Ville);

  Result := FaireRequeteGET(URL.ToString);
end;
```

### Tri des résultats

```pascal
function RecupererTriePar(const Champ: string; Ordre: string = 'asc'): string;  
var  
  URL: string;
begin
  URL := Format('https://api.example.com/users?sort=%s&order=%s',
    [Champ, Ordre]);

  Result := FaireRequeteGET(URL);
end;

// Utilisation
procedure TrierParNom;  
begin  
  // Tri croissant par nom
  Memo1.Lines.Text := RecupererTriePar('name', 'asc');
end;

procedure TrierParAge;  
begin  
  // Tri décroissant par âge
  Memo1.Lines.Text := RecupererTriePar('age', 'desc');
end;
```

## Gestion des erreurs API

### Classe d'erreur personnalisée

```pascal
type
  TAPIException = class(Exception)
  private
    FStatusCode: Integer;
    FErrorCode: string;
    FDetails: string;
  public
    constructor Create(StatusCode: Integer; const ErrorCode, Message, Details: string);
    property StatusCode: Integer read FStatusCode;
    property ErrorCode: string read FErrorCode;
    property Details: string read FDetails;
  end;

constructor TAPIException.Create(StatusCode: Integer;
  const ErrorCode, Message, Details: string);
begin
  inherited Create(Message);
  FStatusCode := StatusCode;
  FErrorCode := ErrorCode;
  FDetails := Details;
end;
```

### Parser les erreurs JSON

```pascal
procedure TraiterErreurAPI(Response: IHTTPResponse);  
var  
  JSONError: TJSONObject;
  ErrorCode, Message, Details: string;
begin
  if Response.StatusCode >= 400 then
  begin
    try
      JSONError := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        ErrorCode := JSONError.GetValue<string>('error_code');
        Message := JSONError.GetValue<string>('message');
        Details := JSONError.GetValue<string>('details');

        raise TAPIException.Create(Response.StatusCode, ErrorCode, Message, Details);
      finally
        JSONError.Free;
      end;
    except
      on E: Exception do
        raise TAPIException.Create(Response.StatusCode, '',
          'Erreur API', Response.ContentAsString);
    end;
  end;
end;
```

### Wrapper avec gestion d'erreurs

```pascal
function AppelerAPISafe(const URL: string): string;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Tentatives: Integer;
  Delai: Integer;
begin
  Tentatives := 0;
  Delai := 1000;

  while Tentatives < 3 do
  begin
    HttpClient := THTTPClient.Create;
    try
      try
        Response := HttpClient.Get(URL);

        case Response.StatusCode of
          200..299:
            begin
              Result := Response.ContentAsString;
              Exit;
            end;

          429: // Too Many Requests
            begin
              Inc(Tentatives);
              Sleep(Delai);
              Delai := Delai * 2; // Backoff exponentiel
              Continue;
            end;

          500..599: // Erreur serveur
            begin
              Inc(Tentatives);
              Sleep(Delai);
              Continue;
            end;
        else
          TraiterErreurAPI(Response);
        end;
      except
        on E: Exception do
        begin
          if Tentatives >= 2 then
            raise
          else
          begin
            Inc(Tentatives);
            Sleep(Delai);
          end;
        end;
      end;
    finally
      HttpClient.Free;
    end;
  end;

  raise Exception.Create('Échec après 3 tentatives');
end;
```

## Rate Limiting

### Détecter les limites de taux

```pascal
type
  TRateLimitInfo = record
    Limite: Integer;
    Restant: Integer;
    Reset: TDateTime;
  end;

function ExtraireRateLimitInfo(Response: IHTTPResponse): TRateLimitInfo;  
var  
  LimiteStr, RestantStr, ResetStr: string;
  ResetTimestamp: Int64;
begin
  // Lire les en-têtes de rate limit
  LimiteStr := Response.HeaderValue['X-RateLimit-Limit'];
  RestantStr := Response.HeaderValue['X-RateLimit-Remaining'];
  ResetStr := Response.HeaderValue['X-RateLimit-Reset'];

  if LimiteStr <> '' then
    Result.Limite := StrToIntDef(LimiteStr, 0);

  if RestantStr <> '' then
    Result.Restant := StrToIntDef(RestantStr, 0);

  if ResetStr <> '' then
  begin
    ResetTimestamp := StrToInt64Def(ResetStr, 0);
    Result.Reset := UnixToDateTime(ResetTimestamp);
  end;
end;

procedure AfficherRateLimitInfo(const Info: TRateLimitInfo);  
begin  
  ShowMessage(Format(
    'Limite: %d requêtes' + sLineBreak +
    'Restantes: %d' + sLineBreak +
    'Reset: %s',
    [Info.Limite, Info.Restant, FormatDateTime('hh:nn:ss', Info.Reset)]
  ));
end;
```

### Gestionnaire de rate limiting

```pascal
type
  TRateLimitManager = class
  private
    FDerniereRequete: TDateTime;
    FDelaiMinimum: Integer; // millisecondes
    FRestant: Integer;
    FReset: TDateTime;
  public
    constructor Create(DelaiMinimumMS: Integer);
    procedure AttendreAvantRequete;
    procedure MettreAJourLimites(const Info: TRateLimitInfo);
    function PeutFaireRequete: Boolean;
  end;

constructor TRateLimitManager.Create(DelaiMinimumMS: Integer);  
begin  
  FDelaiMinimum := DelaiMinimumMS;
  FDerniereRequete := 0;
  FRestant := -1;
end;

procedure TRateLimitManager.AttendreAvantRequete;  
var  
  Ecoule, AAttendre: Integer;
begin
  // Attendre si nécessaire entre les requêtes
  if FDerniereRequete > 0 then
  begin
    Ecoule := MilliSecondsBetween(Now, FDerniereRequete);
    if Ecoule < FDelaiMinimum then
    begin
      AAttendre := FDelaiMinimum - Ecoule;
      Sleep(AAttendre);
    end;
  end;

  // Attendre jusqu'au reset si limite atteinte
  if (FRestant = 0) and (FReset > Now) then
  begin
    AAttendre := MilliSecondsBetween(FReset, Now);
    if AAttendre > 0 then
    begin
      ShowMessage(Format('Limite atteinte. Attente de %d secondes...',
        [AAttendre div 1000]));
      Sleep(AAttendre);
    end;
  end;

  FDerniereRequete := Now;
end;

function TRateLimitManager.PeutFaireRequete: Boolean;  
begin  
  Result := (FRestant < 0) or (FRestant > 0) or (Now >= FReset);
end;

procedure TRateLimitManager.MettreAJourLimites(const Info: TRateLimitInfo);  
begin  
  FRestant := Info.Restant;
  FReset := Info.Reset;
end;
```

## WebHooks

### Recevoir des WebHooks

Les WebHooks sont des notifications HTTP que l'API envoie à votre serveur.

```pascal
uses
  IdHTTPServer, IdContext, IdCustomHTTPServer;

type
  TWebHookServer = class
  private
    FServer: TIdHTTPServer;
    procedure TraiterWebHook(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Demarrer;
    procedure Arreter;
  end;

constructor TWebHookServer.Create(Port: Integer);  
begin  
  FServer := TIdHTTPServer.Create(nil);
  FServer.DefaultPort := Port;
  FServer.OnCommandGet := TraiterWebHook;
end;

destructor TWebHookServer.Destroy;  
begin  
  FServer.Free;
  inherited;
end;

procedure TWebHookServer.TraiterWebHook(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  JSONData: TJSONObject;
  EventType, Data: string;
begin
  // Lire le JSON du webhook
  JSONData := TJSONObject.ParseJSONValue(ARequestInfo.PostStream) as TJSONObject;
  try
    EventType := JSONData.GetValue<string>('event');
    Data := JSONData.GetValue<string>('data');

    // Traiter selon le type d'événement
    if EventType = 'user.created' then
      TraiterNouvelUtilisateur(Data)
    else if EventType = 'payment.success' then
      TraiterPaiementReussi(Data)
    else if EventType = 'order.shipped' then
      TraiterCommandeExpediee(Data);

    // Répondre avec succès
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentText := '{"status":"ok"}';
  finally
    JSONData.Free;
  end;
end;
```

### Vérifier la signature des WebHooks

Les WebHooks signés garantissent l'authenticité :

```pascal
uses
  System.Hash, System.NetEncoding;

function VerifierSignatureWebHook(const Payload, Signature, Secret: string): Boolean;  
var  
  HashCalcule: string;
begin
  // Calculer HMAC-SHA256
  HashCalcule := THashSHA2.GetHMACAsString(Payload, Secret, SHA256);

  // Comparer avec la signature reçue
  Result := SameText(HashCalcule, Signature);
end;

procedure TraiterWebHookSecurise(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Signature, Payload, Secret: string;
begin
  Signature := ARequestInfo.RawHeaders.Values['X-Signature'];
  // Lire le contenu du flux POST
  ARequestInfo.PostStream.Position := 0;
  with TStreamReader.Create(ARequestInfo.PostStream, TEncoding.UTF8) do
  try
    Payload := ReadToEnd;
  finally
    Free;
  end;
  Secret := 'votre_secret_webhook';

  if not VerifierSignatureWebHook(Payload, Signature, Secret) then
  begin
    AResponseInfo.ResponseNo := 401;
    AResponseInfo.ContentText := '{"error":"Invalid signature"}';
    Exit;
  end;

  // Traiter le webhook...
  AResponseInfo.ResponseNo := 200;
end;
```

## Classe wrapper réutilisable

### Base pour client API

```pascal
type
  TAPIClient = class
  private
    FBaseURL: string;
    FAPIKey: string;
    FHttpClient: THTTPClient;
    FRateLimitManager: TRateLimitManager;
    function CreerHeaders: TNetHeaders;
    procedure TraiterReponse(Response: IHTTPResponse);
  protected
    function Get(const Endpoint: string): string;
    function Post(const Endpoint: string; const Data: TJSONObject): string;
    function Put(const Endpoint: string; const Data: TJSONObject): string;
    function Delete(const Endpoint: string): Boolean;
  public
    constructor Create(const BaseURL, APIKey: string);
    destructor Destroy; override;
  end;

constructor TAPIClient.Create(const BaseURL, APIKey: string);  
begin  
  FBaseURL := BaseURL;
  FAPIKey := APIKey;
  FHttpClient := THTTPClient.Create;
  FRateLimitManager := TRateLimitManager.Create(1000);
end;

destructor TAPIClient.Destroy;  
begin  
  FRateLimitManager.Free;
  FHttpClient.Free;
  inherited;
end;

function TAPIClient.CreerHeaders: TNetHeaders;  
begin  
  SetLength(Result, 2);
  Result[0] := TNetHeader.Create('Authorization', 'Bearer ' + FAPIKey);
  Result[1] := TNetHeader.Create('Content-Type', 'application/json');
end;

function TAPIClient.Get(const Endpoint: string): string;  
var  
  Response: IHTTPResponse;
  URL: string;
begin
  FRateLimitManager.AttendreAvantRequete;

  URL := FBaseURL + Endpoint;
  Response := FHttpClient.Get(URL, nil, CreerHeaders);

  TraiterReponse(Response);
  Result := Response.ContentAsString;
end;

function TAPIClient.Post(const Endpoint: string; const Data: TJSONObject): string;  
var  
  Response: IHTTPResponse;
  PostData: TStringStream;
  URL: string;
begin
  FRateLimitManager.AttendreAvantRequete;

  URL := FBaseURL + Endpoint;
  PostData := TStringStream.Create(Data.ToString, TEncoding.UTF8);
  try
    Response := FHttpClient.Post(URL, PostData, nil, CreerHeaders);
    TraiterReponse(Response);
    Result := Response.ContentAsString;
  finally
    PostData.Free;
  end;
end;

procedure TAPIClient.TraiterReponse(Response: IHTTPResponse);  
var  
  RateLimitInfo: TRateLimitInfo;
begin
  // Mettre à jour le rate limiting
  RateLimitInfo := ExtraireRateLimitInfo(Response);
  FRateLimitManager.MettreAJourLimites(RateLimitInfo);

  // Gérer les erreurs
  if Response.StatusCode >= 400 then
    TraiterErreurAPI(Response);
end;

// Client spécifique héritant de TAPIClient
type
  TMonAPIClient = class(TAPIClient)
  public
    constructor Create(const APIKey: string);
    function ObtenirUtilisateurs: TJSONArray;
    function CreerUtilisateur(const Nom, Email: string): TJSONObject;
  end;

constructor TMonAPIClient.Create(const APIKey: string);  
begin  
  inherited Create('https://api.example.com/v1', APIKey);
end;

function TMonAPIClient.ObtenirUtilisateurs: TJSONArray;  
var  
  JSON: string;
begin
  JSON := Get('/users');
  Result := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
end;

function TMonAPIClient.CreerUtilisateur(const Nom, Email: string): TJSONObject;  
var  
  Data: TJSONObject;
  JSON: string;
begin
  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Nom);
    Data.AddPair('email', Email);

    JSON := Post('/users', Data);
    Result := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  finally
    Data.Free;
  end;
end;
```

## Tests et débogage

### Logger les requêtes

```pascal
procedure LogRequeteAPI(const Methode, URL, Headers, Body, Response: string;
  StatusCode: Integer);
var
  Log: TStringList;
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Log := TStringList.Create;
  try
    if FileExists('api_debug.log') then
      Log.LoadFromFile('api_debug.log');

    Log.Add('=== ' + Timestamp + ' ===');
    Log.Add('Method: ' + Methode);
    Log.Add('URL: ' + URL);
    Log.Add('Headers: ' + Headers);
    if Body <> '' then
      Log.Add('Body: ' + Body);
    Log.Add('Status: ' + IntToStr(StatusCode));
    Log.Add('Response: ' + Response);
    Log.Add('');

    Log.SaveToFile('api_debug.log');
  finally
    Log.Free;
  end;
end;
```

### Mode debug

```pascal
type
  TAPIClientDebug = class(TAPIClient)
  private
    FDebugMode: Boolean;
  public
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

// Dans les méthodes, ajouter :
if FDebugMode then
  LogRequeteAPI('GET', URL, HeadersToString, '', Response.ContentAsString,
    Response.StatusCode);
```

## Résumé

Les API REST sont le standard actuel pour l'intégration de services web.

**Points clés :**

1. **Méthodes HTTP** : GET (lire), POST (créer), PUT/PATCH (modifier), DELETE (supprimer)
2. **Codes de statut** : 2xx succès, 4xx erreur client, 5xx erreur serveur
3. **Authentification** : API Key, Bearer Token, Basic Auth selon l'API
4. **JSON** : Format de données standard, facile à parser
5. **En-têtes** : Content-Type, Authorization, Accept sont essentiels
6. **Pagination** : Gérer les grandes listes (offset, page, curseur)
7. **Rate Limiting** : Respecter les limites pour éviter le blocage
8. **Gestion d'erreurs** : Toujours gérer les erreurs et réessayer si nécessaire
9. **WebHooks** : Notifications push du serveur vers votre application
10. **Wrapper** : Créer une classe réutilisable pour chaque API

Consultez toujours la documentation officielle de l'API que vous utilisez : chaque API a ses spécificités (authentification, format, limites).

⏭️ [Encapsulation d'API natives pour multi-plateformes](/14-utilisation-dapi-et-bibliotheques-externes/08-encapsulation-dapi-natives-pour-multi-plateformes.md)
