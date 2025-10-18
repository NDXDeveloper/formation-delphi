🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.9 Intégration avec les services cloud (AWS, Azure, Google Cloud)

## Introduction au Cloud Computing

### Qu'est-ce que le Cloud ?

Le **Cloud Computing** (informatique en nuage) est l'accès à la demande à des ressources informatiques (serveurs, stockage, bases de données, applications) via Internet, avec un paiement à l'usage.

**Analogie simple :**
Imaginez l'électricité dans votre maison. Vous n'avez pas besoin de posséder une centrale électrique : vous branchez vos appareils et payez seulement ce que vous consommez. Le cloud fonctionne de la même façon pour l'informatique.

**Avant le cloud (infrastructure traditionnelle) :**
```
┌────────────────────────────────┐
│ Votre entreprise               │
│                                │
│ ┌──────────┐  ┌──────────┐   │
│ │ Serveur 1│  │ Serveur 2│   │
│ └──────────┘  └──────────┘   │
│                                │
│ - Achat initial coûteux        │
│ - Maintenance 24/7             │
│ - Espace physique requis       │
│ - Risque de panne              │
└────────────────────────────────┘
```

**Avec le cloud :**
```
┌────────────────────┐          ┌─────────────────────┐
│ Votre application  │ ←─────→  │ Provider Cloud      │
│ Delphi             │          │ (AWS/Azure/GCP)     │
└────────────────────┘          │                     │
                                │ • Serveurs virtuels │
                                │ • Stockage          │
                                │ • Bases de données  │
                                │ • IA/ML             │
                                │ • Et bien plus...   │
                                └─────────────────────┘

Vous payez seulement ce que vous utilisez
```

### Avantages du Cloud

**1. Économies de coûts**
- Pas d'investissement initial en matériel
- Paiement à l'usage (pay-as-you-go)
- Réduction des coûts de maintenance

**2. Scalabilité**
- Ajustement automatique des ressources
- Gérer les pics de charge facilement
- Croissance sans limites

**3. Disponibilité**
- Haute disponibilité (99.9%+)
- Redondance géographique
- Sauvegardes automatiques

**4. Agilité**
- Déploiement rapide
- Expérimentation facile
- Innovation accélérée

**5. Accès mondial**
- Données accessibles partout
- Latence réduite (CDN)
- Collaboration facilitée

### Modèles de service Cloud

**IaaS (Infrastructure as a Service)**
```
Vous gérez :    Application, Données, Runtime, Middleware, OS
Provider gère :  Virtualisation, Serveurs, Storage, Network
Exemple :       EC2 (AWS), VM (Azure), Compute Engine (GCP)
```

**PaaS (Platform as a Service)**
```
Vous gérez :    Application, Données
Provider gère :  Runtime, Middleware, OS, Infrastructure
Exemple :       Elastic Beanstalk (AWS), App Service (Azure), App Engine (GCP)
```

**SaaS (Software as a Service)**
```
Vous gérez :    Rien (juste utilisation)
Provider gère :  Tout
Exemple :       Office 365, Gmail, Salesforce
```

## Les principaux providers Cloud

### Tableau comparatif

| Critère | AWS | Azure | Google Cloud |
|---------|-----|-------|--------------|
| **Part de marché** | ~32% | ~23% | ~10% |
| **Lancé** | 2006 | 2010 | 2008 |
| **Forces** | Pionnier, Mature | Intégration Microsoft | IA/ML, Analytics |
| **Régions** | 30+ | 60+ | 35+ |
| **Services** | 200+ | 200+ | 100+ |
| **Prix** | Moyen | Moyen-Élevé | Compétitif |
| **Documentation** | Excellente | Très bonne | Bonne |
| **Free Tier** | Généreux | Bon | Très bon |
| **Certificats** | Nombreux | Nombreux | En croissance |
| **Meilleur pour** | Tout | Entreprise Microsoft | Data/ML |

### AWS (Amazon Web Services)

**Services principaux :**
- **EC2** : Serveurs virtuels
- **S3** : Stockage d'objets
- **RDS** : Bases de données relationnelles
- **Lambda** : Fonctions serverless
- **DynamoDB** : Base NoSQL
- **CloudFront** : CDN
- **SageMaker** : Machine Learning

**Avantages :**
- Le plus mature et complet
- Très grande communauté
- Documentation exhaustive
- Innovations constantes

### Azure (Microsoft)

**Services principaux :**
- **Virtual Machines** : Serveurs virtuels
- **Blob Storage** : Stockage d'objets
- **SQL Database** : Bases SQL
- **Functions** : Fonctions serverless
- **Cosmos DB** : Base NoSQL globale
- **CDN** : Réseau de distribution
- **Cognitive Services** : IA prête à l'emploi

**Avantages :**
- Intégration parfaite avec l'écosystème Microsoft
- Excellent pour les entreprises Windows
- Active Directory intégré
- Hybrid Cloud facile

### Google Cloud Platform (GCP)

**Services principaux :**
- **Compute Engine** : Serveurs virtuels
- **Cloud Storage** : Stockage d'objets
- **Cloud SQL** : Bases SQL
- **Cloud Functions** : Fonctions serverless
- **Firestore** : Base NoSQL
- **Cloud CDN** : Distribution
- **AI Platform** : Machine Learning avancé

**Avantages :**
- Leader en IA/ML
- Excellent pour l'analyse de données (BigQuery)
- Infrastructure de Google
- Prix compétitifs

## Authentification et sécurité

### Concepts de sécurité Cloud

**Responsabilité partagée :**
```
┌─────────────────────────────────────┐
│ VOTRE RESPONSABILITÉ                │
│ - Données                           │
│ - Gestion des identités             │
│ - Applications                      │
│ - Système d'exploitation (IaaS)     │
├─────────────────────────────────────┤
│ RESPONSABILITÉ DU PROVIDER          │
│ - Infrastructure physique           │
│ - Réseau                            │
│ - Hyperviseur                       │
│ - Stockage physique                 │
└─────────────────────────────────────┘
```

### AWS IAM (Identity and Access Management)

**Créer un utilisateur avec accès API :**

```pascal
unit AWSAuth;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.Hash,
  System.NetEncoding;

type
  TAWSCredentials = record
    AccessKeyID: string;
    SecretAccessKey: string;
    Region: string;
  end;

  TAWSAuthHelper = class
  public
    class function SignRequest(const Credentials: TAWSCredentials;
      const Service, HTTPMethod, CanonicalURI: string;
      Headers: TStrings): string;
    class function GetAuthorizationHeader(const Credentials: TAWSCredentials;
      const Service, HTTPMethod, CanonicalURI: string;
      const DateISO: string): string;
  end;

implementation

class function TAWSAuthHelper.SignRequest(const Credentials: TAWSCredentials;
  const Service, HTTPMethod, CanonicalURI: string;
  Headers: TStrings): string;
var
  DateISO, DateStamp: string;
  CanonicalHeaders, SignedHeaders: string;
  CanonicalRequest, StringToSign: string;
  SigningKey, Signature: TBytes;
begin
  // Date au format ISO 8601
  DateISO := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));
  DateStamp := Copy(DateISO, 1, 8);

  // En-têtes canoniques
  CanonicalHeaders :=
    'host:' + Service + '.' + Credentials.Region + '.amazonaws.com' + #10 +
    'x-amz-date:' + DateISO + #10;

  SignedHeaders := 'host;x-amz-date';

  // Requête canonique
  CanonicalRequest :=
    HTTPMethod + #10 +
    CanonicalURI + #10 +
    '' + #10 +  // Query string vide
    CanonicalHeaders + #10 +
    SignedHeaders + #10 +
    THashSHA2.GetHashString('', SHA256); // Payload vide

  // Chaîne à signer
  StringToSign :=
    'AWS4-HMAC-SHA256' + #10 +
    DateISO + #10 +
    DateStamp + '/' + Credentials.Region + '/' + Service + '/aws4_request' + #10 +
    THashSHA2.GetHashString(CanonicalRequest, SHA256);

  // Clé de signature (processus complexe AWS)
  SigningKey := TEncoding.UTF8.GetBytes('AWS4' + Credentials.SecretAccessKey);
  SigningKey := THashSHA2.GetHMACAsBytes(DateStamp, SigningKey);
  SigningKey := THashSHA2.GetHMACAsBytes(Credentials.Region, SigningKey);
  SigningKey := THashSHA2.GetHMACAsBytes(Service, SigningKey);
  SigningKey := THashSHA2.GetHMACAsBytes('aws4_request', SigningKey);

  Signature := THashSHA2.GetHMACAsBytes(StringToSign, SigningKey);

  // Retourner la signature hexadécimale
  Result := TNetEncoding.Base16.Encode(Signature).ToLower;
end;

class function TAWSAuthHelper.GetAuthorizationHeader(
  const Credentials: TAWSCredentials;
  const Service, HTTPMethod, CanonicalURI, DateISO: string): string;
var
  Signature: string;
begin
  Signature := SignRequest(Credentials, Service, HTTPMethod, CanonicalURI, nil);

  Result := Format(
    'AWS4-HMAC-SHA256 Credential=%s/%s/%s/%s/aws4_request, ' +
    'SignedHeaders=host;x-amz-date, Signature=%s',
    [Credentials.AccessKeyID,
     Copy(DateISO, 1, 8),
     Credentials.Region,
     Service,
     Signature]);
end;

end.
```

### Azure Shared Access Signature (SAS)

```pascal
unit AzureAuth;

interface

uses
  System.SysUtils, System.DateUtils, System.Hash, System.NetEncoding;

type
  TAzureSASHelper = class
  public
    class function GenerateSAS(const AccountName, AccountKey: string;
      const ContainerName, BlobName: string;
      ExpiryMinutes: Integer = 60): string;
  end;

implementation

class function TAzureSASHelper.GenerateSAS(const AccountName, AccountKey: string;
  const ContainerName, BlobName: string; ExpiryMinutes: Integer): string;
var
  StartTime, ExpiryTime: TDateTime;
  StartTimeStr, ExpiryTimeStr: string;
  StringToSign, Signature: string;
  SignatureBytes, KeyBytes: TBytes;
begin
  // Calcul des dates
  StartTime := TTimeZone.Local.ToUniversalTime(Now);
  ExpiryTime := IncMinute(StartTime, ExpiryMinutes);

  StartTimeStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', StartTime);
  ExpiryTimeStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', ExpiryTime);

  // Chaîne à signer
  StringToSign := Format(
    'r'#10 +        // Permission: read
    '%s'#10 +       // Start time
    '%s'#10 +       // Expiry time
    '/blob/%s/%s/%s'#10 + // Resource
    ''#10 +         // Identifier
    ''#10 +         // IP
    'https'#10 +    // Protocol
    '2020-08-04'#10 + // Version
    ''#10 +         // Resource
    ''#10 +         // Snapshot time
    ''#10 +         // Encryption scope
    ''#10 +         // Cache control
    ''#10 +         // Content disposition
    ''#10 +         // Content encoding
    ''#10 +         // Content language
    '',             // Content type
    [StartTimeStr, ExpiryTimeStr, AccountName, ContainerName, BlobName]);

  // Calculer la signature HMAC-SHA256
  KeyBytes := TNetEncoding.Base64.DecodeStringToBytes(AccountKey);
  SignatureBytes := THashSHA2.GetHMACAsBytes(StringToSign, KeyBytes);
  Signature := TNetEncoding.Base64.EncodeBytesToString(SignatureBytes);

  // Construire l'URL SAS
  Result := Format(
    'sv=2020-08-04&st=%s&se=%s&sr=b&sp=r&sig=%s',
    [TNetEncoding.URL.Encode(StartTimeStr),
     TNetEncoding.URL.Encode(ExpiryTimeStr),
     TNetEncoding.URL.Encode(Signature)]);
end;

end.
```

### Google Cloud Service Account

```pascal
unit GoogleCloudAuth;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.DateUtils, System.Hash, System.Net.HttpClient;

type
  TGoogleCloudAuth = class
  private
    FServiceAccountEmail: string;
    FPrivateKey: string;
    FAccessToken: string;
    FTokenExpiry: TDateTime;
  public
    constructor Create(const ServiceAccountJSON: string);

    function GetAccessToken: string;
    function IsTokenValid: Boolean;

    property ServiceAccountEmail: string read FServiceAccountEmail;
  end;

implementation

uses
  System.NetConsts;

constructor TGoogleCloudAuth.Create(const ServiceAccountJSON: string);
var
  JSON: TJSONObject;
begin
  inherited Create;

  JSON := TJSONObject.ParseJSONValue(ServiceAccountJSON) as TJSONObject;
  try
    FServiceAccountEmail := JSON.GetValue<string>('client_email');
    FPrivateKey := JSON.GetValue<string>('private_key');
  finally
    JSON.Free;
  end;
end;

function TGoogleCloudAuth.IsTokenValid: Boolean;
begin
  Result := (not FAccessToken.IsEmpty) and (Now < FTokenExpiry);
end;

function TGoogleCloudAuth.GetAccessToken: string;
var
  JWT: string;
  HTTPClient: THTTPClient;
  RequestBody: TStringList;
  Response: IHTTPResponse;
  ResponseJSON: TJSONObject;
  ExpiresIn: Integer;
begin
  // Si le token est encore valide, le retourner
  if IsTokenValid then
  begin
    Result := FAccessToken;
    Exit;
  end;

  // Créer un JWT (JSON Web Token)
  JWT := CreateJWT;

  // Échanger le JWT contre un access token
  HTTPClient := THTTPClient.Create;
  RequestBody := TStringList.Create;
  try
    RequestBody.Add('grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer');
    RequestBody.Add('assertion=' + JWT);

    HTTPClient.ContentType := 'application/x-www-form-urlencoded';
    Response := HTTPClient.Post('https://oauth2.googleapis.com/token', RequestBody);

    if Response.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        FAccessToken := ResponseJSON.GetValue<string>('access_token');
        ExpiresIn := ResponseJSON.GetValue<Integer>('expires_in');
        FTokenExpiry := IncSecond(Now, ExpiresIn - 60); // Marge de 1 minute

        Result := FAccessToken;
      finally
        ResponseJSON.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur OAuth: %d', [Response.StatusCode]);

  finally
    RequestBody.Free;
    HTTPClient.Free;
  end;
end;

function CreateJWT: string;
var
  Header, Payload: TJSONObject;
  HeaderEncoded, PayloadEncoded: string;
  IssuedAt, Expiry: Int64;
begin
  // Header
  Header := TJSONObject.Create;
  try
    Header.AddPair('alg', 'RS256');
    Header.AddPair('typ', 'JWT');
    HeaderEncoded := Base64URLEncode(Header.ToString);
  finally
    Header.Free;
  end;

  // Payload
  IssuedAt := DateTimeToUnix(Now);
  Expiry := IssuedAt + 3600; // 1 heure

  Payload := TJSONObject.Create;
  try
    Payload.AddPair('iss', FServiceAccountEmail);
    Payload.AddPair('scope', 'https://www.googleapis.com/auth/cloud-platform');
    Payload.AddPair('aud', 'https://oauth2.googleapis.com/token');
    Payload.AddPair('iat', TJSONNumber.Create(IssuedAt));
    Payload.AddPair('exp', TJSONNumber.Create(Expiry));

    PayloadEncoded := Base64URLEncode(Payload.ToString);
  finally
    Payload.Free;
  end;

  // Signature avec la clé privée RSA
  // (Nécessite une bibliothèque de cryptographie RSA)
  Signature := SignWithRSA(HeaderEncoded + '.' + PayloadEncoded, FPrivateKey);

  Result := HeaderEncoded + '.' + PayloadEncoded + '.' + Signature;
end;

end.
```

## Stockage d'objets (S3, Blob Storage, Cloud Storage)

### AWS S3 (Simple Storage Service)

**Client S3 pour Delphi :**

```pascal
unit AWSS3Client;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Hash,
  System.NetEncoding, System.DateUtils, AWSAuth;

type
  TAWSS3Client = class
  private
    FCredentials: TAWSCredentials;
    FHTTPClient: THTTPClient;
    FBucketName: string;
  public
    constructor Create(const Credentials: TAWSCredentials; const BucketName: string);
    destructor Destroy; override;

    procedure UploadFile(const LocalFilePath, S3Key: string);
    procedure DownloadFile(const S3Key, LocalFilePath: string);
    procedure DeleteFile(const S3Key: string);
    function ListFiles(const Prefix: string = ''): TArray<string>;
    function GetFileURL(const S3Key: string; ExpirySeconds: Integer = 3600): string;
  end;

implementation

constructor TAWSS3Client.Create(const Credentials: TAWSCredentials;
  const BucketName: string);
begin
  inherited Create;
  FCredentials := Credentials;
  FBucketName := BucketName;
  FHTTPClient := THTTPClient.Create;
end;

destructor TAWSS3Client.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

procedure TAWSS3Client.UploadFile(const LocalFilePath, S3Key: string);
var
  FileStream: TFileStream;
  URL, DateISO, AuthHeader: string;
  Response: IHTTPResponse;
begin
  if not FileExists(LocalFilePath) then
    raise Exception.Create('Fichier introuvable: ' + LocalFilePath);

  // Construire l'URL S3
  URL := Format('https://%s.s3.%s.amazonaws.com/%s',
    [FBucketName, FCredentials.Region, S3Key]);

  DateISO := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));

  // Préparer les en-têtes
  FHTTPClient.CustomHeaders['x-amz-date'] := DateISO;
  FHTTPClient.CustomHeaders['x-amz-content-sha256'] := 'UNSIGNED-PAYLOAD';

  // Signature AWS
  AuthHeader := TAWSAuthHelper.GetAuthorizationHeader(
    FCredentials, 's3', 'PUT', '/' + S3Key, DateISO);
  FHTTPClient.CustomHeaders['Authorization'] := AuthHeader;

  // Upload
  FileStream := TFileStream.Create(LocalFilePath, fmOpenRead);
  try
    Response := FHTTPClient.Put(URL, FileStream);

    if Response.StatusCode <> 200 then
      raise Exception.CreateFmt('Erreur S3: %d - %s',
        [Response.StatusCode, Response.ContentAsString]);
  finally
    FileStream.Free;
  end;
end;

procedure TAWSS3Client.DownloadFile(const S3Key, LocalFilePath: string);
var
  FileStream: TFileStream;
  URL, DateISO, AuthHeader: string;
  Response: IHTTPResponse;
begin
  URL := Format('https://%s.s3.%s.amazonaws.com/%s',
    [FBucketName, FCredentials.Region, S3Key]);

  DateISO := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));

  FHTTPClient.CustomHeaders['x-amz-date'] := DateISO;

  AuthHeader := TAWSAuthHelper.GetAuthorizationHeader(
    FCredentials, 's3', 'GET', '/' + S3Key, DateISO);
  FHTTPClient.CustomHeaders['Authorization'] := AuthHeader;

  // Download
  FileStream := TFileStream.Create(LocalFilePath, fmCreate);
  try
    Response := FHTTPClient.Get(URL, FileStream);

    if Response.StatusCode <> 200 then
      raise Exception.CreateFmt('Erreur S3: %d', [Response.StatusCode]);
  finally
    FileStream.Free;
  end;
end;

procedure TAWSS3Client.DeleteFile(const S3Key: string);
var
  URL, DateISO, AuthHeader: string;
  Response: IHTTPResponse;
begin
  URL := Format('https://%s.s3.%s.amazonaws.com/%s',
    [FBucketName, FCredentials.Region, S3Key]);

  DateISO := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));

  FHTTPClient.CustomHeaders['x-amz-date'] := DateISO;

  AuthHeader := TAWSAuthHelper.GetAuthorizationHeader(
    FCredentials, 's3', 'DELETE', '/' + S3Key, DateISO);
  FHTTPClient.CustomHeaders['Authorization'] := AuthHeader;

  Response := FHTTPClient.Delete(URL);

  if Response.StatusCode <> 204 then
    raise Exception.CreateFmt('Erreur S3: %d', [Response.StatusCode]);
end;

function TAWSS3Client.GetFileURL(const S3Key: string;
  ExpirySeconds: Integer): string;
var
  Expiry: Int64;
  StringToSign, Signature: string;
begin
  Expiry := DateTimeToUnix(IncSecond(Now, ExpirySeconds));

  StringToSign := Format('GET'#10#10#10'%d'#10'/%s/%s',
    [Expiry, FBucketName, S3Key]);

  Signature := THashSHA2.GetHMAC(StringToSign, FCredentials.SecretAccessKey, SHA256);
  Signature := TNetEncoding.URL.Encode(
    TNetEncoding.Base64.EncodeBytesToString(
      TEncoding.UTF8.GetBytes(Signature)));

  Result := Format(
    'https://%s.s3.%s.amazonaws.com/%s?AWSAccessKeyId=%s&Expires=%d&Signature=%s',
    [FBucketName, FCredentials.Region, S3Key,
     FCredentials.AccessKeyID, Expiry, Signature]);
end;

end.
```

**Utilisation :**

```pascal
var
  Credentials: TAWSCredentials;
  S3Client: TAWSS3Client;
begin
  // Configuration
  Credentials.AccessKeyID := 'VOTRE_ACCESS_KEY';
  Credentials.SecretAccessKey := 'VOTRE_SECRET_KEY';
  Credentials.Region := 'eu-west-1'; // Paris

  S3Client := TAWSS3Client.Create(Credentials, 'mon-bucket');
  try
    // Upload d'un fichier
    S3Client.UploadFile('C:\Documents\photo.jpg', 'images/photo.jpg');
    ShowMessage('Fichier uploadé !');

    // Télécharger un fichier
    S3Client.DownloadFile('images/photo.jpg', 'C:\Temp\photo.jpg');

    // Obtenir une URL temporaire
    URL := S3Client.GetFileURL('images/photo.jpg', 3600); // 1 heure
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);

    // Supprimer
    S3Client.DeleteFile('images/photo.jpg');

  finally
    S3Client.Free;
  end;
end;
```

### Azure Blob Storage

```pascal
unit AzureBlobClient;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient,
  System.NetEncoding, AzureAuth;

type
  TAzureBlobClient = class
  private
    FAccountName: string;
    FAccountKey: string;
    FHTTPClient: THTTPClient;
  public
    constructor Create(const AccountName, AccountKey: string);
    destructor Destroy; override;

    procedure UploadBlob(const ContainerName, BlobName, LocalFilePath: string);
    procedure DownloadBlob(const ContainerName, BlobName, LocalFilePath: string);
    procedure DeleteBlob(const ContainerName, BlobName: string);
    function GetBlobURL(const ContainerName, BlobName: string): string;
  end;

implementation

constructor TAzureBlobClient.Create(const AccountName, AccountKey: string);
begin
  inherited Create;
  FAccountName := AccountName;
  FAccountKey := AccountKey;
  FHTTPClient := THTTPClient.Create;
end;

destructor TAzureBlobClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

procedure TAzureBlobClient.UploadBlob(const ContainerName, BlobName,
  LocalFilePath: string);
var
  FileStream: TFileStream;
  URL: string;
  Response: IHTTPResponse;
  DateStr: string;
  AuthHeader: string;
begin
  URL := Format('https://%s.blob.core.windows.net/%s/%s',
    [FAccountName, ContainerName, BlobName]);

  DateStr := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"',
    TTimeZone.Local.ToUniversalTime(Now));

  FileStream := TFileStream.Create(LocalFilePath, fmOpenRead);
  try
    // En-têtes Azure
    FHTTPClient.CustomHeaders['x-ms-date'] := DateStr;
    FHTTPClient.CustomHeaders['x-ms-version'] := '2020-08-04';
    FHTTPClient.CustomHeaders['x-ms-blob-type'] := 'BlockBlob';

    // Calculer l'en-tête d'autorisation
    // (Signature Azure complexe - simplifié ici)
    AuthHeader := 'SharedKey ' + FAccountName + ':SIGNATURE';
    FHTTPClient.CustomHeaders['Authorization'] := AuthHeader;

    Response := FHTTPClient.Put(URL, FileStream);

    if Response.StatusCode <> 201 then
      raise Exception.CreateFmt('Erreur Azure: %d', [Response.StatusCode]);
  finally
    FileStream.Free;
  end;
end;

procedure TAzureBlobClient.DownloadBlob(const ContainerName, BlobName,
  LocalFilePath: string);
var
  FileStream: TFileStream;
  URL, SAS: string;
  Response: IHTTPResponse;
begin
  // Générer un SAS token
  SAS := TAzureSASHelper.GenerateSAS(FAccountName, FAccountKey,
    ContainerName, BlobName);

  URL := Format('https://%s.blob.core.windows.net/%s/%s?%s',
    [FAccountName, ContainerName, BlobName, SAS]);

  FileStream := TFileStream.Create(LocalFilePath, fmCreate);
  try
    Response := FHTTPClient.Get(URL, FileStream);

    if Response.StatusCode <> 200 then
      raise Exception.CreateFmt('Erreur Azure: %d', [Response.StatusCode]);
  finally
    FileStream.Free;
  end;
end;

function TAzureBlobClient.GetBlobURL(const ContainerName, BlobName: string): string;
var
  SAS: string;
begin
  SAS := TAzureSASHelper.GenerateSAS(FAccountName, FAccountKey,
    ContainerName, BlobName, 60);

  Result := Format('https://%s.blob.core.windows.net/%s/%s?%s',
    [FAccountName, ContainerName, BlobName, SAS]);
end;

end.
```

### Google Cloud Storage

```pascal
unit GoogleCloudStorageClient;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient,
  GoogleCloudAuth;

type
  TGoogleCloudStorageClient = class
  private
    FAuth: TGoogleCloudAuth;
    FHTTPClient: THTTPClient;
    FBucketName: string;
  public
    constructor Create(Auth: TGoogleCloudAuth; const BucketName: string);
    destructor Destroy; override;

    procedure UploadFile(const LocalFilePath, ObjectName: string);
    procedure DownloadFile(const ObjectName, LocalFilePath: string);
    procedure DeleteFile(const ObjectName: string);
  end;

implementation

constructor TGoogleCloudStorageClient.Create(Auth: TGoogleCloudAuth;
  const BucketName: string);
begin
  inherited Create;
  FAuth := Auth;
  FBucketName := BucketName;
  FHTTPClient := THTTPClient.Create;
end;

destructor TGoogleCloudStorageClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

procedure TGoogleCloudStorageClient.UploadFile(const LocalFilePath,
  ObjectName: string);
var
  FileStream: TFileStream;
  URL: string;
  Response: IHTTPResponse;
begin
  URL := Format('https://storage.googleapis.com/upload/storage/v1/b/%s/o?uploadType=media&name=%s',
    [FBucketName, ObjectName]);

  FileStream := TFileStream.Create(LocalFilePath, fmOpenRead);
  try
    // Token d'authentification
    FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuth.GetAccessToken;

    Response := FHTTPClient.Post(URL, FileStream);

    if Response.StatusCode <> 200 then
      raise Exception.CreateFmt('Erreur GCS: %d', [Response.StatusCode]);
  finally
    FileStream.Free;
  end;
end;

procedure TGoogleCloudStorageClient.DownloadFile(const ObjectName,
  LocalFilePath: string);
var
  FileStream: TFileStream;
  URL: string;
  Response: IHTTPResponse;
begin
  URL := Format('https://storage.googleapis.com/storage/v1/b/%s/o/%s?alt=media',
    [FBucketName, ObjectName]);

  FileStream := TFileStream.Create(LocalFilePath, fmCreate);
  try
    FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuth.GetAccessToken;

    Response := FHTTPClient.Get(URL, FileStream);

    if Response.StatusCode <> 200 then
      raise Exception.CreateFmt('Erreur GCS: %d', [Response.StatusCode]);
  finally
    FileStream.Free;
  end;
end;

procedure TGoogleCloudStorageClient.DeleteFile(const ObjectName: string);
var
  URL: string;
  Response: IHTTPResponse;
begin
  URL := Format('https://storage.googleapis.com/storage/v1/b/%s/o/%s',
    [FBucketName, ObjectName]);

  FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuth.GetAccessToken;

  Response := FHTTPClient.Delete(URL);

  if Response.StatusCode <> 204 then
    raise Exception.CreateFmt('Erreur GCS: %d', [Response.StatusCode]);
end;

end.
```

## Bases de données Cloud

### AWS RDS (Relational Database Service)

**Connexion à une base MySQL sur RDS :**

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Phys.MySQL;

procedure ConnecterRDS;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    // Configuration RDS
    Connection.DriverName := 'MySQL';
    Connection.Params.Values['Server'] := 'mydb.abc123.eu-west-1.rds.amazonaws.com';
    Connection.Params.Values['Port'] := '3306';
    Connection.Params.Values['Database'] := 'mabase';
    Connection.Params.Values['User_Name'] := 'admin';
    Connection.Params.Values['Password'] := 'MotDePasse123!';

    // SSL/TLS pour la sécurité
    Connection.Params.Values['UseSSL'] := 'True';

    Connection.Connected := True;

    ShowMessage('Connecté à RDS !');

    // Utiliser normalement avec FireDAC

  finally
    Connection.Free;
  end;
end;
```

### Azure SQL Database

```pascal
procedure ConnecterAzureSQL;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    Connection.DriverName := 'MSSQL';
    Connection.Params.Values['Server'] := 'myserver.database.windows.net';
    Connection.Params.Values['Database'] := 'mabase';
    Connection.Params.Values['User_Name'] := 'adminuser';
    Connection.Params.Values['Password'] := 'P@ssw0rd!';

    // Chiffrement obligatoire pour Azure
    Connection.Params.Values['Encrypt'] := 'yes';
    Connection.Params.Values['TrustServerCertificate'] := 'no';

    Connection.Connected := True;

    ShowMessage('Connecté à Azure SQL !');

  finally
    Connection.Free;
  end;
end;
```

### Google Cloud SQL

```pascal
procedure ConnecterGoogleCloudSQL;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    // Via IP publique ou Cloud SQL Proxy
    Connection.DriverName := 'PG'; // PostgreSQL
    Connection.Params.Values['Server'] := '35.x.x.x'; // IP publique
    Connection.Params.Values['Port'] := '5432';
    Connection.Params.Values['Database'] := 'mabase';
    Connection.Params.Values['User_Name'] := 'postgres';
    Connection.Params.Values['Password'] := 'MotDePasse';

    Connection.Connected := True;

    ShowMessage('Connecté à Cloud SQL !');

  finally
    Connection.Free;
  end;
end;
```

## Services d'IA et Machine Learning

### AWS Rekognition (Reconnaissance d'images)

```pascal
unit AWSRekognition;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  AWSAuth;

type
  TAWSRekognition = class
  private
    FCredentials: TAWSCredentials;
    FHTTPClient: THTTPClient;
  public
    constructor Create(const Credentials: TAWSCredentials);
    destructor Destroy; override;

    function DetectLabels(const ImageFilePath: string): TJSONArray;
    function DetectFaces(const ImageFilePath: string): TJSONArray;
    function CompareFaces(const SourceImage, TargetImage: string): Double;
  end;

implementation

uses
  System.NetEncoding;

constructor TAWSRekognition.Create(const Credentials: TAWSCredentials);
begin
  inherited Create;
  FCredentials := Credentials;
  FHTTPClient := THTTPClient.Create;
end;

destructor TAWSRekognition.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TAWSRekognition.DetectLabels(const ImageFilePath: string): TJSONArray;
var
  ImageBytes: TBytes;
  ImageBase64: string;
  RequestBody: TJSONObject;
  RequestStream: TStringStream;
  Response: IHTTPResponse;
  ResponseJSON: TJSONObject;
  URL, DateISO: string;
begin
  // Lire l'image
  with TFileStream.Create(ImageFilePath, fmOpenRead) do
  try
    SetLength(ImageBytes, Size);
    ReadBuffer(ImageBytes[0], Size);
  finally
    Free;
  end;

  ImageBase64 := TNetEncoding.Base64.EncodeBytesToString(ImageBytes);

  // Préparer la requête
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('Image', TJSONObject.Create.AddPair('Bytes', ImageBase64));
    RequestBody.AddPair('MaxLabels', TJSONNumber.Create(10));
    RequestBody.AddPair('MinConfidence', TJSONNumber.Create(70));

    URL := Format('https://rekognition.%s.amazonaws.com/', [FCredentials.Region]);
    DateISO := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));

    RequestStream := TStringStream.Create(RequestBody.ToString, TEncoding.UTF8);
    try
      FHTTPClient.ContentType := 'application/x-amz-json-1.1';
      FHTTPClient.CustomHeaders['X-Amz-Target'] := 'RekognitionService.DetectLabels';
      FHTTPClient.CustomHeaders['x-amz-date'] := DateISO;

      // Authentification AWS
      FHTTPClient.CustomHeaders['Authorization'] :=
        TAWSAuthHelper.GetAuthorizationHeader(FCredentials, 'rekognition',
          'POST', '/', DateISO);

      Response := FHTTPClient.Post(URL, RequestStream);

      if Response.StatusCode = 200 then
      begin
        ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ResponseJSON.GetValue<TJSONArray>('Labels').Clone as TJSONArray;
        finally
          ResponseJSON.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur Rekognition: %d', [Response.StatusCode]);

    finally
      RequestStream.Free;
    end;
  finally
    RequestBody.Free;
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  Rekognition: TAWSRekognition;
  Labels: TJSONArray;
  i: Integer;
  Label: TJSONObject;
begin
  Rekognition := TAWSRekognition.Create(Credentials);
  try
    Labels := Rekognition.DetectLabels('C:\Photos\plage.jpg');
    try
      Memo1.Lines.Add('Objets détectés:');

      for i := 0 to Labels.Count - 1 do
      begin
        Label := Labels.Items[i] as TJSONObject;
        Memo1.Lines.Add(Format('- %s (confiance: %.1f%%)',
          [Label.GetValue<string>('Name'),
           Label.GetValue<Double>('Confidence')]));
      end;
    finally
      Labels.Free;
    end;
  finally
    Rekognition.Free;
  end;
end;
```

### Azure Cognitive Services

```pascal
unit AzureCognitiveServices;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient;

type
  TAzureComputerVision = class
  private
    FEndpoint: string;
    FSubscriptionKey: string;
    FHTTPClient: THTTPClient;
  public
    constructor Create(const Endpoint, SubscriptionKey: string);
    destructor Destroy; override;

    function AnalyzeImage(const ImageURL: string): TJSONObject;
    function OCR(const ImageFilePath: string): string;
  end;

implementation

constructor TAzureComputerVision.Create(const Endpoint, SubscriptionKey: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FSubscriptionKey := SubscriptionKey;
  FHTTPClient := THTTPClient.Create;
end;

destructor TAzureComputerVision.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TAzureComputerVision.AnalyzeImage(const ImageURL: string): TJSONObject;
var
  URL: string;
  RequestBody: TJSONObject;
  RequestStream: TStringStream;
  Response: IHTTPResponse;
begin
  URL := FEndpoint + '/vision/v3.2/analyze?visualFeatures=Categories,Description,Color,Tags';

  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('url', ImageURL);

    RequestStream := TStringStream.Create(RequestBody.ToString, TEncoding.UTF8);
    try
      FHTTPClient.ContentType := 'application/json';
      FHTTPClient.CustomHeaders['Ocp-Apim-Subscription-Key'] := FSubscriptionKey;

      Response := FHTTPClient.Post(URL, RequestStream);

      if Response.StatusCode = 200 then
        Result := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject
      else
        raise Exception.CreateFmt('Erreur Azure: %d', [Response.StatusCode]);

    finally
      RequestStream.Free;
    end;
  finally
    RequestBody.Free;
  end;
end;

function TAzureComputerVision.OCR(const ImageFilePath: string): string;
var
  URL: string;
  FileStream: TFileStream;
  Response: IHTTPResponse;
  ResponseJSON: TJSONObject;
  Regions, Lines, Words: TJSONArray;
  i, j, k: Integer;
  TextBuilder: TStringBuilder;
begin
  URL := FEndpoint + '/vision/v3.2/ocr';

  FileStream := TFileStream.Create(ImageFilePath, fmOpenRead);
  try
    FHTTPClient.ContentType := 'application/octet-stream';
    FHTTPClient.CustomHeaders['Ocp-Apim-Subscription-Key'] := FSubscriptionKey;

    Response := FHTTPClient.Post(URL, FileStream);

    if Response.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        TextBuilder := TStringBuilder.Create;
        try
          Regions := ResponseJSON.GetValue<TJSONArray>('regions');

          for i := 0 to Regions.Count - 1 do
          begin
            Lines := (Regions.Items[i] as TJSONObject).GetValue<TJSONArray>('lines');

            for j := 0 to Lines.Count - 1 do
            begin
              Words := (Lines.Items[j] as TJSONObject).GetValue<TJSONArray>('words');

              for k := 0 to Words.Count - 1 do
              begin
                TextBuilder.Append(
                  (Words.Items[k] as TJSONObject).GetValue<string>('text'));
                TextBuilder.Append(' ');
              end;

              TextBuilder.AppendLine;
            end;
          end;

          Result := TextBuilder.ToString;
        finally
          TextBuilder.Free;
        end;
      finally
        ResponseJSON.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur OCR: %d', [Response.StatusCode]);

  finally
    FileStream.Free;
  end;
end;

end.
```

### Google Cloud Vision API

```pascal
unit GoogleCloudVision;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.NetEncoding, GoogleCloudAuth;

type
  TGoogleCloudVision = class
  private
    FAuth: TGoogleCloudAuth;
    FHTTPClient: THTTPClient;
  public
    constructor Create(Auth: TGoogleCloudAuth);
    destructor Destroy; override;

    function DetectLabels(const ImageFilePath: string): TJSONArray;
    function DetectText(const ImageFilePath: string): string;
  end;

implementation

constructor TGoogleCloudVision.Create(Auth: TGoogleCloudAuth);
begin
  inherited Create;
  FAuth := Auth;
  FHTTPClient := THTTPClient.Create;
end;

destructor TGoogleCloudVision.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TGoogleCloudVision.DetectLabels(const ImageFilePath: string): TJSONArray;
var
  ImageBytes: TBytes;
  ImageBase64: string;
  RequestBody, ImageObj, Feature: TJSONObject;
  RequestsArray, FeaturesArray: TJSONArray;
  RequestStream: TStringStream;
  Response: IHTTPResponse;
  ResponseJSON: TJSONObject;
const
  API_URL = 'https://vision.googleapis.com/v1/images:annotate';
begin
  // Lire et encoder l'image
  with TFileStream.Create(ImageFilePath, fmOpenRead) do
  try
    SetLength(ImageBytes, Size);
    ReadBuffer(ImageBytes[0], Size);
  finally
    Free;
  end;

  ImageBase64 := TNetEncoding.Base64.EncodeBytesToString(ImageBytes);

  // Construire la requête
  RequestBody := TJSONObject.Create;
  try
    RequestsArray := TJSONArray.Create;

    ImageObj := TJSONObject.Create;
    ImageObj.AddPair('content', ImageBase64);

    FeaturesArray := TJSONArray.Create;
    Feature := TJSONObject.Create;
    Feature.AddPair('type', 'LABEL_DETECTION');
    Feature.AddPair('maxResults', TJSONNumber.Create(10));
    FeaturesArray.Add(Feature);

    RequestsArray.Add(
      TJSONObject.Create
        .AddPair('image', ImageObj)
        .AddPair('features', FeaturesArray));

    RequestBody.AddPair('requests', RequestsArray);

    RequestStream := TStringStream.Create(RequestBody.ToString, TEncoding.UTF8);
    try
      FHTTPClient.ContentType := 'application/json';
      FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuth.GetAccessToken;

      Response := FHTTPClient.Post(API_URL, RequestStream);

      if Response.StatusCode = 200 then
      begin
        ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ResponseJSON.GetValue<TJSONArray>('responses')
                                .Items[0].GetValue<TJSONArray>('labelAnnotations')
                                .Clone as TJSONArray;
        finally
          ResponseJSON.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur Vision API: %d', [Response.StatusCode]);

    finally
      RequestStream.Free;
    end;
  finally
    RequestBody.Free;
  end;
end;

end.
```

## Bonnes pratiques Cloud

### 1. Gérer les coûts

```pascal
// ✅ Bon - Arrêter les ressources inutilisées
procedure OptimiserCouts;
begin
  // Utiliser des instances "Spot" ou "Preemptible" pour tâches non critiques
  // Arrêter les VM en dehors des heures de travail
  // Utiliser l'auto-scaling
  // Monitorer les coûts avec CloudWatch/Azure Monitor
end;
```

### 2. Sécurité d'abord

```pascal
// ✅ Ne jamais hardcoder les credentials
// ❌ Mauvais
const
  AWS_KEY = 'AKIAI...';
  AWS_SECRET = 'wJalr...';

// ✅ Bon - Variables d'environnement ou gestionnaire de secrets
function GetAWSKey: string;
begin
  Result := GetEnvironmentVariable('AWS_ACCESS_KEY_ID');
end;
```

### 3. Gérer les erreurs réseau

```pascal
function AppelCloudAvecRetry(Operation: TProc): Boolean;
var
  Tentatives: Integer;
  Delai: Integer;
begin
  Tentatives := 0;
  Delai := 1000; // 1 seconde

  while Tentatives < 3 do
  begin
    try
      Operation();
      Result := True;
      Exit;
    except
      on E: Exception do
      begin
        Inc(Tentatives);
        if Tentatives >= 3 then
          raise;

        Sleep(Delai);
        Delai := Delai * 2; // Exponential backoff
      end;
    end;
  end;

  Result := False;
end;
```

### 4. Utiliser le cache

```pascal
type
  TCloudCache = class
  private
    FCache: TDictionary<string, TBytes>;
    FExpiry: TDictionary<string, TDateTime>;
  public
    function GetOrFetch(const Key: string;
      Fetcher: TFunc<TBytes>): TBytes;
  end;

function TCloudCache.GetOrFetch(const Key: string;
  Fetcher: TFunc<TBytes>): TBytes;
begin
  // Vérifier le cache
  if FCache.ContainsKey(Key) and (Now < FExpiry[Key]) then
    Result := FCache[Key]
  else
  begin
    // Fetch depuis le cloud
    Result := Fetcher();

    // Mettre en cache
    FCache.AddOrSetValue(Key, Result);
    FExpiry.AddOrSetValue(Key, IncMinute(Now, 10));
  end;
end;
```

### 5. Monitorer et logger

```pascal
procedure LoggerAppelCloud(const Service, Operation: string;
  const Success: Boolean; const Duration: Integer);
begin
  // Logger vers CloudWatch, Application Insights, ou Stackdriver
  WriteLn(Format('[%s] %s.%s - Success: %s, Duration: %dms',
    [DateTimeToStr(Now), Service, Operation,
     BoolToStr(Success, True), Duration]));
end;
```

### 6. Multi-région pour la résilience

```pascal
type
  TMultiRegionClient = class
  private
    FPrimaryRegion: string;
    FBackupRegion: string;
  public
    function ExecuteWithFailover(Operation: TFunc<string, Boolean>): Boolean;
  end;

function TMultiRegionClient.ExecuteWithFailover(
  Operation: TFunc<string, Boolean>): Boolean;
begin
  try
    Result := Operation(FPrimaryRegion);
  except
    // Basculer vers la région de backup
    Result := Operation(FBackupRegion);
  end;
end;
```

## Résumé

### Points clés du Cloud

✅ **Concepts fondamentaux :**
- **Cloud** = Ressources informatiques à la demande
- **IaaS/PaaS/SaaS** = Niveaux de gestion
- **Pay-as-you-go** = Paiement à l'usage
- **Scalabilité** = Ajustement automatique

✅ **Principaux providers :**
- **AWS** : Le plus mature et complet
- **Azure** : Parfait pour écosystème Microsoft
- **Google Cloud** : Leader en IA/ML et analytics

✅ **Services essentiels :**
- **Stockage** : S3, Blob Storage, Cloud Storage
- **Bases de données** : RDS, Azure SQL, Cloud SQL
- **IA/ML** : Rekognition, Cognitive Services, Vision API
- **Compute** : EC2, VMs, Compute Engine

✅ **Authentification :**
- AWS : IAM avec Access Key et Secret
- Azure : SAS tokens et Azure AD
- Google : Service Accounts et OAuth2

✅ **Implémentation Delphi :**
- API REST standard
- Signatures complexes pour AWS
- Tokens pour Azure et Google
- HTTPClient natif suffisant

✅ **Bonnes pratiques :**
- Ne jamais hardcoder les credentials
- Gérer les retry et erreurs réseau
- Utiliser le cache intelligemment
- Monitorer les coûts
- Multi-région pour résilience
- Sécurité en premier

Le cloud computing démocratise l'accès à une infrastructure professionnelle pour tous les développeurs. Avec Delphi, vous pouvez facilement intégrer ces services puissants dans vos applications !

⏭️ [WebSockets et communications temps réel](/10-communication-et-services-reseaux/10-websockets-et-communications-temps-reel.md)
