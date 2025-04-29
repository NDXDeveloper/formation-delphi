# 10.9 Intégration avec les services cloud (AWS, Azure, Google Cloud)

## Introduction

Le cloud computing a révolutionné la façon dont nous développons et déployons les applications. Les services cloud offrent de nombreux avantages : scalabilité, haute disponibilité, réduction des coûts d'infrastructure, et accès à des technologies avancées. Dans ce chapitre, nous allons découvrir comment intégrer votre application Delphi avec les principaux fournisseurs de services cloud : Amazon Web Services (AWS), Microsoft Azure et Google Cloud Platform (GCP).

## Pourquoi utiliser les services cloud ?

Avant de plonger dans le code, voyons pourquoi vous pourriez vouloir intégrer votre application Delphi avec les services cloud :

- **Stockage évolutif** : Stockez des fichiers, des images et des documents sans vous soucier de l'espace disque.
- **Bases de données managées** : Utilisez des bases de données professionnelles sans gérer l'infrastructure.
- **Services d'intelligence artificielle** : Reconnaissance d'images, traduction automatique, analyse de texte...
- **Authentification sécurisée** : Utilisez des services d'identité robustes.
- **Services de messagerie** : Envoi d'emails, SMS, notifications push...
- **Analytique et journalisation** : Surveillez et analysez l'utilisation de votre application.

## Principes généraux de l'intégration cloud

Quelle que soit la plateforme cloud choisie, certains principes sont universels :

1. **Authentification** : Vous aurez besoin de clés d'API, de jetons ou d'autres identifiants.
2. **Communication sécurisée** : Les échanges se font généralement via HTTPS.
3. **Format des données** : JSON ou XML sont les formats les plus courants.
4. **Gestion des erreurs** : Les services cloud peuvent être indisponibles ou retourner des erreurs.
5. **Coûts** : La plupart des services sont facturés en fonction de l'utilisation.

## Amazon Web Services (AWS)

AWS est l'un des leaders du marché cloud avec une large gamme de services.

### Configuration initiale pour AWS

Avant de commencer, vous devez créer un compte AWS et obtenir vos identifiants :

1. Créez un compte sur [aws.amazon.com](https://aws.amazon.com)
2. Dans la console AWS, allez dans "IAM" (Identity and Access Management)
3. Créez un nouvel utilisateur avec des accès programmatiques
4. Notez l'ID de clé d'accès (`Access Key ID`) et la clé d'accès secrète (`Secret Access Key`)

### Intégration avec Amazon S3 (Simple Storage Service)

S3 est le service de stockage d'objets d'AWS, idéal pour stocker des fichiers de toute taille.

#### Structure de base pour accéder à S3

```pascal
unit AmazonS3Client;

interface

uses
  System.SysUtils, System.Classes, System.Hash, System.DateUtils,
  System.NetEncoding, IdHTTP, IdSSLOpenSSL, IdGlobal;

type
  TS3Client = class
  private
    FHTTP: TIdHTTP;
    FAccessKey: string;
    FSecretKey: string;
    FBucketName: string;
    FRegion: string;

    function CalculateSignature(const StringToSign, DateStamp: string): string;
    function GetAuthorizationHeader(const HttpVerb, ContentType, Date, Resource: string): string;
  public
    constructor Create(const AccessKey, SecretKey, BucketName, Region: string);
    destructor Destroy; override;

    function UploadFile(const FileName, ContentType: string; FileContent: TStream): Boolean;
    function DownloadFile(const FileName: string; OutputStream: TStream): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function ListFiles(const Prefix: string = ''): TStringList;
  end;

implementation

// ... Implémentation complète à suivre
```

#### Exemple d'upload d'un fichier vers S3

```pascal
procedure TForm1.btnUploadClick(Sender: TObject);
var
  S3Client: TS3Client;
  FileStream: TFileStream;
  Success: Boolean;
begin
  // Vérifier qu'un fichier est sélectionné
  if OpenDialog1.FileName = '' then
  begin
    ShowMessage('Veuillez sélectionner un fichier à uploader.');
    Exit;
  end;

  S3Client := TS3Client.Create(
    'VOTRE_CLE_DACCES',
    'VOTRE_CLE_SECRETE',
    'nom-de-votre-bucket',
    'us-east-1'  // Région de votre bucket
  );

  try
    FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // Déterminer le type de contenu en fonction de l'extension
      var ContentType := 'application/octet-stream'; // Par défaut
      if ExtractFileExt(OpenDialog1.FileName).ToLower = '.jpg' then
        ContentType := 'image/jpeg'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.png' then
        ContentType := 'image/png'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.pdf' then
        ContentType := 'application/pdf';

      // Uploader le fichier
      Success := S3Client.UploadFile(
        ExtractFileName(OpenDialog1.FileName),
        ContentType,
        FileStream
      );

      if Success then
        ShowMessage('Fichier uploadé avec succès!')
      else
        ShowMessage('Échec de l''upload du fichier.');
    finally
      FileStream.Free;
    end;
  finally
    S3Client.Free;
  end;
end;
```

#### Implémentation de la méthode UploadFile

```pascal
function TS3Client.UploadFile(const FileName, ContentType: string; FileContent: TStream): Boolean;
var
  URL: string;
  Resource: string;
  Date: string;
  AuthHeader: string;
  Response: string;
begin
  Result := False;

  try
    // Préparer les en-têtes et l'URL
    Date := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', Now) + ' GMT';
    Resource := '/' + FBucketName + '/' + FileName;
    URL := 'https://' + FBucketName + '.s3.' + FRegion + '.amazonaws.com/' + FileName;

    // Configurer la requête HTTP
    FHTTP.Request.ContentType := ContentType;
    FHTTP.Request.CustomHeaders.Clear;
    FHTTP.Request.CustomHeaders.Add('Date: ' + Date);

    // Calculer l'en-tête d'autorisation
    AuthHeader := GetAuthorizationHeader('PUT', ContentType, Date, Resource);
    FHTTP.Request.CustomHeaders.Add('Authorization: ' + AuthHeader);

    // Envoyer la requête
    FHTTP.Put(URL, FileContent);

    // Vérifier la réponse
    Result := (FHTTP.ResponseCode >= 200) and (FHTTP.ResponseCode < 300);
  except
    on E: Exception do
    begin
      // Gérer l'erreur (dans une application réelle, vous voudriez journaliser cela)
      OutputDebugString(PChar('Erreur S3 Upload: ' + E.Message));
      Result := False;
    end;
  end;
end;
```

### Autres services AWS populaires

#### Amazon DynamoDB (Base de données NoSQL)

```pascal
procedure TForm1.btnSaveToDynamoDBClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  RequestBody, Response: string;
  RequestStream, ResponseStream: TStringStream;
  Date, AuthHeader: string;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  RequestStream := TStringStream.Create;
  ResponseStream := TStringStream.Create;

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Préparer la date pour les en-têtes
    Date := FormatDateTime('yyyymmdd', Now) + 'T' +
            FormatDateTime('hhnnss', Now) + 'Z';

    // Configurer les en-têtes
    HTTP.Request.CustomHeaders.Clear;
    HTTP.Request.ContentType := 'application/x-amz-json-1.0';
    HTTP.Request.CustomHeaders.Add('X-Amz-Date: ' + Date);
    HTTP.Request.CustomHeaders.Add('X-Amz-Target: DynamoDB_20120810.PutItem');

    // Créer l'en-tête d'autorisation (simplifié pour l'exemple)
    AuthHeader := 'AWS4-HMAC-SHA256 Credential=...';
    HTTP.Request.CustomHeaders.Add('Authorization: ' + AuthHeader);

    // Préparer le corps de la requête (un exemple simplifié)
    RequestBody :=
      '{' +
      '  "TableName": "Users",' +
      '  "Item": {' +
      '    "UserID": {"S": "123"},' +
      '    "Name": {"S": "John Doe"},' +
      '    "Email": {"S": "john@example.com"}' +
      '  }' +
      '}';

    RequestStream.WriteString(RequestBody);
    RequestStream.Position := 0;

    // Envoyer la requête
    HTTP.Post('https://dynamodb.us-east-1.amazonaws.com/', RequestStream, ResponseStream);

    // Traiter la réponse
    Response := ResponseStream.DataString;
    ShowMessage('Données enregistrées avec succès!');
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  finally
    HTTP.Free;
    SSL.Free;
    RequestStream.Free;
    ResponseStream.Free;
  end;
end;
```

#### Amazon SQS (Simple Queue Service)

```pascal
procedure TForm1.btnSendToQueueClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  Params: TStringList;
  Response: string;
  ResponseStream: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Params := TStringList.Create;
  ResponseStream := TStringStream.Create;

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Préparer les paramètres
    Params.Add('Action=SendMessage');
    Params.Add('QueueUrl=https://sqs.us-east-1.amazonaws.com/123456789012/MyQueue');
    Params.Add('MessageBody=' + TNetEncoding.URL.Encode('Votre message ici'));
    Params.Add('Version=2012-11-05');

    // Ajouter des en-têtes d'authentification (simplifiés pour l'exemple)
    HTTP.Request.CustomHeaders.Clear;
    HTTP.Request.CustomHeaders.Add('Authorization: ...');

    // Envoyer la requête
    HTTP.Post('https://sqs.us-east-1.amazonaws.com/', Params, ResponseStream);

    // Traiter la réponse
    Response := ResponseStream.DataString;
    ShowMessage('Message envoyé à la file d''attente!');
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  finally
    HTTP.Free;
    SSL.Free;
    Params.Free;
    ResponseStream.Free;
  end;
end;
```

## Microsoft Azure

Azure est la plateforme cloud de Microsoft, parfaitement intégrée avec les autres services Microsoft.

### Configuration initiale pour Azure

1. Créez un compte sur [azure.microsoft.com](https://azure.microsoft.com)
2. Dans le portail Azure, créez un "App Registration" pour obtenir un ID client et un secret client
3. Attribuez les rôles appropriés à votre application

### Stockage Azure Blob

Le service Blob Storage d'Azure est similaire à S3 d'AWS.

```pascal
unit AzureBlobClient;

interface

uses
  System.SysUtils, System.Classes, System.Hash, System.DateUtils,
  System.NetEncoding, IdHTTP, IdSSLOpenSSL, IdGlobal;

type
  TAzureBlobClient = class
  private
    FHTTP: TIdHTTP;
    FAccountName: string;
    FAccountKey: string;
    FContainerName: string;

    function GetAuthorizationHeader(const HttpVerb, ContentType,
                                   Date, Resource: string): string;
  public
    constructor Create(const AccountName, AccountKey, ContainerName: string);
    destructor Destroy; override;

    function UploadBlob(const BlobName, ContentType: string;
                       BlobContent: TStream): Boolean;
    function DownloadBlob(const BlobName: string; OutputStream: TStream): Boolean;
    function DeleteBlob(const BlobName: string): Boolean;
    function ListBlobs(const Prefix: string = ''): TStringList;
  end;

implementation

constructor TAzureBlobClient.Create(const AccountName, AccountKey, ContainerName: string);
begin
  inherited Create;
  FAccountName := AccountName;
  FAccountKey := AccountKey;
  FContainerName := ContainerName;

  FHTTP := TIdHTTP.Create(nil);
  FHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
  TIdSSLIOHandlerSocketOpenSSL(FHTTP.IOHandler).SSLOptions.Method := sslvTLSv1_2;
end;

destructor TAzureBlobClient.Destroy;
begin
  FHTTP.IOHandler.Free;
  FHTTP.Free;
  inherited;
end;

function TAzureBlobClient.UploadBlob(const BlobName, ContentType: string;
                                   BlobContent: TStream): Boolean;
var
  URL: string;
  Date: string;
  AuthHeader: string;
  Resource: string;
begin
  Result := False;

  try
    // Préparer les en-têtes et l'URL
    Date := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', Now) + ' GMT';
    Resource := '/' + FAccountName + '/' + FContainerName + '/' + BlobName;
    URL := 'https://' + FAccountName + '.blob.core.windows.net/' +
           FContainerName + '/' + BlobName;

    // Configurer la requête HTTP
    FHTTP.Request.ContentType := ContentType;
    FHTTP.Request.CustomHeaders.Clear;
    FHTTP.Request.CustomHeaders.Add('x-ms-date: ' + Date);
    FHTTP.Request.CustomHeaders.Add('x-ms-version: 2020-04-08');
    FHTTP.Request.CustomHeaders.Add('x-ms-blob-type: BlockBlob');

    // Calculer l'en-tête d'autorisation
    AuthHeader := GetAuthorizationHeader('PUT', ContentType, Date, Resource);
    FHTTP.Request.CustomHeaders.Add('Authorization: ' + AuthHeader);

    // Envoyer la requête
    FHTTP.Put(URL, BlobContent);

    // Vérifier la réponse
    Result := (FHTTP.ResponseCode >= 200) and (FHTTP.ResponseCode < 300);
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('Erreur Azure Blob Upload: ' + E.Message));
      Result := False;
    end;
  end;
end;

// La version simplifiée pour l'exemple - dans un cas réel,
// il faudrait implémenter la signature HMAC-SHA256 complète
function TAzureBlobClient.GetAuthorizationHeader(const HttpVerb, ContentType,
                                               Date, Resource: string): string;
var
  StringToSign: string;
begin
  StringToSign :=
    HttpVerb + #10 +                          // HTTP Verb
    '' + #10 +                                // Content-Encoding
    '' + #10 +                                // Content-Language
    IntToStr(BlobContent.Size) + #10 +        // Content-Length
    '' + #10 +                                // Content-MD5
    ContentType + #10 +                       // Content-Type
    '' + #10 +                                // Date
    '' + #10 +                                // If-Modified-Since
    '' + #10 +                                // If-Match
    '' + #10 +                                // If-None-Match
    '' + #10 +                                // If-Unmodified-Since
    '' + #10 +                                // Range
    'x-ms-blob-type:BlockBlob' + #10 +        // Custom headers
    'x-ms-date:' + Date + #10 +
    'x-ms-version:2020-04-08' + #10 +
    Resource;                                 // Resource path

  // En pratique, nous devrions signer StringToSign avec l'AccountKey
  // en utilisant HMAC-SHA256, puis encoder le résultat en Base64
  Result := 'SharedKey ' + FAccountName + ':' + 'SignatureIciEnBase64';
end;
```

### Exemple d'upload vers Azure Blob Storage

```pascal
procedure TForm1.btnUploadToAzureClick(Sender: TObject);
var
  BlobClient: TAzureBlobClient;
  FileStream: TFileStream;
  Success: Boolean;
begin
  // Vérifier qu'un fichier est sélectionné
  if OpenDialog1.FileName = '' then
  begin
    ShowMessage('Veuillez sélectionner un fichier à uploader.');
    Exit;
  end;

  BlobClient := TAzureBlobClient.Create(
    'votre-compte-stockage',
    'votre-cle-acces',
    'nom-de-votre-conteneur'
  );

  try
    FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // Déterminer le type de contenu
      var ContentType := 'application/octet-stream'; // Par défaut
      if ExtractFileExt(OpenDialog1.FileName).ToLower = '.jpg' then
        ContentType := 'image/jpeg'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.png' then
        ContentType := 'image/png'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.pdf' then
        ContentType := 'application/pdf';

      // Uploader le fichier
      Success := BlobClient.UploadBlob(
        ExtractFileName(OpenDialog1.FileName),
        ContentType,
        FileStream
      );

      if Success then
        ShowMessage('Fichier uploadé vers Azure avec succès!')
      else
        ShowMessage('Échec de l''upload du fichier vers Azure.');
    finally
      FileStream.Free;
    end;
  finally
    BlobClient.Free;
  end;
end;
```

### Utilisation d'Azure Cognitive Services pour la reconnaissance d'images

```pascal
procedure TForm1.btnAnalyzeImageClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  RequestStream, ResponseStream: TMemoryStream;
  ResponseText: string;
  JsonResponse: TJSONObject;
begin
  if OpenPictureDialog1.Execute then
  begin
    HTTP := TIdHTTP.Create(nil);
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    RequestStream := TMemoryStream.Create;
    ResponseStream := TMemoryStream.Create;

    try
      // Charger l'image
      RequestStream.LoadFromFile(OpenPictureDialog1.FileName);
      RequestStream.Position := 0;

      // Configuration SSL
      SSL.SSLOptions.Method := sslvTLSv1_2;
      HTTP.IOHandler := SSL;

      // Configurer les en-têtes
      HTTP.Request.ContentType := 'application/octet-stream';
      HTTP.Request.CustomHeaders.Clear;
      HTTP.Request.CustomHeaders.Add('Ocp-Apim-Subscription-Key: votre-cle-cognitive-services');

      // Envoyer la requête
      HTTP.Post(
        'https://votre-ressource.cognitiveservices.azure.com/vision/v3.2/analyze?visualFeatures=Tags,Description,Objects',
        RequestStream,
        ResponseStream
      );

      // Traiter la réponse
      ResponseStream.Position := 0;
      SetLength(ResponseText, ResponseStream.Size);
      ResponseStream.ReadBuffer(ResponseText[1], ResponseStream.Size);

      // Analyser le JSON
      JsonResponse := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
      try
        var Description := (JsonResponse.GetValue('description') as TJSONObject)
                           .GetValue('captions') as TJSONArray;
        var Caption := (Description.Items[0] as TJSONObject).GetValue('text').Value;
        var Confidence := (Description.Items[0] as TJSONObject).GetValue('confidence').Value;

        Memo1.Lines.Clear;
        Memo1.Lines.Add('Description: ' + Caption);
        Memo1.Lines.Add('Confiance: ' + Confidence);

        // Afficher les tags
        Memo1.Lines.Add('Tags:');
        var Tags := JsonResponse.GetValue('tags') as TJSONArray;
        for var I := 0 to Min(5, Tags.Count - 1) do
        begin
          var Tag := Tags.Items[I] as TJSONObject;
          Memo1.Lines.Add('- ' + Tag.GetValue('name').Value + ' (' +
                         Tag.GetValue('confidence').Value + ')');
        end;
      finally
        JsonResponse.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    finally
      HTTP.Free;
      SSL.Free;
      RequestStream.Free;
      ResponseStream.Free;
    end;
  end;
end;
```

## Google Cloud Platform (GCP)

Google Cloud offre également une large gamme de services adaptés pour de nombreux cas d'utilisation.

### Configuration initiale pour GCP

1. Créez un compte sur [cloud.google.com](https://cloud.google.com)
2. Créez un projet dans la console Google Cloud
3. Créez des identifiants (clé API, compte de service...)

### Stockage avec Google Cloud Storage

```pascal
unit GoogleCloudStorageClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils,
  IdHTTP, IdSSLOpenSSL, IdGlobal, IdCoderMIME;

type
  TGoogleCloudStorageClient = class
  private
    FHTTP: TIdHTTP;
    FBucketName: string;
    FApiKey: string;
    // Pour l'authentification complète, nous aurions besoin du jeton JWT
  public
    constructor Create(const BucketName, ApiKey: string);
    destructor Destroy; override;

    function UploadObject(const ObjectName: string;
                         ContentType: string; Content: TStream): Boolean;
    function DownloadObject(const ObjectName: string; Output: TStream): Boolean;
    function DeleteObject(const ObjectName: string): Boolean;
    function ListObjects(const Prefix: string = ''): TStringList;
  end;

implementation

constructor TGoogleCloudStorageClient.Create(const BucketName, ApiKey: string);
begin
  inherited Create;
  FBucketName := BucketName;
  FApiKey := ApiKey;

  FHTTP := TIdHTTP.Create(nil);
  FHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
  TIdSSLIOHandlerSocketOpenSSL(FHTTP.IOHandler).SSLOptions.Method := sslvTLSv1_2;
end;

destructor TGoogleCloudStorageClient.Destroy;
begin
  FHTTP.IOHandler.Free;
  FHTTP.Free;
  inherited;
end;

function TGoogleCloudStorageClient.UploadObject(const ObjectName: string;
                                              ContentType: string; Content: TStream): Boolean;
var
  URL: string;
  ResponseContent: string;
  ResponseStream: TStringStream;
begin
  Result := False;

  try
    // En pratique, nous aurions besoin d'un jeton d'authentification OAuth 2.0
    // et non d'une simple clé API, mais ceci est simplifié pour l'exemple
    URL := 'https://storage.googleapis.com/upload/storage/v1/b/' +
           FBucketName + '/o?uploadType=media&name=' +
           TNetEncoding.URL.Encode(ObjectName) + '&key=' + FApiKey;

    // Configurer la requête HTTP
    FHTTP.Request.ContentType := ContentType;
    FHTTP.Request.CustomHeaders.Clear;

    // Créer le stream pour la réponse
    ResponseStream := TStringStream.Create;
    try
      // Envoyer la requête
      FHTTP.Post(URL, Content, ResponseStream);

      // Vérifier la réponse
      ResponseContent := ResponseStream.DataString;
      Result := (FHTTP.ResponseCode >= 200) and (FHTTP.ResponseCode < 300);
    finally
      ResponseStream.Free;
    end;
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('Erreur GCP Upload: ' + E.Message));
      Result := False;
    end;
  end;
end;
```

### Utilisation de l'API Vision de Google Cloud

```pascal
procedure TForm1.btnAnalyzeWithGoogleClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  RequestBody, ResponseText: string;
  RequestStream, ResponseStream: TStringStream;
  Image64: string;
  ImageStream: TMemoryStream;
  JsonRequest, JsonResponse: TJSONObject;
  Features: TJSONArray;
begin
  if OpenPictureDialog1.Execute then
  begin
    HTTP := TIdHTTP.Create(nil);
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    ImageStream := TMemoryStream.Create;

    try
      // Charger l'image et convertir en Base64
      ImageStream.LoadFromFile(OpenPictureDialog1.FileName);
      Image64 := TIdEncoderMIME.EncodeStream(ImageStream);

      // Préparer la requête JSON
      JsonRequest := TJSONObject.Create;
      try
        var Requests := TJSONArray.Create;
        var Request := TJSONObject.Create;

        // Ajouter l'image
        var Image := TJSONObject.Create;
        Image.AddPair('content', Image64);
        Request.AddPair('image', Image);

        // Ajouter les fonctionnalités demandées
        Features := TJSONArray.Create;
        var Feature := TJSONObject.Create;
        Feature.AddPair('type', 'LABEL_DETECTION');
        Feature.AddPair('maxResults', TJSONNumber.Create(10));
        Features.Add(Feature);

        // Ajouter une autre fonctionnalité
        Feature := TJSONObject.Create;
        Feature.AddPair('type', 'TEXT_DETECTION');
        Features.Add(Feature);

        Request.AddPair('features', Features);
        Requests.Add(Request);
        JsonRequest.AddPair('requests', Requests);

        // Créer le stream de requête
        RequestBody := JsonRequest.ToString;
        RequestStream := TStringStream.Create(RequestBody);
      finally
        JsonRequest.Free;
      end;

      // Configuration SSL
      SSL.SSLOptions.Method := sslvTLSv1_2;
      HTTP.IOHandler := SSL;

      // Configurer les en-têtes
      HTTP.Request.ContentType := 'application/json';
      HTTP.Request.CustomHeaders.Clear;

      // Créer le stream de réponse
      ResponseStream := TStringStream.Create;

      try
        // Envoyer la requête
        HTTP.Post(
          'https://vision.googleapis.com/v1/images:annotate?key=VOTRE_CLE_API',
          RequestStream,
          ResponseStream
        );

        // Traiter la réponse
        ResponseText := ResponseStream.DataString;
        JsonResponse := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

        try
          var Responses := JsonResponse.GetValue('responses') as TJSONArray;
          var Response := Responses.Items[0] as TJSONObject;

          // Récupérer les étiquettes (labels)
          Memo1.Lines.Clear;
          Memo1.Lines.Add('Étiquettes détectées:');
          if Response.TryGetValue<TJSONArray>('labelAnnotations', TJSONArray(var Labels)) then
          begin
            for var I := 0 to Labels.Count - 1 do
            begin
              var Label := Labels.Items[I] as TJSONObject;
              Memo1.Lines.Add('- ' + Label.GetValue('description').Value + ' (' +
                             FormatFloat('0.00',
                             (Label.GetValue('score') as TJSONNumber).AsDouble * 100) + '%)');
            end;
          end;


        // Récupérer le texte
        if Response.TryGetValue<TJSONArray>('textAnnotations', TJSONArray(var Texts))
        and (Texts.Count > 0) then
        begin
        var Text := Texts.Items[0] as TJSONObject;
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Texte détecté:');
        Memo1.Lines.Add(Text.GetValue('description').Value);
        end;

        finally
        JsonResponse.Free;
        end;

        except
        on E: Exception do
            ShowMessage('Erreur: ' + E.Message);
        finally
        HTTP.Free;
        SSL.Free;
        ImageStream.Free;
        RequestStream.Free;
        ResponseStream.Free;
        end;
end;
```

## Création d'une bibliothèque réutilisable pour les services cloud

Pour faciliter l'intégration avec les services cloud, nous allons créer une bibliothèque réutilisable avec une interface commune pour les trois principaux fournisseurs cloud.

### Interface commune pour les opérations de stockage

```pascal
unit CloudStorageInterface;

interface

uses
  System.SysUtils, System.Classes;

type
  ICloudStorageService = interface
    ['{B2A3D729-F6B2-4A1B-9C18-02F9D8B78D5E}']
    function UploadFile(const FileName, ContentType: string;
                         Content: TStream): Boolean;
    function DownloadFile(const FileName: string; Output: TStream): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function ListFiles(const Prefix: string = ''): TStringList;
    function GetPublicURL(const FileName: string): string;
  end;

  TCloudStorageFactory = class
  public
    class function CreateAWSStorage(const AccessKey, SecretKey, BucketName,
                                    Region: string): ICloudStorageService;
    class function CreateAzureStorage(const AccountName, AccountKey,
                                     ContainerName: string): ICloudStorageService;
    class function CreateGoogleStorage(const BucketName, ApiKey,
                                      ServiceAccountJson: string): ICloudStorageService;
  end;

implementation

uses
  AWSStorageService, AzureStorageService, GoogleStorageService;

class function TCloudStorageFactory.CreateAWSStorage(const AccessKey, SecretKey,
                                                     BucketName, Region: string): ICloudStorageService;
begin
  Result := TAWSStorageService.Create(AccessKey, SecretKey, BucketName, Region);
end;

class function TCloudStorageFactory.CreateAzureStorage(const AccountName, AccountKey,
                                                      ContainerName: string): ICloudStorageService;
begin
  Result := TAzureStorageService.Create(AccountName, AccountKey, ContainerName);
end;

class function TCloudStorageFactory.CreateGoogleStorage(const BucketName, ApiKey,
                                                       ServiceAccountJson: string): ICloudStorageService;
begin
  Result := TGoogleStorageService.Create(BucketName, ApiKey, ServiceAccountJson);
end;

end.
```

### Implémentation pour AWS S3

```pascal
unit AWSStorageService;

interface

uses
  System.SysUtils, System.Classes, CloudStorageInterface, System.Hash, System.DateUtils,
  System.NetEncoding, IdHTTP, IdSSLOpenSSL, IdGlobal;

type
  TAWSStorageService = class(TInterfacedObject, ICloudStorageService)
  private
    FHTTP: TIdHTTP;
    FAccessKey: string;
    FSecretKey: string;
    FBucketName: string;
    FRegion: string;

    function CalculateSignature(const StringToSign, DateStamp: string): string;
    function GetAuthorizationHeader(const HttpVerb, ContentType, Date, Resource: string): string;
  public
    constructor Create(const AccessKey, SecretKey, BucketName, Region: string);
    destructor Destroy; override;

    // Implémentation de l'interface
    function UploadFile(const FileName, ContentType: string; Content: TStream): Boolean;
    function DownloadFile(const FileName: string; Output: TStream): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function ListFiles(const Prefix: string = ''): TStringList;
    function GetPublicURL(const FileName: string): string;
  end;

implementation

// Implémentation... (comme montré dans les exemples précédents)
```

### Implémentation pour Azure Blob Storage

```pascal
unit AzureStorageService;

interface

uses
  System.SysUtils, System.Classes, CloudStorageInterface, System.Hash, System.DateUtils,
  System.NetEncoding, IdHTTP, IdSSLOpenSSL, IdGlobal;

type
  TAzureStorageService = class(TInterfacedObject, ICloudStorageService)
  private
    FHTTP: TIdHTTP;
    FAccountName: string;
    FAccountKey: string;
    FContainerName: string;

    function GetAuthorizationHeader(const HttpVerb, ContentType,
                                   Date, Resource: string): string;
  public
    constructor Create(const AccountName, AccountKey, ContainerName: string);
    destructor Destroy; override;

    // Implémentation de l'interface
    function UploadFile(const FileName, ContentType: string; Content: TStream): Boolean;
    function DownloadFile(const FileName: string; Output: TStream): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function ListFiles(const Prefix: string = ''): TStringList;
    function GetPublicURL(const FileName: string): string;
  end;

implementation

// Implémentation... (comme montré dans les exemples précédents)
```

### Utilisation de l'interface commune

```pascal
procedure TForm1.btnUploadToCloudClick(Sender: TObject);
var
  StorageService: ICloudStorageService;
  FileStream: TFileStream;
  CloudProvider: string;
  Success: Boolean;
  PublicURL: string;
begin
  // Vérifier qu'un fichier est sélectionné
  if OpenDialog1.FileName = '' then
  begin
    ShowMessage('Veuillez sélectionner un fichier à uploader.');
    Exit;
  end;

  // Obtenir le fournisseur cloud sélectionné
  if RadioGroup1.ItemIndex = 0 then // AWS
  begin
    StorageService := TCloudStorageFactory.CreateAWSStorage(
      edtAccessKey.Text,
      edtSecretKey.Text,
      edtBucketName.Text,
      edtRegion.Text
    );
    CloudProvider := 'AWS S3';
  end
  else if RadioGroup1.ItemIndex = 1 then // Azure
  begin
    StorageService := TCloudStorageFactory.CreateAzureStorage(
      edtAccountName.Text,
      edtAccountKey.Text,
      edtContainerName.Text
    );
    CloudProvider := 'Azure Blob Storage';
  end
  else // Google Cloud
  begin
    StorageService := TCloudStorageFactory.CreateGoogleStorage(
      edtGoogleBucket.Text,
      edtApiKey.Text,
      edtServiceAccountPath.Text
    );
    CloudProvider := 'Google Cloud Storage';
  end;

  try
    FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // Déterminer le type de contenu
      var ContentType := 'application/octet-stream'; // Par défaut
      if ExtractFileExt(OpenDialog1.FileName).ToLower = '.jpg' then
        ContentType := 'image/jpeg'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.png' then
        ContentType := 'image/png'
      else if ExtractFileExt(OpenDialog1.FileName).ToLower = '.pdf' then
        ContentType := 'application/pdf';

      // Uploader le fichier
      Success := StorageService.UploadFile(
        ExtractFileName(OpenDialog1.FileName),
        ContentType,
        FileStream
      );

      if Success then
      begin
        PublicURL := StorageService.GetPublicURL(ExtractFileName(OpenDialog1.FileName));

        ShowMessage('Fichier uploadé vers ' + CloudProvider + ' avec succès!' +
                    #13#10 + 'URL publique: ' + PublicURL);

        // Copier l'URL dans le presse-papiers
        Clipboard.AsText := PublicURL;
      end
      else
        ShowMessage('Échec de l''upload du fichier vers ' + CloudProvider + '.');
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

## Authentification et sécurité

L'authentification est un aspect critique de l'intégration avec les services cloud. Voici comment gérer l'authentification pour chaque fournisseur.

### Authentification AWS

AWS utilise généralement la signature Signature Version 4 (SigV4) pour authentifier les requêtes.

```pascal
function CalculateAWSSignature(const StringToSign, DateStamp, Region, Service, SecretKey: string): string;
var
  KDate, KRegion, KService, KSigning, Signature: TBytes;
  HmacSha256: THMACSHA256;
begin
  // Calculer la clé de signature
  KDate := HMACSHA256(TEncoding.UTF8.GetBytes('AWS4' + SecretKey), TEncoding.UTF8.GetBytes(DateStamp));
  KRegion := HMACSHA256(KDate, TEncoding.UTF8.GetBytes(Region));
  KService := HMACSHA256(KRegion, TEncoding.UTF8.GetBytes(Service));
  KSigning := HMACSHA256(KService, TEncoding.UTF8.GetBytes('aws4_request'));

  // Calculer la signature
  Signature := HMACSHA256(KSigning, TEncoding.UTF8.GetBytes(StringToSign));

  // Convertir en hexadécimal
  Result := BytesToHex(Signature);
end;

// Fonction auxiliaire pour le calcul HMAC-SHA256
function HMACSHA256(const Key, Data: TBytes): TBytes;
var
  HmacSha256: THMACSHA256;
begin
  HmacSha256 := THMACSHA256.Create(Key);
  try
    Result := HmacSha256.HashData(Data);
  finally
    HmacSha256.Free;
  end;
end;

// Fonction auxiliaire pour convertir des bytes en chaîne hexadécimale
function BytesToHex(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
  Result := LowerCase(Result);
end;
```

### Authentification Azure

Azure utilise souvent la signature partagée (SAS) pour l'authentification.

```pascal
function GenerateAzureSASToken(const AccountName, AccountKey, ContainerName,
                               BlobName: string; ExpiryHours: Integer = 1): string;
var
  StartTime, ExpiryTime: TDateTime;
  StringToSign, Signature, SignatureEncoded: string;
  DecodedKey: TBytes;
  HmacSha256: THMACSHA256;
begin
  // Définir les heures de début et d'expiration
  StartTime := Now;
  ExpiryTime := StartTime + (ExpiryHours / 24);

  // Créer la chaîne à signer
  StringToSign :=
    AccountName + #10 +
    'r' + #10 +  // Permissions (r = read)
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', StartTime) + #10 +
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', ExpiryTime) + #10 +
    '/' + AccountName + '/' + ContainerName;

  if BlobName <> '' then
    StringToSign := StringToSign + '/' + BlobName;

  StringToSign := StringToSign + #10 +
    '' + #10 +  // id de la politique stockée
    '2020-04-08'; // version de l'API

  // Décoder la clé
  DecodedKey := TNetEncoding.Base64.DecodeStringToBytes(AccountKey);

  // Calculer la signature
  HmacSha256 := THMACSHA256.Create(DecodedKey);
  try
    Signature := TNetEncoding.Base64.EncodeBytesToString(
      HmacSha256.HashData(TEncoding.UTF8.GetBytes(StringToSign))
    );
  finally
    HmacSha256.Free;
  end;

  // Encoder la signature pour l'URL
  SignatureEncoded := TNetEncoding.URL.Encode(Signature);

  // Créer le jeton SAS
  Result := 'sv=2020-04-08' +
            '&sr=b' +  // ressource (b = blob)
            '&sp=r' +  // permissions (r = read)
            '&st=' + TNetEncoding.URL.Encode(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', StartTime)) +
            '&se=' + TNetEncoding.URL.Encode(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', ExpiryTime)) +
            '&spr=https' +  // protocole
            '&sig=' + SignatureEncoded;
end;
```

### Authentification Google Cloud

Google Cloud utilise généralement OAuth 2.0 avec des jetons JWT pour l'authentification.

```pascal
function GenerateGoogleCloudJWT(const ServiceAccountJson: string;
                               const Scope: string; ExpiryMinutes: Integer = 60): string;
var
  JsonObj: TJSONObject;
  Header, Claims, PrivateKey, StringToSign, Signature: string;
  SignatureBytes: TBytes;
  RSA: TRSA;
begin
  // Analyser le fichier JSON du compte de service
  JsonObj := TJSONObject.ParseJSONValue(ServiceAccountJson) as TJSONObject;
  try
    // Créer l'en-tête JWT
    Header := TNetEncoding.Base64URL.Encode(
      '{"alg":"RS256","typ":"JWT"}'
    );

    // Créer les revendications (claims) JWT
    Claims := TJSONObject.Create;
    try
      Claims.AddPair('iss', JsonObj.GetValue('client_email').Value);
      Claims.AddPair('scope', Scope);
      Claims.AddPair('aud', 'https://oauth2.googleapis.com/token');

      var IssueTime := Round((Now - EncodeDate(1970, 1, 1)) * 24 * 60 * 60);
      Claims.AddPair('iat', TJSONNumber.Create(IssueTime));
      Claims.AddPair('exp', TJSONNumber.Create(IssueTime + ExpiryMinutes * 60));

      var ClaimsBase64 := TNetEncoding.Base64URL.Encode(Claims.ToString);
    finally
      Claims.Free;
    end;

    // Récupérer la clé privée
    PrivateKey := JsonObj.GetValue('private_key').Value;

    // Créer la chaîne à signer
    StringToSign := Header + '.' + ClaimsBase64;

    // Signer avec RSA SHA-256
    RSA := TRSA.Create;
    try
      RSA.LoadPrivateKey(PrivateKey);
      SignatureBytes := RSA.SignData(TEncoding.UTF8.GetBytes(StringToSign), SHA256);
      Signature := TNetEncoding.Base64URL.EncodeBytesToString(SignatureBytes);
    finally
      RSA.Free;
    end;

    // Assembler le JWT complet
    Result := StringToSign + '.' + Signature;
  finally
    JsonObj.Free;
  end;
end;

// Cette fonction est simplifiée et nécessite des bibliothèques supplémentaires pour fonctionner
// dans une application réelle.
```

## Fonctionnalités supplémentaires des services cloud

### Notifications avec AWS SNS (Simple Notification Service)

```pascal
procedure TForm1.btnSendNotificationClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  Params: TStringList;
  Response: string;
  ResponseStream: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Params := TStringList.Create;
  ResponseStream := TStringStream.Create;

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Préparer les paramètres
    Params.Add('Action=Publish');
    Params.Add('TopicArn=arn:aws:sns:us-east-1:123456789012:MyTopic');
    Params.Add('Message=' + TNetEncoding.URL.Encode(Memo1.Text));
    Params.Add('Subject=' + TNetEncoding.URL.Encode(edtSubject.Text));
    Params.Add('Version=2010-03-31');

    // Ajouter des en-têtes d'authentification (simplifiés pour l'exemple)
    HTTP.Request.CustomHeaders.Clear;
    HTTP.Request.CustomHeaders.Add('Authorization: ...');

    // Envoyer la requête
    HTTP.Post('https://sns.us-east-1.amazonaws.com/', Params, ResponseStream);

    // Traiter la réponse
    Response := ResponseStream.DataString;
    ShowMessage('Notification envoyée avec succès!');
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  finally
    HTTP.Free;
    SSL.Free;
    Params.Free;
    ResponseStream.Free;
  end;
end;
```

### Traduction automatique avec Google Cloud Translation

```pascal
procedure TForm1.btnTranslateClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  RequestBody, ResponseText: string;
  RequestStream, ResponseStream: TStringStream;
  JsonRequest, JsonResponse: TJSONObject;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Configurer les en-têtes
    HTTP.Request.ContentType := 'application/json; charset=utf-8';
    HTTP.Request.CustomHeaders.Clear;

    // Préparer la requête JSON
    JsonRequest := TJSONObject.Create;
    try
      JsonRequest.AddPair('q', Memo1.Text);
      JsonRequest.AddPair('source', 'fr');
      JsonRequest.AddPair('target', 'en');
      JsonRequest.AddPair('format', 'text');

      RequestBody := JsonRequest.ToString;
      RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    finally
      JsonRequest.Free;
    end;

    // Créer le stream de réponse
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);

    try
      // Envoyer la requête
      HTTP.Post(
        'https://translation.googleapis.com/language/translate/v2?key=VOTRE_CLE_API',
        RequestStream,
        ResponseStream
      );

      // Traiter la réponse
      ResponseText := ResponseStream.DataString;
      JsonResponse := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

      try
        var Data := JsonResponse.GetValue('data') as TJSONObject;
        var Translations := Data.GetValue('translations') as TJSONArray;

        if Translations.Count > 0 then
        begin
          var Translation := Translations.Items[0] as TJSONObject;
          Memo2.Text := Translation.GetValue('translatedText').Value;
        end;
      finally
        JsonResponse.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HTTP.Free;
    SSL.Free;
  end;
end;
```

### Service de file d'attente Azure Storage Queue

```pascal
procedure TForm1.btnEnqueueMessageClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  URL, Message64, Date, AuthHeader: string;
  RequestStream, ResponseStream: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Encoder le message en Base64
    Message64 := TNetEncoding.Base64.Encode(Memo1.Text);

    // Préparer l'URL
    URL := Format('https://%s.queue.core.windows.net/%s/messages',
      [edtStorageAccount.Text, edtQueueName.Text]);

    // Préparer les en-têtes
    Date := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', Now) + ' GMT';
    HTTP.Request.CustomHeaders.Clear;
    HTTP.Request.CustomHeaders.Add('x-ms-date: ' + Date);
    HTTP.Request.CustomHeaders.Add('x-ms-version: 2020-04-08');

    // Calculer l'en-tête d'autorisation (simplifié pour l'exemple)
    AuthHeader := 'SharedKey ' + edtStorageAccount.Text + ':SignatureIciEnBase64';
    HTTP.Request.CustomHeaders.Add('Authorization: ' + AuthHeader);

    // Préparer le corps de la requête
    RequestStream := TStringStream.Create(
      '<QueueMessage><MessageText>' + Message64 + '</MessageText></QueueMessage>',
      TEncoding.UTF8
    );

    // Créer le stream de réponse
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);

    try
      // Envoyer la requête
      HTTP.Post(URL, RequestStream, ResponseStream);

      // Vérifier la réponse
      if (HTTP.ResponseCode >= 200) and (HTTP.ResponseCode < 300) then
        ShowMessage('Message ajouté à la file d''attente avec succès!')
      else
        ShowMessage('Erreur: ' + ResponseStream.DataString);
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HTTP.Free;
    SSL.Free;
  end;
end;
```

## Bonnes pratiques pour l'intégration cloud

### 1. Sécurité des identifiants

Ne stockez jamais vos identifiants cloud en dur dans votre code. Utilisez plutôt :

```pascal
procedure TForm1.LoadCloudCredentials;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'config.ini');
  try
    // Charger les identifiants AWS
    edtAccessKey.Text := IniFile.ReadString('AWS', 'AccessKey', '');
    edtSecretKey.Text := IniFile.ReadString('AWS', 'SecretKey', '');
    edtBucketName.Text := IniFile.ReadString('AWS', 'BucketName', '');
    edtRegion.Text := IniFile.ReadString('AWS', 'Region', 'us-east-1');

    // Charger les identifiants Azure
    edtAccountName.Text := IniFile.ReadString('Azure', 'AccountName', '');
    edtAccountKey.Text := IniFile.ReadString('Azure', 'AccountKey', '');
    edtContainerName.Text := IniFile.ReadString('Azure', 'ContainerName', '');

    // Charger les identifiants Google Cloud
    edtGoogleBucket.Text := IniFile.ReadString('Google', 'BucketName', '');
    edtApiKey.Text := IniFile.ReadString('Google', 'ApiKey', '');
    edtServiceAccountPath.Text := IniFile.ReadString('Google', 'ServiceAccountPath', '');
  finally
    IniFile.Free;
  end;
end;
```

### 2. Gestion des erreurs et des retries

```pascal
function RetryOperation(const Operation: TFunc<Boolean>;
                        MaxRetries: Integer = 3; InitialDelayMs: Integer = 500): Boolean;
var
  RetryCount: Integer;
  DelayMs: Integer;
begin
  RetryCount := 0;
  DelayMs := InitialDelayMs;
  Result := False;

  while (not Result) and (RetryCount < MaxRetries) do
  begin
    try
      Result := Operation();

      if not Result then
      begin
        Inc(RetryCount);
        Sleep(DelayMs);
        DelayMs := DelayMs * 2; // Backoff exponentiel
      end;
    except
      on E: Exception do
      begin
        // Journaliser l'erreur
        LogError('Tentative ' + IntToStr(RetryCount + 1) + ': ' + E.Message);

        Inc(RetryCount);

        if RetryCount >= MaxRetries then
          raise
        else
        begin
          Sleep(DelayMs);
          DelayMs := DelayMs * 2;
        end;
      end;
    end;
  end;
end;
```

### 3. Surveillance et journalisation

```pascal
procedure LogCloudOperation(const Provider, Operation, Resource: string;
                            Success: Boolean; const ErrorMsg: string = '');
var
  LogFile: TStreamWriter;
  LogEntry: string;
begin
  try
    LogFile := TStreamWriter.Create(ExtractFilePath(Application.ExeName) + 'cloud_log.txt', True);
    try
      LogEntry := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' | ' +
                  Provider + ' | ' +
                  Operation + ' | ' +
                  Resource + ' | ' +
                  BoolToStr(Success, True);

      if (not Success) and (ErrorMsg <> '') then
        LogEntry := LogEntry + ' | ' + ErrorMsg;

      LogFile.WriteLine(LogEntry);
    finally
      LogFile.Free;
    end;
  except
    // En cas d'erreur lors de la journalisation, ne pas planter l'application
  end;
end;
```

### 4. Économie de coûts

```pascal
// Vérifier la taille des fichiers avant upload
function CanUploadFile(const FileName: string; MaxSizeMB: Integer = 10): Boolean;
var
  FileInfo: TFileInfo;
begin
  FileInfo := TFile.GetFileInfo(FileName);
  Result := FileInfo.Size <= Int64(MaxSizeMB) * 1024 * 1024;

  if not Result then
    ShowMessage('Le fichier dépasse la taille maximale de ' +
                IntToStr(MaxSizeMB) + ' Mo autorisée.');
end;

// Utiliser des URL signées temporaires plutôt que des fichiers publics
procedure TForm1.btnGetTemporaryURLClick(Sender: TObject);
var
  ExpiryHours: Integer;
  TempURL: string;
begin
  ExpiryHours := StrToIntDef(edtExpiryHours.Text, 1);

  if RadioGroup1.ItemIndex = 0 then // AWS
  begin
    // Générer une URL présignée AWS S3
    TempURL := GeneratePresignedS3URL(
      edtAccessKey.Text,
      edtSecretKey.Text,
      edtBucketName.Text,
      edtRegion.Text,
      edtFileName.Text,
      ExpiryHours
    );
  end
  else if RadioGroup1.ItemIndex = 1 then // Azure
  begin
    // Générer un jeton SAS Azure
    TempURL := GenerateAzureBlobSASURL(
      edtAccountName.Text,
      edtAccountKey.Text,
      edtContainerName.Text,
      edtFileName.Text,
      ExpiryHours
    );
  end
  else // Google Cloud
  begin
    // Générer une URL signée Google Cloud Storage
    TempURL := GenerateGoogleStorageSignedURL(
      edtGoogleBucket.Text,
      edtServiceAccountPath.Text,
      edtFileName.Text,
      ExpiryHours
    );
  end;

  if TempURL <> '' then
  begin
    edtTemporaryURL.Text := TempURL;
    Clipboard.AsText := TempURL;
    ShowMessage('URL temporaire générée et copiée dans le presse-papiers.');
  end;
end;
```

## Conseils pour le déploiement en production

### 1. Environnements séparés

Utilisez des environnements séparés pour le développement, les tests et la production.

```pascal
type
  TEnvironment = (envDevelopment, envTest, envProduction);

function GetCloudConfiguration(Env: TEnvironment): TCloudConfig;
begin
  case Env of
    envDevelopment:
      begin
        Result.AWS.BucketName := 'mon-app-dev';
        Result.Azure.ContainerName := 'monapp-dev';
        Result.Google.BucketName := 'mon-app-dev';
        // Autres configurations pour l'environnement de développement
      end;

    envTest:
      begin
        Result.AWS.BucketName := 'mon-app-test';
        Result.Azure.ContainerName := 'monapp-test';
        Result.Google.BucketName := 'mon-app-test';
        // Autres configurations pour l'environnement de test
      end;

    envProduction:
      begin
        Result.AWS.BucketName := 'mon-app-prod';
        Result.Azure.ContainerName := 'monapp-prod';
        Result.Google.BucketName := 'mon-app-prod';
        // Autres configurations pour l'environnement de production
      end;
  end;

  // Charger les identifiants à partir d'un fichier chiffré ou d'un coffre-fort
  LoadSensitiveCredentials(Env, Result);
end;
```

### 2. Chiffrement des données sensibles

Chiffrez toujours les données sensibles avant de les stocker dans le cloud.

```pascal
procedure UploadSensitiveFile(CloudService: ICloudStorageService;
                              const FilePath, DestFileName: string);
var
  OriginalStream, EncryptedStream: TStream;
  Cipher: TCipher_Rijndael;
  Key, IV: TBytes;
begin
  // Charger le fichier original
  OriginalStream := TFileStream.Create(FilePath, fmOpenRead);
  try
    // Préparer le chiffrement
    Key := GetEncryptionKey;  // Récupérer la clé depuis un endroit sécurisé
    IV := GenerateRandomBytes(16);  // Vecteur d'initialisation aléatoire

    // Créer le flux chiffré
    EncryptedStream := TMemoryStream.Create;
    try
      // Ajouter l'IV au début du flux (nécessaire pour le déchiffrement)
      EncryptedStream.WriteBuffer(IV[0], Length(IV));

      // Chiffrer les données
      Cipher := TCipher_Rijndael.Create;
      try
        Cipher.Mode := cmCBC;
        Cipher.Init(Key, IV);
        Cipher.EncryptStream(OriginalStream, EncryptedStream, OriginalStream.Size);
      finally
        Cipher.Free;
      end;

      // Remettre le pointeur au début
      EncryptedStream.Position := 0;

      // Upload vers le cloud
      CloudService.UploadFile(DestFileName, 'application/octet-stream', EncryptedStream);
    finally
      EncryptedStream.Free;
    end;
  finally
    OriginalStream.Free;
  end;
end;

function DownloadAndDecryptFile(CloudService: ICloudStorageService;
                               const SourceFileName, DestFilePath: string): Boolean;
var
  EncryptedStream, DecryptedStream: TStream;
  Cipher: TCipher_Rijndael;
  Key, IV: TBytes;
begin
  Result := False;

  // Créer un flux pour les données chiffrées
  EncryptedStream := TMemoryStream.Create;
  try
    // Télécharger le fichier chiffré
    if not CloudService.DownloadFile(SourceFileName, EncryptedStream) then
      Exit;

    // Vérifier qu'il y a assez de données
    if EncryptedStream.Size <= 16 then
      Exit;

    // Retourner au début du flux
    EncryptedStream.Position := 0;

    // Lire l'IV (les 16 premiers octets)
    SetLength(IV, 16);
    EncryptedStream.ReadBuffer(IV[0], 16);

    // Récupérer la clé depuis un endroit sécurisé
    Key := GetEncryptionKey;

    // Créer le flux déchiffré
    DecryptedStream := TFileStream.Create(DestFilePath, fmCreate);
    try
      // Déchiffrer les données
      Cipher := TCipher_Rijndael.Create;
      try
        Cipher.Mode := cmCBC;
        Cipher.Init(Key, IV);
        Cipher.DecryptStream(EncryptedStream, DecryptedStream, EncryptedStream.Size - 16);
      finally
        Cipher.Free;
      end;

      Result := True;
    finally
      DecryptedStream.Free;
    end;
  finally
    EncryptedStream.Free;
  end;
end;
```

### 3. Gestion des quotas et des limites

Soyez attentif aux quotas et limites des services cloud et gérez-les de manière proactive.

```pascal
type
  TQuotaManager = class
  private
    FLastChecks: TDictionary<string, TDateTime>;
    FUsages: TDictionary<string, Integer>;
    FLimits: TDictionary<string, Integer>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLimit(const ResourceName: string; Limit: Integer);
    function CanUseResource(const ResourceName: string; Amount: Integer = 1): Boolean;
    procedure RecordUsage(const ResourceName: string; Amount: Integer = 1);
    procedure ResetDailyCounters;
  end;

constructor TQuotaManager.Create;
begin
  inherited;
  FLastChecks := TDictionary<string, TDateTime>.Create;
  FUsages := TDictionary<string, Integer>.Create;
  FLimits := TDictionary<string, Integer>.Create;
end;

destructor TQuotaManager.Destroy;
begin
  FLastChecks.Free;
  FUsages.Free;
  FLimits.Free;
  inherited;
end;

procedure TQuotaManager.SetLimit(const ResourceName: string; Limit: Integer);
begin
  FLimits.AddOrSetValue(ResourceName, Limit);

  if not FUsages.ContainsKey(ResourceName) then
    FUsages.Add(ResourceName, 0);

  if not FLastChecks.ContainsKey(ResourceName) then
    FLastChecks.Add(ResourceName, Date);
end;

function TQuotaManager.CanUseResource(const ResourceName: string; Amount: Integer): Boolean;
var
  CurrentUsage, Limit: Integer;
  LastCheck: TDateTime;
begin
  // Vérifier si la ressource est gérée
  if not FLimits.TryGetValue(ResourceName, Limit) then
  begin
    Result := True;  // Non gérée, donc on autorise
    Exit;
  end;

  // Vérifier si nous avons changé de jour
  if FLastChecks.TryGetValue(ResourceName, LastCheck) then
  begin
    if Trunc(Now) > Trunc(LastCheck) then
    begin
      // Nouveau jour, remettre le compteur à zéro
      FUsages[ResourceName] := 0;
      FLastChecks[ResourceName] := Now;
    end;
  end;

  // Vérifier l'utilisation actuelle
  if FUsages.TryGetValue(ResourceName, CurrentUsage) then
    Result := (CurrentUsage + Amount) <= Limit
  else
    Result := Amount <= Limit;
end;

procedure TQuotaManager.RecordUsage(const ResourceName: string; Amount: Integer);
var
  CurrentUsage: Integer;
begin
  if FUsages.TryGetValue(ResourceName, CurrentUsage) then
    FUsages[ResourceName] := CurrentUsage + Amount
  else
    FUsages.Add(ResourceName, Amount);

  if not FLastChecks.ContainsKey(ResourceName) then
    FLastChecks.Add(ResourceName, Now);
end;

procedure TQuotaManager.ResetDailyCounters;
var
  ResourceName: string;
begin
  for ResourceName in FUsages.Keys.ToArray do
  begin
    FUsages[ResourceName] := 0;
    FLastChecks[ResourceName] := Now;
  end;
end;
```

### 4. Surveillance et alertes

Mettez en place une surveillance des opérations cloud et des alertes en cas de problème.

```pascal
type
  TCloudMonitor = class
  private
    FAlertThresholds: TDictionary<string, Double>;
    FMetrics: TDictionary<string, TList<Double>>;
    FEmailAlerts: TStringList;
    FLastAlertTimes: TDictionary<string, TDateTime>;

    procedure SendAlertEmail(const Subject, Body: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetAlertThreshold(const MetricName: string; Threshold: Double);
    procedure RecordMetric(const MetricName: string; Value: Double);
    procedure CheckAlerts;
    property EmailAlerts: TStringList read FEmailAlerts;
  end;

constructor TCloudMonitor.Create;
begin
  inherited;
  FAlertThresholds := TDictionary<string, Double>.Create;
  FMetrics := TDictionary<string, TList<Double>>.Create;
  FEmailAlerts := TStringList.Create;
  FLastAlertTimes := TDictionary<string, TDateTime>.Create;
end;

destructor TCloudMonitor.Destroy;
begin
  FAlertThresholds.Free;

  // Libérer les listes de métriques
  for var MetricList in FMetrics.Values do
    MetricList.Free;

  FMetrics.Free;
  FEmailAlerts.Free;
  FLastAlertTimes.Free;
  inherited;
end;

procedure TCloudMonitor.SetAlertThreshold(const MetricName: string; Threshold: Double);
begin
  FAlertThresholds.AddOrSetValue(MetricName, Threshold);

  // Créer la liste de métriques si elle n'existe pas
  if not FMetrics.ContainsKey(MetricName) then
    FMetrics.Add(MetricName, TList<Double>.Create);
end;

procedure TCloudMonitor.RecordMetric(const MetricName: string; Value: Double);
var
  MetricList: TList<Double>;
begin
  if not FMetrics.TryGetValue(MetricName, MetricList) then
  begin
    MetricList := TList<Double>.Create;
    FMetrics.Add(MetricName, MetricList);
  end;

  // Ajouter la valeur
  MetricList.Add(Value);

  // Limiter la taille de l'historique (garder les 100 dernières valeurs)
  if MetricList.Count > 100 then
    MetricList.Delete(0);

  // Vérifier si on dépasse le seuil
  if FAlertThresholds.ContainsKey(MetricName) and
     (Value > FAlertThresholds[MetricName]) then
  begin
    // Vérifier si on a déjà envoyé une alerte récemment
    var CanAlert := True;
    var LastAlertTime: TDateTime;

    if FLastAlertTimes.TryGetValue(MetricName, LastAlertTime) then
      CanAlert := (Now - LastAlertTime) > (1 / 24); // Au moins 1 heure entre les alertes

    if CanAlert then
    begin
      SendAlertEmail(
        'Alerte: ' + MetricName + ' dépasse le seuil',
        'La métrique ' + MetricName + ' a atteint la valeur ' +
        FloatToStr(Value) + ', dépassant le seuil de ' +
        FloatToStr(FAlertThresholds[MetricName])
      );

      FLastAlertTimes.AddOrSetValue(MetricName, Now);
    end;
  end;
end;

procedure TCloudMonitor.CheckAlerts;
var
  MetricName: string;
  MetricList: TList<Double>;
  Average: Double;
  Sum: Double;
begin
  for MetricName in FMetrics.Keys do
  begin
    MetricList := FMetrics[MetricName];

    if MetricList.Count > 0 then
    begin
      // Calculer la moyenne
      Sum := 0;
      for var Value in MetricList do
        Sum := Sum + Value;

      Average := Sum / MetricList.Count;

      // Vérifier si la moyenne dépasse le seuil
      if FAlertThresholds.ContainsKey(MetricName) and
         (Average > FAlertThresholds[MetricName]) then
      begin
        // Vérifier si on a déjà envoyé une alerte récemment
        var CanAlert := True;
        var LastAlertTime: TDateTime;

        if FLastAlertTimes.TryGetValue(MetricName, LastAlertTime) then
          CanAlert := (Now - LastAlertTime) > (1 / 24); // Au moins 1 heure entre les alertes

        if CanAlert then
        begin
          SendAlertEmail(
            'Alerte: Moyenne de ' + MetricName + ' dépasse le seuil',
            'La moyenne de la métrique ' + MetricName + ' est de ' +
            FloatToStr(Average) + ', dépassant le seuil de ' +
            FloatToStr(FAlertThresholds[MetricName])
          );

          FLastAlertTimes.AddOrSetValue(MetricName, Now);
        end;
      end;
    end;
  end;
end;

procedure TCloudMonitor.SendAlertEmail(const Subject, Body: string);
begin
  // En production, envoyez un email ou une notification réelle
  // Ici, nous nous contentons de l'ajouter à la liste
  FEmailAlerts.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ' + Subject);

  // Journaliser l'alerte
  OutputDebugString(PChar('ALERTE: ' + Subject + ' - ' + Body));
end;
```

## Exemple complet d'application : Gestionnaire de documents multi-cloud

Pour illustrer les concepts présentés dans ce chapitre, voici un exemple d'application permettant de gérer des documents en utilisant différents services cloud.

### Structure du projet

1. **Interfaces communes** pour l'abstraction des services cloud
2. **Implémentations spécifiques** pour AWS, Azure et Google Cloud
3. **Interface utilisateur** pour interagir avec les services

### Interface principale

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Grids, Vcl.Buttons, System.ImageList, Vcl.ImgList, Vcl.Menus,
  CloudStorageInterface, CloudSettings, CloudMonitor, QuotaManager;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    cboCloudProvider: TComboBox;
    btnConnect: TButton;
    btnSettings: TButton;
    pnlClient: TPanel;
    lstFiles: TListView;
    pnlBottom: TPanel;
    btnUpload: TButton;
    btnDownload: TButton;
    btnDelete: TButton;
    btnRefresh: TButton;
    StatusBar1: TStatusBar;
    btnGenerateURL: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    mnuDownload: TMenuItem;
    mnuDelete: TMenuItem;
    mnuGenerateURL: TMenuItem;
    N1: TMenuItem;
    mnuRefresh: TMenuItem;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnGenerateURLClick(Sender: TObject);
    procedure lstFilesDblClick(Sender: TObject);
    procedure lstFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure cboCloudProviderChange(Sender: TObject);
  private
    FCloudService: ICloudStorageService;
    FCloudSettings: TCloudSettings;
    FCloudMonitor: TCloudMonitor;
    FQuotaManager: TQuotaManager;

    procedure UpdateButtonStates;
    procedure LoadFileList;
    procedure ConnectToCloud;
    procedure RecordCloudOperation(const Operation: string;
                                  Success: Boolean; const ErrorMsg: string = '');
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  CloudStorageFactory, SettingsForm, GenerateURLForm;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCloudSettings := TCloudSettings.Create;
  FCloudMonitor := TCloudMonitor.Create;
  FQuotaManager := TQuotaManager.Create;

  // Configurer les quotas
  FQuotaManager.SetLimit('uploads', 100);  // 100 uploads par jour
  FQuotaManager.SetLimit('downloads', 200); // 200 downloads par jour

  // Configurer les seuils d'alerte
  FCloudMonitor.SetAlertThreshold('upload_time', 5.0);  // Plus de 5 secondes = alerte
  FCloudMonitor.SetAlertThreshold('error_rate', 0.1);   // Plus de 10% d'erreurs = alerte

  // Remplir la liste des fournisseurs
  cboCloudProvider.Items.Clear;
  cboCloudProvider.Items.Add('Amazon S3');
  cboCloudProvider.Items.Add('Azure Blob Storage');
  cboCloudProvider.Items.Add('Google Cloud Storage');

  cboCloudProvider.ItemIndex := 0;

  // Charger les paramètres
  FCloudSettings.LoadSettings;

  // Mettre à jour l'état des boutons
  UpdateButtonStates;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FCloudSettings.SaveSettings;
  FCloudSettings.Free;
  FCloudMonitor.Free;
  FQuotaManager.Free;
end;

procedure TfrmMain.cboCloudProviderChange(Sender: TObject);
begin
  // Déconnecter le service actuel
  FCloudService := nil;

  // Mettre à jour l'état des boutons
  UpdateButtonStates;

  // Effacer la liste des fichiers
  lstFiles.Items.Clear;
end;

procedure TfrmMain.UpdateButtonStates;
var
  IsConnected: Boolean;
  IsItemSelected: Boolean;
begin
  IsConnected := Assigned(FCloudService);
  IsItemSelected := (lstFiles.Selected <> nil);

  btnConnect.Caption := IfThen(IsConnected, 'Déconnecter', 'Connecter');

  btnUpload.Enabled := IsConnected;
  btnRefresh.Enabled := IsConnected;
  btnDownload.Enabled := IsConnected and IsItemSelected;
  btnDelete.Enabled := IsConnected and IsItemSelected;
  btnGenerateURL.Enabled := IsConnected and IsItemSelected;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  if Assigned(FCloudService) then
  begin
    // Déconnecter
    FCloudService := nil;
    StatusBar1.SimpleText := 'Déconnecté';
    lstFiles.Items.Clear;
  end
  else
  begin
    // Connecter
    ConnectToCloud;
  end;

  UpdateButtonStates;
end;

procedure TfrmMain.ConnectToCloud;
begin
  Screen.Cursor := crHourGlass;
  try
    case cboCloudProvider.ItemIndex of
      0: // AWS S3
        begin
          FCloudService := TCloudStorageFactory.CreateAWSStorage(
            FCloudSettings.AWS.AccessKey,
            FCloudSettings.AWS.SecretKey,
            FCloudSettings.AWS.BucketName,
            FCloudSettings.AWS.Region
          );
          StatusBar1.SimpleText := 'Connecté à Amazon S3';
        end;

      1: // Azure Blob Storage
        begin
          FCloudService := TCloudStorageFactory.CreateAzureStorage(
            FCloudSettings.Azure.AccountName,
            FCloudSettings.Azure.AccountKey,
            FCloudSettings.Azure.ContainerName
          );
          StatusBar1.SimpleText := 'Connecté à Azure Blob Storage';
        end;

      2: // Google Cloud Storage
        begin
          FCloudService := TCloudStorageFactory.CreateGoogleStorage(
            FCloudSettings.Google.BucketName,
            FCloudSettings.Google.ApiKey,
            FCloudSettings.Google.ServiceAccountJson
          );
          StatusBar1.SimpleText := 'Connecté à Google Cloud Storage';
        end;
    end;

    // Charger la liste des fichiers
    if Assigned(FCloudService) then
      LoadFileList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
  with TfrmSettings.Create(Self) do
  try
    // Transférer les paramètres actuels
    CloudSettings := FCloudSettings;

    if ShowModal = mrOk then
    begin
      // Paramètres mis à jour
      FCloudSettings := CloudSettings;
      FCloudSettings.SaveSettings;

      // Si connecté, se reconnecter avec les nouveaux paramètres
      if Assigned(FCloudService) then
      begin
        FCloudService := nil;
        ConnectToCloud;
        UpdateButtonStates;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.LoadFileList;
var
  Files: TStringList;
  I: Integer;
  Item: TListItem;
  StartTime: TDateTime;
  ElapsedSeconds: Double;
begin
  if not Assigned(FCloudService) then
    Exit;

  Screen.Cursor := crHourGlass;
  ProgressBar1.Visible := True;
  StatusBar1.SimpleText := 'Chargement de la liste des fichiers...';
  lstFiles.Items.Clear;

  StartTime := Now;

  try
    Files := FCloudService.ListFiles;
    try
      ProgressBar1.Max := Files.Count;

      for I := 0 to Files.Count - 1 do
      begin
        Item := lstFiles.Items.Add;
        Item.Caption := Files[I];

        // Déterminer l'icône en fonction de l'extension
        var Ext := ExtractFileExt(Files[I]).ToLower;
        if (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.gif') then
          Item.ImageIndex := 0  // Image
        else if (Ext = '.pdf') then
          Item.ImageIndex := 1  // PDF
        else if (Ext = '.doc') or (Ext = '.docx') then
          Item.ImageIndex := 2  // Document
        else if (Ext = '.xls') or (Ext = '.xlsx') then
          Item.ImageIndex := 3  // Feuille de calcul
        else
          Item.ImageIndex := 4;  // Fichier générique

        ProgressBar1.Position := I + 1;
        Application.ProcessMessages;
      end;

      ElapsedSeconds := (Now - StartTime) * 24 * 60 * 60;
      FCloudMonitor.RecordMetric('list_time', ElapsedSeconds);

      StatusBar1.SimpleText := Format('%d fichiers trouvés', [Files.Count]);
      RecordCloudOperation('ListFiles', True);
    finally
      Files.Free;
    end;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Erreur: ' + E.Message;
      RecordCloudOperation('ListFiles', False, E.Message);
    end;
  end;

  ProgressBar1.Visible := False;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.btnUploadClick(Sender: TObject);
var
  FileName, RemoteFileName: string;
  FileStream: TFileStream;
  ContentType: string;
  StartTime: TDateTime;
  ElapsedSeconds: Double;
  Success: Boolean;
begin
  if not Assigned(FCloudService) then
    Exit;

  // Vérifier le quota
  if not FQuotaManager.CanUseResource('uploads') then
  begin
    ShowMessage('Quota journalier d''uploads atteint. Veuillez réessayer demain.');
    Exit;
  end;

  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    RemoteFileName := ExtractFileName(FileName);

    // Demander confirmation si le fichier existe déjà
    for var I := 0 to lstFiles.Items.Count - 1 do
    begin
      if SameText(lstFiles.Items[I].Caption, RemoteFileName) then
      begin
        if MessageDlg('Un fichier avec ce nom existe déjà. Voulez-vous le remplacer?',
                     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          Exit;

        Break;
      end;
    end;

    Screen.Cursor := crHourGlass;
    ProgressBar1.Visible := True;
    StatusBar1.SimpleText := 'Upload en cours...';

    StartTime := Now;

    try
      // Déterminer le type de contenu
      ContentType := 'application/octet-stream'; // Par défaut
      var Ext := ExtractFileExt(FileName).ToLower;
      if (Ext = '.jpg') or (Ext = '.jpeg') then
        ContentType := 'image/jpeg'
      else if (Ext = '.png') then
        ContentType := 'image/png'
      else if (Ext = '.gif') then
        ContentType := 'image/gif'
      else if (Ext = '.pdf') then
        ContentType := 'application/pdf'
      else if (Ext = '.doc') or (Ext = '.docx') then
        ContentType := 'application/msword'
      else if (Ext = '.xls') or (Ext = '.xlsx') then
        ContentType := 'application/vnd.ms-excel';

      // Ouvrir le fichier
      FileStream := TFileStream.Create(FileName, fmOpenRead);
      try
        // Configurer la barre de progression
        ProgressBar1.Max := 100;
        ProgressBar1.Position := 0;

        // Simuler une progression (dans une vraie application, on utiliserait
        // un événement de progression)
        var Timer := TTimer.Create(nil);
        try
          Timer.Interval := 100;
          Timer.OnTimer := procedure(Sender: TObject)
          begin
            if ProgressBar1.Position < 100 then
              ProgressBar1.Position := ProgressBar1.Position + 1;
          end;
          Timer.Enabled := True;

          // Upload du fichier
          Success := FCloudService.UploadFile(RemoteFileName, ContentType, FileStream);
        finally
          Timer.Free;
        end;

        ElapsedSeconds := (Now - StartTime) * 24 * 60 * 60;
        FCloudMonitor.RecordMetric('upload_time', ElapsedSeconds);

        if Success then
        begin
          StatusBar1.SimpleText := 'Fichier uploadé avec succès en ' +
                                   FormatFloat('0.00', ElapsedSeconds) + ' secondes';

          // Enregistrer l'utilisation du quota
          FQuotaManager.RecordUsage('uploads');

          // Recharger la liste des fichiers
          LoadFileList;

          RecordCloudOperation('UploadFile', True);
        end
        else
        begin
          StatusBar1.SimpleText := 'Échec de l''upload du fichier';
          RecordCloudOperation('UploadFile', False, 'Échec non spécifié');
        end;
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
      begin
        StatusBar1.SimpleText := 'Erreur: ' + E.Message;
        RecordCloudOperation('UploadFile', False, E.Message);
      end;
    end;

    ProgressBar1.Visible := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.RecordCloudOperation(const Operation: string;
                                       Success: Boolean; const ErrorMsg: string);
var
  ProviderName: string;
begin
  case cboCloudProvider.ItemIndex of
    0: ProviderName := 'AWS';
    1: ProviderName := 'Azure';
    2: ProviderName := 'Google';
    else ProviderName := 'Unknown';
  end;

  // Journaliser l'opération
  var LogFile := TStreamWriter.Create(ExtractFilePath(Application.ExeName) + 'cloud_log.txt', True);
  try
    LogFile.WriteLine(
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' | ' +
      ProviderName + ' | ' +
      Operation + ' | ' +
      BoolToStr(Success, True) + IfThen(not Success, ' | ' + ErrorMsg, '')
    );
  finally
    LogFile.Free;
  end;

  // Enregistrer pour les métriques
  if not Success then
    FCloudMonitor.RecordMetric('error_rate', 1.0)
  else
    FCloudMonitor.RecordMetric('error_rate', 0.0);
end;

procedure TfrmMain.btnDownloadClick(Sender: TObject);
var
  FileName: string;
  FileStream: TFileStream;
  StartTime: TDateTime;
  ElapsedSeconds: Double;
  Success: Boolean;
begin
  if not Assigned(FCloudService) or (lstFiles.Selected = nil) then
    Exit;

  // Vérifier le quota
  if not FQuotaManager.CanUseResource('downloads') then
  begin
    ShowMessage('Quota journalier de downloads atteint. Veuillez réessayer demain.');
    Exit;
  end;

  FileName := lstFiles.Selected.Caption;

  SaveDialog1.FileName := FileName;
  if SaveDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    ProgressBar1.Visible := True;
    StatusBar1.SimpleText := 'Téléchargement en cours...';

    StartTime := Now;

    try
      // Créer le fichier de destination
      FileStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
      try
        // Configurer la barre de progression
        ProgressBar1.Max := 100;
        ProgressBar1.Position := 0;

        // Simuler une progression
        var Timer := TTimer.Create(nil);
        try
          Timer.Interval := 100;
          Timer.OnTimer := procedure(Sender: TObject)
          begin
            if ProgressBar1.Position < 100 then
              ProgressBar1.Position := ProgressBar1.Position + 1;
          end;
          Timer.Enabled := True;

          // Télécharger le fichier
          Success := FCloudService.DownloadFile(FileName, FileStream);
        finally
          Timer.Free;
        end;

        ElapsedSeconds := (Now - StartTime) * 24 * 60 * 60;
        FCloudMonitor.RecordMetric('download_time', ElapsedSeconds);

        if Success then
        begin
          StatusBar1.SimpleText := 'Fichier téléchargé avec succès en ' +
                                   FormatFloat('0.00', ElapsedSeconds) + ' secondes';

          // Enregistrer l'utilisation du quota
          FQuotaManager.RecordUsage('downloads');

          RecordCloudOperation('DownloadFile', True);
        end
        else
        begin
          StatusBar1.SimpleText := 'Échec du téléchargement du fichier';
          RecordCloudOperation('DownloadFile', False, 'Échec non spécifié');
        end;
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
      begin
        StatusBar1.SimpleText := 'Erreur: ' + E.Message;
        RecordCloudOperation('DownloadFile', False, E.Message);
      end;
    end;

    ProgressBar1.Visible := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var
  FileName: string;
  Success: Boolean;
begin
  if not Assigned(FCloudService) or (lstFiles.Selected = nil) then
    Exit;

  FileName := lstFiles.Selected.Caption;

  if MessageDlg('Êtes-vous sûr de vouloir supprimer le fichier "' + FileName + '" ?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Screen.Cursor := crHourGlass;
  StatusBar1.SimpleText := 'Suppression en cours...';

  try
    // Supprimer le fichier
    Success := FCloudService.DeleteFile(FileName);

    if Success then
    begin
      StatusBar1.SimpleText := 'Fichier supprimé avec succès';

      // Recharger la liste des fichiers
      LoadFileList;

      RecordCloudOperation('DeleteFile', True);
    end
    else
    begin
      StatusBar1.SimpleText := 'Échec de la suppression du fichier';
      RecordCloudOperation('DeleteFile', False, 'Échec non spécifié');
    end;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Erreur: ' + E.Message;
      RecordCloudOperation('DeleteFile', False, E.Message);
    end;
  end;

  Screen.Cursor := crDefault;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  LoadFileList;
end;

procedure TfrmMain.btnGenerateURLClick(Sender: TObject);
begin
  if not Assigned(FCloudService) or (lstFiles.Selected = nil) then
    Exit;

  with TfrmGenerateURL.Create(Self) do
  try
    FileName := lstFiles.Selected.Caption;

    case cboCloudProvider.ItemIndex of
      0: CloudProvider := cpAWS;
      1: CloudProvider := cpAzure;
      2: CloudProvider := cpGoogle;
    end;

    CloudSettings := FCloudSettings;

    if ShowModal = mrOk then
    begin
      // URL temporaire générée
      Clipboard.AsText := GeneratedURL;
      StatusBar1.SimpleText := 'URL temporaire copiée dans le presse-papiers';

      RecordCloudOperation('GenerateURL', True);
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.lstFilesDblClick(Sender: TObject);
begin
  if btnDownload.Enabled then
    btnDownloadClick(Sender);
end;

procedure TfrmMain.lstFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateButtonStates;
end;

end.
```

## Modèles de services cloud courants

Outre le stockage de fichiers, les fournisseurs cloud offrent de nombreux autres services que vous pouvez intégrer dans vos applications Delphi. Voici quelques modèles courants :

### 1. Base de données en tant que service (DBaaS)

```pascal
procedure TForm1.ConnectToCloudDatabase;
begin
  case cboCloudProvider.ItemIndex of
    0: // AWS DynamoDB
      ConnectToDynamoDB(edtAWSRegion.Text, edtAWSAccessKey.Text, edtAWSSecretKey.Text);

    1: // Azure Cosmos DB
      ConnectToCosmosDB(edtCosmosEndpoint.Text, edtCosmosKey.Text);

    2: // Google Cloud Firestore
      ConnectToFirestore(edtFirestoreProjectID.Text, edtServiceAccountPath.Text);
  end;
end;

procedure TForm1.ConnectToDynamoDB(const Region, AccessKey, SecretKey: string);
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Configurer les en-têtes d'authentification
    // ...

    // Tester la connexion
    HTTP.Get('https://dynamodb.' + Region + '.amazonaws.com');

    ShowMessage('Connexion à DynamoDB réussie!');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  finally
    HTTP.Free;
    SSL.Free;
  end;
end;
```

### 2. Authentification et autorisation

```pascal
procedure TForm1.ConnectWithAzureAD;
var
  URL: string;
  ClientID, TenantID, RedirectURI, Scope: string;
begin
  // Paramètres OAuth 2.0
  ClientID := edtClientID.Text;
  TenantID := edtTenantID.Text;
  RedirectURI := 'http://localhost:8080';
  Scope := 'https://graph.microsoft.com/.default';

  // Construire l'URL d'autorisation
  URL := Format(
    'https://login.microsoftonline.com/%s/oauth2/v2.0/authorize?' +
    'client_id=%s&response_type=code&redirect_uri=%s&scope=%s&response_mode=query',
    [TenantID, ClientID, TNetEncoding.URL.Encode(RedirectURI), TNetEncoding.URL.Encode(Scope)]
  );

  // Ouvrir le navigateur pour l'authentification
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;
```

### 3. Serverless (Functions as a Service)

```pascal
procedure TForm1.InvokeAWSLambda;
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  RequestBody, ResponseText: string;
  RequestStream, ResponseStream: TStringStream;
  JsonRequest, JsonResponse: TJSONObject;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // Configuration SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSL;

    // Configurer les en-têtes
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.CustomHeaders.Clear;

    // Préparer le corps de la requête
    JsonRequest := TJSONObject.Create;
    try
      JsonRequest.AddPair('name', edtName.Text);
      JsonRequest.AddPair('age', TJSONNumber.Create(StrToIntDef(edtAge.Text, 0)));

      RequestBody := JsonRequest.ToString;
      RequestStream := TStringStream.Create(RequestBody);
    finally
      JsonRequest.Free;
    end;

    // Créer le stream de réponse
    ResponseStream := TStringStream.Create;

    try
      // Envoyer la requête
      HTTP.Post(
        'https://lambda.us-east-1.amazonaws.com/2015-03-31/functions/MyFunction/invocations',
        RequestStream,
        ResponseStream
      );

      // Traiter la réponse
      ResponseText := ResponseStream.DataString;
      JsonResponse := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

      try
        Memo1.Text := JsonResponse.Format(2);
      finally
        JsonResponse.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HTTP.Free;
    SSL.Free;
  end;
end;
```

## Défis et limites de l'intégration cloud

### 1. Dépendance aux services externes

Lorsque vous intégrez des services cloud, votre application devient dépendante de services externes que vous ne contrôlez pas. Voici comment gérer cette dépendance :

```pascal
procedure TForm1.ExecuteWithFallback(OnlineOperation, OfflineOperation: TProc);
begin
  try
    // Vérifier d'abord la connectivité réseau
    if IsInternetConnected then
    begin
      try
        // Essayer l'opération en ligne
        OnlineOperation();
      except
        // En cas d'échec, basculer vers l'opération hors ligne
        ShowMessage('Service cloud indisponible. Basculement en mode hors ligne.');
        OfflineOperation();
      end;
    end
    else
    begin
      // Pas de connexion internet, utiliser directement le mode hors ligne
      ShowMessage('Aucune connexion Internet détectée. Utilisation du mode hors ligne.');
      OfflineOperation();
    end;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;

function TForm1.IsInternetConnected: Boolean;
var
  HTTP: TIdHTTP;
  ConnectionTest: TIdTCPClient;
begin
  Result := False;

  // Créer un client TCP pour tester la connectivité
  ConnectionTest := TIdTCPClient.Create(nil);
  try
    try
      // Essayer de se connecter à un serveur fiable
      ConnectionTest.Host := 'www.google.com';
      ConnectionTest.Port := 443;
      ConnectionTest.ConnectTimeout := 3000; // 3 secondes
      ConnectionTest.Connect;
      Result := ConnectionTest.Connected;
    except
      Result := False;
    end;
  finally
    ConnectionTest.Free;
  end;
end;
```

### 2. Coûts et optimisation

Les services cloud sont généralement facturés en fonction de l'utilisation. Il est donc important d'optimiser votre code pour minimiser les coûts.

```pascal
type
  TCostOptimizer = class
  private
    FUsageStats: TDictionary<string, Integer>;
    FLastReset: TDateTime;
    FCostPerUnit: TDictionary<string, Double>;
    const
      DAY_SECONDS = 24 * 60 * 60;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordUsage(const ServiceName: string; Units: Integer = 1);
    procedure ResetDailyCounters;
    function GetEstimatedCost: Double;
    function GetServiceCost(const ServiceName: string): Double;
    procedure SetCostPerUnit(const ServiceName: string; Cost: Double);
  end;

constructor TCostOptimizer.Create;
begin
  inherited;
  FUsageStats := TDictionary<string, Integer>.Create;
  FCostPerUnit := TDictionary<string, Double>.Create;
  FLastReset := Date;

  // Coûts par défaut (exemples)
  FCostPerUnit.Add('s3_upload', 0.005);      // $0.005 par upload
  FCostPerUnit.Add('s3_download', 0.004);    // $0.004 par download
  FCostPerUnit.Add('lambda_invocation', 0.0000002); // $0.0000002 par invocation
end;

destructor TCostOptimizer.Destroy;
begin
  FUsageStats.Free;
  FCostPerUnit.Free;
  inherited;
end;

procedure TCostOptimizer.RecordUsage(const ServiceName: string; Units: Integer);
var
  CurrentUsage: Integer;
begin
  // Vérifier si on doit réinitialiser les compteurs (nouveau jour)
  if Trunc(Now) > Trunc(FLastReset) then
    ResetDailyCounters;

  // Mettre à jour les statistiques d'utilisation
  if FUsageStats.TryGetValue(ServiceName, CurrentUsage) then
    FUsageStats[ServiceName] := CurrentUsage + Units
  else
    FUsageStats.Add(ServiceName, Units);
end;

procedure TCostOptimizer.ResetDailyCounters;
begin
  FUsageStats.Clear;
  FLastReset := Now;
end;

function TCostOptimizer.GetEstimatedCost: Double;
var
  ServiceName: string;
  Usage: Integer;
  Cost: Double;
begin
  Result := 0;

  for ServiceName in FUsageStats.Keys do
  begin
    if FCostPerUnit.TryGetValue(ServiceName, Cost) and
       FUsageStats.TryGetValue(ServiceName, Usage) then
    begin
      Result := Result + (Cost * Usage);
    end;
  end;
end;

function TCostOptimizer.GetServiceCost(const ServiceName: string): Double;
var
  Usage: Integer;
  Cost: Double;
begin
  Result := 0;

  if FCostPerUnit.TryGetValue(ServiceName, Cost) and
     FUsageStats.TryGetValue(ServiceName, Usage) then
  begin
    Result := Cost * Usage;
  end;
end;

procedure TCostOptimizer.SetCostPerUnit(const ServiceName: string; Cost: Double);
begin
  FCostPerUnit.AddOrSetValue(ServiceName, Cost);
end;
```

## Résumé

Dans ce chapitre, nous avons exploré l'intégration des services cloud dans les applications Delphi. Voici les points clés à retenir :

1. **Abstraction des services** : Créez des interfaces génériques qui masquent les détails spécifiques des fournisseurs
2. **Sécurité** : Gérez correctement les identifiants et chiffrez les données sensibles
3. **Optimisation** : Minimisez les appels aux services cloud et compressez les données quand c'est possible
4. **Robustesse** : Implémentez des mécanismes de retry et de fallback pour gérer les pannes
5. **Surveillance** : Suivez l'utilisation des services et configurez des alertes

L'intégration avec les services cloud offre de nombreux avantages aux applications Delphi, en particulier en termes de scalabilité et d'accès à des fonctionnalités avancées. Avec les techniques présentées dans ce chapitre, vous pouvez créer des applications robustes qui tirent pleinement parti du cloud tout en restant fiables et performantes.

## Ressources supplémentaires

- Documentation AWS : [https://docs.aws.amazon.com](https://docs.aws.amazon.com)
- Documentation Azure : [https://docs.microsoft.com/azure](https://docs.microsoft.com/azure)
- Documentation Google Cloud : [https://cloud.google.com/docs](https://cloud.google.com/docs)
- Bibliothèques et composants tiers pour Delphi :
  - REST Debugger (inclus dans Delphi)
  - AWS SDK pour Delphi (plusieurs projets open source)
  - Composants Azure pour Delphi
  - Google API pour Delphi

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria.*
