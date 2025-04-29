# 10.4 Services SOAP et WebServices

## Introduction

Les WebServices constituent un moyen standardisé permettant à des applications de communiquer entre elles via Internet, indépendamment des langages de programmation ou des systèmes d'exploitation utilisés. Dans ce chapitre, nous allons explorer les services SOAP (Simple Object Access Protocol) et les WebServices, comprendre leur fonctionnement et apprendre à les implémenter dans nos applications.

## Comprendre les WebServices

### Qu'est-ce qu'un WebService ?

Un WebService est une technologie qui permet à des applications de communiquer via un réseau en utilisant des formats standardisés. L'idée fondamentale est de permettre à des systèmes hétérogènes (développés dans différents langages et sur différentes plateformes) d'interagir facilement.

### Principaux types de WebServices

Il existe plusieurs approches pour créer des WebServices, mais les deux principales sont :

1. **Services SOAP** (Simple Object Access Protocol)
   - Basés sur XML
   - Utilisent un format de message très structuré
   - Généralement plus formels et rigides
   - Souvent utilisés dans les environnements d'entreprise

2. **Services REST** (Representational State Transfer)
   - Plus légers et flexibles
   - Généralement basés sur le format JSON
   - Utilisent directement les méthodes HTTP (GET, POST, PUT, DELETE)
   - Plus populaires pour les applications web et mobiles modernes

Dans ce chapitre, nous nous concentrerons principalement sur les services SOAP, bien que nous mentionnerons également les services REST pour compléter votre compréhension.

## Services SOAP en détail

### Principes de base de SOAP

SOAP est un protocole basé sur XML pour l'échange d'informations structurées. Voici ses caractéristiques principales :

- **Indépendance** : Fonctionne sur n'importe quel système d'exploitation avec n'importe quel langage de programmation
- **Standard** : Suit des spécifications W3C précises
- **Extensibilité** : Peut être étendu avec des fonctionnalités supplémentaires
- **Transport flexible** : Généralement transmis via HTTP, mais peut utiliser d'autres protocoles

### Structure d'un message SOAP

Un message SOAP est un document XML structuré de la façon suivante :

```xml
<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope">
  <soap:Header>
    <!-- Informations d'en-tête (optionnelles) -->
  </soap:Header>
  <soap:Body>
    <!-- Corps du message (données) -->
    <m:GetPrice xmlns:m="http://www.example.org/stock">
      <m:StockName>IBM</m:StockName>
    </m:GetPrice>
  </soap:Body>
</soap:Envelope>
```

Les principales parties sont :
- **Envelope** (enveloppe) : L'élément racine qui identifie le document XML comme un message SOAP
- **Header** (en-tête) : Contient des informations supplémentaires comme l'authentification ou la gestion des transactions (optionnel)
- **Body** (corps) : Contient les données réelles à échanger
- **Fault** (erreur) : Un élément spécial du corps qui fournit des informations sur les erreurs (si nécessaire)

### WSDL (Web Services Description Language)

Le WSDL est un document XML qui décrit un WebService SOAP. Il spécifie :

- Les opérations (méthodes) disponibles
- Les formats des messages d'entrée/sortie
- Les types de données utilisés
- Les protocoles de transport et les adresses du service

Voici un exemple simplifié de WSDL :

```xml
<definitions name="StockQuote"
             targetNamespace="http://example.com/stockquote.wsdl"
             xmlns="http://schemas.xmlsoap.org/wsdl/">

  <message name="GetStockPriceInput">
    <part name="symbol" type="xsd:string"/>
  </message>

  <message name="GetStockPriceOutput">
    <part name="price" type="xsd:float"/>
  </message>

  <portType name="StockQuotePortType">
    <operation name="GetStockPrice">
      <input message="tns:GetStockPriceInput"/>
      <output message="tns:GetStockPriceOutput"/>
    </operation>
  </portType>

  <!-- Binding et service omis pour la simplicité -->

</definitions>
```

## Consommer un WebService SOAP

Voyons maintenant comment consommer (utiliser) un WebService SOAP existant dans vos applications.

### Création d'un client SOAP

La première étape consiste à créer un client SOAP à partir d'un WSDL. Voici comment procéder :

```pascal
procedure TForm1.CreerClientSOAP;
var
  HTTPRIO: THTTPRIO;
begin
  // Créer l'objet HTTPRIO qui va gérer la communication
  HTTPRIO := THTTPRIO.Create(nil);
  try
    // Configurer l'URL du service
    HTTPRIO.URL := 'http://www.example.com/stockquote';

    // Spécifier le WSDL (peut être un fichier local ou une URL)
    HTTPRIO.WSDLLocation := 'http://www.example.com/stockquote.wsdl';

    // Créer l'interface du service
    FStockService := HTTPRIO as IStockQuoteService;

    ShowMessage('Client SOAP créé avec succès');
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la création du client SOAP: ' + E.Message);
      HTTPRIO.Free;
    end;
  end;
end;
```

### Utilisation de l'assistant WebService

La plupart des environnements de développement offrent des assistants pour faciliter la création de clients SOAP. Voici comment l'utiliser :

1. Dans votre IDE, sélectionnez **Fichier > Nouveau > Autre**
2. Cherchez "Web Services" ou "SOAP Client" dans les options
3. Entrez l'URL du WSDL du service que vous souhaitez utiliser
4. L'assistant va automatiquement :
   - Télécharger et analyser le WSDL
   - Générer les interfaces nécessaires
   - Créer le code pour communiquer avec le service

### Appel d'une méthode du WebService

Une fois le client SOAP créé, vous pouvez appeler les méthodes du service comme si elles étaient des méthodes locales :

```pascal
procedure TForm1.BtnGetPriceClick(Sender: TObject);
var
  StockSymbol: string;
  Price: Double;
begin
  try
    // Récupérer le symbole de l'action
    StockSymbol := EditSymbol.Text;

    // Appeler la méthode du WebService
    Price := FStockService.GetStockPrice(StockSymbol);

    // Afficher le résultat
    LabelPrice.Caption := Format('Prix: %.2f €', [Price]);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l'appel du service: ' + E.Message);
  end;
end;
```

### Gestion des erreurs SOAP

Les erreurs dans SOAP sont retournées via l'élément `Fault`. Voici comment les gérer :

```pascal
procedure TForm1.AppelerServiceAvecGestionErreurs;
var
  Response: TStockPriceResponse;
begin
  try
    Response := FStockService.GetDetailedStockInfo(EditSymbol.Text);

    if Response.Success then
      LabelPrice.Caption := Format('Prix: %.2f €', [Response.Price])
    else
      ShowMessage('Erreur du service: ' + Response.ErrorMessage);
  except
    on E: ESOAPHTTPException do
    begin
      // Exception spécifique aux erreurs HTTP dans SOAP
      ShowMessage('Erreur HTTP: ' + E.Message + ' (Code: ' +
                 IntToStr(E.StatusCode) + ')');
    end;
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

## Créer un WebService SOAP

Maintenant, voyons comment créer votre propre WebService SOAP.

### Configuration de base d'un serveur SOAP

Voici comment créer un serveur SOAP simple :

```pascal
unit StockQuoteService;

interface

uses
  System.SysUtils, Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns;

type
  IStockQuoteService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-4321-8765-9ABCDEF01234}']
    function GetStockPrice(const Symbol: string): Double; stdcall;
    function GetCompanyInfo(const Symbol: string): string; stdcall;
  end;

  TStockQuoteService = class(TInvokableClass, IStockQuoteService)
  public
    function GetStockPrice(const Symbol: string): Double; stdcall;
    function GetCompanyInfo(const Symbol: string): string; stdcall;
  end;

implementation

function TStockQuoteService.GetStockPrice(const Symbol: string): Double;
begin
  // Dans une application réelle, vous récupéreriez le prix
  // depuis une base de données ou un autre service
  if Symbol = 'AAPL' then
    Result := 142.56
  else if Symbol = 'MSFT' then
    Result := 265.23
  else if Symbol = 'GOOG' then
    Result := 2350.72
  else
    Result := 0.0;
end;

function TStockQuoteService.GetCompanyInfo(const Symbol: string): string;
begin
  if Symbol = 'AAPL' then
    Result := 'Apple Inc. - Entreprise technologique basée à Cupertino, Californie.'
  else if Symbol = 'MSFT' then
    Result := 'Microsoft Corporation - Entreprise technologique basée à Redmond, Washington.'
  else if Symbol = 'GOOG' then
    Result := 'Alphabet Inc. (Google) - Entreprise technologique basée à Mountain View, Californie.'
  else
    Result := 'Information non disponible pour ' + Symbol;
end;

initialization
  // Enregistrer la classe de service
  InvRegistry.RegisterInvokableClass(TStockQuoteService);
  // Enregistrer l'interface
  InvRegistry.RegisterInterface(TypeInfo(IStockQuoteService));
end.
```

### Déploiement du WebService

Pour déployer le WebService, vous devez créer un serveur Web qui expose votre service. Voici un exemple simple d'application serveur :

```pascal
program StockQuoteServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Soap.WebBroker,
  Soap.WSDLPub,
  Soap.WSDLIntf,
  IdHTTPWebBrokerBridge,
  StockQuoteService in 'StockQuoteService.pas';

{$R *.res}

var
  Server: TIdHTTPWebBrokerBridge;
  Port: Integer;

begin
  try
    // Demander le port d'écoute
    Writeln('Sur quel port souhaitez-vous démarrer le serveur (8080 par défaut) ?');
    Port := StrToIntDef(Readln, 8080);

    // Créer et configurer le serveur
    Server := TIdHTTPWebBrokerBridge.Create(nil);
    try
      Server.DefaultPort := Port;

      // Démarrer le serveur
      Server.Active := True;

      Writeln('Serveur démarré sur le port ' + IntToStr(Port));
      Writeln('Appuyez sur Entrée pour arrêter le serveur');

      // Attendre que l'utilisateur appuie sur Entrée
      Readln;
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Erreur : ' + E.Message);
      Readln;
    end;
  end;
end.
```

### Types de données complexes

Les WebServices supportent également des types de données plus complexes :

```pascal
// Définir des types complexes
type
  TStockInfo = class(TRemotable)
  private
    FSymbol: string;
    FPrice: Double;
    FCompanyName: string;
    FLastUpdate: TDateTime;
  published
    property Symbol: string read FSymbol write FSymbol;
    property Price: Double read FPrice write FPrice;
    property CompanyName: string read FCompanyName write FCompanyName;
    property LastUpdate: TDateTime read FLastUpdate write FLastUpdate;
  end;

  // Ajouter à l'interface
  IStockQuoteService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-4321-8765-9ABCDEF01234}']
    function GetStockPrice(const Symbol: string): Double; stdcall;
    function GetCompanyInfo(const Symbol: string): string; stdcall;
    function GetDetailedStockInfo(const Symbol: string): TStockInfo; stdcall;
  end;

// Implémentation
function TStockQuoteService.GetDetailedStockInfo(const Symbol: string): TStockInfo;
begin
  Result := TStockInfo.Create;

  Result.Symbol := Symbol;
  Result.LastUpdate := Now;

  if Symbol = 'AAPL' then
  begin
    Result.Price := 142.56;
    Result.CompanyName := 'Apple Inc.';
  end
  else if Symbol = 'MSFT' then
  begin
    Result.Price := 265.23;
    Result.CompanyName := 'Microsoft Corporation';
  end
  else
  begin
    Result.Price := 0.0;
    Result.CompanyName := 'Inconnu';
  end;
end;

// Dans l'initialisation
initialization
  // Enregistrer le type complexe
  RemClassRegistry.RegisterXSClass(TStockInfo, 'http://tempuri.org', 'TStockInfo');
  // ... autres enregistrements
```

### Sécurisation du WebService

Pour sécuriser votre WebService, vous pouvez ajouter une authentification :

```pascal
// Dans la classe de service
TSecureStockService = class(TInvokableClass, IStockQuoteService)
private
  function VerifierAuthentification(const Username, Password: string): Boolean;
public
  function GetStockPrice(const Symbol, Username, Password: string): Double; stdcall;
  // ... autres méthodes
end;

function TSecureStockService.VerifierAuthentification(const Username, Password: string): Boolean;
begin
  // Dans une application réelle, vérifiez dans une base de données
  Result := (Username = 'admin') and (Password = 'secret123');
end;

function TSecureStockService.GetStockPrice(const Symbol, Username, Password: string): Double;
begin
  // Vérifier l'authentification avant de traiter la demande
  if not VerifierAuthentification(Username, Password) then
    raise Exception.Create('Authentification échouée');

  // Si l'authentification réussit, continuer normalement
  if Symbol = 'AAPL' then
    Result := 142.56
  else if Symbol = 'MSFT' then
    Result := 265.23
  else
    Result := 0.0;
end;
```

Une approche plus moderne consiste à utiliser SSL/TLS et des jetons d'authentification :

```pascal
// Configuration du serveur avec SSL
Server.IOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Server);
TIdServerIOHandlerSSLOpenSSL(Server.IOHandler).SSLOptions.CertFile := 'server.crt';
TIdServerIOHandlerSSLOpenSSL(Server.IOHandler).SSLOptions.KeyFile := 'server.key';
TIdServerIOHandlerSSLOpenSSL(Server.IOHandler).SSLOptions.Method := sslvTLSv1_2;
```

## Différences entre SOAP et REST

Pour vous aider à choisir le bon type de WebService pour votre application, voici les principales différences entre SOAP et REST :

| Caractéristique | SOAP | REST |
|----------------|------|------|
| **Format** | XML uniquement | Plusieurs formats (JSON, XML, etc.) |
| **Protocole** | Indépendant (souvent HTTP) | HTTP |
| **Structure** | Très formelle et stricte | Flexible et légère |
| **Contrat** | WSDL obligatoire | Optionnel (OpenAPI/Swagger) |
| **État** | Peut être avec ou sans état | Sans état |
| **Performances** | Plus lourd | Plus léger |
| **Utilisation** | Applications d'entreprise | Web, Mobile, API publiques |
| **Complexité** | Plus complexe | Plus simple |

### Quand utiliser SOAP ou REST ?

**Utilisez SOAP si :**
- Vous avez besoin d'une grande formalisation
- Vous travaillez dans un environnement d'entreprise plus traditionnel
- Vous avez besoin de fonctionnalités avancées comme les transactions distribuées
- La sécurité de niveau entreprise est primordiale

**Utilisez REST si :**
- Vous développez pour le web ou les applications mobiles
- Vous voulez une API légère et facile à consommer
- Les performances sont importantes
- Vous souhaitez une API plus facile à comprendre et à utiliser

## Exemple pratique : Client de service météo

Voici un exemple complet d'un client SOAP qui utilise un service météo public :

```pascal
unit WeatherClientMain;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, Soap.InvokeRegistry, Soap.Rio, Soap.SOAPHTTPClient;

type
  TWeatherInfo = class(TRemotable)
  private
    FTemperature: Double;
    FHumidity: Integer;
    FCondition: string;
    FCity: string;
  published
    property Temperature: Double read FTemperature write FTemperature;
    property Humidity: Integer read FHumidity write FHumidity;
    property Condition: string read FCondition write FCondition;
    property City: string read FCity write FCity;
  end;

  IWeatherService = interface(IInvokable)
    ['{4D41B746-2D8F-4433-A4E0-8E3F0B3F6A42}']
    function GetWeather(const City: string): TWeatherInfo; stdcall;
  end;

  TFormWeather = class(TForm)
    EditCity: TEdit;
    BtnGetWeather: TButton;
    LabelTemperature: TLabel;
    LabelHumidity: TLabel;
    LabelCondition: TLabel;
    PanelWeather: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnGetWeatherClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHTTPRIO: THTTPRIO;
    FWeatherService: IWeatherService;
  public
    procedure DisplayWeather(WeatherInfo: TWeatherInfo);
  end;

var
  FormWeather: TFormWeather;

implementation

{$R *.dfm}

procedure TFormWeather.FormCreate(Sender: TObject);
begin
  FHTTPRIO := THTTPRIO.Create(nil);
  FHTTPRIO.URL := 'http://example.com/WeatherService';

  // Dans une application réelle, utilisez une URL de service fonctionnelle
  // Ici nous utilisons une URL fictive pour l'exemple

  FWeatherService := FHTTPRIO as IWeatherService;

  PanelWeather.Visible := False;
end;

procedure TFormWeather.FormDestroy(Sender: TObject);
begin
  FHTTPRIO.Free;
end;

procedure TFormWeather.BtnGetWeatherClick(Sender: TObject);
var
  City: string;
  WeatherInfo: TWeatherInfo;
begin
  City := EditCity.Text;

  if City = '' then
  begin
    ShowMessage('Veuillez entrer un nom de ville');
    Exit;
  end;

  BtnGetWeather.Enabled := False;
  Screen.Cursor := crHourGlass;

  try
    try
      // Appel au WebService
      WeatherInfo := FWeatherService.GetWeather(City);
      DisplayWeather(WeatherInfo);
    except
      on E: Exception do
      begin
        ShowMessage('Erreur lors de l''obtention des données météo: ' + E.Message);
        PanelWeather.Visible := False;
      end;
    end;
  finally
    BtnGetWeather.Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWeather.DisplayWeather(WeatherInfo: TWeatherInfo);
begin
  // Afficher les informations météo
  LabelTemperature.Caption := Format('Température: %.1f °C', [WeatherInfo.Temperature]);
  LabelHumidity.Caption := Format('Humidité: %d %%', [WeatherInfo.Humidity]);
  LabelCondition.Caption := Format('Conditions: %s', [WeatherInfo.Condition]);

  PanelWeather.Caption := 'Météo pour ' + WeatherInfo.City;
  PanelWeather.Visible := True;
end;

end.
```

## Débogage des WebServices

Le débogage des WebServices peut être difficile en raison de leur nature distribuée. Voici quelques astuces :

### Capture et analyse des messages SOAP

Pour voir les messages SOAP échangés :

```pascal
procedure TForm1.ConfigurerDebogage;
begin
  // Activer la journalisation des messages SOAP
  FHTTPRIO.Converter.Options := FHTTPRIO.Converter.Options + [soSaveToFile];

  // Dossier où les fichiers seront enregistrés
  FHTTPRIO.Converter.TempDir := 'C:\Temp\SOAP';
end;
```

### Utilisation d'outils d'inspection réseau

Des outils comme Wireshark ou Fiddler peuvent être utilisés pour capturer et analyser les échanges HTTP entre le client et le serveur.

### Tests unitaires pour WebServices

Voici comment créer un test unitaire simple pour un WebService :

```pascal
procedure TTestStockService.TestGetStockPrice;
var
  Service: IStockQuoteService;
  HTTPRIO: THTTPRIO;
  Price: Double;
begin
  // Créer le client SOAP
  HTTPRIO := THTTPRIO.Create(nil);
  try
    HTTPRIO.URL := 'http://localhost:8080/soap/IStockQuoteService';
    Service := HTTPRIO as IStockQuoteService;

    // Appeler la méthode et vérifier le résultat
    Price := Service.GetStockPrice('AAPL');

    // Vérifier que le prix est positif
    CheckTrue(Price > 0, 'Le prix devrait être positif');

    // Vérifier que le prix pour un symbole invalide est 0
    Price := Service.GetStockPrice('INVALID');
    CheckEquals(0, Price, 'Le prix pour un symbole invalide devrait être 0');
  finally
    HTTPRIO.Free;
  end;
end;
```

## Bonnes pratiques

1. **Versionnez vos services** - Utilisez le versionnement pour éviter de casser les clients existants lors des mises à jour
2. **Documentez vos WebServices** - Une bonne documentation est essentielle pour les développeurs qui vont consommer votre service
3. **Évitez les dépendances inutiles** - Rendez vos WebServices aussi autonomes que possible
4. **Gérez correctement les erreurs** - Retournez des messages d'erreur clairs et utiles
5. **Testez rigoureusement** - Les WebServices sont difficiles à déboguer une fois déployés
6. **Surveillez les performances** - Les WebServices peuvent devenir des goulots d'étranglement
7. **Sécurisez vos données** - Utilisez HTTPS et des mécanismes d'authentification appropriés

## Tendances modernes

Les tendances actuelles dans le domaine des WebServices sont :

- Migration de SOAP vers REST et GraphQL
- Utilisation de formats plus légers (JSON au lieu de XML)
- Architecture de microservices
- API as a Service (AaaS)
- Sécurité basée sur les jetons (JWT, OAuth 2.0)

Cependant, SOAP reste pertinent dans de nombreux environnements d'entreprise et pour les systèmes existants.

## Conclusion

Les WebServices SOAP offrent un moyen puissant et standardisé pour créer des applications distribuées. Bien que plus complexes que les services REST, ils offrent des fonctionnalités avancées et une formalisation qui conviennent parfaitement à certains contextes, notamment dans les environnements d'entreprise.

En maîtrisant les concepts présentés dans ce chapitre, vous serez capable de consommer des WebServices existants et de créer vos propres services pour exposer vos fonctionnalités à d'autres applications.

## Exercices pratiques

1. **Client météo**
   - Créez un client qui se connecte à un WebService météo public
   - Affichez les prévisions pour plusieurs jours

2. **Service de conversion de devises**
   - Créez un WebService simple qui convertit entre différentes devises
   - Ajoutez une mise en cache des taux de change

3. **Application de gestion de tâches**
   - Créez un WebService pour gérer une liste de tâches
   - Implémentez les opérations CRUD (Create, Read, Update, Delete)
   - Ajoutez une authentification simple

4. **Comparaison SOAP vs REST**
   - Implémentez le même service en utilisant SOAP et REST
   - Comparez les performances et la facilité d'utilisation
