🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.4 Services SOAP et WebServices

## Introduction aux Services Web

### Qu'est-ce qu'un WebService ?

Un **WebService** (service web) est une application accessible via Internet qui permet à différents systèmes d'échanger des données et de communiquer entre eux, indépendamment de leurs langages de programmation ou de leurs plateformes.

**Analogie simple :**
Imaginez un restaurant avec un service de livraison. Vous passez commande par téléphone (requête), le restaurant prépare le plat (traitement), et vous livre le résultat (réponse). Le WebService fonctionne de la même manière, mais pour les applications informatiques.

**Avantages des WebServices :**
- **Interopérabilité** : Une application Delphi peut communiquer avec un serveur Java, .NET, PHP, etc.
- **Standardisation** : Protocoles reconnus mondialement
- **Réutilisabilité** : Un même service peut être utilisé par plusieurs applications
- **Intégration** : Permet de connecter des systèmes hétérogènes

### Qu'est-ce que SOAP ?

**SOAP** (Simple Object Access Protocol) est un protocole standardisé pour l'échange de messages structurés dans les services web. Il utilise le format **XML** pour encapsuler les données.

**Caractéristiques de SOAP :**
- Protocole basé sur XML
- Indépendant du langage et de la plateforme
- Supporte différents protocoles de transport (HTTP, SMTP, etc.)
- Intègre la sécurité et la gestion des transactions
- Standard W3C

**Cas d'utilisation typiques :**
- Systèmes bancaires et financiers
- Applications d'entreprise (ERP, CRM)
- Services gouvernementaux
- Intégration B2B (Business-to-Business)
- Systèmes nécessitant une sécurité stricte

## SOAP vs REST : Les différences

### Comparaison

| Critère | SOAP | REST |
|---------|------|------|
| **Type** | Protocole strict | Style architectural |
| **Format** | XML uniquement | JSON, XML, HTML, texte |
| **Verbosité** | Très verbeux | Léger et concis |
| **Complexité** | Plus complexe | Plus simple |
| **Sécurité** | WS-Security intégré | HTTPS, OAuth |
| **Transactions** | Support natif (ACID) | Non standardisé |
| **Cache** | Difficile | Facile (HTTP) |
| **Vitesse** | Plus lent | Plus rapide |
| **Standards** | WSDL, WS-* | Pas de standard strict |
| **Cas d'usage** | Entreprise, banque | Web, mobile, API publiques |

### Quand utiliser SOAP ?

**Choisissez SOAP pour :**
- Applications d'entreprise critiques
- Transactions financières
- Exigences de sécurité strictes (WS-Security)
- Besoin de transactions ACID
- Interopérabilité avec des systèmes legacy
- Contrats formels (WSDL)

**Choisissez REST pour :**
- Applications web et mobiles
- API publiques
- Besoins de performance
- Simplicité et rapidité de développement
- Consommation de ressources limitée

## Structure d'un message SOAP

### Anatomie d'un message SOAP

Un message SOAP est composé de plusieurs parties :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">

  <!-- En-tête (optionnel) -->
  <soap:Header>
    <Authentication>
      <Username>utilisateur</Username>
      <Password>motdepasse</Password>
    </Authentication>
  </soap:Header>

  <!-- Corps du message (obligatoire) -->
  <soap:Body>
    <GetUserInfo xmlns="http://example.com/webservice">
      <UserId>12345</UserId>
    </GetUserInfo>
  </soap:Body>

</soap:Envelope>
```

**Composants d'un message SOAP :**

1. **Envelope** (Enveloppe) : Élément racine qui encapsule tout le message
2. **Header** (En-tête) : Informations optionnelles (authentification, métadonnées)
3. **Body** (Corps) : Contient la requête ou la réponse
4. **Fault** (Erreur) : Gestion des erreurs (dans le Body)

### Message de réponse SOAP

```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetUserInfoResponse xmlns="http://example.com/webservice">
      <User>
        <Id>12345</Id>
        <Name>Jean Dupont</Name>
        <Email>jean.dupont@example.com</Email>
        <Active>true</Active>
      </User>
    </GetUserInfoResponse>
  </soap:Body>
</soap:Envelope>
```

### Message d'erreur SOAP (Fault)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <soap:Fault>
      <faultcode>soap:Client</faultcode>
      <faultstring>Utilisateur introuvable</faultstring>
      <detail>
        <ErrorCode>404</ErrorCode>
        <Message>Aucun utilisateur avec l'ID 12345</Message>
      </detail>
    </soap:Fault>
  </soap:Body>
</soap:Envelope>
```

## WSDL : Le contrat du service

### Qu'est-ce que WSDL ?

**WSDL** (Web Services Description Language) est un document XML qui décrit complètement un service SOAP :
- Les opérations disponibles
- Les paramètres d'entrée et de sortie
- Les types de données utilisés
- L'adresse du service

**Analogie :**
Le WSDL est comme un mode d'emploi détaillé d'un appareil. Il vous dit exactement comment l'utiliser, quels boutons presser, et ce que vous obtiendrez en retour.

### Exemple de WSDL simplifié

```xml
<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/webservice"
             targetNamespace="http://example.com/webservice">

  <!-- Types de données -->
  <types>
    <schema xmlns="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://example.com/webservice">
      <element name="GetUserRequest">
        <complexType>
          <sequence>
            <element name="UserId" type="int"/>
          </sequence>
        </complexType>
      </element>

      <element name="GetUserResponse">
        <complexType>
          <sequence>
            <element name="Name" type="string"/>
            <element name="Email" type="string"/>
          </sequence>
        </complexType>
      </element>
    </schema>
  </types>

  <!-- Messages -->
  <message name="GetUserRequestMessage">
    <part name="parameters" element="tns:GetUserRequest"/>
  </message>

  <message name="GetUserResponseMessage">
    <part name="parameters" element="tns:GetUserResponse"/>
  </message>

  <!-- Interface du service (Port Type) -->
  <portType name="UserServicePortType">
    <operation name="GetUser">
      <input message="tns:GetUserRequestMessage"/>
      <output message="tns:GetUserResponseMessage"/>
    </operation>
  </portType>

  <!-- Liaison (Binding) -->
  <binding name="UserServiceBinding" type="tns:UserServicePortType">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http"/>
    <operation name="GetUser">
      <soap:operation soapAction="http://example.com/webservice/GetUser"/>
      <input>
        <soap:body use="literal"/>
      </input>
      <output>
        <soap:body use="literal"/>
      </output>
    </operation>
  </binding>

  <!-- Point d'accès (Service) -->
  <service name="UserService">
    <port name="UserServicePort" binding="tns:UserServiceBinding">
      <soap:address location="http://example.com/services/UserService"/>
    </port>
  </service>

</definitions>
```

**Sections importantes du WSDL :**
- **Types** : Définit les structures de données
- **Message** : Définit les messages échangés
- **PortType** : Définit les opérations disponibles
- **Binding** : Spécifie le protocole (SOAP/HTTP)
- **Service** : Indique l'URL du service

## Consommer un service SOAP dans Delphi

### Utiliser l'assistant WSDL Importer

Delphi facilite grandement l'utilisation de services SOAP grâce à l'assistant **WSDL Importer**.

**Étapes pour importer un service SOAP :**

1. **Menu** : File → New → Other → WebServices → WSDL Importer
2. **URL ou Fichier** : Entrez l'URL du WSDL ou sélectionnez un fichier local
3. **Options** : Configurez les options d'importation
4. **Génération** : Delphi génère automatiquement les units Pascal

**Exemple d'URL WSDL publique pour tester :**
```
http://www.dneonline.com/calculator.asmx?WSDL
```

### Code généré automatiquement

Delphi génère automatiquement une unit avec :
- Les interfaces des services
- Les types de données
- Les méthodes pour appeler le service

**Exemple de code généré :**

```pascal
unit CalculatorService;

interface

uses
  System.SysUtils, System.Classes, Soap.InvokeRegistry, Soap.Rio,
  Soap.SOAPHTTPClient, System.Types;

type
  // Interface du service
  ICalculatorSoap = interface(IInvokable)
    ['{...GUID...}']
    function Add(intA: Integer; intB: Integer): Integer; stdcall;
    function Subtract(intA: Integer; intB: Integer): Integer; stdcall;
    function Multiply(intA: Integer; intB: Integer): Integer; stdcall;
    function Divide(intA: Integer; intB: Integer): Integer; stdcall;
  end;

function GetICalculatorSoap(UseWSDL: Boolean = False;
  Addr: string = ''; HTTPRIO: THTTPRIO = nil): ICalculatorSoap;

implementation

function GetICalculatorSoap(UseWSDL: Boolean; Addr: string;
  HTTPRIO: THTTPRIO): ICalculatorSoap;
const
  defWSDL = 'http://www.dneonline.com/calculator.asmx?WSDL';
  defURL = 'http://www.dneonline.com/calculator.asmx';
  defSvc = 'Calculator';
  defPrt = 'CalculatorSoap';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;

  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;

  try
    Result := (RIO as ICalculatorSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end
    else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(ICalculatorSoap),
    'http://tempuri.org/', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(ICalculatorSoap),
    'http://tempuri.org/%operationName%');

end.
```

### Utiliser le service généré

Une fois l'unit générée, l'utilisation est très simple :

```pascal
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CalculatorService; // Unit générée par WSDL Importer

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ButtonAddition: TButton;
    LabelResultat: TLabel;
    procedure ButtonAdditionClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonAdditionClick(Sender: TObject);  
var  
  Calculator: ICalculatorSoap;
  Nombre1, Nombre2, Resultat: Integer;
begin
  try
    // Récupérer les valeurs
    Nombre1 := StrToInt(Edit1.Text);
    Nombre2 := StrToInt(Edit2.Text);

    // Obtenir l'interface du service
    Calculator := GetICalculatorSoap(False);

    // Appeler la méthode du service
    Resultat := Calculator.Add(Nombre1, Nombre2);

    // Afficher le résultat
    LabelResultat.Caption := 'Résultat: ' + IntToStr(Resultat);

  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;

end.
```

**Explication :**
- `GetICalculatorSoap` : Fonction générée qui retourne l'interface du service
- `Calculator.Add` : Appel direct de la méthode du service
- Tout est typé, pas besoin de manipuler le XML manuellement

## Appel manuel de services SOAP

### Utiliser THTTPRIO directement

Pour plus de contrôle, vous pouvez utiliser directement `THTTPRIO` :

```pascal
uses
  Soap.Rio, Soap.SOAPHTTPClient;

procedure TForm1.AppelSOAPManuel;  
var  
  HTTPRIO: THTTPRIO;
  Service: ICalculatorSoap;
  Resultat: Integer;
begin
  HTTPRIO := THTTPRIO.Create(nil);
  try
    // Configuration
    HTTPRIO.URL := 'http://www.dneonline.com/calculator.asmx';
    HTTPRIO.Converter.Options := HTTPRIO.Converter.Options + [soSendMultiRefObj, soTryAllSchema];

    // Obtenir l'interface
    Service := HTTPRIO as ICalculatorSoap;

    // Appeler la méthode
    Resultat := Service.Add(10, 20);

    ShowMessage('Résultat: ' + IntToStr(Resultat));

  finally
    HTTPRIO.Free;
  end;
end;
```

### Envoyer un message SOAP brut

Pour un contrôle total, vous pouvez construire le XML manuellement :

```pascal
uses
  System.Net.HttpClient, System.Net.URLClient;

procedure TForm1.EnvoyerSOAPBrut;  
var  
  HTTPClient: THTTPClient;
  RequestSOAP, ResponseSOAP: string;
  Response: IHTTPResponse;
  Stream: TStringStream;
begin
  // Construire le message SOAP
  RequestSOAP :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
    '  <soap:Body>' +
    '    <Add xmlns="http://tempuri.org/">' +
    '      <intA>10</intA>' +
    '      <intB>20</intB>' +
    '    </Add>' +
    '  </soap:Body>' +
    '</soap:Envelope>';

  HTTPClient := THTTPClient.Create;
  Stream := TStringStream.Create(RequestSOAP, TEncoding.UTF8);
  try
    // Configurer les en-têtes
    HTTPClient.ContentType := 'text/xml; charset=utf-8';
    HTTPClient.CustomHeaders['SOAPAction'] := 'http://tempuri.org/Add';

    // Envoyer la requête
    Response := HTTPClient.Post('http://www.dneonline.com/calculator.asmx', Stream);

    // Récupérer la réponse
    ResponseSOAP := Response.ContentAsString;

    Memo1.Text := ResponseSOAP;

  finally
    Stream.Free;
    HTTPClient.Free;
  end;
end;
```

## Gestion de l'authentification

### Authentification de base (Basic Auth)

```pascal
procedure TForm1.SOAPAvecAuthentification;  
var  
  HTTPRIO: THTTPRIO;
  Service: IMonService;
begin
  HTTPRIO := THTTPRIO.Create(nil);
  try
    HTTPRIO.URL := 'http://example.com/service.asmx';

    // Configurer l'authentification
    HTTPRIO.HTTPWebNode.UserName := 'utilisateur';
    HTTPRIO.HTTPWebNode.Password := 'motdepasse';

    Service := HTTPRIO as IMonService;

    // Appeler le service
    Service.MaMethode;

  finally
    HTTPRIO.Free;
  end;
end;
```

### WS-Security (Username Token)

Pour une authentification WS-Security plus robuste :

```pascal
uses
  Soap.SOAPHTTPClient, Soap.WSDLBind;

procedure TForm1.SOAPAvecWSSecurity;  
var  
  HTTPRIO: THTTPRIO;
  Service: IMonService;
begin
  HTTPRIO := THTTPRIO.Create(nil);
  try
    HTTPRIO.URL := 'http://example.com/service.asmx';

    // Activer WS-Security
    HTTPRIO.Converter.Options := HTTPRIO.Converter.Options + [soSendWSSecurity];

    // Ajouter les credentials
    (HTTPRIO.HTTPWebNode as THTTPReqResp).Username := 'utilisateur';
    (HTTPRIO.HTTPWebNode as THTTPReqResp).Password := 'motdepasse';

    Service := HTTPRIO as IMonService;
    Service.MaMethode;

  finally
    HTTPRIO.Free;
  end;
end;
```

### Authentification par certificat SSL

```pascal
procedure TForm1.SOAPAvecCertificat;  
var  
  HTTPRIO: THTTPRIO;
  HTTPReqResp: THTTPReqResp;
  Service: IMonService;
begin
  HTTPRIO := THTTPRIO.Create(nil);
  try
    HTTPRIO.URL := 'https://example.com/service.asmx';

    // Accéder au client HTTP
    HTTPReqResp := HTTPRIO.HTTPWebNode as THTTPReqResp;

    // Configurer le certificat
    HTTPReqResp.InvokeOptions := HTTPReqResp.InvokeOptions + [soIgnoreInvalidCerts];

    // Pour utiliser un certificat client spécifique,
    // vous devrez utiliser Indy ou WinHTTP avec configuration avancée

    Service := HTTPRIO as IMonService;
    Service.MaMethode;

  finally
    HTTPRIO.Free;
  end;
end;
```

## Gestion des erreurs SOAP

### Intercepter les erreurs SOAP Fault

```pascal
uses
  Soap.InvokeRegistry, Soap.SOAPHTTPClient;

procedure TForm1.GererErreurSOAP;  
var  
  Calculator: ICalculatorSoap;
  Resultat: Integer;
begin
  try
    Calculator := GetICalculatorSoap(False);

    // Cette opération pourrait échouer
    Resultat := Calculator.Divide(10, 0); // Division par zéro

    ShowMessage('Résultat: ' + IntToStr(Resultat));

  except
    on E: ERemotableException do
    begin
      // Erreur SOAP spécifique
      ShowMessage('Erreur SOAP: ' + E.Message + #13#10 +
                  'Fault Code: ' + E.FaultCode + #13#10 +
                  'Fault String: ' + E.FaultString);
    end;

    on E: Exception do
    begin
      // Autres erreurs (réseau, timeout, etc.)
      ShowMessage('Erreur: ' + E.Message);
    end;
  end;
end;
```

### Logger les messages SOAP

Pour déboguer, il est utile de voir les messages échangés :

```pascal
uses
  Soap.Rio, Soap.SOAPHTTPClient;

type
  TSOAPLogger = class(TInterfacedObject, IInterface)
  private
    FMemo: TMemo;
  public
    constructor Create(AMemo: TMemo);
    procedure BeforeExecute(const MethodName: string; SOAPRequest: TStream);
    procedure AfterExecute(const MethodName: string; SOAPResponse: TStream);
  end;

constructor TSOAPLogger.Create(AMemo: TMemo);  
begin  
  inherited Create;
  FMemo := AMemo;
end;

procedure TSOAPLogger.BeforeExecute(const MethodName: string; SOAPRequest: TStream);  
var  
  Request: TStringStream;
begin
  Request := TStringStream.Create('', TEncoding.UTF8);
  try
    SOAPRequest.Position := 0;
    Request.CopyFrom(SOAPRequest, SOAPRequest.Size);

    FMemo.Lines.Add('=== REQUÊTE ' + MethodName + ' ===');
    FMemo.Lines.Add(Request.DataString);
    FMemo.Lines.Add('');
  finally
    Request.Free;
  end;
end;

procedure TSOAPLogger.AfterExecute(const MethodName: string; SOAPResponse: TStream);  
var  
  Response: TStringStream;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    SOAPResponse.Position := 0;
    Response.CopyFrom(SOAPResponse, SOAPResponse.Size);

    FMemo.Lines.Add('=== RÉPONSE ' + MethodName + ' ===');
    FMemo.Lines.Add(Response.DataString);
    FMemo.Lines.Add('');
  finally
    Response.Free;
  end;
end;
```

### Configurer les timeouts

```pascal
procedure TForm1.ConfigurerTimeouts;  
var  
  HTTPRIO: THTTPRIO;
  HTTPReqResp: THTTPReqResp;
begin
  HTTPRIO := THTTPRIO.Create(nil);
  try
    HTTPRIO.URL := 'http://example.com/service.asmx';

    // Accéder au client HTTP
    HTTPReqResp := HTTPRIO.HTTPWebNode as THTTPReqResp;

    // Configurer les timeouts (en millisecondes)
    HTTPReqResp.ConnectTimeout := 5000;   // 5 secondes pour se connecter
    HTTPReqResp.SendTimeout := 10000;     // 10 secondes pour envoyer
    HTTPReqResp.ReceiveTimeout := 30000;  // 30 secondes pour recevoir

  finally
    HTTPRIO.Free;
  end;
end;
```

## Exemple complet : Consommer un service météo

Voici un exemple complet utilisant un service SOAP de météo :

```pascal
unit UnitMeteoSOAP;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Soap.InvokeRegistry, Soap.Rio,
  Soap.SOAPHTTPClient;

type
  TFormMeteo = class(TForm)
    EditVille: TEdit;
    ButtonRechercher: TButton;
    MemoResultat: TMemo;
    LabelVille: TLabel;
    PanelHaut: TPanel;
    procedure ButtonRechercherClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AfficherMeteo(const Ville, Temperature, Description: string);
  public
  end;

var
  FormMeteo: TFormMeteo;

implementation

{$R *.dfm}

procedure TFormMeteo.FormCreate(Sender: TObject);  
begin  
  MemoResultat.Clear;
  MemoResultat.Lines.Add('Entrez un nom de ville et cliquez sur Rechercher');
end;

procedure TFormMeteo.ButtonRechercherClick(Sender: TObject);  
var  
  Ville: string;
  HTTPRIO: THTTPRIO;
  HTTPReqResp: THTTPReqResp;
begin
  Ville := EditVille.Text.Trim;

  if Ville.IsEmpty then
  begin
    ShowMessage('Veuillez entrer un nom de ville');
    Exit;
  end;

  HTTPRIO := THTTPRIO.Create(nil);
  try
    try
      // Configuration du service
      HTTPRIO.URL := 'http://www.webservicex.net/globalweather.asmx';
      HTTPRIO.WSDLLocation := 'http://www.webservicex.net/globalweather.asmx?WSDL';

      // Configurer les timeouts
      HTTPReqResp := HTTPRIO.HTTPWebNode as THTTPReqResp;
      HTTPReqResp.ConnectTimeout := 5000;
      HTTPReqResp.ReceiveTimeout := 10000;

      MemoResultat.Lines.Clear;
      MemoResultat.Lines.Add('Recherche en cours pour : ' + Ville);
      MemoResultat.Lines.Add('Connexion au service SOAP...');

      // Note: Ce service peut ne plus être disponible
      // C'est un exemple à but pédagogique

      // Dans un cas réel, vous appelleriez les méthodes du service ici
      // Exemple: Resultat := (HTTPRIO as IGlobalWeather).GetWeather(Ville, 'France');

      MemoResultat.Lines.Add('Service contacté avec succès');

    except
      on E: ERemotableException do
      begin
        MemoResultat.Lines.Add('Erreur SOAP:');
        MemoResultat.Lines.Add('Code: ' + E.FaultCode);
        MemoResultat.Lines.Add('Message: ' + E.FaultString);
      end;

      on E: Exception do
      begin
        MemoResultat.Lines.Add('Erreur: ' + E.Message);
        ShowMessage('Impossible de contacter le service: ' + E.Message);
      end;
    end;

  finally
    HTTPRIO.Free;
  end;
end;

procedure TFormMeteo.AfficherMeteo(const Ville, Temperature, Description: string);  
begin  
  MemoResultat.Lines.Clear;
  MemoResultat.Lines.Add('Météo pour : ' + Ville);
  MemoResultat.Lines.Add('');
  MemoResultat.Lines.Add('Température : ' + Temperature);
  MemoResultat.Lines.Add('Description : ' + Description);
end;

end.
```

## Créer un serveur SOAP avec Delphi

### Utiliser l'assistant WebService Application

Pour créer votre propre service SOAP :

**Étapes :**
1. File → New → Other → WebServices → SOAP Server Application
2. Choisir le type de serveur (ISAPI, CGI, ou Standalone)
3. Définir l'interface du service
4. Implémenter les méthodes

**Exemple d'interface de service :**

```pascal
unit InterfaceCalculateur;

interface

uses
  System.SysUtils, Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns;

type
  // Interface du service (contrat)
  ICalculateur = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789012}']

    function Addition(A, B: Double): Double; stdcall;
    function Soustraction(A, B: Double): Double; stdcall;
    function Multiplication(A, B: Double): Double; stdcall;
    function Division(A, B: Double): Double; stdcall;
    function Puissance(Base, Exposant: Double): Double; stdcall;
  end;

implementation

initialization
  // Enregistrer l'interface
  InvRegistry.RegisterInterface(TypeInfo(ICalculateur),
    'urn:CalculateurIntf', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(ICalculateur),
    'urn:CalculateurIntf-ICalculateur#%operationName%');

end.
```

**Implémentation du service :**

```pascal
unit ImplementationCalculateur;

interface

uses
  System.SysUtils, Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns,
  InterfaceCalculateur;

type
  // Classe d'implémentation
  TCalculateur = class(TInvokableClass, ICalculateur)
  public
    function Addition(A, B: Double): Double; stdcall;
    function Soustraction(A, B: Double): Double; stdcall;
    function Multiplication(A, B: Double): Double; stdcall;
    function Division(A, B: Double): Double; stdcall;
    function Puissance(Base, Exposant: Double): Double; stdcall;
  end;

implementation

uses
  System.Math;

{ TCalculateur }

function TCalculateur.Addition(A, B: Double): Double;  
begin  
  Result := A + B;
end;

function TCalculateur.Soustraction(A, B: Double): Double;  
begin  
  Result := A - B;
end;

function TCalculateur.Multiplication(A, B: Double): Double;  
begin  
  Result := A * B;
end;

function TCalculateur.Division(A, B: Double): Double;  
begin  
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');
  Result := A / B;
end;

function TCalculateur.Puissance(Base, Exposant: Double): Double;  
begin  
  Result := Power(Base, Exposant);
end;

initialization
  // Enregistrer l'implémentation
  InvRegistry.RegisterInvokableClass(TCalculateur);

end.
```

**Module WebService :**

```pascal
unit WebModuleCalculateur;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Soap.InvokeRegistry,
  Soap.WSDLIntf, Soap.WebServExp, Soap.WSDLBind, Xml.XMLSchema,
  InterfaceCalculateur, ImplementationCalculateur;

type
  TWebModule1 = class(TWebModule)
    HTTPSoapDispatcher1: THTTPSoapDispatcher;
    HTTPSoapPascalInvoker1: THTTPSoapPascalInvoker;
    WSDLHTMLPublish1: TWSDLHTMLPublish;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // Publier le WSDL
  WSDLHTMLPublish1.ServiceInfo(Sender, Request, Response, Handled);
end;

end.
```

### Tester le service SOAP

Une fois le serveur démarré, vous pouvez :

1. Accéder au WSDL : `http://localhost:8080/wsdl/ICalculateur`
2. Voir la page d'accueil : `http://localhost:8080`
3. Utiliser le service avec un client SOAP

## Outils et bibliothèques utiles

### SoapUI

**SoapUI** est un outil gratuit pour tester les services SOAP :
- Importer un WSDL
- Créer des requêtes de test
- Valider les réponses
- Tester la performance
- Simuler des services (mock)

**Utilisation :**
1. Télécharger SoapUI (https://www.soapui.org/)
2. New SOAP Project
3. Entrer l'URL du WSDL
4. Tester les opérations

### Postman

**Postman** supporte également SOAP :
- Créer des requêtes SOAP
- Gérer les en-têtes
- Sauvegarder les requêtes
- Partager avec l'équipe

### Composants tiers pour Delphi

- **RemObjects SDK** : Framework complet pour services web
- **kbmMW** : Middleware multi-niveaux avec support SOAP
- **TMS XData** : Services REST et OData

## Bonnes pratiques

### 1. Utiliser WSDL Importer

Toujours utiliser l'assistant WSDL Importer plutôt que coder manuellement :
```pascal
// ✅ Bon - Généré automatiquement
Calculator := GetICalculatorSoap(False);  
Result := Calculator.Add(10, 20);  

// ❌ Éviter - Construction XML manuelle
// Trop verbeux et sujet aux erreurs
```

### 2. Gérer les timeouts

Toujours configurer des timeouts appropriés :
```pascal
HTTPReqResp := HTTPRIO.HTTPWebNode as THTTPReqResp;  
HTTPReqResp.ConnectTimeout := 5000;  
HTTPReqResp.ReceiveTimeout := 30000;  
```

### 3. Gérer les erreurs proprement

```pascal
try
  Result := Service.MaMethode;
except
  on E: ERemotableException do
    // Erreur SOAP
  on E: Exception do
    // Erreur réseau/autre
end;
```

### 4. Libérer les ressources

```pascal
HTTPRIO := THTTPRIO.Create(nil);  
try  
  // Utilisation
finally
  HTTPRIO.Free;
end;
```

### 5. Logger pour le débogage

En développement, activez le logging des messages :
```pascal
// Afficher les requêtes/réponses SOAP
// Aide énormément au débogage
```

### 6. Valider les données

Avant d'envoyer, validez les paramètres :
```pascal
if Montant <= 0 then
  raise Exception.Create('Le montant doit être positif');

if Email.IsEmpty or not ContainsText(Email, '@') then
  raise Exception.Create('Email invalide');
```

### 7. Utiliser HTTPS en production

Pour la sécurité, toujours utiliser HTTPS :
```pascal
HTTPRIO.URL := 'https://secure.example.com/service.asmx';
```

### 8. Mettre en cache les services

Si vous appelez souvent le même service, conservez l'instance :
```pascal
type
  TFormPrincipale = class(TForm)
  private
    FServiceCalculateur: ICalculateur;
    function GetServiceCalculateur: ICalculateur;
  end;

function TFormPrincipale.GetServiceCalculateur: ICalculateur;  
begin  
  if not Assigned(FServiceCalculateur) then
    FServiceCalculateur := GetICalculateurSoap(False);
  Result := FServiceCalculateur;
end;
```

## Migration de SOAP vers REST

### Pourquoi migrer ?

De nombreuses organisations migrent de SOAP vers REST pour :
- Simplicité accrue
- Meilleures performances
- Support mobile facilité
- Moins de verbosité

### Approche de migration

**Étape 1 : Maintenir SOAP et REST en parallèle**
```pascal
// Offrir les deux options
if PreferencesUtilisateur.UseREST then
  Result := AppelerServiceREST
else
  Result := AppelerServiceSOAP;
```

**Étape 2 : Créer une couche d'abstraction**
```pascal
type
  IServiceCalculateur = interface
    function Additionner(A, B: Double): Double;
  end;

  TServiceSOAP = class(TInterfacedObject, IServiceCalculateur)
    function Additionner(A, B: Double): Double;
  end;

  TServiceREST = class(TInterfacedObject, IServiceCalculateur)
    function Additionner(A, B: Double): Double;
  end;
```

**Étape 3 : Migrer progressivement**
- Commencer par les nouvelles fonctionnalités en REST
- Migrer les services les moins critiques
- Tester exhaustivement
- Migrer les services critiques en dernier

## Résumé

### Points clés SOAP

✅ **Concepts fondamentaux :**
- SOAP = protocole basé XML pour services web
- WSDL = contrat décrivant le service
- Structure : Envelope → Header → Body
- Support des transactions et sécurité WS-*

✅ **Utilisation dans Delphi :**
- WSDL Importer pour générer le code
- THTTPRIO pour les appels
- Gestion d'erreurs avec ERemotableException
- Configuration des timeouts

✅ **Quand utiliser SOAP :**
- Applications d'entreprise
- Transactions critiques
- Sécurité stricte (WS-Security)
- Interopérabilité avec systèmes legacy

✅ **Bonnes pratiques :**
- Utiliser WSDL Importer
- Configurer les timeouts
- Gérer les erreurs proprement
- Logger en développement
- Utiliser HTTPS en production
- Valider les données

✅ **Alternatives modernes :**
- REST pour nouvelles API
- JSON pour légèreté
- GraphQL pour flexibilité

SOAP reste pertinent pour les systèmes d'entreprise nécessitant robustesse, sécurité et transactions, même si REST domine les nouvelles applications web et mobiles.

⏭️ [Architecture client-serveur](/10-communication-et-services-reseaux/05-architecture-client-serveur.md)
