🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.7 Stockage sécurisé des identifiants

## Introduction

Le stockage des identifiants (mots de passe, clés API, tokens, certificats) est l'un des défis les plus critiques en sécurité. Un seul identifiant compromis peut donner accès à l'ensemble de votre système.

**Analogie du monde réel** : Imaginez que vous cachez la clé de votre maison. La mettre sous le paillasson (stocker en clair dans le code) est très pratique mais extrêmement dangereux. La mettre dans un coffre-fort avec un code (chiffrement + protection système) est bien plus sûr.

### Le problème du stockage des identifiants

**Les dangers** :
- Code source accessible (GitHub, partage, etc.)
- Fichiers de configuration lisibles
- Décompilation des exécutables
- Vol de l'ordinateur ou du serveur
- Attaques par accès mémoire

**Ce qu'il faut protéger** :
- Mots de passe de bases de données
- Clés API (Google, AWS, Azure, etc.)
- Tokens d'authentification
- Certificats et clés privées
- Secrets de chiffrement
- Identifiants SMTP, FTP, etc.

## Les méthodes dangereuses (à NE JAMAIS FAIRE)

### ❌ Dans le code source

```pascal
// ❌ EXTRÊMEMENT DANGEREUX - Ne JAMAIS faire ça !
const
  DB_PASSWORD = 'MotDePasseSecret123';
  API_KEY = 'sk_live_51Hxyz...';
  ENCRYPTION_KEY = 'MaCleDeChiffrement';

procedure ConnecterBD;
begin
  FDConnection1.Params.Add('Password=' + DB_PASSWORD);  // DANGER !
end;
```

**Pourquoi c'est dangereux ?**
- Visible dans le code source
- Récupérable par décompilation
- Exposé dans les systèmes de versionnement (Git)
- Impossible à changer sans recompiler

### ❌ Dans un fichier INI non chiffré

```pascal
// ❌ DANGEREUX - Fichier config.ini :
[Database]
Server=localhost
Username=admin
Password=MotDePasseSecret123    // Lisible par n'importe qui !

[API]
GoogleAPIKey=AIzaSyD...
AWSSecretKey=wJalrXUtnF...
```

### ❌ Dans la base de données en clair

```sql
-- ❌ DANGEREUX
CREATE TABLE Configuration (
    Cle VARCHAR(50),
    Valeur VARCHAR(255)
);

INSERT INTO Configuration VALUES ('SMTP_Password', 'motdepasse123');
```

### ❌ Dans les variables d'environnement Windows Registry

```pascal
// ❌ MOYENNEMENT DANGEREUX (mieux que rien mais pas sécurisé)
procedure SauvegarderDansRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    Registry.OpenKey('\Software\MonApp\Config', True);
    Registry.WriteString('Password', 'MonMotDePasse');  // En clair !
  finally
    Registry.Free;
  end;
end;
```

## Solutions sécurisées

### 1. Windows DPAPI (Data Protection API)

Le DPAPI est une API Windows qui chiffre les données en utilisant les identifiants de l'utilisateur ou de la machine.

**Avantages** :
- Chiffrement automatique par Windows
- Pas besoin de gérer les clés
- Intégré au système d'exploitation
- Gratuit et fiable

**Principe** : Les données sont chiffrées avec une clé dérivée du compte Windows. Seul ce compte peut les déchiffrer.

```pascal
uses
  Winapi.Windows, System.SysUtils;

type
  DATA_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;

  TDPAPIHelper = class
  public
    class function ChiffrerDonnees(const ADonnees: string): TBytes;
    class function DechiffrerDonnees(const ADonneesChiffrees: TBytes): string;
  end;

// Importation des fonctions DPAPI
function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: PWideChar;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer; pPromptStruct: Pointer;
  dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall;
  external 'Crypt32.dll' name 'CryptProtectData';

function CryptUnprotectData(pDataIn: PDATA_BLOB; ppszDataDescr: PPWideChar;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer; pPromptStruct: Pointer;
  dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall;
  external 'Crypt32.dll' name 'CryptUnprotectData';

class function TDPAPIHelper.ChiffrerDonnees(const ADonnees: string): TBytes;
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  DonneesBytes: TBytes;
begin
  // Convertir la chaîne en bytes
  DonneesBytes := TEncoding.UTF8.GetBytes(ADonnees);

  // Préparer la structure d'entrée
  DataIn.cbData := Length(DonneesBytes);
  DataIn.pbData := @DonneesBytes[0];

  // Chiffrer avec DPAPI
  if CryptProtectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    try
      // Copier les données chiffrées
      SetLength(Result, DataOut.cbData);
      Move(DataOut.pbData^, Result[0], DataOut.cbData);
    finally
      // Libérer la mémoire allouée par Windows
      LocalFree(HLOCAL(DataOut.pbData));
    end;
  end
  else
    raise Exception.Create('Erreur de chiffrement DPAPI');
end;

class function TDPAPIHelper.DechiffrerDonnees(const ADonneesChiffrees: TBytes): string;
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  ResultBytes: TBytes;
begin
  // Préparer la structure d'entrée
  DataIn.cbData := Length(ADonneesChiffrees);
  DataIn.pbData := @ADonneesChiffrees[0];

  // Déchiffrer avec DPAPI
  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    try
      // Copier les données déchiffrées
      SetLength(ResultBytes, DataOut.cbData);
      Move(DataOut.pbData^, ResultBytes[0], DataOut.cbData);
      Result := TEncoding.UTF8.GetString(ResultBytes);
    finally
      // Libérer la mémoire
      LocalFree(HLOCAL(DataOut.pbData));
    end;
  end
  else
    raise Exception.Create('Erreur de déchiffrement DPAPI');
end;

// Exemple d'utilisation
procedure SauvegarderMotDePasseSecurise;
var
  MotDePasse: string;
  DonneesChiffrees: TBytes;
  Fichier: TFileStream;
begin
  MotDePasse := 'MonMotDePasseSecret123';

  // Chiffrer avec DPAPI
  DonneesChiffrees := TDPAPIHelper.ChiffrerDonnees(MotDePasse);

  // Sauvegarder dans un fichier
  Fichier := TFileStream.Create('password.dat', fmCreate);
  try
    Fichier.Write(DonneesChiffrees[0], Length(DonneesChiffrees));
  finally
    Fichier.Free;
  end;
end;

procedure ChargerMotDePasseSecurise;
var
  DonneesChiffrees: TBytes;
  Fichier: TFileStream;
  MotDePasse: string;
begin
  // Lire le fichier
  Fichier := TFileStream.Create('password.dat', fmOpenRead);
  try
    SetLength(DonneesChiffrees, Fichier.Size);
    Fichier.Read(DonneesChiffrees[0], Fichier.Size);
  finally
    Fichier.Free;
  end;

  // Déchiffrer avec DPAPI
  MotDePasse := TDPAPIHelper.DechiffrerDonnees(DonneesChiffrees);

  // Utiliser le mot de passe
  FDConnection1.Params.Add('Password=' + MotDePasse);
end;
```

### 2. macOS Keychain

Sur macOS, utilisez le Keychain pour stocker les identifiants de manière sécurisée.

```pascal
{$IFDEF MACOS}
uses
  Macapi.Security;

function SauvegarderDansKeychain(const AService, ACompte, AMotDePasse: string): Boolean;
var
  Status: OSStatus;
  ServicePtr: MarshaledAString;
  ComptePtr: MarshaledAString;
  MotDePassePtr: Pointer;
begin
  ServicePtr := MarshaledAString(TMarshal.AsAnsi(AService));
  ComptePtr := MarshaledAString(TMarshal.AsAnsi(ACompte));
  MotDePassePtr := MarshaledAString(TMarshal.AsAnsi(AMotDePasse));

  Status := SecKeychainAddGenericPassword(
    nil,  // Keychain par défaut
    Length(AService),
    ServicePtr,
    Length(ACompte),
    ComptePtr,
    Length(AMotDePasse),
    MotDePassePtr,
    nil
  );

  Result := Status = errSecSuccess;
end;

function ChargerDepuisKeychain(const AService, ACompte: string): string;
var
  Status: OSStatus;
  ServicePtr: MarshaledAString;
  ComptePtr: MarshaledAString;
  MotDePasseLength: UInt32;
  MotDePassePtr: Pointer;
begin
  Result := '';
  ServicePtr := MarshaledAString(TMarshal.AsAnsi(AService));
  ComptePtr := MarshaledAString(TMarshal.AsAnsi(ACompte));

  Status := SecKeychainFindGenericPassword(
    nil,  // Keychain par défaut
    Length(AService),
    ServicePtr,
    Length(ACompte),
    ComptePtr,
    @MotDePasseLength,
    @MotDePassePtr,
    nil
  );

  if Status = errSecSuccess then
  begin
    SetString(Result, PAnsiChar(MotDePassePtr), MotDePasseLength);
    SecKeychainItemFreeContent(nil, MotDePassePtr);
  end;
end;

// Utilisation
procedure ConfigurerBaseDonnees;
var
  Password: string;
begin
  Password := ChargerDepuisKeychain('MonApplication', 'DatabasePassword');
  FDConnection1.Params.Add('Password=' + Password);
end;
{$ENDIF}
```

### 3. Classe multi-plateforme de gestion des secrets

Créons une classe qui utilise la meilleure méthode selon la plateforme :

```pascal
unit UnitGestionSecrets;

interface

uses
  System.SysUtils, System.Classes;

type
  TGestionSecrets = class
  private
    class function ChiffrerWindowsDPAPI(const ADonnees: string): TBytes;
    class function DechiffrerWindowsDPAPI(const ADonnees: TBytes): string;
    {$IFDEF MACOS}
    class function SauvegarderKeychainMac(const ACle, AValeur: string): Boolean;
    class function ChargerKeychainMac(const ACle: string): string;
    {$ENDIF}
    class function ChiffrerGenerique(const ADonnees, ACleMaitre: string): string;
    class function DechiffrerGenerique(const ADonnees, ACleMaitre: string): string;
  public
    class procedure Sauvegarder(const ACle, AValeur: string);
    class function Charger(const ACle: string): string;
    class procedure Supprimer(const ACle: string);
    class function Existe(const ACle: string): Boolean;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF MACOS}
  Macapi.Security,
  {$ENDIF}
  System.IOUtils, System.NetEncoding;

class procedure TGestionSecrets.Sauvegarder(const ACle, AValeur: string);
{$IFDEF MSWINDOWS}
var
  DonneesChiffrees: TBytes;
  CheminFichier: string;
  Fichier: TFileStream;
begin
  // Utiliser DPAPI sur Windows
  DonneesChiffrees := ChiffrerWindowsDPAPI(AValeur);

  CheminFichier := TPath.Combine(
    TPath.GetHomePath,
    '.monapp_secrets',
    ACle + '.dat'
  );

  ForceDirectories(ExtractFilePath(CheminFichier));

  Fichier := TFileStream.Create(CheminFichier, fmCreate);
  try
    Fichier.Write(DonneesChiffrees[0], Length(DonneesChiffrees));
  finally
    Fichier.Free;
  end;
end;
{$ELSE}
{$IFDEF MACOS}
begin
  // Utiliser Keychain sur macOS
  SauvegarderKeychainMac(ACle, AValeur);
end;
{$ELSE}
var
  CleMaitre: string;
  DonneesChiffrees: string;
  CheminFichier: string;
begin
  // Chiffrement générique pour Linux/Android/iOS
  CleMaitre := ObtenirCleMaitre; // À implémenter selon vos besoins
  DonneesChiffrees := ChiffrerGenerique(AValeur, CleMaitre);

  CheminFichier := TPath.Combine(
    TPath.GetHomePath,
    '.monapp_secrets',
    ACle + '.enc'
  );

  ForceDirectories(ExtractFilePath(CheminFichier));
  TFile.WriteAllText(CheminFichier, DonneesChiffrees);
end;
{$ENDIF}
{$ENDIF}

class function TGestionSecrets.Charger(const ACle: string): string;
{$IFDEF MSWINDOWS}
var
  DonneesChiffrees: TBytes;
  CheminFichier: string;
  Fichier: TFileStream;
begin
  CheminFichier := TPath.Combine(
    TPath.GetHomePath,
    '.monapp_secrets',
    ACle + '.dat'
  );

  if not FileExists(CheminFichier) then
    Exit('');

  Fichier := TFileStream.Create(CheminFichier, fmOpenRead);
  try
    SetLength(DonneesChiffrees, Fichier.Size);
    Fichier.Read(DonneesChiffrees[0], Fichier.Size);
  finally
    Fichier.Free;
  end;

  Result := DechiffrerWindowsDPAPI(DonneesChiffrees);
end;
{$ELSE}
{$IFDEF MACOS}
begin
  Result := ChargerKeychainMac(ACle);
end;
{$ELSE}
var
  CleMaitre: string;
  DonneesChiffrees: string;
  CheminFichier: string;
begin
  CheminFichier := TPath.Combine(
    TPath.GetHomePath,
    '.monapp_secrets',
    ACle + '.enc'
  );

  if not FileExists(CheminFichier) then
    Exit('');

  DonneesChiffrees := TFile.ReadAllText(CheminFichier);
  CleMaitre := ObtenirCleMaitre;
  Result := DechiffrerGenerique(DonneesChiffrees, CleMaitre);
end;
{$ENDIF}
{$ENDIF}

class procedure TGestionSecrets.Supprimer(const ACle: string);
var
  CheminFichier: string;
begin
  {$IFDEF MSWINDOWS}
  CheminFichier := TPath.Combine(TPath.GetHomePath, '.monapp_secrets', ACle + '.dat');
  {$ELSE}
  CheminFichier := TPath.Combine(TPath.GetHomePath, '.monapp_secrets', ACle + '.enc');
  {$ENDIF}

  if FileExists(CheminFichier) then
    DeleteFile(CheminFichier);
end;

class function TGestionSecrets.Existe(const ACle: string): Boolean;
var
  CheminFichier: string;
begin
  {$IFDEF MSWINDOWS}
  CheminFichier := TPath.Combine(TPath.GetHomePath, '.monapp_secrets', ACle + '.dat');
  {$ELSE}
  CheminFichier := TPath.Combine(TPath.GetHomePath, '.monapp_secrets', ACle + '.enc');
  {$ENDIF}

  Result := FileExists(CheminFichier);
end;

// Implémentations des méthodes privées (ChiffrerWindowsDPAPI, etc.)
// Voir exemples précédents

end.

// Utilisation simple et multi-plateforme
procedure ConfigurerApplication;
begin
  // Sauvegarder un mot de passe (une seule fois, lors de la configuration)
  TGestionSecrets.Sauvegarder('DBPassword', 'MotDePasseSecret123');
  TGestionSecrets.Sauvegarder('APIKey', 'sk_live_51Hxyz...');

  // Charger les identifiants au démarrage de l'application
  FDConnection1.Params.Add('Password=' + TGestionSecrets.Charger('DBPassword'));

  FAPIKey := TGestionSecrets.Charger('APIKey');
end;
```

### 4. Variables d'environnement

Les variables d'environnement sont une bonne solution pour les applications serveur.

```pascal
uses
  System.SysUtils;

function LireVariableEnvironnement(const ANom: string; const AParDefaut: string = ''): string;
begin
  Result := GetEnvironmentVariable(ANom);
  if Result = '' then
    Result := AParDefaut;
end;

procedure ConfigurerDepuisEnvironnement;
begin
  // Lire depuis les variables d'environnement
  FDConnection1.Params.Add('Server=' + LireVariableEnvironnement('DB_SERVER', 'localhost'));
  FDConnection1.Params.Add('Database=' + LireVariableEnvironnement('DB_NAME', 'mydb'));
  FDConnection1.Params.Add('User_Name=' + LireVariableEnvironnement('DB_USER', 'root'));
  FDConnection1.Params.Add('Password=' + LireVariableEnvironnement('DB_PASSWORD'));

  // Clé API depuis variable d'environnement
  FAPIKey := LireVariableEnvironnement('GOOGLE_API_KEY');

  if FAPIKey = '' then
    raise Exception.Create('Variable d''environnement GOOGLE_API_KEY non définie');
end;
```

**Configuration des variables sous Windows** :
```batch
REM Définir temporairement (session actuelle)
set DB_PASSWORD=MotDePasseSecret

REM Définir de manière permanente (utilisateur)
setx DB_PASSWORD "MotDePasseSecret"

REM Définir de manière permanente (système - admin requis)
setx DB_PASSWORD "MotDePasseSecret" /M
```

**Configuration sous Linux/macOS** :
```bash
# Temporaire (session actuelle)
export DB_PASSWORD="MotDePasseSecret"

# Permanent (ajouter dans ~/.bashrc ou ~/.zshrc)
echo 'export DB_PASSWORD="MotDePasseSecret"' >> ~/.bashrc
```

### 5. Fichiers de configuration chiffrés

Créez un fichier de configuration dont le contenu est chiffré :

```pascal
type
  TConfigSecurisee = class
  private
    FCheminFichier: string;
    FCleMaitre: string;
    FValeurs: TDictionary<string, string>;
    procedure ChargerFichier;
    procedure SauvegarderFichier;
    function ChiffrerContenu(const AContenu: string): string;
    function DechiffrerContenu(const AContenu: string): string;
  public
    constructor Create(const ACheminFichier, ACleMaitre: string);
    destructor Destroy; override;
    procedure DefinirValeur(const ACle, AValeur: string);
    function ObtenirValeur(const ACle: string; const AParDefaut: string = ''): string;
    procedure Enregistrer;
  end;

constructor TConfigSecurisee.Create(const ACheminFichier, ACleMaitre: string);
begin
  inherited Create;
  FCheminFichier := ACheminFichier;
  FCleMaitre := ACleMaitre;
  FValeurs := TDictionary<string, string>.Create;

  if FileExists(FCheminFichier) then
    ChargerFichier;
end;

destructor TConfigSecurisee.Destroy;
begin
  FValeurs.Free;
  inherited;
end;

procedure TConfigSecurisee.ChargerFichier;
var
  ContenuChiffre: string;
  ContenuClair: string;
  Lignes: TStringList;
  Ligne: string;
  Parties: TArray<string>;
begin
  // Lire le fichier chiffré
  ContenuChiffre := TFile.ReadAllText(FCheminFichier);

  // Déchiffrer
  ContenuClair := DechiffrerContenu(ContenuChiffre);

  // Parser les lignes
  Lignes := TStringList.Create;
  try
    Lignes.Text := ContenuClair;

    for Ligne in Lignes do
    begin
      if (Ligne.Trim <> '') and (not Ligne.StartsWith('#')) then
      begin
        Parties := Ligne.Split(['='], 2);
        if Length(Parties) = 2 then
          FValeurs.AddOrSetValue(Parties[0].Trim, Parties[1].Trim);
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

procedure TConfigSecurisee.SauvegarderFichier;
var
  Lignes: TStringList;
  Cle: string;
  ContenuClair: string;
  ContenuChiffre: string;
begin
  Lignes := TStringList.Create;
  try
    Lignes.Add('# Configuration sécurisée - Chiffrée');
    Lignes.Add('# Générée le : ' + DateTimeToStr(Now));
    Lignes.Add('');

    for Cle in FValeurs.Keys do
      Lignes.Add(Cle + '=' + FValeurs[Cle]);

    ContenuClair := Lignes.Text;
  finally
    Lignes.Free;
  end;

  // Chiffrer le contenu
  ContenuChiffre := ChiffrerContenu(ContenuClair);

  // Sauvegarder
  TFile.WriteAllText(FCheminFichier, ContenuChiffre);
end;

function TConfigSecurisee.ChiffrerContenu(const AContenu: string): string;
begin
  // Utiliser votre méthode de chiffrement préférée (AES, etc.)
  // Exemple simplifié avec Base64 (à remplacer par un vrai chiffrement)
  Result := TNetEncoding.Base64.Encode(AContenu);
end;

function TConfigSecurisee.DechiffrerContenu(const AContenu: string): string;
begin
  // Déchiffrer (correspondant à ChiffrerContenu)
  Result := TNetEncoding.Base64.Decode(AContenu);
end;

procedure TConfigSecurisee.DefinirValeur(const ACle, AValeur: string);
begin
  FValeurs.AddOrSetValue(ACle, AValeur);
end;

function TConfigSecurisee.ObtenirValeur(const ACle: string; const AParDefaut: string): string;
begin
  if not FValeurs.TryGetValue(ACle, Result) then
    Result := AParDefaut;
end;

procedure TConfigSecurisee.Enregistrer;
begin
  SauvegarderFichier;
end;

// Utilisation
procedure ConfigurerAvecFichierSecurise;
var
  Config: TConfigSecurisee;
  CleMaitre: string;
begin
  // La clé maître pourrait venir de DPAPI, d'un prompt utilisateur, etc.
  CleMaitre := TGestionSecrets.Charger('MasterKey');

  Config := TConfigSecurisee.Create('config.secure', CleMaitre);
  try
    // Lire la configuration
    FDConnection1.Params.Add('Server=' + Config.ObtenirValeur('DB_Server', 'localhost'));
    FDConnection1.Params.Add('Password=' + Config.ObtenirValeur('DB_Password'));

    FAPIKey := Config.ObtenirValeur('API_Key');
  finally
    Config.Free;
  end;
end;
```

### 6. Gestionnaire de secrets cloud

Pour les applications cloud, utilisez des services dédiés :

```pascal
type
  TAWSSecretsManager = class
  private
    FRegion: string;
    FAccessKeyID: string;
    FSecretAccessKey: string;
  public
    constructor Create(const ARegion, AAccessKeyID, ASecretAccessKey: string);
    function ObtenirSecret(const ASecretName: string): string;
  end;

constructor TAWSSecretsManager.Create(const ARegion, AAccessKeyID, ASecretAccessKey: string);
begin
  inherited Create;
  FRegion := ARegion;
  FAccessKeyID := AAccessKeyID;
  FSecretAccessKey := ASecretAccessKey;
end;

function TAWSSecretsManager.ObtenirSecret(const ASecretName: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONResponse: TJSONObject;
  URL: string;
begin
  // Construire l'URL de l'API AWS Secrets Manager
  URL := Format('https://secretsmanager.%s.amazonaws.com/', [FRegion]);

  RESTClient := TRESTClient.Create(URL);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajouter l'authentification AWS (simplifiée, nécessite signature réelle)
    RESTRequest.AddAuthParameter('X-Amz-Target', 'secretsmanager.GetSecretValue',
                                  TRESTRequestParameterKind.pkHTTPHEADER);

    // Corps de la requête
    RESTRequest.AddBody(Format('{"SecretId":"%s"}', [ASecretName]),
                        TRESTContentType.ctAPPLICATION_JSON);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      try
        Result := JSONResponse.GetValue<string>('SecretString');
      finally
        JSONResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur AWS : %s', [RESTResponse.Content]);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

// Utilisation
procedure ConfigurerAvecAWS;
var
  SecretsManager: TAWSSecretsManager;
  DBPassword: string;
begin
  SecretsManager := TAWSSecretsManager.Create(
    'eu-west-1',
    'AKIA...',  // Ces identifiants viendraient d'un endroit sécurisé
    'wJalr...'
  );
  try
    // Récupérer le secret depuis AWS
    DBPassword := SecretsManager.ObtenirSecret('prod/database/password');

    FDConnection1.Params.Add('Password=' + DBPassword);
  finally
    SecretsManager.Free;
  end;
end;
```

**Services cloud alternatifs** :
- **Azure Key Vault** : Microsoft Azure
- **Google Cloud Secret Manager** : Google Cloud Platform
- **HashiCorp Vault** : Solution open-source indépendante

## Première configuration de l'application

### Invite utilisateur au premier lancement

```pascal
type
  TConfigurationInitiale = class
  public
    class procedure DemanderEtSauvegarderIdentifiants;
  end;

class procedure TConfigurationInitiale.DemanderEtSauvegarderIdentifiants;
var
  Form: TForm;
  EditServer: TEdit;
  EditDatabase: TEdit;
  EditUser: TEdit;
  EditPassword: TEdit;
  BtnOK: TButton;
  ModalResult: Integer;
begin
  // Vérifier si c'est la première exécution
  if TGestionSecrets.Existe('DB_Password') then
    Exit; // Déjà configuré

  Form := TForm.Create(nil);
  try
    Form.Caption := 'Configuration initiale';
    Form.Width := 400;
    Form.Height := 250;
    Form.Position := poScreenCenter;

    // Créer les contrôles
    EditServer := TEdit.Create(Form);
    EditServer.Parent := Form;
    EditServer.Left := 20;
    EditServer.Top := 30;
    EditServer.Width := 350;
    EditServer.TextHint := 'Serveur de base de données';

    EditDatabase := TEdit.Create(Form);
    EditDatabase.Parent := Form;
    EditDatabase.Left := 20;
    EditDatabase.Top := 60;
    EditDatabase.Width := 350;
    EditDatabase.TextHint := 'Nom de la base de données';

    EditUser := TEdit.Create(Form);
    EditUser.Parent := Form;
    EditUser.Left := 20;
    EditUser.Top := 90;
    EditUser.Width := 350;
    EditUser.TextHint := 'Nom d''utilisateur';

    EditPassword := TEdit.Create(Form);
    EditPassword.Parent := Form;
    EditPassword.Left := 20;
    EditPassword.Top := 120;
    EditPassword.Width := 350;
    EditPassword.TextHint := 'Mot de passe';
    EditPassword.PasswordChar := '*';

    BtnOK := TButton.Create(Form);
    BtnOK.Parent := Form;
    BtnOK.Caption := 'Enregistrer';
    BtnOK.Left := 150;
    BtnOK.Top := 160;
    BtnOK.ModalResult := mrOk;

    ModalResult := Form.ShowModal;

    if ModalResult = mrOk then
    begin
      // Sauvegarder de manière sécurisée
      TGestionSecrets.Sauvegarder('DB_Server', EditServer.Text);
      TGestionSecrets.Sauvegarder('DB_Database', EditDatabase.Text);
      TGestionSecrets.Sauvegarder('DB_User', EditUser.Text);
      TGestionSecrets.Sauvegarder('DB_Password', EditPassword.Text);

      ShowMessage('Configuration enregistrée de manière sécurisée');
    end;
  finally
    Form.Free;
  end;
end;

// Appeler au démarrage de l'application
procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  TConfigurationInitiale.DemanderEtSauvegarderIdentifiants;

  // Charger la configuration
  ChargerConfiguration;
end;
```

## Rotation des secrets

Il est important de changer régulièrement les identifiants :

```pascal
type
  TRotationSecrets = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure RoterMotDePasseBD;
    procedure RoterCleAPI;
    procedure VerifierDateExpiration;
  end;

constructor TRotationSecrets.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TRotationSecrets.RoterMotDePasseBD;
var
  NouveauPassword: string;
  Query: TFDQuery;
begin
  // Générer un nouveau mot de passe fort
  NouveauPassword := GenererMotDePasseFort(32);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Changer le mot de passe dans MySQL
    Query.SQL.Text := 'ALTER USER ''monuser''@''localhost'' IDENTIFIED BY :NewPassword';
    Query.ParamByName('NewPassword').AsString := NouveauPassword;
    Query.ExecSQL;

    // Sauvegarder le nouveau mot de passe
    TGestionSecrets.Sauvegarder('DB_Password', NouveauPassword);

    // Logger l'événement
    TLogger.Instance.Info('Rotation mot de passe BD', 'Succès');
  finally
    Query.Free;
  end;
end;

procedure TRotationSecrets.VerifierDateExpiration;
var
  DateDerniereRotation: TDateTime;
  JoursDepuisRotation: Integer;
begin
  // Lire la date de dernière rotation
  DateDerniereRotation := StrToDateDef(
    TGestionSecrets.Charger('LastPasswordRotation'),
    Now - 365  // Par défaut : il y a 1 an
  );

  JoursDepuisRotation := DaysBetween(Now, DateDerniereRotation);

  // Rotation tous les 90 jours
  if JoursDepuisRotation > 90 then
  begin
    ShowMessage('Le mot de passe de la base de données doit être changé');
    // Déclencher la rotation ou avertir l'administrateur
  end;
end;

function GenererMotDePasseFort(ALongueur: Integer): string;
const
  CARACTERES = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()';
var
  i: Integer;
begin
  Result := '';
  Randomize;
  for i := 1 to ALongueur do
    Result := Result + CARACTERES[Random(Length(CARACTERES)) + 1];
end;
```

## Audit des accès aux secrets

```pascal
type
  TAuditSecrets = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure LoggerAccesSecret(const ANomSecret: string; AIDUtilisateur: Integer);
    procedure GenererRapportAcces(const ANomFichier: string);
  end;

constructor TAuditSecrets.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TAuditSecrets.LoggerAccesSecret(const ANomSecret: string; AIDUtilisateur: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsAccesSecrets (NomSecret, IDUtilisateur, DateHeure) ' +
      'VALUES (:Nom, :IDUser, NOW())';
    Query.ParamByName('Nom').AsString := ANomSecret;
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TAuditSecrets.GenererRapportAcces(const ANomFichier: string);
var
  Query: TFDQuery;
  Fichier: TextFile;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT las.DateHeure, u.Username, las.NomSecret ' +
      'FROM LogsAccesSecrets las ' +
      'JOIN Users u ON las.IDUtilisateur = u.ID ' +
      'WHERE las.DateHeure >= DATE_SUB(NOW(), INTERVAL 30 DAY) ' +
      'ORDER BY las.DateHeure DESC';
    Query.Open;

    AssignFile(Fichier, ANomFichier);
    Rewrite(Fichier);
    try
      WriteLn(Fichier, 'RAPPORT D''ACCÈS AUX SECRETS - 30 derniers jours');
      WriteLn(Fichier, StringOfChar('=', 70));
      WriteLn(Fichier, '');

      while not Query.Eof do
      begin
        WriteLn(Fichier, Format('%s | %s | %s',
          [DateTimeToStr(Query.FieldByName('DateHeure').AsDateTime),
           Query.FieldByName('Username').AsString,
           Query.FieldByName('NomSecret').AsString]));
        Query.Next;
      end;
    finally
      CloseFile(Fichier);
    end;
  finally
    Query.Free;
  end;
end;
```

## Bonnes pratiques

### ✅ À faire

**1. Utiliser les API système appropriées**
```pascal
// ✅ Windows → DPAPI
// ✅ macOS → Keychain
// ✅ Linux → Secret Service API
// ✅ Mobile → Stockage sécurisé natif
```

**2. Ne jamais commiter les secrets dans Git**
```
# Ajouter dans .gitignore
config.secure
*.dat
*.enc
.env
secrets/
```

**3. Chiffrer avant de stocker**
```pascal
// ✅ BON
MotDePasseChiffre := TDPAPIHelper.ChiffrerDonnees(MotDePasse);
SauvegarderDansFichier(MotDePasseChiffre);

// ❌ MAUVAIS
SauvegarderDansFichier(MotDePasse); // En clair !
```

**4. Limiter l'accès**
```pascal
// Permissions restrictives sur les fichiers de secrets
// Windows : Uniquement l'utilisateur actuel
// Linux : chmod 600 fichier_secrets.dat
```

**5. Auditer les accès**
```pascal
// Logger chaque fois qu'un secret est lu
procedure ChargerSecret(const ANom: string): string;
begin
  Result := TGestionSecrets.Charger(ANom);
  AuditSecrets.LoggerAcces(ANom, UtilisateurActuel);
end;
```

**6. Rotation régulière**
```pascal
// Changer les mots de passe tous les 90 jours
// Utiliser un système automatisé
```

### ❌ À éviter

**1. Secrets dans le code**
```pascal
// ❌ JAMAIS ça
const API_KEY = 'sk_live_...';
```

**2. Logs avec secrets**
```pascal
// ❌ DANGEREUX
TLogger.Log('Connexion avec password: ' + Password);

// ✅ BON
TLogger.Log('Connexion réussie pour utilisateur: ' + Username);
```

**3. Transmission non chiffrée**
```pascal
// ❌ Envoyer par email
// ❌ Envoyer par messagerie instantanée
// ❌ Mettre dans un document partagé

// ✅ Utiliser un gestionnaire de secrets
```

**4. Un seul secret pour tout**
```pascal
// ❌ MAUVAIS - même mot de passe partout
const MASTER_PASSWORD = 'admin123';

// ✅ BON - secrets différents pour chaque service
DBPassword := TGestionSecrets.Charger('DB_Password');
APIKey := TGestionSecrets.Charger('API_Key');
```

## Checklist de sécurité des identifiants

Avant le déploiement :

### Stockage
- [ ] Aucun secret dans le code source
- [ ] Secrets chiffrés au repos
- [ ] Utilisation des API système (DPAPI, Keychain)
- [ ] Fichiers de secrets avec permissions restrictives
- [ ] Pas de secrets dans le contrôle de version (Git)

### Accès
- [ ] Principe du moindre privilège
- [ ] Audit des accès aux secrets
- [ ] Secrets différents par environnement (dev/prod)
- [ ] Révocation possible en cas de compromission

### Maintenance
- [ ] Rotation régulière des secrets (90 jours)
- [ ] Procédure de révocation documentée
- [ ] Sauvegarde chiffrée des secrets
- [ ] Plan de récupération en cas de perte

### Transmission
- [ ] Jamais par email ou chat
- [ ] Utilisation de canaux sécurisés
- [ ] Chiffrement bout-en-bout
- [ ] Durée de vie limitée des tokens

### Développement
- [ ] Variables d'environnement en développement
- [ ] Secrets factices dans les tests
- [ ] Documentation sans secrets réels
- [ ] Formation de l'équipe

## Résumé des points essentiels

✅ **Solutions recommandées par plateforme** :
- **Windows** : DPAPI (Data Protection API)
- **macOS** : Keychain
- **Linux** : Secret Service API ou chiffrement avec clé dérivée
- **Mobile** : Stockage sécurisé natif (iOS Keychain, Android Keystore)
- **Cloud** : Services de gestion de secrets (AWS Secrets Manager, Azure Key Vault)

❌ **Erreurs fatales à éviter** :
- Secrets en clair dans le code source
- Mots de passe dans les fichiers de configuration non chiffrés
- Commit de secrets dans Git
- Même mot de passe pour tous les services
- Pas de rotation des identifiants
- Transmission de secrets par canaux non sécurisés

🔒 **Règles d'or** :
1. Ne JAMAIS stocker de secrets en clair
2. Utiliser les outils système appropriés
3. Chiffrer avant de sauvegarder
4. Auditer tous les accès
5. Rotation régulière (90 jours max)
6. Séparation dev/prod
7. Formation de l'équipe

## Aller plus loin

**Outils recommandés** :
- **HashiCorp Vault** : Gestion centralisée des secrets
- **Azure Key Vault** : Solution Microsoft Cloud
- **AWS Secrets Manager** : Solution Amazon Cloud
- **1Password CLI** : Pour les équipes de développement
- **Git-secrets** : Prévenir les commits de secrets

**Ressources** :
- OWASP Secrets Management Cheat Sheet
- NIST Guidelines on Password Management
- Documentation DPAPI Microsoft
- Guide de sécurité des API cloud

Le stockage sécurisé des identifiants n'est pas une option, c'est une nécessité absolue. Un seul secret compromis peut donner accès à l'ensemble de votre système. Prenez le temps de le faire correctement dès le début.

⏭️ [GDPR et confidentialité des données](/16-securite-des-applications/08-gdpr-et-confidentialite-des-donnees.md)
