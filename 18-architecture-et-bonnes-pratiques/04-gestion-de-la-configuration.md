🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.4 Gestion de la configuration

## Introduction

Imaginez que vous déménagez dans une nouvelle maison. Vous devez reconfigurer votre connexion Internet, reprogrammer votre alarme, ajuster le thermostat, etc. Si tous ces réglages étaient codés en dur dans les murs, vous devriez démolir et reconstruire à chaque déménagement !

En programmation, c'est pareil : **ne jamais coder en dur** les paramètres qui peuvent changer. La configuration est l'ensemble des paramètres qui peuvent varier selon l'environnement, l'utilisateur ou les besoins, sans avoir à recompiler l'application.

### Qu'est-ce que la configuration ?

La configuration d'une application comprend tous les paramètres qui peuvent changer sans modifier le code source :

**Exemples de paramètres de configuration :**
- Chaîne de connexion à la base de données
- URL d'une API
- Chemins de fichiers et dossiers
- Préférences utilisateur (langue, thème)
- Paramètres de l'application (timeout, taille de cache)
- Clés d'API et identifiants
- Adresses de serveurs

### Pourquoi gérer la configuration ?

Sans gestion de configuration, vous rencontrez ces problèmes :

#### 1. Code en dur = inflexibilité

**Mauvais exemple :**
```pascal
procedure TFormMain.ConnecterBDD;
begin
  FDConnection.Params.Values['Server'] := 'localhost';
  FDConnection.Params.Values['Database'] := 'mabase';
  FDConnection.Params.Values['User_Name'] := 'root';
  FDConnection.Params.Values['Password'] := 'motdepasse123';
  FDConnection.Connected := True;
end;
```

**Problèmes :**
- Pour changer de serveur, il faut recompiler
- Le mot de passe est visible dans le code
- Impossible d'avoir plusieurs environnements (dev, test, production)
- L'utilisateur ne peut pas personnaliser

#### 2. Maintenance difficile

Si les paramètres sont éparpillés dans tout le code, modifier un paramètre devient un cauchemar :
- Où est défini ce chemin ?
- Combien de fois est-il répété ?
- Ai-je modifié toutes les occurrences ?

#### 3. Sécurité compromise

Les mots de passe et clés API dans le code peuvent être :
- Lus par décompilation
- Accidentellement partagés sur Git
- Exposés dans les logs

#### 4. Déploiement complexe

Sans configuration externe, chaque environnement (développement, test, production) nécessite une version différente de l'application.

## Types de configuration

Il existe plusieurs types de configuration selon leur portée et leur usage.

### 1. Configuration d'application

Paramètres globaux qui affectent toute l'application, identiques pour tous les utilisateurs.

**Exemples :**
- URL de l'API backend
- Paramètres de connexion à la base de données
- Chemins de fichiers logs
- Timeout des requêtes réseau
- Taille maximale des fichiers uploadés

**Caractéristiques :**
- Stockée dans un fichier à côté de l'exécutable
- Modifiable par l'administrateur uniquement
- Souvent différente selon l'environnement

### 2. Configuration utilisateur

Préférences personnelles de chaque utilisateur.

**Exemples :**
- Langue de l'interface
- Thème visuel (clair/sombre)
- Position et taille des fenêtres
- Colonnes affichées dans les grilles
- Derniers fichiers ouverts

**Caractéristiques :**
- Stockée dans le profil utilisateur
- Chaque utilisateur a ses propres paramètres
- L'utilisateur peut les modifier via l'interface

### 3. Configuration système

Paramètres liés au système d'exploitation ou à la machine.

**Exemples :**
- Paramètres dans le registre Windows
- Variables d'environnement
- Configurations matérielles

**Caractéristiques :**
- Stockée au niveau système
- Nécessite souvent des droits administrateur
- Partagée entre applications

### 4. Configuration sensible

Informations confidentielles qui doivent être protégées.

**Exemples :**
- Mots de passe de base de données
- Clés d'API
- Certificats
- Tokens d'authentification

**Caractéristiques :**
- Doit être chiffrée
- Jamais dans le code source
- Accès restreint

## Formats de fichiers de configuration

Plusieurs formats sont disponibles pour stocker la configuration. Choisissez selon vos besoins.

### Format INI

Le format INI est simple, lisible et natif à Windows.

**Structure :**
```ini
; Commentaire
[Section1]
Cle1=Valeur1
Cle2=Valeur2

[Section2]
Cle3=Valeur3
```

**Exemple concret :**
```ini
; Configuration de l'application
[Database]
Server=localhost
Port=3306
Database=gestion_clients
UserName=admin
Password=encrypted_password

[Application]
Language=fr
Theme=light
LogLevel=info
MaxConnections=10

[Paths]
DataFolder=C:\Data
LogFolder=C:\Logs
TempFolder=C:\Temp
```

**Utilisation en Delphi :**
```pascal
unit ConfigManager;

interface

uses
  System.IniFiles, System.SysUtils;

type
  TAppConfig = class
  private
    FIniFile: TIniFile;
    FConfigFile: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Lecture
    function GetDatabaseServer: string;
    function GetDatabasePort: Integer;
    function GetLanguage: string;
    function GetTheme: string;

    // Écriture
    procedure SetLanguage(const Value: string);
    procedure SetTheme(const Value: string);

    property DatabaseServer: string read GetDatabaseServer;
    property DatabasePort: Integer read GetDatabasePort;
    property Language: string read GetLanguage write SetLanguage;
    property Theme: string read GetTheme write SetTheme;
  end;

implementation

uses
  System.IOUtils;

constructor TAppConfig.Create;
begin
  inherited;
  // Fichier de config à côté de l'exécutable
  FConfigFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.ini');
  FIniFile := TIniFile.Create(FConfigFile);
end;

destructor TAppConfig.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TAppConfig.GetDatabaseServer: string;
begin
  Result := FIniFile.ReadString('Database', 'Server', 'localhost');
end;

function TAppConfig.GetDatabasePort: Integer;
begin
  Result := FIniFile.ReadInteger('Database', 'Port', 3306);
end;

function TAppConfig.GetLanguage: string;
begin
  Result := FIniFile.ReadString('Application', 'Language', 'fr');
end;

function TAppConfig.GetTheme: string;
begin
  Result := FIniFile.ReadString('Application', 'Theme', 'light');
end;

procedure TAppConfig.SetLanguage(const Value: string);
begin
  FIniFile.WriteString('Application', 'Language', Value);
end;

procedure TAppConfig.SetTheme(const Value: string);
begin
  FIniFile.WriteString('Application', 'Theme', Value);
end;

end.
```

**Avantages du format INI :**
- ✅ Simple et lisible
- ✅ Facilement éditable manuellement
- ✅ Support natif en Delphi
- ✅ Léger

**Inconvénients :**
- ❌ Structure limitée (pas de hiérarchie profonde)
- ❌ Pas de types complexes
- ❌ Pas de validation de schéma

### Format JSON

JSON est moderne, structuré et largement utilisé.

**Exemple :**
```json
{
  "database": {
    "server": "localhost",
    "port": 3306,
    "database": "gestion_clients",
    "username": "admin",
    "password": "encrypted_password",
    "options": {
      "timeout": 30,
      "poolSize": 10
    }
  },
  "application": {
    "language": "fr",
    "theme": "light",
    "features": {
      "enableLogs": true,
      "enableCache": true,
      "cacheSize": 100
    }
  },
  "paths": {
    "data": "C:\\Data",
    "logs": "C:\\Logs",
    "temp": "C:\\Temp"
  },
  "api": {
    "baseUrl": "https://api.example.com",
    "endpoints": {
      "customers": "/customers",
      "orders": "/orders"
    },
    "timeout": 60
  }
}
```

**Utilisation en Delphi :**
```pascal
unit ConfigManager.Json;

interface

uses
  System.JSON, System.SysUtils, System.IOUtils, System.Classes;

type
  TDatabaseConfig = record
    Server: string;
    Port: Integer;
    Database: string;
    Username: string;
    Password: string;
    Timeout: Integer;
  end;

  TAppConfig = class
  private
    FJsonObject: TJSONObject;
    FConfigFile: string;
    procedure LoadConfig;
    procedure SaveConfig;
    function GetDatabaseConfig: TDatabaseConfig;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property DatabaseConfig: TDatabaseConfig read GetDatabaseConfig;
    property Language: string read GetLanguage write SetLanguage;

    // Méthodes utilitaires
    function GetString(const Path: string; const Default: string = ''): string;
    function GetInteger(const Path: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Path: string; const Default: Boolean = False): Boolean;
    procedure SetString(const Path: string; const Value: string);
  end;

implementation

constructor TAppConfig.Create;
begin
  inherited;
  FConfigFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.json');
  LoadConfig;
end;

destructor TAppConfig.Destroy;
begin
  FJsonObject.Free;
  inherited;
end;

procedure TAppConfig.LoadConfig;
var
  JsonString: string;
begin
  if TFile.Exists(FConfigFile) then
  begin
    JsonString := TFile.ReadAllText(FConfigFile, TEncoding.UTF8);
    FJsonObject := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  end
  else
    FJsonObject := TJSONObject.Create;
end;

procedure TAppConfig.SaveConfig;
var
  JsonString: string;
begin
  JsonString := FJsonObject.Format;
  TFile.WriteAllText(FConfigFile, JsonString, TEncoding.UTF8);
end;

function TAppConfig.GetDatabaseConfig: TDatabaseConfig;
var
  DbObject: TJSONObject;
begin
  DbObject := FJsonObject.GetValue<TJSONObject>('database');
  if Assigned(DbObject) then
  begin
    Result.Server := DbObject.GetValue<string>('server', 'localhost');
    Result.Port := DbObject.GetValue<Integer>('port', 3306);
    Result.Database := DbObject.GetValue<string>('database', '');
    Result.Username := DbObject.GetValue<string>('username', '');
    Result.Password := DbObject.GetValue<string>('password', '');
    Result.Timeout := DbObject.GetValue<TJSONObject>('options').GetValue<Integer>('timeout', 30);
  end
  else
  begin
    Result.Server := 'localhost';
    Result.Port := 3306;
    Result.Database := '';
    Result.Username := '';
    Result.Password := '';
    Result.Timeout := 30;
  end;
end;

function TAppConfig.GetLanguage: string;
var
  AppObject: TJSONObject;
begin
  AppObject := FJsonObject.GetValue<TJSONObject>('application');
  if Assigned(AppObject) then
    Result := AppObject.GetValue<string>('language', 'fr')
  else
    Result := 'fr';
end;

procedure TAppConfig.SetLanguage(const Value: string);
var
  AppObject: TJSONObject;
begin
  AppObject := FJsonObject.GetValue<TJSONObject>('application');
  if not Assigned(AppObject) then
  begin
    AppObject := TJSONObject.Create;
    FJsonObject.AddPair('application', AppObject);
  end;

  AppObject.RemovePair('language');
  AppObject.AddPair('language', Value);
  SaveConfig;
end;

function TAppConfig.GetString(const Path: string; const Default: string): string;
var
  Parts: TArray<string>;
  Current: TJSONValue;
  I: Integer;
begin
  Result := Default;
  Parts := Path.Split(['.']);
  Current := FJsonObject;

  for I := 0 to High(Parts) do
  begin
    if Current is TJSONObject then
      Current := TJSONObject(Current).GetValue(Parts[I])
    else
      Exit;

    if not Assigned(Current) then
      Exit;
  end;

  if Current is TJSONString then
    Result := TJSONString(Current).Value;
end;

function TAppConfig.GetInteger(const Path: string; const Default: Integer): Integer;
var
  Value: string;
begin
  Value := GetString(Path);
  if not TryStrToInt(Value, Result) then
    Result := Default;
end;

function TAppConfig.GetBoolean(const Path: string; const Default: Boolean): Boolean;
var
  Value: string;
begin
  Value := GetString(Path).ToLower;
  if (Value = 'true') or (Value = '1') then
    Result := True
  else if (Value = 'false') or (Value = '0') then
    Result := False
  else
    Result := Default;
end;

procedure TAppConfig.SetString(const Path: string; const Value: string);
var
  Parts: TArray<string>;
  Current: TJSONObject;
  I: Integer;
begin
  Parts := Path.Split(['.']);
  Current := FJsonObject;

  for I := 0 to High(Parts) - 1 do
  begin
    if not Assigned(Current.GetValue(Parts[I])) then
      Current.AddPair(Parts[I], TJSONObject.Create);
    Current := Current.GetValue<TJSONObject>(Parts[I]);
  end;

  Current.RemovePair(Parts[High(Parts)]);
  Current.AddPair(Parts[High(Parts)], Value);
  SaveConfig;
end;

end.
```

**Avantages du format JSON :**
- ✅ Structure hiérarchique riche
- ✅ Support des tableaux et objets
- ✅ Standard web (interopérabilité)
- ✅ Validation de schéma possible

**Inconvénients :**
- ❌ Moins lisible pour les non-développeurs
- ❌ Sensible aux erreurs de syntaxe
- ❌ Pas de commentaires (selon les parseurs)

### Format XML

XML est verbeux mais très structuré.

**Exemple :**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
  <database>
    <server>localhost</server>
    <port>3306</port>
    <database>gestion_clients</database>
    <username>admin</username>
    <password>encrypted_password</password>
  </database>

  <application>
    <language>fr</language>
    <theme>light</theme>
    <features>
      <enableLogs>true</enableLogs>
      <enableCache>true</enableCache>
    </features>
  </application>

  <paths>
    <data>C:\Data</data>
    <logs>C:\Logs</logs>
    <temp>C:\Temp</temp>
  </paths>
</configuration>
```

**Avantages :**
- ✅ Validation stricte avec schémas (XSD)
- ✅ Support des attributs et métadonnées
- ✅ Standard bien établi

**Inconvénients :**
- ❌ Très verbeux
- ❌ Plus complexe à parser
- ❌ Lourd pour des configurations simples

### Le Registre Windows

Le registre est une base de données système Windows.

**Utilisation en Delphi :**
```pascal
unit ConfigManager.Registry;

interface

uses
  System.Win.Registry, Winapi.Windows;

type
  TAppConfig = class
  private
    FRegistry: TRegistry;
    const APP_KEY = 'Software\MaSociete\MonApplication';
  public
    constructor Create;
    destructor Destroy; override;

    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetWindowPosition(out Left, Top, Width, Height: Integer): Boolean;
    procedure SetWindowPosition(Left, Top, Width, Height: Integer);
  end;

implementation

constructor TAppConfig.Create;
begin
  inherited;
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;
end;

destructor TAppConfig.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TAppConfig.GetLanguage: string;
begin
  Result := 'fr'; // Valeur par défaut

  if FRegistry.OpenKeyReadOnly(APP_KEY) then
  try
    if FRegistry.ValueExists('Language') then
      Result := FRegistry.ReadString('Language');
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TAppConfig.SetLanguage(const Value: string);
begin
  if FRegistry.OpenKey(APP_KEY, True) then
  try
    FRegistry.WriteString('Language', Value);
  finally
    FRegistry.CloseKey;
  end;
end;

function TAppConfig.GetWindowPosition(out Left, Top, Width, Height: Integer): Boolean;
begin
  Result := False;

  if FRegistry.OpenKeyReadOnly(APP_KEY + '\Window') then
  try
    if FRegistry.ValueExists('Left') and
       FRegistry.ValueExists('Top') and
       FRegistry.ValueExists('Width') and
       FRegistry.ValueExists('Height') then
    begin
      Left := FRegistry.ReadInteger('Left');
      Top := FRegistry.ReadInteger('Top');
      Width := FRegistry.ReadInteger('Width');
      Height := FRegistry.ReadInteger('Height');
      Result := True;
    end;
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TAppConfig.SetWindowPosition(Left, Top, Width, Height: Integer);
begin
  if FRegistry.OpenKey(APP_KEY + '\Window', True) then
  try
    FRegistry.WriteInteger('Left', Left);
    FRegistry.WriteInteger('Top', Top);
    FRegistry.WriteInteger('Width', Width);
    FRegistry.WriteInteger('Height', Height);
  finally
    FRegistry.CloseKey;
  end;
end;

end.
```

**Avantages du registre :**
- ✅ Natif Windows
- ✅ Permissions système intégrées
- ✅ Centralisé

**Inconvénients :**
- ❌ Spécifique à Windows
- ❌ Difficile à déployer/migrer
- ❌ Peut nécessiter des droits admin

## Où stocker les fichiers de configuration ?

Le choix de l'emplacement dépend du type de configuration.

### 1. À côté de l'exécutable

Pour la configuration d'application partagée par tous les utilisateurs.

```pascal
function GetAppConfigPath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.ini');
end;
```

**Avantages :**
- Simple
- Pas besoin de droits particuliers
- Facile à trouver et éditer

**Inconvénients :**
- Peut nécessiter des droits admin dans Program Files
- Non adapté pour les configs utilisateur

### 2. Dans le dossier de données de l'application

Windows : `C:\ProgramData\MonApplication\`

```pascal
function GetAppDataConfigPath: string;
begin
  Result := TPath.Combine(
    TPath.GetPublicPath,  // C:\ProgramData
    'MonApplication',
    'config.ini'
  );
end;
```

**Avantages :**
- Emplacement standard Windows
- Accessible à tous les utilisateurs
- Survit aux désinstallations

**Inconvénients :**
- Peut nécessiter des droits admin pour créer

### 3. Dans le dossier utilisateur

Windows : `C:\Users\[Utilisateur]\AppData\Roaming\MonApplication\`

```pascal
function GetUserConfigPath: string;
begin
  Result := TPath.Combine(
    TPath.GetHomePath,  // C:\Users\[User]
    'AppData',
    'Roaming',
    'MonApplication',
    'user.ini'
  );
end;
```

**Avantages :**
- Configuration par utilisateur
- Pas besoin de droits admin
- Roaming possible (profils itinérants)

**Inconvénients :**
- Un fichier par utilisateur

### 4. Dans le dossier local utilisateur

Windows : `C:\Users\[Utilisateur]\AppData\Local\MonApplication\`

```pascal
function GetLocalConfigPath: string;
begin
  Result := TPath.Combine(
    TPath.GetHomePath,
    'AppData',
    'Local',
    'MonApplication',
    'cache.ini'
  );
end;
```

**Utilisation :** Pour les données temporaires ou spécifiques à la machine (pas de roaming).

### Classe utilitaire pour les chemins

```pascal
unit AppPaths;

interface

uses
  System.SysUtils, System.IOUtils;

type
  TAppPaths = class
  private
    class var FAppName: string;
  public
    class constructor Create;

    class function GetAppConfigFile: string;
    class function GetUserConfigFile: string;
    class function GetDataFolder: string;
    class function GetLogFolder: string;
    class function GetTempFolder: string;

    class procedure EnsureFolderExists(const Path: string);

    class property AppName: string read FAppName write FAppName;
  end;

implementation

class constructor TAppPaths.Create;
begin
  FAppName := 'MonApplication'; // Peut être configuré
end;

class function TAppPaths.GetAppConfigFile: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.ini');
end;

class function TAppPaths.GetUserConfigFile: string;
begin
  Result := TPath.Combine(
    TPath.GetHomePath,
    'AppData',
    'Roaming',
    FAppName,
    'user.ini'
  );
  EnsureFolderExists(TPath.GetDirectoryName(Result));
end;

class function TAppPaths.GetDataFolder: string;
begin
  Result := TPath.Combine(
    TPath.GetPublicPath,
    FAppName,
    'Data'
  );
  EnsureFolderExists(Result);
end;

class function TAppPaths.GetLogFolder: string;
begin
  Result := TPath.Combine(
    TPath.GetHomePath,
    'AppData',
    'Local',
    FAppName,
    'Logs'
  );
  EnsureFolderExists(Result);
end;

class function TAppPaths.GetTempFolder: string;
begin
  Result := TPath.Combine(
    TPath.GetTempPath,
    FAppName
  );
  EnsureFolderExists(Result);
end;

class procedure TAppPaths.EnsureFolderExists(const Path: string);
begin
  if not TDirectory.Exists(Path) then
    TDirectory.CreateDirectory(Path);
end;

end.
```

## Gestion des environnements

Une application professionnelle doit fonctionner dans plusieurs environnements : développement, test et production. Chaque environnement a ses propres paramètres.

### Approche 1 : Fichiers de configuration séparés

Créez un fichier par environnement :
- `config.dev.ini` - Développement
- `config.test.ini` - Tests
- `config.prod.ini` - Production

```pascal
unit EnvironmentConfig;

interface

type
  TEnvironment = (envDevelopment, envTesting, envProduction);

  TEnvironmentConfig = class
  private
    FEnvironment: TEnvironment;
    FConfigFile: string;
    procedure DetermineEnvironment;
    function GetConfigFileName: string;
  public
    constructor Create;
    property Environment: TEnvironment read FEnvironment;
    property ConfigFile: string read FConfigFile;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

constructor TEnvironmentConfig.Create;
begin
  inherited;
  DetermineEnvironment;
  FConfigFile := GetConfigFileName;
end;

procedure TEnvironmentConfig.DetermineEnvironment;
var
  EnvVar: string;
begin
  // Méthode 1 : Variable d'environnement
  EnvVar := GetEnvironmentVariable('APP_ENV');

  if EnvVar = 'production' then
    FEnvironment := envProduction
  else if EnvVar = 'testing' then
    FEnvironment := envTesting
  else
  begin
    // Méthode 2 : Fichier indicateur
    if TFile.Exists(TPath.Combine(ExtractFilePath(ParamStr(0)), '.production')) then
      FEnvironment := envProduction
    else if TFile.Exists(TPath.Combine(ExtractFilePath(ParamStr(0)), '.testing')) then
      FEnvironment := envTesting
    else
      FEnvironment := envDevelopment; // Par défaut
  end;
end;

function TEnvironmentConfig.GetConfigFileName: string;
var
  BasePath: string;
begin
  BasePath := ExtractFilePath(ParamStr(0));

  case FEnvironment of
    envDevelopment: Result := TPath.Combine(BasePath, 'config.dev.ini');
    envTesting:     Result := TPath.Combine(BasePath, 'config.test.ini');
    envProduction:  Result := TPath.Combine(BasePath, 'config.prod.ini');
  end;
end;

end.
```

**Utilisation :**
```pascal
var
  EnvConfig: TEnvironmentConfig;
  Config: TAppConfig;
begin
  EnvConfig := TEnvironmentConfig.Create;
  try
    Config := TAppConfig.Create(EnvConfig.ConfigFile);
    try
      // Utiliser Config...
    finally
      Config.Free;
    end;
  finally
    EnvConfig.Free;
  end;
end;
```

### Approche 2 : Sections d'environnement

Un seul fichier avec des sections par environnement :

```ini
[Development.Database]
Server=localhost
Database=test_db

[Testing.Database]
Server=test-server
Database=test_db

[Production.Database]
Server=prod-server.company.com
Database=production_db
```

```pascal
function GetConfigSection(const BaseSection: string): string;
var
  Env: string;
begin
  Env := GetEnvironmentVariable('APP_ENV');
  if Env = '' then
    Env := 'Development';

  Result := Env + '.' + BaseSection;
end;

// Utilisation
Server := IniFile.ReadString(GetConfigSection('Database'), 'Server', 'localhost');
```

### Approche 3 : Configuration hiérarchique

Un fichier de base + un fichier d'override par environnement :

1. `config.default.json` - Configuration par défaut
2. `config.local.json` - Override local (ignoré par Git)

```pascal
unit HierarchicalConfig;

interface

uses
  System.JSON, System.SysUtils, System.IOUtils;

type
  THierarchicalConfig = class
  private
    FDefaultConfig: TJSONObject;
    FLocalConfig: TJSONObject;
    procedure LoadConfigs;
    function GetValue(const Path: string; const Default: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetString(const Path: string; const Default: string = ''): string;
    function GetInteger(const Path: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Path: string; const Default: Boolean = False): Boolean;
  end;

implementation

constructor THierarchicalConfig.Create;
begin
  inherited;
  LoadConfigs;
end;

destructor THierarchicalConfig.Destroy;
begin
  FDefaultConfig.Free;
  FLocalConfig.Free;
  inherited;
end;

procedure THierarchicalConfig.LoadConfigs;
var
  DefaultFile, LocalFile: string;
  JsonString: string;
begin
  DefaultFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.default.json');
  LocalFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.local.json');

  // Charger config par défaut
  if TFile.Exists(DefaultFile) then
  begin
    JsonString := TFile.ReadAllText(DefaultFile, TEncoding.UTF8);
    FDefaultConfig := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  end
  else
    FDefaultConfig := TJSONObject.Create;

  // Charger config locale (override)
  if TFile.Exists(LocalFile) then
  begin
    JsonString := TFile.ReadAllText(LocalFile, TEncoding.UTF8);
    FLocalConfig := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  end
  else
    FLocalConfig := TJSONObject.Create;
end;

function THierarchicalConfig.GetValue(const Path: string; const Default: string): string;
var
  Parts: TArray<string>;
  Current: TJSONValue;
  I: Integer;
begin
  Result := Default;
  Parts := Path.Split(['.']);

  // Chercher d'abord dans la config locale
  Current := FLocalConfig;
  for I := 0 to High(Parts) do
  begin
    if Current is TJSONObject then
      Current := TJSONObject(Current).GetValue(Parts[I])
    else
      Break;

    if not Assigned(Current) then
      Break;
  end;

  if Assigned(Current) and (Current is TJSONString) then
  begin
    Result := TJSONString(Current).Value;
    Exit; // Trouvé dans local
  end;

  // Si pas trouvé, chercher dans default
  Current := FDefaultConfig;
  for I := 0 to High(Parts) do
  begin
    if Current is TJSONObject then
      Current := TJSONObject(Current).GetValue(Parts[I])
    else
      Exit;

    if not Assigned(Current) then
      Exit;
  end;

  if Assigned(Current) and (Current is TJSONString) then
    Result := TJSONString(Current).Value;
end;

function THierarchicalConfig.GetString(const Path: string; const Default: string): string;
begin
  Result := GetValue(Path, Default);
end;

function THierarchicalConfig.GetInteger(const Path: string; const Default: Integer): Integer;
var
  Value: string;
begin
  Value := GetValue(Path, IntToStr(Default));
  if not TryStrToInt(Value, Result) then
    Result := Default;
end;

function THierarchicalConfig.GetBoolean(const Path: string; const Default: Boolean): Boolean;
var
  Value: string;
begin
  Value := GetValue(Path, BoolToStr(Default, True)).ToLower;
  if (Value = 'true') or (Value = '1') then
    Result := True
  else if (Value = 'false') or (Value = '0') then
    Result := False
  else
    Result := Default;
end;

end.
```

## Sécurisation des informations sensibles

Ne JAMAIS stocker en clair les mots de passe et clés sensibles.

### Technique 1 : Chiffrement simple

```pascal
unit ConfigSecurity;

interface

uses
  System.SysUtils, System.NetEncoding;

type
  TConfigSecurity = class
  public
    class function Encrypt(const PlainText: string): string;
    class function Decrypt(const CipherText: string): string;
  end;

implementation

uses
  System.Hash;

class function TConfigSecurity.Encrypt(const PlainText: string): string;
var
  Bytes: TBytes;
  Key: Byte;
  I: Integer;
begin
  // ATTENTION : Ceci est une encryption très simple pour l'exemple
  // En production, utilisez des méthodes cryptographiques robustes

  Key := 42; // Clé de chiffrement (à complexifier en production)
  Bytes := TEncoding.UTF8.GetBytes(PlainText);

  for I := 0 to High(Bytes) do
    Bytes[I] := Bytes[I] xor Key;

  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
end;

class function TConfigSecurity.Decrypt(const CipherText: string): string;
var
  Bytes: TBytes;
  Key: Byte;
  I: Integer;
begin
  Key := 42;
  Bytes := TNetEncoding.Base64.DecodeStringToBytes(CipherText);

  for I := 0 to High(Bytes) do
    Bytes[I] := Bytes[I] xor Key;

  Result := TEncoding.UTF8.GetString(Bytes);
end;

end.
```

**Utilisation :**
```pascal
// Lors de l'enregistrement
EncryptedPassword := TConfigSecurity.Encrypt('motdepasse123');
IniFile.WriteString('Database', 'Password', EncryptedPassword);

// Lors de la lecture
EncryptedPassword := IniFile.ReadString('Database', 'Password', '');
Password := TConfigSecurity.Decrypt(EncryptedPassword);
```

### Technique 2 : Utiliser Windows DPAPI

DPAPI (Data Protection API) chiffre les données spécifiquement pour l'utilisateur et la machine.

```pascal
unit ConfigSecurity.DPAPI;

interface

uses
  Winapi.Windows, System.SysUtils;

type
  TDPAPIConfig = class
  public
    class function ProtectString(const PlainText: string): TBytes;
    class function UnprotectString(const ProtectedData: TBytes): string;
  end;

implementation

uses
  System.NetEncoding;

class function TDPAPIConfig.ProtectString(const PlainText: string): TBytes;
var
  DataIn, DataOut: DATA_BLOB;
  PlainBytes: TBytes;
begin
  PlainBytes := TEncoding.UTF8.GetBytes(PlainText);

  DataIn.cbData := Length(PlainBytes);
  DataIn.pbData := @PlainBytes[0];

  if CryptProtectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[0], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Erreur de chiffrement');
end;

class function TDPAPIConfig.UnprotectString(const ProtectedData: TBytes): string;
var
  DataIn, DataOut: DATA_BLOB;
  ResultBytes: TBytes;
begin
  DataIn.cbData := Length(ProtectedData);
  DataIn.pbData := @ProtectedData[0];

  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(ResultBytes, DataOut.cbData);
    Move(DataOut.pbData^, ResultBytes[0], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
    Result := TEncoding.UTF8.GetString(ResultBytes);
  end
  else
    raise Exception.Create('Erreur de déchiffrement');
end;

end.
```

### Technique 3 : Variables d'environnement

Pour les secrets en production, utilisez des variables d'environnement plutôt que des fichiers :

```pascal
function GetDatabasePassword: string;
begin
  // D'abord chercher dans les variables d'environnement
  Result := GetEnvironmentVariable('DB_PASSWORD');

  // Si pas trouvé, utiliser le fichier de config
  if Result = '' then
    Result := Config.GetString('database.password');
end;
```

### Bonnes pratiques de sécurité

1. **Ne jamais commiter les secrets sur Git**
   - Ajoutez `config.local.ini` et `config.prod.ini` au `.gitignore`
   - Utilisez des valeurs par défaut non sensibles dans `config.default.ini`

2. **Chiffrer les mots de passe**
   - Utilisez DPAPI pour Windows
   - Utilisez des bibliothèques cryptographiques robustes

3. **Séparer les secrets**
   - Un fichier séparé pour les informations sensibles
   - Permissions système restrictives

4. **Documentation**
   - Créez un fichier `config.template.ini` avec des exemples
   - Documentez quelles valeurs doivent être changées

```ini
; config.template.ini - Copiez ce fichier en config.local.ini et configurez vos valeurs

[Database]
Server=localhost
Port=3306
Database=votre_base
UserName=votre_utilisateur
Password=CHANGEZ_MOI
```

## Classe de configuration centralisée

Créons une classe complète pour gérer toute la configuration de l'application.

```pascal
unit AppConfiguration;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils, System.Generics.Collections;

type
  TDatabaseConfig = record
    Server: string;
    Port: Integer;
    Database: string;
    Username: string;
    Password: string;
    function GetConnectionString: string;
  end;

  TApplicationConfig = record
    Language: string;
    Theme: string;
    LogLevel: string;
    MaxConnections: Integer;
  end;

  TPathsConfig = record
    DataFolder: string;
    LogFolder: string;
    TempFolder: string;
    procedure EnsureFoldersExist;
  end;

  TAppConfiguration = class
  private
    FIniFile: TIniFile;
    FConfigFile: string;
    FDatabase: TDatabaseConfig;
    FApplication: TApplicationConfig;
    FPaths: TPathsConfig;

    procedure LoadConfiguration;
    procedure LoadDatabaseConfig;
    procedure LoadApplicationConfig;
    procedure LoadPathsConfig;
    procedure CreateDefaultConfig;
  public
    constructor Create; overload;
    constructor Create(const ConfigFile: string); overload;
    destructor Destroy; override;

    procedure Reload;
    procedure Save;

    property Database: TDatabaseConfig read FDatabase write FDatabase;
    property Application: TApplicationConfig read FApplication write FApplication;
    property Paths: TPathsConfig read FPaths write FPaths;

    property ConfigFile: string read FConfigFile;
  end;

var
  AppConfig: TAppConfiguration;

implementation

uses
  ConfigSecurity;

{ TDatabaseConfig }

function TDatabaseConfig.GetConnectionString: string;
begin
  Result := Format('Server=%s;Port=%d;Database=%s;User=%s;Password=%s',
    [Server, Port, Database, Username, Password]);
end;

{ TPathsConfig }

procedure TPathsConfig.EnsureFoldersExist;
begin
  if not TDirectory.Exists(DataFolder) then
    TDirectory.CreateDirectory(DataFolder);
  if not TDirectory.Exists(LogFolder) then
    TDirectory.CreateDirectory(LogFolder);
  if not TDirectory.Exists(TempFolder) then
    TDirectory.CreateDirectory(TempFolder);
end;

{ TAppConfiguration }

constructor TAppConfiguration.Create;
begin
  Create(TPath.Combine(ExtractFilePath(ParamStr(0)), 'config.ini'));
end;

constructor TAppConfiguration.Create(const ConfigFile: string);
begin
  inherited Create;
  FConfigFile := ConfigFile;

  if not TFile.Exists(FConfigFile) then
    CreateDefaultConfig;

  FIniFile := TIniFile.Create(FConfigFile);
  LoadConfiguration;
end;

destructor TAppConfiguration.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TAppConfiguration.CreateDefaultConfig;
var
  DefaultIni: TIniFile;
begin
  DefaultIni := TIniFile.Create(FConfigFile);
  try
    // Base de données
    DefaultIni.WriteString('Database', 'Server', 'localhost');
    DefaultIni.WriteInteger('Database', 'Port', 3306);
    DefaultIni.WriteString('Database', 'Database', 'myapp');
    DefaultIni.WriteString('Database', 'Username', 'root');
    DefaultIni.WriteString('Database', 'Password', TConfigSecurity.Encrypt(''));

    // Application
    DefaultIni.WriteString('Application', 'Language', 'fr');
    DefaultIni.WriteString('Application', 'Theme', 'light');
    DefaultIni.WriteString('Application', 'LogLevel', 'info');
    DefaultIni.WriteInteger('Application', 'MaxConnections', 10);

    // Chemins
    DefaultIni.WriteString('Paths', 'DataFolder',
      TPath.Combine(ExtractFilePath(ParamStr(0)), 'Data'));
    DefaultIni.WriteString('Paths', 'LogFolder',
      TPath.Combine(ExtractFilePath(ParamStr(0)), 'Logs'));
    DefaultIni.WriteString('Paths', 'TempFolder',
      TPath.Combine(TPath.GetTempPath, 'MyApp'));
  finally
    DefaultIni.Free;
  end;
end;

procedure TAppConfiguration.LoadConfiguration;
begin
  LoadDatabaseConfig;
  LoadApplicationConfig;
  LoadPathsConfig;
  FPaths.EnsureFoldersExist;
end;

procedure TAppConfiguration.LoadDatabaseConfig;
var
  EncryptedPassword: string;
begin
  FDatabase.Server := FIniFile.ReadString('Database', 'Server', 'localhost');
  FDatabase.Port := FIniFile.ReadInteger('Database', 'Port', 3306);
  FDatabase.Database := FIniFile.ReadString('Database', 'Database', '');
  FDatabase.Username := FIniFile.ReadString('Database', 'Username', '');

  EncryptedPassword := FIniFile.ReadString('Database', 'Password', '');
  if EncryptedPassword <> '' then
    FDatabase.Password := TConfigSecurity.Decrypt(EncryptedPassword)
  else
    FDatabase.Password := '';
end;

procedure TAppConfiguration.LoadApplicationConfig;
begin
  FApplication.Language := FIniFile.ReadString('Application', 'Language', 'fr');
  FApplication.Theme := FIniFile.ReadString('Application', 'Theme', 'light');
  FApplication.LogLevel := FIniFile.ReadString('Application', 'LogLevel', 'info');
  FApplication.MaxConnections := FIniFile.ReadInteger('Application', 'MaxConnections', 10);
end;

procedure TAppConfiguration.LoadPathsConfig;
begin
  FPaths.DataFolder := FIniFile.ReadString('Paths', 'DataFolder',
    TPath.Combine(ExtractFilePath(ParamStr(0)), 'Data'));
  FPaths.LogFolder := FIniFile.ReadString('Paths', 'LogFolder',
    TPath.Combine(ExtractFilePath(ParamStr(0)), 'Logs'));
  FPaths.TempFolder := FIniFile.ReadString('Paths', 'TempFolder',
    TPath.Combine(TPath.GetTempPath, 'MyApp'));
end;

procedure TAppConfiguration.Reload;
begin
  FIniFile.Free;
  FIniFile := TIniFile.Create(FConfigFile);
  LoadConfiguration;
end;

procedure TAppConfiguration.Save;
begin
  // Sauvegarder la base de données
  FIniFile.WriteString('Database', 'Server', FDatabase.Server);
  FIniFile.WriteInteger('Database', 'Port', FDatabase.Port);
  FIniFile.WriteString('Database', 'Database', FDatabase.Database);
  FIniFile.WriteString('Database', 'Username', FDatabase.Username);
  FIniFile.WriteString('Database', 'Password',
    TConfigSecurity.Encrypt(FDatabase.Password));

  // Sauvegarder l'application
  FIniFile.WriteString('Application', 'Language', FApplication.Language);
  FIniFile.WriteString('Application', 'Theme', FApplication.Theme);
  FIniFile.WriteString('Application', 'LogLevel', FApplication.LogLevel);
  FIniFile.WriteInteger('Application', 'MaxConnections', FApplication.MaxConnections);

  // Sauvegarder les chemins
  FIniFile.WriteString('Paths', 'DataFolder', FPaths.DataFolder);
  FIniFile.WriteString('Paths', 'LogFolder', FPaths.LogFolder);
  FIniFile.WriteString('Paths', 'TempFolder', FPaths.TempFolder);
end;

initialization
  AppConfig := TAppConfiguration.Create;

finalization
  AppConfig.Free;

end.
```

**Utilisation dans toute l'application :**

```pascal
uses
  AppConfiguration;

// Connexion à la base de données
procedure TDataModule.ConnecterBDD;
begin
  FDConnection.Params.Values['Server'] := AppConfig.Database.Server;
  FDConnection.Params.Values['Port'] := IntToStr(AppConfig.Database.Port);
  FDConnection.Params.Values['Database'] := AppConfig.Database.Database;
  FDConnection.Params.Values['User_Name'] := AppConfig.Database.Username;
  FDConnection.Params.Values['Password'] := AppConfig.Database.Password;
  FDConnection.Connected := True;
end;

// Changement de langue
procedure TFormMain.MenuFrancaisClick(Sender: TObject);
begin
  AppConfig.Application.Language := 'fr';
  AppConfig.Save;
  // Recharger l'interface...
end;

// Écriture de logs
procedure WriteLog(const Message: string);
var
  LogFile: string;
begin
  LogFile := TPath.Combine(AppConfig.Paths.LogFolder,
    FormatDateTime('yyyy-mm-dd', Now) + '.log');
  // Écrire dans le fichier...
end;
```

## Configuration utilisateur

Pour gérer les préférences utilisateur séparément.

```pascal
unit UserPreferences;

interface

uses
  System.IniFiles, System.SysUtils, System.IOUtils;

type
  TUserPreferences = class
  private
    FIniFile: TIniFile;
    FConfigFile: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Préférences d'interface
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetTheme: string;
    procedure SetTheme(const Value: string);

    // Position des fenêtres
    procedure SaveWindowPosition(const FormName: string; Left, Top, Width, Height: Integer);
    function LoadWindowPosition(const FormName: string;
      out Left, Top, Width, Height: Integer): Boolean;

    // Colonnes de grilles
    procedure SaveGridColumns(const GridName: string; const Columns: TArray<Integer>);
    function LoadGridColumns(const GridName: string): TArray<Integer>;

    // Derniers fichiers ouverts
    procedure AddRecentFile(const FileName: string);
    function GetRecentFiles: TArray<string>;
  end;

var
  UserPrefs: TUserPreferences;

implementation

constructor TUserPreferences.Create;
begin
  inherited;
  FConfigFile := TPath.Combine(
    TPath.GetHomePath,
    'AppData',
    'Roaming',
    'MonApplication',
    'user.ini'
  );

  // Créer le dossier si nécessaire
  ForceDirectories(TPath.GetDirectoryName(FConfigFile));

  FIniFile := TIniFile.Create(FConfigFile);
end;

destructor TUserPreferences.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TUserPreferences.GetLanguage: string;
begin
  Result := FIniFile.ReadString('UI', 'Language', 'fr');
end;

procedure TUserPreferences.SetLanguage(const Value: string);
begin
  FIniFile.WriteString('UI', 'Language', Value);
end;

function TUserPreferences.GetTheme: string;
begin
  Result := FIniFile.ReadString('UI', 'Theme', 'light');
end;

procedure TUserPreferences.SetTheme(const Value: string);
begin
  FIniFile.WriteString('UI', 'Theme', Value);
end;

procedure TUserPreferences.SaveWindowPosition(const FormName: string;
  Left, Top, Width, Height: Integer);
var
  Section: string;
begin
  Section := 'Window.' + FormName;
  FIniFile.WriteInteger(Section, 'Left', Left);
  FIniFile.WriteInteger(Section, 'Top', Top);
  FIniFile.WriteInteger(Section, 'Width', Width);
  FIniFile.WriteInteger(Section, 'Height', Height);
end;

function TUserPreferences.LoadWindowPosition(const FormName: string;
  out Left, Top, Width, Height: Integer): Boolean;
var
  Section: string;
begin
  Section := 'Window.' + FormName;
  Result := FIniFile.SectionExists(Section);

  if Result then
  begin
    Left := FIniFile.ReadInteger(Section, 'Left', 100);
    Top := FIniFile.ReadInteger(Section, 'Top', 100);
    Width := FIniFile.ReadInteger(Section, 'Width', 800);
    Height := FIniFile.ReadInteger(Section, 'Height', 600);
  end;
end;

procedure TUserPreferences.SaveGridColumns(const GridName: string; const Columns: TArray<Integer>);
var
  I: Integer;
  Section: string;
begin
  Section := 'Grid.' + GridName;
  FIniFile.EraseSection(Section);

  for I := 0 to High(Columns) do
    FIniFile.WriteInteger(Section, 'Col' + IntToStr(I), Columns[I]);
end;

function TUserPreferences.LoadGridColumns(const GridName: string): TArray<Integer>;
var
  Section: string;
  Keys: TStringList;
  I: Integer;
begin
  Section := 'Grid.' + GridName;
  Keys := TStringList.Create;
  try
    FIniFile.ReadSection(Section, Keys);
    SetLength(Result, Keys.Count);

    for I := 0 to Keys.Count - 1 do
      Result[I] := FIniFile.ReadInteger(Section, Keys[I], 100);
  finally
    Keys.Free;
  end;
end;

procedure TUserPreferences.AddRecentFile(const FileName: string);
var
  RecentFiles: TArray<string>;
  I, Count: Integer;
begin
  // Charger les fichiers existants
  RecentFiles := GetRecentFiles;

  // Supprimer si déjà présent
  for I := High(RecentFiles) downto 0 do
  begin
    if SameText(RecentFiles[I], FileName) then
    begin
      Delete(RecentFiles, I, 1);
      Break;
    end;
  end;

  // Ajouter en première position
  SetLength(RecentFiles, Length(RecentFiles) + 1);
  for I := High(RecentFiles) downto 1 do
    RecentFiles[I] := RecentFiles[I - 1];
  RecentFiles[0] := FileName;

  // Limiter à 10 fichiers
  if Length(RecentFiles) > 10 then
    SetLength(RecentFiles, 10);

  // Sauvegarder
  FIniFile.EraseSection('RecentFiles');
  for I := 0 to High(RecentFiles) do
    FIniFile.WriteString('RecentFiles', 'File' + IntToStr(I), RecentFiles[I]);
end;

function TUserPreferences.GetRecentFiles: TArray<string>;
var
  I: Integer;
  FileName: string;
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    I := 0;
    while True do
    begin
      FileName := FIniFile.ReadString('RecentFiles', 'File' + IntToStr(I), '');
      if FileName = '' then
        Break;
      if TFile.Exists(FileName) then
        Files.Add(FileName);
      Inc(I);
    end;

    Result := Files.ToStringArray;
  finally
    Files.Free;
  end;
end;

initialization
  UserPrefs := TUserPreferences.Create;

finalization
  UserPrefs.Free;

end.
```

**Utilisation :**

```pascal
// Sauvegarder la position de la fenêtre
procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UserPrefs.SaveWindowPosition('Main', Left, Top, Width, Height);
end;

// Restaurer la position
procedure TFormMain.FormCreate(Sender: TObject);
var
  L, T, W, H: Integer;
begin
  if UserPrefs.LoadWindowPosition('Main', L, T, W, H) then
  begin
    Left := L;
    Top := T;
    Width := W;
    Height := H;
  end;
end;

// Ajouter un fichier récent
procedure TFormMain.OpenFile(const FileName: string);
begin
  // Ouvrir le fichier...
  UserPrefs.AddRecentFile(FileName);
  UpdateRecentFilesMenu;
end;
```

## Validation de la configuration

Validez toujours la configuration au démarrage.

```pascal
unit ConfigValidator;

interface

uses
  System.SysUtils, System.IOUtils, AppConfiguration;

type
  TConfigValidator = class
  public
    class function Validate(Config: TAppConfiguration; out Errors: TArray<string>): Boolean;
  private
    class function ValidateDatabase(const DBConfig: TDatabaseConfig; var Errors: TArray<string>): Boolean;
    class function ValidatePaths(const PathsConfig: TPathsConfig; var Errors: TArray<string>): Boolean;
    class procedure AddError(var Errors: TArray<string>; const Error: string);
  end;

implementation

class procedure TConfigValidator.AddError(var Errors: TArray<string>; const Error: string);
begin
  SetLength(Errors, Length(Errors) + 1);
  Errors[High(Errors)] := Error;
end;

class function TConfigValidator.Validate(Config: TAppConfiguration;
  out Errors: TArray<string>): Boolean;
begin
  SetLength(Errors, 0);

  ValidateDatabase(Config.Database, Errors);
  ValidatePaths(Config.Paths, Errors);

  Result := Length(Errors) = 0;
end;

class function TConfigValidator.ValidateDatabase(const DBConfig: TDatabaseConfig;
  var Errors: TArray<string>): Boolean;
begin
  Result := True;

  if DBConfig.Server = '' then
  begin
    AddError(Errors, 'Le serveur de base de données n''est pas configuré');
    Result := False;
  end;

  if DBConfig.Database = '' then
  begin
    AddError(Errors, 'Le nom de la base de données n''est pas configuré');
    Result := False;
  end;

  if (DBConfig.Port < 1) or (DBConfig.Port > 65535) then
  begin
    AddError(Errors, Format('Le port %d est invalide', [DBConfig.Port]));
    Result := False;
  end;
end;

class function TConfigValidator.ValidatePaths(const PathsConfig: TPathsConfig;
  var Errors: TArray<string>): Boolean;
begin
  Result := True;

  if not TDirectory.Exists(PathsConfig.DataFolder) then
  begin
    AddError(Errors, Format('Le dossier de données n''existe pas : %s', [PathsConfig.DataFolder]));
    Result := False;
  end;

  if not TDirectory.Exists(PathsConfig.LogFolder) then
  begin
    AddError(Errors, Format('Le dossier de logs n''existe pas : %s', [PathsConfig.LogFolder]));
    Result := False;
  end;
end;

end.
```

**Utilisation au démarrage :**

```pascal
program MonApplication;

uses
  Vcl.Forms,
  Vcl.Dialogs,
  AppConfiguration,
  ConfigValidator,
  MainForm;

var
  Errors: TArray<string>;
  Error: string;
  ErrorMessage: string;

begin
  Application.Initialize;

  // Valider la configuration
  if not TConfigValidator.Validate(AppConfig, Errors) then
  begin
    ErrorMessage := 'Erreurs de configuration :' + sLineBreak + sLineBreak;
    for Error in Errors do
      ErrorMessage := ErrorMessage + '- ' + Error + sLineBreak;

    ShowMessage(ErrorMessage);
    Exit;
  end;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

## Bonnes pratiques

### 1. Valeurs par défaut sensées

Toujours fournir des valeurs par défaut raisonnables :

```pascal
Language := Config.GetString('Application.Language', 'fr'); // Défaut : français
Timeout := Config.GetInteger('Network.Timeout', 30); // Défaut : 30 secondes
```

### 2. Documentation

Créez un fichier template avec des commentaires :

```ini
; config.template.ini
; Copiez ce fichier en config.ini et configurez vos valeurs

[Database]
; Adresse du serveur MySQL (localhost pour un serveur local)
Server=localhost

; Port MySQL (3306 par défaut)
Port=3306

; Nom de votre base de données
Database=votre_base

; Identifiant de connexion
Username=root

; Mot de passe (sera automatiquement chiffré)
Password=

[Application]
; Langue de l'interface : fr, en, es, de
Language=fr

; Thème visuel : light, dark
Theme=light

; Niveau de log : debug, info, warning, error
LogLevel=info
```

### 3. Fichier .gitignore

Ne versionnez pas les configurations locales :

```gitignore
# Fichiers de configuration
config.ini
config.local.ini
config.prod.ini
*.local.json

# Préférences utilisateur
user.ini
```

### 4. Migration de configuration

Gérez les changements de structure :

```pascal
unit ConfigMigration;

interface

uses
  System.IniFiles;

type
  TConfigMigration = class
  private
    class function GetConfigVersion(IniFile: TIniFile): Integer;
    class procedure SetConfigVersion(IniFile: TIniFile; Version: Integer);
    class procedure MigrateFrom1To2(IniFile: TIniFile);
    class procedure MigrateFrom2To3(IniFile: TIniFile);
  public
    class procedure Migrate(const ConfigFile: string);
  end;

implementation

const
  CURRENT_VERSION = 3;

class function TConfigMigration.GetConfigVersion(IniFile: TIniFile): Integer;
begin
  Result := IniFile.ReadInteger('Meta', 'Version', 1);
end;

class procedure TConfigMigration.SetConfigVersion(IniFile: TIniFile; Version: Integer);
begin
  IniFile.WriteInteger('Meta', 'Version', Version);
end;

class procedure TConfigMigration.MigrateFrom1To2(IniFile: TIniFile);
begin
  // Version 1 → 2 : Ajout du timeout
  if not IniFile.ValueExists('Network', 'Timeout') then
    IniFile.WriteInteger('Network', 'Timeout', 30);
end;

class procedure TConfigMigration.MigrateFrom2To3(IniFile: TIniFile);
begin
  // Version 2 → 3 : Renommage de la section
  if IniFile.SectionExists('App') then
  begin
    // Copier les valeurs
    IniFile.WriteString('Application', 'Language',
      IniFile.ReadString('App', 'Language', 'fr'));
    // Supprimer l'ancienne section
    IniFile.EraseSection('App');
  end;
end;

class procedure TConfigMigration.Migrate(const ConfigFile: string);
var
  IniFile: TIniFile;
  CurrentVersion: Integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  try
    CurrentVersion := GetConfigVersion(IniFile);

    // Appliquer les migrations successives
    if CurrentVersion < 2 then
    begin
      MigrateFrom1To2(IniFile);
      SetConfigVersion(IniFile, 2);
    end;

    if CurrentVersion < 3 then
    begin
      MigrateFrom2To3(IniFile);
      SetConfigVersion(IniFile, 3);
    end;

  finally
    IniFile.Free;
  end;
end;

end.
```

## Conclusion

La gestion de la configuration est un aspect crucial de toute application professionnelle. Une bonne gestion de la configuration offre :

**Avantages immédiats :**
- Flexibilité sans recompilation
- Déploiement simplifié
- Personnalisation utilisateur

**Avantages à long terme :**
- Maintenance facilitée
- Support multi-environnements
- Sécurité renforcée

**Points clés à retenir :**

1. **Ne jamais coder en dur** - Tout ce qui peut changer doit être configurable
2. **Séparer les types de config** - Application, utilisateur, sensible
3. **Choisir le bon format** - INI simple, JSON structuré, Registre système
4. **Sécuriser les secrets** - Chiffrement, DPAPI, variables d'environnement
5. **Valider la configuration** - Vérifier au démarrage
6. **Documenter** - Template et README clairs
7. **Ne pas versionner les secrets** - Utiliser .gitignore
8. **Centraliser** - Une classe de configuration globale

En appliquant ces principes, vous créerez des applications flexibles, sécurisées et faciles à déployer dans différents environnements. La gestion de configuration n'est pas un détail technique, c'est une composante essentielle de l'architecture de votre application.

Dans la prochaine section, nous explorerons le versionnement et la gestion de code source, un autre pilier fondamental du développement professionnel.

⏭️ [Versionnement et gestion de code source](/18-architecture-et-bonnes-pratiques/05-versionnement-et-gestion-de-code-source.md)
