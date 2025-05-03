# 16. Sécurité des applications
## 16.7 Stockage sécurisé des identifiants

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Le stockage sécurisé des identifiants (mots de passe, clés API, jetons d'accès, etc.) est un aspect crucial de la sécurité des applications. Des identifiants mal protégés peuvent entraîner des accès non autorisés, des fuites de données et des violations de confidentialité. Dans ce chapitre, nous allons explorer diverses techniques pour stocker les identifiants de manière sécurisée dans vos applications Delphi.

### Pourquoi éviter le stockage en texte clair ?

Le stockage des identifiants en texte clair dans votre code ou dans des fichiers de configuration présente de sérieux risques :

```pas
// DANGEREUX : Ne faites jamais ceci
const
  DATABASE_PASSWORD = 'MotDePasse123!';
  API_KEY = 'sk_live_abcdefghijklmnopqrstuvwxyz';
```

Les problèmes avec cette approche sont nombreux :
- Les identifiants peuvent être découverts par simple inspection du code
- Ils peuvent être exposés dans les systèmes de contrôle de version
- Ils sont visibles dans la mémoire de l'application
- Ils sont difficiles à modifier sans recompiler l'application

### 1. Utilisation du Windows Data Protection API (DPAPI)

L'API DPAPI de Windows est spécialement conçue pour le chiffrement/déchiffrement de données sensibles. Elle utilise des clés liées au compte utilisateur Windows ou à l'ordinateur, ce qui la rend idéale pour stocker des identifiants de manière sécurisée.

#### Création d'une classe de gestion des identifiants avec DPAPI

```pas
unit SecureCredentials;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, System.NetEncoding;

type
  TCredentialType = (ctPassword, ctAPIKey, ctConnectionString, ctToken);

  TSecureCredentialManager = class
  private
    FAppName: string;

    function ProtectData(const Data: TBytes; Scope: Integer): TBytes;
    function UnprotectData(const Data: TBytes; Scope: Integer): TBytes;
  public
    constructor Create(const AppName: string);

    // Stockage sécurisé des identifiants
    procedure StoreCredential(const CredentialName: string;
                             const Value: string;
                             CredentialType: TCredentialType;
                             MachineWide: Boolean = False);

    // Récupération sécurisée des identifiants
    function RetrieveCredential(const CredentialName: string;
                               CredentialType: TCredentialType;
                               MachineWide: Boolean = False): string;

    // Suppression d'un identifiant
    procedure DeleteCredential(const CredentialName: string;
                              CredentialType: TCredentialType;
                              MachineWide: Boolean = False);

    // Vérification de l'existence d'un identifiant
    function CredentialExists(const CredentialName: string;
                             CredentialType: TCredentialType;
                             MachineWide: Boolean = False): Boolean;
  end;

implementation

uses
  System.Win.Registry, System.IOUtils;

const
  // Identifiants pour les options de protection DPAPI
  CRYPTPROTECT_UI_FORBIDDEN = $1;
  CRYPTPROTECT_LOCAL_MACHINE = $4;

// Déclarations des fonctions de l'API Windows
function CryptProtectData(
  var DataIn: DATA_BLOB;
  szDataDescr: PWideChar;
  var OptionalEntropy: DATA_BLOB;
  pvReserved: Pointer;
  pPromptStruct: Pointer;
  dwFlags: DWORD;
  var DataOut: DATA_BLOB
): BOOL; stdcall; external 'Crypt32.dll';

function CryptUnprotectData(
  var DataIn: DATA_BLOB;
  ppszDataDescr: PPWideChar;
  var OptionalEntropy: DATA_BLOB;
  pvReserved: Pointer;
  pPromptStruct: Pointer;
  dwFlags: DWORD;
  var DataOut: DATA_BLOB
): BOOL; stdcall; external 'Crypt32.dll';

{ TSecureCredentialManager }

constructor TSecureCredentialManager.Create(const AppName: string);
begin
  inherited Create;
  FAppName := AppName;
end;

function TSecureCredentialManager.ProtectData(const Data: TBytes; Scope: Integer): TBytes;
var
  DataIn, DataOut, Entropy: DATA_BLOB;
  Flags: DWORD;
begin
  // Initialisation des structures
  DataIn.cbData := Length(Data);
  DataIn.pbData := Pointer(Data);

  Entropy.cbData := Length(FAppName) * SizeOf(Char);
  Entropy.pbData := Pointer(StringToOleStr(FAppName));

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // Définir les flags
  Flags := CRYPTPROTECT_UI_FORBIDDEN;
  if Scope = 1 then
    Flags := Flags or CRYPTPROTECT_LOCAL_MACHINE;

  // Chiffrer les données
  if CryptProtectData(
    DataIn,
    'SecureCredential',  // Description
    Entropy,            // Entropie supplémentaire
    nil,                // Réservé
    nil,                // Structure d'invite
    Flags,              // Flags
    DataOut             // Données chiffrées
  ) then
  begin
    // Convertir le résultat en tableau de bytes
    SetLength(Result, DataOut.cbData);
    if DataOut.cbData > 0 then
      Move(DataOut.pbData^, Result[0], DataOut.cbData);

    // Libérer la mémoire allouée par CryptProtectData
    if DataOut.pbData <> nil then
      LocalFree(Cardinal(DataOut.pbData));
  end
  else
    raise Exception.Create('Échec du chiffrement des données: ' + SysErrorMessage(GetLastError));
end;

function TSecureCredentialManager.UnprotectData(const Data: TBytes; Scope: Integer): TBytes;
var
  DataIn, DataOut, Entropy: DATA_BLOB;
  Flags: DWORD;
begin
  // Initialisation des structures
  DataIn.cbData := Length(Data);
  DataIn.pbData := Pointer(Data);

  Entropy.cbData := Length(FAppName) * SizeOf(Char);
  Entropy.pbData := Pointer(StringToOleStr(FAppName));

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // Définir les flags
  Flags := CRYPTPROTECT_UI_FORBIDDEN;
  if Scope = 1 then
    Flags := Flags or CRYPTPROTECT_LOCAL_MACHINE;

  // Déchiffrer les données
  if CryptUnprotectData(
    DataIn,
    nil,                // Description (non utilisée)
    Entropy,            // Entropie supplémentaire
    nil,                // Réservé
    nil,                // Structure d'invite
    Flags,              // Flags
    DataOut             // Données déchiffrées
  ) then
  begin
    // Convertir le résultat en tableau de bytes
    SetLength(Result, DataOut.cbData);
    if DataOut.cbData > 0 then
      Move(DataOut.pbData^, Result[0], DataOut.cbData);

    // Libérer la mémoire allouée par CryptUnprotectData
    if DataOut.pbData <> nil then
      LocalFree(Cardinal(DataOut.pbData));
  end
  else
    raise Exception.Create('Échec du déchiffrement des données: ' + SysErrorMessage(GetLastError));
end;

procedure TSecureCredentialManager.StoreCredential(const CredentialName: string;
                                                 const Value: string;
                                                 CredentialType: TCredentialType;
                                                 MachineWide: Boolean);
var
  Registry: TRegistry;
  RegKey: string;
  TypePrefix: string;
  ProtectedData: TBytes;
  ProtectedBase64: string;
  Scope: Integer;
begin
  // Déterminer le préfixe en fonction du type d'identifiant
  case CredentialType of
    ctPassword: TypePrefix := 'PWD';
    ctAPIKey: TypePrefix := 'API';
    ctConnectionString: TypePrefix := 'CON';
    ctToken: TypePrefix := 'TOK';
  end;

  // Déterminer la portée (utilisateur ou machine)
  if MachineWide then
    Scope := 1
  else
    Scope := 0;

  // Construire la clé de registre
  RegKey := 'Software\' + FAppName + '\SecureCredentials';

  // Chiffrer la valeur
  ProtectedData := ProtectData(TEncoding.UTF8.GetBytes(Value), Scope);

  // Convertir en Base64 pour le stockage
  ProtectedBase64 := TNetEncoding.Base64.EncodeBytesToString(ProtectedData);

  // Stocker dans le registre
  Registry := TRegistry.Create;
  try
    if MachineWide then
      Registry.RootKey := HKEY_LOCAL_MACHINE
    else
      Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey(RegKey, True) then
    begin
      Registry.WriteString(TypePrefix + '_' + CredentialName, ProtectedBase64);
      Registry.CloseKey;
    end
    else
      raise Exception.Create('Impossible d''ouvrir la clé de registre');
  finally
    Registry.Free;
  end;
end;

function TSecureCredentialManager.RetrieveCredential(const CredentialName: string;
                                                   CredentialType: TCredentialType;
                                                   MachineWide: Boolean): string;
var
  Registry: TRegistry;
  RegKey: string;
  TypePrefix: string;
  ProtectedBase64: string;
  ProtectedData: TBytes;
  DecryptedData: TBytes;
  Scope: Integer;
begin
  Result := '';

  // Déterminer le préfixe en fonction du type d'identifiant
  case CredentialType of
    ctPassword: TypePrefix := 'PWD';
    ctAPIKey: TypePrefix := 'API';
    ctConnectionString: TypePrefix := 'CON';
    ctToken: TypePrefix := 'TOK';
  end;

  // Déterminer la portée (utilisateur ou machine)
  if MachineWide then
    Scope := 1
  else
    Scope := 0;

  // Construire la clé de registre
  RegKey := 'Software\' + FAppName + '\SecureCredentials';

  // Récupérer depuis le registre
  Registry := TRegistry.Create;
  try
    if MachineWide then
      Registry.RootKey := HKEY_LOCAL_MACHINE
    else
      Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKeyReadOnly(RegKey) then
    begin
      if Registry.ValueExists(TypePrefix + '_' + CredentialName) then
      begin
        ProtectedBase64 := Registry.ReadString(TypePrefix + '_' + CredentialName);
        Registry.CloseKey;

        // Convertir depuis Base64
        ProtectedData := TNetEncoding.Base64.DecodeStringToBytes(ProtectedBase64);

        // Déchiffrer la valeur
        try
          DecryptedData := UnprotectData(ProtectedData, Scope);
          Result := TEncoding.UTF8.GetString(DecryptedData);
        except
          on E: Exception do
            raise Exception.Create('Erreur lors du déchiffrement: ' + E.Message);
        end;
      end
      else
        Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSecureCredentialManager.DeleteCredential(const CredentialName: string;
                                                  CredentialType: TCredentialType;
                                                  MachineWide: Boolean);
var
  Registry: TRegistry;
  RegKey: string;
  TypePrefix: string;
begin
  // Déterminer le préfixe en fonction du type d'identifiant
  case CredentialType of
    ctPassword: TypePrefix := 'PWD';
    ctAPIKey: TypePrefix := 'API';
    ctConnectionString: TypePrefix := 'CON';
    ctToken: TypePrefix := 'TOK';
  end;

  // Construire la clé de registre
  RegKey := 'Software\' + FAppName + '\SecureCredentials';

  // Supprimer du registre
  Registry := TRegistry.Create;
  try
    if MachineWide then
      Registry.RootKey := HKEY_LOCAL_MACHINE
    else
      Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey(RegKey, False) then
    begin
      if Registry.ValueExists(TypePrefix + '_' + CredentialName) then
        Registry.DeleteValue(TypePrefix + '_' + CredentialName);

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

function TSecureCredentialManager.CredentialExists(const CredentialName: string;
                                                 CredentialType: TCredentialType;
                                                 MachineWide: Boolean): Boolean;
var
  Registry: TRegistry;
  RegKey: string;
  TypePrefix: string;
begin
  Result := False;

  // Déterminer le préfixe en fonction du type d'identifiant
  case CredentialType of
    ctPassword: TypePrefix := 'PWD';
    ctAPIKey: TypePrefix := 'API';
    ctConnectionString: TypePrefix := 'CON';
    ctToken: TypePrefix := 'TOK';
  end;

  // Construire la clé de registre
  RegKey := 'Software\' + FAppName + '\SecureCredentials';

  // Vérifier dans le registre
  Registry := TRegistry.Create;
  try
    if MachineWide then
      Registry.RootKey := HKEY_LOCAL_MACHINE
    else
      Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKeyReadOnly(RegKey) then
    begin
      Result := Registry.ValueExists(TypePrefix + '_' + CredentialName);
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

end.
```

#### Exemple d'utilisation de la classe de gestion des identifiants

```pas
procedure TMainForm.SaveDatabaseCredentials;
var
  CredManager: TSecureCredentialManager;
begin
  CredManager := TSecureCredentialManager.Create('MyDelphiApp');
  try
    // Stocker les identifiants
    CredManager.StoreCredential('DatabaseServer',
                               'server.example.com',
                               ctConnectionString);

    CredManager.StoreCredential('DatabaseUser',
                               'admin',
                               ctPassword);

    CredManager.StoreCredential('DatabasePassword',
                               EditPassword.Text,
                               ctPassword);

    ShowMessage('Identifiants stockés avec succès');
  finally
    CredManager.Free;
  end;
end;

procedure TMainForm.ConnectToDatabase;
var
  CredManager: TSecureCredentialManager;
  Server, Username, Password: string;
begin
  CredManager := TSecureCredentialManager.Create('MyDelphiApp');
  try
    // Récupérer les identifiants
    Server := CredManager.RetrieveCredential('DatabaseServer', ctConnectionString);
    Username := CredManager.RetrieveCredential('DatabaseUser', ctPassword);
    Password := CredManager.RetrieveCredential('DatabasePassword', ctPassword);

    if (Server <> '') and (Username <> '') and (Password <> '') then
    begin
      // Utiliser les identifiants pour la connexion
      FDConnection1.Params.Values['Server'] := Server;
      FDConnection1.Params.Values['User_Name'] := Username;
      FDConnection1.Params.Values['Password'] := Password;

      try
        FDConnection1.Connected := True;
        ShowMessage('Connexion réussie !');
      except
        on E: Exception do
          ShowMessage('Erreur de connexion : ' + E.Message);
      end;
    end
    else
      ShowMessage('Identifiants manquants. Veuillez configurer la connexion.');
  finally
    CredManager.Free;
  end;
end;
```

### 2. Utilisation du Credential Manager de Windows

Windows dispose d'un gestionnaire d'identifiants intégré (Credential Manager) que vous pouvez utiliser via l'API Windows. Cela offre l'avantage d'une interface utilisateur intégrée à Windows pour la gestion des identifiants.

```pas
unit WindowsCredentialManager;

interface

uses
  System.SysUtils, Winapi.Windows;

type
  TWindowsCredentialManager = class
  public
    // Stockage d'identifiants
    function StoreCredential(const TargetName, Username, Password: string): Boolean;

    // Récupération d'identifiants
    function RetrieveCredential(const TargetName: string;
                               out Username, Password: string): Boolean;

    // Suppression d'identifiants
    function DeleteCredential(const TargetName: string): Boolean;
  end;

implementation

const
  CRED_TYPE_GENERIC = 1;
  CRED_PERSIST_LOCAL_MACHINE = 2;

type
  CREDENTIAL_ATTRIBUTEW = record
    Keyword: PWideChar;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: Pointer;
  end;
  PCREDENTIAL_ATTRIBUTEW = ^CREDENTIAL_ATTRIBUTEW;

  CREDENTIALW = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: PWideChar;
    Comment: PWideChar;
    LastWritten: TFileTime;
    CredentialBlobSize: DWORD;
    CredentialBlob: Pointer;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEW;
    TargetAlias: PWideChar;
    UserName: PWideChar;
  end;
  PCREDENTIALW = ^CREDENTIALW;
  PPCREDENTIALW = ^PCREDENTIALW;

function CredWriteW(Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;
  external 'Advapi32.dll' name 'CredWriteW';
function CredReadW(TargetName: PWideChar; Type_: DWORD; Flags: DWORD;
  var Credential: PCREDENTIALW): BOOL; stdcall;
  external 'Advapi32.dll' name 'CredReadW';
function CredFree(Buffer: Pointer): BOOL; stdcall;
  external 'Advapi32.dll' name 'CredFree';
function CredDeleteW(TargetName: PWideChar; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
  external 'Advapi32.dll' name 'CredDeleteW';

{ TWindowsCredentialManager }

function TWindowsCredentialManager.StoreCredential(const TargetName, Username,
  Password: string): Boolean;
var
  Credential: CREDENTIALW;
  PasswordBytes: TBytes;
begin
  FillChar(Credential, SizeOf(Credential), 0);

  // Convertir le mot de passe en bytes
  PasswordBytes := TEncoding.Unicode.GetBytes(Password);

  // Paramétrer la structure d'identifiants
  Credential.Type_ := CRED_TYPE_GENERIC;
  Credential.TargetName := PWideChar(TargetName);
  Credential.UserName := PWideChar(Username);
  Credential.CredentialBlobSize := Length(PasswordBytes);
  Credential.CredentialBlob := Pointer(PasswordBytes);
  Credential.Persist := CRED_PERSIST_LOCAL_MACHINE;

  // Écrire l'identifiant
  Result := CredWriteW(@Credential, 0);
end;

function TWindowsCredentialManager.RetrieveCredential(const TargetName: string;
  out Username, Password: string): Boolean;
var
  CredentialPtr: PCREDENTIALW;
begin
  Result := False;
  Username := '';
  Password := '';

  // Lire l'identifiant
  if CredReadW(PWideChar(TargetName), CRED_TYPE_GENERIC, 0, CredentialPtr) then
  begin
    try
      // Récupérer le nom d'utilisateur
      Username := CredentialPtr^.UserName;

      // Récupérer le mot de passe
      if (CredentialPtr^.CredentialBlobSize > 0) and
         (CredentialPtr^.CredentialBlob <> nil) then
      begin
        SetLength(Password, CredentialPtr^.CredentialBlobSize div SizeOf(Char));
        Move(CredentialPtr^.CredentialBlob^, Password[1], CredentialPtr^.CredentialBlobSize);
      end;

      Result := True;
    finally
      // Libérer la mémoire allouée par CredRead
      CredFree(CredentialPtr);
    end;
  end;
end;

function TWindowsCredentialManager.DeleteCredential(const TargetName: string): Boolean;
begin
  Result := CredDeleteW(PWideChar(TargetName), CRED_TYPE_GENERIC, 0);
end;

end.
```

#### Utilisation du Credential Manager Windows

```pas
procedure TMainForm.SaveAPICredentials;
var
  CredManager: TWindowsCredentialManager;
begin
  CredManager := TWindowsCredentialManager.Create;
  try
    if CredManager.StoreCredential('MyApp:API', 'apiuser', EditAPIKey.Text) then
      ShowMessage('Clé API stockée avec succès')
    else
      ShowMessage('Erreur lors du stockage de la clé API : ' + SysErrorMessage(GetLastError));
  finally
    CredManager.Free;
  end;
end;

procedure TMainForm.RetrieveAPICredentials;
var
  CredManager: TWindowsCredentialManager;
  Username, APIKey: string;
begin
  CredManager := TWindowsCredentialManager.Create;
  try
    if CredManager.RetrieveCredential('MyApp:API', Username, APIKey) then
    begin
      EditUsername.Text := Username;
      EditAPIKey.Text := APIKey;
    end
    else
      ShowMessage('Identifiants non trouvés ou erreur : ' + SysErrorMessage(GetLastError));
  finally
    CredManager.Free;
  end;
end;
```

### 3. Chiffrement avec une clé personnalisée

Si vous avez besoin de plus de contrôle ou si vous développez pour des plateformes autres que Windows, vous pouvez implémenter votre propre système de chiffrement avec une clé personnalisée.

```pas
unit CustomKeyEncryption;

interface

uses
  System.SysUtils, System.Classes, System.Hash, System.NetEncoding,
  System.Generics.Collections;

type
  TCustomKeyCredentialManager = class
  private
    FEncryptionKey: string;
    FCredentials: TDictionary<string, string>;
    FFilePath: string;

    function DeriveKey(const MasterKey, Salt: string): TBytes;
    function Encrypt(const PlainText, Salt: string): string;
    function Decrypt(const CipherText, Salt: string): string;

    procedure LoadCredentials;
    procedure SaveCredentials;
  public
    constructor Create(const FilePath, MasterKey: string);
    destructor Destroy; override;

    procedure StoreCredential(const Name, Value: string);
    function RetrieveCredential(const Name: string): string;
    procedure DeleteCredential(const Name: string);
    function CredentialExists(const Name: string): Boolean;
  end;

implementation

uses
  System.IOUtils, System.JSON;

constructor TCustomKeyCredentialManager.Create(const FilePath, MasterKey: string);
begin
  inherited Create;
  FFilePath := FilePath;
  FEncryptionKey := MasterKey;
  FCredentials := TDictionary<string, string>.Create;

  // Charger les identifiants existants
  if FileExists(FFilePath) then
    LoadCredentials;
end;

destructor TCustomKeyCredentialManager.Destroy;
begin
  FCredentials.Free;
  inherited;
end;

function TCustomKeyCredentialManager.DeriveKey(const MasterKey, Salt: string): TBytes;
var
  CombinedKey: string;
begin
  // Dériver une clé à partir de la clé maître et du sel
  // Dans une implémentation réelle, utilisez un algorithme comme PBKDF2
  CombinedKey := MasterKey + Salt;
  Result := THashSHA2.GetHashBytes(CombinedKey);
end;

function TCustomKeyCredentialManager.Encrypt(const PlainText, Salt: string): string;
var
  Key: TBytes;
  PlainBytes, CipherBytes: TBytes;
  I: Integer;
begin
  // Cet exemple utilise un chiffrement simple XOR
  // Pour une application réelle, utilisez un algorithme standard comme AES

  // Dériver la clé
  Key := DeriveKey(FEncryptionKey, Salt);

  // Convertir le texte en bytes
  PlainBytes := TEncoding.UTF8.GetBytes(PlainText);

  // Chiffrer (XOR simple)
  SetLength(CipherBytes, Length(PlainBytes));
  for I := 0 to High(PlainBytes) do
    CipherBytes[I] := PlainBytes[I] xor Key[I mod Length(Key)];

  // Encoder en Base64
  Result := TNetEncoding.Base64.EncodeBytesToString(CipherBytes);
end;

function TCustomKeyCredentialManager.Decrypt(const CipherText, Salt: string): string;
var
  Key: TBytes;
  CipherBytes, PlainBytes: TBytes;
  I: Integer;
begin
  // Dériver la clé
  Key := DeriveKey(FEncryptionKey, Salt);

  // Décoder le Base64
  CipherBytes := TNetEncoding.Base64.DecodeStringToBytes(CipherText);

  // Déchiffrer (XOR simple)
  SetLength(PlainBytes, Length(CipherBytes));
  for I := 0 to High(CipherBytes) do
    PlainBytes[I] := CipherBytes[I] xor Key[I mod Length(Key)];

  // Convertir en chaîne
  Result := TEncoding.UTF8.GetString(PlainBytes);
end;

procedure TCustomKeyCredentialManager.LoadCredentials;
var
  JSONText: string;
  JSONObject: TJSONObject;
  JSONPair: TJSONPair;
begin
  try
    // Lire le fichier chiffré
    JSONText := TFile.ReadAllText(FFilePath);

    // Parser le JSON
    JSONObject := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
    if JSONObject <> nil then
    try
      FCredentials.Clear;

      // Parcourir les paires nom/valeur
      for JSONPair in JSONObject do
      begin
        FCredentials.Add(JSONPair.JsonString.Value, JSONPair.JsonValue.Value);
      end;
    finally
      JSONObject.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer les erreurs de chargement
      // Dans une application réelle, journalisez l'erreur
    end;
  end;
end;

procedure TCustomKeyCredentialManager.SaveCredentials;
var
  JSONObject: TJSONObject;
  Pair: TPair<string, string>;
begin
  JSONObject := TJSONObject.Create;
  try
    // Ajouter chaque identifiant au JSON
    for Pair in FCredentials do
    begin
      JSONObject.AddPair(Pair.Key, Pair.Value);
    end;

    // Sauvegarder dans le fichier
    TFile.WriteAllText(FFilePath, JSONObject.ToString);
  finally
    JSONObject.Free;
  end;
end;

procedure TCustomKeyCredentialManager.StoreCredential(const Name, Value: string);
var
  EncryptedValue: string;
begin
  // Chiffrer la valeur avec le nom comme sel
  EncryptedValue := Encrypt(Value, Name);

  // Stocker dans le dictionnaire
  FCredentials.AddOrSetValue(Name, EncryptedValue);

  // Sauvegarder les changements
  SaveCredentials;
end;

function TCustomKeyCredentialManager.RetrieveCredential(const Name: string): string;
var
  EncryptedValue: string;
begin
  Result := '';

  // Récupérer la valeur chiffrée
  if FCredentials.TryGetValue(Name, EncryptedValue) then
  begin
    // Déchiffrer avec le nom comme sel
    Result := Decrypt(EncryptedValue, Name);
  end;
end;

procedure TCustomKeyCredentialManager.DeleteCredential(const Name: string);
begin
  // Supprimer l'identifiant
  if FCredentials.ContainsKey(Name) then
  begin
    FCredentials.Remove(Name);

    // Sauvegarder les changements
    SaveCredentials;
  end;
end;

function TCustomKeyCredentialManager.CredentialExists(const Name: string): Boolean;
begin
  Result := FCredentials.ContainsKey(Name);
end;

end.
```

#### Utilisation de l'encryptage avec clé personnalisée

```pas
procedure TMainForm.SaveSecretApiKey;
var
  CredManager: TCustomKeyCredentialManager;
  MasterKey: string;
begin
  // La clé maître pourrait provenir d'une entrée utilisateur ou d'une autre source sécurisée
  MasterKey := GetApplicationMasterKey;

  CredManager := TCustomKeyCredentialManager.Create(
    TPath.Combine(TPath.GetDocumentsPath, 'myapp_credentials.dat'),
    MasterKey
  );
  try
    CredManager.StoreCredential('ApiKey', EditApiKey.Text);
    ShowMessage('Clé API enregistrée avec succès');
  finally
    CredManager.Free;
  end;
end;

function TMainForm.GetApplicationMasterKey: string;
var
  DeviceID: string;
begin
  // Dans une implémentation réelle, utilisez une méthode plus sécurisée
  // pour obtenir ou générer la clé maître
  DeviceID := GetComputerIdentifier; // Une fonction qui génère un ID unique pour l'ordinateur
  Result := THashSHA2.GetHashString('MySecretAppSalt' + DeviceID);
end;
```

> ⚠️ **Important** : L'exemple de chiffrement ci-dessus est simplifié à des fins éducatives. Pour une application réelle, utilisez des algorithmes cryptographiques standards comme AES avec une implémentation éprouvée.

### 4. Stockage dans des coffres-forts de mots de passe tiers

Pour les applications professionnelles, vous pouvez intégrer votre application avec des gestionnaires de mots de passe tiers comme KeePass, LastPass ou 1Password via leurs API.

```pas
unit KeePassIntegration;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.JSON;

type
  TKeePassHTTPClient = class
  private
    FPort: Integer;
    FHost: string;
    FClientID: string;
    FClientKey: string;

    function SendRequest(const Action, URL, Key, ID: string; Data: TJSONObject = nil): TJSONObject;
  public
    constructor Create(const Host: string = 'localhost'; Port: Integer = 19455);

    function TestAssociation: Boolean;
    function Associate: Boolean;
    function GetCredentials(const URL: string): TJSONArray;
    function GetLogins(const URL: string): TJSONArray;
    function SetLogin(const URL, Username, Password: string): Boolean;
  end;

implementation

// Note: Cette implémentation est simplifiée et partielle
// Une intégration réelle avec KeePass nécessiterait plus de code

constructor TKeePassHTTPClient.Create(const Host: string; Port: Integer);
begin
  inherited Create;
  FHost := Host;
  FPort := Port;
  FClientID := 'MyDelphiApp';

  // Dans une implémentation réelle, stockez et récupérez la clé client
  // de manière sécurisée, par exemple avec DPAPI
  FClientKey := ''; // À récupérer depuis un stockage sécurisé
end;

function TKeePassHTTPClient.SendRequest(const Action, URL, Key, ID: string;
  Data: TJSONObject): TJSONObject;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  RequestJSON, ResponseJSON: TJSONObject;
  RequestBody, ResponseBody: string;
begin
  Result := nil;

  // Créer l'objet JSON pour la requête
  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.AddPair('action', Action);
    RequestJSON.AddPair('url', URL);
    RequestJSON.AddPair('key', Key);
    RequestJSON.AddPair('id', ID);

    if Data <> nil then
    begin
      // Ajouter les données supplémentaires
      for var Pair in Data do
      begin
        RequestJSON.AddPair(Pair.JsonString.Clone as TJSONString,
                           Pair.JsonValue.Clone as TJSONValue);
      end;
    end;

    RequestBody := RequestJSON.ToString;
  finally
    RequestJSON.Free;
  end;

  // Envoyer la requête HTTP
  HTTPClient := THTTPClient.Create;
  try
    Response := HTTPClient.Post(
      Format('http://%s:%d', [FHost, FPort]),
      TStringStream.Create(RequestBody),
      nil
    );

    ResponseBody := Response.ContentAsString;

    if Response.StatusCode = 200 then
    begin
      // Parser la réponse JSON
      ResponseJSON := TJSONObject.ParseJSONValue(ResponseBody) as TJSONObject;
      if ResponseJSON <> nil then
        Result := ResponseJSON;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TKeePassHTTPClient.TestAssociation: Boolean;
var
  Response: TJSONObject;
begin
  Result := False;

  if (FClientID = '') or (FClientKey = '') then
    Exit;

  Response := SendRequest('test-associate', '', FClientKey, FClientID);
  if Response <> nil then
  try
    Result := Response.GetValue<Boolean>('success', False);
  finally
    Response.Free;
  end;
end;

function TKeePassHTTPClient.Associate: Boolean;
var
  Response: TJSONObject;
begin
  Result := False;

  // Générer une nouvelle clé
  FClientKey := GenerateRandomKey; // À implémenter

  Response := SendRequest('associate', '', FClientKey, FClientID);
  if Response <> nil then
  try
    Result := Response.GetValue<Boolean>('success', False);

    if Result then
    begin
      // Stocker la clé client de manière sécurisée
      SaveClientKey(FClientKey); // À implémenter
    end;
  finally
    Response.Free;
  end;
end;

function TKeePassHTTPClient.GetCredentials(const URL: string): TJSONArray;
var
  Response: TJSONObject;
begin
  Result := nil;

  if not TestAssociation then
    if not Associate then
      Exit;

  Response := SendRequest('get-logins', URL, FClientKey, FClientID);
  if Response <> nil then
  try
    if Response.GetValue<Boolean>('success', False) then
    begin
      if Response.TryGetValue<TJSONArray>('entries', Result) then
        Result := Result.Clone as TJSONArray;
    end;
  finally
    Response.Free;
  end;
end;

// Autres méthodes à implémenter...

end.
```

### 5. Demander les identifiants à l'utilisateur

Pour des raisons de sécurité, parfois la meilleure approche est de demander les identifiants à l'utilisateur à chaque fois qu'ils sont nécessaires, sans les stocker du tout.

```pas
function TMainForm.GetDatabaseCredentials(out Username, Password: string): Boolean;
var
  CredentialForm: TCredentialForm;
begin
  CredentialForm := TCredentialForm.Create(nil);
  try
    // Préremplir le nom d'utilisateur si disponible
    CredentialForm.EditUsername.Text := LastUsername;

    // Afficher le dialogue
    Result := CredentialForm.ShowModal = mrOk;

    if Result then
    begin
      Username := CredentialForm.EditUsername.Text;
      Password := CredentialForm.EditPassword.Text;

      // Enregistrer le nom d'utilisateur pour la prochaine fois
      LastUsername := Username;
    end;
  finally
    CredentialForm.Free;
  end;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
var
  Username, Password: string;
begin
  if GetDatabaseCredentials(Username, Password) then
  begin
    try
      // Utiliser les identifiants pour la connexion
      FDConnection1.Params.Values['User_Name'] := Username;
      FDConnection1.Params.Values['Password'] := Password;
      FDConnection1.Connected := True;

      ShowMessage('Connexion réussie !');
    except
      on E: Exception do
        ShowMessage('Erreur de connexion : ' + E.Message);
    end;
  end;
end;
```

### 6. Utilisation de fichiers de configuration sécurisés

Pour les déploiements où un fichier de configuration est nécessaire, vous pouvez utiliser un fichier chiffré plutôt qu'un fichier en texte clair.

```pas
unit SecureConfig;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils;

type
  TSecureConfig = class
  private
    FConfigPath: string;
    FEncryptionKey: string;
    FConfig: TJSONObject;

    function Encrypt(const Data: string): string;
    function Decrypt(const Data: string): string;
  public
    constructor Create(const ConfigPath, EncryptionKey: string);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    function GetValue(const Section, Key: string; DefaultValue: string = ''): string;
    procedure SetValue(const Section, Key, Value: string);

    function GetEncryptedValue(const Section, Key: string; DefaultValue: string = ''): string;
    procedure SetEncryptedValue(const Section, Key, Value: string);
  end;

implementation

constructor TSecureConfig.Create(const ConfigPath, EncryptionKey: string);
begin
  inherited Create;
  FConfigPath := ConfigPath;
  FEncryptionKey := EncryptionKey;
  FConfig := TJSONObject.Create;

  // Charger la configuration si le fichier existe
  if FileExists(FConfigPath) then
    Load;
end;

destructor TSecureConfig.Destroy;
begin
  FConfig.Free;
  inherited;
end;

function TSecureConfig.Encrypt(const Data: string): string;
begin
  // Implémentez ici votre méthode de chiffrement préférée
  // Pour l'exemple, nous utilisons une fonction fictive
  Result := SimpleEncrypt(Data, FEncryptionKey);
end;

function TSecureConfig.Decrypt(const Data: string): string;
begin
  // Implémentez ici votre méthode de déchiffrement correspondante
  Result := SimpleDecrypt(Data, FEncryptionKey);
end;

procedure TSecureConfig.Load;
var
  EncryptedText, JsonText: string;
begin
  try
    // Lire le fichier chiffré
    EncryptedText := TFile.ReadAllText(FConfigPath);

    // Déchiffrer
    JsonText := Decrypt(EncryptedText);

    // Parser le JSON
    FConfig.Free;
    FConfig := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;

    if FConfig = nil then
      FConfig := TJSONObject.Create;
  except
    on E: Exception do
    begin
      // En cas d'erreur, créer un objet vide
      FConfig.Free;
      FConfig := TJSONObject.Create;
    end;
  end;
end;

procedure TSecureConfig.Save;
var
  JsonText, EncryptedText: string;
begin
  // Convertir en JSON
  JsonText := FConfig.ToString;

  // Chiffrer
  EncryptedText := Encrypt(JsonText);

  // Sauvegarder dans le fichier
  TFile.WriteAllText(FConfigPath, EncryptedText);
end;

function TSecureConfig.GetValue(const Section, Key: string; DefaultValue: string): string;
var
  SectionObj: TJSONObject;
begin
  Result := DefaultValue;

  // Chercher la section
  if FConfig.TryGetValue<TJSONObject>(Section, SectionObj) then
  begin
    // Chercher la clé dans la section
    SectionObj.TryGetValue<string>(Key, Result);
  end;
end;

procedure TSecureConfig.SetValue(const Section, Key, Value: string);
var
  SectionObj: TJSONObject;
begin
  // Vérifier si la section existe, sinon la créer
  if not FConfig.TryGetValue<TJSONObject>(Section, SectionObj) then
  begin
    SectionObj := TJSONObject.Create;
    FConfig.AddPair(Section, SectionObj);
  end;

  // Ajouter ou mettre à jour la valeur
  if SectionObj.Get(Key) <> nil then
    SectionObj.RemovePair(Key);

  SectionObj.AddPair(Key, Value);
end;

function TSecureConfig.GetEncryptedValue(const Section, Key: string; DefaultValue: string): string;
var
  EncryptedValue: string;
begin
  Result := DefaultValue;

  // Récupérer la valeur chiffrée
  EncryptedValue := GetValue(Section, Key, '');

  if EncryptedValue <> '' then
  begin
    try
      // Déchiffrer la valeur
      Result := Decrypt(EncryptedValue);
    except
      // En cas d'erreur, retourner la valeur par défaut
      Result := DefaultValue;
    end;
  end;
end;

procedure TSecureConfig.SetEncryptedValue(const Section, Key, Value: string);
var
  EncryptedValue: string;
begin
  // Chiffrer la valeur
  EncryptedValue := Encrypt(Value);

  // Stocker la valeur chiffrée
  SetValue(Section, Key, EncryptedValue);
end;

end.
```

#### Utilisation du fichier de configuration sécurisé

```pas
procedure TMainForm.SaveConnectionSettings;
var
  Config: TSecureConfig;
begin
  Config := TSecureConfig.Create(
    TPath.Combine(TPath.GetDocumentsPath, 'myapp_config.dat'),
    GetConfigEncryptionKey
  );
  try
    // Stocker les paramètres de connexion
    Config.SetValue('Database', 'Server', EditServer.Text);
    Config.SetValue('Database', 'Database', EditDatabase.Text);
    Config.SetValue('Database', 'Port', EditPort.Text);

    // Stocker les identifiants de manière chiffrée
    Config.SetEncryptedValue('Database', 'Username', EditUsername.Text);
    Config.SetEncryptedValue('Database', 'Password', EditPassword.Text);

    // Sauvegarder la configuration
    Config.Save;

    ShowMessage('Paramètres de connexion enregistrés');
  finally
    Config.Free;
  end;
end;

procedure TMainForm.LoadConnectionSettings;
var
  Config: TSecureConfig;
begin
  Config := TSecureConfig.Create(
    TPath.Combine(TPath.GetDocumentsPath, 'myapp_config.dat'),
    GetConfigEncryptionKey
  );
  try
    // Charger les paramètres de connexion
    EditServer.Text := Config.GetValue('Database', 'Server');
    EditDatabase.Text := Config.GetValue('Database', 'Database');
    EditPort.Text := Config.GetValue('Database', 'Port', '3306');

    // Charger les identifiants chiffrés
    EditUsername.Text := Config.GetEncryptedValue('Database', 'Username');
    EditPassword.Text := Config.GetEncryptedValue('Database', 'Password');
  finally
    Config.Free;
  end;
end;

function TMainForm.GetConfigEncryptionKey: string;
begin
  // Dans une application réelle, utilisez une méthode sécurisée
  // pour obtenir la clé de chiffrement
  Result := 'VotreCleDEncryptionSecrete'; // Ne faites pas cela en production!
end;
```

> ⚠️ **Important** : Dans une application réelle, ne codez jamais en dur vos clés de chiffrement. Utilisez des méthodes comme DPAPI pour protéger votre clé principale.

### 7. Rotation et expiration des identifiants

Pour une sécurité renforcée, il est recommandé de mettre en place une rotation régulière des identifiants et des clés.

```pas
unit CredentialRotation;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils;

type
  TCredentialInfo = record
    Value: string;
    Created: TDateTime;
    Expires: TDateTime;
    IsExpired: Boolean;
  end;

  TCredentialRotationManager = class
  private
    FCredentialManager: TSecureCredentialManager; // Utilisez votre gestionnaire d'identifiants préféré

    function GetCredentialInfo(const CredentialName: string): TCredentialInfo;
    procedure StoreCredentialInfo(const CredentialName: string; const Info: TCredentialInfo);
  public
    constructor Create(CredentialManager: TSecureCredentialManager);

    // Stockage avec date d'expiration
    procedure StoreCredential(const Name, Value: string; ExpiresInDays: Integer = 90);

    // Récupération avec vérification d'expiration
    function RetrieveCredential(const Name: string; out Value: string): Boolean;

    // Vérification de l'expiration
    function IsCredentialExpired(const Name: string): Boolean;

    // Rotation d'identifiants
    function RotateCredential(const Name: string; const NewValue: string): Boolean;

    // Notification pour les identifiants qui vont bientôt expirer
    function GetSoonExpiringCredentials(DaysThreshold: Integer = 7): TArray<string>;
  end;

implementation

uses
  System.JSON;

constructor TCredentialRotationManager.Create(CredentialManager: TSecureCredentialManager);
begin
  inherited Create;
  FCredentialManager := CredentialManager;
end;

function TCredentialRotationManager.GetCredentialInfo(const CredentialName: string): TCredentialInfo;
var
  InfoJson: string;
  JsonObj: TJSONObject;
begin
  // Initialiser avec des valeurs par défaut
  Result.Value := '';
  Result.Created := 0;
  Result.Expires := 0;
  Result.IsExpired := True;

  // Récupérer les informations de l'identifiant
  InfoJson := FCredentialManager.RetrieveCredential(CredentialName + '_INFO',
                                                  ctConnectionString);

  if InfoJson <> '' then
  begin
    JsonObj := TJSONObject.ParseJSONValue(InfoJson) as TJSONObject;
    if JsonObj <> nil then
    try
      Result.Value := FCredentialManager.RetrieveCredential(CredentialName, ctPassword);
      Result.Created := ISO8601ToDate(JsonObj.GetValue<string>('created', ''));
      Result.Expires := ISO8601ToDate(JsonObj.GetValue<string>('expires', ''));
      Result.IsExpired := Now > Result.Expires;
    finally
      JsonObj.Free;
    end;
  end;
end;

procedure TCredentialRotationManager.StoreCredentialInfo(const CredentialName: string;
                                                       const Info: TCredentialInfo);
var
  JsonObj: TJSONObject;
  InfoJson: string;
begin
  // Stocker la valeur de l'identifiant
  FCredentialManager.StoreCredential(CredentialName, Info.Value, ctPassword);

  // Stocker les métadonnées de l'identifiant
  JsonObj := TJSONObject.Create;
  try
    JsonObj.AddPair('created', DateToISO8601(Info.Created));
    JsonObj.AddPair('expires', DateToISO8601(Info.Expires));

    InfoJson := JsonObj.ToString;
  finally
    JsonObj.Free;
  end;

  FCredentialManager.StoreCredential(CredentialName + '_INFO', InfoJson, ctConnectionString);
end;

procedure TCredentialRotationManager.StoreCredential(const Name, Value: string;
                                                   ExpiresInDays: Integer);
var
  Info: TCredentialInfo;
begin
  // Créer les informations de l'identifiant
  Info.Value := Value;
  Info.Created := Now;
  Info.Expires := IncDay(Now, ExpiresInDays);
  Info.IsExpired := False;

  // Stocker l'identifiant avec ses métadonnées
  StoreCredentialInfo(Name, Info);
end;

function TCredentialRotationManager.RetrieveCredential(const Name: string;
                                                     out Value: string): Boolean;
var
  Info: TCredentialInfo;
begin
  Info := GetCredentialInfo(Name);

  // Vérifier si l'identifiant existe et n'est pas expiré
  Result := (Info.Value <> '') and not Info.IsExpired;

  if Result then
    Value := Info.Value
  else
    Value := '';
end;

function TCredentialRotationManager.IsCredentialExpired(const Name: string): Boolean;
var
  Info: TCredentialInfo;
begin
  Info := GetCredentialInfo(Name);
  Result := Info.IsExpired;
end;

function TCredentialRotationManager.RotateCredential(const Name: string;
                                                   const NewValue: string): Boolean;
var
  Info: TCredentialInfo;
begin
  Info := GetCredentialInfo(Name);

  // Vérifier si l'identifiant existe
  Result := Info.Value <> '';

  if Result then
  begin
    // Mettre à jour la valeur et réinitialiser les dates
    Info.Value := NewValue;
    Info.Created := Now;
    Info.Expires := IncDay(Now, DaysBetween(Info.Created, Info.Expires));
    Info.IsExpired := False;

    // Stocker le nouvel identifiant
    StoreCredentialInfo(Name, Info);
  end;
end;

function TCredentialRotationManager.GetSoonExpiringCredentials(DaysThreshold: Integer): TArray<string>;
var
  ExpiringList: TList<string>;
  ThresholdDate: TDateTime;
  Credential: string;
  Info: TCredentialInfo;
begin
  ExpiringList := TList<string>.Create;
  try
    ThresholdDate := IncDay(Now, DaysThreshold);

    // Parcourir tous les identifiants (vous devrez adapter cette partie)
    for Credential in GetAllCredentialNames do
    begin
      Info := GetCredentialInfo(Credential);

      // Vérifier si l'identifiant expire bientôt
      if (not Info.IsExpired) and (Info.Expires <= ThresholdDate) then
        ExpiringList.Add(Credential);
    end;

    Result := ExpiringList.ToArray;
  finally
    ExpiringList.Free;
  end;
end;

end.
```

#### Mise en place de notifications d'expiration

```pas
procedure TMainForm.CheckCredentialExpirations;
var
  RotationManager: TCredentialRotationManager;
  ExpiringCredentials: TArray<string>;
  CredentialName: string;
begin
  RotationManager := TCredentialRotationManager.Create(FCredentialManager);
  try
    // Vérifier les identifiants qui expirent dans les 7 prochains jours
    ExpiringCredentials := RotationManager.GetSoonExpiringCredentials(7);

    if Length(ExpiringCredentials) > 0 then
    begin
      // Créer un message d'alerte
      var Message := 'Les identifiants suivants vont bientôt expirer :' + sLineBreak + sLineBreak;

      for CredentialName in ExpiringCredentials do
        Message := Message + '- ' + CredentialName + sLineBreak;

      Message := Message + sLineBreak + 'Veuillez les mettre à jour dès que possible.';

      // Afficher l'alerte
      ShowNotification('Identifiants expirant bientôt', Message);
    end;
  finally
    RotationManager.Free;
  end;
end;
```

### 8. Bibliothèques tierces pour la gestion des identifiants

Plusieurs bibliothèques tierces peuvent vous aider à gérer les identifiants de manière sécurisée dans vos applications Delphi :

1. **SecureBlackbox** : Une suite complète de composants de sécurité pour Delphi, comprenant des fonctionnalités de chiffrement et de stockage sécurisé.

2. **TurboPower LockBox** : Une bibliothèque de cryptographie open source pour Delphi.

3. **Spring4D Cryptography** : Partie de la bibliothèque Spring4D, elle offre des fonctionnalités cryptographiques modernes.

4. **Delphi Encryption Compendium (DEC)** : Une collection d'algorithmes cryptographiques pour Delphi.

### Meilleures pratiques pour le stockage des identifiants

1. **Ne stockez jamais les identifiants en texte clair** dans votre code, dans des fichiers ou dans des bases de données.

2. **Utilisez des APIs de système** comme DPAPI sous Windows lorsque c'est possible.

3. **Chiffrez toujours les identifiants sensibles** avant de les stocker.

4. **Ne réutilisez pas les clés de chiffrement** pour différents types de données.

5. **Implémentez la rotation et l'expiration** des identifiants pour limiter les risques en cas de compromission.

6. **Limitez l'accès aux identifiants stockés** en utilisant des contrôles d'accès appropriés.

7. **Évitez de stocker les identifiants** si possible, en les demandant à l'utilisateur lorsqu'ils sont nécessaires.

8. **Effacez les identifiants de la mémoire** dès qu'ils ne sont plus nécessaires.

9. **Ne journalisez jamais les identifiants** dans les fichiers de log.

10. **Utilisez des algorithmes de cryptographie standards** et évitez de créer vos propres algorithmes.

### Gérer les clés de chiffrement

La sécurité de vos identifiants chiffrés dépend de la sécurité de vos clés de chiffrement. Voici quelques approches pour gérer ces clés :

#### 1. Dérivation de clé basée sur un mot de passe

```pas
function DeriveKeyFromPassword(const Password, Salt: string; Iterations: Integer = 10000): TBytes;
var
  // Utiliser PBKDF2 pour dériver une clé à partir du mot de passe
  // Ceci est une pseudo-implémentation, utilisez une bibliothèque cryptographique réelle
  KeyMaterial: TBytes;
  I: Integer;
begin
  // Initialiser avec le mot de passe et le sel
  KeyMaterial := TEncoding.UTF8.GetBytes(Password + Salt);

  // Appliquer des itérations pour renforcer contre les attaques par force brute
  for I := 1 to Iterations do
    KeyMaterial := THashSHA2.GetHashBytes(KeyMaterial);

  Result := KeyMaterial;
end;
```

#### 2. Utiliser une phrase secrète demandée à l'utilisateur

```pas
function GetEncryptionKey: TBytes;
var
  Passphrase: string;
  Salt: string;
begin
  // Demander la phrase secrète à l'utilisateur au démarrage
  if not GetPassphrase(Passphrase) then
    raise Exception.Create('Phrase secrète requise pour déverrouiller les identifiants');

  // Utiliser un sel spécifique à l'application
  Salt := 'MyAppSpecificSalt';

  // Dériver la clé
  Result := DeriveKeyFromPassword(Passphrase, Salt, 10000);
end;

function GetPassphrase(out Passphrase: string): Boolean;
var
  PassphraseForm: TPassphraseForm;
begin
  PassphraseForm := TPassphraseForm.Create(nil);
  try
    Result := PassphraseForm.ShowModal = mrOk;
    if Result then
      Passphrase := PassphraseForm.EditPassphrase.Text;
  finally
    PassphraseForm.Free;
  end;
end;
```

#### 3. Utiliser une clé matérielle (comme une clé USB)

```pas
function GetKeyFromHardwareToken: TBytes;
var
  USBDrives: TStringDynArray;
  KeyFile: string;
  I: Integer;
begin
  // Rechercher les lecteurs USB
  USBDrives := GetUSBDrives; // À implémenter selon votre besoin

  for I := 0 to High(USBDrives) do
  begin
    KeyFile := TPath.Combine(USBDrives[I], 'myapp_key.dat');

    if FileExists(KeyFile) then
    begin
      try
        // Lire la clé depuis le fichier
        Result := TFile.ReadAllBytes(KeyFile);
        Exit;
      except
        // Continuer avec le prochain lecteur en cas d'erreur
      end;
    end;
  end;

  // Aucune clé trouvée
  raise Exception.Create('Clé matérielle non trouvée. Veuillez insérer la clé USB appropriée.');
end;
```

### Conclusion

Le stockage sécurisé des identifiants est une composante critique de la sécurité des applications. En utilisant les techniques appropriées, vous pouvez protéger efficacement les informations sensibles contre les accès non autorisés.

Dans ce chapitre, nous avons exploré différentes approches, des plus simples aux plus avancées, pour stocker et gérer les identifiants dans vos applications Delphi. Choisissez la méthode qui correspond le mieux à vos besoins de sécurité et aux exigences de votre application.

Rappelez-vous que la sécurité est un processus continu et qu'aucune solution n'est parfaite. Combinez ces techniques avec d'autres mesures de sécurité (comme celles présentées dans les chapitres précédents) pour une protection optimale.

### Exercices pratiques

1. **Créez une implémentation simple** de `TSecureCredentialManager` utilisant DPAPI pour stocker et récupérer une clé API.

2. **Modifiez une application existante** qui stocke des identifiants en texte clair pour utiliser une des méthodes de stockage sécurisé présentées dans ce chapitre.

3. **Créez un formulaire de gestion des identifiants** permettant à l'utilisateur de visualiser, modifier et supprimer ses identifiants stockés, avec les contrôles d'accès appropriés.

4. **Implémentez la rotation automatique des identifiants** pour une application qui se connecte à une API externe, avec des notifications lorsque les identifiants sont sur le point d'expirer.

5. **Comparez les performances** des différentes méthodes de stockage des identifiants en mesurant le temps nécessaire pour stocker et récupérer un grand nombre d'identifiants.

6. **Pour les plus avancés** : Créez un système de gestion des identifiants multi-utilisateurs où chaque utilisateur ne peut accéder qu'à ses propres identifiants, même s'ils sont stockés dans le même fichier ou base de données.

7. **Pour les plus avancés** : Développez une extension pour l'IDE Delphi qui permet de détecter et de sécuriser automatiquement les identifiants en texte clair dans le code.

### Application pratique : Un gestionnaire de mots de passe simple

Voici un exemple complet d'une application de gestion de mots de passe simple qui met en pratique les concepts présentés dans ce chapitre :

```pas
program SimplePasswordManager;

{$APPTYPE GUI}

uses
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Grids,
  System.Hash,
  System.UITypes,
  SecureCredentials in 'SecureCredentials.pas';

type
  TMainForm = class(TForm)
    PanelTop: TPanel;
    PanelBottom: TPanel;
    StringGridCredentials: TStringGrid;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    ButtonExit: TButton;
    LabelMasterPassword: TLabel;
    EditMasterPassword: TEdit;
    ButtonUnlock: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonUnlockClick(Sender: TObject);
    procedure UpdateGrid;
  private
    FCredentialManager: TSecureCredentialManager;
    FMasterPasswordHash: string;
    FIsUnlocked: Boolean;
    procedure SetupGrid;
    procedure UnlockApp(const MasterPassword: string);
    procedure LockApp;
    procedure LoadCredentials;
    procedure SaveCredentials;
    function ShowCredentialDialog(const Title: string; var Name, Username, Password: string): Boolean;
  end;

var
  MainForm: TMainForm;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser l'interface
  Caption := 'Gestionnaire de Mots de Passe';
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
  Width := 600;
  Height := 400;

  // Configurer le panneau supérieur
  PanelTop.Align := alTop;
  PanelTop.Height := 50;
  PanelTop.BevelOuter := bvNone;
  PanelTop.ParentBackground := False;
  PanelTop.ParentColor := False;
  PanelTop.Color := clWhite;

  // Configurer le panneau inférieur
  PanelBottom.Align := alBottom;
  PanelBottom.Height := 50;
  PanelBottom.BevelOuter := bvNone;

  // Configurer la grille
  StringGridCredentials.Align := alClient;
  StringGridCredentials.DefaultRowHeight := 22;
  StringGridCredentials.RowCount := 1; // Juste l'en-tête au début
  StringGridCredentials.Options := [goFixedVertLine, goFixedHorzLine,
                                   goVertLine, goHorzLine,
                                   goRowSelect, goThumbTracking];
  StringGridCredentials.ColCount := 3;
  StringGridCredentials.Cells[0, 0] := 'Site / Application';
  StringGridCredentials.Cells[1, 0] := 'Nom d''utilisateur';
  StringGridCredentials.Cells[2, 0] := 'Mot de passe';
  StringGridCredentials.ColWidths[0] := 200;
  StringGridCredentials.ColWidths[1] := 200;
  StringGridCredentials.ColWidths[2] := 180;

  // Configurer les boutons
  ButtonAdd.Parent := PanelBottom;
  ButtonAdd.Caption := 'Ajouter';
  ButtonAdd.Left := 10;
  ButtonAdd.Top := 10;
  ButtonAdd.Width := 100;
  ButtonAdd.Enabled := False;

  ButtonEdit.Parent := PanelBottom;
  ButtonEdit.Caption := 'Modifier';
  ButtonEdit.Left := 120;
  ButtonEdit.Top := 10;
  ButtonEdit.Width := 100;
  ButtonEdit.Enabled := False;

  ButtonDelete.Parent := PanelBottom;
  ButtonDelete.Caption := 'Supprimer';
  ButtonDelete.Left := 230;
  ButtonDelete.Top := 10;
  ButtonDelete.Width := 100;
  ButtonDelete.Enabled := False;

  ButtonExit.Parent := PanelBottom;
  ButtonExit.Caption := 'Quitter';
  ButtonExit.Left := 490;
  ButtonExit.Top := 10;
  ButtonExit.Width := 100;

  // Configurer le contrôle de mot de passe maître
  LabelMasterPassword.Parent := PanelTop;
  LabelMasterPassword.Caption := 'Mot de passe maître:';
  LabelMasterPassword.Left := 10;
  LabelMasterPassword.Top := 15;

  EditMasterPassword.Parent := PanelTop;
  EditMasterPassword.Left := 120;
  EditMasterPassword.Top := 12;
  EditMasterPassword.Width := 200;
  EditMasterPassword.PasswordChar := '•';

  ButtonUnlock.Parent := PanelTop;
  ButtonUnlock.Caption := 'Déverrouiller';
  ButtonUnlock.Left := 330;
  ButtonUnlock.Top := 11;
  ButtonUnlock.Width := 100;

  // Charger le hash du mot de passe maître (dans une application réelle, cela serait stocké de manière sécurisée)
  var AppDataPath := TPath.Combine(TPath.GetDocumentsPath, 'PasswordManager');

  if not DirectoryExists(AppDataPath) then
    ForceDirectories(AppDataPath);

  var HashFile := TPath.Combine(AppDataPath, 'master.hash');

  if FileExists(HashFile) then
    FMasterPasswordHash := TFile.ReadAllText(HashFile)
  else
  begin
    // Premier lancement, demander de créer un mot de passe maître
    var NewPass := InputBox('Configuration', 'Créez un mot de passe maître:', '');

    if NewPass = '' then
    begin
      ShowMessage('Un mot de passe maître est requis.');
      Application.Terminate;
      Exit;
    end;

    // Stocker le hash du mot de passe
    FMasterPasswordHash := THashSHA2.GetHashString(NewPass);
    TFile.WriteAllText(HashFile, FMasterPasswordHash);
  end;

  // Initialiser les autres variables
  FIsUnlocked := False;

  // Configurer l'accès à la grille
  StringGridCredentials.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCredentialManager.Free;
end;

procedure TMainForm.ButtonUnlockClick(Sender: TObject);
begin
  UnlockApp(EditMasterPassword.Text);
end;

procedure TMainForm.UnlockApp(const MasterPassword: string);
begin
  // Vérifier le mot de passe maître
  if THashSHA2.GetHashString(MasterPassword) = FMasterPasswordHash then
  begin
    FIsUnlocked := True;

    // Initialiser le gestionnaire d'identifiants
    FCredentialManager := TSecureCredentialManager.Create('PasswordManager');

    // Activer les contrôles
    ButtonAdd.Enabled := True;
    ButtonEdit.Enabled := True;
    ButtonDelete.Enabled := True;
    StringGridCredentials.Enabled := True;

    // Désactiver les contrôles de déverrouillage
    LabelMasterPassword.Enabled := False;
    EditMasterPassword.Enabled := False;
    ButtonUnlock.Enabled := False;

    // Changer le texte du bouton de déverrouillage
    ButtonUnlock.Caption := 'Verrouiller';
    ButtonUnlock.OnClick := procedure(Sender: TObject)
    begin
      LockApp;
    end;

    // Charger les identifiants
    LoadCredentials;

    ShowMessage('Application déverrouillée !');
  end
  else
    ShowMessage('Mot de passe incorrect.');
end;

procedure TMainForm.LockApp;
begin
  FIsUnlocked := False;

  // Libérer le gestionnaire d'identifiants
  FreeAndNil(FCredentialManager);

  // Désactiver les contrôles
  ButtonAdd.Enabled := False;
  ButtonEdit.Enabled := False;
  ButtonDelete.Enabled := False;
  StringGridCredentials.Enabled := False;

  // Réactiver les contrôles de déverrouillage
  LabelMasterPassword.Enabled := True;
  EditMasterPassword.Enabled := True;
  ButtonUnlock.Enabled := True;

  // Changer le texte du bouton de déverrouillage
  ButtonUnlock.Caption := 'Déverrouiller';
  ButtonUnlock.OnClick := ButtonUnlockClick;

  // Effacer le mot de passe
  EditMasterPassword.Text := '';

  // Effacer la grille
  StringGridCredentials.RowCount := 1;

  ShowMessage('Application verrouillée.');
end;

procedure TMainForm.LoadCredentials;
var
  CredentialList: TStringList;
  I: Integer;
  Name, Username, Password: string;
begin
  // Dans une application réelle, vous utiliseriez une méthode plus sophistiquée
  // pour récupérer la liste des identifiants
  CredentialList := TStringList.Create;
  try
    try
      var ListPath := TPath.Combine(TPath.GetDocumentsPath, 'PasswordManager', 'credential_list.txt');

      if FileExists(ListPath) then
      begin
        CredentialList.LoadFromFile(ListPath);

        // Configurer la grille
        StringGridCredentials.RowCount := CredentialList.Count + 1; // +1 pour l'en-tête

        // Remplir la grille
        for I := 0 to CredentialList.Count - 1 do
        begin
          Name := CredentialList[I];
          Username := FCredentialManager.RetrieveCredential(Name + '_username', ctPassword);
          Password := FCredentialManager.RetrieveCredential(Name + '_password', ctPassword);

          StringGridCredentials.Cells[0, I + 1] := Name;
          StringGridCredentials.Cells[1, I + 1] := Username;
          StringGridCredentials.Cells[2, I + 1] := StringOfChar('*', Length(Password));
        end;
      end
      else
        StringGridCredentials.RowCount := 1; // Juste l'en-tête
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement des identifiants : ' + E.Message);
    end;
  finally
    CredentialList.Free;
  end;
end;

procedure TMainForm.SaveCredentials;
var
  CredentialList: TStringList;
  I: Integer;
begin
  // Sauvegarder la liste des noms d'identifiants
  CredentialList := TStringList.Create;
  try
    for I := 1 to StringGridCredentials.RowCount - 1 do
      CredentialList.Add(StringGridCredentials.Cells[0, I]);

    var ListPath := TPath.Combine(TPath.GetDocumentsPath, 'PasswordManager', 'credential_list.txt');
    CredentialList.SaveToFile(ListPath);
  finally
    CredentialList.Free;
  end;
end;

function TMainForm.ShowCredentialDialog(const Title: string; var Name, Username, Password: string): Boolean;
var
  Form: TForm;
  LabelName, LabelUsername, LabelPassword: TLabel;
  EditName, EditUsername, EditPassword: TEdit;
  ButtonOK, ButtonCancel: TButton;
begin
  Result := False;

  // Créer le formulaire
  Form := TForm.Create(nil);
  try
    Form.Caption := Title;
    Form.Position := poScreenCenter;
    Form.BorderStyle := bsDialog;
    Form.Width := 400;
    Form.Height := 200;

    // Labels
    LabelName := TLabel.Create(Form);
    LabelName.Parent := Form;
    LabelName.Caption := 'Site / Application:';
    LabelName.Left := 20;
    LabelName.Top := 20;

    LabelUsername := TLabel.Create(Form);
    LabelUsername.Parent := Form;
    LabelUsername.Caption := 'Nom d''utilisateur:';
    LabelUsername.Left := 20;
    LabelUsername.Top := 50;

    LabelPassword := TLabel.Create(Form);
    LabelPassword.Parent := Form;
    LabelPassword.Caption := 'Mot de passe:';
    LabelPassword.Left := 20;
    LabelPassword.Top := 80;

    // Edits
    EditName := TEdit.Create(Form);
    EditName.Parent := Form;
    EditName.Left := 150;
    EditName.Top := 20;
    EditName.Width := 220;
    EditName.Text := Name;

    EditUsername := TEdit.Create(Form);
    EditUsername.Parent := Form;
    EditUsername.Left := 150;
    EditUsername.Top := 50;
    EditUsername.Width := 220;
    EditUsername.Text := Username;

    EditPassword := TEdit.Create(Form);
    EditPassword.Parent := Form;
    EditPassword.Left := 150;
    EditPassword.Top := 80;
    EditPassword.Width := 220;
    EditPassword.Text := Password;
    EditPassword.PasswordChar := '•';

    // Buttons
    ButtonOK := TButton.Create(Form);
    ButtonOK.Parent := Form;
    ButtonOK.Caption := 'OK';
    ButtonOK.Left := 210;
    ButtonOK.Top := 130;
    ButtonOK.Width := 75;
    ButtonOK.Default := True;
    ButtonOK.ModalResult := mrOk;

    ButtonCancel := TButton.Create(Form);
    ButtonCancel.Parent := Form;
    ButtonCancel.Caption := 'Annuler';
    ButtonCancel.Left := 295;
    ButtonCancel.Top := 130;
    ButtonCancel.Width := 75;
    ButtonCancel.Cancel := True;
    ButtonCancel.ModalResult := mrCancel;

    // Afficher le dialogue
    if Form.ShowModal = mrOk then
    begin
      // Validation
      if (EditName.Text = '') or (EditUsername.Text = '') or (EditPassword.Text = '') then
      begin
        ShowMessage('Tous les champs sont obligatoires.');
        Exit;
      end;

      Name := EditName.Text;
      Username := EditUsername.Text;
      Password := EditPassword.Text;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

procedure TMainForm.ButtonAddClick(Sender: TObject);
var
  Name, Username, Password: string;
begin
  Name := '';
  Username := '';
  Password := '';

  if ShowCredentialDialog('Ajouter un identifiant', Name, Username, Password) then
  begin
    // Vérifier si le nom existe déjà
    for var I := 1 to StringGridCredentials.RowCount - 1 do
    begin
      if StringGridCredentials.Cells[0, I] = Name then
      begin
        ShowMessage('Un identifiant avec ce nom existe déjà.');
        Exit;
      end;
    end;

    // Stocker l'identifiant
    FCredentialManager.StoreCredential(Name + '_username', Username, ctPassword);
    FCredentialManager.StoreCredential(Name + '_password', Password, ctPassword);

    // Ajouter à la grille
    StringGridCredentials.RowCount := StringGridCredentials.RowCount + 1;
    var NewRow := StringGridCredentials.RowCount - 1;

    StringGridCredentials.Cells[0, NewRow] := Name;
    StringGridCredentials.Cells[1, NewRow] := Username;
    StringGridCredentials.Cells[2, NewRow] := StringOfChar('*', Length(Password));

    // Sauvegarder la liste
    SaveCredentials;

    ShowMessage('Identifiant ajouté avec succès.');
  end;
end;

procedure TMainForm.ButtonEditClick(Sender: TObject);
var
  SelectedRow: Integer;
  Name, Username, Password: string;
begin
  SelectedRow := StringGridCredentials.Row;

  // Vérifier qu'une ligne est sélectionnée
  if (SelectedRow <= 0) or (SelectedRow >= StringGridCredentials.RowCount) then
  begin
    ShowMessage('Veuillez sélectionner un identifiant à modifier.');
    Exit;
  end;

  // Récupérer les informations actuelles
  Name := StringGridCredentials.Cells[0, SelectedRow];
  Username := FCredentialManager.RetrieveCredential(Name + '_username', ctPassword);
  Password := FCredentialManager.RetrieveCredential(Name + '_password', ctPassword);

  // Afficher le dialogue d'édition
  if ShowCredentialDialog('Modifier l''identifiant', Name, Username, Password) then
  begin
    // Vérifier si le nouveau nom n'existe pas déjà (sauf s'il s'agit du même)
    for var I := 1 to StringGridCredentials.RowCount - 1 do
    begin
      if (I <> SelectedRow) and (StringGridCredentials.Cells[0, I] = Name) then
      begin
        ShowMessage('Un identifiant avec ce nom existe déjà.');
        Exit;
      end;
    end;

    // Supprimer l'ancien identifiant si le nom a changé
    var OldName := StringGridCredentials.Cells[0, SelectedRow];
    if OldName <> Name then
    begin
      FCredentialManager.DeleteCredential(OldName + '_username', ctPassword);
      FCredentialManager.DeleteCredential(OldName + '_password', ctPassword);
    end;

    // Stocker le nouvel identifiant
    FCredentialManager.StoreCredential(Name + '_username', Username, ctPassword);
    FCredentialManager.StoreCredential(Name + '_password', Password, ctPassword);

    // Mettre à jour la grille
    StringGridCredentials.Cells[0, SelectedRow] := Name;
    StringGridCredentials.Cells[1, SelectedRow] := Username;
    StringGridCredentials.Cells[2, SelectedRow] := StringOfChar('*', Length(Password));

    // Sauvegarder la liste
    SaveCredentials;

    ShowMessage('Identifiant modifié avec succès.');
  end;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
  SelectedRow: Integer;
  Name: string;
begin
  SelectedRow := StringGridCredentials.Row;

  // Vérifier qu'une ligne est sélectionnée
  if (SelectedRow <= 0) or (SelectedRow >= StringGridCredentials.RowCount) then
  begin
    ShowMessage('Veuillez sélectionner un identifiant à supprimer.');
    Exit;
  end;

  Name := StringGridCredentials.Cells[0, SelectedRow];

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer l''identifiant "' + Name + '" ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Supprimer l'identifiant
    FCredentialManager.DeleteCredential(Name + '_username', ctPassword);
    FCredentialManager.DeleteCredential(Name + '_password', ctPassword);

    // Supprimer de la grille
    for var I := SelectedRow to StringGridCredentials.RowCount - 2 do
    begin
      StringGridCredentials.Cells[0, I] := StringGridCredentials.Cells[0, I + 1];
      StringGridCredentials.Cells[1, I] := StringGridCredentials.Cells[1, I + 1];
      StringGridCredentials.Cells[2, I] := StringGridCredentials.Cells[2, I + 1];
    end;

    StringGridCredentials.RowCount := StringGridCredentials.RowCount - 1;

    // Sauvegarder la liste
    SaveCredentials;

    ShowMessage('Identifiant supprimé avec succès.');
  end;
end;

procedure TMainForm.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.UpdateGrid;
begin
  // Mettre à jour les colonnes
  StringGridCredentials.ColWidths[0] := 200;
  StringGridCredentials.ColWidths[1] := 200;
  StringGridCredentials.ColWidths[2] := 180;
end;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

Cette application illustre une façon simple de gérer des identifiants de manière sécurisée. Dans une application réelle, vous utiliseriez des techniques de chiffrement plus avancées et des contrôles de sécurité supplémentaires.

En mettant en pratique les concepts présentés dans ce chapitre, vous pourrez garantir que les informations sensibles de vos utilisateurs sont protégées de manière adéquate, contribuant ainsi à la sécurité globale de vos applications Delphi.

⏭️ [GDPR et confidentialité des données](16-securite-des-applications/08-gdpr-et-confidentialite-des-donnees.md)


