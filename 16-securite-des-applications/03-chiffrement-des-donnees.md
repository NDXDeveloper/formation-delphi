# 16. Sécurité des applications
## 16.3 Chiffrement des données

Le chiffrement est une technique qui permet de protéger vos données en les rendant illisibles sans la clé appropriée. Dans ce chapitre, nous allons explorer comment chiffrer et déchiffrer des données sensibles dans vos applications Delphi.

### Pourquoi chiffrer les données ?

Même avec une authentification et une autorisation solides, il reste important de protéger les données sensibles par chiffrement pour plusieurs raisons :

1. **Protection contre les accès non autorisés à la base de données** : Si quelqu'un obtient un accès direct à votre base de données (en contournant votre application), les données chiffrées resteront protégées.

2. **Protection des données au repos** : Les données stockées sur le disque dur, dans une base de données ou dans des fichiers de configuration sont vulnérables si elles ne sont pas chiffrées.

3. **Protection des données en transit** : Les informations envoyées sur un réseau peuvent être interceptées si elles ne sont pas chiffrées.

4. **Conformité réglementaire** : De nombreuses réglementations comme le RGPD (GDPR) exigent que les données personnelles soient protégées par des mesures techniques appropriées, dont le chiffrement.

### Types de chiffrement

Il existe deux principaux types de chiffrement :

1. **Chiffrement symétrique** : Une seule clé est utilisée à la fois pour chiffrer et déchiffrer les données. C'est rapide mais nécessite de partager la clé secrète de manière sécurisée.

2. **Chiffrement asymétrique** : Utilise une paire de clés - une clé publique pour chiffrer et une clé privée pour déchiffrer. C'est plus sécurisé pour l'échange de clés mais plus lent que le chiffrement symétrique.

### Chiffrement dans Delphi

Delphi propose plusieurs options pour implémenter le chiffrement :

1. **Bibliothèques intégrées** : Delphi inclut des unités comme `System.Hash` et `System.NetEncoding` qui fournissent des fonctions de base.

2. **API de chiffrement Windows** : Delphi peut accéder aux fonctions de chiffrement du système d'exploitation.

3. **Bibliothèques tierces** : Il existe plusieurs bibliothèques de chiffrement disponibles pour Delphi, comme DCPcrypt, LockBox, CryptoLib4Pascal, etc.

Nous allons nous concentrer sur les solutions intégrées à Delphi et quelques approches simples à mettre en œuvre.

### Chiffrement symétrique simple

#### 1. Utilisation de l'API TEACipher (Tiny Encryption Algorithm)

Cette méthode est disponible dans Delphi via le module `DCPcrypt`. Voici un exemple simple :

```pas
unit SimpleEncryption;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding,
  DCPtea, DCPsha256, DCPcrypt2;

type
  TCryptoHelper = class
  public
    class function EncryptString(const PlainText, Password: string): string;
    class function DecryptString(const EncryptedText, Password: string): string;
  end;

implementation

{ TCryptoHelper }

class function TCryptoHelper.EncryptString(const PlainText, Password: string): string;
var
  Cipher: TDCP_tea;
  SourceStream, DestStream: TMemoryStream;
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
begin
  Result := '';

  SourceStream := TMemoryStream.Create;
  DestStream := TMemoryStream.Create;
  Cipher := TDCP_tea.Create(nil);
  Hash := TDCP_sha256.Create(nil);
  try
    // Convertir la chaîne en bytes et l'écrire dans le SourceStream
    SourceStream.WriteBuffer(PChar(PlainText)^, Length(PlainText) * SizeOf(Char));
    SourceStream.Position := 0;

    // Créer un hash du mot de passe pour la clé de chiffrement
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(Digest);

    // Initialiser le chiffrement avec la clé
    Cipher.Init(Digest, Sizeof(Digest) * 8, nil);

    // Chiffrer les données
    Cipher.EncryptStream(SourceStream, DestStream, SourceStream.Size);
    DestStream.Position := 0;

    // Convertir le résultat en Base64 pour le stockage
    SetLength(Result, DestStream.Size);
    DestStream.ReadBuffer(Result[1], DestStream.Size);
    Result := TNetEncoding.Base64.Encode(Result);
  finally
    Cipher.Free;
    Hash.Free;
    SourceStream.Free;
    DestStream.Free;
  end;
end;

class function TCryptoHelper.DecryptString(const EncryptedText, Password: string): string;
var
  Cipher: TDCP_tea;
  SourceStream, DestStream: TMemoryStream;
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
  DecodedStr: string;
  Size: Integer;
begin
  Result := '';

  // Décoder le texte Base64
  DecodedStr := TNetEncoding.Base64.Decode(EncryptedText);

  SourceStream := TMemoryStream.Create;
  DestStream := TMemoryStream.Create;
  Cipher := TDCP_tea.Create(nil);
  Hash := TDCP_sha256.Create(nil);
  try
    // Écrire les données décodées dans le SourceStream
    SourceStream.WriteBuffer(DecodedStr[1], Length(DecodedStr));
    SourceStream.Position := 0;

    // Créer un hash du mot de passe pour la clé de déchiffrement
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(Digest);

    // Initialiser le déchiffrement avec la clé
    Cipher.Init(Digest, Sizeof(Digest) * 8, nil);

    // Déchiffrer les données
    Cipher.DecryptStream(SourceStream, DestStream, SourceStream.Size);
    DestStream.Position := 0;

    // Convertir le résultat en chaîne
    Size := DestStream.Size div SizeOf(Char);
    SetLength(Result, Size);
    DestStream.ReadBuffer(Result[1], Size * SizeOf(Char));
  finally
    Cipher.Free;
    Hash.Free;
    SourceStream.Free;
    DestStream.Free;
  end;
end;

end.
```

> ⚠️ **Note** : Pour utiliser ce code, vous devez installer la bibliothèque DCPcrypt, qui peut être obtenue via GetIt Package Manager ou sur GitHub.

#### 2. Utilisation de l'API intégrée (pour Delphi 12 ou supérieur)

Dans les versions récentes de Delphi, vous pouvez utiliser les API intégrées :

```pas
// Nécessite Delphi 12 ou supérieur
unit ModernEncryption;

interface

uses
  System.SysUtils, System.NetEncoding, System.Generics.Collections, System.Hash,
  System.Crypto, System.IOUtils;

type
  TModernCrypto = class
  public
    class function EncryptString(const PlainText, Password: string): string;
    class function DecryptString(const EncryptedText, Password: string): string;

    class function EncryptFile(const SourceFile, DestFile, Password: string): Boolean;
    class function DecryptFile(const SourceFile, DestFile, Password: string): Boolean;
  end;

implementation

{ TModernCrypto }

class function TModernCrypto.EncryptString(const PlainText, Password: string): string;
var
  Key: TBytes;
  IV: TBytes;
  EncryptedBytes: TBytes;
begin
  // Créer une clé forte à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);

  // Générer un vecteur d'initialisation aléatoire
  SetLength(IV, 16);
  TCrypto.RandomBytes(IV);

  // Chiffrer les données
  EncryptedBytes := TCipher.AES.Encrypt(
    TEncoding.UTF8.GetBytes(PlainText),
    Key,
    IV
  );

  // Combiner IV et données chiffrées (IV doit être connu pour le déchiffrement)
  // et encoder en Base64 pour un stockage facile
  Result := TNetEncoding.Base64.EncodeBytesToString(IV) + '.' +
            TNetEncoding.Base64.EncodeBytesToString(EncryptedBytes);
end;

class function TModernCrypto.DecryptString(const EncryptedText, Password: string): string;
var
  Key: TBytes;
  IV, EncryptedBytes, DecryptedBytes: TBytes;
  Parts: TArray<string>;
begin
  Result := '';

  // Séparer IV et données chiffrées
  Parts := EncryptedText.Split(['.']);
  if Length(Parts) <> 2 then
    raise Exception.Create('Format de texte chiffré invalide');

  // Décoder de Base64
  IV := TNetEncoding.Base64.DecodeStringToBytes(Parts[0]);
  EncryptedBytes := TNetEncoding.Base64.DecodeStringToBytes(Parts[1]);

  // Recréer la clé à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);

  // Déchiffrer les données
  try
    DecryptedBytes := TCipher.AES.Decrypt(EncryptedBytes, Key, IV);
    Result := TEncoding.UTF8.GetString(DecryptedBytes);
  except
    on E: Exception do
      raise Exception.Create('Erreur de déchiffrement : ' + E.Message);
  end;
end;

class function TModernCrypto.EncryptFile(const SourceFile, DestFile, Password: string): Boolean;
var
  Key: TBytes;
  IV: TBytes;
  SourceBytes, EncryptedBytes: TBytes;
  HeaderBytes: TBytes;
begin
  Result := False;

  try
    // Lire le fichier source
    SourceBytes := TFile.ReadAllBytes(SourceFile);

    // Créer une clé forte à partir du mot de passe
    Key := THashSHA2.GetHashBytes(Password);

    // Générer un vecteur d'initialisation aléatoire
    SetLength(IV, 16);
    TCrypto.RandomBytes(IV);

    // Chiffrer les données
    EncryptedBytes := TCipher.AES.Encrypt(SourceBytes, Key, IV);

    // Préparer l'en-tête : une signature + IV
    // La signature permet de vérifier que c'est bien un fichier chiffré par notre application
    HeaderBytes := TEncoding.UTF8.GetBytes('DELPHIENC');

    // Écrire l'en-tête, l'IV et les données chiffrées dans le fichier de destination
    TFile.WriteAllBytes(DestFile, HeaderBytes + IV + EncryptedBytes);

    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors du chiffrement du fichier : ' + E.Message);
  end;
end;

class function TModernCrypto.DecryptFile(const SourceFile, DestFile, Password: string): Boolean;
var
  Key: TBytes;
  IV: TBytes;
  SourceBytes, DecryptedBytes: TBytes;
  HeaderBytes: TBytes;
  Signature: string;
begin
  Result := False;

  try
    // Lire le fichier source
    SourceBytes := TFile.ReadAllBytes(SourceFile);

    // Vérifier la signature (les 9 premiers octets)
    if Length(SourceBytes) < 25 then // 9 (signature) + 16 (IV)
      raise Exception.Create('Fichier trop petit pour être un fichier chiffré valide');

    SetLength(HeaderBytes, 9);
    Move(SourceBytes[0], HeaderBytes[0], 9);
    Signature := TEncoding.UTF8.GetString(HeaderBytes);

    if Signature <> 'DELPHIENC' then
      raise Exception.Create('Ce fichier n''est pas un fichier chiffré valide');

    // Extraire l'IV (les 16 octets suivants)
    SetLength(IV, 16);
    Move(SourceBytes[9], IV[0], 16);

    // Créer une clé forte à partir du mot de passe
    Key := THashSHA2.GetHashBytes(Password);

    // Extraire les données chiffrées (le reste du fichier)
    SetLength(EncryptedBytes, Length(SourceBytes) - 25);
    Move(SourceBytes[25], EncryptedBytes[0], Length(SourceBytes) - 25);

    // Déchiffrer les données
    DecryptedBytes := TCipher.AES.Decrypt(EncryptedBytes, Key, IV);

    // Écrire les données déchiffrées dans le fichier de destination
    TFile.WriteAllBytes(DestFile, DecryptedBytes);

    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors du déchiffrement du fichier : ' + E.Message);
  end;
end;

end.
```

### Exemple d'utilisation dans une application

Voici comment utiliser ces classes de chiffrement dans votre application :

```pas
procedure TForm1.ButtonEncryptClick(Sender: TObject);
var
  PlainText, Password, EncryptedText: string;
begin
  PlainText := MemoPlainText.Text;
  Password := EditPassword.Text;

  if (PlainText = '') or (Password = '') then
  begin
    ShowMessage('Veuillez entrer du texte et un mot de passe');
    Exit;
  end;

  try
    // Pour Delphi 12 ou supérieur
    EncryptedText := TModernCrypto.EncryptString(PlainText, Password);

    // Pour les versions antérieures avec DCPcrypt
    // EncryptedText := TCryptoHelper.EncryptString(PlainText, Password);

    MemoEncrypted.Text := EncryptedText;
    ShowMessage('Texte chiffré avec succès');
  except
    on E: Exception do
      ShowMessage('Erreur de chiffrement : ' + E.Message);
  end;
end;

procedure TForm1.ButtonDecryptClick(Sender: TObject);
var
  EncryptedText, Password, PlainText: string;
begin
  EncryptedText := MemoEncrypted.Text;
  Password := EditPassword.Text;

  if (EncryptedText = '') or (Password = '') then
  begin
    ShowMessage('Veuillez entrer du texte chiffré et un mot de passe');
    Exit;
  end;

  try
    // Pour Delphi 12 ou supérieur
    PlainText := TModernCrypto.DecryptString(EncryptedText, Password);

    // Pour les versions antérieures avec DCPcrypt
    // PlainText := TCryptoHelper.DecryptString(EncryptedText, Password);

    MemoPlainText.Text := PlainText;
    ShowMessage('Texte déchiffré avec succès');
  except
    on E: Exception do
      ShowMessage('Erreur de déchiffrement : ' + E.Message);
  end;
end;

procedure TForm1.ButtonEncryptFileClick(Sender: TObject);
var
  SourceFile, DestFile, Password: string;
begin
  if OpenDialog1.Execute then
  begin
    SourceFile := OpenDialog1.FileName;

    if SaveDialog1.Execute then
    begin
      DestFile := SaveDialog1.FileName;
      Password := EditFilePassword.Text;

      if Password = '' then
      begin
        ShowMessage('Veuillez entrer un mot de passe');
        Exit;
      end;

      try
        if TModernCrypto.EncryptFile(SourceFile, DestFile, Password) then
          ShowMessage('Fichier chiffré avec succès');
      except
        on E: Exception do
          ShowMessage('Erreur : ' + E.Message);
      end;
    end;
  end;
end;
```

### Chiffrement des données sensibles dans une base de données

Il est souvent nécessaire de chiffrer certaines données sensibles dans une base de données, comme les numéros de carte de crédit, les informations médicales, etc.

```pas
procedure TClientDataModule.AddCustomer(const Name, Email, CreditCard: string);
var
  Query: TFDQuery;
  EncryptedCreditCard: string;
begin
  // Chiffrer le numéro de carte de crédit avant de le stocker
  EncryptedCreditCard := TModernCrypto.EncryptString(
    CreditCard,
    GetApplicationEncryptionKey()
  );

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO customers (name, email, credit_card_encrypted) ' +
      'VALUES (:name, :email, :creditCard)';
    Query.ParamByName('name').AsString := Name;
    Query.ParamByName('email').AsString := Email;
    Query.ParamByName('creditCard').AsString := EncryptedCreditCard;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TClientDataModule.GetCustomerCreditCard(const CustomerID: Integer): string;
var
  Query: TFDQuery;
  EncryptedCreditCard: string;
begin
  Result := '';

  // Vérifier si l'utilisateur a la permission de voir les données sensibles
  if not AuthManager.HasPermission(Session.UserId, permViewSensitiveData) then
    Exit;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'SELECT credit_card_encrypted FROM customers WHERE id = :id';
    Query.ParamByName('id').AsInteger := CustomerID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      EncryptedCreditCard := Query.FieldByName('credit_card_encrypted').AsString;

      // Déchiffrer le numéro de carte de crédit
      Result := TModernCrypto.DecryptString(
        EncryptedCreditCard,
        GetApplicationEncryptionKey()
      );

      // Masquer partiellement le numéro pour l'affichage
      if Length(Result) > 4 then
        Result := StringOfChar('*', Length(Result) - 4) + Copy(Result, Length(Result) - 3, 4);
    end;
  finally
    Query.Free;
  end;
end;

// Fonction pour obtenir la clé de chiffrement de l'application
function GetApplicationEncryptionKey: string;
var
  IniFile: TIniFile;
  EncryptedKey: string;
  Password: string;
begin
  // Cette méthode est très simplifiée
  // Dans une application réelle, vous utiliseriez une méthode plus sécurisée
  // pour stocker et récupérer votre clé de chiffrement

  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    EncryptedKey := IniFile.ReadString('Security', 'EncryptedKey', '');

    if EncryptedKey = '' then
    begin
      // Générer une nouvelle clé si elle n'existe pas
      Password := GenerateRandomPassword(32);
      EncryptedKey := ProtectString(Password);
      IniFile.WriteString('Security', 'EncryptedKey', EncryptedKey);
    end
    else
    begin
      // Déchiffrer la clé existante
      Password := UnprotectString(EncryptedKey);
    end;

    Result := Password;
  finally
    IniFile.Free;
  end;
end;
```

> ⚠️ **Important** : Dans une application réelle, vous ne stockeriez pas la clé de chiffrement dans un fichier INI. Vous utiliseriez plutôt un stockage sécurisé comme le Windows Data Protection API (DPAPI) ou un HSM (Hardware Security Module).

### Protection des clés de chiffrement

La sécurité de vos données chiffrées dépend fortement de la protection de vos clés de chiffrement. Voici comment utiliser le Windows Data Protection API (DPAPI) pour protéger vos clés :

```pas
// Nécessite Delphi 12 ou supérieur pour certaines fonctions
unit KeyProtection;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, System.NetEncoding;

// Protéger une chaîne avec DPAPI (Windows Data Protection API)
function ProtectString(const PlainText: string): string;
// Récupérer une chaîne protégée avec DPAPI
function UnprotectString(const ProtectedText: string): string;

implementation

function ProtectString(const PlainText: string): string;
var
  DataIn: TBytes;
  DataOut: TBytes;
  DataOutSize: DWORD;
  Success: Boolean;
begin
  DataIn := TEncoding.UTF8.GetBytes(PlainText);

  // Première passe pour obtenir la taille nécessaire
  DataOutSize := 0;
  Success := CryptProtectData(
    DataIn,             // Données à chiffrer
    nil,                // Description (optionnel)
    nil,                // Entropie supplémentaire (optionnel)
    nil,                // Réservé
    nil,                // Invite (optionnel)
    CRYPTPROTECT_UI_FORBIDDEN, // Flags
    DataOut,            // Données chiffrées
    DataOutSize         // Taille des données chiffrées
  );

  if not Success then
    raise Exception.Create('Erreur lors de la protection des données');

  // Allouer la mémoire pour les données chiffrées
  SetLength(DataOut, DataOutSize);

  // Chiffrer les données
  Success := CryptProtectData(
    DataIn,
    nil,
    nil,
    nil,
    nil,
    CRYPTPROTECT_UI_FORBIDDEN,
    DataOut,
    DataOutSize
  );

  if not Success then
    raise Exception.Create('Erreur lors de la protection des données');

  // Encoder en Base64 pour le stockage
  Result := TNetEncoding.Base64.EncodeBytesToString(DataOut);
end;

function UnprotectString(const ProtectedText: string): string;
var
  DataIn: TBytes;
  DataOut: TBytes;
  DataOutSize: DWORD;
  Success: Boolean;
begin
  // Décoder de Base64
  DataIn := TNetEncoding.Base64.DecodeStringToBytes(ProtectedText);

  // Première passe pour obtenir la taille nécessaire
  DataOutSize := 0;
  Success := CryptUnprotectData(
    DataIn,             // Données chiffrées
    nil,                // Description
    nil,                // Entropie
    nil,                // Réservé
    nil,                // Invite
    CRYPTPROTECT_UI_FORBIDDEN, // Flags
    DataOut,            // Données déchiffrées
    DataOutSize         // Taille des données déchiffrées
  );

  if not Success then
    raise Exception.Create('Erreur lors de la récupération des données protégées');

  // Allouer la mémoire pour les données déchiffrées
  SetLength(DataOut, DataOutSize);

  // Déchiffrer les données
  Success := CryptUnprotectData(
    DataIn,
    nil,
    nil,
    nil,
    nil,
    CRYPTPROTECT_UI_FORBIDDEN,
    DataOut,
    DataOutSize
  );

  if not Success then
    raise Exception.Create('Erreur lors de la récupération des données protégées');

  // Convertir en chaîne
  Result := TEncoding.UTF8.GetString(DataOut);
end;

end.
```

### Hachage pour stocker les mots de passe

Pour stocker les mots de passe de manière sécurisée, vous ne devez jamais utiliser de chiffrement, mais plutôt des fonctions de hachage. Le hachage est un processus à sens unique : vous ne pouvez pas récupérer le mot de passe original à partir du hachage.

```pas
unit PasswordHashing;

interface

uses
  System.SysUtils, System.Hash, System.NetEncoding;

type
  TPasswordHasher = class
  public
    // Créer un hachage sécurisé d'un mot de passe
    class function HashPassword(const Password: string): string;

    // Vérifier si un mot de passe correspond à un hachage stocké
    class function VerifyPassword(const Password, StoredHash: string): Boolean;
  end;

implementation

{ TPasswordHasher }

class function TPasswordHasher.HashPassword(const Password: string): string;
var
  Salt: TBytes;
  SaltedPassword: TBytes;
  PasswordBytes: TBytes;
  HashedBytes: TBytes;
  SaltString: string;
begin
  // Générer un sel aléatoire
  SetLength(Salt, 16);
  TCrypto.RandomBytes(Salt);

  // Convertir le mot de passe en bytes
  PasswordBytes := TEncoding.UTF8.GetBytes(Password);

  // Combiner le mot de passe et le sel
  SetLength(SaltedPassword, Length(PasswordBytes) + Length(Salt));
  Move(Salt[0], SaltedPassword[0], Length(Salt));
  Move(PasswordBytes[0], SaltedPassword[Length(Salt)], Length(PasswordBytes));

  // Hacher avec SHA-256
  HashedBytes := THashSHA2.GetHashBytes(SaltedPassword);

  // Encoder le sel et le hachage en Base64
  SaltString := TNetEncoding.Base64.EncodeBytesToString(Salt);
  Result := SaltString + '.' + TNetEncoding.Base64.EncodeBytesToString(HashedBytes);
end;

class function TPasswordHasher.VerifyPassword(const Password, StoredHash: string): Boolean;
var
  Parts: TArray<string>;
  Salt: TBytes;
  PasswordBytes: TBytes;
  SaltedPassword: TBytes;
  HashedBytes: TBytes;
  StoredHashBytes: TBytes;
begin
  Result := False;

  // Séparer le sel et le hachage
  Parts := StoredHash.Split(['.']);
  if Length(Parts) <> 2 then
    Exit;

  // Décoder le sel et le hachage stocké
  Salt := TNetEncoding.Base64.DecodeStringToBytes(Parts[0]);
  StoredHashBytes := TNetEncoding.Base64.DecodeStringToBytes(Parts[1]);

  // Convertir le mot de passe en bytes
  PasswordBytes := TEncoding.UTF8.GetBytes(Password);

  // Combiner le mot de passe et le sel
  SetLength(SaltedPassword, Length(PasswordBytes) + Length(Salt));
  Move(Salt[0], SaltedPassword[0], Length(Salt));
  Move(PasswordBytes[0], SaltedPassword[Length(Salt)], Length(PasswordBytes));

  // Hacher avec SHA-256
  HashedBytes := THashSHA2.GetHashBytes(SaltedPassword);

  // Comparer les hachages
  Result := (Length(HashedBytes) = Length(StoredHashBytes));

  if Result then
  begin
    for var I := 0 to Length(HashedBytes) - 1 do
    begin
      if HashedBytes[I] <> StoredHashBytes[I] then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

end.
```

### Stockage sécurisé des mots de passe dans une base de données

Voici comment utiliser la classe `TPasswordHasher` pour stocker et vérifier les mots de passe :

```pas
procedure TUserManager.CreateUser(const Username, Email, Password: string);
var
  HashedPassword: string;
  Query: TFDQuery;
begin
  // Hacher le mot de passe avant de le stocker
  HashedPassword := TPasswordHasher.HashPassword(Password);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO users (username, email, password_hash) ' +
      'VALUES (:username, :email, :passwordHash)';
    Query.ParamByName('username').AsString := Username;
    Query.ParamByName('email').AsString := Email;
    Query.ParamByName('passwordHash').AsString := HashedPassword;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TUserManager.VerifyUserPassword(const Username, Password: string): Boolean;
var
  Query: TFDQuery;
  StoredHash: string;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'SELECT password_hash FROM users WHERE username = :username';
    Query.ParamByName('username').AsString := Username;
    Query.Open;

    if not Query.IsEmpty then
    begin
      StoredHash := Query.FieldByName('password_hash').AsString;
      Result := TPasswordHasher.VerifyPassword(Password, StoredHash);
    end;
  finally
    Query.Free;
  end;
end;
```

### Chiffrement des configurations et des fichiers sensibles

Les fichiers de configuration contiennent souvent des informations sensibles comme des clés d'API, des identifiants de base de données, etc. Voici comment les protéger :

```pas
unit SecureConfig;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.JSON,
  ModernEncryption;

type
  TSecureConfig = class
  private
    FConfigFileName: string;
    FMasterPassword: string;
    FConfigData: TJSONObject;

    function LoadEncryptedConfig: Boolean;
    procedure SaveEncryptedConfig;
  public
    constructor Create(const ConfigFileName, MasterPassword: string);
    destructor Destroy; override;

    function GetValue(const Section, Key: string; DefaultValue: string = ''): string;
    procedure SetValue(const Section, Key, Value: string);
    procedure Save;
  end;

implementation

constructor TSecureConfig.Create(const ConfigFileName, MasterPassword: string);
begin
  inherited Create;
  FConfigFileName := ConfigFileName;
  FMasterPassword := MasterPassword;
  FConfigData := TJSONObject.Create;

  if FileExists(FConfigFileName) then
  begin
    if not LoadEncryptedConfig then
    begin
      // Si le chargement échoue, créer un nouveau fichier de configuration
      FConfigData.Free;
      FConfigData := TJSONObject.Create;
    end;
  end;
end;

destructor TSecureConfig.Destroy;
begin
  FConfigData.Free;
  inherited;
end;

function TSecureConfig.LoadEncryptedConfig: Boolean;
var
  EncryptedContent, JsonContent: string;
begin
  Result := False;

  try
    // Lire le contenu chiffré
    EncryptedContent := TFile.ReadAllText(FConfigFileName);

    // Déchiffrer
    JsonContent := TModernCrypto.DecryptString(EncryptedContent, FMasterPassword);

    // Parser le JSON
    FConfigData.Free;
    FConfigData := TJSONObject.ParseJSONValue(JsonContent) as TJSONObject;

    Result := FConfigData <> nil;
  except
    // En cas d'erreur, retourner False
    Result := False;
  end;
end;

procedure TSecureConfig.SaveEncryptedConfig;
var
  JsonContent, EncryptedContent: string;
begin
  // Convertir en JSON
  JsonContent := FConfigData.ToString;

  // Chiffrer
  EncryptedContent := TModernCrypto.EncryptString(JsonContent, FMasterPassword);

  // Sauvegarder
  TFile.WriteAllText(FConfigFileName, EncryptedContent);
end;

function TSecureConfig.GetValue(const Section, Key: string; DefaultValue: string): string;
var
  SectionObj: TJSONObject;
begin
  Result := DefaultValue;

  // Vérifier si la section existe
  if FConfigData.TryGetValue<TJSONObject>(Section, SectionObj) then
  begin
    // Vérifier si la clé existe dans la section
    SectionObj.TryGetValue<string>(Key, Result);
  end;
end;

procedure TSecureConfig.SetValue(const Section, Key, Value: string);
var
  SectionObj: TJSONObject;
begin
  // Vérifier si la section existe, sinon la créer
  if not FConfigData.TryGetValue<TJSONObject>(Section, SectionObj) then
  begin
    SectionObj := TJSONObject.Create;
    FConfigData.AddPair(Section, SectionObj);
  end;

  // Mettre à jour ou ajouter la valeur
  if SectionObj.Values[Key] <> nil then
    SectionObj.RemovePair(Key);

  SectionObj.AddPair(Key, Value);
end;

procedure TSecureConfig.Save;
begin
  SaveEncryptedConfig;
end;

end.
```

### Exemple d'utilisation de la configuration sécurisée

```pas
procedure TForm1.FormCreate(Sender: TObject);
var
  ConfigFile: string;
  MasterPassword: string;
begin
  ConfigFile := ChangeFileExt(Application.ExeName, '.cfg');

  // Dans une application réelle, demander le mot de passe à l'utilisateur
  // ou le récupérer depuis un stockage sécurisé
  MasterPassword := 'motDePasseTrèsComplexe123!';

  SecureConfig := TSecureConfig.Create(ConfigFile, MasterPassword);

  // Charger les données de configuration
  EditAPIKey.Text := SecureConfig.GetValue('API', 'Key', '');
  EditAPISecret.Text := SecureConfig.GetValue('API', 'Secret', '');

  CheckBoxRememberLogin.Checked := SecureConfig.GetValue('Login', 'Remember', 'False') = 'True';

  if CheckBoxRememberLogin.Checked then
  begin
    EditUsername.Text := SecureConfig.GetValue('Login', 'Username', '');
    // Ne jamais stocker le mot de passe, même chiffré, si possible
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SecureConfig.Free;
end;

procedure TForm1.ButtonSaveConfigClick(Sender: TObject);
begin
  // Enregistrer les paramètres
  SecureConfig.SetValue('API', 'Key', EditAPIKey.Text);
  SecureConfig.SetValue('API', 'Secret', EditAPISecret.Text);

  SecureConfig.SetValue('Login', 'Remember', BoolToStr(CheckBoxRememberLogin.Checked, True));

  if CheckBoxRememberLogin.Checked then
    SecureConfig.SetValue('Login', 'Username', EditUsername.Text)
  else
    SecureConfig.SetValue('Login', 'Username', '');

  // Sauvegarder le fichier
  SecureConfig.Save;

  ShowMessage('Configuration enregistrée avec succès');
end;
```

### Chiffrement des communications réseau

Pour protéger les données envoyées sur le réseau, vous devez utiliser HTTPS (TLS/SSL) pour les communications. Voici comment configurer une requête REST sécurisée :

```pas
procedure TRESTClient.ConfigureSecureConnection;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://api.exemple.com';  // Notez le "https"
    RESTClient.Accept := 'application/json';
    RESTClient.AcceptCharset := 'UTF-8';

    // Activation de la vérification SSL
    RESTClient.SecureProtocols := [THTTPSecureProtocol.TLS12];

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Ajout des données à envoyer
    RESTRequest.AddParameter('username', 'user1');
    RESTRequest.AddParameter('password', 'password123');

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      // Traitement de la réponse réussie
    end
    else
    begin
      // Traitement de l'erreur
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

> ⚠️ **Important** : Toujours utiliser HTTPS plutôt que HTTP pour les communications contenant des données sensibles. Vérifiez que le certificat SSL du serveur est valide et à jour.

### Chiffrement de bout en bout

Le chiffrement de bout en bout garantit que seuls l'expéditeur et le destinataire peuvent lire les messages échangés. Voici un exemple simple utilisant le chiffrement asymétrique RSA :

```pas
// Nécessite Delphi 12 ou supérieur pour certaines fonctions
unit EndToEndEncryption;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding,
  System.Generics.Collections, System.IOUtils,
  System.Security.Cryptography;

type
  TEndToEndEncryption = class
  private
    class var FRSAProvider: TRSAProvider;
  public
    class constructor Create;
    class destructor Destroy;

    // Génération de clés
    class procedure GenerateKeyPair(const PrivateKeyFile, PublicKeyFile: string);

    // Chiffrement avec la clé publique du destinataire
    class function EncryptMessage(const Message, RecipientPublicKeyFile: string): string;

    // Déchiffrement avec sa propre clé privée
    class function DecryptMessage(const EncryptedMessage, PrivateKeyFile: string): string;
  end;

implementation

class constructor TEndToEndEncryption.Create;
begin
  FRSAProvider := TRSAProvider.Create;
end;

class destructor TEndToEndEncryption.Destroy;
begin
  FRSAProvider.Free;
end;

class procedure TEndToEndEncryption.GenerateKeyPair(
  const PrivateKeyFile, PublicKeyFile: string);
var
  PrivateKey, PublicKey: string;
begin
  // Générer une paire de clés RSA
  FRSAProvider.GenerateKeys(2048); // 2048 bits pour une bonne sécurité

  // Exporter les clés au format PEM
  PrivateKey := FRSAProvider.ExportPrivateKey(TPemRSAFormat.PKCS8);
  PublicKey := FRSAProvider.ExportPublicKey(TPemRSAFormat.PKCS8);

  // Sauvegarder les clés dans des fichiers
  TFile.WriteAllText(PrivateKeyFile, PrivateKey);
  TFile.WriteAllText(PublicKeyFile, PublicKey);
end;

class function TEndToEndEncryption.EncryptMessage(
  const Message, RecipientPublicKeyFile: string): string;
var
  PublicKey: string;
  EncryptedBytes: TBytes;
begin
  // Charger la clé publique du destinataire
  PublicKey := TFile.ReadAllText(RecipientPublicKeyFile);

  // Importer la clé
  FRSAProvider.ImportPublicKey(PublicKey, TPemRSAFormat.PKCS8);

  // Chiffrer le message
  EncryptedBytes := FRSAProvider.Encrypt(TEncoding.UTF8.GetBytes(Message));

  // Encoder en Base64 pour faciliter le stockage et la transmission
  Result := TNetEncoding.Base64.EncodeBytesToString(EncryptedBytes);
end;

class function TEndToEndEncryption.DecryptMessage(
  const EncryptedMessage, PrivateKeyFile: string): string;
var
  PrivateKey: string;
  EncryptedBytes, DecryptedBytes: TBytes;
begin
  // Charger la clé privée
  PrivateKey := TFile.ReadAllText(PrivateKeyFile);

  // Importer la clé
  FRSAProvider.ImportPrivateKey(PrivateKey, TPemRSAFormat.PKCS8);

  // Décoder le message chiffré
  EncryptedBytes := TNetEncoding.Base64.DecodeStringToBytes(EncryptedMessage);

  // Déchiffrer le message
  DecryptedBytes := FRSAProvider.Decrypt(EncryptedBytes);

  // Convertir en chaîne
  Result := TEncoding.UTF8.GetString(DecryptedBytes);
end;

end.
```

### Chiffrement hybride pour les grandes quantités de données

Le chiffrement asymétrique (comme RSA) est lent pour les grandes quantités de données. Pour cette raison, on utilise généralement un chiffrement hybride :

1. Générer une clé symétrique aléatoire (AES)
2. Chiffrer les données avec cette clé symétrique
3. Chiffrer la clé symétrique avec la clé publique du destinataire
4. Envoyer les données chiffrées et la clé symétrique chiffrée

```pas
// Nécessite Delphi 12 ou supérieur
function EncryptLargeFile(
  const SourceFile, DestFile, RecipientPublicKeyFile: string): Boolean;
var
  PublicKey: string;
  AESKey, IV: TBytes;
  EncryptedKey: TBytes;
  SourceBytes, EncryptedBytes: TBytes;
  HeaderBytes: TBytes;
  CombinedOutput: TMemoryStream;
begin
  Result := False;

  try
    // 1. Générer une clé AES aléatoire et un IV
    SetLength(AESKey, 32); // 256 bits
    SetLength(IV, 16);     // 128 bits
    TCrypto.RandomBytes(AESKey);
    TCrypto.RandomBytes(IV);

    // 2. Charger la clé publique du destinataire
    PublicKey := TFile.ReadAllText(RecipientPublicKeyFile);
    FRSAProvider.ImportPublicKey(PublicKey, TPemRSAFormat.PKCS8);

    // 3. Chiffrer la clé AES avec la clé publique RSA
    EncryptedKey := FRSAProvider.Encrypt(AESKey);

    // 4. Lire le fichier source
    SourceBytes := TFile.ReadAllBytes(SourceFile);

    // 5. Chiffrer les données avec AES
    EncryptedBytes := TCipher.AES.Encrypt(SourceBytes, AESKey, IV);

    // 6. Assembler le résultat final :
    // [Signature (8 bytes)][EncryptedKeySize (4 bytes)][EncryptedKey][IV (16 bytes)][EncryptedData]
    CombinedOutput := TMemoryStream.Create;
    try
      // Écrire la signature
      HeaderBytes := TEncoding.UTF8.GetBytes('DELPHIE2E');
      CombinedOutput.WriteBuffer(HeaderBytes[0], Length(HeaderBytes));

      // Écrire la taille de la clé chiffrée
      var KeySize: Integer := Length(EncryptedKey);
      CombinedOutput.WriteBuffer(KeySize, SizeOf(Integer));

      // Écrire la clé chiffrée
      CombinedOutput.WriteBuffer(EncryptedKey[0], Length(EncryptedKey));

      // Écrire l'IV
      CombinedOutput.WriteBuffer(IV[0], Length(IV));

      // Écrire les données chiffrées
      CombinedOutput.WriteBuffer(EncryptedBytes[0], Length(EncryptedBytes));

      // Sauvegarder le tout dans le fichier de destination
      CombinedOutput.SaveToFile(DestFile);

      Result := True;
    finally
      CombinedOutput.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors du chiffrement hybride : ' + E.Message);
  end;
end;

function DecryptLargeFile(
  const SourceFile, DestFile, PrivateKeyFile: string): Boolean;
var
  PrivateKey: string;
  FileStream: TFileStream;
  Signature: TBytes;
  KeySize: Integer;
  EncryptedKey, AESKey, IV: TBytes;
  EncryptedBytes, DecryptedBytes: TBytes;
begin
  Result := False;

  try
    // 1. Charger la clé privée
    PrivateKey := TFile.ReadAllText(PrivateKeyFile);
    FRSAProvider.ImportPrivateKey(PrivateKey, TPemRSAFormat.PKCS8);

    // 2. Ouvrir le fichier source
    FileStream := TFileStream.Create(SourceFile, fmOpenRead);
    try
      // 3. Lire et vérifier la signature
      SetLength(Signature, 9);
      FileStream.ReadBuffer(Signature[0], 9);
      if TEncoding.UTF8.GetString(Signature) <> 'DELPHIE2E' then
        raise Exception.Create('Format de fichier invalide');

      // 4. Lire la taille de la clé chiffrée
      FileStream.ReadBuffer(KeySize, SizeOf(Integer));

      // 5. Lire la clé chiffrée
      SetLength(EncryptedKey, KeySize);
      FileStream.ReadBuffer(EncryptedKey[0], KeySize);

      // 6. Déchiffrer la clé AES avec la clé privée RSA
      AESKey := FRSAProvider.Decrypt(EncryptedKey);

      // 7. Lire l'IV
      SetLength(IV, 16);
      FileStream.ReadBuffer(IV[0], 16);

      // 8. Lire les données chiffrées
      SetLength(EncryptedBytes, FileStream.Size - FileStream.Position);
      FileStream.ReadBuffer(EncryptedBytes[0], Length(EncryptedBytes));
    finally
      FileStream.Free;
    end;

    // 9. Déchiffrer les données avec AES
    DecryptedBytes := TCipher.AES.Decrypt(EncryptedBytes, AESKey, IV);

    // 10. Enregistrer les données déchiffrées
    TFile.WriteAllBytes(DestFile, DecryptedBytes);

    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors du déchiffrement hybride : ' + E.Message);
  end;
end;
```

### Meilleures pratiques de chiffrement

1. **Utilisez des algorithmes éprouvés** : N'inventez pas vos propres algorithmes de chiffrement. Utilisez des standards reconnus comme AES, RSA, etc.

2. **Taille de clé adéquate** : Utilisez des tailles de clé suffisantes (AES-256, RSA-2048 ou plus).

3. **Génération de clés sécurisée** : Utilisez des générateurs de nombres aléatoires cryptographiquement sûrs.

4. **Protection des clés** : Vos données chiffrées ne sont pas plus sécurisées que vos clés. Protégez-les adéquatement.

5. **Sel aléatoire** : Utilisez toujours un sel unique et aléatoire pour le hachage des mots de passe.

6. **Mise à jour régulière** : Les algorithmes de chiffrement deviennent moins sûrs avec le temps. Prévoyez des mécanismes pour mettre à jour vos méthodes de chiffrement.

7. **Minimisation des données sensibles** : La meilleure protection est de ne pas stocker de données sensibles si ce n'est pas nécessaire.

8. **Chiffrement en transit** : Utilisez toujours HTTPS pour les communications réseau.

9. **Validation des entrées** : Validez toujours les entrées avant de les traiter, surtout pour les opérations cryptographiques.

10. **Auditabilité** : Enregistrez les événements liés à la sécurité pour détecter d'éventuelles tentatives d'intrusion.

### Exemple pratique : Application de notes sécurisées

Voici un exemple simple d'application de notes sécurisées qui utilise le chiffrement pour protéger les notes de l'utilisateur :

```pas
unit SecureNotesForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.IOUtils,
  ModernEncryption;

type
  TFormSecureNotes = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    MemoNote: TMemo;
    Panel2: TPanel;
    ButtonNewNote: TButton;
    ButtonSaveNote: TButton;
    ButtonDeleteNote: TButton;
    EditTitle: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonNewNoteClick(Sender: TObject);
    procedure ButtonSaveNoteClick(Sender: TObject);
    procedure ButtonDeleteNoteClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FNotesFolder: string;
    FMasterPassword: string;
    FCurrentNoteFile: string;

    procedure LoadNotesList;
    procedure ClearNoteEditor;
    function GetNoteFileName(const Title: string): string;
    function SaveEncryptedNote(const FileName, Title, Content: string): Boolean;
    function LoadEncryptedNote(const FileName: string; out Title, Content: string): Boolean;
  end;

var
  FormSecureNotes: TFormSecureNotes;

implementation

{$R *.dfm}

procedure TFormSecureNotes.FormCreate(Sender: TObject);
begin
  // Créer le dossier pour les notes s'il n'existe pas
  FNotesFolder := TPath.Combine(TPath.GetDocumentsPath, 'SecureNotes');
  if not TDirectory.Exists(FNotesFolder) then
    TDirectory.CreateDirectory(FNotesFolder);

  // En production, demandez le mot de passe à l'utilisateur
  // et stockez-le de manière sécurisée
  FMasterPassword := 'motDePasseComplexe123!';

  LoadNotesList;
  ClearNoteEditor;
end;

procedure TFormSecureNotes.FormDestroy(Sender: TObject);
begin
  // Sauvegarder la note actuelle si elle a été modifiée
  if (FCurrentNoteFile <> '') and MemoNote.Modified then
    ButtonSaveNoteClick(nil);
end;

procedure TFormSecureNotes.LoadNotesList;
var
  Files: TStringDynArray;
  FileName: string;
  ListItem: TListItem;
  Title, Content: string;
begin
  ListView1.Items.Clear;

  // Obtenir la liste des fichiers de notes (.enote)
  Files := TDirectory.GetFiles(FNotesFolder, '*.enote');

  // Ajouter chaque note à la liste
  for FileName in Files do
  begin
    if LoadEncryptedNote(FileName, Title, Content) then
    begin
      ListItem := ListView1.Items.Add;
      ListItem.Caption := Title;
      ListItem.Data := Pointer(PChar(string(FileName))); // Stocker le nom de fichier
    end;
  end;
end;

function TFormSecureNotes.GetNoteFileName(const Title: string): string;
var
  SafeFileName: string;
  I: Integer;
begin
  // Créer un nom de fichier sûr à partir du titre
  SafeFileName := Title;

  // Remplacer les caractères non autorisés
  for I := 1 to Length(SafeFileName) do
    if not CharInSet(SafeFileName[I], ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', ' ']) then
      SafeFileName[I] := '_';

  // Ajouter un timestamp pour éviter les collisions
  Result := TPath.Combine(FNotesFolder,
    SafeFileName + '_' + FormatDateTime('yyyymmddhhnnss', Now) + '.enote');
end;

function TFormSecureNotes.SaveEncryptedNote(const FileName, Title, Content: string): Boolean;
var
  NoteData: string;
  EncryptedData: string;
begin
  Result := False;

  try
    // Préparer les données (JSON serait mieux en pratique)
    NoteData := 'TITLE:' + Title + #13#10 + 'CONTENT:' + Content;

    // Chiffrer les données
    EncryptedData := TModernCrypto.EncryptString(NoteData, FMasterPassword);

    // Sauvegarder dans le fichier
    TFile.WriteAllText(FileName, EncryptedData);

    Result := True;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la sauvegarde : ' + E.Message);
  end;
end;

function TFormSecureNotes.LoadEncryptedNote(const FileName: string; out Title, Content: string): Boolean;
var
  EncryptedData, NoteData: string;
  Lines: TArray<string>;
  I: Integer;
begin
  Result := False;
  Title := '';
  Content := '';

  try
    // Lire le fichier chiffré
    EncryptedData := TFile.ReadAllText(FileName);

    // Déchiffrer les données
    NoteData := TModernCrypto.DecryptString(EncryptedData, FMasterPassword);

    // Parser les données (JSON serait mieux en pratique)
    Lines := NoteData.Split([#13#10]);

    for I := 0 to Length(Lines) - 1 do
    begin
      if Lines[I].StartsWith('TITLE:') then
        Title := Lines[I].Substring(6)
      else if Lines[I].StartsWith('CONTENT:') then
        Content := Lines[I].Substring(8);
    end;

    Result := (Title <> '');
  except
    on E: Exception do
    begin
      // Afficher un message si c'est un problème de déchiffrement
      // (probablement mauvais mot de passe)
      if E.Message.Contains('déchiffrement') then
        ShowMessage('Impossible de déchiffrer cette note. ' + E.Message)
      else
        // Ne pas afficher les erreurs pour les fichiers corrompus,
        // simplement les ignorer
        Result := False;
    end;
  end;
end;

procedure TFormSecureNotes.ClearNoteEditor;
begin
  EditTitle.Text := '';
  MemoNote.Text := '';
  MemoNote.Modified := False;
  FCurrentNoteFile := '';
end;

procedure TFormSecureNotes.ButtonNewNoteClick(Sender: TObject);
begin
  // Sauvegarder la note actuelle si elle a été modifiée
  if (FCurrentNoteFile <> '') and MemoNote.Modified then
  begin
    if MessageDlg('Sauvegarder les modifications de la note actuelle ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ButtonSaveNoteClick(nil);
  end;

  ClearNoteEditor;
  EditTitle.SetFocus;
end;

procedure TFormSecureNotes.ButtonSaveNoteClick(Sender: TObject);
var
  Title, FileName: string;
  ListItem: TListItem;
  I: Integer;
begin
  Title := Trim(EditTitle.Text);

  if Title = '' then
  begin
    ShowMessage('Veuillez entrer un titre pour la note.');
    EditTitle.SetFocus;
    Exit;
  end;

  // Nouvelle note ou mise à jour ?
  if FCurrentNoteFile = '' then
  begin
    // Nouvelle note
    FileName := GetNoteFileName(Title);

    if SaveEncryptedNote(FileName, Title, MemoNote.Text) then
    begin
      FCurrentNoteFile := FileName;
      MemoNote.Modified := False;

      // Ajouter à la liste
      ListItem := ListView1.Items.Add;
      ListItem.Caption := Title;
      ListItem.Data := Pointer(PChar(string(FileName)));
      ListItem.Selected := True;

      ShowMessage('Note enregistrée avec succès.');
    end;
  end
  else
  begin
    // Mise à jour de la note existante
    if SaveEncryptedNote(FCurrentNoteFile, Title, MemoNote.Text) then
    begin
      MemoNote.Modified := False;

      // Mettre à jour le titre dans la liste
      for I := 0 to ListView1.Items.Count - 1 do
      begin
        if string(ListView1.Items[I].Data) = FCurrentNoteFile then
        begin
          ListView1.Items[I].Caption := Title;
          Break;
        end;
      end;

      ShowMessage('Note mise à jour avec succès.');
    end;
  end;
end;

procedure TFormSecureNotes.ButtonDeleteNoteClick(Sender: TObject);
var
  I: Integer;
begin
  if FCurrentNoteFile = '' then
    Exit;

  if MessageDlg('Êtes-vous sûr de vouloir supprimer cette note ?',
               mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Supprimer le fichier
    try
      TFile.Delete(FCurrentNoteFile);

      // Supprimer de la liste
      for I := 0 to ListView1.Items.Count - 1 do
      begin
        if string(ListView1.Items[I].Data) = FCurrentNoteFile then
        begin
          ListView1.Items.Delete(I);
          Break;
        end;
      end;

      // Effacer l'éditeur
      ClearNoteEditor;

      ShowMessage('Note supprimée avec succès.');
    except
      on E: Exception do
        ShowMessage('Erreur lors de la suppression : ' + E.Message);
    end;
  end;
end;

procedure TFormSecureNotes.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  FileName: string;
  Title, Content: string;
begin
  if not Selected then
    Exit;

  // Sauvegarder la note actuelle si elle a été modifiée
  if (FCurrentNoteFile <> '') and MemoNote.Modified then
  begin
    if MessageDlg('Sauvegarder les modifications de la note actuelle ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ButtonSaveNoteClick(nil);
  end;

  // Charger la note sélectionnée
  FileName := string(Item.Data);

  if LoadEncryptedNote(FileName, Title, Content) then
  begin
    EditTitle.Text := Title;
    MemoNote.Text := Content;
    MemoNote.Modified := False;
    FCurrentNoteFile := FileName;
  end
  else
  begin
    ShowMessage('Impossible de charger la note. Le fichier est peut-être corrompu.');
    ClearNoteEditor;
  end;
end;
```

### Protection des données sensibles en mémoire

Les données sensibles ne sont pas seulement vulnérables sur le disque ou lors des transferts réseau, mais aussi en mémoire. Voici quelques techniques pour protéger les données en mémoire :

#### 1. Effacement sécurisé des variables sensibles

```pas
// Cette classe gère un mot de passe en mémoire et l'efface correctement
unit SecureString;

interface

uses
  System.SysUtils, System.Classes;

type
  TSecureString = class
  private
    FData: TBytes;
    FLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue(const Value: string);
    function GetValue: string;
    procedure Clear;

    property Length: Integer read FLength;
  end;

implementation

constructor TSecureString.Create;
begin
  inherited;
  FLength := 0;
  SetLength(FData, 0);
end;

destructor TSecureString.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSecureString.SetValue(const Value: string);
begin
  // Effacer d'abord les données existantes
  Clear;

  // Allouer et copier les nouvelles données
  FLength := System.Length(Value) * SizeOf(Char);
  SetLength(FData, FLength);

  if FLength > 0 then
    Move(Value[1], FData[0], FLength);
end;

function TSecureString.GetValue: string;
begin
  SetLength(Result, FLength div SizeOf(Char));

  if FLength > 0 then
    Move(FData[0], Result[1], FLength);
end;

procedure TSecureString.Clear;
var
  I: Integer;
begin
  // Remplir la mémoire avec des zéros avant de libérer
  for I := 0 to Length(FData) - 1 do
    FData[I] := 0;

  SetLength(FData, 0);
  FLength := 0;
end;

end.
```

#### 2. Utilisation de SecureString dans un formulaire d'authentification

```pas
unit LoginForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, SecureString;

type
  TFormLogin = class(TForm)
    EditUsername: TEdit;
    EditPassword: TEdit;
    ButtonLogin: TButton;
    CheckBoxRememberUsername: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
  private
    FPassword: TSecureString;
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.dfm}

procedure TFormLogin.FormCreate(Sender: TObject);
begin
  FPassword := TSecureString.Create;
end;

procedure TFormLogin.FormDestroy(Sender: TObject);
begin
  FPassword.Free;
end;

procedure TFormLogin.EditPasswordChange(Sender: TObject);
begin
  // Stocker le mot de passe dans la classe sécurisée
  FPassword.SetValue(EditPassword.Text);
end;

procedure TFormLogin.ButtonLoginClick(Sender: TObject);
var
  Success: Boolean;
begin
  // Utiliser FPassword.GetValue() au lieu de EditPassword.Text
  Success := AuthenticateUser(EditUsername.Text, FPassword.GetValue);

  if Success then
  begin
    // Authentification réussie

    // Effacer le mot de passe de la mémoire
    FPassword.Clear;
    EditPassword.Text := '';

    // Stocker le nom d'utilisateur si demandé
    if CheckBoxRememberUsername.Checked then
      SaveUsername(EditUsername.Text)
    else
      ClearSavedUsername;

    ModalResult := mrOk;
  end
  else
  begin
    ShowMessage('Nom d''utilisateur ou mot de passe incorrect.');
    EditPassword.SetFocus;
  end;
end;
```

### Cryptographie et conformité légale

L'utilisation de la cryptographie peut être soumise à des restrictions légales dans certains pays. Voici quelques points à considérer :

1. **Restrictions à l'exportation** : Certains pays limitent l'exportation de logiciels utilisant des algorithmes cryptographiques forts.

2. **Obligation de divulgation** : Dans certaines juridictions, les autorités peuvent exiger la divulgation des clés de chiffrement.

3. **Conformité réglementaire** : Des réglementations comme le RGPD (GDPR) en Europe, le HIPAA aux États-Unis pour les données médicales, ou le PCI DSS pour les données de cartes de paiement, imposent des exigences spécifiques en matière de protection des données.

Voici un exemple de dialogue pour informer l'utilisateur sur la conformité réglementaire :

```pas
procedure TMainForm.ShowPrivacyPolicy;
begin
  MessageDlg(
    'Politique de confidentialité et de chiffrement' + #13#10#13#10 +
    'Cette application utilise le chiffrement pour protéger vos données personnelles. ' +
    'Nous utilisons l''algorithme AES-256 pour chiffrer toutes les notes que vous créez.' + #13#10#13#10 +
    'Important : Si vous oubliez votre mot de passe principal, vos données ne pourront ' +
    'pas être récupérées. Nous ne stockons pas votre mot de passe et ne pouvons pas ' +
    'le réinitialiser.' + #13#10#13#10 +
    'En utilisant cette application, vous acceptez ces conditions.',
    mtInformation, [mbOk], 0);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Afficher la politique de confidentialité lors de la première utilisation
  if IsFirstRun then
    ShowPrivacyPolicy;

  // Reste du code d'initialisation
end;
```

### Détection de manipulation de données chiffrées

Pour détecter si des données chiffrées ont été manipulées, utilisez un code d'authentification de message (MAC) ou un chiffrement authentifié comme AES-GCM :

```pas
// Nécessite Delphi 12 ou supérieur
function EncryptWithAuthentication(const PlainText, Password: string): string;
var
  Key, IV, EncryptedBytes, AuthTag: TBytes;
  CombinedOutput: TBytes;
begin
  // Générer une clé à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);

  // Générer un IV aléatoire
  SetLength(IV, 12); // 96 bits pour GCM
  TCrypto.RandomBytes(IV);

  // Chiffrer avec AES-GCM
  TCipher.AES.EncryptGCM(
    TEncoding.UTF8.GetBytes(PlainText),
    Key,
    IV,
    nil, // Données associées (optionnel)
    EncryptedBytes,
    AuthTag
  );

  // Combiner IV + AuthTag + Données chiffrées
  SetLength(CombinedOutput, Length(IV) + Length(AuthTag) + Length(EncryptedBytes));

  // Copier l'IV
  Move(IV[0], CombinedOutput[0], Length(IV));

  // Copier le tag d'authentification
  Move(AuthTag[0], CombinedOutput[Length(IV)], Length(AuthTag));

  // Copier les données chiffrées
  Move(EncryptedBytes[0], CombinedOutput[Length(IV) + Length(AuthTag)], Length(EncryptedBytes));

  // Encoder en Base64
  Result := TNetEncoding.Base64.EncodeBytesToString(CombinedOutput);
end;

function DecryptWithAuthentication(const EncryptedText, Password: string): string;
var
  Key, CombinedInput, IV, AuthTag, EncryptedBytes, DecryptedBytes: TBytes;
begin
  // Décoder de Base64
  CombinedInput := TNetEncoding.Base64.DecodeStringToBytes(EncryptedText);

  // Vérifier la taille minimale
  if Length(CombinedInput) < 28 then // 12 (IV) + 16 (AuthTag) minimum
    raise Exception.Create('Données chiffrées invalides');

  // Générer la clé à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);

  // Extraire l'IV (12 premiers octets)
  SetLength(IV, 12);
  Move(CombinedInput[0], IV[0], 12);

  // Extraire le tag d'authentification (16 octets suivants)
  SetLength(AuthTag, 16);
  Move(CombinedInput[12], AuthTag[0], 16);

  // Extraire les données chiffrées (le reste)
  SetLength(EncryptedBytes, Length(CombinedInput) - 28);
  Move(CombinedInput[28], EncryptedBytes[0], Length(EncryptedBytes));

  try
    // Déchiffrer avec AES-GCM (vérifie automatiquement l'authenticité)
    DecryptedBytes := TCipher.AES.DecryptGCM(
      EncryptedBytes,
      Key,
      IV,
      nil, // Données associées (doit correspondre à l'encodage)
      AuthTag
    );

    // Convertir en chaîne
    Result := TEncoding.UTF8.GetString(DecryptedBytes);
  except
    on E: Exception do
      raise Exception.Create('Échec du déchiffrement ou données manipulées : ' + E.Message);
  end;
end;
```

### Chiffrement des paramètres de connexion à la base de données

Les paramètres de connexion à la base de données sont particulièrement sensibles. Voici comment les protéger :

```pas
unit SecureConnectionParams;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles,
  FireDAC.Comp.Client, KeyProtection;

type
  TDBConnectionManager = class
  private
    FConnectionParams: TStringList;

    procedure LoadEncryptedParams;
    procedure SaveEncryptedParams;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConfigureConnection(Connection: TFDConnection);

    // Accesseurs pour les paramètres individuels
    function GetServer: string;
    procedure SetServer(const Value: string);

    function GetDatabase: string;
    procedure SetDatabase(const Value: string);

    function GetUsername: string;
    procedure SetUsername(const Value: string);

    function GetPassword: string;
    procedure SetPassword(const Value: string);

    property Server: string read GetServer write SetServer;
    property Database: string read GetDatabase write SetDatabase;
    property Username: string read GetUsername write SetUsername;
    property Password: string read GetPassword write SetPassword;
  end;

implementation

constructor TDBConnectionManager.Create;
begin
  inherited;

  FConnectionParams := TStringList.Create;
  LoadEncryptedParams;
end;

destructor TDBConnectionManager.Destroy;
begin
  SaveEncryptedParams;
  FConnectionParams.Free;

  inherited;
end;

procedure TDBConnectionManager.LoadEncryptedParams;
var
  IniFile: TIniFile;
  EncryptedContent: string;
  DecryptedContent: string;
begin
  FConnectionParams.Clear;

  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    EncryptedContent := IniFile.ReadString('Database', 'EncryptedParams', '');

    // Si nous avons des paramètres chiffrés, les déchiffrer
    if EncryptedContent <> '' then
    begin
      try
        // Utiliser le DPAPI pour déchiffrer
        DecryptedContent := UnprotectString(EncryptedContent);
        FConnectionParams.Text := DecryptedContent;
      except
        // En cas d'erreur, utiliser des valeurs par défaut ou demander à l'utilisateur
        FConnectionParams.Values['Server'] := 'localhost';
        FConnectionParams.Values['Database'] := 'mydb';
        FConnectionParams.Values['Username'] := 'root';
        FConnectionParams.Values['Password'] := '';
      end;
    end
    else
    begin
      // Aucune configuration existante, utiliser des valeurs par défaut
      FConnectionParams.Values['Server'] := 'localhost';
      FConnectionParams.Values['Database'] := 'mydb';
      FConnectionParams.Values['Username'] := 'root';
      FConnectionParams.Values['Password'] := '';
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TDBConnectionManager.SaveEncryptedParams;
var
  IniFile: TIniFile;
  EncryptedContent: string;
begin
  // Protéger les paramètres avec le DPAPI
  EncryptedContent := ProtectString(FConnectionParams.Text);

  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    IniFile.WriteString('Database', 'EncryptedParams', EncryptedContent);
  finally
    IniFile.Free;
  end;
end;

procedure TDBConnectionManager.ConfigureConnection(Connection: TFDConnection);
begin
  Connection.Params.Clear;
  Connection.Params.Add('Server=' + GetServer);
  Connection.Params.Add('Database=' + GetDatabase);
  Connection.Params.Add('User_Name=' + GetUsername);
  Connection.Params.Add('Password=' + GetPassword);
  Connection.Params.Add('DriverID=MySQL'); // Adapter selon votre SGBD
end;

function TDBConnectionManager.GetServer: string;
begin
  Result := FConnectionParams.Values['Server'];
end;

procedure TDBConnectionManager.SetServer(const Value: string);
begin
  FConnectionParams.Values['Server'] := Value;
end;

function TDBConnectionManager.GetDatabase: string;
begin
  Result := FConnectionParams.Values['Database'];
end;

procedure TDBConnectionManager.SetDatabase(const Value: string);
begin
  FConnectionParams.Values['Database'] := Value;
end;

function TDBConnectionManager.GetUsername: string;
begin
  Result := FConnectionParams.Values['Username'];
end;

procedure TDBConnectionManager.SetUsername(const Value: string);
begin
  FConnectionParams.Values['Username'] := Value;
end;

function TDBConnectionManager.GetPassword: string;
begin
  Result := FConnectionParams.Values['Password'];
end;

procedure TDBConnectionManager.SetPassword(const Value: string);
begin
  FConnectionParams.Values['Password'] := Value;
end;

end.
```

### Création d'un formulaire de configuration de connexion sécurisée

```pas
unit DBConfigForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, SecureConnectionParams;

type
  TFormDBConfig = class(TForm)
    EditServer: TEdit;
    EditDatabase: TEdit;
    EditUsername: TEdit;
    EditPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ButtonTest: TButton;
    ButtonSave: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    FConnectionManager: TDBConnectionManager;
  end;

var
  FormDBConfig: TFormDBConfig;

implementation

{$R *.dfm}

uses
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.UI.Intf;

procedure TFormDBConfig.FormCreate(Sender: TObject);
begin
  FConnectionManager := TDBConnectionManager.Create;

  // Remplir les champs du formulaire
  EditServer.Text := FConnectionManager.Server;
  EditDatabase.Text := FConnectionManager.Database;
  EditUsername.Text := FConnectionManager.Username;
  EditPassword.Text := FConnectionManager.Password;
end;

procedure TFormDBConfig.ButtonTestClick(Sender: TObject);
var
  Connection: TFDConnection;
begin
  // Mettre à jour les valeurs temporairement
  FConnectionManager.Server := EditServer.Text;
  FConnectionManager.Database := EditDatabase.Text;
  FConnectionManager.Username := EditUsername.Text;
  FConnectionManager.Password := EditPassword.Text;

  Connection := TFDConnection.Create(nil);
  try
    try
      Screen.Cursor := crHourGlass;

      // Configurer la connexion avec nos paramètres
      FConnectionManager.ConfigureConnection(Connection);

      // Tester la connexion
      Connection.Connected := True;

      ShowMessage('Connexion réussie !');
    except
      on E: Exception do
        ShowMessage('Erreur de connexion : ' + E.Message);
    end;
  finally
    Connection.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormDBConfig.ButtonSaveClick(Sender: TObject);
begin
  // Sauvegarder les nouveaux paramètres
  FConnectionManager.Server := EditServer.Text;
  FConnectionManager.Database := EditDatabase.Text;
  FConnectionManager.Username := EditUsername.Text;
  FConnectionManager.Password := EditPassword.Text;

  // Les paramètres seront chiffrés et sauvegardés lors de la destruction de FConnectionManager
  ModalResult := mrOk;
end;

end.
```

### Conclusion

Le chiffrement des données est une composante essentielle de la sécurité des applications modernes. En utilisant les techniques présentées dans ce chapitre, vous pouvez protéger efficacement les données sensibles dans vos applications Delphi, que ce soit au repos, en transit ou en mémoire.

Récapitulons les points clés :

1. **Utilisez toujours des algorithmes standards et éprouvés** comme AES pour le chiffrement symétrique et RSA pour le chiffrement asymétrique.

2. **Protégez vos clés de chiffrement** avec des mécanismes comme le DPAPI sous Windows.

3. **N'utilisez jamais de chiffrement pour les mots de passe**, mais plutôt des fonctions de hachage avec sel aléatoire.

4. **Chiffrez les communications réseau** en utilisant HTTPS (TLS/SSL).

5. **Effacez de manière sécurisée les données sensibles en mémoire** lorsqu'elles ne sont plus nécessaires.

6. **Respectez les réglementations** en matière de protection des données dans votre juridiction.

7. **Mettez en place une stratégie de gestion des clés** adaptée à votre application.

8. **Utilisez le chiffrement authentifié** (comme AES-GCM) pour détecter toute manipulation des données chiffrées.

Dans le prochain chapitre, nous aborderons la sécurisation des connexions réseau, qui complète les techniques de chiffrement présentées ici pour une protection complète des données.

### Exercices pratiques

1. Créez une petite application qui permet de chiffrer et déchiffrer un fichier texte avec un mot de passe.

2. Modifiez une application existante pour stocker les mots de passe des utilisateurs de manière sécurisée avec hachage et sel.

3. Implémentez un système de configuration qui stocke des informations sensibles de manière chiffrée (comme des clés d'API ou des identifiants de base de données).

4. Créez une application de carnet d'adresses qui chiffre les coordonnées des contacts (numéros de téléphone, adresses, etc.).

5. Pour les plus avancés : Implémentez un système de chiffrement de bout en bout pour l'échange de messages entre deux utilisateurs de votre application.
