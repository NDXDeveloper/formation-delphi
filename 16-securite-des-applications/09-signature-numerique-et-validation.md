# 16. Sécurité des applications
## 16.9 Signature numérique et validation

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La signature numérique est une technique cryptographique qui permet de garantir l'authenticité et l'intégrité d'un document ou d'un message. Dans un monde où les données circulent en permanence, il est crucial de pouvoir vérifier que l'information n'a pas été altérée et qu'elle provient bien de l'émetteur attendu.

Dans ce chapitre, nous allons explorer comment mettre en œuvre les signatures numériques dans vos applications Delphi, avec des exemples concrets et accessibles aux débutants.

### Comprendre le concept de signature numérique

Avant de plonger dans le code, prenons un moment pour comprendre le principe de la signature numérique avec une analogie simple :

Imaginez que vous envoyez une lettre importante par courrier. Pour prouver que vous êtes bien l'auteur de cette lettre, vous la signez avec votre signature manuscrite. Le destinataire, qui connaît votre signature, peut alors vérifier qu'elle vient bien de vous. La signature numérique fonctionne sur un principe similaire, mais avec des garanties mathématiques bien plus fortes.

La signature numérique s'appuie sur la **cryptographie asymétrique** (ou à clé publique) et implique généralement trois étapes principales :

1. **Génération d'une empreinte (hash)** du document ou des données à signer
2. **Chiffrement de cette empreinte** avec la clé privée du signataire
3. **Vérification de la signature** par le destinataire en utilisant la clé publique du signataire

### Comment fonctionne une signature numérique ?

Voici le processus simplifié :

**Pour signer :**
1. On calcule une empreinte unique (hash) du document
2. On chiffre cette empreinte avec sa clé privée (que personne d'autre ne possède)
3. Le résultat est la signature numérique

**Pour vérifier :**
1. Le destinataire calcule l'empreinte du document reçu
2. Il déchiffre la signature avec la clé publique du signataire
3. Si les deux empreintes correspondent, la signature est valide

Ce mécanisme garantit à la fois que :
- Le document n'a pas été modifié (intégrité)
- L'expéditeur est bien celui qu'il prétend être (authenticité)

### Cas d'utilisation des signatures numériques

Les signatures numériques sont utilisées dans de nombreux contextes :

- Authentification de documents électroniques (contrats, factures...)
- Vérification de l'intégrité des logiciels téléchargés
- Sécurisation des communications
- Protection contre la modification non autorisée des données
- Certification de l'identité de l'émetteur d'un message

### Mise en œuvre des signatures numériques avec Delphi

Delphi offre plusieurs façons d'implémenter les signatures numériques. Nous allons explorer trois approches principales :

1. Utilisation de l'API Windows CryptoAPI
2. Utilisation des bibliothèques cryptographiques intégrées à Delphi
3. Utilisation de bibliothèques tierces populaires

#### 1. Signature numérique avec l'API Windows CryptoAPI

L'API Windows CryptoAPI est disponible sur toutes les versions de Windows et permet d'implémenter des fonctions cryptographiques de base. Voici un exemple simplifié de classe pour gérer les signatures numériques :

```pas
unit DigitalSignature;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.WinCrypt;

type
  TCryptoAPIException = class(Exception);

  TDigitalSignature = class
  private
    FProviderHandle: HCRYPTPROV;
    FKeyPairHandle: HCRYPTKEY;
    FHashHandle: HCRYPTHASH;

    procedure CheckCryptoAPIError(Success: Boolean; const Operation: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Génération d'une paire de clés
    procedure GenerateKeyPair;

    // Signature et vérification
    function SignData(const Data: TBytes): TBytes;
    function VerifySignature(const Data, Signature: TBytes): Boolean;

    // Signature et vérification de fichiers
    function SignFile(const FileName: string): TBytes;
    function VerifyFileSignature(const FileName: string; const Signature: TBytes): Boolean;
  end;

implementation

constructor TDigitalSignature.Create;
begin
  inherited Create;

  // Acquérir un contexte cryptographique
  if not CryptAcquireContext(FProviderHandle, nil, nil,
                            PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    CheckCryptoAPIError(False, 'Create');
end;

destructor TDigitalSignature.Destroy;
begin
  // Libérer les ressources
  if FHashHandle <> 0 then
    CryptDestroyHash(FHashHandle);

  if FKeyPairHandle <> 0 then
    CryptDestroyKey(FKeyPairHandle);

  if FProviderHandle <> 0 then
    CryptReleaseContext(FProviderHandle, 0);

  inherited Destroy;
end;

procedure TDigitalSignature.CheckCryptoAPIError(Success: Boolean; const Operation: string);
var
  ErrorCode: Integer;
  ErrorMessage: string;
begin
  if not Success then
  begin
    ErrorCode := GetLastError;
    ErrorMessage := SysErrorMessage(ErrorCode);
    raise TCryptoAPIException.CreateFmt('CryptoAPI %s error: %s (Code: %d)',
                                       [Operation, ErrorMessage, ErrorCode]);
  end;
end;

procedure TDigitalSignature.GenerateKeyPair;
begin
  // Libérer la clé précédente si elle existe
  if FKeyPairHandle <> 0 then
  begin
    CryptDestroyKey(FKeyPairHandle);
    FKeyPairHandle := 0;
  end;

  // Générer une nouvelle paire de clés RSA (2048 bits)
  CheckCryptoAPIError(
    CryptGenKey(FProviderHandle, AT_SIGNATURE,
               CRYPT_EXPORTABLE or 2048 shl 16, FKeyPairHandle),
    'GenerateKeyPair'
  );
end;

function TDigitalSignature.SignData(const Data: TBytes): TBytes;
var
  Signature: TBytes;
  SignatureSize: DWORD;
begin
  if FKeyPairHandle = 0 then
    raise TCryptoAPIException.Create('Key pair not generated');

  // Créer un objet de hash
  if FHashHandle <> 0 then
  begin
    CryptDestroyHash(FHashHandle);
    FHashHandle := 0;
  end;

  // Utiliser l'algorithme SHA-256 pour le hash
  CheckCryptoAPIError(
    CryptCreateHash(FProviderHandle, CALG_SHA_256, 0, 0, FHashHandle),
    'SignData (create hash)'
  );

  // Ajouter les données au hash
  CheckCryptoAPIError(
    CryptHashData(FHashHandle, @Data[0], Length(Data), 0),
    'SignData (hash data)'
  );

  // Obtenir la taille de la signature
  SignatureSize := 0;
  CryptSignHash(FHashHandle, AT_SIGNATURE, nil, 0, nil, SignatureSize);

  // Allouer la mémoire pour la signature
  SetLength(Signature, SignatureSize);

  // Signer le hash
  CheckCryptoAPIError(
    CryptSignHash(FHashHandle, AT_SIGNATURE, nil, 0, @Signature[0], SignatureSize),
    'SignData (sign hash)'
  );

  // Détruire l'objet hash après utilisation
  CryptDestroyHash(FHashHandle);
  FHashHandle := 0;

  Result := Signature;
end;

function TDigitalSignature.VerifySignature(const Data, Signature: TBytes): Boolean;
begin
  if FKeyPairHandle = 0 then
    raise TCryptoAPIException.Create('Key pair not imported');

  // Créer un objet de hash
  if FHashHandle <> 0 then
  begin
    CryptDestroyHash(FHashHandle);
    FHashHandle := 0;
  end;

  // Utiliser l'algorithme SHA-256 pour le hash
  CheckCryptoAPIError(
    CryptCreateHash(FProviderHandle, CALG_SHA_256, 0, 0, FHashHandle),
    'VerifySignature (create hash)'
  );

  // Ajouter les données au hash
  CheckCryptoAPIError(
    CryptHashData(FHashHandle, @Data[0], Length(Data), 0),
    'VerifySignature (hash data)'
  );

  // Vérifier la signature
  Result := CryptVerifySignature(FHashHandle, @Signature[0], Length(Signature),
                                FKeyPairHandle, nil, 0);

  // Détruire l'objet hash après utilisation
  CryptDestroyHash(FHashHandle);
  FHashHandle := 0;
end;

function TDigitalSignature.SignFile(const FileName: string): TBytes;
var
  FileStream: TFileStream;
  FileData: TBytes;
begin
  // Lire le fichier
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FileData, FileStream.Size);
    FileStream.ReadBuffer(FileData[0], FileStream.Size);
  finally
    FileStream.Free;
  end;

  // Signer les données du fichier
  Result := SignData(FileData);
end;

function TDigitalSignature.VerifyFileSignature(const FileName: string; const Signature: TBytes): Boolean;
var
  FileStream: TFileStream;
  FileData: TBytes;
begin
  // Lire le fichier
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FileData, FileStream.Size);
    FileStream.ReadBuffer(FileData[0], FileStream.Size);
  finally
    FileStream.Free;
  end;

  // Vérifier la signature
  Result := VerifySignature(FileData, Signature);
end;

end.
```

#### 2. Signature numérique avec les bibliothèques cryptographiques intégrées à Delphi

Depuis Delphi 11 Alexandria, l'IDE inclut la bibliothèque `System.Crypto` qui offre des API modernes pour la cryptographie, y compris les signatures numériques.

```pas
unit ModernDigitalSignature;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding;

type
  TSignatureMode = (smRSA, smECDsa);

  TModernDigitalSignature = class
  private
    FPrivateKeyFile: string;
    FPublicKeyFile: string;
    FSignatureMode: TSignatureMode;
  public
    constructor Create(const PrivateKeyFile, PublicKeyFile: string;
                      Mode: TSignatureMode = smRSA);

    // Génération d'une paire de clés
    procedure GenerateKeyPair;

    // Signature et vérification
    function SignData(const Data: TBytes): string;
    function VerifySignature(const Data: TBytes; const Signature: string): Boolean;

    // Signature et vérification de fichiers
    function SignFile(const FileName: string): string;
    function VerifyFileSignature(const FileName, Signature: string): Boolean;
  end;

implementation

uses
  {$IF CompilerVersion >= 35} // Delphi 11 ou supérieur
  System.Crypto,
  {$ENDIF}
  System.IOUtils;

constructor TModernDigitalSignature.Create(const PrivateKeyFile, PublicKeyFile: string;
  Mode: TSignatureMode);
begin
  inherited Create;
  FPrivateKeyFile := PrivateKeyFile;
  FPublicKeyFile := PublicKeyFile;
  FSignatureMode := Mode;
end;

{$IF CompilerVersion >= 35} // Nécessite Delphi 11 ou supérieur
procedure TModernDigitalSignature.GenerateKeyPair;
var
  KeyPair: TCryptoKeyPair;
  PrivateKey, PublicKey: string;
begin
  case FSignatureMode of
    smRSA:
      begin
        // Créer une paire de clés RSA
        KeyPair := TCryptoRSA.Create;
        try
          // Générer une nouvelle paire de clés (2048 bits)
          TCryptoRSA(KeyPair).GenerateKey(2048);

          // Exporter les clés au format PEM
          PrivateKey := TCryptoRSA(KeyPair).ExportPrivateKey(TPEMFormat.pfPKCS8);
          PublicKey := TCryptoRSA(KeyPair).ExportPublicKey(TPEMFormat.pfPKCS8);
        finally
          KeyPair.Free;
        end;
      end;
    smECDsa:
      begin
        // Créer une paire de clés ECDSA
        KeyPair := TCryptoECDsa.Create;
        try
          // Générer une nouvelle paire de clés (courbe P-256)
          TCryptoECDsa(KeyPair).GenerateKey(TECCurve.c256);

          // Exporter les clés au format PEM
          PrivateKey := TCryptoECDsa(KeyPair).ExportPrivateKey(TPEMFormat.pfPKCS8);
          PublicKey := TCryptoECDsa(KeyPair).ExportPublicKey(TPEMFormat.pfPKCS8);
        finally
          KeyPair.Free;
        end;
      end;
  end;

  // Sauvegarder les clés dans des fichiers
  TFile.WriteAllText(FPrivateKeyFile, PrivateKey);
  TFile.WriteAllText(FPublicKeyFile, PublicKey);
end;

function TModernDigitalSignature.SignData(const Data: TBytes): string;
var
  Signer: TCryptoSignature;
  Signature: TBytes;
begin
  case FSignatureMode of
    smRSA:
      begin
        // Créer un objet de signature RSA
        Signer := TCryptoRSASignature.Create;
        try
          // Importer la clé privée
          TCryptoRSASignature(Signer).ImportPrivateKey(
            TFile.ReadAllText(FPrivateKeyFile), TPEMFormat.pfPKCS8);

          // Signer les données avec SHA-256
          Signature := Signer.Sign(Data, TSignatureHashAlgorithm.SHA256);
        finally
          Signer.Free;
        end;
      end;
    smECDsa:
      begin
        // Créer un objet de signature ECDSA
        Signer := TCryptoECDsaSignature.Create;
        try
          // Importer la clé privée
          TCryptoECDsaSignature(Signer).ImportPrivateKey(
            TFile.ReadAllText(FPrivateKeyFile), TPEMFormat.pfPKCS8);

          // Signer les données avec SHA-256
          Signature := Signer.Sign(Data, TSignatureHashAlgorithm.SHA256);
        finally
          Signer.Free;
        end;
      end;
  end;

  // Encoder la signature en Base64 pour faciliter son stockage
  Result := TNetEncoding.Base64.EncodeBytesToString(Signature);
end;

function TModernDigitalSignature.VerifySignature(const Data: TBytes; const Signature: string): Boolean;
var
  Verifier: TCryptoSignature;
  SignatureBytes: TBytes;
begin
  Result := False;

  // Décoder la signature Base64
  SignatureBytes := TNetEncoding.Base64.DecodeStringToBytes(Signature);

  case FSignatureMode of
    smRSA:
      begin
        // Créer un objet de vérification RSA
        Verifier := TCryptoRSASignature.Create;
        try
          // Importer la clé publique
          TCryptoRSASignature(Verifier).ImportPublicKey(
            TFile.ReadAllText(FPublicKeyFile), TPEMFormat.pfPKCS8);

          // Vérifier la signature avec SHA-256
          Result := Verifier.Verify(Data, SignatureBytes, TSignatureHashAlgorithm.SHA256);
        finally
          Verifier.Free;
        end;
      end;
    smECDsa:
      begin
        // Créer un objet de vérification ECDSA
        Verifier := TCryptoECDsaSignature.Create;
        try
          // Importer la clé publique
          TCryptoECDsaSignature(Verifier).ImportPublicKey(
            TFile.ReadAllText(FPublicKeyFile), TPEMFormat.pfPKCS8);

          // Vérifier la signature avec SHA-256
          Result := Verifier.Verify(Data, SignatureBytes, TSignatureHashAlgorithm.SHA256);
        finally
          Verifier.Free;
        end;
      end;
  end;
end;
{$ELSE}
procedure TModernDigitalSignature.GenerateKeyPair;
begin
  raise Exception.Create('La génération de clés nécessite Delphi 11 ou supérieur. ' +
                        'Veuillez utiliser un outil externe pour générer vos clés.');
end;

function TModernDigitalSignature.SignData(const Data: TBytes): string;
begin
  raise Exception.Create('La signature numérique avec cette méthode nécessite Delphi 11 ou supérieur.');
end;

function TModernDigitalSignature.VerifySignature(const Data: TBytes; const Signature: string): Boolean;
begin
  raise Exception.Create('La vérification de signatures nécessite Delphi 11 ou supérieur.');
end;
{$ENDIF}

function TModernDigitalSignature.SignFile(const FileName: string): string;
var
  FileData: TBytes;
begin
  FileData := TFile.ReadAllBytes(FileName);
  Result := SignData(FileData);
end;

function TModernDigitalSignature.VerifyFileSignature(const FileName, Signature: string): Boolean;
var
  FileData: TBytes;
begin
  FileData := TFile.ReadAllBytes(FileName);
  Result := VerifySignature(FileData, Signature);
end;

end.
```

> [!NOTE]
> **Nécessite Delphi 11 ou supérieur.**
> Les fonctions de signature numérique modernes utilisant `System.Crypto` sont disponibles uniquement dans Delphi 11 Alexandria et versions ultérieures. Pour les versions antérieures, utilisez l'API CryptoAPI ou une bibliothèque tierce.

### Exemple d'application : Vérification de l'intégrité de fichiers

Voici un exemple concret d'application qui utilise la signature numérique pour vérifier l'intégrité des fichiers :

```pas
procedure TFormMain.btnSignClick(Sender: TObject);
var
  Signature: TModernDigitalSignature;
  SignatureStr: string;
  FileName: string;
begin
  if not OpenDialog.Execute then
    Exit;

  FileName := OpenDialog.FileName;
  StatusBar.SimpleText := 'Signature du fichier en cours...';

  Signature := TModernDigitalSignature.Create('private_key.pem', 'public_key.pem');
  try
    // Générer une paire de clés si nécessaire
    if not FileExists('private_key.pem') or not FileExists('public_key.pem') then
    begin
      StatusBar.SimpleText := 'Génération des clés...';
      Signature.GenerateKeyPair;
    end;

    // Signer le fichier
    SignatureStr := Signature.SignFile(FileName);

    // Sauvegarder la signature
    SaveDialog.FileName := ExtractFileName(FileName) + '.sig';
    if SaveDialog.Execute then
    begin
      TFile.WriteAllText(SaveDialog.FileName, SignatureStr);
      StatusBar.SimpleText := 'Fichier signé avec succès. Signature sauvegardée.';
    end;
  finally
    Signature.Free;
  end;
end;

procedure TFormMain.btnVerifyClick(Sender: TObject);
var
  Signature: TModernDigitalSignature;
  SignatureStr: string;
  FileName, SigFileName: string;
  IsValid: Boolean;
begin
  // Sélectionner le fichier à vérifier
  if not OpenDialog.Execute then
    Exit;
  FileName := OpenDialog.FileName;

  // Sélectionner le fichier de signature
  OpenDialog.Title := 'Sélectionner le fichier de signature';
  OpenDialog.Filter := 'Fichiers de signature (*.sig)|*.sig|Tous les fichiers (*.*)|*.*';
  if not OpenDialog.Execute then
    Exit;
  SigFileName := OpenDialog.FileName;

  StatusBar.SimpleText := 'Vérification de la signature...';

  Signature := TModernDigitalSignature.Create('private_key.pem', 'public_key.pem');
  try
    // Lire la signature
    SignatureStr := TFile.ReadAllText(SigFileName);

    // Vérifier la signature
    IsValid := Signature.VerifyFileSignature(FileName, SignatureStr);

    if IsValid then
    begin
      StatusBar.SimpleText := 'Signature valide ! Le fichier est authentique.';
      ShowMessage('Le fichier est authentique et n''a pas été modifié.');
    end
    else
    begin
      StatusBar.SimpleText := 'Attention : signature invalide !';
      ShowMessage('ATTENTION : La signature est invalide. Le fichier a peut-être été modifié !');
    end;
  finally
    Signature.Free;
  end;
end;
```

### Bonnes pratiques pour l'utilisation des signatures numériques

1. **Protégez vos clés privées** : Ne stockez jamais les clés privées dans votre code source ou dans un endroit accessible au public.

2. **Utilisez des algorithmes robustes** : Préférez RSA avec une clé d'au moins 2048 bits ou ECDSA avec des courbes comme P-256.

3. **Validez les données avant de les signer** : Assurez-vous que vous ne signez que des données valides et sûres.

4. **Utilisez des fonctions de hashage sécurisées** : Préférez SHA-256 ou supérieur à des algorithmes plus anciens comme MD5 ou SHA-1.

5. **Mettez en place une gestion sécurisée des clés** : Prévoyez la rotation des clés et des mécanismes de révocation.

6. **Informez les utilisateurs** : Affichez clairement les résultats de la vérification de signature pour que les utilisateurs comprennent les implications.

### Conclusion

Les signatures numériques constituent un élément fondamental de la sécurité des applications modernes. Avec Delphi, vous disposez de plusieurs options pour les mettre en œuvre, des API Windows classiques aux bibliothèques modernes intégrées dans les dernières versions.

En utilisant les signatures numériques, vous pouvez garantir l'authenticité et l'intégrité des données manipulées par vos applications, augmentant ainsi la confiance des utilisateurs et la sécurité globale de votre système.

---

> [!TIP]
> **Pour aller plus loin**
>
> - Explorez la certification de code pour signer vos applications Delphi avant distribution
> - Découvrez les infrastructures à clé publique (PKI) pour gérer les certificats à grande échelle
> - Intégrez les signatures numériques à vos processus d'authentification et de validation de documents

⏭️ [Sécurité des applications mobiles](16-securite-des-applications/10-securite-des-applications-mobiles.md)
