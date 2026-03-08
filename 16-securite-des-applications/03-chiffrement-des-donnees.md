🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.3 Chiffrement des données

## Introduction

Le chiffrement est l'une des techniques les plus importantes pour protéger les données sensibles. Il consiste à transformer des informations lisibles (texte clair) en un format illisible (texte chiffré) que seules les personnes autorisées peuvent déchiffrer.

**Analogie simple** : Imaginez que vous envoyez une lettre secrète. Le chiffrement, c'est comme écrire cette lettre dans un code secret que seul votre destinataire peut déchiffrer avec la bonne clé.

### Pourquoi chiffrer les données ?

**Protection contre le vol** : Si un attaquant accède à vos données chiffrées, il ne peut pas les lire sans la clé de déchiffrement.

**Exemples de données à chiffrer** :
- Mots de passe (avec hash plutôt que chiffrement)
- Numéros de cartes bancaires
- Données médicales
- Documents confidentiels
- Communications privées
- Données personnelles sensibles

**Règle importante** : Le chiffrement ne remplace pas la sécurité, il la complète. Même avec du chiffrement, vous devez toujours protéger l'accès à vos systèmes.

## Concepts fondamentaux

### Terminologie de base

**Texte clair (Plaintext)** : Les données originales, lisibles
```
Exemple : "MonMotDePasseSecret"
```

**Texte chiffré (Ciphertext)** : Les données après chiffrement, illisibles
```
Exemple : "8j2kL9pQ3mN5vB7wX1cZ4fR6tY0hG"
```

**Clé de chiffrement** : L'information secrète utilisée pour chiffrer et déchiffrer
```
Exemple : "MaCleSecrete123!"
```

**Algorithme de chiffrement** : La méthode mathématique utilisée pour transformer les données
```
Exemples : AES, RSA, 3DES
```

### Chiffrement vs Hachage

C'est une confusion courante, clarifions la différence :

| Chiffrement | Hachage |
|-------------|---------|
| **Réversible** : on peut déchiffrer | **Irréversible** : impossible de retrouver l'original |
| Nécessite une clé | Pas de clé nécessaire |
| Usage : protéger des données qu'on doit relire | Usage : vérifier l'intégrité, stocker des mots de passe |
| Exemple : chiffrer un numéro de carte | Exemple : hasher un mot de passe |

```
Chiffrement :
"Hello" + clé → "8k2Lp" → déchiffrement → "Hello"

Hachage :
"Hello" → "2cf24dba5fb0a30e..." (impossible de revenir en arrière)
```

## Types de chiffrement

### 1. Chiffrement symétrique

**Principe** : La même clé est utilisée pour chiffrer et déchiffrer.

**Analogie** : C'est comme un cadenas avec une seule clé. La personne qui ferme le cadenas et celle qui l'ouvre utilisent la même clé.

```
Expéditeur                          Destinataire
    |                                    |
    | Texte clair                        |
    | "Bonjour"                          |
    |                                    |
    v                                    v
Chiffre avec clé K         Déchiffre avec clé K
    |                                    |
    v                                    v
"e7Kp2m9Q"  ──────────────→  "Bonjour"
```

**Avantages** :
- Très rapide
- Efficace pour de grandes quantités de données
- Moins gourmand en ressources

**Inconvénients** :
- Il faut partager la clé secrète de manière sécurisée
- Une clé compromise = toutes les données compromises

**Algorithmes courants** :
- **AES (Advanced Encryption Standard)** : Le standard actuel, très sûr
- **3DES (Triple DES)** : Ancien, moins recommandé aujourd'hui
- **Blowfish** : Rapide, pour des données non critiques
- **ChaCha20** : Moderne, très performant sur mobile

### 2. Chiffrement asymétrique

**Principe** : Deux clés différentes sont utilisées - une clé publique pour chiffrer, une clé privée pour déchiffrer.

**Analogie** : C'est comme une boîte aux lettres. N'importe qui peut y déposer un courrier (clé publique), mais seul le propriétaire peut l'ouvrir (clé privée).

```
Alice                                   Bob
  |                                      |
  | Clé publique de Bob                 | Clé privée de Bob
  v                                      v
Chiffre "Secret"           Déchiffre avec clé privée
  |                                      |
  v                                      v
"9Km2pL5Q" ──────────────→    "Secret"
```

**Avantages** :
- Pas besoin de partager une clé secrète
- Permet la signature numérique

**Inconvénients** :
- Plus lent que le chiffrement symétrique
- Limité en taille de données

**Algorithmes courants** :
- **RSA** : Le plus utilisé, très fiable
- **ECC (Elliptic Curve Cryptography)** : Plus rapide, clés plus courtes
- **DSA** : Pour les signatures numériques

### 3. Chiffrement hybride

**Principe** : Combine les avantages des deux méthodes.

**Fonctionnement** :
1. Générer une clé symétrique aléatoire
2. Chiffrer les données avec cette clé symétrique (rapide)
3. Chiffrer la clé symétrique avec la clé publique du destinataire (asymétrique)
4. Envoyer les données chiffrées + la clé chiffrée

**Usage** : C'est ce que fait HTTPS ! Les données volumineuses sont chiffrées en symétrique, mais la clé est échangée de manière sécurisée en asymétrique.

## Implémentation en Delphi

### Utilisation de System.Hash pour le hachage

Bien que le hachage ne soit pas du chiffrement, il est souvent utilisé en complément :

```pascal
uses
  System.Hash, System.SysUtils;

// Hacher une chaîne avec SHA-256
function HasherTexte(const ATexte: string): string;  
begin  
  Result := THashSHA2.GetHashString(ATexte);
end;

// Exemple d'utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  TexteClair: string;
  Hash: string;
begin
  TexteClair := 'MonMotDePasse123';
  Hash := HasherTexte(TexteClair);

  ShowMessage('Original : ' + TexteClair + sLineBreak +
              'Hash SHA-256 : ' + Hash);
  // Hash SHA-256 : 8d969eef6ecad3c29a3a629280e686cf...
end;
```

### Chiffrement symétrique avec Indy

La bibliothèque Indy (Internet Direct) incluse avec Delphi offre des composants de chiffrement.

```pascal
uses
  IdGlobal, IdHashSHA, IdCoderMIME, System.SysUtils;

// Chiffrement simple avec XOR (à des fins éducatives uniquement, PAS SÉCURISÉ)
function ChiffrerXOR(const ATexte, ACle: string): string;  
var  
  i: Integer;
  TexteBytes: TIdBytes;
  CleBytes: TIdBytes;
begin
  TexteBytes := ToBytes(ATexte);
  CleBytes := ToBytes(ACle);

  for i := 0 to Length(TexteBytes) - 1 do
    TexteBytes[i] := TexteBytes[i] xor CleBytes[i mod Length(CleBytes)];

  Result := BytesToString(TexteBytes);
end;

// Note : XOR est utilisé ici pour illustration.
// Pour une vraie application, utilisez AES !
```

### Chiffrement AES avec System.NetEncoding

Delphi moderne inclut des fonctionnalités de chiffrement dans ses unités système.

```pascal
uses
  System.SysUtils, System.NetEncoding;

// Exemple de base avec encodage Base64 (pas du chiffrement, juste de l'encodage)
function EncoderBase64(const ATexte: string): string;  
begin  
  Result := TNetEncoding.Base64.Encode(ATexte);
end;

function DecoderBase64(const ATexteEncode: string): string;  
begin  
  Result := TNetEncoding.Base64.Decode(ATexteEncode);
end;

// Utilisation
procedure TForm1.BtnEncoderClick(Sender: TObject);  
begin  
  EditEncode.Text := EncoderBase64(EditClair.Text);
  // "Bonjour" devient "Qm9uam91cg=="
end;

procedure TForm1.BtnDecoderClick(Sender: TObject);  
begin  
  EditDecode.Text := DecoderBase64(EditEncode.Text);
  // "Qm9uam91cg==" redevient "Bonjour"
end;
```

**Important** : Base64 n'est PAS du chiffrement ! C'est juste un encodage. N'importe qui peut le décoder. C'est utile pour transporter des données binaires, pas pour la sécurité.

### Chiffrement AES robuste

Pour un vrai chiffrement sécurisé, utilisez une bibliothèque comme DCPcrypt ou Lockbox :

```pascal
// Exemple conceptuel avec pseudo-code
// (nécessite une bibliothèque tierce comme DCPcrypt)

uses
  DCPcrypt2, DCPrijndael, DCPsha256;

type
  TChiffrementAES = class
  private
    FCipher: TDCP_rijndael;
  public
    constructor Create(const ACle: string);
    destructor Destroy; override;
    function Chiffrer(const ATexte: string): string;
    function Dechiffrer(const ATexteChiffre: string): string;
  end;

constructor TChiffrementAES.Create(const ACle: string);  
var  
  Hash: TDCP_sha256;
  Key: array[0..31] of byte;
begin
  inherited Create;
  FCipher := TDCP_rijndael.Create(nil);

  // Créer une clé de 256 bits à partir du mot de passe
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(ACle);
    Hash.Final(Key);
  finally
    Hash.Free;
  end;

  FCipher.Init(Key, SizeOf(Key) * 8, nil);
end;

destructor TChiffrementAES.Destroy;  
begin  
  FCipher.Free;
  inherited;
end;

function TChiffrementAES.Chiffrer(const ATexte: string): string;  
var  
  Input: TBytes;
  Output: TBytes;
begin
  Input := TEncoding.UTF8.GetBytes(ATexte);
  SetLength(Output, Length(Input));

  FCipher.EncryptCBC(Input[0], Output[0], Length(Input));

  Result := TNetEncoding.Base64.EncodeBytesToString(Output);
end;

function TChiffrementAES.Dechiffrer(const ATexteChiffre: string): string;  
var  
  Input: TBytes;
  Output: TBytes;
begin
  Input := TNetEncoding.Base64.DecodeStringToBytes(ATexteChiffre);
  SetLength(Output, Length(Input));

  FCipher.DecryptCBC(Input[0], Output[0], Length(Input));

  Result := TEncoding.UTF8.GetString(Output);
end;

// Utilisation
procedure TForm1.BtnChiffrerClick(Sender: TObject);  
var  
  Chiffrement: TChiffrementAES;
begin
  Chiffrement := TChiffrementAES.Create('MaCleSecrete123!');
  try
    EditChiffre.Text := Chiffrement.Chiffrer(EditClair.Text);
  finally
    Chiffrement.Free;
  end;
end;
```

## Gestion des clés de chiffrement

La gestion des clés est **CRUCIALE**. Un bon algorithme avec une mauvaise gestion des clés = sécurité nulle.

### Principes de base

**1. Longueur des clés**

Plus la clé est longue, plus le chiffrement est sûr :
- AES-128 : 128 bits (16 octets) - Bon pour la plupart des usages
- AES-192 : 192 bits (24 octets) - Très sûr
- AES-256 : 256 bits (32 octets) - Extrêmement sûr

**2. Génération de clés**

```pascal
uses
  System.SysUtils;

// Générer une clé aléatoire
function GenererCleAleatoire(ATaille: Integer): TBytes;  
var  
  i: Integer;
begin
  SetLength(Result, ATaille);
  Randomize;
  for i := 0 to ATaille - 1 do
    Result[i] := Random(256);
end;

// Dériver une clé depuis un mot de passe (PBKDF2)
// Utilise plusieurs itérations pour ralentir les attaques par force brute
function DeriverCleDepuisMotDePasse(const AMotDePasse, ASalt: string;
  AIterations: Integer): TBytes;
begin
  // Implémentation PBKDF2
  // Nécessite une bibliothèque cryptographique
  // Exemple simplifié :
  Result := GenererCleAleatoire(32); // À remplacer par vraie implémentation
end;
```

**3. Ne jamais coder en dur les clés**

```pascal
// ❌ TRÈS MAUVAIS - clé en dur dans le code
const
  CLE_CHIFFREMENT = 'MaCleSecrete123!';

// ✅ BON - clé depuis configuration sécurisée
function ChargerCle: string;  
var  
  ConfigFile: TIniFile;
begin
  ConfigFile := TIniFile.Create(GetConfigPath);
  try
    // Lire depuis un fichier de config protégé
    // ou mieux : depuis un coffre-fort de clés
    Result := ConfigFile.ReadString('Security', 'Key', '');
  finally
    ConfigFile.Free;
  end;
end;
```

### Stockage sécurisé des clés

**Options par ordre de sécurité** :

1. **Coffre-fort système** (le plus sûr)
   - Windows : Data Protection API (DPAPI)
   - macOS : Keychain
   - Linux : Secret Service API

2. **Variables d'environnement** (acceptable)
   - Pas dans le code source
   - Configurées au déploiement

3. **Fichier de configuration protégé** (minimum)
   - Fichier avec permissions restreintes
   - Lui-même chiffré si possible

4. **Base de données chiffrée** (pour clés multiples)
   - Table dédiée aux clés
   - Chiffrées avec une clé maître

```pascal
// Exemple : Utiliser Windows DPAPI
uses
  Winapi.Windows, System.SysUtils;

function ChiffrerAvecDPAPI(const ATexte: string): TBytes;  
var  
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  TexteBytes: TBytes;
begin
  TexteBytes := TEncoding.UTF8.GetBytes(ATexte);
  DataIn.cbData := Length(TexteBytes);
  DataIn.pbData := @TexteBytes[0];

  if CryptProtectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[0], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Erreur de chiffrement DPAPI');
end;

function DechiffrerAvecDPAPI(const ATexteChiffre: TBytes): string;  
var  
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  ResultBytes: TBytes;
begin
  DataIn.cbData := Length(ATexteChiffre);
  DataIn.pbData := @ATexteChiffre[0];

  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(ResultBytes, DataOut.cbData);
    Move(DataOut.pbData^, ResultBytes[0], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
    Result := TEncoding.UTF8.GetString(ResultBytes);
  end
  else
    raise Exception.Create('Erreur de déchiffrement DPAPI');
end;
```

## Chiffrement de fichiers

### Chiffrer un fichier complet

```pascal
uses
  System.Classes, System.SysUtils, System.IOUtils;

procedure ChiffrerFichier(const AFichierSource, AFichierDestination, ACle: string);  
var  
  StreamSource: TFileStream;
  StreamDest: TFileStream;
  Buffer: TBytes;
  Chiffrement: TChiffrementAES; // Classe définie précédemment
begin
  StreamSource := TFileStream.Create(AFichierSource, fmOpenRead);
  StreamDest := TFileStream.Create(AFichierDestination, fmCreate);
  Chiffrement := TChiffrementAES.Create(ACle);
  try
    // Lire le fichier source
    SetLength(Buffer, StreamSource.Size);
    StreamSource.Read(Buffer[0], StreamSource.Size);

    // Chiffrer
    // (Simplification - en réalité, traiter par blocs pour gros fichiers)
    Buffer := ChiffrerBuffer(Buffer, Chiffrement);

    // Écrire le fichier chiffré
    StreamDest.Write(Buffer[0], Length(Buffer));
  finally
    Chiffrement.Free;
    StreamDest.Free;
    StreamSource.Free;
  end;
end;

procedure DechiffrerFichier(const AFichierChiffre, AFichierDestination, ACle: string);  
var  
  StreamSource: TFileStream;
  StreamDest: TFileStream;
  Buffer: TBytes;
  Chiffrement: TChiffrementAES;
begin
  StreamSource := TFileStream.Create(AFichierChiffre, fmOpenRead);
  StreamDest := TFileStream.Create(AFichierDestination, fmCreate);
  Chiffrement := TChiffrementAES.Create(ACle);
  try
    // Lire le fichier chiffré
    SetLength(Buffer, StreamSource.Size);
    StreamSource.Read(Buffer[0], StreamSource.Size);

    // Déchiffrer
    Buffer := DechiffrerBuffer(Buffer, Chiffrement);

    // Écrire le fichier déchiffré
    StreamDest.Write(Buffer[0], Length(Buffer));
  finally
    Chiffrement.Free;
    StreamDest.Free;
    StreamSource.Free;
  end;
end;

// Utilisation
procedure TForm1.BtnChiffrerFichierClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    ChiffrerFichier(
      OpenDialog1.FileName,
      ChangeFileExt(OpenDialog1.FileName, '.encrypted'),
      'MaCleSecrete123!'
    );
    ShowMessage('Fichier chiffré avec succès');
  end;
end;
```

### Chiffrement par blocs pour gros fichiers

Pour de gros fichiers, chiffrez par blocs pour éviter de saturer la mémoire :

```pascal
procedure ChiffrerGrosFichier(const AFichierSource, AFichierDestination, ACle: string);  
const  
  TAILLE_BLOC = 1024 * 1024; // 1 Mo par bloc
var
  StreamSource: TFileStream;
  StreamDest: TFileStream;
  Buffer: TBytes;
  BytesLus: Integer;
  Chiffrement: TChiffrementAES;
begin
  StreamSource := TFileStream.Create(AFichierSource, fmOpenRead);
  StreamDest := TFileStream.Create(AFichierDestination, fmCreate);
  Chiffrement := TChiffrementAES.Create(ACle);
  try
    SetLength(Buffer, TAILLE_BLOC);

    repeat
      // Lire un bloc
      BytesLus := StreamSource.Read(Buffer[0], TAILLE_BLOC);

      if BytesLus > 0 then
      begin
        // Chiffrer ce bloc
        SetLength(Buffer, BytesLus);
        Buffer := ChiffrerBuffer(Buffer, Chiffrement);

        // Écrire le bloc chiffré
        StreamDest.Write(Buffer[0], Length(Buffer));
        SetLength(Buffer, TAILLE_BLOC);
      end;
    until BytesLus = 0;
  finally
    Chiffrement.Free;
    StreamDest.Free;
    StreamSource.Free;
  end;
end;
```

## Chiffrement de bases de données

### 1. Chiffrement au niveau des colonnes

Chiffrez uniquement les colonnes sensibles :

```sql
CREATE TABLE Clients (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    Nom VARCHAR(100),
    Email VARCHAR(100),
    -- Colonnes chiffrées (stockées en Base64 après chiffrement)
    NumeroCarteChiffre TEXT,
    AdresseChiffree TEXT
);
```

```pascal
procedure InsererClientAvecDonneesChiffrees;  
var  
  Query: TFDQuery;
  Chiffrement: TChiffrementAES;
  NumeroCarte: string;
  NumeroCarteChiffre: string;
begin
  NumeroCarte := '1234-5678-9012-3456';

  Chiffrement := TChiffrementAES.Create('CleDeLaBase123!');
  try
    NumeroCarteChiffre := Chiffrement.Chiffrer(NumeroCarte);

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'INSERT INTO Clients (Nom, Email, NumeroCarteChiffre) ' +
                        'VALUES (:Nom, :Email, :Carte)';
      Query.ParamByName('Nom').AsString := 'Jean Dupont';
      Query.ParamByName('Email').AsString := 'jean@example.com';
      Query.ParamByName('Carte').AsString := NumeroCarteChiffre;
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  finally
    Chiffrement.Free;
  end;
end;

function LireNumeroCarteClient(AIDClient: Integer): string;  
var  
  Query: TFDQuery;
  Chiffrement: TChiffrementAES;
  NumeroCarteChiffre: string;
begin
  Query := TFDQuery.Create(nil);
  Chiffrement := TChiffrementAES.Create('CleDeLaBase123!');
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT NumeroCarteChiffre FROM Clients WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := AIDClient;
    Query.Open;

    if not Query.IsEmpty then
    begin
      NumeroCarteChiffre := Query.FieldByName('NumeroCarteChiffre').AsString;
      Result := Chiffrement.Dechiffrer(NumeroCarteChiffre);
    end
    else
      Result := '';
  finally
    Chiffrement.Free;
    Query.Free;
  end;
end;
```

### 2. Chiffrement de la connexion à la base

FireDAC supporte SSL/TLS pour les connexions MySQL :

```pascal
procedure ConfigurerConnexionSSL;  
begin  
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=mabase');
  FDConnection1.Params.Add('User_Name=utilisateur');
  FDConnection1.Params.Add('Password=motdepasse');

  // Activer SSL/TLS
  FDConnection1.Params.Add('UseSSL=True');
  FDConnection1.Params.Add('SSLCert=client-cert.pem');
  FDConnection1.Params.Add('SSLKey=client-key.pem');
  FDConnection1.Params.Add('SSLCA=ca-cert.pem');

  FDConnection1.Connected := True;
end;
```

### 3. Chiffrement transparent de la base (TDE)

Certains SGBD comme SQL Server et Oracle proposent le TDE (Transparent Data Encryption) qui chiffre toute la base automatiquement. Configurez-le côté serveur, Delphi n'a rien de spécial à faire.

## Chiffrement des communications réseau

### HTTPS avec TRESTClient

```pascal
uses
  REST.Client, REST.Types, System.JSON;

procedure AppelerAPISecurisee;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://api.example.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // Configurer l'authentification
    RESTRequest.AddAuthParameter('Authorization', 'Bearer VotreTokenAPI',
                                  TRESTRequestParameterKind.pkHTTPHEADER,
                                  [TRESTRequestParameterOption.poDoNotEncode]);

    RESTRequest.Resource := 'users';
    RESTRequest.Method := TRESTRequestMethod.rmGET;

    // Exécuter la requête (automatiquement en HTTPS)
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
      ShowMessage('Données reçues : ' + RESTResponse.Content)
    else
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

### Socket SSL avec Indy

```pascal
uses
  IdSSLOpenSSL, IdTCPClient;

procedure ConnexionSecuriseeSSL;  
var  
  Client: TIdTCPClient;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  Client := TIdTCPClient.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configurer SSL
    SSL.SSLOptions.Method := sslvTLSv1_2;
    SSL.SSLOptions.Mode := sslmClient;

    Client.IOHandler := SSL;
    Client.Host := 'secure.example.com';
    Client.Port := 443;

    // Se connecter
    Client.Connect;

    // Envoyer des données chiffrées
    Client.IOHandler.WriteLn('GET / HTTP/1.1');
    Client.IOHandler.WriteLn('Host: secure.example.com');
    Client.IOHandler.WriteLn('');

    // Lire la réponse chiffrée
    ShowMessage(Client.IOHandler.ReadLn);
  finally
    Client.Free;
    SSL.Free;
  end;
end;
```

## Vecteurs d'initialisation (IV)

Un IV est une valeur aléatoire utilisée avec la clé de chiffrement pour garantir que le même texte ne produise pas toujours le même résultat chiffré.

**Importance** : Sans IV, un attaquant peut détecter des motifs dans les données chiffrées.

```pascal
procedure ChiffrerAvecIV(const ATexte: string; const ACle: TBytes; out ATexteChiffre, AIV: TBytes);  
var  
  i: Integer;
begin
  // Générer un IV aléatoire
  SetLength(AIV, 16); // 16 octets pour AES
  Randomize;
  for i := 0 to 15 do
    AIV[i] := Random(256);

  // Chiffrer avec la clé ET l'IV
  // (Code simplifié - utilisez une vraie bibliothèque crypto)
  ATexteChiffre := ChiffrerAESAvecIV(ATexte, ACle, AIV);
end;

// Lors du stockage, garder l'IV avec les données chiffrées
procedure StockerDonneesChiffrees(const ATexte: string);  
var  
  Cle: TBytes;
  TexteChiffre: TBytes;
  IV: TBytes;
  Fichier: TFileStream;
begin
  Cle := ChargerCleSecurisee;
  ChiffrerAvecIV(ATexte, Cle, TexteChiffre, IV);

  Fichier := TFileStream.Create('data.encrypted', fmCreate);
  try
    // Écrire d'abord l'IV (pas secret, mais nécessaire pour déchiffrer)
    Fichier.Write(IV[0], Length(IV));
    // Puis les données chiffrées
    Fichier.Write(TexteChiffre[0], Length(TexteChiffre));
  finally
    Fichier.Free;
  end;
end;
```

## Modes de chiffrement par blocs

Les algorithmes comme AES chiffrent par blocs (128 bits). Les modes déterminent comment les blocs sont liés :

### CBC (Cipher Block Chaining)

Le plus courant. Chaque bloc dépend du précédent.

```
Bloc 1 → Chiffré → XOR avec Bloc 2 → Chiffré → ...
```

**Avantage** : Motifs cachés  
**Inconvénient** : Erreur dans un bloc affecte les suivants  

### ECB (Electronic Codebook)

**À ÉVITER** : Chiffre chaque bloc indépendamment. Les motifs restent visibles.

### GCM (Galois/Counter Mode)

**Recommandé** : Mode moderne qui offre à la fois chiffrement et authentification.

```pascal
// Exemple conceptuel avec GCM
procedure ChiffrerModeGCM(const ATexte: string);  
begin  
  // Utilise AES-GCM pour chiffrer ET authentifier
  // Garantit que les données n'ont pas été modifiées
  // Nécessite une bibliothèque supportant GCM
end;
```

## Bonnes pratiques

### ✅ À faire

**1. Utilisez des algorithmes éprouvés**
```pascal
// ✅ BON - AES est standard et sûr
Chiffrement := TChiffrementAES.Create(Cle);

// ❌ MAUVAIS - algorithme "maison" non testé
Chiffrement := MonAlgoPerso.Create(Cle);
```

**2. Utilisez des clés suffisamment longues**
```pascal
// ✅ BON - 256 bits minimum pour AES
Cle := GenererCle(32); // 32 octets = 256 bits

// ❌ MAUVAIS - trop court
Cle := GenererCle(8); // 64 bits, cassable en quelques secondes
```

**3. Changez régulièrement les clés**
```pascal
// Rotation des clés tous les 90 jours
if DaysBetween(Now, DateDerniereRotation) > 90 then
  RoterCleChiffrement;
```

**4. Utilisez un IV différent à chaque chiffrement**
```pascal
// ✅ BON - IV aléatoire à chaque fois
IV := GenererIVAleatoire;

// ❌ MAUVAIS - même IV réutilisé
const IV_FIXE = '1234567890123456';
```

**5. Combinez chiffrement et authentification**
```pascal
// Utilisez HMAC pour vérifier l'intégrité
HMAC := CalculerHMAC(TexteChiffre, CleHMAC);  
StockerAvecAuthentification(TexteChiffre, HMAC);  
```

### ❌ À éviter

**1. Ne jamais implémenter votre propre algorithme de chiffrement**

Même les experts font des erreurs. Utilisez des bibliothèques éprouvées.

**2. Ne pas chiffrer ce qui doit être hashé**

```pascal
// ❌ MAUVAIS - chiffrer un mot de passe
MotDePasseChiffre := Chiffrer(MotDePasse);

// ✅ BON - hasher un mot de passe
MotDePasseHash := Hasher(MotDePasse + Salt);
```

**3. Ne pas exposer les données chiffrées telles quelles**

Même chiffrées, les données ont une valeur. Contrôlez l'accès.

**4. Ne pas oublier de nettoyer la mémoire**

```pascal
procedure UtiliserCle;  
var  
  Cle: string;
begin
  Cle := ChargerCleSecrete;
  try
    // Utiliser la clé
    ChiffrerDonnees(Cle);
  finally
    // Effacer la clé de la mémoire
    FillChar(Cle[1], Length(Cle) * SizeOf(Char), 0);
    Cle := '';
  end;
end;
```

**5. Ne pas négliger la performance**

Le chiffrement a un coût. Pour de gros volumes, optimisez :
- Chiffrez par blocs
- Utilisez le multi-threading
- Cachez les données déchiffrées si utilisées souvent

## Cas d'usage pratiques

### Application de gestion : chiffrer les données sensibles

```pascal
type
  TClientSecurise = class
  private
    FID: Integer;
    FNom: string;
    FEmail: string;
    FNumeroCarteChiffre: string;
    function GetNumeroCarte: string;
    procedure SetNumeroCarte(const Value: string);
  public
    property ID: Integer read FID write FID;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
    property NumeroCarte: string read GetNumeroCarte write SetNumeroCarte;
  end;

function TClientSecurise.GetNumeroCarte: string;  
var  
  Chiffrement: TChiffrementAES;
begin
  if FNumeroCarteChiffre = '' then
    Exit('');

  Chiffrement := TChiffrementAES.Create(CleGlobale);
  try
    Result := Chiffrement.Dechiffrer(FNumeroCarteChiffre);
  finally
    Chiffrement.Free;
  end;
end;

procedure TClientSecurise.SetNumeroCarte(const Value: string);  
var  
  Chiffrement: TChiffrementAES;
begin
  Chiffrement := TChiffrementAES.Create(CleGlobale);
  try
    FNumeroCarteChiffre := Chiffrement.Chiffrer(Value);
  finally
    Chiffrement.Free;
  end;
end;
```

### Application mobile : sauvegarder des données localement

```pascal
// Chiffrer avant de sauvegarder sur le mobile
procedure SauvegarderDonneesLocales(const ADonnees: string);  
var  
  Fichier: TFileStream;
  Chiffrement: TChiffrementAES;
  DonneesChiffrees: TBytes;
  CheminFichier: string;
begin
  CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'data.enc');

  Chiffrement := TChiffrementAES.Create(ObtenirCleAppareil);
  try
    DonneesChiffrees := Chiffrement.ChiffrerEnBytes(ADonnees);

    Fichier := TFileStream.Create(CheminFichier, fmCreate);
    try
      Fichier.Write(DonneesChiffrees[0], Length(DonneesChiffrees));
    finally
      Fichier.Free;
    end;
  finally
    Chiffrement.Free;
  end;
end;
```

### Application cloud : chiffrer avant l'envoi

```pascal
procedure EnvoyerFichierChiffre(const AFichierLocal: string);  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  FichierChiffre: TMemoryStream;
  Chiffrement: TChiffrementAES;
begin
  // Chiffrer le fichier localement
  FichierChiffre := TMemoryStream.Create;
  Chiffrement := TChiffrementAES.Create(CleUtilisateur);
  try
    ChiffrerFichierVersStream(AFichierLocal, FichierChiffre, Chiffrement);
    FichierChiffre.Position := 0;

    // Envoyer le fichier chiffré au cloud
    RESTClient := TRESTClient.Create('https://api.cloud.com');
    RESTRequest := TRESTRequest.Create(nil);
    try
      RESTRequest.Client := RESTClient;
      RESTRequest.Method := TRESTRequestMethod.rmPOST;
      RESTRequest.Resource := 'upload';

      RESTRequest.AddBody(FichierChiffre, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
      RESTRequest.Execute;

      if RESTRequest.Response.StatusCode = 200 then
        ShowMessage('Fichier chiffré envoyé avec succès')
      else
        ShowMessage('Erreur d''envoi');
    finally
      RESTRequest.Free;
      RESTClient.Free;
    end;
  finally
    Chiffrement.Free;
    FichierChiffre.Free;
  end;
end;
```

## Chiffrement et conformité RGPD

Le RGPD recommande (et parfois impose) le chiffrement des données personnelles :

**Article 32** : "pseudonymisation et chiffrement des données à caractère personnel"

**Ce qu'il faut chiffrer** :
- Numéros de sécurité sociale
- Données bancaires
- Données médicales
- Toute donnée sensible

**Avantage RGPD** : Si vos données chiffrées sont volées mais que la clé est en sécurité, vous n'avez pas à notifier la violation dans certains cas.

```pascal
// Implémenter une table d'audit du chiffrement
procedure JournaliserChiffrement(ATableau, AColonne: string);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO AuditChiffrement (TableName, ColumnName, DateChiffrement, Algorithme) ' +
      'VALUES (:Table, :Column, NOW(), :Algo)';
    Query.ParamByName('Table').AsString := ATableau;
    Query.ParamByName('Column').AsString := AColonne;
    Query.ParamByName('Algo').AsString := 'AES-256';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

## Résumé des points essentiels

✅ **Points clés à retenir** :
- Le chiffrement protège les données, pas les systèmes
- Utilisez AES-256 pour le chiffrement symétrique
- Utilisez RSA ou ECC pour le chiffrement asymétrique
- Ne réinventez jamais la roue en cryptographie
- La gestion des clés est aussi importante que l'algorithme
- Utilisez toujours un IV différent pour chaque chiffrement
- Combinez chiffrement et authentification (HMAC, GCM)
- Chiffrez les données en transit (HTTPS) et au repos (base, fichiers)

❌ **Erreurs critiques à éviter** :
- Stocker les clés dans le code source
- Utiliser des algorithmes obsolètes (DES, MD5 pour sécurité)
- Réutiliser les mêmes IV
- Chiffrer ce qui doit être hashé (mots de passe)
- Négliger la sécurisation des clés
- Oublier de chiffrer les sauvegardes

## Aller plus loin

**Prochaines sections du chapitre 16** :
- **16.4** : Sécurisation des connexions (HTTPS, SSL/TLS, certificats)
- **16.5** : Protection contre les vulnérabilités (injections, XSS, CSRF)
- **16.9** : Signature numérique et validation

**Ressources recommandées** :
- Documentation des bibliothèques cryptographiques (OpenSSL, Indy)
- Standards NIST sur la cryptographie
- Cours sur la cryptographie appliquée

Le chiffrement est une brique essentielle de la sécurité, mais il doit être combiné avec d'autres mesures (authentification, autorisation, sécurité réseau) pour une protection complète.

⏭️ [Sécurisation des connexions](/16-securite-des-applications/04-securisation-des-connexions.md)
