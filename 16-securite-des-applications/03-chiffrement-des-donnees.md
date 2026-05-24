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
- **AES (Advanced Encryption Standard)** : Le standard actuel, très sûr — clés 128/192/256 bits
- **ChaCha20** : Moderne, très performant sur mobile (alternative AES sans accélération matérielle)
- **3DES (Triple DES)** : ❌ **Déprécié par le NIST depuis 2017, interdit pour le chiffrement de nouvelles données depuis le 31 décembre 2023** (SP 800-131A Rev. 2). Bloc de 64 bits → vulnérable à Sweet32. Ne plus utiliser pour de nouveaux projets ; ne conserver que pour déchiffrer d'anciennes données.
- **Blowfish** : ❌ Même problème de bloc 64 bits que 3DES → vulnérable à Sweet32 sur les long flux. Son successeur **Twofish** (128 bits) reste sûr mais n'est plus très utilisé. Préférez AES ou ChaCha20.
- **DES** : ❌ Cassé depuis 1998 (force brute en quelques heures). À ne JAMAIS utiliser.

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
- **RSA** : Largement déployé. ⚠ Selon le NIST SP 800-131A Rév. 2 et les recommandations ANSSI : **RSA-2048 acceptable jusqu'à fin 2030**, **RSA-3072 minimum à partir de 2031**. Pour tout nouveau déploiement en 2026, privilégier directement 3072 bits afin d'éviter une migration prochaine. Toujours utiliser **OAEP** (padding pour le chiffrement) et **PSS** (padding pour la signature) — l'ancien PKCS#1 v1.5 a des vulnérabilités connues (Bleichenbacher).
- **ECC (Elliptic Curve Cryptography)** : plus rapide qu'RSA, clés beaucoup plus courtes (256 bits ECC ≈ 3072 bits RSA). Courbes recommandées : **P-256** (NIST), **Curve25519** (Bernstein, performance + résistance side-channel).
- **EdDSA / Ed25519** : signatures modernes sur Curve25519, ~100× plus rapides qu'RSA-3072 pour signer. Standardisées RFC 8032, supportées partout en 2026.
- **DSA** : ⚠ Largement remplacé par **ECDSA** (DSA sur courbes elliptiques) et surtout **Ed25519**. À éviter pour de nouveaux projets.

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

Pour un vrai chiffrement sécurisé, utilisez une bibliothèque éprouvée. **La RTL Delphi ne fournit pas AES nativement** — il faut donc passer par une bibliothèque tierce ou OpenSSL.

> 💡 **Bibliothèques AES recommandées pour Delphi 13 (2026)** :  
> - **OpenSSL via Indy** (`IdHashSHA`, `IdSSL`…) — éprouvé, gratuit, à jour  
> - **LockBox 3** — open source, maintenu, compatible Delphi récent  
> - **GrijjyFoundation** — `Grijjy.System.Crypto` moderne (AES-GCM, ChaCha20)  
> - **TMS Cryptography Pack** — commercial, support officiel  
> - **DCPcrypt** — historique, encore fonctionnel mais peu maintenu depuis 2014  
>  
> Préférez **AES-GCM** (authentifié) plutôt qu'AES-CBC pour tout nouveau projet : il combine chiffrement et intégrité en une seule opération.

> 🚨 **Le code ci-dessous présente PLUSIEURS faiblesses** que nous laissons volontairement pour les corriger ensuite :  
> 1. **Pas d'IV explicite** : `FCipher.Init(Key, SizeOf(Key) * 8, nil)` avec `nil` ⇒ IV nul = même donnée chiffrée donne toujours le même résultat = motifs détectables.  
> 2. **Dérivation de clé par simple SHA-256** : insuffisant si la clé vient d'un mot de passe utilisateur. Il faut **PBKDF2 / Argon2** (voir plus bas).  
> 3. **Pas d'authentification** : un attaquant peut altérer le chiffré sans qu'on le détecte. Il faut un HMAC ou utiliser un mode authentifié (GCM).  
>  
> Versions sécurisées dans la suite du chapitre.

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

  // ⚠ Hash SHA-256 simple — INSUFFISANT pour dériver une clé depuis
  //   un mot de passe utilisateur. À remplacer par PBKDF2 ou Argon2
  //   (voir la section « Dérivation de clé » plus bas).
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(ACle);
    Hash.Final(Key);
  finally
    Hash.Free;
  end;

  // ⚠ IV nul (`nil`) ! En production, générer un IV aléatoire de
  //   16 octets via un CSPRNG et le stocker à côté du chiffré.
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

  // ⚠ AES-CBC opère sur des blocs de 16 octets. Si `Length(Input)` n'est pas
  //   un multiple de 16, il faut PADDER les données (typiquement PKCS#7) :
  //   chaque octet de padding contient la valeur N = nombre d'octets à
  //   ajouter (1 à 16). L'absence de padding produit un crash ou une
  //   troncature silencieuse en fin de message.
  //
  //   Code ci-dessous : suppose que la bibliothèque DCPcrypt gère le
  //   padding interne (vrai pour `EncryptStream` ; faux pour `EncryptCBC`
  //   dans l'API « bas niveau »). Pour une vraie implémentation :
  //     1. Ajouter le padding PKCS#7 manuellement avant l'appel.
  //     2. Allouer Output de taille `Input.Length + (16 - Input.Length mod 16)`.
  //     3. Au déchiffrement, lire le dernier octet pour connaître le padding
  //        à retirer.
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

> 🚨 **`Random` n'est PAS cryptographiquement sûr.** Le générateur pseudo-aléatoire de Delphi est prévisible : un attaquant qui devine la *seed* peut reconstruire toutes les clés générées. Pour la cryptographie, il faut un **CSPRNG** (*Cryptographically Secure PRNG*) :  
> - **Windows** : `BCryptGenRandom` (API CNG) ou l'ancien `CryptGenRandom`  
> - **Linux / Android** : `getrandom(2)` ou `/dev/urandom`  
> - **macOS / iOS** : `SecRandomCopyBytes` (`Security.framework`)  
> - **Cross-platform** : OpenSSL `RAND_bytes` (via `IdSSLOpenSSL`)

```pascal
uses
  System.SysUtils, System.Hash, System.NetEncoding;

// ❌ NE PAS FAIRE — version naïve qui montre le piège :
// function GenererCleAleatoire(ATaille: Integer): TBytes;
// var i: Integer;
// begin
//   SetLength(Result, ATaille);
//   Randomize;                        // Graine = horloge système, prédictible
//   for i := 0 to ATaille - 1 do
//     Result[i] := Random(256);       // Mersenne Twister, pas cryptographique
// end;

// ✅ Version correcte : utiliser le CSPRNG du système d'exploitation.
//    `RemplirOctetsCSPRNG` est défini en section 16.1 (BCryptGenRandom sur
//    Windows, /dev/urandom sur Linux, SecRandomCopyBytes sur macOS/iOS).
function GenererCleAleatoire(ATaille: Integer): TBytes;  
begin  
  SetLength(Result, ATaille);
  RemplirOctetsCSPRNG(Result);
end;

// ✅ Dérive une clé robuste depuis un mot de passe via PBKDF2-HMAC-SHA-256.
//    Disponible NATIVEMENT dans System.Hash depuis Delphi 11 — aucune
//    bibliothèque externe nécessaire.
function DeriverCleDepuisMotDePasse(const AMotDePasse: string;
  const ASalt: TBytes; AIterations, ATailleCleOctets: Integer): TBytes;
begin
  Result := THashPBKDF2_SHA256.GetHashBytes(
    AMotDePasse, ASalt, AIterations, ATailleCleOctets);
end;

// Exemple d'utilisation
procedure DemoDerivationCle;  
var  
  Sel, Cle: TBytes;
begin
  // 1. Générer un sel aléatoire (16 octets minimum)
  Sel := GenererCleAleatoire(16);

  // 2. Dériver une clé AES-256 (32 octets) avec 600 000 itérations
  //    (recommandation OWASP 2026 pour SHA-256)
  Cle := DeriverCleDepuisMotDePasse('MotDePasseUtilisateur', Sel, 600000, 32);

  // 3. Stocker le sel à côté du chiffré (pas secret) ; la clé ne sort
  //    jamais de la mémoire.
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
// ⚠ Les types `DATA_BLOB`, `CryptProtectData`, `CryptUnprotectData` sont
//   définis dans l'unité `Winapi.Wincrypt`, PAS dans `Winapi.Windows`.
//   Selon la version de Delphi, certains noms peuvent aussi se trouver
//   dans `JwaWinCrypt` (JEDI). Ajuster vos `uses` en conséquence.
uses
  Winapi.Windows, Winapi.Wincrypt, System.SysUtils;

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
  // ⚠ CETTE IMPLÉMENTATION CHARGE TOUT LE FICHIER EN MÉMOIRE.
  //   Pour un fichier de 100 Mo : pas de problème. Pour un fichier de
  //   plusieurs Go, le process plante (OutOfMemory) ou ralentit
  //   considérablement (swap). Voir la version « gros fichiers » plus bas.
  //
  // ⚠ `Read(Buffer[0], N)` peut lire MOINS de N octets (rare mais légal
  //   pour les flux réseau, pipes, etc.). Pour un fichier, `ReadBuffer`
  //   est préférable : il lève une exception si la lecture est
  //   incomplète, évitant un buffer partiellement initialisé.
  StreamSource := TFileStream.Create(AFichierSource, fmOpenRead or fmShareDenyWrite);
  StreamDest := TFileStream.Create(AFichierDestination, fmCreate);
  Chiffrement := TChiffrementAES.Create(ACle);
  try
    // Lire le fichier source d'un coup (ReadBuffer = lecture exhaustive)
    SetLength(Buffer, StreamSource.Size);
    StreamSource.ReadBuffer(Buffer, StreamSource.Size);

    // Chiffrer
    Buffer := ChiffrerBuffer(Buffer, Chiffrement);

    // Écrire le fichier chiffré (WriteBuffer = écriture exhaustive)
    StreamDest.WriteBuffer(Buffer, Length(Buffer));
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

> 💡 **Recommandation PCI DSS pour les numéros de carte** : ne JAMAIS afficher le PAN (Primary Account Number) complet à l'utilisateur. Masquer tout sauf les 6 premiers et les 4 derniers chiffres (`1234 56XX XXXX 3456`). Pour le journal de transaction, ne conserver que le PAN masqué ou un *token* irréversible (tokenisation). Et de toute façon, **ne stockez pas de numéros de carte si vous n'avez pas la certification PCI DSS appropriée** : déléguez cela à un prestataire (Stripe, Adyen, Mollie) qui retourne juste un identifiant de carte ré-utilisable.

> ⚠️ **Données déchiffrées en mémoire** : une fois en clair côté application, le numéro reste vulnérable aux *core dumps*, à l'inspection par debugger, ou au swap disque. Limitez la durée de vie : déchiffrer juste avant l'usage, écraser le buffer (`FillChar`) juste après. Si vous manipulez les données par `string`, sachez que la copy-on-write de Delphi rend l'effacement difficile : préférez `TBytes`.

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

  // ⚠ IMPORTANT : préciser le mode de vérification du certificat serveur.
  //   Sans cela, MySQL accepte un certificat auto-signé ou un certificat
  //   pour un autre serveur → attaque MITM possible.
  //   Valeurs MySQL : DISABLED, PREFERRED, REQUIRED, VERIFY_CA, VERIFY_IDENTITY.
  //   VERIFY_IDENTITY = vérifie CA + correspondance du nom d'hôte (production).
  FDConnection1.Params.Add('SSLMode=VERIFY_IDENTITY');

  // Certificat de la CA pour vérifier le serveur (obligatoire avec VERIFY_*)
  FDConnection1.Params.Add('SSLCA=ca-cert.pem');

  // Certificat client pour authentification mutuelle (mTLS) — optionnel
  FDConnection1.Params.Add('SSLCert=client-cert.pem');
  FDConnection1.Params.Add('SSLKey=client-key.pem');

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
    // Configurer SSL — accepter TLS 1.2 ET 1.3, refuser tout en-dessous.
    // (TLS 1.0/1.1 sont obsolètes depuis 2020 ; SSLv3 et antérieurs interdits.)
    SSL.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_3];
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
procedure ChiffrerAvecIV(const ATexte: string; const ACle: TBytes;
                          out ATexteChiffre, AIV: TBytes);
begin
  // ⚠ L'IV doit provenir d'un CSPRNG, JAMAIS de `Random`. Un IV prédictible
  //   pour AES-CBC permet une attaque par clair choisi (BEAST, 2011).
  //   Pour AES-GCM, un IV réutilisé est CATASTROPHIQUE : il révèle la clé
  //   d'authentification GMAC, l'attaquant peut forger des messages.
  SetLength(AIV, 16); // 16 octets pour AES-CBC, 12 octets pour AES-GCM
  RemplirOctetsCSPRNG(AIV);   // wrapper plate-forme — voir section 16.1

  // Chiffrer avec la clé ET l'IV
  // (Code simplifié - utilisez une vraie bibliothèque crypto)
  ATexteChiffre := ChiffrerAESAvecIV(ATexte, ACle, AIV);
end;

// Lors du stockage, garder l'IV avec les données chiffrées.
// Format suggéré : [IV][Ciphertext]  ou  [IV][Tag GCM][Ciphertext]
procedure StockerDonneesChiffrees(const ATexte: string);  
var  
  Cle: TBytes;
  TexteChiffre: TBytes;
  IV: TBytes;
  Fichier: TFileStream;
begin
  Cle := ChargerCleSecurisee;
  ChiffrerAvecIV(ATexte, Cle, TexteChiffre, IV);

  // ⚠ Toujours utiliser `WriteBuffer` (lève une exception si écriture
  //   incomplète) plutôt que `Write` (retourne silencieusement le
  //   nombre d'octets écrits).
  Fichier := TFileStream.Create('data.encrypted', fmCreate);
  try
    // Écrire d'abord l'IV (pas secret, mais nécessaire pour déchiffrer)
    Fichier.WriteBuffer(Pointer(IV)^, Length(IV));
    // Puis les données chiffrées
    Fichier.WriteBuffer(Pointer(TexteChiffre)^, Length(TexteChiffre));
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

**Recommandé** : Mode moderne qui offre à la fois chiffrement et authentification (AEAD — *Authenticated Encryption with Associated Data*). Le chiffré est accompagné d'un *tag d'authentification* de 16 octets ; toute altération est détectée au déchiffrement.

```pascal
// Pattern d'usage AES-256-GCM via OpenSSL (Indy fournit IdSSLOpenSSLHeaders).
// Note : System.Hash ne contient PAS d'AES — il faut OpenSSL, LockBox ou TMS.
//
// Format du chiffré stocké : [IV 12 octets][Ciphertext][Tag 16 octets]
//
// 1. Chiffrement :
//    - Générer IV de 12 octets via CSPRNG (NE JAMAIS réutiliser !)
//    - EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), nil, nil, nil)
//    - EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 12, nil)
//    - EVP_EncryptInit_ex(ctx, nil, nil, Key, IV)
//    - (option) EVP_EncryptUpdate(ctx, nil, ..., AAD, Length(AAD)) // données associées
//    - EVP_EncryptUpdate(ctx, OutBuf, ..., InBuf, Length(InBuf))
//    - EVP_EncryptFinal_ex(ctx, ...)
//    - EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16, Tag)
//
// 2. Déchiffrement : mêmes appels avec EVP_Decrypt*. SET_TAG AVANT le Final.
//    Si EVP_DecryptFinal_ex retourne 0 → le tag ne matche pas → MESSAGE ALTÉRÉ.
//    Dans ce cas, NE PAS retourner le clair partiel, lever une exception.
```

### ChaCha20-Poly1305

Alternative AEAD à AES-GCM, **recommandée par TLS 1.3** et particulièrement utile sur les appareils mobiles qui n'ont pas d'accélération matérielle AES-NI :

- **Performance** : 3 à 5× plus rapide qu'AES sur ARM sans extensions cryptographiques.
- **Sécurité** : même niveau qu'AES-256, sans les pièges de timing des implémentations AES non protégées.
- **Standardisation** : RFC 8439 (2018), supporté par OpenSSL ≥ 1.1.0 (`EVP_chacha20_poly1305()`).

Préférez **ChaCha20-Poly1305 sur mobile et IoT**, **AES-256-GCM sur serveur** (où AES-NI est disponible).

> 🔮 **Cryptographie post-quantique (PQC)** : NIST a finalisé en août 2024 ses trois premiers standards (FIPS 203/204/205) — ML-KEM (ex-Kyber) pour l'échange de clés, ML-DSA (ex-Dilithium) et SLH-DSA (ex-SPHINCS+) pour la signature. Ils remplaceront progressivement RSA et ECC d'ici la prochaine décennie face à la menace d'un ordinateur quantique capable de casser ces deux algorithmes (algorithme de Shor). Pour AES-256 et SHA-256/512, la menace quantique est marginale (algorithme de Grover ne fait que doubler la longueur de clé effective requise) : ils restent les choix sûrs pour la décennie à venir. Surveillez l'apparition de ces algorithmes dans OpenSSL ≥ 3.5 et anticipez la « *crypto-agility* » dans vos applications (rendre les algorithmes paramétrables, pas en dur).

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

> ⚠ **Le `String` Delphi est *managé* et *copy-on-write*.** Faire `FillChar(Cle[1], ...)` sur un `string` n'efface PAS forcément la mémoire originale : Delphi peut avoir déjà copié la chaîne ailleurs (assignation, paramètre, retour de fonction). Pour vraiment effacer un secret, manipulez un `TBytes` (tableau dynamique d'octets, non managé en COW) et écrasez-le avant `SetLength(0)`.

```pascal
procedure UtiliserCleSecurise;  
var  
  CleBytes: TBytes;
begin
  CleBytes := ChargerCleSecreteEnBytes;  // jamais transformé en String !
  try
    ChiffrerDonneesAvecBytes(CleBytes);
  finally
    // Écraser explicitement, puis libérer le tableau
    if Length(CleBytes) > 0 then
      FillChar(CleBytes[0], Length(CleBytes), 0);
    SetLength(CleBytes, 0);
  end;
end;
```

> 💡 Cette précaution sert principalement contre les *crash dumps* et l'analyse mémoire post-mortem. Un attaquant ayant déjà obtenu un accès lecture au processus a souvent les moyens de tout récupérer — mais réduire la fenêtre d'exposition reste une bonne pratique défensive.

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
      // ⚠ `WriteBuffer` lève une exception si écriture incomplète ;
      //   `Write` retourne silencieusement le nombre d'octets écrits.
      //   Pour des données chiffrées, une écriture partielle = fichier
      //   illisible — donc on veut une exception.
      Fichier.WriteBuffer(Pointer(DonneesChiffrees)^, Length(DonneesChiffrees));
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

      // ⚠ Un POST d'upload réussi peut retourner 200 (OK), 201 (Created),
      //   202 (Accepted) ou 204 (No Content) selon l'API. Tester la plage 2xx.
      if (RESTRequest.Response.StatusCode >= 200) and
         (RESTRequest.Response.StatusCode < 300) then
        ShowMessage('Fichier chiffré envoyé avec succès')
      else
        ShowMessage('Erreur d''envoi : ' +
                    IntToStr(RESTRequest.Response.StatusCode));
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

**Avantage RGPD** : l'article 34§3a permet d'**éviter la notification aux personnes concernées** si les données étaient « *rendues incompréhensibles à toute personne qui n'est pas autorisée à y avoir accès* » — typiquement par chiffrement avec une clé restée non compromise. ⚠ Attention : la notification à la CNIL (article 33) reste obligatoire dans la plupart des cas, et il faut être en mesure de **prouver** que la clé n'a pas été compromise (gestion documentée des clés, audits, séparation des secrets).

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
