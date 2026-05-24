🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.9 Signature numérique et validation

## Introduction

La signature numérique est l'équivalent électronique d'une signature manuscrite, mais avec des garanties bien plus fortes. Elle permet de prouver l'authenticité d'un document ou d'un fichier et de garantir qu'il n'a pas été modifié.

**Analogie du monde réel** : Imaginez que vous envoyez une lettre scellée avec de la cire et votre cachet personnel. Le destinataire peut :
1. Vérifier que c'est bien votre cachet (authentification)
2. S'assurer que personne n'a ouvert la lettre (intégrité)

La signature numérique fait exactement cela, mais de manière cryptographique et infalsifiable.

### Pourquoi utiliser des signatures numériques ?

**Authentification** : Prouver qui a créé ou envoyé le document
- "Ce fichier provient bien de Microsoft, pas d'un pirate"

**Intégrité** : Garantir que le contenu n'a pas été modifié
- "Ce document n'a pas été altéré depuis sa signature"

**Non-répudiation** : Empêcher le déni
- "Vous ne pouvez pas nier avoir signé ce contrat"

**Cas d'usage** :
- Signature de logiciels et mises à jour
- Signature de documents officiels
- Validation de transactions
- Vérification de l'intégrité de fichiers téléchargés
- Contrats électroniques

## Signature numérique vs Chiffrement

C'est une confusion fréquente, clarifions :

| Signature numérique | Chiffrement |
|---------------------|-------------|
| **Objectif** : Authentifier et garantir l'intégrité | **Objectif** : Protéger la confidentialité |
| **Clé utilisée** : Clé privée pour signer, clé publique pour vérifier | **Clé utilisée** : Clé publique pour chiffrer, clé privée pour déchiffrer |
| **Le contenu** : Reste lisible | **Le contenu** : Devient illisible |
| **Résultat** : Document + Signature | **Résultat** : Document chiffré |

```
Signature numérique :  
Document original (lisible) + Signature (preuve d'authenticité)  
┌────────────────┐     ┌──────────┐
│  "Bonjour"     │  +  │ Signature│
│  (lisible)     │     │ (proof)  │
└────────────────┘     └──────────┘

Chiffrement :  
Document chiffré (illisible)  
┌────────────────┐
│  "8k2Lp9mQ"    │
│  (illisible)   │
└────────────────┘
```

## Comment fonctionne une signature numérique

### Le processus de signature

**Étape 1 : Calculer l'empreinte (hash)**
```
Document original → Fonction de hash → Empreinte unique
"Contrat de vente"  → SHA-256 →      "a4f5b2c8d..."
```

**Étape 2 : Signer l'empreinte avec la clé privée**
```
Empreinte → Signature avec clé privée → Signature
"a4f5b2c8d..." → RSA-PSS / ECDSA →     Signature numérique
```

> 💡 **Pédagogiquement on dit souvent « chiffrer le hash avec la clé privée »** — c'est une simplification historique vraie pour RSA PKCS#1 v1.5 mais inexacte en général. Une **signature** est une opération mathématique distincte du chiffrement :  
> - **RSA-PSS** (RFC 8017) : padding probabiliste, masque l'empreinte avant l'opération RSA. Recommandé sur PKCS#1 v1.5.  
> - **ECDSA** (NIST FIPS 186-5) : signature sur courbes elliptiques, pas du tout du « chiffrement ».  
> - **EdDSA / Ed25519** (RFC 8032) : signature déterministe, pas du chiffrement non plus.  
>  
> La signature et le chiffrement utilisent les mêmes paires de clés mais ne sont **pas symétriques** : on ne « déchiffre » pas une signature, on la **vérifie**.

**Étape 3 : Attacher la signature au document**
```
Document original + Signature = Document signé
```

### Le processus de vérification

**Étape 1 : Calculer l'empreinte du document reçu**
```
Document reçu → Fonction de hash → Empreinte calculée
```

**Étape 2 : Vérifier la signature avec la clé publique**
```
(Empreinte calculée, Signature, Clé publique) → Vérificateur → True / False
```

**Étape 3 : Résultat**
```
True  → Signature valide, document non modifié, provient bien du signataire  
False → Signature invalide OU document altéré OU mauvaise clé publique  
```

> 💡 Notez que la vérification est **directement booléenne** : on ne retrouve pas l'empreinte originale séparément — l'algorithme combine empreinte calculée + signature + clé publique et retourne juste un verdict. C'est plus simple et plus sûr (pas d'oracle pour aider une attaque).

### Schéma complet

```
SIGNATURE                           VÉRIFICATION
──────────                         ─────────────

┌─────────────┐                   ┌─────────────┐
│  Document   │                   │  Document   │
└──────┬──────┘                   └──────┬──────┘
       │                                 │
       v                                 v
  [ Hash SHA-256 ]                 [ Hash SHA-256 ]
       │                                 │
       v                                 v
  Empreinte "abc123"              Empreinte "abc123" (A)
       │
       v
[ Chiffrement ]                         Signature
[ Clé privée  ]                              │
       │                                      v
       v                              [ Déchiffrement ]
   Signature ────────────────────────>[ Clé publique  ]
                                               │
                                               v
                                        Empreinte "abc123" (B)
                                               │
                                               v
                                        Comparaison A = B ?
                                               │
                                       ┌───────┴────────┐
                                       v                v
                                    ✓ Valide      ✗ Invalide
```

## Implémentation des fonctions de hash en Delphi

### Hash SHA-256 d'une chaîne

```pascal
uses
  System.Hash, System.SysUtils;

function CalculerHashSHA256(const ATexte: string): string;  
begin  
  // ⚠ `THashSHA2.GetHashString(string)` encode le `string` Unicode (UTF-16
  //   LE en mémoire dans Delphi) avant de le hasher — depuis Delphi 10.3,
  //   l'encodage par défaut est UTF-8. Vérifiez la documentation de votre
  //   version : si vous comparez avec un hash calculé par un autre langage
  //   (Python `hashlib.sha256(s.encode("utf-8"))`, Node, openssl en CLI),
  //   il FAUT utiliser le même encodage des deux côtés. Pour lever toute
  //   ambiguïté, hashez explicitement des `TBytes` produits par
  //   `TEncoding.UTF8.GetBytes(ATexte)`.
  Result := THashSHA2.GetHashString(ATexte);
end;

// Variante non ambiguë : hash de la représentation UTF-8 explicite
function CalculerHashSHA256UTF8(const ATexte: string): string;  
begin  
  Result := THashSHA2.GetHashString(TEncoding.UTF8.GetBytes(ATexte));
end;

// Exemple d'utilisation
procedure TForm1.BtnHashClick(Sender: TObject);  
var  
  Texte: string;
  Hash: string;
begin
  Texte := 'Ceci est un document important';
  Hash := CalculerHashSHA256(Texte);

  Memo1.Lines.Add('Texte : ' + Texte);
  Memo1.Lines.Add('Hash SHA-256 : ' + Hash);

  // Si on modifie ne serait-ce qu'un caractère, le hash change complètement
  Texte := 'Ceci est un document Important'; // Majuscule à "Important"
  Hash := CalculerHashSHA256(Texte);
  Memo1.Lines.Add('Hash modifié : ' + Hash); // Complètement différent !
end;
```

### Hash d'un fichier

```pascal
function CalculerHashFichier(const ANomFichier: string): string;  
var  
  FileStream: TFileStream;
  HashSHA: THashSHA2;
  Buffer: TBytes;
  BytesLus: Integer;
const
  TAILLE_BUFFER = 8192;
begin
  FileStream := TFileStream.Create(ANomFichier, fmOpenRead or fmShareDenyWrite);
  try
    HashSHA := THashSHA2.Create;
    SetLength(Buffer, TAILLE_BUFFER);

    repeat
      BytesLus := FileStream.Read(Buffer[0], TAILLE_BUFFER);
      if BytesLus > 0 then
        HashSHA.Update(Buffer, BytesLus);
    until BytesLus = 0;

    Result := HashSHA.HashAsString;
  finally
    FileStream.Free;
  end;
end;

// Vérifier l'intégrité d'un fichier téléchargé
procedure VerifierIntegriteFichier(const AFichier, AHashAttendu: string);  
var  
  HashCalcule: string;
begin
  HashCalcule := CalculerHashFichier(AFichier);

  // ⚠ Utiliser `SameText` (comparaison insensible à la casse) plutôt que `=` :
  //   les éditeurs publient leurs checksums tantôt en majuscules
  //   (`A3F5B2C8...`), tantôt en minuscules (`a3f5b2c8...`). `THashSHA2`
  //   retourne en minuscules, mais l'attendu peut venir de n'importe où.
  if SameText(HashCalcule, AHashAttendu) then
    ShowMessage('✓ Fichier intègre - Hash correct')
  else
    ShowMessage('✗ ATTENTION : Fichier corrompu ou modifié !');
end;

// Exemple
procedure TForm1.BtnVerifierClick(Sender: TObject);  
begin  
  // Hash fourni par l'éditeur du logiciel
  VerifierIntegriteFichier(
    'C:\Downloads\application.exe',
    'a3f5b2c8d4e6f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6'
  );
end;
```

## Signature RSA basique

### Génération de paires de clés

```pascal
uses
  System.SysUtils, System.Classes;

type
  TPaireCles = record
    ClePrivee: string;
    ClePublique: string;
  end;

// Note : Pour une vraie application, utilisez une bibliothèque crypto robuste
// comme OpenSSL ou les composants Indy
function GenererPaireClesRSA: TPaireCles;  
begin  
  // Génération simplifiée pour l'exemple
  // En production, utilisez une vraie bibliothèque RSA

  Result.ClePrivee := 'PRIVATE_KEY_PLACEHOLDER';
  Result.ClePublique := 'PUBLIC_KEY_PLACEHOLDER';

  // Avec une vraie bibliothèque :
  // RSA.GenerateKeys(2048);
  // Result.ClePrivee := RSA.PrivateKey;
  // Result.ClePublique := RSA.PublicKey;
end;
```

### Signature et vérification conceptuelle

```pascal
type
  TSignatureNumerique = class
  private
    FClePrivee: string;
    FClePublique: string;
  public
    constructor Create(const AClePrivee, AClePublique: string);
    function SignerDocument(const ADocument: string): string;
    function VerifierSignature(const ADocument, ASignature: string): Boolean;
  end;

constructor TSignatureNumerique.Create(const AClePrivee, AClePublique: string);  
begin  
  inherited Create;
  FClePrivee := AClePrivee;
  FClePublique := AClePublique;
end;

function TSignatureNumerique.SignerDocument(const ADocument: string): string;  
var  
  Hash: string;
begin
  // 1. Calculer le hash du document
  Hash := THashSHA2.GetHashString(ADocument);

  // 2. ⚠️ STUB PÉDAGOGIQUE — NE JAMAIS UTILISER EN PRODUCTION
  // L'expression ci-dessous ne « signe » rien : elle préfixe simplement le
  // hash par un texte constant. N'importe qui peut reproduire ce résultat
  // sans posséder la clé privée. Le code n'est ici que pour illustrer
  // l'ordre des étapes (hasher puis chiffrer le hash).
  Result := 'SIGNATURE_' + Hash;

  // Vraie implémentation (PKCS#1 v1.5 ou RSASSA-PSS, voir OpenSSL via Indy
  // ou la classe `THashFactory` couplée à OpenSSL) :
  //   Result := RSA.SignPKCS1v15(Hash, FClePrivee, 'SHA256');
  // Au minimum, RSA-3072 bits ou ECDSA P-256 sont recommandés en 2026.
  raise Exception.Create(
    'SignerDocument : implémentation factice. Voir documentation OpenSSL ' +
    'ou SecureBlackbox pour une vraie signature RSA/ECDSA.');
end;

function TSignatureNumerique.VerifierSignature(const ADocument, ASignature: string): Boolean;  
var  
  HashCalcule: string;
  HashDechiffre: string;
begin
  // ⚠️ STUB pédagogique — symétrique à SignerDocument ci-dessus.
  // En production, appeler `RSA.VerifyPKCS1v15(HashCalcule, ASignature,
  // FClePublique, 'SHA256')` et utiliser une comparaison à temps constant
  // pour le booléen retourné (voir HashEgalTempsConstant en section 16.1).
  HashCalcule := THashSHA2.GetHashString(ADocument);
  HashDechiffre := StringReplace(ASignature, 'SIGNATURE_', '', []);
  Result := (HashCalcule = HashDechiffre);
end;

// Utilisation
procedure TForm1.BtnSignerClick(Sender: TObject);  
var  
  Signature: TSignatureNumerique;
  Document: string;
  SignatureDoc: string;
  EstValide: Boolean;
  Paire: TPaireCles;
begin
  // Générer une paire de clés
  Paire := GenererPaireClesRSA;

  Signature := TSignatureNumerique.Create(Paire.ClePrivee, Paire.ClePublique);
  try
    Document := MemoDocument.Lines.Text;

    // Signer le document
    SignatureDoc := Signature.SignerDocument(Document);
    MemoSignature.Lines.Text := SignatureDoc;
    ShowMessage('Document signé');

    // Vérifier la signature
    EstValide := Signature.VerifierSignature(Document, SignatureDoc);

    if EstValide then
      ShowMessage('✓ Signature valide')
    else
      ShowMessage('✗ Signature invalide');
  finally
    Signature.Free;
  end;
end;
```

## Empreinte de fichiers avec Indy

> ⚠️ **Vocabulaire** : ce qui suit est une **empreinte** (checksum, hash), pas une **signature**. Une vraie signature implique une clé privée et offre des garanties d'**authenticité** en plus de l'intégrité. Une empreinte SHA-256 toute seule prouve uniquement qu'on a bien le même fichier — n'importe qui peut recalculer un nouveau hash pour un fichier modifié. Pour la vraie signature, voir la section *Code Signing* plus bas.

Pour une implémentation robuste de l'empreinte, utilisez Indy (inclus avec Delphi) :

```pascal
uses
  IdHashSHA, IdGlobal, System.SysUtils;

type
  TSignatureFichier = class
  public
    class function CalculerEmpreinte(const AFichier: string): string;
    class procedure SauvegarderEmpreinte(const AFichier, AFichierSignature: string);
    class function VerifierEmpreinte(const AFichier, AFichierSignature: string): Boolean;
  end;

class function TSignatureFichier.CalculerEmpreinte(const AFichier: string): string;  
var  
  HashSHA: TIdHashSHA256;
begin
  HashSHA := TIdHashSHA256.Create;
  try
    Result := HashSHA.HashFileAsHex(AFichier);
  finally
    HashSHA.Free;
  end;
end;

class procedure TSignatureFichier.SauvegarderEmpreinte(const AFichier, AFichierSignature: string);  
var  
  Empreinte: string;
  Signature: TStringList;
begin
  Empreinte := CalculerEmpreinte(AFichier);

  Signature := TStringList.Create;
  try
    Signature.Add('Fichier: ' + ExtractFileName(AFichier));
    // ISO 8601 + UTC pour éviter toute ambiguïté de format / fuseau
    Signature.Add('Date: ' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                                            TTimeZone.Local.ToUniversalTime(Now)));
    Signature.Add('SHA-256: ' + Empreinte);
    // Encodage UTF-8 explicite pour préserver les accents éventuels du
    // nom de fichier sur tous les systèmes
    Signature.SaveToFile(AFichierSignature, TEncoding.UTF8);
  finally
    Signature.Free;
  end;
end;

class function TSignatureFichier.VerifierEmpreinte(const AFichier, AFichierSignature: string): Boolean;  
var  
  EmpreinteCalculee: string;
  EmpreinteStockee: string;
  Signature: TStringList;
  i: Integer;
begin
  Result := False;

  if not FileExists(AFichier) or not FileExists(AFichierSignature) then
    Exit;

  // Calculer l'empreinte actuelle
  EmpreinteCalculee := CalculerEmpreinte(AFichier);

  // Lire l'empreinte stockée
  Signature := TStringList.Create;
  try
    // Préciser l'encodage UTF-8 pour rester cohérent avec l'écriture
    Signature.LoadFromFile(AFichierSignature, TEncoding.UTF8);

    for i := 0 to Signature.Count - 1 do
    begin
      if Signature[i].StartsWith('SHA-256: ') then
      begin
        EmpreinteStockee := Copy(Signature[i], 10, Length(Signature[i]));
        Break;
      end;
    end;

    // Comparer
    Result := (EmpreinteCalculee = EmpreinteStockee);
  finally
    Signature.Free;
  end;
end;

// Utilisation
procedure TForm1.BtnSignerFichierClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    TSignatureFichier.SauvegarderEmpreinte(
      OpenDialog1.FileName,
      OpenDialog1.FileName + '.sig'
    );
    ShowMessage('Signature créée : ' + OpenDialog1.FileName + '.sig');
  end;
end;

procedure TForm1.BtnVerifierFichierClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    if TSignatureFichier.VerifierEmpreinte(
         OpenDialog1.FileName,
         OpenDialog1.FileName + '.sig') then
      ShowMessage('✓ Fichier authentique et non modifié')
    else
      ShowMessage('✗ ALERTE : Fichier modifié ou signature invalide !');
  end;
end;
```

## Certificats numériques

Un certificat numérique lie une clé publique à une identité (personne, organisation, site web).

### Structure d'un certificat

```
┌────────────────────────────────────────────────────────────┐
│                CERTIFICAT NUMÉRIQUE                        │
├────────────────────────────────────────────────────────────┤
│ Version    : X.509 v3                                      │
│ Numéro     : 04:8e:b6:b2:51:55:c1:b4:5b:fc:81:b3:7c:79:d1  │
│              (entier de 16+ octets, format hexadécimal)    │
│                                                            │
│ Émetteur (CA) :                                            │
│   CN = DigiCert TLS RSA SHA256 2020 CA1                    │
│   O  = DigiCert Inc                                        │
│   C  = US                                                  │
│                                                            │
│ Sujet (Propriétaire) :                                     │
│   CN = www.monentreprise.fr                                │
│   O  = MonEntreprise SAS                                   │
│   L  = Paris                                               │
│   C  = FR                                                  │
│                                                            │
│ Subject Alternative Names (SAN) :                          │
│   DNS: www.monentreprise.fr                                │
│   DNS: monentreprise.fr                                    │
│   DNS: api.monentreprise.fr                                │
│                                                            │
│ Validité (durée max 398 jours depuis sept. 2020) :         │
│   Du : 2025-09-01T00:00:00Z                                │
│   Au : 2026-09-30T23:59:59Z                                │
│                                                            │
│ Clé publique :                                             │
│   Algorithme : ECDSA P-256 (ou RSA-3072+ pour les TLS)     │
│   [Clé publique en DER]                                    │
│                                                            │
│ Extensions :                                               │
│   - Key Usage : Digital Signature, Key Encipherment        │
│   - Extended Key Usage : Server Authentication             │
│   - CRL Distribution Points : http://crl.digicert.com/...  │
│   - OCSP : http://ocsp.digicert.com                        │
│   - CT Pre-Certificate SCTs : (Certificate Transparency)   │
│                                                            │
│ Signature de la CA :                                       │
│   Algorithme : sha256WithRSAEncryption                     │
│   [Signature numérique de DigiCert]                        │
└────────────────────────────────────────────────────────────┘
```

> 💡 **Validité maximale 398 jours** : depuis septembre 2020, Apple, Google et Mozilla refusent les certificats TLS publics dont la validité dépasse **398 jours** (~13 mois). Cette limite est descendue à **47 jours** progressivement à partir de 2026 selon le calendrier du CA/Browser Forum. Conséquence : automatiser le renouvellement (Certbot/ACME) n'est plus optionnel.

### Hiérarchie de certification

```
Certificat Racine (Root CA)
  ├─ Certificat Intermédiaire
  │   ├─ Certificat de Site Web (www.example.com)
  │   └─ Certificat de Code Signing
  └─ Certificat Intermédiaire
      └─ Certificat Personnel
```

> 💡 **Vérifier la chaîne, pas juste la signature** : « la signature est valide » ne suffit pas. Une vérification complète passe par 4 étapes :  
> 1. **Vérifier la signature** elle-même (cryptographiquement).  
> 2. **Reconstituer la chaîne** : remonter de proche en proche du certificat signataire jusqu'à un certificat racine de confiance dans votre magasin (Windows : `Cert:\LocalMachine\Root`, navigateurs : Mozilla/Apple/Microsoft trust store).  
> 3. **Vérifier la validité temporelle** : `notBefore ≤ now ≤ notAfter` pour CHAQUE certificat de la chaîne (y compris les intermédiaires).  
> 4. **Vérifier la révocation** : interroger le CRL ou OCSP de l'émetteur (un certificat compromis peut être révoqué avant sa date d'expiration). **OCSP Stapling** permet au serveur de joindre la preuve OCSP avec son certificat pour éviter l'aller-retour réseau.  
>  
> `WinVerifyTrust` sur Windows fait tout cela automatiquement (cf section *Code Signing*). Sur d'autres plateformes, OpenSSL fournit `X509_verify_cert` qui couvre les 4 étapes si on lui fournit le bon contexte.

### Lire un certificat en Delphi

```pascal
uses
  IdSSLOpenSSL, IdX509;

procedure LireInformationsCertificat(const AFichierCert: string);  
var  
  Certificate: TIdX509;
begin
  Certificate := TIdX509.Create(nil);
  try
    Certificate.LoadFromFile(AFichierCert);

    Memo1.Lines.Add('=== INFORMATIONS DU CERTIFICAT ===');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Sujet (Propriétaire):');
    Memo1.Lines.Add('  ' + Certificate.Subject.OneLine);
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Émetteur (CA):');
    Memo1.Lines.Add('  ' + Certificate.Issuer.OneLine);
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Période de validité:');
    Memo1.Lines.Add('  Du: ' + DateTimeToStr(Certificate.notBefore));
    Memo1.Lines.Add('  Au: ' + DateTimeToStr(Certificate.notAfter));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Numéro de série:');
    Memo1.Lines.Add('  ' + IntToStr(Certificate.SerialNumber));
    Memo1.Lines.Add('');

    // Vérifier si le certificat est encore valide
    if Certificate.notAfter > Now then
      Memo1.Lines.Add('Statut: ✓ Valide')
    else
      Memo1.Lines.Add('Statut: ✗ EXPIRÉ');
  finally
    Certificate.Free;
  end;
end;
```

## Code Signing (Signature d'applications)

Le code signing permet de signer vos exécutables pour prouver leur authenticité.

### Pourquoi signer votre application ?

**Confiance** : Les utilisateurs savent que l'application vient de vous

**Sécurité** : Windows SmartScreen ne bloquera pas votre application

**Intégrité** : Garantie que l'exécutable n'a pas été modifié par un malware

### Obtenir un certificat de code signing

**Options** :
1. **Certificats commerciaux** (recommandé pour la production)
   - DigiCert, Sectigo, GlobalSign, SSL.com, Certum
   - Prix 2026 (depuis l'obligation de token hardware en juin 2023) :
     - **OV Code Signing** : ~300-500 €/an
     - **EV Code Signing** : ~500-1500 €/an (livré sur token USB physique)
   - Compter 1 à 4 semaines de délai pour la validation d'identité.

2. **Certificats auto-signés** (développement uniquement)
   - Gratuit
   - Non reconnu par les navigateurs/Windows (déclenche SmartScreen)
   - Utile pour les tests, le développement interne, ou pour distribuer dans une entreprise où vous installez le certificat racine sur tous les postes.

### Créer un certificat auto-signé (développement)

> ⚠️ **makecert est déprécié** depuis le SDK Windows 8.1 (2014) et n'est plus livré dans les versions récentes du Windows SDK. Microsoft recommande la cmdlet PowerShell `New-SelfSignedCertificate`.

```powershell
# PowerShell — méthode actuelle (Windows 10/11)
$cert = New-SelfSignedCertificate `
  -Type CodeSigningCert `
  -Subject "CN=MonEntreprise" `
  -KeyAlgorithm RSA -KeyLength 3072 `
  -HashAlgorithm SHA256 `
  -CertStoreLocation "Cert:\CurrentUser\My" `
  -NotAfter (Get-Date).AddYears(2)

# Exporter en .pfx pour SignTool
$pwd = ConvertTo-SecureString -String "MotDePasse123" -Force -AsPlainText
Export-PfxCertificate -Cert $cert -FilePath "MonApp.pfx" -Password $pwd
```

```batch
REM Ancienne méthode (à éviter — makecert est obsolète) :  
REM makecert -sv MonApp.pvk -n "CN=MonEntreprise" MonApp.cer -r  
REM pvk2pfx -pvk MonApp.pvk -spc MonApp.cer -pfx MonApp.pfx -po MotDePasse123  
```

### Signer un exécutable

**Avec SignTool (Windows SDK)** :

```batch
REM ⚠ Plusieurs pièges à éviter :  
REM   1. /p MotDePasse en clair ⇒ visible dans tasklist/history.  
REM      → Préférer omettre /p (SignTool demandera le PIN) ou utiliser  
REM        une variable d'environnement passée juste avant l'appel.  
REM   2. /t (timestamp Authenticode SHA-1) est LEGACY. Les certificats  
REM      modernes utilisent SHA-256 ⇒ il faut /tr (timestamp RFC 3161)  
REM      avec /td sha256.  
REM   3. Préciser /fd sha256 pour que la signature elle-même soit SHA-256.  
REM  
REM Signer avec SignTool (commande moderne 2026)  
signtool sign /f MonApp.pfx ^  
              /fd sha256 ^
              /tr http://timestamp.digicert.com ^
              /td sha256 ^
              MonApplication.exe

REM Vérifier la signature (kernel + user-mode policies)  
signtool verify /pa /v MonApplication.exe  
```

> ⚠️ **EV Code Signing et token hardware obligatoire (depuis juin 2023)** : les CIA/Baseline Requirements du CA/Browser Forum imposent que les clés privées des certificats **EV Code Signing** soient stockées sur un **module cryptographique matériel** (HSM, token USB type SafeNet eToken, YubiKey 5 FIPS). Le fichier `.pfx` exporté n'est plus accepté pour ces certificats — il faut utiliser SignTool avec `/csp` ou via le driver fourni par la CA. Depuis juin 2023, la même règle s'applique aussi aux certificats **OV Code Signing** standards. Pratiquement, vous ne recevrez plus un `.pfx` par email : un token hardware physique sera envoyé par courrier.

> 💡 **Smart App Control (Windows 11 22H2+)** : ce mode active par défaut sur les nouvelles installations Windows 11 N'EXÉCUTE QUE les binaires signés avec un certificat de **haute réputation**, ou explicitement autorisés. Un nouveau certificat (même EV) met plusieurs mois à acquérir cette réputation. Pour les nouvelles applications grand public, planifier l'obtention du certificat **bien avant** la première release publique.

**Automatiser dans Delphi** :

```pascal
procedure SignerExecutable(const AFichierExe, ACertificat, AMotDePasse: string);  
var  
  Commande: string;
  ExitCode: Cardinal;
begin
  // ⚠ Construction de la ligne de commande :
  //   - `/fd sha256` : digest de la signature (PAS sha1, déprécié) ;
  //   - `/tr` + `/td sha256` : timestamp RFC 3161 SHA-256 (PAS `/t` SHA-1) ;
  //   - mot de passe entre guillemets pour gérer les espaces et caractères
  //     spéciaux ; en alternative, passer `/p` sans valeur → SignTool
  //     demande interactivement le mot de passe (mieux pour les humains)
  //     ou utiliser une variable d'environnement.
  // ⚠ Le mot de passe en ligne de commande est visible dans le gestionnaire
  //   de tâches et l'historique shell. En CI, utiliser un secret du runner
  //   (GitHub Secrets, GitLab CI/CD variables, Azure Key Vault…).
  Commande := Format(
    'signtool.exe sign /f "%s" /p "%s" /fd sha256 ' +
    '/tr http://timestamp.digicert.com /td sha256 /v "%s"',
    [ACertificat, AMotDePasse, AFichierExe]
  );

  // Exécuter SignTool
  ExitCode := ExecuterCommande(Commande);

  if ExitCode = 0 then
    ShowMessage('✓ Application signée avec succès')
  else
    ShowMessage('✗ Erreur lors de la signature');
end;

function ExecuterCommande(const ACommande: string): Cardinal;  
var  
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  if CreateProcess(nil, PChar(ACommande), nil, nil, False,
                   CREATE_NO_WINDOW, nil, nil, StartupInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
    Result := GetLastError;
end;

// Intégration dans le build
procedure TFormBuild.BtnCompilerEtSignerClick(Sender: TObject);  
begin  
  // 1. Compiler l'application
  CompilerProjet;

  // 2. Signer l'exécutable
  SignerExecutable(
    'C:\Projets\MonApp\MonApp.exe',
    'C:\Certificats\MonApp.pfx',
    'MotDePasseSecret'
  );

  ShowMessage('Build et signature terminés');
end;
```

### Vérifier la signature d'un exécutable

```pascal
// ⚠️ Une approche basée uniquement sur GetFileVersionInfoSize ne vérifierait
// qu'un bloc VERSIONINFO est présent. Cela n'a AUCUN rapport avec une
// signature Authenticode. La VRAIE vérification doit passer par
// WinVerifyTrust de Wintrust.dll, qui :
//   1. valide la chaîne du certificat jusqu'à une CA de confiance ;
//   2. vérifie la révocation (CRL/OCSP) ;
//   3. valide l'horodatage RFC 3161.

uses
  Winapi.Windows, Winapi.WinTrust, Winapi.SoftPub, System.SysUtils;

function VerifierSignatureExecutable(const AFichier: string): Boolean;  
var  
  FileInfo: WINTRUST_FILE_INFO;
  TrustData: WINTRUST_DATA;
  Action: TGUID;
  Status: HRESULT;
  FichierW: WideString;
begin
  Action := WINTRUST_ACTION_GENERIC_VERIFY_V2;
  FichierW := AFichier;

  FillChar(FileInfo, SizeOf(FileInfo), 0);
  FileInfo.cbStruct := SizeOf(FileInfo);
  FileInfo.pcwszFilePath := PWideChar(FichierW);

  FillChar(TrustData, SizeOf(TrustData), 0);
  TrustData.cbStruct := SizeOf(TrustData);
  TrustData.dwUIChoice := WTD_UI_NONE;
  TrustData.fdwRevocationChecks := WTD_REVOKE_WHOLECHAIN;
  TrustData.dwUnionChoice := WTD_CHOICE_FILE;
  TrustData.pFile := @FileInfo;
  TrustData.dwStateAction := WTD_STATEACTION_VERIFY;

  Status := WinVerifyTrust(INVALID_HANDLE_VALUE, Action, @TrustData);
  Result := Status = ERROR_SUCCESS;

  // Libérer l'état interne maintenu par Wintrust
  TrustData.dwStateAction := WTD_STATEACTION_CLOSE;
  WinVerifyTrust(INVALID_HANDLE_VALUE, Action, @TrustData);
end;

procedure TForm1.BtnVerifierSignatureClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    if VerifierSignatureExecutable(OpenDialog1.FileName) then
      ShowMessage('✓ L''exécutable est signé')
    else
      ShowMessage('✗ L''exécutable n''est PAS signé');
  end;
end;
```

### Notarization Apple (macOS)

Sur macOS depuis 10.14.5 (2019), tout binaire signé distribué **hors App Store** doit en plus être **notarisé** par Apple, sinon Gatekeeper le bloque au premier lancement.

Le processus :
1. Signer le binaire avec un certificat **Developer ID Application** (Apple Developer Program, 99 $/an).
2. Soumettre le binaire à `notarytool` (anciennement `altool` jusqu'en novembre 2023) :
   ```bash
   xcrun notarytool submit MonApp.dmg \
                          --keychain-profile "AC_PROFILE" \
                          --wait
   ```
3. Apple scanne le binaire (analyse statique antivirus + signature) et retourne en quelques minutes un *ticket* de notarization.
4. *Stapler* le ticket sur le binaire pour qu'il fonctionne hors-ligne :
   ```bash
   xcrun stapler staple MonApp.dmg
   ```

Sans notarization, l'utilisateur voit le message *« <App> peut contenir un logiciel malveillant et ne peut être ouvert »* et doit faire un clic-droit → Ouvrir pour contourner.

> 💡 **Hardened Runtime** obligatoire pour la notarization : compiler avec `-fhardened-runtime` et déclarer dans le `Info.plist` toutes les entitlements utilisées (`com.apple.security.network.client`, etc.). Sans cela, la soumission est rejetée.

## Signature de documents PDF

Pour signer des documents PDF en Delphi :

```pascal
// Utiliser une bibliothèque comme Gnostice PDFtoolkit ou Winsoft PDF Library

type
  TSignaturePDF = class
  public
    procedure SignerPDF(const AFichierPDF, ACertificat, AMotDePasse: string);
    function VerifierSignaturePDF(const AFichierPDF: string): Boolean;
  end;

procedure TSignaturePDF.SignerPDF(const AFichierPDF, ACertificat, AMotDePasse: string);  
begin  
  // Exemple conceptuel (nécessite une bibliothèque PDF)

  // 1. Ouvrir le PDF
  // PDF := TPDFDocument.Create;
  // PDF.LoadFromFile(AFichierPDF);

  // 2. Charger le certificat
  // Cert := LoadCertificate(ACertificat, AMotDePasse);

  // 3. Signer
  // PDF.Sign(Cert, 'Signé par MonEntreprise le ' + DateToStr(Date));

  // 4. Sauvegarder
  // PDF.SaveToFile(AFichierPDF);

  ShowMessage('PDF signé');
end;
```

## Horodatage (Timestamping)

L'horodatage prouve quand un document a été signé, même après expiration du certificat.

### Pourquoi horodater ?

**Problème** : Votre certificat expire dans 1 an, mais votre signature doit rester valide 10 ans.

**Solution** : L'horodatage prouve que la signature a été créée AVANT l'expiration du certificat.

```
Sans horodatage:
  Certificat expire → Signature invalide

Avec horodatage:
  Certificat expire → Mais la signature a été créée AVANT expiration
                   → Signature reste valide ✓
```

### Serveurs d'horodatage

```pascal
const
  // Serveurs d'horodatage publics gratuits
  TIMESTAMP_DIGICERT = 'http://timestamp.digicert.com';
  TIMESTAMP_SECTIGO = 'http://timestamp.sectigo.com';
  TIMESTAMP_GLOBALSIGN = 'http://timestamp.globalsign.com';

procedure SignerAvecHorodatage(const AFichier, ACertificat: string);  
var  
  Commande: string;
begin
  // ⚠ `/t` (timestamp Authenticode legacy SHA-1) est obsolète.
  //   Utiliser `/tr` (timestamp RFC 3161) avec `/td sha256` pour un
  //   horodatage moderne. Préciser aussi `/fd sha256` pour que la
  //   signature elle-même soit SHA-256 et non SHA-1.
  Commande := Format(
    'signtool sign /f "%s" /fd sha256 /tr %s /td sha256 /v "%s"',
    [ACertificat, TIMESTAMP_DIGICERT, AFichier]
  );

  ExecuterCommande(Commande);
end;
```

## Checksum et vérification d'intégrité

Pour les fichiers téléchargeables, fournissez toujours des checksums.

### Générer des checksums multiples

```pascal
type
  TChecksums = record
    MD5: string;
    SHA1: string;
    SHA256: string;
    SHA512: string;
  end;

function CalculerTousLesChecksums(const AFichier: string): TChecksums;  
var  
  FileStream: TFileStream;
  HashMD5: THashMD5;
  HashSHA1: THashSHA1;
  HashSHA256: THashSHA2;
  HashSHA512: THashSHA2;
begin
  // ⚠️ MD5 et SHA-1 sont conservés ci-dessous uniquement pour générer un
  // checksum compatible avec d'anciens fichiers `.md5` / `.sha1` distribués.
  // Ils sont CRYPTOGRAPHIQUEMENT CASSÉS (collisions pratiques : SHAttered 2017
  // pour SHA-1, Chosen-Prefix 2009 pour MD5) et NE DOIVENT PLUS être utilisés
  // comme preuve d'intégrité contre un adversaire. SHA-256 ou SHA-512 sont
  // les seuls recommandés.

  FileStream := TFileStream.Create(AFichier, fmOpenRead or fmShareDenyWrite);
  try
    // MD5 — héritage seulement
    HashMD5 := THashMD5.Create;
    Result.MD5 := HashMD5.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-1 — héritage seulement
    HashSHA1 := THashSHA1.Create;
    Result.SHA1 := HashSHA1.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-256 — recommandé (NIST FIPS 180-4)
    HashSHA256 := THashSHA2.Create(SHA256);
    Result.SHA256 := HashSHA256.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-512 — recommandé, plus rapide sur 64 bits (NIST FIPS 180-4)
    // ⚠️ IMPORTANT : il faut INSTANCIER un nouveau THashSHA2 en précisant le
    // paramètre SHA512, sinon la valeur par défaut SHA256 est utilisée et
    // SHA256 et SHA512 retournent la même chose.
    HashSHA512 := THashSHA2.Create(SHA512);
    Result.SHA512 := HashSHA512.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure GenererFichierChecksums(const AFichier: string);  
var  
  Checksums: TChecksums;
  Fichier: TStringList;
  NomFichier: string;
begin
  Checksums := CalculerTousLesChecksums(AFichier);
  NomFichier := ExtractFileName(AFichier);

  Fichier := TStringList.Create;
  try
    Fichier.Add('Checksums pour : ' + NomFichier);
    Fichier.Add('Généré le : ' + DateTimeToStr(Now));
    Fichier.Add('');
    Fichier.Add('MD5    : ' + Checksums.MD5);
    Fichier.Add('SHA-1  : ' + Checksums.SHA1);
    Fichier.Add('SHA-256: ' + Checksums.SHA256);
    Fichier.Add('SHA-512: ' + Checksums.SHA512);

    Fichier.SaveToFile(AFichier + '.checksums.txt');
    ShowMessage('Fichier de checksums créé');
  finally
    Fichier.Free;
  end;
end;

// Utilisation
procedure TForm1.BtnGenererChecksumsClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
    GenererFichierChecksums(OpenDialog1.FileName);
end;
```

## Validation de mises à jour

Pour sécuriser les mises à jour de votre application :

```pascal
type
  TValidateurMiseAJour = class
  private
    FClePublique: string;
  public
    constructor Create(const AClePublique: string);
    function VerifierMiseAJour(const AFichierMAJ, ASignature: string): Boolean;
    procedure TelechargerEtInstaller(const AURL: string);
  end;

constructor TValidateurMiseAJour.Create(const AClePublique: string);  
begin  
  inherited Create;
  FClePublique := AClePublique;
end;

function TValidateurMiseAJour.VerifierMiseAJour(const AFichierMAJ, ASignature: string): Boolean;  
var  
  HashCalcule: string;
begin
  // 1. Calculer le hash du fichier téléchargé
  HashCalcule := CalculerHashFichier(AFichierMAJ);

  // 2. Vérifier la signature avec la clé publique (RSA-PSS ou ECDSA via OpenSSL)
  // 3. Comparer le hash signé avec celui calculé

  // ⚠️ STUB DANGEREUX — RETOURNER `True` PAR DÉFAUT EST UNE BACKDOOR.
  //    Toute mise à jour, même malveillante, sera installée. Cette
  //    implémentation factice est conservée UNIQUEMENT pour illustrer
  //    la structure ; en production, lever une exception explicite
  //    tant que la vraie vérification n'est pas branchée :
  raise Exception.Create(
    'VerifierMiseAJour : implémentation factice. Brancher OpenSSL ou ' +
    'SecureBlackbox avant de mettre cette fonction en production.');

  // Code à brancher en remplacement du raise ci-dessus :
  //   Result := RSA.VerifyPSS(HashCalcule, ASignature, FClePublique, 'SHA256');
end;

procedure TValidateurMiseAJour.TelechargerEtInstaller(const AURL: string);  
var  
  FichierMAJ: string;
  FichierSignature: string;
  HTTP: TIdHTTP;
begin
  // ⚠ `TPath.GetTempPath` est PARTAGÉ entre toutes les apps de l'utilisateur.
  //   Une app malicieuse pourrait pré-créer `update.exe` puis exploiter la
  //   fenêtre TOCTOU entre `HTTP.Get` (écrasement) et `ShellExecute`.
  //   Sécuriser en créant un sous-dossier propre à l'app, idéalement avec
  //   un nom unique par exécution (timestamp ou GUID).
  // ⚠ Le nom `update.exe` n'est pas portable : sur Linux il n'y a pas
  //   d'extension, sur macOS c'est `.app` ou `.pkg`. Adapter au contexte
  //   ou détecter via `{$IFDEF}`.

  HTTP := TIdHTTP.Create(nil);
  try
    // Télécharger la mise à jour
    FichierMAJ := TPath.Combine(TPath.GetTempPath, 'update.exe');
    HTTP.Get(AURL, FichierMAJ);

    // Télécharger la signature
    FichierSignature := TPath.Combine(TPath.GetTempPath, 'update.sig');
    HTTP.Get(AURL + '.sig', FichierSignature);

    // ⚠ `TFile.ReadAllText` lit le fichier comme du TEXTE et essaie de
    //   détecter un BOM ou de l'interpréter comme UTF-8 — cela corrompt
    //   une signature binaire RSA/ECDSA. Si le serveur sert la signature
    //   binaire (.sig brut), utiliser `TFile.ReadAllBytes` et adapter
    //   `VerifierMiseAJour` pour accepter un `TBytes`. Si la signature
    //   est servie en Base64, alors `ReadAllText` est OK et il faut la
    //   décoder côté `VerifierMiseAJour` avant comparaison.
    if VerifierMiseAJour(FichierMAJ, TFile.ReadAllText(FichierSignature)) then
    begin
      ShowMessage('✓ Mise à jour authentique, installation...');
      // Lancer l'installateur
      ShellExecute(0, 'open', PChar(FichierMAJ), nil, nil, SW_SHOW);
    end
    else
    begin
      ShowMessage('✗ ALERTE : Signature invalide ! Mise à jour refusée.');
      DeleteFile(FichierMAJ);
      DeleteFile(FichierSignature);
    end;
  finally
    HTTP.Free;
  end;
end;
```

## Blockchain et signature distribuée

Concept moderne : enregistrer les signatures dans une blockchain.

```pascal
type
  TSignatureBlockchain = class
  public
    function EnregistrerSignature(const ADocument, ASignature: string): string; // Retourne hash transaction
    function VerifierSurBlockchain(const AHashTransaction: string): Boolean;
  end;

function TSignatureBlockchain.EnregistrerSignature(const ADocument, ASignature: string): string;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONBody: TJSONObject;
begin
  // Exemple avec une API blockchain (Ethereum, Polygon, etc.)
  RESTClient := TRESTClient.Create('https://api.blockchain.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  JSONBody := TJSONObject.Create;
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := 'register';

    JSONBody.AddPair('document_hash', THashSHA2.GetHashString(ADocument));
    JSONBody.AddPair('signature', ASignature);
    JSONBody.AddPair('timestamp', IntToStr(DateTimeToUnix(Now)));

    RESTRequest.AddBody(JSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // ⚠ Ne PAS faire `(ParseJSONValue(...) as TJSONObject).GetValue(...)`
      //   en une seule expression : le TJSONObject créé par ParseJSONValue
      //   ne sera JAMAIS libéré → fuite mémoire à chaque appel.
      // ⚠ Si la réponse n'est pas un JSON valide, ParseJSONValue retourne
      //   nil. Toujours tester Assigned avant d'accéder. Préférer aussi
      //   TryGetValue à GetValue (qui lève EJSONException si clé absente).
      var JSONReponse: TJSONObject :=
        TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      try
        Result := '';
        if Assigned(JSONReponse) then
          JSONReponse.TryGetValue<string>('transaction_hash', Result);
      finally
        JSONReponse.Free;
      end;
    end
    else
      raise Exception.Create('Erreur blockchain');
  finally
    JSONBody.Free;
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

## Bonnes pratiques

### ✅ À faire

**1. Utiliser SHA-256 ou supérieur**
```pascal
// ✅ BON
Hash := THashSHA2.GetHashString(Document);

// ❌ ÉVITER - MD5 est cassé
Hash := THashMD5.GetHashString(Document);
```

**2. Toujours horodater les signatures**
```batch
REM Horodatage pour validité à long terme — version moderne 2026  
REM (`/tr` au lieu du legacy `/t`, `/td sha256` au lieu du SHA-1 par défaut)  
signtool sign /fd sha256 /tr http://timestamp.digicert.com /td sha256 MonApp.exe  
```

**3. Protéger la clé privée**
```pascal
// Stocker dans un endroit sécurisé (HSM, coffre-fort)
// Jamais dans le code source ou Git
```

**4. Vérifier les certificats**
```pascal
// Vérifier l'émetteur et la date de validité
if Certificate.notAfter < Now then
  ShowMessage('Certificat expiré !');
```

**5. Fournir plusieurs checksums**
```pascal
// SHA-256 + SHA-512 pour compatibilité et sécurité
```

### ❌ À éviter

**1. Utiliser des algorithmes obsolètes**
```pascal
// ❌ MD5 est cassé (collisions possibles)
// ❌ SHA-1 est déconseillé
```

**2. Clé privée non protégée**
```pascal
// ❌ Stocker la clé privée en clair
const PRIVATE_KEY = '...';
```

**3. Ignorer l'expiration des certificats**
```pascal
// ❌ Ne pas vérifier la date de validité
```

**4. Signature sans horodatage**
```pascal
// ❌ La signature devient invalide après expiration du certificat
```

## Checklist signature numérique

### Pour les développeurs

- [ ] Obtenir un certificat de code signing valide
- [ ] Signer tous les exécutables et installateurs
- [ ] Ajouter l'horodatage lors de la signature
- [ ] Fournir des checksums (SHA-256 minimum)
- [ ] Vérifier les signatures avant l'installation de mises à jour
- [ ] Protéger la clé privée (ne jamais commiter)
- [ ] Renouveler le certificat avant expiration

### Pour les utilisateurs

- [ ] Vérifier la signature avant d'exécuter un fichier téléchargé
- [ ] Vérifier les checksums des fichiers importants
- [ ] Ne pas ignorer les alertes Windows SmartScreen
- [ ] Méfiance envers les fichiers non signés

## Résumé des points essentiels

✅ **Principes clés** :
- La signature numérique garantit **authenticité** et **intégrité**
- Différent du chiffrement (visible vs caché)
- Basée sur la cryptographie asymétrique (RSA)
- Hash + Clé privée = Signature
- Vérification avec clé publique

🔐 **Composants essentiels** :
- **Hash** : Empreinte unique du document (SHA-256/SHA-512)
- **Clé privée** : Pour signer (à protéger absolument)
- **Clé publique** : Pour vérifier (peut être partagée)
- **Certificat** : Lie identité et clé publique
- **Horodatage** : Prouve la date de signature

📋 **Applications pratiques** :
- Code signing : Signer vos exécutables
- Signature de documents : PDF, contrats
- Validation de mises à jour
- Checksums pour téléchargements
- Preuve d'intégrité de fichiers

## Outils utiles

**Windows SDK** :
- **SignTool** : signature d'exécutables (cf section Code Signing pour les options modernes `/fd sha256 /tr /td sha256`)
- ~~MakeCert~~ : **déprécié depuis 2014** — utiliser `New-SelfSignedCertificate` (PowerShell) à la place pour générer des certificats de test
- **Get-AuthenticodeSignature** (PowerShell) : vérifier la signature d'un fichier en script

**Bibliothèques Delphi** :
- Indy (IdSSL) : Certificats, SSL/TLS
- System.Hash : Fonctions de hash
- OpenSSL : Crypto complète

**Services en ligne** :
- DigiCert, Sectigo : Certificats commerciaux
- Let's Encrypt : Certificats SSL gratuits
- Timestamp servers : Horodatage

La signature numérique est essentielle pour établir la confiance dans vos applications. Investissez dans un bon certificat et signez systématiquement tout ce que vous distribuez.

⏭️ [Sécurité des applications mobiles](/16-securite-des-applications/10-securite-des-applications-mobiles.md)
