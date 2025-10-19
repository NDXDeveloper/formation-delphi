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

**Étape 2 : Chiffrer l'empreinte avec la clé privée**
```
Empreinte → Chiffrement clé privée → Signature
"a4f5b2c8d..." → RSA clé privée →    Signature numérique
```

**Étape 3 : Attacher la signature au document**
```
Document original + Signature = Document signé
```

### Le processus de vérification

**Étape 1 : Calculer l'empreinte du document reçu**
```
Document reçu → Fonction de hash → Empreinte calculée
```

**Étape 2 : Déchiffrer la signature avec la clé publique**
```
Signature → Déchiffrement clé publique → Empreinte originale
```

**Étape 3 : Comparer les deux empreintes**
```
Si Empreinte calculée = Empreinte originale
  → Signature valide, document non modifié
Sinon
  → Signature invalide ou document altéré
```

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
  Result := THashSHA2.GetHashString(ATexte);
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

  if HashCalcule = AHashAttendu then
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
  TPaireClés = record
    CléPrivée: string;
    CléPublique: string;
  end;

// Note : Pour une vraie application, utilisez une bibliothèque crypto robuste
// comme OpenSSL ou les composants Indy
function GénérerPaireClésRSA: TPaireClés;
begin
  // Génération simplifiée pour l'exemple
  // En production, utilisez une vraie bibliothèque RSA

  Result.CléPrivée := 'PRIVATE_KEY_PLACEHOLDER';
  Result.CléPublique := 'PUBLIC_KEY_PLACEHOLDER';

  // Avec une vraie bibliothèque :
  // RSA.GenerateKeys(2048);
  // Result.CléPrivée := RSA.PrivateKey;
  // Result.CléPublique := RSA.PublicKey;
end;
```

### Signature et vérification conceptuelle

```pascal
type
  TSignatureNumérique = class
  private
    FCléPrivée: string;
    FCléPublique: string;
  public
    constructor Create(const ACléPrivée, ACléPublique: string);
    function SignerDocument(const ADocument: string): string;
    function VérifierSignature(const ADocument, ASignature: string): Boolean;
  end;

constructor TSignatureNumérique.Create(const ACléPrivée, ACléPublique: string);
begin
  inherited Create;
  FCléPrivée := ACléPrivée;
  FCléPublique := ACléPublique;
end;

function TSignatureNumérique.SignerDocument(const ADocument: string): string;
var
  Hash: string;
begin
  // 1. Calculer le hash du document
  Hash := THashSHA2.GetHashString(ADocument);

  // 2. "Chiffrer" le hash avec la clé privée (= signer)
  // En production, utilisez une vraie bibliothèque RSA
  Result := 'SIGNATURE_' + Hash;

  // Avec une vraie bibliothèque :
  // Result := RSA.Sign(Hash, FCléPrivée);
end;

function TSignatureNumérique.VérifierSignature(const ADocument, ASignature: string): Boolean;
var
  HashCalculé: string;
  HashDéchiffré: string;
begin
  // 1. Calculer le hash du document reçu
  HashCalculé := THashSHA2.GetHashString(ADocument);

  // 2. "Déchiffrer" la signature avec la clé publique
  // En production, utilisez une vraie bibliothèque RSA
  HashDéchiffré := StringReplace(ASignature, 'SIGNATURE_', '', []);

  // Avec une vraie bibliothèque :
  // HashDéchiffré := RSA.Verify(ASignature, FCléPublique);

  // 3. Comparer les hash
  Result := (HashCalculé = HashDéchiffré);
end;

// Utilisation
procedure TForm1.BtnSignerClick(Sender: TObject);
var
  Signature: TSignatureNumérique;
  Document: string;
  SignatureDoc: string;
  EstValide: Boolean;
begin
  // Générer une paire de clés
  Paire := GénérerPaireClésRSA;

  Signature := TSignatureNumérique.Create(Paire.CléPrivée, Paire.CléPublique);
  try
    Document := MemoDocument.Lines.Text;

    // Signer le document
    SignatureDoc := Signature.SignerDocument(Document);
    MemoSignature.Lines.Text := SignatureDoc;
    ShowMessage('Document signé');

    // Vérifier la signature
    EstValide := Signature.VérifierSignature(Document, SignatureDoc);

    if EstValide then
      ShowMessage('✓ Signature valide')
    else
      ShowMessage('✗ Signature invalide');
  finally
    Signature.Free;
  end;
end;
```

## Signature de fichiers avec Indy

Pour une implémentation robuste, utilisez Indy (inclus avec Delphi) :

```pascal
uses
  IdHashSHA, IdGlobal, System.SysUtils;

type
  TSignatureFichier = class
  public
    class function CalculerEmpreinte(const AFichier: string): string;
    class procedure SauvegarderEmpreinte(const AFichier, AFichierSignature: string);
    class function VérifierEmpreinte(const AFichier, AFichierSignature: string): Boolean;
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
    Signature.Add('Date: ' + DateTimeToStr(Now));
    Signature.Add('SHA-256: ' + Empreinte);
    Signature.SaveToFile(AFichierSignature);
  finally
    Signature.Free;
  end;
end;

class function TSignatureFichier.VérifierEmpreinte(const AFichier, AFichierSignature: string): Boolean;
var
  EmpreinteCalculée: string;
  EmpreinteStockée: string;
  Signature: TStringList;
  i: Integer;
begin
  Result := False;

  if not FileExists(AFichier) or not FileExists(AFichierSignature) then
    Exit;

  // Calculer l'empreinte actuelle
  EmpreinteCalculée := CalculerEmpreinte(AFichier);

  // Lire l'empreinte stockée
  Signature := TStringList.Create;
  try
    Signature.LoadFromFile(AFichierSignature);

    for i := 0 to Signature.Count - 1 do
    begin
      if Signature[i].StartsWith('SHA-256: ') then
      begin
        EmpreinteStockée := Copy(Signature[i], 10, Length(Signature[i]));
        Break;
      end;
    end;

    // Comparer
    Result := (EmpreinteCalculée = EmpreinteStockée);
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

procedure TForm1.BtnVérifierFichierClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if TSignatureFichier.VérifierEmpreinte(
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
┌─────────────────────────────────────────┐
│         CERTIFICAT NUMÉRIQUE            │
├─────────────────────────────────────────┤
│ Version: X.509 v3                       │
│ Numéro de série: 1234567890             │
│                                         │
│ Émetteur (CA):                          │
│   Nom: DigiCert                         │
│   Pays: US                              │
│                                         │
│ Sujet (Propriétaire):                   │
│   Nom: MonEntreprise SAS                │
│   Pays: FR                              │
│   Email: admin@monentreprise.com        │
│                                         │
│ Validité:                               │
│   Du: 2024-01-01                        │
│   Au: 2025-01-01                        │
│                                         │
│ Clé publique:                           │
│   [Clé publique RSA 2048 bits]          │
│                                         │
│ Signature de la CA:                     │
│   [Signature numérique de DigiCert]     │
└─────────────────────────────────────────┘
```

### Hiérarchie de certification

```
Certificat Racine (Root CA)
  ├─ Certificat Intermédiaire
  │   ├─ Certificat de Site Web (www.example.com)
  │   └─ Certificat de Code Signing
  └─ Certificat Intermédiaire
      └─ Certificat Personnel
```

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
   - DigiCert (Code Signing Certificate)
   - Sectigo (Code Signing Certificate)
   - GlobalSign
   - Prix : 200-400€ par an

2. **Certificats auto-signés** (développement uniquement)
   - Gratuit
   - Non reconnu par les navigateurs/Windows
   - Utile pour les tests

### Créer un certificat auto-signé (développement)

```batch
REM Créer un certificat de test avec makecert (Windows SDK)
makecert -sv MonApp.pvk -n "CN=MonEntreprise" MonApp.cer -r
pvk2pfx -pvk MonApp.pvk -spc MonApp.cer -pfx MonApp.pfx -po MotDePasse123
```

### Signer un exécutable

**Avec SignTool (Windows SDK)** :

```batch
REM Signer avec SignTool
signtool sign /f MonApp.pfx /p MotDePasse123 /t http://timestamp.digicert.com MonApplication.exe

REM Vérifier la signature
signtool verify /pa MonApplication.exe
```

**Automatiser dans Delphi** :

```pascal
procedure SignerExécutable(const AFichierExe, ACertificat, AMotDePasse: string);
var
  Commande: string;
  ExitCode: Cardinal;
begin
  // Construire la ligne de commande SignTool
  Commande := Format(
    'signtool.exe sign /f "%s" /p %s /t http://timestamp.digicert.com /v "%s"',
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
  SignerExécutable(
    'C:\Projets\MonApp\MonApp.exe',
    'C:\Certificats\MonApp.pfx',
    'MotDePasseSecret'
  );

  ShowMessage('Build et signature terminés');
end;
```

### Vérifier la signature d'un exécutable

```pascal
uses
  Winapi.Windows, System.SysUtils;

function VérifierSignatureExécutable(const AFichier: string): Boolean;
var
  VersionInfo: DWORD;
  VersionInfoSize: DWORD;
  VersionData: Pointer;
begin
  Result := False;

  // Vérifier si le fichier a des informations de version signées
  VersionInfoSize := GetFileVersionInfoSize(PChar(AFichier), VersionInfo);

  if VersionInfoSize > 0 then
  begin
    GetMem(VersionData, VersionInfoSize);
    try
      if GetFileVersionInfo(PChar(AFichier), 0, VersionInfoSize, VersionData) then
      begin
        // En production, vérifier réellement la signature avec WinVerifyTrust
        Result := True;
      end;
    finally
      FreeMem(VersionData);
    end;
  end;
end;

procedure TForm1.BtnVérifierSignatureClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if VérifierSignatureExécutable(OpenDialog1.FileName) then
      ShowMessage('✓ L''exécutable est signé')
    else
      ShowMessage('✗ L''exécutable n''est PAS signé');
  end;
end;
```

## Signature de documents PDF

Pour signer des documents PDF en Delphi :

```pascal
// Utiliser une bibliothèque comme Gnostice PDFtoolkit ou Winsoft PDF Library

type
  TSignaturePDF = class
  public
    procedure SignerPDF(const AFichierPDF, ACertificat, AMotDePasse: string);
    function VérifierSignaturePDF(const AFichierPDF: string): Boolean;
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
  Commande := Format(
    'signtool sign /f "%s" /t %s /v "%s"',
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
begin
  FileStream := TFileStream.Create(AFichier, fmOpenRead or fmShareDenyWrite);
  try
    // MD5 (déconseillé pour la sécurité, mais encore utilisé)
    HashMD5 := THashMD5.Create;
    Result.MD5 := HashMD5.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-1 (déconseillé pour la sécurité critique)
    HashSHA1 := THashSHA1.Create;
    Result.SHA1 := HashSHA1.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-256 (recommandé)
    HashSHA256 := THashSHA2.Create;
    Result.SHA256 := HashSHA256.GetHashString(FileStream);
    FileStream.Position := 0;

    // SHA-512 (très sûr, mais plus lent)
    Result.SHA512 := THashSHA2.GetHashString(FileStream);
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
    FCléPublique: string;
  public
    constructor Create(const ACléPublique: string);
    function VérifierMiseAJour(const AFichierMAJ, ASignature: string): Boolean;
    procedure TéléchargerEtInstaller(const AURL: string);
  end;

constructor TValidateurMiseAJour.Create(const ACléPublique: string);
begin
  inherited Create;
  FCléPublique := ACléPublique;
end;

function TValidateurMiseAJour.VérifierMiseAJour(const AFichierMAJ, ASignature: string): Boolean;
var
  HashCalculé: string;
  SignatureDécodée: string;
begin
  Result := False;

  // 1. Calculer le hash du fichier téléchargé
  HashCalculé := CalculerHashFichier(AFichierMAJ);

  // 2. Vérifier la signature avec la clé publique
  // SignatureDécodée := RSA.Verify(ASignature, FCléPublique);

  // 3. Comparer
  // Result := (HashCalculé = SignatureDécodée);

  // Version simplifiée pour l'exemple
  Result := True; // Implémenter la vraie vérification
end;

procedure TValidateurMiseAJour.TéléchargerEtInstaller(const AURL: string);
var
  FichierMAJ: string;
  FichierSignature: string;
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    // Télécharger la mise à jour
    FichierMAJ := TPath.Combine(TPath.GetTempPath, 'update.exe');
    HTTP.Get(AURL, FichierMAJ);

    // Télécharger la signature
    FichierSignature := TPath.Combine(TPath.GetTempPath, 'update.sig');
    HTTP.Get(AURL + '.sig', FichierSignature);

    // Vérifier la signature
    if VérifierMiseAJour(FichierMAJ, TFile.ReadAllText(FichierSignature)) then
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
    function VérifierSurBlockchain(const AHashTransaction: string): Boolean;
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
      Result := (TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject).GetValue<string>('transaction_hash')
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
```pascal
// Horodatage pour validité à long terme
signtool sign /t http://timestamp.digicert.com MonApp.exe
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
- SignTool : Signature d'exécutables
- MakeCert : Certificats de test

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
