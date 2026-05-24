🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.1 Authentification des utilisateurs

## Introduction

L'authentification des utilisateurs est un mécanisme fondamental de sécurité qui permet de vérifier l'identité d'une personne souhaitant accéder à votre application. C'est généralement la première ligne de défense pour protéger vos données et fonctionnalités sensibles.

**Qu'est-ce que l'authentification ?**

L'authentification répond à la question : "Qui êtes-vous ?" Elle permet de s'assurer que l'utilisateur est bien celui qu'il prétend être, généralement via un nom d'utilisateur et un mot de passe.

> **À ne pas confondre avec l'autorisation** : L'authentification vérifie l'identité, tandis que l'autorisation détermine ce que l'utilisateur peut faire une fois authentifié.

## Les différents types d'authentification

> 💡 **Les trois familles de facteurs (NIST SP 800-63B)** : toute méthode d'authentification se classe dans l'une de ces trois catégories :  
>  
> 1. **Quelque chose que vous SAVEZ** (*knowledge*) — mot de passe, code PIN, question secrète.  
> 2. **Quelque chose que vous AVEZ** (*possession*) — téléphone (TOTP, push), token hardware (YubiKey), smart card, certificat numérique sur clé USB.  
> 3. **Quelque chose que vous ÊTES** (*inherence*) — empreinte digitale, reconnaissance faciale, iris, voix.  
>  
> Combiner deux familles différentes = **MFA** (Multi-Factor Authentication). Deux mots de passe ne sont PAS du MFA : ce sont deux facteurs de la même famille.

### 1. Authentification simple (nom d'utilisateur/mot de passe)

C'est la méthode la plus courante et la plus simple à implémenter. L'utilisateur fournit :
- Un identifiant unique (nom d'utilisateur, email, etc.)
- Un mot de passe secret

### 2. Authentification à deux facteurs (2FA)

Cette méthode ajoute une couche de sécurité supplémentaire en demandant :
- Quelque chose que vous savez (mot de passe)
- Quelque chose que vous possédez (code TOTP via Google Authenticator, push notification, ⚠ pas SMS — voir 16.10 sur le SIM-swap)

### 3. Authentification biométrique

Utilise des caractéristiques physiques uniques :
- Empreinte digitale
- Reconnaissance faciale
- Scan de l'iris

⚠ La biométrie n'est jamais à 100 % fiable (taux de faux positifs et faux négatifs) et n'est **pas révocable** : si votre empreinte fuite, vous ne pouvez pas en changer. À utiliser comme **déverrouillage local** (un secret stocké dans le Keychain/Keystore) plutôt qu'en facteur d'authentification serveur direct.

### 4. Authentification par certificat / Passkeys

Utilise des certificats numériques ou des **paires de clés FIDO2** pour identifier l'utilisateur. Variantes courantes :
- **mTLS** (Mutual TLS) en B2B et microservices internes.
- **Smart cards** (carte d'identité électronique, CAC militaire US, PIV).
- **Passkeys** (FIDO2 / WebAuthn) — la voie moderne grand public depuis 2022-2023.

## Concepts fondamentaux

### Hash de mot de passe

**Règle d'or** : Ne jamais stocker les mots de passe en clair dans votre base de données !

Un hash est une fonction mathématique qui transforme un mot de passe en une chaîne de caractères unique et irréversible.

**Exemple de transformation** :
```
Mot de passe : "MonMotDePasse123"  
Hash (SHA-256) : "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92"  
```

**Propriétés importantes du hash** :
- Irréversible : impossible de retrouver le mot de passe original à partir du hash
- Unique : deux mots de passe différents produisent des hash différents
- Déterministe : le même mot de passe produit toujours le même hash

> 🚨 **SHA-256 seul n'est PAS suffisant pour un mot de passe.** SHA-256 est conçu pour être **rapide** : un attaquant disposant d'un GPU peut tester des **milliards** de candidats par seconde. C'est exactement ce qu'on veut éviter pour un mot de passe.  
>  
> Les algorithmes appropriés pour les mots de passe sont **lents par construction** et acceptent un paramètre de **coût ajustable** (nombre d'itérations) qui rend l'attaque exponentiellement plus difficile :  
> - **Argon2id** — recommandé par l'OWASP en 2026 (meilleur compromis mémoire + temps)  
> - **scrypt** — bonne alternative, résistant au matériel dédié  
> - **bcrypt** — éprouvé depuis 1999, encore valable  
> - **PBKDF2-HMAC-SHA-256** avec ≥ 600 000 itérations — disponible **nativement** dans `System.Hash` (`THashPBKDF2_SHA256`)  
>  
> Les exemples plus loin dans ce chapitre utilisent `THashPBKDF2_SHA256` car il ne nécessite aucune bibliothèque externe.

### Salt (sel cryptographique)

Un salt est une valeur aléatoire ajoutée au mot de passe avant le hashage pour renforcer la sécurité.

**Pourquoi utiliser un salt ?**
- Empêche les attaques par rainbow tables (tables précalculées de hash)
- Même si deux utilisateurs ont le même mot de passe, leurs hash seront différents

**Exemple** :
```
Utilisateur 1 : "password" + salt "abc123" → hash différent  
Utilisateur 2 : "password" + salt "xyz789" → hash différent  
```

> ⚠ **Le sel doit être cryptographiquement aléatoire.** Pas `Random()`, pas l'horodatage, pas un GUID (les GUIDs v1/v4 contiennent des bits prédictibles). Utilisez un vrai CSPRNG (*Cryptographically Secure Pseudo-Random Number Generator*) : `RandomBytes` d'OpenSSL, `SystemRandom` sur Linux, `BCryptGenRandom` sur Windows, ou via JNI/Objective-C sur mobile.  
>  
> **Longueur recommandée** : 16 octets (128 bits) minimum, stockés en Base64 dans la base.

### Session utilisateur

Une fois l'utilisateur authentifié, on crée une session qui permet de :
- Garder l'utilisateur connecté pendant sa navigation
- Stocker des informations temporaires (rôle, préférences)
- Éviter de redemander le mot de passe à chaque action

## Implémentation basique dans Delphi

### Structure de la base de données

Pour gérer l'authentification, vous aurez généralement besoin d'une table utilisateurs :

```sql
CREATE TABLE Utilisateurs (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    NomUtilisateur VARCHAR(50) UNIQUE NOT NULL,
    Email VARCHAR(100) UNIQUE NOT NULL,
    MotDePasseHash VARCHAR(255) NOT NULL,
    Salt VARCHAR(50) NOT NULL,
    DateCreation DATETIME DEFAULT CURRENT_TIMESTAMP,
    DerniereConnexion DATETIME,
    Actif BOOLEAN DEFAULT TRUE
);
```

### Utilisation de FireDAC pour l'authentification

Delphi dispose de FireDAC, un framework puissant pour accéder aux bases de données. Voici les composants nécessaires :

**Composants à placer sur votre formulaire de connexion** :
- `TFDConnection` : pour la connexion à la base de données
- `TFDQuery` : pour exécuter les requêtes SQL
- `TEdit` : pour saisir le nom d'utilisateur
- `TEdit` : pour saisir le mot de passe (avec `PasswordChar` = '*')
- `TButton` : pour déclencher la connexion

### Code de base pour la vérification

Voici un exemple simplifié de vérification d'authentification :

```pascal
procedure TFormConnexion.BtnConnexionClick(Sender: TObject);  
const  
  // Sel et hash factices, utilisés quand l'utilisateur n'existe pas pour
  // déclencher quand même un calcul PBKDF2 → empêcher l'énumération de
  // comptes par mesure du temps de réponse.
  SEL_FACTICE  = 'AAAAAAAAAAAAAAAAAAAAAA==';   // 16 octets de zéros en Base64
  HASH_FACTICE = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=';   // 32 octets
var
  HashStocke, SaltB64: string;
  HashCalcule: string;
  Existe: Boolean;
begin
  // Récupérer le salt et le hash depuis la base de données
  FDQuery1.SQL.Text := 'SELECT MotDePasseHash, Salt FROM Utilisateurs ' +
                       'WHERE NomUtilisateur = :Username AND Actif = TRUE';
  FDQuery1.ParamByName('Username').AsString := EditUtilisateur.Text;
  FDQuery1.Open;

  Existe := not FDQuery1.IsEmpty;
  if Existe then
  begin
    SaltB64    := FDQuery1.FieldByName('Salt').AsString;
    HashStocke := FDQuery1.FieldByName('MotDePasseHash').AsString;
  end
  else
  begin
    // ⚠ TIMING : si on retournait ici, le compte inexistant répondrait
    //   en <1ms tandis qu'un compte existant prendrait ~200ms (PBKDF2).
    //   L'attaquant peut alors énumérer les comptes via timing. On exécute
    //   donc le hashage avec des valeurs factices pour égaliser les temps.
    SaltB64    := SEL_FACTICE;
    HashStocke := HASH_FACTICE;
  end;
  FDQuery1.Close;

  // Calcul PBKDF2 — exécuté DANS TOUS LES CAS, même si l'utilisateur n'existe pas
  HashCalcule := HasherMotDePasse(EditMotDePasse.Text, SaltB64);

  // Comparaison à TEMPS CONSTANT (anti-timing attack).
  // Une comparaison « = » classique court-circuite dès le premier
  // octet différent, ce qui permet à un attaquant de mesurer le
  // temps de réponse pour deviner le hash caractère par caractère.
  if Existe and HashEgalTempsConstant(HashCalcule, HashStocke) then
    ShowMessage('Connexion réussie !')
  else
    ShowMessage('Identifiants incorrects');
end;
```

> 💡 Les fonctions `HasherMotDePasse` et `HashEgalTempsConstant` sont définies plus bas dans la section *Fonctions utilitaires pour le hashage*.

### Création d'un nouvel utilisateur

Lors de l'inscription d'un nouvel utilisateur :

```pascal
procedure TFormInscription.BtnInscrireClick(Sender: TObject);  
var  
  SaltB64, HashB64: string;
begin
  // Générer un sel aléatoire cryptographiquement sûr (16 octets, Base64)
  SaltB64 := GenererSaltCSPRNG;

  // Dériver le hash via PBKDF2-HMAC-SHA-256 (lent par construction)
  HashB64 := HasherMotDePasse(EditMotDePasse.Text, SaltB64);

  // Insérer dans la base de données
  FDQuery1.SQL.Text := 'INSERT INTO Utilisateurs ' +
    '(NomUtilisateur, Email, MotDePasseHash, Salt) ' +
    'VALUES (:Username, :Email, :Hash, :Salt)';
  FDQuery1.ParamByName('Username').AsString := EditUtilisateur.Text;
  FDQuery1.ParamByName('Email').AsString    := EditEmail.Text;
  FDQuery1.ParamByName('Hash').AsString     := HashB64;
  FDQuery1.ParamByName('Salt').AsString     := SaltB64;

  try
    FDQuery1.ExecSQL;
    // ⚠ Réponse identique que l'utilisateur existe déjà ou non — sinon on
    //   permet une énumération des comptes (deviner « jean@dupont.fr »
    //   existe car « erreur de doublon »). Toujours répondre comme si tout
    //   s'était bien passé et envoyer l'email de confirmation (qui n'arrivera
    //   à personne d'autre que l'utilisateur légitime).
    ShowMessage('Si l''inscription a abouti, vous recevrez un email de confirmation.');
    EnvoyerEmailConfirmation(EditEmail.Text);
  except
    on E: EFDDBEngineException do
    begin
      // Si c'est une violation d'unicité (doublon username/email),
      // afficher le même message que le cas normal pour éviter l'énumération.
      // Logger en interne pour suivi.
      TLogger.Instance.Info('Tentative de doublon inscription',
        Format('Username: %s, Email: %s', [EditUtilisateur.Text, EditEmail.Text]));
      ShowMessage('Si l''inscription a abouti, vous recevrez un email de confirmation.');
    end;
    on E: Exception do
    begin
      TLogger.Instance.Error('Erreur inscription', E.Message);
      ShowMessage('Une erreur est survenue. Veuillez réessayer plus tard.');
    end;
  end;
end;
```

## Fonctions utilitaires pour le hashage

`System.Hash` fournit `THashPBKDF2_SHA256` qui implémente PBKDF2 (*Password-Based Key Derivation Function 2*) avec HMAC-SHA-256. C'est l'algorithme à utiliser :

```pascal
uses
  System.Hash, System.NetEncoding, System.SysUtils;

const
  PBKDF2_ITERATIONS = 600000;  // recommandation OWASP 2026 pour SHA-256
  PBKDF2_KEY_LENGTH = 32;      // 32 octets = 256 bits
  SALT_LENGTH       = 16;      // 16 octets = 128 bits

// ⚠ Les importations d'API natives (`external`) DOIVENT être déclarées
//   au niveau de l'unité, JAMAIS à l'intérieur d'une procédure. On les
//   place donc en dehors de RemplirOctetsCSPRNG, conditionnées par
//   plate-forme.

{$IFDEF MSWINDOWS}
const
  BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;

// BCryptGenRandom (Bcrypt.dll, disponible depuis Windows Vista / 2008)
function BCryptGenRandom(hAlgorithm: Pointer; pbBuffer: PByte;
  cbBuffer: ULONG; dwFlags: ULONG): NTSTATUS; stdcall;
  external 'bcrypt.dll';
{$ENDIF}

{$IFDEF MACOS}
// SecRandomCopyBytes du framework Security
// Retourne errSecSuccess (0) sur succès, code d'erreur OSStatus sinon.
function SecRandomCopyBytes(rnd: Pointer; count: NativeUInt;
  bytes: Pointer): Integer; cdecl;
  external '/System/Library/Frameworks/Security.framework/Security'
  name '_SecRandomCopyBytes';
{$ENDIF}

// Wrapper plate-forme : remplit ABytes avec des octets cryptographiquement
// aléatoires. Lève une exception en cas d'échec — la suite du code n'a aucun
// sens si on n'a pas pu obtenir d'aléa sûr.
//
// ⚠ Sur Delphi, `{$IFDEF LINUX}` n'est PAS systématiquement défini sur
//   Android (selon la version). On utilise donc `{$IF DEFINED(LINUX) OR
//   DEFINED(ANDROID)}` pour couvrir explicitement les deux. Sur Android,
//   `/dev/urandom` est accessible depuis l'API 17+ et reste l'approche la
//   plus simple — l'alternative idiomatique serait `java.security.SecureRandom`
//   via JNI, mais cela ajoute une dépendance Java importante.
procedure RemplirOctetsCSPRNG(var ABytes: TBytes);
{$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  F: TFileStream;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Status: NTSTATUS;
{$ENDIF}
begin
  if Length(ABytes) = 0 then Exit;

  {$IFDEF MSWINDOWS}
  Status := BCryptGenRandom(nil, @ABytes[0], Length(ABytes),
                            BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if Status <> 0 then
    raise Exception.CreateFmt('BCryptGenRandom a échoué (NTSTATUS=$%x)', [Status]);
  {$ENDIF}

  {$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  // /dev/urandom est non-bloquant et cryptographiquement sûr sur Linux et
  // Android modernes. Alternative : appel direct à getrandom(2) via libc
  // (Linux ≥ 3.17) ou java.security.SecureRandom via JNI (Android).
  F := TFileStream.Create('/dev/urandom', fmOpenRead);
  try
    F.ReadBuffer(ABytes[0], Length(ABytes));
  finally
    F.Free;
  end;
  {$ENDIF}

  // Couvre macOS desktop ET iOS — sur Delphi, `MACOS` est défini sur les
  // deux ; `IOS` est en plus défini sur iOS. SecRandomCopyBytes existe sur
  // les deux.
  {$IFDEF MACOS}
  if SecRandomCopyBytes(nil, Length(ABytes), @ABytes[0]) <> 0 then
    raise Exception.Create('SecRandomCopyBytes a échoué');
  {$ENDIF}
end;

// Génère un sel aléatoire cryptographiquement sûr, encodé en Base64
function GenererSaltCSPRNG: string;  
var  
  Sel: TBytes;
begin
  SetLength(Sel, SALT_LENGTH);
  RemplirOctetsCSPRNG(Sel);
  Result := TNetEncoding.Base64.EncodeBytesToString(Sel);
end;

// Dérive un hash sécurisé du mot de passe avec PBKDF2-HMAC-SHA-256
function HasherMotDePasse(const AMotDePasse, ASaltB64: string): string;  
var  
  Sel, HashBin: TBytes;
begin
  Sel := TNetEncoding.Base64.DecodeStringToBytes(ASaltB64);
  HashBin := THashPBKDF2_SHA256.GetHashBytes(
    AMotDePasse, Sel, PBKDF2_ITERATIONS, PBKDF2_KEY_LENGTH);
  Result := TNetEncoding.Base64.EncodeBytesToString(HashBin);
end;

// Comparaison à temps constant : compare deux chaînes sans court-
// circuit, pour empêcher un attaquant de deviner le hash byte par
// byte en mesurant le temps de réponse du serveur.
function HashEgalTempsConstant(const A, B: string): Boolean;  
var  
  i, Diff: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  Diff := 0;
  for i := 1 to Length(A) do
    Diff := Diff or (Ord(A[i]) xor Ord(B[i]));
  Result := Diff = 0;
end;
```

> 💡 **Stockage : sel + hash, mais aussi les paramètres.** Pour pouvoir adapter le coût plus tard (sans bloquer les utilisateurs existants), de nombreux projets stockent le tout en une seule chaîne formatée — par exemple `pbkdf2-sha256$600000$<sel_b64>$<hash_b64>`. C'est le format adopté par bcrypt et Argon2 et permet une migration progressive si vous changez d'algorithme.

## Gestion de la session utilisateur

Une fois l'utilisateur authentifié, il faut gérer sa session :

### Variables globales de session

> ⚠️ **Anti-pattern à connaître** : les variables globales fonctionnent pour une application desktop simple monoposte/mono-utilisateur, mais deviennent dangereuses dès que vous avez :  
> - une application **multi-thread** (deux threads peuvent lire/écrire en même temps) ;  
> - un **serveur** WebBroker ou DataSnap qui sert plusieurs utilisateurs simultanément (le « `UtilisateurConnecte` » global serait écrasé par la dernière requête) ;  
> - des tests unitaires qui doivent pouvoir s'exécuter en parallèle.  
>  
> Pour les apps non triviales, encapsulez la session dans un **singleton thread-safe** ou, mieux, un objet `TSession` passé explicitement aux services. Pour un serveur HTTP, utilisez la session associée à la requête (`TWebSession`, `TIdHTTPSessionManager`, JWT côté client...).

```pascal
var
  UtilisateurConnecte: Boolean = False;
  IDUtilisateur: Integer = 0;
  NomUtilisateur: string = '';
  RoleUtilisateur: string = '';
```

### Initialisation de la session

```pascal
procedure InitialiserSession(AID: Integer; ANom: string; ARole: string);  
begin  
  UtilisateurConnecte := True;
  IDUtilisateur := AID;
  NomUtilisateur := ANom;
  RoleUtilisateur := ARole;
end;

procedure TerminerSession;  
begin  
  UtilisateurConnecte := False;
  IDUtilisateur := 0;
  NomUtilisateur := '';
  RoleUtilisateur := '';
end;
```

## Bonnes pratiques de sécurité

### 1. Politique de mot de passe fort

> 💡 **Recommandations 2026 (NIST SP 800-63B révision 4, OWASP ASVS 4.0)** : les anciennes règles « 1 majuscule + 1 chiffre + 1 spécial » sont aujourd'hui **déconseillées**. Elles poussent les utilisateurs à choisir des variantes prédictibles (`Password1!`) et n'améliorent pas la résistance à la force brute. Les nouvelles recommandations privilégient :  
>  
> 1. **La longueur** : 12 caractères minimum, idéalement 14+. Une *passphrase* de 4 mots aléatoires (`tomate-bleu-canard-pluie`) bat largement `P@ssw0rd!`.  
> 2. **Le rejet des mots de passe compromis** : interroger une liste publique de mots de passe fuités (ex. *Have I Been Pwned Passwords*, qui expose un API k-anonyme).  
> 3. **Aucune contrainte arbitraire de composition** : laisser l'utilisateur libre sur les caractères tant que la longueur est respectée.  
> 4. **Aucune expiration périodique** sans signe de compromission : forcer un changement tous les 90 jours conduit à des mots de passe plus faibles.

```pascal
uses
  System.Hash, System.NetEncoding, System.Net.HttpClient, System.SysUtils;

function MotDePasseValide(const AMotDePasse: string;
                          out ARaison: string): Boolean;
const
  LONGUEUR_MINI = 12;
  LONGUEUR_MAXI = 128;   // protège contre les attaques DoS par hash long
begin
  ARaison := '';

  if Length(AMotDePasse) < LONGUEUR_MINI then
  begin
    ARaison := Format('Le mot de passe doit faire au moins %d caractères.',
                      [LONGUEUR_MINI]);
    Exit(False);
  end;

  if Length(AMotDePasse) > LONGUEUR_MAXI then
  begin
    ARaison := Format('Le mot de passe ne doit pas dépasser %d caractères.',
                      [LONGUEUR_MAXI]);
    Exit(False);
  end;

  // Vérifier que ce n'est pas un mot de passe connu pour être compromis.
  if EstMotDePasseCompromis(AMotDePasse) then
  begin
    ARaison := 'Ce mot de passe figure dans une fuite publique de données. ' +
               'Choisissez-en un autre.';
    Exit(False);
  end;

  Result := True;
end;

// Vérification k-anonyme via l'API « Have I Been Pwned Passwords ».
// On envoie SEULEMENT les 5 premiers caractères du SHA-1 du mot de passe.
// L'API renvoie tous les hashes commençant par ce préfixe ; on cherche le
// suffixe localement. Le mot de passe lui-même ne quitte jamais le poste.
function EstMotDePasseCompromis(const AMotDePasse: string): Boolean;  
var  
  HashSHA1, Prefixe, Suffixe, Reponse: string;
  HTTP: THTTPClient;
  Ligne: string;
begin
  Result := False;
  HashSHA1 := UpperCase(THashSHA1.GetHashString(AMotDePasse));
  Prefixe := Copy(HashSHA1, 1, 5);
  Suffixe := Copy(HashSHA1, 6, MaxInt);

  HTTP := THTTPClient.Create;
  try
    HTTP.ConnectionTimeout := 3000;
    HTTP.ResponseTimeout := 3000;
    try
      Reponse := HTTP.Get('https://api.pwnedpasswords.com/range/' + Prefixe)
                     .ContentAsString;
    except
      // Si l'API est injoignable, ne pas bloquer l'utilisateur — on signale
      // simplement « non vérifié ».
      Exit(False);
    end;
  finally
    HTTP.Free;
  end;

  for Ligne in Reponse.Split([#10]) do
    if Ligne.StartsWith(Suffixe + ':') then
      Exit(True);
end;
```

> ⚠️ SHA-1 est utilisé ici **uniquement parce que l'API HIBP l'exige pour des raisons historiques** : c'est un échange réseau, pas le stockage du mot de passe. Ne JAMAIS stocker un mot de passe hashé en SHA-1.

### 2. Limitation des tentatives de connexion

Pour éviter les attaques par force brute, on suit deux indicateurs distincts **côté serveur** (pas seulement côté client, sinon trivial à contourner) :

1. **Par compte** : verrouiller (ou ralentir) un compte après N échecs successifs ;
2. **Par IP** : limiter le débit global pour stopper le *credential stuffing* (un attaquant qui essaie un même mot de passe sur des milliers de comptes différents).

> ⚠️ **Anti-pattern fréquent** : compter les échecs dans une variable **globale**. Cela bloque *tous* les utilisateurs à partir de N tentatives échouées d'un seul, et un attaquant qui répartit ses tentatives sur 1000 comptes différents ne déclenche jamais le seuil. Le compteur doit être stocké **par compte cible** (table en base) ou **par IP source** (cache type Redis).

```sql
ALTER TABLE Utilisateurs
  ADD COLUMN TentativesEchouees INT NOT NULL DEFAULT 0,
  ADD COLUMN BloqueJusquA DATETIME NULL;
```

```pascal
const
  MAX_TENTATIVES = 5;
  DELAI_BLOCAGE_MINUTES = 15;

function VerifierEtBloquer(AIDUtilisateur: Integer;
                            ASucces: Boolean): Boolean;
var
  Query: TFDQuery;
  BloqueJusqua: TDateTime;
  Tentatives: Integer;
begin
  // ⚠ `SELECT ... FOR UPDATE` n'est pertinent QUE dans une transaction :
  //   le verrou ligne est posé pour la durée de la transaction et libéré
  //   au COMMIT/ROLLBACK. Sans transaction explicite, FireDAC en mode
  //   auto-commit libère le verrou immédiatement après le SELECT → la
  //   condition de concurrence revient. Encapsuler tout le bloc dans
  //   `FDConnection1.StartTransaction` / `Commit` (omis ici pour la
  //   lisibilité, mais OBLIGATOIRE en production).

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Lire l'état actuel du compte (verrou ligne dans la transaction)
    Query.SQL.Text :=
      'SELECT TentativesEchouees, BloqueJusquA FROM Utilisateurs ' +
      'WHERE ID = :ID FOR UPDATE';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.Open;

    Tentatives := Query.FieldByName('TentativesEchouees').AsInteger;
    BloqueJusqua := Query.FieldByName('BloqueJusquA').AsDateTime;
    Query.Close;

    // Si le compte est encore bloqué, on refuse même en cas de bon
    // mot de passe (sinon l'attaquant pourrait deviner quand débloqué).
    if (BloqueJusqua > 0) and (Now < BloqueJusqua) then
      Exit(False);

    if ASucces then
    begin
      // Réinitialiser le compteur sur succès
      Query.SQL.Text :=
        'UPDATE Utilisateurs SET TentativesEchouees = 0, BloqueJusquA = NULL ' +
        'WHERE ID = :ID';
      Query.ParamByName('ID').AsInteger := AIDUtilisateur;
      Query.ExecSQL;
      Exit(True);
    end;

    // Incrémenter et bloquer si on dépasse le seuil
    // ⚠ `DATE_ADD(NOW(), INTERVAL :M MINUTE)` est syntaxe MySQL/MariaDB.
    //   - PostgreSQL : `NOW() + :M * INTERVAL '1 minute'` (ou `make_interval`)
    //   - SQL Server : `DATEADD(MINUTE, :M, GETDATE())`
    //   - SQLite     : `datetime('now', '+' || :M || ' minutes')`
    //   - Firebird   : `DATEADD(MINUTE, :M, CURRENT_TIMESTAMP)`
    //   Calculer la date côté Delphi avec `IncMinute(Now, M)` est aussi une
    //   solution portable, au prix d'une dérive si l'horloge serveur est
    //   différente de l'horloge applicative.
    Inc(Tentatives);
    if Tentatives >= MAX_TENTATIVES then
      Query.SQL.Text :=
        'UPDATE Utilisateurs SET TentativesEchouees = :T, ' +
        '  BloqueJusquA = DATE_ADD(NOW(), INTERVAL :M MINUTE) WHERE ID = :ID'
    else
      Query.SQL.Text :=
        'UPDATE Utilisateurs SET TentativesEchouees = :T WHERE ID = :ID';

    Query.ParamByName('T').AsInteger := Tentatives;
    if Query.Params.FindParam('M') <> nil then
      Query.ParamByName('M').AsInteger := DELAI_BLOCAGE_MINUTES;
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;

    Result := False;
  finally
    Query.Free;
  end;
end;
```

> 💡 **Délai exponentiel** : plutôt qu'un blocage binaire « 5 tentatives → 15 min », un délai croissant (`2^N` secondes : 1, 2, 4, 8, 16, 32...) gêne énormément les attaquants automatisés tout en restant tolérable pour un utilisateur qui se trompe de touche.

### 3. Utilisation de requêtes paramétrées

**TOUJOURS** utiliser des requêtes paramétrées pour éviter les injections SQL :

```pascal
// ❌ MAUVAIS - vulnérable aux injections SQL
FDQuery1.SQL.Text := 'SELECT * FROM Utilisateurs WHERE NomUtilisateur = "' +
                      EditUtilisateur.Text + '"';

// ✅ BON - sécurisé avec des paramètres
FDQuery1.SQL.Text := 'SELECT * FROM Utilisateurs WHERE NomUtilisateur = :Username';  
FDQuery1.ParamByName('Username').AsString := EditUtilisateur.Text;  
```

### 4. Masquer les informations sensibles

Ne révélez pas d'informations qui pourraient aider un attaquant :

```pascal
// ❌ MAUVAIS - révèle si l'utilisateur existe
if not UserExists then
  ShowMessage('Utilisateur inconnu')
else
  ShowMessage('Mot de passe incorrect');

// ✅ BON - message générique
ShowMessage('Identifiants incorrects');
```

### 5. Journalisation des tentatives de connexion

Conservez un historique des connexions pour détecter les activités suspectes :

```sql
CREATE TABLE HistoriqueConnexions (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    IDUtilisateur INT,
    DateHeure DATETIME DEFAULT CURRENT_TIMESTAMP,
    Reussite BOOLEAN,
    AdresseIP VARCHAR(45),  -- INET_ATON sur MySQL si on cherche par plage
    Navigateur VARCHAR(200),

    -- Index essentiels pour les requêtes typiques :
    INDEX idx_user_date (IDUtilisateur, DateHeure DESC),  -- historique par user
    INDEX idx_date (DateHeure DESC),                      -- log récent global
    INDEX idx_ip_echec (AdresseIP, Reussite)              -- détecter brute-force
);
```

## Authentification avancée

### Récupération de mot de passe

Pour permettre aux utilisateurs de réinitialiser leur mot de passe :

1. **Générer un token unique temporaire**

> ⚠️ **Ne PAS utiliser `CreateGUID` comme token de sécurité.** Sur Windows, `CreateGUID` appelle `UuidCreate` qui retourne un UUID v4 — son aléa réel dépend de l'implémentation et n'est pas garanti cryptographique. Un attaquant qui devine la fenêtre temporelle peut énumérer les UUID plausibles. Un token de reset est un **secret de courte durée** : il doit provenir d'un CSPRNG dédié.

```pascal
uses
  System.Hash, System.NetEncoding, System.SysUtils;

const
  TOKEN_BYTES = 32; // 256 bits — résiste à toute énumération réaliste

function GenererTokenReset: string;  
var  
  Octets: TBytes;
begin
  SetLength(Octets, TOKEN_BYTES);
  // RemplirOctetsCSPRNG → wrapper plate-forme :
  //   Windows : BCryptGenRandom (Bcrypt.dll)
  //   Linux   : getrandom() ou /dev/urandom
  //   macOS/iOS : SecRandomCopyBytes
  //   Android : SecureRandom (java.security)
  RemplirOctetsCSPRNG(Octets);

  // Encodage Base64URL (RFC 4648 §5) — variante de Base64 où `+` → `-` et
  // `/` → `_`, pas de padding. Lisible et compact dans une URL, sans
  // produire de séquences `%XX` qui rendraient le lien deux fois plus long.
  // ⚠ NE PAS confondre avec TNetEncoding.URL.EncodeBytesToString qui fait
  //   du percent-encoding (`%XX` pour les octets non ASCII).
  Result := TNetEncoding.Base64URL.EncodeBytesToString(Octets);
end;
```

> 💡 **Bonne pratique** : ne stocker en base **que le hash** du token (par exemple `THashSHA2.GetHashString(token)`). Envoyer le token en clair dans l'email ; lors du clic, hasher la valeur reçue et comparer au hash stocké. Ainsi un dump de base ne révèle pas les tokens actifs.

2. **Enregistrer le HASH du token avec une date d'expiration**
```sql
-- On stocke le HASH SHA-256 (64 caractères hexadécimaux), pas le token lui-même.
-- Comparaison à la validation : SHA256(token reçu) == HashTokenReset stocké ?
ALTER TABLE Utilisateurs ADD COLUMN HashTokenReset CHAR(64);  
ALTER TABLE Utilisateurs ADD COLUMN TokenExpiration DATETIME;  

-- Index pour rechercher rapidement un token (la recherche se fait sur le hash)
CREATE INDEX idx_hash_token ON Utilisateurs(HashTokenReset);

-- ⚠ Quand l'utilisateur clique sur le lien, MARQUER le token comme consommé
--   pour qu'il ne soit pas réutilisable. Stocker `TokenUtilise BOOLEAN` ou
--   simplement effacer `HashTokenReset = NULL` après usage.
```

> ⚠️ **Portabilité du DDL** : `ALTER TABLE ADD COLUMN` est syntaxe MySQL/PostgreSQL/SQLite. SQL Server omet `COLUMN` (`ALTER TABLE T ADD ColumnName Type`). Oracle utilise `ALTER TABLE T ADD (ColumnName Type)`. Adaptez selon votre SGBD.

3. **Envoyer un email avec le lien de réinitialisation**

4. **Vérifier le token et permettre la création d'un nouveau mot de passe**

### "Se souvenir de moi"

Pour garder l'utilisateur connecté entre les sessions :

> 🚨 **Stocker le token dans un `.ini` en clair est dangereux.** Le fichier est lisible par tout malware ou utilisateur ayant accès au disque. Pour un vrai « Remember me » :  
> - **Windows** : `CredWrite` / `CredRead` (API Windows Credential Manager), unité `Windows.WinCred` (via JEDI ou wrapper manuel).  
> - **macOS** : Keychain Services (`SecItemAdd`, `SecItemCopyMatching`).  
> - **Linux** : `libsecret` (GNOME Keyring / KWallet via D-Bus).  
> - **iOS / Android** : Keychain iOS / Android KeyStore (voir chapitre 16.7).  
>  
> Le code ci-dessous est un **exemple minimaliste** pour comprendre le principe. En production, n'utilisez **jamais** un `.ini` non chiffré pour stocker un token de session.

```pascal
// ⚠ EXEMPLE PÉDAGOGIQUE — token en clair dans un .ini.
//   À remplacer par un coffre-fort système (voir chapitre 16.7).
function CheminFichierSession: string;  
begin  
  // ⚠ `ChangeFileExt(ParamStr(0), '.ini')` place le fichier à côté de
  //   l'exécutable, ce qui n'est PAS portable :
  //   - Linux/macOS : /usr/local/bin/ est généralement read-only ;
  //   - Windows depuis l'UAC : Program Files refuse l'écriture par défaut.
  //   Utiliser plutôt le dossier de configuration utilisateur :
  Result := TPath.Combine(
    TPath.GetHomePath,                       // ~/AppData/Roaming sur Windows,
                                              // ~/ sur Linux, ~/Library/.../ sur macOS
    'MonApp' + PathDelim + 'session.ini'
  );
  ForceDirectories(ExtractFilePath(Result));
end;

procedure SauvegarderToken(const AToken: string);  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(CheminFichierSession);
  try
    IniFile.WriteString('Session', 'Token', AToken);
  finally
    IniFile.Free;
  end;
end;

function ChargerToken: string;  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(CheminFichierSession);
  try
    Result := IniFile.ReadString('Session', 'Token', '');
  finally
    IniFile.Free;
  end;
end;
```

**Bonnes pratiques complémentaires** :
- Le token doit avoir une **durée de validité courte** côté serveur (ex. 30 jours), avec rotation à chaque utilisation.
- Côté serveur, lier le token à l'IP / au *user-agent* pour révoquer en cas d'usage suspect.
- Proposer à l'utilisateur un écran « Sessions actives » pour révoquer ses tokens manuellement.

## Considérations pour les applications multi-plateformes

Lorsque vous développez avec FireMonkey pour plusieurs plateformes :

### Stockage sécurisé des identifiants

- **Windows** : Utilisez le système de credentials Windows
- **macOS** : Utilisez le Keychain
- **iOS/Android** : Utilisez le stockage sécurisé natif

Delphi fournit des APIs pour accéder à ces systèmes de manière unifiée.

### Authentification biométrique mobile

Pour iOS et Android, vous pouvez intégrer Touch ID / Face ID / Empreinte digitale :

> ⚠ **L'unité `FMX.Biometrics` et la classe `TBiometricAuth` ne sont pas standard dans la RTL Delphi 13.** L'API native s'appelle `LocalAuthentication.LAContext` sur iOS et `androidx.biometric.BiometricPrompt` sur Android. Plusieurs bibliothèques tierces fournissent un wrapper unifié (Kastri Free, TMS, etc.). Le code ci-dessous illustre le **pattern d'utilisation conceptuel** — adaptez les noms d'unités à la bibliothèque que vous intégrez.

```pascal
// Pseudo-code conceptuel — voir la note ci-dessus.
uses
  FMX.Platform, FMX.Biometrics;  // ⚠ unités fournies par un wrapper tiers

procedure AuthentifierParBiometrie;  
var  
  BiometricAuth: TBiometricAuth;
begin
  BiometricAuth := TBiometricAuth.Create(nil);
  try
    if BiometricAuth.BiometryType <> TBiometryType.None then
    begin
      BiometricAuth.Authenticate('Veuillez vous authentifier',
        procedure(const ASuccess: Boolean)
        begin
          if ASuccess then
            ShowMessage('Authentification réussie')
          else
            ShowMessage('Authentification échouée');
        end);
    end
    else
      ShowMessage('Authentification biométrique non disponible');
  finally
    BiometricAuth.Free;
  end;
end;
```

> 🔒 **Important : la biométrie déverrouille, elle n'authentifie pas seule.** Sur mobile, le pattern recommandé est :  
> 1. Première connexion : login/mot de passe classique, génération d'un token serveur.  
> 2. Stockage du token dans le **Keychain iOS** ou **Keystore Android** avec une contrainte biométrique (accessibilité biométrique).  
> 3. Connexions suivantes : `LAContext.evaluatePolicy` / `BiometricPrompt.authenticate` déverrouille l'accès au token, qui est ensuite envoyé au serveur.  
>  
> Ainsi, même si l'attaquant vole le téléphone et le déverrouille, il ne peut pas extraire le token sans biométrie valide.

## Authentification à deux facteurs : TOTP

Le mécanisme le plus courant est **TOTP** (*Time-based One-Time Password*, RFC 6238) — c'est ce qu'utilisent Google Authenticator, Authy, Microsoft Authenticator, etc. Principe :

1. Le serveur génère un secret partagé (≥ 160 bits) lors de l'activation du 2FA.
2. Ce secret est encodé en Base32 et affiché en QR code (URI `otpauth://`).
3. L'application mobile calcule, à chaque instant, un code à 6 chiffres : `HMAC-SHA1(secret, floor(unix_time / 30))` réduit modulo 10⁶.
4. Le serveur fait le même calcul et accepte le code s'il correspond, en tolérant ± 1 fenêtre de 30 s pour la dérive d'horloge.

```pascal
uses
  System.Hash, System.NetEncoding, System.DateUtils, System.SysUtils;

// Implémentation HOTP (RFC 4226) — TOTP en dépend.
function HOTP(const ASecret: TBytes; ACompteur: UInt64;
              ANbChiffres: Integer = 6): string;
var
  Compteur8: TBytes;          // ⚠ TBytes et non array[0..7] of Byte —
                               //   GetHMACAsBytes attend strictement TBytes
  i, Offset, Code: Integer;
  HMAC: TBytes;
begin
  // 1. Encoder le compteur en big-endian sur 8 octets
  SetLength(Compteur8, 8);
  for i := 7 downto 0 do
  begin
    Compteur8[i] := ACompteur and $FF;
    ACompteur := ACompteur shr 8;
  end;

  // 2. HMAC-SHA1(secret, compteur) — signature : GetHMACAsBytes(AValue, AKey).
  //    On passe donc le compteur en AValue (le message) et le secret en AKey.
  HMAC := THashSHA1.GetHMACAsBytes(Compteur8, ASecret);

  // 3. Dynamic truncation (RFC 4226 §5.3)
  Offset := HMAC[Length(HMAC) - 1] and $0F;
  Code := ((HMAC[Offset]     and $7F) shl 24) or
          ((HMAC[Offset + 1] and $FF) shl 16) or
          ((HMAC[Offset + 2] and $FF) shl 8 ) or
           (HMAC[Offset + 3] and $FF);

  // 4. Réduire au nombre de chiffres demandé
  Code := Code mod Round(IntPower(10, ANbChiffres));
  Result := Format('%.*d', [ANbChiffres, Code]);
end;

// Décodeur Base32 (alphabet RFC 4648 §6 : A-Z + 2-7). Insensible à la casse,
// ignore les caractères de remplissage `=` et les espaces.
function DecoderBase32(const ASource: string): TBytes;  
const  
  ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
var
  Buffer: UInt64;
  BitsAccumules: Integer;
  C: Char;
  Indice: Integer;
  Liste: TList<Byte>;
begin
  Buffer := 0;
  BitsAccumules := 0;
  Liste := TList<Byte>.Create;
  try
    for C in ASource do
    begin
      if (C = '=') or (C = ' ') then
        Continue;
      Indice := Pos(UpCase(C), ALPHABET) - 1;   // 0 à 31
      if Indice < 0 then
        raise Exception.CreateFmt('Caractère Base32 invalide : %s', [C]);

      Buffer := (Buffer shl 5) or UInt64(Indice);
      Inc(BitsAccumules, 5);
      if BitsAccumules >= 8 then
      begin
        Dec(BitsAccumules, 8);
        Liste.Add(Byte((Buffer shr BitsAccumules) and $FF));
      end;
    end;
    Result := Liste.ToArray;
  finally
    Liste.Free;
  end;
end;

// TOTP = HOTP avec le compteur = floor(unix_time / 30) — GÉNÉRATION du code
// courant. Pour la VÉRIFICATION d'un code soumis (avec tolérance d'horloge),
// utiliser VerifierTOTP plus bas.
function TOTP(const ASecretBase32: string): string;  
var  
  Secret: TBytes;
  Fenetre: UInt64;
begin
  Secret := DecoderBase32(ASecretBase32);
  Fenetre := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) div 30;
  Result := HOTP(Secret, Fenetre);
end;

// Vérifie un code TOTP soumis par l'utilisateur en tolérant ±ATolerance
// fenêtres de 30 s, afin d'absorber la dérive d'horloge du téléphone.
// Avec ATolerance=1, on teste t-1, t et t+1 → tolérance de ±30 s.
function VerifierTOTP(const ASecretBase32, ACodeSoumis: string;
                      ATolerance: Integer = 1): Boolean;
var
  Secret: TBytes;
  FenetreCourante: UInt64;
  Decalage, DiffOuEgal: Integer;
  CodeAttendu: string;
begin
  Result := False;
  Secret := DecoderBase32(ASecretBase32);
  FenetreCourante := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) div 30;

  // ⚠ Comparaison à temps constant sur l'ensemble des fenêtres autorisées :
  //   on ne sort PAS dès qu'on trouve une correspondance pour ne pas révéler
  //   via le temps de réponse à quelle fenêtre le code appartient.
  DiffOuEgal := 1;   // 1 = aucune correspondance, 0 = correspondance trouvée
  for Decalage := -ATolerance to ATolerance do
  begin
    CodeAttendu := HOTP(Secret, FenetreCourante + UInt64(Decalage));
    if HashEgalTempsConstant(CodeAttendu, ACodeSoumis) then
      DiffOuEgal := 0;
  end;
  Result := DiffOuEgal = 0;
end;

// URI à présenter en QR code pour qu'une app comme Google Authenticator
// enregistre le secret automatiquement
function UriOtpauth(const ALabel, ASecretBase32, AIssuer: string): string;  
begin  
  Result := Format('otpauth://totp/%s?secret=%s&issuer=%s&algorithm=SHA1&digits=6&period=30',
    [TNetEncoding.URL.Encode(ALabel),
     ASecretBase32,
     TNetEncoding.URL.Encode(AIssuer)]);
end;
```

> 💡 **Tolérance d'horloge et anti-rejeu** : `VerifierTOTP` ci-dessus accepte ±`ATolerance` fenêtres pour absorber la dérive entre serveur et téléphone (mode avion, fuseau mal réglé, etc.). MAIS **toujours marquer un code consommé** côté serveur : il ne doit pas être réutilisable même pendant les 30 s de validité (sinon un attaquant qui observe l'écran de l'utilisateur a 30 s pour le rejouer). Stocker `(IDUtilisateur, FenetreUtilisee)` après chaque validation réussie et refuser tout code dont la fenêtre est ≤ à la dernière utilisée.

## Authentification sans mot de passe : Passkeys / FIDO2

Depuis 2022-2023, les principaux acteurs (Apple, Google, Microsoft) poussent les **Passkeys** (basés sur le standard FIDO2 / WebAuthn) comme remplacement des mots de passe. Le principe :

- Lors de l'inscription, le téléphone ou la clé USB FIDO2 génère une **paire de clés asymétriques** propre au service.
- La clé publique est envoyée au serveur, la clé privée reste sur le périphérique, protégée par biométrie ou code PIN.
- À chaque connexion, le serveur envoie un *challenge* aléatoire, le périphérique le signe avec la clé privée, le serveur vérifie avec la clé publique.

**Avantages** :
- Pas de mot de passe à hasher / stocker / fuiter.
- Résistant au phishing (la clé privée est liée à l'origine — un site fictif ne pourra pas l'utiliser).
- L'utilisateur n'a rien à mémoriser.

**Côté Delphi** :
- Pour une application **web servie par WebBroker**, c'est l'API `navigator.credentials` du navigateur qui fait le travail ; le serveur Delphi expose des endpoints `/webauthn/register` et `/webauthn/authenticate` qui suivent les spécifications CTAP/WebAuthn (utiliser une bibliothèque Pascal comme `mORMot2.crypt.webauthn` ou un microservice Go/Node spécialisé).
- Pour une application **native**, il faut interfacer Windows Hello (`WebAuthn.dll`), Apple Passkeys (Authentication Services framework) ou Android Credential Manager — c'est non trivial, mais les composants tiers commencent à apparaître.

> 🎯 **Recommandation 2026** : pour une nouvelle application web ou mobile, prévoir l'ajout des Passkeys est devenu un standard de facto. Le mot de passe reste un *fallback*, pas la méthode principale.

## Authentification moderne avec OAuth2 et SSO

### Qu'est-ce que OAuth2 ?

OAuth2 permet aux utilisateurs de se connecter avec leurs comptes existants (Google, Microsoft, Facebook, etc.) sans créer de nouveau compte.

**Avantages** :
- Expérience utilisateur simplifiée
- Pas besoin de gérer les mots de passe
- Sécurité renforcée par les grands fournisseurs

> ⚠️ **PKCE obligatoire pour les applications natives et SPA** (RFC 7636, OAuth 2.1). Sans PKCE, le flow *Authorization Code* est vulnérable à l'interception du code par une autre application installée sur le même appareil. Le PKCE ajoute deux paramètres :  
>  
> 1. À la requête `/authorize`, l'app envoie `code_challenge = SHA256(code_verifier)` (où `code_verifier` est un secret CSPRNG de 43 à 128 caractères).  
> 2. À l'échange `/token`, l'app renvoie `code_verifier` ; le serveur recalcule SHA256 et vérifie qu'il correspond.  
>  
> Tous les grands fournisseurs (Google, Microsoft, Auth0, Okta) imposent PKCE pour les *clients publics* (sans secret) depuis 2023-2024. Pour Delphi, voir la section 16.4 pour un exemple complet.

### Single Sign-On (SSO)

Le SSO permet à un utilisateur de se connecter une seule fois et d'accéder à plusieurs applications.

**Implémentation basique** :
1. Rediriger l'utilisateur vers le fournisseur d'identité (avec `code_challenge` PKCE)
2. Recevoir un *authorization code* sur le redirect URI
3. Échanger le code contre un access token + refresh token (en envoyant `code_verifier`)
4. Valider le token (vérifier signature JWT, audience, expiration)
5. Créer la session locale

## Résumé des points essentiels

✅ **À faire** :
- Toujours hasher les mots de passe avec un salt
- Utiliser des requêtes paramétrées
- Imposer des mots de passe forts
- Limiter les tentatives de connexion
- Journaliser les événements de sécurité
- Utiliser HTTPS pour transmettre les identifiants

❌ **À ne jamais faire** :
- Stocker les mots de passe en clair
- Afficher des messages d'erreur détaillés
- Permettre des tentatives de connexion illimitées
- Transmettre des identifiants via GET ou en URL
- Négliger les mises à jour de sécurité

## Aller plus loin

L'authentification est un domaine vaste qui évolue constamment. Pour approfondir :

- **JWT (JSON Web Tokens)** : pour les applications web et API
- **OpenID Connect** : extension d'OAuth2 pour l'identité
- **SAML** : pour les environnements d'entreprise
- **Authentification multi-facteur** : pour une sécurité renforcée
- **Biométrie** : empreinte digitale, reconnaissance faciale

Dans les sections suivantes du chapitre 16, nous aborderons l'autorisation et le contrôle d'accès, le chiffrement des données, et d'autres aspects cruciaux de la sécurité des applications.

⏭️ [Autorisation et contrôle d'accès](/16-securite-des-applications/02-autorisation-et-controle-dacces.md)
