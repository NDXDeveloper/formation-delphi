🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.11 Sécurisation des accès et prévention des injections SQL

## Introduction

La **sécurité** est l'un des aspects les plus critiques du développement d'applications de bases de données. Une faille de sécurité peut entraîner des conséquences désastreuses : vol de données personnelles, destruction de données, accès non autorisé, et atteinte à la réputation de votre entreprise.

Dans ce chapitre, nous allons explorer les menaces principales et apprendre à sécuriser efficacement vos applications Delphi connectées à MySQL/MariaDB.

## Les injections SQL : La menace n°1

### Qu'est-ce qu'une injection SQL ?

Une **injection SQL** se produit quand un attaquant insère du code SQL malveillant dans une requête via les entrées utilisateur.

### Exemple concret d'attaque

#### Scénario : Formulaire de connexion

```pascal
// ❌ CODE VULNÉRABLE - NE JAMAIS FAIRE ÇA !
procedure TFormLogin.btnConnecterClick(Sender: TObject);  
var  
  SQL: string;
begin
  SQL := 'SELECT * FROM utilisateurs ' +
         'WHERE login = ''' + editLogin.Text + ''' ' +
         'AND password = ''' + editPassword.Text + '''';

  FDQuery1.SQL.Text := SQL;
  FDQuery1.Open;

  if not FDQuery1.IsEmpty then
    ShowMessage('Connexion réussie')
  else
    ShowMessage('Identifiants invalides');
end;
```

#### L'attaque

Un utilisateur malveillant entre ces valeurs :

```
Login : admin  
Password : ' OR '1'='1  
```

**La requête SQL devient :**

```sql
SELECT * FROM utilisateurs  
WHERE login = 'admin'  
AND password = '' OR '1'='1'  
```

**Résultat :** `'1'='1'` est toujours vrai, donc la condition est validée **même avec un mauvais mot de passe** ! 😱

L'attaquant peut se connecter **sans connaître le mot de passe**.

### Pire encore : Destruction de données

```
Login : admin  
Password : '; DROP TABLE utilisateurs; --  
```

**La requête devient :**

```sql
SELECT * FROM utilisateurs  
WHERE login = 'admin'  
AND password = '';  
DROP TABLE utilisateurs;  
--'
```

**Résultat :** La table `utilisateurs` est **SUPPRIMÉE** ! 💥

### Visualisation de l'attaque

```
┌─────────────────────────────────────────┐
│  Formulaire de connexion                │
├─────────────────────────────────────────┤
│  Login :    [admin____________]         │
│  Password : [' OR '1'='1______]         │
│             [Se connecter]              │
└─────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────┐
│  SQL généré (VULNÉRABLE)                │
├─────────────────────────────────────────┤
│  SELECT * FROM utilisateurs             │
│  WHERE login = 'admin'                  │
│  AND password = '' OR '1'='1'           │
│                      ↑                  │
│              Code injecté !             │
└─────────────────────────────────────────┘
              ↓
        Accès accordé ! ☠️
```

## La solution : Requêtes paramétrées

### Principe

Au lieu de **concaténer** les valeurs dans le SQL, on utilise des **paramètres** qui sont échappés automatiquement par FireDAC.

```pascal
// ✅ CODE SÉCURISÉ - TOUJOURS FAIRE COMME ÇA
procedure TFormLogin.btnConnecterClick(Sender: TObject);  
begin  
  FDQuery1.SQL.Text :=
    'SELECT * FROM utilisateurs ' +
    'WHERE login = :login ' +
    'AND password = :password';

  // Les paramètres sont échappés automatiquement
  FDQuery1.ParamByName('login').AsString := editLogin.Text;
  FDQuery1.ParamByName('password').AsString := editPassword.Text;

  FDQuery1.Open;

  if not FDQuery1.IsEmpty then
    ShowMessage('Connexion réussie')
  else
    ShowMessage('Identifiants invalides');
end;
```

### Comment ça fonctionne ?

Avec la même tentative d'injection :

```
Login : admin  
Password : ' OR '1'='1  
```

**FireDAC traite le tout comme une chaîne littérale :**

```sql
SELECT * FROM utilisateurs  
WHERE login = 'admin'  
AND password = '\' OR \'1\'=\'1'  
-- Les apostrophes sont échappées automatiquement
```

**Résultat :** Aucun utilisateur ne correspond, l'attaque échoue ! ✅

### Règle d'or

> **JAMAIS de concaténation de chaînes dans les requêtes SQL**  
>  
> **TOUJOURS utiliser des paramètres**

```pascal
// ❌ DANGEREUX
SQL := 'SELECT * FROM clients WHERE nom = ''' + editNom.Text + '''';

// ✅ SÉCURISÉ
SQL := 'SELECT * FROM clients WHERE nom = :nom';  
ParamByName('nom').AsString := editNom.Text;  
```

> 📌 **Note** : l'exemple de formulaire de connexion ci-dessus illustre **uniquement** comment se prémunir des injections SQL. Il compare un mot de passe en clair, ce qui **n'est pas la bonne pratique** : les mots de passe doivent être hachés en base (jamais en clair) et la vérification se fait en comparant les **hachages**, pas les chaînes brutes. La section suivante explique comment faire correctement.

## Sécurisation des mots de passe

### Ne JAMAIS stocker en clair

```sql
-- ❌ TERRIBLE - Mots de passe en clair
CREATE TABLE utilisateurs (
    id INT PRIMARY KEY,
    login VARCHAR(50),
    password VARCHAR(50)  -- ← DANGEREUX !
);

INSERT INTO utilisateurs VALUES (1, 'admin', 'motdepasse123');  -- ← Visible par tous !
```

**Problème :** Si quelqu'un accède à la base, tous les mots de passe sont exposés.

### Utiliser le hachage

```sql
-- ✅ BON - Mots de passe hachés
CREATE TABLE utilisateurs (
    id INT PRIMARY KEY,
    login VARCHAR(50) UNIQUE,
    password_hash VARCHAR(255)  -- Hash du mot de passe
);
```

### Hacher avec BCrypt (recommandé)

```pascal
uses
  BCrypt;  // Ajoutez la bibliothèque BCrypt pour Delphi

// Lors de la création d'un utilisateur
function TUserManager.CreerUtilisateur(const Login, Password: string): Boolean;  
var  
  Hash: string;
begin
  // Générer le hash du mot de passe
  Hash := TBCrypt.HashPassword(Password);

  // Stocker le hash (pas le mot de passe !)
  FDQuery1.SQL.Text :=
    'INSERT INTO utilisateurs (login, password_hash) ' +
    'VALUES (:login, :hash)';
  FDQuery1.ParamByName('login').AsString := Login;
  FDQuery1.ParamByName('hash').AsString := Hash;
  FDQuery1.ExecSQL;

  Result := True;
end;

// Lors de la vérification
function TUserManager.VerifierConnexion(const Login, Password: string): Boolean;  
var  
  HashStocke: string;
begin
  Result := False;

  FDQuery1.SQL.Text :=
    'SELECT password_hash FROM utilisateurs WHERE login = :login';
  FDQuery1.ParamByName('login').AsString := Login;
  FDQuery1.Open;

  if not FDQuery1.IsEmpty then
  begin
    HashStocke := FDQuery1.FieldByName('password_hash').AsString;

    // Comparer le hash (pas le mot de passe en clair !)
    Result := TBCrypt.CompareHash(Password, HashStocke);
  end;
end;
```

**Avantages de BCrypt :**
- Impossible de retrouver le mot de passe original
- Résistant aux attaques par force brute (slow by design)
- Salt automatique (chaque hash est unique)

> 💡 **Bonne pratique de sécurité — messages d'erreur** : à la connexion, ne **JAMAIS** distinguer dans le message d'erreur entre « login inexistant » et « mot de passe incorrect » :  
>  
> ```pascal  
> // ❌ Mauvais : révèle à l'attaquant si le login existe  
> if not LoginExiste then  
>   ShowMessage('Login inexistant')      // ← Énumération possible  
> else if not PasswordCorrect then  
>   ShowMessage('Mot de passe incorrect');  
>  
> // ✅ Bon : message identique dans les deux cas  
> ShowMessage('Identifiants invalides');  
> ```  
>  
> Pour aller encore plus loin (paranoïa avancée), un attaquant pourrait **mesurer le temps de réponse** : si la vérification est rapide quand le login n'existe pas (pas de hash à comparer) et plus lente sinon (hash BCrypt = ~100ms), il peut découvrir les logins valides. Pour égaliser les temps, comparez toujours contre un hash factice si le login n'existe pas :  
>  
> ```pascal  
> if Query.IsEmpty then  
>   TBCrypt.CompareHash(Password, HASH_FACTICE)  // Consomme le temps habituel  
> else  
>   Result := TBCrypt.CompareHash(Password, HashStocke);  
> ```

### ⚠️ SHA-256 + Salt : insuffisant en pratique

> **Avertissement important** : **SHA-256 même avec un salt n'est PAS un substitut acceptable à BCrypt/Argon2/scrypt** pour hacher des mots de passe. SHA-256 est une fonction de hachage **rapide** (volontairement), conçue pour les empreintes de fichiers ou la signature ; sur du matériel moderne, un attaquant peut tester **des milliards de mots de passe par seconde** avec un GPU. BCrypt et Argon2 sont **volontairement lents** (paramètres `cost`/`iterations` ajustables) pour qu'une seule vérification prenne ~100 ms, ce qui rend les attaques par force brute irréalistes.  
>  
> N'utilisez SHA-256 + salt **que dans un contexte pédagogique** ou pour des données qui ne sont pas des mots de passe. Pour les mots de passe en production :  
>  
> - **Premier choix** : Argon2id (moderne, gagnant du Password Hashing Competition 2015)  
> - **Choix classique éprouvé** : BCrypt (toujours sûr en 2026)  
> - **Alternative** : scrypt  
>  
> Bibliothèques Delphi : `DelphiBcrypt`, `Sodium for Delphi` (Argon2), `TMS Cryptography Pack`.

À titre **purement illustratif**, voici la mécanique d'un hachage avec salt :

```pascal
uses
  System.Hash;

function GenererSalt: string;  
var  
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;

function HashPasswordAvecSalt(const Password, Salt: string): string;  
begin  
  // ⚠️ NE PAS UTILISER EN PRODUCTION — voir avertissement ci-dessus.
  // SHA-256 ne « ralentit » pas l'attaquant ; pour des mots de passe il faut
  // BCrypt, Argon2 ou scrypt (avec coût ajustable).
  Result := THashSHA2.GetHashString(Password + Salt);
end;

// Utilisation (toujours à titre illustratif uniquement)
procedure CreerUtilisateur(const Login, Password: string);  
var  
  Salt, Hash: string;
begin
  Salt := GenererSalt;
  Hash := HashPasswordAvecSalt(Password, Salt);

  // Stocker le hash ET le salt
  FDQuery1.SQL.Text :=
    'INSERT INTO utilisateurs (login, password_hash, salt) ' +
    'VALUES (:login, :hash, :salt)';
  FDQuery1.ParamByName('login').AsString := Login;
  FDQuery1.ParamByName('hash').AsString := Hash;
  FDQuery1.ParamByName('salt').AsString := Salt;
  FDQuery1.ExecSQL;
end;
```

## Sécurisation de la connexion à la base

### Ne jamais hardcoder les identifiants

```pascal
// ❌ DANGEREUX - Identifiants en dur dans le code
FDConnection1.Params.Add('User_Name=root');  
FDConnection1.Params.Add('Password=motdepasse123');  
```

**Problèmes :**
- Visible dans le code source
- Visible dans l'exécutable (désassemblage)
- Difficile à changer

### Solution 1 : Fichier de configuration avec mot de passe offusqué (pas réellement chiffré)

> ⚠️ **Attention au vocabulaire** : l'exemple ci-dessous **n'est PAS du chiffrement** au sens cryptographique. Un XOR avec une clé d'un octet codée en dur est de l'**offuscation** — n'importe qui peut le retrouver en quelques minutes. Cela complique seulement la lecture pour un utilisateur curieux ; cela ne protège pas contre un attaquant. Pour du véritable chiffrement, voir la note en fin de section.

```pascal
unit uConfigManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.NetEncoding;

type
  TConfigManager = class
  private
    FIniFile: TIniFile;
    function Desoffusquer(const TexteOffusque: string): string;
    function Offusquer(const TexteClair: string): string;
  public
    constructor Create(const CheminFichier: string);
    destructor Destroy; override;

    function LireMotDePasse: string;
    procedure EcrireMotDePasse(const MotDePasse: string);
  end;

implementation

uses
  System.Hash;

constructor TConfigManager.Create(const CheminFichier: string);  
begin  
  inherited Create;
  FIniFile := TIniFile.Create(CheminFichier);
end;

destructor TConfigManager.Destroy;  
begin  
  FIniFile.Free;
  inherited;
end;

function TConfigManager.Offusquer(const TexteClair: string): string;  
var  
  Bytes: TBytes;
  i: Integer;
  Cle: Byte;
begin
  // ⚠️ Offuscation XOR — PAS du chiffrement.
  // Une vraie clé doit faire au minimum 128 bits, être imprévisible, et l'algo
  // doit être AES (ou ChaCha20). Pour des secrets en production, utilisez
  // System.Net.Crypto, Sodium for Delphi, ou les coffres-forts de l'OS.
  Cle := 42;  // Clé triviale (à titre d'exemple seulement)

  Bytes := TEncoding.UTF8.GetBytes(TexteClair);
  for i := 0 to Length(Bytes) - 1 do
    Bytes[i] := Bytes[i] xor Cle;

  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
end;

function TConfigManager.Desoffusquer(const TexteOffusque: string): string;  
var  
  Bytes: TBytes;
  i: Integer;
  Cle: Byte;
begin
  Cle := 42;

  Bytes := TNetEncoding.Base64.DecodeStringToBytes(TexteOffusque);
  for i := 0 to Length(Bytes) - 1 do
    Bytes[i] := Bytes[i] xor Cle;

  Result := TEncoding.UTF8.GetString(Bytes);
end;

function TConfigManager.LireMotDePasse: string;  
var  
  TexteOffusque: string;
begin
  TexteOffusque := FIniFile.ReadString('Database', 'Password', '');
  if TexteOffusque <> '' then
    Result := Desoffusquer(TexteOffusque)
  else
    Result := '';
end;

procedure TConfigManager.EcrireMotDePasse(const MotDePasse: string);  
var  
  TexteOffusque: string;
begin
  TexteOffusque := Offusquer(MotDePasse);
  FIniFile.WriteString('Database', 'Password', TexteOffusque);
end;

end.
```

> 🔐 **Pour du vrai chiffrement** : utilisez **AES-256-GCM** (chiffrement authentifié, recommandé en 2026) avec une clé issue d'un dérivateur (PBKDF2, Argon2). Bibliothèques :  
>  
> - `System.Net.Crypto` (Delphi) — primitive bas niveau  
> - **Sodium for Delphi** (libsodium) — API moderne, sûre par défaut  
> - **TMS Cryptography Pack** (commercial)  
> - **DCPCrypt** (open source historique)  
>  
> Et stockez la clé maître **en dehors** du binaire (variable d'environnement, coffre OS, KMS cloud — voir solutions 2 et 3 ci-dessous).

**Utilisation :**

```pascal
procedure TdmDatabase.ConfigurerConnexion;  
var  
  Config: TConfigManager;
  MotDePasse: string;
begin
  Config := TConfigManager.Create('config.ini');
  try
    MotDePasse := Config.LireMotDePasse;

    FDConnection1.Params.Clear;
    FDConnection1.Params.Add('Server=localhost');
    FDConnection1.Params.Add('Database=ma_base');
    FDConnection1.Params.Add('User_Name=delphi_user');
    FDConnection1.Params.Add('Password=' + MotDePasse);

  finally
    Config.Free;
  end;
end;
```

### Solution 2 : Variables d'environnement

```pascal
function ObtenirMotDePasseEnvironnement: string;  
begin  
  Result := GetEnvironmentVariable('DB_PASSWORD');
  if Result = '' then
    raise Exception.Create('Variable DB_PASSWORD non définie');
end;

// Utilisation
FDConnection1.Params.Add('Password=' + ObtenirMotDePasseEnvironnement);
```

**Configuration sur le serveur :**
```bash
# Linux/macOS
export DB_PASSWORD="MonMotDePasseSecurise"

# Windows
setx DB_PASSWORD "MonMotDePasseSecurise"
```

### Solution 3 : Demander à l'utilisateur

```pascal
procedure TdmDatabase.DataModuleCreate(Sender: TObject);  
var  
  Password: string;
begin
  if InputQuery('Connexion', 'Mot de passe base de données:', Password, True) then
  begin
    FDConnection1.Params.Add('Password=' + Password);
    FDConnection1.Connected := True;
  end
  else
    Application.Terminate;
end;
```

## Connexions SSL/TLS

### Activer SSL pour MySQL

```pascal
procedure ConfigurerSSL;  
begin  
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=serveur-distant.com');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=delphi_user');
  FDConnection1.Params.Add('Password=MotDePasse');

  // Activer SSL
  FDConnection1.Params.Add('UseSSL=True');
  FDConnection1.Params.Add('SSL_CA=C:\certs\ca-cert.pem');
  FDConnection1.Params.Add('SSL_CERT=C:\certs\client-cert.pem');
  FDConnection1.Params.Add('SSL_KEY=C:\certs\client-key.pem');
end;
```

**Avantages :**
- Communications chiffrées
- Protection contre l'écoute réseau (man-in-the-middle)
- Authentification mutuelle possible

## Principe du moindre privilège

### Créer des utilisateurs avec droits limités

```sql
-- ❌ DANGEREUX - Donner tous les droits
GRANT ALL PRIVILEGES ON *.* TO 'delphi_app'@'%';

-- ✅ BON - Droits minimaux nécessaires
GRANT SELECT, INSERT, UPDATE, DELETE ON ma_base.* TO 'delphi_app'@'localhost';

-- ✅ ENCORE MIEUX - Droits spécifiques par table
GRANT SELECT ON ma_base.clients TO 'delphi_readonly'@'localhost';  
GRANT INSERT, UPDATE ON ma_base.commandes TO 'delphi_orders'@'localhost';  
```

### Utilisateurs séparés par fonction

```sql
-- Utilisateur lecture seule (rapports)
CREATE USER 'app_readonly'@'localhost' IDENTIFIED BY 'password1';  
GRANT SELECT ON ma_base.* TO 'app_readonly'@'localhost';  

-- Utilisateur application standard
CREATE USER 'app_standard'@'localhost' IDENTIFIED BY 'password2';  
GRANT SELECT, INSERT, UPDATE ON ma_base.* TO 'app_standard'@'localhost';  

-- Utilisateur admin (migrations)
CREATE USER 'app_admin'@'localhost' IDENTIFIED BY 'password3';  
GRANT ALL PRIVILEGES ON ma_base.* TO 'app_admin'@'localhost';  
```

### Dans Delphi : Connexions multiples

```pascal
type
  TdmConnexions = class(TDataModule)
    FDConnectionReadOnly: TFDConnection;
    FDConnectionStandard: TFDConnection;
    FDConnectionAdmin: TFDConnection;
  public
    procedure ConfigurerConnexions;
  end;

procedure TdmConnexions.ConfigurerConnexions;  
begin  
  // Connexion lecture seule pour les rapports
  FDConnectionReadOnly.Params.Add('User_Name=app_readonly');
  FDConnectionReadOnly.Params.Add('Password=password1');

  // Connexion standard pour l'application
  FDConnectionStandard.Params.Add('User_Name=app_standard');
  FDConnectionStandard.Params.Add('Password=password2');

  // Connexion admin pour les migrations (utilisée rarement)
  FDConnectionAdmin.Params.Add('User_Name=app_admin');
  FDConnectionAdmin.Params.Add('Password=password3');
end;

procedure GenererRapport;  
begin  
  // Utiliser la connexion read-only
  FDQuery1.Connection := dmConnexions.FDConnectionReadOnly;
  FDQuery1.SQL.Text := 'SELECT * FROM statistiques';
  FDQuery1.Open;
end;
```

> ⚠️ **Important — nom de la classe** : la classe doit avoir un nom **différent** de `TDataModule` (par exemple `TdmConnexions`). Une déclaration `TDataModule = class(TDataModule)` ne compile pas (référence circulaire à elle-même).

## Validation des entrées utilisateur

### Valider côté client

```pascal
function ValiderEmail(const Email: string): Boolean;  
begin  
  Result := (Trim(Email) <> '') and
            (Pos('@', Email) > 0) and
            (Pos('.', Email) > Pos('@', Email));
end;

function ValiderNumerique(const Texte: string): Boolean;  
var  
  Valeur: Integer;
begin
  Result := TryStrToInt(Texte, Valeur);
end;

procedure TForm1.btnSaveClick(Sender: TObject);  
begin  
  // Valider avant d'envoyer à la base
  if not ValiderEmail(editEmail.Text) then
  begin
    ShowMessage('Email invalide');
    editEmail.SetFocus;
    Exit;
  end;

  if not ValiderNumerique(editAge.Text) then
  begin
    ShowMessage('Âge invalide');
    editAge.SetFocus;
    Exit;
  end;

  // OK, sauvegarder
  SauvegarderClient;
end;
```

### Limiter la longueur des entrées

```pascal
// Dans le formulaire
editNom.MaxLength := 100;  
editEmail.MaxLength := 150;  
editTelephone.MaxLength := 20;  
```

### Échapper les caractères spéciaux

```pascal
function EchapperHTML(const Texte: string): string;  
begin  
  Result := Texte;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;
```

## Protection contre les attaques par force brute

### Limiter les tentatives de connexion

```pascal
// Unités nécessaires : System.DateUtils (MinutesBetween),
// System.Generics.Collections (TDictionary)
uses
  System.SysUtils, System.DateUtils, System.Generics.Collections;

type
  TTentativeConnexion = record
    Login: string;
    NombreTentatives: Integer;
    DerniereTentative: TDateTime;
  end;

var
  // Variable globale unique pour toute l'application — à créer dans
  // le bloc initialization de l'unité (ou au démarrage de l'app) et
  // à libérer dans finalization (voir note plus bas).
  Tentatives: TDictionary<string, TTentativeConnexion>;

function TentativeAutorisee(const Login: string): Boolean;  
var  
  Tentative: TTentativeConnexion;
  MinutesEcoulees: Double;
begin
  Result := True;

  if Tentatives.TryGetValue(Login, Tentative) then
  begin
    MinutesEcoulees := MinutesBetween(Now, Tentative.DerniereTentative);

    // Si plus de 3 tentatives en moins de 5 minutes
    if (Tentative.NombreTentatives >= 3) and (MinutesEcoulees < 5) then
    begin
      Result := False;
      ShowMessage('Trop de tentatives. Réessayez dans ' +
        IntToStr(5 - Round(MinutesEcoulees)) + ' minute(s)');
    end
    else if MinutesEcoulees >= 5 then
    begin
      // Réinitialiser après 5 minutes
      Tentative.NombreTentatives := 0;
      Tentatives.AddOrSetValue(Login, Tentative);
    end;
  end;
end;

procedure EnregistrerTentativeEchouee(const Login: string);  
var  
  Tentative: TTentativeConnexion;
begin
  if Tentatives.TryGetValue(Login, Tentative) then
  begin
    Tentative.NombreTentatives := Tentative.NombreTentatives + 1;
    Tentative.DerniereTentative := Now;
  end
  else
  begin
    Tentative.Login := Login;
    Tentative.NombreTentatives := 1;
    Tentative.DerniereTentative := Now;
  end;

  Tentatives.AddOrSetValue(Login, Tentative);
end;

// Utilisation
procedure TFormLogin.btnConnecterClick(Sender: TObject);  
begin  
  if not TentativeAutorisee(editLogin.Text) then
    Exit;

  if VerifierConnexion(editLogin.Text, editPassword.Text) then
  begin
    ShowMessage('Connexion réussie');
    // Réinitialiser les tentatives en cas de succès
    Tentatives.Remove(editLogin.Text);
  end
  else
  begin
    EnregistrerTentativeEchouee(editLogin.Text);
    ShowMessage('Identifiants invalides');
  end;
end;

// À placer en fin d'unité, après tout le code :
initialization
  Tentatives := TDictionary<string, TTentativeConnexion>.Create;

finalization
  Tentatives.Free;
```

> ⚠️ **Note pour la production** :  
>  
> - **Thread-safety** : si l'application est multi-thread (serveur, web service…), protégez les accès à `Tentatives` avec un `TCriticalSection` — sinon deux threads concurrents peuvent corrompre le dictionnaire.  
> - **Persistance** : le dictionnaire est en mémoire et redémarre à zéro à chaque relance de l'application. Pour une vraie protection en production, stockez le compteur dans la **base de données** (table `tentatives_connexion`) avec horodatage, et purgez périodiquement les anciennes entrées.  
> - **Identification de l'attaquant** : limiter par login seul ne suffit pas — un attaquant peut tester des logins différents. Limiter aussi par **adresse IP** (ou les deux en combinaison) ; à coupler avec un système comme **fail2ban** au niveau OS pour bannir les IP suspectes.

## Audit et journalisation (Logging)

### Logger les actions sensibles

```pascal
unit uAuditLogger;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.IOUtils;

type
  TAuditLogger = class
  private
    FLogFile: string;
    FLock: TCriticalSection;  // Sérialise les écritures concurrentes
    procedure EcrireLigne(const Ligne: string);
  public
    constructor Create(const CheminLog: string);
    destructor Destroy; override;

    procedure LogConnexion(const Utilisateur: string; Succes: Boolean);
    procedure LogAction(const Utilisateur, Action, Details: string);
    procedure LogErreur(const Utilisateur, Erreur: string);
  end;

implementation

constructor TAuditLogger.Create(const CheminLog: string);  
begin  
  inherited Create;
  FLogFile := CheminLog;
  FLock := TCriticalSection.Create;
end;

destructor TAuditLogger.Destroy;  
begin  
  FLock.Free;
  inherited;
end;

procedure TAuditLogger.EcrireLigne(const Ligne: string);  
var  
  Ligne_UTF8: TBytes;
  Stream: TFileStream;
begin
  // Préparer la ligne en UTF-8 avec un saut de ligne final
  Ligne_UTF8 := TEncoding.UTF8.GetBytes(
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' | ' + Ligne + sLineBreak);

  // Section critique : un seul thread écrit à la fois pour éviter
  // l'entrelacement des lignes ou la corruption du fichier.
  FLock.Enter;
  try
    if TFile.Exists(FLogFile) then
      Stream := TFileStream.Create(FLogFile, fmOpenWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FLogFile, fmCreate);
    try
      Stream.Seek(0, soEnd);  // Ajouter à la fin
      Stream.WriteBuffer(Ligne_UTF8[0], Length(Ligne_UTF8));
    finally
      Stream.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAuditLogger.LogConnexion(const Utilisateur: string; Succes: Boolean);  
begin  
  if Succes then
    EcrireLigne('CONNEXION RÉUSSIE | ' + Utilisateur)
  else
    EcrireLigne('CONNEXION ÉCHOUÉE | ' + Utilisateur);
end;

procedure TAuditLogger.LogAction(const Utilisateur, Action, Details: string);  
begin  
  EcrireLigne('ACTION | ' + Utilisateur + ' | ' + Action + ' | ' + Details);
end;

procedure TAuditLogger.LogErreur(const Utilisateur, Erreur: string);  
begin  
  EcrireLigne('ERREUR | ' + Utilisateur + ' | ' + Erreur);
end;

end.
```

> 💡 **Pourquoi ces choix ?**  
>  
> - **`TCriticalSection`** : sans verrou, deux threads qui appellent `LogAction` en même temps peuvent entrelacer leurs lignes ou corrompre le fichier. La section critique sérialise l'écriture.  
> - **`TFileStream` + `TEncoding.UTF8`** : `TextFile`/`WriteLn` écrit en encodage ANSI système sous Windows, ce qui casse les accents (« RÉUSSIE », « ÉCHOUÉE ») à l'ouverture sous Linux ou dans un éditeur Unicode. `TFileStream` + UTF-8 produit un fichier portable.  
> - **`fmShareDenyWrite`** : permet à un outil de monitoring (`tail -f`, lecteur de logs) d'ouvrir le fichier en lecture pendant qu'on y écrit.

**Utilisation :**

```pascal
var
  AuditLogger: TAuditLogger;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  AuditLogger := TAuditLogger.Create('audit.log');
end;

procedure TFormMain.SupprimerClient(ClientID: Integer);  
begin  
  AuditLogger.LogAction(
    UtilisateurConnecte,
    'SUPPRESSION CLIENT',
    'ID: ' + IntToStr(ClientID)
  );

  // Supprimer...
end;
```

### Logger dans la base de données

```sql
CREATE TABLE audit_log (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    date_action DATETIME NOT NULL,
    utilisateur VARCHAR(50),
    action VARCHAR(100),
    table_concernee VARCHAR(50),
    id_enregistrement INT,
    details TEXT,
    ip_address VARCHAR(45)
);

CREATE INDEX idx_date ON audit_log(date_action);  
CREATE INDEX idx_user ON audit_log(utilisateur);  
```

```pascal
procedure LoggerActionBDD(const Utilisateur, Action, TableConcernee: string;
  ID: Integer; const Details: string);
begin
  FDQuery1.SQL.Text :=
    'INSERT INTO audit_log (date_action, utilisateur, action, ' +
    '  table_concernee, id_enregistrement, details) ' +
    'VALUES (NOW(), :user, :action, :table, :id, :details)';

  FDQuery1.ParamByName('user').AsString := Utilisateur;
  FDQuery1.ParamByName('action').AsString := Action;
  FDQuery1.ParamByName('table').AsString := TableConcernee;
  FDQuery1.ParamByName('id').AsInteger := ID;
  FDQuery1.ParamByName('details').AsString := Details;

  FDQuery1.ExecSQL;
end;
```

## Chiffrement des données sensibles

### Chiffrer les données personnelles

> ⚠️ **À nouveau, attention au vocabulaire** : l'exemple ci-dessous illustre le **principe** d'un chiffrement à clé répétée, mais ce **n'est PAS** un chiffrement sérieux. **Ne stockez jamais** un vrai numéro de sécurité sociale, une carte bancaire, ou des données médicales avec ce code. Utilisez **AES-256-GCM** via une bibliothèque éprouvée (voir liste plus bas).

```sql
-- Stocker des données chiffrées
CREATE TABLE clients (
    id INT PRIMARY KEY,
    nom VARCHAR(100),
    email VARCHAR(150),
    numero_securite_sociale_chiffre BLOB,  -- Données sensibles chiffrées
    cle_chiffrement VARCHAR(255)
);
```

Exemple **pédagogique** (XOR à clé répétée, NE PAS UTILISER EN PRODUCTION) :

```pascal
uses
  System.NetEncoding;

function OffusquerDonnees(const Donnees: string; const Cle: string): string;  
var  
  Bytes: TBytes;
  i: Integer;
begin
  // ⚠️ XOR à clé répétée — facile à casser par analyse de fréquence.
  // À titre d'illustration uniquement. Pour de vraies données sensibles,
  // utilisez AES-256-GCM (cf. Sodium for Delphi, TMS Cryptography Pack).
  Bytes := TEncoding.UTF8.GetBytes(Donnees);
  for i := 0 to Length(Bytes) - 1 do
    Bytes[i] := Bytes[i] xor Ord(Cle[1 + (i mod Length(Cle))]);

  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
end;

function DesoffusquerDonnees(const DonneesOffusquees: string; const Cle: string): string;  
var  
  Bytes: TBytes;
  i: Integer;
begin
  Bytes := TNetEncoding.Base64.DecodeStringToBytes(DonneesOffusquees);

  for i := 0 to Length(Bytes) - 1 do
    Bytes[i] := Bytes[i] xor Ord(Cle[1 + (i mod Length(Cle))]);

  Result := TEncoding.UTF8.GetString(Bytes);
end;

// Utilisation (à des fins pédagogiques uniquement)
procedure SauvegarderClientAvecDonneesSensibles;  
var  
  NSS_Offusque: string;
  Cle: string;
begin
  Cle := 'CleSecreteTresDifficile123!@#';
  NSS_Offusque := OffusquerDonnees('1 85 03 75 116 234 56', Cle);

  FDQuery1.SQL.Text :=
    'INSERT INTO clients (nom, email, numero_securite_sociale_chiffre) ' +
    'VALUES (:nom, :email, :nss)';

  FDQuery1.ParamByName('nom').AsString := 'Dupont';
  FDQuery1.ParamByName('email').AsString := 'dupont@email.fr';
  FDQuery1.ParamByName('nss').AsString := NSS_Offusque;

  FDQuery1.ExecSQL;
end;
```

> 💡 **Données ultra-sensibles (numéro de sécurité sociale, carte bancaire, données de santé)** : ces données sont encadrées par la **RGPD** (Europe) et le **PCI DSS** (cartes bancaires). Dans la plupart des cas, la bonne approche n'est **pas de chiffrer vous-même**, mais :  
>  
> - **Tokeniser** : ne jamais stocker le numéro brut, le remplacer par un identifiant opaque (le vrai numéro est géré par un prestataire spécialisé comme Stripe, Adyen…)  
> - **Déléguer** à un coffre-fort géré (HashiCorp Vault, AWS KMS, Azure Key Vault)  
> - **Hash + sel** si la donnée n'a besoin que d'être comparée et jamais relue en clair  
>  
> Ne réinventez pas la cryptographie — utilisez des composants éprouvés et audités.

## Checklist de sécurité

### ✅ Base de données

- [ ] Requêtes paramétrées pour TOUTES les entrées utilisateur
- [ ] Mots de passe hachés avec **Argon2id** ou **BCrypt** (PAS SHA-256 nu — voir section dédiée)
- [ ] Connexions SSL/TLS activées
- [ ] Principe du moindre privilège (droits minimaux)
- [ ] Pas de compte root/admin dans l'application
- [ ] Sauvegardes régulières
- [ ] Base de données à jour (patches de sécurité)

### ✅ Application

- [ ] Identifiants NON stockés en dur dans le code
- [ ] Validation de toutes les entrées utilisateur
- [ ] Protection contre force brute (limite de tentatives)
- [ ] Journalisation des actions sensibles
- [ ] Gestion des erreurs sans révéler d'informations sensibles
- [ ] Chiffrement des données personnelles
- [ ] Timeout de session
- [ ] Protection des fichiers de configuration

### ✅ Réseau

- [ ] Pare-feu configuré
- [ ] Port MySQL (3306) NON exposé à Internet
- [ ] Connexions depuis IP autorisées uniquement
- [ ] VPN pour accès distant
- [ ] Certificats SSL valides

### ✅ Code

- [ ] Pas de mots de passe dans les commentaires
- [ ] Pas de code de debug en production
- [ ] Gestion propre des exceptions
- [ ] Nettoyage des ressources (queries, connexions)
- [ ] Code review réguliers

## Tests de sécurité

### Tester les injections SQL

```pascal
procedure TesterInjectionSQL;  
var  
  CasTests: array[0..4] of string;
  i: Integer;
begin
  CasTests[0] := ''' OR ''1''=''1';
  CasTests[1] := '''; DROP TABLE clients; --';
  CasTests[2] := ''' UNION SELECT * FROM utilisateurs --';
  CasTests[3] := ''') OR (''1''=''1';
  CasTests[4] := 'admin''--';

  for i := Low(CasTests) to High(CasTests) do
  begin
    try
      // Tenter la connexion avec l'injection
      TesterConnexion('admin', CasTests[i]);

      // Si on arrive ici, c'est que l'injection N'A PAS fonctionné (bon)
      Memo1.Lines.Add('✓ Injection bloquée : ' + CasTests[i]);
    except
      on E: Exception do
        Memo1.Lines.Add('✗ VULNÉRABLE à : ' + CasTests[i]);
    end;
  end;
end;
```

### Scanner de vulnérabilités

Utilisez des outils professionnels :
- **SQLMap** : détection d'injections SQL
- **OWASP ZAP** : scanner de sécurité web
- **Nessus** : scanner de vulnérabilités réseau

## Bonnes pratiques de développement sécurisé

### 1. Principe de défense en profondeur

```
┌─────────────────────────────────────┐
│  Couche 1 : Validation UI           │  ← Première ligne de défense
├─────────────────────────────────────┤
│  Couche 2 : Validation métier       │  ← Vérifications business
├─────────────────────────────────────┤
│  Couche 3 : Requêtes paramétrées    │  ← Protection SQL
├─────────────────────────────────────┤
│  Couche 4 : Droits base de données  │  ← Limitation des permissions
├─────────────────────────────────────┤
│  Couche 5 : Pare-feu réseau         │  ← Protection réseau
└─────────────────────────────────────┘
```

### 2. Fail secure (échec sécurisé)

```pascal
// ✅ Par défaut : refuser
function UtilisateurADroit(Utilisateur: string; Droit: string): Boolean;  
begin  
  Result := False;  // Par défaut : pas de droit

  try
    // Vérifier les droits
    if VerifierDroitsDansBase(Utilisateur, Droit) then
      Result := True;
  except
    Result := False;  // En cas d'erreur : refuser
  end;
end;
```

### 3. Principe du moindre étonnement

```pascal
// ✅ Comportement prévisible
if not UtilisateurADroit(User, 'SUPPRIMER') then  
begin  
  ShowMessage('Vous n''avez pas les droits pour supprimer');
  Exit;
end;

// ❌ Comportement trompeur
if UtilisateurADroit(User, 'SUPPRIMER') then
  // Supprimer
else
  // Ne rien faire silencieusement (l'utilisateur ne sait pas pourquoi)
```

## Ressources et standards

### Standards de sécurité

- **OWASP Top 10** : Les 10 vulnérabilités les plus critiques
- **PCI DSS** : Standard pour les paiements par carte
- **RGPD** : Protection des données personnelles (Europe)
- **ISO 27001** : Système de management de la sécurité

### Bibliothèques recommandées

- **Sodium for Delphi** (libsodium) : Argon2id pour les mots de passe, AES-256-GCM pour les données — API moderne, défauts sûrs
- **DelphiBcrypt** : Hachage de mots de passe BCrypt (open source)
- **TMS Cryptography Pack** : Suite cryptographique complète (commercial)
- **OpenSSL** : Chiffrement et SSL/TLS (via Indy ou wrapper)
- **Indy** : Communications sécurisées (TLS/SSL, SMTPS, HTTPS…)
- **DCPCrypt** : Bibliothèque historique open source (chiffrement symétrique/asymétrique)

## Résumé

### Les 5 règles d'or

1. **TOUJOURS utiliser des paramètres SQL**
   ```pascal
   // ✅ ParamByName
   // ❌ Concaténation
   ```

2. **JAMAIS stocker les mots de passe en clair**
   ```pascal
   // ✅ Hash : Argon2id, BCrypt, scrypt (algorithmes "lents" anti-bruteforce)
   // ❌ SHA-256 / MD5 / SHA-1 nus : trop rapides, vulnérables au GPU bruteforce
   // ❌ Texte brut
   ```

3. **Appliquer le principe du moindre privilège**
   ```pascal
   // ✅ Droits minimaux
   // ❌ ALL PRIVILEGES
   ```

4. **Valider toutes les entrées utilisateur**
   ```pascal
   // ✅ Validation stricte
   // ❌ Confiance aveugle
   ```

5. **Logger les actions sensibles**
   ```pascal
   // ✅ Audit trail
   // ❌ Pas de traçabilité
   ```

### Menaces principales

| Menace | Solution |
|--------|----------|
| **Injection SQL** | Requêtes paramétrées |
| **Injection NoSQL** (MongoDB) | Échappement via `TJSONObject` + désactiver `$where` |
| **Mots de passe compromis** | Hachage **Argon2id** ou BCrypt (PAS SHA-256 nu) |
| **Écoute réseau** | SSL/TLS |
| **Accès non autorisé** | Authentification + droits |
| **Force brute** | Limitation de tentatives + algos lents |
| **Fuite de données** | Chiffrement AES-256-GCM + audit |
| **Identifiants en dur** | Variables d'env / coffre OS / KMS |

### Checklist rapide avant déploiement

- [ ] Aucune concaténation SQL dans le code
- [ ] Tous les mots de passe hachés
- [ ] SSL activé pour connexions distantes
- [ ] Droits utilisateurs minimaux
- [ ] Fichiers de config sécurisés
- [ ] Logging activé
- [ ] Tests de sécurité effectués
- [ ] Code review réalisé

## Conclusion

La sécurité n'est **jamais** une réflexion après coup. Elle doit être intégrée dès la conception de votre application. Une seule faille de sécurité peut avoir des conséquences catastrophiques : perte de données, vol d'informations, poursuites légales, perte de confiance des clients.

**Rappelez-vous :** La sécurité est un processus continu, pas une destination. Restez vigilant, formez-vous régulièrement, et suivez les meilleures pratiques de l'industrie.

Avec les connaissances de ce chapitre, vous êtes maintenant capable de créer des applications Delphi sécurisées qui protègent efficacement les données de vos utilisateurs !

⏭️ [Autres moteurs de bases de données (SQLite, PostgreSQL, SQL Server)](/08-acces-aux-bases-de-donnees-mysql-mariadb/12-autres-moteurs-de-bases-de-donnees.md)
