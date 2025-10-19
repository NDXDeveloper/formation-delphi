🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.1 Authentification des utilisateurs

## Introduction

L'authentification des utilisateurs est un mécanisme fondamental de sécurité qui permet de vérifier l'identité d'une personne souhaitant accéder à votre application. C'est généralement la première ligne de défense pour protéger vos données et fonctionnalités sensibles.

**Qu'est-ce que l'authentification ?**

L'authentification répond à la question : "Qui êtes-vous ?" Elle permet de s'assurer que l'utilisateur est bien celui qu'il prétend être, généralement via un nom d'utilisateur et un mot de passe.

> **À ne pas confondre avec l'autorisation** : L'authentification vérifie l'identité, tandis que l'autorisation détermine ce que l'utilisateur peut faire une fois authentifié.

## Les différents types d'authentification

### 1. Authentification simple (nom d'utilisateur/mot de passe)

C'est la méthode la plus courante et la plus simple à implémenter. L'utilisateur fournit :
- Un identifiant unique (nom d'utilisateur, email, etc.)
- Un mot de passe secret

### 2. Authentification à deux facteurs (2FA)

Cette méthode ajoute une couche de sécurité supplémentaire en demandant :
- Quelque chose que vous savez (mot de passe)
- Quelque chose que vous possédez (code SMS, application d'authentification)

### 3. Authentification biométrique

Utilise des caractéristiques physiques uniques :
- Empreinte digitale
- Reconnaissance faciale
- Scan de l'iris

### 4. Authentification par certificat

Utilise des certificats numériques pour identifier l'utilisateur, courante dans les environnements d'entreprise.

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
var
  MotDePasseHash: string;
  Salt: string;
  HashCalcule: string;
begin
  // Récupérer le salt et le hash depuis la base de données
  FDQuery1.SQL.Text := 'SELECT MotDePasseHash, Salt FROM Utilisateurs ' +
                        'WHERE NomUtilisateur = :Username AND Actif = TRUE';
  FDQuery1.ParamByName('Username').AsString := EditUtilisateur.Text;
  FDQuery1.Open;

  if not FDQuery1.IsEmpty then
  begin
    Salt := FDQuery1.FieldByName('Salt').AsString;
    MotDePasseHash := FDQuery1.FieldByName('MotDePasseHash').AsString;

    // Calculer le hash du mot de passe saisi
    HashCalcule := CalculerHash(EditMotDePasse.Text + Salt);

    // Comparer les hash
    if HashCalcule = MotDePasseHash then
    begin
      ShowMessage('Connexion réussie !');
      // Créer la session utilisateur
      // Ouvrir le formulaire principal
    end
    else
      ShowMessage('Mot de passe incorrect');
  end
  else
    ShowMessage('Utilisateur non trouvé');

  FDQuery1.Close;
end;
```

### Création d'un nouvel utilisateur

Lors de l'inscription d'un nouvel utilisateur :

```pascal
procedure TFormInscription.BtnInscrireClick(Sender: TObject);
var
  Salt: string;
  HashMotDePasse: string;
begin
  // Générer un salt aléatoire
  Salt := GenererSaltAleatoire();

  // Calculer le hash du mot de passe avec le salt
  HashMotDePasse := CalculerHash(EditMotDePasse.Text + Salt);

  // Insérer dans la base de données
  FDQuery1.SQL.Text := 'INSERT INTO Utilisateurs (NomUtilisateur, Email, MotDePasseHash, Salt) ' +
                        'VALUES (:Username, :Email, :Hash, :Salt)';
  FDQuery1.ParamByName('Username').AsString := EditUtilisateur.Text;
  FDQuery1.ParamByName('Email').AsString := EditEmail.Text;
  FDQuery1.ParamByName('Hash').AsString := HashMotDePasse;
  FDQuery1.ParamByName('Salt').AsString := Salt;

  try
    FDQuery1.ExecSQL;
    ShowMessage('Inscription réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur lors de l\'inscription : ' + E.Message);
  end;
end;
```

## Fonctions utilitaires pour le hashage

Delphi propose plusieurs unités pour le hashage. Voici un exemple avec l'unité `System.Hash` :

```pascal
uses
  System.Hash, System.SysUtils;

function CalculerHash(const ATexte: string): string;
begin
  // Utilise SHA-256 pour créer un hash sécurisé
  Result := THashSHA2.GetHashString(ATexte);
end;

function GenererSaltAleatoire(): string;
var
  GUID: TGUID;
begin
  // Génère un GUID unique comme salt
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;
```

## Gestion de la session utilisateur

Une fois l'utilisateur authentifié, il faut gérer sa session :

### Variables globales de session

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

Imposez des règles pour les mots de passe :
- Longueur minimale (8-12 caractères)
- Combinaison de majuscules, minuscules, chiffres et caractères spéciaux
- Pas de mots du dictionnaire

```pascal
function MotDePasseValide(const AMotDePasse: string): Boolean;
var
  AMajuscule, AMinuscule, AChiffre, ASpecial: Boolean;
  i: Integer;
begin
  Result := False;

  // Vérifier la longueur minimale
  if Length(AMotDePasse) < 8 then
    Exit;

  AMajuscule := False;
  AMinuscule := False;
  AChiffre := False;
  ASpecial := False;

  // Vérifier les différents types de caractères
  for i := 1 to Length(AMotDePasse) do
  begin
    if CharInSet(AMotDePasse[i], ['A'..'Z']) then
      AMajuscule := True
    else if CharInSet(AMotDePasse[i], ['a'..'z']) then
      AMinuscule := True
    else if CharInSet(AMotDePasse[i], ['0'..'9']) then
      AChiffre := True
    else
      ASpecial := True;
  end;

  Result := AMajuscule and AMinuscule and AChiffre and ASpecial;
end;
```

### 2. Limitation des tentatives de connexion

Pour éviter les attaques par force brute :

```pascal
var
  TentativesEchouees: Integer = 0;
  DerniereeTentative: TDateTime;

const
  MAX_TENTATIVES = 5;
  DELAI_BLOCAGE_MINUTES = 15;

procedure TFormConnexion.BtnConnexionClick(Sender: TObject);
begin
  // Vérifier si le compte est temporairement bloqué
  if (TentativesEchouees >= MAX_TENTATIVES) and
     (MinutesBetween(Now, DerniereTentative) < DELAI_BLOCAGE_MINUTES) then
  begin
    ShowMessage('Trop de tentatives échouées. Réessayez dans ' +
                IntToStr(DELAI_BLOCAGE_MINUTES - MinutesBetween(Now, DerniereTentative)) +
                ' minutes.');
    Exit;
  end;

  // Réinitialiser si le délai est passé
  if MinutesBetween(Now, DerniereTentative) >= DELAI_BLOCAGE_MINUTES then
    TentativesEchouees := 0;

  // Vérifier les identifiants
  if VerifierAuthentification(EditUtilisateur.Text, EditMotDePasse.Text) then
  begin
    TentativesEchouees := 0;
    // Connexion réussie
  end
  else
  begin
    Inc(TentativesEchouees);
    DerniereTentative := Now;
    ShowMessage('Identifiants incorrects. Tentative ' +
                IntToStr(TentativesEchouees) + '/' + IntToStr(MAX_TENTATIVES));
  end;
end;
```

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
    AdresseIP VARCHAR(45),
    Navigateur VARCHAR(200)
);
```

## Authentification avancée

### Récupération de mot de passe

Pour permettre aux utilisateurs de réinitialiser leur mot de passe :

1. **Générer un token unique temporaire**
```pascal
function GenererTokenReset(): string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;
```

2. **Enregistrer le token avec une date d'expiration**
```sql
ALTER TABLE Utilisateurs ADD COLUMN TokenReset VARCHAR(100);
ALTER TABLE Utilisateurs ADD COLUMN TokenExpiration DATETIME;
```

3. **Envoyer un email avec le lien de réinitialisation**

4. **Vérifier le token et permettre la création d'un nouveau mot de passe**

### "Se souvenir de moi"

Pour garder l'utilisateur connecté entre les sessions :

```pascal
procedure SauvegarderToken(const AToken: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    IniFile.WriteString('Session', 'Token', AToken);
  finally
    IniFile.Free;
  end;
end;

function ChargerToken(): string;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Result := IniFile.ReadString('Session', 'Token', '');
  finally
    IniFile.Free;
  end;
end;
```

## Considérations pour les applications multi-plateformes

Lorsque vous développez avec FireMonkey pour plusieurs plateformes :

### Stockage sécurisé des identifiants

- **Windows** : Utilisez le système de credentials Windows
- **macOS** : Utilisez le Keychain
- **iOS/Android** : Utilisez le stockage sécurisé natif

Delphi fournit des APIs pour accéder à ces systèmes de manière unifiée.

### Authentification biométrique mobile

Pour iOS et Android, vous pouvez intégrer Touch ID / Face ID / Empreinte digitale :

```pascal
uses
  FMX.Platform, FMX.Biometrics;

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

## Authentification moderne avec OAuth2 et SSO

### Qu'est-ce que OAuth2 ?

OAuth2 permet aux utilisateurs de se connecter avec leurs comptes existants (Google, Microsoft, Facebook, etc.) sans créer de nouveau compte.

**Avantages** :
- Expérience utilisateur simplifiée
- Pas besoin de gérer les mots de passe
- Sécurité renforcée par les grands fournisseurs

### Single Sign-On (SSO)

Le SSO permet à un utilisateur de se connecter une seule fois et d'accéder à plusieurs applications.

**Implémentation basique** :
1. Rediriger l'utilisateur vers le fournisseur d'identité
2. Recevoir un token d'authentification
3. Valider le token
4. Créer la session locale

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
