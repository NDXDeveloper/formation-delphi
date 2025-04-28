# 8.11 Sécurisation des accès et prévention des injections SQL

La sécurité est un aspect fondamental du développement d'applications qui manipulent des bases de données. Une application non sécurisée peut exposer des données sensibles ou permettre à des utilisateurs malveillants de compromettre votre système. Dans cette section, nous allons explorer les techniques essentielles pour sécuriser vos applications Delphi qui utilisent MySQL/MariaDB.

## Comprendre les risques de sécurité

Avant d'aborder les solutions, identifions les principaux risques de sécurité liés aux bases de données :

### 1. Injection SQL

L'injection SQL est l'une des vulnérabilités les plus courantes et dangereuses. Elle se produit lorsqu'un attaquant insère du code SQL malveillant dans les entrées utilisateur qui sont ensuite exécutées par votre application.

**Exemple d'injection SQL simple :**

Imaginez une requête de connexion non sécurisée :

```delphi
// Approche NON SÉCURISÉE
Query.SQL.Text := 'SELECT * FROM utilisateurs WHERE nom = ''' + EditNom.Text +
                  ''' AND mot_de_passe = ''' + EditMotDePasse.Text + '''';
```

Si un utilisateur malveillant entre `' OR '1'='1` dans le champ de nom et n'importe quoi dans le champ de mot de passe, la requête devient :

```sql
SELECT * FROM utilisateurs WHERE nom = '' OR '1'='1' AND mot_de_passe = 'peu_importe'
```

Cette requête retournera tous les utilisateurs car la condition `'1'='1'` est toujours vraie !

### 2. Accès non autorisé

Un autre risque majeur est l'accès non autorisé à votre base de données, soit par des identifiants faibles, soit par des privilèges trop élevés accordés aux utilisateurs de la base de données.

### 3. Exposition de données sensibles

Stocker des données sensibles (mots de passe, informations de paiement, etc.) sans chiffrement approprié constitue un risque important en cas de compromission de la base de données.

## Prévention des injections SQL

La bonne nouvelle est que FireDAC offre des outils puissants pour prévenir les injections SQL. Voici les approches recommandées :

### 1. Utiliser des requêtes paramétrées

Les requêtes paramétrées (ou préparées) sont la meilleure défense contre les injections SQL. Elles permettent de séparer le code SQL des données.

**Exemple avec des paramètres nommés :**

```delphi
// Approche SÉCURISÉE avec des paramètres nommés
Query.SQL.Text := 'SELECT * FROM utilisateurs WHERE nom = :nom AND mot_de_passe = :mdp';
Query.ParamByName('nom').AsString := EditNom.Text;
Query.ParamByName('mdp').AsString := EditMotDePasse.Text;
Query.Open;
```

**Exemple avec des paramètres par position :**

```delphi
// Approche SÉCURISÉE avec des paramètres par position
Query.SQL.Text := 'SELECT * FROM utilisateurs WHERE nom = ? AND mot_de_passe = ?';
Query.Params[0].AsString := EditNom.Text;
Query.Params[1].AsString := EditMotDePasse.Text;
Query.Open;
```

Avec cette approche, même si un utilisateur entre du code SQL malveillant, il sera traité comme une simple chaîne de caractères et non comme du code exécutable.

### 2. Utiliser des procédures stockées

Les procédures stockées offrent une couche supplémentaire de sécurité car elles encapsulent la logique SQL dans la base de données.

**Exemple de procédure stockée MySQL :**

```sql
-- Créer la procédure stockée
DELIMITER //
CREATE PROCEDURE authentifier_utilisateur(IN p_nom VARCHAR(100), IN p_mdp VARCHAR(100))
BEGIN
    SELECT * FROM utilisateurs WHERE nom = p_nom AND mot_de_passe = p_mdp;
END //
DELIMITER ;
```

**Appel depuis Delphi :**

```delphi
// Utilisation d'une procédure stockée
Query.SQL.Text := 'CALL authentifier_utilisateur(:nom, :mdp)';
Query.ParamByName('nom').AsString := EditNom.Text;
Query.ParamByName('mdp').AsString := EditMotDePasse.Text;
Query.Open;
```

### 3. Validation des entrées utilisateur

Une bonne pratique consiste à valider toutes les entrées utilisateur avant de les utiliser dans des requêtes SQL.

```delphi
function TFormMain.ValiderEntreeUtilisateur(const Entree: string): Boolean;
begin
  // Vérifier la longueur minimum/maximum
  if (Length(Entree) < 3) or (Length(Entree) > 50) then
    Exit(False);

  // Vérifier les caractères autorisés (ex: lettres, chiffres et quelques symboles)
  for var i := 1 to Length(Entree) do
  begin
    if not CharInSet(Entree[i], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.', '@']) then
      Exit(False);
  end;

  Result := True;
end;

procedure TFormMain.ButtonConnexionClick(Sender: TObject);
begin
  if not ValiderEntreeUtilisateur(EditNom.Text) then
  begin
    ShowMessage('Nom d''utilisateur invalide');
    Exit;
  end;

  if not ValiderEntreeUtilisateur(EditMotDePasse.Text) then
  begin
    ShowMessage('Mot de passe invalide');
    Exit;
  end;

  // Continuer avec l'authentification sécurisée
  // ...
end;
```

### 4. Échapper les caractères spéciaux

Si pour une raison quelconque vous ne pouvez pas utiliser de requêtes paramétrées, assurez-vous d'échapper les caractères spéciaux dans les entrées utilisateur. Cette méthode est moins fiable que les requêtes paramétrées et devrait être évitée si possible.

```delphi
function EchapperSQL(const Texte: string): string;
begin
  Result := StringReplace(Texte, '''', '''''', [rfReplaceAll]);
  // Échapper d'autres caractères si nécessaire
end;
```

## Gestion sécurisée des mots de passe

Les mots de passe ne doivent jamais être stockés en clair dans une base de données. Voici comment les gérer de manière sécurisée :

### 1. Hachage des mots de passe

Utilisez des algorithmes de hachage spécifiques pour les mots de passe, comme BCrypt ou PBKDF2.

```delphi
// Utilisation de la bibliothèque BCrypt pour Delphi
uses
  System.SysUtils, BCrypt;

// Hacher un mot de passe
function HasherMotDePasse(const MotDePasse: string): string;
begin
  Result := TBCrypt.HashPassword(MotDePasse);
end;

// Vérifier un mot de passe
function VerifierMotDePasse(const MotDePasse, MotDePasseHache: string): Boolean;
begin
  Result := TBCrypt.CompareHash(MotDePasse, MotDePasseHache);
end;
```

**Note :** Vous devrez installer une bibliothèque de hachage comme BCrypt. Vous pouvez utiliser le gestionnaire de paquets GetIt de Delphi ou obtenir une bibliothèque compatible sur GitHub.

### 2. Stockage sécurisé des mots de passe dans la base de données

```sql
-- Structure de table recommandée pour les utilisateurs
CREATE TABLE utilisateurs (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom_utilisateur VARCHAR(50) NOT NULL UNIQUE,
  mot_de_passe_hache VARCHAR(100) NOT NULL,  -- Assez long pour stocker un hash BCrypt
  sel VARCHAR(50),  -- Si vous utilisez un sel personnalisé
  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP,
  derniere_connexion DATETIME,
  tentatives_echec INT DEFAULT 0
);
```

### 3. Implémentation d'une classe d'authentification sécurisée

```delphi
// UAuthentication.pas
unit UAuthentication;

interface

uses
  System.SysUtils, FireDAC.Comp.Client, BCrypt;

type
  TAuthentication = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);

    // Gestion des utilisateurs
    function CreerUtilisateur(const NomUtilisateur, MotDePasse: string): Boolean;
    function ModifierMotDePasse(const NomUtilisateur, AncienMotDePasse,
                                NouveauMotDePasse: string): Boolean;

    // Authentification
    function Authentifier(const NomUtilisateur, MotDePasse: string): Boolean;
    procedure EnregistrerConnexion(const NomUtilisateur: string);
    procedure EnregistrerEchecConnexion(const NomUtilisateur: string);
  end;

implementation

constructor TAuthentication.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TAuthentication.CreerUtilisateur(const NomUtilisateur, MotDePasse: string): Boolean;
var
  Query: TFDQuery;
  MotDePasseHache: string;
begin
  Result := False;

  // Vérifier que le nom d'utilisateur n'existe pas déjà
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT COUNT(*) FROM utilisateurs WHERE nom_utilisateur = :nom';
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.Open;

    if Query.Fields[0].AsInteger > 0 then
      Exit;  // Nom d'utilisateur déjà pris

    // Hacher le mot de passe
    MotDePasseHache := TBCrypt.HashPassword(MotDePasse);

    // Créer l'utilisateur
    Query.Close;
    Query.SQL.Text := 'INSERT INTO utilisateurs (nom_utilisateur, mot_de_passe_hache) ' +
                      'VALUES (:nom, :mdp)';
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.ParamByName('mdp').AsString := MotDePasseHache;
    Query.ExecSQL;

    Result := Query.RowsAffected > 0;
  finally
    Query.Free;
  end;
end;

function TAuthentication.Authentifier(const NomUtilisateur, MotDePasse: string): Boolean;
var
  Query: TFDQuery;
  MotDePasseHache: string;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT mot_de_passe_hache FROM utilisateurs WHERE nom_utilisateur = :nom';
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.Open;

    if Query.IsEmpty then
      Exit;  // Utilisateur non trouvé

    MotDePasseHache := Query.FieldByName('mot_de_passe_hache').AsString;

    // Vérifier le mot de passe
    if TBCrypt.CompareHash(MotDePasse, MotDePasseHache) then
    begin
      // Authentification réussie
      EnregistrerConnexion(NomUtilisateur);
      Result := True;
    end
    else
    begin
      // Échec d'authentification
      EnregistrerEchecConnexion(NomUtilisateur);
    end;
  finally
    Query.Free;
  end;
end;

procedure TAuthentication.EnregistrerConnexion(const NomUtilisateur: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE utilisateurs SET ' +
                      'derniere_connexion = NOW(), ' +
                      'tentatives_echec = 0 ' +
                      'WHERE nom_utilisateur = :nom';
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TAuthentication.EnregistrerEchecConnexion(const NomUtilisateur: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE utilisateurs SET ' +
                      'tentatives_echec = tentatives_echec + 1 ' +
                      'WHERE nom_utilisateur = :nom';
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TAuthentication.ModifierMotDePasse(const NomUtilisateur, AncienMotDePasse,
                                           NouveauMotDePasse: string): Boolean;
begin
  // Vérifier d'abord l'ancien mot de passe
  if not Authentifier(NomUtilisateur, AncienMotDePasse) then
    Exit(False);

  // Modifier le mot de passe
  var Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE utilisateurs SET ' +
                      'mot_de_passe_hache = :nouveau_mdp ' +
                      'WHERE nom_utilisateur = :nom';
    Query.ParamByName('nouveau_mdp').AsString := TBCrypt.HashPassword(NouveauMotDePasse);
    Query.ParamByName('nom').AsString := NomUtilisateur;
    Query.ExecSQL;

    Result := Query.RowsAffected > 0;
  finally
    Query.Free;
  end;
end;

end.
```

## Sécurisation de la connexion à la base de données

### 1. Privilèges minimaux

Suivez le principe du moindre privilège : accordez à chaque utilisateur de base de données uniquement les droits dont il a besoin.

```sql
-- Création d'un utilisateur avec privilèges limités pour l'application
CREATE USER 'app_user'@'localhost' IDENTIFIED BY 'password_secure';

-- Accorder uniquement les privilèges nécessaires
GRANT SELECT, INSERT, UPDATE, DELETE ON ma_base.* TO 'app_user'@'localhost';

-- Révoquer les privilèges dangereux
REVOKE DROP, ALTER, CREATE, REFERENCES ON ma_base.* FROM 'app_user'@'localhost';
```

### 2. Stockage sécurisé des informations de connexion

Ne stockez jamais les identifiants de base de données en dur dans votre code. Utilisez plutôt une des approches suivantes :

#### Option 1 : Fichier de configuration chiffré

```delphi
// UDBConfig.pas
unit UDBConfig;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  TDBConfig = class
  private
    class function DechiffrerTexte(const TexteChiffre: string): string;
    class function ChiffrerTexte(const Texte: string): string;
  public
    class procedure SauvegarderConfiguration(const Serveur, BaseDeDonnees,
                                            Utilisateur, MotDePasse: string);
    class procedure ChargerConfiguration(var Serveur, BaseDeDonnees,
                                        Utilisateur, MotDePasse: string);
  end;

implementation

// Une implémentation simple de chiffrement/déchiffrement
// (Utilisez une bibliothèque professionnelle pour une vraie application)
class function TDBConfig.ChiffrerTexte(const Texte: string): string;
var
  i: Integer;
  Cle: Integer;
begin
  Result := '';
  Cle := 129; // Clé simple, utilisez quelque chose de plus robuste en production

  for i := 1 to Length(Texte) do
    Result := Result + Char(Ord(Texte[i]) xor Cle);

  Result := EncodeBase64(Result);
end;

class function TDBConfig.DechiffrerTexte(const TexteChiffre: string): string;
var
  i: Integer;
  Cle: Integer;
  TexteDecoded: string;
begin
  Result := '';
  Cle := 129;

  TexteDecoded := DecodeBase64(TexteChiffre);

  for i := 1 to Length(TexteDecoded) do
    Result := Result + Char(Ord(TexteDecoded[i]) xor Cle);
end;

class procedure TDBConfig.SauvegarderConfiguration(const Serveur, BaseDeDonnees,
                                                  Utilisateur, MotDePasse: string);
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := ChangeFileExt(ParamStr(0), '.ini');

  Ini := TIniFile.Create(ConfigPath);
  try
    Ini.WriteString('Database', 'Server', ChiffrerTexte(Serveur));
    Ini.WriteString('Database', 'Database', ChiffrerTexte(BaseDeDonnees));
    Ini.WriteString('Database', 'User', ChiffrerTexte(Utilisateur));
    Ini.WriteString('Database', 'Password', ChiffrerTexte(MotDePasse));
  finally
    Ini.Free;
  end;
end;

class procedure TDBConfig.ChargerConfiguration(var Serveur, BaseDeDonnees,
                                              Utilisateur, MotDePasse: string);
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := ChangeFileExt(ParamStr(0), '.ini');

  if not FileExists(ConfigPath) then
    Exit;

  Ini := TIniFile.Create(ConfigPath);
  try
    Serveur := DechiffrerTexte(Ini.ReadString('Database', 'Server', ''));
    BaseDeDonnees := DechiffrerTexte(Ini.ReadString('Database', 'Database', ''));
    Utilisateur := DechiffrerTexte(Ini.ReadString('Database', 'User', ''));
    MotDePasse := DechiffrerTexte(Ini.ReadString('Database', 'Password', ''));
  finally
    Ini.Free;
  end;
end;

end.
```

**Utilisation :**

```delphi
procedure TFormMain.ConnecterBaseDeDonnees;
var
  Serveur, BaseDeDonnees, Utilisateur, MotDePasse: string;
begin
  // Charger les paramètres de connexion
  TDBConfig.ChargerConfiguration(Serveur, BaseDeDonnees, Utilisateur, MotDePasse);

  // Si la configuration n'existe pas encore, on affiche un formulaire de configuration
  if (Serveur = '') or (BaseDeDonnees = '') then
  begin
    FormConfig := TFormConfig.Create(Self);
    try
      if FormConfig.ShowModal = mrOk then
      begin
        // Sauvegarder la nouvelle configuration
        TDBConfig.SauvegarderConfiguration(
          FormConfig.EditServeur.Text,
          FormConfig.EditBaseDeDonnees.Text,
          FormConfig.EditUtilisateur.Text,
          FormConfig.EditMotDePasse.Text
        );

        // Recharger
        TDBConfig.ChargerConfiguration(Serveur, BaseDeDonnees, Utilisateur, MotDePasse);
      end
      else
        Exit;  // L'utilisateur a annulé
    finally
      FormConfig.Free;
    end;
  end;

  // Configurer la connexion
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=' + Serveur);
  FDConnection1.Params.Add('Database=' + BaseDeDonnees);
  FDConnection1.Params.Add('User_Name=' + Utilisateur);
  FDConnection1.Params.Add('Password=' + MotDePasse);

  try
    FDConnection1.Connected := True;
    StatusBar1.SimpleText := 'Connecté à ' + BaseDeDonnees;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      // Afficher le formulaire de configuration en cas d'erreur
      TFormConfig.Create(Self).ShowModal;
    end;
  end;
end;
```

#### Option 2 : Accès biométrique ou par authentification système

Pour les applications d'entreprise, vous pouvez utiliser l'authentification Windows ou biométrique (via des bibliothèques spécialisées) pour sécuriser l'accès aux informations de connexion.

### 3. Chiffrement de la connexion

Utilisez SSL/TLS pour chiffrer la connexion entre votre application et MySQL :

```delphi
procedure TFormMain.ConfigurerConnexionSecurisee;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=serveur.exemple.com');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=utilisateur');
  FDConnection1.Params.Add('Password=mot_de_passe');

  // Activer SSL/TLS
  FDConnection1.Params.Add('SSL=True');
  FDConnection1.Params.Add('SSLCACert=chemin/vers/ca-cert.pem');
  FDConnection1.Params.Add('SSLCert=chemin/vers/client-cert.pem');
  FDConnection1.Params.Add('SSLKey=chemin/vers/client-key.pem');

  try
    FDConnection1.Connected := True;
    StatusBar1.SimpleText := 'Connecté de manière sécurisée';
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

## Gestion des droits utilisateurs dans l'application

Outre la sécurité au niveau de la base de données, vous devez également gérer les droits des utilisateurs dans votre application.

### 1. Implémentation d'un système de rôles

```sql
-- Tables pour la gestion des rôles
CREATE TABLE roles (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(50) NOT NULL UNIQUE,
  description VARCHAR(255)
);

CREATE TABLE permissions (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(50) NOT NULL UNIQUE,
  description VARCHAR(255)
);

CREATE TABLE role_permissions (
  role_id INT NOT NULL,
  permission_id INT NOT NULL,
  PRIMARY KEY (role_id, permission_id),
  FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE,
  FOREIGN KEY (permission_id) REFERENCES permissions(id) ON DELETE CASCADE
);

CREATE TABLE utilisateur_roles (
  utilisateur_id INT NOT NULL,
  role_id INT NOT NULL,
  PRIMARY KEY (utilisateur_id, role_id),
  FOREIGN KEY (utilisateur_id) REFERENCES utilisateurs(id) ON DELETE CASCADE,
  FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE
);

-- Insertion de données de base
INSERT INTO roles (nom, description) VALUES
('admin', 'Administrateur avec tous les droits'),
('manager', 'Gestionnaire avec droits limités'),
('utilisateur', 'Utilisateur standard');

INSERT INTO permissions (nom, description) VALUES
('clients_view', 'Voir les clients'),
('clients_edit', 'Modifier les clients'),
('clients_delete', 'Supprimer des clients'),
('produits_view', 'Voir les produits'),
('produits_edit', 'Modifier les produits'),
('produits_delete', 'Supprimer des produits'),
('utilisateurs_view', 'Voir les utilisateurs'),
('utilisateurs_edit', 'Modifier les utilisateurs'),
('utilisateurs_delete', 'Supprimer des utilisateurs');

-- Attribuer des permissions aux rôles
INSERT INTO role_permissions (role_id, permission_id) VALUES
(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9),  -- Admin a toutes les permissions
(2, 1), (2, 2), (2, 4), (2, 5),  -- Manager peut voir/modifier clients et produits
(3, 1), (3, 4);  -- Utilisateur standard peut seulement voir clients et produits
```

### 2. Classe de gestion des droits

```delphi
// UGestionDroits.pas
unit UGestionDroits;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, FireDAC.Comp.Client;

type
  TGestionDroits = class
  private
    FConnection: TFDConnection;
    FUtilisateurID: Integer;
    FPermissions: TDictionary<string, Boolean>;

    procedure ChargerPermissions;
  public
    constructor Create(AConnection: TFDConnection; AUtilisateurID: Integer);
    destructor Destroy; override;

    function UtilisateurAPermission(const Permission: string): Boolean;
    function UtilisateurEstDansRole(const Role: string): Boolean;

    property UtilisateurID: Integer read FUtilisateurID;
  end;

implementation

constructor TGestionDroits.Create(AConnection: TFDConnection; AUtilisateurID: Integer);
begin
  inherited Create;
  FConnection := AConnection;
  FUtilisateurID := AUtilisateurID;
  FPermissions := TDictionary<string, Boolean>.Create;

  ChargerPermissions;
end;

destructor TGestionDroits.Destroy;
begin
  FPermissions.Free;
  inherited;
end;

procedure TGestionDroits.ChargerPermissions;
var
  Query: TFDQuery;
begin
  FPermissions.Clear;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT DISTINCT p.nom ' +
      'FROM permissions p ' +
      'JOIN role_permissions rp ON p.id = rp.permission_id ' +
      'JOIN utilisateur_roles ur ON rp.role_id = ur.role_id ' +
      'WHERE ur.utilisateur_id = :user_id';
    Query.ParamByName('user_id').AsInteger := FUtilisateurID;
    Query.Open;

    while not Query.Eof do
    begin
      FPermissions.Add(Query.FieldByName('nom').AsString, True);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TGestionDroits.UtilisateurAPermission(const Permission: string): Boolean;
begin
  Result := FPermissions.ContainsKey(Permission);
end;

function TGestionDroits.UtilisateurEstDansRole(const Role: string): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT 1 ' +
      'FROM utilisateur_roles ur ' +
      'JOIN roles r ON ur.role_id = r.id ' +
      'WHERE ur.utilisateur_id = :user_id AND r.nom = :role_nom';
    Query.ParamByName('user_id').AsInteger := FUtilisateurID;
    Query.ParamByName('role_nom').AsString := Role;
    Query.Open;

    Result := not Query.IsEmpty;
  finally
    Query.Free;
  end;
end;

end.
```

### 3. Utilisation de la gestion des droits dans vos formulaires

```delphi
procedure TFormMain.ApprouverAcces;
begin
  // Supposons que UtilisateurConnecte est défini lors de la connexion
  if not Assigned(FGestionDroits) then
    Exit;

  // Activer/désactiver les fonctionnalités selon les droits
  MenuAjouterClient.Visible := FGestionDroits.UtilisateurAPermission('clients_edit');
  MenuSupprimerClient.Visible := FGestionDroits.UtilisateurAPermission('clients_delete');

  ButtonModifierProduit.Enabled := FGestionDroits.UtilisateurAPermission('produits_edit');

  // Les administrateurs ont accès au panneau d'administration
  TabSheetAdmin.TabVisible := FGestionDroits.UtilisateurEstDansRole('admin');
end;
```

## Journalisation et audit de sécurité

Pour détecter et analyser les problèmes de sécurité, il est important de mettre en place une journalisation adéquate.

### 1. Table d'audit

```sql
CREATE TABLE journal_securite (
  id INT AUTO_INCREMENT PRIMARY KEY,
  date_evenement DATETIME DEFAULT CURRENT_TIMESTAMP,
  utilisateur_id INT,
  nom_utilisateur VARCHAR(50),
  adresse_ip VARCHAR(45),
  type_evenement VARCHAR(50) NOT NULL,
  details TEXT,
  FOREIGN KEY (utilisateur_id) REFERENCES utilisateurs(id) ON DELETE SET NULL
);
```

### 2. Classe de journalisation

```delphi
// UJournalSecurite.pas
unit UJournalSecurite;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, System.DateUtils;

type
  TTypeEvenement = (teConnexion, teDeconnexion, teEchecConnexion,
                    teAccesRefuse, teModificationDonnees, teSuppressionDonnees);

  TJournalSecurite = class
  private
    FConnection: TFDConnection;
    FUtilisateurID: Integer;
    FNomUtilisateur: string;

    function TypeEvenementToString(TypeEvenement: TTypeEvenement): string;
    function ObtenirAdresseIP: string;
  public
    constructor Create(AConnection: TFDConnection);

    procedure EnregistrerEvenement(TypeEvenement: TTypeEvenement;
                                  const Details: string = ''); overload;
    procedure EnregistrerEvenement(TypeEvenement: TTypeEvenement;
                                  UtilisateurID: Integer; const NomUtilisateur: string;
                                  const Details: string = ''); overload;

    procedure DefinirUtilisateurActif(UtilisateurID: Integer; const NomUtilisateur: string);

    // Méthodes pratiques
    procedure EnregistrerConnexion;
    procedure EnregistrerDeconnexion;
    procedure EnregistrerEchecConnexion(const NomUtilisateur: string);
    procedure EnregistrerAccesRefuse(const Resource: string);
    procedure EnregistrerModification(const TableNom: string; ID: Integer);
    procedure EnregistrerSuppression(const TableNom: string; ID: Integer);

    // Recherche dans les journaux
    function RechercherEvenements(DateDebut, DateFin: TDateTime;
                                 TypeEvenement: TTypeEvenement = TTypeEvenement(-1);
                                 UtilisateurID: Integer = 0): TFDQuery;
  end;

implementation

uses
  Winapi.Windows, Winapi.Winsock, IdStack;

constructor TJournalSecurite.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FUtilisateurID := 0;
  FNomUtilisateur := '';
end;

function TJournalSecurite.TypeEvenementToString(TypeEvenement: TTypeEvenement): string;
begin
  case TypeEvenement of
    teConnexion: Result := 'Connexion';
    teDeconnexion: Result := 'Déconnexion';
    teEchecConnexion: Result := 'Échec de connexion';
    teAccesRefuse: Result := 'Accès refusé';
    teModificationDonnees: Result := 'Modification de données';
    teSuppressionDonnees: Result := 'Suppression de données';
    else Result := 'Inconnu';
  end;
end;

function TJournalSecurite.ObtenirAdresseIP: string;
begin
  // Utiliser l'unité IdStack pour récupérer l'adresse IP locale
  try
    Result := GStack.LocalAddress;
  except
    Result := '127.0.0.1';  // Adresse de bouclage par défaut
  end;
end;

procedure TJournalSecurite.DefinirUtilisateurActif(UtilisateurID: Integer;
                                                  const NomUtilisateur: string);
begin
  FUtilisateurID := UtilisateurID;
  FNomUtilisateur := NomUtilisateur;
end;

procedure TJournalSecurite.EnregistrerEvenement(TypeEvenement: TTypeEvenement;
                                              const Details: string);
begin
  EnregistrerEvenement(TypeEvenement, FUtilisateurID, FNomUtilisateur, Details);
end;

procedure TJournalSecurite.EnregistrerEvenement(TypeEvenement: TTypeEvenement;
                                              UtilisateurID: Integer;
                                              const NomUtilisateur: string;
                                              const Details: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO journal_securite ' +
      '(utilisateur_id, nom_utilisateur, adresse_ip, type_evenement, details) ' +
      'VALUES (:user_id, :user_name, :ip, :event_type, :details)';

    if UtilisateurID > 0 then
      Query.ParamByName('user_id').AsInteger := UtilisateurID
    else
      Query.ParamByName('user_id').Clear;

    Query.ParamByName('user_name').AsString := NomUtilisateur;
    Query.ParamByName('ip').AsString := ObtenirAdresseIP;
    Query.ParamByName('event_type').AsString := TypeEvenementToString(TypeEvenement);
    Query.ParamByName('details').AsString := Details;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TJournalSecurite.EnregistrerConnexion;
begin
  EnregistrerEvenement(teConnexion, 'Connexion réussie');
end;

procedure TJournalSecurite.EnregistrerDeconnexion;
begin
  EnregistrerEvenement(teDeconnexion, 'Déconnexion');
end;

procedure TJournalSecurite.EnregistrerEchecConnexion(const NomUtilisateur: string);
begin
  EnregistrerEvenement(teEchecConnexion, 0, NomUtilisateur,
                     'Échec de connexion : mot de passe incorrect');
end;

procedure TJournalSecurite.EnregistrerAccesRefuse(const Resource: string);
begin
  EnregistrerEvenement(teAccesRefuse,
                     Format('Accès refusé à la ressource : %s', [Resource]));
end;

procedure TJournalSecurite.EnregistrerModification(const TableNom: string; ID: Integer);
begin
  EnregistrerEvenement(teModificationDonnees,
                     Format('Modification dans %s, ID: %d', [TableNom, ID]));
end;

procedure TJournalSecurite.EnregistrerSuppression(const TableNom: string; ID: Integer);
begin
  EnregistrerEvenement(teSuppressionDonnees,
                     Format('Suppression dans %s, ID: %d', [TableNom, ID]));
end;

function TJournalSecurite.RechercherEvenements(DateDebut, DateFin: TDateTime;
                                             TypeEvenement: TTypeEvenement;
                                             UtilisateurID: Integer): TFDQuery;
var
  SQL: string;
  Conditions: TStringList;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;

  SQL := 'SELECT * FROM journal_securite WHERE 1=1';
  Conditions := TStringList.Create;
  try
    // Ajouter les filtres conditionnels
    if DateDebut > 0 then
      Conditions.Add('date_evenement >= :date_debut');

    if DateFin > 0 then
      Conditions.Add('date_evenement <= :date_fin');

    if Integer(TypeEvenement) >= 0 then
      Conditions.Add('type_evenement = :type_evenement');

    if UtilisateurID > 0 then
      Conditions.Add('utilisateur_id = :utilisateur_id');

    // Construire la requête complète
    if Conditions.Count > 0 then
      SQL := SQL + ' AND ' + Conditions.DelimitedText.Replace(',', ' AND');

    SQL := SQL + ' ORDER BY date_evenement DESC';

    Result.SQL.Text := SQL;

    // Définir les paramètres
    if DateDebut > 0 then
      Result.ParamByName('date_debut').AsDateTime := DateDebut;

    if DateFin > 0 then
      Result.ParamByName('date_fin').AsDateTime := DateFin;

    if Integer(TypeEvenement) >= 0 then
      Result.ParamByName('type_evenement').AsString := TypeEvenementToString(TypeEvenement);

    if UtilisateurID > 0 then
      Result.ParamByName('utilisateur_id').AsInteger := UtilisateurID;

    // Exécuter la requête
    Result.Open;
  finally
    Conditions.Free;
  end;
end;

end.
```

### 3. Utilisation de la journalisation dans l'application

```delphi
// Dans le formulaire principal
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Initialiser la journalisation
  FJournal := TJournalSecurite.Create(FDConnection1);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Libérer la journalisation
  FJournal.Free;
end;

procedure TFormMain.ButtonConnexionClick(Sender: TObject);
var
  Authentication: TAuthentication;
begin
  Authentication := TAuthentication.Create(FDConnection1);
  try
    if Authentication.Authentifier(EditUtilisateur.Text, EditMotDePasse.Text) then
    begin
      // Authentification réussie
      ShowMessage('Connexion réussie!');

      // Récupérer l'ID de l'utilisateur
      var UtilisateurID := ObtenirUtilisateurID(EditUtilisateur.Text);

      // Configurer la journalisation
      FJournal.DefinirUtilisateurActif(UtilisateurID, EditUtilisateur.Text);
      FJournal.EnregistrerConnexion;

      // Initialiser les droits
      FGestionDroits := TGestionDroits.Create(FDConnection1, UtilisateurID);

      // Charger l'interface principale
      ChargerInterface;
    end
    else
    begin
      // Échec d'authentification
      ShowMessage('Nom d''utilisateur ou mot de passe incorrect.');

      // Journaliser l'échec
      FJournal.EnregistrerEchecConnexion(EditUtilisateur.Text);
    end;
  finally
    Authentication.Free;
  end;
end;

procedure TFormMain.MenuDeconnexionClick(Sender: TObject);
begin
  // Journaliser la déconnexion
  FJournal.EnregistrerDeconnexion;

  // Libérer les ressources
  FreeAndNil(FGestionDroits);

  // Revenir à l'écran de connexion
  AfficherEcranConnexion;
end;

// Exemple d'utilisation du journal lors d'une action sécurisée
procedure TFormMain.ButtonSupprimerClientClick(Sender: TObject);
begin
  if not Assigned(FGestionDroits) then
    Exit;

  if not FGestionDroits.UtilisateurAPermission('clients_delete') then
  begin
    ShowMessage('Vous n''avez pas les droits pour supprimer des clients.');
    FJournal.EnregistrerAccesRefuse('Suppression de client');
    Exit;
  end;

  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce client ?',
               mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    var ClientID := FDQueryClients.FieldByName('id').AsInteger;

    FDQueryClients.Delete;

    // Journaliser la suppression
    FJournal.EnregistrerSuppression('clients', ClientID);
  end;
end;
```

### 4. Formulaire de consultation des journaux de sécurité

```delphi
// UFormJournal.pas
unit UFormJournal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.Client,
  UJournalSecurite;

type
  TFormJournal = class(TForm)
    Panel1: TPanel;
    DateTimePickerDebut: TDateTimePicker;
    DateTimePickerFin: TDateTimePicker;
    LabelDebut: TLabel;
    LabelFin: TLabel;
    ComboBoxType: TComboBox;
    LabelType: TLabel;
    ButtonRechercher: TButton;
    ButtonExporter: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ButtonFermer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure ButtonExporterClick(Sender: TObject);
    procedure ButtonFermerClick(Sender: TObject);
  private
    FConnection: TFDConnection;
    FJournal: TJournalSecurite;
    FQuery: TFDQuery;

    procedure RemplirComboBoxType;
  public
    constructor Create(AOwner: TComponent; AConnection: TFDConnection); reintroduce;
  end;

implementation

{$R *.dfm}

constructor TFormJournal.Create(AOwner: TComponent; AConnection: TFDConnection);
begin
  inherited Create(AOwner);
  FConnection := AConnection;
  FJournal := TJournalSecurite.Create(FConnection);
end;

procedure TFormJournal.FormCreate(Sender: TObject);
begin
  // Initialiser les dates
  DateTimePickerDebut.Date := StartOfDay(IncDay(Now, -7));  // 7 jours en arrière
  DateTimePickerFin.Date := EndOfDay(Now);

  // Remplir la combobox des types d'événements
  RemplirComboBoxType;

  // Configurer le DBGrid
  DBGrid1.Options := DBGrid1.Options + [dgRowLines, dgColLines, dgRowSelect] - [dgEditing];
end;

procedure TFormJournal.FormDestroy(Sender: TObject);
begin
  FJournal.Free;

  if Assigned(FQuery) then
    FQuery.Free;
end;

procedure TFormJournal.RemplirComboBoxType;
begin
  ComboBoxType.Items.Clear;

  ComboBoxType.Items.AddObject('Tous', TObject(-1));
  ComboBoxType.Items.AddObject('Connexion', TObject(Integer(teConnexion)));
  ComboBoxType.Items.AddObject('Déconnexion', TObject(Integer(teDeconnexion)));
  ComboBoxType.Items.AddObject('Échec de connexion', TObject(Integer(teEchecConnexion)));
  ComboBoxType.Items.AddObject('Accès refusé', TObject(Integer(teAccesRefuse)));
  ComboBoxType.Items.AddObject('Modification de données', TObject(Integer(teModificationDonnees)));
  ComboBoxType.Items.AddObject('Suppression de données', TObject(Integer(teSuppressionDonnees)));

  ComboBoxType.ItemIndex := 0;  // "Tous" par défaut
end;

procedure TFormJournal.ButtonRechercherClick(Sender: TObject);
var
  TypeEvenement: TTypeEvenement;
begin
  // Libérer la requête précédente
  if Assigned(FQuery) then
    FreeAndNil(FQuery);

  // Récupérer le type d'événement sélectionné
  TypeEvenement := TTypeEvenement(Integer(ComboBoxType.Items.Objects[ComboBoxType.ItemIndex]));

  // Rechercher les événements
  FQuery := FJournal.RechercherEvenements(
    DateTimePickerDebut.DateTime,
    DateTimePickerFin.DateTime,
    TypeEvenement
  );

  // Afficher les résultats
  DataSource1.DataSet := FQuery;

  // Mettre à jour le titre du formulaire
  Caption := Format('Journal de sécurité - %d événements trouvés', [FQuery.RecordCount]);
end;

procedure TFormJournal.ButtonExporterClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  CSV: TStringList;
  i: Integer;
  Ligne: string;
begin
  if not Assigned(FQuery) or FQuery.IsEmpty then
  begin
    ShowMessage('Aucune donnée à exporter.');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter le journal de sécurité';
    SaveDialog.DefaultExt := 'csv';
    SaveDialog.Filter := 'Fichiers CSV (*.csv)|*.csv|Tous les fichiers (*.*)|*.*';
    SaveDialog.FileName := 'journal_securite_' + FormatDateTime('yyyymmdd', Now) + '.csv';

    if SaveDialog.Execute then
    begin
      CSV := TStringList.Create;
      try
        // Entête CSV
        Ligne := '';
        for i := 0 to FQuery.FieldCount - 1 do
        begin
          if i > 0 then Ligne := Ligne + ';';
          Ligne := Ligne + FQuery.Fields[i].FieldName;
        end;
        CSV.Add(Ligne);

        // Données
        FQuery.First;
        while not FQuery.Eof do
        begin
          Ligne := '';
          for i := 0 to FQuery.FieldCount - 1 do
          begin
            if i > 0 then Ligne := Ligne + ';';

            // Formatage spécial pour les dates
            if FQuery.Fields[i].DataType in [ftDate, ftDateTime, ftTimeStamp] then
              Ligne := Ligne + FormatDateTime('yyyy-mm-dd hh:nn:ss', FQuery.Fields[i].AsDateTime)
            else
              Ligne := Ligne + FQuery.Fields[i].AsString;
          end;
          CSV.Add(Ligne);

          FQuery.Next;
        end;

        // Sauvegarder
        CSV.SaveToFile(SaveDialog.FileName);
        ShowMessage('Journal exporté avec succès dans : ' + SaveDialog.FileName);
      finally
        CSV.Free;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormJournal.ButtonFermerClick(Sender: TObject);
begin
  Close;
end;

end.
```

## Bonnes pratiques de sécurité MySQL avec Delphi

Pour conclure cette section, voici un récapitulatif des bonnes pratiques de sécurité à adopter lors du développement d'applications Delphi avec MySQL :

### 1. Sécurité de la base de données

- **Utilisez des comptes avec privilèges limités** : Créez des utilisateurs spécifiques pour votre application avec uniquement les droits nécessaires.
- **Ne stockez jamais les mots de passe en clair** : Utilisez des algorithmes de hachage modernes (BCrypt, Argon2).
- **Activez SSL/TLS** pour chiffrer les communications avec la base de données.
- **Sécurisez le serveur MySQL** en suivant les recommandations de sécurité (pare-feu, désactivation des ports inutilisés, etc.).

### 2. Sécurité du code

- **Utilisez TOUJOURS des requêtes paramétrées** pour prévenir les injections SQL.
- **Validez toutes les entrées utilisateur** avant de les traiter.
- **Séparez les couches** de votre application (présentation, logique métier, accès aux données).
- **Implémentez un système de journalisation** pour détecter les tentatives d'intrusion.

### 3. Protection des informations sensibles

- **Ne stockez jamais les identifiants de connexion en dur** dans le code.
- **Chiffrez les données sensibles** stockées localement (fichiers de configuration, etc.).
- **Limitez l'affichage des erreurs techniques** aux utilisateurs finaux.
- **Utilisez un système de gestion des droits** pour contrôler l'accès aux fonctionnalités.

### 4. Sécurité de l'application

- **Implémentez un mécanisme robuste d'authentification** avec verrouillage de compte après plusieurs échecs.
- **Exigez des mots de passe forts** avec des règles de complexité.
- **Mettez en place des sessions avec expiration** pour limiter les risques en cas d'ordinateur partagé.
- **Effectuez des audits de sécurité réguliers** pour détecter les vulnérabilités.

## Exemple complet : Formulaire de connexion sécurisé

Pour illustrer plusieurs des concepts abordés, voici un exemple de formulaire de connexion sécurisé :

```delphi
// UFormConnexion.pas
unit UFormConnexion;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, FireDAC.Comp.Client, UAuthentication, UJournalSecurite;

type
  TFormConnexion = class(TForm)
    PanelTop: TPanel;
    LabelTitre: TLabel;
    ImageLogo: TImage;
    PanelCentre: TPanel;
    LabelUtilisateur: TLabel;
    EditUtilisateur: TEdit;
    LabelMotDePasse: TLabel;
    EditMotDePasse: TEdit;
    CheckBoxMemorise: TCheckBox;
    ButtonConnexion: TButton;
    LabelErreur: TLabel;
    LabelVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnexionClick(Sender: TObject);
    procedure EditMotDePasseKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FConnection: TFDConnection;
    FJournal: TJournalSecurite;
    FAuthentication: TAuthentication;
    FTentativesEchouees: Integer;
    FUtilisateurID: Integer;
    FNomUtilisateur: string;

    function ValiderChamps: Boolean;
    function ConnecterBaseDeDonnees: Boolean;
    procedure ChargerParametresConnexion;
    procedure SauvegarderUtilisateurMemorise;
    procedure ChargerUtilisateurMemorise;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property UtilisateurID: Integer read FUtilisateurID;
    property NomUtilisateur: string read FNomUtilisateur;
  end;

implementation

{$R *.dfm}

uses
  UDBConfig;

constructor TFormConnexion.Create(AOwner: TComponent);
begin
  inherited;
  FTentativesEchouees := 0;
  FUtilisateurID := 0;
  FNomUtilisateur := '';

  // Créer la connexion
  FConnection := TFDConnection.Create(Self);
  FConnection.DriverName := 'MySQL';

  // Charger les paramètres de connexion
  ChargerParametresConnexion;

  // Créer les objets de sécurité
  FJournal := TJournalSecurite.Create(FConnection);
  FAuthentication := TAuthentication.Create(FConnection);
end;

destructor TFormConnexion.Destroy;
begin
  FAuthentication.Free;
  FJournal.Free;
  inherited;
end;

procedure TFormConnexion.FormCreate(Sender: TObject);
begin
  // Initialiser l'interface
  LabelErreur.Caption := '';
  LabelVersion.Caption := 'v' + GetVersionInfo;  // Fonction qui obtient la version

  // Charger l'utilisateur mémorisé
  ChargerUtilisateurMemorise;
end;

procedure TFormConnexion.ChargerParametresConnexion;
var
  Serveur, BaseDeDonnees, Utilisateur, MotDePasse: string;
begin
  // Charger depuis la configuration
  TDBConfig.ChargerConfiguration(Serveur, BaseDeDonnees, Utilisateur, MotDePasse);

  // Si la configuration n'existe pas, utiliser des valeurs par défaut
  if (Serveur = '') or (BaseDeDonnees = '') then
  begin
    Serveur := 'localhost';
    BaseDeDonnees := 'ma_base';
    Utilisateur := 'app_user';
    MotDePasse := '';

    // Sauvegarder la configuration par défaut
    TDBConfig.SauvegarderConfiguration(Serveur, BaseDeDonnees, Utilisateur, MotDePasse);
  end;

  // Configurer la connexion
  FConnection.Params.Clear;
  FConnection.Params.Add('Server=' + Serveur);
  FConnection.Params.Add('Database=' + BaseDeDonnees);
  FConnection.Params.Add('User_Name=' + Utilisateur);
  FConnection.Params.Add('Password=' + MotDePasse);
  FConnection.Params.Add('CharacterSet=utf8mb4');
end;

function TFormConnexion.ConnecterBaseDeDonnees: Boolean;
begin
  Result := False;

  try
    // Tenter de se connecter
    FConnection.Connected := True;
    Result := FConnection.Connected;
  except
    on E: Exception do
    begin
      LabelErreur.Caption := 'Erreur de connexion à la base de données: ' + E.Message;
    end;
  end;
end;

function TFormConnexion.ValiderChamps: Boolean;
const
  MIN_LENGTH = 3;
begin
  Result := False;
  LabelErreur.Caption := '';

  // Vérifier le nom d'utilisateur
  if Length(Trim(EditUtilisateur.Text)) < MIN_LENGTH then
  begin
    LabelErreur.Caption := 'Le nom d''utilisateur doit contenir au moins ' +
                          IntToStr(MIN_LENGTH) + ' caractères.';
    EditUtilisateur.SetFocus;
    Exit;
  end;

  // Vérifier le mot de passe
  if Length(EditMotDePasse.Text) < MIN_LENGTH then
  begin
    LabelErreur.Caption := 'Le mot de passe doit contenir au moins ' +
                          IntToStr(MIN_LENGTH) + ' caractères.';
    EditMotDePasse.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TFormConnexion.ButtonConnexionClick(Sender: TObject);
begin
  // Valider les champs
  if not ValiderChamps then
    Exit;

  // Se connecter à la base de données si ce n'est pas déjà fait
  if not FConnection.Connected then
    if not ConnecterBaseDeDonnees then
      Exit;

  // Tenter l'authentification
  if FAuthentication.Authentifier(EditUtilisateur.Text, EditMotDePasse.Text) then
  begin
    // Authentification réussie
    FTentativesEchouees := 0;
    LabelErreur.Caption := '';

    // Récupérer l'ID de l'utilisateur
    var Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := 'SELECT id FROM utilisateurs WHERE nom_utilisateur = :nom';
      Query.ParamByName('nom').AsString := EditUtilisateur.Text;
      Query.Open;

      if not Query.IsEmpty then
        FUtilisateurID := Query.FieldByName('id').AsInteger;
    finally
      Query.Free;
    end;

    // Mémoriser l'utilisateur si demandé
    if CheckBoxMemorise.Checked then
      SauvegarderUtilisateurMemorise;

    // Journaliser la connexion
    FNomUtilisateur := EditUtilisateur.Text;
    FJournal.DefinirUtilisateurActif(FUtilisateurID, FNomUtilisateur);
    FJournal.EnregistrerConnexion;

    // Fermer la boîte de dialogue avec succès
    ModalResult := mrOk;
  end
  else
  begin
    // Échec d'authentification
    Inc(FTentativesEchouees);

    // Journaliser l'échec
    FJournal.EnregistrerEchecConnexion(EditUtilisateur.Text);

    // Afficher un message d'erreur
    if FTentativesEchouees >= 5 then
      LabelErreur.Caption := 'Trop de tentatives échouées. Veuillez réessayer plus tard.'
    else
      LabelErreur.Caption := 'Nom d''utilisateur ou mot de passe incorrect.';

    // Effacer le mot de passe
    EditMotDePasse.Text := '';
    EditMotDePasse.SetFocus;
  end;
end;

procedure TFormConnexion.EditMotDePasseKeyPress(Sender: TObject; var Key: Char);
begin
  // Appuyer sur Entrée équivaut à cliquer sur le bouton Connexion
  if Key = #13 then
  begin
    Key := #0;  // Supprimer le bip sonore
    ButtonConnexionClick(Sender);
  end;
end;

procedure TFormConnexion.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Empêcher la fermeture si trop de tentatives échouées (protection contre les attaques par force brute)
  if (FTentativesEchouees >= 5) and (ModalResult <> mrOk) then
  begin
    // Attendre quelques secondes avant de permettre une nouvelle tentative
    Sleep(3000);
    FTentativesEchouees := 0;
    LabelErreur.Caption := 'Veuillez réessayer.';
    CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TFormConnexion.SauvegarderUtilisateurMemorise;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey('Software\VotreApplication\Auth', True) then
    begin
      Registry.WriteString('LastUser', EditUtilisateur.Text);
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TFormConnexion.ChargerUtilisateurMemorise;
var
  Registry: TRegistry;
  LastUser: string;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKey('Software\VotreApplication\Auth', False) then
    begin
      if Registry.ValueExists('LastUser') then
      begin
        LastUser := Registry.ReadString('LastUser');
        EditUtilisateur.Text := LastUser;
        CheckBoxMemorise.Checked := True;
      end;

      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

end.
```

## Prévention des attaques de type SQL Injection

Les injections SQL restent l'une des vulnérabilités les plus courantes et dangereuses dans les applications qui manipulent des bases de données. Voyons quelques exemples concrets d'attaques et comment les prévenir.

### Exemples d'attaques d'injection SQL

#### 1. Injection dans une recherche

**Code vulnérable :**
```delphi
// Code NON SÉCURISÉ
procedure TFormClient.RechercherClient(const Terme: string);
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE ' +
                       'nom LIKE ''%' + Terme + '%'' OR ' +
                       'prenom LIKE ''%' + Terme + '%'' OR ' +
                       'email LIKE ''%' + Terme + '%''';
  FDQuery1.Open;
end;
```

Si un attaquant entre `'; DROP TABLE clients; --` dans le champ de recherche, la requête devient :
```sql
SELECT * FROM clients WHERE nom LIKE '%'; DROP TABLE clients; --%' OR prenom LIKE '%...' OR email LIKE '%...'
```

Cela supprimera la table clients !

**Solution correcte :**
```delphi
// Code SÉCURISÉ
procedure TFormClient.RechercherClient(const Terme: string);
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE ' +
                       'nom LIKE :terme OR ' +
                       'prenom LIKE :terme OR ' +
                       'email LIKE :terme';
  FDQuery1.ParamByName('terme').AsString := '%' + Terme + '%';
  FDQuery1.Open;
end;
```

#### 2. Injection dans un filtre par ID

**Code vulnérable :**
```delphi
// Code NON SÉCURISÉ
procedure TFormProduit.AfficherProduit(const ID: string);
begin
  FDQuery1.SQL.Text := 'SELECT * FROM produits WHERE id = ' + ID;
  FDQuery1.Open;
end;
```

Si un attaquant entre `1 OR 1=1` comme ID, la requête devient :
```sql
SELECT * FROM produits WHERE id = 1 OR 1=1
```

Cela affichera tous les produits, même ceux qui devraient être inaccessibles.

**Solution correcte :**
```delphi
// Code SÉCURISÉ
procedure TFormProduit.AfficherProduit(const ID: string);
begin
  FDQuery1.SQL.Text := 'SELECT * FROM produits WHERE id = :id';
  FDQuery1.ParamByName('id').AsString := ID;
  FDQuery1.Open;
end;
```

#### 3. Injection dans une fonction d'authentification

**Code vulnérable :**
```delphi
// Code NON SÉCURISÉ
function TAuthManager.Authentifier(const Utilisateur, MotDePasse: string): Boolean;
begin
  FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM utilisateurs WHERE ' +
                       'nom_utilisateur = ''' + Utilisateur + ''' AND ' +
                       'mot_de_passe = ''' + MotDePasse + '''';
  FDQuery1.Open;
  Result := FDQuery1.Fields[0].AsInteger > 0;
end;
```

Un attaquant pourrait entrer `admin' --` comme nom d'utilisateur et n'importe quoi comme mot de passe. La requête deviendrait :
```sql
SELECT COUNT(*) FROM utilisateurs WHERE nom_utilisateur = 'admin' -- ' AND mot_de_passe = '...'
```

Le `--` commente le reste de la requête, permettant l'accès sans connaître le mot de passe.

**Solution correcte :**
```delphi
// Code SÉCURISÉ
function TAuthManager.Authentifier(const Utilisateur, MotDePasse: string): Boolean;
begin
  FDQuery1.SQL.Text := 'SELECT mot_de_passe_hache FROM utilisateurs WHERE nom_utilisateur = :user';
  FDQuery1.ParamByName('user').AsString := Utilisateur;
  FDQuery1.Open;

  if FDQuery1.IsEmpty then
    Result := False
  else
    // Vérifier le hash du mot de passe avec une fonction de hachage sécurisée
    Result := VerifierMotDePasse(MotDePasse, FDQuery1.FieldByName('mot_de_passe_hache').AsString);
end;
```

### Règles d'or pour prévenir les injections SQL

1. **TOUJOURS utiliser des requêtes paramétrées** - C'est la règle la plus importante.
2. **Valider toutes les entrées utilisateur** - Assurez-vous que les entrées correspondent aux types attendus.
3. **Utiliser des procédures stockées** quand c'est possible - Elles offrent une couche supplémentaire de protection.
4. **Appliquer le principe du moindre privilège** - Limitez les droits de l'utilisateur de la base de données.
5. **Échapper les caractères spéciaux** UNIQUEMENT si les requêtes paramétrées ne sont pas possibles.
6. **Ne jamais concaténer directement des entrées utilisateur** dans des chaînes SQL.
7. **Gérer correctement les erreurs** - Ne pas exposer les détails techniques aux utilisateurs.

## Protection contre d'autres types d'attaques

Outre les injections SQL, votre application peut être vulnérable à d'autres types d'attaques.

### 1. Cross-Site Scripting (XSS)

Bien que moins courant dans les applications Delphi traditionnelles, ce risque existe si vous affichez des données provenant de la base de données dans un navigateur web intégré.

**Solution :**
```delphi
// Échapper les balises HTML avant affichage
function EchapperHTML(const HTML: string): string;
begin
  Result := StringReplace(HTML, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  // Échapper d'autres caractères si nécessaire
end;

// Utilisation
WebBrowser1.NavigateToString(EchapperHTML(FDQuery1.FieldByName('commentaire').AsString));
```

### 2. Attaque par dictionnaire / force brute

Les attaquants peuvent tenter de deviner les mots de passe par essais répétés.

**Solutions :**
1. Limiter le nombre de tentatives de connexion (comme dans notre exemple).
2. Implémenter un délai croissant après chaque échec.
3. Utiliser un CAPTCHA après plusieurs échecs.
4. Journaliser les tentatives suspectes.

### 3. Élévation de privilèges

Un utilisateur pourrait tenter d'accéder à des fonctionnalités auxquelles il n'a pas droit.

**Solution :**
```delphi
// Vérifier les droits avant chaque action sensible
procedure TFormAdmin.ButtonSupprimerUtilisateurClick(Sender: TObject);
begin
  if not GestionDroits.UtilisateurAPermission('utilisateurs_delete') then
  begin
    Journal.EnregistrerAccesRefuse('Suppression d''utilisateur');
    ShowMessage('Accès refusé. Cet incident a été journalisé.');
    Exit;
  end;

  // Suite du code si l'utilisateur a les droits
end;
```

### 4. Attaques Man-in-the-Middle (MitM)

Un attaquant pourrait intercepter les communications entre votre application et la base de données.

**Solutions :**
1. Utiliser SSL/TLS pour chiffrer toutes les communications.
2. Vérifier les certificats SSL pour éviter les attaques par usurpation.
3. Ne jamais se connecter à la base de données sur des réseaux non sécurisés.

## Audit et amélioration continue de la sécurité

La sécurité est un processus continu, pas un état final. Voici quelques pratiques à adopter :

### 1. Audit de code régulier

Examinez régulièrement votre code pour détecter les vulnérabilités potentielles.

### 2. Tests de pénétration

Engagez des professionnels pour tester la sécurité de votre application.

### 3. Surveillance active

Analysez les journaux de sécurité régulièrement pour détecter les comportements suspects.

### 4. Mises à jour régulières

Gardez à jour votre environnement Delphi, MySQL et toutes les bibliothèques tierces.

### 5. Formation continue

Restez informé des nouvelles vulnérabilités et techniques d'attaque.

## Conclusion

La sécurisation des accès à la base de données et la prévention des injections SQL sont des aspects fondamentaux du développement d'applications professionnelles avec Delphi et MySQL. En suivant les bonnes pratiques présentées dans cette section, vous protégerez efficacement vos applications et les données de vos utilisateurs.

Points clés à retenir :
- Utilisez toujours des requêtes paramétrées pour prévenir les injections SQL
- Implémentez un système robuste d'authentification avec hachage des mots de passe
- Gérez correctement les droits des utilisateurs dans votre application
- Journalisez les événements de sécurité pour détecter les tentatives d'intrusion
- Suivez le principe du moindre privilège pour limiter les risques
- Chiffrez les informations sensibles, tant au repos qu'en transit

En investissant du temps dans la sécurité dès le début de votre projet, vous éviterez des problèmes potentiellement graves par la suite et gagnerez la confiance de vos utilisateurs.

---

**À suivre :** 8.12 Autres moteurs de bases de données (SQLite, PostgreSQL, SQL Server)
