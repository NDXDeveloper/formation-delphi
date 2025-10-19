🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.2 Autorisation et contrôle d'accès

## Introduction

Une fois qu'un utilisateur est authentifié (nous savons **qui** il est), nous devons déterminer **ce qu'il peut faire** dans l'application. C'est le rôle de l'autorisation et du contrôle d'accès.

### Différence entre authentification et autorisation

| Authentification | Autorisation |
|-----------------|--------------|
| Qui êtes-vous ? | Que pouvez-vous faire ? |
| Vérification d'identité | Vérification des permissions |
| Se produit en premier | Se produit après l'authentification |
| Exemple : login/mot de passe | Exemple : accès à une fonctionnalité |

**Analogie du monde réel** :
- **Authentification** : Montrer votre badge à l'entrée d'un bâtiment pour prouver qui vous êtes
- **Autorisation** : Votre badge détermine à quels étages et bureaux vous avez accès

## Concepts fondamentaux

### 1. Les rôles

Un rôle est un ensemble de permissions regroupées logiquement qui correspondent à une fonction dans l'organisation.

**Exemples de rôles courants** :
- **Administrateur** : accès complet à toutes les fonctionnalités
- **Gestionnaire** : peut créer, modifier, supprimer des données dans son périmètre
- **Utilisateur standard** : peut consulter et modifier ses propres données
- **Invité** : accès en lecture seule

```
Administrateur
    ├── Gestion des utilisateurs
    ├── Configuration système
    ├── Accès aux rapports
    └── Toutes les autres permissions

Gestionnaire
    ├── Création de documents
    ├── Modification de documents
    └── Accès aux rapports de son service

Utilisateur
    ├── Consultation de documents
    └── Modification de ses propres données
```

### 2. Les permissions

Une permission est une action spécifique qu'un utilisateur peut effectuer.

**Types de permissions courantes** :
- **Lecture** (Read) : consulter des informations
- **Création** (Create) : ajouter de nouvelles données
- **Modification** (Update) : changer des données existantes
- **Suppression** (Delete) : effacer des données

On parle souvent de **CRUD** : Create, Read, Update, Delete

### 3. Les ressources

Une ressource est un élément de votre application sur lequel s'appliquent les permissions.

**Exemples de ressources** :
- Un formulaire
- Un rapport
- Un menu
- Une table de base de données
- Un bouton
- Un document

## Modèles de contrôle d'accès

### 1. RBAC (Role-Based Access Control)

Le modèle le plus courant et le plus simple à implémenter. Les permissions sont attribuées aux rôles, et les utilisateurs se voient attribuer des rôles.

**Schéma** :
```
Utilisateur → Rôle → Permissions → Ressources
```

**Avantages** :
- Simple à comprendre et à gérer
- Facilite l'administration (on modifie le rôle, pas chaque utilisateur)
- Correspond bien aux structures organisationnelles

**Exemple** :
```
Jean (Utilisateur) → Gestionnaire (Rôle) → [Créer, Modifier, Lire] → Documents
Marie (Utilisateur) → Administrateur (Rôle) → [Toutes permissions] → Tout
```

### 2. ACL (Access Control List)

Permissions définies directement pour chaque utilisateur sur chaque ressource.

**Avantages** :
- Contrôle très précis
- Flexibilité maximale

**Inconvénients** :
- Complexe à gérer avec beaucoup d'utilisateurs
- Difficile à maintenir

### 3. ABAC (Attribute-Based Access Control)

Permissions basées sur des attributs (département, localisation, heure, etc.)

**Exemple** :
```
SI (Département = "Ventes" ET Heure < 18h00 ET Localisation = "Bureau")
ALORS Autoriser accès CRM
```

Plus complexe mais très puissant pour des besoins spécifiques.

## Structure de base de données pour l'autorisation

### Modèle RBAC simple

Voici une structure de base de données pour implémenter le RBAC :

```sql
-- Table des rôles
CREATE TABLE Roles (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    NomRole VARCHAR(50) UNIQUE NOT NULL,
    Description VARCHAR(255),
    Actif BOOLEAN DEFAULT TRUE
);

-- Table des permissions
CREATE TABLE Permissions (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    NomPermission VARCHAR(50) UNIQUE NOT NULL,
    Description VARCHAR(255),
    CodePermission VARCHAR(50) UNIQUE NOT NULL
);

-- Table d'association rôles-permissions
CREATE TABLE RolesPermissions (
    IDRole INT NOT NULL,
    IDPermission INT NOT NULL,
    PRIMARY KEY (IDRole, IDPermission),
    FOREIGN KEY (IDRole) REFERENCES Roles(ID),
    FOREIGN KEY (IDPermission) REFERENCES Permissions(ID)
);

-- Table d'association utilisateurs-rôles
CREATE TABLE UtilisateursRoles (
    IDUtilisateur INT NOT NULL,
    IDRole INT NOT NULL,
    DateAttribution DATETIME DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (IDUtilisateur, IDRole),
    FOREIGN KEY (IDUtilisateur) REFERENCES Utilisateurs(ID),
    FOREIGN KEY (IDRole) REFERENCES Roles(ID)
);
```

### Exemple de données initiales

```sql
-- Insertion des rôles
INSERT INTO Roles (NomRole, Description) VALUES
('Administrateur', 'Accès complet au système'),
('Gestionnaire', 'Gestion des données et des utilisateurs de son service'),
('Utilisateur', 'Accès standard aux fonctionnalités'),
('Invité', 'Accès en lecture seule');

-- Insertion des permissions
INSERT INTO Permissions (NomPermission, Description, CodePermission) VALUES
('Lire utilisateurs', 'Consulter la liste des utilisateurs', 'USER_READ'),
('Créer utilisateur', 'Ajouter un nouvel utilisateur', 'USER_CREATE'),
('Modifier utilisateur', 'Modifier les informations utilisateur', 'USER_UPDATE'),
('Supprimer utilisateur', 'Supprimer un utilisateur', 'USER_DELETE'),
('Lire documents', 'Consulter les documents', 'DOC_READ'),
('Créer document', 'Créer un nouveau document', 'DOC_CREATE'),
('Modifier document', 'Modifier un document existant', 'DOC_UPDATE'),
('Supprimer document', 'Supprimer un document', 'DOC_DELETE'),
('Accès configuration', 'Accéder aux paramètres système', 'CONFIG_ACCESS');

-- Attribution des permissions aux rôles
-- Administrateur : toutes les permissions
INSERT INTO RolesPermissions (IDRole, IDPermission)
SELECT 1, ID FROM Permissions;

-- Gestionnaire : gestion documents + lecture utilisateurs
INSERT INTO RolesPermissions (IDRole, IDPermission) VALUES
(2, (SELECT ID FROM Permissions WHERE CodePermission = 'USER_READ')),
(2, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_READ')),
(2, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_CREATE')),
(2, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_UPDATE')),
(2, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_DELETE'));

-- Utilisateur : lecture et modification documents
INSERT INTO RolesPermissions (IDRole, IDPermission) VALUES
(3, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_READ')),
(3, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_UPDATE'));

-- Invité : lecture seule
INSERT INTO RolesPermissions (IDRole, IDPermission) VALUES
(4, (SELECT ID FROM Permissions WHERE CodePermission = 'DOC_READ'));
```

## Implémentation dans Delphi

### 1. Classe de gestion des permissions

Créons une classe pour gérer facilement les permissions dans notre application :

```pascal
unit UnitGestionPermissions;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, FireDAC.Comp.Client;

type
  TGestionPermissions = class
  private
    FIDUtilisateur: Integer;
    FPermissions: TList<string>;
    FRoles: TList<string>;
    FConnection: TFDConnection;
    procedure ChargerPermissions;
    procedure ChargerRoles;
  public
    constructor Create(AConnection: TFDConnection; AIDUtilisateur: Integer);
    destructor Destroy; override;

    function APermission(const ACodePermission: string): Boolean;
    function ARoles(const ANomRole: string): Boolean;
    procedure Rafraichir;

    property IDUtilisateur: Integer read FIDUtilisateur;
    property Permissions: TList<string> read FPermissions;
    property Roles: TList<string> read FRoles;
  end;

implementation

{ TGestionPermissions }

constructor TGestionPermissions.Create(AConnection: TFDConnection; AIDUtilisateur: Integer);
begin
  inherited Create;
  FConnection := AConnection;
  FIDUtilisateur := AIDUtilisateur;
  FPermissions := TList<string>.Create;
  FRoles := TList<string>.Create;
  ChargerPermissions;
  ChargerRoles;
end;

destructor TGestionPermissions.Destroy;
begin
  FPermissions.Free;
  FRoles.Free;
  inherited;
end;

procedure TGestionPermissions.ChargerPermissions;
var
  Query: TFDQuery;
begin
  FPermissions.Clear;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT DISTINCT p.CodePermission ' +
      'FROM Permissions p ' +
      'INNER JOIN RolesPermissions rp ON p.ID = rp.IDPermission ' +
      'INNER JOIN UtilisateursRoles ur ON rp.IDRole = ur.IDRole ' +
      'WHERE ur.IDUtilisateur = :IDUser';
    Query.ParamByName('IDUser').AsInteger := FIDUtilisateur;
    Query.Open;

    while not Query.Eof do
    begin
      FPermissions.Add(Query.FieldByName('CodePermission').AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TGestionPermissions.ChargerRoles;
var
  Query: TFDQuery;
begin
  FRoles.Clear;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT r.NomRole ' +
      'FROM Roles r ' +
      'INNER JOIN UtilisateursRoles ur ON r.ID = ur.IDRole ' +
      'WHERE ur.IDUtilisateur = :IDUser AND r.Actif = TRUE';
    Query.ParamByName('IDUser').AsInteger := FIDUtilisateur;
    Query.Open;

    while not Query.Eof do
    begin
      FRoles.Add(Query.FieldByName('NomRole').AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TGestionPermissions.APermission(const ACodePermission: string): Boolean;
begin
  Result := FPermissions.Contains(ACodePermission);
end;

function TGestionPermissions.ARoles(const ANomRole: string): Boolean;
begin
  Result := FRoles.Contains(ANomRole);
end;

procedure TGestionPermissions.Rafraichir;
begin
  ChargerPermissions;
  ChargerRoles;
end;

end.
```

### 2. Variable globale pour les permissions

Dans votre unité principale, déclarez une variable globale :

```pascal
var
  GestionPermissions: TGestionPermissions;

procedure InitialiserPermissions(AConnection: TFDConnection; AIDUtilisateur: Integer);
begin
  if Assigned(GestionPermissions) then
    GestionPermissions.Free;
  GestionPermissions := TGestionPermissions.Create(AConnection, AIDUtilisateur);
end;

procedure LibererPermissions;
begin
  if Assigned(GestionPermissions) then
  begin
    GestionPermissions.Free;
    GestionPermissions := nil;
  end;
end;
```

### 3. Contrôler l'accès aux fonctionnalités

#### a) Activer/désactiver des boutons

```pascal
procedure TFormPrincipal.FormShow(Sender: TObject);
begin
  // Contrôle des boutons selon les permissions
  BtnCreerUtilisateur.Enabled := GestionPermissions.APermission('USER_CREATE');
  BtnModifierUtilisateur.Enabled := GestionPermissions.APermission('USER_UPDATE');
  BtnSupprimerUtilisateur.Enabled := GestionPermissions.APermission('USER_DELETE');

  // Contrôle des menus
  MenuConfiguration.Visible := GestionPermissions.APermission('CONFIG_ACCESS');

  // Contrôle par rôle
  if GestionPermissions.ARoles('Administrateur') then
  begin
    PanelAdmin.Visible := True;
    // Afficher toutes les options avancées
  end;
end;
```

#### b) Vérifier avant une action

```pascal
procedure TFormDocuments.BtnSupprimerClick(Sender: TObject);
begin
  // Vérifier la permission avant de supprimer
  if not GestionPermissions.APermission('DOC_DELETE') then
  begin
    ShowMessage('Vous n''avez pas la permission de supprimer des documents.');
    Exit;
  end;

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce document ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Effectuer la suppression
    SupprimerDocument(IDDocumentSelectionne);
  end;
end;
```

#### c) Contrôler l'accès à un formulaire entier

```pascal
procedure TFormPrincipal.MenuGestionUtilisateursClick(Sender: TObject);
begin
  if not GestionPermissions.APermission('USER_READ') then
  begin
    ShowMessage('Accès refusé. Vous n''avez pas les droits nécessaires.');
    Exit;
  end;

  // Ouvrir le formulaire
  FormGestionUtilisateurs := TFormGestionUtilisateurs.Create(Self);
  try
    FormGestionUtilisateurs.ShowModal;
  finally
    FormGestionUtilisateurs.Free;
  end;
end;
```

### 4. Masquer des éléments selon les permissions

Pour une meilleure expérience utilisateur, masquez les éléments inaccessibles plutôt que de les désactiver :

```pascal
procedure TFormPrincipal.AppliquerPermissionsInterface;
begin
  // Masquer complètement les options non autorisées
  BtnCreerUtilisateur.Visible := GestionPermissions.APermission('USER_CREATE');
  BtnModifierUtilisateur.Visible := GestionPermissions.APermission('USER_UPDATE');
  BtnSupprimerUtilisateur.Visible := GestionPermissions.APermission('USER_DELETE');

  // Masquer les onglets inaccessibles
  TabSheetConfiguration.TabVisible := GestionPermissions.APermission('CONFIG_ACCESS');

  // Adapter le menu selon le rôle
  if GestionPermissions.ARoles('Invité') then
  begin
    // Mode lecture seule pour les invités
    DBGrid1.ReadOnly := True;
    PanelActions.Visible := False;
  end;
end;
```

## Permissions granulaires

Pour un contrôle plus fin, vous pouvez implémenter des permissions au niveau des enregistrements :

### Propriété des données

```sql
-- Ajouter une colonne propriétaire
ALTER TABLE Documents ADD COLUMN IDProprietaire INT;
ALTER TABLE Documents ADD FOREIGN KEY (IDProprietaire) REFERENCES Utilisateurs(ID);
```

### Vérification de propriété

```pascal
function TDocumentManager.PeutModifierDocument(AIDDocument, AIDUtilisateur: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  // Les administrateurs peuvent tout modifier
  if GestionPermissions.ARoles('Administrateur') then
  begin
    Result := True;
    Exit;
  end;

  // Vérifier la propriété du document
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT IDProprietaire FROM Documents WHERE ID = :IDDoc';
    Query.ParamByName('IDDoc').AsInteger := AIDDocument;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // L'utilisateur peut modifier s'il est propriétaire ET a la permission
      Result := (Query.FieldByName('IDProprietaire').AsInteger = AIDUtilisateur) and
                GestionPermissions.APermission('DOC_UPDATE');
    end;
  finally
    Query.Free;
  end;
end;
```

## Gestion des rôles multiples

Un utilisateur peut avoir plusieurs rôles simultanément :

```pascal
procedure AttribuerRole(AIDUtilisateur, AIDRole: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Vérifier si l'association existe déjà
    Query.SQL.Text := 'SELECT COUNT(*) as Compte FROM UtilisateursRoles ' +
                      'WHERE IDUtilisateur = :IDUser AND IDRole = :IDRole';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('IDRole').AsInteger := AIDRole;
    Query.Open;

    if Query.FieldByName('Compte').AsInteger = 0 then
    begin
      // Créer l'association
      Query.Close;
      Query.SQL.Text := 'INSERT INTO UtilisateursRoles (IDUtilisateur, IDRole) ' +
                        'VALUES (:IDUser, :IDRole)';
      Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
      Query.ParamByName('IDRole').AsInteger := AIDRole;
      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

procedure RetirerRole(AIDUtilisateur, AIDRole: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'DELETE FROM UtilisateursRoles ' +
                      'WHERE IDUtilisateur = :IDUser AND IDRole = :IDRole';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('IDRole').AsInteger := AIDRole;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

## Interface d'administration des rôles et permissions

### Formulaire de gestion des rôles

```pascal
procedure TFormGestionRoles.ChargerRoles;
begin
  FDQueryRoles.SQL.Text := 'SELECT * FROM Roles ORDER BY NomRole';
  FDQueryRoles.Open;
end;

procedure TFormGestionRoles.ChargerPermissionsRole(AIDRole: Integer);
begin
  FDQueryPermissions.SQL.Text :=
    'SELECT p.ID, p.NomPermission, p.Description, ' +
    '       IF(rp.IDRole IS NULL, 0, 1) as Attribuee ' +
    'FROM Permissions p ' +
    'LEFT JOIN RolesPermissions rp ON p.ID = rp.IDPermission AND rp.IDRole = :IDRole ' +
    'ORDER BY p.NomPermission';
  FDQueryPermissions.ParamByName('IDRole').AsInteger := AIDRole;
  FDQueryPermissions.Open;
end;

procedure TFormGestionRoles.AttribuerOuRetirerPermission(AIDRole, AIDPermission: Integer; AAttribuer: Boolean);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    if AAttribuer then
    begin
      Query.SQL.Text := 'INSERT INTO RolesPermissions (IDRole, IDPermission) ' +
                        'VALUES (:IDRole, :IDPerm)';
    end
    else
    begin
      Query.SQL.Text := 'DELETE FROM RolesPermissions ' +
                        'WHERE IDRole = :IDRole AND IDPermission = :IDPerm';
    end;

    Query.ParamByName('IDRole').AsInteger := AIDRole;
    Query.ParamByName('IDPerm').AsInteger := AIDPermission;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

## Permissions temporaires et expiration

Pour des accès temporaires :

```sql
ALTER TABLE UtilisateursRoles ADD COLUMN DateDebut DATETIME;
ALTER TABLE UtilisateursRoles ADD COLUMN DateFin DATETIME;
```

```pascal
function RoleActif(AIDUtilisateur, AIDRole: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'SELECT COUNT(*) as Compte FROM UtilisateursRoles ' +
      'WHERE IDUtilisateur = :IDUser AND IDRole = :IDRole ' +
      'AND (DateDebut IS NULL OR DateDebut <= NOW()) ' +
      'AND (DateFin IS NULL OR DateFin >= NOW())';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('IDRole').AsInteger := AIDRole;
    Query.Open;

    Result := Query.FieldByName('Compte').AsInteger > 0;
  finally
    Query.Free;
  end;
end;
```

## Hiérarchie des rôles

Pour des organisations complexes avec héritage de rôles :

```sql
CREATE TABLE HierarchieRoles (
    IDRoleParent INT NOT NULL,
    IDRoleEnfant INT NOT NULL,
    PRIMARY KEY (IDRoleParent, IDRoleEnfant),
    FOREIGN KEY (IDRoleParent) REFERENCES Roles(ID),
    FOREIGN KEY (IDRoleEnfant) REFERENCES Roles(ID)
);
```

**Exemple** :
```
Administrateur (hérite de tout)
    ├── Gestionnaire (hérite de Utilisateur)
    │       └── Utilisateur
    └── Support (hérite de Utilisateur)
            └── Utilisateur
```

## Journalisation des accès

Il est important de tracer qui accède à quoi et quand :

```sql
CREATE TABLE JournalAcces (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    IDUtilisateur INT NOT NULL,
    Action VARCHAR(100) NOT NULL,
    Ressource VARCHAR(100),
    DateHeure DATETIME DEFAULT CURRENT_TIMESTAMP,
    AdresseIP VARCHAR(45),
    Resultat ENUM('Autorisé', 'Refusé') NOT NULL,
    FOREIGN KEY (IDUtilisateur) REFERENCES Utilisateurs(ID)
);
```

```pascal
procedure JournaliserAcces(AIDUtilisateur: Integer; const AAction, ARessource: string; AAutorise: Boolean);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO JournalAcces (IDUtilisateur, Action, Ressource, Resultat) ' +
      'VALUES (:IDUser, :Action, :Ressource, :Resultat)';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('Action').AsString := AAction;
    Query.ParamByName('Ressource').AsString := ARessource;
    if AAutorise then
      Query.ParamByName('Resultat').AsString := 'Autorisé'
    else
      Query.ParamByName('Resultat').AsString := 'Refusé';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Principe du moindre privilège

Accordez uniquement les permissions nécessaires pour accomplir une tâche :

```pascal
// ❌ MAUVAIS - donner trop de droits
AttribuerRole(IDUtilisateur, RoleAdministrateur);

// ✅ BON - donner juste ce qui est nécessaire
AttribuerRole(IDUtilisateur, RoleUtilisateur);
```

### 2. Permissions par défaut sécurisées

Par défaut, tout devrait être interdit :

```pascal
function TGestionPermissions.APermission(const ACodePermission: string): Boolean;
begin
  // Par défaut : refusé
  Result := False;

  // Vérifier si l'utilisateur est actif
  if not UtilisateurActif(FIDUtilisateur) then
    Exit;

  // Vérifier la permission
  Result := FPermissions.Contains(ACodePermission);
end;
```

### 3. Séparation des préoccupations

Séparez la logique métier de la logique d'autorisation :

```pascal
// Couche métier
procedure SupprimerDocument(AIDDocument: Integer);
begin
  // Logique de suppression uniquement
  ExecuterRequeteSuppressionDocument(AIDDocument);
end;

// Couche présentation avec contrôle d'accès
procedure TFormDocuments.BtnSupprimerClick(Sender: TObject);
begin
  // Vérifier l'autorisation
  if not GestionPermissions.APermission('DOC_DELETE') then
  begin
    ShowMessage('Accès refusé');
    Exit;
  end;

  // Appeler la logique métier
  SupprimerDocument(IDDocumentSelectionne);
end;
```

### 4. Éviter le hardcoding des rôles

```pascal
// ❌ MAUVAIS
if NomRole = 'Administrateur' then
  ShowMessage('Bienvenue Admin');

// ✅ BON - utiliser des constantes
const
  ROLE_ADMIN = 'Administrateur';
  ROLE_GESTIONNAIRE = 'Gestionnaire';
  ROLE_UTILISATEUR = 'Utilisateur';

if GestionPermissions.ARoles(ROLE_ADMIN) then
  ShowMessage('Bienvenue Admin');
```

### 5. Tester les autorisations côté serveur

Dans les applications client-serveur, vérifiez TOUJOURS les permissions côté serveur :

```pascal
// Sur le client (interface)
if not GestionPermissions.APermission('DOC_DELETE') then
begin
  BtnSupprimer.Enabled := False; // Interface seulement
  Exit;
end;

// Sur le serveur (API/Service)
function ServiceDocuments.SupprimerDocument(AIDUtilisateur, AIDDocument: Integer): Boolean;
begin
  // Vérification côté serveur obligatoire
  if not VerifierPermissionServeur(AIDUtilisateur, 'DOC_DELETE') then
    raise Exception.Create('Accès refusé');

  // Effectuer la suppression
  Result := ExecuterSuppression(AIDDocument);
end;
```

## Gestion des permissions dans les applications mobiles

Pour FireMonkey (iOS/Android) :

```pascal
procedure TFormMobile.VerifierPermissionsEtAfficher;
begin
  // Charger les permissions depuis le serveur
  ChargerPermissionsDepuisServeur;

  // Adapter l'interface
  if GestionPermissions.APermission('DOC_CREATE') then
    ListViewActions.Items[0].Visible := True
  else
    ListViewActions.Items[0].Visible := False;

  // Mode hors ligne : utiliser les permissions en cache
  if not ConnecteAuServeur then
  begin
    // Charger depuis le stockage local
    ChargerPermissionsLocales;
  end;
end;
```

## Cas d'usage avancés

### Délégation de permissions

Permettre à un utilisateur de déléguer temporairement ses permissions :

```sql
CREATE TABLE DelegationsPermissions (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    IDUtilisateurDelegant INT NOT NULL,
    IDUtilisateurDelegate INT NOT NULL,
    IDPermission INT NOT NULL,
    DateDebut DATETIME NOT NULL,
    DateFin DATETIME NOT NULL,
    Actif BOOLEAN DEFAULT TRUE,
    FOREIGN KEY (IDUtilisateurDelegant) REFERENCES Utilisateurs(ID),
    FOREIGN KEY (IDUtilisateurDelegate) REFERENCES Utilisateurs(ID),
    FOREIGN KEY (IDPermission) REFERENCES Permissions(ID)
);
```

### Permissions contextuelles

Permissions qui dépendent du contexte (heure, lieu, etc.) :

```pascal
function PermissionContextuelle(const ACodePermission: string): Boolean;
var
  HeureActuelle: TTime;
begin
  Result := GestionPermissions.APermission(ACodePermission);

  if Result and (ACodePermission = 'ACCES_FINANCE') then
  begin
    HeureActuelle := Time;
    // Accès aux données financières uniquement pendant les heures de bureau
    Result := (HeureActuelle >= EncodeTime(8, 0, 0, 0)) and
              (HeureActuelle <= EncodeTime(18, 0, 0, 0));
  end;
end;
```

## Résumé des points essentiels

✅ **Bonnes pratiques** :
- Utiliser le modèle RBAC pour la simplicité
- Implémenter le principe du moindre privilège
- Vérifier les permissions côté serveur ET client
- Journaliser tous les accès importants
- Séparer authentification et autorisation
- Utiliser des constantes pour les codes de permission

❌ **Erreurs à éviter** :
- Hardcoder les rôles et permissions dans le code
- Se fier uniquement aux contrôles côté client
- Donner trop de permissions par défaut
- Négliger la journalisation des accès
- Oublier de rafraîchir les permissions après modification

## Aller plus loin

Dans les prochaines sections, nous aborderons :
- **16.3 Chiffrement des données** : protéger les informations sensibles
- **16.4 Sécurisation des connexions** : HTTPS, TLS, certificats
- **16.5 Protection contre les vulnérabilités** : injection SQL, XSS, CSRF

L'autorisation et le contrôle d'accès sont des piliers de la sécurité de votre application. Prenez le temps de bien concevoir votre système de permissions dès le début du projet, car il est difficile de le modifier par la suite.

⏭️ [Chiffrement des données](/16-securite-des-applications/03-chiffrement-des-donnees.md)
