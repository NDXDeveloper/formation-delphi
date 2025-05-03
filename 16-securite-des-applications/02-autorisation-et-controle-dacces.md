# 16. Sécurité des applications
## 16.2 Autorisation et contrôle d'accès

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Une fois qu'un utilisateur est authentifié (a prouvé son identité), l'étape suivante consiste à gérer ce qu'il est autorisé à faire dans votre application. C'est ce qu'on appelle l'**autorisation** ou le **contrôle d'accès**.

### Distinction entre authentification et autorisation

Avant de plonger dans les détails, clarifions la différence entre ces deux concepts souvent confondus :

- **Authentification** : Vérifie l'identité de l'utilisateur ("Qui êtes-vous ?")
- **Autorisation** : Détermine ce que l'utilisateur peut faire ("Qu'avez-vous le droit de faire ?")

### Approches courantes pour l'autorisation

#### 1. Contrôle d'accès basé sur les rôles (RBAC)

C'est l'approche la plus simple et la plus couramment utilisée. Chaque utilisateur se voit attribuer un ou plusieurs rôles, et chaque rôle dispose d'un ensemble d'autorisations prédéfinies.

##### Schéma de base de données pour le RBAC

Voici une structure possible pour gérer les rôles et les permissions dans MySQL/MariaDB :

```sql
-- Table des rôles
CREATE TABLE roles (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(50) NOT NULL UNIQUE,
  description VARCHAR(255)
);

-- Table des permissions
CREATE TABLE permissions (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(50) NOT NULL UNIQUE,
  description VARCHAR(255)
);

-- Table de liaison entre rôles et permissions
CREATE TABLE role_permissions (
  role_id INT NOT NULL,
  permission_id INT NOT NULL,
  PRIMARY KEY (role_id, permission_id),
  FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE,
  FOREIGN KEY (permission_id) REFERENCES permissions(id) ON DELETE CASCADE
);

-- Table de liaison entre utilisateurs et rôles (pour les rôles multiples)
CREATE TABLE user_roles (
  user_id INT NOT NULL,
  role_id INT NOT NULL,
  PRIMARY KEY (user_id, role_id),
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE
);
```

##### Implémentation du RBAC dans Delphi

Créons une unité pour gérer les autorisations :

```pas
unit Authorization;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, UserSession;

type
  TPermission = (
    // Permissions liées aux utilisateurs
    permViewUsers,
    permCreateUser,
    permEditUser,
    permDeleteUser,

    // Permissions liées aux produits
    permViewProducts,
    permCreateProduct,
    permEditProduct,
    permDeleteProduct,

    // Permissions liées aux rapports
    permViewReports,
    permExportReports,

    // Permissions administratives
    permManageRoles,
    permAccessSettings,
    permViewLogs
  );

  TPermissions = set of TPermission;

  TRole = class
  private
    FName: string;
    FPermissions: TPermissions;
  public
    constructor Create(const AName: string; const APermissions: TPermissions);
    property Name: string read FName;
    property Permissions: TPermissions read FPermissions;
  end;

  TAuthorizationManager = class
  private
    FRoles: TObjectDictionary<string, TRole>;
    FUserRoles: TDictionary<Integer, TList<string>>;
    FIsInitialized: Boolean;

    procedure LoadRolesFromDatabase;
    procedure LoadUserRolesFromDatabase;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    function HasPermission(UserID: Integer; Permission: TPermission): Boolean;
    function GetUserRoles(UserID: Integer): TArray<string>;
    function GetRolePermissions(const RoleName: string): TPermissions;

    // Gestion des rôles et permissions (pour les administrateurs)
    procedure AssignRoleToUser(UserID: Integer; const RoleName: string);
    procedure RemoveRoleFromUser(UserID: Integer; const RoleName: string);
    procedure CreateRole(const RoleName: string; Permissions: TPermissions);
    procedure UpdateRolePermissions(const RoleName: string; Permissions: TPermissions);
    procedure DeleteRole(const RoleName: string);
  end;

var
  AuthManager: TAuthorizationManager;

implementation

uses
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, DataModule;

// Conversion entre chaînes de caractères et énumérations de permissions
function PermissionToString(Permission: TPermission): string;
begin
  case Permission of
    permViewUsers: Result := 'view_users';
    permCreateUser: Result := 'create_user';
    permEditUser: Result := 'edit_user';
    permDeleteUser: Result := 'delete_user';
    permViewProducts: Result := 'view_products';
    permCreateProduct: Result := 'create_product';
    permEditProduct: Result := 'edit_product';
    permDeleteProduct: Result := 'delete_product';
    permViewReports: Result := 'view_reports';
    permExportReports: Result := 'export_reports';
    permManageRoles: Result := 'manage_roles';
    permAccessSettings: Result := 'access_settings';
    permViewLogs: Result := 'view_logs';
  else
    Result := '';
  end;
end;

function StringToPermission(const PermStr: string): TPermission;
begin
  if PermStr = 'view_users' then Result := permViewUsers
  else if PermStr = 'create_user' then Result := permCreateUser
  else if PermStr = 'edit_user' then Result := permEditUser
  else if PermStr = 'delete_user' then Result := permDeleteUser
  else if PermStr = 'view_products' then Result := permViewProducts
  else if PermStr = 'create_product' then Result := permCreateProduct
  else if PermStr = 'edit_product' then Result := permEditProduct
  else if PermStr = 'delete_product' then Result := permDeleteProduct
  else if PermStr = 'view_reports' then Result := permViewReports
  else if PermStr = 'export_reports' then Result := permExportReports
  else if PermStr = 'manage_roles' then Result := permManageRoles
  else if PermStr = 'access_settings' then Result := permAccessSettings
  else if PermStr = 'view_logs' then Result := permViewLogs
  else
    raise Exception.CreateFmt('Permission inconnue: %s', [PermStr]);
end;

{ TRole }

constructor TRole.Create(const AName: string; const APermissions: TPermissions);
begin
  inherited Create;
  FName := AName;
  FPermissions := APermissions;
end;

{ TAuthorizationManager }

constructor TAuthorizationManager.Create;
begin
  inherited;
  FRoles := TObjectDictionary<string, TRole>.Create([doOwnsValues]);
  FUserRoles := TDictionary<Integer, TList<string>>.Create;
  FIsInitialized := False;
end;

destructor TAuthorizationManager.Destroy;
var
  List: TList<string>;
begin
  for List in FUserRoles.Values do
    List.Free;

  FUserRoles.Free;
  FRoles.Free;
  inherited;
end;

procedure TAuthorizationManager.Initialize;
begin
  if not FIsInitialized then
  begin
    LoadRolesFromDatabase;
    LoadUserRolesFromDatabase;
    FIsInitialized := True;
  end;
end;

procedure TAuthorizationManager.LoadRolesFromDatabase;
var
  RoleQuery, PermQuery: TFDQuery;
  RoleName: string;
  Permissions: TPermissions;
begin
  FRoles.Clear;

  RoleQuery := TFDQuery.Create(nil);
  PermQuery := TFDQuery.Create(nil);
  try
    RoleQuery.Connection := DataModule1.FDConnection1;
    PermQuery.Connection := DataModule1.FDConnection1;

    RoleQuery.SQL.Text := 'SELECT id, name FROM roles';
    RoleQuery.Open;

    while not RoleQuery.Eof do
    begin
      RoleName := RoleQuery.FieldByName('name').AsString;
      Permissions := [];

      PermQuery.SQL.Text :=
        'SELECT p.name FROM permissions p ' +
        'JOIN role_permissions rp ON p.id = rp.permission_id ' +
        'WHERE rp.role_id = :roleId';
      PermQuery.ParamByName('roleId').AsInteger := RoleQuery.FieldByName('id').AsInteger;
      PermQuery.Open;

      while not PermQuery.Eof do
      begin
        Include(Permissions, StringToPermission(PermQuery.FieldByName('name').AsString));
        PermQuery.Next;
      end;

      FRoles.Add(RoleName, TRole.Create(RoleName, Permissions));
      RoleQuery.Next;
    end;
  finally
    PermQuery.Free;
    RoleQuery.Free;
  end;
end;

procedure TAuthorizationManager.LoadUserRolesFromDatabase;
var
  Query: TFDQuery;
  UserID: Integer;
  RoleName: string;
  UserRolesList: TList<string>;
begin
  for UserRolesList in FUserRoles.Values do
    UserRolesList.Free;

  FUserRoles.Clear;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'SELECT ur.user_id, r.name FROM user_roles ur ' +
      'JOIN roles r ON ur.role_id = r.id';
    Query.Open;

    while not Query.Eof do
    begin
      UserID := Query.FieldByName('user_id').AsInteger;
      RoleName := Query.FieldByName('name').AsString;

      if not FUserRoles.TryGetValue(UserID, UserRolesList) then
      begin
        UserRolesList := TList<string>.Create;
        FUserRoles.Add(UserID, UserRolesList);
      end;

      UserRolesList.Add(RoleName);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TAuthorizationManager.HasPermission(UserID: Integer;
  Permission: TPermission): Boolean;
var
  UserRolesList: TList<string>;
  RoleName: string;
  Role: TRole;
begin
  Result := False;

  // Vérifier si l'utilisateur a des rôles assignés
  if not FUserRoles.TryGetValue(UserID, UserRolesList) then
    Exit;

  // Vérifier si un des rôles de l'utilisateur a la permission demandée
  for RoleName in UserRolesList do
  begin
    if FRoles.TryGetValue(RoleName, Role) then
    begin
      if Permission in Role.Permissions then
        Exit(True);
    end;
  end;
end;

function TAuthorizationManager.GetUserRoles(UserID: Integer): TArray<string>;
var
  UserRolesList: TList<string>;
begin
  if FUserRoles.TryGetValue(UserID, UserRolesList) then
    Result := UserRolesList.ToArray
  else
    SetLength(Result, 0);
end;

function TAuthorizationManager.GetRolePermissions(
  const RoleName: string): TPermissions;
var
  Role: TRole;
begin
  Result := [];
  if FRoles.TryGetValue(RoleName, Role) then
    Result := Role.Permissions;
end;

procedure TAuthorizationManager.AssignRoleToUser(UserID: Integer;
  const RoleName: string);
var
  Query: TFDQuery;
  RoleID: Integer;
  UserRolesList: TList<string>;
begin
  // Vérifier si le rôle existe
  if not FRoles.ContainsKey(RoleName) then
    raise Exception.CreateFmt('Le rôle "%s" n''existe pas.', [RoleName]);

  // Trouver l'ID du rôle
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    Query.SQL.Text := 'SELECT id FROM roles WHERE name = :name';
    Query.ParamByName('name').AsString := RoleName;
    Query.Open;

    if Query.RecordCount = 0 then
      raise Exception.CreateFmt('Le rôle "%s" n''existe pas en base de données.', [RoleName]);

    RoleID := Query.FieldByName('id').AsInteger;

    // Vérifier si l'attribution existe déjà
    Query.SQL.Text :=
      'SELECT 1 FROM user_roles WHERE user_id = :userId AND role_id = :roleId';
    Query.ParamByName('userId').AsInteger := UserID;
    Query.ParamByName('roleId').AsInteger := RoleID;
    Query.Open;

    if Query.RecordCount = 0 then
    begin
      // Ajouter l'attribution
      Query.SQL.Text :=
        'INSERT INTO user_roles (user_id, role_id) VALUES (:userId, :roleId)';
      Query.ParamByName('userId').AsInteger := UserID;
      Query.ParamByName('roleId').AsInteger := RoleID;
      Query.ExecSQL;
    end;

    // Mettre à jour la liste en mémoire
    if not FUserRoles.TryGetValue(UserID, UserRolesList) then
    begin
      UserRolesList := TList<string>.Create;
      FUserRoles.Add(UserID, UserRolesList);
    end;

    if UserRolesList.IndexOf(RoleName) < 0 then
      UserRolesList.Add(RoleName);

  finally
    Query.Free;
  end;
end;

procedure TAuthorizationManager.RemoveRoleFromUser(UserID: Integer;
  const RoleName: string);
var
  Query: TFDQuery;
  RoleID: Integer;
  UserRolesList: TList<string>;
  Index: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    // Trouver l'ID du rôle
    Query.SQL.Text := 'SELECT id FROM roles WHERE name = :name';
    Query.ParamByName('name').AsString := RoleName;
    Query.Open;

    if Query.RecordCount = 0 then
      Exit; // Le rôle n'existe pas, rien à faire

    RoleID := Query.FieldByName('id').AsInteger;

    // Supprimer l'attribution
    Query.SQL.Text :=
      'DELETE FROM user_roles WHERE user_id = :userId AND role_id = :roleId';
    Query.ParamByName('userId').AsInteger := UserID;
    Query.ParamByName('roleId').AsInteger := RoleID;
    Query.ExecSQL;

    // Mettre à jour la liste en mémoire
    if FUserRoles.TryGetValue(UserID, UserRolesList) then
    begin
      Index := UserRolesList.IndexOf(RoleName);
      if Index >= 0 then
        UserRolesList.Delete(Index);
    end;
  finally
    Query.Free;
  end;
end;

procedure TAuthorizationManager.CreateRole(const RoleName: string;
  Permissions: TPermissions);
var
  Query: TFDQuery;
  RoleID: Integer;
  Permission: TPermission;
  PermissionID: Integer;
begin
  if FRoles.ContainsKey(RoleName) then
    raise Exception.CreateFmt('Le rôle "%s" existe déjà.', [RoleName]);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    // Créer le rôle
    Query.SQL.Text := 'INSERT INTO roles (name) VALUES (:name)';
    Query.ParamByName('name').AsString := RoleName;
    Query.ExecSQL;

    // Récupérer l'ID du rôle créé
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
    Query.Open;
    RoleID := Query.FieldByName('id').AsInteger;

    // Ajouter les permissions
    for Permission in Permissions do
    begin
      // Trouver l'ID de la permission
      Query.SQL.Text := 'SELECT id FROM permissions WHERE name = :name';
      Query.ParamByName('name').AsString := PermissionToString(Permission);
      Query.Open;

      if Query.RecordCount > 0 then
      begin
        PermissionID := Query.FieldByName('id').AsInteger;

        // Associer la permission au rôle
        Query.SQL.Text :=
          'INSERT INTO role_permissions (role_id, permission_id) VALUES (:roleId, :permId)';
        Query.ParamByName('roleId').AsInteger := RoleID;
        Query.ParamByName('permId').AsInteger := PermissionID;
        Query.ExecSQL;
      end;
    end;

    // Mettre à jour la liste en mémoire
    FRoles.Add(RoleName, TRole.Create(RoleName, Permissions));
  finally
    Query.Free;
  end;
end;

procedure TAuthorizationManager.UpdateRolePermissions(const RoleName: string;
  Permissions: TPermissions);
var
  Query: TFDQuery;
  RoleID: Integer;
  Permission: TPermission;
  PermissionID: Integer;
  Role: TRole;
begin
  if not FRoles.TryGetValue(RoleName, Role) then
    raise Exception.CreateFmt('Le rôle "%s" n''existe pas.', [RoleName]);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    // Trouver l'ID du rôle
    Query.SQL.Text := 'SELECT id FROM roles WHERE name = :name';
    Query.ParamByName('name').AsString := RoleName;
    Query.Open;

    if Query.RecordCount = 0 then
      raise Exception.CreateFmt('Le rôle "%s" n''existe pas en base de données.', [RoleName]);

    RoleID := Query.FieldByName('id').AsInteger;

    // Supprimer toutes les permissions existantes
    Query.SQL.Text := 'DELETE FROM role_permissions WHERE role_id = :roleId';
    Query.ParamByName('roleId').AsInteger := RoleID;
    Query.ExecSQL;

    // Ajouter les nouvelles permissions
    for Permission in Permissions do
    begin
      // Trouver l'ID de la permission
      Query.SQL.Text := 'SELECT id FROM permissions WHERE name = :name';
      Query.ParamByName('name').AsString := PermissionToString(Permission);
      Query.Open;

      if Query.RecordCount > 0 then
      begin
        PermissionID := Query.FieldByName('id').AsInteger;

        // Associer la permission au rôle
        Query.SQL.Text :=
          'INSERT INTO role_permissions (role_id, permission_id) VALUES (:roleId, :permId)';
        Query.ParamByName('roleId').AsInteger := RoleID;
        Query.ParamByName('permId').AsInteger := PermissionID;
        Query.ExecSQL;
      end;
    end;

    // Mettre à jour en mémoire
    FRoles.AddOrSetValue(RoleName, TRole.Create(RoleName, Permissions));
  finally
    Query.Free;
  end;
end;

procedure TAuthorizationManager.DeleteRole(const RoleName: string);
var
  Query: TFDQuery;
  RoleID: Integer;
begin
  if not FRoles.ContainsKey(RoleName) then
    Exit; // Le rôle n'existe pas, rien à faire

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    // Trouver l'ID du rôle
    Query.SQL.Text := 'SELECT id FROM roles WHERE name = :name';
    Query.ParamByName('name').AsString := RoleName;
    Query.Open;

    if Query.RecordCount = 0 then
      Exit; // Le rôle n'existe pas en base de données

    RoleID := Query.FieldByName('id').AsInteger;

    // Les suppressions en cascade s'occuperont des tables role_permissions et user_roles
    Query.SQL.Text := 'DELETE FROM roles WHERE id = :roleId';
    Query.ParamByName('roleId').AsInteger := RoleID;
    Query.ExecSQL;

    // Supprimer de la liste en mémoire
    FRoles.Remove(RoleName);
  finally
    Query.Free;
  end;
end;

initialization
  AuthManager := TAuthorizationManager.Create;

finalization
  AuthManager.Free;

end.
```

##### Utilisation du système d'autorisation

Voici comment utiliser le système d'autorisation dans votre application :

```pas
procedure InitializeAppSecurity;
begin
  // Charger les rôles et permissions
  AuthManager.Initialize;

  // Si votre base de données est vide, créer des rôles par défaut
  if AuthManager.GetUserRoles(1).Length = 0 then
  begin
    // Rôle administrateur
    AuthManager.CreateRole('admin', [
      permViewUsers, permCreateUser, permEditUser, permDeleteUser,
      permViewProducts, permCreateProduct, permEditProduct, permDeleteProduct,
      permViewReports, permExportReports,
      permManageRoles, permAccessSettings, permViewLogs
    ]);

    // Rôle manager
    AuthManager.CreateRole('manager', [
      permViewUsers,
      permViewProducts, permCreateProduct, permEditProduct,
      permViewReports, permExportReports
    ]);

    // Rôle utilisateur standard
    AuthManager.CreateRole('user', [
      permViewProducts,
      permViewReports
    ]);

    // Assigner le rôle admin à l'utilisateur 1 (souvent le premier créé)
    AuthManager.AssignRoleToUser(1, 'admin');
  end;
end;

// Vérifier les permissions avant d'exécuter une action
procedure TMainForm.ButtonAddUserClick(Sender: TObject);
begin
  if not AuthManager.HasPermission(Session.UserId, permCreateUser) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour créer un utilisateur.');
    Exit;
  end;

  // L'utilisateur a la permission, on peut continuer
  UserForm := TUserForm.Create(nil);
  try
    UserForm.Mode := umCreate;
    UserForm.ShowModal;
  finally
    UserForm.Free;
  end;
end;

// Activer/désactiver les contrôles selon les permissions
procedure TMainForm.UpdateUIBasedOnPermissions;
begin
  // Onglet Utilisateurs
  TabSheetUsers.TabVisible := AuthManager.HasPermission(Session.UserId, permViewUsers);
  ButtonAddUser.Visible := AuthManager.HasPermission(Session.UserId, permCreateUser);
  ButtonEditUser.Visible := AuthManager.HasPermission(Session.UserId, permEditUser);
  ButtonDeleteUser.Visible := AuthManager.HasPermission(Session.UserId, permDeleteUser);

  // Onglet Produits
  TabSheetProducts.TabVisible := AuthManager.HasPermission(Session.UserId, permViewProducts);
  ButtonAddProduct.Visible := AuthManager.HasPermission(Session.UserId, permCreateProduct);
  ButtonEditProduct.Visible := AuthManager.HasPermission(Session.UserId, permEditProduct);
  ButtonDeleteProduct.Visible := AuthManager.HasPermission(Session.UserId, permDeleteProduct);

  // Onglet Rapports
  TabSheetReports.TabVisible := AuthManager.HasPermission(Session.UserId, permViewReports);
  ButtonExportReport.Visible := AuthManager.HasPermission(Session.UserId, permExportReports);

  // Onglet Administration
  TabSheetAdmin.TabVisible := AuthManager.HasPermission(Session.UserId, permManageRoles) or
                              AuthManager.HasPermission(Session.UserId, permAccessSettings) or
                              AuthManager.HasPermission(Session.UserId, permViewLogs);
  ButtonManageRoles.Visible := AuthManager.HasPermission(Session.UserId, permManageRoles);
  ButtonSettings.Visible := AuthManager.HasPermission(Session.UserId, permAccessSettings);
  ButtonViewLogs.Visible := AuthManager.HasPermission(Session.UserId, permViewLogs);
end;
```

#### 2. Contrôle d'accès basé sur les attributs (ABAC)

Pour des scénarios plus complexes, le RBAC peut être insuffisant. Le ABAC permet des décisions d'autorisation basées sur une combinaison d'attributs de l'utilisateur, de la ressource et du contexte.

```pas
// Exemple simplifié d'ABAC pour l'accès à un dossier médical
function CanAccessMedicalRecord(UserID, PatientID: Integer): Boolean;
var
  Query: TFDQuery;
  IsDoctor, IsPatientOwner, IsEmergency: Boolean;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    // Vérifier si l'utilisateur est un médecin
    Query.SQL.Text := 'SELECT 1 FROM doctors WHERE user_id = :userId';
    Query.ParamByName('userId').AsInteger := UserID;
    Query.Open;
    IsDoctor := Query.RecordCount > 0;

    // Vérifier si l'utilisateur est le patient lui-même
    Query.SQL.Text := 'SELECT 1 FROM patients WHERE user_id = :userId AND id = :patientId';
    Query.ParamByName('userId').AsInteger := UserID;
    Query.ParamByName('patientId').AsInteger := PatientID;
    Query.Open;
    IsPatientOwner := Query.RecordCount > 0;

    // Vérifier si c'est une situation d'urgence
    Query.SQL.Text := 'SELECT is_emergency FROM system_settings';
    Query.Open;
    IsEmergency := Query.FieldByName('is_emergency').AsBoolean;

    // Règle ABAC :
    // 1. Les médecins peuvent accéder aux dossiers de leurs patients
    // 2. Les patients peuvent accéder à leur propre dossier
    // 3. En cas d'urgence, tous les médecins peuvent accéder à tous les dossiers

    Result := IsPatientOwner or
              (IsDoctor and (IsEmergency or IsPatientDoctor(UserID, PatientID)));
  finally
    Query.Free;
  end;
end;

function IsPatientDoctor(DoctorUserID, PatientID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;

    Query.SQL.Text :=
      'SELECT 1 FROM doctor_patients dp ' +
      'JOIN doctors d ON dp.doctor_id = d.id ' +
      'WHERE d.user_id = :doctorUserId AND dp.patient_id = :patientId';
    Query.ParamByName('doctorUserId').AsInteger := DoctorUserID;
    Query.ParamByName('patientId').AsInteger := PatientID;
    Query.Open;

    Result := Query.RecordCount > 0;
  finally
    Query.Free;
  end;
end;
```

### Mise en œuvre du contrôle d'accès dans l'interface utilisateur

#### 1. Désactiver ou masquer les éléments inaccessibles

Une bonne pratique consiste à ne pas afficher ou désactiver les fonctionnalités auxquelles l'utilisateur n'a pas accès.

```pas
procedure TMainForm.FormShow(Sender: TObject);
begin
  // Mettre à jour l'interface selon les permissions
  UpdateUIBasedOnPermissions;
end;

procedure TMainForm.UpdateUIBasedOnPermissions;
begin
  // Menu principal
  MenuItemUsers.Visible := AuthManager.HasPermission(Session.UserId, permViewUsers);
  MenuItemProducts.Visible := AuthManager.HasPermission(Session.UserId, permViewProducts);
  MenuItemReports.Visible := AuthManager.HasPermission(Session.UserId, permViewReports);
  MenuItemAdmin.Visible := AuthManager.HasPermission(Session.UserId, permAccessSettings);

  // Barre d'outils
  ToolButtonAddUser.Visible := AuthManager.HasPermission(Session.UserId, permCreateUser);
  ToolButtonEditUser.Visible := AuthManager.HasPermission(Session.UserId, permEditUser);
  ToolButtonDeleteUser.Visible := AuthManager.HasPermission(Session.UserId, permDeleteUser);

  // Ajuster la disposition si nécessaire
  ToolBar1.Realign;
end;
```

#### 2. Vérification dynamique des permissions

Pour les fonctionnalités qui ne peuvent pas être simplement masquées, vérifiez les permissions au moment de l'exécution :

```pas
procedure TProductForm.ButtonSaveClick(Sender: TObject);
begin
  // Vérifier les permissions selon le mode
  if (Mode = pmCreate) and
     not AuthManager.HasPermission(Session.UserId, permCreateProduct) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour créer un produit.');
    Exit;
  end
  else if (Mode = pmEdit) and
          not AuthManager.HasPermission(Session.UserId, permEditProduct) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour modifier un produit.');
    Exit;
  end;

  // L'utilisateur a les permissions nécessaires, on peut continuer
  if ValidateForm then
  begin
    SaveProductToDatabase;
    ModalResult := mrOk;
  end;
end;
```

### Journalisation des accès et tentatives non autorisées

Une bonne pratique de sécurité consiste à enregistrer toutes les tentatives d'accès, notamment celles qui échouent :

```pas
procedure LogAccessAttempt(
  const UserID: Integer;
  const ActionType: string;
  const ResourceName: string;
  const Success: Boolean;
  const Details: string = ''
);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO access_logs ' +
      '(user_id, action_type, resource_name, success, ip_address, details, log_date) ' +
      'VALUES (:userId, :actionType, :resourceName, :success, :ipAddress, :details, NOW())';

    Query.ParamByName('userId').AsInteger := UserID;
    Query.ParamByName('actionType').AsString := ActionType;
    Query.ParamByName('resourceName').AsString := ResourceName;
    Query.ParamByName('success').AsBoolean := Success;
    Query.ParamByName('ipAddress').AsString := GetClientIPAddress;
    Query.ParamByName('details').AsString := Details;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// Exemple d'utilisation
procedure TUserForm.ButtonDeleteClick(Sender: TObject);
begin
  if not AuthManager.HasPermission(Session.UserId, permDeleteUser) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour supprimer un utilisateur.');

    // Journaliser la tentative non autorisée
    LogAccessAttempt(
      Session.UserId,
      'DELETE',
      'User:' + IntToStr(UserID),
      False,
      'Tentative de suppression non autorisée'
    );

    Exit;
  end;

  // L'utilisateur a la permission, on peut continuer
  if MessageDlg('Êtes-vous sûr de vouloir supprimer cet utilisateur ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DeleteUserFromDatabase;

    // Journaliser l'action réussie
    LogAccessAttempt(
      Session.UserId,
      'DELETE',
      'User:' + IntToStr(UserID),
      True
    );

    ModalResult := mrOk;
  end;
end;
```

### Gestion des autorisations dans une application multi-fenêtres

Dans une application avec plusieurs formulaires, il est utile de créer une classe de base qui gère les autorisations :

```pas
unit SecureForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Authorization;

type
  TFormMode = (fmView, fmCreate, fmEdit, fmDelete);

  TSecureForm = class(TForm)
  private
    FMode: TFormMode;
    FRequiredPermission: TPermission;
    FResourceName: string;
    FResourceId: Integer;

    procedure CheckPermission;
  protected
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Mode: TFormMode read FMode write FMode;
    property RequiredPermission: TPermission read FRequiredPermission write FRequiredPermission;
    property ResourceName: string read FResourceName write FResourceName;
    property ResourceId: Integer read FResourceId write FResourceId;
  end;

implementation

constructor TSecureForm.Create(AOwner: TComponent);
begin
  inherited;
  FMode := fmView;
  FResourceId := 0;
end;

procedure TSecureForm.DoShow;
begin
  CheckPermission;
  inherited;
end;

procedure TSecureForm.CheckPermission;
var
  PermName: string;
  ActionName: string;
begin
  // Déterminer la permission nécessaire selon le mode
  case FMode of
    fmView: begin
      PermName := 'view';
      ActionName := 'VIEW';
    end;
    fmCreate: begin
      PermName := 'create';
      ActionName := 'CREATE';
    end;
    fmEdit: begin
      PermName := 'edit';
      ActionName := 'EDIT';
    end;
    fmDelete: begin
      PermName := 'delete';
      ActionName := 'DELETE';
    end;
  end;

  // Vérifier si l'utilisateur a la permission requise
  if not AuthManager.HasPermission(Session.UserId, FRequiredPermission) then
  begin
    // Journaliser la tentative non autorisée
    LogAccessAttempt(
      Session.UserId,
      ActionName,
      FResourceName + ':' + IntToStr(FResourceId),
      False,
      'Tentative d''accès non autorisée'
    );

    // Afficher un message d'erreur
    ShowMessage('Vous n''avez pas les droits nécessaires pour ' + PermName +
                ' ' + FResourceName + '.');

    // Fermer le formulaire
    Close;
  end
  else
  begin
    // Journaliser l'accès autorisé
    LogAccessAttempt(
      Session.UserId,
      ActionName,
      FResourceName + ':' + IntToStr(FResourceId),
      True
    );
  end;
end;

end.
```

Vous pouvez ensuite utiliser cette classe comme base pour vos formulaires :

```pas
unit ProductForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls,
  Vcl.ExtCtrls, SecureForm, Authorization;

type
  TProductForm = class(TSecureForm)
    EditName: TEdit;
    EditPrice: TEdit;
    ButtonSave: TButton;
    ButtonCancel: TButton;
    // ...
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    // ...
  private
    FProductId: Integer;
    procedure LoadProduct;
    procedure SaveProduct;
  public
    property ProductId: Integer read FProductId write FProductId;
  end;

implementation

{$R *.dfm}

procedure TProductForm.FormCreate(Sender: TObject);
begin
  ResourceName := 'Produit';

  if ProductId = 0 then
  begin
    Mode := fmCreate;
    RequiredPermission := permCreateProduct;
    Caption := 'Nouveau produit';
  end
  else
  begin
    Mode := fmEdit;
    RequiredPermission := permEditProduct;
    ResourceId := ProductId;
    Caption := 'Modifier produit';
    LoadProduct;
  end;
end;

// ...

end.
```

### Créer un formulaire de gestion des rôles et permissions

Un administrateur doit pouvoir gérer facilement les rôles et les permissions. Voici un exemple de formulaire pour cela :

```pas
unit RoleManagerForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Buttons, Authorization;

type
  TRoleManagerForm = class(TForm)
    ListBoxRoles: TListBox;
    CheckListBoxPermissions: TCheckListBox;
    PanelButtons: TPanel;
    ButtonAddRole: TButton;
    ButtonDeleteRole: TButton;
    ButtonSave: TButton;
    ButtonClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxRolesClick(Sender: TObject);
    procedure ButtonAddRoleClick(Sender: TObject);
    procedure ButtonDeleteRoleClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckListBoxPermissionsClickCheck(Sender: TObject);
  private
    FCurrentRole: string;
    FHasChanges: Boolean;

    procedure LoadRoles;
    procedure LoadPermissionsForRole(const RoleName: string);
    function GetSelectedPermissions: TPermissions;
    procedure SavePermissionsForRole;
  end;

implementation

{$R *.dfm}

procedure TRoleManagerForm.FormCreate(Sender: TObject);
var
  Permission: TPermission;
begin
  // Vérifier que l'utilisateur a le droit de gérer les rôles
  if not AuthManager.HasPermission(Session.UserId, permManageRoles) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour gérer les rôles.');
    Close;
    Exit;
  end;

  // Remplir la liste des rôles
  LoadRoles;

  // Remplir la liste des permissions
  CheckListBoxPermissions.Items.Clear;
  for Permission := Low(TPermission) to High(TPermission) do
    CheckListBoxPermissions.Items.Add(PermissionToString(Permission));

  FHasChanges := False;
end;

procedure TRoleManagerForm.LoadRoles;
var
  Roles: TArray<string>;
  Role: string;
begin
  ListBoxRoles.Items.Clear;

  // Cette méthode est fictive, vous devrez l'implémenter
  Roles := AuthManager.GetAllRoles;

  for Role in Roles do
    ListBoxRoles.Items.Add(Role);

  if ListBoxRoles.Items.Count > 0 then
  begin
    ListBoxRoles.ItemIndex := 0;
    FCurrentRole := ListBoxRoles.Items[0];
    LoadPermissionsForRole(FCurrentRole);
  end
  else
  begin
    FCurrentRole := '';
    for var I := 0 to CheckListBoxPermissions.Items.Count - 1 do
      CheckListBoxPermissions.Checked[I] := False;
    CheckListBoxPermissions.Enabled := False;
  end;
end;

procedure TRoleManagerForm.LoadPermissionsForRole(const RoleName: string);
var
  Permissions: TPermissions;
  I: Integer;
begin
  Permissions := AuthManager.GetRolePermissions(RoleName);

  for I := 0 to CheckListBoxPermissions.Items.Count - 1 do
    CheckListBoxPermissions.Checked[I] := StringToPermission(CheckListBoxPermissions.Items[I]) in Permissions;

  CheckListBoxPermissions.Enabled := True;
end;

procedure TRoleManagerForm.ListBoxRolesClick(Sender: TObject);
begin
  if (ListBoxRoles.ItemIndex >= 0) and (FCurrentRole <> ListBoxRoles.Items[ListBoxRoles.ItemIndex]) then
  begin
    // Si des modifications ont été apportées, demander confirmation
    if FHasChanges then
    begin
      if MessageDlg('Vous avez des modifications non enregistrées. Continuer quand même ?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      begin
        // Revenir à la sélection précédente
        for var I := 0 to ListBoxRoles.Items.Count - 1 do
          if ListBoxRoles.Items[I] = FCurrentRole then
          begin
            ListBoxRoles.ItemIndex := I;
            Break;
          end;
        Exit;
      end;
    end;

    FCurrentRole := ListBoxRoles.Items[ListBoxRoles.ItemIndex];
    LoadPermissionsForRole(FCurrentRole);
    FHasChanges := False;
  end;
end;

procedure TRoleManagerForm.CheckListBoxPermissionsClickCheck(Sender: TObject);
begin
  FHasChanges := True;
end;

function TRoleManagerForm.GetSelectedPermissions: TPermissions;
var
  I: Integer;
begin
  Result := [];

  for I := 0 to CheckListBoxPermissions.Items.Count - 1 do
    if CheckListBoxPermissions.Checked[I] then
      Include(Result, StringToPermission(CheckListBoxPermissions.Items[I]));
end;

procedure TRoleManagerForm.SavePermissionsForRole;
begin
  if FCurrentRole <> '' then
  begin
    AuthManager.UpdateRolePermissions(FCurrentRole, GetSelectedPermissions);
    FHasChanges := False;
  end;
end;

procedure TRoleManagerForm.ButtonAddRoleClick(Sender: TObject);
var
  NewRoleName: string;
begin
  if InputQuery('Nouveau rôle', 'Nom du rôle :', NewRoleName) and (NewRoleName <> '') then
  begin
    // Vérifier si le rôle existe déjà
    if ListBoxRoles.Items.IndexOf(NewRoleName) >= 0 then
    begin
      ShowMessage('Un rôle avec ce nom existe déjà.');
      Exit;
    end;

    // Créer le rôle sans permissions initiales
    AuthManager.CreateRole(NewRoleName, []);

    // Rafraîchir la liste
    LoadRoles;

    // Sélectionner le nouveau rôle
    ListBoxRoles.ItemIndex := ListBoxRoles.Items.IndexOf(NewRoleName);
    FCurrentRole := NewRoleName;
    LoadPermissionsForRole(FCurrentRole);
  end;
end;

procedure TRoleManagerForm.ButtonDeleteRoleClick(Sender: TObject);
begin
  if (ListBoxRoles.ItemIndex >= 0) and
     (MessageDlg('Êtes-vous sûr de vouloir supprimer ce rôle ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    AuthManager.DeleteRole(ListBoxRoles.Items[ListBoxRoles.ItemIndex]);
    LoadRoles;
    FHasChanges := False;
  end;
end;

procedure TRoleManagerForm.ButtonSaveClick(Sender: TObject);
begin
  SavePermissionsForRole;
  ShowMessage('Modifications enregistrées avec succès.');
end;

end.
```

### Autorisations au niveau des données

Dans certains cas, vous devez restreindre l'accès aux données spécifiques plutôt qu'aux fonctionnalités. Par exemple, un utilisateur peut voir uniquement ses propres données ou celles de son département.

```pas
// Filtrer les données selon les autorisations de l'utilisateur
procedure TDataModule1.LoadFilteredCustomers(DataSet: TDataSet);
var
  SQL: string;
begin
  SQL := 'SELECT * FROM customers';

  // Si l'utilisateur n'est pas administrateur, filtrer les données
  if not AuthManager.HasPermission(Session.UserId, permViewAllCustomers) then
  begin
    // Obtenir l'ID du département de l'utilisateur
    var DeptID := GetUserDepartmentID(Session.UserId);

    if DeptID > 0 then
      SQL := SQL + ' WHERE department_id = ' + IntToStr(DeptID)
    else
      SQL := SQL + ' WHERE created_by = ' + IntToStr(Session.UserId);
  end;

  SQL := SQL + ' ORDER BY name';

  with DataSet as TFDQuery do
  begin
    Close;
    SQL.Text := SQL;
    Open;
  end;
end;
```

### Sécurité au niveau des champs

Vous pouvez également restreindre l'accès à certains champs spécifiques d'un formulaire :

```pas
procedure TCustomerForm.UpdateFieldSecurity;
begin
  // Champ de remise uniquement visible pour les managers et admins
  LabelDiscount.Visible := AuthManager.HasPermission(Session.UserId, permApplyDiscount);
  EditDiscount.Visible := LabelDiscount.Visible;

  // Champ de limite de crédit en lecture seule pour les vendeurs
  EditCreditLimit.ReadOnly := not AuthManager.HasPermission(Session.UserId, permEditCreditLimit);
  if EditCreditLimit.ReadOnly then
    EditCreditLimit.Color := clBtnFace
  else
    EditCreditLimit.Color := clWindow;

  // Onglet d'historique des paiements visible uniquement pour la finance
  TabSheetPaymentHistory.TabVisible := AuthManager.HasPermission(Session.UserId, permViewPaymentHistory);
end;
```

### Meilleures pratiques pour l'autorisation

1. **Principe du moindre privilège** : Accordez uniquement les permissions nécessaires à chaque rôle pour accomplir ses tâches.

2. **Vérification côté serveur** : Ne vous fiez jamais uniquement aux contrôles d'interface utilisateur pour l'autorisation. Vérifiez toujours les permissions au niveau du traitement des données.

3. **Journalisation** : Enregistrez toutes les tentatives d'accès, réussies ou non, pour pouvoir détecter d'éventuelles tentatives d'accès non autorisé.

4. **Séparation des responsabilités** : Définissez des rôles distincts qui se complètent, plutôt que de créer des "super-utilisateurs" qui peuvent tout faire.

5. **Réévaluation périodique** : Révisez régulièrement les rôles et permissions attribués aux utilisateurs pour vous assurer qu'ils correspondent toujours à leurs besoins réels.

6. **Révocation immédiate** : Mettez en place un processus pour retirer rapidement les droits d'un utilisateur lorsqu'il change de fonction ou quitte l'organisation.

7. **Double validation** : Pour les actions critiques, envisagez d'exiger l'approbation de deux utilisateurs distincts.

### Implementation du principe des quatre yeux (Double validation)

Pour les opérations critiques, comme les virements bancaires ou l'approbation de contrats, il est recommandé d'implémenter le principe des "quatre yeux" :

```pas
procedure TPaymentForm.ButtonApproveClick(Sender: TObject);
var
  ApprovalForm: TApprovalForm;
  ApproverID: Integer;
begin
  // Vérifier si l'utilisateur a le droit d'initier une approbation
  if not AuthManager.HasPermission(Session.UserId, permInitiatePayment) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour initier un paiement.');
    Exit;
  end;

  // Créer une demande d'approbation
  ApprovalForm := TApprovalForm.Create(nil);
  try
    ApprovalForm.PaymentID := PaymentID;
    ApprovalForm.PaymentAmount := StrToFloat(EditAmount.Text);
    ApprovalForm.PaymentDescription := MemoDescription.Text;

    if ApprovalForm.ShowModal = mrOk then
    begin
      ApproverID := ApprovalForm.SelectedApproverID;

      // Enregistrer la demande en base de données
      SaveApprovalRequest(PaymentID, Session.UserId, ApproverID);

      ShowMessage('Demande d''approbation envoyée avec succès.');
      ModalResult := mrOk;
    end;
  finally
    ApprovalForm.Free;
  end;
end;

// Dans un autre formulaire pour les approbateurs
procedure TApprovalListForm.ButtonApproveClick(Sender: TObject);
var
  RequestID: Integer;
begin
  if ListViewRequests.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner une demande à approuver.');
    Exit;
  end;

  RequestID := Integer(ListViewRequests.Selected.Data);

  // Vérifier si l'utilisateur est bien l'approbateur désigné
  if not IsUserRequestApprover(RequestID, Session.UserId) then
  begin
    ShowMessage('Vous n''êtes pas l''approbateur désigné pour cette demande.');
    Exit;
  end;

  // Vérifier si l'utilisateur a le droit d'approuver
  if not AuthManager.HasPermission(Session.UserId, permApprovePayment) then
  begin
    ShowMessage('Vous n''avez pas les droits nécessaires pour approuver un paiement.');
    Exit;
  end;

  // Vérifier que l'approbateur est différent de l'initiateur
  if IsUserRequestInitiator(RequestID, Session.UserId) then
  begin
    ShowMessage('Vous ne pouvez pas approuver une demande que vous avez initiée.');
    Exit;
  end;

  // Procéder à l'approbation
  if MessageDlg('Êtes-vous sûr de vouloir approuver cette demande ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ApproveRequest(RequestID, Session.UserId);
    ShowMessage('Demande approuvée avec succès.');
    LoadPendingRequests;
  end;
end;
```

### Conclusion

La mise en place d'un système robuste d'autorisation et de contrôle d'accès est essentielle pour protéger les données sensibles et assurer que chaque utilisateur n'a accès qu'aux fonctionnalités dont il a besoin. En combinant différentes approches comme le RBAC et l'ABAC, vous pouvez créer un système flexible qui s'adapte aux besoins spécifiques de votre application.

Dans le prochain chapitre, nous aborderons le chiffrement des données, qui constitue une couche supplémentaire de sécurité pour protéger les informations sensibles stockées dans votre application.

### Exercices pratiques

1. Créez un système RBAC simple avec trois rôles : administrateur, manager et utilisateur.

2. Implémentez un écran de gestion des rôles permettant d'ajouter/modifier/supprimer des rôles et leurs permissions.

3. Modifiez une application existante pour intégrer le contrôle d'accès basé sur les rôles.

4. Créez un système de journalisation des accès et affichez un rapport des tentatives d'accès non autorisées.

5. Pour les plus avancés : Implémentez un système d'autorisation basé sur les attributs pour une application de gestion de documents où l'accès dépend du département de l'utilisateur et de la classification du document.

⏭️ [Chiffrement des données](16-securite-des-applications/03-chiffrement-des-donnees.md)
