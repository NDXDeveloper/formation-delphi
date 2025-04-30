# 16. Sécurité des applications
## 16.1 Authentification des utilisateurs

L'authentification des utilisateurs est une fonctionnalité essentielle pour de nombreuses applications modernes. Elle permet de vérifier l'identité d'un utilisateur et de lui accorder des droits spécifiques. Dans ce chapitre, nous allons explorer différentes méthodes d'authentification que vous pouvez implémenter dans vos applications Delphi.

### Qu'est-ce que l'authentification ?

L'authentification est le processus de vérification de l'identité d'un utilisateur. En termes simples, c'est le mécanisme qui permet de confirmer qu'une personne est bien celle qu'elle prétend être. Ce processus est généralement basé sur un ou plusieurs des éléments suivants :

- **Quelque chose que l'utilisateur connaît** : un mot de passe, un code PIN
- **Quelque chose que l'utilisateur possède** : un téléphone, une carte à puce
- **Quelque chose que l'utilisateur est** : empreinte digitale, reconnaissance faciale

### Méthodes d'authentification courantes dans Delphi

#### 1. Authentification par mot de passe

C'est la méthode la plus répandue. Voici comment implémenter une authentification basique par nom d'utilisateur et mot de passe avec une base de données MySQL/MariaDB :

```pas
procedure TFormLogin.ButtonLoginClick(Sender: TObject);
var
  Query: TFDQuery;
  HashedPassword: string;
begin
  // Ne jamais stocker les mots de passe en clair !
  // Toujours utiliser une fonction de hachage
  HashedPassword := THashSHA2.GetHashString(EditPassword.Text);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text := 'SELECT id, username, role FROM users ' +
                      'WHERE username = :username AND password_hash = :password';
    Query.ParamByName('username').AsString := EditUsername.Text;
    Query.ParamByName('password').AsString := HashedPassword;
    Query.Open;

    if Query.RecordCount > 0 then
    begin
      // Authentification réussie
      Session.LoggedUser := Query.FieldByName('username').AsString;
      Session.UserRole := Query.FieldByName('role').AsString;
      Session.UserId := Query.FieldByName('id').AsInteger;
      Session.IsAuthenticated := True;

      ShowMessage('Connexion réussie !');
      ModalResult := mrOk;
    end
    else
    begin
      // Échec de l'authentification
      ShowMessage('Nom d'utilisateur ou mot de passe incorrect.');
      EditPassword.Clear;
      EditPassword.SetFocus;
    end;
  finally
    Query.Free;
  end;
end;
```

> ⚠️ **Important** : Ne stockez jamais les mots de passe en texte clair dans votre base de données. Utilisez toujours des fonctions de hachage comme SHA-256 avec une valeur aléatoire (sel) unique pour chaque utilisateur.

#### 2. Authentification à deux facteurs (2FA)

Pour améliorer la sécurité, vous pouvez implémenter l'authentification à deux facteurs. Après la vérification du mot de passe, l'utilisateur doit fournir un code temporaire généralement envoyé par SMS ou généré par une application d'authentification.

```pas
// Nécessite Delphi 12 ou supérieur pour certaines bibliothèques de cryptographie
procedure TForm2FA.ButtonVerifyCodeClick(Sender: TObject);
var
  TOTPGenerator: TTOTPGenerator;
  IsValid: Boolean;
begin
  TOTPGenerator := TTOTPGenerator.Create;
  try
    TOTPGenerator.SecretKey := Session.UserTOTPSecret;  // Clé secrète stockée pour l'utilisateur
    IsValid := TOTPGenerator.VerifyCode(EditVerificationCode.Text);

    if IsValid then
    begin
      Session.Is2FAVerified := True;
      ModalResult := mrOk;
    end
    else
    begin
      ShowMessage('Code de vérification incorrect.');
      EditVerificationCode.Clear;
      EditVerificationCode.SetFocus;
    end;
  finally
    TOTPGenerator.Free;
  end;
end;
```

> 💡 **Astuce** : Vous pouvez utiliser des bibliothèques tierces comme [DelphiOTP](https://github.com/wendelb/DelphiOTP) pour implémenter facilement le TOTP (Time-based One-Time Password).

#### 3. Authentification par jeton (Token)

Cette méthode est particulièrement utile pour les applications qui communiquent avec des API ou des services web. Une fois l'utilisateur authentifié, un jeton lui est délivré et utilisé pour les requêtes suivantes.

```pas
function TAuthService.GetAuthToken(Username, Password: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := '';

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTClient.BaseURL := 'https://api.example.com/auth';
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Ajout des paramètres d'authentification
    RESTRequest.AddParameter('username', Username);
    RESTRequest.AddParameter('password', Password);

    // Exécution de la requête
    RESTRequest.Execute;

    // Vérification de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      // Extraction du jeton depuis la réponse JSON
      Result := RESTResponse.JSONValue.GetValue<string>('token');

      // Stockage du jeton pour une utilisation future
      Session.AuthToken := Result;
      Session.TokenExpiry := Now + (1/24); // Expire dans 1 heure
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

### Stockage sécurisé des informations d'authentification

#### Sessions utilisateur

Pour gérer les informations d'authentification pendant l'exécution de l'application, créez une classe de session :

```pas
unit UserSession;

interface

type
  TUserSession = class
  private
    FIsAuthenticated: Boolean;
    FLoggedUser: string;
    FUserRole: string;
    FUserId: Integer;
    FAuthToken: string;
    FTokenExpiry: TDateTime;
    FIs2FAVerified: Boolean;
  public
    constructor Create;
    procedure Clear;
    property IsAuthenticated: Boolean read FIsAuthenticated write FIsAuthenticated;
    property LoggedUser: string read FLoggedUser write FLoggedUser;
    property UserRole: string read FUserRole write FUserRole;
    property UserId: Integer read FUserId write FUserId;
    property AuthToken: string read FAuthToken write FAuthToken;
    property TokenExpiry: TDateTime read FTokenExpiry write FTokenExpiry;
    property Is2FAVerified: Boolean read FIs2FAVerified write FIs2FAVerified;
  end;

var
  Session: TUserSession;

implementation

constructor TUserSession.Create;
begin
  inherited;
  Clear;
end;

procedure TUserSession.Clear;
begin
  FIsAuthenticated := False;
  FLoggedUser := '';
  FUserRole := '';
  FUserId := 0;
  FAuthToken := '';
  FTokenExpiry := 0;
  FIs2FAVerified := False;
end;

initialization
  Session := TUserSession.Create;

finalization
  Session.Free;

end.
```

#### Stockage persistant des informations d'authentification

Pour permettre à l'utilisateur de rester connecté entre les sessions, vous pouvez stocker les informations de manière sécurisée :

```pas
// Nécessite Delphi 12 ou supérieur pour les fonctions avancées de cryptographie
procedure SaveRememberMeToken(UserId: Integer; Token: string);
var
  EncryptedToken: string;
  IniFile: TIniFile;
begin
  // Chiffrer le jeton avant de le stocker
  EncryptedToken := TNetEncoding.Base64.Encode(
    TCipher.AES.Encrypt(Token, AppSecretKey)
  );

  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    IniFile.WriteString('Auth', 'UserID', IntToStr(UserId));
    IniFile.WriteString('Auth', 'RememberToken', EncryptedToken);
    IniFile.WriteDateTime('Auth', 'Expiry', Now + 30); // Expire dans 30 jours
  finally
    IniFile.Free;
  end;
end;

function TryAutoLogin: Boolean;
var
  IniFile: TIniFile;
  EncryptedToken, Token: string;
  UserId: Integer;
  Expiry: TDateTime;
  Query: TFDQuery;
begin
  Result := False;

  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    if not IniFile.SectionExists('Auth') then
      Exit;

    UserId := StrToIntDef(IniFile.ReadString('Auth', 'UserID', '0'), 0);
    EncryptedToken := IniFile.ReadString('Auth', 'RememberToken', '');
    Expiry := IniFile.ReadDateTime('Auth', 'Expiry', 0);

    // Vérifier si le jeton n'est pas expiré
    if (UserId = 0) or (EncryptedToken = '') or (Now > Expiry) then
      Exit;

    // Déchiffrer le jeton
    try
      Token := TCipher.AES.Decrypt(
        TNetEncoding.Base64.Decode(EncryptedToken),
        AppSecretKey
      );
    except
      Exit;
    end;

    // Vérifier le jeton en base de données
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DataModule1.FDConnection1;
      Query.SQL.Text := 'SELECT username, role FROM users ' +
                        'WHERE id = :id AND remember_token = :token';
      Query.ParamByName('id').AsInteger := UserId;
      Query.ParamByName('token').AsString := Token;
      Query.Open;

      if Query.RecordCount > 0 then
      begin
        // Authentification automatique réussie
        Session.LoggedUser := Query.FieldByName('username').AsString;
        Session.UserRole := Query.FieldByName('role').AsString;
        Session.UserId := UserId;
        Session.IsAuthenticated := True;

        Result := True;
      end;
    finally
      Query.Free;
    end;
  finally
    IniFile.Free;
  end;
end;
```

> ⚠️ **Sécurité** : Pour les applications professionnelles ou traitant des données sensibles, envisagez d'utiliser un stockage encore plus sécurisé comme le Credential Manager de Windows ou le Keychain sur macOS.

### Création d'un formulaire de connexion complet

Voici un exemple de formulaire de connexion complet intégrant les bonnes pratiques :

```pas
unit LoginForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Hash, UserSession, DataModule;

type
  TFormLogin = class(TForm)
    EditUsername: TEdit;
    EditPassword: TEdit;
    ButtonLogin: TButton;
    CheckBoxRememberMe: TCheckBox;
    LabelForgotPassword: TLabel;
    PanelBottom: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure LabelForgotPasswordClick(Sender: TObject);
  private
    procedure GenerateRememberMeToken(UserId: Integer);
  public
    class function Execute: Boolean;
  end;

implementation

{$R *.dfm}

class function TFormLogin.Execute: Boolean;
var
  Form: TFormLogin;
begin
  // Essayer d'abord la connexion automatique
  if TryAutoLogin then
    Exit(True);

  Form := TFormLogin.Create(nil);
  try
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TFormLogin.FormCreate(Sender: TObject);
begin
  // Positionnement du formulaire au centre
  Position := poScreenCenter;

  // Focus sur le champ username
  EditUsername.SetFocus;
end;

procedure TFormLogin.ButtonLoginClick(Sender: TObject);
var
  Query: TFDQuery;
  HashedPassword: string;
begin
  // Validation des champs
  if EditUsername.Text.Trim = '' then
  begin
    ShowMessage('Veuillez entrer un nom d''utilisateur.');
    EditUsername.SetFocus;
    Exit;
  end;

  if EditPassword.Text = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe.');
    EditPassword.SetFocus;
    Exit;
  end;

  // Authentification
  HashedPassword := THashSHA2.GetHashString(EditPassword.Text);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text := 'SELECT id, username, role FROM users ' +
                      'WHERE username = :username AND password_hash = :password';
    Query.ParamByName('username').AsString := EditUsername.Text;
    Query.ParamByName('password').AsString := HashedPassword;
    Query.Open;

    if Query.RecordCount > 0 then
    begin
      // Authentification réussie
      Session.LoggedUser := Query.FieldByName('username').AsString;
      Session.UserRole := Query.FieldByName('role').AsString;
      Session.UserId := Query.FieldByName('id').AsInteger;
      Session.IsAuthenticated := True;

      // Gestion du "Se souvenir de moi"
      if CheckBoxRememberMe.Checked then
        GenerateRememberMeToken(Session.UserId);

      ModalResult := mrOk;
    end
    else
    begin
      // Échec de l'authentification
      ShowMessage('Nom d''utilisateur ou mot de passe incorrect.');
      EditPassword.Clear;
      EditPassword.SetFocus;
    end;
  finally
    Query.Free;
  end;
end;

procedure TFormLogin.GenerateRememberMeToken(UserId: Integer);
var
  Token: string;
  Query: TFDQuery;
begin
  // Générer un jeton aléatoire
  Token := THashSHA2.GetHashString(
    IntToStr(UserId) +
    FormatDateTime('yyyymmddhhnnsszzz', Now) +
    IntToStr(Random(100000))
  );

  // Sauvegarder le jeton en base de données
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text := 'UPDATE users SET remember_token = :token ' +
                      'WHERE id = :id';
    Query.ParamByName('token').AsString := Token;
    Query.ParamByName('id').AsInteger := UserId;
    Query.ExecSQL;

    // Sauvegarder le jeton localement
    SaveRememberMeToken(UserId, Token);
  finally
    Query.Free;
  end;
end;

procedure TFormLogin.LabelForgotPasswordClick(Sender: TObject);
begin
  // Ouvrir un formulaire de récupération de mot de passe
  if EditUsername.Text.Trim <> '' then
    ShowMessage('Un e-mail de réinitialisation a été envoyé si ce compte existe.')
  else
    ShowMessage('Veuillez d''abord entrer votre nom d''utilisateur.');
end;

end.
```

### Schéma de base de données

Voici un exemple de structure de table pour gérer les utilisateurs dans MySQL/MariaDB :

```sql
CREATE TABLE users (
  id INT AUTO_INCREMENT PRIMARY KEY,
  username VARCHAR(50) NOT NULL UNIQUE,
  password_hash VARCHAR(64) NOT NULL,
  salt VARCHAR(32) NOT NULL,
  email VARCHAR(100) NOT NULL UNIQUE,
  role VARCHAR(20) NOT NULL DEFAULT 'user',
  totp_secret VARCHAR(32) NULL,
  remember_token VARCHAR(64) NULL,
  reset_token VARCHAR(64) NULL,
  reset_token_expiry DATETIME NULL,
  created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  last_login DATETIME NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE
);
```

### Meilleures pratiques de sécurité

1. **Toujours hacher les mots de passe** avec un algorithme sécurisé comme SHA-256 ou mieux encore, utilisez des algorithmes spécialement conçus pour les mots de passe comme Argon2 ou bcrypt.

2. **Utiliser un sel unique** pour chaque utilisateur afin de se protéger contre les attaques par table arc-en-ciel.

3. **Implémenter une politique de mots de passe forts** :
   - Longueur minimale (au moins 8 caractères)
   - Mélange de lettres majuscules et minuscules, chiffres et caractères spéciaux
   - Vérification contre les mots de passe courants

4. **Limiter les tentatives de connexion** pour se protéger contre les attaques par force brute.

5. **Utiliser HTTPS** pour toutes les communications réseau impliquant des données d'authentification.

6. **Ne pas stocker d'informations sensibles** en texte clair dans les fichiers de configuration ou la base de données.

7. **Mettre en place des délais d'expiration de session** pour réduire le risque d'accès non autorisé.

### Exemple : Vérification de force du mot de passe

```pas
function IsStrongPassword(const Password: string): Boolean;
begin
  Result := (Length(Password) >= 8) and
            ContainsUpperCase(Password) and
            ContainsLowerCase(Password) and
            ContainsDigit(Password) and
            ContainsSpecialChar(Password);
end;

function ContainsUpperCase(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if S[I] in ['A'..'Z'] then
      Exit(True);
end;

function ContainsLowerCase(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if S[I] in ['a'..'z'] then
      Exit(True);
end;

function ContainsDigit(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Exit(True);
end;

function ContainsSpecialChar(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if not (S[I] in ['A'..'Z', 'a'..'z', '0'..'9']) then
      Exit(True);
end;
```

### Conclusion

L'authentification des utilisateurs est une composante critique de la sécurité de vos applications. En suivant les meilleures pratiques décrites dans ce chapitre, vous pouvez créer un système d'authentification robuste qui protège efficacement les données de vos utilisateurs.

Dans le prochain chapitre, nous aborderons la gestion des autorisations qui détermine ce qu'un utilisateur authentifié peut ou ne peut pas faire dans votre application.

### Exercices pratiques

1. Créez un formulaire de connexion simple avec nom d'utilisateur et mot de passe.
2. Ajoutez une validation de force du mot de passe lors de la création d'un compte.
3. Implémentez un système "Se souvenir de moi" qui garde l'utilisateur connecté entre les sessions.
4. Créez un système de récupération de mot de passe par e-mail.
5. Pour les plus avancés : Implémentez l'authentification à deux facteurs avec une application comme Google Authenticator.
