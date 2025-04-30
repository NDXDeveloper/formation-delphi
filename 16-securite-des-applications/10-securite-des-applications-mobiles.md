# 16. Sécurité des applications
## 16.10 Sécurité des applications mobiles

La sécurité des applications mobiles est un enjeu crucial dans le développement d'applications modernes. En effet, les applications mobiles accèdent souvent à des données sensibles et personnelles, et elles sont exposées à divers risques de sécurité. Dans ce chapitre, nous explorerons les meilleures pratiques et techniques pour sécuriser vos applications mobiles développées avec Delphi et FireMonkey (FMX).

### Les défis spécifiques de la sécurité mobile

Le développement d'applications mobiles sécurisées présente des défis uniques par rapport aux applications desktop :

1. **Environnement physique non contrôlé** : Les appareils mobiles peuvent être perdus ou volés, exposant potentiellement les données de l'application.

2. **Écosystème d'applications** : L'appareil exécute de nombreuses applications simultanément, certaines potentiellement malveillantes.

3. **Connectivité multiple** : Les appareils mobiles utilisent diverses méthodes de connexion (Wi-Fi, données cellulaires, Bluetooth) qui présentent chacune des risques de sécurité spécifiques.

4. **Ressources limitées** : Les contraintes de batterie et de performance peuvent limiter l'implémentation de certaines mesures de sécurité.

5. **Permissions et accès aux fonctionnalités sensibles** : Caméra, géolocalisation, contacts, etc.

### Mesures de sécurité fondamentales

#### 1. Sécurisation des données stockées localement

Les applications mobiles stockent souvent des données sur l'appareil. Ces données doivent être protégées adéquatement.

##### a. Chiffrement des données locales

```pas
uses
  System.SysUtils, System.IOUtils,
  {$IF CompilerVersion >= 33} // Delphi 10.3 Rio ou supérieur
  System.Hash, System.Crypto,
  {$ENDIF}
  FMX.DialogService;

// Fonction simple pour chiffrer une chaîne avec AES
function EncryptString(const PlainText, Password: string): string;
{$IF CompilerVersion >= 33}
var
  Cipher: TCipher_AES;
  Key: TBytes;
  EncryptedBytes: TBytes;
begin
  // Générer une clé à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);
  SetLength(Key, 32); // AES-256 nécessite une clé de 32 bytes

  // Créer un chiffreur AES
  Cipher := TCipher_AES.Create;
  try
    Cipher.Mode := TCipherMode.cmCBC;
    Cipher.Padding := TPadding.pkPKCS7;
    Cipher.Key := Key;

    // Générer un IV aléatoire (vecteur d'initialisation)
    Cipher.GenerateIV;

    // Chiffrer les données
    EncryptedBytes := Cipher.EncryptString(PlainText, TEncoding.UTF8);

    // Concaténer l'IV avec les données chiffrées pour le stockage
    Result := TNetEncoding.Base64.EncodeBytesToString(
      Cipher.IV + EncryptedBytes);
  finally
    Cipher.Free;
  end;
{$ELSE}
begin
  // Pour les versions antérieures, utilisez des bibliothèques tierces ou CryptoAPI
  Result := PlainText; // Ceci est juste un placeholder
  ShowMessage('Le chiffrement AES nécessite Delphi 10.3 Rio ou supérieur.');
{$ENDIF}
end;

// Fonction pour déchiffrer une chaîne avec AES
function DecryptString(const CipherText, Password: string): string;
{$IF CompilerVersion >= 33}
var
  Cipher: TCipher_AES;
  Key: TBytes;
  CipherBytes, IV: TBytes;
begin
  // Générer la même clé à partir du mot de passe
  Key := THashSHA2.GetHashBytes(Password);
  SetLength(Key, 32);

  // Décodage du texte chiffré base64
  CipherBytes := TNetEncoding.Base64.DecodeStringToBytes(CipherText);

  // Extraire l'IV des 16 premiers octets
  SetLength(IV, 16);
  if Length(CipherBytes) < 16 then
    Exit(''); // Données chiffrées invalides

  Move(CipherBytes[0], IV[0], 16);

  // Créer un chiffreur AES pour le déchiffrement
  Cipher := TCipher_AES.Create;
  try
    Cipher.Mode := TCipherMode.cmCBC;
    Cipher.Padding := TPadding.pkPKCS7;
    Cipher.Key := Key;
    Cipher.IV := IV;

    // Déchiffrer les données (en ignorant les 16 premiers bytes qui sont l'IV)
    SetLength(CipherBytes, Length(CipherBytes) - 16);
    Move(CipherBytes[16], CipherBytes[0], Length(CipherBytes));

    Result := Cipher.DecryptString(CipherBytes, TEncoding.UTF8);
  finally
    Cipher.Free;
  end;
{$ELSE}
begin
  // Pour les versions antérieures, utilisez des bibliothèques tierces
  Result := CipherText; // Ceci est juste un placeholder
  ShowMessage('Le déchiffrement AES nécessite Delphi 10.3 Rio ou supérieur.');
{$ENDIF}
end;
```

##### b. Utilisation du stockage sécurisé spécifique à la plateforme

Delphi permet d'accéder aux mécanismes de stockage sécurisé propres à chaque plateforme :

```pas
unit SecureStorage;

interface

uses
  System.SysUtils, System.Classes;

type
  TSecureStorage = class
  public
    // Stocke une valeur de manière sécurisée
    function StoreSecurely(const Key, Value: string): Boolean;

    // Récupère une valeur stockée de manière sécurisée
    function RetrieveSecurely(const Key: string; out Value: string): Boolean;

    // Supprime une valeur stockée
    function RemoveSecureValue(const Key: string): Boolean;
  end;

implementation

{$IF DEFINED(IOS)}
uses
  Macapi.ObjectiveC, iOSapi.Foundation, iOSapi.Security;
{$ENDIF}

{$IF DEFINED(ANDROID)}
uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Androidapi.JNI.App;
{$ENDIF}

{$IF DEFINED(IOS)}
function TSecureStorage.StoreSecurely(const Key, Value: string): Boolean;
var
  KeychainQuery: NSMutableDictionary;
  KeychainItemRef: Pointer;
  SecItemStatus: OSStatus;
begin
  Result := False;

  // Créer un dictionnaire pour la requête Keychain
  KeychainQuery := TNSMutableDictionary.Create;
  try
    // Configuration de la requête pour le keychain iOS
    KeychainQuery.setValue(NSClassFromString(StrToNSStr('genp')),
                          StrToNSStr(kSecClass));
    KeychainQuery.setValue(StrToNSStr(Key),
                          StrToNSStr(kSecAttrAccount));
    KeychainQuery.setValue(StrToNSStr(Value),
                          StrToNSStr(kSecValueData));

    // Vérifier d'abord si l'élément existe déjà
    SecItemCopyMatching(KeychainQuery.getObjectID, KeychainItemRef);

    if KeychainItemRef <> nil then
    begin
      // L'élément existe, mettre à jour
      SecItemDelete(KeychainQuery.getObjectID);
    end;

    // Ajouter l'élément dans le keychain
    SecItemStatus := SecItemAdd(KeychainQuery.getObjectID, KeychainItemRef);
    Result := (SecItemStatus = errSecSuccess);
  finally
    KeychainQuery.release;
  end;
end;

// Autres méthodes pour iOS...
{$ELSEIF DEFINED(ANDROID)}
function TSecureStorage.StoreSecurely(const Key, Value: string): Boolean;
var
  SharedPrefs: JSharedPreferences;
  Editor: JSharedPreferences_Editor;
begin
  Result := False;
  try
    // Obtenir les SharedPreferences en mode privé
    SharedPrefs := TAndroidHelper.Context.getSharedPreferences(
      StringToJString('SecurePrefs'), TJContext.JavaClass.MODE_PRIVATE);

    // Utiliser l'éditeur pour stocker les valeurs
    Editor := SharedPrefs.edit;
    Editor.putString(StringToJString(Key), StringToJString(Value));

    // Valider les modifications
    Result := Editor.commit;
  except
    // Gérer les exceptions
  end;
end;

// Autres méthodes pour Android...
{$ELSE}
// Pour Windows, utiliser le stockage sécurisé de Windows
function TSecureStorage.StoreSecurely(const Key, Value: string): Boolean;
begin
  // Utiliser Windows Credential Manager ou un autre mécanisme sécurisé
  // Ceci est un placeholder
  Result := False;
end;
{$ENDIF}

// Autres implémentations...

end.
```

> [!NOTE]
> **Implémenter par plateforme**
>
> Cette implémentation est simplifiée. Pour une solution complète, il est recommandé d'utiliser des bibliothèques tierces spécialisées ou d'implémenter les méthodes de stockage sécurisé spécifiques à chaque plateforme.

#### 2. Protection contre les attaques par injection de code

##### a. Utilisation de requêtes paramétrées pour SQL

Lorsque vous travaillez avec des bases de données locales (comme SQLite) sur mobile, utilisez toujours des requêtes paramétrées :

```pas
procedure TDataModule1.InsertUserDataSafely(const Username, Email: string);
begin
  // INCORRECT - Vulnérable aux injections SQL
  // FDQuery1.SQL.Text := 'INSERT INTO Users (Username, Email) VALUES ("' +
  //   Username + '", "' + Email + '")';

  // CORRECT - Utilisation de paramètres
  FDQuery1.SQL.Text := 'INSERT INTO Users (Username, Email) VALUES (:Username, :Email)';
  FDQuery1.ParamByName('Username').AsString := Username;
  FDQuery1.ParamByName('Email').AsString := Email;
  FDQuery1.ExecSQL;
end;
```

##### b. Validation des données d'entrée

```pas
function IsValidEmail(const Email: string): Boolean;
var
  RegEx: TRegEx;
begin
  // Expression régulière simple pour la validation d'emails
  RegEx := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
  Result := RegEx.IsMatch(Email);
end;

procedure TForm1.btnSubmitClick(Sender: TObject);
begin
  // Validation avant traitement
  if (Trim(edtUsername.Text) = '') then
  begin
    ShowMessage('Le nom d''utilisateur ne peut pas être vide');
    Exit;
  end;

  if not IsValidEmail(edtEmail.Text) then
  begin
    ShowMessage('Adresse email invalide');
    Exit;
  end;

  // Si la validation réussit, traiter les données
  DataModule1.InsertUserDataSafely(edtUsername.Text, edtEmail.Text);
end;
```

#### 3. Sécurisation des communications réseau

##### a. Utilisation de HTTPS

Toujours utiliser HTTPS plutôt que HTTP pour les communications réseau :

```pas
procedure TForm1.GetSecureData;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configurer le client REST avec HTTPS
    RESTClient.BaseURL := 'https://api.exemple.com/data';
    RESTClient.Accept := 'application/json';

    // Paramètres de sécurité supplémentaires
    RESTClient.SecureProtocols := [THTTPSecureProtocol.TLS12]; // Utiliser TLS 1.2 ou supérieur

    // Configurer la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmGET;

    // Exécuter la requête
    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // Traiter la réponse
      Memo1.Lines.Add(RESTResponse.Content);
    end
    else
    begin
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

##### b. Certificat pinning (épinglage de certificat)

Pour une sécurité renforcée, vous pouvez implémenter le certificat pinning pour vous assurer que votre application communique uniquement avec le serveur légitime :

```pas
procedure TForm1.ConfigureRESTClientWithPinning(RESTClient: TRESTClient);
var
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  // Créer un gestionnaire SSL
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configurer le gestionnaire SSL avec le certificat épinglé
    SSLHandler.SSLOptions.RootCertFile := TPath.Combine(TPath.GetDocumentsPath, 'cert.pem');
    SSLHandler.SSLOptions.VerifyDepth := 9;
    SSLHandler.SSLOptions.VerifyMode := [sslvrfPeer, sslvrfFailIfNoPeerCert];

    // Attribuer le gestionnaire SSL au client REST
    // Note: Ceci est un exemple conceptuel, l'implémentation exacte
    // peut varier selon la version de Delphi et les bibliothèques utilisées
    RESTClient.HTTPClient.IOHandler := SSLHandler;
  except
    SSLHandler.Free;
    raise;
  end;
end;
```

> [!NOTE]
> **Certificat pinning**
>
> L'implémentation du certificat pinning peut nécessiter du code spécifique à la plateforme ou des bibliothèques tierces selon la version de Delphi que vous utilisez.

#### 4. Gestion sécurisée des autorisations

Les applications mobiles nécessitent des autorisations pour accéder à diverses fonctionnalités de l'appareil. Il est important de gérer ces autorisations de manière sécurisée et transparente.

```pas
procedure TForm1.btnTakePhotoClick(Sender: TObject);
begin
  {$IF DEFINED(ANDROID)}
  PermissionsService.RequestPermissions(['android.permission.CAMERA'],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        TakePhoto
      else
        ShowMessage('Permission d''accès à la caméra refusée');
    end);
  {$ELSEIF DEFINED(IOS)}
  // Code spécifique iOS pour demander l'accès à la caméra
  {$ELSE}
  TakePhoto; // Sur d'autres plateformes, les permissions peuvent être gérées différemment
  {$ENDIF}
end;

procedure TForm1.TakePhoto;
begin
  // Code pour prendre une photo
  // ...
end;
```

### Bonnes pratiques supplémentaires

#### 1. Détection des appareils rootés/jailbreakés

Les appareils rootés (Android) ou jailbreakés (iOS) présentent des risques de sécurité supplémentaires. Votre application peut détecter ces situations :

```pas
function IsDeviceRooted: Boolean;
begin
  Result := False;

  {$IF DEFINED(ANDROID)}
  // Vérifier les chemins courants de binaires su sur Android
  Result := FileExists('/system/bin/su') or
            FileExists('/system/xbin/su') or
            FileExists('/sbin/su') or
            FileExists('/data/local/xbin/su') or
            FileExists('/data/local/bin/su');
  {$ELSEIF DEFINED(IOS)}
  // Vérifier les applications courantes sur les appareils jailbreakés
  Result := DirectoryExists('/Applications/Cydia.app') or
            FileExists('/bin/bash') or
            FileExists('/bin/sh') or
            FileExists('/private/var/lib/apt/');
  {$ENDIF}
end;

procedure TForm1.CheckDeviceSecurity;
begin
  if IsDeviceRooted then
  begin
    ShowMessage('Attention : Votre appareil semble être rooté/jailbreaké. ' +
                'L''utilisation de l''application sur cet appareil peut compromettre ' +
                'la sécurité de vos données.');

    // Selon la politique de votre application, vous pouvez:
    // 1. Simplement informer l'utilisateur (comme ci-dessus)
    // 2. Limiter certaines fonctionnalités
    // 3. Quitter l'application
    // Application.Terminate;
  end;
end;
```

#### 2. Obfuscation du code

Pour rendre le reverse engineering plus difficile, vous pouvez utiliser l'obfuscation de code :

```pas
{$IF DEFINED(RELEASE)}
{$OBFUSCATE ON} // Directive pour l'obfuscateur (dépend de l'outil utilisé)
{$ENDIF}

procedure TForm1.ProcessSensitiveData;
begin
  // Code sensible qui sera obfusqué en version release
end;

{$IF DEFINED(RELEASE)}
{$OBFUSCATE OFF}
{$ENDIF}
```

> [!NOTE]
> **Obfuscation**
>
> Delphi n'a pas de système d'obfuscation intégré. Vous devez utiliser des outils tiers comme "Smart Mobile Studio", "DexGuard" (pour Android) ou d'autres solutions d'obfuscation compatibles avec Delphi.

#### 3. Prévention des captures d'écran pour les informations sensibles

Pour les écrans contenant des informations sensibles, vous pouvez empêcher les captures d'écran :

```pas
procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IF DEFINED(ANDROID)}
  // Empêcher les captures d'écran sur Android
  var Window := MainActivity.getWindow;
  Window.setFlags(
    TJWindowManager_LayoutParams.JavaClass.FLAG_SECURE,
    TJWindowManager_LayoutParams.JavaClass.FLAG_SECURE);
  {$ELSEIF DEFINED(IOS)}
  // iOS ne permet pas de désactiver les captures d'écran au niveau de l'API,
  // mais vous pouvez utiliser d'autres techniques comme masquer le contenu
  // lorsque l'application passe en arrière-plan
  {$ENDIF}
end;

procedure TForm1.ApplicationEventHandler(Sender: TObject; AEvent: TApplicationEvent);
begin
  case AEvent of
    TApplicationEvent.EnteredBackground:
      HideSensitiveData;

    TApplicationEvent.BecameActive:
      ShowSensitiveData;
  end;
end;
```

#### 4. Mise en œuvre du verrouillage de l'application

Pour protéger les applications contenant des données sensibles, vous pouvez mettre en place un verrouillage par code PIN ou biométrique :

```pas
unit AppLock;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms;

type
  TAppLockType = (altNone, altPIN, altBiometric);

  TAppLock = class
  private
    FLockType: TAppLockType;
    FIsLocked: Boolean;
    FPINHash: string;

    function HashPIN(const PIN: string): string;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create;

    function SetupPINLock(const PIN: string): Boolean;
    function VerifyPIN(const PIN: string): Boolean;

    function SetupBiometricLock: Boolean;
    function VerifyBiometric: Boolean;

    procedure LockApplication;
    function UnlockApplication(const PIN: string = ''): Boolean;

    property IsLocked: Boolean read FIsLocked;
    property LockType: TAppLockType read FLockType;
  end;

// Implémentation...

implementation

uses
  {$IF DEFINED(IOS)}
  iOSapi.LocalAuthentication,
  {$ELSEIF DEFINED(ANDROID)}
  Androidapi.JNI.BiometricPrompt,
  Androidapi.Helpers,
  {$ENDIF}
  System.Hash, System.IOUtils;

constructor TAppLock.Create;
begin
  inherited Create;
  LoadSettings;
  // Par défaut, l'application est verrouillée au démarrage si un verrou est configuré
  FIsLocked := FLockType <> altNone;
end;

function TAppLock.HashPIN(const PIN: string): string;
begin
  // Utiliser un algorithme de hachage sécurisé avec sel
  // Ceci est simplifié pour l'exemple
  Result := THashSHA2.GetHashString(PIN + 'SomeRandomSalt');
end;

// Autres implémentations...

end.
```

### Exemple d'application complète avec sécurité renforcée

Voici un exemple de formulaire principal qui intègre plusieurs des concepts de sécurité discutés :

```pas
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.ListBox,
  AppLock, SecureStorage;

type
  TFormMain = class(TForm)
    btnLogin: TButton;
    edtUsername: TEdit;
    edtPassword: TEdit;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ApplicationEventHandler(Sender: TObject; AEvent: TApplicationEvent);
  private
    FAppLock: TAppLock;
    FSecureStorage: TSecureStorage;
    procedure CheckDeviceSecurity;
    procedure OnBiometricResult(Success: Boolean; const ErrorMsg: string);
    procedure HideSensitiveData;
    procedure ShowSensitiveData;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Vérifier la sécurité de l'appareil
  CheckDeviceSecurity;

  // Initialiser le verrou d'application
  FAppLock := TAppLock.Create;

  // Initialiser le stockage sécurisé
  FSecureStorage := TSecureStorage.Create;

  // Abonnement aux événements d'application
  Application.OnActivate := ApplicationEventHandler;
  Application.OnDeactivate := ApplicationEventHandler;

  // Masquer les données sensibles par défaut
  HideSensitiveData;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FAppLock.Free;
  FSecureStorage.Free;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  // Si l'application est verrouillée, demander l'authentification
  if FAppLock.IsLocked then
  begin
    case FAppLock.LockType of
      altPIN:
        // Afficher l'écran de saisie du PIN
        ShowPINDialog;

      altBiometric:
        // Lancer l'authentification biométrique
        StartBiometricAuth(OnBiometricResult);
    end;
  end;
end;

procedure TFormMain.ApplicationEventHandler(Sender: TObject; AEvent: TApplicationEvent);
begin
  case AEvent of
    TApplicationEvent.EnteredBackground:
      begin
        // Verrouiller l'application et masquer les données sensibles
        FAppLock.LockApplication;
        HideSensitiveData;
      end;

    TApplicationEvent.BecameActive:
      begin
        // L'authentification sera gérée dans FormActivate
      end;
  end;
end;

procedure TFormMain.btnLoginClick(Sender: TObject);
var
  Username, Password, StoredHash, NewHash: string;
begin
  Username := edtUsername.Text;
  Password := edtPassword.Text;

  // Validation des entrées
  if (Trim(Username) = '') or (Trim(Password) = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs');
    Exit;
  end;

  // Vérifier les identifiants
  if FSecureStorage.RetrieveSecurely('UserHash_' + Username, StoredHash) then
  begin
    // Calculer le hash du mot de passe entré
    NewHash := THashSHA2.GetHashString(Password + Username); // Sel = nom d'utilisateur

    if StoredHash = NewHash then
    begin
      lblStatus.Text := 'Connexion réussie';

      // Stockage sécurisé de la session
      FSecureStorage.StoreSecurely('CurrentUser', Username);

      // Afficher les données sensibles
      ShowSensitiveData;
    end
    else
    begin
      lblStatus.Text := 'Identifiants incorrects';
      // Logique de limitation des tentatives pourrait être ajoutée ici
    end;
  end
  else
  begin
    lblStatus.Text := 'Utilisateur inconnu';
  end;

  // Effacer le mot de passe de l'interface
  edtPassword.Text := '';
end;

procedure TFormMain.CheckDeviceSecurity;
begin
  if IsDeviceRooted then
  begin
    TDialogService.MessageDialog(
      'Attention : Votre appareil semble être rooté/jailbreaké. ' +
      'L''utilisation de cette application sur un appareil non sécurisé ' +
      'peut compromettre vos données personnelles.',
      TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
      TMsgDlgBtn.mbOK,
      0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrCancel then
          Application.Terminate;
      end
    );
  end;
end;

procedure TFormMain.HideSensitiveData;
begin
  // Masquer les informations sensibles
  edtUsername.Text := '';
  edtPassword.Text := '';
  lblStatus.Text := '';

  // D'autres contrôles sensibles seraient masqués ici
end;

procedure TFormMain.ShowSensitiveData;
var
  Username: string;
begin
  // Récupérer et afficher des informations non sensibles
  if FSecureStorage.RetrieveSecurely('CurrentUser', Username) then
    lblStatus.Text := 'Bienvenue, ' + Username;

  // D'autres données seraient chargées de manière sécurisée ici
end;

procedure TFormMain.OnBiometricResult(Success: Boolean; const ErrorMsg: string);
begin
  if Success then
  begin
    // Authentification biométrique réussie
    FAppLock.UnlockApplication;
    ShowSensitiveData;
  end
  else
  begin
    // Échec de l'authentification
    lblStatus.Text := 'Authentification échouée: ' + ErrorMsg;

    // Selon votre politique de sécurité, vous pourriez:
    // 1. Redemander l'authentification
    // 2. Basculer vers un autre mode d'authentification (PIN)
    // 3. Bloquer temporairement l'accès après plusieurs échecs
  end;
end;

end.
```

### Liste de contrôle de sécurité pour les applications mobiles

Avant de publier votre application, vérifiez les points suivants :

1. **Données sensibles**
   - [ ] Chiffrement de toutes les données sensibles stockées localement
   - [ ] Utilisation du stockage sécurisé spécifique à la plateforme quand c'est possible
   - [ ] Aucun mot de passe ou clé en dur dans le code

2. **Communications réseau**
   - [ ] Utilisation exclusive de HTTPS
   - [ ] Validation des certificats SSL
   - [ ] Mise en œuvre du certificate pinning pour les API critiques

3. **Authentification et autorisation**
   - [ ] Politique de mot de passe robuste
   - [ ] Verrouillage de l'application après inactivité
   - [ ] Gestion appropriée des sessions

4. **Code et données**
   - [ ] Validation de toutes les entrées utilisateur
   - [ ] Protection contre les injections SQL
   - [ ] Obfuscation du code pour les fonctionnalités sensibles
   - [ ] Suppression des informations de débogage en production

5. **Permissions**
   - [ ] Demande uniquement des permissions nécessaires
   - [ ] Explication claire à l'utilisateur de la raison de chaque permission
   - [ ] Gestion appropriée des refus de permission

6. **Sécurité de l'appareil**
   - [ ] Détection des appareils rootés/jailbreakés
   - [ ] Adaptation du niveau de sécurité selon l'état de l'appareil

7. **Protection contre les attaques**
   - [ ] Protection contre les attaques par force brute
   - [ ] Protection contre le débogage non autorisé
   - [ ] Protection contre les modifications de l'application (vérification d'intégrité)

8. **Journalisation et surveillance**
   - [ ] Journalisation sécurisée des événements critiques
   - [ ] Absence d'informations sensibles dans les journaux
   - [ ] Mécanisme de rapport d'incidents

### Protection contre le piratage et le reverse engineering

#### 1. Vérification de l'intégrité de l'application

Les applications mobiles peuvent être modifiées après leur installation. Une vérification d'intégrité permet de détecter ces modifications :

```pas
function VerifyAppIntegrity: Boolean;
var
  Installer: JPackageManager;
  AppInfo: JPackageInfo;
  AppSignature: TJavaObjectArray<JSignature>;
  Signature: JSignature;
begin
  Result := False;

  {$IF DEFINED(ANDROID)}
  try
    // Obtenir le gestionnaire de packages
    Installer := TAndroidHelper.Context.getPackageManager;

    // Obtenir les informations de l'application avec les signatures
    AppInfo := Installer.getPackageInfo(
      TAndroidHelper.Context.getPackageName,
      TJPackageManager.JavaClass.GET_SIGNATURES);

    // Obtenir les signatures
    AppSignature := AppInfo.signatures;

    if AppSignature.Length > 0 then
    begin
      Signature := TJSignature.Wrap(AppSignature.Items[0]);

      // Vérifier que la signature correspond à celle attendue
      // Note: Dans une application réelle, vous devez vérifier par rapport
      // à une valeur connue stockée de manière sécurisée
      Result := (Signature.hashCode = YOUR_EXPECTED_HASH_CODE);
    end;
  except
    Result := False;
  end;
  {$ELSEIF DEFINED(IOS)}
  // Sur iOS, le système d'exploitation assure déjà l'intégrité
  // des applications via le sandboxing et la signature de code
  Result := True;
  {$ELSE}
  // Implémentation pour d'autres plateformes
  Result := True;
  {$ENDIF}
end;

procedure TForm1.CheckAppIntegrity;
begin
  if not VerifyAppIntegrity then
  begin
    ShowMessage('Attention : L''intégrité de l''application semble compromise. ' +
                'Pour votre sécurité, l''application va se fermer.');
    Application.Terminate;
  end;
end;
```

#### 2. Protection contre le débogage

Le débogage non autorisé peut être utilisé pour analyser votre application. Vous pouvez détecter et prévenir ces tentatives :

```pas
function IsBeingDebugged: Boolean;
begin
  Result := False;

  {$IF DEFINED(ANDROID)}
  try
    // Vérifier si le débogueur est attaché via Android Debug Bridge
    var Process := TJProcessBuilder.JavaClass.init;
    Process.command(StringToJString('/system/bin/cat'),
                   StringToJString('/proc/self/status'));
    Process.redirectErrorStream(True);

    var P := Process.start;
    var Reader := TJBufferedReader.JavaClass.init(
      TJInputStreamReader.JavaClass.init(P.getInputStream));

    var Line: JString;
    while Reader.ready do
    begin
      Line := Reader.readLine;
      if Line.contains(StringToJString('TracerPid')) then
      begin
        // Si TracerPid n'est pas 0, l'application est en cours de débogage
        Line := Line.trim;
        var LastSpace := Line.lastIndexOf(StringToJString(' '));
        var TracerPid := Line.substring(LastSpace).trim;

        Result := not (JStringToString(TracerPid) = '0');
        Break;
      end;
    end;

    Reader.close;
    P.destroy;
  except
    Result := False;
  end;
  {$ELSEIF DEFINED(IOS)}
  // Code pour détecter le débogage sur iOS
  // Nécessite généralement du code Objective-C spécifique
  {$ELSE}
  // Implémentation pour d'autres plateformes
  {$ENDIF}
end;

procedure TForm1.CheckForDebugging;
begin
  if IsBeingDebugged then
  begin
    // En production, vous pourriez vouloir fermer l'application
    // ou désactiver les fonctionnalités sensibles
    ShowMessage('Débogage détecté. Les fonctionnalités sensibles seront désactivées.');
    DisableSensitiveFunctions;
  end;
end;
```

#### 3. Protection contre les émulateurs

Dans certains cas, vous pourriez vouloir détecter si votre application s'exécute sur un émulateur plutôt que sur un vrai appareil :

```pas
function IsRunningOnEmulator: Boolean;
begin
  Result := False;

  {$IF DEFINED(ANDROID)}
  try
    // Vérifier les propriétés du système qui sont typiques des émulateurs
    var TelephonyManager := TJTelephonyManager.Wrap(
      TAndroidHelper.Context.getSystemService(
        TJContext.JavaClass.TELEPHONY_SERVICE));

    // Vérifier l'IMEI - les émulateurs ont souvent des valeurs spécifiques
    var DeviceId := JStringToString(TelephonyManager.getDeviceId);

    // Vérifier le nom du modèle et du fabricant
    var Build := TJBuild.JavaClass;
    var Model := JStringToString(Build.MODEL);
    var Manufacturer := JStringToString(Build.MANUFACTURER);
    var Product := JStringToString(Build.PRODUCT);

    // La plupart des émulateurs ont des valeurs spécifiques pour ces propriétés
    Result := (Model.Contains('sdk')) or
              (Manufacturer.ToLower = 'genymotion') or
              (Product.Contains('sdk')) or
              (DeviceId = '000000000000000') or
              (DeviceId = '012345678912345');
  except
    Result := False;
  end;
  {$ELSEIF DEFINED(IOS)}
  // Code pour détecter les simulateurs iOS
  // Généralement en vérifiant la présence de certains chemins de fichiers
  {$ELSE}
  // Implémentation pour d'autres plateformes
  {$ENDIF}
end;

procedure TForm1.HandleEmulatorDetection;
begin
  if IsRunningOnEmulator then
  begin
    // Décider comment gérer l'exécution sur émulateur
    // Par exemple, limiter certaines fonctionnalités ou afficher un avertissement
    ShowMessage('Cette application s''exécute sur un émulateur. ' +
                'Certaines fonctionnalités peuvent être limitées.');
  end;
end;
```

### Implémentation de l'authentification biométrique

Les appareils modernes supportent l'authentification biométrique (empreinte digitale, reconnaissance faciale). Voici comment l'implémenter dans Delphi :

```pas
procedure TForm1.AuthenticateWithBiometrics;
{$IF DEFINED(ANDROID)}
var
  BiometricPrompt: JBiometricPrompt;
  Executor: JExecutor;
  PromptInfo: JBiometricPrompt_PromptInfo;
  AuthCallback: JBiometricPrompt_AuthenticationCallback;
begin
  // Créer un executor pour les callbacks
  Executor := TJMainThreadExecutor.Create;

  // Créer le callback d'authentification
  AuthCallback := TJBiometricCallback.Create(
    procedure(Result: Integer)
    begin
      case Result of
        0: // SUCCESS
          begin
            ShowMessage('Authentification réussie');
            // Débloquer les fonctionnalités sécurisées
          end;
        1: // ERROR
          ShowMessage('Erreur d''authentification');
        2: // FAILED
          ShowMessage('Authentification échouée');
      end;
    end
  );

  // Créer le BiometricPrompt
  BiometricPrompt := TJBiometricPrompt.JavaClass.init(
    TAndroidHelper.Activity,
    Executor,
    AuthCallback);

  // Configurer les options
  PromptInfo := TJBiometricPrompt_PromptInfo_Builder.JavaClass.init
    .setTitle(StringToJString('Authentification biométrique'))
    .setSubtitle(StringToJString('Veuillez utiliser votre empreinte digitale'))
    .setNegativeButtonText(StringToJString('Annuler'))
    .build;

  // Lancer l'authentification
  BiometricPrompt.authenticate(PromptInfo);
{$ELSEIF DEFINED(IOS)}
var
  Context: LAContext;
  Error: NSError;
  CanEvaluate: Boolean;
begin
  // Créer un contexte d'authentification
  Context := TLAContext.Create;

  // Vérifier si la biométrie est disponible
  CanEvaluate := Context.canEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, Error);

  if CanEvaluate then
  begin
    // Lancer l'authentification
    Context.evaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics,
      StrToNSStr('Veuillez vous authentifier avec Touch ID/Face ID'),
      procedure(success: Boolean; error: NSError)
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            if success then
            begin
              ShowMessage('Authentification réussie');
              // Débloquer les fonctionnalités sécurisées
            end
            else
            begin
              ShowMessage('Authentification échouée');
            end;
          end);
      end);
  end
  else
  begin
    ShowMessage('Biométrie non disponible sur cet appareil');
  end;
{$ELSE}
begin
  ShowMessage('Authentification biométrique non prise en charge sur cette plateforme');
{$ENDIF}
end;
```

> [!NOTE]
> **Compatibilité biométrique**
>
> L'authentification biométrique nécessite des API spécifiques à chaque plateforme. Sur Android 9 et supérieur, utilisez BiometricPrompt. Sur iOS, utilisez LocalAuthentication. Vérifiez toujours la disponibilité de la fonctionnalité avant de l'utiliser.

### Bonnes pratiques pour la protection des données sensibles en mémoire

Lorsque votre application traite des données sensibles, il est important de les protéger également en mémoire :

```pas
type
  // Classe pour gérer les chaînes sensibles
  TSensitiveString = class
  private
    FData: TBytes;
    FLength: Integer;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AsString: string read GetAsString write SetAsString;
  end;

constructor TSensitiveString.Create;
begin
  inherited Create;
  FLength := 0;
end;

destructor TSensitiveString.Destroy;
begin
  // Effacer les données sensibles avant libération
  Clear;
  inherited Destroy;
end;

procedure TSensitiveString.Clear;
var
  I: Integer;
begin
  // Remplir avec des zéros
  for I := 0 to Length(FData) - 1 do
    FData[I] := 0;

  SetLength(FData, 0);
  FLength := 0;
end;

function TSensitiveString.GetAsString: string;
begin
  if Length(FData) > 0 then
    Result := TEncoding.UTF8.GetString(FData)
  else
    Result := '';
end;

procedure TSensitiveString.SetAsString(const Value: string);
begin
  // Effacer les données précédentes
  Clear;

  if Value <> '' then
  begin
    // Stocker les nouvelles données
    FData := TEncoding.UTF8.GetBytes(Value);
    FLength := Length(Value);
  end;
end;

// Exemple d'utilisation
procedure TForm1.HandleSensitiveData;
var
  Password: TSensitiveString;
begin
  Password := TSensitiveString.Create;
  try
    // Stocker le mot de passe
    Password.AsString := edtPassword.Text;

    // Utiliser le mot de passe pour l'authentification
    // ...

    // Effacer immédiatement le champ de saisie
    edtPassword.Text := '';
  finally
    // La méthode Destroy appelera Clear
    Password.Free;
  end;
end;
```

### Vérifications de sécurité automatisées

Pour maintenir un niveau de sécurité élevé, vous pouvez intégrer des vérifications automatisées dans votre application :

```pas
unit SecurityChecks;

interface

uses
  System.SysUtils, System.Classes;

type
  TSecurityRisk = record
    Description: string;
    Severity: (rsLow, rsMedium, rsHigh, rsCritical);
    Recommendation: string;
  end;

  TSecurityRisks = TArray<TSecurityRisk>;

  TSecurityChecker = class
  public
    class function PerformSecurityChecks: TSecurityRisks;
  end;

implementation

// Implémentation des vérifications de sécurité
class function TSecurityChecker.PerformSecurityChecks: TSecurityRisks;
var
  Risks: TSecurityRisks;
begin
  SetLength(Risks, 0);

  // Vérifier si l'appareil est rooté/jailbreaké
  if IsDeviceRooted then
  begin
    SetLength(Risks, Length(Risks) + 1);
    Risks[High(Risks)].Description := 'Appareil rooté/jailbreaké détecté';
    Risks[High(Risks)].Severity := rsHigh;
    Risks[High(Risks)].Recommendation :=
      'L''utilisation de cet appareil peut compromettre la sécurité de vos données.';
  end;

  // Vérifier si les connexions réseau utilisent HTTPS
  if not AllNetworkConnectionsUseHTTPS then
  begin
    SetLength(Risks, Length(Risks) + 1);
    Risks[High(Risks)].Description := 'Connexions non sécurisées détectées';
    Risks[High(Risks)].Severity := rsCritical;
    Risks[High(Risks)].Recommendation :=
      'Toutes les communications doivent utiliser HTTPS.';
  end;

  // Vérifier si le stockage de données sensibles est chiffré
  if not IsStorageEncrypted then
  begin
    SetLength(Risks, Length(Risks) + 1);
    Risks[High(Risks)].Description := 'Stockage non chiffré détecté';
    Risks[High(Risks)].Severity := rsHigh;
    Risks[High(Risks)].Recommendation :=
      'Les données sensibles doivent être chiffrées avant stockage.';
  end;

  // Autres vérifications...

  Result := Risks;
end;

// Implémentations des fonctions de vérification...

end.
```

### Conclusion

La sécurité des applications mobiles est un domaine en constante évolution. Les développeurs Delphi doivent rester vigilants et adopter une approche proactive pour protéger leurs applications et les données des utilisateurs.

Les principales mesures à mettre en œuvre sont :

1. **Protection des données en transit** en utilisant HTTPS et le certificate pinning
2. **Protection des données au repos** via le chiffrement et le stockage sécurisé
3. **Authentification sécurisée** avec des mécanismes modernes comme la biométrie
4. **Validation des entrées et protection contre les injections**
5. **Détection des environnements non sécurisés** comme les appareils rootés
6. **Protection contre le débogage non autorisé et le reverse engineering**

En suivant ces bonnes pratiques et en effectuant des audits de sécurité réguliers, vous pouvez développer des applications mobiles Delphi qui offrent un niveau de sécurité adapté aux exigences modernes.

> [!TIP]
> **Ressources supplémentaires**
>
> - [OWASP Mobile Security Project](https://owasp.org/www-project-mobile-security/) - Guide de référence pour la sécurité mobile
> - [Embarcadero Documentation](https://docwiki.embarcadero.com/) - Documentation officielle Delphi
> - [FireMonkey Security Best Practices](https://blogs.embarcadero.com/) - Articles sur les meilleures pratiques de sécurité

---

> [!IMPORTANT]
> La sécurité est un processus continu, pas un état final. Restez informé des nouvelles vulnérabilités et mettez régulièrement à jour vos applications pour maintenir un niveau de sécurité optimal.
