🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.10 Sécurité des applications mobiles

## Introduction

Les applications mobiles présentent des défis de sécurité uniques. Contrairement aux applications desktop, elles s'exécutent sur des appareils personnels qui peuvent être facilement perdus, volés ou utilisés dans des environnements non sécurisés (WiFi public, etc.).

**Analogie du monde réel** : Un smartphone, c'est comme un portefeuille que vous transportez partout. Il contient vos photos, vos contacts, vos données bancaires, vos mots de passe. Si vous le perdez dans la rue, tous ces éléments deviennent potentiellement accessibles.

### Spécificités de la sécurité mobile

**Contraintes matérielles** :
- Ressources limitées (CPU, RAM, batterie)
- Écran tactile (risque de shoulder surfing)
- Caméra et microphone (risques de surveillance)
- GPS (traçage de localisation)

**Environnement d'exécution** :
- Appareil partagé ou prêté
- Connexions réseau variées (4G, WiFi public, etc.)
- Applications tierces potentiellement malveillantes
- Absence de contrôle physique de l'appareil

**Données sensibles** :
- Contacts et messages
- Photos et vidéos
- Localisation en temps réel
- Données biométriques
- Informations bancaires

### Différences iOS vs Android

| Aspect | iOS | Android |
|--------|-----|---------|
| **Modèle de sécurité** | Sandbox strict | Plus permissif |
| **Permissions** | Demandées à l'utilisation | Demandées à l'installation (ancien) ou utilisation (récent) |
| **Stockage sécurisé** | Keychain | KeyStore |
| **Fragmentation** | Faible (versions récentes) | Élevée (nombreuses versions) |
| **App Store** | Contrôle strict | Plus ouvert |
| **Root/Jailbreak** | Jailbreak rare | Root plus courant |

## Stockage sécurisé sur mobile

### iOS Keychain

Le Keychain est le système de stockage sécurisé d'iOS. Les données y sont chiffrées automatiquement.

```pascal
uses
  {$IFDEF IOS}
  iOSapi.Security, iOSapi.Foundation, Macapi.ObjectiveC,
  {$ENDIF}
  System.SysUtils;

{$IFDEF IOS}
type
  TKeychainHelper = class
  public
    class function Sauvegarder(const ACle, AValeur: string): Boolean;
    class function Charger(const ACle: string): string;
    class function Supprimer(const ACle: string): Boolean;
  end;

class function TKeychainHelper.Sauvegarder(const ACle, AValeur: string): Boolean;
var
  Query: NSMutableDictionary;
  Status: OSStatus;
  DataValue: NSData;
begin
  // Créer le dictionnaire de requête
  Query := TNSMutableDictionary.Create;

  // Type d'élément : mot de passe générique
  Query.setObject((kSecClassGenericPassword as ILocalObject).GetObjectID,
                  (kSecClass as ILocalObject).GetObjectID);

  // Identifiant de l'élément
  Query.setObject(StrToNSStr(ACle),
                  (kSecAttrAccount as ILocalObject).GetObjectID);

  // Supprimer l'ancien si existe
  SecItemDelete((Query as ILocalObject).GetObjectID);

  // Valeur à stocker
  DataValue := StrToNSStr(AValeur).dataUsingEncoding(NSUTF8StringEncoding);
  Query.setObject((DataValue as ILocalObject).GetObjectID,
                  (kSecValueData as ILocalObject).GetObjectID);

  // Ajouter au Keychain
  Status := SecItemAdd((Query as ILocalObject).GetObjectID, nil);

  Result := (Status = errSecSuccess);
end;

class function TKeychainHelper.Charger(const ACle: string): string;
var
  Query: NSMutableDictionary;
  Status: OSStatus;
  DataRef: Pointer;
  Data: NSData;
begin
  Result := '';

  Query := TNSMutableDictionary.Create;

  Query.setObject((kSecClassGenericPassword as ILocalObject).GetObjectID,
                  (kSecClass as ILocalObject).GetObjectID);
  Query.setObject(StrToNSStr(ACle),
                  (kSecAttrAccount as ILocalObject).GetObjectID);
  Query.setObject((kSecReturnData as ILocalObject).GetObjectID,
                  (kCFBooleanTrue as ILocalObject).GetObjectID);

  Status := SecItemCopyMatching((Query as ILocalObject).GetObjectID, @DataRef);

  if Status = errSecSuccess then
  begin
    Data := TNSData.Wrap(DataRef);
    Result := NSStrToStr(TNSString.Wrap(
      TNSString.Alloc.initWithData(Data, NSUTF8StringEncoding)));
  end;
end;

class function TKeychainHelper.Supprimer(const ACle: string): Boolean;
var
  Query: NSMutableDictionary;
  Status: OSStatus;
begin
  Query := TNSMutableDictionary.Create;

  Query.setObject((kSecClassGenericPassword as ILocalObject).GetObjectID,
                  (kSecClass as ILocalObject).GetObjectID);
  Query.setObject(StrToNSStr(ACle),
                  (kSecAttrAccount as ILocalObject).GetObjectID);

  Status := SecItemDelete((Query as ILocalObject).GetObjectID);

  Result := (Status = errSecSuccess);
end;

// Utilisation
procedure SauvegarderTokeniOS;
begin
  if TKeychainHelper.Sauvegarder('auth_token', 'eyJhbGciOiJIUzI1NiIs...') then
    ShowMessage('Token sauvegardé dans le Keychain')
  else
    ShowMessage('Erreur de sauvegarde');
end;

procedure ChargerTokeniOS;
var
  Token: string;
begin
  Token := TKeychainHelper.Charger('auth_token');
  if Token <> '' then
    ShowMessage('Token chargé : ' + Token)
  else
    ShowMessage('Token non trouvé');
end;
{$ENDIF}
```

### Android KeyStore

Le KeyStore Android fournit un stockage sécurisé similaire.

```pascal
uses
  {$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers, Androidapi.JNI.App,
  {$ENDIF}
  System.SysUtils;

{$IFDEF ANDROID}
type
  TAndroidKeyStore = class
  public
    class function Sauvegarder(const ACle, AValeur: string): Boolean;
    class function Charger(const ACle: string): string;
    class function Supprimer(const ACle: string): Boolean;
  end;

class function TAndroidKeyStore.Sauvegarder(const ACle, AValeur: string): Boolean;
var
  SharedPrefs: JSharedPreferences;
  Editor: JSharedPreferences_Editor;
begin
  try
    // Utiliser SharedPreferences en mode privé
    SharedPrefs := TAndroidHelper.Context.getSharedPreferences(
      StringToJString('SecureStorage'),
      TJContext.JavaClass.MODE_PRIVATE);

    Editor := SharedPrefs.edit;
    Editor.putString(StringToJString(ACle), StringToJString(AValeur));
    Editor.apply;

    Result := True;
  except
    Result := False;
  end;
end;

class function TAndroidKeyStore.Charger(const ACle: string): string;
var
  SharedPrefs: JSharedPreferences;
begin
  try
    SharedPrefs := TAndroidHelper.Context.getSharedPreferences(
      StringToJString('SecureStorage'),
      TJContext.JavaClass.MODE_PRIVATE);

    Result := JStringToString(SharedPrefs.getString(StringToJString(ACle),
                                                     StringToJString('')));
  except
    Result := '';
  end;
end;

class function TAndroidKeyStore.Supprimer(const ACle: string): Boolean;
var
  SharedPrefs: JSharedPreferences;
  Editor: JSharedPreferences_Editor;
begin
  try
    SharedPrefs := TAndroidHelper.Context.getSharedPreferences(
      StringToJString('SecureStorage'),
      TJContext.JavaClass.MODE_PRIVATE);

    Editor := SharedPrefs.edit;
    Editor.remove(StringToJString(ACle));
    Editor.apply;

    Result := True;
  except
    Result := False;
  end;
end;

// Utilisation
procedure SauvegarderTokenAndroid;
begin
  if TAndroidKeyStore.Sauvegarder('auth_token', 'eyJhbGciOiJIUzI1NiIs...') then
    ShowMessage('Token sauvegardé dans SharedPreferences')
  else
    ShowMessage('Erreur de sauvegarde');
end;
{$ENDIF}
```

### Classe unifiée multi-plateforme

```pascal
type
  TStockageSecurise = class
  public
    class function Sauvegarder(const ACle, AValeur: string): Boolean;
    class function Charger(const ACle: string): string;
    class function Supprimer(const ACle: string): Boolean;
  end;

class function TStockageSecurise.Sauvegarder(const ACle, AValeur: string): Boolean;
begin
  {$IFDEF IOS}
  Result := TKeychainHelper.Sauvegarder(ACle, AValeur);
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := TAndroidKeyStore.Sauvegarder(ACle, AValeur);
  {$ENDIF}

  {$IF NOT DEFINED(IOS) AND NOT DEFINED(ANDROID)}
  // Desktop : utiliser DPAPI ou autre
  Result := False;
  {$ENDIF}
end;

class function TStockageSecurise.Charger(const ACle: string): string;
begin
  {$IFDEF IOS}
  Result := TKeychainHelper.Charger(ACle);
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := TAndroidKeyStore.Charger(ACle);
  {$ENDIF}

  {$IF NOT DEFINED(IOS) AND NOT DEFINED(ANDROID)}
  Result := '';
  {$ENDIF}
end;

class function TStockageSecurise.Supprimer(const ACle: string): Boolean;
begin
  {$IFDEF IOS}
  Result := TKeychainHelper.Supprimer(ACle);
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := TAndroidKeyStore.Supprimer(ACle);
  {$ENDIF}

  {$IF NOT DEFINED(IOS) AND NOT DEFINED(ANDROID)}
  Result := False;
  {$ENDIF}
end;

// Utilisation simple et multi-plateforme
procedure ConfigurerApplicationMobile;
begin
  // Sauvegarder le token d'authentification
  TStockageSecurise.Sauvegarder('auth_token', TokenUtilisateur);

  // Sauvegarder les préférences sensibles
  TStockageSecurise.Sauvegarder('api_key', CleAPI);
end;

procedure ChargerConfiguration;
var
  Token: string;
begin
  Token := TStockageSecurise.Charger('auth_token');
  if Token <> '' then
    AuthentifierAvecToken(Token);
end;
```

## Authentification biométrique

### Touch ID / Face ID (iOS) et Empreinte digitale (Android)

```pascal
uses
  FMX.Platform, FMX.BiometricAuth, System.SysUtils;

type
  TAuthBiometrique = class
  private
    FBiometricAuth: TBiometricAuth;
  public
    constructor Create;
    destructor Destroy; override;
    function EstDisponible: Boolean;
    function TypeBiometrie: string;
    procedure Authentifier(const ARaison: string; ACallback: TProc<Boolean>);
  end;

constructor TAuthBiometrique.Create;
begin
  inherited Create;
  FBiometricAuth := TBiometricAuth.Create(nil);
end;

destructor TAuthBiometrique.Destroy;
begin
  FBiometricAuth.Free;
  inherited;
end;

function TAuthBiometrique.EstDisponible: Boolean;
begin
  Result := FBiometricAuth.BiometryType <> TBiometryType.None;
end;

function TAuthBiometrique.TypeBiometrie: string;
begin
  case FBiometricAuth.BiometryType of
    TBiometryType.None: Result := 'Aucune biométrie disponible';
    TBiometryType.FaceID: Result := 'Face ID';
    TBiometryType.TouchID: Result := 'Touch ID (Empreinte digitale)';
    TBiometryType.Fingerprint: Result := 'Empreinte digitale';
  else
    Result := 'Biométrie inconnue';
  end;
end;

procedure TAuthBiometrique.Authentifier(const ARaison: string; ACallback: TProc<Boolean>);
begin
  if not EstDisponible then
  begin
    ShowMessage('Authentification biométrique non disponible sur cet appareil');
    if Assigned(ACallback) then
      ACallback(False);
    Exit;
  end;

  FBiometricAuth.Authenticate(
    ARaison,
    procedure(const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
      begin
        ShowMessage('✓ Authentification réussie')
      end
      else
      begin
        ShowMessage('✗ Authentification échouée : ' + AError);
      end;

      if Assigned(ACallback) then
        ACallback(ASuccess);
    end
  );
end;

// Utilisation dans un formulaire
procedure TFormLogin.BtnBiometriqueClick(Sender: TObject);
var
  AuthBio: TAuthBiometrique;
begin
  AuthBio := TAuthBiometrique.Create;
  try
    // Vérifier la disponibilité
    if not AuthBio.EstDisponible then
    begin
      ShowMessage('Biométrie non disponible. Utilisez un mot de passe.');
      Exit;
    end;

    // Afficher le type
    LabelBiometrie.Text := 'Disponible : ' + AuthBio.TypeBiometrie;

    // Authentifier
    AuthBio.Authentifier(
      'Authentifiez-vous pour accéder à l''application',
      procedure(ASuccess: Boolean)
      begin
        if ASuccess then
        begin
          // Charger le token depuis le stockage sécurisé
          TokenUtilisateur := TStockageSecurise.Charger('auth_token');

          // Ouvrir l'application
          OuvrirApplicationPrincipale;
        end;
      end
    );
  finally
    AuthBio.Free;
  end;
end;

// Configuration des préférences
procedure TFormParametres.SwitchBiometrieSwitch(Sender: TObject);
var
  AuthBio: TAuthBiometrique;
begin
  AuthBio := TAuthBiometrique.Create;
  try
    if SwitchBiometrie.IsChecked then
    begin
      // Activer la biométrie
      if AuthBio.EstDisponible then
      begin
        AuthBio.Authentifier(
          'Configurez l''authentification biométrique',
          procedure(ASuccess: Boolean)
          begin
            if ASuccess then
            begin
              TStockageSecurise.Sauvegarder('biometrie_activee', 'true');
              ShowMessage('Biométrie activée');
            end
            else
            begin
              SwitchBiometrie.IsChecked := False;
            end;
          end
        );
      end
      else
      begin
        ShowMessage('Biométrie non disponible');
        SwitchBiometrie.IsChecked := False;
      end;
    end
    else
    begin
      // Désactiver la biométrie
      TStockageSecurise.Supprimer('biometrie_activee');
      ShowMessage('Biométrie désactivée');
    end;
  finally
    AuthBio.Free;
  end;
end;
```

## Gestion des permissions

### Demander les permissions au moment approprié

```pascal
uses
  FMX.MediaLibrary, FMX.Platform, System.Permissions;

type
  TGestionPermissions = class
  private
    FPermissionsService: IFMXPermissionsService;
    procedure PermissionRequestResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
  public
    constructor Create;
    procedure DemanderPermissionCamera(ACallback: TProc<Boolean>);
    procedure DemanderPermissionLocalisation(ACallback: TProc<Boolean>);
    procedure DemanderPermissionStockage(ACallback: TProc<Boolean>);
  end;

constructor TGestionPermissions.Create;
begin
  inherited Create;
  TPlatformServices.Current.SupportsPlatformService(
    IFMXPermissionsService,
    FPermissionsService);
end;

procedure TGestionPermissions.DemanderPermissionCamera(ACallback: TProc<Boolean>);
begin
  {$IFDEF ANDROID}
  FPermissionsService.RequestPermissions(
    ['android.permission.CAMERA'],
    procedure(const APermissions: TClassicStringDynArray;
              const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        if Assigned(ACallback) then
          ACallback(True);
      end
      else
      begin
        ShowMessage('Permission caméra refusée');
        if Assigned(ACallback) then
          ACallback(False);
      end;
    end
  );
  {$ENDIF}

  {$IFDEF IOS}
  // iOS demande automatiquement la permission
  // au premier accès à la caméra
  if Assigned(ACallback) then
    ACallback(True);
  {$ENDIF}
end;

procedure TGestionPermissions.DemanderPermissionLocalisation(ACallback: TProc<Boolean>);
begin
  {$IFDEF ANDROID}
  FPermissionsService.RequestPermissions(
    ['android.permission.ACCESS_FINE_LOCATION'],
    procedure(const APermissions: TClassicStringDynArray;
              const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        if Assigned(ACallback) then
          ACallback(True);
      end
      else
      begin
        ShowMessage('Permission localisation refusée');
        if Assigned(ACallback) then
          ACallback(False);
      end;
    end
  );
  {$ENDIF}

  {$IFDEF IOS}
  // iOS : configurer dans Info.plist
  // NSLocationWhenInUseUsageDescription
  if Assigned(ACallback) then
    ACallback(True);
  {$ENDIF}
end;

// Utilisation avec justification claire
procedure TFormCarte.BtnLocaliserClick(Sender: TObject);
var
  Permissions: TGestionPermissions;
begin
  Permissions := TGestionPermissions.Create;
  try
    // Expliquer POURQUOI on demande la permission
    ShowMessage('Cette application a besoin de votre localisation pour ' +
                'afficher les restaurants à proximité.');

    Permissions.DemanderPermissionLocalisation(
      procedure(AAccorde: Boolean)
      begin
        if AAccorde then
          AfficherCarteAvecLocalisation
        else
          AfficherCarteSansLocalisation;
      end
    );
  finally
    Permissions.Free;
  end;
end;
```

### Configuration des permissions dans les manifests

**AndroidManifest.xml** :
```xml
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
<uses-permission android:name="android.permission.CAMERA" />
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
```

**Info.plist (iOS)** :
```xml
<key>NSCameraUsageDescription</key>
<string>Cette application a besoin d'accéder à la caméra pour prendre des photos de vos reçus</string>

<key>NSLocationWhenInUseUsageDescription</key>
<string>Cette application utilise votre localisation pour trouver les restaurants à proximité</string>

<key>NSPhotoLibraryUsageDescription</key>
<string>Cette application a besoin d'accéder à vos photos pour vous permettre de les partager</string>
```

## Sécurité des communications

### Forcer HTTPS

```pascal
uses
  System.Net.HttpClient, System.SysUtils;

type
  TAPIMobile = class
  private
    FHTTPClient: THTTPClient;
    FBaseURL: string;
  public
    constructor Create(const ABaseURL: string);
    destructor Destroy; override;
    function Get(const AEndpoint: string): string;
    function Post(const AEndpoint, AData: string): string;
  end;

constructor TAPIMobile.Create(const ABaseURL: string);
begin
  inherited Create;

  // ✅ Forcer HTTPS
  if not ABaseURL.StartsWith('https://') then
    raise Exception.Create('HTTPS requis pour la sécurité');

  FBaseURL := ABaseURL;
  FHTTPClient := THTTPClient.Create;

  // Configurer des timeouts raisonnables
  FHTTPClient.ConnectionTimeout := 10000; // 10 secondes
  FHTTPClient.ResponseTimeout := 30000;   // 30 secondes
end;

destructor TAPIMobile.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TAPIMobile.Get(const AEndpoint: string): string;
var
  Response: IHTTPResponse;
begin
  try
    Response := FHTTPClient.Get(FBaseURL + AEndpoint);

    if Response.StatusCode = 200 then
      Result := Response.ContentAsString
    else
      raise Exception.CreateFmt('Erreur API : %d', [Response.StatusCode]);
  except
    on E: Exception do
    begin
      // Logger l'erreur mais ne pas exposer de détails à l'utilisateur
      TLogger.Instance.Error('Erreur API', E.Message);
      raise Exception.Create('Erreur de connexion au serveur');
    end;
  end;
end;

function TAPIMobile.Post(const AEndpoint, AData: string): string;
var
  Response: IHTTPResponse;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    FHTTPClient.ContentType := 'application/json';

    Response := FHTTPClient.Post(FBaseURL + AEndpoint, Stream);

    if Response.StatusCode in [200, 201] then
      Result := Response.ContentAsString
    else
      raise Exception.CreateFmt('Erreur API : %d', [Response.StatusCode]);
  finally
    Stream.Free;
  end;
end;

// Utilisation avec token d'authentification
procedure AppelerAPIAvecToken;
var
  API: TAPIMobile;
  Token: string;
  Response: string;
begin
  Token := TStockageSecurise.Charger('auth_token');

  API := TAPIMobile.Create('https://api.monapp.com');
  try
    // Ajouter le token dans les headers
    API.FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + Token;

    Response := API.Get('/user/profile');
    // Traiter la réponse...
  finally
    API.Free;
  end;
end;
```

### Certificate Pinning

Pour une sécurité maximale, épinglez le certificat SSL :

```pascal
type
  TAPIPinned = class(TAPIMobile)
  private
    const
      CERTIFICATE_FINGERPRINT = 'A1:B2:C3:D4:E5:F6:...'; // SHA-256 du certificat
  public
    constructor Create(const ABaseURL: string);
  end;

constructor TAPIPinned.Create(const ABaseURL: string);
begin
  inherited Create(ABaseURL);

  // Configuration du certificate pinning
  // (Implémentation spécifique selon la plateforme)

  {$IFDEF ANDROID}
  // Configurer TrustManager avec le certificat épinglé
  {$ENDIF}

  {$IFDEF IOS}
  // Configurer NSURLSession avec le certificat épinglé
  {$ENDIF}
end;
```

## Protection du code et obfuscation

### Techniques de protection

```pascal
// 1. Ne jamais inclure de secrets en dur
const
  // ❌ DANGEREUX
  API_KEY = 'sk_live_51Hxyz...';

  // ✅ BON - Charger depuis un serveur ou stockage sécurisé

// 2. Éviter le code sensible facilement décompilable
procedure ProtegerLogiqueCritique;
begin
  // Déplacer la logique critique côté serveur
  // Le mobile ne fait qu'afficher les résultats
end;

// 3. Vérifier l'intégrité de l'application
function VerifierIntegrite: Boolean;
begin
  // Vérifier que l'app n'a pas été modifiée
  // Détecter le jailbreak/root
  Result := not EstJailbreakOuRoot;
end;

// 4. Obscurcir les chaînes sensibles
function DecodeSecret(const AEncoded: string): string;
begin
  // Décoder une chaîne Base64 ou XOR au runtime
  Result := TNetEncoding.Base64.Decode(AEncoded);
end;
```

### Détection de jailbreak/root

```pascal
type
  TDetectionSecurite = class
  public
    class function EstJailbreak: Boolean;
    class function EstRoot: Boolean;
    class function EstEmulateur: Boolean;
  end;

{$IFDEF IOS}
class function TDetectionSecurite.EstJailbreak: Boolean;
var
  CheminsSuspects: array[0..3] of string;
  i: Integer;
begin
  Result := False;

  // Chemins typiques d'un appareil jailbreaké
  CheminsSuspects[0] := '/Applications/Cydia.app';
  CheminsSuspects[1] := '/private/var/lib/apt/';
  CheminsSuspects[2] := '/usr/sbin/sshd';
  CheminsSuspects[3] := '/usr/bin/ssh';

  for i := 0 to High(CheminsSuspects) do
  begin
    if FileExists(CheminsSuspects[i]) then
    begin
      Result := True;
      Break;
    end;
  end;

  // Test d'écriture dans un emplacement protégé
  if not Result then
  begin
    try
      if DirectoryExists('/private') then
        Result := True; // Ne devrait pas être accessible
    except
      // Exception = appareil normal
    end;
  end;
end;
{$ENDIF}

{$IFDEF ANDROID}
class function TDetectionSecurite.EstRoot: Boolean;
var
  CheminsSU: array[0..5] of string;
  i: Integer;
begin
  Result := False;

  // Chemins typiques pour les binaires SU (root)
  CheminsSU[0] := '/system/app/Superuser.apk';
  CheminsSU[1] := '/sbin/su';
  CheminsSU[2] := '/system/bin/su';
  CheminsSU[3] := '/system/xbin/su';
  CheminsSU[4] := '/data/local/xbin/su';
  CheminsSU[5] := '/data/local/bin/su';

  for i := 0 to High(CheminsSU) do
  begin
    if FileExists(CheminsSU[i]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;
{$ENDIF}

// Utilisation au démarrage
procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  if TDetectionSecurite.EstJailbreak or TDetectionSecurite.EstRoot then
  begin
    ShowMessage('AVERTISSEMENT : Appareil modifié détecté. ' +
                'Cette application pourrait ne pas fonctionner correctement ' +
                'ou compromettre la sécurité de vos données.');

    // Option : Bloquer l'application
    // Application.Terminate;
  end;
end;
```

## Gestion des données locales

### Chiffrer la base de données locale

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Option, FireDAC.Stan.Def;

procedure CreerBaseDonneesChiffree;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    // SQLite avec chiffrement
    Connection.DriverName := 'SQLite';

    {$IFDEF IOS}
    Connection.Params.Values['Database'] :=
      TPath.Combine(TPath.GetDocumentsPath, 'app.db');
    {$ENDIF}

    {$IFDEF ANDROID}
    Connection.Params.Values['Database'] :=
      TPath.Combine(TPath.GetDocumentsPath, 'app.db');
    {$ENDIF}

    // Activer le chiffrement avec une clé
    Connection.Params.Values['Encrypt'] := 'aes-256';
    Connection.Params.Values['Password'] := GenererCleChiffrement;

    Connection.Connected := True;

    // Créer les tables...
  finally
    Connection.Free;
  end;
end;

function GenererCleChiffrement: string;
var
  DeviceID: string;
begin
  // Générer une clé unique basée sur l'appareil
  {$IFDEF IOS}
  DeviceID := GetUniqueDeviceID_iOS;
  {$ENDIF}

  {$IFDEF ANDROID}
  DeviceID := GetUniqueDeviceID_Android;
  {$ENDIF}

  // Dériver une clé de chiffrement
  Result := THashSHA2.GetHashString(DeviceID + 'SECRET_SALT_12345');
end;
```

### Nettoyer les données sensibles

```pascal
type
  TNettoyageDonnees = class
  public
    class procedure EffacerCache;
    class procedure EffacerDonneesTemporaires;
    class procedure EffacerHistorique;
  end;

class procedure TNettoyageDonnees.EffacerCache;
var
  CheminCache: string;
begin
  {$IFDEF IOS}
  CheminCache := TPath.Combine(TPath.GetCachePath, '');
  {$ENDIF}

  {$IFDEF ANDROID}
  CheminCache := TPath.Combine(TPath.GetCachePath, '');
  {$ENDIF}

  if DirectoryExists(CheminCache) then
  begin
    TDirectory.Delete(CheminCache, True);
    ForceDirectories(CheminCache);
  end;
end;

class procedure TNettoyageDonnees.EffacerDonneesTemporaires;
var
  CheminTemp: string;
begin
  CheminTemp := TPath.GetTempPath;

  if DirectoryExists(CheminTemp) then
  begin
    TDirectory.Delete(CheminTemp, True);
    ForceDirectories(CheminTemp);
  end;
end;

// Nettoyer à la déconnexion
procedure TFormPrincipal.BtnDeconnexionClick(Sender: TObject);
begin
  // Supprimer le token
  TStockageSecurise.Supprimer('auth_token');

  // Nettoyer les données temporaires
  TNettoyageDonnees.EffacerCache;
  TNettoyageDonnees.EffacerDonneesTemporaires;

  // Retour à l'écran de connexion
  RetournerAuLogin;
end;
```

## Sécurité de l'interface utilisateur

### Empêcher les captures d'écran

```pascal
{$IFDEF ANDROID}
uses
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers;

procedure EmpecherCapturesEcran;
var
  Window: JWindow;
begin
  Window := TAndroidHelper.Activity.getWindow;
  Window.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_SECURE);
end;
{$ENDIF}

{$IFDEF IOS}
// iOS ne permet pas de bloquer les captures d'écran
// Mais on peut détecter quand une capture est prise
procedure DetecterCaptureEcran;
begin
  // S'abonner aux notifications de capture d'écran
  // et réagir (flouter l'écran, déconnecter, etc.)
end;
{$ENDIF}

// Appeler au démarrage des écrans sensibles
procedure TFormDonneesBancaires.FormShow(Sender: TObject);
begin
  {$IFDEF ANDROID}
  EmpecherCapturesEcran;
  {$ENDIF}
end;
```

### Masquer le contenu dans le sélecteur d'apps

```pascal
procedure TFormPrincipal.FormDeactivate(Sender: TObject);
begin
  // Masquer le contenu sensible quand l'app passe en arrière-plan
  PanelDonneesSensibles.Visible := False;
  ImageOverlay.Visible := True; // Afficher un écran de veille
end;

procedure TFormPrincipal.FormActivate(Sender: TObject);
begin
  // Réafficher le contenu après vérification
  ImageOverlay.Visible := False;

  // Optionnel : redemander l'authentification
  if DemanderAuthApresBackground then
    DemanderAuthentificationBiometrique;
end;
```

### Timeout de session

```pascal
type
  TGestionSession = class
  private
    FDerniereActivite: TDateTime;
    FTimeoutMinutes: Integer;
    FTimer: TTimer;
    procedure TimerTick(Sender: TObject);
  public
    constructor Create(ATimeoutMinutes: Integer);
    destructor Destroy; override;
    procedure ResetTimeout;
    procedure Deconnecter;
  end;

constructor TGestionSession.Create(ATimeoutMinutes: Integer);
begin
  inherited Create;
  FTimeoutMinutes := ATimeoutMinutes;
  FDerniereActivite := Now;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 60000; // Vérifier chaque minute
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := True;
end;

destructor TGestionSession.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TGestionSession.TimerTick(Sender: TObject);
var
  MinutesInactif: Integer;
begin
  MinutesInactif := MinutesBetween(Now, FDerniereActivite);

  if MinutesInactif >= FTimeoutMinutes then
  begin
    FTimer.Enabled := False;
    Deconnecter;
  end;
end;

procedure TGestionSession.ResetTimeout;
begin
  FDerniereActivite := Now;
end;

procedure TGestionSession.Deconnecter;
begin
  // Supprimer les données sensibles
  TStockageSecurise.Supprimer('auth_token');

  // Retour au login
  ShowMessage('Session expirée. Veuillez vous reconnecter.');
  Application.MainForm := FormLogin;
  FormLogin.Show;
end;

// Utilisation dans les formulaires
var
  GestionSession: TGestionSession;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  GestionSession := TGestionSession.Create(15); // 15 minutes
end;

procedure TFormPrincipal.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  // Réinitialiser le timeout à chaque interaction
  GestionSession.ResetTimeout;
end;

procedure TFormPrincipal.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  GestionSession.ResetTimeout;
end;
```

## Checklist de sécurité mobile

### Avant la publication

**Stockage des données** :
- [ ] Utiliser Keychain (iOS) / KeyStore (Android) pour les données sensibles
- [ ] Chiffrer la base de données locale
- [ ] Ne jamais stocker de mots de passe en clair
- [ ] Nettoyer les données temporaires

**Authentification** :
- [ ] Implémenter l'authentification biométrique
- [ ] Timeout de session configuré
- [ ] Déconnexion automatique après inactivité
- [ ] Bloquer après plusieurs échecs de connexion

**Communications** :
- [ ] Forcer HTTPS partout
- [ ] Certificate pinning pour les APIs critiques
- [ ] Valider les certificats SSL
- [ ] Timeouts réseau configurés

**Permissions** :
- [ ] Demander uniquement les permissions nécessaires
- [ ] Expliquer pourquoi chaque permission est nécessaire
- [ ] Fonctionnalités dégradées si permissions refusées
- [ ] Manifests configurés correctement

**Interface utilisateur** :
- [ ] Masquer contenu sensible en arrière-plan
- [ ] Bloquer captures d'écran sur écrans sensibles
- [ ] Validation côté client ET serveur
- [ ] Messages d'erreur génériques

**Code** :
- [ ] Aucun secret en dur dans le code
- [ ] Détection de jailbreak/root
- [ ] Code critique côté serveur
- [ ] Obfuscation si nécessaire

**Tests** :
- [ ] Tests sur appareils réels
- [ ] Tests sur iOS et Android
- [ ] Tests avec jailbreak/root
- [ ] Tests de pénétration mobile

### Pendant l'exploitation

- [ ] Monitoring des crashs et erreurs
- [ ] Analyse des logs de sécurité
- [ ] Mises à jour régulières
- [ ] Rotation des clés API
- [ ] Révocation de tokens compromis

## Bonnes pratiques

### ✅ À faire

**1. Utiliser le stockage natif sécurisé**
```pascal
// ✅ BON
TStockageSecurise.Sauvegarder('token', Token);

// ❌ MAUVAIS
TFile.WriteAllText('token.txt', Token); // Fichier non chiffré
```

**2. Authentification biométrique pour les apps sensibles**
```pascal
// Applications bancaires, médicales, etc.
AuthentifierAvecBiometrie;
```

**3. Valider côté serveur**
```pascal
// Ne JAMAIS faire confiance au client
// Toujours revalider côté serveur
```

**4. Minimiser les permissions**
```pascal
// Demander uniquement ce qui est strictement nécessaire
```

**5. HTTPS obligatoire**
```pascal
if not URL.StartsWith('https://') then
  raise Exception.Create('HTTPS requis');
```

### ❌ À éviter

**1. Secrets en dur**
```pascal
// ❌ JAMAIS
const API_KEY = 'sk_live_...';
```

**2. Ignorer les appareils modifiés**
```pascal
// ❌ Les apps sensibles doivent détecter jailbreak/root
```

**3. Stocker des données sensibles sans chiffrement**
```pascal
// ❌ Fichiers texte, SharedPreferences non chiffré
```

**4. Faire confiance au client**
```pascal
// ❌ Logique critique côté mobile
// ✅ Logique critique côté serveur
```

## Résumé des points essentiels

✅ **Principes fondamentaux** :
- Utiliser le stockage sécurisé natif (Keychain/KeyStore)
- Authentification biométrique pour les apps sensibles
- HTTPS obligatoire, certificate pinning recommandé
- Permissions minimales avec justifications claires
- Détection de jailbreak/root pour apps critiques
- Timeout de session automatique

🔒 **Données sensibles** :
- Chiffrer la base de données locale
- Ne jamais stocker de secrets en clair
- Nettoyer les données temporaires régulièrement
- Masquer le contenu en arrière-plan
- Empêcher captures d'écran sur écrans sensibles

📱 **Spécificités mobiles** :
- iOS : Keychain, Touch ID/Face ID, Info.plist
- Android : KeyStore, Empreinte, Manifeste
- Multi-plateforme : Abstraire les différences
- Appareil physique : Perte, vol, partage

⚠️ **Erreurs critiques** :
- Secrets en dur dans le code
- HTTP au lieu de HTTPS
- Pas de timeout de session
- Stockage non chiffré
- Permissions excessives
- Ignorer jailbreak/root

## Ressources utiles

**Documentation officielle** :
- Apple Security : https://developer.apple.com/security/
- Android Security : https://developer.android.com/security
- OWASP Mobile Top 10 : https://owasp.org/www-project-mobile-top-10/

**Outils de test** :
- MobSF (Mobile Security Framework)
- Frida (Dynamic instrumentation)
- Objection (Runtime mobile exploration)

**Standards** :
- OWASP MASVS (Mobile Application Security Verification Standard)
- NIST Mobile Security Guidelines

La sécurité mobile nécessite une approche spécifique prenant en compte les contraintes et risques uniques des appareils mobiles. Protégez vos utilisateurs en appliquant ces principes dès la conception de votre application.

⏭️ [Distribution et déploiement](/17-distribution-et-deploiement/README.md)
