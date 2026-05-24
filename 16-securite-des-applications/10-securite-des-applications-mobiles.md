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
| **Permissions** | Demandées à l'utilisation | Demandées à l'utilisation (depuis Android 6.0 / 2015) |
| **Stockage sécurisé** | Keychain (Secure Enclave sur iPhone 5s+) | KeyStore + StrongBox sur les appareils compatibles |
| **Chiffrement disque** | Intégral par défaut depuis iOS 8 (2014) | Intégral par défaut depuis Android 10 (2019) |
| **Fragmentation** | Faible (versions récentes) | Élevée (nombreuses versions) |
| **App Store** | Contrôle strict | Plus ouvert (sideloading possible) |
| **Root/Jailbreak** | Jailbreak rare | Root plus courant |
| **Attestation d'intégrité** | DeviceCheck, App Attest (iOS 14+) | Play Integrity API (remplace SafetyNet, déprécié 2024) |
| **Auto-révocation permissions** | Apps non utilisées (iOS 12+) | Apps non utilisées depuis 3 mois (Android 11+) |

### Référentiels OWASP pour le mobile

Deux documents de référence à connaître :

- **OWASP Mobile Top 10 (2024)** : équivalent mobile du Top 10 web. Catégories principales : *Improper Credential Usage*, *Inadequate Supply Chain Security*, *Insecure Authentication/Authorization*, *Insufficient Input/Output Validation*, *Insecure Communication*, *Inadequate Privacy Controls*, *Insufficient Binary Protections*, *Security Misconfiguration*, *Insecure Data Storage*, *Insufficient Cryptography*.
- **OWASP MASVS** (*Mobile Application Security Verification Standard*) : référentiel de vérification en 3 niveaux (L1 standard, L2 défense en profondeur, R protection contre rétro-ingénierie). Sert de checklist d'audit. Accompagné du MASTG (*Mobile Application Security Testing Guide*) qui détaille les techniques de test.

> 💡 **Pour une app traitant des données sensibles** (banque, santé, identité), viser la conformité MASVS-L2 + MASVS-R est un standard de marché en 2026.

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
  // ⚠ NE JAMAIS afficher la valeur d'un token dans une boîte de dialogue
  //   ou un log : un screenshot, un screen-recording, un crash report
  //   ou un peer regardant l'écran peut alors le récupérer trivialement.
  //   L'utilisateur n'a pas besoin de voir son token — juste de savoir
  //   si l'app a accès à un compte valide.
  if Token <> '' then
    ShowMessage('Session restaurée.')
  else
    ShowMessage('Aucune session sauvegardée. Veuillez vous reconnecter.');
end;
{$ENDIF}
```

### Android KeyStore

Le KeyStore Android (`android.security.keystore.KeyStore`) fournit un stockage matériel des **clés cryptographiques**, mais il ne stocke pas directement vos *valeurs*. Pour stocker un secret sur Android, on combine :
1. une clé AES générée et conservée dans le KeyStore (idéalement adossée au TEE/StrongBox) ;
2. la valeur chiffrée par cette clé puis écrite dans `SharedPreferences` ou un fichier.

La bibliothèque AndroidX `androidx.security:security-crypto` (`EncryptedSharedPreferences`, `MasterKey`) automatise ce processus.

> ⚠️ **L'exemple ci-dessous N'utilise PAS le KeyStore** — il se contente d'écrire dans `SharedPreferences` en mode `MODE_PRIVATE`. C'est suffisant contre une autre application sur un appareil **non rooté**, mais sur un appareil rooté le fichier `/data/data/<package>/shared_prefs/SecureStorage.xml` est **lisible en clair**. Pour un vrai stockage sécurisé, il faut interfacer Delphi avec `EncryptedSharedPreferences` (JNI) ou chiffrer la valeur côté Delphi (AES-256-GCM, voir 16.3) avec une clé conservée dans `KeyStore`.

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
  // ⚠ `try..except Result := False end` est un anti-pattern : il masque
  //   TOUTES les exceptions sans laisser de trace. Le bug devient
  //   invisible (mauvaise clé, OOM, permission refusée...). En production,
  //   logger l'erreur avec sa classe et son message via `TLogger`, ou
  //   au minimum re-lever pour ne pas perdre l'information.
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
    on E: Exception do
    begin
      TLogger.Instance.Error('SharedPrefs.Sauvegarder',
                             Format('Classe: %s, Message: %s',
                                    [E.ClassName, E.Message]));
      Result := False;
    end;
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

> ⚠️ **L'unité `FMX.BiometricAuth` et la classe `TBiometricAuth` utilisées ci-dessous ne font PAS partie de la RTL FMX standard de Delphi 13** — elles n'apparaissent que dans certains composants tiers. La voie supportée en 2026 passe par les APIs natives :  
> - **iOS** : `LocalAuthentication.LAContext` (`evaluatePolicy:LAPolicyDeviceOwnerAuthenticationWithBiometrics`) via les bindings `iOSapi.LocalAuthentication`.  
> - **Android** : `BiometricPrompt` du package `androidx.biometric` (recommandé depuis Android 9), interfacé via JNI.  
>  
> L'exemple ci-dessous décrit l'**API qu'aurait** un wrapper équivalent ; remplacez `TBiometricAuth` par votre propre wrapper ou par celui d'un composant tiers (TMS FNC Mobile, Konopka Signature VCL, FGX Native).

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

    // ⚠ PIÈGE D'OBJET ASYNCHRONE : `Authentifier` lance une opération sur
    //   le système d'exploitation et retourne IMMÉDIATEMENT. Le `finally
    //   AuthBio.Free` ci-dessous libère `AuthBio` AVANT que la callback ne
    //   s'exécute → use-after-free quand la callback accède aux champs
    //   du wrapper.
    //
    //   Solutions :
    //   1. Faire de `AuthBio` un membre `private` du form, libéré dans
    //      `FormDestroy` (recommandé).
    //   2. Capturer `AuthBio` dans la closure et libérer DANS la callback :
    AuthBio.Authentifier(
      'Authentifiez-vous pour accéder à l''application',
      procedure(ASuccess: Boolean)
      begin
        try
          if ASuccess then
          begin
            // Charger le token depuis le stockage sécurisé
            TokenUtilisateur := TStockageSecurise.Charger('auth_token');

            // Ouvrir l'application
            OuvrirApplicationPrincipale;
          end;
        finally
          AuthBio.Free;   // libération différée jusqu'au retour de l'OS
        end;
      end
    );
    // ⚠ NE PAS faire `AuthBio.Free` ici : on transfère la responsabilité
    //   à la callback ci-dessus.
  except
    // Si une exception survient AVANT que la callback ne soit programmée,
    // c'est nous qui libérons.
    AuthBio.Free;
    raise;
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
  System.Permissions  // PermissionsService (variable globale)
  {$IFDEF ANDROID}, Androidapi.Helpers, Androidapi.JNI.Os {$ENDIF};

type
  TGestionPermissions = class
  public
    procedure DemanderPermissionCamera(ACallback: TProc<Boolean>);
    procedure DemanderPermissionLocalisation(ACallback: TProc<Boolean>);
    procedure DemanderPermissionStockage(ACallback: TProc<Boolean>);
  end;

// 💡 Depuis Delphi 10.3, on accède au service via la variable globale
//   `PermissionsService` de `System.Permissions`. Plus besoin de passer
//   par `TPlatformServices.Current.SupportsPlatformService` ni de garder
//   une référence à `IFMXPermissionsService`.

procedure TGestionPermissions.DemanderPermissionCamera(ACallback: TProc<Boolean>);  
begin  
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions(
    ['android.permission.CAMERA'],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
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
  PermissionsService.RequestPermissions(
    ['android.permission.ACCESS_FINE_LOCATION'],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
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
<!-- Permissions générales -->
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
<uses-permission android:name="android.permission.CAMERA" />

<!-- Accès au stockage : les règles ont changé. -->
<!-- ⚠ Depuis Android 11 (API 30, 2020), READ/WRITE_EXTERNAL_STORAGE -->
<!--    ne donnent plus accès au stockage partagé pour les apps ciblant -->
<!--    API ≥ 30. Il faut utiliser MediaStore + le Storage Access Framework. -->
<!-- ⚠ Depuis Android 13 (API 33, 2022), READ_EXTERNAL_STORAGE est -->
<!--    remplacée par des permissions GRANULAIRES par type de média : -->
<uses-permission android:name="android.permission.READ_MEDIA_IMAGES"
                 android:maxSdkVersion="34" />
<uses-permission android:name="android.permission.READ_MEDIA_VIDEO"
                 android:maxSdkVersion="34" />
<uses-permission android:name="android.permission.READ_MEDIA_AUDIO"
                 android:maxSdkVersion="34" />

<!-- ⚠ Depuis Android 14 (API 34, 2023), nouvelle permission pour -->
<!--    accéder UNIQUEMENT à des médias sélectionnés par l'utilisateur -->
<!--    via le photo picker, sans accéder à toute la galerie : -->
<uses-permission android:name="android.permission.READ_MEDIA_VISUAL_USER_SELECTED" />

<!-- Conservés pour API ≤ 28 (les anciennes versions Android) -->
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE"
                 android:maxSdkVersion="32" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"
                 android:maxSdkVersion="28" />
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
  // ⚠ `StartsWith('https://')` est sensible à la casse par défaut :
  //   `HTTPS://api...` serait refusé alors qu'il est valide. Utiliser
  //   le second paramètre `IgnoreCase = True`. Et tester que l'URL ne
  //   contient pas d'autres caractères suspects (ex : `https://attaquant@api...`
  //   utilise la syntaxe userinfo de l'URI pour tromper l'utilisateur).
  if not ABaseURL.StartsWith('https://', True) then
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
    // ⚠ `FHTTPClient` est déclaré `private` dans la classe ci-dessus :
    //   pour permettre cet accès, il faudrait exposer une propriété
    //   `property AuthToken: string read GetAuthToken write SetAuthToken`
    //   qui encapsule la manipulation du header. Cet exemple suppose
    //   cette propriété ajoutée.
    API.AuthToken := Token;

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
  // ⚠ « Obscurcir » N'EST PAS « sécuriser ». Tout outil de rétro-ingénierie
  //   (IDA, Ghidra, Hopper) repère facilement les chaînes Base64 dans le
  //   binaire et les décode. Un secret « obscurci » est un secret en clair
  //   avec une étape supplémentaire d'environ 2 secondes de travail.
  //
  //   L'obscurcissement n'a de sens que comme RALENTISSEUR pour les
  //   attaquants opportunistes ; pour un vrai secret, utilisez :
  //   - un appel serveur à chaque démarrage (le binaire ne contient JAMAIS
  //     la valeur, seulement le mécanisme d'authentification au serveur) ;
  //   - le Keychain/KeyStore après une première phase d'enrôlement ;
  //   - un certificat client (mTLS) plutôt qu'une clé API.
  Result := TNetEncoding.Base64.Decode(AEncoded);
end;
```

### Détection de jailbreak/root

> ⚠️ **Limites de la détection** : tous les contrôles montrés ci-dessous (présence de `Cydia.app`, d'un binaire `su`, etc.) sont **trivialement contournables** par des outils publics (Liberty Lite, Shadow, Magisk Hide, objection, Frida). Considérez-les comme une **mesure de défense en profondeur** qui élève la barre pour les attaquants opportunistes, **jamais comme une garantie**. Une application traitant de données très sensibles doit en plus :  
> - exécuter les contrôles critiques côté serveur (SafetyNet/Play Integrity API pour Android, DeviceCheck/App Attest pour iOS) ;  
> - utiliser un *runtime application self-protection* (RASP) si le risque le justifie ;  
> - et accepter qu'un attaquant local et déterminé finira par contourner ces contrôles.

```pascal
type
  TDetectionSecurite = class
  public
    // ⚠ Les méthodes ci-dessous sont déclarées sur toutes les plateformes
    //   pour permettre la compilation cross-platform. Sur les plateformes
    //   où la notion n'a pas de sens (jailbreak sur desktop, root sur iOS),
    //   elles retournent simplement False.
    class function EstJailbreak: Boolean;
    class function EstRoot: Boolean;
    class function EstEmulateur: Boolean;
  end;

class function TDetectionSecurite.EstJailbreak: Boolean;
{$IFDEF IOS}
var
  CheminsSuspects: array[0..3] of string;
  i: Integer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF IOS}
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
  {$ENDIF}
end;

class function TDetectionSecurite.EstRoot: Boolean;
{$IFDEF ANDROID}
var
  CheminsSU: array[0..5] of string;
  i: Integer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF ANDROID}
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
  {$ENDIF}
end;

class function TDetectionSecurite.EstEmulateur: Boolean;  
begin  
  Result := False;
  // Implémentation possible : sur Android, lire Build.FINGERPRINT et
  // chercher 'generic', 'sdk', 'emulator' ; sur iOS, processeur arm64
  // émulé sur x86_64 (rare car simulator est différent).
end;

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

    // `TPath.GetDocumentsPath` retourne le bon dossier sur chaque plateforme
    // (Documents/ sandbox sur iOS, /data/data/<pkg>/files/ sur Android,
    // ~/Documents sur desktop). Pas besoin de différencier IOS/ANDROID ici.
    Connection.Params.Values['Database'] :=
      TPath.Combine(TPath.GetDocumentsPath, 'app.db');

    // Activer le chiffrement avec une clé (nécessite le driver SQLCipher
    // de FireDAC ; le SQLite « ordinaire » embarqué dans la RTL ne fait
    // PAS de chiffrement). Procurez-vous le binaire SQLCipher et configurez
    // la propriété `EngineHandle` ou `VendorLib` pour le pointer.
    Connection.Params.Values['Encrypt'] := 'aes-256';
    Connection.Params.Values['Password'] := GenererCleChiffrement;

    Connection.Connected := True;

    // Créer les tables...
  finally
    Connection.Free;
  end;
end;

// ⚠ Question essentielle : OÙ stocker la clé SQLCipher ?
//   - Si on la dérive d'un mot de passe utilisateur (Argon2id/PBKDF2),
//     l'utilisateur doit le ressaisir à chaque démarrage — c'est le
//     modèle des apps de gestion de mots de passe (1Password, Bitwarden).
//   - Si on veut un déblocage automatique au démarrage, stocker la clé
//     dans le Keychain (iOS) / KeyStore (Android) AVEC une contrainte
//     d'accès biométrique : ainsi la clé est inaccessible sans le doigt
//     ou le visage de l'utilisateur, même si l'appareil est rooté.
//   - JAMAIS dériver la clé d'un identifiant de device : ils changent
//     (réinstallation, factory reset) et l'utilisateur perd ses données.

function GenererCleChiffrement: string;  
var  
  DeviceID: string;
  Sel: TBytes;
  CleBin: TBytes;
const
  // ⚠️ Ce sel ne doit PAS être codé en dur : tout attaquant qui décompile
  // l'APK/IPA voit la chaîne. Il faut générer un sel aléatoire (CSPRNG) au
  // premier lancement et le conserver dans le Keychain (iOS) / KeyStore
  // (Android). Cette constante n'est gardée que pour montrer la STRUCTURE.
  SEL_DEMO: array[0..15] of Byte =
    ($A0, $11, $C2, $33, $44, $55, $66, $77,
     $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);
  PBKDF2_ITERATIONS = 600000;   // OWASP 2023+
  PBKDF2_KEY_BYTES  = 32;       // 256 bits pour AES-256
begin
  // ⚠️ `GetUniqueDeviceID_iOS` n'existe pas en tant que tel : Apple a retiré
  // l'accès au UDID en iOS 7 (2013). L'`identifierForVendor` (iOSapi.UIKit :
  // [TUIDevice currentDevice].identifierForVendor) change si l'utilisateur
  // désinstalle puis réinstalle l'app — il ne s'agit donc PAS d'un identifiant
  // matériel persistant.
  //
  // Sur Android, `Settings.Secure.ANDROID_ID` change selon la signature de
  // l'app et l'utilisateur (depuis Android 8.0). Il ne doit pas servir de
  // secret cryptographique.
  //
  // Pour une vraie clé de chiffrement local, préférez :
  //   - une clé AES générée par le KeyStore matériel (TEE/StrongBox)
  //     adossée à l'authentification biométrique ;
  //   - ou une clé dérivée d'un mot de passe utilisateur via PBKDF2/Argon2id.
  {$IFDEF IOS}
  DeviceID := GetUniqueDeviceID_iOS;       // wrapper à fournir
  {$ENDIF}
  {$IFDEF ANDROID}
  DeviceID := GetUniqueDeviceID_Android;   // wrapper à fournir
  {$ENDIF}

  // ✅ Dérivation via PBKDF2-HMAC-SHA-256 — SHA-256 SEUL n'est PAS une KDF :
  // il est rapide et permet une attaque par force brute massive sur GPU.
  // PBKDF2 / Argon2id ralentissent les attaquants en imposant un coût.
  SetLength(Sel, Length(SEL_DEMO));
  Move(SEL_DEMO[0], Sel[0], Length(SEL_DEMO));

  CleBin := THashPBKDF2_SHA256.GetHashBytes(
    DeviceID, Sel, PBKDF2_ITERATIONS, PBKDF2_KEY_BYTES);

  Result := TNetEncoding.Base64.EncodeBytesToString(CleBin);
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
  // `TPath.GetCachePath` retourne déjà le chemin complet — pas besoin de
  // `TPath.Combine(..., '')` qui ne fait qu'ajouter un séparateur final.
  CheminCache := TPath.GetCachePath;

  // ⚠ Supprimer puis recréer le dossier root du cache peut casser des
  //   fichiers de cache utilisés par d'autres bibliothèques (WebView,
  //   images cachées par TBitmap, etc.). Préférer parcourir et supprimer
  //   uniquement vos propres sous-dossiers (`MonApp/Sensible/`) plutôt
  //   que tout `TPath.GetCachePath`.
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
  // ⚠ Idem : `TPath.GetTempPath` est partagé. Sur iOS c'est le dossier `tmp/`
  //   du sandbox de l'app — supprimer son contenu en bloc peut interférer
  //   avec d'autres composants. À utiliser avec parcimonie.
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
// ⚠ iOS ne permet PAS de bloquer les captures d'écran (contrairement à
//   Android avec FLAG_SECURE). Pour empêcher la capture, il faut masquer
//   les vues sensibles AVANT que la capture soit prise — ce qui est
//   impossible sans préavis.
//
// En revanche, iOS notifie l'application APRÈS une capture via :
//   - `UIApplicationUserDidTakeScreenshotNotification` (iOS 7+)
//   - `screenIsCapturedDidChangeNotification` (iOS 11+) pour l'enregistrement
//     d'écran ET le miroir d'écran (Screen Mirroring).
//
// Réactions possibles à la capture : afficher un message d'avertissement
// à l'utilisateur, journaliser l'événement côté serveur, ou (apps bancaires
// strictes) déconnecter immédiatement.
procedure SAbonnerNotificationsCapture;  
begin  
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter)
    .addObserver(SelfAsNSObject, sel_getUid('captureDetectee:'),
                 StrToNSStr('UIApplicationUserDidTakeScreenshotNotification'),
                 nil);
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
  // ⚠ `Now` reflète l'horloge système, qui peut être modifiée par
  //   l'utilisateur (manuellement ou via un changement de fuseau).
  //   Un attaquant local pourrait reculer l'horloge pour empêcher
  //   l'expiration de session.
  //
  //   Pour un compteur d'inactivité, préférer `TThread.GetTickCount64`
  //   qui est **monotone** (incrémente toujours, ignore les modifications
  //   d'horloge). On comparerait alors `GetTickCount64 - FTickDerniereActivite`
  //   en millisecondes.
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

// ⚠ Cette variable globale doit être créée DANS FormCreate ET libérée
//   DANS FormDestroy. Sans le Free correspondant, on a une fuite mémoire
//   et le TTimer survit à la fermeture du formulaire.
procedure TFormPrincipal.FormCreate(Sender: TObject);  
begin  
  GestionSession := TGestionSession.Create(15); // 15 minutes
end;

// ⚠ Sur mobile, il n'y a pas d'événement `MouseMove` — la VCL utilise
//   `MouseMove` mais FireMonkey génère des `Tap`, `Touch` ou `Gesture`.
//   Pour FMX, brancher l'écoute sur les événements tactiles globaux :
procedure TFormPrincipal.FormTouch(Sender: TObject; const Touches: TTouches;
                                    const Action: TTouchAction);
begin
  GestionSession.ResetTimeout;
end;

procedure TFormPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
                                     var KeyChar: Char; Shift: TShiftState);
begin
  // Capture aussi les saisies au clavier (virtuel ou Bluetooth)
  GestionSession.ResetTimeout;
end;
```

> 💡 **Le timeout côté client ne suffit pas** : un attaquant qui contrôle le téléphone peut désactiver le timer. **Le serveur doit aussi** invalider les tokens après une période d'inactivité (`last_used_at` mis à jour à chaque appel, expiration côté serveur après N minutes sans activité).

## Conformité éditeurs : exigences 2024-2026

### Privacy Manifest (Apple)

**Obligatoire depuis le 1ᵉʳ mai 2024** pour toutes les apps soumises à l'App Store et leurs SDK tiers. C'est un fichier `PrivacyInfo.xcprivacy` (XML) à inclure dans le bundle, qui déclare :

1. **NSPrivacyAccessedAPITypes** : utilisation des « *Required Reason APIs* » (UserDefaults, attributs de timestamp de fichier, `mach_absolute_time`, `systemUptime`, `disk space`…) — chacune doit déclarer une « raison approuvée par Apple ».
2. **NSPrivacyTrackingDomains** : domaines utilisés pour le tracking inter-apps.
3. **NSPrivacyCollectedDataTypes** : types de données collectées et finalités (pour les *Privacy Nutrition Labels* affichées sur l'App Store).

Une app sans Privacy Manifest est rejetée par App Store Connect. Pour Delphi 13, le manifest est à ajouter manuellement dans le bundle iOS jusqu'à intégration dans l'IDE.

### Play Integrity API (Google)

Google a déprécié **SafetyNet Attestation** en 2024 au profit de **Play Integrity API**. Différence principale :

- **SafetyNet** : un verdict booléen `ctsProfileMatch` qui s'attestait globalement.
- **Play Integrity** : trois verdicts séparés — `MEETS_DEVICE_INTEGRITY` (appareil non rooté/modifié), `MEETS_BASIC_INTEGRITY` (intégrité minimale), `MEETS_STRONG_INTEGRITY` (appareil avec bootloader verrouillé et Play Protect actif). Permet une politique graduée selon le niveau de risque.

Le résultat est un JWT signé par Google que le serveur vérifie. Côté Delphi, on appelle l'API via JNI :

```pascal
// Pseudo-code pour appel Play Integrity
function VerifierIntegritePlay(const ANonce: string): TIntegrityVerdict;  
begin  
  // 1. Appeler IntegrityManager.requestIntegrityToken(IntegrityTokenRequest)
  //    via JNI avec le nonce généré côté serveur.
  // 2. Récupérer le JWT et l'envoyer au serveur.
  // 3. Serveur : appeler l'API Google Play Integrity pour décoder et
  //    valider le JWT, retourne le verdict.
  // 4. Décider si l'app peut continuer ou doit être bloquée.
end;
```

> 💡 **Bonnes pratiques** :  
> - Ne JAMAIS faire confiance à un verdict décodé côté client — l'attestation doit être validée côté serveur.  
> - Le nonce doit être unique par requête, généré par le serveur, lié à l'action sensible (paiement, déverrouillage de fonctionnalité premium…).  
> - Prévoir un *fallback gracieux* : un téléphone rooté légitime (développeur, power user) ne doit pas être totalement bloqué — proposer une auth renforcée à la place.

### Apple App Attest

L'équivalent Apple, disponible depuis iOS 14 (2020). Fournit deux primitives :

- **DeviceCheck** : un identifiant persistant (2 bits par app) pour suivre l'éligibilité d'un appareil (un essai gratuit, par exemple) sans collecter de données personnelles.
- **App Attest** : attestation cryptographique que la requête vient bien d'une instance non modifiée de votre app, sur un appareil Apple authentique. Utilisé contre le scraping et l'API abuse.

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
// `IgnoreCase = True` car `HTTPS://...` est valide aussi (RFC 3986).
if not URL.StartsWith('https://', True) then
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
