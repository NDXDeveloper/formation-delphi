# 15.10 Permissions et confidentialité des données

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La gestion des permissions et la protection de la confidentialité des données sont des aspects fondamentaux du développement d'applications mobiles modernes. Les utilisateurs sont de plus en plus sensibles à la façon dont leurs données personnelles sont utilisées, et les réglementations comme le RGPD (Règlement Général sur la Protection des Données) en Europe ou le CCPA (California Consumer Privacy Act) aux États-Unis imposent des exigences strictes aux développeurs. Dans cette section, nous explorerons comment gérer correctement les permissions et protéger la confidentialité des utilisateurs dans vos applications Delphi.

## Comprendre les permissions mobiles

### Qu'est-ce qu'une permission ?

Une permission est une autorisation que l'utilisateur accorde à votre application pour accéder à certaines fonctionnalités du système ou à des données personnelles. Par exemple :

- Accès à la caméra ou au microphone
- Accès à la localisation (GPS)
- Accès aux contacts ou au calendrier
- Accès aux photos et aux fichiers
- Envoi de notifications

### Types de permissions

Les systèmes d'exploitation mobiles classifient généralement les permissions en différentes catégories selon leur niveau de sensibilité :

#### Android

- **Permissions normales** : Accordées automatiquement (accès à Internet, vibration, etc.)
- **Permissions dangereuses** : Nécessitent l'approbation explicite de l'utilisateur (localisation, caméra, contacts, etc.)
- **Permissions spéciales** : Nécessitent des étapes supplémentaires (accès aux notifications, usage des statistiques, etc.)

#### iOS

- **Permissions d'utilisation** : Nécessitent l'approbation de l'utilisateur via des boîtes de dialogue système

## Gestion des permissions dans Delphi

Delphi simplifie la gestion des permissions sur les différentes plateformes grâce à ses API unifiées, mais il reste important de comprendre les spécificités de chaque système.

### Configuration des permissions dans votre projet

#### Pour Android

Dans Delphi, vous devez déclarer les permissions Android dans les options du projet :

1. Ouvrez **Project > Options**
2. Sélectionnez **Building > Android > Permissions**
3. Cochez les permissions nécessaires à votre application

![Permissions Android](https://votre-serveur.com/images/android-permissions.png)

Les permissions les plus courantes incluent :

- `INTERNET` : Accès Internet
- `ACCESS_FINE_LOCATION` : Accès à la localisation précise (GPS)
- `ACCESS_COARSE_LOCATION` : Accès à la localisation approximative (réseau)
- `CAMERA` : Accès à la caméra
- `READ_EXTERNAL_STORAGE` : Lecture du stockage externe
- `WRITE_EXTERNAL_STORAGE` : Écriture sur le stockage externe
- `READ_CONTACTS` : Lecture des contacts
- `RECORD_AUDIO` : Enregistrement audio

#### Pour iOS

Pour iOS, vous devez configurer les "usage descriptions" dans le fichier Info.plist :

1. Ouvrez **Project > Options**
2. Sélectionnez **Building > iOS > Version Info**
3. Cliquez sur **Custom plist**
4. Ajoutez les entrées NSxxxUsageDescription nécessaires

![Permissions iOS](https://votre-serveur.com/images/ios-permissions.png)

Les descriptions d'usage les plus courantes incluent :

- `NSCameraUsageDescription` : Pourquoi l'application veut accéder à la caméra
- `NSLocationWhenInUseUsageDescription` : Pourquoi l'application veut accéder à la localisation
- `NSPhotoLibraryUsageDescription` : Pourquoi l'application veut accéder aux photos
- `NSMicrophoneUsageDescription` : Pourquoi l'application veut accéder au microphone
- `NSContactsUsageDescription` : Pourquoi l'application veut accéder aux contacts

Ces descriptions apparaîtront dans les boîtes de dialogue de demande d'autorisation, donc assurez-vous qu'elles expliquent clairement pourquoi votre application a besoin de ces accès.

### Demande de permissions au moment opportun

Il est essentiel de demander les permissions au bon moment, généralement juste avant d'utiliser la fonctionnalité concernée, et non pas toutes en même temps au démarrage de l'application.

#### Utilisation du service de permissions FireMonkey

Delphi fournit un service unifié pour gérer les permissions avec l'interface `IFMXPermissionsService` :

```pascal
uses
  System.Permissions, FMX.Platform;

procedure TMainForm.RequestCameraPermission;
var
  PermissionsService: IFMXPermissionsService;
begin
  // Récupérer le service de permissions
  if TPlatformServices.Current.SupportsPlatformService(IFMXPermissionsService, IInterface(PermissionsService)) then
  begin
    // Liste des permissions requises
    var PermissionsArray: TArray<string> := [
      {$IFDEF ANDROID}
      'android.permission.CAMERA'
      {$ENDIF}
      {$IFDEF IOS}
      'NSCameraUsageDescription'
      {$ENDIF}
    ];

    // Demander les permissions
    PermissionsService.RequestPermissions(PermissionsArray,
      // Callback de résultat
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      var
        AllGranted: Boolean;
        I: Integer;
      begin
        AllGranted := True;
        for I := 0 to High(AGrantResults) do
          if AGrantResults[I] <> TPermissionStatus.Granted then
          begin
            AllGranted := False;
            Break;
          end;

        if AllGranted then
          OpenCamera  // Permission accordée, on peut ouvrir la caméra
        else
          ShowMessage('La permission d''accès à la caméra est nécessaire pour cette fonctionnalité');
      end);
  end
  else
    // Le service de permissions n'est pas disponible
    ShowMessage('Service de permissions non disponible');
end;
```

#### Vérification préalable des permissions

Avant de demander une permission, il est souvent utile de vérifier si elle est déjà accordée :

```pascal
procedure TMainForm.CheckCameraPermission;
var
  PermissionsService: IFMXPermissionsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPermissionsService, IInterface(PermissionsService)) then
  begin
    // Vérifier si la permission est déjà accordée
    {$IFDEF ANDROID}
    var Permission := 'android.permission.CAMERA';
    {$ENDIF}
    {$IFDEF IOS}
    var Permission := 'NSCameraUsageDescription';
    {$ENDIF}

    if PermissionsService.IsPermissionGranted(Permission) then
      OpenCamera  // Permission déjà accordée
    else
      RequestCameraPermission;  // Demander la permission
  end;
end;
```

### Gestion des refus de permissions

Il est important de gérer correctement les cas où l'utilisateur refuse une permission :

1. **Informez clairement** pourquoi la permission est nécessaire
2. **Proposez une alternative** si possible
3. **Évitez de redemander** immédiatement (risque d'agacement)
4. **Guidez l'utilisateur** vers les paramètres système si nécessaire

```pascal
procedure TMainForm.HandleCameraPermissionDenial;
begin
  // Créer une boîte de dialogue explicative
  var Dialog := TDialogBuilder.Create(Self)
    .SetTitle('Accès à la caméra')
    .SetMessage('Sans accès à la caméra, vous ne pourrez pas scanner de codes QR. ' +
                'Vous pouvez activer cette permission dans les paramètres de l''application.')
    .AddButton('Ignorer', nil)
    .AddButton('Paramètres', procedure(Sender: TObject)
      begin
        // Ouvrir les paramètres de l'application
        {$IFDEF ANDROID}
        OpenAppSettings;
        {$ENDIF}
        {$IFDEF IOS}
        OpenIOSSettings;
        {$ENDIF}
      end)
    .Build;

  Dialog.Show;
end;

{$IFDEF ANDROID}
procedure TMainForm.OpenAppSettings;
var
  Intent: JIntent;
  Uri: Jnet_Uri;
begin
  Intent := TJIntent.Create;
  Intent.setAction(TJSettings.JavaClass.ACTION_APPLICATION_DETAILS_SETTINGS);
  Uri := TJnet_Uri.JavaClass.fromParts(StringToJString('package'),
                                      StringToJString(JStringToString(TAndroidHelper.Context.getPackageName)),
                                      nil);
  Intent.setData(Uri);
  TAndroidHelper.Activity.startActivity(Intent);
end;
{$ENDIF}

{$IFDEF IOS}
procedure TMainForm.OpenIOSSettings;
begin
  // Ouvrir l'URL des paramètres iOS
  if TOSVersion.Check(10) then
    SharedApplication.openURL(TNSURL.OCClass.URLWithString(StrToNSStr('app-settings:')))
  else
    SharedApplication.openURL(TNSURL.OCClass.URLWithString(StrToNSStr('prefs:root=')));
end;
{$ENDIF}
```

### Sensibilisation contextuelle à la demande de permissions

Pour augmenter les chances d'acceptation, expliquez toujours pourquoi vous demandez une permission juste avant de la demander :

```pascal
procedure TMainForm.PrepareForLocationAccess;
begin
  // Afficher une explication avant de demander la permission
  var Dialog := TDialogBuilder.Create(Self)
    .SetTitle('Accès à la localisation')
    .SetMessage('Pour vous montrer les restaurants à proximité, ' +
                'nous avons besoin d''accéder à votre position. ' +
                'Votre localisation ne sera jamais partagée avec des tiers.')
    .AddButton('Annuler', nil)
    .AddButton('Continuer', procedure(Sender: TObject)
      begin
        RequestLocationPermission;
      end)
    .Build;

  Dialog.Show;
end;
```

## Protection de la confidentialité des données

Au-delà des permissions, la protection des données utilisateur implique plusieurs pratiques importantes :

### 1. Minimisation des données collectées

Ne collectez que les données strictement nécessaires au fonctionnement de votre application :

- Évaluez chaque type de donnée collectée
- Justifiez pourquoi elle est nécessaire
- Supprimez les données qui ne sont plus nécessaires

### 2. Stockage sécurisé des données sensibles

Pour stocker des données sensibles localement, utilisez des méthodes de cryptage :

```pascal
uses
  System.SysUtils, System.IOUtils, System.Hash,
  DUnitCryptography, DUnitX.Attributes;

function EncryptData(const Data, Password: string): string;
var
  Encryptor: TAES;
  EncryptedData: TBytes;
  Salt: TBytes;
  Key: TBytes;
begin
  // Générer un sel aléatoire
  SetLength(Salt, 16);
  FillRandomBytes(Salt);

  // Dériver une clé du mot de passe
  Key := PBKDF2_HMAC_SHA256(Password, Salt, 10000, 32);

  // Créer un encrypteur AES
  Encryptor := TAES.Create;
  try
    Encryptor.Mode := TCipherMode.CBC;
    Encryptor.KeySize := TKeySize.ks256;
    Encryptor.Key := Key;
    Encryptor.GenerateIV;

    // Crypter les données
    EncryptedData := Encryptor.EncryptStringToBytes(Data);

    // Retourner le sel + IV + données cryptées en Base64
    Result := TNetEncoding.Base64.EncodeBytesToString(
      ConcatBytes([Salt, Encryptor.IV, EncryptedData]));
  finally
    Encryptor.Free;
  end;
end;

function DecryptData(const EncryptedBase64, Password: string): string;
var
  Decryptor: TAES;
  EncryptedBytes, Salt, IV, EncryptedData: TBytes;
  Key: TBytes;
begin
  Result := '';

  // Décoder la chaîne Base64
  EncryptedBytes := TNetEncoding.Base64.DecodeStringToBytes(EncryptedBase64);
  if Length(EncryptedBytes) < 48 then  // 16 (salt) + 16 (IV) + min data
    Exit;

  // Extraire le sel, l'IV et les données
  Salt := Copy(EncryptedBytes, 0, 16);
  IV := Copy(EncryptedBytes, 16, 16);
  EncryptedData := Copy(EncryptedBytes, 32, Length(EncryptedBytes) - 32);

  // Dériver la clé
  Key := PBKDF2_HMAC_SHA256(Password, Salt, 10000, 32);

  // Créer un déchiffreur AES
  Decryptor := TAES.Create;
  try
    Decryptor.Mode := TCipherMode.CBC;
    Decryptor.KeySize := TKeySize.ks256;
    Decryptor.Key := Key;
    Decryptor.IV := IV;

    // Déchiffrer les données
    Result := Decryptor.DecryptBytesToString(EncryptedData);
  except
    Result := '';  // Mot de passe incorrect ou données corrompues
  finally
    Decryptor.Free;
  end;
end;
```

Pour utiliser ce code pour sécuriser des données sensibles :

```pascal
procedure TSecureStorage.SaveSecureData(const Key, Value: string);
var
  EncryptedValue: string;
  AppPassword: string;
begin
  // Obtenir un mot de passe pour l'application (idéalement stocké de manière sécurisée)
  AppPassword := GetAppPassword;

  // Crypter la valeur
  EncryptedValue := EncryptData(Value, AppPassword);

  // Enregistrer dans un fichier ou les préférences
  SaveValueToStorage(Key, EncryptedValue);
end;

function TSecureStorage.LoadSecureData(const Key: string): string;
var
  EncryptedValue: string;
  AppPassword: string;
begin
  // Récupérer la valeur cryptée
  EncryptedValue := LoadValueFromStorage(Key);
  if EncryptedValue = '' then
    Exit('');

  // Obtenir le mot de passe de l'application
  AppPassword := GetAppPassword;

  // Décrypter la valeur
  Result := DecryptData(EncryptedValue, AppPassword);
end;
```

### 3. Stockage sécurisé des identifiants

Sur iOS, vous pouvez utiliser le trousseau (Keychain) pour stocker des données sensibles comme les identifiants :

```pascal
{$IFDEF IOS}
uses
  Macapi.ObjectiveC, iOSapi.Foundation, iOSapi.Security;

function SaveToKeychain(const Service, Account, Password: string): Boolean;
var
  Query: NSMutableDictionary;
begin
  // Créer le dictionnaire de requête
  Query := TNSMutableDictionary.Create;

  // Configuration des attributs du trousseau
  Query.setObject(StrToNSStr(Service), NSStr('svce'));
  Query.setObject(StrToNSStr(Account), NSStr('acct'));
  Query.setObject(TNSNumber.OCClass.numberWithInt(kSecClassGenericPassword), NSStr('class'));
  Query.setObject(TNSData.OCClass.dataWithBytes(
    PByte(MarshaledAString(Password)), Length(Password)), NSStr('v_Data'));

  // Supprimer toute entrée existante
  SecItemDelete(Query);

  // Ajouter la nouvelle entrée
  Result := SecItemAdd(Query, nil) = errSecSuccess;
end;

function LoadFromKeychain(const Service, Account: string; out Password: string): Boolean;
var
  Query, Results: NSMutableDictionary;
  PasswordData: NSData;
begin
  Password := '';
  Result := False;

  // Créer le dictionnaire de requête
  Query := TNSMutableDictionary.Create;

  // Configuration des attributs du trousseau
  Query.setObject(StrToNSStr(Service), NSStr('svce'));
  Query.setObject(StrToNSStr(Account), NSStr('acct'));
  Query.setObject(TNSNumber.OCClass.numberWithInt(kSecClassGenericPassword), NSStr('class'));
  Query.setObject(kCFBooleanTrue, NSStr('r_Data'));

  // Récupérer l'entrée
  if SecItemCopyMatching(Query, @Results) = errSecSuccess then
  begin
    PasswordData := TNSData.Wrap(Results.objectForKey(NSStr('v_Data')));
    if Assigned(PasswordData) then
    begin
      SetLength(Password, PasswordData.length);
      Move(PasswordData.bytes^, PChar(Password)^, PasswordData.length);
      Result := True;
    end;
  end;
end;
{$ENDIF}
```

Sur Android, vous pouvez utiliser le KeyStore pour les versions récentes :

```pascal
{$IFDEF ANDROID}
uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Security,
  Androidapi.Helpers, Androidapi.JNI.App;

function GetMasterKey: JSecretKey;
var
  KeyStore: JKeyStore;
  KeyGenerator: JKeyGenerator;
begin
  Result := nil;

  try
    // Accéder au KeyStore Android
    KeyStore := TJKeyStore.JavaClass.getInstance(StringToJString('AndroidKeyStore'));
    KeyStore.load(nil);

    // Vérifier si la clé existe déjà
    if not KeyStore.containsAlias(StringToJString('MasterKey')) then
    begin
      // Générer une nouvelle clé
      KeyGenerator := TJKeyGenerator.JavaClass.getInstance(
        StringToJString('AES'),
        StringToJString('AndroidKeyStore'));

      // Configuration du générateur de clés
      var Builder := TJKeyGenParameterSpec_Builder.JavaClass.init(
        StringToJString('MasterKey'),
        TJKeyProperties.JavaClass.PURPOSE_ENCRYPT or TJKeyProperties.JavaClass.PURPOSE_DECRYPT);

      Builder.setBlockModes(StringToJString('CBC'));
      Builder.setEncryptionPaddings(StringToJString('PKCS7Padding'));
      Builder.setUserAuthenticationRequired(False);

      KeyGenerator.init(Builder.build);
      KeyGenerator.generateKey;
    end;

    // Récupérer la clé
    var Entry := KeyStore.getEntry(StringToJString('MasterKey'), nil);
    Result := TJSecretKey.Wrap(TJKeyStore_SecretKeyEntry.Wrap(Entry).getSecretKey);
  except
    Result := nil;
  end;
end;

function EncryptWithKeyStore(const PlainText: string): string;
var
  Cipher: JCipher;
  SecretKey: JSecretKey;
  EncryptedBytes, IVBytes: TJavaArray<Byte>;
begin
  Result := '';

  try
    // Obtenir la clé maître
    SecretKey := GetMasterKey;
    if SecretKey = nil then
      Exit;

    // Initialiser le chiffrement
    Cipher := TJCipher.JavaClass.getInstance(StringToJString('AES/CBC/PKCS7Padding'));
    Cipher.init(TJCipher.JavaClass.ENCRYPT_MODE, SecretKey);

    // Chiffrer les données
    var PlainBytes := StringToJString(PlainText).getBytes;
    EncryptedBytes := Cipher.doFinal(PlainBytes);

    // Récupérer le vecteur d'initialisation (IV)
    IVBytes := Cipher.getIV;

    // Concaténer IV + données chiffrées en Base64
    var Combined := CombineByteArrays(IVBytes, EncryptedBytes);
    Result := TAndroidHelper.Base64Encode(ArrayToBytes(Combined));
  except
    Result := '';
  end;
end;

function DecryptWithKeyStore(const EncryptedBase64: string): string;
var
  Cipher: JCipher;
  SecretKey: JSecretKey;
  IVBytes, EncryptedBytes, DecryptedBytes: TJavaArray<Byte>;
  AllBytes: TBytes;
begin
  Result := '';

  try
    // Obtenir la clé maître
    SecretKey := GetMasterKey;
    if SecretKey = nil then
      Exit;

    // Décoder la Base64
    AllBytes := TAndroidHelper.Base64Decode(EncryptedBase64);

    // Extraire l'IV (16 premiers octets) et les données chiffrées
    IVBytes := TJavaArray<Byte>.Create(16);
    Move(AllBytes[0], IVBytes.Data^, 16);

    EncryptedBytes := TJavaArray<Byte>.Create(Length(AllBytes) - 16);
    Move(AllBytes[16], EncryptedBytes.Data^, Length(AllBytes) - 16);

    // Initialiser le déchiffrement
    Cipher := TJCipher.JavaClass.getInstance(StringToJString('AES/CBC/PKCS7Padding'));
    var IvSpec := TJIvParameterSpec.JavaClass.init(IVBytes);
    Cipher.init(TJCipher.JavaClass.DECRYPT_MODE, SecretKey, IvSpec);

    // Déchiffrer
    DecryptedBytes := Cipher.doFinal(EncryptedBytes);

    // Convertir en chaîne
    var JStr := TJString.JavaClass.init(DecryptedBytes);
    Result := JStringToString(JStr);
  except
    Result := '';
  end;
end;
{$ENDIF}
```

### 4. Politique de confidentialité

Une politique de confidentialité est un document essentiel qui explique aux utilisateurs :

- Quelles données sont collectées
- Comment les données sont utilisées
- Avec qui les données sont partagées
- Comment les utilisateurs peuvent contrôler leurs données

Exemple d'implémentation d'un écran de politique de confidentialité :

```pascal
procedure TMainForm.ShowPrivacyPolicy;
var
  PrivacyForm: TForm;
  WebBrowser: TWebBrowser;
begin
  // Créer un formulaire pour afficher la politique
  PrivacyForm := TForm.Create(nil);
  try
    PrivacyForm.Caption := 'Politique de confidentialité';
    PrivacyForm.Position := TFormPosition.MainFormCenter;
    PrivacyForm.Width := 600;
    PrivacyForm.Height := 800;

    // Ajouter un navigateur web
    WebBrowser := TWebBrowser.Create(PrivacyForm);
    WebBrowser.Parent := PrivacyForm;
    WebBrowser.Align := TAlignLayout.Client;

    // Charger la politique depuis une ressource ou en ligne
    {$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
    WebBrowser.URL := 'https://www.votreapp.com/privacy';
    {$ELSE}
    // Pour les appareils mobiles, préférez une version locale
    // pour éviter les problèmes de connectivité
    var PrivacyPath := TPath.Combine(TPath.GetDocumentsPath, 'privacy_policy.html');
    if TFile.Exists(PrivacyPath) then
      WebBrowser.Navigate('file://' + PrivacyPath)
    else
      WebBrowser.LoadFromStrings('<html><body><h1>Politique de confidentialité</h1>...</body></html>', '');
    {$ENDIF}

    // Afficher le formulaire modal
    PrivacyForm.ShowModal;
  finally
    PrivacyForm.Free;
  end;
end;
```

### 5. Consentement de l'utilisateur

Obtenez et enregistrez le consentement explicite des utilisateurs pour la collecte de données :

```pascal
procedure TMainForm.ShowConsentDialog;
var
  ConsentForm: TForm;
  Label1: TLabel;
  CheckAnalytics, CheckPersonalized: TCheckBox;
  BtnAccept, BtnDecline: TButton;
begin
  // Créer un formulaire de consentement
  ConsentForm := TForm.Create(nil);
  try
    ConsentForm.Caption := 'Votre vie privée est importante';
    ConsentForm.Position := TFormPosition.MainFormCenter;
    ConsentForm.Width := 400;
    ConsentForm.Height := 300;

    // Ajouter une description
    Label1 := TLabel.Create(ConsentForm);
    Label1.Parent := ConsentForm;
    Label1.Align := TAlignLayout.Top;
    Label1.Height := 100;
    Label1.WordWrap := True;
    Label1.Text := 'Nous respectons votre vie privée. Veuillez choisir ' +
                   'comment vous souhaitez que vos données soient utilisées.';

    // Checkbox pour les analytiques
    CheckAnalytics := TCheckBox.Create(ConsentForm);
    CheckAnalytics.Parent := ConsentForm;
    CheckAnalytics.Position.Y := 120;
    CheckAnalytics.Position.X := 20;
    CheckAnalytics.Width := 360;
    CheckAnalytics.Text := 'Autoriser les statistiques anonymes d''utilisation';

    // Checkbox pour le contenu personnalisé
    CheckPersonalized := TCheckBox.Create(ConsentForm);
    CheckPersonalized.Parent := ConsentForm;
    CheckPersonalized.Position.Y := 150;
    CheckPersonalized.Position.X := 20;
    CheckPersonalized.Width := 360;
    CheckPersonalized.Text := 'Autoriser le contenu personnalisé';

    // Boutons
    BtnAccept := TButton.Create(ConsentForm);
    BtnAccept.Parent := ConsentForm;
    BtnAccept.Text := 'Accepter la sélection';
    BtnAccept.Position.X := 20;
    BtnAccept.Position.Y := 200;
    BtnAccept.Width := 170;
    BtnAccept.OnClick := procedure(Sender: TObject)
    begin
      // Enregistrer les préférences de consentement
      SaveConsentPreferences(CheckAnalytics.IsChecked, CheckPersonalized.IsChecked);
      ConsentForm.ModalResult := mrOk;
    end;

    BtnDecline := TButton.Create(ConsentForm);
    BtnDecline.Parent := ConsentForm;
    BtnDecline.Text := 'Tout refuser';
    BtnDecline.Position.X := 210;
    BtnDecline.Position.Y := 200;
    BtnDecline.Width := 170;
    BtnDecline.OnClick := procedure(Sender: TObject)
    begin
      // Enregistrer un refus global
      SaveConsentPreferences(False, False);
      ConsentForm.ModalResult := mrCancel;
    end;

    // Afficher le formulaire modal
    ConsentForm.ShowModal;
  finally
    ConsentForm.Free;
  end;
end;

procedure TMainForm.SaveConsentPreferences(AllowAnalytics, AllowPersonalized: Boolean);
begin
  // Enregistrer les préférences dans un stockage sécurisé
  {$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  var Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\YourCompany\YourApp', True) then
    begin
      Registry.WriteBool('ConsentAnalytics', AllowAnalytics);
      Registry.WriteBool('ConsentPersonalized', AllowPersonalized);
      Registry.WriteDateTime('ConsentDate', Now);
    end;
  finally
    Registry.Free;
  end;
  {$ELSE}
  // Pour les plateformes mobiles
  TPreferencesService.Current.SetValue('ConsentAnalytics', AllowAnalytics);
  TPreferencesService.Current.SetValue('ConsentPersonalized', AllowPersonalized);
  TPreferencesService.Current.SetValue('ConsentDate', DateTimeToStr(Now));
  {$ENDIF}
end;
```

## Bonnes pratiques pour les permissions et la confidentialité

### 1. Suivez le principe du moindre privilège

Demandez uniquement les permissions essentielles au fonctionnement de votre application :

- Évitez de demander l'accès à la localisation si vous avez seulement besoin du pays
- Préférez la localisation approximative à la localisation précise si possible
- Utilisez les alternatives moins intrusives quand elles existent

### 2. Timing approprié pour les demandes de permissions

Demandez les permissions au moment où l'utilisateur en a besoin, dans le bon contexte :

```pascal
procedure TMainForm.ButtonTakePhotoClick(Sender: TObject);
begin
  // Demander la permission au moment où l'utilisateur veut utiliser la caméra
  RequestCameraPermission;
end;
```

### 3. Soyez transparent sur l'utilisation des données

Expliquez clairement :

- Pourquoi vous collectez des données
- Comment vous les utilisez
- Avec qui vous les partagez

### 4. Fournissez un contrôle à l'utilisateur

Permettez aux utilisateurs de :

- Consulter leurs données
- Modifier leurs préférences de confidentialité
- Supprimer leurs données

```pascal
procedure TMainForm.ShowPrivacySettingsScreen;
var
  SettingsForm: TForm;
  CheckLocation, CheckCamera, CheckContacts, CheckAnalytics: TCheckBox;
  BtnDelete, BtnSave: TButton;
  ScrollBox: TScrollBox;
  Panel: TPanel;
begin
  // Créer un formulaire pour les paramètres de confidentialité
  SettingsForm := TForm.Create(nil);
  try
    SettingsForm.Caption := 'Paramètres de confidentialité';
    SettingsForm.Position := TFormPosition.MainFormCenter;
    SettingsForm.Width := 400;
    SettingsForm.Height := 500;

    // Ajouter un ScrollBox pour permettre le défilement
    ScrollBox := TScrollBox.Create(SettingsForm);
    ScrollBox.Parent := SettingsForm;
    ScrollBox.Align := TAlignLayout.Client;

    // Panneau de contenu dans le ScrollBox
    Panel := TPanel.Create(SettingsForm);
    Panel.Parent := ScrollBox;
    Panel.Width := ScrollBox.Width;
    Panel.Height := 600; // Plus grand que le ScrollBox pour permettre le défilement
    Panel.Align := TAlignLayout.Top;

    // En-tête explicatif
    var LabelHeader := TLabel.Create(SettingsForm);
    LabelHeader.Parent := Panel;
    LabelHeader.Position.X := 20;
    LabelHeader.Position.Y := 20;
    LabelHeader.Width := Panel.Width - 40;
    LabelHeader.Height := 80;
    LabelHeader.TextSettings.WordWrap := True;
    LabelHeader.Text := 'Gérez vos préférences de confidentialité. ' +
                        'Désactiver certaines fonctionnalités peut limiter ' +
                        'l''expérience utilisateur de l''application.';

    // Checkbox pour la localisation
    CheckLocation := TCheckBox.Create(SettingsForm);
    CheckLocation.Parent := Panel;
    CheckLocation.Position.X := 20;
    CheckLocation.Position.Y := 120;
    CheckLocation.Width := Panel.Width - 40;
    CheckLocation.Text := 'Autoriser l''accès à ma localisation';
    CheckLocation.IsChecked := TPreferencesService.Current.GetValue('ConsentLocation', False);

    // Explication sous la checkbox
    var LabelLocation := TLabel.Create(SettingsForm);
    LabelLocation.Parent := Panel;
    LabelLocation.Position.X := 40;
    LabelLocation.Position.Y := 150;
    LabelLocation.Width := Panel.Width - 60;
    LabelLocation.Height := 40;
    LabelLocation.TextSettings.WordWrap := True;
    LabelLocation.TextSettings.FontColor := TAlphaColors.Gray;
    LabelLocation.Text := 'Utilisé pour vous montrer des services à proximité ' +
                         'et personnaliser les résultats de recherche.';

    // Checkbox pour la caméra
    CheckCamera := TCheckBox.Create(SettingsForm);
    CheckCamera.Parent := Panel;
    CheckCamera.Position.X := 20;
    CheckCamera.Position.Y := 200;
    CheckCamera.Width := Panel.Width - 40;
    CheckCamera.Text := 'Autoriser l''accès à la caméra';
    CheckCamera.IsChecked := TPreferencesService.Current.GetValue('ConsentCamera', False);

    // Explication sous la checkbox
    var LabelCamera := TLabel.Create(SettingsForm);
    LabelCamera.Parent := Panel;
    LabelCamera.Position.X := 40;
    LabelCamera.Position.Y := 230;
    LabelCamera.Width := Panel.Width - 60;
    LabelCamera.Height := 40;
    LabelCamera.TextSettings.WordWrap := True;
    LabelCamera.TextSettings.FontColor := TAlphaColors.Gray;
    LabelCamera.Text := 'Utilisé pour scanner des codes QR et prendre des photos.';

    // Checkbox pour les contacts
    CheckContacts := TCheckBox.Create(SettingsForm);
    CheckContacts.Parent := Panel;
    CheckContacts.Position.X := 20;
    CheckContacts.Position.Y := 280;
    CheckContacts.Width := Panel.Width - 40;
    CheckContacts.Text := 'Autoriser l''accès à mes contacts';
    CheckContacts.IsChecked := TPreferencesService.Current.GetValue('ConsentContacts', False);

    // Explication sous la checkbox
    var LabelContacts := TLabel.Create(SettingsForm);
    LabelContacts.Parent := Panel;
    LabelContacts.Position.X := 40;
    LabelContacts.Position.Y := 310;
    LabelContacts.Width := Panel.Width - 60;
    LabelContacts.Height := 40;
    LabelContacts.TextSettings.WordWrap := True;
    LabelContacts.TextSettings.FontColor := TAlphaColors.Gray;
    LabelContacts.Text := 'Utilisé pour vous permettre de partager du contenu ' +
                         'avec vos contacts.';

    // Checkbox pour les analytics
    CheckAnalytics := TCheckBox.Create(SettingsForm);
    CheckAnalytics.Parent := Panel;
    CheckAnalytics.Position.X := 20;
    CheckAnalytics.Position.Y := 360;
    CheckAnalytics.Width := Panel.Width - 40;
    CheckAnalytics.Text := 'Autoriser les statistiques d''utilisation anonymes';
    CheckAnalytics.IsChecked := TPreferencesService.Current.GetValue('ConsentAnalytics', False);

    // Explication sous la checkbox
    var LabelAnalytics := TLabel.Create(SettingsForm);
    LabelAnalytics.Parent := Panel;
    LabelAnalytics.Position.X := 40;
    LabelAnalytics.Position.Y := 390;
    LabelAnalytics.Width := Panel.Width - 60;
    LabelAnalytics.Height := 40;
    LabelAnalytics.TextSettings.WordWrap := True;
    LabelAnalytics.TextSettings.FontColor := TAlphaColors.Gray;
    LabelAnalytics.Text := 'Nous aide à améliorer l''application en analysant ' +
                          'comment elle est utilisée.';

    // Bouton pour supprimer toutes les données
    BtnDelete := TButton.Create(SettingsForm);
    BtnDelete.Parent := Panel;
    BtnDelete.Position.X := 20;
    BtnDelete.Position.Y := 450;
    BtnDelete.Width := Panel.Width - 40;
    BtnDelete.Height := 44;
    BtnDelete.Text := 'Supprimer toutes mes données';
    BtnDelete.OnClick := procedure(Sender: TObject)
    begin
      // Demander confirmation
      if MessageDlg('Êtes-vous sûr de vouloir supprimer toutes vos données ? ' +
                   'Cette action est irréversible.',
                   TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
      begin
        DeleteUserData;
        ShowMessage('Toutes vos données ont été supprimées.');
        SettingsForm.Close;
      end;
    end;

    // Bouton pour sauvegarder les paramètres
    BtnSave := TButton.Create(SettingsForm);
    BtnSave.Parent := Panel;
    BtnSave.Position.X := 20;
    BtnSave.Position.Y := 510;
    BtnSave.Width := Panel.Width - 40;
    BtnSave.Height := 44;
    BtnSave.Text := 'Enregistrer les paramètres';
    BtnSave.OnClick := procedure(Sender: TObject)
    begin
      // Sauvegarder les préférences
      TPreferencesService.Current.SetValue('ConsentLocation', CheckLocation.IsChecked);
      TPreferencesService.Current.SetValue('ConsentCamera', CheckCamera.IsChecked);
      TPreferencesService.Current.SetValue('ConsentContacts', CheckContacts.IsChecked);
      TPreferencesService.Current.SetValue('ConsentAnalytics', CheckAnalytics.IsChecked);

      // Appliquer les changements
      ApplyPrivacySettings;

      ShowMessage('Vos préférences de confidentialité ont été enregistrées.');
      SettingsForm.Close;
    end;

    // Afficher le formulaire
    SettingsForm.ShowModal;
  finally
    SettingsForm.Free;
  end;
end;

procedure TMainForm.DeleteUserData;
begin
  // Supprimer toutes les données utilisateur

  // 1. Effacer les préférences
  TPreferencesService.Current.RemoveAll;

  // 2. Effacer les fichiers locaux
  var UserDataPath := TPath.Combine(TPath.GetDocumentsPath, 'UserData');
  if TDirectory.Exists(UserDataPath) then
    TDirectory.Delete(UserDataPath, True);

  // 3. Effacer les données de la base de données locale
  var DB := FDatabaseHelper.GetConnection;
  try
    DB.ExecSQL('DELETE FROM user_data');
    DB.ExecSQL('DELETE FROM user_preferences');
    DB.ExecSQL('DELETE FROM user_history');
  except
    // Gérer les erreurs silencieusement
  end;

  // 4. Demander au serveur de supprimer les données utilisateur (si applicable)
  if IsLoggedIn then
  begin
    var UserService := TUserService.Create;
    try
      UserService.DeleteUserData(GetUserID);
    finally
      UserService.Free;
    end;
  end;

  // 5. Déconnecter l'utilisateur
  Logout;
end;

procedure TMainForm.ApplyPrivacySettings;
begin
  // Appliquer les paramètres de confidentialité à l'application

  // 1. Gestion des données de localisation
  var AllowLocation := TPreferencesService.Current.GetValue('ConsentLocation', False);
  if AllowLocation then
    StartLocationServices
  else
    StopLocationServices;

  // 2. Gestion des analytics
  var AllowAnalytics := TPreferencesService.Current.GetValue('ConsentAnalytics', False);
  if AllowAnalytics then
    StartAnalyticsTracking
  else
    StopAnalyticsTracking;

  // Etc. pour les autres paramètres
end;
```

### 5. Respectez les réglementations locales

Adaptez votre application aux différentes réglementations en fonction de la localisation de l'utilisateur :

```pascal
procedure TMainForm.ApplyRegionalPrivacySettings;
var
  UserCountry: string;
begin
  // Déterminer le pays de l'utilisateur
  UserCountry := GetUserCountry;

  // Appliquer des paramètres spécifiques selon la région
  if (UserCountry = 'FR') or (UserCountry = 'DE') or (UserCountry = 'IT') or
     (UserCountry = 'ES') or (UserCountry = 'NL') then
  begin
    // Application du RGPD pour les utilisateurs européens
    ApplyGDPRSettings;
  end
  else if UserCountry = 'US' then
  begin
    // Vérifier si l'utilisateur est en Californie
    if IsUserInCalifornia then
      // Application du CCPA
      ApplyCCPASettings;
  end
  else if UserCountry = 'BR' then
  begin
    // Application de la LGPD brésilienne
    ApplyLGPDSettings;
  end
  // Etc. pour d'autres réglementations régionales
end;

function TMainForm.GetUserCountry: string;
begin
  // Essayer d'obtenir le pays à partir des paramètres de l'appareil
  Result := '';

  {$IFDEF ANDROID}
  var Locale := TJLocale.JavaClass.getDefault;
  Result := JStringToString(Locale.getCountry);
  {$ENDIF}

  {$IFDEF IOS}
  var Locale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  Result := NSStrToStr(Locale.countryCode);
  {$ENDIF}

  // Si le pays n'est pas disponible, utiliser l'adresse IP (si l'utilisateur a donné son consentement)
  if (Result = '') and TPreferencesService.Current.GetValue('ConsentLocation', False) then
  begin
    var IPGeolocationService := TIPGeolocationService.Create;
    try
      Result := IPGeolocationService.GetCountryCode;
    finally
      IPGeolocationService.Free;
    end;
  end;
end;

procedure TMainForm.ApplyGDPRSettings;
begin
  // Implémenter des paramètres conformes au RGPD

  // 1. S'assurer que le consentement est explicite
  if not TPreferencesService.Current.GetValue('ConsentExplicit', False) then
    ShowConsentDialog;

  // 2. S'assurer que l'utilisateur peut facilement accéder à ses données
  AddGDPRMenuItems;

  // 3. Limiter la durée de conservation des données
  SetupDataRetentionPolicy(180); // 180 jours pour l'UE
end;

procedure TMainForm.ApplyCCPASettings;
begin
  // Implémenter des paramètres conformes au CCPA

  // 1. Ajouter un lien "Ne pas vendre mes informations"
  AddDoNotSellMenuItem;

  // 2. S'assurer que l'utilisateur peut voir et supprimer ses données
  AddCCPAPrivacyControls;
end;
```

## Audit et conformité des permissions

### Automatisation des tests de permissions

Pour vous assurer que votre application demande correctement les permissions, créez des tests automatisés :

```pascal
procedure TPermissionTests.TestCameraPermission;
begin
  // Simuler un clic sur le bouton qui devrait déclencher la demande de permission caméra
  FMainForm.btnTakePhoto.Click;

  // Vérifier qu'une demande de permission a été effectuée
  Assert.IsTrue(FPermissionServiceMock.PermissionRequested('CAMERA'),
    'La permission de caméra aurait dû être demandée');

  // Simuler un refus de permission
  FPermissionServiceMock.SimulatePermissionDenial('CAMERA');

  // Vérifier que l'application gère correctement le refus
  Assert.IsFalse(FCameraComponent.Active,
    'La caméra ne devrait pas être active après un refus de permission');
  Assert.IsTrue(FMainForm.lblCameraStatus.Text.Contains('permission'),
    'Un message sur la permission devrait être affiché');
end;
```

### Liste de vérification pour la conformité RGPD

Utilisez cette liste de vérification pour vous assurer que votre application est conforme au RGPD :

1. **Consentement explicite** :
   - [ ] L'utilisateur peut donner son consentement de manière explicite
   - [ ] Chaque type de données a son propre consentement
   - [ ] Le consentement est stocké avec un horodatage

2. **Transparence** :
   - [ ] Politique de confidentialité claire et accessible
   - [ ] Explication de chaque demande de permission
   - [ ] Information sur les tiers qui reçoivent des données

3. **Contrôle utilisateur** :
   - [ ] Possibilité de consulter les données collectées
   - [ ] Option pour télécharger les données personnelles
   - [ ] Mécanisme de suppression des données

4. **Sécurité des données** :
   - [ ] Données sensibles cryptées
   - [ ] Transmission sécurisée (HTTPS)
   - [ ] Durée de conservation limitée

5. **Minimisation des données** :
   - [ ] Seules les données nécessaires sont collectées
   - [ ] Les données sont anonymisées quand c'est possible
   - [ ] Suppression automatique des données obsolètes

## Mise en œuvre d'une stratégie de confidentialité complète

Pour mettre en place une stratégie de confidentialité efficace, suivez ces étapes :

### 1. Cartographiez les données collectées

Documentez toutes les données personnelles que votre application collecte :

```pascal
type
  TDataCategory = (dcIdentification, dcContact, dcLocation, dcBehavioral, dcTechnical);

  TDataItem = record
    Name: string;
    Category: TDataCategory;
    Purpose: string;
    Retention: Integer; // Jours
    RequiresConsent: Boolean;
    ThirdPartySharing: Boolean;
  end;

procedure DocumentDataCollection;
var
  DataInventory: TList<TDataItem>;
begin
  DataInventory := TList<TDataItem>.Create;
  try
    // Exemples de données collectées
    DataInventory.Add(TDataItem.Create('Email', dcContact, 'Authentification et communication',
                                     -1, True, False));
    DataInventory.Add(TDataItem.Create('Nom', dcIdentification, 'Personnalisation',
                                     -1, True, False));
    DataInventory.Add(TDataItem.Create('Localisation précise', dcLocation, 'Recherche à proximité',
                                     30, True, False));
    DataInventory.Add(TDataItem.Create('Historique de recherche', dcBehavioral, 'Suggestions personnalisées',
                                     90, True, False));
    DataInventory.Add(TDataItem.Create('Adresse IP', dcTechnical, 'Sécurité et débogage',
                                     30, False, False));

    // Exporter l'inventaire pour documentation
    ExportDataInventory(DataInventory);
  finally
    DataInventory.Free;
  end;
end;
```

### 2. Implémentez un système de gestion du consentement

Créez un système complet pour gérer le consentement des utilisateurs :

```pascal
unit ConsentManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.DateUtils;

type
  TConsentPurpose = (cpEssential, cpFunctional, cpAnalytics, cpMarketing, cpThirdParty);

  TConsentManager = class
  private
    FConsents: TDictionary<TConsentPurpose, Boolean>;
    FConsentDates: TDictionary<TConsentPurpose, TDateTime>;

    procedure LoadConsents;
    procedure SaveConsents;
  public
    constructor Create;
    destructor Destroy; override;

    function HasConsent(Purpose: TConsentPurpose): Boolean;
    procedure SetConsent(Purpose: TConsentPurpose; Granted: Boolean);
    function GetConsentDate(Purpose: TConsentPurpose): TDateTime;
    procedure RevokeAllConsents;

    procedure ShowConsentDialog;
    function NeedsConsentUpdate: Boolean;
  end;

implementation

const
  CONSENT_VERSION = 2; // Incrémenter quand les politiques changent

constructor TConsentManager.Create;
begin
  inherited Create;
  FConsents := TDictionary<TConsentPurpose, Boolean>.Create;
  FConsentDates := TDictionary<TConsentPurpose, TDateTime>.Create;

  LoadConsents;
end;

destructor TConsentManager.Destroy;
begin
  SaveConsents;
  FConsents.Free;
  FConsentDates.Free;
  inherited;
end;

procedure TConsentManager.LoadConsents;
var
  JSON: string;
  JSONObj: TJSONObject;
  Purpose: TConsentPurpose;
begin
  // Initialiser avec des valeurs par défaut
  for Purpose := Low(TConsentPurpose) to High(TConsentPurpose) do
  begin
    FConsents.Add(Purpose, False);
    FConsentDates.Add(Purpose, 0);
  end;

  // Seuls les éléments essentiels sont activés par défaut
  FConsents[cpEssential] := True;

  // Charger les consentements sauvegardés
  JSON := TPreferencesService.Current.GetValue('UserConsents', '');
  if JSON <> '' then
  begin
    JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    if JSONObj <> nil then
    try
      // Vérifier la version du consentement
      var Version := JSONObj.GetValue<Integer>('version', 0);

      // Si la version est obsolète, ne pas charger les anciens consentements
      if Version = CONSENT_VERSION then
      begin
        // Charger les consentements
        var Consents := JSONObj.GetValue('consents') as TJSONObject;
        if Consents <> nil then
        begin
          for Purpose := Low(TConsentPurpose) to High(TConsentPurpose) do
          begin
            var PurposeName := GetEnumName(TypeInfo(TConsentPurpose), Ord(Purpose));
            PurposeName := PurposeName.Substring(2); // Supprimer le préfixe 'cp'

            if Consents.TryGetValue(PurposeName, var ConsentsValue) and (ConsentsValue is TJSONObject) then
            begin
              var ConsentObj := ConsentsValue as TJSONObject;
              FConsents[Purpose] := ConsentObj.GetValue<Boolean>('granted', False);

              var DateStr := ConsentObj.GetValue<string>('date', '');
              if DateStr <> '' then
                FConsentDates[Purpose] := ISO8601ToDate(DateStr);
            end;
          end;
        end;
      end;
    finally
      JSONObj.Free;
    end;
  end;
end;

procedure TConsentManager.SaveConsents;
var
  JSONObj, ConsentsObj: TJSONObject;
  Purpose: TConsentPurpose;
begin
  JSONObj := TJSONObject.Create;
  try
    // Enregistrer la version
    JSONObj.AddPair('version', TJSONNumber.Create(CONSENT_VERSION));

    // Créer l'objet des consentements
    ConsentsObj := TJSONObject.Create;
    JSONObj.AddPair('consents', ConsentsObj);

    // Ajouter chaque consentement
    for Purpose := Low(TConsentPurpose) to High(TConsentPurpose) do
    begin
      var PurposeName := GetEnumName(TypeInfo(TConsentPurpose), Ord(Purpose));
      PurposeName := PurposeName.Substring(2); // Supprimer le préfixe 'cp'

      var ConsentObj := TJSONObject.Create;
      ConsentObj.AddPair('granted', TJSONBool.Create(FConsents[Purpose]));

      if FConsentDates[Purpose] > 0 then
        ConsentObj.AddPair('date', DateToISO8601(FConsentDates[Purpose]));

      ConsentsObj.AddPair(PurposeName, ConsentObj);
    end;

    // Sauvegarder dans les préférences
    TPreferencesService.Current.SetValue('UserConsents', JSONObj.ToString);
  finally
    JSONObj.Free;
  end;
end;

function TConsentManager.HasConsent(Purpose: TConsentPurpose): Boolean;
begin
  Result := FConsents.ContainsKey(Purpose) and FConsents[Purpose];
end;

procedure TConsentManager.SetConsent(Purpose: TConsentPurpose; Granted: Boolean);
begin
  FConsents[Purpose] := Granted;
  FConsentDates[Purpose] := Now;
  SaveConsents;
end;

function TConsentManager.GetConsentDate(Purpose: TConsentPurpose): TDateTime;
begin
  Result := FConsentDates[Purpose];
end;

procedure TConsentManager.RevokeAllConsents;
var
  Purpose: TConsentPurpose;
begin
  for Purpose := Low(TConsentPurpose) to High(TConsentPurpose) do
  begin
    // Les consentements essentiels ne peuvent pas être révoqués
    if Purpose <> cpEssential then
      FConsents[Purpose] := False;
  end;

  SaveConsents;
end;

function TConsentManager.NeedsConsentUpdate: Boolean;
var
  LastConsentVersion: Integer;
begin
  // Vérifier si la version du consentement a changé
  LastConsentVersion := TPreferencesService.Current.GetValue('LastConsentVersion', 0);
  Result := LastConsentVersion < CONSENT_VERSION;

  // Si un consentement essentiel n'a jamais été donné
  if not Result then
    Result := FConsentDates[cpEssential] = 0;
end;

procedure TConsentManager.ShowConsentDialog;
begin
  // Implémentation du dialogue de consentement
  // ...

  // Mettre à jour la version du consentement
  TPreferencesService.Current.SetValue('LastConsentVersion', CONSENT_VERSION);
end;

end.
```

### 3. Vérifiez le consentement avant d'utiliser les données

Assurez-vous de vérifier le consentement avant d'utiliser les données personnelles :

```pascal
procedure TAnalyticsService.TrackEvent(const Category, Action, Label: string);
begin
  // Vérifier le consentement avant de collecter des analytics
  if ConsentManager.HasConsent(cpAnalytics) then
  begin
    // Collecter et envoyer les données analytiques
    // ...
  end
  else
  begin
    // Logger localement sans données personnelles
    LogEventWithoutPersonalData(Category, Action);
  end;
end;

procedure TLocationService.GetUserLocation(Callback: TProc<TLocationCoord2D>);
begin
  // Vérifier le consentement avant d'accéder à la localisation
  if ConsentManager.HasConsent(cpFunctional) then
  begin
    // Demander la localisation
    RequestLocationPermission(
      procedure(Granted: Boolean)
      begin
        if Granted then
        begin
          // Obtenir et retourner la localisation
          GetCurrentLocation(Callback);
        end
        else
          Callback(TLocationCoord2D.Create(0, 0));
      end);
  end
  else
  begin
    // Informer que le consentement est nécessaire
    ShowConsentRequiredMessage('localisation', cpFunctional);
    Callback(TLocationCoord2D.Create(0, 0));
  end;
end;
```

### 4. Mettez en place un système de journalisation des accès

Conservez un journal des accès aux données sensibles :

```pascal
type
  TDataAccessType = (datRead, datWrite, datExport, datDelete);

  TDataAccessLog = class
  private
    FLogFile: string;
  public
    constructor Create;

    procedure LogAccess(const UserID, DataCategory: string;
                      AccessType: TDataAccessType; Success: Boolean);
    function GetAccessLogs(const UserID: string): TArray<string>;
    procedure PurgeOldLogs(DaysToKeep: Integer);
  end;

constructor TDataAccessLog.Create;
begin
  inherited Create;
  FLogFile := TPath.Combine(TPath.GetDocumentsPath, 'data_access.log');
end;

procedure TDataAccessLog.LogAccess(const UserID, DataCategory: string;
                                 AccessType: TDataAccessType; Success: Boolean);
var
  LogEntry: string;
  LogStream: TStreamWriter;
begin
  // Créer l'entrée de journal
  LogEntry := Format('%s|%s|%s|%s|%s|%d', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    UserID,
    DataCategory,
    GetEnumName(TypeInfo(TDataAccessType), Ord(AccessType)),
    BoolToStr(Success, True),
    GetDeviceInfo
  ]);

  // Ajouter au fichier de journal
  try
    LogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8);
    try
      LogStream.WriteLine(LogEntry);
    finally
      LogStream.Free;
    end;
  except
    // Gérer silencieusement les erreurs de journalisation
  end;
end;
```

## Conclusion

La gestion des permissions et la protection de la confidentialité des données sont des aspects cruciaux du développement d'applications mobiles modernes. En suivant les bonnes pratiques présentées dans ce chapitre, vous pourrez :

1. **Respecter les réglementations** comme le RGPD, le CCPA et autres lois sur la protection des données
2. **Gagner la confiance de vos utilisateurs** en étant transparent sur l'utilisation de leurs données
3. **Éviter les problèmes juridiques** potentiellement coûteux
4. **Offrir une meilleure expérience utilisateur** en demandant les permissions au bon moment

Rappelez-vous que la protection de la vie privée doit être intégrée dès la conception de votre application ("Privacy by Design") et non ajoutée comme une réflexion après coup. En prenant en compte ces considérations dès le début de votre projet, vous créerez des applications plus respectueuses et plus susceptibles d'être appréciées par vos utilisateurs.

Dans la prochaine section, nous explorerons l'intégration des services Firebase dans vos applications mobiles Delphi pour ajouter des fonctionnalités puissantes comme les notifications push, l'authentification et l'analyse en temps réel.

⏭️ [Intégration des services Firebase](/15-applications-mobiles-avec-delphi/11-integration-des-services-firebase.md)
