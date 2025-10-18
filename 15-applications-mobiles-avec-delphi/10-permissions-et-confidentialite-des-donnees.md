🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.10 Permissions et confidentialité des données

## Introduction

Dans le monde mobile moderne, la protection de la vie privée est devenue une priorité absolue. Chaque jour, nous utilisons des applications qui accèdent à nos photos, notre localisation, nos contacts, notre microphone... Ces accès peuvent être légitimes et nécessaires au fonctionnement de l'application, mais ils peuvent aussi représenter des risques pour notre vie privée s'ils sont mal gérés.

C'est pourquoi iOS et Android ont mis en place des systèmes de permissions sophistiqués qui donnent le contrôle à l'utilisateur. Avant qu'une application puisse accéder à des données sensibles ou à des fonctionnalités matérielles, elle doit explicitement demander l'autorisation, et l'utilisateur peut refuser ou révoquer ces permissions à tout moment.

En tant que développeur, vous devez non seulement respecter ces règles techniques, mais aussi adopter une approche éthique de la confidentialité : ne demandez que les permissions vraiment nécessaires, expliquez clairement pourquoi vous en avez besoin, et protégez les données que vous collectez.

Dans cette section, nous allons explorer comment gérer correctement les permissions dans vos applications Delphi, comment respecter la confidentialité des utilisateurs, et comment vous conformer aux réglementations comme le RGPD.

## Comprendre le système de permissions

### Évolution des permissions mobiles

**Avant (Android < 6.0, iOS < 8)** :
- Permissions demandées à l'installation
- Tout ou rien : accepter toutes les permissions ou ne pas installer
- Aucun contrôle granulaire pour l'utilisateur

**Maintenant (Android 6+, iOS 8+)** :
- Permissions demandées à l'exécution (runtime permissions)
- L'utilisateur peut accepter ou refuser chaque permission individuellement
- L'utilisateur peut révoquer une permission à tout moment dans les paramètres
- Certaines permissions nécessitent une justification contextuelle

### Différence entre Android et iOS

**Android** :
- Permissions classées en "Normal" et "Dangerous"
- Permissions normales accordées automatiquement
- Permissions dangereuses nécessitent une demande explicite
- Groupes de permissions (ex: toutes les permissions de localisation ensemble)

**iOS** :
- Toutes les permissions sensibles nécessitent une demande explicite
- Descriptions obligatoires dans Info.plist expliquant l'usage
- Interface système standard pour toutes les demandes
- Granularité fine (ex: "Toujours", "Pendant l'utilisation", "Jamais" pour la localisation)

## Types de permissions

### Permissions dangereuses (nécessitant l'accord explicite)

**Localisation** :
```pascal
// Android
android.permission.ACCESS_FINE_LOCATION
android.permission.ACCESS_COARSE_LOCATION
android.permission.ACCESS_BACKGROUND_LOCATION

// iOS (Info.plist)
NSLocationWhenInUseUsageDescription
NSLocationAlwaysUsageDescription
NSLocationAlwaysAndWhenInUseUsageDescription
```

**Caméra et Photos** :
```pascal
// Android
android.permission.CAMERA
android.permission.READ_EXTERNAL_STORAGE
android.permission.WRITE_EXTERNAL_STORAGE

// iOS
NSCameraUsageDescription
NSPhotoLibraryUsageDescription
NSPhotoLibraryAddUsageDescription
```

**Microphone** :
```pascal
// Android
android.permission.RECORD_AUDIO

// iOS
NSMicrophoneUsageDescription
```

**Contacts** :
```pascal
// Android
android.permission.READ_CONTACTS
android.permission.WRITE_CONTACTS

// iOS
NSContactsUsageDescription
```

**Calendrier** :
```pascal
// Android
android.permission.READ_CALENDAR
android.permission.WRITE_CALENDAR

// iOS
NSCalendarsUsageDescription
```

**Capteurs et matériel** :
```pascal
// Android
android.permission.BODY_SENSORS
android.permission.BLUETOOTH
android.permission.BLUETOOTH_ADMIN

// iOS
NSMotionUsageDescription
NSBluetoothPeripheralUsageDescription
```

### Permissions normales (accordées automatiquement)

**Android** :
- Internet : `android.permission.INTERNET`
- État du réseau : `android.permission.ACCESS_NETWORK_STATE`
- WiFi : `android.permission.ACCESS_WIFI_STATE`
- Vibration : `android.permission.VIBRATE`
- Réveil : `android.permission.WAKE_LOCK`

**iOS** :
- Internet : Aucune permission nécessaire
- Vibration : Aucune permission nécessaire

## Configuration des permissions dans Delphi

### Android : Déclarer les permissions

Dans Delphi, configurez les permissions Android :

```
Project > Options > Uses Permissions (Android)
```

Cochez les permissions dont vous avez besoin :
- ☑ Camera
- ☑ Access Fine Location
- ☑ Record Audio
- ☑ Read External Storage
- etc.

Ces permissions seront ajoutées au fichier `AndroidManifest.xml` généré :

```xml
<manifest>
    <uses-permission android:name="android.permission.CAMERA" />
    <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
    <uses-permission android:name="android.permission.RECORD_AUDIO" />
</manifest>
```

### iOS : Déclarer les descriptions d'usage

Pour iOS, vous devez expliquer pourquoi vous avez besoin de chaque permission :

```
Project > Options > Version Info (iOS)
```

Ajoutez les clés dans le fichier Info.plist :

```xml
<key>NSCameraUsageDescription</key>
<string>Cette application a besoin d'accéder à votre appareil photo pour prendre des photos de vos documents.</string>

<key>NSLocationWhenInUseUsageDescription</key>
<string>Nous utilisons votre localisation pour vous montrer les magasins à proximité.</string>

<key>NSMicrophoneUsageDescription</key>
<string>Le microphone est utilisé pour enregistrer des notes vocales.</string>
```

**Important** : Ces descriptions sont visibles par l'utilisateur et par l'équipe de validation d'Apple. Elles doivent être claires, honnêtes et en français si votre application cible un public francophone.

## Demander des permissions avec Delphi

### Framework de base

Delphi fournit le service `TPermissionsService` pour gérer les permissions de manière unifiée.

```pascal
uses
  System.Permissions, FMX.DialogService;

// Vérifier si une permission est accordée
function TFormMain.PermissionCameraAccordee: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted('android.permission.CAMERA');
end;
```

### Demander une permission simple

```pascal
procedure TFormMain.DemanderPermissionCamera;
begin
  // Vérifier d'abord si on a déjà la permission
  if PermissionsService.IsPermissionGranted('android.permission.CAMERA') then
  begin
    // Permission déjà accordée, utiliser la caméra
    OuvrirCamera;
  end
  else
  begin
    // Demander la permission
    PermissionsService.RequestPermissions(
      ['android.permission.CAMERA'],
      procedure(const APermissions: TArray<string>;
                const AGrantResults: TArray<TPermissionStatus>)
      begin
        if (Length(AGrantResults) > 0) and
           (AGrantResults[0] = TPermissionStatus.Granted) then
        begin
          // Permission accordée
          OuvrirCamera;
        end
        else
        begin
          // Permission refusée
          TDialogService.ShowMessage(
            'L''accès à la caméra est nécessaire pour prendre des photos. ' +
            'Veuillez activer cette autorisation dans les paramètres de l''application.');
        end;
      end);
  end;
end;
```

### Demander plusieurs permissions

```pascal
procedure TFormMain.DemanderPermissionsMultiples;
const
  PERMISSIONS: array[0..2] of string = (
    'android.permission.CAMERA',
    'android.permission.READ_EXTERNAL_STORAGE',
    'android.permission.WRITE_EXTERNAL_STORAGE'
  );
begin
  PermissionsService.RequestPermissions(PERMISSIONS,
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    var
      i: Integer;
      ToutesAccordees: Boolean;
    begin
      ToutesAccordees := True;

      for i := 0 to High(AGrantResults) do
      begin
        if AGrantResults[i] <> TPermissionStatus.Granted then
        begin
          ToutesAccordees := False;
          ShowMessage('Permission refusée : ' + APermissions[i]);
        end;
      end;

      if ToutesAccordees then
      begin
        ShowMessage('Toutes les permissions ont été accordées');
        InitialiserFonctionnalites;
      end
      else
      begin
        ShowMessage('Certaines fonctionnalités seront limitées.');
      end;
    end);
end;
```

### Classe utilitaire pour gérer les permissions

Créons une classe réutilisable pour simplifier la gestion des permissions :

```pascal
unit Utils.Permissions;

interface

uses
  System.Permissions;

type
  TPermissionResult = (prAccordee, prRefusee, prDejaAccordee);
  TPermissionCallback = reference to procedure(Result: TPermissionResult);

  TPermissionHelper = class
  public
    // Constantes pour les permissions courantes
    const PERMISSION_CAMERA = 'android.permission.CAMERA';
    const PERMISSION_LOCATION_FINE = 'android.permission.ACCESS_FINE_LOCATION';
    const PERMISSION_LOCATION_COARSE = 'android.permission.ACCESS_COARSE_LOCATION';
    const PERMISSION_RECORD_AUDIO = 'android.permission.RECORD_AUDIO';
    const PERMISSION_READ_STORAGE = 'android.permission.READ_EXTERNAL_STORAGE';
    const PERMISSION_WRITE_STORAGE = 'android.permission.WRITE_EXTERNAL_STORAGE';
    const PERMISSION_READ_CONTACTS = 'android.permission.READ_CONTACTS';

    // Méthodes principales
    class function EstAccordee(const Permission: string): Boolean;
    class procedure Demander(const Permission: string;
      Callback: TPermissionCallback);
    class procedure DemanderPlusieurs(const Permissions: TArray<string>;
      CallbackTousAccordes: TProc;
      CallbackRefus: TProc);
  end;

implementation

uses
  FMX.DialogService;

class function TPermissionHelper.EstAccordee(const Permission: string): Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(Permission);
end;

class procedure TPermissionHelper.Demander(const Permission: string;
  Callback: TPermissionCallback);
begin
  // Si déjà accordée, callback immédiat
  if EstAccordee(Permission) then
  begin
    if Assigned(Callback) then
      Callback(TPermissionResult.prDejaAccordee);
    Exit;
  end;

  // Sinon, demander
  PermissionsService.RequestPermissions([Permission],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if Assigned(Callback) then
      begin
        if (Length(AGrantResults) > 0) and
           (AGrantResults[0] = TPermissionStatus.Granted) then
          Callback(TPermissionResult.prAccordee)
        else
          Callback(TPermissionResult.prRefusee);
      end;
    end);
end;

class procedure TPermissionHelper.DemanderPlusieurs(
  const Permissions: TArray<string>;
  CallbackTousAccordes: TProc;
  CallbackRefus: TProc);
var
  i: Integer;
  ToutesDejaAccordees: Boolean;
begin
  // Vérifier si toutes sont déjà accordées
  ToutesDejaAccordees := True;
  for i := 0 to High(Permissions) do
  begin
    if not EstAccordee(Permissions[i]) then
    begin
      ToutesDejaAccordees := False;
      Break;
    end;
  end;

  if ToutesDejaAccordees then
  begin
    if Assigned(CallbackTousAccordes) then
      CallbackTousAccordes;
    Exit;
  end;

  // Demander les permissions
  PermissionsService.RequestPermissions(Permissions,
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    var
      j: Integer;
      ToutesAccordees: Boolean;
    begin
      ToutesAccordees := True;
      for j := 0 to High(AGrantResults) do
      begin
        if AGrantResults[j] <> TPermissionStatus.Granted then
        begin
          ToutesAccordees := False;
          Break;
        end;
      end;

      if ToutesAccordees then
      begin
        if Assigned(CallbackTousAccordes) then
          CallbackTousAccordes;
      end
      else
      begin
        if Assigned(CallbackRefus) then
          CallbackRefus;
      end;
    end);
end;

end.
```

**Utilisation de la classe utilitaire** :

```pascal
uses
  Utils.Permissions;

// Exemple simple
procedure TFormMain.BtnPrendrePhotoClick(Sender: TObject);
begin
  TPermissionHelper.Demander(TPermissionHelper.PERMISSION_CAMERA,
    procedure(Result: TPermissionResult)
    begin
      case Result of
        prAccordee, prDejaAccordee:
          PrendrePhoto;
        prRefusee:
          ShowMessage('Impossible de prendre une photo sans accès à la caméra.');
      end;
    end);
end;

// Exemple avec plusieurs permissions
procedure TFormMain.BtnDemarrerTrackingClick(Sender: TObject);
begin
  TPermissionHelper.DemanderPlusieurs(
    [TPermissionHelper.PERMISSION_LOCATION_FINE,
     TPermissionHelper.PERMISSION_LOCATION_COARSE],
    procedure
    begin
      // Toutes les permissions accordées
      DemarrerTracking;
    end,
    procedure
    begin
      // Au moins une refusée
      ShowMessage('Le tracking nécessite l''accès à votre localisation.');
    end);
end;
```

## Expliquer avant de demander

Il est fortement recommandé d'expliquer à l'utilisateur **pourquoi** vous avez besoin d'une permission **avant** de la demander.

### Dialogue explicatif

```pascal
procedure TFormMain.DemanderPermissionAvecExplication;
begin
  // D'abord, expliquer
  TDialogService.MessageDialog(
    'Pour vous permettre de prendre des photos de vos reçus et de les ' +
    'joindre à vos notes de frais, nous avons besoin d''accéder à votre ' +
    'appareil photo.' + sLineBreak + sLineBreak +
    'Nous ne stockons ces photos que localement sur votre appareil et ne ' +
    'les partageons jamais avec des tiers.',
    TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
    TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOK then
      begin
        // L'utilisateur comprend, maintenant demander la permission
        TPermissionHelper.Demander(TPermissionHelper.PERMISSION_CAMERA,
          procedure(Result: TPermissionResult)
          begin
            if Result in [prAccordee, prDejaAccordee] then
              OuvrirCamera
            else
              ShowMessage('Vous pourrez activer cette permission plus tard ' +
                'dans les paramètres de l''application.');
          end);
      end;
    end);
end;
```

### Afficher une explication contextuelle

```pascal
// Afficher l'explication seulement la première fois
procedure TFormMain.DemanderPermissionIntelligente(const Permission: string;
  const Explication: string; Callback: TProc);
var
  DejaExplique: Boolean;
begin
  // Vérifier si on a déjà expliqué cette permission
  DejaExplique := LirePreference('permission_expliquee_' + Permission, False);

  if not DejaExplique then
  begin
    // Première fois : expliquer
    TDialogService.MessageDialog(Explication,
      TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK],
      TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        SauvegarderPreference('permission_expliquee_' + Permission, True);
        DemanderPermissionSimple(Permission, Callback);
      end);
  end
  else
  begin
    // Déjà expliqué, demander directement
    DemanderPermissionSimple(Permission, Callback);
  end;
end;

procedure TFormMain.DemanderPermissionSimple(const Permission: string;
  Callback: TProc);
begin
  TPermissionHelper.Demander(Permission,
    procedure(Result: TPermissionResult)
    begin
      if Result in [prAccordee, prDejaAccordee] then
      begin
        if Assigned(Callback) then
          Callback;
      end;
    end);
end;
```

## Gérer le refus de permissions

### Détecter un refus permanent

Sur Android, si l'utilisateur refuse une permission et coche "Ne plus demander", vous ne pourrez plus afficher le dialogue de permission. Vous devez détecter cette situation et guider l'utilisateur vers les paramètres.

```pascal
procedure TFormMain.GererRefusPermission(const Permission: string);
begin
  {$IFDEF ANDROID}
  // Sur Android, vérifier si l'utilisateur a coché "Ne plus demander"
  var Activity := TAndroidHelper.Activity;
  if not Activity.shouldShowRequestPermissionRationale(
    StringToJString(Permission)) then
  begin
    // L'utilisateur a refusé définitivement
    TDialogService.MessageDialog(
      'Pour utiliser cette fonctionnalité, vous devez activer l''autorisation ' +
      'dans les paramètres de l''application.' + sLineBreak + sLineBreak +
      'Paramètres > Applications > ' + GetNomApplication + ' > Autorisations',
      TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
      TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrOK then
          OuvrirParametresApplication;
      end,
      'Ouvrir paramètres', 'Annuler');
  end
  else
  begin
    // Simple refus, on peut redemander plus tard
    ShowMessage('Cette fonctionnalité nécessite votre autorisation.');
  end;
  {$ENDIF}

  {$IFDEF IOS}
  // Sur iOS, guider vers les paramètres
  TDialogService.MessageDialog(
    'Pour utiliser cette fonctionnalité, activez l''autorisation dans :' +
    sLineBreak + 'Réglages > ' + GetNomApplication,
    TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK],
    TMsgDlgBtn.mbOK, 0);
  {$ENDIF}
end;

// Ouvrir les paramètres de l'application
procedure TFormMain.OuvrirParametresApplication;
begin
  {$IFDEF ANDROID}
  var Intent := TJIntent.Create;
  Intent.setAction(TJSettings.JavaClass.ACTION_APPLICATION_DETAILS_SETTINGS);
  Intent.setData(StrToJURI('package:' +
    JStringToString(TAndroidHelper.Context.getPackageName)));
  TAndroidHelper.Context.startActivity(Intent);
  {$ENDIF}

  {$IFDEF IOS}
  var URL := StrToNSUrl('app-settings:');
  SharedApplication.openURL(URL);
  {$ENDIF}
end;
```

### Mode dégradé

Proposez toujours une alternative quand une permission est refusée :

```pascal
procedure TFormMain.BtnAjouterPhotoClick(Sender: TObject);
begin
  TPermissionHelper.Demander(TPermissionHelper.PERMISSION_CAMERA,
    procedure(Result: TPermissionResult)
    begin
      case Result of
        prAccordee, prDejaAccordee:
          // Prendre une photo avec la caméra
          PrendrePhotoCamera;
        prRefusee:
          // Proposer de choisir une photo existante
          TDialogService.MessageDialog(
            'Voulez-vous choisir une photo existante depuis votre galerie ?',
            TMsgDlgType.mtConfirmation,
            [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
            TMsgDlgBtn.mbYes, 0,
            procedure(const AResult: TModalResult)
            begin
              if AResult = mrYes then
                ChoisirPhotoGalerie;
            end);
      end;
    end);
end;
```

## Confidentialité des données

### Principes du RGPD

Le Règlement Général sur la Protection des Données (RGPD) s'applique à toutes les applications qui traitent des données de citoyens européens. Voici les principes clés :

**1. Consentement explicite** :
- L'utilisateur doit donner son accord clair
- Le consentement doit être libre, spécifique et informé
- Doit être aussi facile de retirer son consentement que de le donner

**2. Minimisation des données** :
- Ne collectez que les données strictement nécessaires
- Ne gardez les données que le temps nécessaire

**3. Transparence** :
- Informez clairement sur les données collectées
- Expliquez comment et pourquoi elles sont utilisées
- Indiquez avec qui elles sont partagées

**4. Droit d'accès et de rectification** :
- L'utilisateur peut consulter ses données
- L'utilisateur peut corriger des données erronées
- L'utilisateur peut demander la suppression de ses données

**5. Sécurité** :
- Protégez les données contre les accès non autorisés
- Chiffrez les données sensibles
- Notifiez en cas de violation de données

### Politique de confidentialité

Toute application collectant des données personnelles **doit** avoir une politique de confidentialité :

```pascal
procedure TFormMain.AfficherPolitiqueConfidentialite;
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open',
    PChar('https://votresite.com/politique-confidentialite'),
    nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  // Sur mobile
  var URL := 'https://votresite.com/politique-confidentialite';
  {$IFDEF ANDROID}
  var Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  TAndroidHelper.Context.startActivity(Intent);
  {$ENDIF}
  {$IFDEF IOS}
  SharedApplication.openURL(StrToNSUrl(URL));
  {$ENDIF}
  {$ENDIF}
end;

// Afficher lors de la première utilisation
procedure TFormMain.FormCreate(Sender: TObject);
begin
  if not LirePreference('politique_acceptee', False) then
  begin
    TDialogService.MessageDialog(
      'Bienvenue !' + sLineBreak + sLineBreak +
      'Pour utiliser cette application, veuillez lire et accepter notre ' +
      'politique de confidentialité.',
      TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK],
      TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        AfficherDialogueConsentement;
      end);
  end;
end;

procedure TFormMain.AfficherDialogueConsentement;
begin
  // Créer un formulaire avec la politique et une case à cocher
  var Form := TFormConsentement.Create(Self);
  try
    Form.ShowModal(
      procedure(ModalResult: TModalResult)
      begin
        if ModalResult = mrOK then
        begin
          SauvegarderPreference('politique_acceptee', True);
          SauvegarderPreference('date_acceptation', DateTimeToStr(Now));
        end
        else
        begin
          // L'utilisateur n'accepte pas : fermer l'application
          Application.Terminate;
        end;
      end);
  finally
    Form.Free;
  end;
end;
```

### Consentement pour le tracking

Depuis iOS 14.5, vous devez demander explicitement la permission pour tracker l'utilisateur :

```pascal
{$IFDEF IOS}
uses
  iOSapi.AppTrackingTransparency;

procedure TFormMain.DemanderPermissionTracking;
begin
  // Demander la permission de tracking sur iOS 14.5+
  if TOSVersion.Major >= 14 then
  begin
    ATTrackingManager.requestTrackingAuthorizationWithCompletionHandler(
      procedure(status: ATTrackingManagerAuthorizationStatus)
      begin
        case status of
          ATTrackingManagerAuthorizationStatusAuthorized:
            begin
              // Tracking autorisé
              ActiverAnalytics;
            end;
          ATTrackingManagerAuthorizationStatusDenied,
          ATTrackingManagerAuthorizationStatusRestricted:
            begin
              // Tracking refusé
              DesactiverAnalytics;
            end;
        end;
      end);
  end;
end;
{$ENDIF}
```

### Gestion du consentement dans l'application

Créez une page de paramètres de confidentialité :

```pascal
procedure TFormMain.AfficherParametresConfidentialite;
begin
  // Afficher les options de confidentialité
  SwitchAnalytics.IsChecked := LirePreference('analytics_enabled', False);
  SwitchPartageUsage.IsChecked := LirePreference('usage_sharing', False);
  SwitchNotificationsMarketing.IsChecked :=
    LirePreference('marketing_notif', False);
end;

procedure TFormMain.SwitchAnalyticsSwitch(Sender: TObject);
begin
  SauvegarderPreference('analytics_enabled', SwitchAnalytics.IsChecked);

  if SwitchAnalytics.IsChecked then
    ActiverAnalytics
  else
    DesactiverAnalytics;
end;

procedure TFormMain.BtnSupprimerDonneesClick(Sender: TObject);
begin
  TDialogService.MessageDialog(
    'Êtes-vous sûr de vouloir supprimer toutes vos données ?' + sLineBreak +
    'Cette action est irréversible.',
    TMsgDlgType.mtWarning,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
      begin
        SupprimerToutesLesDonnees;
        ShowMessage('Vos données ont été supprimées.');
      end;
    end);
end;
```

## Stockage sécurisé des données sensibles

### Ne jamais stocker en clair

```pascal
// ❌ MAUVAIS : Mot de passe en clair
procedure SauvegarderMotDePasse(const MotDePasse: string);
begin
  SauvegarderPreference('password', MotDePasse); // DANGEREUX !
end;

// ✅ BON : Utiliser un hash
uses
  System.Hash;

procedure SauvegarderMotDePasseSecurise(const MotDePasse: string);
var
  Hash: string;
begin
  // Hasher avec SHA256
  Hash := THashSHA2.GetHashString(MotDePasse);
  SauvegarderPreference('password_hash', Hash);
end;

function VerifierMotDePasse(const MotDePasse: string): Boolean;
var
  HashStocke, HashSaisi: string;
begin
  HashStocke := LirePreference('password_hash', '');
  HashSaisi := THashSHA2.GetHashString(MotDePasse);
  Result := HashStocke = HashSaisi;
end;
```

### Chiffrement des données sensibles

```pascal
uses
  System.NetEncoding;

// Chiffrer une chaîne (exemple simple avec Base64)
// En production, utilisez un vrai chiffrement (AES)
function ChiffrerDonnee(const Donnee: string): string;
begin
  Result := TNetEncoding.Base64.Encode(Donnee);
end;

function DechiffrerDonnee(const DonneeChiffree: string): string;
begin
  Result := TNetEncoding.Base64.Decode(DonneeChiffree);
end;

// Sauvegarder des données sensibles
procedure SauvegarderTokenAPI(const Token: string);
var
  TokenChiffre: string;
begin
  TokenChiffre := ChiffrerDonnee(Token);
  SauvegarderPreference('api_token', TokenChiffre);
end;

function RecupererTokenAPI: string;
var
  TokenChiffre: string;
begin
  TokenChiffre := LirePreference('api_token', '');
  if not TokenChiffre.IsEmpty then
    Result := DechiffrerDonnee(TokenChiffre)
  else
    Result := '';
end;
```

### Utiliser le Keychain (iOS) et Keystore (Android)

Pour les données très sensibles, utilisez les systèmes sécurisés du système :

```pascal
// Note : Ceci nécessite des composants tiers ou des API natives
// Exemple conceptuel

{$IFDEF IOS}
procedure SauvegarderDansKeychain(const Cle, Valeur: string);
begin
  // Utiliser l'API Keychain d'iOS
  // La valeur est stockée de manière sécurisée par le système
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure SauvegarderDansKeystore(const Cle, Valeur: string);
begin
  // Utiliser l'API Keystore d'Android
  // Les clés sont protégées par le matériel sur les appareils récents
end;
{$ENDIF}
```

## Bonnes pratiques

### 1. Principe du moindre privilège

Ne demandez que les permissions strictement nécessaires :

```pascal
// ❌ MAUVAIS : Demander toutes les permissions "au cas où"
DemanderPermissions([CAMERA, LOCATION, CONTACTS, MICROPHONE, ...]);

// ✅ BON : Demander seulement ce qui est nécessaire
if FonctionnaliteNecessiteCamera then
  DemanderPermission(CAMERA);
```

### 2. Demander au bon moment

```pascal
// ❌ MAUVAIS : Demander toutes les permissions au démarrage
procedure TFormMain.FormCreate(Sender: TObject);
begin
  DemanderToutesLesPermissions; // Bombardement de dialogues !
end;

// ✅ BON : Demander juste avant d'utiliser la fonctionnalité
procedure TFormMain.BtnPrendrePhotoClick(Sender: TObject);
begin
  // Demander seulement quand l'utilisateur veut prendre une photo
  DemanderPermission(CAMERA);
end;
```

### 3. Respecter les choix de l'utilisateur

```pascal
// Sauvegarder si l'utilisateur a explicitement refusé
procedure TFormMain.EnregistrerRefusPermission(const Permission: string);
begin
  SauvegarderPreference('permission_refusee_' + Permission, True);
  SauvegarderPreference('date_refus_' + Permission, DateTimeToStr(Now));
end;

// Ne pas harceler l'utilisateur
procedure TFormMain.DemanderPermissionRespectueuse(const Permission: string);
var
  DateRefus: TDateTime;
  JoursDepuisRefus: Integer;
begin
  if LirePreference('permission_refusee_' + Permission, False) then
  begin
    DateRefus := StrToDateTimeDef(
      LirePreference('date_refus_' + Permission, ''), 0);
    JoursDepuisRefus := DaysBetween(Now, DateRefus);

    // Ne redemander qu'après 7 jours
    if JoursDepuisRefus < 7 then
    begin
      ShowMessage('Cette fonctionnalité nécessite une autorisation que ' +
        'vous avez précédemment refusée.');
      Exit;
    end;
  end;

  // Demander la permission
  DemanderPermission(Permission);
end;
```

### 4. Transparence totale

```pascal
procedure TFormMain.AfficherUtilisationDonnees;
begin
  var Message :=
    'Données collectées par cette application :' + sLineBreak + sLineBreak +
    '• Votre adresse email (pour l''authentification)' + sLineBreak +
    '• Votre localisation (uniquement pendant l''utilisation)' + sLineBreak +
    '• Vos photos (stockées localement)' + sLineBreak + sLineBreak +
    'Ces données ne sont JAMAIS :' + sLineBreak +
    '• Vendues à des tiers' + sLineBreak +
    '• Partagées sans votre consentement' + sLineBreak +
    '• Utilisées à des fins publicitaires' + sLineBreak + sLineBreak +
    'Vous pouvez supprimer toutes vos données à tout moment dans les paramètres.';

  ShowMessage(Message);
end;
```

### 5. Anonymisation des données

```pascal
// Anonymiser les données avant l'envoi au serveur
function AnonymizerDonnees(const UserID: string): string;
var
  Hash: string;
begin
  // Utiliser un hash au lieu de l'ID réel
  Hash := THashSHA2.GetHashString(UserID + 'sel_secret');
  Result := Hash.Substring(0, 16); // Tronquer pour plus d'anonymat
end;

procedure EnvoyerStatistiques;
var
  Stats: TJSONObject;
begin
  Stats := TJSONObject.Create;
  try
    // Utiliser un ID anonyme
    Stats.AddPair('user_hash', AnonymizerDonnees(GetUserID));

    // Données agrégées seulement
    Stats.AddPair('actions_count', TJSONNumber.Create(GetActionsCount));
    Stats.AddPair('session_duration', TJSONNumber.Create(GetSessionDuration));

    // PAS de données personnelles identifiables
    // Stats.AddPair('email', UserEmail); // ❌ NON !

    EnvoyerAuServeur(Stats);
  finally
    Stats.Free;
  end;
end;
```

### 6. Audit et logs

```pascal
// Logger tous les accès aux données sensibles
procedure TFormMain.LoggerAccesDonnees(const Action, Donnee: string);
var
  LogEntry: TJSONObject;
begin
  LogEntry := TJSONObject.Create;
  try
    LogEntry.AddPair('timestamp', DateTimeToStr(Now));
    LogEntry.AddPair('action', Action);
    LogEntry.AddPair('data_type', Donnee);
    LogEntry.AddPair('user_id', AnonymizerDonnees(GetUserID));

    // Sauvegarder le log localement
    AjouterAuFichierLog(LogEntry.ToString);
  finally
    LogEntry.Free;
  end;
end;

// Utilisation
procedure TFormMain.AccederContacts;
begin
  LoggerAccesDonnees('READ', 'CONTACTS');
  // ... accéder aux contacts
end;
```

## Conformité avec les stores

### Google Play Store

Google exige maintenant une déclaration de sécurité des données :

**Informations à fournir** :
- Types de données collectées (localisation, contacts, photos, etc.)
- Si les données sont partagées avec des tiers
- Si les données sont chiffrées en transit
- Si l'utilisateur peut demander la suppression de ses données

**Dans Play Console** :
1. Allez dans "Contenu de l'application"
2. Remplissez la section "Sécurité des données"
3. Soyez honnête et complet

### Apple App Store

Apple est très strict sur la confidentialité :

**Privacy Nutrition Labels** (obligatoire) :
- Déclarez toutes les données collectées
- Précisez si elles sont liées à l'identité de l'utilisateur
- Indiquez si elles sont utilisées pour le tracking

**App Tracking Transparency** (iOS 14.5+) :
- Demandez la permission pour tout tracking cross-app
- Fournissez une explication claire dans le dialogue système

## Conclusion

La gestion des permissions et la protection de la vie privée ne sont pas des contraintes, mais des opportunités de créer une relation de confiance avec vos utilisateurs. En respectant leur vie privée, vous construisez une application durable et appréciée.

**Points clés à retenir** :

1. **Permissions** : Ne demandez que ce qui est nécessaire, au bon moment
2. **Transparence** : Expliquez clairement pourquoi vous avez besoin de chaque permission
3. **Respect** : Acceptez les refus et proposez des alternatives
4. **Sécurité** : Protégez les données que vous collectez
5. **RGPD** : Respectez les droits des utilisateurs (accès, rectification, suppression)
6. **Consentement** : Obtenez un accord explicite et informé
7. **Minimisation** : Collectez le minimum de données nécessaires
8. **Audit** : Loggez et surveillez les accès aux données sensibles

Une application respectueuse de la vie privée n'est pas seulement une obligation légale, c'est aussi un avantage concurrentiel. Les utilisateurs sont de plus en plus conscients de leurs droits et choisissent les applications qui les respectent.

En suivant ces principes et en implémentant correctement les systèmes de permissions, vous créerez des applications mobiles sécurisées, conformes aux réglementations et dignes de confiance. Vos utilisateurs vous en remercieront, et les stores aussi !

⏭️ [Intégration des services Firebase](/15-applications-mobiles-avec-delphi/11-integration-des-services-firebase.md)
