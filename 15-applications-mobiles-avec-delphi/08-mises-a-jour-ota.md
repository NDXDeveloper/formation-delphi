# 15.8 Mises à jour OTA (Over The Air)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les mises à jour OTA (Over The Air) permettent de mettre à jour certains aspects de votre application sans passer par les processus de validation des magasins d'applications, ce qui peut considérablement accélérer le déploiement de correctifs et de nouvelles fonctionnalités. Dans cette section, nous explorons comment implémenter ce mécanisme dans vos applications mobiles Delphi.

## Comprendre les mises à jour OTA

### Qu'est-ce qu'une mise à jour OTA ?

Une mise à jour OTA (Over The Air) est un mécanisme qui permet de mettre à jour le contenu, les ressources ou certaines parties de votre application directement à partir d'un serveur, sans avoir à soumettre une nouvelle version aux magasins d'applications.

### Ce qui peut être mis à jour via OTA

- **Contenu textuel** : textes, traductions, descriptions
- **Images et ressources** : icônes, bannières, illustrations
- **Données de configuration** : paramètres, URLs d'API
- **Mises en page** : position et visibilité des éléments d'interface
- **Fichiers HTML/JavaScript** : pour les webviews intégrées
- **Scripts ou règles métier** : logique simple interprétée par l'application

### Ce qui ne peut pas être mis à jour via OTA

- **Code natif compilé** : le binaire principal de l'application
- **Fonctionnalités systèmes** : permissions, intégrations profondes avec le système
- **Structure de l'application** : changements majeurs dans l'architecture

### Avantages des mises à jour OTA

1. **Réactivité** : déploiement immédiat sans attendre la validation des stores
2. **Flexibilité** : modification du comportement de l'application en production
3. **Correction rapide** : résolution des problèmes sans soumettre une nouvelle version
4. **Tests A/B** : déploiement de différentes versions à différents utilisateurs
5. **Personnalisation** : adaptation du contenu en fonction des utilisateurs

## Conception d'un système de mise à jour OTA

Pour implémenter un système de mise à jour OTA efficace, vous devez suivre ces principes de conception :

### 1. Architecture modulaire

Structurez votre application en composants qui peuvent être mis à jour indépendamment.

### 2. Système de contrôle de version

Mettez en place un mécanisme pour suivre les versions des différents composants et identifier les mises à jour nécessaires.

### 3. Sécurité

Assurez-vous que les mises à jour proviennent bien de votre serveur et n'ont pas été altérées en cours de route.

### 4. Résilience aux erreurs

Prévoyez des mécanismes de secours en cas d'échec de mise à jour pour éviter de bloquer l'application.

### 5. Expérience utilisateur cohérente

Informez l'utilisateur des mises à jour sans perturber son expérience.

## Implémentation des mises à jour OTA avec Delphi

Voyons comment mettre en œuvre un système de mise à jour OTA dans une application Delphi.

### Étape 1 : Créer une structure pour les ressources mises à jour

Commencez par définir la structure des données que vous souhaitez mettre à jour. Voici un exemple avec un fichier de configuration JSON :

```pascal
type
  TAppConfig = class
  private
    FVersion: Integer;
    FWelcomeMessage: string;
    FFeatureEnabled: Boolean;
    FServerEndpoint: string;
    FThemeColors: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJSON(const JSON: string);
    function SaveToJSON: string;

    // Propriétés
    property Version: Integer read FVersion write FVersion;
    property WelcomeMessage: string read FWelcomeMessage write FWelcomeMessage;
    property FeatureEnabled: Boolean read FFeatureEnabled write FFeatureEnabled;
    property ServerEndpoint: string read FServerEndpoint write FServerEndpoint;
    property ThemeColors: TDictionary<string, string> read FThemeColors;
  end;

constructor TAppConfig.Create;
begin
  inherited Create;
  FVersion := 1;
  FWelcomeMessage := 'Bienvenue dans notre application !';
  FFeatureEnabled := False;
  FServerEndpoint := 'https://api.example.com/v1/';
  FThemeColors := TDictionary<string, string>.Create;

  // Couleurs par défaut
  FThemeColors.Add('primary', '#3498db');
  FThemeColors.Add('secondary', '#2ecc71');
  FThemeColors.Add('accent', '#e74c3c');
end;

destructor TAppConfig.Destroy;
begin
  FThemeColors.Free;
  inherited;
end;

procedure TAppConfig.LoadFromJSON(const JSON: string);
var
  JSONObj: TJSONObject;
  ColorsObj: TJSONObject;
  ColorPair: TJSONPair;
begin
  if JSON = '' then
    Exit;

  try
    JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    if JSONObj <> nil then
    try
      // Charger les propriétés de base
      if JSONObj.TryGetValue<Integer>('version', FVersion) then;
      if JSONObj.TryGetValue<string>('welcomeMessage', FWelcomeMessage) then;
      if JSONObj.TryGetValue<Boolean>('featureEnabled', FFeatureEnabled) then;
      if JSONObj.TryGetValue<string>('serverEndpoint', FServerEndpoint) then;

      // Charger les couleurs
      if JSONObj.TryGetValue('themeColors', ColorsObj) and (ColorsObj <> nil) then
      begin
        FThemeColors.Clear;

        for ColorPair in ColorsObj do
        begin
          if ColorPair.JsonValue is TJSONString then
            FThemeColors.Add(ColorPair.JsonString.Value,
                            (ColorPair.JsonValue as TJSONString).Value);
        end;
      end;
    finally
      JSONObj.Free;
    end;
  except
    // Gérer les erreurs d'analyse JSON
    // En cas d'erreur, on garde les valeurs par défaut
  end;
end;

function TAppConfig.SaveToJSON: string;
var
  JSONObj: TJSONObject;
  ColorsObj: TJSONObject;
  Color: TPair<string, string>;
begin
  JSONObj := TJSONObject.Create;
  try
    // Enregistrer les propriétés de base
    JSONObj.AddPair('version', TJSONNumber.Create(FVersion));
    JSONObj.AddPair('welcomeMessage', FWelcomeMessage);
    JSONObj.AddPair('featureEnabled', TJSONBool.Create(FFeatureEnabled));
    JSONObj.AddPair('serverEndpoint', FServerEndpoint);

    // Enregistrer les couleurs
    ColorsObj := TJSONObject.Create;
    for Color in FThemeColors do
      ColorsObj.AddPair(Color.Key, Color.Value);

    JSONObj.AddPair('themeColors', ColorsObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;
```

### Étape 2 : Créer un gestionnaire de mise à jour OTA

Ensuite, nous allons créer une classe qui gère le téléchargement et l'application des mises à jour :

```pascal
uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.IOUtils, System.Threading;

type
  TOTAUpdateStatus = (usUpToDate, usUpdateAvailable, usUpdating, usUpdateSuccessful,
                     usUpdateFailed, usNoConnection);

  TOTAUpdateManager = class
  private
    FAppConfig: TAppConfig;
    FUpdateStatus: TOTAUpdateStatus;
    FUpdateCheckURL: string;
    FUpdateDownloadURL: string;
    FLocalConfigPath: string;
    FOnUpdateStatusChanged: TProc<TOTAUpdateStatus>;
    FOnConfigUpdated: TProc<TAppConfig>;

    procedure SetUpdateStatus(const Value: TOTAUpdateStatus);
    function CheckForUpdates: Boolean;
    function DownloadUpdate: Boolean;
    function ApplyUpdate(const UpdateJSON: string): Boolean;
    procedure SaveConfigLocally;
    procedure LoadConfigLocally;
  public
    constructor Create(const UpdateServerURL: string);
    destructor Destroy; override;

    procedure CheckAndUpdate;
    procedure ForceUpdate;

    property AppConfig: TAppConfig read FAppConfig;
    property UpdateStatus: TOTAUpdateStatus read FUpdateStatus write SetUpdateStatus;
    property OnUpdateStatusChanged: TProc<TOTAUpdateStatus>
      read FOnUpdateStatusChanged write FOnUpdateStatusChanged;
    property OnConfigUpdated: TProc<TAppConfig>
      read FOnConfigUpdated write FOnConfigUpdated;
  end;

constructor TOTAUpdateManager.Create(const UpdateServerURL: string);
begin
  inherited Create;
  FAppConfig := TAppConfig.Create;
  FUpdateStatus := usUpToDate;

  // Configurer les URLs
  FUpdateCheckURL := UpdateServerURL + '/check';
  FUpdateDownloadURL := UpdateServerURL + '/download';

  // Définir le chemin de stockage local
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  FLocalConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'app_config.json');
  {$ELSE}
  FLocalConfigPath := TPath.Combine(TPath.GetHomePath, 'app_config.json');
  {$ENDIF}

  // Charger la configuration locale si elle existe
  LoadConfigLocally;
end;

destructor TOTAUpdateManager.Destroy;
begin
  FAppConfig.Free;
  inherited;
end;

procedure TOTAUpdateManager.SetUpdateStatus(const Value: TOTAUpdateStatus);
begin
  if FUpdateStatus <> Value then
  begin
    FUpdateStatus := Value;

    // Notifier du changement de statut
    if Assigned(FOnUpdateStatusChanged) then
      FOnUpdateStatusChanged(FUpdateStatus);
  end;
end;

procedure TOTAUpdateManager.LoadConfigLocally;
begin
  // Vérifier si le fichier de configuration existe
  if TFile.Exists(FLocalConfigPath) then
  try
    // Charger le contenu du fichier
    var JSONContent := TFile.ReadAllText(FLocalConfigPath);
    FAppConfig.LoadFromJSON(JSONContent);
  except
    // En cas d'erreur, on garde la configuration par défaut
  end;
end;

procedure TOTAUpdateManager.SaveConfigLocally;
begin
  try
    // Enregistrer la configuration dans un fichier local
    var JSONContent := FAppConfig.SaveToJSON;
    TFile.WriteAllText(FLocalConfigPath, JSONContent);
  except
    // Gérer les erreurs d'écriture de fichier
  end;
end;

function TOTAUpdateManager.CheckForUpdates: Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  RequestParams: TStringList;
  ResponseObj: TJSONObject;
  AvailableVersion: Integer;
begin
  Result := False;

  Client := THTTPClient.Create;
  RequestParams := TStringList.Create;
  try
    // Ajouter la version actuelle comme paramètre
    RequestParams.Add('currentVersion=' + FAppConfig.Version.ToString);
    RequestParams.Add('platform=' + {$IF DEFINED(ANDROID)}'android'{$ELSEIF DEFINED(IOS)}'ios'{$ELSE}'desktop'{$ENDIF});

    try
      // Envoyer la requête au serveur
      Response := Client.Get(FUpdateCheckURL + '?' + RequestParams.DelimitedText);

      // Vérifier la réponse
      if Response.StatusCode = 200 then
      begin
        ResponseObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        if ResponseObj <> nil then
        try
          // Extraire la version disponible
          AvailableVersion := ResponseObj.GetValue<Integer>('latestVersion');

          // Vérifier si une mise à jour est disponible
          Result := AvailableVersion > FAppConfig.Version;

          if Result then
            UpdateStatus := usUpdateAvailable
          else
            UpdateStatus := usUpToDate;
        finally
          ResponseObj.Free;
        end;
      end;
    except
      // Gérer les erreurs de connexion
      UpdateStatus := usNoConnection;
    end;
  finally
    RequestParams.Free;
    Client.Free;
  end;
end;

function TOTAUpdateManager.DownloadUpdate: Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  RequestParams: TStringList;
begin
  Result := False;

  UpdateStatus := usUpdating;

  Client := THTTPClient.Create;
  RequestParams := TStringList.Create;
  try
    // Ajouter les paramètres de requête
    RequestParams.Add('currentVersion=' + FAppConfig.Version.ToString);
    RequestParams.Add('platform=' + {$IF DEFINED(ANDROID)}'android'{$ELSEIF DEFINED(IOS)}'ios'{$ELSE}'desktop'{$ENDIF});

    try
      // Télécharger la mise à jour
      Response := Client.Get(FUpdateDownloadURL + '?' + RequestParams.DelimitedText);

      // Vérifier la réponse
      if Response.StatusCode = 200 then
        Result := ApplyUpdate(Response.ContentAsString)
      else
        UpdateStatus := usUpdateFailed;
    except
      // Gérer les erreurs de connexion
      UpdateStatus := usNoConnection;
    end;
  finally
    RequestParams.Free;
    Client.Free;
  end;
end;

function TOTAUpdateManager.ApplyUpdate(const UpdateJSON: string): Boolean;
begin
  Result := False;

  try
    // Appliquer la mise à jour à la configuration
    FAppConfig.LoadFromJSON(UpdateJSON);

    // Sauvegarder localement
    SaveConfigLocally;

    // Notifier que la configuration a été mise à jour
    if Assigned(FOnConfigUpdated) then
      FOnConfigUpdated(FAppConfig);

    UpdateStatus := usUpdateSuccessful;
    Result := True;
  except
    UpdateStatus := usUpdateFailed;
  end;
end;

procedure TOTAUpdateManager.CheckAndUpdate;
begin
  // Exécuter la vérification et la mise à jour en arrière-plan
  TTask.Run(procedure
  begin
    // Vérifier si une mise à jour est disponible
    if CheckForUpdates then
    begin
      // Si c'est le cas, télécharger et appliquer la mise à jour
      DownloadUpdate;
    end;
  end);
end;

procedure TOTAUpdateManager.ForceUpdate;
begin
  TTask.Run(procedure
  begin
    // Forcer le téléchargement et l'application de la mise à jour
    DownloadUpdate;
  end);
end;
```

### Étape 3 : Intégrer le gestionnaire de mise à jour dans votre application

Voici comment utiliser ce gestionnaire dans votre application :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, OTAUpdateManager;

type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    btnCheckUpdate: TButton;
    lblStatus: TLabel;
    lblWelcome: TLabel;
    SwitchFeature: TSwitch;
    lblFeature: TLabel;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCheckUpdateClick(Sender: TObject);
    procedure SwitchFeatureSwitch(Sender: TObject);
  private
    FUpdateManager: TOTAUpdateManager;
    procedure HandleUpdateStatusChanged(Status: TOTAUpdateStatus);
    procedure HandleConfigUpdated(Config: TAppConfig);
    procedure ApplyConfiguration;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Créer le gestionnaire de mise à jour
  FUpdateManager := TOTAUpdateManager.Create('https://votre-serveur.com/api/updates');

  // Configurer les gestionnaires d'événements
  FUpdateManager.OnUpdateStatusChanged := HandleUpdateStatusChanged;
  FUpdateManager.OnConfigUpdated := HandleConfigUpdated;

  // Appliquer la configuration initiale
  ApplyConfiguration;

  // Vérifier les mises à jour au démarrage
  FUpdateManager.CheckAndUpdate;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FUpdateManager.Free;
end;

procedure TFormMain.HandleUpdateStatusChanged(Status: TOTAUpdateStatus);
begin
  // Mettre à jour l'interface en fonction du statut
  TThread.Synchronize(nil, procedure
  begin
    case Status of
      usUpToDate:
        lblStatus.Text := 'Application à jour';
      usUpdateAvailable:
        lblStatus.Text := 'Mise à jour disponible';
      usUpdating:
        lblStatus.Text := 'Mise à jour en cours...';
      usUpdateSuccessful:
        lblStatus.Text := 'Mise à jour réussie';
      usUpdateFailed:
        lblStatus.Text := 'Échec de la mise à jour';
      usNoConnection:
        lblStatus.Text := 'Pas de connexion';
    end;

    // Activer/désactiver le bouton selon le statut
    btnCheckUpdate.Enabled := (Status <> usUpdating);
  end);
end;

procedure TFormMain.HandleConfigUpdated(Config: TAppConfig);
begin
  // Appliquer la nouvelle configuration
  TThread.Synchronize(nil, procedure
  begin
    ApplyConfiguration;
  end);
end;

procedure TFormMain.ApplyConfiguration;
var
  PrimaryColor: Cardinal;
begin
  // Appliquer les paramètres de configuration à l'interface

  // Message de bienvenue
  lblWelcome.Text := FUpdateManager.AppConfig.WelcomeMessage;

  // État de la fonctionnalité
  SwitchFeature.IsChecked := FUpdateManager.AppConfig.FeatureEnabled;
  SwitchFeature.Enabled := FUpdateManager.AppConfig.FeatureEnabled;
  lblFeature.Visible := FUpdateManager.AppConfig.FeatureEnabled;

  // Couleurs du thème
  if FUpdateManager.AppConfig.ThemeColors.TryGetValue('primary', var ColorStr) then
  begin
    if TryStrToUInt('$' + ColorStr.Replace('#', ''), PrimaryColor) then
      Rectangle1.Fill.Color := PrimaryColor;
  end;

  // Afficher la version actuelle
  lblStatus.Text := 'Version: ' + FUpdateManager.AppConfig.Version.ToString;
end;

procedure TFormMain.btnCheckUpdateClick(Sender: TObject);
begin
  // Vérifier manuellement les mises à jour
  FUpdateManager.CheckAndUpdate;
end;

procedure TFormMain.SwitchFeatureSwitch(Sender: TObject);
begin
  // Cette fonctionnalité est contrôlée par la configuration
  // Si elle est désactivée dans la configuration, l'utilisateur ne peut pas l'activer
  SwitchFeature.IsChecked := FUpdateManager.AppConfig.FeatureEnabled and SwitchFeature.IsChecked;
end;
```

## Configuration du serveur pour les mises à jour OTA

Pour que les mises à jour OTA fonctionnent, vous avez besoin d'un serveur qui distribue les configurations. Voici un exemple simplifié d'une API de mise à jour en PHP :

```php
<?php
// Fichier: check.php
// Ce script vérifie si une mise à jour est disponible

// Récupérer les paramètres
$currentVersion = isset($_GET['currentVersion']) ? intval($_GET['currentVersion']) : 0;
$platform = isset($_GET['platform']) ? $_GET['platform'] : '';

// Déterminer la dernière version disponible
// Dans un scénario réel, cela viendrait d'une base de données
$latestVersions = [
    'android' => 3,
    'ios' => 3,
    'desktop' => 2
];

// Obtenir la dernière version pour cette plateforme
$latestVersion = isset($latestVersions[$platform]) ? $latestVersions[$platform] : 1;

// Préparer la réponse
$response = [
    'latestVersion' => $latestVersion,
    'updateAvailable' => ($latestVersion > $currentVersion)
];

// Envoyer la réponse en JSON
header('Content-Type: application/json');
echo json_encode($response);
?>

<?php
// Fichier: download.php
// Ce script fournit la configuration mise à jour

// Récupérer les paramètres
$currentVersion = isset($_GET['currentVersion']) ? intval($_GET['currentVersion']) : 0;
$platform = isset($_GET['platform']) ? $_GET['platform'] : '';

// Charger la configuration selon la plateforme
// Dans un scénario réel, cela viendrait d'une base de données
$configs = [
    'android' => [
        'version' => 3,
        'welcomeMessage' => 'Bienvenue dans la nouvelle version Android !',
        'featureEnabled' => true,
        'serverEndpoint' => 'https://api.example.com/v2/',
        'themeColors' => [
            'primary' => '#4285F4',
            'secondary' => '#34A853',
            'accent' => '#EA4335'
        ]
    ],
    'ios' => [
        'version' => 3,
        'welcomeMessage' => 'Bienvenue dans la nouvelle version iOS !',
        'featureEnabled' => true,
        'serverEndpoint' => 'https://api.example.com/v2/',
        'themeColors' => [
            'primary' => '#007AFF',
            'secondary' => '#34C759',
            'accent' => '#FF3B30'
        ]
    ],
    'desktop' => [
        'version' => 2,
        'welcomeMessage' => 'Bienvenue dans la nouvelle version Desktop !',
        'featureEnabled' => false,
        'serverEndpoint' => 'https://api.example.com/v1/',
        'themeColors' => [
            'primary' => '#0078D7',
            'secondary' => '#107C10',
            'accent' => '#D83B01'
        ]
    ]
];

// Obtenir la configuration pour cette plateforme
$config = isset($configs[$platform]) ? $configs[$platform] : $configs['desktop'];

// Envoyer la configuration en JSON
header('Content-Type: application/json');
echo json_encode($config);
?>
```

Ce code simplifié illustre comment un serveur peut fournir des mises à jour de configuration. Dans un environnement de production, vous auriez besoin d'ajouter :

1. **Authentification** pour sécuriser votre API
2. **Journalisation** pour suivre les mises à jour
3. **Versionnement plus sophistiqué** pour gérer des mises à jour plus complexes
4. **Rollback** pour revenir en arrière en cas de problème

## Types de mises à jour OTA spécifiques

### Mise à jour du contenu multimédia

Pour les ressources comme les images :

```pascal
procedure TFormMain.UpdateImages;
var
  URL: string;
  FileName: string;
begin
  // URL de l'image à télécharger
  URL := FUpdateManager.AppConfig.ServerEndpoint + 'assets/banner.jpg';

  // Chemin local pour enregistrer l'image
  FileName := TPath.Combine(TPath.GetDocumentsPath, 'banner.jpg');

  // Télécharger l'image en arrière-plan
  TTask.Run(procedure
  var
    Client: THTTPClient;
    Response: IHTTPResponse;
    FileStream: TFileStream;
  begin
    Client := THTTPClient.Create;
    try
      // Télécharger l'image
      Response := Client.Get(URL);

      if Response.StatusCode = 200 then
      begin
        // Enregistrer l'image téléchargée
        FileStream := TFileStream.Create(FileName, fmCreate);
        try
          FileStream.WriteBuffer(Response.ContentAsBytes[0], Length(Response.ContentAsBytes));
        finally
          FileStream.Free;
        end;

        // Charger l'image dans l'interface
        TThread.Synchronize(nil, procedure
        begin
          Image1.Bitmap.LoadFromFile(FileName);
        end);
      end;
    finally
      Client.Free;
    end;
  end);
end;
```

### Mise à jour de contenu HTML pour WebView

Si votre application utilise une WebView pour afficher du contenu, vous pouvez mettre à jour ce contenu via OTA :

```pascal
procedure TFormMain.UpdateWebContent;
var
  URL: string;
  LocalHTMLPath: string;
begin
  // URL du contenu HTML à télécharger
  URL := FUpdateManager.AppConfig.ServerEndpoint + 'content/help.html';

  // Chemin local pour enregistrer le HTML
  LocalHTMLPath := TPath.Combine(TPath.GetDocumentsPath, 'help.html');

  // Télécharger le contenu en arrière-plan
  TTask.Run(procedure
  var
    Client: THTTPClient;
    Response: IHTTPResponse;
  begin
    Client := THTTPClient.Create;
    try
      // Télécharger le contenu HTML
      Response := Client.Get(URL);

      if Response.StatusCode = 200 then
      begin
        // Enregistrer le HTML téléchargé
        TFile.WriteAllText(LocalHTMLPath, Response.ContentAsString);

        // Charger le HTML dans la WebView
        TThread.Synchronize(nil, procedure
        begin
          WebBrowser1.Navigate('file://' + LocalHTMLPath);
        end);
      end;
    finally
      Client.Free;
    end;
  end);
end;
```

### Mise à jour des traductions

Pour les applications multilingues, vous pouvez mettre à jour les traductions :

```pascal
unit LocalizationManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON;

type
  TLocalizationManager = class
  private
    FStrings: TDictionary<string, string>;
    FCurrentLanguage: string;
    FVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetString(const Key: string; const DefaultValue: string = ''): string;
    procedure LoadFromJSON(const JSON: string);

    property CurrentLanguage: string read FCurrentLanguage;
    property Version: Integer read FVersion;
  end;

implementation

constructor TLocalizationManager.Create;
begin
  inherited Create;
  FStrings := TDictionary<string, string>.Create;
  FCurrentLanguage := 'en';
  FVersion := 1;
end;

destructor TLocalizationManager.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TLocalizationManager.GetString(const Key: string;
                                      const DefaultValue: string = ''): string;
begin
  if not FStrings.TryGetValue(Key, Result) then
    Result := DefaultValue;
end;

procedure TLocalizationManager.LoadFromJSON(const JSON: string);
var
  JSONObj, StringsObj: TJSONObject;
  StringPair: TJSONPair;
begin
  if JSON = '' then
    Exit;

  try
    JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    if JSONObj <> nil then
    try
      // Charger les métadonnées
      if JSONObj.TryGetValue<string>('language', FCurrentLanguage) then;
      if JSONObj.TryGetValue<Integer>('version', FVersion) then;

      // Charger les chaînes
      if JSONObj.TryGetValue('strings', StringsObj) and (StringsObj <> nil) then
      begin
        FStrings.Clear;

        for StringPair in StringsObj do
        begin
          if StringPair.JsonValue is TJSONString then
            FStrings.Add(StringPair.JsonString.Value,
                        (StringPair.JsonValue as TJSONString).Value);
        end;
      end;
    finally
      JSONObj.Free;
    end;
  except
    // Gérer les erreurs d'analyse JSON
  end;
end;
```

Pour utiliser ce gestionnaire de traductions avec notre système de mise à jour OTA, il suffit d'ajouter le téléchargement des fichiers de langue :

```pascal
procedure TFormMain.UpdateTranslations;
var
  URL: string;
begin
  // URL du fichier de traduction à télécharger
  URL := FUpdateManager.AppConfig.ServerEndpoint + 'languages/' +
         GetDeviceLanguage() + '.json';

  // Télécharger les traductions en arrière-plan
  TTask.Run(procedure
  var
    Client: THTTPClient;
    Response: IHTTPResponse;
  begin
    Client := THTTPClient.Create;
    try
      // Télécharger le fichier de langue
      Response := Client.Get(URL);

      if Response.StatusCode = 200 then
      begin
        // Appliquer les traductions
        LocalizationManager.LoadFromJSON(Response.ContentAsString);

        // Mettre à jour l'interface
        TThread.Synchronize(nil, procedure
        begin
          // Mettre à jour tous les textes de l'interface
          lblWelcome.Text := LocalizationManager.GetString('welcome', 'Bienvenue');
          lblFeature.Text := LocalizationManager.GetString('feature', 'Fonctionnalité');
          btnCheckUpdate.Text := LocalizationManager.GetString('check_update', 'Vérifier les mises à jour');

          // Sauvegarder la langue actuelle dans les préférences
          SaveCurrentLanguage(LocalizationManager.CurrentLanguage);
        end);
      end;
    finally
      Client.Free;
    end;
  end);
end;

function TFormMain.GetDeviceLanguage: string;
begin
  {$IF DEFINED(ANDROID)}
  Result := JStringToString(TJLocale.JavaClass.getDefault.getLanguage);
  {$ELSEIF DEFINED(IOS)}
  Result := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale).languageCode;
  {$ELSE}
  Result := 'en'; // Fallback pour les autres plateformes
  {$ENDIF}
end;
```

## Bonnes pratiques pour les mises à jour OTA

Pour implémenter des mises à jour OTA efficaces et sécurisées, suivez ces bonnes pratiques :

### 1. Sécurité avant tout

- **Utilisez HTTPS** pour toutes les communications
- **Validez l'intégrité des données** téléchargées (hachage, signatures)
- **Limitez les permissions** de votre application aux stricts besoins

Exemple d'implémentation de vérification d'intégrité :

```pascal
function VerifyFileIntegrity(const FilePath, ExpectedHash: string): Boolean;
var
  FileStream: TFileStream;
  MD5Context: THashMD5;
  Hash: TBytes;
  HashStr: string;
begin
  Result := False;

  if not TFile.Exists(FilePath) then
    Exit;

  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Hash := THashMD5.GetHashBytes(FileStream);
    HashStr := THash.DigestAsString(Hash).ToLower;
    Result := (HashStr = ExpectedHash.ToLower);
  finally
    FileStream.Free;
  end;
end;
```

### 2. Gestion hors ligne

- **Stockez localement** les mises à jour téléchargées
- **Appliquez les mises à jour** même en l'absence de connexion
- **Priorisez la fonctionnalité hors ligne**

### 3. Expérience utilisateur

- **Informez l'utilisateur** des mises à jour disponibles
- **Affichez une progression** pendant le téléchargement
- **Permettez les mises à jour manuelles** ou automatiques selon les préférences

Exemple d'interface pour la gestion des mises à jour :

```pascal
procedure TFormMain.ShowUpdateDialog;
begin
  // Créer une boîte de dialogue personnalisée
  var Dialog := TDialogUpdate.Create(Self);
  try
    // Configurer la boîte de dialogue
    Dialog.LabelVersion.Text := 'Version actuelle : ' +
                               FUpdateManager.AppConfig.Version.ToString;
    Dialog.LabelAvailable.Text := 'Nouvelle version disponible !';
    Dialog.CheckAutoUpdate.IsChecked := GetAutoUpdatePreference;

    // Afficher la boîte de dialogue
    if Dialog.ShowModal = mrOk then
    begin
      // Démarrer la mise à jour
      FUpdateManager.ForceUpdate;

      // Sauvegarder la préférence de mise à jour automatique
      SaveAutoUpdatePreference(Dialog.CheckAutoUpdate.IsChecked);
    end;
  finally
    Dialog.Free;
  end;
end;
```

### 4. Optimisation des performances

- **Utilisez la compression** pour réduire la taille des données
- **Implémentez des mises à jour différentielles** pour économiser la bande passante
- **Mettez en cache** les ressources fréquemment utilisées

### 5. Monitoring et analyse

- **Suivez le taux de succès** des mises à jour
- **Collectez des métriques** sur l'utilisation des fonctionnalités
- **Implémentez des logs détaillés** pour le débogage

Exemple d'implémentation de journalisation :

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogger = class
  private
    FLogPath: string;
    FEnabled: Boolean;
    procedure WriteToFile(const Level: TLogLevel; const Message: string);
  public
    constructor Create(const LogFileName: string);

    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

constructor TLogger.Create(const LogFileName: string);
begin
  inherited Create;
  FEnabled := True;
  FLogPath := TPath.Combine(TPath.GetDocumentsPath, LogFileName);
end;

procedure TLogger.WriteToFile(const Level: TLogLevel; const Message: string);
var
  LogFile: TextFile;
  LevelStr: string;
begin
  if not FEnabled then
    Exit;

  case Level of
    llDebug:   LevelStr := 'DEBUG';
    llInfo:    LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError:   LevelStr := 'ERROR';
  end;

  try
    if not TFile.Exists(FLogPath) then
    begin
      AssignFile(LogFile, FLogPath);
      Rewrite(LogFile);
    end
    else
    begin
      AssignFile(LogFile, FLogPath);
      Append(LogFile);
    end;

    WriteLn(LogFile, Format('%s [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LevelStr, Message]));

    CloseFile(LogFile);
  except
    // Ignorer les erreurs d'écriture du fichier de log
  end;
end;

procedure TLogger.Debug(const Message: string);
begin
  WriteToFile(llDebug, Message);
end;

procedure TLogger.Info(const Message: string);
begin
  WriteToFile(llInfo, Message);
end;

procedure TLogger.Warning(const Message: string);
begin
  WriteToFile(llWarning, Message);
end;

procedure TLogger.Error(const Message: string);
begin
  WriteToFile(llError, Message);
end;
```

## Gestion des versions et de la compatibilité

La gestion des versions est cruciale pour les mises à jour OTA. Voici quelques stratégies :

### 1. Versionnement sémantique

Adoptez un système de versionnement clair :

- **Version majeure** : changements incompatibles
- **Version mineure** : nouvelles fonctionnalités compatibles
- **Patch** : corrections de bugs

### 2. Migrations de schéma

Si vos mises à jour OTA modifient la structure des données, prévoyez des migrations :

```pascal
procedure TMigrationManager.ApplyMigrations;
var
  CurrentVersion, TargetVersion: Integer;
begin
  // Déterminer la version actuelle
  CurrentVersion := GetCurrentSchemaVersion;

  // Déterminer la version cible
  TargetVersion := GetTargetSchemaVersion;

  // Appliquer les migrations séquentiellement
  while CurrentVersion < TargetVersion do
  begin
    CurrentVersion := CurrentVersion + 1;

    case CurrentVersion of
      2: MigrateToV2;
      3: MigrateToV3;
      4: MigrateToV4;
      // Etc.
    end;

    // Mettre à jour la version actuelle
    SaveCurrentSchemaVersion(CurrentVersion);
  end;
end;
```

### 3. Fallback et rollback

Prévoyez toujours un mécanisme de secours en cas d'échec :

```pascal
procedure TOTAUpdateManager.ApplyUpdateWithFallback(const UpdateJSON: string);
var
  BackupJSON: string;
begin
  // Sauvegarder la configuration actuelle
  BackupJSON := FAppConfig.SaveToJSON;

  try
    // Tenter d'appliquer la mise à jour
    if not ApplyUpdate(UpdateJSON) then
      raise Exception.Create('Failed to apply update');

    // Tester la configuration mise à jour
    if not TestConfiguration then
    begin
      // Si le test échoue, restaurer la configuration précédente
      FAppConfig.LoadFromJSON(BackupJSON);
      SaveConfigLocally;

      UpdateStatus := usUpdateFailed;
      raise Exception.Create('Update validation failed');
    end;
  except
    // En cas d'erreur, restaurer la configuration
    FAppConfig.LoadFromJSON(BackupJSON);
    SaveConfigLocally;

    UpdateStatus := usUpdateFailed;
  end;
end;

function TOTAUpdateManager.TestConfiguration: Boolean;
begin
  // Implementer des tests de validation
  // Par exemple, vérifier que les URLs sont valides,
  // que les valeurs sont dans des plages acceptables, etc.
  Result := True;

  try
    // Tester les URLs
    if not IsValidURL(FAppConfig.ServerEndpoint) then
      Exit(False);

    // Autres tests selon vos besoins
  except
    Result := False;
  end;
end;
```

## Techniques avancées pour les mises à jour OTA

### Mise à jour sélective basée sur l'utilisateur

Vous pouvez mettre en place un système de déploiement progressif :

```pascal
function ShouldReceiveUpdate(const UserID: string; RolloutPercentage: Integer): Boolean;
var
  Hash: Cardinal;
  HashStr: string;
begin
  // Générer un hash déterministe basé sur l'ID utilisateur
  HashStr := UserID;
  Hash := 0;

  for var I := 1 to Length(HashStr) do
    Hash := ((Hash shl 5) + Hash) + Ord(HashStr[I]);

  // Convertir en pourcentage (0-99)
  Hash := Hash mod 100;

  // Déterminer si l'utilisateur doit recevoir la mise à jour
  Result := Hash < RolloutPercentage;
end;
```

### Tests A/B avec mises à jour OTA

Vous pouvez utiliser les mises à jour OTA pour tester différentes variantes :

```pascal
procedure TFormMain.InitializeABTest;
var
  UserGroup: string;
  TestConfig: string;
begin
  // Déterminer le groupe de test de l'utilisateur
  UserGroup := GetUserTestGroup;

  // URL spécifique au groupe
  TestConfig := FUpdateManager.AppConfig.ServerEndpoint +
                'ab_test/' + UserGroup + '.json';

  // Télécharger la configuration spécifique au groupe A/B
  TTask.Run(procedure
  var
    Client: THTTPClient;
    Response: IHTTPResponse;
  begin
    Client := THTTPClient.Create;
    try
      Response := Client.Get(TestConfig);

      if Response.StatusCode = 200 then
      begin
        // Appliquer la configuration du test A/B
        FABTestConfig.LoadFromJSON(Response.ContentAsString);

        // Mettre à jour l'interface selon la variante
        TThread.Synchronize(nil, procedure
        begin
          ApplyABTestConfiguration;
        end);

        // Enregistrer l'impression du test
        LogABTestImpression(UserGroup);
      end;
    finally
      Client.Free;
    end;
  end);
end;
```

### Pré-chargement des mises à jour en arrière-plan

Pour améliorer l'expérience utilisateur, préchargez les mises à jour :

```pascal
procedure TUpdateService.PreloadUpdates;
begin
  // Vérifier si le préchargement est autorisé
  if not IsWifiConnected and not AllowCellularDownloads then
    Exit;

  // Vérifier l'espace disponible
  if GetAvailableDiskSpace < MIN_REQUIRED_SPACE then
    Exit;

  // Télécharger les mises à jour en arrière-plan avec basse priorité
  TTask.Run(procedure
  begin
    // Obtenir la liste des ressources à précharger
    var ResourceList := GetResourcesManifest;

    // Télécharger chaque ressource
    for var Resource in ResourceList do
    begin
      // Vérifier si la ressource existe déjà
      if not IsResourceUpToDate(Resource) then
      begin
        // Télécharger avec basse priorité
        DownloadResourceWithLowPriority(Resource);
      end;
    end;
  end);
end;
```

## Limites et considérations

### Restrictions des plateformes

Il est important de connaître les limites imposées par chaque plateforme :

#### iOS

- **Taille maximale** : Les applications iOS ne peuvent pas télécharger plus de 60 Mo de contenu exécutable via OTA
- **Contenu interprété** : Apple est strict concernant le téléchargement de code interprété

#### Android

- **Permissions** : Vérifiez que vous avez les permissions nécessaires pour télécharger et stocker du contenu
- **Stockage** : Soyez conscient des limitations de stockage sur les appareils bas de gamme

### Conformité aux conditions des stores

Assurez-vous que vos mises à jour OTA respectent les conditions des stores :

- **App Store** : Ne téléchargez pas de code qui change le comportement fondamental de l'application
- **Play Store** : Respectez les règles concernant le contenu téléchargé dynamiquement

## Exemple complet : Application avec mises à jour de contenu

Voici un exemple plus complet qui combine plusieurs types de mises à jour OTA dans une application de type magazine/news :

```pascal
unit ContentUpdateManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Generics.Collections, System.Threading, System.IOUtils;

type
  TArticle = class
  private
    FID: string;
    FTitle: string;
    FContent: string;
    FImageURL: string;
    FPublishDate: TDateTime;
    FLocalImagePath: string;
  public
    constructor Create;

    procedure LoadFromJSON(const JSONObj: TJSONObject);

    property ID: string read FID;
    property Title: string read FTitle;
    property Content: string read FContent;
    property ImageURL: string read FImageURL;
    property PublishDate: TDateTime read FPublishDate;
    property LocalImagePath: string read FLocalImagePath write FLocalImagePath;
  end;

  TIssue = class
  private
    FID: string;
    FTitle: string;
    FVersion: Integer;
    FPublishDate: TDateTime;
    FArticles: TObjectList<TArticle>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJSON(const JSON: string);

    property ID: string read FID;
    property Title: string read FTitle;
    property Version: Integer read FVersion;
    property PublishDate: TDateTime read FPublishDate;
    property Articles: TObjectList<TArticle> read FArticles;
  end;

  TContentManager = class
  private
    FBaseURL: string;
    FCurrentIssue: TIssue;
    FContentDirectory: string;
    FOnIssueUpdated: TProc<TIssue>;
    FOnDownloadProgress: TProc<Integer>;

    function DownloadIssueManifest: Boolean;
    function DownloadArticleImages: Boolean;
    function GetArticleImagePath(const ArticleID: string): string;
  public
    constructor Create(const BaseURL: string);
    destructor Destroy; override;

    procedure CheckForUpdates;
    procedure ForceUpdate;

    property CurrentIssue: TIssue read FCurrentIssue;
    property OnIssueUpdated: TProc<TIssue> read FOnIssueUpdated write FOnIssueUpdated;
    property OnDownloadProgress: TProc<Integer> read FOnDownloadProgress
                               write FOnDownloadProgress;
  end;

implementation

{ TArticle }

constructor TArticle.Create;
begin
  inherited Create;
end;

procedure TArticle.LoadFromJSON(const JSONObj: TJSONObject);
begin
  if JSONObj = nil then
    Exit;

  FID := JSONObj.GetValue<string>('id');
  FTitle := JSONObj.GetValue<string>('title');
  FContent := JSONObj.GetValue<string>('content');
  FImageURL := JSONObj.GetValue<string>('imageUrl');

  var DateStr := JSONObj.GetValue<string>('publishDate');
  if DateStr <> '' then
    FPublishDate := ISO8601ToDate(DateStr)
  else
    FPublishDate := 0;
end;

{ TIssue }

constructor TIssue.Create;
begin
  inherited Create;
  FArticles := TObjectList<TArticle>.Create(True);
end;

destructor TIssue.Destroy;
begin
  FArticles.Free;
  inherited;
end;

procedure TIssue.LoadFromJSON(const JSON: string);
var
  JSONObj: TJSONObject;
  ArticlesArray: TJSONArray;
  I: Integer;
  ArticleObj: TJSONObject;
  Article: TArticle;
begin
  if JSON = '' then
    Exit;

  JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  if JSONObj = nil then
    Exit;

  try
    // Charger les métadonnées de l'édition
    FID := JSONObj.GetValue<string>('id');
    FTitle := JSONObj.GetValue<string>('title');
    FVersion := JSONObj.GetValue<Integer>('version');

    var DateStr := JSONObj.GetValue<string>('publishDate');
    if DateStr <> '' then
      FPublishDate := ISO8601ToDate(DateStr)
    else
      FPublishDate := 0;

    // Charger les articles
    if JSONObj.TryGetValue('articles', ArticlesArray) and (ArticlesArray <> nil) then
    begin
      FArticles.Clear;

      for I := 0 to ArticlesArray.Count - 1 do
      begin
        ArticleObj := ArticlesArray.Items[I] as TJSONObject;
        if ArticleObj <> nil then
        begin
          Article := TArticle.Create;
          Article.LoadFromJSON(ArticleObj);
          FArticles.Add(Article);
        end;
      end;
    end;
  finally
    JSONObj.Free;
  end;
end;

{ TContentManager }

constructor TContentManager.Create(const BaseURL: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FCurrentIssue := TIssue.Create;

  // Créer le répertoire de contenu
  FContentDirectory := TPath.Combine(TPath.GetDocumentsPath, 'content');
  if not TDirectory.Exists(FContentDirectory) then
    TDirectory.CreateDirectory(FContentDirectory);
end;

destructor TContentManager.Destroy;
begin
  FCurrentIssue.Free;
  inherited;
end;

procedure TContentManager.CheckForUpdates;
begin
  // Exécuter la vérification en arrière-plan
  TTask.Run(procedure
  begin
    // Télécharger le manifeste de l'édition
    if DownloadIssueManifest then
    begin
      // Télécharger les images des articles
      DownloadArticleImages;

      // Notifier de la mise à jour
      if Assigned(FOnIssueUpdated) then
        TThread.Synchronize(nil, procedure
        begin
          FOnIssueUpdated(FCurrentIssue);
        end);
    end;
  end);
end;

procedure TContentManager.ForceUpdate;
begin
  // Forcer une mise à jour complète
  CheckForUpdates;
end;

function TContentManager.DownloadIssueManifest: Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
  LocalPath: string;
begin
  Result := False;

  Client := THTTPClient.Create;
  try
    // Construire l'URL
    URL := FBaseURL + '/api/issues/latest';

    // Tenter de télécharger le manifeste
    try
      Response := Client.Get(URL);

      if Response.StatusCode = 200 then
      begin
        // Sauvegarder le manifeste localement
        LocalPath := TPath.Combine(FContentDirectory, 'latest_issue.json');
        TFile.WriteAllText(LocalPath, Response.ContentAsString);

        // Charger le manifeste dans l'objet d'édition
        FCurrentIssue.LoadFromJSON(Response.ContentAsString);

        Result := True;
      end;
    except
      // Gérer les erreurs de connexion
      Result := False;
    end;
  finally
    Client.Free;
  end;
end;

function TContentManager.GetArticleImagePath(const ArticleID: string): string;
begin
  Result := TPath.Combine(FContentDirectory, 'images');

  // Créer le répertoire d'images s'il n'existe pas
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);

  Result := TPath.Combine(Result, ArticleID + '.jpg');
end;

function TContentManager.DownloadArticleImages: Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  TotalImages, Downloaded: Integer;
  ProgressPercent: Integer;
begin
  Result := False;

  if FCurrentIssue.Articles.Count = 0 then
    Exit;

  Client := THTTPClient.Create;
  try
    TotalImages := FCurrentIssue.Articles.Count;
    Downloaded := 0;

    // Télécharger les images pour chaque article
    for var Article in FCurrentIssue.Articles do
    begin
      if Article.ImageURL <> '' then
      begin
        var LocalPath := GetArticleImagePath(Article.ID);

        // Vérifier si l'image existe déjà
        if not TFile.Exists(LocalPath) then
        begin
          try
            // Télécharger l'image
            Response := Client.Get(Article.ImageURL);

            if Response.StatusCode = 200 then
            begin
              // Enregistrer l'image
              TFile.WriteAllBytes(LocalPath, Response.ContentAsBytes);

              // Mettre à jour le chemin local
              Article.LocalImagePath := LocalPath;
            end;
          except
            // Ignorer les erreurs individuelles de téléchargement
          end;
        end
        else
        begin
          // Utiliser l'image existante
          Article.LocalImagePath := LocalPath;
        end;

        // Mettre à jour la progression
        Inc(Downloaded);
        ProgressPercent := Round((Downloaded / TotalImages) * 100);

        // Notifier de la progression
        if Assigned(FOnDownloadProgress) then
          TThread.Synchronize(nil, procedure
          begin
            FOnDownloadProgress(ProgressPercent);
          end);
      end;
    end;

    Result := True;
  finally
    Client.Free;
  end;
end;
```

Pour utiliser ce gestionnaire de contenu dans votre application :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Créer le gestionnaire de contenu
  FContentManager := TContentManager.Create('https://api.example.com');

  // Configurer les gestionnaires d'événements
  FContentManager.OnIssueUpdated := HandleIssueUpdated;
  FContentManager.OnDownloadProgress := HandleDownloadProgress;

  // Vérifier les mises à jour au démarrage
  FContentManager.CheckForUpdates;
end;

procedure TMainForm.HandleIssueUpdated(Issue: TIssue);
begin
  // Mettre à jour le titre
  lblIssueTitle.Text := Issue.Title;

  // Mettre à jour la liste des articles
  lstArticles.Items.Clear;

  for var Article in Issue.Articles do
  begin
    var Item := lstArticles.Items.Add;
    Item.Text := Article.Title;
    Item.Detail := FormatDateTime('dd/mm/yyyy', Article.PublishDate);
    Item.TagObject := Article;

    // Charger l'image si disponible
    if (Article.LocalImagePath <> '') and TFile.Exists(Article.LocalImagePath) then
    begin
      var Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromFile(Article.LocalImagePath);
        Item.Bitmap := Bitmap;
      finally
        Bitmap.Free;
      end;
    end;
  end;

  // Afficher la date de publication
  lblPublishDate.Text := 'Publié le ' +
                         FormatDateTime('dd/mm/yyyy', Issue.PublishDate);
end;

procedure TMainForm.HandleDownloadProgress(Progress: Integer);
begin
  // Mettre à jour la barre de progression
  ProgressBar1.Value := Progress;

  // Afficher/masquer selon le statut
  ProgressBar1.Visible := (Progress < 100);
end;

procedure TMainForm.lstArticlesItemClick(const Sender: TObject;
                                       const Item: TListViewItem);
var
  Article: TArticle;
begin
  // Récupérer l'article sélectionné
  Article := TArticle(Item.TagObject);
  if Article <> nil then
  begin
    // Afficher le contenu de l'article
    txtContent.Text := Article.Content;

    // Charger l'image
    if (Article.LocalImagePath <> '') and TFile.Exists(Article.LocalImagePath) then
      imgArticle.Bitmap.LoadFromFile(Article.LocalImagePath)
    else
      imgArticle.Bitmap.Clear(0);
  end;
end;
```

## Conclusion

Les mises à jour OTA sont un outil puissant qui permet de maintenir votre application à jour et d'introduire de nouvelles fonctionnalités sans passer par le processus de validation des stores. Avec Delphi, vous pouvez implémenter des mécanismes de mise à jour robustes qui améliorent l'expérience utilisateur et vous donnent plus de flexibilité.

En suivant les bonnes pratiques et en implémentant des mécanismes de sécurité adéquats, vous pouvez tirer pleinement parti des mises à jour OTA tout en garantissant la stabilité et la sécurité de votre application.

N'oubliez pas que les mises à jour OTA ne remplacent pas complètement les mises à jour traditionnelles via les stores. Les changements majeurs dans le code natif nécessiteront toujours une nouvelle soumission. Les mises à jour OTA constituent plutôt un complément qui vous permet d'itérer plus rapidement sur certains aspects de votre application.

Dans la prochaine section, nous verrons comment partager du code entre vos applications mobiles et desktop, une approche qui peut considérablement réduire le temps de développement et de maintenance de vos projets multi-plateformes.

⏭️ [Partage de code entre applications mobile et desktop](/15-applications-mobiles-avec-delphi/09-partage-de-code-entre-applications.md)
