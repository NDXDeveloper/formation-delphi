# 15.5 Notifications

Les notifications sont un moyen essentiel de communiquer avec vos utilisateurs, même lorsqu'ils n'utilisent pas activement votre application. Elles permettent d'informer l'utilisateur d'événements importants, de lui rappeler des tâches ou de l'engager à revenir dans l'application. Dans cette section, nous explorerons comment implémenter différents types de notifications dans vos applications mobiles Delphi.

## Types de notifications

Il existe principalement deux types de notifications que vous pouvez implémenter dans vos applications :

1. **Notifications locales** : Générées et programmées directement par votre application sur l'appareil, sans nécessiter de connexion internet.

2. **Notifications push (distantes)** : Envoyées depuis un serveur distant via des services comme Firebase Cloud Messaging (FCM) pour Android ou Apple Push Notification Service (APNS) pour iOS.

## Notifications locales

Les notifications locales sont plus simples à mettre en œuvre car elles ne nécessitent pas d'infrastructure serveur. Elles sont idéales pour les rappels, les alertes basées sur l'heure ou les événements locaux à l'appareil.

### Configuration préalable

Pour Android, vous devez ajouter certaines permissions dans les options du projet :

1. Ouvrez **Project > Options > Uses Permissions**
2. Cochez `RECEIVE_BOOT_COMPLETED` si vous souhaitez que vos notifications persistent après un redémarrage de l'appareil

Pour iOS, aucune configuration spéciale n'est nécessaire dans le projet, mais vous devrez demander l'autorisation à l'utilisateur dans le code.

### Création de notifications locales basiques

Voici comment créer et afficher une notification locale simple :

```pascal
uses
  System.Notification, System.PushNotification;

procedure TNotificationsForm.ShowSimpleNotification;
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
begin
  // Obtenir le centre de notification
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    // Créer une notification
    Notification := NotificationCenter.CreateNotification;
    try
      // Configurer la notification
      Notification.Title := 'Titre de la notification';
      Notification.AlertBody := 'Ceci est le corps de la notification qui contient plus de détails.';

      // Définir une icône (facultatif)
      Notification.EnableSound := True;

      {$IFDEF ANDROID}
      // Options spécifiques à Android
      Notification.Number := 1; // Badge numérique
      {$ENDIF}

      // Présenter immédiatement la notification
      NotificationCenter.PresentNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;

  ShowMessage('Notification envoyée');
end;
```

Pour ajouter ce code à votre application :

1. Créez un nouveau formulaire ou utilisez un formulaire existant
2. Ajoutez un bouton pour déclencher la notification
3. Implémentez la méthode ci-dessus et associez-la au clic du bouton

### Notifications programmées

Vous pouvez également programmer des notifications pour qu'elles apparaissent à un moment précis dans le futur :

```pascal
procedure TNotificationsForm.ScheduleNotification;
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
  TriggerTime: TDateTime;
begin
  // Obtenir le centre de notification
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    // Créer une notification
    Notification := NotificationCenter.CreateNotification;
    try
      // Configurer la notification
      Notification.Title := 'Rappel programmé';
      Notification.AlertBody := 'Cette notification était programmée pour apparaître à cette heure.';
      Notification.EnableSound := True;

      // Générer un identifiant unique pour cette notification
      Notification.Name := 'Rappel_' + FormatDateTime('yyyymmddhhnnss', Now);

      // Définir l'heure de déclenchement (5 minutes dans le futur)
      TriggerTime := Now + (5 / (24 * 60)); // 5 minutes en fraction de jour
      Notification.FireDate := TriggerTime;

      // Programmer la notification
      NotificationCenter.ScheduleNotification(Notification);

      // Informer l'utilisateur
      ShowMessage('Notification programmée pour ' +
                   FormatDateTime('hh:nn:ss', TriggerTime));
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;
end;
```

### Gestion des notifications programmées

Il est important de pouvoir gérer les notifications que vous avez programmées, par exemple pour les annuler :

```pascal
// Annuler une notification spécifique par son nom
procedure TNotificationsForm.CancelSpecificNotification(const NotificationName: string);
var
  NotificationCenter: TNotificationCenter;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    NotificationCenter.CancelNotification(NotificationName);
    ShowMessage('Notification "' + NotificationName + '" annulée');
  finally
    NotificationCenter.Free;
  end;
end;

// Annuler toutes les notifications programmées
procedure TNotificationsForm.CancelAllNotifications;
var
  NotificationCenter: TNotificationCenter;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    NotificationCenter.CancelAll;
    ShowMessage('Toutes les notifications ont été annulées');
  finally
    NotificationCenter.Free;
  end;
end;
```

### Notifications répétitives

Vous pouvez également configurer des notifications qui se répètent régulièrement :

```pascal
procedure TNotificationsForm.ScheduleRepeatingNotification;
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
  TriggerTime: TDateTime;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    Notification := NotificationCenter.CreateNotification;
    try
      // Configuration de base
      Notification.Title := 'Rappel quotidien';
      Notification.AlertBody := 'N''oubliez pas de vérifier vos tâches !';
      Notification.EnableSound := True;
      Notification.Name := 'RappelQuotidien';

      // Configurer l'heure de déclenchement initial (demain à 9h00)
      TriggerTime := Trunc(Now + 1) + EncodeTime(9, 0, 0, 0);
      Notification.FireDate := TriggerTime;

      // Configurer la répétition
      {$IFDEF ANDROID}
      // Sur Android, on utilise RepeatInterval
      Notification.RepeatInterval := TRepeatInterval.Day;
      {$ENDIF}

      {$IFDEF IOS}
      // Sur iOS, CalendarInterval est préféré pour les répétitions précises
      Notification.RepeatInterval := TRepeatInterval.Day;
      {$ENDIF}

      // Programmer la notification
      NotificationCenter.ScheduleNotification(Notification);

      ShowMessage('Notification quotidienne programmée à partir de ' +
                   FormatDateTime('dd/mm/yyyy hh:nn', TriggerTime));
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;
end;
```

### Réagir à l'activation des notifications

Lorsque l'utilisateur appuie sur une notification, vous voudrez probablement exécuter une action spécifique. Voici comment configurer un gestionnaire pour traiter ces événements :

```pascal
type
  TNotificationsForm = class(TForm)
    // ... autres composants et méthodes
  private
    FNotificationCenter: TNotificationCenter;
    procedure NotificationReceived(Sender: TObject; const Notification: TNotification);
  public
    // ... autres méthodes
  end;

implementation

procedure TNotificationsForm.FormCreate(Sender: TObject);
begin
  // Créer et configurer le centre de notification
  FNotificationCenter := TNotificationCenter.Create(Self);
  FNotificationCenter.OnNotificationReceived := NotificationReceived;

  // Autres initialisations...
end;

procedure TNotificationsForm.NotificationReceived(Sender: TObject;
  const Notification: TNotification);
begin
  // Vérifier d'où vient la notification par son nom
  if Notification.Name.StartsWith('Rappel_') then
  begin
    ShowMessage('Vous avez cliqué sur un rappel!');
    // Naviguez vers l'écran approprié ou effectuez l'action nécessaire
  end
  else if Notification.Name = 'RappelQuotidien' then
  begin
    ShowMessage('Voici vos tâches du jour');
    // Afficher les tâches du jour
  end;

  // Pour les cas génériques
  Memo1.Lines.Add('Notification reçue: ' + Notification.Name +
                   ' à ' + FormatDateTime('hh:nn:ss', Now));
end;

procedure TNotificationsForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FNotificationCenter.Free;
end;
```

### Meilleures pratiques pour les notifications locales

1. **Ne pas abuser** : Limitez le nombre de notifications pour éviter de frustrer l'utilisateur
2. **Pertinence** : Assurez-vous que chaque notification apporte une réelle valeur à l'utilisateur
3. **Clarté** : Utilisez des titres explicites et des messages concis
4. **Persistance** : Stockez les informations sur les notifications programmées pour pouvoir les gérer
5. **Respect de la vie privée** : N'incluez pas d'informations sensibles dans les notifications

## Notifications push (distantes)

Les notifications push permettent d'envoyer des messages à vos utilisateurs depuis un serveur distant, même lorsqu'ils n'utilisent pas activement votre application. Elles sont essentielles pour les applications nécessitant des mises à jour en temps réel.

### Configuration des services de notification push

Pour utiliser les notifications push, vous aurez besoin de configurer votre application pour utiliser FCM (Firebase Cloud Messaging) pour Android et APNS (Apple Push Notification Service) pour iOS. Voici les étapes générales :

#### Configuration pour Android (FCM)

1. Créez un projet dans la [Console Firebase](https://console.firebase.google.com/)
2. Ajoutez votre application Android au projet
3. Téléchargez le fichier `google-services.json`
4. Placez ce fichier dans le dossier `<Projet>\Android\Debug` et `<Projet>\Android\Release`

#### Configuration pour iOS (APNS)

1. Créez un certificat de notification push dans votre compte développeur Apple
2. Téléchargez et installez le certificat dans votre keychain
3. Configurez votre application dans le portail développeur Apple pour utiliser les notifications push

### Implémentation des notifications push dans l'application

Une fois les services configurés, vous pouvez implémenter la réception des notifications push dans votre application Delphi :

```pascal
uses
  System.PushNotification;

type
  TPushNotificationsForm = class(TForm)
    // ... autres composants
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPushService: TPushService;
    FServiceConnection: TPushServiceConnection;
    procedure HandlePushNotification(Sender: TObject; const AData: TPushServiceData);
    procedure DeviceTokenReceived(Sender: TObject; const AData: TPushServiceData);
  public
    // ... autres méthodes
  end;

implementation

procedure TPushNotificationsForm.FormCreate(Sender: TObject);
begin
  // Créer et configurer le service de notification push
  FPushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);

  if FPushService <> nil then
  begin
    FServiceConnection := TPushServiceConnection.Create(FPushService);
    FServiceConnection.Active := True;

    // Configurer les gestionnaires d'événements
    FServiceConnection.OnPushNotification := HandlePushNotification;
    FServiceConnection.OnDeviceTokenReceived := DeviceTokenReceived;

    // Démarrer le service
    FServiceConnection.Active := True;
  end
  else
    ShowMessage('Service de notification push non disponible');
end;

procedure TPushNotificationsForm.DeviceTokenReceived(Sender: TObject;
  const AData: TPushServiceData);
var
  DeviceToken: string;
begin
  // Récupérer le token d'appareil
  DeviceToken := AData.Token;

  // Afficher le token (pour le développement)
  Memo1.Lines.Add('Token reçu: ' + DeviceToken);

  // Dans une application réelle, vous enverriez ce token à votre serveur
  // pour permettre l'envoi de notifications push à cet appareil
end;

procedure TPushNotificationsForm.HandlePushNotification(Sender: TObject;
  const AData: TPushServiceData);
var
  MessageText: string;
  NotificationData: TPushServiceNotificationData;
begin
  // Vérifier si c'est une notification
  if AData is TPushServiceNotificationData then
  begin
    NotificationData := TPushServiceNotificationData(AData);

    // Récupérer le message
    if NotificationData.Message <> nil then
      MessageText := NotificationData.Message.Text
    else
      MessageText := 'Notification sans texte';

    // Traiter la notification
    Memo1.Lines.Add('Notification push reçue: ' + MessageText);

    // Vous pouvez extraire d'autres données personnalisées
    if NotificationData.DataObject.Count > 0 then
    begin
      // Exemple: récupérer une valeur spécifique
      if NotificationData.DataObject.Contains('action') then
      begin
        var Action := NotificationData.DataObject.GetValue('action').Value;
        // Traiter l'action selon sa valeur
        if Action = 'open_profile' then
          ShowMessage('Ouverture du profil demandée');
      end;
    end;
  end;
end;

procedure TPushNotificationsForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  if FServiceConnection <> nil then
  begin
    FServiceConnection.Active := False;
    FServiceConnection.Free;
  end;
end;
```

### Envoi de notifications push

Pour envoyer des notifications push à vos utilisateurs, vous devrez implémenter un serveur ou utiliser un service tiers. Voici un exemple de code côté serveur (en PHP) pour envoyer une notification via FCM (Firebase Cloud Messaging) :

```php
<?php
// Exemple de code PHP pour envoyer une notification FCM

$serverKey = 'VOTRE_CLÉ_SERVEUR_FCM'; // Obtenue depuis la console Firebase
$deviceToken = 'TOKEN_DE_L_APPAREIL_CIBLE'; // Token reçu de l'application

$data = [
    'notification' => [
        'title' => 'Nouvelle mise à jour',
        'body' => 'Une nouvelle version de l\'application est disponible!',
        'sound' => 'default'
    ],
    'data' => [
        'action' => 'update_app',
        'custom_data' => 'valeur_personnalisée'
    ],
    'to' => $deviceToken
];

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'https://fcm.googleapis.com/fcm/send');
curl_setopt($ch, CURLOPT_POST, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, [
    'Authorization: key=' . $serverKey,
    'Content-Type: application/json'
]);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));

$result = curl_exec($ch);
if ($result === FALSE) {
    die('Erreur curl: ' . curl_error($ch));
}

curl_close($ch);
echo $result;
?>
```

> **Note** : Dans un environnement de production, vous utiliserez probablement une solution plus robuste, comme un service backend dédié ou une plateforme de notification push tierce (OneSignal, Firebase, etc.).

## Mettre en œuvre des notifications avancées

### Notifications avec actions

Sur les plateformes modernes, vous pouvez ajouter des boutons d'action à vos notifications :

```pascal
procedure TAdvNotificationsForm.ShowNotificationWithActions;
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    Notification := NotificationCenter.CreateNotification;
    try
      // Configuration de base
      Notification.Title := 'Nouveau message';
      Notification.AlertBody := 'Vous avez reçu un nouveau message de Jean';
      Notification.EnableSound := True;
      Notification.Name := 'Message_' + FormatDateTime('yyyymmddhhnnss', Now);

      {$IFDEF ANDROID}
      // Actions pour Android
      Notification.Category := 'message';
      var Actions := TList<TNotificationAction>.Create;
      try
        // Action "Répondre"
        var ActionReply := TNotificationAction.Create;
        ActionReply.Name := 'reply';
        ActionReply.Title := 'Répondre';

        // Action "Marquer comme lu"
        var ActionRead := TNotificationAction.Create;
        ActionRead.Name := 'mark_read';
        ActionRead.Title := 'Marquer comme lu';

        // Ajouter les actions
        Actions.Add(ActionReply);
        Actions.Add(ActionRead);

        // Associer les actions à la notification
        Notification.Actions := Actions;
      finally
        Actions.Free;
      end;
      {$ENDIF}

      {$IFDEF IOS}
      // Actions pour iOS - configuration similaire mais syntaxe légèrement différente
      // selon la version de Delphi
      {$ENDIF}

      // Présenter la notification
      NotificationCenter.PresentNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;
end;
```

> **Note** : L'implémentation des actions de notification peut varier selon la version de Delphi et la plateforme cible. Consultez la documentation la plus récente pour votre version spécifique.

### Notifications avec contenu enrichi

Les notifications modernes peuvent également inclure des images ou d'autres contenus enrichis :

```pascal
procedure TRichNotificationsForm.ShowRichNotification;
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    Notification := NotificationCenter.CreateNotification;
    try
      // Configuration de base
      Notification.Title := 'Nouvelle photo';
      Notification.AlertBody := 'Un ami a partagé une photo avec vous';
      Notification.EnableSound := True;
      Notification.Name := 'Photo_' + FormatDateTime('yyyymmddhhnnss', Now);

      {$IFDEF ANDROID}
      // Sur Android, vous pouvez spécifier une icône de grande taille
      Notification.LargeIconURI := TPath.Combine(
        TPath.GetDocumentsPath, 'notification_image.png');
      {$ENDIF}

      // Présenter la notification
      NotificationCenter.PresentNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;
end;
```

## Exemple complet : Gestionnaire de rappels avec notifications

Voici un exemple plus complet d'une application de rappels qui utilise les notifications locales :

```pascal
unit ReminderManager;

interface

uses
  System.SysUtils, System.Classes, System.Notification, System.DateUtils,
  System.Generics.Collections, System.JSON;

type
  TReminderPriority = (Low, Medium, High);

  TReminder = class
  private
    FId: string;
    FTitle: string;
    FMessage: string;
    FDueDate: TDateTime;
    FPriority: TReminderPriority;
    FIsCompleted: Boolean;
  public
    constructor Create(const ATitle, AMessage: string; ADueDate: TDateTime;
      APriority: TReminderPriority);
    property Id: string read FId;
    property Title: string read FTitle write FTitle;
    property Message: string read FMessage write FMessage;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property Priority: TReminderPriority read FPriority write FPriority;
    property IsCompleted: Boolean read FIsCompleted write FIsCompleted;

    function ToJSON: TJSONObject;
    procedure FromJSON(AJson: TJSONObject);
  end;

  TReminderManager = class
  private
    FReminders: TObjectList<TReminder>;
    FNotificationCenter: TNotificationCenter;
    FStorageFileName: string;

    procedure ScheduleNotification(const Reminder: TReminder);
    procedure CancelNotification(const ReminderId: string);
  public
    constructor Create;
    destructor Destroy; override;

    function AddReminder(const Title, Message: string; DueDate: TDateTime;
      Priority: TReminderPriority): TReminder;
    procedure UpdateReminder(const Reminder: TReminder);
    procedure DeleteReminder(const ReminderId: string);
    procedure MarkAsCompleted(const ReminderId: string);
    function GetReminder(const ReminderId: string): TReminder;
    function GetAllReminders: TArray<TReminder>;

    procedure SaveReminders;
    procedure LoadReminders;
  end;

implementation

{ TReminder }

constructor TReminder.Create(const ATitle, AMessage: string; ADueDate: TDateTime;
  APriority: TReminderPriority);
begin
  inherited Create;
  FId := TGUID.NewGuid.ToString;
  FTitle := ATitle;
  FMessage := AMessage;
  FDueDate := ADueDate;
  FPriority := APriority;
  FIsCompleted := False;
end;

function TReminder.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', FId);
  Result.AddPair('title', FTitle);
  Result.AddPair('message', FMessage);
  Result.AddPair('dueDate', DateToISO8601(FDueDate));
  Result.AddPair('priority', TJSONNumber.Create(Ord(FPriority)));
  Result.AddPair('isCompleted', TJSONBool.Create(FIsCompleted));
end;

procedure TReminder.FromJSON(AJson: TJSONObject);
begin
  FId := AJson.GetValue<string>('id');
  FTitle := AJson.GetValue<string>('title');
  FMessage := AJson.GetValue<string>('message');
  FDueDate := ISO8601ToDate(AJson.GetValue<string>('dueDate'));
  FPriority := TReminderPriority(AJson.GetValue<Integer>('priority'));
  FIsCompleted := AJson.GetValue<Boolean>('isCompleted');
end;

{ TReminderManager }

constructor TReminderManager.Create;
begin
  inherited Create;
  FReminders := TObjectList<TReminder>.Create(True);
  FNotificationCenter := TNotificationCenter.Create(nil);

  // Définir le nom du fichier de stockage
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  FStorageFileName := TPath.Combine(TPath.GetDocumentsPath, 'reminders.json');
  {$ELSE}
  FStorageFileName := TPath.Combine(TPath.GetHomePath, 'reminders.json');
  {$ENDIF}

  // Charger les rappels existants
  LoadReminders;
end;

destructor TReminderManager.Destroy;
begin
  SaveReminders;
  FReminders.Free;
  FNotificationCenter.Free;
  inherited;
end;

function TReminderManager.AddReminder(const Title, Message: string;
  DueDate: TDateTime; Priority: TReminderPriority): TReminder;
begin
  Result := TReminder.Create(Title, Message, DueDate, Priority);
  FReminders.Add(Result);

  // Programmer la notification si la date d'échéance est dans le futur
  if not Result.IsCompleted and (Result.DueDate > Now) then
    ScheduleNotification(Result);

  // Sauvegarder les modifications
  SaveReminders;
end;

procedure TReminderManager.UpdateReminder(const Reminder: TReminder);
var
  ExistingReminder: TReminder;
begin
  ExistingReminder := GetReminder(Reminder.Id);
  if ExistingReminder <> nil then
  begin
    // Mettre à jour les propriétés
    ExistingReminder.Title := Reminder.Title;
    ExistingReminder.Message := Reminder.Message;
    ExistingReminder.DueDate := Reminder.DueDate;
    ExistingReminder.Priority := Reminder.Priority;
    ExistingReminder.IsCompleted := Reminder.IsCompleted;

    // Annuler la notification existante
    CancelNotification(Reminder.Id);

    // Reprogrammer si nécessaire
    if not ExistingReminder.IsCompleted and (ExistingReminder.DueDate > Now) then
      ScheduleNotification(ExistingReminder);

    // Sauvegarder les modifications
    SaveReminders;
  end;
end;

procedure TReminderManager.DeleteReminder(const ReminderId: string);
var
  I: Integer;
begin
  for I := 0 to FReminders.Count - 1 do
  begin
    if FReminders[I].Id = ReminderId then
    begin
      // Annuler la notification
      CancelNotification(ReminderId);

      // Supprimer le rappel
      FReminders.Delete(I);

      // Sauvegarder les modifications
      SaveReminders;
      Break;
    end;
  end;
end;

procedure TReminderManager.MarkAsCompleted(const ReminderId: string);
var
  Reminder: TReminder;
begin
  Reminder := GetReminder(ReminderId);
  if Reminder <> nil then
  begin
    Reminder.IsCompleted := True;

    // Annuler la notification
    CancelNotification(ReminderId);

    // Sauvegarder les modifications
    SaveReminders;
  end;
end;

function TReminderManager.GetReminder(const ReminderId: string): TReminder;
var
  R: TReminder;
begin
  Result := nil;
  for R in FReminders do
  begin
    if R.Id = ReminderId then
    begin
      Result := R;
      Break;
    end;
  end;
end;

function TReminderManager.GetAllReminders: TArray<TReminder>;
var
  I: Integer;
begin
  SetLength(Result, FReminders.Count);
  for I := 0 to FReminders.Count - 1 do
    Result[I] := FReminders[I];
end;

procedure TReminderManager.ScheduleNotification(const Reminder: TReminder);
var
  Notification: TNotification;
begin
  Notification := FNotificationCenter.CreateNotification;
  try
    // Configurer la notification
    Notification.Name := 'Reminder_' + Reminder.Id;
    Notification.Title := Reminder.Title;
    Notification.AlertBody := Reminder.Message;
    Notification.FireDate := Reminder.DueDate;
    Notification.EnableSound := True;

    // Ajouter des propriétés selon la priorité
    case Reminder.Priority of
      TReminderPriority.High:
      begin
        Notification.Title := '🔴 ' + Notification.Title;
        {$IFDEF ANDROID}
        Notification.Importance := TImportance.High;
        {$ENDIF}
      end;
      TReminderPriority.Medium:
      begin
        Notification.Title := '🟠 ' + Notification.Title;
        {$IFDEF ANDROID}
        Notification.Importance := TImportance.Default;
        {$ENDIF}
      end;
      TReminderPriority.Low:
      begin
        Notification.Title := '🟢 ' + Notification.Title;
        {$IFDEF ANDROID}
        Notification.Importance := TImportance.Low;
        {$ENDIF}
      end;
    end;

    // Programmer la notification
    FNotificationCenter.ScheduleNotification(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TReminderManager.CancelNotification(const ReminderId: string);
begin
  FNotificationCenter.CancelNotification('Reminder_' + ReminderId);
end;

procedure TReminderManager.SaveReminders;
var
  JsonArray: TJSONArray;
  R: TReminder;
  JsonStr: string;
begin
  JsonArray := TJSONArray.Create;
  try
    // Convertir chaque rappel en JSON
    for R in FReminders do
      JsonArray.AddElement(R.ToJSON);

    // Convertir en chaîne JSON
    JsonStr := JsonArray.ToString;

    // Enregistrer dans un fichier
    TFile.WriteAllText(FStorageFileName, JsonStr);
  finally
    JsonArray.Free;
  end;
end;

procedure TReminderManager.LoadReminders;
var
  JsonStr: string;
  JsonArray: TJSONArray;
  JsonObj: TJSONObject;
  Reminder: TReminder;
  I: Integer;
begin
  // Vider la liste actuelle
  FReminders.Clear;

  // Vérifier si le fichier existe
  if not TFile.Exists(FStorageFileName) then
    Exit;

  try
    // Lire le contenu du fichier
    JsonStr := TFile.ReadAllText(FStorageFileName);

    // Analyser le JSON
    JsonArray := TJSONObject.ParseJSONValue(JsonStr) as TJSONArray;
    if JsonArray <> nil then
    try
      // Parcourir tous les éléments
      for I := 0 to JsonArray.Count - 1 do
      begin
        JsonObj := JsonArray.Items[I] as TJSONObject;

        // Créer un nouveau rappel
        Reminder := TReminder.Create('', '', Now, TReminderPriority.Medium);
        Reminder.FromJSON(JsonObj);

        // Ajouter à la liste
        FReminders.Add(Reminder);

        // Programmer la notification si nécessaire
        if not Reminder.IsCompleted and (Reminder.DueDate > Now) then
          ScheduleNotification(Reminder);
      end;
    finally
      JsonArray.Free;
    end;
  except
    // Gérer les erreurs silencieusement - fichier corrompu, etc.
    // Dans une application réelle, vous voudriez journaliser cette erreur
  end;
end;
```

## Travailler avec le composant ReminderManager

L'exemple ci-dessus fournit une classe complète pour gérer les rappels et les notifications associées. Voici comment l'utiliser dans votre application :

```pascal
unit MainFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.DateTimeCtrls, FMX.ListBox,
  FMX.Edit, FMX.Layouts, ReminderManager;

type
  TMainForm = class(TForm)
    ListView1: TListView;
    ToolBar1: TToolBar;
    btnAdd: TButton;
    Layout1: TLayout;
    edtTitle: TEdit;
    edtMessage: TEdit;
    dtpDueDate: TDateEdit;
    timeDueTime: TTimeEdit;
    cmbPriority: TComboBox;
    btnSave: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListView1DeleteItem(Sender: TObject; AIndex: Integer);
  private
    FReminderManager: TReminderManager;
    FIsEditing: Boolean;
    FCurrentReminderId: string;

    procedure RefreshReminderList;
    procedure ShowAddEditPanel(const Show: Boolean);
    procedure PrepareForEdit(const ReminderId: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Créer le gestionnaire de rappels
  FReminderManager := TReminderManager.Create;

  // Initialiser l'interface
  ShowAddEditPanel(False);

  // Configurer la liste déroulante des priorités
  cmbPriority.Items.Clear;
  cmbPriority.Items.Add('Basse');
  cmbPriority.Items.Add('Moyenne');
  cmbPriority.Items.Add('Haute');
  cmbPriority.ItemIndex := 1; // Par défaut: Moyenne

  // Définir l'heure par défaut à 1 heure dans le futur
  dtpDueDate.Date := Date;
  timeDueTime.Time := IncHour(Time, 1);

  // Remplir la liste des rappels
  RefreshReminderList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FReminderManager.Free;
end;

procedure TMainForm.RefreshReminderList;
var
  Reminders: TArray<TReminder>;
  R: TReminder;
  Item: TListViewItem;
begin
  // Vider la liste
  ListView1.Items.Clear;

  // Obtenir tous les rappels
  Reminders := FReminderManager.GetAllReminders;

  // Ajouter chaque rappel à la liste
  for R in Reminders do
  begin
    Item := ListView1.Items.Add;
    Item.Tag := Integer(R); // Pour un accès rapide au rappel (pas idéal en production)
    Item.Text := R.Title;
    Item.Detail := R.Message;

    // Formater la date et l'heure
    Item.Data['Date'] := FormatDateTime('dd/mm/yyyy HH:mm', R.DueDate);

    // Ajouter un indicateur de priorité
    case R.Priority of
      TReminderPriority.Low:  Item.Data['Priority'] := '🟢';
      TReminderPriority.Medium: Item.Data['Priority'] := '🟠';
      TReminderPriority.High: Item.Data['Priority'] := '🔴';
    end;

    // Marquer les rappels terminés
    if R.IsCompleted then
      Item.Data['Status'] := '✓'
    else
      Item.Data['Status'] := '';
  end;
end;

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  // Préparer pour l'ajout d'un nouveau rappel
  FIsEditing := False;
  FCurrentReminderId := '';

  // Réinitialiser les champs
  edtTitle.Text := '';
  edtMessage.Text := '';
  dtpDueDate.Date := Date;
  timeDueTime.Time := IncHour(Time, 1);
  cmbPriority.ItemIndex := 1; // Moyenne

  // Afficher le panneau d'édition
  ShowAddEditPanel(True);
end;

procedure TMainForm.PrepareForEdit(const ReminderId: string);
var
  Reminder: TReminder;
begin
  Reminder := FReminderManager.GetReminder(ReminderId);
  if Reminder <> nil then
  begin
    // Mode édition
    FIsEditing := True;
    FCurrentReminderId := ReminderId;

    // Remplir les champs avec les données existantes
    edtTitle.Text := Reminder.Title;
    edtMessage.Text := Reminder.Message;
    dtpDueDate.Date := DateOf(Reminder.DueDate);
    timeDueTime.Time := TimeOf(Reminder.DueDate);
    cmbPriority.ItemIndex := Ord(Reminder.Priority);

    // Afficher le panneau d'édition
    ShowAddEditPanel(True);
  end;
end;

procedure TMainForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  Reminder: TReminder;
begin
  // Récupérer le rappel associé
  Reminder := TReminder(AItem.Tag);
  if Reminder <> nil then
    PrepareForEdit(Reminder.Id);
end;

procedure TMainForm.ListView1DeleteItem(Sender: TObject; AIndex: Integer);
var
  Item: TListViewItem;
  Reminder: TReminder;
begin
  // Récupérer l'item et le rappel associé
  Item := ListView1.Items[AIndex];
  Reminder := TReminder(Item.Tag);

  // Supprimer le rappel
  if Reminder <> nil then
    FReminderManager.DeleteReminder(Reminder.Id);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  Title, Message: string;
  DueDate: TDateTime;
  Priority: TReminderPriority;
  Reminder: TReminder;
begin
  // Récupérer les valeurs des champs
  Title := Trim(edtTitle.Text);
  Message := Trim(edtMessage.Text);
  DueDate := dtpDueDate.Date + timeDueTime.Time;
  Priority := TReminderPriority(cmbPriority.ItemIndex);

  // Valider les entrées
  if Title = '' then
  begin
    ShowMessage('Veuillez saisir un titre');
    Exit;
  end;

  // Ajouter ou mettre à jour le rappel
  if FIsEditing then
  begin
    // Mettre à jour un rappel existant
    Reminder := FReminderManager.GetReminder(FCurrentReminderId);
    if Reminder <> nil then
    begin
      Reminder.Title := Title;
      Reminder.Message := Message;
      Reminder.DueDate := DueDate;
      Reminder.Priority := Priority;

      FReminderManager.UpdateReminder(Reminder);
    end;
  end
  else
  begin
    // Créer un nouveau rappel
    FReminderManager.AddReminder(Title, Message, DueDate, Priority);
  end;

  // Masquer le panneau d'édition
  ShowAddEditPanel(False);

  // Rafraîchir la liste
  RefreshReminderList;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  // Annuler l'édition/ajout
  ShowAddEditPanel(False);
end;

procedure TMainForm.ShowAddEditPanel(const Show: Boolean);
begin
  Layout1.Visible := Show;
  ListView1.Visible := not Show;
  ToolBar1.Visible := not Show;
end;
```

Ce code suppose que vous avez créé une interface utilisateur avec les composants suivants :
- Une `TListView` pour afficher les rappels
- Un panneau `TLayout` contenant les contrôles d'édition
- Des contrôles `TEdit` pour le titre et le message
- Des contrôles `TDateEdit` et `TTimeEdit` pour la date d'échéance
- Un contrôle `TComboBox` pour la priorité
- Des boutons pour l'ajout, la sauvegarde et l'annulation

## Réagir aux notifications en arrière-plan

Pour qu'une application puisse réagir aux notifications lorsqu'elle est en arrière-plan ou fermée, vous devez configurer un gestionnaire spécial :

### Pour Android

Sur Android, vous pouvez créer un service qui s'exécute en arrière-plan pour gérer les notifications :

```pascal
// Créer dans un fichier séparé, par exemple NotificationService.pas
unit NotificationService;

interface

{$IFDEF ANDROID}
uses
  System.SysUtils, System.Classes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.JavaTypes;

type
  JMyNotificationServiceClass = interface(JBroadcastReceiverClass)
    ['{A1234567-1234-1234-1234-1234567890AB}']
  end;

  [JavaSignature('com/yourcompany/yourapp/MyNotificationService')]
  JMyNotificationService = interface(JBroadcastReceiver)
    ['{B1234567-1234-1234-1234-1234567890AB}']
  end;

  TJMyNotificationService = class(TJavaGenericImport<JMyNotificationServiceClass, JMyNotificationService>)
  end;
{$ENDIF}

implementation

{$IFDEF ANDROID}
// Implémentation du service
procedure RegisterServiceReceiver;
begin
  // Enregistrer votre service ici
end;

initialization
  RegisterServiceReceiver;
{$ENDIF}

end.
```

Vous devrez également ajouter une entrée dans le fichier AndroidManifest.template.xml pour enregistrer votre service.

### Pour iOS

Sur iOS, les notifications sont gérées par le système d'exploitation et peuvent lancer votre application lorsque l'utilisateur interagit avec elles. Vous devez configurer votre projet pour gérer cela :

```pascal
// Dans votre fichier principal de projet

procedure HandleReceivedLocalNotification(Sender: TObject;
  const Notification: TNotification);
begin
  // Gérer la notification reçue
  if Notification.Name.StartsWith('Reminder_') then
  begin
    var ReminderId := Notification.Name.Substring(9); // Enlever 'Reminder_'
    // Naviguer vers l'écran approprié
    // ...
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  NotificationCenter: TNotificationCenter;
begin
  // ... autre code d'initialisation

  // Configurer le gestionnaire de notifications
  NotificationCenter := TNotificationCenter.Create(Self);
  NotificationCenter.OnReceivedLocalNotification := HandleReceivedLocalNotification;
end;
```

## Bonnes pratiques pour les notifications dans les applications mobiles

1. **Respect de l'utilisateur**
   - Ne bombardez pas l'utilisateur de notifications inutiles
   - Offrez des paramètres pour personnaliser les notifications
   - Respectez les choix de l'utilisateur concernant les notifications

2. **Pertinence et timing**
   - Envoyez des notifications au bon moment de la journée
   - Assurez-vous que chaque notification apporte une valeur à l'utilisateur
   - Utilisez des données contextuelles pour personnaliser les notifications

3. **Messages clairs et concis**
   - Utilisez des titres explicites qui identifient immédiatement la source
   - Gardez les messages courts et directs
   - Évitez le jargon technique ou les abréviations peu claires

4. **Actions et navigation**
   - Assurez-vous que le tap sur une notification mène à l'écran approprié
   - Incluez des actions rapides lorsque c'est pertinent
   - Gérez correctement le deeplink (ouverture directe d'un écran spécifique)

5. **Gestion des erreurs**
   - Prévoyez un comportement dégradé si les notifications sont désactivées
   - Surveillez les échecs d'envoi pour les notifications push
   - Mettez en place un système de nouvelle tentative pour les notifications importantes

## Dépannage des problèmes courants

### Les notifications locales ne s'affichent pas

1. **Vérifiez les permissions**
   - Assurez-vous que les permissions appropriées sont configurées
   - Sur Android 13+, vérifiez que POST_NOTIFICATIONS est demandé

2. **Vérifiez le timing**
   - Assurez-vous que la date de déclenchement est dans le futur
   - Vérifiez que la date est correctement formatée

3. **Vérifiez les paramètres de l'appareil**
   - Les notifications peuvent être désactivées dans les paramètres de l'appareil
   - La mode Ne pas déranger peut bloquer les notifications

### Problèmes avec les notifications push

1. **Vérifiez la configuration du service**
   - Assurez-vous que les clés FCM/APNS sont correctes
   - Vérifiez que les certificats ne sont pas expirés

2. **Vérifiez la connectivité**
   - Les notifications push nécessitent une connexion internet
   - Vérifiez que les pare-feux n'interfèrent pas

3. **Vérifiez l'enregistrement du token**
   - Assurez-vous que le token d'appareil est correctement enregistré sur votre serveur
   - Vérifiez que les tokens sont mis à jour lors des changements

## Conclusion

Les notifications sont un outil puissant pour maintenir l'engagement des utilisateurs avec votre application. En utilisant Delphi, vous pouvez facilement implémenter des notifications locales et push dans vos applications mobiles cross-plateformes.

L'exemple du gestionnaire de rappels présenté dans ce chapitre montre comment intégrer les notifications dans une application réelle, en utilisant une approche modulaire et réutilisable. En suivant les bonnes pratiques et en tenant compte des spécificités de chaque plateforme, vous pouvez créer une expérience de notification qui améliore réellement l'utilité de votre application.

N'oubliez pas que les notifications, bien qu'utiles, doivent être utilisées avec parcimonie. Une application qui envoie trop de notifications risque d'être désinstallée ou de voir ses notifications désactivées par l'utilisateur. Concentrez-vous sur la qualité plutôt que sur la quantité, et assurez-vous que chaque notification apporte une réelle valeur à l'utilisateur.

Dans la prochaine section, nous aborderons le stockage local et la synchronisation des données, essentiels pour créer des applications mobiles qui fonctionnent efficacement même en l'absence de connexion internet.
