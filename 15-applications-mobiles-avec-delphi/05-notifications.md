🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.5 Notifications

## Introduction

Les notifications sont devenues un élément essentiel des applications mobiles modernes. Elles permettent à votre application de communiquer avec l'utilisateur même lorsqu'elle n'est pas active, en affichant des messages courts et pertinents sur l'écran d'accueil ou dans le centre de notifications de l'appareil.

Que ce soit pour rappeler un rendez-vous, signaler un nouveau message, informer d'une mise à jour, ou simplement maintenir l'engagement de l'utilisateur, les notifications sont un outil puissant qu'il faut utiliser avec discernement. Une notification bien pensée peut améliorer considérablement l'expérience utilisateur, tandis qu'un usage excessif ou inapproprié peut rapidement agacer et conduire à la désinstallation de l'application.

Avec Delphi, vous pouvez créer deux types de notifications : les **notifications locales** (générées directement par l'application) et les **notifications push** (envoyées depuis un serveur). Dans cette section, nous allons explorer ces deux approches et apprendre à les utiliser efficacement.

## Types de notifications

### Notifications locales

Les **notifications locales** sont programmées et déclenchées directement par votre application sur l'appareil de l'utilisateur. Elles ne nécessitent aucune connexion réseau et fonctionnent même lorsque l'application est fermée.

**Cas d'usage typiques** :
- Rappels et alarmes (réveil, minuteur)
- Notifications planifiées (rappel de médicament, rendez-vous)
- Événements déclenchés localement (fin d'un téléchargement, progression d'une tâche)
- Rappels contextuels (basés sur la localisation ou le temps)

### Notifications push

Les **notifications push** (ou notifications distantes) sont envoyées depuis un serveur vers l'appareil de l'utilisateur via les services de notification des plateformes (Apple Push Notification Service pour iOS, Firebase Cloud Messaging pour Android).

**Cas d'usage typiques** :
- Nouveaux messages ou commentaires
- Mises à jour d'actualités
- Promotions et offres spéciales
- Notifications en temps réel (scores sportifs, alertes)
- Notifications personnalisées basées sur le comportement utilisateur

## Notifications locales avec Delphi

Commençons par les notifications locales, qui sont plus simples à mettre en œuvre et ne nécessitent pas de configuration serveur.

### Configuration de base

Delphi fournit le composant `TNotificationCenter` pour gérer les notifications locales.

```pascal
uses
  System.Notification;

var
  NotificationCenter: TNotificationCenter;

// Initialiser le centre de notifications
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  NotificationCenter := TNotificationCenter.Create(nil);
end;

procedure TFormMain.FormDestroy(Sender: TObject);  
begin  
  NotificationCenter.Free;
end;
```

### Créer et afficher une notification simple

```pascal
// Créer et afficher une notification immédiate
procedure TFormMain.AfficherNotificationSimple;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    // Configurer la notification
    Notification.Name := 'NotificationSimple';
    Notification.Title := 'Nouvelle notification';
    Notification.AlertBody := 'Ceci est le contenu de la notification';

    // Afficher immédiatement (dans quelques secondes)
    Notification.FireDate := Now + EncodeTime(0, 0, 5, 0); // Dans 5 secondes

    // Programmer la notification
    NotificationCenter.ScheduleNotification(Notification);

    ShowMessage('Notification programmée !');
  finally
    Notification.Free;
  end;
end;
```

### Notification avec son et badge

```pascal
// Créer une notification avec son et badge
procedure TFormMain.NotificationAvecOptions;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'NotificationComplète';
    Notification.Title := 'Nouveau message';
    Notification.AlertBody := 'Vous avez reçu un nouveau message de Marie';

    // Ajouter un son (utilise le son par défaut du système)
    Notification.EnableSound := True;

    // Ajouter un badge (petit chiffre sur l'icône de l'app)
    // ⚠ Sur iOS, c'est natif et géré par le système. Sur Android,
    //   c'est le lanceur qui décide (Samsung et Pixel récents le
    //   gèrent, d'autres l'ignorent). Ne jamais en faire un canal
    //   d'information critique : c'est purement décoratif.
    Notification.Number := 3;

    // Programmer pour maintenant
    Notification.FireDate := Now;

    NotificationCenter.PresentNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

### Notifications récurrentes

```pascal
// Créer une notification qui se répète
procedure TFormMain.NotificationQuotidienne;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'RappelQuotidien';
    Notification.Title := 'Rappel quotidien';
    Notification.AlertBody := 'N''oubliez pas de prendre vos médicaments !';

    // Programmer pour demain à 9h00
    var DemainNeufHeures := Date + 1 + EncodeTime(9, 0, 0, 0);
    Notification.FireDate := DemainNeufHeures;

    // Définir la répétition (quotidienne)
    Notification.RepeatInterval := TRepeatInterval.Day;

    NotificationCenter.ScheduleNotification(Notification);

    ShowMessage('Notification quotidienne programmée pour 9h00');
  finally
    Notification.Free;
  end;
end;

// Récapitulatif des valeurs disponibles pour RepeatInterval.
// ⚠ Code purement illustratif — chaque affectation est indépendante,
//   on ne réutilise PAS la même variable Notification pour plusieurs
//   intervalles différents en pratique : créez UNE notification par
//   planning souhaité via NotificationCenter.CreateNotification.

// TRepeatInterval.None    — aucune répétition (valeur par défaut)
// TRepeatInterval.Second  — toutes les secondes
// TRepeatInterval.Minute  — toutes les minutes (utile pour les tests)
// TRepeatInterval.Hour    — toutes les heures
// TRepeatInterval.Day     — tous les jours
// TRepeatInterval.Weekday — chaque jour de semaine (lun → ven)
// TRepeatInterval.Week    — toutes les semaines (même jour)
// TRepeatInterval.Month   — tous les mois (même date)
// TRepeatInterval.Year    — tous les ans (même date)
```

### Notification avec actions

Sur certaines plateformes, vous pouvez ajouter des boutons d'action aux notifications.

```pascal
// Créer une notification avec boutons d'action
procedure TFormMain.NotificationAvecActions;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'NotificationActions';
    Notification.Title := 'Nouveau message';
    Notification.AlertBody := 'Jean vous a envoyé un message';
    Notification.FireDate := Now;

    // Ajouter des actions (boutons)
    Notification.AlertAction := 'Voir'; // Texte du bouton principal

    NotificationCenter.PresentNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

### Gérer la réponse aux notifications

Lorsque l'utilisateur clique sur une notification, votre application doit réagir de manière appropriée.

```pascal
// Gérer l'événement quand l'utilisateur clique sur une notification
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  NotificationCenter := TNotificationCenter.Create(nil);
  NotificationCenter.OnReceiveLocalNotification := GererNotificationRecue;
end;

procedure TFormMain.GererNotificationRecue(Sender: TObject;
  ANotification: TNotification);
begin
  // L'utilisateur a cliqué sur la notification
  ShowMessage('Notification reçue : ' + ANotification.Title);

  // Agir selon le nom de la notification
  if ANotification.Name = 'NotificationMessage' then
    OuvrirBoiteMessages
  else if ANotification.Name = 'RappelRendezVous' then
    OuvrirCalendrier;
end;

procedure TFormMain.OuvrirBoiteMessages;  
begin  
  // Naviguer vers l'écran des messages
  TabControl1.ActiveTab := TabMessages;
end;
```

### Annuler une notification programmée

```pascal
// Annuler une notification spécifique
procedure TFormMain.AnnulerNotification(NomNotification: string);  
begin  
  NotificationCenter.CancelNotification(NomNotification);
  ShowMessage('Notification "' + NomNotification + '" annulée');
end;

// Annuler toutes les notifications en attente
procedure TFormMain.AnnulerToutesNotifications;  
begin  
  NotificationCenter.CancelAll;
  ShowMessage('Toutes les notifications ont été annulées');
end;

// Vérifier les notifications programmées
procedure TFormMain.ListerNotificationsProgrammees;  
var  
  NotifsList: TNotificationsList;
  Notif: TNotification;
begin
  NotifsList := NotificationCenter.GetScheduledNotifications;
  try
    Memo1.Lines.Clear;
    Memo1.Lines.Add('Notifications programmées : ' + NotifsList.Count.ToString);

    for Notif in NotifsList do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Nom : ' + Notif.Name);
      Memo1.Lines.Add('Titre : ' + Notif.Title);
      Memo1.Lines.Add('Date : ' + DateTimeToStr(Notif.FireDate));
    end;
  finally
    NotifsList.Free;
  end;
end;
```

## Cas d'usage pratiques des notifications locales

### Minuteur et alarme

```pascal
uses
  System.DateUtils;  // pour IncMinute

// Créer un minuteur
procedure TFormMain.DemarrerMinuteur(Minutes: Integer);  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'Minuteur';
    Notification.Title := 'Minuteur terminé !';
    Notification.AlertBody := 'Le minuteur de ' + Minutes.ToString +
      ' minutes est terminé.';
    Notification.EnableSound := True;

    // ⚠ Ne PAS faire `Now + EncodeTime(0, Minutes, 0, 0)` :
    //   EncodeTime n'accepte que 0..59 pour le champ minute,
    //   donc Minutes >= 60 lèverait une exception.
    //   On utilise IncMinute (System.DateUtils) qui ne plafonne pas.
    Notification.FireDate := IncMinute(Now, Minutes);

    NotificationCenter.ScheduleNotification(Notification);

    ShowMessage('Minuteur programmé pour ' + Minutes.ToString + ' minutes');
  finally
    Notification.Free;
  end;
end;

// Créer une alarme quotidienne
procedure TFormMain.CreerAlarme(Heure, Minute: Integer);  
var  
  Notification: TNotification;
  DateAlarme: TDateTime;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'Alarme_' + Heure.ToString + '_' + Minute.ToString;
    Notification.Title := 'Alarme';
    Notification.AlertBody := 'Il est ' + Format('%.2d:%.2d', [Heure, Minute]);
    Notification.EnableSound := True;

    // Calculer la prochaine occurrence
    DateAlarme := Date + EncodeTime(Heure, Minute, 0, 0);
    if DateAlarme < Now then
      DateAlarme := DateAlarme + 1; // Si l'heure est passée, programmer pour demain

    Notification.FireDate := DateAlarme;
    Notification.RepeatInterval := TRepeatInterval.Day; // Répéter chaque jour

    NotificationCenter.ScheduleNotification(Notification);

    ShowMessage('Alarme programmée pour ' + TimeToStr(DateAlarme));
  finally
    Notification.Free;
  end;
end;
```

### Rappels de tâches

```pascal
uses
  System.DateUtils;  // pour IncHour

// Système de rappel de tâches
type
  TTache = record
    Titre: string;
    Description: string;
    DateEcheance: TDateTime;
  end;

procedure TFormMain.CreerRappelTache(Tache: TTache);  
var  
  Notification, NotificationJour: TNotification;
begin
  // ⚠ Notification.Name doit être unique et idéalement sans espaces /
  //   accents. On utilise l'ID de la tâche plutôt que son titre.
  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'Tache_AvantEcheance_' + Tache.Titre;
    Notification.Title := 'Rappel : ' + Tache.Titre;
    Notification.AlertBody := Tache.Description;
    Notification.EnableSound := True;

    // Rappel 1 heure avant l'échéance (IncHour gère bien les bornes)
    Notification.FireDate := IncHour(Tache.DateEcheance, -1);

    NotificationCenter.ScheduleNotification(Notification);

    // Créer un second rappel exactement à l'échéance
    NotificationJour := NotificationCenter.CreateNotification;
    try
      NotificationJour.Name := 'Tache_Echeance_' + Tache.Titre;
      NotificationJour.Title := 'Aujourd''hui : ' + Tache.Titre;
      NotificationJour.AlertBody := Tache.Description;
      NotificationJour.EnableSound := True;
      NotificationJour.FireDate := Tache.DateEcheance;

      NotificationCenter.ScheduleNotification(NotificationJour);
    finally
      NotificationJour.Free;
    end;
  finally
    Notification.Free;
  end;
end;
```

### Rappels basés sur la localisation

```pascal
// Créer un rappel géolocalisé (conceptuel)
procedure TFormMain.RappelGeolocalise;  
var  
  Notification: TNotification;
begin
  // Note : Les notifications géolocalisées nécessitent un service en arrière-plan
  // qui surveille la position et déclenche la notification manuellement

  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'RappelLieu';
    Notification.Title := 'Vous êtes proche !';
    Notification.AlertBody := 'Vous êtes près du supermarché, n''oubliez pas le lait !';
    Notification.EnableSound := True;

    // La notification sera déclenchée par votre code quand l'utilisateur
    // entre dans la zone définie (géorepérage/geofencing)
    NotificationCenter.PresentNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

## Notifications Push

Les notifications push permettent d'envoyer des notifications depuis un serveur distant vers les appareils de vos utilisateurs, même lorsque votre application n'est pas en cours d'exécution.

### Architecture des notifications push

Le système de notifications push implique plusieurs éléments :

1. **Votre serveur** : Envoie les notifications
2. **Service de notification** :
   - **APNs** (Apple Push Notification service) pour iOS
   - **FCM** (Firebase Cloud Messaging) pour Android
3. **Appareil de l'utilisateur** : Reçoit et affiche la notification
4. **Votre application** : Réagit lorsque l'utilisateur interagit avec la notification

### Configuration Firebase Cloud Messaging (Android)

Pour Android, vous devez configurer Firebase Cloud Messaging :

**Étapes de configuration** :

1. Créer un projet dans la console Firebase (https://console.firebase.google.com)
2. Ajouter votre application Android au projet
3. Télécharger le fichier `google-services.json`
4. Configurer Delphi pour utiliser FCM

```pascal
// Dans le manifest Android (Project > Options > Version Info)
// Ajouter les permissions nécessaires :

// <uses-permission android:name="android.permission.INTERNET" />
// <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />
// <uses-permission android:name="com.google.android.c2dm.permission.RECEIVE" />
```

### Configuration Apple Push Notification Service (iOS)

Pour iOS, la configuration est plus complexe :

1. Créer un certificat push dans le portail développeur Apple
2. Configurer l'identifiant de votre application avec les notifications push
3. Générer et télécharger les certificats nécessaires
4. Configurer Delphi avec le profil de provisionnement approprié

### Recevoir un token de notification

Avant de pouvoir recevoir des notifications push, votre application doit obtenir un token unique qui identifie l'appareil.

```pascal
uses
  System.PushNotification;

var
  PushService: TPushService;

// Initialiser le service de notifications push.
// Le nom du service dépend de la plateforme : « GCM » sur Android
// (l'identifiant historique a été conservé même si la techno
// derrière est FCM), « APS » sur iOS (Apple Push Service).
procedure TFormMain.FormCreate(Sender: TObject);  
const  
  {$IFDEF ANDROID}
  PUSH_SERVICE_NAME = TPushService.TServiceNames.GCM;
  {$ENDIF}
  {$IFDEF IOS}
  PUSH_SERVICE_NAME = TPushService.TServiceNames.APS;
  {$ENDIF}
begin
  PushService := TPushServiceManager.Instance.GetServiceByName(
    PUSH_SERVICE_NAME);

  if Assigned(PushService) then
  begin
    PushService.OnChange := GererChangementPush;
    PushService.OnReceiveNotification := GererNotificationPushRecue;
  end;
end;

// Demander l'activation des notifications push
procedure TFormMain.ActiverNotificationsPush;  
begin  
  if Assigned(PushService) then
  begin
    PushService.Active := True;
  end;
end;

// Gérer le changement de statut et récupérer le token
procedure TFormMain.GererChangementPush(Sender: TObject;
  AChange: TPushService.TChanges);
begin
  if TPushService.TChange.DeviceToken in AChange then
  begin
    // Token reçu, l'envoyer à votre serveur
    var DeviceToken := PushService.DeviceTokenValue[
      TPushService.TDeviceTokenNames.DeviceToken];

    ShowMessage('Token reçu : ' + DeviceToken);
    EnvoyerTokenAuServeur(DeviceToken);
  end;
end;

// Envoyer le token à votre serveur.
// ⚠ NE PAS concaténer Token directement dans une chaîne JSON :
//   un caractère " ou \ casserait le JSON. On construit le corps
//   via TJSONObject qui échappe correctement.
procedure TFormMain.EnvoyerTokenAuServeur(Token: string);  
var  
  HttpClient: THTTPClient;
  Body: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
begin
  HttpClient := THTTPClient.Create;
  Body := TJSONObject.Create;
  try
    Body.AddPair('token', Token);

    RequestBody := TStringStream.Create(Body.ToString, TEncoding.UTF8);
    try
      Response := HttpClient.Post(
        'https://votreserveur.com/api/register-device', RequestBody);

      if Response.StatusCode = 200 then
        ShowMessage('Token enregistré sur le serveur')
      else
        ShowMessage('Erreur lors de l''enregistrement : ' +
                    Response.StatusCode.ToString);
    finally
      RequestBody.Free;
    end;
  finally
    Body.Free;
    HttpClient.Free;
  end;
end;
```

### Recevoir et gérer les notifications push

```pascal
// Gérer la réception d'une notification push
procedure TFormMain.GererNotificationPushRecue(Sender: TObject;
  const ANotification: TPushServiceNotification);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      // Afficher le contenu de la notification
      ShowMessage('Notification push reçue : ' + ANotification.DataObject.ToString);

      // Extraire les données
      var Titre := ANotification.DataKey['title'];
      var Message := ANotification.DataKey['message'];
      var TypeNotif := ANotification.DataKey['type'];

      // Agir selon le type de notification
      if TypeNotif = 'message' then
        OuvrirMessages
      else if TypeNotif = 'alerte' then
        AfficherAlerte(Message);
    end);
end;
```

### Envoyer une notification push depuis le serveur

Voici un exemple de code serveur (en pseudo-code) pour envoyer une notification push :

> ⚠ **API FCM Legacy décommissionnée le 20 juin 2024.** L'endpoint historique `https://fcm.googleapis.com/fcm/send` et l'authentification par clé serveur (`Authorization: key=…`) ne fonctionnent plus. La **nouvelle API FCM HTTP v1** utilise :  
> - URL : `https://fcm.googleapis.com/v1/projects/<PROJECT_ID>/messages:send`  
> - Authentification : **OAuth 2.0** avec un *access token* généré depuis un compte de service Google (fichier `service-account.json`, JWT court-vivant)  
> - Corps : structure JSON différente (`{"message": {"token": "...", "notification": {...}}}`)  
>  
> Voir la documentation officielle : https://firebase.google.com/docs/cloud-messaging/migrate-v1  
>  
> Le code ci-dessous illustre la **structure historique** pour comprendre le principe ; il ne fonctionnera plus tel quel et doit être adapté à HTTP v1 + OAuth 2.0 pour tout nouveau projet.

```pascal
// Exemple conceptuel d'envoi de notification via FCM (Firebase)
// Ce code s'exécuterait sur votre serveur, pas dans l'application mobile

procedure EnvoyerNotificationPush(DeviceToken, Titre, Message: string);  
var  
  HttpClient: THTTPClient;
  RequestBody: TJSONObject;
  Headers: TNetHeaders;
  PostStream: TStringStream;
begin
  // Créer le corps de la requête JSON
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('to', DeviceToken);

    var Notification := TJSONObject.Create;
    Notification.AddPair('title', Titre);
    Notification.AddPair('body', Message);
    Notification.AddPair('sound', 'default');

    RequestBody.AddPair('notification', Notification);

    // Configurer les en-têtes avec votre clé serveur FCM
    SetLength(Headers, 2);
    Headers[0] := TNetHeader.Create('Authorization',
      'key=VOTRE_CLE_SERVEUR_FCM');
    Headers[1] := TNetHeader.Create('Content-Type', 'application/json');

    // Envoyer la requête à FCM (ancien endpoint legacy, voir avertissement)
    HttpClient := THTTPClient.Create;
    PostStream := TStringStream.Create(RequestBody.ToString, TEncoding.UTF8);
    try
      var Response := HttpClient.Post(
        'https://fcm.googleapis.com/fcm/send',
        PostStream,
        nil,
        Headers);

      WriteLn('Notification envoyée : ' + Response.StatusCode.ToString);
    finally
      PostStream.Free;
      HttpClient.Free;
    end;
  finally
    RequestBody.Free;
  end;
end;
```

## Permissions pour les notifications

Sur les versions récentes d'Android et iOS, les applications doivent demander explicitement la permission d'afficher des notifications.

### Demander la permission

```pascal
uses
  System.Permissions, FMX.DialogService;

// Vérifier et demander la permission pour les notifications
procedure TFormMain.DemanderPermissionNotifications;  
begin  
  {$IFDEF ANDROID}
  // Android 13+ (API 33+) : permission runtime POST_NOTIFICATIONS
  // exigée. Sans elle, les notifications sont silencieusement
  // ignorées par le système.
  PermissionsService.RequestPermissions(
    ['android.permission.POST_NOTIFICATIONS'],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        ShowMessage('Permission accordée pour les notifications');
        ActiverNotifications;
      end
      else
        TDialogService.ShowMessage(
          'Les notifications sont désactivées. ' +
          'Vous pouvez les activer dans les paramètres.');
    end);
  {$ENDIF}

  {$IFDEF IOS}
  // iOS : la première `ScheduleNotification` ou `PresentNotification`
  // déclenche automatiquement le dialogue système de demande
  // d'autorisation. TNotificationCenter.AuthorizationStatus permet
  // ensuite de vérifier le statut (Authorized / Denied / NotDetermined).
  // Pour les notifications PUSH, l'autorisation est demandée lors
  // de l'appel `PushService.Active := True`.
  ActiverNotifications;
  {$ENDIF}
end;

procedure TFormMain.ActiverNotifications;  
begin  
  // Activer les notifications locales et/ou push
  if Assigned(PushService) then
    PushService.Active := True;
end;
```

## Bonnes pratiques pour les notifications

### Fréquence et pertinence

```pascal
// Éviter de spammer l'utilisateur
type
  TGestionnaireNotifications = class
  private
    FDerniereNotification: TDateTime;
    FNombreNotificationsJour: Integer;
  public
    function PeutEnvoyerNotification: Boolean;
    procedure EnregistrerNotificationEnvoyee;
  end;

function TGestionnaireNotifications.PeutEnvoyerNotification: Boolean;  
const  
  DelaiMinimum = 1 / 24; // 1 heure minimum entre deux notifications
  MaxNotificationsParJour = 5;
begin
  // Vérifier le délai depuis la dernière notification
  if Now - FDerniereNotification < DelaiMinimum then
  begin
    Result := False;
    Exit;
  end;

  // Vérifier le nombre de notifications aujourd'hui
  if FNombreNotificationsJour >= MaxNotificationsParJour then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TGestionnaireNotifications.EnregistrerNotificationEnvoyee;  
begin  
  FDerniereNotification := Now;
  Inc(FNombreNotificationsJour);

  // Réinitialiser le compteur à minuit
  if Date > Trunc(FDerniereNotification) then
    FNombreNotificationsJour := 1;
end;
```

### Personnalisation et contexte

```pascal
// Créer des notifications personnalisées selon l'utilisateur
procedure TFormMain.NotificationPersonnalisee(NomUtilisateur: string;
  TypeEvenement: string);
var
  Notification: TNotification;
begin
  Notification := NotificationCenter.CreateNotification;
  try
    if TypeEvenement = 'anniversaire' then
    begin
      Notification.Title := 'Joyeux anniversaire !';
      Notification.AlertBody := NomUtilisateur +
        ', nous vous souhaitons un excellent anniversaire ! 🎉';
    end
    else if TypeEvenement = 'objectif_atteint' then
    begin
      Notification.Title := 'Objectif atteint ! 🎯';
      Notification.AlertBody := 'Félicitations ' + NomUtilisateur +
        ', vous avez atteint votre objectif !';
    end
    else if TypeEvenement = 'rappel_activite' then
    begin
      Notification.Title := 'On y va ?';
      Notification.AlertBody := NomUtilisateur +
        ', cela fait 3 jours que vous n''avez pas ouvert l''application.';
    end;

    Notification.FireDate := Now;
    Notification.EnableSound := True;
    NotificationCenter.PresentNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

### Respect des heures de silence

```pascal
// Ne pas envoyer de notifications la nuit
function TFormMain.EstHeureAppropriee: Boolean;  
var  
  Heure: Word;
begin
  Heure := HourOf(Now);

  // Éviter les notifications entre 22h et 8h
  Result := (Heure >= 8) and (Heure < 22);
end;

procedure TFormMain.ProgrammerNotificationIntelligente;  
var  
  Notification: TNotification;
  DateNotification: TDateTime;
begin
  DateNotification := Now;

  // Si ce n'est pas une heure appropriée, reporter au lendemain matin
  if not EstHeureAppropriee then
  begin
    // Programmer pour demain à 9h00
    DateNotification := Date + 1 + EncodeTime(9, 0, 0, 0);
  end;

  Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'NotificationIntelligente';
    Notification.Title := 'Rappel';
    Notification.AlertBody := 'Vous avez des tâches en attente';
    Notification.FireDate := DateNotification;

    NotificationCenter.ScheduleNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

### Permettre à l'utilisateur de contrôler les notifications

```pascal
// Interface de paramètres des notifications
procedure TFormMain.ConfigurerPreferencesNotifications;  
begin  
  // Créer une page de paramètres
  SwitchNotificationsActives.IsChecked := LirePreference('NotificationsActives', True);
  SwitchNotificationsSonores.IsChecked := LirePreference('NotificationsSonores', True);
  SwitchNotificationsNuit.IsChecked := LirePreference('NotificationsNuit', False);

  ComboFrequence.ItemIndex := LirePreference('FrequenceNotifications', 1);
  // 0 = Immédiat, 1 = Normal, 2 = Rare
end;

procedure TFormMain.SwitchNotificationsActivesSwitch(Sender: TObject);  
begin  
  SauvegarderPreference('NotificationsActives',
    SwitchNotificationsActives.IsChecked);

  if not SwitchNotificationsActives.IsChecked then
  begin
    // Désactiver toutes les notifications
    NotificationCenter.CancelAll;
    if Assigned(PushService) then
      PushService.Active := False;
  end
  else
  begin
    // Réactiver les notifications
    if Assigned(PushService) then
      PushService.Active := True;
  end;
end;
```

## Test et débogage des notifications

### Tester les notifications locales

```pascal
// Interface de test des notifications
procedure TFormMain.BtnTestNotificationClick(Sender: TObject);  
begin  
  // Créer une notification de test qui s'affiche rapidement
  var Notification := NotificationCenter.CreateNotification;
  try
    Notification.Name := 'Test';
    Notification.Title := 'Test notification';
    Notification.AlertBody := 'Ceci est une notification de test';
    Notification.EnableSound := True;

    // Afficher dans 3 secondes pour laisser le temps de mettre l'app en arrière-plan
    Notification.FireDate := Now + EncodeTime(0, 0, 3, 0);

    NotificationCenter.ScheduleNotification(Notification);

    ShowMessage('Notification de test programmée. ' +
      'Mettez l''application en arrière-plan maintenant.');
  finally
    Notification.Free;
  end;
end;

// Afficher toutes les notifications programmées
procedure TFormMain.BtnVoirNotificationsProgrammeesClick(Sender: TObject);  
var  
  NotifsList: TNotificationsList;
  Notif: TNotification;
begin
  ListBox1.Clear;

  NotifsList := NotificationCenter.GetScheduledNotifications;
  try
    for Notif in NotifsList do
    begin
      ListBox1.Items.Add(
        Format('%s - %s (%s)',
        [Notif.Title, Notif.AlertBody, DateTimeToStr(Notif.FireDate)]));
    end;

    if ListBox1.Count = 0 then
      ListBox1.Items.Add('Aucune notification programmée');
  finally
    NotifsList.Free;
  end;
end;
```

## Conclusion

Les notifications sont un outil puissant pour maintenir l'engagement des utilisateurs et améliorer l'expérience de votre application mobile. Cependant, elles doivent être utilisées avec discernement et respect envers l'utilisateur.

Les points clés à retenir :

1. **Notifications locales** : Simples à implémenter, ne nécessitent pas de serveur, idéales pour les rappels
2. **Notifications push** : Permettent la communication en temps réel depuis un serveur, nécessitent une configuration plus complexe
3. **Pertinence** : N'envoyez que des notifications utiles et contextuelles
4. **Fréquence** : Limitez le nombre de notifications pour éviter d'agacer l'utilisateur
5. **Timing** : Respectez les heures appropriées (évitez la nuit)
6. **Personnalisation** : Adaptez les notifications au contexte et aux préférences de l'utilisateur
7. **Contrôle** : Donnez toujours à l'utilisateur la possibilité de désactiver les notifications
8. **Permissions** : Demandez et gérez correctement les permissions nécessaires

Une stratégie de notification bien pensée peut transformer une application ordinaire en un compagnon indispensable pour l'utilisateur. Dans la section suivante, nous verrons comment gérer le stockage local et la synchronisation des données pour que vos applications fonctionnent même hors ligne.

⏭️ [Stockage local et synchronisation](/15-applications-mobiles-avec-delphi/06-stockage-local-et-synchronisation.md)
