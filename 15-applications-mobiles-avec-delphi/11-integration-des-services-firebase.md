🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.11 Intégration des services Firebase

## Introduction

Firebase est une plateforme complète développée par Google qui offre une multitude de services pour faciliter le développement d'applications mobiles. Imaginez Firebase comme une boîte à outils tout-en-un qui vous fournit des solutions prêtes à l'emploi pour les problèmes courants du développement mobile : notifications push, analytics, authentification, base de données en temps réel, stockage de fichiers, et bien plus encore.

L'un des grands avantages de Firebase est qu'il élimine le besoin de créer et gérer votre propre infrastructure serveur pour de nombreuses fonctionnalités. Vous pouvez vous concentrer sur le développement de votre application pendant que Firebase s'occupe du backend, de la scalabilité et de la fiabilité.

Dans cette section, nous allons explorer comment intégrer les principaux services Firebase dans vos applications Delphi, étape par étape, de manière accessible même si vous n'avez jamais utilisé Firebase auparavant.

## Qu'est-ce que Firebase ?

### Vue d'ensemble

Firebase est une plateforme **BaaS** (Backend as a Service) qui propose :

**Services essentiels** :
- **Authentication** : Authentification des utilisateurs (email, Google, Facebook, etc.)
- **Realtime Database** : Base de données NoSQL synchronisée en temps réel
- **Cloud Firestore** : Base de données NoSQL nouvelle génération, plus flexible
- **Cloud Storage** : Stockage de fichiers (images, vidéos, documents)
- **Cloud Messaging (FCM)** : Notifications push vers Android et iOS

**Services de développement** :
- **Analytics** : Statistiques d'utilisation détaillées
- **Crashlytics** : Rapports de crash en temps réel
- **Performance Monitoring** : Surveillance des performances
- **Test Lab** : Tests sur de vrais appareils

**Services de croissance** :
- **Remote Config** : Configuration à distance sans mise à jour
- **A/B Testing** : Tests de variantes de fonctionnalités
- **Dynamic Links** : Liens intelligents pour l'acquisition d'utilisateurs

### Pourquoi utiliser Firebase avec Delphi ?

**Avantages** :
- Réduction du temps de développement (pas besoin de créer un backend)
- Infrastructure scalable automatiquement
- Gratuit pour les petits projets (offre généreuse)
- Documentation extensive et communauté active
- Intégration native avec Android (Delphi compile en natif Android)

**Cas d'usage idéaux** :
- Application de chat en temps réel
- Application avec authentification sociale
- Application nécessitant des notifications push
- Application avec synchronisation multi-appareils
- MVP (Minimum Viable Product) à développer rapidement

## Configuration initiale de Firebase

### Étape 1 : Créer un projet Firebase

1. Allez sur https://console.firebase.google.com
2. Cliquez sur **Ajouter un projet** (ou "Add project")
3. Donnez un nom à votre projet (ex: "MonAppDelphi")
4. Acceptez les conditions d'utilisation
5. Activez Google Analytics si vous le souhaitez (recommandé)
6. Cliquez sur **Créer le projet**

### Étape 2 : Ajouter une application Android

1. Dans la console Firebase, cliquez sur l'icône **Android**
2. Entrez le **nom du package** de votre application Delphi
   - Trouvez-le dans Delphi : `Project > Options > Version Info`
   - Exemple : `com.monentreprise.monapp`
3. (Optionnel) Entrez un surnom et un certificat SHA-1
4. Cliquez sur **Enregistrer l'application**
5. **Téléchargez le fichier `google-services.json`**
6. Cliquez sur **Suivant** puis **Continuer vers la console**

### Étape 3 : Ajouter une application iOS (optionnel)

1. Dans la console Firebase, cliquez sur l'icône **iOS**
2. Entrez le **Bundle ID** de votre application
   - Trouvez-le dans Delphi : `Project > Options > Version Info (iOS)`
   - Doit correspondre exactement au Bundle ID
3. Téléchargez le fichier **GoogleService-Info.plist**
4. Suivez les instructions de configuration iOS

### Étape 4 : Intégrer les fichiers de configuration dans Delphi

**Pour Android** :

```
1. Copiez le fichier google-services.json
2. Dans Delphi : Project > Deployment
3. Cliquez sur "Add Files"
4. Sélectionnez google-services.json
5. Remote Path : assets\internal\
6. Condition : Android
```

**Pour iOS** :

```
1. Copiez le fichier GoogleService-Info.plist
2. Dans Delphi : Project > Deployment
3. Ajoutez le fichier
4. Remote Path : StartUp\Documents\
5. Condition : iOS Device
```

## Firebase Cloud Messaging (Notifications Push)

FCM permet d'envoyer des notifications push à vos utilisateurs sur Android et iOS.

### Activation de FCM dans Firebase

1. Dans la console Firebase, allez dans **Project Settings** (⚙️)
2. Onglet **Cloud Messaging**
3. Pour l'envoi côté serveur, créez un **compte de service** (Service Account) via l'onglet « Comptes de service » des Project Settings — la *Server Key* historique n'est plus utilisable (voir l'encadré sur FCM HTTP v1 plus bas).

### Configuration dans Delphi

**Permissions Android** :

```
Project > Options > Uses Permissions  
Cocher :  
☑ Receive Boot Completed
☑ Internet
☑ Access Network State
☑ Post Notifications      ← OBLIGATOIRE depuis Android 13 (API 33)
☑ Wake Lock               ← Recommandé pour traiter les notifications en veille
```

> ⚠ **Android 13+ : permission `POST_NOTIFICATIONS` obligatoire.** Sans cette permission *runtime* explicitement demandée à l'utilisateur (via `PermissionsService.RequestPermissions`), l'application **ne reçoit aucune notification** sur les appareils Android 13 ou supérieur, même si le token FCM est correctement obtenu.

**Composants nécessaires** :

Delphi fournit `TPushService` pour gérer FCM.

```pascal
uses
  System.PushNotification, System.JSON;

var
  PushService: TPushService;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Créer le service push
  PushService := TPushServiceManager.Instance.GetServiceByName(
    TPushService.TServiceNames.GCM); // GCM pour Android (Google Cloud Messaging)

  if Assigned(PushService) then
  begin
    // Configurer les événements
    PushService.OnChange := PushServiceChange;
    PushService.OnReceiveNotification := PushServiceReceiveNotification;

    // Activer le service
    PushService.Active := True;
  end;
end;
```

### Obtenir le token FCM

Le token FCM est un identifiant unique pour chaque appareil :

```pascal
// Gérer les changements du service push
procedure TFormMain.PushServiceChange(Sender: TObject;
  AChange: TPushService.TChanges);
var
  DeviceToken: string;
begin
  // Vérifier si on a reçu le token
  if TPushService.TChange.DeviceToken in AChange then
  begin
    DeviceToken := PushService.DeviceTokenValue[
      TPushService.TDeviceTokenNames.DeviceToken];

    Memo1.Lines.Add('Token FCM reçu :');
    Memo1.Lines.Add(DeviceToken);

    // Envoyer ce token à votre serveur
    EnvoyerTokenAuServeur(DeviceToken);
  end;
end;

// Envoyer le token à votre serveur
procedure TFormMain.EnvoyerTokenAuServeur(const Token: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONData: TJSONObject;
  PostStream: TStringStream;
begin
  HttpClient := THTTPClient.Create;
  try
    JSONData := TJSONObject.Create;
    try
      JSONData.AddPair('device_token', Token);
      JSONData.AddPair('user_id', GetUserID);
      JSONData.AddPair('platform', {$IFDEF ANDROID}'android'{$ELSE}'ios'{$ENDIF});

      // Stream créé explicitement pour pouvoir le libérer ensuite —
      // THTTPClient.Post ne prend pas la propriété du stream.
      PostStream := TStringStream.Create(JSONData.ToString, TEncoding.UTF8);
      try
        Response := HttpClient.Post(
          'https://votreserveur.com/api/register-device',
          PostStream);
      finally
        PostStream.Free;
      end;

      if Response.StatusCode = 200 then
        ShowMessage('Token enregistré sur le serveur')
      else
        ShowMessage('Erreur : ' + Response.StatusCode.ToString);
    finally
      JSONData.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

### Recevoir des notifications

```pascal
// Gérer les notifications reçues
procedure TFormMain.PushServiceReceiveNotification(Sender: TObject;
  const ANotification: TPushServiceNotification);
var
  Titre, Message: string;
begin
  // Extraire les données de la notification
  Titre := ANotification.DataKey['title'];
  Message := ANotification.DataKey['message'];

  // Afficher dans un log
  Memo1.Lines.Add('Notification reçue :');
  Memo1.Lines.Add('Titre : ' + Titre);
  Memo1.Lines.Add('Message : ' + Message);

  // Afficher à l'utilisateur
  TThread.Synchronize(nil,
    procedure
    begin
      ShowMessage(Titre + sLineBreak + Message);
    end);

  // Traiter les données personnalisées
  var ActionType := ANotification.DataKey['action'];
  if ActionType = 'open_chat' then
  begin
    var ChatID := ANotification.DataKey['chat_id'];
    OuvrirChat(ChatID);
  end;
end;
```

### Envoyer une notification depuis un serveur

Vous pouvez envoyer des notifications depuis votre serveur ou depuis la console Firebase.

**Depuis la console Firebase** (pour tester) :

1. Allez dans **Cloud Messaging**
2. Cliquez sur **Envoyer votre premier message**
3. Rédigez le message
4. Sélectionnez l'application cible
5. Envoyez

**Depuis votre serveur** (exemple en pseudo-code) :

> 🚨 **API FCM Legacy décommissionnée le 20 juin 2024.** L'endpoint historique `https://fcm.googleapis.com/fcm/send` ainsi que l'authentification par *Server Key* (`Authorization: key=…`) **ne fonctionnent plus**. Toute application qui dépendait encore de cette API renvoie désormais des erreurs HTTP 404 ou 401.  
>  
> La **nouvelle API FCM HTTP v1** utilise :  
> - URL : `https://fcm.googleapis.com/v1/projects/<PROJECT_ID>/messages:send`  
> - Authentification : **OAuth 2.0** avec un *access token* généré depuis un compte de service Google (fichier JSON téléchargé depuis Firebase Console > Project Settings > Service Accounts).  
> - Format du payload légèrement différent (objet `message: { token, notification, data }`).  
>  
> L'OAuth 2.0 doit être effectué **côté serveur** (jamais dans l'app cliente : le secret du compte de service ne doit pas se retrouver dans un APK/IPA).

```pascal
// Exemple de requête HTTP vers FCM HTTP v1 — l'access token OAuth 2.0
// doit avoir été obtenu au préalable côté serveur à partir du JSON du
// compte de service Google (scope https://www.googleapis.com/auth/firebase.messaging).
procedure EnvoyerNotificationFCM(const ProjectID, AccessToken,
  DeviceToken, Titre, Message: string);
var
  HttpClient: THTTPClient;
  Headers: TNetHeaders;
  Body, MessageObj, Notification, Data: TJSONObject;
  PostStream: TStringStream;
  URL: string;
  Response: IHTTPResponse;
begin
  HttpClient := THTTPClient.Create;
  try
    // En-têtes : Bearer token OAuth 2.0 + JSON
    Headers := [
      TNetHeader.Create('Authorization', 'Bearer ' + AccessToken),
      TNetHeader.Create('Content-Type',  'application/json')
    ];

    // Corps de la requête au format FCM HTTP v1
    Body := TJSONObject.Create;
    try
      MessageObj := TJSONObject.Create;
      MessageObj.AddPair('token', DeviceToken);

      Notification := TJSONObject.Create;
      Notification.AddPair('title', Titre);
      Notification.AddPair('body',  Message);
      MessageObj.AddPair('notification', Notification);

      // Données personnalisées (toujours des chaînes en HTTP v1)
      Data := TJSONObject.Create;
      Data.AddPair('action',    'open_screen');
      Data.AddPair('screen_id', 'main');
      MessageObj.AddPair('data', Data);

      Body.AddPair('message', MessageObj);

      URL := Format(
        'https://fcm.googleapis.com/v1/projects/%s/messages:send',
        [ProjectID]);

      PostStream := TStringStream.Create(Body.ToString, TEncoding.UTF8);
      try
        Response := HttpClient.Post(URL, PostStream, nil, Headers);
        Memo1.Lines.Add('Réponse FCM : ' + Response.ContentAsString);
      finally
        PostStream.Free;
      end;
    finally
      Body.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Firebase Analytics

Firebase Analytics vous permet de suivre le comportement des utilisateurs dans votre application.

### Configuration

Analytics est automatiquement activé quand vous ajoutez le fichier `google-services.json`.

> 💡 **À propos de l'API Analytics dans Delphi.** Les unités `FMX.Analytics` / `System.Analytics` et l'interface `IFMXAnalyticsService` utilisées ci-dessous sont **conceptuelles** : la RTL Delphi standard ne fournit pas de wrapper officiel pour le SDK Firebase Analytics. En pratique, on intègre Analytics via :  
> - une bibliothèque tierce (TMS FNC Cloud Pack, Kastri Free, etc.),  
> - ou un wrapper JNI/Objective-C maison autour des SDK natifs Firebase Analytics.  
>  
> Le code ci-dessous illustre uniquement le **pattern** d'utilisation côté Delphi ; les noms exacts d'unités et d'interfaces dépendent de la bibliothèque que vous intégrez.

### Logger des événements

```pascal
uses
  FMX.Analytics, System.Analytics;  // ⚠ unités conceptuelles, voir note ci-dessus

// Logger un événement simple
procedure TFormMain.LoggerEvenement(const NomEvenement: string);  
var  
  AnalyticsService: IFMXAnalyticsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXAnalyticsService, AnalyticsService) then
  begin
    AnalyticsService.TrackEvent(NomEvenement);
  end;
end;

// Logger un événement avec paramètres
procedure TFormMain.LoggerEvenementAvecParams(const NomEvenement: string;
  Params: TArray<string>);
var
  AnalyticsService: IFMXAnalyticsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXAnalyticsService, AnalyticsService) then
  begin
    AnalyticsService.TrackEvent(NomEvenement, Params);
  end;
end;

// Exemples d'utilisation
procedure TFormMain.BtnAcheterClick(Sender: TObject);  
begin  
  // Logger l'achat
  LoggerEvenementAvecParams('purchase',
    ['item_id', '12345', 'price', '9.99', 'currency', 'EUR']);

  // Continuer avec l'achat
  TraiterAchat;
end;

procedure TFormMain.BtnPartagerClick(Sender: TObject);  
begin  
  // Logger le partage
  LoggerEvenementAvecParams('share',
    ['content_type', 'article', 'item_id', 'article_123']);

  PartagerContenu;
end;
```

### Événements prédéfinis

Firebase propose des événements standards pour les cas d'usage courants :

> 💡 **Événements automatiquement collectés.** Firebase Analytics enregistre **automatiquement** une liste d'événements sans intervention de votre part : `first_open`, `session_start`, `app_remove`, `screen_view` (sur Android avec le SDK officiel), `notification_open`, etc. Vous n'avez **pas besoin** de logger manuellement `app_open` au démarrage — sauf si vous voulez ajouter vos propres paramètres. La liste complète est dans la documentation Firebase.

```pascal
// Connexion utilisateur (événement standard)
procedure TFormMain.UtilisateurConnecte;  
begin  
  LoggerEvenementAvecParams('login',
    ['method', 'email']);
end;

// Inscription
procedure TFormMain.UtilisateurInscrit;  
begin  
  LoggerEvenementAvecParams('sign_up',
    ['method', 'email']);
end;

// Vue d'écran
procedure TFormMain.EcranAffiche(const NomEcran: string);  
begin  
  LoggerEvenementAvecParams('screen_view',
    ['screen_name', NomEcran]);
end;

// Recherche
procedure TFormMain.RechercheEffectuee(const Termes: string);  
begin  
  LoggerEvenementAvecParams('search',
    ['search_term', Termes]);
end;
```

### Propriétés utilisateur

Vous pouvez définir des propriétés pour segmenter vos utilisateurs :

```pascal
procedure TFormMain.DefinirProprietesUtilisateur;  
var  
  AnalyticsService: IFMXAnalyticsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXAnalyticsService, AnalyticsService) then
  begin
    // Définir l'âge de l'utilisateur
    AnalyticsService.SetUserProperty('age_group', '25-34');

    // Définir le type d'abonnement
    AnalyticsService.SetUserProperty('subscription', 'premium');

    // Définir les préférences
    AnalyticsService.SetUserProperty('theme', 'dark');
  end;
end;
```

### Consulter les Analytics

1. Ouvrez la console Firebase
2. Allez dans **Analytics** > **Dashboard**
3. Visualisez les statistiques en temps réel
4. Explorez les événements, les audiences et les conversions

## Firebase Realtime Database

La Realtime Database est une base de données NoSQL qui synchronise les données en temps réel entre tous les clients.

### Structure de données

Firebase utilise une structure JSON :

```json
{
  "users": {
    "user1": {
      "name": "Jean Dupont",
      "email": "jean@example.com",
      "age": 30
    },
    "user2": {
      "name": "Marie Martin",
      "email": "marie@example.com",
      "age": 25
    }
  },
  "messages": {
    "message1": {
      "text": "Bonjour !",
      "sender": "user1",
      "timestamp": 1634567890
    }
  }
}
```

### Configuration des règles de sécurité

Dans la console Firebase :

```json
{
  "rules": {
    ".read": "auth != null",
    ".write": "auth != null",

    "users": {
      "$uid": {
        ".read": "$uid === auth.uid",
        ".write": "$uid === auth.uid"
      }
    }
  }
}
```

### Accès à la base de données avec REST

> 💡 **Nom de domaine Realtime Database.** Le suffixe dépend de la région choisie à la création :  
> - `https://<projet>.firebaseio.com` (US central, ancien format),  
> - `https://<projet>-default-rtdb.firebaseio.com` (US central, format actuel),  
> - `https://<projet>-default-rtdb.europe-west1.firebasedatabase.app` (UE),  
> - `https://<projet>-default-rtdb.asia-southeast1.firebasedatabase.app` (Asie).  
>  
> L'URL exacte est visible dans la console Firebase, onglet *Realtime Database*. Les exemples ci-dessous utilisent l'ancien format pour la lisibilité ; remplacez par votre URL réelle.

Firebase expose la Realtime Database via une API REST :

> ⚠ **Attention aux fuites mémoire avec `TStringStream`.** Dans les exemples qui suivent, le pattern `HttpClient.Post(URL, TStringStream.Create(...))` est utilisé pour la lisibilité, **mais il fuit le stream** : `THTTPClient` n'en prend pas la propriété. En production, créez le stream dans une variable et libérez-le dans un bloc `try..finally` :  
>  
> ```pascal  
> PostStream := TStringStream.Create(Body.ToString, TEncoding.UTF8);  
> try  
>   Response := HttpClient.Post(URL, PostStream);  
> finally  
>   PostStream.Free;  
> end;  
> ```  
>  
> Voir l'exemple `EnvoyerNotificationFCM` plus haut pour le pattern complet.

```pascal
uses
  System.Net.HttpClient, System.JSON;

// Lire des données
procedure TFormMain.LireDonnees;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONData: TJSONValue;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    // URL de votre base Firebase
    URL := 'https://votre-projet.firebaseio.com/users/user1.json';

    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONData := TJSONObject.ParseJSONValue(Response.ContentAsString);
      try
        // Extraire les données
        var Nom := JSONData.GetValue<string>('name');
        var Email := JSONData.GetValue<string>('email');

        Memo1.Lines.Add('Nom : ' + Nom);
        Memo1.Lines.Add('Email : ' + Email);
      finally
        JSONData.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Écrire des données
procedure TFormMain.EcrireDonnees;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONData: TJSONObject;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    URL := 'https://votre-projet.firebaseio.com/users/user3.json';

    JSONData := TJSONObject.Create;
    try
      JSONData.AddPair('name', 'Paul Bernard');
      JSONData.AddPair('email', 'paul@example.com');
      JSONData.AddPair('age', TJSONNumber.Create(28));

      Response := HttpClient.Put(URL,
        TStringStream.Create(JSONData.ToString, TEncoding.UTF8));

      if Response.StatusCode = 200 then
        ShowMessage('Données enregistrées')
      else
        ShowMessage('Erreur : ' + Response.StatusCode.ToString);
    finally
      JSONData.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Mettre à jour des données
procedure TFormMain.MettreAJourDonnees;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONData: TJSONObject;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    URL := 'https://votre-projet.firebaseio.com/users/user1.json';

    JSONData := TJSONObject.Create;
    try
      // Mettre à jour seulement l'âge
      JSONData.AddPair('age', TJSONNumber.Create(31));

      Response := HttpClient.Patch(URL,
        TStringStream.Create(JSONData.ToString, TEncoding.UTF8));

      if Response.StatusCode = 200 then
        ShowMessage('Données mises à jour');
    finally
      JSONData.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Supprimer des données
procedure TFormMain.SupprimerDonnees;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    URL := 'https://votre-projet.firebaseio.com/users/user3.json';

    Response := HttpClient.Delete(URL);

    if Response.StatusCode = 200 then
      ShowMessage('Données supprimées');
  finally
    HttpClient.Free;
  end;
end;
```

### Authentification avec Firebase Auth

Pour sécuriser l'accès, utilisez un token d'authentification :

```pascal
// Lire avec authentification
procedure TFormMain.LireAvecAuth;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
  AuthToken: string;
begin
  AuthToken := ObtenirTokenAuthentification; // Voir section Authentication

  HttpClient := THTTPClient.Create;
  try
    URL := Format('https://votre-projet.firebaseio.com/users/user1.json?auth=%s',
      [AuthToken]);

    Response := HttpClient.Get(URL);

    // Traiter la réponse...
  finally
    HttpClient.Free;
  end;
end;
```

## Firebase Authentication

Firebase Authentication gère l'authentification des utilisateurs avec différentes méthodes.

### Méthodes d'authentification disponibles

- Email/Mot de passe
- Google
- Facebook
- Twitter
- Téléphone (SMS)
- Anonyme
- Apple (iOS)

### Activation dans Firebase

1. Console Firebase > **Authentication**
2. Onglet **Sign-in method**
3. Activez les méthodes souhaitées (ex: Email/Password)

### API REST pour l'authentification

Firebase fournit une API REST pour l'authentification :

> 💡 **L'`API_KEY` Firebase côté client n'est PAS un secret.** Contrairement à une *Server Key* ou un *Service Account JSON* (qui DOIVENT rester côté serveur), l'API Key publique Firebase peut être embarquée dans l'application. Elle sert uniquement à identifier le projet — la sécurité repose sur les **règles de sécurité** Firestore/RTDB/Storage et les **restrictions d'API** que vous configurez dans la console Google Cloud (restrictions par bundle ID, SHA-1 Android, etc.). C'est pour cette raison que le même fichier `google-services.json` ou `GoogleService-Info.plist` est livré tel quel dans l'APK / IPA, sans crainte.

```pascal
// Inscription avec email/mot de passe
procedure TFormMain.InscrireUtilisateur(const Email, MotDePasse: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest, JSONResponse: TJSONObject;
  URL: string;
  APIKey: string;
begin
  APIKey := 'VOTRE_API_KEY_FIREBASE'; // Dans Project Settings

  HttpClient := THTTPClient.Create;
  try
    URL := Format(
      'https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=%s',
      [APIKey]);

    JSONRequest := TJSONObject.Create;
    try
      JSONRequest.AddPair('email', Email);
      JSONRequest.AddPair('password', MotDePasse);
      JSONRequest.AddPair('returnSecureToken', TJSONBool.Create(True));

      Response := HttpClient.Post(URL,
        TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8));

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(
          Response.ContentAsString) as TJSONObject;
        try
          var IDToken := JSONResponse.GetValue<string>('idToken');
          var UserID := JSONResponse.GetValue<string>('localId');

          // Sauvegarder le token
          SauvegarderToken(IDToken);

          ShowMessage('Inscription réussie !');
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        ShowMessage('Erreur d''inscription : ' + Response.ContentAsString);
      end;
    finally
      JSONRequest.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Connexion avec email/mot de passe
procedure TFormMain.ConnecterUtilisateur(const Email, MotDePasse: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest, JSONResponse: TJSONObject;
  URL: string;
  APIKey: string;
begin
  APIKey := 'VOTRE_API_KEY_FIREBASE';

  HttpClient := THTTPClient.Create;
  try
    URL := Format(
      'https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=%s',
      [APIKey]);

    JSONRequest := TJSONObject.Create;
    try
      JSONRequest.AddPair('email', Email);
      JSONRequest.AddPair('password', MotDePasse);
      JSONRequest.AddPair('returnSecureToken', TJSONBool.Create(True));

      Response := HttpClient.Post(URL,
        TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8));

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(
          Response.ContentAsString) as TJSONObject;
        try
          var IDToken := JSONResponse.GetValue<string>('idToken');
          var UserID := JSONResponse.GetValue<string>('localId');
          var UserEmail := JSONResponse.GetValue<string>('email');

          SauvegarderToken(IDToken);
          SauvegarderUserID(UserID);

          ShowMessage('Connexion réussie !');
          AfficherEcranPrincipal;
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        ShowMessage('Identifiants incorrects');
      end;
    finally
      JSONRequest.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Réinitialisation du mot de passe
procedure TFormMain.ReinitialiserMotDePasse(const Email: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONRequest: TJSONObject;
  URL: string;
  APIKey: string;
begin
  APIKey := 'VOTRE_API_KEY_FIREBASE';

  HttpClient := THTTPClient.Create;
  try
    URL := Format(
      'https://identitytoolkit.googleapis.com/v1/accounts:sendOobCode?key=%s',
      [APIKey]);

    JSONRequest := TJSONObject.Create;
    try
      JSONRequest.AddPair('requestType', 'PASSWORD_RESET');
      JSONRequest.AddPair('email', Email);

      Response := HttpClient.Post(URL,
        TStringStream.Create(JSONRequest.ToString, TEncoding.UTF8));

      if Response.StatusCode = 200 then
        ShowMessage('Email de réinitialisation envoyé à ' + Email)
      else
        ShowMessage('Erreur : ' + Response.ContentAsString);
    finally
      JSONRequest.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Firebase Cloud Storage

Firebase Storage permet de stocker des fichiers (images, vidéos, documents).

### Configuration des règles

```javascript
rules_version = '2';  
service firebase.storage {  
  match /b/{bucket}/o {
    match /users/{userId}/{allPaths=**} {
      allow read, write: if request.auth != null && request.auth.uid == userId;
    }
    match /public/{allPaths=**} {
      allow read: if true;
      allow write: if request.auth != null;
    }
  }
}
```

### Upload de fichier

> 💡 **API Firebase Storage REST.** L'endpoint d'upload binaire simple est `/v0/b/{bucket}/o?name={chemin}&uploadType=media`. Pour des métadonnées riches (Content-Disposition, *custom metadata*), on utilise plutôt `uploadType=multipart` avec un corps `multipart/related`. La requête doit être authentifiée par un token Firebase Auth (voir section Authentication).

```pascal
uses
  System.Net.HttpClient, System.Net.URLClient, System.IOUtils, System.NetEncoding;

// Uploader une image vers Firebase Storage (uploadType=media)
procedure TFormMain.UploaderImage(const CheminLocal, AuthToken: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
  URL, NomFichier: string;
  Headers: TNetHeaders;
begin
  if not TFile.Exists(CheminLocal) then
  begin
    ShowMessage('Fichier introuvable');
    Exit;
  end;

  HttpClient := THTTPClient.Create;
  FileStream := TFileStream.Create(CheminLocal, fmOpenRead);
  try
    NomFichier := TPath.GetFileName(CheminLocal);

    // L'encodage des / dans le path se fait via TNetEncoding.URL
    URL := Format(
      'https://firebasestorage.googleapis.com/v0/b/votre-projet.appspot.com/o?' +
      'uploadType=media&name=%s',
      [TNetEncoding.URL.Encode('images/' + NomFichier)]);

    // En-têtes : type MIME + token Firebase Auth
    Headers := [
      TNetHeader.Create('Content-Type',  'image/jpeg'),
      TNetHeader.Create('Authorization', 'Firebase ' + AuthToken)
    ];

    Response := HttpClient.Post(URL, FileStream, nil, Headers);

    if Response.StatusCode in [200, 201] then
    begin
      var JSONResponse := TJSONObject.ParseJSONValue(
        Response.ContentAsString) as TJSONObject;
      try
        // ⚠ `downloadTokens` est le token, pas l'URL finale.
        // L'URL complète est de la forme :
        //   https://firebasestorage.googleapis.com/v0/b/<bucket>/o/<path>
        //     ?alt=media&token=<downloadTokens>
        var DownloadToken := JSONResponse.GetValue<string>('downloadTokens');
        ShowMessage('Image uploadée avec succès !');
        Memo1.Lines.Add('Token de téléchargement : ' + DownloadToken);
      finally
        JSONResponse.Free;
      end;
    end
    else
      ShowMessage('Erreur d''upload : ' + Response.StatusCode.ToString);
  finally
    FileStream.Free;
    HttpClient.Free;
  end;
end;

// Télécharger une image depuis Firebase Storage
procedure TFormMain.TelevergerImage(const NomFichier, CheminDestination: string);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    URL := Format(
      'https://firebasestorage.googleapis.com/v0/b/votre-projet.appspot.com/o/images%%2F%s?alt=media',
      [NomFichier]);

    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      FileStream := TFileStream.Create(CheminDestination, fmCreate);
      try
        FileStream.CopyFrom(Response.ContentStream, 0);
        ShowMessage('Image téléchargée avec succès !');
      finally
        FileStream.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

## Firebase Crashlytics

Crashlytics rapporte automatiquement les crashs de votre application.

### Configuration

1. Console Firebase > **Crashlytics**
2. Suivez les instructions d'intégration
3. Ajoutez le SDK Crashlytics à votre projet Delphi

### Logger des informations personnalisées

```pascal
// Logger des événements personnalisés avant un crash
procedure TFormMain.LoggerEvenementCrashlytics(const Message: string);  
begin  
  // Note : Nécessite l'intégration native du SDK Crashlytics
  // Exemple conceptuel

  // Crashlytics.log(Message);
end;

// Définir des clés personnalisées
procedure TFormMain.DefinirClesPersonnalisees;  
begin  
  // Crashlytics.setCustomKey('user_id', GetUserID);
  // Crashlytics.setCustomKey('screen', 'MainScreen');
end;

// Logger une exception non fatale
procedure TFormMain.LoggerException(E: Exception);  
begin  
  // Crashlytics.recordError(E);
end;
```

## Bonnes pratiques

### 1. Gestion des quotas

Firebase offre un plan gratuit généreux, mais avec des limites strictes au-delà desquelles le service est suspendu (plan Spark) ou facturé (plan Blaze, *pay-as-you-go*).

**Limites principales du plan Spark (gratuit) en 2026** (vérifier les valeurs exactes sur [firebase.google.com/pricing](https://firebase.google.com/pricing)) :

| Service | Limite |
|---|---|
| **Realtime Database** | 1 Go de stockage, 10 Go/mois en téléchargement, 100 connexions simultanées |
| **Cloud Firestore** | 1 Gio de stockage, 50 000 lectures/jour, 20 000 écritures/jour, 20 000 suppressions/jour |
| **Cloud Storage** | 5 Go stockés, 1 Go/jour en téléchargement, 20 000 opérations d'upload/jour |
| **Authentication (téléphone)** | 10 000 vérifications/mois |
| **Cloud Messaging (FCM)** | **Aucune limite** (gratuit et illimité, même sur Blaze) |
| **Analytics** | **Aucune limite** sur le nombre d'événements |

```pascal
// Optimiser les requêtes pour éviter d'exploser les quotas
procedure TFormMain.OptimiserRequetes;  
begin  
  // ✅ BON : Charger seulement ce qui est nécessaire
  ChargerDonnees('users/user1');

  // ❌ MAUVAIS : Charger toute la base
  // ChargerDonnees('');

  // ✅ MIEUX : Utiliser le cache local (voir section suivante)
  //           et ne re-télécharger que les changements
end;
```

### 2. Sécurité des règles

```json
// ❌ MAUVAIS : Tout le monde peut tout lire/écrire
{
  "rules": {
    ".read": true,
    ".write": true
  }
}

// ✅ BON : Seulement les utilisateurs authentifiés
{
  "rules": {
    ".read": "auth != null",
    ".write": "auth != null",
    "users": {
      "$uid": {
        ".write": "$uid === auth.uid"
      }
    }
  }
}
```

### 3. Gestion des erreurs

> 💡 Une requête HTTP qui renvoie un code 4xx ou 5xx ne lève **pas** d'exception : `THTTPClient` retourne simplement une `IHTTPResponse` avec `StatusCode` non-200. Pour réagir aux erreurs HTTP, on inspecte `Response.StatusCode` ; pour réagir aux erreurs de transport (DNS, TLS, timeout, pas de réseau), on capture `ENetException` et `ENetHTTPClientException`.

```pascal
procedure TFormMain.RequeteFirebaseSafe;  
var  
  Response: IHTTPResponse;
begin
  try
    Response := ExecuterRequeteFirebase;  // retourne la réponse HTTP

    case Response.StatusCode of
      200, 201, 204: TraiterReponse(Response);
      401:           ShowMessage('Non authentifié — token expiré ?');
      403:           ShowMessage('Accès refusé par les règles Firebase');
      404:           ShowMessage('Donnée introuvable');
      429:           ShowMessage('Quota Firebase dépassé, réessayer plus tard');
      500..599:      ShowMessage('Erreur côté serveur Firebase');
    else
      ShowMessage('Code HTTP inattendu : ' + Response.StatusCode.ToString);
    end;
  except
    // Erreurs de transport : pas de réseau, DNS, certificat, timeout…
    on E: ENetException do
      ShowMessage('Problème réseau : ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur inattendue : ' + E.Message);
  end;
end;
```

### 4. Cache et mode hors ligne

> ⚠ **Cycle de vie des `TJSONValue` en cache.** Un `TJSONValue` est un objet — il faut décider qui le possède. La règle simple : si le cache stocke un `TJSONValue`, c'est lui qui est responsable de le `Free` (dans son destructeur et lors d'un remplacement). Pour éviter les doubles libérations, on **clone** la valeur à l'ajout au cache et à la lecture, ce qui découple le cycle de vie côté appelant.

```pascal
type
  TFirebaseCache = class
  private
    FCache: TDictionary<string, TJSONValue>;
  public
    constructor Create;
    destructor Destroy; override;

    // Le cache prend une copie (Clone) pour rester indépendant
    procedure AjouterAuCache(const Cle: string; Valeur: TJSONValue);
    // Retourne un Clone — l'appelant doit le libérer (ou nil si absent)
    function ObtenirDuCache(const Cle: string): TJSONValue;
    function EstEnCache(const Cle: string): Boolean;
    procedure Vider;
  end;

destructor TFirebaseCache.Destroy;  
begin  
  Vider;
  FCache.Free;
  inherited;
end;

procedure TFirebaseCache.AjouterAuCache(const Cle: string; Valeur: TJSONValue);  
var  
  Ancien: TJSONValue;
begin
  // Libérer l'éventuelle valeur précédente avant de remplacer
  if FCache.TryGetValue(Cle, Ancien) then
    Ancien.Free;

  FCache.AddOrSetValue(Cle, Valeur.Clone as TJSONValue);
end;

function TFirebaseCache.ObtenirDuCache(const Cle: string): TJSONValue;  
var  
  Brut: TJSONValue;
begin
  if FCache.TryGetValue(Cle, Brut) then
    Result := Brut.Clone as TJSONValue
  else
    Result := nil;
end;

// Utilisation
procedure TFormMain.ChargerAvecCache(const Path: string);  
var  
  Donnees: TJSONValue;
begin
  if Cache.EstEnCache(Path) then
  begin
    Donnees := Cache.ObtenirDuCache(Path);
    try
      AfficherDonnees(Donnees);
    finally
      Donnees.Free;  // libération du clone retourné
    end;
  end
  else
  begin
    ChargerDepuisFirebase(Path,
      procedure(Donnees: TJSONValue)
      begin
        Cache.AjouterAuCache(Path, Donnees);  // le cache clone en interne
        AfficherDonnees(Donnees);
      end);
  end;
end;
```

### 5. Monitoring et analytics

```pascal
// Logger toutes les interactions Firebase importantes
procedure TFormMain.LoggerInteractionFirebase(const Action, Resource: string);  
begin  
  LoggerEvenementAvecParams('firebase_interaction',
    ['action', Action, 'resource', Resource]);
end;

// Utilisation
procedure TFormMain.SauvegarderDonnees;  
begin  
  LoggerInteractionFirebase('write', 'users/user1');
  EcrireDonneesFirebase;
end;
```

## Dépannage des problèmes courants

### Le token FCM ne s'affiche pas

```pascal
// Vérifier la configuration
procedure TFormMain.VerifierConfigurationFCM;  
begin  
  if not Assigned(PushService) then
  begin
    ShowMessage('PushService non disponible');
    Exit;
  end;

  if not PushService.Active then
  begin
    ShowMessage('PushService non activé');
    PushService.Active := True;
  end;

  // Vérifier les permissions
  {$IFDEF ANDROID}
  if not PermissionsService.IsPermissionGranted(
    'android.permission.POST_NOTIFICATIONS') then
  begin
    ShowMessage('Permission notifications manquante');
  end;
  {$ENDIF}
end;
```

### Erreur d'authentification Firebase

> ⚠ **Où se trouve `google-services.json` à l'exécution ?** Si vous l'avez déployé via *Project > Deployment* dans `assets\internal\`, son chemin sur l'appareil Android est obtenu par `TPath.GetHomePath` (pas `GetDocumentsPath`). Sur iOS, `GoogleService-Info.plist` est dans le bundle de l'application — accessible via `TPath.GetHomePath` ou les API natives `NSBundle`. La vérification doit cibler le bon répertoire selon votre configuration Deployment.

```pascal
// Vérifier que le fichier de configuration Firebase est bien déployé.
// ⚠ Le chemin dépend du « Remote Path » configuré dans Project >
//   Deployment. Avec le réglage par défaut (assets/internal/ sur
//   Android, StartUp\Documents\ sur iOS), c'est GetHomePath et non
//   GetDocumentsPath qu'il faut interroger.
procedure TFormMain.VerifierConfiguration;  
var  
  CheminConfig: string;
begin
  {$IFDEF ANDROID}
  CheminConfig := TPath.Combine(TPath.GetHomePath, 'google-services.json');
  {$ENDIF}
  {$IFDEF IOS}
  CheminConfig := TPath.Combine(TPath.GetHomePath, 'GoogleService-Info.plist');
  {$ENDIF}

  if (CheminConfig <> '') and not TFile.Exists(CheminConfig) then
    ShowMessage('Fichier de configuration Firebase manquant : ' + CheminConfig);
end;
```

## Conclusion

Firebase est un outil puissant qui peut considérablement accélérer le développement de vos applications mobiles Delphi. En éliminant le besoin de créer et gérer votre propre infrastructure backend, vous pouvez vous concentrer sur ce qui compte vraiment : créer une excellente expérience utilisateur.

**Points clés à retenir** :

1. **Configuration** : Créez un projet Firebase et intégrez les fichiers de configuration
2. **FCM** : Utilisez TPushService pour les notifications push
3. **Analytics** : Suivez le comportement des utilisateurs avec Firebase Analytics
4. **Realtime Database** : Synchronisez les données en temps réel via REST API
5. **Authentication** : Gérez l'authentification des utilisateurs simplement
6. **Storage** : Stockez des fichiers avec Firebase Cloud Storage
7. **Sécurité** : Configurez correctement les règles de sécurité
8. **Optimisation** : Utilisez le cache et surveillez les quotas

Firebase offre bien plus que ce que nous avons couvert ici : Remote Config, A/B Testing, Dynamic Links, App Indexing, etc. N'hésitez pas à explorer la documentation officielle pour découvrir toutes les possibilités.

En combinant la puissance de Delphi pour le développement rapide d'applications natives et les services cloud de Firebase, vous disposez d'une stack technologique moderne et efficace pour créer des applications mobiles de qualité professionnelle !

**Ressources utiles** :
- Documentation Firebase : https://firebase.google.com/docs
- Console Firebase : https://console.firebase.google.com
- Status Firebase : https://status.firebase.google.com
- Firebase Blog : https://firebase.googleblog.com

⏭️ [Sécurité des applications](/16-securite-des-applications/README.md)
