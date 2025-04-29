# 15.11 Intégration des services Firebase

Firebase est une plateforme de développement d'applications mobiles créée par Google qui offre de nombreux services pour améliorer vos applications. Delphi permet d'intégrer facilement Firebase dans vos projets pour Android et iOS. Dans cette section, nous explorerons comment configurer et utiliser les services Firebase les plus populaires dans vos applications mobiles Delphi.

## Qu'est-ce que Firebase ?

Firebase est un ensemble de services hébergés dans le cloud qui peuvent aider à :

- **Développer** plus rapidement avec des outils prêts à l'emploi
- **Améliorer la qualité** de votre application avec des services de test et de surveillance
- **Faire croître votre base d'utilisateurs** avec des outils d'analyse et d'engagement

Les services Firebase les plus couramment utilisés comprennent :

- **Authentication** : Système d'authentification complet
- **Cloud Firestore** : Base de données NoSQL en temps réel
- **Realtime Database** : Base de données JSON en temps réel
- **Cloud Storage** : Stockage de fichiers
- **Cloud Messaging** : Notifications push
- **Analytics** : Analyse du comportement des utilisateurs
- **Crashlytics** : Rapports de crash

## Configuration de Firebase avec Delphi

### Étape 1 : Créer un projet Firebase

Avant d'intégrer Firebase dans votre application Delphi, vous devez créer un projet Firebase :

1. Rendez-vous sur la [console Firebase](https://console.firebase.google.com/)
2. Cliquez sur "Ajouter un projet"
3. Donnez un nom à votre projet et suivez les étapes de configuration
4. Une fois le projet créé, vous serez redirigé vers le tableau de bord du projet

### Étape 2 : Ajouter des applications à votre projet Firebase

Pour chaque plateforme (Android et iOS), vous devez enregistrer votre application dans Firebase :

#### Pour Android :

1. Dans le tableau de bord de votre projet Firebase, cliquez sur l'icône Android
2. Entrez le nom du package de votre application (ex : `com.votreentreprise.votreapp`)
   - Pour trouver le nom du package dans Delphi : Project > Options > Application > Package Name
3. (Optionnel) Entrez un surnom pour l'application
4. Téléchargez le fichier `google-services.json`

#### Pour iOS :

1. Dans le tableau de bord de votre projet Firebase, cliquez sur l'icône iOS
2. Entrez l'ID du bundle (ex : `com.votreentreprise.votreapp`)
   - Pour trouver l'ID du bundle dans Delphi : Project > Options > Application > Bundle Identifier
3. (Optionnel) Entrez un surnom pour l'application
4. Téléchargez le fichier `GoogleService-Info.plist`

### Étape 3 : Installation des composants Firebase dans Delphi

Delphi propose des composants natifs pour Firebase. Pour les installer :

1. Ouvrez Delphi et sélectionnez Tools > GetIt Package Manager
2. Recherchez "Firebase" dans la barre de recherche
3. Installez les packages Firebase qui vous intéressent

> **Note** : Certains services Firebase peuvent nécessiter des composants supplémentaires ou des bibliothèques tierces.

### Étape 4 : Configuration de votre projet Delphi

Après avoir installé les composants, vous devez configurer votre projet pour utiliser Firebase :

1. Placez le fichier `google-services.json` dans le dossier `<Projet>\Android\Debug` et `<Projet>\Android\Release`
2. Placez le fichier `GoogleService-Info.plist` dans le dossier `<Projet>\iOS\Debug` et `<Projet>\iOS\Release`

3. Ajoutez les permissions nécessaires dans les options de votre projet :
   - Pour Android : Project > Options > Uses Permissions
   - Pour iOS : Project > Options > Version Info > Custom plist

## Firebase Authentication

Firebase Authentication permet de gérer l'authentification des utilisateurs avec différentes méthodes (email/mot de passe, réseaux sociaux, etc.) de manière sécurisée.

### Configuration de Authentication dans Firebase

1. Dans la console Firebase, allez dans "Authentication" > "Commencer"
2. Activez les méthodes d'authentification que vous souhaitez utiliser (Email/Mot de passe, Google, Facebook, etc.)

### Intégration de Authentication dans Delphi

#### 1. Ajout des composants nécessaires

Placez ces composants sur votre formulaire depuis la palette :

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseAuth FAuth;
```

#### 2. Initialisation de Firebase dans votre application

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Auth
  FAuth := TFirebaseAuth.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FAuth.Free;
end;
```

#### 3. Implémentation de l'inscription par email et mot de passe

```pascal
procedure TMainForm.btnSignUpClick(Sender: TObject);
var
  Email, Password: string;
begin
  // Récupérer les informations depuis l'interface
  Email := edtEmail.Text;
  Password := edtPassword.Text;

  // Vérifier les entrées
  if (Email = '') or (Password = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs.');
    Exit;
  end;

  // Afficher un indicateur de chargement
  ShowLoadingIndicator;

  // Créer un nouvel utilisateur
  FAuth.CreateUserWithEmailAndPassword(Email, Password,
    procedure(const AuthResult: IFirebaseUser; const Error: string)
    begin
      // Masquer l'indicateur de chargement
      HideLoadingIndicator;

      // Vérifier s'il y a une erreur
      if Error <> '' then
      begin
        ShowMessage('Erreur d''inscription : ' + Error);
        Exit;
      end;

      // Succès, afficher un message
      ShowMessage('Inscription réussie !');

      // Naviguer vers l'écran suivant
      NavigateToHome;
    end
  );
end;
```

#### 4. Implémentation de la connexion par email et mot de passe

```pascal
procedure TMainForm.btnLoginClick(Sender: TObject);
var
  Email, Password: string;
begin
  // Récupérer les informations depuis l'interface
  Email := edtEmail.Text;
  Password := edtPassword.Text;

  // Vérifier les entrées
  if (Email = '') or (Password = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs.');
    Exit;
  end;

  // Afficher un indicateur de chargement
  ShowLoadingIndicator;

  // Se connecter
  FAuth.SignInWithEmailAndPassword(Email, Password,
    procedure(const AuthResult: IFirebaseUser; const Error: string)
    begin
      // Masquer l'indicateur de chargement
      HideLoadingIndicator;

      // Vérifier s'il y a une erreur
      if Error <> '' then
      begin
        ShowMessage('Erreur de connexion : ' + Error);
        Exit;
      end;

      // Succès, afficher un message
      ShowMessage('Connexion réussie !');

      // Naviguer vers l'écran suivant
      NavigateToHome;
    end
  );
end;
```

#### 5. Implémentation de l'authentification Google

```pascal
procedure TMainForm.btnGoogleSignInClick(Sender: TObject);
begin
  // Afficher un indicateur de chargement
  ShowLoadingIndicator;

  // Se connecter avec Google
  FAuth.SignInWithGoogle(
    procedure(const AuthResult: IFirebaseUser; const Error: string)
    begin
      // Masquer l'indicateur de chargement
      HideLoadingIndicator;

      // Vérifier s'il y a une erreur
      if Error <> '' then
      begin
        ShowMessage('Erreur de connexion Google : ' + Error);
        Exit;
      end;

      // Succès, afficher un message
      ShowMessage('Connexion Google réussie !');

      // Naviguer vers l'écran suivant
      NavigateToHome;
    end
  );
end;
```

#### 6. Déconnexion

```pascal
procedure TMainForm.btnLogoutClick(Sender: TObject);
begin
  // Déconnecter l'utilisateur
  FAuth.SignOut;

  // Naviguer vers l'écran de connexion
  NavigateToLogin;
end;
```

#### 7. Vérification de l'état de connexion

```pascal
procedure TMainForm.CheckAuthState;
var
  CurrentUser: IFirebaseUser;
begin
  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;

  // Vérifier si un utilisateur est connecté
  if CurrentUser <> nil then
  begin
    // Utilisateur connecté
    lblStatus.Text := 'Connecté en tant que : ' + CurrentUser.Email;
    btnLogout.Visible := True;
    btnLogin.Visible := False;
  end
  else
  begin
    // Utilisateur non connecté
    lblStatus.Text := 'Non connecté';
    btnLogout.Visible := False;
    btnLogin.Visible := True;
  end;
end;
```

## Cloud Firestore

Cloud Firestore est une base de données NoSQL flexible et évolutive qui permet de stocker et de synchroniser des données entre utilisateurs et appareils.

### Configuration de Firestore dans Firebase

1. Dans la console Firebase, allez dans "Firestore Database" > "Créer une base de données"
2. Choisissez le mode de démarrage (test ou production)
3. Sélectionnez l'emplacement de votre base de données

### Intégration de Firestore dans Delphi

#### 1. Ajout des composants nécessaires

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseFirestore FFirestore;
```

#### 2. Initialisation de Firestore

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Firestore
  FFirestore := TFirebaseFirestore.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FFirestore.Free;
end;
```

#### 3. Ajout de données dans Firestore

```pascal
procedure TMainForm.AddTaskToFirestore(const Title, Description: string; DueDate: TDateTime);
var
  Collection: IFirestoreCollection;
  Document: IFirestoreDocument;
  TaskData: TJSONObject;
begin
  // Créer un objet JSON avec les données de la tâche
  TaskData := TJSONObject.Create;
  try
    TaskData.AddPair('title', Title);
    TaskData.AddPair('description', Description);
    TaskData.AddPair('dueDate', DateToISO8601(DueDate));
    TaskData.AddPair('completed', TJSONBool.Create(False));
    TaskData.AddPair('createdAt', DateToISO8601(Now));

    // Obtenir la référence à la collection "tasks"
    Collection := FFirestore.Collection('tasks');

    // Ajouter un nouveau document
    Collection.Add(TaskData,
      procedure(const Document: IFirestoreDocument; const Error: string)
      begin
        if Error <> '' then
        begin
          ShowMessage('Erreur lors de l''ajout de la tâche : ' + Error);
          Exit;
        end;

        ShowMessage('Tâche ajoutée avec succès !');

        // Actualiser la liste des tâches
        LoadTasks;
      end
    );
  finally
    TaskData.Free;
  end;
end;
```

#### 4. Lecture de données depuis Firestore

```pascal
procedure TMainForm.LoadTasks;
var
  Collection: IFirestoreCollection;
  Query: IFirestoreQuery;
begin
  // Afficher un indicateur de chargement
  ShowLoadingIndicator;

  // Obtenir la référence à la collection "tasks"
  Collection := FFirestore.Collection('tasks');

  // Créer une requête pour récupérer les tâches
  Query := Collection.OrderBy('dueDate', ftAscending);

  // Exécuter la requête
  Query.GetDocuments(
    procedure(const Documents: TArray<IFirestoreDocument>; const Error: string)
    begin
      // Masquer l'indicateur de chargement
      HideLoadingIndicator;

      if Error <> '' then
      begin
        ShowMessage('Erreur lors du chargement des tâches : ' + Error);
        Exit;
      end;

      // Effacer la liste actuelle
      lvTasks.Items.Clear;

      // Ajouter chaque tâche à la liste
      for var Doc in Documents do
      begin
        var Item := lvTasks.Items.Add;

        // Récupérer les données du document
        var Data := Doc.Data;

        // Configurer l'élément de la liste
        Item.Text := Data.GetValue<string>('title');
        Item.Detail := Data.GetValue<string>('description');

        // Stocker l'ID du document pour référence future
        Item.Tag := Integer(Doc.Id);

        // Afficher un indicateur si la tâche est terminée
        if Data.GetValue<Boolean>('completed') then
          Item.Accessory := TListBoxItemAccessory.aCheckmark
        else
          Item.Accessory := TListBoxItemAccessory.aNone;
      end;

      // Afficher un message si aucune tâche n'est trouvée
      if Length(Documents) = 0 then
        ShowMessage('Aucune tâche trouvée');
    end
  );
end;
```

#### 5. Mise à jour de données dans Firestore

```pascal
procedure TMainForm.UpdateTaskStatus(const DocumentId: string; Completed: Boolean);
var
  Document: IFirestoreDocument;
  UpdateData: TJSONObject;
begin
  // Obtenir la référence au document
  Document := FFirestore.Collection('tasks').Document(DocumentId);

  // Créer les données à mettre à jour
  UpdateData := TJSONObject.Create;
  try
    UpdateData.AddPair('completed', TJSONBool.Create(Completed));
    UpdateData.AddPair('updatedAt', DateToISO8601(Now));

    // Mettre à jour le document
    Document.Update(UpdateData,
      procedure(const Error: string)
      begin
        if Error <> '' then
        begin
          ShowMessage('Erreur lors de la mise à jour de la tâche : ' + Error);
          Exit;
        end;

        ShowMessage('Tâche mise à jour avec succès !');

        // Actualiser la liste des tâches
        LoadTasks;
      end
    );
  finally
    UpdateData.Free;
  end;
end;
```

#### 6. Suppression de données dans Firestore

```pascal
procedure TMainForm.DeleteTask(const DocumentId: string);
var
  Document: IFirestoreDocument;
begin
  // Obtenir la référence au document
  Document := FFirestore.Collection('tasks').Document(DocumentId);

  // Supprimer le document
  Document.Delete(
    procedure(const Error: string)
    begin
      if Error <> '' then
      begin
        ShowMessage('Erreur lors de la suppression de la tâche : ' + Error);
        Exit;
      end;

      ShowMessage('Tâche supprimée avec succès !');

      // Actualiser la liste des tâches
      LoadTasks;
    end
  );
end;
```

#### 7. Écoute des changements en temps réel

```pascal
procedure TMainForm.StartTasksListener;
var
  Collection: IFirestoreCollection;
  Query: IFirestoreQuery;
  ListenerRegistration: IFirestoreListenerRegistration;
begin
  // Obtenir la référence à la collection "tasks"
  Collection := FFirestore.Collection('tasks');

  // Créer une requête pour écouter les tâches
  Query := Collection.OrderBy('dueDate', ftAscending);

  // Écouter les changements
  ListenerRegistration := Query.AddSnapshotListener(
    procedure(const Documents: TArray<IFirestoreDocument>; const Error: string)
    begin
      if Error <> '' then
      begin
        ShowMessage('Erreur d''écoute : ' + Error);
        Exit;
      end;

      // Mettre à jour l'interface utilisateur
      UpdateTasksList(Documents);
    end
  );

  // Stocker l'enregistrement d'écoute pour pouvoir l'arrêter plus tard
  FListenerRegistration := ListenerRegistration;
end;

procedure TMainForm.StopTasksListener;
begin
  // Arrêter l'écoute
  if FListenerRegistration <> nil then
  begin
    FListenerRegistration.Remove;
    FListenerRegistration := nil;
  end;
end;
```

## Cloud Storage

Firebase Storage permet de stocker et de récupérer facilement des fichiers générés par les utilisateurs, tels que des images, des fichiers audio et des vidéos.

### Configuration de Storage dans Firebase

1. Dans la console Firebase, allez dans "Storage" > "Commencer"
2. Choisissez le mode de sécurité (test ou production)
3. Sélectionnez l'emplacement de votre bucket

### Intégration de Storage dans Delphi

#### 1. Ajout des composants nécessaires

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseStorage FStorage;
```

#### 2. Initialisation de Storage

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Storage
  FStorage := TFirebaseStorage.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FStorage.Free;
end;
```

#### 3. Téléchargement d'une image vers Storage

```pascal
procedure TMainForm.UploadImage(const LocalFilePath: string);
var
  StorageReference: IStorageReference;
  FileName: string;
begin
  // Générer un nom de fichier unique
  FileName := 'images/' + FormatDateTime('yyyymmddhhnnss', Now) + '.jpg';

  // Obtenir une référence au fichier dans Storage
  StorageReference := FStorage.Reference.Child(FileName);

  // Afficher un indicateur de progression
  progressBar.Visible := True;

  // Télécharger le fichier
  StorageReference.PutFile(LocalFilePath,
    // Callback de progression
    procedure(const Progress: Cardinal; const Total: Cardinal)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Mettre à jour la barre de progression
          if Total > 0 then
            progressBar.Value := (Progress / Total) * 100;
        end
      );
    end,
    // Callback de fin
    procedure(const MetaData: IStorageMetadata; const Error: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Masquer la barre de progression
          progressBar.Visible := False;

          if Error <> '' then
          begin
            ShowMessage('Erreur de téléchargement : ' + Error);
            Exit;
          end;

          // Succès
          ShowMessage('Image téléchargée avec succès !');

          // Obtenir l'URL de téléchargement
          StorageReference.GetDownloadURL(
            procedure(const URL: string; const Error: string)
            begin
              if Error = '' then
              begin
                // Stocker l'URL dans Firestore ou l'afficher
                edtImageURL.Text := URL;
              end;
            end
          );
        end
      );
    end
  );
end;
```

#### 4. Téléchargement d'une image depuis Storage

```pascal
procedure TMainForm.DownloadImage(const StoragePath: string; const ImageControl: TImage);
var
  StorageReference: IStorageReference;
  LocalFilePath: string;
begin
  // Obtenir une référence au fichier dans Storage
  StorageReference := FStorage.Reference.Child(StoragePath);

  // Créer un chemin local temporaire
  LocalFilePath := TPath.Combine(TPath.GetTempPath, TPath.GetFileName(StoragePath));

  // Afficher un indicateur de progression
  progressBar.Visible := True;

  // Télécharger le fichier
  StorageReference.GetFile(LocalFilePath,
    // Callback de progression
    procedure(const Progress: Cardinal; const Total: Cardinal)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Mettre à jour la barre de progression
          if Total > 0 then
            progressBar.Value := (Progress / Total) * 100;
        end
      );
    end,
    // Callback de fin
    procedure(const Error: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Masquer la barre de progression
          progressBar.Visible := False;

          if Error <> '' then
          begin
            ShowMessage('Erreur de téléchargement : ' + Error);
            Exit;
          end;

          // Charger l'image dans le contrôle
          try
            ImageControl.Bitmap.LoadFromFile(LocalFilePath);
            ShowMessage('Image téléchargée avec succès !');
          except
            on E: Exception do
              ShowMessage('Erreur lors du chargement de l''image : ' + E.Message);
          end;
        end
      );
    end
  );
end;
```

## Cloud Messaging (FCM)

Firebase Cloud Messaging (FCM) permet d'envoyer des notifications push à vos utilisateurs sur différentes plateformes.

### Configuration de FCM dans Firebase

1. Dans la console Firebase, allez dans "Cloud Messaging" > "Commencer"
2. (Pour iOS uniquement) Téléchargez votre certificat APNs

### Intégration de FCM dans Delphi

#### 1. Ajout des composants nécessaires

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseMessaging FMessaging;
```

#### 2. Initialisation de FCM

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Messaging
  FMessaging := TFirebaseMessaging.Create;

  // S'abonner aux événements de message
  FMessaging.OnTokenReceived := HandleTokenReceived;
  FMessaging.OnMessageReceived := HandleMessageReceived;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FMessaging.Free;
end;
```

#### 3. Gestion du token FCM

```pascal
procedure TMainForm.HandleTokenReceived(Sender: TObject; const Token: string);
begin
  // Sauvegarder le token localement
  TPreferencesService.Current.SetValue('FCMToken', Token);

  // Afficher le token pour le débogage
  Log('FCM Token: ' + Token);

  // Envoyer le token au serveur
  SendTokenToServer(Token);
end;

procedure TMainForm.SendTokenToServer(const Token: string);
begin
  // Implémenter l'envoi du token à votre serveur
  // Ceci est nécessaire pour que votre serveur puisse envoyer des notifications
  // à cet appareil spécifique

  // Exemple avec TRESTClient
  var Client := TRESTClient.Create('https://votre-serveur.com/api');
  try
    var Request := TRESTRequest.Create(Client);
    try
      Request.Resource := 'devices';
      Request.Method := TRESTRequestMethod.rmPOST;

      var Params := TJSONObject.Create;
      try
        Params.AddPair('token', Token);
        Params.AddPair('platform', {$IFDEF ANDROID}'android'{$ELSE}'ios'{$ENDIF});
        Params.AddPair('user_id', GetCurrentUserId);

        Request.Body.JSONValue := Params;
        Request.Execute;
      finally
        Params.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

#### 4. Gestion des messages reçus

```pascal
procedure TMainForm.HandleMessageReceived(Sender: TObject; const RemoteMessage: IFirebaseMessage);
begin
  // Traiter le message reçu
  var Title := RemoteMessage.Notification.Title;
  var Body := RemoteMessage.Notification.Body;

  // Afficher une notification locale
  ShowLocalNotification(Title, Body);

  // Traiter les données du message
  if RemoteMessage.Data.Count > 0 then
  begin
    // Exemple: vérifier si le message concerne un nouveau chat
    if RemoteMessage.Data.ContainsKey('chat_id') then
    begin
      var ChatId := RemoteMessage.Data.Items['chat_id'];
      OpenChatScreen(ChatId);
    end;
  end;
end;

procedure TMainForm.ShowLocalNotification(const Title, Body: string);
var
  NotificationCenter: TNotificationCenter;
  Notification: TNotification;
begin
  NotificationCenter := TNotificationCenter.Create(nil);
  try
    Notification := NotificationCenter.CreateNotification;
    try
      Notification.Title := Title;
      Notification.AlertBody := Body;

      // Configurer d'autres propriétés de notification si nécessaire
      Notification.EnableSound := True;

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

#### 5. Abonnement à des sujets (topics)

```pascal
procedure TMainForm.SubscribeToTopic(const Topic: string);
begin
  FMessaging.Subscribe(Topic,
    procedure(const Success: Boolean; const Error: string)
    begin
      if Success then
        ShowMessage('Abonné au sujet : ' + Topic)
      else
        ShowMessage('Erreur d''abonnement : ' + Error);
    end
  );
end;

procedure TMainForm.UnsubscribeFromTopic(const Topic: string);
begin
  FMessaging.Unsubscribe(Topic,
    procedure(const Success: Boolean; const Error: string)
    begin
      if Success then
        ShowMessage('Désabonné du sujet : ' + Topic)
      else
        ShowMessage('Erreur de désabonnement : ' + Error);
    end
  );
end;
```

## Firebase Analytics

Firebase Analytics fournit des informations sur le comportement des utilisateurs dans votre application.

### Configuration d'Analytics dans Firebase

1. Dans la console Firebase, Analytics est généralement activé par défaut

### Intégration d'Analytics dans Delphi

#### 1. Ajout des composants nécessaires

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseAnalytics FAnalytics;
```

#### 2. Initialisation d'Analytics

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Analytics
  FAnalytics := TFirebaseAnalytics.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FAnalytics.Free;
end;
```

#### 3. Suivi des événements

```pascal
procedure TMainForm.LogViewItemEvent(const ItemName, ItemCategory: string);
begin
  // Créer un dictionnaire de paramètres
  var Params := TFirebaseAnalyticsParameters.Create;
  try
    // Ajouter des paramètres
    Params.Add(FirebaseAnalytics.ParameterItemName, ItemName);
    Params.Add(FirebaseAnalytics.ParameterItemCategory, ItemCategory);

    // Enregistrer l'événement
    FAnalytics.LogEvent(FirebaseAnalytics.EventViewItem, Params);
  finally
    Params.Free;
  end;
end;

procedure TMainForm.LogPurchaseEvent(const Currency: string; const Value: Double;
                                   const ItemID, ItemName: string);
begin
  var Params := TFirebaseAnalyticsParameters.Create;
  try
    Params.Add(FirebaseAnalytics.ParameterCurrency, Currency);
    Params.Add(FirebaseAnalytics.ParameterValue, Value);
    Params.Add(FirebaseAnalytics.ParameterItemID, ItemID);
    Params.Add(FirebaseAnalytics.ParameterItemName, ItemName);

    FAnalytics.LogEvent(FirebaseAnalytics.EventPurchase, Params);
  finally
    Params.Free;
  end;
end;
```

#### 4. Définition des propriétés utilisateur

```pascal
procedure TMainForm.SetUserProperties;
begin
  // Définir des propriétés utilisateur pour segmentation
  FAnalytics.SetUserProperty('user_type', 'premium');
  FAnalytics.SetUserProperty('subscription_level', 'gold');
  FAnalytics.SetUserProperty('favorite_category', 'sports');
end;
```

## Firebase Crashlytics

Crashlytics est un rapporteur de plantage léger et en temps réel qui vous aide à suivre, hiérarchiser et résoudre les problèmes de stabilité afin d'améliorer la qualité de votre application.

### Configuration de Crashlytics dans Firebase

1. Dans la console Firebase, allez dans "Crashlytics" > "Commencer"
2. Suivez les instructions pour configurer Crashlytics dans votre projet

### Intégration de Crashlytics dans Delphi

#### 1. Ajout des composants nécessaires

```pascal
// Dans le fichier .h de votre formulaire
private:
  TFirebaseCrashlytics FCrashlytics;
```

#### 2. Initialisation de Crashlytics

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  Firebase.Core.TFirebaseApp.Initialize;

  // Créer l'instance Crashlytics
  FCrashlytics := TFirebaseCrashlytics.Create;

  // Activer la collecte des rapports de plantage
  FCrashlytics.SetCrashlyticsCollectionEnabled(True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FCrashlytics.Free;
end;
```

#### 3. Journalisation des informations pour le débogage

```pascal
procedure TMainForm.LogCustomKey(const Key, Value: string);
begin
  // Ajouter une clé personnalisée qui apparaîtra dans les rapports de crash
  FCrashlytics.SetCustomKey(Key, Value);
end;

procedure TMainForm.LogInfo(const Message: string);
begin
  // Enregistrer un message qui apparaîtra dans les rapports de crash
  FCrashlytics.Log(Message);
end;

procedure TMainForm.SetUserIdentifier(const UserID: string);
begin
  // Définir un identifiant utilisateur pour les rapports de crash
  FCrashlytics.SetUserIdentifier(UserID);
end;
```

#### 4. Signalement d'une exception non fatale

```pascal
procedure TMainForm.ReportNonFatalException(const ErrorMessage: string; const Exception: Exception);
begin
  // Enregistrer une exception non fatale
  FCrashlytics.RecordException(Exception);

  // Journaliser des informations supplémentaires
  FCrashlytics.Log('Erreur non fatale : ' + ErrorMessage);
end;
```

#### 5. Test de Crashlytics

```pascal
procedure TMainForm.TestCrashlytics;
begin
  // ATTENTION : Cette méthode va intentionnellement planter l'application
  // N'utilisez ceci que pour tester la configuration de Crashlytics
  FCrashlytics.Crash;
end;

procedure TMainForm.btnTestCrashClick(Sender: TObject);
begin
  // Afficher une confirmation avant de planter
  if MessageDlg('Cette action va planter l''application pour tester Crashlytics. Continuer ?',
                TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
  begin
    // Journaliser des informations de test
    FCrashlytics.Log('Test de crash intentionnel');
    FCrashlytics.SetCustomKey('test_crash', 'true');

    // Déclencher un crash
    TestCrashlytics;
  end;
end;
```

#### 6. Gestion globale des exceptions

```pascal
procedure HandleGlobalException(Sender: TObject; E: Exception);
begin
  // Enregistrer l'exception dans Crashlytics
  if Assigned(MainForm) and Assigned(MainForm.FCrashlytics) then
  begin
    MainForm.FCrashlytics.RecordException(E);
    MainForm.FCrashlytics.Log('Exception globale : ' + E.Message);
  end;

  // Afficher un message d'erreur convivial
  ShowMessage('Une erreur s''est produite : ' + E.Message);
end;

initialization
  // Définir le gestionnaire d'exceptions global
  Application.OnException := HandleGlobalException;
```

## Intégration de plusieurs services Firebase

Dans une application réelle, vous utiliserez probablement plusieurs services Firebase ensemble. Voici un exemple d'intégration combinée :

### Exemple d'application de tâches avec Firebase

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.DateTimeCtrls, FMX.Layouts,
  Firebase.Core, Firebase.Auth, Firebase.Firestore, Firebase.Analytics, Firebase.Crashlytics;

type
  TTaskStatus = (tsOpen, tsInProgress, tsCompleted);

  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    btnAdd: TButton;
    btnRefresh: TButton;
    lblTitle: TLabel;
    ListView1: TListView;
    PanelAdd: TPanel;
    edtTitle: TEdit;
    edtDescription: TMemo;
    dtpDueDate: TDateEdit;
    cmbStatus: TComboBox;
    btnSave: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    FAuth: TFirebaseAuth;
    FFirestore: TFirebaseFirestore;
    FAnalytics: TFirebaseAnalytics;
    FCrashlytics: TFirebaseCrashlytics;
    FCurrentTaskId: string;
    FIsEditing: Boolean;

    procedure InitializeFirebase;
    procedure CheckAuthState;
    procedure LoadTasks;
    procedure AddTask;
    procedure UpdateTask;
    procedure DeleteTask(const TaskId: string);
    procedure ShowAddEditPanel(Show: Boolean);
    procedure LogTaskOperation(const Operation, TaskId, TaskTitle: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser Firebase
  InitializeFirebase;

  // Remplir le combobox des statuts
  cmbStatus.Items.Clear;
  cmbStatus.Items.Add('À faire');
  cmbStatus.Items.Add('En cours');
  cmbStatus.Items.Add('Terminé');
  cmbStatus.ItemIndex := 0;

  // Masquer le panneau d'ajout au démarrage
  ShowAddEditPanel(False);

  // Vérifier l'état d'authentification
  CheckAuthState;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FAuth.Free;
  FFirestore.Free;
  FAnalytics.Free;
  FCrashlytics.Free;
end;

procedure TMainForm.InitializeFirebase;
begin
  // Initialiser l'application Firebase
  TFirebaseApp.Initialize;

  // Créer les instances des services Firebase
  FAuth := TFirebaseAuth.Create;
  FFirestore := TFirebaseFirestore.Create;
  FAnalytics := TFirebaseAnalytics.Create;
  FCrashlytics := TFirebaseCrashlytics.Create;

  // Configurer Crashlytics
  FCrashlytics.SetCrashlyticsCollectionEnabled(True);

  // Configurer Analytics
  FAnalytics.SetAnalyticsCollectionEnabled(True);
  FAnalytics.LogEvent('app_open', nil);
end;

procedure TMainForm.CheckAuthState;
var
  CurrentUser: IFirebaseUser;
begin
  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;

  if CurrentUser <> nil then
  begin
    // Utilisateur connecté
    lblTitle.Text := 'Tâches de ' + CurrentUser.DisplayName;

    // Charger les tâches de l'utilisateur
    LoadTasks;

    // Définir l'ID utilisateur pour Crashlytics
    FCrashlytics.SetUserIdentifier(CurrentUser.UID);

    // Définir les propriétés utilisateur pour Analytics
    FAnalytics.SetUserProperty('account_type', 'registered');
  end
  else
  begin
    // Utilisateur non connecté - rediriger vers l'écran de connexion
    ShowLoginScreen;
  end;
end;

procedure TMainForm.LoadTasks;
var
  Collection: IFirestoreCollection;
  Query: IFirestoreQuery;
begin
  // Obtenir l'utilisateur actuel
  var CurrentUser := FAuth.CurrentUser;
  if CurrentUser = nil then
    Exit;

  // Référence à la collection des tâches de l'utilisateur
  Collection := FFirestore.Collection('users').Document(CurrentUser.UID).Collection('tasks');

  // Créer une requête pour obtenir les tâches triées par date d'échéance
  Query := Collection.OrderBy('dueDate', ftAscending);

  // Exécuter la requête
  Query.GetDocuments(
    procedure(const Documents: TArray<IFirestoreDocument>; const Error: string)
    begin
      if Error <> '' then
      begin
        ShowMessage('Erreur lors du chargement des tâches : ' + Error);
        FCrashlytics.Log('Erreur Firestore : ' + Error);
        Exit;
      end;

      // Effacer la liste actuelle
      ListView1.Items.Clear;

      // Ajouter chaque tâche à la liste
      for var Doc in Documents do
      begin
        var Item := ListView1.Items.Add;

        // Récupérer les données du document
        var Data := Doc.Data;

        // Configurer l'élément de la liste
        Item.Text := Data.GetValue<string>('title');
        Item.Detail := Data.GetValue<string>('description');

        // Stocker l'ID du document pour référence future
        Item.TagString := Doc.Id;

        // Configurer l'icône en fonction du statut
        var Status := Data.GetValue<Integer>('status');
        case Status of
          0: Item.ImageIndex := 0;  // À faire
          1: Item.ImageIndex := 1;  // En cours
          2: Item.ImageIndex := 2;  // Terminé
        end;
      end;

      // Analyser le chargement des tâches
      FAnalytics.LogEvent('tasks_loaded',
        TFirebaseAnalyticsParameters.Create.Add('count', ListView1.Items.Count));
    end
  );
end;

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  // Préparer pour l'ajout d'une nouvelle tâche
  FIsEditing := False;
  FCurrentTaskId := '';

  // Effacer les champs
  edtTitle.Text := '';
  edtDescription.Text := '';
  dtpDueDate.Date := Date + 1;  // Date d'échéance par défaut = demain
  cmbStatus.ItemIndex := 0;     // Statut par défaut = À faire

  // Afficher le panneau d'ajout
  ShowAddEditPanel(True);

  // Mettre le focus sur le champ du titre
  edtTitle.SetFocus;

  // Analyser l'ajout de tâche
  FAnalytics.LogEvent('add_task_started', nil);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  // Valider les entrées
  if edtTitle.Text.Trim = '' then
  begin
    ShowMessage('Veuillez entrer un titre pour la tâche');
    edtTitle.SetFocus;
    Exit;
  end;

  // Ajouter ou mettre à jour la tâche
  if FIsEditing then
    UpdateTask
  else
    AddTask;

  // Masquer le panneau d'ajout
  ShowAddEditPanel(False);
end;

procedure TMainForm.AddTask;
var
  Collection: IFirestoreCollection;
  TaskData: TJSONObject;
  CurrentUser: IFirebaseUser;
begin
  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;
  if CurrentUser = nil then
    Exit;

  // Préparer les données de la tâche
  TaskData := TJSONObject.Create;
  try
    TaskData.AddPair('title', edtTitle.Text);
    TaskData.AddPair('description', edtDescription.Text);
    TaskData.AddPair('dueDate', DateToISO8601(dtpDueDate.Date));
    TaskData.AddPair('status', TJSONNumber.Create(cmbStatus.ItemIndex));
    TaskData.AddPair('createdAt', DateToISO8601(Now));

    // Obtenir la référence à la collection des tâches de l'utilisateur
    Collection := FFirestore.Collection('users').Document(CurrentUser.UID).Collection('tasks');

    // Ajouter la tâche
    Collection.Add(TaskData,
      procedure(const Document: IFirestoreDocument; const Error: string)
      begin
        if Error <> '' then
        begin
          ShowMessage('Erreur lors de l''ajout de la tâche : ' + Error);
          FCrashlytics.Log('Erreur Firestore : ' + Error);
          Exit;
        end;

        // Journaliser l'opération
        LogTaskOperation('add', Document.Id, edtTitle.Text);

        // Recharger les tâches
        LoadTasks;
      end
    );
  finally
    TaskData.Free;
  end;
end;

procedure TMainForm.UpdateTask;
var
  Document: IFirestoreDocument;
  TaskData: TJSONObject;
  CurrentUser: IFirebaseUser;
begin
  // Vérifier si nous avons un ID de tâche valide
  if FCurrentTaskId = '' then
    Exit;

  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;
  if CurrentUser = nil then
    Exit;

  // Préparer les données de la tâche
  TaskData := TJSONObject.Create;
  try
    TaskData.AddPair('title', edtTitle.Text);
    TaskData.AddPair('description', edtDescription.Text);
    TaskData.AddPair('dueDate', DateToISO8601(dtpDueDate.Date));
    TaskData.AddPair('status', TJSONNumber.Create(cmbStatus.ItemIndex));
    TaskData.AddPair('updatedAt', DateToISO8601(Now));

    // Obtenir la référence au document
    Document := FFirestore.Collection('users').Document(CurrentUser.UID)
                          .Collection('tasks').Document(FCurrentTaskId);

    // Mettre à jour la tâche
    Document.Update(TaskData,
      procedure(const Error: string)
      begin
        if Error <> '' then
        begin
          ShowMessage('Erreur lors de la mise à jour de la tâche : ' + Error);
          FCrashlytics.Log('Erreur Firestore : ' + Error);
          Exit;
        end;

        // Journaliser l'opération
        LogTaskOperation('update', FCurrentTaskId, edtTitle.Text);

        // Recharger les tâches
        LoadTasks;
      end
    );
  finally
    TaskData.Free;
  end;
end;

procedure TMainForm.DeleteTask(const TaskId: string);
var
  Document: IFirestoreDocument;
  CurrentUser: IFirebaseUser;
begin
  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;
  if CurrentUser = nil then
    Exit;

  // Confirmer la suppression
  if MessageDlg('Êtes-vous sûr de vouloir supprimer cette tâche ?',
                TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
    Exit;

  // Obtenir la référence au document
  Document := FFirestore.Collection('users').Document(CurrentUser.UID)
                        .Collection('tasks').Document(TaskId);

  // Supprimer la tâche
  Document.Delete(
    procedure(const Error: string)
    begin
      if Error <> '' then
      begin
        ShowMessage('Erreur lors de la suppression de la tâche : ' + Error);
        FCrashlytics.Log('Erreur Firestore : ' + Error);
        Exit;
      end;

      // Journaliser l'opération
      LogTaskOperation('delete', TaskId, '');

      // Recharger les tâches
      LoadTasks;
    end
  );
end;

procedure TMainForm.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  Document: IFirestoreDocument;
  CurrentUser: IFirebaseUser;
begin
  // Obtenir l'utilisateur actuel
  CurrentUser := FAuth.CurrentUser;
  if CurrentUser = nil then
    Exit;

  // Stocker l'ID de la tâche sélectionnée
  FCurrentTaskId := AItem.TagString;

  // Obtenir la référence au document
  Document := FFirestore.Collection('users').Document(CurrentUser.UID)
                        .Collection('tasks').Document(FCurrentTaskId);

  // Récupérer les données de la tâche
  Document.GetDocument(
    procedure(const Document: IFirestoreDocument; const Error: string)
    begin
      if Error <> '' then
      begin
        ShowMessage('Erreur lors de la récupération de la tâche : ' + Error);
        FCrashlytics.Log('Erreur Firestore : ' + Error);
        Exit;
      end;

      // Préparer pour l'édition
      FIsEditing := True;

      // Remplir les champs avec les données de la tâche
      var Data := Document.Data;
      edtTitle.Text := Data.GetValue<string>('title');
      edtDescription.Text := Data.GetValue<string>('description');

      // Convertir la date d'échéance
      var DueDateStr := Data.GetValue<string>('dueDate');
      if DueDateStr <> '' then
        dtpDueDate.Date := ISO8601ToDate(DueDateStr)
      else
        dtpDueDate.Date := Date + 1;

      // Définir le statut
      cmbStatus.ItemIndex := Data.GetValue<Integer>('status');

      // Afficher le panneau d'édition
      ShowAddEditPanel(True);

      // Analyser l'édition de tâche
      FAnalytics.LogEvent('edit_task_started',
        TFirebaseAnalyticsParameters.Create.Add('task_id', FCurrentTaskId));
    end
  );
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  // Actualiser la liste des tâches
  LoadTasks;

  // Analyser l'actualisation
  FAnalytics.LogEvent('tasks_refreshed', nil);
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  // Masquer le panneau d'ajout/édition
  ShowAddEditPanel(False);

  // Analyser l'annulation
  if FIsEditing then
    FAnalytics.LogEvent('edit_task_cancelled', nil)
  else
    FAnalytics.LogEvent('add_task_cancelled', nil);
end;

procedure TMainForm.ShowAddEditPanel(Show: Boolean);
begin
  // Afficher ou masquer le panneau d'ajout/édition
  PanelAdd.Visible := Show;

  // Afficher ou masquer les contrôles principaux
  ListView1.Visible := not Show;
  ToolBar1.Visible := not Show;
end;

procedure TMainForm.LogTaskOperation(const Operation, TaskId, TaskTitle: string);
begin
  // Journaliser l'opération pour Crashlytics
  FCrashlytics.Log(Format('Task operation: %s, ID: %s, Title: %s',
                         [Operation, TaskId, TaskTitle]));

  // Analyser l'opération
  var Params := TFirebaseAnalyticsParameters.Create;
  try
    Params.Add('operation', Operation);

    if TaskId <> '' then
      Params.Add('task_id', TaskId);

    if TaskTitle <> '' then
      Params.Add('task_title', TaskTitle);

    FAnalytics.LogEvent('task_operation', Params);
  finally
    Params.Free;
  end;
end;
```

## Bonnes pratiques pour l'intégration de Firebase

### 1. Gestion des clés et secrets

- Ne jamais stocker les clés API ou secrets directement dans le code source
- Utiliser des fichiers de configuration externes (`google-services.json` et `GoogleService-Info.plist`)
- Protéger l'accès à votre projet Firebase avec des règles de sécurité appropriées

### 2. Optimisation des performances

- Utiliser la mise en cache pour les requêtes fréquentes
- Utiliser les transactions Firestore pour les opérations complexes
- Limiter le nombre de documents récupérés dans les requêtes

```pascal
// Exemple de limitation du nombre de documents récupérés
procedure TMainForm.LoadRecentTasks;
var
  Collection: IFirestoreCollection;
  Query: IFirestoreQuery;
begin
  Collection := FFirestore.Collection('tasks');

  // Limiter à 10 tâches récentes
  Query := Collection.OrderBy('createdAt', ftDescending).Limit(10);

  Query.GetDocuments(
    procedure(const Documents: TArray<IFirestoreDocument>; const Error: string)
    begin
      // Traiter les documents...
    end
  );
end;
```

### 3. Gestion du mode hors ligne

Firebase Firestore prend en charge le mode hors ligne, ce qui permet à votre application de fonctionner même sans connexion internet :

```pascal
procedure TMainForm.ConfigureOfflineMode;
begin
  // Activer la persistance des données pour Firestore
  FFirestore.EnablePersistence(
    procedure(const Success: Boolean; const Error: string)
    begin
      if Success then
        ShowMessage('Mode hors ligne activé')
      else
        ShowMessage('Erreur lors de l''activation du mode hors ligne : ' + Error);
    end
  );
end;
```

### 4. Sécurité et règles Firestore

Pour sécuriser vos données Firestore, configurez des règles dans la console Firebase. Exemple de règles de base :

```javascript
// Règles Firestore dans la console Firebase
rules_version = '2';
service cloud.firestore {
  match /databases/{database}/documents {
    // Authentification requise pour toutes les opérations
    match /{document=**} {
      allow read, write: if request.auth != null;
    }

    // Les utilisateurs ne peuvent accéder qu'à leurs propres documents
    match /users/{userId}/{document=**} {
      allow read, write: if request.auth.uid == userId;
    }

    // Les tâches publiques peuvent être lues par tous les utilisateurs authentifiés
    match /publicTasks/{taskId} {
      allow read: if request.auth != null;
      allow write: if request.auth != null && request.resource.data.ownerId == request.auth.uid;
    }
  }
}
```

### 5. Gestion des coûts

Firebase propose un niveau gratuit généreux, mais une utilisation intensive peut entraîner des coûts. Quelques conseils pour gérer les coûts :

- Limiter les lectures/écritures Firestore en utilisant efficacement la mise en cache
- Optimiser les requêtes pour récupérer uniquement les données nécessaires
- Surveiller régulièrement l'utilisation dans la console Firebase
- Définir des alertes de budget dans Google Cloud Platform

## Résolution des problèmes courants

### 1. Problèmes de configuration

**Problème** : Firebase ne s'initialise pas correctement.

**Solution** :
- Vérifier que les fichiers `google-services.json` et `GoogleService-Info.plist` sont correctement placés dans les dossiers appropriés
- Vérifier que le nom du package et l'ID du bundle correspondent exactement à ceux enregistrés dans Firebase
- Vérifier que toutes les dépendances nécessaires sont installées

### 2. Erreurs d'authentification

**Problème** : Les opérations d'authentification échouent.

**Solution** :
- Vérifier que la méthode d'authentification est activée dans la console Firebase
- Vérifier les règles de sécurité dans la console Firebase
- Examiner les messages d'erreur détaillés dans les callbacks d'authentification

```pascal
procedure TMainForm.DiagnoseAuthError(const Error: string);
begin
  FCrashlytics.Log('Erreur d''authentification : ' + Error);

  // Analyser le message d'erreur pour fournir des conseils spécifiques
  if Error.Contains('network') then
    ShowMessage('Vérifiez votre connexion internet')
  else if Error.Contains('password') then
    ShowMessage('Mot de passe incorrect ou compte inexistant')
  else if Error.Contains('badly-formatted') then
    ShowMessage('Adresse e-mail invalide')
  else
    ShowMessage('Erreur d''authentification : ' + Error);
end;
```

### 3. Problèmes de temps de réponse

**Problème** : Les opérations Firebase sont lentes.

**Solution** :
- Utiliser des indicateurs de chargement pour informer l'utilisateur
- Optimiser les requêtes Firestore
- Vérifier la qualité de la connexion internet
- Implémenter la mise en cache côté client

```pascal
procedure TMainForm.ShowLoadingIndicator(const Message: string);
begin
  // Afficher un indicateur de chargement
  lblLoading.Text := Message;
  aniLoading.Visible := True;
  aniLoading.Enabled := True;
end;

procedure TMainForm.HideLoadingIndicator;
begin
  // Masquer l'indicateur de chargement
  aniLoading.Enabled := False;
  aniLoading.Visible := False;
end;
```

## Conclusion

L'intégration des services Firebase dans vos applications Delphi vous permet d'ajouter rapidement des fonctionnalités puissantes comme l'authentification, le stockage de données en temps réel, les notifications push et l'analyse. Firebase offre une infrastructure backend complète qui vous permet de vous concentrer sur le développement de votre application plutôt que sur la gestion des serveurs.

En suivant les bonnes pratiques et en comprenant comment intégrer efficacement chaque service, vous pouvez créer des applications mobiles robustes et évolutives qui offrent une excellente expérience utilisateur.

N'oubliez pas que Firebase est une plateforme en constante évolution, avec de nouvelles fonctionnalités ajoutées régulièrement. Consultez la documentation officielle de Firebase et les ressources Delphi pour rester à jour avec les dernières améliorations et capacités.

Dans la prochaine section, nous explorerons comment développer des jeux mobiles avec Delphi et FireMonkey, en utilisant les capacités graphiques avancées et les API de jeu pour créer des expériences interactives engageantes.
