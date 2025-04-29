# 11.5 Tâches asynchrones et callbacks

## Introduction

Dans les sections précédentes, nous avons exploré les threads et les tâches parallèles pour exécuter du code en arrière-plan. Dans cette partie, nous allons découvrir une approche plus élégante pour gérer les opérations de longue durée : la programmation asynchrone.

La programmation asynchrone permet d'écrire du code qui peut "attendre" qu'une opération se termine sans bloquer le thread principal. Delphi offre plusieurs mécanismes pour implémenter ces fonctionnalités, notamment le modèle async/await et les callbacks.

## Différence entre parallèle et asynchrone

Avant d'entrer dans les détails, clarifions la différence entre programmation parallèle et asynchrone :

- **Parallèle** : Exécuter plusieurs opérations en même temps sur des threads différents.
- **Asynchrone** : Démarrer une opération et continuer à exécuter d'autres tâches sans attendre qu'elle se termine.

Un programme peut être :
- Asynchrone sans être parallèle (un seul thread qui bascule entre différentes tâches)
- Parallèle sans être asynchrone (plusieurs threads qui exécutent des opérations synchrones)
- Les deux à la fois (plusieurs threads exécutant des opérations asynchrones)

## Modèle de programmation asynchrone en Delphi

### Le modèle de promesse avec IFuture<T>

L'interface `IFuture<T>` représente le résultat futur d'une opération asynchrone. Elle est similaire à `TTask<T>` mais est conçue spécifiquement pour les opérations asynchrones.

```pascal
uses
  System.Threading;

var
  MonFutur: IFuture<string>;
begin
  // Démarrer une opération asynchrone
  MonFutur := TTask.Future<string>(
    function: string
    begin
      // Simuler une opération longue
      Sleep(3000);
      Result := 'Opération terminée';
    end
  );

  // Le code continue immédiatement, sans attendre la fin de l'opération
  Label1.Caption := 'Opération en cours...';

  // Plus tard, quand nous avons besoin du résultat :
  ShowMessage(MonFutur.Value); // Cette ligne attend si nécessaire
end;
```

### Les méthodes asynchrones avec "async/await"

Delphi XE8 et versions ultérieures proposent un modèle similaire à l'async/await de C# avec les méthodes `TTask.Run` et `TTask.Await` :

```pascal
procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  // Démarrer une opération asynchrone
  TTask.Run(
    procedure
    begin
      // Simuler une opération longue
      Sleep(3000);

      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Caption := 'Opération terminée';
        end
      );
    end
  );

  // Le code continue sans attendre
  Label1.Caption := 'Opération en cours...';
end;
```

## Utilisation des callbacks

Les callbacks sont des fonctions ou procédures que vous définissez pour être exécutées lorsqu'une opération asynchrone est terminée. C'est un modèle très courant en programmation asynchrone.

### Callbacks avec TTask

```pascal
procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  Label1.Caption := 'Démarrage...';

  TTask.Run(
    procedure
    begin
      // Simuler une opération longue
      Sleep(3000);
    end
  ).ContinueWith(
    procedure(Task: ITask)
    begin
      // Ce code s'exécute quand la tâche est terminée
      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Caption := 'Opération terminée';
          ButtonStart.Enabled := True;
        end
      );
    end
  );

  ButtonStart.Enabled := False;
  Label1.Caption := 'Opération en cours...';
end;
```

Le callback défini avec `ContinueWith` sera exécuté automatiquement lorsque la tâche sera terminée.

### Chaînes de callbacks

Vous pouvez enchaîner plusieurs callbacks pour créer des séquences d'opérations asynchrones :

```pascal
TTask.Run(
  procedure
  begin
    // Première opération
    Sleep(1000);
  end
).ContinueWith(
  function(Task: ITask): ITask
  begin
    // Deuxième opération, exécutée après la première
    Result := TTask.Run(
      procedure
      begin
        Sleep(1000);
      end
    );
  end
).Unwrap.ContinueWith(
  procedure(Task: ITask)
  begin
    // Troisième opération, exécutée après la deuxième
    TThread.Synchronize(nil,
      procedure
      begin
        ShowMessage('Toutes les opérations sont terminées !');
      end
    );
  end
);
```

La méthode `Unwrap` est nécessaire pour "déballer" le résultat de la tâche intermédiaire.

## Exemple pratique : téléchargement asynchrone avec callbacks

Voici un exemple plus réaliste de téléchargement asynchrone avec callbacks :

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
var
  URL: string;
begin
  URL := EditURL.Text;
  ButtonDownload.Enabled := False;
  ProgressBar1.Position := 0;
  LabelStatus.Caption := 'Préparation du téléchargement...';

  // Tâche 1 : Vérification de l'URL
  TTask.Run(
    function: Boolean
    begin
      // Simuler une vérification
      Sleep(1000);
      Result := True; // URL valide
    end
  ).ContinueWith(
    function(AntecedentTask: ITask<Boolean>): ITask
    begin
      if AntecedentTask.Value then
      begin
        // URL valide, continuer avec le téléchargement
        TThread.Queue(nil,
          procedure
          begin
            LabelStatus.Caption := 'Téléchargement en cours...';
          end
        );

        // Tâche 2 : Téléchargement
        Result := TTask.Run(
          procedure
          var
            HTTPClient: TNetHTTPClient;
            HTTPRequest: TNetHTTPRequest;
            ResponseStream: TFileStream;
          begin
            HTTPClient := TNetHTTPClient.Create(nil);
            HTTPRequest := TNetHTTPRequest.Create(nil);
            try
              HTTPRequest.Client := HTTPClient;

              // Événement pour suivre la progression
              HTTPClient.OnReceiveData := procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
              begin
                if AContentLength > 0 then
                  TThread.Queue(nil,
                    procedure
                    begin
                      ProgressBar1.Position := Round((AReadCount / AContentLength) * 100);
                    end
                  );
              end;

              // Création du fichier de destination
              ResponseStream := TFileStream.Create('downloaded_file.dat', fmCreate);
              try
                // Téléchargement du fichier
                HTTPRequest.Get(URL, ResponseStream);
              finally
                ResponseStream.Free;
              end;
            finally
              HTTPRequest.Free;
              HTTPClient.Free;
            end;
          end
        );
      end
      else
      begin
        // URL invalide, afficher un message d'erreur
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage('URL invalide !');
            ButtonDownload.Enabled := True;
            LabelStatus.Caption := 'Téléchargement annulé';
          end
        );
        Result := nil;
      end;
    end
  ).Unwrap.ContinueWith(
    procedure(Task: ITask)
    begin
      // Dernière étape : mise à jour de l'interface
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonDownload.Enabled := True;
          LabelStatus.Caption := 'Téléchargement terminé !';
          ShowMessage('Fichier téléchargé avec succès !');
        end
      );
    end
  );
end;
```

Cette approche vous permet de diviser une opération complexe en étapes distinctes, chacune s'exécutant après que la précédente soit terminée.

## Gestion des erreurs dans les callbacks

La gestion des erreurs est importante dans les opérations asynchrones. Voici comment gérer les exceptions dans les callbacks :

```pascal
TTask.Run(
  procedure
  begin
    // Code qui peut générer une exception
    raise Exception.Create('Erreur dans la tâche');
  end
).ContinueWith(
  procedure(AntecedentTask: ITask)
  begin
    if AntecedentTask.Status = TTaskStatus.Faulted then
    begin
      // Gestion de l'erreur
      TThread.Synchronize(nil,
        procedure
        var
          E: Exception;
        begin
          E := AntecedentTask.Exception.InnerException;
          ShowMessage('Erreur : ' + E.Message);
        end
      );
    end
    else
    begin
      // Code exécuté si tout s'est bien passé
      TThread.Synchronize(nil,
        procedure
        begin
          ShowMessage('Tâche terminée avec succès !');
        end
      );
    end;
  end
);
```

## Bonnes pratiques pour les méthodes asynchrones

### 1. Rendre les méthodes asynchrones "awaitable"

Pour créer vos propres méthodes asynchrones que d'autres peuvent attendre, renvoyez un `ITask` ou `IFuture<T>` :

```pascal
function TéléchargerFichierAsync(const URL: string): ITask;
begin
  Result := TTask.Run(
    procedure
    begin
      // Code de téléchargement...
    end
  );
end;

// Utilisation
procedure TForm1.ButtonClick(Sender: TObject);
var
  TâcheTéléchargement: ITask;
begin
  TâcheTéléchargement := TéléchargerFichierAsync('http://exemple.com/fichier.txt');

  // Ajouter un callback
  TâcheTéléchargement.ContinueWith(
    procedure(Task: ITask)
    begin
      // Code à exécuter après le téléchargement...
    end
  );
end;
```

### 2. Créer des méthodes asynchrones avec valeur de retour

```pascal
function CalculerAsync(const Valeur: Integer): IFuture<Integer>;
begin
  Result := TTask.Future<Integer>(
    function: Integer
    begin
      // Simuler un calcul long
      Sleep(2000);
      Result := Valeur * Valeur;
    end
  );
end;

// Utilisation
procedure TForm1.ButtonClick(Sender: TObject);
var
  Résultat: IFuture<Integer>;
begin
  Résultat := CalculerAsync(5);

  // Ajouter un callback pour traiter le résultat
  TTask.Run(
    procedure
    begin
      // Attendre le résultat (dans un thread secondaire)
      var Valeur := Résultat.Value;

      // Mettre à jour l'interface
      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Caption := 'Résultat : ' + IntToStr(Valeur);
        end
      );
    end
  );
end;
```

### 3. Éviter de bloquer le thread principal

Évitez d'appeler directement `.Value` ou `.Wait` sur une tâche depuis le thread principal. Utilisez plutôt des callbacks ou exécutez ces appels dans un thread secondaire.

```pascal
// À éviter dans le thread principal
ShowMessage(MonFutur.Value); // Bloque le thread principal

// Préférer
MonFutur.ContinueWith(
  procedure(Task: ITask<string>)
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        ShowMessage(Task.Value);
      end
    );
  end
);
```

## Utilisation des opérations asynchrones intégrées

Delphi fournit des versions asynchrones de nombreuses opérations courantes, notamment pour les entrées/sorties et les communications réseau.

### Exemple avec THTTPClient asynchrone

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
var
  Client: TNetHTTPClient;
  Request: TNetHTTPRequest;
  URL: string;
begin
  URL := EditURL.Text;
  ButtonDownload.Enabled := False;
  LabelStatus.Caption := 'Téléchargement en cours...';

  Client := TNetHTTPClient.Create(nil);
  Request := TNetHTTPRequest.Create(nil);
  try
    Request.Client := Client;

    // Définir un callback pour lorsque le téléchargement est terminé
    Request.OnRequestCompleted := procedure(const Sender: TObject; const AResponse: IHTTPResponse)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonDownload.Enabled := True;
          LabelStatus.Caption := 'Téléchargement terminé !';
          MemoResponse.Text := AResponse.ContentAsString;
        end
      );
    end;

    // Définir un callback en cas d'erreur
    Request.OnRequestError := procedure(const Sender: TObject; const AError: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonDownload.Enabled := True;
          LabelStatus.Caption := 'Erreur !';
          ShowMessage('Erreur : ' + AError);
        end
      );
    end;

    // Lancer le téléchargement asynchrone
    Request.GetAsync(URL);
  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      Client.Free;
      Request.Free;
      ButtonDownload.Enabled := True;
      LabelStatus.Caption := 'Erreur !';
    end;
  end;
end;
```

Cette méthode utilise les événements intégrés `OnRequestCompleted` et `OnRequestError` pour gérer de manière asynchrone la fin du téléchargement.

## Combiner callbacks et méthodes anonymes

Les méthodes anonymes (ou lambdas) sont particulièrement utiles avec les callbacks. Elles permettent de définir directement le comportement à exécuter, rendant le code plus concis et plus lisible :

```pascal
ButtonCalculer.OnClick := procedure(Sender: TObject)
begin
  // Désactiver le bouton pendant le calcul
  ButtonCalculer.Enabled := False;

  // Lancer le calcul en arrière-plan
  TTask.Run(
    function: Integer
    begin
      // Simuler un calcul long
      Sleep(3000);
      Result := 42;
    end
  ).ContinueWith(
    procedure(AntecedentTask: ITask<Integer>)
    begin
      // Récupérer le résultat
      var Résultat := AntecedentTask.Value;

      // Mettre à jour l'interface
      TThread.Synchronize(nil,
        procedure
        begin
          EditRésultat.Text := IntToStr(Résultat);
          ButtonCalculer.Enabled := True;
        end
      );
    end
  );
end;
```

## Exemple avancé : opérations asynchrones multiples

Parfois, vous devez exécuter plusieurs opérations asynchrones et attendre qu'elles soient toutes terminées. Voici comment procéder :

```pascal
procedure TForm1.ButtonStartClick(Sender: TObject);
var
  Tâches: array[0..2] of ITask;
begin
  ButtonStart.Enabled := False;
  LabelStatus.Caption := 'Opérations en cours...';

  // Créer trois tâches asynchrones
  Tâches[0] := TTask.Run(
    procedure
    begin
      // Première opération
      Sleep(2000);
    end
  );

  Tâches[1] := TTask.Run(
    procedure
    begin
      // Deuxième opération
      Sleep(3000);
    end
  );

  Tâches[2] := TTask.Run(
    procedure
    begin
      // Troisième opération
      Sleep(1500);
    end
  );

  // Créer une tâche qui attend que toutes les autres soient terminées
  TTask.Run(
    procedure
    begin
      // Attendre que toutes les tâches soient terminées
      TTask.WaitForAll(Tâches);

      // Mettre à jour l'interface
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonStart.Enabled := True;
          LabelStatus.Caption := 'Toutes les opérations sont terminées !';
          ShowMessage('Traitement terminé avec succès !');
        end
      );
    end
  );
end;
```

## TPromise : un mécanisme complet pour les opérations asynchrones

Dans les versions récentes de Delphi, la classe `TPromise<T>` offre un mécanisme complet pour gérer les opérations asynchrones avec les états de réussite, d'échec et de progression :

```pascal
// Création d'une promesse pour un téléchargement
var Promise := TPromise<TBytes>.Create(
  // Executor - code qui sera exécuté par la promesse
  procedure(
    const Resolve: TProc<TBytes>;   // Procédure appelée en cas de succès
    const Reject: TProc<Exception>; // Procédure appelée en cas d'échec
    const OnProgress: TProc<Integer> // Procédure pour signaler la progression
  )
  var
    HTTPClient: TNetHTTPClient;
    HTTPRequest: TNetHTTPRequest;
    URL: string;
    BytesLus: Integer;
    TailleTotale: Integer;
  begin
    try
      URL := 'https://exemple.com/fichier.zip';

      HTTPClient := TNetHTTPClient.Create(nil);
      HTTPRequest := TNetHTTPRequest.Create(nil);
      try
        HTTPRequest.Client := HTTPClient;

        // Événement pour suivre la progression
        HTTPClient.OnReceiveData := procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
        begin
          if AContentLength > 0 then
          begin
            BytesLus := AReadCount;
            TailleTotale := AContentLength;
            OnProgress(Round((BytesLus / TailleTotale) * 100));
          end;
        end;

        // Télécharger le fichier
        var Response := HTTPRequest.Get(URL);

        // Résoudre la promesse avec les données téléchargées
        Resolve(Response.ContentAsBytes);
      finally
        HTTPRequest.Free;
        HTTPClient.Free;
      end;
    except
      on E: Exception do
        Reject(E); // Rejeter la promesse en cas d'erreur
    end;
  end
);

// Utilisation de la promesse
Promise.Then(
  // Succès
  procedure(const DataBytes: TBytes)
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        ShowMessage('Téléchargement réussi ! Taille : ' + IntToStr(Length(DataBytes)) + ' octets');
        // Traiter les données...
      end
    );
  end
).Catch(
  // Échec
  procedure(const E: Exception)
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        ShowMessage('Erreur : ' + E.Message);
      end
    );
  end
).Progress(
  // Progression
  procedure(const ProgressValue: Integer)
  begin
    TThread.Queue(nil,
      procedure
      begin
        ProgressBar1.Position := ProgressValue;
        LabelProgress.Caption := IntToStr(ProgressValue) + '%';
      end
    );
  end
);
```

> ⚠️ **Note** : `TPromise` nécessite Delphi 12 ou supérieur.

## Exercice pratique

Créez une application qui télécharge simultanément plusieurs images à partir d'une liste d'URLs. L'application devrait :

1. Permettre à l'utilisateur d'ajouter des URLs à une liste
2. Avoir un bouton pour démarrer le téléchargement de toutes les images
3. Afficher la progression individuelle de chaque téléchargement
4. Afficher les images au fur et à mesure qu'elles sont téléchargées
5. Afficher un message lorsque tous les téléchargements sont terminés

Utilisez les concepts de tâches asynchrones et de callbacks pour implémenter cette fonctionnalité.

## Résumé

- La programmation asynchrone permet d'exécuter des opérations de longue durée sans bloquer le thread principal
- Les callbacks sont des fonctions qui s'exécutent lorsqu'une opération asynchrone est terminée
- Vous pouvez enchaîner plusieurs callbacks pour créer des séquences d'opérations
- Delphi offre plusieurs mécanismes pour la programmation asynchrone : `TTask`, `IFuture<T>`, `TPromise<T>`, etc.
- Utilisez `ContinueWith` pour ajouter des callbacks à vos tâches
- Évitez de bloquer le thread principal en appelant directement `.Value` ou `.Wait`
- Utilisez les méthodes asynchrones intégrées pour les opérations d'entrée/sortie et de réseau

La programmation asynchrone et les callbacks sont des outils puissants pour créer des applications réactives qui peuvent effectuer des opérations complexes sans bloquer l'interface utilisateur. En maîtrisant ces concepts, vous pourrez développer des applications Delphi modernes et performantes.
