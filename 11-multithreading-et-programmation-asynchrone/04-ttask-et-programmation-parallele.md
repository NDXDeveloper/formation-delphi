# 11.4 TTask et programmation parallèle

## Introduction

Jusqu'à présent, nous avons étudié la création et la gestion de threads avec la classe `TThread`, qui offre un contrôle précis mais nécessite une certaine quantité de code. Delphi propose une approche plus moderne et plus simple pour la programmation parallèle : la classe `TTask`.

`TTask` fait partie du framework de programmation parallèle (PPL - Parallel Programming Library) introduit dans Delphi XE7. Il offre une syntaxe plus concise et s'intègre parfaitement avec les fonctionnalités modernes du langage comme les expressions lambda et les méthodes anonymes.

## Avantages de TTask par rapport à TThread

- Code plus compact et plus lisible
- Pas besoin de créer une classe séparée pour chaque type de thread
- Gestion automatique du cycle de vie des tâches
- Prise en charge intégrée des callbacks (actions à exécuter à la fin de la tâche)
- Possibilité d'attendre facilement plusieurs tâches

## Premiers pas avec TTask

### Créer et démarrer une tâche simple

Voici comment créer une tâche simple avec `TTask` :

```pascal
uses
  System.Threading;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      // Code à exécuter dans la tâche
      Sleep(2000);  // Simuler un travail qui prend du temps

      // Mettre à jour l'interface utilisateur
      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Caption := 'Tâche terminée !';
        end
      );
    end
  );
end;
```

Notez que nous utilisons une procédure anonyme pour définir le code de la tâche. Cela nous évite de créer une classe séparée.

### Attendre la fin d'une tâche

Si vous avez besoin d'attendre qu'une tâche se termine, vous pouvez utiliser la méthode `Wait` :

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      // Code de la tâche...
    end
  );

  // Faire autre chose pendant que la tâche s'exécute...

  // Attendre que la tâche se termine
  MaTache.Wait;

  // Continuer l'exécution après la fin de la tâche
end;
```

> ⚠️ **Attention** : N'appelez jamais `Wait` directement depuis le thread principal dans une application avec interface utilisateur, car cela bloquerait l'interface. Utilisez plutôt les mécanismes asynchrones décrits plus loin.

## Traiter le résultat d'une tâche

### Obtenir une valeur de retour avec TTask<T>

Pour récupérer une valeur calculée par une tâche, utilisez `TTask<T>` :

```pascal
var
  TacheCalcul: ITask<Integer>;
begin
  TacheCalcul := TTask.Future<Integer>(
    function: Integer
    begin
      // Simuler un calcul long
      Sleep(3000);
      Result := 42;  // Valeur calculée
    end
  );

  // La tâche s'exécute en arrière-plan

  // Récupérer le résultat (cette ligne attend automatiquement la fin de la tâche)
  ShowMessage('Résultat : ' + IntToStr(TacheCalcul.Value));
end;
```

La propriété `Value` renvoie le résultat de la tâche. Si la tâche n'est pas encore terminée, l'accès à `Value` bloquera jusqu'à ce qu'elle se termine.

### Exécuter du code à la fin d'une tâche

Au lieu d'attendre activement la fin d'une tâche, vous pouvez spécifier une action à exécuter lorsqu'elle se termine :

```pascal
TTask.Run(
  procedure
  begin
    // Code de la tâche...
  end
).ContinueWith(
  procedure(AntecedentTask: ITask)
  begin
    // Ce code s'exécute quand la tâche est terminée
    TThread.Synchronize(nil,
      procedure
      begin
        ShowMessage('Tâche terminée !');
      end
    );
  end
);
```

Le paramètre `AntecedentTask` vous donne accès à la tâche qui vient de se terminer.

## Exemples pratiques avec TTask

### Exemple 1 : Téléchargement de fichier avec indicateur de progression

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
var
  URL: string;
begin
  URL := EditURL.Text;
  ButtonDownload.Enabled := False;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;

  TTask.Run(
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

        // Mise à jour de l'interface à la fin
        TThread.Synchronize(nil,
          procedure
          begin
            ButtonDownload.Enabled := True;
            ShowMessage('Téléchargement terminé !');
          end
        );
      finally
        HTTPRequest.Free;
        HTTPClient.Free;
      end;
    end
  );
end;
```

### Exemple 2 : Traitement d'image parallèle

```pascal
procedure TForm1.ButtonProcessImageClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    var
      Bitmap: TBitmap;
    begin
      // Charger l'image
      Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromFile('image.bmp');

        // Appliquer un effet (simulé)
        TThread.Synchronize(nil,
          procedure
          begin
            LabelStatus.Caption := 'Application de l''effet...';
          end
        );

        // Simuler un traitement long
        Sleep(3000);

        // Sauvegarder le résultat
        Bitmap.SaveToFile('image_processed.bmp');

        // Mise à jour de l'interface
        TThread.Synchronize(nil,
          procedure
          begin
            ImageProcessed.Picture.LoadFromFile('image_processed.bmp');
            LabelStatus.Caption := 'Traitement terminé !';
          end
        );
      finally
        Bitmap.Free;
      end;
    end
  );
end;
```

## TThread.Queue vs TThread.Synchronize

Lorsque vous utilisez `TTask`, vous aurez souvent besoin de mettre à jour l'interface utilisateur. Delphi propose deux méthodes pour cela :

1. **TThread.Synchronize** : Exécute le code immédiatement, en bloquant le thread appelant jusqu'à ce que l'exécution soit terminée.

   ```pascal
   TThread.Synchronize(nil,
     procedure
     begin
       Label1.Caption := 'Terminé';
     end
   );
   ```

2. **TThread.Queue** : Place le code dans une file d'attente pour qu'il soit exécuté plus tard par le thread principal, sans bloquer le thread appelant.

   ```pascal
   TThread.Queue(nil,
     procedure
     begin
       Label1.Caption := 'Terminé';
     end
   );
   ```

Quand utiliser l'un ou l'autre ?
- Utilisez `TThread.Synchronize` quand le thread secondaire a besoin d'attendre que l'action soit complétée.
- Utilisez `TThread.Queue` pour des mises à jour plus légères de l'interface, où le thread peut continuer son travail sans attendre.

## Exécution de plusieurs tâches en parallèle

### Exécuter plusieurs tâches indépendantes

```pascal
var
  Taches: array[1..3] of ITask;
begin
  // Créer trois tâches
  Taches[1] := TTask.Run(
    procedure
    begin
      // Code de la tâche 1...
    end
  );

  Taches[2] := TTask.Run(
    procedure
    begin
      // Code de la tâche 2...
    end
  );

  Taches[3] := TTask.Run(
    procedure
    begin
      // Code de la tâche 3...
    end
  );

  // Attendre que toutes les tâches soient terminées
  TTask.WaitForAll(Taches);

  // Continuer après que toutes les tâches sont terminées
end;
```

### Attendre la première tâche terminée

Si vous avez besoin d'attendre qu'au moins une tâche se termine :

```pascal
var
  Taches: array[1..3] of ITask;
  IndexTermine: Integer;
begin
  // Créer trois tâches...

  // Attendre qu'au moins une tâche se termine
  IndexTermine := TTask.WaitForAny(Taches);

  // IndexTermine contient l'index de la première tâche terminée
  ShowMessage('La tâche ' + IntToStr(IndexTermine) + ' a terminé en premier !');
end;
```

## Parallélisme de données avec Parallel.For

Pour traiter un grand nombre d'éléments en parallèle, vous pouvez utiliser `Parallel.For` :

```pascal
uses
  System.Threading;

procedure TraiterDonneesMassives;
const
  NOMBRE_ELEMENTS = 10000;
var
  Donnees: array[0..NOMBRE_ELEMENTS-1] of Integer;
  i: Integer;
begin
  // Initialiser les données
  for i := 0 to NOMBRE_ELEMENTS-1 do
    Donnees[i] := i;

  // Traitement parallèle
  TParallel.For(0, NOMBRE_ELEMENTS-1,
    procedure(Index: Integer)
    begin
      // Traiter Donnees[Index]
      Donnees[Index] := Donnees[Index] * 2;
    end
  );

  // À ce stade, toutes les données ont été traitées
end;
```

`Parallel.For` divise automatiquement la plage d'indices en blocs et les traite en parallèle sur plusieurs threads. C'est idéal pour les opérations indépendantes sur de grands ensembles de données.

> ⚠️ **Attention** : Assurez-vous que les opérations dans la boucle parallèle sont indépendantes. Si elles modifient des données partagées, vous devrez utiliser des mécanismes de synchronisation comme les sections critiques.

## Parallélisme de données avec Parallel.ForEach

Pour itérer sur une collection en parallèle :

```pascal
var
  MaListe: TList<string>;
begin
  MaListe := TList<string>.Create;
  try
    // Remplir la liste...

    // Traitement parallèle de tous les éléments
    TParallel.ForEach(MaListe,
      procedure(const Element: string)
      begin
        // Traiter chaque élément
      end
    );
  finally
    MaListe.Free;
  end;
end;
```

## Annulation de tâches

Delphi fournit un mécanisme d'annulation pour les tâches via la classe `TThreadPool` :

```pascal
var
  Annulation: ICancellationToken;
  Tache: ITask;
begin
  // Créer un token d'annulation
  Annulation := TCancellationTokenSource.Create.Token;

  // Créer une tâche qui vérifie périodiquement le token d'annulation
  Tache := TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 100 do
      begin
        // Vérifier si la tâche doit être annulée
        if Annulation.IsCancellationRequested then
          Exit;

        // Faire le travail...
        Sleep(100);

        // Mettre à jour la progression
        TThread.Queue(nil,
          procedure
          begin
            ProgressBar1.Position := i;
          end
        );
      end;
    end,
    Annulation
  );

  // Pour annuler la tâche plus tard
  ButtonCancel.OnClick := procedure(Sender: TObject)
  begin
    TCancellationTokenSource(Annulation).Cancel;
  end;
end;
```

## Gestion des exceptions dans les tâches

Lorsqu'une exception se produit dans une tâche, elle n'est pas automatiquement propagée au thread principal. Vous devez explicitement récupérer et gérer ces exceptions :

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      // Code qui peut générer une exception
      raise Exception.Create('Erreur dans la tâche');
    end
  );

  try
    // Attendre la fin de la tâche
    MaTache.Wait;
  except
    on E: Exception do
    begin
      ShowMessage('Exception dans la tâche : ' + E.Message);
    end;
  end;
end;
```

Pour les tâches avec valeur de retour, vous pouvez vérifier si une exception s'est produite avec la propriété `Status` :

```pascal
var
  TacheCalcul: ITask<Integer>;
begin
  TacheCalcul := TTask.Future<Integer>(
    function: Integer
    begin
      // Code qui peut générer une exception
      raise Exception.Create('Erreur de calcul');
      Result := 0; // Jamais atteint
    end
  );

  // Attendre la fin de la tâche
  TacheCalcul.Wait;

  // Vérifier le statut
  if TacheCalcul.Status = TTaskStatus.Faulted then
    ShowMessage('La tâche a échoué avec une exception')
  else
    ShowMessage('Résultat : ' + IntToStr(TacheCalcul.Value));
end;
```

## Pool de threads et paramètres

Les tâches créées avec `TTask.Run` utilisent un pool de threads géré par Delphi. Vous pouvez configurer ce pool :

```pascal
// Définir le nombre maximum de threads
TThreadPool.Default.MaxWorkerThreads := ProcessorCount * 2;
```

En général, il est recommandé de laisser Delphi gérer automatiquement le pool, mais ces paramètres peuvent être utiles dans certains cas avancés.

## Exemple complet : Traitement d'images en parallèle

Voici un exemple plus complet qui traite plusieurs images en parallèle :

```pascal
procedure TForm1.ButtonProcessImagesClick(Sender: TObject);
var
  Fichiers: TStringList;
  TotalTraite: Integer;
  Section: TCriticalSection;
begin
  // Liste des fichiers à traiter
  Fichiers := TStringList.Create;
  try
    Fichiers.LoadFromFile('liste_images.txt');

    // Initialisation
    TotalTraite := 0;
    Section := TCriticalSection.Create;
    try
      ProgressBar1.Max := Fichiers.Count;
      ProgressBar1.Position := 0;
      LabelStatus.Caption := 'Traitement en cours...';

      // Traiter toutes les images en parallèle
      TParallel.For(0, Fichiers.Count - 1,
        procedure(Index: Integer)
        var
          NomFichier: string;
        begin
          NomFichier := Fichiers[Index];

          // Simuler un traitement d'image
          Sleep(500 + Random(1000));

          // Mettre à jour le compteur et la barre de progression
          Section.Enter;
          try
            Inc(TotalTraite);
          finally
            Section.Leave;
          end;

          // Mettre à jour l'interface
          TThread.Queue(nil,
            procedure
            begin
              ProgressBar1.Position := TotalTraite;
              LabelStatus.Caption := Format('Traitement en cours... %d/%d',
                                           [TotalTraite, Fichiers.Count]);

              // Ajouter le fichier à la liste des traitements terminés
              ListBoxCompleted.Items.Add(NomFichier);

              // Vérifier si tout est terminé
              if TotalTraite = Fichiers.Count then
                ShowMessage('Traitement terminé !');
            end
          );
        end
      );
    finally
      Section.Free;
    end;
  finally
    Fichiers.Free;
  end;
end;
```

## Bonnes pratiques avec TTask

1. **Évitez de créer trop de tâches**
   Le nombre optimal de tâches parallèles est généralement proche du nombre de cœurs de processeur disponibles. Créer trop de tâches peut réduire les performances à cause du coût des changements de contexte.

2. **Utilisez TThread.Queue pour les mises à jour fréquentes de l'interface**
   Pour éviter de bloquer votre tâche, préférez `TThread.Queue` à `TThread.Synchronize` lorsque vous mettez fréquemment à jour l'interface utilisateur.

3. **Gérez correctement les ressources**
   Assurez-vous que les ressources sont correctement libérées, même en cas d'exception dans la tâche.

4. **Évitez les dépendances entre tâches**
   Les tâches parallèles sont plus efficaces lorsqu'elles sont indépendantes. Si vous avez des dépendances, envisagez d'utiliser `ContinueWith`.

5. **Protégez les données partagées**
   Si plusieurs tâches accèdent aux mêmes données, utilisez des mécanismes de synchronisation comme les sections critiques.

## Exercice pratique

Créez une application qui calcule les nombres premiers dans un intervalle donné en utilisant le parallélisme. L'application devrait :

1. Permettre à l'utilisateur de spécifier l'intervalle (ex. de 1 à 1 000 000)
2. Diviser l'intervalle en sous-intervalles pour un traitement parallèle
3. Afficher la progression en temps réel
4. Afficher le temps d'exécution total
5. Permettre d'annuler le calcul

Cet exercice vous permettra d'appliquer les concepts de `TTask`, `TParallel.For`, l'annulation, et la mise à jour de l'interface utilisateur depuis des tâches parallèles.

## Résumé

- `TTask` offre une approche moderne et plus simple pour la programmation parallèle
- Vous pouvez créer des tâches sans avoir à définir de nouvelles classes
- `TTask<T>` permet de récupérer des résultats calculés par des tâches
- `TParallel.For` et `TParallel.ForEach` facilitent le traitement parallèle des collections
- Utilisez `TThread.Synchronize` ou `TThread.Queue` pour mettre à jour l'interface utilisateur
- La gestion des exceptions et l'annulation nécessitent une attention particulière

La programmation parallèle avec `TTask` est puissante et plus facile à utiliser que les threads traditionnels. Elle vous permet d'exploiter au maximum les processeurs multi-cœurs modernes tout en simplifiant votre code.

Dans la prochaine section, nous explorerons les tâches asynchrones et les callbacks, qui constituent une autre approche élégante pour la programmation concurrente.
