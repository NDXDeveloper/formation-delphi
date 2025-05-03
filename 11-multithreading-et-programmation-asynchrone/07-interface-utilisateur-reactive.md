# 11.7 Interface utilisateur réactive

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Une application bien conçue doit rester fluide et réactive, même lorsqu'elle exécute des opérations longues ou complexes en arrière-plan. Les utilisateurs s'attendent à pouvoir interagir avec l'interface à tout moment, sans blocages ni gel de l'application. Dans ce chapitre, nous allons découvrir comment créer des interfaces utilisateur réactives en Delphi en tirant parti des techniques de multithreading que nous avons apprises.

## Pourquoi une interface utilisateur réactive est-elle importante ?

Imaginez que vous utilisez une application qui se fige complètement pendant plusieurs secondes (ou minutes) chaque fois que vous effectuez une opération. Cette expérience utilisateur médiocre peut :

- Frustrer les utilisateurs qui pensent que l'application a planté
- Réduire la productivité
- Donner une impression de mauvaise qualité
- Dans certains cas, pousser les utilisateurs à forcer l'arrêt de l'application

Heureusement, avec les techniques de multithreading, nous pouvons éviter ces problèmes et créer des applications qui restent réactives en toutes circonstances.

## Le thread principal et l'interface utilisateur

Comme nous l'avons vu précédemment, Delphi (comme la plupart des frameworks d'interface graphique) utilise un modèle où l'interface utilisateur est gérée par un seul thread : le thread principal. Tous les contrôles visuels, les événements et les messages Windows sont traités par ce thread.

### La règle fondamentale

**Règle d'or** : Ne jamais bloquer le thread principal avec des opérations longues.

Si vous effectuez une opération qui prend plus de quelques dizaines de millisecondes dans le thread principal, l'interface utilisateur sera gelée pendant cette durée.

## Identification des opérations qui bloquent l'interface

Voici les types d'opérations qui peuvent bloquer l'interface utilisateur si elles sont exécutées dans le thread principal :

1. **Opérations d'entrée/sortie** :
   - Lecture/écriture de fichiers volumineux
   - Requêtes réseau/web
   - Requêtes de base de données complexes

2. **Calculs intensifs** :
   - Traitement d'images ou de vidéos
   - Calculs mathématiques complexes
   - Analyses de données volumineuses

3. **Attentes explicites** :
   - Appels à `Sleep()`, `WaitFor()`, etc.
   - Boucles d'attente active

## Techniques pour maintenir une interface réactive

### 1. Déplacer les opérations longues dans des threads secondaires

La technique la plus fondamentale consiste à déplacer toutes les opérations longues ou bloquantes dans des threads secondaires. Voici un exemple simple :

```pascal
// Approche qui bloque l'interface (à éviter)
procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  // Cette opération bloque l'interface pendant son exécution
  ProcesserDonnéesVolumineuses;
  ShowMessage('Traitement terminé !');
end;

// Approche réactive (recommandée)
procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  // Désactiver le bouton pour éviter les clics multiples
  ButtonProcess.Enabled := False;

  // Créer et démarrer un thread pour l'opération longue
  TTask.Run(
    procedure
    begin
      // Opération longue exécutée dans un thread secondaire
      ProcesserDonnéesVolumineuses;

      // Mettre à jour l'interface dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonProcess.Enabled := True;
          ShowMessage('Traitement terminé !');
        end
      );
    end
  );
end;
```

### 2. Indiquer la progression des opérations longues

Les utilisateurs apprécient de savoir qu'une opération est en cours et de voir sa progression. Cela réduit l'impression que l'application est bloquée.

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
begin
  // Configurer l'interface pour l'opération
  ButtonDownload.Enabled := False;
  ProgressBar1.Visible := True;
  ProgressBar1.Position := 0;
  LabelStatus.Caption := 'Téléchargement en cours...';

  // Démarrer l'opération dans un thread
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

        // Configurer l'événement de progression
        HTTPClient.OnReceiveData := procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
        begin
          if AContentLength > 0 then
            // Mettre à jour la barre de progression de manière asynchrone
            TThread.Queue(nil,
              procedure
              begin
                ProgressBar1.Position := Round((AReadCount / AContentLength) * 100);
                LabelStatus.Caption := Format('Téléchargement en cours... %d%%',
                                            [ProgressBar1.Position]);
              end
            );
        end;

        // Créer le fichier de destination
        ResponseStream := TFileStream.Create('downloaded_file.dat', fmCreate);
        try
          // Télécharger le fichier
          HTTPRequest.Get('https://exemple.com/fichier.zip', ResponseStream);

          // Mettre à jour l'interface à la fin
          TThread.Synchronize(nil,
            procedure
            begin
              ButtonDownload.Enabled := True;
              ProgressBar1.Visible := False;
              LabelStatus.Caption := 'Téléchargement terminé !';
              ShowMessage('Fichier téléchargé avec succès !');
            end
          );
        finally
          ResponseStream.Free;
        end;
      finally
        HTTPRequest.Free;
        HTTPClient.Free;
      end;
    end
  );
end;
```

### 3. Permettre l'annulation des opérations longues

Les utilisateurs apprécient de pouvoir annuler une opération longue s'ils changent d'avis ou s'ils l'ont lancée par erreur.

```pascal
var
  CancellationTokenSource: TCancellationTokenSource;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  // Préparer l'interface
  ButtonStart.Enabled := False;
  ButtonCancel.Enabled := True;
  ProgressBar1.Position := 0;

  // Créer un token d'annulation
  CancellationTokenSource := TCancellationTokenSource.Create;

  // Démarrer la tâche avec le token d'annulation
  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 100 do
      begin
        // Vérifier si l'opération a été annulée
        if CancellationTokenSource.Token.IsCancellationRequested then
        begin
          // Mettre à jour l'interface pour indiquer l'annulation
          TThread.Synchronize(nil,
            procedure
            begin
              LabelStatus.Caption := 'Opération annulée';
              ButtonStart.Enabled := True;
              ButtonCancel.Enabled := False;
            end
          );
          Exit; // Sortir de la procédure
        end;

        // Simuler une étape de travail
        Sleep(100);

        // Mettre à jour la progression
        TThread.Queue(nil,
          procedure
          begin
            ProgressBar1.Position := i;
            LabelStatus.Caption := Format('Progression : %d%%', [i]);
          end
        );
      end;

      // Opération terminée avec succès
      TThread.Synchronize(nil,
        procedure
        begin
          LabelStatus.Caption := 'Opération terminée';
          ButtonStart.Enabled := True;
          ButtonCancel.Enabled := False;
        end
      );
    end,
    CancellationTokenSource.Token
  );
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
  // Demander l'annulation
  if Assigned(CancellationTokenSource) then
    CancellationTokenSource.Cancel;

  ButtonCancel.Enabled := False;
  LabelStatus.Caption := 'Annulation en cours...';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Libérer le token d'annulation
  if Assigned(CancellationTokenSource) then
    CancellationTokenSource.Free;
end;
```

### 4. Mise à jour incrémentale de l'interface

Pour certaines opérations, vous pouvez afficher les résultats progressivement, au fur et à mesure qu'ils sont disponibles, plutôt que d'attendre la fin de l'opération complète.

```pascal
procedure TForm1.ButtonSearchClick(Sender: TObject);
var
  SearchTerm: string;
  Files: TStringDynArray;
begin
  SearchTerm := EditSearch.Text;
  ListBoxResults.Clear;
  LabelStatus.Caption := 'Recherche en cours...';

  // Récupérer la liste des fichiers à analyser
  Files := TDirectory.GetFiles('C:\Documents', '*.txt', TSearchOption.soAllDirectories);
  LabelStatus.Caption := Format('Recherche dans %d fichiers...', [Length(Files)]);

  // Démarrer la recherche dans un thread
  TTask.Run(
    procedure
    var
      i: Integer;
      FileName: string;
      FileContent: string;
    begin
      for i := 0 to Length(Files) - 1 do
      begin
        FileName := Files[i];

        try
          // Lire le contenu du fichier
          FileContent := TFile.ReadAllText(FileName);

          // Chercher le terme dans le contenu
          if Pos(SearchTerm, FileContent) > 0 then
          begin
            // Ajouter immédiatement ce résultat à la liste
            TThread.Queue(nil,
              procedure
              begin
                ListBoxResults.Items.Add(FileName);
              end
            );
          end;
        except
          // Ignorer les erreurs de lecture de fichier
        end;

        // Mettre à jour la progression
        if i mod 10 = 0 then
        begin
          TThread.Queue(nil,
            procedure
            begin
              LabelStatus.Caption := Format('Recherche... %d/%d fichiers analysés',
                                          [i + 1, Length(Files)]);
            end
          );
        end;
      end;

      // Recherche terminée
      TThread.Synchronize(nil,
        procedure
        begin
          LabelStatus.Caption := Format('Recherche terminée. %d fichiers trouvés.',
                                        [ListBoxResults.Items.Count]);
        end
      );
    end
  );
end;
```

### 5. Throttling des mises à jour de l'interface

Si une opération génère des mises à jour très fréquentes (plusieurs fois par seconde), mettre à jour l'interface à chaque fois peut ralentir l'application. Il est préférable de limiter (throttle) la fréquence des mises à jour.

```pascal
type
  TUpdateThrottler = class
  private
    FLastUpdate: TDateTime;
    FMinInterval: TTimeSpan;
  public
    constructor Create(MinIntervalMS: Integer);
    function ShouldUpdate: Boolean;
  end;

constructor TUpdateThrottler.Create(MinIntervalMS: Integer);
begin
  inherited Create;
  FMinInterval := TTimeSpan.FromMilliseconds(MinIntervalMS);
  FLastUpdate := Now - 1; // Pour garantir la première mise à jour
end;

function TUpdateThrottler.ShouldUpdate: Boolean;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;
  Result := (CurrentTime - FLastUpdate) * 86400000 >= FMinInterval.TotalMilliseconds;
  if Result then
    FLastUpdate := CurrentTime;
end;

// Utilisation
procedure TForm1.ExecuteLongTask;
var
  Throttler: TUpdateThrottler;
begin
  Throttler := TUpdateThrottler.Create(100); // Mettre à jour au maximum 10 fois par seconde
  try
    TTask.Run(
      procedure
      var
        i: Integer;
      begin
        for i := 1 to 10000 do
        begin
          // Traitement...

          // Vérifier s'il faut mettre à jour l'interface
          if Throttler.ShouldUpdate then
          begin
            TThread.Queue(nil,
              procedure
              begin
                LabelStatus.Caption := Format('Traitement... %d/10000', [i]);
                ProgressBar1.Position := i div 100;
              end
            );
          end;
        end;
      end
    );
  finally
    Throttler.Free;
  end;
end;
```

## Améliorer l'expérience utilisateur pendant les opérations longues

### 1. Donner un retour immédiat

Même si l'opération complète prendra du temps, donnez un retour immédiat à l'utilisateur pour confirmer que son action a été prise en compte.

```pascal
procedure TForm1.ButtonGenerateReportClick(Sender: TObject);
begin
  // Retour immédiat
  ButtonGenerateReport.Enabled := False;
  LabelStatus.Caption := 'Génération du rapport en cours...';
  ProgressBar1.Visible := True;

  // Bref délai pour que l'interface se mette à jour
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(50); // Délai court pour laisser l'interface se rafraîchir

      // Démarrer l'opération longue
      TThread.Synchronize(nil, GenerateReportInBackground);
    end
  ).Start;
end;

procedure TForm1.GenerateReportInBackground;
begin
  TTask.Run(
    procedure
    begin
      // Code de génération du rapport...
    end
  );
end;
```

### 2. Afficher des animations ou indicateurs d'activité

Les animations donnent une indication visuelle que l'application est toujours en train de travailler, même si la progression exacte n'est pas connue.

```pascal
procedure TForm1.StartAnimation;
begin
  // Vous pouvez utiliser un TTimer ou un composant d'animation
  Timer1.Enabled := True;

  // Ou utiliser une image animée
  AnimatedImage1.Visible := True;
  AnimatedImage1.Active := True;
end;

procedure TForm1.StopAnimation;
begin
  Timer1.Enabled := False;
  AnimatedImage1.Active := False;
  AnimatedImage1.Visible := False;
end;
```

### 3. Permettre l'interaction avec d'autres parties de l'application

Même pendant une opération longue, l'utilisateur devrait pouvoir interagir avec les autres parties de l'application qui ne sont pas directement affectées.

```pascal
procedure TForm1.ConfigureUIForLongOperation(IsProcessing: Boolean);
begin
  // Désactiver uniquement les contrôles liés à l'opération en cours
  ButtonProcess.Enabled := not IsProcessing;
  ProgressBar1.Visible := IsProcessing;

  // Les autres contrôles restent actifs
  ButtonSettings.Enabled := True;
  MenuFile.Enabled := True;
  // ...etc.
end;
```

## Gestion des erreurs dans les interfaces réactives

Lorsqu'une erreur se produit dans un thread secondaire, elle ne sera pas automatiquement propagée à l'interface utilisateur. Vous devez la capturer et l'afficher explicitement.

```pascal
procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  ButtonProcess.Enabled := False;
  LabelStatus.Caption := 'Traitement en cours...';

  TTask.Run(
    procedure
    begin
      try
        // Opération qui peut générer une exception
        ProcesserDonnées;

        // Traitement terminé avec succès
        TThread.Synchronize(nil,
          procedure
          begin
            ButtonProcess.Enabled := True;
            LabelStatus.Caption := 'Traitement terminé avec succès !';
          end
        );
      except
        on E: Exception do
        begin
          // Capturer et afficher l'erreur
          TThread.Synchronize(nil,
            procedure
            begin
              ButtonProcess.Enabled := True;
              LabelStatus.Caption := 'Erreur de traitement';
              ShowMessage('Une erreur est survenue : ' + E.Message);
            end
          );
        end;
      end;
    end
  );
end;
```

## Exemple complet : explorateur de fichiers réactif

Voici un exemple plus complet d'une application qui reste réactive pendant la recherche de fichiers :

```pascal
type
  TMainForm = class(TForm)
    EditPath: TEdit;
    ButtonBrowse: TButton;
    EditSearchPattern: TEdit;
    ButtonSearch: TButton;
    ButtonCancel: TButton;
    ProgressBar1: TProgressBar;
    LabelStatus: TLabel;
    ListViewFiles: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FCancellationTokenSource: TCancellationTokenSource;
    procedure ConfigureUIForSearch(IsSearching: Boolean);
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCancellationTokenSource := nil;
  ConfigureUIForSearch(False);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCancellationTokenSource) then
    FCancellationTokenSource.Free;
end;

procedure TMainForm.ButtonBrowseClick(Sender: TObject);
var
  SelectDirDialog: TSelectDirectoryDialog;
begin
  SelectDirDialog := TSelectDirectoryDialog.Create(Self);
  try
    if SelectDirDialog.Execute then
      EditPath.Text := SelectDirDialog.FileName;
  finally
    SelectDirDialog.Free;
  end;
end;

procedure TMainForm.ConfigureUIForSearch(IsSearching: Boolean);
begin
  ButtonSearch.Enabled := not IsSearching;
  ButtonCancel.Enabled := IsSearching;
  EditPath.Enabled := not IsSearching;
  EditSearchPattern.Enabled := not IsSearching;
  ButtonBrowse.Enabled := not IsSearching;
  ProgressBar1.Visible := IsSearching;

  if not IsSearching then
  begin
    ProgressBar1.Position := 0;
    LabelStatus.Caption := 'Prêt';
  end;
end;

procedure TMainForm.ButtonSearchClick(Sender: TObject);
var
  SearchPath, SearchPattern: string;
begin
  SearchPath := EditPath.Text;
  SearchPattern := EditSearchPattern.Text;

  if (SearchPath = '') or (not DirectoryExists(SearchPath)) then
  begin
    ShowMessage('Veuillez sélectionner un dossier valide.');
    Exit;
  end;

  if SearchPattern = '' then
    SearchPattern := '*.*';

  // Préparer l'interface
  ListViewFiles.Clear;
  ConfigureUIForSearch(True);
  LabelStatus.Caption := 'Préparation de la recherche...';

  // Créer un nouveau token d'annulation
  if Assigned(FCancellationTokenSource) then
    FCancellationTokenSource.Free;
  FCancellationTokenSource := TCancellationTokenSource.Create;

  // Démarrer la recherche dans un thread
  TTask.Run(
    procedure
    var
      FoundFiles: TStringDynArray;
      FileCount, ProcessedCount, AddedCount: Integer;
      Throttler: TUpdateThrottler;
    begin
      try
        // Afficher un message pendant la recherche
        TThread.Queue(nil,
          procedure
          begin
            LabelStatus.Caption := 'Recherche des fichiers...';
          end
        );

        // Obtenir tous les fichiers correspondant au pattern
        FoundFiles := TDirectory.GetFiles(SearchPath, SearchPattern, TSearchOption.soAllDirectories);
        FileCount := Length(FoundFiles);

        // Mettre à jour l'interface pour indiquer le nombre de fichiers trouvés
        TThread.Queue(nil,
          procedure
          begin
            LabelStatus.Caption := Format('Traitement de %d fichiers...', [FileCount]);
            ProgressBar1.Max := FileCount;
          end
        );

        // Créer un throttler pour limiter les mises à jour de l'interface
        Throttler := TUpdateThrottler.Create(200); // Max 5 updates per second
        try
          ProcessedCount := 0;
          AddedCount := 0;

          // Traiter chaque fichier
          for var FileName in FoundFiles do
          begin
            Inc(ProcessedCount);

            // Vérifier si l'opération a été annulée
            if FCancellationTokenSource.Token.IsCancellationRequested then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  LabelStatus.Caption := 'Recherche annulée.';
                  ConfigureUIForSearch(False);
                end
              );
              Exit;
            end;

            try
              // Obtenir les informations sur le fichier
              var FileInfo := TFileInfo.Create(FileName);
              try
                // Ajouter le fichier à la liste
                Inc(AddedCount);

                TThread.Queue(nil,
                  procedure
                  var
                    Item: TListItem;
                  begin
                    Item := ListViewFiles.Items.Add;
                    Item.Caption := ExtractFileName(FileName);
                    Item.SubItems.Add(ExtractFilePath(FileName));
                    Item.SubItems.Add(FormatFloat('#,##0', FileInfo.Size) + ' octets');
                    Item.SubItems.Add(DateTimeToStr(FileInfo.Modified));
                  end
                );
              finally
                FileInfo.Free;
              end;
            except
              // Ignorer les erreurs individuelles de fichier
            end;

            // Mettre à jour la progression si nécessaire
            if Throttler.ShouldUpdate then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  ProgressBar1.Position := ProcessedCount;
                  LabelStatus.Caption := Format('Traitement... %d/%d fichiers',
                                               [ProcessedCount, FileCount]);
                end
              );
            end;
          end;

          // Recherche terminée
          TThread.Queue(nil,
            procedure
            begin
              LabelStatus.Caption := Format('Recherche terminée. %d fichiers trouvés.', [AddedCount]);
              ConfigureUIForSearch(False);
            end
          );
        finally
          Throttler.Free;
        end;
      except
        on E: Exception do
        begin
          // Gérer les erreurs globales
          TThread.Queue(nil,
            procedure
            begin
              LabelStatus.Caption := 'Erreur pendant la recherche.';
              ShowMessage('Une erreur est survenue : ' + E.Message);
              ConfigureUIForSearch(False);
            end
          );
        end;
      end;
    end,
    FCancellationTokenSource.Token
  );
end;

procedure TMainForm.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FCancellationTokenSource) then
  begin
    FCancellationTokenSource.Cancel;
    LabelStatus.Caption := 'Annulation en cours...';
    ButtonCancel.Enabled := False;
  end;
end;
```

## Bonnes pratiques pour une interface utilisateur réactive

### 1. Planifier dès le début

Identifiez toutes les opérations potentiellement longues dans votre application et planifiez leur exécution en arrière-plan dès la phase de conception.

### 2. Utiliser TThread.Queue pour les mises à jour fréquentes

Pour les mises à jour fréquentes de l'interface, préférez `TThread.Queue` à `TThread.Synchronize`, car il ne bloque pas le thread secondaire.

### 3. Limiter les mises à jour de l'interface

Trop de mises à jour de l'interface peuvent ralentir l'application. Utilisez des techniques de "throttling" pour limiter la fréquence des mises à jour.

### 4. Toujours permettre l'annulation

Les utilisateurs apprécient de pouvoir annuler une opération longue. Implémentez cette fonctionnalité dès que possible.

### 5. Tester sur des machines lentes

Testez votre application sur des machines plus lentes que votre environnement de développement pour identifier les problèmes de réactivité.

### 6. Utiliser des animations subtiles

Les animations subtiles pendant les opérations longues donnent l'impression que l'application est toujours active.

### 7. Maintenir l'état de l'interface cohérent

Assurez-vous que l'interface reste dans un état cohérent, même si une opération est annulée ou échoue. Restaurez toujours l'interface dans un état utilisable.

## Exercice pratique

Créez une application de recherche de texte dans des fichiers qui reste réactive pendant la recherche :

1. Permettre à l'utilisateur de sélectionner un dossier
2. Permettre à l'utilisateur d'entrer un texte à rechercher
3. Rechercher ce texte dans tous les fichiers du dossier (et sous-dossiers)
4. Afficher les résultats au fur et à mesure qu'ils sont trouvés
5. Permettre d'annuler la recherche à tout moment
6. Afficher une barre de progression et des statistiques (fichiers analysés, occurrences trouvées)

Cet exercice vous permettra de mettre en pratique les concepts d'interface utilisateur réactive dans un contexte réel.

## Résumé

- Une interface utilisateur réactive est essentielle pour une bonne expérience utilisateur.
- Le thread principal doit toujours rester libre pour traiter les interactions avec l'interface.
- Déplacez toutes les opérations longues dans des threads secondaires.
- Indiquez la progression des opérations longues.
- Permettez l'annulation des opérations longues.
- Limitez la fréquence des mises à jour de l'interface pour maintenir de bonnes performances.
- Gérez correctement les erreurs qui se produisent dans les threads secondaires.

Dans la prochaine section, nous explorerons des cas d'usage concrets du multithreading dans différents types d'applications.

⏭️ [Cas d'usage concrets](/11-multithreading-et-programmation-asynchrone/08-cas-dusage-concrets.md)
