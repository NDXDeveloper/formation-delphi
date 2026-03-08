🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.5 Tâches asynchrones et callbacks

## Qu'est-ce qu'une tâche asynchrone ?

Une **tâche asynchrone** est une opération qui s'exécute en arrière-plan et qui vous notifie quand elle est terminée, sans bloquer votre application.

### Analogie : Commander au restaurant

**Mode synchrone (bloquant)** :
1. Vous commandez un plat
2. Vous attendez debout devant le comptoir
3. Le cuisinier prépare votre plat
4. Vous recevez votre plat
5. Vous pouvez enfin faire autre chose

**Mode asynchrone (non-bloquant)** :
1. Vous commandez un plat
2. On vous donne un numéro
3. Vous retournez à votre table et discutez avec vos amis (vous êtes libre !)
4. Quand c'est prêt, on vous appelle avec votre numéro
5. Vous allez chercher votre plat

Dans le mode asynchrone, vous n'êtes pas bloqué à attendre !

### Synchrone vs Asynchrone en programmation

```pascal
// MODE SYNCHRONE (bloquant)
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  Label1.Caption := 'Téléchargement en cours...';

  // L'interface se fige pendant 5 secondes !
  TelechargerFichier('http://example.com/data.json');

  Label1.Caption := 'Téléchargement terminé';
end;

// MODE ASYNCHRONE (non-bloquant)
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  Label1.Caption := 'Téléchargement en cours...';

  // Lance le téléchargement et continue immédiatement
  TelechargerFichierAsync('http://example.com/data.json',
    procedure
    begin
      // Ce code s'exécute quand le téléchargement est terminé
      Label1.Caption := 'Téléchargement terminé';
    end
  );

  // L'interface reste réactive !
  // L'utilisateur peut continuer à utiliser l'application
end;
```

## Les callbacks (fonctions de rappel)

Un **callback** (ou fonction de rappel) est une fonction que vous passez à une autre fonction, et qui sera appelée plus tard quand une opération sera terminée.

### Structure d'un callback

```pascal
// Définir un type de callback
type
  TCallbackProcedure = reference to procedure(const Resultat: string);

// Fonction qui accepte un callback
procedure FaireQuelqueChoseAsync(ACallback: TCallbackProcedure);  
begin  
  TTask.Run(
    procedure
    var
      Resultat: string;
    begin
      // Faire un travail long
      Sleep(2000);
      Resultat := 'Travail terminé !';

      // Appeler le callback avec le résultat
      TThread.Queue(nil,
        procedure
        begin
          ACallback(Resultat);
        end
      );
    end
  );
end;

// Utilisation
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  FaireQuelqueChoseAsync(
    procedure(const Resultat: string)
    begin
      // Ce code s'exécute quand c'est terminé
      ShowMessage(Resultat);
    end
  );
end;
```

## Callbacks avec TTask

### Exemple simple : Callback de succès

```pascal
procedure TelechargerAsync(const URL: string; OnTermine: TProc);  
begin  
  TTask.Run(
    procedure
    begin
      // Télécharger le fichier
      TelechargerFichier(URL);

      // Notifier que c'est terminé
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(OnTermine) then
            OnTermine();
        end
      );
    end
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  TelechargerAsync('http://example.com/data.json',
    procedure
    begin
      ShowMessage('Téléchargement terminé !');
    end
  );
end;
```

### Callbacks avec paramètres

```pascal
type
  TResultCallback = reference to procedure(const Donnees: string; Erreur: Boolean);

procedure ChargerDonneesAsync(const URL: string; OnTermine: TResultCallback);  
begin  
  TTask.Run(
    procedure
    var
      Donnees: string;
      ErreurRencontree: Boolean;
    begin
      ErreurRencontree := False;

      try
        // Charger les données
        Donnees := TelechargerContenu(URL);
      except
        on E: Exception do
        begin
          ErreurRencontree := True;
          Donnees := E.Message;
        end;
      end;

      // Appeler le callback
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(OnTermine) then
            OnTermine(Donnees, ErreurRencontree);
        end
      );
    end
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ChargerDonneesAsync('http://example.com/api/users',
    procedure(const Donnees: string; Erreur: Boolean)
    begin
      if Erreur then
        ShowMessage('Erreur : ' + Donnees)
      else
        Memo1.Text := Donnees;
    end
  );
end;
```

## Callbacks multiples : Succès et Échec

Une approche courante consiste à avoir deux callbacks : un pour le succès, un pour l'échec.

```pascal
type
  TSuccessCallback = reference to procedure(const Resultat: string);
  TErrorCallback = reference to procedure(const MessageErreur: string);

procedure ExecuterAsync(
  const Operation: string;
  OnSuccess: TSuccessCallback;
  OnError: TErrorCallback);
begin
  TTask.Run(
    procedure
    var
      Resultat: string;
    begin
      try
        // Exécuter l'opération
        Resultat := FaireOperation(Operation);

        // Succès
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(OnSuccess) then
              OnSuccess(Resultat);
          end
        );
      except
        on E: Exception do
        begin
          // Échec
          TThread.Queue(nil,
            procedure
            begin
              if Assigned(OnError) then
                OnError(E.Message);
            end
          );
        end;
      end;
    end
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ExecuterAsync('Traiter les données',
    // Callback de succès
    procedure(const Resultat: string)
    begin
      ShowMessage('Succès : ' + Resultat);
    end,
    // Callback d'erreur
    procedure(const MessageErreur: string)
    begin
      ShowMessage('Erreur : ' + MessageErreur);
    end
  );
end;
```

## Chaînage de callbacks

Parfois, vous devez exécuter plusieurs opérations asynchrones l'une après l'autre.

### Le problème du "Callback Hell"

```pascal
// ❌ Code difficile à lire (pyramide de doom)
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  OperationAsync1(
    procedure(Resultat1: string)
    begin
      OperationAsync2(Resultat1,
        procedure(Resultat2: string)
        begin
          OperationAsync3(Resultat2,
            procedure(Resultat3: string)
            begin
              OperationAsync4(Resultat3,
                procedure(ResultatFinal: string)
                begin
                  ShowMessage(ResultatFinal);
                end
              );
            end
          );
        end
      );
    end
  );
end;
```

### Solution : Méthodes séparées

```pascal
// ✅ Code plus lisible
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Etape1;
end;

procedure TForm1.Etape1;  
begin  
  OperationAsync1(
    procedure(Resultat1: string)
    begin
      FResultat1 := Resultat1;
      Etape2;
    end
  );
end;

procedure TForm1.Etape2;  
begin  
  OperationAsync2(FResultat1,
    procedure(Resultat2: string)
    begin
      FResultat2 := Resultat2;
      Etape3;
    end
  );
end;

procedure TForm1.Etape3;  
begin  
  OperationAsync3(FResultat2,
    procedure(ResultatFinal: string)
    begin
      ShowMessage(ResultatFinal);
    end
  );
end;
```

## Callbacks avec progression

Pour les opérations longues, il est utile de notifier la progression.

```pascal
type
  TProgressCallback = reference to procedure(Pourcentage: Integer);
  TCompleteCallback = reference to procedure;

procedure TelechargerGrossFichierAsync(
  const URL: string;
  OnProgress: TProgressCallback;
  OnComplete: TCompleteCallback);
begin
  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      // Simuler un téléchargement par morceaux
      for i := 1 to 100 do
      begin
        Sleep(50); // Simuler le téléchargement d'un morceau

        // Notifier la progression
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(OnProgress) then
              OnProgress(i);
          end
        );
      end;

      // Téléchargement terminé
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(OnComplete) then
            OnComplete();
        end
      );
    end
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ProgressBar1.Position := 0;

  TelechargerGrossFichierAsync('http://example.com/bigfile.zip',
    // Callback de progression
    procedure(Pourcentage: Integer)
    begin
      ProgressBar1.Position := Pourcentage;
      Label1.Caption := Format('Téléchargement : %d%%', [Pourcentage]);
    end,
    // Callback de fin
    procedure
    begin
      ShowMessage('Téléchargement terminé !');
    end
  );
end;
```

## Pattern Future et Continuation

Utiliser IFuture avec des continuations permet d'enchaîner des opérations de manière élégante.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Future: IFuture<string>;
begin
  // Première opération asynchrone
  Future := TTask.Future<string>(
    function: string
    begin
      Sleep(2000);
      Result := 'Données chargées';
    end
  );

  // Continuer avec une autre tâche quand la première est terminée
  TTask.Run(
    procedure
    var
      Resultat: string;
    begin
      // Attendre la fin de la première tâche
      Resultat := Future.Value;

      // Traiter le résultat
      Resultat := Resultat + ' et traitées';

      // Mettre à jour l'interface
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage(Resultat);
        end
      );
    end
  );
end;
```

## Exemple pratique : Requête HTTP asynchrone

```pascal
uses
  System.Net.HttpClient;

type
  TForm1 = class(TForm)
  private
    procedure RequeteHTTPAsync(
      const URL: string;
      OnSuccess: TProc<string>;
      OnError: TProc<string>);
  end;

procedure TForm1.RequeteHTTPAsync(
  const URL: string;
  OnSuccess: TProc<string>;
  OnError: TProc<string>);
begin
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      Response: IHTTPResponse;
      Contenu: string;
    begin
      HttpClient := THTTPClient.Create;
      try
        try
          // Faire la requête HTTP
          Response := HttpClient.Get(URL);

          if Response.StatusCode = 200 then
          begin
            Contenu := Response.ContentAsString;

            // Succès
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(OnSuccess) then
                  OnSuccess(Contenu);
              end
            );
          end
          else
          begin
            // Erreur HTTP
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(OnError) then
                  OnError('Erreur HTTP : ' + IntToStr(Response.StatusCode));
              end
            );
          end;
        except
          on E: Exception do
          begin
            // Erreur exception
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(OnError) then
                  OnError('Exception : ' + E.Message);
              end
            );
          end;
        end;
      finally
        HttpClient.Free;
      end;
    end
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Label1.Caption := 'Chargement...';

  RequeteHTTPAsync('https://api.github.com/users/embarcadero',
    // Succès
    procedure(const Contenu: string)
    begin
      Memo1.Text := Contenu;
      Label1.Caption := 'Chargé !';
    end,
    // Erreur
    procedure(const MessageErreur: string)
    begin
      ShowMessage(MessageErreur);
      Label1.Caption := 'Erreur';
    end
  );

  // L'interface reste réactive immédiatement !
end;
```

## Gestion de plusieurs opérations asynchrones

### Exécuter plusieurs tâches et attendre toutes les réponses

```pascal
procedure TForm1.ChargerPlusieursFichiersAsync;  
var  
  Taches: TArray<ITask>;
  Resultats: TArray<string>;
begin
  SetLength(Resultats, 3);
  SetLength(Taches, 3);

  // Lancer 3 téléchargements en parallèle
  Taches[0] := TTask.Run(
    procedure
    begin
      Resultats[0] := TelechargerContenu('http://example.com/file1.txt');
    end
  );

  Taches[1] := TTask.Run(
    procedure
    begin
      Resultats[1] := TelechargerContenu('http://example.com/file2.txt');
    end
  );

  Taches[2] := TTask.Run(
    procedure
    begin
      Resultats[2] := TelechargerContenu('http://example.com/file3.txt');
    end
  );

  // Attendre que toutes les tâches soient terminées
  TTask.Run(
    procedure
    begin
      TTask.WaitForAll(Taches);

      // Toutes les tâches sont terminées
      TThread.Queue(nil,
        procedure
        begin
          Memo1.Lines.Add('Fichier 1 : ' + Resultats[0]);
          Memo1.Lines.Add('Fichier 2 : ' + Resultats[1]);
          Memo1.Lines.Add('Fichier 3 : ' + Resultats[2]);
          ShowMessage('Tous les fichiers sont chargés !');
        end
      );
    end
  );
end;
```

## Annulation avec callbacks

Permettre d'annuler une opération asynchrone en cours.

```pascal
type
  TOperationAsyncHandle = class
  private
    FToken: ICancellationToken;
    FTask: ITask;
  public
    constructor Create(AToken: ICancellationToken; ATask: ITask);
    procedure Cancel;
  end;

constructor TOperationAsyncHandle.Create(AToken: ICancellationToken; ATask: ITask);  
begin  
  FToken := AToken;
  FTask := ATask;
end;

procedure TOperationAsyncHandle.Cancel;  
begin  
  FToken.Cancel;
end;

function ExecuterOperationAnnulable(
  OnProgress: TProc<Integer>;
  OnComplete: TProc): TOperationAsyncHandle;
var
  TokenSource: ICancellationTokenSource;
  Token: ICancellationToken;
  Task: ITask;
begin
  TokenSource := TCancellationTokenSource.Create;
  Token := TokenSource.Token;

  Task := TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 100 do
      begin
        // Vérifier l'annulation
        if Token.IsCancelled then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Opération annulée');
            end
          );
          Exit;
        end;

        Sleep(50);

        // Notifier la progression
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(OnProgress) then
              OnProgress(i);
          end
        );
      end;

      // Terminé
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(OnComplete) then
            OnComplete();
        end
      );
    end
  );

  Result := TOperationAsyncHandle.Create(Token, Task);
end;

// Utilisation
var
  OperationEnCours: TOperationAsyncHandle;

procedure TForm1.ButtonDemarrerClick(Sender: TObject);  
begin  
  OperationEnCours := ExecuterOperationAnnulable(
    // Progression
    procedure(Pourcentage: Integer)
    begin
      ProgressBar1.Position := Pourcentage;
    end,
    // Complet
    procedure
    begin
      ShowMessage('Opération terminée !');
      OperationEnCours := nil;
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  if Assigned(OperationEnCours) then
  begin
    OperationEnCours.Cancel;
    OperationEnCours := nil;
  end;
end;
```

## Bonnes pratiques avec les callbacks

### 1. Toujours vérifier si le callback est assigné

```pascal
// ✅ BON
if Assigned(OnComplete) then
  OnComplete();

// ❌ MAUVAIS (peut planter si OnComplete est nil)
OnComplete();
```

### 2. Utiliser TThread.Queue pour les callbacks qui modifient l'interface

```pascal
// ✅ BON
TThread.Queue(nil,
  procedure
  begin
    OnComplete(); // Sûr pour l'interface
  end
);

// ❌ DANGEREUX (si appelé depuis un thread)
OnComplete(); // Peut planter l'interface
```

### 3. Éviter les captures de variables dangereuses

```pascal
// ❌ DANGEREUX
procedure TForm1.MauvaiseMethode;  
var  
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    TTask.Run(
      procedure
      begin
        // i peut avoir changé !
        ShowMessage(IntToStr(i));
      end
    );
  end;
end;

// ✅ BON
procedure TForm1.BonneMethode;  
var  
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    TTask.Run(
      procedure
      var
        Valeur: Integer;
      begin
        Valeur := i; // Copie locale
        TThread.Queue(nil,
          procedure
          begin
            ShowMessage(IntToStr(Valeur));
          end
        );
      end
    );
  end;
end;
```

### 4. Gérer les erreurs dans les callbacks

```pascal
procedure MonCallback(const Resultat: string);  
begin  
  try
    // Traiter le résultat
    TraiterResultat(Resultat);
  except
    on E: Exception do
    begin
      // Logger ou notifier l'erreur
      ShowMessage('Erreur dans le callback : ' + E.Message);
    end;
  end;
end;
```

### 5. Nettoyer les ressources

Si votre callback capture des objets, assurez-vous qu'ils sont toujours valides :

```pascal
// ❌ DANGEREUX
procedure TForm1.MauvaiseMethode;  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;

  TTask.Run(
    procedure
    begin
      Sleep(2000);
      // Liste peut avoir été libérée entre-temps !
      Liste.Add('Danger');
    end
  );

  Liste.Free; // Libéré avant la fin de la tâche !
end;

// ✅ BON
procedure TForm1.BonneMethode;  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;

  TTask.Run(
    procedure
    begin
      try
        Sleep(2000);

        TThread.Queue(nil,
          procedure
          begin
            Liste.Add('Sûr');
          end
        );
      finally
        Liste.Free; // Libéré dans le thread
      end;
    end
  );
end;
```

## Points clés à retenir

- Les **tâches asynchrones** permettent de garder l'interface réactive pendant les opérations longues
- Les **callbacks** sont des fonctions appelées quand une opération est terminée
- Utilisez `TProc` et `reference to procedure` pour définir des callbacks
- Séparez les callbacks de succès et d'erreur pour une meilleure gestion
- Les callbacks avec progression améliorent l'expérience utilisateur
- Évitez le "callback hell" en décomposant votre code en méthodes
- Toujours vérifier si un callback est assigné avec `Assigned()`
- Utilisez `TThread.Queue` pour les callbacks qui modifient l'interface
- Gérez proprement les erreurs dans vos callbacks
- Faites attention aux captures de variables et à la durée de vie des objets

Les tâches asynchrones et les callbacks sont essentiels pour créer des applications modernes et réactives. Ils permettent d'offrir une excellente expérience utilisateur en évitant que l'interface ne se fige pendant les opérations longues.

⏭️ [Files d'attente et pools de threads](/11-multithreading-et-programmation-asynchrone/06-files-dattente-et-pools-de-threads.md)
