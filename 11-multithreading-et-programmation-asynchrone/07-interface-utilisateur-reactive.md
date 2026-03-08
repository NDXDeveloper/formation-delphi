🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.7 Interface utilisateur réactive

## Qu'est-ce qu'une interface réactive ?

Une **interface utilisateur réactive** reste fluide et répond immédiatement aux actions de l'utilisateur, même pendant l'exécution d'opérations longues.

### Le problème : L'interface qui se fige

Vous avez probablement déjà vécu cette expérience frustrante :
- Vous cliquez sur un bouton
- L'application "se fige" pendant plusieurs secondes
- Le curseur se transforme en sablier
- Impossible de cliquer ailleurs ou de fermer la fenêtre
- L'application semble "plantée" (même si elle fonctionne en coulisses)

**Pourquoi cela arrive-t-il ?**

### Le thread principal et la boucle de messages

Chaque application Delphi possède un **thread principal** (aussi appelé thread UI) qui :
1. Gère l'affichage de l'interface
2. Reçoit et traite les événements (clics, saisies, mouvements de souris)
3. Redessine les fenêtres

**La règle d'or** : Ce thread doit TOUJOURS être disponible pour traiter les événements.

### Exemple du problème

```pascal
// ❌ MAUVAIS : Bloque l'interface
procedure TForm1.ButtonTelechargerClick(Sender: TObject);  
var  
  i: Integer;
begin
  Label1.Caption := 'Téléchargement en cours...';

  // Opération longue dans le thread principal
  for i := 1 to 1000000 do
  begin
    // Traitement lourd
    EffectuerCalculComplexe(i);
  end;

  // Pendant ces secondes, l'interface est complètement figée !
  Label1.Caption := 'Terminé';
end;
```

**Conséquence** : Pendant l'exécution de la boucle, le thread principal ne peut pas :
- Traiter les clics de souris
- Redessiner l'interface
- Répondre aux touches clavier
- Même fermer l'application !

## Solutions pour garder l'interface réactive

### Solution 1 : Application.ProcessMessages

La méthode la plus simple (mais pas idéale) : permettre au thread principal de traiter les messages pendant une opération longue.

```pascal
// ⚠️ Solution simple mais limitée
procedure TForm1.ButtonTraiterClick(Sender: TObject);  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // Traitement
    TraiterElement(i);

    // Permettre à l'interface de se rafraîchir
    Application.ProcessMessages;

    // Mise à jour de la progression
    ProgressBar1.Position := (i * 100) div 1000;
  end;
end;
```

**Avantages** :
- Simple à mettre en œuvre
- Pas de gestion de threads

**Inconvénients** :
- L'utilisateur peut cliquer plusieurs fois sur le bouton
- Peut causer des comportements imprévisibles
- Ne profite pas des processeurs multi-cœurs

### Solution 2 : Utiliser TTask (RECOMMANDÉ)

La meilleure approche moderne : déporter le travail dans un thread séparé.

```pascal
// ✅ BON : Interface réactive
procedure TForm1.ButtonTraiterClick(Sender: TObject);  
begin  
  // Désactiver le bouton pendant le traitement
  Button1.Enabled := False;
  ProgressBar1.Position := 0;
  Label1.Caption := 'Traitement en cours...';

  // Lancer le traitement dans un thread séparé
  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 1000 do
      begin
        // Traitement lourd dans le thread
        TraiterElement(i);

        // Mise à jour de l'interface (de manière sûre)
        TThread.Queue(nil,
          procedure
          begin
            ProgressBar1.Position := (i * 100) div 1000;
          end
        );
      end;

      // Traitement terminé
      TThread.Queue(nil,
        procedure
        begin
          Label1.Caption := 'Terminé !';
          Button1.Enabled := True;
        end
      );
    end
  );

  // Le code continue immédiatement
  // L'interface reste réactive !
end;
```

## Mise à jour progressive de l'interface

### Barre de progression

Une barre de progression informe l'utilisateur de l'avancement et montre que l'application n'est pas figée.

```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);  
var  
  Total: Integer;
begin
  Total := 5000;
  ProgressBar1.Max := Total;
  ProgressBar1.Position := 0;

  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to Total do
      begin
        // Traitement
        EffectuerCalcul(i);

        // Mettre à jour la barre tous les 50 éléments (pour ne pas surcharger)
        if (i mod 50) = 0 then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ProgressBar1.Position := i;
              Label1.Caption := Format('Progression : %d%%', [(i * 100) div Total]);
            end
          );
        end;
      end;

      // Fin
      TThread.Synchronize(nil,
        procedure
        begin
          ProgressBar1.Position := Total;
          ShowMessage('Calcul terminé !');
        end
      );
    end
  );
end;
```

### Affichage de messages intermédiaires

```pascal
procedure TForm1.ButtonImporterClick(Sender: TObject);  
begin  
  Memo1.Clear;

  TTask.Run(
    procedure
    var
      i: Integer;
      Message: string;
    begin
      for i := 1 to 100 do
      begin
        // Traiter un fichier
        Message := Format('Traitement du fichier %d...', [i]);

        TThread.Queue(nil,
          procedure
          begin
            Memo1.Lines.Add(Message);
          end
        );

        TraiterFichier(i);
        Sleep(100);
      end;

      TThread.Queue(nil,
        procedure
        begin
          Memo1.Lines.Add('');
          Memo1.Lines.Add('=== Import terminé ===');
        end
      );
    end
  );
end;
```

## Annulation des opérations longues

Permettre à l'utilisateur d'annuler une opération améliore grandement l'expérience.

### Avec une variable booléenne

```pascal
type
  TForm1 = class(TForm)
  private
    FAnnuler: Boolean;
  end;

procedure TForm1.ButtonDemarrerClick(Sender: TObject);  
begin  
  FAnnuler := False;
  ButtonDemarrer.Enabled := False;
  ButtonAnnuler.Enabled := True;

  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 10000 do
      begin
        // Vérifier si l'utilisateur a annulé
        if FAnnuler then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Opération annulée par l''utilisateur');
              ButtonDemarrer.Enabled := True;
              ButtonAnnuler.Enabled := False;
            end
          );
          Exit; // Sortir de la boucle
        end;

        // Traitement
        TraiterElement(i);

        // Mise à jour
        if (i mod 100) = 0 then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ProgressBar1.Position := (i * 100) div 10000;
            end
          );
        end;
      end;

      // Terminé normalement
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage('Opération terminée');
          ButtonDemarrer.Enabled := True;
          ButtonAnnuler.Enabled := False;
        end
      );
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  FAnnuler := True;
end;
```

### Avec un token d'annulation

```pascal
type
  TForm1 = class(TForm)
  private
    FTokenSource: ICancellationTokenSource;
  end;

procedure TForm1.ButtonDemarrerClick(Sender: TObject);  
var  
  Token: ICancellationToken;
begin
  // Créer un token d'annulation
  FTokenSource := TCancellationTokenSource.Create;
  Token := FTokenSource.Token;

  ButtonDemarrer.Enabled := False;
  ButtonAnnuler.Enabled := True;

  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 10000 do
      begin
        // Vérifier l'annulation
        if Token.IsCancelled then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Annulé');
              ButtonDemarrer.Enabled := True;
              ButtonAnnuler.Enabled := False;
            end
          );
          Exit;
        end;

        TraiterElement(i);
      end;

      // Terminé
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage('Terminé');
          ButtonDemarrer.Enabled := True;
          ButtonAnnuler.Enabled := False;
        end
      );
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  if Assigned(FTokenSource) then
    FTokenSource.Cancel;
end;
```

## Indicateur d'activité (Activity Indicator)

Pour les opérations dont on ne connaît pas la durée, utilisez un indicateur d'activité.

```pascal
procedure TForm1.ButtonConnecterClick(Sender: TObject);  
begin  
  ActivityIndicator1.Animate := True;
  Label1.Caption := 'Connexion au serveur...';
  ButtonConnecter.Enabled := False;

  TTask.Run(
    procedure
    var
      Succes: Boolean;
    begin
      // Tentative de connexion (durée inconnue)
      Succes := TenterConnexionServeur;

      // Résultat
      TThread.Queue(nil,
        procedure
        begin
          ActivityIndicator1.Animate := False;

          if Succes then
          begin
            Label1.Caption := 'Connecté !';
            ShowMessage('Connexion réussie');
          end
          else
          begin
            Label1.Caption := 'Échec de la connexion';
            ShowMessage('Impossible de se connecter');
            ButtonConnecter.Enabled := True;
          end;
        end
      );
    end
  );
end;
```

## Empêcher les clics multiples

Éviter que l'utilisateur lance plusieurs fois la même opération.

```pascal
type
  TForm1 = class(TForm)
  private
    FEnTraitement: Boolean;
  end;

procedure TForm1.ButtonTraiterClick(Sender: TObject);  
begin  
  // Vérifier si un traitement est déjà en cours
  if FEnTraitement then
  begin
    ShowMessage('Un traitement est déjà en cours');
    Exit;
  end;

  FEnTraitement := True;
  ButtonTraiter.Enabled := False;

  TTask.Run(
    procedure
    begin
      try
        // Traitement long
        EffectuerTraitement;
      finally
        // Toujours réactiver, même en cas d'erreur
        TThread.Queue(nil,
          procedure
          begin
            FEnTraitement := False;
            ButtonTraiter.Enabled := True;
          end
        );
      end;
    end
  );
end;
```

## Exemple complet : Téléchargement avec contrôle total

```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    ButtonAnnuler: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
  private
    FAnnuler: Boolean;
    FEnCours: Boolean;
    procedure TelechargerFichier(const URL, Destination: string);
  end;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  if FEnCours then Exit;

  FEnCours := True;
  FAnnuler := False;
  Button1.Enabled := False;
  ButtonAnnuler.Enabled := True;
  ProgressBar1.Position := 0;

  TelechargerFichier(
    'http://example.com/bigfile.zip',
    'C:\Temp\download.zip'
  );
end;

procedure TForm1.TelechargerFichier(const URL, Destination: string);  
begin  
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      FileStream: TFileStream;
      Response: IHTTPResponse;
      TailleTotal, TailleRecue: Int64;
      Buffer: TBytes;
      BytesLus: Integer;
      Pourcentage: Integer;
      Vitesse: Double;
      Debut: TDateTime;
    begin
      HttpClient := THTTPClient.Create;
      FileStream := nil;

      try
        try
          // Démarrer le téléchargement
          Response := HttpClient.Get(URL);
          TailleTotal := Response.ContentLength;
          TailleRecue := 0;
          Debut := Now;

          // Créer le fichier de destination
          FileStream := TFileStream.Create(Destination, fmCreate);

          // Lire par morceaux
          SetLength(Buffer, 8192); // 8 Ko à la fois

          while not FAnnuler do
          begin
            BytesLus := Response.ContentStream.Read(Buffer[0], Length(Buffer));

            if BytesLus <= 0 then
              Break; // Fin du téléchargement

            // Écrire dans le fichier
            FileStream.Write(Buffer[0], BytesLus);
            Inc(TailleRecue, BytesLus);

            // Calculer la progression
            if TailleTotal > 0 then
              Pourcentage := (TailleRecue * 100) div TailleTotal
            else
              Pourcentage := 0;

            // Calculer la vitesse
            Vitesse := TailleRecue / (1024 * 1024) / ((Now - Debut) * 24 * 60 * 60);

            // Mettre à jour l'interface
            TThread.Queue(nil,
              procedure
              begin
                ProgressBar1.Position := Pourcentage;
                Label1.Caption := Format('Téléchargé : %d Mo / %d Mo',
                  [TailleRecue div (1024 * 1024), TailleTotal div (1024 * 1024)]);
                Label2.Caption := Format('Vitesse : %.2f Mo/s', [Vitesse]);
              end
            );
          end;

          // Vérifier si annulé ou terminé
          if FAnnuler then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ShowMessage('Téléchargement annulé');
              end
            );

            // Supprimer le fichier partiel
            if FileExists(Destination) then
              DeleteFile(Destination);
          end
          else
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ShowMessage('Téléchargement terminé !');
              end
            );
          end;

        except
          on E: Exception do
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ShowMessage('Erreur : ' + E.Message);
              end
            );
          end;
        end;

      finally
        FileStream.Free;
        HttpClient.Free;

        // Réactiver les contrôles
        TThread.Queue(nil,
          procedure
          begin
            Button1.Enabled := True;
            ButtonAnnuler.Enabled := False;
            FEnCours := False;
          end
        );
      end;
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  FAnnuler := True;
end;
```

## Feedback visuel pendant l'attente

### Changer le curseur

```pascal
procedure TForm1.ButtonTraiterClick(Sender: TObject);  
begin  
  Screen.Cursor := crHourGlass; // Sablier

  TTask.Run(
    procedure
    begin
      try
        // Traitement
        EffectuerTraitement;
      finally
        TThread.Queue(nil,
          procedure
          begin
            Screen.Cursor := crDefault; // Retour au curseur normal
          end
        );
      end;
    end
  );
end;
```

### Désactiver temporairement les contrôles

```pascal
procedure TForm1.DesactiverInterface;  
begin  
  Panel1.Enabled := False; // Désactive tous les contrôles du panel
  Cursor := crHourGlass;
end;

procedure TForm1.ReactiverInterface;  
begin  
  Panel1.Enabled := True;
  Cursor := crDefault;
end;

procedure TForm1.ButtonTraiterClick(Sender: TObject);  
begin  
  DesactiverInterface;

  TTask.Run(
    procedure
    begin
      try
        EffectuerTraitement;
      finally
        TThread.Queue(nil, ReactiverInterface);
      end;
    end
  );
end;
```

## Optimisation : Limiter les mises à jour

Mettre à jour l'interface trop fréquemment peut ralentir l'application.

```pascal
// ❌ MAUVAIS : Mise à jour à chaque itération (lent)
for i := 1 to 100000 do  
begin  
  TraiterElement(i);
  TThread.Queue(nil,
    procedure
    begin
      Label1.Caption := IntToStr(i);
    end
  );
end;

// ✅ BON : Mise à jour périodique (rapide)
var
  DerniereMAJ: TDateTime;
begin
  DerniereMAJ := 0;

  for i := 1 to 100000 do
  begin
    TraiterElement(i);

    // Mettre à jour seulement toutes les 100ms
    if MilliSecondsBetween(Now, DerniereMAJ) > 100 then
    begin
      TThread.Queue(nil,
        procedure
        begin
          Label1.Caption := IntToStr(i);
          ProgressBar1.Position := (i * 100) div 100000;
        end
      );
      DerniereMAJ := Now;
    end;
  end;
end;
```

## TThread.Queue vs TThread.Synchronize

Comprendre la différence pour optimiser la réactivité.

```pascal
// TThread.Synchronize : BLOQUE le thread jusqu'à l'exécution
TThread.Synchronize(nil,
  procedure
  begin
    Label1.Caption := 'Message';
  end
);
// Le thread attend ici que l'interface soit mise à jour

// TThread.Queue : NE BLOQUE PAS, continue immédiatement
TThread.Queue(nil,
  procedure
  begin
    Label1.Caption := 'Message';
  end
);
// Le thread continue sans attendre
```

**Recommandation** : Utilisez `Queue` autant que possible pour une meilleure réactivité.

## Bonnes pratiques

### 1. Ne jamais bloquer le thread principal

```pascal
// ❌ MAUVAIS
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  OperationLongue(); // Bloque l'interface
end;

// ✅ BON
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  TTask.Run(
    procedure
    begin
      OperationLongue(); // Dans un thread séparé
    end
  );
end;
```

### 2. Toujours donner du feedback

```pascal
// ✅ BON : L'utilisateur sait ce qui se passe
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  Label1.Caption := 'Traitement en cours...';
  ProgressBar1.Visible := True;

  TTask.Run(
    procedure
    begin
      // ...
    end
  );
end;
```

### 3. Permettre l'annulation pour les opérations longues

```pascal
// ✅ BON : L'utilisateur garde le contrôle
if FAnnuler then Exit;
```

### 4. Réactiver les contrôles, même en cas d'erreur

```pascal
// ✅ BON : Toujours dans un try-finally
try
  EffectuerTraitement;
finally
  TThread.Queue(nil,
    procedure
    begin
      Button1.Enabled := True;
    end
  );
end;
```

### 5. Tester avec des opérations réellement longues

```pascal
// Pour tester, simulez des délais
Sleep(3000); // 3 secondes
```

## Points clés à retenir

- Le **thread principal** doit toujours rester disponible pour l'interface
- Utilisez **TTask.Run** pour déporter le travail lourd dans des threads séparés
- **Application.ProcessMessages** est une solution simple mais limitée
- Toujours utiliser **TThread.Queue** ou **TThread.Synchronize** pour modifier l'interface
- Fournissez un **feedback visuel** (barres de progression, messages)
- Permettez l'**annulation** des opérations longues
- **Désactivez les boutons** pour éviter les clics multiples
- **Limitez la fréquence** des mises à jour d'interface pour les performances
- Préférez **TThread.Queue** à **TThread.Synchronize** pour la réactivité
- Toujours **réactiver les contrôles** dans un bloc finally

Une interface réactive est essentielle pour une bonne expérience utilisateur. Elle donne l'impression d'une application rapide et professionnelle, même si les opérations prennent du temps. Le multithreading avec TTask rend cette réactivité facile à implémenter dans vos applications Delphi.

⏭️ [Cas d'usage concrets](/11-multithreading-et-programmation-asynchrone/08-cas-dusage-concrets.md)
