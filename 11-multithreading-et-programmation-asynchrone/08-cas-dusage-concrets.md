🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.8 Cas d'usage concrets

Cette section présente des exemples pratiques et réels d'utilisation du multithreading dans des applications Delphi. Chaque cas d'usage montre comment le multithreading résout un problème concret.

## 1. Téléchargement de fichiers multiples

**Problème** : Télécharger 50 fichiers l'un après l'autre prend beaucoup de temps.

**Solution** : Télécharger plusieurs fichiers simultanément.

```pascal
uses
  System.Threading, System.Net.HttpClient, System.Generics.Collections;

type
  TForm1 = class(TForm)
  private
    FNbTermines: Integer;
    FCS: TCriticalSection;
    procedure TelechargerFichiers(const URLs: TArray<string>);
    procedure TelechargerUnFichier(const URL: string; Index: Integer);
  end;

procedure TForm1.TelechargerFichiers(const URLs: TArray<string>);
var
  i: Integer;
  Taches: TArray<ITask>;
begin
  FNbTermines := 0;
  FCS := TCriticalSection.Create;

  try
    SetLength(Taches, Length(URLs));
    ProgressBar1.Max := Length(URLs);
    ProgressBar1.Position := 0;

    // Lancer le téléchargement de chaque fichier en parallèle
    for i := 0 to High(URLs) do
    begin
      Taches[i] := TTask.Run(
        procedure
        var
          Index: Integer;
        begin
          Index := i; // Capture locale
          TelechargerUnFichier(URLs[Index], Index);
        end
      );
    end;

    // Attendre que tous les téléchargements soient terminés
    TTask.WaitForAll(Taches);

    ShowMessage('Tous les fichiers ont été téléchargés !');

  finally
    FCS.Free;
  end;
end;

procedure TForm1.TelechargerUnFichier(const URL: string; Index: Integer);
var
  HttpClient: THTTPClient;
  NomFichier: string;
begin
  HttpClient := THTTPClient.Create;
  try
    try
      NomFichier := Format('C:\Temp\fichier_%d.dat', [Index]);
      HttpClient.Get(URL).ContentAsStream.SaveToFile(NomFichier);

      // Incrémenter le compteur de manière thread-safe
      FCS.Enter;
      try
        Inc(FNbTermines);
      finally
        FCS.Leave;
      end;

      // Mettre à jour l'interface
      TThread.Queue(nil,
        procedure
        begin
          ProgressBar1.Position := FNbTermines;
          Memo1.Lines.Add(Format('Fichier %d téléchargé', [Index]));
        end
      );

    except
      on E: Exception do
      begin
        TThread.Queue(nil,
          procedure
          begin
            Memo1.Lines.Add(Format('Erreur fichier %d : %s', [Index, E.Message]));
          end
        );
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonTelechargerClick(Sender: TObject);
var
  URLs: TArray<string>;
begin
  URLs := [
    'http://example.com/file1.zip',
    'http://example.com/file2.zip',
    'http://example.com/file3.zip',
    'http://example.com/file4.zip',
    'http://example.com/file5.zip'
  ];

  TelechargerFichiers(URLs);
end;
```

**Avantages** :
- Téléchargement jusqu'à 5× plus rapide (selon la limite de téléchargements parallèles)
- Interface reste réactive
- Progression en temps réel

## 2. Recherche dans une grande base de données

**Problème** : Rechercher dans 1 million d'enregistrements peut prendre plusieurs secondes et bloquer l'interface.

**Solution** : Effectuer la recherche dans un thread séparé.

```pascal
type
  TForm1 = class(TForm)
  private
    FRechercheEnCours: ITask;
    FAnnuler: Boolean;
    procedure RechercherAsync(const Critere: string);
  end;

procedure TForm1.RechercherAsync(const Critere: string);
begin
  // Annuler la recherche précédente si elle existe
  FAnnuler := True;
  if Assigned(FRechercheEnCours) then
    FRechercheEnCours.Wait;

  // Nouvelle recherche
  FAnnuler := False;
  ListView1.Clear;
  Label1.Caption := 'Recherche en cours...';

  FRechercheEnCours := TTask.Run(
    procedure
    var
      Query: TFDQuery;
      NbResultats: Integer;
    begin
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := FDConnection1;
        Query.SQL.Text :=
          'SELECT id, nom, prenom, email FROM clients ' +
          'WHERE nom LIKE :critere OR email LIKE :critere';
        Query.ParamByName('critere').AsString := '%' + Critere + '%';

        Query.Open;
        NbResultats := 0;

        while not Query.Eof do
        begin
          // Vérifier l'annulation
          if FAnnuler then
          begin
            TThread.Queue(nil,
              procedure
              begin
                Label1.Caption := 'Recherche annulée';
              end
            );
            Exit;
          end;

          // Ajouter le résultat à l'interface
          TThread.Queue(nil,
            procedure
            var
              Item: TListItem;
            begin
              Item := ListView1.Items.Add;
              Item.Caption := Query.FieldByName('id').AsString;
              Item.SubItems.Add(Query.FieldByName('nom').AsString);
              Item.SubItems.Add(Query.FieldByName('prenom').AsString);
              Item.SubItems.Add(Query.FieldByName('email').AsString);
            end
          );

          Inc(NbResultats);
          Query.Next;
        end;

        // Résultats finaux
        TThread.Synchronize(nil,
          procedure
          begin
            Label1.Caption := Format('%d résultat(s) trouvé(s)', [NbResultats]);
          end
        );

      finally
        Query.Free;
      end;
    end
  );
end;

// Recherche instantanée pendant la frappe
procedure TForm1.EditRechercheChange(Sender: TObject);
begin
  if Length(EditRecherche.Text) >= 3 then
    RechercherAsync(EditRecherche.Text);
end;
```

**Avantages** :
- L'interface reste fluide pendant la recherche
- Recherche en temps réel pendant la frappe
- Possibilité d'annuler une recherche en cours

## 3. Traitement d'images par lot

**Problème** : Redimensionner 500 photos prend beaucoup de temps.

**Solution** : Traiter plusieurs images simultanément en utilisant tous les cœurs du processeur.

```pascal
procedure TForm1.TraiterImagesParallele(const Dossier: string);
var
  Fichiers: TArray<string>;
  NbTraites: Integer;
  CS: TCriticalSection;
begin
  Fichiers := TDirectory.GetFiles(Dossier, '*.jpg');
  NbTraites := 0;
  CS := TCriticalSection.Create;

  try
    ProgressBar1.Max := Length(Fichiers);
    ProgressBar1.Position := 0;
    Label1.Caption := Format('Traitement de %d images...', [Length(Fichiers)]);

    // Traiter en parallèle (utilise automatiquement tous les cœurs)
    TParallel.For(0, High(Fichiers),
      procedure(Index: Integer)
      var
        Image: TBitmap;
        ImageRedim: TBitmap;
        NomSortie: string;
      begin
        Image := TBitmap.Create;
        ImageRedim := TBitmap.Create;
        try
          // Charger l'image
          Image.LoadFromFile(Fichiers[Index]);

          // Redimensionner (800x600)
          ImageRedim.Width := 800;
          ImageRedim.Height := 600;
          ImageRedim.Canvas.StretchDraw(
            Rect(0, 0, 800, 600),
            Image
          );

          // Sauvegarder
          NomSortie := TPath.Combine(
            TPath.GetDirectoryName(Fichiers[Index]),
            'redim_' + TPath.GetFileName(Fichiers[Index])
          );
          ImageRedim.SaveToFile(NomSortie);

          // Mise à jour de la progression
          CS.Enter;
          try
            Inc(NbTraites);
          finally
            CS.Leave;
          end;

          TThread.Queue(nil,
            procedure
            begin
              ProgressBar1.Position := NbTraites;
              Label2.Caption := ExtractFileName(Fichiers[Index]);
            end
          );

        finally
          Image.Free;
          ImageRedim.Free;
        end;
      end
    );

    ShowMessage('Traitement terminé !');

  finally
    CS.Free;
  end;
end;
```

**Avantages** :
- Jusqu'à 4-8× plus rapide sur processeur multi-cœurs
- Utilisation optimale des ressources
- Code simple avec TParallel.For

## 4. Surveillance de dossier en temps réel

**Problème** : Détecter automatiquement quand de nouveaux fichiers sont ajoutés dans un dossier.

**Solution** : Un thread surveille le dossier en permanence.

```pascal
type
  TThreadSurveillanceDossier = class(TThread)
  private
    FDossier: string;
    FFichiersPrecedents: TStringList;
    procedure VerifierNouveauxFichiers;
    procedure NotifierNouveauFichier(const Fichier: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const ADossier: string);
    destructor Destroy; override;
  end;

constructor TThreadSurveillanceDossier.Create(const ADossier: string);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FDossier := ADossier;
  FFichiersPrecedents := TStringList.Create;
  FFichiersPrecedents.Sorted := True;
  FFichiersPrecedents.Duplicates := dupIgnore;

  // Initialiser la liste avec les fichiers existants
  FFichiersPrecedents.AddStrings(
    TDirectory.GetFiles(FDossier, '*.*', TSearchOption.soTopDirectoryOnly)
  );
end;

destructor TThreadSurveillanceDossier.Destroy;
begin
  FFichiersPrecedents.Free;
  inherited;
end;

procedure TThreadSurveillanceDossier.Execute;
begin
  while not Terminated do
  begin
    VerifierNouveauxFichiers;
    Sleep(2000); // Vérifier toutes les 2 secondes
  end;
end;

procedure TThreadSurveillanceDossier.VerifierNouveauxFichiers;
var
  FichiersActuels: TStringList;
  Fichier: string;
begin
  FichiersActuels := TStringList.Create;
  try
    FichiersActuels.Sorted := True;
    FichiersActuels.AddStrings(
      TDirectory.GetFiles(FDossier, '*.*', TSearchOption.soTopDirectoryOnly)
    );

    // Trouver les nouveaux fichiers
    for Fichier in FichiersActuels do
    begin
      if FFichiersPrecedents.IndexOf(Fichier) = -1 then
      begin
        // Nouveau fichier détecté !
        NotifierNouveauFichier(Fichier);
        FFichiersPrecedents.Add(Fichier);
      end;
    end;

  finally
    FichiersActuels.Free;
  end;
end;

procedure TThreadSurveillanceDossier.NotifierNouveauFichier(const Fichier: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Form1.Memo1.Lines.Add('Nouveau fichier : ' + ExtractFileName(Fichier));

      // Traiter automatiquement le fichier
      Form1.TraiterFichier(Fichier);
    end
  );
end;

// Utilisation
var
  ThreadSurveillance: TThreadSurveillanceDossier;

procedure TForm1.ButtonDemarrerSurveillanceClick(Sender: TObject);
begin
  ThreadSurveillance := TThreadSurveillanceDossier.Create('C:\Uploads');
  Memo1.Lines.Add('Surveillance démarrée...');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(ThreadSurveillance) then
  begin
    ThreadSurveillance.Terminate;
    ThreadSurveillance.WaitFor;
  end;
end;
```

**Avantages** :
- Détection automatique et temps réel
- Ne consomme presque pas de CPU
- Application reste réactive

## 5. Export de données vers Excel

**Problème** : Exporter 50 000 lignes vers Excel peut prendre 30 secondes.

**Solution** : Effectuer l'export dans un thread séparé.

```pascal
procedure TForm1.ExporterVersExcelAsync;
begin
  ButtonExporter.Enabled := False;
  ProgressBar1.Position := 0;
  Label1.Caption := 'Export en cours...';

  TTask.Run(
    procedure
    var
      Excel: Variant;
      Workbook, Worksheet: Variant;
      Query: TFDQuery;
      Ligne, Col: Integer;
    begin
      try
        // Créer Excel (COM)
        Excel := CreateOleObject('Excel.Application');
        Excel.Visible := False;
        Workbook := Excel.Workbooks.Add;
        Worksheet := Workbook.Worksheets[1];

        // Préparer la requête
        Query := TFDQuery.Create(nil);
        try
          Query.Connection := FDConnection1;
          Query.SQL.Text := 'SELECT * FROM ventes ORDER BY date_vente';
          Query.Open;

          // En-têtes
          for Col := 0 to Query.FieldCount - 1 do
            Worksheet.Cells[1, Col + 1] := Query.Fields[Col].FieldName;

          // Données
          Ligne := 2;
          while not Query.Eof do
          begin
            for Col := 0 to Query.FieldCount - 1 do
              Worksheet.Cells[Ligne, Col + 1] := Query.Fields[Col].AsString;

            // Mise à jour de la progression
            if (Ligne mod 100) = 0 then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  ProgressBar1.Position := (Ligne * 100) div Query.RecordCount;
                  Label1.Caption := Format('Export : %d / %d lignes',
                    [Ligne - 1, Query.RecordCount]);
                end
              );
            end;

            Inc(Ligne);
            Query.Next;
          end;

          // Sauvegarder
          Workbook.SaveAs('C:\Exports\ventes_' +
            FormatDateTime('yyyymmdd_hhnnss', Now) + '.xlsx');
          Workbook.Close;

        finally
          Query.Free;
        end;

        Excel.Quit;

        // Succès
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage('Export terminé !');
          end
        );

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

      // Réactiver l'interface
      TThread.Queue(nil,
        procedure
        begin
          ButtonExporter.Enabled := True;
          Label1.Caption := 'Prêt';
        end
      );
    end
  );
end;
```

**Avantages** :
- L'utilisateur peut continuer à travailler pendant l'export
- Progression visible en temps réel
- Possibilité d'annuler si nécessaire

## 6. Validation de formulaire en temps réel

**Problème** : Vérifier si un email existe déjà dans la base de données à chaque frappe.

**Solution** : Validation asynchrone avec délai.

```pascal
type
  TForm1 = class(TForm)
  private
    FDerniereValidation: TDateTime;
    FValidationEnCours: ITask;
    procedure ValiderEmailAsync(const Email: string);
  end;

procedure TForm1.EditEmailChange(Sender: TObject);
begin
  // Attendre 500ms après la dernière frappe
  FDerniereValidation := Now;

  TTask.Run(
    procedure
    var
      Email: string;
    begin
      Email := EditEmail.Text;
      Sleep(500);

      // Vérifier si l'utilisateur continue de taper
      if MilliSecondsBetween(Now, FDerniereValidation) < 500 then
        Exit;

      // Valider
      ValiderEmailAsync(Email);
    end
  );
end;

procedure TForm1.ValiderEmailAsync(const Email: string);
begin
  // Annuler la validation précédente si elle existe
  if Assigned(FValidationEnCours) then
    FValidationEnCours.Wait;

  FValidationEnCours := TTask.Run(
    procedure
    var
      Query: TFDQuery;
      Existe: Boolean;
    begin
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := FDConnection1;
        Query.SQL.Text := 'SELECT COUNT(*) FROM users WHERE email = :email';
        Query.ParamByName('email').AsString := Email;
        Query.Open;

        Existe := Query.Fields[0].AsInteger > 0;

        TThread.Synchronize(nil,
          procedure
          begin
            if Existe then
            begin
              LabelValidation.Caption := '✗ Cet email est déjà utilisé';
              LabelValidation.Font.Color := clRed;
              ButtonInscrire.Enabled := False;
            end
            else
            begin
              LabelValidation.Caption := '✓ Email disponible';
              LabelValidation.Font.Color := clGreen;
              ButtonInscrire.Enabled := True;
            end;
          end
        );

      finally
        Query.Free;
      end;
    end
  );
end;
```

**Avantages** :
- Validation instantanée sans cliquer sur un bouton
- Pas de ralentissement de la frappe
- Meilleure expérience utilisateur

## 7. Envoi d'emails en masse

**Problème** : Envoyer 1000 emails un par un prend trop de temps.

**Solution** : Envoyer plusieurs emails en parallèle avec une file d'attente.

```pascal
type
  TEmailAEnvoyer = record
    Destinataire: string;
    Sujet: string;
    Corps: string;
  end;

procedure TForm1.EnvoyerEmailsEnMasse(const Emails: TArray<TEmailAEnvoyer>);
var
  FileEmails: TThreadedQueue<TEmailAEnvoyer>;
  NbEnvoyes, NbEchecs: Integer;
  CS: TCriticalSection;
  i: Integer;
begin
  FileEmails := TThreadedQueue<TEmailAEnvoyer>.Create;
  CS := TCriticalSection.Create;
  NbEnvoyes := 0;
  NbEchecs := 0;

  try
    // Remplir la file
    for i := 0 to High(Emails) do
      FileEmails.PushItem(Emails[i]);

    // Signal de fin
    var EmailFin: TEmailAEnvoyer;
    EmailFin.Destinataire := 'FIN';
    FileEmails.PushItem(EmailFin);

    ProgressBar1.Max := Length(Emails);
    ProgressBar1.Position := 0;

    // Créer 5 threads d'envoi parallèles
    for i := 1 to 5 do
    begin
      TTask.Run(
        procedure
        var
          Email: TEmailAEnvoyer;
          IdMTP: TIdSMTP;
          Message: TIdMessage;
        begin
          IdMTP := TIdSMTP.Create(nil);
          Message := TIdMessage.Create(nil);
          try
            // Configuration SMTP
            IdMTP.Host := 'smtp.example.com';
            IdMTP.Port := 587;
            // ... autres paramètres ...

            while True do
            begin
              // Récupérer un email de la file
              if FileEmails.PopItem(Email, 1000) = wrSignaled then
              begin
                // Vérifier la fin
                if Email.Destinataire = 'FIN' then
                begin
                  FileEmails.PushItem(Email); // Pour les autres threads
                  Break;
                end;

                try
                  // Envoyer l'email
                  Message.Recipients.Clear;
                  Message.Recipients.Add.Address := Email.Destinataire;
                  Message.Subject := Email.Sujet;
                  Message.Body.Text := Email.Corps;

                  IdMTP.Send(Message);

                  // Succès
                  CS.Enter;
                  try
                    Inc(NbEnvoyes);
                  finally
                    CS.Leave;
                  end;

                except
                  // Échec
                  CS.Enter;
                  try
                    Inc(NbEchecs);
                  finally
                    CS.Leave;
                  end;
                end;

                // Mise à jour
                TThread.Queue(nil,
                  procedure
                  begin
                    ProgressBar1.Position := NbEnvoyes + NbEchecs;
                    Label1.Caption := Format('Envoyés : %d | Échecs : %d',
                      [NbEnvoyes, NbEchecs]);
                  end
                );
              end;
            end;

          finally
            Message.Free;
            IdMTP.Free;
          end;
        end
      );
    end;

  finally
    // Attendre que tous les threads aient fini
    while (NbEnvoyes + NbEchecs) < Length(Emails) do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;

    FileEmails.Free;
    CS.Free;

    ShowMessage(Format('Envoi terminé : %d réussis, %d échecs',
      [NbEnvoyes, NbEchecs]));
  end;
end;
```

**Avantages** :
- 5× plus rapide avec 5 threads parallèles
- Gestion des échecs
- Progression en temps réel

## 8. Mise à jour automatique d'application

**Problème** : Vérifier et télécharger une mise à jour sans bloquer l'application.

**Solution** : Vérification et téléchargement en arrière-plan.

```pascal
procedure TForm1.VerifierMiseAJourAsync;
begin
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      Response: IHTTPResponse;
      VersionServeur, VersionActuelle: string;
    begin
      HttpClient := THTTPClient.Create;
      try
        try
          // Vérifier la version sur le serveur
          Response := HttpClient.Get('http://example.com/version.txt');
          VersionServeur := Response.ContentAsString.Trim;
          VersionActuelle := '1.0.0'; // Version actuelle

          if VersionServeur <> VersionActuelle then
          begin
            // Une mise à jour est disponible
            TThread.Synchronize(nil,
              procedure
              begin
                if MessageDlg(
                  Format('Une nouvelle version (%s) est disponible. ' +
                         'Voulez-vous la télécharger ?', [VersionServeur]),
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                begin
                  TelechargerMiseAJour(VersionServeur);
                end;
              end
            );
          end
          else
          begin
            TThread.Queue(nil,
              procedure
              begin
                StatusBar1.Panels[0].Text := 'Application à jour';
              end
            );
          end;

        except
          on E: Exception do
          begin
            // Erreur silencieuse, ne pas déranger l'utilisateur
          end;
        end;
      finally
        HttpClient.Free;
      end;
    end
  );
end;

procedure TForm1.TelechargerMiseAJour(const Version: string);
begin
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      FileStream: TFileStream;
      URL, CheminLocal: string;
    begin
      URL := Format('http://example.com/updates/setup_%s.exe', [Version]);
      CheminLocal := TPath.Combine(TPath.GetTempPath, 'update.exe');

      HttpClient := THTTPClient.Create;
      FileStream := nil;
      try
        try
          FileStream := TFileStream.Create(CheminLocal, fmCreate);

          // Télécharger avec progression
          HttpClient.Get(URL).ContentStream.Position := 0;
          FileStream.CopyFrom(
            HttpClient.Get(URL).ContentStream,
            HttpClient.Get(URL).ContentLength
          );

          // Téléchargement terminé
          TThread.Synchronize(nil,
            procedure
            begin
              if MessageDlg(
                'Mise à jour téléchargée. Installer maintenant ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
              begin
                // Lancer l'installateur
                ShellExecute(0, 'open', PChar(CheminLocal), nil, nil, SW_SHOWNORMAL);
                Application.Terminate;
              end;
            end
          );

        except
          on E: Exception do
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ShowMessage('Erreur lors du téléchargement : ' + E.Message);
              end
            );
          end;
        end;
      finally
        FileStream.Free;
        HttpClient.Free;
      end;
    end
  );
end;

// Vérifier au démarrage de l'application
procedure TForm1.FormCreate(Sender: TObject);
begin
  VerifierMiseAJourAsync;
end;
```

**Avantages** :
- Vérification transparente en arrière-plan
- Ne dérange pas l'utilisateur si pas nécessaire
- Téléchargement sans bloquer l'application

## 9. Compression de fichiers en arrière-plan

**Problème** : Compresser un gros fichier peut prendre plusieurs minutes.

**Solution** : Compression dans un thread avec possibilité d'annulation.

```pascal
type
  TForm1 = class(TForm)
  private
    FAnnulerCompression: Boolean;
    procedure CompresserFichierAsync(const FichierSource, FichierDest: string);
  end;

procedure TForm1.CompresserFichierAsync(const FichierSource, FichierDest: string);
begin
  FAnnulerCompression := False;
  ButtonCompressi.Enabled := False;
  ButtonAnnuler.Enabled := True;
  ProgressBar1.Position := 0;

  TTask.Run(
    procedure
    var
      SourceStream, DestStream: TFileStream;
      CompStream: TZCompressionStream;
      Buffer: array[0..8191] of Byte;
      TailleTotal, TailleLue: Int64;
      BytesLus: Integer;
    begin
      SourceStream := nil;
      DestStream := nil;
      CompStream := nil;

      try
        try
          SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
          DestStream := TFileStream.Create(FichierDest, fmCreate);
          CompStream := TZCompressionStream.Create(DestStream, zcMax);

          TailleTotal := SourceStream.Size;
          TailleLue := 0;

          // Compresser par blocs
          while TailleLue < TailleTotal do
          begin
            // Vérifier l'annulation
            if FAnnulerCompression then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  ShowMessage('Compression annulée');
                  DeleteFile(FichierDest); // Supprimer le fichier partiel
                end
              );
              Exit;
            end;

            // Lire et compresser
            BytesLus := SourceStream.Read(Buffer, SizeOf(Buffer));
            if BytesLus > 0 then
            begin
              CompStream.Write(Buffer, BytesLus);
              Inc(TailleLue, BytesLus);

              // Mise à jour de la progression
              TThread.Queue(nil,
                procedure
                var
                  Pourcentage: Integer;
                begin
                  Pourcentage := (TailleLue * 100) div TailleTotal;
                  ProgressBar1.Position := Pourcentage;
                  Label1.Caption := Format('Compression : %d%%', [Pourcentage]);
                end
              );
            end;
          end;

          // Terminé
          TThread.Synchronize(nil,
            procedure
            var
              TauxCompression: Double;
            begin
              TauxCompression := (1 - (DestStream.Size / TailleTotal)) * 100;
              ShowMessage(Format('Compression terminée !%s' +
                'Taille originale : %s%s' +
                'Taille compressée : %s%s' +
                'Taux de compression : %.1f%%',
                [sLineBreak,
                 FormatFloat('#,##0', TailleTotal), sLineBreak,
                 FormatFloat('#,##0', DestStream.Size), sLineBreak,
                 TauxCompression]));
            end
          );

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
        CompStream.Free;
        DestStream.Free;
        SourceStream.Free;

        TThread.Queue(nil,
          procedure
          begin
            ButtonCompressi.Enabled := True;
            ButtonAnnuler.Enabled := False;
          end
        );
      end;
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);
begin
  FAnnulerCompression := True;
end;
```

**Avantages** :
- Application reste utilisable pendant la compression
- Possibilité d'annuler
- Feedback détaillé sur la progression

## Points clés à retenir

- Le **multithreading** est essentiel pour les **opérations longues** (téléchargements, traitements, exports)
- **TTask.Run** est la solution la plus simple pour la majorité des cas
- **TParallel.For** optimise automatiquement les boucles sur processeurs multi-cœurs
- Les **files d'attente** (TThreadedQueue) permettent le pattern producteur-consommateur
- Toujours donner un **feedback visuel** (progression, messages) pendant les opérations
- Permettre l'**annulation** des opérations longues améliore l'expérience utilisateur
- La **validation asynchrone** rend les formulaires plus réactifs
- Les **threads de surveillance** détectent automatiquement les changements
- Protégez les **compteurs partagés** avec TCriticalSection
- Utilisez **TThread.Queue** pour mettre à jour l'interface depuis les threads

Ces exemples montrent que le multithreading n'est pas réservé aux experts : avec TTask et les outils modernes de Delphi, vous pouvez facilement rendre vos applications plus rapides, plus réactives et plus professionnelles.

⏭️ [Programmation réactive avec le pattern Observer](/11-multithreading-et-programmation-asynchrone/09-programmation-reactive-avec-pattern-observer.md)
