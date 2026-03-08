🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.6 Traitement par lots (Batch)

## Introduction

Le traitement par lots (ou "batch processing" en anglais) consiste à exécuter automatiquement une série d'opérations sur plusieurs fichiers ou données sans intervention manuelle. C'est l'équivalent informatique d'une chaîne de production automatisée.

**Analogie simple :** Imaginez que vous devez trier 1000 photos :
- **Manuellement** : vous ouvrez chaque photo une par une, la renommez, la redimensionnez, puis la sauvegardez. Très long et fastidieux !
- **Par lots** : vous écrivez un programme qui fait tout automatiquement pendant que vous prenez un café ☕

Le traitement par lots est un outil puissant pour automatiser les tâches répétitives et gagner un temps considérable.

## Concepts fondamentaux

### Qu'est-ce que le traitement par lots ?

Le traitement par lots permet de :
- **Traiter plusieurs fichiers** en une seule opération
- **Automatiser des tâches répétitives** (renommage, conversion, traitement)
- **Exécuter des opérations planifiées** (sauvegardes, nettoyage)
- **Appliquer les mêmes transformations** à de nombreux éléments

### Composants d'un traitement par lots

1. **Source** : Liste des fichiers ou données à traiter
2. **Traitement** : Opération(s) à effectuer sur chaque élément
3. **Destination** : Où stocker les résultats
4. **Gestion des erreurs** : Que faire en cas d'échec
5. **Progression** : Informer l'utilisateur de l'avancement
6. **Journal (Log)** : Enregistrer ce qui a été fait

---

## Opérations de base sur les fichiers

### Lister les fichiers d'un dossier

```pascal
uses
  System.IOUtils, System.SysUtils, System.Classes;

// Lister tous les fichiers d'un dossier
procedure ListerFichiers(const Dossier: string; Liste: TStrings);  
var  
  Fichiers: TStringDynArray;
  Fichier: string;
begin
  Liste.Clear;

  // Récupérer tous les fichiers
  Fichiers := TDirectory.GetFiles(Dossier);

  for Fichier in Fichiers do
    Liste.Add(Fichier);
end;

// Lister avec un filtre (exemple : seulement les .txt)
procedure ListerFichiersAvecFiltre(const Dossier, Filtre: string;
                                   Liste: TStrings);
var
  Fichiers: TStringDynArray;
  Fichier: string;
begin
  Liste.Clear;

  // Récupérer les fichiers correspondant au filtre
  Fichiers := TDirectory.GetFiles(Dossier, Filtre);

  for Fichier in Fichiers do
    Liste.Add(Fichier);
end;

// Lister récursivement (inclut les sous-dossiers)
procedure ListerFichiersRecursif(const Dossier, Filtre: string;
                                 Liste: TStrings);
var
  Fichiers: TStringDynArray;
  Fichier: string;
begin
  Liste.Clear;

  // TSearchOption.soAllDirectories = recherche récursive
  Fichiers := TDirectory.GetFiles(Dossier, Filtre,
                                  TSearchOption.soAllDirectories);

  for Fichier in Fichiers do
    Liste.Add(Fichier);
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  // Lister tous les fichiers .txt
  ListerFichiersAvecFiltre('C:\Documents', '*.txt', Memo1.Lines);

  ShowMessage(Format('%d fichiers trouvés', [Memo1.Lines.Count]));
end;
```

### Filtrer par critères multiples

```pascal
function FiltrerFichiers(const Fichiers: TStringList;
                         TailleMin, TailleMax: Int64;
                         const Extensions: array of string): TStringList;
var
  i: Integer;
  Fichier, Extension: string;
  Taille: Int64;
  ExtensionValide: Boolean;
  Ext: string;
begin
  Result := TStringList.Create;

  for i := 0 to Fichiers.Count - 1 do
  begin
    Fichier := Fichiers[i];

    // Vérifier la taille
    Taille := TFile.GetSize(Fichier);
    if (Taille < TailleMin) or (Taille > TailleMax) then
      Continue;

    // Vérifier l'extension
    Extension := LowerCase(ExtractFileExt(Fichier));
    ExtensionValide := False;

    for Ext in Extensions do
    begin
      if Extension = LowerCase(Ext) then
      begin
        ExtensionValide := True;
        Break;
      end;
    end;

    if ExtensionValide then
      Result.Add(Fichier);
  end;
end;

// Utilisation
procedure TForm1.FiltrerFichiersImages;  
var  
  TousFichiers, FichiersFiltres: TStringList;
begin
  TousFichiers := TStringList.Create;
  try
    ListerFichiersRecursif('C:\Photos', '*.*', TousFichiers);

    // Filtrer : entre 100 Ko et 10 Mo, seulement images
    FichiersFiltres := FiltrerFichiers(TousFichiers,
                                       100 * 1024,      // 100 Ko min
                                       10 * 1024 * 1024, // 10 Mo max
                                       ['.jpg', '.jpeg', '.png', '.bmp']);
    try
      Memo1.Lines.Assign(FichiersFiltres);
    finally
      FichiersFiltres.Free;
    end;
  finally
    TousFichiers.Free;
  end;
end;
```

---

## Traitement simple par lots

### Structure de base d'un traitement par lots

```pascal
type
  TResultatTraitement = record
    NomFichier: string;
    Reussi: Boolean;
    MessageErreur: string;
  end;

function TraiterFichiersParLots(const Fichiers: TStringList): TArray<TResultatTraitement>;  
var  
  i: Integer;
  Fichier: string;
begin
  SetLength(Result, Fichiers.Count);

  for i := 0 to Fichiers.Count - 1 do
  begin
    Fichier := Fichiers[i];
    Result[i].NomFichier := ExtractFileName(Fichier);

    try
      // VOTRE TRAITEMENT ICI
      // Exemple : TraiterFichier(Fichier);

      Result[i].Reussi := True;
      Result[i].MessageErreur := '';
    except
      on E: Exception do
      begin
        Result[i].Reussi := False;
        Result[i].MessageErreur := E.Message;
      end;
    end;
  end;
end;
```

### Exemple 1 : Renommer des fichiers en masse

```pascal
function RenommerFichiersEnMasse(const Fichiers: TStringList;
                                 const Prefixe: string;
                                 CommencerA: Integer): Integer;
var
  i: Integer;
  FichierSource, FichierDest: string;
  Extension, NouveauNom: string;
begin
  Result := 0; // Compteur de fichiers renommés

  for i := 0 to Fichiers.Count - 1 do
  begin
    FichierSource := Fichiers[i];
    Extension := ExtractFileExt(FichierSource);

    // Créer le nouveau nom : Prefixe_001.ext, Prefixe_002.ext, etc.
    NouveauNom := Format('%s_%s%s',
                        [Prefixe,
                         FormatFloat('000', CommencerA + i),
                         Extension]);

    FichierDest := ExtractFilePath(FichierSource) + NouveauNom;

    try
      // Vérifier que le fichier destination n'existe pas déjà
      if not FileExists(FichierDest) then
      begin
        RenameFile(FichierSource, FichierDest);
        Inc(Result);
      end;
    except
      on E: Exception do
        // Log l'erreur mais continue avec les autres fichiers
        WriteLn('Erreur lors du renommage de ' + FichierSource + ': ' + E.Message);
    end;
  end;
end;

// Utilisation
procedure TForm1.RenommerPhotos;  
var  
  Fichiers: TStringList;
  NombreRenommes: Integer;
begin
  Fichiers := TStringList.Create;
  try
    // Lister les photos
    ListerFichiersAvecFiltre('C:\Photos\Vacances', '*.jpg', Fichiers);

    // Renommer : Vacances_001.jpg, Vacances_002.jpg, etc.
    NombreRenommes := RenommerFichiersEnMasse(Fichiers, 'Vacances', 1);

    ShowMessage(Format('%d fichiers renommés sur %d',
      [NombreRenommes, Fichiers.Count]));
  finally
    Fichiers.Free;
  end;
end;
```

### Exemple 2 : Copier des fichiers par lots

```pascal
procedure CopierFichiersParLots(const FichiersSource: TStringList;
                                const DossierDestination: string;
                                EcraserExistants: Boolean);
var
  i: Integer;
  FichierSource, FichierDest: string;
  NomFichier: string;
begin
  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(DossierDestination) then
    ForceDirectories(DossierDestination);

  for i := 0 to FichiersSource.Count - 1 do
  begin
    FichierSource := FichiersSource[i];
    NomFichier := ExtractFileName(FichierSource);
    FichierDest := IncludeTrailingPathDelimiter(DossierDestination) + NomFichier;

    try
      TFile.Copy(FichierSource, FichierDest, EcraserExistants);
    except
      on E: Exception do
        ShowMessage('Erreur lors de la copie de ' + NomFichier + ': ' + E.Message);
    end;
  end;
end;
```

### Exemple 3 : Déplacer des fichiers par type

```pascal
procedure OrganiserFichiersParType(const DossierSource: string);  
var  
  Fichiers: TStringDynArray;
  Fichier, Extension, DossierDest: string;
begin
  Fichiers := TDirectory.GetFiles(DossierSource);

  for Fichier in Fichiers do
  begin
    Extension := ExtractFileExt(Fichier);
    Extension := Copy(Extension, 2, Length(Extension)); // Enlever le point

    if Extension = '' then
      Extension := 'Sans_extension';

    // Créer un sous-dossier par type
    DossierDest := IncludeTrailingPathDelimiter(DossierSource) +
                   UpperCase(Extension) + '\';

    if not DirectoryExists(DossierDest) then
      ForceDirectories(DossierDest);

    try
      // Déplacer le fichier
      TFile.Move(Fichier, DossierDest + ExtractFileName(Fichier));
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;

// Utilisation
procedure TForm1.Button2Click(Sender: TObject);  
begin  
  OrganiserFichiersParType('C:\Telechargements');
  ShowMessage('Fichiers organisés par type');
end;
```

---

## Traitement avec progression

### Affichage de la progression avec TProgressBar

```pascal
procedure TraiterAvecProgression(const Fichiers: TStringList;
                                 ProgressBar: TProgressBar;
                                 LabelStatus: TLabel);
var
  i: Integer;
  Fichier: string;
begin
  ProgressBar.Max := Fichiers.Count;
  ProgressBar.Position := 0;

  for i := 0 to Fichiers.Count - 1 do
  begin
    Fichier := Fichiers[i];

    // Mettre à jour le label
    LabelStatus.Caption := Format('Traitement de %s... (%d/%d)',
      [ExtractFileName(Fichier), i + 1, Fichiers.Count]);

    // Mettre à jour la barre de progression
    ProgressBar.Position := i + 1;

    // Permettre à l'interface de se rafraîchir
    Application.ProcessMessages;

    try
      // VOTRE TRAITEMENT ICI
      // Exemple : TraiterFichier(Fichier);
      Sleep(100); // Simulation d'un traitement
    except
      on E: Exception do
        // Gérer l'erreur
    end;
  end;

  LabelStatus.Caption := 'Traitement terminé';
end;
```

### Pourcentage et temps estimé

```pascal
procedure TraiterAvecInfosDetaillees(const Fichiers: TStringList;
                                     ProgressBar: TProgressBar;
                                     LabelStatus: TLabel);
var
  i: Integer;
  Fichier: string;
  HeureDebut, HeureCourante: TDateTime;
  TempsEcoule, TempsEstime: Double;
  Pourcentage: Integer;
begin
  HeureDebut := Now;
  ProgressBar.Max := Fichiers.Count;
  ProgressBar.Position := 0;

  for i := 0 to Fichiers.Count - 1 do
  begin
    Fichier := Fichiers[i];

    // Calculer les statistiques
    Pourcentage := Round((i / Fichiers.Count) * 100);
    HeureCourante := Now;
    TempsEcoule := (HeureCourante - HeureDebut) * 24 * 60 * 60; // en secondes

    if i > 0 then
    begin
      TempsEstime := (TempsEcoule / i) * (Fichiers.Count - i);

      LabelStatus.Caption := Format(
        'Fichier %d/%d (%d%%) - Temps restant estimé : %s',
        [i + 1,
         Fichiers.Count,
         Pourcentage,
         FormatDateTime('nn:ss', TempsEstime / 86400)]);
    end
    else
      LabelStatus.Caption := Format('Fichier %d/%d (%d%%)',
        [i + 1, Fichiers.Count, Pourcentage]);

    ProgressBar.Position := i + 1;
    Application.ProcessMessages;

    try
      // Traiter le fichier
      TraiterFichier(Fichier);
    except
      on E: Exception do
        // Gérer l'erreur
    end;
  end;

  LabelStatus.Caption := Format('Terminé en %s',
    [FormatDateTime('nn:ss', (Now - HeureDebut) * 24 * 60 * 60 / 86400)]);
end;

// Fonction de traitement (exemple)
procedure TraiterFichier(const NomFichier: string);  
begin  
  // Votre logique de traitement ici
  Sleep(50); // Simulation
end;
```

---

## Système de journalisation (Log)

### Classe TBatchLogger simple

```pascal
type
  TBatchLogger = class
  private
    FLogFile: string;
    FLogLines: TStringList;
  public
    constructor Create(const LogFileName: string);
    destructor Destroy; override;

    procedure LogInfo(const Message: string);
    procedure LogWarning(const Message: string);
    procedure LogError(const Message: string);
    procedure LogSuccess(const Message: string);

    procedure SaveToFile;
    procedure Clear;
  end;

constructor TBatchLogger.Create(const LogFileName: string);  
begin  
  inherited Create;
  FLogFile := LogFileName;
  FLogLines := TStringList.Create;

  // En-tête du log
  FLogLines.Add('=== TRAITEMENT PAR LOTS ===');
  FLogLines.Add('Début : ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FLogLines.Add('');
end;

destructor TBatchLogger.Destroy;  
begin  
  SaveToFile;
  FLogLines.Free;
  inherited;
end;

procedure TBatchLogger.LogInfo(const Message: string);  
begin  
  FLogLines.Add(Format('[INFO] %s - %s',
    [FormatDateTime('hh:nn:ss', Now), Message]));
end;

procedure TBatchLogger.LogWarning(const Message: string);  
begin  
  FLogLines.Add(Format('[ATTENTION] %s - %s',
    [FormatDateTime('hh:nn:ss', Now), Message]));
end;

procedure TBatchLogger.LogError(const Message: string);  
begin  
  FLogLines.Add(Format('[ERREUR] %s - %s',
    [FormatDateTime('hh:nn:ss', Now), Message]));
end;

procedure TBatchLogger.LogSuccess(const Message: string);  
begin  
  FLogLines.Add(Format('[SUCCÈS] %s - %s',
    [FormatDateTime('hh:nn:ss', Now), Message]));
end;

procedure TBatchLogger.SaveToFile;  
begin  
  FLogLines.Add('');
  FLogLines.Add('Fin : ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FLogLines.SaveToFile(FLogFile);
end;

procedure TBatchLogger.Clear;  
begin  
  FLogLines.Clear;
end;

// Utilisation
procedure TraiterAvecLog(const Fichiers: TStringList);  
var  
  Logger: TBatchLogger;
  i: Integer;
  Fichier: string;
  Reussis, Echecs: Integer;
begin
  Logger := TBatchLogger.Create('traitement_' +
    FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');
  try
    Logger.LogInfo(Format('Début du traitement de %d fichiers',
      [Fichiers.Count]));

    Reussis := 0;
    Echecs := 0;

    for i := 0 to Fichiers.Count - 1 do
    begin
      Fichier := Fichiers[i];

      try
        // Traiter le fichier
        TraiterFichier(Fichier);

        Logger.LogSuccess('Traité : ' + ExtractFileName(Fichier));
        Inc(Reussis);
      except
        on E: Exception do
        begin
          Logger.LogError(ExtractFileName(Fichier) + ' : ' + E.Message);
          Inc(Echecs);
        end;
      end;
    end;

    Logger.LogInfo(Format('Traitement terminé : %d réussis, %d échecs',
      [Reussis, Echecs]));
  finally
    Logger.Free;
  end;
end;
```

---

## Gestion des erreurs avancée

### Stratégies de gestion d'erreurs

```pascal
type
  TErrorHandlingStrategy = (
    ehStopOnFirstError,      // Arrêter au premier échec
    ehContinueOnError,       // Continuer malgré les erreurs
    ehRetryOnError,          // Réessayer en cas d'échec
    ehSkipOnError           // Ignorer et passer au suivant
  );

  TBatchProcessor = class
  private
    FErrorStrategy: TErrorHandlingStrategy;
    FMaxRetries: Integer;
    FLogger: TBatchLogger;

    function TryProcessFile(const FileName: string;
                           var ErrorMsg: string): Boolean;
  public
    constructor Create(Logger: TBatchLogger);

    procedure ProcessFiles(const Files: TStringList);

    property ErrorStrategy: TErrorHandlingStrategy
      read FErrorStrategy write FErrorStrategy;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
  end;

constructor TBatchProcessor.Create(Logger: TBatchLogger);  
begin  
  inherited Create;
  FLogger := Logger;
  FErrorStrategy := ehContinueOnError;
  FMaxRetries := 3;
end;

function TBatchProcessor.TryProcessFile(const FileName: string;
                                        var ErrorMsg: string): Boolean;
var
  Retry: Integer;
begin
  Result := False;
  ErrorMsg := '';
  Retry := 0;

  repeat
    try
      // Traiter le fichier
      TraiterFichier(FileName);
      Result := True;
      Break;
    except
      on E: Exception do
      begin
        ErrorMsg := E.Message;
        Inc(Retry);

        if (FErrorStrategy = ehRetryOnError) and (Retry < FMaxRetries) then
        begin
          FLogger.LogWarning(Format('Échec tentative %d/%d pour %s : %s',
            [Retry, FMaxRetries, ExtractFileName(FileName), E.Message]));
          Sleep(1000); // Attendre 1 seconde avant de réessayer
        end
        else
          Break;
      end;
    end;
  until Retry >= FMaxRetries;
end;

procedure TBatchProcessor.ProcessFiles(const Files: TStringList);  
var  
  i: Integer;
  FileName, ErrorMsg: string;
  Success: Boolean;
begin
  for i := 0 to Files.Count - 1 do
  begin
    FileName := Files[i];

    Success := TryProcessFile(FileName, ErrorMsg);

    if Success then
      FLogger.LogSuccess('Traité : ' + ExtractFileName(FileName))
    else
    begin
      FLogger.LogError(ExtractFileName(FileName) + ' : ' + ErrorMsg);

      case FErrorStrategy of
        ehStopOnFirstError:
          begin
            FLogger.LogError('Arrêt du traitement sur erreur');
            Break;
          end;
        ehContinueOnError:
          Continue; // Passer au suivant
        ehSkipOnError:
          Continue; // Passer au suivant
      end;
    end;
  end;
end;
```

---

## Traitement parallèle (multithreading)

### Traitement simple en arrière-plan

```pascal
uses
  System.Threading;

procedure TraiterEnArrierePlan(const Fichiers: TStringList);  
begin  
  TTask.Run(
    procedure
    var
      i: Integer;
      Fichier: string;
    begin
      for i := 0 to Fichiers.Count - 1 do
      begin
        Fichier := Fichiers[i];

        try
          TraiterFichier(Fichier);
        except
          on E: Exception do
            // Gérer l'erreur
        end;
      end;

      // Notifier la fin (dans le thread principal)
      TThread.Synchronize(nil,
        procedure
        begin
          ShowMessage('Traitement terminé');
        end);
    end);
end;
```

### Traitement parallèle avec TParallel.For

```pascal
procedure TraiterEnParallele(const Fichiers: TStringList);  
begin  
  TParallel.For(0, Fichiers.Count - 1,
    procedure(Index: Integer)
    var
      Fichier: string;
    begin
      Fichier := Fichiers[Index];

      try
        TraiterFichier(Fichier);
      except
        on E: Exception do
          // Gérer l'erreur (attention au thread-safety)
      end;
    end);
end;
```

### Classe complète pour traitement asynchrone

```pascal
type
  TAsyncBatchProcessor = class
  private
    FFichiers: TStringList;
    FProgressBar: TProgressBar;
    FLabelStatus: TLabel;
    FLogger: TBatchLogger;
    FTask: ITask;

    procedure UpdateProgress(Current, Total: Integer);
    procedure ProcessComplete;
  public
    constructor Create(ProgressBar: TProgressBar; LabelStatus: TLabel);
    destructor Destroy; override;

    procedure StartProcessing(const Files: TStringList);
    procedure CancelProcessing;
    function IsProcessing: Boolean;
  end;

constructor TAsyncBatchProcessor.Create(ProgressBar: TProgressBar;
                                        LabelStatus: TLabel);
begin
  inherited Create;
  FProgressBar := ProgressBar;
  FLabelStatus := LabelStatus;
  FFichiers := TStringList.Create;
end;

destructor TAsyncBatchProcessor.Destroy;  
begin  
  FFichiers.Free;
  inherited;
end;

procedure TAsyncBatchProcessor.UpdateProgress(Current, Total: Integer);  
begin  
  TThread.Synchronize(nil,
    procedure
    begin
      FProgressBar.Max := Total;
      FProgressBar.Position := Current;
      FLabelStatus.Caption := Format('Traitement %d/%d', [Current, Total]);
    end);
end;

procedure TAsyncBatchProcessor.ProcessComplete;  
begin  
  TThread.Synchronize(nil,
    procedure
    begin
      FLabelStatus.Caption := 'Traitement terminé';
      ShowMessage('Tous les fichiers ont été traités');
    end);
end;

procedure TAsyncBatchProcessor.StartProcessing(const Files: TStringList);  
begin  
  FFichiers.Assign(Files);

  FTask := TTask.Run(
    procedure
    var
      i: Integer;
      Fichier: string;
    begin
      for i := 0 to FFichiers.Count - 1 do
      begin
        Fichier := FFichiers[i];

        try
          TraiterFichier(Fichier);
        except
          on E: Exception do
            // Log error
        end;

        UpdateProgress(i + 1, FFichiers.Count);
      end;

      ProcessComplete;
    end);
end;

procedure TAsyncBatchProcessor.CancelProcessing;  
begin  
  if Assigned(FTask) then
    FTask.Cancel;
end;

function TAsyncBatchProcessor.IsProcessing: Boolean;  
begin  
  Result := Assigned(FTask) and (FTask.Status = TTaskStatus.Running);
end;
```

---

## Exemples pratiques complets

### Exemple 1 : Convertisseur d'images par lots

```pascal
uses
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, Vcl.Graphics;

type
  TImageBatchConverter = class
  private
    FSourceFolder: string;
    FDestFolder: string;
    FQuality: Integer;
    FLogger: TBatchLogger;
  public
    constructor Create(const SourceFolder, DestFolder: string);
    destructor Destroy; override;

    procedure ConvertToJPEG(Quality: Integer = 85);
    procedure ConvertToPNG;
    procedure ResizeImages(NewWidth, NewHeight: Integer);

    property Quality: Integer read FQuality write FQuality;
  end;

constructor TImageBatchConverter.Create(const SourceFolder,
                                        DestFolder: string);
begin
  inherited Create;
  FSourceFolder := IncludeTrailingPathDelimiter(SourceFolder);
  FDestFolder := IncludeTrailingPathDelimiter(DestFolder);
  FQuality := 85;

  if not DirectoryExists(FDestFolder) then
    ForceDirectories(FDestFolder);

  FLogger := TBatchLogger.Create(FDestFolder + 'conversion.log');
end;

destructor TImageBatchConverter.Destroy;  
begin  
  FLogger.Free;
  inherited;
end;

procedure TImageBatchConverter.ConvertToJPEG(Quality: Integer);  
var  
  Fichiers: TStringDynArray;
  Fichier, FichierDest: string;
  Image: TBitmap;
  JPEG: TJPEGImage;
begin
  FLogger.LogInfo('Début conversion en JPEG (qualité ' + IntToStr(Quality) + ')');

  // Récupérer toutes les images
  Fichiers := TDirectory.GetFiles(FSourceFolder, '*.bmp');

  Image := TBitmap.Create;
  JPEG := TJPEGImage.Create;
  try
    JPEG.CompressionQuality := Quality;

    for Fichier in Fichiers do
    begin
      try
        // Charger l'image
        Image.LoadFromFile(Fichier);

        // Assigner au JPEG
        JPEG.Assign(Image);

        // Sauvegarder
        FichierDest := FDestFolder +
                      ChangeFileExt(ExtractFileName(Fichier), '.jpg');
        JPEG.SaveToFile(FichierDest);

        FLogger.LogSuccess('Converti : ' + ExtractFileName(Fichier));
      except
        on E: Exception do
          FLogger.LogError(ExtractFileName(Fichier) + ' : ' + E.Message);
      end;
    end;
  finally
    Image.Free;
    JPEG.Free;
  end;

  FLogger.LogInfo('Conversion terminée');
end;

procedure TImageBatchConverter.ResizeImages(NewWidth, NewHeight: Integer);  
var  
  Fichiers: TStringDynArray;
  Fichier, FichierDest: string;
  Image, Resized: TBitmap;
begin
  FLogger.LogInfo(Format('Début redimensionnement (%dx%d)',
    [NewWidth, NewHeight]));

  Fichiers := TDirectory.GetFiles(FSourceFolder, '*.jpg');

  Image := TBitmap.Create;
  Resized := TBitmap.Create;
  try
    for Fichier in Fichiers do
    begin
      try
        Image.LoadFromFile(Fichier);

        Resized.Width := NewWidth;
        Resized.Height := NewHeight;
        Resized.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Image);

        FichierDest := FDestFolder + ExtractFileName(Fichier);
        Resized.SaveToFile(FichierDest);

        FLogger.LogSuccess('Redimensionné : ' + ExtractFileName(Fichier));
      except
        on E: Exception do
          FLogger.LogError(ExtractFileName(Fichier) + ' : ' + E.Message);
      end;
    end;
  finally
    Image.Free;
    Resized.Free;
  end;

  FLogger.LogInfo('Redimensionnement terminé');
end;

// Utilisation
procedure TForm1.ConvertirImages;  
var  
  Converter: TImageBatchConverter;
begin
  Converter := TImageBatchConverter.Create('C:\Photos\Original',
                                           'C:\Photos\Converti');
  try
    Converter.ConvertToJPEG(90);
    ShowMessage('Conversion terminée');
  finally
    Converter.Free;
  end;
end;
```

### Exemple 2 : Nettoyeur de fichiers temporaires

```pascal
type
  TFileCleanerRule = record
    Pattern: string;
    AgeInDays: Integer;
    Recursive: Boolean;
  end;

  TFileCleaner = class
  private
    FRules: TArray<TFileCleanerRule>;
    FLogger: TBatchLogger;
    FDryRun: Boolean; // Mode test sans suppression réelle

    function FileIsOlderThan(const FileName: string; Days: Integer): Boolean;
  public
    constructor Create(DryRun: Boolean = False);
    destructor Destroy; override;

    procedure AddRule(const Pattern: string; AgeInDays: Integer;
                     Recursive: Boolean = False);
    procedure CleanFolder(const FolderPath: string);
    procedure ShowReport;

    property DryRun: Boolean read FDryRun write FDryRun;
  end;

constructor TFileCleaner.Create(DryRun: Boolean);  
begin  
  inherited Create;
  FDryRun := DryRun;
  FLogger := TBatchLogger.Create('nettoyage_' +
    FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');

  if FDryRun then
    FLogger.LogInfo('MODE TEST : Aucun fichier ne sera supprimé');
end;

destructor TFileCleaner.Destroy;  
begin  
  FLogger.Free;
  inherited;
end;

procedure TFileCleaner.AddRule(const Pattern: string; AgeInDays: Integer;
                               Recursive: Boolean);
var
  Rule: TFileCleanerRule;
begin
  Rule.Pattern := Pattern;
  Rule.AgeInDays := AgeInDays;
  Rule.Recursive := Recursive;

  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := Rule;
end;

function TFileCleaner.FileIsOlderThan(const FileName: string;
                                      Days: Integer): Boolean;
var
  FileAge: TDateTime;
begin
  FileAge := TFile.GetLastWriteTime(FileName);
  Result := (Now - FileAge) > Days;
end;

procedure TFileCleaner.CleanFolder(const FolderPath: string);  
var  
  Rule: TFileCleanerRule;
  Fichiers: TStringDynArray;
  Fichier: string;
  SearchOption: TSearchOption;
  TailleSupprimer, TailleTotale: Int64;
  NombreSupprimes: Integer;
begin
  FLogger.LogInfo('Nettoyage du dossier : ' + FolderPath);

  TailleTotale := 0;
  NombreSupprimes := 0;

  for Rule in FRules do
  begin
    if Rule.Recursive then
      SearchOption := TSearchOption.soAllDirectories
    else
      SearchOption := TSearchOption.soTopDirectoryOnly;

    Fichiers := TDirectory.GetFiles(FolderPath, Rule.Pattern, SearchOption);

    for Fichier in Fichiers do
    begin
      if FileIsOlderThan(Fichier, Rule.AgeInDays) then
      begin
        TailleSupprimer := TFile.GetSize(Fichier);

        if FDryRun then
        begin
          FLogger.LogInfo(Format('[TEST] Supprimerait : %s (%d Ko)',
            [Fichier, TailleSupprimer div 1024]));
        end
        else
        begin
          try
            DeleteFile(Fichier);
            FLogger.LogSuccess(Format('Supprimé : %s (%d Ko)',
              [Fichier, TailleSupprimer div 1024]));
            Inc(NombreSupprimes);
            Inc(TailleTotale, TailleSupprimer);
          except
            on E: Exception do
              FLogger.LogError('Impossible de supprimer ' + Fichier + ' : ' +
                E.Message);
          end;
        end;
      end;
    end;
  end;

  if FDryRun then
    FLogger.LogInfo(Format('Fin du test : %d fichiers seraient supprimés',
      [NombreSupprimes]))
  else
    FLogger.LogInfo(Format('Nettoyage terminé : %d fichiers supprimés, %d Mo libérés',
      [NombreSupprimes, TailleTotale div (1024 * 1024)]));
end;

procedure TFileCleaner.ShowReport;  
begin  
  // Afficher le contenu du log
end;

// Utilisation
procedure TForm1.NettoyerFichiersTemporaires;  
var  
  Cleaner: TFileCleaner;
begin
  // Mode test d'abord
  Cleaner := TFileCleaner.Create(True);
  try
    // Définir les règles
    Cleaner.AddRule('*.tmp', 7);        // Fichiers .tmp > 7 jours
    Cleaner.AddRule('*.log', 30);       // Fichiers .log > 30 jours
    Cleaner.AddRule('*.bak', 14);       // Fichiers .bak > 14 jours

    // Tester
    Cleaner.CleanFolder('C:\Temp');

    if MessageDlg('Le test est terminé. Voulez-vous effectuer le nettoyage réel ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Cleaner.DryRun := False;
      Cleaner.CleanFolder('C:\Temp');
      ShowMessage('Nettoyage effectué');
    end;
  finally
    Cleaner.Free;
  end;
end;
```

### Exemple 3 : Système de sauvegarde incrémentielle

```pascal
type
  TIncrementalBackup = class
  private
    FSourceFolder: string;
    FBackupFolder: string;
    FLogger: TBatchLogger;

    function GetBackupFileName: string;
    function FileNeedsBackup(const SourceFile, BackupFile: string): Boolean;
  public
    constructor Create(const SourceFolder, BackupFolder: string);
    destructor Destroy; override;

    procedure PerformBackup;
    procedure RestoreLatestBackup(const DestFolder: string);
  end;

constructor TIncrementalBackup.Create(const SourceFolder,
                                      BackupFolder: string);
begin
  inherited Create;
  FSourceFolder := IncludeTrailingPathDelimiter(SourceFolder);
  FBackupFolder := IncludeTrailingPathDelimiter(BackupFolder);

  if not DirectoryExists(FBackupFolder) then
    ForceDirectories(FBackupFolder);

  FLogger := TBatchLogger.Create(FBackupFolder + 'backup.log');
end;

destructor TIncrementalBackup.Destroy;  
begin  
  FLogger.Free;
  inherited;
end;

function TIncrementalBackup.GetBackupFileName: string;  
begin  
  Result := FBackupFolder + 'backup_' +
            FormatDateTime('yyyymmdd_hhnnss', Now);
end;

function TIncrementalBackup.FileNeedsBackup(const SourceFile,
                                            BackupFile: string): Boolean;
var
  SourceDate, BackupDate: TDateTime;
begin
  // Si le backup n'existe pas, il faut sauvegarder
  if not FileExists(BackupFile) then
  begin
    Result := True;
    Exit;
  end;

  // Comparer les dates de modification
  SourceDate := TFile.GetLastWriteTime(SourceFile);
  BackupDate := TFile.GetLastWriteTime(BackupFile);

  Result := SourceDate > BackupDate;
end;

procedure TIncrementalBackup.PerformBackup;  
var  
  FichiersSource: TStringDynArray;
  Fichier, CheminRelatif, FichierBackup, DossierBackup: string;
  BackupFolder: string;
  Nouveaux, Modifies, Ignores: Integer;
  EstNouveau: Boolean;
begin
  FLogger.LogInfo('Début de la sauvegarde incrémentielle');
  FLogger.LogInfo('Source : ' + FSourceFolder);

  BackupFolder := GetBackupFileName + '\';
  ForceDirectories(BackupFolder);

  FLogger.LogInfo('Destination : ' + BackupFolder);

  Nouveaux := 0;
  Modifies := 0;
  Ignores := 0;

  // Récupérer tous les fichiers source
  FichiersSource := TDirectory.GetFiles(FSourceFolder, '*.*',
    TSearchOption.soAllDirectories);

  for Fichier in FichiersSource do
  begin
    // Obtenir le chemin relatif
    CheminRelatif := StringReplace(Fichier, FSourceFolder, '', []);
    FichierBackup := BackupFolder + CheminRelatif;

    // Créer les sous-dossiers si nécessaire
    DossierBackup := ExtractFilePath(FichierBackup);
    if not DirectoryExists(DossierBackup) then
      ForceDirectories(DossierBackup);

    // Vérifier si le fichier doit être sauvegardé
    if FileNeedsBackup(Fichier, FichierBackup) then
    begin
      // Déterminer si c'est un nouveau fichier avant la copie
      EstNouveau := not FileExists(FichierBackup);

      try
        TFile.Copy(Fichier, FichierBackup, True);

        if EstNouveau then
        begin
          FLogger.LogSuccess('Nouveau : ' + CheminRelatif);
          Inc(Nouveaux);
        end
        else
        begin
          FLogger.LogInfo('Modifié : ' + CheminRelatif);
          Inc(Modifies);
        end;
      except
        on E: Exception do
          FLogger.LogError(CheminRelatif + ' : ' + E.Message);
      end;
    end
    else
    begin
      Inc(Ignores);
    end;
  end;

  FLogger.LogInfo(Format('Sauvegarde terminée : %d nouveaux, %d modifiés, %d inchangés',
    [Nouveaux, Modifies, Ignores]));
end;

procedure TIncrementalBackup.RestoreLatestBackup(const DestFolder: string);  
var  
  Backups: TStringDynArray;
  LatestBackup: string;
  FichiersBackup: TStringDynArray;
  Fichier, CheminRelatif, FichierDest, DossierDest: string;
begin
  // Trouver la dernière sauvegarde
  Backups := TDirectory.GetDirectories(FBackupFolder, 'backup_*');

  if Length(Backups) = 0 then
  begin
    FLogger.LogError('Aucune sauvegarde trouvée');
    Exit;
  end;

  // Trier et prendre la plus récente
  TArray.Sort<string>(Backups);
  LatestBackup := Backups[High(Backups)];

  FLogger.LogInfo('Restauration depuis : ' + LatestBackup);

  // Récupérer tous les fichiers de la sauvegarde
  FichiersBackup := TDirectory.GetFiles(LatestBackup, '*.*',
    TSearchOption.soAllDirectories);

  for Fichier in FichiersBackup do
  begin
    CheminRelatif := StringReplace(Fichier, LatestBackup + '\', '', []);
    FichierDest := IncludeTrailingPathDelimiter(DestFolder) + CheminRelatif;

    DossierDest := ExtractFilePath(FichierDest);
    if not DirectoryExists(DossierDest) then
      ForceDirectories(DossierDest);

    try
      TFile.Copy(Fichier, FichierDest, True);
      FLogger.LogSuccess('Restauré : ' + CheminRelatif);
    except
      on E: Exception do
        FLogger.LogError(CheminRelatif + ' : ' + E.Message);
    end;
  end;

  FLogger.LogInfo('Restauration terminée');
end;

// Utilisation
procedure TForm1.EffectuerSauvegarde;  
var  
  Backup: TIncrementalBackup;
begin
  Backup := TIncrementalBackup.Create('C:\MesDonnees', 'D:\Sauvegardes');
  try
    Backup.PerformBackup;
    ShowMessage('Sauvegarde effectuée');
  finally
    Backup.Free;
  end;
end;
```

---

## Bonnes pratiques

### 1. Toujours valider les entrées

```pascal
function ValidateInputs(const SourceFolder, DestFolder: string): Boolean;  
begin  
  Result := False;

  if not DirectoryExists(SourceFolder) then
  begin
    ShowMessage('Le dossier source n''existe pas');
    Exit;
  end;

  if SourceFolder = DestFolder then
  begin
    ShowMessage('Les dossiers source et destination doivent être différents');
    Exit;
  end;

  if not HasWriteAccess(DestFolder) then
  begin
    ShowMessage('Pas de droits d''écriture sur le dossier destination');
    Exit;
  end;

  Result := True;
end;
```

### 2. Prévoir une option d'annulation

```pascal
var
  FCancelled: Boolean;

procedure ProcessWithCancellation(const Files: TStringList);  
var  
  i: Integer;
begin
  FCancelled := False;

  for i := 0 to Files.Count - 1 do
  begin
    if FCancelled then
    begin
      ShowMessage('Traitement annulé par l''utilisateur');
      Break;
    end;

    // Traiter le fichier
    Application.ProcessMessages; // Permet de réagir au bouton Annuler
  end;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);  
begin  
  FCancelled := True;
end;
```

### 3. Sauvegarder avant les opérations destructives

```pascal
procedure SafeDelete(const Files: TStringList);  
var  
  BackupFolder: string;
  i: Integer;
begin
  // Créer un dossier de backup
  BackupFolder := 'C:\Backup_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '\';
  ForceDirectories(BackupFolder);

  // Copier d'abord
  for i := 0 to Files.Count - 1 do
    TFile.Copy(Files[i], BackupFolder + ExtractFileName(Files[i]));

  // Puis supprimer
  for i := 0 to Files.Count - 1 do
    DeleteFile(Files[i]);
end;
```

### 4. Estimer le temps avant de commencer

```pascal
procedure EstimateTime(const Files: TStringList);  
var  
  SampleSize, i: Integer;
  StartTime: TDateTime;
  AvgTime, EstimatedTotal: Double;
begin
  SampleSize := Min(10, Files.Count);
  StartTime := Now;

  // Traiter quelques fichiers pour estimer
  for i := 0 to SampleSize - 1 do
    ProcessFile(Files[i]);

  AvgTime := (Now - StartTime) / SampleSize;
  EstimatedTotal := AvgTime * Files.Count;

  ShowMessage(Format('Temps estimé : %s',
    [FormatDateTime('nn:ss', EstimatedTotal)]));
end;
```

### 5. Fournir un résumé à la fin

```pascal
type
  TBatchSummary = record
    TotalFiles: Integer;
    SuccessCount: Integer;
    ErrorCount: Integer;
    SkippedCount: Integer;
    TotalSize: Int64;
    Duration: TDateTime;
  end;

function GenerateSummary(const Summary: TBatchSummary): string;  
begin  
  Result := Format(
    'RÉSUMÉ DU TRAITEMENT PAR LOTS' + #13#10 +
    '=============================' + #13#10 +
    'Fichiers traités : %d' + #13#10 +
    'Succès : %d' + #13#10 +
    'Erreurs : %d' + #13#10 +
    'Ignorés : %d' + #13#10 +
    'Taille totale : %d Mo' + #13#10 +
    'Durée : %s',
    [Summary.TotalFiles,
     Summary.SuccessCount,
     Summary.ErrorCount,
     Summary.SkippedCount,
     Summary.TotalSize div (1024 * 1024),
     FormatDateTime('nn:ss', Summary.Duration)]);
end;
```

---

## Résumé

Dans ce chapitre, vous avez découvert le traitement par lots en Delphi :

**Concepts clés :**
- Traitement automatisé de plusieurs fichiers
- Automatisation de tâches répétitives
- Gain de temps considérable

**Techniques apprises :**
- Lister et filtrer des fichiers
- Renommer, copier, déplacer en masse
- Organiser automatiquement
- Afficher la progression
- Gérer les erreurs
- Créer des logs détaillés

**Opérations courantes :**
- Conversion d'images par lots
- Nettoyage de fichiers temporaires
- Sauvegardes incrémentales
- Organisation automatique

**Bonnes pratiques :**
- Valider les entrées
- Permettre l'annulation
- Sauvegarder avant les opérations destructives
- Estimer le temps nécessaire
- Fournir un résumé détaillé
- Logger toutes les opérations

**Performance :**
- Traitement séquentiel simple
- Traitement asynchrone pour l'interface
- Traitement parallèle pour la vitesse

Le traitement par lots est un outil indispensable pour automatiser les tâches répétitives et gérer efficacement de grandes quantités de fichiers dans vos applications Delphi !

⏭️ [Utilisation de formats modernes (JSON, XML, YAML)](/07-gestion-des-fichiers-et-flux-de-donnees/07-utilisation-de-formats-modernes.md)
