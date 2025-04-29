# 11.8 Cas d'usage concrets

## Introduction

Dans les sections précédentes, nous avons exploré les concepts théoriques et techniques du multithreading. Maintenant, examinons comment appliquer ces connaissances à des cas d'usage réels. Ce chapitre présente plusieurs exemples concrets où le multithreading apporte une valeur significative, avec des exemples de code complets que vous pourrez adapter à vos propres projets.

## 1. Application de traitement par lots de fichiers

### Scénario

Imaginez une application qui doit traiter un grand nombre de fichiers : conversion d'images, extraction de métadonnées, compression, etc. Sans multithreading, le traitement serait séquentiel et pourrait prendre beaucoup de temps, en particulier pour des centaines ou des milliers de fichiers.

### Solution avec multithreading

```pascal
type
  TFileProcessor = class
  private
    FFiles: TStringList;
    FProcessedCount: Integer;
    FTotalCount: Integer;
    FLock: TCriticalSection;
    FOnProgress: TProc<Integer, Integer>; // Callback: (Processed, Total)
    FOnComplete: TProc;
    FCancelled: Boolean;

    procedure ProcessFiles;
    function ProcessSingleFile(const FileName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const FileName: string);
    procedure AddFiles(const Directory, FileMask: string; IncludeSubdirectories: Boolean = False);
    procedure ProcessAllFiles;
    procedure Cancel;

    property OnProgress: TProc<Integer, Integer> read FOnProgress write FOnProgress;
    property OnComplete: TProc read FOnComplete write FOnComplete;
  end;

constructor TFileProcessor.Create;
begin
  inherited;
  FFiles := TStringList.Create;
  FLock := TCriticalSection.Create;
  FProcessedCount := 0;
  FCancelled := False;
end;

destructor TFileProcessor.Destroy;
begin
  FFiles.Free;
  FLock.Free;
  inherited;
end;

procedure TFileProcessor.AddFile(const FileName: string);
begin
  if FileExists(FileName) then
    FFiles.Add(FileName);
end;

procedure TFileProcessor.AddFiles(const Directory, FileMask: string; IncludeSubdirectories: Boolean = False);
var
  Option: TSearchOption;
  Files: TStringDynArray;
  FileName: string;
begin
  if IncludeSubdirectories then
    Option := TSearchOption.soAllDirectories
  else
    Option := TSearchOption.soTopDirectoryOnly;

  Files := TDirectory.GetFiles(Directory, FileMask, Option);

  for FileName in Files do
    FFiles.Add(FileName);
end;

procedure TFileProcessor.ProcessAllFiles;
begin
  // Réinitialiser le compteur
  FProcessedCount := 0;
  FTotalCount := FFiles.Count;
  FCancelled := False;

  // Démarrer le traitement dans un thread séparé
  TTask.Run(ProcessFiles);
end;

procedure TFileProcessor.ProcessFiles;
var
  ThreadPool: TThreadPool;
  i: Integer;
  TaskList: TList<ITask>;
begin
  // Créer une liste pour suivre toutes les tâches
  TaskList := TList<ITask>.Create;
  try
    // Créer une tâche pour chaque fichier (dans la limite du nombre de cœurs)
    for i := 0 to FFiles.Count - 1 do
    begin
      if FCancelled then
        Break;

      // Capture de l'index pour la closure
      var Index := i;

      // Créer et démarrer une tâche pour ce fichier
      var Task := TTask.Run(
        procedure
        begin
          if FCancelled then
            Exit;

          var FileName := FFiles[Index];
          var Success := ProcessSingleFile(FileName);

          // Mettre à jour le compteur de façon thread-safe
          FLock.Enter;
          try
            Inc(FProcessedCount);

            // Appeler le callback de progression
            if Assigned(FOnProgress) then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  FOnProgress(FProcessedCount, FTotalCount);
                end
              );
            end;
          finally
            FLock.Leave;
          end;
        end
      );

      TaskList.Add(Task);
    end;

    // Attendre que toutes les tâches soient terminées
    TTask.WaitForAll(TaskList.ToArray);

    // Appeler le callback de fin
    if Assigned(FOnComplete) and (not FCancelled) then
    begin
      TThread.Queue(nil,
        procedure
        begin
          FOnComplete;
        end
      );
    end;
  finally
    TaskList.Free;
  end;
end;

function TFileProcessor.ProcessSingleFile(const FileName: string): Boolean;
begin
  Result := False;
  try
    // Ici, mettez votre code de traitement spécifique
    // Par exemple, pour le redimensionnement d'images :
    if ExtractFileExt(FileName).ToLower = '.jpg' then
    begin
      var Image := TImage.Create(nil);
      try
        Image.Picture.LoadFromFile(FileName);

        // Redimensionner l'image
        var Bitmap := TBitmap.Create;
        try
          Bitmap.SetSize(500, 500 * Image.Picture.Height div Image.Picture.Width);
          Bitmap.Canvas.StretchDraw(Rect(0, 0, Bitmap.Width, Bitmap.Height), Image.Picture.Graphic);

          // Sauvegarder l'image redimensionnée
          var OutputFileName := ChangeFileExt(FileName, '_resized.jpg');
          Bitmap.SaveToFile(OutputFileName);
        finally
          Bitmap.Free;
        end;
      finally
        Image.Free;
      end;
    end;

    Result := True;
  except
    // Gérer les erreurs spécifiques à chaque fichier
    Result := False;
  end;
end;

procedure TFileProcessor.Cancel;
begin
  FCancelled := True;
end;
```

### Utilisation dans un formulaire

```pascal
procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  ButtonProcess.Enabled := False;
  ButtonCancel.Enabled := True;
  ProgressBar1.Position := 0;

  // Créer et configurer le processeur de fichiers
  var Processor := TFileProcessor.Create;
  Processor.OnProgress :=
    procedure(Processed, Total: Integer)
    begin
      ProgressBar1.Max := Total;
      ProgressBar1.Position := Processed;
      LabelStatus.Caption := Format('Traitement en cours... %d/%d (%d%%)',
                                  [Processed, Total, Round(Processed / Total * 100)]);
    end;

  Processor.OnComplete :=
    procedure
    begin
      ButtonProcess.Enabled := True;
      ButtonCancel.Enabled := False;
      LabelStatus.Caption := 'Traitement terminé !';
      ShowMessage('Tous les fichiers ont été traités avec succès !');
      Processor.Free;
    end;

  // Ajouter des fichiers à traiter
  Processor.AddFiles(EditDirectory.Text, '*.jpg', CheckBoxIncludeSubdirs.Checked);

  // Démarrer le traitement
  Processor.ProcessAllFiles;

  // Configurer le bouton d'annulation
  ButtonCancel.OnClick :=
    procedure(Sender: TObject)
    begin
      Processor.Cancel;
      ButtonCancel.Enabled := False;
      LabelStatus.Caption := 'Annulation en cours...';
    end;
end;
```

## 2. Téléchargement parallèle de fichiers

### Scénario

Une application qui doit télécharger plusieurs fichiers depuis un serveur. Le téléchargement parallèle peut réduire considérablement le temps total.

### Solution avec multithreading

```pascal
type
  TDownloadItem = record
    URL: string;
    DestinationFile: string;
    Size: Int64;
    Downloaded: Int64;
    Status: (dsQueued, dsDownloading, dsCompleted, dsFailed);
    ErrorMessage: string;
  end;

  TDownloadManager = class
  private
    FItems: TList<TDownloadItem>;
    FLock: TCriticalSection;
    FMaxConcurrent: Integer;
    FActiveCount: Integer;
    FOnItemProgress: TProc<Integer, TDownloadItem>; // Index, Item
    FOnAllCompleted: TProc;
    FCancelled: Boolean;

    procedure StartNextDownload;
    procedure DownloadFile(Index: Integer);
    function AllDownloadsComplete: Boolean;
  public
    constructor Create(MaxConcurrentDownloads: Integer = 3);
    destructor Destroy; override;
    function AddDownload(const URL, DestinationFile: string): Integer;
    procedure StartDownloads;
    procedure CancelAll;

    property OnItemProgress: TProc<Integer, TDownloadItem> read FOnItemProgress write FOnItemProgress;
    property OnAllCompleted: TProc read FOnAllCompleted write FOnAllCompleted;
  end;

constructor TDownloadManager.Create(MaxConcurrentDownloads: Integer = 3);
begin
  inherited Create;
  FItems := TList<TDownloadItem>.Create;
  FLock := TCriticalSection.Create;
  FMaxConcurrent := MaxConcurrentDownloads;
  FActiveCount := 0;
  FCancelled := False;
end;

destructor TDownloadManager.Destroy;
begin
  FItems.Free;
  FLock.Free;
  inherited;
end;

function TDownloadManager.AddDownload(const URL, DestinationFile: string): Integer;
var
  Item: TDownloadItem;
begin
  Item.URL := URL;
  Item.DestinationFile := DestinationFile;
  Item.Size := 0;
  Item.Downloaded := 0;
  Item.Status := dsQueued;
  Item.ErrorMessage := '';

  FLock.Enter;
  try
    Result := FItems.Add(Item);
  finally
    FLock.Leave;
  end;
end;

procedure TDownloadManager.StartDownloads;
var
  i: Integer;
begin
  FCancelled := False;
  FActiveCount := 0;

  // Démarrer jusqu'à FMaxConcurrent téléchargements
  for i := 1 to FMaxConcurrent do
    StartNextDownload;
end;

procedure TDownloadManager.StartNextDownload;
var
  i: Integer;
  Item: TDownloadItem;
begin
  if FCancelled then
    Exit;

  FLock.Enter;
  try
    // Chercher le prochain téléchargement en attente
    for i := 0 to FItems.Count - 1 do
    begin
      Item := FItems[i];
      if Item.Status = dsQueued then
      begin
        // Marquer comme en cours
        Item.Status := dsDownloading;
        FItems[i] := Item;
        Inc(FActiveCount);

        // Lancer le téléchargement dans un thread
        TTask.Run(
          procedure
          begin
            DownloadFile(i);
          end
        );

        Exit; // Une seule tâche démarrée à la fois
      end;
    end;

    // Si tous les téléchargements sont terminés, appeler le callback
    if (FActiveCount = 0) and AllDownloadsComplete and Assigned(FOnAllCompleted) then
    begin
      TThread.Queue(nil,
        procedure
        begin
          FOnAllCompleted;
        end
      );
    end;
  finally
    FLock.Leave;
  end;
end;

function TDownloadManager.AllDownloadsComplete: Boolean;
var
  i: Integer;
  Item: TDownloadItem;
begin
  Result := True;

  for i := 0 to FItems.Count - 1 do
  begin
    Item := FItems[i];
    if (Item.Status <> dsCompleted) and (Item.Status <> dsFailed) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TDownloadManager.DownloadFile(Index: Integer);
var
  Item: TDownloadItem;
  Client: TNetHTTPClient;
  Request: TNetHTTPRequest;
  Response: IHTTPResponse;
  Stream: TFileStream;
begin
  Client := TNetHTTPClient.Create(nil);
  Request := TNetHTTPRequest.Create(nil);
  try
    Request.Client := Client;

    // Configurer le callback de progression
    Client.OnReceiveData :=
      procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
      begin
        FLock.Enter;
        try
          if FCancelled then
          begin
            AAbort := True;
            Exit;
          end;

          Item := FItems[Index];
          Item.Size := AContentLength;
          Item.Downloaded := AReadCount;
          FItems[Index] := Item;

          // Appeler le callback de progression
          if Assigned(FOnItemProgress) then
          begin
            TThread.Queue(nil,
              procedure
              begin
                FOnItemProgress(Index, Item);
              end
            );
          end;
        finally
          FLock.Leave;
        end;
      end;

    try
      // Créer le répertoire de destination si nécessaire
      ForceDirectories(ExtractFilePath(FItems[Index].DestinationFile));

      // Créer le fichier de destination
      Stream := TFileStream.Create(FItems[Index].DestinationFile, fmCreate);
      try
        // Télécharger le fichier
        Response := Request.Get(FItems[Index].URL, Stream);

        // Mettre à jour le statut
        FLock.Enter;
        try
          Item := FItems[Index];
          Item.Status := dsCompleted;
          FItems[Index] := Item;
          Dec(FActiveCount);
        finally
          FLock.Leave;
        end;
      finally
        Stream.Free;
      end;
    except
      on E: Exception do
      begin
        // Gérer l'erreur
        FLock.Enter;
        try
          Item := FItems[Index];
          Item.Status := dsFailed;
          Item.ErrorMessage := E.Message;
          FItems[Index] := Item;
          Dec(FActiveCount);
        finally
          FLock.Leave;
        end;
      end;
    end;

    // Appeler le callback de progression une dernière fois
    if Assigned(FOnItemProgress) then
    begin
      Item := FItems[Index];
      TThread.Queue(nil,
        procedure
        begin
          FOnItemProgress(Index, Item);
        end
      );
    end;

    // Démarrer le prochain téléchargement
    StartNextDownload;
  finally
    Request.Free;
    Client.Free;
  end;
end;

procedure TDownloadManager.CancelAll;
begin
  FCancelled := True;
end;
```

### Utilisation dans un formulaire

```pascal
type
  TDownloadForm = class(TForm)
    ButtonAddURL: TButton;
    ButtonStartDownloads: TButton;
    ButtonCancelAll: TButton;
    EditURL: TEdit;
    EditDestination: TEdit;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddURLClick(Sender: TObject);
    procedure ButtonStartDownloadsClick(Sender: TObject);
    procedure ButtonCancelAllClick(Sender: TObject);
  private
    FDownloadManager: TDownloadManager;
    procedure UpdateDownloadProgress(Index: Integer; const Item: TDownloadItem);
    procedure AllDownloadsCompleted;
  end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  // Configurer le grid
  StringGrid1.ColCount := 5;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := 'URL';
  StringGrid1.Cells[1, 0] := 'Destination';
  StringGrid1.Cells[2, 0] := 'Taille';
  StringGrid1.Cells[3, 0] := 'Progression';
  StringGrid1.Cells[4, 0] := 'Statut';

  // Créer le gestionnaire de téléchargements
  FDownloadManager := TDownloadManager.Create;
  FDownloadManager.OnItemProgress := UpdateDownloadProgress;
  FDownloadManager.OnAllCompleted := AllDownloadsCompleted;
end;

procedure TDownloadForm.FormDestroy(Sender: TObject);
begin
  FDownloadManager.Free;
end;

procedure TDownloadForm.ButtonAddURLClick(Sender: TObject);
var
  URL, Destination: string;
  Index: Integer;
begin
  URL := EditURL.Text;
  Destination := EditDestination.Text;

  if (URL = '') or (Destination = '') then
  begin
    ShowMessage('Veuillez entrer une URL et un chemin de destination.');
    Exit;
  end;

  // Ajouter le téléchargement
  Index := FDownloadManager.AddDownload(URL, Destination);

  // Ajouter une ligne au grid
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  StringGrid1.Cells[0, StringGrid1.RowCount - 1] := URL;
  StringGrid1.Cells[1, StringGrid1.RowCount - 1] := Destination;
  StringGrid1.Cells[2, StringGrid1.RowCount - 1] := 'En attente...';
  StringGrid1.Cells[3, StringGrid1.RowCount - 1] := '0%';
  StringGrid1.Cells[4, StringGrid1.RowCount - 1] := 'En attente';

  // Effacer les champs
  EditURL.Clear;
  EditDestination.Clear;
  EditURL.SetFocus;
end;

procedure TDownloadForm.ButtonStartDownloadsClick(Sender: TObject);
begin
  ButtonStartDownloads.Enabled := False;
  ButtonCancelAll.Enabled := True;

  // Démarrer les téléchargements
  FDownloadManager.StartDownloads;
end;

procedure TDownloadForm.ButtonCancelAllClick(Sender: TObject);
begin
  FDownloadManager.CancelAll;
  ButtonCancelAll.Enabled := False;
end;

procedure TDownloadForm.UpdateDownloadProgress(Index: Integer; const Item: TDownloadItem);
var
  RowIndex, Percentage: Integer;
  SizeStr, StatusStr: string;
begin
  // L'index dans le grid est décalé de 1 à cause de la ligne d'en-tête
  RowIndex := Index + 1;

  if RowIndex < StringGrid1.RowCount then
  begin
    // Calculer le pourcentage
    if Item.Size > 0 then
      Percentage := Round((Item.Downloaded / Item.Size) * 100)
    else
      Percentage := 0;

    // Formater la taille
    if Item.Size > 0 then
      SizeStr := FormatFloat('#,##0', Item.Size) + ' octets'
    else
      SizeStr := 'Inconnue';

    // Déterminer le statut
    case Item.Status of
      dsQueued: StatusStr := 'En attente';
      dsDownloading: StatusStr := 'Téléchargement';
      dsCompleted: StatusStr := 'Terminé';
      dsFailed: StatusStr := 'Erreur: ' + Item.ErrorMessage;
    end;

    // Mettre à jour le grid
    StringGrid1.Cells[2, RowIndex] := SizeStr;
    StringGrid1.Cells[3, RowIndex] := IntToStr(Percentage) + '%';
    StringGrid1.Cells[4, RowIndex] := StatusStr;
  end;
end;

procedure TDownloadForm.AllDownloadsCompleted;
begin
  ButtonStartDownloads.Enabled := True;
  ButtonCancelAll.Enabled := False;
  ShowMessage('Tous les téléchargements sont terminés !');
end;
```

## 3. Analyse de données en temps réel

### Scénario

Une application qui reçoit un flux constant de données (par exemple, des capteurs, une base de données en temps réel, ou un flux réseau) et qui doit analyser ces données sans bloquer l'interface utilisateur.

### Solution avec multithreading

```pascal
type
  TDataPoint = record
    Timestamp: TDateTime;
    Value: Double;
  end;

  TAnalysisResult = record
    Min, Max, Average: Double;
    Count: Integer;
    Trend: Double; // Pente de la tendance
  end;

  TRealTimeAnalyzer = class
  private
    FDataQueue: TThreadedQueue<TDataPoint>;
    FAnalysisInterval: Integer; // En millisecondes
    FIsRunning: Boolean;
    FAnalysisThread: TThread;
    FOnNewResult: TProc<TAnalysisResult>;
    FLock: TCriticalSection;
    FLatestResult: TAnalysisResult;
    FDataPoints: TList<TDataPoint>;
    FMaxDataPoints: Integer;

    procedure AnalysisThreadProc;
    procedure CalculateStatistics;
  public
    constructor Create(AnalysisInterval: Integer = 1000; MaxDataPoints: Integer = 1000);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure AddDataPoint(const Point: TDataPoint);
    function GetLatestResult: TAnalysisResult;

    property OnNewResult: TProc<TAnalysisResult> read FOnNewResult write FOnNewResult;
  end;

constructor TRealTimeAnalyzer.Create(AnalysisInterval: Integer = 1000; MaxDataPoints: Integer = 1000);
begin
  inherited Create;
  FDataQueue := TThreadedQueue<TDataPoint>.Create(10000, INFINITE);
  FLock := TCriticalSection.Create;
  FDataPoints := TList<TDataPoint>.Create;
  FAnalysisInterval := AnalysisInterval;
  FMaxDataPoints := MaxDataPoints;
  FIsRunning := False;
end;

destructor TRealTimeAnalyzer.Destroy;
begin
  Stop;
  FDataQueue.Free;
  FLock.Free;
  FDataPoints.Free;
  inherited;
end;

procedure TRealTimeAnalyzer.Start;
begin
  if not FIsRunning then
  begin
    FIsRunning := True;

    // Créer et démarrer le thread d'analyse
    FAnalysisThread := TThread.CreateAnonymousThread(AnalysisThreadProc);
    FAnalysisThread.FreeOnTerminate := False;
    FAnalysisThread.Start;
  end;
end;

procedure TRealTimeAnalyzer.Stop;
begin
  if FIsRunning then
  begin
    FIsRunning := False;

    // Attendre que le thread se termine
    if Assigned(FAnalysisThread) then
    begin
      FAnalysisThread.WaitFor;
      FAnalysisThread.Free;
      FAnalysisThread := nil;
    end;
  end;
end;

procedure TRealTimeAnalyzer.AddDataPoint(const Point: TDataPoint);
begin
  FDataQueue.PushItem(Point);
end;

function TRealTimeAnalyzer.GetLatestResult: TAnalysisResult;
begin
  FLock.Enter;
  try
    Result := FLatestResult;
  finally
    FLock.Leave;
  end;
end;

procedure TRealTimeAnalyzer.AnalysisThreadProc;
var
  Point: TDataPoint;
  Result: TWaitResult;
  LastAnalysisTime: TDateTime;
  NeedAnalysis: Boolean;
begin
  LastAnalysisTime := Now;

  while FIsRunning do
  begin
    // Essayer de récupérer un nouveau point de données (avec timeout)
    Result := FDataQueue.PopItem(Point, 100);

    if Result = wrSignaled then
    begin
      // Ajouter le point aux données stockées
      FLock.Enter;
      try
        FDataPoints.Add(Point);

        // Limiter le nombre de points stockés
        while FDataPoints.Count > FMaxDataPoints do
          FDataPoints.Delete(0);
      finally
        FLock.Leave;
      end;
    end;

    // Vérifier si c'est le moment d'effectuer une analyse
    NeedAnalysis := MilliSecondsBetween(Now, LastAnalysisTime) >= FAnalysisInterval;

    if NeedAnalysis then
    begin
      CalculateStatistics;
      LastAnalysisTime := Now;
    end;
  end;
end;

procedure TRealTimeAnalyzer.CalculateStatistics;
var
  i: Integer;
  Sum, SumX, SumY, SumXX, SumXY: Double;
  N: Integer;
  Point: TDataPoint;
  Result: TAnalysisResult;
  X, Y: Double;
begin
  FLock.Enter;
  try
    // Initialiser les variables
    Result.Min := MaxDouble;
    Result.Max := -MaxDouble;
    Sum := 0;
    N := FDataPoints.Count;

    // Variables pour le calcul de la tendance
    SumX := 0;
    SumY := 0;
    SumXX := 0;
    SumXY := 0;

    if N > 0 then
    begin
      // Calculer les statistiques de base
      for i := 0 to N - 1 do
      begin
        Point := FDataPoints[i];
        Y := Point.Value;
        X := i; // Utiliser l'index comme valeur X pour la tendance

        // Min, Max, Sum
        if Y < Result.Min then Result.Min := Y;
        if Y > Result.Max then Result.Max := Y;
        Sum := Sum + Y;

        // Variables pour la régression linéaire
        SumX := SumX + X;
        SumY := SumY + Y;
        SumXX := SumXX + X * X;
        SumXY := SumXY + X * Y;
      end;

      // Calculer la moyenne
      Result.Average := Sum / N;

      // Calculer la pente de la tendance (régression linéaire)
      if N > 1 then
        Result.Trend := (N * SumXY - SumX * SumY) / (N * SumXX - SumX * SumX)
      else
        Result.Trend := 0;
    end
    else
    begin
      // Aucune donnée
      Result.Min := 0;
      Result.Max := 0;
      Result.Average := 0;
      Result.Trend := 0;
    end;

    Result.Count := N;

    // Stocker le résultat
    FLatestResult := Result;
  finally
    FLock.Leave;
  end;

  // Notifier les listeners
  if Assigned(FOnNewResult) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FOnNewResult(Result);
      end
    );
  end;
end;
```

### Utilisation dans un formulaire

```pascal
type
  TAnalysisForm = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Timer1: TTimer;
    LabelMin: TLabel;
    LabelMax: TLabel;
    LabelAverage: TLabel;
    LabelCount: TLabel;
    LabelTrend: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FAnalyzer: TRealTimeAnalyzer;
    FDataCounter: Integer;
    procedure OnNewAnalysisResult(const Result: TAnalysisResult);
  end;

procedure TAnalysisForm.FormCreate(Sender: TObject);
begin
  // Initialiser le graphique
  Chart1.Title.Text.Add('Données en temps réel');
  Chart1.BottomAxis.Title.Caption := 'Temps';
  Chart1.LeftAxis.Title.Caption := 'Valeur';

  // Créer l'analyseur
  FAnalyzer := TRealTimeAnalyzer.Create(500, 100); // Analyse toutes les 500ms, max 100 points
  FAnalyzer.OnNewResult := OnNewAnalysisResult;
  FAnalyzer.Start;

  // Initialiser le compteur
  FDataCounter := 0;

  // Démarrer le timer pour simuler l'arrivée de données
  Timer1.Interval := 100; // Nouvelle donnée toutes les 100ms
  Timer1.Enabled := True;
end;

procedure TAnalysisForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  FAnalyzer.Free;
end;

procedure TAnalysisForm.Timer1Timer(Sender: TObject);
var
  Point: TDataPoint;
begin
  // Simuler un nouveau point de données
  Inc(FDataCounter);
  Point.Timestamp := Now;

  // Créer une valeur sinusoïdale avec du bruit
  Point.Value := 50 + 30 * Sin(FDataCounter / 20) + Random(10) - 5;

  // Ajouter au graphique
  Series1.AddXY(FDataCounter, Point.Value);

  // Limiter le nombre de points affichés
  if Series1.Count > 100 then
    Series1.Delete(0);

  // Envoyer à l'analyseur
  FAnalyzer.AddDataPoint(Point);
end;

procedure TAnalysisForm.OnNewAnalysisResult(const Result: TAnalysisResult);
begin
  // Mettre à jour les labels avec les résultats d'analyse
  LabelMin.Caption := Format('Minimum: %.2f', [Result.Min]);
  LabelMax.Caption := Format('Maximum: %.2f', [Result.Max]);
  LabelAverage.Caption := Format('Moyenne: %.2f', [Result.Average]);
  LabelCount.Caption := Format('Nombre de points: %d', [Result.Count]);

  // Afficher la tendance avec une flèche
  if Result.Trend > 0.1 then
    LabelTrend.Caption := '↗ Tendance à la hausse'
  else if Result.Trend < -0.1 then
    LabelTrend.Caption := '↘ Tendance à la baisse'
  else
    LabelTrend.Caption := '→ Tendance stable';
end;
```

## 4. Serveur de chat simple

### Scénario

Une application serveur qui gère plusieurs connexions clientes simultanément. Chaque client peut envoyer des messages que le serveur diffuse à tous les autres clients.

### Solution avec multithreading

```pascal
type
  TClient = class
  private
    FSocket: TSocket;
    FAddress: string;
    FUsername: string;
    FConnected: Boolean;
  public
    constructor Create(ASocket: TSocket; const AAddress: string);
    property Socket: TSocket read FSocket;
    property Address: string read FAddress;
    property Username: string read FUsername write FUsername;
    property Connected: Boolean read FConnected write FConnected;
  end;

  TClientList = class
  private
    FList: TList<TClient>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Client: TClient);
    procedure Remove(Client: TClient);
    function Count: Integer;
    function GetClientAt(Index: Integer): TClient;
    procedure ForEach(Proc: TProc<TClient>);
  end;

  TChatServer = class
  private
    FServerSocket: TSocket;
    FPort: Integer;
    FClients: TClientList;
    FListenerThread: TThread;
    FRunning: Boolean;
    FOnLog: TProc<string>;

    procedure ListenerThreadProc;
    procedure HandleClient(Client: TClient);
    procedure BroadcastMessage(const Sender, Message: string);
    procedure Log(const Message: string);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;

    property OnLog: TProc<string> read FOnLog write FOnLog;
  end;

constructor TClient.Create(ASocket: TSocket; const AAddress: string);
begin
  inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FUsername := 'Anonymous';
  FConnected := True;
end;

constructor TClientList.Create;
begin
  inherited;
  FList := TList<TClient>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TClientList.Destroy;
begin
  FLock.Free;
  FList.Free;
  inherited;
end;

procedure TClientList.Add(Client: TClient);
begin
  FLock.Enter;
  try
    FList.Add(Client);
  finally
    FLock.Leave;
  end;
end;

procedure TClientList.Remove(Client: TClient);
begin
  FLock.Enter;
  try
    FList.Remove(Client);
  finally
    FLock.Leave;
  end;
end;

function TClientList.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FList.Count;
  finally
    FLock.Leave;
  end;
end;

function TClientList.GetClientAt(Index: Integer): TClient;
begin
  FLock.Enter;
  try
    Result := FList[Index];
  finally
    FLock.Leave;
  end;
end;

procedure TClientList.ForEach(Proc: TProc<TClient>);
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FList.Count - 1 do
      Proc(FList[i]);
  finally
    FLock.Leave;
  end;
end;

constructor TChatServer.Create(Port: Integer);
begin
  inherited Create;
  FPort := Port;
  FClients := TClientList.Create;
  FRunning := False;
end;

destructor TChatServer.Destroy;
begin
  Stop;
  FClients.Free;
  inherited;
end;

procedure TChatServer.Start;
begin
  if not FRunning then
  begin
    FRunning := True;

    // Initialiser le socket serveur
    FServerSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if FServerSocket = INVALID_SOCKET then
    begin
      Log('Erreur lors de la création du socket serveur');
      Exit;
    end;

    // Configurer l'adresse du serveur
    var Addr: TSockAddrIn;
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FPort);
    Addr.sin_addr.s_addr := INADDR_ANY;

    // Lier le socket à l'adresse
    if bind(FServerSocket, Addr, SizeOf(Addr)) = SOCKET_ERROR then
    begin
      Log('Erreur lors du bind du socket serveur');
      closesocket(FServerSocket);
      Exit;
    end;

    // Mettre le socket en écoute
    if listen(FServerSocket, SOMAXCONN) = SOCKET_ERROR then
    begin
      Log('Erreur lors de la mise en écoute du socket serveur');
      closesocket(FServerSocket);
      Exit;
    end;

    Log(Format('Serveur démarré sur le port %d', [FPort]));

    // Démarrer le thread d'écoute
    FListenerThread := TThread.CreateAnonymousThread(ListenerThreadProc);
    FListenerThread.FreeOnTerminate := False;
    FListenerThread.Start;
  end;
end;

procedure TChatServer.Stop;
begin
  if FRunning then
  begin
    FRunning := False;

    // Fermer le socket serveur pour débloquer accept()
    closesocket(FServerSocket);

    // Attendre que le thread d'écoute se termine
    if Assigned(FListenerThread) then
    begin
      FListenerThread.WaitFor;
      FListenerThread.Free;
      FListenerThread := nil;
    end;

    // Fermer toutes les connexions clientes
    FClients.ForEach(
      procedure(Client: TClient)
      begin
        closesocket(Client.Socket);
        Client.Free;
      end
    );

    Log('Serveur arrêté');
  end;
end;

procedure TChatServer.ListenerThreadProc;
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddrIn;
  ClientAddrLen: Integer;
  ClientAddress: string;
  NewClient: TClient;
begin
  ClientAddrLen := SizeOf(ClientAddr);

  while FRunning do
  begin
    // Accepter une nouvelle connexion
    ClientSocket := accept(FServerSocket, @ClientAddr, @ClientAddrLen);

    if ClientSocket <> INVALID_SOCKET then
    begin
      // Convertir l'adresse IP
      ClientAddress := inet_ntoa(ClientAddr.sin_addr) + ':' + IntToStr(ntohs(ClientAddr.sin_port));

      // Créer un nouvel objet client
      NewClient := TClient.Create(ClientSocket, ClientAddress);
      FClients.Add(NewClient);

      Log(Format('Nouveau client connecté: %s', [ClientAddress]));

      // Démarrer un thread pour gérer ce client
      TTask.Run(
        procedure
        begin
          HandleClient(NewClient);
        end
      );
    end
    else if FRunning then
    begin
      // Une erreur s'est produite (mais pas due à l'arrêt du serveur)
      Log('Erreur lors de l''acceptation d''une connexion');
      Sleep(100);
    end;
  end;
end;

procedure TChatServer.HandleClient(Client: TClient);
var
  Buffer: array[0..1023] of Char;
  BytesRead: Integer;
  Message, Username: string;
  Command: string;
begin
  try
    // Envoyer un message de bienvenue
    var WelcomeMsg := 'Bienvenue sur le serveur de chat! Utilisez /username <nom> pour changer votre nom.' + #13#10;
    send(Client.Socket, WelcomeMsg[1], Length(WelcomeMsg), 0);

    // Annoncer l'arrivée du client
    BroadcastMessage('Serveur', Format('Un nouveau client s''est connecté depuis %s', [Client.Address]));

    // Boucle de réception des messages
    while Client.Connected and FRunning do
    begin
      // Lire les données du client
      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesRead := recv(Client.Socket, Buffer, SizeOf(Buffer) - 1, 0);

      if BytesRead > 0 then
      begin
        // Convertir en chaîne
        Message := string(Buffer);

        // Traiter les commandes
        if Message.StartsWith('/') then
        begin
          // Extraire la commande
          var SpacePos := Pos(' ', Message);
          if SpacePos > 0 then
          begin
            Command := Copy(Message, 1, SpacePos - 1);
            Message := Trim(Copy(Message, SpacePos + 1, Length(Message)));
          end
          else
          begin
            Command := Message;
            Message := '';
          end;

          // Gérer les différentes commandes
          if Command = '/username' then
          begin
            if Message <> '' then
            begin
              Username := Client.Username;
              Client.Username := Message;
              BroadcastMessage('Serveur', Format('%s s''appelle maintenant %s', [Username, Message]));
            end;
          end
          else if Command = '/quit' then
          begin
            Client.Connected := False;
          end
          else
          begin
            // Commande inconnue
            var ErrorMsg := Format('Commande inconnue: %s'#13#10, [Command]);
            send(Client.Socket, ErrorMsg[1], Length(ErrorMsg), 0);
          end;
        end
        else
        begin
          // Message normal, le diffuser à tous
          BroadcastMessage(Client.Username, Message);
        end;
      end
      else
      begin
        // Erreur de lecture ou connexion fermée
        Client.Connected := False;
      end;
    end;
  finally
    // Nettoyer
    BroadcastMessage('Serveur', Format('%s s''est déconnecté', [Client.Username]));
    closesocket(Client.Socket);
    FClients.Remove(Client);
    Client.Free;
    Log(Format('Client déconnecté: %s', [Client.Address]));
  end;
end;

procedure TChatServer.BroadcastMessage(const Sender, Message: string);
var
  FullMessage: string;
begin
  FullMessage := Format('[%s] %s: %s'#13#10, [FormatDateTime('hh:nn:ss', Now), Sender, Message]);
  Log(FullMessage.Trim);

  // Envoyer à tous les clients
  FClients.ForEach(
    procedure(Client: TClient)
    begin
      send(Client.Socket, FullMessage[1], Length(FullMessage), 0);
    end
  );
end;

procedure TChatServer.Log(const Message: string);
begin
  if Assigned(FOnLog) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FOnLog(Message);
      end
    );
  end;
end;
```

### Utilisation dans un formulaire

```pascal
type
  TServerForm = class(TForm)
    EditPort: TEdit;
    ButtonStart: TButton;
    ButtonStop: TButton;
    MemoLog: TMemo;
    LabelStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    FServer: TChatServer;
    procedure ServerLog(const Message: string);
  end;

procedure TServerForm.FormCreate(Sender: TObject);
begin
  EditPort.Text := '8080';
  LabelStatus.Caption := 'Serveur arrêté';
  ButtonStop.Enabled := False;
end;

procedure TServerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
    FServer.Free;
end;

procedure TServerForm.ButtonStartClick(Sender: TObject);
var
  Port: Integer;
begin
  // Valider le port
  if not TryStrToInt(EditPort.Text, Port) or (Port < 1) or (Port > 65535) then
  begin
    ShowMessage('Veuillez entrer un numéro de port valide (1-65535)');
    Exit;
  end;

  // Créer et démarrer le serveur
  FServer := TChatServer.Create(Port);
  FServer.OnLog := ServerLog;
  FServer.Start;

  // Mettre à jour l'interface
  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
  EditPort.Enabled := False;
  LabelStatus.Caption := Format('Serveur en cours d''exécution sur le port %d', [Port]);
end;

procedure TServerForm.ButtonStopClick(Sender: TObject);
begin
  // Arrêter le serveur
  if Assigned(FServer) then
  begin
    FServer.Stop;
    FreeAndNil(FServer);
  end;

  // Mettre à jour l'interface
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  EditPort.Enabled := True;
  LabelStatus.Caption := 'Serveur arrêté';
end;

procedure TServerForm.ServerLog(const Message: string);
begin
  // Ajouter le message au mémo
  MemoLog.Lines.Add(Message);

  // Faire défiler jusqu'en bas
  SendMessage(MemoLog.Handle, EM_SCROLLCARET, 0, 0);
end;
```

## 5. Application de traitement d'images

### Scénario

Une application qui permet aux utilisateurs d'appliquer divers filtres et effets à des images. Les opérations de traitement d'image peuvent être longues, surtout pour les grandes images et les filtres complexes.

### Solution avec multithreading

```pascal
type
  TFilterType = (ftGrayscale, ftBlur, ftSharpen, ftSepia, ftNegative);

  TFilterTask = record
    Image: TBitmap;
    FilterType: TFilterType;
    Intensity: Integer; // Pour les filtres qui prennent un paramètre d'intensité
  end;

  TImageProcessor = class
  private
    FProcessingQueue: TThreadedQueue<TFilterTask>;
    FThreads: array of TThread;
    FThreadCount: Integer;
    FRunning: Boolean;
    FOnImageProcessed: TProc<TBitmap>;

    procedure ProcessorThreadProc;
    procedure ApplyFilter(var Task: TFilterTask);
  public
    constructor Create(ThreadCount: Integer = 0);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure ApplyFilterAsync(Image: TBitmap; FilterType: TFilterType; Intensity: Integer = 50);

    property OnImageProcessed: TProc<TBitmap> read FOnImageProcessed write FOnImageProcessed;
  end;

constructor TImageProcessor.Create(ThreadCount: Integer = 0);
begin
  inherited Create;

  // Déterminer le nombre de threads
  if ThreadCount <= 0 then
    FThreadCount := TThread.ProcessorCount
  else
    FThreadCount := ThreadCount;

  FProcessingQueue := TThreadedQueue<TFilterTask>.Create(100, INFINITE);
  FRunning := False;
end;

destructor TImageProcessor.Destroy;
begin
  Stop;
  FProcessingQueue.Free;
  inherited;
end;

procedure TImageProcessor.Start;
var
  i: Integer;
begin
  if not FRunning then
  begin
    FRunning := True;

    // Créer les threads de traitement
    SetLength(FThreads, FThreadCount);
    for i := 0 to FThreadCount - 1 do
    begin
      FThreads[i] := TThread.CreateAnonymousThread(ProcessorThreadProc);
      FThreads[i].FreeOnTerminate := False;
      FThreads[i].Start;
    end;
  end;
end;

procedure TImageProcessor.Stop;
var
  i: Integer;
  EmptyTask: TFilterTask;
begin
  if FRunning then
  begin
    FRunning := False;

    // Réveiller tous les threads en attente
    for i := 0 to FThreadCount - 1 do
      FProcessingQueue.PushItem(EmptyTask);

    // Attendre que tous les threads se terminent
    for i := 0 to FThreadCount - 1 do
    begin
      FThreads[i].WaitFor;
      FThreads[i].Free;
    end;

    SetLength(FThreads, 0);
  end;
end;

procedure TImageProcessor.ApplyFilterAsync(Image: TBitmap; FilterType: TFilterType; Intensity: Integer = 50);
var
  Task: TFilterTask;
  CopyImage: TBitmap;
begin
  // Créer une copie de l'image
  CopyImage := TBitmap.Create;
  CopyImage.Assign(Image);

  // Créer la tâche
  Task.Image := CopyImage;
  Task.FilterType := FilterType;
  Task.Intensity := Intensity;

  // Ajouter à la file d'attente
  FProcessingQueue.PushItem(Task);
end;

procedure TImageProcessor.ProcessorThreadProc;
var
  Task: TFilterTask;
  Result: TWaitResult;
begin
  while FRunning do
  begin
    // Attendre une tâche
    Result := FProcessingQueue.PopItem(Task);

    if (Result = wrSignaled) and FRunning and (Task.Image <> nil) then
    begin
      try
        // Appliquer le filtre
        ApplyFilter(Task);

        // Notifier que l'image a été traitée
        if Assigned(FOnImageProcessed) and FRunning then
        begin
          TThread.Queue(nil,
            procedure
            begin
              FOnImageProcessed(Task.Image);
            end
          );
        end;
      except
        // Gérer les erreurs
        Task.Image.Free;
      end;
    end;
  end;
end;

procedure TImageProcessor.ApplyFilter(var Task: TFilterTask);
var
  x, y: Integer;
  Pixel: TRGBTriple;
  Gray: Byte;
begin
  // Convertir l'image en format 24 bits si nécessaire
  if Task.Image.PixelFormat <> pf24bit then
    Task.Image.PixelFormat := pf24bit;

  // Appliquer le filtre en fonction du type
  case Task.FilterType of
    ftGrayscale:
      begin
        // Parcourir tous les pixels
        for y := 0 to Task.Image.Height - 1 do
        begin
          var ScanLine := Task.Image.ScanLine[y];
          for x := 0 to Task.Image.Width - 1 do
          begin
            Pixel := PScanline(x)^;
            // Convertir en niveaux de gris
            Gray := Round(0.299 * Pixel.rgbtRed + 0.587 * Pixel.rgbtGreen + 0.114 * Pixel.rgbtBlue);
            Pixel.rgbtRed := Gray;
            Pixel.rgbtGreen := Gray;
            Pixel.rgbtBlue := Gray;
            PScanline(x)^ := Pixel;
          end;
        end;
      end;

    ftSepia:
      begin
        // Parcourir tous les pixels
        for y := 0 to Task.Image.Height - 1 do
        begin
          var ScanLine := Task.Image.ScanLine[y];
          for x := 0 to Task.Image.Width - 1 do
          begin
            Pixel := PScanline(x)^;

            // Convertir en niveaux de gris
            Gray := Round(0.299 * Pixel.rgbtRed + 0.587 * Pixel.rgbtGreen + 0.114 * Pixel.rgbtBlue);

            // Appliquer l'effet sépia
            Pixel.rgbtRed := Min(255, Round(Gray * 1.4));
            Pixel.rgbtGreen := Min(255, Round(Gray * 1.2));
            Pixel.rgbtBlue := Min(255, Round(Gray * 0.8));

            PScanline(x)^ := Pixel;
          end;
        end;
      end;

    ftNegative:
      begin
        // Parcourir tous les pixels
        for y := 0 to Task.Image.Height - 1 do
        begin
          var ScanLine := Task.Image.ScanLine[y];
          for x := 0 to Task.Image.Width - 1 do
          begin
            Pixel := PScanline(x)^;

            // Inverser chaque composante
            Pixel.rgbtRed := 255 - Pixel.rgbtRed;
            Pixel.rgbtGreen := 255 - Pixel.rgbtGreen;
            Pixel.rgbtBlue := 255 - Pixel.rgbtBlue;

            PScanline(x)^ := Pixel;
          end;
        end;
      end;

    // Implémentez d'autres filtres selon vos besoins...
  end;
end;
```

### Utilisation dans un formulaire

```pascal
type
  TImageForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    ButtonLoad: TButton;
    ButtonGrayscale: TButton;
    ButtonSepia: TButton;
    ButtonNegative: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ButtonSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonGrayscaleClick(Sender: TObject);
    procedure ButtonSepiaClick(Sender: TObject);
    procedure ButtonNegativeClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    FImageProcessor: TImageProcessor;
    FOriginalImage: TBitmap;
    FProcessedImage: TBitmap;
    procedure OnImageProcessed(Image: TBitmap);
    procedure EnableFilterButtons(Enable: Boolean);
  end;

procedure TImageForm.FormCreate(Sender: TObject);
begin
  // Créer le processeur d'images
  FImageProcessor := TImageProcessor.Create;
  FImageProcessor.OnImageProcessed := OnImageProcessed;
  FImageProcessor.Start;

  // Initialiser les images
  FOriginalImage := nil;
  FProcessedImage := nil;

  // Désactiver les boutons de filtre
  EnableFilterButtons(False);
end;

procedure TImageForm.FormDestroy(Sender: TObject);
begin
  FImageProcessor.Free;

  if Assigned(FOriginalImage) then
    FOriginalImage.Free;

  if Assigned(FProcessedImage) then
    FProcessedImage.Free;
end;

procedure TImageForm.ButtonLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    // Charger l'image
    if Assigned(FOriginalImage) then
      FOriginalImage.Free;

    FOriginalImage := TBitmap.Create;
    FOriginalImage.LoadFromFile(OpenDialog1.FileName);

    // Afficher l'image
    Image1.Picture.Assign(FOriginalImage);

    // Activer les boutons de filtre
    EnableFilterButtons(True);
  end;
end;

procedure TImageForm.ButtonGrayscaleClick(Sender: TObject);
begin
  if Assigned(FOriginalImage) then
  begin
    EnableFilterButtons(False);
    FImageProcessor.ApplyFilterAsync(FOriginalImage, ftGrayscale);
  end;
end;

procedure TImageForm.ButtonSepiaClick(Sender: TObject);
begin
  if Assigned(FOriginalImage) then
  begin
    EnableFilterButtons(False);
    FImageProcessor.ApplyFilterAsync(FOriginalImage, ftSepia);
  end;
end;

procedure TImageForm.ButtonNegativeClick(Sender: TObject);
begin
  if Assigned(FOriginalImage) then
  begin
    EnableFilterButtons(False);
    FImageProcessor.ApplyFilterAsync(FOriginalImage, ftNegative);
  end;
end;

procedure TImageForm.ButtonSaveClick(Sender: TObject);
begin
  if Assigned(FProcessedImage) and SaveDialog1.Execute then
  begin
    FProcessedImage.SaveToFile(SaveDialog1.FileName);
    ShowMessage('Image sauvegardée avec succès !');
  end;
end;

procedure TImageForm.OnImageProcessed(Image: TBitmap);
begin
  // Stocker l'image traitée
  if Assigned(FProcessedImage) then
    FProcessedImage.Free;

  FProcessedImage := Image;

  // Afficher l'image traitée
  Image1.Picture.Assign(FProcessedImage);

  // Réactiver les boutons
  EnableFilterButtons(True);
  ButtonSave.Enabled := True;
end;

procedure TImageForm.EnableFilterButtons(Enable: Boolean);
begin
  ButtonGrayscale.Enabled := Enable;
  ButtonSepia.Enabled := Enable;
  ButtonNegative.Enabled := Enable;
  ButtonSave.Enabled := Enable and Assigned(FProcessedImage);
end;
```

## Résumé

Dans ce chapitre, nous avons exploré plusieurs cas d'usage concrets du multithreading en Delphi :

1. **Application de traitement par lots de fichiers** : Utilisation d'un pool de threads pour traiter efficacement de grands ensembles de fichiers.

2. **Téléchargement parallèle de fichiers** : Implémentation d'un gestionnaire de téléchargements parallèles avec limitation du nombre de téléchargements simultanés.

3. **Analyse de données en temps réel** : Création d'un système qui analyse continuellement un flux de données sans bloquer l'interface utilisateur.

4. **Serveur de chat simple** : Utilisation de threads pour gérer simultanément plusieurs connexions clientes.

5. **Application de traitement d'images** : Mise en œuvre d'un système qui applique des filtres à des images en arrière-plan.

Ces exemples illustrent comment le multithreading peut améliorer significativement les performances et la réactivité des applications Delphi dans diverses situations. Les techniques présentées peuvent être adaptées et combinées pour répondre aux besoins spécifiques de vos propres projets.

## Exercice pratique

Développez une application qui combine plusieurs des techniques vues dans ce chapitre. Par exemple, une application de surveillance de dossier qui :

1. Surveille un ou plusieurs dossiers pour détecter de nouveaux fichiers
2. Analyse automatiquement les nouveaux fichiers (par exemple, extraction de métadonnées)
3. Génère des miniatures pour les images
4. Envoie des notifications lorsque certains types de fichiers sont détectés
5. Permet de rechercher des fichiers selon divers critères

Cet exercice vous permettra d'appliquer les concepts de multithreading dans un contexte plus complet et intégré.

Dans le prochain chapitre, nous explorerons la programmation réactive avec le pattern Observer, qui constitue une autre approche puissante pour gérer les événements et les flux de données dans les applications Delphi.
