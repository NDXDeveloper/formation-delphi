# 7. Gestion des fichiers et flux de données

## 7.6 Traitement par lots (Batch)

Le traitement par lots, ou traitement batch, consiste à effectuer une série d'opérations sur un ensemble de fichiers ou de données, généralement sans intervention de l'utilisateur. Cette approche est particulièrement utile pour automatiser des tâches répétitives ou pour traiter de grandes quantités de données.

### Introduction au traitement par lots

Les applications de traitement par lots sont utilisées dans de nombreux contextes :
- Conversion de fichiers (images, documents, etc.)
- Importation ou exportation de données
- Traitement nocturne de données d'entreprise
- Sauvegarde automatique de fichiers
- Génération de rapports
- Nettoyage et maintenance de systèmes

### Principes fondamentaux

Un traitement par lots efficace repose sur quelques principes clés :

1. **Automatisation** : Minimiser l'intervention humaine
2. **Journalisation** : Conserver une trace des opérations effectuées
3. **Gestion des erreurs** : Anticiper et gérer les problèmes sans interrompre le processus
4. **Performances** : Optimiser l'utilisation des ressources
5. **Flexibilité** : Permettre la configuration des traitements

### Traitement par lots de fichiers

#### Exemple simple : Renommer des fichiers

Voici un exemple simple pour renommer tous les fichiers d'un dossier en ajoutant un préfixe :

```pascal
procedure RenommerLotFichiers(const Dossier, Prefixe: string);
var
  Fichiers: TStringDynArray;
  Fichier, NouveauNom: string;
  Journal: TStringList;
  NbSucces, NbEchecs: Integer;
begin
  // Initialiser le journal
  Journal := TStringList.Create;
  try
    Journal.Add('Début du traitement : ' + DateTimeToStr(Now));
    Journal.Add('Dossier: ' + Dossier);
    Journal.Add('Préfixe: ' + Prefixe);
    Journal.Add('---------------------------------');

    NbSucces := 0;
    NbEchecs := 0;

    // Obtenir la liste des fichiers
    Fichiers := TDirectory.GetFiles(Dossier);

    // Traiter chaque fichier
    for Fichier in Fichiers do
    begin
      try
        // Calculer le nouveau nom
        NouveauNom := IncludeTrailingPathDelimiter(Dossier) + Prefixe +
                      ExtractFileName(Fichier);

        // Vérifier si le nouveau nom existe déjà
        if FileExists(NouveauNom) then
        begin
          Journal.Add('ÉCHEC - Le fichier existe déjà: ' + NouveauNom);
          Inc(NbEchecs);
          Continue;
        end;

        // Renommer le fichier
        if RenameFile(Fichier, NouveauNom) then
        begin
          Journal.Add('SUCCÈS - Renommé: ' + ExtractFileName(Fichier) +
                      ' -> ' + Prefixe + ExtractFileName(Fichier));
          Inc(NbSucces);
        end
        else
        begin
          Journal.Add('ÉCHEC - Impossible de renommer: ' + ExtractFileName(Fichier));
          Inc(NbEchecs);
        end;
      except
        on E: Exception do
        begin
          Journal.Add('ERREUR - ' + ExtractFileName(Fichier) + ': ' + E.Message);
          Inc(NbEchecs);
        end;
      end;
    end;

    // Ajouter le résumé
    Journal.Add('---------------------------------');
    Journal.Add('Fin du traitement : ' + DateTimeToStr(Now));
    Journal.Add('Fichiers traités: ' + IntToStr(Length(Fichiers)));
    Journal.Add('Succès: ' + IntToStr(NbSucces));
    Journal.Add('Échecs: ' + IntToStr(NbEchecs));

    // Sauvegarder le journal
    Journal.SaveToFile(IncludeTrailingPathDelimiter(Dossier) +
                        'journal_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt');
  finally
    Journal.Free;
  end;
end;
```

Utilisation :

```pascal
procedure TForm1.ButtonRenommerClick(Sender: TObject);
var
  Dossier: string;
begin
  // Demander le dossier à traiter
  if SelectDirectory('Sélectionnez un dossier', '', Dossier) then
  begin
    // Demander le préfixe à ajouter
    var Prefixe := InputBox('Préfixe', 'Entrez le préfixe à ajouter:', 'PREFIX_');

    // Lancer le traitement
    RenommerLotFichiers(Dossier, Prefixe);

    ShowMessage('Traitement terminé !');
  end;
end;
```

#### Traitement par lots avec barre de progression

Pour les traitements plus longs, il est utile d'afficher une barre de progression :

```pascal
procedure TraiterLotsAvecProgression(const Dossier: string;
                                     ProgressBar: TProgressBar;
                                     LabelStatus: TLabel);
var
  Fichiers: TStringDynArray;
  Fichier: string;
  i: Integer;
begin
  // Obtenir la liste des fichiers
  Fichiers := TDirectory.GetFiles(Dossier, '*.jpg');

  // Configurer la barre de progression
  ProgressBar.Min := 0;
  ProgressBar.Max := Length(Fichiers);
  ProgressBar.Position := 0;

  LabelStatus.Caption := 'Initialisation...';
  Application.ProcessMessages;  // Mettre à jour l'interface

  // Traiter chaque fichier
  for i := 0 to Length(Fichiers) - 1 do
  begin
    Fichier := Fichiers[i];

    // Mettre à jour le statut
    LabelStatus.Caption := 'Traitement de ' + ExtractFileName(Fichier) +
                           ' (' + IntToStr(i+1) + '/' + IntToStr(Length(Fichiers)) + ')';
    Application.ProcessMessages;  // Mettre à jour l'interface

    try
      // Effectuer le traitement (par exemple, redimensionner une image)
      RedimensionnerImage(Fichier, Fichier + '.resized.jpg', 800, 600);

      // Pause pour simuler un traitement plus long (à supprimer en production)
      Sleep(100);
    except
      on E: Exception do
        // Journaliser l'erreur mais continuer le traitement
        Memo1.Lines.Add('Erreur: ' + ExtractFileName(Fichier) + ' - ' + E.Message);
    end;

    // Mettre à jour la barre de progression
    ProgressBar.Position := i + 1;
    Application.ProcessMessages;  // Mettre à jour l'interface
  end;

  LabelStatus.Caption := 'Traitement terminé !';
end;
```

> **Note :** L'utilisation de `Application.ProcessMessages` permet de maintenir l'interface réactive pendant un traitement long. Cependant, cela peut introduire des problèmes si l'utilisateur interagit avec l'application pendant le traitement. Pour les applications professionnelles, préférez l'utilisation de threads comme nous le verrons plus loin.

#### Traitement multithreadé pour de meilleures performances

Pour les traitements intensifs ou sur de grandes quantités de données, l'utilisation de threads permet de mieux exploiter les processeurs multi-cœurs :

```pascal
type
  TTraitementThread = class(TThread)
  private
    FDossierSource: string;
    FDossierDestination: string;
    FIndex, FTotal: Integer;
    FNomFichier: string;
    FOnProgression: TProc<Integer, Integer, string>;
  protected
    procedure Execute; override;
    procedure AfficherProgression;
  public
    constructor Create(const DossierSource, DossierDestination: string;
                      Index, Total: Integer;
                      const NomFichier: string;
                      OnProgression: TProc<Integer, Integer, string>);
  end;

constructor TTraitementThread.Create(const DossierSource, DossierDestination: string;
                                    Index, Total: Integer;
                                    const NomFichier: string;
                                    OnProgression: TProc<Integer, Integer, string>);
begin
  FDossierSource := DossierSource;
  FDossierDestination := DossierDestination;
  FIndex := Index;
  FTotal := Total;
  FNomFichier := NomFichier;
  FOnProgression := OnProgression;

  FreeOnTerminate := True;  // Le thread se libère automatiquement
  inherited Create(False);  // Démarrer immédiatement
end;

procedure TTraitementThread.Execute;
var
  FichierSource, FichierDestination: string;
begin
  try
    FichierSource := IncludeTrailingPathDelimiter(FDossierSource) + FNomFichier;
    FichierDestination := IncludeTrailingPathDelimiter(FDossierDestination) + FNomFichier;

    // Effectuer le traitement (par exemple, redimensionner une image)
    RedimensionnerImage(FichierSource, FichierDestination, 800, 600);

    // Notifier la progression en synchronisant avec le thread principal
    Synchronize(AfficherProgression);
  except
    // Gérer les erreurs...
  end;
end;

procedure TTraitementThread.AfficherProgression;
begin
  if Assigned(FOnProgression) then
    FOnProgression(FIndex, FTotal, FNomFichier);
end;

// Utilisation des threads
procedure TraiterLotsMultithread(const DossierSource, DossierDestination: string;
                                ProgressBar: TProgressBar; LabelStatus: TLabel);
var
  Fichiers: TStringDynArray;
  Threads: array of TTraitementThread;
  ThreadsActifs, i, NbThreadsMax, FichiersTraites: Integer;
begin
  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(DossierDestination) then
    ForceDirectories(DossierDestination);

  // Obtenir la liste des fichiers
  Fichiers := TDirectory.GetFiles(DossierSource, '*.jpg');

  // Configurer la barre de progression
  ProgressBar.Min := 0;
  ProgressBar.Max := Length(Fichiers);
  ProgressBar.Position := 0;

  // Définir le nombre maximum de threads en fonction des cœurs du processeur
  NbThreadsMax := TThread.ProcessorCount;
  if NbThreadsMax > 8 then NbThreadsMax := 8;  // Limiter à 8 threads maximum

  // Créer le tableau de threads
  SetLength(Threads, NbThreadsMax);

  // Callback de progression
  var OnProgression := procedure(Index, Total: Integer; FileName: string)
  begin
    ProgressBar.Position := FichiersTraites + 1;
    LabelStatus.Caption := 'Traitement de ' + FileName +
                           ' (' + IntToStr(FichiersTraites + 1) + '/' +
                           IntToStr(Length(Fichiers)) + ')';
    Inc(FichiersTraites);
  end;

  // Initialisation
  FichiersTraites := 0;
  i := 0;

  // Tant qu'il reste des fichiers à traiter
  while (i < Length(Fichiers)) or (FichiersTraites < Length(Fichiers)) do
  begin
    // Compter les threads actifs
    ThreadsActifs := 0;
    for var j := 0 to NbThreadsMax - 1 do
    begin
      if Assigned(Threads[j]) and not Threads[j].Finished then
        Inc(ThreadsActifs);
    end;

    // Lancer de nouveaux threads si possible
    while (ThreadsActifs < NbThreadsMax) and (i < Length(Fichiers)) do
    begin
      // Trouver un emplacement libre dans le tableau de threads
      var ThreadIndex := -1;
      for var j := 0 to NbThreadsMax - 1 do
      begin
        if not Assigned(Threads[j]) or Threads[j].Finished then
        begin
          ThreadIndex := j;
          Break;
        end;
      end;

      if ThreadIndex >= 0 then
      begin
        // Créer un nouveau thread de traitement
        Threads[ThreadIndex] := TTraitementThread.Create(
          DossierSource, DossierDestination, i, Length(Fichiers),
          ExtractFileName(Fichiers[i]), OnProgression);

        Inc(i);
        Inc(ThreadsActifs);
      end;
    end;

    // Petite pause pour éviter de saturer le CPU avec cette boucle
    Sleep(50);
    Application.ProcessMessages;  // Maintenir l'interface réactive
  end;

  LabelStatus.Caption := 'Traitement terminé !';
end;
```

### Traitement par lots avec configuration

Pour plus de flexibilité, il est judicieux de permettre la configuration des traitements par lots :

```pascal
type
  TOptionsTraitement = record
    DossierSource: string;
    DossierDestination: string;
    FiltreFichiers: string;
    IncluireSousDossiers: Boolean;
    Redimensionner: Boolean;
    LargeurMax: Integer;
    HauteurMax: Integer;
    Prefixe: string;
    Suffixe: string;
    ConvertirFormat: Boolean;
    FormatCible: string;  // 'JPG', 'PNG', etc.
    Qualite: Integer;     // Pour JPG (1-100)
  end;

procedure TraiterLotsAvecOptions(const Options: TOptionsTraitement;
                                ProgressBar: TProgressBar;
                                LabelStatus: TLabel;
                                MemoLog: TMemo);
var
  Fichiers: TStringDynArray;
  Fichier, FichierDestination, Extension: string;
  i: Integer;
begin
  // Ajouter l'en-tête au journal
  MemoLog.Lines.Add('Début du traitement: ' + DateTimeToStr(Now));
  MemoLog.Lines.Add('Dossier source: ' + Options.DossierSource);
  MemoLog.Lines.Add('Dossier destination: ' + Options.DossierDestination);
  MemoLog.Lines.Add('Filtre: ' + Options.FiltreFichiers);
  MemoLog.Lines.Add('---------------------------------');

  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(Options.DossierDestination) then
    ForceDirectories(Options.DossierDestination);

  // Obtenir la liste des fichiers
  if Options.IncluireSousDossiers then
    Fichiers := TDirectory.GetFiles(Options.DossierSource, Options.FiltreFichiers,
                                   TSearchOption.soAllDirectories)
  else
    Fichiers := TDirectory.GetFiles(Options.DossierSource, Options.FiltreFichiers,
                                   TSearchOption.soTopDirectoryOnly);

  // Configurer la barre de progression
  ProgressBar.Min := 0;
  ProgressBar.Max := Length(Fichiers);
  ProgressBar.Position := 0;

  // Traiter chaque fichier
  for i := 0 to Length(Fichiers) - 1 do
  begin
    Fichier := Fichiers[i];

    // Mettre à jour le statut
    LabelStatus.Caption := 'Traitement de ' + ExtractFileName(Fichier) +
                           ' (' + IntToStr(i+1) + '/' + IntToStr(Length(Fichiers)) + ')';
    Application.ProcessMessages();

    try
      // Déterminer le nom du fichier de destination
      if Options.ConvertirFormat then
        Extension := '.' + LowerCase(Options.FormatCible)
      else
        Extension := ExtractFileExt(Fichier);

      FichierDestination := IncludeTrailingPathDelimiter(Options.DossierDestination) +
                            Options.Prefixe +
                            ChangeFileExt(ExtractFileName(Fichier), '') +
                            Options.Suffixe +
                            Extension;

      // Traitement de l'image selon les options
      if Options.Redimensionner then
      begin
        var Image := TImage.Create(nil);
        try
          Image.Picture.LoadFromFile(Fichier);

          var BitmapOriginal := TBitmap.Create;
          try
            BitmapOriginal.Assign(Image.Picture.Graphic);

            var LargeurOrig := BitmapOriginal.Width;
            var HauteurOrig := BitmapOriginal.Height;
            var NouvelleHauteur, NouvelleLargeur: Integer;

            // Calculer les nouvelles dimensions en préservant le ratio
            if (LargeurOrig > Options.LargeurMax) or (HauteurOrig > Options.HauteurMax) then
            begin
              if LargeurOrig / HauteurOrig > Options.LargeurMax / Options.HauteurMax then
              begin
                NouvelleLargeur := Options.LargeurMax;
                NouvelleHauteur := Round(HauteurOrig * Options.LargeurMax / LargeurOrig);
              end
              else
              begin
                NouvelleHauteur := Options.HauteurMax;
                NouvelleLargeur := Round(LargeurOrig * Options.HauteurMax / HauteurOrig);
              end;
            end
            else
            begin
              NouvelleLargeur := LargeurOrig;
              NouvelleHauteur := HauteurOrig;
            end;

            // Redimensionner l'image
            var BitmapRedim := TBitmap.Create;
            try
              BitmapRedim.SetSize(NouvelleLargeur, NouvelleHauteur);
              BitmapRedim.Canvas.StretchDraw(Rect(0, 0, NouvelleLargeur, NouvelleHauteur),
                                            BitmapOriginal);

              // Sauvegarder selon le format cible
              if Options.ConvertirFormat and (LowerCase(Options.FormatCible) = 'jpg') then
              begin
                var Jpeg := TJPEGImage.Create;
                try
                  Jpeg.Assign(BitmapRedim);
                  Jpeg.CompressionQuality := Options.Qualite;
                  Jpeg.SaveToFile(FichierDestination);
                finally
                  Jpeg.Free;
                end;
              end
              else if Options.ConvertirFormat and (LowerCase(Options.FormatCible) = 'png') then
              begin
                var Png := TPngImage.Create;
                try
                  Png.Assign(BitmapRedim);
                  Png.SaveToFile(FichierDestination);
                finally
                  Png.Free;
                end;
              end
              else
              begin
                BitmapRedim.SaveToFile(FichierDestination);
              end;
            finally
              BitmapRedim.Free;
            end;
          finally
            BitmapOriginal.Free;
          end;
        finally
          Image.Free;
        end;
      end
      else
      begin
        // Simplement copier le fichier sans redimensionnement
        TFile.Copy(Fichier, FichierDestination, True);
      end;

      MemoLog.Lines.Add('SUCCÈS - ' + ExtractFileName(Fichier) + ' -> ' +
                        ExtractFileName(FichierDestination));
    except
      on E: Exception do
      begin
        MemoLog.Lines.Add('ERREUR - ' + ExtractFileName(Fichier) + ': ' + E.Message);
      end;
    end;

    // Mettre à jour la barre de progression
    ProgressBar.Position := i + 1;
    Application.ProcessMessages();
  end;

  // Ajouter le résumé au journal
  MemoLog.Lines.Add('---------------------------------');
  MemoLog.Lines.Add('Fin du traitement: ' + DateTimeToStr(Now));
  MemoLog.Lines.Add('Fichiers traités: ' + IntToStr(Length(Fichiers)));

  LabelStatus.Caption := 'Traitement terminé !';
end;
```

### Interface utilisateur pour traitement par lots

Voici un exemple d'interface utilisateur pour configurer et lancer un traitement par lots :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.FileCtrl, Vcl.Buttons;

type
  TOptionsTraitement = record
    DossierSource: string;
    DossierDestination: string;
    FiltreFichiers: string;
    IncluireSousDossiers: Boolean;
    Redimensionner: Boolean;
    LargeurMax: Integer;
    HauteurMax: Integer;
    Prefixe: string;
    Suffixe: string;
    ConvertirFormat: Boolean;
    FormatCible: string;
    Qualite: Integer;
  end;

  TfrmMain = class(TForm)
    pnlConfig: TPanel;
    grpSource: TGroupBox;
    lblDossierSource: TLabel;
    edtDossierSource: TEdit;
    btnChoisirSource: TButton;
    lblFiltre: TLabel;
    edtFiltre: TEdit;
    chkSousDossiers: TCheckBox;
    grpDestination: TGroupBox;
    lblDossierDestination: TLabel;
    edtDossierDestination: TEdit;
    btnChoisirDestination: TButton;
    grpOptions: TGroupBox;
    chkRedimensionner: TCheckBox;
    lblLargeur: TLabel;
    edtLargeur: TEdit;
    lblHauteur: TLabel;
    edtHauteur: TEdit;
    lblPrefixe: TLabel;
    edtPrefixe: TEdit;
    lblSuffixe: TLabel;
    edtSuffixe: TEdit;
    chkConvertir: TCheckBox;
    cboFormat: TComboBox;
    lblQualite: TLabel;
    trkQualite: TTrackBar;
    pnlTraitement: TPanel;
    btnDemarrer: TButton;
    btnArreter: TButton;
    pbProgression: TProgressBar;
    lblStatus: TLabel;
    pnlJournal: TPanel;
    memoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnChoisirSourceClick(Sender: TObject);
    procedure btnChoisirDestinationClick(Sender: TObject);
    procedure chkRedimensionnerClick(Sender: TObject);
    procedure chkConvertirClick(Sender: TObject);
    procedure btnDemarrerClick(Sender: TObject);
    procedure btnArreterClick(Sender: TObject);
  private
    FTraitementEnCours: Boolean;
    FOptionsTraitement: TOptionsTraitement;
    procedure InitialiserInterface;
    procedure MettreAJourInterface;
    procedure RecupererOptions;
    procedure TraiterLots;
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitialiserInterface;
  FTraitementEnCours := False;
end;

procedure TfrmMain.InitialiserInterface;
begin
  // Initialiser les valeurs par défaut
  edtFiltre.Text := '*.jpg;*.jpeg;*.png;*.bmp';
  chkSousDossiers.Checked := False;

  chkRedimensionner.Checked := False;
  edtLargeur.Text := '800';
  edtHauteur.Text := '600';
  edtLargeur.Enabled := False;
  edtHauteur.Enabled := False;
  lblLargeur.Enabled := False;
  lblHauteur.Enabled := False;

  edtPrefixe.Text := '';
  edtSuffixe.Text := '_resized';

  chkConvertir.Checked := False;
  cboFormat.Items.Clear;
  cboFormat.Items.Add('JPG');
  cboFormat.Items.Add('PNG');
  cboFormat.Items.Add('BMP');
  cboFormat.ItemIndex := 0;
  cboFormat.Enabled := False;
  lblQualite.Enabled := False;
  trkQualite.Enabled := False;
  trkQualite.Min := 1;
  trkQualite.Max := 100;
  trkQualite.Position := 80;

  btnArreter.Enabled := False;
  pbProgression.Position := 0;
  lblStatus.Caption := 'Prêt';

  memoLog.Clear;
end;

procedure TfrmMain.MettreAJourInterface;
begin
  // Activer/désactiver les contrôles selon les options choisies
  edtLargeur.Enabled := chkRedimensionner.Checked;
  edtHauteur.Enabled := chkRedimensionner.Checked;
  lblLargeur.Enabled := chkRedimensionner.Checked;
  lblHauteur.Enabled := chkRedimensionner.Checked;

  cboFormat.Enabled := chkConvertir.Checked;
  lblQualite.Enabled := chkConvertir.Checked and
                        (LowerCase(cboFormat.Text) = 'jpg');
  trkQualite.Enabled := lblQualite.Enabled;
end;

procedure TfrmMain.RecupererOptions;
begin
  // Récupérer les options depuis l'interface
  FOptionsTraitement.DossierSource := edtDossierSource.Text;
  FOptionsTraitement.DossierDestination := edtDossierDestination.Text;
  FOptionsTraitement.FiltreFichiers := edtFiltre.Text;
  FOptionsTraitement.IncluireSousDossiers := chkSousDossiers.Checked;

  FOptionsTraitement.Redimensionner := chkRedimensionner.Checked;
  FOptionsTraitement.LargeurMax := StrToIntDef(edtLargeur.Text, 800);
  FOptionsTraitement.HauteurMax := StrToIntDef(edtHauteur.Text, 600);

  FOptionsTraitement.Prefixe := edtPrefixe.Text;
  FOptionsTraitement.Suffixe := edtSuffixe.Text;

  FOptionsTraitement.ConvertirFormat := chkConvertir.Checked;
  FOptionsTraitement.FormatCible := cboFormat.Text;
  FOptionsTraitement.Qualite := trkQualite.Position;
end;

procedure TfrmMain.btnChoisirSourceClick(Sender: TObject);
var
  Dossier: string;
begin
  if SelectDirectory('Sélectionnez le dossier source', '', Dossier) then
    edtDossierSource.Text := Dossier;
end;

procedure TfrmMain.btnChoisirDestinationClick(Sender: TObject);
var
  Dossier: string;
begin
  if SelectDirectory('Sélectionnez le dossier de destination', '', Dossier) then
    edtDossierDestination.Text := Dossier;
end;

procedure TfrmMain.chkRedimensionnerClick(Sender: TObject);
begin
  MettreAJourInterface;
end;

procedure TfrmMain.chkConvertirClick(Sender: TObject);
begin
  MettreAJourInterface;
end;

procedure TfrmMain.btnDemarrerClick(Sender: TObject);
begin
  // Vérifier les dossiers
  if Trim(edtDossierSource.Text) = '' then
  begin
    ShowMessage('Veuillez sélectionner un dossier source !');
    Exit;
  end;

  if not DirectoryExists(edtDossierSource.Text) then
  begin
    ShowMessage('Le dossier source n''existe pas !');
    Exit;
  end;

  if Trim(edtDossierDestination.Text) = '' then
  begin
    ShowMessage('Veuillez sélectionner un dossier de destination !');
    Exit;
  end;

  // Préparer l'interface pour le traitement
  btnDemarrer.Enabled := False;
  btnArreter.Enabled := True;
  FTraitementEnCours := True;

  // Récupérer les options
  RecupererOptions;

  // Lancer le traitement dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        TraiterLots;
      finally
        // Rétablir l'interface après le traitement
        TThread.Synchronize(nil,
          procedure
          begin
            btnDemarrer.Enabled := True;
            btnArreter.Enabled := False;
            FTraitementEnCours := False;
            lblStatus.Caption := 'Traitement terminé !';
          end);
      end;
    end).Start;
end;

procedure TfrmMain.btnArreterClick(Sender: TObject);
begin
  if FTraitementEnCours then
  begin
    if MessageDlg('Êtes-vous sûr de vouloir arrêter le traitement en cours ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FTraitementEnCours := False;
      lblStatus.Caption := 'Traitement interrompu';
      memoLog.Lines.Add('INTERRUPTION - Traitement arrêté par l''utilisateur');
    end;
  end;
end;

procedure TfrmMain.TraiterLots;
var
  Fichiers: TStringDynArray;
  Fichier, FichierDestination, Extension: string;
  i: Integer;
begin
  // Ajouter l'en-tête au journal
  TThread.Synchronize(nil,
    procedure
    begin
      memoLog.Lines.Clear;
      memoLog.Lines.Add('Début du traitement: ' + DateTimeToStr(Now));
      memoLog.Lines.Add('Dossier source: ' + FOptionsTraitement.DossierSource);
      memoLog.Lines.Add('Dossier destination: ' + FOptionsTraitement.DossierDestination);
      memoLog.Lines.Add('Filtre: ' + FOptionsTraitement.FiltreFichiers);
      memoLog.Lines.Add('---------------------------------');
    end);

  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(FOptionsTraitement.DossierDestination) then
    ForceDirectories(FOptionsTraitement.DossierDestination);

  // Obtenir la liste des fichiers selon les filtres
  var Filtres := FOptionsTraitement.FiltreFichiers.Split([';']);
  Fichiers := [];

  // Pour chaque filtre, ajouter les fichiers correspondants
  for var Filtre in Filtres do
  begin
    var FichiersDuFiltre: TStringDynArray;
    if FOptionsTraitement.IncluireSousDossiers then
      FichiersDuFiltre := TDirectory.GetFiles(FOptionsTraitement.DossierSource,
                                           Trim(Filtre),
                                           TSearchOption.soAllDirectories)
    else
      FichiersDuFiltre := TDirectory.GetFiles(FOptionsTraitement.DossierSource,
                                           Trim(Filtre),
                                           TSearchOption.soTopDirectoryOnly);

    // Ajouter les fichiers trouvés à la liste principale
    for var FichierTrouve in FichiersDuFiltre do
      Fichiers := Fichiers + [FichierTrouve];
  end;

  // Initialiser la barre de progression
  TThread.Synchronize(nil,
    procedure
    begin
      pbProgression.Min := 0;
      pbProgression.Max := Length(Fichiers);
      pbProgression.Position := 0;
    end);

  // Variables pour les statistiques
  var NbSucces := 0;
  var NbEchecs := 0;

  // Traiter chaque fichier
  for i := 0 to Length(Fichiers) - 1 do
  begin
    // Vérifier si le traitement a été interrompu
    if not FTraitementEnCours then
      Break;

    Fichier := Fichiers[i];

    // Mettre à jour le statut
    TThread.Synchronize(nil,
      procedure
      begin
        lblStatus.Caption := 'Traitement de ' + ExtractFileName(Fichier) +
                             ' (' + IntToStr(i+1) + '/' + IntToStr(Length(Fichiers)) + ')';
      end);

    try
      // Déterminer le nom du fichier de destination
      if FOptionsTraitement.ConvertirFormat then
        Extension := '.' + LowerCase(FOptionsTraitement.FormatCible)
      else
        Extension := ExtractFileExt(Fichier);

      FichierDestination := IncludeTrailingPathDelimiter(FOptionsTraitement.DossierDestination) +
                            FOptionsTraitement.Prefixe +
                            ChangeFileExt(ExtractFileName(Fichier), '') +
                            FOptionsTraitement.Suffixe +
                            Extension;

      // Traitement de l'image selon les options
      if FOptionsTraitement.Redimensionner then
      begin
        // Cette partie nécessite d'être dans le thread principal pour manipuler les images
        TThread.Synchronize(nil,
          procedure
          var
            Image: TImage;
            BitmapOriginal, BitmapRedim: TBitmap;
            Jpeg: TJPEGImage;
            Png: TPngImage;
            LargeurOrig, HauteurOrig, NouvelleHauteur, NouvelleLargeur: Integer;
          begin
            Image := TImage.Create(nil);
            try
              Image.Picture.LoadFromFile(Fichier);

              BitmapOriginal := TBitmap.Create;
              try
                BitmapOriginal.Assign(Image.Picture.Graphic);

                LargeurOrig := BitmapOriginal.Width;
                HauteurOrig := BitmapOriginal.Height;

                // Calculer les nouvelles dimensions en préservant le ratio
                if (LargeurOrig > FOptionsTraitement.LargeurMax) or
                   (HauteurOrig > FOptionsTraitement.HauteurMax) then
                begin
                  if LargeurOrig / HauteurOrig > FOptionsTraitement.LargeurMax / FOptionsTraitement.HauteurMax then
                  begin
                    NouvelleLargeur := FOptionsTraitement.LargeurMax;
                    NouvelleHauteur := Round(HauteurOrig * FOptionsTraitement.LargeurMax / LargeurOrig);
                  end
                  else
                  begin
                    NouvelleHauteur := FOptionsTraitement.HauteurMax;
                    NouvelleLargeur := Round(LargeurOrig * FOptionsTraitement.HauteurMax / HauteurOrig);
                  end;
                end
                else
                begin
                  NouvelleLargeur := LargeurOrig;
                  NouvelleHauteur := HauteurOrig;
                end;

                // Redimensionner l'image
                BitmapRedim := TBitmap.Create;
                try
                  BitmapRedim.SetSize(NouvelleLargeur, NouvelleHauteur);
                  BitmapRedim.Canvas.StretchDraw(Rect(0, 0, NouvelleLargeur, NouvelleHauteur),
                                                BitmapOriginal);

                  // Sauvegarder selon le format cible
                  if FOptionsTraitement.ConvertirFormat and
                     (LowerCase(FOptionsTraitement.FormatCible) = 'jpg') then
                  begin
                    Jpeg := TJPEGImage.Create;
                    try
                      Jpeg.Assign(BitmapRedim);
                      Jpeg.CompressionQuality := FOptionsTraitement.Qualite;
                      Jpeg.SaveToFile(FichierDestination);
                    finally
                      Jpeg.Free;
                    end;
                  end
                  else if FOptionsTraitement.ConvertirFormat and
                          (LowerCase(FOptionsTraitement.FormatCible) = 'png') then
                  begin
                    Png := TPngImage.Create;
                    try
                      Png.Assign(BitmapRedim);
                      Png.SaveToFile(FichierDestination);
                    finally
                      Png.Free;
                    end;
                  end
                  else
                  begin
                    BitmapRedim.SaveToFile(FichierDestination);
                  end;
                finally
                  BitmapRedim.Free;
                end;
              finally
                BitmapOriginal.Free;
              end;
            finally
              Image.Free;
            end;
          end);
      end
      else
      begin
        // Simplement copier le fichier sans redimensionnement
        TFile.Copy(Fichier, FichierDestination, True);
      end;

      // Journaliser le succès
      TThread.Synchronize(nil,
        procedure
        begin
          memoLog.Lines.Add('SUCCÈS - ' + ExtractFileName(Fichier) + ' -> ' +
                            ExtractFileName(FichierDestination));
        end);

      Inc(NbSucces);
    except
      on E: Exception do
      begin
        // Journaliser l'erreur
        TThread.Synchronize(nil,
          procedure
          begin
            memoLog.Lines.Add('ERREUR - ' + ExtractFileName(Fichier) + ': ' + E.Message);
          end);

        Inc(NbEchecs);
      end;
    end;

    // Mettre à jour la barre de progression
    TThread.Synchronize(nil,
      procedure
      begin
        pbProgression.Position := i + 1;
      end);
  end;

  // Ajouter le résumé au journal
  TThread.Synchronize(nil,
    procedure
    begin
      memoLog.Lines.Add('---------------------------------');
      memoLog.Lines.Add('Fin du traitement: ' + DateTimeToStr(Now));
      memoLog.Lines.Add('Fichiers traités: ' + IntToStr(Length(Fichiers)));
      memoLog.Lines.Add('Succès: ' + IntToStr(NbSucces));
      memoLog.Lines.Add('Échecs: ' + IntToStr(NbEchecs));

      if not FTraitementEnCours then
        lblStatus.Caption := 'Traitement interrompu'
      else
        lblStatus.Caption := 'Traitement terminé !';
    end);
end;

end.
```

Voici le fichier DFM de l'interface graphique :

```pascal
object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Traitement par lots d'#39'images'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 209
    Align = alTop
    TabOrder = 0
    object grpSource: TGroupBox
      Left = 8
      Top = 8
      Width = 377
      Height = 89
      Caption = 'Source'
      TabOrder = 0
      object lblDossierSource: TLabel
        Left = 8
        Top = 24
        Width = 47
        Height = 13
        Caption = 'Dossier :'
      end
      object lblFiltre: TLabel
        Left = 8
        Top = 56
        Width = 31
        Height = 13
        Caption = 'Filtre :'
      end
      object edtDossierSource: TEdit
        Left = 61
        Top = 21
        Width = 227
        Height = 21
        TabOrder = 0
      end
      object btnChoisirSource: TButton
        Left = 294
        Top = 19
        Width = 75
        Height = 25
        Caption = 'Parcourir...'
        TabOrder = 1
        OnClick = btnChoisirSourceClick
      end
      object edtFiltre: TEdit
        Left = 61
        Top = 53
        Width = 227
        Height = 21
        TabOrder = 2
      end
      object chkSousDossiers: TCheckBox
        Left = 294
        Top = 55
        Width = 73
        Height = 17
        Caption = 'Inclure tous'
        TabOrder = 3
      end
    end
    object grpDestination: TGroupBox
      Left = 391
      Top = 8
      Width = 377
      Height = 89
      Caption = 'Destination'
      TabOrder = 1
      object lblDossierDestination: TLabel
        Left = 8
        Top = 24
        Width = 47
        Height = 13
        Caption = 'Dossier :'
      end
      object edtDossierDestination: TEdit
        Left = 61
        Top = 21
        Width = 227
        Height = 21
        TabOrder = 0
      end
      object btnChoisirDestination: TButton
        Left = 294
        Top = 19
        Width = 75
        Height = 25
        Caption = 'Parcourir...'
        TabOrder = 1
        OnClick = btnChoisirDestinationClick
      end
    end
    object grpOptions: TGroupBox
      Left = 8
      Top = 103
      Width = 760
      Height = 98
      Caption = 'Options'
      TabOrder = 2
      object lblLargeur: TLabel
        Left = 124
        Top = 28
        Width = 48
        Height = 13
        Caption = 'Largeur :'
        Enabled = False
      end
      object lblHauteur: TLabel
        Left = 124
        Top = 60
        Width = 47
        Height = 13
        Caption = 'Hauteur :'
        Enabled = False
      end
      object lblPrefixe: TLabel
        Left = 280
        Top = 28
        Width = 43
        Height = 13
        Caption = 'Pr'#233'fixe :'
      end
      object lblSuffixe: TLabel
        Left = 280
        Top = 60
        Width = 42
        Height = 13
        Caption = 'Suffixe :'
      end
      object lblQualite: TLabel
        Left = 584
        Top = 28
        Width = 42
        Height = 13
        Caption = 'Qualit'#233' :'
        Enabled = False
      end
      object chkRedimensionner: TCheckBox
        Left = 8
        Top = 28
        Width = 110
        Height = 17
        Caption = 'Redimensionner'
        TabOrder = 0
        OnClick = chkRedimensionnerClick
      end
      object edtLargeur: TEdit
        Left = 178
        Top = 25
        Width = 73
        Height = 21
        Enabled = False
        TabOrder = 1
        Text = '800'
      end
      object edtHauteur: TEdit
        Left = 178
        Top = 57
        Width = 73
        Height = 21
        Enabled = False
        TabOrder = 2
        Text = '600'
      end
      object edtPrefixe: TEdit
        Left = 329
        Top = 25
        Width = 121
        Height = 21
        TabOrder = 3
      end
      object edtSuffixe: TEdit
        Left = 329
        Top = 57
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '_resized'
      end
      object chkConvertir: TCheckBox
        Left = 472
        Top = 28
        Width = 106
        Height = 17
        Caption = 'Convertir en :'
        TabOrder = 5
        OnClick = chkConvertirClick
      end
      object cboFormat: TComboBox
        Left = 472
        Top = 57
        Width = 89
        Height = 21
        Enabled = False
        TabOrder = 6
        Text = 'JPG'
        Items.Strings = (
          'JPG'
          'PNG'
          'BMP')
      end
      object trkQualite: TTrackBar
        Left = 584
        Top = 47
        Width = 153
        Height = 45
        Enabled = False
        Max = 100
        Min = 1
        Position = 80
        TabOrder = 7
      end
    end
  end
  object pnlTraitement: TPanel
    Left = 0
    Top = 209
    Width = 784
    Height = 48
    Align = alTop
    TabOrder = 1
    object lblStatus: TLabel
      Left = 8
      Top = 17
      Width = 28
      Height = 13
      Caption = 'Pr'#234't'
    end
    object btnDemarrer: TButton
      Left = 584
      Top = 12
      Width = 89
      Height = 25
      Caption = 'D'#233'marrer'
      TabOrder = 0
      OnClick = btnDemarrerClick
    end
    object btnArreter: TButton
      Left = 679
      Top = 12
      Width = 89
      Height = 25
      Caption = 'Arr'#234'ter'
      Enabled = False
      TabOrder = 1
      OnClick = btnArreterClick
    end
    object pbProgression: TProgressBar
      Left = 144
      Top = 14
      Width = 425
      Height = 21
      TabOrder = 2
    end
  end
  object pnlJournal: TPanel
    Left = 0
    Top = 257
    Width = 784
    Height = 304
    Align = alClient
    Caption = 'pnlJournal'
    TabOrder = 2
    object memoLog: TMemo
      Left = 1
      Top = 1
      Width = 782
      Height = 302
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
```

### Traitement par lots de fichiers texte

Le traitement par lots n'est pas limité aux images. Voici un exemple qui permet de traiter des fichiers texte en lot :

```pascal
procedure TraiterFichiersTexte(const DossierSource, DossierDestination, Filtre: string;
                              const Remplacements: array of TSouceDestination);
var
  Fichiers: TStringDynArray;
  Fichier, FichierDestination: string;
  Contenu: TStringList;
  i, j: Integer;
  NbRemplacements: Integer;
begin
  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(DossierDestination) then
    ForceDirectories(DossierDestination);

  // Obtenir la liste des fichiers
  Fichiers := TDirectory.GetFiles(DossierSource, Filtre);

  // Créer un StringList pour manipuler le contenu
  Contenu := TStringList.Create;
  try
    // Traiter chaque fichier
    for i := 0 to Length(Fichiers) - 1 do
    begin
      Fichier := Fichiers[i];
      FichierDestination := IncludeTrailingPathDelimiter(DossierDestination) +
                            ExtractFileName(Fichier);

      try
        // Charger le contenu du fichier
        Contenu.LoadFromFile(Fichier);

        // Effectuer les remplacements
        NbRemplacements := 0;
        for j := 0 to Length(Remplacements) - 1 do
        begin
          // Remplacer toutes les occurrences
          var TexteOriginal := Contenu.Text;
          Contenu.Text := StringReplace(Contenu.Text,
                                        Remplacements[j].Source,
                                        Remplacements[j].Destination,
                                        [rfReplaceAll]);

          // Compter le nombre de remplacements
          if TexteOriginal <> Contenu.Text then
            Inc(NbRemplacements);
        end;

        // Sauvegarder le fichier modifié
        Contenu.SaveToFile(FichierDestination);

        // Journaliser le résultat
        WriteLn('Fichier traité : ' + ExtractFileName(Fichier) +
                ' (' + IntToStr(NbRemplacements) + ' remplacements)');
      except
        on E: Exception do
          WriteLn('Erreur lors du traitement de ' + ExtractFileName(Fichier) +
                  ': ' + E.Message);
      end;
    end;
  finally
    Contenu.Free;
  end;
end;

// Type pour les remplacements
type
  TSouceDestination = record
    Source: string;
    Destination: string;
  end;

// Utilisation
var
  Remplacements: array of TSouceDestination;
begin
  SetLength(Remplacements, 3);
  Remplacements[0].Source := 'ancien_texte1';
  Remplacements[0].Destination := 'nouveau_texte1';
  Remplacements[1].Source := 'ancien_texte2';
  Remplacements[1].Destination := 'nouveau_texte2';
  Remplacements[2].Source := 'ancien_texte3';
  Remplacements[2].Destination := 'nouveau_texte3';

  TraiterFichiersTexte('C:\Source', 'C:\Destination', '*.txt', Remplacements);
end;
```

### Traitement par lots et base de données

Voici un exemple de traitement par lots qui importe des données d'un ensemble de fichiers CSV dans une base de données :

```pascal
procedure ImporterCSVersBDD(const DossierSource: string;
                           Connection: TFDConnection;
                           TableCible: string;
                           const MappingColonnes: array of string);
var
  Fichiers: TStringDynArray;
  Fichier: string;
  CSV: TStringList;
  Query: TFDQuery;
  i, j: Integer;
  Ligne: TArray<string>;
  SQLInsert: string;
  ValeurSQL: string;
begin
  // Obtenir la liste des fichiers CSV
  Fichiers := TDirectory.GetFiles(DossierSource, '*.csv');

  // Créer l'objet pour lire les CSV
  CSV := TStringList.Create;
  try
    // Créer la requête
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := Connection;

      // Traiter chaque fichier
      for i := 0 to Length(Fichiers) - 1 do
      begin
        Fichier := Fichiers[i];

        try
          // Charger le fichier CSV
          CSV.LoadFromFile(Fichier, TEncoding.UTF8);

          // Ignorer la première ligne si c'est un en-tête
          var PremiereLigne := 1;  // 0 si on veut traiter l'en-tête

          // Traiter chaque ligne du CSV
          for j := PremiereLigne to CSV.Count - 1 do
          begin
            // Séparer les valeurs
            Ligne := CSV[j].Split([',', ';']);

            // Vérifier que le nombre de colonnes correspond
            if Length(Ligne) >= Length(MappingColonnes) then
            begin
              // Construire la requête INSERT
              SQLInsert := 'INSERT INTO ' + TableCible + ' (';

              // Ajouter les noms de colonnes
              for var k := 0 to Length(MappingColonnes) - 1 do
              begin
                SQLInsert := SQLInsert + MappingColonnes[k];
                if k < Length(MappingColonnes) - 1 then
                  SQLInsert := SQLInsert + ', ';
              end;

              SQLInsert := SQLInsert + ') VALUES (';

              // Ajouter les valeurs
              for var k := 0 to Length(MappingColonnes) - 1 do
              begin
                // Préparer la valeur avec les apostrophes si nécessaire
                ValeurSQL := QuotedStr(Trim(Ligne[k]));

                SQLInsert := SQLInsert + ValeurSQL;
                if k < Length(MappingColonnes) - 1 then
                  SQLInsert := SQLInsert + ', ';
              end;

              SQLInsert := SQLInsert + ')';

              // Exécuter la requête
              Query.SQL.Text := SQLInsert;
              Query.ExecSQL;
            end;
          end;

          WriteLn('Fichier importé : ' + ExtractFileName(Fichier) +
                  ' (' + IntToStr(CSV.Count - PremiereLigne) + ' lignes)');
        except
          on E: Exception do
            WriteLn('Erreur lors de l''importation de ' + ExtractFileName(Fichier) +
                    ': ' + E.Message);
        end;
      end;
    finally
      Query.Free;
    end;
  finally
    CSV.Free;
  end;
end;

// Utilisation
procedure TForm1.ButtonImporterClick(Sender: TObject);
var
  Mapping: array of string;
begin
  // Configurer le mapping des colonnes
  SetLength(Mapping, 3);
  Mapping[0] := 'NOM';
  Mapping[1] := 'PRENOM';
  Mapping[2] := 'EMAIL';

  // Lancer l'importation
  ImporterCSVersBDD('C:\Data\CSV', FDConnection1, 'CLIENTS', Mapping);
end;
```

### Planification de traitements par lots

Pour automatiser complètement vos traitements par lots, vous pouvez créer des applications qui s'exécutent selon un planning :

```pascal
procedure ExecuterTraitementPlanifie;
var
  HeureCourante: TDateTime;
  HeureExecution: TDateTime;
begin
  // Configurer l'heure d'exécution (par exemple, 3h du matin)
  HeureExecution := EncodeTime(3, 0, 0, 0);

  // Boucle infinie (à utiliser dans un service Windows ou une application qui tourne en continu)
  while True do
  begin
    // Obtenir l'heure courante
    HeureCourante := Time;

    // Vérifier si c'est l'heure de l'exécution
    if (HeureCourante >= HeureExecution) and
       (HeureCourante < IncMinute(HeureExecution, 1)) then
    begin
      try
        // Exécuter le traitement par lots
        ExecuterTraitementQuotidien;

        // Attendre jusqu'au lendemain
        Sleep(24 * 60 * 60 * 1000);  // 24 heures en millisecondes
      except
        on E: Exception do
          LogError('Erreur lors de l''exécution planifiée : ' + E.Message);
      end;
    end;

    // Pause pour économiser les ressources
    Sleep(30000);  // 30 secondes
  end;
end;
```

> **Note :** Pour une planification plus robuste, il est recommandé d'utiliser le Planificateur de tâches de Windows ou des bibliothèques tierces spécialisées.

### Traitement par lots et ligne de commande

Vous pouvez créer des applications en ligne de commande pour automatiser les traitements par lots :

```pascal
program BatchProcessor;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils;

procedure AfficherAide;
begin
  WriteLn('Utilisation: BatchProcessor.exe [options]');
  WriteLn('Options:');
  WriteLn('  -s, --source      Dossier source (obligatoire)');
  WriteLn('  -d, --destination Dossier destination (obligatoire)');
  WriteLn('  -f, --filter      Filtre de fichiers (défaut: *.*)');
  WriteLn('  -r, --recursive   Inclure les sous-dossiers');
  WriteLn('  -h, --help        Afficher cette aide');
  WriteLn('');
  WriteLn('Exemple:');
  WriteLn('  BatchProcessor.exe -s C:\Source -d C:\Destination -f *.txt -r');
end;

procedure TraiterFichiers(const DossierSource, DossierDestination, Filtre: string;
                         Recursif: Boolean);
var
  Fichiers: TStringDynArray;
  Fichier, FichierDestination, CheminRelatif: string;
  Option: TSearchOption;
begin
  // Déterminer l'option de recherche
  if Recursif then
    Option := TSearchOption.soAllDirectories
  else
    Option := TSearchOption.soTopDirectoryOnly;

  // Obtenir la liste des fichiers
  Fichiers := TDirectory.GetFiles(DossierSource, Filtre, Option);

  WriteLn('Traitement de ' + IntToStr(Length(Fichiers)) + ' fichiers...');

  // Traiter chaque fichier
  for Fichier in Fichiers do
  begin
    try
      // Calculer le chemin relatif
      CheminRelatif := ExtractRelativePath(IncludeTrailingPathDelimiter(DossierSource), Fichier);

      // Déterminer le chemin de destination
      FichierDestination := IncludeTrailingPathDelimiter(DossierDestination) + CheminRelatif;

      // Créer le dossier de destination si nécessaire
      ForceDirectories(ExtractFilePath(FichierDestination));

      // Effectuer le traitement (ici, simple copie comme exemple)
      TFile.Copy(Fichier, FichierDestination, True);

      WriteLn('Traité: ' + CheminRelatif);
    except
      on E: Exception do
        WriteLn('Erreur: ' + E.Message + ' (' + ExtractFileName(Fichier) + ')');
    end;
  end;

  WriteLn('Traitement terminé');
end;

var
  i: Integer;
  DossierSource, DossierDestination, Filtre: string;
  Recursif: Boolean;
  Param, ParamSuivant: string;
begin
  try
    // Valeurs par défaut
    DossierSource := '';
    DossierDestination := '';
    Filtre := '*.*';
    Recursif := False;

    // Analyser les paramètres de ligne de commande
    i := 1;
    while i <= ParamCount do
    begin
      Param := ParamStr(i);

      // Paramètre suivant (s'il existe)
      if i < ParamCount then
        ParamSuivant := ParamStr(i + 1)
      else
        ParamSuivant := '';

      // Traiter les options
      if (Param = '-s') or (Param = '--source') then
      begin
        if ParamSuivant <> '' then
        begin
          DossierSource := ParamSuivant;
          Inc(i);  // Sauter le paramètre suivant
        end;
      end
      else if (Param = '-d') or (Param = '--destination') then
      begin
        if ParamSuivant <> '' then
        begin
          DossierDestination := ParamSuivant;
          Inc(i);  // Sauter le paramètre suivant
        end;
      end
      else if (Param = '-f') or (Param = '--filter') then
      begin
        if ParamSuivant <> '' then
        begin
          Filtre := ParamSuivant;
          Inc(i);  // Sauter le paramètre suivant
        end;
      end
      else if (Param = '-r') or (Param = '--recursive') then
      begin
        Recursif := True;
      end
      else if (Param = '-h') or (Param = '--help') then
      begin
        AfficherAide;
        Exit;
      end;

      Inc(i);  // Passer au paramètre suivant
    end;

    // Vérifier les paramètres obligatoires
    if (DossierSource = '') or (DossierDestination = '') then
    begin
      WriteLn('Erreur: Les dossiers source et destination sont obligatoires.');
      WriteLn('');
      AfficherAide;
      Exit;
    end;

    // Vérifier que le dossier source existe
    if not DirectoryExists(DossierSource) then
    begin
      WriteLn('Erreur: Le dossier source n''existe pas: ' + DossierSource);
      Exit;
    end;

    // Lancer le traitement
    TraiterFichiers(DossierSource, DossierDestination, Filtre, Recursif);

  except
    on E: Exception do
      WriteLn('Erreur: ' + E.Message);
  end;
end.
```

### Journalisation avancée pour le traitement par lots

Une bonne journalisation est essentielle pour les traitements par lots, surtout ceux qui s'exécutent sans supervision. Voici une classe de journalisation avancée :

```pascal
unit LoggerUnit;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogger = class
  private
    FLogFile: string;
    FMinLevel: TLogLevel;
    FConsoleOutput: Boolean;
    FLogFileStream: TStreamWriter;

    function LogLevelToString(Level: TLogLevel): string;
  public
    constructor Create(const LogFile: string; MinLevel: TLogLevel = llInfo;
                      ConsoleOutput: Boolean = True);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Message: string); overload;
    procedure Log(Level: TLogLevel; const Format: string; const Args: array of const); overload;

    procedure Debug(const Message: string); overload;
    procedure Debug(const Format: string; const Args: array of const); overload;

    procedure Info(const Message: string); overload;
    procedure Info(const Format: string; const Args: array of const); overload;

    procedure Warning(const Message: string); overload;
    procedure Warning(const Format: string; const Args: array of const); overload;

    procedure Error(const Message: string); overload;
    procedure Error(const Format: string; const Args: array of const); overload;

    procedure Critical(const Message: string); overload;
    procedure Critical(const Format: string; const Args: array of const); overload;

    property LogFile: string read FLogFile;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property ConsoleOutput: Boolean read FConsoleOutput write FConsoleOutput;
  end;

implementation

constructor TLogger.Create(const LogFile: string; MinLevel: TLogLevel;
                          ConsoleOutput: Boolean);
begin
  inherited Create;
  FLogFile := LogFile;
  FMinLevel := MinLevel;
  FConsoleOutput := ConsoleOutput;

  // Créer le dossier du journal si nécessaire
  ForceDirectories(ExtractFilePath(FLogFile));

  // Ouvrir le fichier journal en mode ajout
  FLogFileStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8);

  // Écrire un en-tête au démarrage
  FLogFileStream.WriteLine('');
  FLogFileStream.WriteLine('=== Démarrage du journal: ' +
                          FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ===');
  FLogFileStream.Flush;
end;

destructor TLogger.Destroy;
begin
  if Assigned(FLogFileStream) then
  begin
    // Écrire un pied de page à la fermeture
    FLogFileStream.WriteLine('=== Fermeture du journal: ' +
                            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ===');
    FLogFileStream.Flush;
    FLogFileStream.Free;
  end;

  inherited;
end;

function TLogger.LogLevelToString(Level: TLogLevel): string;
begin
  case Level of
    llDebug:    Result := 'DEBUG';
    llInfo:     Result := 'INFO';
    llWarning:  Result := 'WARNING';
    llError:    Result := 'ERROR';
    llCritical: Result := 'CRITICAL';
    else        Result := 'UNKNOWN';
  end;
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string);
var
  LogEntry: string;
begin
  // Vérifier si le niveau de log est suffisant
  if Level < FMinLevel then
    Exit;

  // Formater l'entrée de journal
  LogEntry := Format('%s [%s] %s',
                     [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
                      LogLevelToString(Level),
                      Message]);

  // Écrire dans le fichier journal
  FLogFileStream.WriteLine(LogEntry);
  FLogFileStream.Flush;  // Garantit que l'entrée est immédiatement écrite

  // Afficher dans la console si demandé
  if FConsoleOutput then
  begin
    // Colorier selon le niveau (uniquement pour les applications console)
    case Level of
      llDebug:    TextColor := LightGray;
      llInfo:     TextColor := White;
      llWarning:  TextColor := Yellow;
      llError:    TextColor := LightRed;
      llCritical: TextColor := Red;
    end;

    WriteLn(LogEntry);
    TextColor := White;  // Restaurer la couleur par défaut
  end;
end;

procedure TLogger.Log(Level: TLogLevel; const Format: string; const Args: array of const);
begin
  Log(Level, SysUtils.Format(Format, Args));
end;

procedure TLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

procedure TLogger.Debug(const Format: string; const Args: array of const);
begin
  Log(llDebug, Format, Args);
end;

procedure TLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TLogger.Info(const Format: string; const Args: array of const);
begin
  Log(llInfo, Format, Args);
end;

procedure TLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TLogger.Warning(const Format: string; const Args: array of const);
begin
  Log(llWarning, Format, Args);
end;

procedure TLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TLogger.Error(const Format: string; const Args: array of const);
begin
  Log(llError, Format, Args);
end;

procedure TLogger.Critical(const Message: string);
begin
  Log(llCritical, Message);
end;

procedure TLogger.Critical(const Format: string; const Args: array of const);
begin
  Log(llCritical, Format, Args);
end;

end.
```

Utilisation de cette classe de journalisation dans un traitement par lots :

```pascal
uses
  LoggerUnit;

procedure TraiterLotsAvecJournal(const Options: TOptionsTraitement);
var
  Fichiers: TStringDynArray;
  Fichier: string;
  i: Integer;
  Logger: TLogger;
begin
  // Créer le journal
  Logger := TLogger.Create('C:\Logs\traitement_' +
                          FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');
  try
    Logger.Info('Démarrage du traitement par lots');
    Logger.Info('Dossier source: %s', [Options.DossierSource]);
    Logger.Info('Dossier destination: %s', [Options.DossierDestination]);

    // Obtenir la liste des fichiers
    try
      Fichiers := TDirectory.GetFiles(Options.DossierSource, Options.FiltreFichiers);
      Logger.Info('Nombre de fichiers à traiter: %d', [Length(Fichiers)]);
    except
      on E: Exception do
      begin
        Logger.Critical('Erreur lors de la recherche des fichiers: %s', [E.Message]);
        Exit;
      end;
    end;

    // Traiter chaque fichier
    for i := 0 to Length(Fichiers) - 1 do
    begin
      Fichier := Fichiers[i];
      Logger.Debug('Traitement du fichier: %s', [ExtractFileName(Fichier)]);

      try
        // Effectuer le traitement...

        Logger.Info('Fichier traité avec succès: %s', [ExtractFileName(Fichier)]);
      except
        on E: Exception do
          Logger.Error('Erreur lors du traitement de %s: %s',
                       [ExtractFileName(Fichier), E.Message]);
      end;
    end;

    Logger.Info('Traitement par lots terminé');
  finally
    Logger.Free;
  end;
end;
```

### Gestion des erreurs dans les traitements par lots

Pour les traitements par lots robustes, il est important de bien gérer les erreurs :

```pascal
procedure TraiterLotsAvecGestionErreurs(const Options: TOptionsTraitement);
var
  Fichiers: TStringDynArray;
  Fichier: string;
  i: Integer;
  Logger: TLogger;
  NbSucces, NbEchecs: Integer;
  FichiersEnEchec: TStringList;
  PeutContinuer: Boolean;
begin
  Logger := TLogger.Create('C:\Logs\traitement.log');
  FichiersEnEchec := TStringList.Create;
  try
    NbSucces := 0;
    NbEchecs := 0;

    Logger.Info('Démarrage du traitement');

    // Obtenir la liste des fichiers
    try
      Fichiers := TDirectory.GetFiles(Options.DossierSource, Options.FiltreFichiers);
    except
      on E: Exception do
      begin
        Logger.Critical('Erreur fatale: %s', [E.Message]);
        Exit;
      end;
    end;

    // Traiter chaque fichier
    for i := 0 to Length(Fichiers) - 1 do
    begin
      Fichier := Fichiers[i];

      try
        // Vérifier l'espace disque disponible
        if not VerifierEspaceDisque(Options.DossierDestination, 100*1024*1024) then
        begin
          Logger.Critical('Espace disque insuffisant sur le disque de destination');
          Break;
        end;

        // Décider si on peut traiter ce fichier
        PeutContinuer := True;

        // Vérifier si le fichier est accessible en lecture
        if not FileIsReadOnly(Fichier) then
        begin
          // Effectuer le traitement proprement dit...

          Inc(NbSucces);
          Logger.Info('Fichier traité: %s', [ExtractFileName(Fichier)]);
        end
        else
        begin
          Logger.Warning('Fichier en lecture seule ignoré: %s', [ExtractFileName(Fichier)]);
          FichiersEnEchec.Add(Fichier + ';READONLY');
          Inc(NbEchecs);
        end;
      except
        on E: EOutOfMemory do
        begin
          // Erreur critique qui nécessite d'arrêter le traitement
          Logger.Critical('Mémoire insuffisante: %s', [E.Message]);
          Break;
        end;
        on E: Exception do
        begin
          // Erreur non critique, on continue avec le fichier suivant
          Logger.Error('Erreur: %s (%s)', [E.Message, ExtractFileName(Fichier)]);
          FichiersEnEchec.Add(Fichier + ';' + E.Message);
          Inc(NbEchecs);
        end;
      end;

      // Vérifier si on doit continuer en fonction du nombre d'erreurs
      if (NbEchecs > 10) and (NbEchecs > NbSucces) then
      begin
        Logger.Critical('Trop d''erreurs, arrêt du traitement');
        Break;
      end;
    end;

    // Enregistrer la liste des fichiers en échec
    if FichiersEnEchec.Count > 0 then
      FichiersEnEchec.SaveToFile('C:\Logs\fichiers_en_echec.csv');

    Logger.Info('Traitement terminé: %d succès, %d échecs', [NbSucces, NbEchecs]);
  finally
    FichiersEnEchec.Free;
    Logger.Free;
  end;
end;

// Fonction utilitaire pour vérifier l'espace disque
function VerifierEspaceDisque(const Dossier: string; EspaceMinimum: Int64): Boolean;
var
  DisqueDispo: Int64;
  Disque: string;
begin
  // Obtenir la lettre du disque
  Disque := ExtractFileDrive(Dossier);

  // Obtenir l'espace disponible
  if not GetDiskFreeSpaceEx(PChar(Disque), DisqueDispo, nil, nil) then
    Exit(False);

  // Vérifier si l'espace est suffisant
  Result := DisqueDispo >= EspaceMinimum;
end;
```

### Bonnes pratiques pour le traitement par lots

1. **Journalisation complète**
   - Journalisez le début et la fin de chaque traitement
   - Incluez des informations détaillées sur les erreurs
   - Utilisez différents niveaux de journalisation (debug, info, warning, error)

2. **Gestion robuste des erreurs**
   - Distinguez les erreurs critiques des erreurs non critiques
   - Mettez en place des mécanismes de reprise après erreur
   - Stockez les informations sur les fichiers non traités pour une reprise ultérieure

3. **Surveillance des ressources**
   - Vérifiez l'espace disque disponible
   - Surveillez l'utilisation de la mémoire
   - Limitez le nombre de threads en fonction des ressources disponibles

4. **Notification et reporting**
   - Envoyez des emails ou des notifications pour les erreurs critiques
   - Générez des rapports de synthèse après chaque traitement
   - Mettez en place des tableaux de bord pour suivre les tendances

5. **Modularité et maintenabilité**
   - Divisez le traitement en modules réutilisables
   - Utilisez la configuration pour éviter de coder en dur les paramètres
   - Documentez clairement le fonctionnement du traitement

6. **Performance**
   - Utilisez le multithreading pour les traitements intensifs
   - Minimisez les opérations d'entrée/sortie
   - Traitez les fichiers par lots pour réduire les frais généraux

### Exercice pratique

Créez une application de traitement par lots qui :

1. Permet de configurer les dossiers source et destination
2. Offre plusieurs options de traitement (renommage, redimensionnement, conversion)
3. Affiche une progression détaillée pendant le traitement
4. Génère un rapport complet à la fin du traitement
5. Gère correctement les erreurs et permet une reprise après interruption

Cet exercice vous permettra de mettre en pratique les différentes techniques présentées dans cette section tout en créant une application utile pour vos besoins quotidiens.

---

À suivre dans la prochaine section : **7.7 Utilisation de formats modernes (JSON, XML, YAML)**
