# 7. Gestion des fichiers et flux de données

## 7.5 Compression et décompression

La compression de données est une technique qui permet de réduire la taille des fichiers ou des flux de données. Cette section vous guidera à travers les différentes méthodes disponibles en Delphi pour compresser et décompresser des données.

### Introduction à la compression

La compression permet de :
- Réduire l'espace disque utilisé
- Accélérer les transferts réseau
- Économiser la bande passante
- Archiver plus efficacement les données

Il existe deux types principaux de compression :
- **Compression sans perte** : aucune information n'est perdue (utilisée pour les documents, programmes, etc.)
- **Compression avec perte** : certaines informations sont supprimées (utilisée pour les images, sons, vidéos)

Delphi propose plusieurs solutions pour la compression sans perte, qui est celle que nous allons explorer dans cette section.

### Compression avec System.Zip

Depuis les versions récentes de Delphi, l'unité `System.Zip` offre des fonctionnalités natives pour la compression et décompression au format ZIP. C'est la solution la plus simple et la plus directe.

#### Compression d'un fichier avec TZipFile

```pascal
uses
  System.Zip, System.SysUtils, System.IOUtils;

procedure CompresserFichier(const FichierSource, FichierDestination: string);
var
  Zip: TZipFile;
begin
  // Vérifier que le fichier source existe
  if not FileExists(FichierSource) then
  begin
    ShowMessage('Le fichier source n''existe pas !');
    Exit;
  end;

  // Créer l'archive ZIP
  Zip := TZipFile.Create;
  try
    // Créer un nouveau fichier ZIP
    Zip.Open(FichierDestination, zmWrite);

    // Ajouter le fichier à l'archive
    // Paramètres : FichierSource = chemin du fichier à ajouter
    //              '' = répertoire dans l'archive (racine ici)
    //              0 = niveau de compression (0 = défaut)
    Zip.Add(FichierSource, '');

    // Fermer l'archive
    Zip.Close;

    ShowMessage('Compression terminée avec succès !');
  finally
    Zip.Free;
  end;
end;
```

#### Compression de plusieurs fichiers

```pascal
procedure CompresserPlusieurs(const FichiersSource: TArray<string>;
                             const FichierDestination: string);
var
  Zip: TZipFile;
  Fichier: string;
begin
  Zip := TZipFile.Create;
  try
    Zip.Open(FichierDestination, zmWrite);

    // Ajouter chaque fichier à l'archive
    for Fichier in FichiersSource do
    begin
      if FileExists(Fichier) then
        Zip.Add(Fichier, '');
    end;

    Zip.Close;
  finally
    Zip.Free;
  end;
end;

// Utilisation :
procedure TForm1.ButtonCompresserClick(Sender: TObject);
var
  Fichiers: TArray<string>;
begin
  if OpenDialog1.Execute then
  begin
    Fichiers := OpenDialog1.Files.ToStringArray;

    if SaveDialog1.Execute then
      CompresserPlusieurs(Fichiers, SaveDialog1.FileName);
  end;
end;
```

#### Compression avec structure de dossiers

Pour conserver la structure de dossiers dans une archive ZIP :

```pascal
procedure CompresserDossier(const DossierSource, FichierDestination: string);
var
  Zip: TZipFile;
  Fichiers: TArray<string>;
  Fichier: string;
  CheminRelatif: string;
begin
  // Obtenir tous les fichiers du dossier et ses sous-dossiers
  Fichiers := TDirectory.GetFiles(DossierSource, '*', TSearchOption.soAllDirectories);

  Zip := TZipFile.Create;
  try
    Zip.Open(FichierDestination, zmWrite);

    // Ajouter chaque fichier en préservant les chemins relatifs
    for Fichier in Fichiers do
    begin
      // Calculer le chemin relatif du fichier par rapport au dossier source
      CheminRelatif := StringReplace(
        ExtractRelativePath(IncludeTrailingPathDelimiter(DossierSource), Fichier),
        '\', '/', [rfReplaceAll]);

      // Ajouter le fichier avec son chemin relatif dans l'archive
      Zip.Add(Fichier, CheminRelatif);
    end;

    Zip.Close;
  finally
    Zip.Free;
  end;
end;
```

#### Extraction d'un fichier ZIP

```pascal
procedure ExtraireFichierZip(const FichierZip, DossierDestination: string);
var
  Zip: TZipFile;
begin
  // Vérifier que le fichier ZIP existe
  if not FileExists(FichierZip) then
  begin
    ShowMessage('Le fichier ZIP n''existe pas !');
    Exit;
  end;

  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(DossierDestination) then
    ForceDirectories(DossierDestination);

  Zip := TZipFile.Create;
  try
    Zip.Open(FichierZip, zmRead);

    // Extraire tous les fichiers vers le dossier de destination
    Zip.ExtractAll(DossierDestination);

    Zip.Close;

    ShowMessage('Extraction terminée avec succès !');
  finally
    Zip.Free;
  end;
end;
```

#### Extraction sélective

```pascal
procedure ExtraireFichierSpecifique(const FichierZip, NomFichierDansZip, FichierDestination: string);
var
  Zip: TZipFile;
  Index: Integer;
begin
  Zip := TZipFile.Create;
  try
    Zip.Open(FichierZip, zmRead);

    // Chercher l'index du fichier dans l'archive
    Index := Zip.IndexOf(NomFichierDansZip);

    if Index >= 0 then
    begin
      // Extraire le fichier spécifique
      Zip.Extract(Index, FichierDestination);
      ShowMessage('Fichier extrait avec succès !');
    end
    else
      ShowMessage('Fichier non trouvé dans l''archive !');

    Zip.Close;
  finally
    Zip.Free;
  end;
end;
```

#### Lister le contenu d'un ZIP

```pascal
procedure ListerContenuZip(const FichierZip: string; Memo: TMemo);
var
  Zip: TZipFile;
  Header: TZipHeader;
  i: Integer;
begin
  Memo.Clear;

  Zip := TZipFile.Create;
  try
    Zip.Open(FichierZip, zmRead);

    Memo.Lines.Add('Contenu de l''archive :');
    Memo.Lines.Add('-------------------');

    // Parcourir tous les fichiers de l'archive
    for i := 0 to Zip.FileCount - 1 do
    begin
      Zip.Read(i, Header);

      Memo.Lines.Add(Format(
        '%s - Taille: %d octets, Taille compressée: %d octets (%.1f%%)',
        [Header.FileName,
         Header.UncompressedSize,
         Header.CompressedSize,
         (1 - Header.CompressedSize / Header.UncompressedSize) * 100]));
    end;

    Zip.Close;
  finally
    Zip.Free;
  end;
end;
```

#### Niveaux de compression

TZipFile permet de spécifier le niveau de compression lors de l'ajout de fichiers :

```pascal
// Paramètres : Fichier à ajouter, chemin dans l'archive, niveau de compression
Zip.Add(FichierSource, '', TZipCompression.zcMaximum);  // Compression maximale
```

Les niveaux disponibles sont :
- `zcNone` - Pas de compression (stockage simple)
- `zcFastest` - Compression rapide mais moins efficace
- `zcDefault` - Compromis entre vitesse et taux de compression
- `zcMaximum` - Compression optimale mais plus lente
- `zcLevel1` à `zcLevel9` - Niveaux intermédiaires (1=rapide, 9=optimal)

### Compression avec ZLib

Pour une compression plus flexible ou pour utiliser l'algorithme DEFLATE directement, Delphi fournit l'accès à la bibliothèque ZLib via les classes `TZCompressionStream` et `TZDecompressionStream`.

#### Compression d'un flux mémoire

```pascal
uses
  System.Classes, System.SysUtils, System.ZLib;

procedure CompresserMemoire(const SourceBytes: TBytes; out CompressedBytes: TBytes);
var
  SourceStream: TBytesStream;
  CompressedStream: TBytesStream;
  CompressionStream: TZCompressionStream;
begin
  SourceStream := TBytesStream.Create(SourceBytes);
  try
    CompressedStream := TBytesStream.Create;
    try
      // Créer un flux de compression avec compression maximale
      CompressionStream := TZCompressionStream.Create(
        CompressedStream, TZCompressionLevel.zcMax);
      try
        // Copier les données source dans le flux de compression
        CompressionStream.CopyFrom(SourceStream, 0);
      finally
        // Important : fermer le flux de compression avant d'utiliser les résultats
        CompressionStream.Free;
      end;

      // Récupérer les données compressées
      CompressedBytes := CompressedStream.Bytes;
    finally
      CompressedStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DecompresserMemoire(const CompressedBytes: TBytes; out SourceBytes: TBytes);
var
  CompressedStream: TBytesStream;
  SourceStream: TBytesStream;
  DecompressionStream: TZDecompressionStream;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
begin
  CompressedStream := TBytesStream.Create(CompressedBytes);
  try
    SourceStream := TBytesStream.Create;
    try
      // Créer un flux de décompression
      DecompressionStream := TZDecompressionStream.Create(CompressedStream);
      try
        // Lire les données décompressées par blocs
        repeat
          BytesRead := DecompressionStream.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            SourceStream.WriteBuffer(Buffer, BytesRead);
        until BytesRead = 0;
      finally
        DecompressionStream.Free;
      end;

      // Récupérer les données décompressées
      SourceBytes := SourceStream.Bytes;
    finally
      SourceStream.Free;
    end;
  finally
    CompressedStream.Free;
  end;
end;
```

#### Compression de fichiers avec ZLib

```pascal
procedure CompresserFichierZLib(const FichierSource, FichierDestination: string);
var
  SourceStream: TFileStream;
  DestStream: TFileStream;
  CompressionStream: TZCompressionStream;
begin
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de compression
      CompressionStream := TZCompressionStream.Create(
        DestStream, TZCompressionLevel.zcMax);
      try
        // Copier le fichier source dans le flux de compression
        CompressionStream.CopyFrom(SourceStream, 0);
      finally
        // Important : fermer le flux de compression avant le flux de destination
        CompressionStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DecompresserFichierZLib(const FichierCompresse, FichierDestination: string);
var
  SourceStream: TFileStream;
  DestStream: TFileStream;
  DecompressionStream: TZDecompressionStream;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
begin
  SourceStream := TFileStream.Create(FichierCompresse, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de décompression
      DecompressionStream := TZDecompressionStream.Create(SourceStream);
      try
        // Lire les données décompressées par blocs
        repeat
          BytesRead := DecompressionStream.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            DestStream.WriteBuffer(Buffer, BytesRead);
        until BytesRead = 0;
      finally
        DecompressionStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### Compression de chaînes de caractères

Pour compresser des chaînes de caractères rapidement, vous pouvez utiliser les fonctions utilitaires de ZLib :

```pascal
uses
  System.ZLib, System.SysUtils;

function CompresserChaine(const Source: string): TBytes;
var
  SourceBytes: TBytes;
begin
  // Convertir la chaîne en tableau d'octets
  SourceBytes := TEncoding.UTF8.GetBytes(Source);

  // Compresser les données
  Result := ZCompressBytes(SourceBytes);
end;

function DecompresserVersChaine(const Compressed: TBytes): string;
var
  SourceBytes: TBytes;
begin
  // Décompresser les données
  SourceBytes := ZDecompressBytes(Compressed);

  // Convertir en chaîne
  Result := TEncoding.UTF8.GetString(SourceBytes);
end;
```

### Gestion des fichiers GZIP

Le format GZIP est un format de compression populaire, en particulier pour les fichiers individuels ou les transferts web. Delphi supporte également la manipulation de fichiers GZIP :

```pascal
uses
  System.ZLib, System.Classes, System.SysUtils;

procedure CompresserFichierGZip(const FichierSource, FichierDestination: string);
var
  SourceStream: TFileStream;
  GZipStream: TGZFileStream;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    // Créer un flux GZIP
    GZipStream := TGZFileStream.Create(FichierDestination, gzOpenWrite);
    try
      // Copier les données par blocs
      repeat
        BytesRead := SourceStream.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          GZipStream.WriteBuffer(Buffer, BytesRead);
      until BytesRead = 0;
    finally
      GZipStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DecompresserFichierGZip(const FichierGZip, FichierDestination: string);
var
  GZipStream: TGZFileStream;
  DestStream: TFileStream;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  // Ouvrir le fichier GZIP en lecture
  GZipStream := TGZFileStream.Create(FichierGZip, gzOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Lire par blocs
      repeat
        BytesRead := GZipStream.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          DestStream.WriteBuffer(Buffer, BytesRead);
      until BytesRead = 0;
    finally
      DestStream.Free;
    end;
  finally
    GZipStream.Free;
  end;
end;
```

### Création d'archives personnalisées

Vous pouvez créer votre propre format d'archive en combinant plusieurs fichiers dans un seul fichier compressé :

```pascal
type
  TFileHeader = record
    Signature: array[0..3] of AnsiChar;  // Signature de l'archive ('MYAR')
    Version: Word;                       // Version du format
    NomFichier: array[0..255] of AnsiChar; // Nom du fichier original
    TailleOriginale: Int64;              // Taille avant compression
    TailleCompresse: Int64;              // Taille après compression
    DateCreation: TDateTime;             // Date de création
  end;

procedure AjouterFichierArchive(const FichierArchive, FichierAjouter: string);
var
  ArchiveStream: TFileStream;
  SourceStream: TFileStream;
  CompressedStream: TMemoryStream;
  CompressionStream: TZCompressionStream;
  Header: TFileHeader;
  FileNameAnsi: AnsiString;
begin
  // Ouvrir ou créer l'archive
  if FileExists(FichierArchive) then
    ArchiveStream := TFileStream.Create(FichierArchive, fmOpenReadWrite)
  else
    ArchiveStream := TFileStream.Create(FichierArchive, fmCreate);

  try
    // Se positionner à la fin de l'archive
    ArchiveStream.Position := ArchiveStream.Size;

    // Préparer l'en-tête
    FillChar(Header, SizeOf(Header), 0);
    Header.Signature := 'MYAR';
    Header.Version := 1;

    FileNameAnsi := AnsiString(ExtractFileName(FichierAjouter));
    if Length(FileNameAnsi) > 255 then
      SetLength(FileNameAnsi, 255);
    Move(FileNameAnsi[1], Header.NomFichier, Length(FileNameAnsi));

    Header.DateCreation := Now;

    // Compresser le fichier en mémoire
    SourceStream := TFileStream.Create(FichierAjouter, fmOpenRead);
    try
      Header.TailleOriginale := SourceStream.Size;

      CompressedStream := TMemoryStream.Create;
      try
        CompressionStream := TZCompressionStream.Create(
          CompressedStream, TZCompressionLevel.zcMax);
        try
          CompressionStream.CopyFrom(SourceStream, 0);
        finally
          CompressionStream.Free;
        end;

        // Récupérer la taille compressée
        Header.TailleCompresse := CompressedStream.Size;

        // Écrire l'en-tête dans l'archive
        ArchiveStream.WriteBuffer(Header, SizeOf(Header));

        // Écrire les données compressées
        CompressedStream.Position := 0;
        ArchiveStream.CopyFrom(CompressedStream, 0);
      finally
        CompressedStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  finally
    ArchiveStream.Free;
  end;
end;

procedure ExtraireFichierArchive(const FichierArchive, IndexOuNom: Variant; const DossierDestination: string);
var
  ArchiveStream: TFileStream;
  DecompressedStream: TFileStream;
  CompressedStream: TMemoryStream;
  DecompressionStream: TZDecompressionStream;
  Header: TFileHeader;
  Position: Int64;
  Index, CurrentIndex: Integer;
  FichierCherche: string;
  NomFichier: string;
  FichierTrouve: Boolean;
begin
  ArchiveStream := TFileStream.Create(FichierArchive, fmOpenRead);
  try
    Position := 0;
    CurrentIndex := 0;
    FichierTrouve := False;

    // Si c'est un index numérique
    if VarType(IndexOuNom) in [varByte, varWord, varLongWord, varInteger, varInt64] then
      Index := IndexOuNom
    else
      // Sinon c'est un nom de fichier
      FichierCherche := IndexOuNom;

    // Parcourir l'archive
    while Position < ArchiveStream.Size do
    begin
      // Se positionner pour lire l'en-tête
      ArchiveStream.Position := Position;

      // Lire l'en-tête
      ArchiveStream.ReadBuffer(Header, SizeOf(Header));

      // Vérifier la signature
      if String(Header.Signature) <> 'MYAR' then
        raise Exception.Create('Format d''archive invalide');

      // Extraire le nom du fichier
      NomFichier := String(Header.NomFichier).Trim;

      // Vérifier si c'est le fichier recherché
      if ((VarType(IndexOuNom) in [varByte, varWord, varLongWord, varInteger, varInt64]) and
          (CurrentIndex = Index)) or
         ((VarType(IndexOuNom) = varString) and
          (AnsiCompareText(NomFichier, FichierCherche) = 0)) then
      begin
        // Créer le flux de décompression
        CompressedStream := TMemoryStream.Create;
        try
          // Copier les données compressées
          ArchiveStream.Position := Position + SizeOf(Header);
          CompressedStream.CopyFrom(ArchiveStream, Header.TailleCompresse);
          CompressedStream.Position := 0;

          // Créer le fichier de destination
          if not DirectoryExists(DossierDestination) then
            ForceDirectories(DossierDestination);

          DecompressedStream := TFileStream.Create(
            IncludeTrailingPathDelimiter(DossierDestination) + NomFichier, fmCreate);
          try
            // Décompresser le fichier
            DecompressionStream := TZDecompressionStream.Create(CompressedStream);
            try
              DecompressedStream.CopyFrom(DecompressionStream, 0);
            finally
              DecompressionStream.Free;
            end;
          finally
            DecompressedStream.Free;
          end;

          FichierTrouve := True;
          Break;
        finally
          CompressedStream.Free;
        end;
      end;

      // Passer au fichier suivant
      Position := Position + SizeOf(Header) + Header.TailleCompresse;
      Inc(CurrentIndex);
    end;

    if not FichierTrouve then
      raise Exception.Create('Fichier non trouvé dans l''archive');
  finally
    ArchiveStream.Free;
  end;
end;
```

### Compression et décompression dans les applications réseau

La compression est particulièrement utile pour les applications réseau, car elle permet de réduire la quantité de données transférées :

```pascal
procedure EnvoyerDonneesCompressees(Socket: TClientSocket; const Donnees: string);
var
  MemStream: TMemoryStream;
  CompressedStream: TMemoryStream;
  CompressionStream: TZCompressionStream;
  CompressedSize: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    // Convertir la chaîne en octets et écrire dans le flux
    WriteStringToStream(MemStream, Donnees);
    MemStream.Position := 0;

    CompressedStream := TMemoryStream.Create;
    try
      // Compresser les données
      CompressionStream := TZCompressionStream.Create(CompressedStream, TZCompressionLevel.zcDefault);
      try
        CompressionStream.CopyFrom(MemStream, 0);
      finally
        CompressionStream.Free;
      end;

      // Envoyer la taille des données compressées
      CompressedSize := CompressedStream.Size;
      Socket.Socket.SendBuf(CompressedSize, SizeOf(Integer));

      // Envoyer les données compressées
      CompressedStream.Position := 0;
      Socket.Socket.SendStream(CompressedStream);
    finally
      CompressedStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

function RecevoirDonneesCompressees(Socket: TClientSocket): string;
var
  CompressedStream: TMemoryStream;
  DecompressedStream: TMemoryStream;
  DecompressionStream: TZDecompressionStream;
  CompressedSize: Integer;
begin
  // Recevoir la taille des données compressées
  Socket.Socket.ReceiveBuf(CompressedSize, SizeOf(Integer));

  CompressedStream := TMemoryStream.Create;
  try
    // Redimensionner le flux pour accueillir les données
    CompressedStream.Size := CompressedSize;

    // Recevoir les données compressées
    Socket.Socket.ReceiveBuf(CompressedStream.Memory^, CompressedSize);
    CompressedStream.Position := 0;

    DecompressedStream := TMemoryStream.Create;
    try
      // Décompresser les données
      DecompressionStream := TZDecompressionStream.Create(CompressedStream);
      try
        DecompressedStream.CopyFrom(DecompressionStream, 0);
      finally
        DecompressionStream.Free;
      end;

      // Convertir le flux en chaîne
      DecompressedStream.Position := 0;
      Result := ReadStringFromStream(DecompressedStream);
    finally
      DecompressedStream.Free;
    end;
  finally
    CompressedStream.Free;
  end;
end;
```

### Exemple d'application : Gestionnaire d'archives

Voici un exemple simple d'application permettant de créer et manipuler des archives ZIP :

```pascal
unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, System.Zip, System.IOUtils;

type
  TFormMain = class(TForm)
    pnlTop: TPanel;
    btnNouveau: TButton;
    btnOuvrir: TButton;
    btnAjouter: TButton;
    btnExtraire: TButton;
    lvFichiers: TListView;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnSupprimer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnNouveauClick(Sender: TObject);
    procedure btnOuvrirClick(Sender: TObject);
    procedure btnAjouterClick(Sender: TObject);
    procedure btnExtraireClick(Sender: TObject);
    procedure btnSupprimerClick(Sender: TObject);
  private
    FArchiveCourante: string;
    procedure AfficherContenuArchive;
    procedure MajBarreEtat;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FArchiveCourante := '';
  StatusBar1.SimpleText := 'Prêt';
end;

procedure TFormMain.AfficherContenuArchive;
var
  Zip: TZipFile;
  Header: TZipHeader;
  i: Integer;
  ListItem: TListItem;
begin
  lvFichiers.Clear;

  if (FArchiveCourante = '') or not FileExists(FArchiveCourante) then
    Exit;

  Zip := TZipFile.Create;
  try
    Zip.Open(FArchiveCourante, zmRead);

    // Ajouter chaque fichier à la liste
    for i := 0 to Zip.FileCount - 1 do
    begin
      Zip.Read(i, Header);

      ListItem := lvFichiers.Items.Add;
      ListItem.Caption := Header.FileName;
      ListItem.SubItems.Add(FormatFloat('#,##0', Header.UncompressedSize) + ' octets');
      ListItem.SubItems.Add(FormatFloat('#,##0', Header.CompressedSize) + ' octets');

      // Calculer le taux de compression
      if Header.UncompressedSize > 0 then
        ListItem.SubItems.Add(FormatFloat('0.0',
          (1 - Header.CompressedSize / Header.UncompressedSize) * 100) + '%')
      else
        ListItem.SubItems.Add('0%');

      ListItem.SubItems.Add(DateTimeToStr(FileDateToDateTime(Header.ModifiedDateTime)));
    end;

    Zip.Close;
  finally
    Zip.Free;
  end;

  MajBarreEtat;
end;

procedure TFormMain.MajBarreEtat;
begin
  if FArchiveCourante <> '' then
  begin
    StatusBar1.SimpleText := 'Archive : ' + FArchiveCourante + ' (' +
      IntToStr(lvFichiers.Items.Count) + ' fichiers)';
  end
  else
    StatusBar1.SimpleText := 'Aucune archive ouverte';
end;

procedure TFormMain.btnNouveauClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Archives ZIP (*.zip)|*.zip';
  SaveDialog1.DefaultExt := 'zip';

  if SaveDialog1.Execute then
  begin
    // Créer une nouvelle archive vide
    if FileExists(SaveDialog1.FileName) then
    begin
      if MessageDlg('Ce fichier existe déjà. Voulez-vous l''écraser ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;

      DeleteFile(SaveDialog1.FileName);
    end;

    // Créer un fichier ZIP vide
    var Zip := TZipFile.Create;
    try
      Zip.Open(SaveDialog1.FileName, zmWrite);
      Zip.Close;
    finally
      Zip.Free;
    end;

    FArchiveCourante := SaveDialog1.FileName;
    AfficherContenuArchive;
  end;
end;

procedure TFormMain.btnOuvrirClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Archives ZIP (*.zip)|*.zip';

  if OpenDialog1.Execute then
  begin
    if not FileExists(OpenDialog1.FileName) then
    begin
      ShowMessage('Le fichier n''existe pas !');
      Exit;
    end;

    FArchiveCourante := OpenDialog1.FileName;
    AfficherContenuArchive;
  end;
end;

procedure TFormMain.btnAjouterClick(Sender: TObject);
var
  Zip: TZipFile;
  i: Integer;
begin
  if FArchiveCourante = '' then
  begin
    ShowMessage('Aucune archive ouverte !');
    Exit;
  end;

  OpenDialog1.Filter := 'Tous les fichiers (*.*)|*.*';
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];

  if OpenDialog1.Execute then
  begin
    Zip := TZipFile.Create;
    try
      Zip.Open(FArchiveCourante, zmReadWrite);

      // Ajouter chaque fichier sélectionné
      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
        if FileExists(OpenDialog1.Files[i]) then
        begin
          try
            Zip.Add(OpenDialog1.Files[i], ExtractFileName(OpenDialog1.Files[i]));
          except
            on E: Exception do
              ShowMessage('Erreur lors de l''ajout de ' + OpenDialog1.Files[i] +
                ': ' + E.Message);
          end;
        end;
      end;

      Zip.Close;
    finally
      Zip.Free;
    end;

    // Rafraîchir l'affichage
    AfficherContenuArchive;
  end;

  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
end;

procedure TFormMain.btnExtraireClick(Sender: TObject);
var
  Zip: TZipFile;
  DossierDestination: string;
  i: Integer;
begin
  if FArchiveCourante = '' then
  begin
    ShowMessage('Aucune archive ouverte !');
    Exit;
  end;

  if lvFichiers.SelCount = 0 then
  begin
    ShowMessage('Aucun fichier sélectionné !');
    Exit;
  end;

  // Demander le dossier de destination
  var SelectDir := TFileOpenDialog.Create(nil);
  try
    SelectDir.Options := [fdoPickFolders];
    SelectDir.Title := 'Sélectionner le dossier de destination';

    if not SelectDir.Execute then
      Exit;

    DossierDestination := SelectDir.FileName;
  finally
    SelectDir.Free;
  end;

  // Extraire les fichiers sélectionnés
  Zip := TZipFile.Create;
  try
    Zip.Open(FArchiveCourante, zmRead);

    for i := 0 to lvFichiers.Items.Count - 1 do
    begin
      if lvFichiers.Items[i].Selected then
      begin
        try
          Zip.Extract(lvFichiers.Items[i].Caption,
            IncludeTrailingPathDelimiter(DossierDestination), False);
        except
          on E: Exception do
            ShowMessage('Erreur lors de l''extraction de ' +
              lvFichiers.Items[i].Caption + ': ' + E.Message);
        end;
      end;
    end;

    Zip.Close;

    ShowMessage('Extraction terminée avec succès !');
  finally
    Zip.Free;
  end;
end;

procedure TFormMain.btnSupprimerClick(Sender: TObject);
var
  TempArchive: string;
  Zip, TempZip: TZipFile;
  i, j: Integer;
  Header: TZipHeader;
  ASupprimer: TStringList;
begin
  if FArchiveCourante = '' then
  begin
    ShowMessage('Aucune archive ouverte !');
    Exit;
  end;

  if lvFichiers.SelCount = 0 then
  begin
    ShowMessage('Aucun fichier sélectionné !');
    Exit;
  end;

  if MessageDlg('Êtes-vous sûr de vouloir supprimer les fichiers sélectionnés ?',
    mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  // Créer une liste des fichiers à supprimer
  ASupprimer := TStringList.Create;
  try
    for i := 0 to lvFichiers.Items.Count - 1 do
    begin
      if lvFichiers.Items[i].Selected then
        ASupprimer.Add(lvFichiers.Items[i].Caption);
    end;

    // Créer une archive temporaire
    TempArchive := ChangeFileExt(FArchiveCourante, '.tmp');

    Zip := TZipFile.Create;
    TempZip := TZipFile.Create;
    try
      Zip.Open(FArchiveCourante, zmRead);
      TempZip.Open(TempArchive, zmWrite);

      // Copier uniquement les fichiers non sélectionnés
      for i := 0 to Zip.FileCount - 1 do
      begin
        Zip.Read(i, Header);

        if ASupprimer.IndexOf(Header.FileName) < 0 then
        begin
          // Extraire temporairement le fichier
          var TempFile := TPath.GetTempFileName;
          try
            Zip.Extract(i, TempFile);

            // L'ajouter à la nouvelle archive
            TempZip.Add(TempFile, Header.FileName);
          finally
            DeleteFile(TempFile);
          end;
        end;
      end;

      Zip.Close;
      TempZip.Close;

      // Remplacer l'ancienne archive par la nouvelle
      DeleteFile(FArchiveCourante);
      RenameFile(TempArchive, FArchiveCourante);

      // Rafraîchir l'affichage
      AfficherContenuArchive;
    finally
      Zip.Free;
      TempZip.Free;
    end;
  finally
    ASupprimer.Free;
  end;
end;

end.
```

Voici le fichier DFM de l'interface graphique :

```pascal
object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Gestionnaire d'#39'Archives'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 628
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnNouveau: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Nouveau'
      TabOrder = 0
      OnClick = btnNouveauClick
    end
    object btnOuvrir: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ouvrir...'
      TabOrder = 1
      OnClick = btnOuvrirClick
    end
    object btnAjouter: TButton
      Left = 170
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ajouter...'
      TabOrder = 2
      OnClick = btnAjouterClick
    end
    object btnExtraire: TButton
      Left = 251
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Extraire...'
      TabOrder = 3
      OnClick = btnExtraireClick
    end
    object btnSupprimer: TButton
      Left = 332
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Supprimer'
      TabOrder = 4
      OnClick = btnSupprimerClick
    end
  end
  object lvFichiers: TListView
    Left = 0
    Top = 41
    Width = 628
    Height = 382
    Align = alClient
    Columns = <
      item
        Caption = 'Nom du fichier'
        Width = 250
      end
      item
        Caption = 'Taille originale'
        Width = 100
      end
      item
        Caption = 'Taille compress'#233'e'
        Width = 100
      end
      item
        Caption = 'Taux'
        Width = 50
      end
      item
        Caption = 'Date'
        Width = 120
      end>
    GridLines = True
    MultiSelect = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 423
    Width = 628
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object OpenDialog1: TOpenDialog
    Left = 432
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 496
    Top = 8
  end
end
```

### Comparaison des différentes techniques de compression

Voici un tableau comparatif des différentes méthodes de compression disponibles en Delphi :

| Méthode | Avantages | Inconvénients | Cas d'utilisation |
|---------|-----------|---------------|-------------------|
| `TZipFile` | - Simple d'utilisation<br>- Format ZIP standard<br>- Gestion des fichiers et dossiers | - Moins flexible pour les données en mémoire | - Archivage de fichiers<br>- Sauvegarde de données<br>- Installation d'applications |
| `TZCompressionStream` | - Contrôle précis du niveau de compression<br>- Flexible pour tous types de données<br>- Intégration parfaite avec le système de flux | - Compression d'un flux à la fois<br>- Nécessite plus de code | - Compression à la volée<br>- Données en mémoire<br>- Transferts réseau |
| `ZCompressBytes` | - Utilisation très simple<br>- Idéal pour petites données | - Moins d'options<br>- Moins performant pour gros volumes | - Compression de chaînes<br>- Petites données binaires |
| `TGZFileStream` | - Format GZIP standard<br>- Utilisé sur le web | - Limité à un seul fichier | - Fichiers individuels<br>- Transferts HTTP |

### Bonnes pratiques pour la compression

1. **Choisir le bon format** :
   - ZIP pour les fichiers multiples avec structure de dossiers
   - GZIP pour les fichiers uniques ou transferts web
   - ZLib brut pour les données en mémoire ou transferts réseau

2. **Niveau de compression** :
   - Utiliser `zcMaximum` pour les archives ou sauvegardes (priorité à la taille)
   - Utiliser `zcDefault` pour un bon compromis
   - Utiliser `zcFastest` pour les transferts réseau en temps réel (priorité à la vitesse)

3. **Performance** :
   - Pour les gros fichiers, compresser par blocs
   - Utiliser des threads séparés pour la compression de gros fichiers
   - Fournir une barre de progression pour les opérations longues

4. **Sécurité** :
   - Vérifier les erreurs lors de la décompression
   - Méfiez-vous des "bombs de compression" (fichiers qui, une fois décompressés, deviennent extrêmement volumineux)
   - Limiter la taille maximale de décompression pour les données externes

5. **Interopérabilité** :
   - Préférer les formats standards (ZIP, GZIP) pour les fichiers qui seront utilisés sur d'autres plateformes
   - Tester la compatibilité avec les outils standards (WinZip, 7-Zip, etc.)

### Optimisation de la compression

La compression de données peut être coûteuse en ressources. Voici quelques conseils pour l'optimiser :

```pascal
procedure CompresserFichierGrosEnThreads(const FichierSource, FichierDestination: string);
var
  Taille, PositionCourante, TailleBloc: Int64;
  Threads: array of TThread;
  i, NbThreads: Integer;
  ThreadsTermines: Boolean;
  FichiersTmp: TStringList;
begin
  // Déterminer la taille du fichier
  Taille := TFile.GetSize(FichierSource);

  // Adapter le nombre de threads et la taille des blocs
  if Taille < 1024 * 1024 then  // Moins de 1 Mo
  begin
    NbThreads := 1;
    TailleBloc := Taille;
  end
  else if Taille < 10 * 1024 * 1024 then  // Moins de 10 Mo
  begin
    NbThreads := 2;
    TailleBloc := Taille div NbThreads;
  end
  else if Taille < 100 * 1024 * 1024 then  // Moins de 100 Mo
  begin
    NbThreads := 4;
    TailleBloc := Taille div NbThreads;
  end
  else  // Plus de 100 Mo
  begin
    NbThreads := 8;
    TailleBloc := Taille div NbThreads;
  end;

  // Créer les fichiers temporaires pour chaque bloc
  FichiersTmp := TStringList.Create;
  try
    SetLength(Threads, NbThreads);
    PositionCourante := 0;

    // Créer un thread pour chaque bloc
    for i := 0 to NbThreads - 1 do
    begin
      var FichierTmp := TPath.GetTempFileName;
      FichiersTmp.Add(FichierTmp);

      var BlocDebut := PositionCourante;
      var BlocFin := BlocDebut + TailleBloc;

      // Ajuster le dernier bloc
      if i = NbThreads - 1 then
        BlocFin := Taille;

      // Créer le thread de compression
      Threads[i] := TThread.CreateAnonymousThread(
        procedure
        var
          SourceStream: TFileStream;
          DestStream: TFileStream;
          CompressionStream: TZCompressionStream;
        begin
          SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
          try
            SourceStream.Position := BlocDebut;

            DestStream := TFileStream.Create(FichierTmp, fmCreate);
            try
              CompressionStream := TZCompressionStream.Create(
                DestStream, TZCompressionLevel.zcDefault);
              try
                CompressionStream.CopyFrom(SourceStream, BlocFin - BlocDebut);
              finally
                CompressionStream.Free;
              end;
            finally
              DestStream.Free;
            end;
          finally
            SourceStream.Free;
          end;
        end);

      // Démarrer le thread
      Threads[i].Start;

      // Préparer pour le prochain bloc
      PositionCourante := BlocFin;
    end;

    // Attendre que tous les threads soient terminés
    repeat
      ThreadsTermines := True;
      for i := 0 to NbThreads - 1 do
      begin
        if not Threads[i].Finished then
        begin
          ThreadsTermines := False;
          Break;
        end;
      end;

      if not ThreadsTermines then
        Sleep(100);
    until ThreadsTermines;

    // Fusionner les fichiers temporaires en un seul fichier
    var FinalStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      for i := 0 to FichiersTmp.Count - 1 do
      begin
        var TmpStream := TFileStream.Create(FichiersTmp[i], fmOpenRead);
        try
          FinalStream.CopyFrom(TmpStream, 0);
        finally
          TmpStream.Free;
        end;
      end;
    finally
      FinalStream.Free;
    end;
  finally
    // Supprimer les fichiers temporaires
    for i := 0 to FichiersTmp.Count - 1 do
      DeleteFile(FichiersTmp[i]);

    FichiersTmp.Free;
  end;
end;
```

> **Note :** Ce code est un exemple simplifié et ne gère pas tous les cas particuliers. Dans une application réelle, vous auriez besoin d'ajouter plus de vérifications d'erreurs et de synchronisation entre les threads.

### Compression et formats de données courants

#### Images

Delphi permet de manipuler différents formats d'images qui utilisent déjà la compression :

```pascal
procedure CompressImage(const SourceFileName, DestFileName: string; Quality: Integer);
var
  SourceImage, JpegImage: TImage;
  JpegBitmap: TBitmap;
  Jpeg: TJPEGImage;
begin
  // Qualité doit être entre 1 (faible qualité, haute compression) et 100 (haute qualité)
  if (Quality < 1) or (Quality > 100) then
    Quality := 80;  // Valeur par défaut

  SourceImage := TImage.Create(nil);
  JpegImage := TImage.Create(nil);
  try
    // Charger l'image source
    SourceImage.Picture.LoadFromFile(SourceFileName);

    // Préparer l'image JPEG
    JpegBitmap := TBitmap.Create;
    try
      // Copier l'image source dans un bitmap
      JpegBitmap.Assign(SourceImage.Picture.Graphic);

      // Créer un JPEG à partir du bitmap
      Jpeg := TJPEGImage.Create;
      try
        Jpeg.Assign(JpegBitmap);
        Jpeg.CompressionQuality := Quality;
        Jpeg.SaveToFile(DestFileName);
      finally
        Jpeg.Free;
      end;
    finally
      JpegBitmap.Free;
    end;
  finally
    SourceImage.Free;
    JpegImage.Free;
  end;
end;
```

#### Base64

La représentation Base64 est souvent utilisée pour encoder des données binaires en texte, mais elle augmente la taille d'environ 33%. En combinant la compression et le Base64, vous pouvez réduire cette augmentation :

```pascal
function CompressAndBase64Encode(const Data: TBytes): string;
var
  CompressedData: TBytes;
begin
  // Compresser les données
  CompressedData := ZCompressBytes(Data);

  // Encoder en Base64
  Result := TNetEncoding.Base64.EncodeBytesToString(CompressedData);
end;

function Base64DecodeAndDecompress(const Base64Data: string): TBytes;
var
  CompressedData: TBytes;
begin
  // Décoder le Base64
  CompressedData := TNetEncoding.Base64.DecodeStringToBytes(Base64Data);

  // Décompresser les données
  Result := ZDecompressBytes(CompressedData);
end;
```

### Exercice pratique

Créez une application qui permet de :

1. Compresser un dossier entier en archive ZIP
2. Afficher les statistiques de compression (taux, gain d'espace)
3. Extraire des fichiers sélectionnés
4. Ajouter une protection par mot de passe (indice : recherchez TZipFile.Password)

Cet exercice vous permettra de mettre en pratique les concepts de compression tout en créant une application utile.

---

À suivre dans la prochaine section : **7.6 Traitement par lots (Batch)**
