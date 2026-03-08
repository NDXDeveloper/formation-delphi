🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.5 Compression et décompression

## Introduction

La compression de données est une technique permettant de réduire la taille des fichiers ou des données en mémoire. C'est particulièrement utile pour économiser de l'espace de stockage, accélérer les transferts réseau, ou optimiser les performances d'une application.

**Analogie simple :** La compression est comme ranger des vêtements dans une valise. Au lieu de les laisser dépliés, vous les pliez soigneusement pour qu'ils prennent moins de place. La décompression, c'est sortir les vêtements et les déplier pour les utiliser à nouveau.

## Concepts fondamentaux

### Qu'est-ce que la compression ?

La **compression** est le processus de réduction de la taille d'un fichier ou d'un ensemble de données en éliminant les redondances ou en utilisant des algorithmes mathématiques intelligents.

La **décompression** est l'opération inverse : reconstituer les données originales à partir des données compressées.

### Types de compression

Il existe deux grandes catégories de compression :

#### 1. Compression sans perte (Lossless)

Les données décompressées sont **exactement identiques** aux données originales.

**Exemples :**
- ZIP, GZIP, 7Z
- PNG (images)
- FLAC (audio)

**Usage :** Documents, code source, bases de données, archives

#### 2. Compression avec perte (Lossy)

Les données décompressées sont **similaires mais pas identiques** aux données originales. Certaines informations sont perdues pour obtenir un meilleur taux de compression.

**Exemples :**
- JPEG (images)
- MP3, AAC (audio)
- MP4, H.264 (vidéo)

**Usage :** Multimédia (photos, musique, vidéos)

> **Note :** Dans ce chapitre, nous nous concentrerons sur la compression sans perte, qui est celle utilisée pour les données et fichiers dans les applications.

### Taux de compression

Le **taux de compression** indique à quel point les données ont été réduites :

```
Taux de compression = (Taille originale - Taille compressée) / Taille originale × 100%

Exemple :  
Fichier original : 1000 Ko  
Fichier compressé : 300 Ko  
Taux : (1000 - 300) / 1000 × 100% = 70%  
```

**Note :** Tous les fichiers ne se compressent pas aussi bien. Les fichiers texte ou répétitifs se compressent bien, tandis que les fichiers déjà compressés (JPEG, MP3, ZIP) ne se compressent pratiquement pas.

---

## ZLib : La bibliothèque standard

Delphi inclut nativement la bibliothèque **ZLib**, qui est l'une des bibliothèques de compression les plus utilisées au monde. Elle implémente l'algorithme DEFLATE.

### Importer l'unité ZLib

```pascal
uses
  System.ZLib, System.Classes, System.SysUtils;
```

---

## Compression de streams

### Compression basique avec TZCompressionStream

```pascal
procedure CompresserStream(Source, Destination: TStream);  
var  
  Compresseur: TZCompressionStream;
begin
  Source.Position := 0;
  Destination.Size := 0;

  // Créer le compresseur
  Compresseur := TZCompressionStream.Create(Destination);
  try
    // Copier les données source dans le compresseur
    Compresseur.CopyFrom(Source, 0);
  finally
    Compresseur.Free;
  end;
end;

// Exemple d'utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Original, Compresse: TMemoryStream;
  i: Integer;
  TauxCompression: Double;
begin
  Original := TMemoryStream.Create;
  Compresse := TMemoryStream.Create;
  try
    // Créer des données à comprimer
    for i := 1 to 10000 do
      Original.WriteBuffer(i, SizeOf(Integer));

    // Comprimer
    CompresserStream(Original, Compresse);

    // Calculer et afficher le taux de compression
    TauxCompression := (1 - Compresse.Size / Original.Size) * 100;

    ShowMessage(Format('Taille originale : %d octets' + #13#10 +
                       'Taille compressée : %d octets' + #13#10 +
                       'Taux de compression : %.2f%%',
                       [Original.Size, Compresse.Size, TauxCompression]));
  finally
    Original.Free;
    Compresse.Free;
  end;
end;
```

### Décompression avec TZDecompressionStream

```pascal
procedure DecompresserStream(Source, Destination: TStream);  
var  
  Decompresseur: TZDecompressionStream;
  Buffer: array[0..4095] of Byte;
  BytesLus: Integer;
begin
  Source.Position := 0;
  Destination.Size := 0;

  // Créer le décompresseur
  Decompresseur := TZDecompressionStream.Create(Source);
  try
    // Lire par blocs car TZDecompressionStream ne connaît pas sa taille
    repeat
      BytesLus := Decompresseur.Read(Buffer, SizeOf(Buffer));
      if BytesLus > 0 then
        Destination.WriteBuffer(Buffer, BytesLus);
    until BytesLus = 0;
  finally
    Decompresseur.Free;
  end;
end;

// Exemple complet : compression et décompression
procedure TForm1.Button2Click(Sender: TObject);  
var  
  Original, Compresse, Decompresse: TMemoryStream;
  Texte, TexteDecompresse: AnsiString;
begin
  Original := TMemoryStream.Create;
  Compresse := TMemoryStream.Create;
  Decompresse := TMemoryStream.Create;
  try
    // Créer du texte répétitif (se compresse bien)
    Texte := 'Bonjour tout le monde ! ';
    Texte := Texte + Texte + Texte + Texte; // Répéter plusieurs fois

    // Écrire dans le stream original
    Original.WriteBuffer(Texte[1], Length(Texte));

    ShowMessage('Original : ' + IntToStr(Original.Size) + ' octets');

    // Comprimer
    CompresserStream(Original, Compresse);
    ShowMessage('Compressé : ' + IntToStr(Compresse.Size) + ' octets');

    // Décompresser
    DecompresserStream(Compresse, Decompresse);
    ShowMessage('Décompressé : ' + IntToStr(Decompresse.Size) + ' octets');

    // Vérifier que c'est identique
    Decompresse.Position := 0;
    SetLength(TexteDecompresse, Decompresse.Size);
    Decompresse.ReadBuffer(TexteDecompresse[1], Decompresse.Size);

    if Texte = TexteDecompresse then
      ShowMessage('Décompression réussie : données identiques')
    else
      ShowMessage('Erreur : données différentes');
  finally
    Original.Free;
    Compresse.Free;
    Decompresse.Free;
  end;
end;
```

---

## Niveaux de compression

ZLib offre différents niveaux de compression qui permettent de choisir entre vitesse et taux de compression.

### Les niveaux disponibles

```pascal
type
  TZCompressionLevel = (
    zcNone,       // Pas de compression (0)
    zcFastest,    // Le plus rapide, compression minimale (1)
    zcDefault,    // Compromis vitesse/compression (6)
    zcMax         // Compression maximale, plus lent (9)
  );
```

### Utilisation des niveaux

```pascal
procedure CompresserAvecNiveau(Source, Destination: TStream;
                               Niveau: TZCompressionLevel);
var
  Compresseur: TZCompressionStream;
begin
  Source.Position := 0;
  Destination.Size := 0;

  // Créer le compresseur avec le niveau spécifié
  Compresseur := TZCompressionStream.Create(Destination, Niveau);
  try
    Compresseur.CopyFrom(Source, 0);
  finally
    Compresseur.Free;
  end;
end;

// Comparaison des niveaux
procedure TForm1.CompareNiveaux;  
var  
  Original, Compresse1, Compresse2, Compresse3: TMemoryStream;
  i: Integer;
  Debut, Fin: Cardinal;
  Temps: Cardinal;
begin
  Original := TMemoryStream.Create;
  Compresse1 := TMemoryStream.Create;
  Compresse2 := TMemoryStream.Create;
  Compresse3 := TMemoryStream.Create;
  try
    // Créer des données test
    for i := 1 to 100000 do
      Original.WriteBuffer(i, SizeOf(Integer));

    // Niveau zcFastest
    Debut := GetTickCount;
    CompresserAvecNiveau(Original, Compresse1, zcFastest);
    Temps := GetTickCount - Debut;
    Memo1.Lines.Add(Format('Fastest : %d octets, %d ms',
      [Compresse1.Size, Temps]));

    // Niveau zcDefault
    Debut := GetTickCount;
    CompresserAvecNiveau(Original, Compresse2, zcDefault);
    Temps := GetTickCount - Debut;
    Memo1.Lines.Add(Format('Default : %d octets, %d ms',
      [Compresse2.Size, Temps]));

    // Niveau zcMax
    Debut := GetTickCount;
    CompresserAvecNiveau(Original, Compresse3, zcMax);
    Temps := GetTickCount - Debut;
    Memo1.Lines.Add(Format('Max : %d octets, %d ms',
      [Compresse3.Size, Temps]));
  finally
    Original.Free;
    Compresse1.Free;
    Compresse2.Free;
    Compresse3.Free;
  end;
end;
```

**Recommandations :**
- **zcFastest** : pour de gros volumes où la vitesse prime
- **zcDefault** : bon compromis pour la plupart des usages
- **zcMax** : pour maximiser l'économie d'espace (archives, backups)

---

## Compression de fichiers

### Comprimer un fichier

```pascal
procedure CompresserFichier(const FichierSource, FichierDestination: string);  
var  
  SourceStream, DestStream: TFileStream;
  Compresseur: TZCompressionStream;
begin
  // Ouvrir le fichier source
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    // Créer le fichier destination
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer le compresseur
      Compresseur := TZCompressionStream.Create(DestStream, zcDefault);
      try
        // Copier et comprimer
        Compresseur.CopyFrom(SourceStream, 0);
      finally
        Compresseur.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button3Click(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    CompresserFichier(OpenDialog1.FileName,
                      OpenDialog1.FileName + '.zlib');
    ShowMessage('Fichier compressé avec succès');
  end;
end;
```

### Décompresser un fichier

```pascal
procedure DecompresserFichier(const FichierSource, FichierDestination: string);  
var  
  SourceStream, DestStream: TFileStream;
  Decompresseur: TZDecompressionStream;
begin
  // Ouvrir le fichier compressé
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    // Créer le fichier destination
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer le décompresseur et lire par blocs
      Decompresseur := TZDecompressionStream.Create(SourceStream);
      try
        var Buffer: array[0..4095] of Byte;
        var BytesLus: Integer;
        repeat
          BytesLus := Decompresseur.Read(Buffer, SizeOf(Buffer));
          if BytesLus > 0 then
            DestStream.WriteBuffer(Buffer, BytesLus);
        until BytesLus = 0;
      finally
        Decompresseur.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button4Click(Sender: TObject);  
var  
  FichierCompresse, FichierDecompresse: string;
begin
  if OpenDialog1.Execute then
  begin
    FichierCompresse := OpenDialog1.FileName;
    FichierDecompresse := ChangeFileExt(FichierCompresse, '.txt');

    DecompresserFichier(FichierCompresse, FichierDecompresse);
    ShowMessage('Fichier décompressé : ' + FichierDecompresse);
  end;
end;
```

### Fonction complète avec gestion d'erreurs

```pascal
function CompresserFichierSecurise(const Source, Destination: string): Boolean;  
begin  
  Result := False;

  // Vérifier que le fichier source existe
  if not FileExists(Source) then
  begin
    ShowMessage('Fichier source introuvable : ' + Source);
    Exit;
  end;

  try
    CompresserFichier(Source, Destination);
    Result := True;
  except
    on E: EFOpenError do
      ShowMessage('Impossible d''ouvrir le fichier : ' + E.Message);
    on E: EWriteError do
      ShowMessage('Erreur d''écriture : ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur lors de la compression : ' + E.Message);
  end;
end;
```

---

## Compression de chaînes de caractères

### Fonctions utilitaires

```pascal
function CompresserString(const S: string): TBytes;  
var  
  SourceStream, DestStream: TMemoryStream;
  Compresseur: TZCompressionStream;
  Bytes: TBytes;
begin
  // Convertir la chaîne en bytes
  Bytes := TEncoding.UTF8.GetBytes(S);

  SourceStream := TMemoryStream.Create;
  DestStream := TMemoryStream.Create;
  try
    // Écrire dans le stream source
    if Length(Bytes) > 0 then
      SourceStream.WriteBuffer(Bytes[0], Length(Bytes));

    SourceStream.Position := 0;

    // Comprimer
    Compresseur := TZCompressionStream.Create(DestStream, zcDefault);
    try
      Compresseur.CopyFrom(SourceStream, 0);
    finally
      Compresseur.Free;
    end;

    // Récupérer les bytes compressés
    SetLength(Result, DestStream.Size);
    DestStream.Position := 0;
    DestStream.ReadBuffer(Result[0], DestStream.Size);
  finally
    SourceStream.Free;
    DestStream.Free;
  end;
end;

function DecompresserString(const CompressedData: TBytes): string;  
var  
  SourceStream, DestStream: TMemoryStream;
  Decompresseur: TZDecompressionStream;
  Bytes: TBytes;
begin
  SourceStream := TMemoryStream.Create;
  DestStream := TMemoryStream.Create;
  try
    // Écrire les données compressées
    if Length(CompressedData) > 0 then
      SourceStream.WriteBuffer(CompressedData[0], Length(CompressedData));

    SourceStream.Position := 0;

    // Décompresser par blocs
    Decompresseur := TZDecompressionStream.Create(SourceStream);
    try
      var Buffer: array[0..4095] of Byte;
      var BytesLus: Integer;
      repeat
        BytesLus := Decompresseur.Read(Buffer, SizeOf(Buffer));
        if BytesLus > 0 then
          DestStream.WriteBuffer(Buffer, BytesLus);
      until BytesLus = 0;
    finally
      Decompresseur.Free;
    end;

    // Convertir en string
    if DestStream.Size > 0 then
    begin
      SetLength(Bytes, DestStream.Size);
      DestStream.Position := 0;
      DestStream.ReadBuffer(Bytes[0], DestStream.Size);
      Result := TEncoding.UTF8.GetString(Bytes);
    end
    else
      Result := '';
  finally
    SourceStream.Free;
    DestStream.Free;
  end;
end;

// Exemple d'utilisation
procedure TForm1.Button5Click(Sender: TObject);  
var  
  TexteOriginal, TexteDecompresse: string;
  DonneesCompressees: TBytes;
  TauxCompression: Double;
begin
  TexteOriginal := Memo1.Lines.Text;

  // Comprimer
  DonneesCompressees := CompresserString(TexteOriginal);

  TauxCompression := (1 - Length(DonneesCompressees) /
                     (Length(TexteOriginal) * SizeOf(Char))) * 100;

  ShowMessage(Format('Original : %d octets' + #13#10 +
                     'Compressé : %d octets' + #13#10 +
                     'Taux : %.2f%%',
                     [Length(TexteOriginal) * SizeOf(Char),
                      Length(DonneesCompressees),
                      TauxCompression]));

  // Décompresser
  TexteDecompresse := DecompresserString(DonneesCompressees);

  // Vérifier
  if TexteOriginal = TexteDecompresse then
    ShowMessage('Décompression réussie !')
  else
    ShowMessage('Erreur : textes différents');
end;
```

---

## Archives ZIP

Pour créer et manipuler des archives ZIP complètes, Delphi offre l'unité `System.Zip`.

### Créer une archive ZIP

```pascal
uses
  System.Zip;

procedure CreerArchiveZip(const FichiersACompresser: TStringList;
                          const NomArchive: string);
var
  ZipFile: TZipFile;
  Fichier: string;
begin
  ZipFile := TZipFile.Create;
  try
    // Ouvrir/créer l'archive en mode écriture
    ZipFile.Open(NomArchive, zmWrite);

    // Ajouter chaque fichier
    for Fichier in FichiersACompresser do
    begin
      if FileExists(Fichier) then
        ZipFile.Add(Fichier);
    end;

    // Fermer l'archive
    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;

// Utilisation
procedure TForm1.Button6Click(Sender: TObject);  
var  
  Fichiers: TStringList;
begin
  Fichiers := TStringList.Create;
  try
    // Ajouter les fichiers à archiver
    Fichiers.Add('C:\Documents\fichier1.txt');
    Fichiers.Add('C:\Documents\fichier2.txt');
    Fichiers.Add('C:\Documents\photo.jpg');

    CreerArchiveZip(Fichiers, 'C:\Archives\monarchive.zip');
    ShowMessage('Archive créée avec succès');
  finally
    Fichiers.Free;
  end;
end;
```

### Extraire une archive ZIP

```pascal
procedure ExtraireArchiveZip(const NomArchive, DossierDestination: string);  
var  
  ZipFile: TZipFile;
begin
  ZipFile := TZipFile.Create;
  try
    // Ouvrir l'archive en lecture
    ZipFile.Open(NomArchive, zmRead);

    // Extraire tous les fichiers
    ZipFile.ExtractAll(DossierDestination);

    // Fermer l'archive
    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;

// Utilisation
procedure TForm1.Button7Click(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    ExtraireArchiveZip(OpenDialog1.FileName, 'C:\Extraction\');
    ShowMessage('Extraction terminée');
  end;
end;
```

### Lister le contenu d'une archive

```pascal
procedure ListerContenuZip(const NomArchive: string; Liste: TStrings);  
var  
  ZipFile: TZipFile;
  NomFichier: string;
begin
  Liste.Clear;

  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(NomArchive, zmRead);

    // Parcourir tous les fichiers
    for NomFichier in ZipFile.FileNames do
      Liste.Add(NomFichier);

    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;

// Utilisation
procedure TForm1.Button8Click(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    ListerContenuZip(OpenDialog1.FileName, Memo1.Lines);
    ShowMessage(Format('L''archive contient %d fichiers',
      [Memo1.Lines.Count]));
  end;
end;
```

### Extraire un seul fichier d'une archive

```pascal
procedure ExtraireFichierSpecifique(const NomArchive, NomFichier,
                                    Destination: string);
var
  ZipFile: TZipFile;
begin
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(NomArchive, zmRead);

    // Extraire le fichier spécifique
    ZipFile.Extract(NomFichier, Destination);

    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;

// Utilisation
procedure TForm1.ExtraireUnFichier;  
var  
  Archive, Fichier, Destination: string;
begin
  Archive := 'C:\monarchive.zip';
  Fichier := 'documents/rapport.pdf';
  Destination := 'C:\Extraction\';

  ExtraireFichierSpecifique(Archive, Fichier, Destination);
end;
```

### Ajouter un fichier à une archive existante

```pascal
procedure AjouterFichierAArchive(const NomArchive, FichierAAjouter: string);  
var  
  ZipFile: TZipFile;
begin
  ZipFile := TZipFile.Create;
  try
    // Ouvrir en mode lecture/écriture
    ZipFile.Open(NomArchive, zmReadWrite);

    // Ajouter le fichier
    ZipFile.Add(FichierAAjouter);

    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;
```

### Informations détaillées sur une archive

```pascal
procedure AfficherInfosArchive(const NomArchive: string);  
var  
  ZipFile: TZipFile;
  Header: TZipHeader;
  i: Integer;
  TailleCompresse, TailleOriginale: Int64;
  TauxCompression: Double;
  Infos: string;
begin
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(NomArchive, zmRead);

    TailleCompresse := 0;
    TailleOriginale := 0;

    Infos := Format('Archive : %s' + #13#10 +
                    'Nombre de fichiers : %d' + #13#10#13#10,
                    [ExtractFileName(NomArchive),
                     ZipFile.FileCount]);

    // Parcourir les fichiers via FileInfo (TZipHeader)
    for i := 0 to ZipFile.FileCount - 1 do
    begin
      Header := ZipFile.FileInfo[i];
      TailleOriginale := TailleOriginale + Header.UncompressedSize;
      TailleCompresse := TailleCompresse + Header.CompressedSize;

      Infos := Infos + Format('%s : %d -> %d octets' + #13#10,
        [ZipFile.FileNames[i],
         Header.UncompressedSize,
         Header.CompressedSize]);
    end;

    TauxCompression := (1 - TailleCompresse / TailleOriginale) * 100;

    Infos := Infos + #13#10 + Format('Total original : %d octets' + #13#10 +
                                      'Total compressé : %d octets' + #13#10 +
                                      'Taux de compression : %.2f%%',
                                      [TailleOriginale, TailleCompresse,
                                       TauxCompression]);

    ShowMessage(Infos);
    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;
```

---

## Application pratique : Sauvegarde compressée

Voici un exemple complet de système de sauvegarde avec compression.

```pascal
type
  TBackupManager = class
  private
    FDossierSource: string;
    FDossierDestination: string;
  public
    constructor Create(const Source, Destination: string);

    function CreerSauvegarde: Boolean;
    procedure RestaurerSauvegarde(const NomArchive: string);
    function ListerSauvegardes: TStringList;

    property DossierSource: string read FDossierSource write FDossierSource;
    property DossierDestination: string read FDossierDestination
      write FDossierDestination;
  end;

constructor TBackupManager.Create(const Source, Destination: string);  
begin  
  inherited Create;
  FDossierSource := IncludeTrailingPathDelimiter(Source);
  FDossierDestination := IncludeTrailingPathDelimiter(Destination);

  // Créer le dossier de destination s'il n'existe pas
  if not DirectoryExists(FDossierDestination) then
    ForceDirectories(FDossierDestination);
end;

function TBackupManager.CreerSauvegarde: Boolean;  
var  
  ZipFile: TZipFile;
  NomArchive: string;
  Fichiers: TStringDynArray;
  Fichier: string;
begin
  Result := False;

  try
    // Nom de l'archive avec horodatage
    NomArchive := FDossierDestination + 'backup_' +
                  FormatDateTime('yyyymmdd_hhnnss', Now) + '.zip';

    // Récupérer tous les fichiers du dossier source
    Fichiers := TDirectory.GetFiles(FDossierSource, '*.*',
                                    TSearchOption.soAllDirectories);

    if Length(Fichiers) = 0 then
    begin
      ShowMessage('Aucun fichier à sauvegarder');
      Exit;
    end;

    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(NomArchive, zmWrite);

      // Ajouter chaque fichier
      for Fichier in Fichiers do
      begin
        // Conserver la structure de répertoires
        ZipFile.Add(Fichier,
                   StringReplace(Fichier, FDossierSource, '', []));
      end;

      ZipFile.Close;
      Result := True;

      ShowMessage(Format('Sauvegarde créée : %s' + #13#10 +
                         '%d fichiers archivés',
                         [NomArchive, Length(Fichiers)]));
    finally
      ZipFile.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la sauvegarde : ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TBackupManager.RestaurerSauvegarde(const NomArchive: string);  
var  
  ZipFile: TZipFile;
  DossierRestauration: string;
begin
  if not FileExists(NomArchive) then
  begin
    ShowMessage('Archive introuvable : ' + NomArchive);
    Exit;
  end;

  try
    // Créer un dossier de restauration
    DossierRestauration := FDossierDestination + 'restauration_' +
                          FormatDateTime('yyyymmdd_hhnnss', Now) + '\';
    ForceDirectories(DossierRestauration);

    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(NomArchive, zmRead);
      ZipFile.ExtractAll(DossierRestauration);
      ZipFile.Close;

      ShowMessage('Restauration terminée dans : ' + DossierRestauration);
    finally
      ZipFile.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la restauration : ' + E.Message);
  end;
end;

function TBackupManager.ListerSauvegardes: TStringList;  
var  
  Fichiers: TStringDynArray;
  Fichier: string;
begin
  Result := TStringList.Create;

  // Chercher tous les fichiers .zip dans le dossier destination
  Fichiers := TDirectory.GetFiles(FDossierDestination, 'backup_*.zip');

  for Fichier in Fichiers do
    Result.Add(Fichier);

  Result.Sort;
end;

// Utilisation
procedure TForm1.Button9Click(Sender: TObject);  
var  
  BackupManager: TBackupManager;
begin
  BackupManager := TBackupManager.Create('C:\MesDonnees\', 'C:\Sauvegardes\');
  try
    if BackupManager.CreerSauvegarde then
      ShowMessage('Sauvegarde réussie');
  finally
    BackupManager.Free;
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);  
var  
  BackupManager: TBackupManager;
  Sauvegardes: TStringList;
begin
  BackupManager := TBackupManager.Create('C:\MesDonnees\', 'C:\Sauvegardes\');
  try
    Sauvegardes := BackupManager.ListerSauvegardes;
    try
      Memo1.Lines.Assign(Sauvegardes);
    finally
      Sauvegardes.Free;
    end;
  finally
    BackupManager.Free;
  end;
end;
```

---

## Note sur la protection par mot de passe (ZIP)

> **Important :** La classe `TZipFile` de Delphi (`System.Zip`) **ne supporte pas nativement** la création d'archives ZIP protégées par mot de passe. Pour cette fonctionnalité, vous devrez utiliser une bibliothèque tierce comme **Abbrevia**, **TurboPower Abbrevia** ou **7-Zip SDK**.
>
> Si vous avez besoin de protéger des données, vous pouvez combiner la compression ZLib avec un chiffrement séparé (par exemple via les API de chiffrement de Delphi dans `System.NetEncoding` ou des bibliothèques cryptographiques).

---

## Optimisation et bonnes pratiques

### 1. Choisir le bon niveau de compression

```pascal
function ChoisirNiveauCompression(TailleFichier: Int64): TZCompressionLevel;  
begin  
  if TailleFichier < 1024 * 1024 then // < 1 Mo
    Result := zcMax  // Petits fichiers : compression max
  else if TailleFichier < 100 * 1024 * 1024 then // < 100 Mo
    Result := zcDefault  // Fichiers moyens : compromis
  else
    Result := zcFastest;  // Gros fichiers : vitesse prioritaire
end;
```

### 2. Compresser par blocs pour les gros fichiers

```pascal
procedure CompresserParBlocs(const Source, Destination: string);  
const  
  TAILLE_BLOC = 1024 * 1024; // 1 Mo
var
  SourceStream, DestStream: TFileStream;
  Compresseur: TZCompressionStream;
  Buffer: array[0..TAILLE_BLOC-1] of Byte;
  BytesLus: Integer;
begin
  SourceStream := TFileStream.Create(Source, fmOpenRead);
  try
    DestStream := TFileStream.Create(Destination, fmCreate);
    try
      Compresseur := TZCompressionStream.Create(DestStream, zcDefault);
      try
        repeat
          BytesLus := SourceStream.Read(Buffer, TAILLE_BLOC);
          if BytesLus > 0 then
            Compresseur.WriteBuffer(Buffer, BytesLus);
        until BytesLus = 0;
      finally
        Compresseur.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### 3. Vérifier si la compression est utile

```pascal
function CompressionEstUtile(const Extension: string): Boolean;  
var  
  ExtensionsNonCompressibles: TStringList;
begin
  ExtensionsNonCompressibles := TStringList.Create;
  try
    // Fichiers déjà compressés
    ExtensionsNonCompressibles.Add('.zip');
    ExtensionsNonCompressibles.Add('.rar');
    ExtensionsNonCompressibles.Add('.7z');
    ExtensionsNonCompressibles.Add('.jpg');
    ExtensionsNonCompressibles.Add('.jpeg');
    ExtensionsNonCompressibles.Add('.png');
    ExtensionsNonCompressibles.Add('.mp3');
    ExtensionsNonCompressibles.Add('.mp4');
    ExtensionsNonCompressibles.Add('.avi');
    ExtensionsNonCompressibles.Add('.mkv');

    Result := ExtensionsNonCompressibles.IndexOf(
      LowerCase(Extension)) = -1;
  finally
    ExtensionsNonCompressibles.Free;
  end;
end;

procedure CompresserIntelligent(const Source, Destination: string);  
var  
  Extension: string;
  TailleSource, TailleCompresse: Int64;
begin
  Extension := ExtractFileExt(Source);

  if not CompressionEstUtile(Extension) then
  begin
    // Copier simplement le fichier
    TFile.Copy(Source, Destination, True);
    ShowMessage('Fichier copié sans compression (déjà compressé)');
    Exit;
  end;

  // Comprimer
  CompresserFichier(Source, Destination);

  // Vérifier si la compression a été efficace
  TailleSource := TFile.GetSize(Source);
  TailleCompresse := TFile.GetSize(Destination);

  if TailleCompresse >= TailleSource * 0.95 then
  begin
    // Compression inefficace (< 5%), utiliser le fichier original
    DeleteFile(Destination);
    TFile.Copy(Source, Destination, True);
    ShowMessage('Compression peu efficace, fichier copié tel quel');
  end;
end;
```

### 4. Afficher une barre de progression

```pascal
procedure CompresserAvecProgression(const Source, Destination: string;
                                    ProgressBar: TProgressBar);
const
  TAILLE_BLOC = 64 * 1024; // 64 Ko
var
  SourceStream, DestStream: TFileStream;
  Compresseur: TZCompressionStream;
  Buffer: array[0..TAILLE_BLOC-1] of Byte;
  BytesLus, TotalLus: Integer;
  TailleTotal: Int64;
begin
  SourceStream := TFileStream.Create(Source, fmOpenRead);
  try
    TailleTotal := SourceStream.Size;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;

    DestStream := TFileStream.Create(Destination, fmCreate);
    try
      Compresseur := TZCompressionStream.Create(DestStream, zcDefault);
      try
        TotalLus := 0;

        repeat
          BytesLus := SourceStream.Read(Buffer, TAILLE_BLOC);
          if BytesLus > 0 then
          begin
            Compresseur.WriteBuffer(Buffer, BytesLus);
            Inc(TotalLus, BytesLus);

            // Mettre à jour la barre de progression
            ProgressBar.Position := Round((TotalLus / TailleTotal) * 100);
            Application.ProcessMessages;
          end;
        until BytesLus = 0;
      finally
        Compresseur.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### 5. Gestion des erreurs robuste

```pascal
function CompresserSecurise(const Source, Destination: string): Boolean;  
begin  
  Result := False;

  // Vérifications préalables
  if not FileExists(Source) then
  begin
    ShowMessage('Fichier source introuvable');
    Exit;
  end;

  if FileExists(Destination) then
  begin
    if MessageDlg('Le fichier destination existe déjà. Écraser ?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;

  try
    CompresserFichier(Source, Destination);

    // Vérifier que le fichier a bien été créé
    if not FileExists(Destination) then
    begin
      ShowMessage('Erreur : fichier destination non créé');
      Exit;
    end;

    // Vérifier que le fichier n'est pas vide
    if TFile.GetSize(Destination) = 0 then
    begin
      ShowMessage('Erreur : fichier destination vide');
      DeleteFile(Destination);
      Exit;
    end;

    Result := True;
  except
    on E: EInOutError do
    begin
      ShowMessage('Erreur d''entrée/sortie : ' + E.Message);
      if FileExists(Destination) then
        DeleteFile(Destination);
    end;
    on E: EOutOfMemory do
    begin
      ShowMessage('Mémoire insuffisante pour la compression');
      if FileExists(Destination) then
        DeleteFile(Destination);
    end;
    on E: Exception do
    begin
      ShowMessage('Erreur inattendue : ' + E.Message);
      if FileExists(Destination) then
        DeleteFile(Destination);
    end;
  end;
end;
```

---

## Comparaison des méthodes

| Méthode | Usage | Avantages | Inconvénients |
|---------|-------|-----------|---------------|
| **ZLib (Streams)** | Données en mémoire, fichiers simples | Intégré, rapide, contrôle total | Format propriétaire, pas d'archives |
| **System.Zip** | Archives multi-fichiers | Standard ZIP, compatible, mot de passe | Plus lent, moins de contrôle |
| **Compression chaînes** | Données texte, communication | Simple pour petites données | Pas efficace pour gros volumes |

---

## Conseils pratiques

### Quand compresser ?

**OUI, compresser dans ces cas :**
- Fichiers texte (logs, configuration, code source)
- Sauvegardes
- Transferts réseau
- Stockage long terme
- Fichiers CSV, XML, JSON
- Bases de données texte

**NON, éviter la compression pour :**
- Fichiers déjà compressés (JPEG, MP3, ZIP, etc.)
- Très petits fichiers (< 1 Ko)
- Données nécessitant un accès aléatoire fréquent
- Applications temps réel critiques

### Taux de compression typiques

- **Code source** : 80-90%
- **Fichiers texte** : 60-80%
- **Images BMP** : 70-90%
- **Bases de données** : 50-70%
- **Fichiers exécutables** : 40-60%
- **Images JPEG** : 0-5% (déjà compressées)

---

## Résumé

Dans ce chapitre, vous avez découvert la compression et décompression en Delphi :

**Concepts clés :**
- Compression = réduire la taille des données
- Décompression = reconstituer les données originales
- Sans perte (lossless) vs avec perte (lossy)
- Taux de compression et niveaux

**Outils disponibles :**
- **System.ZLib** : compression de streams et fichiers
- **System.Zip** : création et manipulation d'archives ZIP
- Niveaux de compression (Fastest, Default, Max)
- Protection par mot de passe

**Techniques apprises :**
- Compresser des streams, fichiers et chaînes
- Créer et extraire des archives ZIP
- Gérer des sauvegardes compressées
- Optimiser selon le type de données

**Bonnes pratiques :**
- Choisir le bon niveau de compression
- Vérifier si la compression est utile
- Compresser par blocs les gros fichiers
- Gérer les erreurs robustement
- Afficher la progression pour les longs traitements

La compression est un outil puissant pour économiser de l'espace disque, accélérer les transferts et améliorer les performances de vos applications Delphi !

⏭️ [Traitement par lots (Batch)](/07-gestion-des-fichiers-et-flux-de-donnees/06-traitement-par-lots.md)
