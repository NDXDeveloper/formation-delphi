# 7. Gestion des fichiers et flux de données

## 7.3 Utilisation des TStream et classes dérivées

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les flux (streams) sont l'un des concepts les plus puissants et flexibles de Delphi pour la manipulation de données. Ils permettent de traiter uniformément diverses sources et destinations de données, comme les fichiers, la mémoire, ou même les connexions réseau.

### Introduction aux flux (TStream)

Un flux (`TStream`) est une abstraction qui représente une séquence d'octets, indépendamment de leur origine ou de leur destination. Il fournit une interface commune pour lire et écrire des données.

La classe `TStream` est la classe de base abstraite. Vous n'utilisez jamais directement une instance de `TStream`, mais plutôt une de ses classes dérivées qui implémentent des comportements spécifiques.

### Hiérarchie des classes TStream

Voici une vue simplifiée de la hiérarchie des classes de flux en Delphi :

```
TStream (classe abstraite)
  ├─ THandleStream
  │    ├─ TFileStream
  │    └─ TPipeStream
  ├─ TCustomMemoryStream
  │    ├─ TMemoryStream
  │    └─ TResourceStream
  ├─ TStringStream
  ├─ TBytesStream
  └─ TCustomStream
       └─ ... (autres classes dérivées spécialisées)
```

### Méthodes communes à tous les flux

Tous les flux partagent certaines méthodes et propriétés importantes :

| Méthode/Propriété | Description |
|-------------------|-------------|
| `Read` | Lit un nombre spécifié d'octets dans un tampon |
| `Write` | Écrit un nombre spécifié d'octets depuis un tampon |
| `ReadBuffer` | Lit exactement le nombre d'octets spécifié (lève une exception sinon) |
| `WriteBuffer` | Écrit exactement le nombre d'octets spécifié |
| `Seek` | Déplace la position dans le flux |
| `Position` | Position actuelle dans le flux (en octets depuis le début) |
| `Size` | Taille totale du flux en octets |
| `CopyFrom` | Copie des données depuis un autre flux |

### Les classes de flux les plus utilisées

#### TFileStream

Comme nous l'avons vu dans la section précédente, `TFileStream` permet d'accéder aux fichiers sur disque. Voici un rappel de son utilisation de base :

```pascal
var
  Flux: TFileStream;
begin
  // Créer un nouveau fichier ou écraser un fichier existant
  Flux := TFileStream.Create('monfichier.dat', fmCreate);
  try
    // Opérations sur le flux...
  finally
    Flux.Free;
  end;
end;
```

Les modes d'ouverture disponibles pour `TFileStream` sont :

| Mode | Description |
|------|-------------|
| `fmCreate` | Crée un nouveau fichier ou écrase un fichier existant |
| `fmOpenRead` | Ouvre un fichier existant en lecture seule |
| `fmOpenWrite` | Ouvre un fichier existant en écriture seule |
| `fmOpenReadWrite` | Ouvre un fichier existant en lecture et écriture |

Ces modes peuvent être combinés avec des options de partage :

| Option de partage | Description |
|-------------------|-------------|
| `fmShareExclusive` | Accès exclusif (aucun autre processus ne peut ouvrir le fichier) |
| `fmShareDenyWrite` | Empêche les autres processus d'ouvrir le fichier en écriture |
| `fmShareDenyRead` | Empêche les autres processus d'ouvrir le fichier en lecture |
| `fmShareDenyNone` | Permet aux autres processus d'ouvrir le fichier en lecture et écriture |

Exemple :
```pascal
// Ouvrir un fichier en lecture et autoriser d'autres processus à le lire
Flux := TFileStream.Create('monfichier.dat', fmOpenRead or fmShareDenyWrite);
```

#### TMemoryStream

`TMemoryStream` stocke les données en mémoire. C'est utile pour manipuler des données avant de les sauvegarder ou après les avoir lues :

```pascal
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    // Écrire des données dans le flux mémoire
    var Valeur: Integer := 12345;
    MemStream.WriteBuffer(Valeur, SizeOf(Integer));

    // Revenir au début du flux
    MemStream.Position := 0;

    // Lire les données
    var ValeurLue: Integer;
    MemStream.ReadBuffer(ValeurLue, SizeOf(Integer));

    ShowMessage(Format('Valeur lue: %d', [ValeurLue]));
  finally
    MemStream.Free;
  end;
end;
```

Méthodes spécifiques à `TMemoryStream` :

| Méthode | Description |
|---------|-------------|
| `Clear` | Vide le contenu du flux et réinitialise la position |
| `SetSize` | Définit la taille du flux (redimensionne le tampon mémoire) |
| `SaveToFile` | Sauvegarde le contenu du flux dans un fichier |
| `LoadFromFile` | Charge le contenu d'un fichier dans le flux |
| `Memory` | Retourne un pointeur vers le tampon mémoire interne |

#### TStringStream

`TStringStream` est spécialement conçu pour manipuler des chaînes de caractères dans un flux. Il gère automatiquement les encodages :

```pascal
uses
  System.Classes, System.SysUtils;

procedure ExempleStringStream;
var
  StringStream: TStringStream;
begin
  // Créer un flux de chaîne avec du contenu initial (UTF-8 par défaut)
  StringStream := TStringStream.Create('Bonjour, Delphi !');
  try
    // Ajouter du texte à la fin
    StringStream.WriteString(' Comment ça va ?');

    // Récupérer tout le contenu sous forme de chaîne
    ShowMessage('Contenu : ' + StringStream.DataString);

    // Réinitialiser la position pour relire depuis le début
    StringStream.Position := 0;

    // Lire les 8 premiers caractères
    var Debut := StringStream.ReadString(8);
    ShowMessage('Début : ' + Debut);
  finally
    StringStream.Free;
  end;
end;
```

`TStringStream` est particulièrement utile pour :
- Convertir des données binaires en texte encodé (Base64, Hex...)
- Manipuler du texte comme s'il s'agissait d'un fichier
- Analyser du texte avec des méthodes de flux

#### TBytesStream

`TBytesStream` est similaire à `TMemoryStream` mais travaille directement avec le type `TBytes` (tableau dynamique d'octets) :

```pascal
var
  BytesStream: TBytesStream;
  Donnees: TBytes;
begin
  // Créer un tableau d'octets
  SetLength(Donnees, 5);
  for var i := 0 to 4 do
    Donnees[i] := i * 10;

  // Créer un flux à partir du tableau d'octets
  BytesStream := TBytesStream.Create(Donnees);
  try
    // Ajouter plus de données
    var AutresDonnees: TBytes := [100, 101, 102];
    BytesStream.WriteBuffer(AutresDonnees, Length(AutresDonnees));

    // Obtenir le tableau d'octets complet
    var ResultatBytes := BytesStream.Bytes;

    // Afficher le contenu
    var Texte := '';
    for var i := 0 to Length(ResultatBytes) - 1 do
      Texte := Texte + ResultatBytes[i].ToString + ' ';

    ShowMessage('Contenu: ' + Texte);
  finally
    BytesStream.Free;
  end;
end;
```

#### TResourceStream

`TResourceStream` permet d'accéder aux ressources intégrées à l'exécutable. C'est utile pour inclure des données dans votre application sans nécessiter de fichiers externes :

```pascal
var
  ResStream: TResourceStream;
begin
  // Créer un flux à partir d'une ressource nommée 'MONIMAGE' de type 'JPEG'
  ResStream := TResourceStream.Create(HInstance, 'MONIMAGE', 'JPEG');
  try
    // Utiliser la ressource (par exemple, charger une image)
    Image1.Picture.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;
```

Pour inclure des ressources dans votre application, vous devez les définir dans un fichier `.rc` et le compiler avec votre projet.

### Opérations communes sur les flux

#### Positionnement dans un flux (Seek)

La méthode `Seek` permet de déplacer la position dans le flux :

```pascal
var
  Flux: TFileStream;
begin
  Flux := TFileStream.Create('donnees.bin', fmOpenRead);
  try
    // Aller à la position 100 depuis le début
    Flux.Seek(100, soBeginning);

    // Avancer de 50 octets depuis la position actuelle
    Flux.Seek(50, soCurrent);

    // Aller 20 octets avant la fin du fichier
    Flux.Seek(-20, soEnd);

    // Obtenir la position actuelle
    ShowMessage(Format('Position: %d', [Flux.Position]));
  finally
    Flux.Free;
  end;
end;
```

Les constantes pour l'origine du déplacement sont :
- `soBeginning` : depuis le début du flux
- `soCurrent` : depuis la position actuelle
- `soEnd` : depuis la fin du flux

#### Copie entre flux

La méthode `CopyFrom` permet de copier des données d'un flux à un autre :

```pascal
procedure CopierFichier(const SourceNom, DestinationNom: string);
var
  Source, Destination: TFileStream;
begin
  Source := TFileStream.Create(SourceNom, fmOpenRead or fmShareDenyWrite);
  try
    Destination := TFileStream.Create(DestinationNom, fmCreate);
    try
      // Copier tout le contenu de Source vers Destination
      Destination.CopyFrom(Source, 0); // 0 signifie tout copier
    finally
      Destination.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure CopierPartie(const SourceNom, DestinationNom: string;
  Debut, Taille: Int64);
var
  Source, Destination: TFileStream;
begin
  Source := TFileStream.Create(SourceNom, fmOpenRead);
  try
    // Positionner au point de départ
    Source.Position := Debut;

    Destination := TFileStream.Create(DestinationNom, fmCreate);
    try
      // Copier seulement 'Taille' octets
      Destination.CopyFrom(Source, Taille);
    finally
      Destination.Free;
    end;
  finally
    Source.Free;
  end;
end;
```

### Lecture et écriture de données typées

Pour faciliter la lecture et l'écriture de types de données spécifiques, on peut créer des méthodes d'extension :

```pascal
// Placer dans une unité d'utilitaires
unit StreamUtils;

interface

uses
  System.Classes, System.SysUtils;

type
  // Méthodes d'extension pour TStream
  TStreamHelper = class helper for TStream
  public
    // Lecture de types simples
    function ReadBoolean: Boolean;
    function ReadInteger: Integer;
    function ReadInt64: Int64;
    function ReadDouble: Double;
    function ReadString: string;

    // Écriture de types simples
    procedure WriteBoolean(Value: Boolean);
    procedure WriteInteger(Value: Integer);
    procedure WriteInt64(Value: Int64);
    procedure WriteDouble(Value: Double);
    procedure WriteString(const Value: string);
  end;

implementation

function TStreamHelper.ReadBoolean: Boolean;
begin
  ReadBuffer(Result, SizeOf(Boolean));
end;

function TStreamHelper.ReadInteger: Integer;
begin
  ReadBuffer(Result, SizeOf(Integer));
end;

function TStreamHelper.ReadInt64: Int64;
begin
  ReadBuffer(Result, SizeOf(Int64));
end;

function TStreamHelper.ReadDouble: Double;
begin
  ReadBuffer(Result, SizeOf(Double));
end;

function TStreamHelper.ReadString: string;
var
  Len: Integer;
  Bytes: TBytes;
begin
  // Lire la longueur de la chaîne
  ReadBuffer(Len, SizeOf(Integer));

  // Lire les octets de la chaîne
  if Len > 0 then
  begin
    SetLength(Bytes, Len);
    ReadBuffer(Bytes[0], Len);
    Result := TEncoding.UTF8.GetString(Bytes);
  end
  else
    Result := '';
end;

procedure TStreamHelper.WriteBoolean(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Boolean));
end;

procedure TStreamHelper.WriteInteger(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Integer));
end;

procedure TStreamHelper.WriteInt64(Value: Int64);
begin
  WriteBuffer(Value, SizeOf(Int64));
end;

procedure TStreamHelper.WriteDouble(Value: Double);
begin
  WriteBuffer(Value, SizeOf(Double));
end;

procedure TStreamHelper.WriteString(const Value: string);
var
  Bytes: TBytes;
  Len: Integer;
begin
  // Convertir la chaîne en octets
  Bytes := TEncoding.UTF8.GetBytes(Value);
  Len := Length(Bytes);

  // Écrire la longueur puis les octets
  WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    WriteBuffer(Bytes[0], Len);
end;

end.
```

Utilisation :

```pascal
uses
  StreamUtils;

procedure ExempleUtiliserHelper;
var
  Flux: TMemoryStream;
begin
  Flux := TMemoryStream.Create;
  try
    // Écrire des données typées
    Flux.WriteInteger(42);
    Flux.WriteString('Bonjour Delphi');
    Flux.WriteBoolean(True);
    Flux.WriteDouble(3.14159);

    // Repositionner au début
    Flux.Position := 0;

    // Lire les données
    var Entier := Flux.ReadInteger;
    var Texte := Flux.ReadString;
    var Bool := Flux.ReadBoolean;
    var Reel := Flux.ReadDouble;

    ShowMessage(Format('Entier: %d'#13#10 +
                       'Texte: %s'#13#10 +
                       'Booléen: %s'#13#10 +
                       'Réel: %f',
                       [Entier, Texte, BoolToStr(Bool, True), Reel]));
  finally
    Flux.Free;
  end;
end;
```

> **Note :** Le concept de classes "helper" nécessite Delphi 10.4 ou supérieur pour l'utilisation complète des fonctionnalités montrées ici.

### Flux adaptateurs

Delphi permet également de créer des flux adaptateurs qui transforment les données lors de la lecture ou de l'écriture.

#### TBufferedFileStream

Permet d'améliorer les performances en utilisant un tampon mémoire pour les opérations sur les fichiers :

```pascal
uses
  System.Classes, System.SysUtils, Vcl.Dialogs;

// Cette classe n'est pas fournie par Delphi, nous la créons
type
  TBufferedFileStream = class(TStream)
  private
    FFileStream: TFileStream;
    FBuffer: TMemoryStream;
    FBufferSize: Integer;
    FBufferDirty: Boolean;
    FBufferStart: Int64;
    procedure FlushBuffer;
    procedure FillBuffer(Position: Int64);
  public
    constructor Create(const AFileName: string; Mode: Word; BufferSize: Integer = 8192);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word; BufferSize: Integer);
begin
  inherited Create;
  FFileStream := TFileStream.Create(AFileName, Mode);
  FBuffer := TMemoryStream.Create;
  FBufferSize := BufferSize;
  FBuffer.Size := BufferSize;
  FBufferDirty := False;
  FBufferStart := -1; // Marque le tampon comme non rempli
end;

destructor TBufferedFileStream.Destroy;
begin
  if FBufferDirty then
    FlushBuffer;
  FBuffer.Free;
  FFileStream.Free;
  inherited;
end;

procedure TBufferedFileStream.FlushBuffer;
begin
  if FBufferDirty and (FBufferStart >= 0) then
  begin
    FFileStream.Position := FBufferStart;
    FFileStream.WriteBuffer(FBuffer.Memory^, FBuffer.Size);
    FBufferDirty := False;
  end;
end;

procedure TBufferedFileStream.FillBuffer(Position: Int64);
var
  BytesRead: Integer;
begin
  if FBufferDirty then
    FlushBuffer;

  FFileStream.Position := Position;
  FBuffer.Clear;
  FBufferStart := Position;

  // Remplir le tampon avec les données du fichier
  BytesRead := FFileStream.Read(FBuffer.Memory^, FBufferSize);
  FBuffer.Size := BytesRead;
  FBufferDirty := False;
end;

function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
  BufferPtr: PByte;
  BytesToRead, AvailableInBuffer, BufferOffset: Integer;
  CurrentPosition: Int64;
begin
  Result := 0;
  BufferPtr := @Buffer;
  BytesToRead := Count;
  CurrentPosition := Position;

  while BytesToRead > 0 do
  begin
    // Si la position courante est en dehors du tampon, le recharger
    if (CurrentPosition < FBufferStart) or
       (CurrentPosition >= FBufferStart + FBuffer.Size) then
    begin
      FillBuffer(CurrentPosition);
      if FBuffer.Size = 0 then // Fin de fichier atteinte
        Break;
    end;

    // Calculer le décalage dans le tampon et les octets disponibles
    BufferOffset := CurrentPosition - FBufferStart;
    AvailableInBuffer := FBuffer.Size - BufferOffset;

    if AvailableInBuffer <= 0 then
    begin
      FillBuffer(CurrentPosition);
      if FBuffer.Size = 0 then // Fin de fichier atteinte
        Break;
      BufferOffset := 0;
      AvailableInBuffer := FBuffer.Size;
    end;

    // Lire autant que possible du tampon
    if BytesToRead <= AvailableInBuffer then
    begin
      Move(PByte(FBuffer.Memory)[BufferOffset]^, BufferPtr^, BytesToRead);
      Inc(Result, BytesToRead);
      Inc(CurrentPosition, BytesToRead);
      BytesToRead := 0;
    end
    else
    begin
      Move(PByte(FBuffer.Memory)[BufferOffset]^, BufferPtr^, AvailableInBuffer);
      Inc(Result, AvailableInBuffer);
      Inc(BufferPtr, AvailableInBuffer);
      Inc(CurrentPosition, AvailableInBuffer);
      Dec(BytesToRead, AvailableInBuffer);
    end;
  end;

  Position := CurrentPosition;
end;

function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
  BufferPtr: PByte;
  BytesToWrite, SpaceInBuffer, BufferOffset: Integer;
  CurrentPosition: Int64;
begin
  Result := 0;
  BufferPtr := @Buffer;
  BytesToWrite := Count;
  CurrentPosition := Position;

  while BytesToWrite > 0 do
  begin
    // Si la position courante est en dehors du tampon, le recharger
    if (CurrentPosition < FBufferStart) or
       (CurrentPosition >= FBufferStart + FBufferSize) then
    begin
      FlushBuffer;
      FillBuffer(CurrentPosition);
      if FBuffer.Size < FBufferSize then
        FBuffer.Size := FBufferSize; // Préparer le tampon pour l'écriture
    end;

    // Calculer le décalage dans le tampon et l'espace disponible
    BufferOffset := CurrentPosition - FBufferStart;
    SpaceInBuffer := FBufferSize - BufferOffset;

    if SpaceInBuffer <= 0 then
    begin
      FlushBuffer;
      FillBuffer(CurrentPosition);
      BufferOffset := 0;
      SpaceInBuffer := FBufferSize;
    end;

    // Écrire autant que possible dans le tampon
    if BytesToWrite <= SpaceInBuffer then
    begin
      Move(BufferPtr^, PByte(FBuffer.Memory)[BufferOffset]^, BytesToWrite);
      Inc(Result, BytesToWrite);
      Inc(CurrentPosition, BytesToWrite);
      BytesToWrite := 0;
      FBufferDirty := True;
    end
    else
    begin
      Move(BufferPtr^, PByte(FBuffer.Memory)[BufferOffset]^, SpaceInBuffer);
      Inc(Result, SpaceInBuffer);
      Inc(BufferPtr, SpaceInBuffer);
      Inc(CurrentPosition, SpaceInBuffer);
      Dec(BytesToWrite, SpaceInBuffer);
      FBufferDirty := True;
      FlushBuffer;
    end;
  end;

  Position := CurrentPosition;
end;

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: Result := Offset;
    soCurrent: Result := Position + Offset;
    soEnd:
    begin
      FlushBuffer;
      Result := FFileStream.Seek(Offset, soEnd);
    end;
    else
      Result := Position;
  end;
end;
```

Utilisation :

```pascal
procedure TestBufferedFileStream;
var
  BufStream: TBufferedFileStream;
  StartTime: TDateTime;
  Data: array of Byte;
  i: Integer;
begin
  // Créer des données de test
  SetLength(Data, 10 * 1024 * 1024); // 10 Mo
  for i := 0 to Length(Data) - 1 do
    Data[i] := i mod 256;

  // Tester avec TBufferedFileStream
  StartTime := Now;
  BufStream := TBufferedFileStream.Create('test_buffered.dat', fmCreate, 64 * 1024);
  try
    BufStream.WriteBuffer(Data[0], Length(Data));
  finally
    BufStream.Free;
  end;
  ShowMessage(Format('Temps avec tampon: %.2f ms',
                     [(Now - StartTime) * 24 * 60 * 60 * 1000]));

  // On pourrait comparer avec TFileStream standard pour voir la différence
end;
```

> **Note :** Cette implémentation est simplifiée pour illustration. Dans un contexte de production, vous pourriez utiliser des bibliothèques existantes ou des implémentations plus robustes.

### Flux de compression et décompression

Delphi fournit des flux pour la compression et décompression dans l'unité `System.Zip` :

```pascal
uses
  System.Classes, System.Zip, System.SysUtils;

procedure CompresserFichier(const FichierSource, FichierDestination: string);
var
  SourceStream, DestStream: TFileStream;
  ZipStream: TZCompressionStream;
begin
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de compression (niveau de compression 9 = maximum)
      ZipStream := TZCompressionStream.Create(DestStream, zcMax);
      try
        // Copier les données sources dans le flux de compression
        ZipStream.CopyFrom(SourceStream, 0); // 0 = copier tout
      finally
        // Important: fermer le flux de compression avant le flux de destination
        ZipStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DecompresserFichier(const FichierCompresse, FichierDestination: string);
var
  SourceStream, DestStream: TFileStream;
  UnzipStream: TZDecompressionStream;
  Buffer: array[0..8191] of Byte;
  Count: Integer;
begin
  SourceStream := TFileStream.Create(FichierCompresse, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de décompression
      UnzipStream := TZDecompressionStream.Create(SourceStream);
      try
        // Lire par blocs et écrire dans le fichier de destination
        repeat
          Count := UnzipStream.Read(Buffer, SizeOf(Buffer));
          if Count > 0 then
            DestStream.WriteBuffer(Buffer, Count);
        until Count = 0;
      finally
        UnzipStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### Flux de chiffrement et déchiffrement

Bien que Delphi ne fournisse pas directement des flux de chiffrement, vous pouvez créer vos propres descendants de `TStream` pour chiffrer et déchiffrer des données :

```pascal
uses
  System.Classes, System.SysUtils, System.Hash;

// Exemple simplifié de flux de chiffrement avec XOR
type
  TXORCryptoStream = class(TStream)
  private
    FInnerStream: TStream;
    FKey: TBytes;
    FOwnsStream: Boolean;
    FPosition: Int64;
  public
    constructor Create(AStream: TStream; const AKey: string; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property InnerStream: TStream read FInnerStream;
  end;

constructor TXORCryptoStream.Create(AStream: TStream; const AKey: string; AOwnsStream: Boolean);
begin
  inherited Create;
  FInnerStream := AStream;
  FOwnsStream := AOwnsStream;
  FPosition := 0;

  // Créer une clé de chiffrement à partir de la chaîne
  FKey := THashMD5.GetHashBytes(AKey);
end;

destructor TXORCryptoStream.Destroy;
begin
  if FOwnsStream then
    FInnerStream.Free;
  inherited Destroy;
end;

function TXORCryptoStream.Read(var Buffer; Count: Longint): Longint;
var
  I: Integer;
  PBuf: PByte;
begin
  // Lire les données du flux sous-jacent
  Result := FInnerStream.Read(Buffer, Count);

  // Déchiffrer en appliquant XOR avec la clé
  PBuf := @Buffer;
  for I := 0 to Result - 1 do
  begin
    PBuf^ := PBuf^ xor FKey[(FPosition + I) mod Length(FKey)];
    Inc(PBuf);
  end;

  Inc(FPosition, Result);
end;

function TXORCryptoStream.Write(const Buffer; Count: Longint): Longint;
var
  I: Integer;
  TempBuffer: TBytes;
  PBuf: PByte;
begin
  // Copier les données pour ne pas modifier l'original
  SetLength(TempBuffer, Count);
  Move(Buffer, TempBuffer[0], Count);

  // Chiffrer en appliquant XOR avec la clé
  for I := 0 to Count - 1 do
    TempBuffer[I] := TempBuffer[I] xor FKey[(FPosition + I) mod Length(FKey)];

  // Écrire les données chiffrées dans le flux sous-jacent
  Result := FInnerStream.Write(TempBuffer[0], Count);

  Inc(FPosition, Result);
end;

function TXORCryptoStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FInnerStream.Seek(Offset, Origin);
  FPosition := Result;
end;
```

Utilisation :

```pascal
procedure ChiffrerFichier(const FichierSource, FichierDestination, MotDePasse: string);
var
  SourceStream, DestStream: TFileStream;
  CryptoStream: TXORCryptoStream;
begin
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de chiffrement
      CryptoStream := TXORCryptoStream.Create(DestStream, MotDePasse, False);
      try
        // Copier les données sources dans le flux de chiffrement
        CryptoStream.CopyFrom(SourceStream, 0);
      finally
        CryptoStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DechiffrerFichier(const FichierChiffre, FichierDestination, MotDePasse: string);
var
  SourceStream, DestStream: TFileStream;
  CryptoStream: TXORCryptoStream;
begin
  SourceStream := TFileStream.Create(FichierChiffre, fmOpenRead);
  try
    DestStream := TFileStream.Create(FichierDestination, fmCreate);
    try
      // Créer un flux de déchiffrement (même classe puisque XOR fonctionne dans les deux sens)
      CryptoStream := TXORCryptoStream.Create(SourceStream, MotDePasse, False);
      try
        // Copier les données chiffrées vers le fichier de destination
        DestStream.CopyFrom(CryptoStream, 0);
      finally
        CryptoStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

> **Note :** Cet exemple utilise un chiffrement XOR simple pour illustrer le concept. Pour une application réelle, utilisez des algorithmes de chiffrement plus robustes comme AES ou RSA, disponibles dans l'unité `System.Hash`.

### Utilisation des flux dans les composants visuels

De nombreux composants Delphi prennent en charge les flux via des méthodes `LoadFromStream` et `SaveToStream` :

```pascal
procedure ExempleFluxAvecComposants;
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    // Sauvegarder le contenu d'un mémo dans un flux
    Memo1.Lines.SaveToStream(MemStream);

    // Revenir au début du flux
    MemStream.Position := 0;

    // Charger le contenu dans un autre mémo
    Memo2.Lines.LoadFromStream(MemStream);

    // Utiliser le même flux pour une image
    MemStream.Clear;
    if Assigned(Image1.Picture.Graphic) then
      Image1.Picture.Graphic.SaveToStream(MemStream);

    // Revenir au début
    MemStream.Position := 0;

    // Charger l'image dans un autre composant
    Image2.Picture.LoadFromStream(MemStream);
  finally
    MemStream.Free;
  end;
end;
```

Composants qui prennent en charge les flux :
- `TMemo` et `TRichEdit` via `Lines.LoadFromStream` et `Lines.SaveToStream`
- `TImage` via `Picture.LoadFromStream` et `Picture.SaveToStream`
- `TListBox` et `TComboBox` via `Items.LoadFromStream` et `Items.SaveToStream`
- `TBitmap`, `TJPEGImage` et autres classes graphiques

### Flux pour les bases de données

Les champs mémo et BLOB (Binary Large OBject) des bases de données fonctionnent également avec les flux :

```pascal
procedure SauvegarderImageDansBlobField;
var
  MemStream: TMemoryStream;
begin
  if not DataModule1.TableClients.Active then
    DataModule1.TableClients.Open;

  // Passer en mode édition
  DataModule1.TableClients.Edit;

  MemStream := TMemoryStream.Create;
  try
    // Sauvegarder l'image dans le flux
    if Assigned(Image1.Picture.Graphic) then
      Image1.Picture.Graphic.SaveToStream(MemStream);

    // Revenir au début du flux
    MemStream.Position := 0;

    // Charger le flux dans le champ BLOB
    TBlobField(DataModule1.TableClients.FieldByName('Photo')).LoadFromStream(MemStream);

    // Valider les modifications
    DataModule1.TableClients.Post;
  finally
    MemStream.Free;
  end;
end;

procedure ChargerImageDepuisBlobField;
var
  MemStream: TMemoryStream;
begin
  if not DataModule1.TableClients.Active then
    DataModule1.TableClients.Open;

  MemStream := TMemoryStream.Create;
  try
    // Charger le champ BLOB dans le flux
    TBlobField(DataModule1.TableClients.FieldByName('Photo')).SaveToStream(MemStream);

    // Revenir au début du flux
    MemStream.Position := 0;

    // Si le BLOB n'est pas vide
    if MemStream.Size > 0 then
    begin
      // Essayer de charger l'image
      try
        Image1.Picture.LoadFromStream(MemStream);
      except
        on E: Exception do
          ShowMessage('Erreur lors du chargement de l''image : ' + E.Message);
      end;
    end;
  finally
    MemStream.Free;
  end;
end;
```

### Création de votre propre classe de flux

Dans certains cas, vous pourriez avoir besoin de créer votre propre classe de flux spécialisée. Voici un exemple de flux qui ne permet d'accéder qu'à une partie d'un autre flux :

```pascal
type
  TPartialStream = class(TStream)
  private
    FBaseStream: TStream;
    FStartPos: Int64;
    FSize: Int64;
    FPosition: Int64;
    FOwnsStream: Boolean;
  public
    // Crée un flux qui représente une partie d'un autre flux
    constructor Create(ABaseStream: TStream; AStartPos, ASize: Int64; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property BaseStream: TStream read FBaseStream;
    property StartPos: Int64 read FStartPos;
    property Size: Int64 read FSize;
  end;

constructor TPartialStream.Create(ABaseStream: TStream; AStartPos, ASize: Int64; AOwnsStream: Boolean);
begin
  inherited Create;
  FBaseStream := ABaseStream;
  FStartPos := AStartPos;
  FSize := ASize;
  FPosition := 0;
  FOwnsStream := AOwnsStream;

  // Vérifier les paramètres
  if FBaseStream = nil then
    raise EStreamError.Create('Flux de base non défini');

  if FStartPos < 0 then
    raise EStreamError.Create('Position de départ négative');

  if FSize < 0 then
    raise EStreamError.Create('Taille négative');

  // Vérifier que la portion demandée est dans les limites du flux de base
  if FStartPos + FSize > FBaseStream.Size then
    raise EStreamError.Create('La portion demandée dépasse la taille du flux de base');
end;

destructor TPartialStream.Destroy;
begin
  if FOwnsStream then
    FBaseStream.Free;
  inherited;
end;

function TPartialStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesLeft: Int64;
begin
  // Calculer combien d'octets peuvent être réellement lus
  BytesLeft := FSize - FPosition;
  if Count > BytesLeft then
    Count := BytesLeft;

  if Count <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Positionner le flux de base
  FBaseStream.Position := FStartPos + FPosition;

  // Lire les données
  Result := FBaseStream.Read(Buffer, Count);

  // Mettre à jour notre position
  Inc(FPosition, Result);
end;

function TPartialStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesLeft: Int64;
begin
  // Calculer combien d'octets peuvent être réellement écrits
  BytesLeft := FSize - FPosition;
  if Count > BytesLeft then
    Count := BytesLeft;

  if Count <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Positionner le flux de base
  FBaseStream.Position := FStartPos + FPosition;

  // Écrire les données
  Result := FBaseStream.Write(Buffer, Count);

  // Mettre à jour notre position
  Inc(FPosition, Result);
end;

function TPartialStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  NewPos: Int64;
begin
  // Calculer la nouvelle position en fonction de l'origine
  case Origin of
    soBeginning: NewPos := Offset;
    soCurrent:   NewPos := FPosition + Offset;
    soEnd:       NewPos := FSize + Offset;
    else
      raise EStreamError.Create('Origine de déplacement invalide');
  end;

  // Vérifier les limites
  if NewPos < 0 then
    NewPos := 0
  else if NewPos > FSize then
    NewPos := FSize;

  // Mettre à jour la position
  FPosition := NewPos;
  Result := FPosition;
end;
```

Utilisation :

```pascal
procedure ExtraireSectionFichier(const FichierSource, FichierDestination: string;
  Debut, Taille: Int64);
var
  SourceStream, DestStream: TFileStream;
  PartStream: TPartialStream;
begin
  SourceStream := TFileStream.Create(FichierSource, fmOpenRead);
  try
    // S'assurer que les paramètres sont valides
    if (Debut < 0) or (Debut >= SourceStream.Size) then
      raise Exception.Create('Position de départ invalide');

    if (Taille <= 0) or (Debut + Taille > SourceStream.Size) then
      Taille := SourceStream.Size - Debut;

    // Créer un flux partiel qui représente seulement la portion demandée
    PartStream := TPartialStream.Create(SourceStream, Debut, Taille);
    try
      // Créer le fichier de destination
      DestStream := TFileStream.Create(FichierDestination, fmCreate);
      try
        // Copier la portion du fichier source vers la destination
        DestStream.CopyFrom(PartStream, 0);
      finally
        DestStream.Free;
      end;
    finally
      PartStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### Flux pour les opérations réseau

Delphi fournit également des flux spécialisés pour les opérations réseau, comme `TSocketStream` et d'autres dans les composants Indy ou les clients REST.

### Techniques avancées avec les flux

#### Clonage et copie de flux

Créer une copie d'un flux peut être utile, notamment pour conserver une version intermédiaire des données :

```pascal
function CloneStream(Source: TStream): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Source.Position := 0;
    Result.CopyFrom(Source, 0);
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;
```

#### Traitement par blocs pour les grands fichiers

Pour les fichiers volumineux, il est préférable de traiter les données par blocs plutôt que de tout charger en mémoire :

```pascal
procedure TraiterGrandFichierParBlocs(const NomFichier: string);
const
  TAILLE_BLOC = 1024 * 1024; // 1 Mo par bloc
var
  Flux: TFileStream;
  Buffer: TBytes;
  BytesLus: Integer;
  TotalTraite: Int64;
begin
  SetLength(Buffer, TAILLE_BLOC);
  Flux := TFileStream.Create(NomFichier, fmOpenRead);
  try
    TotalTraite := 0;

    repeat
      // Lire un bloc
      BytesLus := Flux.Read(Buffer[0], TAILLE_BLOC);

      if BytesLus > 0 then
      begin
        // Traiter les données du bloc...
        // Par exemple, calculer une somme de contrôle

        Inc(TotalTraite, BytesLus);

        // Afficher la progression
        Label1.Caption := Format('Traité: %.2f Mo (%.1f%%)',
                                [TotalTraite / (1024 * 1024),
                                 (TotalTraite / Flux.Size) * 100]);
        Application.ProcessMessages; // Permettre la mise à jour de l'interface
      end;
    until BytesLus = 0;

    ShowMessage(Format('Traitement terminé: %.2f Mo', [TotalTraite / (1024 * 1024)]));
  finally
    Flux.Free;
  end;
end;
```

### Bonnes pratiques avec les flux

1. **Toujours libérer les ressources** : Utilisez `try...finally` pour vous assurer que les flux sont libérés même en cas d'exception.

2. **Gérer les erreurs** : Capturez les exceptions spécifiques comme `EStreamError` ou `EFOpenError` pour un traitement d'erreur précis.

3. **Flux imbriqués** : Libérez les flux dans l'ordre inverse de leur création (de l'intérieur vers l'extérieur).

4. **Position des flux** : N'oubliez pas de repositionner les flux au début après écriture si vous prévoyez de les lire.

5. **Performances** :
   - Utilisez `WriteBuffer`/`ReadBuffer` plutôt que `Write`/`Read` lorsque vous voulez garantir la lecture/écriture de tous les octets.
   - Utilisez un tampon de taille appropriée (ni trop petit, ni trop grand) pour les opérations par blocs.
   - Pour les grands fichiers, traitez par blocs plutôt que de tout charger en mémoire.

6. **Propriétaire des flux** : Définissez clairement qui est responsable de libérer les flux, surtout lorsque vous les passez entre différentes méthodes.

### Exemple de projet complet

Voici un exemple complet de gestionnaire de fichiers qui montre l'utilisation de plusieurs types de flux :

```pascal
unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, System.Zip, System.Hash;

type
  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MemoTexte: TMemo;
    ButtonCharger: TButton;
    ButtonSauvegarder: TButton;
    ButtonCompresser: TButton;
    ButtonDecompresser: TButton;
    Image1: TImage;
    ButtonChargerImage: TButton;
    ButtonSauvegarderImage: TButton;
    ButtonRotationImage: TButton;
    ProgressBar1: TProgressBar;
    MemoStats: TMemo;
    ButtonEncrypter: TButton;
    ButtonDecrypter: TButton;
    EditMotDePasse: TEdit;
    Label1: TLabel;
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonSauvegarderClick(Sender: TObject);
    procedure ButtonCompresserClick(Sender: TObject);
    procedure ButtonDecompresserClick(Sender: TObject);
    procedure ButtonChargerImageClick(Sender: TObject);
    procedure ButtonSauvegarderImageClick(Sender: TObject);
    procedure ButtonRotationImageClick(Sender: TObject);
    procedure ButtonEncrypterClick(Sender: TObject);
    procedure ButtonDecrypterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AfficheStats(const Fichier: string);
    function CreateZipStream(AStream: TStream): TStream;
    function CreateUnzipStream(AStream: TStream): TStream;
    function CreateEncryptStream(AStream: TStream; const Password: string): TStream;
    function CreateDecryptStream(AStream: TStream; const Password: string): TStream;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TFormMain.AfficheStats(const Fichier: string);
var
  Info: TSearchRec;
begin
  if FindFirst(Fichier, faAnyFile, Info) = 0 then
  begin
    MemoStats.Lines.Clear;
    MemoStats.Lines.Add('Nom: ' + ExtractFileName(Fichier));
    MemoStats.Lines.Add('Taille: ' + FormatFloat('#,##0', Info.Size) + ' octets');
    MemoStats.Lines.Add('Date: ' + DateTimeToStr(FileDateToDateTime(Info.Time)));
    FindClose(Info);
  end;
end;

procedure TFormMain.ButtonChargerClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    Stream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      MemoTexte.Lines.LoadFromStream(Stream);
      AfficheStats(OpenDialog1.FileName);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TFormMain.ButtonSauvegarderClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if SaveDialog1.Execute then
  begin
    Stream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      MemoTexte.Lines.SaveToStream(Stream);
      AfficheStats(SaveDialog1.FileName);
    finally
      Stream.Free;
    end;
  end;
end;

function TFormMain.CreateZipStream(AStream: TStream): TStream;
begin
  Result := TZCompressionStream.Create(TCompressionLevel.clMax, AStream);
end;

function TFormMain.CreateUnzipStream(AStream: TStream): TStream;
begin
  Result := TZDecompressionStream.Create(AStream);
end;

// Implémentation simplifiée pour l'exemple
function TFormMain.CreateEncryptStream(AStream: TStream; const Password: string): TStream;
begin
  // Dans une application réelle, utilisez un algorithme plus fort que XOR
  Result := TXORCryptoStream.Create(AStream, Password, False);
end;

function TFormMain.CreateDecryptStream(AStream: TStream; const Password: string): TStream;
begin
  // Même classe que pour le chiffrement car XOR est réversible
  Result := TXORCryptoStream.Create(AStream, Password, False);
end;

procedure TFormMain.ButtonCompresserClick(Sender: TObject);
var
  SourceStream, DestStream, ZipStream: TStream;
begin
  if OpenDialog1.Execute then
  begin
    if SaveDialog1.Execute then
    begin
      SourceStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
      try
        DestStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          ZipStream := CreateZipStream(DestStream);
          try
            ProgressBar1.Max := 100;
            ProgressBar1.Position := 0;

            // Copier le contenu
            ZipStream.CopyFrom(SourceStream, 0);
            ProgressBar1.Position := 100;

            AfficheStats(SaveDialog1.FileName);
            ShowMessage('Compression terminée !');
          finally
            ZipStream.Free;
          end;
        finally
          DestStream.Free;
        end;
      finally
        SourceStream.Free;
      end;
    end;
  end;
end;

procedure TFormMain.ButtonDecompresserClick(Sender: TObject);
var
  SourceStream, DestStream, UnzipStream: TStream;
  Buffer: array[0..8191] of Byte;
  Count: Integer;
  TotalRead: Int64;
  SourceSize: Int64;
begin
  if OpenDialog1.Execute then
  begin
    if SaveDialog1.Execute then
    begin
      SourceStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
      try
        SourceSize := SourceStream.Size;
        DestStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          UnzipStream := CreateUnzipStream(SourceStream);
          try
            ProgressBar1.Max := 100;
            ProgressBar1.Position := 0;
            TotalRead := 0;

            // Lire par blocs
            repeat
              Count := UnzipStream.Read(Buffer, SizeOf(Buffer));
              if Count > 0 then
              begin
                DestStream.WriteBuffer(Buffer, Count);
                Inc(TotalRead, Count);

                // Mise à jour de la progression (approximative)
                ProgressBar1.Position := Min(100, Round((TotalRead / (SourceSize * 2)) * 100));
                Application.ProcessMessages;
              end;
            until Count = 0;

            ProgressBar1.Position := 100;
            AfficheStats(SaveDialog1.FileName);
            ShowMessage('Décompression terminée !');
          finally
            UnzipStream.Free;
          end;
        finally
          DestStream.Free;
        end;
      finally
        SourceStream.Free;
      end;
    end;
  end;
end;

procedure TFormMain.ButtonChargerImageClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    Stream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      Image1.Picture.LoadFromStream(Stream);
      AfficheStats(OpenDialog1.FileName);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TFormMain.ButtonSauvegarderImageClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if SaveDialog1.Execute then
  begin
    Stream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      if Assigned(Image1.Picture.Graphic) then
        Image1.Picture.SaveToFile(SaveDialog1.FileName);

      AfficheStats(SaveDialog1.FileName);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TFormMain.ButtonRotationImageClick(Sender: TObject);
var
  Bitmap: TBitmap;
  MemStream: TMemoryStream;
  x, y: Integer;
  SrcPixel, DstPixel: PRGBQuad;
  SrcScanline, DstScanline: PByte;
  OrigWidth, OrigHeight: Integer;
begin
  if not Assigned(Image1.Picture.Graphic) then
    Exit;

  MemStream := TMemoryStream.Create;
  try
    // Sauvegarder l'image actuelle dans un flux mémoire
    Image1.Picture.Graphic.SaveToStream(MemStream);

    // Créer un nouveau bitmap
    Bitmap := TBitmap.Create;
    try
      // Charger l'image originale
      MemStream.Position := 0;
      Bitmap.LoadFromStream(MemStream);

      // Définir la profondeur de couleur à 32 bits
      Bitmap.PixelFormat := pf32bit;

      OrigWidth := Bitmap.Width;
      OrigHeight := Bitmap.Height;

      // Créer un nouveau bitmap pour la rotation
      var RotatedBmp := TBitmap.Create;
      try
        RotatedBmp.PixelFormat := pf32bit;
        RotatedBmp.SetSize(OrigHeight, OrigWidth); // Inverser dimensions

        // Rotation 90° dans le sens horaire
        for y := 0 to OrigHeight - 1 do
        begin
          SrcScanline := Bitmap.ScanLine[y];

          for x := 0 to OrigWidth - 1 do
          begin
            SrcPixel := PRGBQuad(SrcScanline + x * 4);
            DstScanline := RotatedBmp.ScanLine[OrigWidth - 1 - x];
            DstPixel := PRGBQuad(DstScanline + y * 4);

            // Copier les pixels
            DstPixel^ := SrcPixel^;
          end;
        end;

        // Appliquer l'image tournée
        Image1.Picture.Assign(RotatedBmp);
      finally
        RotatedBmp.Free;
      end;
    finally
      Bitmap.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TFormMain.ButtonEncrypterClick(Sender: TObject);
var
  SourceStream, DestStream, CryptoStream: TStream;
begin
  if EditMotDePasse.Text = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe');
    Exit;
  end;

  if OpenDialog1.Execute then
  begin
    if SaveDialog1.Execute then
    begin
      SourceStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
      try
        DestStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          CryptoStream := CreateEncryptStream(DestStream, EditMotDePasse.Text);
          try
            ProgressBar1.Max := 100;
            ProgressBar1.Position := 0;

            // Copier le contenu
            CryptoStream.CopyFrom(SourceStream, 0);
            ProgressBar1.Position := 100;

            AfficheStats(SaveDialog1.FileName);
            ShowMessage('Chiffrement terminé !');
          finally
            CryptoStream.Free;
          end;
        finally
          DestStream.Free;
        end;
      finally
        SourceStream.Free;
      end;
    end;
  end;
end;

procedure TFormMain.ButtonDecrypterClick(Sender: TObject);
var
  SourceStream, DestStream, CryptoStream: TStream;
begin
  if EditMotDePasse.Text = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe');
    Exit;
  end;

  if OpenDialog1.Execute then
  begin
    if SaveDialog1.Execute then
    begin
      SourceStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
      try
        DestStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          CryptoStream := CreateDecryptStream(SourceStream, EditMotDePasse.Text);
          try
            ProgressBar1.Max := 100;
            ProgressBar1.Position := 0;

            // Copier le contenu
            DestStream.CopyFrom(CryptoStream, 0);
            ProgressBar1.Position := 100;

            AfficheStats(SaveDialog1.FileName);
            ShowMessage('Déchiffrement terminé !');
          finally
            CryptoStream.Free;
          end;
        finally
          DestStream.Free;
        end;
      finally
        SourceStream.Free;
      end;
    end;
  end;
end;

end.
```

### Exercice pratique

Créez une application qui utilise différents types de flux pour :
1. Charger une image
2. Lui appliquer un filtre simple (comme la conversion en niveaux de gris)
3. La compresser
4. La sauvegarder dans un fichier chiffré
5. Puis être capable de faire l'opération inverse (déchiffrement, décompression, chargement)

Cet exercice vous permettra de combiner plusieurs types de flux et de voir comment ils peuvent être utilisés ensemble de manière pratique.

---

À suivre dans la prochaine section : **7.4 Sérialisation et persistance d'objets**

⏭️ [Sérialisation et persistance d'objets](/07-gestion-des-fichiers-et-flux-de-donnees/04-serialisation-et-persistance-dobjets.md)
