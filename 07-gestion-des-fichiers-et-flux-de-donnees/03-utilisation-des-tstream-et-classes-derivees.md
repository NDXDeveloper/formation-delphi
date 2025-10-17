🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.3 Utilisation des TStream et classes dérivées

## Introduction

Les streams (flux de données) constituent l'un des concepts les plus puissants et polyvalents de Delphi pour manipuler des données. Que vous travailliez avec des fichiers, de la mémoire, des chaînes de caractères ou des ressources, les streams offrent une interface unifiée et cohérente.

Dans ce chapitre, nous allons explorer en profondeur la classe `TStream` et toutes ses classes dérivées, en comprenant leur utilité et leurs cas d'usage spécifiques.

## Qu'est-ce qu'un Stream ?

Un stream (flux) est une abstraction qui représente une séquence d'octets. Pensez à un stream comme à un tuyau par lequel les données circulent. Vous pouvez :

- Lire des données depuis le stream
- Écrire des données dans le stream
- Vous déplacer à différentes positions dans le stream
- Copier des données d'un stream vers un autre

**Analogie simple :** Imaginez un stream comme une cassette VHS. Vous pouvez :
- Lire le contenu (lecture)
- Enregistrer dessus (écriture)
- Avancer ou reculer rapidement (déplacement)
- Copier sur une autre cassette (copie)

---

## La classe de base : TStream

`TStream` est une classe abstraite qui définit l'interface commune pour tous les types de streams. Elle se trouve dans l'unité `System.Classes`.

### Propriétés principales

```pascal
var
  Stream: TStream;
begin
  // Position actuelle dans le stream (en octets depuis le début)
  Stream.Position := 0;
  ShowMessage('Position : ' + IntToStr(Stream.Position));

  // Taille totale du stream en octets
  ShowMessage('Taille : ' + IntToStr(Stream.Size));
end;
```

### Méthodes essentielles

#### 1. Read - Lire des données

```pascal
function Read(var Buffer; Count: Longint): Longint;

// Exemple
var
  Stream: TMemoryStream;
  Nombre: Integer;
  BytesLus: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    // ... (remplir le stream)
    Stream.Position := 0;

    // Lire un entier
    BytesLus := Stream.Read(Nombre, SizeOf(Integer));
    ShowMessage('Nombre lu : ' + IntToStr(Nombre));
    ShowMessage('Octets lus : ' + IntToStr(BytesLus));
  finally
    Stream.Free;
  end;
end;
```

**Note :** `Read` retourne le nombre d'octets effectivement lus, qui peut être inférieur à ce qui a été demandé si on atteint la fin du stream.

#### 2. Write - Écrire des données

```pascal
function Write(const Buffer; Count: Longint): Longint;

// Exemple
var
  Stream: TMemoryStream;
  Nombre: Integer;
  BytesEcrits: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    Nombre := 42;
    BytesEcrits := Stream.Write(Nombre, SizeOf(Integer));
    ShowMessage('Octets écrits : ' + IntToStr(BytesEcrits));
  finally
    Stream.Free;
  end;
end;
```

#### 3. Seek - Se déplacer dans le stream

```pascal
function Seek(Offset: Longint; Origin: Word): Longint;

// Exemples
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create('data.bin', fmOpenRead);
  try
    // Aller au début
    Stream.Seek(0, soBeginning);
    // ou simplement : Stream.Position := 0;

    // Aller à la fin
    Stream.Seek(0, soEnd);

    // Avancer de 10 octets
    Stream.Seek(10, soCurrent);

    // Reculer de 5 octets
    Stream.Seek(-5, soCurrent);

    // Aller à l'octet 100
    Stream.Seek(100, soBeginning);
  finally
    Stream.Free;
  end;
end;
```

**Origines possibles :**
- `soBeginning` (ou `soFromBeginning`) : depuis le début
- `soCurrent` (ou `soFromCurrent`) : depuis la position actuelle
- `soEnd` (ou `soFromEnd`) : depuis la fin

#### 4. CopyFrom - Copier depuis un autre stream

```pascal
function CopyFrom(Source: TStream; Count: Int64): Int64;

// Exemple : copier un fichier vers un autre
var
  Source, Dest: TFileStream;
begin
  Source := TFileStream.Create('source.dat', fmOpenRead);
  try
    Dest := TFileStream.Create('destination.dat', fmCreate);
    try
      // Copier tout le contenu (0 = tout)
      Dest.CopyFrom(Source, 0);
      ShowMessage('Copie terminée');
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;
```

#### 5. ReadBuffer et WriteBuffer - Versions strictes

Ces méthodes lèvent une exception si le nombre d'octets demandé n'a pas pu être lu ou écrit.

```pascal
var
  Stream: TMemoryStream;
  Nombre: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    Nombre := 100;

    // WriteBuffer garantit que tous les octets sont écrits
    Stream.WriteBuffer(Nombre, SizeOf(Integer));

    Stream.Position := 0;

    // ReadBuffer garantit que tous les octets sont lus
    // (lève une exception sinon)
    Stream.ReadBuffer(Nombre, SizeOf(Integer));
  finally
    Stream.Free;
  end;
end;
```

---

## Hiérarchie des classes de Stream

```
TStream (classe abstraite)
├── THandleStream
│   └── TFileStream
├── TCustomMemoryStream
│   ├── TMemoryStream
│   ├── TBytesStream
│   └── TStringStream
├── TResourceStream
└── Autres classes dérivées
```

Explorons maintenant chaque classe en détail.

---

## TFileStream - Stream pour les fichiers

`TFileStream` permet de lire et écrire des fichiers comme des streams. Nous l'avons déjà rencontré dans le chapitre précédent, mais approfondissons son utilisation.

### Création et modes d'ouverture

```pascal
// Créer un nouveau fichier
Stream := TFileStream.Create('fichier.dat', fmCreate);

// Ouvrir en lecture seule
Stream := TFileStream.Create('fichier.dat', fmOpenRead);

// Ouvrir en écriture seule
Stream := TFileStream.Create('fichier.dat', fmOpenWrite);

// Ouvrir en lecture/écriture
Stream := TFileStream.Create('fichier.dat', fmOpenReadWrite);

// Créer ou ouvrir
Stream := TFileStream.Create('fichier.dat', fmOpenReadWrite or fmCreate);

// Avec partage
Stream := TFileStream.Create('fichier.dat',
                            fmOpenRead or fmShareDenyWrite);
```

### Exemple pratique : Log avec TFileStream

```pascal
procedure AjouterAuLog(const Message: string);
var
  Stream: TFileStream;
  Texte: UTF8String;
  NomFichier: string;
begin
  NomFichier := 'application.log';

  // Ouvrir ou créer le fichier
  if FileExists(NomFichier) then
    Stream := TFileStream.Create(NomFichier, fmOpenReadWrite or fmShareDenyWrite)
  else
    Stream := TFileStream.Create(NomFichier, fmCreate);

  try
    // Aller à la fin
    Stream.Seek(0, soEnd);

    // Préparer le message
    Texte := UTF8String(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
                        ' - ' + Message + #13#10);

    // Écrire
    Stream.WriteBuffer(Texte[1], Length(Texte));
  finally
    Stream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  AjouterAuLog('Application démarrée');
  AjouterAuLog('Utilisateur a cliqué sur le bouton');
end;
```

---

## TMemoryStream - Stream en mémoire

`TMemoryStream` stocke les données en mémoire RAM. C'est extrêmement rapide mais limité par la mémoire disponible.

### Utilisation de base

```pascal
var
  MemStream: TMemoryStream;
  Nombre: Integer;
  Texte: AnsiString;
begin
  MemStream := TMemoryStream.Create;
  try
    // Écrire différents types de données
    Nombre := 42;
    MemStream.Write(Nombre, SizeOf(Integer));

    Nombre := 100;
    MemStream.Write(Nombre, SizeOf(Integer));

    // Revenir au début
    MemStream.Position := 0;

    // Relire
    MemStream.Read(Nombre, SizeOf(Integer));
    ShowMessage('Premier nombre : ' + IntToStr(Nombre));

    MemStream.Read(Nombre, SizeOf(Integer));
    ShowMessage('Deuxième nombre : ' + IntToStr(Nombre));
  finally
    MemStream.Free;
  end;
end;
```

### Charger et sauvegarder depuis/vers un fichier

```pascal
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    // Charger un fichier en mémoire
    MemStream.LoadFromFile('données.dat');

    // Manipuler les données en mémoire
    // ... votre code ...

    // Sauvegarder en fichier
    MemStream.SaveToFile('données_modifiées.dat');
  finally
    MemStream.Free;
  end;
end;
```

### SetSize - Préallouer de la mémoire

Pour optimiser les performances lors d'écritures multiples :

```pascal
var
  MemStream: TMemoryStream;
  i: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    // Préallouer 1 Mo
    MemStream.SetSize(1024 * 1024);

    // Réinitialiser la position
    MemStream.Position := 0;

    // Écrire des données
    for i := 1 to 1000 do
      MemStream.Write(i, SizeOf(Integer));

    // Ajuster à la taille réelle
    MemStream.SetSize(MemStream.Position);
  finally
    MemStream.Free;
  end;
end;
```

### Accès direct à la mémoire

```pascal
var
  MemStream: TMemoryStream;
  Pointeur: PByte;
begin
  MemStream := TMemoryStream.Create;
  try
    // ... remplir le stream ...

    // Accéder directement à la mémoire
    Pointeur := MemStream.Memory;

    // Lire le premier octet
    ShowMessage('Premier octet : ' + IntToStr(Pointeur^));
  finally
    MemStream.Free;
  end;
end;
```

### Exemple pratique : Buffer de données

```pascal
type
  TDataBuffer = class
  private
    FStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterEntier(Valeur: Integer);
    procedure AjouterReel(Valeur: Double);
    procedure AjouterTexte(const Texte: string);

    procedure Effacer;
    procedure SauvegarderDans(const NomFichier: string);
    function ObtenirTaille: Int64;
  end;

constructor TDataBuffer.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TDataBuffer.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TDataBuffer.AjouterEntier(Valeur: Integer);
begin
  FStream.Write(Valeur, SizeOf(Integer));
end;

procedure TDataBuffer.AjouterReel(Valeur: Double);
begin
  FStream.Write(Valeur, SizeOf(Double));
end;

procedure TDataBuffer.AjouterTexte(const Texte: string);
var
  Bytes: TBytes;
  Longueur: Integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(Texte);
  Longueur := Length(Bytes);

  // Écrire la longueur puis les données
  FStream.Write(Longueur, SizeOf(Integer));
  if Longueur > 0 then
    FStream.Write(Bytes[0], Longueur);
end;

procedure TDataBuffer.Effacer;
begin
  FStream.Clear;
  FStream.Position := 0;
end;

procedure TDataBuffer.SauvegarderDans(const NomFichier: string);
begin
  FStream.SaveToFile(NomFichier);
end;

function TDataBuffer.ObtenirTaille: Int64;
begin
  Result := FStream.Size;
end;

// Utilisation
var
  Buffer: TDataBuffer;
begin
  Buffer := TDataBuffer.Create;
  try
    Buffer.AjouterEntier(42);
    Buffer.AjouterReel(3.14);
    Buffer.AjouterTexte('Bonjour Delphi');

    Buffer.SauvegarderDans('données.bin');
    ShowMessage('Taille : ' + IntToStr(Buffer.ObtenirTaille) + ' octets');
  finally
    Buffer.Free;
  end;
end;
```

---

## TStringStream - Stream pour les chaînes

`TStringStream` permet de traiter une chaîne de caractères comme un stream. Très utile pour la sérialisation et la manipulation de texte.

### Utilisation de base

```pascal
uses
  System.Classes, System.SysUtils;

var
  StringStream: TStringStream;
begin
  // Créer un stream vide
  StringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Écrire du texte
    StringStream.WriteString('Bonjour ');
    StringStream.WriteString('le monde !');

    // Récupérer le contenu
    ShowMessage(StringStream.DataString);

    // Résultat : "Bonjour le monde !"
  finally
    StringStream.Free;
  end;
end;
```

### Créer depuis une chaîne existante

```pascal
var
  StringStream: TStringStream;
  Ligne: string;
begin
  StringStream := TStringStream.Create('Ligne 1'#13#10'Ligne 2'#13#10'Ligne 3',
                                       TEncoding.UTF8);
  try
    // Lire ligne par ligne
    while StringStream.Position < StringStream.Size do
    begin
      Ligne := '';
      // Lire caractère par caractère jusqu'à la fin de ligne
      // (exemple simplifié)
    end;
  finally
    StringStream.Free;
  end;
end;
```

### Exemple : Génération de CSV

```pascal
function GenererCSV(const Donnees: array of array of string): string;
var
  StringStream: TStringStream;
  i, j: Integer;
begin
  StringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    for i := Low(Donnees) to High(Donnees) do
    begin
      for j := Low(Donnees[i]) to High(Donnees[i]) do
      begin
        StringStream.WriteString(Donnees[i][j]);

        if j < High(Donnees[i]) then
          StringStream.WriteString(';')
        else
          StringStream.WriteString(#13#10);
      end;
    end;

    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

// Utilisation
var
  CSV: string;
  Donnees: array[0..2] of array[0..2] of string;
begin
  Donnees[0][0] := 'Nom'; Donnees[0][1] := 'Prénom'; Donnees[0][2] := 'Âge';
  Donnees[1][0] := 'Dupont'; Donnees[1][1] := 'Jean'; Donnees[1][2] := '30';
  Donnees[2][0] := 'Martin'; Donnees[2][1] := 'Marie'; Donnees[2][2] := '25';

  CSV := GenererCSV(Donnees);
  Memo1.Text := CSV;
end;
```

### Sauvegarder et charger

```pascal
var
  StringStream: TStringStream;
begin
  // Créer et sauvegarder
  StringStream := TStringStream.Create('Mon contenu texte', TEncoding.UTF8);
  try
    StringStream.SaveToFile('texte.txt');
  finally
    StringStream.Free;
  end;

  // Charger
  StringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    StringStream.LoadFromFile('texte.txt');
    ShowMessage(StringStream.DataString);
  finally
    StringStream.Free;
  end;
end;
```

---

## TBytesStream - Stream pour les tableaux d'octets

`TBytesStream` travaille avec des tableaux dynamiques d'octets (`TBytes`).

### Utilisation de base

```pascal
var
  BytesStream: TBytesStream;
  Bytes: TBytes;
begin
  // Créer depuis un tableau d'octets
  SetLength(Bytes, 5);
  Bytes[0] := 10;
  Bytes[1] := 20;
  Bytes[2] := 30;
  Bytes[3] := 40;
  Bytes[4] := 50;

  BytesStream := TBytesStream.Create(Bytes);
  try
    ShowMessage('Taille : ' + IntToStr(BytesStream.Size));

    // Accéder aux octets
    ShowMessage('Premier octet : ' + IntToStr(BytesStream.Bytes[0]));
  finally
    BytesStream.Free;
  end;
end;
```

### Exemple : Manipulation d'images

```pascal
procedure ChargerEtModifierImage;
var
  BytesStream: TBytesStream;
  Bytes: TBytes;
  Image: TJPEGImage;
begin
  // Charger une image dans un tableau d'octets
  Bytes := TFile.ReadAllBytes('photo.jpg');

  BytesStream := TBytesStream.Create(Bytes);
  try
    // Charger dans un composant image
    Image := TJPEGImage.Create;
    try
      Image.LoadFromStream(BytesStream);

      // Afficher
      Form1.Image1.Picture.Assign(Image);
    finally
      Image.Free;
    end;
  finally
    BytesStream.Free;
  end;
end;
```

### Convertir entre différents streams

```pascal
procedure ConvertirFileStreamEnBytes;
var
  FileStream: TFileStream;
  BytesStream: TBytesStream;
  Bytes: TBytes;
begin
  FileStream := TFileStream.Create('données.dat', fmOpenRead);
  try
    // Créer un tableau d'octets de la bonne taille
    SetLength(Bytes, FileStream.Size);

    // Lire tout le contenu
    FileStream.Read(Bytes[0], FileStream.Size);

    // Créer un BytesStream
    BytesStream := TBytesStream.Create(Bytes);
    try
      // Manipuler les données
      ShowMessage('Converti : ' + IntToStr(BytesStream.Size) + ' octets');
    finally
      BytesStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

---

## TResourceStream - Stream pour les ressources

`TResourceStream` permet d'accéder aux ressources incorporées dans l'exécutable.

### Incorporer une ressource

D'abord, créez un fichier de ressources (`.rc`) :

```
// fichier resources.rc
MYDATA RCDATA "donnees.bin"
MYTEXT RCDATA "texte.txt"
MYICON ICON "icone.ico"
```

Compilez-le avec :
```
brcc32 resources.rc
```

Puis incluez-le dans votre projet :
```pascal
{$R resources.res}
```

### Lire une ressource

```pascal
procedure LireRessource;
var
  ResStream: TResourceStream;
  Texte: TStringList;
begin
  // Charger la ressource 'MYTEXT' de type 'RCDATA'
  ResStream := TResourceStream.Create(HInstance, 'MYTEXT', RT_RCDATA);
  try
    Texte := TStringList.Create;
    try
      Texte.LoadFromStream(ResStream);
      Memo1.Lines.Text := Texte.Text;
    finally
      Texte.Free;
    end;
  finally
    ResStream.Free;
  end;
end;
```

### Charger une image depuis les ressources

```pascal
procedure ChargerImageRessource;
var
  ResStream: TResourceStream;
  Image: TJPEGImage;
begin
  ResStream := TResourceStream.Create(HInstance, 'MYPHOTO', RT_RCDATA);
  try
    Image := TJPEGImage.Create;
    try
      Image.LoadFromStream(ResStream);
      Form1.Image1.Picture.Assign(Image);
    finally
      Image.Free;
    end;
  finally
    ResStream.Free;
  end;
end;
```

### Vérifier l'existence d'une ressource

```pascal
function RessourceExiste(const NomRessource: string): Boolean;
var
  ResStream: TResourceStream;
begin
  Result := False;
  try
    ResStream := TResourceStream.Create(HInstance, NomRessource, RT_RCDATA);
    try
      Result := True;
    finally
      ResStream.Free;
    end;
  except
    Result := False;
  end;
end;
```

---

## Opérations avancées avec les Streams

### 1. Copie partielle entre streams

```pascal
procedure CopierPartie(Source, Dest: TStream; Debut, Taille: Int64);
begin
  // Se positionner au début de la partie à copier
  Source.Position := Debut;

  // Copier la taille demandée
  Dest.CopyFrom(Source, Taille);
end;

// Exemple : copier les 1000 premiers octets
var
  Source, Dest: TFileStream;
begin
  Source := TFileStream.Create('source.dat', fmOpenRead);
  try
    Dest := TFileStream.Create('extrait.dat', fmCreate);
    try
      CopierPartie(Source, Dest, 0, 1000);
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;
```

### 2. Lecture ligne par ligne d'un stream texte

```pascal
function LireLigne(Stream: TStream): string;
var
  C: AnsiChar;
  Ligne: AnsiString;
begin
  Ligne := '';

  while Stream.Position < Stream.Size do
  begin
    Stream.Read(C, 1);

    if C = #13 then
    begin
      // Lire aussi le #10 si présent
      if Stream.Position < Stream.Size then
      begin
        Stream.Read(C, 1);
        if C <> #10 then
          Stream.Position := Stream.Position - 1;
      end;
      Break;
    end
    else if C = #10 then
      Break
    else
      Ligne := Ligne + C;
  end;

  Result := string(Ligne);
end;

// Utilisation
var
  FileStream: TFileStream;
  Ligne: string;
begin
  FileStream := TFileStream.Create('texte.txt', fmOpenRead);
  try
    while FileStream.Position < FileStream.Size do
    begin
      Ligne := LireLigne(FileStream);
      Memo1.Lines.Add(Ligne);
    end;
  finally
    FileStream.Free;
  end;
end;
```

### 3. Compression de stream (avec ZLib)

```pascal
uses
  System.ZLib;

procedure CompresserStream(Source, Dest: TStream);
var
  Compressor: TZCompressionStream;
begin
  Source.Position := 0;
  Dest.Size := 0;

  Compressor := TZCompressionStream.Create(Dest);
  try
    Compressor.CopyFrom(Source, 0);
  finally
    Compressor.Free;
  end;
end;

procedure DecompresserStream(Source, Dest: TStream);
var
  Decompressor: TZDecompressionStream;
begin
  Source.Position := 0;
  Dest.Size := 0;

  Decompressor := TZDecompressionStream.Create(Source);
  try
    Dest.CopyFrom(Decompressor, 0);
  finally
    Decompressor.Free;
  end;
end;

// Exemple d'utilisation
var
  Original, Compresse, Decompresse: TMemoryStream;
  i: Integer;
  TauxCompression: Double;
begin
  Original := TMemoryStream.Create;
  Compresse := TMemoryStream.Create;
  Decompresse := TMemoryStream.Create;
  try
    // Créer des données à comprimer
    for i := 1 to 10000 do
      Original.Write(i, SizeOf(Integer));

    // Comprimer
    CompresserStream(Original, Compresse);

    TauxCompression := (Compresse.Size / Original.Size) * 100;
    ShowMessage(Format('Original : %d octets' + #13#10 +
                       'Compressé : %d octets' + #13#10 +
                       'Taux : %.2f%%',
                       [Original.Size, Compresse.Size, TauxCompression]));

    // Décompresser
    DecompresserStream(Compresse, Decompresse);

    ShowMessage('Décompressé : ' + IntToStr(Decompresse.Size) + ' octets');
  finally
    Original.Free;
    Compresse.Free;
    Decompresse.Free;
  end;
end;
```

### 4. Chiffrement simple de stream

```pascal
// ATTENTION : Ceci est un exemple pédagogique simple
// Pour du chiffrement réel, utilisez des bibliothèques cryptographiques
procedure ChiffrerStreamSimple(Stream: TStream; const Cle: Byte);
var
  Buffer: array[0..4095] of Byte;
  BytesLus, i: Integer;
  Position: Int64;
begin
  Position := Stream.Position;
  Stream.Position := 0;

  repeat
    BytesLus := Stream.Read(Buffer, SizeOf(Buffer));

    if BytesLus > 0 then
    begin
      // XOR simple avec la clé
      for i := 0 to BytesLus - 1 do
        Buffer[i] := Buffer[i] xor Cle;

      Stream.Position := Stream.Position - BytesLus;
      Stream.Write(Buffer, BytesLus);
    end;
  until BytesLus = 0;

  Stream.Position := Position;
end;

// Utilisation (chiffrer et déchiffrer utilisent la même fonction avec XOR)
var
  MemStream: TMemoryStream;
  Texte: AnsiString;
begin
  MemStream := TMemoryStream.Create;
  try
    Texte := 'Message secret';
    MemStream.Write(Texte[1], Length(Texte));

    // Chiffrer
    ChiffrerStreamSimple(MemStream, 42);
    MemStream.SaveToFile('chiffre.dat');

    // Déchiffrer
    MemStream.Position := 0;
    ChiffrerStreamSimple(MemStream, 42);

    MemStream.Position := 0;
    SetLength(Texte, MemStream.Size);
    MemStream.Read(Texte[1], MemStream.Size);
    ShowMessage(string(Texte));
  finally
    MemStream.Free;
  end;
end;
```

---

## Création d'une classe de Stream personnalisée

Vous pouvez créer vos propres classes dérivées de TStream pour des besoins spécifiques.

### Exemple : Stream avec compteur de lectures/écritures

```pascal
type
  TCountingStream = class(TStream)
  private
    FStream: TStream;
    FBytesLus: Int64;
    FBytesEcrits: Int64;
    FNombreLectures: Integer;
    FNombreEcritures: Integer;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property BytesLus: Int64 read FBytesLus;
    property BytesEcrits: Int64 read FBytesEcrits;
    property NombreLectures: Integer read FNombreLectures;
    property NombreEcritures: Integer read FNombreEcritures;
  end;

constructor TCountingStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBytesLus := 0;
  FBytesEcrits := 0;
  FNombreLectures := 0;
  FNombreEcritures := 0;
end;

destructor TCountingStream.Destroy;
begin
  // Ne pas libérer FStream, on ne le possède pas
  inherited;
end;

function TCountingStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
  Inc(FBytesLus, Result);
  Inc(FNombreLectures);
end;

function TCountingStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
  Inc(FBytesEcrits, Result);
  Inc(FNombreEcritures);
end;

function TCountingStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TCountingStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TCountingStream.SetSize(NewSize: Longint);
begin
  FStream.Size := NewSize;
end;

// Utilisation
var
  FileStream: TFileStream;
  CountingStream: TCountingStream;
  Nombre: Integer;
  i: Integer;
begin
  FileStream := TFileStream.Create('test.dat', fmCreate);
  try
    CountingStream := TCountingStream.Create(FileStream);
    try
      // Écrire des données
      for i := 1 to 100 do
        CountingStream.Write(i, SizeOf(Integer));

      // Afficher les statistiques
      ShowMessage(Format('Écritures : %d' + #13#10 +
                         'Octets écrits : %d',
                         [CountingStream.NombreEcritures,
                          CountingStream.BytesEcrits]));

      // Lire les données
      CountingStream.Position := 0;
      for i := 1 to 100 do
        CountingStream.Read(Nombre, SizeOf(Integer));

      // Afficher les statistiques
      ShowMessage(Format('Lectures : %d' + #13#10 +
                         'Octets lus : %d',
                         [CountingStream.NombreLectures,
                          CountingStream.BytesLus]));
    finally
      CountingStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

---

## Patterns et bonnes pratiques

### 1. Pattern de création sécurisée

```pascal
function CreerEtInitialiserStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    // Initialisation
    Result.SetSize(1024);
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;
```

### 2. Stream Wrapper pour gestion automatique

```pascal
type
  TStreamHelper = class helper for TStream
    procedure WriteInteger(Value: Integer);
    function ReadInteger: Integer;
    procedure WriteString(const Value: string);
    function ReadString: string;
  end;

procedure TStreamHelper.WriteInteger(Value: Integer);
begin
  Self.WriteBuffer(Value, SizeOf(Integer));
end;

function TStreamHelper.ReadInteger: Integer;
begin
  Self.ReadBuffer(Result, SizeOf(Integer));
end;

procedure TStreamHelper.WriteString(const Value: string);
var
  Bytes: TBytes;
  Longueur: Integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(Value);
  Longueur := Length(Bytes);
  Self.WriteBuffer(Longueur, SizeOf(Integer));
  if Longueur > 0 then
    Self.WriteBuffer(Bytes[0], Longueur);
end;

function TStreamHelper.ReadString: string;
var
  Bytes: TBytes;
  Longueur: Integer;
begin
  Self.ReadBuffer(Longueur, SizeOf(Integer));
  if Longueur > 0 then
  begin
    SetLength(Bytes, Longueur);
    Self.ReadBuffer(Bytes[0], Longueur);
    Result := TEncoding.UTF8.GetString(Bytes);
  end
  else
    Result := '';
end;

// Utilisation simplifiée
var
  Stream: TMemoryStream;
  Texte: string;
  Nombre: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteInteger(42);
    Stream.WriteString('Bonjour');

    Stream.Position := 0;

    Nombre := Stream.ReadInteger;
    Texte := Stream.ReadString;

    ShowMessage(Format('Nombre : %d, Texte : %s', [Nombre, Texte]));
  finally
    Stream.Free;
  end;
end;
```

### 3. Utiliser des interfaces pour la gestion automatique

```pascal
type
  IStreamWrapper = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetStream: TStream;
    property Stream: TStream read GetStream;
  end;

  TStreamWrapper = class(TInterfacedObject, IStreamWrapper)
  private
    FStream: TStream;
    function GetStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    property Stream: TStream read GetStream;
  end;

constructor TStreamWrapper.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TStreamWrapper.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TStreamWrapper.GetStream: TStream;
begin
  Result := FStream;
end;

// Utilisation (pas besoin de try/finally)
procedure UtiliserStreamWrapper;
var
  StreamWrapper: IStreamWrapper;
  Stream: TStream;
begin
  StreamWrapper := TStreamWrapper.Create(TMemoryStream.Create);
  Stream := StreamWrapper.Stream;

  // Utiliser le stream
  Stream.Write(...);

  // Pas besoin de Free, l'interface s'en charge automatiquement
end;
```

---

## Cas d'usage pratiques

### 1. Sérialisation d'objets

```pascal
type
  TPerson = class
  private
    FNom: string;
    FAge: Integer;
    FSalaire: Double;
  public
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Salaire: Double read FSalaire write FSalaire;
  end;

procedure TPerson.SaveToStream(Stream: TStream);
var
  Bytes: TBytes;
  Longueur: Integer;
begin
  // Sauvegarder le nom
  Bytes := TEncoding.UTF8.GetBytes(FNom);
  Longueur := Length(Bytes);
  Stream.Write(Longueur, SizeOf(Integer));
  if Longueur > 0 then
    Stream.Write(Bytes[0], Longueur);

  // Sauvegarder l'âge
  Stream.Write(FAge, SizeOf(Integer));

  // Sauvegarder le salaire
  Stream.Write(FSalaire, SizeOf(Double));
end;

procedure TPerson.LoadFromStream(Stream: TStream);
var
  Bytes: TBytes;
  Longueur: Integer;
begin
  // Charger le nom
  Stream.Read(Longueur, SizeOf(Integer));
  if Longueur > 0 then
  begin
    SetLength(Bytes, Longueur);
    Stream.Read(Bytes[0], Longueur);
    FNom := TEncoding.UTF8.GetString(Bytes);
  end;

  // Charger l'âge
  Stream.Read(FAge, SizeOf(Integer));

  // Charger le salaire
  Stream.Read(FSalaire, SizeOf(Double));
end;

// Utilisation
var
  Person: TPerson;
  FileStream: TFileStream;
begin
  Person := TPerson.Create;
  try
    Person.Nom := 'Jean Dupont';
    Person.Age := 30;
    Person.Salaire := 45000.0;

    // Sauvegarder
    FileStream := TFileStream.Create('personne.dat', fmCreate);
    try
      Person.SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;

    // Réinitialiser
    Person.Nom := '';
    Person.Age := 0;
    Person.Salaire := 0;

    // Charger
    FileStream := TFileStream.Create('personne.dat', fmOpenRead);
    try
      Person.LoadFromStream(FileStream);
      ShowMessage(Format('%s, %d ans, %.2f €',
        [Person.Nom, Person.Age, Person.Salaire]));
    finally
      FileStream.Free;
    end;
  finally
    Person.Free;
  end;
end;
```

### 2. Communication réseau avec TMemoryStream

```pascal
// Préparer des données à envoyer
function PreparerMessage(const Commande: string; const Donnees: TBytes): TBytes;
var
  Stream: TMemoryStream;
  Longueur: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    // En-tête : longueur de la commande
    Longueur := Length(Commande);
    Stream.Write(Longueur, SizeOf(Integer));

    // Commande
    if Longueur > 0 then
      Stream.Write(Commande[1], Longueur * SizeOf(Char));

    // Données
    Longueur := Length(Donnees);
    Stream.Write(Longueur, SizeOf(Integer));
    if Longueur > 0 then
      Stream.Write(Donnees[0], Longueur);

    // Convertir en TBytes
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;
```

### 3. Cache avec TMemoryStream

```pascal
type
  TDataCache = class
  private
    FCache: TDictionary<string, TMemoryStream>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ajouter(const Cle: string; const Donnees: TBytes);
    function Obtenir(const Cle: string; out Donnees: TBytes): Boolean;
    procedure Effacer(const Cle: string);
    procedure EffacerTout;
  end;

constructor TDataCache.Create;
begin
  inherited;
  FCache := TDictionary<string, TMemoryStream>.Create;
end;

destructor TDataCache.Destroy;
begin
  EffacerTout;
  FCache.Free;
  inherited;
end;

procedure TDataCache.Ajouter(const Cle: string; const Donnees: TBytes);
var
  Stream: TMemoryStream;
begin
  // Supprimer l'ancienne entrée si elle existe
  if FCache.ContainsKey(Cle) then
    Effacer(Cle);

  Stream := TMemoryStream.Create;
  if Length(Donnees) > 0 then
    Stream.Write(Donnees[0], Length(Donnees));

  FCache.Add(Cle, Stream);
end;

function TDataCache.Obtenir(const Cle: string; out Donnees: TBytes): Boolean;
var
  Stream: TMemoryStream;
begin
  Result := FCache.TryGetValue(Cle, Stream);
  if Result then
  begin
    SetLength(Donnees, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Donnees[0], Stream.Size);
  end;
end;

procedure TDataCache.Effacer(const Cle: string);
var
  Stream: TMemoryStream;
begin
  if FCache.TryGetValue(Cle, Stream) then
  begin
    Stream.Free;
    FCache.Remove(Cle);
  end;
end;

procedure TDataCache.EffacerTout;
var
  Pair: TPair<string, TMemoryStream>;
begin
  for Pair in FCache do
    Pair.Value.Free;
  FCache.Clear;
end;
```

---

## Tableau comparatif des classes de Stream

| Classe | Usage principal | Avantages | Inconvénients |
|--------|----------------|-----------|---------------|
| **TFileStream** | Fichiers sur disque | Pas de limite de taille | Plus lent (I/O disque) |
| **TMemoryStream** | Données en RAM | Très rapide | Limité par la RAM |
| **TStringStream** | Manipulation de texte | Simple pour le texte | Moins flexible |
| **TBytesStream** | Tableaux d'octets | Interface pratique | Duplication mémoire |
| **TResourceStream** | Ressources embarquées | Lecture seule | Lecture seule |

---

## Bonnes pratiques essentielles

### 1. Toujours libérer les streams

```pascal
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    // Votre code
  finally
    Stream.Free;
  end;
end;
```

### 2. Vérifier la position et la taille

```pascal
if Stream.Position + SizeOf(Integer) <= Stream.Size then
  Stream.Read(Valeur, SizeOf(Integer))
else
  raise Exception.Create('Pas assez de données dans le stream');
```

### 3. Utiliser ReadBuffer/WriteBuffer pour la sécurité

```pascal
// Préférer ReadBuffer qui lève une exception si pas assez de données
Stream.ReadBuffer(Valeur, SizeOf(Integer));

// Au lieu de Read qui retourne juste moins d'octets
BytesLus := Stream.Read(Valeur, SizeOf(Integer));
```

### 4. Réinitialiser la position avant lecture

```pascal
Stream.Position := 0;
// Puis lire
```

### 5. Prévoir l'encodage pour le texte

```pascal
// Toujours spécifier l'encodage
StringStream := TStringStream.Create('', TEncoding.UTF8);
```

---

## Résumé

Dans ce chapitre, vous avez découvert l'écosystème complet des streams en Delphi :

**Classes principales :**
- **TStream** : classe de base abstraite avec les méthodes fondamentales
- **TFileStream** : pour les fichiers sur disque
- **TMemoryStream** : pour les données en mémoire
- **TStringStream** : pour manipuler du texte
- **TBytesStream** : pour les tableaux d'octets
- **TResourceStream** : pour les ressources embarquées

**Opérations essentielles :**
- Lecture avec `Read` et `ReadBuffer`
- Écriture avec `Write` et `WriteBuffer`
- Navigation avec `Seek` et `Position`
- Copie avec `CopyFrom`
- Sauvegarde/Chargement avec `SaveToFile` et `LoadFromFile`

**Points clés :**
- Les streams offrent une interface unifiée pour manipuler différentes sources de données
- Toujours libérer les streams dans un bloc `finally`
- Préférer `TMemoryStream` pour la performance, `TFileStream` pour les gros volumes
- Utiliser les bons encodages pour le texte
- Vérifier toujours les positions et tailles avant lecture/écriture

Les streams sont un outil fondamental en Delphi qui vous servira dans de nombreux contextes : fichiers, réseau, sérialisation, compression, chiffrement, etc. Maîtriser les streams vous permettra de manipuler les données de manière efficace et élégante !

⏭️ [Sérialisation et persistance d'objets](/07-gestion-des-fichiers-et-flux-de-donnees/04-serialisation-et-persistance-dobjets.md)
