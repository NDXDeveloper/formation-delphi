# 13.6 Support Unicode et encodages

Le support des différents encodages de caractères, et notamment d'Unicode, est essentiel pour créer des applications véritablement internationales. Dans cette section, nous allons explorer comment Delphi gère les encodages et comment vous pouvez travailler efficacement avec Unicode.

## Comprendre les encodages de caractères

Avant de plonger dans les détails techniques, prenons un moment pour comprendre les concepts de base.

### Qu'est-ce qu'un encodage de caractères ?

Un encodage de caractères est une méthode qui associe des caractères textuels (lettres, chiffres, symboles) à des valeurs numériques que l'ordinateur peut traiter. C'est comme un dictionnaire qui indique à l'ordinateur comment représenter et interpréter les caractères.

### Les principaux types d'encodages

Il existe plusieurs encodages différents, chacun ayant ses propres caractéristiques :

- **ASCII** : L'encodage le plus ancien, ne gère que 128 caractères (principalement l'alphabet latin non accentué et quelques symboles)
- **ANSI/Windows-1252** : Extension d'ASCII avec 256 caractères, utilisé par les anciennes versions de Windows
- **UTF-8** : Encodage Unicode variable (1 à 4 octets par caractère), très répandu sur le web
- **UTF-16** : Encodage Unicode utilisant 2 ou 4 octets par caractère, utilisé par Windows pour l'interface utilisateur
- **UTF-32** : Encodage Unicode utilisant 4 octets par caractère, rarement utilisé mais simple car à taille fixe

### Qu'est-ce qu'Unicode ?

Unicode est une norme informatique qui vise à représenter tous les caractères de toutes les langues écrites. Au lieu des 256 caractères maximum des encodages traditionnels, Unicode peut représenter plus d'un million de caractères, couvrant pratiquement toutes les langues et symboles du monde.

## L'évolution du support Unicode dans Delphi

Le support d'Unicode dans Delphi a considérablement évolué au fil des versions :

- **Delphi 1-2007** : Le type `string` par défaut était `AnsiString` (limité aux jeux de caractères locaux)
- **Delphi 2009 et ultérieur** : Le type `string` par défaut est désormais `UnicodeString` (compatible avec tous les caractères Unicode)

> 💡 Dans les versions modernes de Delphi (à partir de Delphi 2009), vous n'avez généralement pas besoin de vous préoccuper des encodages pour les chaînes en mémoire, car le type `string` gère automatiquement Unicode.

## Types de chaînes dans Delphi moderne

Delphi offre plusieurs types de chaînes différents, chacun avec son propre encodage :

| Type | Description | Encodage | Usage |
|------|-------------|----------|-------|
| `string` | Type de chaîne par défaut | UTF-16 | Usage général |
| `UnicodeString` | Identique à `string` | UTF-16 | Usage général |
| `AnsiString` | Chaîne 8 bits | Selon page de code | Compatibilité avec du code ancien |
| `UTF8String` | Chaîne en UTF-8 | UTF-8 | Échanges web, fichiers texte |
| `RawByteString` | Chaîne d'octets sans encodage spécifique | Non défini | Données binaires |
| `WideString` | Chaîne Unicode compatible COM | UTF-16 | Interopération avec COM |

> ⚠️ Attention : Bien que ces différents types existent, dans le code moderne, vous devriez principalement utiliser le type `string` (qui est `UnicodeString`), sauf en cas de besoin spécifique.

## Conversion entre encodages

### Conversion entre les types de chaînes

Delphi gère automatiquement les conversions entre différents types de chaînes :

```pascal
var
  S: string;           // UnicodeString (UTF-16)
  U8: UTF8String;      // UTF-8
  A: AnsiString;       // ANSI selon la page de code système
begin
  // Assignation d'une chaîne Unicode
  S := 'Bonjour à tous ! こんにちは';

  // Conversion en UTF-8
  U8 := UTF8String(S);

  // Conversion en ANSI (attention : caractères non-ANSI seront altérés)
  A := AnsiString(S);

  // Conversion depuis UTF-8 vers Unicode
  S := string(U8);

  // Affichage des longueurs (nombre de caractères vs octets)
  ShowMessage(Format('Unicode: %d caractères', [Length(S)]));
  ShowMessage(Format('UTF-8: %d octets', [Length(U8)]));
  ShowMessage(Format('ANSI: %d octets', [Length(A)]));
end;
```

### Fonctions de conversion explicites

Pour plus de contrôle, Delphi fournit des fonctions de conversion explicites :

```pascal
uses
  System.SysUtils;

var
  S: string;
  U8Bytes: TBytes;
  U8String: UTF8String;
begin
  S := 'こんにちは'; // "Bonjour" en japonais

  // Conversion de Unicode (UTF-16) vers UTF-8
  U8Bytes := TEncoding.UTF8.GetBytes(S);

  // Conversion de UTF-8 vers Unicode
  S := TEncoding.UTF8.GetString(U8Bytes);

  // Autre méthode pour UTF-8
  U8String := UTF8Encode(S);
  S := UTF8ToString(U8String);
end;
```

## La classe TEncoding

À partir de Delphi 2009, la classe `TEncoding` fournit une approche unifiée pour gérer les différents encodages :

```pascal
uses
  System.SysUtils;

var
  S: string;
  Bytes: TBytes;
begin
  S := 'Texte avec des caractères accentués : é à ç';

  // Conversion en différents encodages
  Bytes := TEncoding.ASCII.GetBytes(S);    // ASCII (attention aux pertes)
  Bytes := TEncoding.ANSI.GetBytes(S);     // ANSI (page de code système)
  Bytes := TEncoding.UTF8.GetBytes(S);     // UTF-8
  Bytes := TEncoding.Unicode.GetBytes(S);  // UTF-16 Little Endian
  Bytes := TEncoding.BigEndianUnicode.GetBytes(S); // UTF-16 Big Endian

  // Reconversion vers chaîne
  S := TEncoding.UTF8.GetString(Bytes);
end;
```

### Encodages disponibles dans TEncoding

La classe `TEncoding` propose plusieurs encodages prédéfinis :

- `TEncoding.ASCII` : Encodage ASCII 7 bits
- `TEncoding.ANSI` : Encodage ANSI (page de code système)
- `TEncoding.UTF8` : Encodage UTF-8
- `TEncoding.Unicode` : Encodage UTF-16 Little Endian (ordre des octets Windows)
- `TEncoding.BigEndianUnicode` : Encodage UTF-16 Big Endian (ordre des octets inverse)
- `TEncoding.UTF7` : Encodage UTF-7 (obsolète, pour compatibilité)

> 💡 Pour la plupart des cas d'utilisation modernes, UTF-8 est recommandé pour l'échange de données et les fichiers texte, car il est compact et universellement compatible.

### Détection automatique d'encodage

`TEncoding` peut également détecter automatiquement l'encodage d'un fichier texte grâce à la marque d'ordre des octets (BOM) :

```pascal
var
  Bytes: TBytes;
  Encoding: TEncoding;
  OffsetToText: Integer;
  S: string;
begin
  // Lire le fichier en mémoire
  Bytes := TFile.ReadAllBytes('monfichier.txt');

  // Détecter l'encodage
  Encoding := nil;
  OffsetToText := TEncoding.GetBufferEncoding(Bytes, Encoding);

  // Si aucun encodage n'est détecté, supposer UTF-8
  if Encoding = nil then
    Encoding := TEncoding.UTF8;

  // Convertir en chaîne en ignorant le BOM
  S := Encoding.GetString(Bytes, OffsetToText, Length(Bytes) - OffsetToText);

  ShowMessage('Encodage détecté : ' + Encoding.ClassName);
  ShowMessage('Contenu : ' + S);
end;
```

## Lecture et écriture de fichiers texte avec différents encodages

### Écriture d'un fichier texte

```pascal
procedure WriteTextFile(const FileName, Text: string; Encoding: TEncoding);
var
  Stream: TStreamWriter;
begin
  Stream := TStreamWriter.Create(FileName, False, Encoding);
  try
    Stream.Write(Text);
  finally
    Stream.Free;
  end;
end;

// Utilisation
begin
  // Écrire un fichier UTF-8 avec BOM
  WriteTextFile('fichier_utf8.txt', 'Texte avec des caractères spéciaux: é à ç', TEncoding.UTF8);

  // Écrire un fichier ANSI
  WriteTextFile('fichier_ansi.txt', 'Texte ANSI', TEncoding.ANSI);

  // Écrire un fichier Unicode (UTF-16)
  WriteTextFile('fichier_unicode.txt', 'Texte Unicode', TEncoding.Unicode);
end;
```

### Lecture d'un fichier texte

```pascal
function ReadTextFile(const FileName: string; Encoding: TEncoding = nil): string;
var
  Stream: TStreamReader;
begin
  // Si aucun encodage n'est spécifié, la détection automatique est utilisée
  if Encoding = nil then
    Stream := TStreamReader.Create(FileName)
  else
    Stream := TStreamReader.Create(FileName, Encoding);

  try
    Result := Stream.ReadToEnd;
  finally
    Stream.Free;
  end;
end;

// Utilisation
begin
  // Lecture avec détection automatique d'encodage
  Memo1.Text := ReadTextFile('monfichier.txt');

  // Forcer la lecture en UTF-8
  Memo1.Text := ReadTextFile('monfichier.txt', TEncoding.UTF8);

  // Forcer la lecture en ANSI
  Memo1.Text := ReadTextFile('monfichier.txt', TEncoding.ANSI);
end;
```

## Gestion des fichiers sans BOM

Certains fichiers UTF-8 n'incluent pas de BOM (Byte Order Mark), notamment ceux créés sur des systèmes Unix/Linux. Pour gérer ces fichiers :

```pascal
function ReadUTF8FileWithoutBOM(const FileName: string): string;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Lire tout le contenu du fichier
    SetLength(Bytes, Stream.Size);
    Stream.ReadBuffer(Bytes, 0, Length(Bytes));

    // Convertir en supposant que c'est de l'UTF-8 sans BOM
    Result := TEncoding.UTF8.GetString(Bytes);
  finally
    Stream.Free;
  end;
end;
```

## Travailler avec les contrôles VCL et Unicode

La plupart des contrôles VCL supportent nativement Unicode dans les versions modernes de Delphi :

```pascal
procedure TForm1.btnSetTextClick(Sender: TObject);
begin
  // Les contrôles VCL supportent Unicode
  Label1.Caption := 'Texte Unicode: こんにちは';
  Edit1.Text := 'Éditeur avec caractères spéciaux: é à ç';
  Memo1.Lines.Add('Mémo avec support multilingue: français, русский, 中文');

  // Les boîtes de dialogue aussi
  ShowMessage('Message Unicode: ¡Hola! שלום! Привет!');
end;
```

### Utilisation de polices avec support Unicode

Pour afficher correctement tous les caractères Unicode, assurez-vous d'utiliser une police qui les prend en charge :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Polices recommandées pour Unicode
  Memo1.Font.Name := 'Arial Unicode MS';  // Large couverture Unicode
  // ou
  Memo1.Font.Name := 'Segoe UI';          // Police moderne Windows
  // ou
  Memo1.Font.Name := 'Noto Sans';         // Police Google avec large support
end;
```

> 💡 Les polices modernes comme Arial Unicode MS, Segoe UI, Noto Sans, ou Microsoft Sans Serif offrent une bonne couverture Unicode pour la plupart des langues.

## Manipuler des caractères Unicode spécifiques

### Codes de caractères Unicode

Vous pouvez insérer des caractères Unicode spécifiques en utilisant leur code :

```pascal
var
  S: string;
begin
  // Insérer des caractères Unicode par leur code (format hexadécimal)
  S := 'Symbole euro: ' + Char($20AC);       // € (U+20AC)
  S := S + #13#10 + 'Copyright: ' + Char($00A9);  // © (U+00A9)
  S := S + #13#10 + 'Cœur: ' + Char($2764);       // ❤ (U+2764)
  S := S + #13#10 + 'Smiley: ' + Char($263A);     // ☺ (U+263A)

  Memo1.Text := S;
end;
```

### Manipulation de chaînes Unicode

Pour manipuler des chaînes Unicode, tenez compte du fait que certains caractères peuvent être composés de plusieurs unités de code :

```pascal
uses
  System.Character;

procedure ManipulateUnicode;
var
  S: string;
  I: Integer;
  C: Char;
begin
  S := 'Texte Unicode: こんにちは';

  Memo1.Lines.Add('Analyse de la chaîne:');

  // Parcourir les caractères
  for I := 1 to Length(S) do
  begin
    C := S[I];

    // Analyser le type de caractère
    if TCharacter.IsLetter(C) then
    begin
      if TCharacter.IsUpper(C) then
        Memo1.Lines.Add(Format('Caractère %d: %s (Lettre majuscule)', [I, C]))
      else if TCharacter.IsLower(C) then
        Memo1.Lines.Add(Format('Caractère %d: %s (Lettre minuscule)', [I, C]))
      else
        Memo1.Lines.Add(Format('Caractère %d: %s (Lettre)', [I, C]));
    end
    else if TCharacter.IsDigit(C) then
      Memo1.Lines.Add(Format('Caractère %d: %s (Chiffre)', [I, C]))
    else if TCharacter.IsWhiteSpace(C) then
      Memo1.Lines.Add(Format('Caractère %d: (Espace)', [I]))
    else if TCharacter.IsPunctuation(C) then
      Memo1.Lines.Add(Format('Caractère %d: %s (Ponctuation)', [I, C]))
    else
      Memo1.Lines.Add(Format('Caractère %d: %s (Autre)', [I, C]));

    // Obtenir le code Unicode du caractère
    Memo1.Lines.Add(Format('  - Code Unicode: U+%4.4X', [Ord(C)]));
  end;
end;
```

## Gestion des pages de code

Dans certains cas, notamment lors de l'interaction avec des systèmes anciens, vous pourriez avoir besoin de travailler avec des pages de code spécifiques :

```pascal
uses
  System.SysUtils;

var
  UnicodeStr: string;
  AnsiStr: AnsiString;
  CodePage: Integer;
begin
  UnicodeStr := 'Texte avec des caractères accentués: é à ç';

  // Conversion avec une page de code spécifique
  CodePage := 1252;  // Europe occidentale (Windows)
  AnsiStr := AnsiString(UnicodeStr);
  SetCodePage(RawByteString(AnsiStr), CodePage, False);

  // Reconversion vers Unicode
  UnicodeStr := string(AnsiStr);
end;
```

> ⚠️ Attention : La manipulation directe des pages de code est rarement nécessaire dans le code moderne et peut entraîner des pertes de données si elle n'est pas effectuée correctement.

## Bases de données et Unicode

### ADO et Unicode

Si vous utilisez ADO pour accéder aux bases de données :

```pascal
procedure TForm1.ADOQueryUnicode;
begin
  ADOQuery1.SQL.Text := 'SELECT * FROM Clients WHERE Nom LIKE :Nom';
  ADOQuery1.Parameters.ParamByName('Nom').Value := '%Müller%';
  ADOQuery1.Open;

  // Les données Unicode sont automatiquement gérées
  while not ADOQuery1.Eof do
  begin
    Memo1.Lines.Add(ADOQuery1.FieldByName('Nom').AsString);
    ADOQuery1.Next;
  end;
end;
```

### FireDAC et Unicode

FireDAC gère nativement Unicode :

```pascal
procedure TForm1.FireDACQueryUnicode;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM Produits WHERE Description LIKE :Desc';
  FDQuery1.ParamByName('Desc').AsString := '%français%';
  FDQuery1.Open;

  // Afficher les résultats Unicode
  while not FDQuery1.Eof do
  begin
    ListBox1.Items.Add(FDQuery1.FieldByName('Description').AsString);
    FDQuery1.Next;
  end;
end;
```

### SQLite et Unicode

SQLite gère nativement l'UTF-8 :

```pascal
procedure TForm1.SQLiteUnicode;
begin
  // Configuration pour SQLite (généralement pas nécessaire, car UTF-8 par défaut)
  FDConnection1.Params.Values['DriverID'] := 'SQLite';
  FDConnection1.Params.Values['Database'] := 'mabase.db';

  // Exécuter une requête avec des données Unicode
  FDQuery1.SQL.Text := 'INSERT INTO Messages (Texte) VALUES (:Texte)';
  FDQuery1.ParamByName('Texte').AsString := 'Message multilingue: español, русский, 日本語';
  FDQuery1.ExecSQL;
end;
```

## Exemple complet : Éditeur de texte multilingue

Voici un exemple d'application simple d'éditeur de texte qui gère différents encodages :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmEditor = class(TForm)
    memText: TMemo;
    pnlTop: TPanel;
    lblEncoding: TLabel;
    cmbEncoding: TComboBox;
    btnOpen: TButton;
    btnSave: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    lblFont: TLabel;
    cmbFont: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cmbFontChange(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    function GetSelectedEncoding: TEncoding;
  end;

var
  frmEditor: TfrmEditor;

implementation

{$R *.dfm}

procedure TfrmEditor.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Initialiser la liste des encodages
  cmbEncoding.Items.Clear;
  cmbEncoding.Items.Add('UTF-8');
  cmbEncoding.Items.Add('ANSI');
  cmbEncoding.Items.Add('Unicode (UTF-16)');
  cmbEncoding.Items.Add('ASCII');
  cmbEncoding.ItemIndex := 0; // UTF-8 par défaut

  // Remplir la liste des polices
  cmbFont.Items.Clear;
  for I := 0 to Screen.Fonts.Count - 1 do
    cmbFont.Items.Add(Screen.Fonts[I]);

  // Sélectionner une police avec bon support Unicode
  if cmbFont.Items.IndexOf('Segoe UI') >= 0 then
    cmbFont.ItemIndex := cmbFont.Items.IndexOf('Segoe UI')
  else if cmbFont.Items.IndexOf('Arial') >= 0 then
    cmbFont.ItemIndex := cmbFont.Items.IndexOf('Arial')
  else
    cmbFont.ItemIndex := 0;

  // Appliquer la police
  cmbFontChange(nil);
end;

procedure TfrmEditor.cmbFontChange(Sender: TObject);
begin
  // Changer la police du mémo
  if cmbFont.ItemIndex >= 0 then
  begin
    memText.Font.Name := cmbFont.Items[cmbFont.ItemIndex];
    memText.Font.Size := 11; // Taille raisonnable
  end;
end;

function TfrmEditor.GetSelectedEncoding: TEncoding;
begin
  // Obtenir l'encodage sélectionné
  case cmbEncoding.ItemIndex of
    0: Result := TEncoding.UTF8;
    1: Result := TEncoding.ANSI;
    2: Result := TEncoding.Unicode;
    3: Result := TEncoding.ASCII;
    else
      Result := TEncoding.UTF8; // Par défaut
  end;
end;

procedure TfrmEditor.btnOpenClick(Sender: TObject);
var
  Encoding: TEncoding;
  FileStream: TFileStream;
  Bytes: TBytes;
  PreambleSize: Integer;
begin
  if dlgOpen.Execute then
  begin
    try
      FileStream := TFileStream.Create(dlgOpen.FileName, fmOpenRead or fmShareDenyWrite);
      try
        // Lire tout le contenu du fichier
        SetLength(Bytes, FileStream.Size);
        if Length(Bytes) > 0 then
          FileStream.ReadBuffer(Bytes[0], Length(Bytes));

        // Détecter l'encodage
        Encoding := nil;
        PreambleSize := TEncoding.GetBufferEncoding(Bytes, Encoding);

        // Si aucun encodage n'est détecté, utiliser celui sélectionné
        if Encoding = nil then
          Encoding := GetSelectedEncoding;
        else
        begin
          // Mettre à jour la sélection dans le combobox
          if Encoding = TEncoding.UTF8 then
            cmbEncoding.ItemIndex := 0
          else if Encoding = TEncoding.Unicode then
            cmbEncoding.ItemIndex := 2
          else if Encoding = TEncoding.ASCII then
            cmbEncoding.ItemIndex := 3
          else
            cmbEncoding.ItemIndex := 1; // ANSI
        end;

        // Convertir les octets en texte
        memText.Text := Encoding.GetString(Bytes, PreambleSize, Length(Bytes) - PreambleSize);
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''ouverture du fichier: ' + E.Message);
    end;
  end;
end;

procedure TfrmEditor.btnSaveClick(Sender: TObject);
var
  Encoding: TEncoding;
  StreamWriter: TStreamWriter;
begin
  if dlgSave.Execute then
  begin
    try
      Encoding := GetSelectedEncoding;

      // Créer un StreamWriter avec l'encodage sélectionné
      StreamWriter := TStreamWriter.Create(dlgSave.FileName, False, Encoding);
      try
        StreamWriter.Write(memText.Text);
      finally
        StreamWriter.Free;
      end;

      ShowMessage('Fichier enregistré avec succès.');
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''enregistrement du fichier: ' + E.Message);
    end;
  end;
end;

end.
```

## Bonnes pratiques pour le support Unicode

1. **Utilisez toujours `string` (UnicodeString)** pour les chaînes dans votre code

2. **Préférez UTF-8 pour les fichiers externes et échanges de données**

3. **Utilisez les classes `TStreamReader` et `TStreamWriter`** pour la lecture et l'écriture de fichiers texte

4. **Pour les bases de données, préférez les paramètres** aux chaînes SQL concaténées pour éviter les problèmes d'encodage

5. **Vérifiez que vos polices supportent les caractères nécessaires** à votre application

6. **Testez avec des caractères de différentes langues**, pas seulement avec des caractères occidentaux

7. **Évitez les fonctions qui ne sont pas compatibles Unicode** comme `StrPos`, `StrCopy` (utilisez plutôt `Pos`, `Copy`)

8. **Ne supposez pas qu'un caractère = un octet**, car en Unicode ce n'est pas toujours le cas

9. **Utilisez la classe `TCharacter`** pour manipuler des caractères Unicode individuels

10. **Gérez correctement les BOM (Byte Order Mark)** lors de la lecture de fichiers

## Conclusion

Le support d'Unicode dans Delphi moderne est robuste et complet, permettant de créer des applications véritablement internationales qui fonctionnent avec toutes les langues. En comprenant les principes fondamentaux des encodages et en suivant les bonnes pratiques, vous pouvez éviter la plupart des problèmes liés aux caractères internationaux.

Les points clés à retenir :

- Les versions modernes de Delphi utilisent `string` (UnicodeString) comme type par défaut, ce qui simplifie grandement la gestion des caractères internationaux
- La classe `TEncoding` fournit une approche unifiée pour gérer les conversions entre différents encodages
- Utilisez `TStreamReader` et `TStreamWriter` pour la lecture et l'écriture de fichiers texte avec différents encodages
- Testez toujours votre application avec des caractères de diverses langues pour vous assurer qu'elle fonctionne correctement dans un contexte international

---

Dans la prochaine section, nous verrons comment gérer les écritures bidirectionnelles pour les langues comme l'arabe et l'hébreu.
