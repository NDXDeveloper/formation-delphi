🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.6 Support Unicode et encodages

## Introduction

L'encodage des caractères est un sujet fondamental mais souvent mal compris en programmation. Comprendre Unicode et les différents encodages est essentiel pour créer des applications internationales robustes. Dans cette section, nous allons démystifier ces concepts et voir comment Delphi gère l'Unicode.

## Qu'est-ce que l'encodage de caractères ?

### Le problème historique

Au début de l'informatique, chaque caractère était représenté par un nombre. Par exemple, la lettre 'A' était représentée par le nombre 65. Mais chaque pays a créé son propre système d'encodage :

| Encodage | Région | Caractères supportés | Problème |
|----------|--------|---------------------|----------|
| **ASCII** | Amérique | 128 caractères (0-127) | Pas d'accents |
| **ISO-8859-1** | Europe occidentale | 256 caractères | Pas de cyrillique |
| **ISO-8859-5** | Europe de l'Est | Alphabet cyrillique | Incompatible avec ISO-8859-1 |
| **Windows-1252** | Windows | Extension d'ISO-8859-1 | Non standard |
| **Shift-JIS** | Japon | Caractères japonais | Incompatible avec tout |

**Le cauchemar** : Un fichier créé en russe (ISO-8859-5) affiché avec l'encodage français (ISO-8859-1) montrait des caractères complètement différents !

### La solution : Unicode

Unicode est un **standard universel** qui attribue un numéro unique (appelé "code point") à **chaque caractère** de **toutes les langues** du monde.

```
Exemples de code points Unicode :  
U+0041 = 'A' (lettre latine majuscule A)  
U+00E9 = 'é' (e accent aigu)  
U+4E2D = '中' (caractère chinois)  
U+0623 = 'أ' (lettre arabe)  
U+1F600 = '😀' (emoji visage souriant)  
```

> 💡 **Unicode** résout le problème : un seul système pour tous les caractères du monde !

## Les différents encodages Unicode

Unicode définit les caractères, mais il existe plusieurs façons de les **encoder en octets** :

### Tableau comparatif des encodages

| Encodage | Taille par caractère | Avantages | Inconvénients | Usage |
|----------|---------------------|-----------|---------------|-------|
| **UTF-8** | 1 à 4 octets | Compatible ASCII, économe | Taille variable | Web, fichiers texte |
| **UTF-16** | 2 ou 4 octets | Bon compromis | Taille variable | Windows, Delphi interne |
| **UTF-32** | 4 octets fixes | Taille fixe, simple | Très lourd | Rare, traitement interne |
| **ANSI** | 1 octet | Compact | Limité à 256 caractères | Héritage, compatibilité |

### UTF-8 : L'encodage universel du Web

UTF-8 est l'encodage le plus populaire aujourd'hui car :
- Compatible avec ASCII (les 128 premiers caractères sont identiques)
- Économe en espace pour les textes latins
- Utilisé par défaut sur le Web

**Exemples de taille en UTF-8 :**

| Caractère | Code point | UTF-8 (octets) | Nombre d'octets |
|-----------|------------|----------------|-----------------|
| A | U+0041 | 41 | 1 octet |
| é | U+00E9 | C3 A9 | 2 octets |
| € | U+20AC | E2 82 AC | 3 octets |
| 中 | U+4E2D | E4 B8 AD | 3 octets |
| 😀 | U+1F600 | F0 9F 98 80 | 4 octets |

### UTF-16 : L'encodage de Windows et Delphi

UTF-16 utilise 2 octets pour la plupart des caractères courants, et 4 octets pour les caractères plus rares.

**Avantages pour Delphi :**
- Efficace pour les langues européennes et asiatiques
- Utilisé nativement par Windows
- Support direct dans Delphi moderne

### UTF-32 : Le plus simple mais le plus lourd

UTF-32 utilise toujours 4 octets par caractère. Simple mais très gourmand en mémoire.

## Support Unicode dans Delphi

### Évolution du support Unicode

| Version Delphi | Type string | Encodage | Support Unicode |
|----------------|-------------|----------|-----------------|
| Delphi 1-2007 | AnsiString | ANSI | ❌ Limité |
| Delphi 2009+ | UnicodeString | UTF-16 | ✅ Complet |
| Delphi actuel | string = UnicodeString | UTF-16 | ✅ Natif |

> 💡 Depuis Delphi 2009, le type `string` est équivalent à `UnicodeString`, ce qui signifie que **l'Unicode est géré nativement**.

### Types de chaînes et leur encodage

```pascal
type
  // Types de chaînes disponibles
  AnsiString;        // Encodage ANSI (1 octet par caractère)
  UnicodeString;     // UTF-16 (2 ou 4 octets par caractère)
  WideString;        // UTF-16 (compatible COM)
  UTF8String;        // UTF-8 (1 à 4 octets par caractère)
  RawByteString;     // Octets bruts sans encodage spécifique

  // Alias pratique
  string = UnicodeString;  // Par défaut dans Delphi moderne
```

### Déclaration et utilisation

```pascal
var
  TexteUnicode: string;           // UTF-16 (recommandé)
  TexteUTF8: UTF8String;          // UTF-8
  TexteAnsi: AnsiString;          // ANSI (ancien)
  TexteBrut: RawByteString;       // Octets bruts
begin
  // Delphi gère automatiquement l'Unicode
  TexteUnicode := 'Bonjour 你好 مرحبا 😀';

  // Tous les caractères du monde sont supportés !
  ShowMessage(TexteUnicode);
end;
```

## Conversion entre encodages

### Conversion automatique

Delphi effectue automatiquement les conversions entre types de chaînes :

```pascal
var
  TexteUnicode: string;
  TexteUTF8: UTF8String;
  TexteAnsi: AnsiString;
begin
  TexteUnicode := 'Café';

  // Conversion automatique Unicode → UTF-8
  TexteUTF8 := UTF8String(TexteUnicode);

  // Conversion automatique Unicode → ANSI
  TexteAnsi := AnsiString(TexteUnicode);
  // ⚠️ Attention : risque de perte de caractères si ANSI ne supporte pas
end;
```

### Conversion explicite avec System.SysUtils

```pascal
uses
  System.SysUtils;

var
  TexteUnicode: string;
  OctetsUTF8: TBytes;
begin
  TexteUnicode := 'Bonjour le monde 🌍';

  // Convertir string → UTF-8 bytes
  OctetsUTF8 := TEncoding.UTF8.GetBytes(TexteUnicode);

  // Reconvertir UTF-8 bytes → string
  TexteUnicode := TEncoding.UTF8.GetString(OctetsUTF8);
end;
```

### Classe TEncoding

La classe `TEncoding` fournit des méthodes pour gérer tous les encodages :

```pascal
uses
  System.SysUtils, System.Classes;

var
  Texte: string;
  OctetsUTF8, OctetsUTF16, OctetsAnsi: TBytes;
begin
  Texte := 'Héllo Wörld! 世界';

  // Obtenir les octets dans différents encodages
  OctetsUTF8 := TEncoding.UTF8.GetBytes(Texte);
  OctetsUTF16 := TEncoding.Unicode.GetBytes(Texte);  // UTF-16
  OctetsAnsi := TEncoding.ANSI.GetBytes(Texte);

  ShowMessage(Format(
    'Taille en UTF-8: %d octets'#13#10 +
    'Taille en UTF-16: %d octets'#13#10 +
    'Taille en ANSI: %d octets',
    [Length(OctetsUTF8), Length(OctetsUTF16), Length(OctetsAnsi)]
  ));
end;
```

### Encodages disponibles

| Encodage | Classe TEncoding | Description |
|----------|------------------|-------------|
| UTF-8 | `TEncoding.UTF8` | Encodage standard du Web |
| UTF-16 LE | `TEncoding.Unicode` | UTF-16 Little Endian (Windows) |
| UTF-16 BE | `TEncoding.BigEndianUnicode` | UTF-16 Big Endian |
| UTF-7 | `TEncoding.UTF7` | Ancien standard email |
| UTF-32 | `TEncoding.UTF32` | 4 octets par caractère |
| ANSI | `TEncoding.ANSI` | Page de code système |
| ASCII | `TEncoding.ASCII` | 7 bits, caractères basiques |

## Lecture et écriture de fichiers

### Le BOM (Byte Order Mark)

Le BOM est une séquence d'octets au début d'un fichier qui indique son encodage.

| Encodage | BOM (octets) | BOM (hexa) |
|----------|--------------|------------|
| UTF-8 | EF BB BF | `$EFBBBF` |
| UTF-16 LE | FF FE | `$FEFF` |
| UTF-16 BE | FE FF | `$FFFE` |
| UTF-32 LE | FF FE 00 00 | `$FFFE0000` |
| UTF-32 BE | 00 00 FE FF | `$0000FEFF` |

> 💡 Le BOM n'est **pas obligatoire** mais **recommandé** car il permet de détecter automatiquement l'encodage.

### Écriture de fichiers avec encodage

```pascal
uses
  System.IOUtils, System.SysUtils, System.Classes;

procedure EcrireFichierUTF8(const NomFichier, Contenu: string);  
var  
  Fichier: TStreamWriter;
begin
  // Créer un fichier en UTF-8 avec BOM
  Fichier := TStreamWriter.Create(NomFichier, False, TEncoding.UTF8);
  try
    Fichier.Write(Contenu);
  finally
    Fichier.Free;
  end;
end;

procedure EcrireFichierSansEncodage;  
begin  
  // Méthode simple : UTF-8 par défaut avec BOM
  TFile.WriteAllText('fichier.txt', 'Contenu avec accents: é à ç', TEncoding.UTF8);
end;
```

### Lecture de fichiers avec détection d'encodage

```pascal
uses
  System.IOUtils, System.SysUtils, System.Classes;

function LireFichierAvecDetection(const NomFichier: string): string;  
var  
  Fichier: TStreamReader;
begin
  // TStreamReader détecte automatiquement l'encodage via le BOM
  Fichier := TStreamReader.Create(NomFichier, True); // True = détecter encodage
  try
    Result := Fichier.ReadToEnd;
  finally
    Fichier.Free;
  end;
end;

function LireFichierUTF8(const NomFichier: string): string;  
begin  
  // Forcer la lecture en UTF-8
  Result := TFile.ReadAllText(NomFichier, TEncoding.UTF8);
end;

function LireFichierAnsi(const NomFichier: string): string;  
begin  
  // Lire un ancien fichier ANSI
  Result := TFile.ReadAllText(NomFichier, TEncoding.ANSI);
end;
```

### Détection manuelle de l'encodage

```pascal
uses
  System.SysUtils, System.Classes;

function DetecterEncodage(const NomFichier: string): TEncoding;  
var  
  Stream: TFileStream;
  Buffer: TBytes;
begin
  Result := nil;

  Stream := TFileStream.Create(NomFichier, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Buffer, 4);
    Stream.Read(Buffer[0], 4);

    // Vérifier le BOM
    if (Length(Buffer) >= 3) and
       (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
      Result := TEncoding.UTF8
    else if (Length(Buffer) >= 2) and
            (Buffer[0] = $FF) and (Buffer[1] = $FE) then
      Result := TEncoding.Unicode  // UTF-16 LE
    else if (Length(Buffer) >= 2) and
            (Buffer[0] = $FE) and (Buffer[1] = $FF) then
      Result := TEncoding.BigEndianUnicode  // UTF-16 BE
    else
      Result := TEncoding.ANSI;  // Par défaut
  finally
    Stream.Free;
  end;
end;
```

## Gestion des caractères spéciaux

### Émojis et caractères au-delà du BMP

Les émojis utilisent des "code points" au-delà du plan multilingue de base (BMP), nécessitant 4 octets en UTF-16.

```pascal
var
  TexteEmoji: string;
  Longueur: Integer;
begin
  TexteEmoji := 'Hello 😀 World';

  // Attention : Length() compte les unités de code, pas les caractères visuels
  Longueur := Length(TexteEmoji); // Peut être 13 ou 14 selon l'emoji

  ShowMessage('Longueur: ' + IntToStr(Longueur));
  // L'emoji 😀 peut être compté comme 1 ou 2 unités selon l'implémentation
end;
```

### Comptage correct des caractères

Pour compter les vrais caractères (graphèmes), utilisez des fonctions spécialisées :

```pascal
uses
  System.Character;

function CompterCaracteres(const Texte: string): Integer;  
var  
  i: Integer;
  Surrogate: Boolean;
begin
  Result := 0;
  Surrogate := False;

  for i := 1 to Length(Texte) do
  begin
    // Vérifier si c'est une paire de substitution (surrogate pair)
    if Char.IsSurrogate(Texte[i]) then
    begin
      if Char.IsHighSurrogate(Texte[i]) then
      begin
        Surrogate := True;
      end
      else if Char.IsLowSurrogate(Texte[i]) and Surrogate then
      begin
        Inc(Result);
        Surrogate := False;
      end;
    end
    else
    begin
      Inc(Result);
      Surrogate := False;
    end;
  end;
end;

// Utilisation
var
  Texte: string;
begin
  Texte := 'Hello 😀 World';
  ShowMessage('Caractères: ' + IntToStr(CompterCaracteres(Texte)));
end;
```

### Manipulation de caractères Unicode

```pascal
uses
  System.Character;

var
  Texte: string;
  c: Char;
begin
  Texte := 'Héllo Wörld! 世界 123';

  for c in Texte do
  begin
    // Tester les propriétés du caractère
    if TCharacter.IsLetter(c) then
      WriteLn(c + ' est une lettre');

    if TCharacter.IsDigit(c) then
      WriteLn(c + ' est un chiffre');

    if TCharacter.IsWhiteSpace(c) then
      WriteLn('Espace blanc');

    if TCharacter.IsUpper(c) then
      WriteLn(c + ' est une majuscule');
  end;
end;
```

## Problèmes courants et solutions

### Problème 1 : Caractères affichés incorrectement

**Symptôme :** Les accents s'affichent comme "Ã©" au lieu de "é"

**Cause :** Mauvais encodage lors de la lecture/écriture

**Solution :**

```pascal
// ❌ MAUVAIS : Sans spécifier l'encodage
TFile.WriteAllText('fichier.txt', 'Café');

// ✅ BON : Spécifier UTF-8
TFile.WriteAllText('fichier.txt', 'Café', TEncoding.UTF8);

// Lecture
var Texte := TFile.ReadAllText('fichier.txt', TEncoding.UTF8);
```

### Problème 2 : Perte de caractères avec ANSI

**Symptôme :** Les caractères spéciaux deviennent "?" ou disparaissent

**Cause :** Conversion vers ANSI qui ne supporte pas tous les caractères

**Solution :**

```pascal
// ❌ MAUVAIS : Conversion vers ANSI
var TexteAnsi: AnsiString;  
TexteAnsi := AnsiString('Café 中国'); // '中国' sera perdu !  

// ✅ BON : Rester en Unicode
var TexteUnicode: string;  
TexteUnicode := 'Café 中国'; // Tout est préservé  
```

### Problème 3 : Fichiers sans BOM

**Symptôme :** Fichiers UTF-8 non détectés correctement

**Cause :** Absence de BOM dans le fichier UTF-8

**Solution :**

```pascal
// Toujours écrire avec BOM pour faciliter la détection
procedure EcrireFichierAvecBOM(const NomFichier, Contenu: string);  
var  
  Writer: TStreamWriter;
begin
  Writer := TStreamWriter.Create(NomFichier, False, TEncoding.UTF8);
  try
    Writer.Write(Contenu);
  finally
    Writer.Free;
  end;
end;
```

### Problème 4 : Comparaison de chaînes sensible à la casse

**Symptôme :** 'é' et 'É' ne sont pas reconnus comme équivalents

**Solution :**

```pascal
uses
  System.SysUtils;

var
  Texte1, Texte2: string;
begin
  Texte1 := 'Café';
  Texte2 := 'CAFÉ';

  // Comparaison insensible à la casse
  if CompareText(Texte1, Texte2) = 0 then
    ShowMessage('Identiques (insensible à la casse)');

  // Ou en utilisant les méthodes de string
  if Texte1.ToLower = Texte2.ToLower then
    ShowMessage('Identiques');
end;
```

## Communication avec les API et bases de données

### Envoi de données à une API REST

```pascal
uses
  System.Net.HttpClient, System.SysUtils, System.Classes;

procedure EnvoyerDonneesUTF8;  
var  
  Client: THTTPClient;
  Reponse: IHTTPResponse;
  Contenu: TStringStream;
  JSON: string;
begin
  Client := THTTPClient.Create;
  try
    // Créer le JSON avec caractères spéciaux
    JSON := '{"nom":"Café", "ville":"Paris", "emoji":"😀"}';

    // Créer un stream UTF-8
    Contenu := TStringStream.Create(JSON, TEncoding.UTF8);
    try
      // Envoyer avec le bon Content-Type
      Client.ContentType := 'application/json; charset=utf-8';
      Reponse := Client.Post('https://api.example.com/data', Contenu);

      ShowMessage('Statut: ' + Reponse.StatusCode.ToString);
    finally
      Contenu.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

### Interaction avec MySQL/MariaDB

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Param;

procedure EnregistrerDonneesUnicode;  
var  
  Connection: TFDConnection;
  Query: TFDQuery;
begin
  Connection := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    // Configuration de la connexion
    Connection.Params.Add('CharacterSet=utf8mb4'); // Important pour Unicode complet
    Connection.Connected := True;

    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO utilisateurs (nom, ville) VALUES (:nom, :ville)';
    Query.ParamByName('nom').AsString := 'Café 中国 😀';
    Query.ParamByName('ville').AsString := 'Paris';
    Query.ExecSQL;
  finally
    Query.Free;
    Connection.Free;
  end;
end;
```

## Utilitaires pour l'Unicode

### Classe d'aide pour l'encodage

```pascal
unit HelperEncodage;

interface

uses
  System.SysUtils, System.Classes;

type
  THelperEncodage = class
  public
    // Détection
    class function DetecterEncodageFichier(const NomFichier: string): TEncoding;
    class function ContientBOM(const NomFichier: string): Boolean;

    // Conversion
    class function ConvertirVersUTF8(const Texte: string): UTF8String;
    class function ConvertirDepuisUTF8(const TexteUTF8: UTF8String): string;

    // Validation
    class function EstUTF8Valide(const Octets: TBytes): Boolean;
    class function ContientCaracteresNonASCII(const Texte: string): Boolean;

    // Information
    class function ObtenirNomEncodage(Encodage: TEncoding): string;
    class function TailleEnOctets(const Texte: string; Encodage: TEncoding): Integer;
  end;

implementation

class function THelperEncodage.DetecterEncodageFichier(const NomFichier: string): TEncoding;  
var  
  Stream: TFileStream;
  Buffer: TBytes;
begin
  Result := TEncoding.UTF8; // Par défaut

  if not FileExists(NomFichier) then
    Exit;

  Stream := TFileStream.Create(NomFichier, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size < 3 then
      Exit;

    SetLength(Buffer, 4);
    Stream.Read(Buffer[0], Min(4, Stream.Size));

    // UTF-8 BOM
    if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
      Result := TEncoding.UTF8
    // UTF-16 LE BOM
    else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
      Result := TEncoding.Unicode
    // UTF-16 BE BOM
    else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
      Result := TEncoding.BigEndianUnicode;
  finally
    Stream.Free;
  end;
end;

class function THelperEncodage.ContientBOM(const NomFichier: string): Boolean;  
var  
  Encodage: TEncoding;
begin
  Encodage := DetecterEncodageFichier(NomFichier);
  Result := Encodage <> TEncoding.ANSI;
end;

class function THelperEncodage.ConvertirVersUTF8(const Texte: string): UTF8String;  
begin  
  Result := UTF8String(Texte);
end;

class function THelperEncodage.ConvertirDepuisUTF8(const TexteUTF8: UTF8String): string;  
begin  
  Result := string(TexteUTF8);
end;

class function THelperEncodage.EstUTF8Valide(const Octets: TBytes): Boolean;  
var  
  i: Integer;
begin
  Result := True;
  i := 0;

  while i < Length(Octets) do
  begin
    if Octets[i] and $80 = 0 then
      Inc(i) // ASCII
    else if Octets[i] and $E0 = $C0 then
      Inc(i, 2) // 2 octets
    else if Octets[i] and $F0 = $E0 then
      Inc(i, 3) // 3 octets
    else if Octets[i] and $F8 = $F0 then
      Inc(i, 4) // 4 octets
    else
    begin
      Result := False;
      Break;
    end;
  end;
end;

class function THelperEncodage.ContientCaracteresNonASCII(const Texte: string): Boolean;  
var  
  c: Char;
begin
  Result := False;
  for c in Texte do
  begin
    if Ord(c) > 127 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function THelperEncodage.ObtenirNomEncodage(Encodage: TEncoding): string;  
begin  
  if Encodage = TEncoding.UTF8 then
    Result := 'UTF-8'
  else if Encodage = TEncoding.Unicode then
    Result := 'UTF-16 LE'
  else if Encodage = TEncoding.BigEndianUnicode then
    Result := 'UTF-16 BE'
  else if Encodage = TEncoding.UTF7 then
    Result := 'UTF-7'
  else if Encodage = TEncoding.ANSI then
    Result := 'ANSI'
  else if Encodage = TEncoding.ASCII then
    Result := 'ASCII'
  else
    Result := 'Inconnu';
end;

class function THelperEncodage.TailleEnOctets(const Texte: string; Encodage: TEncoding): Integer;  
var  
  Octets: TBytes;
begin
  Octets := Encodage.GetBytes(Texte);
  Result := Length(Octets);
end;

end.
```

### Utilisation de la classe helper

```pascal
uses
  HelperEncodage;

procedure ExempleUtilisation;  
var  
  Texte: string;
  Encodage: TEncoding;
  TailleUTF8, TailleUTF16: Integer;
begin
  Texte := 'Café 中国 😀';

  // Détecter l'encodage d'un fichier
  Encodage := THelperEncodage.DetecterEncodageFichier('fichier.txt');
  ShowMessage('Encodage: ' + THelperEncodage.ObtenirNomEncodage(Encodage));

  // Vérifier si contient des caractères spéciaux
  if THelperEncodage.ContientCaracteresNonASCII(Texte) then
    ShowMessage('Le texte contient des caractères non-ASCII');

  // Comparer les tailles
  TailleUTF8 := THelperEncodage.TailleEnOctets(Texte, TEncoding.UTF8);
  TailleUTF16 := THelperEncodage.TailleEnOctets(Texte, TEncoding.Unicode);

  ShowMessage(Format(
    'Taille en UTF-8: %d octets'#13#10 +
    'Taille en UTF-16: %d octets',
    [TailleUTF8, TailleUTF16]
  ));
end;
```

## Bonnes pratiques

### Règles d'or pour Unicode

| Règle | Description | Exemple |
|-------|-------------|---------|
| **Utiliser string** | Toujours utiliser `string` (= UnicodeString) | `var Texte: string;` |
| **UTF-8 pour fichiers** | Sauvegarder les fichiers texte en UTF-8 avec BOM | `TFile.WriteAllText(..., TEncoding.UTF8)` |
| **Spécifier l'encodage** | Toujours spécifier l'encodage lors des I/O | `TStreamWriter.Create(..., TEncoding.UTF8)` |
| **API REST** | Utiliser UTF-8 pour les API Web | `charset=utf-8` |
| **Base de données** | Utiliser utf8mb4 pour MySQL | `CharacterSet=utf8mb4` |
| **Éviter AnsiString** | Ne pas utiliser AnsiString sauf nécessité | Préférer `string` |

### Checklist Unicode

Avant de déployer votre application :

```
□ Tous les fichiers texte sont en UTF-8 avec BOM
□ Les fichiers sources (.pas) sont en UTF-8
□ Les chaînes de ressources supportent Unicode
□ Les connexions BDD utilisent utf8mb4
□ Les API REST utilisent charset=utf-8
□ Aucune conversion AnsiString non nécessaire
□ Les émojis s'affichent correctement
□ Les caractères asiatiques sont supportés
□ Les langues RTL (arabe, hébreu) fonctionnent
□ Aucun caractère transformé en "?"
```

### Tests recommandés

Testez votre application avec ces chaînes :

```pascal
const
  // Caractères européens
  TEST_EUROPEEN = 'àéèêëïôùûüÿæœçÀÉÈÊËÏÔÙÛÜŸÆŒÇ';

  // Caractères spéciaux
  TEST_SPECIAUX = '€£¥©®™§¶†‡•…‹›«»‚„"'""';

  // Caractères asiatiques
  TEST_ASIATIQUE = '中国日本한국';

  // Caractères arabes
  TEST_ARABE = 'مرحبا السلام عليكم';

  // Émojis
  TEST_EMOJIS = '😀😃😄😁😆😅🤣😂🙂🙃';

  // Caractères rares
  TEST_RARES = '𝕳𝖊𝖑𝖑𝖔 𝓦𝓸𝓻𝓵𝓭'; // Mathématiques
```

## Débogage des problèmes d'encodage

### Inspecteur d'octets

```pascal
procedure AfficherOctets(const Texte: string);  
var  
  Octets: TBytes;
  i: Integer;
  Resultat: string;
begin
  Octets := TEncoding.UTF8.GetBytes(Texte);

  Resultat := 'Octets UTF-8 : ';
  for i := 0 to Length(Octets) - 1 do
    Resultat := Resultat + IntToHex(Octets[i], 2) + ' ';

  ShowMessage(Resultat);
end;

// Utilisation
AfficherOctets('Café');
// Affiche : "43 61 66 C3 A9"
//            C  a  f  é(2 octets)
```

### Comparaison d'encodages

```pascal
procedure ComparerEncodages(const Texte: string);  
var  
  UTF8, UTF16, ANSI: TBytes;
begin
  UTF8 := TEncoding.UTF8.GetBytes(Texte);
  UTF16 := TEncoding.Unicode.GetBytes(Texte);
  ANSI := TEncoding.ANSI.GetBytes(Texte);

  ShowMessage(Format(
    'Texte: "%s"'#13#10 +
    'UTF-8: %d octets'#13#10 +
    'UTF-16: %d octets'#13#10 +
    'ANSI: %d octets',
    [Texte, Length(UTF8), Length(UTF16), Length(ANSI)]
  ));
end;

// Test
ComparerEncodages('Hello');    // ASCII simple  
ComparerEncodages('Café');     // Avec accent  
ComparerEncodages('中国');     // Caractères chinois  
ComparerEncodages('😀');       // Emoji  
```

## Conclusion

Le support Unicode dans Delphi moderne est excellent et transparent pour le développeur. En suivant quelques règles simples, vous pouvez créer des applications qui fonctionnent parfaitement avec toutes les langues du monde.

**Points clés à retenir :**

- **Unicode** : Un système universel pour tous les caractères
- **UTF-8** : L'encodage standard pour les fichiers et le Web
- **UTF-16** : L'encodage interne de Delphi et Windows
- **string** : Utiliser toujours `string` (= UnicodeString) dans Delphi moderne
- **TEncoding** : Classe puissante pour gérer tous les encodages
- **BOM** : Recommandé pour faciliter la détection automatique
- **Spécifier l'encodage** : Toujours lors de la lecture/écriture de fichiers

Avec ces connaissances, vous êtes maintenant équipé pour gérer correctement l'Unicode et les encodages dans vos applications Delphi internationales !

⏭️ [Gestion des écritures bidirectionnelles (RTL)](/13-internationalisation-et-localisation/07-gestion-des-ecritures-bidirectionnelles.md)
