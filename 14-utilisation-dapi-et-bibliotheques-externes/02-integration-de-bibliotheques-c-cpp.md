🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.2 Intégration de bibliothèques C/C++

## Introduction

### Pourquoi intégrer des bibliothèques C/C++ ?

Le monde du développement C/C++ possède un vaste écosystème de bibliothèques pour tous les domaines : traitement d'images, intelligence artificielle, cryptographie, compression, mathématiques avancées, etc. Pouvoir utiliser ces bibliothèques dans vos applications Delphi vous ouvre de nombreuses possibilités.

### Différence avec les DLLs Windows

Contrairement aux DLLs Windows standard que nous avons vues précédemment, les bibliothèques C/C++ :

- Utilisent des conventions de nommage différentes
- Peuvent avoir des structures de données complexes
- Utilisent souvent la convention d'appel `cdecl` au lieu de `stdcall`
- Nécessitent parfois la traduction de fichiers d'en-tête (.h)
- Peuvent manipuler des pointeurs de manière intensive

## Concepts fondamentaux

### Conventions d'appel

La plupart des bibliothèques C/C++ utilisent la convention **cdecl** :

```pascal
// Convention Windows (DLLs système)
function FonctionWindows(param: Integer): Integer; stdcall; external 'ma.dll';

// Convention C/C++ (bibliothèques tierces)
function FonctionC(param: Integer): Integer; cdecl; external 'ma.dll';
```

La différence principale : avec `cdecl`, c'est l'appelant qui nettoie la pile, pas la fonction appelée. Cela permet les fonctions avec un nombre variable de paramètres (comme `printf` en C).

### Name Mangling (décoration de noms)

En C++, les noms de fonctions sont "décorés" pour inclure des informations sur les types de paramètres. Par exemple, une fonction C++ `int Add(int a, int b)` pourrait devenir `?Add@@YAHHH@Z` dans la DLL.

Pour éviter ce problème, les bibliothèques C/C++ bien conçues exposent leurs fonctions en C pur avec le mot-clé `extern "C"` :

```cpp
// Dans la bibliothèque C++
extern "C" {
    int __cdecl Add(int a, int b);
}
```

Cela produit un nom simple `Add` que Delphi peut utiliser facilement.

### Vérifier les noms exportés

Pour connaître les noms réels des fonctions dans une DLL, utilisez des outils comme :

- **Dependency Walker** (depends.exe)
- **DLL Export Viewer** de NirSoft
- **dumpbin** (fourni avec Visual Studio)

Ces outils vous montreront les noms exacts des fonctions exportées.

## Conversion des types de données

### Types numériques de base

Correspondance entre C/C++ et Delphi :

```pascal
// Types entiers
type
  c_char = AnsiChar;          // char (C)
  c_int = Integer;            // int (C)
  c_uint = Cardinal;          // unsigned int (C)
  c_long = Longint;           // long (C, 32-bit)
  c_ulong = LongWord;         // unsigned long (C, 32-bit)
  c_int64 = Int64;            // long long (C)
  c_uint64 = UInt64;          // unsigned long long (C)

// Types à virgule flottante
  c_float = Single;           // float (C)
  c_double = Double;          // double (C)

// Types de taille
  size_t = NativeUInt;        // size_t (C)
```

### Pointeurs

Les pointeurs C/C++ se traduisent par des pointeurs Delphi :

```pascal
// En C: int* ptr
var
  ptr: PInteger;  // En Delphi

// En C: void* ptr
var
  ptr: Pointer;   // En Delphi

// En C: char* str
var
  str: PAnsiChar; // En Delphi
```

### Chaînes de caractères

Les chaînes C sont des tableaux de caractères terminés par un zéro :

```pascal
// Fonction C qui attend une chaîne
function StrLen(s: PAnsiChar): size_t; cdecl; external 'msvcrt.dll';

// Utilisation
procedure Exemple;
var
  texte: AnsiString;
  longueur: NativeUInt;
begin
  texte := 'Bonjour';
  longueur := StrLen(PAnsiChar(texte));
  ShowMessage('Longueur: ' + IntToStr(longueur));
end;
```

**Important :** Les chaînes Delphi (String) doivent être converties en PAnsiChar ou PWideChar avant d'être passées aux fonctions C.

### Structures (struct)

Les structures C se traduisent par des records Delphi :

```pascal
// En C:
// struct Point {
//     int x;
//     int y;
// };

// En Delphi:
type
  TPoint = record
    x: Integer;
    y: Integer;
  end;
```

**Attention à l'alignement :** Les structures C peuvent avoir un alignement différent. Utilisez la directive `{$ALIGN}` si nécessaire :

```pascal
type
  {$ALIGN 1}  // Alignement sur 1 octet (packed)
  TStructurePacked = record
    a: Byte;
    b: Integer;
  end;
  {$ALIGN ON}  // Retour à l'alignement par défaut
```

### Tableaux

Les tableaux C se traduisent de différentes manières :

```pascal
// Tableau de taille fixe
// En C: int array[10];
type
  TArrayInt10 = array[0..9] of Integer;

// Tableau dynamique (pointeur)
// En C: int* array;
type
  PIntArray = ^Integer;  // Ou utiliser un tableau dynamique Delphi
```

### Énumérations

Les énumérations C deviennent des énumérations ou constantes Delphi :

```pascal
// En C:
// enum Color { RED, GREEN, BLUE };

// En Delphi (énumération):
type
  TColor = (RED, GREEN, BLUE);

// Ou en constantes:
const
  RED = 0;
  GREEN = 1;
  BLUE = 2;
```

## Exemple pratique : Bibliothèque zlib

La bibliothèque zlib est une bibliothèque C populaire pour la compression de données.

### Déclaration des fonctions

```pascal
unit ZLibWrapper;

interface

const
  ZLIB_DLL = 'zlib1.dll';

type
  z_stream = record
    next_in: PAnsiChar;      // Données en entrée
    avail_in: Cardinal;      // Taille disponible en entrée
    total_in: Cardinal;      // Total traité en entrée

    next_out: PAnsiChar;     // Données en sortie
    avail_out: Cardinal;     // Taille disponible en sortie
    total_out: Cardinal;     // Total produit en sortie

    msg: PAnsiChar;          // Message d'erreur
    state: Pointer;          // État interne

    // Champs additionnels...
    zalloc: Pointer;
    zfree: Pointer;
    opaque: Pointer;
  end;

// Constantes zlib
const
  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -1;
  Z_STREAM_ERROR = -2;
  Z_DATA_ERROR = -3;
  Z_MEM_ERROR = -4;
  Z_BUF_ERROR = -5;
  Z_VERSION_ERROR = -6;

// Niveaux de compression
const
  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -1;

// Déclaration des fonctions
function deflateInit_(strm: Pointer; level: Integer;
  version: PAnsiChar; stream_size: Integer): Integer;
  cdecl; external ZLIB_DLL;

function deflate(strm: Pointer; flush: Integer): Integer;
  cdecl; external ZLIB_DLL;

function deflateEnd(strm: Pointer): Integer;
  cdecl; external ZLIB_DLL;

implementation

end.
```

### Utilisation de la bibliothèque

```pascal
uses
  ZLibWrapper;

function CompresserDonnees(const Donnees: AnsiString): AnsiString;
var
  strm: z_stream;
  ret: Integer;
  sortie: AnsiString;
begin
  // Initialiser la structure
  FillChar(strm, SizeOf(strm), 0);

  // Préparer les données d'entrée
  strm.next_in := PAnsiChar(Donnees);
  strm.avail_in := Length(Donnees);

  // Préparer le buffer de sortie
  SetLength(sortie, Length(Donnees));
  strm.next_out := PAnsiChar(sortie);
  strm.avail_out := Length(sortie);

  // Initialiser la compression
  ret := deflateInit_(
    @strm,
    Z_DEFAULT_COMPRESSION,
    PAnsiChar(AnsiString('1.2.11')),  // Version de zlib
    SizeOf(z_stream)
  );

  if ret <> Z_OK then
  begin
    Result := '';
    Exit;
  end;

  try
    // Compresser
    ret := deflate(@strm, Z_FINISH);

    if ret = Z_STREAM_END then
    begin
      // Ajuster la taille du résultat
      SetLength(sortie, strm.total_out);
      Result := sortie;
    end
    else
      Result := '';

  finally
    // Libérer les ressources
    deflateEnd(@strm);
  end;
end;
```

## Gestion de la mémoire

### Allocation mémoire côté C

Certaines bibliothèques C allouent de la mémoire qu'il faut libérer :

```pascal
// Fonction C qui alloue de la mémoire
function CreerObjet: Pointer; cdecl; external 'ma.dll';

// Fonction C qui libère la mémoire
procedure DetruireObjet(obj: Pointer); cdecl; external 'ma.dll';

// Utilisation
procedure Exemple;
var
  obj: Pointer;
begin
  obj := CreerObjet;
  try
    // Utiliser l'objet...
  finally
    DetruireObjet(obj);  // TOUJOURS libérer !
  end;
end;
```

### Allocation mémoire côté Delphi

Quand vous passez de la mémoire allouée par Delphi à une fonction C :

```pascal
function RemplirBuffer(buffer: PAnsiChar; taille: Integer): Integer;
  cdecl; external 'ma.dll';

procedure Exemple;
var
  buffer: PAnsiChar;
  taille: Integer;
begin
  taille := 1024;
  GetMem(buffer, taille);  // Allocation
  try
    RemplirBuffer(buffer, taille);
    // Utiliser le buffer...
  finally
    FreeMem(buffer);  // Libération
  end;
end;
```

## Callbacks (fonctions de rappel)

Certaines bibliothèques C utilisent des callbacks, c'est-à-dire des fonctions que vous fournissez et qui seront appelées par la bibliothèque.

### Déclaration d'un callback

```pascal
// Type de callback
type
  TMonCallback = function(valeur: Integer; userData: Pointer): Integer; cdecl;

// Fonction C qui prend un callback
procedure TraiterAvecCallback(callback: TMonCallback; userData: Pointer);
  cdecl; external 'ma.dll';
```

### Implémentation du callback

```pascal
// Le callback DOIT utiliser la convention cdecl
function MonCallbackImpl(valeur: Integer; userData: Pointer): Integer; cdecl;
begin
  ShowMessage('Valeur reçue: ' + IntToStr(valeur));
  Result := 0;
end;

// Utilisation
procedure Exemple;
begin
  TraiterAvecCallback(@MonCallbackImpl, nil);
end;
```

**Important :** Le callback doit être une fonction globale ou une méthode statique de classe, pas une méthode d'instance classique.

### Callback avec méthode de classe

Pour utiliser une méthode d'instance comme callback, il faut une astuce :

```pascal
type
  TMonObjet = class
  private
    FValeur: Integer;
    // Méthode statique qui fait le pont
    class procedure CallbackStatique(valeur: Integer; userData: Pointer); cdecl; static;
  public
    procedure TraiterCallback(valeur: Integer);
    procedure Executer;
  end;

class procedure TMonObjet.CallbackStatique(valeur: Integer; userData: Pointer); cdecl;
var
  Instance: TMonObjet;
begin
  Instance := TMonObjet(userData);
  Instance.TraiterCallback(valeur);
end;

procedure TMonObjet.TraiterCallback(valeur: Integer);
begin
  FValeur := valeur;
  ShowMessage('Traitement: ' + IntToStr(FValeur));
end;

procedure TMonObjet.Executer;
begin
  // Passer Self comme userData
  TraiterAvecCallback(@TMonObjet.CallbackStatique, Self);
end;
```

## Gestion des fichiers d'en-tête (.h)

### Traduction manuelle

Les fichiers d'en-tête C (.h) définissent les structures et fonctions. Il faut les traduire en Delphi :

```c
// Fichier C: exemple.h
#define MAX_SIZE 100

typedef struct {
    int id;
    char name[50];
} Person;

int CreatePerson(Person* p, int id, const char* name);
```

Devient en Delphi :

```pascal
// Traduction Delphi
const
  MAX_SIZE = 100;

type
  TPerson = record
    id: Integer;
    name: array[0..49] of AnsiChar;
  end;
  PPerson = ^TPerson;

function CreatePerson(p: PPerson; id: Integer; name: PAnsiChar): Integer;
  cdecl; external 'exemple.dll';
```

### Outils d'aide à la traduction

Plusieurs outils peuvent vous aider :

- **h2pas** : Outil en ligne de commande pour convertir .h en .pas
- **C to Delphi converter** : Outils en ligne
- **Recherche de traductions existantes** : Beaucoup de bibliothèques populaires ont déjà des en-têtes Delphi

## Exemple complet : SQLite

SQLite est une base de données C très populaire. Voyons comment l'intégrer :

### Déclarations

```pascal
unit SQLite3;

interface

const
  SQLITE_DLL = 'sqlite3.dll';

type
  TSQLite3 = Pointer;
  TSQLite3Stmt = Pointer;

// Codes de retour
const
  SQLITE_OK = 0;
  SQLITE_ERROR = 1;
  SQLITE_ROW = 100;
  SQLITE_DONE = 101;

// Fonctions principales
function sqlite3_open(filename: PAnsiChar; var db: TSQLite3): Integer;
  cdecl; external SQLITE_DLL;

function sqlite3_close(db: TSQLite3): Integer;
  cdecl; external SQLITE_DLL;

function sqlite3_prepare_v2(db: TSQLite3; sql: PAnsiChar; nByte: Integer;
  var stmt: TSQLite3Stmt; var pzTail: PAnsiChar): Integer;
  cdecl; external SQLITE_DLL;

function sqlite3_step(stmt: TSQLite3Stmt): Integer;
  cdecl; external SQLITE_DLL;

function sqlite3_finalize(stmt: TSQLite3Stmt): Integer;
  cdecl; external SQLITE_DLL;

function sqlite3_column_text(stmt: TSQLite3Stmt; iCol: Integer): PAnsiChar;
  cdecl; external SQLITE_DLL;

function sqlite3_errmsg(db: TSQLite3): PAnsiChar;
  cdecl; external SQLITE_DLL;

implementation

end.
```

### Utilisation

```pascal
uses
  SQLite3;

procedure ExempleUtilisation;
var
  db: TSQLite3;
  stmt: TSQLite3Stmt;
  ret: Integer;
  texte: string;
begin
  // Ouvrir la base de données
  ret := sqlite3_open(PAnsiChar(AnsiString('test.db')), db);
  if ret <> SQLITE_OK then
  begin
    ShowMessage('Erreur ouverture');
    Exit;
  end;

  try
    // Préparer une requête
    ret := sqlite3_prepare_v2(
      db,
      PAnsiChar(AnsiString('SELECT * FROM users')),
      -1,
      stmt,
      nil
    );

    if ret <> SQLITE_OK then
    begin
      ShowMessage('Erreur préparation: ' +
        string(sqlite3_errmsg(db)));
      Exit;
    end;

    try
      // Exécuter et lire les résultats
      while sqlite3_step(stmt) = SQLITE_ROW do
      begin
        texte := string(AnsiString(sqlite3_column_text(stmt, 0)));
        ShowMessage('Résultat: ' + texte);
      end;
    finally
      sqlite3_finalize(stmt);
    end;

  finally
    sqlite3_close(db);
  end;
end;
```

## Débogage

### Problèmes courants

**Violation d'accès :** Souvent causée par :
- Mauvaise convention d'appel (cdecl vs stdcall)
- Types de paramètres incorrects
- Pointeurs invalides
- Mauvais alignement de structures

**DLL introuvable :**
- Vérifiez que la DLL est dans le même dossier que l'exécutable
- Ou dans le PATH système
- Utilisez un chemin complet si nécessaire

**Fonction introuvable :**
- Vérifiez le nom exact avec Dependency Walker
- Attention au name mangling en C++

### Techniques de débogage

**Vérification pas à pas :**

```pascal
procedure DebugAppelC;
var
  handle: THandle;
  proc: Pointer;
begin
  // 1. Vérifier que la DLL existe
  if not FileExists('ma.dll') then
  begin
    ShowMessage('DLL introuvable');
    Exit;
  end;

  // 2. Charger la DLL
  handle := LoadLibrary('ma.dll');
  if handle = 0 then
  begin
    ShowMessage('Échec chargement DLL: ' +
      SysErrorMessage(GetLastError));
    Exit;
  end;

  try
    // 3. Chercher la fonction
    proc := GetProcAddress(handle, 'MaFonction');
    if proc = nil then
    begin
      ShowMessage('Fonction introuvable');
      Exit;
    end;

    ShowMessage('Tout est OK !');

  finally
    FreeLibrary(handle);
  end;
end;
```

## Bonnes pratiques

### Encapsulation dans une classe

Créez une classe wrapper pour faciliter l'utilisation :

```pascal
type
  TSQLiteDatabase = class
  private
    FHandle: TSQLite3;
    FConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const FileName: string): Boolean;
    procedure Disconnect;
    function Execute(const SQL: string): Boolean;
    function Query(const SQL: string): TStringList;
    property Connected: Boolean read FConnected;
  end;
```

### Gestion centralisée des erreurs

```pascal
type
  ESQLiteError = class(Exception);

procedure CheckSQLiteResult(db: TSQLite3; ret: Integer);
begin
  if ret <> SQLITE_OK then
    raise ESQLiteError.Create(
      'Erreur SQLite: ' + string(sqlite3_errmsg(db))
    );
end;
```

### Documentation

Documentez les spécificités de chaque bibliothèque :

```pascal
/// <summary>
/// Wrapper pour la bibliothèque SQLite3
/// </summary>
/// <remarks>
/// Nécessite sqlite3.dll (version 3.36+)
/// Convention d'appel: cdecl
/// Thread-safe: Non (utiliser un mutex)
/// </remarks>
unit SQLite3Wrapper;
```

### Tests unitaires

Testez vos wrappers de bibliothèques :

```pascal
procedure TTestSQLite.TestConnection;
var
  db: TSQLiteDatabase;
begin
  db := TSQLiteDatabase.Create;
  try
    CheckTrue(db.Connect('test.db'), 'Échec connexion');
    CheckTrue(db.Connected, 'Pas connecté');
  finally
    db.Free;
  end;
end;
```

## Compatibilité multi-plateforme

Si vous développez pour plusieurs plateformes (Windows, macOS, Linux), considérez :

```pascal
const
  {$IFDEF MSWINDOWS}
  SQLITE_LIB = 'sqlite3.dll';
  {$ENDIF}
  {$IFDEF MACOS}
  SQLITE_LIB = 'libsqlite3.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  SQLITE_LIB = 'libsqlite3.so';
  {$ENDIF}
```

## Résumé

L'intégration de bibliothèques C/C++ dans Delphi permet d'accéder à un vaste écosystème de fonctionnalités.

**Points clés :**

1. Utilisez la convention d'appel **cdecl** (pas stdcall)
2. Traduisez correctement les **types de données**
3. Faites attention à l'**alignement des structures**
4. Gérez correctement la **mémoire** (qui alloue, qui libère)
5. Vérifiez les **noms exportés** avec des outils appropriés
6. **Encapsulez** les appels dans des classes Delphi
7. **Testez** minutieusement chaque fonction
8. **Documentez** les spécificités de chaque bibliothèque
9. Attention au **name mangling** en C++
10. Gérez les **callbacks** correctement

Avec ces connaissances, vous pouvez maintenant intégrer pratiquement n'importe quelle bibliothèque C/C++ dans vos applications Delphi !

⏭️ [API Windows natif](/14-utilisation-dapi-et-bibliotheques-externes/03-api-windows-natif.md)
