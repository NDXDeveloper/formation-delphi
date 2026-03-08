🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.1 Appels aux DLLs

## Introduction aux DLLs

### Qu'est-ce qu'une DLL ?

Une **DLL** (Dynamic Link Library, ou bibliothèque de liens dynamiques) est un fichier qui contient du code et des données pouvant être utilisés par plusieurs programmes simultanément. C'est comme une boîte à outils partagée : au lieu que chaque programme ait sa propre copie des mêmes outils, tous peuvent utiliser la même boîte.

Les DLLs ont l'extension `.dll` sous Windows (par exemple : `user32.dll`, `kernel32.dll`).

### Pourquoi utiliser des DLLs ?

Les DLLs présentent plusieurs avantages :

**Réutilisation du code** : Une même fonctionnalité peut être utilisée par plusieurs applications sans dupliquer le code.

**Économie de mémoire** : Plusieurs programmes peuvent partager une seule copie de la DLL en mémoire.

**Mise à jour facilitée** : On peut mettre à jour une DLL sans recompiler toutes les applications qui l'utilisent.

**Modularité** : Le code peut être organisé en modules logiques indépendants.

**Accès aux fonctionnalités système** : Windows expose beaucoup de ses fonctionnalités via des DLLs.

### Types d'appels de DLL

Il existe deux méthodes principales pour appeler des fonctions dans une DLL :

1. **Liaison statique** : La DLL est chargée au démarrage du programme
2. **Liaison dynamique** : La DLL est chargée uniquement quand on en a besoin

## Liaison statique avec une DLL

### Déclaration d'une fonction externe

Pour utiliser une fonction d'une DLL, vous devez d'abord la déclarer dans votre code Delphi. La syntaxe de base est :

```pascal
function NomDeLaFonction(parametres): TypeRetour; stdcall; external 'NomDLL.dll';
```

### Exemple simple : MessageBox de Windows

Voici comment appeler la fonction MessageBox de Windows, qui affiche une boîte de dialogue :

```pascal
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: UINT): Integer;
  stdcall; external 'user32.dll' name 'MessageBoxW';
```

**Explication des éléments :**

- `function MessageBox` : nom de la fonction dans votre code
- `(hWnd: HWND; ...)` : paramètres de la fonction
- `Integer` : type de retour
- `stdcall` : convention d'appel (comment les paramètres sont passés)
- `external 'user32.dll'` : indique que la fonction se trouve dans user32.dll
- `name 'MessageBoxW'` : nom réel de la fonction Unicode dans la DLL (suffixe W pour Wide/Unicode, A pour ANSI)

### Utilisation de la fonction

Une fois déclarée, vous pouvez utiliser la fonction comme n'importe quelle autre fonction Delphi :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  MessageBox(0, 'Ceci est un message', 'Titre', MB_OK);
end;
```

### Conventions d'appel

Les conventions d'appel définissent comment les paramètres sont passés à la fonction. Les plus courantes sont :

**stdcall** : Convention standard de Windows, utilisée par la plupart des DLLs Windows.

**cdecl** : Convention utilisée par les DLLs C/C++.

**pascal** : Ancienne convention, rarement utilisée aujourd'hui.

**register** : Convention Delphi par défaut (pas pour les DLLs externes).

Il est crucial d'utiliser la bonne convention, sinon votre programme peut planter.

## Liaison dynamique avec une DLL

### Pourquoi la liaison dynamique ?

La liaison dynamique est utile quand :

- La DLL n'est pas toujours disponible
- Vous voulez charger la DLL seulement si nécessaire
- Vous voulez gérer l'absence de la DLL de manière élégante

### Étapes de la liaison dynamique

La liaison dynamique se fait en trois étapes :

1. **Charger la DLL** avec `LoadLibrary`
2. **Obtenir l'adresse de la fonction** avec `GetProcAddress`
3. **Libérer la DLL** avec `FreeLibrary` quand on a fini

### Exemple complet

```pascal
type
  TMessageBoxFunc = function(hWnd: HWND; lpText, lpCaption: PChar;
    uType: UINT): Integer; stdcall;

procedure TForm1.Button2Click(Sender: TObject);  
var  
  DLLHandle: THandle;
  MessageBoxFunc: TMessageBoxFunc;
begin
  // 1. Charger la DLL
  DLLHandle := LoadLibrary('user32.dll');

  if DLLHandle = 0 then
  begin
    ShowMessage('Impossible de charger la DLL');
    Exit;
  end;

  try
    // 2. Obtenir l'adresse de la fonction
    @MessageBoxFunc := GetProcAddress(DLLHandle, 'MessageBoxW');

    if @MessageBoxFunc = nil then
    begin
      ShowMessage('Fonction introuvable dans la DLL');
      Exit;
    end;

    // 3. Utiliser la fonction
    MessageBoxFunc(0, 'Message dynamique', 'Titre', MB_OK);

  finally
    // 4. Libérer la DLL
    FreeLibrary(DLLHandle);
  end;
end;
```

**Explication détaillée :**

1. On définit un type de fonction (`TMessageBoxFunc`) qui correspond à la signature de la fonction dans la DLL
2. `LoadLibrary` charge la DLL et retourne un handle (identificateur)
3. `GetProcAddress` retourne l'adresse de la fonction dans la DLL
4. On utilise `@` pour obtenir l'adresse de notre variable de fonction
5. Le bloc `try...finally` garantit que la DLL sera libérée même en cas d'erreur

## Types de données et paramètres

### Correspondance des types

Lors de l'appel de DLLs, il est important de faire correspondre correctement les types :

**Types numériques :**
- `Integer` → `int` (C/C++)
- `Cardinal` → `unsigned int`
- `Int64` → `__int64`
- `Single` → `float`
- `Double` → `double`

**Chaînes de caractères :**
- `PChar` → `char*` (chaîne C)
- `PWideChar` → `wchar_t*` (chaîne Unicode)
- `PAnsiChar` → `char*` (chaîne ANSI)

**Pointeurs :**
- `Pointer` → `void*`
- `HWND`, `HANDLE` → handles Windows

### Passage de chaînes

Les chaînes nécessitent une attention particulière :

```pascal
// Pour une fonction qui attend une chaîne ANSI
function MaFonction(texte: PAnsiChar): Integer;
  stdcall; external 'madll.dll';

procedure Exemple;  
var  
  chaine: AnsiString;
begin
  chaine := 'Mon texte';
  MaFonction(PAnsiChar(chaine));
end;
```

### Passage de structures

Pour passer des structures (records) à une DLL :

```pascal
type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

function CalculerDistance(const point1, point2: TPoint): Double;
  stdcall; external 'geometrie.dll';

procedure Utilisation;  
var  
  p1, p2: TPoint;
  distance: Double;
begin
  p1.X := 10;
  p1.Y := 20;
  p2.X := 30;
  p2.Y := 40;

  distance := CalculerDistance(p1, p2);
end;
```

## DLLs Windows courantes

### user32.dll

Contient les fonctions d'interface utilisateur Windows :

```pascal
// Obtenir la position de la souris
function GetCursorPos(var lpPoint: TPoint): BOOL;
  stdcall; external 'user32.dll';

// Masquer/afficher le curseur
function ShowCursor(bShow: BOOL): Integer;
  stdcall; external 'user32.dll';
```

### kernel32.dll

Contient les fonctions système de base :

```pascal
// Obtenir le nom de l'ordinateur
function GetComputerName(lpBuffer: PChar; var nSize: DWORD): BOOL;
  stdcall; external 'kernel32.dll' name 'GetComputerNameW';

// Mettre en pause l'exécution
procedure Sleep(dwMilliseconds: DWORD);
  stdcall; external 'kernel32.dll';
```

### shell32.dll

Contient les fonctions du shell Windows :

```pascal
// Ouvrir un fichier avec l'application associée
function ShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): HINST;
  stdcall; external 'shell32.dll' name 'ShellExecuteW';
```

## Gestion des erreurs

### Vérification du chargement

Toujours vérifier que le chargement de la DLL a réussi :

```pascal
var
  DLLHandle: THandle;
begin
  DLLHandle := LoadLibrary('madll.dll');

  if DLLHandle = 0 then
  begin
    ShowMessage('Erreur : ' + SysErrorMessage(GetLastError));
    Exit;
  end;

  try
    // Utilisation de la DLL
  finally
    FreeLibrary(DLLHandle);
  end;
end;
```

### Gestion des exceptions

Protégez vos appels de DLL avec des blocs try-except :

```pascal
procedure AppelerDLL;  
begin  
  try
    // Appel de fonction DLL
    Resultat := MaFonctionDLL(param1, param2);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l appel DLL: ' + E.Message);
  end;
end;
```

## Bonnes pratiques

### Encapsulation

Créez une classe ou une unité pour encapsuler les appels à la DLL :

```pascal
unit MaDLLWrapper;

interface

type
  TMaDLLWrapper = class
  private
    FDLLHandle: THandle;
    FChargee: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ChargerDLL: Boolean;
    procedure DechargerDLL;
    function AppelerFonction(param: Integer): Integer;
  end;

implementation

constructor TMaDLLWrapper.Create;  
begin  
  inherited;
  FDLLHandle := 0;
  FChargee := False;
end;

destructor TMaDLLWrapper.Destroy;  
begin  
  if FChargee then
    DechargerDLL;
  inherited;
end;

function TMaDLLWrapper.ChargerDLL: Boolean;  
begin  
  FDLLHandle := LoadLibrary('madll.dll');
  FChargee := FDLLHandle <> 0;
  Result := FChargee;
end;

procedure TMaDLLWrapper.DechargerDLL;  
begin  
  if FDLLHandle <> 0 then
  begin
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    FChargee := False;
  end;
end;

end.
```

### Vérification de la présence

Avant de charger une DLL, vérifiez qu'elle existe :

```pascal
uses
  System.SysUtils;

function DLLExiste(const NomDLL: string): Boolean;  
begin  
  Result := FileExists(NomDLL);
end;

procedure ChargerMaDLL;  
const  
  NOM_DLL = 'madll.dll';
begin
  if not DLLExiste(NOM_DLL) then
  begin
    ShowMessage('La DLL ' + NOM_DLL + ' est introuvable');
    Exit;
  end;

  // Charger la DLL...
end;
```

### Documentation

Documentez toujours vos déclarations de DLL :

```pascal
/// <summary>
/// Affiche une boîte de message Windows standard
/// </summary>
/// <param name="hWnd">Handle de la fenêtre parente (0 pour aucune)</param>
/// <param name="lpText">Texte du message</param>
/// <param name="lpCaption">Titre de la boîte de dialogue</param>
/// <param name="uType">Type de boîte (MB_OK, MB_YESNO, etc.)</param>
/// <returns>ID du bouton cliqué</returns>
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: UINT): Integer; stdcall; external 'user32.dll' name 'MessageBoxW';
```

## Compatibilité 32-bit et 64-bit

### Différences importantes

Les DLLs 32-bit ne peuvent pas être chargées par des applications 64-bit et vice-versa. Vous devez :

1. Compiler votre application en 32-bit pour utiliser des DLLs 32-bit
2. Compiler votre application en 64-bit pour utiliser des DLLs 64-bit

### Chargement conditionnel

Pour gérer les deux architectures :

```pascal
function ChargerDLLCompatible: THandle;  
begin  
  {$IFDEF WIN64}
  Result := LoadLibrary('madll64.dll');
  {$ELSE}
  Result := LoadLibrary('madll32.dll');
  {$ENDIF}
end;
```

## Résumé

Les DLLs sont un outil puissant pour :
- Réutiliser du code existant
- Accéder aux fonctionnalités système Windows
- Intégrer des bibliothèques tierces
- Modulariser votre application

**Points clés à retenir :**

1. Utilisez la **liaison statique** pour les DLLs toujours présentes
2. Utilisez la **liaison dynamique** pour les DLLs optionnelles
3. Respectez la **convention d'appel** (stdcall, cdecl, etc.)
4. Faites correspondre correctement les **types de données**
5. Gérez toujours les **erreurs** de chargement
6. **Libérez** les DLLs chargées dynamiquement
7. Attention à la **compatibilité 32/64-bit**

Dans la section suivante, nous verrons comment intégrer des bibliothèques C/C++ plus complexes.

⏭️ [Intégration de bibliothèques C/C++](/14-utilisation-dapi-et-bibliotheques-externes/02-integration-de-bibliotheques-c-cpp.md)
