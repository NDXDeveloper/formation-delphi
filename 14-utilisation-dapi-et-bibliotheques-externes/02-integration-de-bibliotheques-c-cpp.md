# 14.2 Intégration de bibliothèques C/C++

## Introduction

Delphi est un environnement de développement très puissant, mais parfois vous aurez besoin d'utiliser des fonctionnalités développées en C ou C++. Cela peut être nécessaire pour :

- Utiliser des bibliothèques populaires disponibles uniquement en C/C++
- Intégrer du code existant écrit dans ces langages
- Tirer parti des performances optimisées de certaines bibliothèques C/C++
- Accéder à des API système de bas niveau

Ce chapitre vous guidera dans l'intégration de bibliothèques C/C++ à vos projets Delphi, en expliquant les différentes approches disponibles.

## Méthodes d'intégration

Il existe plusieurs façons d'intégrer des bibliothèques C/C++ dans vos projets Delphi :

1. **Utilisation via des DLLs** - La méthode la plus courante
2. **Intégration directe via des fichiers objets** - Pour une intégration plus étroite
3. **Wrappers statiques** - Via des classes d'encapsulation
4. **Création de projets mixtes** - En combinant du code Delphi et C++ Builder

Nous allons explorer ces différentes approches, en commençant par la plus simple.

## 1. Intégration via DLLs

La méthode la plus courante et la plus simple est d'utiliser les bibliothèques C/C++ compilées sous forme de DLLs.

### Étapes de base

1. **Obtenir ou compiler la DLL C/C++**
2. **Créer des déclarations d'imports pour les fonctions**
3. **Appeler les fonctions depuis votre code Delphi**

### Exemple concret

Imaginons que nous souhaitons utiliser la bibliothèque mathématique avancée `MathLib.dll` écrite en C. Elle contient une fonction pour calculer la factorielle :

```cpp
// Définition C++ dans MathLib.cpp
extern "C" __declspec(dllexport) long long CalculateFactorial(int number) {
    if (number <= 1) return 1;
    return number * CalculateFactorial(number - 1);
}
```

Voici comment l'intégrer dans Delphi :

```pascal
// Déclaration d'import dans Delphi
function CalculateFactorial(Number: Integer): Int64; stdcall; external 'MathLib.dll';

procedure TForm1.CalculateButtonClick(Sender: TObject);
var
  Number, Result: Int64;
begin
  Number := StrToIntDef(InputBox('Factorielle', 'Entrez un nombre:', '5'), 5);

  try
    Result := CalculateFactorial(Number);
    ShowMessage('Factorielle de ' + IntToStr(Number) + ' = ' + IntToStr(Result));
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

### Gestion des conventions d'appel

Les bibliothèques C/C++ peuvent utiliser différentes conventions d'appel :

- **cdecl** - Convention d'appel C standard
- **stdcall** - Convention d'appel standard Win32 API
- **fastcall** - Optimisée pour la vitesse (moins courante)

Dans votre déclaration d'import, vous devez préciser la bonne convention :

```pascal
// Pour une fonction 'cdecl'
function MaFonctionC(Param: Integer): Integer; cdecl; external 'MaBibliotheque.dll';

// Pour une fonction 'stdcall'
function MaFonctionStd(Param: Integer): Integer; stdcall; external 'MaBibliotheque.dll';
```

### Différence de nommage

En C/C++, les noms de fonctions sont souvent modifiés par le compilateur (name mangling). Pour éviter ce problème, les fonctions sont généralement déclarées avec `extern "C"` dans le code C++, ce qui préserve leurs noms.

Si la fonction a un nom modifié ou différent dans la DLL, utilisez la clause `name` :

```pascal
function MaFonction(Param1, Param2: Integer): Integer; stdcall;
  external 'MaBibliotheque.dll' name '_MaFonction@8';
```

## 2. Gestion des types de données

La correspondance entre les types de données Delphi et C/C++ est cruciale pour une intégration réussie.

### Types simples

| Type Delphi     | Type C/C++          | Taille (bits) |
|-----------------|---------------------|--------------|
| Byte            | unsigned char       | 8            |
| ShortInt        | signed char         | 8            |
| Word            | unsigned short      | 16           |
| SmallInt        | short               | 16           |
| Cardinal        | unsigned int        | 32           |
| Integer         | int                 | 32           |
| Int64           | long long           | 64           |
| UInt64          | unsigned long long  | 64           |
| Single          | float               | 32           |
| Double          | double              | 64           |
| Boolean         | bool                | 8            |
| Char            | char                | 8            |
| WideChar        | wchar_t             | 16           |

### Structures et types complexes

Pour les structures, vous devez déclarer une structure équivalente en Delphi :

```cpp
// Structure C++
struct Rectangle {
    int x, y;
    int width, height;
};
```

```pascal
// Structure Delphi équivalente
type
  TRectangle = record
    x, y: Integer;
    width, height: Integer;
  end;
```

### Pointeurs et tableaux

Les pointeurs en C/C++ correspondent généralement aux pointeurs en Delphi :

```cpp
// Fonction C qui manipule un tableau via pointeur
void ProcessArray(int* data, int length);
```

```pascal
// Déclaration Delphi
procedure ProcessArray(data: PInteger; length: Integer); cdecl; external 'MaLib.dll';

// Utilisation
var
  MyArray: array[0..9] of Integer;
  i: Integer;
begin
  for i := 0 to 9 do
    MyArray[i] := i * 10;

  ProcessArray(@MyArray[0], 10);
end;
```

### Chaînes de caractères

Le passage de chaînes nécessite une attention particulière :

```pascal
// Fonction C qui attend une chaîne de caractères
function ProcessString(text: PAnsiChar): Integer; cdecl; external 'MaLib.dll';

// Pour chaînes ANSI
var
  s: AnsiString;
begin
  s := 'Test';
  ProcessString(PAnsiChar(s));
end;

// Pour chaînes Unicode (avec conversion)
var
  s: string;  // Unicode par défaut dans Delphi moderne
begin
  s := 'Test Unicode';
  ProcessString(PAnsiChar(AnsiString(s)));
end;
```

## 3. Utilisation de bibliothèques statiques (.lib)

Parfois, les bibliothèques C/C++ sont disponibles sous forme de fichiers `.lib` (bibliothèques statiques) plutôt que de DLLs. Delphi ne peut pas les utiliser directement, mais vous pouvez :

1. **Créer une DLL wrapper** - En C/C++, créez une DLL qui expose les fonctionnalités de la bibliothèque statique
2. **Compiler les sources** - Si vous disposez du code source, recompilez-le en DLL

### Exemple de wrapper C++

```cpp
// Wrapper.cpp
#include "BibliothequeStatique.h"

extern "C" {
    __declspec(dllexport) int WINAPI FonctionWrapper(int param) {
        return FonctionBibliothequeStatique(param);
    }
}
```

Compilez ce code en DLL, puis utilisez-le depuis Delphi comme expliqué précédemment.

## 4. Création d'une classe d'encapsulation

Pour une utilisation plus propre et sécurisée, créez une classe Delphi qui encapsule l'accès à la bibliothèque C/C++ :

```pascal
type
  TMathLibrary = class
  private
    FDLLHandle: THandle;
    FCalculateFactorial: function(Number: Integer): Int64; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function IsLoaded: Boolean;
    function Factorial(Number: Integer): Int64;
  end;

constructor TMathLibrary.Create;
begin
  inherited;
  FDLLHandle := LoadLibrary('MathLib.dll');
  if FDLLHandle <> 0 then
    @FCalculateFactorial := GetProcAddress(FDLLHandle, 'CalculateFactorial');
end;

destructor TMathLibrary.Destroy;
begin
  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);
  inherited;
end;

function TMathLibrary.IsLoaded: Boolean;
begin
  Result := (FDLLHandle <> 0) and Assigned(FCalculateFactorial);
end;

function TMathLibrary.Factorial(Number: Integer): Int64;
begin
  if IsLoaded then
    Result := FCalculateFactorial(Number)
  else
    raise Exception.Create('La bibliothèque mathématique n''est pas chargée');
end;
```

## 5. Utilisation de headers C/C++ avec HeaderConverter

Delphi inclut un outil appelé "HeaderConverter" qui peut convertir des fichiers d'en-tête C/C++ (`.h`) en unités Delphi. Cet outil est particulièrement utile pour les bibliothèques complexes.

### Utilisation du HeaderConverter

1. Dans Delphi, allez dans **Menu** → **Projet** → **Import Component** → **Import C header file**
2. Sélectionnez le fichier d'en-tête `.h` de votre bibliothèque
3. Suivez les étapes de l'assistant

Le résultat sera une unité Delphi contenant toutes les déclarations nécessaires.

## 6. Gestion des callbacks

Les bibliothèques C/C++ utilisent souvent des callbacks (fonctions de rappel). Voici comment les gérer en Delphi :

```cpp
// Définition du callback en C++
typedef void (*CallbackFunc)(int value);

// Fonction qui prend un callback
void RegisterCallback(CallbackFunc callback);
```

```pascal
// Implémentation en Delphi
type
  TCallbackFunc = procedure(Value: Integer); cdecl;

var
  OriginalCallback: TCallbackFunc;

procedure CallbackFunc(Value: Integer); cdecl;
begin
  // Traitement du callback
  ShowMessage('Callback reçu avec valeur: ' + IntToStr(Value));
end;

procedure RegisterCallback(callback: TCallbackFunc); cdecl; external 'MaLib.dll';

procedure TForm1.ButtonRegisterClick(Sender: TObject);
begin
  // Sauvegarde de la référence de la fonction pour éviter qu'elle soit libérée
  OriginalCallback := CallbackFunc;

  // Enregistrement du callback
  RegisterCallback(OriginalCallback);
end;
```

> ⚠️ **Important** : Conservez une référence globale à votre fonction de callback pour éviter qu'elle soit libérée par le garbage collector.

## 7. Exemple pratique : Intégration de OpenCV (bibliothèque de vision par ordinateur)

OpenCV est une bibliothèque de vision par ordinateur populaire écrite en C++. Voici comment l'intégrer à Delphi :

### 1. Obtenir les DLLs OpenCV

Téléchargez OpenCV et récupérez les fichiers DLL (par exemple : `opencv_world460.dll`).

### 2. Créer des déclarations pour les fonctions

```pascal
unit OpenCV_Core;

interface

uses
  System.SysUtils, System.Types, Winapi.Windows;

// Types OpenCV
type
  PCvMat = Pointer;
  PCvSize = Pointer;

// Création/destruction de matrices
function cvCreateMat(rows, cols, type_: Integer): PCvMat; cdecl; external 'opencv_world460.dll';
procedure cvReleaseMat(var mat: PCvMat); cdecl; external 'opencv_world460.dll';

// Fonction de traitement d'image
function cvCanny(src, dst: PCvMat; threshold1, threshold2: Double; aperture_size: Integer): PCvMat; cdecl;
  external 'opencv_world460.dll';

implementation

end.
```

### 3. Utiliser les fonctions dans votre application

```pascal
uses
  OpenCV_Core;

procedure TForm1.DetectEdgesButtonClick(Sender: TObject);
var
  SourceMatrix, ResultMatrix: PCvMat;
begin
  // Création des matrices
  SourceMatrix := cvCreateMat(ImageHeight, ImageWidth, CV_8UC3);
  ResultMatrix := cvCreateMat(ImageHeight, ImageWidth, CV_8UC1);

  try
    // Copie de l'image vers la matrice source (code non montré)
    // ...

    // Détection de contours avec Canny
    cvCanny(SourceMatrix, ResultMatrix, 50, 150, 3);

    // Affichage ou traitement du résultat (code non montré)
    // ...

  finally
    // Libération des ressources
    cvReleaseMat(ResultMatrix);
    cvReleaseMat(SourceMatrix);
  end;
end;
```

## 8. Projet mixte avec C++ Builder

Si vous disposez de Delphi dans la suite RAD Studio, vous pouvez créer des projets mixtes utilisant à la fois du code Delphi et C++ Builder :

1. Créez une bibliothèque de paquets Delphi (BPL)
2. Utilisez C++ Builder pour créer un package qui utilise le BPL Delphi
3. Importez vos bibliothèques C/C++ dans le package C++ Builder
4. Exposez les fonctionnalités via des classes C++ accessible depuis Delphi

Cette approche avancée est idéale pour des projets complexes nécessitant une intégration étroite des deux langages.

## Bonnes pratiques

1. **Gérez bien la libération des ressources** - Particulièrement important avec du code C/C++ qui peut allouer de la mémoire
2. **Protégez les appels** - Utilisez `try..except` pour éviter les plantages en cas d'erreur dans la bibliothèque
3. **Testez minutieusement** - Les différences de conventions d'appel et de types peuvent causer des problèmes subtils
4. **Documentez vos wrappers** - Précisez les correspondances de types et les particularités d'utilisation
5. **Validez les valeurs de retour** - Beaucoup de fonctions C renvoient des codes d'erreur qu'il faut vérifier

## Dépannage

### Erreur "Fonction introuvable"

Si Delphi ne trouve pas une fonction dans la DLL :

1. Vérifiez l'orthographe exacte du nom de la fonction
2. Utilisez l'outil `DependencyWalker` pour voir les fonctions exportées par la DLL
3. Vérifiez si la fonction utilise le décorateur "C" (`extern "C"`) dans le code source C++
4. Essayez d'ajouter un underscore devant le nom (`name '_MaFonction'`)

### Corruption de mémoire et plantages

Si votre application plante lors de l'appel à une fonction C/C++ :

1. Vérifiez la correspondance exacte des types de paramètres
2. Assurez-vous d'utiliser la bonne convention d'appel (`stdcall`, `cdecl`, etc.)
3. Vérifiez les tailles de structures et l'alignement
4. Vérifiez que les strings sont correctement terminées par un zéro
5. Utilisez des outils de débogage comme `Process Monitor` ou les points d'arrêt sur exceptions

## Pour aller plus loin

Pour les intégrations complexes, envisagez ces approches avancées :

1. **COM** - Créez des composants COM en C++ utilisables depuis Delphi
2. **WebAssembly** - Pour les applications web, intégrez du code C/C++ via WebAssembly
3. **FFI (Foreign Function Interface)** - Des bibliothèques tiers comme FFIDELPHI

## Conclusion

L'intégration de bibliothèques C/C++ dans Delphi ouvre d'immenses possibilités en vous permettant d'exploiter l'écosystème riche des bibliothèques C/C++ tout en conservant la productivité et l'élégance de Delphi.

Bien que cette intégration puisse sembler complexe au premier abord, les techniques présentées dans ce chapitre vous permettront de créer des ponts robustes entre ces mondes de programmation. Commencez par des intégrations simples via DLL, puis progressez vers des techniques plus avancées à mesure que vos besoins évoluent.

La clé du succès réside dans une bonne compréhension des conventions d'appel, de la gestion des types, et des bonnes pratiques pour la gestion des ressources.
