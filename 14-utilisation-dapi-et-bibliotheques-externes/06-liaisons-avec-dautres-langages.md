# 14.6 Liaisons avec d'autres langages

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Dans le monde du développement logiciel, il est rare qu'un seul langage de programmation puisse répondre à tous les besoins. Chaque langage a ses forces et ses faiblesses, et parfois, la meilleure solution consiste à combiner plusieurs langages dans un même projet. Delphi offre plusieurs moyens de communiquer avec du code écrit dans d'autres langages de programmation.

Ce chapitre vous expliquera comment votre application Delphi peut interagir avec du code écrit en C/C++, Python, JavaScript et d'autres langages populaires. Nous verrons des exemples concrets et des techniques accessibles, même si vous êtes débutant.

## Pourquoi utiliser plusieurs langages ?

Voici quelques raisons qui pourraient vous amener à vouloir combiner Delphi avec d'autres langages :

1. **Réutilisation de code existant** : Intégrer une bibliothèque déjà écrite dans un autre langage
2. **Fonctionnalités spécifiques** : Utiliser un langage particulièrement adapté à certaines tâches (Python pour l'IA, JavaScript pour le web)
3. **Compétences de l'équipe** : Permettre à chaque développeur de travailler dans le langage qu'il maîtrise le mieux
4. **Migration progressive** : Moderniser une application existante en intégrant de nouvelles technologies

## 1. Liaison avec C/C++

C et C++ sont probablement les langages les plus faciles à intégrer avec Delphi, grâce à plusieurs mécanismes bien établis.

### Utilisation de fichiers DLL (Dynamic Link Library)

La méthode la plus courante consiste à utiliser des DLLs (bibliothèques à liaison dynamique). Une DLL C/C++ expose des fonctions que Delphi peut appeler.

#### Exemple de DLL C++ simple

Voici un exemple de code C++ pour une DLL :

```cpp
// MathLib.cpp
#include <windows.h>

extern "C" {
    // L'attribut __declspec(dllexport) indique que la fonction est exportée
    __declspec(dllexport) int __stdcall AddNumbers(int a, int b) {
        return a + b;
    }

    __declspec(dllexport) double __stdcall CalculateAverage(int* values, int count) {
        if (count <= 0) return 0;

        double sum = 0;
        for (int i = 0; i < count; i++) {
            sum += values[i];
        }

        return sum / count;
    }
}
```

#### Appel de la DLL depuis Delphi

Pour utiliser cette DLL dans Delphi, vous devez déclarer les fonctions externes :

```pascal
// Déclaration des fonctions externes
function AddNumbers(A, B: Integer): Integer; stdcall; external 'MathLib.dll';
function CalculateAverage(Values: PInteger; Count: Integer): Double; stdcall; external 'MathLib.dll';

procedure TForm1.ButtonCallDLLClick(Sender: TObject);
var
  Result: Integer;
  Numbers: array[0..4] of Integer;
  Average: Double;
begin
  // Appel de la fonction AddNumbers
  Result := AddNumbers(5, 7);
  ShowMessage('5 + 7 = ' + IntToStr(Result));

  // Appel de CalculateAverage
  Numbers[0] := 10;
  Numbers[1] := 20;
  Numbers[2] := 30;
  Numbers[3] := 40;
  Numbers[4] := 50;

  Average := CalculateAverage(@Numbers[0], 5);
  ShowMessage('Moyenne : ' + FloatToStr(Average));
end;
```

### Points importants pour les DLLs C/C++

1. **Conventions d'appel** : Utilisez `stdcall` pour la compatibilité avec Windows, ou `cdecl` pour la compatibilité avec le C standard.
2. **Name mangling** : Utilisez `extern "C"` dans le code C++ pour éviter la décoration des noms de fonctions.
3. **Types de données** : Assurez-vous de la correspondance des types entre Delphi et C/C++.

| Type Delphi     | Type C/C++          | Taille (bits) |
|-----------------|---------------------|--------------|
| Byte            | unsigned char       | 8            |
| ShortInt        | signed char         | 8            |
| Word            | unsigned short      | 16           |
| SmallInt        | short               | 16           |
| Cardinal        | unsigned int        | 32           |
| Integer         | int                 | 32           |
| Int64           | long long           | 64           |
| Single          | float               | 32           |
| Double          | double              | 64           |
| Boolean         | bool                | 8            |
| Char            | char                | 8            |
| WideChar        | wchar_t             | 16           |
| PChar           | char*               | 32/64        |
| PWideChar       | wchar_t*            | 32/64        |

4. **Gestion de la mémoire** : Soyez attentif à qui alloue et qui libère la mémoire. Idéalement, la mémoire devrait être libérée par le même langage qui l'a allouée.

### Chargement dynamique des DLLs

Au lieu de lier statiquement à la DLL (ce qui nécessite que la DLL soit présente au démarrage), vous pouvez charger la DLL dynamiquement :

```pascal
procedure TForm1.ButtonDynamicLoadClick(Sender: TObject);
type
  TAddNumbersFunc = function(A, B: Integer): Integer; stdcall;

var
  DLLHandle: THandle;
  AddNumbersFunc: TAddNumbersFunc;
  Result: Integer;
begin
  // Charger dynamiquement la DLL
  DLLHandle := LoadLibrary('MathLib.dll');

  if DLLHandle <> 0 then
  begin
    try
      // Obtenir l'adresse de la fonction
      @AddNumbersFunc := GetProcAddress(DLLHandle, 'AddNumbers');

      if Assigned(AddNumbersFunc) then
      begin
        // Appeler la fonction
        Result := AddNumbersFunc(5, 7);
        ShowMessage('5 + 7 = ' + IntToStr(Result));
      end
      else
        ShowMessage('Fonction non trouvée dans la DLL');
    finally
      // Décharger la DLL
      FreeLibrary(DLLHandle);
    end;
  end
  else
    ShowMessage('Impossible de charger la DLL');
end;
```

### Création d'une classe d'encapsulation

Pour une meilleure organisation, vous pouvez créer une classe qui encapsule l'accès à la DLL :

```pascal
type
  TMathLibrary = class
  private
    FDLLHandle: THandle;
    FAddNumbers: function(A, B: Integer): Integer; stdcall;
    FCalculateAverage: function(Values: PInteger; Count: Integer): Double; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function IsLoaded: Boolean;
    function Add(A, B: Integer): Integer;
    function Average(const Values: array of Integer): Double;
  end;
```

## 2. Liaison avec Python

Python est un langage très populaire, particulièrement pour le traitement de données, l'intelligence artificielle et l'automatisation. Plusieurs bibliothèques permettent d'intégrer Python à Delphi.

### Utilisation de Python4Delphi

[Python4Delphi](https://github.com/pyscripter/python4delphi) est une bibliothèque qui permet d'exécuter du code Python depuis Delphi et vice versa.

#### Installation de Python4Delphi

1. Installez Python sur votre système
2. Téléchargez Python4Delphi depuis GitHub
3. Installez les packages dans Delphi
4. Configurez les chemins de bibliothèque

#### Exemple simple d'utilisation de Python4Delphi

```pascal
procedure TForm1.ButtonRunPythonClick(Sender: TObject);
var
  PythonEngine: TPythonEngine;
  PythonModule: TPythonModule;
  PythonType: TPythonType;
  PythonGUIInputOutput: TPythonGUIInputOutput;
begin
  // Créer les composants nécessaires
  PythonEngine := TPythonEngine.Create(nil);
  PythonModule := TPythonModule.Create(nil);
  PythonType := TPythonType.Create(nil);
  PythonGUIInputOutput := TPythonGUIInputOutput.Create(nil);

  try
    // Configuration de base
    PythonGUIInputOutput.Output := Memo1;
    PythonEngine.IO := PythonGUIInputOutput;
    PythonModule.Engine := PythonEngine;
    PythonModule.ModuleName := 'mymodule';
    PythonType.Engine := PythonEngine;
    PythonType.Module := PythonModule;

    // Initialiser Python
    PythonEngine.Initialize;

    // Exécuter du code Python
    PythonEngine.ExecString(
      'import sys' + #10 +
      'print("Python version:", sys.version)' + #10 +
      'print("Hello from Python!")' + #10 +
      'result = 42 * 3' + #10 +
      'print("42 * 3 =", result)'
    );
  finally
    // Libérer les ressources
    PythonEngine.Free;
    PythonModule.Free;
    PythonType.Free;
    PythonGUIInputOutput.Free;
  end;
end;
```

#### Appel de fonctions Python depuis Delphi

```pascal
procedure TForm1.ButtonPythonFunctionClick(Sender: TObject);
var
  PythonEngine: TPythonEngine;
  PyObject, PyFunction, PyResult: PPyObject;
  Result: Integer;
begin
  PythonEngine := TPythonEngine.Create(nil);
  try
    PythonEngine.Initialize;

    // Définir une fonction Python
    PythonEngine.ExecString(
      'def calculate_sum(a, b):' + #10 +
      '    return a + b'
    );

    // Obtenir le module principal
    PyObject := PythonEngine.GetMainModule;

    // Obtenir la référence à la fonction
    PyFunction := PythonEngine.PyObject_GetAttrString(PyObject, 'calculate_sum');

    if Assigned(PyFunction) then
    begin
      try
        // Appeler la fonction avec les paramètres (10, 20)
        PyResult := PythonEngine.PyObject_CallFunction(PyFunction, 'ii', [10, 20]);

        if Assigned(PyResult) then
        begin
          try
            // Convertir le résultat en entier
            Result := PythonEngine.PyLong_AsLong(PyResult);
            ShowMessage('10 + 20 = ' + IntToStr(Result));
          finally
            PythonEngine.Py_XDECREF(PyResult);
          end;
        end;
      finally
        PythonEngine.Py_XDECREF(PyFunction);
      end;
    end;
  finally
    PythonEngine.Free;
  end;
end;
```

### Utilisation de bibliothèques Python populaires

Un des grands avantages de l'intégration de Python est l'accès à son écosystème riche de bibliothèques.

```pascal
procedure TForm1.ButtonPythonLibrariesClick(Sender: TObject);
var
  PythonEngine: TPythonEngine;
  PythonGUIInputOutput: TPythonGUIInputOutput;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonGUIInputOutput := TPythonGUIInputOutput.Create(nil);

  try
    PythonGUIInputOutput.Output := Memo1;
    PythonEngine.IO := PythonGUIInputOutput;
    PythonEngine.Initialize;

    // Utilisation de NumPy pour des calculs numériques
    PythonEngine.ExecString(
      'try:' + #10 +
      '    import numpy as np' + #10 +
      '    arr = np.array([1, 2, 3, 4, 5])' + #10 +
      '    print("Array:", arr)' + #10 +
      '    print("Mean:", np.mean(arr))' + #10 +
      '    print("Sum:", np.sum(arr))' + #10 +
      'except ImportError:' + #10 +
      '    print("NumPy n''est pas installé. Installez-le avec pip install numpy")'
    );

    // Utilisation de Matplotlib pour des graphiques
    PythonEngine.ExecString(
      'try:' + #10 +
      '    import matplotlib.pyplot as plt' + #10 +
      '    import io' + #10 +
      '    import base64' + #10 +
      '    plt.figure()' + #10 +
      '    plt.plot([1, 2, 3, 4], [1, 4, 9, 16], ''ro'')' + #10 +
      '    plt.axis([0, 6, 0, 20])' + #10 +
      '    plt.title("Simple Plot")' + #10 +
      '    # Dans une vraie application, vous pourriez sauvegarder l''image' + #10 +
      '    # et l''afficher dans un TImage' + #10 +
      '    plt.savefig("plot.png")' + #10 +
      '    print("Graphique enregistré dans plot.png")' + #10 +
      'except ImportError:' + #10 +
      '    print("Matplotlib n''est pas installé. Installez-le avec pip install matplotlib")'
    );
  finally
    PythonEngine.Free;
    PythonGUIInputOutput.Free;
  end;
end;
```

## 3. Liaison avec JavaScript

JavaScript est omniprésent dans le développement web. Delphi permet d'interagir avec JavaScript de plusieurs façons.

### Utilisation de composants WebBrowser/EdgeBrowser

Vous pouvez exécuter du JavaScript dans un navigateur web intégré à votre application Delphi.

```pascal
procedure TForm1.ButtonRunJavaScriptClick(Sender: TObject);
var
  Document: IHTMLDocument2;
  JavaScript: string;
  Result: OleVariant;
begin
  // Charger une page vide
  WebBrowser1.Navigate('about:blank');
  while WebBrowser1.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  // Obtenir le document
  Document := WebBrowser1.Document as IHTMLDocument2;

  // Écrire un contenu HTML basique
  Document.write('<html><body><h1>Test JavaScript</h1><div id="result"></div></body></html>');
  Document.close;

  // Exécuter du JavaScript
  JavaScript :=
    'function calculateSum(a, b) {' +
    '  return a + b;' +
    '}' +
    'var result = calculateSum(10, 20);' +
    'document.getElementById("result").innerHTML = "10 + 20 = " + result;' +
    'return result;';

  Result := (WebBrowser1.Document as IHTMLDocument2).parentWindow.execScript(JavaScript, 'JavaScript');

  ShowMessage('Résultat retourné: ' + VarToStr(Result));
end;
```

Pour les applications modernes, utilisez le composant `TEdgeBrowser` disponible dans les versions récentes de Delphi qui utilise le moteur Chromium.

### Utilisation de Node.js

Pour des applications plus complexes, vous pouvez intégrer Node.js à votre application Delphi.

```pascal
procedure TForm1.ButtonRunNodeJSClick(Sender: TObject);
var
  NodeJSPath: string;
  ScriptPath: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Command: string;
  OutputFile: string;
  Output: TStringList;
begin
  NodeJSPath := 'C:\Program Files\nodejs\node.exe';

  // Créer un fichier JavaScript temporaire
  ScriptPath := GetTempDir + 'temp_script.js';
  OutputFile := GetTempDir + 'output.json';

  with TStringList.Create do
  try
    Add('// Script Node.js temporaire');
    Add('const fs = require("fs");');
    Add('const data = {');
    Add('  result: 10 + 20,');
    Add('  message: "Calculé par Node.js",');
    Add('  timestamp: new Date().toISOString()');
    Add('};');
    Add('fs.writeFileSync("' + StringReplace(OutputFile, '\', '\\', [rfReplaceAll]) + '", JSON.stringify(data, null, 2));');
    Add('console.log("Traitement terminé");');

    SaveToFile(ScriptPath);
  finally
    Free;
  end;

  // Exécuter Node.js avec notre script
  Command := Format('"%s" "%s"', [NodeJSPath, ScriptPath]);

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);

  if CreateProcess(nil, PChar(Command), nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
    try
      // Attendre que le processus se termine
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

      // Lire le fichier de sortie JSON
      if FileExists(OutputFile) then
      begin
        Output := TStringList.Create;
        try
          Output.LoadFromFile(OutputFile);
          ShowMessage('Résultat de Node.js : ' + Output.Text);
        finally
          Output.Free;
        end;

        // Supprimer les fichiers temporaires
        DeleteFile(OutputFile);
      end;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      DeleteFile(ScriptPath);
    end;
  end
  else
    ShowMessage('Erreur lors du lancement de Node.js : ' + SysErrorMessage(GetLastError));
end;
```

## 4. Liaison avec Java

Java est un langage très utilisé, notamment pour les applications d'entreprise et les applications Android. Il existe plusieurs moyens d'intégrer Java avec Delphi.

### Utilisation de JNI (Java Native Interface)

JNI permet à du code Java d'appeler et d'être appelé par du code natif (comme Delphi).

```pascal
// Cette implémentation est simplifiée et nécessiterait une bibliothèque JNI complète
type
  TJNI = class
  private
    FJavaVM: Pointer;
    FEnv: Pointer;
    FClassPath: string;
  public
    constructor Create(const ClassPath: string);
    destructor Destroy; override;
    function CallStaticMethod(const ClassName, MethodName, Signature: string; Args: array of const): Variant;
  end;

procedure TForm1.ButtonJavaClick(Sender: TObject);
var
  JNI: TJNI;
  Result: Variant;
begin
  JNI := TJNI.Create('C:\Path\To\JavaClasses');
  try
    // Appel d'une méthode Java statique
    Result := JNI.CallStaticMethod(
      'com.example.MathUtils',     // Nom de la classe
      'add',                      // Nom de la méthode
      '(II)I',                    // Signature JNI (2 entiers -> entier)
      [10, 20]                    // Paramètres
    );

    ShowMessage('10 + 20 = ' + VarToStr(Result));
  finally
    JNI.Free;
  end;
end;
```

### Utilisation de pont Java-Delphi

Des bibliothèques comme [DJP (Delphi-Java Bridge)](https://github.com/andrea-magni/DJP) ou [JACOB (Java COM Bridge)](http://danadler.com/jacob/) peuvent faciliter l'intégration.

Pour une implémentation complète, consultez la documentation spécifique de ces bibliothèques.

## 5. Liaison avec d'autres langages

### Liaison avec Ruby

```pascal
procedure TForm1.ButtonRubyClick(Sender: TObject);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  RubyScript: string;
  RubyPath: string;
  TempFile: string;
  Output: TStringList;
  ExitCode: Cardinal;
begin
  RubyPath := 'C:\Ruby30\bin\ruby.exe';
  TempFile := GetTempDir + 'temp_script.rb';

  // Création du script Ruby
  RubyScript :=
    "# Script Ruby temporaire\n" +
    "result = 10 + 20\n" +
    "puts \"Le résultat est: #{result}\"\n" +
    "File.open('" + StringReplace(GetTempDir + 'ruby_output.txt', '\', '/', [rfReplaceAll]) + "', 'w') do |f|\n" +
    "  f.puts result.to_s\n" +
    "end";

  // Sauvegarder le script dans un fichier temporaire
  with TStringList.Create do
  try
    Text := RubyScript;
    SaveToFile(TempFile);
  finally
    Free;
  end;

  // Exécuter Ruby avec notre script
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);

  if CreateProcess(nil, PChar(Format('"%s" "%s"', [RubyPath, TempFile])),
                   nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
    try
      // Attendre que le processus se termine
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

      // Obtenir le code de sortie
      GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);

      // Lire le fichier de sortie
      if FileExists(GetTempDir + 'ruby_output.txt') then
      begin
        Output := TStringList.Create;
        try
          Output.LoadFromFile(GetTempDir + 'ruby_output.txt');
          ShowMessage('Résultat Ruby : ' + Output.Text);
        finally
          Output.Free;
        end;

        // Supprimer le fichier de sortie
        DeleteFile(GetTempDir + 'ruby_output.txt');
      end;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      DeleteFile(TempFile);
    end;
  end
  else
    ShowMessage('Erreur lors du lancement de Ruby : ' + SysErrorMessage(GetLastError));
end;
```

### Liaison avec Go

Go est un langage moderne qui peut être compilé en bibliothèques partagées compatibles avec Delphi.

```go
// mathlib.go
package main

import "C"

//export Add
func Add(a, b int) int {
    return a + b
}

func main() {}
```

Compilez avec : `go build -buildmode=c-shared -o mathlib.dll mathlib.go`

Puis utilisez dans Delphi :

```pascal
function Add(A, B: Integer): Integer; cdecl; external 'mathlib.dll';

procedure TForm1.ButtonGoClick(Sender: TObject);
var
  Result: Integer;
begin
  Result := Add(10, 20);
  ShowMessage('10 + 20 = ' + IntToStr(Result));
end;
```

### Liaison avec Rust

Rust est un langage performant et sûr qui peut être intégré à Delphi via des DLLs.

```rust
// lib.rs
#[no_mangle]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

Compilez avec : `rustc --crate-type cdylib lib.rs`

Puis utilisez dans Delphi :

```pascal
function add(a, b: Integer): Integer; cdecl; external 'lib.dll';

procedure TForm1.ButtonRustClick(Sender: TObject);
var
  Result: Integer;
begin
  Result := add(10, 20);
  ShowMessage('10 + 20 = ' + IntToStr(Result));
end;
```

## Bonnes pratiques pour les liaisons entre langages

### 1. Choisir la bonne approche

- **DLLs** : Bonne option pour C/C++, Rust, Go
- **Processus séparés** : Option sûre pour Python, Ruby, Node.js
- **COM/ActiveX** : Option privilégiée pour les applications Windows
- **Services Web** : Option flexible et indépendante de la plateforme

### 2. Considérations de performance

- Les appels entre langages peuvent être coûteux en performance
- Minimisez le nombre d'appels en regroupant les opérations
- Transférez des lots de données plutôt que des appels individuels

### 3. Gestion des erreurs

Créez un système robuste de gestion des erreurs entre les langages :

```pascal
function SafeCallPython(const PythonCode: string): Boolean;
var
  PythonEngine: TPythonEngine;
begin
  Result := False;
  PythonEngine := TPythonEngine.Create(nil);

  try
    PythonEngine.Initialize;

    try
      PythonEngine.ExecString(PythonCode);
      Result := True;
    except
      on E: Exception do
      begin
        LogError('Erreur Python : ' + E.Message);
        // Gérer l'erreur de manière appropriée
      end;
    end;
  finally
    PythonEngine.Free;
  end;
end;
```

### 4. Conversion de types de données

La conversion des types entre langages peut être délicate. Créez des fonctions helper :

```pascal
// Convertir un tableau Delphi en liste Python
function DelphiArrayToPythonList(PythonEngine: TPythonEngine;
                                const Values: array of Variant): PPyObject;
var
  i: Integer;
begin
  Result := PythonEngine.PyList_New(Length(Values));

  for i := 0 to Length(Values) - 1 do
  begin
    case VarType(Values[i]) of
      varInteger, varByte, varSmallint, varShortInt:
        PythonEngine.PyList_SetItem(Result, i, PythonEngine.PyLong_FromLong(Values[i]));

      varSingle, varDouble, varCurrency:
        PythonEngine.PyList_SetItem(Result, i, PythonEngine.PyFloat_FromDouble(Values[i]));

      varOleStr, varString, varUString:
        PythonEngine.PyList_SetItem(Result, i,
          PythonEngine.PyUnicode_FromString(PChar(string(Values[i]))));

      varBoolean:
        if Values[i] then
          PythonEngine.PyList_SetItem(Result, i, PythonEngine.Py_True)
        else
          PythonEngine.PyList_SetItem(Result, i, PythonEngine.Py_False);
    end;
  end;
end;
```

## Exemple complet : Analyse de données avec Delphi et Python

Voici un exemple plus complet qui montre comment Delphi peut utiliser Python (via NumPy et Pandas) pour analyser des données et afficher les résultats :

```pascal
procedure TForm1.ButtonAnalyzeDataClick(Sender: TObject);
var
  PythonEngine: TPythonEngine;
  PythonGUIInputOutput: TPythonGUIInputOutput;
  CSVFile: string;
  PythonCode: string;
begin
  CSVFile := 'C:\Data\sales_data.csv';

  if not FileExists(CSVFile) then
  begin
    ShowMessage('Fichier CSV non trouvé : ' + CSVFile);
    Exit;
  end;

  PythonEngine := TPythonEngine.Create(nil);
  PythonGUIInputOutput := TPythonGUIInputOutput.Create(nil);

  try
    PythonGUIInputOutput.Output := MemoResult;
    PythonEngine.IO := PythonGUIInputOutput;
    PythonEngine.Initialize;

    PythonCode :=
      'import sys' + #10 +
      'try:' + #10 +
      '    import pandas as pd' + #10 +
      '    import numpy as np' + #10 +
      '    from matplotlib import pyplot as plt' + #10 +
      '    # Charger les données' + #10 +
      '    file_path = r"' + StringReplace(CSVFile, '\', '\\', [rfReplaceAll]) + '"' + #10 +
      '    df = pd.read_csv(file_path)' + #10 +
      '    # Afficher les 5 premières lignes' + #10 +
      '    print("Aperçu des données:")' + #10 +
      '    print(df.head())' + #10 +
      '    # Statistiques descriptives' + #10 +
      '    print("\nStatistiques descriptives:")' + #10 +
      '    print(df.describe())' + #10 +
      '    # Calculer les ventes par catégorie' + #10 +
      '    if "Category" in df.columns and "Sales" in df.columns:' + #10 +
      '        sales_by_category = df.groupby("Category")["Sales"].sum()' + #10 +
      '        print("\nVentes par catégorie:")' + #10 +
      '        print(sales_by_category)' + #10 +
      '        # Créer un graphique' + #10 +
      '        plt.figure(figsize=(10, 6))' + #10 +
      '        sales_by_category.plot(kind="bar", color="skyblue")' + #10 +
      '        plt.title("Ventes totales par catégorie")' + #10 +
      '        plt.xlabel("Catégorie")' + #10 +
      '        plt.ylabel("Ventes totales (€)")' + #10 +
      '        plt.tight_layout()' + #10 +
      '        # Sauvegarder le graphique' + #10 +
      '        chart_path = r"' + StringReplace(ExtractFilePath(Application.ExeName) + 'sales_chart.png',
                                            '\', '\\', [rfReplaceAll]) + '"' + #10 +
      '        plt.savefig(chart_path)' + #10 +
      '        print("\nGraphique sauvegardé dans :", chart_path)' + #10 +
      '        # Analyse supplémentaire' + #10 +
      '        if "Date" in df.columns:' + #10 +
      '            # Convertir la colonne Date en datetime' + #10 +
      '            df["Date"] = pd.to_datetime(df["Date"])' + #10 +
      '            # Ajouter des colonnes pour l''année et le mois' + #10 +
      '            df["Year"] = df["Date"].dt.year' + #10 +
      '            df["Month"] = df["Date"].dt.month' + #10 +
      '            # Ventes mensuelles' + #10 +
      '            monthly_sales = df.groupby(["Year", "Month"])["Sales"].sum().reset_index()' + #10 +
      '            print("\nVentes mensuelles:")' + #10 +
      '            print(monthly_sales.head(12))' + #10 +
      '    else:' + #10 +
      '        print("Colonnes Category ou Sales non trouvées dans le CSV")' + #10 +
      'except ImportError as e:' + #10 +
      '    print(f"Erreur: {e}")' + #10 +
      '    print("Veuillez installer les bibliothèques Python nécessaires:")' + #10 +
      '    print("pip install pandas numpy matplotlib")' + #10 +
      'except Exception as e:' + #10 +
      '    print(f"Erreur lors de l''analyse: {e}")';

    // Exécuter le code Python
    PythonEngine.ExecString(PythonCode);

    // Afficher l'image si elle existe
    if FileExists(ExtractFilePath(Application.ExeName) + 'sales_chart.png') then
    begin
      Image1.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'sales_chart.png');
      Image1.Visible := True;
      TabSheet2.TabVisible := True; // Onglet contenant l'image
    end;

  finally
    PythonEngine.Free;
    PythonGUIInputOutput.Free;
  end;
end;
```

Cet exemple montre comment :
1. Charger un fichier CSV avec Pandas
2. Effectuer une analyse statistique basique
3. Créer un graphique avec Matplotlib
4. Faire des analyses temporelles
5. Afficher les résultats dans l'interface Delphi

## Communication bidirectionnelle

Jusqu'à présent, nous avons principalement vu comment appeler du code d'autres langages depuis Delphi. Voyons maintenant comment permettre à ces langages d'appeler du code Delphi.

### Exposer des fonctions Delphi à Python

Python4Delphi permet d'exposer des fonctions Delphi pour qu'elles soient appelables depuis Python :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser Python
  PythonEngine1.Initialize;

  // Enregistrer les fonctions Delphi dans le module Python
  with PythonModule1 do
  begin
    AddMethod('add_numbers', @PyAddNumbers, 'Additionne deux nombres');
    AddMethod('show_message', @PyShowMessage, 'Affiche un message dans Delphi');
    AddMethod('get_application_path', @PyGetApplicationPath, 'Retourne le chemin de l''application');
  end;
end;

// Fonctions exposées à Python
function PyAddNumbers(Self, Args: PPyObject): PPyObject; cdecl;
var
  a, b: Integer;
begin
  // Extraire les paramètres (deux entiers)
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple(Args, 'ii:add_numbers', [@a, @b]) <> 0 then
      Result := PyLong_FromLong(a + b)
    else
      Result := nil;
  end;
end;

function PyShowMessage(Self, Args: PPyObject): PPyObject; cdecl;
var
  msg: PAnsiChar;
begin
  // Extraire le paramètre (chaîne de caractères)
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple(Args, 's:show_message', [@msg]) <> 0 then
    begin
      ShowMessage(string(msg));
      Result := PyLong_FromLong(1); // Retourne 1 pour succès
    end
    else
      Result := nil;
  end;
end;

function PyGetApplicationPath(Self, Args: PPyObject): PPyObject; cdecl;
begin
  // Pas de paramètres pour cette fonction
  with GetPythonEngine do
  begin
    Result := PyUnicode_FromString(PAnsiChar(AnsiString(ExtractFilePath(Application.ExeName))));
  end;
end;

procedure TForm1.ButtonPythonCallDelphiClick(Sender: TObject);
begin
  // Code Python qui appelle nos fonctions Delphi
  PythonEngine1.ExecString(
    'import delphimodule' + #10 +
    'result = delphimodule.add_numbers(5, 7)' + #10 +
    'print("5 + 7 =", result)' + #10 +
    'delphimodule.show_message("Hello from Python!")' + #10 +
    'app_path = delphimodule.get_application_path()' + #10 +
    'print("Application path:", app_path)'
  );
end;
```

### Créer un serveur COM avec Delphi appelable depuis d'autres langages

Vous pouvez créer un serveur COM avec Delphi qui peut être appelé par d'autres langages, comme JavaScript ou VBScript :

```pascal
// Dans l'unité d'interface
type
  IDelphiMath = interface(IDispatch)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function Add(A, B: Integer): Integer; safecall;
    function Subtract(A, B: Integer): Integer; safecall;
    function Multiply(A, B: Integer): Integer; safecall;
    function Divide(A, B: Double): Double; safecall;
  end;

  TDelphiMath = class(TAutoObject, IDelphiMath)
  protected
    function Add(A, B: Integer): Integer; safecall;
    function Subtract(A, B: Integer): Integer; safecall;
    function Multiply(A, B: Integer): Integer; safecall;
    function Divide(A, B: Double): Double; safecall;
  end;

// Dans l'unité d'implémentation
function TDelphiMath.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TDelphiMath.Subtract(A, B: Integer): Integer;
begin
  Result := A - B;
end;

function TDelphiMath.Multiply(A, B: Integer): Integer;
begin
  Result := A * B;
end;

function TDelphiMath.Divide(A, B: Double): Double;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro')
  else
    Result := A / B;
end;
```

Une fois votre serveur COM enregistré, il peut être utilisé depuis d'autres langages :

```javascript
// JavaScript (dans un fichier .js ou un navigateur)
var math = new ActiveXObject("YourProject.DelphiMath");
var result = math.Add(10, 20);
WScript.Echo("10 + 20 = " + result);
```

```vb
' VBScript
Dim math
Set math = CreateObject("YourProject.DelphiMath")
Dim result
result = math.Multiply(5, 6)
MsgBox "5 * 6 = " & result
```

## Architecture multi-langages

Pour des projets plus complexes, vous pouvez mettre en place une architecture multi-langages où chaque langage est utilisé pour ses points forts :

1. **Delphi** : Interface utilisateur et logique métier principale
2. **Python** : Analyse de données, machine learning
3. **JavaScript** : Interface web, visualisations interactives
4. **C/C++** : Opérations intensives, accès bas niveau au système

### Exemple d'architecture

```
+------------------------------------------+
|               Application Delphi         |
|  +-------------+       +---------------+ |
|  | Interface   |       | Logique       | |
|  | Utilisateur |<----->| Métier        | |
|  +-------------+       +---------------+ |
|         ^                     ^          |
+---------|--------------------|----------+
          |                     |
+---------v----------+  +-------v---------+
| Module JavaScript  |  | Module Python   |
| (Visualisations    |  | (Analyse de     |
|  Web interactives) |  |  données, ML)   |
+-------------------+  +-----------------+
          ^                     ^
          |                     |
+---------v----------+  +-------v---------+
| Bibliothèque C++   |  | API externes    |
| (Opérations        |  | (Services web,  |
|  intensives)       |  |  Cloud)         |
+-------------------+  +-----------------+
```

### Implémentation avec protocole JSON

Pour faire communiquer ces différents modules, vous pouvez utiliser un format d'échange comme JSON :

```pascal
// Structure de données Delphi
type
  TDataRequest = record
    RequestType: string;
    Parameters: TJSONObject;
  end;

  TDataResponse = record
    Success: Boolean;
    Data: TJSONValue;
    ErrorMessage: string;
  end;

// Fonction d'envoi de données à Python
function SendToPython(const Request: TDataRequest): TDataResponse;
var
  PythonEngine: TPythonEngine;
  JsonRequest, ResponseStr: string;
  PyResult: PPyObject;
begin
  // Initialisation
  Result.Success := False;

  // Convertir la requête en JSON
  JsonRequest := Format(
    '{"request_type": "%s", "parameters": %s}',
    [Request.RequestType, Request.Parameters.ToString]
  );

  // Créer et initialiser le moteur Python
  PythonEngine := TPythonEngine.Create(nil);
  try
    PythonEngine.Initialize;

    // Définir une fonction Python pour traiter la requête
    PythonEngine.ExecString(
      'import json' + #10 +
      'def process_request(request_json):' + #10 +
      '    request = json.loads(request_json)' + #10 +
      '    request_type = request["request_type"]' + #10 +
      '    params = request["parameters"]' + #10 +
      '    ' + #10 +
      '    response = {"success": False, "data": None, "error_message": ""}' + #10 +
      '    ' + #10 +
      '    try:' + #10 +
      '        if request_type == "calculate_statistics":' + #10 +
      '            import numpy as np' + #10 +
      '            data = params["data"]' + #10 +
      '            response["data"] = {' + #10 +
      '                "mean": np.mean(data),' + #10 +
      '                "median": np.median(data),' + #10 +
      '                "std_dev": np.std(data),' + #10 +
      '                "min": np.min(data),' + #10 +
      '                "max": np.max(data)' + #10 +
      '            }' + #10 +
      '            response["success"] = True' + #10 +
      '        else:' + #10 +
      '            response["error_message"] = f"Unknown request type: {request_type}"' + #10 +
      '    except Exception as e:' + #10 +
      '        response["error_message"] = str(e)' + #10 +
      '    ' + #10 +
      '    return json.dumps(response)'
    );

    // Appeler la fonction Python
    PyResult := PythonEngine.EvalString(
      Format('process_request(''%s'')', [JsonRequest])
    );

    if Assigned(PyResult) then
    begin
      // Convertir le résultat Python en chaîne Delphi
      ResponseStr := string(PythonEngine.PyObjectAsString(PyResult));

      // Analyser la réponse JSON
      var ResponseJson := TJSONObject.ParseJSONValue(ResponseStr) as TJSONObject;
      if Assigned(ResponseJson) then
      begin
        try
          Result.Success := ResponseJson.GetValue<Boolean>('success');
          Result.ErrorMessage := ResponseJson.GetValue<string>('error_message');

          if Result.Success then
            Result.Data := ResponseJson.GetValue<TJSONValue>('data').Clone as TJSONValue
          else
            Result.Data := nil;
        finally
          ResponseJson.Free;
        end;
      end;
    end;
  finally
    PythonEngine.Free;
  end;
end;
```

## Conclusion et recommandations

L'intégration entre Delphi et d'autres langages offre de nombreuses possibilités pour étendre les capacités de vos applications. Voici quelques recommandations pour réussir cette intégration :

### Quand utiliser quel langage ?

- **Delphi** : Interface utilisateur, accès aux composants Windows, logique métier
- **C/C++** : Opérations nécessitant des performances élevées, accès bas niveau
- **Python** : Analyse de données, Machine Learning, scripting, prototypage rapide
- **JavaScript** : Interfaces web, visualisations interactives
- **Java** : Applications d'entreprise, compatibilité avec Android

### Bonnes pratiques d'architecture

1. **Modularité** : Concevez votre application en modules indépendants qui communiquent par des interfaces bien définies
2. **Standards d'échange** : Utilisez des formats standard comme JSON pour l'échange de données
3. **Isolation** : Isolez le code externe dans des processus séparés pour éviter les plantages
4. **Tests d'intégration** : Testez soigneusement les interactions entre les langages
5. **Documentation** : Documentez clairement comment les différentes parties communiquent

### Points à surveiller

1. **Performance** : Les appels entre langages ajoutent une surcharge, minimisez-les pour les opérations fréquentes
2. **Déploiement** : Assurez-vous que toutes les dépendances sont installées correctement
3. **Débogage** : Le débogage multi-langages peut être complexe, utilisez la journalisation détaillée
4. **Versions** : Fixez les versions des bibliothèques et outils pour éviter les incompatibilités

En suivant ces principes, vous pourrez tirer parti des forces de chaque langage tout en conservant la puissance et la facilité de développement de Delphi comme base de votre application.

L'intégration multi-langages n'est pas seulement une solution technique, c'est une approche stratégique qui vous permet de choisir le meilleur outil pour chaque tâche, maximisant ainsi l'efficacité et la qualité de vos applications.

⏭️ [Liaison avec des API REST tierces](/14-utilisation-dapi-et-bibliotheques-externes/07-liaison-avec-des-api-rest-tierces.md)
