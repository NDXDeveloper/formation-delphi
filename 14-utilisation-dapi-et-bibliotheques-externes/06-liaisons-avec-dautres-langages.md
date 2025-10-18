🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.6 Liaisons avec d'autres langages

## Introduction à l'interopérabilité

### Pourquoi faire communiquer différents langages ?

Dans le monde réel, les applications sont rarement construites avec un seul langage de programmation. Vous pourriez avoir besoin de :

**Réutiliser du code existant** : Utiliser une bibliothèque Python d'analyse de données dans votre application Delphi.

**Exploiter les forces de chaque langage** : Python pour l'IA, JavaScript pour le web, Delphi pour l'interface utilisateur.

**Intégration d'équipe** : Collaborer avec des développeurs utilisant d'autres technologies.

**Accès à des écosystèmes** : Bénéficier des milliers de bibliothèques disponibles dans d'autres langages.

**Migration progressive** : Moderniser une application sans tout réécrire d'un coup.

### Méthodes de communication

Il existe plusieurs façons de faire communiquer Delphi avec d'autres langages :

**Exécution de processus** : Lancer un programme externe et récupérer son résultat.

**Communication inter-processus (IPC)** : Pipes, sockets, mémoire partagée.

**Bibliothèques partagées** : DLL, SO (Linux), DYLIB (macOS).

**Services web** : REST, SOAP, WebSockets.

**COM/OLE** : Pour Windows (déjà vu dans la section précédente).

**Sérialisation de données** : JSON, XML, Protocol Buffers.

**FFI (Foreign Function Interface)** : Appel direct de fonctions.

## Python et Delphi

### Pourquoi intégrer Python ?

Python est extrêmement populaire pour :
- L'analyse de données et le machine learning
- Le traitement scientifique
- L'automatisation
- Les scripts rapides

### Python4Delphi (P4D)

**Python4Delphi** est une bibliothèque qui permet d'exécuter du code Python directement depuis Delphi.

#### Installation

1. Installer Python sur votre système
2. Télécharger Python4Delphi depuis GitHub
3. Ajouter les unités au projet Delphi

#### Exemple basique

```pascal
uses
  PythonEngine;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurer le moteur Python
  PythonEngine1.DllPath := 'C:\Python39';
  PythonEngine1.DllName := 'python39.dll';
  PythonEngine1.LoadDll;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Exécuter du code Python simple
  PythonEngine1.ExecString('print("Hello from Python!")');
  PythonEngine1.ExecString('result = 2 + 2');
  PythonEngine1.ExecString('print("2 + 2 =", result)');
end;
```

#### Passer des variables entre Delphi et Python

```pascal
procedure ExecuterCalculPython(a, b: Integer);
var
  Script: TStringList;
  Result: Variant;
begin
  Script := TStringList.Create;
  try
    // Définir les variables Python depuis Delphi
    PythonEngine1.SetVar('x', a);
    PythonEngine1.SetVar('y', b);

    // Script Python
    Script.Add('# Calculer la somme');
    Script.Add('resultat = x + y');
    Script.Add('message = f"La somme de {x} et {y} est {resultat}"');

    PythonEngine1.ExecStrings(Script);

    // Récupérer le résultat
    Result := PythonEngine1.EvalString('resultat');
    ShowMessage(Format('Résultat: %d', [Integer(Result)]));

    Result := PythonEngine1.EvalString('message');
    ShowMessage(String(Result));
  finally
    Script.Free;
  end;
end;
```

#### Utiliser des bibliothèques Python

```pascal
procedure UtiliserNumPy;
var
  Script: TStringList;
  Moyenne: Variant;
begin
  Script := TStringList.Create;
  try
    Script.Add('import numpy as np');
    Script.Add('');
    Script.Add('# Créer un tableau');
    Script.Add('donnees = np.array([10, 20, 30, 40, 50])');
    Script.Add('');
    Script.Add('# Calculer la moyenne');
    Script.Add('moyenne = np.mean(donnees)');
    Script.Add('');
    Script.Add('# Calculer l''écart-type');
    Script.Add('ecart_type = np.std(donnees)');

    PythonEngine1.ExecStrings(Script);

    Moyenne := PythonEngine1.EvalString('moyenne');
    ShowMessage(Format('Moyenne: %.2f', [Double(Moyenne)]));
  finally
    Script.Free;
  end;
end;
```

#### Exemple avancé : Machine Learning avec scikit-learn

```pascal
procedure EntrainerModeleML;
var
  Script: TStringList;
  Precision: Variant;
begin
  Script := TStringList.Create;
  try
    Script.Add('from sklearn import datasets');
    Script.Add('from sklearn.model_selection import train_test_split');
    Script.Add('from sklearn.ensemble import RandomForestClassifier');
    Script.Add('from sklearn.metrics import accuracy_score');
    Script.Add('');
    Script.Add('# Charger le dataset Iris');
    Script.Add('iris = datasets.load_iris()');
    Script.Add('X = iris.data');
    Script.Add('y = iris.target');
    Script.Add('');
    Script.Add('# Diviser en train/test');
    Script.Add('X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)');
    Script.Add('');
    Script.Add('# Entraîner le modèle');
    Script.Add('clf = RandomForestClassifier(n_estimators=100)');
    Script.Add('clf.fit(X_train, y_train)');
    Script.Add('');
    Script.Add('# Prédire et évaluer');
    Script.Add('y_pred = clf.predict(X_test)');
    Script.Add('precision = accuracy_score(y_test, y_pred)');

    PythonEngine1.ExecStrings(Script);

    Precision := PythonEngine1.EvalString('precision');
    ShowMessage(Format('Précision du modèle: %.2f%%', [Double(Precision) * 100]));
  finally
    Script.Free;
  end;
end;
```

### Alternative : Exécution de scripts Python externes

Si vous n'avez pas besoin d'intégration étroite, vous pouvez simplement exécuter des scripts Python :

```pascal
uses
  System.Classes, System.SysUtils;

function ExecuterScriptPython(const ScriptPath: string;
  const Parametres: string = ''): string;
var
  Process: TProcess;
  OutputList: TStringList;
  CommandLine: string;
begin
  Result := '';

  CommandLine := Format('python "%s" %s', [ScriptPath, Parametres]);

  OutputList := TStringList.Create;
  try
    // Exécuter Python et capturer la sortie
    if ExecuteCaptureOutput(CommandLine, OutputList) then
      Result := OutputList.Text
    else
      raise Exception.Create('Erreur lors de l''exécution du script Python');
  finally
    OutputList.Free;
  end;
end;

// Fonction helper pour capturer la sortie
function ExecuteCaptureOutput(const CommandLine: string;
  Output: TStringList): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  SA: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  Buffer: array[0..4095] of AnsiChar;
  BytesRead: DWORD;
  TempStr: AnsiString;
begin
  Result := False;

  // Créer un pipe pour capturer la sortie
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  if not CreatePipe(ReadPipe, WritePipe, @SA, 0) then
    Exit;

  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
    SI.hStdOutput := WritePipe;
    SI.hStdError := WritePipe;

    if CreateProcess(nil, PChar(CommandLine), nil, nil, True,
       CREATE_NEW_CONSOLE, nil, nil, SI, PI) then
    begin
      CloseHandle(WritePipe);
      WritePipe := 0;

      // Lire la sortie
      repeat
        if ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) then
        begin
          if BytesRead > 0 then
          begin
            SetString(TempStr, Buffer, BytesRead);
            Output.Add(String(TempStr));
          end;
        end
        else
          Break;
      until BytesRead = 0;

      WaitForSingleObject(PI.hProcess, INFINITE);
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
      Result := True;
    end;
  finally
    if WritePipe <> 0 then
      CloseHandle(WritePipe);
    CloseHandle(ReadPipe);
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  Resultat: string;
begin
  Resultat := ExecuterScriptPython('C:\Scripts\analyse.py', '100 200');
  Memo1.Lines.Text := Resultat;
end;
```

#### Communication via JSON

```pascal
// Script Python (analyse_donnees.py)
// import json
// import sys
//
// def analyser(donnees):
//     return {
//         "moyenne": sum(donnees) / len(donnees),
//         "max": max(donnees),
//         "min": min(donnees)
//     }
//
// if __name__ == "__main__":
//     donnees_json = sys.argv[1]
//     donnees = json.loads(donnees_json)
//     resultat = analyser(donnees)
//     print(json.dumps(resultat))

uses
  System.JSON;

function AnalyserDonneesAvecPython(Donnees: TArray<Integer>): TJSONObject;
var
  JSONArray: TJSONArray;
  JSONInput, JSONOutput: string;
  Resultat: string;
  I: Integer;
begin
  // Créer le JSON d'entrée
  JSONArray := TJSONArray.Create;
  try
    for I := 0 to High(Donnees) do
      JSONArray.Add(Donnees[I]);

    JSONInput := JSONArray.ToString;
  finally
    JSONArray.Free;
  end;

  // Exécuter le script Python
  Resultat := ExecuterScriptPython('analyse_donnees.py', '"' + JSONInput + '"');

  // Parser le résultat JSON
  Result := TJSONObject.ParseJSONValue(Resultat) as TJSONObject;
end;

// Utilisation
procedure TForm1.ButtonAnalyserClick(Sender: TObject);
var
  Donnees: TArray<Integer>;
  Resultat: TJSONObject;
  Moyenne: Double;
begin
  Donnees := [10, 20, 30, 40, 50];

  Resultat := AnalyserDonneesAvecPython(Donnees);
  try
    Moyenne := Resultat.GetValue<Double>('moyenne');
    ShowMessage(Format('Moyenne: %.2f', [Moyenne]));
  finally
    Resultat.Free;
  end;
end;
```

## JavaScript et Delphi

### WebView pour exécuter du JavaScript

Delphi peut intégrer un navigateur web et communiquer avec JavaScript.

#### Utilisation de TWebBrowser (EdgeView)

```pascal
uses
  Winapi.WebView2, Winapi.ActiveX;

type
  TForm1 = class(TForm)
    EdgeBrowser1: TEdgeBrowser;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
      Args: TWebMessageReceivedEventArgs);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Charger une page HTML avec JavaScript
  EdgeBrowser1.Navigate('about:blank');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  HTML: string;
begin
  // HTML avec JavaScript intégré
  HTML :=
    '<html>' +
    '<head>' +
    '  <script>' +
    '    function calculer(a, b) {' +
    '      return a + b;' +
    '    }' +
    '    ' +
    '    function envoyerResultat() {' +
    '      var resultat = calculer(10, 20);' +
    '      window.chrome.webview.postMessage(resultat);' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body>' +
    '  <h1>Interface JavaScript</h1>' +
    '  <button onclick="envoyerResultat()">Calculer</button>' +
    '</body>' +
    '</html>';

  EdgeBrowser1.NavigateToString(HTML);
end;

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
begin
  // Recevoir des messages depuis JavaScript
  ShowMessage('Résultat du JavaScript: ' + Args.WebMessageAsString);
end;
```

#### Appeler des fonctions JavaScript depuis Delphi

```pascal
procedure AppelerFonctionJavaScript;
var
  Script: string;
begin
  Script := 'calculer(15, 25)';

  EdgeBrowser1.ExecuteScript(Script,
    procedure(AResult: HRESULT; const AResultObjectAsJson: string)
    begin
      ShowMessage('Résultat: ' + AResultObjectAsJson);
    end);
end;
```

#### Exemple complet : Visualisation de données avec Chart.js

```pascal
procedure AfficherGraphiqueAvecChartJS(Donnees: TArray<Integer>);
var
  HTML, DonneesJSON: string;
  I: Integer;
begin
  // Construire le JSON des données
  DonneesJSON := '[';
  for I := 0 to High(Donnees) do
  begin
    if I > 0 then
      DonneesJSON := DonneesJSON + ',';
    DonneesJSON := DonneesJSON + IntToStr(Donnees[I]);
  end;
  DonneesJSON := DonneesJSON + ']';

  // HTML avec Chart.js
  HTML := Format(
    '<html>' +
    '<head>' +
    '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>' +
    '</head>' +
    '<body>' +
    '  <canvas id="monGraphique"></canvas>' +
    '  <script>' +
    '    const ctx = document.getElementById("monGraphique");' +
    '    new Chart(ctx, {' +
    '      type: "line",' +
    '      data: {' +
    '        labels: ["Jan", "Fév", "Mar", "Avr", "Mai"],' +
    '        datasets: [{' +
    '          label: "Ventes",' +
    '          data: %s,' +
    '          borderWidth: 2' +
    '        }]' +
    '      }' +
    '    });' +
    '  </script>' +
    '</body>' +
    '</html>',
    [DonneesJSON]
  );

  EdgeBrowser1.NavigateToString(HTML);
end;
```

### Node.js depuis Delphi

Exécuter des scripts Node.js :

```pascal
function ExecuterScriptNodeJS(const ScriptPath: string): string;
var
  CommandLine: string;
  Output: TStringList;
begin
  CommandLine := Format('node "%s"', [ScriptPath]);

  Output := TStringList.Create;
  try
    if ExecuteCaptureOutput(CommandLine, Output) then
      Result := Output.Text
    else
      raise Exception.Create('Erreur Node.js');
  finally
    Output.Free;
  end;
end;

// Script Node.js exemple (traitement.js)
// const donnees = process.argv[2];
// const resultat = JSON.parse(donnees).map(x => x * 2);
// console.log(JSON.stringify(resultat));

procedure TForm1.Button1Click(Sender: TObject);
var
  Donnees: string;
  Resultat: string;
begin
  Donnees := '[1, 2, 3, 4, 5]';
  Resultat := ExecuterScriptNodeJS('traitement.js ' + Donnees);
  Memo1.Lines.Text := Resultat;
end;
```

## C# / .NET et Delphi

### Utilisation de DLL .NET

Avec .NET Framework, vous pouvez créer des DLL COM visibles depuis Delphi.

#### Créer une DLL .NET (C#)

```csharp
// Fichier: CalculatriceNET.cs
using System;
using System.Runtime.InteropServices;

namespace CalculatriceNET
{
    [ComVisible(true)]
    [Guid("12345678-1234-1234-1234-123456789ABC")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class Calculatrice
    {
        public int Additionner(int a, int b)
        {
            return a + b;
        }

        public double Multiplier(double a, double b)
        {
            return a * b;
        }

        public string Concatener(string s1, string s2)
        {
            return s1 + " " + s2;
        }
    }
}

// Compiler avec : csc /target:library /out:CalculatriceNET.dll CalculatriceNET.cs
// Enregistrer COM : regasm CalculatriceNET.dll /tlb /codebase
```

#### Utiliser la DLL .NET depuis Delphi

```pascal
uses
  ComObj;

procedure UtiliserDLLNET;
var
  Calc: OleVariant;
  Resultat: Integer;
begin
  // Créer l'objet .NET
  Calc := CreateOleObject('CalculatriceNET.Calculatrice');
  try
    // Appeler les méthodes
    Resultat := Calc.Additionner(10, 20);
    ShowMessage('10 + 20 = ' + IntToStr(Resultat));

    ShowMessage('Concaténation: ' + Calc.Concatener('Hello', 'World'));
  finally
    Calc := Unassigned;
  end;
end;
```

### Communication via services REST

Une approche plus moderne : créer un service REST en C# et l'appeler depuis Delphi.

#### Service REST C# (ASP.NET Core)

```csharp
// API Controller en C#
[ApiController]
[Route("api/[controller]")]
public class CalculController : ControllerBase
{
    [HttpGet("additionner")]
    public ActionResult<int> Additionner(int a, int b)
    {
        return Ok(a + b);
    }

    [HttpPost("traiter")]
    public ActionResult<object> TraiterDonnees([FromBody] int[] donnees)
    {
        return Ok(new {
            somme = donnees.Sum(),
            moyenne = donnees.Average(),
            max = donnees.Max()
        });
    }
}
```

#### Appeler le service depuis Delphi

```pascal
uses
  System.Net.HttpClient, System.JSON;

function AppelerServiceNET(a, b: Integer): Integer;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  URL := Format('http://localhost:5000/api/calcul/additionner?a=%d&b=%d', [a, b]);

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(URL);
    if Response.StatusCode = 200 then
      Result := StrToInt(Response.ContentAsString)
    else
      raise Exception.Create('Erreur service');
  finally
    HttpClient.Free;
  end;
end;
```

## Java et Delphi

### Java Native Interface (JNI)

Pour appeler Java depuis Delphi, vous pouvez utiliser JNI.

#### Créer une classe Java

```java
// Fichier: Calculatrice.java
public class Calculatrice {
    public static int additionner(int a, int b) {
        return a + b;
    }

    public static double calculerMoyenne(int[] tableau) {
        int somme = 0;
        for (int valeur : tableau) {
            somme += valeur;
        }
        return (double) somme / tableau.length;
    }
}

// Compiler: javac Calculatrice.java
// Créer JAR: jar cf calculatrice.jar Calculatrice.class
```

#### Interface Delphi-Java

```pascal
uses
  JNI;  // Nécessite une bibliothèque JNI pour Delphi

type
  TJavaWrapper = class
  private
    FJVM: PJavaVM;
    FEnv: PJNIEnv;
    FInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function InitJVM(const ClassPath: string): Boolean;
    function AppelerMethodeStatique(const ClassName, MethodName: string;
      const Args: array of Variant): Variant;
  end;

function TJavaWrapper.InitJVM(const ClassPath: string): Boolean;
var
  Args: JavaVMInitArgs;
  Options: array[0..1] of JavaVMOption;
begin
  Result := False;

  Options[0].optionString := PAnsiChar(AnsiString('-Djava.class.path=' + ClassPath));
  Options[1].optionString := PAnsiChar(AnsiString('-Xmx512m'));

  Args.version := JNI_VERSION_1_6;
  Args.nOptions := 2;
  Args.options := @Options[0];
  Args.ignoreUnrecognized := JNI_TRUE;

  if JNI_CreateJavaVM(@FJVM, @FEnv, @Args) = JNI_OK then
  begin
    FInitialized := True;
    Result := True;
  end;
end;

// Utilisation simplifiée
procedure ExempleJava;
var
  Java: TJavaWrapper;
  Resultat: Integer;
begin
  Java := TJavaWrapper.Create;
  try
    if Java.InitJVM('C:\MonProjet\calculatrice.jar') then
    begin
      Resultat := Java.AppelerMethodeStatique(
        'Calculatrice',
        'additionner',
        [10, 20]
      );
      ShowMessage('Résultat: ' + IntToStr(Resultat));
    end;
  finally
    Java.Free;
  end;
end;
```

### Alternative : Exécution de JAR

Plus simple : exécuter un JAR et communiquer via entrée/sortie standard.

```pascal
function ExecuterJAR(const JARPath, MainClass: string;
  const Args: string = ''): string;
var
  CommandLine: string;
  Output: TStringList;
begin
  CommandLine := Format('java -cp "%s" %s %s', [JARPath, MainClass, Args]);

  Output := TStringList.Create;
  try
    if ExecuteCaptureOutput(CommandLine, Output) then
      Result := Output.Text;
  finally
    Output.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  Resultat: string;
begin
  Resultat := ExecuterJAR(
    'C:\MonProjet\app.jar',
    'com.exemple.Main',
    '10 20'
  );
  Memo1.Lines.Text := Resultat;
end;
```

## Communication inter-processus (IPC)

### Pipes nommés

Les pipes permettent la communication bidirectionnelle entre processus.

#### Créer un serveur de pipe (Delphi)

```pascal
uses
  Winapi.Windows;

type
  TPipeServer = class(TThread)
  private
    FPipeName: string;
    FOnMessage: TProc<string>;
  protected
    procedure Execute; override;
  public
    constructor Create(const PipeName: string; OnMessage: TProc<string>);
  end;

constructor TPipeServer.Create(const PipeName: string; OnMessage: TProc<string>);
begin
  inherited Create(False);
  FPipeName := PipeName;
  FOnMessage := OnMessage;
  FreeOnTerminate := True;
end;

procedure TPipeServer.Execute;
var
  hPipe: THandle;
  Buffer: array[0..1023] of Char;
  BytesRead: DWORD;
  Message: string;
begin
  hPipe := CreateNamedPipe(
    PChar('\\.\pipe\' + FPipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    PIPE_UNLIMITED_INSTANCES,
    1024,
    1024,
    0,
    nil
  );

  if hPipe = INVALID_HANDLE_VALUE then
    Exit;

  try
    while not Terminated do
    begin
      if ConnectNamedPipe(hPipe, nil) then
      begin
        if ReadFile(hPipe, Buffer, SizeOf(Buffer), BytesRead, nil) then
        begin
          SetString(Message, Buffer, BytesRead div SizeOf(Char));

          if Assigned(FOnMessage) then
            Synchronize(procedure begin FOnMessage(Message); end);
        end;

        DisconnectNamedPipe(hPipe);
      end;
    end;
  finally
    CloseHandle(hPipe);
  end;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  TPipeServer.Create('MonPipe',
    procedure(Msg: string)
    begin
      Memo1.Lines.Add('Message reçu: ' + Msg);
    end);
end;
```

#### Client de pipe (peut être dans n'importe quel langage)

```pascal
function EnvoyerViaPipe(const PipeName, Message: string): Boolean;
var
  hPipe: THandle;
  BytesWritten: DWORD;
begin
  Result := False;

  hPipe := CreateFile(
    PChar('\\.\pipe\' + PipeName),
    GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if hPipe = INVALID_HANDLE_VALUE then
    Exit;

  try
    Result := WriteFile(hPipe, PChar(Message)^,
      Length(Message) * SizeOf(Char), BytesWritten, nil);
  finally
    CloseHandle(hPipe);
  end;
end;
```

#### Client Python pour le pipe

```python
# client_pipe.py
import win32pipe
import win32file

def envoyer_message(pipe_name, message):
    handle = win32file.CreateFile(
        f'\\\\.\\pipe\\{pipe_name}',
        win32file.GENERIC_WRITE,
        0,
        None,
        win32file.OPEN_EXISTING,
        0,
        None
    )

    win32file.WriteFile(handle, message.encode())
    handle.Close()

if __name__ == "__main__":
    envoyer_message("MonPipe", "Hello from Python!")
```

### Sockets TCP/IP

Les sockets permettent la communication réseau entre langages.

#### Serveur socket Delphi

```pascal
uses
  IdTCPServer, IdContext;

type
  TForm1 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    procedure FormCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IdTCPServer1.DefaultPort := 8080;
  IdTCPServer1.Active := True;
end;

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  Message: string;
  Response: string;
begin
  Message := AContext.Connection.IOHandler.ReadLn;

  // Traiter le message
  Response := 'Echo: ' + Message;

  // Envoyer la réponse
  AContext.Connection.IOHandler.WriteLn(Response);
end;
```

#### Client Python pour le serveur Delphi

```python
# client_socket.py
import socket

def envoyer_au_serveur_delphi(message):
    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client.connect(('localhost', 8080))

    client.send(message.encode() + b'\n')
    response = client.recv(1024).decode()

    client.close()
    return response

if __name__ == "__main__":
    reponse = envoyer_au_serveur_delphi("Hello Delphi!")
    print(reponse)
```

### Mémoire partagée

Pour des transferts de données très rapides entre processus.

```pascal
uses
  Winapi.Windows;

type
  TSharedMemory = class
  private
    FHandle: THandle;
    FData: Pointer;
    FSize: Integer;
    FName: string;
  public
    constructor Create(const Name: string; Size: Integer);
    destructor Destroy; override;
    procedure Write(const Data: string);
    function Read: string;
    property Data: Pointer read FData;
  end;

constructor TSharedMemory.Create(const Name: string; Size: Integer);
begin
  FName := Name;
  FSize := Size;

  FHandle := CreateFileMapping(
    INVALID_HANDLE_VALUE,
    nil,
    PAGE_READWRITE,
    0,
    FSize,
    PChar(FName)
  );

  if FHandle = 0 then
    raise Exception.Create('Impossible de créer la mémoire partagée');

  FData := MapViewOfFile(FHandle, FILE_MAP_ALL_ACCESS, 0, 0, FSize);

  if FData = nil then
  begin
    CloseHandle(FHandle);
    raise Exception.Create('Impossible de mapper la mémoire');
  end;
end;

destructor TSharedMemory.Destroy;
begin
  if FData <> nil then
    UnmapViewOfFile(FData);
  if FHandle <> 0 then
    CloseHandle(FHandle);
  inherited;
end;

procedure TSharedMemory.Write(const Data: string);
begin
  if Length(Data) < FSize then
    Move(PChar(Data)^, FData^, Length(Data) * SizeOf(Char));
end;

function TSharedMemory.Read: string;
begin
  SetString(Result, PChar(FData), StrLen(PChar(FData)));
end;

// Utilisation
procedure ExempleMemoire Partagee;
var
  Mem: TSharedMemory;
begin
  Mem := TSharedMemory.Create('MonMemoire', 1024);
  try
    Mem.Write('Données partagées');
    ShowMessage('Données écrites dans la mémoire partagée');
  finally
    Mem.Free;
  end;
end;
```

## Sérialisation de données

### Protocol Buffers (protobuf)

Format binaire efficace de Google, utilisable avec de nombreux langages.

```pascal
// Définition protobuf (message.proto)
// syntax = "proto3";
//
// message Personne {
//   string nom = 1;
//   int32 age = 2;
//   string email = 3;
// }

// Après compilation avec protoc, utiliser la bibliothèque Delphi appropriée
uses
  protobuf;  // Bibliothèque tierce

type
  TPersonne = class(TProtoBufMessage)
  private
    FNom: string;
    FAge: Integer;
    FEmail: string;
  published
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

procedure SerialiserProtobuf;
var
  Personne: TPersonne;
  Bytes: TBytes;
begin
  Personne := TPersonne.Create;
  try
    Personne.Nom := 'Jean Dupont';
    Personne.Age := 30;
    Personne.Email := 'jean@example.com';

    // Sérialiser
    Bytes := Personne.SerializeToBytes;

    // Envoyer les bytes à un autre processus/langage
    // ...
  finally
    Personne.Free;
  end;
end;
```

### MessagePack

Format binaire compact et rapide.

```pascal
uses
  System.JSON;

// Alternative simple : utiliser JSON comme format d'échange
function SerialiserJSON: string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('nom', 'Jean Dupont');
    JSON.AddPair('age', TJSONNumber.Create(30));
    JSON.AddPair('email', 'jean@example.com');
    Result := JSON.ToString;
  finally
    JSON.Free;
  end;
end;
```

## Bonnes pratiques

### Gestion des erreurs inter-langages

```pascal
type
  TInterlangageException = class(Exception)
  private
    FLanguage: string;
    FOriginalError: string;
  public
    constructor Create(const Language, OriginalError: string);
    property Language: string read FLanguage;
    property OriginalError: string read FOriginalError;
  end;

constructor TInterlangageException.Create(const Language, OriginalError: string);
begin
  inherited CreateFmt('Erreur depuis %s: %s', [Language, OriginalError]);
  FLanguage := Language;
  FOriginalError := OriginalError;
end;

procedure ExecuterAvecGestionErreur;
begin
  try
    ExecuterScriptPython('script.py');
  except
    on E: Exception do
      raise TInterlangageException.Create('Python', E.Message);
  end;
end;
```

### Logging inter-langages

```pascal
procedure LogInterlangueMessage(const Source, Message: string);
var
  LogFile: TextFile;
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  AssignFile(LogFile, 'interlangue.log');
  try
    if FileExists('interlangue.log') then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, Format('[%s] [%s] %s', [Timestamp, Source, Message]));
  finally
    CloseFile(LogFile);
  end;
end;
```

### Validation des données

```pascal
function ValiderDonneesJSON(const JSONText: string): Boolean;
var
  JSON: TJSONValue;
begin
  Result := False;
  try
    JSON := TJSONObject.ParseJSONValue(JSONText);
    try
      Result := JSON <> nil;
    finally
      JSON.Free;
    end;
  except
    Result := False;
  end;
end;
```

### Tests d'intégration

```pascal
procedure TesterIntegrationPython;
begin
  // Test 1 : Vérifier que Python est disponible
  if not ExecuterScriptPython('--version') then
    raise Exception.Create('Python non disponible');

  // Test 2 : Vérifier les bibliothèques requises
  if not ExecuterScriptPython('-c "import numpy"') then
    raise Exception.Create('NumPy non installé');

  // Test 3 : Test fonctionnel
  Assert(ExecuterScriptPython('test.py') = 'OK', 'Test fonctionnel échoué');

  ShowMessage('Tous les tests d''intégration ont réussi');
end;
```

## Résumé

L'interopérabilité entre langages ouvre des possibilités infinies pour vos applications Delphi.

**Points clés :**

1. **Python** : Python4Delphi pour l'intégration, scripts externes pour la simplicité
2. **JavaScript** : WebView pour les interfaces web, Node.js pour le backend
3. **C#/.NET** : COM pour l'intégration directe, REST pour les services
4. **Java** : JNI pour l'intégration native, exécution de JAR pour la simplicité
5. **IPC** : Pipes, sockets, mémoire partagée pour la communication
6. **Sérialisation** : JSON (simple), Protocol Buffers (efficace)
7. **Gestion d'erreurs** : Toujours capturer et gérer les erreurs inter-langages
8. **Performance** : Choisir la méthode adaptée (IPC rapide vs REST simple)
9. **Maintenance** : Bien documenter les interfaces entre langages
10. **Tests** : Tester l'intégration de bout en bout

Chaque méthode a ses avantages : l'intégration étroite offre performance et contrôle, tandis que l'exécution de processus séparés offre simplicité et isolation. Choisissez selon vos besoins spécifiques.

⏭️ [Liaison avec des API REST tierces](/14-utilisation-dapi-et-bibliotheques-externes/07-liaison-avec-des-api-rest-tierces.md)
