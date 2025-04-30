# 23.8 WebAssembly et Delphi

## Introduction à WebAssembly

WebAssembly (souvent abrégé Wasm) représente une avancée majeure dans le développement web. Il s'agit d'un format de code binaire conçu pour être exécuté dans les navigateurs web, offrant des performances proches du code natif. Pour les développeurs Delphi, WebAssembly ouvre de nouvelles possibilités pour porter des applications desktop sur le web tout en conservant des performances élevées.

![Note] Cette section présente les concepts de base de WebAssembly et son intégration avec Delphi. Les explications sont conçues pour être accessibles aux débutants, mais une compréhension de base du développement Delphi est recommandée.

## Qu'est-ce que WebAssembly?

WebAssembly est un format de code binaire compact et efficace, conçu pour être:

- **Rapide**: Le code WebAssembly s'exécute presque aussi rapidement que le code natif
- **Sécurisé**: Il s'exécute dans un environnement sandbox avec une mémoire isolée
- **Portable**: Le même code peut fonctionner sur différents systèmes d'exploitation et architectures
- **Déboguable**: Supporte les outils de débogage standard
- **Ouvert**: C'est un standard web ouvert, supporté par tous les navigateurs majeurs

WebAssembly n'est pas destiné à remplacer JavaScript, mais plutôt à le compléter en prenant en charge les tâches nécessitant de hautes performances comme:
- Traitement d'image et vidéo
- Jeux 3D
- Simulations et visualisations complexes
- Applications de bureau portées vers le web

## Comment WebAssembly fonctionne avec Delphi

À ce jour, Delphi ne propose pas de support natif intégré pour compiler directement vers WebAssembly. Cependant, il existe plusieurs approches pour utiliser WebAssembly dans vos projets Delphi:

1. **Utilisation d'outils tiers** comme pas.js ou Smart Pascal
2. **Intégration de modules WebAssembly** créés avec d'autres langages
3. **Solutions hybrides** combinant Delphi avec des technologies web

Dans ce chapitre, nous explorerons ces approches et vous montrerons comment tirer parti de WebAssembly dans vos applications Delphi.

## Comprendre les bases de WebAssembly

Avant de plonger dans l'intégration avec Delphi, il est important de comprendre quelques concepts de base:

### Format de fichier WebAssembly

WebAssembly utilise deux formats principaux:
- **Format binaire (.wasm)**: Format compact utilisé en production
- **Format texte (.wat)**: Format lisible par l'humain, utilisé principalement pour le développement et le débogage

### Cycle de vie d'une application WebAssembly

1. **Compilation**: Le code source (C, C++, Rust, etc.) est compilé en module WebAssembly (.wasm)
2. **Chargement**: Le navigateur télécharge le module WebAssembly
3. **Instantiation**: Le module est instancié et préparé pour l'exécution
4. **Exécution**: Les fonctions WebAssembly sont appelées, généralement depuis JavaScript
5. **Interaction**: WebAssembly interagit avec le DOM et les API web via JavaScript

### Limites de WebAssembly

WebAssembly a certaines limitations qu'il faut connaître:
- Pas d'accès direct au DOM (doit passer par JavaScript)
- Pas d'accès direct aux API Web comme fetch, WebGL, etc.
- Gestion manuelle de la mémoire (pas de garbage collector intégré)

## Approches pour utiliser WebAssembly avec Delphi

### 1. Conversion de code Delphi avec pas.js

[pas.js](https://github.com/smartmobilestudio/pas2js) est un compilateur qui permet de convertir du code Pascal en JavaScript. Bien que ce ne soit pas directement du WebAssembly, c'est une passerelle pour porter des applications Delphi vers le web:

1. **Installation de pas.js**:
   ```
   npm install -g pas.js
   ```

2. **Exemple de conversion d'une unité Delphi simple**:
   ```pascal
   // MathUnit.pas
   unit MathUnit;

   interface

   function AddNumbers(A, B: Integer): Integer;

   implementation

   function AddNumbers(A, B: Integer): Integer;
   begin
     Result := A + B;
   end;

   end.
   ```

3. **Conversion avec pas.js**:
   ```
   pas.js MathUnit.pas
   ```

4. **Utilisation dans une page web**:
   ```html
   <!DOCTYPE html>
   <html>
   <head>
     <title>Test pas.js</title>
     <script src="MathUnit.js"></script>
     <script>
       document.addEventListener('DOMContentLoaded', function() {
         // Utiliser la fonction convertie
         const result = MathUnit.AddNumbers(5, 3);
         document.getElementById('result').textContent = result;
       });
     </script>
   </head>
   <body>
     <h1>Résultat: <span id="result"></span></h1>
   </body>
   </html>
   ```

### 2. Intégration de modules WebAssembly externes

Vous pouvez intégrer des modules WebAssembly créés avec d'autres langages dans votre application web Delphi. Voici comment:

1. **Créez un module WebAssembly** (par exemple avec C/C++ et Emscripten)

2. **Ajoutez une action dans votre WebModule Delphi pour servir le fichier .wasm**:

```delphi
procedure TWebModule1.ServeWasmFileAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  FilePath: string;
  FileStream: TFileStream;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'www\module.wasm';

  if FileExists(FilePath) then
  begin
    Response.ContentType := 'application/wasm';
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      Response.ContentStream := FileStream;
      Response.FreeContentStream := True;
    except
      FileStream.Free;
      raise;
    end;
  end
  else
  begin
    Response.StatusCode := 404;
    Response.Content := 'WebAssembly module not found';
  end;

  Handled := True;
end;
```

3. **Chargez et utilisez le module WebAssembly dans votre page web**:

```html
<script>
  async function loadWasmModule() {
    try {
      // Charger le module WebAssembly
      const response = await fetch('/module.wasm');
      const bytes = await response.arrayBuffer();
      const wasmModule = await WebAssembly.instantiate(bytes, {
        env: {
          // Fonctions importées que le module peut appeler
          consoleLog: function(arg) {
            console.log(arg);
          }
        }
      });

      // Accéder aux fonctions exportées
      const instance = wasmModule.instance;
      const result = instance.exports.calculateSomething(10, 20);

      document.getElementById('wasm-result').textContent = result;
    } catch (error) {
      console.error('Erreur lors du chargement du module WebAssembly:', error);
    }
  }

  // Charger le module au chargement de la page
  document.addEventListener('DOMContentLoaded', loadWasmModule);
</script>
```

## Exemple pratique: Traitement d'image avec WebAssembly et Delphi

Imaginons que nous voulions intégrer un traitement d'image haute performance dans notre application web Delphi. Voici comment nous pourrions procéder:

### Étape 1: Créer un module WebAssembly pour le traitement d'image

Pour cet exemple, nous utiliserons Emscripten pour compiler du code C++ en WebAssembly. Voici un exemple de code C++ pour appliquer un filtre de niveaux de gris:

```cpp
// grayscale.cpp
#include <emscripten.h>
#include <stdint.h>

// Fonction exportée vers JavaScript
extern "C" {
  EMSCRIPTEN_KEEPALIVE
  void applyGrayscale(uint8_t* data, int size) {
    for (int i = 0; i < size; i += 4) {
      // Formule standard pour convertir RGB en niveaux de gris
      uint8_t gray = (uint8_t)(0.299 * data[i] + 0.587 * data[i + 1] + 0.114 * data[i + 2]);
      data[i] = gray;     // R
      data[i + 1] = gray; // G
      data[i + 2] = gray; // B
      // Conserver le canal alpha (i + 3)
    }
  }
}
```

Compilation avec Emscripten:
```
emcc grayscale.cpp -o grayscale.js -s WASM=1 -s EXPORTED_FUNCTIONS='["_applyGrayscale"]' -s EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -O3
```

### Étape 2: Créer une page web Delphi qui utilise le module WebAssembly

```delphi
function TWebModule1.GenerateImageEditorHTML: string;
begin
  Result :=
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head>' + #13#10 +
    '  <title>Éditeur d''image WebAssembly + Delphi</title>' + #13#10 +
    '  <style>' + #13#10 +
    '    body { font-family: Arial, sans-serif; margin: 20px; }' + #13#10 +
    '    .container { display: flex; flex-wrap: wrap; gap: 20px; }' + #13#10 +
    '    canvas { border: 1px solid #ccc; }' + #13#10 +
    '    .controls { display: flex; gap: 10px; margin-bottom: 20px; }' + #13#10 +
    '    button { padding: 8px 16px; background-color: #4a6da7; color: white; border: none; cursor: pointer; }' + #13#10 +
    '    button:hover { background-color: #3a5d97; }' + #13#10 +
    '  </style>' + #13#10 +
    '</head>' + #13#10 +
    '<body>' + #13#10 +
    '  <h1>Éditeur d''image avec WebAssembly et Delphi</h1>' + #13#10 +
    '  <div class="controls">' + #13#10 +
    '    <button id="load-image">Charger une image</button>' + #13#10 +
    '    <button id="apply-grayscale" disabled>Appliquer niveaux de gris</button>' + #13#10 +
    '    <button id="save-image" disabled>Enregistrer l''image</button>' + #13#10 +
    '  </div>' + #13#10 +
    '  <input type="file" id="file-input" style="display: none;" accept="image/*">' + #13#10 +
    '  <div class="container">' + #13#10 +
    '    <div>' + #13#10 +
    '      <h3>Image originale</h3>' + #13#10 +
    '      <canvas id="source-canvas"></canvas>' + #13#10 +
    '    </div>' + #13#10 +
    '    <div>' + #13#10 +
    '      <h3>Image traitée</h3>' + #13#10 +
    '      <canvas id="target-canvas"></canvas>' + #13#10 +
    '    </div>' + #13#10 +
    '  </div>' + #13#10 +
    '  <script src="grayscale.js"></script>' + #13#10 +
    '  <script>' + #13#10 +
    '    let sourceCanvas = document.getElementById("source-canvas");' + #13#10 +
    '    let targetCanvas = document.getElementById("target-canvas");' + #13#10 +
    '    let sourceCtx = sourceCanvas.getContext("2d");' + #13#10 +
    '    let targetCtx = targetCanvas.getContext("2d");' + #13#10 +
    '    let loadedImage = null;' + #13#10 +
    '' + #13#10 +
    '    // Attendre que le module WebAssembly soit chargé' + #13#10 +
    '    Module.onRuntimeInitialized = function() {' + #13#10 +
    '      console.log("Module WebAssembly chargé");' + #13#10 +
    '    };' + #13#10 +
    '' + #13#10 +
    '    document.getElementById("load-image").addEventListener("click", function() {' + #13#10 +
    '      document.getElementById("file-input").click();' + #13#10 +
    '    });' + #13#10 +
    '' + #13#10 +
    '    document.getElementById("file-input").addEventListener("change", function(e) {' + #13#10 +
    '      const file = e.target.files[0];' + #13#10 +
    '      if (file) {' + #13#10 +
    '        const reader = new FileReader();' + #13#10 +
    '        reader.onload = function(event) {' + #13#10 +
    '          loadedImage = new Image();' + #13#10 +
    '          loadedImage.onload = function() {' + #13#10 +
    '            // Redimensionner les canvas pour correspondre à l''image' + #13#10 +
    '            sourceCanvas.width = loadedImage.width;' + #13#10 +
    '            sourceCanvas.height = loadedImage.height;' + #13#10 +
    '            targetCanvas.width = loadedImage.width;' + #13#10 +
    '            targetCanvas.height = loadedImage.height;' + #13#10 +
    '' + #13#10 +
    '            // Dessiner l''image originale' + #13#10 +
    '            sourceCtx.drawImage(loadedImage, 0, 0);' + #13#10 +
    '            targetCtx.drawImage(loadedImage, 0, 0);' + #13#10 +
    '' + #13#10 +
    '            // Activer les boutons' + #13#10 +
    '            document.getElementById("apply-grayscale").disabled = false;' + #13#10 +
    '            document.getElementById("save-image").disabled = false;' + #13#10 +
    '          };' + #13#10 +
    '          loadedImage.src = event.target.result;' + #13#10 +
    '        };' + #13#10 +
    '        reader.readAsDataURL(file);' + #13#10 +
    '      }' + #13#10 +
    '    });' + #13#10 +
    '' + #13#10 +
    '    document.getElementById("apply-grayscale").addEventListener("click", function() {' + #13#10 +
    '      if (!loadedImage) return;' + #13#10 +
    '' + #13#10 +
    '      // Copier l''image originale sur le canvas cible' + #13#10 +
    '      targetCtx.drawImage(loadedImage, 0, 0);' + #13#10 +
    '' + #13#10 +
    '      // Obtenir les données d''image' + #13#10 +
    '      let imageData = targetCtx.getImageData(0, 0, targetCanvas.width, targetCanvas.height);' + #13#10 +
    '      let data = imageData.data;' + #13#10 +
    '' + #13#10 +
    '      // Créer un tampon de mémoire pour WebAssembly' + #13#10 +
    '      let buffer = Module._malloc(data.length);' + #13#10 +
    '      Module.HEAPU8.set(data, buffer);' + #13#10 +
    '' + #13#10 +
    '      // Appeler la fonction WebAssembly' + #13#10 +
    '      Module.ccall(' + #13#10 +
    '        "applyGrayscale",    // nom de la fonction' + #13#10 +
    '        null,                // type de retour' + #13#10 +
    '        ["number", "number"], // types des arguments' + #13#10 +
    '        [buffer, data.length] // valeurs des arguments' + #13#10 +
    '      );' + #13#10 +
    '' + #13#10 +
    '      // Copier le résultat dans les données d''image' + #13#10 +
    '      let result = Module.HEAPU8.subarray(buffer, buffer + data.length);' + #13#10 +
    '      for (let i = 0; i < data.length; i++) {' + #13#10 +
    '        data[i] = result[i];' + #13#10 +
    '      }' + #13#10 +
    '' + #13#10 +
    '      // Libérer la mémoire' + #13#10 +
    '      Module._free(buffer);' + #13#10 +
    '' + #13#10 +
    '      // Mettre à jour le canvas' + #13#10 +
    '      targetCtx.putImageData(imageData, 0, 0);' + #13#10 +
    '    });' + #13#10 +
    '' + #13#10 +
    '    document.getElementById("save-image").addEventListener("click", function() {' + #13#10 +
    '      // Créer un lien de téléchargement' + #13#10 +
    '      const link = document.createElement("a");' + #13#10 +
    '      link.download = "image-traitée.png";' + #13#10 +
    '      link.href = targetCanvas.toDataURL("image/png");' + #13#10 +
    '      link.click();' + #13#10 +
    '    });' + #13#10 +
    '  </script>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;

procedure TWebModule1.ImageEditorActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content := GenerateImageEditorHTML;
  Handled := True;
end;
```

### Étape 3: Ajouter les actions nécessaires pour servir les fichiers

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  Action: TWebActionItem;
begin
  // ... actions existantes ...

  // Ajouter l'action pour l'éditeur d'image
  Action := Actions.Add;
  Action.Name := 'ImageEditorAction';
  Action.PathInfo := '/image-editor';
  Action.MethodType := mtGet;
  Action.OnAction := ImageEditorActionAction;

  // Ajouter l'action pour servir les fichiers WebAssembly
  Action := Actions.Add;
  Action.Name := 'ServeWasmAction';
  Action.PathInfo := '/grayscale.wasm';
  Action.MethodType := mtGet;
  Action.OnAction := ServeWasmFileAction;

  // Ajouter l'action pour servir le fichier JavaScript généré par Emscripten
  Action := Actions.Add;
  Action.Name := 'ServeJSAction';
  Action.PathInfo := '/grayscale.js';
  Action.MethodType := mtGet;
  Action.OnAction := ServeJSFileAction;
end;
```

## WebAssembly avec TMS WEB Core

TMS WEB Core est une extension pour Delphi qui permet de développer des applications web en utilisant le langage Pascal. Bien que TMS WEB Core ne compile pas directement vers WebAssembly, il peut être utilisé en conjonction avec des modules WebAssembly.

Voici comment intégrer un module WebAssembly dans une application TMS WEB Core:

```delphi
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.WebCtrls;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebMemo1: TWebMemo;
    procedure WebButton1Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
  private
    procedure LoadWasmModule;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  LoadWasmModule;
end;

procedure TForm1.LoadWasmModule;
begin
  asm
    // Charger le module WebAssembly
    fetch('grayscale.wasm')
      .then(response => response.arrayBuffer())
      .then(bytes => WebAssembly.instantiate(bytes, {
        env: {
          consoleLog: function(arg) {
            console.log(arg);
          }
        }
      }))
      .then(results => {
        // Stocker l'instance du module dans une variable globale
        window.wasmModule = results.instance;
        this.WebMemo1.Lines.Add('Module WebAssembly chargé avec succès!');
      })
      .catch(error => {
        console.error('Erreur lors du chargement du module WebAssembly:', error);
        this.WebMemo1.Lines.Add('Erreur: ' + error.toString());
      });
  end;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  asm
    // Vérifier si le module est chargé
    if (window.wasmModule) {
      // Appeler une fonction exportée du module WebAssembly
      const result = window.wasmModule.exports.calculateSomething(10, 20);
      this.WebMemo1.Lines.Add('Résultat: ' + result);
    } else {
      this.WebMemo1.Lines.Add('Module WebAssembly non chargé');
    }
  end;
end;

end.
```

## L'avenir: Delphi et WebAssembly

Le support officiel de WebAssembly dans Delphi pourrait évoluer dans le futur. Voici quelques possibilités:

### Compilation directe de Delphi vers WebAssembly

À l'avenir, Embarcadero pourrait ajouter le support natif pour compiler directement du code Delphi en WebAssembly, ce qui permettrait:
- De porter facilement des applications Delphi existantes vers le web
- D'utiliser les composants VCL ou FMX dans des applications web
- De réutiliser le code business existant dans des applications web

### Intégration plus profonde avec TMS WEB Core

TMS WEB Core pourrait évoluer pour offrir une meilleure intégration avec WebAssembly, permettant:
- Un mélange transparent de code Pascal compilé en JavaScript et de modules WebAssembly
- Des performances améliorées pour les parties critiques des applications
- Une transition en douceur des applications desktop vers le web

## Bonnes pratiques pour l'utilisation de WebAssembly avec Delphi

1. **Identifier les parties critiques en termes de performance**: Ne pas tout migrer vers WebAssembly, mais se concentrer sur les fonctionnalités nécessitant des performances élevées.

2. **Gérer correctement la mémoire**: WebAssembly requiert une gestion manuelle de la mémoire, soyez attentif aux allocations et libérations.

3. **Déboguer efficacement**: Utilisez les outils de débogage intégrés aux navigateurs pour identifier les problèmes.

4. **Considérer la taille des modules**: Les gros modules prennent plus de temps à télécharger et à instancier. Divisez-les si nécessaire.

5. **Tester sur différents navigateurs**: Bien que WebAssembly soit supporté par tous les navigateurs modernes, il peut y avoir des différences subtiles.

## Conclusion

WebAssembly ouvre de nouvelles portes pour les développeurs Delphi souhaitant créer des applications web hautes performances. Bien que le support natif soit encore limité, les outils et approches existants permettent déjà d'intégrer WebAssembly dans les projets Delphi.

Que ce soit pour porter des applications existantes vers le web ou pour améliorer les performances des nouvelles applications web, WebAssembly représente une technologie prometteuse qui s'intègre bien dans l'écosystème Delphi.

À mesure que la technologie évolue, nous pouvons nous attendre à une intégration encore plus profonde entre Delphi et WebAssembly, offrant aux développeurs le meilleur des deux mondes: la productivité et la facilité d'utilisation de Delphi, combinées avec les performances et la portabilité de WebAssembly.
