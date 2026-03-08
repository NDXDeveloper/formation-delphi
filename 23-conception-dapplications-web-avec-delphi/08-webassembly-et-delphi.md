🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.8 WebAssembly et Delphi

## Introduction

**WebAssembly** (souvent abrégé **WASM**) est une technologie révolutionnaire qui change la donne pour les applications web. Imaginez pouvoir exécuter du code compilé, presque aussi rapide que du code natif, directement dans un navigateur web. C'est exactement ce que permet WebAssembly !

Dans cette section, nous allons explorer ce qu'est WebAssembly, pourquoi c'est important, et comment cela s'intègre (ou pourrait s'intégrer) avec l'écosystème Delphi.

**Note importante :** À ce jour (2025), Delphi ne compile pas nativement en WebAssembly. Cependant, comprendre cette technologie est essentiel car elle représente l'avenir du développement web haute performance, et Embarcadero pourrait l'intégrer dans les futures versions.

## Qu'est-ce que WebAssembly ?

### Définition simple

**WebAssembly** est un format de code binaire conçu pour être exécuté dans les navigateurs web. C'est un langage de bas niveau (proche du langage machine) qui peut être exécuté à vitesse quasi-native.

**Analogie :**
- **JavaScript** = Une langue que tout le monde peut lire et comprendre, mais qui prend du temps à traduire
- **WebAssembly** = Du code déjà "pré-traduit" en instructions machine, prêt à être exécuté immédiatement

### Avant WebAssembly

```
┌─────────────────────────┐
│  Code JavaScript        │
│  (texte source)         │
└────────────┬────────────┘
             │
             │ Interprétation + Compilation JIT
             │ (lent au démarrage)
             ↓
┌─────────────────────────┐
│  Exécution navigateur   │
└─────────────────────────┘
```

### Avec WebAssembly

```
┌─────────────────────────┐
│  Code source            │
│  (C++, Rust, etc.)      │
└────────────┬────────────┘
             │
             │ Compilation native
             ↓
┌─────────────────────────┐
│  WebAssembly (.wasm)    │
│  (code binaire)         │
└────────────┬────────────┘
             │
             │ Exécution directe
             │ (très rapide)
             ↓
┌─────────────────────────┐
│  Navigateur             │
└─────────────────────────┘
```

### Les caractéristiques de WebAssembly

**1. Performance**
- Exécution à vitesse quasi-native (80-90% de la vitesse native)
- Pas de phase d'interprétation
- Optimisé pour les opérations intensives

**2. Portabilité**
- Fonctionne sur tous les navigateurs modernes
- Indépendant de la plateforme
- Standard W3C

**3. Sécurité**
- Exécution dans un environnement sandboxé
- Pas d'accès direct au système
- Mêmes garanties de sécurité que JavaScript

**4. Compacité**
- Format binaire compact
- Téléchargement rapide
- Parsing efficace

**5. Interopérabilité**
- Peut appeler JavaScript et vice-versa
- Partage de mémoire possible
- Intégration transparente avec le web

## Pourquoi WebAssembly est important ?

### 1. Performance web accrue

WebAssembly permet d'exécuter des applications complexes dans le navigateur :
- Jeux 3D
- Logiciels de traitement d'image/vidéo
- Simulations scientifiques
- CAO/DAO en ligne
- Applications de productivité avancées

**Exemple :** AutoCAD Web, Adobe Photoshop Web, Google Earth utilisent WebAssembly.

### 2. Réutilisation de code existant

Vous avez une bibliothèque C++ performante ? Compilez-la en WebAssembly et utilisez-la dans le navigateur !

**Cas d'usage réels :**
- Bibliothèques de cryptographie
- Moteurs de jeu (Unity, Unreal Engine)
- Codecs audio/vidéo
- Algorithmes de compression
- Moteurs de physique

### 3. Langage agnostique

Contrairement à JavaScript, WebAssembly n'est lié à aucun langage source :

```
┌─────────────┐
│     C++     │──┐
└─────────────┘  │
┌─────────────┐  │
│    Rust     │──┤
└─────────────┘  │
┌─────────────┐  │    ┌──────────────┐    ┌──────────────┐
│      Go     │──┼───→│ WebAssembly  │───→│  Navigateur  │
└─────────────┘  │    └──────────────┘    └──────────────┘
┌─────────────┐  │
│  C# (Blazor)│──┤
└─────────────┘  │
┌─────────────┐  │
│  (Delphi?)  │──┘
└─────────────┘
```

### 4. Nouvelle ère pour le web

WebAssembly ouvre la porte à des applications qui n'étaient pas envisageables auparavant :
- Applications desktop migrées vers le web
- Logiciels professionnels en ligne
- Performance comparable aux applications natives

## Comment fonctionne WebAssembly ?

### Structure d'un fichier WASM

Un fichier `.wasm` contient :
- **Code** : Instructions en bytecode
- **Mémoire** : Description de la mémoire nécessaire
- **Tables** : Références aux fonctions
- **Imports/Exports** : Interface avec JavaScript

**Format texte (WAT - WebAssembly Text) :**
```wat
(module
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )
  (export "add" (func $add))
)
```

**Équivalent en code machine binaire (.wasm) :**
```
00 61 73 6D 01 00 00 00 01 07 01 60 02 7F 7F 01 7F...
```

### Intégration avec JavaScript

**Charger et utiliser un module WebAssembly en JavaScript :**

```javascript
// Charger le fichier WASM
fetch('module.wasm')
  .then(response => response.arrayBuffer())
  .then(bytes => WebAssembly.instantiate(bytes))
  .then(results => {
    const instance = results.instance;

    // Appeler une fonction exportée
    const result = instance.exports.add(5, 10);
    console.log('Résultat:', result); // 15
  });
```

### Communication bidirectionnelle

```
┌─────────────────────────┐
│     JavaScript          │
│  (logique UI, DOM)      │
└───────────┬─────────────┘
            │
            │ Appels de fonctions
            │ Partage de mémoire
            ↕
┌───────────┴─────────────┐
│    WebAssembly          │
│  (calculs intensifs)    │
└─────────────────────────┘
```

**Exemple :**
```javascript
// JavaScript appelle WASM pour calcul intensif
const result = wasmModule.exports.calculateMandelbrot(width, height);

// WASM peut appeler JavaScript (via imports)
const wasmImports = {
  env: {
    log: (value) => console.log('Depuis WASM:', value),
    updateProgress: (percent) => updateProgressBar(percent)
  }
};
```

## WebAssembly vs JavaScript

### Comparaison de performance

| Opération | JavaScript | WebAssembly | Gain |
|-----------|-----------|-------------|------|
| Calculs mathématiques | 100ms | 10ms | 10x |
| Traitement d'image | 1000ms | 100ms | 10x |
| Compression de données | 500ms | 50ms | 10x |
| Rendu 3D | 60fps difficile | 60fps stable | Fluide |

**Note :** Les gains varient selon les cas d'usage. JavaScript moderne (avec JIT) est déjà très rapide pour beaucoup d'opérations.

### Quand utiliser WebAssembly ?

**✅ Utiliser WebAssembly pour :**
- Calculs intensifs (cryptographie, compression, physique)
- Traitement média (image, audio, vidéo)
- Jeux et graphiques 3D
- Simulations scientifiques
- Port d'applications C/C++ existantes
- Performance critique

**✅ Utiliser JavaScript pour :**
- Manipulation du DOM
- Gestion des événements
- Logique UI
- Appels API REST
- Code métier simple
- Intégration avec bibliothèques web

**✅ Utiliser les deux ensemble :**
- JavaScript pour l'interface
- WebAssembly pour les calculs
- Communication via API

## État actuel : Delphi et WebAssembly

### Situation en 2025

**Compilation native Delphi → WebAssembly :** ❌ **Pas disponible officiellement**

Embarcadero n'a pas encore publié de compilateur Delphi vers WebAssembly. Cependant, plusieurs solutions existent ou sont en développement :

### 1. TMS Web Core (JavaScript, pas WASM)

**TMS Web Core** compile Delphi vers **JavaScript**, pas WebAssembly :

```
Code Delphi (Pascal)
        ↓
   TMS Web Core
        ↓
   JavaScript
        ↓
   Navigateur
```

**Avantages :**
- Disponible maintenant
- Productif et mature
- Syntaxe Delphi familière

**Limitations :**
- Performance JavaScript (pas WASM)
- Pas d'accès au code natif Delphi

### 2. Projets communautaires

Quelques projets tentent de créer des ponts :

**A. Pas2JS → WASM**
- Compilateur Pascal open-source
- Génère JavaScript actuellement
- WebAssembly envisagé pour le futur

**B. LLVM et Delphi**
- Utiliser LLVM comme backend
- LLVM peut générer WebAssembly
- Pas encore mature pour Delphi

### 3. Solutions hybrides

**Approche actuelle recommandée :**

```
┌──────────────────────────┐
│  Frontend                │
│  TMS Web Core            │
│  (JavaScript)            │
└────────────┬─────────────┘
             │
             │ API REST
             │
┌────────────┴─────────────┐
│  Backend                 │
│  Delphi natif            │
│  (Performance maximale)  │
└──────────────────────────┘
```

Cette architecture vous donne :
- Interface web moderne (TMS Web Core)
- Performance native pour la logique (Delphi serveur)
- Meilleur des deux mondes

## Comment d'autres langages utilisent WebAssembly

### C/C++ avec Emscripten

**Emscripten** est la chaîne de compilation la plus mature pour WebAssembly.

```bash
# Compiler du C++ vers WebAssembly
emcc hello.cpp -o hello.html
```

**Code C++ :**
```cpp
#include <emscripten.h>
#include <stdio.h>

EMSCRIPTEN_KEEPALIVE  
int fibonacci(int n) {  
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

**Utilisation en JavaScript :**
```javascript
const result = Module._fibonacci(10);  
console.log('Fibonacci(10):', result);  
```

### Rust avec wasm-pack

**Rust** a un excellent support WebAssembly.

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Bonjour, {} depuis Rust!", name)
}
```

**Compilation :**
```bash
wasm-pack build --target web
```

### Go avec TinyGo

**TinyGo** permet de compiler Go vers WebAssembly.

```go
package main

import "syscall/js"

func add(this js.Value, args []js.Value) interface{} {
    return args[0].Int() + args[1].Int()
}

func main() {
    js.Global().Set("add", js.FuncOf(add))
    select {}
}
```

### C# avec Blazor WebAssembly

**Microsoft Blazor** compile C# vers WebAssembly.

```csharp
@page "/counter"

<h1>Counter</h1>
<p>Current count: @currentCount</p>
<button @onclick="IncrementCount">Click me</button>

@code {
    private int currentCount = 0;

    private void IncrementCount()
    {
        currentCount++;
    }
}
```

## Scénarios d'intégration conceptuels

### Scénario 1 : Bibliothèque de calcul

**Hypothèse :** Si Delphi supportait WebAssembly

```pascal
// Unit de calcul Delphi (hypothétique)
unit MathLib;

interface

function CalculateComplexFormula(A, B, C: Double): Double; export;

implementation

function CalculateComplexFormula(A, B, C: Double): Double;  
var  
  i: Integer;
begin
  Result := 0;
  for i := 1 to 1000000 do
    Result := Result + (A * B + C) / i;
end;

end.
```

**Compilation (hypothétique) :**
```bash
# Commande hypothétique
dcc32 -target:wasm MathLib.dpr -o:mathlib.wasm
```

**Utilisation en JavaScript :**
```javascript
// Charger le module Delphi compilé
const delphiModule = await WebAssembly.instantiateStreaming(
  fetch('mathlib.wasm')
);

// Utiliser la fonction Delphi
const result = delphiModule.exports.CalculateComplexFormula(10.5, 20.3, 5.7);  
console.log('Résultat du calcul Delphi:', result);  
```

### Scénario 2 : Moteur de jeu

**Application hybride :**

```
┌─────────────────────────────────────┐
│  Interface utilisateur              │
│  (HTML/CSS/JavaScript ou TMS Web)   │
│  - Menus                            │
│  - HUD                              │
│  - Paramètres                       │
└──────────────┬──────────────────────┘
               │
               │ Appels WebAssembly
               │
┌──────────────┴──────────────────────┐
│  Moteur de jeu Delphi (WASM)        │
│  - Physique                         │
│  - Rendu 3D                         │
│  - IA                               │
│  - Logique de jeu                   │
└─────────────────────────────────────┘
```

### Scénario 3 : Traitement d'images

```pascal
// Hypothétique : Filtre d'image en Delphi/WASM
unit ImageFilter;

interface

type
  TPixelArray = array of Byte;

procedure ApplyBlur(var Pixels: TPixelArray; Width, Height: Integer); export;

implementation

procedure ApplyBlur(var Pixels: TPixelArray; Width, Height: Integer);  
var  
  x, y, i: Integer;
  Sum: Integer;
begin
  // Algorithme de flou
  for y := 1 to Height - 2 do
    for x := 1 to Width - 2 do
    begin
      Sum := 0;
      for i := -1 to 1 do
        Sum := Sum + Pixels[(y + i) * Width + (x + i)];
      Pixels[y * Width + x] := Sum div 3;
    end;
end;

end.
```

**Utilisation côté web :**
```javascript
// Obtenir les pixels d'une image Canvas
const ctx = canvas.getContext('2d');  
const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);  

// Passer à WebAssembly Delphi pour traitement
const memory = new Uint8Array(wasmModule.memory.buffer);  
memory.set(imageData.data);  

wasmModule.exports.ApplyBlur(
  0, // Pointeur vers les données en mémoire WASM
  canvas.width,
  canvas.height
);

// Récupérer les pixels traités
imageData.data.set(memory.slice(0, imageData.data.length));  
ctx.putImageData(imageData, 0, 0);  
```

## Outils et écosystème WebAssembly

### 1. WABT (WebAssembly Binary Toolkit)

Outils pour travailler avec WebAssembly :

```bash
# Convertir WASM en texte lisible (WAT)
wasm2wat module.wasm -o module.wat

# Convertir WAT en WASM
wat2wasm module.wat -o module.wasm

# Valider un module WASM
wasm-validate module.wasm

# Désassembler un module
wasm-objdump -d module.wasm
```

### 2. Wasmer et Wasmtime

Runtimes WebAssembly standalone (hors navigateur) :

```bash
# Exécuter WASM en ligne de commande
wasmer run module.wasm

# Avec Wasmtime
wasmtime module.wasm
```

**Cas d'usage :** Plugins, extensions, sandbox sécurisé

### 3. AssemblyScript

Langage proche de TypeScript qui compile vers WebAssembly :

```typescript
export function add(a: i32, b: i32): i32 {
  return a + b;
}
```

```bash
asc assembly/index.ts -b build/optimized.wasm
```

### 4. WasmEdge

Runtime WebAssembly pour cloud et edge computing :
- Exécution hors navigateur
- Support pour serveurs
- IoT et edge computing

## Limitations actuelles de WebAssembly

### Limitations techniques

**1. Pas d'accès direct au DOM**
- WebAssembly ne peut pas manipuler le DOM directement
- Doit passer par JavaScript pour toute interaction UI

**2. Garbage Collection limitée**
- Pas de GC standard encore (en développement)
- Gestion mémoire manuelle nécessaire
- Complexe pour langages avec GC (comme Delphi)

**3. Exceptions**
- Support des exceptions en cours d'implémentation
- Pas de gestion d'exceptions complète

**4. Threading**
- Support multi-threading limité
- SharedArrayBuffer avec restrictions de sécurité

**5. Taille de téléchargement**
- Fichiers WASM peuvent être volumineux
- Important d'optimiser et compresser

### Limitations pour Delphi spécifiquement

**1. Runtime Delphi**
- Le runtime Delphi (RTL) devrait être adapté pour WASM
- Classes, RTTI, gestion mémoire à porter

**2. VCL/FMX**
- Frameworks UI impossibles en WASM
- Nécessiterait réécriture complète

**3. Composants tiers**
- Écosystème de composants à réinventer
- Dépendances Windows à éliminer

**4. Debugging**
- Outils de débogage WASM limités
- Expérience de développement à améliorer

## Perspectives futures

### Ce qui arrive dans WebAssembly

**WASI (WebAssembly System Interface)**
- Interface standard pour accès système
- Exécution hors navigateur standardisée
- Portabilité universelle

**Component Model**
- Composants réutilisables
- Interopérabilité entre langages
- Écosystème de modules

**Garbage Collection**
- GC natif en cours de développement
- Facilitera les langages managés

**SIMD (Single Instruction Multiple Data)**
- Instructions vectorielles
- Performance accrue pour calculs parallèles

### Et Delphi ?

**Scénarios possibles :**

**1. Support officiel Embarcadero** (espéré)
- Compilateur Delphi → WebAssembly
- RTL adapté pour le web
- Framework UI web natif

**2. Projets communautaires**
- Compilateurs alternatifs
- Transpileurs Delphi → Rust/C++ → WASM
- Outils de pont

**3. Approche hybride** (actuelle)
- TMS Web Core pour frontend (JavaScript)
- Delphi natif pour backend
- Communication via API REST

## Stratégies actuelles pour développeurs Delphi

### Stratégie 1 : Architecture moderne

```
┌────────────────────────────┐
│  Frontend Web              │
│  TMS Web Core / JavaScript │
│  (Interface utilisateur)   │
└──────────┬─────────────────┘
           │
           │ REST API
           │ JSON
           │
┌──────────┴─────────────────┐
│  Backend Delphi            │
│  (Horse, RAD Server)       │
│  (Logique + Performance)   │
└──────────┬─────────────────┘
           │
┌──────────┴─────────────────┐
│  Base de données           │
└────────────────────────────┘
```

**Avantages :**
- Utilise les forces de Delphi (backend)
- Interface web moderne possible
- Performance serveur excellente

### Stratégie 2 : Attendre et préparer

- **Surveiller** les annonces Embarcadero
- **Apprendre** WebAssembly et ses concepts
- **Expérimenter** avec d'autres langages (Rust, C++)
- **Préparer** le code pour portabilité future

### Stratégie 3 : Solutions alternatives

**Option A : TMS Web Core (maintenant)**
- Développement immédiat
- Syntaxe Delphi
- JavaScript sous le capot

**Option B : Blazor WebAssembly + Delphi Backend**
- Frontend C#/Blazor (Microsoft)
- Backend Delphi
- Deux langages mais complémentaires

**Option C : React/Vue + Delphi Backend**
- Frontend JavaScript pur
- Backend Delphi
- Séparation claire

## Exemple de migration conceptuelle

### Application Delphi VCL actuelle

```pascal
unit MainForm;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  end;

implementation

uses
  MathLib; // Bibliothèque de calculs

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Input: Double;
  Resultat: Double;
begin
  Input := StrToFloat(Edit1.Text);
  Resultat := MathLib.ComplexCalculation(Input);
  Label1.Caption := FloatToStr(Resultat);
end;

end.
```

### Version Web future (hypothétique avec WASM)

**Backend (Delphi → WASM) :**
```pascal
// MathLib.pas - Compilé en WASM
unit MathLib;

interface

function ComplexCalculation(Input: Double): Double; export;

implementation

function ComplexCalculation(Input: Double): Double;  
var  
  i: Integer;
  Temp: Double;
begin
  Temp := Input;
  for i := 1 to 10000 do
    Temp := Sqrt(Temp * Temp + 1);
  Result := Temp;
end;

end.
```

**Frontend (HTML + JavaScript) :**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Application Web Delphi</title>
</head>
<body>
  <input type="number" id="input" />
  <button onclick="calculate()">Calculer</button>
  <div id="result"></div>

  <script>
    let wasmModule;

    // Charger le module WASM Delphi
    WebAssembly.instantiateStreaming(fetch('mathlib.wasm'))
      .then(obj => {
        wasmModule = obj.instance;
        console.log('Module Delphi WASM chargé');
      });

    function calculate() {
      const input = parseFloat(document.getElementById('input').value);

      // Appeler la fonction Delphi compilée en WASM
      const result = wasmModule.exports.ComplexCalculation(input);

      document.getElementById('result').textContent =
        'Résultat: ' + result.toFixed(2);
    }
  </script>
</body>
</html>
```

## Apprentissage et ressources

### Pour se préparer à WebAssembly

**1. Apprendre les concepts**
- Comprendre le format binaire
- Étudier la mémoire linéaire
- Maîtriser l'interface JavaScript/WASM

**2. Expérimenter avec d'autres langages**
```bash
# Essayer avec C
emcc hello.c -o hello.wasm

# Essayer avec Rust
cargo build --target wasm32-unknown-unknown
```

**3. Ressources en ligne**
- WebAssembly.org (site officiel)
- MDN Web Docs
- Tutoriels Rust/WASM
- Cours sur Blazor WebAssembly

**4. Outils de développement**
- Chrome DevTools (onglet WebAssembly)
- VS Code avec extensions WASM
- Explorateurs de fichiers WASM

### Communauté Delphi et WASM

**Suivre les développements :**
- Forums Embarcadero
- Groupes Delphi sur Reddit
- Conférences (DelphiCon)
- Blogs de développeurs Delphi

**Contribuer :**
- Projets open-source
- Feedback à Embarcadero
- Partage d'expériences

## Conclusion

WebAssembly représente **l'avenir de la performance web**, mais le support natif dans Delphi n'est pas encore disponible. Cependant, cela ne doit pas vous empêcher de créer d'excellentes applications web avec Delphi !

**Résumé des points clés :**

✅ **WebAssembly = Performance web native** mais nécessite compilation spécifique  
✅ **Delphi aujourd'hui** : Pas de compilation WASM native officielle  
✅ **Solutions actuelles** : TMS Web Core (JavaScript) ou architecture backend Delphi  
✅ **Future potentiel** : Support possible dans futures versions de Delphi  
✅ **Meilleure stratégie** : Architecture moderne (frontend web + backend Delphi)

**Recommandations pratiques :**

1. **Court terme** : Utiliser TMS Web Core ou architecture API REST
2. **Moyen terme** : Suivre les annonces Embarcadero
3. **Long terme** : Se préparer en apprenant les concepts WASM
4. **Toujours** : Créer du code modulaire et portable

**L'important à retenir :**

Même sans WebAssembly, Delphi reste **excellent pour le backend** :
- Performance native côté serveur
- Accès bases de données rapide
- Logique métier sécurisée
- API REST performantes

Et vous pouvez créer des **frontends web modernes** avec :
- TMS Web Core (Delphi → JavaScript)
- Frameworks JavaScript (React, Vue, Angular)
- Combinaison des deux

WebAssembly viendra peut-être un jour enrichir encore plus les possibilités de Delphi pour le web. En attendant, les outils actuels permettent déjà de créer d'excellentes applications web professionnelles !

Dans la section suivante, nous explorerons WebStencils et les techniques d'intégration côté serveur améliorée, qui offrent des alternatives intéressantes pour le développement web avec Delphi.

⏭️ [WebStencils : intégration côté serveur améliorée](/23-conception-dapplications-web-avec-delphi/09-webstencils-integration-cote-serveur-amelioree.md)
