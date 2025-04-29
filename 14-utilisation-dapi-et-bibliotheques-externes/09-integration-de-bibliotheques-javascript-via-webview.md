# 14.9 Intégration de bibliothèques JavaScript via WebView

## Introduction

Le monde du JavaScript regorge de bibliothèques puissantes pour créer des graphiques interactifs, des visualisations de données, des cartes, et bien d'autres fonctionnalités riches. En tant que développeur Delphi, vous pourriez vouloir profiter de ces bibliothèques JavaScript sans avoir à recréer leurs fonctionnalités en Delphi.

Grâce aux composants WebView disponibles dans Delphi, vous pouvez facilement intégrer des bibliothèques JavaScript dans vos applications Delphi. Cette approche vous permet de combiner le meilleur des deux mondes : la robustesse et la rapidité de développement de Delphi avec la richesse des bibliothèques JavaScript.

Dans ce chapitre, nous allons explorer comment intégrer des bibliothèques JavaScript dans vos applications Delphi via un composant WebView.

## Prérequis

- Delphi XE7 ou version ultérieure (TWebBrowser)
- Delphi 10.4 ou version ultérieure pour TEdgeBrowser (recommandé pour les applications modernes)
- Connaissance de base en HTML et JavaScript

## Composants WebView disponibles dans Delphi

Delphi propose plusieurs composants pour intégrer des pages web dans vos applications :

1. **TWebBrowser** : Composant plus ancien basé sur Internet Explorer
2. **TEdgeBrowser** : Composant moderne basé sur Microsoft Edge (Chromium)
3. **TEmbeddedWB** : Composant tiers pour des fonctionnalités étendues

Pour les nouvelles applications, il est recommandé d'utiliser le `TEdgeBrowser` car il est basé sur Chromium, offrant ainsi de meilleures performances et une compatibilité accrue avec les technologies web modernes.

## Étape 1 : Créer un projet avec un composant WebView

Commençons par créer un projet simple avec un composant WebView :

1. Créez un nouveau projet VCL Forms
2. Ajoutez un composant `TEdgeBrowser` (ou `TWebBrowser` si vous utilisez une version plus ancienne de Delphi)
3. Redimensionnez le composant pour qu'il occupe une partie significative de votre formulaire

```pascal
// Dans le fichier .pas de votre formulaire
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Edge;

type
  TForm1 = class(TForm)
    EdgeBrowser1: TEdgeBrowser;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Nous initialiserons le contenu web ici
end;
```

## Étape 2 : Charger du contenu HTML avec JavaScript

Pour intégrer des bibliothèques JavaScript, nous allons d'abord créer une page HTML simple que nous chargerons dans notre WebView :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  HTMLContent: string;
begin
  HTMLContent :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <title>Intégration JavaScript</title>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' +
    '    #container { width: 100%; height: 400px; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <h2>Exemple d''intégration JavaScript</h2>' +
    '  <div id="container"></div>' +
    '  <script>' +
    '    function sayHello() {' +
    '      alert("Bonjour depuis JavaScript!");' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';

  // Charger le contenu HTML dans le navigateur
  EdgeBrowser1.NavigateToString(HTMLContent);
end;
```

Pour le composant `TWebBrowser`, utilisez cette approche alternative :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  HTMLContent: string;
begin
  HTMLContent := '<!DOCTYPE html>...'; // Même contenu que ci-dessus

  // Charger le contenu HTML
  WebBrowser1.Navigate('about:blank');
  while WebBrowser1.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  (WebBrowser1.Document as IHTMLDocument2).write(HTMLContent);
  (WebBrowser1.Document as IHTMLDocument2).close;
end;
```

## Étape 3 : Intégrer une bibliothèque JavaScript externe

Maintenant, intégrons une bibliothèque JavaScript. Dans cet exemple, nous allons utiliser Chart.js, une bibliothèque populaire pour créer des graphiques :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  HTMLContent: string;
begin
  HTMLContent :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <title>Graphique avec Chart.js</title>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' +
    '    #chartContainer { width: 100%; height: 400px; }' +
    '  </style>' +
    '  <!-- Chargement de Chart.js depuis un CDN -->' +
    '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>' +
    '</head>' +
    '<body>' +
    '  <h2>Exemple de graphique avec Chart.js</h2>' +
    '  <div id="chartContainer">' +
    '    <canvas id="myChart"></canvas>' +
    '  </div>' +
    '  <script>' +
    '    // Création du graphique' +
    '    const ctx = document.getElementById("myChart").getContext("2d");' +
    '    const myChart = new Chart(ctx, {' +
    '      type: "bar",' +
    '      data: {' +
    '        labels: ["Janvier", "Février", "Mars", "Avril", "Mai", "Juin"],' +
    '        datasets: [{' +
    '          label: "Ventes 2023",' +
    '          data: [12, 19, 3, 5, 2, 3],' +
    '          backgroundColor: [' +
    '            "rgba(255, 99, 132, 0.2)",' +
    '            "rgba(54, 162, 235, 0.2)",' +
    '            "rgba(255, 206, 86, 0.2)",' +
    '            "rgba(75, 192, 192, 0.2)",' +
    '            "rgba(153, 102, 255, 0.2)",' +
    '            "rgba(255, 159, 64, 0.2)"' +
    '          ],' +
    '          borderColor: [' +
    '            "rgba(255, 99, 132, 1)",' +
    '            "rgba(54, 162, 235, 1)",' +
    '            "rgba(255, 206, 86, 1)",' +
    '            "rgba(75, 192, 192, 1)",' +
    '            "rgba(153, 102, 255, 1)",' +
    '            "rgba(255, 159, 64, 1)"' +
    '          ],' +
    '          borderWidth: 1' +
    '        }]' +
    '      },' +
    '      options: {' +
    '        scales: {' +
    '          y: {' +
    '            beginAtZero: true' +
    '          }' +
    '        }' +
    '      }' +
    '    });' +
    '  </script>' +
    '</body>' +
    '</html>';

  // Charger le contenu HTML dans le navigateur
  EdgeBrowser1.NavigateToString(HTMLContent);
end;
```

### Remarques importantes sur les bibliothèques externes

1. **Utilisation de CDN** : Dans l'exemple ci-dessus, nous chargeons Chart.js depuis un CDN (Content Delivery Network). C'est pratique, mais cela nécessite que l'utilisateur soit connecté à Internet.

2. **Inclusion locale** : Pour les applications qui doivent fonctionner hors ligne, vous pouvez inclure la bibliothèque JavaScript dans vos ressources d'application et l'extraire au besoin :

```pascal
procedure ExtractJSLibrary;
var
  ResourceStream: TResourceStream;
  DestPath: string;
begin
  DestPath := TPath.Combine(TPath.GetDocumentsPath, 'chartjs');

  if not TDirectory.Exists(DestPath) then
    TDirectory.CreateDirectory(DestPath);

  // Extraire la bibliothèque depuis les ressources
  ResourceStream := TResourceStream.Create(HInstance, 'CHARTJS', RT_RCDATA);
  try
    ResourceStream.SaveToFile(TPath.Combine(DestPath, 'chart.js'));
  finally
    ResourceStream.Free;
  end;
end;
```

3. **Data URI** : Une autre approche consiste à incorporer le code JavaScript directement en utilisant des Data URIs :

```pascal
// Charger le contenu du fichier chart.js
var
  ChartJSContent: string;
  HTMLContent: string;
begin
  ChartJSContent := TFile.ReadAllText('chart.js');

  HTMLContent :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <script>' + ChartJSContent + '</script>' +
    // ... reste du HTML
```

## Étape 4 : Communication bidirectionnelle entre Delphi et JavaScript

L'un des aspects les plus puissants de cette intégration est la capacité à communiquer entre Delphi et JavaScript dans les deux sens.

### Appeler du code JavaScript depuis Delphi

#### Avec TEdgeBrowser

```pascal
procedure TForm1.ButtonUpdateChartClick(Sender: TObject);
var
  Script: string;
begin
  // Mettre à jour les données du graphique
  Script :=
    'myChart.data.datasets[0].data = [' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + '];' +
    'myChart.update();';

  EdgeBrowser1.ExecuteScript(Script,
    procedure(const aResult: string; aSuccess: Boolean)
    begin
      if aSuccess then
        ShowMessage('Graphique mis à jour avec succès!')
      else
        ShowMessage('Erreur lors de la mise à jour: ' + aResult);
    end);
end;
```

#### Avec TWebBrowser

```pascal
procedure TForm1.ButtonUpdateChartClick(Sender: TObject);
var
  Script: string;
  Doc: IHTMLDocument2;
  Window: IHTMLWindow2;
begin
  Doc := WebBrowser1.Document as IHTMLDocument2;
  Window := Doc.parentWindow;

  // Mettre à jour les données du graphique
  Script :=
    'myChart.data.datasets[0].data = [' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + ', ' +
    IntToStr(Random(30)) + '];' +
    'myChart.update();';

  Window.execScript(Script, 'JavaScript');
end;
```

### Appeler du code Delphi depuis JavaScript

La communication inverse (JavaScript vers Delphi) nécessite une configuration supplémentaire :

#### Avec TEdgeBrowser

```pascal
// Dans la déclaration de la classe
TForm1 = class(TForm)
  // ...
  procedure WebMessageReceived(Sender: TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
private
  // ...
public
  // ...
end;

// Dans FormCreate
procedure TForm1.FormCreate(Sender: TObject);
begin
  // ... HTML content as before ...

  // Ajouter du code JavaScript pour envoyer des messages à Delphi
  HTMLContent := StringReplace(HTMLContent, '</script>',
    'function sendToDelphi(message) {' +
    '  window.chrome.webview.postMessage(message);' +
    '}' +
    '</script>', []);

  EdgeBrowser1.NavigateToString(HTMLContent);

  // Configurer l'événement pour recevoir les messages
  EdgeBrowser1.OnWebMessageReceived := WebMessageReceived;
end;

// Gestionnaire d'événement pour les messages de JavaScript
procedure TForm1.WebMessageReceived(Sender: TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
var
  Message: string;
begin
  Message := Args.WebMessageAsString;
  ShowMessage('Message reçu de JavaScript: ' + Message);

  // Traiter le message en fonction de son contenu
  if Message = 'chartClicked' then
    // Faire quelque chose...
  else if Message.StartsWith('dataPoint:') then
    // Extraire l'index du point de données et faire quelque chose...
    //var Index := StrToIntDef(Message.Substring(10), -1);
end;
```

Ajoutez ce code JavaScript pour envoyer des messages à Delphi :

```javascript
myChart.options.onClick = function(event, elements) {
  if (elements.length > 0) {
    const index = elements[0].index;
    const value = myChart.data.datasets[0].data[index];
    const label = myChart.data.labels[index];
    sendToDelphi('dataPoint:' + index + ':' + label + ':' + value);
  }
};
```

#### Avec TWebBrowser (plus complexe)

Pour `TWebBrowser`, vous pouvez utiliser un objet externe exposé à JavaScript :

```pascal
// Définir une classe d'interface externe
type
  IExternalObject = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure SendMessage(const AMessage: WideString); safecall;
  end;

  TExternalObject = class(TAutoObject, IExternalObject)
  private
    FForm: TForm1;
  public
    constructor Create(AForm: TForm1);
    procedure SendMessage(const AMessage: WideString); safecall;
  end;

constructor TExternalObject.Create(AForm: TForm1);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TExternalObject.SendMessage(const AMessage: WideString);
begin
  if Assigned(FForm) then
    FForm.HandleJSMessage(string(AMessage));
end;

// Dans la classe du formulaire
procedure TForm1.FormCreate(Sender: TObject);
var
  HTMLContent: string;
  Doc: IHTMLDocument2;
  Window: IDispatch;
begin
  // ... HTML content as before ...

  WebBrowser1.Navigate('about:blank');
  while WebBrowser1.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  Doc := WebBrowser1.Document as IHTMLDocument2;

  // Ajouter l'objet externe
  (Doc as IHTMLDocument2).parentWindow.execScript(
    'var delphiApp = null;', 'JavaScript');

  // Obtenir l'objet window
  Window := (Doc as IHTMLDocument2).parentWindow;

  // Assigner l'objet externe à une variable JavaScript
  (Window as IDispatchEx).SetProperty('delphiApp',
    TExternalObject.Create(Self) as IDispatch);

  // Écrire le contenu HTML
  Doc.write(HTMLContent);
  Doc.close;
end;

procedure TForm1.HandleJSMessage(const AMessage: string);
begin
  ShowMessage('Message reçu de JavaScript: ' + AMessage);

  // Traiter le message...
end;
```

Ajoutez ce code JavaScript pour utiliser l'objet externe :

```javascript
myChart.options.onClick = function(event, elements) {
  if (elements.length > 0 && window.delphiApp) {
    const index = elements[0].index;
    const value = myChart.data.datasets[0].data[index];
    const label = myChart.data.labels[index];
    window.delphiApp.SendMessage('dataPoint:' + index + ':' + label + ':' + value);
  }
};
```

## Exemple pratique : Intégration de D3.js pour des visualisations avancées

Voici un exemple plus complet utilisant D3.js, une bibliothèque puissante pour créer des visualisations de données interactives :

```pascal
procedure TForm1.CreateD3Visualization;
var
  HTMLContent: string;
begin
  HTMLContent :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <title>Visualisation D3.js</title>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' +
    '    #chart { width: 100%; height: 400px; }' +
    '    .bar { fill: steelblue; }' +
    '    .bar:hover { fill: brown; }' +
    '  </style>' +
    '  <!-- Chargement de D3.js depuis un CDN -->' +
    '  <script src="https://d3js.org/d3.v7.min.js"></script>' +
    '</head>' +
    '<body>' +
    '  <h2>Visualisation avec D3.js</h2>' +
    '  <div id="chart"></div>' +
    '  <script>' +
    '    // Données pour le graphique' +
    '    const data = [' +
    '      { name: "A", value: 5 },' +
    '      { name: "B", value: 10 },' +
    '      { name: "C", value: 15 },' +
    '      { name: "D", value: 20 },' +
    '      { name: "E", value: 25 }' +
    '    ];' +
    '' +
    '    // Définir les dimensions et marges du graphique' +
    '    const margin = { top: 20, right: 30, bottom: 30, left: 40 };' +
    '    const width = 600 - margin.left - margin.right;' +
    '    const height = 400 - margin.top - margin.bottom;' +
    '' +
    '    // Créer le conteneur SVG' +
    '    const svg = d3.select("#chart")' +
    '      .append("svg")' +
    '      .attr("width", width + margin.left + margin.right)' +
    '      .attr("height", height + margin.top + margin.bottom)' +
    '      .append("g")' +
    '      .attr("transform", `translate(${margin.left},${margin.top})`);' +
    '' +
    '    // Créer l''échelle X' +
    '    const x = d3.scaleBand()' +
    '      .domain(data.map(d => d.name))' +
    '      .range([0, width])' +
    '      .padding(0.2);' +
    '' +
    '    // Créer l''échelle Y' +
    '    const y = d3.scaleLinear()' +
    '      .domain([0, d3.max(data, d => d.value)])' +
    '      .nice()' +
    '      .range([height, 0]);' +
    '' +
    '    // Ajouter l''axe X' +
    '    svg.append("g")' +
    '      .attr("transform", `translate(0,${height})`)' +
    '      .call(d3.axisBottom(x));' +
    '' +
    '    // Ajouter l''axe Y' +
    '    svg.append("g")' +
    '      .call(d3.axisLeft(y));' +
    '' +
    '    // Ajouter les barres' +
    '    svg.selectAll(".bar")' +
    '      .data(data)' +
    '      .enter()' +
    '      .append("rect")' +
    '      .attr("class", "bar")' +
    '      .attr("x", d => x(d.name))' +
    '      .attr("y", d => y(d.value))' +
    '      .attr("width", x.bandwidth())' +
    '      .attr("height", d => height - y(d.value))' +
    '      .on("click", function(event, d) {' +
    '        // Envoyer les informations à Delphi' +
    '        if (window.chrome && window.chrome.webview) {' +
    '          window.chrome.webview.postMessage("barClick:" + d.name + ":" + d.value);' +
    '        }' +
    '      });' +
    '' +
    '    // Fonction pour mettre à jour les données' +
    '    window.updateD3Chart = function(newData) {' +
    '      // Mettre à jour l''échelle Y' +
    '      y.domain([0, d3.max(newData, d => d.value)]).nice();' +
    '' +
    '      // Sélectionner toutes les barres et lier les nouvelles données' +
    '      const bars = svg.selectAll(".bar").data(newData);' +
    '' +
    '      // Mettre à jour les barres existantes' +
    '      bars.transition()' +
    '        .duration(750)' +
    '        .attr("y", d => y(d.value))' +
    '        .attr("height", d => height - y(d.value));' +
    '' +
    '      // Mettre à jour l''axe Y' +
    '      svg.select("g:nth-child(2)")' +
    '        .transition()' +
    '        .duration(750)' +
    '        .call(d3.axisLeft(y));' +
    '    };' +
    '  </script>' +
    '</body>' +
    '</html>';

  // Charger le contenu HTML dans le navigateur
  EdgeBrowser1.NavigateToString(HTMLContent);

  // Configurer l'événement pour recevoir les messages
  EdgeBrowser1.OnWebMessageReceived := WebMessageReceived;
end;

procedure TForm1.ButtonUpdateD3Click(Sender: TObject);
var
  Script: string;
begin
  // Générer de nouvelles données aléatoires
  Script :=
    'const newData = [' +
    '  { name: "A", value: ' + IntToStr(Random(50)) + ' },' +
    '  { name: "B", value: ' + IntToStr(Random(50)) + ' },' +
    '  { name: "C", value: ' + IntToStr(Random(50)) + ' },' +
    '  { name: "D", value: ' + IntToStr(Random(50)) + ' },' +
    '  { name: "E", value: ' + IntToStr(Random(50)) + ' }' +
    '];' +
    'window.updateD3Chart(newData);';

  EdgeBrowser1.ExecuteScript(Script, nil);
end;

procedure TForm1.WebMessageReceived(Sender: TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
var
  Message: string;
  Parts: TArray<string>;
begin
  Message := Args.WebMessageAsString;

  if Message.StartsWith('barClick:') then
  begin
    Parts := Message.Split([':']);
    if Length(Parts) >= 3 then
    begin
      ShowMessage('Barre cliquée: ' + Parts[1] + ', Valeur: ' + Parts[2]);
      // Faire quelque chose avec ces données...
    end;
  end;
end;
```

## Considérations importantes

### 1. Sécurité

L'intégration de JavaScript dans vos applications Delphi présente certains risques de sécurité :

- **Injection de code** : Évitez de construire dynamiquement du HTML ou du JavaScript à partir de données non fiables.
- **Contenu externe** : Soyez prudent avec les bibliothèques JavaScript chargées depuis des CDN externes.
- **CORS (Cross-Origin Resource Sharing)** : Certaines bibliothèques peuvent rencontrer des problèmes de CORS lors de l'accès à des ressources externes.

### 2. Performance

- **Utilisation de la mémoire** : Les composants WebView peuvent consommer beaucoup de mémoire, surtout avec des visualisations complexes.
- **Temps de chargement** : Le chargement initial des bibliothèques JavaScript peut prendre du temps.
- **Animation fluide** : Les animations JavaScript peuvent ne pas être aussi fluides que les animations natives.

### 3. Compatibilité

- **TWebBrowser** utilise Internet Explorer, qui devient obsolète.
- **TEdgeBrowser** nécessite que WebView2 soit installé sur le système de l'utilisateur.
- **Plateformes mobiles** : Sur Android et iOS, vous devrez utiliser des composants différents (comme TWebBrowser dans FMX).

## Solutions aux problèmes courants

### 1. Communication WebView incorrecte

Si la communication entre Delphi et JavaScript ne fonctionne pas correctement :

1. Assurez-vous que le contenu HTML est complètement chargé avant d'exécuter du code JavaScript.
2. Vérifiez les erreurs JavaScript dans la console (ajoutez un gestionnaire d'erreurs JavaScript).
3. Utilisez des techniques de débogage comme l'alerte JavaScript pour vérifier l'exécution.

```javascript
// Ajouter au début et à la fin de vos fonctions pour le débogage
console.log('Début de la fonction');
// code...
console.log('Fin de la fonction');
```

### 2. Gestion des erreurs JavaScript

```pascal
// Ajouter un gestionnaire d'erreur JavaScript
HTMLContent := StringReplace(HTMLContent, '</script>',
  'window.onerror = function(message, source, lineno, colno, error) {' +
  '  if (window.chrome && window.chrome.webview) {' +
  '    window.chrome.webview.postMessage("error:" + message);' +
  '  }' +
  '  return true;' +
  '};' +
  '</script>', []);
```

### 3. Chargement local des bibliothèques

Pour les applications qui doivent fonctionner hors ligne, créez un petit serveur HTTP local ou utilisez des fichiers de ressources :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Extraire les bibliothèques JavaScript
  ExtractJSLibraries;

  // Modifier le HTML pour utiliser des chemins locaux
  HTMLContent := StringReplace(HTMLContent,
    '<script src="https://d3js.org/d3.v7.min.js"></script>',
    '<script src="file:///' + ExtractFilePath(Application.ExeName) + 'js\d3.v7.min.js"></script>',
    [rfReplaceAll]);

  // ...
end;

procedure TForm1.ExtractJSLibraries;
var
  JSDir: string;
begin
  JSDir := ExtractFilePath(Application.ExeName) + 'js';

  if not DirectoryExists(JSDir) then
    CreateDir(JSDir);

  // Extraire d3.js des ressources de l'application
  with TResourceStream.Create(HInstance, 'D3JS', RT_RCDATA) do
  try
    SaveToFile(JSDir + '\d3.v7.min.js');
  finally
    Free;
  end;
end;
```

## Conclusion

L'intégration de bibliothèques JavaScript via WebView dans vos applications Delphi ouvre un monde de possibilités. Vous pouvez créer des visualisations riches, des graphiques interactifs, des cartes, et bien plus encore, tout en conservant la puissance et la facilité de développement de Delphi.

Bien que cette approche présente certains défis en termes de performances et de compatibilité, elle offre une solution pratique pour tirer parti de l'écosystème JavaScript sans avoir à réinventer la roue.

Pour des applications modernes, nous recommandons d'utiliser `TEdgeBrowser` avec WebView2, qui offre de meilleures performances et une meilleure compatibilité avec les technologies web modernes.

N'oubliez pas que la communication bidirectionnelle entre Delphi et JavaScript est la clé pour créer des applications vraiment intégrées, où chaque partie fait ce qu'elle fait de mieux : Delphi pour la logique métier robuste et le JavaScript pour les interfaces utilisateur riches et interactives.

## Ressources supplémentaires

- [Documentation Microsoft sur WebView2](https://learn.microsoft.com/en-us/microsoft-edge/webview2/)
- [D3.js - Bibliothèque de visualisation de données](https://d3js.org/)
- [Chart.js - Bibliothèque de graphiques simple](https://www.chartjs.org/)
- [Leaflet - Bibliothèque de cartes interactives](https://leafletjs.com/)
- [Three.js - Bibliothèque 3D](https://threejs.org/)
