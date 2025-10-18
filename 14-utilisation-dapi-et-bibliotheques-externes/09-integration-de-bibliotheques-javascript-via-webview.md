🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.9 Intégration de bibliothèques JavaScript via WebView

## Introduction aux WebView

### Qu'est-ce qu'un WebView ?

Un **WebView** est comme un navigateur web miniature intégré directement dans votre application Delphi. C'est une fenêtre qui peut afficher du contenu HTML, CSS et JavaScript, comme si vous utilisiez Chrome ou Firefox, mais à l'intérieur de votre propre application.

**Analogie :** Imaginez que vous ayez un cadre photo chez vous, mais au lieu d'une photo fixe, c'est un écran qui peut afficher n'importe quelle page web. Le WebView est ce cadre intelligent dans votre application.

### Pourquoi utiliser JavaScript via WebView ?

**Visualisations riches** : JavaScript possède des bibliothèques fantastiques pour les graphiques, cartes, animations que Delphi n'a pas nativement.

**Interfaces web modernes** : Créer des interfaces utilisateur avec HTML/CSS/JavaScript, qui sont très flexibles et modernes.

**Réutiliser du code existant** : Si vous avez déjà du code JavaScript fonctionnel, pas besoin de le réécrire en Delphi.

**Écosystème JavaScript** : Accès à des milliers de bibliothèques (Chart.js, D3.js, Leaflet, etc.).

**Interopérabilité** : Combiner la puissance de Delphi (backend, bases de données) avec la richesse de JavaScript (interface).

### Composants WebView disponibles

#### TWebBrowser (Windows)

Ancien composant basé sur Internet Explorer (déprécié).

```pascal
// À éviter : basé sur IE qui est obsolète
TWebBrowser1.Navigate('https://example.com');
```

#### TEdgeBrowser (Windows 10+)

Nouveau composant basé sur Microsoft Edge (Chromium). **Recommandé pour Windows**.

```pascal
uses
  Winapi.WebView2;

type
  TForm1 = class(TForm)
    EdgeBrowser1: TEdgeBrowser;
    procedure FormCreate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.Navigate('https://example.com');
end;
```

#### TWebBrowser (FMX)

Composant multi-plateformes de FireMonkey.

```pascal
uses
  FMX.WebBrowser;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
  end;

// Fonctionne sur Windows, macOS, iOS, Android
```

## Chargement de contenu HTML

### Charger depuis une URL

```pascal
procedure TForm1.ButtonChargerURLClick(Sender: TObject);
begin
  EdgeBrowser1.Navigate('https://www.google.com');
end;
```

### Charger du HTML en chaîne

```pascal
procedure TForm1.ButtonChargerHTMLClick(Sender: TObject);
var
  HTML: string;
begin
  HTML :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <title>Ma Page</title>' +
    '</head>' +
    '<body>' +
    '  <h1>Bonjour depuis Delphi !</h1>' +
    '  <p>Cette page est générée dynamiquement.</p>' +
    '</body>' +
    '</html>';

  EdgeBrowser1.NavigateToString(HTML);
end;
```

### Charger depuis un fichier local

```pascal
procedure TForm1.ButtonChargerFichierClick(Sender: TObject);
var
  CheminFichier: string;
begin
  CheminFichier := TPath.Combine(ExtractFilePath(ParamStr(0)), 'page.html');

  if FileExists(CheminFichier) then
    EdgeBrowser1.Navigate('file:///' + CheminFichier)
  else
    ShowMessage('Fichier HTML introuvable');
end;
```

### Générer du HTML complexe

```pascal
function GenererPageHTML(const Titre, Contenu: string): string;
begin
  Result := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <title>%s</title>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 20px; }' +
    '    h1 { color: #333; }' +
    '    .content { padding: 20px; background: #f5f5f5; border-radius: 8px; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <h1>%s</h1>' +
    '  <div class="content">%s</div>' +
    '</body>' +
    '</html>',
    [Titre, Titre, Contenu]
  );
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  HTML: string;
begin
  HTML := GenererPageHTML('Mon Rapport',
    '<p>Voici le contenu de mon rapport...</p>');
  EdgeBrowser1.NavigateToString(HTML);
end;
```

## Communication Delphi → JavaScript

### Exécuter du code JavaScript depuis Delphi

```pascal
procedure TForm1.ButtonExecuterJSClick(Sender: TObject);
var
  Script: string;
begin
  Script := 'alert("Hello depuis Delphi !");';

  EdgeBrowser1.ExecuteScript(Script,
    procedure(ErrorCode: HRESULT; const ResultObjectAsJson: string)
    begin
      if ErrorCode = S_OK then
        ShowMessage('Script exécuté avec succès')
      else
        ShowMessage('Erreur lors de l''exécution');
    end);
end;
```

### Modifier le contenu HTML

```pascal
procedure ChangerTitreHTML(const NouveauTitre: string);
var
  Script: string;
begin
  Script := Format('document.title = "%s";', [NouveauTitre]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

procedure ChangerContenuElement(const ElementID, NouveauContenu: string);
var
  Script: string;
begin
  // Échapper les guillemets et caractères spéciaux
  Script := Format(
    'document.getElementById("%s").innerHTML = "%s";',
    [ElementID, StringReplace(NouveauContenu, '"', '\"', [rfReplaceAll])]
  );

  EdgeBrowser1.ExecuteScript(Script, nil);
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  ChangerContenuElement('contenu', '<h2>Nouveau contenu !</h2>');
end;
```

### Appeler des fonctions JavaScript

```pascal
// HTML avec fonction JavaScript
const
  HTML_AVEC_FONCTION =
    '<html>' +
    '<head>' +
    '  <script>' +
    '    function calculer(a, b) {' +
    '      return a + b;' +
    '    }' +
    '    ' +
    '    function afficherMessage(message) {' +
    '      document.getElementById("resultat").innerHTML = message;' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body>' +
    '  <div id="resultat">En attente...</div>' +
    '</body>' +
    '</html>';

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.NavigateToString(HTML_AVEC_FONCTION);
end;

procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  Script: string;
begin
  Script := 'calculer(10, 20)';

  EdgeBrowser1.ExecuteScript(Script,
    procedure(ErrorCode: HRESULT; const ResultObjectAsJson: string)
    begin
      if ErrorCode = S_OK then
        ShowMessage('Résultat: ' + ResultObjectAsJson);
    end);
end;

procedure TForm1.ButtonAfficherClick(Sender: TObject);
var
  Script: string;
  Message: string;
begin
  Message := 'Message depuis Delphi à ' + TimeToStr(Now);
  Script := Format('afficherMessage("%s")', [Message]);

  EdgeBrowser1.ExecuteScript(Script, nil);
end;
```

### Récupérer des valeurs depuis JavaScript

```pascal
procedure ObtenirValeurChamp;
var
  Script: string;
begin
  Script := 'document.getElementById("nom").value';

  EdgeBrowser1.ExecuteScript(Script,
    procedure(ErrorCode: HRESULT; const ResultObjectAsJson: string)
    var
      Valeur: string;
    begin
      if ErrorCode = S_OK then
      begin
        // ResultObjectAsJson contient la valeur entre guillemets
        Valeur := StringReplace(ResultObjectAsJson, '"', '', [rfReplaceAll]);
        ShowMessage('Valeur: ' + Valeur);
      end;
    end);
end;
```

## Communication JavaScript → Delphi

### Envoyer des messages depuis JavaScript

```pascal
type
  TForm1 = class(TForm)
    EdgeBrowser1: TEdgeBrowser;
    procedure EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
      Args: TWebMessageReceivedEventArgs);
  end;

// HTML avec envoi de messages
const
  HTML_AVEC_MESSAGES =
    '<html>' +
    '<head>' +
    '  <script>' +
    '    function envoyerMessage() {' +
    '      var message = document.getElementById("message").value;' +
    '      window.chrome.webview.postMessage(message);' +
    '    }' +
    '    ' +
    '    function envoyerJSON() {' +
    '      var data = {' +
    '        type: "notification",' +
    '        titre: "Mon Titre",' +
    '        message: "Mon Message"' +
    '      };' +
    '      window.chrome.webview.postMessage(JSON.stringify(data));' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body>' +
    '  <input type="text" id="message" placeholder="Votre message">' +
    '  <button onclick="envoyerMessage()">Envoyer à Delphi</button>' +
    '  <button onclick="envoyerJSON()">Envoyer JSON</button>' +
    '</body>' +
    '</html>';

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.NavigateToString(HTML_AVEC_MESSAGES);
end;

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  Message: string;
  JSONObject: TJSONObject;
begin
  Message := Args.WebMessageAsString;

  // Vérifier si c'est du JSON
  if Message.StartsWith('{') then
  begin
    JSONObject := TJSONObject.ParseJSONValue(Message) as TJSONObject;
    try
      if JSONObject.GetValue<string>('type') = 'notification' then
      begin
        ShowMessage(
          JSONObject.GetValue<string>('titre') + ': ' +
          JSONObject.GetValue<string>('message')
        );
      end;
    finally
      JSONObject.Free;
    end;
  end
  else
  begin
    // Message simple
    ShowMessage('Message reçu: ' + Message);
  end;
end;
```

### Système de commandes

```pascal
// HTML avec système de commandes
const
  HTML_COMMANDES =
    '<html>' +
    '<head>' +
    '  <script>' +
    '    function envoyerCommande(commande, parametres) {' +
    '      var message = {' +
    '        commande: commande,' +
    '        parametres: parametres' +
    '      };' +
    '      window.chrome.webview.postMessage(JSON.stringify(message));' +
    '    }' +
    '    ' +
    '    function sauvegarder() {' +
    '      var donnees = {' +
    '        nom: document.getElementById("nom").value,' +
    '        email: document.getElementById("email").value' +
    '      };' +
    '      envoyerCommande("sauvegarder", donnees);' +
    '    }' +
    '    ' +
    '    function charger() {' +
    '      envoyerCommande("charger", {id: 123});' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body>' +
    '  <input type="text" id="nom" placeholder="Nom">' +
    '  <input type="email" id="email" placeholder="Email">' +
    '  <button onclick="sauvegarder()">Sauvegarder</button>' +
    '  <button onclick="charger()">Charger</button>' +
    '</body>' +
    '</html>';

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  Message: string;
  JSONMessage, JSONParams: TJSONObject;
  Commande: string;
begin
  Message := Args.WebMessageAsString;

  JSONMessage := TJSONObject.ParseJSONValue(Message) as TJSONObject;
  try
    Commande := JSONMessage.GetValue<string>('commande');
    JSONParams := JSONMessage.GetValue<TJSONObject>('parametres');

    case Commande of
      'sauvegarder':
        TraiterSauvegarde(JSONParams);

      'charger':
        TraiterChargement(JSONParams);

      'supprimer':
        TraiterSuppression(JSONParams);
    end;
  finally
    JSONMessage.Free;
  end;
end;

procedure TForm1.TraiterSauvegarde(Params: TJSONObject);
var
  Nom, Email: string;
begin
  Nom := Params.GetValue<string>('nom');
  Email := Params.GetValue<string>('email');

  // Sauvegarder dans la base de données
  ShowMessage(Format('Sauvegarde: %s (%s)', [Nom, Email]));
end;
```

## Intégration de Chart.js (graphiques)

### Configuration de base

```pascal
function GenererPageChartJS: string;
begin
  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>' +
    '</head>' +
    '<body>' +
    '  <canvas id="monGraphique" width="400" height="200"></canvas>' +
    '  <script>' +
    '    var ctx = document.getElementById("monGraphique").getContext("2d");' +
    '    var chart = null;' +
    '    ' +
    '    function creerGraphique(donnees, labels) {' +
    '      if (chart) chart.destroy();' +
    '      ' +
    '      chart = new Chart(ctx, {' +
    '        type: "line",' +
    '        data: {' +
    '          labels: labels,' +
    '          datasets: [{' +
    '            label: "Ventes",' +
    '            data: donnees,' +
    '            borderColor: "rgb(75, 192, 192)",' +
    '            tension: 0.1' +
    '          }]' +
    '        },' +
    '        options: {' +
    '          responsive: true,' +
    '          plugins: {' +
    '            title: {' +
    '              display: true,' +
    '              text: "Graphique des ventes"' +
    '            }' +
    '          }' +
    '        }' +
    '      });' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.NavigateToString(GenererPageChartJS);
end;
```

### Envoyer des données depuis Delphi

```pascal
procedure TForm1.AfficherGraphique(Donnees: TArray<Integer>;
  Labels: TArray<string>);
var
  Script: string;
  DonneesJSON, LabelsJSON: string;
  I: Integer;
begin
  // Convertir le tableau de données en JSON
  DonneesJSON := '[';
  for I := 0 to High(Donnees) do
  begin
    if I > 0 then
      DonneesJSON := DonneesJSON + ',';
    DonneesJSON := DonneesJSON + IntToStr(Donnees[I]);
  end;
  DonneesJSON := DonneesJSON + ']';

  // Convertir les labels en JSON
  LabelsJSON := '[';
  for I := 0 to High(Labels) do
  begin
    if I > 0 then
      LabelsJSON := LabelsJSON + ',';
    LabelsJSON := LabelsJSON + '"' + Labels[I] + '"';
  end;
  LabelsJSON := LabelsJSON + ']';

  // Appeler la fonction JavaScript
  Script := Format('creerGraphique(%s, %s);', [DonneesJSON, LabelsJSON]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

// Utilisation
procedure TForm1.ButtonAfficherVentesClick(Sender: TObject);
var
  Ventes: TArray<Integer>;
  Mois: TArray<string>;
begin
  Ventes := [1000, 1500, 1200, 1800, 2000, 1700];
  Mois := ['Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Juin'];

  AfficherGraphique(Ventes, Mois);
end;
```

### Graphique interactif avec callback

```pascal
const
  HTML_CHART_INTERACTIF =
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>' +
    '</head>' +
    '<body>' +
    '  <canvas id="chart"></canvas>' +
    '  <script>' +
    '    var ctx = document.getElementById("chart").getContext("2d");' +
    '    var chart = null;' +
    '    ' +
    '    function creerGraphique(donnees, labels) {' +
    '      if (chart) chart.destroy();' +
    '      ' +
    '      chart = new Chart(ctx, {' +
    '        type: "bar",' +
    '        data: {' +
    '          labels: labels,' +
    '          datasets: [{' +
    '            label: "Valeurs",' +
    '            data: donnees' +
    '          }]' +
    '        },' +
    '        options: {' +
    '          onClick: function(event, elements) {' +
    '            if (elements.length > 0) {' +
    '              var index = elements[0].index;' +
    '              var label = this.data.labels[index];' +
    '              var value = this.data.datasets[0].data[index];' +
    '              ' +
    '              window.chrome.webview.postMessage(JSON.stringify({' +
    '                type: "click",' +
    '                label: label,' +
    '                value: value' +
    '              }));' +
    '            }' +
    '          }' +
    '        }' +
    '      });' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  JSONMessage: TJSONObject;
  TypeMessage, Label: string;
  Value: Integer;
begin
  JSONMessage := TJSONObject.ParseJSONValue(Args.WebMessageAsString) as TJSONObject;
  try
    TypeMessage := JSONMessage.GetValue<string>('type');

    if TypeMessage = 'click' then
    begin
      Label := JSONMessage.GetValue<string>('label');
      Value := JSONMessage.GetValue<Integer>('value');

      ShowMessage(Format('Cliqué sur: %s (Valeur: %d)', [Label, Value]));
    end;
  finally
    JSONMessage.Free;
  end;
end;
```

## Intégration de Leaflet (cartes)

### Configuration et affichage de carte

```pascal
function GenererPageLeaflet: string;
begin
  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" />' +
    '  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>' +
    '  <style>' +
    '    body { margin: 0; padding: 0; }' +
    '    #map { height: 100vh; width: 100%; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div id="map"></div>' +
    '  <script>' +
    '    var map = L.map("map").setView([48.8566, 2.3522], 13);' +
    '    ' +
    '    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {' +
    '      attribution: "© OpenStreetMap"' +
    '    }).addTo(map);' +
    '    ' +
    '    var markers = [];' +
    '    ' +
    '    function ajouterMarqueur(lat, lng, titre, description) {' +
    '      var marker = L.marker([lat, lng]).addTo(map);' +
    '      marker.bindPopup("<b>" + titre + "</b><br>" + description);' +
    '      markers.push(marker);' +
    '      return markers.length - 1;' +
    '    }' +
    '    ' +
    '    function centrerSur(lat, lng, zoom) {' +
    '      map.setView([lat, lng], zoom || 13);' +
    '    }' +
    '    ' +
    '    function supprimerMarqueur(index) {' +
    '      if (markers[index]) {' +
    '        map.removeLayer(markers[index]);' +
    '        markers[index] = null;' +
    '      }' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';
end;

procedure TForm1.AjouterMarqueurCarte(Latitude, Longitude: Double;
  const Titre, Description: string);
var
  Script: string;
begin
  Script := Format(
    'ajouterMarqueur(%f, %f, "%s", "%s");',
    [Latitude, Longitude, Titre, Description]
  );

  EdgeBrowser1.ExecuteScript(Script, nil);
end;

procedure TForm1.CentrerCarte(Latitude, Longitude: Double; Zoom: Integer);
var
  Script: string;
begin
  Script := Format('centrerSur(%f, %f, %d);', [Latitude, Longitude, Zoom]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

// Utilisation
procedure TForm1.ButtonAfficherParisClick(Sender: TObject);
begin
  EdgeBrowser1.NavigateToString(GenererPageLeaflet);

  // Attendre que la page soit chargée
  Sleep(1000); // Dans un vrai projet, utiliser l'événement OnNavigationCompleted

  AjouterMarqueurCarte(48.8584, 2.2945, 'Tour Eiffel',
    'Monument emblématique de Paris');
  AjouterMarqueurCarte(48.8606, 2.3376, 'Musée du Louvre',
    'Plus grand musée du monde');
end;
```

## Intégration de D3.js (visualisations avancées)

### Graphique circulaire animé

```pascal
function GenererPageD3: string;
begin
  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <script src="https://d3js.org/d3.v7.min.js"></script>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; }' +
    '    #chart { display: flex; justify-content: center; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div id="chart"></div>' +
    '  <script>' +
    '    function creerGraphiquePie(donnees) {' +
    '      document.getElementById("chart").innerHTML = "";' +
    '      ' +
    '      var width = 450;' +
    '      var height = 450;' +
    '      var margin = 40;' +
    '      var radius = Math.min(width, height) / 2 - margin;' +
    '      ' +
    '      var svg = d3.select("#chart")' +
    '        .append("svg")' +
    '        .attr("width", width)' +
    '        .attr("height", height)' +
    '        .append("g")' +
    '        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");' +
    '      ' +
    '      var color = d3.scaleOrdinal()' +
    '        .domain(donnees.map(d => d.label))' +
    '        .range(d3.schemeSet2);' +
    '      ' +
    '      var pie = d3.pie()' +
    '        .value(d => d.value);' +
    '      ' +
    '      var arc = d3.arc()' +
    '        .innerRadius(0)' +
    '        .outerRadius(radius);' +
    '      ' +
    '      svg.selectAll("path")' +
    '        .data(pie(donnees))' +
    '        .enter()' +
    '        .append("path")' +
    '        .attr("d", arc)' +
    '        .attr("fill", d => color(d.data.label))' +
    '        .attr("stroke", "white")' +
    '        .style("opacity", 0.7)' +
    '        .on("mouseover", function() {' +
    '          d3.select(this).style("opacity", 1);' +
    '        })' +
    '        .on("mouseout", function() {' +
    '          d3.select(this).style("opacity", 0.7);' +
    '        });' +
    '      ' +
    '      svg.selectAll("text")' +
    '        .data(pie(donnees))' +
    '        .enter()' +
    '        .append("text")' +
    '        .text(d => d.data.label + ": " + d.data.value)' +
    '        .attr("transform", d => "translate(" + arc.centroid(d) + ")")' +
    '        .style("text-anchor", "middle")' +
    '        .style("font-size", 14);' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';
end;

procedure TForm1.AfficherGraphiqueD3(Donnees: TArray<TPair<string, Integer>>);
var
  Script, JSONData: string;
  I: Integer;
begin
  // Construire le JSON
  JSONData := '[';
  for I := 0 to High(Donnees) do
  begin
    if I > 0 then
      JSONData := JSONData + ',';
    JSONData := JSONData + Format(
      '{"label":"%s","value":%d}',
      [Donnees[I].Key, Donnees[I].Value]
    );
  end;
  JSONData := JSONData + ']';

  Script := Format('creerGraphiquePie(%s);', [JSONData]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  Donnees: TArray<TPair<string, Integer>>;
begin
  EdgeBrowser1.NavigateToString(GenererPageD3);

  Sleep(1000);

  SetLength(Donnees, 4);
  Donnees[0] := TPair<string, Integer>.Create('Windows', 40);
  Donnees[1] := TPair<string, Integer>.Create('macOS', 25);
  Donnees[2] := TPair<string, Integer>.Create('Linux', 20);
  Donnees[3] := TPair<string, Integer>.Create('Autres', 15);

  AfficherGraphiqueD3(Donnees);
end;
```

## Intégration de Monaco Editor (éditeur de code)

### Configuration de l'éditeur

```pascal
function GenererPageMonaco: string;
begin
  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs/editor/editor.main.css" />' +
    '  <style>' +
    '    body { margin: 0; padding: 0; }' +
    '    #container { height: 100vh; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div id="container"></div>' +
    '  ' +
    '  <script src="https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs/loader.min.js"></script>' +
    '  <script>' +
    '    require.config({ paths: { vs: "https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs" } });' +
    '    ' +
    '    var editor;' +
    '    ' +
    '    require(["vs/editor/editor.main"], function () {' +
    '      editor = monaco.editor.create(document.getElementById("container"), {' +
    '        value: "// Écrivez votre code ici\\n",' +
    '        language: "javascript",' +
    '        theme: "vs-dark",' +
    '        automaticLayout: true' +
    '      });' +
    '      ' +
    '      editor.onDidChangeModelContent(function() {' +
    '        var contenu = editor.getValue();' +
    '        window.chrome.webview.postMessage(JSON.stringify({' +
    '          type: "codeChange",' +
    '          code: contenu' +
    '        }));' +
    '      });' +
    '    });' +
    '    ' +
    '    function definirCode(code, langage) {' +
    '      if (editor) {' +
    '        var model = editor.getModel();' +
    '        monaco.editor.setModelLanguage(model, langage || "javascript");' +
    '        editor.setValue(code);' +
    '      }' +
    '    }' +
    '    ' +
    '    function obtenirCode() {' +
    '      return editor ? editor.getValue() : "";' +
    '    }' +
    '  </script>' +
    '</body>' +
    '</html>';
end;

type
  TForm1 = class(TForm)
    EdgeBrowser1: TEdgeBrowser;
    MemoCode: TMemo;
    ButtonCharger: TButton;
    ButtonSauvegarder: TButton;
    ComboLangage: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonSauvegarderClick(Sender: TObject);
    procedure ComboLangageChange(Sender: TObject);
    procedure EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
      Args: TWebMessageReceivedEventArgs);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.NavigateToString(GenererPageMonaco);

  ComboLangage.Items.Add('javascript');
  ComboLangage.Items.Add('typescript');
  ComboLangage.Items.Add('python');
  ComboLangage.Items.Add('html');
  ComboLangage.Items.Add('css');
  ComboLangage.ItemIndex := 0;
end;

procedure TForm1.ButtonChargerClick(Sender: TObject);
var
  Code, Langage, Script: string;
begin
  Code := MemoCode.Lines.Text;
  Langage := ComboLangage.Text;

  // Échapper les caractères spéciaux
  Code := StringReplace(Code, '\', '\\', [rfReplaceAll]);
  Code := StringReplace(Code, '"', '\"', [rfReplaceAll]);
  Code := StringReplace(Code, #13#10, '\n', [rfReplaceAll]);
  Code := StringReplace(Code, #10, '\n', [rfReplaceAll]);

  Script := Format('definirCode("%s", "%s");', [Code, Langage]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

procedure TForm1.ButtonSauvegarderClick(Sender: TObject);
begin
  EdgeBrowser1.ExecuteScript('obtenirCode()',
    procedure(ErrorCode: HRESULT; const ResultObjectAsJson: string)
    var
      Code: string;
    begin
      if ErrorCode = S_OK then
      begin
        Code := ResultObjectAsJson;
        // Retirer les guillemets au début et à la fin
        Code := Copy(Code, 2, Length(Code) - 2);
        // Reconvertir \n en sauts de ligne
        Code := StringReplace(Code, '\n', #13#10, [rfReplaceAll]);

        MemoCode.Lines.Text := Code;
        ShowMessage('Code récupéré !');
      end;
    end);
end;

procedure TForm1.ComboLangageChange(Sender: TObject);
var
  Script: string;
begin
  Script := Format('if(editor) { ' +
    'monaco.editor.setModelLanguage(editor.getModel(), "%s"); }',
    [ComboLangage.Text]);
  EdgeBrowser1.ExecuteScript(Script, nil);
end;

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  JSONMessage: TJSONObject;
  TypeMessage: string;
begin
  JSONMessage := TJSONObject.ParseJSONValue(Args.WebMessageAsString) as TJSONObject;
  try
    TypeMessage := JSONMessage.GetValue<string>('type');

    if TypeMessage = 'codeChange' then
    begin
      // Le code a changé dans l'éditeur
      // On pourrait sauvegarder automatiquement, etc.
    end;
  finally
    JSONMessage.Free;
  end;
end;
```

## Gestion des ressources locales

### Intégrer des fichiers JavaScript locaux

```pascal
function GenererHTMLAvecJSLocal: string;
var
  CheminJS: string;
begin
  CheminJS := TPath.Combine(ExtractFilePath(ParamStr(0)), 'js\mon-script.js');

  Result := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <script src="file:///%s"></script>' +
    '</head>' +
    '<body>' +
    '  <h1>Page avec JS local</h1>' +
    '</body>' +
    '</html>',
    [StringReplace(CheminJS, '\', '/', [rfReplaceAll])]
  );
end;
```

### Charger des ressources depuis un serveur local

```pascal
uses
  IdHTTPServer, IdContext, IdCustomHTTPServer;

type
  TServeurLocal = class
  private
    FServer: TIdHTTPServer;
    procedure TraiterRequete(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Demarrer;
    procedure Arreter;
  end;

constructor TServeurLocal.Create(Port: Integer);
begin
  FServer := TIdHTTPServer.Create(nil);
  FServer.DefaultPort := Port;
  FServer.OnCommandGet := TraiterRequete;
end;

destructor TServeurLocal.Destroy;
begin
  FServer.Free;
  inherited;
end;

procedure TServeurLocal.TraiterRequete(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  CheminFichier, Contenu: string;
begin
  // Servir des fichiers locaux
  CheminFichier := TPath.Combine(
    ExtractFilePath(ParamStr(0)),
    'web' + ARequestInfo.Document
  );

  if FileExists(CheminFichier) then
  begin
    AResponseInfo.ContentStream := TFileStream.Create(
      CheminFichier, fmOpenRead or fmShareDenyWrite);

    // Définir le type MIME
    if CheminFichier.EndsWith('.js') then
      AResponseInfo.ContentType := 'application/javascript'
    else if CheminFichier.EndsWith('.css') then
      AResponseInfo.ContentType := 'text/css'
    else if CheminFichier.EndsWith('.html') then
      AResponseInfo.ContentType := 'text/html';
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Fichier non trouvé';
  end;
end;

procedure TServeurLocal.Demarrer;
begin
  FServer.Active := True;
end;

procedure TServeurLocal.Arreter;
begin
  FServer.Active := False;
end;

// Utilisation
var
  Serveur: TServeurLocal;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Serveur := TServeurLocal.Create(8080);
  Serveur.Demarrer;

  EdgeBrowser1.Navigate('http://localhost:8080/index.html');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Serveur.Free;
end;
```

## Sécurité

### Valider et nettoyer les données

```pascal
function NettoyerPourJavaScript(const Texte: string): string;
begin
  Result := Texte;

  // Échapper les caractères spéciaux
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);

  // Supprimer les balises HTML potentiellement dangereuses
  Result := StringReplace(Result, '<script', '&lt;script', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '</script', '&lt;/script', [rfReplaceAll, rfIgnoreCase]);
end;

procedure EnvoyerTexteSecurise(const Texte: string);
var
  TexteNettoye, Script: string;
begin
  TexteNettoye := NettoyerPourJavaScript(Texte);
  Script := Format('document.getElementById("contenu").textContent = "%s";',
    [TexteNettoye]);

  EdgeBrowser1.ExecuteScript(Script, nil);
end;
```

### Valider les messages reçus

```pascal
function ValiderMessageJSON(const Message: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  Result := False;

  try
    JSONValue := TJSONObject.ParseJSONValue(Message);
    if JSONValue <> nil then
    begin
      Result := True;
      JSONValue.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  Message: string;
begin
  Message := Args.WebMessageAsString;

  // Valider avant de traiter
  if not ValiderMessageJSON(Message) then
  begin
    ShowMessage('Message invalide reçu');
    Exit;
  end;

  // Traiter le message...
end;
```

### Content Security Policy (CSP)

```pascal
function GenererHTMLAvecCSP: string;
begin
  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <meta http-equiv="Content-Security-Policy" content="' +
    '    default-src ''self''; ' +
    '    script-src ''self'' https://cdn.jsdelivr.net https://unpkg.com; ' +
    '    style-src ''self'' ''unsafe-inline'' https://unpkg.com; ' +
    '    img-src ''self'' data: https:;">' +
    '</head>' +
    '<body>' +
    '  <h1>Page sécurisée avec CSP</h1>' +
    '</body>' +
    '</html>';
end;
```

## Débogage

### Console JavaScript dans Delphi

```pascal
const
  HTML_AVEC_CONSOLE =
    '<html>' +
    '<head>' +
    '  <script>' +
    '    // Rediriger console.log vers Delphi' +
    '    (function() {' +
    '      var originalLog = console.log;' +
    '      console.log = function() {' +
    '        var message = Array.prototype.slice.call(arguments).join(" ");' +
    '        window.chrome.webview.postMessage(JSON.stringify({' +
    '          type: "console",' +
    '          level: "log",' +
    '          message: message' +
    '        }));' +
    '        originalLog.apply(console, arguments);' +
    '      };' +
    '      ' +
    '      console.error = function() {' +
    '        var message = Array.prototype.slice.call(arguments).join(" ");' +
    '        window.chrome.webview.postMessage(JSON.stringify({' +
    '          type: "console",' +
    '          level: "error",' +
    '          message: message' +
    '        }));' +
    '      };' +
    '    })();' +
    '    ' +
    '    function tester() {' +
    '      console.log("Ceci est un test");' +
    '      console.error("Ceci est une erreur");' +
    '    }' +
    '  </script>' +
    '</head>' +
    '<body>' +
    '  <button onclick="tester()">Tester Console</button>' +
    '</body>' +
    '</html>';

procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  JSONMessage: TJSONObject;
  TypeMessage, Level, Message: string;
begin
  JSONMessage := TJSONObject.ParseJSONValue(Args.WebMessageAsString) as TJSONObject;
  try
    TypeMessage := JSONMessage.GetValue<string>('type');

    if TypeMessage = 'console' then
    begin
      Level := JSONMessage.GetValue<string>('level');
      Message := JSONMessage.GetValue<string>('message');

      // Afficher dans un memo de debug
      MemoDebug.Lines.Add(Format('[%s] %s', [Level.ToUpper, Message]));
    end;
  finally
    JSONMessage.Free;
  end;
end;
```

### DevTools embarqué

```pascal
procedure TForm1.ButtonDevToolsClick(Sender: TObject);
begin
  // Ouvrir les DevTools de Edge
  EdgeBrowser1.ExecuteScript('window.chrome.webview.openDevToolsWindow();', nil);
end;
```

## Bonnes pratiques

### Classe wrapper pour communication

```pascal
type
  TWebViewBridge = class
  private
    FWebBrowser: TEdgeBrowser;
    FMessageHandlers: TDictionary<string, TProc<TJSONObject>>;
    procedure OnWebMessageReceived(Sender: TCustomEdgeBrowser;
      Args: TWebMessageReceivedEventArgs);
  public
    constructor Create(WebBrowser: TEdgeBrowser);
    destructor Destroy; override;

    procedure EnregistrerHandler(const TypeMessage: string;
      Handler: TProc<TJSONObject>);
    procedure AppelerFonctionJS(const NomFonction: string;
      const Parametres: array of Variant);
    procedure EnvoyerMessageJS(const TypeMessage: string;
      const Donnees: TJSONObject);
  end;

constructor TWebViewBridge.Create(WebBrowser: TEdgeBrowser);
begin
  FWebBrowser := WebBrowser;
  FWebBrowser.OnWebMessageReceived := OnWebMessageReceived;
  FMessageHandlers := TDictionary<string, TProc<TJSONObject>>.Create;
end;

destructor TWebViewBridge.Destroy;
begin
  FMessageHandlers.Free;
  inherited;
end;

procedure TWebViewBridge.OnWebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  JSONMessage: TJSONObject;
  TypeMessage: string;
  Handler: TProc<TJSONObject>;
begin
  JSONMessage := TJSONObject.ParseJSONValue(Args.WebMessageAsString) as TJSONObject;
  try
    TypeMessage := JSONMessage.GetValue<string>('type');

    if FMessageHandlers.TryGetValue(TypeMessage, Handler) then
      Handler(JSONMessage);
  finally
    JSONMessage.Free;
  end;
end;

procedure TWebViewBridge.EnregistrerHandler(const TypeMessage: string;
  Handler: TProc<TJSONObject>);
begin
  FMessageHandlers.AddOrSetValue(TypeMessage, Handler);
end;

procedure TWebViewBridge.AppelerFonctionJS(const NomFonction: string;
  const Parametres: array of Variant);
var
  Script, Params: string;
  I: Integer;
begin
  Params := '';
  for I := 0 to High(Parametres) do
  begin
    if I > 0 then
      Params := Params + ',';

    case VarType(Parametres[I]) of
      varString, varUString:
        Params := Params + '"' + VarToStr(Parametres[I]) + '"';
      varInteger, varInt64, varDouble:
        Params := Params + VarToStr(Parametres[I]);
      varBoolean:
        if Parametres[I] then
          Params := Params + 'true'
        else
          Params := Params + 'false';
    end;
  end;

  Script := Format('%s(%s);', [NomFonction, Params]);
  FWebBrowser.ExecuteScript(Script, nil);
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  FBridge := TWebViewBridge.Create(EdgeBrowser1);

  // Enregistrer des handlers
  FBridge.EnregistrerHandler('notification',
    procedure(JSON: TJSONObject)
    begin
      ShowMessage(JSON.GetValue<string>('message'));
    end);

  FBridge.EnregistrerHandler('sauvegarder',
    procedure(JSON: TJSONObject)
    begin
      SauvegarderDonnees(JSON);
    end);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Appeler une fonction JavaScript
  FBridge.AppelerFonctionJS('afficherMessage', ['Hello', 123, True]);
end;
```

## Résumé

L'intégration de JavaScript via WebView ouvre un monde de possibilités.

**Points clés :**

1. **WebView** = navigateur embarqué dans votre application
2. **TEdgeBrowser** recommandé pour Windows (basé sur Chromium)
3. **Communication bidirectionnelle** : Delphi ↔ JavaScript
4. **ExecuteScript** : exécuter du JavaScript depuis Delphi
5. **WebMessageReceived** : recevoir des messages de JavaScript
6. **Bibliothèques JS** : Chart.js, D3.js, Leaflet, Monaco Editor, etc.
7. **Sécurité** : toujours nettoyer et valider les données
8. **Débogage** : rediriger console.log, utiliser DevTools
9. **Organisation** : créer des wrappers pour simplifier la communication
10. **Performance** : minimiser les allers-retours Delphi ↔ JavaScript

**Avantages :**
- Accès à l'écosystème JavaScript
- Visualisations riches et modernes
- Interfaces web dans applications natives
- Réutilisation de code existant

**Inconvénients :**
- Légère surcharge mémoire
- Complexité de la communication
- Dépendance aux bibliothèques externes
- Nécessite une connexion pour CDN (ou héberger localement)

L'intégration JavaScript via WebView est parfaite pour ajouter des visualisations avancées, des éditeurs de code, des cartes interactives ou toute fonctionnalité où JavaScript excelle, tout en gardant la logique métier et les données dans Delphi.

⏭️ [Applications mobiles avec Delphi](/14-utilisation-dapi-et-bibliotheques-externes/10-creer-dll-bibliotheques-partagees.md)
