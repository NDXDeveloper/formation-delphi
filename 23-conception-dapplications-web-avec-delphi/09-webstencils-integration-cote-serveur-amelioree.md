🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.9 WebStencils : intégration côté serveur améliorée

## Introduction

**WebStencils** est un moteur de templates côté serveur introduit dans  
**RAD Studio 12.2 Athens** (septembre 2024) et inclus dans Delphi 13  
Florence. Il simplifie la création de contenu HTML dynamique avec Delphi  
en suivant une philosophie inspirée de **Razor (ASP.NET)**.  

Les composants principaux sont :
- **`TWebStencilsProcessor`** : traite un fichier individuel (HTML +
  marqueurs WebStencils) et produit le contenu final
- **`TWebStencilsEngine`** : moteur global qui orchestre le traitement
  des fichiers dans une application Web (utilisable avec WebBroker,
  RAD Server, DataSnap…)

Ils résident dans l'unité **`Web.Stencils`** de la RTL Delphi.

⚠️ **IMPORTANT — Syntaxe** : WebStencils utilise le caractère **`@`** comme
marqueur (à la manière de Razor), **pas** la syntaxe `{{variable}}` de  
Mustache/Handlebars. Les exemples ci-dessous emploient la notation  
`{{variable}}` parce qu'elle est plus visuelle et familière pour
expliquer un système de templating **générique** ; pour la syntaxe
**officielle** de WebStencils en Delphi, consultez le tableau dédié
plus bas et la [DocWiki Embarcadero](https://docwiki.embarcadero.com/RADStudio/Athens/en/WebStencils).

Imaginez que vous puissiez créer des templates HTML avec des "trous" que  
Delphi remplira automatiquement avec des données. C'est exactement ce  
que permettent les WebStencils !  

**Analogie :** WebStencils, c'est comme un pochoir (stencil en anglais) :
vous créez un modèle avec des emplacements vides, et Delphi remplit ces  
emplacements avec les bonnes informations.  

```
Template WebStencil           Données Delphi           Résultat HTML
┌────────────────────┐       ┌─────────────────┐      ┌─────────────────┐
│ <h1>{{titre}}</h1> │   +   │ titre="Bonjour" │ =    │ <h1>Bonjour</h1>│
│ <p>{{texte}}</p>   │       │ texte="..."     │      │ <p>...</p>      │
└────────────────────┘       └─────────────────┘      └─────────────────┘
```

## Qu'est-ce que WebStencils ?

### Définition

**WebStencils** est un système de templating intégré à la **RTL Delphi**
(unité `Web.Stencils`), utilisable avec **WebBroker, RAD Server, DataSnap**
ou tout autre framework HTTP. Il permet de :
- Créer des templates HTML avec une syntaxe simple
- Injecter automatiquement des données Delphi dans ces templates
- Générer des pages web dynamiques efficacement
- Séparer proprement la présentation (HTML) du code (Delphi)

### Avant WebStencils (approche traditionnelle)

```pascal
procedure TServerMethods1.GetHomePage(Request: TWebRequest;
  Response: TWebResponse);
var
  HTML: string;
  UserName: string;
begin
  UserName := GetCurrentUser;

  // Construire le HTML manuellement (fastidieux et peu lisible)
  HTML := '<!DOCTYPE html>' +
          '<html>' +
          '<head><title>Accueil</title></head>' +
          '<body>' +
          '<h1>Bienvenue, ' + UserName + '</h1>' +
          '<p>Il est ' + TimeToStr(Now) + '</p>' +
          '</body>' +
          '</html>';

  Response.Content := HTML;
end;
```

**Problèmes :**
- Code difficile à lire
- Mélange HTML et logique
- Erreurs de syntaxe HTML fréquentes
- Maintenance compliquée
- Impossible pour un designer de modifier
- ⚠️ **Risque XSS** : `UserName` est injecté sans échappement
  (cf. section « Échappement HTML automatique » plus bas — WebStencils
  résout ce problème nativement)

### Avec WebStencils (approche moderne)

**Template HTML (home.html) :**
```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8">
  <title>{{pageTitle}}</title>
</head>
<body>
  <h1>Bienvenue, {{userName}}</h1>
  <p>Il est {{currentTime}}</p>
</body>
</html>
```

**Code Delphi :**
```pascal
procedure TServerMethods1.GetHomePage;  
var  
  Stencil: TWebStencil;
begin
  Stencil := TWebStencil.Create;
  try
    Stencil.LoadFromFile('templates/home.html');

    // Injecter les données
    Stencil.Values['pageTitle'] := 'Accueil';
    Stencil.Values['userName'] := GetCurrentUser;
    Stencil.Values['currentTime'] := TimeToStr(Now);

    // Générer et renvoyer le HTML
    Response.ContentType := 'text/html; charset=utf-8';
    Response.Content := Stencil.Render;
  finally
    Stencil.Free;
  end;
end;
```

**Avantages :**
- Code propre et lisible
- HTML séparé du code Delphi
- Facile à maintenir
- Designer peut modifier les templates
- Moins d'erreurs

## Syntaxe officielle WebStencils (Delphi 12.2+)

Voici la **vraie** syntaxe utilisée par `TWebStencilsProcessor` en Delphi —  
inspirée de Razor :  

| Construction | Syntaxe WebStencils Delphi | Équivalent Mustache (utilisé dans les exemples didactiques ci-dessous) |
|--------------|----------------------------|------------------------------------------------------------------------|
| Valeur d'un objet | `@Object.Property` | `{{property}}` |
| Boucle sur une liste | `@foreach (var item in Items) { … @item.Name … }` | `{{#each items}} … {{/each}}` |
| Condition | `@if (condition) { … } else { … }` | `{{#if condition}} … {{else}} … {{/if}}` |
| Échappement `@` | `@@` (double arobase) | `\{{` |
| Bloc de code | `@{ var x = …; }` | (non disponible) |

**Exemple officiel de template `.html` WebStencils :**
```html
<h1>Bienvenue @Person.FirstName !</h1>
<ul>
@foreach (var p in People)
{
  <li>@p.FirstName @p.LastName (@p.Age ans)</li>
}
</ul>
```

**Côté Delphi**, on dépose un `TWebStencilsProcessor` sur le module Web,
on enregistre les objets/listes via `AddVar`, et on associe le processor
à une `TWebActionItem.Producer`. Voir la [documentation officielle](https://docwiki.embarcadero.com/RADStudio/Athens/en/WebStencils)
pour un exemple complet de bout en bout.

> Le reste de ce document utilise délibérément la syntaxe Mustache  
> (`{{variable}}`) car elle reste très répandue (Handlebars, Jinja, Vue…)  
> et illustre les **concepts généraux** d'un moteur de template. Les  
> mêmes idées s'appliquent à WebStencils — il suffit de transposer la  
> syntaxe.

## Syntaxe (style Mustache, à titre didactique)

### Variables simples

**Syntaxe :** `{{nomVariable}}`

**Template :**
```html
<h1>{{titre}}</h1>
<p>{{description}}</p>
<span>{{prix}} €</span>
```

**Code Delphi :**
```pascal
Stencil.Values['titre'] := 'Produit XYZ';  
Stencil.Values['description'] := 'Un excellent produit';  
Stencil.Values['prix'] := '99.99';  
```

**Résultat :**
```html
<h1>Produit XYZ</h1>
<p>Un excellent produit</p>
<span>99.99 €</span>
```

### Boucles (itération)

**Syntaxe :** `{{#each collection}} ... {{/each}}`

**Template :**
```html
<h2>Liste des clients</h2>
<table>
  <thead>
    <tr>
      <th>Nom</th>
      <th>Email</th>
    </tr>
  </thead>
  <tbody>
    {{#each clients}}
    <tr>
      <td>{{nom}}</td>
      <td>{{email}}</td>
    </tr>
    {{/each}}
  </tbody>
</table>
```

**Code Delphi :**
```pascal
var
  Stencil: TWebStencil;
  ClientsArray: TJSONArray;
  ClientObj: TJSONObject;
  Query: TFDQuery;
begin
  Stencil := TWebStencil.Create;
  try
    Stencil.LoadFromFile('templates/clients.html');

    // ⚠️ ClientsArray DOIT être dans son propre try/finally pour éviter
    //    une fuite si Query.Open ou Stencil.Render lève une exception.
    ClientsArray := TJSONArray.Create;
    try
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := FDConnection1;
        Query.SQL.Text := 'SELECT nom, email FROM clients';
        Query.Open;

        while not Query.Eof do
        begin
          ClientObj := TJSONObject.Create;
          ClientObj.AddPair('nom', Query.FieldByName('nom').AsString);
          ClientObj.AddPair('email', Query.FieldByName('email').AsString);
          ClientsArray.Add(ClientObj);  // ClientsArray possède désormais ClientObj

          Query.Next;
        end;
      finally
        Query.Free;
      end;

      // Passer le tableau au stencil (l'API exacte dépend du moteur
      // de templates ; consulter sa documentation pour savoir si le
      // stencil prend la propriété du JSONArray ou s'en fait une copie).
      Stencil.Values['clients'] := ClientsArray;

      Response.ContentType := 'text/html; charset=utf-8';
      Response.Content := Stencil.Render;
    finally
      ClientsArray.Free;  // libère aussi les ClientObj contenus
    end;
  finally
    Stencil.Free;
  end;
end;
```

### Conditions

**Syntaxe :** `{{#if condition}} ... {{else}} ... {{/if}}`

**Template :**
```html
{{#if isLoggedIn}}
  <p>Bienvenue, {{userName}}</p>
  <a href="/logout">Déconnexion</a>
{{else}}
  <a href="/login">Connexion</a>
{{/if}}

{{#if hasMessages}}
  <div class="notifications">
    Vous avez {{messageCount}} nouveaux messages
  </div>
{{/if}}
```

**Code Delphi :**
```pascal
Stencil.Values['isLoggedIn'] := TJSONBool.Create(UserIsConnected);  
Stencil.Values['userName'] := CurrentUserName;  
Stencil.Values['hasMessages'] := TJSONBool.Create(MessageCount > 0);  
Stencil.Values['messageCount'] := IntToStr(MessageCount);  
```

### Sections imbriquées

**Template :**
```html
<div class="orders">
  {{#each orders}}
  <div class="order">
    <h3>Commande #{{orderNumber}}</h3>
    <p>Date: {{orderDate}}</p>

    <table>
      <tr>
        <th>Produit</th>
        <th>Quantité</th>
        <th>Prix</th>
      </tr>
      {{#each items}}
      <tr>
        <td>{{productName}}</td>
        <td>{{quantity}}</td>
        <td>{{price}} €</td>
      </tr>
      {{/each}}
    </table>

    <p><strong>Total: {{total}} €</strong></p>
  </div>
  {{/each}}
</div>
```

**Code Delphi :**
```pascal
// 💡 Règle de propriété TJSON* : Add / AddPair transfère la propriété au
//    conteneur — c'est lui qui libérera l'enfant lors de son propre Free.
//    Ici, libérer OrdersArray à la fin libère en cascade OrderObj puis
//    ItemsArray puis chaque ItemObj.
var
  OrdersArray, ItemsArray: TJSONArray;
  OrderObj, ItemObj: TJSONObject;
begin
  OrdersArray := TJSONArray.Create;
  try
    // Première commande
    OrderObj := TJSONObject.Create;
    OrderObj.AddPair('orderNumber', '12345');
    OrderObj.AddPair('orderDate', '2026-01-15');
    OrderObj.AddPair('total', '150.00');

    // Articles de la commande
    ItemsArray := TJSONArray.Create;

    ItemObj := TJSONObject.Create;
    ItemObj.AddPair('productName', 'Produit A');
    ItemObj.AddPair('quantity', '2');
    ItemObj.AddPair('price', '50.00');
    ItemsArray.Add(ItemObj);

    ItemObj := TJSONObject.Create;
    ItemObj.AddPair('productName', 'Produit B');
    ItemObj.AddPair('quantity', '1');
    ItemObj.AddPair('price', '50.00');
    ItemsArray.Add(ItemObj);

    OrderObj.AddPair('items', ItemsArray);
    OrdersArray.Add(OrderObj);

    Stencil.Values['orders'] := OrdersArray;
    Response.ContentType := 'text/html; charset=utf-8';
    Response.Content := Stencil.Render;
  finally
    OrdersArray.Free;  // libère récursivement toute la hiérarchie
  end;
end;
```

## Fonctionnalités avancées

### Inclusion de templates (partials)

**Principe :** Réutiliser des morceaux de templates

**Template principal (page.html) :**
```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8">
  <title>{{pageTitle}}</title>
</head>
<body>
  {{> header}}

  <main>
    {{content}}
  </main>

  {{> footer}}
</body>
</html>
```

**Template header (header.html) :**
```html
<header>
  <nav>
    <a href="/">Accueil</a>
    <a href="/clients">Clients</a>
    <a href="/produits">Produits</a>
  </nav>
  {{#if isLoggedIn}}
    <div class="user-info">{{userName}}</div>
  {{/if}}
</header>
```

**Template footer (footer.html) :**
```html
<footer>
  <p>&copy; {{year}} MonApp - Tous droits réservés</p>
</footer>
```

> 💡 `{{year}}` plutôt qu'une année en dur — sinon le footer  
> affichera 2025 ad vitam aeternam. Côté Delphi :  
> `Stencil.Values['year'] := IntToStr(YearOf(Now));`  
> (unité `System.DateUtils`).

**Code Delphi :**
```pascal
var
  Stencil: TWebStencil;
begin
  Stencil := TWebStencil.Create;
  try
    // Enregistrer les partials
    Stencil.RegisterPartial('header', 'templates/header.html');
    Stencil.RegisterPartial('footer', 'templates/footer.html');

    // Charger le template principal
    Stencil.LoadFromFile('templates/page.html');

    // Remplir les données
    Stencil.Values['pageTitle'] := 'Ma Page';
    Stencil.Values['isLoggedIn'] := TJSONBool.Create(True);
    Stencil.Values['userName'] := 'Jean Dupont';
    Stencil.Values['content'] := '<p>Contenu principal de la page</p>';

    Response.ContentType := 'text/html; charset=utf-8';
    Response.Content := Stencil.Render;
  finally
    Stencil.Free;
  end;
end;
```

### Helpers (fonctions d'aide)

**Helpers personnalisés pour formater les données :**

**Template :**
```html
<p>Date de création: {{formatDate createdAt}}</p>
<p>Prix: {{formatCurrency price}}</p>
<p>Description: {{truncate description 100}}</p>
```

**Code Delphi :**
```pascal
// Enregistrer des helpers personnalisés
procedure RegisterCustomHelpers(Stencil: TWebStencil);  
begin  
  // Helper pour formater une date
  Stencil.RegisterHelper('formatDate',
    function(Args: TArray<TValue>): string
    var
      DateValue: TDateTime;
    begin
      if Length(Args) > 0 then
      begin
        DateValue := Args[0].AsType<TDateTime>;
        Result := FormatDateTime('dd/mm/yyyy', DateValue);
      end;
    end);

  // Helper pour formater une devise
  Stencil.RegisterHelper('formatCurrency',
    function(Args: TArray<TValue>): string
    var
      Amount: Currency;
    begin
      if Length(Args) > 0 then
      begin
        Amount := Args[0].AsType<Currency>;
        Result := FormatCurr('#,##0.00 €', Amount);
      end;
    end);

  // Helper pour tronquer un texte
  Stencil.RegisterHelper('truncate',
    function(Args: TArray<TValue>): string
    var
      Text: string;
      MaxLength: Integer;
    begin
      if Length(Args) > 1 then
      begin
        Text := Args[0].AsString;
        MaxLength := Args[1].AsInteger;

        if Length(Text) > MaxLength then
          Result := Copy(Text, 1, MaxLength) + '...'
        else
          Result := Text;
      end;
    end);
end;

// Utilisation
var
  Stencil: TWebStencil;
begin
  Stencil := TWebStencil.Create;
  try
    RegisterCustomHelpers(Stencil);

    Stencil.LoadFromFile('templates/product.html');
    // 💡 TJSONString / TJSONNumber sont confiés au Stencil — qui les
    //    libérera (selon l'API du moteur). Consulter la doc du moteur
    //    de templates utilisé pour vérifier la sémantique d'ownership.
    Stencil.Values['createdAt'] := TJSONString.Create(DateToISO8601(Now));
    Stencil.Values['price'] := TJSONNumber.Create(99.99);
    Stencil.Values['description'] := 'Ceci est une très longue description...';

    Response.ContentType := 'text/html; charset=utf-8';
    Response.Content := Stencil.Render;
  finally
    Stencil.Free;
  end;
end;
```

### Échappement HTML automatique

WebStencils échappe automatiquement les caractères HTML pour éviter les failles XSS.

**Template :**
```html
<!-- Échappé automatiquement (sûr) -->
<p>Commentaire: {{userComment}}</p>

<!-- Non échappé (dangereux - à éviter) -->
<div>{{{unsafeHTML}}}</div>
```

**Code Delphi :**
```pascal
// Le contenu malveillant sera échappé automatiquement
Stencil.Values['userComment'] := '<script>alert("XSS")</script>';

// Résultat dans le HTML:
// <p>Commentaire: &lt;script&gt;alert("XSS")&lt;/script&gt;</p>
```

## Architecture avec WebStencils

### Organisation des fichiers

**Structure de projet recommandée :**
```
MonProjet/
├── Source/
│   ├── ServerMethods.pas
│   ├── Controllers/
│   │   ├── HomeController.pas
│   │   ├── ClientsController.pas
│   │   └── ProductsController.pas
│   └── Models/
│       ├── ClientModel.pas
│       └── ProductModel.pas
├── Templates/
│   ├── layouts/
│   │   └── main.html
│   ├── partials/
│   │   ├── header.html
│   │   ├── footer.html
│   │   └── sidebar.html
│   ├── pages/
│   │   ├── home.html
│   │   ├── clients/
│   │   │   ├── list.html
│   │   │   └── detail.html
│   │   └── products/
│   │       ├── list.html
│   │       └── detail.html
│   └── emails/
│       ├── welcome.html
│       └── notification.html
├── Public/
│   ├── css/
│   │   └── style.css
│   ├── js/
│   │   └── app.js
│   └── images/
└── Config/
    └── routes.ini
```

### Pattern MVC avec WebStencils

**Modèle (Model) :**
```pascal
unit ClientModel;

interface

uses
  System.JSON, FireDAC.Comp.Client;

type
  TClientModel = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    function GetAll: TJSONArray;
    function GetById(ID: Integer): TJSONObject;
    function AddClient(const Name, Email: string): Integer;
    procedure Update(ID: Integer; const Name, Email: string);
    procedure Delete(ID: Integer);
  end;

implementation

constructor TClientModel.Create(AConnection: TFDConnection);  
begin  
  FConnection := AConnection;
end;

function TClientModel.GetAll: TJSONArray;  
var  
  Query: TFDQuery;
  ClientObj: TJSONObject;
begin
  // try/except autour de Result : si Query.Open lève, on libère Result
  // pour éviter une fuite côté appelant.
  Result := TJSONArray.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients ORDER BY nom';
      Query.Open;

      while not Query.Eof do
      begin
        ClientObj := TJSONObject.Create;
        ClientObj.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
        ClientObj.AddPair('nom', Query.FieldByName('nom').AsString);
        ClientObj.AddPair('prenom', Query.FieldByName('prenom').AsString);
        ClientObj.AddPair('email', Query.FieldByName('email').AsString);
        Result.Add(ClientObj);

        Query.Next;
      end;
    finally
      Query.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TClientModel.GetById(ID: Integer): TJSONObject;  
var  
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Result := TJSONObject.Create;
      try
        Result.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
        Result.AddPair('nom', Query.FieldByName('nom').AsString);
        Result.AddPair('prenom', Query.FieldByName('prenom').AsString);
        Result.AddPair('email', Query.FieldByName('email').AsString);
        Result.AddPair('telephone', Query.FieldByName('telephone').AsString);
      except
        Result.Free;
        Result := nil;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

**Contrôleur (Controller) :**
```pascal
unit ClientsController;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,
  ClientModel, WebStencils;

type
  TClientsController = class
  private
    FModel: TClientModel;
    function RenderTemplate(const TemplateName: string;
      Data: TJSONValue): string;
  public
    constructor Create(AModel: TClientModel);
    destructor Destroy; override;

    procedure ListClients(Request: TWebRequest; Response: TWebResponse);
    procedure ShowClient(Request: TWebRequest; Response: TWebResponse);
    procedure EditClient(Request: TWebRequest; Response: TWebResponse);
  end;

implementation

constructor TClientsController.Create(AModel: TClientModel);  
begin  
  FModel := AModel;
end;

destructor TClientsController.Destroy;  
begin  
  inherited;
end;

function TClientsController.RenderTemplate(const TemplateName: string;
  Data: TJSONValue): string;
var
  Stencil: TWebStencil;
  PagesRoot, FullPath: string;
begin
  // 🚨 ANTI PATH-TRAVERSAL : valider que TemplateName ne sort pas du dossier
  //    pages/. Sans cette protection, un appel à RenderTemplate('../../etc/passwd')
  //    pourrait lire un fichier arbitraire.
  PagesRoot := TPath.GetFullPath('templates/pages');
  FullPath := TPath.GetFullPath(TPath.Combine(PagesRoot, TemplateName));
  if not FullPath.StartsWith(PagesRoot + PathDelim) then
    raise Exception.CreateFmt('Template invalide : %s', [TemplateName]);

  Stencil := TWebStencil.Create;
  try
    // Charger le layout principal
    Stencil.LoadFromFile('templates/layouts/main.html');

    // Enregistrer les partials
    Stencil.RegisterPartial('header', 'templates/partials/header.html');
    Stencil.RegisterPartial('footer', 'templates/partials/footer.html');

    // Charger le contenu de la page (FullPath déjà validé)
    Stencil.Values['content'] := LoadFileAsString(FullPath);

    // Ajouter les données
    if Assigned(Data) then
      Stencil.Values['data'] := Data;

    // Variables globales.
    // ⚠️ `CurrentYear` n'existe pas en Delphi standard — utiliser
    //    YearOf(Now) (unité System.DateUtils) pour obtenir l'année.
    Stencil.Values['appName'] := 'MonApp';
    Stencil.Values['year'] := IntToStr(YearOf(Now));

    Result := Stencil.Render;
  finally
    Stencil.Free;
  end;
end;

procedure TClientsController.ListClients(Request: TWebRequest;
  Response: TWebResponse);
var
  Clients: TJSONArray;
  PageData: TJSONObject;
begin
  // Récupérer les données du modèle
  Clients := FModel.GetAll;

  // Préparer les données pour la vue
  PageData := TJSONObject.Create;
  try
    PageData.AddPair('pageTitle', 'Liste des clients');
    PageData.AddPair('clients', Clients);
    PageData.AddPair('clientCount', TJSONNumber.Create(Clients.Count));

    // Rendre le template
    Response.ContentType := 'text/html; charset=utf-8';
    Response.Content := RenderTemplate('clients/list.html', PageData);
  finally
    PageData.Free;
  end;
end;

procedure TClientsController.ShowClient(Request: TWebRequest;
  Response: TWebResponse);
var
  ClientID: Integer;
  Client: TJSONObject;
  PageData: TJSONObject;
begin
  ClientID := StrToIntDef(Request.QueryFields.Values['id'], 0);

  if ClientID > 0 then
  begin
    Client := FModel.GetById(ClientID);

    if Assigned(Client) then
    begin
      PageData := TJSONObject.Create;
      try
        PageData.AddPair('pageTitle', 'Détails du client');
        PageData.AddPair('client', Client);

        Response.ContentType := 'text/html; charset=utf-8';
        Response.Content := RenderTemplate('clients/detail.html', PageData);
      finally
        PageData.Free;
      end;
    end
    else
    begin
      Response.StatusCode := 404;
      Response.ContentType := 'text/plain; charset=utf-8';
      Response.Content := 'Client non trouvé';
    end;
  end
  else
  begin
    Response.StatusCode := 400;
    Response.ContentType := 'text/plain; charset=utf-8';
    Response.Content := 'ID invalide';
  end;
end;

end.
```

**Vue (Template - clients/list.html) :**
```html
<div class="container">
  <h1>{{pageTitle}}</h1>

  <p>Total: {{clientCount}} clients</p>

  <a href="/clients/new" class="btn btn-primary">+ Nouveau client</a>

  <table class="table">
    <thead>
      <tr>
        <th>Nom</th>
        <th>Prénom</th>
        <th>Email</th>
        <th>Actions</th>
      </tr>
    </thead>
    <tbody>
      {{#each data.clients}}
      <tr>
        <td>{{nom}}</td>
        <td>{{prenom}}</td>
        <td>{{email}}</td>
        <td>
          <a href="/clients/{{id}}">Voir</a>
          <a href="/clients/{{id}}/edit">Modifier</a>
        </td>
      </tr>
      {{/each}}
    </tbody>
  </table>

  {{#unless data.clients}}
  <p class="text-muted">Aucun client trouvé.</p>
  {{/unless}}
</div>
```

## Cache de templates

Pour améliorer les performances, mettre en cache les templates :

```pascal
unit TemplateCache;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  System.Generics.Collections, System.SyncObjs;

type
  TTemplateCache = class
  private
    FCache: TDictionary<string, string>;
    FLock: TCriticalSection;  // ⚠️ Indispensable en multi-thread
    FEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTemplate(const FileName: string): string;
    procedure Clear;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  GlobalTemplateCache: TTemplateCache;

implementation

constructor TTemplateCache.Create;  
begin  
  FCache := TDictionary<string, string>.Create;
  FLock := TCriticalSection.Create;
  {$IFDEF DEBUG}
  FEnabled := False; // Désactivé en développement
  {$ELSE}
  FEnabled := True;  // Activé en production
  {$ENDIF}
end;

destructor TTemplateCache.Destroy;  
begin  
  FCache.Free;
  FLock.Free;
  inherited;
end;

function TTemplateCache.GetTemplate(const FileName: string): string;  
begin  
  // ⚠️ Sans verrou, deux threads HTTP simultanés sur le même fichier
  //    peuvent provoquer une EAccessViolation aléatoire (TDictionary
  //    n'est pas thread-safe). On verrouille pour les opérations d'écriture.
  FLock.Enter;
  try
    if FEnabled and FCache.TryGetValue(FileName, Result) then
      Exit;

    // Charger depuis le disque
    if not FileExists(FileName) then
      raise Exception.CreateFmt('Template non trouvé: %s', [FileName]);

    Result := TFile.ReadAllText(FileName, TEncoding.UTF8);

    if FEnabled then
      FCache.AddOrSetValue(FileName, Result);
  finally
    FLock.Leave;
  end;
end;

procedure TTemplateCache.Clear;  
begin  
  FLock.Enter;
  try
    FCache.Clear;
  finally
    FLock.Leave;
  end;
end;

initialization
  GlobalTemplateCache := TTemplateCache.Create;

finalization
  GlobalTemplateCache.Free;

end.
```

**Utilisation :**
```pascal
// Au lieu de charger directement
Stencil.LoadFromFile('template.html');

// Utiliser le cache
Stencil.LoadFromString(GlobalTemplateCache.GetTemplate('template.html'));
```

## Génération d'emails HTML

WebStencils est parfait pour générer des emails HTML personnalisés :

**Template email (welcome.html) :**
```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8">
  <style>
    body { font-family: Arial, sans-serif; }
    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
    .header { background: #007bff; color: white; padding: 20px; text-align: center; }
    .content { padding: 20px; background: #f8f9fa; }
    .button {
      display: inline-block;
      padding: 10px 20px;
      background: #28a745;
      color: white;
      text-decoration: none;
      border-radius: 5px;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>Bienvenue sur {{appName}} !</h1>
    </div>
    <div class="content">
      <p>Bonjour {{userName}},</p>

      <p>Merci de vous être inscrit ! Votre compte a été créé avec succès.</p>

      <p>Vos informations de connexion :</p>
      <ul>
        <li>Email: {{userEmail}}</li>
        <li>Date d'inscription: {{registrationDate}}</li>
      </ul>

      <p style="text-align: center;">
        <a href="{{activationLink}}" class="button">Activer mon compte</a>
      </p>

      <p>Si vous n'êtes pas à l'origine de cette inscription, veuillez ignorer cet email.</p>

      <p>Cordialement,<br>L'équipe {{appName}}</p>
    </div>
  </div>
</body>
</html>
```

**Code Delphi :**
```pascal
uses
  IdSMTP, IdMessage, IdSSLOpenSSL;

procedure SendWelcomeEmail(const UserName, UserEmail: string);  
var  
  Stencil: TWebStencil;
  HTMLContent: string;
  SMTP: TIdSMTP;
  Message: TIdMessage;
  ActivationToken: string;
begin
  // Générer le token d'activation
  ActivationToken := GenerateActivationToken(UserEmail);

  // Préparer le template email
  Stencil := TWebStencil.Create;
  try
    Stencil.LoadFromFile('templates/emails/welcome.html');

    Stencil.Values['appName'] := 'MonApp';
    Stencil.Values['userName'] := UserName;
    Stencil.Values['userEmail'] := UserEmail;
    Stencil.Values['registrationDate'] := FormatDateTime('dd/mm/yyyy', Now);
    // ⚠️ Encoder le token pour l'URL : caractères +, /, = (typiques en
    //    base64) seraient mal interprétés par le navigateur sinon.
    //    Unité requise : System.NetEncoding.
    Stencil.Values['activationLink'] :=
      'https://monapp.com/activate?token=' +
      TNetEncoding.URL.Encode(ActivationToken);

    HTMLContent := Stencil.Render;
  finally
    Stencil.Free;
  end;

  // Envoyer l'email
  SMTP := TIdSMTP.Create(nil);
  Message := TIdMessage.Create(nil);
  try
    // Configuration SMTP avec STARTTLS sur le port 587 (recommandé).
    // ⚠️ Sur le port 587 sans IOHandler SSL, les identifiants partent
    //    EN CLAIR sur le réseau. Toujours associer un TIdSSLIOHandlerSocketOpenSSL
    //    et activer utUseExplicitTLS.
    SMTP.Host := 'smtp.example.com';
    SMTP.Port := 587;
    SMTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(SMTP);
    SMTP.UseTLS := utUseExplicitTLS;     // STARTTLS sur port 587
    SMTP.Username := 'noreply@monapp.com';
    // 🚨 NE JAMAIS hardcoder un mot de passe : charger depuis une variable
    //    d'environnement ou un secret store (cf. avertissement JWT).
    SMTP.Password := GetEnvironmentVariable('SMTP_PASSWORD');

    // Configuration du message
    Message.From.Address := 'noreply@monapp.com';
    Message.From.Name := 'MonApp';
    Message.Recipients.Add.Address := UserEmail;
    Message.Subject := 'Bienvenue sur MonApp !';
    Message.ContentType := 'text/html';
    Message.CharSet := 'utf-8';          // pour les accents
    Message.Body.Text := HTMLContent;

    SMTP.Connect;
    try
      SMTP.Send(Message);
    finally
      SMTP.Disconnect;
    end;
  finally
    Message.Free;
    SMTP.Free;
  end;
end;
```

## Internationalisation (i18n)

Gérer plusieurs langues avec WebStencils :

**Fichiers de traduction :**

**fr.json :**
```json
{
  "welcome": "Bienvenue",
  "login": "Connexion",
  "logout": "Déconnexion",
  "clients": "Clients",
  "products": "Produits",
  "hello": "Bonjour, {{name}} !",
  "itemCount": "{{count}} élément(s)"
}
```

**en.json :**
```json
{
  "welcome": "Welcome",
  "login": "Login",
  "logout": "Logout",
  "clients": "Clients",
  "products": "Products",
  "hello": "Hello, {{name}}!",
  "itemCount": "{{count}} item(s)"
}
```

**Code Delphi :**
```pascal
unit I18n;

interface

uses
  System.JSON, System.Generics.Collections;

type
  TI18n = class
  private
    FTranslations: TDictionary<string, TJSONObject>;
    FCurrentLanguage: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadLanguage(const LanguageCode: string);
    function Translate(const Key: string; Params: TJSONObject = nil): string;

    property CurrentLanguage: string read FCurrentLanguage write FCurrentLanguage;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

constructor TI18n.Create;  
begin  
  FTranslations := TDictionary<string, TJSONObject>.Create;
  FCurrentLanguage := 'fr'; // Langue par défaut
end;

destructor TI18n.Destroy;  
var  
  Trans: TJSONObject;
begin
  for Trans in FTranslations.Values do
    Trans.Free;
  FTranslations.Free;
  inherited;
end;

procedure TI18n.LoadLanguage(const LanguageCode: string);  
const  
  // 🚨 Whitelist anti-path-traversal : sans cela, LanguageCode = '../../etc/passwd'
  //    permettrait de lire un fichier arbitraire du disque.
  ALLOWED_LANGS: array[0..3] of string = ('fr', 'en', 'es', 'de');
var
  FileName: string;
  JSONText: string;
  ParsedValue: TJSONValue;
  JSONObj: TJSONObject;
  i: Integer;
  Allowed: Boolean;
begin
  // Validation stricte du code de langue
  Allowed := False;
  for i := 0 to High(ALLOWED_LANGS) do
    if SameText(LanguageCode, ALLOWED_LANGS[i]) then
    begin
      Allowed := True;
      Break;
    end;
  if not Allowed then
    raise Exception.CreateFmt('Code de langue non supporté : %s', [LanguageCode]);

  // TPath.Combine = séparateur correct sur Windows comme sur Linux
  FileName := TPath.Combine('i18n', LanguageCode + '.json');

  if not FTranslations.ContainsKey(LanguageCode) then
  begin
    if TFile.Exists(FileName) then
    begin
      JSONText := TFile.ReadAllText(FileName, TEncoding.UTF8);
      // ⚠️ ParseJSONValue retourne nil si le JSON est invalide. Un `as
      //    TJSONObject` direct masquerait l'erreur (nil propagé dans le
      //    cache) ou déclencherait une EInvalidCast si la racine n'est
      //    pas un objet (array, string, etc.). On vérifie explicitement
      //    et on libère la valeur parsée si elle n'a pas le bon type.
      ParsedValue := TJSONObject.ParseJSONValue(JSONText);
      if not Assigned(ParsedValue) then
        raise Exception.CreateFmt('JSON de traduction invalide : %s', [FileName]);
      if not (ParsedValue is TJSONObject) then
      begin
        ParsedValue.Free;
        raise Exception.CreateFmt(
          'JSON de traduction doit être un objet (racine) : %s', [FileName]);
      end;
      JSONObj := TJSONObject(ParsedValue);
      FTranslations.Add(LanguageCode, JSONObj);
    end
    else
      raise Exception.CreateFmt('Fichier de traduction non trouvé: %s', [FileName]);
  end;

  FCurrentLanguage := LanguageCode;
end;

function TI18n.Translate(const Key: string; Params: TJSONObject): string;  
var  
  TransObj: TJSONObject;
  Value: TJSONValue;
  Pair: TJSONPair;
begin
  if FTranslations.TryGetValue(FCurrentLanguage, TransObj) then
  begin
    Value := TransObj.GetValue(Key);
    if Assigned(Value) then
    begin
      Result := Value.Value;

      // Remplacer les paramètres
      if Assigned(Params) then
      begin
        for Pair in Params do
          Result := StringReplace(Result,
            '{{' + Pair.JsonString.Value + '}}',
            Pair.JsonValue.Value,
            [rfReplaceAll]);
      end;
    end
    else
      Result := Key; // Retourner la clé si traduction non trouvée
  end
  else
    Result := Key;
end;

end.
```

**Utilisation dans un contrôleur :**
```pascal
// Helper pour extraire le code de langue principal du header Accept-Language.
// Exemple : "fr-FR,fr;q=0.9,en;q=0.8" → "fr"
function ParseAcceptLanguage(const AcceptLang, DefaultLang: string): string;  
var  
  P: Integer;
begin
  Result := Trim(AcceptLang);
  if Result = '' then Exit(DefaultLang);

  // Garder uniquement la première préférence (avant la virgule)
  P := Pos(',', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);

  // Couper la qualité éventuelle (« fr;q=0.9 » → « fr »)
  P := Pos(';', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);

  // Couper la sous-balise régionale (« fr-FR » → « fr ») si on ne supporte
  // que les langues sans variante régionale.
  P := Pos('-', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);

  Result := LowerCase(Trim(Result));
  if Result = '' then
    Result := DefaultLang;
end;

var
  Stencil: TWebStencil;
  i18n: TI18n;
  UserLang: string;
  Params: TJSONObject;
begin
  // ⚠️ Accept-Language est de la forme "fr-FR,fr;q=0.9,en;q=0.8" — il
  //    faut PARSER avant de l'utiliser comme nom de fichier de traduction,
  //    sinon LoadLanguage cherchera un fichier inexistant.
  UserLang := ParseAcceptLanguage(
    Request.GetFieldByName('Accept-Language'), 'fr');

  i18n := TI18n.Create;
  try
    i18n.LoadLanguage(UserLang);

    Stencil := TWebStencil.Create;
    try
      Stencil.LoadFromFile('template.html');

      // Ajouter toutes les traductions au stencil
      Stencil.Values['t_welcome'] := i18n.Translate('welcome');
      Stencil.Values['t_login'] := i18n.Translate('login');
      Stencil.Values['t_logout'] := i18n.Translate('logout');

      // Avec paramètres — Params est libéré APRÈS l'appel à Translate
      Params := TJSONObject.Create;
      try
        Params.AddPair('name', UserName);
        Stencil.Values['t_hello'] := i18n.Translate('hello', Params);
      finally
        Params.Free;  // ⚠️ Sans ce Free, fuite mémoire à chaque requête
      end;

      Response.ContentType := 'text/html; charset=utf-8';
      Response.Content := Stencil.Render;
    finally
      Stencil.Free;
    end;
  finally
    i18n.Free;
  end;
end;
```

## Comparaison avec d'autres solutions

### WebStencils vs Concaténation manuelle

| Aspect | Concaténation | WebStencils |
|--------|---------------|-------------|
| **Lisibilité** | ❌ Mauvaise | ✅ Excellente |
| **Maintenance** | ❌ Difficile | ✅ Facile |
| **Séparation** | ❌ Code mélangé | ✅ Séparé |
| **Performance** | ✅ Rapide | ✅ Rapide (avec cache) |
| **Collaboration** | ❌ Développeurs seuls | ✅ Designers + Devs |

### WebStencils vs JavaScript (TMS Web Core)

| Aspect | TMS Web Core | WebStencils |
|--------|--------------|-------------|
| **Exécution** | Client (navigateur) | Serveur |
| **SEO** | ⚠️ Limité | ✅ Excellent |
| **Performance initiale** | ⚠️ Chargement JS | ✅ HTML direct |
| **Interactivité** | ✅ Excellente | ⚠️ Limitée |
| **Hors ligne** | ✅ Possible (PWA) | ❌ Non |

**Meilleure approche :** Combiner les deux !
- WebStencils pour le rendu initial (SEO, performance)
- JavaScript pour l'interactivité côté client

### WebStencils vs Autres moteurs de templates

| Moteur | Langage | Complexité | Performance |
|--------|---------|-----------|-------------|
| **WebStencils** | Delphi | Moyenne | Excellente |
| **Mustache** | Multi | Faible | Bonne |
| **Handlebars** | JavaScript | Moyenne | Bonne |
| **Jinja2** | Python | Élevée | Bonne |
| **Razor** | C#/ASP.NET | Élevée | Excellente |

## Bonnes pratiques

### 1. Organisation des templates

```
templates/
├── layouts/          # Layouts principaux
│   ├── main.html
│   └── admin.html
├── partials/         # Composants réutilisables
│   ├── header.html
│   ├── footer.html
│   ├── nav.html
│   └── sidebar.html
├── pages/            # Pages complètes
│   ├── home.html
│   ├── about.html
│   └── contact.html
├── components/       # Composants spécifiques
│   ├── card.html
│   ├── table.html
│   └── form.html
└── emails/           # Templates d'emails
    ├── welcome.html
    └── notification.html
```

### 2. Nommage des variables

```pascal
// ❌ Mauvais
Stencil.Values['v1'] := 'valeur';  
Stencil.Values['x'] := ClientName;  

// ✅ Bon
Stencil.Values['pageTitle'] := 'Accueil';  
Stencil.Values['clientName'] := ClientName;  
Stencil.Values['isAuthenticated'] := TJSONBool.Create(True);  
```

### 3. Validation des données

```pascal
procedure RenderClientPage(ClientID: Integer);  
var  
  Client: TJSONObject;
  Nom: string;
begin
  Client := ClientModel.GetById(ClientID);

  // Toujours vérifier que les données existent
  if not Assigned(Client) then
  begin
    Response.StatusCode := 404;
    RenderErrorPage('Client non trouvé');
    Exit;
  end;

  // ⚠️ TryGetValue<T> retourne un BOOLEAN et remplit un OUT.
  //    On extrait d'abord la valeur, puis on la valide.
  if Client.TryGetValue<string>('nom', Nom) and (not Nom.IsEmpty) then
    RenderTemplate('client.html', Client)
  else
    RenderErrorPage('Données client invalides');
end;
```

### 4. Échappement et sécurité

```pascal
// ✅ Toujours échapper les entrées utilisateur
Stencil.Values['userInput'] := HTMLEscape(Request.ContentFields.Values['comment']);

// ❌ Ne jamais utiliser du HTML non échappé depuis l'utilisateur
// Stencil.Values['unsafeContent'] := '{{{' + UserInput + '}}}';
```

### 5. Performance

```pascal
// ✅ Utiliser le cache en production
{$IFDEF RELEASE}
GlobalTemplateCache.Enabled := True;
{$ENDIF}

// ✅ Précharger les templates fréquemment utilisés
procedure PreloadTemplates;  
begin  
  GlobalTemplateCache.GetTemplate('templates/layouts/main.html');
  GlobalTemplateCache.GetTemplate('templates/partials/header.html');
  GlobalTemplateCache.GetTemplate('templates/partials/footer.html');
end;

// ✅ Minimiser les requêtes base de données
// Charger toutes les données nécessaires en une fois
```

## Limitations de WebStencils

### Limitations actuelles

❌ **Pas de logique complexe dans les templates**
- Pas de calculs
- Pas de fonctions complexes
- Logique à gérer en Delphi

❌ **Debugging limité**
- Erreurs de template parfois cryptiques
- Pas de ligne d'erreur précise

❌ **Courbe d'apprentissage**
- Syntaxe spécifique à apprendre
- Documentation parfois limitée

### Solutions de contournement

**Pour la logique complexe :**
```pascal
// Faire les calculs en Delphi avant
TotalHT := CalculateTotalHT(Items);  
TotalTTC := TotalHT * 1.20;  

Stencil.Values['totalHT'] := FormatCurr('#,##0.00', TotalHT);  
Stencil.Values['totalTTC'] := FormatCurr('#,##0.00', TotalTTC);  
```

**Pour le debugging :**
```pascal
// Journaliser les données envoyées au template
{$IFDEF DEBUG}
WriteLn('Template data: ', DataJSON.ToString);
{$ENDIF}
```

## Conclusion

WebStencils apporte une **amélioration significative** au développement web côté serveur avec Delphi en offrant :

✅ **Séparation claire** entre présentation et logique  
✅ **Templates réutilisables** et maintenables  
✅ **Syntaxe simple** et intuitive  
✅ **Performance excellente** avec cache  
✅ **Collaboration facilitée** designers/développeurs  
✅ **Sécurité intégrée** (échappement HTML automatique)

**Points clés à retenir :**

1. **WebStencils simplifie** la génération de HTML dynamique
2. **Architecture MVC** facilitée avec séparation des responsabilités
3. **Cache essentiel** pour optimiser les performances en production
4. **Excellent pour** pages web serveur et emails HTML
5. **Complémentaire** avec JavaScript côté client

WebStencils représente une évolution naturelle du développement web avec Delphi, rendant le code plus propre, plus maintenable et plus professionnel. C'est un outil précieux dans la boîte à outils de tout développeur Delphi créant des applications web modernes.

Avec cette dernière section sur WebStencils, nous concluons notre exploration complète du développement d'applications web avec Delphi. Vous disposez maintenant de tous les outils et techniques pour créer des applications web professionnelles, performantes et modernes avec Delphi !

⏭️ [Tendances et futur de Delphi](/24-tendances-et-futur-de-delphi/README.md)
