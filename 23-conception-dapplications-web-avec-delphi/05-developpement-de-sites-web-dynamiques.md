🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.5 Développement de sites Web dynamiques

## Introduction

Un site web **dynamique** est un site dont le contenu change en fonction des interactions de l'utilisateur, des données en base, de l'heure, ou de tout autre paramètre. Contrairement à un site **statique** où chaque page est un fichier HTML fixe, un site dynamique génère ses pages "à la volée".

**Analogie :**
- **Site statique** = Un livre imprimé : le contenu est figé
- **Site dynamique** = Un journal personnalisé qui s'adapte à chaque lecteur

Dans cette section, nous allons explorer comment créer des sites web dynamiques avec Delphi, en générant du contenu HTML personnalisé pour chaque utilisateur et chaque requête.

## Sites statiques vs sites dynamiques

### Site statique

```
Requête → Serveur Web → Fichier HTML → Navigateur
              ↓
         page1.html
         page2.html
         page3.html
```

**Caractéristiques :**
- Fichiers HTML pré-créés
- Contenu identique pour tous
- Pas de base de données nécessaire
- Rapide mais limité

### Site dynamique

```
Requête → Application Delphi → Génération HTML → Navigateur
              ↓
          Base de données
          Logique métier
          Templates
          Sessions utilisateur
```

**Caractéristiques :**
- Pages générées à la demande
- Contenu personnalisé
- Interaction avec base de données
- Évolutif et interactif

## Techniques de génération de contenu dynamique

### 1. Concaténation de chaînes (méthode basique)

La méthode la plus simple : construire le HTML avec des chaînes de caractères.

```pascal
procedure TWebModule1.ActionHomeAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  HTML: string;
  UserName: string;
begin
  // Récupérer le nom de l'utilisateur depuis la session
  UserName := GetSessionValue('username');
  if UserName = '' then
    UserName := 'Invité';

  // Construire la page HTML
  HTML := '<!DOCTYPE html>' + #13#10 +
          '<html>' + #13#10 +
          '<head>' + #13#10 +
          '  <title>Accueil</title>' + #13#10 +
          '  <meta charset="utf-8">' + #13#10 +
          '</head>' + #13#10 +
          '<body>' + #13#10 +
          '  <h1>Bienvenue, ' + UserName + ' !</h1>' + #13#10 +
          '  <p>Il est actuellement ' + TimeToStr(Now) + '</p>' + #13#10 +
          '</body>' + #13#10 +
          '</html>';

  Response.Content := HTML;
  Handled := True;
end;
```

**Avantages :**
- Simple et direct
- Aucune dépendance
- Total contrôle

**Inconvénients :**
- Difficile à maintenir
- Mélange code et présentation
- Erreurs de syntaxe HTML fréquentes
- Peu lisible

### 2. Utilisation de TStringBuilder (méthode améliorée)

Pour de meilleures performances avec beaucoup de contenu :

```pascal
procedure TWebModule1.GenerateClientList(Response: TWebResponse);
var
  Builder: TStringBuilder;
  Query: TFDQuery;
begin
  Builder := TStringBuilder.Create;
  try
    // En-tête HTML
    Builder.AppendLine('<!DOCTYPE html>');
    Builder.AppendLine('<html>');
    Builder.AppendLine('<head>');
    Builder.AppendLine('  <title>Liste des clients</title>');
    Builder.AppendLine('  <meta charset="utf-8">');
    Builder.AppendLine('  <style>');
    Builder.AppendLine('    table { border-collapse: collapse; width: 100%; }');
    Builder.AppendLine('    th, td { border: 1px solid #ddd; padding: 8px; }');
    Builder.AppendLine('    th { background-color: #4CAF50; color: white; }');
    Builder.AppendLine('  </style>');
    Builder.AppendLine('</head>');
    Builder.AppendLine('<body>');
    Builder.AppendLine('  <h1>Liste des clients</h1>');

    // Table des clients
    Builder.AppendLine('  <table>');
    Builder.AppendLine('    <tr>');
    Builder.AppendLine('      <th>ID</th>');
    Builder.AppendLine('      <th>Nom</th>');
    Builder.AppendLine('      <th>Prénom</th>');
    Builder.AppendLine('      <th>Email</th>');
    Builder.AppendLine('    </tr>');

    // Récupération des données
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients ORDER BY nom';
      Query.Open;

      while not Query.Eof do
      begin
        Builder.AppendLine('    <tr>');
        Builder.AppendFormat('      <td>%d</td>', [Query.FieldByName('id').AsInteger]);
        Builder.AppendLine;
        Builder.AppendFormat('      <td>%s</td>', [Query.FieldByName('nom').AsString]);
        Builder.AppendLine;
        Builder.AppendFormat('      <td>%s</td>', [Query.FieldByName('prenom').AsString]);
        Builder.AppendLine;
        Builder.AppendFormat('      <td>%s</td>', [Query.FieldByName('email').AsString]);
        Builder.AppendLine;
        Builder.AppendLine('    </tr>');
        Query.Next;
      end;
    finally
      Query.Free;
    end;

    Builder.AppendLine('  </table>');
    Builder.AppendLine('</body>');
    Builder.AppendLine('</html>');

    Response.Content := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

**Avantages sur la concaténation simple :**
- Meilleures performances
- Code plus lisible
- Gestion efficace de la mémoire

### 3. Fichiers templates (méthode recommandée)

Séparer le HTML (template) du code Delphi (logique).

**Structure du projet :**
```
MonProjet/
├── Source/
│   ├── WebModuleUnit.pas
│   └── ...
└── Templates/
    ├── header.html
    ├── footer.html
    ├── home.html
    └── clients.html
```

**Template HTML (clients.html) :**
```html
<!DOCTYPE html>
<html>
<head>
  <title>{{TITLE}}</title>
  <meta charset="utf-8">
  <style>
    table { border-collapse: collapse; width: 100%; }
    th, td { border: 1px solid #ddd; padding: 8px; }
    th { background-color: #4CAF50; color: white; }
  </style>
</head>
<body>
  <h1>{{PAGE_TITLE}}</h1>
  <p>{{MESSAGE}}</p>

  <table>
    <tr>
      <th>ID</th>
      <th>Nom</th>
      <th>Prénom</th>
      <th>Email</th>
    </tr>
    {{CLIENTS_ROWS}}
  </table>

  <p>Total : {{TOTAL_CLIENTS}} clients</p>
</body>
</html>
```

**Code Delphi :**
```pascal
unit TemplateEngine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TTemplateEngine = class
  private
    FTemplatePath: string;
  public
    constructor Create(const ATemplatePath: string);
    function LoadTemplate(const FileName: string): string;
    function ReplaceVariables(const Template: string;
      Variables: TDictionary<string, string>): string;
  end;

implementation

constructor TTemplateEngine.Create(const ATemplatePath: string);
begin
  FTemplatePath := ATemplatePath;
end;

function TTemplateEngine.LoadTemplate(const FileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
  FullPath: string;
begin
  FullPath := TPath.Combine(FTemplatePath, FileName);

  if not FileExists(FullPath) then
    raise Exception.CreateFmt('Template non trouvé : %s', [FullPath]);

  FileStream := TFileStream.Create(FullPath, fmOpenRead or fmShareDenyWrite);
  try
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TTemplateEngine.ReplaceVariables(const Template: string;
  Variables: TDictionary<string, string>): string;
var
  Pair: TPair<string, string>;
begin
  Result := Template;

  for Pair in Variables do
  begin
    Result := StringReplace(Result,
      '{{' + Pair.Key + '}}',
      Pair.Value,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

end.
```

**Utilisation dans WebModule :**
```pascal
uses
  TemplateEngine, System.Generics.Collections;

procedure TWebModule1.ActionClientsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Template: TTemplateEngine;
  HTML: string;
  Variables: TDictionary<string, string>;
  ClientRows: string;
  Query: TFDQuery;
begin
  Template := TTemplateEngine.Create('Templates');
  Variables := TDictionary<string, string>.Create;
  try
    // Charger le template
    HTML := Template.LoadTemplate('clients.html');

    // Générer les lignes de la table
    ClientRows := '';
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients';
      Query.Open;

      while not Query.Eof do
      begin
        ClientRows := ClientRows + '<tr>' +
          Format('<td>%d</td>', [Query.FieldByName('id').AsInteger]) +
          Format('<td>%s</td>', [Query.FieldByName('nom').AsString]) +
          Format('<td>%s</td>', [Query.FieldByName('prenom').AsString]) +
          Format('<td>%s</td>', [Query.FieldByName('email').AsString]) +
          '</tr>' + #13#10;
        Query.Next;
      end;

      // Préparer les variables
      Variables.Add('TITLE', 'Gestion des clients');
      Variables.Add('PAGE_TITLE', 'Liste des clients');
      Variables.Add('MESSAGE', 'Voici la liste complète de vos clients.');
      Variables.Add('CLIENTS_ROWS', ClientRows);
      Variables.Add('TOTAL_CLIENTS', IntToStr(Query.RecordCount));
    finally
      Query.Free;
    end;

    // Remplacer les variables
    HTML := Template.ReplaceVariables(HTML, Variables);

    Response.Content := HTML;
  finally
    Variables.Free;
    Template.Free;
  end;

  Handled := True;
end;
```

**Avantages de cette approche :**
- ✅ Séparation code/présentation
- ✅ HTML facilement modifiable par designers
- ✅ Réutilisation des templates
- ✅ Maintenance simplifiée
- ✅ Code Delphi plus propre

## Moteurs de templates avancés

### Mustache Templates

**Mustache** est un système de templates simple et "sans logique" très populaire.

**Installation :**
Via GetIt Package Manager : "Delphi MVC Framework" (inclut support Mustache)

**Template Mustache (clients.mustache) :**
```html
<!DOCTYPE html>
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  <h1>{{pageTitle}}</h1>

  <table>
    <tr>
      <th>Nom</th>
      <th>Email</th>
    </tr>
    {{#clients}}
    <tr>
      <td>{{nom}}</td>
      <td>{{email}}</td>
    </tr>
    {{/clients}}
  </table>

  {{^clients}}
    <p>Aucun client trouvé.</p>
  {{/clients}}
</body>
</html>
```

**Code Delphi avec Mustache :**
```pascal
uses
  SynMustache, SynCommons;

procedure TWebModule1.RenderMustacheTemplate;
var
  Template: string;
  Data: variant;
  HTML: RawUTF8;
begin
  // Charger le template
  Template := StringFromFile('templates/clients.mustache');

  // Préparer les données au format variant/JSON
  Data := _ObjFast([
    'title', 'Liste des clients',
    'pageTitle', 'Nos clients',
    'clients', _Arr([
      _ObjFast(['nom', 'Dupont', 'email', 'dupont@email.com']),
      _ObjFast(['nom', 'Martin', 'email', 'martin@email.com']),
      _ObjFast(['nom', 'Durand', 'email', 'durand@email.com'])
    ])
  ]);

  // Rendre le template
  HTML := TSynMustache.Parse(Template).Render(Data);

  Response.Content := UTF8ToString(HTML);
end;
```

### Template avec boucles et conditions

**Template avancé (dashboard.html) :**
```html
<!DOCTYPE html>
<html>
<head>
  <title>{{SITE_NAME}} - Tableau de bord</title>
  <style>
    .alert { padding: 10px; margin: 10px 0; border-radius: 4px; }
    .alert-info { background-color: #d1ecf1; color: #0c5460; }
    .alert-warning { background-color: #fff3cd; color: #856404; }
    .alert-danger { background-color: #f8d7da; color: #721c24; }
    .stats { display: flex; gap: 20px; }
    .stat-card {
      flex: 1;
      padding: 20px;
      background: #f8f9fa;
      border-radius: 8px;
    }
    .stat-number { font-size: 2em; font-weight: bold; color: #007bff; }
  </style>
</head>
<body>
  <h1>Tableau de bord - {{USER_NAME}}</h1>
  <p>Dernière connexion : {{LAST_LOGIN}}</p>

  <!-- Alertes conditionnelles -->
  {{ALERTS}}

  <!-- Statistiques -->
  <div class="stats">
    <div class="stat-card">
      <div class="stat-number">{{TOTAL_CLIENTS}}</div>
      <div>Clients actifs</div>
    </div>
    <div class="stat-card">
      <div class="stat-number">{{TOTAL_ORDERS}}</div>
      <div>Commandes du mois</div>
    </div>
    <div class="stat-card">
      <div class="stat-number">{{REVENUE}}</div>
      <div>Chiffre d'affaires</div>
    </div>
  </div>

  <!-- Liste des actions récentes -->
  <h2>Actions récentes</h2>
  {{RECENT_ACTIONS}}
</body>
</html>
```

**Code de génération :**
```pascal
procedure TWebModule1.GenerateDashboard(Response: TWebResponse);
var
  Template: TTemplateEngine;
  HTML: string;
  Variables: TDictionary<string, string>;
  Alerts, RecentActions: string;
begin
  Template := TTemplateEngine.Create('Templates');
  Variables := TDictionary<string, string>.Create;
  try
    HTML := Template.LoadTemplate('dashboard.html');

    // Générer les alertes conditionnelles
    Alerts := '';
    if HasPendingOrders then
      Alerts := Alerts + '<div class="alert alert-warning">' +
                '⚠️ Vous avez 5 commandes en attente de validation' +
                '</div>';

    if LowStock then
      Alerts := Alerts + '<div class="alert alert-danger">' +
                '❌ Alerte : Stock faible pour 3 produits' +
                '</div>';

    if Alerts = '' then
      Alerts := '<div class="alert alert-info">✅ Tout va bien !</div>';

    // Générer la liste des actions récentes
    RecentActions := GenerateRecentActionsList;

    // Remplir les variables
    Variables.Add('SITE_NAME', 'MonApp');
    Variables.Add('USER_NAME', GetCurrentUserName);
    Variables.Add('LAST_LOGIN', DateTimeToStr(GetLastLoginTime));
    Variables.Add('TOTAL_CLIENTS', IntToStr(GetClientCount));
    Variables.Add('TOTAL_ORDERS', IntToStr(GetOrderCount));
    Variables.Add('REVENUE', FormatCurr('#,##0.00 €', GetRevenue));
    Variables.Add('ALERTS', Alerts);
    Variables.Add('RECENT_ACTIONS', RecentActions);

    HTML := Template.ReplaceVariables(HTML, Variables);
    Response.Content := HTML;
  finally
    Variables.Free;
    Template.Free;
  end;
end;

function TWebModule1.GenerateRecentActionsList: string;
var
  Query: TFDQuery;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.AppendLine('<ul>');

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text :=
        'SELECT action, date_action FROM actions_log ' +
        'ORDER BY date_action DESC LIMIT 10';
      Query.Open;

      while not Query.Eof do
      begin
        Builder.AppendFormat('<li>%s - %s</li>',
          [Query.FieldByName('action').AsString,
           DateTimeToStr(Query.FieldByName('date_action').AsDateTime)]);
        Builder.AppendLine;
        Query.Next;
      end;
    finally
      Query.Free;
    end;

    Builder.AppendLine('</ul>');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

## Inclusion de templates (header/footer)

**Principe :** Réutiliser des parties communes sur toutes les pages.

**header.html :**
```html
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{{PAGE_TITLE}} - MonApp</title>
  <link rel="stylesheet" href="/css/style.css">
</head>
<body>
  <header>
    <nav>
      <a href="/">Accueil</a>
      <a href="/clients">Clients</a>
      <a href="/produits">Produits</a>
      <a href="/commandes">Commandes</a>
    </nav>
    <div class="user-info">
      Connecté en tant que : <strong>{{USER_NAME}}</strong>
      <a href="/logout">Déconnexion</a>
    </div>
  </header>
  <main>
```

**footer.html :**
```html
  </main>
  <footer>
    <p>&copy; 2025 MonApp - Tous droits réservés</p>
    <p>Version {{APP_VERSION}}</p>
  </footer>
  <script src="/js/app.js"></script>
</body>
</html>
```

**page-clients.html :**
```html
{{HEADER}}

<h1>Liste des clients</h1>
<p>{{MESSAGE}}</p>

<table>
  {{CLIENTS_TABLE}}
</table>

{{FOOTER}}
```

**Code d'assemblage :**
```pascal
procedure TWebModule1.RenderPageWithLayout(
  const PageTemplate: string;
  Variables: TDictionary<string, string>);
var
  Template: TTemplateEngine;
  Header, Footer, Content, HTML: string;
begin
  Template := TTemplateEngine.Create('Templates');
  try
    // Charger les composants
    Header := Template.LoadTemplate('header.html');
    Footer := Template.LoadTemplate('footer.html');
    Content := Template.LoadTemplate(PageTemplate);

    // Ajouter les variables de header/footer
    if not Variables.ContainsKey('APP_VERSION') then
      Variables.Add('APP_VERSION', '1.0.0');

    // Remplacer les variables dans les composants
    Header := Template.ReplaceVariables(Header, Variables);
    Footer := Template.ReplaceVariables(Footer, Variables);

    // Ajouter header et footer au contenu
    Variables.AddOrSetValue('HEADER', Header);
    Variables.AddOrSetValue('FOOTER', Footer);

    // Assembler la page complète
    HTML := Template.ReplaceVariables(Content, Variables);

    Response.Content := HTML;
  finally
    Template.Free;
  end;
end;

// Utilisation
procedure TWebModule1.ActionClientsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Variables: TDictionary<string, string>;
begin
  Variables := TDictionary<string, string>.Create;
  try
    Variables.Add('PAGE_TITLE', 'Clients');
    Variables.Add('USER_NAME', GetCurrentUser);
    Variables.Add('MESSAGE', 'Voici vos clients');
    Variables.Add('CLIENTS_TABLE', GenerateClientsTable);

    RenderPageWithLayout('page-clients.html', Variables);
  finally
    Variables.Free;
  end;

  Handled := True;
end;
```

## Gestion des formulaires dynamiques

### Affichage d'un formulaire

**form-client.html :**
```html
<h2>{{FORM_TITLE}}</h2>

{{ERROR_MESSAGE}}

<form action="/clients/save" method="POST">
  <input type="hidden" name="id" value="{{CLIENT_ID}}">

  <div class="form-group">
    <label for="nom">Nom :</label>
    <input type="text" id="nom" name="nom" value="{{NOM}}" required>
  </div>

  <div class="form-group">
    <label for="prenom">Prénom :</label>
    <input type="text" id="prenom" name="prenom" value="{{PRENOM}}" required>
  </div>

  <div class="form-group">
    <label for="email">Email :</label>
    <input type="email" id="email" name="email" value="{{EMAIL}}" required>
  </div>

  <div class="form-group">
    <label for="telephone">Téléphone :</label>
    <input type="tel" id="telephone" name="telephone" value="{{TELEPHONE}}">
  </div>

  <button type="submit">{{BUTTON_TEXT}}</button>
  <a href="/clients">Annuler</a>
</form>
```

### Traitement du formulaire

```pascal
procedure TWebModule1.ActionClientEditAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Variables: TDictionary<string, string>;
  ClientID: Integer;
  Query: TFDQuery;
begin
  Variables := TDictionary<string, string>.Create;
  try
    // Déterminer si c'est un nouveau client ou une modification
    ClientID := StrToIntDef(Request.QueryFields.Values['id'], 0);

    if ClientID > 0 then
    begin
      // Modification : charger les données existantes
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := FDConnection1;
        Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
        Query.ParamByName('id').AsInteger := ClientID;
        Query.Open;

        if not Query.IsEmpty then
        begin
          Variables.Add('FORM_TITLE', 'Modifier un client');
          Variables.Add('CLIENT_ID', IntToStr(ClientID));
          Variables.Add('NOM', Query.FieldByName('nom').AsString);
          Variables.Add('PRENOM', Query.FieldByName('prenom').AsString);
          Variables.Add('EMAIL', Query.FieldByName('email').AsString);
          Variables.Add('TELEPHONE', Query.FieldByName('telephone').AsString);
          Variables.Add('BUTTON_TEXT', 'Modifier');
        end;
      finally
        Query.Free;
      end;
    end
    else
    begin
      // Nouveau client : formulaire vide
      Variables.Add('FORM_TITLE', 'Nouveau client');
      Variables.Add('CLIENT_ID', '0');
      Variables.Add('NOM', '');
      Variables.Add('PRENOM', '');
      Variables.Add('EMAIL', '');
      Variables.Add('TELEPHONE', '');
      Variables.Add('BUTTON_TEXT', 'Créer');
    end;

    Variables.Add('ERROR_MESSAGE', '');
    Variables.Add('PAGE_TITLE', 'Gestion client');
    Variables.Add('USER_NAME', GetCurrentUser);

    RenderPageWithLayout('form-client.html', Variables);
  finally
    Variables.Free;
  end;

  Handled := True;
end;

procedure TWebModule1.ActionClientSaveAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  ClientID: Integer;
  Nom, Prenom, Email, Telephone: string;
  Query: TFDQuery;
  Variables: TDictionary<string, string>;
begin
  // Récupérer les données du formulaire
  ClientID := StrToIntDef(Request.ContentFields.Values['id'], 0);
  Nom := Trim(Request.ContentFields.Values['nom']);
  Prenom := Trim(Request.ContentFields.Values['prenom']);
  Email := Trim(Request.ContentFields.Values['email']);
  Telephone := Trim(Request.ContentFields.Values['telephone']);

  // Validation
  if (Nom = '') or (Prenom = '') or (Email = '') then
  begin
    // Erreur : ré-afficher le formulaire avec message
    Variables := TDictionary<string, string>.Create;
    try
      Variables.Add('ERROR_MESSAGE',
        '<div class="alert alert-danger">Veuillez remplir tous les champs obligatoires.</div>');
      Variables.Add('FORM_TITLE', 'Modifier un client');
      Variables.Add('CLIENT_ID', IntToStr(ClientID));
      Variables.Add('NOM', Nom);
      Variables.Add('PRENOM', Prenom);
      Variables.Add('EMAIL', Email);
      Variables.Add('TELEPHONE', Telephone);
      Variables.Add('BUTTON_TEXT', 'Modifier');
      Variables.Add('PAGE_TITLE', 'Gestion client');
      Variables.Add('USER_NAME', GetCurrentUser);

      RenderPageWithLayout('form-client.html', Variables);
    finally
      Variables.Free;
    end;

    Handled := True;
    Exit;
  end;

  // Enregistrement
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    if ClientID > 0 then
    begin
      // Modification
      Query.SQL.Text :=
        'UPDATE clients SET nom = :nom, prenom = :prenom, ' +
        'email = :email, telephone = :telephone WHERE id = :id';
      Query.ParamByName('id').AsInteger := ClientID;
    end
    else
    begin
      // Création
      Query.SQL.Text :=
        'INSERT INTO clients (nom, prenom, email, telephone) ' +
        'VALUES (:nom, :prenom, :email, :telephone)';
    end;

    Query.ParamByName('nom').AsString := Nom;
    Query.ParamByName('prenom').AsString := Prenom;
    Query.ParamByName('email').AsString := Email;
    Query.ParamByName('telephone').AsString := Telephone;
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  // Redirection vers la liste
  Response.StatusCode := 302; // Redirect
  Response.Location := '/clients';
  Response.Content := '<html><body>Redirection...</body></html>';

  Handled := True;
end;
```

## Pagination des résultats

```pascal
procedure TWebModule1.ActionClientsWithPagination(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  Variables: TDictionary<string, string>;
  Page, PageSize, TotalRecords, TotalPages: Integer;
  Offset: Integer;
  PaginationHTML: string;
  i: Integer;
begin
  // Paramètres de pagination
  Page := StrToIntDef(Request.QueryFields.Values['page'], 1);
  PageSize := 20; // 20 clients par page
  Offset := (Page - 1) * PageSize;

  Variables := TDictionary<string, string>.Create;
  try
    // Compter le nombre total d'enregistrements
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'SELECT COUNT(*) as total FROM clients';
      Query.Open;
      TotalRecords := Query.FieldByName('total').AsInteger;
      TotalPages := Ceil(TotalRecords / PageSize);

      // Récupérer la page demandée
      Query.Close;
      Query.SQL.Text :=
        'SELECT * FROM clients ' +
        'ORDER BY nom ' +
        'LIMIT :pagesize OFFSET :offset';
      Query.ParamByName('pagesize').AsInteger := PageSize;
      Query.ParamByName('offset').AsInteger := Offset;
      Query.Open;

      // Générer la table
      Variables.Add('CLIENTS_TABLE', GenerateClientsTableFromQuery(Query));

      // Générer la pagination
      PaginationHTML := '<div class="pagination">';

      // Bouton "Précédent"
      if Page > 1 then
        PaginationHTML := PaginationHTML +
          Format('<a href="/clients?page=%d">« Précédent</a> ', [Page - 1])
      else
        PaginationHTML := PaginationHTML + '<span class="disabled">« Précédent</span> ';

      // Numéros de pages
      for i := 1 to TotalPages do
      begin
        if i = Page then
          PaginationHTML := PaginationHTML +
            Format('<span class="current">%d</span> ', [i])
        else
          PaginationHTML := PaginationHTML +
            Format('<a href="/clients?page=%d">%d</a> ', [i, i]);
      end;

      // Bouton "Suivant"
      if Page < TotalPages then
        PaginationHTML := PaginationHTML +
          Format('<a href="/clients?page=%d">Suivant »</a>', [Page + 1])
      else
        PaginationHTML := PaginationHTML + '<span class="disabled">Suivant »</span>';

      PaginationHTML := PaginationHTML + '</div>';

      Variables.Add('PAGINATION', PaginationHTML);
      Variables.Add('TOTAL_RECORDS', IntToStr(TotalRecords));
      Variables.Add('CURRENT_PAGE', IntToStr(Page));
      Variables.Add('TOTAL_PAGES', IntToStr(TotalPages));
    finally
      Query.Free;
    end;

    Variables.Add('PAGE_TITLE', 'Clients');
    Variables.Add('USER_NAME', GetCurrentUser);

    RenderPageWithLayout('page-clients-paginated.html', Variables);
  finally
    Variables.Free;
  end;

  Handled := True;
end;
```

## Filtrage et recherche dynamique

**Formulaire de recherche :**
```html
<form action="/clients/search" method="GET">
  <div class="search-bar">
    <input type="text" name="q" placeholder="Rechercher un client..."
           value="{{SEARCH_QUERY}}">

    <select name="filter">
      <option value="all" {{FILTER_ALL}}>Tous</option>
      <option value="active" {{FILTER_ACTIVE}}>Actifs</option>
      <option value="inactive" {{FILTER_INACTIVE}}>Inactifs</option>
    </select>

    <button type="submit">🔍 Rechercher</button>
    <a href="/clients">Effacer</a>
  </div>
</form>

<p>{{RESULTS_COUNT}} résultat(s) trouvé(s)</p>

{{CLIENTS_TABLE}}
```

**Code de recherche :**
```pascal
procedure TWebModule1.ActionClientSearch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  Variables: TDictionary<string, string>;
  SearchQuery, FilterType: string;
  SQL: string;
begin
  SearchQuery := Trim(Request.QueryFields.Values['q']);
  FilterType := Request.QueryFields.Values['filter'];
  if FilterType = '' then
    FilterType := 'all';

  Variables := TDictionary<string, string>.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;

      // Construction de la requête SQL dynamique
      SQL := 'SELECT * FROM clients WHERE 1=1';

      // Filtre de recherche textuelle
      if SearchQuery <> '' then
      begin
        SQL := SQL + ' AND (nom LIKE :search OR prenom LIKE :search OR email LIKE :search)';
      end;

      // Filtre de statut
      case IndexStr(FilterType, ['all', 'active', 'inactive']) of
        1: SQL := SQL + ' AND actif = 1';
        2: SQL := SQL + ' AND actif = 0';
      end;

      SQL := SQL + ' ORDER BY nom';

      Query.SQL.Text := SQL;

      if SearchQuery <> '' then
        Query.ParamByName('search').AsString := '%' + SearchQuery + '%';

      Query.Open;

      // Générer le tableau des résultats
      Variables.Add('CLIENTS_TABLE', GenerateClientsTableFromQuery(Query));
      Variables.Add('RESULTS_COUNT', IntToStr(Query.RecordCount));
      Variables.Add('SEARCH_QUERY', SearchQuery);

      // Marquer le filtre sélectionné
      Variables.Add('FILTER_ALL', IfThen(FilterType = 'all', 'selected', ''));
      Variables.Add('FILTER_ACTIVE', IfThen(FilterType = 'active', 'selected', ''));
      Variables.Add('FILTER_INACTIVE', IfThen(FilterType = 'inactive', 'selected', ''));
    finally
      Query.Free;
    end;

    Variables.Add('PAGE_TITLE', 'Recherche clients');
    Variables.Add('USER_NAME', GetCurrentUser);

    RenderPageWithLayout('page-clients-search.html', Variables);
  finally
    Variables.Free;
  end;

  Handled := True;
end;
```

## Mise en cache des templates

Pour améliorer les performances, mettre en cache les templates chargés :

```pascal
type
  TTemplateCache = class
  private
    FCache: TDictionary<string, string>;
    FCacheEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTemplate(const FileName: string): string;
    procedure Clear;
    property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
  end;

implementation

constructor TTemplateCache.Create;
begin
  FCache := TDictionary<string, string>.Create;
  FCacheEnabled := True;
end;

destructor TTemplateCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

function TTemplateCache.GetTemplate(const FileName: string): string;
var
  FullPath: string;
begin
  FullPath := TPath.Combine('Templates', FileName);

  // Vérifier le cache
  if FCacheEnabled and FCache.ContainsKey(FullPath) then
  begin
    Result := FCache[FullPath];
    Exit;
  end;

  // Charger depuis le disque
  Result := TFile.ReadAllText(FullPath, TEncoding.UTF8);

  // Ajouter au cache
  if FCacheEnabled then
    FCache.AddOrSetValue(FullPath, Result);
end;

procedure TTemplateCache.Clear;
begin
  FCache.Clear;
end;

// Variable globale
var
  TemplateCache: TTemplateCache;

initialization
  TemplateCache := TTemplateCache.Create;
  {$IFDEF DEBUG}
  TemplateCache.CacheEnabled := False; // Désactiver en développement
  {$ENDIF}

finalization
  TemplateCache.Free;
```

## Bonnes pratiques

### 1. Échapper les caractères HTML

**Toujours échapper les données utilisateur pour éviter XSS :**

```pascal
function HTMLEncode(const Text: string): string;
begin
  Result := Text;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

// Utilisation
Variables.Add('USER_INPUT', HTMLEncode(Request.ContentFields.Values['comment']));
```

### 2. Séparer les responsabilités

```
Controllers/     → Gestion des routes et requêtes
Models/          → Accès aux données
Views/Templates/ → Présentation HTML
Services/        → Logique métier
```

### 3. Utiliser des helpers

```pascal
type
  THTMLHelper = class
  public
    class function FormatDate(const ADate: TDateTime): string;
    class function FormatCurrency(const Amount: Currency): string;
    class function FormatBoolean(const Value: Boolean): string;
  end;

class function THTMLHelper.FormatDate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('dd/mm/yyyy', ADate);
end;

class function THTMLHelper.FormatCurrency(const Amount: Currency): string;
begin
  Result := FormatCurr('#,##0.00 €', Amount);
end;

class function THTMLHelper.FormatBoolean(const Value: Boolean): string;
begin
  if Value then
    Result := '<span class="badge badge-success">✓ Oui</span>'
  else
    Result := '<span class="badge badge-secondary">✗ Non</span>';
end;
```

### 4. Gérer les erreurs élégamment

**error.html :**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Erreur - {{ERROR_CODE}}</title>
  <style>
    .error-container {
      max-width: 600px;
      margin: 100px auto;
      text-align: center;
    }
    .error-code {
      font-size: 4em;
      color: #dc3545;
    }
  </style>
</head>
<body>
  <div class="error-container">
    <div class="error-code">{{ERROR_CODE}}</div>
    <h1>{{ERROR_TITLE}}</h1>
    <p>{{ERROR_MESSAGE}}</p>
    <a href="/">Retour à l'accueil</a>
  </div>
</body>
</html>
```

```pascal
procedure TWebModule1.ShowError(Response: TWebResponse;
  ErrorCode: Integer; const Title, Message: string);
var
  Variables: TDictionary<string, string>;
  Template: TTemplateEngine;
  HTML: string;
begin
  Variables := TDictionary<string, string>.Create;
  Template := TTemplateEngine.Create('Templates');
  try
    Variables.Add('ERROR_CODE', IntToStr(ErrorCode));
    Variables.Add('ERROR_TITLE', Title);
    Variables.Add('ERROR_MESSAGE', Message);

    HTML := Template.LoadTemplate('error.html');
    HTML := Template.ReplaceVariables(HTML, Variables);

    Response.StatusCode := ErrorCode;
    Response.Content := HTML;
  finally
    Template.Free;
    Variables.Free;
  end;
end;

// Utilisation
try
  // Code qui peut échouer
  ProcessRequest;
except
  on E: Exception do
    ShowError(Response, 500, 'Erreur serveur', E.Message);
end;
```

## Conclusion

Le développement de sites web dynamiques avec Delphi offre une grande flexibilité et de nombreuses possibilités. Les points clés à retenir :

✅ **Séparation code/présentation** - Utiliser des templates
✅ **Réutilisation** - Headers, footers, composants partagés
✅ **Performance** - Mise en cache des templates
✅ **Sécurité** - Échapper les données utilisateur
✅ **Maintenabilité** - Organisation claire du code
✅ **Expérience utilisateur** - Pages adaptées et réactives

Les techniques présentées dans cette section vous permettent de créer des sites web professionnels et dynamiques avec Delphi, en capitalisant sur la puissance du langage Object Pascal et l'accès natif aux bases de données via FireDAC.

Que vous utilisiez WebBroker, IntraWeb, Horse ou tout autre framework Delphi, les principes de génération de contenu dynamique et de templating restent les mêmes et constituent la base de toute application web moderne.

Dans la section suivante, nous explorerons l'intégration avec des frameworks JavaScript modernes, permettant de créer des interfaces utilisateur encore plus riches et interactives.

⏭️ [Intégration avec des frameworks JavaScript](/23-conception-dapplications-web-avec-delphi/06-integration-avec-des-frameworks-javascript.md)
