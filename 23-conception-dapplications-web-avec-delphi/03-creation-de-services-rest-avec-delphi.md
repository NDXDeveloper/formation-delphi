# 23.3 Création de services REST avec Delphi

## Introduction aux services REST

Les services REST (Representational State Transfer) sont devenus le standard pour la création d'API web modernes. Ils permettent à différentes applications de communiquer entre elles via Internet en utilisant des requêtes HTTP standard.

Dans cette section, nous allons découvrir comment créer des services REST avec Delphi, ce qui vous permettra de développer la partie serveur d'applications web ou mobiles, ou d'exposer des fonctionnalités de vos applications existantes à d'autres systèmes.

## Pourquoi utiliser des services REST ?

Avant de plonger dans le code, comprenons pourquoi les services REST sont si populaires :

- **Simplicité** : Utilise les méthodes HTTP standard (GET, POST, PUT, DELETE)
- **Sans état** : Chaque requête contient toutes les informations nécessaires
- **Uniformité** : Interface cohérente qui sépare le client du serveur
- **Compatibilité** : Fonctionne avec pratiquement tous les langages et plateformes
- **Performance** : Plus léger que SOAP ou d'autres protocoles complexes
- **Format flexible** : Généralement JSON ou XML, faciles à manipuler

## Principes fondamentaux des API REST

Une API REST bien conçue repose sur quelques principes clés :

1. **Ressources** : Tout est considéré comme une ressource (ex: utilisateurs, produits)
2. **URIs** : Chaque ressource est identifiée par une URI unique (ex: `/api/utilisateurs/123`)
3. **Méthodes HTTP** :
   - `GET` : Récupérer des données
   - `POST` : Créer des données
   - `PUT` : Mettre à jour des données existantes
   - `DELETE` : Supprimer des données
4. **Représentations** : Les ressources sont représentées en formats comme JSON ou XML
5. **Stateless** : Le serveur ne conserve pas l'état du client entre les requêtes

## Technologies Delphi pour les services REST

Delphi offre plusieurs approches pour créer des services REST :

1. **DataSnap REST** : Extension du framework DataSnap pour exposer des services REST
2. **RAD Server** : Solution complète pour créer et déployer des services REST (éditions professionnelles et supérieures)
3. **WebBroker** : Technologie plus ancienne mais puissante pour créer des applications web
4. **MARS-Curiosity** : Framework REST open-source pour Delphi
5. **Bibliothèques tierces** : Comme mORMot, XData, etc.

Pour ce tutoriel, nous allons nous concentrer sur DataSnap REST, qui est disponible dans toutes les éditions de Delphi et est relativement simple à utiliser.

## Création d'un service REST avec DataSnap

### Étape 1 : Créer un nouveau projet DataSnap REST

1. Ouvrez Delphi et sélectionnez **Fichier** > **Nouveau** > **Autres**
2. Naviguez vers **Delphi Projects** > **DataSnap** > **DataSnap REST Application**
3. Cliquez sur **Suivant**
4. Choisissez **Standalone Application** comme type de serveur
5. Laissez les autres options par défaut et cliquez sur **Suivant** et **Terminer**

Delphi va créer un projet avec plusieurs fichiers :
- Un fichier de formulaire pour le serveur (`ServerContainerUnit1.pas`)
- Une unité de méthodes serveur (`ServerMethodsUnit1.pas`)
- Un fichier de projet principal

### Étape 2 : Comprendre la structure du projet

Ouvrez `ServerMethodsUnit1.pas`. Vous y trouverez une classe `TServerMethods1` qui contiendra les méthodes de votre API REST.

```delphi
TServerMethods1 = class(TDSServerModule)
private
  { Private declarations }
public
  { Public declarations }
end;
```

Cette classe vide sera l'endroit où vous ajouterez vos méthodes d'API.

### Étape 3 : Ajouter une méthode REST simple

Ajoutons une méthode simple pour tester notre service :

```delphi
function TServerMethods1.Echo(Value: string): string;
begin
  Result := 'Echo: ' + Value;
end;
```

Ce code crée une méthode `Echo` qui retourne simplement la chaîne reçue précédée du texte 'Echo: '.

Pour l'exposer comme un point de terminaison REST, nous devons l'annoter avec des attributs :

```delphi
[TROServiceDescription]
function TServerMethods1.Echo(Value: string): string;
begin
  Result := 'Echo: ' + Value;
end;
```

L'attribut `[TROServiceDescription]` indique que cette méthode doit être exposée via REST.

### Étape 4 : Compiler et exécuter le service

1. Appuyez sur F9 pour compiler et exécuter le projet
2. Une fenêtre d'application serveur devrait apparaître
3. Votre service REST est maintenant en cours d'exécution, généralement sur `http://localhost:8080`

### Étape 5 : Tester le service

Vous pouvez tester votre service de plusieurs façons :

1. **Via un navigateur web** : Accédez à `http://localhost:8080/datasnap/rest/TServerMethods1/Echo/Bonjour`
2. **Via un outil comme Postman** : Créez une requête GET vers l'URL ci-dessus
3. **Via le client de test intégré** : DataSnap fournit une page de test accessible via `http://localhost:8080/datasnap/rest`

Vous devriez voir la réponse : `"Echo: Bonjour"`

## Création d'une API REST plus complète

Maintenant que nous comprenons les bases, créons une API plus complète pour gérer une liste de tâches (todo list).

### Étape 1 : Définir notre modèle de données

Ajoutez au début de l'unité `ServerMethodsUnit1.pas` :

```delphi
type
  TTodo = class
  private
    FId: Integer;
    FTitle: string;
    FCompleted: Boolean;
  published
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Completed: Boolean read FCompleted write FCompleted;
  end;
```

Cette classe représente une tâche avec un identifiant, un titre et un statut (terminée ou non).

### Étape 2 : Créer une liste pour stocker les tâches

Ajoutez ces champs et méthodes à la classe `TServerMethods1` :

```delphi
private
  FTodos: TObjectList<TTodo>;
  FNextId: Integer;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
```

Et implémentez le constructeur et le destructeur :

```delphi
constructor TServerMethods1.Create(AOwner: TComponent);
begin
  inherited;
  FTodos := TObjectList<TTodo>.Create(True); // True pour posséder les objets
  FNextId := 1;

  // Ajouter quelques tâches de test
  var Todo := TTodo.Create;
  Todo.Id := FNextId;
  Inc(FNextId);
  Todo.Title := 'Apprendre Delphi REST';
  Todo.Completed := False;
  FTodos.Add(Todo);

  Todo := TTodo.Create;
  Todo.Id := FNextId;
  Inc(FNextId);
  Todo.Title := 'Créer une API REST';
  Todo.Completed := False;
  FTodos.Add(Todo);
end;

destructor TServerMethods1.Destroy;
begin
  FTodos.Free;
  inherited;
end;
```

### Étape 3 : Ajouter les méthodes CRUD (Create, Read, Update, Delete)

Maintenant, implémentons les méthodes pour manipuler notre liste de tâches :

```delphi
[TROGet]
function TServerMethods1.GetAllTodos: TJSONArray;
var
  I: Integer;
  TodoObj: TJSONObject;
begin
  Result := TJSONArray.Create;

  for I := 0 to FTodos.Count - 1 do
  begin
    TodoObj := TJSONObject.Create;
    TodoObj.AddPair('id', TJSONNumber.Create(FTodos[I].Id));
    TodoObj.AddPair('title', FTodos[I].Title);
    TodoObj.AddPair('completed', TJSONBool.Create(FTodos[I].Completed));
    Result.AddElement(TodoObj);
  end;
end;

[TROGet]
function TServerMethods1.GetTodo(Id: Integer): TJSONObject;
var
  I: Integer;
  Todo: TTodo;
begin
  Result := nil;

  for I := 0 to FTodos.Count - 1 do
  begin
    if FTodos[I].Id = Id then
    begin
      Todo := FTodos[I];
      Result := TJSONObject.Create;
      Result.AddPair('id', TJSONNumber.Create(Todo.Id));
      Result.AddPair('title', Todo.Title);
      Result.AddPair('completed', TJSONBool.Create(Todo.Completed));
      Break;
    end;
  end;

  if Result = nil then
    raise ERESTException.Create(404, 'Tâche non trouvée');
end;

[TROPost]
function TServerMethods1.AddTodo(TodoData: TJSONObject): TJSONObject;
var
  Todo: TTodo;
begin
  Todo := TTodo.Create;
  try
    Todo.Id := FNextId;
    Inc(FNextId);
    Todo.Title := TodoData.GetValue<string>('title');
    Todo.Completed := TodoData.GetValue<Boolean>('completed', False);
    FTodos.Add(Todo);

    Result := TJSONObject.Create;
    Result.AddPair('id', TJSONNumber.Create(Todo.Id));
    Result.AddPair('title', Todo.Title);
    Result.AddPair('completed', TJSONBool.Create(Todo.Completed));
  except
    Todo.Free;
    raise;
  end;
end;

[TROPut]
function TServerMethods1.UpdateTodo(Id: Integer; TodoData: TJSONObject): TJSONObject;
var
  I: Integer;
  Todo: TTodo;
begin
  for I := 0 to FTodos.Count - 1 do
  begin
    if FTodos[I].Id = Id then
    begin
      Todo := FTodos[I];

      if TodoData.TryGetValue<string>('title', Todo.Title) then
        Todo.Title := TodoData.GetValue<string>('title');

      if TodoData.TryGetValue<Boolean>('completed', Todo.Completed) then
        Todo.Completed := TodoData.GetValue<Boolean>('completed');

      Result := TJSONObject.Create;
      Result.AddPair('id', TJSONNumber.Create(Todo.Id));
      Result.AddPair('title', Todo.Title);
      Result.AddPair('completed', TJSONBool.Create(Todo.Completed));
      Exit;
    end;
  end;

  raise ERESTException.Create(404, 'Tâche non trouvée');
end;

[TRODelete]
procedure TServerMethods1.DeleteTodo(Id: Integer);
var
  I: Integer;
begin
  for I := 0 to FTodos.Count - 1 do
  begin
    if FTodos[I].Id = Id then
    begin
      FTodos.Delete(I);
      Exit;
    end;
  end;

  raise ERESTException.Create(404, 'Tâche non trouvée');
end;
```

Ces méthodes utilisent les attributs `[TROGet]`, `[TROPost]`, `[TROPut]` et `[TRODelete]` pour spécifier les verbes HTTP correspondants.

### Étape 4 : Compiler et tester notre API complète

1. Appuyez sur F9 pour compiler et exécuter le service
2. Testez les différentes méthodes :
   - `GET http://localhost:8080/datasnap/rest/TServerMethods1/GetAllTodos` - Liste toutes les tâches
   - `GET http://localhost:8080/datasnap/rest/TServerMethods1/GetTodo/1` - Obtient la tâche avec Id=1
   - `POST http://localhost:8080/datasnap/rest/TServerMethods1/AddTodo` avec un corps JSON `{"title":"Nouvelle tâche","completed":false}` - Ajoute une tâche
   - `PUT http://localhost:8080/datasnap/rest/TServerMethods1/UpdateTodo/1` avec un corps JSON `{"completed":true}` - Marque la tâche 1 comme terminée
   - `DELETE http://localhost:8080/datasnap/rest/TServerMethods1/DeleteTodo/1` - Supprime la tâche avec Id=1

## Amélioration du service REST

### Configuration des chemins d'URL personnalisés

Les URL par défaut de DataSnap peuvent être assez verbeux. Vous pouvez personnaliser les chemins d'accès en utilisant l'attribut `[TROPath]` :

```delphi
[TROPath('/todos')]
[TROGet]
function TServerMethods1.GetAllTodos: TJSONArray;
// ... reste du code inchangé

[TROPath('/todos/{id}')]
[TROGet]
function TServerMethods1.GetTodo(Id: Integer): TJSONObject;
// ... reste du code inchangé
```

Avec cette configuration, vos endpoints seront plus propres :
- `GET /todos` - Liste toutes les tâches
- `GET /todos/1` - Obtient la tâche avec Id=1

### Gestion des erreurs et exceptions

Une bonne API REST doit gérer correctement les erreurs. DataSnap convertit automatiquement les exceptions en réponses HTTP appropriées, mais vous pouvez personnaliser davantage :

```delphi
try
  // Votre code
except
  on E: EDatabaseError do
    raise ERESTException.Create(500, 'Erreur de base de données: ' + E.Message);
  on E: Exception do
    raise ERESTException.Create(400, 'Erreur: ' + E.Message);
end;
```

### Validation des entrées

Ajoutez une validation pour éviter les données incorrectes :

```delphi
[TROPost]
function TServerMethods1.AddTodo(TodoData: TJSONObject): TJSONObject;
var
  Todo: TTodo;
  Title: string;
begin
  if not TodoData.TryGetValue<string>('title', Title) then
    raise ERESTException.Create(400, 'Le titre est obligatoire');

  if Title = '' then
    raise ERESTException.Create(400, 'Le titre ne peut pas être vide');

  // Suite du code...
end;
```

### Authentification et sécurité

Pour une API en production, vous voudriez ajouter une authentification. DataSnap prend en charge plusieurs options, dont l'authentification par jeton :

```delphi
// Dans ServerContainerUnit1.pas, pendant la création
DSServer1.AuthenticationManager := TDSAuthenticationManager.Create;
DSServer1.AuthenticationManager.OnUserAuthenticate := OnUserAuthenticate;
DSServer1.AuthenticationManager.OnUserAuthorize := OnUserAuthorize;

// Implémentation des gestionnaires d'authentification
function TServerContainer1.OnUserAuthenticate(UserName, Password: string): Boolean;
begin
  // Vérifiez les identifiants ici
  Result := (UserName = 'admin') and (Password = 'secret');
end;

function TServerContainer1.OnUserAuthorize(UserName: string; AuthorizeRoles: TStrings): Boolean;
begin
  // Vérifiez les autorisations ici
  Result := True;
end;
```

## Connexion à une base de données MySQL/MariaDB

Jusqu'à présent, nous avons stocké nos données en mémoire. Dans une application réelle, vous voudriez utiliser une base de données. Voici comment connecter notre service REST à MySQL/MariaDB :

### Étape 1 : Ajouter une connexion à la base de données

1. Ajoutez les unités nécessaires au début de `ServerMethodsUnit1.pas` :

```delphi
uses
  // ... autres unités
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;
```

2. Ajoutez les composants FireDAC à la classe `TServerMethods1` :

```delphi
private
  FDConnection1: TFDConnection;
  qryTodos: TFDQuery;
  // ... autres champs
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  // ... autres méthodes
```

3. Modifiez le constructeur et le destructeur pour initialiser la connexion :

```delphi
constructor TServerMethods1.Create(AOwner: TComponent);
begin
  inherited;

  FDConnection1 := TFDConnection.Create(Self);
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Values['Server'] := 'localhost';
  FDConnection1.Params.Values['Database'] := 'tododb';
  FDConnection1.Params.Values['User_Name'] := 'root';
  FDConnection1.Params.Values['Password'] := 'votremotdepasse';
  FDConnection1.LoginPrompt := False;

  try
    FDConnection1.Connected := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur de connexion à la base de données: ' + E.Message);
  end;

  qryTodos := TFDQuery.Create(Self);
  qryTodos.Connection := FDConnection1;
end;

destructor TServerMethods1.Destroy;
begin
  FDConnection1.Connected := False;
  inherited;
end;
```

### Étape 2 : Modifier les méthodes CRUD pour utiliser la base de données

```delphi
[TROGet]
function TServerMethods1.GetAllTodos: TJSONArray;
begin
  Result := TJSONArray.Create;

  qryTodos.SQL.Text := 'SELECT id, title, completed FROM todos';
  qryTodos.Open;

  while not qryTodos.Eof do
  begin
    var TodoObj := TJSONObject.Create;
    TodoObj.AddPair('id', TJSONNumber.Create(qryTodos.FieldByName('id').AsInteger));
    TodoObj.AddPair('title', qryTodos.FieldByName('title').AsString);
    TodoObj.AddPair('completed', TJSONBool.Create(qryTodos.FieldByName('completed').AsBoolean));
    Result.AddElement(TodoObj);

    qryTodos.Next;
  end;

  qryTodos.Close;
end;

// Implémentez les autres méthodes de manière similaire...
```

## Déploiement du service REST

Une fois votre service REST prêt, vous pouvez le déployer de différentes manières :

1. **Application Standalone** : Exécutée comme un service Windows ou une application
2. **ISAPI DLL** : Déployée sur IIS
3. **Apache Module** : Déployée sur un serveur Apache

Pour un déploiement en tant que service Windows :

1. Modifiez le projet pour qu'il s'exécute comme un service Windows
2. Utilisez un outil comme NSSM (Non-Sucking Service Manager) pour installer l'application en tant que service

## Documentation de l'API REST

Une bonne API REST doit être bien documentée. DataSnap génère automatiquement une documentation basique accessible via `/datasnap/rest`, mais vous pouvez l'améliorer :

1. Utilisez des commentaires détaillés dans votre code
2. Générez une documentation Swagger/OpenAPI
3. Créez une page HTML personnalisée décrivant votre API

## Consommation de l'API REST depuis une application client

Une fois votre API REST créée, vous pouvez la consommer depuis différents clients :

### Depuis une application Delphi :

```delphi
procedure TForm1.ButtonGetTodosClick(Sender: TObject);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONArray: TJSONArray;
  I: Integer;
begin
  RESTClient := TRESTClient.Create('http://localhost:8080');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'datasnap/rest/TServerMethods1/GetAllTodos';
    RESTRequest.Method := TRESTRequestMethod.rmGET;

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      JSONArray := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONArray;
      try
        Memo1.Clear;
        for I := 0 to JSONArray.Count - 1 do
        begin
          var TodoObj := JSONArray.Items[I] as TJSONObject;
          var Title := TodoObj.GetValue<string>('title');
          var Completed := TodoObj.GetValue<Boolean>('completed');

          Memo1.Lines.Add(Format('%s - %s', [
            Title,
            IfThen(Completed, 'Terminé', 'En cours')
          ]));
        end;
      finally
        JSONArray.Free;
      end;
    end
    else
      ShowMessage('Erreur: ' + RESTResponse.StatusText);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### Depuis JavaScript (Web ou mobile) :

```javascript
fetch('http://localhost:8080/datasnap/rest/TServerMethods1/GetAllTodos')
  .then(response => response.json())
  .then(data => {
    console.log('Tâches:', data);
    // Traiter les données...
  })
  .catch(error => {
    console.error('Erreur:', error);
  });
```

## Conclusion

Dans cette section, nous avons appris à créer des services REST avec Delphi en utilisant DataSnap. Nous avons vu comment :

- Créer un projet DataSnap REST de base
- Définir des endpoints REST avec différentes méthodes HTTP
- Manipuler des données JSON
- Connecter l'API à une base de données MySQL/MariaDB
- Sécuriser l'API avec une authentification
- Déployer et documenter l'API
- Consommer l'API depuis différents clients

Les services REST sont une partie essentielle du développement d'applications modernes, et Delphi offre des outils puissants pour les créer facilement.

## Exercices pratiques

1. Étendez l'API Todo pour ajouter une date d'échéance aux tâches
2. Ajoutez une fonctionnalité de filtrage pour obtenir uniquement les tâches terminées ou non terminées
3. Implémentez un système de pagination pour limiter le nombre de tâches retournées
4. Créez une application client Delphi simple qui consomme cette API
5. Ajoutez une authentification à l'API pour que seuls les utilisateurs autorisés puissent modifier les tâches

## Ressources supplémentaires

- Documentation officielle de DataSnap REST : [Embarcadero DocWiki](https://docwiki.embarcadero.com/RADStudio/en/DataSnap_REST)
- Tutoriels vidéo sur DataSnap REST : [Embarcadero YouTube](https://www.youtube.com/user/EmbarcaderoTechNet)
- Forums communautaires Delphi : [Embarcadero Forums](https://forums.embarcadero.com/)
