# 10.1 Appels REST et API Web (TRESTClient)

## Introduction

Dans ce chapitre, nous allons découvrir comment interagir avec des API Web en utilisant le composant `TRESTClient` disponible dans de nombreux environnements de développement. Cette fonctionnalité est essentielle pour créer des applications modernes qui communiquent avec des services distants.

## Qu'est-ce que REST?

REST (Representational State Transfer) est un style d'architecture utilisé pour la conception d'applications en réseau. Une API REST est une interface qui permet à différentes applications de communiquer entre elles via le protocole HTTP.

### Concepts clés de REST:
- **Ressources**: Identifiées par des URLs
- **Méthodes HTTP**: GET, POST, PUT, DELETE, etc.
- **Sans état**: Chaque requête contient toutes les informations nécessaires
- **Format des données**: Généralement JSON ou XML

## Le composant TRESTClient

`TRESTClient` est un composant qui simplifie l'envoi et la réception de requêtes REST. Il offre une interface facile à utiliser pour communiquer avec des API Web sans avoir à gérer manuellement les détails de bas niveau des connexions HTTP.

### Configuration de base

Pour commencer à utiliser `TRESTClient`, suivez ces étapes:

1. Ajoutez le composant `TRESTClient` à votre projet
2. Configurez les propriétés de base:

```pascal
RESTClient1.BaseURL := 'https://api.exemple.com';
RESTClient1.Accept := 'application/json';
RESTClient1.ContentType := 'application/json';
```

## Effectuer des requêtes HTTP

### Requête GET (Récupérer des données)

La méthode GET permet de récupérer des données depuis une API:

```pascal
procedure TForm1.BtnGetDataClick(Sender: TObject);
var
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  // Création des composants
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration
    RESTRequest.Client := RESTClient1;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Resource := '/users';  // Le chemin après l'URL de base

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
      Memo1.Lines.Text := RESTResponse.Content
    else
      ShowMessage('Erreur: ' + IntToStr(RESTResponse.StatusCode));

  finally
    // Libération des ressources
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### Requête POST (Envoyer des données)

La méthode POST permet d'envoyer des données à l'API:

```pascal
procedure TForm1.BtnPostDataClick(Sender: TObject);
var
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JsonBody: TJSONObject;
begin
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  JsonBody := TJSONObject.Create;

  try
    // Préparation du corps JSON à envoyer
    JsonBody.AddPair('nom', 'Dupont');
    JsonBody.AddPair('prenom', 'Jean');
    JsonBody.AddPair('email', 'jean.dupont@exemple.com');

    // Configuration
    RESTRequest.Client := RESTClient1;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := '/users';
    RESTRequest.AddBody(JsonBody.ToString, ContentTypeFromString('application/json'));

    // Exécution
    RESTRequest.Execute;

    // Traitement de la réponse
    if (RESTResponse.StatusCode = 200) or (RESTResponse.StatusCode = 201) then
      ShowMessage('Utilisateur créé avec succès!')
    else
      ShowMessage('Erreur: ' + IntToStr(RESTResponse.StatusCode));

  finally
    JsonBody.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

## Gestion des paramètres

### Paramètres d'URL

Pour ajouter des paramètres à l'URL (par exemple, `/users?id=123`):

```pascal
RESTRequest.AddParameter('id', '123', pkGETorPOST);
```

### En-têtes personnalisés

Pour ajouter des en-têtes HTTP personnalisés:

```pascal
RESTRequest.AddParameter('Authorization', 'Bearer ' + MonToken, pkHTTPHEADER);
```

## Traitement des réponses JSON

La plupart des API modernes renvoient des données au format JSON. Voici comment les traiter:

```pascal
procedure TraiterReponseJSON(const JSONString: string);
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  i: Integer;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    if JSONValue is TJSONObject then
    begin
      // Traitement d'un objet JSON unique
      JSONObject := JSONValue as TJSONObject;
      ShowMessage('Nom: ' + JSONObject.GetValue<string>('nom'));
    end
    else if JSONValue is TJSONArray then
    begin
      // Traitement d'un tableau d'objets JSON
      JSONArray := JSONValue as TJSONArray;

      for i := 0 to JSONArray.Count - 1 do
      begin
        if JSONArray.Items[i] is TJSONObject then
        begin
          JSONObject := JSONArray.Items[i] as TJSONObject;
          Memo1.Lines.Add('Élément ' + IntToStr(i) + ': ' +
                          JSONObject.GetValue<string>('nom'));
        end;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

## Gestion des erreurs

Il est important de gérer correctement les erreurs lors des appels d'API:

```pascal
procedure ExecuterRequeteSecurisee(RESTRequest: TRESTRequest);
begin
  try
    RESTRequest.Execute;

    case RESTResponse.StatusCode of
      200..299: // Succès
        TraiterReponseSuccess(RESTResponse.Content);
      401:      // Non autorisé
        ShowMessage('Authentification requise');
      403:      // Interdit
        ShowMessage('Accès refusé');
      404:      // Non trouvé
        ShowMessage('Ressource non trouvée');
      500..599: // Erreur serveur
        ShowMessage('Erreur serveur: ' + RESTResponse.Content);
      else
        ShowMessage('Erreur non gérée: ' + IntToStr(RESTResponse.StatusCode));
    end;

  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  end;
end;
```

## Exemple complet: Connexion à une API météo

Voici un exemple complet utilisant une API publique de météo:

```pascal
procedure TForm1.BtnMeteoClick(Sender: TObject);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
  Ville: string;
begin
  Ville := EditVille.Text;
  if Ville = '' then
  begin
    ShowMessage('Veuillez saisir une ville');
    Exit;
  end;

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration
    RESTClient.BaseURL := 'https://api.openweathermap.org/data/2.5';
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Resource := '/weather';

    // Paramètres
    RESTRequest.AddParameter('q', Ville);
    RESTRequest.AddParameter('units', 'metric');
    RESTRequest.AddParameter('lang', 'fr');
    RESTRequest.AddParameter('appid', 'VOTRE_CLE_API');  // Remplacez par votre clé API

    // Exécution
    RESTRequest.Execute;

    // Traitement
    if RESTResponse.StatusCode = 200 then
    begin
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        LabelTemperature.Caption := 'Température: ' +
                                   JSONValue.GetValue<TJSONObject>('main')
                                           .GetValue<string>('temp') + '°C';
        LabelDescription.Caption := 'Conditions: ' +
                                   JSONValue.GetValue<TJSONArray>('weather')
                                           .Items[0].GetValue<string>('description');
      finally
        JSONValue.Free;
      end;
    end
    else
      ShowMessage('Erreur: ' + RESTResponse.Content);

  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

## Bonnes pratiques

1. **Libérez toujours les ressources** : Utilisez des blocs `try...finally` pour vous assurer que les objets sont libérés.
2. **Gérez les délais d'attente** : Configurez `RESTClient.Timeout` pour éviter les blocages.
3. **Vérifiez les codes de statut** : Ne supposez pas que la requête a réussi.
4. **Sécurisez vos clés API** : Ne codez pas en dur les clés API dans votre application.
5. **Utilisez les composants non-visuels** : Placez `TRESTClient`, `TRESTRequest` et `TRESTResponse` sur votre formulaire pour une réutilisation facile.

## Conclusion

`TRESTClient` est un outil puissant pour intégrer des API Web dans vos applications. En comprenant les principes de base des appels REST et en suivant les bonnes pratiques, vous pouvez facilement connecter vos applications à de nombreux services en ligne.

## Exercices pratiques

1. Créez une application qui affiche les derniers articles d'un blog en utilisant une API REST.
2. Modifiez l'exemple météo pour permettre d'afficher les prévisions sur plusieurs jours.
3. Créez un client pour une API de votre choix (par exemple, une API de films, de livres ou de musique).

N'oubliez pas que la documentation de l'API que vous utilisez est votre meilleure alliée. Consultez-la pour comprendre les points d'accès disponibles, les paramètres requis et les formats de réponse attendus.
