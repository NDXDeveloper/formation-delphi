# 10.2 Manipulation de JSON et XML

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Lorsque vous travaillez avec des API Web et des échanges de données entre applications, deux formats sont particulièrement répandus : JSON et XML. Dans ce chapitre, nous allons apprendre à manipuler ces formats en programmation, des bases jusqu'aux opérations plus avancées.

## JSON : JavaScript Object Notation

### Qu'est-ce que JSON ?

JSON (JavaScript Object Notation) est un format léger d'échange de données, facile à lire pour les humains et simple à analyser pour les machines. Il est basé sur un sous-ensemble du langage JavaScript.

### Structure de base de JSON

Un document JSON est constitué de deux structures principales :
- Une collection de paires nom/valeur (similaire à un objet ou un dictionnaire)
- Une liste ordonnée de valeurs (similaire à un tableau ou une liste)

Exemple de document JSON simple :

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 32,
  "estActif": true,
  "adresse": {
    "rue": "123 Avenue des Champs",
    "ville": "Paris",
    "codePostal": "75008"
  },
  "telephones": [
    "+33123456789",
    "+33987654321"
  ]
}
```

### Lecture de JSON

Pour lire un document JSON, nous utilisons généralement la classe `TJSONObject` :

```pascal
procedure LireJSON(const JSONString: string);
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Nom, Prenom: string;
  Age: Integer;
  EstActif: Boolean;
begin
  // Analyser la chaîne JSON
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    // Vérifier si c'est un objet JSON valide
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire des valeurs simples
      Nom := JSONObject.GetValue<string>('nom');
      Prenom := JSONObject.GetValue<string>('prenom');
      Age := JSONObject.GetValue<Integer>('age');
      EstActif := JSONObject.GetValue<Boolean>('estActif');

      // Afficher les valeurs
      ShowMessage(Format('Personne: %s %s, %d ans, Actif: %s',
                         [Prenom, Nom, Age, BoolToStr(EstActif, True)]));
    end;
  finally
    // Toujours libérer la mémoire
    JSONValue.Free;
  end;
end;
```

### Accès aux objets imbriqués

Pour accéder à des objets imbriqués, nous pouvons chaîner les appels :

```pascal
procedure LireAdresse(const JSONString: string);
var
  JSONValue: TJSONValue;
  JSONObject, AdresseObj: TJSONObject;
  Rue, Ville, CodePostal: string;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Accéder à l'objet imbriqué
      if JSONObject.TryGetValue<TJSONObject>('adresse', AdresseObj) then
      begin
        // Extraire les valeurs de l'adresse
        Rue := AdresseObj.GetValue<string>('rue');
        Ville := AdresseObj.GetValue<string>('ville');
        CodePostal := AdresseObj.GetValue<string>('codePostal');

        ShowMessage(Format('Adresse: %s, %s %s', [Rue, CodePostal, Ville]));
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Parcourir des tableaux JSON

Pour traiter les tableaux dans un document JSON :

```pascal
procedure LireTelephones(const JSONString: string);
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  TelArray: TJSONArray;
  i: Integer;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Accéder au tableau
      if JSONObject.TryGetValue<TJSONArray>('telephones', TelArray) then
      begin
        Memo1.Lines.Add('Numéros de téléphone:');

        // Parcourir le tableau
        for i := 0 to TelArray.Count - 1 do
        begin
          Memo1.Lines.Add(Format('  %d: %s', [i+1, TelArray.Items[i].Value]));
        end;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Création de JSON

Pour créer un document JSON :

```pascal
function CreerJSON: string;
var
  JSONObject, AdresseObj: TJSONObject;
  TelArray: TJSONArray;
begin
  // Créer l'objet principal
  JSONObject := TJSONObject.Create;

  try
    // Ajouter des valeurs simples
    JSONObject.AddPair('nom', 'Martin');
    JSONObject.AddPair('prenom', 'Sophie');
    JSONObject.AddPair('age', TJSONNumber.Create(28));
    JSONObject.AddPair('estActif', TJSONBool.Create(True));

    // Créer et ajouter un objet imbriqué
    AdresseObj := TJSONObject.Create;
    AdresseObj.AddPair('rue', '45 Rue du Commerce');
    AdresseObj.AddPair('ville', 'Lyon');
    AdresseObj.AddPair('codePostal', '69002');

    JSONObject.AddPair('adresse', AdresseObj);

    // Créer et ajouter un tableau
    TelArray := TJSONArray.Create;
    TelArray.Add('+33611223344');
    TelArray.Add('+33655667788');

    JSONObject.AddPair('telephones', TelArray);

    // Convertir en chaîne
    Result := JSONObject.ToString;
  finally
    // Libérer la mémoire
    // Note: AdresseObj et TelArray sont libérés automatiquement par JSONObject
    JSONObject.Free;
  end;
end;
```

### Modification de JSON

Pour modifier un document JSON existant :

```pascal
function ModifierJSON(const JSONString: string): string;
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  AdresseObj: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Modifier des valeurs existantes
      JSONObject.RemovePair('age').Free;
      JSONObject.AddPair('age', TJSONNumber.Create(33));

      // Modifier un objet imbriqué
      if JSONObject.TryGetValue<TJSONObject>('adresse', AdresseObj) then
      begin
        AdresseObj.RemovePair('ville').Free;
        AdresseObj.AddPair('ville', 'Marseille');
      end;

      // Ajouter une nouvelle paire
      JSONObject.AddPair('profession', 'Développeur');

      Result := JSONObject.ToString;
    end
    else
      Result := '';
  finally
    JSONValue.Free;
  end;
end;
```

## XML : eXtensible Markup Language

### Qu'est-ce que XML ?

XML (eXtensible Markup Language) est un format de données plus ancien que JSON mais toujours très utilisé, notamment dans les services web SOAP et les documents structurés. Il est plus verbeux que JSON mais offre une structure plus riche.

### Structure de base de XML

Un document XML est composé d'éléments, d'attributs et de valeurs :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<personne id="123">
  <nom>Dupont</nom>
  <prenom>Jean</prenom>
  <age>32</age>
  <estActif>true</estActif>
  <adresse>
    <rue>123 Avenue des Champs</rue>
    <ville>Paris</ville>
    <codePostal>75008</codePostal>
  </adresse>
  <telephones>
    <telephone>+33123456789</telephone>
    <telephone>+33987654321</telephone>
  </telephones>
</personne>
```

### Lecture de XML avec DOM

Pour lire un document XML, nous pouvons utiliser l'approche DOM (Document Object Model) :

```pascal
procedure LireXML(const XMLString: string);
var
  XMLDoc: IXMLDocument;
  RootNode, AdresseNode: IXMLNode;
  Nom, Prenom, Ville: string;
begin
  // Créer le document XML
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    // Charger le XML
    XMLDoc.LoadFromXML(XMLString);

    // Accéder au nœud racine
    RootNode := XMLDoc.DocumentElement;

    // Lire des valeurs simples
    Nom := RootNode.ChildNodes['nom'].Text;
    Prenom := RootNode.ChildNodes['prenom'].Text;

    // Accéder à un nœud imbriqué
    AdresseNode := RootNode.ChildNodes['adresse'];
    Ville := AdresseNode.ChildNodes['ville'].Text;

    ShowMessage(Format('%s %s habite à %s', [Prenom, Nom, Ville]));

    // Lire un attribut
    ShowMessage('ID: ' + RootNode.Attributes['id']);
  except
    on E: Exception do
      ShowMessage('Erreur lors de la lecture XML: ' + E.Message);
  end;
end;
```

### Parcourir des nœuds XML

Pour parcourir une liste de nœuds XML :

```pascal
procedure LireTelephonesXML(const XMLString: string);
var
  XMLDoc: IXMLDocument;
  RootNode, TelephonesNode, TelNode: IXMLNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    XMLDoc.LoadFromXML(XMLString);
    RootNode := XMLDoc.DocumentElement;

    // Accéder au nœud contenant la liste
    TelephonesNode := RootNode.ChildNodes['telephones'];

    Memo1.Lines.Add('Numéros de téléphone:');

    // Parcourir les nœuds enfants
    for i := 0 to TelephonesNode.ChildNodes.Count - 1 do
    begin
      TelNode := TelephonesNode.ChildNodes[i];
      Memo1.Lines.Add(Format('  %d: %s', [i+1, TelNode.Text]));
    end;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

### Création de XML

Pour créer un document XML :

```pascal
function CreerXML: string;
var
  XMLDoc: IXMLDocument;
  RootNode, AdresseNode, TelephonesNode, TelNode: IXMLNode;
begin
  // Créer le document XML
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    // Créer le nœud racine
    RootNode := XMLDoc.AddChild('personne');
    RootNode.Attributes['id'] := '456';

    // Ajouter des éléments simples
    RootNode.AddChild('nom').Text := 'Martin';
    RootNode.AddChild('prenom').Text := 'Sophie';
    RootNode.AddChild('age').Text := '28';
    RootNode.AddChild('estActif').Text := 'true';

    // Créer un nœud imbriqué
    AdresseNode := RootNode.AddChild('adresse');
    AdresseNode.AddChild('rue').Text := '45 Rue du Commerce';
    AdresseNode.AddChild('ville').Text := 'Lyon';
    AdresseNode.AddChild('codePostal').Text := '69002';

    // Créer une liste de nœuds
    TelephonesNode := RootNode.AddChild('telephones');

    TelNode := TelephonesNode.AddChild('telephone');
    TelNode.Text := '+33611223344';

    TelNode := TelephonesNode.AddChild('telephone');
    TelNode.Text := '+33655667788';

    // Convertir en chaîne
    XMLDoc.SaveToXML(Result);
  finally
    // Pas besoin de libérer la mémoire grâce à l'interface IXMLDocument
  end;
end;
```

### Modification de XML

Pour modifier un document XML existant :

```pascal
function ModifierXML(const XMLString: string): string;
var
  XMLDoc: IXMLDocument;
  RootNode, AdresseNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    // Charger le XML
    XMLDoc.LoadFromXML(XMLString);
    RootNode := XMLDoc.DocumentElement;

    // Modifier des valeurs existantes
    RootNode.ChildNodes['age'].Text := '33';

    // Modifier un nœud imbriqué
    AdresseNode := RootNode.ChildNodes['adresse'];
    AdresseNode.ChildNodes['ville'].Text := 'Marseille';

    // Ajouter un nouveau nœud
    RootNode.AddChild('profession').Text := 'Développeur';

    // Convertir en chaîne
    XMLDoc.SaveToXML(Result);
  except
    on E: Exception do
    begin
      Result := '';
      ShowMessage('Erreur lors de la modification XML: ' + E.Message);
    end;
  end;
end;
```

## Conversion entre JSON et XML

Dans certains cas, vous pourriez avoir besoin de convertir entre JSON et XML. Voici des exemples simples pour effectuer ces conversions :

### JSON vers XML

```pascal
function JSONversXML(const JSONString: string): string;
var
  JSONValue: TJSONValue;
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;

  // Fonction récursive pour convertir un objet JSON en XML
  procedure JSONObjectToXML(JSONObj: TJSONObject; ParentNode: IXMLNode);
  var
    i: Integer;
    Pair: TJSONPair;
  begin
    for i := 0 to JSONObj.Count - 1 do
    begin
      Pair := JSONObj.Pairs[i];

      if Pair.JsonValue is TJSONObject then
      begin
        // Traiter un objet imbriqué
        JSONObjectToXML(Pair.JsonValue as TJSONObject,
                        ParentNode.AddChild(Pair.JsonString.Value));
      end
      else if Pair.JsonValue is TJSONArray then
      begin
        // Traiter un tableau
        JSONArrayToXML(Pair.JsonValue as TJSONArray,
                      ParentNode.AddChild(Pair.JsonString.Value));
      end
      else
      begin
        // Traiter une valeur simple
        ParentNode.AddChild(Pair.JsonString.Value).Text := Pair.JsonValue.Value;
      end;
    end;
  end;

  // Fonction récursive pour convertir un tableau JSON en XML
  procedure JSONArrayToXML(JSONArr: TJSONArray; ParentNode: IXMLNode);
  var
    i: Integer;
    ElementNode: IXMLNode;
    Item: TJSONValue;
  begin
    for i := 0 to JSONArr.Count - 1 do
    begin
      Item := JSONArr.Items[i];

      // Créer un élément pour chaque item du tableau
      ElementNode := ParentNode.AddChild('item');
      ElementNode.Attributes['index'] := IntToStr(i);

      if Item is TJSONObject then
        JSONObjectToXML(Item as TJSONObject, ElementNode)
      else if Item is TJSONArray then
        JSONArrayToXML(Item as TJSONArray, ElementNode)
      else
        ElementNode.Text := Item.Value;
    end;
  end;

begin
  Result := '';
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  if not Assigned(JSONValue) then
    Exit;

  try
    if JSONValue is TJSONObject then
    begin
      XMLDoc := TXMLDocument.Create(nil);
      XMLDoc.Active := True;

      try
        // Créer un nœud racine
        RootNode := XMLDoc.AddChild('root');

        // Convertir l'objet JSON en XML
        JSONObjectToXML(JSONValue as TJSONObject, RootNode);

        // Retourner le résultat
        XMLDoc.SaveToXML(Result);
      finally
        // Pas besoin de libérer XMLDoc grâce à l'interface IXMLDocument
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### XML vers JSON

```pascal
function XMLversJSON(const XMLString: string): string;
var
  XMLDoc: IXMLDocument;
  JSONObject: TJSONObject;

  // Fonction récursive pour convertir un nœud XML en objet JSON
  function XMLNodeToJSON(XMLNode: IXMLNode): TJSONValue;
  var
    i: Integer;
    ChildNode: IXMLNode;
    JSONObj: TJSONObject;
    JSONArr: TJSONArray;
    ChildNames: TStringList;
    Name: string;
  begin
    JSONObj := TJSONObject.Create;

    // Ajouter les attributs
    for i := 0 to XMLNode.AttributeNodes.Count - 1 do
    begin
      JSONObj.AddPair('@' + XMLNode.AttributeNodes[i].NodeName,
                     XMLNode.AttributeNodes[i].Text);
    end;

    // Vérifier s'il y a des enfants
    if XMLNode.HasChildNodes then
    begin
      // Compter les occurrences de chaque nom d'enfant
      ChildNames := TStringList.Create;
      try
        for i := 0 to XMLNode.ChildNodes.Count - 1 do
        begin
          ChildNode := XMLNode.ChildNodes[i];
          ChildNames.Add(ChildNode.NodeName);
        end;

        // Parcourir les enfants
        for i := 0 to XMLNode.ChildNodes.Count - 1 do
        begin
          ChildNode := XMLNode.ChildNodes[i];
          Name := ChildNode.NodeName;

          // Compter les occurrences du nom
          if ChildNames.IndexOf(Name) <> ChildNames.LastIndexOf(Name) then
          begin
            // Plusieurs enfants avec le même nom = tableau
            if not (JSONObj.GetValue(Name) is TJSONArray) then
            begin
              JSONArr := TJSONArray.Create;
              JSONObj.AddPair(Name, JSONArr);
            end else
              JSONArr := JSONObj.GetValue(Name) as TJSONArray;

            // Ajouter l'élément au tableau
            if ChildNode.HasChildNodes and (ChildNode.ChildNodes.Count > 0) then
              JSONArr.AddElement(XMLNodeToJSON(ChildNode))
            else
              JSONArr.Add(ChildNode.Text);
          end
          else
          begin
            // Un seul enfant avec ce nom
            if ChildNode.HasChildNodes and (ChildNode.ChildNodes.Count > 0) then
              JSONObj.AddPair(Name, XMLNodeToJSON(ChildNode))
            else
              JSONObj.AddPair(Name, ChildNode.Text);
          end;
        end;
      finally
        ChildNames.Free;
      end;
    end
    else
    begin
      // Nœud feuille
      JSONObj.AddPair('#text', XMLNode.Text);
    end;

    Result := JSONObj;
  end;

begin
  Result := '';

  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    XMLDoc.LoadFromXML(XMLString);

    // Convertir le nœud racine en JSON
    JSONObject := XMLNodeToJSON(XMLDoc.DocumentElement) as TJSONObject;
    try
      Result := JSONObject.ToString;
    finally
      JSONObject.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de conversion XML vers JSON: ' + E.Message);
  end;
end;
```

## Validation de données

### Validation de JSON

Pour vérifier si une chaîne est un JSON valide :

```pascal
function EstJSONValide(const JSONString: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  Result := False;

  try
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    Result := Assigned(JSONValue);

    if Result then
      JSONValue.Free;
  except
    // Une exception signifie que le JSON est invalide
    Result := False;
  end;
end;
```

### Validation de XML avec un schéma XSD

Pour valider un document XML par rapport à un schéma XSD :

```pascal
function ValiderXMLAvecSchema(const XMLString, XSDString: string): Boolean;
var
  XMLDoc: IXMLDocument;
  SchemaDoc: IXMLDocument;
  Schema: TXMLSchema;
begin
  Result := False;

  XMLDoc := TXMLDocument.Create(nil);
  SchemaDoc := TXMLDocument.Create(nil);

  try
    XMLDoc.Active := True;
    SchemaDoc.Active := True;

    // Charger le XML et le schéma
    XMLDoc.LoadFromXML(XMLString);
    SchemaDoc.LoadFromXML(XSDString);

    // Créer un validateur de schéma
    Schema := TXMLSchema.Create(nil);
    try
      Schema.LoadFromXML(SchemaDoc);

      // Valider le document XML
      Result := Schema.ValidateDocument(XMLDoc);
    finally
      Schema.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de validation: ' + E.Message);
  end;
end;
```

## Bonnes pratiques

1. **Libérez toujours la mémoire** : Les objets JSON doivent être libérés manuellement.
2. **Utilisez les blocs try-finally** : Pour garantir la libération des ressources.
3. **Vérifiez la validité des entrées** : Toujours vérifier si un document est valide avant de le traiter.
4. **Pensez à la robustesse** : Utilisez des méthodes comme `TryGetValue` qui ne lèvent pas d'exception.
5. **Sécurité** : Méfiez-vous des injections lors de l'analyse de JSON ou XML provenant de sources externes.

## Exemple complet : Application de carnet d'adresses

Voici un exemple combinant JSON et XML pour une application de carnet d'adresses :

```pascal
type
  TContact = record
    Nom: string;
    Prenom: string;
    Email: string;
    Telephone: string;
  end;

  TContacts = array of TContact;

// Sauvegarder les contacts en JSON
procedure SauvegarderContactsJSON(const Contacts: TContacts;
                                 const NomFichier: string);
var
  JSONObject: TJSONObject;
  ContactsArray: TJSONArray;
  ContactObj: TJSONObject;
  i: Integer;
begin
  JSONObject := TJSONObject.Create;
  try
    ContactsArray := TJSONArray.Create;
    JSONObject.AddPair('contacts', ContactsArray);

    for i := 0 to Length(Contacts) - 1 do
    begin
      ContactObj := TJSONObject.Create;
      ContactObj.AddPair('nom', Contacts[i].Nom);
      ContactObj.AddPair('prenom', Contacts[i].Prenom);
      ContactObj.AddPair('email', Contacts[i].Email);
      ContactObj.AddPair('telephone', Contacts[i].Telephone);

      ContactsArray.AddElement(ContactObj);
    end;

    // Écrire dans un fichier
    with TStringList.Create do
    try
      Text := JSONObject.ToString;
      SaveToFile(NomFichier);
    finally
      Free;
    end;
  finally
    JSONObject.Free;
  end;
end;

// Charger les contacts depuis JSON
function ChargerContactsJSON(const NomFichier: string): TContacts;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  ContactsArray: TJSONArray;
  i: Integer;
begin
  SetLength(Result, 0);

  // Lire le fichier
  with TStringList.Create do
  try
    LoadFromFile(NomFichier);
    JSONString := Text;
  finally
    Free;
  end;

  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      if JSONObject.TryGetValue<TJSONArray>('contacts', ContactsArray) then
      begin
        SetLength(Result, ContactsArray.Count);

        for i := 0 to ContactsArray.Count - 1 do
        begin
          if ContactsArray.Items[i] is TJSONObject then
          begin
            Result[i].Nom := ContactsArray.Items[i].GetValue<string>('nom');
            Result[i].Prenom := ContactsArray.Items[i].GetValue<string>('prenom');
            Result[i].Email := ContactsArray.Items[i].GetValue<string>('email');
            Result[i].Telephone := ContactsArray.Items[i].GetValue<string>('telephone');
          end;
        end;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;

// Exporter les contacts en XML
procedure ExporterContactsXML(const Contacts: TContacts;
                             const NomFichier: string);
var
  XMLDoc: IXMLDocument;
  RootNode, ContactsNode, ContactNode: IXMLNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  try
    // Créer la structure XML
    RootNode := XMLDoc.AddChild('carnetAdresses');
    ContactsNode := RootNode.AddChild('contacts');

    for i := 0 to Length(Contacts) - 1 do
    begin
      ContactNode := ContactsNode.AddChild('contact');
      ContactNode.AddChild('nom').Text := Contacts[i].Nom;
      ContactNode.AddChild('prenom').Text := Contacts[i].Prenom;
      ContactNode.AddChild('email').Text := Contacts[i].Email;
      ContactNode.AddChild('telephone').Text := Contacts[i].Telephone;
    end;

    // Enregistrer dans un fichier
    XMLDoc.SaveToFile(NomFichier);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''exportation XML: ' + E.Message);
  end;
end;
```

## Conclusion

La manipulation de JSON et XML est essentielle dans le développement d'applications modernes. Bien que le JSON soit devenu plus populaire pour sa simplicité et sa légèreté, le XML reste important dans de nombreux contextes, notamment pour les documents fortement structurés et les services web SOAP.

En maîtrisant ces deux formats, vous serez capable d'intégrer facilement votre application à des API Web et d'autres services externes.

## Exercices pratiques

1. Créez une application de notes qui sauvegarde les données en JSON.
2. Modifiez l'exemple de carnet d'adresses pour ajouter la gestion des groupes de contacts.
3. Créez un convertisseur qui permet de passer d'un format à l'autre (JSON vers XML et vice versa).
4. Développez un petit éditeur qui permet de visualiser et modifier des documents JSON et XML.

N'oubliez pas que la pratique est la clé pour maîtriser ces concepts. Expérimentez avec différents exemples et structures pour vous familiariser avec les particularités de chaque format.

⏭️ [Socket et communications TCP/IP](/10-communication-et-services-reseaux/03-socket-et-communications-tcp-ip.md)
