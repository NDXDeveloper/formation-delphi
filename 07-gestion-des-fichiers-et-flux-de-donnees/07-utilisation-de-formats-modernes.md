# 7.7 Utilisation de formats modernes (JSON, XML, YAML)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les formats de données structurées comme JSON, XML et YAML sont aujourd'hui omniprésents dans le développement d'applications modernes. Delphi offre des outils puissants et faciles à utiliser pour manipuler ces formats. Ce chapitre vous guidera à travers l'utilisation de chacun d'entre eux.

## 7.7.1 JSON (JavaScript Object Notation)

JSON est devenu l'un des formats d'échange de données les plus populaires en raison de sa légèreté et de sa simplicité. Delphi intègre un support natif pour JSON à travers plusieurs unités.

### Utilisation de l'unité `System.JSON`

L'unité `System.JSON` fournit des classes de base pour la manipulation de JSON.

```pascal
uses
  System.JSON;

procedure ExempleJSON;
var
  JSONObj: TJSONObject;
  JSONArray: TJSONArray;
  JSONPair: TJSONPair;
  JSONString: string;
begin
  // Création d'un objet JSON
  JSONObj := TJSONObject.Create;
  try
    // Ajout de paires clé-valeur
    JSONObj.AddPair('nom', 'Dupont');
    JSONObj.AddPair('prenom', 'Jean');
    JSONObj.AddPair('age', TJSONNumber.Create(42));

    // Création d'un tableau JSON
    JSONArray := TJSONArray.Create;
    JSONArray.Add('football');
    JSONArray.Add('lecture');
    JSONArray.Add('voyages');

    // Ajout du tableau à l'objet
    JSONObj.AddPair('loisirs', JSONArray);

    // Conversion en chaîne
    JSONString := JSONObj.ToString;

    ShowMessage(JSONString);
    // Affiche: {"nom":"Dupont","prenom":"Jean","age":42,"loisirs":["football","lecture","voyages"]}
  finally
    // Ne pas oublier de libérer l'objet (il libère aussi les objets enfants automatiquement)
    JSONObj.Free;
  end;
end;
```

### Lecture de JSON

Pour lire du JSON, vous pouvez utiliser les méthodes de désérialisation :

```pascal
procedure LireJSON;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
  Nom, Prenom: string;
  Age: Integer;
  Loisirs: TJSONArray;
  i: Integer;
begin
  JSONString := '{"nom":"Dupont","prenom":"Jean","age":42,"loisirs":["football","lecture","voyages"]}';

  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONValue);

      // Récupération des valeurs simples
      Nom := JSONObj.GetValue<string>('nom');
      Prenom := JSONObj.GetValue<string>('prenom');
      Age := JSONObj.GetValue<Integer>('age');

      // Récupération et parcours d'un tableau
      Loisirs := JSONObj.GetValue<TJSONArray>('loisirs');
      for i := 0 to Loisirs.Count - 1 do
      begin
        ShowMessage('Loisir ' + IntToStr(i+1) + ': ' + Loisirs.Items[i].Value);
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Utilisation de `System.JSON.Serializers` (plus récent et puissant)

À partir de Delphi 10.4, l'unité `System.JSON.Serializers` offre des fonctionnalités avancées pour sérialiser et désérialiser directement des objets.

```pascal
// Définition d'une classe à sérialiser
type
  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FLoisirs: TArray<string>;
  public
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Loisirs: TArray<string> read FLoisirs write FLoisirs;
  end;
```

```pascal
uses
  System.JSON.Serializers;

procedure SerializerExemple;
var
  Personne: TPersonne;
  JSONSerializer: TJsonSerializer;
  JSONString: string;
  PersonneDeserialisee: TPersonne;
begin
  Personne := TPersonne.Create;
  try
    Personne.Nom := 'Dupont';
    Personne.Prenom := 'Jean';
    Personne.Age := 42;
    Personne.Loisirs := ['football', 'lecture', 'voyages'];

    // Sérialisation
    JSONSerializer := TJsonSerializer.Create;
    try
      JSONString := JSONSerializer.Serialize(Personne);
      ShowMessage('Objet sérialisé: ' + JSONString);

      // Désérialisation
      PersonneDeserialisee := JSONSerializer.Deserialize<TPersonne>(JSONString);
      try
        ShowMessage('Personne désérialisée: ' + PersonneDeserialisee.Prenom + ' ' +
                   PersonneDeserialisee.Nom + ', ' + IntToStr(PersonneDeserialisee.Age) + ' ans');
      finally
        PersonneDeserialisee.Free;
      end;
    finally
      JSONSerializer.Free;
    end;
  finally
    Personne.Free;
  end;
end;
```

> 💡 **Conseil pratique**: Pour un débogage plus facile, utilisez l'option `TJsonSerializer.Create(TJsonOptions.Pretty)` qui formatera votre JSON avec indentation et sauts de ligne.

## 7.7.2 XML (eXtensible Markup Language)

XML est un format plus ancien mais toujours très utilisé, notamment pour les configurations et les échanges avec certains services web. Delphi offre plusieurs approches pour manipuler le XML.

### Utilisation de `XMLDocument`

```pascal
uses
  Xml.XMLDoc, Xml.XMLIntf;

procedure ExempleXML;
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode, LoisirsNode, LoisirNode: IXMLNode;
begin
  // Création d'un document XML
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  // Création de l'élément racine
  RootNode := XMLDoc.AddChild('personnes');

  // Ajout d'un élément personne
  PersonneNode := RootNode.AddChild('personne');
  PersonneNode.Attributes['id'] := '1';

  // Ajout des éléments enfants
  PersonneNode.AddChild('nom').Text := 'Dupont';
  PersonneNode.AddChild('prenom').Text := 'Jean';
  PersonneNode.AddChild('age').Text := '42';

  // Ajout d'une liste de loisirs
  LoisirsNode := PersonneNode.AddChild('loisirs');

  LoisirNode := LoisirsNode.AddChild('loisir');
  LoisirNode.Text := 'football';

  LoisirNode := LoisirsNode.AddChild('loisir');
  LoisirNode.Text := 'lecture';

  LoisirNode := LoisirsNode.AddChild('loisir');
  LoisirNode.Text := 'voyages';

  // Affichage du XML
  XMLDoc.SaveToXML(XMLDoc.XML);
  ShowMessage(XMLDoc.XML.Text);

  // Enregistrement dans un fichier
  XMLDoc.SaveToFile('personnes.xml');
end;
```

### Lecture d'un fichier XML

```pascal
procedure LireXML;
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode, LoisirsNode: IXMLNode;
  i, j: Integer;
  Nom, Prenom, Age: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromFile('personnes.xml');
  XMLDoc.Active := True;

  RootNode := XMLDoc.DocumentElement;

  // Parcours des personnes
  for i := 0 to RootNode.ChildNodes.Count - 1 do
  begin
    PersonneNode := RootNode.ChildNodes[i];

    Nom := PersonneNode.ChildNodes['nom'].Text;
    Prenom := PersonneNode.ChildNodes['prenom'].Text;
    Age := PersonneNode.ChildNodes['age'].Text;

    ShowMessage(Format('Personne: %s %s, %s ans', [Prenom, Nom, Age]));

    // Parcours des loisirs
    LoisirsNode := PersonneNode.ChildNodes['loisirs'];
    if Assigned(LoisirsNode) then
    begin
      for j := 0 to LoisirsNode.ChildNodes.Count - 1 do
      begin
        ShowMessage('Loisir: ' + LoisirsNode.ChildNodes[j].Text);
      end;
    end;
  end;
end;
```

### Requêtes XPath

XPath est un langage puissant pour naviguer dans les documents XML :

```pascal
procedure ExempleXPath;
var
  XMLDoc: IXMLDocument;
  NodeList: IDOMNodeList;
  Node: IDOMNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromFile('personnes.xml');
  XMLDoc.Active := True;

  // Trouver toutes les personnes de plus de 40 ans
  NodeList := (XMLDoc.DOMDocument as IDOMDocument).selectNodes('//personne[age>40]');

  ShowMessage('Personnes de plus de 40 ans:');
  for i := 0 to NodeList.length - 1 do
  begin
    Node := NodeList.item[i];
    ShowMessage('ID: ' + IDOMElement(Node).getAttribute('id'));
  end;

  // Trouver tous les loisirs
  NodeList := (XMLDoc.DOMDocument as IDOMDocument).selectNodes('//loisir');

  ShowMessage('Tous les loisirs:');
  for i := 0 to NodeList.length - 1 do
  begin
    Node := NodeList.item[i];
    ShowMessage(Node.textContent);
  end;
end;
```

## 7.7.3 YAML (YAML Ain't Markup Language)

YAML est un format de sérialisation de données lisible par l'homme, souvent utilisé pour les fichiers de configuration. Delphi n'a pas de support natif pour YAML, mais vous pouvez utiliser des bibliothèques tierces comme `yaml4delphi` ou `TPyYAML`.

### Utilisation avec une bibliothèque tierce

Voici un exemple utilisant la bibliothèque `yaml4delphi` (à installer préalablement via GetIt Package Manager ou GitHub) :

```pascal
uses
  YamlDom, YamlReader, YamlWriter;

procedure ExempleYAML;
var
  YamlDoc: TYamlDocument;
  RootNode, PersonneNode, LoisirsNode: TYamlNode;
  Stream: TStringStream;
  YamlString: string;
begin
  // Création d'un document YAML
  YamlDoc := TYamlDocument.Create;
  try
    // Création du nœud racine (map)
    RootNode := YamlDoc.Root.AsMap;

    // Création d'une personne
    PersonneNode := RootNode.AddMap('personne');
    PersonneNode.AddString('nom', 'Dupont');
    PersonneNode.AddString('prenom', 'Jean');
    PersonneNode.AddInteger('age', 42);

    // Ajout d'une liste de loisirs
    LoisirsNode := PersonneNode.AddSequence('loisirs');
    LoisirsNode.AddString('football');
    LoisirsNode.AddString('lecture');
    LoisirsNode.AddString('voyages');

    // Conversion en chaîne
    Stream := TStringStream.Create;
    try
      SaveYamlToStream(YamlDoc, Stream);
      YamlString := Stream.DataString;
      ShowMessage(YamlString);
    finally
      Stream.Free;
    end;

    // Enregistrement dans un fichier
    SaveYamlToFile(YamlDoc, 'personne.yaml');
  finally
    YamlDoc.Free;
  end;
end;
```

### Lecture d'un fichier YAML

```pascal
procedure LireYAML;
var
  YamlDoc: TYamlDocument;
  PersonneNode, LoisirsNode: TYamlNode;
  i: Integer;
begin
  YamlDoc := LoadYamlFromFile('personne.yaml');
  try
    PersonneNode := YamlDoc.Root.AsMap.GetMap('personne');

    ShowMessage(Format('Personne: %s %s, %d ans',
      [PersonneNode.GetString('prenom'),
       PersonneNode.GetString('nom'),
       PersonneNode.GetInteger('age')]));

    // Parcours des loisirs
    LoisirsNode := PersonneNode.GetSequence('loisirs');
    for i := 0 to LoisirsNode.Count - 1 do
    begin
      ShowMessage('Loisir: ' + LoisirsNode.Items[i].AsString);
    end;
  finally
    YamlDoc.Free;
  end;
end;
```

> ⚠️ **Note** : L'implémentation exacte peut varier selon la bibliothèque YAML utilisée. Vérifiez la documentation de la bibliothèque choisie.

## 7.7.4 Conversion entre formats

Il est parfois nécessaire de convertir des données d'un format à un autre. Voici un exemple simple de conversion de JSON vers XML :

```pascal
uses
  System.JSON, Xml.XMLDoc, Xml.XMLIntf;

procedure ConvertirJSONversXML;
var
  JSONString: string;
  JSONValue: TJSONValue;
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;

  // Fonction récursive pour convertir JSON en XML
  procedure JSONToXML(const JSONValue: TJSONValue; ParentNode: IXMLNode);
  var
    JSONObj: TJSONObject;
    JSONArray: TJSONArray;
    JSONPair: TJSONPair;
    ChildNode: IXMLNode;
    i: Integer;
  begin
    if JSONValue is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONValue);

      for i := 0 to JSONObj.Count - 1 do
      begin
        JSONPair := JSONObj.Pairs[i];

        if JSONPair.JsonValue is TJSONArray then
        begin
          // Création d'un nœud conteneur pour les tableaux
          ChildNode := ParentNode.AddChild(JSONPair.JsonString.Value);
          JSONToXML(JSONPair.JsonValue, ChildNode);
        end
        else if JSONPair.JsonValue is TJSONObject then
        begin
          ChildNode := ParentNode.AddChild(JSONPair.JsonString.Value);
          JSONToXML(JSONPair.JsonValue, ChildNode);
        end
        else
        begin
          // Valeur simple
          ParentNode.AddChild(JSONPair.JsonString.Value).Text := JSONPair.JsonValue.Value;
        end;
      end;
    end
    else if JSONValue is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONValue);

      for i := 0 to JSONArray.Count - 1 do
      begin
        ChildNode := ParentNode.AddChild('item');
        JSONToXML(JSONArray.Items[i], ChildNode);
      end;
    end;
  end;

begin
  JSONString := '{"nom":"Dupont","prenom":"Jean","age":42,"loisirs":["football","lecture","voyages"]}';

  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    // Création du document XML
    XMLDoc := TXMLDocument.Create(nil);
    XMLDoc.Active := True;

    // Création du nœud racine
    RootNode := XMLDoc.AddChild('root');

    // Conversion de JSON vers XML
    JSONToXML(JSONValue, RootNode);

    // Affichage du résultat
    XMLDoc.SaveToXML(XMLDoc.XML);
    ShowMessage(XMLDoc.XML.Text);

    // Enregistrement dans un fichier
    XMLDoc.SaveToFile('resultat.xml');
  finally
    JSONValue.Free;
  end;
end;
```

## 7.7.5 Bonnes pratiques

Voici quelques conseils pour travailler efficacement avec ces formats :

1. **Gestion de la mémoire** : N'oubliez pas de libérer les objets JSON, les documents XML et YAML quand vous avez terminé.

2. **Validation** : Vérifiez toujours que votre JSON ou XML est valide avant de le traiter :
   ```pascal
   // Pour JSON
   if not TJSONObject.ParseJSONValue(JSONString).Null then
     // Le JSON est valide

   // Pour XML
   try
     XMLDoc.LoadFromXML(XMLString);
     // Le XML est valide
   except
     on E: Exception do
       ShowMessage('XML invalide : ' + E.Message);
   end;
   ```

3. **Performances** : Pour les grands fichiers, préférez les approches de type "streaming" ou les parseurs SAX pour XML qui consomment moins de mémoire.

4. **Encodage des caractères** : Assurez-vous d'utiliser l'encodage UTF-8 pour une meilleure compatibilité :
   ```pascal
   XMLDoc.Encoding := 'UTF-8';
   ```

5. **Sécurité** : Méfiez-vous des données externes, surtout pour XML qui peut être sujet aux attaques XXE (XML External Entities).

## 7.7.6 Conclusion

Les formats JSON, XML et YAML offrent des moyens puissants de structurer et d'échanger des données. Delphi fournit des outils natifs pour JSON et XML, tandis que YAML nécessite des bibliothèques tierces.

Le JSON est généralement préféré pour sa légèreté et sa facilité d'utilisation, particulièrement pour les API web modernes. XML reste pertinent pour les configurations complexes et les formats de documents, tandis que YAML excelle dans la lisibilité humaine pour les fichiers de configuration.

Choisissez le format qui correspond le mieux à vos besoins spécifiques, en tenant compte de la complexité des données, des exigences de performance et de l'écosystème dans lequel votre application s'intègre.

## 7.7.7 Exercices pratiques

1. Créez une application simple qui permet de saisir des informations sur une personne et de les enregistrer au format JSON, XML et YAML.

2. Développez un visualiseur qui peut charger et afficher des fichiers dans ces trois formats.

3. Créez un convertisseur qui transforme un format en un autre (par exemple, XML vers JSON).

4. Réalisez une fonction qui valide la structure d'un document JSON ou XML selon un schéma donné.

⏭️ [Manipulation de fichiers CSV et Excel](/07-gestion-des-fichiers-et-flux-de-donnees/08-manipulation-de-fichiers-csv-et-excel.md)
