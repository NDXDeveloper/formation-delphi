🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.2 Manipulation de JSON et XML

## Introduction aux formats de données

### Pourquoi JSON et XML ?

Dans le développement d'applications modernes, vous devez souvent échanger des données avec d'autres systèmes : serveurs web, bases de données, API, fichiers de configuration. Deux formats se distinguent pour structurer ces données : **JSON** et **XML**.

**JSON** (JavaScript Object Notation) est léger, facile à lire et très populaire dans les applications web modernes. **XML** (eXtensible Markup Language) est plus verbeux mais offre une structure riche et est utilisé dans de nombreux systèmes d'entreprise.

### Quand utiliser JSON ou XML ?

**Utilisez JSON pour :**
- Les API REST modernes
- Les fichiers de configuration simples
- L'échange de données avec des applications web
- Les données légères et rapides à traiter

**Utilisez XML pour :**
- Les documents complexes avec métadonnées
- L'interopérabilité avec des systèmes legacy
- Les services SOAP
- Les fichiers nécessitant une validation stricte (schémas)

## Manipulation de JSON avec Delphi

### Structure du format JSON

JSON organise les données en paires clé-valeur :

```json
{
  "nom": "Dubois",
  "prenom": "Marie",
  "age": 28,
  "actif": true,
  "adresse": {
    "rue": "12 Rue de la Paix",
    "ville": "Paris",
    "codePostal": "75001"
  },
  "competences": ["Delphi", "SQL", "JavaScript"]
}
```

**Types de données JSON :**
- **Chaînes** : Entre guillemets `"texte"`
- **Nombres** : `42` ou `3.14`
- **Booléens** : `true` ou `false`
- **Objets** : Entre accolades `{ }`
- **Tableaux** : Entre crochets `[ ]`
- **Null** : `null`

### Les classes JSON de Delphi

Delphi propose plusieurs classes pour manipuler JSON, situées dans l'unité `System.JSON` :

- **TJSONObject** : Représente un objet JSON
- **TJSONArray** : Représente un tableau JSON
- **TJSONString** : Représente une chaîne
- **TJSONNumber** : Représente un nombre
- **TJSONBool** : Représente un booléen
- **TJSONNull** : Représente null

### Créer un objet JSON

Voici comment créer un objet JSON de toutes pièces :

```pascal
uses
  System.JSON;

procedure TForm1.CreerJSON;  
var  
  JSONObject: TJSONObject;
  Adresse: TJSONObject;
  Competences: TJSONArray;
  ResultatJSON: string;
begin
  // Créer l'objet principal
  JSONObject := TJSONObject.Create;
  try
    // Ajouter des propriétés simples
    JSONObject.AddPair('nom', 'Dubois');
    JSONObject.AddPair('prenom', 'Marie');
    JSONObject.AddPair('age', TJSONNumber.Create(28));
    JSONObject.AddPair('actif', TJSONBool.Create(True));

    // Créer un objet imbriqué pour l'adresse
    Adresse := TJSONObject.Create;
    Adresse.AddPair('rue', '12 Rue de la Paix');
    Adresse.AddPair('ville', 'Paris');
    Adresse.AddPair('codePostal', '75001');
    JSONObject.AddPair('adresse', Adresse);

    // Créer un tableau de compétences
    Competences := TJSONArray.Create;
    Competences.Add('Delphi');
    Competences.Add('SQL');
    Competences.Add('JavaScript');
    JSONObject.AddPair('competences', Competences);

    // Convertir en chaîne JSON
    ResultatJSON := JSONObject.ToString;

    // Afficher le résultat
    Memo1.Text := ResultatJSON;

  finally
    JSONObject.Free; // Libère automatiquement tous les objets imbriqués
  end;
end;
```

**Points importants :**
- `AddPair` ajoute une paire clé-valeur
- Pour les nombres et booléens, utilisez `TJSONNumber.Create` et `TJSONBool.Create`
- Libérer l'objet principal libère automatiquement tous les objets imbriqués

### Lire et parser du JSON

Pour lire du JSON existant, vous utilisez `ParseJSONValue` :

```pascal
procedure TForm1.LireJSON;  
var  
  JSONTexte: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Nom, Prenom, Ville: string;
  Age: Integer;
  Actif: Boolean;
begin
  // Le JSON à parser
  JSONTexte := '{"nom":"Dubois","prenom":"Marie","age":28,"actif":true}';

  // Parser le JSON
  JSONValue := TJSONObject.ParseJSONValue(JSONTexte);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire les valeurs
      Nom := JSONObject.GetValue<string>('nom');
      Prenom := JSONObject.GetValue<string>('prenom');
      Age := JSONObject.GetValue<Integer>('age');
      Actif := JSONObject.GetValue<Boolean>('actif');

      // Afficher
      ShowMessage(Format('Nom: %s %s, Age: %d, Actif: %s',
        [Prenom, Nom, Age, BoolToStr(Actif, True)]));
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Accéder aux objets imbriqués

Pour naviguer dans des structures JSON complexes :

```pascal
procedure TForm1.LireJSONImbrique;  
var  
  JSONTexte: string;
  JSONValue: TJSONValue;
  JSONObject, AdresseObject: TJSONObject;
  Ville, CodePostal: string;
begin
  JSONTexte := '{"nom":"Dubois","adresse":{"ville":"Paris","codePostal":"75001"}}';

  JSONValue := TJSONObject.ParseJSONValue(JSONTexte);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Accéder à l'objet imbriqué
      AdresseObject := JSONObject.GetValue<TJSONObject>('adresse');

      if Assigned(AdresseObject) then
      begin
        Ville := AdresseObject.GetValue<string>('ville');
        CodePostal := AdresseObject.GetValue<string>('codePostal');

        ShowMessage('Ville: ' + Ville + ', CP: ' + CodePostal);
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Manipuler les tableaux JSON

Pour travailler avec des tableaux :

```pascal
procedure TForm1.LireTableauJSON;  
var  
  JSONTexte: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Competences: TJSONArray;
  i: Integer;
  Competence: string;
begin
  JSONTexte := '{"nom":"Dubois","competences":["Delphi","SQL","JavaScript"]}';

  JSONValue := TJSONObject.ParseJSONValue(JSONTexte);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Récupérer le tableau
      Competences := JSONObject.GetValue<TJSONArray>('competences');

      if Assigned(Competences) then
      begin
        Memo1.Lines.Add('Compétences:');

        // Parcourir le tableau
        for i := 0 to Competences.Count - 1 do
        begin
          Competence := Competences.Items[i].Value;
          Memo1.Lines.Add('- ' + Competence);
        end;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Vérifier l'existence d'une propriété

Avant d'accéder à une propriété, vérifiez qu'elle existe :

```pascal
procedure TForm1.VerifierPropriete;  
var  
  JSONObject: TJSONObject;
  JSONValue: TJSONValue;
  Email: string;
begin
  JSONObject := TJSONObject.ParseJSONValue('{"nom":"Dubois"}') as TJSONObject;
  try
    // Méthode 1 : TryGetValue
    if JSONObject.TryGetValue<string>('email', Email) then
      ShowMessage('Email: ' + Email)
    else
      ShowMessage('Pas d''email');

    // Méthode 2 : FindValue
    JSONValue := JSONObject.FindValue('email');
    if Assigned(JSONValue) then
      ShowMessage('Email trouvé: ' + JSONValue.Value)
    else
      ShowMessage('Email non trouvé');

  finally
    JSONObject.Free;
  end;
end;
```

### Modifier un objet JSON

Pour modifier des valeurs existantes :

```pascal
procedure TForm1.ModifierJSON;  
var  
  JSONObject: TJSONObject;
  PaireAge: TJSONPair;
begin
  JSONObject := TJSONObject.ParseJSONValue('{"nom":"Dubois","age":28}') as TJSONObject;
  try
    // Méthode 1 : Supprimer et recréer
    JSONObject.RemovePair('age');
    JSONObject.AddPair('age', TJSONNumber.Create(29));

    // Méthode 2 : Accéder à la paire et modifier sa valeur
    PaireAge := JSONObject.Get('age');
    if Assigned(PaireAge) then
    begin
      PaireAge.JsonValue.Free;
      PaireAge.JsonValue := TJSONNumber.Create(30);
    end;

    Memo1.Text := JSONObject.ToString;
  finally
    JSONObject.Free;
  end;
end;
```

### Formater le JSON (Pretty Print)

Pour rendre le JSON plus lisible :

```pascal
procedure TForm1.FormaterJSON;  
var  
  JSONObject: TJSONObject;
  JSONFormate: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('nom', 'Dubois');
    JSONObject.AddPair('prenom', 'Marie');
    JSONObject.AddPair('age', TJSONNumber.Create(28));

    // Format compact (par défaut)
    Memo1.Lines.Add('Compact:');
    Memo1.Lines.Add(JSONObject.ToString);
    Memo1.Lines.Add('');

    // Format indenté
    Memo1.Lines.Add('Formaté:');
    JSONFormate := JSONObject.Format(2); // 2 = indentation de 2 espaces
    Memo1.Lines.Add(JSONFormate);
  finally
    JSONObject.Free;
  end;
end;
```

### Sauvegarder et charger depuis un fichier

Pour persister du JSON :

```pascal
procedure TForm1.SauvegarderJSON;  
var  
  JSONObject: TJSONObject;
  Fichier: TStringList;
begin
  JSONObject := TJSONObject.Create;
  Fichier := TStringList.Create;
  try
    JSONObject.AddPair('nom', 'Dubois');
    JSONObject.AddPair('age', TJSONNumber.Create(28));

    // Sauvegarder
    Fichier.Text := JSONObject.Format(2);
    Fichier.SaveToFile('donnees.json', TEncoding.UTF8);

    ShowMessage('JSON sauvegardé');
  finally
    Fichier.Free;
    JSONObject.Free;
  end;
end;

procedure TForm1.ChargerJSON;  
var  
  Fichier: TStringList;
  JSONObject: TJSONObject;
  Nom: string;
begin
  Fichier := TStringList.Create;
  try
    Fichier.LoadFromFile('donnees.json', TEncoding.UTF8);

    JSONObject := TJSONObject.ParseJSONValue(Fichier.Text) as TJSONObject;
    try
      Nom := JSONObject.GetValue<string>('nom');
      ShowMessage('Nom chargé: ' + Nom);
    finally
      JSONObject.Free;
    end;
  finally
    Fichier.Free;
  end;
end;
```

## Manipulation de XML avec Delphi

### Structure du format XML

XML organise les données en éléments avec balises ouvrantes et fermantes :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<personne>
  <nom>Dubois</nom>
  <prenom>Marie</prenom>
  <age>28</age>
  <actif>true</actif>
  <adresse>
    <rue>12 Rue de la Paix</rue>
    <ville>Paris</ville>
    <codePostal>75001</codePostal>
  </adresse>
  <competences>
    <competence>Delphi</competence>
    <competence>SQL</competence>
    <competence>JavaScript</competence>
  </competences>
</personne>
```

**Caractéristiques XML :**
- Chaque élément a une balise ouvrante `<nom>` et fermante `</nom>`
- Les éléments peuvent avoir des attributs : `<personne id="123">`
- Structure hiérarchique en arbre
- Auto-descriptif et lisible

### Les interfaces XML de Delphi

Delphi propose plusieurs approches pour XML :

**1. XML.XMLDoc** - API DOM (Document Object Model)
- Charge tout le document en mémoire
- Facile à manipuler
- Bon pour les documents de taille moyenne

**2. XML.XMLIntf** - Interfaces XML
- Plus bas niveau
- Plus de contrôle

**3. XML.VerySimple** - API simplifiée
- Très intuitive
- Recommandée pour les débutants

Nous allons principalement utiliser **XML.XMLDoc** qui est le plus courant.

### Créer un document XML

Voici comment créer un document XML :

```pascal
uses
  Xml.XMLDoc, Xml.XMLIntf;

procedure TForm1.CreerXML;  
var  
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode, AdresseNode, CompetencesNode: IXMLNode;
begin
  // Créer le document
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    // Créer le nœud racine
    RootNode := XMLDoc.AddChild('personne');

    // Ajouter des éléments simples
    RootNode.AddChild('nom').Text := 'Dubois';
    RootNode.AddChild('prenom').Text := 'Marie';
    RootNode.AddChild('age').Text := '28';
    RootNode.AddChild('actif').Text := 'true';

    // Ajouter un élément avec attribut
    PersonneNode := RootNode;
    PersonneNode.Attributes['id'] := '123';

    // Créer un élément imbriqué (adresse)
    AdresseNode := RootNode.AddChild('adresse');
    AdresseNode.AddChild('rue').Text := '12 Rue de la Paix';
    AdresseNode.AddChild('ville').Text := 'Paris';
    AdresseNode.AddChild('codePostal').Text := '75001';

    // Créer une liste d'éléments
    CompetencesNode := RootNode.AddChild('competences');
    CompetencesNode.AddChild('competence').Text := 'Delphi';
    CompetencesNode.AddChild('competence').Text := 'SQL';
    CompetencesNode.AddChild('competence').Text := 'JavaScript';

    // Afficher le résultat
    XMLDoc.SaveToXML(Memo1.Lines);

  finally
    XMLDoc := nil;
  end;
end;
```

**Points importants :**
- `IXMLDocument` est une interface, pas besoin de `Free`
- `Active := True` active le document
- `AddChild` crée un nouveau nœud enfant
- `Attributes[]` permet d'ajouter des attributs

### Lire et parser un document XML

Pour lire un XML existant :

```pascal
procedure TForm1.LireXML;  
var  
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
  Nom, Prenom, Age: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;

    // Charger depuis une chaîne
    XMLDoc.LoadFromXML(Memo1.Text);

    // Ou charger depuis un fichier
    // XMLDoc.LoadFromFile('donnees.xml');

    // Obtenir le nœud racine
    RootNode := XMLDoc.DocumentElement;

    // Lire les valeurs
    Nom := RootNode.ChildNodes['nom'].Text;
    Prenom := RootNode.ChildNodes['prenom'].Text;
    Age := RootNode.ChildNodes['age'].Text;

    ShowMessage(Format('Nom: %s %s, Age: %s', [Prenom, Nom, Age]));

  finally
    XMLDoc := nil;
  end;
end;
```

### Parcourir les nœuds XML

Pour naviguer dans la structure :

```pascal
procedure TForm1.ParcoururXML;  
var  
  XMLDoc: IXMLDocument;
  RootNode, AdresseNode, CompetencesNode, Node: IXMLNode;
  i: Integer;
  Ville, Competence: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile('personne.xml');
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    // Accéder à un élément imbriqué
    AdresseNode := RootNode.ChildNodes['adresse'];
    if Assigned(AdresseNode) then
    begin
      Ville := AdresseNode.ChildNodes['ville'].Text;
      Memo1.Lines.Add('Ville: ' + Ville);
    end;

    // Parcourir une liste d'éléments
    CompetencesNode := RootNode.ChildNodes['competences'];
    if Assigned(CompetencesNode) then
    begin
      Memo1.Lines.Add('Compétences:');

      for i := 0 to CompetencesNode.ChildNodes.Count - 1 do
      begin
        Node := CompetencesNode.ChildNodes[i];
        if Node.NodeName = 'competence' then
        begin
          Competence := Node.Text;
          Memo1.Lines.Add('- ' + Competence);
        end;
      end;
    end;

  finally
    XMLDoc := nil;
  end;
end;
```

### Lire et écrire des attributs

Les attributs XML sont des métadonnées attachées aux éléments :

```pascal
procedure TForm1.ManipulerAttributs;  
var  
  XMLDoc: IXMLDocument;
  PersonneNode: IXMLNode;
  ID, TypePersonne: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;

    // Créer un élément avec attributs
    PersonneNode := XMLDoc.AddChild('personne');
    PersonneNode.Attributes['id'] := '123';
    PersonneNode.Attributes['type'] := 'employe';
    PersonneNode.Text := 'Dubois Marie';

    // Lire les attributs
    ID := PersonneNode.Attributes['id'];
    TypePersonne := PersonneNode.Attributes['type'];

    ShowMessage(Format('ID: %s, Type: %s', [ID, TypePersonne]));

    // Vérifier si un attribut existe
    if PersonneNode.HasAttribute('id') then
      ShowMessage('L''attribut id existe');

  finally
    XMLDoc := nil;
  end;
end;
```

### Rechercher des nœuds avec XPath

XPath permet de rechercher des éléments dans un document XML :

```pascal
procedure TForm1.RechercherAvecXPath;  
var  
  XMLDoc: IXMLDocument;
  NodeList: IXMLNodeList;
  Node: IXMLNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile('personnes.xml');
    XMLDoc.Active := True;

    // Rechercher tous les éléments "nom"
    NodeList := XMLDoc.DocumentElement.ChildNodes.FindNode('nom');

    // Parcourir les résultats
    Memo1.Lines.Clear;
    for i := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[i];
      Memo1.Lines.Add('Nom trouvé: ' + Node.Text);
    end;

  finally
    XMLDoc := nil;
  end;
end;
```

### Modifier un document XML

Pour mettre à jour des valeurs :

```pascal
procedure TForm1.ModifierXML;  
var  
  XMLDoc: IXMLDocument;
  RootNode, AgeNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile('personne.xml');
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    // Modifier une valeur existante
    AgeNode := RootNode.ChildNodes['age'];
    if Assigned(AgeNode) then
      AgeNode.Text := '29';

    // Ajouter un nouvel élément
    RootNode.AddChild('telephone').Text := '0612345678';

    // Supprimer un élément
    RootNode.ChildNodes.Remove(RootNode.ChildNodes['actif']);

    // Sauvegarder les modifications
    XMLDoc.SaveToFile('personne.xml');

    ShowMessage('XML modifié');

  finally
    XMLDoc := nil;
  end;
end;
```

### Sauvegarder et charger XML

Gestion des fichiers XML :

```pascal
procedure TForm1.SauvegarderXML;  
var  
  XMLDoc: IXMLDocument;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    // Créer la structure
    with XMLDoc.AddChild('configuration') do
    begin
      AddChild('serveur').Text := 'localhost';
      AddChild('port').Text := '3306';
      AddChild('baseDeDonnees').Text := 'mabase';
    end;

    // Sauvegarder dans un fichier
    XMLDoc.SaveToFile('config.xml');

    ShowMessage('Configuration sauvegardée');

  finally
    XMLDoc := nil;
  end;
end;

procedure TForm1.ChargerXML;  
var  
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
  Serveur, Port, Base: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile('config.xml');
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    Serveur := RootNode.ChildNodes['serveur'].Text;
    Port := RootNode.ChildNodes['port'].Text;
    Base := RootNode.ChildNodes['baseDeDonnees'].Text;

    ShowMessage(Format('Serveur: %s:%s, Base: %s', [Serveur, Port, Base]));

  finally
    XMLDoc := nil;
  end;
end;
```

### Formater le XML (indentation)

Pour un XML plus lisible :

```pascal
procedure TForm1.FormaterXML;  
var  
  XMLDoc: IXMLDocument;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';

    // Par défaut, le XML est indenté
    XMLDoc.Options := XMLDoc.Options + [doNodeAutoIndent];

    with XMLDoc.AddChild('racine') do
    begin
      AddChild('element1').Text := 'valeur1';
      AddChild('element2').Text := 'valeur2';
    end;

    XMLDoc.SaveToXML(Memo1.Lines);

  finally
    XMLDoc := nil;
  end;
end;
```

## Comparaison JSON vs XML

### Avantages du JSON

**Simplicité :**
```json
{"nom": "Dubois", "age": 28}
```

**Léger :** Moins de caractères, fichiers plus petits

**Natif pour le Web :** Directement compatible avec JavaScript

**Facile à lire :** Structure claire et concise

### Avantages du XML

**Richesse :**
```xml
<personne id="123" type="employe">
  <nom>Dubois</nom>
  <age unite="ans">28</age>
</personne>
```

**Attributs :** Permet d'ajouter des métadonnées

**Validation :** Schémas XML (XSD) pour valider la structure

**Standards établis :** SOAP, RSS, SVG, etc.

**Commentaires :** Possibilité d'ajouter des commentaires

### Tableau comparatif

| Critère | JSON | XML |
|---------|------|-----|
| **Taille** | Plus petit | Plus grand |
| **Lisibilité** | Très bonne | Bonne |
| **Vitesse de parsing** | Rapide | Plus lent |
| **Types de données** | Limité (string, number, boolean, array, object, null) | Tout est texte |
| **Attributs** | Non | Oui |
| **Commentaires** | Non | Oui |
| **Namespaces** | Non | Oui |
| **Validation** | JSON Schema | XSD |
| **Popularité Web** | Très élevée | Moyenne |
| **Systèmes entreprise** | Croissante | Très établie |

## Conversion entre JSON et XML

### Convertir JSON vers XML

```pascal
procedure TForm1.JSONVersXML;  
var  
  JSONObject: TJSONObject;
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
  Nom, Prenom: string;
  Age: Integer;
begin
  // Créer le JSON
  JSONObject := TJSONObject.ParseJSONValue(
    '{"nom":"Dubois","prenom":"Marie","age":28}'
  ) as TJSONObject;

  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;

    // Créer la structure XML
    RootNode := XMLDoc.AddChild('personne');

    // Transférer les données
    Nom := JSONObject.GetValue<string>('nom');
    Prenom := JSONObject.GetValue<string>('prenom');
    Age := JSONObject.GetValue<Integer>('age');

    RootNode.AddChild('nom').Text := Nom;
    RootNode.AddChild('prenom').Text := Prenom;
    RootNode.AddChild('age').Text := IntToStr(Age);

    // Afficher le XML
    XMLDoc.SaveToXML(Memo1.Lines);

  finally
    JSONObject.Free;
    XMLDoc := nil;
  end;
end;
```

### Convertir XML vers JSON

```pascal
procedure TForm1.XMLVersJSON;  
var  
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
  JSONObject: TJSONObject;
  Nom, Prenom, Age: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  JSONObject := TJSONObject.Create;
  try
    // Charger le XML
    XMLDoc.LoadFromFile('personne.xml');
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    // Extraire les valeurs
    Nom := RootNode.ChildNodes['nom'].Text;
    Prenom := RootNode.ChildNodes['prenom'].Text;
    Age := RootNode.ChildNodes['age'].Text;

    // Créer le JSON
    JSONObject.AddPair('nom', Nom);
    JSONObject.AddPair('prenom', Prenom);
    JSONObject.AddPair('age', TJSONNumber.Create(StrToInt(Age)));

    // Afficher le JSON
    Memo1.Text := JSONObject.Format(2);

  finally
    XMLDoc := nil;
    JSONObject.Free;
  end;
end;
```

## Gestion des erreurs

### Erreurs JSON courantes

```pascal
procedure TForm1.GererErreursJSON;  
var  
  JSONTexte: string;
  JSONValue: TJSONValue;
begin
  JSONTexte := '{"nom":"Dubois","age":28'; // JSON invalide (manque })

  try
    JSONValue := TJSONObject.ParseJSONValue(JSONTexte);
    try
      if Assigned(JSONValue) then
      begin
        // Traitement
        Memo1.Text := JSONValue.ToString;
      end
      else
        ShowMessage('JSON invalide : le parsing a retourné nil');
    finally
      if Assigned(JSONValue) then
        JSONValue.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de parsing JSON : ' + E.Message);
  end;
end;
```

### Erreurs XML courantes

```pascal
procedure TForm1.GererErreursXML;  
var  
  XMLDoc: IXMLDocument;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    try
      XMLDoc.LoadFromFile('fichier_inexistant.xml');
      XMLDoc.Active := True;
    except
      on E: Exception do
      begin
        ShowMessage('Erreur de chargement XML : ' + E.Message);
        Exit;
      end;
    end;

    // Vérifier qu'un nœud existe avant de l'utiliser
    if Assigned(XMLDoc.DocumentElement) then
    begin
      if Assigned(XMLDoc.DocumentElement.ChildNodes.FindNode('nom')) then
        ShowMessage('Nœud trouvé')
      else
        ShowMessage('Nœud "nom" introuvable');
    end;

  finally
    XMLDoc := nil;
  end;
end;
```

## Bonnes pratiques

### Pour JSON

**1. Toujours libérer la mémoire :**
```pascal
JSONObject := TJSONObject.Create;  
try  
  // Utilisation
finally
  JSONObject.Free;
end;
```

**2. Vérifier les valeurs nulles :**
```pascal
if JSONObject.TryGetValue<string>('email', Email) then
  // Email existe
else
  // Email n'existe pas
```

**3. Utiliser Format pour la lisibilité :**
```pascal
JSONFormate := JSONObject.Format(2); // Indentation de 2 espaces
```

**4. Encoder l'UTF-8 pour les fichiers :**
```pascal
Fichier.SaveToFile('donnees.json', TEncoding.UTF8);
```

### Pour XML

**1. Toujours activer le document :**
```pascal
XMLDoc.Active := True;
```

**2. Utiliser les interfaces (pas de Free) :**
```pascal
var
  XMLDoc: IXMLDocument; // Interface
begin
  XMLDoc := TXMLDocument.Create(nil);
  // Pas besoin de Free
end;
```

**3. Vérifier l'existence des nœuds :**
```pascal
if Assigned(RootNode.ChildNodes.FindNode('nom')) then
  // Le nœud existe
```

**4. Définir l'encodage :**
```pascal
XMLDoc.Encoding := 'UTF-8';
```

## Cas d'usage pratiques

### Fichier de configuration JSON

```pascal
procedure TForm1.SauvegarderConfiguration;  
var  
  Config: TJSONObject;
  Serveur: TJSONObject;
begin
  Config := TJSONObject.Create;
  try
    // Informations du serveur
    Serveur := TJSONObject.Create;
    Serveur.AddPair('hote', 'localhost');
    Serveur.AddPair('port', TJSONNumber.Create(3306));
    Serveur.AddPair('utilisateur', 'root');

    Config.AddPair('serveur', Serveur);
    Config.AddPair('debug', TJSONBool.Create(True));

    // Sauvegarder
    with TStringList.Create do
    try
      Text := Config.Format(2);
      SaveToFile('config.json', TEncoding.UTF8);
    finally
      Free;
    end;

  finally
    Config.Free;
  end;
end;
```

### Fichier de configuration XML

```pascal
procedure TForm1.SauvegarderConfigurationXML;  
var  
  XMLDoc: IXMLDocument;
  ConfigNode, ServeurNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    ConfigNode := XMLDoc.AddChild('configuration');

    // Section serveur
    ServeurNode := ConfigNode.AddChild('serveur');
    ServeurNode.AddChild('hote').Text := 'localhost';
    ServeurNode.AddChild('port').Text := '3306';
    ServeurNode.AddChild('utilisateur').Text := 'root';

    // Options
    ConfigNode.AddChild('debug').Text := 'true';

    XMLDoc.SaveToFile('config.xml');

  finally
    XMLDoc := nil;
  end;
end;
```

## Résumé

### JSON - Points clés

✅ **Utilisez JSON pour :**
- Les API REST modernes
- L'échange de données légères
- Les applications web

✅ **Classes principales :**
- `TJSONObject` pour les objets
- `TJSONArray` pour les tableaux
- `ParseJSONValue` pour parser

✅ **N'oubliez pas :**
- Libérer avec `Free`
- Vérifier l'existence des propriétés
- Gérer les exceptions

### XML - Points clés

✅ **Utilisez XML pour :**
- Les documents structurés complexes
- L'interopérabilité avec des systèmes legacy
- Les données nécessitant des métadonnées riches

✅ **Interfaces principales :**
- `IXMLDocument` pour le document
- `IXMLNode` pour les nœuds
- `Active := True` pour activer

✅ **N'oubliez pas :**
- Pas de `Free` (interfaces)
- Vérifier l'existence des nœuds
- Définir l'encodage UTF-8

Avec ces connaissances, vous pouvez maintenant manipuler efficacement JSON et XML dans vos applications Delphi, que ce soit pour consommer des API, gérer des fichiers de configuration ou échanger des données avec d'autres systèmes !

⏭️ [Socket et communications TCP/IP](/10-communication-et-services-reseaux/03-socket-et-communications-tcp-ip.md)
