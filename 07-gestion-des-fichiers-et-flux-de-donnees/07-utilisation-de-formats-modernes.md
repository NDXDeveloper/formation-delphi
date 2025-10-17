🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.7 Utilisation de formats modernes (JSON, XML, YAML)

## Introduction

Les formats de données structurées permettent d'organiser et d'échanger des informations de manière standardisée et lisible. Contrairement aux fichiers binaires, ces formats sont basés sur du texte, ce qui les rend faciles à lire et à déboguer.

**Analogie simple :** Imaginez que vous devez envoyer une recette de cuisine à un ami :
- **Format binaire** : comme écrire en code morse - compact mais illisible
- **Format texte structuré** : comme écrire avec des titres, des listes numérotées - facile à lire et à comprendre

Dans ce chapitre, nous allons explorer les trois formats modernes les plus utilisés : JSON, XML et YAML.

## Pourquoi utiliser des formats structurés ?

### Avantages

1. **Lisibilité** : Facilement compréhensibles par les humains
2. **Universalité** : Supportés par tous les langages de programmation
3. **Flexibilité** : Peuvent représenter des structures complexes
4. **Débogage** : Faciles à inspecter et corriger
5. **Échange de données** : Standard pour les API et services web

### Cas d'utilisation courants

- **Configuration d'applications** : Paramètres, préférences
- **API REST** : Communication entre client et serveur
- **Stockage de données** : Base de données simple
- **Import/Export** : Échange de données entre systèmes
- **Messages** : Communication entre applications

---

## JSON (JavaScript Object Notation)

JSON est le format le plus populaire aujourd'hui. Simple, léger et très largement supporté.

### Structure de base

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 30,
  "actif": true,
  "adresse": {
    "rue": "123 Rue de la Paix",
    "ville": "Paris",
    "codePostal": "75001"
  },
  "langues": ["français", "anglais", "espagnol"]
}
```

**Éléments JSON :**
- **Objets** : Entre accolades `{}`
- **Tableaux** : Entre crochets `[]`
- **Chaînes** : Entre guillemets doubles `"texte"`
- **Nombres** : Sans guillemets `42`, `3.14`
- **Booléens** : `true` ou `false`
- **Null** : `null`

### Lire JSON en Delphi

```pascal
uses
  System.JSON, System.SysUtils;

procedure LireJSONSimple;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Nom, Prenom: string;
  Age: Integer;
begin
  // Chaîne JSON
  JSONString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  // Parser le JSON
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Extraire les valeurs
      Nom := JSONObject.GetValue<string>('nom');
      Prenom := JSONObject.GetValue<string>('prenom');
      Age := JSONObject.GetValue<Integer>('age');

      ShowMessage(Format('%s %s, %d ans', [Prenom, Nom, Age]));
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Lire JSON depuis un fichier

```pascal
procedure LireJSONDepuisFichier(const NomFichier: string);
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  // Lire le fichier
  JSONString := TFile.ReadAllText(NomFichier, TEncoding.UTF8);

  // Parser
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Utiliser les données
      Memo1.Lines.Add('Nom : ' + JSONObject.GetValue<string>('nom'));
      Memo1.Lines.Add('Prénom : ' + JSONObject.GetValue<string>('prenom'));
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Créer JSON en Delphi

```pascal
procedure CreerJSON;
var
  JSONObject: TJSONObject;
  JSONString: string;
begin
  JSONObject := TJSONObject.Create;
  try
    // Ajouter des paires clé/valeur
    JSONObject.AddPair('nom', 'Martin');
    JSONObject.AddPair('prenom', 'Marie');
    JSONObject.AddPair('age', TJSONNumber.Create(28));
    JSONObject.AddPair('actif', TJSONBool.Create(True));

    // Convertir en chaîne
    JSONString := JSONObject.ToString;

    // Afficher
    Memo1.Lines.Text := JSONString;

    // Sauvegarder dans un fichier
    TFile.WriteAllText('personne.json', JSONString, TEncoding.UTF8);
  finally
    JSONObject.Free;
  end;
end;
```

### Travailler avec des objets imbriqués

```pascal
procedure CreerJSONImbrique;
var
  JSONObject, AdresseObject: TJSONObject;
  JSONString: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('nom', 'Lambert');
    JSONObject.AddPair('prenom', 'Sophie');

    // Créer un objet imbriqué pour l'adresse
    AdresseObject := TJSONObject.Create;
    AdresseObject.AddPair('rue', '456 Avenue des Champs');
    AdresseObject.AddPair('ville', 'Lyon');
    AdresseObject.AddPair('codePostal', '69001');

    // Ajouter l'objet adresse
    JSONObject.AddPair('adresse', AdresseObject);

    // Formater joliment (avec indentation)
    JSONString := JSONObject.Format;
    Memo1.Lines.Text := JSONString;
  finally
    JSONObject.Free;
  end;
end;
```

Résultat :
```json
{
  "nom": "Lambert",
  "prenom": "Sophie",
  "adresse": {
    "rue": "456 Avenue des Champs",
    "ville": "Lyon",
    "codePostal": "69001"
  }
}
```

### Travailler avec des tableaux JSON

```pascal
procedure CreerJSONAvecTableau;
var
  JSONObject: TJSONObject;
  LanguesArray: TJSONArray;
  JSONString: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('nom', 'Rousseau');

    // Créer un tableau
    LanguesArray := TJSONArray.Create;
    LanguesArray.Add('français');
    LanguesArray.Add('anglais');
    LanguesArray.Add('espagnol');

    JSONObject.AddPair('langues', LanguesArray);

    JSONString := JSONObject.Format;
    Memo1.Lines.Text := JSONString;
  finally
    JSONObject.Free;
  end;
end;

// Lire un tableau JSON
procedure LireTableauJSON;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  LanguesArray: TJSONArray;
  i: Integer;
begin
  JSONString := '{"nom":"Rousseau","langues":["français","anglais","espagnol"]}';

  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      // Obtenir le tableau
      LanguesArray := JSONObject.GetValue<TJSONArray>('langues');

      // Parcourir le tableau
      Memo1.Lines.Add('Langues parlées :');
      for i := 0 to LanguesArray.Count - 1 do
        Memo1.Lines.Add('- ' + LanguesArray.Items[i].Value);
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Tableau d'objets JSON

```pascal
procedure CreerTableauObjets;
var
  JSONArray: TJSONArray;
  Personne1, Personne2: TJSONObject;
  JSONString: string;
begin
  JSONArray := TJSONArray.Create;
  try
    // Première personne
    Personne1 := TJSONObject.Create;
    Personne1.AddPair('nom', 'Dupont');
    Personne1.AddPair('age', TJSONNumber.Create(30));
    JSONArray.Add(Personne1);

    // Deuxième personne
    Personne2 := TJSONObject.Create;
    Personne2.AddPair('nom', 'Martin');
    Personne2.AddPair('age', TJSONNumber.Create(25));
    JSONArray.Add(Personne2);

    JSONString := JSONArray.Format;
    Memo1.Lines.Text := JSONString;
  finally
    JSONArray.Free;
  end;
end;
```

Résultat :
```json
[
  {
    "nom": "Dupont",
    "age": 30
  },
  {
    "nom": "Martin",
    "age": 25
  }
]
```

### Gestion des erreurs et validation

```pascal
function LireJSONSecurise(const JSONString: string): Boolean;
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  Result := False;

  if JSONString.IsEmpty then
  begin
    ShowMessage('Chaîne JSON vide');
    Exit;
  end;

  try
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    try
      if not Assigned(JSONValue) then
      begin
        ShowMessage('JSON invalide : impossible de parser');
        Exit;
      end;

      if not (JSONValue is TJSONObject) then
      begin
        ShowMessage('JSON invalide : pas un objet');
        Exit;
      end;

      JSONObject := JSONValue as TJSONObject;

      // Vérifier la présence des champs requis
      if not JSONObject.TryGetValue('nom', JSONValue) then
      begin
        ShowMessage('Champ "nom" manquant');
        Exit;
      end;

      // Tout est OK
      Result := True;
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la lecture JSON : ' + E.Message);
      Result := False;
    end;
  end;
end;
```

### Classe utilitaire pour JSON

```pascal
type
  TJSONHelper = class
    class function ObjectToJSON(const Name, FirstName: string; Age: Integer): string;
    class function JSONToObject(const JSONString: string;
                                out Name, FirstName: string;
                                out Age: Integer): Boolean;
    class function BeautifyJSON(const JSONString: string): string;
    class function MinifyJSON(const JSONString: string): string;
  end;

class function TJSONHelper.ObjectToJSON(const Name, FirstName: string;
                                        Age: Integer): string;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('nom', Name);
    JSONObject.AddPair('prenom', FirstName);
    JSONObject.AddPair('age', TJSONNumber.Create(Age));
    Result := JSONObject.ToString;
  finally
    JSONObject.Free;
  end;
end;

class function TJSONHelper.JSONToObject(const JSONString: string;
                                        out Name, FirstName: string;
                                        out Age: Integer): Boolean;
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  Result := False;

  try
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    try
      if JSONValue is TJSONObject then
      begin
        JSONObject := JSONValue as TJSONObject;
        Name := JSONObject.GetValue<string>('nom');
        FirstName := JSONObject.GetValue<string>('prenom');
        Age := JSONObject.GetValue<Integer>('age');
        Result := True;
      end;
    finally
      JSONValue.Free;
    end;
  except
    Result := False;
  end;
end;

class function TJSONHelper.BeautifyJSON(const JSONString: string): string;
var
  JSONValue: TJSONValue;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if Assigned(JSONValue) then
      Result := JSONValue.Format
    else
      Result := JSONString;
  finally
    JSONValue.Free;
  end;
end;

class function TJSONHelper.MinifyJSON(const JSONString: string): string;
var
  JSONValue: TJSONValue;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if Assigned(JSONValue) then
      Result := JSONValue.ToString
    else
      Result := JSONString;
  finally
    JSONValue.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  JSONString: string;
begin
  // Créer
  JSONString := TJSONHelper.ObjectToJSON('Dubois', 'Pierre', 35);

  // Embellir
  Memo1.Lines.Text := TJSONHelper.BeautifyJSON(JSONString);
end;
```

---

## XML (eXtensible Markup Language)

XML est un format plus ancien mais toujours très utilisé, notamment dans les entreprises et les services web SOAP.

### Structure de base

```xml
<?xml version="1.0" encoding="UTF-8"?>
<personne>
  <nom>Dupont</nom>
  <prenom>Jean</prenom>
  <age>30</age>
  <adresse>
    <rue>123 Rue de la Paix</rue>
    <ville>Paris</ville>
    <codePostal>75001</codePostal>
  </adresse>
  <langues>
    <langue>français</langue>
    <langue>anglais</langue>
    <langue>espagnol</langue>
  </langues>
</personne>
```

**Éléments XML :**
- **Balises** : `<nom>` et `</nom>`
- **Attributs** : `<personne id="123">`
- **Hiérarchie** : Éléments imbriqués
- **Déclaration** : `<?xml version="1.0"?>`

### Lire XML en Delphi

```pascal
uses
  Xml.XMLIntf, Xml.XMLDoc, System.SysUtils;

procedure LireXMLSimple;
var
  XMLDoc: IXMLDocument;
  RootNode, NameNode: IXMLNode;
  Nom, Prenom: string;
  Age: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    // Charger depuis un fichier
    XMLDoc.LoadFromFile('personne.xml');
    XMLDoc.Active := True;

    // Obtenir le nœud racine
    RootNode := XMLDoc.DocumentElement;

    if RootNode.NodeName = 'personne' then
    begin
      // Lire les valeurs
      Nom := RootNode.ChildValues['nom'];
      Prenom := RootNode.ChildValues['prenom'];
      Age := StrToIntDef(RootNode.ChildValues['age'], 0);

      ShowMessage(Format('%s %s, %d ans', [Prenom, Nom, Age]));
    end;
  finally
    XMLDoc := nil;
  end;
end;
```

### Créer XML en Delphi

```pascal
procedure CreerXML;
var
  XMLDoc: IXMLDocument;
  RootNode, AdresseNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    // Créer le nœud racine
    RootNode := XMLDoc.AddChild('personne');

    // Ajouter des éléments
    RootNode.AddChild('nom').Text := 'Martin';
    RootNode.AddChild('prenom').Text := 'Marie';
    RootNode.AddChild('age').Text := '28';

    // Ajouter un élément imbriqué
    AdresseNode := RootNode.AddChild('adresse');
    AdresseNode.AddChild('rue').Text := '456 Avenue des Champs';
    AdresseNode.AddChild('ville').Text := 'Lyon';
    AdresseNode.AddChild('codePostal').Text := '69001';

    // Sauvegarder
    XMLDoc.SaveToFile('personne.xml');

    ShowMessage('Fichier XML créé avec succès');
  finally
    XMLDoc := nil;
  end;
end;
```

### Travailler avec des attributs

```pascal
procedure CreerXMLAvecAttributs;
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;

    RootNode := XMLDoc.AddChild('personnes');

    // Première personne avec attributs
    PersonneNode := RootNode.AddChild('personne');
    PersonneNode.Attributes['id'] := '1';
    PersonneNode.Attributes['actif'] := 'true';
    PersonneNode.AddChild('nom').Text := 'Dupont';
    PersonneNode.AddChild('age').Text := '30';

    // Deuxième personne
    PersonneNode := RootNode.AddChild('personne');
    PersonneNode.Attributes['id'] := '2';
    PersonneNode.Attributes['actif'] := 'false';
    PersonneNode.AddChild('nom').Text := 'Martin';
    PersonneNode.AddChild('age').Text := '25';

    XMLDoc.SaveToFile('personnes.xml');
  finally
    XMLDoc := nil;
  end;
end;
```

Résultat :
```xml
<?xml version="1.0" encoding="UTF-8"?>
<personnes>
  <personne id="1" actif="true">
    <nom>Dupont</nom>
    <age>30</age>
  </personne>
  <personne id="2" actif="false">
    <nom>Martin</nom>
    <age>25</age>
  </personne>
</personnes>
```

### Parcourir tous les nœuds

```pascal
procedure ParcoururXML(const NomFichier: string);
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
  i: Integer;
  ID, Nom: string;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile(NomFichier);
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    Memo1.Lines.Add('Liste des personnes :');
    Memo1.Lines.Add('');

    // Parcourir tous les nœuds enfants
    for i := 0 to RootNode.ChildNodes.Count - 1 do
    begin
      PersonneNode := RootNode.ChildNodes[i];

      if PersonneNode.NodeName = 'personne' then
      begin
        ID := PersonneNode.Attributes['id'];
        Nom := PersonneNode.ChildValues['nom'];

        Memo1.Lines.Add(Format('ID: %s - Nom: %s', [ID, Nom]));
      end;
    end;
  finally
    XMLDoc := nil;
  end;
end;
```

### Rechercher un nœud spécifique

```pascal
function TrouverPersonneParID(const NomFichier: string;
                              const ID: string): string;
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
  i: Integer;
begin
  Result := '';

  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile(NomFichier);
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    for i := 0 to RootNode.ChildNodes.Count - 1 do
    begin
      PersonneNode := RootNode.ChildNodes[i];

      if PersonneNode.Attributes['id'] = ID then
      begin
        Result := PersonneNode.ChildValues['nom'];
        Break;
      end;
    end;
  finally
    XMLDoc := nil;
  end;
end;

// Utilisation
procedure TForm1.Button2Click(Sender: TObject);
var
  Nom: string;
begin
  Nom := TrouverPersonneParID('personnes.xml', '1');
  if Nom <> '' then
    ShowMessage('Trouvé : ' + Nom)
  else
    ShowMessage('Personne non trouvée');
end;
```

### Modifier un XML existant

```pascal
procedure ModifierXML(const NomFichier: string);
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile(NomFichier);
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    // Trouver et modifier la première personne
    for i := 0 to RootNode.ChildNodes.Count - 1 do
    begin
      PersonneNode := RootNode.ChildNodes[i];

      if PersonneNode.Attributes['id'] = '1' then
      begin
        PersonneNode.ChildValues['age'] := '31'; // Modifier l'âge
        Break;
      end;
    end;

    // Sauvegarder les modifications
    XMLDoc.SaveToFile(NomFichier);
    ShowMessage('XML modifié avec succès');
  finally
    XMLDoc := nil;
  end;
end;
```

### Classe utilitaire pour XML

```pascal
type
  TXMLHelper = class
    class function CreatePerson(const Name, FirstName: string;
                               Age: Integer): IXMLDocument;
    class function ReadPersonFromXML(const FileName: string;
                                    out Name, FirstName: string;
                                    out Age: Integer): Boolean;
    class function ValidateXML(const FileName: string): Boolean;
    class function BeautifyXML(const XMLString: string): string;
  end;

class function TXMLHelper.CreatePerson(const Name, FirstName: string;
                                       Age: Integer): IXMLDocument;
var
  RootNode: IXMLNode;
begin
  Result := TXMLDocument.Create(nil);
  Result.Active := True;
  Result.Version := '1.0';
  Result.Encoding := 'UTF-8';

  RootNode := Result.AddChild('personne');
  RootNode.AddChild('nom').Text := Name;
  RootNode.AddChild('prenom').Text := FirstName;
  RootNode.AddChild('age').Text := IntToStr(Age);
end;

class function TXMLHelper.ReadPersonFromXML(const FileName: string;
                                            out Name, FirstName: string;
                                            out Age: Integer): Boolean;
var
  XMLDoc: IXMLDocument;
  RootNode: IXMLNode;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  try
    XMLDoc := TXMLDocument.Create(nil);
    XMLDoc.LoadFromFile(FileName);
    XMLDoc.Active := True;

    RootNode := XMLDoc.DocumentElement;

    Name := RootNode.ChildValues['nom'];
    FirstName := RootNode.ChildValues['prenom'];
    Age := StrToIntDef(RootNode.ChildValues['age'], 0);

    Result := True;
  except
    Result := False;
  end;
end;

class function TXMLHelper.ValidateXML(const FileName: string): Boolean;
var
  XMLDoc: IXMLDocument;
begin
  Result := False;

  try
    XMLDoc := TXMLDocument.Create(nil);
    XMLDoc.LoadFromFile(FileName);
    XMLDoc.Active := True;
    Result := True;
  except
    Result := False;
  end;
end;
```

---

## YAML (YAML Ain't Markup Language)

YAML est un format très lisible, populaire pour les fichiers de configuration. Note : Delphi ne supporte pas YAML nativement, il faut utiliser des bibliothèques tierces ou convertir en JSON/XML.

### Structure de base

```yaml
nom: Dupont
prenom: Jean
age: 30
actif: true
adresse:
  rue: 123 Rue de la Paix
  ville: Paris
  codePostal: 75001
langues:
  - français
  - anglais
  - espagnol
```

**Caractéristiques YAML :**
- **Indentation** : Définit la hiérarchie (comme Python)
- **Pas de balises** : Plus lisible que XML
- **Deux-points** : Séparent clés et valeurs
- **Tiret** : Définit les éléments de liste

### Convertir YAML en JSON

Comme Delphi ne supporte pas nativement YAML, la stratégie est de :
1. Convertir YAML en JSON (avec un outil externe ou une bibliothèque)
2. Lire le JSON avec Delphi

Exemple de conversion (conceptuel) :

```pascal
// Note: Ceci nécessiterait une bibliothèque tierce
function YAMLToJSON(const YAMLString: string): string;
begin
  // Utiliser une bibliothèque comme yaml-delphi
  // ou convertir via un service web
  Result := ''; // Implémentation dépend de la bibliothèque
end;
```

### Alternative : Lire YAML comme texte structuré

Pour des fichiers YAML simples, vous pouvez les parser manuellement :

```pascal
function ParseSimpleYAML(const YAMLContent: string): TDictionary<string, string>;
var
  Lines: TStringList;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
begin
  Result := TDictionary<string, string>.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := YAMLContent;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);

      // Ignorer les lignes vides et commentaires
      if (Line = '') or (Line.StartsWith('#')) then
        Continue;

      ColonPos := Pos(':', Line);
      if ColonPos > 0 then
      begin
        Key := Trim(Copy(Line, 1, ColonPos - 1));
        Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));

        Result.Add(Key, Value);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

// Utilisation
procedure TForm1.LireYAMLSimple;
var
  YAMLContent: string;
  Config: TDictionary<string, string>;
begin
  YAMLContent := TFile.ReadAllText('config.yaml', TEncoding.UTF8);

  Config := ParseSimpleYAML(YAMLContent);
  try
    ShowMessage('Nom : ' + Config['nom']);
    ShowMessage('Age : ' + Config['age']);
  finally
    Config.Free;
  end;
end;
```

**Note importante :** Cette approche simplifiée ne gère pas les structures complexes (listes, objets imbriqués). Pour un support YAML complet, utilisez une bibliothèque tierce.

---

## Comparaison des formats

### Tableau comparatif

| Critère | JSON | XML | YAML |
|---------|------|-----|------|
| **Lisibilité** | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Concision** | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Support Delphi natif** | ✅ Excellent | ✅ Excellent | ❌ Bibliothèques tierces |
| **Performance** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Validation** | Basique | Excellente (XSD) | Basique |
| **Usage API REST** | ✅ Standard | ⚠️ SOAP | ❌ Rare |
| **Configuration** | ⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Commentaires** | ❌ Non | ✅ Oui | ✅ Oui |
| **Types de données** | Limités | Texte | Riches |

### Quand utiliser chaque format ?

**JSON - Choisir pour :**
- API REST et services web modernes
- Communication client-serveur
- Applications JavaScript/web
- Stockage léger de données
- Maximum de compatibilité

**XML - Choisir pour :**
- Services SOAP
- Documents complexes avec métadonnées
- Besoin de validation stricte (XSD)
- Systèmes legacy
- Export de données structurées

**YAML - Choisir pour :**
- Fichiers de configuration
- Scripts d'orchestration (Docker, Kubernetes)
- Maximum de lisibilité
- Documentation avec code

---

## Exemples pratiques complets

### Exemple 1 : Système de configuration JSON

```pascal
type
  TAppConfig = class
  private
    FConfigFile: string;
    FServerURL: string;
    FPort: Integer;
    FUsername: string;
    FAutoSave: Boolean;
    FTheme: string;
    FRecentFiles: TStringList;
  public
    constructor Create(const ConfigFileName: string);
    destructor Destroy; override;

    procedure LoadFromFile;
    procedure SaveToFile;
    procedure SetDefaults;

    property ServerURL: string read FServerURL write FServerURL;
    property Port: Integer read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property Theme: string read FTheme write FTheme;
    property RecentFiles: TStringList read FRecentFiles;
  end;

constructor TAppConfig.Create(const ConfigFileName: string);
begin
  inherited Create;
  FConfigFile := ConfigFileName;
  FRecentFiles := TStringList.Create;

  if FileExists(FConfigFile) then
    LoadFromFile
  else
    SetDefaults;
end;

destructor TAppConfig.Destroy;
begin
  FRecentFiles.Free;
  inherited;
end;

procedure TAppConfig.SetDefaults;
begin
  FServerURL := 'http://localhost';
  FPort := 8080;
  FUsername := '';
  FAutoSave := True;
  FTheme := 'light';
  FRecentFiles.Clear;
end;

procedure TAppConfig.LoadFromFile;
var
  JSONString: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  FilesArray: TJSONArray;
  i: Integer;
begin
  try
    JSONString := TFile.ReadAllText(FConfigFile, TEncoding.UTF8);
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    try
      if JSONValue is TJSONObject then
      begin
        JSONObject := JSONValue as TJSONObject;

        FServerURL := JSONObject.GetValue<string>('serverURL', 'http://localhost');
        FPort := JSONObject.GetValue<Integer>('port', 8080);
        FUsername := JSONObject.GetValue<string>('username', '');
        FAutoSave := JSONObject.GetValue<Boolean>('autoSave', True);
        FTheme := JSONObject.GetValue<string>('theme', 'light');

        // Charger les fichiers récents
        FRecentFiles.Clear;
        if JSONObject.TryGetValue<TJSONArray>('recentFiles', FilesArray) then
        begin
          for i := 0 to FilesArray.Count - 1 do
            FRecentFiles.Add(FilesArray.Items[i].Value);
        end;
      end;
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur, utiliser les valeurs par défaut
      SetDefaults;
    end;
  end;
end;

procedure TAppConfig.SaveToFile;
var
  JSONObject: TJSONObject;
  FilesArray: TJSONArray;
  i: Integer;
  JSONString: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('serverURL', FServerURL);
    JSONObject.AddPair('port', TJSONNumber.Create(FPort));
    JSONObject.AddPair('username', FUsername);
    JSONObject.AddPair('autoSave', TJSONBool.Create(FAutoSave));
    JSONObject.AddPair('theme', FTheme);

    // Sauvegarder les fichiers récents
    FilesArray := TJSONArray.Create;
    for i := 0 to FRecentFiles.Count - 1 do
      FilesArray.Add(FRecentFiles[i]);
    JSONObject.AddPair('recentFiles', FilesArray);

    JSONString := JSONObject.Format;
    TFile.WriteAllText(FConfigFile, JSONString, TEncoding.UTF8);
  finally
    JSONObject.Free;
  end;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  Config := TAppConfig.Create('config.json');

  // Appliquer la configuration
  Edit1.Text := Config.ServerURL;
  SpinEdit1.Value := Config.Port;
  CheckBox1.Checked := Config.AutoSave;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Sauvegarder avant de quitter
  Config.ServerURL := Edit1.Text;
  Config.Port := SpinEdit1.Value;
  Config.AutoSave := CheckBox1.Checked;

  Config.SaveToFile;
  Config.Free;
end;
```

### Exemple 2 : Export de données en XML

```pascal
type
  TDataExporter = class
    class procedure ExportToXML(const FileName: string;
                               Data: TStringList);
    class procedure ExportToJSON(const FileName: string;
                                Data: TStringList);
  end;

class procedure TDataExporter.ExportToXML(const FileName: string;
                                          Data: TStringList);
var
  XMLDoc: IXMLDocument;
  RootNode, RecordNode: IXMLNode;
  i: Integer;
  Fields: TStringList;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    RootNode := XMLDoc.AddChild('records');

    Fields := TStringList.Create;
    try
      for i := 0 to Data.Count - 1 do
      begin
        // Supposons que les données sont séparées par des virgules
        Fields.CommaText := Data[i];

        if Fields.Count >= 3 then
        begin
          RecordNode := RootNode.AddChild('record');
          RecordNode.Attributes['id'] := IntToStr(i + 1);
          RecordNode.AddChild('nom').Text := Fields[0];
          RecordNode.AddChild('prenom').Text := Fields[1];
          RecordNode.AddChild('age').Text := Fields[2];
        end;
      end;
    finally
      Fields.Free;
    end;

    XMLDoc.SaveToFile(FileName);
  finally
    XMLDoc := nil;
  end;
end;

class procedure TDataExporter.ExportToJSON(const FileName: string;
                                           Data: TStringList);
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: Integer;
  Fields: TStringList;
  JSONString: string;
begin
  JSONArray := TJSONArray.Create;
  try
    Fields := TStringList.Create;
    try
      for i := 0 to Data.Count - 1 do
      begin
        Fields.CommaText := Data[i];

        if Fields.Count >= 3 then
        begin
          JSONObject := TJSONObject.Create;
          JSONObject.AddPair('id', TJSONNumber.Create(i + 1));
          JSONObject.AddPair('nom', Fields[0]);
          JSONObject.AddPair('prenom', Fields[1]);
          JSONObject.AddPair('age', TJSONNumber.Create(StrToIntDef(Fields[2], 0)));

          JSONArray.Add(JSONObject);
        end;
      end;
    finally
      Fields.Free;
    end;

    JSONString := JSONArray.Format;
    TFile.WriteAllText(FileName, JSONString, TEncoding.UTF8);
  finally
    JSONArray.Free;
  end;
end;

// Utilisation
procedure TForm1.ExporterDonnees;
var
  Donnees: TStringList;
begin
  Donnees := TStringList.Create;
  try
    Donnees.Add('Dupont,Jean,30');
    Donnees.Add('Martin,Marie,28');
    Donnees.Add('Dubois,Pierre,35');

    // Export XML
    TDataExporter.ExportToXML('export.xml', Donnees);

    // Export JSON
    TDataExporter.ExportToJSON('export.json', Donnees);

    ShowMessage('Données exportées');
  finally
    Donnees.Free;
  end;
end;
```

### Exemple 3 : API REST client

```pascal
uses
  System.Net.HttpClient, System.Net.URLClient;

type
  TAPIClient = class
  private
    FBaseURL: string;
    FHttpClient: THTTPClient;
  public
    constructor Create(const BaseURL: string);
    destructor Destroy; override;

    function GetUser(UserID: Integer): TJSONObject;
    function CreateUser(const Name, Email: string): Boolean;
    function UpdateUser(UserID: Integer; const Name, Email: string): Boolean;
    function DeleteUser(UserID: Integer): Boolean;
  end;

constructor TAPIClient.Create(const BaseURL: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FHttpClient := THTTPClient.Create;
end;

destructor TAPIClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TAPIClient.GetUser(UserID: Integer): TJSONObject;
var
  Response: IHTTPResponse;
  JSONValue: TJSONValue;
  URL: string;
begin
  Result := nil;

  URL := FBaseURL + '/users/' + IntToStr(UserID);

  try
    Response := FHttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONValue := TJSONObject.ParseJSONValue(Response.ContentAsString);
      if JSONValue is TJSONObject then
        Result := JSONValue as TJSONObject
      else
        JSONValue.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur API : ' + E.Message);
  end;
end;

function TAPIClient.CreateUser(const Name, Email: string): Boolean;
var
  JSONObject: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  URL: string;
begin
  Result := False;

  URL := FBaseURL + '/users';

  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', Name);
    JSONObject.AddPair('email', Email);

    RequestBody := TStringStream.Create(JSONObject.ToString, TEncoding.UTF8);
    try
      FHttpClient.ContentType := 'application/json';
      Response := FHttpClient.Post(URL, RequestBody);

      Result := (Response.StatusCode = 201) or (Response.StatusCode = 200);
    finally
      RequestBody.Free;
    end;
  finally
    JSONObject.Free;
  end;
end;

function TAPIClient.UpdateUser(UserID: Integer;
                               const Name, Email: string): Boolean;
var
  JSONObject: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  URL: string;
begin
  Result := False;

  URL := FBaseURL + '/users/' + IntToStr(UserID);

  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', Name);
    JSONObject.AddPair('email', Email);

    RequestBody := TStringStream.Create(JSONObject.ToString, TEncoding.UTF8);
    try
      FHttpClient.ContentType := 'application/json';
      Response := FHttpClient.Put(URL, RequestBody);

      Result := Response.StatusCode = 200;
    finally
      RequestBody.Free;
    end;
  finally
    JSONObject.Free;
  end;
end;

function TAPIClient.DeleteUser(UserID: Integer): Boolean;
var
  Response: IHTTPResponse;
  URL: string;
begin
  URL := FBaseURL + '/users/' + IntToStr(UserID);

  try
    Response := FHttpClient.Delete(URL);
    Result := (Response.StatusCode = 200) or (Response.StatusCode = 204);
  except
    Result := False;
  end;
end;

// Utilisation
procedure TForm1.TestAPI;
var
  API: TAPIClient;
  User: TJSONObject;
begin
  API := TAPIClient.Create('https://api.example.com');
  try
    // Créer un utilisateur
    if API.CreateUser('Jean Dupont', 'jean@example.com') then
      ShowMessage('Utilisateur créé');

    // Récupérer un utilisateur
    User := API.GetUser(1);
    if Assigned(User) then
    try
      Memo1.Lines.Add('Nom : ' + User.GetValue<string>('name'));
      Memo1.Lines.Add('Email : ' + User.GetValue<string>('email'));
    finally
      User.Free;
    end;

    // Mettre à jour
    if API.UpdateUser(1, 'Jean Martin', 'jean.martin@example.com') then
      ShowMessage('Utilisateur mis à jour');

    // Supprimer
    if API.DeleteUser(1) then
      ShowMessage('Utilisateur supprimé');
  finally
    API.Free;
  end;
end;
```

---

## Bonnes pratiques

### 1. Toujours valider les données

```pascal
function ValidateJSON(const JSONString: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  Result := False;

  if JSONString.Trim.IsEmpty then
    Exit;

  try
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    try
      Result := Assigned(JSONValue);
    finally
      JSONValue.Free;
    end;
  except
    Result := False;
  end;
end;
```

### 2. Utiliser des valeurs par défaut

```pascal
function GetJSONValue(JSONObject: TJSONObject; const Key: string;
                      const DefaultValue: string): string;
begin
  if not JSONObject.TryGetValue<string>(Key, Result) then
    Result := DefaultValue;
end;
```

### 3. Gérer l'encodage correctement

```pascal
// Toujours utiliser UTF-8 pour JSON et XML
TFile.WriteAllText(FileName, JSONString, TEncoding.UTF8);
```

### 4. Libérer la mémoire

```pascal
var
  JSONValue: TJSONValue;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    // Utiliser JSONValue
  finally
    JSONValue.Free; // TOUJOURS libérer !
  end;
end;
```

### 5. Logger les erreurs de parsing

```pascal
procedure ParseJSONWithLogging(const JSONString: string);
var
  JSONValue: TJSONValue;
begin
  try
    JSONValue := TJSONObject.ParseJSONValue(JSONString);
    try
      if not Assigned(JSONValue) then
        LogError('JSON invalide : impossible de parser')
      else
        // Traiter le JSON
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
      LogError('Erreur parsing JSON : ' + E.Message);
  end;
end;
```

### 6. Versionner vos formats

```pascal
const
  CONFIG_VERSION = 2;

procedure SaveConfigWithVersion;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('version', TJSONNumber.Create(CONFIG_VERSION));
    JSONObject.AddPair('data', '...');
    // ...
  finally
    JSONObject.Free;
  end;
end;

procedure LoadConfigWithVersion;
var
  JSONObject: TJSONObject;
  Version: Integer;
begin
  // ...
  Version := JSONObject.GetValue<Integer>('version', 1);

  case Version of
    1: LoadConfigV1(JSONObject);
    2: LoadConfigV2(JSONObject);
  else
    raise Exception.Create('Version non supportée');
  end;
end;
```

---

## Résumé

Dans ce chapitre, vous avez découvert les formats modernes de données structurées :

**JSON (JavaScript Object Notation) :**
- Format le plus populaire actuellement
- Support natif excellent dans Delphi
- Idéal pour les API REST
- Léger et performant

**XML (eXtensible Markup Language) :**
- Format établi, toujours très utilisé
- Support natif excellent dans Delphi
- Idéal pour documents complexes
- Validation stricte possible

**YAML (YAML Ain't Markup Language) :**
- Format très lisible
- Idéal pour configuration
- Support Delphi via bibliothèques tierces
- Alternative : conversion vers JSON

**Compétences acquises :**
- Lire et écrire JSON/XML
- Parser des données structurées
- Créer des objets et tableaux
- Gérer les erreurs
- Communiquer avec des API
- Exporter des données

**Bonnes pratiques :**
- Toujours valider les données
- Gérer l'encodage (UTF-8)
- Libérer la mémoire
- Versionner les formats
- Logger les erreurs
- Utiliser des valeurs par défaut

Ces formats sont essentiels dans le développement moderne et vous permettront de créer des applications qui communiquent efficacement avec d'autres systèmes !

⏭️ [Manipulation de fichiers CSV et Excel](/07-gestion-des-fichiers-et-flux-de-donnees/08-manipulation-de-fichiers-csv-et-excel.md)
