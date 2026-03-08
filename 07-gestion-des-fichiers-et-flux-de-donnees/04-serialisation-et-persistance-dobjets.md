🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.4 Sérialisation et persistance d'objets

## Introduction

La sérialisation est le processus de conversion d'un objet en mémoire en une forme qui peut être stockée dans un fichier ou transmise sur un réseau. La persistance, quant à elle, consiste à sauvegarder l'état d'un objet pour le récupérer plus tard.

Imaginez que vous avez créé un personnage dans un jeu vidéo avec ses caractéristiques (nom, niveau, points de vie, équipement). La sérialisation permet de transformer toutes ces informations en données stockables, et la persistance permet de les sauvegarder pour retrouver votre personnage exactement tel quel lors de votre prochaine session de jeu.

## Concepts fondamentaux

### Qu'est-ce que la sérialisation ?

**Sérialisation** : Transformer un objet complexe en une séquence d'octets ou en texte structuré.

**Désérialisation** : Recréer un objet à partir des données sérialisées.

**Analogie :** Pensez à un meuble en kit :
- **Sérialisation** = démonter le meuble et le mettre en pièces dans un carton
- **Désérialisation** = remonter le meuble à partir des pièces et des instructions

### Pourquoi sérialiser des objets ?

1. **Sauvegarde de l'état** : Sauvegarder les paramètres d'une application
2. **Persistance** : Stocker des données entre les sessions
3. **Communication** : Envoyer des objets sur un réseau
4. **Copie profonde** : Dupliquer des objets complexes
5. **Annuler/Refaire** : Implémenter l'historique des actions
6. **Cache** : Stocker temporairement des objets

---

## Méthode 1 : Sérialisation manuelle avec Streams

C'est la méthode la plus basique et la plus contrôlée. Vous écrivez explicitement chaque champ dans un stream.

### Exemple simple : Classe TPerson

```pascal
uses
  System.Classes, System.SysUtils;

type
  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FEmail: string;
  public
    constructor Create(const ANom, APrenom: string; AAge: Integer; const AEmail: string);

    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

constructor TPerson.Create(const ANom, APrenom: string; AAge: Integer; const AEmail: string);  
begin  
  inherited Create;
  FNom := ANom;
  FPrenom := APrenom;
  FAge := AAge;
  FEmail := AEmail;
end;

procedure TPerson.SaveToStream(Stream: TStream);
  // Fonction helper pour écrire une chaîne
  procedure WriteString(const S: string);
  var
    Bytes: TBytes;
    Len: Integer;
  begin
    Bytes := TEncoding.UTF8.GetBytes(S);
    Len := Length(Bytes);
    Stream.WriteBuffer(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.WriteBuffer(Bytes[0], Len);
  end;
begin
  // Écrire un en-tête de version
  Stream.WriteBuffer(Integer(1), SizeOf(Integer)); // Version 1

  // Écrire les champs
  WriteString(FNom);
  WriteString(FPrenom);
  Stream.WriteBuffer(FAge, SizeOf(Integer));
  WriteString(FEmail);
end;

procedure TPerson.LoadFromStream(Stream: TStream);
  // Fonction helper pour lire une chaîne
  function ReadString: string;
  var
    Bytes: TBytes;
    Len: Integer;
  begin
    Stream.ReadBuffer(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.ReadBuffer(Bytes[0], Len);
      Result := TEncoding.UTF8.GetString(Bytes);
    end
    else
      Result := '';
  end;
var
  Version: Integer;
begin
  // Lire l'en-tête de version
  Stream.ReadBuffer(Version, SizeOf(Integer));

  if Version <> 1 then
    raise Exception.Create('Version de fichier non supportée');

  // Lire les champs
  FNom := ReadString;
  FPrenom := ReadString;
  Stream.ReadBuffer(FAge, SizeOf(Integer));
  FEmail := ReadString;
end;

procedure TPerson.SaveToFile(const FileName: string);  
var  
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPerson.LoadFromFile(const FileName: string);  
var  
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Person: TPerson;
begin
  // Créer et sauvegarder
  Person := TPerson.Create('Dupont', 'Jean', 30, 'jean@email.com');
  try
    Person.SaveToFile('personne.dat');
    ShowMessage('Personne sauvegardée');
  finally
    Person.Free;
  end;

  // Charger
  Person := TPerson.Create('', '', 0, '');
  try
    Person.LoadFromFile('personne.dat');
    ShowMessage(Format('%s %s, %d ans, %s',
      [Person.Prenom, Person.Nom, Person.Age, Person.Email]));
  finally
    Person.Free;
  end;
end;
```

### Avantages et inconvénients

**Avantages :**
- Contrôle total sur le format
- Très compact (format binaire)
- Rapide
- Indépendant des frameworks externes

**Inconvénients :**
- Beaucoup de code à écrire
- Difficile à maintenir si la classe évolue
- Format binaire non lisible
- Gestion manuelle des versions

---

## Méthode 2 : Sérialisation JSON

JSON (JavaScript Object Notation) est un format texte léger, lisible et universellement supporté.

### Utilisation de System.JSON

```pascal
uses
  System.JSON, System.Classes, System.SysUtils;

type
  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FEmail: string;
  public
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    function ToJSONString: string;
    procedure FromJSONString(const JSONStr: string);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

function TPerson.ToJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('nom', FNom);
  Result.AddPair('prenom', FPrenom);
  Result.AddPair('age', TJSONNumber.Create(FAge));
  Result.AddPair('email', FEmail);
end;

procedure TPerson.FromJSON(JSONObj: TJSONObject);  
var  
  Pair: TJSONPair;
begin
  Pair := JSONObj.Get('nom');
  if Assigned(Pair) then
    FNom := Pair.JsonValue.Value;

  Pair := JSONObj.Get('prenom');
  if Assigned(Pair) then
    FPrenom := Pair.JsonValue.Value;

  Pair := JSONObj.Get('age');
  if Assigned(Pair) then
    FAge := StrToIntDef(Pair.JsonValue.Value, 0);

  Pair := JSONObj.Get('email');
  if Assigned(Pair) then
    FEmail := Pair.JsonValue.Value;
end;

function TPerson.ToJSONString: string;  
var  
  JSONObj: TJSONObject;
begin
  JSONObj := ToJSON;
  try
    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TPerson.FromJSONString(const JSONStr: string);  
var  
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  if Assigned(JSONObj) then
  try
    FromJSON(JSONObj);
  finally
    JSONObj.Free;
  end;
end;

procedure TPerson.SaveToFile(const FileName: string);  
var  
  JSONStr: string;
begin
  JSONStr := ToJSONString;
  TFile.WriteAllText(FileName, JSONStr, TEncoding.UTF8);
end;

procedure TPerson.LoadFromFile(const FileName: string);  
var  
  JSONStr: string;
begin
  JSONStr := TFile.ReadAllText(FileName, TEncoding.UTF8);
  FromJSONString(JSONStr);
end;

// Utilisation
procedure TForm1.Button2Click(Sender: TObject);  
var  
  Person: TPerson;
  JSONStr: string;
begin
  Person := TPerson.Create;
  try
    // Créer et sauvegarder
    Person.Nom := 'Martin';
    Person.Prenom := 'Marie';
    Person.Age := 28;
    Person.Email := 'marie@email.com';

    Person.SaveToFile('personne.json');

    // Afficher le JSON
    JSONStr := Person.ToJSONString;
    Memo1.Lines.Text := JSONStr;

    // Réinitialiser et charger
    Person.Nom := '';
    Person.Age := 0;

    Person.LoadFromFile('personne.json');
    ShowMessage(Format('%s %s, %d ans',
      [Person.Prenom, Person.Nom, Person.Age]));
  finally
    Person.Free;
  end;
end;
```

Le fichier JSON résultant sera lisible :
```json
{
  "nom": "Martin",
  "prenom": "Marie",
  "age": 28,
  "email": "marie@email.com"
}
```

---

## Méthode 3 : Sérialisation XML

XML est un autre format texte structuré, particulièrement adapté aux documents complexes.

### Utilisation de Xml.XMLIntf et Xml.XMLDoc

```pascal
uses
  System.SysUtils, System.Classes,
  Xml.XMLIntf, Xml.XMLDoc;

type
  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FEmail: string;
  public
    function ToXML: IXMLDocument;
    procedure FromXML(XMLDoc: IXMLDocument);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

function TPerson.ToXML: IXMLDocument;  
var  
  RootNode, PersonNode: IXMLNode;
begin
  Result := TXMLDocument.Create(nil);
  Result.Active := True;
  Result.Version := '1.0';
  Result.Encoding := 'UTF-8';

  RootNode := Result.AddChild('person');
  RootNode.AddChild('nom').Text := FNom;
  RootNode.AddChild('prenom').Text := FPrenom;
  RootNode.AddChild('age').Text := IntToStr(FAge);
  RootNode.AddChild('email').Text := FEmail;
end;

procedure TPerson.FromXML(XMLDoc: IXMLDocument);  
var  
  RootNode: IXMLNode;
begin
  RootNode := XMLDoc.DocumentElement;

  if RootNode.NodeName = 'person' then
  begin
    FNom := RootNode.ChildValues['nom'];
    FPrenom := RootNode.ChildValues['prenom'];
    FAge := StrToIntDef(RootNode.ChildValues['age'], 0);
    FEmail := RootNode.ChildValues['email'];
  end;
end;

procedure TPerson.SaveToFile(const FileName: string);  
var  
  XMLDoc: IXMLDocument;
begin
  XMLDoc := ToXML;
  XMLDoc.SaveToFile(FileName);
end;

procedure TPerson.LoadFromFile(const FileName: string);  
var  
  XMLDoc: IXMLDocument;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromFile(FileName);
  XMLDoc.Active := True;
  FromXML(XMLDoc);
end;

// Utilisation
procedure TForm1.Button3Click(Sender: TObject);  
var  
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.Nom := 'Dubois';
    Person.Prenom := 'Pierre';
    Person.Age := 35;
    Person.Email := 'pierre@email.com';

    Person.SaveToFile('personne.xml');
    ShowMessage('Sauvegardé en XML');

    // Réinitialiser et charger
    Person.Nom := '';
    Person.LoadFromFile('personne.xml');
    ShowMessage('Chargé : ' + Person.Nom);
  finally
    Person.Free;
  end;
end;
```

Le fichier XML résultant :
```xml
<?xml version="1.0" encoding="UTF-8"?>
<person>
  <nom>Dubois</nom>
  <prenom>Pierre</prenom>
  <age>35</age>
  <email>pierre@email.com</email>
</person>
```

---

## Sérialisation d'objets complexes

### Objets imbriqués

```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
  public
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    property Rue: string read FRue write FRue;
    property Ville: string read FVille write FVille;
    property CodePostal: string read FCodePostal write FCodePostal;
  end;

  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FAdresse: TAdresse;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Adresse: TAdresse read FAdresse;
  end;

constructor TPerson.Create;  
begin  
  inherited;
  FAdresse := TAdresse.Create;
end;

destructor TPerson.Destroy;  
begin  
  FAdresse.Free;
  inherited;
end;

function TAdresse.ToJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('rue', FRue);
  Result.AddPair('ville', FVille);
  Result.AddPair('codePostal', FCodePostal);
end;

procedure TAdresse.FromJSON(JSONObj: TJSONObject);  
begin  
  FRue := JSONObj.GetValue<string>('rue');
  FVille := JSONObj.GetValue<string>('ville');
  FCodePostal := JSONObj.GetValue<string>('codePostal');
end;

function TPerson.ToJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('nom', FNom);
  Result.AddPair('prenom', FPrenom);
  Result.AddPair('age', TJSONNumber.Create(FAge));
  Result.AddPair('adresse', FAdresse.ToJSON);
end;

procedure TPerson.FromJSON(JSONObj: TJSONObject);  
var  
  AdresseObj: TJSONObject;
begin
  FNom := JSONObj.GetValue<string>('nom');
  FPrenom := JSONObj.GetValue<string>('prenom');
  FAge := JSONObj.GetValue<Integer>('age');

  AdresseObj := JSONObj.GetValue<TJSONObject>('adresse');
  if Assigned(AdresseObj) then
    FAdresse.FromJSON(AdresseObj);
end;

// Utilisation
var
  Person: TPerson;
  JSONStr: string;
begin
  Person := TPerson.Create;
  try
    Person.Nom := 'Lambert';
    Person.Prenom := 'Sophie';
    Person.Age := 32;
    Person.Adresse.Rue := '123 Rue de la Paix';
    Person.Adresse.Ville := 'Paris';
    Person.Adresse.CodePostal := '75001';

    var JSONObj := Person.ToJSON;
    try
      JSONStr := JSONObj.ToString;
      Memo1.Text := JSONStr;
    finally
      JSONObj.Free;
    end;
  finally
    Person.Free;
  end;
end;
```

JSON résultant :
```json
{
  "nom": "Lambert",
  "prenom": "Sophie",
  "age": 32,
  "adresse": {
    "rue": "123 Rue de la Paix",
    "ville": "Paris",
    "codePostal": "75001"
  }
}
```

### Listes d'objets

```pascal
type
  TPersonList = class
  private
    FPersons: TObjectList<TPerson>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Person: TPerson);
    function Count: Integer;
    function GetPerson(Index: Integer): TPerson;

    function ToJSON: TJSONArray;
    procedure FromJSON(JSONArray: TJSONArray);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

constructor TPersonList.Create;  
begin  
  inherited;
  FPersons := TObjectList<TPerson>.Create(True); // True = possède les objets
end;

destructor TPersonList.Destroy;  
begin  
  FPersons.Free;
  inherited;
end;

procedure TPersonList.Add(Person: TPerson);  
begin  
  FPersons.Add(Person);
end;

function TPersonList.Count: Integer;  
begin  
  Result := FPersons.Count;
end;

function TPersonList.GetPerson(Index: Integer): TPerson;  
begin  
  Result := FPersons[Index];
end;

function TPersonList.ToJSON: TJSONArray;  
var  
  Person: TPerson;
begin
  Result := TJSONArray.Create;
  for Person in FPersons do
    Result.AddElement(Person.ToJSON);
end;

procedure TPersonList.FromJSON(JSONArray: TJSONArray);  
var  
  i: Integer;
  JSONObj: TJSONObject;
  Person: TPerson;
begin
  FPersons.Clear;

  for i := 0 to JSONArray.Count - 1 do
  begin
    JSONObj := JSONArray.Items[i] as TJSONObject;
    Person := TPerson.Create;
    Person.FromJSON(JSONObj);
    FPersons.Add(Person);
  end;
end;

procedure TPersonList.SaveToFile(const FileName: string);  
var  
  JSONArray: TJSONArray;
  JSONStr: string;
begin
  JSONArray := ToJSON;
  try
    JSONStr := JSONArray.ToString;
    TFile.WriteAllText(FileName, JSONStr, TEncoding.UTF8);
  finally
    JSONArray.Free;
  end;
end;

procedure TPersonList.LoadFromFile(const FileName: string);  
var  
  JSONStr: string;
  JSONArray: TJSONArray;
begin
  JSONStr := TFile.ReadAllText(FileName, TEncoding.UTF8);
  JSONArray := TJSONObject.ParseJSONValue(JSONStr) as TJSONArray;
  if Assigned(JSONArray) then
  try
    FromJSON(JSONArray);
  finally
    JSONArray.Free;
  end;
end;

// Utilisation
var
  PersonList: TPersonList;
  Person: TPerson;
  i: Integer;
begin
  PersonList := TPersonList.Create;
  try
    // Ajouter des personnes
    Person := TPerson.Create;
    Person.Nom := 'Dupont';
    Person.Prenom := 'Jean';
    Person.Age := 30;
    PersonList.Add(Person);

    Person := TPerson.Create;
    Person.Nom := 'Martin';
    Person.Prenom := 'Marie';
    Person.Age := 28;
    PersonList.Add(Person);

    // Sauvegarder
    PersonList.SaveToFile('personnes.json');

    // Recharger
    PersonList.LoadFromFile('personnes.json');

    // Afficher
    for i := 0 to PersonList.Count - 1 do
      Memo1.Lines.Add(PersonList.GetPerson(i).Nom);
  finally
    PersonList.Free;
  end;
end;
```

---

## Gestion des versions

Lors de l'évolution de vos classes, il est important de gérer la compatibilité avec les anciennes versions.

### Stratégie de versioning

```pascal
type
  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FEmail: string;
    FTelephone: string; // Nouveau champ ajouté en version 2
  public
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
    property Telephone: string read FTelephone write FTelephone;
  end;

const
  PERSON_VERSION = 2;

function TPerson.ToJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('version', TJSONNumber.Create(PERSON_VERSION));
  Result.AddPair('nom', FNom);
  Result.AddPair('prenom', FPrenom);
  Result.AddPair('age', TJSONNumber.Create(FAge));
  Result.AddPair('email', FEmail);
  Result.AddPair('telephone', FTelephone); // Nouveau champ
end;

procedure TPerson.FromJSON(JSONObj: TJSONObject);  
var  
  Version: Integer;
  Pair: TJSONPair;
begin
  // Lire la version (1 par défaut si absente)
  Pair := JSONObj.Get('version');
  if Assigned(Pair) then
    Version := StrToIntDef(Pair.JsonValue.Value, 1)
  else
    Version := 1;

  // Champs communs à toutes les versions
  FNom := JSONObj.GetValue<string>('nom');
  FPrenom := JSONObj.GetValue<string>('prenom');
  FAge := JSONObj.GetValue<Integer>('age');
  FEmail := JSONObj.GetValue<string>('email');

  // Champ ajouté en version 2
  if Version >= 2 then
  begin
    Pair := JSONObj.Get('telephone');
    if Assigned(Pair) then
      FTelephone := Pair.JsonValue.Value
    else
      FTelephone := ''; // Valeur par défaut
  end
  else
    FTelephone := ''; // Version 1 n'avait pas ce champ
end;
```

---

## Utilisation du RTTI (Run-Time Type Information)

Le RTTI permet d'inspecter et de manipuler les types à l'exécution, ce qui peut simplifier la sérialisation.

### Sérialisation automatique avec RTTI

```pascal
uses
  System.Rtti, System.TypInfo;

type
  TPerson = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FEmail: string;
  published
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

function ObjectToJSON(Obj: TObject): TJSONObject;  
var  
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
begin
  Result := TJSONObject.Create;
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    for Prop in RttiType.GetProperties do
    begin
      // Sérialiser uniquement les propriétés published
      if Prop.Visibility = mvPublished then
      begin
        Value := Prop.GetValue(Obj);

        case Prop.PropertyType.TypeKind of
          tkInteger, tkInt64:
            Result.AddPair(Prop.Name, TJSONNumber.Create(Value.AsInteger));
          tkFloat:
            Result.AddPair(Prop.Name, TJSONNumber.Create(Value.AsExtended));
          tkString, tkUString, tkLString, tkWString:
            Result.AddPair(Prop.Name, Value.AsString);
          tkEnumeration:
            begin
              if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                Result.AddPair(Prop.Name, TJSONBool.Create(Value.AsBoolean))
              else
                Result.AddPair(Prop.Name, TJSONNumber.Create(Value.AsOrdinal));
            end;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

procedure JSONToObject(JSONObj: TJSONObject; Obj: TObject);  
var  
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Pair: TJSONPair;
  Value: TValue;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    for Prop in RttiType.GetProperties do
    begin
      if Prop.Visibility = mvPublished then
      begin
        Pair := JSONObj.Get(Prop.Name);
        if Assigned(Pair) then
        begin
          case Prop.PropertyType.TypeKind of
            tkInteger, tkInt64:
              Value := TValue.From<Integer>(StrToIntDef(Pair.JsonValue.Value, 0));
            tkFloat:
              Value := TValue.From<Extended>(StrToFloatDef(Pair.JsonValue.Value, 0));
            tkString, tkUString, tkLString, tkWString:
              Value := TValue.From<string>(Pair.JsonValue.Value);
            tkEnumeration:
              begin
                if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                  Value := TValue.From<Boolean>(StrToBoolDef(Pair.JsonValue.Value, False))
                else
                  Value := TValue.From<Integer>(StrToIntDef(Pair.JsonValue.Value, 0));
              end;
          end;

          if Prop.IsWritable then
            Prop.SetValue(Obj, Value);
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

// Utilisation
var
  Person: TPerson;
  JSONObj: TJSONObject;
  JSONStr: string;
begin
  Person := TPerson.Create;
  try
    Person.Nom := 'Rousseau';
    Person.Prenom := 'Luc';
    Person.Age := 40;
    Person.Email := 'luc@email.com';

    // Sérialisation automatique
    JSONObj := ObjectToJSON(Person);
    try
      JSONStr := JSONObj.ToString;
      ShowMessage(JSONStr);

      // Réinitialiser
      Person.Nom := '';
      Person.Age := 0;

      // Désérialisation automatique
      JSONToObject(JSONObj, Person);
      ShowMessage(Person.Nom); // Affiche 'Rousseau'
    finally
      JSONObj.Free;
    end;
  finally
    Person.Free;
  end;
end;
```

---

## Persistance des composants VCL/FMX

Delphi dispose d'un mécanisme intégré pour sauvegarder et charger des composants.

### Sauvegarder un composant en fichier

```pascal
// Sauvegarder un composant dans un stream
procedure SaveComponentToFile(Component: TComponent; const FileName: string);  
var  
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    // Écrire le composant dans le stream
    MemStream.WriteComponent(Component);

    // Sauvegarder dans un fichier
    MemStream.Position := 0;
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      ObjectBinaryToText(MemStream, FileStream);
    finally
      FileStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

// Charger un composant depuis un fichier
procedure LoadComponentFromFile(Component: TComponent; const FileName: string);  
var  
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    MemStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(FileStream, MemStream);
      MemStream.Position := 0;
      MemStream.ReadComponent(Component);
    finally
      MemStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

// Exemple : sauvegarder les paramètres d'un formulaire
procedure TForm1.SaveFormSettings;  
begin  
  SaveComponentToFile(Self, 'form_settings.txt');
end;

procedure TForm1.LoadFormSettings;  
begin  
  if FileExists('form_settings.txt') then
    LoadComponentFromFile(Self, 'form_settings.txt');
end;
```

### Sauvegarder uniquement certaines propriétés

```pascal
procedure SaveFormPosition(Form: TForm; const FileName: string);  
var  
  Settings: TStringList;
begin
  Settings := TStringList.Create;
  try
    Settings.Add('Left=' + IntToStr(Form.Left));
    Settings.Add('Top=' + IntToStr(Form.Top));
    Settings.Add('Width=' + IntToStr(Form.Width));
    Settings.Add('Height=' + IntToStr(Form.Height));
    Settings.Add('WindowState=' + IntToStr(Ord(Form.WindowState)));

    Settings.SaveToFile(FileName);
  finally
    Settings.Free;
  end;
end;

procedure LoadFormPosition(Form: TForm; const FileName: string);  
var  
  Settings: TStringList;
begin
  if not FileExists(FileName) then
    Exit;

  Settings := TStringList.Create;
  try
    Settings.LoadFromFile(FileName);

    Form.Left := StrToIntDef(Settings.Values['Left'], Form.Left);
    Form.Top := StrToIntDef(Settings.Values['Top'], Form.Top);
    Form.Width := StrToIntDef(Settings.Values['Width'], Form.Width);
    Form.Height := StrToIntDef(Settings.Values['Height'], Form.Height);
    Form.WindowState := TWindowState(StrToIntDef(Settings.Values['WindowState'], 0));
  finally
    Settings.Free;
  end;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  LoadFormPosition(Self, 'window_position.ini');
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  SaveFormPosition(Self, 'window_position.ini');
end;
```

---

## Patterns avancés

### 1. Interface de sérialisation

```pascal
type
  ISerializable = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);
  end;

  TPerson = class(TInterfacedObject, ISerializable)
  private
    FNom: string;
    FAge: Integer;
  public
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
  end;

function TPerson.ToJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('nom', FNom);
  Result.AddPair('age', TJSONNumber.Create(FAge));
end;

procedure TPerson.FromJSON(JSONObj: TJSONObject);  
begin  
  FNom := JSONObj.GetValue<string>('nom');
  FAge := JSONObj.GetValue<Integer>('age');
end;

// Fonction générique
procedure SaveSerializable(Obj: ISerializable; const FileName: string);  
var  
  JSONObj: TJSONObject;
begin
  JSONObj := Obj.ToJSON;
  try
    TFile.WriteAllText(FileName, JSONObj.ToString, TEncoding.UTF8);
  finally
    JSONObj.Free;
  end;
end;
```

### 2. Factory Pattern pour la désérialisation

```pascal
type
  TPersonFactory = class
    class function CreateFromJSON(const JSONStr: string): TPerson;
    class function CreateFromFile(const FileName: string): TPerson;
  end;

class function TPersonFactory.CreateFromJSON(const JSONStr: string): TPerson;  
var  
  JSONObj: TJSONObject;
begin
  Result := TPerson.Create;
  try
    JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
    if Assigned(JSONObj) then
    try
      Result.FromJSON(JSONObj);
    finally
      JSONObj.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TPersonFactory.CreateFromFile(const FileName: string): TPerson;  
var  
  JSONStr: string;
begin
  JSONStr := TFile.ReadAllText(FileName, TEncoding.UTF8);
  Result := CreateFromJSON(JSONStr);
end;

// Utilisation
var
  Person: TPerson;
begin
  Person := TPersonFactory.CreateFromFile('personne.json');
  try
    ShowMessage(Person.Nom);
  finally
    Person.Free;
  end;
end;
```

### 3. Classe de base pour la sérialisation

```pascal
type
  TSerializableObject = class abstract
  protected
    procedure WriteString(Stream: TStream; const S: string);
    function ReadString(Stream: TStream): string;
    procedure WriteInteger(Stream: TStream; Value: Integer);
    function ReadInteger(Stream: TStream): Integer;
  public
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

procedure TSerializableObject.WriteString(Stream: TStream; const S: string);  
var  
  Bytes: TBytes;
  Len: Integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(S);
  Len := Length(Bytes);
  Stream.WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    Stream.WriteBuffer(Bytes[0], Len);
end;

function TSerializableObject.ReadString(Stream: TStream): string;  
var  
  Bytes: TBytes;
  Len: Integer;
begin
  Stream.ReadBuffer(Len, SizeOf(Integer));
  if Len > 0 then
  begin
    SetLength(Bytes, Len);
    Stream.ReadBuffer(Bytes[0], Len);
    Result := TEncoding.UTF8.GetString(Bytes);
  end
  else
    Result := '';
end;

procedure TSerializableObject.WriteInteger(Stream: TStream; Value: Integer);  
begin  
  Stream.WriteBuffer(Value, SizeOf(Integer));
end;

function TSerializableObject.ReadInteger(Stream: TStream): Integer;  
begin  
  Stream.ReadBuffer(Result, SizeOf(Integer));
end;

procedure TSerializableObject.SaveToFile(const FileName: string);  
var  
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TSerializableObject.LoadFromFile(const FileName: string);  
var  
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

// Utilisation
type
  TPerson = class(TSerializableObject)
  private
    FNom: string;
    FAge: Integer;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;

    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
  end;

procedure TPerson.SaveToStream(Stream: TStream);  
begin  
  WriteString(Stream, FNom);
  WriteInteger(Stream, FAge);
end;

procedure TPerson.LoadFromStream(Stream: TStream);  
begin  
  FNom := ReadString(Stream);
  FAge := ReadInteger(Stream);
end;
```

---

## Gestion des erreurs

### Validation lors de la désérialisation

```pascal
procedure TPerson.FromJSON(JSONObj: TJSONObject);  
begin  
  // Vérifier que l'objet JSON est valide
  if not Assigned(JSONObj) then
    raise Exception.Create('Objet JSON invalide');

  // Vérifier la présence des champs obligatoires
  if not JSONObj.TryGetValue<string>('nom', FNom) then
    raise Exception.Create('Champ "nom" manquant');

  if not JSONObj.TryGetValue<string>('prenom', FPrenom) then
    raise Exception.Create('Champ "prenom" manquant');

  // Champs optionnels avec valeurs par défaut
  if not JSONObj.TryGetValue<Integer>('age', FAge) then
    FAge := 0;

  if not JSONObj.TryGetValue<string>('email', FEmail) then
    FEmail := '';

  // Validation des données
  if FAge < 0 then
    raise Exception.Create('Âge invalide');

  if (FEmail <> '') and (Pos('@', FEmail) = 0) then
    raise Exception.Create('Email invalide');
end;
```

### Try/Catch lors du chargement

```pascal
procedure TForm1.ChargerPersonne(const FileName: string);  
var  
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    try
      Person.LoadFromFile(FileName);

      // Utiliser l'objet chargé
      Edit1.Text := Person.Nom;
      Edit2.Text := IntToStr(Person.Age);

      ShowMessage('Personne chargée avec succès');
    except
      on E: EFileNotFoundException do
        ShowMessage('Fichier introuvable : ' + FileName);
      on E: EFOpenError do
        ShowMessage('Impossible d''ouvrir le fichier : ' + E.Message);
      on E: Exception do
        ShowMessage('Erreur lors du chargement : ' + E.Message);
    end;
  finally
    Person.Free;
  end;
end;
```

---

## Comparaison des méthodes

| Méthode | Avantages | Inconvénients | Usage recommandé |
|---------|-----------|---------------|------------------|
| **Manuelle (Stream)** | Compact, rapide, contrôle total | Beaucoup de code, maintenance difficile | Performances critiques, format propriétaire |
| **JSON** | Lisible, universel, flexible | Plus volumineux, parsing plus lent | API, configuration, échange de données |
| **XML** | Structuré, validable, standard | Verbeux, complexe | Documents hiérarchiques, métadonnées |
| **RTTI** | Automatique, peu de code | Moins de contrôle, propriétés published uniquement | Prototypage rapide, classes simples |
| **Composants VCL/FMX** | Intégré à Delphi | Spécifique à Delphi | Sauvegarde de formulaires, composants |

---

## Bonnes pratiques

### 1. Toujours inclure une version

```pascal
const
  FORMAT_VERSION = 1;

procedure TPerson.SaveToStream(Stream: TStream);  
begin  
  Stream.WriteBuffer(FORMAT_VERSION, SizeOf(Integer));
  // ... reste des données
end;
```

### 2. Valider les données lors du chargement

```pascal
procedure TPerson.LoadFromStream(Stream: TStream);  
var  
  Version: Integer;
begin
  Stream.ReadBuffer(Version, SizeOf(Integer));

  if Version > FORMAT_VERSION then
    raise Exception.Create('Version de fichier trop récente');

  if Version < 1 then
    raise Exception.Create('Version de fichier invalide');

  // Charger selon la version
end;
```

### 3. Utiliser des valeurs par défaut

```pascal
procedure TPerson.FromJSON(JSONObj: TJSONObject);  
begin  
  FNom := JSONObj.GetValue<string>('nom', ''); // Valeur par défaut : chaîne vide
  FAge := JSONObj.GetValue<Integer>('age', 0); // Valeur par défaut : 0
end;
```

### 4. Séparer la logique de sérialisation

```pascal
// Ne pas mettre la logique métier dans les méthodes de sérialisation
// BON
procedure TPerson.SaveToFile(const FileName: string);  
var  
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

// MAUVAIS
procedure TPerson.SaveToFile(const FileName: string);  
begin  
  // Validation métier
  if FAge < 18 then
    raise Exception.Create('La personne doit être majeure');

  // ... sérialisation
end;
```

### 5. Documenter le format

```pascal
{
  Format de fichier Person v1.0

  Structure binaire :
  - 4 bytes : Version (Integer)
  - 4 bytes : Longueur du nom (Integer)
  - n bytes : Nom (UTF-8)
  - 4 bytes : Âge (Integer)
  - 4 bytes : Longueur de l'email (Integer)
  - n bytes : Email (UTF-8)
}
```

### 6. Tester la compatibilité ascendante et descendante

```pascal
// Tester que v2 peut lire des fichiers v1
procedure TestBackwardCompatibility;  
var  
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.LoadFromFile('personne_v1.dat');
    Assert(Person.Nom <> '', 'Nom devrait être chargé');
  finally
    Person.Free;
  end;
end;
```

---

## Exemple complet : Système de configuration

Voici un exemple pratique combinant plusieurs concepts :

```pascal
type
  TApplicationSettings = class
  private
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FWindowLeft: Integer;
    FWindowTop: Integer;
    FLanguage: string;
    FTheme: string;
    FAutoSave: Boolean;
    FRecentFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDefaults;
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property WindowWidth: Integer read FWindowWidth write FWindowWidth;
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;
    property WindowLeft: Integer read FWindowLeft write FWindowLeft;
    property WindowTop: Integer read FWindowTop write FWindowTop;
    property Language: string read FLanguage write FLanguage;
    property Theme: string read FTheme write FTheme;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property RecentFiles: TStringList read FRecentFiles;
  end;

constructor TApplicationSettings.Create;  
begin  
  inherited;
  FRecentFiles := TStringList.Create;
  LoadDefaults;
end;

destructor TApplicationSettings.Destroy;  
begin  
  FRecentFiles.Free;
  inherited;
end;

procedure TApplicationSettings.LoadDefaults;  
begin  
  FWindowWidth := 800;
  FWindowHeight := 600;
  FWindowLeft := 100;
  FWindowTop := 100;
  FLanguage := 'fr';
  FTheme := 'light';
  FAutoSave := True;
  FRecentFiles.Clear;
end;

function TApplicationSettings.ToJSON: TJSONObject;  
var  
  RecentArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Result.AddPair('version', TJSONNumber.Create(1));

  Result.AddPair('windowWidth', TJSONNumber.Create(FWindowWidth));
  Result.AddPair('windowHeight', TJSONNumber.Create(FWindowHeight));
  Result.AddPair('windowLeft', TJSONNumber.Create(FWindowLeft));
  Result.AddPair('windowTop', TJSONNumber.Create(FWindowTop));
  Result.AddPair('language', FLanguage);
  Result.AddPair('theme', FTheme);
  Result.AddPair('autoSave', TJSONBool.Create(FAutoSave));

  RecentArray := TJSONArray.Create;
  for i := 0 to FRecentFiles.Count - 1 do
    RecentArray.Add(FRecentFiles[i]);
  Result.AddPair('recentFiles', RecentArray);
end;

procedure TApplicationSettings.FromJSON(JSONObj: TJSONObject);  
var  
  RecentArray: TJSONArray;
  i: Integer;
begin
  FWindowWidth := JSONObj.GetValue<Integer>('windowWidth', 800);
  FWindowHeight := JSONObj.GetValue<Integer>('windowHeight', 600);
  FWindowLeft := JSONObj.GetValue<Integer>('windowLeft', 100);
  FWindowTop := JSONObj.GetValue<Integer>('windowTop', 100);
  FLanguage := JSONObj.GetValue<string>('language', 'fr');
  FTheme := JSONObj.GetValue<string>('theme', 'light');
  FAutoSave := JSONObj.GetValue<Boolean>('autoSave', True);

  FRecentFiles.Clear;
  if JSONObj.TryGetValue<TJSONArray>('recentFiles', RecentArray) then
  begin
    for i := 0 to RecentArray.Count - 1 do
      FRecentFiles.Add(RecentArray.Items[i].Value);
  end;
end;

procedure TApplicationSettings.SaveToFile(const FileName: string);  
var  
  JSONObj: TJSONObject;
  JSONStr: string;
begin
  JSONObj := ToJSON;
  try
    JSONStr := JSONObj.Format; // Format avec indentation
    TFile.WriteAllText(FileName, JSONStr, TEncoding.UTF8);
  finally
    JSONObj.Free;
  end;
end;

procedure TApplicationSettings.LoadFromFile(const FileName: string);  
var  
  JSONStr: string;
  JSONObj: TJSONObject;
begin
  if not FileExists(FileName) then
  begin
    LoadDefaults;
    Exit;
  end;

  try
    JSONStr := TFile.ReadAllText(FileName, TEncoding.UTF8);
    JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
    if Assigned(JSONObj) then
    try
      FromJSON(JSONObj);
    finally
      JSONObj.Free;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur, charger les valeurs par défaut
      LoadDefaults;
      // Logger l'erreur si nécessaire
    end;
  end;
end;

// Utilisation dans l'application
var
  Settings: TApplicationSettings;
begin
  Settings := TApplicationSettings.Create;
  try
    // Charger au démarrage
    Settings.LoadFromFile('settings.json');

    // Appliquer les paramètres
    Form1.Width := Settings.WindowWidth;
    Form1.Height := Settings.WindowHeight;
    Form1.Left := Settings.WindowLeft;
    Form1.Top := Settings.WindowTop;

    // ... utiliser l'application ...

    // Sauvegarder à la fermeture
    Settings.WindowWidth := Form1.Width;
    Settings.WindowHeight := Form1.Height;
    Settings.WindowLeft := Form1.Left;
    Settings.WindowTop := Form1.Top;

    Settings.SaveToFile('settings.json');
  finally
    Settings.Free;
  end;
end;
```

---

## Résumé

Dans ce chapitre, vous avez découvert la sérialisation et la persistance d'objets en Delphi :

**Méthodes de sérialisation :**
- **Manuelle avec streams** : contrôle total, format binaire compact
- **JSON** : format texte lisible, universel, flexible
- **XML** : format structuré, standard pour documents complexes
- **RTTI** : automatique via l'introspection de types
- **Composants VCL/FMX** : mécanisme intégré de Delphi

**Concepts clés :**
- Sérialisation = transformer un objet en données stockables
- Désérialisation = recréer un objet depuis les données
- Versioning = gérer l'évolution du format
- Validation = vérifier les données lors du chargement

**Bonnes pratiques :**
- Toujours inclure un numéro de version
- Valider les données chargées
- Utiliser des valeurs par défaut
- Gérer les erreurs gracieusement
- Documenter le format de fichier
- Tester la compatibilité

**Quand utiliser quoi :**
- **Stream binaire** : performance, format propriétaire
- **JSON** : configuration, API, échange de données
- **XML** : documents structurés, interopérabilité
- **RTTI** : prototypage rapide, classes simples
- **Composants** : sauvegarde d'interfaces utilisateur

La sérialisation est une compétence essentielle qui vous permettra de créer des applications qui conservent leur état, communiquent avec d'autres systèmes et offrent une meilleure expérience utilisateur !

⏭️ [Compression et décompression](/07-gestion-des-fichiers-et-flux-de-donnees/05-compression-et-decompression.md)
