# 7. Gestion des fichiers et flux de données

## 7.4 Sérialisation et persistance d'objets

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La sérialisation est le processus qui consiste à convertir des objets (avec leurs propriétés et données) en un format qui peut être stocké ou transmis, puis reconstitué ultérieurement. Cette technique est essentielle pour sauvegarder l'état d'une application, partager des données entre différentes instances d'un programme, ou communiquer entre applications.

### Introduction à la sérialisation

En termes simples, la sérialisation transforme un objet en mémoire en une séquence d'octets, et la désérialisation fait l'inverse. Delphi offre plusieurs approches pour réaliser cette tâche, des plus simples aux plus sophistiquées.

### Pourquoi sérialiser des objets ?

- **Persistance des données** : Sauvegarder l'état d'une application pour le restaurer ultérieurement
- **Transfert de données** : Envoyer des objets sur un réseau ou entre processus
- **Clonage d'objets** : Créer des copies profondes d'objets complexes
- **Caching** : Stocker temporairement des objets pour améliorer les performances

### Méthodes de sérialisation en Delphi

#### 1. Approche manuelle

L'approche la plus basique consiste à implémenter manuellement des méthodes pour sauvegarder et charger les propriétés d'un objet :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
  public
    constructor Create;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;
  end;

constructor TPersonne.Create;
begin
  inherited;
  FNom := '';
  FAge := 0;
  FTaille := 0;
end;

procedure TPersonne.SaveToStream(Stream: TStream);
var
  StrLen: Integer;
  TempStr: UTF8String;
begin
  // Écrire le nom
  TempStr := UTF8String(FNom);
  StrLen := Length(TempStr);
  Stream.WriteBuffer(StrLen, SizeOf(Integer));
  if StrLen > 0 then
    Stream.WriteBuffer(TempStr[1], StrLen);

  // Écrire l'âge
  Stream.WriteBuffer(FAge, SizeOf(Integer));

  // Écrire la taille
  Stream.WriteBuffer(FTaille, SizeOf(Double));
end;

procedure TPersonne.LoadFromStream(Stream: TStream);
var
  StrLen: Integer;
  TempStr: UTF8String;
begin
  // Lire le nom
  Stream.ReadBuffer(StrLen, SizeOf(Integer));
  if StrLen > 0 then
  begin
    SetLength(TempStr, StrLen);
    Stream.ReadBuffer(TempStr[1], StrLen);
    FNom := string(TempStr);
  end
  else
    FNom := '';

  // Lire l'âge
  Stream.ReadBuffer(FAge, SizeOf(Integer));

  // Lire la taille
  Stream.ReadBuffer(FTaille, SizeOf(Double));
end;
```

Utilisation :

```pascal
procedure SauvegarderPersonne(const NomFichier: string);
var
  Stream: TFileStream;
  Personne: TPersonne;
begin
  Personne := TPersonne.Create;
  try
    Personne.Nom := 'Dupont';
    Personne.Age := 30;
    Personne.Taille := 1.75;

    Stream := TFileStream.Create(NomFichier, fmCreate);
    try
      Personne.SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    Personne.Free;
  end;
end;

procedure ChargerPersonne(const NomFichier: string);
var
  Stream: TFileStream;
  Personne: TPersonne;
begin
  Personne := TPersonne.Create;
  try
    Stream := TFileStream.Create(NomFichier, fmOpenRead);
    try
      Personne.LoadFromStream(Stream);

      ShowMessage(Format('Nom: %s, Age: %d, Taille: %.2f m',
                        [Personne.Nom, Personne.Age, Personne.Taille]));
    finally
      Stream.Free;
    end;
  finally
    Personne.Free;
  end;
end;
```

#### 2. Utilisation de TReader/TWriter

Delphi fournit les classes `TReader` et `TWriter` qui simplifient la sérialisation en gérant automatiquement certains types de données :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
  public
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;
  end;

procedure TPersonne.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // TWriter gère automatiquement les types standards
    Writer.WriteString(FNom);
    Writer.WriteInteger(FAge);
    Writer.WriteFloat(FTaille);
  finally
    Writer.Free;
  end;
end;

procedure TPersonne.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    FNom := Reader.ReadString;
    FAge := Reader.ReadInteger;
    FTaille := Reader.ReadFloat;
  finally
    Reader.Free;
  end;
end;
```

Les classes `TReader` et `TWriter` offrent des méthodes spécifiques pour différents types de données :

| Méthode d'écriture | Méthode de lecture | Type de données |
|--------------------|-------------------|----------------|
| `WriteBoolean` | `ReadBoolean` | `Boolean` |
| `WriteInteger` | `ReadInteger` | `Integer` |
| `WriteFloat` | `ReadFloat` | `Single`, `Double`, `Extended` |
| `WriteString` | `ReadString` | `string` |
| `WriteChar` | `ReadChar` | `Char` |
| `WriteWideString` | `ReadWideString` | `WideString` |
| `WriteDate` | `ReadDate` | `TDateTime` (date) |
| `WriteBinary` | `ReadBinary` | Données binaires |

#### 3. RTTI (Run-Time Type Information)

La RTTI permet de sérialiser dynamiquement les objets en examinant leurs propriétés à l'exécution. Cette approche est particulièrement utile pour les objets complexes :

```pascal
uses
  System.Rtti, System.TypInfo;

type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
  published  // Important: les propriétés doivent être "published"
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;
  end;

procedure SauvegarderObjetAvecRTTI(Obj: TObject; Stream: TStream);
var
  Contexte: TRttiContext;
  Type_: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
  TypeInfo: string;
  PropCount: Integer;
  PropName: string;
  PropTypeName: string;
  StrLen: Integer;
  S: string;
  I: Integer;
  F: Double;
  B: Boolean;
begin
  Contexte := TRttiContext.Create;
  try
    // Écrire le nom du type
    TypeInfo := Obj.ClassName;
    StrLen := Length(TypeInfo);
    Stream.WriteBuffer(StrLen, SizeOf(Integer));
    if StrLen > 0 then
      Stream.WriteBuffer(TypeInfo[1], StrLen * SizeOf(Char));

    // Obtenir le type RTTI
    Type_ := Contexte.GetType(Obj.ClassType);

    // Écrire le nombre de propriétés publiées
    PropCount := 0;
    for Prop in Type_.GetProperties do
      if Prop.Visibility = TMemberVisibility.mvPublished then
        Inc(PropCount);

    Stream.WriteBuffer(PropCount, SizeOf(Integer));

    // Parcourir et sauvegarder chaque propriété publiée
    for Prop in Type_.GetProperties do
    begin
      if Prop.Visibility = TMemberVisibility.mvPublished then
      begin
        // Écrire le nom de la propriété
        PropName := Prop.Name;
        StrLen := Length(PropName);
        Stream.WriteBuffer(StrLen, SizeOf(Integer));
        if StrLen > 0 then
          Stream.WriteBuffer(PropName[1], StrLen * SizeOf(Char));

        // Écrire le type de la propriété
        PropTypeName := Prop.PropertyType.Name;
        StrLen := Length(PropTypeName);
        Stream.WriteBuffer(StrLen, SizeOf(Integer));
        if StrLen > 0 then
          Stream.WriteBuffer(PropTypeName[1], StrLen * SizeOf(Char));

        // Obtenir la valeur de la propriété
        Value := Prop.GetValue(Obj);

        // Sauvegarder la valeur selon son type
        if Prop.PropertyType.TypeKind = tkString then
        begin
          S := Value.AsString;
          StrLen := Length(S);
          Stream.WriteBuffer(StrLen, SizeOf(Integer));
          if StrLen > 0 then
            Stream.WriteBuffer(S[1], StrLen * SizeOf(Char));
        end
        else if Prop.PropertyType.TypeKind = tkInteger then
        begin
          I := Value.AsInteger;
          Stream.WriteBuffer(I, SizeOf(Integer));
        end
        else if Prop.PropertyType.TypeKind = tkFloat then
        begin
          F := Value.AsExtended;
          Stream.WriteBuffer(F, SizeOf(Double));
        end
        else if Prop.PropertyType.TypeKind = tkEnumeration then
        begin
          if Prop.PropertyType.Handle = TypeInfo(Boolean) then
          begin
            B := Value.AsBoolean;
            Stream.WriteBuffer(B, SizeOf(Boolean));
          end;
        end;
        // Ajouter d'autres types selon vos besoins...
      end;
    end;
  finally
    Contexte.Free;
  end;
end;

function ChargerObjetAvecRTTI(Stream: TStream): TObject;
var
  Contexte: TRttiContext;
  Type_: TRttiType;
  Prop: TRttiProperty;
  TypeInfo: string;
  PropCount: Integer;
  PropName: string;
  PropTypeName: string;
  StrLen: Integer;
  S: string;
  I: Integer;
  F: Double;
  B: Boolean;
  i: Integer;
  Value: TValue;
begin
  Result := nil;
  Contexte := TRttiContext.Create;
  try
    // Lire le nom du type
    Stream.ReadBuffer(StrLen, SizeOf(Integer));
    SetLength(TypeInfo, StrLen);
    if StrLen > 0 then
      Stream.ReadBuffer(TypeInfo[1], StrLen * SizeOf(Char));

    // Créer une instance du type
    for Type_ in Contexte.GetTypes do
    begin
      if (Type_ is TRttiInstanceType) and (Type_.Name = TypeInfo) then
      begin
        Result := TRttiInstanceType(Type_).MetaclassType.Create;
        Break;
      end;
    end;

    if Result = nil then
      Exit;

    // Lire le nombre de propriétés
    Stream.ReadBuffer(PropCount, SizeOf(Integer));

    // Lire chaque propriété
    for i := 0 to PropCount - 1 do
    begin
      // Lire le nom de la propriété
      Stream.ReadBuffer(StrLen, SizeOf(Integer));
      SetLength(PropName, StrLen);
      if StrLen > 0 then
        Stream.ReadBuffer(PropName[1], StrLen * SizeOf(Char));

      // Lire le type de la propriété
      Stream.ReadBuffer(StrLen, SizeOf(Integer));
      SetLength(PropTypeName, StrLen);
      if StrLen > 0 then
        Stream.ReadBuffer(PropTypeName[1], StrLen * SizeOf(Char));

      // Trouver la propriété correspondante
      Type_ := Contexte.GetType(Result.ClassType);
      Prop := Type_.GetProperty(PropName);

      if Assigned(Prop) then
      begin
        // Lire la valeur selon son type
        if Prop.PropertyType.TypeKind = tkString then
        begin
          Stream.ReadBuffer(StrLen, SizeOf(Integer));
          SetLength(S, StrLen);
          if StrLen > 0 then
            Stream.ReadBuffer(S[1], StrLen * SizeOf(Char));
          Prop.SetValue(Result, S);
        end
        else if Prop.PropertyType.TypeKind = tkInteger then
        begin
          Stream.ReadBuffer(I, SizeOf(Integer));
          Prop.SetValue(Result, I);
        end
        else if Prop.PropertyType.TypeKind = tkFloat then
        begin
          Stream.ReadBuffer(F, SizeOf(Double));
          Prop.SetValue(Result, F);
        end
        else if Prop.PropertyType.TypeKind = tkEnumeration then
        begin
          if Prop.PropertyType.Handle = TypeInfo(Boolean) then
          begin
            Stream.ReadBuffer(B, SizeOf(Boolean));
            Prop.SetValue(Result, B);
          end;
        end;
        // Ajouter d'autres types selon vos besoins...
      end;
    end;
  finally
    Contexte.Free;
  end;
end;
```

> **Note :** Cette implémentation RTTI est simplifiée pour l'exemple et ne gère pas tous les types possibles. Dans une application réelle, vous devriez utiliser une bibliothèque plus complète ou développer une solution plus robuste.

#### 4. Sérialisation au format JSON

JSON (JavaScript Object Notation) est un format texte léger, facile à lire et à écrire, qui est devenu un standard pour l'échange de données. Delphi offre d'excellentes fonctionnalités pour sérialiser des objets en JSON :

```pascal
uses
  System.JSON, REST.Json;

type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
  published
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;
  end;

procedure SauvegarderPersonneEnJSON(const NomFichier: string);
var
  Personne: TPersonne;
  JSONObj: TJSONObject;
  JSONString: string;
begin
  Personne := TPersonne.Create;
  try
    Personne.Nom := 'Dupont';
    Personne.Age := 30;
    Personne.Taille := 1.75;

    // Convertir l'objet en JSON
    JSONString := TJson.ObjectToJsonString(Personne);

    // Sauvegarder dans un fichier
    TFile.WriteAllText(NomFichier, JSONString);
  finally
    Personne.Free;
  end;
end;

procedure ChargerPersonneDepuisJSON(const NomFichier: string);
var
  Personne: TPersonne;
  JSONString: string;
begin
  // Lire le contenu du fichier
  JSONString := TFile.ReadAllText(NomFichier);

  // Convertir le JSON en objet
  Personne := TJson.JsonToObject<TPersonne>(JSONString);
  try
    ShowMessage(Format('Nom: %s, Age: %d, Taille: %.2f m',
                      [Personne.Nom, Personne.Age, Personne.Taille]));
  finally
    Personne.Free;
  end;
end;
```

Les avantages de la sérialisation JSON :
- Format lisible par les humains
- Interopérabilité avec d'autres langages et plateformes
- Facilité de débogage
- Support natif dans les API web et REST

> **Note :** L'utilisation de `TJson.ObjectToJsonString` et `TJson.JsonToObject<T>` nécessite Delphi XE7 ou supérieur.

#### 5. Sérialisation au format XML

XML est un autre format populaire pour la sérialisation. Delphi propose plusieurs approches pour travailler avec XML :

```pascal
uses
  Xml.XMLDoc, Xml.XMLIntf;

type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
  public
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;

    procedure SaveToXML(const NomFichier: string);
    procedure LoadFromXML(const NomFichier: string);
  end;

procedure TPersonne.SaveToXML(const NomFichier: string);
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
begin
  // Créer un nouveau document XML
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  // Créer le nœud racine
  RootNode := XMLDoc.AddChild('donnees');

  // Ajouter un nœud pour la personne
  PersonneNode := RootNode.AddChild('personne');
  PersonneNode.ChildValues['nom'] := FNom;
  PersonneNode.ChildValues['age'] := FAge;
  PersonneNode.ChildValues['taille'] := FTaille;

  // Sauvegarder le document XML
  XMLDoc.SaveToFile(NomFichier);
end;

procedure TPersonne.LoadFromXML(const NomFichier: string);
var
  XMLDoc: IXMLDocument;
  RootNode, PersonneNode: IXMLNode;
begin
  // Charger le document XML
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromFile(NomFichier);
  XMLDoc.Active := True;

  // Obtenir le nœud racine
  RootNode := XMLDoc.DocumentElement;

  // Lire les données de la personne
  if Assigned(RootNode) and (RootNode.ChildNodes.Count > 0) then
  begin
    PersonneNode := RootNode.ChildNodes[0];

    if PersonneNode.NodeName = 'personne' then
    begin
      FNom := PersonneNode.ChildValues['nom'];
      FAge := PersonneNode.ChildValues['age'];
      FTaille := PersonneNode.ChildValues['taille'];
    end;
  end;
end;
```

### Persistance d'objets complexes

Pour les objets complexes qui contiennent d'autres objets ou des collections, il faut adapter les méthodes de sérialisation :

```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
  published
    property Rue: string read FRue write FRue;
    property Ville: string read FVille write FVille;
    property CodePostal: string read FCodePostal write FCodePostal;
  end;

  TClient = class
  private
    FNom: string;
    FAge: Integer;
    FAdresse: TAdresse;
    FTelephones: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Adresse: TAdresse read FAdresse;
    property Telephones: TStringList read FTelephones;
  end;

constructor TClient.Create;
begin
  inherited;
  FAdresse := TAdresse.Create;
  FTelephones := TStringList.Create;
end;

destructor TClient.Destroy;
begin
  FAdresse.Free;
  FTelephones.Free;
  inherited;
end;

procedure TClient.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
  Count: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Sauvegarder les propriétés simples
    Writer.WriteString(FNom);
    Writer.WriteInteger(FAge);

    // Sauvegarder l'adresse
    Writer.WriteString(FAdresse.Rue);
    Writer.WriteString(FAdresse.Ville);
    Writer.WriteString(FAdresse.CodePostal);

    // Sauvegarder la liste de téléphones
    Count := FTelephones.Count;
    Writer.WriteInteger(Count);

    for var i := 0 to Count - 1 do
      Writer.WriteString(FTelephones[i]);
  finally
    Writer.Free;
  end;
end;

procedure TClient.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  Count: Integer;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    // Charger les propriétés simples
    FNom := Reader.ReadString;
    FAge := Reader.ReadInteger;

    // Charger l'adresse
    FAdresse.Rue := Reader.ReadString;
    FAdresse.Ville := Reader.ReadString;
    FAdresse.CodePostal := Reader.ReadString;

    // Charger la liste de téléphones
    FTelephones.Clear;
    Count := Reader.ReadInteger;

    for var i := 0 to Count - 1 do
      FTelephones.Add(Reader.ReadString);
  finally
    Reader.Free;
  end;
end;
```

### Versionnement de la sérialisation

Pour maintenir la compatibilité avec les anciennes versions sérialisées, il est recommandé d'ajouter un numéro de version à vos données :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
    FTaille: Double;
    FEmail: string;  // Nouvelle propriété ajoutée dans la version 2
  public
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Taille: Double read FTaille write FTaille;
    property Email: string read FEmail write FEmail;
  end;

procedure TPersonne.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
  Version: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Écrire le numéro de version actuel
    Version := 2;  // Version 2 inclut l'email
    Writer.WriteInteger(Version);

    // Écrire les données de base (version 1)
    Writer.WriteString(FNom);
    Writer.WriteInteger(FAge);
    Writer.WriteFloat(FTaille);

    // Écrire les données ajoutées en version 2
    Writer.WriteString(FEmail);
  finally
    Writer.Free;
  end;
end;

procedure TPersonne.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  Version: Integer;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    // Lire le numéro de version
    Version := Reader.ReadInteger;

    // Lire les données de base (version 1)
    FNom := Reader.ReadString;
    FAge := Reader.ReadInteger;
    FTaille := Reader.ReadFloat;

    // Lire les données supplémentaires selon la version
    if Version >= 2 then
      FEmail := Reader.ReadString
    else
      FEmail := '';  // Valeur par défaut si version ancienne
  finally
    Reader.Free;
  end;
end;
```

### Persistance avec les bases de données

Pour stocker des objets dans une base de données, vous pouvez les sérialiser dans un champ BLOB :

```pascal
procedure SauvegarderPersonneDansDB(Personne: TPersonne; Query: TFDQuery);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    // Sérialiser l'objet
    Personne.SaveToStream(MemStream);

    // Revenir au début du flux
    MemStream.Position := 0;

    // Préparer la requête
    Query.SQL.Text := 'INSERT INTO Personnes (ID, Donnees) VALUES (:ID, :Donnees)';
    Query.ParamByName('ID').AsInteger := 1;  // ID exemple

    // Assigner le flux au paramètre BLOB
    Query.ParamByName('Donnees').LoadFromStream(MemStream, ftBlob);

    // Exécuter la requête
    Query.ExecSQL;
  finally
    MemStream.Free;
  end;
end;

function ChargerPersonneDepuisDB(ID: Integer; Query: TFDQuery): TPersonne;
var
  MemStream: TMemoryStream;
begin
  Result := TPersonne.Create;

  MemStream := TMemoryStream.Create;
  try
    // Préparer la requête
    Query.SQL.Text := 'SELECT Donnees FROM Personnes WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // Charger le BLOB dans le flux
      TBlobField(Query.FieldByName('Donnees')).SaveToStream(MemStream);

      // Revenir au début du flux
      MemStream.Position := 0;

      // Désérialiser l'objet
      Result.LoadFromStream(MemStream);
    end;

    Query.Close;
  finally
    MemStream.Free;
  end;
end;
```

### Bibliothèques de sérialisation tierces

Plusieurs bibliothèques tierces offrent des fonctionnalités avancées de sérialisation :

- **SuperObject** : Manipulation JSON rapide et flexible
- **mORMot** : Framework ORM avec sérialisation performante
- **Spring4D** : Framework avec des fonctionnalités de sérialisation avancées
- **DWScript** : Supporte la sérialisation JSON efficace

### Bonnes pratiques de sérialisation

1. **Versionnez vos formats** : Incluez toujours un numéro de version dans vos données sérialisées pour la compatibilité future.

2. **Validez les données** : Vérifiez l'intégrité des données lors de la désérialisation (checksums, validations).

3. **Gérez les erreurs** : Prévoyez des mécanismes de restauration en cas de corruption des données.

4. **Documenter le format** : Si vous utilisez un format personnalisé, documentez-le pour faciliter la maintenance.

5. **Sécurité** : Attention aux vulnérabilités potentielles lors de la désérialisation de données externes.

6. **Performances** : Pour les grands ensembles de données, préférez les formats binaires aux formats texte.

7. **Interopérabilité** : Utilisez JSON ou XML si vos données doivent être lues par d'autres systèmes.

### Exemple complet : Éditeur de personnes

Voici un exemple d'application qui permet de créer, modifier, sauvegarder et charger des personnes sérialisées :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
  published
    property Rue: string read FRue write FRue;
    property Ville: string read FVille write FVille;
    property CodePostal: string read FCodePostal write FCodePostal;
  end;

  TPersonne = class
  private
    FNom: string;
    FPrenom: string;
    FAge: Integer;
    FAdresse: TAdresse;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  published
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Age: Integer read FAge write FAge;
    property Adresse: TAdresse read FAdresse;
  end;

  TfrmMain = class(TForm)
    pnlTop: TPanel;
    btnNouveau: TButton;
    btnOuvrir: TButton;
    btnSauvegarder: TButton;
    btnSauvegarderJSON: TButton;
    pnlMain: TPanel;
    lblNom: TLabel;
    edtNom: TEdit;
    lblPrenom: TLabel;
    edtPrenom: TEdit;
    lblAge: TLabel;
    edtAge: TEdit;
    grpAdresse: TGroupBox;
    lblRue: TLabel;
    edtRue: TEdit;
    lblVille: TLabel;
    edtVille: TEdit;
    lblCodePostal: TLabel;
    edtCodePostal: TEdit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNouveauClick(Sender: TObject);
    procedure btnOuvrirClick(Sender: TObject);
    procedure btnSauvegarderClick(Sender: TObject);
    procedure btnSauvegarderJSONClick(Sender: TObject);
  private
    FPersonne: TPersonne;
    procedure UpdateUI;
    procedure ObjectToUI;
    procedure UIToObject;
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.JSON, REST.Json, System.IOUtils;

constructor TPersonne.Create;
begin
  inherited;
  FAdresse := TAdresse.Create;
end;

destructor TPersonne.Destroy;
begin
  FAdresse.Free;
  inherited;
end;

procedure TPersonne.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
  Writer: TWriter;
  Version: Integer;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Writer := TWriter.Create(Stream, 4096);
    try
      // Écrire la version du format
      Version := 1;
      Writer.WriteInteger(Version);

      // Écrire les données de la personne
      Writer.WriteString(FNom);
      Writer.WriteString(FPrenom);
      Writer.WriteInteger(FAge);

      // Écrire les données de l'adresse
      Writer.WriteString(FAdresse.Rue);
      Writer.WriteString(FAdresse.Ville);
      Writer.WriteString(FAdresse.CodePostal);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TPersonne.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
  Reader: TReader;
  Version: Integer;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Reader := TReader.Create(Stream, 4096);
    try
      // Lire la version du format
      Version := Reader.ReadInteger;

      // Lire les données selon la version
      if Version >= 1 then
      begin
        FNom := Reader.ReadString;
        FPrenom := Reader.ReadString;
        FAge := Reader.ReadInteger;

        FAdresse.Rue := Reader.ReadString;
        FAdresse.Ville := Reader.ReadString;
        FAdresse.CodePostal := Reader.ReadString;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPersonne := TPersonne.Create;
  UpdateUI;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FPersonne.Free;
end;

procedure TfrmMain.UpdateUI;
begin
  ObjectToUI;
end;

procedure TfrmMain.ObjectToUI;
begin
  edtNom.Text := FPersonne.Nom;
  edtPrenom.Text := FPersonne.Prenom;
  edtAge.Text := IntToStr(FPersonne.Age);

  edtRue.Text := FPersonne.Adresse.Rue;
  edtVille.Text := FPersonne.Adresse.Ville;
  edtCodePostal.Text := FPersonne.Adresse.CodePostal;
end;

procedure TfrmMain.UIToObject;
begin
  FPersonne.Nom := edtNom.Text;
  FPersonne.Prenom := edtPrenom.Text;

  // Convertir en entier avec gestion d'erreur
  try
    FPersonne.Age := StrToInt(edtAge.Text);
  except
    FPersonne.Age := 0;
  end;

  FPersonne.Adresse.Rue := edtRue.Text;
  FPersonne.Adresse.Ville := edtVille.Text;
  FPersonne.Adresse.CodePostal := edtCodePostal.Text;
end;

procedure TfrmMain.btnNouveauClick(Sender: TObject);
begin
  // Créer une nouvelle personne
  FPersonne.Free;
  FPersonne := TPersonne.Create;
  UpdateUI;
end;

procedure TfrmMain.btnOuvrirClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Fichiers personne (*.per)|*.per|Fichiers JSON (*.json)|*.json|Tous les fichiers (*.*)|*.*';

  if OpenDialog1.Execute then
  begin
    if AnsiLowerCase(ExtractFileExt(OpenDialog1.FileName)) = '.json' then
    begin
      // Charger depuis JSON
      var JSONString := TFile.ReadAllText(OpenDialog1.FileName);
      var NouvellePersonne := TJson.JsonToObject<TPersonne>(JSONString);

      // Remplacer la personne actuelle
      FPersonne.Free;
      FPersonne := NouvellePersonne;
    end
    else
    begin
      // Charger depuis le format binaire
      FPersonne.LoadFromFile(OpenDialog1.FileName);
    end;

    UpdateUI;
  end;
end;

procedure TfrmMain.btnSauvegarderClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Fichiers personne (*.per)|*.per|Tous les fichiers (*.*)|*.*';
  SaveDialog1.DefaultExt := 'per';

  if SaveDialog1.Execute then
  begin
    // Mettre à jour l'objet avec les données de l'interface
    UIToObject;

    // Sauvegarder dans le format binaire
    FPersonne.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.btnSauvegarderJSONClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Fichiers JSON (*.json)|*.json|Tous les fichiers (*.*)|*.*';
  SaveDialog1.DefaultExt := 'json';

  if SaveDialog1.Execute then
  begin
    // Mettre à jour l'objet avec les données de l'interface
    UIToObject;

    // Convertir en JSON
    var JSONString := TJson.ObjectToJsonString(FPersonne);

    // Sauvegarder dans un fichier
    TFile.WriteAllText(SaveDialog1.FileName, JSONString);
  end;
end;

end.
```

Pour compléter cet exemple, voici le fichier DFM de l'interface graphique :

```pascal
object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Éditeur de Personnes'
  ClientHeight = 382
  ClientWidth = 538
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 538
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 528
    object btnNouveau: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Nouveau'
      TabOrder = 0
      OnClick = btnNouveauClick
    end
    object btnOuvrir: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ouvrir...'
      TabOrder = 1
      OnClick = btnOuvrirClick
    end
    object btnSauvegarder: TButton
      Left = 178
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Sauvegarder...'
      TabOrder = 2
      OnClick = btnSauvegarderClick
    end
    object btnSauvegarderJSON: TButton
      Left = 281
      Top = 8
      Width = 121
      Height = 25
      Caption = 'Sauvegarder en JSON...'
      TabOrder = 3
      OnClick = btnSauvegarderJSONClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 538
    Height = 341
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 528
    ExplicitHeight = 331
    object lblNom: TLabel
      Left = 16
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Nom :'
    end
    object lblPrenom: TLabel
      Left = 16
      Top = 48
      Width = 44
      Height = 13
      Caption = 'Prénom :'
    end
    object lblAge: TLabel
      Left = 16
      Top = 80
      Width = 25
      Height = 13
      Caption = 'Age :'
    end
    object edtNom: TEdit
      Left = 72
      Top = 16
      Width = 217
      Height = 21
      TabOrder = 0
    end
    object edtPrenom: TEdit
      Left = 72
      Top = 48
      Width = 217
      Height = 21
      TabOrder = 1
    end
    object edtAge: TEdit
      Left = 72
      Top = 80
      Width = 65
      Height = 21
      TabOrder = 2
    end
    object grpAdresse: TGroupBox
      Left = 16
      Top = 120
      Width = 489
      Height = 137
      Caption = 'Adresse'
      TabOrder = 3
      object lblRue: TLabel
        Left = 16
        Top = 24
        Width = 27
        Height = 13
        Caption = 'Rue :'
      end
      object lblVille: TLabel
        Left = 16
        Top = 56
        Width = 25
        Height = 13
        Caption = 'Ville :'
      end
      object lblCodePostal: TLabel
        Left = 16
        Top = 88
        Width = 63
        Height = 13
        Caption = 'Code Postal :'
      end
      object edtRue: TEdit
        Left = 96
        Top = 24
        Width = 369
        Height = 21
        TabOrder = 0
      end
      object edtVille: TEdit
        Left = 96
        Top = 56
        Width = 217
        Height = 21
        TabOrder = 1
      end
      object edtCodePostal: TEdit
        Left = 96
        Top = 88
        Width = 89
        Height = 21
        TabOrder = 2
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 320
    Top = 200
  end
  object SaveDialog1: TSaveDialog
    Left = 392
    Top = 200
  end
end
```

### Persistance d'un arbre d'objets

Lorsque vous avez des structures complexes comme des arbres d'objets, la sérialisation devient plus délicate. Voici un exemple de sérialisation d'un arbre de catégories :

```pascal
type
  TCategorie = class
  private
    FNom: string;
    FDescription: string;
    FSousCategories: TObjectList<TCategorie>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function AddSousCategorie(const Nom: string): TCategorie;
    property Nom: string read FNom write FNom;
    property Description: string read FDescription write FDescription;
    property SousCategories: TObjectList<TCategorie> read FSousCategories;
  end;

constructor TCategorie.Create;
begin
  inherited;
  FSousCategories := TObjectList<TCategorie>.Create(True); // True = Owns objects
end;

destructor TCategorie.Destroy;
begin
  FSousCategories.Free;
  inherited;
end;

function TCategorie.AddSousCategorie(const Nom: string): TCategorie;
begin
  Result := TCategorie.Create;
  Result.Nom := Nom;
  FSousCategories.Add(Result);
end;

procedure TCategorie.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
  Count: Integer;
  i: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Écrire les données de cette catégorie
    Writer.WriteString(FNom);
    Writer.WriteString(FDescription);

    // Écrire le nombre de sous-catégories
    Count := FSousCategories.Count;
    Writer.WriteInteger(Count);

    // Écrire récursivement chaque sous-catégorie
    for i := 0 to Count - 1 do
      FSousCategories[i].SaveToStream(Stream);
  finally
    Writer.Free;
  end;
end;

procedure TCategorie.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  Count, i: Integer;
  SousCategorie: TCategorie;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    // Lire les données de cette catégorie
    FNom := Reader.ReadString;
    FDescription := Reader.ReadString;

    // Lire le nombre de sous-catégories
    Count := Reader.ReadInteger;

    // Vider la liste actuelle
    FSousCategories.Clear;

    // Lire récursivement chaque sous-catégorie
    for i := 0 to Count - 1 do
    begin
      SousCategorie := TCategorie.Create;
      FSousCategories.Add(SousCategorie);
      SousCategorie.LoadFromStream(Stream);
    end;
  finally
    Reader.Free;
  end;
end;
```

### Persistance de collections

Pour les collections, vous pouvez utiliser les mécanismes standards de Delphi ou implémenter votre propre logique :

```pascal
type
  TProduit = class
  private
    FCode: string;
    FNom: string;
    FPrix: Double;
  published
    property Code: string read FCode write FCode;
    property Nom: string read FNom write FNom;
    property Prix: Double read FPrix write FPrix;
  end;

  TProduits = class(TObjectList<TProduit>)
  public
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToJSON(const FileName: string);
    procedure LoadFromJSON(const FileName: string);
  end;

procedure TProduits.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
  Writer: TWriter;
  Count, i: Integer;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Writer := TWriter.Create(Stream, 4096);
    try
      // Écrire le nombre de produits
      Count := Self.Count;
      Writer.WriteInteger(Count);

      // Écrire chaque produit
      for i := 0 to Count - 1 do
      begin
        Writer.WriteString(Self[i].Code);
        Writer.WriteString(Self[i].Nom);
        Writer.WriteFloat(Self[i].Prix);
      end;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TProduits.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
  Reader: TReader;
  Count, i: Integer;
  Produit: TProduit;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Reader := TReader.Create(Stream, 4096);
    try
      // Lire le nombre de produits
      Count := Reader.ReadInteger;

      // Vider la liste actuelle
      Self.Clear;

      // Lire chaque produit
      for i := 0 to Count - 1 do
      begin
        Produit := TProduit.Create;
        Produit.Code := Reader.ReadString;
        Produit.Nom := Reader.ReadString;
        Produit.Prix := Reader.ReadFloat;
        Self.Add(Produit);
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TProduits.SaveToJSON(const FileName: string);
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
begin
  JSONArray := TJSONArray.Create;
  try
    // Créer un objet JSON pour chaque produit
    for i := 0 to Self.Count - 1 do
    begin
      JSONObj := TJSONObject.Create;
      JSONObj.AddPair('code', Self[i].Code);
      JSONObj.AddPair('nom', Self[i].Nom);
      JSONObj.AddPair('prix', TJSONNumber.Create(Self[i].Prix));
      JSONArray.AddElement(JSONObj);
    end;

    // Sauvegarder le tableau JSON dans un fichier
    TFile.WriteAllText(FileName, JSONArray.ToString);
  finally
    JSONArray.Free;
  end;
end;

procedure TProduits.LoadFromJSON(const FileName: string);
var
  JSONString: string;
  JSONArray: TJSONArray;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
  Produit: TProduit;
  i: Integer;
begin
  // Lire le contenu du fichier
  JSONString := TFile.ReadAllText(FileName);

  // Analyser le JSON
  JSONArray := TJSONObject.ParseJSONValue(JSONString) as TJSONArray;
  if Assigned(JSONArray) then
  begin
    try
      // Vider la liste actuelle
      Self.Clear;

      // Parcourir chaque élément du tableau
      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONValue := JSONArray.Items[i];
        if JSONValue is TJSONObject then
        begin
          JSONObj := TJSONObject(JSONValue);

          Produit := TProduit.Create;
          Produit.Code := JSONObj.GetValue<string>('code');
          Produit.Nom := JSONObj.GetValue<string>('nom');
          Produit.Prix := JSONObj.GetValue<Double>('prix');

          Self.Add(Produit);
        end;
      end;
    finally
      JSONArray.Free;
    end;
  end;
end;
```

### Exercice pratique

Créez une application de gestion de bibliothèque avec les fonctionnalités suivantes :

1. Définition de classes pour les livres, auteurs et catégories
2. Interface pour ajouter, modifier et supprimer des livres
3. Sauvegarde de la bibliothèque complète dans un fichier binaire
4. Export/import au format JSON
5. Possibilité de sauvegarder/charger l'état de l'application

Cet exercice vous permettra de mettre en pratique les différentes techniques de sérialisation tout en créant une application utile.

---

À suivre dans la prochaine section : **7.5 Compression et décompression**

⏭️ [Compression et décompression](/07-gestion-des-fichiers-et-flux-de-donnees/05-compression-et-decompression.md)
