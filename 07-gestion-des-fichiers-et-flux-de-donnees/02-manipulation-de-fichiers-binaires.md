🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.2 Manipulation de fichiers binaires

## Introduction

Contrairement aux fichiers texte qui stockent des informations sous forme de caractères lisibles par l'homme, les fichiers binaires stockent des données dans leur format natif, exactement comme elles sont représentées en mémoire par l'ordinateur.

Dans ce chapitre, nous allons découvrir comment manipuler ces fichiers binaires en Delphi, une compétence essentielle pour travailler avec des formats de fichiers personnalisés, des bases de données simples, ou des fichiers de configuration complexes.

## Qu'est-ce qu'un fichier binaire ?

Un fichier binaire contient des données brutes sous forme d'octets (bytes). Contrairement aux fichiers texte :

**Fichier texte :**
- Stocke le nombre 12345 comme "12345" (5 caractères, 5 octets)
- Lisible directement dans un éditeur de texte
- Plus volumineux
- Encodage variable (UTF-8, ANSI, etc.)

**Fichier binaire :**
- Stocke le nombre 12345 comme une valeur binaire (2 ou 4 octets selon le type)
- Illisible dans un éditeur de texte classique
- Plus compact
- Représentation exacte des données en mémoire

### Exemples de fichiers binaires courants

- Images : `.jpg`, `.png`, `.bmp`, `.gif`
- Exécutables : `.exe`, `.dll`
- Archives : `.zip`, `.rar`
- Vidéos : `.mp4`, `.avi`
- Documents : `.pdf`, `.docx`
- Bases de données : `.db`, `.sqlite`

---

## Pourquoi utiliser des fichiers binaires ?

Les fichiers binaires présentent plusieurs avantages :

1. **Compacité** : moins d'espace disque utilisé
2. **Performance** : lecture/écriture plus rapide
3. **Précision** : conservation exacte des valeurs (pas de conversion texte)
4. **Sécurité** : données moins facilement lisibles ou modifiables
5. **Structures complexes** : stockage de données structurées complexes

---

## Les différentes approches en Delphi

Delphi offre plusieurs méthodes pour manipuler des fichiers binaires :

1. **TFileStream** : l'approche moderne et flexible (recommandée)
2. **File of Type** : l'approche classique Pascal (typée)
3. **File non typé** : pour un contrôle total au niveau des octets
4. **TMemoryStream** : pour manipuler des données binaires en mémoire

Nous allons explorer ces méthodes en détail.

---

## Méthode 1 : TFileStream (Recommandée)

`TFileStream` est une classe moderne qui permet de lire et écrire des données binaires de manière flexible et puissante.

### Création et ouverture d'un TFileStream

```pascal
uses
  System.Classes, System.SysUtils;

var
  Stream: TFileStream;
begin
  // Créer un nouveau fichier (ou écraser l'existant)
  Stream := TFileStream.Create('data.bin', fmCreate);
  try
    // Travailler avec le stream
  finally
    Stream.Free;
  end;

  // Ouvrir un fichier existant en lecture seule
  Stream := TFileStream.Create('data.bin', fmOpenRead);

  // Ouvrir en lecture/écriture
  Stream := TFileStream.Create('data.bin', fmOpenReadWrite);

  // Créer s'il n'existe pas, ouvrir sinon
  Stream := TFileStream.Create('data.bin', fmOpenReadWrite or fmCreate);
end;
```

**Modes d'ouverture disponibles :**
- `fmCreate` : crée un nouveau fichier (écrase s'il existe)
- `fmOpenRead` : ouvre en lecture seule
- `fmOpenWrite` : ouvre en écriture seule
- `fmOpenReadWrite` : ouvre en lecture/écriture
- `fmShareDenyNone` : autorise le partage complet
- `fmShareDenyRead` : interdit la lecture par d'autres processus
- `fmShareDenyWrite` : interdit l'écriture par d'autres processus

### Écriture de données simples

```pascal
procedure EcrireDonneesSimples;  
var  
  Stream: TFileStream;
  Nombre: Integer;
  Reel: Double;
  Caractere: Char;
  Booleen: Boolean;
begin
  Stream := TFileStream.Create('donnees.bin', fmCreate);
  try
    // Écrire un entier
    Nombre := 12345;
    Stream.Write(Nombre, SizeOf(Nombre));

    // Écrire un réel
    Reel := 3.14159;
    Stream.Write(Reel, SizeOf(Reel));

    // Écrire un caractère
    Caractere := 'A';
    Stream.Write(Caractere, SizeOf(Caractere));

    // Écrire un booléen
    Booleen := True;
    Stream.Write(Booleen, SizeOf(Booleen));

    ShowMessage('Données écrites avec succès !');
  finally
    Stream.Free;
  end;
end;
```

**Points clés :**
- `Write(Variable, Taille)` : écrit les octets de la variable
- `SizeOf(Variable)` : retourne la taille en octets
- Les données sont écrites dans l'ordre séquentiel

### Lecture de données simples

```pascal
procedure LireDonneesSimples;  
var  
  Stream: TFileStream;
  Nombre: Integer;
  Reel: Double;
  Caractere: Char;
  Booleen: Boolean;
  Message: string;
begin
  if not FileExists('donnees.bin') then
  begin
    ShowMessage('Fichier introuvable !');
    Exit;
  end;

  Stream := TFileStream.Create('donnees.bin', fmOpenRead);
  try
    // Lire dans le même ordre que l'écriture !
    Stream.Read(Nombre, SizeOf(Nombre));
    Stream.Read(Reel, SizeOf(Reel));
    Stream.Read(Caractere, SizeOf(Caractere));
    Stream.Read(Booleen, SizeOf(Booleen));

    // Afficher les résultats
    Message := Format('Nombre: %d' + #13#10 +
                      'Réel: %.5f' + #13#10 +
                      'Caractère: %s' + #13#10 +
                      'Booléen: %s',
                      [Nombre, Reel, Caractere, BoolToStr(Booleen, True)]);
    ShowMessage(Message);
  finally
    Stream.Free;
  end;
end;
```

**Important :**
- Les données doivent être lues **dans le même ordre** qu'elles ont été écrites
- Les types de données doivent correspondre exactement

### Écriture et lecture de chaînes de caractères

Les chaînes nécessitent une attention particulière car leur longueur est variable :

```pascal
// Écrire une chaîne
procedure EcrireChaine(Stream: TFileStream; const S: string);  
var  
  Longueur: Integer;
  Bytes: TBytes;
begin
  // Convertir la chaîne en bytes UTF-8
  Bytes := TEncoding.UTF8.GetBytes(S);
  Longueur := Length(Bytes);

  // Écrire d'abord la longueur
  Stream.Write(Longueur, SizeOf(Longueur));

  // Puis écrire les données
  if Longueur > 0 then
    Stream.Write(Bytes[0], Longueur);
end;

// Lire une chaîne
function LireChaine(Stream: TFileStream): string;  
var  
  Longueur: Integer;
  Bytes: TBytes;
begin
  // Lire la longueur
  Stream.Read(Longueur, SizeOf(Longueur));

  // Lire les données
  if Longueur > 0 then
  begin
    SetLength(Bytes, Longueur);
    Stream.Read(Bytes[0], Longueur);
    Result := TEncoding.UTF8.GetString(Bytes);
  end
  else
    Result := '';
end;

// Exemple d'utilisation
procedure ExempleChaines;  
var  
  Stream: TFileStream;
  Texte, TexteLu: string;
begin
  // Écriture
  Stream := TFileStream.Create('textes.bin', fmCreate);
  try
    EcrireChaine(Stream, 'Bonjour');
    EcrireChaine(Stream, 'Delphi');
    EcrireChaine(Stream, 'Fichiers binaires !');
  finally
    Stream.Free;
  end;

  // Lecture
  Stream := TFileStream.Create('textes.bin', fmOpenRead);
  try
    TexteLu := LireChaine(Stream) + ' ' +
               LireChaine(Stream) + ' ' +
               LireChaine(Stream);
    ShowMessage(TexteLu);
  finally
    Stream.Free;
  end;
end;
```

### Navigation dans le stream

```pascal
procedure NavigationStream;  
var  
  Stream: TFileStream;
  Position, Taille: Int64;
begin
  Stream := TFileStream.Create('donnees.bin', fmOpenRead);
  try
    // Obtenir la taille du fichier
    Taille := Stream.Size;
    ShowMessage('Taille du fichier : ' + IntToStr(Taille) + ' octets');

    // Obtenir la position actuelle
    Position := Stream.Position;
    ShowMessage('Position actuelle : ' + IntToStr(Position));

    // Se déplacer au début
    Stream.Seek(0, soBeginning);

    // Se déplacer à la fin
    Stream.Seek(0, soEnd);

    // Se déplacer de 10 octets depuis le début
    Stream.Seek(10, soBeginning);

    // Avancer de 5 octets depuis la position actuelle
    Stream.Seek(5, soCurrent);

    // Revenir 3 octets en arrière
    Stream.Seek(-3, soCurrent);
  finally
    Stream.Free;
  end;
end;
```

**Paramètres de Seek :**
- `soBeginning` (ou `soFromBeginning`) : depuis le début du fichier
- `soCurrent` (ou `soFromCurrent`) : depuis la position actuelle
- `soEnd` (ou `soFromEnd`) : depuis la fin du fichier

---

## Méthode 2 : Manipulation de Records (Structures)

Les records (enregistrements) permettent de regrouper plusieurs données. C'est idéal pour créer des formats de fichiers structurés.

### Définition et écriture d'un record

```pascal
type
  TPerson = record
    ID: Integer;
    Age: Byte;
    Salaire: Double;
    Actif: Boolean;
    // Note : éviter les string dans les records pour les fichiers binaires
  end;

procedure EcrirePersonne;  
var  
  Stream: TFileStream;
  Personne: TPerson;
begin
  Stream := TFileStream.Create('personnes.bin', fmCreate);
  try
    // Première personne
    Personne.ID := 1;
    Personne.Age := 30;
    Personne.Salaire := 45000.50;
    Personne.Actif := True;
    Stream.Write(Personne, SizeOf(TPerson));

    // Deuxième personne
    Personne.ID := 2;
    Personne.Age := 25;
    Personne.Salaire := 38000.00;
    Personne.Actif := True;
    Stream.Write(Personne, SizeOf(TPerson));

    ShowMessage('Personnes enregistrées');
  finally
    Stream.Free;
  end;
end;
```

### Lecture de records

```pascal
procedure LirePersonnes;  
var  
  Stream: TFileStream;
  Personne: TPerson;
  Texte: string;
begin
  Stream := TFileStream.Create('personnes.bin', fmOpenRead);
  try
    Texte := 'Liste des personnes :' + #13#10#13#10;

    // Lire tant qu'il y a des données
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(Personne, SizeOf(TPerson));

      Texte := Texte + Format('ID: %d, Âge: %d, Salaire: %.2f, Actif: %s' + #13#10,
        [Personne.ID, Personne.Age, Personne.Salaire,
         BoolToStr(Personne.Actif, True)]);
    end;

    ShowMessage(Texte);
  finally
    Stream.Free;
  end;
end;
```

### Record avec chaîne de longueur fixe

Pour inclure du texte dans un record binaire, utilisez des tableaux de caractères :

```pascal
type
  TPersonneAvecNom = packed record
    ID: Integer;
    Nom: array[0..49] of Char;  // 50 caractères maximum
    Age: Byte;
    Salaire: Double;
  end;

procedure EcrirePersonneAvecNom;  
var  
  Stream: TFileStream;
  Personne: TPersonneAvecNom;
begin
  Stream := TFileStream.Create('personnes_noms.bin', fmCreate);
  try
    // Initialiser à zéro
    FillChar(Personne, SizeOf(TPersonneAvecNom), 0);

    Personne.ID := 1;
    StrPCopy(Personne.Nom, 'Jean Dupont');
    Personne.Age := 30;
    Personne.Salaire := 45000.50;

    Stream.Write(Personne, SizeOf(TPersonneAvecNom));
  finally
    Stream.Free;
  end;
end;

function LirePersonneAvecNom: string;  
var  
  Stream: TFileStream;
  Personne: TPersonneAvecNom;
begin
  Stream := TFileStream.Create('personnes_noms.bin', fmOpenRead);
  try
    Stream.Read(Personne, SizeOf(TPersonneAvecNom));

    Result := Format('ID: %d' + #13#10 +
                     'Nom: %s' + #13#10 +
                     'Âge: %d' + #13#10 +
                     'Salaire: %.2f',
                     [Personne.ID,
                      StrPas(Personne.Nom),
                      Personne.Age,
                      Personne.Salaire]);
  finally
    Stream.Free;
  end;
end;
```

**Note importante :**
- Utilisez `packed record` pour éviter l'alignement mémoire automatique
- Les tableaux de Char ont une taille fixe
- `StrPCopy` copie une string dans un tableau de Char
- `StrPas` convertit un tableau de Char en string

---

## Méthode 3 : File of Type (Approche classique)

Cette approche héritée du Pascal traditionnel permet de travailler avec des fichiers typés.

### Fichier d'entiers

```pascal
procedure FichierEntiers;  
var  
  Fichier: File of Integer;
  Nombre: Integer;
  i: Integer;
begin
  // Écriture
  AssignFile(Fichier, 'nombres.dat');
  Rewrite(Fichier);
  try
    for i := 1 to 10 do
      Write(Fichier, i * i);  // Écrire les carrés de 1 à 10
  finally
    CloseFile(Fichier);
  end;

  // Lecture
  AssignFile(Fichier, 'nombres.dat');
  Reset(Fichier);
  try
    while not Eof(Fichier) do
    begin
      Read(Fichier, Nombre);
      ShowMessage(IntToStr(Nombre));
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

### Fichier de records

```pascal
type
  TArticle = packed record
    Code: Integer;
    Nom: array[0..29] of Char;
    Prix: Double;
    Stock: Integer;
  end;

procedure GererArticles;  
var  
  Fichier: File of TArticle;
  Article: TArticle;
begin
  // Créer le fichier et ajouter des articles
  AssignFile(Fichier, 'articles.dat');
  Rewrite(Fichier);
  try
    // Article 1
    FillChar(Article, SizeOf(TArticle), 0);
    Article.Code := 1;
    StrPCopy(Article.Nom, 'Ordinateur portable');
    Article.Prix := 899.99;
    Article.Stock := 15;
    Write(Fichier, Article);

    // Article 2
    FillChar(Article, SizeOf(TArticle), 0);
    Article.Code := 2;
    StrPCopy(Article.Nom, 'Souris sans fil');
    Article.Prix := 29.99;
    Article.Stock := 50;
    Write(Fichier, Article);
  finally
    CloseFile(Fichier);
  end;

  // Lire les articles
  Reset(Fichier);
  try
    while not Eof(Fichier) do
    begin
      Read(Fichier, Article);
      ShowMessage(Format('Code: %d, Nom: %s, Prix: %.2f €, Stock: %d',
        [Article.Code, StrPas(Article.Nom), Article.Prix, Article.Stock]));
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

### Accès direct (lecture d'un enregistrement spécifique)

```pascal
function LireArticleParPosition(Position: Integer): TArticle;  
var  
  Fichier: File of TArticle;
  Article: TArticle;
begin
  AssignFile(Fichier, 'articles.dat');
  Reset(Fichier);
  try
    // Se positionner sur l'enregistrement voulu (commence à 0)
    Seek(Fichier, Position);

    if not Eof(Fichier) then
      Read(Fichier, Article)
    else
      raise Exception.Create('Position invalide');

    Result := Article;
  finally
    CloseFile(Fichier);
  end;
end;

// Utilisation
var
  Article: TArticle;
begin
  Article := LireArticleParPosition(1);  // Lire le 2ème article
  ShowMessage(StrPas(Article.Nom));
end;
```

### Modification d'un enregistrement

```pascal
procedure ModifierArticle(Position: Integer; NouveauPrix: Double);  
var  
  Fichier: File of TArticle;
  Article: TArticle;
begin
  AssignFile(Fichier, 'articles.dat');
  Reset(Fichier);
  try
    // Lire l'article
    Seek(Fichier, Position);
    Read(Fichier, Article);

    // Modifier le prix
    Article.Prix := NouveauPrix;

    // Revenir en arrière et réécrire
    Seek(Fichier, Position);
    Write(Fichier, Article);
  finally
    CloseFile(Fichier);
  end;
end;
```

---

## Méthode 4 : TMemoryStream

`TMemoryStream` permet de manipuler des données binaires en mémoire avant de les sauvegarder.

### Utilisation basique

```pascal
procedure ExempleMemoryStream;  
var  
  MemStream: TMemoryStream;
  FileStream: TFileStream;
  Nombre: Integer;
  Texte: string;
begin
  MemStream := TMemoryStream.Create;
  try
    // Écrire en mémoire
    Nombre := 42;
    MemStream.Write(Nombre, SizeOf(Nombre));

    Nombre := 100;
    MemStream.Write(Nombre, SizeOf(Nombre));

    // Sauvegarder en fichier
    MemStream.SaveToFile('donnees_mem.bin');

    // Réinitialiser la position
    MemStream.Position := 0;

    // Relire depuis la mémoire
    MemStream.Read(Nombre, SizeOf(Nombre));
    ShowMessage('Premier nombre : ' + IntToStr(Nombre));

    MemStream.Read(Nombre, SizeOf(Nombre));
    ShowMessage('Deuxième nombre : ' + IntToStr(Nombre));
  finally
    MemStream.Free;
  end;
end;
```

### Charger un fichier en mémoire

```pascal
procedure TraiterFichierEnMemoire;  
var  
  MemStream: TMemoryStream;
  Nombre: Integer;
  Total: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    // Charger le fichier complet en mémoire
    MemStream.LoadFromFile('nombres.bin');

    // Traiter les données
    MemStream.Position := 0;
    Total := 0;

    while MemStream.Position < MemStream.Size do
    begin
      MemStream.Read(Nombre, SizeOf(Nombre));
      Total := Total + Nombre;
    end;

    ShowMessage('Total : ' + IntToStr(Total));
  finally
    MemStream.Free;
  end;
end;
```

---

## Format de fichier personnalisé avec en-tête

Un bon format de fichier binaire commence généralement par un en-tête (header) qui décrit le contenu.

### Exemple complet : système de configuration binaire

```pascal
type
  // En-tête du fichier
  TConfigHeader = packed record
    Signature: array[0..3] of Char;  // 'CONF'
    Version: Word;                    // Version du format
    NombreEntrees: Integer;           // Nombre de configurations
  end;

  // Une entrée de configuration
  TConfigEntry = packed record
    Nom: array[0..31] of Char;
    ValeurEntier: Integer;
    ValeurReel: Double;
    Actif: Boolean;
  end;

// Écrire un fichier de configuration
procedure CreerFichierConfig;  
var  
  Stream: TFileStream;
  Header: TConfigHeader;
  Entry: TConfigEntry;
begin
  Stream := TFileStream.Create('config.dat', fmCreate);
  try
    // Écrire l'en-tête
    Header.Signature := 'CONF';
    Header.Version := 1;
    Header.NombreEntrees := 2;
    Stream.Write(Header, SizeOf(TConfigHeader));

    // Première entrée
    FillChar(Entry, SizeOf(TConfigEntry), 0);
    StrPCopy(Entry.Nom, 'ResolutionX');
    Entry.ValeurEntier := 1920;
    Entry.ValeurReel := 0.0;
    Entry.Actif := True;
    Stream.Write(Entry, SizeOf(TConfigEntry));

    // Deuxième entrée
    FillChar(Entry, SizeOf(TConfigEntry), 0);
    StrPCopy(Entry.Nom, 'Volume');
    Entry.ValeurEntier := 0;
    Entry.ValeurReel := 0.75;
    Entry.Actif := True;
    Stream.Write(Entry, SizeOf(TConfigEntry));

    ShowMessage('Fichier de configuration créé');
  finally
    Stream.Free;
  end;
end;

// Lire le fichier de configuration
procedure LireFichierConfig;  
var  
  Stream: TFileStream;
  Header: TConfigHeader;
  Entry: TConfigEntry;
  i: Integer;
  Texte: string;
begin
  if not FileExists('config.dat') then
  begin
    ShowMessage('Fichier de configuration introuvable');
    Exit;
  end;

  Stream := TFileStream.Create('config.dat', fmOpenRead);
  try
    // Lire l'en-tête
    Stream.Read(Header, SizeOf(TConfigHeader));

    // Vérifier la signature
    if Header.Signature <> 'CONF' then
    begin
      ShowMessage('Fichier invalide !');
      Exit;
    end;

    Texte := Format('Format version %d' + #13#10 +
                    'Nombre d''entrées : %d' + #13#10#13#10,
                    [Header.Version, Header.NombreEntrees]);

    // Lire les entrées
    for i := 1 to Header.NombreEntrees do
    begin
      Stream.Read(Entry, SizeOf(TConfigEntry));

      Texte := Texte + Format('Nom: %s' + #13#10 +
                              'Valeur entière: %d' + #13#10 +
                              'Valeur réelle: %.2f' + #13#10 +
                              'Actif: %s' + #13#10#13#10,
                              [StrPas(Entry.Nom),
                               Entry.ValeurEntier,
                               Entry.ValeurReel,
                               BoolToStr(Entry.Actif, True)]);
    end;

    ShowMessage(Texte);
  finally
    Stream.Free;
  end;
end;
```

---

## Copie et comparaison de fichiers binaires

### Copier un fichier binaire

```pascal
procedure CopierFichier(const Source, Destination: string);  
var  
  SourceStream, DestStream: TFileStream;
begin
  SourceStream := TFileStream.Create(Source, fmOpenRead);
  try
    DestStream := TFileStream.Create(Destination, fmCreate);
    try
      // Copier tout le contenu
      DestStream.CopyFrom(SourceStream, 0);
      ShowMessage('Copie réussie');
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;
```

### Comparer deux fichiers binaires

```pascal
function FichiersIdentiques(const Fichier1, Fichier2: string): Boolean;  
var  
  Stream1, Stream2: TFileStream;
  Buffer1, Buffer2: array[0..4095] of Byte;
  BytesLus1, BytesLus2: Integer;
begin
  Result := False;

  if not (FileExists(Fichier1) and FileExists(Fichier2)) then
    Exit;

  Stream1 := TFileStream.Create(Fichier1, fmOpenRead);
  try
    Stream2 := TFileStream.Create(Fichier2, fmOpenRead);
    try
      // Vérifier d'abord la taille
      if Stream1.Size <> Stream2.Size then
        Exit;

      // Comparer bloc par bloc
      repeat
        BytesLus1 := Stream1.Read(Buffer1, SizeOf(Buffer1));
        BytesLus2 := Stream2.Read(Buffer2, SizeOf(Buffer2));

        if (BytesLus1 <> BytesLus2) or
           not CompareMem(@Buffer1, @Buffer2, BytesLus1) then
          Exit;

      until BytesLus1 = 0;

      Result := True;
    finally
      Stream2.Free;
    end;
  finally
    Stream1.Free;
  end;
end;
```

---

## Gestion des erreurs et bonnes pratiques

### Vérifications avant manipulation

```pascal
procedure ManipulationSecurisee(const NomFichier: string);  
var  
  Stream: TFileStream;
begin
  // Vérifier l'existence
  if not FileExists(NomFichier) then
  begin
    ShowMessage('Fichier introuvable : ' + NomFichier);
    Exit;
  end;

  // Vérifier la taille
  if TFile.GetSize(NomFichier) = 0 then
  begin
    ShowMessage('Fichier vide');
    Exit;
  end;

  // Vérifier les permissions
  try
    Stream := TFileStream.Create(NomFichier, fmOpenReadWrite);
    try
      // Manipulations...
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur d''accès au fichier : ' + E.Message);
  end;
end;
```

### Gestion complète des exceptions

```pascal
procedure LectureSecurisee(const NomFichier: string);  
var  
  Stream: TFileStream;
  Donnee: Integer;
begin
  Stream := nil;
  try
    try
      Stream := TFileStream.Create(NomFichier, fmOpenRead);

      if Stream.Size < SizeOf(Integer) then
        raise Exception.Create('Fichier trop petit');

      Stream.Read(Donnee, SizeOf(Donnee));
      ShowMessage('Donnée lue : ' + IntToStr(Donnee));

    except
      on E: EFOpenError do
        ShowMessage('Impossible d''ouvrir le fichier : ' + E.Message);
      on E: EReadError do
        ShowMessage('Erreur de lecture : ' + E.Message);
      on E: Exception do
        ShowMessage('Erreur inattendue : ' + E.Message);
    end;
  finally
    if Assigned(Stream) then
      Stream.Free;
  end;
end;
```

---

## Bonnes pratiques essentielles

### 1. Toujours utiliser packed record

```pascal
// BON
type
  TDonnees = packed record
    ID: Integer;
    Valeur: Double;
  end;

// MAUVAIS (alignement mémoire variable)
type
  TDonnees = record
    ID: Integer;
    Valeur: Double;
  end;
```

Le mot-clé `packed` garantit que la structure n'aura pas de remplissage (padding) invisible entre les champs.

### 2. Éviter les types dynamiques dans les records

```pascal
// À ÉVITER dans les fichiers binaires
type
  TMauvaisRecord = packed record
    ID: Integer;
    Nom: string;        // Taille variable !
    Liste: TStringList; // Objet complexe !
  end;

// PRÉFÉRER
type
  TBonRecord = packed record
    ID: Integer;
    Nom: array[0..49] of Char;  // Taille fixe
  end;
```

### 3. Inclure des informations de version

```pascal
type
  THeader = packed record
    Signature: array[0..3] of Char;  // Identification
    Version: Word;                    // Pour compatibilité future
    // ... autres champs
  end;
```

### 4. Toujours libérer les streams

```pascal
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create('fichier.bin', fmCreate);
  try
    // Votre code ici
  finally
    Stream.Free;  // TOUJOURS dans le bloc finally
  end;
end;
```

### 5. Vérifier la taille avant la lecture

```pascal
if Stream.Size < SizeOf(TMonRecord) then
  raise Exception.Create('Fichier corrompu ou incomplet');
```

### 6. Utiliser des constantes pour les tailles

```pascal
const
  TAILLE_NOM = 50;
  TAILLE_DESCRIPTION = 200;

type
  TArticle = packed record
    Nom: array[0..TAILLE_NOM-1] of Char;
    Description: array[0..TAILLE_DESCRIPTION-1] of Char;
  end;
```

---

## Exemple complet : Mini base de données

Voici un exemple pratique d'une petite base de données en fichier binaire :

```pascal
type
  // En-tête de la base
  TDatabaseHeader = packed record
    Signature: array[0..3] of Char;  // 'MYDB'
    Version: Word;
    NombreEnregistrements: Integer;
    DateCreation: TDateTime;
  end;

  // Enregistrement client
  TClient = packed record
    ID: Integer;
    Nom: array[0..49] of Char;
    Email: array[0..99] of Char;
    Telephone: array[0..19] of Char;
    Solde: Double;
    Actif: Boolean;
  end;

// Créer la base de données
procedure CreerBaseDonnees;  
var  
  Stream: TFileStream;
  Header: TDatabaseHeader;
begin
  Stream := TFileStream.Create('clients.db', fmCreate);
  try
    // En-tête
    Header.Signature := 'MYDB';
    Header.Version := 1;
    Header.NombreEnregistrements := 0;
    Header.DateCreation := Now;
    Stream.Write(Header, SizeOf(TDatabaseHeader));
  finally
    Stream.Free;
  end;
end;

// Ajouter un client
procedure AjouterClient(const Nom, Email, Telephone: string; Solde: Double);  
var  
  Stream: TFileStream;
  Header: TDatabaseHeader;
  Client: TClient;
begin
  Stream := TFileStream.Create('clients.db', fmOpenReadWrite);
  try
    // Lire l'en-tête
    Stream.Read(Header, SizeOf(TDatabaseHeader));

    // Préparer le nouveau client
    FillChar(Client, SizeOf(TClient), 0);
    Client.ID := Header.NombreEnregistrements + 1;
    StrPCopy(Client.Nom, Nom);
    StrPCopy(Client.Email, Email);
    StrPCopy(Client.Telephone, Telephone);
    Client.Solde := Solde;
    Client.Actif := True;

    // Se positionner à la fin
    Stream.Seek(0, soEnd);

    // Écrire le client
    Stream.Write(Client, SizeOf(TClient));

    // Mettre à jour l'en-tête
    Header.NombreEnregistrements := Header.NombreEnregistrements + 1;
    Stream.Seek(0, soBeginning);
    Stream.Write(Header, SizeOf(TDatabaseHeader));

    ShowMessage('Client ajouté avec succès');
  finally
    Stream.Free;
  end;
end;

// Lire tous les clients
function LireTousLesClients: string;  
var  
  Stream: TFileStream;
  Header: TDatabaseHeader;
  Client: TClient;
  i: Integer;
  Resultat: string;
begin
  Stream := TFileStream.Create('clients.db', fmOpenRead);
  try
    // Lire l'en-tête
    Stream.Read(Header, SizeOf(TDatabaseHeader));

    Resultat := Format('Base de données - Version %d' + #13#10 +
                       'Créée le : %s' + #13#10 +
                       'Nombre de clients : %d' + #13#10#13#10,
                       [Header.Version,
                        DateTimeToStr(Header.DateCreation),
                        Header.NombreEnregistrements]);

    // Lire chaque client
    for i := 1 to Header.NombreEnregistrements do
    begin
      Stream.Read(Client, SizeOf(TClient));

      if Client.Actif then
        Resultat := Resultat + Format('ID: %d - %s' + #13#10 +
                                      'Email: %s, Tél: %s' + #13#10 +
                                      'Solde: %.2f €' + #13#10#13#10,
                                      [Client.ID,
                                       StrPas(Client.Nom),
                                       StrPas(Client.Email),
                                       StrPas(Client.Telephone),
                                       Client.Solde]);
    end;

    Result := Resultat;
  finally
    Stream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  CreerBaseDonnees;
  AjouterClient('Jean Dupont', 'jean@email.com', '0612345678', 1500.00);
  AjouterClient('Marie Martin', 'marie@email.com', '0698765432', 2300.50);

  Memo1.Text := LireTousLesClients;
end;
```

---

## Tableau comparatif des méthodes

| Critère | TFileStream | File of Type | TMemoryStream |
|---------|-------------|--------------|---------------|
| **Modernité** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Flexibilité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Simplicité** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Performance** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Accès direct** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Mémoire** | Faible | Faible | Élevée |
| **Pour débuter** | ✅ Recommandé | ✅ OK | ⚠️ Avancé |

---

## Résumé

Dans ce chapitre, vous avez appris à manipuler des fichiers binaires en Delphi :

1. **TFileStream** : la méthode moderne et flexible, recommandée pour la plupart des usages
2. **File of Type** : l'approche classique, excellente pour les fichiers typés et l'accès direct
3. **TMemoryStream** : pour travailler en mémoire avant sauvegarde
4. **Records structurés** : pour créer des formats de fichiers personnalisés

**Points clés à retenir :**

- Les fichiers binaires sont plus compacts et rapides que les fichiers texte
- Utilisez `packed record` pour garantir la structure exacte
- Évitez les types dynamiques (string, TStringList, etc.) dans les records binaires
- Incluez toujours un en-tête avec signature et version
- Lisez les données dans le même ordre qu'elles ont été écrites
- Gérez les erreurs et vérifiez les fichiers avant manipulation
- Libérez toujours les streams avec `Free` dans un bloc `finally`

Les fichiers binaires sont particulièrement utiles pour :
- Créer des formats de données personnalisés
- Stocker des configurations complexes
- Implémenter des mini bases de données
- Sauvegarder l'état d'une application
- Échanger des données entre applications

Avec ces connaissances, vous êtes maintenant capable de créer vos propres formats de fichiers binaires et de manipuler efficacement des données structurées !

⏭️ [Utilisation des TStream et classes dérivées](/07-gestion-des-fichiers-et-flux-de-donnees/03-utilisation-des-tstream-et-classes-derivees.md)
