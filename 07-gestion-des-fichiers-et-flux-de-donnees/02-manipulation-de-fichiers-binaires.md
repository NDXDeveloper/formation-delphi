# 7. Gestion des fichiers et flux de données

## 7.2 Manipulation de fichiers binaires

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Contrairement aux fichiers texte, les fichiers binaires stockent les données sous leur forme brute, sans conversion en caractères lisibles. Ils sont utilisés pour stocker des images, des sons, des structures de données complexes ou tout autre contenu non textuel.

### Introduction aux fichiers binaires

Les fichiers binaires sont généralement plus compacts et plus rapides à traiter que les fichiers texte pour certains types de données. En Delphi, il existe plusieurs méthodes pour manipuler ces fichiers, des approches traditionnelles Pascal aux méthodes modernes orientées objet.

### Différences entre fichiers texte et fichiers binaires

| Fichiers texte | Fichiers binaires |
|----------------|-------------------|
| Stockent du texte lisible | Stockent des données brutes |
| Peuvent être ouverts dans un éditeur de texte | Nécessitent généralement un programme spécifique |
| Utilisent des caractères de fin de ligne | N'ont pas de concept de "ligne" |
| Conversion automatique entre formats texte | Pas de conversion - les octets sont écrits tels quels |

### Approche traditionnelle

Comme pour les fichiers texte, Delphi permet l'utilisation de la méthode traditionnelle Pascal pour les fichiers binaires, mais avec le type `File` au lieu de `TextFile`.

#### Lecture d'un fichier binaire

```pascal
procedure LireFichierBinaire(const NomFichier: string);
var
  Fichier: File;
  Buffer: array[0..1023] of Byte;  // Un tampon de 1024 octets
  NbOctetsLus: Integer;
begin
  AssignFile(Fichier, NomFichier);
  try
    Reset(Fichier, 1);  // Le "1" indique la taille d'un bloc (1 octet)

    // Lire des blocs jusqu'à la fin du fichier
    while not Eof(Fichier) do
    begin
      BlockRead(Fichier, Buffer, SizeOf(Buffer), NbOctetsLus);

      // Utiliser les données lues (NbOctetsLus octets dans Buffer)
      // Par exemple, les afficher en hexadécimal
      Memo1.Lines.Add(Format('Lu %d octets', [NbOctetsLus]));
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

#### Écriture dans un fichier binaire

```pascal
procedure EcrireFichierBinaire(const NomFichier: string);
var
  Fichier: File;
  Buffer: array[0..9] of Byte;
begin
  // Préparer quelques données pour l'exemple
  for var i := 0 to 9 do
    Buffer[i] := i * 10;  // Valeurs 0, 10, 20, ..., 90

  AssignFile(Fichier, NomFichier);
  try
    Rewrite(Fichier, 1);  // Créer ou écraser le fichier, bloc de 1 octet

    // Écrire le tableau d'octets dans le fichier
    BlockWrite(Fichier, Buffer, SizeOf(Buffer));
  finally
    CloseFile(Fichier);
  end;
end;
```

### Approche avec types spécifiques

On peut aussi écrire et lire des variables typées directement :

```pascal
type
  TPersonne = record
    Nom: string[50];  // Chaîne de caractères de longueur fixe
    Age: Integer;
    Taille: Double;
  end;

procedure EcrireEnregistrement(const NomFichier: string);
var
  Fichier: File of TPersonne;
  Personne: TPersonne;
begin
  Personne.Nom := 'Dupont';
  Personne.Age := 30;
  Personne.Taille := 1.75;

  AssignFile(Fichier, NomFichier);
  try
    Rewrite(Fichier);  // Taille de bloc automatique basée sur TPersonne

    // Écrire l'enregistrement
    Write(Fichier, Personne);
  finally
    CloseFile(Fichier);
  end;
end;

procedure LireEnregistrement(const NomFichier: string);
var
  Fichier: File of TPersonne;
  Personne: TPersonne;
begin
  AssignFile(Fichier, NomFichier);
  try
    Reset(Fichier);

    if not Eof(Fichier) then
    begin
      Read(Fichier, Personne);

      // Utiliser les données lues
      ShowMessage(Format('Nom: %s, Age: %d, Taille: %.2f',
                         [Personne.Nom, Personne.Age, Personne.Taille]));
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

> **Attention**: Cette méthode à quelques limitations. Les chaînes à longueur variable et certains types complexes peuvent causer des problèmes. Pour les structures de données modernes, préférez la sérialisation (section 7.4).

### Approche moderne avec les flux (TStream)

L'approche recommandée pour manipuler des fichiers binaires en Delphi moderne est d'utiliser les classes dérivées de `TStream`, notamment `TFileStream`.

#### Lecture d'un fichier binaire avec TFileStream

```pascal
uses
  System.Classes, System.SysUtils;

procedure LireFichierAvecStream(const NomFichier: string);
var
  Flux: TFileStream;
  Buffer: TBytes;
  NbOctetsLus: Integer;
begin
  // Allouer le tampon
  SetLength(Buffer, 1024);

  // Créer le flux de fichier en lecture
  Flux := TFileStream.Create(NomFichier, fmOpenRead or fmShareDenyWrite);
  try
    // Lire jusqu'à 1024 octets du fichier
    NbOctetsLus := Flux.Read(Buffer, Length(Buffer));

    // Afficher les premiers octets lus
    var Resultat := '';
    for var i := 0 to Min(NbOctetsLus, 20) - 1 do
      Resultat := Resultat + Format('%2.2x ', [Buffer[i]]);

    ShowMessage(Format('Lu %d octets. Début: %s', [NbOctetsLus, Resultat]));

    // Obtenir la taille totale du fichier
    ShowMessage(Format('Taille totale du fichier: %d octets', [Flux.Size]));

    // Repositionner le curseur au début du fichier
    Flux.Position := 0;

    // Lire d'autres données si nécessaire...

  finally
    Flux.Free;
  end;
end;
```

#### Écriture dans un fichier binaire avec TFileStream

```pascal
procedure EcrireFichierAvecStream(const NomFichier: string);
var
  Flux: TFileStream;
  Donnees: TBytes;
begin
  // Préparer quelques données
  SetLength(Donnees, 10);
  for var i := 0 to 9 do
    Donnees[i] := i * 10;

  // Créer le flux de fichier en écriture
  Flux := TFileStream.Create(NomFichier, fmCreate);
  try
    // Écrire les données dans le fichier
    Flux.WriteBuffer(Donnees, Length(Donnees));

    // Ajouter d'autres données à la fin
    var AutresDonnees: TBytes := [255, 254, 253];
    Flux.WriteBuffer(AutresDonnees, Length(AutresDonnees));
  finally
    Flux.Free;
  end;
end;
```

### Lecture et écriture de structures de données

Pour stocker des structures de données complexes, vous pouvez écrire et lire les champs individuellement :

```pascal
type
  TFicheClient = record
    ID: Integer;
    Nom: string;
    DateInscription: TDateTime;
    Solde: Double;
  end;

procedure EcrireFicheClient(const NomFichier: string; const Fiche: TFicheClient);
var
  Flux: TFileStream;
  NomBytes: TBytes;
begin
  Flux := TFileStream.Create(NomFichier, fmCreate);
  try
    // Écrire l'ID (4 octets)
    Flux.WriteBuffer(Fiche.ID, SizeOf(Integer));

    // Écrire le nom (longueur variable)
    NomBytes := TEncoding.UTF8.GetBytes(Fiche.Nom);

    // D'abord la longueur du nom
    var Longueur: Integer := Length(NomBytes);
    Flux.WriteBuffer(Longueur, SizeOf(Integer));

    // Puis le contenu du nom
    if Longueur > 0 then
      Flux.WriteBuffer(NomBytes, Longueur);

    // Écrire la date (8 octets)
    Flux.WriteBuffer(Fiche.DateInscription, SizeOf(TDateTime));

    // Écrire le solde (8 octets)
    Flux.WriteBuffer(Fiche.Solde, SizeOf(Double));
  finally
    Flux.Free;
  end;
end;

function LireFicheClient(const NomFichier: string): TFicheClient;
var
  Flux: TFileStream;
  NomBytes: TBytes;
  Longueur: Integer;
begin
  Result := Default(TFicheClient);

  if not FileExists(NomFichier) then
    Exit;

  Flux := TFileStream.Create(NomFichier, fmOpenRead);
  try
    // Lire l'ID
    Flux.ReadBuffer(Result.ID, SizeOf(Integer));

    // Lire la longueur du nom
    Flux.ReadBuffer(Longueur, SizeOf(Integer));

    // Lire le contenu du nom
    if Longueur > 0 then
    begin
      SetLength(NomBytes, Longueur);
      Flux.ReadBuffer(NomBytes, Longueur);
      Result.Nom := TEncoding.UTF8.GetString(NomBytes);
    end
    else
      Result.Nom := '';

    // Lire la date
    Flux.ReadBuffer(Result.DateInscription, SizeOf(TDateTime));

    // Lire le solde
    Flux.ReadBuffer(Result.Solde, SizeOf(Double));
  finally
    Flux.Free;
  end;
end;
```

### Utilisation de la classe TMemoryStream

Pour manipuler des données binaires en mémoire avant de les sauvegarder, vous pouvez utiliser `TMemoryStream` :

```pascal
procedure ManipulerDonneesAvantSauvegarde;
var
  MemStream: TMemoryStream;
  FileStream: TFileStream;
  Valeur: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    // Écrire des données dans le flux mémoire
    Valeur := 12345;
    MemStream.WriteBuffer(Valeur, SizeOf(Integer));

    Valeur := 67890;
    MemStream.WriteBuffer(Valeur, SizeOf(Integer));

    // Ajouter d'autres données...

    // Revenir au début du flux mémoire
    MemStream.Position := 0;

    // Sauvegarder le contenu dans un fichier
    FileStream := TFileStream.Create('donnees.bin', fmCreate);
    try
      // Copier tout le contenu du MemoryStream dans le fichier
      FileStream.CopyFrom(MemStream, 0);  // 0 signifie tout copier
    finally
      FileStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;
```

### Manipulation de fichiers binaires communs

#### Exemple : Manipulation d'une image

```pascal
procedure SauvegarderImageDepuisImage(const NomFichier: string; Image: TImage);
var
  Flux: TFileStream;
begin
  Flux := TFileStream.Create(NomFichier, fmCreate);
  try
    if Assigned(Image.Picture.Graphic) then
      Image.Picture.Graphic.SaveToStream(Flux);
  finally
    Flux.Free;
  end;
end;

procedure ChargerImageVersImage(const NomFichier: string; Image: TImage);
var
  Flux: TFileStream;
begin
  if not FileExists(NomFichier) then
    Exit;

  Flux := TFileStream.Create(NomFichier, fmOpenRead);
  try
    Image.Picture.LoadFromStream(Flux);
  finally
    Flux.Free;
  end;
end;
```

#### Exemple : Créer un fichier d'en-tête simple

Les fichiers binaires ont souvent un en-tête avec des informations sur leur contenu :

```pascal
type
  TEnTeteFichier = packed record
    Signature: array[0..3] of AnsiChar;  // Identifiant du format
    Version: Word;                       // Version du format
    NbElements: Integer;                 // Nombre d'éléments stockés
    DateCreation: TDateTime;             // Date de création
  end;

procedure CreerFichierAvecEnTete(const NomFichier: string);
var
  Flux: TFileStream;
  EnTete: TEnTeteFichier;
begin
  // Initialiser l'en-tête
  EnTete.Signature := 'MYAP';  // Signature de mon application
  EnTete.Version := 1;
  EnTete.NbElements := 0;
  EnTete.DateCreation := Now;

  // Créer le fichier et écrire l'en-tête
  Flux := TFileStream.Create(NomFichier, fmCreate);
  try
    Flux.WriteBuffer(EnTete, SizeOf(TEnTeteFichier));

    // Écrire les données suivant l'en-tête...

  finally
    Flux.Free;
  end;
end;

function VerifierFichierValide(const NomFichier: string): Boolean;
var
  Flux: TFileStream;
  EnTete: TEnTeteFichier;
begin
  Result := False;

  if not FileExists(NomFichier) then
    Exit;

  // Vérifier que le fichier a au moins la taille de l'en-tête
  if TFile.GetSize(NomFichier) < SizeOf(TEnTeteFichier) then
    Exit;

  Flux := TFileStream.Create(NomFichier, fmOpenRead);
  try
    // Lire l'en-tête
    Flux.ReadBuffer(EnTete, SizeOf(TEnTeteFichier));

    // Vérifier la signature
    Result := (EnTete.Signature = 'MYAP') and (EnTete.Version <= 1);
  finally
    Flux.Free;
  end;
end;
```

### Utilisation de System.IOUtils pour les fichiers binaires

L'unité `System.IOUtils` offre aussi des méthodes pour les fichiers binaires :

```pascal
uses
  System.IOUtils, System.SysUtils;

procedure ExempleIOUtilsBinaire;
var
  Donnees: TBytes;
begin
  // Préparer des données
  SetLength(Donnees, 10);
  for var i := 0 to 9 do
    Donnees[i] := i * 10;

  // Écrire les données
  TFile.WriteAllBytes('donnees.bin', Donnees);

  // Lire les données
  var DonneesLues := TFile.ReadAllBytes('donnees.bin');

  // Vérifier le contenu
  var Resultat := '';
  for var i := 0 to Length(DonneesLues) - 1 do
    Resultat := Resultat + Format('%d ', [DonneesLues[i]]);

  ShowMessage('Données lues : ' + Resultat);
end;
```

> **Note :** La classe `TFile` avec les méthodes `ReadAllBytes` et `WriteAllBytes` nécessite Delphi 11 ou supérieur.

### Conseils pratiques

1. **Toujours fermer les flux** : Utilisez des blocs `try...finally` pour vous assurer que les ressources sont libérées.

2. **Vérifiez les valeurs de retour** : Les méthodes `Read` et `Write` des flux retournent le nombre d'octets effectivement lus ou écrits.

3. **Format de données personnalisé** : Si vous créez votre propre format de fichier binaire, ajoutez toujours une signature et une version pour faciliter la compatibilité future.

4. **Champs de longueur fixe vs variable** : Pour les champs de longueur variable (comme les chaînes), stockez d'abord la longueur, puis les données.

5. **Utilisez `packed record`** : Pour les structures qui seront écrites/lues directement avec `WriteBuffer`/`ReadBuffer`, utilisez le mot-clé `packed` pour éviter l'alignement mémoire.

6. **Attention à la portabilité** : Les tailles de types peuvent varier selon les plateformes. Utilisez des types de taille fixe (`Int32`, `UInt16`, etc.) pour des fichiers portables.

### Exemple complet : Gestionnaire de données binaires

Voici un exemple simple d'application qui sauvegarde une liste de personnes dans un fichier binaire :

```pascal
type
  TPersonne = record
    ID: Integer;
    Nom: string;
    Age: Integer;
  end;

procedure TFormGestionnaire.ButtonSauvegarderClick(Sender: TObject);
var
  Flux: TFileStream;
  Personne: TPersonne;
  NomBytes: TBytes;
  Longueur: Integer;
begin
  if SaveDialog1.Execute then
  begin
    Flux := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      // Nombre de personnes
      var NbPersonnes: Integer := ListBox1.Items.Count;
      Flux.WriteBuffer(NbPersonnes, SizeOf(Integer));

      // Enregistrer chaque personne
      for var i := 0 to ListBox1.Items.Count - 1 do
      begin
        Personne := TPersonne(ListBox1.Items.Objects[i]);

        // Écrire l'ID
        Flux.WriteBuffer(Personne.ID, SizeOf(Integer));

        // Écrire le nom
        NomBytes := TEncoding.UTF8.GetBytes(Personne.Nom);
        Longueur := Length(NomBytes);
        Flux.WriteBuffer(Longueur, SizeOf(Integer));
        if Longueur > 0 then
          Flux.WriteBuffer(NomBytes[0], Longueur);

        // Écrire l'âge
        Flux.WriteBuffer(Personne.Age, SizeOf(Integer));
      end;

      ShowMessage('Données sauvegardées avec succès !');
    finally
      Flux.Free;
    end;
  end;
end;

procedure TFormGestionnaire.ButtonChargerClick(Sender: TObject);
var
  Flux: TFileStream;
  Personne: TPersonne;
  NomBytes: TBytes;
  Longueur, NbPersonnes: Integer;
begin
  if OpenDialog1.Execute then
  begin
    if not FileExists(OpenDialog1.FileName) then
    begin
      ShowMessage('Le fichier n''existe pas !');
      Exit;
    end;

    Flux := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // Lire le nombre de personnes
      Flux.ReadBuffer(NbPersonnes, SizeOf(Integer));

      // Vider la liste actuelle
      for var i := 0 to ListBox1.Items.Count - 1 do
        TObject(ListBox1.Items.Objects[i]).Free;
      ListBox1.Clear;

      // Lire chaque personne
      for var i := 0 to NbPersonnes - 1 do
      begin
        // Lire l'ID
        Flux.ReadBuffer(Personne.ID, SizeOf(Integer));

        // Lire le nom
        Flux.ReadBuffer(Longueur, SizeOf(Integer));
        if Longueur > 0 then
        begin
          SetLength(NomBytes, Longueur);
          Flux.ReadBuffer(NomBytes[0], Longueur);
          Personne.Nom := TEncoding.UTF8.GetString(NomBytes);
        end
        else
          Personne.Nom := '';

        // Lire l'âge
        Flux.ReadBuffer(Personne.Age, SizeOf(Integer));

        // Ajouter à la liste
        var NouvellePersonne := TPersonne.Create;
        NouvellePersonne.ID := Personne.ID;
        NouvellePersonne.Nom := Personne.Nom;
        NouvellePersonne.Age := Personne.Age;

        ListBox1.Items.AddObject(
          Format('%d - %s (%d ans)', [Personne.ID, Personne.Nom, Personne.Age]),
          NouvellePersonne);
      end;

      ShowMessage('Données chargées avec succès !');
    finally
      Flux.Free;
    end;
  end;
end;
```

### Exercice pratique

Créez une application simple qui :
1. Permet à l'utilisateur de dessiner des formes sur un TImage
2. Sauvegarde le dessin dans un fichier binaire personnalisé
3. Charge le dessin depuis ce fichier

Structure suggérée pour le fichier :
- En-tête : signature, version, nombre de formes
- Pour chaque forme : type (rectangle, cercle...), position, couleur, taille

Cet exercice vous permettra de mettre en pratique les concepts de manipulation de fichiers binaires tout en créant quelque chose de visuel.

---

À suivre dans la prochaine section : **7.3 Utilisation des TStream et classes dérivées**

⏭️ [Utilisation des TStream et classes dérivées](/07-gestion-des-fichiers-et-flux-de-donnees/03-utilisation-des-tstream-et-classes-derivees.md)
