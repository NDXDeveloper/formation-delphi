# 7. Gestion des fichiers et flux de données

## 7.1 Lecture/écriture de fichiers texte

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La manipulation de fichiers texte est une opération fondamentale dans de nombreuses applications. Delphi offre plusieurs méthodes pour lire et écrire des fichiers texte, des plus simples aux plus avancées.

### Introduction

Les fichiers texte sont parmi les types de fichiers les plus courants et les plus simples à manipuler. Ils peuvent contenir des informations de configuration, des journaux, des données exportées, ou tout autre contenu textuel.

Delphi propose différentes approches pour gérer ces fichiers :
- Approche traditionnelle (Pascal)
- Approche orientée objet avec les classes `TStringList` et `TStrings`
- Approche moderne avec les méthodes de l'unité `System.IOUtils`

### Approche traditionnelle

Cette méthode héritée du Pascal standard utilise les types `TextFile` et des procédures comme `AssignFile`, `Reset`, `Rewrite`, `Read`, `Write` et `CloseFile`.

#### Lecture d'un fichier texte

```pascal
procedure LireFichierTexte(const NomFichier: string);
var
  Fichier: TextFile;
  Ligne: string;
begin
  AssignFile(Fichier, NomFichier);
  try
    Reset(Fichier); // Ouvre le fichier en lecture
    while not Eof(Fichier) do
    begin
      ReadLn(Fichier, Ligne);
      // Utiliser la ligne lue (par exemple, l'afficher dans un mémo)
      Memo1.Lines.Add(Ligne);
    end;
  finally
    CloseFile(Fichier); // Ferme le fichier
  end;
end;
```

#### Écriture dans un fichier texte

```pascal
procedure EcrireFichierTexte(const NomFichier: string; const Contenu: string);
var
  Fichier: TextFile;
begin
  AssignFile(Fichier, NomFichier);
  try
    Rewrite(Fichier); // Crée ou écrase le fichier existant
    WriteLn(Fichier, Contenu);
  finally
    CloseFile(Fichier); // Ferme le fichier
  end;
end;
```

#### Ajout à un fichier texte existant

```pascal
procedure AjouterAuFichierTexte(const NomFichier: string; const Contenu: string);
var
  Fichier: TextFile;
begin
  AssignFile(Fichier, NomFichier);
  try
    if FileExists(NomFichier) then
      Append(Fichier) // Ouvre le fichier en ajout
    else
      Rewrite(Fichier); // Crée un nouveau fichier s'il n'existe pas

    WriteLn(Fichier, Contenu);
  finally
    CloseFile(Fichier); // Ferme le fichier
  end;
end;
```

### Approche orientée objet avec TStringList

La classe `TStringList` est très pratique car elle permet de manipuler un ensemble de lignes de texte en mémoire avant de les sauvegarder dans un fichier, ou après les avoir chargées.

#### Lecture d'un fichier avec TStringList

```pascal
procedure LireFichierAvecStringList(const NomFichier: string);
var
  ListeTexte: TStringList;
begin
  ListeTexte := TStringList.Create;
  try
    ListeTexte.LoadFromFile(NomFichier);

    // Tout le contenu est maintenant dans ListeTexte
    // Exemple : copier dans un mémo
    Memo1.Lines.Assign(ListeTexte);

    // Accéder à une ligne spécifique (indice commence à 0)
    if ListeTexte.Count > 0 then
      ShowMessage('Première ligne : ' + ListeTexte[0]);
  finally
    ListeTexte.Free; // Libère la mémoire
  end;
end;
```

#### Écriture dans un fichier avec TStringList

```pascal
procedure EcrireFichierAvecStringList(const NomFichier: string);
var
  ListeTexte: TStringList;
begin
  ListeTexte := TStringList.Create;
  try
    // Ajouter des lignes de texte
    ListeTexte.Add('Première ligne');
    ListeTexte.Add('Deuxième ligne');
    ListeTexte.Add('Troisième ligne');

    // Ou copier depuis un composant mémo
    // ListeTexte.Assign(Memo1.Lines);

    // Sauvegarder dans un fichier
    ListeTexte.SaveToFile(NomFichier);
  finally
    ListeTexte.Free; // Libère la mémoire
  end;
end;
```

### Approche moderne avec System.IOUtils

L'unité `System.IOUtils` (introduite dans les versions récentes de Delphi) propose la classe `TFile` avec des méthodes statiques modernes et simplifiées.

```pascal
uses
  System.IOUtils, System.SysUtils;
```

#### Lecture d'un fichier avec TFile

```pascal
procedure LireFichierAvecTFile(const NomFichier: string);
var
  Contenu: string;
  Lignes: TArray<string>;
begin
  try
    // Lire tout le fichier en une seule chaîne
    Contenu := TFile.ReadAllText(NomFichier);
    ShowMessage('Contenu complet : ' + Contenu);

    // Ou lire toutes les lignes dans un tableau
    Lignes := TFile.ReadAllLines(NomFichier);

    // Parcourir les lignes
    for var Ligne in Lignes do
      Memo1.Lines.Add(Ligne);

  except
    on E: Exception do
      ShowMessage('Erreur de lecture : ' + E.Message);
  end;
end;
```

#### Écriture dans un fichier avec TFile

```pascal
procedure EcrireFichierAvecTFile(const NomFichier: string);
var
  Lignes: TArray<string>;
begin
  try
    // Écrire une chaîne complète
    TFile.WriteAllText(NomFichier, 'Contenu du fichier');

    // Ou écrire un tableau de lignes
    Lignes := ['Première ligne', 'Deuxième ligne', 'Troisième ligne'];
    TFile.WriteAllLines(NomFichier, Lignes);

  except
    on E: Exception do
      ShowMessage('Erreur d''écriture : ' + E.Message);
  end;
end;
```

#### Ajout de texte à un fichier avec TFile

```pascal
procedure AjouterTexteTFile(const NomFichier, NouveauTexte: string);
begin
  try
    // Vérifier si le fichier existe
    if TFile.Exists(NomFichier) then
    begin
      // Lire le contenu existant
      var ContenuExistant := TFile.ReadAllText(NomFichier);

      // Ajouter le nouveau texte
      var NouveauContenu := ContenuExistant + NouveauTexte;

      // Écrire le tout
      TFile.WriteAllText(NomFichier, NouveauContenu);
    end
    else
      // Créer le fichier s'il n'existe pas
      TFile.WriteAllText(NomFichier, NouveauTexte);

    ShowMessage('Texte ajouté avec succès');
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ajout de texte : ' + E.Message);
  end;
end;
```

> **Note:** Les méthodes utilisant `TFile` et certaines syntaxes comme les variables déclarées dans les boucles for (`for var Ligne in...`) nécessitent Delphi 11 ou supérieur.

### Gestion des encodages

Delphi gère différents encodages de texte. Par défaut, les méthodes traditionnelles utilisent l'encodage système, mais vous pouvez spécifier un encodage particulier avec les méthodes modernes :

```pascal
uses
  System.IOUtils, System.SysUtils, System.Classes;

// Lecture avec encodage spécifique
procedure LireFichierAvecEncodage(const NomFichier: string);
var
  ListeTexte: TStringList;
begin
  ListeTexte := TStringList.Create;
  try
    // Spécifier l'encodage UTF-8
    ListeTexte.LoadFromFile(NomFichier, TEncoding.UTF8);
    Memo1.Lines.Assign(ListeTexte);
  finally
    ListeTexte.Free;
  end;
end;

// Écriture avec encodage spécifique
procedure EcrireFichierAvecEncodage(const NomFichier: string);
begin
  // Avec TFile
  TFile.WriteAllText(NomFichier, 'Texte avec caractères spéciaux : éàçèù', TEncoding.UTF8);

  // Ou avec TStringList
  var ListeTexte := TStringList.Create;
  try
    ListeTexte.Add('Ligne avec caractères spéciaux : éàçèù');
    ListeTexte.SaveToFile(NomFichier, TEncoding.UTF8);
  finally
    ListeTexte.Free;
  end;
end;
```

### Exemple pratique : Éditeur de texte simple

Voici un exemple d'application simple d'éditeur de texte utilisant les techniques présentées :

```pascal
procedure TFormEditeur.ButtonOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      MemoTexte.Lines.LoadFromFile(OpenDialog1.FileName);
      StatusBar1.SimpleText := 'Fichier ouvert : ' + OpenDialog1.FileName;
      FichierCourant := OpenDialog1.FileName;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''ouverture du fichier : ' + E.Message);
    end;
  end;
end;

procedure TFormEditeur.ButtonEnregistrerClick(Sender: TObject);
begin
  if FichierCourant = '' then
  begin
    if SaveDialog1.Execute then
      FichierCourant := SaveDialog1.FileName
    else
      Exit;
  end;

  try
    MemoTexte.Lines.SaveToFile(FichierCourant);
    StatusBar1.SimpleText := 'Fichier enregistré : ' + FichierCourant;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''enregistrement du fichier : ' + E.Message);
  end;
end;
```

### Conseils et astuces

1. **Toujours libérer les ressources** : Utilisez des blocs `try...finally` pour vous assurer que les fichiers sont fermés et les objets libérés, même en cas d'erreur.

2. **Gestion des erreurs** : Utilisez des blocs `try...except` pour gérer les erreurs potentielles (fichier inexistant, pas de permission, etc.).

3. **Chemins de fichiers** : Pour les applications portables, utilisez des chemins relatifs ou stockez les fichiers dans des dossiers spécifiques :
   ```pascal
   // Obtenir le dossier des documents de l'utilisateur
   var DossierDocuments := TPath.GetDocumentsPath;
   var CheminFichier := TPath.Combine(DossierDocuments, 'MonFichier.txt');
   ```

4. **Encodage adapté** : Choisissez l'encodage approprié pour votre application, notamment UTF-8 pour la compatibilité internationale.

5. **Performance** : Pour les petits fichiers, `TStringList` est pratique. Pour les très grands fichiers, l'approche par flux (que nous verrons dans les sections suivantes) est plus performante.

### Exercice pratique

Créez une application avec :
- Un mémo pour afficher et éditer du texte
- Des boutons pour ouvrir et enregistrer des fichiers
- Un indicateur d'état pour montrer si le fichier a été modifié

Cette application servira de base pour les exercices des sections suivantes où nous ajouterons des fonctionnalités plus avancées.

---

À suivre dans la prochaine section : **7.2 Manipulation de fichiers binaires**

⏭️ [Manipulation de fichiers binaires](/07-gestion-des-fichiers-et-flux-de-donnees/02-manipulation-de-fichiers-binaires.md)
