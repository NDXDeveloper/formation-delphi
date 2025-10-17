🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.1 Lecture/écriture de fichiers texte

## Introduction

La manipulation de fichiers texte est une compétence fondamentale en programmation. Dans Delphi, vous disposez de plusieurs méthodes pour lire et écrire des fichiers texte, allant des approches classiques héritées de Pascal aux méthodes modernes et plus simples.

Dans ce chapitre, nous allons explorer les différentes techniques disponibles, en privilégiant les approches les plus accessibles pour les débutants tout en présentant également les méthodes traditionnelles.

## Pourquoi manipuler des fichiers texte ?

Les fichiers texte sont partout dans la programmation :
- Fichiers de configuration (`.ini`, `.cfg`, `.conf`)
- Fichiers de logs pour tracer l'activité d'une application
- Export/import de données simples
- Fichiers CSV (Comma-Separated Values)
- Fichiers de données structurées (JSON, XML)

## Les différentes approches en Delphi

Delphi offre trois approches principales pour manipuler des fichiers texte :

1. **L'approche classique avec TextFile** : héritée du Pascal, plus verbeuse mais très contrôlée
2. **L'approche avec TStringList** : simple et pratique pour des fichiers de taille moyenne
3. **L'approche moderne avec TFile** : introduite dans les versions récentes, la plus concise

Nous allons explorer ces trois méthodes en commençant par les plus simples.

---

## Méthode 1 : Utilisation de TStringList (Recommandée pour débuter)

`TStringList` est une classe qui représente une liste de chaînes de caractères. C'est l'approche la plus simple et la plus intuitive pour les débutants.

### Lecture d'un fichier texte avec TStringList

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Lignes: TStringList;
  i: Integer;
begin
  // Créer une instance de TStringList
  Lignes := TStringList.Create;
  try
    // Charger le contenu du fichier
    Lignes.LoadFromFile('C:\MonFichier.txt');

    // Afficher le contenu ligne par ligne
    for i := 0 to Lignes.Count - 1 do
    begin
      ShowMessage('Ligne ' + IntToStr(i + 1) + ': ' + Lignes[i]);
    end;

    // Ou afficher tout le contenu dans un Memo
    Memo1.Lines.Text := Lignes.Text;

  finally
    // Libérer la mémoire (très important !)
    Lignes.Free;
  end;
end;
```

**Points clés à retenir :**
- `Create` : crée l'objet en mémoire
- `LoadFromFile` : charge le fichier entier en mémoire
- `Count` : nombre de lignes dans le fichier
- `Lignes[i]` : accès à la ligne d'index i (commence à 0)
- `Free` : libère la mémoire (dans le bloc `finally` pour garantir l'exécution)

### Écriture d'un fichier texte avec TStringList

```pascal
procedure TForm1.Button2Click(Sender: TObject);
var
  Lignes: TStringList;
begin
  Lignes := TStringList.Create;
  try
    // Ajouter des lignes
    Lignes.Add('Première ligne');
    Lignes.Add('Deuxième ligne');
    Lignes.Add('Troisième ligne');

    // Ou copier depuis un Memo
    Lignes.Text := Memo1.Lines.Text;

    // Sauvegarder dans un fichier
    Lignes.SaveToFile('C:\MonNouveauFichier.txt');

    ShowMessage('Fichier enregistré avec succès !');
  finally
    Lignes.Free;
  end;
end;
```

### Ajout de contenu à un fichier existant

```pascal
procedure AjouterLigneAuFichier(const NomFichier, NouvelleLigne: string);
var
  Lignes: TStringList;
begin
  Lignes := TStringList.Create;
  try
    // Charger le fichier existant (s'il existe)
    if FileExists(NomFichier) then
      Lignes.LoadFromFile(NomFichier);

    // Ajouter la nouvelle ligne
    Lignes.Add(NouvelleLigne);

    // Sauvegarder
    Lignes.SaveToFile(NomFichier);
  finally
    Lignes.Free;
  end;
end;
```

### Gestion de l'encodage avec TStringList

Par défaut, TStringList utilise l'encodage ANSI. Pour gérer différents encodages (notamment UTF-8 pour les caractères accentués) :

```pascal
procedure LireFichierUTF8;
var
  Lignes: TStringList;
begin
  Lignes := TStringList.Create;
  try
    // Charger un fichier UTF-8
    Lignes.LoadFromFile('C:\FichierUTF8.txt', TEncoding.UTF8);

    Memo1.Lines.Text := Lignes.Text;
  finally
    Lignes.Free;
  end;
end;

procedure EcrireFichierUTF8;
var
  Lignes: TStringList;
begin
  Lignes := TStringList.Create;
  try
    Lignes.Add('Texte avec accents : é, è, à, ç');

    // Sauvegarder en UTF-8
    Lignes.SaveToFile('C:\FichierUTF8.txt', TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;
```

**Encodages disponibles :**
- `TEncoding.ANSI` : encodage Windows par défaut
- `TEncoding.UTF8` : recommandé pour les caractères internationaux
- `TEncoding.Unicode` : UTF-16
- `TEncoding.ASCII` : caractères ASCII de base uniquement

---

## Méthode 2 : Approche moderne avec TFile (Delphi XE et supérieur)

La classe `TFile` (dans l'unité `System.IOUtils`) offre des méthodes statiques très concises pour manipuler les fichiers.

### Lecture d'un fichier complet

```pascal
uses
  System.IOUtils;

procedure LireFichierSimple;
var
  Contenu: string;
  Lignes: TArray<string>;
  Ligne: string;
begin
  // Lire tout le fichier en une seule chaîne
  Contenu := TFile.ReadAllText('C:\MonFichier.txt');
  Memo1.Text := Contenu;

  // Ou lire ligne par ligne dans un tableau
  Lignes := TFile.ReadAllLines('C:\MonFichier.txt');
  for Ligne in Lignes do
  begin
    ShowMessage(Ligne);
  end;
end;
```

### Écriture dans un fichier

```pascal
procedure EcrireFichierSimple;
var
  Lignes: TArray<string>;
begin
  // Écrire une chaîne complète (écrase le fichier existant)
  TFile.WriteAllText('C:\Sortie.txt', 'Contenu du fichier');

  // Écrire un tableau de lignes
  SetLength(Lignes, 3);
  Lignes[0] := 'Ligne 1';
  Lignes[1] := 'Ligne 2';
  Lignes[2] := 'Ligne 3';
  TFile.WriteAllLines('C:\Sortie.txt', Lignes);

  // Ajouter du texte à la fin d'un fichier existant
  TFile.AppendAllText('C:\Sortie.txt', 'Nouvelle ligne ajoutée');
end;
```

### Gestion de l'encodage avec TFile

```pascal
// Lire avec un encodage spécifique
var
  Contenu: string;
begin
  Contenu := TFile.ReadAllText('C:\Fichier.txt', TEncoding.UTF8);
end;

// Écrire avec un encodage spécifique
TFile.WriteAllText('C:\Fichier.txt', 'Mon texte', TEncoding.UTF8);
```

**Avantages de TFile :**
- Code très concis
- Pas besoin de gérer la création/destruction d'objets
- Gestion automatique des erreurs de base

**Limitations :**
- Charge tout le fichier en mémoire (pas adapté aux très gros fichiers)
- Moins de contrôle fin sur le processus de lecture/écriture

---

## Méthode 3 : Approche classique avec TextFile

Cette méthode, héritée du Pascal traditionnel, offre un contrôle total sur la lecture et l'écriture, ligne par ligne.

### Lecture ligne par ligne

```pascal
procedure LireFichierClassique;
var
  Fichier: TextFile;
  Ligne: string;
begin
  // Associer la variable au fichier
  AssignFile(Fichier, 'C:\MonFichier.txt');

  try
    // Ouvrir le fichier en lecture
    Reset(Fichier);

    try
      // Lire tant qu'on n'est pas à la fin
      while not Eof(Fichier) do
      begin
        ReadLn(Fichier, Ligne);
        ShowMessage(Ligne);
      end;
    finally
      // Fermer le fichier
      CloseFile(Fichier);
    end;

  except
    on E: Exception do
      ShowMessage('Erreur lors de la lecture : ' + E.Message);
  end;
end;
```

**Explications des commandes :**
- `AssignFile` : associe une variable de type TextFile à un fichier physique
- `Reset` : ouvre le fichier en lecture
- `Eof` (End of File) : retourne True quand on atteint la fin du fichier
- `ReadLn` : lit une ligne complète
- `CloseFile` : ferme le fichier (libère les ressources)

### Écriture ligne par ligne

```pascal
procedure EcrireFichierClassique;
var
  Fichier: TextFile;
begin
  AssignFile(Fichier, 'C:\Sortie.txt');

  try
    // Ouvrir en écriture (écrase le fichier existant)
    Rewrite(Fichier);

    try
      WriteLn(Fichier, 'Première ligne');
      WriteLn(Fichier, 'Deuxième ligne');
      WriteLn(Fichier, 'Troisième ligne');
    finally
      CloseFile(Fichier);
    end;

  except
    on E: Exception do
      ShowMessage('Erreur lors de l''écriture : ' + E.Message);
  end;
end;
```

### Ajout à la fin d'un fichier existant

```pascal
procedure AjouterAuFichierClassique;
var
  Fichier: TextFile;
begin
  AssignFile(Fichier, 'C:\Journal.txt');

  try
    // Ouvrir en mode ajout
    Append(Fichier);

    try
      WriteLn(Fichier, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' : Nouvelle entrée');
    finally
      CloseFile(Fichier);
    end;

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

**Modes d'ouverture :**
- `Reset(Fichier)` : ouvre en lecture
- `Rewrite(Fichier)` : ouvre en écriture (crée ou écrase le fichier)
- `Append(Fichier)` : ouvre en ajout (ajoute à la fin d'un fichier existant)

---

## Gestion des erreurs et vérifications

Avant de manipuler un fichier, il est recommandé de vérifier son existence et de gérer les erreurs potentielles.

### Vérifier l'existence d'un fichier

```pascal
uses
  System.SysUtils;

procedure VerifierFichier;
begin
  if FileExists('C:\MonFichier.txt') then
    ShowMessage('Le fichier existe')
  else
    ShowMessage('Le fichier n''existe pas');
end;
```

### Gestion complète des erreurs

```pascal
procedure LectureSurisee(const NomFichier: string);
var
  Lignes: TStringList;
begin
  // Vérifier l'existence
  if not FileExists(NomFichier) then
  begin
    ShowMessage('Le fichier n''existe pas : ' + NomFichier);
    Exit;
  end;

  Lignes := TStringList.Create;
  try
    try
      Lignes.LoadFromFile(NomFichier);
      Memo1.Lines.Text := Lignes.Text;
      ShowMessage('Fichier chargé avec succès !');
    except
      on E: EInOutError do
        ShowMessage('Erreur d''entrée/sortie : ' + E.Message);
      on E: Exception do
        ShowMessage('Erreur inattendue : ' + E.Message);
    end;
  finally
    Lignes.Free;
  end;
end;
```

### Obtenir des informations sur un fichier

```pascal
uses
  System.IOUtils;

procedure InfosFichier(const NomFichier: string);
var
  Taille: Int64;
  DateModif: TDateTime;
begin
  if FileExists(NomFichier) then
  begin
    // Taille du fichier en octets
    Taille := TFile.GetSize(NomFichier);
    ShowMessage('Taille : ' + IntToStr(Taille) + ' octets');

    // Date de dernière modification
    DateModif := TFile.GetLastWriteTime(NomFichier);
    ShowMessage('Modifié le : ' + DateTimeToStr(DateModif));
  end;
end;
```

---

## Bonnes pratiques

### 1. Toujours libérer les ressources

Avec TStringList, utilisez toujours le pattern `Create` / `try` / `finally` / `Free` :

```pascal
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Votre code ici
  finally
    Liste.Free;
  end;
end;
```

### 2. Toujours fermer les fichiers TextFile

```pascal
var
  F: TextFile;
begin
  AssignFile(F, 'fichier.txt');
  Reset(F);
  try
    // Votre code ici
  finally
    CloseFile(F);
  end;
end;
```

### 3. Utiliser des chemins absolus ou relatifs corrects

```pascal
// Chemin absolu
const
  CHEMIN_ABSOLU = 'C:\MesDocuments\data.txt';

// Chemin relatif au dossier de l'application
var
  CheminRelatif: string;
begin
  CheminRelatif := ExtractFilePath(ParamStr(0)) + 'data.txt';
end;
```

### 4. Gérer l'encodage correctement

Pour les fichiers contenant des caractères accentués ou spéciaux, utilisez systématiquement UTF-8 :

```pascal
// Lecture
Liste.LoadFromFile(NomFichier, TEncoding.UTF8);

// Écriture
Liste.SaveToFile(NomFichier, TEncoding.UTF8);
```

### 5. Traiter les gros fichiers ligne par ligne

Pour les fichiers volumineux, évitez de charger tout le contenu en mémoire. Préférez l'approche TextFile :

```pascal
procedure TraiterGrosFichier(const NomFichier: string);
var
  F: TextFile;
  Ligne: string;
  Compteur: Integer;
begin
  AssignFile(F, NomFichier);
  Reset(F);
  try
    Compteur := 0;
    while not Eof(F) do
    begin
      ReadLn(F, Ligne);
      Inc(Compteur);

      // Traiter la ligne
      // ... votre code ...

      // Afficher la progression tous les 1000 lignes
      if (Compteur mod 1000) = 0 then
        Application.ProcessMessages; // Permet à l'interface de rester réactive
    end;

    ShowMessage(IntToStr(Compteur) + ' lignes traitées');
  finally
    CloseFile(F);
  end;
end;
```

---

## Exemples pratiques complets

### Exemple 1 : Créer un fichier de log

```pascal
procedure AjouterLog(const Message: string);
var
  Lignes: TStringList;
  NomFichierLog: string;
  EntreeLog: string;
begin
  // Créer le nom du fichier avec la date du jour
  NomFichierLog := ExtractFilePath(ParamStr(0)) +
                   'log_' + FormatDateTime('yyyy-mm-dd', Now) + '.txt';

  Lignes := TStringList.Create;
  try
    // Charger le fichier s'il existe
    if FileExists(NomFichierLog) then
      Lignes.LoadFromFile(NomFichierLog, TEncoding.UTF8);

    // Ajouter l'entrée avec horodatage
    EntreeLog := FormatDateTime('hh:nn:ss', Now) + ' - ' + Message;
    Lignes.Add(EntreeLog);

    // Sauvegarder
    Lignes.SaveToFile(NomFichierLog, TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  AjouterLog('Application démarrée');
  AjouterLog('Utilisateur a cliqué sur le bouton');
end;
```

### Exemple 2 : Lire un fichier de configuration simple

```pascal
function LireConfiguration(const Cle: string): string;
var
  Lignes: TStringList;
  i: Integer;
  Ligne: string;
  Position: Integer;
begin
  Result := ''; // Valeur par défaut

  Lignes := TStringList.Create;
  try
    Lignes.LoadFromFile('config.txt', TEncoding.UTF8);

    // Chercher la clé
    for i := 0 to Lignes.Count - 1 do
    begin
      Ligne := Lignes[i];
      Position := Pos('=', Ligne);

      if Position > 0 then
      begin
        // Comparer la clé (partie avant le =)
        if Trim(Copy(Ligne, 1, Position - 1)) = Cle then
        begin
          // Retourner la valeur (partie après le =)
          Result := Trim(Copy(Ligne, Position + 1, Length(Ligne)));
          Break;
        end;
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

// Utilisation
var
  NomUtilisateur: string;
begin
  NomUtilisateur := LireConfiguration('username');
  ShowMessage('Utilisateur : ' + NomUtilisateur);
end;
```

### Exemple 3 : Compter les occurrences d'un mot dans un fichier

```pascal
function CompterMot(const NomFichier, Mot: string): Integer;
var
  Lignes: TStringList;
  i, Position: Integer;
  Ligne, LigneMaj, MotMaj: string;
begin
  Result := 0;

  if not FileExists(NomFichier) then
    Exit;

  Lignes := TStringList.Create;
  try
    Lignes.LoadFromFile(NomFichier, TEncoding.UTF8);

    MotMaj := UpperCase(Mot);

    for i := 0 to Lignes.Count - 1 do
    begin
      Ligne := Lignes[i];
      LigneMaj := UpperCase(Ligne);

      // Compter les occurrences dans la ligne
      Position := 1;
      while Position > 0 do
      begin
        Position := Pos(MotMaj, LigneMaj, Position);
        if Position > 0 then
        begin
          Inc(Result);
          Inc(Position);
        end;
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

// Utilisation
var
  Occurrences: Integer;
begin
  Occurrences := CompterMot('C:\document.txt', 'Delphi');
  ShowMessage('Le mot "Delphi" apparaît ' + IntToStr(Occurrences) + ' fois');
end;
```

---

## Tableau comparatif des méthodes

| Critère | TStringList | TFile | TextFile |
|---------|-------------|-------|----------|
| **Simplicité** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **Contrôle** | ⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Gros fichiers** | ❌ | ❌ | ✅ |
| **Manipulation** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ |
| **Mémoire** | Élevée | Élevée | Faible |
| **Pour débuter** | ✅ Recommandé | ✅ Recommandé | ⚠️ Plus avancé |

---

## Résumé

Dans ce chapitre, vous avez appris trois méthodes pour manipuler des fichiers texte en Delphi :

1. **TStringList** : idéale pour débuter, très flexible, parfaite pour les fichiers de taille petite à moyenne
2. **TFile** : la plus concise, excellente pour des opérations simples et rapides
3. **TextFile** : la plus traditionnelle, nécessaire pour les gros fichiers et un contrôle précis

Pour un débutant, nous recommandons de commencer par **TStringList** car elle offre le meilleur équilibre entre simplicité et fonctionnalités. Une fois à l'aise avec cette méthode, vous pourrez explorer **TFile** pour sa concision et **TextFile** pour des besoins plus spécifiques.

N'oubliez pas les règles essentielles :
- Toujours libérer les ressources (Free, CloseFile)
- Gérer les erreurs avec try/except
- Vérifier l'existence des fichiers avec FileExists
- Utiliser UTF-8 pour les caractères accentués
- Privilégier la lecture ligne par ligne pour les gros fichiers

La manipulation de fichiers texte est une compétence que vous utiliserez constamment dans vos projets Delphi. Prenez le temps de bien comprendre ces concepts, ils vous seront utiles tout au long de votre parcours de développeur !

⏭️ [Manipulation de fichiers binaires](/07-gestion-des-fichiers-et-flux-de-donnees/02-manipulation-de-fichiers-binaires.md)
