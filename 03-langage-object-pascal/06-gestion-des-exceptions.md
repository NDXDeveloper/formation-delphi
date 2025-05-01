# 3.6 Gestion des exceptions

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les exceptions sont un mécanisme qui permet de gérer les erreurs et les situations anormales dans votre programme. Au lieu de vérifier constamment si chaque opération a réussi, le système d'exceptions vous permet de séparer le code normal du code de gestion d'erreurs, rendant vos programmes plus clairs et plus robustes.

## Qu'est-ce qu'une exception ?

Une exception est un événement qui se produit pendant l'exécution d'un programme et qui interrompt le flux normal d'instructions. Quand une erreur survient, le système "lance" (ou "lève") une exception, qui peut être "attrapée" et traitée ailleurs dans le programme.

Quelques exemples de situations pouvant générer des exceptions :
- Division par zéro
- Accès à un index hors des limites d'un tableau
- Conversion de types invalide
- Opération sur un fichier inexistant
- Manque de mémoire

## Structure de base : try-except

La structure de base pour gérer les exceptions en Object Pascal est le bloc `try-except` :

```pascal
try
  // Code qui pourrait générer une exception
except
  // Code de gestion des exceptions
end;
```

Exemple simple :

```pascal
var
  A, B, Resultat: Integer;
begin
  A := 10;
  B := 0;

  try
    Resultat := A div B;  // Ceci va générer une exception (division par zéro)
    ShowMessage('Résultat : ' + IntToStr(Resultat));  // Cette ligne ne sera jamais exécutée
  except
    ShowMessage('Erreur : Division par zéro !');
  end;

  ShowMessage('Le programme continue...');  // Cette ligne sera exécutée
end;
```

Dans cet exemple, l'opération `A div B` génère une exception de division par zéro. Le code dans le bloc `except` est alors exécuté, puis le programme continue après le bloc `try-except`.

## Capturer des types d'exceptions spécifiques

Vous pouvez capturer et traiter différents types d'exceptions de manière spécifique :

```pascal
try
  // Code qui pourrait générer différentes exceptions
except
  on E: EDivByZero do
    ShowMessage('Erreur : Division par zéro !');
  on E: EConvertError do
    ShowMessage('Erreur : Conversion invalide !');
  on E: Exception do
    ShowMessage('Autre erreur : ' + E.Message);
end;
```

Dans cet exemple :
- Si une exception `EDivByZero` se produit, le premier gestionnaire est exécuté
- Si une exception `EConvertError` se produit, le deuxième gestionnaire est exécuté
- Pour toute autre exception, le dernier gestionnaire est exécuté (car `Exception` est la classe parent de toutes les exceptions)

## Accéder aux informations de l'exception

La variable `E` dans l'exemple précédent contient l'objet exception qui a été levé. Vous pouvez accéder à ses propriétés pour obtenir plus d'informations :

```pascal
try
  StrToInt('abc');  // Ceci va générer une exception EConvertError
except
  on E: Exception do
  begin
    ShowMessage('Type d''erreur : ' + E.ClassName);
    ShowMessage('Message d''erreur : ' + E.Message);
  end;
end;
```

## Structure try-finally

La structure `try-finally` garantit qu'un bloc de code sera toujours exécuté, qu'une exception se produise ou non :

```pascal
try
  // Code qui pourrait générer une exception
finally
  // Code qui sera toujours exécuté
end;
```

Cette structure est particulièrement utile pour le nettoyage de ressources (fermeture de fichiers, libération de mémoire, etc.) :

```pascal
var
  F: TextFile;
begin
  AssignFile(F, 'donnees.txt');
  Reset(F);  // Ouvre le fichier

  try
    // Opérations sur le fichier
    // (si une exception se produit ici, le fichier sera quand même fermé)
  finally
    CloseFile(F);  // Ferme le fichier dans tous les cas
  end;
end;
```

## Structure combinée try-except-finally

Vous pouvez combiner les deux structures précédentes :

```pascal
try
  // Code qui pourrait générer une exception
except
  // Gestion des exceptions
end;

// La partie finally est placée après le bloc except
finally
  // Code qui sera toujours exécuté
end;
```

Exemple complet avec des opérations sur un fichier :

```pascal
var
  F: TextFile;
  Ligne: string;
begin
  AssignFile(F, 'donnees.txt');

  try
    try
      Reset(F);  // Tente d'ouvrir le fichier

      while not Eof(F) do
      begin
        ReadLn(F, Ligne);
        // Traitement de la ligne...
      end;
    except
      on E: EInOutError do
        ShowMessage('Erreur de fichier : ' + E.Message);
      on E: Exception do
        ShowMessage('Autre erreur : ' + E.Message);
    end;
  finally
    if TTextRec(F).Mode <> fmClosed then  // Vérifie si le fichier est ouvert
      CloseFile(F);
  end;
end;
```

## Lever ses propres exceptions

Vous pouvez aussi lever vos propres exceptions avec le mot-clé `raise` :

```pascal
procedure VerifierAge(Age: Integer);
begin
  if Age < 0 then
    raise Exception.Create('L''âge ne peut pas être négatif');

  if Age > 120 then
    raise Exception.Create('L''âge semble trop élevé');

  // Traitement normal si l'âge est valide
end;
```

Utilisez cette procédure dans un bloc `try-except` :

```pascal
try
  VerifierAge(-5);
except
  on E: Exception do
    ShowMessage('Erreur : ' + E.Message);
end;
```

## Créer ses propres types d'exceptions

Pour des applications plus complexes, vous pouvez créer vos propres types d'exceptions en dérivant de la classe `Exception` :

```pascal
type
  EAgeInvalide = class(Exception);
  EConnexionBD = class(Exception);

procedure VerifierAge(Age: Integer);
begin
  if Age < 0 then
    raise EAgeInvalide.Create('L''âge ne peut pas être négatif');

  if Age > 120 then
    raise EAgeInvalide.Create('L''âge semble trop élevé');
end;
```

Cela permet une gestion plus précise des différents types d'erreurs :

```pascal
try
  VerifierAge(-5);
except
  on E: EAgeInvalide do
    ShowMessage('Âge invalide : ' + E.Message);
  on E: EConnexionBD do
    ShowMessage('Erreur de connexion à la base de données : ' + E.Message);
  on E: Exception do
    ShowMessage('Autre erreur : ' + E.Message);
end;
```

## Relancer une exception

Parfois, vous voulez traiter une exception mais la relancer pour qu'elle soit gérée à un niveau supérieur :

```pascal
try
  // Code qui pourrait générer une exception
except
  on E: Exception do
  begin
    // Journalisation de l'erreur
    LogErreur(E.Message);

    // Relance l'exception pour qu'elle soit gérée ailleurs
    raise;  // Sans paramètre, cela relance l'exception courante
  end;
end;
```

## Types d'exceptions courants

Voici quelques types d'exceptions prédéfinis que vous rencontrerez souvent :

- `Exception` : Classe de base pour toutes les exceptions
- `EAbort` : Exception spéciale qui indique une interruption sans erreur
- `EInOutError` : Erreurs d'entrée/sortie (fichiers)
- `EConvertError` : Erreurs de conversion de types
- `EDivByZero` : Division par zéro
- `ERangeError` : Index hors limites
- `EAccessViolation` : Accès mémoire illégal
- `EZeroDivide` : Division par zéro (nombres à virgule)
- `EOverflow` : Dépassement arithmétique

## Bonnes pratiques

### 1. Soyez spécifique

Capturez les exceptions les plus spécifiques possibles plutôt que toutes les exceptions :

```pascal
// Approche préférable
try
  // Code
except
  on E: EDivByZero do
    // Traitement spécifique
  on E: EConvertError do
    // Traitement spécifique
end;

// Évitez ceci sauf en dernier recours
try
  // Code
except
  on E: Exception do
    // Traitement générique
end;
```

### 2. N'avalez pas les exceptions silencieusement

Évitez de capturer des exceptions sans les traiter correctement :

```pascal
// À ÉVITER
try
  // Code risqué
except
  // Vide - l'exception est "avalée" sans traitement
end;
```

### 3. Utilisez toujours finally pour la libération des ressources

```pascal
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Opérations sur la liste
  finally
    Liste.Free;  // La liste sera toujours libérée
  end;
end;
```

### 4. Gardez les blocs try courts et précis

Limitez la portée des blocs `try` au code qui pourrait réellement générer une exception :

```pascal
// À ÉVITER
try
  // Beaucoup de code ici...
  // Difficile de savoir quelle partie peut générer une exception
except
  // Gestion des erreurs
end;

// PRÉFÉRABLE
// Code normal qui ne génère pas d'exceptions

try
  // Seulement le code qui pourrait générer une exception
except
  // Gestion des erreurs
end;

// Suite du code normal
```

### 5. Utilisez le modèle de protection

Pour les objets qui doivent être libérés, utilisez ce modèle :

```pascal
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    // Utilisez Obj ici
  finally
    Obj.Free;  // Libération garantie
  end;
end;
```

Ce modèle s'applique aussi aux transactions de base de données, aux fichiers, et à d'autres ressources.

## Exemple : application concrète

Voici un exemple plus complet montrant comment gérer les exceptions dans un contexte réel :

```pascal
procedure TraiterFichierClient(const NomFichier: string);
var
  Fichier: TStringList;
  i: Integer;
  Ligne, Nom, Email: string;
  Position: Integer;
begin
  Fichier := TStringList.Create;
  try
    try
      Fichier.LoadFromFile(NomFichier);

      for i := 0 to Fichier.Count - 1 do
      begin
        Ligne := Fichier[i];

        try
          Position := Pos(';', Ligne);
          if Position <= 0 then
            raise Exception.Create('Format de ligne invalide');

          Nom := Copy(Ligne, 1, Position - 1);
          Email := Copy(Ligne, Position + 1, Length(Ligne));

          if Trim(Nom) = '' then
            raise Exception.Create('Nom manquant');

          if Pos('@', Email) <= 0 then
            raise Exception.Create('Email invalide');

          // Traitement du client valide
          EnregistrerClient(Nom, Email);

        except
          on E: Exception do
          begin
            // Journalise l'erreur mais continue avec la ligne suivante
            LogErreur('Erreur à la ligne ' + IntToStr(i+1) + ': ' + E.Message);
          end;
        end;
      end;

      ShowMessage('Traitement terminé');
    except
      on E: EFOpenError do
        ShowMessage('Impossible d''ouvrir le fichier : ' + E.Message);
      on E: Exception do
        ShowMessage('Erreur lors du traitement : ' + E.Message);
    end;
  finally
    Fichier.Free;
  end;
end;
```

Dans cet exemple, nous utilisons des blocs `try-except` imbriqués pour :
1. Gérer les erreurs de fichier au niveau externe
2. Gérer les erreurs de format pour chaque ligne à l'intérieur de la boucle
3. Garantir que la liste `Fichier` est toujours libérée avec `finally`

## Exceptions et performances

Les exceptions sont conçues pour gérer les situations exceptionnelles, pas pour le contrôle de flux normal. Utiliser les exceptions de manière excessive peut nuire aux performances :

```pascal
// À ÉVITER : Utiliser les exceptions pour le contrôle de flux normal
try
  Valeur := TDictionnaire[Cle];
except
  on E: Exception do
    Valeur := ValeurParDefaut;
end;

// PRÉFÉRABLE : Vérification explicite
if TDictionnaire.ContainsKey(Cle) then
  Valeur := TDictionnaire[Cle]
else
  Valeur := ValeurParDefaut;
```

---

La gestion des exceptions est un élément crucial dans le développement d'applications robustes. En utilisant correctement les structures `try-except-finally`, vous pouvez créer des programmes qui réagissent gracieusement aux erreurs, protègent les ressources et fournissent des informations utiles pour le débogage.

Dans la prochaine section, nous aborderons la programmation orientée objet, un paradigme fondamental en Delphi qui vous permettra de structurer votre code de manière encore plus efficace.

⏭️ [Programmation orientée objet](/03-langage-object-pascal/07-programmation-orientee-objets.md)
