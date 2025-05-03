# 13.1 Gestion des chaînes de caractères

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'internationalisation d'une application commence par une bonne gestion des chaînes de caractères. Delphi offre des outils puissants pour manipuler le texte dans différentes langues et encodages.

## Les types de chaînes en Delphi

Delphi propose plusieurs types pour représenter les chaînes de caractères :

| Type | Description | Utilisation |
|------|-------------|-------------|
| `string` | Type de chaîne par défaut (Unicode) | Usage général |
| `AnsiString` | Chaîne ANSI (encodage selon la page de code) | Compatibilité avec du code ancien |
| `UnicodeString` | Chaîne Unicode (identique à `string` dans les versions récentes) | Support multi-langues |
| `WideString` | Chaîne Unicode compatible COM | Communication avec des composants COM |
| `UTF8String` | Chaîne au format UTF-8 | Échanges web, fichiers texte |

> 💡 Dans les versions modernes de Delphi, `string` est équivalent à `UnicodeString`, ce qui permet nativement la gestion de tous les caractères internationaux.

## Manipulation de base des chaînes

### Déclaration et affectation

```pascal
var
  MaChaine: string;
begin
  MaChaine := 'Bonjour le monde';
  ShowMessage(MaChaine);
end;
```

### Concaténation de chaînes

```pascal
var
  Prenom, NomComplet: string;
begin
  Prenom := 'Marie';
  NomComplet := 'Bonjour ' + Prenom + ' !';
  // Ou avec l'opérateur +=
  NomComplet := 'Bonjour ';
  NomComplet += Prenom;
  NomComplet += ' !';
end;
```

### Longueur d'une chaîne

```pascal
var
  MaChaine: string;
  Longueur: Integer;
begin
  MaChaine := 'Bonjour';
  Longueur := Length(MaChaine); // Retourne 7
end;
```

## Fonctions utiles pour les chaînes

Delphi offre de nombreuses fonctions pour manipuler les chaînes :

### Recherche dans une chaîne

```pascal
var
  Position: Integer;
begin
  // Recherche la position du mot "monde" (commence à 1, pas à 0)
  Position := Pos('monde', 'Bonjour le monde');
  // Position vaut 12
end;
```

### Extraction de sous-chaînes

```pascal
var
  MaChaine, SousChaine: string;
begin
  MaChaine := 'Bonjour le monde';

  // Extraction avec Copy (chaîne, position de départ, nombre de caractères)
  SousChaine := Copy(MaChaine, 9, 2); // Résultat : "le"

  // Extraction du début (gauche)
  SousChaine := LeftStr(MaChaine, 7); // Résultat : "Bonjour"

  // Extraction de la fin (droite)
  SousChaine := RightStr(MaChaine, 5); // Résultat : "monde"

  // Extraction du milieu
  SousChaine := MidStr(MaChaine, 9, 2); // Résultat : "le"
end;
```

> ⚠️ Les fonctions `LeftStr`, `RightStr` et `MidStr` nécessitent d'ajouter `StrUtils` dans la clause `uses`.

### Modification de chaînes

```pascal
var
  MaChaine: string;
begin
  MaChaine := 'Bonjour le monde';

  // Remplacement
  MaChaine := StringReplace(MaChaine, 'monde', 'Delphi', [rfReplaceAll]);
  // Résultat : "Bonjour le Delphi"

  // Conversion en majuscules
  MaChaine := UpperCase(MaChaine);
  // Résultat : "BONJOUR LE DELPHI"

  // Conversion en minuscules
  MaChaine := LowerCase(MaChaine);
  // Résultat : "bonjour le delphi"

  // Première lettre en majuscule
  MaChaine := UpperCase(MaChaine[1]) + Copy(MaChaine, 2, Length(MaChaine) - 1);
  // Résultat : "Bonjour le delphi"
end;
```

## Chaînes formatées

La fonction `Format` permet de créer des chaînes complexes en insérant des valeurs à des emplacements spécifiques :

```pascal
var
  Nom, Prenom, Message: string;
  Age: Integer;
begin
  Nom := 'Dupont';
  Prenom := 'Jean';
  Age := 35;

  // %s pour les chaînes, %d pour les entiers
  Message := Format('Bonjour %s %s, vous avez %d ans.', [Prenom, Nom, Age]);
  // Résultat : "Bonjour Jean Dupont, vous avez 35 ans."
end;
```

## Conversion entre types de chaînes

Il est parfois nécessaire de convertir entre différents formats de chaînes :

```pascal
var
  S: string;
  U: UTF8String;
  A: AnsiString;
begin
  S := 'Caractères spéciaux: é à ç ê';

  // Conversion en UTF-8
  U := UTF8Encode(S);

  // Conversion d'UTF-8 vers string (UnicodeString)
  S := UTF8ToString(U);

  // Conversion en AnsiString (attention à la perte possible de caractères)
  A := AnsiString(S);

  // Reconversion en string
  S := string(A);
end;
```

## Manipuler des chaînes avec TStringHelper

À partir de Delphi XE3, le type `string` dispose d'un helper qui ajoute des méthodes directement à la chaîne :

```pascal
var
  S: string;
  Contient: Boolean;
  Position: Integer;
begin
  S := 'Bonjour le monde';

  // Test si la chaîne contient "monde"
  Contient := S.Contains('monde');

  // Recherche de "monde" (commence à 0, pas à 1 comme Pos())
  Position := S.IndexOf('monde');

  // Débute par
  if S.StartsWith('Bon') then
    ShowMessage('La chaîne commence par "Bon"');

  // Termine par
  if S.EndsWith('de') then
    ShowMessage('La chaîne finit par "de"');

  // Supprimer des espaces en début et fin
  S := ' Texte avec espaces  '.Trim;  // "Texte avec espaces"
  S := ' Texte avec espaces  '.TrimLeft;  // "Texte avec espaces  "
  S := ' Texte avec espaces  '.TrimRight;  // " Texte avec espaces"

  // Autres méthodes utiles
  S := 'bonjour'.ToUpper;  // "BONJOUR"
  S := 'BONJOUR'.ToLower;  // "bonjour"
  S := 'Bonjour le monde'.Substring(8, 2);  // "le"
end;
```

> 💡 L'utilisation de `TStringHelper` rend le code plus lisible et orienté objet.

## Gestion des caractères spéciaux

### Caractères d'échappement

Pour inclure des caractères spéciaux dans une chaîne :

```pascal
var
  S: string;
begin
  // Utilisation du caractère #13#10 pour un retour à la ligne
  S := 'Première ligne' + #13#10 + 'Deuxième ligne';

  // Utilisation de guillemets dans une chaîne
  S := 'Il a dit: "Bonjour"';

  // Doubler les apostrophes pour les inclure
  S := 'L''apostrophe est doublée';
end;
```

### Chaînes multilignes

Pour des chaînes multilignes, on peut utiliser la syntaxe de l'apostrophe répétée :

```pascal
var
  S: string;
begin
  S := 'Ceci est une chaîne'
     + ' qui s''étend sur'
     + ' plusieurs lignes de code';

  // Ou avec des constantes
  S := 'Première ligne' + sLineBreak + 'Deuxième ligne';
end;
```

## Conseils pour l'internationalisation

1. **Évitez les chaînes codées en dur** dans votre code. Utilisez plutôt des ressources de chaînes ou des fichiers de ressources.

2. **Utilisez toujours le type `string`** (UnicodeString) pour assurer la compatibilité avec tous les caractères internationaux.

3. **Attention aux opérations de comparaison** qui peuvent être sensibles à la casse et aux accents selon les paramètres régionaux.

4. **Pour les comparaisons insensibles à la casse**, utilisez `SameText` ou `CompareText` plutôt que de convertir les chaînes en majuscules :

```pascal
if SameText(Chaine1, Chaine2) then
  ShowMessage('Les chaînes sont identiques (insensible à la casse)');

if CompareText(Chaine1, Chaine2) = 0 then
  ShowMessage('Les chaînes sont identiques (insensible à la casse)');
```

## Exemple complet : Manipulation de texte

Voici un exemple qui illustre plusieurs techniques de manipulation de chaînes :

```pascal
procedure TForm1.btnTraiterClick(Sender: TObject);
var
  Texte, Mot, Resultat: string;
  Position, Compteur: Integer;
begin
  Texte := edtTexte.Text;
  Mot := edtMot.Text;
  Compteur := 0;
  Resultat := '';

  // Conversion en minuscules pour recherche insensible à la casse
  Texte := LowerCase(Texte);
  Mot := LowerCase(Mot);

  // Recherche de toutes les occurrences du mot
  Position := Pos(Mot, Texte);
  while Position > 0 do
  begin
    Inc(Compteur);
    // Continue la recherche après l'occurrence trouvée
    Position := Pos(Mot, Texte, Position + Length(Mot));
  end;

  Resultat := Format('Le mot "%s" apparaît %d fois dans le texte.', [edtMot.Text, Compteur]);

  // Affichage du résultat
  memResultat.Lines.Add(Resultat);
end;
```

> ⚠️ La fonction `Pos` avec trois paramètres (pour spécifier la position de départ) nécessite Delphi 10.4 ou supérieur. Pour les versions antérieures, utilisez `PosEx` de l'unité `StrUtils`.

---

En maîtrisant ces techniques de gestion des chaînes de caractères, vous serez en mesure de créer des applications Delphi qui gèrent correctement les textes de différentes langues, ce qui est essentiel pour l'internationalisation de vos logiciels.

⏭️ [Ressources linguistiques](13-internationalisation-et-localisation/02-ressources-linguistiques.md)
