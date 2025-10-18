🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.1 Gestion des chaînes de caractères

## Introduction

Les chaînes de caractères sont parmi les types de données les plus utilisés en programmation. Elles permettent de stocker et manipuler du texte, que ce soit pour afficher des messages à l'utilisateur, traiter des données, ou gérer l'internationalisation d'une application. Dans Delphi, la gestion des chaînes de caractères est puissante et flexible.

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

### String (le type principal)

Le type `string` est le type de chaîne par défaut et le plus couramment utilisé. Depuis les versions modernes de Delphi, `string` est un alias pour `UnicodeString`, ce qui signifie qu'il gère automatiquement les caractères Unicode.

```pascal
var
  Nom: string;
  Message: string;
begin
  Nom := 'Alice';
  Message := 'Bonjour ' + Nom;
end;
```

**Caractéristiques principales :**
- Gestion automatique de la mémoire (pas besoin de libérer manuellement)
- Support complet d'Unicode (tous les alphabets du monde)
- Longueur dynamique (s'adapte automatiquement)
- Indexation à partir de 1 (le premier caractère est à l'index 1)

### AnsiString

`AnsiString` est un type de chaîne qui utilise l'encodage ANSI (un caractère = un octet). Il est principalement utilisé pour la compatibilité avec du code ancien ou des APIs qui nécessitent cet encodage.

```pascal
var
  TexteAnsi: AnsiString;
begin
  TexteAnsi := 'Texte en ANSI';
end;
```

### WideString

`WideString` stocke les caractères en Unicode (UTF-16), similaire à `UnicodeString`, mais avec une gestion mémoire différente. Il est principalement utilisé pour l'interopérabilité COM.

### Char et les caractères individuels

Le type `Char` représente un seul caractère Unicode. Pour accéder à un caractère dans une chaîne, on utilise la notation entre crochets.

```pascal
var
  Texte: string;
  PremierCaractere: Char;
begin
  Texte := 'Bonjour';
  PremierCaractere := Texte[1]; // 'B'
end;
```

## Déclaration et initialisation

### Déclaration simple

```pascal
var
  Prenom: string;
  Ville: string;
```

### Initialisation lors de la déclaration

```pascal
var
  Prenom: string = 'Marie';
  Ville: string = 'Paris';
```

### Chaînes vides

Une chaîne peut être vide (ne contenir aucun caractère).

```pascal
var
  TexteVide: string;
begin
  TexteVide := ''; // Chaîne vide
  // ou
  TexteVide := EmptyStr; // Constante pour chaîne vide
end;
```

## Opérations de base sur les chaînes

### Concaténation (assemblage de chaînes)

La concaténation permet d'assembler plusieurs chaînes en une seule.

```pascal
var
  Prenom, Nom, NomComplet: string;
begin
  Prenom := 'Jean';
  Nom := 'Dupont';

  // Méthode 1 : opérateur +
  NomComplet := Prenom + ' ' + Nom; // 'Jean Dupont'

  // Méthode 2 : fonction Concat
  NomComplet := Concat(Prenom, ' ', Nom);
end;
```

### Longueur d'une chaîne

Pour connaître le nombre de caractères dans une chaîne, on utilise la fonction `Length`.

```pascal
var
  Texte: string;
  Longueur: Integer;
begin
  Texte := 'Bonjour';
  Longueur := Length(Texte); // 7
end;
```

### Accès aux caractères individuels

On accède aux caractères d'une chaîne par leur index (position), en commençant à 1.

```pascal
var
  Mot: string;
  Premier, Dernier: Char;
begin
  Mot := 'Delphi';
  Premier := Mot[1];              // 'D'
  Dernier := Mot[Length(Mot)];    // 'i'
end;
```

### Extraction de sous-chaînes

La fonction `Copy` permet d'extraire une partie d'une chaîne.

```pascal
// Copy(Chaîne, Position, Nombre)
var
  Texte, SousTexte: string;
begin
  Texte := 'Bonjour le monde';

  // Extraire 7 caractères à partir de la position 1
  SousTexte := Copy(Texte, 1, 7); // 'Bonjour'

  // Extraire à partir de la position 9
  SousTexte := Copy(Texte, 9, 2); // 'le'
end;
```

## Fonctions de manipulation courantes

### Conversion de casse (majuscules/minuscules)

```pascal
var
  Texte: string;
begin
  Texte := 'Bonjour';

  // Convertir en majuscules
  Texte := UpperCase(Texte); // 'BONJOUR'

  // Convertir en minuscules
  Texte := LowerCase('MONDE'); // 'monde'
end;
```

### Suppression des espaces

```pascal
var
  Texte: string;
begin
  Texte := '  Bonjour  ';

  // Supprimer les espaces à gauche et à droite
  Texte := Trim(Texte); // 'Bonjour'

  // Supprimer uniquement les espaces à gauche
  Texte := TrimLeft('  Bonjour'); // 'Bonjour'

  // Supprimer uniquement les espaces à droite
  Texte := TrimRight('Bonjour  '); // 'Bonjour'
end;
```

### Recherche dans une chaîne

La fonction `Pos` permet de trouver la position d'une sous-chaîne dans une chaîne.

```pascal
var
  Texte: string;
  Position: Integer;
begin
  Texte := 'Bonjour le monde';

  // Chercher 'monde'
  Position := Pos('monde', Texte); // 12

  // Si la sous-chaîne n'est pas trouvée, Pos retourne 0
  Position := Pos('salut', Texte); // 0
end;
```

### Remplacement de texte

La fonction `StringReplace` permet de remplacer des occurrences dans une chaîne.

```pascal
uses
  System.SysUtils;

var
  Texte, Resultat: string;
begin
  Texte := 'Bonjour le monde';

  // Remplacer 'monde' par 'univers'
  Resultat := StringReplace(Texte, 'monde', 'univers', [rfReplaceAll]);
  // 'Bonjour le univers'

  // Options disponibles :
  // rfReplaceAll : remplacer toutes les occurrences
  // rfIgnoreCase : ignorer la casse (majuscules/minuscules)
end;
```

## Comparaison de chaînes

### Comparaison simple

```pascal
var
  Chaine1, Chaine2: string;
begin
  Chaine1 := 'Bonjour';
  Chaine2 := 'Bonjour';

  if Chaine1 = Chaine2 then
    ShowMessage('Les chaînes sont identiques');

  if Chaine1 <> 'Au revoir' then
    ShowMessage('Les chaînes sont différentes');
end;
```

### Comparaison avec fonction

Pour des comparaisons plus sophistiquées, utilisez `CompareText` (insensible à la casse) ou `CompareStr` (sensible à la casse).

```pascal
var
  Resultat: Integer;
begin
  // CompareText : insensible à la casse
  Resultat := CompareText('Bonjour', 'bonjour'); // 0 (identiques)

  // CompareStr : sensible à la casse
  Resultat := CompareStr('Bonjour', 'bonjour'); // <> 0 (différentes)

  // La fonction retourne :
  // 0 si les chaînes sont égales
  // < 0 si la première chaîne est "plus petite"
  // > 0 si la première chaîne est "plus grande"
end;
```

### Vérifier le début ou la fin d'une chaîne

```pascal
var
  Texte: string;
begin
  Texte := 'Bonjour le monde';

  // Vérifier si commence par
  if Texte.StartsWith('Bonjour') then
    ShowMessage('Commence par Bonjour');

  // Vérifier si se termine par
  if Texte.EndsWith('monde') then
    ShowMessage('Se termine par monde');
end;
```

## Formatage de chaînes

### Format simple avec Format()

La fonction `Format` permet de créer des chaînes formatées de manière élégante.

```pascal
var
  Nom: string;
  Age: Integer;
  Message: string;
begin
  Nom := 'Alice';
  Age := 25;

  // %s pour les chaînes, %d pour les entiers
  Message := Format('Bonjour %s, vous avez %d ans', [Nom, Age]);
  // 'Bonjour Alice, vous avez 25 ans'
end;
```

### Spécificateurs de format courants

```pascal
var
  Entier: Integer;
  Reel: Double;
  Texte: string;
begin
  Entier := 42;
  Reel := 3.14159;

  // %d : entier décimal
  Texte := Format('Entier : %d', [Entier]); // 'Entier : 42'

  // %f : nombre à virgule flottante
  Texte := Format('Pi : %f', [Reel]); // 'Pi : 3.14159000000000'

  // %.2f : deux décimales
  Texte := Format('Pi : %.2f', [Reel]); // 'Pi : 3.14'

  // %s : chaîne de caractères
  Texte := Format('Texte : %s', ['Hello']); // 'Texte : Hello'

  // %x : hexadécimal
  Texte := Format('Hexa : %x', [255]); // 'Hexa : FF'
end;
```

## Conversion entre types

### Chaîne vers nombre

```pascal
var
  TexteNombre: string;
  Nombre: Integer;
  NombreReel: Double;
begin
  TexteNombre := '42';

  // Conversion en entier
  Nombre := StrToInt(TexteNombre); // 42

  // Conversion en entier avec valeur par défaut en cas d'erreur
  Nombre := StrToIntDef('abc', 0); // 0 (car 'abc' n'est pas un nombre)

  // Conversion en réel
  NombreReel := StrToFloat('3.14'); // 3.14
end;
```

### Nombre vers chaîne

```pascal
var
  Nombre: Integer;
  NombreReel: Double;
  Texte: string;
begin
  Nombre := 42;
  NombreReel := 3.14159;

  // Entier vers chaîne
  Texte := IntToStr(Nombre); // '42'

  // Réel vers chaîne
  Texte := FloatToStr(NombreReel); // '3.14159'

  // Avec format spécifique
  Texte := FloatToStrF(NombreReel, ffFixed, 15, 2); // '3.14'
end;
```

## Parcours d'une chaîne

### Parcours caractère par caractère

```pascal
var
  Texte: string;
  Caractere: Char;
  i: Integer;
begin
  Texte := 'Bonjour';

  // Méthode 1 : boucle for classique
  for i := 1 to Length(Texte) do
  begin
    Caractere := Texte[i];
    // Traiter le caractère
  end;

  // Méthode 2 : boucle for-in (plus moderne)
  for Caractere in Texte do
  begin
    // Traiter le caractère
  end;
end;
```

## Division de chaînes (Split)

Pour diviser une chaîne en plusieurs parties selon un séparateur.

```pascal
uses
  System.StrUtils;

var
  Texte: string;
  Parties: TArray<string>;
  Partie: string;
begin
  Texte := 'Pomme,Poire,Banane';

  // Diviser selon la virgule
  Parties := SplitString(Texte, ',');

  // Parcourir les parties
  for Partie in Parties do
    ShowMessage(Partie); // Affiche 'Pomme', puis 'Poire', puis 'Banane'
end;
```

## Construction efficace de chaînes

Lorsqu'on doit construire une longue chaîne par concaténations successives, il est préférable d'utiliser `TStringBuilder` pour de meilleures performances.

```pascal
uses
  System.SysUtils;

var
  Builder: TStringBuilder;
  Resultat: string;
  i: Integer;
begin
  Builder := TStringBuilder.Create;
  try
    // Ajouter des morceaux de texte
    Builder.Append('Bonjour ');
    Builder.Append('le ');
    Builder.Append('monde');

    // Ajouter avec retour à la ligne
    Builder.AppendLine('Ligne 1');
    Builder.AppendLine('Ligne 2');

    // Ajouter avec format
    Builder.AppendFormat('Nombre : %d', [42]);

    // Récupérer la chaîne finale
    Resultat := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

## Bonnes pratiques

### Éviter les concaténations multiples en boucle

**Mauvaise pratique :**
```pascal
var
  Resultat: string;
  i: Integer;
begin
  Resultat := '';
  for i := 1 to 1000 do
    Resultat := Resultat + IntToStr(i) + '; '; // Très lent !
end;
```

**Bonne pratique :**
```pascal
var
  Builder: TStringBuilder;
  Resultat: string;
  i: Integer;
begin
  Builder := TStringBuilder.Create;
  try
    for i := 1 to 1000 do
      Builder.Append(IntToStr(i)).Append('; ');
    Resultat := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

### Vérifier si une chaîne est vide

```pascal
var
  Texte: string;
begin
  // Méthode standard
  if Texte = '' then
    ShowMessage('Chaîne vide');

  // Méthode alternative
  if Length(Texte) = 0 then
    ShowMessage('Chaîne vide');

  // Méthode moderne (Delphi récent)
  if Texte.IsEmpty then
    ShowMessage('Chaîne vide');
end;
```

### Gérer les erreurs de conversion

```pascal
var
  TexteSaisi: string;
  Nombre: Integer;
begin
  TexteSaisi := '123abc'; // Texte invalide

  // Avec gestion d'erreur
  if TryStrToInt(TexteSaisi, Nombre) then
    ShowMessage('Conversion réussie : ' + IntToStr(Nombre))
  else
    ShowMessage('Erreur : texte invalide');

  // Ou avec valeur par défaut
  Nombre := StrToIntDef(TexteSaisi, -1);
  if Nombre = -1 then
    ShowMessage('Erreur de conversion');
end;
```

## Encodage et Unicode

Delphi gère nativement Unicode depuis plusieurs versions. Le type `string` peut donc contenir n'importe quel caractère de n'importe quel alphabet.

```pascal
var
  TexteMultilingue: string;
begin
  // Tous ces caractères sont gérés correctement
  TexteMultilingue := 'Bonjour 你好 مرحبا Привет 🎉';

  ShowMessage(TexteMultilingue); // Affiche correctement tous les caractères
end;
```

### Points d'attention avec Unicode

Certains caractères Unicode peuvent être composés de plusieurs "code points". Par exemple, un emoji avec une variation de couleur de peau est techniquement composé de plusieurs caractères Unicode.

```pascal
var
  Emoji: string;
  Longueur: Integer;
begin
  Emoji := '👋'; // Un emoji simple
  Longueur := Length(Emoji); // Peut retourner 1 ou 2 selon l'emoji

  // Pour un comptage précis des "caractères visuels",
  // utilisez des fonctions spécialisées
end;
```

## Résumé des fonctions essentielles

| Fonction | Description | Exemple |
|----------|-------------|---------|
| `Length(s)` | Longueur de la chaîne | `Length('Hello')` → 5 |
| `Copy(s, pos, len)` | Extraire une sous-chaîne | `Copy('Bonjour', 1, 3)` → 'Bon' |
| `Pos(sub, s)` | Position d'une sous-chaîne | `Pos('jour', 'Bonjour')` → 4 |
| `UpperCase(s)` | Convertir en majuscules | `UpperCase('hello')` → 'HELLO' |
| `LowerCase(s)` | Convertir en minuscules | `LowerCase('HELLO')` → 'hello' |
| `Trim(s)` | Supprimer espaces début/fin | `Trim('  Hi  ')` → 'Hi' |
| `IntToStr(n)` | Entier vers chaîne | `IntToStr(42)` → '42' |
| `StrToInt(s)` | Chaîne vers entier | `StrToInt('42')` → 42 |
| `Format(fmt, params)` | Formatage de chaîne | `Format('%s: %d', ['Age', 25])` |
| `StringReplace(s, old, new, flags)` | Remplacement | `StringReplace('Hi', 'i', 'ello', [])` |

## Conclusion

La gestion des chaînes de caractères en Delphi est riche et intuitive. Le type `string` moderne gère automatiquement la mémoire et supporte pleinement Unicode, ce qui facilite grandement le développement d'applications internationales. Les nombreuses fonctions disponibles permettent de manipuler le texte de manière efficace et élégante.

Dans les prochaines sections, nous explorerons comment utiliser ces connaissances pour gérer l'internationalisation complète d'une application, avec les ressources linguistiques et l'adaptation aux différentes cultures.

⏭️ [Ressources linguistiques](/13-internationalisation-et-localisation/02-ressources-linguistiques.md)
