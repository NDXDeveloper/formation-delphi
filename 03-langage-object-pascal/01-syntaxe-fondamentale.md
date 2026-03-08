🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.1 Syntaxe fondamentale du langage Object Pascal

## Introduction

Object Pascal est le langage de programmation utilisé par Delphi. Sa syntaxe est claire, lisible et structurée, ce qui en fait un excellent langage pour débuter en programmation. Dans cette section, nous allons découvrir les éléments de base qui constituent tout programme Delphi.

## Structure générale d'un programme

Un programme Delphi est organisé en unités (units) qui contiennent du code. Voici la structure de base d'une unité :

```pascal
unit MonUnite;

interface

// Déclarations publiques (visibles par d'autres unités)

implementation

// Code d'implémentation

end.
```

Chaque unité se termine obligatoirement par le mot-clé `end` suivi d'un point (`.`).

## Les commentaires

Les commentaires permettent de documenter votre code. Ils sont ignorés lors de la compilation et servent uniquement à améliorer la lisibilité pour les développeurs.

Il existe trois façons d'écrire des commentaires en Object Pascal :

```pascal
// Commentaire sur une seule ligne

{ Commentaire sur une ou
  plusieurs lignes }

(* Autre forme de commentaire
   sur plusieurs lignes *)
```

**Recommandation :** Privilégiez `//` pour les commentaires courts et `{ }` pour les commentaires longs ou la désactivation temporaire de code.

## Les instructions et le point-virgule

En Object Pascal, chaque instruction se termine par un point-virgule (`;`) qui sert de séparateur d'instructions.

```pascal
var
  Age: Integer;
  Nom: string;
begin
  Age := 25;
  Nom := 'Marie';
  ShowMessage('Bonjour ' + Nom);
end;
```

**Point important :** Le point-virgule n'est pas obligatoire avant certains mots-clés comme `end`, `else`, `until`, ou `except`, car ces mots-clés marquent déjà une séparation logique.

## La casse : sensibilité aux majuscules/minuscules

Object Pascal n'est **pas sensible à la casse** pour les identifiants. Cela signifie que :

```pascal
MaVariable  
mavariable  
MAVARIABLE  
```

Ces trois écritures font référence à la même variable. Cependant, par convention et pour la lisibilité, on utilise généralement :
- **PascalCase** (majuscule à chaque début de mot) pour les noms de types, classes, procédures et fonctions : `MonBouton`, `CalculerTotal`
- **camelCase** (première lettre minuscule) pour les variables locales : `monCompteur`, `totalVentes`

## Les blocs begin/end

Les mots-clés `begin` et `end` délimitent un bloc d'instructions. Ils fonctionnent comme des accolades `{ }` dans d'autres langages.

```pascal
begin
  // Première instruction
  // Deuxième instruction
  // Troisième instruction
end;
```

Un bloc peut contenir une ou plusieurs instructions. Ils sont particulièrement utiles avec les structures de contrôle :

```pascal
if Condition then  
begin  
  // Plusieurs instructions si la condition est vraie
  Instruction1;
  Instruction2;
  Instruction3;
end;
```

**Note :** Si vous n'avez qu'une seule instruction après un `if`, le `begin/end` n'est pas obligatoire, mais il est recommandé pour la clarté.

## L'indentation et la lisibilité

Bien que l'indentation ne soit pas obligatoire en Object Pascal (contrairement à Python), elle est essentielle pour la lisibilité du code.

**Mauvaise pratique :**
```pascal
procedure Test;  
begin  
if X > 0 then  
begin  
Y := X * 2;  
ShowMessage('Résultat');  
end;  
end;  
```

**Bonne pratique :**
```pascal
procedure Test;  
begin  
  if X > 0 then
  begin
    Y := X * 2;
    ShowMessage('Résultat');
  end;
end;
```

L'IDE Delphi offre des outils de formatage automatique du code (Ctrl+D) qui appliquent les conventions d'indentation.

## Les mots-clés réservés

Object Pascal possède des mots-clés réservés qui ne peuvent pas être utilisés comme noms de variables ou d'identifiants. Voici les principaux :

- **Structure :** `program`, `unit`, `interface`, `implementation`, `uses`, `begin`, `end`
- **Déclarations :** `var`, `const`, `type`, `procedure`, `function`, `class`, `record`
- **Contrôle :** `if`, `then`, `else`, `case`, `of`, `for`, `to`, `downto`, `while`, `do`, `repeat`, `until`
- **Autres :** `and`, `or`, `not`, `nil`, `true`, `false`, `try`, `except`, `finally`, `raise`

## Les espaces blancs

Les espaces, tabulations et sauts de ligne sont généralement ignorés par le compilateur (sauf dans les chaînes de caractères). Vous pouvez les utiliser librement pour améliorer la lisibilité :

```pascal
X:=Y+Z;        // Valide mais peu lisible  
X := Y + Z;    // Préférable  
```

## Exemple complet : Structure d'une procédure simple

Voici un exemple qui rassemble plusieurs éléments de syntaxe :

```pascal
procedure AfficherMessage;  
var  
  Message: string;  // Déclaration d'une variable locale
begin
  // Initialisation de la variable
  Message := 'Bienvenue dans Delphi !';

  // Affichage du message
  ShowMessage(Message);
end;
```

**Décortiquons ce code :**
1. `procedure AfficherMessage;` - Déclaration de la procédure
2. `var Message: string;` - Déclaration d'une variable locale de type chaîne
3. `begin` - Début du bloc d'instructions
4. `Message := 'Bienvenue dans Delphi !';` - Affectation d'une valeur
5. `ShowMessage(Message);` - Appel d'une procédure système
6. `end;` - Fin du bloc

## Les séparateurs et délimiteurs

Object Pascal utilise plusieurs symboles comme séparateurs et délimiteurs :

| Symbole | Usage |
|---------|-------|
| `;` | Séparateur d'instructions |
| `,` | Séparateur de paramètres ou d'éléments de liste |
| `.` | Accès aux membres d'un objet ou d'un enregistrement |
| `:` | Séparateur entre identifiant et type |
| `()` | Délimiteur de paramètres ou expression |
| `[]` | Délimiteur pour les tableaux et ensembles |
| `''` | Délimiteur pour les chaînes de caractères |

## Règles de nommage

Pour nommer vos variables, procédures et autres identifiants, suivez ces règles :

1. Un identifiant doit commencer par une lettre (A-Z, a-z) ou un underscore (_)
2. Il peut ensuite contenir des lettres, des chiffres (0-9) ou des underscores
3. Il ne peut pas contenir d'espaces ou de caractères spéciaux
4. Il ne peut pas être un mot-clé réservé

**Exemples valides :**
```pascal
MaVariable  
ma_variable  
Variable123  
_temp
CalculTotal
```

**Exemples invalides :**
```pascal
123Variable    // Commence par un chiffre
Ma-Variable    // Contient un tiret  
Ma Variable    // Contient un espace  
begin          // Mot-clé réservé  
```

## Points clés à retenir

1. Chaque instruction se termine par un point-virgule (`;`)
2. Les blocs de code sont délimités par `begin` et `end`
3. Object Pascal n'est pas sensible à la casse
4. L'indentation améliore la lisibilité mais n'est pas obligatoire
5. Les commentaires commencent par `//` ou sont entourés de `{ }`
6. Les mots-clés réservés ne peuvent pas être utilisés comme identifiants
7. Une unité se termine toujours par `end.` (avec un point)

## Conseils pour débuter

- Prenez l'habitude d'indenter votre code dès le début
- Commentez votre code de manière claire et concise
- Utilisez des noms de variables explicites (évitez `x`, `y`, `z` sauf pour des cas très simples)
- Respectez les conventions de nommage pour faciliter la lecture
- Utilisez le formateur automatique de l'IDE (Ctrl+D) pour garder un code propre

---

La maîtrise de cette syntaxe fondamentale est la première étape essentielle pour programmer efficacement en Delphi. Dans les sections suivantes, nous approfondirons chacun de ces concepts et découvrirons les types de données, les variables, et les structures de contrôle.

⏭️ [Types de données et conversions](/03-langage-object-pascal/02-types-de-donnees-et-conversions.md)
