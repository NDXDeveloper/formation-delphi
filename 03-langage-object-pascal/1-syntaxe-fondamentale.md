# 3.1 Syntaxe fondamentale

Le langage Object Pascal utilisé dans Delphi est à la fois puissant et accessible. Cette section vous présentera les bases syntaxiques dont vous aurez besoin pour commencer à programmer avec Delphi.

## Structure générale d'un programme

Un programme Delphi est généralement composé de plusieurs unités (fichiers `.pas`). Chaque unité possède une structure spécifique :

```pascal
unit NomUnite;

interface
  // Déclarations publiques (visibles par les autres unités)
  // Types, constantes, variables, procédures, fonctions, classes...

implementation
  // Code d'implémentation et déclarations privées
  // Définitions des procédures et fonctions
  // Variables locales à l'unité

initialization
  // Code exécuté quand l'unité est chargée (optionnel)

finalization
  // Code exécuté quand l'unité est déchargée (optionnel)

end.
```

Remarquez que chaque instruction se termine par un point-virgule (`;`) et que l'unité elle-même se termine par `end.` (avec un point).

## Commentaires

Les commentaires permettent de documenter votre code. Delphi supporte deux styles de commentaires :

```pascal
// Commentaire sur une seule ligne

{ Commentaire
  sur plusieurs
  lignes }

(* Autre style de
   commentaire sur
   plusieurs lignes *)
```

Il est recommandé d'utiliser généreusement les commentaires pour expliquer votre code, surtout pour les débutants.

## Sensibilité à la casse

Object Pascal n'est **pas sensible à la casse**. Cela signifie que les identifiants suivants sont considérés comme identiques :

```pascal
MaVariable
mavariable
MAVARIABLE
```

Cependant, il est fortement recommandé d'adopter une convention de nommage cohérente, comme le "PascalCase" (chaque mot commence par une majuscule) pour améliorer la lisibilité.

## Blocs de code

Les blocs de code sont délimités par les mots-clés `begin` et `end`. Ils regroupent plusieurs instructions à exécuter séquentiellement :

```pascal
begin
  Instruction1;
  Instruction2;
  Instruction3;
end;
```

Notez que le mot-clé `end` est généralement suivi d'un point-virgule, sauf à la fin de l'unité où il est suivi d'un point.

## Indentation et formatage

Bien que l'indentation ne soit pas obligatoire pour le compilateur, elle est essentielle pour la lisibilité du code :

```pascal
procedure ExempleIndentation;
begin
  if Condition then
  begin
    // Code si la condition est vraie
    Instruction1;
    Instruction2;
  end
  else
  begin
    // Code si la condition est fausse
    Instruction3;
    Instruction4;
  end;
end;
```

## Déclaration de variables

Les variables sont déclarées avec le mot-clé `var` suivi du nom de la variable et de son type :

```pascal
var
  Age: Integer;
  Nom: string;
  EstActif: Boolean;
  Prix: Double;
```

Vous pouvez également déclarer plusieurs variables du même type sur une ligne :

```pascal
var
  X, Y, Z: Integer;
  Prenom, Nom: string;
```

## Affectation de valeurs

L'opérateur d'affectation en Object Pascal est `:=` (et non `=` comme dans certains autres langages) :

```pascal
Age := 25;
Nom := 'Dupont';
EstActif := True;
Prix := 19.99;
```

## Terminologie importante

Pour faciliter la compréhension des prochaines sections, voici quelques termes fondamentaux :

- **Identificateur** : nom donné à une variable, une procédure, une fonction, une classe, etc.
- **Mot-clé** : mot réservé par le langage (comme `begin`, `end`, `var`, `if`, etc.)
- **Expression** : combinaison de variables, constantes et opérateurs qui produit une valeur
- **Instruction** : ligne de code qui effectue une action spécifique
- **Bloc** : ensemble d'instructions regroupées entre `begin` et `end`

## Premier exemple : Hello World

Voici un exemple simple de code dans un événement OnClick d'un bouton qui affiche un message :

```pascal
procedure TForm1.ButtonHelloClick(Sender: TObject);
begin
  ShowMessage('Bonjour, monde !');
end;
```

## Noms de fichiers et unités

Dans Delphi, le nom du fichier `.pas` correspond généralement au nom de l'unité qu'il contient. Par exemple, l'unité `MaForme` sera stockée dans le fichier `MaForme.pas`.

Les formulaires ont également un fichier `.dfm` associé qui stocke les informations de conception visuelle (comme `MaForme.dfm`).

## Projet principal

Un projet Delphi contient généralement un fichier `.dpr` qui sert de point d'entrée au programme :

```pascal
program MonApplication;

uses
  System.SysUtils,
  MaForme in 'MaForme.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

La section `uses` liste les unités utilisées par le projet, et les instructions dans le bloc principal initialisent et lancent l'application.

## Directive de compilation

Les directives de compilation sont des instructions spéciales pour le compilateur, encadrées par `{$...}` :

```pascal
{$IFDEF DEBUG}
  ShowMessage('Mode débogage activé');
{$ENDIF}
```

Ces directives sont puissantes et permettent de personnaliser la compilation selon différents critères.

---

Dans les prochaines sections, nous aborderons plus en détail les types de données, les structures de contrôle et la programmation orientée objet en Object Pascal. Cette introduction à la syntaxe fondamentale vous donne les bases pour comprendre et écrire du code Delphi.
