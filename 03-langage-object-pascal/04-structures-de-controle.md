# 3.4 Structures de contrôle (conditions, boucles)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les structures de contrôle sont au cœur de la programmation. Elles permettent de diriger le flux d'exécution de votre programme en fonction de conditions ou de répéter des instructions plusieurs fois. Dans cette section, nous allons explorer les différentes structures de contrôle disponibles en Object Pascal.

## Structures conditionnelles

Les structures conditionnelles vous permettent d'exécuter différentes parties de code en fonction de conditions spécifiques.

### If-Then-Else

La structure `if-then-else` est la structure conditionnelle la plus courante :

```pascal
if Condition then
  Instruction1
else
  Instruction2;
```

Si la `Condition` est vraie (`True`), alors `Instruction1` est exécutée. Sinon, `Instruction2` est exécutée.

Si vous avez plusieurs instructions à exécuter dans chaque branche, utilisez un bloc `begin-end` :

```pascal
if Age >= 18 then
begin
  ShowMessage('Vous êtes majeur');
  AccesAutorise := True;
end
else
begin
  ShowMessage('Vous êtes mineur');
  AccesAutorise := False;
end;
```

Vous pouvez également enchaîner plusieurs conditions avec `else if` :

```pascal
if Note >= 16 then
  ShowMessage('Très bien')
else if Note >= 14 then
  ShowMessage('Bien')
else if Note >= 12 then
  ShowMessage('Assez bien')
else if Note >= 10 then
  ShowMessage('Passable')
else
  ShowMessage('Insuffisant');
```

#### Conditions composées

Vous pouvez combiner plusieurs conditions avec les opérateurs logiques `and`, `or` et `not` :

```pascal
if (Age >= 18) and (PossedeCarte = True) then
  ShowMessage('Accès autorisé')
else
  ShowMessage('Accès refusé');
```

#### Opérateurs de court-circuit

Pour plus d'efficacité, vous pouvez utiliser les opérateurs de court-circuit `and then` et `or else`. Ils n'évaluent la seconde condition que si nécessaire :

```pascal
// La fonction CouteuseACalculer ne sera appelée que si Age >= 18
if (Age >= 18) and then (CouteuseACalculer > 1000) then
  ShowMessage('Condition remplie');
```

### Case

La structure `case` permet de choisir entre plusieurs options en fonction de la valeur d'une expression :

```pascal
case Expression of
  Valeur1: Instruction1;
  Valeur2: Instruction2;
  Valeur3: Instruction3;
else
  InstructionDefaut;
end;
```

Par exemple :

```pascal
case JourSemaine of
  Lundi:    ShowMessage('Début de semaine');
  Vendredi: ShowMessage('Fin de semaine');
  Samedi, Dimanche: ShowMessage('Week-end');
else
  ShowMessage('Milieu de semaine');
end;
```

Le `case` est souvent utilisé avec des énumérations :

```pascal
type
  TCouleur = (Rouge, Vert, Bleu, Jaune, Orange);

var
  Couleur: TCouleur;
begin
  Couleur := Vert;

  case Couleur of
    Rouge, Orange: ShowMessage('Couleur chaude');
    Bleu, Vert:   ShowMessage('Couleur froide');
    Jaune:        ShowMessage('Couleur intermédiaire');
  end;
end;
```

### Opérateur ternaire

Depuis Delphi 10.3, vous pouvez utiliser l'opérateur ternaire (`?:`) pour des conditions simples en une seule ligne :

```pascal
// Syntaxe: Condition ? ValeurSiVrai : ValeurSiFaux
Resultat := Age >= 18 ? 'Majeur' : 'Mineur';
```

Cela équivaut à :

```pascal
if Age >= 18 then
  Resultat := 'Majeur'
else
  Resultat := 'Mineur';
```

## Structures de boucle

Les boucles permettent de répéter des instructions plusieurs fois. Delphi propose plusieurs types de boucles pour différentes situations.

### For

La boucle `for` est utilisée pour répéter un bloc d'instructions un nombre précis de fois :

```pascal
// La variable Compteur prendra successivement les valeurs de Debut à Fin
for Compteur := Debut to Fin do
  Instruction;
```

Exemple :

```pascal
var
  I: Integer;
  Somme: Integer;
begin
  Somme := 0;

  for I := 1 to 10 do
    Somme := Somme + I;  // Calcule 1+2+3+...+10

  ShowMessage('La somme est : ' + IntToStr(Somme));  // Affiche 55
end;
```

Pour compter à rebours, utilisez `downto` au lieu de `to` :

```pascal
for I := 10 downto 1 do
  ShowMessage(IntToStr(I));  // Compte à rebours de 10 à 1
```

Si vous avez plusieurs instructions dans la boucle, utilisez un bloc `begin-end` :

```pascal
for I := 1 to 5 do
begin
  X := I * I;
  ShowMessage('Le carré de ' + IntToStr(I) + ' est ' + IntToStr(X));
end;
```

#### For avec tableaux dynamiques

Depuis Delphi XE7, vous pouvez parcourir les éléments d'un tableau dynamique directement :

```pascal
var
  Nombres: array of Integer;
  Nombre: Integer;
begin
  SetLength(Nombres, 5);
  Nombres[0] := 10;
  Nombres[1] := 20;
  Nombres[2] := 30;
  Nombres[3] := 40;
  Nombres[4] := 50;

  // Parcourt directement les éléments (pas les indices)
  for Nombre in Nombres do
    ShowMessage(IntToStr(Nombre));
end;
```

Cela fonctionne également avec les chaînes, les listes et d'autres collections :

```pascal
var
  Texte: string;
  C: Char;
begin
  Texte := 'Bonjour';

  for C in Texte do
    ShowMessage(C);  // Affiche chaque caractère séparément
end;
```

### While

La boucle `while` répète un bloc d'instructions tant qu'une condition est vraie :

```pascal
while Condition do
  Instruction;
```

La condition est vérifiée avant chaque itération. Si elle est fausse dès le départ, le bloc d'instructions n'est jamais exécuté.

Exemple :

```pascal
var
  Compteur: Integer;
begin
  Compteur := 1;

  while Compteur <= 5 do
  begin
    ShowMessage(IntToStr(Compteur));
    Compteur := Compteur + 1;
  end;
end;
```

La boucle `while` est utile lorsque vous ne savez pas à l'avance combien d'itérations seront nécessaires :

```pascal
var
  Nombre: Integer;
begin
  Nombre := 1000;

  while Nombre > 1 do
  begin
    ShowMessage(IntToStr(Nombre));
    Nombre := Nombre div 2;  // Division entière par 2
  end;
end;
```

### Repeat-Until

La boucle `repeat-until` est similaire à `while`, mais la condition est vérifiée après chaque itération, et la boucle continue jusqu'à ce que la condition soit vraie :

```pascal
repeat
  Instructions;
until Condition;
```

Le bloc d'instructions est toujours exécuté au moins une fois, même si la condition est vraie dès le départ.

Exemple :

```pascal
var
  Nombre: Integer;
begin
  Nombre := 1;

  repeat
    ShowMessage(IntToStr(Nombre));
    Nombre := Nombre * 2;
  until Nombre > 100;  // Continue jusqu'à ce que Nombre dépasse 100
end;
```

Différence principale avec `while` :
- `while` : vérifie la condition avant chaque itération
- `repeat-until` : vérifie la condition après chaque itération

### Boucles infinies contrôlées

Parfois, vous avez besoin d'une boucle qui continue indéfiniment jusqu'à ce qu'une condition spécifique soit remplie à l'intérieur de la boucle :

```pascal
while True do
begin
  // Traitement

  if ConditionArret then
    Break;  // Sort immédiatement de la boucle
end;
```

L'instruction `Break` permet de sortir immédiatement d'une boucle, quelle que soit la condition :

```pascal
var
  I: Integer;
begin
  for I := 1 to 100 do
  begin
    if I = 10 then
      Break;  // Sort de la boucle quand I atteint 10

    ShowMessage(IntToStr(I));
  end;
end;
```

L'instruction `Continue` permet de passer directement à l'itération suivante :

```pascal
var
  I: Integer;
begin
  for I := 1 to 10 do
  begin
    if I mod 2 = 0 then
      Continue;  // Passe à l'itération suivante si I est pair

    ShowMessage(IntToStr(I));  // N'affiche que les nombres impairs
  end;
end;
```

## Structures imbriquées

Vous pouvez imbriquer des structures de contrôle les unes dans les autres pour créer des logiques plus complexes :

### Conditions imbriquées

```pascal
if ConditionA then
begin
  if ConditionB then
    ShowMessage('A et B sont vraies')
  else
    ShowMessage('A est vraie, B est fausse');
end
else
begin
  ShowMessage('A est fausse');
end;
```

### Boucles imbriquées

```pascal
var
  I, J: Integer;
begin
  for I := 1 to 3 do
  begin
    for J := 1 to 3 do
    begin
      ShowMessage('I = ' + IntToStr(I) + ', J = ' + IntToStr(J));
    end;
  end;
end;
```

Cet exemple affichera toutes les combinaisons de I et J pour des valeurs de 1 à 3.

## Conseils pratiques

1. **Lisibilité** :
   - Indentez correctement votre code pour rendre la structure plus claire
   - Utilisez des blocs `begin-end` même pour une seule instruction (bien que facultatif) pour faciliter les modifications futures

2. **Performances** :
   - Pour des itérations simples sur une plage de nombres, `for` est généralement plus efficace
   - Utilisez `and then` et `or else` pour éviter d'évaluer des conditions inutiles

3. **Choix de la boucle** :
   - `for` : quand vous connaissez à l'avance le nombre d'itérations
   - `while` : quand vous ne savez pas combien d'itérations seront nécessaires et que la condition doit être vérifiée avant de commencer
   - `repeat-until` : quand vous voulez exécuter le bloc au moins une fois

4. **Éviter les boucles infinies** :
   - Assurez-vous qu'il y a toujours un moyen de sortir d'une boucle
   - Vérifiez que vos variables de boucle sont correctement incrémentées/décrémentées

5. **Simplifiez votre code** :
   - Utilisez `case` plutôt que de longues séries de `if-else if` quand vous testez une seule variable
   - Pour Delphi 10.3 et supérieur, l'opérateur ternaire peut rendre le code plus concis pour des conditions simples

---

Cette section vous a présenté les structures de contrôle fondamentales en Object Pascal. Maîtriser ces structures vous permettra de créer des programmes capables de prendre des décisions et de répéter des actions selon vos besoins. Dans la prochaine section, nous aborderons les procédures et fonctions, qui vous permettront d'organiser votre code en blocs réutilisables.

⏭️ [Procédures et fonctions](/03-langage-object-pascal/05-procedures-et-fonctions.md)
