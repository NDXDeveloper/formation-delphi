# 3.3 Variables, constantes et opérateurs

Dans cette section, nous allons explorer comment déclarer et utiliser les variables et constantes en Object Pascal, ainsi que les opérateurs qui permettent de manipuler leurs valeurs. Ces concepts sont fondamentaux pour tout programme Delphi.

## Variables

Une variable est un espace en mémoire nommé qui peut contenir une valeur d'un type spécifique. Cette valeur peut changer au cours de l'exécution du programme.

### Déclaration de variables

En Object Pascal, les variables doivent être déclarées avant d'être utilisées. La déclaration se fait généralement dans une section spécifique, délimitée par le mot-clé `var` :

```pascal
var
  Age: Integer;
  Nom: string;
  Salaire: Currency;
  EstActif: Boolean;
```

### Où déclarer les variables ?

Les variables peuvent être déclarées à différents niveaux :

1. **Variables globales (au niveau de l'unité)** - Accessibles partout dans l'unité :
   ```pascal
   unit MonUnite;

   interface
     // Variables visibles par d'autres unités
     var
       CompteurGlobal: Integer;

   implementation
     // Variables privées à cette unité
     var
       CompteurInterne: Integer;
   ```

2. **Variables locales (dans une procédure ou fonction)** - Accessibles uniquement à l'intérieur de la procédure :
   ```pascal
   procedure CalculerTotal;
   var
     SousTotal: Double;
     TVA: Double;
     Total: Double;
   begin
     // Code utilisant ces variables
   end;
   ```

3. **Variables de classe (dans une classe)** - Déclarées comme champs d'une classe :
   ```pascal
   type
     TClient = class
     private
       FNom: string;
       FAge: Integer;
     public
       property Nom: string read FNom write FNom;
       property Age: Integer read FAge write FAge;
     end;
   ```

### Initialisation des variables

Les variables peuvent être initialisées lors de leur déclaration ou plus tard dans le code :

```pascal
var
  Compteur: Integer = 0;        // Initialisation directe (Delphi 2009+)
  NomClient: string = 'Dupont';

procedure Test;
var
  X: Integer;
begin
  X := 10;                      // Initialisation avec l'opérateur d'affectation
end;
```

Dans Delphi, les variables globales et les champs de classe sont automatiquement initialisés à une valeur par défaut (0 pour les nombres, vide pour les chaînes, `nil` pour les pointeurs et objets, etc.). Cependant, les variables locales ne sont pas initialisées automatiquement, il est donc recommandé de toujours les initialiser explicitement.

### Portée des variables

La portée d'une variable détermine où celle-ci peut être utilisée dans votre code :

- Les variables globales sont accessibles dans toute l'unité (et potentiellement d'autres unités)
- Les variables locales ne sont accessibles que dans le bloc où elles sont déclarées
- Les variables de boucle sont limitées à la boucle elle-même

```pascal
procedure Exemple;
var
  X: Integer;
begin
  X := 10;  // X est accessible ici

  for I := 1 to 10 do  // I est la variable de boucle
  begin
    // X et I sont accessibles ici
    X := X + I;
  end;

  // X est encore accessible ici, mais pas I

  begin
    var Y: Integer;  // Variable locale à ce bloc (Delphi 10.3+)
    Y := X * 2;
    // X et Y sont accessibles ici
  end;

  // X est accessible ici, mais pas Y
end;
```

Pour les versions récentes de Delphi (10.3+), vous pouvez déclarer des variables directement dans un bloc de code grâce à la déclaration de variable inline avec le mot-clé `var`.

### Durée de vie des variables

La durée de vie d'une variable détermine quand elle est créée et détruite :

- Les variables globales existent pendant toute la durée d'exécution du programme
- Les variables locales existent uniquement pendant l'exécution de la procédure ou fonction
- Les variables de boucle existent uniquement pendant l'exécution de la boucle

## Constantes

Une constante est similaire à une variable, mais sa valeur ne peut pas être modifiée après sa déclaration. Les constantes améliorent la lisibilité et la maintenance du code.

### Déclaration de constantes

Les constantes sont déclarées avec le mot-clé `const` :

```pascal
const
  PI = 3.14159265358979;
  VERSION = '1.0.0';
  MAX_UTILISATEURS = 100;
  COULEUR_FOND = $00FFFF;  // Valeur hexadécimale (jaune)
```

Notez que pour les constantes simples, on utilise `=` plutôt que `:=`.

### Constantes typées

Depuis Delphi 2009, vous pouvez déclarer des constantes avec un type explicite :

```pascal
const
  PI: Double = 3.14159265358979;
  VERSION: string = '1.0.0';
```

### Constantes d'énumération

Les énumérations sont un type spécial de constantes groupées :

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TMois = (Janvier, Fevrier, Mars, Avril, Mai, Juin,
           Juillet, Aout, Septembre, Octobre, Novembre, Decembre);
```

Ces valeurs peuvent être utilisées comme ceci :

```pascal
var
  Jour: TJourSemaine;
begin
  Jour := Mercredi;

  if Jour = Vendredi then
    ShowMessage('Bon weekend !');

  // On peut aussi accéder à la valeur ordinale (position)
  ShowMessage(IntToStr(Ord(Jour)));  // Affiche 2 (car Lundi est 0)
end;
```

### Constantes de ressources

Delphi permet également de définir des constantes dans un fichier de ressources (.rc), utiles pour les chaînes multilingues ou les grands blocs de données.

## Opérateurs

Les opérateurs permettent d'effectuer des opérations sur les variables et les constantes.

### Opérateurs arithmétiques

Pour les opérations mathématiques de base :

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `+` | Addition | `A + B` |
| `-` | Soustraction | `A - B` |
| `*` | Multiplication | `A * B` |
| `/` | Division réelle | `A / B` (donne un résultat à virgule) |
| `div` | Division entière | `A div B` (donne un entier, ignore reste) |
| `mod` | Modulo (reste) | `A mod B` (reste de la division entière) |

Exemple d'utilisation :

```pascal
var
  A, B, C: Integer;
  D: Double;
begin
  A := 10;
  B := 3;

  C := A + B;      // C = 13
  C := A - B;      // C = 7
  C := A * B;      // C = 30
  C := A div B;    // C = 3 (division entière)
  C := A mod B;    // C = 1 (reste de 10 / 3)

  D := A / B;      // D = 3.33333... (division réelle)
end;
```

### Opérateurs d'assignation composés

Delphi 12 a introduit des opérateurs d'assignation composés pour simplifier les opérations courantes :

```pascal
var
  X: Integer;
begin
  X := 5;
  X += 3;  // Équivalent à X := X + 3
  X -= 2;  // Équivalent à X := X - 2
  X *= 4;  // Équivalent à X := X * 4
  X /= 2;  // Équivalent à X := X / 2 (pour les types réels)
end;
```

<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur**</span>

### Opérateurs de comparaison

Pour comparer des valeurs :

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `=` | Égal à | `A = B` |
| `<>` | Différent de | `A <> B` |
| `<` | Inférieur à | `A < B` |
| `>` | Supérieur à | `A > B` |
| `<=` | Inférieur ou égal à | `A <= B` |
| `>=` | Supérieur ou égal à | `A >= B` |

Exemple d'utilisation :

```pascal
var
  Age: Integer;
  EstAdulte: Boolean;
begin
  Age := 20;

  EstAdulte := Age >= 18;  // True

  if Age > 65 then
    ShowMessage('Senior')
  else if Age >= 18 then
    ShowMessage('Adulte')
  else
    ShowMessage('Mineur');
end;
```

### Opérateurs logiques

Pour combiner des expressions booléennes :

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `and` | ET logique | `A and B` |
| `or` | OU logique | `A or B` |
| `not` | NON logique | `not A` |
| `xor` | OU exclusif | `A xor B` |

Exemple d'utilisation :

```pascal
var
  Age: Integer;
  EstMembre: Boolean;
  AAcces: Boolean;
begin
  Age := 20;
  EstMembre := True;

  // Accès autorisé si adulte ET membre
  AAcces := (Age >= 18) and EstMembre;  // True

  // Autre exemple avec plusieurs conditions
  if (Age >= 18) and (Age <= 65) and EstMembre then
    ShowMessage('Membre actif en âge de travailler');
end;
```

Les opérateurs logiques évaluent généralement les deux opérandes, même si le résultat peut être déterminé à partir du premier. Dans Delphi, il existe aussi des opérateurs de court-circuit qui peuvent optimiser l'évaluation :

```pascal
// Opérateurs de court-circuit
if EstMembre and then (CalculerPoints > 100) then
  // Le calcul n'est effectué que si EstMembre est True

if EstBloque or else (SoldeNegatif) then
  // Le solde n'est vérifié que si EstBloque est False
```

### Opérateurs de chaînes

Pour les chaînes de caractères :

```pascal
var
  Prenom, Nom, NomComplet: string;
begin
  Prenom := 'Jean';
  Nom := 'Dupont';

  // Concaténation avec l'opérateur +
  NomComplet := Prenom + ' ' + Nom;  // 'Jean Dupont'
end;
```

### Opérateurs d'ensemble

Pour manipuler les ensembles :

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `+` | Union | `A + B` |
| `-` | Différence | `A - B` |
| `*` | Intersection | `A * B` |
| `<=` | Est sous-ensemble de | `A <= B` |
| `>=` | Est sur-ensemble de | `A >= B` |
| `in` | Appartient à | `e in A` |

Exemple d'utilisation :

```pascal
type
  TFruit = (Pomme, Poire, Banane, Orange, Fraise);
  TFruits = set of TFruit;

var
  FruitsRouges, FruitsJaunes, MesFruits: TFruits;
begin
  FruitsRouges := [Pomme, Fraise];
  FruitsJaunes := [Banane, Poire];

  // Union
  MesFruits := FruitsRouges + FruitsJaunes;  // [Pomme, Poire, Banane, Fraise]

  // Test d'appartenance
  if Pomme in MesFruits then
    ShowMessage('J''ai des pommes !');
end;
```

### Priorité des opérateurs

Comme en mathématiques, les opérateurs ont une priorité qui détermine l'ordre d'évaluation :

1. Opérateurs unaires (`not`, `-` unaire)
2. Opérateurs multiplicatifs (`*`, `/`, `div`, `mod`, `and`)
3. Opérateurs additifs (`+`, `-`, `or`, `xor`)
4. Opérateurs relationnels (`=`, `<>`, `<`, `>`, `<=`, `>=`, `in`)

Pour modifier cette priorité, utilisez des parenthèses :

```pascal
var
  A, B, C, Resultat: Integer;
begin
  A := 5;
  B := 3;
  C := (10);

  // Sans parenthèses : multiplication d'abord
  Resultat := A + B * C;      // 5 + (3 * 10) = 5 + 30 = 35

  // Avec parenthèses : addition d'abord
  Resultat := (A + B) * C;    // (5 + 3) * 10 = 8 * 10 = 80
end;
```

## Conseils pratiques

1. **Nommage explicite** :
   - Utilisez des noms de variables descriptifs qui expliquent leur rôle
   - Adoptez une convention cohérente (comme PascalCase ou camelCase)

2. **Gestion de la portée** :
   - Limitez la portée des variables au minimum nécessaire
   - Évitez les variables globales quand possible

3. **Utilisation des constantes** :
   - Utilisez des constantes pour toutes les valeurs fixes dans votre code
   - Regroupez les constantes liées dans des énumérations

4. **Clarté du code** :
   - Utilisez des parenthèses pour clarifier l'ordre des opérations
   - Divisez les expressions complexes en plusieurs étapes

5. **Initialisation** :
   - Initialisez toujours vos variables locales avant de les utiliser
   - Vérifiez les valeurs avant les opérations problématiques (divisions, etc.)

6. **Conventions de préfixe** :
   - Pour les champs privés de classes, le préfixe `F` est couramment utilisé :
   ```pascal
   TClient = class
   private
     FNom: string;    // Champ privé avec préfixe F
   public
     property Nom: string read FNom write FNom;
   end;
   ```

---

Cette section vous a présenté les variables, constantes et opérateurs fondamentaux en Object Pascal. Ces concepts sont les outils de base que vous utiliserez dans tous vos programmes Delphi. Dans la prochaine section, nous aborderons les structures de contrôle qui vous permettront de diriger le flux d'exécution de vos programmes.
