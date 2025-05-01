# 3.11 Records avancés et opérateurs surchargés

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans les versions récentes de Delphi, les records sont devenus beaucoup plus puissants, offrant des fonctionnalités autrefois réservées aux classes. Dans cette section, nous explorerons les capacités avancées des records et comment surcharger les opérateurs pour créer des types de données personnalisés plus intuitifs et plus faciles à utiliser.

## Records : bien plus que de simples structures

Traditionnellement, un record en Pascal était simplement un regroupement de champs de différents types. Mais dans le Delphi moderne, les records peuvent avoir des méthodes, des propriétés, des constructeurs, et même surcharger des opérateurs.

### Rappel sur les records de base

Commençons par un rappel sur les records de base :

```pascal
type
  TAdresse = record
    Rue: string;
    Ville: string;
    CodePostal: string;
    Pays: string;
  end;

var
  Adresse: TAdresse;
begin
  Adresse.Rue := '123 Rue Principale';
  Adresse.Ville := 'Paris';
  Adresse.CodePostal := '75000';
  Adresse.Pays := 'France';
end;
```

### Pourquoi utiliser des records avancés ?

Les records avancés offrent plusieurs avantages :

1. **Efficacité** : Les records sont alloués sur la pile (stack) plutôt que sur le tas (heap), ce qui les rend plus rapides à créer et détruire
2. **Pas de gestion mémoire manuelle** : Contrairement aux classes, vous n'avez pas besoin d'appeler `Free`
3. **Sémantique de valeur** : Les records sont copiés par valeur, ce qui simplifie leur manipulation
4. **Performances** : Pour les petites structures de données, les records peuvent être plus performants que les classes

## Ajout de méthodes aux records

Vous pouvez ajouter des méthodes à vos records, tout comme pour les classes :

```pascal
type
  TPoint = record
  private
    FX, FY: Integer;
  public
    // Méthodes
    procedure Initialiser(X, Y: Integer);
    function Distance(const AutrePoint: TPoint): Double;
    procedure Deplacer(DX, DY: Integer);

    // Propriétés
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

procedure TPoint.Initialiser(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;

function TPoint.Distance(const AutrePoint: TPoint): Double;
begin
  Result := Sqrt(Sqr(FX - AutrePoint.X) + Sqr(FY - AutrePoint.Y));
end;

procedure TPoint.Deplacer(DX, DY: Integer);
begin
  FX := FX + DX;
  FY := FY + DY;
end;
```

Utilisation :

```pascal
var
  Point1, Point2: TPoint;
  Dist: Double;
begin
  Point1.Initialiser(10, 20);
  Point2.Initialiser(30, 40);

  Dist := Point1.Distance(Point2);
  ShowMessage(Format('Distance entre les points : %.2f', [Dist]));

  Point1.Deplacer(5, 10);
  ShowMessage(Format('Nouvelles coordonnées : (%d, %d)', [Point1.X, Point1.Y]));
end;
```

## Constructeurs dans les records

Les records peuvent avoir des constructeurs, ce qui facilite leur initialisation :

```pascal
type
  TPoint = record
  private
    FX, FY: Integer;
  public
    constructor Create(X, Y: Integer);

    property X: Integer read FX;
    property Y: Integer read FY;
  end;

constructor TPoint.Create(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;
```

Utilisation :

```pascal
var
  Point: TPoint;
begin
  Point := TPoint.Create(10, 20);
  ShowMessage(Format('Point : (%d, %d)', [Point.X, Point.Y]));
end;
```

Contrairement aux classes, le constructeur d'un record n'a pas besoin d'être appelé. Vous pouvez simplement utiliser un record sans l'initialiser, auquel cas tous ses champs auront leur valeur par défaut (0, nil, False, etc.).

## Records avec des sections de visibilité

Comme les classes, les records peuvent avoir des sections de visibilité (`private`, `public`, etc.) pour contrôler l'accès à leurs membres :

```pascal
type
  TRectangle = record
  private
    FLargeur, FHauteur: Double;
    function GetSurface: Double;
    function GetPerimetre: Double;
  public
    constructor Create(Largeur, Hauteur: Double);

    property Largeur: Double read FLargeur write FLargeur;
    property Hauteur: Double read FHauteur write FHauteur;
    property Surface: Double read GetSurface;
    property Perimetre: Double read GetPerimetre;
  end;

constructor TRectangle.Create(Largeur, Hauteur: Double);
begin
  FLargeur := Largeur;
  FHauteur := Hauteur;
end;

function TRectangle.GetSurface: Double;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.GetPerimetre: Double;
begin
  Result := 2 * (FLargeur + FHauteur);
end;
```

## Records imbriqués

Vous pouvez imbriquer des records pour créer des structures de données plus complexes :

```pascal
type
  TAdresse = record
    Rue: string;
    Ville: string;
    CodePostal: string;
    Pays: string;
  end;

  TPersonne = record
    Nom: string;
    Prenom: string;
    DateNaissance: TDate;
    Adresse: TAdresse;  // Record imbriqué

    function Age: Integer;
  end;

function TPersonne.Age: Integer;
begin
  Result := YearsBetween(Date, DateNaissance);
end;
```

## Surcharge d'opérateurs

Une des fonctionnalités les plus puissantes des records avancés est la possibilité de surcharger les opérateurs. Cela vous permet de définir comment les opérateurs standard (`+`, `-`, `*`, etc.) se comportent avec vos types personnalisés.

### Pourquoi surcharger les opérateurs ?

La surcharge d'opérateurs permet d'utiliser vos types personnalisés de manière plus naturelle et intuitive. Par exemple, si vous avez un type `TVector`, il est plus naturel d'écrire `V1 + V2` que `V1.Add(V2)`.

### Opérateurs disponibles

En Object Pascal, vous pouvez surcharger les opérateurs suivants :

- Arithmétiques : `+`, `-`, `*`, `/`, `div`, `mod`
- Comparaison : `=`, `<>`, `<`, `>`, `<=`, `>=`
- Logiques : `and`, `or`, `xor`, `not`
- Explicites et implicites : conversions entre types

### Syntaxe pour surcharger les opérateurs

Pour surcharger un opérateur, vous utilisez le mot-clé `class operator` suivi de l'opérateur à surcharger :

```pascal
type
  TPoint = record
  private
    FX, FY: Integer;
  public
    constructor Create(X, Y: Integer);

    // Surcharge d'opérateurs
    class operator Add(const A, B: TPoint): TPoint;
    class operator Subtract(const A, B: TPoint): TPoint;
    class operator Equal(const A, B: TPoint): Boolean;
    class operator NotEqual(const A, B: TPoint): Boolean;

    property X: Integer read FX;
    property Y: Integer read FY;
  end;

constructor TPoint.Create(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;

class operator TPoint.Add(const A, B: TPoint): TPoint;
begin
  Result := TPoint.Create(A.X + B.X, A.Y + B.Y);
end;

class operator TPoint.Subtract(const A, B: TPoint): TPoint;
begin
  Result := TPoint.Create(A.X - B.X, A.Y - B.Y);
end;

class operator TPoint.Equal(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TPoint.NotEqual(const A, B: TPoint): Boolean;
begin
  Result := not ((A.X = B.X) and (A.Y = B.Y));
end;
```

Utilisation :

```pascal
var
  P1, P2, P3: TPoint;
begin
  P1 := TPoint.Create(10, 20);
  P2 := TPoint.Create(30, 40);

  // Utilisation des opérateurs surchargés
  P3 := P1 + P2;  // Équivalent à P1.Add(P2)
  ShowMessage(Format('P1 + P2 = (%d, %d)', [P3.X, P3.Y]));

  P3 := P2 - P1;  // Équivalent à P2.Subtract(P1)
  ShowMessage(Format('P2 - P1 = (%d, %d)', [P3.X, P3.Y]));

  if P1 = P2 then  // Équivalent à P1.Equal(P2)
    ShowMessage('Les points sont égaux')
  else
    ShowMessage('Les points sont différents');
end;
```

### Conversion de types avec des opérateurs

Vous pouvez également surcharger les opérateurs de conversion pour convertir votre record en d'autres types et vice versa :

```pascal
type
  TPoint = record
  private
    FX, FY: Integer;
  public
    constructor Create(X, Y: Integer);

    // Conversions
    class operator Implicit(const Point: TPoint): string;
    class operator Explicit(const Str: string): TPoint;

    property X: Integer read FX;
    property Y: Integer read FY;
  end;

class operator TPoint.Implicit(const Point: TPoint): string;
begin
  Result := Format('(%d,%d)', [Point.X, Point.Y]);
end;

class operator TPoint.Explicit(const Str: string): TPoint;
var
  X, Y: Integer;
  Parts: TArray<string>;
begin
  // Format attendu : "(X,Y)"
  Parts := Str.Replace('(', '').Replace(')', '').Split([',']);
  if Length(Parts) = 2 then
  begin
    if TryStrToInt(Parts[0], X) and TryStrToInt(Parts[1], Y) then
      Result := TPoint.Create(X, Y)
    else
      Result := TPoint.Create(0, 0);
  end
  else
    Result := TPoint.Create(0, 0);
end;
```

Utilisation :

```pascal
var
  P: TPoint;
  S: string;
begin
  P := TPoint.Create(10, 20);

  // Conversion implicite de TPoint vers string
  S := P;  // S = "(10,20)"
  ShowMessage('Point sous forme de chaîne : ' + S);

  // Conversion explicite de string vers TPoint
  P := TPoint(S);
  ShowMessage(Format('Point converti : (%d, %d)', [P.X, P.Y]));
end;
```

#### Conversion implicite vs explicite

- **Implicite** (`Implicit`) : La conversion est automatique et ne nécessite pas de cast explicite
- **Explicite** (`Explicit`) : La conversion nécessite un cast explicite (par exemple, `TPoint(S)`)

En général, utilisez `Implicit` pour les conversions sans perte d'information et `Explicit` pour les conversions potentiellement ambiguës ou avec perte d'information.

## Exemple complet : type de fraction

Voici un exemple complet d'un record avancé représentant une fraction, avec constructeurs, méthodes et opérateurs surchargés :

```pascal
type
  TFraction = record
  private
    FNumerateur: Integer;
    FDenominateur: Integer;
    procedure Simplifier;
    function GetValeur: Double;
  public
    constructor Create(Numerateur: Integer; Denominateur: Integer = 1);

    // Méthodes
    function ToString: string;
    function Inverse: TFraction;

    // Opérateurs
    class operator Add(const A, B: TFraction): TFraction;
    class operator Subtract(const A, B: TFraction): TFraction;
    class operator Multiply(const A, B: TFraction): TFraction;
    class operator Divide(const A, B: TFraction): TFraction;
    class operator Equal(const A, B: TFraction): Boolean;
    class operator NotEqual(const A, B: TFraction): Boolean;
    class operator LessThan(const A, B: TFraction): Boolean;
    class operator GreaterThan(const A, B: TFraction): Boolean;
    class operator Implicit(const Value: Integer): TFraction;
    class operator Implicit(const Fraction: TFraction): Double;

    property Numerateur: Integer read FNumerateur;
    property Denominateur: Integer read FDenominateur;
    property Valeur: Double read GetValeur;
  end;

// Fonction utilitaire pour le PGCD (Plus Grand Commun Diviseur)
function PGCD(A, B: Integer): Integer;
begin
  if B = 0 then
    Result := A
  else
    Result := PGCD(B, A mod B);
end;

constructor TFraction.Create(Numerateur, Denominateur: Integer);
begin
  if Denominateur = 0 then
    raise Exception.Create('Le dénominateur ne peut pas être zéro');

  FNumerateur := Numerateur;
  FDenominateur := Denominateur;

  // Gestion du signe
  if FDenominateur < 0 then
  begin
    FNumerateur := -FNumerateur;
    FDenominateur := -FDenominateur;
  end;

  Simplifier;
end;

procedure TFraction.Simplifier;
var
  Diviseur: Integer;
begin
  // Ne rien faire si le numérateur est zéro
  if FNumerateur = 0 then
  begin
    FDenominateur := 1;
    Exit;
  end;

  // Calculer le PGCD et simplifier
  Diviseur := PGCD(Abs(FNumerateur), FDenominateur);
  if Diviseur > 1 then
  begin
    FNumerateur := FNumerateur div Diviseur;
    FDenominateur := FDenominateur div Diviseur;
  end;
end;

function TFraction.GetValeur: Double;
begin
  Result := FNumerateur / FDenominateur;
end;

function TFraction.ToString: string;
begin
  if FDenominateur = 1 then
    Result := IntToStr(FNumerateur)
  else
    Result := Format('%d/%d', [FNumerateur, FDenominateur]);
end;

function TFraction.Inverse: TFraction;
begin
  if FNumerateur = 0 then
    raise Exception.Create('Impossible d''inverser une fraction nulle');

  Result := TFraction.Create(FDenominateur, FNumerateur);
end;

class operator TFraction.Add(const A, B: TFraction): TFraction;
begin
  Result := TFraction.Create(
    A.Numerateur * B.Denominateur + B.Numerateur * A.Denominateur,
    A.Denominateur * B.Denominateur
  );
end;

class operator TFraction.Subtract(const A, B: TFraction): TFraction;
begin
  Result := TFraction.Create(
    A.Numerateur * B.Denominateur - B.Numerateur * A.Denominateur,
    A.Denominateur * B.Denominateur
  );
end;

class operator TFraction.Multiply(const A, B: TFraction): TFraction;
begin
  Result := TFraction.Create(
    A.Numerateur * B.Numerateur,
    A.Denominateur * B.Denominateur
  );
end;

class operator TFraction.Divide(const A, B: TFraction): TFraction;
begin
  if B.Numerateur = 0 then
    raise Exception.Create('Division par zéro');

  Result := TFraction.Create(
    A.Numerateur * B.Denominateur,
    A.Denominateur * B.Numerateur
  );
end;

class operator TFraction.Equal(const A, B: TFraction): Boolean;
begin
  // Les fractions sont déjà simplifiées, donc on peut comparer directement
  Result := (A.Numerateur = B.Numerateur) and (A.Denominateur = B.Denominateur);
end;

class operator TFraction.NotEqual(const A, B: TFraction): Boolean;
begin
  Result := not (A = B);
end;

class operator TFraction.LessThan(const A, B: TFraction): Boolean;
begin
  // On compare les produits croisés pour éviter les erreurs d'arrondi
  Result := A.Numerateur * B.Denominateur < B.Numerateur * A.Denominateur;
end;

class operator TFraction.GreaterThan(const A, B: TFraction): Boolean;
begin
  Result := B < A;
end;

class operator TFraction.Implicit(const Value: Integer): TFraction;
begin
  Result := TFraction.Create(Value, 1);
end;

class operator TFraction.Implicit(const Fraction: TFraction): Double;
begin
  Result := Fraction.Valeur;
end;
```

Utilisation :

```pascal
var
  F1, F2, F3: TFraction;
  D: Double;
begin
  F1 := TFraction.Create(1, 2);  // 1/2
  F2 := TFraction.Create(3, 4);  // 3/4

  // Addition
  F3 := F1 + F2;
  ShowMessage('1/2 + 3/4 = ' + F3.ToString);  // 5/4

  // Soustraction
  F3 := F2 - F1;
  ShowMessage('3/4 - 1/2 = ' + F3.ToString);  // 1/4

  // Multiplication
  F3 := F1 * F2;
  ShowMessage('1/2 * 3/4 = ' + F3.ToString);  // 3/8

  // Division
  F3 := F1 / F2;
  ShowMessage('1/2 / 3/4 = ' + F3.ToString);  // 2/3

  // Comparaison
  if F1 < F2 then
    ShowMessage('1/2 est inférieur à 3/4');

  // Conversion implicite d'entier vers fraction
  F3 := 5;  // Équivalent à TFraction.Create(5, 1)
  ShowMessage('F3 = ' + F3.ToString);  // 5

  // Conversion implicite de fraction vers double
  D := F1;  // Équivalent à F1.Valeur
  ShowMessage('F1 en décimal = ' + FloatToStr(D));  // 0.5

  // Inverse
  F3 := F1.Inverse;
  ShowMessage('Inverse de 1/2 = ' + F3.ToString);  // 2
end;
```

## Différences clés entre records et classes

Pour vous aider à choisir entre records et classes, voici les principales différences :

| Caractéristique | Record | Classe |
|-----------------|--------|--------|
| Allocation | Sur la pile (stack) | Sur le tas (heap) |
| Libération mémoire | Automatique | Manuelle (`Free`) |
| Sémantique | Par valeur (copie) | Par référence (pointeur) |
| Héritage | Non | Oui |
| Polymorphisme | Non | Oui |
| Constructeurs | Optionnels | Obligatoires |
| Destructeurs | Non | Oui |
| Méthodes virtuelles | Non | Oui |
| Surcharge d'opérateurs | Oui | Non |
| Taille maximale recommandée | Petite/Moyenne | Illimitée |

### Quand utiliser un record plutôt qu'une classe ?

- Pour les petites structures de données (quelques champs)
- Quand vous avez besoin de la sémantique de valeur (copies, comparaisons)
- Quand vous voulez éviter la gestion manuelle de la mémoire
- Quand vous avez besoin de surcharger des opérateurs
- Pour des types immuables (qui ne changent pas après création)

### Quand utiliser une classe plutôt qu'un record ?

- Pour les structures complexes avec beaucoup de champs et méthodes
- Quand vous avez besoin d'héritage ou de polymorphisme
- Quand vous avez besoin de méthodes virtuelles
- Pour des cycles de vie longs ou contrôlés

## Bonnes pratiques pour les records avancés

1. **Gardez-les petits** : Les records sont copiés par valeur, donc ils devraient rester relativement petits
2. **Rendez-les immuables** : Lorsque possible, concevez vos records pour qu'ils ne changent pas après création
3. **Utilisez des propriétés** : Préférez les propriétés aux champs publics pour un meilleur encapsulation
4. **Surchargez judicieusement** : Ne surchargez que les opérateurs qui ont un sens pour votre type
5. **Respectez les conventions** : Assurez-vous que vos opérateurs se comportent comme attendu (par exemple, `a + b` devrait être égal à `b + a` pour l'addition)

## Exemple pratique : vecteur 2D

Voici un autre exemple pratique d'un record représentant un vecteur 2D avec des opérateurs surchargés :

```pascal
type
  TVecteur2D = record
  private
    FX, FY: Double;
  public
    constructor Create(X, Y: Double);

    // Méthodes
    function Magnitude: Double;
    function Normaliser: TVecteur2D;
    function ProduitScalaire(const Autre: TVecteur2D): Double;

    // Opérateurs
    class operator Add(const A, B: TVecteur2D): TVecteur2D;
    class operator Subtract(const A, B: TVecteur2D): TVecteur2D;
    class operator Multiply(const Vecteur: TVecteur2D; Scalaire: Double): TVecteur2D;
    class operator Multiply(Scalaire: Double; const Vecteur: TVecteur2D): TVecteur2D;
    class operator Negative(const Vecteur: TVecteur2D): TVecteur2D;
    class operator Equal(const A, B: TVecteur2D): Boolean;

    function ToString: string;

    property X: Double read FX;
    property Y: Double read FY;
  end;

constructor TVecteur2D.Create(X, Y: Double);
begin
  FX := X;
  FY := Y;
end;

function TVecteur2D.Magnitude: Double;
begin
  Result := Sqrt(FX * FX + FY * FY);
end;

function TVecteur2D.Normaliser: TVecteur2D;
var
  Mag: Double;
begin
  Mag := Magnitude;
  if Mag > 0 then
    Result := TVecteur2D.Create(FX / Mag, FY / Mag)
  else
    Result := TVecteur2D.Create(0, 0);
end;

function TVecteur2D.ProduitScalaire(const Autre: TVecteur2D): Double;
begin
  Result := FX * Autre.X + FY * Autre.Y;
end;

class operator TVecteur2D.Add(const A, B: TVecteur2D): TVecteur2D;
begin
  Result := TVecteur2D.Create(A.X + B.X, A.Y + B.Y);
end;

class operator TVecteur2D.Subtract(const A, B: TVecteur2D): TVecteur2D;
begin
  Result := TVecteur2D.Create(A.X - B.X, A.Y - B.Y);
end;

class operator TVecteur2D.Multiply(const Vecteur: TVecteur2D; Scalaire: Double): TVecteur2D;
begin
  Result := TVecteur2D.Create(Vecteur.X * Scalaire, Vecteur.Y * Scalaire);
end;

class operator TVecteur2D.Multiply(Scalaire: Double; const Vecteur: TVecteur2D): TVecteur2D;
begin
  Result := Vecteur * Scalaire;  // Réutilise l'autre opérateur Multiply
end;

class operator TVecteur2D.Negative(const Vecteur: TVecteur2D): TVecteur2D;
begin
  Result := TVecteur2D.Create(-Vecteur.X, -Vecteur.Y);
end;

class operator TVecteur2D.Equal(const A, B: TVecteur2D): Boolean;
const
  Epsilon = 1E-10;
begin
  Result := (Abs(A.X - B.X) < Epsilon) and (Abs(A.Y - B.Y) < Epsilon);
end;

function TVecteur2D.ToString: string;
begin
  Result := Format('(%.2f, %.2f)', [FX, FY]);
end;
```

Utilisation :

```pascal
var
  V1, V2, V3: TVecteur2D;
  ProduitScalaire: Double;
begin
  V1 := TVecteur2D.Create(3, 4);
  V2 := TVecteur2D.Create(1, 2);

  // Addition
  V3 := V1 + V2;
  ShowMessage('V1 + V2 = ' + V3.ToString);  // (4.00, 6.00)

  // Soustraction
  V3 := V1 - V2;
  ShowMessage('V1 - V2 = ' + V3.ToString);  // (2.00, 2.00)

  // Multiplication par un scalaire
  V3 := V1 * 2;
  ShowMessage('V1 * 2 = ' + V3.ToString);  // (6.00, 8.00)

  // L'opérateur fonctionne dans les deux sens
  V3 := 0.5 * V1;
  ShowMessage('0.5 * V1 = ' + V3.ToString);  // (1.50, 2.00)

  // Négation
  V3 := -V1;
  ShowMessage('-V1 = ' + V3.ToString);  // (-3.00, -4.00)

  // Magnitude
  ShowMessage('Magnitude de V1 = ' + FloatToStr(V1.Magnitude));  // 5.00

  // Normalisation
  V3 := V1.Normaliser;
  ShowMessage('V1 normalisé = ' + V3.ToString);  // (0.60, 0.80)

  // Produit scalaire
  ProduitScalaire := V1.ProduitScalaire(V2);
  ShowMessage('V1 · V2 = ' + FloatToStr(ProduitScalaire));  // 11.00
end;
```

## Conclusion

Les records avancés et les opérateurs surchargés sont des fonctionnalités puissantes d'Object Pascal qui vous permettent de créer des types de données personnalisés, intuitifs et faciles à utiliser. Utilisés correctement, ils peuvent rendre votre code plus propre, plus lisible et plus maintenable.

Les records sont particulièrement utiles pour représenter des valeurs immuables comme des points, des vecteurs, des fractions, des coordonnées GPS, des dates, etc. En surchargeant les opérateurs, vous pouvez donner à ces types un comportement naturel et cohérent.

Dans la prochaine section, nous explorerons d'autres aspects avancés du langage Object Pascal pour vous aider à écrire des applications encore plus puissantes et élégantes.

⏭️ [Conception d'Interfaces Utilisateur avec la VCL](/04-conception-dinterfaces-utilisateur-avec-la-vcl/README.md)
