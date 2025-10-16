🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.11 Records avancés et opérateurs surchargés

## Introduction

Les **records** (enregistrements) sont des structures de données qui regroupent plusieurs valeurs de types différents. Contrairement aux classes, les records sont des **types valeur** stockés sur la pile (stack), ce qui les rend plus rapides et légers.

Depuis Delphi 2006, les records ont été considérablement améliorés pour ressembler davantage aux classes, tout en conservant leurs avantages de performance.

## Records simples (rappel de base)

### Qu'est-ce qu'un record ?

Un **record** est une structure qui regroupe plusieurs champs de types différents.

```pascal
type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

var
  P: TPoint;
begin
  P.X := 10;
  P.Y := 20;
  ShowMessage(Format('Point : (%d, %d)', [P.X, P.Y]));
end;
```

### Différences entre record et classe

| Caractéristique | Record | Classe |
|----------------|--------|--------|
| **Type** | Type valeur | Type référence |
| **Stockage** | Pile (stack) | Tas (heap) |
| **Création** | Automatique | Avec Create |
| **Libération** | Automatique | Avec Free |
| **Héritage** | Non | Oui |
| **Interfaces** | Oui (depuis Delphi 2009) | Oui |
| **Performance** | Plus rapide | Légèrement plus lent |
| **Copie** | Copie de valeur | Copie de référence |

### Quand utiliser un record ?

**✅ Utilisez un record pour** :
- Structures de données simples et légères
- Types de données mathématiques (Point, Rectangle, Complexe)
- Données temporaires qui ne nécessitent pas d'héritage
- Optimisation de la performance (moins d'allocations mémoire)

**❌ Utilisez une classe pour** :
- Structures complexes avec héritage
- Gestion de ressources (fichiers, connexions)
- Comportements polymorphes
- Objets à durée de vie longue

## Records avec constructeurs

### Déclaration d'un constructeur

Un record peut avoir un constructeur pour initialiser ses champs :

```pascal
type
  TPoint = record
    X, Y: Integer;
    constructor Create(AX, AY: Integer);
  end;

constructor TPoint.Create(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;
```

### Utilisation

```pascal
var
  P1, P2: TPoint;
begin
  // Avec constructeur
  P1 := TPoint.Create(10, 20);

  // Sans constructeur (initialisation manuelle)
  P2.X := 30;
  P2.Y := 40;

  ShowMessage(Format('P1 : (%d, %d)', [P1.X, P1.Y]));
  ShowMessage(Format('P2 : (%d, %d)', [P2.X, P2.Y]));
end;
```

### Plusieurs constructeurs (surcharge)

```pascal
type
  TRectangle = record
    Gauche, Haut, Droite, Bas: Integer;

    constructor Create(AGauche, AHaut, ADroite, ABas: Integer); overload;
    constructor Create(ALargeur, AHauteur: Integer); overload;
    constructor CreateCentre(ACentreX, ACentreY, ALargeur, AHauteur: Integer);
  end;

constructor TRectangle.Create(AGauche, AHaut, ADroite, ABas: Integer);
begin
  Gauche := AGauche;
  Haut := AHaut;
  Droite := ADroite;
  Bas := ABas;
end;

constructor TRectangle.Create(ALargeur, AHauteur: Integer);
begin
  Gauche := 0;
  Haut := 0;
  Droite := ALargeur;
  Bas := AHauteur;
end;

constructor TRectangle.CreateCentre(ACentreX, ACentreY, ALargeur, AHauteur: Integer);
begin
  Gauche := ACentreX - (ALargeur div 2);
  Haut := ACentreY - (AHauteur div 2);
  Droite := ACentreX + (ALargeur div 2);
  Bas := ACentreY + (AHauteur div 2);
end;
```

### Utilisation des différents constructeurs

```pascal
var
  R1, R2, R3: TRectangle;
begin
  R1 := TRectangle.Create(0, 0, 100, 50);
  R2 := TRectangle.Create(100, 50);
  R3 := TRectangle.CreateCentre(50, 25, 100, 50);
end;
```

## Records avec méthodes

### Ajouter des méthodes à un record

Les records peuvent contenir des méthodes comme les classes :

```pascal
type
  TPoint = record
    X, Y: Integer;

    constructor Create(AX, AY: Integer);

    // Méthodes
    function Distance(Autre: TPoint): Double;
    function EstOrigine: Boolean;
    procedure Deplacer(DeltaX, DeltaY: Integer);
    procedure DeplacerVers(Nouveau: TPoint);
    function ToString: string;
    procedure Afficher;
  end;

constructor TPoint.Create(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

function TPoint.Distance(Autre: TPoint): Double;
begin
  Result := Sqrt(Sqr(Autre.X - X) + Sqr(Autre.Y - Y));
end;

function TPoint.EstOrigine: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

procedure TPoint.Deplacer(DeltaX, DeltaY: Integer);
begin
  X := X + DeltaX;
  Y := Y + DeltaY;
end;

procedure TPoint.DeplacerVers(Nouveau: TPoint);
begin
  X := Nouveau.X;
  Y := Nouveau.Y;
end;

function TPoint.ToString: string;
begin
  Result := Format('(%d, %d)', [X, Y]);
end;

procedure TPoint.Afficher;
begin
  ShowMessage(ToString);
end;
```

### Utilisation

```pascal
var
  P1, P2: TPoint;
  Dist: Double;
begin
  P1 := TPoint.Create(10, 20);
  P2 := TPoint.Create(30, 40);

  P1.Afficher;  // Affiche "(10, 20)"

  if P1.EstOrigine then
    ShowMessage('P1 est à l''origine')
  else
    ShowMessage('P1 n''est pas à l''origine');

  P1.Deplacer(5, 5);
  P1.Afficher;  // Affiche "(15, 25)"

  Dist := P1.Distance(P2);
  ShowMessage(Format('Distance : %.2f', [Dist]));
end;
```

## Méthodes de classe (class methods)

Les records peuvent avoir des méthodes de classe (static) :

```pascal
type
  TPoint = record
    X, Y: Integer;

    constructor Create(AX, AY: Integer);

    // Méthodes de classe
    class function Origine: TPoint; static;
    class function Distance(P1, P2: TPoint): Double; static;
    class function Milieu(P1, P2: TPoint): TPoint; static;
  end;

class function TPoint.Origine: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TPoint.Distance(P1, P2: TPoint): Double;
begin
  Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
end;

class function TPoint.Milieu(P1, P2: TPoint): TPoint;
begin
  Result.X := (P1.X + P2.X) div 2;
  Result.Y := (P1.Y + P2.Y) div 2;
end;
```

### Utilisation

```pascal
var
  P1, P2, PMilieu: TPoint;
  Dist: Double;
begin
  // Créer un point à l'origine
  P1 := TPoint.Origine;

  P2 := TPoint.Create(100, 50);

  // Calculer la distance entre deux points
  Dist := TPoint.Distance(P1, P2);
  ShowMessage(Format('Distance : %.2f', [Dist]));

  // Trouver le point milieu
  PMilieu := TPoint.Milieu(P1, P2);
  ShowMessage(Format('Milieu : (%d, %d)', [PMilieu.X, PMilieu.Y]));
end;
```

## Opérateurs surchargés

### Qu'est-ce qu'un opérateur surchargé ?

La **surcharge d'opérateurs** permet de définir comment les opérateurs mathématiques (+, -, *, /, etc.) et de comparaison (=, <, >, etc.) fonctionnent avec vos types personnalisés.

### Syntaxe générale

```pascal
type
  TMonType = record
    // Champs...

    class operator NomOperateur(Paramètres): TypeRetour;
  end;
```

## Opérateurs arithmétiques

### Addition (+)

```pascal
type
  TPoint = record
    X, Y: Integer;

    constructor Create(AX, AY: Integer);

    // Opérateur d'addition
    class operator Add(const A, B: TPoint): TPoint;
  end;

class operator TPoint.Add(const A, B: TPoint): TPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;
```

### Utilisation

```pascal
var
  P1, P2, P3: TPoint;
begin
  P1 := TPoint.Create(10, 20);
  P2 := TPoint.Create(5, 15);

  // Utilisation de l'opérateur +
  P3 := P1 + P2;  // P3 = (15, 35)

  ShowMessage(Format('P3 : (%d, %d)', [P3.X, P3.Y]));
end;
```

### Soustraction (-)

```pascal
type
  TPoint = record
    X, Y: Integer;

    class operator Subtract(const A, B: TPoint): TPoint;
  end;

class operator TPoint.Subtract(const A, B: TPoint): TPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;
```

### Multiplication (*) et Division (/)

```pascal
type
  TPoint = record
    X, Y: Integer;

    // Multiplication par un scalaire
    class operator Multiply(const Point: TPoint; Scalaire: Integer): TPoint;
    class operator Multiply(Scalaire: Integer; const Point: TPoint): TPoint;

    // Division par un scalaire
    class operator Divide(const Point: TPoint; Diviseur: Integer): TPoint;
  end;

class operator TPoint.Multiply(const Point: TPoint; Scalaire: Integer): TPoint;
begin
  Result.X := Point.X * Scalaire;
  Result.Y := Point.Y * Scalaire;
end;

class operator TPoint.Multiply(Scalaire: Integer; const Point: TPoint): TPoint;
begin
  Result := Multiply(Point, Scalaire);
end;

class operator TPoint.Divide(const Point: TPoint; Diviseur: Integer): TPoint;
begin
  if Diviseur = 0 then
    raise Exception.Create('Division par zéro');
  Result.X := Point.X div Diviseur;
  Result.Y := Point.Y div Diviseur;
end;
```

### Utilisation

```pascal
var
  P1, P2, P3, P4: TPoint;
begin
  P1 := TPoint.Create(10, 20);

  P2 := P1 * 2;      // (20, 40)
  P3 := 3 * P1;      // (30, 60)
  P4 := P1 / 2;      // (5, 10)
end;
```

## Opérateurs unaires

### Négation (-)

```pascal
type
  TPoint = record
    X, Y: Integer;

    class operator Negative(const Point: TPoint): TPoint;
  end;

class operator TPoint.Negative(const Point: TPoint): TPoint;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;
```

### Utilisation

```pascal
var
  P1, P2: TPoint;
begin
  P1 := TPoint.Create(10, 20);
  P2 := -P1;  // P2 = (-10, -20)
end;
```

## Opérateurs de comparaison

### Égalité (=) et Inégalité (<>)

```pascal
type
  TPoint = record
    X, Y: Integer;

    class operator Equal(const A, B: TPoint): Boolean;
    class operator NotEqual(const A, B: TPoint): Boolean;
  end;

class operator TPoint.Equal(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TPoint.NotEqual(const A, B: TPoint): Boolean;
begin
  Result := not (A = B);
end;
```

### Comparaisons (<, >, <=, >=)

```pascal
type
  TPoint = record
    X, Y: Integer;

    // Comparaison basée sur la distance à l'origine
    class operator LessThan(const A, B: TPoint): Boolean;
    class operator GreaterThan(const A, B: TPoint): Boolean;
    class operator LessThanOrEqual(const A, B: TPoint): Boolean;
    class operator GreaterThanOrEqual(const A, B: TPoint): Boolean;
  private
    function DistanceOrigine: Double;
  end;

function TPoint.DistanceOrigine: Double;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

class operator TPoint.LessThan(const A, B: TPoint): Boolean;
begin
  Result := A.DistanceOrigine < B.DistanceOrigine;
end;

class operator TPoint.GreaterThan(const A, B: TPoint): Boolean;
begin
  Result := A.DistanceOrigine > B.DistanceOrigine;
end;

class operator TPoint.LessThanOrEqual(const A, B: TPoint): Boolean;
begin
  Result := A.DistanceOrigine <= B.DistanceOrigine;
end;

class operator TPoint.GreaterThanOrEqual(const A, B: TPoint): Boolean;
begin
  Result := A.DistanceOrigine >= B.DistanceOrigine;
end;
```

### Utilisation

```pascal
var
  P1, P2: TPoint;
begin
  P1 := TPoint.Create(10, 20);
  P2 := TPoint.Create(5, 10);

  if P1 = P2 then
    ShowMessage('Points égaux')
  else
    ShowMessage('Points différents');

  if P1 > P2 then
    ShowMessage('P1 est plus loin de l''origine que P2');
end;
```

## Opérateurs de conversion (cast)

### Conversion implicite

```pascal
type
  TPoint = record
    X, Y: Integer;

    // Conversion implicite depuis un entier (point sur l'axe X)
    class operator Implicit(Valeur: Integer): TPoint;

    // Conversion implicite vers une chaîne
    class operator Implicit(const Point: TPoint): string;
  end;

class operator TPoint.Implicit(Valeur: Integer): TPoint;
begin
  Result.X := Valeur;
  Result.Y := 0;
end;

class operator TPoint.Implicit(const Point: TPoint): string;
begin
  Result := Format('(%d, %d)', [Point.X, Point.Y]);
end;
```

### Utilisation

```pascal
var
  P: TPoint;
  S: string;
begin
  // Conversion implicite depuis Integer
  P := 10;  // P = (10, 0)

  // Conversion implicite vers string
  S := P;  // S = "(10, 0)"
  ShowMessage(S);
end;
```

### Conversion explicite

```pascal
type
  TPoint = record
    X, Y: Integer;

    // Conversion explicite vers Double (distance à l'origine)
    class operator Explicit(const Point: TPoint): Double;
  end;

class operator TPoint.Explicit(const Point: TPoint): Double;
begin
  Result := Sqrt(Sqr(Point.X) + Sqr(Point.Y));
end;
```

### Utilisation

```pascal
var
  P: TPoint;
  Distance: Double;
begin
  P := TPoint.Create(3, 4);

  // Conversion explicite avec cast
  Distance := Double(P);  // Distance = 5.0

  ShowMessage(FloatToStr(Distance));
end;
```

## Exemple complet : Nombre complexe

Voici un exemple complet d'un record avec tous les types d'opérateurs :

```pascal
type
  TComplexe = record
    Reel: Double;
    Imaginaire: Double;

    // Constructeurs
    constructor Create(AReel, AImagianire: Double); overload;
    constructor Create(AReel: Double); overload;

    // Méthodes
    function Module: Double;
    function Conjugue: TComplexe;
    function ToString: string;

    // Méthodes de classe
    class function Zero: TComplexe; static;
    class function Un: TComplexe; static;
    class function I: TComplexe; static;

    // Opérateurs arithmétiques
    class operator Add(const A, B: TComplexe): TComplexe;
    class operator Subtract(const A, B: TComplexe): TComplexe;
    class operator Multiply(const A, B: TComplexe): TComplexe;
    class operator Divide(const A, B: TComplexe): TComplexe;

    // Opérateurs unaires
    class operator Negative(const C: TComplexe): TComplexe;

    // Opérateurs de comparaison
    class operator Equal(const A, B: TComplexe): Boolean;
    class operator NotEqual(const A, B: TComplexe): Boolean;

    // Opérateurs de conversion
    class operator Implicit(Valeur: Double): TComplexe;
    class operator Implicit(const C: TComplexe): string;
    class operator Explicit(const C: TComplexe): Double;
  end;

// Constructeurs
constructor TComplexe.Create(AReel, AImagianire: Double);
begin
  Reel := AReel;
  Imaginaire := AImagianire;
end;

constructor TComplexe.Create(AReel: Double);
begin
  Create(AReel, 0);
end;

// Méthodes
function TComplexe.Module: Double;
begin
  Result := Sqrt(Sqr(Reel) + Sqr(Imaginaire));
end;

function TComplexe.Conjugue: TComplexe;
begin
  Result.Reel := Reel;
  Result.Imaginaire := -Imaginaire;
end;

function TComplexe.ToString: string;
begin
  if Imaginaire >= 0 then
    Result := Format('%.2f + %.2fi', [Reel, Imaginaire])
  else
    Result := Format('%.2f - %.2fi', [Reel, Abs(Imaginaire)]);
end;

// Méthodes de classe
class function TComplexe.Zero: TComplexe;
begin
  Result := TComplexe.Create(0, 0);
end;

class function TComplexe.Un: TComplexe;
begin
  Result := TComplexe.Create(1, 0);
end;

class function TComplexe.I: TComplexe;
begin
  Result := TComplexe.Create(0, 1);
end;

// Opérateurs arithmétiques
class operator TComplexe.Add(const A, B: TComplexe): TComplexe;
begin
  Result.Reel := A.Reel + B.Reel;
  Result.Imaginaire := A.Imaginaire + B.Imaginaire;
end;

class operator TComplexe.Subtract(const A, B: TComplexe): TComplexe;
begin
  Result.Reel := A.Reel - B.Reel;
  Result.Imaginaire := A.Imaginaire - B.Imaginaire;
end;

class operator TComplexe.Multiply(const A, B: TComplexe): TComplexe;
begin
  // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
  Result.Reel := A.Reel * B.Reel - A.Imaginaire * B.Imaginaire;
  Result.Imaginaire := A.Reel * B.Imaginaire + A.Imaginaire * B.Reel;
end;

class operator TComplexe.Divide(const A, B: TComplexe): TComplexe;
var
  Denominateur: Double;
begin
  Denominateur := Sqr(B.Reel) + Sqr(B.Imaginaire);
  if Denominateur = 0 then
    raise Exception.Create('Division par zéro');

  Result.Reel := (A.Reel * B.Reel + A.Imaginaire * B.Imaginaire) / Denominateur;
  Result.Imaginaire := (A.Imaginaire * B.Reel - A.Reel * B.Imaginaire) / Denominateur;
end;

class operator TComplexe.Negative(const C: TComplexe): TComplexe;
begin
  Result.Reel := -C.Reel;
  Result.Imaginaire := -C.Imaginaire;
end;

// Opérateurs de comparaison
class operator TComplexe.Equal(const A, B: TComplexe): Boolean;
begin
  Result := (Abs(A.Reel - B.Reel) < 0.0001) and
            (Abs(A.Imaginaire - B.Imaginaire) < 0.0001);
end;

class operator TComplexe.NotEqual(const A, B: TComplexe): Boolean;
begin
  Result := not (A = B);
end;

// Opérateurs de conversion
class operator TComplexe.Implicit(Valeur: Double): TComplexe;
begin
  Result := TComplexe.Create(Valeur, 0);
end;

class operator TComplexe.Implicit(const C: TComplexe): string;
begin
  Result := C.ToString;
end;

class operator TComplexe.Explicit(const C: TComplexe): Double;
begin
  Result := C.Module;
end;
```

### Utilisation du nombre complexe

```pascal
var
  C1, C2, C3: TComplexe;
  Texte: string;
  Module: Double;
begin
  // Création
  C1 := TComplexe.Create(3, 4);
  C2 := TComplexe.Create(1, 2);

  // Addition
  C3 := C1 + C2;  // (4 + 6i)
  ShowMessage('C1 + C2 = ' + string(C3));

  // Soustraction
  C3 := C1 - C2;  // (2 + 2i)
  ShowMessage('C1 - C2 = ' + string(C3));

  // Multiplication
  C3 := C1 * C2;  // (-5 + 10i)
  ShowMessage('C1 * C2 = ' + string(C3));

  // Division
  C3 := C1 / C2;
  ShowMessage('C1 / C2 = ' + string(C3));

  // Négation
  C3 := -C1;  // (-3 - 4i)
  ShowMessage('-C1 = ' + string(C3));

  // Conversion depuis Double
  C3 := 5.0;  // (5 + 0i)

  // Conversion vers string
  Texte := C1;  // "3.00 + 4.00i"
  ShowMessage(Texte);

  // Conversion explicite vers Double (module)
  Module := Double(C1);  // 5.0
  ShowMessage('Module de C1 = ' + FloatToStr(Module));

  // Méthodes de classe
  ShowMessage('Zero = ' + string(TComplexe.Zero));
  ShowMessage('Un = ' + string(TComplexe.Un));
  ShowMessage('I = ' + string(TComplexe.I));

  // Comparaison
  if C1 = C2 then
    ShowMessage('Égaux')
  else
    ShowMessage('Différents');
end;
```

## Exemple pratique : Vecteur 2D

```pascal
type
  TVecteur2D = record
    X, Y: Double;

    constructor Create(AX, AY: Double);

    // Méthodes
    function Longueur: Double;
    function Normaliser: TVecteur2D;
    function ProduitScalaire(Autre: TVecteur2D): Double;
    function Angle: Double;
    function ToString: string;

    // Méthodes de classe
    class function Zero: TVecteur2D; static;
    class function Unitaire(Angle: Double): TVecteur2D; static;

    // Opérateurs arithmétiques
    class operator Add(const A, B: TVecteur2D): TVecteur2D;
    class operator Subtract(const A, B: TVecteur2D): TVecteur2D;
    class operator Multiply(const V: TVecteur2D; Scalaire: Double): TVecteur2D;
    class operator Multiply(Scalaire: Double; const V: TVecteur2D): TVecteur2D;
    class operator Divide(const V: TVecteur2D; Diviseur: Double): TVecteur2D;

    // Opérateur unaire
    class operator Negative(const V: TVecteur2D): TVecteur2D;

    // Opérateurs de comparaison
    class operator Equal(const A, B: TVecteur2D): Boolean;
    class operator NotEqual(const A, B: TVecteur2D): Boolean;
  end;

constructor TVecteur2D.Create(AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

function TVecteur2D.Longueur: Double;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

function TVecteur2D.Normaliser: TVecteur2D;
var
  Len: Double;
begin
  Len := Longueur;
  if Len = 0 then
    Result := Self
  else
  begin
    Result.X := X / Len;
    Result.Y := Y / Len;
  end;
end;

function TVecteur2D.ProduitScalaire(Autre: TVecteur2D): Double;
begin
  Result := X * Autre.X + Y * Autre.Y;
end;

function TVecteur2D.Angle: Double;
begin
  Result := ArcTan2(Y, X);
end;

function TVecteur2D.ToString: string;
begin
  Result := Format('(%.2f, %.2f)', [X, Y]);
end;

class function TVecteur2D.Zero: TVecteur2D;
begin
  Result := TVecteur2D.Create(0, 0);
end;

class function TVecteur2D.Unitaire(Angle: Double): TVecteur2D;
begin
  Result.X := Cos(Angle);
  Result.Y := Sin(Angle);
end;

class operator TVecteur2D.Add(const A, B: TVecteur2D): TVecteur2D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVecteur2D.Subtract(const A, B: TVecteur2D): TVecteur2D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVecteur2D.Multiply(const V: TVecteur2D; Scalaire: Double): TVecteur2D;
begin
  Result.X := V.X * Scalaire;
  Result.Y := V.Y * Scalaire;
end;

class operator TVecteur2D.Multiply(Scalaire: Double; const V: TVecteur2D): TVecteur2D;
begin
  Result := V * Scalaire;
end;

class operator TVecteur2D.Divide(const V: TVecteur2D; Diviseur: Double): TVecteur2D;
begin
  if Diviseur = 0 then
    raise Exception.Create('Division par zéro');
  Result.X := V.X / Diviseur;
  Result.Y := V.Y / Diviseur;
end;

class operator TVecteur2D.Negative(const V: TVecteur2D): TVecteur2D;
begin
  Result.X := -V.X;
  Result.Y := -V.Y;
end;

class operator TVecteur2D.Equal(const A, B: TVecteur2D): Boolean;
begin
  Result := (Abs(A.X - B.X) < 0.0001) and (Abs(A.Y - B.Y) < 0.0001);
end;

class operator TVecteur2D.NotEqual(const A, B: TVecteur2D): Boolean;
begin
  Result := not (A = B);
end;
```

### Utilisation

```pascal
var
  V1, V2, V3: TVecteur2D;
begin
  V1 := TVecteur2D.Create(3, 4);
  V2 := TVecteur2D.Create(1, 2);

  // Opérations vectorielles
  V3 := V1 + V2;          // Addition
  V3 := V1 - V2;          // Soustraction
  V3 := V1 * 2;           // Multiplication par scalaire
  V3 := V1 / 2;           // Division par scalaire
  V3 := -V1;              // Négation

  // Méthodes
  ShowMessage('Longueur V1 : ' + FloatToStr(V1.Longueur));
  ShowMessage('Produit scalaire : ' + FloatToStr(V1.ProduitScalaire(V2)));

  V3 := V1.Normaliser;
  ShowMessage('V1 normalisé : ' + V3.ToString);
end;
```

## Liste des opérateurs disponibles

| Opérateur | Méthode | Usage |
|-----------|---------|-------|
| + | Add | A + B |
| - (binaire) | Subtract | A - B |
| * | Multiply | A * B |
| / | Divide | A / B |
| - (unaire) | Negative | -A |
| = | Equal | A = B |
| <> | NotEqual | A <> B |
| < | LessThan | A < B |
| > | GreaterThan | A > B |
| <= | LessThanOrEqual | A <= B |
| >= | GreaterThanOrEqual | A >= B |
| Implicit | Implicit | A := B (conversion auto) |
| Explicit | Explicit | A := TypeA(B) (cast) |
| Inc | Inc | Inc(A) |
| Dec | Dec | Dec(A) |
| LogicalAnd | LogicalAnd | A and B |
| LogicalOr | LogicalOr | A or B |
| LogicalNot | LogicalNot | not A |
| BitwiseAnd | BitwiseAnd | A and B |
| BitwiseOr | BitwiseOr | A or B |

## Bonnes pratiques

### 1. Utilisez const pour les paramètres d'opérateurs

```pascal
// ✅ Bon - évite la copie inutile
class operator Add(const A, B: TPoint): TPoint;

// ❌ Moins bon - copie les paramètres
class operator Add(A, B: TPoint): TPoint;
```

### 2. Soyez cohérent avec les mathématiques

```pascal
// ✅ Bon - comportement attendu
class operator Add(const A, B: TPoint): TPoint;

// ❌ Confusant - + qui soustrait !
class operator Add(const A, B: TPoint): TPoint;
begin
  Result.X := A.X - B.X;  // Non !
end;
```

### 3. Validez les entrées

```pascal
class operator Divide(const A: TPoint; Diviseur: Integer): TPoint;
begin
  if Diviseur = 0 then
    raise Exception.Create('Division par zéro');
  Result.X := A.X div Diviseur;
  Result.Y := A.Y div Diviseur;
end;
```

### 4. Implémentez les opérateurs par paires

Si vous implémentez `=`, implémentez aussi `<>`.
Si vous implémentez `<`, implémentez aussi `>`, `<=`, `>=`.

### 5. Utilisez les records pour les types valeur

```pascal
// ✅ Bon - type valeur léger
type
  TPoint = record
    X, Y: Integer;
  end;

// ❌ Moins approprié - overhead de classe inutile
type
  TPoint = class
    X, Y: Integer;
  end;
```

## Limites des records

### Pas d'héritage

```pascal
// ❌ ERREUR - les records ne supportent pas l'héritage
type
  TPoint2D = record
    X, Y: Integer;
  end;

  TPoint3D = record(TPoint2D)  // Erreur de compilation !
    Z: Integer;
  end;
```

### Solution : composition

```pascal
// ✅ Solution - utiliser la composition
type
  TPoint2D = record
    X, Y: Integer;
  end;

  TPoint3D = record
    Point2D: TPoint2D;  // Composition
    Z: Integer;
  end;
```

### Attention aux références

```pascal
var
  P1, P2: TPoint;
begin
  P1 := TPoint.Create(10, 20);
  P2 := P1;  // Copie de valeur, pas de référence

  P2.X := 99;

  // P1.X vaut toujours 10 (pas 99)
  // P2.X vaut 99
end;
```

## Résumé

- **Records avancés** : structures légères avec méthodes
  - Constructeurs pour initialisation
  - Méthodes d'instance et de classe
  - Plus performants que les classes pour les petites structures

- **Opérateurs surchargés** : définir le comportement des opérateurs
  - **Arithmétiques** : +, -, *, /
  - **Unaires** : - (négation)
  - **Comparaison** : =, <>, <, >, <=, >=
  - **Conversion** : Implicit, Explicit

- **Avantages** :
  - Code plus naturel et lisible
  - Performance optimale (type valeur)
  - Pas besoin de Create/Free
  - Parfait pour types mathématiques

- **Bonnes pratiques** :
  - Paramètres const pour éviter les copies
  - Cohérence mathématique
  - Validation des entrées
  - Implémenter les opérateurs par paires

- **Limitations** :
  - Pas d'héritage
  - Copie de valeur (pas de référence)
  - Utiliser composition au lieu d'héritage

Les records avancés avec opérateurs surchargés sont parfaits pour créer des types de données mathématiques élégants et performants comme des points, vecteurs, matrices, nombres complexes, fractions, etc.

⏭️ [Conception d'Interfaces Utilisateur avec la VCL](/04-conception-dinterfaces-utilisateur-avec-la-vcl/README.md)
