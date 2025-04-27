# 3.7 Programmation orientée objet

La programmation orientée objet (POO) est un paradigme de programmation qui permet d'organiser le code de manière plus structurée et modulaire. Object Pascal, le langage utilisé dans Delphi, est un langage orienté objet très puissant. Cette section vous présente les concepts fondamentaux de la POO dans Delphi.

## Qu'est-ce que la programmation orientée objet ?

La programmation orientée objet est une approche qui permet de modéliser des problèmes en termes d'"objets", qui sont des entités combinant des données (attributs) et des comportements (méthodes). Cette approche présente plusieurs avantages par rapport à la programmation procédurale traditionnelle :

- **Modularité** : Le code est organisé en unités indépendantes (objets)
- **Réutilisabilité** : Les objets peuvent être réutilisés dans différentes parties du programme ou même dans d'autres projets
- **Extensibilité** : Vous pouvez créer de nouveaux objets basés sur des objets existants
- **Maintenance** : Le code est généralement plus facile à maintenir et à faire évoluer

## Concepts clés de la POO

Avant de plonger dans les détails, voici les concepts fondamentaux que vous devez comprendre :

### Objets et classes

- Une **classe** est un modèle qui définit les caractéristiques et comportements d'un type d'objet.
- Un **objet** est une instance concrète d'une classe.

Pour faire une analogie, une classe est comme un plan d'architecte pour une maison, tandis qu'un objet est une maison réelle construite selon ce plan. Vous pouvez créer plusieurs maisons (objets) à partir d'un même plan (classe).

### Attributs et méthodes

- Les **attributs** (ou champs) sont les données stockées dans un objet.
- Les **méthodes** sont les fonctions et procédures qui définissent le comportement d'un objet.

Par exemple, une classe `TVoiture` pourrait avoir des attributs comme `Couleur`, `Marque` et `VitesseMaximale`, et des méthodes comme `Demarrer()`, `Accelerer()` et `Freiner()`.

### Encapsulation

L'**encapsulation** est le principe qui consiste à regrouper les données et les méthodes au sein d'une classe, tout en cachant les détails internes. Cela se fait généralement en utilisant des niveaux de visibilité (`private`, `protected`, `public`, etc.).

L'encapsulation permet de :
- Protéger les données contre les modifications accidentelles
- Cacher la complexité interne
- Présenter une interface simple et claire pour utiliser l'objet

### Héritage

L'**héritage** permet de créer une nouvelle classe (classe dérivée) à partir d'une classe existante (classe de base). La classe dérivée hérite de tous les attributs et méthodes de la classe de base, et peut en ajouter de nouveaux ou modifier ceux existants.

Par exemple, à partir d'une classe `TVehicule`, vous pourriez créer des classes dérivées comme `TVoiture`, `TMoto` et `TCamion`, qui héritent des caractéristiques communes tout en ajoutant leurs spécificités.

### Polymorphisme

Le **polymorphisme** permet à des objets de classes différentes d'être traités comme des objets d'une classe commune. Cela se fait généralement via des méthodes virtuelles qui peuvent être redéfinies dans les classes dérivées.

Par exemple, différents types de véhicules peuvent avoir une méthode `Avancer()`, mais chacun l'implémente différemment.

### Abstraction

L'**abstraction** consiste à se concentrer sur les caractéristiques essentielles d'un objet tout en ignorant les détails moins importants. Cela permet de créer des modèles simplifiés mais fonctionnels de concepts complexes.

## Pourquoi utiliser la POO en Delphi ?

Delphi est construit autour de la POO. En fait, tous les composants visuels que vous placez sur un formulaire sont des objets :
- Un bouton (`TButton`) est un objet
- Un champ texte (`TEdit`) est un objet
- Le formulaire lui-même (`TForm`) est un objet

En comprenant et en utilisant la POO, vous pourrez :
- Créer vos propres composants réutilisables
- Étendre les fonctionnalités des composants existants
- Organiser votre code de manière plus logique et maintenable
- Profiter pleinement de la puissance de Delphi

## Différence avec la programmation procédurale

Jusqu'à présent dans ce tutoriel, nous avons principalement utilisé la programmation procédurale, qui organise le code en procédures et fonctions. Voici les principales différences entre ces deux approches :

| Programmation procédurale | Programmation orientée objet |
|---------------------------|------------------------------|
| Se concentre sur les procédures | Se concentre sur les objets |
| Les données et les fonctions sont séparées | Les données et les méthodes sont regroupées dans des classes |
| Approche descendante (top-down) | Approche basée sur les objets et leurs interactions |
| Plus simple pour les petits programmes | Plus adaptée aux grands projets complexes |

En Delphi, vous pouvez utiliser les deux approches, mais la POO est particulièrement avantageuse pour les projets d'envergure.

## Quand utiliser la POO ?

La POO n'est pas toujours nécessaire pour tous les aspects d'un programme. Voici quelques situations où elle est particulièrement utile :

- Quand vous modélisez des entités du monde réel (clients, produits, commandes...)
- Quand vous créez des composants graphiques personnalisés
- Quand vous avez besoin de réutiliser du code dans différents contextes
- Quand vous travaillez sur un grand projet avec plusieurs développeurs
- Quand vous voulez créer une architecture maintenable à long terme

## Exemple simple : de procédural à orienté objet

Pour illustrer la différence entre les deux approches, prenons l'exemple d'une calculatrice simple.

**Approche procédurale :**

```pascal
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function Soustraire(A, B: Integer): Integer;
begin
  Result := A - B;
end;

procedure UtiliserCalculatrice;
var
  X, Y, Resultat: Integer;
begin
  X := 10;
  Y := 5;

  Resultat := Additionner(X, Y);
  ShowMessage('Addition : ' + IntToStr(Resultat));

  Resultat := Soustraire(X, Y);
  ShowMessage('Soustraction : ' + IntToStr(Resultat));
end;
```

**Approche orientée objet :**

```pascal
type
  TCalculatrice = class
  private
    FDernierResultat: Integer;
  public
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
    property DernierResultat: Integer read FDernierResultat;
  end;

function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  FDernierResultat := A + B;
  Result := FDernierResultat;
end;

function TCalculatrice.Soustraire(A, B: Integer): Integer;
begin
  FDernierResultat := A - B;
  Result := FDernierResultat;
end;

procedure UtiliserCalculatrice;
var
  Calc: TCalculatrice;
  X, Y: Integer;
begin
  Calc := TCalculatrice.Create;
  try
    X := 10;
    Y := 5;

    ShowMessage('Addition : ' + IntToStr(Calc.Additionner(X, Y)));
    ShowMessage('Soustraction : ' + IntToStr(Calc.Soustraire(X, Y)));
    ShowMessage('Dernier résultat : ' + IntToStr(Calc.DernierResultat));
  finally
    Calc.Free;  // Libération de la mémoire
  end;
end;
```

Dans l'approche orientée objet :
- Nous avons une classe `TCalculatrice` qui regroupe les opérations et les données
- La classe maintient un état interne (le dernier résultat)
- Nous créons un objet `Calc` à partir de cette classe
- Nous appelons les méthodes de cet objet
- Nous libérons la mémoire avec `Free` quand nous avons terminé

## Préparation aux sous-sections suivantes

Dans les sous-sections suivantes, nous explorerons en détail tous les aspects de la programmation orientée objet en Delphi :

- Classes et objets
- Propriétés et méthodes
- Héritage et polymorphisme
- Constructeurs et destructeurs
- Interfaces
- Généricité

Ces concepts vous permettront de tirer pleinement parti de la puissance de Delphi et de créer des applications bien structurées et maintenables.

---

La programmation orientée objet est un paradigme puissant qui vous permettra de mieux organiser votre code et de créer des applications plus robustes. Dans les sections suivantes, nous explorerons en détail chaque aspect de la POO en Delphi, en commençant par les classes et les objets.

# 3.7.1 Classes et objets

Après notre introduction à la programmation orientée objet, nous allons maintenant explorer plus en détail le concept fondamental des classes et des objets en Object Pascal. C'est la base sur laquelle repose toute la programmation orientée objet dans Delphi.

## Qu'est-ce qu'une classe ?

Une classe est un modèle, un "plan" qui définit :
- Les **données** (ou attributs) que contiendra un objet
- Les **opérations** (ou méthodes) que l'objet pourra effectuer

Pensez à une classe comme à la définition d'un nouveau type de données personnalisé. De la même façon que `Integer` définit un type de données numériques, une classe comme `TPerson` définirait un type de données représentant une personne.

## Qu'est-ce qu'un objet ?

Un objet est une instance concrète d'une classe. Si la classe est le plan, l'objet est la "maison" construite selon ce plan. Vous pouvez créer plusieurs objets à partir d'une même classe, chacun avec ses propres valeurs de données.

## Déclaration d'une classe

En Object Pascal, une classe se déclare avec le mot-clé `class` :

```pascal
type
  TPerson = class
    // Définition de la classe
  end;
```

Par convention, les noms de classes en Delphi commencent généralement par la lettre `T` (pour "Type").

## Champs (attributs)

Les champs sont les variables qui stockent les données d'un objet. Ils se déclarent comme des variables normales, mais à l'intérieur de la définition de la classe :

```pascal
type
  TPerson = class
    FirstName: string;
    LastName: string;
    Age: Integer;
    Height: Double;
  end;
```

## Méthodes

Les méthodes sont les procédures et fonctions qui définissent le comportement d'un objet. Elles se déclarent dans la définition de la classe, mais leur code est généralement implémenté séparément :

```pascal
type
  TPerson = class
    FirstName: string;
    LastName: string;
    Age: Integer;
    Height: Double;

    procedure Introduce;
    function GetFullName: string;
    procedure Birthday;
  end;

// Implémentation des méthodes
procedure TPerson.Introduce;
begin
  ShowMessage('Bonjour, je m''appelle ' + FirstName + ' ' + LastName +
              ' et j''ai ' + IntToStr(Age) + ' ans.');
end;

function TPerson.GetFullName: string;
begin
  Result := FirstName + ' ' + LastName;
end;

procedure TPerson.Birthday;
begin
  Age := Age + 1;
  ShowMessage('Joyeux anniversaire! J''ai maintenant ' + IntToStr(Age) + ' ans.');
end;
```

Notez que dans l'implémentation des méthodes, nous préfixons le nom de la méthode avec le nom de la classe et un point : `TPerson.Introduce`.

## Visibilité et encapsulation

Une des forces de la POO est l'encapsulation, qui consiste à protéger les données internes d'un objet. En Object Pascal, cela se fait avec des sections de visibilité :

```pascal
type
  TPerson = class
  private
    FFirstName: string;  // Préfixe F pour les champs privés
    FLastName: string;
    FAge: Integer;
  public
    procedure SetFirstName(const Value: string);
    function GetFullName: string;
    procedure Birthday;
  end;
```

Les niveaux de visibilité courants sont :

- `private` : Accessible uniquement à l'intérieur de la classe
- `protected` : Accessible à l'intérieur de la classe et de ses descendantes
- `public` : Accessible partout
- `published` : Comme public, mais avec des fonctionnalités supplémentaires pour les composants visuels

Par convention, les champs privés sont souvent préfixés par la lettre `F`.

## Création et utilisation d'objets

Pour utiliser une classe, vous devez d'abord créer un objet (une instance) de cette classe. Cela se fait avec la méthode `Create` :

```pascal
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.FirstName := 'Jean';
    Person.LastName := 'Dupont';
    Person.Age := 30;

    Person.Introduce;
    ShowMessage('Nom complet : ' + Person.GetFullName);

    Person.Birthday;
  finally
    Person.Free;  // Libération de la mémoire
  end;
end;
```

Notez les points importants :
1. Nous déclarons une variable `Person` de type `TPerson`
2. Nous créons un nouvel objet avec `TPerson.Create`
3. Nous utilisons l'objet en accédant à ses champs et méthodes avec la notation pointée
4. Nous libérons la mémoire avec `Free` quand nous avons terminé

## Structure try-finally pour la gestion de mémoire

En Delphi, vous êtes responsable de la libération de la mémoire utilisée par vos objets. L'utilisation d'un bloc `try-finally` comme ci-dessus est une bonne pratique qui garantit que la mémoire sera libérée même en cas d'exception.

```pascal
Person := TPerson.Create;
try
  // Utilisation de l'objet
finally
  Person.Free;  // Sera toujours exécuté
end;
```

## Champs vs. méthodes vs. propriétés

- **Champs** : Variables qui stockent les données (état) de l'objet
- **Méthodes** : Procédures et fonctions qui définissent le comportement de l'objet
- **Propriétés** : Interface contrôlée pour accéder aux champs (nous les verrons en détail dans la section suivante)

## Self : référence à l'objet courant

Dans les méthodes d'une classe, `Self` est une référence implicite à l'objet sur lequel la méthode est appelée :

```pascal
procedure TPerson.ChangeFirstName(const NewName: string);
begin
  Self.FFirstName := NewName;  // Explicite avec Self
  FLastName := FLastName;      // Implicite (Self est sous-entendu)
end;
```

L'utilisation de `Self` est généralement facultative, sauf en cas d'ambiguïté.

## Classes et mémoire

Les objets en Delphi sont alloués sur le tas (heap) et non sur la pile (stack). Cela signifie :
- Ils persistent jusqu'à ce que vous les libériez explicitement
- Ils peuvent être plus grands que les types simples
- Vous devez gérer leur cycle de vie (création et destruction)

C'est pourquoi nous utilisons `Create` et `Free` comme dans l'exemple ci-dessus.

## Exemple complet d'une classe

Voici un exemple complet d'une classe `TProduct` qui pourrait être utilisée dans une application de gestion d'inventaire :

```pascal
type
  TProduct = class
  private
    FCode: string;
    FName: string;
    FPrice: Currency;
    FQuantity: Integer;

  public
    // Constructeur personnalisé
    constructor Create(const ACode, AName: string; APrice: Currency; AQuantity: Integer = 0);

    // Méthodes
    procedure AddStock(Amount: Integer);
    procedure RemoveStock(Amount: Integer);
    function CalculateTotalValue: Currency;
    procedure Display;

    // Accès aux champs privés
    property Code: string read FCode;
    property Name: string read FName write FName;
    property Price: Currency read FPrice write FPrice;
    property Quantity: Integer read FQuantity;
  end;

// Implémentation
constructor TProduct.Create(const ACode, AName: string; APrice: Currency; AQuantity: Integer = 0);
begin
  inherited Create;  // Appelle le constructeur de la classe parente
  FCode := ACode;
  FName := AName;
  FPrice := APrice;
  FQuantity := AQuantity;
end;

procedure TProduct.AddStock(Amount: Integer);
begin
  if Amount > 0 then
    FQuantity := FQuantity + Amount;
end;

procedure TProduct.RemoveStock(Amount: Integer);
begin
  if (Amount > 0) and (Amount <= FQuantity) then
    FQuantity := FQuantity - Amount;
end;

function TProduct.CalculateTotalValue: Currency;
begin
  Result := FPrice * FQuantity;
end;

procedure TProduct.Display;
begin
  ShowMessage(Format('Produit: %s - %s'#13#10'Prix: %.2f €'#13#10'Quantité: %d'#13#10'Valeur totale: %.2f €',
                    [FCode, FName, FPrice, FQuantity, CalculateTotalValue]));
end;
```

Utilisation de cette classe :

```pascal
procedure TForm1.ButtonCreateProductClick(Sender: TObject);
var
  Product: TProduct;
begin
  Product := TProduct.Create('A001', 'Clavier sans fil', 49.99, 10);
  try
    // Utilisation du produit
    Product.AddStock(5);
    Product.Display;

    // Modification du prix
    Product.Price := 45.99;

    // Retrait du stock
    Product.RemoveStock(3);

    // Affichage mis à jour
    Product.Display;
  finally
    Product.Free;
  end;
end;
```

## Déclaration et implémentation dans des unités

Dans une application réelle, vous placerez généralement :

1. La déclaration de classe dans la section `interface` de l'unité :
```pascal
unit ProductUnit;

interface

type
  TProduct = class
    // Déclaration de la classe
  end;

implementation

// Implémentation des méthodes ici
```

2. L'implémentation des méthodes dans la section `implementation` :
```pascal
implementation

procedure TProduct.AddStock(Amount: Integer);
begin
  // Code ici
end;

// Autres méthodes...
```

## Classes déjà disponibles dans Delphi

Delphi est livré avec de nombreuses classes prédéfinies :

- `TObject` : Classe de base dont héritent toutes les classes
- `TComponent` : Base pour les composants
- `TControl` : Base pour les contrôles visuels
- `TForm` : Formulaires
- `TButton`, `TEdit`, `TLabel`, etc. : Contrôles visuels courants
- `TStringList`, `TList` : Collections
- Et bien d'autres...

## Conseils pratiques

1. **Nommage cohérent** :
   - Préfixez les classes par `T`
   - Préfixez les champs privés par `F`
   - Utilisez des noms clairs et descriptifs

2. **Encapsulation** :
   - Rendez les champs `private` et accédez-y via des propriétés ou méthodes
   - Ne rendez `public` que ce qui doit l'être

3. **Gestion de la mémoire** :
   - Utilisez toujours `try-finally` pour libérer vos objets
   - Initialisez vos variables d'objets à `nil`
   - Vérifiez si un objet est `nil` avant de l'utiliser

4. **Conception** :
   - Une classe devrait avoir une responsabilité claire et unique
   - Gardez vos classes relativement petites et focalisées

---

Les classes et objets forment le fondement de la programmation orientée objet. Maintenant que vous comprenez ces concepts de base, nous verrons dans la section suivante comment utiliser les propriétés et méthodes pour créer des interfaces plus élégantes et contrôlées pour vos classes.


# 3.7.2 Propriétés et méthodes

Après avoir découvert les classes et objets, nous allons maintenant explorer en détail deux éléments fondamentaux qui les composent : les propriétés et les méthodes. Ces concepts sont essentiels pour créer des classes bien conçues et faciles à utiliser.

## Méthodes : le comportement d'une classe

Les méthodes sont des procédures et fonctions associées à une classe. Elles définissent ce que les objets de la classe peuvent faire (leur comportement).

### Déclaration et implémentation des méthodes

La déclaration d'une méthode se fait dans la définition de la classe, tandis que son implémentation est généralement placée dans la section `implementation` de l'unité :

```pascal
type
  TCalculatrice = class
  public
    function Additionner(A, B: Integer): Integer;
    procedure AfficherResultat(Valeur: Integer);
  end;

implementation

// Implémentation de la première méthode
function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Implémentation de la seconde méthode
procedure TCalculatrice.AfficherResultat(Valeur: Integer);
begin
  ShowMessage('Résultat : ' + IntToStr(Valeur));
end;
```

### Types de méthodes

Il existe plusieurs types de méthodes en Object Pascal :

#### 1. Méthodes d'instance

Ce sont les méthodes les plus courantes. Elles opèrent sur un objet spécifique et ont accès à tous ses champs.

```pascal
procedure TPerson.SetAge(NewAge: Integer);
begin
  if (NewAge >= 0) and (NewAge <= 120) then
    FAge := NewAge;
end;
```

#### 2. Méthodes de classe (class methods)

Ces méthodes sont associées à la classe plutôt qu'à une instance spécifique. Elles sont déclarées avec le mot-clé `class`.

```pascal
type
  TMaths = class
  public
    class function Max(A, B: Integer): Integer;
  end;

class function TMaths.Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
```

Utilisation :
```pascal
X := TMaths.Max(10, 20);  // Donne 20 - notez qu'on utilise le nom de la classe
```

#### 3. Méthodes statiques (static methods)

Semblables aux méthodes de classe, mais sans accès à la classe elle-même.

```pascal
type
  TConversion = class
  public
    class var
      TauxChange: Double;
    class function EurosEnDollars(Euros: Double): Double; static;
  end;

class function TConversion.EurosEnDollars(Euros: Double): Double;
begin
  Result := Euros * TauxChange;  // Peut accéder aux champs de classe
end;
```

<span style="color: #0066CC">**Nécessite Delphi 10.4 ou supérieur pour static**</span>

#### 4. Méthodes virtuelles

Les méthodes virtuelles peuvent être redéfinies dans les classes dérivées. Nous les verrons plus en détail dans la section sur l'héritage.

```pascal
type
  TAnimal = class
  public
    function FaireBruit: string; virtual;
  end;
```

### Surcharge de méthodes

La surcharge permet d'avoir plusieurs méthodes avec le même nom mais des paramètres différents :

```pascal
type
  TCalculatrice = class
  public
    function Calculer(A, B: Integer): Integer; overload;
    function Calculer(A, B: Double): Double; overload;
  end;

implementation

function TCalculatrice.Calculer(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TCalculatrice.Calculer(A, B: Double): Double;
begin
  Result := A + B;
end;
```

Utilisation :
```pascal
var
  Calc: TCalculatrice;
begin
  Calc := TCalculatrice.Create;
  try
    ShowMessage(IntToStr(Calc.Calculer(5, 3)));      // Appelle la version Integer
    ShowMessage(FloatToStr(Calc.Calculer(5.2, 3.7))); // Appelle la version Double
  finally
    Calc.Free;
  end;
end;
```

## Propriétés : l'interface contrôlée aux données

Les propriétés sont une caractéristique puissante d'Object Pascal. Elles fournissent une interface contrôlée pour accéder aux champs d'une classe, combinant la simplicité d'utilisation des champs avec le contrôle des méthodes.

### Définition de base d'une propriété

Une propriété est typiquement associée à un champ privé et des méthodes d'accès :

```pascal
type
  TPerson = class
  private
    FAge: Integer;           // Champ privé
    procedure SetAge(Value: Integer);  // Méthode d'accès en écriture
  public
    property Age: Integer read FAge write SetAge;  // Propriété
  end;

implementation

procedure TPerson.SetAge(Value: Integer);
begin
  if (Value >= 0) and (Value <= 120) then  // Validation
    FAge := Value;
end;
```

Utilisation :
```pascal
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.Age := 30;  // Utilise SetAge en interne
    ShowMessage('Âge : ' + IntToStr(Person.Age));  // Lit directement FAge
  finally
    Person.Free;
  end;
end;
```

### Pourquoi utiliser des propriétés ?

Les propriétés offrent plusieurs avantages par rapport à l'accès direct aux champs :

1. **Validation** : Vous pouvez valider les valeurs avant de modifier un champ
2. **Encapsulation** : Le champ reste privé, mais la propriété peut être publique
3. **Fonctionnalité supplémentaire** : Vous pouvez exécuter du code supplémentaire lors de l'accès
4. **Compatibilité** : Vous pouvez modifier l'implémentation interne sans changer l'interface
5. **Support de l'IDE** : Les propriétés sont visibles dans l'inspecteur d'objets et l'éditeur de code

### Différentes façons de définir des propriétés

#### 1. Accès direct au champ

```pascal
property Nom: string read FNom write FNom;
```

#### 2. Méthodes d'accès

```pascal
property Age: Integer read GetAge write SetAge;
```

#### 3. Mixte (lecture directe, écriture par méthode)

```pascal
property Couleur: TColor read FCouleur write SetCouleur;
```

#### 4. Propriété en lecture seule

```pascal
property DateNaissance: TDateTime read FDateNaissance;
```

#### 5. Propriété en écriture seule (rare)

```pascal
property MotDePasse: string write SetMotDePasse;
```

### Méthodes d'accès (Getters et Setters)

Les méthodes d'accès permettent d'ajouter de la logique à l'accès aux propriétés :

```pascal
type
  TRectangle = class
  private
    FLargeur, FHauteur: Integer;
    function GetSurface: Integer;
    procedure SetLargeur(Value: Integer);
    procedure SetHauteur(Value: Integer);
  public
    property Largeur: Integer read FLargeur write SetLargeur;
    property Hauteur: Integer read FHauteur write SetHauteur;
    property Surface: Integer read GetSurface;  // Propriété calculée
  end;

implementation

procedure TRectangle.SetLargeur(Value: Integer);
begin
  if Value > 0 then
    FLargeur := Value;
end;

procedure TRectangle.SetHauteur(Value: Integer);
begin
  if Value > 0 then
    FHauteur := Value;
end;

function TRectangle.GetSurface: Integer;
begin
  Result := FLargeur * FHauteur;
end;
```

### Propriétés calculées

Une propriété calculée n'a pas de champ associé, sa valeur est calculée à la demande :

```pascal
type
  TPersonne = class
  private
    FPrenom: string;
    FNom: string;
    function GetNomComplet: string;
  public
    property Prenom: string read FPrenom write FPrenom;
    property Nom: string read FNom write FNom;
    property NomComplet: string read GetNomComplet;  // Propriété calculée
  end;

implementation

function TPersonne.GetNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;
```

### Propriétés indexées

Les propriétés peuvent avoir un index, ce qui permet de créer des collections accessibles via des propriétés :

```pascal
type
  TTableau = class
  private
    FValeurs: array[0..9] of Integer;
    function GetValeur(Index: Integer): Integer;
    procedure SetValeur(Index: Integer; Value: Integer);
  public
    property Valeurs[Index: Integer]: Integer read GetValeur write SetValeur;
    property Count: Integer read 10;  // Nombre fixe d'éléments
  end;

implementation

function TTableau.GetValeur(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < 10) then
    Result := FValeurs[Index]
  else
    Result := 0;
end;

procedure TTableau.SetValeur(Index: Integer; Value: Integer);
begin
  if (Index >= 0) and (Index < 10) then
    FValeurs[Index] := Value;
end;
```

Utilisation :
```pascal
var
  Tab: TTableau;
begin
  Tab := TTableau.Create;
  try
    Tab.Valeurs[0] := 42;
    Tab.Valeurs[1] := 123;

    ShowMessage(IntToStr(Tab.Valeurs[0]));  // Affiche 42
  finally
    Tab.Free;
  end;
end;
```

### Propriété par défaut

Une classe peut définir une propriété par défaut avec le mot-clé `default`. Cette propriété peut être utilisée sans être nommée explicitement :

```pascal
type
  TTableau = class
  private
    FValeurs: array[0..9] of Integer;
    function GetValeur(Index: Integer): Integer;
    procedure SetValeur(Index: Integer; Value: Integer);
  public
    property Valeurs[Index: Integer]: Integer read GetValeur write SetValeur; default;
  end;
```

Utilisation :
```pascal
Tab[0] := 42;      // Équivalent à Tab.Valeurs[0] := 42
X := Tab[0];       // Équivalent à X := Tab.Valeurs[0]
```

### Attributs de propriété

Les propriétés peuvent avoir des attributs supplémentaires qui indiquent comment elles doivent être traitées par l'IDE et les outils :

```pascal
type
  TPersonne = class
  published  // Section spéciale pour les propriétés visibles dans l'inspecteur d'objets
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write SetAge default 0;
    property Adresse: string read FAdresse write FAdresse stored IsAdresseStored;
    property Telephone: string read FTelephone write FTelephone stored False;
  end;
```

Attributs courants :
- `default` : Valeur par défaut
- `stored` : Indique si la propriété doit être sauvegardée (avec True, False ou une fonction)
- `nodefault` : La propriété n'a pas de valeur par défaut

## Organisation des méthodes et propriétés

Une bonne organisation des méthodes et propriétés rend votre classe plus facile à comprendre et à utiliser :

```pascal
type
  TClient = class
  private
    // Champs privés
    FNom: string;
    FAdresse: string;
    FSolde: Currency;

    // Méthodes d'accès privées
    procedure SetSolde(Value: Currency);

  protected
    // Méthodes accessibles aux classes dérivées
    procedure CalculerRemise; virtual;

  public
    // Constructeur et destructeur
    constructor Create(const ANom: string);
    destructor Destroy; override;

    // Méthodes publiques
    procedure Acheter(Montant: Currency);
    procedure Payer(Montant: Currency);
    function ObtenirEtatCompte: string;

    // Propriétés publiques
    property Nom: string read FNom write FNom;
    property Adresse: string read FAdresse write FAdresse;
    property Solde: Currency read FSolde write SetSolde;
  end;
```

## Conseils pratiques

### 1. Préfixes courants

- Champs privés : préfixe `F` (ex. `FNom`)
- Méthodes d'accès : préfixes `Get` et `Set` (ex. `GetAge`, `SetAge`)

### 2. Réutilisation du code

Divisez les opérations complexes en plusieurs méthodes plus petites et plus spécifiques :

```pascal
// Au lieu de ceci :
procedure TTraitement.TraiterTout;
begin
  // 100 lignes de code faisant tout
end;

// Préférez ceci :
procedure TTraitement.TraiterTout;
begin
  ChargerDonnees;
  ValiderDonnees;
  TransformerDonnees;
  EnregistrerResultats;
end;
```

### 3. Validation dans les setters

Ajoutez toujours des validations dans les méthodes de définition des propriétés :

```pascal
procedure TPersonne.SetAge(Value: Integer);
begin
  if (Value < 0) then
    raise Exception.Create('L''âge ne peut pas être négatif');

  if (Value > 120) then
    raise Exception.Create('L''âge semble trop élevé');

  FAge := Value;
end;
```

### 4. Documentation des propriétés et méthodes

Ajoutez des commentaires pour expliquer le but et le fonctionnement de vos propriétés et méthodes :

```pascal
type
  TCompte = class
  private
    FSolde: Currency;
  public
    { Effectue un dépôt sur le compte.
      @param Montant Le montant à déposer (doit être positif)
      @return True si l'opération a réussi, False sinon }
    function Deposer(Montant: Currency): Boolean;

    { Effectue un retrait du compte.
      @param Montant Le montant à retirer (doit être positif)
      @return True si l'opération a réussi, False sinon }
    function Retirer(Montant: Currency): Boolean;

    { Solde actuel du compte en euros }
    property Solde: Currency read FSolde;
  end;
```

## Exemple complet

Voici un exemple complet illustrant l'utilisation des propriétés et méthodes dans une classe :

```pascal
unit CompteBancaire;

interface

type
  TTypeCompte = (tcCourant, tcEpargne, tcProfessionnel);

  TCompteBancaire = class
  private
    FNumero: string;
    FTitulaire: string;
    FSolde: Currency;
    FTypeCompte: TTypeCompte;
    FDateCreation: TDateTime;
    FDecouvertAutorise: Currency;

    function GetAge: Integer;
    procedure SetSolde(const Value: Currency);
    procedure SetDecouvertAutorise(const Value: Currency);
  public
    constructor Create(const ANumero, ATitulaire: string; ATypeCompte: TTypeCompte);
    destructor Destroy; override;

    function Deposer(Montant: Currency): Boolean;
    function Retirer(Montant: Currency): Boolean;
    function CalculerInterets(TauxAnnuel: Double): Currency;
    function ToString: string; override;

    property Numero: string read FNumero;
    property Titulaire: string read FTitulaire write FTitulaire;
    property Solde: Currency read FSolde write SetSolde;
    property TypeCompte: TTypeCompte read FTypeCompte;
    property DateCreation: TDateTime read FDateCreation;
    property Age: Integer read GetAge;
    property DecouvertAutorise: Currency read FDecouvertAutorise write SetDecouvertAutorise;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TCompteBancaire }

function TCompteBancaire.CalculerInterets(TauxAnnuel: Double): Currency;
begin
  // Calcul simplifié des intérêts
  if TypeCompte = tcEpargne then
    Result := FSolde * (TauxAnnuel / 100)
  else
    Result := 0;
end;

constructor TCompteBancaire.Create(const ANumero, ATitulaire: string; ATypeCompte: TTypeCompte);
begin
  inherited Create;
  FNumero := ANumero;
  FTitulaire := ATitulaire;
  FTypeCompte := ATypeCompte;
  FSolde := 0;
  FDateCreation := Now;

  // Définir le découvert autorisé selon le type de compte
  case FTypeCompte of
    tcCourant: FDecouvertAutorise := 500;
    tcProfessionnel: FDecouvertAutorise := 2000;
    else FDecouvertAutorise := 0;
  end;
end;

destructor TCompteBancaire.Destroy;
begin
  // Rien de spécial à faire ici, mais bonne pratique
  // d'avoir un destructeur pour les classes complexes
  inherited;
end;

function TCompteBancaire.Deposer(Montant: Currency): Boolean;
begin
  Result := False;

  if Montant <= 0 then
    Exit;

  FSolde := FSolde + Montant;
  Result := True;
end;

function TCompteBancaire.GetAge: Integer;
begin
  Result := YearsBetween(Now, FDateCreation);
end;

function TCompteBancaire.Retirer(Montant: Currency): Boolean;
begin
  Result := False;

  if Montant <= 0 then
    Exit;

  if (FSolde - Montant) < -FDecouvertAutorise then
    Exit;

  FSolde := FSolde - Montant;
  Result := True;
end;

procedure TCompteBancaire.SetDecouvertAutorise(const Value: Currency);
begin
  if (FTypeCompte = tcEpargne) and (Value > 0) then
    raise Exception.Create('Pas de découvert autorisé pour les comptes épargne');

  if Value < 0 then
    raise Exception.Create('Le découvert autorisé ne peut pas être négatif');

  FDecouvertAutorise := Value;
end;

procedure TCompteBancaire.SetSolde(const Value: Currency);
begin
  if (Value < -FDecouvertAutorise) then
    raise Exception.Create('Solde insuffisant');

  FSolde := Value;
end;

function TCompteBancaire.ToString: string;
const
  TypeCompteStr: array[TTypeCompte] of string = ('Courant', 'Épargne', 'Professionnel');
begin
  Result := Format('Compte %s n°%s'#13#10'Titulaire: %s'#13#10'Solde: %.2f €'#13#10'Découvert: %.2f €',
    [TypeCompteStr[FTypeCompte], FNumero, FTitulaire, FSolde, FDecouvertAutorise]);
end;

end.
```

Utilisation :
```pascal
var
  Compte: TCompteBancaire;
begin
  Compte := TCompteBancaire.Create('123456789', 'Jean Dupont', tcCourant);
  try
    Compte.Deposer(1000);

    if Compte.Retirer(200) then
      ShowMessage('Retrait effectué avec succès')
    else
      ShowMessage('Retrait impossible');

    ShowMessage(Compte.ToString);

    // Utilisation des propriétés
    ShowMessage('Age du compte : ' + IntToStr(Compte.Age) + ' ans');

    // Modification d'une propriété avec validation
    try
      Compte.DecouvertAutorise := 1000;
      ShowMessage('Nouveau découvert autorisé : ' + CurrToStr(Compte.DecouvertAutorise));
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  finally
    Compte.Free;
  end;
end;
```

---

Dans cette section, nous avons exploré en détail les méthodes et propriétés, deux concepts fondamentaux de la programmation orientée objet en Delphi. Les méthodes définissent le comportement de vos objets, tandis que les propriétés offrent une interface contrôlée pour accéder à leurs données. En combinant ces deux éléments de manière judicieuse, vous pouvez créer des classes bien encapsulées, faciles à utiliser et maintenables.

Dans la prochaine section, nous explorerons l'héritage et le polymorphisme, qui vous permettront d'exploiter au maximum la réutilisation de code et la flexibilité offertes par la programmation orientée objet.

# 3.7.3 Héritage et polymorphisme

L'héritage et le polymorphisme sont deux concepts fondamentaux de la programmation orientée objet qui permettent de créer des hiérarchies de classes flexibles et extensibles. Dans cette section, nous allons explorer ces concepts en Object Pascal et voir comment ils peuvent rendre votre code plus organisé et réutilisable.

## Qu'est-ce que l'héritage ?

L'héritage est un mécanisme qui permet à une classe (appelée classe dérivée ou enfant) d'hériter des propriétés et méthodes d'une autre classe (appelée classe de base ou parent). C'est comme si vous créiez une nouvelle classe qui possède déjà toutes les fonctionnalités de la classe existante, et à laquelle vous pouvez ajouter des fonctionnalités supplémentaires.

### Pourquoi utiliser l'héritage ?

- **Réutilisation du code** : Évitez de réécrire le même code dans plusieurs classes
- **Extension** : Ajoutez de nouvelles fonctionnalités à des classes existantes
- **Spécialisation** : Créez des versions plus spécifiques de classes générales
- **Organisation** : Structurez vos classes en hiérarchies logiques

## Syntaxe de base de l'héritage

En Object Pascal, l'héritage s'exprime avec le mot-clé `(parent)` après le nom de la classe :

```pascal
type
  TAnimal = class
    // Classe de base
  end;

  TChien = class(TAnimal)
    // Classe dérivée qui hérite de TAnimal
  end;
```

Dans cet exemple, `TChien` hérite de tous les champs, propriétés et méthodes de `TAnimal`.

## Exemple simple d'héritage

Voici un exemple concret pour illustrer l'héritage :

```pascal
type
  TVehicule = class
  private
    FMarque: string;
    FModele: string;
    FAnnee: Integer;
  public
    constructor Create(const AMarque, AModele: string; AAnnee: Integer);
    procedure Demarrer; virtual;
    procedure Arreter; virtual;

    property Marque: string read FMarque;
    property Modele: string read FModele;
    property Annee: Integer read FAnnee;
  end;

  TVoiture = class(TVehicule)
  private
    FNombrePortes: Integer;
  public
    constructor Create(const AMarque, AModele: string; AAnnee, ANombrePortes: Integer);
    procedure Demarrer; override;
    procedure Klaxonner;

    property NombrePortes: Integer read FNombrePortes;
  end;
```

Implémentation :

```pascal
{ TVehicule }

constructor TVehicule.Create(const AMarque, AModele: string; AAnnee: Integer);
begin
  inherited Create;  // Appelle le constructeur de la classe parente (TObject)
  FMarque := AMarque;
  FModele := AModele;
  FAnnee := AAnnee;
end;

procedure TVehicule.Demarrer;
begin
  ShowMessage('Le véhicule démarre');
end;

procedure TVehicule.Arreter;
begin
  ShowMessage('Le véhicule s''arrête');
end;

{ TVoiture }

constructor TVoiture.Create(const AMarque, AModele: string; AAnnee, ANombrePortes: Integer);
begin
  inherited Create(AMarque, AModele, AAnnee);  // Appelle le constructeur du parent
  FNombrePortes := ANombrePortes;
end;

procedure TVoiture.Demarrer;
begin
  ShowMessage('La voiture démarre avec un bruit de moteur');
  // Si on veut aussi exécuter le code du parent :
  // inherited Demarrer;
end;

procedure TVoiture.Klaxonner;
begin
  ShowMessage('Beep beep!');
end;
```

Utilisation :

```pascal
var
  MaVoiture: TVoiture;
begin
  MaVoiture := TVoiture.Create('Renault', 'Clio', 2020, 5);
  try
    // Méthodes héritées
    ShowMessage(MaVoiture.Marque + ' ' + MaVoiture.Modele);  // "Renault Clio"

    // Méthode redéfinie
    MaVoiture.Demarrer;  // "La voiture démarre avec un bruit de moteur"

    // Méthode spécifique à TVoiture
    MaVoiture.Klaxonner;  // "Beep beep!"

    // Méthode héritée non redéfinie
    MaVoiture.Arreter;  // "Le véhicule s'arrête"
  finally
    MaVoiture.Free;
  end;
end;
```

## Méthodes virtuelles et redéfinition

Pour permettre à une classe dérivée de modifier le comportement d'une méthode héritée, nous utilisons les mots-clés `virtual` et `override` :

- `virtual` dans la classe de base indique qu'une méthode peut être redéfinie
- `override` dans la classe dérivée indique que l'on redéfinit la méthode

```pascal
type
  TAnimal = class
  public
    procedure FaireBruit; virtual;  // Peut être redéfinie
  end;

  TChien = class(TAnimal)
  public
    procedure FaireBruit; override;  // Redéfinition
  end;

  TChat = class(TAnimal)
  public
    procedure FaireBruit; override;  // Autre redéfinition
  end;

implementation

procedure TAnimal.FaireBruit;
begin
  ShowMessage('...');
end;

procedure TChien.FaireBruit;
begin
  ShowMessage('Wouf!');
end;

procedure TChat.FaireBruit;
begin
  ShowMessage('Miaou!');
end;
```

### Appel de la méthode parente

Dans une méthode redéfinie, vous pouvez appeler la version de la classe parente avec le mot-clé `inherited` :

```pascal
procedure TChien.FaireBruit;
begin
  inherited FaireBruit;  // Appelle TAnimal.FaireBruit
  ShowMessage('Wouf!');
end;
```

## Hiérarchie d'héritage

L'héritage peut s'étendre sur plusieurs niveaux pour créer des hiérarchies de classes :

```pascal
type
  TAnimal = class
    // Classe de base
  end;

  TMammifere = class(TAnimal)
    // Hérite de TAnimal
  end;

  TChien = class(TMammifere)
    // Hérite de TMammifere, qui hérite de TAnimal
  end;
```

Une classe dérivée hérite de toutes les classes dans sa chaîne d'héritage.

## Type Object et classes

En Object Pascal, toutes les classes dérivent automatiquement de `TObject`, même si vous ne le spécifiez pas :

```pascal
type
  TExemple = class  // Implicitement TExemple = class(TObject)
    // ...
  end;
```

`TObject` fournit des méthodes de base comme :
- `Create` : Constructeur par défaut
- `Free` : Libère l'objet
- `ClassName` : Renvoie le nom de la classe
- `ClassType` : Renvoie le type de la classe
- `InheritsFrom` : Vérifie si la classe hérite d'une autre classe

## Héritage simple vs multiple

Object Pascal supporte uniquement l'héritage simple (une classe ne peut hériter que d'une seule classe parente). Pour contourner cette limitation et réutiliser du code de plusieurs sources, Delphi utilise les interfaces (que nous verrons dans une section ultérieure).

## Polymorphisme : qu'est-ce que c'est ?

Le polymorphisme (du grec "plusieurs formes") est la capacité d'objets de classes différentes à être traités comme des objets d'une classe commune. C'est l'un des concepts les plus puissants de la POO.

Concrètement, cela signifie que vous pouvez :
1. Référencer un objet d'une classe dérivée via une variable de la classe de base
2. Appeler des méthodes qui se comporteront différemment selon le type réel de l'objet

## Polymorphisme en action

Pour illustrer le polymorphisme, reprenons notre exemple avec les animaux :

```pascal
procedure FaireBruitAuxAnimaux;
var
  Animaux: array[0..2] of TAnimal;
  i: Integer;
begin
  // Création d'objets de différentes classes
  Animaux[0] := TAnimal.Create;
  Animaux[1] := TChien.Create;
  Animaux[2] := TChat.Create;

  try
    // Polymorphisme : même appel, comportements différents
    for i := 0 to 2 do
      Animaux[i].FaireBruit;
      // Affiche "...", puis "Wouf!", puis "Miaou!"
  finally
    // Libération de la mémoire
    for i := 0 to 2 do
      Animaux[i].Free;
  end;
end;
```

Dans cet exemple :
1. Nous avons un tableau d'objets `TAnimal`
2. Nous y stockons des objets de types `TAnimal`, `TChien` et `TChat`
3. Nous appelons la même méthode `FaireBruit` sur chaque objet
4. Chaque objet exécute sa propre version de la méthode

C'est le cœur du polymorphisme : un même code qui produit différents comportements selon le type réel des objets.

## Méthodes abstraites

Une méthode abstraite est déclarée dans une classe de base mais n'y est pas implémentée. Les classes dérivées doivent fournir leur propre implémentation.

```pascal
type
  TFigure = class
  public
    function CalculerSurface: Double; virtual; abstract;
    function CalculerPerimetre: Double; virtual; abstract;
  end;

  TCercle = class(TFigure)
  private
    FRayon: Double;
  public
    constructor Create(ARayon: Double);
    function CalculerSurface: Double; override;
    function CalculerPerimetre: Double; override;
  end;

  TRectangle = class(TFigure)
  private
    FLargeur, FHauteur: Double;
  public
    constructor Create(ALargeur, AHauteur: Double);
    function CalculerSurface: Double; override;
    function CalculerPerimetre: Double; override;
  end;
```

Implémentation :

```pascal
{ TCercle }

constructor TCercle.Create(ARayon: Double);
begin
  inherited Create;
  FRayon := ARayon;
end;

function TCercle.CalculerSurface: Double;
begin
  Result := Pi * FRayon * FRayon;
end;

function TCercle.CalculerPerimetre: Double;
begin
  Result := 2 * Pi * FRayon;
end;

{ TRectangle }

constructor TRectangle.Create(ALargeur, AHauteur: Double);
begin
  inherited Create;
  FLargeur := ALargeur;
  FHauteur := AHauteur;
end;

function TRectangle.CalculerSurface: Double;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.CalculerPerimetre: Double;
begin
  Result := 2 * (FLargeur + FHauteur);
end;
```

Utilisation avec polymorphisme :

```pascal
procedure AfficherInfosFigure(Figure: TFigure);
begin
  ShowMessage('Surface : ' + FloatToStr(Figure.CalculerSurface));
  ShowMessage('Périmètre : ' + FloatToStr(Figure.CalculerPerimetre));
end;

var
  Cercle: TCercle;
  Rectangle: TRectangle;
begin
  Cercle := TCercle.Create(5);
  Rectangle := TRectangle.Create(4, 6);

  try
    AfficherInfosFigure(Cercle);      // Utilise TCercle.CalculerSurface/Perimetre
    AfficherInfosFigure(Rectangle);   // Utilise TRectangle.CalculerSurface/Perimetre
  finally
    Cercle.Free;
    Rectangle.Free;
  end;
end;
```

## Classes abstraites

Une classe abstraite contient au moins une méthode abstraite et ne peut pas être instanciée directement. Elle sert de modèle pour les classes dérivées.

```pascal
type
  TFigure = class  // Classe abstraite (contient des méthodes abstraites)
  public
    function CalculerSurface: Double; virtual; abstract;
    function CalculerPerimetre: Double; virtual; abstract;
  end;
```

Vous ne pouvez pas créer d'objet `TFigure` directement :

```pascal
var
  Figure: TFigure;
begin
  Figure := TFigure.Create;  // Erreur de compilation!
end;
```

## Vérification dynamique de type

Parfois, vous avez besoin de connaître le type réel d'un objet pour effectuer des opérations spécifiques :

```pascal
procedure TraiterVehicule(Vehicule: TVehicule);
begin
  // Opérations communes à tous les véhicules
  Vehicule.Demarrer;

  // Opérations spécifiques selon le type
  if Vehicule is TVoiture then
  begin
    ShowMessage('C''est une voiture!');
    TVoiture(Vehicule).Klaxonner;  // Cast explicite
  end
  else if Vehicule is TMoto then
  begin
    ShowMessage('C''est une moto!');
    // Opérations spécifiques aux motos
  end;

  Vehicule.Arreter;
end;
```

Deux opérateurs importants :
- `is` : vérifie si un objet est d'un type spécifique
- `as` : convertit un objet en un autre type (avec vérification)

```pascal
if Vehicule is TVoiture then
begin
  // Deux façons de faire un cast :
  MaVoiture := TVoiture(Vehicule);  // Cast traditionnel (pas de vérification)
  // ou
  MaVoiture := Vehicule as TVoiture;  // Cast sécurisé (lève une exception si incompatible)

  MaVoiture.Klaxonner;
end;
```

## Visibilité et héritage

Rappel sur les niveaux de visibilité et leur impact sur l'héritage :

- `private` : Accessible uniquement dans la classe déclarante
- `protected` : Accessible dans la classe déclarante et ses descendants
- `public` : Accessible partout
- `published` : Comme public, mais avec des fonctionnalités supplémentaires

Pour l'héritage, la distinction `private`/`protected` est cruciale :

```pascal
type
  TParent = class
  private
    FChampPrive: Integer;      // Non accessible aux descendants
  protected
    FChampProtege: Integer;    // Accessible aux descendants
  public
    property ChampPrive: Integer read FChampPrive;
    property ChampProtege: Integer read FChampProtege;
  end;

  TEnfant = class(TParent)
  public
    procedure Exemple;
  end;

procedure TEnfant.Exemple;
begin
  // FChampPrive := 10;    // Erreur! Champ privé inaccessible
  FChampProtege := 20;     // OK, champ protégé accessible
end;
```

## Héritage et propriétés

Les propriétés peuvent aussi être redéfinies dans les classes dérivées :

```pascal
type
  TVehicule = class
  private
    FVitesseMax: Integer;
  protected
    function GetDescription: string; virtual;
  public
    property VitesseMax: Integer read FVitesseMax write FVitesseMax;
    property Description: string read GetDescription;
  end;

  TVoiture = class(TVehicule)
  protected
    function GetDescription: string; override;
  end;

function TVehicule.GetDescription: string;
begin
  Result := 'Véhicule - Vitesse max: ' + IntToStr(FVitesseMax) + ' km/h';
end;

function TVoiture.GetDescription: string;
begin
  Result := 'Voiture - ' + inherited GetDescription;
end;
```

## Exemple complet

Voici un exemple complet illustrant l'héritage et le polymorphisme avec une hiérarchie de formes géométriques :

```pascal
unit FormeGeometrique;

interface

uses
  System.SysUtils, System.Classes;

type
  TFormeGeometrique = class
  private
    FCouleur: string;
  protected
    function GetAire: Double; virtual; abstract;
    function GetPerimetre: Double; virtual; abstract;
  public
    constructor Create(const ACouleur: string);
    function Dessiner: string; virtual;
    function Description: string; virtual;

    property Couleur: string read FCouleur write FCouleur;
    property Aire: Double read GetAire;
    property Perimetre: Double read GetPerimetre;
  end;

  TRectangle = class(TFormeGeometrique)
  private
    FLargeur: Double;
    FHauteur: Double;
  protected
    function GetAire: Double; override;
    function GetPerimetre: Double; override;
  public
    constructor Create(const ACouleur: string; ALargeur, AHauteur: Double);
    function Dessiner: string; override;
    function Description: string; override;

    property Largeur: Double read FLargeur write FLargeur;
    property Hauteur: Double read FHauteur write FHauteur;
  end;

  TCercle = class(TFormeGeometrique)
  private
    FRayon: Double;
  protected
    function GetAire: Double; override;
    function GetPerimetre: Double; override;
  public
    constructor Create(const ACouleur: string; ARayon: Double);
    function Dessiner: string; override;
    function Description: string; override;

    property Rayon: Double read FRayon write FRayon;
  end;

  TTriangle = class(TFormeGeometrique)
  private
    FCoteA: Double;
    FCoteB: Double;
    FCoteC: Double;
  protected
    function GetAire: Double; override;
    function GetPerimetre: Double; override;
  public
    constructor Create(const ACouleur: string; ACoteA, ACoteB, ACoteC: Double);
    function Dessiner: string; override;
    function Description: string; override;

    property CoteA: Double read FCoteA write FCoteA;
    property CoteB: Double read FCoteB write FCoteB;
    property CoteC: Double read FCoteC write FCoteC;
  end;

implementation

{ TFormeGeometrique }

constructor TFormeGeometrique.Create(const ACouleur: string);
begin
  inherited Create;
  FCouleur := ACouleur;
end;

function TFormeGeometrique.Description: string;
begin
  Result := 'Forme géométrique de couleur ' + FCouleur;
end;

function TFormeGeometrique.Dessiner: string;
begin
  Result := 'Dessin d''une forme';
end;

{ TRectangle }

constructor TRectangle.Create(const ACouleur: string; ALargeur, AHauteur: Double);
begin
  inherited Create(ACouleur);
  FLargeur := ALargeur;
  FHauteur := AHauteur;
end;

function TRectangle.Description: string;
begin
  Result := Format('Rectangle %s de dimensions %.2f x %.2f',
                  [Couleur, FLargeur, FHauteur]);
end;

function TRectangle.Dessiner: string;
begin
  Result := 'Dessin d''un rectangle ' + Couleur;
end;

function TRectangle.GetAire: Double;
begin
  Result := FLargeur * FHauteur;
end;

function TRectangle.GetPerimetre: Double;
begin
  Result := 2 * (FLargeur + FHauteur);
end;

{ TCercle }

constructor TCercle.Create(const ACouleur: string; ARayon: Double);
begin
  inherited Create(ACouleur);
  FRayon := ARayon;
end;

function TCercle.Description: string;
begin
  Result := Format('Cercle %s de rayon %.2f', [Couleur, FRayon]);
end;

function TCercle.Dessiner: string;
begin
  Result := 'Dessin d''un cercle ' + Couleur;
end;

function TCercle.GetAire: Double;
begin
  Result := Pi * FRayon * FRayon;
end;

function TCercle.GetPerimetre: Double;
begin
  Result := 2 * Pi * FRayon;
end;

{ TTriangle }

constructor TTriangle.Create(const ACouleur: string; ACoteA, ACoteB, ACoteC: Double);
begin
  inherited Create(ACouleur);
  FCoteA := ACoteA;
  FCoteB := ACoteB;
  FCoteC := ACoteC;
end;

function TTriangle.Description: string;
begin
  Result := Format('Triangle %s de côtés %.2f, %.2f et %.2f',
                  [Couleur, FCoteA, FCoteB, FCoteC]);
end;

function TTriangle.Dessiner: string;
begin
  Result := 'Dessin d''un triangle ' + Couleur;
end;

function TTriangle.GetAire: Double;
var
  S: Double;
begin
  // Formule de Héron
  S := (FCoteA + FCoteB + FCoteC) / 2;
  Result := Sqrt(S * (S - FCoteA) * (S - FCoteB) * (S - FCoteC));
end;

function TTriangle.GetPerimetre: Double;
begin
  Result := FCoteA + FCoteB + FCoteC;
end;

end.
```

Utilisation avec polymorphisme :

```pascal
procedure DemonstrationPolymorphisme;
var
  Formes: array[0..2] of TFormeGeometrique;
  i: Integer;
  TotalAire: Double;
begin
  // Création d'objets de différentes classes
  Formes[0] := TRectangle.Create('Rouge', 5, 3);
  Formes[1] := TCercle.Create('Bleu', 4);
  Formes[2] := TTriangle.Create('Vert', 3, 4, 5);

  try
    // Affichage polymorphique
    for i := 0 to 2 do
    begin
      ShowMessage(Formes[i].Description);
      ShowMessage(Formes[i].Dessiner);
      ShowMessage('Aire: ' + FloatToStr(Formes[i].Aire));
      ShowMessage('Périmètre: ' + FloatToStr(Formes[i].Perimetre));
    end;

    // Calcul de l'aire totale (polymorphisme en action)
    TotalAire := 0;
    for i := 0 to 2 do
      TotalAire := TotalAire + Formes[i].Aire;

    ShowMessage('Aire totale: ' + FloatToStr(TotalAire));

  finally
    // Libération de la mémoire
    for i := 0 to 2 do
      Formes[i].Free;
  end;
end;
```

## Conseils pratiques

1. **Utilisez protected pour l'héritage** :
   - Les éléments qui doivent être accessibles aux classes dérivées mais pas au monde extérieur doivent être `protected`

2. **Pensez à la redéfinition** :
   - Déclarez comme `virtual` les méthodes qui pourraient avoir besoin d'être spécialisées
   - Utilisez `override` (et non `reintroduce`) pour redéfinir une méthode

3. **Appel de la méthode parent** :
   - N'oubliez pas d'appeler la méthode parent avec `inherited` quand nécessaire
   - C'est particulièrement important pour les constructeurs et destructeurs

4. **Hiérarchies d'objets** :
   - Concevez vos hiérarchies avec soin, en plaçant les comportements communs dans les classes de base
   - Évitez les hiérarchies trop profondes (plus de 3-4 niveaux peuvent devenir difficiles à maintenir)

5. **Destructeurs virtuels** :
   - Assurez-vous que vos destructeurs sont virtuels pour éviter les fuites de mémoire

```pascal
destructor TFormeGeometrique.Destroy; override;  // override car TObject.Destroy est virtual
begin
  // Nettoyage spécifique ici

  inherited;  // Appel du destructeur parent (APRÈS le nettoyage)
end;
```

---

L'héritage et le polymorphisme sont des outils puissants qui vous permettent de créer des hiérarchies de classes flexibles et extensibles. En comprenant comment les utiliser efficacement, vous pourrez concevoir des applications Delphi bien structurées et faciles à maintenir.

Dans la prochaine section, nous approfondirons la gestion des objets en explorant les constructeurs et destructeurs, qui sont essentiels pour initialiser correctement vos objets et libérer les ressources qu'ils utilisent.

# 3.7.4 Constructeurs et destructeurs

Les constructeurs et destructeurs sont des méthodes spéciales qui gèrent respectivement la création et la destruction des objets. Ces méthodes sont essentielles pour garantir que vos objets sont correctement initialisés et que toutes les ressources qu'ils utilisent sont proprement libérées. Dans cette section, nous allons explorer comment utiliser efficacement ces méthodes en Object Pascal.

## Constructeurs : créer et initialiser des objets

Un constructeur est une méthode spéciale qui est appelée automatiquement lors de la création d'un objet. Son rôle principal est d'initialiser l'objet et de s'assurer qu'il est dans un état valide avant d'être utilisé.

### Le constructeur par défaut

Toutes les classes en Delphi héritent d'un constructeur par défaut nommé `Create` de la classe `TObject` :

```pascal
var
  MaClasse: TMaClasse;
begin
  MaClasse := TMaClasse.Create;  // Appel du constructeur par défaut
  try
    // Utilisation de l'objet
  finally
    MaClasse.Free;
  end;
end;
```

### Création d'un constructeur personnalisé

Pour créer votre propre constructeur, vous devez :
1. Déclarer un constructeur dans la section de classe
2. Implémenter le constructeur, généralement en appelant d'abord le constructeur parent

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const ANom: string; AAge: Integer);
    // ...
  end;

constructor TPersonne.Create(const ANom: string; AAge: Integer);
begin
  inherited Create;  // Appelle le constructeur de la classe parente (TObject)
  FNom := ANom;
  FAge := AAge;
end;
```

Utilisation :

```pascal
var
  Personne: TPersonne;
begin
  Personne := TPersonne.Create('Jean Dupont', 30);
  try
    // Utilisation de l'objet
  finally
    Personne.Free;
  end;
end;
```

### Plusieurs constructeurs

Une classe peut avoir plusieurs constructeurs, avec des signatures différentes :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create; overload;  // Constructeur par défaut
    constructor Create(const ANom: string); overload;  // Avec nom seulement
    constructor Create(const ANom: string; AAge: Integer); overload;  // Avec nom et âge
    // ...
  end;

constructor TPersonne.Create;
begin
  inherited Create;
  FNom := 'Inconnu';
  FAge := 0;
end;

constructor TPersonne.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FAge := 0;
end;

constructor TPersonne.Create(const ANom: string; AAge: Integer);
begin
  inherited Create;
  FNom := ANom;
  FAge := AAge;
end;
```

### Réutilisation de code entre constructeurs

Pour éviter la duplication de code, vous pouvez faire en sorte qu'un constructeur en appelle un autre :

```pascal
constructor TPersonne.Create;
begin
  Create('Inconnu', 0);  // Appel à un autre constructeur de la même classe
end;

constructor TPersonne.Create(const ANom: string);
begin
  Create(ANom, 0);  // Appel à un autre constructeur de la même classe
end;

constructor TPersonne.Create(const ANom: string; AAge: Integer);
begin
  inherited Create;  // Appel du constructeur parent
  FNom := ANom;
  FAge := AAge;
end;
```

### Paramètres par défaut

Une autre approche consiste à utiliser des paramètres par défaut :

```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const ANom: string = 'Inconnu'; AAge: Integer = 0);
    // ...
  end;

constructor TPersonne.Create(const ANom: string; AAge: Integer);
begin
  inherited Create;
  FNom := ANom;
  FAge := AAge;
end;
```

Utilisation :

```pascal
Personne1 := TPersonne.Create;                // Utilise les valeurs par défaut
Personne2 := TPersonne.Create('Jean');        // Nom = 'Jean', Age = 0
Personne3 := TPersonne.Create('Marie', 25);   // Nom = 'Marie', Age = 25
```

### Constructeurs virtuels

En Delphi, les constructeurs peuvent être virtuels, ce qui permet aux classes dérivées de personnaliser le processus de création :

```pascal
type
  TBase = class
  public
    constructor Create; virtual;
  end;

  TDerivee = class(TBase)
  public
    constructor Create; override;
  end;
```

Les constructeurs virtuels sont particulièrement utiles pour les frameworks et les bibliothèques, mais pour les applications standard, les constructeurs non virtuels sont généralement suffisants.

## Destructeurs : libérer les ressources

Un destructeur est une méthode spéciale qui est appelée avant qu'un objet ne soit détruit. Son rôle principal est de libérer les ressources que l'objet a acquises pendant sa vie.

### Le destructeur par défaut

Toutes les classes héritent d'un destructeur par défaut nommé `Destroy` de la classe `TObject`, qui est déclaré comme `virtual` :

```pascal
type
  TObject = class
  public
    constructor Create;
    destructor Destroy; virtual;
    // ...
  end;
```

### Création d'un destructeur personnalisé

Pour créer votre propre destructeur, vous devez :
1. Déclarer un destructeur dans la section de classe avec le mot-clé `override`
2. Implémenter le destructeur, en terminant généralement par l'appel au destructeur parent

```pascal
type
  TFichierTexte = class
  private
    FFichier: TextFile;
    FFichierOuvert: Boolean;
  public
    constructor Create(const NomFichier: string);
    destructor Destroy; override;
    // ...
  end;

constructor TFichierTexte.Create(const NomFichier: string);
begin
  inherited Create;
  AssignFile(FFichier, NomFichier);
  Reset(FFichier);
  FFichierOuvert := True;
end;

destructor TFichierTexte.Destroy;
begin
  if FFichierOuvert then
    CloseFile(FFichier);

  inherited Destroy;  // Appel du destructeur parent (TOUJOURS EN DERNIER)
end;
```

### Ordre des opérations dans un destructeur

Dans un destructeur, il est important de :
1. Libérer d'abord vos propres ressources
2. Appeler `inherited Destroy` en dernier

Cet ordre est l'inverse de celui d'un constructeur où vous appelez d'abord `inherited Create` puis initialisez vos propres ressources.

```pascal
destructor TMaClasse.Destroy;
begin
  // 1. Libération des ressources propres à cette classe
  MonObjet.Free;
  MaListe.Free;

  // 2. Appel du destructeur parent (TOUJOURS EN DERNIER)
  inherited Destroy;
end;
```

### Pourquoi appeler inherited Destroy en dernier ?

Imaginez une hiérarchie de classes où chaque niveau alloue ses propres ressources :

```
TObject
  |
  +--> TParent (alloue ressource A)
        |
        +--> TEnfant (alloue ressource B)
```

Si `TEnfant.Destroy` appelle `inherited Destroy` avant de libérer la ressource B, la séquence serait :
1. `TEnfant.Destroy` commence
2. `TParent.Destroy` est appelé et libère la ressource A
3. `TObject.Destroy` est appelé
4. Retour à `TEnfant.Destroy` pour libérer la ressource B, mais l'objet pourrait déjà être dans un état invalide

En appelant `inherited Destroy` en dernier, vous garantissez que l'objet reste dans un état cohérent jusqu'à ce que toutes ses propres ressources soient libérées.

## Free vs Destroy

Dans Delphi, vous devez rarement appeler `Destroy` directement. À la place, utilisez la méthode `Free` :

```pascal
MonObjet.Free;  // Préférer cette approche
// plutôt que
MonObjet.Destroy;
```

Pourquoi ? Parce que `Free` vérifie d'abord si l'objet est `nil` avant d'appeler `Destroy` :

```pascal
procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;
```

Cela vous évite les erreurs "Accès à une référence nil" si l'objet a déjà été libéré ou n'a jamais été créé.

## FreeAndNil

Pour éviter les références pendantes, vous pouvez utiliser `FreeAndNil` qui libère l'objet et met la référence à `nil` :

```pascal
var
  MonObjet: TMonObjet;
begin
  MonObjet := TMonObjet.Create;
  // ...
  FreeAndNil(MonObjet);  // Libère l'objet et met MonObjet à nil

  // Maintenant, c'est sûr :
  if MonObjet = nil then
    ShowMessage('L''objet a été libéré');
end;
```

## Gestion automatique de la mémoire avec try-finally

Le pattern standard pour utiliser des objets en Delphi est :

```pascal
var
  MonObjet: TMonObjet;
begin
  MonObjet := TMonObjet.Create;
  try
    // Utilisation de l'objet
  finally
    MonObjet.Free;
  end;
end;
```

Le bloc `try-finally` garantit que `MonObjet.Free` sera appelé même si une exception se produit dans le bloc `try`. C'est essentiel pour éviter les fuites de mémoire.

## Constructeurs, héritage et polymorphisme

### Héritage et constructeurs

Lorsque vous créez une classe dérivée, vous devez généralement :
1. Créer un constructeur qui accepte les paramètres nécessaires
2. Appeler le constructeur de la classe parente avec les paramètres appropriés

```pascal
type
  TPersonne = class
  private
    FNom: string;
  public
    constructor Create(const ANom: string);
    // ...
  end;

  TEmploye = class(TPersonne)
  private
    FPoste: string;
  public
    constructor Create(const ANom, APoste: string);
    // ...
  end;

constructor TPersonne.Create(const ANom: string);
begin
  inherited Create;  // Appelle TObject.Create
  FNom := ANom;
end;

constructor TEmploye.Create(const ANom, APoste: string);
begin
  inherited Create(ANom);  // Appelle TPersonne.Create avec le nom
  FPoste := APoste;
end;
```

### Polymorphisme et destructeurs

Les destructeurs doivent toujours être déclarés comme `override` pour garantir le bon fonctionnement du polymorphisme :

```pascal
type
  TBase = class
  public
    destructor Destroy; override;  // override car TObject.Destroy est virtual
  end;

  TDerivee = class(TBase)
  public
    destructor Destroy; override;  // override car TBase.Destroy est override
  end;
```

Si vous oubliez `override`, le destructeur de votre classe dérivée ne sera pas appelé lorsque l'objet est référencé via une variable de la classe de base :

```pascal
var
  Base: TBase;
begin
  Base := TDerivee.Create;
  try
    // ...
  finally
    Base.Free;  // Sans override, seul TBase.Destroy serait appelé!
  end;
end;
```

## Initialisation d'objets complexes

Pour les objets complexes, il peut être utile de séparer la création et l'initialisation :

```pascal
type
  TApplication = class
  private
    FConfig: TConfig;
    FDatabase: TDatabase;
    FLogger: TLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    // ...
  end;

constructor TApplication.Create;
begin
  inherited Create;
  // Création des objets sans initialisation complexe
  FConfig := TConfig.Create;
  FDatabase := TDatabase.Create;
  FLogger := TLogger.Create;
end;

procedure TApplication.Initialize;
begin
  // Initialisation des objets, potentiellement avec des dépendances entre eux
  FConfig.LoadFromFile('config.ini');
  FLogger.SetLogLevel(FConfig.LogLevel);
  FDatabase.Connect(FConfig.DatabaseConnectionString);
end;

destructor TApplication.Destroy;
begin
  FLogger.Free;
  FDatabase.Free;
  FConfig.Free;
  inherited Destroy;
end;
```

Cette approche permet d'avoir un code plus clair et de gérer les dépendances entre objets plus facilement.

## Constructeurs de classes

Delphi supporte également les "constructeurs de classe", qui sont des méthodes statiques appelées sans instance :

```pascal
type
  TPersonne = class
  private
    FNom: string;
  public
    constructor Create(const ANom: string);

    // Constructeur de classe
    class function CreateDefaultPerson: TPersonne;
  end;

class function TPersonne.CreateDefaultPerson: TPersonne;
begin
  Result := TPersonne.Create('John Doe');
end;
```

Utilisation :

```pascal
var
  Personne: TPersonne;
begin
  Personne := TPersonne.CreateDefaultPerson;
  try
    // ...
  finally
    Personne.Free;
  end;
end;
```

Ces méthodes sont utiles pour encapsuler la logique de création complexe ou pour fournir des objets préconfigurés.

## Exemples concrets

### Classe simple avec constructeur et destructeur

```pascal
type
  TJoueur = class
  private
    FNom: string;
    FScore: Integer;
  public
    constructor Create(const ANom: string);
    destructor Destroy; override;

    procedure AugmenterScore(Points: Integer);

    property Nom: string read FNom;
    property Score: Integer read FScore;
  end;

constructor TJoueur.Create(const ANom: string);
begin
  inherited Create;

  if ANom = '' then
    raise Exception.Create('Le nom du joueur ne peut pas être vide');

  FNom := ANom;
  FScore := 0;
end;

destructor TJoueur.Destroy;
begin
  // Pas de ressources spéciales à libérer ici

  inherited Destroy;
end;

procedure TJoueur.AugmenterScore(Points: Integer);
begin
  if Points > 0 then
    FScore := FScore + Points;
end;
```

### Classe avec des ressources à libérer

```pascal
type
  TGestionnaireFichiers = class
  private
    FNomFichier: string;
    FFichier: TextFile;
    FFichierOuvert: Boolean;
    FLignes: TStringList;
  public
    constructor Create(const ANomFichier: string);
    destructor Destroy; override;

    procedure ChargerFichier;
    procedure SauvegarderFichier;
    function ObtenirLigne(Index: Integer): string;

    property NomFichier: string read FNomFichier;
  end;

constructor TGestionnaireFichiers.Create(const ANomFichier: string);
begin
  inherited Create;

  FNomFichier := ANomFichier;
  FFichierOuvert := False;

  // Création d'objets internes
  FLignes := TStringList.Create;
end;

destructor TGestionnaireFichiers.Destroy;
begin
  // Fermeture du fichier si nécessaire
  if FFichierOuvert then
    CloseFile(FFichier);

  // Libération des objets internes
  FLignes.Free;

  inherited Destroy;
end;

procedure TGestionnaireFichiers.ChargerFichier;
var
  Ligne: string;
begin
  if FileExists(FNomFichier) then
  begin
    AssignFile(FFichier, FNomFichier);
    Reset(FFichier);
    FFichierOuvert := True;

    FLignes.Clear;
    while not Eof(FFichier) do
    begin
      ReadLn(FFichier, Ligne);
      FLignes.Add(Ligne);
    end;

    CloseFile(FFichier);
    FFichierOuvert := False;
  end;
end;

procedure TGestionnaireFichiers.SauvegarderFichier;
var
  i: Integer;
begin
  AssignFile(FFichier, FNomFichier);
  Rewrite(FFichier);
  FFichierOuvert := True;

  for i := 0 to FLignes.Count - 1 do
    WriteLn(FFichier, FLignes[i]);

  CloseFile(FFichier);
  FFichierOuvert := False;
end;

function TGestionnaireFichiers.ObtenirLigne(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FLignes.Count) then
    Result := FLignes[Index]
  else
    Result := '';
end;
```

## Bonnes pratiques

1. **Validez les paramètres** dans le constructeur pour vous assurer que l'objet est dans un état valide :
   ```pascal
   constructor TProduit.Create(const ACode: string; APrix: Currency);
   begin
     inherited Create;

     if ACode = '' then
       raise Exception.Create('Le code produit ne peut pas être vide');

     if APrix < 0 then
       raise Exception.Create('Le prix ne peut pas être négatif');

     FCode := ACode;
     FPrix := APrix;
   end;
   ```

2. **Libérez les ressources** dans l'ordre inverse de leur création dans le destructeur :
   ```pascal
   destructor TMonObjet.Destroy;
   begin
     // Libération dans l'ordre inverse de la création
     FObjet3.Free;  // Créé en dernier
     FObjet2.Free;
     FObjet1.Free;  // Créé en premier

     inherited Destroy;
   end;
   ```

3. **Utilisez toujours `try-finally`** pour garantir la libération des objets :
   ```pascal
   var
     Obj: TMonObjet;
   begin
     Obj := TMonObjet.Create;
     try
       // Utilisation de l'objet
     finally
       Obj.Free;
     end;
   end;
   ```

4. **Initialisez les variables d'objets à `nil`** :
   ```pascal
   var
     Obj: TMonObjet;
   begin
     Obj := nil;  // Bonne pratique
     try
       Obj := TMonObjet.Create;
       // ...
     finally
       Obj.Free;  // Sûr même si Create a échoué
     end;
   end;
   ```

5. **N'oubliez pas `override`** pour les destructeurs :
   ```pascal
   destructor TMonObjet.Destroy; override;  // Ne pas oublier override
   ```

6. **Utilisez `FreeAndNil`** pour éviter les références pendantes :
   ```pascal
   FreeAndNil(MonObjet);  // Libère et met à nil
   ```

---

Les constructeurs et destructeurs sont essentiels pour garantir que vos objets sont correctement initialisés et que toutes les ressources qu'ils utilisent sont proprement libérées. En suivant les bonnes pratiques présentées dans cette section, vous éviterez les fuites de mémoire et autres problèmes liés à la gestion des objets.

Dans la prochaine section, nous explorerons le concept d'interfaces, qui offre une alternative à l'héritage pour créer des conceptions flexibles et réutilisables.

# 3.7.5 Interfaces

Les interfaces constituent un concept fondamental de la programmation orientée objet qui offre une alternative puissante à l'héritage. Dans cette section, nous allons découvrir ce que sont les interfaces, pourquoi elles sont utiles et comment les utiliser efficacement en Object Pascal.

## Qu'est-ce qu'une interface ?

Une interface est une sorte de "contrat" qui définit un ensemble de méthodes et de propriétés, sans fournir leur implémentation. Elle spécifie ce qu'un objet peut faire, sans préciser comment il le fait. Les classes qui "implémentent" une interface s'engagent à fournir le code pour toutes les méthodes et propriétés définies par cette interface.

Pour faire une analogie, si une classe est comme un plan de maison complet avec tous les détails, une interface est comme une liste d'exigences que la maison doit satisfaire (avoir une porte d'entrée, avoir une cuisine, etc.), sans spécifier comment ces exigences doivent être réalisées.

## Pourquoi utiliser des interfaces ?

Les interfaces offrent plusieurs avantages importants :

1. **Séparation entre spécification et implémentation** : Elles permettent de définir ce qu'un objet doit faire, sans se préoccuper de comment il le fait.

2. **Polymorphisme sans héritage** : Elles permettent à des classes non liées par héritage d'être utilisées de manière interchangeable.

3. **Implémentation multiple** : Contrairement à l'héritage où une classe ne peut hériter que d'une seule classe parente, une classe peut implémenter plusieurs interfaces.

4. **Extensibilité** : Elles facilitent l'ajout de nouvelles fonctionnalités sans modifier le code existant.

5. **Découplage** : Elles réduisent les dépendances entre les différentes parties de votre application.

## Déclaration d'une interface

En Object Pascal, une interface se déclare avec le mot-clé `interface` (à ne pas confondre avec la section `interface` d'une unité) :

```pascal
type
  IMonInterface = interface
    // Déclarations de méthodes et propriétés
  end;
```

Par convention, les noms d'interfaces commencent généralement par la lettre `I` (pour "Interface").

Exemple d'une interface simple :

```pascal
type
  IVehicule = interface
    procedure Demarrer;
    procedure Arreter;
    function GetVitesseMaximale: Integer;
    property VitesseMaximale: Integer read GetVitesseMaximale;
  end;
```

Cette interface définit deux procédures (`Demarrer` et `Arreter`), une fonction (`GetVitesseMaximale`) et une propriété en lecture seule (`VitesseMaximale`).

## GUID d'interface

En Delphi, il est recommandé d'associer un identifiant unique global (GUID) à chaque interface pour faciliter leur identification :

```pascal
type
  IVehicule = interface
    ['{A1B2C3D4-E5F6-4747-8899-AABBCCDDEEFF}']  // GUID unique
    procedure Demarrer;
    procedure Arreter;
    function GetVitesseMaximale: Integer;
    property VitesseMaximale: Integer read GetVitesseMaximale;
  end;
```

Pour générer un GUID, vous pouvez utiliser la combinaison de touches Ctrl+Shift+G dans l'éditeur Delphi.

## Implémentation d'une interface

Pour qu'une classe implémente une interface, elle doit :
1. Déclarer l'interface dans la clause `implements`
2. Fournir une implémentation pour toutes les méthodes et propriétés définies par l'interface

```pascal
type
  TVoiture = class(TObject, IVehicule)  // La classe implémente IVehicule
  private
    FVitesseMax: Integer;
    function GetVitesseMaximale: Integer;
  public
    constructor Create(AVitesseMax: Integer);

    // Implémentation des méthodes de l'interface
    procedure Demarrer;
    procedure Arreter;

    // Autres méthodes spécifiques à TVoiture
    procedure Klaxonner;
  end;

implementation

constructor TVoiture.Create(AVitesseMax: Integer);
begin
  inherited Create;
  FVitesseMax := AVitesseMax;
end;

procedure TVoiture.Demarrer;
begin
  ShowMessage('La voiture démarre');
end;

procedure TVoiture.Arreter;
begin
  ShowMessage('La voiture s''arrête');
end;

function TVoiture.GetVitesseMaximale: Integer;
begin
  Result := FVitesseMax;
end;

procedure TVoiture.Klaxonner;
begin
  ShowMessage('Beep beep!');
end;
```

## Utilisation des interfaces

Voici comment utiliser des objets via leurs interfaces :

```pascal
var
  Vehicule: IVehicule;
  Voiture: TVoiture;
begin
  Voiture := TVoiture.Create(200);

  // Assignation d'un objet à une variable d'interface
  Vehicule := Voiture;

  // Utilisation à travers l'interface
  Vehicule.Demarrer;
  ShowMessage('Vitesse maximale : ' + IntToStr(Vehicule.VitesseMaximale));
  Vehicule.Arreter;

  // Pas besoin de libérer la mémoire avec Free!
end;
```

Remarquez qu'il n'y a pas de `Voiture.Free` à la fin. C'est l'un des avantages des interfaces, comme nous allons le voir.

## Comptage de références automatique

Un des grands avantages des interfaces en Delphi est la gestion automatique de la mémoire grâce au comptage de références :

1. Quand un objet est assigné à une variable d'interface, son compteur de références est incrémenté
2. Quand une variable d'interface sort de portée ou est réassignée, le compteur est décrémenté
3. Quand le compteur atteint zéro, l'objet est automatiquement détruit

```pascal
procedure UtiliserVehicule;
var
  Vehicule: IVehicule;
begin
  Vehicule := TVoiture.Create(200);  // Compteur = 1

  Vehicule.Demarrer;
  Vehicule.Arreter;

  // À la fin de la procédure, Vehicule sort de portée,
  // le compteur passe à 0 et l'objet est détruit automatiquement
end;
```

Cette gestion automatique de la mémoire simplifie considérablement le code et réduit le risque de fuites mémoire.

## Attention au mélange d'objets et d'interfaces

Il faut être prudent lorsqu'on mélange les références d'objet traditionnelles et les interfaces :

```pascal
var
  Voiture: TVoiture;
  Vehicule: IVehicule;
begin
  Voiture := TVoiture.Create(200);  // Vous êtes responsable de libérer cet objet
  Vehicule := Voiture;             // Compteur = 1

  // ...

  Vehicule := nil;                 // Compteur = 0, l'objet est détruit

  // DANGER : Voiture pointe maintenant vers un objet détruit!
  Voiture.Klaxonner;  // Ceci va provoquer une erreur d'accès

  // Voiture.Free; // Ceci provoquerait une double libération
end;
```

Pour éviter ce problème, choisissez l'une de ces approches :
1. Utilisez uniquement des variables d'interface
2. Utilisez `FreeAndNil(Voiture)` avant d'assigner `nil` à `Vehicule`
3. Utilisez l'interface `_AddRef` et `_Release` (avancé)

## Interfaces multiples

Une classe peut implémenter plusieurs interfaces :

```pascal
type
  IVehicule = interface
    ['{A1B2C3D4-E5F6-4747-8899-AABBCCDDEEFF}']
    procedure Demarrer;
    procedure Arreter;
  end;

  IVehiculeTerrain = interface
    ['{F1E2D3C4-B5A6-4747-8899-FFEEDDCCBBAA}']
    procedure TraverserRiviere;
    function EstToutTerrain: Boolean;
  end;

  IJouet = interface
    ['{1234ABCD-5678-ABCD-EF01-23456789ABCD}']
    procedure Jouer;
    function GetTrancheAge: string;
    property TrancheAge: string read GetTrancheAge;
  end;

  TVoitureToutTerrain = class(TObject, IVehicule, IVehiculeTerrain, IJouet)
  private
    function GetTrancheAge: string;
  public
    // Implémentation de IVehicule
    procedure Demarrer;
    procedure Arreter;

    // Implémentation de IVehiculeTerrain
    procedure TraverserRiviere;
    function EstToutTerrain: Boolean;

    // Implémentation de IJouet
    procedure Jouer;
  end;
```

Cette flexibilité est un des grands avantages des interfaces par rapport à l'héritage.

## Conflit de noms et résolution explicite

Que se passe-t-il si deux interfaces définissent des méthodes avec le même nom ? Vous pouvez utiliser la résolution explicite :

```pascal
type
  IAnimal = interface
    procedure Manger;
  end;

  IMachine = interface
    procedure Manger;  // Même nom que dans IAnimal
  end;

  TRobotAnimal = class(TObject, IAnimal, IMachine)
  private
    // Résolution explicite des conflits
    procedure IAnimal.Manger = MangerCommeAnimal;
    procedure IMachine.Manger = MangerCommeMachine;

    // Implémentations réelles
    procedure MangerCommeAnimal;
    procedure MangerCommeMachine;
  public
    // Autres méthodes...
  end;

implementation

procedure TRobotAnimal.MangerCommeAnimal;
begin
  ShowMessage('Je mange de la nourriture');
end;

procedure TRobotAnimal.MangerCommeMachine;
begin
  ShowMessage('Je consomme de l''électricité');
end;
```

## Héritage d'interfaces

Les interfaces peuvent hériter d'autres interfaces :

```pascal
type
  IVehicule = interface
    procedure Demarrer;
    procedure Arreter;
  end;

  IVoiture = interface(IVehicule)  // Hérite de IVehicule
    procedure Klaxonner;
    function NombrePortes: Integer;
  end;
```

Une classe qui implémente `IVoiture` doit fournir une implémentation pour toutes les méthodes de `IVoiture` ET de `IVehicule`.

## Interfaces et polymorphisme

Les interfaces permettent un polymorphisme puissant, même entre classes sans relation d'héritage :

```pascal
procedure FaireDemarrerEtArreter(Vehicule: IVehicule);
begin
  Vehicule.Demarrer;
  Sleep(1000);  // Attendre 1 seconde
  Vehicule.Arreter;
end;

var
  Voiture: TVoiture;
  Moto: TMoto;
  TondeuseChevauchable: TTondeuse;  // Pas de relation d'héritage avec TVoiture ou TMoto
begin
  Voiture := TVoiture.Create(200);
  Moto := TMoto.Create(250);
  TondeuseChevauchable := TTondeuse.Create(20);

  try
    // Toutes ces classes implémentent IVehicule, donc cette fonction
    // fonctionne avec chacune d'elles
    FaireDemarrerEtArreter(Voiture);
    FaireDemarrerEtArreter(Moto);
    FaireDemarrerEtArreter(TondeuseChevauchable);
  finally
    Voiture.Free;
    Moto.Free;
    TondeuseChevauchable.Free;
  end;
end;
```

## Interface supports

Delphi fournit un mécanisme appelé "supports" pour vérifier si un objet implémente une interface spécifique :

```pascal
var
  Obj: TObject;
  Vehicule: IVehicule;
begin
  Obj := TVoiture.Create(200);
  try
    if Supports(Obj, IVehicule, Vehicule) then
    begin
      // Obj implémente IVehicule
      Vehicule.Demarrer;
      Vehicule.Arreter;
    end
    else
      ShowMessage('Cet objet n''est pas un véhicule');
  finally
    Obj.Free;
  end;
end;
```

La fonction `Supports` essaie de convertir l'objet en interface et renvoie `True` si c'est possible.

## Interfaces vs classes abstraites

Voici une comparaison entre les interfaces et les classes abstraites :

| Caractéristique | Interface | Classe abstraite |
|-----------------|-----------|------------------|
| Implémentation | Aucune implémentation | Peut contenir des implémentations |
| Héritage multiple | Une classe peut implémenter plusieurs interfaces | Une classe ne peut hériter que d'une seule classe |
| État | Pas d'état (pas de champs) | Peut avoir un état (champs) |
| Constructeur | Pas de constructeur | Peut avoir un constructeur |
| Gestion mémoire | Comptage de références automatique | Manuelle (Free) |

## Exemple concret : système de journalisation

Voici un exemple pratique montrant comment les interfaces peuvent être utilisées pour créer un système de journalisation flexible :

```pascal
type
  // Interface de journalisation
  ILogger = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Log(const AMessage: string);
    procedure LogError(const AError: string);
  end;

  // Implémentation de journalisation dans un fichier
  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
    FLogFile: TextFile;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    // Implémentation ILogger
    procedure Log(const AMessage: string);
    procedure LogError(const AError: string);
  end;

  // Implémentation de journalisation dans la console
  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    // Implémentation ILogger
    procedure Log(const AMessage: string);
    procedure LogError(const AError: string);
  end;

  // Implémentation de journalisation dans une base de données
  TDatabaseLogger = class(TInterfacedObject, ILogger)
  private
    FConnection: TDatabaseConnection;  // Hypothétique
  public
    constructor Create(AConnection: TDatabaseConnection);

    // Implémentation ILogger
    procedure Log(const AMessage: string);
    procedure LogError(const AError: string);
  end;

  // Notre application qui utilise la journalisation
  TMonApplication = class
  private
    FLogger: ILogger;
  public
    constructor Create(ALogger: ILogger);
    procedure Executer;
  end;

implementation

// Implémentation de TFileLogger
constructor TFileLogger.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  AssignFile(FLogFile, FFileName);
  if FileExists(FFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TFileLogger.Destroy;
begin
  CloseFile(FLogFile);
  inherited;
end;

procedure TFileLogger.Log(const AMessage: string);
begin
  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - INFO: ' + AMessage);
  Flush(FLogFile);
end;

procedure TFileLogger.LogError(const AError: string);
begin
  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ERROR: ' + AError);
  Flush(FLogFile);
end;

// Implémentation de TConsoleLogger
procedure TConsoleLogger.Log(const AMessage: string);
begin
  WriteLn('INFO: ' + AMessage);
end;

procedure TConsoleLogger.LogError(const AError: string);
begin
  WriteLn('ERROR: ' + AError);
end;

// Implémentation de TMonApplication
constructor TMonApplication.Create(ALogger: ILogger);
begin
  inherited Create;
  FLogger := ALogger;
end;

procedure TMonApplication.Executer;
begin
  FLogger.Log('Application démarrée');
  try
    // Code de l'application...
    FLogger.Log('Opération réussie');
  except
    on E: Exception do
      FLogger.LogError('Erreur: ' + E.Message);
  end;
  FLogger.Log('Application terminée');
end;
```

Utilisation :

```pascal
var
  FileLogger: ILogger;
  ConsoleLogger: ILogger;
  App1, App2: TMonApplication;
begin
  // Création de différents types de loggers
  FileLogger := TFileLogger.Create('app.log');
  ConsoleLogger := TConsoleLogger.Create;

  // Création de deux applications avec différents loggers
  App1 := TMonApplication.Create(FileLogger);
  App2 := TMonApplication.Create(ConsoleLogger);
  try
    App1.Executer;  // Journalise dans un fichier
    App2.Executer;  // Journalise dans la console
  finally
    App1.Free;
    App2.Free;
    // Pas besoin de libérer FileLogger et ConsoleLogger (interfaces)
  end;
end;
```

L'avantage de cette approche est que vous pouvez facilement changer le système de journalisation sans modifier le code de l'application.

## TInterfacedObject

Delphi fournit une classe de base `TInterfacedObject` qui implémente le comptage de références pour vous :

```pascal
type
  TMyClass = class(TInterfacedObject, IMyInterface)
    // Implémentation...
  end;
```

Cette classe gère automatiquement les méthodes `_AddRef` et `_Release` requises pour le comptage de références.

## Bonnes pratiques

1. **Nommez clairement vos interfaces** :
   - Utilisez le préfixe `I`
   - Choisissez des noms qui décrivent le rôle ou le comportement (`IComparable`, `IEnumerable`, `IDisposable`)

2. **Gardez les interfaces focalisées** :
   - Une interface devrait avoir une responsabilité unique
   - Préférez plusieurs petites interfaces plutôt qu'une grande

3. **Utilisez des GUID** :
   - Ajoutez toujours un GUID à vos interfaces

4. **Attention au mélange objets/interfaces** :
   - Évitez de mélanger les références d'objets et d'interfaces pour le même objet
   - Si vous devez le faire, soyez conscient des pièges liés au comptage de références

5. **Utilisez des interfaces pour le découplage** :
   - Les interfaces sont excellentes pour séparer les différentes parties de votre application
   - Elles facilitent les tests unitaires en permettant de remplacer facilement des implémentations

---

Les interfaces constituent un outil puissant dans votre boîte à outils de programmation orientée objet en Delphi. Elles vous permettent de créer des designs flexibles et extensibles, tout en profitant de la gestion automatique de la mémoire grâce au comptage de références. En maîtrisant les interfaces, vous pourrez créer des applications plus modulaires et plus faciles à maintenir.

Dans la prochaine section, nous explorerons la généricité, qui vous permettra de créer des classes et des méthodes travaillant avec différents types de données.

# 3.7.6 Généricité

La généricité est une fonctionnalité puissante qui permet de créer des classes, des méthodes et des procédures qui peuvent fonctionner avec différents types de données. C'est un concept qui peut sembler complexe au premier abord, mais qui offre d'énormes avantages en termes de réutilisation et de sécurité du code. Dans cette section, nous allons explorer la généricité en Object Pascal et voir comment l'utiliser efficacement.

## Qu'est-ce que la généricité ?

La généricité permet de créer du code qui fonctionne avec des types non spécifiés à l'avance. Au lieu d'écrire une classe ou une méthode pour chaque type de données, vous écrivez un modèle (template) qui peut être instancié pour n'importe quel type.

Imaginez que vous créez une boîte. Avec la généricité, vous ne créez pas une boîte spécifique pour les pommes, une autre pour les chaussures, etc. Vous créez un modèle de boîte qui peut contenir n'importe quel type d'objet. C'est comme si vous écriviez une fois le plan, puis que vous pouviez fabriquer différentes boîtes selon les besoins.

## Pourquoi utiliser la généricité ?

La généricité offre plusieurs avantages importants :

1. **Réutilisation du code** : Écrivez une fois, utilisez pour plusieurs types
2. **Sécurité de type** : Vérification des types à la compilation
3. **Performance** : Pas besoin de conversions de types à l'exécution
4. **Lisibilité** : Code plus clair et plus expressif
5. **Maintenabilité** : Moins de code à maintenir

## Classes génériques

Commençons par découvrir les classes génériques en Object Pascal. Une classe générique se déclare en ajoutant un ou plusieurs paramètres de type entre chevrons `<>` :

```pascal
type
  TBoite<T> = class
  private
    FContenu: T;
  public
    procedure AjouterContenu(const Valeur: T);
    function ObtenirContenu: T;
  end;
```

Dans cet exemple, `T` est un paramètre de type qui sera remplacé par un type réel lorsque la classe sera utilisée.

### Implémentation d'une classe générique

L'implémentation d'une classe générique est similaire à celle d'une classe normale :

```pascal
procedure TBoite<T>.AjouterContenu(const Valeur: T);
begin
  FContenu := Valeur;
end;

function TBoite<T>.ObtenirContenu: T;
begin
  Result := FContenu;
end;
```

### Utilisation d'une classe générique

Pour utiliser une classe générique, vous devez spécifier le type concret qui remplacera le paramètre de type :

```pascal
var
  BoiteEntiers: TBoite<Integer>;
  BoiteChaines: TBoite<string>;
begin
  // Création d'une boîte pour les entiers
  BoiteEntiers := TBoite<Integer>.Create;
  try
    BoiteEntiers.AjouterContenu(42);
    ShowMessage('Contenu : ' + IntToStr(BoiteEntiers.ObtenirContenu));
  finally
    BoiteEntiers.Free;
  end;

  // Création d'une boîte pour les chaînes
  BoiteChaines := TBoite<string>.Create;
  try
    BoiteChaines.AjouterContenu('Bonjour Delphi');
    ShowMessage('Contenu : ' + BoiteChaines.ObtenirContenu);
  finally
    BoiteChaines.Free;
  end;
end;
```

Notez que nous avons créé deux instances différentes de la même classe générique : une pour les entiers et une pour les chaînes.

## Méthodes génériques

En plus des classes, vous pouvez également créer des méthodes génériques :

```pascal
type
  TUtilitaires = class
  public
    class procedure Echanger<T>(var A, B: T);
  end;

class procedure TUtilitaires.Echanger<T>(var A, B: T);
var
  Temp: T;
begin
  Temp := A;
  A := B;
  B := Temp;
end;
```

Utilisation :

```pascal
var
  X, Y: Integer;
  S1, S2: string;
begin
  X := 10;
  Y := 20;
  TUtilitaires.Echanger<Integer>(X, Y);
  ShowMessage('X = ' + IntToStr(X) + ', Y = ' + IntToStr(Y));  // X = 20, Y = 10

  S1 := 'Bonjour';
  S2 := 'Delphi';
  TUtilitaires.Echanger<string>(S1, S2);
  ShowMessage('S1 = ' + S1 + ', S2 = ' + S2);  // S1 = Delphi, S2 = Bonjour
end;
```

Delphi peut souvent inférer le type à partir des paramètres, donc vous pouvez généralement omettre le type entre chevrons :

```pascal
TUtilitaires.Echanger(X, Y);  // Le compilateur déduit <Integer>
TUtilitaires.Echanger(S1, S2);  // Le compilateur déduit <string>
```

## Exemple pratique : liste générique

Créons un exemple un peu plus complexe avec une liste générique simple :

```pascal
type
  TListeGenerique<T> = class
  private
    FItems: array of T;
    FCount: Integer;
  public
    constructor Create;
    procedure AjouterItem(const Item: T);
    function ObtenirItem(Index: Integer): T;
    procedure SupprimerItem(Index: Integer);
    property Count: Integer read FCount;
  end;

constructor TListeGenerique<T>.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FItems, 0);
end;

procedure TListeGenerique<T>.AjouterItem(const Item: T);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

function TListeGenerique<T>.ObtenirItem(Index: Integer): T;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    raise Exception.Create('Index hors limites');
end;

procedure TListeGenerique<T>.SupprimerItem(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    // Décaler les éléments
    for I := Index to FCount - 2 do
      FItems[I] := FItems[I + 1];

    // Réduire le tableau
    Dec(FCount);
    SetLength(FItems, FCount);
  end
  else
    raise Exception.Create('Index hors limites');
end;
```

Utilisation :

```pascal
var
  ListeNombres: TListeGenerique<Integer>;
  ListeNoms: TListeGenerique<string>;
  I: Integer;
begin
  // Liste d'entiers
  ListeNombres := TListeGenerique<Integer>.Create;
  try
    ListeNombres.AjouterItem(10);
    ListeNombres.AjouterItem(20);
    ListeNombres.AjouterItem(30);

    for I := 0 to ListeNombres.Count - 1 do
      ShowMessage('Nombre ' + IntToStr(I) + ' : ' + IntToStr(ListeNombres.ObtenirItem(I)));

    ListeNombres.SupprimerItem(1);  // Supprime 20
    ShowMessage('Après suppression : ' + IntToStr(ListeNombres.Count) + ' éléments');
  finally
    ListeNombres.Free;
  end;

  // Liste de chaînes
  ListeNoms := TListeGenerique<string>.Create;
  try
    ListeNoms.AjouterItem('Alice');
    ListeNoms.AjouterItem('Bob');
    ListeNoms.AjouterItem('Charlie');

    for I := 0 to ListeNoms.Count - 1 do
      ShowMessage('Nom ' + IntToStr(I) + ' : ' + ListeNoms.ObtenirItem(I));
  finally
    ListeNoms.Free;
  end;
end;
```

## Contraintes de type

Par défaut, un paramètre de type générique peut être remplacé par n'importe quel type. Cependant, il est parfois nécessaire de restreindre les types possibles. C'est là qu'interviennent les contraintes de type.

### Types de contraintes

En Object Pascal, vous pouvez appliquer plusieurs types de contraintes :

#### Contrainte de classe

Limite le paramètre de type aux classes dérivées d'une classe spécifique :

```pascal
type
  TAnimal = class
    procedure Manger; virtual; abstract;
  end;

  TChien = class(TAnimal)
    procedure Manger; override;
  end;

  TChat = class(TAnimal)
    procedure Manger; override;
  end;

  // T doit hériter de TAnimal
  TZoo<T: TAnimal> = class
  private
    FAnimaux: array of T;
    FCount: Integer;
  public
    procedure AjouterAnimal(Animal: T);
    procedure NourrirAnimaux;
  end;

procedure TZoo<T>.NourrirAnimaux;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FAnimaux[I].Manger;  // On peut appeler Manger car T hérite de TAnimal
end;
```

#### Contrainte d'interface

Limite le paramètre de type aux classes qui implémentent une interface spécifique :

```pascal
type
  IComparable = interface
    function ComparerAvec(Autre: TObject): Integer;
  end;

  // T doit implémenter IComparable
  TTrieur<T: IComparable> = class
  public
    procedure Trier(var Items: array of T);
  end;
```

#### Contrainte de constructeur

Spécifie que le type doit avoir un constructeur par défaut :

```pascal
type
  // T doit avoir un constructeur par défaut
  TFabrique<T: constructor> = class
  public
    function CreerInstance: T;
  end;

function TFabrique<T>.CreerInstance: T;
begin
  Result := T.Create;  // On peut appeler le constructeur par défaut
end;
```

#### Contraintes multiples

Vous pouvez combiner plusieurs contraintes :

```pascal
type
  // T doit hériter de TPersonne et implémenter IComparable
  TGestionnaire<T: TPersonne, IComparable> = class
    // ...
  end;
```

## Classes génériques prédéfinies dans Delphi

Delphi offre plusieurs classes génériques prêtes à l'emploi dans l'unité `System.Generics.Collections` :

### TList\<T>

Une liste dynamique d'éléments de type T :

```pascal
uses
  System.Generics.Collections;

var
  ListeEntiers: TList<Integer>;
begin
  ListeEntiers := TList<Integer>.Create;
  try
    ListeEntiers.Add(10);
    ListeEntiers.Add(20);
    ListeEntiers.Add(30);

    ShowMessage('Premier élément : ' + IntToStr(ListeEntiers[0]));
    ShowMessage('Nombre d''éléments : ' + IntToStr(ListeEntiers.Count));

    ListeEntiers.Delete(1);  // Supprime 20

    if ListeEntiers.Contains(30) then
      ShowMessage('La liste contient 30');
  finally
    ListeEntiers.Free;
  end;
end;
```

### TDictionary\<TKey, TValue>

Une table de hachage associant des clés et des valeurs :

```pascal
var
  Dictionnaire: TDictionary<string, Integer>;
begin
  Dictionnaire := TDictionary<string, Integer>.Create;
  try
    // Ajout d'éléments
    Dictionnaire.Add('Un', 1);
    Dictionnaire.Add('Deux', 2);
    Dictionnaire.Add('Trois', 3);

    // Accès par clé
    ShowMessage('Deux = ' + IntToStr(Dictionnaire['Deux']));

    // Vérification d'existence
    if Dictionnaire.ContainsKey('Trois') then
      ShowMessage('La clé "Trois" existe');

    // Parcours des clés et valeurs
    for var Paire in Dictionnaire do
      ShowMessage(Paire.Key + ' = ' + IntToStr(Paire.Value));
  finally
    Dictionnaire.Free;
  end;
end;
```

### TQueue\<T>

Une file d'attente (premier entré, premier sorti) :

```pascal
var
  File: TQueue<string>;
begin
  File := TQueue<string>.Create;
  try
    File.Enqueue('Premier');
    File.Enqueue('Deuxième');
    File.Enqueue('Troisième');

    while File.Count > 0 do
      ShowMessage('Traitement de : ' + File.Dequeue);
  finally
    File.Free;
  end;
end;
```

### TStack\<T>

Une pile (dernier entré, premier sorti) :

```pascal
var
  Pile: TStack<Integer>;
begin
  Pile := TStack<Integer>.Create;
  try
    Pile.Push(10);
    Pile.Push(20);
    Pile.Push(30);

    while Pile.Count > 0 do
      ShowMessage('Dépilage : ' + IntToStr(Pile.Pop));
    // Affiche 30, puis 20, puis 10
  finally
    Pile.Free;
  end;
end;
```

## Exemple pratique : un gestionnaire de tâches générique

Voici un exemple plus complet montrant comment utiliser la généricité pour créer un gestionnaire de tâches flexible :

```pascal
type
  // Interface pour les tâches
  ITask = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Execute;
    function GetDescription: string;
    property Description: string read GetDescription;
  end;

  // Classe générique pour gérer une file d'attente de tâches
  TTaskManager<T: ITask> = class
  private
    FTasks: TQueue<T>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(const Task: T);
    procedure ProcessAllTasks;
    function GetPendingTasksCount: Integer;
  end;

  // Implémentation de tâches spécifiques
  TEmailTask = class(TInterfacedObject, ITask)
  private
    FRecipient: string;
    FSubject: string;
    FContent: string;
    function GetDescription: string;
  public
    constructor Create(const ARecipient, ASubject, AContent: string);
    procedure Execute;
  end;

  TPrintTask = class(TInterfacedObject, ITask)
  private
    FDocument: string;
    FPrinter: string;
    function GetDescription: string;
  public
    constructor Create(const ADocument, APrinter: string);
    procedure Execute;
  end;

{ TTaskManager<T> }

constructor TTaskManager<T>.Create;
begin
  inherited Create;
  FTasks := TQueue<T>.Create;
end;

destructor TTaskManager<T>.Destroy;
begin
  FTasks.Free;
  inherited;
end;

procedure TTaskManager<T>.AddTask(const Task: T);
begin
  FTasks.Enqueue(Task);
end;

procedure TTaskManager<T>.ProcessAllTasks;
var
  Task: T;
begin
  while FTasks.Count > 0 do
  begin
    Task := FTasks.Dequeue;
    try
      ShowMessage('Exécution de : ' + Task.Description);
      Task.Execute;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''exécution de la tâche : ' + E.Message);
    end;
  end;
end;

function TTaskManager<T>.GetPendingTasksCount: Integer;
begin
  Result := FTasks.Count;
end;

{ TEmailTask }

constructor TEmailTask.Create(const ARecipient, ASubject, AContent: string);
begin
  inherited Create;
  FRecipient := ARecipient;
  FSubject := ASubject;
  FContent := AContent;
end;

procedure TEmailTask.Execute;
begin
  // Code d'envoi d'email (simulation)
  ShowMessage('Email envoyé à ' + FRecipient);
end;

function TEmailTask.GetDescription: string;
begin
  Result := 'Email à ' + FRecipient + ' : ' + FSubject;
end;

{ TPrintTask }

constructor TPrintTask.Create(const ADocument, APrinter: string);
begin
  inherited Create;
  FDocument := ADocument;
  FPrinter := APrinter;
end;

procedure TPrintTask.Execute;
begin
  // Code d'impression (simulation)
  ShowMessage('Document ' + FDocument + ' imprimé sur ' + FPrinter);
end;

function TPrintTask.GetDescription: string;
begin
  Result := 'Impression de ' + FDocument + ' sur ' + FPrinter;
end;
```

Utilisation :

```pascal
var
  TaskManager: TTaskManager<ITask>;
  EmailTask: ITask;
  PrintTask: ITask;
begin
  TaskManager := TTaskManager<ITask>.Create;
  try
    // Création de tâches
    EmailTask := TEmailTask.Create('john.doe@example.com', 'Rappel', 'N''oubliez pas la réunion');
    PrintTask := TPrintTask.Create('Rapport.pdf', 'HP LaserJet');

    // Ajout des tâches au gestionnaire
    TaskManager.AddTask(EmailTask);
    TaskManager.AddTask(PrintTask);

    ShowMessage('Nombre de tâches en attente : ' + IntToStr(TaskManager.GetPendingTasksCount));

    // Traitement de toutes les tâches
    TaskManager.ProcessAllTasks;

    ShowMessage('Toutes les tâches ont été traitées');
  finally
    TaskManager.Free;
    // Pas besoin de libérer EmailTask et PrintTask (interfaces)
  end;
end;
```

## Limitations et considérations

### 1. Surcharge du code généré

La généricité peut générer beaucoup de code, car le compilateur crée une version distincte pour chaque instanciation de type. Cela peut augmenter la taille de votre exécutable.

### 2. Lisibilité

Les types génériques complexes peuvent être difficiles à lire et à comprendre, surtout avec plusieurs paramètres de type et contraintes.

### 3. Erreurs de compilation

Les erreurs dans le code générique peuvent être difficiles à détecter et à corriger, car elles apparaissent souvent lors de l'instanciation du type.

## Bonnes pratiques

1. **Utilisez des noms de paramètres de type significatifs** :
   - `T` est conventionnel pour un seul paramètre général
   - Pour plusieurs paramètres, utilisez des noms comme `TKey`, `TValue`, `TItem`

2. **Documentez les hypothèses** :
   - Ajoutez des commentaires expliquant ce que vous attendez des types génériques

3. **Utilisez des contraintes appropriées** :
   - Restreignez les paramètres de type quand c'est logique
   - Cela rend votre code plus sûr et plus clair

4. **Préférez les classes génériques prédéfinies** :
   - Utilisez `TList<T>`, `TDictionary<TKey, TValue>`, etc. quand possible
   - Elles sont bien testées et optimisées

5. **Évitez la généricité excessive** :
   - N'utilisez pas la généricité quand une solution simple suffit
   - Trop de paramètres de type rend le code difficile à comprendre

---

La généricité est un outil puissant qui permet de créer du code réutilisable et typé de manière sécurisée. En comprenant ses principes et en l'utilisant judicieusement, vous pouvez améliorer considérablement la qualité et la maintenabilité de vos applications Delphi.

Dans les sections suivantes de ce tutoriel, nous verrons comment utiliser ces concepts dans des applications réelles et comment ils s'intègrent avec d'autres fonctionnalités avancées de Delphi.
