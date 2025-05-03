# 12.3 Test unitaire avec DUnit/DUnitX

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux tests unitaires

Les tests unitaires constituent une pratique fondamentale du développement logiciel moderne. Ils vous permettent de vérifier que chaque "unité" de votre code (généralement une fonction ou une méthode) fonctionne correctement de façon isolée. Pour les développeurs Delphi, deux frameworks principaux sont disponibles : DUnit (plus ancien) et DUnitX (plus moderne et recommandé).

## Pourquoi faire des tests unitaires ?

Avant de plonger dans les détails techniques, comprenons pourquoi les tests unitaires sont si importants :

1. **Détection précoce des bugs** : Identifiez les problèmes au plus tôt dans le cycle de développement
2. **Confiance lors des modifications** : Modifiez votre code sans craindre de casser des fonctionnalités existantes
3. **Documentation vivante** : Les tests montrent comment votre code est censé fonctionner
4. **Conception améliorée** : Écrire des tests vous pousse à créer un code plus modulaire et mieux structuré
5. **Facilitation du refactoring** : Changez l'implémentation interne tout en gardant le comportement externe

## DUnit vs DUnitX

### DUnit
- Framework de test plus ancien
- Fourni avec les anciennes versions de Delphi
- Moins activement maintenu
- Interface utilisateur plus basique

### DUnitX
- Framework de test moderne et activement maintenu
- Support natif dans les versions récentes de Delphi
- Compatible avec les fonctionnalités récentes du langage (génériques, attributs...)
- Plus flexible et extensible
- Inspiré des frameworks de test modernes comme NUnit et xUnit

> 💡 **Recommandation** : Pour les nouveaux projets, privilégiez DUnitX qui est plus moderne et mieux supporté. DUnit reste utile pour maintenir des projets existants.

## Installation de DUnitX

DUnitX est inclus dans Delphi depuis la version 10.2 Tokyo, mais vous pouvez aussi l'obtenir depuis GitHub pour les versions antérieures.

### Pour Delphi 10.2 et versions ultérieures :

1. Créez un nouveau projet
2. Allez dans **File > New > Other**
3. Choisissez **DUnitX > DUnitX Test Project**
4. Suivez les instructions de l'assistant

### Pour les versions antérieures :

1. Téléchargez DUnitX depuis GitHub : [https://github.com/VSoftTechnologies/DUnitX](https://github.com/VSoftTechnologies/DUnitX)
2. Clonez ou téléchargez le projet
3. Ajoutez le répertoire source à votre chemin de recherche Delphi

## Création d'un premier projet de test avec DUnitX

Voyons comment créer et exécuter un projet de test simple avec DUnitX :

### Étape 1 : Créer un projet de test

1. Allez dans **File > New > Other**
2. Sélectionnez **DUnitX > DUnitX Test Project**
3. Donnez un nom à votre projet (par exemple, "MonProjetTests")
4. Cliquez sur **OK**

Delphi crée automatiquement la structure de base du projet avec une classe de test d'exemple.

### Étape 2 : Comprendre le code généré

Le projet créé contient généralement ces éléments :

```pascal
// Fichier principal généré par l'assistant
program MonProjetTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  TestMaClasse in 'TestMaClasse.pas';

{$R *.res}

begin
  // Code d'initialisation des tests...
end.

// Fichier de test généré (TestMaClasse.pas)
unit TestMaClasse;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestMaClasse = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMethod1;
  end;

implementation

procedure TTestMaClasse.Setup;
begin
end;

procedure TTestMaClasse.TearDown;
begin
end;

procedure TTestMaClasse.TestMethod1;
begin
  // Un test d'exemple qui vérifie que TRUE est TRUE
  Assert.IsTrue(True);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMaClasse);

end.
```

### Étape 3 : Écrire votre premier test réel

Modifions l'exemple pour tester une fonction simple. Supposons que nous ayons une fonction qui additionne deux nombres :

1. Créez une unité pour le code à tester :

```pascal
// Fichier MathUtils.pas
unit MathUtils;

interface

function Additionner(A, B: Integer): Integer;

implementation

function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

end.
```

2. Modifiez votre fichier de test pour utiliser cette fonction :

```pascal
unit TestMathUtils;

interface

uses
  DUnitX.TestFramework,
  MathUtils; // Notre unité à tester

type
  [TestFixture]
  TTestMathUtils = class
  public
    [Test]
    procedure TestAdditionner;
  end;

implementation

procedure TTestMathUtils.TestAdditionner;
begin
  // Vérifie que 2 + 3 = 5
  Assert.AreEqual(5, Additionner(2, 3), 'Additionner(2, 3) devrait retourner 5');

  // Vérifie que -1 + 1 = 0
  Assert.AreEqual(0, Additionner(-1, 1), 'Additionner(-1, 1) devrait retourner 0');

  // Vérifie que 0 + 0 = 0
  Assert.AreEqual(0, Additionner(0, 0), 'Additionner(0, 0) devrait retourner 0');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMathUtils);

end.
```

3. Assurez-vous d'ajouter votre unité de test à la clause `uses` du fichier projet.

### Étape 4 : Exécuter les tests

1. Compilez le projet en appuyant sur **F9**
2. Les tests s'exécuteront automatiquement dans une fenêtre console
3. Vous verrez les résultats de chaque test (succès ou échec)

## Structure d'un test DUnitX

### Attributs importants

DUnitX utilise des attributs (annotations) pour définir la structure des tests :

- `[TestFixture]` : Indique qu'une classe contient des tests
- `[Test]` : Marque une méthode comme étant un test à exécuter
- `[Setup]` : Code à exécuter avant chaque test
- `[TearDown]` : Code à exécuter après chaque test
- `[TestCase]` : Définit des données pour un test paramétré
- `[Ignore]` : Indique qu'un test doit être ignoré

### Cycle de vie d'un test

Pour une classe de test, le cycle d'exécution est généralement :

1. Construction de l'instance de la classe de test
2. Exécution de la méthode `[Setup]` (si présente)
3. Exécution de la méthode de test `[Test]`
4. Exécution de la méthode `[TearDown]` (si présente)
5. Destruction de l'instance de la classe de test

## Assertions

Les assertions sont le cœur des tests unitaires. DUnitX fournit de nombreuses méthodes d'assertion via la classe `Assert` :

### Assertions de base

```pascal
// Vérification d'égalité
Assert.AreEqual(Attendu, Réel, 'Message d'erreur');

// Vérification booléenne
Assert.IsTrue(Condition, 'Message d'erreur');
Assert.IsFalse(Condition, 'Message d'erreur');

// Vérification de nullité
Assert.IsNull(Valeur, 'Message d'erreur');
Assert.IsNotNull(Valeur, 'Message d'erreur');

// Vérification d'exception
Assert.WillRaise(
  procedure
  begin
    // Code qui devrait lever une exception
  end,
  ETypeException,
  'Le code devrait lever une exception'
);
```

### Assertions pour types spécifiques

```pascal
// Comparaison de chaînes (insensible à la casse)
Assert.AreEqualIgnoreCase('texte', Variable, 'Message d'erreur');

// Comparaison de flottants (avec tolérance)
Assert.AreEqual(3.14, MaVariable, 0.001, 'Message d'erreur'); // Tolère une différence de 0.001

// Vérification de contenu
Assert.Contains('abc', 'abcdef', 'La chaîne devrait contenir "abc"');
```

## Exemple plus complet

Imaginons une classe `TCalculatrice` avec diverses opérations :

```pascal
// Calculatrice.pas
unit Calculatrice;

interface

type
  TCalculatrice = class
  public
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
    function Multiplier(A, B: Integer): Integer;
    function Diviser(A, B: Integer): Double;
  end;

implementation

function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TCalculatrice.Soustraire(A, B: Integer): Integer;
begin
  Result := A - B;
end;

function TCalculatrice.Multiplier(A, B: Integer): Integer;
begin
  Result := A * B;
end;

function TCalculatrice.Diviser(A, B: Integer): Double;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro');

  Result := A / B;
end;

end.
```

Voici comment nous pourrions tester cette classe :

```pascal
// TestCalculatrice.pas
unit TestCalculatrice;

interface

uses
  DUnitX.TestFramework,
  Calculatrice;

type
  [TestFixture]
  TTestCalculatrice = class
  private
    FCalculatrice: TCalculatrice;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Tests des différentes opérations
    [Test]
    procedure TestAdditionner;
    [Test]
    procedure TestSoustraire;
    [Test]
    procedure TestMultiplier;
    [Test]
    procedure TestDiviser;
    [Test]
    procedure TestDiviserParZero;

    // Test paramétré
    [Test]
    [TestCase('Addition simple','2,3,5')]
    [TestCase('Nombres négatifs','-1,5,4')]
    [TestCase('Résultat négatif','3,-8,-5')]
    procedure TestAdditionnerCasDivers(A, B, Resultat: Integer);
  end;

implementation

uses
  System.SysUtils;

procedure TTestCalculatrice.Setup;
begin
  // Préparation avant chaque test
  FCalculatrice := TCalculatrice.Create;
end;

procedure TTestCalculatrice.TearDown;
begin
  // Nettoyage après chaque test
  FCalculatrice.Free;
end;

procedure TTestCalculatrice.TestAdditionner;
begin
  Assert.AreEqual(5, FCalculatrice.Additionner(2, 3), 'Addition incorrecte');
end;

procedure TTestCalculatrice.TestSoustraire;
begin
  Assert.AreEqual(7, FCalculatrice.Soustraire(10, 3), 'Soustraction incorrecte');
end;

procedure TTestCalculatrice.TestMultiplier;
begin
  Assert.AreEqual(12, FCalculatrice.Multiplier(3, 4), 'Multiplication incorrecte');
end;

procedure TTestCalculatrice.TestDiviser;
begin
  Assert.AreEqual(2.5, FCalculatrice.Diviser(5, 2), 0.001, 'Division incorrecte');
end;

procedure TTestCalculatrice.TestDiviserParZero;
begin
  // Teste qu'une exception est bien levée lors d'une division par zéro
  Assert.WillRaise(
    procedure
    begin
      FCalculatrice.Diviser(10, 0);
    end,
    Exception,
    'La division par zéro devrait lever une exception'
  );
end;

procedure TTestCalculatrice.TestAdditionnerCasDivers(A, B, Resultat: Integer);
begin
  // Test paramétré qui sera exécuté plusieurs fois avec différentes valeurs
  Assert.AreEqual(Resultat, FCalculatrice.Additionner(A, B),
    Format('Addition incorrect pour %d + %d', [A, B]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalculatrice);

end.
```

## Fonctionnalités avancées de DUnitX

### Tests paramétrés

Les tests paramétrés vous permettent d'exécuter la même logique de test avec différentes données :

```pascal
[Test]
[TestCase('Cas 1','1,2,3')] // Format: A,B,Résultat attendu
[TestCase('Cas 2','5,5,10')]
[TestCase('Cas 3','0,0,0')]
procedure TestAddition(A, B, Expected: Integer);
begin
  Assert.AreEqual(Expected, Additionner(A, B));
end;
```

### Tests avec fixtures de configuration

Pour des tests qui partagent la même configuration :

```pascal
[TestFixture]
TTestAvecFixture = class
private
  FConnexionBD: TConnexion;
public
  [SetupFixture]  // Exécuté une seule fois avant tous les tests de cette classe
  procedure SetupFixture;

  [TearDownFixture]  // Exécuté une seule fois après tous les tests de cette classe
  procedure TearDownFixture;

  [Setup]  // Exécuté avant chaque test
  procedure Setup;

  [TearDown]  // Exécuté après chaque test
  procedure TearDown;

  // Tests...
end;
```

### Catégories de tests

Vous pouvez organiser vos tests en catégories pour les exécuter sélectivement :

```pascal
[Test]
[Category('Rapide')]
procedure TestRapide;
begin
  // Test rapide...
end;

[Test]
[Category('Lent')]
procedure TestLent;
begin
  // Test lent...
end;
```

Vous pouvez ensuite exécuter uniquement les tests d'une catégorie spécifique en configurant le runner.

### Mocks et stubs

Pour tester des unités isolément, vous pouvez utiliser des frameworks de mock comme Delphi-Mocks :

```pascal
// Exemple simplifié
interface

type
  IServiceBase = interface
    function ObtenirDonnees: string;
  end;

  TClasseTestee = class
  private
    FService: IServiceBase;
  public
    constructor Create(AService: IServiceBase);
    function TraiterDonnees: string;
  end;

// Dans le test
procedure TestAvecMock;
var
  MockService: TMock<IServiceBase>;
  ClasseTestee: TClasseTestee;
  Resultat: string;
begin
  // Création du mock
  MockService := TMock<IServiceBase>.Create;
  MockService.Setup.WillReturn('Données de test').When.ObtenirDonnees;

  // Utilisation du mock
  ClasseTestee := TClasseTestee.Create(MockService);
  try
    Resultat := ClasseTestee.TraiterDonnees;
    Assert.AreEqual('Résultat attendu', Resultat);
  finally
    ClasseTestee.Free;
  end;
end;
```

> 💡 **Nécessite Delphi 12 ou supérieur pour les fonctionnalités avancées de mocking**

## Bonnes pratiques en matière de tests unitaires

### 1. Gardez les tests indépendants
Chaque test doit pouvoir s'exécuter indépendamment des autres. Évitez les dépendances entre tests.

### 2. Suivez le modèle AAA (Arrange-Act-Assert)
- **Arrange** : Préparez les données et conditions préalables
- **Act** : Exécutez l'opération à tester
- **Assert** : Vérifiez le résultat

```pascal
procedure TestSomething;
begin
  // Arrange - Préparation
  var Input1 := 2;
  var Input2 := 3;

  // Act - Action
  var Result := Additionner(Input1, Input2);

  // Assert - Vérification
  Assert.AreEqual(5, Result);
end;
```

### 3. Test One Condition per Test
Testez une seule chose par test. Cela rend les tests plus clairs et facilite le diagnostic en cas d'échec.

### 4. Utilisez des noms significatifs
Nommez vos tests de manière descriptive. Un bon format est `Test[Méthode]_[Scénario]_[RésultatAttendu]`.

```pascal
procedure TestDiviser_AvecZero_LeveException;
```

### 5. Pensez à la maintenabilité
Évitez la duplication de code dans les tests en utilisant des méthodes d'aide et le pattern Setup/TearDown.

## Intégration avec un système CI/CD

DUnitX peut générer des rapports XML au format NUnit, ce qui permet une intégration facile avec les systèmes d'intégration continue comme Jenkins, GitLab CI, ou Azure DevOps.

```pascal
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
begin
  // Création du runner de test
  Runner := TDUnitX.CreateRunner;

  // Ajout des loggers
  Logger := TDUnitXConsoleLogger.Create(true);
  Runner.AddLogger(Logger);

  // Logger format NUnit pour CI
  NUnitLogger := TDUnitXXMLNUnitFileLogger.Create('TestResults.xml');
  Runner.AddLogger(NUnitLogger);

  // Exécution des tests
  Results := Runner.Execute;

  // Code de sortie pour le système CI
  System.ExitCode := if Results.AllPassed then 0 else 1;
end;
```

## Conclusion

Les tests unitaires sont un investissement qui paie rapidement en termes de qualité logicielle et de productivité. DUnitX offre un cadre moderne et puissant pour implémenter ces tests dans vos projets Delphi.

Commencez simplement avec quelques tests sur vos fonctions les plus critiques, puis étendez progressivement votre couverture. Avec le temps, vous développerez une suite de tests complète qui vous donnera confiance lors de l'évolution de votre code.

Dans la prochaine section, nous explorerons le profilage et l'optimisation des performances de vos applications Delphi.

⏭️ [Profilage et optimisation des performances](12-debogage-et-tests/04-profilage-et-optimisation-des-performances.md)
