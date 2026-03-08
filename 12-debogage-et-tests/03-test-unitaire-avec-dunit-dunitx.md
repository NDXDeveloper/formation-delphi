🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.3 Test unitaire avec DUnit/DUnitX

## Introduction

Les tests unitaires représentent une approche moderne et professionnelle du développement logiciel. Plutôt que de tester manuellement votre application à chaque modification, vous écrivez du code qui teste automatiquement votre code. Cela peut sembler étrange au début pour un débutant : "Pourquoi écrire plus de code alors que j'ai déjà du mal à terminer mon application ?" La réponse est simple : les tests unitaires vous font gagner un temps considérable à long terme et vous donnent confiance dans la qualité de votre code.

Dans ce chapitre, nous allons découvrir **DUnit** et **DUnitX**, les deux frameworks de tests unitaires les plus utilisés dans l'écosystème Delphi.

## Qu'est-ce qu'un test unitaire ?

### Définition simple

Un test unitaire est un **petit morceau de code** qui vérifie qu'une **partie spécifique** de votre application (généralement une fonction ou une méthode) fonctionne correctement. On l'appelle "unitaire" parce qu'il teste une seule "unité" de code à la fois.

### Analogie pour comprendre

Imaginez que vous construisez une voiture. Avant d'assembler tous les composants, vous testez individuellement :
- Les freins fonctionnent-ils ?
- Le moteur démarre-t-il ?
- Les phares s'allument-ils ?

Les tests unitaires font exactement la même chose pour votre code : ils vérifient que chaque "pièce" fonctionne correctement avant de les assembler dans l'application complète.

### Exemple concret

Supposons que vous ayez écrit une fonction pour calculer la TVA :

```pascal
function CalculerTVA(MontantHT: Double; TauxTVA: Double): Double;  
begin  
  Result := MontantHT * (TauxTVA / 100);
end;
```

Un test unitaire pour cette fonction vérifierait :
- Avec 100€ HT et 20% de TVA, obtient-on bien 20€ de TVA ?
- Avec 0€ HT, obtient-on bien 0€ de TVA ?
- Avec un taux de 0%, obtient-on bien 0€ de TVA ?

## Pourquoi utiliser des tests unitaires ?

### Détecter les bugs rapidement

Sans tests unitaires, vous découvrez souvent les bugs lorsqu'un utilisateur les rencontre ou lorsque vous testez manuellement l'application entière. Avec des tests unitaires, vous détectez les problèmes **immédiatement** après avoir écrit (ou modifié) le code.

### Faciliter les modifications

Imaginez que vous devez modifier une fonction complexe utilisée à 50 endroits dans votre application. Comment être sûr que votre modification n'a rien cassé ? Avec des tests unitaires, vous exécutez simplement tous les tests. S'ils passent tous, vous avez une bonne confiance que tout fonctionne encore.

### Documentation vivante

Les tests unitaires servent de documentation. En lisant un test, on comprend immédiatement comment utiliser une fonction et ce qu'elle est censée faire. Contrairement aux commentaires qui peuvent devenir obsolètes, les tests restent à jour car ils échouent si le code change.

### Améliorer la conception du code

Écrire des tests vous force à penser à la conception de votre code. Un code difficile à tester est souvent un code mal conçu. Les tests vous poussent donc naturellement vers un meilleur design.

### Réduire la peur du changement

Avec une bonne couverture de tests, vous pouvez modifier, améliorer et refactoriser votre code en toute confiance. Les tests vous alerteront immédiatement si vous cassez quelque chose.

## DUnit vs DUnitX : Lequel choisir ?

Delphi propose deux frameworks de tests unitaires principaux. Comprendre leurs différences vous aidera à choisir le bon pour votre projet.

### DUnit

**DUnit** est le framework de tests unitaires historique de Delphi. Il existe depuis de nombreuses années et est très stable.

**Avantages :**
- Intégré nativement dans Delphi depuis longtemps
- Documentation abondante et communauté établie
- Stabilité éprouvée
- Interface graphique intégrée à l'IDE

**Inconvénients :**
- Architecture plus ancienne
- Moins de fonctionnalités modernes
- Pas de support multi-plateforme optimal

**Quand l'utiliser :**
- Projets VCL existants utilisant déjà DUnit
- Applications Windows uniquement
- Quand la stabilité à long terme est prioritaire

### DUnitX

**DUnitX** est la version moderne et améliorée, inspirée des frameworks de tests modernes comme NUnit (.NET) et JUnit (Java).

**Avantages :**
- Architecture moderne et flexible
- Support complet de FireMonkey (multi-plateforme)
- Attributs pour une syntaxe plus claire
- Fonctionnalités avancées (Setup/Teardown, tests paramétrés, etc.)
- Meilleure intégration avec les outils modernes de CI/CD
- Développement actif

**Inconvénients :**
- Moins de documentation que DUnit
- Courbe d'apprentissage légèrement plus raide pour certains concepts avancés

**Quand l'utiliser :**
- Nouveaux projets
- Applications multi-plateformes (FireMonkey)
- Quand vous voulez bénéficier des fonctionnalités modernes
- Intégration avec des systèmes d'intégration continue

### Recommandation pour débutants

Pour un **nouveau projet** ou si vous apprenez les tests unitaires, nous recommandons **DUnitX**. C'est le framework qui représente l'avenir de Delphi et il offre une meilleure expérience de développement. Le reste de ce chapitre se concentrera principalement sur DUnitX, tout en mentionnant DUnit quand c'est pertinent.

## Installation et configuration de DUnitX

### Vérifier si DUnitX est disponible

DUnitX est généralement inclus dans les versions récentes de Delphi. Pour vérifier :

1. Ouvrez Delphi
2. Allez dans **File > New > Other**
3. Dans la catégorie **Unit Tests**, cherchez **DUnitX Test Project**

Si vous le voyez, DUnitX est déjà installé et vous pouvez passer à la section suivante.

### Installation manuelle (si nécessaire)

Si DUnitX n'est pas disponible, vous pouvez l'installer via GetIt Package Manager :

1. Dans Delphi, ouvrez **Tools > GetIt Package Manager**
2. Recherchez "DUnitX"
3. Cliquez sur **Install**
4. Suivez les instructions à l'écran

Alternativement, vous pouvez télécharger DUnitX depuis GitHub (https://github.com/VSoftTechnologies/DUnitX) et l'ajouter manuellement à votre projet.

## Créer votre premier projet de tests

### Méthode 1 : Créer un projet de test dédié

C'est l'approche recommandée : créer un projet séparé uniquement pour les tests.

**Étapes :**

1. Dans Delphi, allez dans **File > New > Other**
2. Sélectionnez **Unit Tests > DUnitX Test Project**
3. Donnez un nom à votre projet, par exemple : `MonAppTests.dpr`
4. Choisissez le type d'application :
   - **Console Application** : Pour des tests rapides en ligne de commande
   - **VCL Application** : Pour une interface graphique sous Windows
   - **FireMonkey Application** : Pour une interface multi-plateforme

Pour débuter, choisissez **Console Application** car c'est le plus simple.

### Méthode 2 : Ajouter des tests à un projet existant

Vous pouvez également ajouter des unités de test à votre projet existant :

1. Dans votre projet, allez dans **File > New > Other**
2. Sélectionnez **Unit Tests > DUnitX Test Unit**
3. Donnez un nom à l'unité, par exemple : `TestCalculs.pas`
4. L'IDE génère un squelette de test prêt à l'emploi

## Structure d'un projet de tests DUnitX

Lorsque vous créez un projet de tests DUnitX, Delphi génère automatiquement du code de base. Comprendre cette structure est essentiel.

### Le fichier projet principal (.dpr)

```pascal
program MonAppTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.TestFramework;

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
begin
  try
    // Créer le runner de tests
    runner := TDUnitX.CreateRunner;

    // Ajouter un logger pour afficher les résultats
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);

    // Exécuter tous les tests
    results := runner.Execute;

    // Afficher un résumé
    System.WriteLn('Tests terminés');

    {$IFNDEF CI}
    System.WriteLn('Appuyez sur Entrée pour quitter');
    System.Readln;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

Ce fichier est le point d'entrée qui :
- Configure le framework de tests
- Exécute tous les tests trouvés dans le projet
- Affiche les résultats

**En tant que débutant, vous n'avez généralement pas besoin de modifier ce fichier.**

### Une unité de test simple

```pascal
unit TestCalculs;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestCalculs = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAddition;

    [Test]
    procedure TestMultiplication;
  end;

implementation

procedure TTestCalculs.Setup;  
begin  
  // Code exécuté AVANT chaque test
  // Utilisé pour préparer l'environnement de test
end;

procedure TTestCalculs.TearDown;  
begin  
  // Code exécuté APRÈS chaque test
  // Utilisé pour nettoyer l'environnement
end;

procedure TTestCalculs.TestAddition;  
begin  
  // Votre test ici
  Assert.AreEqual(4, 2 + 2, 'Addition incorrecte');
end;

procedure TTestCalculs.TestMultiplication;  
begin  
  // Votre test ici
  Assert.AreEqual(6, 2 * 3, 'Multiplication incorrecte');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalculs);

end.
```

## Comprendre les attributs DUnitX

DUnitX utilise des **attributs** (les éléments entre crochets comme `[Test]`) pour identifier et configurer les tests. Voici les principaux :

### [TestFixture]

Cet attribut marque une classe comme contenant des tests. Une "fixture" est simplement un ensemble de tests liés.

```pascal
[TestFixture]
TTestMaClasse = class
  // Vos tests ici
end;
```

**Analogie :** Pensez à une fixture comme à un classeur contenant plusieurs tests liés à un même sujet.

### [Test]

Marque une méthode comme étant un test à exécuter.

```pascal
[Test]
procedure MonTest;  
begin  
  // Code de test
end;
```

**Important :** Seules les méthodes marquées avec `[Test]` seront exécutées comme des tests.

### [Setup]

Méthode exécutée **avant chaque test** de la fixture. Utilisée pour préparer l'environnement de test.

```pascal
[Setup]
procedure Setup;  
begin  
  // Créer les objets nécessaires
  FCalculateur := TCalculateur.Create;
end;
```

### [TearDown]

Méthode exécutée **après chaque test** pour nettoyer les ressources.

```pascal
[TearDown]
procedure TearDown;  
begin  
  // Libérer les objets
  FCalculateur.Free;
end;
```

### [SetupFixture] et [TearDownFixture]

Exécutés **une seule fois** avant tous les tests de la fixture (Setup) et après tous les tests (TearDown).

```pascal
[SetupFixture]
procedure SetupFixture;  
begin  
  // Initialisation coûteuse faite une seule fois
end;

[TearDownFixture]
procedure TearDownFixture;  
begin  
  // Nettoyage final
end;
```

### [Ignore] et [Ignore('Raison')]

Désactive temporairement un test sans le supprimer.

```pascal
[Test]
[Ignore('Test désactivé pendant le refactoring')]
procedure TestEnCours;  
begin  
  // Ce test ne sera pas exécuté
end;
```

## Les assertions : Le cœur des tests

Les assertions sont les instructions qui vérifient que votre code produit les résultats attendus. Si une assertion échoue, le test échoue.

### Assert.AreEqual

Vérifie que deux valeurs sont égales.

```pascal
procedure TestAddition;  
begin  
  Assert.AreEqual(4, 2 + 2, 'Addition devrait donner 4');
  Assert.AreEqual('Bonjour', GetSalutation, 'Salutation incorrecte');
end;
```

**Syntaxe :** `Assert.AreEqual(ValeurAttendue, ValeurActuelle, Message)`

Le message est optionnel mais **fortement recommandé** car il vous aide à comprendre rapidement quel test a échoué et pourquoi.

### Assert.AreNotEqual

Vérifie que deux valeurs sont différentes.

```pascal
procedure TestGenererID;  
var  
  ID1, ID2: string;
begin
  ID1 := GenererNouvelID;
  ID2 := GenererNouvelID;
  Assert.AreNotEqual(ID1, ID2, 'Deux ID consécutifs doivent être différents');
end;
```

### Assert.IsTrue et Assert.IsFalse

Vérifient qu'une condition booléenne est vraie ou fausse.

```pascal
procedure TestEstMajeur;  
begin  
  Assert.IsTrue(EstMajeur(25), 'Personne de 25 ans doit être majeure');
  Assert.IsFalse(EstMajeur(15), 'Personne de 15 ans ne doit pas être majeure');
end;
```

### Assert.IsNull et Assert.IsNotNull

Pour vérifier si un objet ou pointeur est nil ou non.

```pascal
procedure TestCreerObjet;  
var  
  MonObjet: TMonObjet;
begin
  MonObjet := TMonObjet.Create;
  try
    Assert.IsNotNull(MonObjet, 'Objet devrait être créé');
  finally
    MonObjet.Free;
  end;
end;
```

### Assert.WillRaise

Vérifie qu'une exception spécifique est levée.

```pascal
procedure TestDivisionParZero;  
begin  
  Assert.WillRaise(
    procedure
    begin
      Diviser(10, 0);
    end,
    EDivByZero,
    'Division par zéro devrait lever une exception'
  );
end;
```

Ceci est particulièrement important pour tester que votre code gère correctement les situations d'erreur.

### Assert.Contains et Assert.StartsWith

Pour les chaînes de caractères :

```pascal
procedure TestMessage;  
var  
  Message: string;
begin
  Message := 'Bonjour le monde';
  Assert.Contains('monde', Message, 'Message devrait contenir "monde"');
  Assert.StartsWith('Bonjour', Message, 'Message devrait commencer par "Bonjour"');
end;
```

### Assertions pour les nombres à virgule

Pour les nombres à virgule flottante (Double, Single), utilisez une tolérance car la comparaison exacte peut échouer à cause de la précision limitée.

```pascal
procedure TestCalculPI;  
var  
  ValeurCalculee: Double;
begin
  ValeurCalculee := CalculerPI;
  Assert.AreEqual(3.14159, ValeurCalculee, 0.00001, 'PI approximatif');
end;
```

Le quatrième paramètre (0.00001) est la tolérance acceptée.

## Écrire vos premiers tests

### Test d'une fonction simple

Supposons que vous ayez cette fonction dans votre application :

```pascal
// Dans l'unité MesCalculs.pas
function Additionner(A, B: Integer): Integer;  
begin  
  Result := A + B;
end;
```

Voici comment la tester :

```pascal
// Dans l'unité TestMesCalculs.pas
unit TestMesCalculs;

interface

uses
  DUnitX.TestFramework,
  MesCalculs;  // Votre unité à tester

type
  [TestFixture]
  TTestCalculs = class
  public
    [Test]
    procedure TestAdditionPositifs;

    [Test]
    procedure TestAdditionNegatifs;

    [Test]
    procedure TestAdditionZero;
  end;

implementation

procedure TTestCalculs.TestAdditionPositifs;  
begin  
  Assert.AreEqual(5, Additionner(2, 3), 'Addition de nombres positifs');
  Assert.AreEqual(100, Additionner(50, 50), 'Addition de grands nombres');
end;

procedure TTestCalculs.TestAdditionNegatifs;  
begin  
  Assert.AreEqual(-5, Additionner(-2, -3), 'Addition de nombres négatifs');
  Assert.AreEqual(1, Additionner(-2, 3), 'Addition mixte positif/négatif');
end;

procedure TTestCalculs.TestAdditionZero;  
begin  
  Assert.AreEqual(5, Additionner(5, 0), 'Addition avec zéro');
  Assert.AreEqual(0, Additionner(0, 0), 'Addition de deux zéros');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalculs);
end.
```

**Notez** comment nous testons différents scénarios (cas nominaux, cas limites, valeurs spéciales).

### Test d'une classe

Testons maintenant une classe plus complexe :

```pascal
// Dans l'unité Calculateur.pas
type
  TCalculateur = class
  private
    FMemoire: Double;
  public
    constructor Create;
    procedure Effacer;
    procedure Ajouter(Valeur: Double);
    function ObtenirTotal: Double;
  end;

implementation

constructor TCalculateur.Create;  
begin  
  inherited;
  FMemoire := 0;
end;

procedure TCalculateur.Effacer;  
begin  
  FMemoire := 0;
end;

procedure TCalculateur.Ajouter(Valeur: Double);  
begin  
  FMemoire := FMemoire + Valeur;
end;

function TCalculateur.ObtenirTotal: Double;  
begin  
  Result := FMemoire;
end;
```

Et voici les tests :

```pascal
unit TestCalculateur;

interface

uses
  DUnitX.TestFramework,
  Calculateur;

type
  [TestFixture]
  TTestCalculateur = class
  private
    FCalculateur: TCalculateur;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreation;

    [Test]
    procedure TestAjout;

    [Test]
    procedure TestEffacer;

    [Test]
    procedure TestPlusieursOperations;
  end;

implementation

procedure TTestCalculateur.Setup;  
begin  
  // Créer une nouvelle instance avant chaque test
  FCalculateur := TCalculateur.Create;
end;

procedure TTestCalculateur.TearDown;  
begin  
  // Libérer l'instance après chaque test
  FCalculateur.Free;
end;

procedure TTestCalculateur.TestCreation;  
begin  
  Assert.IsNotNull(FCalculateur, 'Calculateur devrait être créé');
  Assert.AreEqual(0.0, FCalculateur.ObtenirTotal, 0.001, 'Total initial devrait être 0');
end;

procedure TTestCalculateur.TestAjout;  
begin  
  FCalculateur.Ajouter(5);
  Assert.AreEqual(5.0, FCalculateur.ObtenirTotal, 0.001, 'Après ajout de 5');

  FCalculateur.Ajouter(3);
  Assert.AreEqual(8.0, FCalculateur.ObtenirTotal, 0.001, 'Après ajout de 3');
end;

procedure TTestCalculateur.TestEffacer;  
begin  
  FCalculateur.Ajouter(10);
  FCalculateur.Effacer;
  Assert.AreEqual(0.0, FCalculateur.ObtenirTotal, 0.001, 'Total après effacement');
end;

procedure TTestCalculateur.TestPlusieursOperations;  
begin  
  FCalculateur.Ajouter(10);
  FCalculateur.Ajouter(20);
  FCalculateur.Ajouter(-5);
  Assert.AreEqual(25.0, FCalculateur.ObtenirTotal, 0.001, 'Série d''opérations');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalculateur);
end.
```

**Points importants :**

- Nous utilisons `Setup` et `TearDown` pour créer et détruire l'objet avant/après chaque test
- Cela garantit que chaque test commence avec un objet "propre"
- Nous testons non seulement les fonctionnalités individuelles, mais aussi leur combinaison

## Exécuter les tests

### En mode console

Si vous avez créé un projet de test console :

1. Compilez le projet (Shift+F9)
2. Exécutez-le (F9)
3. Une fenêtre de console s'ouvre et affiche les résultats des tests
4. Les tests réussis sont marqués en vert (ou avec [PASSED])
5. Les tests échoués sont marqués en rouge (ou avec [FAILED]) avec le message d'erreur

**Exemple de sortie :**

```
DUnitX - Version 1.0.0.0

Tests terminés
  TestCalculateur.TestCreation                [PASSED]
  TestCalculateur.TestAjout                   [PASSED]
  TestCalculateur.TestEffacer                 [PASSED]
  TestCalculateur.TestPlusieursOperations     [PASSED]

4 tests exécutés
4 tests réussis
0 tests échoués
```

### En mode GUI (interface graphique)

Si vous avez créé un projet avec interface graphique :

1. Exécutez le projet (F9)
2. Une fenêtre s'affiche avec une interface conviviale
3. Cliquez sur "Run All" pour exécuter tous les tests
4. Vous pouvez aussi exécuter des tests individuellement en les sélectionnant
5. Les résultats s'affichent avec des indicateurs visuels clairs

## Organisation des tests

### Une unité de test par unité de code

Pour garder vos tests organisés, créez généralement une unité de test pour chaque unité de code de votre application.

**Exemple :**

```
MonApplication/
├── Source/
│   ├── Calculateur.pas
│   ├── GestionClients.pas
│   └── Utils.pas
└── Tests/
    ├── TestCalculateur.pas
    ├── TestGestionClients.pas
    └── TestUtils.pas
```

### Nommage cohérent

Adoptez une convention de nommage claire :

- **Unités de test :** `Test` + nom de l'unité testée (ex: `TestCalculateur.pas`)
- **Classes de test :** `TTest` + nom de la classe testée (ex: `TTestCalculateur`)
- **Méthodes de test :** `Test` + nom de ce qui est testé (ex: `TestAddition`)

### Grouper les tests par fonctionnalité

Vous pouvez créer plusieurs fixtures pour tester différents aspects d'une même unité :

```pascal
[TestFixture]
TTestCalculateurBase = class
  // Tests des opérations de base
end;

[TestFixture]
TTestCalculateurAvance = class
  // Tests des opérations avancées
end;
```

## Bonnes pratiques pour écrire des tests

### 1. Un test doit tester une seule chose

Chaque test devrait vérifier un comportement spécifique. Si un test échoue, vous devriez immédiatement savoir quelle fonctionnalité est cassée.

**Mauvais exemple :**

```pascal
procedure TestTout;  
begin  
  Assert.AreEqual(4, Additionner(2, 2));
  Assert.AreEqual(6, Multiplier(2, 3));
  Assert.AreEqual(10, Diviser(100, 10));
  // Trop de choses testées en une fois !
end;
```

**Bon exemple :**

```pascal
procedure TestAddition;  
begin  
  Assert.AreEqual(4, Additionner(2, 2));
end;

procedure TestMultiplication;  
begin  
  Assert.AreEqual(6, Multiplier(2, 3));
end;

procedure TestDivision;  
begin  
  Assert.AreEqual(10, Diviser(100, 10));
end;
```

### 2. Les tests doivent être indépendants

Chaque test doit pouvoir s'exécuter seul, sans dépendre du résultat d'un autre test. C'est pourquoi `Setup` et `TearDown` sont si importants : ils réinitialisent l'environnement pour chaque test.

### 3. Utilisez des messages descriptifs

Toujours inclure un message explicatif dans vos assertions :

```pascal
// Mauvais
Assert.AreEqual(10, Calculer);

// Bon
Assert.AreEqual(10, Calculer, 'Le calcul avec les paramètres par défaut devrait donner 10');
```

### 4. Testez les cas limites (edge cases)

Ne testez pas seulement les cas normaux, testez aussi :

- Les valeurs nulles ou vides
- Les valeurs minimales et maximales
- Les valeurs négatives
- Les cas d'erreur

```pascal
procedure TestDivision;  
begin  
  // Cas normal
  Assert.AreEqual(5, Diviser(10, 2));

  // Cas limites
  Assert.AreEqual(0, Diviser(0, 5), 'Zéro divisé par un nombre');
  Assert.WillRaise(procedure begin Diviser(10, 0); end, EDivByZero, 'Division par zéro');
end;
```

### 5. Les tests doivent être rapides

Un test unitaire devrait s'exécuter en quelques millisecondes. Si vos tests sont lents, vous serez tenté de ne pas les exécuter régulièrement.

**Évitez dans les tests unitaires :**
- Les accès à des bases de données réelles
- Les appels réseau
- Les opérations sur des fichiers
- Les délais (Sleep)

Pour ces cas, utilisez des **mocks** (objets simulés) ou des tests d'intégration séparés.

### 6. Donnez des noms explicites à vos tests

Le nom d'un test devrait décrire clairement ce qu'il teste :

```pascal
// Moins clair
procedure Test1;

// Plus clair
procedure TestAdditionDeuxNombresPositifs;  
procedure TestDivisionParZeroLeveException;  
procedure TestListeVideRetourneFalseAContient;  
```

### 7. Arrangez, Agissez, Affirmez (AAA Pattern)

Organisez vos tests selon le pattern AAA :

```pascal
procedure TestAjouterClient;  
var  
  Gestionnaire: TGestionnaireClients;
  NombreInitial, NombreApres: Integer;
begin
  // Arrange (Préparer) : Configurer l'environnement de test
  Gestionnaire := TGestionnaireClients.Create;
  try
    NombreInitial := Gestionnaire.ObtenirNombreClients;

    // Act (Agir) : Exécuter l'action à tester
    Gestionnaire.AjouterClient('Jean Dupont');
    NombreApres := Gestionnaire.ObtenirNombreClients;

    // Assert (Affirmer) : Vérifier le résultat
    Assert.AreEqual(NombreInitial + 1, NombreApres, 'Un client devrait être ajouté');
  finally
    Gestionnaire.Free;
  end;
end;
```

## Comprendre les résultats des tests

### Test réussi (Passed)

Toutes les assertions ont réussi. Le code fonctionne comme prévu pour ce scénario de test.

### Test échoué (Failed)

Au moins une assertion a échoué. Le message d'erreur vous indique :
- Quelle assertion a échoué
- La valeur attendue
- La valeur obtenue
- Votre message personnalisé

**Exemple de message d'échec :**

```
TestAddition FAILED  
Expected: 5  
But was: 4  
Message: Addition devrait donner 5  
```

### Test ignoré (Ignored)

Le test a été marqué avec l'attribut `[Ignore]` et n'a pas été exécuté.

### Erreur (Error)

Une exception non gérée s'est produite pendant l'exécution du test (ce n'est pas la même chose qu'une assertion qui échoue).

## Intégration continue et automatisation

### Pourquoi automatiser les tests ?

L'idéal est d'exécuter automatiquement tous vos tests :
- À chaque commit dans votre système de contrôle de version
- Avant chaque déploiement
- Régulièrement (par exemple, chaque nuit)

### Exécution en ligne de commande

Les projets de tests DUnitX console peuvent être exécutés depuis la ligne de commande, ce qui facilite leur intégration dans des scripts :

```
MonAppTests.exe --exit-behavior:continue
```

Le code de sortie indique si les tests ont réussi (0) ou échoué (non-zéro).

### Génération de rapports

DUnitX peut générer des rapports dans différents formats (XML, JSON) utilisables par des systèmes d'intégration continue comme Jenkins, TeamCity, ou Azure DevOps.

## DUnit : Les bases (pour référence)

Si vous travaillez sur un projet existant utilisant DUnit, voici les concepts de base :

### Structure d'un test DUnit

```pascal
unit TestCalculs;

interface

uses
  TestFramework;  // Unité principale de DUnit

type
  TTestCalculs = class(TTestCase)
  published
    procedure TestAddition;
    procedure TestMultiplication;
  end;

implementation

procedure TTestCalculs.TestAddition;  
begin  
  CheckEquals(4, 2 + 2, 'Addition incorrecte');
end;

procedure TTestCalculs.TestMultiplication;  
begin  
  CheckEquals(6, 2 * 3, 'Multiplication incorrecte');
end;

initialization
  RegisterTest(TTestCalculs.Suite);
end.
```

### Principales différences avec DUnitX

- **Pas d'attributs** : Les tests sont identifiés par la section `published`
- **Méthodes Check*** au lieu d'`Assert.*` : `CheckEquals`, `CheckTrue`, `CheckNotNull`, etc.
- **Héritage de TTestCase** : Les classes de test héritent de `TTestCase`
- **Registration différente** : `RegisterTest(TTestCalculs.Suite)`

## Conseils pour débutants

### Commencez petit

Ne vous lancez pas immédiatement dans des tests complexes. Commencez par tester des fonctions simples pour vous familiariser avec le framework.

### Écrivez les tests en même temps que le code

Idéalement, écrivez un test avant ou juste après avoir écrit une fonction. Ne laissez pas s'accumuler du code non testé.

### Ne visez pas 100% de couverture dès le début

Avoir des tests est mieux que de ne pas en avoir du tout. Commencez par tester les parties critiques de votre application et augmentez progressivement la couverture.

### Relancez vos tests régulièrement

Prenez l'habitude d'exécuter vos tests fréquemment :
- Après chaque modification significative
- Avant de commiter votre code
- Au moins une fois par jour

### Les tests qui échouent sont précieux

Un test qui échoue vous alerte immédiatement qu'un problème existe. C'est exactement le but ! Ne soyez pas frustré, mais reconnaissant d'avoir détecté le bug rapidement.

### Documentez vos tests

Si un test vérifie un cas complexe ou un bug spécifique, ajoutez un commentaire expliquant le contexte.

```pascal
[Test]
procedure TestCalculMensuel;  
begin  
  // Ce test vérifie le correctif pour le bug #1234
  // où les mois de février étaient mal calculés
  Assert.AreEqual(28, CalculerJoursMois(2, 2023));
end;
```

## Dépannage des problèmes courants

### "No tests found" ou "Aucun test trouvé"

**Causes possibles :**
- Vous avez oublié l'attribut `[Test]` sur vos méthodes
- Vous avez oublié l'attribut `[TestFixture]` sur votre classe
- La section `initialization` avec `RegisterTestFixture` est manquante
- L'unité de test n'est pas référencée dans le projet

### Les tests s'exécutent mais échouent tous

**Vérifiez :**
- Que vous testez bien ce que vous pensez tester
- L'ordre des paramètres dans `Assert.AreEqual` (attendu, puis actuel)
- Que vos objets sont correctement créés dans `Setup`

### Erreur "Access Violation" pendant les tests

**Causes fréquentes :**
- Un objet n'a pas été créé (nil)
- Tentative d'utiliser un objet après l'avoir libéré
- Problème de gestion de mémoire dans votre code

### Les tests passent individuellement mais échouent en groupe

Cela indique généralement que vos tests ne sont **pas indépendants** :
- Un test modifie un état global que les autres utilisent
- Des ressources ne sont pas correctement nettoyées dans `TearDown`
- Des singletons ou variables globales causent des dépendances

## Conclusion

Les tests unitaires avec DUnit/DUnitX sont un investissement qui rapporte rapidement. Bien qu'ils demandent un effort initial, ils vous font gagner énormément de temps à long terme en détectant les bugs tôt et en vous donnant confiance lors des modifications.

**Points clés à retenir :**

- Les tests unitaires vérifient automatiquement que votre code fonctionne
- DUnitX est le framework moderne recommandé pour les nouveaux projets
- Utilisez les attributs `[TestFixture]` et `[Test]` pour structurer vos tests
- Les assertions (`Assert.*`) vérifient les résultats attendus
- `Setup` et `TearDown` préparent et nettoient l'environnement de test
- Testez non seulement les cas normaux, mais aussi les cas limites et les erreurs
- Les tests doivent être rapides, indépendants et clairement nommés
- Exécutez vos tests régulièrement pendant le développement

En maîtrisant les tests unitaires, vous rejoignez les rangs des développeurs professionnels qui produisent du code fiable et maintenable. C'est une compétence qui vous servira tout au long de votre carrière de développeur Delphi.

⏭️ [Profilage et optimisation des performances](/12-debogage-et-tests/04-profilage-et-optimisation-des-performances.md)
