# 12.10 Couverture de code et qualité

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction à la couverture de code

La couverture de code est une mesure qui permet d'évaluer quelle proportion de votre code est effectivement exécutée pendant les tests. C'est un indicateur important de la qualité de vos tests et, par extension, de la qualité globale de votre application. Une couverture élevée signifie que vos tests vérifient une plus grande partie de votre code, réduisant ainsi le risque de bugs non détectés.

## Pourquoi mesurer la couverture de code ?

La mesure de la couverture de code offre plusieurs avantages :

1. **Identifier les parties non testées** : Elle révèle les sections de code qui ne sont pas exécutées par vos tests.

2. **Améliorer la confiance** : Une couverture élevée donne davantage confiance dans la fiabilité du code.

3. **Guider le développement des tests** : Elle aide à cibler les efforts de test sur les zones les moins couvertes.

4. **Mesurer les progrès** : Elle permet de suivre l'amélioration de la qualité des tests au fil du temps.

5. **Faciliter la maintenance** : Un code bien testé est généralement plus facile à modifier sans introduire de régressions.

## Types de couverture de code

Il existe plusieurs types de métriques de couverture de code, chacune fournissant des informations différentes :

### 1. Couverture de lignes (Line Coverage)

La plus simple et la plus courante, elle mesure le pourcentage de lignes de code exécutées pendant les tests.

### 2. Couverture de branches (Branch Coverage)

Mesure si chaque branche conditionnelle (if/else, case, etc.) a été exécutée dans les deux sens (vrai/faux).

### 3. Couverture de chemins (Path Coverage)

Évalue si tous les chemins d'exécution possibles à travers le code ont été testés, ce qui est généralement difficile à atteindre à 100%.

### 4. Couverture de fonctions (Function Coverage)

Indique quelles fonctions ou méthodes ont été appelées pendant les tests.

### 5. Couverture de conditions (Condition Coverage)

Vérifie si chaque sous-expression dans une condition complexe a été évaluée à vrai et à faux.

## Outils de couverture de code pour Delphi

Plusieurs outils sont disponibles pour mesurer la couverture de code dans vos projets Delphi :

### 1. TestInsight

TestInsight est un outil populaire qui s'intègre à l'IDE Delphi et offre des fonctionnalités de couverture de code.

#### Installation et configuration :

1. Téléchargez TestInsight depuis [le site officiel](https://bitbucket.org/sglienke/testinsight/wiki/Home) ou via GetIt Package Manager
2. Installez le package pour votre version de Delphi
3. Redémarrez l'IDE

#### Utilisation avec votre projet de test :

1. Ouvrez votre projet de test
2. Assurez-vous que les informations de débogage sont activées (Project > Options > Linking > Include TD32 debug info)
3. Exécutez les tests via TestInsight (View > TestInsight Explorer)

![Interface de TestInsight](https://via.placeholder.com/600x350)

### 2. Delphi Code Coverage (DCC)

Un outil en ligne de commande gratuit et open source pour mesurer la couverture de code.

#### Utilisation :

```
CodeCoverage.exe -e YourApp.exe -m YourApp.map -u YourUnitNames -ife -spf UnitPrefixes
```

### 3. SonarQube avec plugin Delphi

SonarQube est une plateforme d'analyse de qualité de code qui peut également mesurer la couverture.

### 4. AQtime Pro

Un outil commercial puissant qui offre des fonctionnalités avancées d'analyse de performance et de couverture.

## Mesurer la couverture de code avec TestInsight

Voyons comment mesurer la couverture de code en utilisant TestInsight, l'un des outils les plus accessibles pour les développeurs Delphi :

### Étape 1 : Préparer votre projet de test

Assurez-vous que votre projet de test est correctement configuré :

1. Créez un projet de test DUnit ou DUnitX
2. Vérifiez que la génération d'informations de débogage détaillées est activée
3. Assurez-vous que l'optimisation est désactivée en mode Debug

```pascal
// Exemple d'une classe de test
[TestFixture]
TCalculatriceTests = class
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;

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
end;
```

### Étape 2 : Exécuter les tests avec couverture

1. Ouvrez l'explorateur TestInsight (View > TestInsight Explorer)
2. Cliquez sur l'icône "Run with Coverage" (ou activez l'option dans le menu)
3. Attendez que tous les tests soient exécutés

### Étape 3 : Analyser les résultats

TestInsight affiche un rapport détaillé avec :

- Le pourcentage global de couverture
- La couverture par unité
- La couverture par classe et méthode
- Des surlignages de code montrant les lignes exécutées et non exécutées

![Rapport de couverture TestInsight](https://via.placeholder.com/600x400)

Interprétation des couleurs dans le rapport :
- **Vert** : Code exécuté
- **Rouge** : Code non exécuté
- **Jaune/Orange** : Branches partiellement couvertes

## Exemple pratique : Améliorer la couverture de code

Supposons que nous avons une classe `TCalculatrice` avec quelques méthodes, et un rapport de couverture initial qui montre des lacunes. Voici comment nous pourrions améliorer la couverture :

### Code initial avec couverture incomplète

```pascal
// Unité Calculatrice.pas
unit Calculatrice;

interface

type
  TCalculatrice = class
  public
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
    function Multiplier(A, B: Integer): Integer;
    function Diviser(A, B: Double): Double;
    function Puissance(Base, Exposant: Double): Double;
    function EstPair(Valeur: Integer): Boolean;
  end;

implementation

uses
  System.SysUtils, System.Math;

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

function TCalculatrice.Diviser(A, B: Double): Double;
begin
  if B = 0 then
    raise EDivByZero.Create('Division par zéro');
  Result := A / B;
end;

function TCalculatrice.Puissance(Base, Exposant: Double): Double;
begin
  // Cette méthode n'est pas testée dans notre suite initiale
  Result := Power(Base, Exposant);
end;

function TCalculatrice.EstPair(Valeur: Integer): Boolean;
begin
  // Cette méthode est partiellement testée (seulement avec des valeurs paires)
  Result := (Valeur mod 2) = 0;
end;

end.
```

### Tests initiaux avec couverture incomplète

```pascal
// Unité CalculatriceTests.pas
unit CalculatriceTests;

interface

uses
  DUnitX.TestFramework, Calculatrice;

type
  [TestFixture]
  TCalculatriceTests = class
  private
    FCalculatrice: TCalculatrice;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

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
    // Manque des tests pour Puissance et un cas pour EstPair avec valeur impaire
  end;

implementation

procedure TCalculatriceTests.Setup;
begin
  FCalculatrice := TCalculatrice.Create;
end;

procedure TCalculatriceTests.TearDown;
begin
  FCalculatrice.Free;
end;

procedure TCalculatriceTests.TestAdditionner;
begin
  Assert.AreEqual(5, FCalculatrice.Additionner(2, 3));
end;

procedure TCalculatriceTests.TestSoustraire;
begin
  Assert.AreEqual(7, FCalculatrice.Soustraire(10, 3));
end;

procedure TCalculatriceTests.TestMultiplier;
begin
  Assert.AreEqual(12, FCalculatrice.Multiplier(3, 4));
end;

procedure TCalculatriceTests.TestDiviser;
begin
  Assert.AreEqual(2.5, FCalculatrice.Diviser(5, 2));
end;

procedure TCalculatriceTests.TestDiviserParZero;
begin
  Assert.WillRaise(
    procedure
    begin
      FCalculatrice.Diviser(10, 0);
    end,
    EDivByZero
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TCalculatriceTests);

end.
```

### Analyse du rapport de couverture initial

Après exécution des tests avec couverture, nous constatons :
- La méthode `Puissance` n'est jamais exécutée (0% de couverture)
- La méthode `EstPair` est partiellement couverte (seulement la branche "true" est testée)

### Amélioration des tests pour augmenter la couverture

```pascal
// Ajout de nouveaux tests
[Test]
procedure TCalculatriceTests.TestPuissance;
begin
  Assert.AreEqual(8.0, FCalculatrice.Puissance(2, 3));
  Assert.AreEqual(0.25, FCalculatrice.Puissance(2, -2));
  Assert.AreEqual(1.0, FCalculatrice.Puissance(5, 0));
end;

[Test]
procedure TCalculatriceTests.TestEstPair;
begin
  Assert.IsTrue(FCalculatrice.EstPair(2));
  Assert.IsFalse(FCalculatrice.EstPair(3)); // Teste la branche "false"
end;
```

### Nouvelle analyse après amélioration

Après avoir ajouté ces tests, la couverture s'améliore considérablement :
- La méthode `Puissance` est maintenant couverte à 100%
- La méthode `EstPair` est maintenant couverte à 100% (les deux branches sont testées)

## Stratégies pour améliorer la couverture de code

### 1. Identifier les points chauds

Commencez par les zones à faible couverture qui présentent le plus grand risque :
- Code complexe avec beaucoup de branches
- Code gérant des cas d'erreur
- Code critique pour la fonctionnalité de l'application

### 2. Tester les cas limites

Assurez-vous de tester les valeurs aux limites de chaque fonction :
- Valeurs nulles ou vides
- Valeurs minimales et maximales
- Cas spéciaux (comme la division par zéro)

### 3. Utiliser des tests paramétrés

Les tests paramétrés permettent de tester une fonction avec plusieurs ensembles de données :

```pascal
[Test]
[TestCase('Addition positive','2,3,5')]
[TestCase('Zéro et positif','0,5,5')]
[TestCase('Négatif et positif','-2,7,5')]
[TestCase('Deux négatifs','-2,-3,-5')]
procedure TCalculatriceTests.TestAdditionnerParametre(A, B, Resultat: Integer);
begin
  Assert.AreEqual(Resultat, FCalculatrice.Additionner(A, B));
end;
```

### 4. Refactoriser le code complexe

Si certaines méthodes sont difficiles à tester complètement, envisagez de les refactoriser :
- Divisez les méthodes longues en méthodes plus petites
- Isolez la logique complexe
- Réduisez le nombre de branches conditionnelles

### 5. Ajouter des tests de régression

Pour chaque bug corrigé, ajoutez un test qui reproduit le problème avant la correction :

```pascal
[Test]
procedure TValidatorTests.TestBugFixNumero123;
begin
  // Ce test vérifie la correction du bug #123
  // où l'entrée '12345' était incorrectement validée
  Assert.IsFalse(FValidator.EstCodePostalValide('12345'));
end;
```

## Au-delà de la couverture : métriques de qualité du code

La couverture de code n'est qu'une dimension de la qualité. D'autres métriques importantes incluent :

### 1. Complexité cyclomatique

Mesure la complexité d'une méthode en comptant le nombre de chemins d'exécution indépendants.

```pascal
// Fonction avec une complexité cyclomatique élevée (à éviter)
function CalculerPrix(Client: TClient; Produit: TProduit): Double;
begin
  if Client = nil then
    Exit(0);

  Result := Produit.PrixBase;

  if Client.EstPremium then
  begin
    if Produit.EnPromotion then
      Result := Result * 0.7
    else
      Result := Result * 0.9;
  end
  else
  begin
    if Produit.EnPromotion then
    begin
      if Client.PointsFidelite > 100 then
        Result := Result * 0.8
      else
        Result := Result * 0.85;
    end
    else
    begin
      if Client.PointsFidelite > 100 then
        Result := Result * 0.95;
    end;
  end;

  if Produit.EstPerissable and (Produit.JoursAvantExpiration < 5) then
    Result := Result * 0.7;

  if Client.CodePostal.StartsWith('75') then
    Result := Result + 5 // Frais de livraison Paris
  else
    Result := Result + 10; // Frais de livraison province
end;
```

Une refactorisation en méthodes plus petites réduirait considérablement la complexité et améliorerait la testabilité.

### 2. Dette technique

La dette technique représente le coût futur des raccourcis pris aujourd'hui. Les analyses de code peuvent vous aider à la quantifier.

### 3. Respect des standards de code

Vérifiez l'adhérence aux conventions et bonnes pratiques Delphi.

### 4. Duplication de code

Identifiez et éliminez le code dupliqué qui peut entraîner des incohérences lors des modifications.

## Outils d'analyse statique pour Delphi

L'analyse statique examine le code sans l'exécuter pour détecter les problèmes potentiels :

### 1. Embarcadero Audit

Disponible dans certaines éditions de Delphi, il analyse la qualité du code source.

### 2. Pascal Analyzer

Un outil tiers populaire qui détecte de nombreux problèmes potentiels.

### 3. SonarQube

Avec le plugin Delphi approprié, SonarQube offre une analyse complète de la qualité.

### 4. Delphi Code Auditor

Vérifie l'adhérence aux standards de codage et identifie les problèmes potentiels.

## Intégration dans un processus d'intégration continue

Pour maximiser l'efficacité, intégrez la couverture de code et les analyses de qualité dans votre processus d'intégration continue (CI) :

### Exemple avec Jenkins

```yaml
pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                bat '"C:\\Program Files (x86)\\Embarcadero\\Studio\\22.0\\bin\\rsvars.bat" && msbuild MonProjet.dproj /t:Build /p:Config=Debug'
            }
        }
        stage('Test') {
            steps {
                bat '"C:\\Program Files (x86)\\Embarcadero\\Studio\\22.0\\bin\\rsvars.bat" && msbuild TestProjet.dproj /t:Build /p:Config=Debug'
                bat 'CodeCoverage.exe -e TestProjet.exe -m TestProjet.map -uf Calculatrice.pas -xml'
            }
        }
        stage('Analyze') {
            steps {
                bat 'PascalAnalyzer.exe -p MonProjet.dproj -r html'
                bat 'SonarScanner.bat -D sonar.projectKey=MonProjet -D sonar.sources=. -D sonar.host.url=http://localhost:9000'
            }
        }
    }

    post {
        success {
            archiveArtifacts artifacts: 'Coverage/*.xml, Reports/*.html', fingerprint: true
            publishHTML([
                allowMissing: false,
                alwaysLinkToLastBuild: false,
                keepAll: true,
                reportDir: 'Reports',
                reportFiles: 'index.html',
                reportName: 'Code Quality Report'
            ])
        }
    }
}
```

## Nouvelles fonctionnalités de qualité de code dans Delphi 12

> 💡 **Nécessite Delphi 12 ou supérieur**

Delphi 12 Athens introduit plusieurs améliorations pour la qualité et l'analyse de code :

### 1. Code Insights intégré

Code Insights est un outil d'analyse statique intégré qui détecte les problèmes potentiels pendant que vous codez :

```pascal
procedure ExempleAvecProblemes;
var
  i: Integer;
  s: string;
begin
  // Code Insights signalera les problèmes suivants :
  i := i + 1; // Variable non initialisée
  s := 'Hello' // Point-virgule manquant
  if i = 5 then ; // Bloc vide après if
end;
```

### 2. Métriques de code améliorées

Delphi 12 fournit des métriques plus détaillées sur la qualité du code directement dans l'IDE.

### 3. Refactoring automatique

Des outils de refactoring plus puissants pour améliorer la qualité du code existant.

## Bonnes pratiques pour maintenir une haute qualité de code

### 1. Définir des standards de code

Établissez et documentez vos conventions de codage et utilisez des outils pour les appliquer automatiquement.

### 2. Revues de code régulières

Instaurez un processus de revue de code où les développeurs examinent le code des autres.

### 3. Tests continus

Maintenez une suite de tests complète et exécutez-la fréquemment.

### 4. Refactoring régulier

Consacrez du temps à améliorer le code existant, pas seulement à ajouter de nouvelles fonctionnalités.

### 5. Objectifs de couverture réalistes

Visez une couverture élevée mais réaliste, par exemple 80-90%, plutôt que de poursuivre obstinément les 100%.

### 6. Analyse de tendances

Suivez l'évolution des métriques de qualité au fil du temps pour identifier les tendances.

## Exemple concret : Analyse complète d'un projet

Pour illustrer l'application pratique de ces concepts, considérons un petit projet Delphi :

### Étape 1 : Mesurer la couverture initiale

```
Résultats initiaux:
- Couverture globale: 65%
- Units/Couverture:
  - Calculatrice.pas: 70%
  - Validateur.pas: 60%
  - GestionClients.pas: 55%
  - RapportPDF.pas: 75%
```

### Étape 2 : Analyser les métriques de qualité

```
Résultats de l'analyse statique:
- 12 méthodes avec complexité > 15
- 8 variables non utilisées
- 5 sections de code dupliqué
- 3 méthodes avec trop de paramètres (>7)
```

### Étape 3 : Plan d'amélioration

```
Plan d'action:
1. Augmenter la couverture de GestionClients.pas à 80%
2. Refactoriser les 5 méthodes les plus complexes
3. Éliminer tout le code dupliqué
4. Ajouter des tests pour les cas limites identifiés
```

### Étape 4 : Réévaluation après modifications

```
Résultats après amélioration:
- Couverture globale: 82% (+17%)
- Réduction de la complexité moyenne: -25%
- Code dupliqué: 0 occurences (-5)
- Nombre de bugs détectés en production: -30%
```

## Conclusion

La couverture de code et les métriques de qualité sont des outils précieux pour améliorer la fiabilité et la maintenabilité de vos applications Delphi. En intégrant ces pratiques dans votre processus de développement, vous construirez des applications plus robustes et plus faciles à maintenir.

Souvenez-vous que la couverture de code n'est pas une fin en soi, mais un outil pour identifier les zones potentiellement problématiques. Une couverture de 100% ne garantit pas l'absence de bugs, mais elle réduit considérablement leur probabilité.

En combinant tests unitaires, couverture de code, analyse statique et revues de code, vous créerez un environnement de développement qui favorise la qualité à chaque étape du processus.

Dans les sections suivantes de ce tutoriel, nous explorerons d'autres aspects du développement Delphi, en continuant à mettre l'accent sur les bonnes pratiques et la qualité du code.

⏭️ [Internationalisation et localisation](13-internationalisation-et-localisation/README.md)
