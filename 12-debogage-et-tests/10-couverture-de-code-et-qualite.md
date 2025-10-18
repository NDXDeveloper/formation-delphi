🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.10 Couverture de code et qualité

## Introduction

Imaginez que vous êtes professeur et que vous devez évaluer un étudiant. Vous lui posez 10 questions sur un cours de 100 leçons. L'étudiant répond parfaitement aux 10 questions. Pouvez-vous affirmer qu'il maîtrise tout le cours ? Non, car vous n'avez testé que 10% du contenu.

C'est exactement le même principe avec la **couverture de code** : elle mesure quelle proportion de votre code est réellement exécutée par vos tests. Si vos tests ne couvrent que 10% de votre code, alors 90% de votre code n'est pas testé et peut contenir des bugs.

Pour un débutant, comprendre la couverture de code et les métriques de qualité est essentiel pour créer des applications robustes et maintenables. Ce chapitre vous guide à travers ces concepts cruciaux du développement professionnel.

## Qu'est-ce que la couverture de code ?

### Définition simple

La **couverture de code** (code coverage en anglais) est une mesure qui indique quel pourcentage de votre code source est exécuté lorsque vos tests s'exécutent.

**Formule basique :**

```
Couverture = (Lignes exécutées / Lignes totales) × 100%
```

### Analogie pour comprendre

Pensez à un manuel d'utilisation d'un appareil :
- **Couverture 100%** : Vous avez lu et testé chaque instruction du manuel
- **Couverture 50%** : Vous n'avez lu que la moitié du manuel
- **Couverture 0%** : Vous n'avez jamais ouvert le manuel

Plus la couverture est haute, plus vous avez de chances d'avoir trouvé les bugs.

### Exemple visuel simple

```pascal
procedure CalculerRemise(Prix: Currency; EstMembre: Boolean);
begin
  if Prix > 100 then        // Ligne 1 - Toujours exécutée dans les tests
    if EstMembre then       // Ligne 2 - Toujours exécutée dans les tests
      Prix := Prix * 0.9    // Ligne 3 - Exécutée seulement si EstMembre = True
    else
      Prix := Prix * 0.95;  // Ligne 4 - JAMAIS exécutée dans nos tests !

  AfficherPrix(Prix);       // Ligne 5 - Toujours exécutée dans les tests
end;
```

**Test existant :**

```pascal
procedure TestRemiseMembre;
begin
  CalculerRemise(150, True);  // Exécute lignes 1, 2, 3, 5
end;
```

**Résultat :**
- Lignes exécutées : 4 sur 5
- Couverture : 80%
- **Ligne 4 non testée** : Risque de bug non détecté !

## Types de couverture

Il existe plusieurs niveaux de granularité pour mesurer la couverture.

### 1. Couverture de lignes (Line Coverage)

C'est la plus simple : quelle proportion des lignes de code est exécutée ?

```pascal
procedure Exemple;
var
  X: Integer;
begin
  X := 10;           // Ligne 1
  if X > 5 then      // Ligne 2
    WriteLn('Grand') // Ligne 3
  else
    WriteLn('Petit'); // Ligne 4
  WriteLn('Fin');    // Ligne 5
end;
```

**Test :**

```pascal
Exemple;  // X = 10, donc X > 5 est vrai
```

**Résultat :**
- Lignes exécutées : 1, 2, 3, 5 (4 lignes)
- Ligne non exécutée : 4
- Couverture : 80%

### 2. Couverture de branches (Branch Coverage)

Mesure si toutes les branches de décision (if, case, etc.) ont été testées.

```pascal
procedure ExempleCondition(A, B: Integer);
begin
  if (A > 0) and (B > 0) then  // 2 branches possibles : vrai ou faux
    WriteLn('Positifs')
  else
    WriteLn('Négatifs');
end;
```

**Pour 100% de couverture de branches, il faut :**
1. Un test où la condition est vraie
2. Un test où la condition est fausse

**Tests complets :**

```pascal
ExempleCondition(1, 1);   // Condition vraie
ExempleCondition(-1, 1);  // Condition fausse
```

### 3. Couverture de chemins (Path Coverage)

Mesure si tous les chemins d'exécution possibles ont été testés.

```pascal
procedure ExempleChemins(A, B: Integer);
begin
  if A > 0 then       // Condition 1
    WriteLn('A+');

  if B > 0 then       // Condition 2
    WriteLn('B+');
end;
```

**Chemins possibles :**
1. A > 0 ET B > 0 : Les deux messages
2. A > 0 ET B ≤ 0 : Seulement "A+"
3. A ≤ 0 ET B > 0 : Seulement "B+"
4. A ≤ 0 ET B ≤ 0 : Aucun message

**Pour 100% de couverture de chemins :**

```pascal
ExempleChemins(1, 1);    // Chemin 1
ExempleChemins(1, -1);   // Chemin 2
ExempleChemins(-1, 1);   // Chemin 3
ExempleChemins(-1, -1);  // Chemin 4
```

### 4. Couverture de conditions (Condition Coverage)

Vérifie que chaque condition booléenne a été évaluée à la fois à vrai et à faux.

```pascal
if (A > 0) and (B > 0) then  // Deux conditions : (A > 0) et (B > 0)
  // ...
```

**Pour 100% :**
- Test où A > 0 est vrai ET B > 0 est vrai
- Test où A > 0 est faux
- Test où B > 0 est faux

### Comparaison des types

| Type | Granularité | Difficulté | Exhaustivité |
|------|-------------|------------|--------------|
| **Lignes** | Basique | Facile | Bonne pour commencer |
| **Branches** | Moyenne | Moyenne | Recommandée |
| **Conditions** | Fine | Élevée | Très complète |
| **Chemins** | Très fine | Très élevée | Exhaustive (parfois impossible) |

**Recommandation pour débutants :** Commencez par la couverture de lignes, puis progressez vers les branches.

## Pourquoi la couverture de code est importante

### 1. Identifier le code non testé

La couverture vous montre exactement quelles parties de votre code ne sont pas testées.

**Sans couverture :**
"J'ai écrit des tests, donc mon code est probablement correct."

**Avec couverture :**
"Mes tests couvrent 85% du code. Je vois que les 15% restants ne sont pas testés. Ce sont les fonctions d'erreur et les cas limites."

### 2. Trouver le code mort

Le code qui n'est jamais exécuté, même par les tests, est probablement du **code mort** (dead code) qui peut être supprimé.

```pascal
procedure AncienneFonction;
begin
  // Couverture : 0%
  // Jamais appelée, même par l'application
  // → Peut être supprimée
end;
```

### 3. Prioriser les efforts de test

Si vous avez un temps limité, concentrez-vous sur le code le plus critique qui n'est pas encore couvert.

**Exemple de rapport :**

```
Unité              Couverture
----------------------------
ClientService      95%   ← Bien testé
OrderProcessor     45%   ← À améliorer !
PaymentGateway     30%   ← Critique et peu testé !
Logger             100%  ← Parfait
```

Priorité : **PaymentGateway** (critique pour le business ET peu testé).

### 4. Mesurer l'amélioration

Suivre la couverture au fil du temps :

```
Semaine 1 : 35%
Semaine 2 : 42%
Semaine 3 : 58%
Semaine 4 : 67%
```

Progression visible et motivante !

### 5. Prévenir les régressions

Quand vous ajoutez du nouveau code, la couverture globale peut baisser si vous n'ajoutez pas de tests.

**Exemple :**

```
Avant ajout nouvelle fonctionnalité : 75% de couverture
Après ajout de 500 lignes sans tests : 60% de couverture
```

**Alarme** : La couverture a baissé → Ajoutez des tests !

## Outils de couverture pour Delphi

### Delphi Code Coverage

**Delphi Code Coverage** est l'outil open-source le plus populaire pour mesurer la couverture de code en Delphi.

**Caractéristiques :**
- Gratuit et open-source
- Support de DUnit et DUnitX
- Rapports HTML détaillés
- Ligne de commande et intégration CI/CD
- Support Windows 32/64 bit

**Installation :**

1. Télécharger depuis GitHub : https://github.com/DelphiCodeCoverage/DelphiCodeCoverage
2. Extraire l'archive
3. Placer `CodeCoverage.exe` dans votre PATH ou dans le dossier du projet

**Utilisation basique :**

```cmd
CodeCoverage.exe -e MonAppTests.exe -m MonApp.exe -a
```

**Paramètres :**
- `-e` : Exécutable de tests
- `-m` : Module à analyser (votre application)
- `-a` : Générer tous les rapports

**Sortie :**
- Fichiers HTML dans le dossier `CodeCoverage_html/`
- Ouvrez `index.html` dans un navigateur

**Rapport HTML typique :**

```
MyApp - Couverture : 73.5%

Unités                    Lignes    Couverture
-----------------------------------------------
ClientManager.pas         250       95.2%  ✓
OrderProcessor.pas        180       68.3%  ⚠
PaymentService.pas        120       45.8%  ✗
Utils.pas                 80        100%   ✓
```

### Intégration avec les tests

**Configuration recommandée :**

Créez un fichier batch `RunTestsWithCoverage.bat` :

```batch
@echo off
echo Exécution des tests avec couverture...

REM Nettoyer les anciens rapports
if exist CodeCoverage_html rmdir /s /q CodeCoverage_html

REM Exécuter les tests avec couverture
CodeCoverage.exe ^
  -e MonAppTests.exe ^
  -m MonApp.exe ^
  -a ^
  -ife ^
  -spf *.pas ^
  -uf System ^
  -uf Vcl ^
  -uf Winapi

REM Ouvrir le rapport
start CodeCoverage_html\index.html

echo Terminé !
pause
```

**Paramètres additionnels :**
- `-ife` : Exclure les fichiers d'exemple
- `-spf` : N'inclure que les fichiers .pas
- `-uf` : Exclure les unités système (System, Vcl, etc.)

### Autres outils

**AQtime Pro**
- Outil commercial très complet
- Couverture de code + profiling
- Interface graphique riche
- Intégration IDE
- **Coût** : Élevé

**TestInsight avec couverture**
- Plugin IDE pour Delphi
- Visualisation de la couverture dans l'éditeur
- Lignes couvertes en vert, non couvertes en rouge
- Intégration fluide

**SmartBear Automated QA**
- Suite complète de test
- Inclut couverture de code
- Pour les grandes entreprises

**Comparaison :**

| Outil | Prix | Facilité | Fonctionnalités | Recommandation |
|-------|------|----------|-----------------|----------------|
| **Delphi Code Coverage** | Gratuit | Moyenne | Bonnes | ★★★★★ Débutants |
| **AQtime** | $$$ | Facile | Excellentes | ★★★★☆ Pros |
| **TestInsight** | Gratuit | Très facile | Basiques | ★★★★☆ Débutants |

## Interpréter les rapports de couverture

### Rapport par unité

```
Unité: ClientManager.pas
Couverture globale: 85.2%

Ligne    Code                               État
---------------------------------------------------
10       constructor TClientManager.Create   ✓
11       begin                               ✓
12         FClients := TList<TClient>.Create ✓
13       end;                                ✓
...
45       if Client.Age < 18 then            ✓
46         raise Exception.Create('Mineur') ✗ Non couvert
47       else                                ✓
48         ProcessClient(Client);            ✓
```

**Analyse :**
- Ligne 46 n'est jamais exécutée
- Signifie : Aucun test ne vérifie le cas où Age < 18
- **Action** : Ajouter un test pour ce cas

### Rapport par méthode

```
TClientManager.AddClient - Couverture: 100%
TClientManager.RemoveClient - Couverture: 100%
TClientManager.ValidateAge - Couverture: 50%  ← Attention !
TClientManager.SendEmail - Couverture: 0%    ← Jamais testé !
```

**Actions :**
1. Améliorer les tests de `ValidateAge`
2. Créer des tests pour `SendEmail`

### Rapport de branches

```
Méthode: ProcessOrder

if (Order.Total > 1000) and (Customer.IsPremium) then
   Branch 1 (True):  ✓ Couvert
   Branch 2 (False): ✗ Non couvert

→ Manque un test où la condition est fausse
```

**Test à ajouter :**

```pascal
procedure TestProcessOrderNonPremium;
begin
  // Order.Total > 1000 mais Customer.IsPremium = False
end;
```

### Code couvert mais non testé

**Important :** 100% de couverture ne signifie pas que le code est correct !

```pascal
function Diviser(A, B: Integer): Integer;
begin
  Result := A div B;  // Couvert à 100%
end;

// Test
procedure TestDiviser;
begin
  Assert.AreEqual(5, Diviser(10, 2));  // ✓ Ligne exécutée
end;
```

**Problème :** Le test ne vérifie pas la division par zéro !

```pascal
Diviser(10, 0);  // CRASH non détecté !
```

La ligne est "couverte" mais le cas d'erreur n'est pas testé.

## Objectifs de couverture

### Quel objectif viser ?

**Il n'y a pas de chiffre magique**, mais voici des repères :

| Couverture | Évaluation | Contexte |
|------------|------------|----------|
| **< 20%** | Insuffisant | Projet en début de vie ou legacy sans tests |
| **20-40%** | Faible | Tests basiques, beaucoup de code non testé |
| **40-60%** | Moyen | Bon début, continuer à améliorer |
| **60-80%** | Bon | Niveau professionnel acceptable |
| **80-90%** | Très bon | Excellent niveau de qualité |
| **> 90%** | Excellent | Rare et coûteux à maintenir |
| **100%** | Utopique | Généralement impossible et non rentable |

### Objectif réaliste : 70-80%

Pour la plupart des projets, **70-80%** de couverture est un excellent objectif qui équilibre :
- Qualité du code
- Effort de test
- Temps de développement
- Maintenabilité

### Pourquoi pas 100% ?

**Raisons pratiques :**

1. **Code trivial**
   - Getters/Setters simples
   - Constructeurs basiques
   - Code généré automatiquement

2. **Code d'interface**
   - Affichage UI (difficilement testable)
   - Code de présentation

3. **Code de gestion d'erreurs rares**
   - Pannes système
   - Erreurs matérielles
   - Situations impossibles à reproduire

4. **Coût vs Bénéfice**
   - Passer de 90% à 95% peut prendre plus de temps que de 0% à 90%
   - Les derniers pourcents sont les plus difficiles

**Citation célèbre :**

> "Viser 100% de couverture est contre-productif. Visez une couverture élevée des parties critiques."
> – Martin Fowler

### Prioriser les zones critiques

Plutôt qu'un objectif global, utilisez des objectifs par zone :

```
Module                    Objectif    Actuel
--------------------------------------------
Paiements                 90%         85%  ← Critique
Calculs métier            80%         75%  ← Important
Interface utilisateur     50%         45%  ← Moins critique
Utilitaires               70%         100% ← OK
Logging                   30%         35%  ← Non critique
```

## Métriques de qualité au-delà de la couverture

La couverture de code est importante, mais ce n'est qu'une métrique parmi d'autres.

### 1. Complexité cyclomatique

**Définition :** Mesure la complexité d'une fonction basée sur le nombre de chemins d'exécution.

**Formule simplifiée :**
```
Complexité = Nombre de points de décision + 1
```

**Exemple :**

```pascal
// Complexité = 1 (pas de décision)
function SimpleAddition(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Complexité = 2 (un if)
function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

// Complexité = 4 (trois if)
function ComplexFunction(A, B, C: Integer): Integer;
begin
  if A > 0 then
    if B > 0 then
      if C > 0 then
        Result := A + B + C
      else
        Result := A + B
    else
      Result := A
  else
    Result := 0;
end;
```

**Interprétation :**

| Complexité | Évaluation | Action |
|------------|------------|--------|
| **1-5** | Simple | ✓ OK |
| **6-10** | Moyenne | ⚠ Surveiller |
| **11-20** | Élevée | ⚠ Refactorer recommandé |
| **> 20** | Très élevée | ✗ Refactorer obligatoire |

**Pourquoi c'est important :**
- Code complexe = Plus de bugs
- Code complexe = Plus difficile à tester
- Code complexe = Plus difficile à maintenir

### 2. Profondeur d'imbrication

**Définition :** Nombre de niveaux de structures imbriquées (if, for, while, etc.).

**Exemple :**

```pascal
// Profondeur = 1
if Condition then
  Traiter;

// Profondeur = 3 (difficile à lire)
if Condition1 then
  if Condition2 then
    for i := 1 to 10 do
      if Condition3 then
        Traiter;

// Recommandation : Max 3 niveaux
```

**Objectif :** Maximum 3-4 niveaux d'imbrication.

### 3. Longueur des fonctions

**Recommandations :**

| Lignes | Évaluation |
|--------|------------|
| **< 20** | ✓ Idéal |
| **20-50** | ✓ Acceptable |
| **50-100** | ⚠ Long |
| **> 100** | ✗ Trop long, découper |

**Principe :** Une fonction devrait faire une seule chose et le faire bien.

**Mauvais exemple (150 lignes) :**

```pascal
procedure ProcessOrder;
begin
  // Valider les données (30 lignes)
  // Calculer les totaux (40 lignes)
  // Appliquer les remises (35 lignes)
  // Processer le paiement (25 lignes)
  // Envoyer les emails (20 lignes)
end;
```

**Bon exemple (5 fonctions de 20-30 lignes) :**

```pascal
procedure ProcessOrder;
begin
  ValidateOrderData;
  CalculateTotals;
  ApplyDiscounts;
  ProcessPayment;
  SendConfirmationEmails;
end;
```

### 4. Couplage et cohésion

**Couplage :** Degré de dépendance entre les modules.
- **Couplage faible** : ✓ Bon (modules indépendants)
- **Couplage fort** : ✗ Mauvais (modules interdépendants)

**Cohésion :** Degré de relation entre les éléments d'un module.
- **Cohésion forte** : ✓ Bon (module fait une chose bien)
- **Cohésion faible** : ✗ Mauvais (module fait plein de choses non liées)

### 5. Duplication de code

**Règle DRY (Don't Repeat Yourself) :** Ne répétez pas le code.

**Indicateur :** Pourcentage de code dupliqué.

| Duplication | Évaluation |
|-------------|------------|
| **< 3%** | ✓ Excellent |
| **3-5%** | ✓ Bon |
| **5-10%** | ⚠ Moyen |
| **> 10%** | ✗ Problématique |

**Outils :**
- Analyseurs de code statiques
- Clone Detector
- Recherche manuelle de patterns répétitifs

### 6. Commentaires et documentation

**Ratio commentaires/code :**
- Trop peu : Code difficile à comprendre
- Trop : Code probablement mal écrit

**Objectif :** 10-20% de commentaires

**Qualité > Quantité :**

```pascal
// ❌ Commentaire inutile
i := i + 1;  // Incrémenter i

// ✓ Commentaire utile
// Appliquer une réduction de 15% pour les clients VIP
// selon la politique commerciale 2024
Prix := Prix * 0.85;
```

## Améliorer la couverture de code

### Stratégie progressive

**Étape 1 : Mesurer l'existant**

Exécutez vos tests actuels avec la couverture. Notez le pourcentage de départ.

**Étape 2 : Identifier les gaps**

Examinez le rapport de couverture :
- Quelles unités ont la plus faible couverture ?
- Quelles sont les plus critiques ?

**Étape 3 : Prioriser**

Créez une liste priorisée :

```
1. PaymentService (critique, 30% couvert) - PRIORITÉ 1
2. OrderProcessor (important, 45% couvert) - PRIORITÉ 2
3. ClientManager (important, 65% couvert) - PRIORITÉ 3
4. Logger (utilitaire, 40% couvert) - PRIORITÉ 4
```

**Étape 4 : Ajouter des tests**

Pour chaque unité prioritaire, ajoutez des tests pour couvrir :
1. Les chemins principaux non couverts
2. Les cas d'erreur
3. Les cas limites

**Étape 5 : Mesurer l'amélioration**

Relancez la couverture et vérifiez la progression.

**Étape 6 : Itérer**

Répétez le processus jusqu'à atteindre votre objectif.

### Techniques pour améliorer la couverture

**1. Tests des cas limites**

```pascal
function Diviser(A, B: Integer): Integer;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro');
  Result := A div B;
end;

// Tests
procedure TestDiviserNormal;
begin
  Assert.AreEqual(5, Diviser(10, 2));
end;

procedure TestDiviserParZero;  // ← Ajoute 100% de couverture
begin
  Assert.WillRaise(procedure
  begin
    Diviser(10, 0);
  end, Exception);
end;
```

**2. Tests des branches else**

```pascal
if Condition then
  BrancheA
else
  BrancheB;  // ← Souvent oubliée !

// Ajouter un test spécifique pour le else
```

**3. Tests des exceptions**

```pascal
procedure ChargerClient(ID: Integer);
begin
  if ID <= 0 then
    raise EArgumentException.Create('ID invalide');  // ← Tester ça
  // ...
end;
```

**4. Tests des boucles vides**

```pascal
for Item in Liste do
  Traiter(Item);

// Tester avec Liste.Count = 0
```

**5. Tests des conditions multiples**

```pascal
if (A and B) or C then
  // ...

// Tester : A=T,B=T,C=F / A=F,B=T,C=T / etc.
```

## Qualité du code : Au-delà des métriques

### Code reviews (Revues de code)

Les métriques ne remplacent pas l'œil humain.

**Avantages des code reviews :**
- Détectent les problèmes de design
- Partagent les connaissances
- Améliorent la qualité globale
- Détectent les bugs que les tests manquent

**Checklist de review :**

```markdown
□ Le code est-il lisible et clair ?
□ Les noms de variables sont-ils descriptifs ?
□ Y a-t-il du code dupliqué ?
□ Les fonctions sont-elles trop longues ?
□ Les erreurs sont-elles gérées correctement ?
□ Y a-t-il des tests pour ce code ?
□ Les tests sont-ils compréhensibles ?
□ La couverture est-elle acceptable ?
```

### Principes SOLID

Les cinq principes SOLID améliorent la qualité du code :

**S - Single Responsibility**
Une classe = une seule responsabilité

**O - Open/Closed**
Ouvert à l'extension, fermé à la modification

**L - Liskov Substitution**
Les classes dérivées doivent pouvoir remplacer les classes de base

**I - Interface Segregation**
Plusieurs interfaces spécifiques valent mieux qu'une interface générale

**D - Dependency Inversion**
Dépendre d'abstractions, pas de concrétions

### Clean Code

**Principes du code propre :**

**1. Noms significatifs**

```pascal
// ❌ Mauvais
var
  d: Integer;  // Qu'est-ce que c'est ?

// ✓ Bon
var
  DaysUntilExpiration: Integer;
```

**2. Fonctions courtes**

```pascal
// ✓ Bon
function EstValide(Client: TClient): Boolean;
begin
  Result := (Client <> nil) and
            (Client.Email <> '') and
            (Client.Age >= 18);
end;
```

**3. Éviter les nombres magiques**

```pascal
// ❌ Mauvais
if Age > 18 then

// ✓ Bon
const
  AGE_MAJORITE = 18;

if Age > AGE_MAJORITE then
```

**4. Gérer les erreurs proprement**

```pascal
// ✓ Bon
try
  ProcesserCommande(Commande);
except
  on E: EPaymentException do
    LoggerErreur('Paiement échoué', E);
  on E: Exception do
  begin
    LoggerErreur('Erreur inattendue', E);
    raise;
  end;
end;
```

### Dette technique

**Définition :** Compromis fait pour livrer plus vite, créant du travail futur.

**Exemples :**
- Code dupliqué ("on refactorera plus tard")
- Tests manquants ("on les écrira plus tard")
- Design imparfait ("ça marche pour l'instant")

**Gestion de la dette :**

```
Chaque sprint :
- 80% nouvelles fonctionnalités
- 20% réduction de la dette technique
```

**Mesure de la dette :**
- Temps estimé pour "nettoyer" le code
- Nombre de TODOs dans le code
- Complexité moyenne du code

## Intégration dans le processus de développement

### Intégration Continue (CI)

Automatisez les tests et la couverture à chaque commit.

**Exemple de pipeline CI (GitLab CI) :**

```yaml
stages:
  - build
  - test
  - coverage

build:
  stage: build
  script:
    - msbuild MonProjet.dpr /p:Configuration=Release

test:
  stage: test
  script:
    - MonAppTests.exe

coverage:
  stage: coverage
  script:
    - CodeCoverage.exe -e MonAppTests.exe -m MonApp.exe -a
  artifacts:
    paths:
      - CodeCoverage_html/
  coverage: '/Couverture globale: (\d+\.\d+)%/'
```

**Avantages :**
- Couverture calculée automatiquement
- Alerte si la couverture baisse
- Historique de la couverture

### Badges de couverture

Affichez la couverture sur votre README :

```markdown
![Couverture](https://img.shields.io/badge/couverture-75%25-yellow)
```

### Quality Gates

Définissez des seuils minimaux :

```yaml
quality_gate:
  coverage: 70%  # Minimum 70%
  complexity: 15  # Max 15 de complexité cyclomatique
  duplication: 5%  # Max 5% de duplication
```

Si un seuil n'est pas respecté, le build échoue.

**Exemple :**

```
✗ Build échoué : Couverture (65%) < Minimum (70%)
  → Ajoutez des tests avant de merger
```

### Revue de code avec couverture

Lors des code reviews, examinez :
- Le code ajouté
- Les tests correspondants
- L'impact sur la couverture globale

**Commentaire type :**

```
Changement : +150 lignes de code, +0 lignes de tests
Couverture : 75% → 68% (baisse de 7%)

Action : Merci d'ajouter des tests pour les nouvelles fonctionnalités
avant que la PR soit acceptée.
```

## Pièges à éviter

### 1. Viser 100% à tout prix

**Problème :** Passer trop de temps sur les derniers pourcents.

**Solution :** Acceptez que 70-90% est excellent.

### 2. Tests inutiles juste pour la couverture

```pascal
// ❌ Test inutile
procedure TestGetterSetter;
begin
  Client.Nom := 'Test';
  Assert.AreEqual('Test', Client.Nom);  // Évident, inutile
end;
```

**Solution :** Testez la logique, pas les évidences.

### 3. Ignorer la qualité des tests

**Problème :** Tests qui passent toujours, même avec du code cassé.

```pascal
// ❌ Test qui ne teste rien vraiment
procedure TestMauvais;
begin
  Fonction;  // Pas d'assertion !
end;
```

**Solution :** Chaque test doit avoir des assertions significatives.

### 4. Se fier uniquement aux métriques

**Problème :** "100% de couverture, donc le code est parfait !"

**Réalité :** Les métriques ne remplacent pas la réflexion.

**Solution :** Utilisez les métriques comme indicateurs, pas comme vérité absolue.

### 5. Négliger le code legacy

**Problème :** "Ce vieux code n'a pas de tests, on ne peut rien faire."

**Solution :** Amélioration progressive :
1. Ajoutez des tests aux zones que vous modifiez
2. Refactorez progressivement pour rendre testable
3. Fixez un objectif réaliste (50% pour le legacy)

## Outils d'analyse statique

En complément de la couverture, les analyseurs statiques détectent les problèmes sans exécuter le code.

### FixInsight

Analyseur statique pour Delphi.

**Détecte :**
- Variables non utilisées
- Code mort
- Conversions de type dangereuses
- Fuites mémoire potentielles
- Mauvaises pratiques

**Intégration :** Plugin IDE ou ligne de commande

### Pascal Analyzer

Outil commercial d'analyse statique.

**Fonctionnalités :**
- Plus de 300 règles de qualité
- Métriques détaillées
- Rapports personnalisables
- Intégration CI/CD

### SonarQube pour Delphi

Avec le plugin Delphi, SonarQube peut analyser :
- Complexité
- Duplication
- Violations de règles
- Dette technique
- Couverture de code

## Conseils pour débutants

### 1. Commencez petit

Ne tentez pas d'atteindre 80% de couverture sur tout le projet d'un coup.

**Approche :**
1. Semaine 1 : Une unité à 70%
2. Semaine 2 : Deux autres unités
3. Semaine 3 : Continuer progressivement

### 2. Testez d'abord le nouveau code

Pour le code nouveau, visez une bonne couverture dès le départ.

**Règle :** Chaque nouvelle fonctionnalité = tests avec couverture > 70%

### 3. Utilisez la couverture comme guide

La couverture vous dit QUOI tester, pas COMMENT.

**Workflow :**
1. Écrire le code
2. Écrire des tests
3. Vérifier la couverture
4. Identifier les branches non testées
5. Ajouter des tests ciblés

### 4. Ne sacrifiez pas la qualité

```
Couverture 90% avec tests médiocres < Couverture 70% avec tests excellents
```

Préférez moins de tests mais de bonne qualité.

### 5. Apprenez des rapports

Étudiez les rapports de couverture pour comprendre :
- Quelles zones sont difficiles à tester (refactoring nécessaire)
- Quels patterns de code augmentent la couverture
- Où sont vos angles morts

### 6. Partagez avec l'équipe

La couverture est un indicateur d'équipe, pas individuel.

**Pratiques :**
- Stand-up : Mentionner la couverture actuelle
- Review : Vérifier l'impact sur la couverture
- Rétrospective : Discuter des objectifs de couverture

### 7. Automatisez tout

N'exécutez pas la couverture manuellement. Intégrez-la dans :
- Votre IDE (si possible)
- Votre système de build
- Votre CI/CD

### 8. Fixez des objectifs réalistes

```
Projet legacy (10% actuellement) → Objectif 40% en 6 mois
Nouveau projet → Objectif 70% dès le début
Module critique → Objectif 85%
```

### 9. Célébrez les progrès

```
Mois 1 : 35% → Mois 2 : 48% → Mois 3 : 59%

🎉 Bravo l'équipe ! +24% en 3 mois !
```

### 10. N'oubliez pas le but

**Le but n'est PAS d'avoir 100% de couverture.**

**Le but EST d'avoir un code fiable, maintenable, et de qualité.**

La couverture est un moyen, pas une fin.

## Exemples de rapports de couverture

### Rapport simple

```
=== Rapport de couverture ===
Date : 2025-10-18
Projet : MonApplication

Résumé global :
- Lignes totales : 12,450
- Lignes couvertes : 8,936
- Couverture : 71.8%

Top 5 unités bien couvertes :
1. ClientService.pas : 95.2%
2. Utils.pas : 92.8%
3. DataAccess.pas : 88.5%
4. Logger.pas : 100%
5. Config.pas : 94.1%

Top 5 unités peu couvertes :
1. PaymentGateway.pas : 23.4%  ⚠️
2. EmailService.pas : 31.7%    ⚠️
3. ReportGenerator.pas : 42.1%
4. CacheManager.pas : 48.9%
5. OrderProcessor.pas : 52.3%

Action recommandée :
Focus sur PaymentGateway et EmailService (modules critiques)
```

### Dashboard de suivi

```
Projet : MonApplication
Période : Octobre 2025

     35%  45%  58%  67%  71.8%
Sem1  ▓   ░    ░    ░    ░
Sem2  ▓   ▓    ░    ░    ░
Sem3  ▓   ▓    ▓    ░    ░
Sem4  ▓   ▓    ▓    ▓    ░
Sem5  ▓   ▓    ▓    ▓    ▓

Objectif 75% : En bonne voie ✓
Complexité moyenne : 8.2 (bon)
Dette technique : -15% ce mois
```

## Checklist qualité complète

**□ Couverture de code**
- [ ] Couverture globale > 70%
- [ ] Modules critiques > 80%
- [ ] Nouveaux codes > 70%
- [ ] Rapport de couverture généré automatiquement

**□ Tests**
- [ ] Tests unitaires présents
- [ ] Tests d'intégration présents
- [ ] Tests passent tous
- [ ] Temps d'exécution acceptable (<5 min)

**□ Complexité**
- [ ] Complexité cyclomatique < 15 par fonction
- [ ] Profondeur imbrication < 4
- [ ] Fonctions < 50 lignes

**□ Code propre**
- [ ] Pas de code dupliqué (< 5%)
- [ ] Noms descriptifs
- [ ] Commentaires pertinents
- [ ] Pas de code mort

**□ Architecture**
- [ ] Principes SOLID respectés
- [ ] Couplage faible
- [ ] Cohésion forte
- [ ] Dépendances bien gérées

**□ Documentation**
- [ ] README à jour
- [ ] API documentée
- [ ] Commentaires de code
- [ ] Diagrammes d'architecture

**□ Processus**
- [ ] Code reviews systématiques
- [ ] CI/CD en place
- [ ] Quality gates configurés
- [ ] Métriques suivies

## Conclusion

La couverture de code et les métriques de qualité sont des outils puissants pour améliorer la fiabilité et la maintenabilité de vos applications Delphi. Cependant, ce ne sont que des outils : ils vous guident mais ne remplacent pas la réflexion et le jugement.

**Points clés à retenir :**

**Couverture de code :** Mesure quelle proportion de votre code est exécutée par les tests. Visez 70-80% pour un équilibre qualité/effort.

**Types de couverture :** Lignes, branches, chemins, conditions. Commencez par les lignes, progressez vers les branches.

**Outils :** Delphi Code Coverage est gratuit et efficace. Intégrez-le dans votre workflow dès le début.

**Au-delà de la couverture :** Complexité cyclomatique, longueur de fonctions, couplage, duplication sont tout aussi importants.

**Qualité > Quantité :** 70% de couverture avec d'excellents tests vaut mieux que 95% avec des tests faibles.

**Amélioration progressive :** N'essayez pas d'atteindre 80% d'un coup. Progressez semaine après semaine, module par module.

**Automatisation :** Intégrez les métriques dans votre CI/CD pour un suivi continu et automatique.

**Équipe :** La qualité est une responsabilité d'équipe. Partagez les métriques, discutez-en, progressez ensemble.

**Pragmatisme :** Ne sacrifiez pas la livraison de valeur pour atteindre des métriques parfaites. Trouvez le bon équilibre.

**But final :** Un code fiable, maintenable, compréhensible, et qui apporte de la valeur aux utilisateurs.

En intégrant ces pratiques dans votre développement Delphi quotidien, vous créez une culture de qualité qui se traduit par des applications plus robustes, moins de bugs en production, et une base de code plus facile à faire évoluer. C'est un investissement dans le long terme qui profite à toute l'équipe et aux utilisateurs finaux. Les métriques sont vos alliées pour mesurer et améliorer continuellement, mais rappelez-vous toujours que le véritable objectif est la qualité globale du produit, pas juste des chiffres élevés sur un tableau de bord.

⏭️ [Internationalisation et localisation](/13-internationalisation-et-localisation/README.md)
