🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.5 Procédures et fonctions

## Introduction

Les procédures et fonctions sont des blocs de code réutilisables qui permettent d'organiser votre programme en sous-tâches logiques. Au lieu d'écrire le même code plusieurs fois, vous le regroupez dans une procédure ou fonction que vous pouvez appeler autant de fois que nécessaire.

**Avantages :**
- **Réutilisabilité** : Écrivez une fois, utilisez partout
- **Lisibilité** : Code mieux structuré et plus facile à comprendre
- **Maintenance** : Modifiez le code en un seul endroit
- **Testabilité** : Testez chaque fonction indépendamment
- **Collaboration** : Divisez le travail entre développeurs

## Différence entre procédure et fonction

| Caractéristique | Procédure | Fonction |
|----------------|-----------|----------|
| Retourne une valeur | Non | Oui |
| Mot-clé | `procedure` | `function` |
| Usage typique | Effectuer une action | Calculer et retourner un résultat |
| Exemple | Afficher un message | Calculer une somme |

**Analogie simple :**
- **Procédure** = Recette de cuisine (on fait quelque chose)
- **Fonction** = Machine à calculer (on obtient un résultat)

## Les procédures

### Déclaration simple

**Syntaxe :**
```pascal
procedure NomProcedure;
begin
  // Instructions
end;
```

**Exemple :**
```pascal
procedure DireBonjour;
begin
  ShowMessage('Bonjour !');
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  DireBonjour;  // Appel de la procédure
end;
```

### Procédure avec paramètres

Les paramètres permettent de passer des informations à la procédure.

**Syntaxe :**
```pascal
procedure NomProcedure(parametre1: Type1; parametre2: Type2);
begin
  // Instructions utilisant les paramètres
end;
```

**Exemple : Un paramètre**
```pascal
procedure DireBonjourA(const Nom: string);
begin
  ShowMessage('Bonjour ' + Nom + ' !');
end;

// Utilisation
begin
  DireBonjourA('Marie');    // Affiche : Bonjour Marie !
  DireBonjourA('Pierre');   // Affiche : Bonjour Pierre !
end;
```

**Exemple : Plusieurs paramètres**
```pascal
procedure AfficherInfos(const Nom, Prenom: string; Age: Integer);
begin
  ShowMessage(
    'Nom : ' + Nom + #13#10 +
    'Prénom : ' + Prenom + #13#10 +
    'Âge : ' + IntToStr(Age) + ' ans'
  );
end;

// Utilisation
begin
  AfficherInfos('Dupont', 'Marie', 25);
end;
```

### Paramètres par valeur vs par référence

#### Paramètres par valeur (défaut)

Le paramètre reçoit une **copie** de la valeur. Les modifications n'affectent pas la variable d'origine.

```pascal
procedure AugmenterValeur(Nombre: Integer);
begin
  Nombre := Nombre + 10;
  ShowMessage('Dans la procédure : ' + IntToStr(Nombre));
end;

// Utilisation
var
  X: Integer;
begin
  X := 5;
  AugmenterValeur(X);
  ShowMessage('Après la procédure : ' + IntToStr(X));  // Affiche toujours 5
end;
```

#### Paramètres par référence (var)

Le paramètre reçoit une **référence** à la variable. Les modifications affectent la variable d'origine.

```pascal
procedure AugmenterValeur(var Nombre: Integer);
begin
  Nombre := Nombre + 10;
  ShowMessage('Dans la procédure : ' + IntToStr(Nombre));
end;

// Utilisation
var
  X: Integer;
begin
  X := 5;
  AugmenterValeur(X);
  ShowMessage('Après la procédure : ' + IntToStr(X));  // Affiche 15
end;
```

**Exemple pratique : Échanger deux valeurs**
```pascal
procedure Echanger(var A, B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

// Utilisation
var
  X, Y: Integer;
begin
  X := 10;
  Y := 20;

  ShowMessage('Avant : X = ' + IntToStr(X) + ', Y = ' + IntToStr(Y));
  Echanger(X, Y);
  ShowMessage('Après : X = ' + IntToStr(X) + ', Y = ' + IntToStr(Y));
  // Après : X = 20, Y = 10
end;
```

#### Paramètres constants (const)

Utiliser `const` indique que le paramètre ne sera pas modifié. C'est une optimisation pour les types complexes (chaînes, enregistrements, objets).

```pascal
procedure AfficherMessage(const Message: string);
begin
  ShowMessage(Message);
  // Message := 'Autre chose';  // ← ERREUR : impossible de modifier un const
end;
```

**Avantages de const :**
- Plus performant pour les chaînes et structures
- Protège contre les modifications accidentelles
- Indique clairement l'intention au lecteur du code

#### Paramètres de sortie (out)

Similaire à `var` mais indique que le paramètre est uniquement utilisé pour **sortir** une valeur.

```pascal
procedure ObtenirDimensions(out Largeur, Hauteur: Integer);
begin
  Largeur := 1920;
  Hauteur := 1080;
end;

// Utilisation
var
  W, H: Integer;
begin
  ObtenirDimensions(W, H);
  ShowMessage(Format('Dimensions : %d x %d', [W, H]));
end;
```

### Tableau récapitulatif des types de paramètres

| Type | Mot-clé | Passage | Modification | Usage |
|------|---------|---------|--------------|-------|
| Par valeur | *(aucun)* | Copie | Variable d'origine non modifiée | Valeurs simples en lecture |
| Par référence | `var` | Référence | Variable d'origine modifiée | Modification de la variable |
| Constant | `const` | Copie ou référence optimisée | Interdit | Lecture seule, optimisation |
| Sortie | `out` | Référence | Variable d'origine modifiée | Paramètre de sortie uniquement |

### Paramètres avec valeurs par défaut

Depuis Delphi 4, vous pouvez définir des valeurs par défaut pour les paramètres.

```pascal
procedure Saluer(const Nom: string = 'Invité'; const Titre: string = '');
begin
  if Titre <> '' then
    ShowMessage('Bonjour ' + Titre + ' ' + Nom)
  else
    ShowMessage('Bonjour ' + Nom);
end;

// Utilisations possibles
begin
  Saluer;                           // Bonjour Invité
  Saluer('Marie');                  // Bonjour Marie
  Saluer('Dupont', 'Madame');       // Bonjour Madame Dupont
end;
```

**Règles importantes :**
- Les paramètres avec valeur par défaut doivent être à la fin
- Tous les paramètres après le premier paramètre par défaut doivent aussi avoir une valeur par défaut

```pascal
// ✅ CORRECT
procedure Test1(A: Integer; B: Integer = 10; C: string = 'test');

// ❌ ERREUR : paramètre sans défaut après un paramètre avec défaut
procedure Test2(A: Integer; B: Integer = 10; C: string);
```

### Paramètres tableaux ouverts

Permet de passer un tableau de taille variable.

```pascal
procedure AfficherNombres(const Nombres: array of Integer);
var
  i: Integer;
begin
  for i := Low(Nombres) to High(Nombres) do
    ShowMessage(IntToStr(Nombres[i]));
end;

// Utilisation
begin
  AfficherNombres([1, 2, 3]);
  AfficherNombres([10, 20, 30, 40, 50]);
end;
```

**Fonctions utiles :**
- `Low(Tableau)` : Retourne l'indice minimum
- `High(Tableau)` : Retourne l'indice maximum
- `Length(Tableau)` : Retourne la taille du tableau

## Les fonctions

### Déclaration simple

**Syntaxe :**
```pascal
function NomFonction: TypeRetour;
begin
  // Instructions
  Result := valeur;  // ou : NomFonction := valeur;
end;
```

**Exemple :**
```pascal
function ObtenirMessage: string;
begin
  Result := 'Bienvenue dans Delphi !';
end;

// Utilisation
var
  Message: string;
begin
  Message := ObtenirMessage;
  ShowMessage(Message);
end;
```

### Fonction avec paramètres

```pascal
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Utilisation
var
  Somme: Integer;
begin
  Somme := Additionner(5, 3);
  ShowMessage('5 + 3 = ' + IntToStr(Somme));  // Affiche : 5 + 3 = 8
end;
```

### Result vs nom de fonction

Deux syntaxes possibles pour retourner une valeur :

```pascal
// Syntaxe moderne (recommandée)
function Carre(N: Integer): Integer;
begin
  Result := N * N;
end;

// Syntaxe ancienne (déconseillée mais valide)
function Carre(N: Integer): Integer;
begin
  Carre := N * N;
end;
```

**Recommandation :** Utilisez toujours `Result` pour plus de clarté.

### Exemples de fonctions utiles

#### Fonction de validation

```pascal
function EstEmailValide(const Email: string): Boolean;
begin
  Result := (Pos('@', Email) > 0) and (Pos('.', Email) > 0);
end;

// Utilisation
begin
  if EstEmailValide('marie@example.com') then
    ShowMessage('Email valide')
  else
    ShowMessage('Email invalide');
end;
```

#### Fonction de calcul

```pascal
function CalculerTTC(PrixHT: Double; TauxTVA: Double = 0.20): Double;
begin
  Result := PrixHT * (1 + TauxTVA);
end;

// Utilisation
var
  PrixFinal: Double;
begin
  PrixFinal := CalculerTTC(100.0);       // Utilise la TVA par défaut (20%)
  ShowMessage(FormatFloat('0.00 €', PrixFinal));  // 120.00 €

  PrixFinal := CalculerTTC(100.0, 0.055);  // TVA à 5.5%
  ShowMessage(FormatFloat('0.00 €', PrixFinal));  // 105.50 €
end;
```

#### Fonction de transformation

```pascal
function CapitaliserMots(const Texte: string): string;
var
  i: Integer;
  NouveauMot: Boolean;
begin
  Result := LowerCase(Texte);
  NouveauMot := True;

  for i := 1 to Length(Result) do
  begin
    if NouveauMot and (Result[i] <> ' ') then
    begin
      Result[i] := UpCase(Result[i]);
      NouveauMot := False;
    end;

    if Result[i] = ' ' then
      NouveauMot := True;
  end;
end;

// Utilisation
begin
  ShowMessage(CapitaliserMots('bonjour le monde'));  // Bonjour Le Monde
end;
```

#### Fonction avec plusieurs valeurs de retour

Utilisez des paramètres `var` ou `out` pour retourner plusieurs valeurs.

```pascal
function DiviserAvecReste(Dividende, Diviseur: Integer;
                          out Reste: Integer): Integer;
begin
  Result := Dividende div Diviseur;
  Reste := Dividende mod Diviseur;
end;

// Utilisation
var
  Quotient, Reste: Integer;
begin
  Quotient := DiviserAvecReste(17, 5, Reste);
  ShowMessage(Format('17 ÷ 5 = %d reste %d', [Quotient, Reste]));
  // Affiche : 17 ÷ 5 = 3 reste 2
end;
```

### Fonctions booléennes

Les fonctions qui retournent `Boolean` sont très utiles pour les tests.

```pascal
function EstPair(Nombre: Integer): Boolean;
begin
  Result := (Nombre mod 2) = 0;
end;

function EstDansIntervalle(Valeur, Min, Max: Integer): Boolean;
begin
  Result := (Valeur >= Min) and (Valeur <= Max);
end;

function EstAnneeBissextile(Annee: Integer): Boolean;
begin
  Result := ((Annee mod 4) = 0) and
            (((Annee mod 100) <> 0) or ((Annee mod 400) = 0));
end;

// Utilisation
var
  N: Integer;
begin
  N := 24;

  if EstPair(N) then
    ShowMessage(IntToStr(N) + ' est pair');

  if EstDansIntervalle(N, 1, 100) then
    ShowMessage(IntToStr(N) + ' est entre 1 et 100');

  if EstAnneeBissextile(2024) then
    ShowMessage('2024 est une année bissextile');
end;
```

## Portée des variables

### Variables locales

Déclarées dans une procédure/fonction, elles n'existent que pendant son exécution.

```pascal
procedure Calculer;
var
  Total: Integer;  // Variable locale
begin
  Total := 100;
  ShowMessage(IntToStr(Total));
end;

procedure AutreProcedure;
begin
  ShowMessage(IntToStr(Total));  // ❌ ERREUR : Total n'existe pas ici
end;
```

### Variables globales

Déclarées au niveau de l'unité, elles sont accessibles partout dans l'unité.

```pascal
unit MonUnite;

interface

var
  CompteurGlobal: Integer;  // Variable globale

procedure Incrementer;

implementation

procedure Incrementer;
begin
  Inc(CompteurGlobal);  // Accès à la variable globale
end;

procedure Afficher;
begin
  ShowMessage(IntToStr(CompteurGlobal));  // Accès à la variable globale
end;

end.
```

**Bonnes pratiques :**
- Évitez les variables globales autant que possible
- Privilégiez le passage de paramètres
- Si nécessaire, utilisez des propriétés de classe plutôt que des variables globales

### Variables locales avec même nom

Une variable locale peut avoir le même nom qu'une variable globale. La variable locale a priorité.

```pascal
var
  Compteur: Integer;  // Variable globale

procedure Test;
var
  Compteur: Integer;  // Variable locale (cache la globale)
begin
  Compteur := 10;  // Modifie la variable locale
  ShowMessage(IntToStr(Compteur));  // Affiche 10
end;

begin
  Compteur := 5;  // Variable globale
  Test;
  ShowMessage(IntToStr(Compteur));  // Affiche 5 (globale non modifiée)
end;
```

## Procédures et fonctions imbriquées

Vous pouvez déclarer des procédures/fonctions à l'intérieur d'autres procédures/fonctions.

```pascal
procedure ProcedurePrincipale;

  // Procédure imbriquée
  procedure ProcedureInterne;
  begin
    ShowMessage('Procédure interne');
  end;

begin
  ShowMessage('Procédure principale');
  ProcedureInterne;  // Appel de la procédure imbriquée
end;
```

**Exemple pratique :**
```pascal
procedure TraiterDonnees;
var
  Total: Integer;

  function CalculerSomme(A, B: Integer): Integer;
  begin
    Result := A + B;
  end;

  procedure AfficherResultat;
  begin
    ShowMessage('Total : ' + IntToStr(Total));
  end;

begin
  Total := CalculerSomme(10, 20);
  AfficherResultat;
end;
```

**Note :** Les procédures/fonctions imbriquées ont accès aux variables locales de la procédure parente.

## Récursivité

Une fonction récursive est une fonction qui s'appelle elle-même.

### Exemple : Factorielle

```pascal
function Factorielle(N: Integer): Int64;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorielle(N - 1);
end;

// Utilisation
begin
  ShowMessage('5! = ' + IntToStr(Factorielle(5)));  // 5! = 120
end;
```

**Explication :**
- Factorielle(5) = 5 × Factorielle(4)
- Factorielle(4) = 4 × Factorielle(3)
- Factorielle(3) = 3 × Factorielle(2)
- Factorielle(2) = 2 × Factorielle(1)
- Factorielle(1) = 1 (condition d'arrêt)

### Exemple : Suite de Fibonacci

```pascal
function Fibonacci(N: Integer): Integer;
begin
  if N <= 1 then
    Result := N
  else
    Result := Fibonacci(N - 1) + Fibonacci(N - 2);
end;

// Utilisation
var
  i: Integer;
begin
  for i := 0 to 10 do
    ShowMessage(Format('Fibonacci(%d) = %d', [i, Fibonacci(i)]));
end;
```

### Exemple : Puissance

```pascal
function Puissance(Base, Exposant: Integer): Int64;
begin
  if Exposant = 0 then
    Result := 1
  else
    Result := Base * Puissance(Base, Exposant - 1);
end;

// Utilisation
begin
  ShowMessage('2^10 = ' + IntToStr(Puissance(2, 10)));  // 2^10 = 1024
end;
```

**Points importants sur la récursivité :**
1. **Condition d'arrêt obligatoire** : sinon boucle infinie
2. **Consommation mémoire** : chaque appel utilise de la pile
3. **Performance** : parfois moins efficace qu'une boucle
4. **Élégance** : souvent plus claire pour certains problèmes

## Surcharge de procédures et fonctions (Overload)

La surcharge permet d'avoir plusieurs procédures/fonctions avec le même nom mais des paramètres différents.

**Mot-clé :** `overload`

```pascal
// Additionner deux entiers
function Additionner(A, B: Integer): Integer; overload;
begin
  Result := A + B;
end;

// Additionner trois entiers
function Additionner(A, B, C: Integer): Integer; overload;
begin
  Result := A + B + C;
end;

// Additionner deux réels
function Additionner(A, B: Double): Double; overload;
begin
  Result := A + B;
end;

// Utilisation
var
  ResInt: Integer;
  ResDouble: Double;
begin
  ResInt := Additionner(5, 3);           // Appelle la version à 2 entiers
  ResInt := Additionner(5, 3, 2);        // Appelle la version à 3 entiers
  ResDouble := Additionner(5.5, 3.2);    // Appelle la version à 2 réels
end;
```

**Exemple pratique : Affichage flexible**
```pascal
procedure Afficher(const Message: string); overload;
begin
  ShowMessage(Message);
end;

procedure Afficher(Nombre: Integer); overload;
begin
  ShowMessage(IntToStr(Nombre));
end;

procedure Afficher(Nombre: Double; Decimales: Integer); overload;
begin
  ShowMessage(FormatFloat('0.' + StringOfChar('0', Decimales), Nombre));
end;

// Utilisation
begin
  Afficher('Bonjour');        // Affiche une chaîne
  Afficher(42);               // Affiche un entier
  Afficher(3.14159, 2);       // Affiche un réel avec 2 décimales
end;
```

## Procédures et fonctions anonymes

Depuis Delphi 2009, vous pouvez créer des procédures et fonctions anonymes (closures).

```pascal
var
  Additionner: TFunc<Integer, Integer, Integer>;
begin
  // Fonction anonyme
  Additionner := function(A, B: Integer): Integer
                 begin
                   Result := A + B;
                 end;

  ShowMessage(IntToStr(Additionner(5, 3)));  // Affiche 8
end;
```

**Exemple avec procédure anonyme :**
```pascal
var
  DireBonjour: TProc<string>;
begin
  DireBonjour := procedure(const Nom: string)
                 begin
                   ShowMessage('Bonjour ' + Nom);
                 end;

  DireBonjour('Marie');
end;
```

**Note :** Les fonctions anonymes sont un sujet avancé. Pour débuter, concentrez-vous sur les procédures et fonctions classiques.

## Forward declaration

Parfois, vous devez déclarer qu'une fonction existe avant de la définir.

```pascal
// Déclaration forward
function FonctionA(N: Integer): Integer; forward;

function FonctionB(N: Integer): Integer;
begin
  if N > 0 then
    Result := FonctionA(N - 1)  // FonctionA n'est pas encore définie
  else
    Result := 0;
end;

// Définition de FonctionA
function FonctionA(N: Integer): Integer;
begin
  if N > 0 then
    Result := N + FonctionB(N - 1)
  else
    Result := 1;
end;
```

## Bonnes pratiques

### 1. Nommage clair

```pascal
// ✅ BON : noms descriptifs
function CalculerPrixTTC(PrixHT: Double): Double;
procedure EnvoyerEmail(const Destinataire, Sujet, Corps: string);

// ❌ MAUVAIS : noms vagues
function Calc(P: Double): Double;
procedure Send(const D, S, C: string);
```

### 2. Une fonction = une responsabilité

```pascal
// ✅ BON : fonction focalisée
function CalculerSomme(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// ❌ MAUVAIS : fait trop de choses
function ToutFaire(A, B: Integer): Integer;
begin
  Result := A + B;
  ShowMessage('Calcul effectué');
  Log('Addition de ' + IntToStr(A) + ' et ' + IntToStr(B));
  SaveToFile(Result);
end;
```

### 3. Limiter le nombre de paramètres

```pascal
// ❌ Trop de paramètres
procedure CreerUtilisateur(Nom, Prenom, Email, Tel, Adresse, Ville,
                          CodePostal, Pays: string; Age: Integer);

// ✅ Utiliser un enregistrement ou une classe
type
  TUtilisateur = record
    Nom, Prenom, Email, Tel: string;
    Adresse, Ville, CodePostal, Pays: string;
    Age: Integer;
  end;

procedure CreerUtilisateur(const Utilisateur: TUtilisateur);
```

### 4. Utiliser const pour les paramètres en lecture seule

```pascal
// ✅ BON : const pour les chaînes non modifiées
procedure AfficherMessage(const Message: string);

// ❌ Moins efficace
procedure AfficherMessage(Message: string);
```

### 5. Valider les paramètres

```pascal
function Diviser(A, B: Double): Double;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');

  Result := A / B;
end;

function ObtenirElement(const Liste: TStringList; Index: Integer): string;
begin
  if (Index < 0) or (Index >= Liste.Count) then
    raise Exception.Create('Index hors limites');

  Result := Liste[Index];
end;
```

### 6. Documenter les fonctions complexes

```pascal
/// <summary>
/// Calcule le montant TTC à partir d'un prix HT
/// </summary>
/// <param name="PrixHT">Prix hors taxes</param>
/// <param name="TauxTVA">Taux de TVA (0.20 pour 20%)</param>
/// <returns>Prix TTC calculé</returns>
function CalculerPrixTTC(PrixHT: Double; TauxTVA: Double = 0.20): Double;
begin
  Result := PrixHT * (1 + TauxTVA);
end;
```

### 7. Initialiser Result dès le début

```pascal
// ✅ BON : Result initialisé
function Chercher(const Valeur: string): Integer;
begin
  Result := -1;  // Valeur par défaut si non trouvé

  // ... code de recherche ...

  if Trouve then
    Result := Index;
end;

// ❌ Risqué : Result peut rester indéfini
function Chercher(const Valeur: string): Integer;
begin
  // ... code de recherche ...

  if Trouve then
    Result := Index;
  // Si Trouve = False, Result n'est pas défini !
end;
```

### 8. Préférer les fonctions aux procédures avec var

```pascal
// ✅ BON : fonction pure
function Carre(N: Integer): Integer;
begin
  Result := N * N;
end;

// ❌ Moins clair
procedure Carre(N: Integer; var Resultat: Integer);
begin
  Resultat := N * N;
end;
```

## Erreurs courantes à éviter

### Erreur 1 : Oublier d'assigner Result

```pascal
// ❌ ERREUR : Result non assigné
function Additionner(A, B: Integer): Integer;
begin
  ShowMessage('Addition en cours...');
  // Oubli d'assigner Result !
end;

// ✅ CORRECT
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;
```

### Erreur 2 : Modifier un paramètre const

```pascal
// ❌ ERREUR : tentative de modification d'un const
procedure Traiter(const Texte: string);
begin
  Texte := UpperCase(Texte);  // ← Erreur de compilation
end;

// ✅ CORRECT : utiliser une variable locale
procedure Traiter(const Texte: string);
var
  TexteMajuscules: string;
begin
  TexteMajuscules := UpperCase(Texte);
  ShowMessage(TexteMajuscules);
end;
```

### Erreur 3 : Récursivité sans condition d'arrêt

```pascal
// ❌ ERREUR : boucle infinie
function Compte(N: Integer): Integer;
begin
  Result := N + Compte(N - 1);  // Pas de condition d'arrêt !
end;

// ✅ CORRECT : avec condition d'arrêt
function Compte(N: Integer): Integer;
begin
  if N <= 0 then
    Result := 0
  else
    Result := N + Compte(N - 1);
end;
```

### Erreur 4 : Paramètres dans le mauvais ordre

```pascal
function Diviser(Dividende, Diviseur: Double): Double;
begin
  Result := Dividende / Diviseur;
end;

// ❌ Attention à l'ordre !
var
  Resultat: Double;
begin
  Resultat := Diviser(2, 10);  // 2 / 10 = 0.2
  // Voulait-on 10 / 2 = 5 ?
end;
```

### Erreur 5 : Confusion entre procédure et fonction

```pascal
// ❌ ERREUR : une procédure ne retourne pas de valeur
procedure Additionner(A, B: Integer): Integer;  // ← Erreur de syntaxe

// ✅ CORRECT : c'est une fonction
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;
```

## Organisation du code

### Ordre de déclaration dans une unité

```pascal
unit MonUnite;

interface

uses
  System.SysUtils;

// 1. Constantes
const
  MAX_VALEUR = 100;

// 2. Types
type
  TMonType = record
    Valeur: Integer;
  end;

// 3. Variables globales (à éviter si possible)
var
  CompteurGlobal: Integer;

// 4. Déclarations de procédures/fonctions publiques
procedure ProcedurePublique;
function FonctionPublique(N: Integer): string;

implementation

// 5. Procédures/fonctions privées
procedure ProcedurePrivee;
begin
  // ...
end;

// 6. Implémentation des procédures/fonctions publiques
procedure ProcedurePublique;
begin
  // ...
end;

function FonctionPublique(N: Integer): string;
begin
  Result := IntToStr(N);
end;

// 7. Initialisation (optionnelle)
initialization
  CompteurGlobal := 0;

// 8. Finalisation (optionnelle)
finalization
  // Code de nettoyage

end.
```

## Points clés à retenir

1. **Procédure** : effectue une action, ne retourne pas de valeur
2. **Fonction** : calcule et retourne une valeur
3. **Paramètres** : par valeur (défaut), par référence (`var`), constants (`const`), sortie (`out`)
4. **Result** : variable spéciale pour retourner la valeur d'une fonction
5. **Portée** : variables locales vs globales
6. **Overload** : permet plusieurs versions d'une même fonction
7. **Récursivité** : une fonction qui s'appelle elle-même (attention à la condition d'arrêt)
8. **Une fonction = une responsabilité**
9. Toujours initialiser `Result` dans les fonctions
10. Valider les paramètres pour éviter les erreurs

---

Les procédures et fonctions sont les briques fondamentales qui permettent de construire des programmes bien structurés et maintenables. Dans la section suivante, nous découvrirons la gestion des exceptions pour rendre nos programmes plus robustes face aux erreurs.

⏭️ [Gestion des exceptions](/03-langage-object-pascal/06-gestion-des-exceptions.md)
