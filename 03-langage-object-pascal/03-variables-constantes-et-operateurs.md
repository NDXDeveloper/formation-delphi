🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.3 Variables, constantes et opérateurs

## Introduction

Les variables et constantes sont les conteneurs qui stockent les données dans vos programmes, tandis que les opérateurs sont les outils qui vous permettent de manipuler ces données. Cette section vous apprendra à déclarer, initialiser et utiliser ces éléments fondamentaux.

## Les variables

### Qu'est-ce qu'une variable ?

Une variable est un emplacement en mémoire qui contient une valeur et qui peut être modifiée pendant l'exécution du programme. Chaque variable possède :
- Un **nom** (identifiant)
- Un **type** (Integer, String, Boolean, etc.)
- Une **valeur**
- Une **portée** (scope)

### Déclaration de variables

Pour déclarer une variable, on utilise le mot-clé `var` suivi du nom de la variable et de son type :

```pascal
var
  Age: Integer;
  Nom: string;
  EstActif: Boolean;
  Prix: Double;
```

**Déclaration multiple :**
```pascal
var
  Largeur, Hauteur, Profondeur: Integer;
  Prenom, NomFamille: string;
```

### Initialisation de variables

#### Initialisation au moment de l'utilisation

```pascal
var
  Compteur: Integer;
begin
  Compteur := 0;  // Initialisation
  Compteur := Compteur + 1;
end;
```

#### Initialisation inline (depuis Delphi 2009)

Vous pouvez initialiser une variable directement lors de sa déclaration :

```pascal
var
  Age: Integer = 25;
  Nom: string = 'Marie';
  Prix: Double = 19.99;
  EstActif: Boolean = True;
begin
  // Les variables sont déjà initialisées
  ShowMessage(Nom);
end;
```

**Avantage :** Cela rend le code plus lisible et évite les oublis d'initialisation.

### Variables locales vs variables globales

#### Variables locales

Déclarées dans une procédure ou fonction, elles n'existent que pendant l'exécution de celle-ci :

```pascal
procedure CalculerTotal;
var
  Total: Double;  // Variable locale
begin
  Total := 100.0;
  // Total n'existe que dans cette procédure
end;
```

#### Variables globales

Déclarées dans la section `implementation` ou `interface` d'une unité :

```pascal
unit MonUnite;

interface

var
  CompteurGlobal: Integer;  // Variable globale accessible partout

implementation

var
  ConfigInterne: string;    // Variable globale accessible uniquement dans cette unité

end.
```

**Bonnes pratiques :**
- Privilégiez les variables locales
- Évitez les variables globales sauf nécessité absolue
- Les variables globales rendent le code plus difficile à maintenir

### Portée des variables (Scope)

La portée détermine où une variable est accessible dans votre code.

```pascal
var
  X: Integer;  // Portée : toute l'unité

procedure Test1;
var
  Y: Integer;  // Portée : uniquement dans Test1
begin
  X := 10;  // OK : X est accessible
  Y := 20;  // OK : Y est accessible ici
end;

procedure Test2;
begin
  X := 30;  // OK : X est accessible
  Y := 40;  // ERREUR : Y n'existe pas dans Test2
end;
```

### Variables inline (Delphi 11+)

Depuis Delphi 11, vous pouvez déclarer des variables directement dans le code :

```pascal
begin
  var Compteur: Integer := 0;
  var Nom: string := 'Delphi';

  for var i: Integer := 1 to 10 do
  begin
    Compteur := Compteur + i;
  end;

  ShowMessage(IntToStr(Compteur));
end;
```

**Avantage :** La variable est déclarée au plus proche de son utilisation.

## Les constantes

### Qu'est-ce qu'une constante ?

Une constante est une valeur qui ne peut pas être modifiée pendant l'exécution du programme. Elle est définie une seule fois et reste fixe.

### Déclaration de constantes

#### Constantes typées

```pascal
const
  TauxTVA: Double = 0.20;
  NombreMaxUtilisateurs: Integer = 100;
  NomApplication: string = 'MonApp';
  ModeDebug: Boolean = True;
```

#### Constantes non typées

```pascal
const
  Pi = 3.14159265358979;
  Titre = 'Bienvenue';
  MaxValeur = 1000;
```

**Note :** Le type est déduit automatiquement de la valeur assignée.

### Constantes vs Variables

| Caractéristique | Variable | Constante |
|----------------|----------|-----------|
| Modification | Peut être modifiée | Immuable |
| Mot-clé | `var` | `const` |
| Performance | Standard | Légèrement optimisée |
| Usage | Valeurs changeantes | Valeurs fixes |

### Avantages des constantes

```pascal
const
  TauxTVA = 0.20;
  TauxRemise = 0.10;

procedure CalculerPrix;
var
  PrixHT, PrixTTC: Double;
begin
  PrixHT := 100.0;
  PrixTTC := PrixHT * (1 + TauxTVA);
  // Si le taux change, on modifie uniquement la constante
end;
```

**Avantages :**
1. Code plus lisible (nom explicite au lieu d'une valeur "magique")
2. Maintenance facilitée (modification en un seul endroit)
3. Évite les erreurs de frappe
4. Documentation intégrée

### Constantes de ressources

Les constantes sont souvent utilisées pour les messages et textes :

```pascal
const
  MSG_ERREUR_CONNEXION = 'Impossible de se connecter à la base de données';
  MSG_SUCCES_SAUVEGARDE = 'Les données ont été sauvegardées avec succès';
  MSG_CONFIRMATION = 'Êtes-vous sûr de vouloir continuer ?';

procedure Sauvegarder;
begin
  if EnregistrerDonnees then
    ShowMessage(MSG_SUCCES_SAUVEGARDE)
  else
    ShowMessage(MSG_ERREUR_CONNEXION);
end;
```

## Les opérateurs

Les opérateurs permettent d'effectuer des opérations sur les variables et constantes.

### Opérateurs arithmétiques

| Opérateur | Description | Exemple | Résultat |
|-----------|-------------|---------|----------|
| `+` | Addition | `5 + 3` | 8 |
| `-` | Soustraction | `5 - 3` | 2 |
| `*` | Multiplication | `5 * 3` | 15 |
| `/` | Division réelle | `5 / 2` | 2.5 |
| `div` | Division entière | `5 div 2` | 2 |
| `mod` | Modulo (reste) | `5 mod 2` | 1 |

**Exemples :**
```pascal
var
  A, B, Resultat: Integer;
  Quotient: Double;
begin
  A := 10;
  B := 3;

  Resultat := A + B;      // 13
  Resultat := A - B;      // 7
  Resultat := A * B;      // 30
  Resultat := A div B;    // 3 (division entière)
  Resultat := A mod B;    // 1 (reste de la division)

  Quotient := A / B;      // 3.333... (division réelle)
end;
```

### Opérateurs de comparaison

| Opérateur | Description | Exemple | Résultat |
|-----------|-------------|---------|----------|
| `=` | Égal à | `5 = 5` | True |
| `<>` | Différent de | `5 <> 3` | True |
| `<` | Inférieur à | `3 < 5` | True |
| `>` | Supérieur à | `5 > 3` | True |
| `<=` | Inférieur ou égal | `5 <= 5` | True |
| `>=` | Supérieur ou égal | `5 >= 3` | True |

**Exemples :**
```pascal
var
  Age: Integer;
  EstMajeur: Boolean;
begin
  Age := 20;

  EstMajeur := Age >= 18;           // True

  if Age = 18 then
    ShowMessage('Vous avez exactement 18 ans');

  if Age <> 18 then
    ShowMessage('Vous n''avez pas 18 ans');

  if Age > 65 then
    ShowMessage('Senior')
  else if Age >= 18 then
    ShowMessage('Adulte')
  else
    ShowMessage('Mineur');
end;
```

### Opérateurs logiques

| Opérateur | Description | Exemple | Résultat |
|-----------|-------------|---------|----------|
| `and` | ET logique | `True and False` | False |
| `or` | OU logique | `True or False` | True |
| `not` | NON logique | `not True` | False |
| `xor` | OU exclusif | `True xor True` | False |

**Exemples :**
```pascal
var
  Age: Integer;
  APermis: Boolean;
  PeutConduire: Boolean;
begin
  Age := 20;
  APermis := True;

  // ET logique : les deux conditions doivent être vraies
  PeutConduire := (Age >= 18) and APermis;

  // OU logique : au moins une condition doit être vraie
  if (Age < 18) or (not APermis) then
    ShowMessage('Ne peut pas conduire');

  // NON logique : inverse la valeur booléenne
  if not PeutConduire then
    ShowMessage('Conduite interdite');
end;
```

### Tables de vérité

**Opérateur AND :**
| A | B | A and B |
|---|---|---------|
| True | True | True |
| True | False | False |
| False | True | False |
| False | False | False |

**Opérateur OR :**
| A | B | A or B |
|---|---|--------|
| True | True | True |
| True | False | True |
| False | True | True |
| False | False | False |

**Opérateur NOT :**
| A | not A |
|---|-------|
| True | False |
| False | True |

### Opérateurs de chaînes

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `+` | Concaténation | `'Hello' + ' ' + 'World'` → `'Hello World'` |

**Exemples :**
```pascal
var
  Prenom, Nom, NomComplet: string;
  Message: string;
begin
  Prenom := 'Marie';
  Nom := 'Dupont';

  // Concaténation simple
  NomComplet := Prenom + ' ' + Nom;  // 'Marie Dupont'

  // Concaténation avec des variables et du texte
  Message := 'Bonjour ' + Prenom + ', bienvenue !';

  // Concaténation avec des nombres (conversion automatique)
  Message := 'Vous avez ' + IntToStr(25) + ' ans';
end;
```

### Opérateurs d'affectation

| Opérateur | Description | Équivalent |
|-----------|-------------|------------|
| `:=` | Affectation simple | `X := 5` |
| `+=` | Addition et affectation | `X := X + 5` |
| `-=` | Soustraction et affectation | `X := X - 5` |
| `*=` | Multiplication et affectation | `X := X * 5` |
| `/=` | Division et affectation | `X := X / 5` |

**Note :** Les opérateurs composés (`+=`, `-=`, etc.) sont disponibles depuis Delphi 2010.

**Exemples :**
```pascal
var
  Compteur: Integer;
  Total: Double;
begin
  Compteur := 10;
  Compteur += 5;   // Équivalent à : Compteur := Compteur + 5;  // 15
  Compteur -= 3;   // Équivalent à : Compteur := Compteur - 3;  // 12
  Compteur *= 2;   // Équivalent à : Compteur := Compteur * 2;  // 24

  Total := 100.0;
  Total /= 4;      // Équivalent à : Total := Total / 4;  // 25.0
end;
```

### Opérateurs d'incrémentation et décrémentation

```pascal
var
  Compteur: Integer;
begin
  Compteur := 10;

  Inc(Compteur);      // Compteur := Compteur + 1;  // 11
  Inc(Compteur, 5);   // Compteur := Compteur + 5;  // 16

  Dec(Compteur);      // Compteur := Compteur - 1;  // 15
  Dec(Compteur, 3);   // Compteur := Compteur - 3;  // 12
end;
```

**Avantage :** `Inc` et `Dec` sont légèrement plus rapides que `+=` et `-=`.

### Opérateurs sur les bits

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `and` | ET bit à bit | `12 and 10` → 8 |
| `or` | OU bit à bit | `12 or 10` → 14 |
| `xor` | OU exclusif bit à bit | `12 xor 10` → 6 |
| `not` | Complément bit à bit | `not 12` |
| `shl` | Décalage à gauche | `5 shl 2` → 20 |
| `shr` | Décalage à droite | `20 shr 2` → 5 |

**Exemples :**
```pascal
var
  A, B, Resultat: Integer;
begin
  A := 12;  // En binaire : 1100
  B := 10;  // En binaire : 1010

  Resultat := A and B;  // 1000 = 8
  Resultat := A or B;   // 1110 = 14
  Resultat := A xor B;  // 0110 = 6

  // Décalages (multiplication/division par puissances de 2)
  Resultat := 5 shl 2;  // 5 * 2² = 20
  Resultat := 20 shr 2; // 20 / 2² = 5
end;
```

### Opérateur d'appartenance

L'opérateur `in` teste si une valeur appartient à un ensemble :

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  Jour: TJourSemaine;
  EstWeekend: Boolean;
begin
  Jour := Samedi;

  EstWeekend := Jour in [Samedi, Dimanche];  // True

  if Jour in [Lundi..Vendredi] then
    ShowMessage('Jour ouvrable')
  else
    ShowMessage('Week-end');
end;
```

### Opérateur is

Vérifie si un objet est d'un type particulier :

```pascal
if MonComposant is TButton then
  ShowMessage('C''est un bouton');
```

### Opérateur as

Convertit un objet vers un type spécifique :

```pascal
var
  MonBouton: TButton;
begin
  if MonComposant is TButton then
    MonBouton := MonComposant as TButton;
end;
```

## 3.3.1 L'opérateur ternaire en Delphi 13

### Nouveauté Delphi 13 Florence

Delphi 13 introduit l'**opérateur ternaire** (également appelé opérateur conditionnel), une fonctionnalité très attendue par la communauté. Cet opérateur permet d'écrire des expressions conditionnelles de manière plus concise.

### Syntaxe

```pascal
resultat := si condition alors valeur_si_vrai sinon valeur_si_faux;
```

En Delphi 13, la syntaxe est :

```pascal
resultat := if condition then valeur_si_vrai else valeur_si_faux;
```

### Exemples d'utilisation

#### Exemple simple

**Avant Delphi 13 :**
```pascal
var
  Age: Integer;
  Statut: string;
begin
  Age := 20;

  if Age >= 18 then
    Statut := 'Majeur'
  else
    Statut := 'Mineur';
end;
```

**Avec Delphi 13 :**
```pascal
var
  Age: Integer;
  Statut: string;
begin
  Age := 20;

  Statut := if Age >= 18 then 'Majeur' else 'Mineur';
end;
```

#### Exemple avec calculs

```pascal
var
  Quantite: Integer;
  PrixUnitaire: Double;
  Remise: Double;
begin
  Quantite := 15;
  PrixUnitaire := 10.0;

  // Application d'une remise si quantité >= 10
  Remise := if Quantite >= 10 then 0.15 else 0.0;

  // Calcul direct du prix
  Total := Quantite * PrixUnitaire * (1 - Remise);
end;
```

#### Exemple avec chaînes de caractères

```pascal
var
  EstConnecte: Boolean;
  Message: string;
begin
  EstConnecte := True;

  Message := if EstConnecte then 'Bienvenue !' else 'Veuillez vous connecter';
  ShowMessage(Message);
end;
```

#### Opérateurs ternaires imbriqués

```pascal
var
  Note: Integer;
  Mention: string;
begin
  Note := 15;

  Mention := if Note >= 16 then 'Très bien'
             else if Note >= 14 then 'Bien'
             else if Note >= 12 then 'Assez bien'
             else if Note >= 10 then 'Passable'
             else 'Insuffisant';

  ShowMessage('Mention : ' + Mention);
end;
```

#### Utilisation dans des affectations de propriétés

```pascal
// Activer/désactiver un bouton selon une condition
Button1.Enabled := if Edit1.Text <> '' then True else False;

// Version simplifiée (le résultat est déjà booléen)
Button1.Enabled := Edit1.Text <> '';

// Changer la couleur selon une condition
Panel1.Color := if Erreur then clRed else clGreen;
```

#### Utilisation dans des appels de fonctions

```pascal
ShowMessage(if Valide then 'Données valides' else 'Erreur de validation');

Resultat := CalculerTotal(if AppliquerTVA then TauxTVA else 0);
```

### Avantages de l'opérateur ternaire

1. **Code plus concis** : Réduit le nombre de lignes
2. **Lisibilité améliorée** : Pour les conditions simples
3. **Expressions fonctionnelles** : Permet d'utiliser des conditions dans des expressions
4. **Modernisation** : Aligne Delphi avec d'autres langages modernes

### Quand utiliser l'opérateur ternaire ?

**✅ Utilisez-le pour :**
- Affectations simples basées sur une condition
- Valeurs par défaut conditionnelles
- Messages ou textes conditionnels
- Conditions simples et évidentes

**❌ Évitez-le pour :**
- Logique complexe avec plusieurs conditions
- Code qui nécessite des explications détaillées
- Situations où un `if...else` traditionnel est plus clair

### Comparaison : if traditionnel vs opérateur ternaire

```pascal
// Cas simple : opérateur ternaire préférable
Couleur := if Actif then clGreen else clGray;

// Cas complexe : if traditionnel préférable
if (Utilisateur.EstConnecte) and (Utilisateur.ALesDroits) and (not Utilisateur.EstBloque) then
begin
  AfficherTableauDeBord;
  ChargerDonnees;
  InitialiserSession;
end
else
begin
  AfficherPageConnexion;
  LoggerTentative;
end;
```

## Priorité des opérateurs

Lorsque plusieurs opérateurs sont utilisés dans une expression, ils sont évalués selon leur priorité :

| Priorité | Opérateurs | Type |
|----------|------------|------|
| 1 (haute) | `not`, `@` | Unaires |
| 2 | `*`, `/`, `div`, `mod`, `and`, `shl`, `shr`, `as` | Multiplicatifs |
| 3 | `+`, `-`, `or`, `xor` | Additifs |
| 4 | `=`, `<>`, `<`, `>`, `<=`, `>=`, `in`, `is` | Relationnels |
| 5 (basse) | Opérateur ternaire | Conditionnel |

**Exemples :**
```pascal
var
  Resultat: Integer;
begin
  Resultat := 5 + 3 * 2;      // 11 (pas 16) : * avant +
  Resultat := (5 + 3) * 2;    // 16 : parenthèses en priorité

  if (X > 0) and (Y > 0) then // Bonnes pratiques : utiliser des parenthèses
    ShowMessage('Positif');
end;
```

**Conseil :** En cas de doute, utilisez des parenthèses pour clarifier l'ordre d'évaluation.

## Bonnes pratiques

### Nommage des variables

```pascal
// ✅ Bonnes pratiques
var
  NombreUtilisateurs: Integer;
  PrixTotalHT: Double;
  EstValide: Boolean;
  MessageErreur: string;

// ❌ Mauvaises pratiques
var
  x: Integer;           // Nom trop vague
  p: Double;            // Pas explicite
  flag: Boolean;        // Pas clair
  str: string;          // Nom générique
```

### Initialisation

```pascal
// ✅ Toujours initialiser les variables
var
  Compteur: Integer = 0;
  Total: Double = 0.0;
  Nom: string = '';

// ❌ Variable non initialisée (valeur imprévisible)
var
  Compteur: Integer;
begin
  Compteur := Compteur + 1;  // DANGER : Compteur a une valeur aléatoire
end;
```

### Utilisation des constantes

```pascal
// ✅ Utiliser des constantes pour les valeurs fixes
const
  TAUX_TVA = 0.20;
  NB_MAX_TENTATIVES = 3;
  MSG_ERREUR = 'Une erreur est survenue';

// ❌ Valeurs "magiques" dans le code
if Tentatives > 3 then  // Que représente 3 ?
  ShowMessage('Une erreur est survenue');
```

### Opérateurs logiques

```pascal
// ✅ Utiliser des parenthèses pour la clarté
if (Age >= 18) and (APermis) then

// ✅ Simplifier les expressions booléennes
EstValide := (Champ <> '');  // Au lieu de : if Champ <> '' then EstValide := True else EstValide := False;

// ✅ Éviter les doubles négations
if EstActif then  // Au lieu de : if not (not EstActif) then
```

## Erreurs courantes à éviter

### Confusion entre = et :=

```pascal
// ❌ ERREUR : = est pour la comparaison
X = 5;  // Erreur de syntaxe

// ✅ CORRECT : := est pour l'affectation
X := 5;

// ✅ CORRECT : = pour les comparaisons
if X = 5 then
  ShowMessage('X vaut 5');
```

### Division entière vs division réelle

```pascal
var
  A, B: Integer;
  Resultat: Double;
begin
  A := 5;
  B := 2;

  // ❌ Division entière (résultat tronqué)
  Resultat := A div B;  // Resultat = 2

  // ✅ Division réelle
  Resultat := A / B;    // Resultat = 2.5
end;
```

### Modification de constantes

```pascal
const
  MaxValeur = 100;

begin
  MaxValeur := 200;  // ❌ ERREUR : impossible de modifier une constante
end;
```

## Points clés à retenir

1. Utilisez `:=` pour l'affectation et `=` pour la comparaison
2. Initialisez toujours vos variables
3. Privilégiez les constantes pour les valeurs fixes
4. Choisissez des noms de variables explicites
5. Utilisez les parenthèses pour clarifier les expressions complexes
6. L'opérateur ternaire (Delphi 13) simplifie les affectations conditionnelles
7. Utilisez `Inc` et `Dec` pour les incrémentations simples
8. Les opérateurs composés (`+=`, `-=`) rendent le code plus concis

---

La maîtrise des variables, constantes et opérateurs est essentielle pour écrire du code efficace et maintenable. Dans la section suivante, nous découvrirons les structures de contrôle qui permettent de diriger le flux d'exécution de vos programmes.

⏭️ [L'opérateur ternaire en Delphi 13](/03-langage-object-pascal/03.1-operateur-ternaire.md)
