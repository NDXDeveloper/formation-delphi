🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.2 Types de données et conversions

## Introduction

Un type de données définit la nature de l'information qu'une variable peut contenir et les opérations possibles sur cette information. Object Pascal est un langage **fortement typé**, ce qui signifie que chaque variable doit avoir un type déclaré explicitement. Cette rigueur permet de détecter de nombreuses erreurs dès la compilation.

## Les types de données de base

### Types entiers

Les types entiers permettent de stocker des nombres sans décimale. Object Pascal propose plusieurs types entiers selon la plage de valeurs et l'espace mémoire nécessaire.

| Type | Taille | Plage de valeurs |
|------|--------|------------------|
| `Byte` | 1 octet | 0 à 255 |
| `ShortInt` | 1 octet | -128 à 127 |
| `Word` | 2 octets | 0 à 65 535 |
| `SmallInt` | 2 octets | -32 768 à 32 767 |
| `Cardinal` | 4 octets | 0 à 4 294 967 295 |
| `Integer` | 4 octets | -2 147 483 648 à 2 147 483 647 |
| `Int64` | 8 octets | -9 223 372 036 854 775 808 à 9 223 372 036 854 775 807 |
| `UInt64` | 8 octets | 0 à 18 446 744 073 709 551 615 |
| `NativeInt` / `NativeUInt` | 4 octets en 32 bits, 8 octets en 64 bits | s'adapte à la plateforme |

> 💡 **NativeInt / NativeUInt** : Ces types entiers s'adaptent automatiquement à la taille de mot du processeur cible. Pratiques pour stocker des adresses mémoire ou des handles de manière portable entre Win32 et Win64.

**Exemples :**
```pascal
var
  Age: Integer;
  Population: Cardinal;
  Compteur: Byte;
begin
  Age := 25;
  Population := 67000000;
  Compteur := 0;
end;
```

**Type recommandé :** Pour la plupart des usages, `Integer` est le choix par défaut car il offre un bon compromis entre plage et performance.

### Types réels (nombres à virgule)

Les types réels permettent de stocker des nombres avec décimales.

| Type | Taille | Précision | Plage approximative |
|------|--------|-----------|---------------------|
| `Single` | 4 octets | 7-8 chiffres | 1,5 × 10⁻⁴⁵ à 3,4 × 10³⁸ |
| `Double` | 8 octets | 15-16 chiffres | 5,0 × 10⁻³²⁴ à 1,7 × 10³⁰⁸ |
| `Extended` | 10 octets (Win32) / 8 octets (Win64) | 19-20 chiffres (Win32) / 15-16 (Win64) | dépend de la plateforme |
| `Currency` | 8 octets | 4 décimales fixes | -922 337 203 685 477,5808 à 922 337 203 685 477,5807 |

> ⚠️ **Attention `Extended`** : Sur les plateformes 64 bits (Win64, macOS, Linux, iOS, Android), `Extended` est en réalité un alias de `Double` (8 octets) pour des raisons de compatibilité. Si vous avez besoin d'une précision étendue garantie, utilisez `Double` directement ou consultez la DocWiki Embarcadero pour des alternatives. La précision « 10 octets » historique n'existe que sur Win32.

**Exemples :**
```pascal
var
  Prix: Double;
  Taux: Single;
  MontantFacture: Currency;
begin
  Prix := 19.99;
  Taux := 0.055;
  MontantFacture := 1234.56;
end;
```

**Points importants :**
- Utilisez `Double` pour les calculs scientifiques généraux
- Utilisez `Currency` pour les calculs monétaires (évite les erreurs d'arrondi)
- Le séparateur décimal **dans le code source** est toujours le point (`.`), pas la virgule — c'est une convention universelle en programmation, même quand l'utilisateur final voit `19,99 €` à l'écran

```pascal
Prix := 19.99;   // ✅ Correct : point décimal dans le code
// Prix := 19,99;   // ❌ Erreur de syntaxe : la virgule est un séparateur de paramètres
```

### Type booléen

Le type `Boolean` ne peut avoir que deux valeurs : `True` (vrai) ou `False` (faux).

```pascal
var
  EstActif: Boolean;
  EstMajeur: Boolean;
begin
  EstActif := True;
  EstMajeur := False;

  if EstActif then
    ShowMessage('L''utilisateur est actif');
end;
```

**Variantes :**
- `Boolean` : type standard (1 octet)
- `ByteBool`, `WordBool`, `LongBool` : compatibilité avec d'autres langages

### Type caractère

Les types caractères permettent de stocker un seul caractère.

| Type | Description | Exemple |
|------|-------------|---------|
| `Char` | Caractère Unicode (2 octets) | `'A'`, `'é'`, `'€'` |
| `AnsiChar` | Caractère ANSI (1 octet) | `'A'`, `'B'` |

**Exemple :**
```pascal
var
  Initiale: Char;
  Grade: Char;
begin
  Initiale := 'M';
  Grade := 'A';
end;
```

**Note :** Les caractères littéraux sont entourés d'apostrophes simples (`'`).

### Type chaîne de caractères

Les chaînes de caractères (`string`) permettent de stocker du texte.

```pascal
var
  Nom: string;
  Prenom: string;
  Message: string;
begin
  Nom := 'Dupont';
  Prenom := 'Marie';
  Message := 'Bonjour ' + Prenom + ' ' + Nom + ' !';
  ShowMessage(Message); // Affiche : Bonjour Marie Dupont !
end;
```

**Types de chaînes :**
- `string` : chaîne Unicode dynamique, taille variable (type recommandé). Depuis Delphi 2009, `string` est un alias de `UnicodeString` (encodage UTF-16).
- `AnsiString` : chaîne dynamique encodée selon une page de code (legacy, à éviter dans le code nouveau).
- `WideString` : chaîne Unicode large utilisée principalement pour l'interopérabilité COM/OLE sous Windows.
- `ShortString` : chaîne à longueur variable mais **limitée à 255 caractères maximum**, héritée du Pascal historique (taille codée sur 1 octet en tête). Rarement utile aujourd'hui.

**Opérations courantes :**
```pascal
var
  Texte: string;
  Longueur: Integer;
begin
  Texte := 'Delphi';
  Longueur := Length(Texte);        // Retourne 6
  Texte := UpperCase(Texte);        // Convertit en majuscules : 'DELPHI'
  Texte := LowerCase(Texte);        // Convertit en minuscules : 'delphi'
  Texte := Trim('  Espace  ');      // Supprime les espaces : 'Espace'
end;
```

## Types énumérés

Les types énumérés permettent de définir un ensemble de valeurs nommées.

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TCouleur = (Rouge, Vert, Bleu, Jaune);

var
  Jour: TJourSemaine;
  Couleur: TCouleur;
begin
  Jour := Lundi;
  Couleur := Rouge;

  if Jour = Samedi then
    ShowMessage('C''est le week-end !');
end;
```

### Valeur ordinale d'une énumération

Chaque valeur d'une énumération possède une **valeur ordinale** entière, à partir de 0 :

```pascal
var
  Jour: TJourSemaine;
begin
  Jour := Mercredi;
  ShowMessage(IntToStr(Ord(Jour)));    // Affiche 2 (Lundi=0, Mardi=1, Mercredi=2…)
  ShowMessage(IntToStr(Ord(Dimanche))); // Affiche 6
end;
```

Vous pouvez parcourir une énumération avec `Low` et `High` :

```pascal
for Jour := Low(TJourSemaine) to High(TJourSemaine) do
  ShowMessage(IntToStr(Ord(Jour)));
```

### Valeurs ordinales explicites

Vous pouvez aussi forcer certaines valeurs ordinales (utile pour la sérialisation ou la compatibilité binaire) :

```pascal
type
  TStatutCommande = (
    scNouvelle    = 1,
    scValidee     = 2,
    scExpediee    = 5,
    scLivree      = 10,
    scAnnulee     = 99
  );
```

> ⚠️ Lorsqu'on attribue des valeurs explicites, l'énumération devient une **énumération non contiguë**. Certaines fonctionnalités comme les `set of` deviennent alors impossibles (un `set` nécessite que les ordinaux tiennent dans la plage 0..255). Réservez cette technique aux cas où la valeur numérique est imposée par un protocole externe.

**Avantages :** Les énumérations rendent le code plus lisible et évitent les erreurs avec des valeurs arbitraires.

## Type ensemble (Set)

Un ensemble est une collection de valeurs d'un type énuméré ou ordinal.

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJoursTravail = set of TJourSemaine;

var
  JoursOuvrables: TJoursTravail;
begin
  JoursOuvrables := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  if Samedi in JoursOuvrables then
    ShowMessage('Samedi est un jour ouvrable')
  else
    ShowMessage('Samedi n''est pas un jour ouvrable');
end;
```

### Opérations sur les ensembles

Les ensembles supportent des opérations algébriques très expressives :

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `+` | Union | `[1, 2] + [2, 3]` → `[1, 2, 3]` |
| `-` | Différence | `[1, 2, 3] - [2]` → `[1, 3]` |
| `*` | Intersection | `[1, 2, 3] * [2, 3, 4]` → `[2, 3]` |
| `<=` | Inclusion | `[1, 2] <= [1, 2, 3]` → `True` |
| `>=` | Surensemble | `[1, 2, 3] >= [1, 2]` → `True` |
| `in` | Appartenance | `2 in [1, 2, 3]` → `True` |

```pascal
var
  Permissions, Lecture, Ecriture: set of (pRead, pWrite, pExecute);
begin
  Lecture   := [pRead];
  Ecriture  := [pWrite];

  Permissions := Lecture + Ecriture;       // Union : [pRead, pWrite]
  Permissions := Permissions - [pWrite];   // Retrait : [pRead]
end;
```

> 💡 **Limite des ensembles** : Un type `set of` doit avoir des ordinaux entre 0 et 255 (taille maximale 32 octets). Au-delà, utilisez `TDictionary<T, Boolean>` ou un autre conteneur.

## Type pointeur

Un pointeur contient l'adresse mémoire d'une variable.

```pascal
var
  Valeur: Integer;
  PValeur: ^Integer;  // Pointeur vers un entier (pointeur typé)
  PGenerique: Pointer; // Pointeur non typé (peut référencer n'importe quoi)
begin
  Valeur := 42;
  PValeur := @Valeur;        // @ retourne l'adresse de Valeur
  ShowMessage(IntToStr(PValeur^));  // ^ déréférence le pointeur : affiche 42

  PGenerique := @Valeur;     // Pointer accepte n'importe quelle adresse
end;
```

**Variantes de pointeurs :**
- `Pointer` : pointeur non typé, équivalent du `void*` en C
- `^TType` : pointeur typé (recommandé car contrôlé par le compilateur)
- `PInteger`, `PChar`, `PByte`, etc. : alias prédéfinis (`PInteger = ^Integer`)

**Note pour débutants :** Les pointeurs sont un concept avancé. Dans la plupart des cas en programmation Delphi moderne, vous n'aurez pas besoin de les manipuler directement : les objets (`TObject` et ses descendants) sont déjà des références sous-jacentes.

## Type Variant

Le type `Variant` peut contenir différents types de données.

```pascal
var
  V: Variant;
begin
  V := 42;              // Entier
  V := 'Texte';         // Chaîne
  V := 3.14;            // Réel
  V := True;            // Booléen
end;
```

**Attention :** Utilisez `Variant` avec parcimonie. Il est moins performant et moins sûr que les types fortement typés.

## Conversions de types

### Conversions automatiques (implicites)

Certaines conversions se font automatiquement lorsqu'il n'y a pas de perte de données :

```pascal
var
  EntierCourt: SmallInt;
  EntierLong: Integer;
  NombreReel: Double;
begin
  EntierCourt := 100;
  EntierLong := EntierCourt;      // OK : SmallInt → Integer
  NombreReel := EntierLong;       // OK : Integer → Double
end;
```

### Conversions explicites (cast)

Pour les autres conversions, vous devez les effectuer explicitement.

#### Fonctions de conversion courantes

**Vers chaîne de caractères :**
```pascal
var
  Nombre: Integer;
  Prix: Double;
  Texte: string;
begin
  Nombre := 42;
  Texte := IntToStr(Nombre);        // Integer → String

  Prix := 19.99;
  Texte := FloatToStr(Prix);        // Double → String
  Texte := FormatFloat('0.00', Prix); // Formatage personnalisé : '19.99'
end;
```

**Depuis chaîne de caractères :**
```pascal
var
  Texte: string;
  Nombre: Integer;
  Prix: Double;
begin
  Texte := '42';
  Nombre := StrToInt(Texte);        // String → Integer

  Texte := '19,99';                 // ⚠ Séparateur de la locale française
  Prix := StrToFloat(Texte);        // OK en locale fr-FR, échoue en en-US
end;
```

> ⚠️ **Piège locale-dépendant de `StrToFloat`** : `StrToFloat` utilise le `DecimalSeparator` de la locale système. En **français**, c'est la virgule (`'19,99'`), en **anglais** le point (`'19.99'`). Pour lire une chaîne au format **fixe** (point décimal, indépendant de la locale — pratique pour les fichiers, JSON, XML…), passez `TFormatSettings.Invariant` :
>
> ```pascal
> uses System.SysUtils;
>
> Prix := StrToFloat('19.99', TFormatSettings.Invariant);  // Toujours OK
> ```

**Conversions avec gestion d'erreurs :**
```pascal
var
  Texte: string;
  Nombre: Integer;
begin
  Texte := 'abc';  // Pas un nombre valide

  if TryStrToInt(Texte, Nombre) then
    ShowMessage('Conversion réussie : ' + IntToStr(Nombre))
  else
    ShowMessage('Erreur de conversion');
end;
```

**Pourquoi utiliser TryStrToInt ?** Cette fonction ne génère pas d'exception en cas d'erreur, ce qui rend le code plus robuste.

#### Entre types numériques

```pascal
var
  Entier: Integer;
  Reel: Double;
begin
  // Double → Integer (troncature)
  Reel := 3.7;
  Entier := Trunc(Reel);    // Résultat : 3 (supprime la partie décimale)
  Entier := Round(Reel);    // Résultat : 4 (arrondit)

  // Integer → Double
  Entier := 42;
  Reel := Entier;           // Conversion automatique
end;
```

#### Fonctions d'arrondi

| Fonction | Description | Exemple |
|----------|-------------|---------|
| `Trunc(x)` | Supprime la partie décimale | `Trunc(3.7)` = 3 |
| `Round(x)` | Arrondit au plus proche | `Round(3.7)` = 4 |
| `Ceil(x)` | Arrondit au supérieur | `Ceil(3.2)` = 4 |
| `Floor(x)` | Arrondit à l'inférieur | `Floor(3.7)` = 3 |

> ⚠️ **Piège de `Round` : l'arrondi banquier** : Par défaut, `Round` utilise le mode *banker's rounding* (arrondi au pair le plus proche) lorsqu'il y a égalité (`.5`). Ainsi :
>
> - `Round(2.5)` retourne **2** (pas 3 !)
> - `Round(3.5)` retourne **4**
> - `Round(0.5)` retourne **0**
> - `Round(1.5)` retourne **2**
>
> Cette règle (norme IEEE 754) réduit le biais cumulatif sur de grands volumes de calculs. Pour obtenir l'arrondi « scolaire » (toujours au-dessus pour `.5`), utilisez `SimpleRoundTo` ou implémentez `Trunc(X + 0.5)` pour les positifs.

#### Conversions de booléens

```pascal
var
  Actif: Boolean;
  Valeur: Integer;
  Texte: string;
begin
  // Boolean → String
  Actif := True;
  Texte := BoolToStr(Actif, True);  // Retourne 'True'

  // String → Boolean
  Texte := 'True';
  Actif := StrToBool(Texte);

  // Boolean → Integer
  Valeur := Ord(Actif);  // True = 1, False = 0
end;
```

#### Conversions de caractères

```pascal
var
  Caractere: Char;
  Code: Integer;
begin
  // Char → Integer (code ASCII/Unicode)
  Caractere := 'A';
  Code := Ord(Caractere);  // Retourne 65

  // Integer → Char
  Code := 66;
  Caractere := Chr(Code);  // Retourne 'B'
end;
```

### Tableau récapitulatif des fonctions de conversion

| Depuis | Vers | Fonction |
|--------|------|----------|
| Integer | String | `IntToStr(valeur)` |
| String | Integer | `StrToInt(texte)` ou `TryStrToInt(texte, resultat)` |
| Double | String | `FloatToStr(valeur)` ou `FormatFloat(format, valeur)` |
| String | Double | `StrToFloat(texte)` ou `TryStrToFloat(texte, resultat)` |
| Boolean | String | `BoolToStr(valeur, UseBoolStrs)` |
| String | Boolean | `StrToBool(texte)` |
| Char | Integer | `Ord(caractere)` |
| Integer | Char | `Chr(code)` |
| Double | Integer | `Trunc(valeur)` ou `Round(valeur)` |

## Formatage de valeurs

### Formatage de nombres

```pascal
var
  Prix: Double;
  Texte: string;
begin
  Prix := 1234.56;

  // Différentes options de formatage
  Texte := FormatFloat('0.00', Prix);        // '1234.56' (séparateur dépend de la locale)
  Texte := FormatFloat('#,##0.00', Prix);    // '1 234,56' en fr-FR, '1,234.56' en en-US
  Texte := FormatFloat('0.00 €', Prix);      // '1234,56 €' en fr-FR
end;
```

> 💡 **Locale et séparateurs** : Les caractères `,` et `.` dans la chaîne de format sont des **marqueurs symboliques**, pas des caractères littéraux. Ils sont remplacés à l'exécution par les séparateurs de la locale courante (`FormatSettings.DecimalSeparator` et `FormatSettings.ThousandSeparator`). Pour forcer un format indépendant de la locale, passez un `TFormatSettings` explicite : `FormatFloat('0.00', Prix, FormatSettings)`.

### Formatage de dates

```pascal
var
  MaDate: TDateTime;
  Texte: string;
begin
  MaDate := Now;  // Date et heure actuelles

  Texte := DateToStr(MaDate);                    // '16/10/2025' (selon les paramètres régionaux Windows)
  Texte := FormatDateTime('dd/mm/yyyy', MaDate); // '16/10/2025'
  Texte := FormatDateTime('dd mmmm yyyy', MaDate); // '16 octobre 2025' (si la locale est française)
end;
```

> 💡 **Paramètres régionaux** : `DateToStr` utilise le format défini par les paramètres régionaux du système (Windows), tandis que `FormatDateTime` permet de spécifier explicitement le format. Pour un format indépendant de la locale, préférez `FormatDateTime` avec une chaîne de format explicite.

### Format avec paramètres multiples

La fonction `Format` permet de créer des chaînes avec plusieurs valeurs :

```pascal
var
  Nom: string;
  Age: Integer;
  Taille: Double;
  Message: string;
begin
  Nom := 'Marie';
  Age := 25;
  Taille := 1.68;

  Message := Format('%s a %d ans et mesure %.2f m', [Nom, Age, Taille]);
  // Résultat : 'Marie a 25 ans et mesure 1.68 m'
  ShowMessage(Message);
end;
```

**Spécificateurs de format courants :**
- `%s` : chaîne de caractères
- `%d` : entier décimal
- `%f` : nombre à virgule flottante
- `%.2f` : nombre avec 2 décimales

## Vérification de type

### Opérateur is

Permet de vérifier le type d'un objet :

```pascal
if MonObjet is TButton then
  ShowMessage('C''est un bouton');
```

### Opérateur as

Permet de convertir (cast) un objet vers un type spécifique :

```pascal
var
  MonBouton: TButton;
begin
  if MonObjet is TButton then
    MonBouton := MonObjet as TButton;
end;
```

> ⚠️ **Attention** : Si `MonObjet` n'est pas réellement un `TButton`, l'opérateur `as` lève une exception `EInvalidCast`. C'est pourquoi on précède classiquement `as` d'un test `is`. Pour un cast sans vérification (plus rapide mais dangereux), utilisez la syntaxe parenthésée : `MonBouton := TButton(MonObjet);` — mais alors, si le type ne correspond pas, vous obtiendrez un comportement indéfini.

## Erreurs courantes et pièges à éviter

### Dépassement de capacité

```pascal
var
  Petit: Byte;  // Max 255
begin
  Petit := 300;  // Dépassement : Petit vaudra 44 (300 mod 256) !
end;
```

**Solution :** Choisissez un type adapté à la plage de valeurs attendue.

> ⚠️ **Important** : Avec une **constante littérale** comme `300`, le compilateur détecte le dépassement à la compilation et refuse de compiler (`E2010 Constant expression violates subrange bounds`). Mais si la valeur provient d'un calcul à l'exécution (par exemple `Petit := A + B`), le dépassement n'est détecté **que si la directive `{$R+}` (range checking) est activée**. En mode Release, elle est généralement désactivée pour les performances : la valeur sera tronquée silencieusement.

### Conversion sans vérification

```pascal
var
  Texte: string;
  Nombre: Integer;
begin
  Texte := 'abc';
  Nombre := StrToInt(Texte);  // ERREUR : exception levée !
end;
```

**Solution :** Utilisez les fonctions `Try...` pour les conversions incertaines.

### Perte de précision

```pascal
var
  Reel: Double;
  Entier: Integer;
begin
  Reel := 3.7;
  Entier := Trunc(Reel);  // Entier = 3 (la décimale est perdue)
end;
```

**Solution :** Soyez conscient des pertes de données lors des conversions.

## Points clés à retenir

1. Object Pascal est un langage **fortement typé** : chaque variable a un type fixe
2. Choisissez le type de données adapté à vos besoins (plage, précision, performance)
3. `Integer` et `Double` sont les types par défaut pour les nombres
4. `string` est le type recommandé pour le texte
5. `Currency` est préférable pour les calculs monétaires
6. Utilisez les fonctions `Try...` pour les conversions avec gestion d'erreurs
7. Le formatage des valeurs améliore la présentation des données
8. Évitez le type `Variant` sauf nécessité absolue

## Conseils pratiques

- Déclarez toujours explicitement le type de vos variables
- Utilisez des types énumérés pour les valeurs qui ont un sens métier
- Préférez `TryStrToInt` à `StrToInt` pour éviter les exceptions
- Utilisez `Format` pour créer des messages complexes
- Documentez les plages de valeurs attendues dans vos commentaires
- Testez les conversions avec des valeurs limites

---

La maîtrise des types de données et des conversions est fondamentale pour écrire du code fiable et efficace. Dans la section suivante, nous verrons comment déclarer et utiliser des variables et des constantes.

⏭️ [Variables, constantes et opérateurs](/03-langage-object-pascal/03-variables-constantes-et-operateurs.md)
