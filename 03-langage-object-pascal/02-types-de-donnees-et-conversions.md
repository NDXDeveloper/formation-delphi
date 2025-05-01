# 3.2 Types de données et conversions

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La manipulation des données est au cœur de tout programme. Dans cette section, nous explorerons les différents types de données disponibles dans le langage Object Pascal et comment les convertir d'un type à un autre.

## Types de données fondamentaux

Delphi offre un large éventail de types de données pour répondre à différents besoins. Voici les principaux types que vous utiliserez fréquemment :

### Types numériques entiers

Ces types stockent des nombres entiers (sans décimales) :

| Type | Plage | Taille en mémoire |
|------|-------|-------------------|
| `Byte` | 0 à 255 | 1 octet |
| `ShortInt` | -128 à 127 | 1 octet |
| `Word` | 0 à 65 535 | 2 octets |
| `SmallInt` | -32 768 à 32 767 | 2 octets |
| `Cardinal` | 0 à 4 294 967 295 | 4 octets |
| `Integer` | -2 147 483 648 à 2 147 483 647 | 4 octets |
| `Int64` | Très grand (±9,2×10¹⁸) | 8 octets |
| `UInt64` | 0 à très grand (1,8×10¹⁹) | 8 octets |

```pascal
var
  Age: Byte;        // Pour de petites valeurs positives
  Compteur: Integer; // Type entier général le plus courant
  GrandNombre: Int64; // Pour les très grands nombres entiers
```

### Types numériques à virgule flottante

Ces types stockent des nombres avec une partie décimale :

| Type | Précision | Plage approximative | Taille |
|------|-----------|---------------------|--------|
| `Single` | ~7 chiffres | ±1,5×10⁻⁴⁵ à ±3,4×10³⁸ | 4 octets |
| `Double` | ~15-16 chiffres | ±5,0×10⁻³²⁴ à ±1,7×10³⁰⁸ | 8 octets |
| `Extended` | ~19-20 chiffres | ±3,6×10⁻⁴⁹⁵¹ à ±1,1×10⁴⁹³² | 10 octets |
| `Currency` | 4 décimales fixes | ±922 337 203 685 477,5807 | 8 octets |

```pascal
var
  Prix: Double;      // Pour la plupart des calculs avec décimales
  Montant: Currency; // Idéal pour les calculs financiers (évite les erreurs d'arrondi)
```

La `Currency` est particulièrement utile pour les calculs financiers car elle évite les erreurs d'arrondi courantes avec les nombres à virgule flottante.

### Type booléen

Le type `Boolean` peut prendre deux valeurs :

```pascal
var
  EstValide: Boolean; // Peut valoir True ou False
```

Il existe aussi des variantes comme `ByteBool`, `WordBool` et `LongBool` qui ont la même fonction mais une taille en mémoire différente.

### Type caractère

Pour stocker un caractère unique :

```pascal
var
  Lettre: Char;          // Un caractère Unicode (2 octets)
  CaractereANSI: AnsiChar; // Un caractère ANSI (1 octet)
```

### Types chaîne de caractères

Pour stocker du texte :

```pascal
var
  Prenom: string;         // Chaîne Unicode moderne (dynamique)
  TexteAncien: AnsiString; // Chaîne ANSI (pour la compatibilité)
  TexteCourt: ShortString; // Limitée à 255 caractères
```

Le type `string` est le plus couramment utilisé et a une longueur dynamique (s'adapte à son contenu).

### Type date et heure

```pascal
var
  Aujourd'hui: TDate;      // Date seule
  Maintenant: TTime;       // Heure seule
  DateHeure: TDateTime;    // Date et heure combinées
```

Sous le capot, `TDateTime` est en fait un `Double` où la partie entière représente les jours (depuis le 30/12/1899) et la partie décimale représente l'heure.

## Types structurés

### Tableaux

Les tableaux permettent de stocker plusieurs valeurs du même type :

```pascal
var
  // Tableau statique (taille fixe)
  Notes: array[1..5] of Integer;

  // Tableau dynamique (taille variable)
  Etudiants: array of string;
```

Pour utiliser un tableau dynamique :

```pascal
// Définir la taille
SetLength(Etudiants, 10);  // Tableau de 10 éléments

// Accéder aux éléments (les indices commencent à 0)
Etudiants[0] := 'Alice';
Etudiants[1] := 'Bob';
```

### Enregistrements (Records)

Les records permettent de regrouper des données de différents types en une seule structure :

```pascal
type
  TPersonne = record
    Nom: string;
    Prenom: string;
    Age: Integer;
    EstActif: Boolean;
  end;

var
  Employe: TPersonne;
```

Pour accéder aux champs :

```pascal
Employe.Nom := 'Dupont';
Employe.Prenom := 'Jean';
Employe.Age := 42;
Employe.EstActif := True;
```

Delphi 12 supporte également les initialisateurs d'enregistrements :

```pascal
Employe := TPersonne.Create('Dupont', 'Jean', 42, True);
```
<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur**</span>

### Ensembles (Sets)

Les ensembles permettent de stocker un groupe de valeurs du même type ordinal :

```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJoursOuvres = set of TJourSemaine;

var
  JoursTravailles: TJoursOuvres;
```

Utilisation :

```pascal
// Initialisation d'un ensemble
JoursTravailles := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

// Test d'appartenance
if Samedi in JoursTravailles then
  ShowMessage('Le samedi est travaillé')
else
  ShowMessage('Le samedi n''est pas travaillé');
```

## Types pointeurs

Les pointeurs stockent des adresses mémoire. Ils sont généralement utilisés dans des cas avancés :

```pascal
var
  MonEntier: Integer;
  PointeurEntier: ^Integer; // Déclaration d'un pointeur vers un entier
```

Utilisation :

```pascal
MonEntier := 42;
PointeurEntier := @MonEntier; // @ donne l'adresse de MonEntier
ShowMessage(IntToStr(PointeurEntier^)); // ^ accède à la valeur pointée
```

> 🔹 **Note pour débutants**: Les pointeurs sont un concept avancé. Vous pouvez programmer en Delphi pendant longtemps sans avoir à les utiliser directement, car le langage offre des mécanismes de plus haut niveau.

## Conversions de types

La conversion de types est essentielle lorsque vous travaillez avec différents types de données. Voici les principales méthodes :

### Conversions numériques

```pascal
var
  MonEntier: Integer;
  MonReel: Double;
begin
  // Conversion entier vers réel (implicite, sans risque)
  MonEntier := 42;
  MonReel := MonEntier; // Conversion automatique

  // Conversion réel vers entier (explicite, avec risque de perte de précision)
  MonReel := 42.75;
  MonEntier := Trunc(MonReel); // Tronque à 42 (supprime la partie décimale)
  // ou
  MonEntier := Round(MonReel); // Arrondit à 43 (au plus proche)
end;
```

### Conversions avec des chaînes

Delphi offre plusieurs fonctions pour convertir des valeurs numériques en chaînes et vice-versa :

```pascal
var
  Texte: string;
  Nombre: Integer;
  Reel: Double;
begin
  // De numérique vers chaîne
  Nombre := 42;
  Texte := IntToStr(Nombre); // Convertit un entier en chaîne

  Reel := 3.14159;
  Texte := FloatToStr(Reel); // Convertit un réel en chaîne
  // ou avec formatage
  Texte := Format('%.2f', [Reel]); // '3.14'

  // De chaîne vers numérique
  Texte := '42';
  Nombre := StrToInt(Texte); // Convertit une chaîne en entier
  // ou en sécurité
  if TryStrToInt(Texte, Nombre) then
    ShowMessage('Conversion réussie: ' + IntToStr(Nombre))
  else
    ShowMessage('Erreur de conversion');

  Texte := '3.14';
  Reel := StrToFloat(Texte); // Convertit une chaîne en réel
  // ou en sécurité
  if TryStrToFloat(Texte, Reel) then
    ShowMessage('Conversion réussie: ' + FloatToStr(Reel))
  else
    ShowMessage('Erreur de conversion');
end;
```

### Conversions de dates

```pascal
var
  DateTexte: string;
  MaDate: TDateTime;
begin
  // De date vers chaîne
  MaDate := Now; // Date et heure actuelles
  DateTexte := DateToStr(MaDate); // Format date uniquement
  DateTexte := TimeToStr(MaDate); // Format heure uniquement
  DateTexte := DateTimeToStr(MaDate); // Format date et heure

  // Avec formatage personnalisé
  DateTexte := FormatDateTime('dd/mm/yyyy hh:nn:ss', MaDate);

  // De chaîne vers date
  DateTexte := '01/05/2023';
  MaDate := StrToDate(DateTexte);
  // ou en sécurité
  if TryStrToDate(DateTexte, MaDate) then
    ShowMessage('Date valide')
  else
    ShowMessage('Format de date invalide');
end;
```

### Cast de types (Transtypage)

Le cast de types permet de convertir explicitement une variable d'un type à un autre compatible :

```pascal
var
  O: TObject;
  B: TButton;
begin
  O := Button1; // Button1 est un contrôle sur la forme

  // Cast pour récupérer l'objet sous sa vraie classe
  if O is TButton then // Vérification du type avec "is"
    B := TButton(O);   // Cast avec TButton(...)
end;
```

## Types génériques

Delphi supporte également la généricité, permettant de créer des structures ou classes qui peuvent fonctionner avec différents types :

```pascal
// Exemple simple avec une liste générique
var
  ListeEntiers: TList<Integer>;
  ListeChaines: TList<string>;
begin
  ListeEntiers := TList<Integer>.Create;
  try
    ListeEntiers.Add(42);
    ListeEntiers.Add(123);

    ShowMessage(IntToStr(ListeEntiers[0])); // Affiche 42
  finally
    ListeEntiers.Free; // Libération de la mémoire
  end;
end;
```

Les génériques sont un sujet avancé qui sera couvert plus en détail dans une section ultérieure.

## Conseils pratiques

1. **Choix du bon type** :
   - Utilisez `Integer` pour les nombres entiers généraux
   - Utilisez `Double` pour les calculs scientifiques
   - Utilisez `Currency` pour les calculs financiers
   - Utilisez `string` pour la plupart des manipulations de texte

2. **Vérifiez les conversions** :
   - Utilisez toujours les fonctions `Try...` pour les conversions de chaînes en nombres ou dates
   - Soyez conscient des pertes potentielles de précision lors de la conversion de réels en entiers

3. **Déclaration explicite** :
   - Déclarez toujours explicitement le type de vos variables pour améliorer la lisibilité et éviter les erreurs

4. **Constantes typées** :
   - Utilisez des constantes typées pour améliorer la maintenabilité :
   ```pascal
   const
     PI: Double = 3.14159265358979;
     MAX_ETUDIANTS: Integer = 100;
   ```

---

Vous avez maintenant une bonne compréhension des types de données disponibles dans Delphi et des méthodes pour les convertir. Dans la prochaine section, nous explorerons les variables, constantes et opérateurs qui vous permettront de manipuler ces données de manière efficace.

⏭️ [Variables, constantes et opérateurs](/03-langage-object-pascal/03-variables-constantes-et-operateurs.md)
