# 3.10 Nouveautés de la syntaxe Object Pascal (dernières versions)

Le langage Object Pascal continue d'évoluer avec chaque nouvelle version de Delphi. Dans cette section, nous allons explorer les améliorations récentes de la syntaxe qui rendent le code plus concis, plus lisible et plus puissant. Ces nouveautés vous permettront d'écrire du code plus moderne tout en préservant la clarté et la robustesse qui font la réputation d'Object Pascal.

## Variables inline (Delphi 10.3 Rio)

Traditionnellement, en Object Pascal, toutes les variables devaient être déclarées au début d'un bloc. Depuis Delphi 10.3, vous pouvez désormais déclarer des variables à l'endroit où vous en avez besoin, ce qui rend le code plus lisible et réduit le risque d'erreurs.

### Ancienne approche
```pascal
procedure CalculerStatistiques(const Valeurs: array of Integer);
var
  Somme: Integer;
  Moyenne: Double;
  i: Integer;
begin
  Somme := 0;
  for i := 0 to High(Valeurs) do
    Somme := Somme + Valeurs[i];

  Moyenne := Somme / Length(Valeurs);
  ShowMessage('Moyenne : ' + FloatToStr(Moyenne));
end;
```

### Nouvelle approche
```pascal
procedure CalculerStatistiques(const Valeurs: array of Integer);
begin
  var Somme := 0;
  for var i := 0 to High(Valeurs) do
    Somme := Somme + Valeurs[i];

  var Moyenne := Somme / Length(Valeurs);
  ShowMessage('Moyenne : ' + FloatToStr(Moyenne));
end;
```

Les variables inline sont particulièrement utiles dans les boucles et les structures conditionnelles :

```pascal
if FileExists(NomFichier) then
begin
  var Contenu := TFile.ReadAllText(NomFichier);
  // Traitement du contenu...
end;
```

<span style="color: #0066CC">**Nécessite Delphi 10.3 ou supérieur**</span>

## Inférence de type

Accompagnant les variables inline, l'inférence de type permet au compilateur de déduire automatiquement le type d'une variable à partir de l'expression d'initialisation.

```pascal
// Le compilateur déduit que Compteur est un Integer
var Compteur := 1;

// Le compilateur déduit que Nom est une string
var Nom := 'Jean Dupont';

// Le compilateur déduit que Prix est un Double
var Prix := 19.99;

// Pour les tableaux
var Nombres := TArray<Integer>.Create(1, 2, 3, 4, 5);
```

Cela fonctionne également avec les objets :

```pascal
// Le compilateur déduit que Liste est un TStringList
var Liste := TStringList.Create;
```

<span style="color: #0066CC">**Nécessite Delphi 10.3 ou supérieur**</span>

## Opérateur ternaire (Delphi 10.3 Rio)

L'opérateur ternaire `?:` permet d'écrire des conditions simples de manière plus concise. Il évalue une condition et renvoie une valeur différente selon que la condition est vraie ou fausse.

### Ancienne approche
```pascal
var
  Age: Integer;
  Statut: string;
begin
  Age := 17;

  if Age >= 18 then
    Statut := 'Majeur'
  else
    Statut := 'Mineur';
end;
```

### Nouvelle approche
```pascal
var
  Age: Integer;
  Statut: string;
begin
  Age := 17;

  Statut := Age >= 18 ? 'Majeur' : 'Mineur';
end;
```

L'opérateur ternaire peut également être utilisé dans les expressions :

```pascal
ShowMessage('Vous êtes ' + (Age >= 18 ? 'majeur' : 'mineur'));

var Prix := Quantite * (EstMembre ? TarifMembre : TarifStandard);
```

<span style="color: #0066CC">**Nécessite Delphi 10.3 ou supérieur**</span>

## Opérateurs d'assignation composés (Delphi 12 Athens)

Les opérateurs d'assignation composés permettent de combiner une opération et une assignation en une seule expression. Ils rendent le code plus concis et plus lisible.

### Ancienne approche
```pascal
var
  Compteur: Integer;
begin
  Compteur := 0;

  Compteur := Compteur + 1;    // Incrémentation
  Compteur := Compteur - 5;    // Décrémentation
  Compteur := Compteur * 2;    // Multiplication
  Compteur := Compteur div 3;  // Division entière
end;
```

### Nouvelle approche
```pascal
var
  Compteur: Integer;
begin
  Compteur := 0;

  Compteur += 1;    // Équivalent à Compteur := Compteur + 1
  Compteur -= 5;    // Équivalent à Compteur := Compteur - 5
  Compteur *= 2;    // Équivalent à Compteur := Compteur * 2
  Compteur div= 3;  // Équivalent à Compteur := Compteur div 3
end;
```

Ces opérateurs fonctionnent avec tous les types numériques et les chaînes :

```pascal
var Texte := 'Bonjour';
Texte += ' monde';  // Équivalent à Texte := Texte + ' monde'
```

<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur**</span>

## Paramètres nommés (Delphi 12 Athens)

Les paramètres nommés permettent de spécifier les arguments d'une procédure ou fonction par leur nom plutôt que par leur position, ce qui rend le code plus lisible et moins sujet aux erreurs.

### Ancienne approche
```pascal
procedure ConfigurerUtilisateur(const Nom, Prenom, Email: string; Age: Integer; EstActif: Boolean);
begin
  // Code...
end;

// Appel de la procédure
ConfigurerUtilisateur('Dupont', 'Jean', 'jean.dupont@email.com', 35, True);
```

### Nouvelle approche
```pascal
procedure ConfigurerUtilisateur(const Nom, Prenom, Email: string; Age: Integer; EstActif: Boolean);
begin
  // Code...
end;

// Appel avec paramètres nommés
ConfigurerUtilisateur(
  Nom := 'Dupont',
  Prenom := 'Jean',
  Email := 'jean.dupont@email.com',
  Age := 35,
  EstActif := True
);
```

Les avantages sont encore plus évidents lorsque vous avez beaucoup de paramètres ou lorsque vous voulez en spécifier seulement quelques-uns et laisser les autres à leur valeur par défaut :

```pascal
procedure ConfigurerOptions(
  const TaillePolice: Integer = 12;
  const NomPolice: string = 'Arial';
  const Couleur: TColor = clBlack;
  const Gras: Boolean = False;
  const Italique: Boolean = False;
  const Souligne: Boolean = False
);
begin
  // Code...
end;

// Spécifier seulement les paramètres qui nous intéressent
ConfigurerOptions(
  TaillePolice := 14,
  Couleur := clRed,
  Italique := True
);
```

<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur**</span>

## Pour avec énumérateurs (Delphi XE7)

La boucle `for..in` permet de parcourir facilement les éléments d'une collection sans se soucier des indices ou des itérateurs explicites.

### Ancienne approche
```pascal
var
  Liste: TStringList;
  i: Integer;
begin
  Liste := TStringList.Create;
  try
    Liste.Add('Un');
    Liste.Add('Deux');
    Liste.Add('Trois');

    for i := 0 to Liste.Count - 1 do
      ShowMessage(Liste[i]);
  finally
    Liste.Free;
  end;
end;
```

### Nouvelle approche
```pascal
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    Liste.Add('Un');
    Liste.Add('Deux');
    Liste.Add('Trois');

    for var Element in Liste do
      ShowMessage(Element);
  finally
    Liste.Free;
  end;
end;
```

Cette syntaxe fonctionne avec de nombreux types :

```pascal
// Parcourir un tableau
var Nombres: TArray<Integer> := [1, 2, 3, 4, 5];
for var Nombre in Nombres do
  ShowMessage(IntToStr(Nombre));

// Parcourir une chaîne caractère par caractère
var Texte := 'Bonjour';
for var C in Texte do
  ShowMessage(C);

// Parcourir un dictionnaire
var Dict := TDictionary<string, Integer>.Create;
try
  Dict.Add('Un', 1);
  Dict.Add('Deux', 2);

  for var Paire in Dict do
    ShowMessage(Paire.Key + ': ' + IntToStr(Paire.Value));
finally
  Dict.Free;
end;
```

## Expressions lambda (Delphi XE3)

Les expressions lambda permettent de définir des fonctions anonymes de manière concise. Elles sont particulièrement utiles pour les callbacks et les fonctions de filtrage.

```pascal
// Déclaration d'une expression lambda
var EstPair := function(Valeur: Integer): Boolean
begin
  Result := (Valeur mod 2) = 0;
end;

// Utilisation
if EstPair(4) then
  ShowMessage('4 est pair');
```

Elles sont souvent utilisées avec les méthodes de collection comme `Filter` et `Map` :

```pascal
uses
  System.Generics.Collections, System.Generics.Defaults;

var
  Nombres: TList<Integer>;
  NombresPairs: TList<Integer>;
begin
  Nombres := TList<Integer>.Create;
  try
    Nombres.AddRange([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

    // Filtrer les nombres pairs
    NombresPairs := Nombres.Filter(function(const Item: Integer): Boolean
    begin
      Result := (Item mod 2) = 0;
    end);

    try
      // Afficher les nombres pairs
      for var Nombre in NombresPairs do
        ShowMessage(IntToStr(Nombre));
    finally
      NombresPairs.Free;
    end;
  finally
    Nombres.Free;
  end;
end;
```

## Paramètres constref (Delphi 11 Alexandria)

Les paramètres `constref` combinent les avantages des paramètres `const` (pas de copie) et `var` (passage par référence) tout en garantissant que la valeur ne sera pas modifiée.

```pascal
procedure TraiterChaine(constref Texte: string);
begin
  // Texte ne peut pas être modifié
  // Mais aucune copie n'est effectuée, contrairement à const
  ShowMessage(Texte);
end;
```

C'est particulièrement utile pour les grands objets ou les chaînes longues, car cela évite les copies inutiles tout en garantissant l'intégrité des données.

<span style="color: #0066CC">**Nécessite Delphi 11 ou supérieur**</span>

## Initialiseurs de collection et de record (Delphi 10 Seattle)

Les initialiseurs de collection et de record permettent d'initialiser des structures de données complexes en une seule expression.

### Initialiseurs de tableau

```pascal
// Ancienne approche
var
  Nombres: TArray<Integer>;
begin
  SetLength(Nombres, 5);
  Nombres[0] := 1;
  Nombres[1] := 2;
  Nombres[2] := 3;
  Nombres[3] := 4;
  Nombres[4] := 5;
end;

// Nouvelle approche
var
  Nombres: TArray<Integer> := [1, 2, 3, 4, 5];
```

### Initialiseurs de record

```pascal
type
  TAdresse = record
    Rue: string;
    Ville: string;
    CodePostal: string;
  end;

// Ancienne approche
var
  Adresse: TAdresse;
begin
  Adresse.Rue := '123 Rue Principale';
  Adresse.Ville := 'Paris';
  Adresse.CodePostal := '75000';
end;

// Nouvelle approche (Delphi 12)
var
  Adresse: TAdresse := (
    Rue := '123 Rue Principale',
    Ville := 'Paris',
    CodePostal := '75000'
  );
```

<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur pour les initialiseurs de record nommés**</span>

## Supports pour les records (Delphi 10.4 Sydney)

Les records en Delphi sont devenus beaucoup plus puissants avec la possibilité d'y ajouter des méthodes, des opérateurs et des propriétés.

```pascal
type
  TPoint = record
  private
    FX, FY: Integer;
  public
    constructor Create(X, Y: Integer);

    // Méthodes
    function Distance(const Other: TPoint): Double;
    procedure Translate(DX, DY: Integer);

    // Propriétés
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;

    // Opérateurs
    class operator Add(const A, B: TPoint): TPoint;
    class operator Equal(const A, B: TPoint): Boolean;
  end;

// Implémentation
constructor TPoint.Create(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;

function TPoint.Distance(const Other: TPoint): Double;
begin
  Result := Sqrt(Sqr(FX - Other.X) + Sqr(FY - Other.Y));
end;

procedure TPoint.Translate(DX, DY: Integer);
begin
  FX := FX + DX;
  FY := FY + DY;
end;

class operator TPoint.Add(const A, B: TPoint): TPoint;
begin
  Result.FX := A.FX + B.FX;
  Result.FY := A.FY + B.FY;
end;

class operator TPoint.Equal(const A, B: TPoint): Boolean;
begin
  Result := (A.FX = B.FX) and (A.FY = B.FY);
end;
```

Utilisation :

```pascal
var
  P1: TPoint := TPoint.Create(10, 20);
  P2: TPoint := TPoint.Create(30, 40);
  P3: TPoint;
begin
  P3 := P1 + P2;  // Utilisation de l'opérateur +

  ShowMessage(Format('P3 = (%d, %d)', [P3.X, P3.Y]));

  P1.Translate(5, 5);
  ShowMessage(Format('Distance entre P1 et P2 : %f', [P1.Distance(P2)]));

  if P1 = P2 then  // Utilisation de l'opérateur =
    ShowMessage('Points égaux')
  else
    ShowMessage('Points différents');
end;
```

## Méthodes de classe anonymes (Delphi 11 Alexandria)

Les méthodes de classe anonymes permettent de définir des méthodes sans les déclarer explicitement dans la classe.

```pascal
type
  TCalcul = class
  public
    class var Addition: function(A, B: Integer): Integer;
    class var Soustraction: function(A, B: Integer): Integer;
  end;

// Initialisation
initialization
  TCalcul.Addition := function(A, B: Integer): Integer
  begin
    Result := A + B;
  end;

  TCalcul.Soustraction := function(A, B: Integer): Integer
  begin
    Result := A - B;
  end;
```

Utilisation :

```pascal
var
  Resultat: Integer;
begin
  Resultat := TCalcul.Addition(5, 3);  // Resultat = 8
  ShowMessage('5 + 3 = ' + IntToStr(Resultat));

  Resultat := TCalcul.Soustraction(5, 3);  // Resultat = 2
  ShowMessage('5 - 3 = ' + IntToStr(Resultat));
end;
```

<span style="color: #0066CC">**Nécessite Delphi 11 ou supérieur**</span>

## Exemple complet

Voici un exemple qui combine plusieurs de ces nouveautés syntaxiques pour créer une application simple de gestion de tâches :

```pascal
type
  TTachePriorite = (tpBasse, tpNormale, tpHaute, tpUrgente);

  TTache = record
  private
    FTitre: string;
    FDescription: string;
    FPriorite: TTachePriorite;
    FTerminee: Boolean;
  public
    constructor Create(const Titre, Description: string; Priorite: TTachePriorite);

    property Titre: string read FTitre write FTitre;
    property Description: string read FDescription write FDescription;
    property Priorite: TTachePriorite read FPriorite write FPriorite;
    property Terminee: Boolean read FTerminee write FTerminee;

    function ToString: string;
  end;

  TGestionnaireTaches = class
  private
    FTaches: TList<TTache>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterTache(const Tache: TTache);
    procedure MarquerTerminee(Index: Integer);
    function ObtenirTaches(Filtre: TPredicate<TTache> = nil): TArray<TTache>;
    function ObtenirTachesNonTerminees: TArray<TTache>;
    function ObtenirTachesParPriorite(Priorite: TTachePriorite): TArray<TTache>;
  end;

{ TTache }

constructor TTache.Create(const Titre, Description: string; Priorite: TTachePriorite);
begin
  FTitre := Titre;
  FDescription := Description;
  FPriorite := Priorite;
  FTerminee := False;
end;

function TTache.ToString: string;
const
  PrioriteTexte: array[TTachePriorite] of string = ('Basse', 'Normale', 'Haute', 'Urgente');
begin
  Result := Format('%s [%s]%s', [
    FTitre,
    PrioriteTexte[FPriorite],
    FTerminee ? ' - Terminée' : ''
  ]);
end;

{ TGestionnaireTaches }

constructor TGestionnaireTaches.Create;
begin
  inherited;
  FTaches := TList<TTache>.Create;
end;

destructor TGestionnaireTaches.Destroy;
begin
  FTaches.Free;
  inherited;
end;

procedure TGestionnaireTaches.AjouterTache(const Tache: TTache);
begin
  FTaches.Add(Tache);
end;

procedure TGestionnaireTaches.MarquerTerminee(Index: Integer);
begin
  if (Index >= 0) and (Index < FTaches.Count) then
  begin
    var Tache := FTaches[Index];
    Tache.Terminee := True;
    FTaches[Index] := Tache;
  end;
end;

function TGestionnaireTaches.ObtenirTaches(Filtre: TPredicate<TTache>): TArray<TTache>;
begin
  if Filtre = nil then
    Result := FTaches.ToArray
  else
    Result := FTaches.Filter(Filtre).ToArray;
end;

function TGestionnaireTaches.ObtenirTachesNonTerminees: TArray<TTache>;
begin
  Result := ObtenirTaches(function(const Tache: TTache): Boolean
  begin
    Result := not Tache.Terminee;
  end);
end;

function TGestionnaireTaches.ObtenirTachesParPriorite(Priorite: TTachePriorite): TArray<TTache>;
begin
  Result := ObtenirTaches(function(const Tache: TTache): Boolean
  begin
    Result := Tache.Priorite = Priorite;
  end);
end;
```

Utilisation :

```pascal
procedure TForm1.ButtonExecuteClick(Sender: TObject);
var
  Gestionnaire: TGestionnaireTaches;
begin
  Gestionnaire := TGestionnaireTaches.Create;
  try
    // Ajouter quelques tâches avec la nouvelle syntaxe
    Gestionnaire.AjouterTache(TTache.Create(
      Titre := 'Acheter du lait',
      Description := 'Au supermarché du coin',
      Priorite := tpNormale
    ));

    Gestionnaire.AjouterTache(TTache.Create(
      Titre := 'Payer les factures',
      Description := 'Électricité et internet',
      Priorite := tpHaute
    ));

    Gestionnaire.AjouterTache(TTache.Create(
      Titre := 'Appeler grand-mère',
      Description := 'Pour son anniversaire',
      Priorite := tpUrgente
    ));

    // Marquer une tâche comme terminée
    Gestionnaire.MarquerTerminee(0);

    // Afficher toutes les tâches
    Memo1.Lines.Add('Toutes les tâches :');
    for var Tache in Gestionnaire.ObtenirTaches do
      Memo1.Lines.Add('- ' + Tache.ToString);

    Memo1.Lines.Add('');

    // Afficher les tâches non terminées
    Memo1.Lines.Add('Tâches non terminées :');
    for var Tache in Gestionnaire.ObtenirTachesNonTerminees do
      Memo1.Lines.Add('- ' + Tache.ToString);

    Memo1.Lines.Add('');

    // Afficher les tâches urgentes
    var TachesUrgentes := Gestionnaire.ObtenirTachesParPriorite(tpUrgente);
    Memo1.Lines.Add('Tâches urgentes (' + TachesUrgentes.Length.ToString + ') :');
    for var Tache in TachesUrgentes do
      Memo1.Lines.Add('- ' + Tache.ToString);

  finally
    Gestionnaire.Free;
  end;
end;
```

## Compatibilité et migration

Si vous travaillez avec du code existant ou devez maintenir la compatibilité avec des versions antérieures de Delphi, voici quelques conseils :

1. **Vérifiez la version cible** : Assurez-vous que votre projet cible une version de Delphi qui supporte les fonctionnalités que vous souhaitez utiliser.

2. **Directives de compilation conditionnelle** : Utilisez des directives pour adapter votre code selon la version du compilateur :

```pascal
{$IFDEF DELPHIX103_UP}
  // Code utilisant des fonctionnalités de Delphi 10.3 ou supérieur
  var x := 10;
{$ELSE}
  // Code compatible avec les versions antérieures
  var
    x: Integer;
  begin
    x := 10;
{$ENDIF}
```

3. **Adoption progressive** : Introduisez les nouvelles fonctionnalités progressivement, en commençant par les parties du code les plus faciles à tester.

## Conclusion

Les récentes évolutions syntaxiques d'Object Pascal rendent le langage plus moderne et plus agréable à utiliser, tout en préservant sa clarté et sa robustesse. En adoptant ces nouvelles fonctionnalités, vous pouvez :

- Écrire du code plus concis et plus lisible
- Réduire les risques d'erreurs
- Augmenter votre productivité
- Profiter des paradigmes de programmation modernes

L'évolution constante du langage montre l'engagement d'Embarcadero à maintenir Delphi comme un outil de développement pertinent et puissant pour les applications modernes.

---

Dans les prochaines sections, nous explorerons des aspects plus avancés d'Object Pascal, comme les records avancés et les opérateurs surchargés, qui vous permettront de tirer pleinement parti de la puissance du langage.
