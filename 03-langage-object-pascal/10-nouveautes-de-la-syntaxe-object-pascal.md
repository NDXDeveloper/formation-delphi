🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.10 Nouveautés de la syntaxe Object Pascal (dernières versions)

## Introduction

Object Pascal, le langage de Delphi, évolue constamment. Chaque nouvelle version de Delphi apporte son lot d'améliorations et de nouvelles fonctionnalités qui rendent le code plus moderne, plus lisible et plus puissant.

Cette section présente les nouveautés importantes ajoutées dans les versions récentes de Delphi (Delphi 10.x à Delphi 13 Florence).

## Opérateur ternaire (Delphi 13 Florence)

### Qu'est-ce que l'opérateur ternaire ?

L'**opérateur ternaire** permet d'écrire une condition `if...then...else` sur une seule ligne de manière concise. C'est une des nouveautés majeures de Delphi 13 Florence.

### Syntaxe

```pascal
Résultat := if Condition then ValeurSiVrai else ValeurSiFaux;
```

### Avant Delphi 13 (ancienne méthode)

```pascal
var
  Message: string;
  Age: Integer;
begin
  Age := 25;

  // Méthode traditionnelle - 5 lignes
  if Age >= 18 then
    Message := 'Majeur'
  else
    Message := 'Mineur';

  ShowMessage(Message);
end;
```

### Avec Delphi 13 (nouvelle méthode)

```pascal
var
  Message: string;
  Age: Integer;
begin
  Age := 25;

  // Opérateur ternaire - 1 ligne !
  Message := if Age >= 18 then 'Majeur' else 'Mineur';

  ShowMessage(Message);
end;
```

### Exemples pratiques

```pascal
// Déterminer le maximum entre deux valeurs
var
  Max: Integer;
begin
  Max := if A > B then A else B;
end;

// Message personnalisé selon le statut
var
  Statut: string;
begin
  Statut := if Connecte then 'En ligne' else 'Hors ligne';
end;

// Calcul de prix avec remise
var
  PrixFinal: Double;
  EstMembre: Boolean;
  PrixBase: Double;
begin
  PrixBase := 100;
  EstMembre := True;

  PrixFinal := if EstMembre then PrixBase * 0.9 else PrixBase;
  // Si membre → 90€, sinon → 100€
end;

// Utilisation directe dans un appel de fonction
ShowMessage(if Erreur then 'Échec' else 'Succès');

// Imbrication (à utiliser avec modération)
var
  Categorie: string;
  Age: Integer;
begin
  Age := 25;
  Categorie := if Age < 12 then 'Enfant'
               else if Age < 18 then 'Adolescent'
               else 'Adulte';
end;
```

### Avantages

**✅ Code plus concis** : moins de lignes  
**✅ Plus lisible** : pour les conditions simples  
**✅ Expressif** : l'intention est claire  

### Quand l'utiliser ?

```pascal
// ✅ Bon - condition simple
Resultat := if X > 0 then 'Positif' else 'Négatif ou nul';

// ❌ À éviter - trop complexe
Resultat := if (X > 0) and (Y < 10) and (Z = 5) then
              CalculComplexe(X, Y, Z)
            else
              AutreCalcul(X * 2, Y - 1);
// Mieux vaut utiliser un if...then...else classique ici
```

## Variables inline (Delphi 10.3 Rio)

### Qu'est-ce qu'une variable inline ?

Les **variables inline** permettent de déclarer une variable directement à l'endroit où vous l'utilisez, au lieu de la déclarer en début de bloc.

### Avant Delphi 10.3

```pascal
procedure Exemple;  
var  
  I: Integer;        // Déclaration au début
  Somme: Integer;
  Nom: string;
begin
  Somme := 0;

  for I := 1 to 10 do
    Somme := Somme + I;

  Nom := 'Dupont';
  ShowMessage(Nom);
end;
```

### Avec Delphi 10.3 et supérieur

```pascal
procedure Exemple;  
begin  
  var Somme := 0;  // Déclaration inline

  for var I := 1 to 10 do  // Variable de boucle inline
    Somme := Somme + I;

  var Nom := 'Dupont';  // Type déduit automatiquement (string)
  ShowMessage(Nom);
end;
```

### Exemples pratiques

```pascal
// Dans une boucle for
for var I := 0 to ListeNoms.Count - 1 do
  ShowMessage(ListeNoms[I]);

// Dans une boucle for-in
for var Client in ListeClients do
  ShowMessage(Client.Nom);

// Avec type explicite
var Total: Double := 0.0;  
var Compteur: Integer := 0;  

// Avec type inféré (le compilateur devine le type)
var Message := 'Bonjour';  // Type string déduit  
var Prix := 19.99;         // Type Double déduit  
var Actif := True;         // Type Boolean déduit  

// Multiple variables sur une ligne
var X := 10; var Y := 20; var Z := 30;

// Déclaration proche de l'utilisation
var Trouve := Rechercher(Valeur);  
if Trouve then  
  ShowMessage('Trouvé !');
```

### Avantages

**✅ Portée limitée** : la variable n'existe que dans son contexte  
**✅ Code plus lisible** : déclaration proche de l'utilisation  
**✅ Moins d'erreurs** : moins de risque de réutiliser une variable par erreur  

### Bonnes pratiques

```pascal
// ✅ Bon - variable utilisée uniquement dans la boucle
for var I := 0 to 10 do
  WriteLn(I);

// ❌ Moins bon - si I doit être utilisé après la boucle
for var I := 0 to 10 do
  if I = 5 then Break;
// I n'est plus accessible ici !

// Dans ce cas, déclarez I normalement
var I: Integer;  
for I := 0 to 10 do  
  if I = 5 then Break;
ShowMessage(IntToStr(I));  // I est accessible
```

## Méthodes anonymes (Delphi 2009+)

### Qu'est-ce qu'une méthode anonyme ?

Une **méthode anonyme** (ou fonction anonyme) est une fonction sans nom que vous pouvez créer à la volée et passer en paramètre.

### Syntaxe de base

```pascal
// Procédure anonyme
procedure  
begin  
  // Code
end;

// Fonction anonyme
function(Param: Type): TypeRetour  
begin  
  // Code
  Result := valeur;
end;
```

### Exemples simples

```pascal
var
  Salutation: TProc;  // Type pour une procédure anonyme
begin
  // Créer une méthode anonyme
  Salutation := procedure
                begin
                  ShowMessage('Bonjour !');
                end;

  // L'appeler
  Salutation();
end;

// Fonction anonyme avec paramètres
var
  Doubler: TFunc<Integer, Integer>;
begin
  Doubler := function(X: Integer): Integer
             begin
               Result := X * 2;
             end;

  ShowMessage(IntToStr(Doubler(5)));  // Affiche 10
end;
```

### Utilisation pratique : tri personnalisé

```pascal
uses
  System.Generics.Collections, System.Generics.Defaults;

var
  Noms: TList<string>;
begin
  Noms := TList<string>.Create;
  try
    Noms.Add('Zébulon');
    Noms.Add('Alice');
    Noms.Add('Bob');

    // Tri personnalisé avec méthode anonyme
    Noms.Sort(TComparer<string>.Construct(
      function(const A, B: string): Integer
      begin
        // Tri par longueur de chaîne
        Result := Length(A) - Length(B);
      end
    ));

    for var Nom in Noms do
      WriteLn(Nom);

  finally
    Noms.Free;
  end;
end;
```

### Capture de variables (closure)

Les méthodes anonymes peuvent "capturer" les variables de leur contexte :

```pascal
procedure Exemple;  
var  
  Compteur: Integer;
  IncrémenterEtAfficher: TProc;
begin
  Compteur := 0;

  // La méthode anonyme capture la variable Compteur
  IncrémenterEtAfficher := procedure
                           begin
                             Inc(Compteur);
                             ShowMessage('Compteur : ' + IntToStr(Compteur));
                           end;

  IncrémenterEtAfficher();  // Affiche 1
  IncrémenterEtAfficher();  // Affiche 2
  IncrémenterEtAfficher();  // Affiche 3
end;
```

### Utilisation avec les threads

```pascal
uses
  System.Threading;

// Exécuter du code dans un thread séparé
TTask.Run(procedure
          begin
            Sleep(2000);  // Simulation travail
            TThread.Synchronize(nil, procedure
                                     begin
                                       ShowMessage('Terminé !');
                                     end);
          end);
```

### Avantages

**✅ Code plus concis** : pas besoin de créer une méthode séparée  
**✅ Flexibilité** : créer des comportements à la volée  
**✅ Callbacks** : parfait pour les événements et les callbacks  

## Attributs personnalisés (Delphi 2010+)

### Qu'est-ce qu'un attribut ?

Les **attributs** (ou annotations) permettent d'ajouter des métadonnées à vos classes, méthodes ou propriétés.

### Définir un attribut

```pascal
type
  // Définir un attribut personnalisé
  TableAttribute = class(TCustomAttribute)
  private
    FNomTable: string;
  public
    constructor Create(const ANomTable: string);
    property NomTable: string read FNomTable;
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FNomColonne: string;
  public
    constructor Create(const ANomColonne: string);
    property NomColonne: string read FNomColonne;
  end;

constructor TableAttribute.Create(const ANomTable: string);  
begin  
  inherited Create;
  FNomTable := ANomTable;
end;

constructor ColumnAttribute.Create(const ANomColonne: string);  
begin  
  inherited Create;
  FNomColonne := ANomColonne;
end;
```

### Utiliser un attribut

```pascal
type
  [Table('Clients')]  // Attribut sur la classe
  TClient = class
  private
    FID: Integer;
    FNom: string;
    FEmail: string;
  public
    [Column('client_id')]  // Attribut sur la propriété
    property ID: Integer read FID write FID;

    [Column('client_nom')]
    property Nom: string read FNom write FNom;

    [Column('client_email')]
    property Email: string read FEmail write FEmail;
  end;
```

### Lire les attributs avec RTTI

```pascal
uses
  System.Rtti;

procedure AfficherInfosClasse(AClass: TClass);  
var  
  Context: TRttiContext;
  RttiType: TRttiType;
  Attr: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass);

    // Lire les attributs de la classe
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is TableAttribute then
        ShowMessage('Table : ' + TableAttribute(Attr).NomTable);
    end;

  finally
    Context.Free;
  end;
end;

// Utilisation
AfficherInfosClasse(TClient);  // Affiche "Table : Clients"
```

### Utilisations pratiques

1. **ORM (Object-Relational Mapping)** : mapper classes et tables
2. **Validation** : définir des règles de validation
3. **Sérialisation** : contrôler la conversion JSON/XML
4. **Documentation** : ajouter des métadonnées

## Helper Types (Delphi 2006+)

### Qu'est-ce qu'un Helper ?

Un **Helper** permet d'ajouter des méthodes à des types existants sans les modifier ni hériter.

### Helper pour types simples

```pascal
type
  TMonStringHelper = record helper for string
  public
    function EstVide: Boolean;
    function Inverser: string;
    function ContientChiffres: Boolean;
  end;

function TMonStringHelper.EstVide: Boolean;  
begin  
  Result := Trim(Self) = '';
end;

function TMonStringHelper.Inverser: string;  
var  
  I: Integer;
begin
  Result := '';
  for I := Length(Self) downto 1 do
    Result := Result + Self[I];
end;

function TMonStringHelper.ContientChiffres: Boolean;  
var  
  C: Char;
begin
  Result := False;
  for C in Self do
    if CharInSet(C, ['0'..'9']) then
      Exit(True);
end;
```

### Utilisation

```pascal
var
  Texte: string;
begin
  Texte := 'Bonjour123';

  if Texte.EstVide then
    ShowMessage('Vide')
  else
    ShowMessage('Pas vide');

  ShowMessage(Texte.Inverser);  // Affiche "321ruojnoB"

  if Texte.ContientChiffres then
    ShowMessage('Contient des chiffres');
end;
```

### Helper pour classes existantes

```pascal
type
  TListHelper = class helper for TList<Integer>
  public
    function Somme: Integer;
    function Moyenne: Double;
  end;

function TListHelper.Somme: Integer;  
var  
  Valeur: Integer;
begin
  Result := 0;
  for Valeur in Self do
    Result := Result + Valeur;
end;

function TListHelper.Moyenne: Double;  
begin  
  if Self.Count = 0 then
    Result := 0
  else
    Result := Somme / Self.Count;
end;
```

### Utilisation

```pascal
var
  Nombres: TList<Integer>;
begin
  Nombres := TList<Integer>.Create;
  try
    Nombres.Add(10);
    Nombres.Add(20);
    Nombres.Add(30);

    ShowMessage('Somme : ' + IntToStr(Nombres.Somme));      // 60
    ShowMessage('Moyenne : ' + FloatToStr(Nombres.Moyenne)); // 20

  finally
    Nombres.Free;
  end;
end;
```

### Avantages

**✅ Extension** : ajouter des méthodes sans modifier la classe originale  
**✅ Lisibilité** : syntaxe naturelle  
**✅ Réutilisabilité** : helpers utilisables partout  

## Littéraux binaires (Delphi 10.4 Sydney)

### Qu'est-ce qu'un littéral binaire ?

Les **littéraux binaires** permettent d'écrire des nombres directement en notation binaire.

### Syntaxe

```pascal
var
  Masque: Byte;
begin
  // Avant - notation hexadécimale
  Masque := $0F;  // 00001111 en binaire

  // Avec Delphi 10.4+ - notation binaire directe
  Masque := %00001111;  // Plus clair !
end;
```

### Exemples pratiques

```pascal
const
  // Masques de bits
  BIT_0 = %00000001;
  BIT_1 = %00000010;
  BIT_2 = %00000100;
  BIT_3 = %00001000;
  BIT_4 = %00010000;
  BIT_5 = %00100000;
  BIT_6 = %01000000;
  BIT_7 = %10000000;

  // Permissions (exemple)
  PERM_READ    = %001;  // 1
  PERM_WRITE   = %010;  // 2
  PERM_EXECUTE = %100;  // 4

var
  Valeur: Byte;
  Permissions: Byte;
begin
  // Définir des bits
  Valeur := %11110000;  // 240 en décimal

  // Combinaison de permissions
  Permissions := PERM_READ or PERM_WRITE;  // %011 = 3

  // Vérifier un bit
  if (Valeur and BIT_4) = BIT_4 then
    ShowMessage('Bit 4 est actif');
end;
```

### Avantages

**✅ Clarté** : manipulation de bits plus évidente  
**✅ Lisibilité** : on voit directement les bits  
**✅ Moins d'erreurs** : pas besoin de convertir mentalement  

## Expressions régulières intégrées (Delphi XE+)

### Qu'est-ce qu'une expression régulière ?

Les **expressions régulières** (regex) permettent de rechercher et de manipuler du texte selon des motifs complexes.

### Utilisation basique

```pascal
uses
  System.RegularExpressions;

var
  Email: string;
begin
  Email := 'utilisateur@example.com';

  // Vérifier si c'est un email valide
  if TRegEx.IsMatch(Email, '^[\w\.-]+@[\w\.-]+\.\w+$') then
    ShowMessage('Email valide')
  else
    ShowMessage('Email invalide');
end;
```

### Recherche et extraction

```pascal
var
  Texte: string;
  Match: TMatch;
begin
  Texte := 'Mon numéro est 0612345678';

  // Rechercher un numéro de téléphone
  Match := TRegEx.Match(Texte, '\d{10}');

  if Match.Success then
    ShowMessage('Numéro trouvé : ' + Match.Value);  // 0612345678
end;
```

### Remplacement

```pascal
var
  Texte: string;
  Resultat: string;
begin
  Texte := 'Prix : 19.99€, Remise : 5.00€';

  // Remplacer tous les nombres
  Resultat := TRegEx.Replace(Texte, '\d+\.\d+', 'XX.XX');

  ShowMessage(Resultat);  // "Prix : XX.XX€, Remise : XX.XX€"
end;
```

### Extraction de plusieurs correspondances

```pascal
var
  Texte: string;
  Matches: TMatchCollection;
  Match: TMatch;
begin
  Texte := 'Emails : alice@test.com, bob@example.com, charlie@domain.org';

  // Trouver tous les emails
  Matches := TRegEx.Matches(Texte, '[\w\.-]+@[\w\.-]+\.\w+');

  for Match in Matches do
    ShowMessage('Email : ' + Match.Value);
end;
```

### Motifs courants

```pascal
// Validation d'email
const REGEX_EMAIL = '^[\w\.-]+@[\w\.-]+\.\w+$';

// Numéro de téléphone français
const REGEX_TEL_FR = '^0[1-9]\d{8}$';

// Code postal français
const REGEX_CP_FR = '^\d{5}$';

// URL
const REGEX_URL = '^https?:\/\/[\w\.-]+\.\w{2,}';

// Date format JJ/MM/AAAA
const REGEX_DATE = '^\d{2}\/\d{2}\/\d{4}$';
```

## Améliorations des records (Delphi 2006+)

### Records avec méthodes

Les records modernes peuvent contenir des méthodes comme les classes :

```pascal
type
  TPoint = record
    X, Y: Integer;
    constructor Create(AX, AY: Integer);
    function Distance(Autre: TPoint): Double;
    procedure Deplacer(DeltaX, DeltaY: Integer);
    function ToString: string;
  end;

constructor TPoint.Create(AX, AY: Integer);  
begin  
  X := AX;
  Y := AY;
end;

function TPoint.Distance(Autre: TPoint): Double;  
begin  
  Result := Sqrt(Sqr(Autre.X - X) + Sqr(Autre.Y - Y));
end;

procedure TPoint.Deplacer(DeltaX, DeltaY: Integer);  
begin  
  X := X + DeltaX;
  Y := Y + DeltaY;
end;

function TPoint.ToString: string;  
begin  
  Result := Format('(%d, %d)', [X, Y]);
end;
```

### Utilisation

```pascal
var
  P1, P2: TPoint;
  Dist: Double;
begin
  P1 := TPoint.Create(10, 20);
  P2 := TPoint.Create(30, 40);

  ShowMessage('P1 : ' + P1.ToString);

  P1.Deplacer(5, 5);
  ShowMessage('P1 après déplacement : ' + P1.ToString);

  Dist := P1.Distance(P2);
  ShowMessage('Distance : ' + FloatToStr(Dist));
end;
```

### Opérateurs surchargés

```pascal
type
  TComplexe = record
    Reel, Imaginaire: Double;
    constructor Create(AReel, AImaginaire: Double);
    class operator Add(const A, B: TComplexe): TComplexe;
    class operator Subtract(const A, B: TComplexe): TComplexe;
    function ToString: string;
  end;

constructor TComplexe.Create(AReel, AImaginaire: Double);  
begin  
  Reel := AReel;
  Imaginaire := AImaginaire;
end;

class operator TComplexe.Add(const A, B: TComplexe): TComplexe;  
begin  
  Result.Reel := A.Reel + B.Reel;
  Result.Imaginaire := A.Imaginaire + B.Imaginaire;
end;

class operator TComplexe.Subtract(const A, B: TComplexe): TComplexe;  
begin  
  Result.Reel := A.Reel - B.Reel;
  Result.Imaginaire := A.Imaginaire - B.Imaginaire;
end;

function TComplexe.ToString: string;  
begin  
  if Imaginaire >= 0 then
    Result := Format('%.2f + %.2fi', [Reel, Imaginaire])
  else
    Result := Format('%.2f - %.2fi', [Reel, Abs(Imaginaire)]);
end;
```

### Utilisation

```pascal
var
  C1, C2, C3: TComplexe;
begin
  C1 := TComplexe.Create(3, 4);
  C2 := TComplexe.Create(1, 2);

  C3 := C1 + C2;  // Utilise l'opérateur Add
  ShowMessage('C1 + C2 = ' + C3.ToString);  // "4.00 + 6.00i"

  C3 := C1 - C2;  // Utilise l'opérateur Subtract
  ShowMessage('C1 - C2 = ' + C3.ToString);  // "2.00 + 2.00i"
end;
```

## For-in amélioré (Delphi 2005+)

### Parcourir des collections

```pascal
// Tableaux
const
  Nombres: array[1..5] of Integer = (10, 20, 30, 40, 50);
begin
  for var N in Nombres do
    ShowMessage(IntToStr(N));
end;

// TList
var
  Liste: TList<string>;
begin
  Liste := TList<string>.Create;
  try
    Liste.Add('Alice');
    Liste.Add('Bob');
    Liste.Add('Charlie');

    for var Nom in Liste do
      ShowMessage(Nom);
  finally
    Liste.Free;
  end;
end;

// TDictionary
var
  Dico: TDictionary<string, Integer>;
  Paire: TPair<string, Integer>;
begin
  Dico := TDictionary<string, Integer>.Create;
  try
    Dico.Add('Alice', 25);
    Dico.Add('Bob', 30);

    for Paire in Dico do
      ShowMessage(Format('%s a %d ans', [Paire.Key, Paire.Value]));
  finally
    Dico.Free;
  end;
end;
```

## Améliorations des chaînes de caractères

### Chaînes multi-lignes

```pascal
const
  SQL_QUERY = 'SELECT * FROM Clients ' +
              'WHERE Actif = 1 ' +
              'ORDER BY Nom';

// Ou plus lisible avec des constantes
const
  SQL_SELECT = 'SELECT * FROM Clients';
  SQL_WHERE  = 'WHERE Actif = 1';
  SQL_ORDER  = 'ORDER BY Nom';
  SQL_QUERY2 = SQL_SELECT + ' ' + SQL_WHERE + ' ' + SQL_ORDER;
```

### Interpolation avec Format

```pascal
var
  Nom: string;
  Age: Integer;
begin
  Nom := 'Alice';
  Age := 25;

  ShowMessage(Format('%s a %d ans', [Nom, Age]));
end;
```

## Gestion des ressources avec managed records (Delphi 10.4+)

### Records gérés automatiquement

Delphi 10.4 Sydney a introduit les **managed records** qui peuvent implémenter une gestion automatique de la mémoire :

```pascal
type
  TAutoFree<T: class> = record
  private
    FValue: T;
  public
    constructor Create(AValue: T);
    class operator Initialize(out Dest: TAutoFree<T>);
    class operator Finalize(var Dest: TAutoFree<T>);
    class operator Implicit(const Value: TAutoFree<T>): T;
    property Value: T read FValue;
  end;

// Libération automatique - pas besoin de try..finally !
var
  Liste: TAutoFree<TStringList>;
begin
  Liste := TAutoFree<TStringList>.Create(TStringList.Create);
  Liste.Value.Add('Test');
  // Libéré automatiquement à la fin du scope
end;
```

## Résumé des nouveautés par version

### Delphi 13 Florence (2024)
- ✨ **Opérateur ternaire** : `if...then...else` inline
- Améliorations VCL, FireMonkey et FireDAC
- Support LLDB v12
- Améliorations IA

### Delphi 12 Athens (2023)
- Améliorations de performance
- Nouvelles fonctionnalités IDE

### Delphi 11 Alexandria (2021)
- Amélioration des helpers
- Nouvelles API de threading

### Delphi 10.4 Sydney (2020)
- ✨ **Littéraux binaires** : notation %
- ✨ **Managed records** : gestion automatique (Initialize/Finalize)
- Améliorations des génériques
- Support macOS 64-bit

### Delphi 10.3 Rio (2018)
- ✨ **Variables inline** : déclaration à la volée
- Support Linux pour serveurs
- Améliorations IDE

### Delphi 10 Seattle/Berlin (2015-2016)
- Améliorations FireDAC
- Support Windows 10
- Améliorations mobile

## Conseils d'utilisation

### Adoptez progressivement

```pascal
// ✅ Commencez simple
var Message := 'Bonjour';  // Variables inline

// Puis ajoutez progressivement
Message := if EstMatin then 'Bonjour' else 'Bonsoir';  // Opérateur ternaire

// Utilisez les méthodes anonymes quand approprié
TTask.Run(procedure
          begin
            // Traitement asynchrone
          end);
```

### Ne sur-utilisez pas

```pascal
// ❌ Trop complexe
var X := if A > B then if C < D then E else F else if G > H then I else J;

// ✅ Plus clair
var X: Integer;  
if A > B then  
begin  
  if C < D then
    X := E
  else
    X := F;
end  
else  
begin  
  if G > H then
    X := I
  else
    X := J;
end;
```

### Restez compatible

Si vous partagez du code avec d'autres projets, assurez-vous que tous utilisent une version suffisamment récente de Delphi pour supporter les nouvelles syntaxes.

## Résumé

Les principales nouveautés modernes d'Object Pascal :

- **Opérateur ternaire** (Delphi 13) : conditions inline concises
- **Variables inline** (10.3+) : déclaration à la volée
- **Méthodes anonymes** : fonctions sans nom, callbacks
- **Attributs** : métadonnées sur classes et propriétés
- **Helpers** : étendre des types existants
- **Littéraux binaires** (10.4+) : notation binaire directe
- **Expressions régulières** : manipulation avancée de texte
- **Records améliorés** : méthodes et opérateurs
- **For-in** : parcours de collections simplifié
- **Managed records** (10.4+) : gestion automatique

Ces fonctionnalités rendent Object Pascal plus moderne, plus expressif et plus puissant tout en conservant sa clarté et sa lisibilité. N'hésitez pas à les utiliser dans vos projets pour un code plus élégant et maintenable !

⏭️ [Records avancés et opérateurs surchargés](/03-langage-object-pascal/11-records-avances-et-operateurs-surcharges.md)
