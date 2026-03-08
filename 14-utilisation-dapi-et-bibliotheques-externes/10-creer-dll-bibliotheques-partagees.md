🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.10 Créer ses propres DLL et bibliothèques partagées

## Introduction aux bibliothèques partagées

### Qu'est-ce qu'une bibliothèque partagée ?

Une **bibliothèque partagée** est un fichier contenant du code compilé qui peut être utilisé par plusieurs applications simultanément. Selon la plateforme, elle porte différents noms :

- **Windows** : `.dll` (Dynamic Link Library)
- **Linux** : `.so` (Shared Object)
- **macOS** : `.dylib` (Dynamic Library)

**Analogie :** Imaginez une bibliothèque publique avec des livres. Plutôt que chaque personne achète le même livre, tous empruntent le même exemplaire. C'est exactement ce que fait une DLL : un seul fichier utilisé par plusieurs programmes.

### Pourquoi créer une bibliothèque partagée ?

**Réutilisation du code** : Écrire le code une fois et l'utiliser dans plusieurs applications.

**Modularité** : Séparer votre application en modules indépendants, plus faciles à maintenir.

**Partage avec d'autres langages** : Une DLL peut être utilisée par C++, C#, Python, et pratiquement tous les langages.

**Mises à jour facilitées** : Vous pouvez mettre à jour la DLL sans recompiler toutes les applications qui l'utilisent.

**Protection du code** : Distribuer des fonctionnalités sans révéler le code source.

**Plugins et extensions** : Permettre à d'autres de créer des extensions pour votre application.

### Différences entre DLL et exécutable

| Caractéristique | EXE | DLL |
|-----------------|-----|-----|
| Exécution directe | Oui | Non |
| Point d'entrée | Program | Library |
| Extension | .exe | .dll/.so/.dylib |
| Peut être chargé | Non | Oui |
| Contient | Application complète | Fonctions exportées |

## Créer une DLL simple sous Windows

### Structure de base d'une DLL

```pascal
library MaBibliotheque;

// Déclarations uses
uses
  System.SysUtils,
  System.Classes;

// Fonctions exportées
function Additionner(a, b: Integer): Integer; stdcall;  
begin  
  Result := a + b;
end;

function Multiplier(a, b: Double): Double; stdcall;  
begin  
  Result := a * b;
end;

// Export des fonctions
exports
  Additionner,
  Multiplier;

begin
  // Code d'initialisation (optionnel)
end.
```

**Éléments clés :**

1. **library** au lieu de **program** : Indique que c'est une bibliothèque, pas un programme
2. **stdcall** : Convention d'appel standard pour Windows
3. **exports** : Liste les fonctions accessibles depuis l'extérieur
4. **begin...end.** : Code exécuté au chargement de la DLL (optionnel)

### Créer le projet dans Delphi

1. **Fichier** → **Nouveau** → **Autre**
2. Choisir **Bibliothèque de liens dynamiques**
3. Sauvegarder le projet (par exemple : `MaBibliotheque.dpr`)
4. Compiler : **Projet** → **Compiler MaBibliotheque**

Delphi génère alors le fichier `MaBibliotheque.dll`.

### Tester la DLL

Créer une application test :

```pascal
program TestDLL;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

// Déclaration des fonctions de la DLL
function Additionner(a, b: Integer): Integer; stdcall;
  external 'MaBibliotheque.dll';

function Multiplier(a, b: Double): Double; stdcall;
  external 'MaBibliotheque.dll';

begin
  try
    WriteLn('Test de la DLL');
    WriteLn('10 + 20 = ', Additionner(10, 20));
    WriteLn('3.5 * 2.0 = ', Multiplier(3.5, 2.0):0:2);
    ReadLn;
  except
    on E: Exception do
      WriteLn('Erreur: ' + E.Message);
  end;
end.
```

## Liaison statique vs liaison dynamique

### Comprendre la différence fondamentale

Quand vous créez une DLL, les utilisateurs de votre bibliothèque ont deux façons de l'utiliser. Cette distinction est **essentielle** pour comprendre comment votre DLL sera intégrée dans d'autres applications.

#### Liaison statique (Static Linking)

La DLL est **liée à la compilation** avec le mot-clé `external`. Les fonctions sont référencées directement dans le code.

```pascal
// Déclaration dans l'application utilisatrice
function Additionner(a, b: Integer): Integer; stdcall;
  external 'MaBibliotheque.dll';

// Utilisation directe
procedure TForm1.Button1Click(Sender: TObject);  
var  
  resultat: Integer;
begin
  resultat := Additionner(10, 20); // Appel direct comme une fonction normale
  ShowMessage('Résultat: ' + IntToStr(resultat));
end;
```

**Caractéristiques :**
- ✅ Code simple et direct
- ✅ Appel optimisé, légèrement plus rapide
- ✅ Vérification des fonctions à la compilation
- ✅ IntelliSense et auto-complétion dans l'IDE
- ❌ La DLL **doit** être présente au démarrage de l'application
- ❌ L'application ne démarre pas si la DLL est absente ou corrompue
- ❌ Impossible de gérer l'absence de la DLL gracieusement

#### Liaison dynamique (Dynamic Linking)

La DLL est **chargée à l'exécution** avec les fonctions Windows `LoadLibrary` et `GetProcAddress`.

```pascal
// Type de fonction (signature)
type
  TAdditionner = function(a, b: Integer): Integer; stdcall;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  DLLHandle: THandle;
  Additionner: TAdditionner;
  resultat: Integer;
begin
  // Charger la DLL à l'exécution
  DLLHandle := LoadLibrary('MaBibliotheque.dll');

  if DLLHandle <> 0 then
  begin
    try
      // Obtenir l'adresse de la fonction
      @Additionner := GetProcAddress(DLLHandle, 'Additionner');

      if @Additionner <> nil then
      begin
        resultat := Additionner(10, 20);
        ShowMessage('Résultat: ' + IntToStr(resultat));
      end
      else
        ShowMessage('Fonction "Additionner" introuvable dans la DLL');
    finally
      // Libérer la DLL
      FreeLibrary(DLLHandle);
    end;
  end
  else
    ShowMessage('DLL "MaBibliotheque.dll" introuvable');
end;
```

**Caractéristiques :**
- ✅ Gestion élégante de l'absence de DLL
- ✅ Charge la DLL uniquement quand nécessaire
- ✅ Peut décharger la DLL pour libérer la mémoire
- ✅ Permet de choisir dynamiquement quelle DLL charger
- ❌ Code plus verbeux et complexe
- ❌ Pas de vérification à la compilation
- ❌ Pas d'IntelliSense pour les fonctions
- ❌ Overhead minimal lors de l'appel

### Compatibilité automatique

**Bonne nouvelle :** Votre DLL fonctionne automatiquement avec les deux approches ! Vous n'avez rien de spécial à faire lors de la création.

```pascal
library MaBibliotheque;

uses
  System.SysUtils;

function Additionner(a, b: Integer): Integer; stdcall;  
begin  
  Result := a + b;
end;

function Soustraire(a, b: Integer): Integer; stdcall;  
begin  
  Result := a - b;
end;

exports
  Additionner,
  Soustraire;
  // Ces fonctions sont accessibles en liaison statique ET dynamique

end.
```

### Faciliter la liaison dynamique pour vos utilisateurs

Créez une classe wrapper pour simplifier l'utilisation en liaison dynamique :

```pascal
// MaBibliotheque.Loader.pas
unit MaBibliotheque.Loader;

interface

uses
  Winapi.Windows, System.SysUtils;

type
  TMaBibliothequeLoader = class
  private
    FHandle: THandle;
    FChargee: Boolean;

    // Pointeurs vers les fonctions de la DLL
    FAdditionner: function(a, b: Integer): Integer; stdcall;
    FSoustraire: function(a, b: Integer): Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    function Charger(const CheminDLL: string = 'MaBibliotheque.dll'): Boolean;
    procedure Decharger;

    // Méthodes wrapper
    function Additionner(a, b: Integer): Integer;
    function Soustraire(a, b: Integer): Integer;

    property Chargee: Boolean read FChargee;
  end;

implementation

constructor TMaBibliothequeLoader.Create;  
begin  
  inherited;
  FHandle := 0;
  FChargee := False;
end;

destructor TMaBibliothequeLoader.Destroy;  
begin  
  Decharger;
  inherited;
end;

function TMaBibliothequeLoader.Charger(const CheminDLL: string): Boolean;  
begin  
  if FChargee then
    Exit(True);

  if not FileExists(CheminDLL) then
    Exit(False);

  FHandle := LoadLibrary(PChar(CheminDLL));
  if FHandle = 0 then
    Exit(False);

  try
    @FAdditionner := GetProcAddress(FHandle, 'Additionner');
    @FSoustraire := GetProcAddress(FHandle, 'Soustraire');

    if (@FAdditionner = nil) or (@FSoustraire = nil) then
    begin
      FreeLibrary(FHandle);
      FHandle := 0;
      Exit(False);
    end;

    FChargee := True;
    Result := True;
  except
    FreeLibrary(FHandle);
    FHandle := 0;
    Result := False;
  end;
end;

procedure TMaBibliothequeLoader.Decharger;  
begin  
  if FHandle <> 0 then
  begin
    FreeLibrary(FHandle);
    FHandle := 0;
    FChargee := False;
  end;
end;

function TMaBibliothequeLoader.Additionner(a, b: Integer): Integer;  
begin  
  if not FChargee then
    raise Exception.Create('DLL non chargée');
  Result := FAdditionner(a, b);
end;

function TMaBibliothequeLoader.Soustraire(a, b: Integer): Integer;  
begin  
  if not FChargee then
    raise Exception.Create('DLL non chargée');
  Result := FSoustraire(a, b);
end;

end.
```

**Utilisation simplifiée :**

```pascal
var
  Lib: TMaBibliothequeLoader;
begin
  Lib := TMaBibliothequeLoader.Create;
  try
    if Lib.Charger then
      ShowMessage('10 + 20 = ' + IntToStr(Lib.Additionner(10, 20)))
    else
      ShowMessage('DLL non disponible');
  finally
    Lib.Free;
  end;
end;
```

### Tableau comparatif

| Critère | Liaison statique | Liaison dynamique |
|---------|------------------|-------------------|
| **Simplicité** | ⭐⭐⭐⭐⭐ | ⭐⭐ (⭐⭐⭐⭐⭐ avec wrapper) |
| **Performance** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Gestion d'absence** | ❌ Crash | ✅ Élégante |
| **Vérification compilation** | ✅ Oui | ❌ Non |
| **IntelliSense** | ✅ Oui | ❌ Non |
| **Plugins/Extensions** | ❌ Non | ✅ Idéal |

### Quand utiliser quelle approche ?

**Liaison statique :** Applications où la DLL est toujours nécessaire, code simple, débutants.

**Liaison dynamique :** Fonctionnalités optionnelles, système de plugins, gestion gracieuse d'absence.

## Exporter des fonctions

### Conventions d'appel

Les conventions d'appel définissent comment les paramètres sont passés aux fonctions.

#### stdcall (Standard Call)

**Usage :** Standard Windows, recommandé pour les DLL Windows.

```pascal
function MaFonction(param: Integer): Integer; stdcall;  
begin  
  Result := param * 2;
end;

exports
  MaFonction;
```

**Caractéristiques :**
- Paramètres passés de droite à gauche
- La fonction appelée nettoie la pile
- Compatible avec C/C++, C#, VB

#### cdecl (C Declaration)

**Usage :** Bibliothèques C/C++, fonctions avec nombre variable de paramètres.

```pascal
function MaFonctionC(param: Integer): Integer; cdecl;  
begin  
  Result := param * 2;
end;

exports
  MaFonctionC;
```

**Caractéristiques :**
- Paramètres passés de droite à gauche
- L'appelant nettoie la pile
- Permet les paramètres variables (varargs)

#### register (Delphi par défaut)

**Usage :** Appels entre unités Delphi uniquement.

```pascal
function MaFonctionDelphi(param: Integer): Integer; register;  
begin  
  Result := param * 2;
end;
```

**Attention :** Ne pas utiliser `register` pour les fonctions exportées d'une DLL destinée à être appelée par d'autres langages.

### Exporter avec des noms personnalisés

```pascal
function Addition(a, b: Integer): Integer; stdcall;  
begin  
  Result := a + b;
end;

exports
  Addition name 'Add',        // Nom externe : Add
  Addition name 'Additionner'; // Alias français
```

Les autres applications peuvent appeler `Add` ou `Additionner`, les deux appellent la même fonction.

### Passage de chaînes

Les chaînes nécessitent une attention particulière :

```pascal
library StringDLL;

uses
  System.SysUtils;

// Fonction qui écrit le résultat dans un buffer fourni par l'appelant
function Concatener(s1, s2: PChar; buffer: PChar; bufferSize: Integer): Integer; stdcall;  
var  
  Resultat: string;
begin
  Resultat := string(s1) + string(s2);
  Result := Length(Resultat);
  if (buffer <> nil) and (bufferSize > Length(Resultat)) then
    StrPCopy(buffer, Resultat);
end;

// Fonction qui modifie une chaîne en place
procedure MettreEnMajuscules(s: PChar); stdcall;  
var  
  i: Integer;
begin
  for i := 0 to StrLen(s) - 1 do
    s[i] := UpCase(s[i]);
end;

exports
  Concatener,
  MettreEnMajuscules;

end.
```

**Important :** Les chaînes Delphi (`string`) ne peuvent pas être directement passées. Utilisez `PChar` pour la compatibilité.

### Passage de structures

```pascal
library StructureDLL;

uses
  System.SysUtils, System.Math;

type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;
  PPoint = ^TPoint;

  TPersonne = record
    Nom: array[0..49] of Char;
    Age: Integer;
    Salaire: Double;
  end;
  PPersonne = ^TPersonne;

// Fonction qui calcule la distance
function CalculerDistance(const p1, p2: TPoint): Double; stdcall;  
begin  
  Result := Sqrt(Sqr(p2.X - p1.X) + Sqr(p2.Y - p1.Y));
end;

// Fonction qui modifie une structure
procedure AugmenterSalaire(p: PPersonne; pourcentage: Double); stdcall;  
begin  
  p^.Salaire := p^.Salaire * (1 + pourcentage / 100);
end;

// Fonction qui crée une structure
function CreerPersonne(nom: PChar; age: Integer; salaire: Double): PPersonne; stdcall;  
begin  
  GetMem(Result, SizeOf(TPersonne));
  StrPCopy(Result^.Nom, nom);
  Result^.Age := age;
  Result^.Salaire := salaire;
end;

// Fonction qui libère une structure
procedure LibererPersonne(p: PPersonne); stdcall;  
begin  
  FreeMem(p);
end;

exports
  CalculerDistance,
  AugmenterSalaire,
  CreerPersonne,
  LibererPersonne;

end.
```

### Retourner des valeurs complexes

```pascal
library TableauxDLL;

uses
  System.SysUtils;

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..99] of Integer;

// Retourner un tableau
function GenererNombres(count: Integer): PIntegerArray; stdcall;  
var  
  i: Integer;
begin
  GetMem(Result, count * SizeOf(Integer));
  for i := 0 to count - 1 do
    Result^[i] := i * 10;
end;

// Libérer le tableau
procedure LibererTableau(arr: PIntegerArray); stdcall;  
begin  
  FreeMem(arr);
end;

// Récupérer un élément
function ObtenirElement(arr: PIntegerArray; index: Integer): Integer; stdcall;  
begin  
  Result := arr^[index];
end;

exports
  GenererNombres,
  LibererTableau,
  ObtenirElement;

end.
```

## Gestion de la mémoire

### Règle d'or

**Qui alloue doit libérer.** Si la DLL alloue de la mémoire, elle doit fournir une fonction pour la libérer.

### Mauvais exemple

```pascal
// MAUVAIS : L'appelant ne peut pas libérer correctement
function CreerTableau(size: Integer): PInteger; stdcall;  
begin  
  GetMem(Result, size * SizeOf(Integer));
  // L'appelant ne peut pas libérer avec sa propre fonction Free!
end;
```

### Bon exemple

```pascal
// BON : Fournir les deux fonctions
function CreerTableau(size: Integer): PInteger; stdcall;  
begin  
  GetMem(Result, size * SizeOf(Integer));
end;

procedure LibererTableau(arr: PInteger); stdcall;  
begin  
  FreeMem(arr);
end;

exports
  CreerTableau,
  LibererTableau;
```

### Utilisation de ShareMem

Pour partager le gestionnaire de mémoire entre DLL et application :

```pascal
library MaDLL;

uses
  // ShareMem DOIT être la première unité
  ShareMem,
  System.SysUtils;

// Maintenant on peut passer des String directement
function Concatener(const s1, s2: string): string; stdcall;  
begin  
  Result := s1 + s2;
end;

exports
  Concatener;

end.
```

**Important :** L'application appelante doit aussi inclure `ShareMem` en première position.

## Exporter des classes

### Impossible directement

On ne peut pas exporter une classe Delphi directement. Il faut utiliser une approche basée sur des handles.

### Approche par handle

```pascal
library ClasseDLL;

uses
  System.SysUtils,
  System.Classes;

type
  // Classe interne à la DLL
  TCalculatrice = class
  private
    FMemoire: Double;
  public
    constructor Create;
    procedure Additionner(valeur: Double);
    procedure Soustraire(valeur: Double);
    procedure Multiplier(valeur: Double);
    procedure Diviser(valeur: Double);
    procedure Effacer;
    function ObtenirResultat: Double;
  end;

constructor TCalculatrice.Create;  
begin  
  inherited;
  FMemoire := 0;
end;

procedure TCalculatrice.Additionner(valeur: Double);  
begin  
  FMemoire := FMemoire + valeur;
end;

procedure TCalculatrice.Soustraire(valeur: Double);  
begin  
  FMemoire := FMemoire - valeur;
end;

procedure TCalculatrice.Multiplier(valeur: Double);  
begin  
  FMemoire := FMemoire * valeur;
end;

procedure TCalculatrice.Diviser(valeur: Double);  
begin  
  if valeur <> 0 then
    FMemoire := FMemoire / valeur;
end;

procedure TCalculatrice.Effacer;  
begin  
  FMemoire := 0;
end;

function TCalculatrice.ObtenirResultat: Double;  
begin  
  Result := FMemoire;
end;

// Fonctions exportées qui manipulent la classe
type
  TCalculatriceHandle = Pointer;

function Calculatrice_Creer: TCalculatriceHandle; stdcall;  
begin  
  Result := TCalculatrice.Create;
end;

procedure Calculatrice_Detruire(handle: TCalculatriceHandle); stdcall;  
begin  
  TCalculatrice(handle).Free;
end;

procedure Calculatrice_Additionner(handle: TCalculatriceHandle; valeur: Double); stdcall;  
begin  
  TCalculatrice(handle).Additionner(valeur);
end;

procedure Calculatrice_Soustraire(handle: TCalculatriceHandle; valeur: Double); stdcall;  
begin  
  TCalculatrice(handle).Soustraire(valeur);
end;

procedure Calculatrice_Multiplier(handle: TCalculatriceHandle; valeur: Double); stdcall;  
begin  
  TCalculatrice(handle).Multiplier(valeur);
end;

procedure Calculatrice_Diviser(handle: TCalculatriceHandle; valeur: Double); stdcall;  
begin  
  TCalculatrice(handle).Diviser(valeur);
end;

procedure Calculatrice_Effacer(handle: TCalculatriceHandle); stdcall;  
begin  
  TCalculatrice(handle).Effacer;
end;

function Calculatrice_ObtenirResultat(handle: TCalculatriceHandle): Double; stdcall;  
begin  
  Result := TCalculatrice(handle).ObtenirResultat;
end;

exports
  Calculatrice_Creer,
  Calculatrice_Detruire,
  Calculatrice_Additionner,
  Calculatrice_Soustraire,
  Calculatrice_Multiplier,
  Calculatrice_Diviser,
  Calculatrice_Effacer,
  Calculatrice_ObtenirResultat;

end.
```

### Utilisation depuis une application

```pascal
type
  TCalculatriceHandle = Pointer;

function Calculatrice_Creer: TCalculatriceHandle; stdcall;
  external 'ClasseDLL.dll';
procedure Calculatrice_Detruire(handle: TCalculatriceHandle); stdcall;
  external 'ClasseDLL.dll';
procedure Calculatrice_Additionner(handle: TCalculatriceHandle; valeur: Double); stdcall;
  external 'ClasseDLL.dll';
function Calculatrice_ObtenirResultat(handle: TCalculatriceHandle): Double; stdcall;
  external 'ClasseDLL.dll';

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Calc: TCalculatriceHandle;
  Resultat: Double;
begin
  Calc := Calculatrice_Creer;
  try
    Calculatrice_Additionner(Calc, 10);
    Calculatrice_Additionner(Calc, 20);
    Resultat := Calculatrice_ObtenirResultat(Calc);
    ShowMessage('Résultat: ' + FloatToStr(Resultat));
  finally
    Calculatrice_Detruire(Calc);
  end;
end;
```

## Callbacks et événements

### Définir un type de callback

```pascal
library CallbackDLL;

uses
  System.SysUtils, Winapi.Windows;

type
  TProgressCallback = procedure(position, total: Integer); stdcall;
  TMessageCallback = procedure(message: PChar); stdcall;

// Fonction qui utilise un callback
procedure TraiterDonnees(count: Integer; callback: TProgressCallback); stdcall;  
var  
  i: Integer;
begin
  for i := 1 to count do
  begin
    Sleep(100);
    if Assigned(callback) then
      callback(i, count);
  end;
end;

// Fonction qui envoie des messages
procedure ExecuterTache(callback: TMessageCallback); stdcall;  
begin  
  if Assigned(callback) then
  begin
    callback('Début de la tâche');
    Sleep(500);
    callback('Traitement en cours...');
    Sleep(500);
    callback('Tâche terminée');
  end;
end;

exports
  TraiterDonnees,
  ExecuterTache;

end.
```

### Utilisation des callbacks

```pascal
type
  TProgressCallback = procedure(position, total: Integer); stdcall;
  TMessageCallback = procedure(message: PChar); stdcall;

procedure TraiterDonnees(count: Integer; callback: TProgressCallback); stdcall;
  external 'CallbackDLL.dll';
procedure ExecuterTache(callback: TMessageCallback); stdcall;
  external 'CallbackDLL.dll';

// Implémentation du callback
procedure MonCallbackProgress(position, total: Integer); stdcall;  
begin  
  Form1.ProgressBar1.Max := total;
  Form1.ProgressBar1.Position := position;
  Form1.Label1.Caption := Format('%d / %d', [position, total]);
  Application.ProcessMessages;
end;

procedure MonCallbackMessage(message: PChar); stdcall;  
begin  
  Form1.Memo1.Lines.Add(string(message));
  Application.ProcessMessages;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  TraiterDonnees(10, @MonCallbackProgress);
end;

procedure TForm1.Button2Click(Sender: TObject);  
begin  
  ExecuterTache(@MonCallbackMessage);
end;
```

## Bibliothèques multi-plateformes

### Structure conditionnelle

```pascal
library MaBiblioMulti;

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows,
  System.SysUtils;
{$ENDIF}

{$IFDEF LINUX}
uses
  Posix.Stdlib,
  System.SysUtils;
{$ENDIF}

{$IFDEF MACOS}
uses
  Macapi.CoreFoundation,
  System.SysUtils;
{$ENDIF}

// Fonction multi-plateformes
function ObtenirNomPlateforme: PChar;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
    Result := 'iOS';
    {$ELSE}
    Result := 'macOS';
    {$ENDIF}
  {$ENDIF}
end;

// Fonction spécifique à la plateforme
function ObtenirCheminDocuments: PChar;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  {$IFDEF MSWINDOWS}
  Result := PChar(GetEnvironmentVariable('USERPROFILE') + '\Documents');
  {$ENDIF}

  {$IFDEF LINUX}
  Result := PChar(GetEnvironmentVariable('HOME') + '/Documents');
  {$ENDIF}

  {$IFDEF MACOS}
  Result := PChar(GetEnvironmentVariable('HOME') + '/Documents');
  {$ENDIF}
end;

exports
  ObtenirNomPlateforme,
  ObtenirCheminDocuments;

end.
```

### Conventions d'appel multi-plateformes

```pascal
// Convention adaptative
{$IFDEF MSWINDOWS}
  {$DEFINE STDCALL}
{$ELSE}
  {$DEFINE CDECL}
{$ENDIF}

function MaFonction(param: Integer): Integer;
  {$IFDEF STDCALL}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := param * 2;
end;
```

## Initialisation et finalisation

### Section d'initialisation

```pascal
library InitDLL;

uses
  System.SysUtils;

var
  CompteurAppels: Integer;
  FichierLog: TextFile;

function IncrémenterCompteur: Integer; stdcall;  
begin  
  Inc(CompteurAppels);
  Result := CompteurAppels;

  WriteLn(FichierLog, Format('Appel #%d à %s',
    [CompteurAppels, FormatDateTime('hh:nn:ss', Now)]));
  Flush(FichierLog);
end;

exports
  IncrémenterCompteur;

initialization
  CompteurAppels := 0;
  AssignFile(FichierLog, 'dll_log.txt');
  Rewrite(FichierLog);
  WriteLn(FichierLog, 'DLL chargée à ' + FormatDateTime('hh:nn:ss', Now));
  Flush(FichierLog);

finalization
  WriteLn(FichierLog, Format('DLL déchargée après %d appels', [CompteurAppels]));
  CloseFile(FichierLog);

end.
```

### DllProc (gestion avancée)

En Delphi, on utilise la variable `DllProc` pour intercepter les événements du cycle de vie de la DLL :

```pascal
library DllProcExample;

uses
  Winapi.Windows,
  System.SysUtils;

procedure DllEntryPoint(Reason: DWORD);  
begin  
  case Reason of
    DLL_PROCESS_ATTACH:
      OutputDebugString('DLL_PROCESS_ATTACH');

    DLL_PROCESS_DETACH:
      OutputDebugString('DLL_PROCESS_DETACH');

    DLL_THREAD_ATTACH:
      OutputDebugString('DLL_THREAD_ATTACH');

    DLL_THREAD_DETACH:
      OutputDebugString('DLL_THREAD_DETACH');
  end;
end;

begin
  // Assigner notre procédure comme gestionnaire d'événements DLL
  DllProc := @DllEntryPoint;
  // Appeler manuellement pour le premier attachement
  DllEntryPoint(DLL_PROCESS_ATTACH);
end.
```

## Fichier de définition de module (.def)

### Créer un fichier .def

Pour contrôler finement les exports, créez un fichier `MaDLL.def` :

```
LIBRARY MaBibliotheque  
EXPORTS  
  Additionner       @1
  Soustraire        @2
  Multiplier        @3
  Diviser           @4
  ObtenirVersion    @5
```

Les numéros (@1, @2, etc.) sont les indices ordinaux, permettant un appel plus rapide.

### Utiliser le fichier .def

Dans les options du projet :
1. **Projet** → **Options**
2. **Éditeur de liens**
3. **Fichier de définition de module** : `MaDLL.def`

## Versionning de DLL

### Informations de version

```pascal
library VersionDLL;

uses
  System.SysUtils;

const
  VERSION_MAJOR = 1;
  VERSION_MINOR = 2;
  VERSION_BUILD = 345;

function ObtenirVersionMajor: Integer; stdcall;  
begin  
  Result := VERSION_MAJOR;
end;

function ObtenirVersionMinor: Integer; stdcall;  
begin  
  Result := VERSION_MINOR;
end;

function ObtenirVersionBuild: Integer; stdcall;  
begin  
  Result := VERSION_BUILD;
end;

function ObtenirVersionComplete(buffer: PChar; bufferSize: Integer): Integer; stdcall;  
var  
  versionStr: string;
begin
  versionStr := Format('%d.%d.%d', [VERSION_MAJOR, VERSION_MINOR, VERSION_BUILD]);
  Result := Length(versionStr);

  if (buffer <> nil) and (bufferSize > Length(versionStr)) then
    StrPCopy(buffer, versionStr);
end;

exports
  ObtenirVersionMajor,
  ObtenirVersionMinor,
  ObtenirVersionBuild,
  ObtenirVersionComplete;

end.
```

### Vérifier la compatibilité

```pascal
// Dans l'application appelante
function VerifierVersionDLL: Boolean;  
var  
  Major, Minor: Integer;
begin
  Major := ObtenirVersionMajor;
  Minor := ObtenirVersionMinor;

  Result := (Major = 1) and (Minor >= 2);

  if not Result then
    ShowMessage(Format('Version incompatible: %d.%d (requis: 1.2+)',
      [Major, Minor]));
end;
```

## Débogage de DLL

### Configurer le débogage

1. **Projet** → **Options** → **Débogage**
2. **Fichier hôte** : Chemin vers l'application qui charge la DLL
3. **Paramètres** : Arguments de ligne de commande (si nécessaire)

### Points d'arrêt dans la DLL

```pascal
library DebugDLL;

uses
  System.SysUtils;

function FonctionADebuguer(param: Integer): Integer; stdcall;  
var  
  temp: Integer;
begin
  temp := param * 2;  // ← Placer un point d'arrêt ici
  Result := temp + 10;
end;

exports
  FonctionADebuguer;

end.
```

Lors de l'exécution de l'application hôte, le débogueur s'arrêtera au point d'arrêt dans la DLL.

### Logging pour débogage

```pascal
library LogDLL;

uses
  System.SysUtils;

procedure LogMessage(const msg: string);  
var  
  F: TextFile;
begin
  AssignFile(F, 'dll_debug.log');
  if FileExists('dll_debug.log') then
    Append(F)
  else
    Rewrite(F);

  try
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + msg);
  finally
    CloseFile(F);
  end;
end;

function TraiterDonnees(data: Integer): Integer; stdcall;  
begin  
  LogMessage('TraiterDonnees appelée avec: ' + IntToStr(data));

  try
    Result := data * 2;
    LogMessage('Résultat: ' + IntToStr(Result));
  except
    on E: Exception do
    begin
      LogMessage('ERREUR: ' + E.Message);
      raise;
    end;
  end;
end;

exports
  TraiterDonnees;

end.
```

## Tests unitaires de DLL

### Créer un projet de test

```pascal
program TestMaDLL;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

function Additionner(a, b: Integer): Integer; stdcall;
  external 'MaBibliotheque.dll';
function Multiplier(a, b: Double): Double; stdcall;
  external 'MaBibliotheque.dll';

type
  TTestResult = record
    Nom: string;
    Reussi: Boolean;
    Message: string;
  end;

var
  Tests: array of TTestResult;

procedure AjouterTest(const Nom: string; Reussi: Boolean; const Message: string = '');  
begin  
  SetLength(Tests, Length(Tests) + 1);
  Tests[High(Tests)].Nom := Nom;
  Tests[High(Tests)].Reussi := Reussi;
  Tests[High(Tests)].Message := Message;
end;

procedure TestAddition;  
begin  
  try
    AjouterTest('Addition simple', Additionner(2, 3) = 5);
    AjouterTest('Addition zéro', Additionner(0, 0) = 0);
    AjouterTest('Addition négative', Additionner(-5, 3) = -2);
  except
    on E: Exception do
      AjouterTest('Addition', False, E.Message);
  end;
end;

procedure TestMultiplication;  
begin  
  try
    AjouterTest('Multiplication simple',
      Abs(Multiplier(2.5, 4.0) - 10.0) < 0.001);
    AjouterTest('Multiplication par zéro',
      Abs(Multiplier(5.0, 0.0)) < 0.001);
  except
    on E: Exception do
      AjouterTest('Multiplication', False, E.Message);
  end;
end;

procedure AfficherResultats;  
var  
  i, reussis, total: Integer;
begin
  total := Length(Tests);
  reussis := 0;

  WriteLn('=== RÉSULTATS DES TESTS ===');
  WriteLn;

  for i := 0 to High(Tests) do
  begin
    Write(Tests[i].Nom + ': ');
    if Tests[i].Reussi then
    begin
      WriteLn('OK');
      Inc(reussis);
    end
    else
    begin
      WriteLn('ÉCHEC');
      if Tests[i].Message <> '' then
        WriteLn('  ' + Tests[i].Message);
    end;
  end;

  WriteLn;
  WriteLn(Format('Résultat: %d/%d tests réussis (%.1f%%)',
    [reussis, total, (reussis / total) * 100]));
end;

begin
  try
    WriteLn('Test de MaBibliotheque.dll');
    WriteLn;

    TestAddition;
    TestMultiplication;

    AfficherResultats;

    WriteLn;
    Write('Appuyez sur Entrée pour quitter...');
    ReadLn;
  except
    on E: Exception do
      WriteLn('ERREUR CRITIQUE: ' + E.Message);
  end;
end.
```

## Bonnes pratiques

### 1. Préfixer les noms de fonctions

Pour éviter les conflits, préfixez vos fonctions :

```pascal
exports
  MonProjet_Initialiser,
  MonProjet_Traiter,
  MonProjet_Terminer;
```

### 2. Fournir des fonctions de version

```pascal
function MonProjet_ObtenirVersion: PChar; stdcall;  
begin  
  Result := '1.0.0';
end;

exports
  MonProjet_ObtenirVersion;
```

### 3. Gérer les exceptions

```pascal
function DiviserSecurise(a, b: Double; var resultat: Double): Integer; stdcall;  
begin  
  try
    if b = 0 then
    begin
      Result := -1; // Code d'erreur
      Exit;
    end;

    resultat := a / b;
    Result := 0; // Succès
  except
    Result := -2; // Erreur inattendue
  end;
end;
```

### 4. Documenter l'API

Créer un fichier d'en-tête (`MaBibliotheque.h` pour C/C++) :

```c
// MaBibliotheque.h
#ifndef MABIBLIOTHEQUE_H
#define MABIBLIOTHEQUE_H

#ifdef __cplusplus
extern "C" {
#endif

// Additionne deux entiers
int __stdcall Additionner(int a, int b);

// Multiplie deux nombres à virgule
double __stdcall Multiplier(double a, double b);

#ifdef __cplusplus
}
#endif

#endif // MABIBLIOTHEQUE_H
```

### 5. Éviter les dépendances externes

Minimisez les unités utilisées pour réduire la taille de la DLL :

```pascal
library PetiteDLL;

// N'inclure que le strict nécessaire
uses
  System.SysUtils;  // Seulement si vraiment nécessaire

function Additionner(a, b: Integer): Integer; stdcall;  
begin  
  Result := a + b;  // Pas besoin de SysUtils pour ça!
end;

exports
  Additionner;

end.
```

### 6. Thread-safety

Si votre DLL sera utilisée par plusieurs threads :

```pascal
library ThreadSafeDLL;

uses
  System.SysUtils,
  System.SyncObjs;

var
  VerrouGlobal: TCriticalSection;
  CompteurPartage: Integer;

function IncrémenterThreadSafe: Integer; stdcall;  
begin  
  VerrouGlobal.Enter;
  try
    Inc(CompteurPartage);
    Result := CompteurPartage;
  finally
    VerrouGlobal.Leave;
  end;
end;

exports
  IncrémenterThreadSafe;

initialization
  VerrouGlobal := TCriticalSection.Create;
  CompteurPartage := 0;

finalization
  VerrouGlobal.Free;

end.
```

## Distribution de votre DLL

### Fichiers à inclure

Lors de la distribution, incluez :
- Le fichier `.dll` / `.so` / `.dylib`
- Le fichier d'en-tête (`.h` pour C/C++)
- Un fichier README avec la documentation
- Des exemples d'utilisation (liaison statique ET dynamique)
- Le fichier de licence

### Structure recommandée

```
MonProjet/
├── bin/
│   ├── Win32/
│   │   └── MaBibliotheque.dll
│   ├── Win64/
│   │   └── MaBibliotheque.dll
│   ├── Linux64/
│   │   └── libMaBibliotheque.so
│   └── macOS/
│       └── libMaBibliotheque.dylib
├── include/
│   ├── Delphi/
│   │   ├── MaBibliotheque.Static.pas
│   │   └── MaBibliotheque.Loader.pas
│   └── C/
│       └── MaBibliotheque.h
├── examples/
│   ├── StaticLinking/
│   ├── DynamicLinking/
│   ├── Delphi/
│   ├── C/
│   └── Python/
└── README.md
```

### Documentation minimale

````markdown
# MaBibliotheque v1.0

## Installation

### Liaison statique
1. Copier la DLL dans le répertoire de votre application
2. Déclarer les fonctions avec `external`

### Liaison dynamique
1. Copier la DLL où vous voulez
2. Utiliser `LoadLibrary` et `GetProcAddress`
3. Ou utiliser la classe wrapper fournie

## Utilisation

### Liaison statique (simple)
```delphi
function Additionner(a, b: Integer): Integer; stdcall;
  external 'MaBibliotheque.dll';

var
  resultat: Integer;
begin
  resultat := Additionner(10, 20);
end;
```

### Liaison dynamique (flexible)
```delphi
uses MaBibliotheque.Loader;

var
  Lib: TMaBibliothequeLoader;
begin
  Lib := TMaBibliothequeLoader.Create;
  try
    if Lib.Charger then
      ShowMessage(IntToStr(Lib.Additionner(10, 20)))
    else
      ShowMessage('DLL non disponible');
  finally
    Lib.Free;
  end;
end;
```

## Fonctions disponibles

- `Additionner(a, b: Integer): Integer` - Additionne deux nombres
- `Multiplier(a, b: Double): Double` - Multiplie deux nombres
- `ObtenirVersion(): PChar` - Retourne la version de la DLL

## Compatibilité

- Windows 32/64-bit
- Linux 64-bit
- macOS (Intel & Apple Silicon)

## Licence

MIT License
````

## Résumé

Créer des DLL et bibliothèques partagées permet de partager du code efficacement.

**Points clés :**

1. **library** au lieu de **program** pour créer une DLL
2. **exports** liste les fonctions accessibles
3. **stdcall** pour Windows, **cdecl** pour multi-plateformes
4. **Liaison statique** = simple, **liaison dynamique** = flexible
5. **PChar** pour les chaînes, pas String
6. **Qui alloue doit libérer** : gestion de la mémoire
7. **Handle pattern** pour exporter des classes
8. **Callbacks** pour la communication asynchrone
9. **Directives conditionnelles** pour le multi-plateformes
10. **Versionning** pour la compatibilité
11. **Documentation** complète de l'API
12. **Fournir un wrapper** pour faciliter la liaison dynamique

**Avantages des DLL :**
- Réutilisation du code
- Modularité
- Partage multi-langages
- Mises à jour facilitées
- Protection du code source
- Système de plugins

**Défis :**
- Gestion de la mémoire délicate
- Types de données limités
- Pas d'export direct de classes
- Compatibilité des versions
- Débogage plus complexe
- Documentation nécessaire

**Liaison statique vs dynamique :**
- **Statique** : Simple, rapide, mais DLL obligatoire
- **Dynamique** : Flexible, optionnel, mais code plus complexe
- **Les deux** sont supportées automatiquement par votre DLL

Créer des bibliothèques partagées de qualité demande de la rigueur, mais c'est un excellent moyen de partager votre code et de créer des systèmes modulaires et maintenables. N'oubliez pas de fournir des exemples pour les deux méthodes de liaison afin de faciliter la vie de vos utilisateurs !

⏭️ [Applications mobiles avec Delphi](/15-applications-mobiles-avec-delphi/README.md)

