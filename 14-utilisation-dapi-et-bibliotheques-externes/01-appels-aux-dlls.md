# 14.1 Appels aux DLLs

## Introduction

Les DLLs (Dynamic Link Libraries) sont des bibliothèques de code compilé qui peuvent être partagées entre plusieurs applications. Elles offrent plusieurs avantages :

- **Réutilisation du code** : les mêmes fonctionnalités peuvent être utilisées par différentes applications
- **Économie de mémoire** : le code n'est chargé qu'une seule fois en mémoire
- **Facilité de maintenance** : une mise à jour de la DLL bénéficie à toutes les applications qui l'utilisent
- **Modularité** : séparation claire des fonctionnalités

Delphi permet d'interagir facilement avec des DLLs, qu'elles soient créées avec Delphi ou d'autres langages comme C/C++.

## Déclaration des fonctions externes

Pour appeler une fonction contenue dans une DLL, nous devons d'abord la déclarer dans notre code Delphi. Cette déclaration spécifie :
- Le nom de la fonction
- Les paramètres qu'elle accepte
- Le type de valeur retournée
- Le nom de la DLL qui contient cette fonction

### Syntaxe de base

```pascal
function NomFonction(Paramètres): TypeRetour; stdcall; external 'NomDeLaDLL.dll';
```

L'attribut `stdcall` indique la convention d'appel, qui définit comment les paramètres sont passés à la fonction. C'est la convention généralement utilisée par les DLLs Windows.

### Exemple simple

Imaginons une DLL nommée `MathLib.dll` contenant une fonction `Addition` qui additionne deux nombres entiers :

```pascal
function Addition(A, B: Integer): Integer; stdcall; external 'MathLib.dll';

procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  Resultat: Integer;
begin
  Resultat := Addition(10, 20);
  ShowMessage('10 + 20 = ' + IntToStr(Resultat));
end;
```

## Chargement dynamique des DLLs

La méthode précédente (liaison statique) charge automatiquement la DLL au démarrage de l'application. Si la DLL n'est pas trouvée, l'application ne démarre pas. Une approche plus flexible consiste à charger la DLL dynamiquement à l'exécution :

```pascal
var
  DLLHandle: THandle;
  AdditionFunc: function(A, B: Integer): Integer; stdcall;

begin
  // Chargement de la DLL
  DLLHandle := LoadLibrary('MathLib.dll');

  if DLLHandle <> 0 then
  begin
    try
      // Récupération de l'adresse de la fonction
      @AdditionFunc := GetProcAddress(DLLHandle, 'Addition');

      if Assigned(AdditionFunc) then
      begin
        // Appel de la fonction
        ShowMessage('10 + 20 = ' + IntToStr(AdditionFunc(10, 20)));
      end
      else
        ShowMessage('Fonction non trouvée dans la DLL');
    finally
      // Libération de la DLL
      FreeLibrary(DLLHandle);
    end;
  end
  else
    ShowMessage('Impossible de charger la DLL');
end;
```

Cette méthode permet de :
- Gérer l'absence de la DLL de façon élégante
- Charger/décharger les DLLs uniquement quand nécessaire
- Utiliser différentes versions de DLLs selon les besoins

## Création d'une classe d'encapsulation

Pour une approche plus organisée, il est recommandé de créer une classe qui encapsule l'accès à la DLL :

```pascal
type
  TMathLibrary = class
  private
    FDLLHandle: THandle;
    FAddition: function(A, B: Integer): Integer; stdcall;
    FMultiplication: function(A, B: Integer): Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function EstChargee: Boolean;
    function Additionner(A, B: Integer): Integer;
    function Multiplier(A, B: Integer): Integer;
  end;

constructor TMathLibrary.Create;
begin
  inherited;
  FDLLHandle := LoadLibrary('MathLib.dll');
  if FDLLHandle <> 0 then
  begin
    @FAddition := GetProcAddress(FDLLHandle, 'Addition');
    @FMultiplication := GetProcAddress(FDLLHandle, 'Multiplication');
  end;
end;

destructor TMathLibrary.Destroy;
begin
  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);
  inherited;
end;

function TMathLibrary.EstChargee: Boolean;
begin
  Result := (FDLLHandle <> 0);
end;

function TMathLibrary.Additionner(A, B: Integer): Integer;
begin
  if Assigned(FAddition) then
    Result := FAddition(A, B)
  else
    raise Exception.Create('Fonction Addition non disponible');
end;

function TMathLibrary.Multiplier(A, B: Integer): Integer;
begin
  if Assigned(FMultiplication) then
    Result := FMultiplication(A, B)
  else
    raise Exception.Create('Fonction Multiplication non disponible');
end;
```

Utilisation :

```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  MathLib: TMathLibrary;
begin
  MathLib := TMathLibrary.Create;
  try
    if MathLib.EstChargee then
    begin
      ShowMessage('10 + 20 = ' + IntToStr(MathLib.Additionner(10, 20)));
      ShowMessage('10 * 20 = ' + IntToStr(MathLib.Multiplier(10, 20)));
    end
    else
      ShowMessage('La bibliothèque mathématique n''a pas pu être chargée');
  finally
    MathLib.Free;
  end;
end;
```

## Gestion des types de données

La conversion des types de données entre Delphi et les DLLs peut être complexe, surtout si la DLL a été écrite dans un autre langage.

### Types simples

Les types simples se correspondent généralement bien :

| Type Delphi | Type C/C++ |
|-------------|------------|
| Integer     | int        |
| Cardinal    | unsigned int |
| Single      | float      |
| Double      | double     |
| Boolean     | bool       |
| PChar       | char*      |
| PWideChar   | wchar_t*   |

### Types complexes et structures

Pour les structures et types complexes, nous devons déclarer des types équivalents :

```pascal
// Structure côté C/C++
// struct Point {
//   int x;
//   int y;
// };

// Déclaration équivalente en Delphi
type
  TPoint = record
    x: Integer;
    y: Integer;
  end;

// Fonction DLL qui utilise cette structure
function CalculerDistance(A, B: TPoint): Double; stdcall; external 'GeometrieLib.dll';
```

## Passage de chaînes de caractères

Le passage de chaînes nécessite une attention particulière :

```pascal
// Déclaration de la fonction dans la DLL
function AfficherMessage(Message: PChar): Integer; stdcall; external 'MessageLib.dll';

// Utilisation
var
  Message: string;
begin
  Message := 'Bonjour depuis Delphi !';
  AfficherMessage(PChar(Message));
end;
```

Pour les chaînes Unicode (WideString) :

```pascal
function AfficherMessageUnicode(Message: PWideChar): Integer; stdcall; external 'MessageLib.dll';

// Utilisation
var
  Message: string;  // string est Unicode par défaut depuis Delphi 2009
begin
  Message := 'Bonjour avec caractères spéciaux : éàçèù';
  AfficherMessageUnicode(PWideChar(Message));
end;
```

## Cas pratique : utilisation d'une DLL Windows

Voici un exemple d'utilisation de la DLL `user32.dll`, qui fait partie de Windows :

```pascal
// Déclaration de la fonction MessageBox de user32.dll
function MessageBox(hWnd: THandle; lpText, lpCaption: PChar; uType: Cardinal): Integer; stdcall; external 'user32.dll' name 'MessageBoxA';

// Pour la version Unicode
function MessageBoxW(hWnd: THandle; lpText, lpCaption: PWideChar; uType: Cardinal): Integer; stdcall; external 'user32.dll' name 'MessageBoxW';

procedure TForm1.ButtonDemoClick(Sender: TObject);
begin
  // Version ANSI
  MessageBox(0, 'Message de test', 'Titre', 0);

  // Version Unicode (recommandé pour Delphi moderne)
  MessageBoxW(0, 'Message avec caractères spéciaux: éèàù', 'Titre', 0);
end;
```

Notez l'utilisation de `name 'MessageBoxA'` et `name 'MessageBoxW'` pour spécifier le nom exact de la fonction dans la DLL. Cela est utile quand le nom de la fonction Pascal diffère du nom dans la DLL.

## Bonnes pratiques

1. **Vérification des erreurs** : Toujours vérifier si la DLL est correctement chargée et si les fonctions sont disponibles.

2. **Libération des ressources** : Assurez-vous de libérer la DLL avec `FreeLibrary` quand vous n'en avez plus besoin.

3. **Documentation** : Conservez la documentation des fonctions de la DLL, particulièrement si elle a été développée par un tiers.

4. **Versionnement** : Soyez attentif aux versions des DLLs. Un changement de version peut modifier le comportement des fonctions.

5. **Encapsulation** : Créez des classes ou unités dédiées pour encapsuler l'accès aux DLLs.

6. **Recherche de chemins** : Si la DLL n'est pas dans le même dossier que l'application ou dans le chemin système, spécifiez son chemin complet :

```pascal
DLLHandle := LoadLibrary('C:\Chemin\Vers\MaDLL.dll');
```

## Création de vos propres DLLs avec Delphi

Delphi permet également de créer vos propres DLLs. Pour cela, créez un nouveau projet de type "DLL" dans Delphi :

1. **File** → **New** → **Other** → **Delphi Projects** → **DLL**
2. Définissez les fonctions à exporter :

```pascal
library MathLib;

uses
  System.SysUtils;

function Addition(A, B: Integer): Integer; stdcall;
begin
  Result := A + B;
end;

function Multiplication(A, B: Integer): Integer; stdcall;
begin
  Result := A * B;
end;

exports
  Addition,
  Multiplication;

begin
end.
```

La section `exports` déclare les fonctions qui seront disponibles pour les applications qui utiliseront cette DLL.

## Conclusion

L'utilisation des DLLs en Delphi offre une grande flexibilité et permet d'étendre les capacités de vos applications en intégrant du code externe. Que vous utilisiez des DLLs existantes ou que vous créiez les vôtres, cette approche modulaire favorise la réutilisation du code et facilite la maintenance.

Les DLLs sont particulièrement utiles pour :
- Partager du code entre plusieurs applications
- Intégrer des bibliothèques tierces
- Accéder aux fonctionnalités du système d'exploitation
- Créer des architectures modulaires et extensibles

En maîtrisant les techniques présentées dans ce chapitre, vous pourrez tirer pleinement parti de cet aspect puissant de la programmation Delphi.
