# 3.5 Procédures et fonctions

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les procédures et fonctions sont des blocs de code réutilisables qui vous permettent d'organiser votre programme en modules. Elles sont essentielles pour créer du code bien structuré, lisible et maintenable. Dans cette section, nous explorons comment définir et utiliser ces éléments fondamentaux en Object Pascal.

## Pourquoi utiliser des procédures et fonctions ?

Les avantages principaux sont :

- **Réutilisation du code** : Écrire une fois, utiliser partout
- **Lisibilité** : Un code bien découpé est plus facile à comprendre
- **Maintenance** : Des modifications localisées dans une seule procédure
- **Modularité** : Découper un problème complexe en sous-problèmes plus simples
- **Tests** : Tester des unités de code isolées

## Différence entre procédure et fonction

La distinction principale est simple :

- Une **procédure** exécute une série d'actions sans renvoyer de valeur
- Une **fonction** exécute des actions ET renvoie une valeur

## Création d'une procédure

La syntaxe de base d'une procédure est :

```pascal
procedure NomDeLaProcedure(Paramètre1: Type1; Paramètre2: Type2);
begin
  // Code à exécuter
end;
```

Exemple d'une procédure simple :

```pascal
procedure AfficherBonjour;
begin
  ShowMessage('Bonjour !');
end;
```

Pour utiliser (appeler) cette procédure :

```pascal
AfficherBonjour;  // Affiche la boîte de dialogue "Bonjour !"
```

## Création d'une fonction

La syntaxe de base d'une fonction est :

```pascal
function NomDeLaFonction(Paramètre1: Type1; Paramètre2: Type2): TypeDeRetour;
begin
  // Code à exécuter

  Result := ValeurDeRetour;  // Valeur renvoyée
end;
```

Exemple d'une fonction simple :

```pascal
function Carre(Nombre: Integer): Integer;
begin
  Result := Nombre * Nombre;
end;
```

Pour utiliser cette fonction :

```pascal
var
  Resultat: Integer;
begin
  Resultat := Carre(5);  // Resultat = 25
  ShowMessage('Le carré de 5 est ' + IntToStr(Resultat));
end;
```

Le mot-clé `Result` est une variable spéciale qui contient la valeur que la fonction va renvoyer. Vous pouvez aussi utiliser une instruction `Exit` avec la valeur à renvoyer pour sortir immédiatement de la fonction :

```pascal
function EstPositif(Nombre: Integer): Boolean;
begin
  if Nombre > 0 then
    Exit(True);  // Sort immédiatement avec la valeur True

  Result := False;  // Exécuté seulement si Nombre <= 0
end;
```

## Paramètres

Les paramètres permettent de passer des données à vos procédures et fonctions.

### Types de paramètres

Delphi offre différentes façons de passer des paramètres :

#### Paramètres par valeur

Par défaut, les paramètres sont passés par valeur, ce qui signifie que la procédure ou fonction travaille sur une copie des données :

```pascal
procedure Incrementer(Nombre: Integer);
begin
  Nombre := Nombre + 1;  // Modifie la copie locale, pas la variable originale
  ShowMessage('Dans la procédure : ' + IntToStr(Nombre));
end;

var
  X: Integer;
begin
  X := 10;
  Incrementer(X);  // Affiche "Dans la procédure : 11"
  ShowMessage('Après appel : ' + IntToStr(X));  // Affiche "Après appel : 10"
end;
```

#### Paramètres par référence (var)

Pour modifier la variable originale, utilisez le mot-clé `var` :

```pascal
procedure Incrementer(var Nombre: Integer);
begin
  Nombre := Nombre + 1;  // Modifie la variable originale
  ShowMessage('Dans la procédure : ' + IntToStr(Nombre));
end;

var
  X: Integer;
begin
  X := 10;
  Incrementer(X);  // Affiche "Dans la procédure : 11"
  ShowMessage('Après appel : ' + IntToStr(X));  // Affiche "Après appel : 11"
end;
```

#### Paramètres constants (const)

Pour les paramètres que vous ne modifierez pas (surtout pour les objets volumineux comme les chaînes), utilisez `const` pour améliorer les performances :

```pascal
procedure AfficherMessage(const Message: string);
begin
  // Message ne peut pas être modifié ici
  ShowMessage(Message);
end;
```

#### Paramètres par constante (constref)

Depuis Delphi 11, vous pouvez utiliser `constref` pour les objets gérés (comme les chaînes et interfaces) :

```pascal
procedure AfficherMessage(constref Message: string);
begin
  ShowMessage(Message);
end;
```

<span style="color: #0066CC">**Nécessite Delphi 11 ou supérieur**</span>

#### Paramètres de sortie (out)

Pour les paramètres qui ne sont pas utilisés en entrée, mais seulement pour renvoyer des valeurs :

```pascal
procedure ObtenirDimensions(out Largeur, Hauteur: Integer);
begin
  // Les valeurs initiales de Largeur et Hauteur sont ignorées
  Largeur := 800;
  Hauteur := 600;
end;

var
  L, H: Integer;
begin
  ObtenirDimensions(L, H);
  ShowMessage(Format('Dimensions : %d x %d', [L, H]));
end;
```

### Paramètres par défaut

Depuis Delphi 2009, vous pouvez définir des valeurs par défaut pour les paramètres :

```pascal
procedure ConfigurerApplication(const Titre: string = 'Mon Application';
                               const Largeur: Integer = 800;
                               const Hauteur: Integer = 600);
begin
  // Utilisation des paramètres
  Form1.Caption := Titre;
  Form1.Width := Largeur;
  Form1.Height := Hauteur;
end;
```

Vous pouvez alors appeler cette procédure de différentes façons :

```pascal
ConfigurerApplication;  // Utilise tous les paramètres par défaut
ConfigurerApplication('Nouvelle App');  // Personnalise seulement le titre
ConfigurerApplication('Petite App', 400, 300);  // Personnalise tout
```

### Paramètres avec nom (named parameters)

Depuis Delphi 12, vous pouvez utiliser les paramètres nommés pour plus de clarté :

```pascal
ConfigurerApplication(Hauteur := 400, Titre := 'Application spéciale');
```

<span style="color: #0066CC">**Nécessite Delphi 12 ou supérieur**</span>

## Surcharge de procédures et fonctions

La surcharge permet de définir plusieurs versions d'une même procédure ou fonction avec des paramètres différents :

```pascal
// Calcule l'aire d'un carré
function CalculerAire(Cote: Double): Double;
begin
  Result := Cote * Cote;
end;

// Calcule l'aire d'un rectangle
function CalculerAire(Largeur, Hauteur: Double): Double;
begin
  Result := Largeur * Hauteur;
end;
```

Delphi choisit la bonne version en fonction des paramètres que vous passez :

```pascal
var
  AireCarre, AireRectangle: Double;
begin
  AireCarre := CalculerAire(5);  // Appelle la première version
  AireRectangle := CalculerAire(5, 10);  // Appelle la seconde version
end;
```

## Procédures et fonctions anonymes

Depuis Delphi 2009, vous pouvez créer des procédures et fonctions anonymes (aussi appelées "closures" ou "expressions lambda") :

```pascal
var
  Carre: TFunc<Integer, Integer>;
begin
  Carre := function(X: Integer): Integer
  begin
    Result := X * X;
  end;

  ShowMessage(IntToStr(Carre(5)));  // Affiche 25
end;
```

Elles sont particulièrement utiles pour les callbacks et les événements :

```pascal
Button1.OnClick := procedure(Sender: TObject)
begin
  ShowMessage('Bouton cliqué !');
end;
```

## Récursivité

Une fonction ou procédure peut s'appeler elle-même, ce qu'on appelle la récursivité :

```pascal
function Factorielle(N: Integer): Integer;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorielle(N - 1);
end;
```

Cet exemple calcule la factorielle d'un nombre (ex : 5! = 5×4×3×2×1 = 120).

```pascal
ShowMessage('5! = ' + IntToStr(Factorielle(5)));  // Affiche "5! = 120"
```

Attention : la récursivité mal contrôlée peut causer un dépassement de pile.

## Déclaration forward

Si vous avez besoin de deux procédures qui s'appellent mutuellement, vous devez utiliser une déclaration forward :

```pascal
procedure ProcedureB; forward;  // Déclaration préalable

procedure ProcedureA;
begin
  ShowMessage('Procédure A');
  ProcedureB;  // Appel à ProcedureB
end;

procedure ProcedureB;  // Implémentation réelle
begin
  ShowMessage('Procédure B');
  // Peut appeler ProcedureA si nécessaire
end;
```

## Organisation des procédures et fonctions

### Au niveau de l'unité

Les procédures et fonctions peuvent être définies au niveau de l'unité :

```pascal
unit MaCalculatrice;

interface

// Déclarations visibles par les autres unités
function Additionner(A, B: Integer): Integer;
function Soustraire(A, B: Integer): Integer;

implementation

// Implémentations
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function Soustraire(A, B: Integer): Integer;
begin
  Result := A - B;
end;

// Procédure privée, visible uniquement dans cette unité
procedure LogOperation(const Operation: string; A, B, Resultat: Integer);
begin
  // Code de journalisation
end;

end.
```

### Au sein d'une procédure

Vous pouvez définir des procédures et fonctions locales à l'intérieur d'autres procédures :

```pascal
procedure TraiterDonnees(const Donnees: array of Integer);
  // Fonction locale, accessible uniquement à l'intérieur de TraiterDonnees
  function Moyenne: Double;
  var
    Somme, I: Integer;
  begin
    Somme := 0;
    for I := 0 to High(Donnees) do
      Somme := Somme + Donnees[I];

    if Length(Donnees) > 0 then
      Result := Somme / Length(Donnees)
    else
      Result := 0;
  end;

begin
  ShowMessage('Moyenne : ' + FloatToStr(Moyenne));
  // Autres traitements...
end;
```

### Au sein d'une classe

Les procédures et fonctions peuvent être des méthodes de classes (nous verrons cela plus en détail dans la section sur la programmation orientée objet) :

```pascal
type
  TCalculatrice = class
  public
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
  end;

function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TCalculatrice.Soustraire(A, B: Integer): Integer;
begin
  Result := A - B;
end;
```

## Conseils pratiques

1. **Nommage explicite** :
   - Utilisez des noms qui décrivent clairement ce que fait la procédure ou fonction
   - Les verbes sont généralement bons pour les procédures (ex: `CalculerTotal`, `AfficherResultat`)
   - Les fonctions peuvent être nommées par ce qu'elles renvoient (ex: `Moyenne`, `EstValide`)

2. **Taille et responsabilité** :
   - Chaque procédure ou fonction devrait avoir une seule responsabilité claire
   - Visez des procédures de moins de 50 lignes si possible
   - Si une procédure devient trop grande, découpez-la en sous-procédures

3. **Documentation** :
   - Ajoutez des commentaires décrivant le but, les paramètres et les valeurs de retour
   ```pascal
   { Calcule la distance entre deux points.
     @param X1,Y1 Coordonnées du premier point
     @param X2,Y2 Coordonnées du second point
     @return La distance euclidienne entre les points
   }
   function Distance(X1, Y1, X2, Y2: Double): Double;
   ```

4. **Paramètres** :
   - Utilisez `const` pour les paramètres que vous ne modifiez pas (surtout pour les types complexes)
   - Limitez le nombre de paramètres (idéalement moins de 5)
   - Groupez les paramètres liés dans des records si nécessaire

5. **Portée** :
   - Gardez vos procédures et fonctions aussi privées que possible
   - N'exposez dans l'interface que ce qui est réellement nécessaire

---

Les procédures et fonctions sont les éléments de base qui vous permettent de structurer et d'organiser votre code. En maîtrisant ces concepts, vous serez capable de créer des applications Delphi bien conçues et faciles à maintenir. Dans la prochaine section, nous aborderons la gestion des exceptions, qui vous permettra de gérer proprement les erreurs dans vos applications.

⏭️ [Gestion des exceptions](/03-langage-object-pascal/06-gestion-des-exceptions.md)
