🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.4 Structures de contrôle (conditions, boucles)

## Introduction

Les structures de contrôle permettent de diriger le flux d'exécution d'un programme. Sans elles, le code s'exécuterait toujours de manière linéaire, ligne après ligne. Grâce aux structures de contrôle, vous pouvez :
- **Prendre des décisions** (conditions)
- **Répéter des actions** (boucles)
- **Contrôler le flux** de votre application

Ce sont les éléments fondamentaux qui donnent à vos programmes leur logique et leur intelligence.

## Les structures conditionnelles

Les structures conditionnelles permettent d'exécuter du code uniquement si une condition est remplie.

### L'instruction if...then

La structure la plus simple pour tester une condition.

**Syntaxe :**
```pascal
if condition then
  instruction;
```

**Exemple simple :**
```pascal
var
  Age: Integer;
begin
  Age := 20;

  if Age >= 18 then
    ShowMessage('Vous êtes majeur');
end;
```

**Avec un bloc d'instructions :**
```pascal
var
  Age: Integer;
begin
  Age := 20;

  if Age >= 18 then
  begin
    ShowMessage('Vous êtes majeur');
    ShowMessage('Vous pouvez voter');
    ShowMessage('Vous pouvez passer le permis');
  end;
end;
```

**Point important :** Si vous avez plusieurs instructions à exécuter, utilisez obligatoirement `begin...end`.

### L'instruction if...then...else

Permet d'exécuter du code alternatif si la condition n'est pas remplie.

**Syntaxe :**
```pascal
if condition then
  instruction_si_vrai
else
  instruction_si_faux;
```

**Exemple :**
```pascal
var
  Age: Integer;
begin
  Age := 15;

  if Age >= 18 then
    ShowMessage('Vous êtes majeur')
  else
    ShowMessage('Vous êtes mineur');
end;
```

**Avec des blocs :**
```pascal
var
  Age: Integer;
begin
  Age := 15;

  if Age >= 18 then
  begin
    ShowMessage('Vous êtes majeur');
    Label1.Caption := 'Adulte';
    Button1.Enabled := True;
  end
  else
  begin
    ShowMessage('Vous êtes mineur');
    Label1.Caption := 'Enfant/Adolescent';
    Button1.Enabled := False;
  end;
end;
```

**Attention :** Pas de point-virgule avant le `else` !

```pascal
// ❌ ERREUR : point-virgule avant else
if Condition then
  Instruction1;  // ← Point-virgule en trop !
else
  Instruction2;

// ✅ CORRECT
if Condition then
  Instruction1
else
  Instruction2;
```

### L'instruction if...then...else if

Pour tester plusieurs conditions successives.

**Syntaxe :**
```pascal
if condition1 then
  instruction1
else if condition2 then
  instruction2
else if condition3 then
  instruction3
else
  instruction_par_defaut;
```

**Exemple : Système de notation**
```pascal
var
  Note: Integer;
begin
  Note := 15;

  if Note >= 16 then
    ShowMessage('Mention : Très bien')
  else if Note >= 14 then
    ShowMessage('Mention : Bien')
  else if Note >= 12 then
    ShowMessage('Mention : Assez bien')
  else if Note >= 10 then
    ShowMessage('Mention : Passable')
  else
    ShowMessage('Mention : Insuffisant');
end;
```

**Exemple : Tarification**
```pascal
var
  Age: Integer;
  Tarif: Double;
begin
  Age := 25;

  if Age < 12 then
  begin
    Tarif := 5.0;
    ShowMessage('Tarif enfant');
  end
  else if Age < 18 then
  begin
    Tarif := 8.0;
    ShowMessage('Tarif adolescent');
  end
  else if Age < 65 then
  begin
    Tarif := 12.0;
    ShowMessage('Tarif adulte');
  end
  else
  begin
    Tarif := 9.0;
    ShowMessage('Tarif senior');
  end;

  ShowMessage('Prix : ' + FormatFloat('0.00 €', Tarif));
end;
```

### Conditions imbriquées

Vous pouvez imbriquer des structures `if` à l'intérieur d'autres `if`.

```pascal
var
  Age: Integer;
  APermis: Boolean;
begin
  Age := 20;
  APermis := True;

  if Age >= 18 then
  begin
    if APermis then
      ShowMessage('Vous pouvez conduire')
    else
      ShowMessage('Vous devez obtenir le permis');
  end
  else
    ShowMessage('Vous êtes trop jeune pour conduire');
end;
```

**Astuce :** Pour une meilleure lisibilité, utilisez des opérateurs logiques plutôt que d'imbriquer trop de conditions.

```pascal
// ✅ Plus lisible
if (Age >= 18) and APermis then
  ShowMessage('Vous pouvez conduire');

// ❌ Moins lisible
if Age >= 18 then
  if APermis then
    ShowMessage('Vous pouvez conduire');
```

### Opérateurs de comparaison dans les conditions

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `=` | Égal à | `if X = 5 then` |
| `<>` | Différent de | `if X <> 5 then` |
| `<` | Inférieur à | `if X < 5 then` |
| `>` | Supérieur à | `if X > 5 then` |
| `<=` | Inférieur ou égal | `if X <= 5 then` |
| `>=` | Supérieur ou égal | `if X >= 5 then` |

### Opérateurs logiques dans les conditions

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `and` | ET logique (toutes les conditions doivent être vraies) | `if (X > 0) and (Y > 0) then` |
| `or` | OU logique (au moins une condition doit être vraie) | `if (X > 0) or (Y > 0) then` |
| `not` | NON logique (inverse la condition) | `if not EstVide then` |
| `xor` | OU exclusif (une seule condition doit être vraie) | `if A xor B then` |

**Exemples pratiques :**
```pascal
var
  X, Y: Integer;
  Nom: string;
begin
  X := 10;
  Y := 20;
  Nom := 'Marie';

  // AND : toutes les conditions doivent être vraies
  if (X > 0) and (Y > 0) then
    ShowMessage('Les deux nombres sont positifs');

  // OR : au moins une condition doit être vraie
  if (X > 100) or (Y > 100) then
    ShowMessage('Au moins un nombre dépasse 100');

  // NOT : inverse le résultat
  if not (Nom = '') then
    ShowMessage('Le nom est renseigné');

  // Combinaison
  if (X > 0) and ((Y < 50) or (Nom <> '')) then
    ShowMessage('Condition complexe validée');
end;
```

### L'instruction case...of

Lorsque vous devez comparer une variable à plusieurs valeurs, `case...of` est plus lisible que de multiples `if...else if`.

**Syntaxe :**
```pascal
case variable of
  valeur1: instruction1;
  valeur2: instruction2;
  valeur3: instruction3;
else
  instruction_par_defaut;
end;
```

**Exemple avec des nombres :**
```pascal
var
  Jour: Integer;
begin
  Jour := 3;

  case Jour of
    1: ShowMessage('Lundi');
    2: ShowMessage('Mardi');
    3: ShowMessage('Mercredi');
    4: ShowMessage('Jeudi');
    5: ShowMessage('Vendredi');
    6: ShowMessage('Samedi');
    7: ShowMessage('Dimanche');
  else
    ShowMessage('Jour invalide');
  end;
end;
```

**Exemple avec des caractères :**
```pascal
var
  Note: Char;
begin
  Note := 'B';

  case Note of
    'A': ShowMessage('Excellent');
    'B': ShowMessage('Très bien');
    'C': ShowMessage('Bien');
    'D': ShowMessage('Passable');
    'F': ShowMessage('Échec');
  else
    ShowMessage('Note invalide');
  end;
end;
```

**Exemple avec des plages de valeurs :**
```pascal
var
  Age: Integer;
begin
  Age := 25;

  case Age of
    0..11:  ShowMessage('Enfant');
    12..17: ShowMessage('Adolescent');
    18..64: ShowMessage('Adulte');
    65..120: ShowMessage('Senior');
  else
    ShowMessage('Âge invalide');
  end;
end;
```

**Exemple avec des valeurs multiples :**
```pascal
var
  Mois: Integer;
begin
  Mois := 7;

  case Mois of
    1, 3, 5, 7, 8, 10, 12: ShowMessage('31 jours');
    4, 6, 9, 11: ShowMessage('30 jours');
    2: ShowMessage('28 ou 29 jours');
  else
    ShowMessage('Mois invalide');
  end;
end;
```

**Avec des blocs d'instructions :**
```pascal
var
  Choix: Integer;
begin
  Choix := 2;

  case Choix of
    1:
      begin
        ShowMessage('Option 1 sélectionnée');
        Label1.Caption := 'Choix 1';
        Button1.Enabled := True;
      end;
    2:
      begin
        ShowMessage('Option 2 sélectionnée');
        Label1.Caption := 'Choix 2';
        Button1.Enabled := False;
      end;
    3:
      begin
        ShowMessage('Option 3 sélectionnée');
        Label1.Caption := 'Choix 3';
        Button1.Enabled := True;
      end;
  else
    ShowMessage('Choix invalide');
  end;
end;
```

**Case avec des types énumérés :**
```pascal
type
  TJourSemaine = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  Jour: TJourSemaine;
begin
  Jour := Mercredi;

  case Jour of
    Lundi..Vendredi: ShowMessage('Jour ouvrable');
    Samedi, Dimanche: ShowMessage('Week-end');
  end;
end;
```

## Les boucles

Les boucles permettent de répéter des instructions plusieurs fois.

### La boucle for...to

Utilisée quand on connaît à l'avance le nombre d'itérations.

**Syntaxe :**
```pascal
for variable := valeur_debut to valeur_fin do
  instruction;
```

**Exemple simple :**
```pascal
var
  i: Integer;
begin
  for i := 1 to 10 do
    ShowMessage('Itération numéro ' + IntToStr(i));
end;
```

**Avec un bloc d'instructions :**
```pascal
var
  i: Integer;
  Total: Integer;
begin
  Total := 0;

  for i := 1 to 10 do
  begin
    Total := Total + i;
    ShowMessage('i = ' + IntToStr(i) + ', Total = ' + IntToStr(Total));
  end;

  ShowMessage('Somme totale : ' + IntToStr(Total));  // Affiche 55
end;
```

**Exemple : Remplir une liste :**
```pascal
var
  i: Integer;
begin
  ListBox1.Clear;

  for i := 1 to 20 do
    ListBox1.Items.Add('Élément ' + IntToStr(i));
end;
```

**Exemple : Calculer une factorielle :**
```pascal
var
  i, N: Integer;
  Factorielle: Int64;
begin
  N := 10;
  Factorielle := 1;

  for i := 1 to N do
    Factorielle := Factorielle * i;

  ShowMessage('Factorielle de ' + IntToStr(N) + ' = ' + IntToStr(Factorielle));
end;
```

### La boucle for...downto

Identique à `for...to` mais compte à rebours.

**Syntaxe :**
```pascal
for variable := valeur_debut downto valeur_fin do
  instruction;
```

**Exemple :**
```pascal
var
  i: Integer;
begin
  for i := 10 downto 1 do
    ShowMessage('Compte à rebours : ' + IntToStr(i));

  ShowMessage('Décollage !');
end;
```

**Exemple : Parcourir une liste à l'envers :**
```pascal
var
  i: Integer;
begin
  for i := ListBox1.Items.Count - 1 downto 0 do
    ShowMessage(ListBox1.Items[i]);
end;
```

### Boucle for avec variable inline (Delphi 10.3+)

Depuis Delphi 10.3 Rio, vous pouvez déclarer la variable directement dans la boucle.

```pascal
begin
  for var i: Integer := 1 to 10 do
    ShowMessage('Itération : ' + IntToStr(i));
end;
```

**Avantage :** La variable `i` n'existe que dans la boucle, ce qui évite les conflits de noms.

### La boucle for...in

Permet de parcourir facilement des collections (tableaux, listes, etc.).

**Syntaxe :**
```pascal
for element in collection do
  instruction;
```

**Exemple avec un tableau :**
```pascal
var
  Nombres: array[0..4] of Integer;
  Nombre: Integer;
begin
  Nombres[0] := 10;
  Nombres[1] := 20;
  Nombres[2] := 30;
  Nombres[3] := 40;
  Nombres[4] := 50;

  for Nombre in Nombres do
    ShowMessage(IntToStr(Nombre));
end;
```

**Exemple avec un tableau dynamique :**
```pascal
var
  Prenoms: array of string;
  Prenom: string;
begin
  SetLength(Prenoms, 4);
  Prenoms[0] := 'Marie';
  Prenoms[1] := 'Jean';
  Prenoms[2] := 'Sophie';
  Prenoms[3] := 'Pierre';

  for Prenom in Prenoms do
    ShowMessage('Bonjour ' + Prenom);
end;
```

**Exemple avec une TStringList :**
```pascal
var
  Liste: TStringList;
  Element: string;
begin
  Liste := TStringList.Create;
  try
    Liste.Add('Apple');
    Liste.Add('Banana');
    Liste.Add('Cherry');

    for Element in Liste do
      ShowMessage(Element);
  finally
    Liste.Free;
  end;
end;
```

### La boucle while...do

Répète tant qu'une condition est vraie. La condition est testée **avant** chaque itération.

**Syntaxe :**
```pascal
while condition do
  instruction;
```

**Exemple simple :**
```pascal
var
  Compteur: Integer;
begin
  Compteur := 1;

  while Compteur <= 10 do
  begin
    ShowMessage('Compteur : ' + IntToStr(Compteur));
    Inc(Compteur);
  end;
end;
```

**Exemple : Saisie utilisateur :**
```pascal
var
  Reponse: string;
begin
  Reponse := '';

  while Reponse <> 'oui' do
  begin
    Reponse := InputBox('Question', 'Tapez "oui" pour continuer', '');
    Reponse := LowerCase(Reponse);
  end;

  ShowMessage('Vous avez dit oui !');
end;
```

**Exemple : Lecture de fichier :**
```pascal
var
  Fichier: TextFile;
  Ligne: string;
begin
  AssignFile(Fichier, 'C:\data.txt');
  Reset(Fichier);
  try
    while not Eof(Fichier) do
    begin
      ReadLn(Fichier, Ligne);
      Memo1.Lines.Add(Ligne);
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

**Attention avec while :**
```pascal
// ❌ DANGER : Boucle infinie si la condition reste toujours vraie
var
  X: Integer;
begin
  X := 1;
  while X > 0 do
  begin
    ShowMessage('X = ' + IntToStr(X));
    // X n'est jamais modifié → boucle infinie !
  end;
end;

// ✅ CORRECT : La variable change dans la boucle
var
  X: Integer;
begin
  X := 10;
  while X > 0 do
  begin
    ShowMessage('X = ' + IntToStr(X));
    Dec(X);  // X diminue à chaque itération
  end;
end;
```

### La boucle repeat...until

Répète jusqu'à ce qu'une condition soit vraie. La condition est testée **après** chaque itération (au moins une exécution garantie).

**Syntaxe :**
```pascal
repeat
  instruction1;
  instruction2;
until condition;
```

**Différence avec while :**
- `while` : exécute tant que la condition est **vraie** (test avant)
- `repeat...until` : exécute jusqu'à ce que la condition soit **vraie** (test après)

**Exemple simple :**
```pascal
var
  Compteur: Integer;
begin
  Compteur := 1;

  repeat
    ShowMessage('Compteur : ' + IntToStr(Compteur));
    Inc(Compteur);
  until Compteur > 10;
end;
```

**Exemple : Menu interactif :**
```pascal
var
  Choix: string;
begin
  repeat
    Choix := InputBox('Menu', 'Choisissez une option (1, 2, 3 ou Q pour quitter)', '');

    case Choix of
      '1': ShowMessage('Option 1 sélectionnée');
      '2': ShowMessage('Option 2 sélectionnée');
      '3': ShowMessage('Option 3 sélectionnée');
      'Q', 'q': ShowMessage('Au revoir !');
    else
      ShowMessage('Choix invalide');
    end;
  until (Choix = 'Q') or (Choix = 'q');
end;
```

**Exemple : Validation de saisie :**
```pascal
var
  Nombre: Integer;
  Saisie: string;
begin
  repeat
    Saisie := InputBox('Nombre', 'Entrez un nombre entre 1 et 100', '');

    if not TryStrToInt(Saisie, Nombre) then
    begin
      ShowMessage('Veuillez entrer un nombre valide');
      Nombre := 0;
    end
    else if (Nombre < 1) or (Nombre > 100) then
      ShowMessage('Le nombre doit être entre 1 et 100');
  until (Nombre >= 1) and (Nombre <= 100);

  ShowMessage('Vous avez saisi : ' + IntToStr(Nombre));
end;
```

**Point important :** `repeat...until` ne nécessite pas de `begin...end` car `repeat` et `until` jouent déjà ce rôle.

### Comparaison while vs repeat...until

```pascal
// WHILE : peut ne jamais s'exécuter
var
  X: Integer;
begin
  X := 100;
  while X < 10 do  // Condition fausse dès le début
  begin
    ShowMessage('Ceci ne s''affichera jamais');
    Inc(X);
  end;
end;

// REPEAT...UNTIL : s'exécute au moins une fois
var
  X: Integer;
begin
  X := 100;
  repeat
    ShowMessage('Ceci s''affichera une fois');  // S'exécute même si X >= 10
    Inc(X);
  until X >= 10;
end;
```

## Instructions de contrôle de boucle

### Break : Sortir d'une boucle

`Break` permet de sortir immédiatement d'une boucle.

```pascal
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    ShowMessage('i = ' + IntToStr(i));

    if i = 5 then
      Break;  // Sort de la boucle quand i = 5
  end;

  ShowMessage('Boucle terminée');
end;
```

**Exemple : Recherche dans une liste :**
```pascal
var
  i: Integer;
  Trouve: Boolean;
  Recherche: string;
begin
  Recherche := 'Sophie';
  Trouve := False;

  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    if ListBox1.Items[i] = Recherche then
    begin
      Trouve := True;
      ShowMessage('Trouvé à la position ' + IntToStr(i));
      Break;  // Inutile de continuer
    end;
  end;

  if not Trouve then
    ShowMessage('Non trouvé');
end;
```

### Continue : Passer à l'itération suivante

`Continue` saute le reste de l'itération en cours et passe à la suivante.

```pascal
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    if i mod 2 = 0 then
      Continue;  // Saute les nombres pairs

    ShowMessage('Nombre impair : ' + IntToStr(i));
  end;
end;
```

**Exemple : Filtrer des données :**
```pascal
var
  i: Integer;
begin
  ListBox2.Clear;

  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    // Ignorer les lignes vides
    if ListBox1.Items[i] = '' then
      Continue;

    // Ignorer les lignes commençant par #
    if Copy(ListBox1.Items[i], 1, 1) = '#' then
      Continue;

    // Ajouter les autres lignes
    ListBox2.Items.Add(ListBox1.Items[i]);
  end;
end;
```

### Exit : Sortir d'une procédure ou fonction

`Exit` permet de quitter immédiatement une procédure ou fonction.

```pascal
procedure Verifier(const Nom: string);  
begin  
  if Nom = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Exit;  // Sort de la procédure
  end;

  if Length(Nom) < 3 then
  begin
    ShowMessage('Le nom doit contenir au moins 3 caractères');
    Exit;
  end;

  ShowMessage('Nom valide : ' + Nom);
end;
```

**Avec une fonction :**
```pascal
function EstNombreValide(Nombre: Integer): Boolean;  
begin  
  Result := False;  // Valeur par défaut

  if Nombre < 1 then
    Exit;  // Sort avec Result = False

  if Nombre > 100 then
    Exit;

  Result := True;  // Nombre valide
end;
```

## Boucles imbriquées

Vous pouvez imbriquer des boucles les unes dans les autres.

**Exemple : Table de multiplication :**
```pascal
var
  i, j: Integer;
  Resultat: string;
begin
  Memo1.Clear;

  for i := 1 to 10 do
  begin
    Resultat := '';
    for j := 1 to 10 do
      Resultat := Resultat + Format('%4d', [i * j]);

    Memo1.Lines.Add(Resultat);
  end;
end;
```

**Exemple : Parcourir une grille :**
```pascal
var
  Ligne, Colonne: Integer;
begin
  for Ligne := 0 to StringGrid1.RowCount - 1 do
  begin
    for Colonne := 0 to StringGrid1.ColCount - 1 do
    begin
      StringGrid1.Cells[Colonne, Ligne] :=
        Format('L%d-C%d', [Ligne, Colonne]);
    end;
  end;
end;
```

**Break dans des boucles imbriquées :**
```pascal
var
  i, j: Integer;
  Trouve: Boolean;
begin
  Trouve := False;

  for i := 1 to 10 do
  begin
    for j := 1 to 10 do
    begin
      if (i * j) = 50 then
      begin
        ShowMessage(Format('Trouvé : %d × %d = 50', [i, j]));
        Trouve := True;
        Break;  // Sort de la boucle interne uniquement
      end;
    end;

    if Trouve then
      Break;  // Sort de la boucle externe
  end;
end;
```

## Bonnes pratiques

### 1. Choisir la bonne structure

```pascal
// ✅ BON : for quand on connaît le nombre d'itérations
for i := 1 to 10 do
  Traiter(i);

// ✅ BON : while pour les conditions complexes
while (not Eof(Fichier)) and (LignesLues < MaxLignes) do
  LireLigne;

// ✅ BON : repeat...until pour garantir au moins une exécution
repeat
  Saisie := DemanderSaisie;
until SaisieValide(Saisie);

// ✅ BON : case pour comparer à plusieurs valeurs
case CodeRetour of
  0: Succes;
  1: Avertissement;
  2: Erreur;
end;
```

### 2. Éviter les boucles infinies

```pascal
// ❌ DANGER : Boucle infinie
while True do
  DoSomething;  // N'a aucune condition de sortie

// ✅ BON : Condition de sortie claire
var
  Continuer: Boolean;
begin
  Continuer := True;
  while Continuer do
  begin
    DoSomething;
    Continuer := DemanderContinuer;
  end;
end;
```

### 3. Initialiser les variables de boucle

```pascal
// ❌ MAUVAIS : Variable non initialisée
var
  Total: Integer;
  i: Integer;
begin
  for i := 1 to 10 do
    Total := Total + i;  // Total a une valeur aléatoire au départ
end;

// ✅ BON : Variable initialisée
var
  Total: Integer;
  i: Integer;
begin
  Total := 0;
  for i := 1 to 10 do
    Total := Total + i;
end;
```

### 4. Utiliser des noms de variables explicites

```pascal
// ❌ Peu clair
for i := 0 to c - 1 do
  p := p + v[i];

// ✅ Clair
for Indice := 0 to NombreElements - 1 do
  Total := Total + Valeurs[Indice];
```

### 5. Limiter la profondeur d'imbrication

```pascal
// ❌ Trop d'imbrication (difficile à lire)
if Condition1 then
  if Condition2 then
    if Condition3 then
      if Condition4 then
        DoSomething;

// ✅ Utiliser des opérateurs logiques
if Condition1 and Condition2 and Condition3 and Condition4 then
  DoSomething;

// ✅ Ou sortir tôt
if not Condition1 then Exit;  
if not Condition2 then Exit;  
if not Condition3 then Exit;  
if not Condition4 then Exit;  
DoSomething;  
```

### 6. Utiliser begin...end pour la clarté

```pascal
// ❌ Ambigu sans begin...end
if Condition then
  Instruction1;
  Instruction2;  // S'exécute toujours, pas dans le if !

// ✅ Clair avec begin...end
if Condition then  
begin  
  Instruction1;
  Instruction2;
end;
```

## Erreurs courantes à éviter

### Erreur 1 : Point-virgule avant else

```pascal
// ❌ ERREUR
if Condition then
  DoSomething;  // ← Point-virgule problématique
else
  DoSomethingElse;

// ✅ CORRECT
if Condition then
  DoSomething
else
  DoSomethingElse;
```

### Erreur 2 : Modification de la variable de contrôle dans for

```pascal
// ❌ À ÉVITER : Modifier i dans une boucle for
for i := 1 to 10 do  
begin  
  ShowMessage(IntToStr(i));
  i := i + 1;  // ← Ne faites pas cela !
end;

// ✅ Utilisez while si vous devez contrôler manuellement
i := 1;  
while i <= 10 do  
begin  
  ShowMessage(IntToStr(i));
  i := i + 2;  // OK dans une boucle while
end;
```

### Erreur 3 : Confusion entre = et :=

```pascal
// ❌ ERREUR : = au lieu de :=
if Nombre = 5 then
  Nombre = 10;  // ← Erreur de syntaxe

// ✅ CORRECT
if Nombre = 5 then
  Nombre := 10;
```

### Erreur 4 : Oublier le else dans case

```pascal
// ❌ Pas de gestion des cas non prévus
case Valeur of
  1: DoAction1;
  2: DoAction2;
end;  // Que se passe-t-il si Valeur = 3 ?

// ✅ Toujours prévoir un cas par défaut
case Valeur of
  1: DoAction1;
  2: DoAction2;
else
  ShowMessage('Valeur non gérée');
end;
```

### Erreur 5 : Accès hors limites dans les boucles

```pascal
// ❌ ERREUR : Dépasse les limites du tableau
var
  Tableau: array[0..9] of Integer;
  i: Integer;
begin
  for i := 0 to 10 do  // ← Erreur : le tableau va de 0 à 9
    Tableau[i] := i;
end;

// ✅ CORRECT
var
  Tableau: array[0..9] of Integer;
  i: Integer;
begin
  for i := 0 to 9 do  // ou : Low(Tableau) to High(Tableau)
    Tableau[i] := i;
end;
```

## Points clés à retenir

1. **if...then...else** : Pour les décisions simples
2. **case...of** : Pour comparer une variable à plusieurs valeurs
3. **for** : Quand on connaît le nombre d'itérations
4. **while** : Répète tant que la condition est vraie (test avant)
5. **repeat...until** : Répète jusqu'à ce que la condition soit vraie (test après)
6. **Break** : Sortir d'une boucle
7. **Continue** : Passer à l'itération suivante
8. **Exit** : Sortir d'une procédure/fonction
9. Toujours utiliser `begin...end` pour plusieurs instructions
10. Pas de point-virgule avant `else`

---

La maîtrise des structures de contrôle est essentielle pour créer des programmes logiques et efficaces. Dans la section suivante, nous découvrirons les procédures et fonctions qui permettent d'organiser et de réutiliser votre code.

⏭️ [Procédures et fonctions](/03-langage-object-pascal/05-procedures-et-fonctions.md)
