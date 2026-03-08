🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.6 Gestion des exceptions

## Introduction

Les exceptions sont des événements anormaux qui surviennent pendant l'exécution d'un programme et qui perturbent son flux normal. Une bonne gestion des exceptions permet de :
- **Éviter les plantages** de l'application
- **Informer l'utilisateur** de manière claire
- **Maintenir l'intégrité** des données
- **Faciliter le débogage** en identifiant les problèmes
- **Rendre le code plus robuste** et professionnel

Sans gestion d'exceptions, une simple erreur (division par zéro, fichier introuvable, etc.) peut faire planter toute l'application.

## Qu'est-ce qu'une exception ?

Une exception est un objet qui représente une erreur ou une condition anormale. Lorsqu'une erreur se produit, on dit que le programme **"lève une exception"** (raise an exception).

**Exemples de situations générant des exceptions :**
- Division par zéro
- Conversion de chaîne invalide (transformer "abc" en nombre)
- Fichier introuvable
- Accès à un index hors limites d'un tableau
- Mémoire insuffisante
- Connexion réseau perdue

## Programme sans gestion d'exceptions

Voyons ce qui se passe sans gestion d'exceptions :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nombre: Integer;
begin
  Nombre := StrToInt(Edit1.Text);  // Si l'utilisateur entre "abc", le programme plante !
  ShowMessage('Nombre saisi : ' + IntToStr(Nombre));
end;
```

**Problème :** Si l'utilisateur entre du texte non numérique, l'application génère une erreur et peut se fermer brutalement.

## La structure try...except

La structure `try...except` permet d'intercepter et de gérer les exceptions.

**Syntaxe :**
```pascal
try
  // Code susceptible de générer une exception
except
  // Code exécuté en cas d'exception
end;
```

**Exemple simple :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nombre: Integer;
begin
  try
    Nombre := StrToInt(Edit1.Text);
    ShowMessage('Nombre saisi : ' + IntToStr(Nombre));
  except
    ShowMessage('Erreur : veuillez entrer un nombre valide');
  end;
end;
```

**Avantage :** Le programme ne plante plus. L'utilisateur reçoit un message clair et peut corriger sa saisie.

### Capturer le message d'exception

Vous pouvez récupérer l'exception pour afficher son message :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nombre: Integer;
begin
  try
    Nombre := StrToInt(Edit1.Text);
    ShowMessage('Nombre saisi : ' + IntToStr(Nombre));
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

**Explication :**
- `on E: Exception do` : capture toutes les exceptions dans la variable `E`
- `E.Message` : contient le message d'erreur descriptif

### Capturer des types d'exceptions spécifiques

Vous pouvez traiter différemment selon le type d'exception :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nombre: Integer;
begin
  try
    Nombre := StrToInt(Edit1.Text);
    Nombre := 100 div Nombre;  // Division
    ShowMessage('Résultat : ' + IntToStr(Nombre));
  except
    on E: EConvertError do
      ShowMessage('Format de nombre invalide : ' + E.Message);
    on E: EDivByZero do
      ShowMessage('Erreur : division par zéro impossible');
    on E: Exception do
      ShowMessage('Erreur inattendue : ' + E.Message);
  end;
end;
```

**Important :** Les exceptions sont testées dans l'ordre. Placez les exceptions les plus spécifiques en premier, et l'exception générale `Exception` en dernier.

### Exemple : Ouverture de fichier

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Fichier: TextFile;
  Ligne: string;
begin
  try
    AssignFile(Fichier, 'C:\données.txt');
    Reset(Fichier);

    while not Eof(Fichier) do
    begin
      ReadLn(Fichier, Ligne);
      Memo1.Lines.Add(Ligne);
    end;

    CloseFile(Fichier);
    ShowMessage('Fichier chargé avec succès');
  except
    on E: EInOutError do
      ShowMessage('Erreur d''accès au fichier : ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## La structure try...finally

La structure `try...finally` garantit qu'un code sera exécuté, **qu'une exception se produise ou non**. Elle est essentielle pour libérer des ressources (fichiers, objets, connexions, etc.).

**Syntaxe :**
```pascal
try
  // Code susceptible de générer une exception
finally
  // Code exécuté TOUJOURS (avec ou sans exception)
end;
```

### Exemple : Libération d'objet

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    Liste.LoadFromFile('C:\données.txt');
    Memo1.Lines.Assign(Liste);
  finally
    Liste.Free;  // Libération garantie, même en cas d'erreur
  end;
end;
```

**Pourquoi finally est important :**
Sans `finally`, si une exception se produit pendant `LoadFromFile`, l'objet `Liste` ne serait jamais libéré, causant une fuite mémoire.

### Exemple : Fermeture de fichier

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Fichier: TextFile;
  Ligne: string;
begin
  AssignFile(Fichier, 'C:\données.txt');
  Reset(Fichier);
  try
    while not Eof(Fichier) do
    begin
      ReadLn(Fichier, Ligne);
      Memo1.Lines.Add(Ligne);
    end;
  finally
    CloseFile(Fichier);  // Fermeture garantie
  end;
end;
```

### Exemple : Curseur de souris

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Screen.Cursor := crHourGlass;  // Curseur sablier
  try
    // Traitement long
    Sleep(5000);
    // ... opérations ...
  finally
    Screen.Cursor := crDefault;  // Restauration garantie du curseur
  end;
end;
```

### Exemple : Activation/désactivation de contrôles

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Button1.Enabled := False;
  try
    // Traitement
    TraiterDonnees;
  finally
    Button1.Enabled := True;  // Réactivation garantie
  end;
end;
```

## La structure try...except...finally

Vous pouvez combiner `except` et `finally` pour gérer les exceptions ET garantir le nettoyage :

**Syntaxe :**
```pascal
try
  // Code susceptible de générer une exception
except
  // Gestion des exceptions
end;  
finally  
  // Nettoyage (toujours exécuté)
end;
```

**Attention :** Cette syntaxe n'est pas directement supportée. Il faut imbriquer deux blocs try :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    try
      Liste.LoadFromFile(Edit1.Text);
      Memo1.Lines.Assign(Liste);
      ShowMessage('Fichier chargé avec succès');
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement : ' + E.Message);
    end;
  finally
    Liste.Free;  // Toujours libéré
  end;
end;
```

**Autre approche (plus lisible) :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    Liste.LoadFromFile(Edit1.Text);
    Memo1.Lines.Assign(Liste);
    ShowMessage('Fichier chargé avec succès');
  except
    on E: Exception do
      ShowMessage('Erreur lors du chargement : ' + E.Message);
  end;
  // Libération après le bloc try...except
  Liste.Free;
end;
```

**Mais attention :** Cette dernière approche ne garantit pas la libération si une exception se produit APRÈS le bloc except. Utilisez `try...finally` autour du tout pour être sûr.

**Meilleure pratique :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    try
      Liste.LoadFromFile(Edit1.Text);
      Memo1.Lines.Assign(Liste);
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  finally
    Liste.Free;
  end;
end;
```

## Lever une exception (raise)

Vous pouvez lever vos propres exceptions pour signaler des erreurs.

**Syntaxe :**
```pascal
raise Exception.Create('Message d''erreur');
```

### Exemple : Validation de données

```pascal
procedure ValiderAge(Age: Integer);  
begin  
  if Age < 0 then
    raise Exception.Create('L''âge ne peut pas être négatif');

  if Age > 150 then
    raise Exception.Create('L''âge semble invalide');
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Age: Integer;
begin
  try
    Age := StrToInt(Edit1.Text);
    ValiderAge(Age);
    ShowMessage('Âge valide : ' + IntToStr(Age));
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Exemple : Division sécurisée

```pascal
function DiviserSecurise(A, B: Double): Double;  
begin  
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');

  Result := A / B;
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Resultat: Double;
begin
  try
    Resultat := DiviserSecurise(10, 0);
    ShowMessage(FloatToStr(Resultat));
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Re-lever une exception (raise sans paramètre)

Vous pouvez intercepter une exception, effectuer une action, puis la re-lever :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Code risqué
    TraiterDonnees;
  except
    on E: Exception do
    begin
      // Journaliser l'erreur
      Log('Erreur survenue : ' + E.Message);

      // Re-lever l'exception pour qu'elle soit gérée plus haut
      raise;
    end;
  end;
end;
```

## Types d'exceptions courants

Delphi fournit de nombreux types d'exceptions prédéfinis :

| Exception | Description | Exemple |
|-----------|-------------|---------|
| `Exception` | Exception de base (parent de toutes) | Toute erreur générique |
| `EConvertError` | Erreur de conversion | `StrToInt('abc')` |
| `EDivByZero` | Division par zéro | `X := 10 div 0` |
| `EInOutError` | Erreur d'entrée/sortie | Fichier introuvable |
| `ERangeError` | Dépassement de plage | Accès hors limites d'un tableau |
| `EAccessViolation` | Violation d'accès mémoire | Pointeur nil déréférencé |
| `EOutOfMemory` | Mémoire insuffisante | Allocation impossible |
| `EIntOverflow` | Dépassement d'entier | Résultat trop grand |
| `EInvalidOp` | Opération invalide | Opération mathématique invalide |
| `EAbort` | Abandon silencieux | Interruption volontaire |

### Exemples d'exceptions courantes

```pascal
// EConvertError
try
  X := StrToInt('abc');
except
  on E: EConvertError do
    ShowMessage('Conversion impossible');
end;

// EDivByZero
try
  X := 10 div 0;
except
  on E: EDivByZero do
    ShowMessage('Division par zéro');
end;

// ERangeError
var
  Tableau: array[0..9] of Integer;
begin
  try
    Tableau[100] := 5;  // Hors limites
  except
    on E: ERangeError do
      ShowMessage('Index hors limites');
  end;
end;

// EAccessViolation
var
  Objet: TStringList;
begin
  Objet := nil;
  try
    Objet.Add('test');  // Objet nil
  except
    on E: EAccessViolation do
      ShowMessage('Objet non initialisé');
  end;
end;
```

## Créer des exceptions personnalisées

Vous pouvez créer vos propres types d'exceptions pour des situations spécifiques.

**Syntaxe :**
```pascal
type
  EMonException = class(Exception);
```

**Exemple simple :**
```pascal
type
  EAgeInvalide = class(Exception);

procedure ValiderAge(Age: Integer);  
begin  
  if (Age < 0) or (Age > 150) then
    raise EAgeInvalide.Create('Âge invalide : ' + IntToStr(Age));
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Age: Integer;
begin
  try
    Age := StrToInt(Edit1.Text);
    ValiderAge(Age);
    ShowMessage('Âge valide');
  except
    on E: EAgeInvalide do
      ShowMessage('Erreur de validation : ' + E.Message);
    on E: EConvertError do
      ShowMessage('Veuillez entrer un nombre');
  end;
end;
```

**Exemple avec plusieurs exceptions personnalisées :**
```pascal
type
  ECompteInvalide = class(Exception);
  ESoldeInsuffisant = class(Exception);
  EMontantNegatif = class(Exception);

procedure Retirer(Compte: TCompte; Montant: Double);  
begin  
  if Compte = nil then
    raise ECompteInvalide.Create('Compte non initialisé');

  if Montant < 0 then
    raise EMontantNegatif.Create('Le montant ne peut pas être négatif');

  if Compte.Solde < Montant then
    raise ESoldeInsuffisant.CreateFmt('Solde insuffisant. Disponible : %.2f €',
                                     [Compte.Solde]);

  Compte.Solde := Compte.Solde - Montant;
end;

// Utilisation
try
  Retirer(MonCompte, 100);
except
  on E: ECompteInvalide do
    ShowMessage('Erreur de compte : ' + E.Message);
  on E: ESoldeInsuffisant do
    ShowMessage('Erreur : ' + E.Message);
  on E: EMontantNegatif do
    ShowMessage('Montant invalide : ' + E.Message);
end;
```

### Exception avec données supplémentaires

```pascal
type
  EValidationErreur = class(Exception)
  private
    FChamp: string;
    FValeur: string;
  public
    constructor Create(const Champ, Valeur, Message: string);
    property Champ: string read FChamp;
    property Valeur: string read FValeur;
  end;

constructor EValidationErreur.Create(const Champ, Valeur, Message: string);  
begin  
  inherited Create(Message);
  FChamp := Champ;
  FValeur := Valeur;
end;

// Utilisation
procedure ValiderEmail(const Email: string);  
begin  
  if Pos('@', Email) = 0 then
    raise EValidationErreur.Create('Email', Email, 'Format d''email invalide');
end;

// Capture
try
  ValiderEmail('adresse.invalide');
except
  on E: EValidationErreur do
    ShowMessage(Format('Erreur dans le champ "%s" (valeur: "%s"): %s',
                      [E.Champ, E.Valeur, E.Message]));
end;
```

## EAbort : Exception silencieuse

`EAbort` est une exception spéciale qui ne génère pas de message d'erreur. Elle est utilisée pour interrompre proprement une opération.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  if MessageDlg('Continuer ?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Abort;  // Lève EAbort

  // Suite du traitement
  TraiterDonnees;
end;
```

**Particularité :** Si `EAbort` n'est pas capturé, il ne génère pas de message d'erreur à l'utilisateur.

## Fonctions de conversion sécurisées

Delphi fournit des versions sécurisées des fonctions de conversion qui ne lèvent pas d'exception :

### TryStrToInt

```pascal
var
  Nombre: Integer;
begin
  if TryStrToInt(Edit1.Text, Nombre) then
    ShowMessage('Nombre : ' + IntToStr(Nombre))
  else
    ShowMessage('Conversion impossible');
end;
```

### TryStrToFloat

```pascal
var
  Valeur: Double;
begin
  if TryStrToFloat(Edit1.Text, Valeur) then
    ShowMessage('Valeur : ' + FloatToStr(Valeur))
  else
    ShowMessage('Conversion impossible');
end;
```

### TryStrToDate

```pascal
var
  Date: TDateTime;
begin
  if TryStrToDate(Edit1.Text, Date) then
    ShowMessage('Date : ' + DateToStr(Date))
  else
    ShowMessage('Format de date invalide');
end;
```

**Avantage des fonctions Try... :**
- Plus rapides (pas de gestion d'exception)
- Code plus lisible pour les validations simples
- Recommandées pour les conversions fréquentes

## Bonnes pratiques

### 1. Être spécifique dans la capture

```pascal
// ✅ BON : capture spécifique
try
  Nombre := StrToInt(Edit1.Text);
except
  on E: EConvertError do
    ShowMessage('Format invalide : ' + E.Message);
end;

// ❌ Moins bon : capture tout
try
  Nombre := StrToInt(Edit1.Text);
except
  ShowMessage('Erreur');  // Quelle erreur ? On ne sait pas
end;
```

### 2. Toujours libérer les ressources avec finally

```pascal
// ✅ BON : libération garantie
Liste := TStringList.Create;  
try  
  // Utilisation
finally
  Liste.Free;
end;

// ❌ MAUVAIS : risque de fuite mémoire
Liste := TStringList.Create;
// Utilisation
Liste.Free;  // Si erreur avant, Free n'est pas appelé
```

### 3. Ne pas capturer les exceptions qu'on ne peut pas gérer

```pascal
// ❌ MAUVAIS : capture et ignore
try
  OperationCritique;
except
  // Ne rien faire cache le problème
end;

// ✅ BON : laisser remonter ou gérer correctement
try
  OperationCritique;
except
  on E: Exception do
  begin
    Log('Erreur critique : ' + E.Message);
    raise;  // Re-lever pour que l'appelant sache
  end;
end;
```

### 4. Utiliser des messages clairs

```pascal
// ❌ MAUVAIS : message vague
raise Exception.Create('Erreur');

// ✅ BON : message descriptif
raise Exception.Create('Impossible d''ouvrir le fichier : ' + NomFichier);
```

### 5. Valider tôt, échouer vite

```pascal
// ✅ BON : validation au début
function Diviser(A, B: Double): Double;  
begin  
  if B = 0 then
    raise Exception.Create('Division par zéro');

  Result := A / B;
end;

// ❌ Moins bon : vérification tardive
function Diviser(A, B: Double): Double;  
begin  
  // Beaucoup de code...

  if B = 0 then
    // Erreur découverte trop tard
    raise Exception.Create('Division par zéro');

  Result := A / B;
end;
```

### 6. Préférer Try... pour les conversions fréquentes

```pascal
// ✅ BON : plus performant
if TryStrToInt(Edit1.Text, Nombre) then
  Traiter(Nombre)
else
  ShowMessage('Nombre invalide');

// ❌ Moins efficace pour des conversions fréquentes
try
  Nombre := StrToInt(Edit1.Text);
  Traiter(Nombre);
except
  on E: EConvertError do
    ShowMessage('Nombre invalide');
end;
```

### 7. Documenter les exceptions levées

```pascal
/// <summary>
/// Divise deux nombres
/// </summary>
/// <exception cref="Exception">Levée si le diviseur est zéro</exception>
function Diviser(A, B: Double): Double;  
begin  
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');

  Result := A / B;
end;
```

### 8. Laisser finally s'occuper du nettoyage

```pascal
procedure TraiterFichier(const NomFichier: string);  
var  
  Fichier: TextFile;
begin
  AssignFile(Fichier, NomFichier);
  Reset(Fichier);
  try
    // Traitement
    if not ConditionValide then
      raise Exception.Create('Condition non valide');

    // Suite du traitement...
  finally
    CloseFile(Fichier);  // Nettoyage garanti, même si exception levée
  end;
end;
```

**Point important :** Ne fermez pas la ressource manuellement avant de lever l'exception : le bloc `finally` s'en chargera automatiquement. Sinon, vous risquez une double fermeture.

## Gestion centralisée des exceptions

Vous pouvez gérer globalement les exceptions non capturées :

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Application.OnException := GererExceptionGlobale;
end;

procedure TForm1.GererExceptionGlobale(Sender: TObject; E: Exception);  
begin  
  // Journaliser
  Log('Exception non gérée : ' + E.Message);

  // Afficher un message personnalisé
  MessageDlg('Une erreur est survenue : ' + E.Message,
             mtError, [mbOK], 0);
end;
```

## Journalisation des exceptions

Il est important de garder une trace des exceptions pour le débogage :

```pascal
procedure LogErreur(const Message: string);  
var  
  Fichier: TextFile;
begin
  AssignFile(Fichier, 'C:\Logs\erreurs.log');
  if FileExists('C:\Logs\erreurs.log') then
    Append(Fichier)
  else
    Rewrite(Fichier);

  try
    WriteLn(Fichier, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Message);
  finally
    CloseFile(Fichier);
  end;
end;

// Utilisation
try
  OperationRisquee;
except
  on E: Exception do
  begin
    LogErreur('Erreur dans OperationRisquee : ' + E.Message);
    ShowMessage('Une erreur est survenue');
  end;
end;
```

## Erreurs courantes à éviter

### Erreur 1 : Capturer sans gérer

```pascal
// ❌ MAUVAIS : cache les problèmes
try
  OperationImportante;
except
  // Rien...
end;
```

### Erreur 2 : Oublier finally pour les ressources

```pascal
// ❌ MAUVAIS : fuite mémoire si exception
Liste := TStringList.Create;  
Liste.LoadFromFile('fichier.txt');  
Liste.Free;  

// ✅ BON
Liste := TStringList.Create;  
try  
  Liste.LoadFromFile('fichier.txt');
finally
  Liste.Free;
end;
```

### Erreur 3 : Ordre incorrect des exceptions

```pascal
// ❌ MAUVAIS : Exception générale en premier
try
  // Code
except
  on E: Exception do
    ShowMessage('Erreur générale');  // Capture TOUT
  on E: EConvertError do
    ShowMessage('Conversion');  // Ne sera jamais atteint !
end;

// ✅ BON : spécifique avant générique
try
  // Code
except
  on E: EConvertError do
    ShowMessage('Conversion');
  on E: Exception do
    ShowMessage('Erreur générale');
end;
```

### Erreur 4 : Re-créer l'exception au lieu de la re-lever

```pascal
// ❌ MAUVAIS : perd la trace de l'exception originale
try
  // Code
except
  on E: Exception do
    raise Exception.Create(E.Message);  // Nouvelle exception
end;

// ✅ BON : conserve la trace
try
  // Code
except
  on E: Exception do
  begin
    Log(E.Message);
    raise;  // Re-lève l'exception originale
  end;
end;
```

### Erreur 5 : Utiliser les exceptions pour le contrôle de flux

```pascal
// ❌ MAUVAIS : exception pour la logique normale
function TrouverUtilisateur(ID: Integer): TUtilisateur;  
begin  
  if not UtilisateurExiste(ID) then
    raise Exception.Create('Utilisateur non trouvé');
  // ...
end;

// ✅ BON : utiliser un booléen ou nil
function TrouverUtilisateur(ID: Integer): TUtilisateur;  
begin  
  if not UtilisateurExiste(ID) then
    Result := nil  // Ou retourner False avec un paramètre out
  else
    // Chercher l'utilisateur
end;
```

## Points clés à retenir

1. **try...except** : capture et gère les exceptions
2. **try...finally** : garantit l'exécution du code de nettoyage
3. **raise** : lève une exception
4. Capturer les exceptions **spécifiques** avant les génériques
5. Toujours **libérer les ressources** dans un bloc finally
6. Utiliser **Try...** fonctions pour éviter les exceptions fréquentes
7. **Ne pas capturer** les exceptions qu'on ne peut pas gérer correctement
8. Fournir des **messages d'erreur clairs** et informatifs
9. **Journaliser** les exceptions pour le débogage
10. Les exceptions sont pour les **situations anormales**, pas la logique normale

---

La gestion des exceptions est essentielle pour créer des applications robustes et professionnelles. Elle permet de prévoir les erreurs, d'informer clairement l'utilisateur et de maintenir l'intégrité de votre application. Dans la section suivante, nous explorerons la programmation orientée objet, qui permettra de structurer encore mieux votre code.

⏭️ [Programmation orientée objet](/03-langage-object-pascal/07-programmation-orientee-objet.md)
