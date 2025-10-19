🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.7 Revue de code et refactoring

## Introduction

Imaginez que vous écrivez un livre. Vous avez terminé votre premier jet, vous êtes content. Mais avant de le publier, vous le faites relire par des amis, un éditeur, un correcteur. Ils trouvent des fautes de frappe, des phrases confuses, des passages à améliorer. Le livre final est bien meilleur grâce à ces relectures.

Le code, c'est pareil. La **revue de code** (code review) et le **refactoring** sont les processus qui transforment du code "qui marche" en code de qualité professionnelle.

### Deux pratiques complémentaires

**Revue de code :** Un autre développeur relit votre code avant qu'il soit fusionné dans le projet principal. C'est une deuxième paire d'yeux qui peut voir ce que vous avez manqué.

**Refactoring :** Améliorer la structure interne du code sans changer son comportement externe. C'est comme réorganiser une maison : l'extérieur reste le même, mais l'intérieur devient plus fonctionnel.

### L'importance de ces pratiques

**Statistiques révélatrices :**
- 60-80% des bugs sont détectés en revue de code
- Le refactoring régulier réduit la dette technique de 40%
- Les équipes qui font des revues de code produisent 15% moins de bugs

**Citation célèbre :**
> "N'importe quel idiot peut écrire du code qu'un ordinateur peut comprendre. Les bons programmeurs écrivent du code que les humains peuvent comprendre."
> — Martin Fowler

## Partie 1 : La revue de code

### Qu'est-ce qu'une revue de code ?

Une revue de code est un processus où un ou plusieurs développeurs examinent le code écrit par un autre développeur avant qu'il soit intégré au projet.

**Le processus typique :**

```
1. Développeur A écrit du code
   ↓
2. Développeur A crée une Pull Request (PR)
   ↓
3. Développeur B relit le code
   ↓
4. Développeur B laisse des commentaires
   ↓
5. Développeur A apporte les corrections
   ↓
6. Développeur B approuve
   ↓
7. Le code est fusionné dans main
```

### Pourquoi faire des revues de code ?

#### 1. Détection des bugs

Un autre développeur voit souvent des problèmes que l'auteur n'a pas vus.

**Exemple réel :**
```pascal
// Code original
function CalculerRemise(Prix: Currency; Pourcentage: Integer): Currency;
begin
  Result := Prix * Pourcentage / 100;
end;

// Reviewer : "Si Pourcentage = 10, tu retournes 10% du prix, pas la remise!"
// Correction nécessaire :
Result := Prix - (Prix * Pourcentage / 100);
```

#### 2. Amélioration de la qualité

Les reviewers suggèrent des améliorations :
- Meilleur nommage
- Code plus clair
- Performances optimisées
- Sécurité renforcée

#### 3. Partage de connaissances

Les juniors apprennent des seniors, et vice versa. Chacun découvre de nouvelles techniques.

**Exemple :**
```pascal
// Code junior
function EstVide(Liste: TStringList): Boolean;
begin
  if Liste.Count = 0 then
    Result := True
  else
    Result := False;
end;

// Reviewer senior : "Tu peux simplifier :"
function EstVide(Liste: TStringList): Boolean;
begin
  Result := Liste.Count = 0;
end;
```

#### 4. Cohérence du code

Les reviewers s'assurent que le code suit les conventions du projet.

#### 5. Responsabilité partagée

Le code n'appartient plus à une seule personne. L'équipe entière en est responsable.

### Types de revues de code

#### Revue formelle (inspection)

Processus structuré avec réunion :
- Auteur présente le code
- Reviewers posent des questions
- Notes et actions décidées collectivement

**Avantages :**
- Très approfondi
- Bon pour le code critique

**Inconvénients :**
- Chronophage
- Peut être intimidant

#### Revue légère (Pull Request)

Revue asynchrone via GitHub/GitLab :
- Pull Request créée
- Reviewers commentent en ligne
- Discussion asynchrone
- Approbation finale

**Avantages :**
- Flexible
- Pas de réunion
- Historique conservé

**Inconvénients :**
- Peut être superficiel
- Communication écrite parfois ambiguë

#### Pair Programming

Deux développeurs travaillent ensemble sur le même code :
- Un écrit (driver)
- L'autre relit en temps réel (navigator)
- Changement de rôle régulier

**Avantages :**
- Revue instantanée
- Transfert de connaissances
- Moins de bugs dès le départ

**Inconvénients :**
- Coûte deux développeurs
- Fatiguant sur la durée

### Comment faire une bonne revue de code

#### Pour l'auteur (celui qui soumet le code)

**1. Préparez le code**

Avant de demander une revue :

```bash
# Vérifiez que tout compile
# Exécutez les tests
# Relisez votre propre code
# Vérifiez le formatage
```

**2. Faites des PR de taille raisonnable**

✅ **Bon :** 200-400 lignes de code
- Facile à relire
- Commentaires pertinents

❌ **Mauvais :** 2000 lignes de code
- Impossible à relire correctement
- Reviewers fatigués = bugs manqués

**Si votre PR est grosse :**
- Découpez en plusieurs PR
- Ou demandez une revue préliminaire

**3. Décrivez clairement vos modifications**

**Mauvaise description :**
```
Mise à jour
```

**Bonne description :**
```markdown
## Objectif
Ajout de la fonctionnalité d'export Excel pour les rapports clients

## Modifications
- Création de la classe `TExcelExporter`
- Ajout du bouton d'export dans le formulaire de rapport
- Implémentation du formatage des cellules (couleurs, bordures)
- Gestion des erreurs d'export

## Tests effectués
- Export de 100 lignes : OK
- Export de 10 000 lignes : OK (5 secondes)
- Gestion du fichier déjà ouvert : OK (message d'erreur approprié)

## Points d'attention
Vérifiez particulièrement la fonction `FormaterCellule()` ligne 245,
j'ai un doute sur la gestion des dates.
```

**4. Répondez aux commentaires constructivement**

```
❌ "Non, mon code est bon"
✅ "Bonne remarque ! Je vais changer ça"

❌ "Tu comprends rien"
✅ "Je ne suis pas sûr de comprendre ton point, peux-tu clarifier ?"

❌ Ignorer les commentaires
✅ Répondre à chaque commentaire, même pour dire "Fait !"
```

**5. Ne le prenez pas personnellement**

La revue critique le CODE, pas VOUS. C'est une opportunité d'apprentissage.

#### Pour le reviewer (celui qui relit)

**1. Checklist de revue**

Voici ce qu'il faut vérifier :

**Fonctionnalité**
- [ ] Le code fait-il ce qu'il est censé faire ?
- [ ] Les cas limites sont-ils gérés ?
- [ ] Les erreurs sont-elles bien gérées ?

**Lisibilité**
- [ ] Le code est-il facile à comprendre ?
- [ ] Les noms de variables sont-ils clairs ?
- [ ] Y a-t-il des commentaires où nécessaire ?

**Architecture**
- [ ] Le code respecte-t-il l'architecture du projet ?
- [ ] La séparation des responsabilités est-elle respectée ?
- [ ] Les dépendances sont-elles appropriées ?

**Performance**
- [ ] Y a-t-il des problèmes de performance évidents ?
- [ ] Les boucles sont-elles optimisées ?
- [ ] Les ressources sont-elles libérées correctement ?

**Sécurité**
- [ ] Les entrées utilisateur sont-elles validées ?
- [ ] Y a-t-il des risques d'injection SQL ?
- [ ] Les mots de passe sont-ils chiffrés ?

**Tests**
- [ ] Le code est-il testable ?
- [ ] Des tests sont-ils présents ?
- [ ] Les tests couvrent-ils les cas importants ?

**Documentation**
- [ ] Les fonctions publiques sont-elles documentées ?
- [ ] Les algorithmes complexes sont-ils expliqués ?
- [ ] Le README est-il à jour si nécessaire ?

**2. Faites des commentaires constructifs**

**❌ Commentaires destructifs :**
```
"Ce code est nul"
"N'importe quoi"
"Tu ne sais pas coder"
"Refais tout"
```

**✅ Commentaires constructifs :**
```
"Cette fonction est complexe. Que penses-tu de la découper en 2 fonctions ?"
"J'ai remarqué que cette boucle est appelée souvent. On pourrait optimiser ?"
"Super implémentation ! Une petite suggestion : on pourrait ajouter un test unitaire ici"
"Je ne suis pas sûr de comprendre cette logique, peux-tu clarifier ?"
```

**Structure d'un bon commentaire :**
1. **Observation** : "J'ai remarqué que..."
2. **Explication** : "Cela pourrait causer..."
3. **Suggestion** : "Que penses-tu de..."

**Exemple :**
```pascal
// Code à revoir
function Calculer(X: Integer): Integer;
begin
  Result := X * 2 + 5 - X div 2;
end;

// ❌ Mauvais commentaire
"Cette fonction est incompréhensible"

// ✅ Bon commentaire
"J'ai du mal à comprendre ce calcul. Peux-tu ajouter un commentaire
expliquant la formule métier ? Ou mieux, découper en plusieurs étapes
avec des variables intermédiaires nommées explicitement ?"
```

**3. Différenciez les catégories de commentaires**

Utilisez des préfixes pour clarifier l'importance :

```
[BLOQUANT] : Doit être corrigé avant merge
[IMPORTANT] : Devrait être corrigé, mais pas bloquant
[SUGGESTION] : Idée d'amélioration, optionnelle
[QUESTION] : Demande de clarification
[NIT] : Détail mineur (nit = nitpicking)
[COMPLIMENT] : Bravo pour cette partie !
```

**Exemples :**
```
[BLOQUANT] Cette fonction ne libère pas la mémoire, risque de fuite

[IMPORTANT] Cette requête SQL devrait utiliser des paramètres pour
éviter les injections

[SUGGESTION] Tu pourrais utiliser une TList<T> au lieu d'un TStringList
pour plus de sécurité de type

[QUESTION] Pourquoi utilises-tu un Sleep(100) ici ?

[NIT] Petite typo dans le commentaire ligne 42

[COMPLIMENT] Excellente gestion des cas limites !
```

**4. Soyez spécifique**

```
❌ "Le code n'est pas bon"
✅ "La fonction `CalculerTotal` ligne 156 ne gère pas le cas où
la liste est vide, ce qui causera une exception"

❌ "Il manque des tests"
✅ "Il faudrait ajouter un test pour vérifier le comportement
quand le montant est négatif"

❌ "C'est lent"
✅ "Cette boucle parcourt la liste 3 fois (lignes 45, 67, 89).
On pourrait tout faire en un seul passage ?"
```

**5. Reconnaissez le bon travail**

```
✅ "Super idée d'avoir extrait cette logique dans une fonction séparée !"
✅ "J'aime beaucoup cette approche, c'est très élégant"
✅ "Excellente gestion des erreurs"
✅ "Code très clair et bien documenté, merci !"
```

**6. Proposez des solutions, pas seulement des problèmes**

```
❌ "Cette fonction est trop longue"

✅ "Cette fonction est longue (150 lignes). Que penses-tu de la découper ainsi :
- ExtractValidation() pour la validation
- ExtractCalculation() pour les calculs
- ExtractSave() pour la sauvegarde
Cela rendrait le code plus testable et plus lisible."
```

**7. Limitez le nombre de commentaires**

Trop de commentaires = reviewer épuisé = auteur découragé

**Règle d'or :** Maximum 15-20 commentaires par PR. Si vous en avez plus, c'est que :
- La PR est trop grosse (demandez de découper)
- Il y a des problèmes structurels (discussion nécessaire)

### Exemple de revue de code complète

**Code soumis :**

```pascal
unit ClientManager;

interface

uses
  System.SysUtils, Data.DB, FireDAC.Comp.Client;

type
  TClientManager = class
  public
    function GetClient(id: Integer): TDataSet;
    procedure Save(name, email: string; premium: Boolean);
  end;

var
  ClientManager: TClientManager;

implementation

function TClientManager.GetClient(id: Integer): TDataSet;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  Query.SQL.Text := 'SELECT * FROM clients WHERE id = ' + IntToStr(id);
  Query.Open;
  Result := Query;
end;

procedure TClientManager.Save(name, email: string; premium: Boolean);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  Query.SQL.Text := 'INSERT INTO clients (name, email, premium) VALUES (' +
    QuotedStr(name) + ', ' + QuotedStr(email) + ', ' +
    IntToStr(Ord(premium)) + ')';
  Query.ExecSQL;
  Query.Free;
end;

end.
```

**Commentaires de revue :**

```
[BLOQUANT] Ligne 25-27 : Injection SQL possible
La requête concatène directement l'ID dans le SQL. Utilise des paramètres :

Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
Query.ParamByName('id').AsInteger := id;

---

[BLOQUANT] Ligne 25, 33 : Fuites mémoire
Les TFDQuery créés ne sont jamais libérés. Utilise try..finally :

Query := TFDQuery.Create(nil);
try
  // ... utilisation
finally
  Query.Free;
end;

---

[IMPORTANT] Ligne 33-36 : Injection SQL aussi ici
Même problème que GetClient. Utilise des paramètres.

---

[IMPORTANT] Ligne 21 : Type de retour inapproprié
Retourner un TDataSet force le code appelant à gérer la mémoire.
Mieux : retourner un objet TClient avec les données copiées.

---

[SUGGESTION] Ligne 30 : Extraction de méthode
La logique de sauvegarde pourrait être dans une méthode privée
ExecuteInsert() pour la réutilisabilité.

---

[SUGGESTION] Ligne 18-19 : Convention de nommage
Les paramètres devraient commencer par une majuscule en Delphi :
- name → Name
- email → Email
- id → ID

---

[NIT] Ligne 38 : Variable globale
ClientManager en variable globale n'est pas une bonne pratique.
Privilégie un singleton ou dependency injection.

---

[QUESTION] Ligne 30
Comment gères-tu le cas où le client existe déjà ?
Faut-il un UPDATE au lieu d'INSERT ?

---

[COMPLIMENT]
Bonne séparation GetClient et Save ! La structure de base est solide.
```

**Code révisé :**

```pascal
unit ClientManager;

interface

uses
  System.SysUtils, Data.DB, FireDAC.Comp.Client;

type
  /// <summary>
  ///   Gestionnaire de clients avec accès base de données sécurisé
  /// </summary>
  TClient = record
    ID: Integer;
    Name: string;
    Email: string;
    Premium: Boolean;
  end;

  TClientManager = class
  private
    FConnection: TFDConnection;
    function CreateQuery: TFDQuery;
  public
    constructor Create(Connection: TFDConnection);

    /// <summary>
    ///   Récupère un client par son ID
    /// </summary>
    /// <returns>
    ///   Structure TClient remplie, ou exception si non trouvé
    /// </returns>
    function GetClient(ID: Integer): TClient;

    /// <summary>
    ///   Sauvegarde un nouveau client ou met à jour s'il existe
    /// </summary>
    procedure SaveClient(const Client: TClient);
  end;

implementation

constructor TClientManager.Create(Connection: TFDConnection);
begin
  inherited Create;
  FConnection := Connection;
end;

function TClientManager.CreateQuery: TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

function TClientManager.GetClient(ID: Integer): TClient;
var
  Query: TFDQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if Query.IsEmpty then
      raise Exception.CreateFmt('Client %d non trouvé', [ID]);

    Result.ID := Query.FieldByName('id').AsInteger;
    Result.Name := Query.FieldByName('name').AsString;
    Result.Email := Query.FieldByName('email').AsString;
    Result.Premium := Query.FieldByName('premium').AsBoolean;
  finally
    Query.Free;
  end;
end;

procedure TClientManager.SaveClient(const Client: TClient);
var
  Query: TFDQuery;
begin
  Query := CreateQuery;
  try
    // Vérifier si le client existe
    Query.SQL.Text := 'SELECT COUNT(*) FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := Client.ID;
    Query.Open;

    if Query.Fields[0].AsInteger > 0 then
    begin
      // Mise à jour
      Query.Close;
      Query.SQL.Text := 'UPDATE clients SET name = :name, email = :email, ' +
        'premium = :premium WHERE id = :id';
    end
    else
    begin
      // Insertion
      Query.Close;
      Query.SQL.Text := 'INSERT INTO clients (name, email, premium) ' +
        'VALUES (:name, :email, :premium)';
    end;

    Query.ParamByName('name').AsString := Client.Name;
    Query.ParamByName('email').AsString := Client.Email;
    Query.ParamByName('premium').AsBoolean := Client.Premium;
    if Client.ID > 0 then
      Query.ParamByName('id').AsInteger := Client.ID;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

**Réponse de l'auteur :**

```
Merci pour la revue détaillée !

[BLOQUANT] SQL injection : Corrigé, j'utilise maintenant des paramètres partout
[BLOQUANT] Fuites mémoire : Corrigé avec try..finally
[IMPORTANT] Type retour : Excellente idée ! J'ai créé un record TClient
[IMPORTANT] UPDATE vs INSERT : Bonne question ! J'ai ajouté la logique
[SUGGESTION] Extraction : J'ai créé CreateQuery() comme méthode helper
[SUGGESTION] Conventions : Corrigé
[NIT] Variable globale : J'ai ajouté un constructeur avec dependency injection
[QUESTION] : Répondu avec le code UPDATE/INSERT

Tous les tests passent. Prêt pour un second regard ?
```

## Partie 2 : Le refactoring

### Qu'est-ce que le refactoring ?

**Définition formelle :** Modifier la structure interne du code pour le rendre plus propre, plus maintenable, sans changer son comportement externe.

**Analogie :** Réorganiser une bibliothèque. Les livres (fonctionnalités) restent les mêmes, mais ils sont mieux rangés, plus faciles à trouver.

**Ce que le refactoring N'EST PAS :**
- ❌ Ajouter des fonctionnalités
- ❌ Corriger des bugs
- ❌ Changer le comportement visible

**Ce que le refactoring EST :**
- ✅ Renommer pour plus de clarté
- ✅ Extraire des fonctions
- ✅ Simplifier la logique
- ✅ Éliminer la duplication
- ✅ Améliorer la structure

### Pourquoi refactoriser ?

#### Le concept de dette technique

Imaginez que vous construisez une maison rapidement avec des raccourcis. Au début, ça va. Mais avec le temps :
- Les fondations bougent
- Les murs se fissurent
- Les réparations deviennent difficiles

La **dette technique**, c'est pareil :
- Au début, le code "qui marche" suffit
- Avec le temps, il devient difficile à modifier
- Chaque changement prend de plus en plus de temps

**Le refactoring, c'est rembourser cette dette.**

#### Les signes qu'il faut refactoriser

- Vous avez peur de toucher au code
- Corriger un bug en crée deux autres
- Ajouter une fonctionnalité prend des jours
- Vous ne comprenez plus votre propre code
- Le code est copié-collé partout
- Les fonctions font plus de 50 lignes
- Les classes ont plus de 500 lignes
- Les tests sont impossibles à écrire

### Les "Code Smells" (mauvaises odeurs)

Les "code smells" sont des signes que le code a besoin de refactoring.

#### 1. Duplication de code

**Le problème :**
```pascal
// Dans FormClient
procedure TFormClient.ButtonSaveClick(Sender: TObject);
begin
  if EditNom.Text = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Exit;
  end;
  if not EditEmail.Text.Contains('@') then
  begin
    ShowMessage('Email invalide');
    Exit;
  end;
  // Sauvegarde...
end;

// Dans FormFournisseur (même code dupliqué !)
procedure TFormFournisseur.ButtonSaveClick(Sender: TObject);
begin
  if EditNom.Text = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Exit;
  end;
  if not EditEmail.Text.Contains('@') then
  begin
    ShowMessage('Email invalide');
    Exit;
  end;
  // Sauvegarde...
end;
```

**La solution (Extract Method) :**
```pascal
unit ValidationHelper;

interface

type
  TValidationHelper = class
  public
    class function ValiderNom(const Nom: string; out Erreur: string): Boolean;
    class function ValiderEmail(const Email: string; out Erreur: string): Boolean;
  end;

implementation

class function TValidationHelper.ValiderNom(const Nom: string; out Erreur: string): Boolean;
begin
  if Trim(Nom) = '' then
  begin
    Erreur := 'Le nom est obligatoire';
    Exit(False);
  end;
  Result := True;
end;

class function TValidationHelper.ValiderEmail(const Email: string; out Erreur: string): Boolean;
begin
  if not Email.Contains('@') then
  begin
    Erreur := 'Email invalide';
    Exit(False);
  end;
  Result := True;
end;

end.

// Utilisation
procedure TFormClient.ButtonSaveClick(Sender: TObject);
var
  Erreur: string;
begin
  if not TValidationHelper.ValiderNom(EditNom.Text, Erreur) then
  begin
    ShowMessage(Erreur);
    Exit;
  end;

  if not TValidationHelper.ValiderEmail(EditEmail.Text, Erreur) then
  begin
    ShowMessage(Erreur);
    Exit;
  end;

  // Sauvegarde...
end;
```

#### 2. Fonctions trop longues

**Le problème :**
```pascal
procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  Total: Currency;
  Client: TClient;
  Erreur: string;
begin
  // 200 lignes de code faisant :
  // - Validation des champs
  // - Chargement du client
  // - Calcul du total
  // - Application des remises
  // - Vérification du stock
  // - Génération de facture
  // - Envoi d'email
  // - Mise à jour de l'affichage
  // ...
end;
```

**La solution (Extract Method) :**
```pascal
procedure TFormCommande.ButtonValiderClick(Sender: TObject);
begin
  if not ValiderFormulaire then
    Exit;

  if not VerifierStock then
  begin
    ShowMessage('Stock insuffisant');
    Exit;
  end;

  EnregistrerCommande;
  GenererFacture;
  EnvoyerEmailConfirmation;
  MettreAJourAffichage;

  ShowMessage('Commande validée avec succès !');
end;

function TFormCommande.ValiderFormulaire: Boolean;
begin
  // Logique de validation
  Result := True;
end;

function TFormCommande.VerifierStock: Boolean;
begin
  // Vérification du stock
  Result := True;
end;

procedure TFormCommande.EnregistrerCommande;
begin
  // Enregistrement
end;

// etc.
```

#### 3. Listes de paramètres trop longues

**Le problème :**
```pascal
procedure CreerClient(Nom, Prenom, Email, Telephone, Adresse, Ville,
  CodePostal, Pays: string; EstPremium: Boolean; DateNaissance: TDateTime;
  Profession, Commentaire: string);
```

**La solution (Introduce Parameter Object) :**
```pascal
type
  TClientData = record
    Nom: string;
    Prenom: string;
    Email: string;
    Telephone: string;
    Adresse: string;
    Ville: string;
    CodePostal: string;
    Pays: string;
    EstPremium: Boolean;
    DateNaissance: TDateTime;
    Profession: string;
    Commentaire: string;
  end;

procedure CreerClient(const Data: TClientData);
```

#### 4. Classe tentaculaire (God Object)

**Le problème :**
```pascal
type
  TApplicationManager = class
  public
    // 50 méthodes qui font tout
    procedure GererClients;
    procedure GererCommandes;
    procedure GererFactures;
    procedure GererStock;
    procedure GererUtilisateurs;
    procedure GererRapports;
    procedure GererEmails;
    procedure GererSauvegardes;
    // ... 42 autres méthodes
  end;
```

**La solution (Extract Class) :**
```pascal
type
  TClientManager = class
    // Gestion des clients uniquement
  end;

  TCommandeManager = class
    // Gestion des commandes uniquement
  end;

  TFactureManager = class
    // Gestion des factures uniquement
  end;

  // etc.
```

#### 5. Commentaires excessifs

**Le problème :**
```pascal
// Déclarer une variable pour le total
var Total: Currency;
// Initialiser le total à zéro
Total := 0;
// Boucler sur chaque ligne
for I := 0 to Liste.Count - 1 do
begin
  // Ajouter le prix au total
  Total := Total + Liste[I].Prix;
end;
```

**La solution (Rename pour plus de clarté) :**
```pascal
function CalculerTotal: Currency;
var
  MontantTotal: Currency;
  Ligne: TLigneCommande;
begin
  MontantTotal := 0;

  for Ligne in Lignes do
    MontantTotal := MontantTotal + Ligne.Prix;

  Result := MontantTotal;
end;
```

#### 6. Variable temporaire inutile

**Le problème :**
```pascal
function ObtenirNomComplet: string;
var
  Resultat: string;
begin
  Resultat := FPrenom + ' ' + FNom;
  Result := Resultat;
end;
```

**La solution (Inline Temp) :**
```pascal
function ObtenirNomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;
```

#### 7. Conditions complexes

**Le problème :**
```pascal
if (Client.Age >= 18) and (Client.Age <= 65) and
   (Client.Solde > 1000) and (not Client.EstBloque) and
   ((Client.Type = ctPremium) or (Client.AncienneteAnnees > 5)) then
begin
  // Autoriser l'opération
end;
```

**La solution (Extract Method) :**
```pascal
function ClientPeutEffectuerOperation(Client: TClient): Boolean;
begin
  Result := EstDansTrancheAge(Client) and
            ADuCredit(Client) and
            EstAutorise(Client) and
            EstClientFidele(Client);
end;

function EstDansTrancheAge(Client: TClient): Boolean;
begin
  Result := (Client.Age >= 18) and (Client.Age <= 65);
end;

function ADuCredit(Client: TClient): Boolean;
begin
  Result := Client.Solde > 1000;
end;

function EstAutorise(Client: TClient): Boolean;
begin
  Result := not Client.EstBloque;
end;

function EstClientFidele(Client: TClient): Boolean;
begin
  Result := (Client.Type = ctPremium) or (Client.AncienneteAnnees > 5);
end;

// Utilisation
if ClientPeutEffectuerOperation(Client) then
begin
  // Autoriser l'opération
end;
```

### Techniques de refactoring courantes

#### 1. Rename (Renommer)

Donner des noms plus explicites.

**Avant :**
```pascal
var
  d: TDateTime;
  amt: Currency;
  calc: Boolean;
```

**Après :**
```pascal
var
  DateCommande: TDateTime;
  MontantTotal: Currency;
  DoitRecalculer: Boolean;
```

#### 2. Extract Method (Extraire une méthode)

Prendre un morceau de code et en faire une fonction.

**Avant :**
```pascal
procedure Afficher;
begin
  // Calcul complexe sur 20 lignes
  X := A * B + C / D - E;
  Y := X * 2 + F;
  Z := Y - G * H;
  Result := Z / I;
  // ...

  ShowMessage(FloatToStr(Result));
end;
```

**Après :**
```pascal
procedure Afficher;
var
  Resultat: Double;
begin
  Resultat := CalculerFormuleComplexe(A, B, C, D, E, F, G, H, I);
  ShowMessage(FloatToStr(Resultat));
end;

function CalculerFormuleComplexe(A, B, C, D, E, F, G, H, I: Double): Double;
var
  X, Y, Z: Double;
begin
  X := A * B + C / D - E;
  Y := X * 2 + F;
  Z := Y - G * H;
  Result := Z / I;
end;
```

#### 3. Inline Method (Intégrer une méthode)

L'inverse : si une méthode est trop simple, l'intégrer dans l'appelant.

**Avant :**
```pascal
function EstMajeur(Age: Integer): Boolean;
begin
  Result := Age >= 18;
end;

procedure Verifier;
begin
  if EstMajeur(Client.Age) then
    // ...
end;
```

**Après :**
```pascal
procedure Verifier;
begin
  if Client.Age >= 18 then
    // ...
end;
```

#### 4. Extract Class (Extraire une classe)

Quand une classe fait trop de choses, en extraire une partie.

**Avant :**
```pascal
type
  TClient = class
  private
    FNom: string;
    FEmail: string;
    FRue: string;
    FVille: string;
    FCodePostal: string;
    FPays: string;
  public
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
    property Rue: string read FRue write FRue;
    property Ville: string read FVille write FVille;
    property CodePostal: string read FCodePostal write FCodePostal;
    property Pays: string read FPays write FPays;
  end;
```

**Après :**
```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
    FPays: string;
  public
    property Rue: string read FRue write FRue;
    property Ville: string read FVille write FVille;
    property CodePostal: string read FCodePostal write FCodePostal;
    property Pays: string read FPays write FPays;
    function ToString: string;
  end;

  TClient = class
  private
    FNom: string;
    FEmail: string;
    FAdresse: TAdresse;
  public
    constructor Create;
    destructor Destroy; override;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
    property Adresse: TAdresse read FAdresse;
  end;
```

#### 5. Introduce Variable (Introduire une variable)

Donner un nom à une expression complexe.

**Avant :**
```pascal
if (Client.Commandes.Count > 10) and
   (Client.Commandes.Total > 5000) and
   (YearsBetween(Now, Client.DateInscription) > 2) then
begin
  // Appliquer remise fidélité
end;
```

**Après :**
```pascal
var
  EstClientActif: Boolean;
  ATotalEleve: Boolean;
  EstAncien: Boolean;
  MeriteRemiseFidelite: Boolean;
begin
  EstClientActif := Client.Commandes.Count > 10;
  ATotalEleve := Client.Commandes.Total > 5000;
  EstAncien := YearsBetween(Now, Client.DateInscription) > 2;

  MeriteRemiseFidelite := EstClientActif and ATotalEleve and EstAncien;

  if MeriteRemiseFidelite then
  begin
    // Appliquer remise fidélité
  end;
end;
```

#### 6. Replace Magic Number with Constant

Remplacer les nombres "magiques" par des constantes nommées.

**Avant :**
```pascal
procedure CalculerRemise;
begin
  if Montant > 1000 then
    Remise := Montant * 0.1
  else if Montant > 500 then
    Remise := Montant * 0.05
  else
    Remise := 0;

  if Client.AncienneteJours > 365 then
    Remise := Remise * 1.5;
end;
```

**Après :**
```pascal
const
  SEUIL_REMISE_ELEVEE = 1000;
  SEUIL_REMISE_STANDARD = 500;
  TAUX_REMISE_ELEVEE = 0.10;  // 10%
  TAUX_REMISE_STANDARD = 0.05; // 5%
  BONUS_FIDELITE = 1.5;
  JOURS_PAR_AN = 365;

procedure CalculerRemise;
begin
  if Montant > SEUIL_REMISE_ELEVEE then
    Remise := Montant * TAUX_REMISE_ELEVEE
  else if Montant > SEUIL_REMISE_STANDARD then
    Remise := Montant * TAUX_REMISE_STANDARD
  else
    Remise := 0;

  if Client.AncienneteJours > JOURS_PAR_AN then
    Remise := Remise * BONUS_FIDELITE;
end;
```

#### 7. Simplify Conditional (Simplifier condition)

**Avant :**
```pascal
if not (Client.EstBloque) then
begin
  if Client.Solde > 0 then
    Autoriser
  else
    Refuser;
end
else
  Refuser;
```

**Après :**
```pascal
if Client.EstBloque or (Client.Solde <= 0) then
  Refuser
else
  Autoriser;
```

### Processus de refactoring

#### 1. Identifiez le problème

Utilisez la checklist des code smells.

#### 2. Écrivez ou vérifiez les tests

**IMPORTANT :** Avant de refactoriser, assurez-vous d'avoir des tests !

```pascal
// Test avant refactoring
procedure TestCalculerRemise;
begin
  Assert(CalculerRemise(800) = 40, 'Remise 5% sur 800');
  Assert(CalculerRemise(1500) = 150, 'Remise 10% sur 1500');
  Assert(CalculerRemise(100) = 0, 'Pas de remise sous 500');
end;
```

#### 3. Refactorisez par petites étapes

**❌ Mauvaise approche :**
```
Tout refactoriser d'un coup pendant 3 jours
```

**✅ Bonne approche :**
```
Petit refactoring → Tests → Commit
Petit refactoring → Tests → Commit
Petit refactoring → Tests → Commit
```

#### 4. Testez après chaque étape

Lancez les tests après CHAQUE modification.

#### 5. Commitez fréquemment

```bash
git commit -m "refactor: Extract method CalculerRemise"
git commit -m "refactor: Rename variable X en MontantTotal"
git commit -m "refactor: Extract class TAdresse"
```

### Exemple complet de refactoring

**Code initial (avec problèmes) :**

```pascal
unit GestionCommandes;

interface

type
  TFormCommande = class(TForm)
    ButtonValider: TButton;
    EditClient: TEdit;
    EditProduit: TEdit;
    EditQuantite: TEdit;
    EditPrix: TEdit;
    procedure ButtonValiderClick(Sender: TObject);
  end;

implementation

procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  c, p, q: Integer;
  pr: Double;
  t: Double;
  r: Double;
  Query: TFDQuery;
begin
  // Validation
  c := StrToIntDef(EditClient.Text, 0);
  if c = 0 then
  begin
    ShowMessage('Client invalide');
    Exit;
  end;

  p := StrToIntDef(EditProduit.Text, 0);
  if p = 0 then
  begin
    ShowMessage('Produit invalide');
    Exit;
  end;

  q := StrToIntDef(EditQuantite.Text, 0);
  if q <= 0 then
  begin
    ShowMessage('Quantité invalide');
    Exit;
  end;

  pr := StrToFloatDef(EditPrix.Text, 0);
  if pr <= 0 then
  begin
    ShowMessage('Prix invalide');
    Exit;
  end;

  // Calculs
  t := pr * q;

  // Remise
  if t > 1000 then
    r := t * 0.1
  else if t > 500 then
    r := t * 0.05
  else
    r := 0;

  t := t - r;

  // TVA
  t := t * 1.2;

  // Sauvegarde
  Query := TFDQuery.Create(nil);
  Query.SQL.Text := 'INSERT INTO commandes (client_id, produit_id, quantite, ' +
    'prix, total) VALUES (' + IntToStr(c) + ', ' + IntToStr(p) + ', ' +
    IntToStr(q) + ', ' + FloatToStr(pr) + ', ' + FloatToStr(t) + ')';
  Query.ExecSQL;
  Query.Free;

  ShowMessage('Commande enregistrée !');
end;

end.
```

**Problèmes identifiés :**
1. ❌ Noms de variables non explicites (c, p, q, pr, t, r)
2. ❌ Tout dans un seul bouton (150 lignes)
3. ❌ Validation mélangée avec logique métier
4. ❌ Nombres magiques (1000, 500, 0.1, 0.05, 1.2)
5. ❌ Injection SQL
6. ❌ Fuite mémoire (Query pas en try..finally)
7. ❌ Pas testable

**Étape 1 : Rename (Renommer les variables)**

```pascal
procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  ClientID, ProduitID, Quantite: Integer;
  PrixUnitaire: Double;
  Total: Double;
  Remise: Double;
  Query: TFDQuery;
begin
  // Validation
  ClientID := StrToIntDef(EditClient.Text, 0);
  if ClientID = 0 then
  begin
    ShowMessage('Client invalide');
    Exit;
  end;

  // ... suite identique avec nouveaux noms
end;
```

**Étape 2 : Extract Method (Validation)**

```pascal
function ValiderFormulaire(out ClientID, ProduitID, Quantite: Integer;
  out PrixUnitaire: Double; out MessageErreur: string): Boolean;
begin
  Result := False;

  ClientID := StrToIntDef(EditClient.Text, 0);
  if ClientID = 0 then
  begin
    MessageErreur := 'Client invalide';
    Exit;
  end;

  ProduitID := StrToIntDef(EditProduit.Text, 0);
  if ProduitID = 0 then
  begin
    MessageErreur := 'Produit invalide';
    Exit;
  end;

  Quantite := StrToIntDef(EditQuantite.Text, 0);
  if Quantite <= 0 then
  begin
    MessageErreur := 'Quantité invalide';
    Exit;
  end;

  PrixUnitaire := StrToFloatDef(EditPrix.Text, 0);
  if PrixUnitaire <= 0 then
  begin
    MessageErreur := 'Prix invalide';
    Exit;
  end;

  Result := True;
end;

procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  ClientID, ProduitID, Quantite: Integer;
  PrixUnitaire: Double;
  MessageErreur: string;
begin
  if not ValiderFormulaire(ClientID, ProduitID, Quantite, PrixUnitaire, MessageErreur) then
  begin
    ShowMessage(MessageErreur);
    Exit;
  end;

  // Suite...
end;
```

**Étape 3 : Extract Method (Calculs)**

```pascal
const
  SEUIL_REMISE_ELEVEE = 1000;
  SEUIL_REMISE_STANDARD = 500;
  TAUX_REMISE_ELEVEE = 0.10;
  TAUX_REMISE_STANDARD = 0.05;
  TAUX_TVA = 0.20;

function CalculerRemise(MontantHT: Currency): Currency;
begin
  if MontantHT > SEUIL_REMISE_ELEVEE then
    Result := MontantHT * TAUX_REMISE_ELEVEE
  else if MontantHT > SEUIL_REMISE_STANDARD then
    Result := MontantHT * TAUX_REMISE_STANDARD
  else
    Result := 0;
end;

function CalculerTotalTTC(PrixUnitaire: Currency; Quantite: Integer): Currency;
var
  MontantHT, Remise, MontantAvecRemise: Currency;
begin
  MontantHT := PrixUnitaire * Quantite;
  Remise := CalculerRemise(MontantHT);
  MontantAvecRemise := MontantHT - Remise;
  Result := MontantAvecRemise * (1 + TAUX_TVA);
end;
```

**Étape 4 : Extract Class (Séparer la logique)**

```pascal
unit CommandeManager;

interface

type
  TCommandeData = record
    ClientID: Integer;
    ProduitID: Integer;
    Quantite: Integer;
    PrixUnitaire: Currency;
  end;

  TCommandeManager = class
  private
    FConnection: TFDConnection;
    function CalculerRemise(MontantHT: Currency): Currency;
    function CalculerTotalTTC(const Data: TCommandeData): Currency;
  public
    constructor Create(Connection: TFDConnection);
    function EnregistrerCommande(const Data: TCommandeData): Boolean;
  end;

implementation

constructor TCommandeManager.Create(Connection: TFDConnection);
begin
  inherited Create;
  FConnection := Connection;
end;

function TCommandeManager.CalculerRemise(MontantHT: Currency): Currency;
const
  SEUIL_REMISE_ELEVEE = 1000;
  SEUIL_REMISE_STANDARD = 500;
  TAUX_REMISE_ELEVEE = 0.10;
  TAUX_REMISE_STANDARD = 0.05;
begin
  if MontantHT > SEUIL_REMISE_ELEVEE then
    Result := MontantHT * TAUX_REMISE_ELEVEE
  else if MontantHT > SEUIL_REMISE_STANDARD then
    Result := MontantHT * TAUX_REMISE_STANDARD
  else
    Result := 0;
end;

function TCommandeManager.CalculerTotalTTC(const Data: TCommandeData): Currency;
const
  TAUX_TVA = 0.20;
var
  MontantHT, Remise, MontantAvecRemise: Currency;
begin
  MontantHT := Data.PrixUnitaire * Data.Quantite;
  Remise := CalculerRemise(MontantHT);
  MontantAvecRemise := MontantHT - Remise;
  Result := MontantAvecRemise * (1 + TAUX_TVA);
end;

function TCommandeManager.EnregistrerCommande(const Data: TCommandeData): Boolean;
var
  Query: TFDQuery;
  Total: Currency;
begin
  Result := False;
  Total := CalculerTotalTTC(Data);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'INSERT INTO commandes (client_id, produit_id, ' +
      'quantite, prix, total) VALUES (:client, :produit, :quantite, :prix, :total)';
    Query.ParamByName('client').AsInteger := Data.ClientID;
    Query.ParamByName('produit').AsInteger := Data.ProduitID;
    Query.ParamByName('quantite').AsInteger := Data.Quantite;
    Query.ParamByName('prix').AsCurrency := Data.PrixUnitaire;
    Query.ParamByName('total').AsCurrency := Total;
    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

end.
```

**Code final du formulaire (propre et simple) :**

```pascal
unit GestionCommandes;

interface

type
  TFormCommande = class(TForm)
    ButtonValider: TButton;
    EditClient: TEdit;
    EditProduit: TEdit;
    EditQuantite: TEdit;
    EditPrix: TEdit;
    procedure ButtonValiderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCommandeManager: TCommandeManager;
    function ValiderFormulaire(out Data: TCommandeData;
      out MessageErreur: string): Boolean;
  end;

implementation

uses
  CommandeManager;

procedure TFormCommande.FormCreate(Sender: TObject);
begin
  FCommandeManager := TCommandeManager.Create(dmMain.Connection);
end;

procedure TFormCommande.FormDestroy(Sender: TObject);
begin
  FCommandeManager.Free;
end;

function TFormCommande.ValiderFormulaire(out Data: TCommandeData;
  out MessageErreur: string): Boolean;
begin
  Result := False;

  Data.ClientID := StrToIntDef(EditClient.Text, 0);
  if Data.ClientID = 0 then
  begin
    MessageErreur := 'Client invalide';
    Exit;
  end;

  Data.ProduitID := StrToIntDef(EditProduit.Text, 0);
  if Data.ProduitID = 0 then
  begin
    MessageErreur := 'Produit invalide';
    Exit;
  end;

  Data.Quantite := StrToIntDef(EditQuantite.Text, 0);
  if Data.Quantite <= 0 then
  begin
    MessageErreur := 'Quantité invalide';
    Exit;
  end;

  Data.PrixUnitaire := StrToFloatDef(EditPrix.Text, 0);
  if Data.PrixUnitaire <= 0 then
  begin
    MessageErreur := 'Prix invalide';
    Exit;
  end;

  Result := True;
end;

procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  Data: TCommandeData;
  MessageErreur: string;
begin
  if not ValiderFormulaire(Data, MessageErreur) then
  begin
    ShowMessage(MessageErreur);
    Exit;
  end;

  if FCommandeManager.EnregistrerCommande(Data) then
    ShowMessage('Commande enregistrée avec succès !')
  else
    ShowMessage('Erreur lors de l''enregistrement');
end;

end.
```

**Bénéfices du refactoring :**
- ✅ Code lisible et compréhensible
- ✅ Facilement testable (TCommandeManager)
- ✅ Pas d'injection SQL
- ✅ Pas de fuite mémoire
- ✅ Réutilisable
- ✅ Maintenable
- ✅ Séparation UI / logique

## Outils et techniques

### IDE Delphi

**Refactoring intégré :**
- Rename (Ctrl+Shift+E)
- Extract Method
- Declare Variable
- Find References
- Safe Delete

### Outils externes

**1. Formatter/Linter**
- **DelphiLint** : Analyse statique du code
- **Formatter** : Formatage automatique

**2. Analyseurs de code**
- **Pascal Analyzer** : Détecte les problèmes de qualité
- **FixInsight** : Suggestions d'amélioration

**3. Metrics**
- **Project Metrics** : Mesure la complexité

### Tests automatisés

Les tests sont essentiels pour refactoriser en toute sécurité.

```pascal
// Tests unitaires avec DUnitX
procedure TestCalculerRemise;
begin
  Assert.AreEqual(0, CalculerRemise(100), 'Pas de remise sous 500');
  Assert.AreEqual(25, CalculerRemise(500), 'Remise 5% sur 500');
  Assert.AreEqual(150, CalculerRemise(1500), 'Remise 10% sur 1500');
end;
```

## Bonnes pratiques

### 1. La règle du Boy Scout

> "Laissez le code plus propre que vous l'avez trouvé"

À chaque fois que vous touchez du code, améliorez-le un peu.

### 2. Refactorisez en continu

Ne attendez pas d'avoir 10 000 lignes de dette technique.

**Mauvaise approche :**
```
Coder pendant 6 mois → Grande phase de refactoring
```

**Bonne approche :**
```
Coder → Mini refactoring → Coder → Mini refactoring
```

### 3. Utilisez les tests

**Règle d'or :** Ne refactorisez jamais sans tests.

```
Tests existants → Refactoring → Tests passent
```

### 4. Une modification à la fois

```
❌ Renommer + extraire méthode + changer logique

✅ Renommer → Tests → Commit
✅ Extraire méthode → Tests → Commit
✅ Changer logique → Tests → Commit
```

### 5. Documentez pourquoi

```pascal
// REFACTORING: Extraction de la validation dans une fonction séparée
// pour améliorer la testabilité et la réutilisabilité
function ValiderDonnees(...): Boolean;
```

### 6. Revue de code après refactoring

Faites relire vos refactorings, surtout les gros.

### 7. Mesurez l'impact

**Avant :**
- Complexité cyclomatique : 15
- Lignes de code : 250
- Tests : 0

**Après :**
- Complexité cyclomatique : 5
- Lignes de code : 180
- Tests : 15

### 8. Ne sur-refactorisez pas

Parfois, "assez bon" est suffisant. Ne cherchez pas la perfection absolue.

## Quand NE PAS refactoriser

### 1. Code qui fonctionne et ne change jamais

Si c'est du code stable que personne ne touche, laissez-le.

### 2. Code qui sera supprimé bientôt

Pas la peine de refactoriser ce qui va disparaître.

### 3. Sous pression de deadline

Refactorisez APRÈS la deadline, pas avant.

### 4. Sans tests

Si vous n'avez pas de tests, écrivez-en d'abord.

### 5. Code dont vous ne comprenez pas la logique

Comprenez d'abord, refactorisez ensuite.

## Checklist de revue et refactoring

**Avant de merger du code :**

### Revue de code
- [ ] Le code compile sans warning
- [ ] Les tests passent
- [ ] La PR est de taille raisonnable (< 400 lignes)
- [ ] Les fonctions sont courtes (< 50 lignes)
- [ ] Les noms sont explicites
- [ ] Pas de duplication
- [ ] Pas d'injection SQL
- [ ] Les ressources sont libérées
- [ ] La documentation est à jour
- [ ] Les commentaires TODO sont justifiés

### Refactoring
- [ ] Les nombres magiques sont remplacés par des constantes
- [ ] Les fonctions longues sont découpées
- [ ] Les conditions complexes sont simplifiées
- [ ] La duplication est éliminée
- [ ] Les classes trop grosses sont découpées
- [ ] Les variables temporaires inutiles sont supprimées
- [ ] Les tests couvrent le code refactorisé

## Conclusion

La revue de code et le refactoring sont deux piliers de la qualité logicielle professionnelle.

**Revue de code :**
- Détecte les bugs tôt
- Améliore la qualité
- Partage les connaissances
- Crée une culture d'équipe

**Refactoring :**
- Maintient le code propre
- Rembourse la dette technique
- Facilite l'évolution
- Rend le code testable

**Points clés à retenir :**

1. **Faites des revues systématiques** - Tout code doit être relu
2. **Soyez constructif** - Critiquez le code, pas la personne
3. **Refactorisez en continu** - Petits changements réguliers
4. **Testez toujours** - Pas de refactoring sans tests
5. **Commitez fréquemment** - Petites étapes sécurisées
6. **Utilisez les outils** - IDE, analyseurs, formatters
7. **Apprenez des autres** - Chaque revue est une opportunité
8. **Ne sur-optimisez pas** - "Assez bon" est souvent suffisant

**Citations inspirantes :**

> "Le code est lu 10 fois plus souvent qu'il n'est écrit"
> — Robert C. Martin

> "Tout le monde peut écrire du code qu'un ordinateur comprend. Les bons développeurs écrivent du code que les humains comprennent"
> — Martin Fowler

> "Rendez-le d'abord fonctionnel, puis rendez-le beau, puis rendez-le rapide (si nécessaire)"
> — Kent Beck

La revue de code et le refactoring ne sont pas du temps perdu. C'est un investissement qui vous fera gagner énormément de temps à moyen et long terme. Un code de qualité est un plaisir à maintenir. Un code chaotique est un enfer quotidien.

Commencez dès aujourd'hui :
- Demandez une revue de code pour votre prochaine PR
- Refactorisez une petite fonction qui vous dérange
- Apprenez une nouvelle technique de refactoring

Votre futur vous remerciera !

---

**Prochaine étape :** Dans la section suivante, nous explorerons l'intégration continue et le déploiement continu (CI/CD), pour automatiser la qualité et le déploiement de vos applications Delphi.

⏭️ [Intégration avec Git et CI/CD](/18-architecture-et-bonnes-pratiques/08-integration-avec-git-et-ci-cd.md)
