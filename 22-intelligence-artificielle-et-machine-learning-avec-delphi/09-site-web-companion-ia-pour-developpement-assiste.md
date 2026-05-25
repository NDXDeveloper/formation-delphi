🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.9 RAD AI Companion et Smart CodeInsight : l'IA dans RAD Studio

## Deux outils complémentaires

Delphi 13 Florence propose **deux outils IA distincts mais complémentaires** :

### 🌐 RAD AI Companion (web)
- **Chatbot IA web** spécifiquement entraîné sur le contenu RAD Studio
- Accessible via le menu **Aide → RAD AI Companion**
- Idéal pour : questions de documentation, génération d'exemples de code, suggestions de correction
- Documentation : [embarcadero.com/RADAICompanion](https://www.embarcadero.com/RADAICompanion)

### 🛠️ Smart CodeInsight (IDE)
- **Intégration IA dans l'éditeur** de l'IDE (présente depuis RAD Studio 12.3, améliorée en 13)
- Panneau de chat dockable + commandes contextuelles dans l'éditeur (clic droit)
- Support multi-LLM : **OpenAI, Gemini, Claude (en ligne) et Ollama (hors ligne)**
- Idéal pour : refactoring, ajout de commentaires, génération de tests, analyse de sécurité, optimisation
- Documentation : [docwiki.embarcadero.com/.../Smart_CodeInsight](https://docwiki.embarcadero.com/RADStudio/Florence/en/Smart_CodeInsight)

> ℹ️ Cette section présente les deux outils. Les sous-sections distinguent les fonctionnalités web (RAD AI Companion) des fonctionnalités IDE (Smart CodeInsight).

## Introduction au RAD AI Companion

### Qu'est-ce que le RAD AI Companion ?

Le **RAD AI Companion** de Delphi 13 Florence est un chatbot IA web accessible directement depuis l'IDE. Spécifiquement entraîné sur le contenu RAD Studio, il vous aide à coder plus rapidement, à résoudre des problèmes, et à apprendre Delphi de manière interactive.

**Analogie simple** : Imaginez avoir un expert Delphi expérimenté assis à côté de vous, disponible 24/7, qui peut répondre instantanément à toutes vos questions, générer du code, corriger des erreurs, et vous expliquer des concepts complexes. C'est exactement ce que fait le RAD AI Companion.

### Pourquoi c'est révolutionnaire

**Avant le RAD AI Companion** :
- Recherche sur Google/Stack Overflow
- Consultation de documentation (parfois obsolète)
- Attente de réponses sur les forums
- Essais/erreurs pour comprendre
- Temps perdu à chercher des exemples

**Avec le RAD AI Companion** :
- Réponses instantanées dans le contexte de votre code
- Code généré adapté à votre projet
- Explications détaillées sur demande
- Suggestions intelligentes basées sur votre contexte
- Apprentissage personnalisé à votre rythme

### Différence avec ChatGPT ou Claude génériques

**ChatGPT/Claude standard** :
- Connaissance générale de Delphi
- Pas de contexte de votre projet
- Peut donner des exemples non adaptés
- Nécessite de copier-coller le code

**RAD AI Companion** :
- Entraîné spécifiquement sur le contenu RAD Studio
- Connaissance approfondie de Delphi, VCL, FMX, FireDAC, etc.
- Code généré prêt à utiliser
- Suggestions basées sur les bibliothèques officielles

## Accès et configuration

### Activation du RAD AI Companion

**Première utilisation** :

1. **Ouvrir le RAD AI Companion** :
   - Menu : **Aide → RAD AI Companion** (chemin officiel Embarcadero)
   - Ou icône dans la barre d'outils si disponible

2. **Configuration initiale** :
   - Accepter les conditions d'utilisation
   - Le RAD AI Companion s'ouvre dans son interface web intégrée
   - Configuration du compte (optionnelle pour fonctionnalités avancées)

3. **Interface** :
   - Interface web intégrée à l'IDE
   - Chatbot avec génération de code et réponses contextuelles
   - Possibilité de basculer entre chat et exemples de code

### Options de configuration

**Paramètres accessibles via** : Outils → Options → RAD AI Companion (ou rubrique dédiée selon la version)

### Configuration officielle de Smart CodeInsight

**Chemin d'accès** : **Outils → Options → Éditeur → Smart CodeInsight**

**Fournisseurs LLM supportés officiellement** :
1. **OpenAI** (API payante)
2. **Gemini by Google** (tier gratuit disponible — ⚠️ le tier gratuit utilise vos données pour l'entraînement)
3. **Claude by Anthropic** (API payante)
4. **Ollama** (hors ligne, self-hosted, gratuit MIT)

**Options de configuration** :
- Activation/désactivation globale de Smart CodeInsight
- Activation individuelle par moteur (par onglet)
- Moteur par défaut pour le chat et pour les commandes de l'éditeur
- Paramètres par moteur :
  - API BaseURL configurable
  - Choix du modèle (dropdown)
  - Paramètres spécifiques au fournisseur
- **Stockage chiffré des clés API** (sécurité)
- **Aucun "brokering" par Embarcadero** (les requêtes vont directement au fournisseur choisi)

### Fenêtre de chat Smart CodeInsight (dockable dans l'IDE)

**Commandes spéciales dans la zone de saisie** :
- `chatgpt>` — basculer vers OpenAI
- `gemini>` — basculer vers Gemini
- `claude>` — basculer vers Claude
- `ollama>` — basculer vers Ollama (local)
- `clear>` — effacer le memo de réponse
- `stop>` — arrêter la génération en cours
- **`Ctrl + Enter`** — démarrer la génération

### Commandes contextuelles dans l'éditeur (clic droit sur du code sélectionné)

Smart CodeInsight ajoute ces commandes au menu contextuel de l'éditeur :

| Commande | Description |
|----------|-------------|
| **AI Chat** | Ouvrir la fenêtre de chat |
| **Find Bugs** | Identifier les problèmes potentiels du code |
| **Explain Code** | Expliquer le code sélectionné |
| **Add Comment** | Insérer des commentaires explicatifs |
| **Complete the Code** | Compléter / étendre un snippet de code |
| **Optimize Code** | Améliorer l'efficacité du code |
| **Add Unit Test** | Générer du code de test unitaire |
| **Convert to Assembly** | Générer la sortie en assembleur |
| **Convert to Delphi** | Transpiler depuis C++ ou assembleur |
| **Convert to C++ Builder** | Transpiler vers C++ |

> 💡 Le résultat de ces commandes est inséré dans l'éditeur sous forme de **commentaire** après le code analysé, ce qui permet de relire et adapter le contenu généré avant utilisation.

### Modes d'utilisation

**Mode Chat (RAD AI Companion ou fenêtre Smart CodeInsight)** :
Conversation libre avec l'assistant, idéal pour :
- Poser des questions générales sur Delphi/RAD Studio (RAD AI Companion est spécifiquement entraîné dessus)
- Demander des explications
- Brainstorming de solutions
- Apprentissage de concepts

**Mode Contextuel (Smart CodeInsight dans l'éditeur)** :
Intégré à l'éditeur de code via le **clic droit sur du code sélectionné**, voir la liste des commandes ci-dessus.

**Mode Documentation** :
Le RAD AI Companion est spécifiquement entraîné sur la documentation RAD Studio, ce qui le rend très efficace pour répondre aux questions de documentation Embarcadero.

## Fonctionnalités principales

### 1. Génération de code

**Principe** : Décrivez ce que vous voulez faire en langage naturel, le RAD AI Companion génère le code Delphi correspondant.

**Comment l'utiliser** :

**Option A : Via le chat**
```
Vous : Crée une fonction qui valide un email

Companion : Voici une fonction de validation d'email :

function ValiderEmail(const Email: string): Boolean;  
var  
  RegEx: TRegEx;
begin
  RegEx := TRegEx.Create('^[\w\.-]+@[\w\.-]+\.\w{2,}$');
  Result := RegEx.IsMatch(Email);
end;

Cette fonction utilise une expression régulière pour vérifier le format.  
Voulez-vous des validations supplémentaires ?  
```

**Option B : Directement dans l'éditeur**
1. Tapez un commentaire : `// Fonction pour calculer la TVA`
2. Appuyez sur `Ctrl+Shift+G`
3. Le code est généré automatiquement

```pascal
// Fonction pour calculer la TVA
function CalculerTVA(MontantHT: Double; TauxTVA: Double = 20.0): Double;  
begin  
  Result := MontantHT * (TauxTVA / 100);
end;

function CalculerMontantTTC(MontantHT: Double; TauxTVA: Double = 20.0): Double;  
begin  
  Result := MontantHT + CalculerTVA(MontantHT, TauxTVA);
end;
```

**Exemples de demandes** :

```
"Crée une classe pour gérer une liste de clients avec CRUD"
"Génère une fonction de connexion à MySQL avec FireDAC"
"Fais une procédure pour exporter un StringGrid vers Excel"
"Crée un formulaire de login avec validation"
"Génère un thread pour télécharger un fichier en arrière-plan"
```

**Le RAD AI Companion comprend le contexte** :

Si vous avez déjà une classe `TClient` dans votre projet :
```
Vous : Ajoute une méthode pour rechercher un client par email

Companion : J'ai vu votre classe TClient. Voici une méthode de classe :

class function TClient.RechercherParEmail(
  Query: TFDQuery; const Email: string): Boolean;
begin
  // ℹ️ `class function` car la recherche ne porte pas sur une instance
  //    existante ; on vérifie juste si un client avec cet email existe.
  //    Pour un vrai pattern d'accès, voir le Repository plus bas.
  Query.Close;
  Query.SQL.Text := 'SELECT 1 FROM Clients WHERE Email = :Email';
  Query.ParamByName('Email').AsString := Email;
  Query.Open;
  Result := not Query.IsEmpty;
end;
```

### 2. Explication de code

**Principe** : Sélectionnez du code que vous ne comprenez pas, Smart CodeInsight l'explique en détail (commande officielle **Explain Code**).

**Utilisation** :

1. Sélectionnez le code mystérieux dans l'éditeur
2. **Clic droit → Explain Code** (commande officielle Smart CodeInsight)
3. L'explication est insérée comme commentaire après le code

**Exemple** :

**Code sélectionné** :
```pascal
TTask.Run(procedure  
begin  
  TThread.Synchronize(nil, procedure
  begin
    Label1.Caption := 'Traitement terminé';
  end);
end);
```

**Explication du RAD AI Companion** :
```
Ce code utilise le multithreading avec TTask pour exécuter du code  
en arrière-plan :  

1. TTask.Run crée une tâche asynchrone qui s'exécute dans un thread
   secondaire

2. Le code dans TTask.Run ne bloque pas l'interface utilisateur

3. TThread.Synchronize retourne au thread principal (UI thread)
   pour modifier l'interface en toute sécurité

4. Label1.Caption est modifié dans le thread principal, ce qui est
   obligatoire pour les composants visuels

⚠️ Important : Ne jamais modifier directement des composants VCL/FMX
depuis un thread secondaire sans Synchronize ou Queue.

Voulez-vous un exemple plus complet avec gestion d'erreurs ?
```

**Niveaux d'explication** :

- **Débutant** : Explications simples, analogies, pas de jargon
- **Intermédiaire** : Détails techniques, bonnes pratiques
- **Avancé** : Optimisations, edge cases, considérations avancées

### 3. Détection et correction d'erreurs

**Principe** : Lorsque votre code a un bug potentiel, Smart CodeInsight analyse et suggère des corrections (commande officielle **Find Bugs**).

**Scénario 1 : Code avec bug potentiel**

**Code avec erreur** :
```pascal
var
  Liste: TStringList;
begin
  Liste.Add('Test');  // ❌ Erreur : variable non initialisée
end;
```

**Action** : Sélectionner le bloc puis **Clic droit → Find Bugs** (commande officielle Smart CodeInsight)

**Suggestion du RAD AI Companion** :
```
Erreur détectée : "Variable Liste might not have been initialized"

Problème : Vous utilisez Liste sans l'avoir créée avec .Create

Solution :

var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    Liste.Add('Test');
    // Utiliser la liste...
  finally
    Liste.Free;  // Toujours libérer la mémoire
  end;
end;

💡 Bonne pratique : Utilisez toujours try-finally pour garantir
la libération des objets.

[Appliquer le correctif] [Expliquer davantage]
```

**Scénario 2 : Erreur à l'exécution**

Lorsqu'une exception se produit :

```
Exception levée : EConvertError - "'abc' is not a valid integer"
```

**Companion analyse** :
```
Cette erreur se produit lors d'une conversion de string vers entier.

Dans votre code ligne 45 :
  Valeur := StrToInt(EditNombre.Text);

Problème : EditNombre.Text contient 'abc' qui ne peut pas être  
converti en entier.  

Solutions possibles :

1. Validation avant conversion :
   if TryStrToInt(EditNombre.Text, Valeur) then
     // Conversion réussie
   else
     ShowMessage('Veuillez entrer un nombre valide');

2. Gestion d'exception :
   try
     Valeur := StrToInt(EditNombre.Text);
   except
     on E: EConvertError do
       ShowMessage('Entrée invalide : ' + E.Message);
   end;

3. Validation dans l'événement OnKeyPress :
   procedure EditNombreKeyPress(Sender: TObject; var Key: Char);
   begin
     if not CharInSet(Key, ['0'..'9', #8]) then
       Key := #0;  // Bloquer les caractères non-numériques
   end;

Quelle approche préférez-vous ?
```

### 4. Refactoring assisté

**Principe** : Améliorer du code existant tout en conservant son comportement.

**Opérations disponibles** :

**A. Extraire une méthode**

**Code initial** :
```pascal
procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  if EditNom.Text.Trim.IsEmpty then
  begin
    ShowMessage('Le nom est requis');
    EditNom.SetFocus;
    Exit;
  end;

  if EditEmail.Text.Trim.IsEmpty then
  begin
    ShowMessage('L''email est requis');
    EditEmail.SetFocus;
    Exit;
  end;

  if not ValiderFormatEmail(EditEmail.Text) then
  begin
    ShowMessage('Format email invalide');
    EditEmail.SetFocus;
    Exit;
  end;

  // Sauvegarder...
end;
```

**Action** : Sélectionner le code → **Clic droit → Optimize Code** (commande officielle Smart CodeInsight) ou demander un refactor dans le chat

**Companion suggère** :
```pascal
function TForm1.ValiderFormulaire: Boolean;  
begin  
  Result := False;

  if EditNom.Text.Trim.IsEmpty then
  begin
    ShowMessage('Le nom est requis');
    EditNom.SetFocus;
    Exit;
  end;

  if EditEmail.Text.Trim.IsEmpty then
  begin
    ShowMessage('L''email est requis');
    EditEmail.SetFocus;
    Exit;
  end;

  if not ValiderFormatEmail(EditEmail.Text) then
  begin
    ShowMessage('Format email invalide');
    EditEmail.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  if not ValiderFormulaire then
    Exit;

  // Sauvegarder...
end;

✅ Code plus lisible et réutilisable
✅ Séparation des responsabilités
✅ Facilite les tests unitaires
```

**B. Moderniser du code**

**Code ancien** :
```pascal
var
  i: Integer;
  Trouve: Boolean;
begin
  Trouve := False;
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    if ListBox1.Items[i] = 'Test' then
    begin
      Trouve := True;
      Break;
    end;
  end;

  if Trouve then
    ShowMessage('Trouvé');
end;
```

**Companion modernise** :
```pascal
// Version moderne et concise (TStrings expose nativement IndexOf)
if ListBox1.Items.IndexOf('Test') >= 0 then
  ShowMessage('Trouvé');

// Ou recherche insensible à la casse :
uses
  System.StrUtils;

if MatchText('Test', ListBox1.Items.ToStringArray) then
  ShowMessage('Trouvé (insensible à la casse)');

💡 La méthode IndexOf est plus efficace et lisible que la boucle manuelle.
   Évitez `TList<string>.Create(...).Contains(...)` qui crée une liste
   intermédiaire JAMAIS LIBÉRÉE → fuite mémoire.
```

**C. Optimiser les performances**

**Code à optimiser** :
```pascal
for i := 0 to 10000 do  
begin  
  Query.Close;
  Query.SQL.Text := 'SELECT * FROM Clients WHERE ID = ' + IntToStr(i);
  Query.Open;
  // Traiter...
end;
```

**Companion suggère** :
```pascal
// ❌ Problème : 10 000 requêtes SQL = très lent

// ✅ Solution 1 : Une seule requête
Query.Close;  
Query.SQL.Text := 'SELECT * FROM Clients WHERE ID BETWEEN 0 AND 10000';  
Query.Open;  
while not Query.Eof do  
begin  
  // Traiter...
  Query.Next;
end;

// ✅ Solution 2 : Requête paramétrée (si vraiment nécessaire de boucler)
Query.SQL.Text := 'SELECT * FROM Clients WHERE ID = :ID';  
for i := 0 to 10000 do  
begin  
  Query.Close;
  Query.ParamByName('ID').AsInteger := i;
  Query.Open;
  // Traiter...
end;

⚡ Gain de performance : ~100x plus rapide avec une seule requête
```

### 5. Recherche dans la documentation

**Principe** : Recherche intelligente qui comprend votre intention, pas seulement les mots-clés.

**Recherche traditionnelle** :
```
Recherche : "TButton"  
Résultats : Documentation standard de TButton  
```

**Recherche avec RAD AI Companion** :
```
Vous : Comment changer la couleur d'un bouton au survol de la souris ?

Companion : Pour changer la couleur d'un bouton au survol, utilisez les
événements OnMouseEnter et OnMouseLeave :

procedure TForm1.Button1MouseEnter(Sender: TObject);  
begin  
  Button1.Color := clLime;  // VCL
  // ou
  Button1.Fill.Color := TAlphaColors.Lime;  // FMX
end;

procedure TForm1.Button1MouseLeave(Sender: TObject);  
begin  
  Button1.Color := clBtnFace;  // Couleur par défaut VCL
  // ou
  Button1.Fill.Color := TAlphaColors.Silver;  // FMX
end;

⚠️ Pour TButton VCL, la couleur peut être ignorée par les thèmes Windows.
   Options pour un vrai effet visuel :
   - Désactiver les thèmes : `TButton.StyleElements := [];`
   - Utiliser TPanel ou TSpeedButton qui supportent mieux la coloration
   - Utiliser un style VCL personnalisé

💡 Pour un effet plus sophistiqué avec FireMonkey, utilisez les styles
   et animations (TColorAnimation, TFloatAnimation).

Documentation connexe :
- [TControl.OnMouseEnter](lien)
- [Styles FireMonkey](lien)
- [Animations avec TColorAnimation](lien)

Voulez-vous un exemple avec animations ?
```

**Recherche contextuelle** :

Si vous êtes dans un fichier utilisant FireDAC :
```
Vous : Comment optimiser les requêtes ?

Companion : Dans le contexte FireDAC, voici les optimisations principales :

1. FetchOptions.Mode := fmAll (charge tout en mémoire si dataset petit)
2. FetchOptions.RowsetSize := 100 (pour gros datasets)
3. Utiliser CachedUpdates pour modifications batch
4. Indexes locaux pour filtrage client

Code exemple basé sur votre TFDQuery existant :

FDQuery1.FetchOptions.Mode := fmAll;  
FDQuery1.FetchOptions.RowsetSize := 100;  
FDQuery1.CachedUpdates := True;  
// Modifications...
FDQuery1.ApplyUpdates; // Envoyer toutes les modifications en une fois

[Voir plus d'optimisations] [Comparer avec d'autres frameworks]
```

### 6. Suggestions de code intelligentes

**Principe** : Pendant que vous codez, le RAD AI Companion suggère automatiquement du code pertinent.

**Scénario 1 : Complétion intelligente**

**Vous tapez** :
```pascal
function ConnecterMySQL
```

**Companion suggère automatiquement** (en grisé) :
```pascal
function ConnecterMySQL(const Host, Database, Username, Password: string): Boolean;  
var  
  Connection: TFDConnection;
begin
  // ⚠️ `function` (pas `procedure`) car on retourne un Boolean. Une procédure
  // n'a pas de valeur de retour Result.
  Result := False;
  Connection := TFDConnection.Create(nil);
  try
    Connection.DriverName := 'MySQL';
    Connection.Params.Values['Server'] := Host;
    Connection.Params.Values['Database'] := Database;
    Connection.Params.Values['User_Name'] := Username;
    Connection.Params.Values['Password'] := Password;

    try
      Connection.Connected := True;
      Result := True;
    except
      on E: Exception do
        ShowMessage('Erreur de connexion : ' + E.Message);
        // Result reste False (initialisé en début de fonction)
    end;
  finally
    Connection.Free;
  end;
end;
```

⚠️ **Note pédagogique** : cet exemple ferme la connexion juste après le test (via Connection.Free), ce qui est rarement utile en pratique. En production, on garde généralement la TFDConnection en variable de classe et on ferme à la fermeture de l'application.

**Appuyez sur `Tab`** pour accepter la suggestion.

**Scénario 2 : Pattern recognition**

**Vous commencez** :
```pascal
try
  // Votre code
```

**Companion complète** :
```pascal
try
  // Votre code
except
  on E: Exception do
  begin
    // Gérer l'exception
    ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

**Scénario 3 : Suggestions basées sur le contexte**

Si vous avez un `TFDQuery` nommé `QueryClients` :

**Vous tapez** : `Query`

**Companion suggère** :
```
QueryClients.Close;  
QueryClients.SQL.Text := '';  
QueryClients.Open;  
```

Plutôt qu'une suggestion générique.

### 7. Apprentissage personnalisé

**Principe** : Le RAD AI Companion adapte ses réponses à votre niveau et style de code.

**Initialisation** :

À la première utilisation, quelques questions :
```
Quel est votre niveau en Delphi ?
☐ Débutant (< 6 mois)
☐ Intermédiaire (6 mois - 2 ans)
☑ Avancé (> 2 ans)

Quel type d'applications développez-vous ?
☑ Desktop (VCL)
☑ Multi-plateforme (FMX)
☐ Mobile
☐ Services/API

Frameworks utilisés :
☑ FireDAC
☑ REST
☐ DataSnap
☐ Autre
```

**Adaptation des réponses** :

**Pour un débutant** :
```
Vous : Comment créer un bouton dynamiquement ?

Companion : Pour créer un bouton pendant l'exécution (dynamiquement) :

1. Déclarez une variable de type TButton
2. Créez-le avec .Create
3. Définissez ses propriétés (taille, position, texte)
4. Définissez son Parent (formulaire ou panel)
5. Attachez un événement si nécessaire

Code complet avec explications :

var
  MonBouton: TButton;
begin
  // 1. Créer le bouton (le paramètre Self = propriétaire)
  MonBouton := TButton.Create(Self);

  // 2. Définir où il apparaît
  MonBouton.Parent := Self;  // Sur le formulaire

  // 3. Position et taille
  MonBouton.Left := 100;
  MonBouton.Top := 100;
  MonBouton.Width := 100;
  MonBouton.Height := 30;

  // 4. Texte affiché
  MonBouton.Caption := 'Cliquez-moi';

  // 5. Action au clic
  MonBouton.OnClick := BoutonDynamiqueClick;
end;

procedure TForm1.BoutonDynamiqueClick(Sender: TObject);  
begin  
  ShowMessage('Bouton dynamique cliqué !');
end;

💡 Le paramètre Self dans .Create(Self) fait que le bouton sera
automatiquement libéré quand le formulaire sera fermé.

[Tester ce code] [Voir plus d'exemples] [Créer plusieurs boutons]
```

**Pour un développeur avancé** :
```
Vous : Comment créer un bouton dynamiquement ?

Companion :  
var  
  Btn: TButton;
begin
  Btn := TButton.Create(Self);
  Btn.Parent := Self;
  Btn.SetBounds(100, 100, 100, 30);
  Btn.Caption := 'Dynamic';
  Btn.OnClick := BoutonDynamiqueClick;  // Voir méthode ci-dessus
end;

Alternative avec TButtonedEdit pour validation inline, ou TButtonStyleLink  
pour FMX custom styling ?  
```

### 8. Génération de tests unitaires

**Principe** : Smart CodeInsight génère automatiquement des tests pour votre code via la commande officielle **Add Unit Test**.

**Votre code** :
```pascal
function CalculerRemise(MontantTotal: Double; CodePromo: string): Double;  
begin  
  Result := 0;

  if MontantTotal <= 0 then
    Exit;

  if CodePromo = 'PROMO10' then
    Result := MontantTotal * 0.10
  else if CodePromo = 'PROMO20' then
    Result := MontantTotal * 0.20
  else if CodePromo = 'NOEL' then
    Result := MontantTotal * 0.25;
end;
```

**Action** : Sélectionner la fonction → **Clic droit → Add Unit Test** (commande officielle Smart CodeInsight)

**Tests générés** (DUnitX) :
```pascal
unit Tests.Remises;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestsRemises = class
  public
    [Test]
    procedure Test_CalculerRemise_Promo10;
    [Test]
    procedure Test_CalculerRemise_Promo20;
    [Test]
    procedure Test_CalculerRemise_Noel;
    [Test]
    procedure Test_CalculerRemise_CodeInvalide;
    [Test]
    procedure Test_CalculerRemise_MontantNegatif;
    [Test]
    procedure Test_CalculerRemise_MontantZero;
  end;

implementation

uses
  UnitCalculs;

procedure TTestsRemises.Test_CalculerRemise_Promo10;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(100.0, 'PROMO10');
  Assert.AreEqual(10.0, Resultat, 0.01);
end;

procedure TTestsRemises.Test_CalculerRemise_Promo20;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(100.0, 'PROMO20');
  Assert.AreEqual(20.0, Resultat, 0.01);
end;

procedure TTestsRemises.Test_CalculerRemise_Noel;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(100.0, 'NOEL');
  Assert.AreEqual(25.0, Resultat, 0.01);
end;

procedure TTestsRemises.Test_CalculerRemise_CodeInvalide;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(100.0, 'INVALID');
  Assert.AreEqual(0.0, Resultat, 0.01);
end;

procedure TTestsRemises.Test_CalculerRemise_MontantNegatif;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(-50.0, 'PROMO10');
  Assert.AreEqual(0.0, Resultat, 0.01);
end;

procedure TTestsRemises.Test_CalculerRemise_MontantZero;  
var  
  Resultat: Double;
begin
  Resultat := CalculerRemise(0.0, 'PROMO10');
  Assert.AreEqual(0.0, Resultat, 0.01);
end;

end.
```

**Le RAD AI Companion explique** :
```
J'ai généré 6 tests couvrant les cas principaux :
✅ Trois codes promotionnels valides
✅ Code invalide
✅ Montant négatif (edge case)
✅ Montant zéro (edge case)

Couverture : ~95% des branches de code

Suggestions d'amélioration de votre fonction :
1. Valider que CodePromo n'est pas vide
2. Gérer les cas insensibles à la casse (uppercase)
3. Logger les tentatives de codes invalides

[Exécuter les tests] [Améliorer le code] [Ajouter plus de tests]
```

## Cas d'usage avancés

### 1. Conversion de code

**Scénario** : Vous avez du code C# et voulez l'adapter en Delphi.

**Code C#** :
```csharp
public class Client
{
    public int Id { get; set; }
    public string Nom { get; set; }
    public string Email { get; set; }

    public bool EstValide()
    {
        return !string.IsNullOrEmpty(Nom) &&
               !string.IsNullOrEmpty(Email);
    }
}
```

**Action** : Smart CodeInsight propose la commande **Convert to Delphi** (transpile depuis C++ ou assembleur). Pour C# / autres langages, utilisez le chat du RAD AI Companion ou collez le code dans la fenêtre de chat Smart CodeInsight.

**Résultat Delphi** :
```pascal
type
  TClient = class
  private
    FId: Integer;
    FNom: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;

    function EstValide: Boolean;
  end;

implementation

function TClient.EstValide: Boolean;  
begin  
  Result := not Nom.Trim.IsEmpty and not Email.Trim.IsEmpty;
end;
```

### 2. Architecture et design patterns

**Vous** : Je veux implémenter le pattern Repository pour mes clients

**Companion** :
```pascal
// Interface du repository (pour injection de dépendance)
// ⚠️ Générer le GUID avec `Ctrl+Shift+G` dans l'IDE (raccourci standard
//    Delphi pour insérer un GUID au point d'insertion).
type
  IClientRepository = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetAll: TObjectList<TClient>;
    function GetById(Id: Integer): TClient;
    function Add(Client: TClient): Boolean;
    function Update(Client: TClient): Boolean;
    function Delete(Id: Integer): Boolean;
  end;

// Implémentation avec FireDAC
type
  TClientRepository = class(TInterfacedObject, IClientRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(Connection: TFDConnection);

    function GetAll: TObjectList<TClient>;
    function GetById(Id: Integer): TClient;
    function Add(Client: TClient): Boolean;
    function Update(Client: TClient): Boolean;
    function Delete(Id: Integer): Boolean;
  end;

implementation

constructor TClientRepository.Create(Connection: TFDConnection);  
begin  
  inherited Create;
  FConnection := Connection;
end;

function TClientRepository.GetAll: TObjectList<TClient>;  
var  
  Query: TFDQuery;
  Client: TClient;
begin
  Result := TObjectList<TClient>.Create(True);
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM Clients';
    Query.Open;

    while not Query.Eof do
    begin
      Client := TClient.Create;
      Client.Id := Query.FieldByName('Id').AsInteger;
      Client.Nom := Query.FieldByName('Nom').AsString;
      Client.Email := Query.FieldByName('Email').AsString;
      Result.Add(Client);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

// ... autres méthodes ...

// Utilisation dans votre formulaire :
var
  Repository: IClientRepository;
  Clients: TObjectList<TClient>;
begin
  Repository := TClientRepository.Create(FDConnection1);
  Clients := Repository.GetAll;
  try
    // Utiliser la liste...
  finally
    Clients.Free;
  end;
end;

✅ Avantages du pattern Repository :
- Séparation logique métier / accès données
- Facilite les tests unitaires (mock possible)
- Code réutilisable
- Changement de source de données facilité

[Implémenter Unit of Work] [Ajouter le caching] [Tests unitaires]
```

### 3. Migration de code legacy

**Vous** : J'ai du vieux code Delphi 7, aide-moi à le moderniser pour Delphi 13

**Code Delphi 7** :
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  i: Integer;
  s: string;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    s := ListBox1.Items[i];
    if Pos('test', LowerCase(s)) > 0 then
    begin
      Memo1.Lines.Add(s);
    end;
  end;
end;
```

**Companion modernise** :
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Item: string;
begin
  // Version moderne avec for-in et ContainsText
  for Item in ListBox1.Items do
  begin
    if ContainsText(Item, 'test') then
      Memo1.Lines.Add(Item);
  end;
end;

// Ou version utilisant un TList<string> pour filtrer puis transférer
uses
  System.Generics.Collections, System.StrUtils;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Item: string;
  Filtered: TList<string>;
begin
  Filtered := TList<string>.Create;
  try
    for Item in ListBox1.Items do
      if ContainsText(Item, 'test') then
        Filtered.Add(Item);

    Memo1.Lines.AddStrings(Filtered.ToArray);
  finally
    Filtered.Free; // ⚠️ Toujours libérer les TList créés manuellement
  end;
end;

Améliorations apportées :
✅ Syntaxe for-in plus lisible
✅ ContainsText (case-insensitive) au lieu de Pos+LowerCase
✅ Méthodes modernes de la RTL
✅ Pattern try/finally pour éviter les fuites mémoire

⚠️ Note : `TArray.Filter<T>` n'existe pas dans la RTL Delphi standard.
   Pour du vrai filtrage fonctionnel, utilisez Spring4D ou implémentez
   votre propre helper.

Autres modernisations possibles :
- [Utiliser TTask pour exécution asynchrone]
- [Ajouter validation d'entrée]
- [Implémenter recherche incrémentale]
```

### 4. Optimisation de requêtes SQL

**Vous** : Cette requête est lente, comment l'optimiser ?

**Votre requête** :
```sql
SELECT c.*,
       (SELECT COUNT(*) FROM Commandes WHERE ClientId = c.Id) as NbCommandes,
       (SELECT SUM(Montant) FROM Commandes WHERE ClientId = c.Id) as TotalAchats
FROM Clients c  
WHERE c.Ville = 'Paris'  
```

**Companion analyse** :
```
Problèmes détectés :
❌ Deux sous-requêtes corrélées (exécutées pour chaque ligne de Clients)
❌ Scan complet de Commandes pour chaque client
❌ Pas d'index visible sur ClientId / Ville

Version optimisée :

SELECT c.*,
       COALESCE(stats.NbCommandes, 0) as NbCommandes,
       COALESCE(stats.TotalAchats, 0) as TotalAchats
FROM Clients c  
LEFT JOIN (  
    SELECT ClientId,
           COUNT(*) as NbCommandes,
           SUM(Montant) as TotalAchats
    FROM Commandes
    GROUP BY ClientId
) stats ON stats.ClientId = c.Id
WHERE c.Ville = 'Paris'

Améliorations :
✅ Une seule jointure au lieu de N sous-requêtes
✅ Agrégation groupée efficace
✅ COALESCE pour gérer les clients sans commande

Recommandations supplémentaires :
1. Créer un index : CREATE INDEX idx_clients_ville ON Clients(Ville)
2. Créer un index : CREATE INDEX idx_commandes_clientid ON Commandes(ClientId)
3. Si requête très fréquente : envisager une vue matérialisée

Gain de performance estimé : 10x à 100x selon volume de données

[Tester la requête] [Créer les index] [Voir le plan d'exécution]
```

## Intégration avec le workflow

### Raccourcis essentiels

**À mémoriser** :

```
Ctrl+Shift+A  → Ouvrir le RAD AI Companion (menu Aide → RAD AI Companion)  
Ctrl+Shift+G  → Générer code depuis commentaire  
Ctrl+Shift+E  → Expliquer sélection  
Ctrl+Shift+F  → Corriger erreur  
Ctrl+Shift+R  → Refactorer  
Ctrl+Shift+T  → Générer tests  
Ctrl+Shift+D  → Documentation interactive  
```

### Workflow de développement optimal

**1. Phase de conception** :
```
Vous → Companion : "Je veux créer une application de gestion de stock.
                     Suggère une architecture."

Companion → Génère structure de projet complète avec :
- Schéma de base de données
- Architecture en couches
- Classes principales
- Interfaces utilisateur suggérées
```

**2. Phase de développement** :
```
- Écrire des commentaires décrivant ce que vous voulez
- Companion génère le code
- Ajuster/personnaliser au besoin
- Demander explications sur le code généré si nécessaire
```

**3. Phase de débogage** :
```
- Erreur détectée
- Clic sur ampoule 💡
- Companion suggère corrections
- Appliquer ou adapter
```

**4. Phase d'optimisation** :
```
- Sélectionner code à optimiser
- "Optimiser ce code"
- Companion analyse et suggère améliorations
- Comparer performances
```

**5. Phase de documentation** :
```
- Sélectionner méthode/classe
- "Générer documentation XML"
- Documentation générée automatiquement
```

## Confidentialité et sécurité

### Ce qui est envoyé aux fournisseurs IA

**Important** : Selon la documentation officielle Embarcadero, **les requêtes ne sont PAS "brokerées" par Embarcadero** — elles vont directement au fournisseur IA que vous avez configuré (OpenAI, Claude, Gemini ou Ollama). Embarcadero ne reçoit ni ne traite vos données.

**Pour Smart CodeInsight (commandes éditeur ou chat IDE)** :
- Vous contrôlez explicitement ce qui est envoyé : sélection + clic droit + commande
- Le code sélectionné ET le prompt vont au fournisseur configuré
- Les clés API sont **chiffrées** dans la configuration locale

**Pour RAD AI Companion (chatbot web Embarcadero)** :
- Le chat est entraîné sur le contenu RAD Studio officiel
- ⚠️ Selon la page Embarcadero : *"questions and responses may be recorded and shared with third parties for analysis and improvement"*
- Ne saisissez pas de données confidentielles dans ce chat

**Bonnes pratiques** :
- **JAMAIS** envoyer de clés API, mots de passe, secrets dans le code envoyé
- **JAMAIS** envoyer de données utilisateur réelles (clients, etc.)
- Anonymisez les exemples avant de demander de l'aide
- Si vous traitez des données sensibles, privilégiez **Ollama** (LLM local, hors ligne)

### Mode hors ligne via Ollama

**Smart CodeInsight supporte Ollama**, un runtime LLM local et open source (licence MIT) :

- ✅ Aucune donnée n'est envoyée à un serveur externe
- ✅ Idéal pour les secteurs régulés (banque, santé, gouvernement)
- ✅ Pas de coût d'API
- ⚠️ Nécessite des ressources matérielles locales (GPU recommandé pour de bonnes performances)

**Configuration Ollama** :
1. Installer Ollama depuis [ollama.com](https://ollama.com)
2. Télécharger un modèle : `ollama pull llama3`
3. Configurer Smart CodeInsight pour pointer vers `http://localhost:11434`

## Limitations et considérations

### Limitations actuelles

**1. Le RAD AI Companion peut se tromper**

Toujours vérifier et tester le code généré.

```
⚠️ Le RAD AI Companion génère du code basé sur des patterns appris.
   Il peut parfois :
   - Utiliser des noms de variables inappropriés
   - Manquer des cas limites
   - Suggérer du code sous-optimal dans certains contextes

   → Toujours relire et comprendre le code avant de l'utiliser
```

**2. Connaissances limitées**

```
Le RAD AI Companion connaît bien :
✅ Delphi jusqu'à la version courante (Delphi 13 Florence)
✅ VCL, FireMonkey
✅ Patterns et pratiques courantes
✅ Bibliothèques populaires

Connaissances limitées :
⚠️ Bibliothèques tierces très spécifiques
⚠️ Code propriétaire de votre entreprise
⚠️ Évolutions très récentes du langage (date de coupure des données d'entraînement)
```

**3. Contexte limité**

Le RAD AI Companion voit votre code par morceaux, pas l'architecture complète.

**Solution** : Fournir du contexte dans vos questions.

```
❌ Mauvais : "Optimise cette fonction"

✅ Bon : "Optimise cette fonction de recherche. Elle est appelée
         1000 fois par seconde dans une boucle. La liste contient
         environ 10 000 éléments."
```

### Bonnes pratiques

**1. Soyez précis dans vos demandes**

```
❌ "Fais une fonction de tri"

✅ "Crée une fonction qui trie une TStringList par ordre alphabétique
    décroissant, en ignorant la casse"
```

**2. Itérez progressivement**

```
Étape 1 : "Crée une classe de base Client"
Étape 2 : "Ajoute validation email"
Étape 3 : "Ajoute méthode ToString"
```

Plutôt que tout demander d'un coup.

**3. Validez toujours le code**

- Compiler pour vérifier la syntaxe
- Tester avec différents cas d'usage
- Vérifier les edge cases
- Exécuter les tests unitaires

**4. Apprenez du code généré**

Utilisez le RAD AI Companion comme outil d'apprentissage :
- Demandez des explications sur le code généré
- Comprenez les patterns utilisés
- Explorez les alternatives suggérées

## Statistiques et métriques

### Tableau de bord d'utilisation

Accessible via : Aide → RAD AI Companion (puis section Statistiques selon les versions)

**Métriques disponibles** :

```
📊 Statistiques de la semaine

Productivité :
- Lignes de code générées : 2,847
- Erreurs corrigées : 23
- Temps gagné estimé : 8.5 heures

Utilisation :
- Questions posées : 156
- Code expliqué : 89 fois
- Refactoring : 12 fois

Top questions :
1. "Comment créer un thread ?"
2. "Optimiser requête SQL"
3. "Gérer les exceptions"

Satisfaction :
👍 94% des suggestions acceptées
⭐ Note moyenne : 4.7/5
```

### Mesure de l'impact

**Avant RAD AI Companion** :
- Temps moyen pour résoudre une erreur : 15 min
- Temps pour implémenter nouvelle fonctionnalité : 2h
- Recherche documentation : 30 min/jour

**Avec RAD AI Companion** :
- Temps moyen pour résoudre une erreur : 2 min (-87%)
- Temps pour implémenter nouvelle fonctionnalité : 45 min (-62%)
- Recherche documentation : 5 min/jour (-83%)

**ROI estimé** : Gain de productivité de 30-50% en développement

## Conclusion

Le **RAD AI Companion** de Delphi 13 Florence n'est pas qu'un simple outil de génération de code. C'est un véritable assistant de développement qui transforme votre façon de travailler avec Delphi.

**Ce que le RAD AI Companion change** :
- Apprentissage accéléré pour les débutants
- Productivité décuplée pour les experts
- Réduction drastique du temps de débogage
- Code de meilleure qualité
- Documentation instantanée
- Tests générés automatiquement

**Comment en tirer le maximum** :
1. Utilisez-le dès le début de votre apprentissage
2. N'hésitez pas à poser des questions "bêtes"
3. Demandez toujours des explications sur le code généré
4. Itérez sur les solutions proposées
5. Combinez votre expertise avec l'assistance IA
6. Partagez vos découvertes avec la communauté

**L'avenir du développement Delphi** :

Le RAD AI Companion n'est que le début. Embarcadero continue d'investir dans l'IA pour améliorer l'expérience de développement. Les prochaines versions apporteront :
- Compréhension encore meilleure du contexte
- Suggestions proactives plus pertinentes
- Génération d'architectures complètes
- Détection automatique de vulnérabilités
- Optimisations encore plus poussées

**Le développeur reste au centre** :

Le RAD AI Companion est un outil puissant, mais c'est vous, le développeur, qui gardez le contrôle. L'IA vous assiste, vous suggère, vous accélère, mais c'est votre expertise, votre créativité et votre jugement qui font la différence.

Avec le RAD AI Companion de Delphi 13, vous avez maintenant un superpouvoir pour développer plus rapidement, mieux, et avec plus de plaisir. Utilisez-le bien !

---

**Fin du chapitre 22 - Intelligence Artificielle et Machine Learning avec Delphi**

Vous avez maintenant toutes les clés pour intégrer l'IA dans vos applications Delphi, des concepts fondamentaux aux outils les plus avancés. L'ère de l'IA avec Delphi ne fait que commencer, et vous êtes maintenant prêt à en faire partie !

⏭️ [Conception d'applications Web avec Delphi](/23-conception-dapplications-web-avec-delphi/README.md)
