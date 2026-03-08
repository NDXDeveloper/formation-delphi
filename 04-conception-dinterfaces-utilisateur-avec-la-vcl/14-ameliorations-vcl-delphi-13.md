🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.14 Améliorations VCL de Delphi 13

## Introduction

Delphi 13 Florence marque une étape importante dans l'évolution de la VCL (Visual Component Library). Cette version apporte de nombreuses améliorations qui facilitent le développement, améliorent les performances et offrent une meilleure intégration avec Windows 11. Ce chapitre explore toutes ces nouveautés pour vous permettre d'en tirer le meilleur parti.

## 4.14.1 Vue d'ensemble des nouveautés

### Améliorations principales

**1. Support Windows 11 avancé**
- Nouveaux styles VCL Windows 11
- Support natif des coins arrondis
- Intégration avec les thèmes système
- Effets Mica et Acrylic

**2. Langage Object Pascal**
- Opérateur ternaire (if-then-else inline)
- Améliorations de la syntaxe
- Optimisations du compilateur
- Meilleure gestion de la mémoire

**3. IDE et productivité**
- Site web companion IA pour assistance
- GetIt Package Manager amélioré
- Support LLDB v12 pour débogage avancé
- Outils de refactoring améliorés

**4. Composants VCL**
- Composants modernisés pour Windows 11
- Nouvelles propriétés et méthodes
- Support DPI amélioré
- Performances optimisées

**5. FireMonkey (FMX)**
- Améliorations de rendu
- Nouveaux contrôles
- Meilleures performances
- Support Linux amélioré

---

## 4.14.2 L'opérateur ternaire

### Qu'est-ce que l'opérateur ternaire ?

L'opérateur ternaire est une nouveauté majeure de Delphi 13 qui permet d'écrire des conditions if-then-else de manière concise sur une seule ligne.

**Syntaxe :**
```pascal
Résultat := if Condition then ValeurSiVrai else ValeurSiFaux;
```

### Exemples pratiques

**Avant Delphi 13 (méthode classique) :**
```pascal
var
  Message: string;
  Statut: string;
  Couleur: TColor;
begin
  // Exemple 1 : Message selon l'âge
  if Age >= 18 then
    Message := 'Majeur'
  else
    Message := 'Mineur';

  // Exemple 2 : Statut selon le score
  if Score >= 50 then
    Statut := 'Réussi'
  else
    Statut := 'Échoué';

  // Exemple 3 : Couleur selon l'état
  if EstActif then
    Couleur := clGreen
  else
    Couleur := clRed;
end;
```

**Avec Delphi 13 (opérateur ternaire) :**
```pascal
var
  Message: string;
  Statut: string;
  Couleur: TColor;
begin
  // Exemple 1 : Message selon l'âge
  Message := if Age >= 18 then 'Majeur' else 'Mineur';

  // Exemple 2 : Statut selon le score
  Statut := if Score >= 50 then 'Réussi' else 'Échoué';

  // Exemple 3 : Couleur selon l'état
  Couleur := if EstActif then clGreen else clRed;
end;
```

### Cas d'usage avancés

**Dans les propriétés :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration conditionnelle en une ligne
  Button1.Enabled := if CheckBox1.Checked then True else False;

  // Ou plus simplement
  Button1.Enabled := CheckBox1.Checked; // Directement le booléen

  // Mais utile pour des cas plus complexes
  Button1.Caption := if Edit1.Text <> '' then 'Valider' else 'Saisir';

  Label1.Font.Color := if Value > 100 then clRed else clBlack;
end;
```

**Dans les calculs :**
```pascal
function CalculerRemise(Montant: Double; EstMembre: Boolean): Double;  
begin  
  // Remise de 10% pour les membres, sinon 5%
  Result := Montant * (if EstMembre then 0.90 else 0.95);
end;

function ObtenirNote(Score: Integer): string;  
begin  
  // Notes simplifiées
  Result := if Score >= 90 then 'A'
       else if Score >= 80 then 'B'
       else if Score >= 70 then 'C'
       else if Score >= 60 then 'D'
       else 'F';
end;
```

**Dans les affichages :**
```pascal
procedure TForm1.AfficherResultat;  
begin  
  // Message pluriel/singulier
  LabelResultat.Caption := Format('Vous avez %d %s',
    [NbItems, if NbItems > 1 then 'éléments' else 'élément']);

  // Statut avec emoji
  LabelStatut.Caption := if EstConnecte then '✓ Connecté' else '✗ Déconnecté';

  // Taille conditionnelle
  Panel1.Height := if EstExpanded then ScaleValue(300) else ScaleValue(50);
end;
```

**Imbrications (à utiliser avec modération) :**
```pascal
var
  Note: string;
begin
  // Ternaire imbriqué (attention à la lisibilité)
  Note := if Score >= 90 then 'Excellent'
     else if Score >= 70 then 'Bien'
     else if Score >= 50 then 'Moyen'
     else 'Insuffisant';

  // Peut devenir difficile à lire si trop imbriqué
  // Dans ce cas, préférer un case of classique
end;
```

### Bonnes pratiques

**✓ À faire :**
```pascal
// Expressions simples et lisibles
Couleur := if EstActif then clGreen else clRed;  
Message := if Count = 0 then 'Aucun' else IntToStr(Count);  
Visible := if Mode = mEdit then True else False;  
```

**✗ À éviter :**
```pascal
// Trop complexe, difficile à lire
Result := if (A > B) and ((C < D) or (E = F)) then
            if G > H then X else Y
          else
            if I < J then Z else W;

// Préférer dans ce cas une structure if-then-else classique
```

### Avantages de l'opérateur ternaire

**1. Code plus concis**
```pascal
// 4 lignes → 1 ligne
Texte := if Condition then 'Oui' else 'Non';
```

**2. Meilleure lisibilité (pour cas simples)**
```pascal
// Plus clair pour des assignations conditionnelles simples
Button1.Caption := if Editing then 'Enregistrer' else 'Modifier';
```

**3. Utilisable dans les expressions**
```pascal
// Peut être utilisé directement dans un calcul
Total := Quantite * (if EstMembre then PrixMembre else PrixNormal);
```

**4. Moins d'erreurs**
```pascal
// Une seule assignation = moins de risque d'oubli
Status := if Validated then 'OK' else 'KO';
```

---

## 4.14.3 Nouveaux styles VCL Windows 11

### Styles intégrés

Delphi 13 inclut de nouveaux styles spécialement conçus pour Windows 11.

**Liste des nouveaux styles :**
```pascal
const
  NOUVEAUX_STYLES_DELPHI13: array[0..3] of string = (
    'Windows11 Modern Light',   // Thème clair Windows 11
    'Windows11 Modern Dark',    // Thème sombre Windows 11
    'Windows11 Polar Light',    // Variante claire avec bleus
    'Windows11 Mica Dark'       // Thème sombre avec effet Mica
  );
```

### Caractéristiques des nouveaux styles

**Windows11 Modern Light :**
```
Caractéristiques :
- Fond blanc éclatant
- Accents bleus modernes
- Ombres subtiles
- Coins arrondis natifs
- Parfait pour applications professionnelles
```

**Windows11 Modern Dark :**
```
Caractéristiques :
- Fond gris foncé (#202020)
- Excellents contrastes
- Moins de fatigue oculaire
- Design épuré
- Idéal pour travail prolongé
```

**Windows11 Polar Light :**
```
Caractéristiques :
- Tons bleus/gris clairs
- Ambiance froide et claire
- Contraste doux
- Adapté aux applications créatives
```

**Windows11 Mica Dark :**
```
Caractéristiques :
- Effet Mica intégré
- Transparence dynamique
- Suit la couleur du bureau
- Ultra moderne
```

### Application des nouveaux styles

```pascal
uses
  Vcl.Themes;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Méthode 1 : Style spécifique
  if TStyleManager.IsValidStyle('Windows11 Modern Dark') then
    TStyleManager.SetStyle('Windows11 Modern Dark');

  // Méthode 2 : Auto-détection du thème système
  AppliquerStyleSysteme;
end;

procedure TFormMain.AppliquerStyleSysteme;  
var  
  StyleName: string;
begin
  // Détecter le thème Windows
  if SystemUseDarkMode then
    StyleName := 'Windows11 Modern Dark'
  else
    StyleName := 'Windows11 Modern Light';

  if TStyleManager.IsValidStyle(StyleName) then
    TStyleManager.SetStyle(StyleName);
end;

function SystemUseDarkMode: Boolean;  
var  
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
        Result := Reg.ReadInteger('AppsUseLightTheme') = 0;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
```

### Amélioration du rendu des styles

Delphi 13 améliore le rendu des styles VCL avec :
- Meilleure performance (jusqu'à 30% plus rapide)
- Moins d'artefacts visuels
- Transitions plus fluides
- Support DPI amélioré

---

## 4.14.4 Support LLDB v12

### Qu'est-ce que LLDB ?

LLDB (Low Level Debugger) est un débogueur moderne et puissant intégré à Delphi 13.

**Avantages de LLDB v12 :**
- Débogage plus rapide
- Meilleure inspection des variables
- Support multi-plateforme amélioré
- Visualisation de données avancée
- Points d'arrêt conditionnels améliorés

### Nouvelles fonctionnalités de débogage

**1. Inspection de variables améliorée**

```pascal
// Dans le débogueur, vous pouvez maintenant :

// Voir le contenu complet des strings longues
var
  LongText: string;
begin
  LongText := 'Texte très long...'; // LLDB v12 affiche tout
  // Delphi 12 et antérieur : tronqué à 255 caractères
  // Delphi 13 LLDB v12 : affichage complet
end;

// Inspecter des structures complexes
type
  TPersonne = record
    Nom: string;
    Age: Integer;
    Adresse: record
      Rue: string;
      Ville: string;
      CodePostal: string;
    end;
  end;

// LLDB v12 affiche toute la hiérarchie clairement
```

**2. Évaluation d'expressions à la volée**

```pascal
// Pendant le débogage, vous pouvez évaluer des expressions complexes
// dans la fenêtre "Évaluer/Modifier"

// Exemple : Calculer dynamiquement
// Expression : List.Count * 2 + Offset
// LLDB v12 évalue en temps réel

// Exemple : Appeler des méthodes
// Expression : MyObject.GetTotalPrice(true)
// Fonctionne maintenant dans plus de cas
```

**3. Points d'arrêt conditionnels avancés**

```pascal
procedure TForm1.TraiterListe;  
var  
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    // Point d'arrêt conditionnel :
    // Condition : (i > 100) and (List[i].Status = 'Error')
    // S'arrête uniquement quand la condition est vraie

    ProcessItem(List[i]);
  end;
end;
```

**4. Surveillance de mémoire**

```pascal
// LLDB v12 permet de surveiller les adresses mémoire
// Utile pour détecter les corruptions

var
  Buffer: array[0..99] of Byte;
begin
  // Configurer une surveillance mémoire sur Buffer
  // Le débogueur s'arrête si cette zone est modifiée
end;
```

### Configuration LLDB dans Delphi 13

```
1. Outils → Options
2. Débogueur → Options du débogueur
3. Cocher "Utiliser LLDB pour le débogage"
4. OK

Pour iOS/macOS, LLDB est utilisé automatiquement  
Pour Windows, vous pouvez choisir entre Classic et LLDB  
```

---

## 4.14.5 GetIt Package Manager amélioré

### Nouveautés du GetIt

Le gestionnaire de packages GetIt a été considérablement amélioré dans Delphi 13.

**Améliorations principales :**
- Interface redessinée plus moderne
- Recherche plus rapide et précise
- Gestion des versions améliorée
- Mises à jour automatiques
- Meilleure résolution des dépendances

### Interface modernisée

```
GetIt Package Manager - Delphi 13
┌────────────────────────────────────────┐
│ 🔍 Rechercher...            [≡] Filtrer│
├────────────────────────────────────────┤
│ Catégories          Packages           │
│ ├─📦 Composants UI  ┌────────────────┐ │
│ ├─🔧 Utilitaires    │ TMS VCL        │ │
│ ├─🌐 Réseau         │ ★★★★★ (500) │ │
│ ├─📊 Données        │ Version 12.1   │ │
│ └─🎨 Graphisme      │ [Installer]    │ │
│                     └────────────────┘ │
│                                        │
│ Description :                          │
│ Bibliothèque de composants VCL...      │
└────────────────────────────────────────┘
```

### Gestion des versions

```pascal
// GetIt gère maintenant plusieurs versions d'un même package

// Exemple : FireDAC
Package: FireDAC
├─ Version 28.0 (Delphi 13) ✓ Installée
├─ Version 27.0 (Delphi 12)
└─ Version 26.0 (Delphi 11)

// Vous pouvez :
// - Mettre à jour automatiquement
// - Revenir à une version précédente
// - Installer plusieurs versions (si compatible)
```

### Installation simplifiée

**Avant Delphi 13 :**
```
1. Télécharger manuellement
2. Décompresser
3. Compiler les packages
4. Installer dans l'IDE
5. Configurer les chemins
```

**Avec Delphi 13 GetIt :**
```
1. Rechercher le package
2. Cliquer sur "Installer"
→ Tout est automatique !
```

### Exemple d'utilisation

```pascal
// Une fois un package installé via GetIt, il est immédiatement disponible

uses
  // Package installé via GetIt
  TMS.VCL.Grid,
  TMS.VCL.Controls;

procedure TForm1.FormCreate(Sender: TObject);  
var  
  Grid: TTMSFMXGrid;
begin
  // Utiliser le composant installé
  Grid := TTMSFMXGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
end;
```

### Mises à jour automatiques

```pascal
// GetIt peut vérifier automatiquement les mises à jour

procedure TFormMain.VerifierMisesAJour;  
begin  
  // Dans Outils → GetIt Package Manager
  // Onglet "Mises à jour"
  // Liste tous les packages ayant des nouvelles versions

  // Installation des mises à jour en un clic
end;
```

---

## 4.14.6 Site web companion IA

### Assistance IA intégrée

Delphi 13 introduit un site web companion avec assistance IA pour les développeurs.

**Fonctionnalités :**
- Génération de code
- Explication de code
- Détection de bugs
- Suggestions d'optimisation
- Aide à la documentation

**Accès :**
```
1. Depuis l'IDE : Menu Aide → Companion IA
2. Ou directement : https://delphi-ai.embarcadero.com
3. Connexion avec compte Embarcadero
```

### Exemples d'utilisation

**1. Génération de code**

```
Prompt : "Créer une classe TClient avec nom, prénom, email"

Réponse IA :
```

```pascal
type
  TClient = class
  private
    FNom: string;
    FPrenom: string;
    FEmail: string;
    procedure SetNom(const Value: string);
    procedure SetPrenom(const Value: string);
    procedure SetEmail(const Value: string);
  public
    constructor Create(const ANom, APrenom, AEmail: string);
    property Nom: string read FNom write SetNom;
    property Prenom: string read FPrenom write SetPrenom;
    property Email: string read FEmail write SetEmail;
    function EstEmailValide: Boolean;
    function NomComplet: string;
  end;

implementation

constructor TClient.Create(const ANom, APrenom, AEmail: string);  
begin  
  FNom := ANom;
  FPrenom := APrenom;
  FEmail := AEmail;
end;

procedure TClient.SetNom(const Value: string);  
begin  
  if Trim(Value) = '' then
    raise Exception.Create('Le nom ne peut pas être vide');
  FNom := Value;
end;

procedure TClient.SetPrenom(const Value: string);  
begin  
  if Trim(Value) = '' then
    raise Exception.Create('Le prénom ne peut pas être vide');
  FPrenom := Value;
end;

procedure TClient.SetEmail(const Value: string);  
begin  
  if not EstEmailValide then
    raise Exception.Create('Email invalide');
  FEmail := Value;
end;

function TClient.EstEmailValide: Boolean;  
begin  
  Result := (Pos('@', FEmail) > 0) and (Pos('.', FEmail) > 0);
end;

function TClient.NomComplet: string;  
begin  
  Result := Format('%s %s', [FPrenom, FNom]);
end;
```

**2. Explication de code**

```
Code soumis :
```

```pascal
function BinarySearch(A: array of Integer; X: Integer): Integer;  
var  
  L, R, M: Integer;
begin
  L := Low(A);
  R := High(A);
  Result := -1;
  while L <= R do
  begin
    M := (L + R) div 2;
    if A[M] = X then
      Exit(M)
    else if A[M] < X then
      L := M + 1
    else
      R := M - 1;
  end;
end;
```

```
Explication IA :  
Cette fonction implémente l'algorithme de recherche binaire.  

Fonctionnement :
1. Initialise les bornes gauche (L) et droite (R)
2. Tant que la zone de recherche n'est pas vide :
   - Calcule le milieu (M)
   - Compare l'élément du milieu avec la valeur recherchée
   - Ajuste les bornes selon le résultat
3. Retourne l'index si trouvé, -1 sinon

Complexité : O(log n)  
Prérequis : Le tableau doit être trié  
```

**3. Détection de bugs**

```
Code avec problème :
```

```pascal
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  Liste.Add('Item 1');
  Liste.Add('Item 2');
  // Oubli : Liste.Free;
end;
```

```
Alerte IA :
⚠️ Fuite mémoire détectée !

Problème : L'objet 'Liste' est créé mais jamais libéré.

Solution recommandée :
```

```pascal
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    Liste.Add('Item 1');
    Liste.Add('Item 2');
  finally
    Liste.Free; // ✓ Libération garantie
  end;
end;
```

**4. Suggestions d'optimisation**

```
Code à optimiser :
```

```pascal
function CompterVoyelles(const Texte: string): Integer;  
var  
  i: Integer;
  c: Char;
begin
  Result := 0;
  for i := 1 to Length(Texte) do
  begin
    c := LowerCase(Texte[i]);
    if (c = 'a') or (c = 'e') or (c = 'i') or (c = 'o') or (c = 'u') or (c = 'y') then
      Inc(Result);
  end;
end;
```

```
Suggestion IA :  
Cette fonction peut être optimisée :  

1. Appel de LowerCase à chaque itération (coûteux)
2. Comparaisons multiples

Version optimisée :
```

```pascal
function CompterVoyelles(const Texte: string): Integer;  
const  
  VOYELLES = ['a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];
var
  c: Char;
begin
  Result := 0;
  for c in Texte do
    if CharInSet(c, VOYELLES) then
      Inc(Result);
end;

// Avantages :
// - Pas d'appel de fonction dans la boucle
// - Utilisation d'un set (plus rapide)
// - Gère directement majuscules et minuscules
// - Performance améliorée d'environ 3x
```

---

## 4.14.7 Améliorations des composants VCL

### TButton amélioré

**Nouvelles propriétés :**

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Style moderne
  Button1.StyleElements := [seFont, seClient, seBorder];

  // Images avec alignement
  Button1.Images := ImageList1;
  Button1.ImageIndex := 0;
  Button1.ImageAlignment := iaLeft;  // Nouvelle propriété
  Button1.ImageMargins.Left := 4;   // Marges de l'image

  // Espacement du texte
  Button1.Spacing := 8; // Espace entre image et texte

  // États visuels améliorés
  Button1.HotImageIndex := 1;  // Image au survol
  Button1.PressedImageIndex := 2; // Image pressée
  Button1.DisabledImageIndex := 3; // Image désactivée
end;
```

### TEdit amélioré

**Nouvelles fonctionnalités :**

```pascal
procedure TForm1.ConfigurerEdit;  
begin  
  // Texte d'indication (placeholder)
  Edit1.TextHint := 'Entrez votre nom...';
  Edit1.TextHintColor := clGray;

  // Validation améliorée
  Edit1.NumbersOnly := True; // Nouveauté : chiffres uniquement
  Edit1.CharCase := ecUpperCase; // Majuscules automatiques

  // Icônes intégrées (Delphi 13)
  Edit1.LeftIcon := ImageList1;
  Edit1.LeftIconIndex := 0;
  Edit1.RightIcon := ImageList1;
  Edit1.RightIconIndex := 1;

  // Boutons d'action
  Edit1.RightButton.Visible := True;
  Edit1.RightButton.Glyph.LoadFromFile('clear.png');
  Edit1.RightButton.OnClick := EditClearClick;
end;

procedure TForm1.EditClearClick(Sender: TObject);  
begin  
  Edit1.Clear;
  Edit1.SetFocus;
end;
```

### TListView amélioré

**Performances accrues :**

```pascal
procedure TForm1.RemplirListeOptimisee;  
var  
  i: Integer;
  Item: TListItem;
begin
  // Delphi 13 : BeginUpdate/EndUpdate optimisé
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    // Ajout de 10000 éléments
    for i := 1 to 10000 do
    begin
      Item := ListView1.Items.Add;
      Item.Caption := Format('Ligne %d', [i]);
      Item.SubItems.Add(Format('Valeur %d', [i * 10]));
      Item.SubItems.Add(FormatDateTime('dd/mm/yyyy', Now));
    end;

    // Delphi 13 : Jusqu'à 40% plus rapide qu'avant
  finally
    ListView1.Items.EndUpdate;
  end;
end;

// Nouvelle propriété : Groupes améliorés
procedure TForm1.ConfigurerGroupes;  
begin  
  ListView1.GroupView := True;
  ListView1.Groups.Clear;

  with ListView1.Groups.Add do
  begin
    Header := 'Groupe 1';
    State := [lgsCollapsible]; // Groupe repliable (Delphi 13)
    Collapsed := False;
  end;
end;
```

### TTreeView modernisé

```pascal
procedure TForm1.ConfigurerTreeView;  
begin  
  // Style moderne
  TreeView1.StyleElements := [seFont, seClient, seBorder];

  // Nouvelle propriété : CheckBoxes avec images
  TreeView1.StateImages := ImageList1;

  // Performance : Tri amélioré
  TreeView1.SortType := stText;
  TreeView1.AutoExpand := True; // Expansion automatique

  // Nouvelle méthode : Recherche rapide
  RechercherDansArbre('Recherche');
end;

procedure TForm1.RechercherDansArbre(const Texte: string);  
var  
  Node: TTreeNode;
begin
  // Delphi 13 : Méthode de recherche optimisée
  Node := TreeView1.Items.FindNode(Texte, True, True);
  if Assigned(Node) then
  begin
    Node.MakeVisible;
    TreeView1.Selected := Node;
  end;
end;
```

### TPanel avec effets

```pascal
procedure TForm1.CreerPanelModerne;  
begin  
  // Nouveautés Delphi 13
  Panel1.DoubleBuffered := True; // Amélioration du rendu
  Panel1.ShowCaption := True;
  Panel1.Padding.SetBounds(16, 16, 16, 16);

  // Coins arrondis (si style Windows 11)
  Panel1.BevelEdges := [beLeft, beTop, beRight, beBottom];
  Panel1.BevelKind := bkSoft;

  // Ombre (via style VCL)
  Panel1.StyleElements := [seFont, seClient, seBorder];
end;
```

---

## 4.14.8 Améliorations FireDAC

### Connexions optimisées

```pascal
// Delphi 13 : Pool de connexions amélioré
procedure TDataModule1.ConfigurerConnexion;  
begin  
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=mydb');

  // Nouvelles options Delphi 13
  FDConnection1.Params.Add('Pooled=True');
  FDConnection1.Params.Add('POOL_MaximumItems=50'); // Nouveau : Pool étendu
  FDConnection1.Params.Add('POOL_CleanupTimeout=30000');
  FDConnection1.Params.Add('POOL_ExpireTimeout=90000');

  // Reconnexion automatique améliorée
  FDConnection1.Params.Add('AutoReconnect=True');
  FDConnection1.LoginPrompt := False;
end;
```

### Requêtes asynchrones améliorées

```pascal
procedure TForm1.ExecuterRequeteAsync;  
begin  
  // Delphi 13 : Performances asynchrones améliorées
  FDQuery1.SQL.Text := 'SELECT * FROM customers WHERE country = :country';
  FDQuery1.ParamByName('country').AsString := 'France';

  // Exécution asynchrone avec callback
  FDQuery1.OpenAsync(
    procedure
    begin
      // Appelé quand les données sont prêtes
      TThread.Synchronize(nil,
        procedure
        begin
          LabelNbResultats.Caption := Format('%d résultats',
            [FDQuery1.RecordCount]);
          DBGrid1.DataSource.DataSet := FDQuery1;
        end
      );
    end
  );

  // L'interface reste réactive pendant le chargement
end;
```

### Support JSON amélioré

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Param, System.JSON;

procedure TForm1.ExporterEnJSON;  
var  
  JSONArray: TJSONArray;
begin
  // Delphi 13 : Conversion dataset → JSON optimisée
  JSONArray := FDQuery1.ToJSONArray;

  Memo1.Lines.Text := JSONArray.Format(2); // Formatage avec indentation

  JSONArray.Free;
end;

procedure TForm1.ImporterDepuisJSON;  
var  
  JSONText: string;
begin
  JSONText := Memo1.Lines.Text;

  // Delphi 13 : Conversion JSON → dataset améliorée
  FDMemTable1.LoadFromJSON(JSONText);
  FDMemTable1.Open;

  DBGrid1.DataSource.DataSet := FDMemTable1;
end;
```

---

## 4.14.9 IDE et productivité

### Éditeur de code amélioré

**Nouvelles fonctionnalités :**

**1. Complétion de code intelligente**
```pascal
// Delphi 13 : Complétion contextuelle améliorée

var
  Client: TClient;
begin
  Client := TClient.Cr[Ctrl+Space]
  // Suggère automatiquement :
  // - Create (constructeur)
  // - ClassName (propriété héritée)
  // Avec documentation inline
end;
```

**2. Refactoring étendu**
```
Nouveau : Clic droit → Refactoriser
- Extraire méthode (amélioré)
- Renommer symbole (plus fiable)
- Changer la signature
- Introduire variable locale
- Extraire interface (nouveau)
- Convertir en propriété (nouveau)
```

**3. Navigation améliorée**
```
Raccourcis améliorés :
- Ctrl+Clic : Aller à la définition
- Alt+← : Retour arrière
- Alt+→ : Avancer
- Ctrl+Shift+↑ : Aller à la déclaration
- Ctrl+Shift+↓ : Aller à l'implémentation
- F12 : Basculer Code/Designer (plus rapide)
```

**4. Analyse de code en temps réel**
```pascal
// Delphi 13 : Détection de problèmes en temps réel

procedure Test;  
var  
  x: Integer;
begin
  x := 10;
  // ⚠️ Variable assignée mais jamais utilisée
end;

procedure Test2;  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  Liste.Add('Test');
  // ⚠️ Fuite mémoire potentielle : Liste non libérée
end;
```

### Structure du code améliorée

**Vue Structure modernisée :**
```
Structure - Unit1.pas
├─📦 uses
│  ├─ System.SysUtils
│  ├─ System.Classes
│  └─ Vcl.Forms
├─📋 type
│  └─🎯 TForm1
│     ├─ private
│     │  └─ FValeur: Integer
│     ├─ public
│     │  └─ procedure Traiter;
│     └─ published
│        └─ property Valeur: Integer
└─⚙️ implementation
   ├─ procedure TForm1.Traiter
   └─ function CalculTotal: Double
```

### Recherche améliorée

```
Recherche dans les fichiers (Ctrl+Shift+F) :
- Expressions régulières améliorées
- Recherche dans les résultats
- Filtres avancés (type de fichier, dossier)
- Prévisualisation instantanée
- Remplacement par lot
```

---

## 4.14.10 Performances générales

### Améliorations du compilateur

**Optimisations Delphi 13 :**

```pascal
// 1. Optimisation des strings
// Le compilateur détecte et optimise les opérations sur strings

function ConcatenerStrings(A, B, C: string): string;  
begin  
  // Delphi 13 : Optimisé automatiquement
  Result := A + B + C;
  // Ancien : Plusieurs allocations
  // Nouveau : Une seule allocation
end;

// 2. Inline automatique étendu
// Plus de fonctions sont automatiquement inlinées

function Carre(X: Integer): Integer; inline;  
begin  
  Result := X * X;
end;

// Delphi 13 : Inline même dans des cas complexes

// 3. Optimisation des boucles
// Détection et optimisation de patterns courants

procedure Traiter(const Liste: TArray<Integer>);  
var  
  i, Total: Integer;
begin
  Total := 0;
  for i := 0 to High(Liste) do
    Total := Total + Liste[i];

  // Delphi 13 : Vectorisation automatique si possible
end;
```

### Temps de compilation

```
Améliorations moyennes du temps de compilation :

Petit projet (< 100 units) : -10%  
Projet moyen (100-500 units) : -20%  
Grand projet (> 500 units) : -30%  

Compilation incrémentale améliorée :
- Détection plus fine des dépendances
- Recompilation seulement de ce qui a changé
- Cache de compilation optimisé
```

### Taille des exécutables

```
Optimisations de taille :

Sans smart linking :
- Application simple : ~2 MB
- Application moyenne : ~5-8 MB

Avec smart linking (recommandé) :
- Application simple : ~1.5 MB
- Application moyenne : ~3-5 MB

Réduction moyenne : 25-35%
```

---

## 4.14.11 Comparaison avec Delphi 12

### Tableau récapitulatif

| Fonctionnalité | Delphi 12 | Delphi 13 | Amélioration |
|----------------|-----------|-----------|--------------|
| **Opérateur ternaire** | ❌ Non | ✅ Oui | 🆕 Nouveau |
| **Styles Windows 11** | 2 styles | 4+ styles | +100% |
| **LLDB** | v11 | v12 | Version majeure |
| **GetIt** | Basique | Modernisé | Interface + Versions |
| **Companion IA** | ❌ Non | ✅ Oui | 🆕 Nouveau |
| **Compilation** | Rapide | Plus rapide | -20% temps |
| **TButton** | Standard | Images étendues | +5 propriétés |
| **TEdit** | Basique | Placeholder + Icônes | +6 propriétés |
| **TListView** | OK | Optimisé | +40% performance |
| **FireDAC** | Stable | Pool étendu | +Async amélioré |
| **IDE** | Bon | Excellent | Refactoring++ |
| **DPI** | Support | Support étendu | Windows 11 |

### Migration de Delphi 12 vers 13

**Code compatible à 99% :**
```pascal
// Votre code Delphi 12 fonctionne tel quel dans Delphi 13

// Nouveautés utilisables immédiatement :
// 1. Opérateur ternaire
Status := if Validated then 'OK' else 'KO';

// 2. Nouveaux styles
TStyleManager.SetStyle('Windows11 Modern Dark');

// 3. Nouvelles propriétés des composants
Button1.ImageAlignment := iaLeft;  
Edit1.TextHint := 'Placeholder';  

// Pas de changement breaking !
```

---

## 4.14.12 Bonnes pratiques avec Delphi 13

### 1. Utiliser l'opérateur ternaire judicieusement

```pascal
// ✓ Bon usage : assignations simples
Caption := if IsModified then 'Enregistrer*' else 'Enregistrer';

// ✗ Mauvais usage : logique complexe
Result := if (A and B) or (C and not D) then
            if E > F then X else Y
          else
            if G < H then Z else W;
// → Préférer if-then-else classique
```

### 2. Profiter des nouveaux styles

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Détecter et appliquer le thème système
  AppliquerThemeSysteme;

  // S'assurer que tous les composants suivent le style
  VerifierStyleElements;
end;
```

### 3. Optimiser avec LLDB

```pascal
// Utiliser les points d'arrêt conditionnels
// au lieu de if dans le code

// Au lieu de :
if DebugMode and (i > 100) then
  ShowMessage('Debug'); // ← Code de debug dans prod

// Préférer :
// Point d'arrêt conditionnel : (i > 100)
// Pas de code de debug en production
```

### 4. Maintenir à jour avec GetIt

```
Routine mensuelle :
1. Ouvrir GetIt Package Manager
2. Vérifier les mises à jour
3. Lire les notes de version
4. Mettre à jour si stable
5. Tester l'application
```

### 5. Exploiter le Companion IA

```
Workflow recommandé :
1. Écrire le code manuellement
2. Demander une revue à l'IA
3. Appliquer les suggestions pertinentes
4. Comprendre les changements
5. Améliorer progressivement

Ne pas copier-coller aveuglément !
```

---

## 4.14.13 Ressources et documentation

### Documentation officielle

```
1. DocWiki Embarcadero
   https://docwiki.embarcadero.com/RADStudio/

2. Notes de version Delphi 13
   Documentation complète des nouveautés

3. Exemples de code
   C:\Users\Public\Documents\Embarcadero\Studio\23.0\Samples\

4. Webinaires Embarcadero
   Vidéos de présentation des nouveautés
```

### Communauté

```
Forums et groupes :
- forums.embarcadero.com
- stackoverflow.com (tag: delphi)
- reddit.com/r/delphi
- Discord Delphi Community

Blogs recommandés :
- blog.marcocantu.com
- blogs.embarcadero.com
- delphifeeds.com
```

### Formation continue

```
Rester à jour :
1. Suivre les blogs officiels
2. Participer aux webinaires
3. Tester les nouvelles fonctionnalités
4. Contribuer à la communauté
5. Expérimenter sur des projets tests
```

---

## Conclusion

Delphi 13 Florence représente une évolution majeure de la VCL avec des améliorations significatives dans tous les domaines. De l'opérateur ternaire qui simplifie le code aux nouveaux styles Windows 11 qui modernisent l'apparence, en passant par les outils d'IA qui accélèrent le développement, cette version offre tout ce dont vous avez besoin pour créer des applications modernes et performantes.

### Points clés à retenir

✅ **Opérateur ternaire** - Code plus concis et lisible  
✅ **Styles Windows 11** - Interface moderne et native  
✅ **LLDB v12** - Débogage plus puissant  
✅ **GetIt amélioré** - Gestion de packages simplifiée  
✅ **Companion IA** - Assistance intelligente  
✅ **Composants VCL** - Nouvelles propriétés et performances  
✅ **FireDAC** - Connexions et async optimisés  
✅ **IDE** - Productivité accrue  
✅ **Performance** - Compilation et exécution plus rapides  
✅ **Compatibilité** - Migration facile depuis Delphi 12

### Prochaines étapes

1. **Explorer** les nouvelles fonctionnalités
2. **Tester** l'opérateur ternaire dans votre code
3. **Appliquer** les nouveaux styles Windows 11
4. **Essayer** le Companion IA
5. **Optimiser** vos applications existantes
6. **Partager** vos découvertes avec la communauté

Delphi 13 Florence vous donne tous les outils pour créer les meilleures applications VCL de nouvelle génération ! 🚀

⏭️ [Développement multi-plateforme avec FireMonkey (FMX)](/05-developpement-multi-plateforme-avec-firemonkey/README.md)
