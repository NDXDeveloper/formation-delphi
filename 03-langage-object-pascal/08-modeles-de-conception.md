🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.8 Modèles de conception (Design Patterns)

## Introduction

Les **modèles de conception** (ou **Design Patterns** en anglais) sont des solutions éprouvées et réutilisables à des problèmes courants de conception logicielle. Ce sont comme des recettes de cuisine pour résoudre des problèmes de programmation qui reviennent régulièrement.

## Qu'est-ce qu'un Design Pattern ?

Un design pattern n'est pas un morceau de code que vous pouvez copier-coller. C'est plutôt une **description d'une solution** à un problème récurrent, une sorte de "meilleure pratique" qui a fait ses preuves.

### Analogie du monde réel

Imaginez que vous construisez une maison :
- Vous n'inventez pas comment construire un escalier à chaque fois
- Il existe des **modèles d'escaliers** éprouvés (en colimaçon, droit, tournant...)
- Ces modèles ont été raffinés au fil des années
- Vous choisissez et adaptez le modèle qui convient à votre situation

Les design patterns fonctionnent de la même manière en programmation : ce sont des solutions éprouvées que vous adaptez à vos besoins spécifiques.

## Pourquoi utiliser des Design Patterns ?

### Avantages

1. **Communication facilitée** : les patterns ont des noms reconnus. Dire "j'utilise un Singleton" est plus rapide que d'expliquer tout le mécanisme.

2. **Solutions éprouvées** : au lieu de réinventer la roue, vous utilisez ce qui fonctionne déjà.

3. **Code plus maintenable** : les patterns créent du code structuré et facile à comprendre.

4. **Gain de temps** : vous ne perdez pas de temps à chercher comment résoudre un problème déjà résolu.

5. **Meilleure conception** : les patterns vous aident à concevoir des applications robustes et évolutives.

### Mise en garde

Les design patterns ne sont pas :
- Une solution miracle à tous les problèmes
- À utiliser à tout prix (pas de "sur-ingénierie")
- Un substitut à la réflexion et à la conception

**Règle d'or** : utilisez un pattern seulement s'il résout vraiment un problème que vous avez.

## Catégories de Design Patterns

Les design patterns sont généralement classés en trois catégories :

### Patterns de création
Concernent la manière de créer des objets.
- Singleton
- Factory
- Builder

### Patterns de structure
Concernent la composition et l'organisation des classes et objets.
- Adapter
- Decorator
- Facade

### Patterns de comportement
Concernent les interactions et responsabilités entre objets.
- Observer
- Strategy
- Template Method

## Pattern Singleton

### Problème
Vous avez besoin qu'une classe ait **une seule instance** dans toute l'application, accessible globalement.

### Exemples concrets
- Configuration de l'application (on ne veut qu'une seule configuration)
- Gestionnaire de logs (un seul fichier de log)
- Connexion à la base de données (une seule connexion partagée)

### Solution

```pascal
type
  TConfiguration = class
  private
    class var FInstance: TConfiguration;
    FCheminFichiers: string;
    FLangue: string;
    constructor Create;  // Constructeur privé !
  public
    destructor Destroy; override;
    class function Instance: TConfiguration;
    class procedure LibererInstance;

    property CheminFichiers: string read FCheminFichiers write FCheminFichiers;
    property Langue: string read FLangue write FLangue;
  end;

constructor TConfiguration.Create;
begin
  inherited Create;
  FCheminFichiers := 'C:\MesDocuments';
  FLangue := 'FR';
end;

destructor TConfiguration.Destroy;
begin
  inherited Destroy;
end;

class function TConfiguration.Instance: TConfiguration;
begin
  if FInstance = nil then
    FInstance := TConfiguration.Create;
  Result := FInstance;
end;

class procedure TConfiguration.LibererInstance;
begin
  FreeAndNil(FInstance);
end;
```

### Utilisation

```pascal
begin
  // Partout dans l'application, on accède à la même instance
  TConfiguration.Instance.Langue := 'EN';
  ShowMessage('Langue : ' + TConfiguration.Instance.Langue);

  // Ailleurs dans l'application
  ShowMessage('Langue : ' + TConfiguration.Instance.Langue);  // Affiche 'EN'

  // À la fin de l'application
  TConfiguration.LibererInstance;
end;
```

### Avantages et inconvénients

**✅ Avantages** :
- Une seule instance garantie
- Accès global facile
- Initialisation paresseuse (créé seulement quand nécessaire)

**⚠️ Inconvénients** :
- Peut être difficile à tester
- Crée un couplage fort
- À utiliser avec modération

## Pattern Factory (Fabrique)

### Problème
Vous voulez créer des objets sans spécifier leur classe exacte, en laissant une fabrique décider quelle classe instancier.

### Exemples concrets
- Créer différents types de documents (PDF, Word, Excel)
- Créer différents types de notifications (Email, SMS, Push)
- Créer différents moyens de paiement (Carte, PayPal, Virement)

### Solution

```pascal
type
  // Classe de base abstraite
  TDocument = class
  public
    procedure Ouvrir; virtual; abstract;
    procedure Sauvegarder; virtual; abstract;
  end;

  // Classes concrètes
  TDocumentPDF = class(TDocument)
  public
    procedure Ouvrir; override;
    procedure Sauvegarder; override;
  end;

  TDocumentWord = class(TDocument)
  public
    procedure Ouvrir; override;
    procedure Sauvegarder; override;
  end;

  TDocumentExcel = class(TDocument)
  public
    procedure Ouvrir; override;
    procedure Sauvegarder; override;
  end;

  // Type d'énumération
  TTypeDocument = (tdPDF, tdWord, tdExcel);

  // Fabrique
  TDocumentFactory = class
  public
    class function CreerDocument(TypeDoc: TTypeDocument): TDocument;
  end;

// Implémentations
procedure TDocumentPDF.Ouvrir;
begin
  ShowMessage('Ouverture du PDF');
end;

procedure TDocumentPDF.Sauvegarder;
begin
  ShowMessage('Sauvegarde du PDF');
end;

procedure TDocumentWord.Ouvrir;
begin
  ShowMessage('Ouverture du document Word');
end;

procedure TDocumentWord.Sauvegarder;
begin
  ShowMessage('Sauvegarde du document Word');
end;

procedure TDocumentExcel.Ouvrir;
begin
  ShowMessage('Ouverture du classeur Excel');
end;

procedure TDocumentExcel.Sauvegarder;
begin
  ShowMessage('Sauvegarde du classeur Excel');
end;

// La fabrique décide quelle classe créer
class function TDocumentFactory.CreerDocument(TypeDoc: TTypeDocument): TDocument;
begin
  case TypeDoc of
    tdPDF:   Result := TDocumentPDF.Create;
    tdWord:  Result := TDocumentWord.Create;
    tdExcel: Result := TDocumentExcel.Create;
  else
    raise Exception.Create('Type de document inconnu');
  end;
end;
```

### Utilisation

```pascal
var
  Document: TDocument;
begin
  // On ne sait pas à l'avance quel type de document
  Document := TDocumentFactory.CreerDocument(tdPDF);
  try
    Document.Ouvrir;
    Document.Sauvegarder;
  finally
    Document.Free;
  end;

  // Facile de changer le type
  Document := TDocumentFactory.CreerDocument(tdWord);
  try
    Document.Ouvrir;
  finally
    Document.Free;
  end;
end;
```

### Avantages

**✅ Avantages** :
- Séparation entre création et utilisation
- Facile d'ajouter de nouveaux types
- Code client plus simple
- Centralisation de la logique de création

## Pattern Observer (Observateur)

### Problème
Un objet (le sujet) doit notifier automatiquement plusieurs autres objets (les observateurs) quand son état change, sans créer un couplage fort entre eux.

### Exemples concrets
- Notification de plusieurs écrans quand les données changent
- Système d'événements
- Mise à jour de plusieurs vues d'un même modèle

### Solution

```pascal
type
  // Interface pour les observateurs
  IObservateur = interface
    ['{1A2B3C4D-5E6F-7A8B-9C0D-1E2F3A4B5C6D}']
    procedure MettreAJour(const Message: string);
  end;

  // Sujet observable
  TDonneesStock = class
  private
    FObservateurs: TList<IObservateur>;
    FQuantite: Integer;
    procedure NotifierObservateurs(const Message: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterObservateur(Observateur: IObservateur);
    procedure RetirerObservateur(Observateur: IObservateur);
    procedure ModifierQuantite(NouvelleQuantite: Integer);
    property Quantite: Integer read FQuantite;
  end;

  // Observateur concret : Affichage
  TAffichageStock = class(TInterfacedObject, IObservateur)
  private
    FNom: string;
  public
    constructor Create(const ANom: string);
    procedure MettreAJour(const Message: string);
  end;

  // Observateur concret : Alerte
  TAlerteStock = class(TInterfacedObject, IObservateur)
  public
    procedure MettreAJour(const Message: string);
  end;

// Implémentation TDonneesStock
constructor TDonneesStock.Create;
begin
  inherited Create;
  FObservateurs := TList<IObservateur>.Create;
  FQuantite := 100;
end;

destructor TDonneesStock.Destroy;
begin
  FObservateurs.Free;
  inherited Destroy;
end;

procedure TDonneesStock.AjouterObservateur(Observateur: IObservateur);
begin
  FObservateurs.Add(Observateur);
end;

procedure TDonneesStock.RetirerObservateur(Observateur: IObservateur);
begin
  FObservateurs.Remove(Observateur);
end;

procedure TDonneesStock.NotifierObservateurs(const Message: string);
var
  Observateur: IObservateur;
begin
  for Observateur in FObservateurs do
    Observateur.MettreAJour(Message);
end;

procedure TDonneesStock.ModifierQuantite(NouvelleQuantite: Integer);
begin
  FQuantite := NouvelleQuantite;
  NotifierObservateurs(Format('Stock modifié : %d unités', [FQuantite]));
end;

// Implémentation TAffichageStock
constructor TAffichageStock.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
end;

procedure TAffichageStock.MettreAJour(const Message: string);
begin
  ShowMessage(Format('[%s] %s', [FNom, Message]));
end;

// Implémentation TAlerteStock
procedure TAlerteStock.MettreAJour(const Message: string);
begin
  ShowMessage('[ALERTE] ' + Message);
end;
```

### Utilisation

```pascal
var
  Stock: TDonneesStock;
  Affichage1, Affichage2: IObservateur;
  Alerte: IObservateur;
begin
  Stock := TDonneesStock.Create;
  try
    // Créer les observateurs
    Affichage1 := TAffichageStock.Create('Écran principal');
    Affichage2 := TAffichageStock.Create('Écran secondaire');
    Alerte := TAlerteStock.Create;

    // Enregistrer les observateurs
    Stock.AjouterObservateur(Affichage1);
    Stock.AjouterObservateur(Affichage2);
    Stock.AjouterObservateur(Alerte);

    // Quand le stock change, tous les observateurs sont notifiés
    Stock.ModifierQuantite(50);
    // Tous les observateurs reçoivent la notification automatiquement !

  finally
    Stock.Free;
  end;
end;
```

### Avantages

**✅ Avantages** :
- Couplage faible entre sujet et observateurs
- Facile d'ajouter de nouveaux observateurs
- Les observateurs peuvent s'enregistrer/se retirer dynamiquement

## Pattern Strategy (Stratégie)

### Problème
Vous avez plusieurs algorithmes pour faire la même chose et vous voulez pouvoir changer facilement d'algorithme à l'exécution.

### Exemples concrets
- Différents algorithmes de tri
- Différentes méthodes de calcul de prix (normal, réduit, membre VIP)
- Différents modes de paiement

### Solution

```pascal
type
  // Interface pour la stratégie
  IStrategieCalculPrix = interface
    ['{7F8E9D0C-1B2A-3D4E-5F6A-7B8C9D0E1F2A}']
    function CalculerPrix(PrixBase: Double): Double;
  end;

  // Stratégie : Prix normal
  TStrategiePrixNormal = class(TInterfacedObject, IStrategieCalculPrix)
  public
    function CalculerPrix(PrixBase: Double): Double;
  end;

  // Stratégie : Prix réduit
  TStrategiePrixReduit = class(TInterfacedObject, IStrategieCalculPrix)
  private
    FPourcentageReduction: Double;
  public
    constructor Create(APourcentageReduction: Double);
    function CalculerPrix(PrixBase: Double): Double;
  end;

  // Stratégie : Prix membre VIP
  TStrategiePrixVIP = class(TInterfacedObject, IStrategieCalculPrix)
  public
    function CalculerPrix(PrixBase: Double): Double;
  end;

  // Contexte qui utilise une stratégie
  TCalculateurPrix = class
  private
    FStrategie: IStrategieCalculPrix;
  public
    constructor Create(AStrategie: IStrategieCalculPrix);
    procedure DefinirStrategie(AStrategie: IStrategieCalculPrix);
    function Calculer(PrixBase: Double): Double;
  end;

// Implémentations
function TStrategiePrixNormal.CalculerPrix(PrixBase: Double): Double;
begin
  Result := PrixBase;
end;

constructor TStrategiePrixReduit.Create(APourcentageReduction: Double);
begin
  inherited Create;
  FPourcentageReduction := APourcentageReduction;
end;

function TStrategiePrixReduit.CalculerPrix(PrixBase: Double): Double;
begin
  Result := PrixBase * (1 - FPourcentageReduction / 100);
end;

function TStrategiePrixVIP.CalculerPrix(PrixBase: Double): Double;
begin
  // VIP : 25% de réduction + livraison gratuite
  Result := PrixBase * 0.75;
end;

constructor TCalculateurPrix.Create(AStrategie: IStrategieCalculPrix);
begin
  inherited Create;
  FStrategie := AStrategie;
end;

procedure TCalculateurPrix.DefinirStrategie(AStrategie: IStrategieCalculPrix);
begin
  FStrategie := AStrategie;
end;

function TCalculateurPrix.Calculer(PrixBase: Double): Double;
begin
  Result := FStrategie.CalculerPrix(PrixBase);
end;
```

### Utilisation

```pascal
var
  Calculateur: TCalculateurPrix;
  PrixBase: Double;
begin
  PrixBase := 100.0;

  // Client normal
  Calculateur := TCalculateurPrix.Create(TStrategiePrixNormal.Create);
  try
    ShowMessage(Format('Prix normal : %.2f €', [Calculateur.Calculer(PrixBase)]));

    // Changer pour client avec réduction
    Calculateur.DefinirStrategie(TStrategiePrixReduit.Create(10));
    ShowMessage(Format('Prix réduit : %.2f €', [Calculateur.Calculer(PrixBase)]));

    // Changer pour client VIP
    Calculateur.DefinirStrategie(TStrategiePrixVIP.Create);
    ShowMessage(Format('Prix VIP : %.2f €', [Calculateur.Calculer(PrixBase)]));
  finally
    Calculateur.Free;
  end;
end;
```

### Avantages

**✅ Avantages** :
- Changement d'algorithme à l'exécution
- Évite les structures if/case complexes
- Facile d'ajouter de nouvelles stratégies
- Chaque stratégie est testable indépendamment

## Pattern Decorator (Décorateur)

### Problème
Vous voulez ajouter des fonctionnalités à un objet dynamiquement, sans modifier sa classe.

### Exemples concrets
- Ajouter des options à un café (lait, sucre, chocolat)
- Ajouter des bordures, défilement à un composant visuel
- Ajouter des fonctionnalités à un flux de données (compression, chiffrement)

### Solution

```pascal
type
  // Interface de base
  IBoisson = interface
    ['{9B8C7D6E-5F4A-3B2C-1D0E-9F8A7B6C5D4E}']
    function ObtenirDescription: string;
    function ObtenirPrix: Double;
  end;

  // Boisson de base
  TCafe = class(TInterfacedObject, IBoisson)
  public
    function ObtenirDescription: string;
    function ObtenirPrix: Double;
  end;

  // Décorateur abstrait
  TDecorateurBoisson = class(TInterfacedObject, IBoisson)
  protected
    FBoisson: IBoisson;
  public
    constructor Create(ABoisson: IBoisson);
    function ObtenirDescription: string; virtual;
    function ObtenirPrix: Double; virtual;
  end;

  // Décorateurs concrets
  TAvecLait = class(TDecorateurBoisson)
  public
    function ObtenirDescription: string; override;
    function ObtenirPrix: Double; override;
  end;

  TAvecChocolat = class(TDecorateurBoisson)
  public
    function ObtenirDescription: string; override;
    function ObtenirPrix: Double; override;
  end;

  TAvecCreme = class(TDecorateurBoisson)
  public
    function ObtenirDescription: string; override;
    function ObtenirPrix: Double; override;
  end;

// Implémentation TCafe
function TCafe.ObtenirDescription: string;
begin
  Result := 'Café';
end;

function TCafe.ObtenirPrix: Double;
begin
  Result := 2.50;
end;

// Implémentation TDecorateurBoisson
constructor TDecorateurBoisson.Create(ABoisson: IBoisson);
begin
  inherited Create;
  FBoisson := ABoisson;
end;

function TDecorateurBoisson.ObtenirDescription: string;
begin
  Result := FBoisson.ObtenirDescription;
end;

function TDecorateurBoisson.ObtenirPrix: Double;
begin
  Result := FBoisson.ObtenirPrix;
end;

// Implémentation TAvecLait
function TAvecLait.ObtenirDescription: string;
begin
  Result := FBoisson.ObtenirDescription + ' + Lait';
end;

function TAvecLait.ObtenirPrix: Double;
begin
  Result := FBoisson.ObtenirPrix + 0.50;
end;

// Implémentation TAvecChocolat
function TAvecChocolat.ObtenirDescription: string;
begin
  Result := FBoisson.ObtenirDescription + ' + Chocolat';
end;

function TAvecChocolat.ObtenirPrix: Double;
begin
  Result := FBoisson.ObtenirPrix + 0.70;
end;

// Implémentation TAvecCreme
function TAvecCreme.ObtenirDescription: string;
begin
  Result := FBoisson.ObtenirDescription + ' + Crème';
end;

function TAvecCreme.ObtenirPrix: Double;
begin
  Result := FBoisson.ObtenirPrix + 0.60;
end;
```

### Utilisation

```pascal
var
  MaBoisson: IBoisson;
begin
  // Café simple
  MaBoisson := TCafe.Create;
  ShowMessage(Format('%s : %.2f €',
    [MaBoisson.ObtenirDescription, MaBoisson.ObtenirPrix]));

  // Café avec lait
  MaBoisson := TAvecLait.Create(TCafe.Create);
  ShowMessage(Format('%s : %.2f €',
    [MaBoisson.ObtenirDescription, MaBoisson.ObtenirPrix]));

  // Café avec lait et chocolat
  MaBoisson := TAvecChocolat.Create(TAvecLait.Create(TCafe.Create));
  ShowMessage(Format('%s : %.2f €',
    [MaBoisson.ObtenirDescription, MaBoisson.ObtenirPrix]));

  // Café avec lait, chocolat et crème
  MaBoisson := TAvecCreme.Create(
                 TAvecChocolat.Create(
                   TAvecLait.Create(TCafe.Create)));
  ShowMessage(Format('%s : %.2f €',
    [MaBoisson.ObtenirDescription, MaBoisson.ObtenirPrix]));
end;
```

### Avantages

**✅ Avantages** :
- Ajouter des fonctionnalités sans modifier le code existant
- Combinaison flexible de fonctionnalités
- Alternative à l'héritage multiple
- Respect du principe ouvert/fermé

## Pattern Adapter (Adaptateur)

### Problème
Vous avez deux classes avec des interfaces incompatibles et vous voulez qu'elles puissent travailler ensemble.

### Exemples concrets
- Adapter une ancienne bibliothèque à une nouvelle interface
- Connecter un système européen à un système américain
- Faire fonctionner ensemble des composants de fournisseurs différents

### Solution

```pascal
type
  // Interface cible attendue par le client
  ILecteurAudio = interface
    ['{5A6B7C8D-9E0F-1A2B-3C4D-5E6F7A8B9C0D}']
    procedure Lire(NomFichier: string);
  end;

  // Classe existante avec interface incompatible
  TLecteurMP3Ancien = class
  public
    procedure LireMP3(Chemin: string);
  end;

  // Adaptateur
  TAdaptateurMP3 = class(TInterfacedObject, ILecteurAudio)
  private
    FLecteurAncien: TLecteurMP3Ancien;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lire(NomFichier: string);
  end;

  // Nouveau lecteur qui implémente directement l'interface
  TLecteurMP4 = class(TInterfacedObject, ILecteurAudio)
  public
    procedure Lire(NomFichier: string);
  end;

// Implémentation TLecteurMP3Ancien
procedure TLecteurMP3Ancien.LireMP3(Chemin: string);
begin
  ShowMessage('Lecture du fichier MP3 : ' + Chemin);
end;

// Implémentation TAdaptateurMP3
constructor TAdaptateurMP3.Create;
begin
  inherited Create;
  FLecteurAncien := TLecteurMP3Ancien.Create;
end;

destructor TAdaptateurMP3.Destroy;
begin
  FLecteurAncien.Free;
  inherited Destroy;
end;

procedure TAdaptateurMP3.Lire(NomFichier: string);
begin
  // Adapter l'appel vers l'ancienne interface
  FLecteurAncien.LireMP3(NomFichier);
end;

// Implémentation TLecteurMP4
procedure TLecteurMP4.Lire(NomFichier: string);
begin
  ShowMessage('Lecture du fichier MP4 : ' + NomFichier);
end;
```

### Utilisation

```pascal
procedure UtiliserLecteur(Lecteur: ILecteurAudio; Fichier: string);
begin
  Lecteur.Lire(Fichier);
end;

var
  LecteurMP3: ILecteurAudio;
  LecteurMP4: ILecteurAudio;
begin
  // L'ancien lecteur MP3 fonctionne grâce à l'adaptateur
  LecteurMP3 := TAdaptateurMP3.Create;
  UtiliserLecteur(LecteurMP3, 'musique.mp3');

  // Le nouveau lecteur MP4 fonctionne directement
  LecteurMP4 := TLecteurMP4.Create;
  UtiliserLecteur(LecteurMP4, 'video.mp4');

  // Le code client n'a pas besoin de connaître la différence !
end;
```

### Avantages

**✅ Avantages** :
- Réutilisation de code existant
- Intégration de composants tiers
- Séparation des préoccupations
- Facilite la migration progressive

## Pattern MVC (Model-View-Controller)

### Problème
Vous voulez séparer la logique métier (données), la présentation (interface) et le contrôle (interactions) pour faciliter la maintenance et les tests.

### Composants

1. **Model (Modèle)** : contient les données et la logique métier
2. **View (Vue)** : affiche les données à l'utilisateur
3. **Controller (Contrôleur)** : gère les interactions utilisateur

### Solution simplifiée

```pascal
type
  // MODEL : les données
  TModelProduit = class
  private
    FNom: string;
    FPrix: Double;
    FStock: Integer;
  public
    constructor Create(ANom: string; APrix: Double; AStock: Integer);
    procedure ModifierStock(NouveauStock: Integer);
    property Nom: string read FNom write FNom;
    property Prix: Double read FPrix write FPrix;
    property Stock: Integer read FStock;
  end;

  // VIEW : l'affichage
  TVueProduit = class
  public
    procedure Afficher(Produit: TModelProduit);
    procedure AfficherMessage(const Message: string);
  end;

  // CONTROLLER : la logique de contrôle
  TControleurProduit = class
  private
    FModel: TModelProduit;
    FVue: TVueProduit;
  public
    constructor Create(AModel: TModelProduit; AVue: TVueProduit);
    destructor Destroy; override;
    procedure AfficherProduit;
    procedure AjouterStock(Quantite: Integer);
    procedure RetirerStock(Quantite: Integer);
  end;

// Implémentation TModelProduit
constructor TModelProduit.Create(ANom: string; APrix: Double; AStock: Integer);
begin
  inherited Create;
  FNom := ANom;
  FPrix := APrix;
  FStock := AStock;
end;

procedure TModelProduit.ModifierStock(NouveauStock: Integer);
begin
  if NouveauStock >= 0 then
    FStock := NouveauStock;
end;

// Implémentation TVueProduit
procedure TVueProduit.Afficher(Produit: TModelProduit);
begin
  ShowMessage(Format('Produit : %s'#13#10 +
                     'Prix : %.2f €'#13#10 +
                     'Stock : %d unités',
                     [Produit.Nom, Produit.Prix, Produit.Stock]));
end;

procedure TVueProduit.AfficherMessage(const Message: string);
begin
  ShowMessage(Message);
end;

// Implémentation TControleurProduit
constructor TControleurProduit.Create(AModel: TModelProduit; AVue: TVueProduit);
begin
  inherited Create;
  FModel := AModel;
  FVue := AVue;
end;

destructor TControleurProduit.Destroy;
begin
  FModel.Free;
  FVue.Free;
  inherited Destroy;
end;

procedure TControleurProduit.AfficherProduit;
begin
  FVue.Afficher(FModel);
end;

procedure TControleurProduit.AjouterStock(Quantite: Integer);
begin
  FModel.ModifierStock(FModel.Stock + Quantite);
  FVue.AfficherMessage(Format('%d unités ajoutées', [Quantite]));
end;

procedure TControleurProduit.RetirerStock(Quantite: Integer);
begin
  if FModel.Stock >= Quantite then
  begin
    FModel.ModifierStock(FModel.Stock - Quantite);
    FVue.AfficherMessage(Format('%d unités retirées', [Quantite]));
  end
  else
    FVue.AfficherMessage('Stock insuffisant !');
end;
```

### Utilisation

```pascal
var
  Model: TModelProduit;
  Vue: TVueProduit;
  Controleur: TControleurProduit;
begin
  // Créer les composants
  Model := TModelProduit.Create('Ordinateur portable', 899.99, 10);
  Vue := TVueProduit.Create;
  Controleur := TControleurProduit.Create(Model, Vue);

  try
    // Afficher le produit
    Controleur.AfficherProduit;

    // Ajouter au stock
    Controleur.AjouterStock(5);
    Controleur.AfficherProduit;

    // Retirer du stock
    Controleur.RetirerStock(3);
    Controleur.AfficherProduit;

  finally
    Controleur.Free;
  end;
end;
```

### Avantages

**✅ Avantages** :
- Séparation claire des responsabilités
- Facilite les tests unitaires
- Plusieurs vues pour un même modèle
- Modifications d'interface sans toucher à la logique

## Autres Patterns importants

### Template Method

Définit le squelette d'un algorithme, en laissant les sous-classes redéfinir certaines étapes.

```pascal
type
  TTraitementDocument = class
  public
    procedure Traiter;  // Méthode template
  protected
    procedure OuvrirDocument; virtual; abstract;
    procedure AnalyserContenu; virtual; abstract;
    procedure GenererRapport; virtual; abstract;
    procedure FermerDocument; virtual; abstract;
  end;

procedure TTraitementDocument.Traiter;
begin
  OuvrirDocument;
  AnalyserContenu;
  GenererRapport;
  FermerDocument;
end;
```

### Command

Encapsule une requête comme un objet, permettant de paramétrer, mettre en file d'attente ou annuler des opérations.

```pascal
type
  ICommande = interface
    procedure Executer;
    procedure Annuler;
  end;

  TCommandeSauvegarder = class(TInterfacedObject, ICommande)
  private
    FDocument: TDocument;
  public
    constructor Create(ADocument: TDocument);
    procedure Executer;
    procedure Annuler;
  end;
```

## Quand utiliser quel Pattern ?

| Pattern | Quand l'utiliser |
|---------|------------------|
| **Singleton** | Une seule instance nécessaire (configuration, log) |
| **Factory** | Création d'objets complexe ou variable |
| **Observer** | Notification de changements à plusieurs objets |
| **Strategy** | Plusieurs algorithmes interchangeables |
| **Decorator** | Ajouter des fonctionnalités dynamiquement |
| **Adapter** | Rendre compatibles des interfaces différentes |
| **MVC** | Séparer logique, présentation et contrôle |

## Bonnes pratiques

### 1. Ne pas sur-utiliser les patterns

```pascal
// ❌ Mauvais - pattern inutile pour un cas simple
TFactorySingletonStrategyAdapter...  // Trop complexe !

// ✅ Bon - simple et direct
MaClasse := TMaClasse.Create;
```

### 2. Comprendre le problème avant d'appliquer un pattern

Un pattern est une solution à un problème spécifique. Identifiez d'abord le problème !

### 3. Les patterns évoluent

N'hésitez pas à adapter un pattern à vos besoins spécifiques.

### 4. Nommer clairement

```pascal
// ✅ Bon - le pattern est évident dans le nom
TDocumentFactory
TConfigurationSingleton
TObserverDonnees

// ⚠️ Moins clair
TDoc
TConf
TData
```

### 5. Documenter l'utilisation du pattern

```pascal
/// <summary>
/// Factory pour créer différents types de documents
/// Pattern : Factory
/// </summary>
TDocumentFactory = class
```

## Anti-patterns à éviter

### God Object (Objet Dieu)

Une classe qui fait tout - viole le principe de responsabilité unique.

```pascal
// ❌ Mauvais
TApplication = class
  procedure GererBaseDonnees;
  procedure AfficherInterface;
  procedure EnvoyerEmails;
  procedure CalculerRapports;
  procedure GererReseau;
  // ... fait TOUT !
end;
```

### Spaghetti Code

Code emmêlé sans structure claire.

### Copy-Paste Programming

Dupliquer du code au lieu de le factoriser.

## Patterns spécifiques à Delphi

### DataModule

Un conteneur pour les composants non-visuels (connexions DB, requêtes...).

```pascal
type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
  end;
```

### LiveBindings

Liaison automatique entre composants visuels et données (pattern Observer intégré).

## Ressources pour approfondir

- **Gang of Four (GoF)** : le livre de référence sur les design patterns
- **Refactoring Guru** : site web avec explications et exemples
- **Documentation Delphi** : patterns utilisés dans la VCL/FMX
- **Code source de Delphi** : étudiez comment sont conçues les classes standard

## Résumé

- Les **Design Patterns** sont des solutions éprouvées à des problèmes récurrents

- **Principaux patterns** :
  - **Singleton** : une seule instance
  - **Factory** : création d'objets flexible
  - **Observer** : notification de changements
  - **Strategy** : algorithmes interchangeables
  - **Decorator** : ajout de fonctionnalités
  - **Adapter** : compatibilité d'interfaces
  - **MVC** : séparation des responsabilités

- **Avantages** :
  - Communication facilitée
  - Solutions éprouvées
  - Code maintenable
  - Meilleure conception

- **Règle d'or** : utilisez un pattern seulement s'il résout vraiment un problème

- **Évitez** : la sur-ingénierie, les anti-patterns

Les design patterns sont des outils puissants dans votre boîte à outils de développeur. Avec la pratique, vous reconnaîtrez naturellement les situations où ils sont utiles. Commencez par les patterns les plus simples (Singleton, Factory) avant de vous attaquer aux plus complexes.

⏭️ [Organisation du code source et modularité](/03-langage-object-pascal/09-organisation-du-code-source-et-modularite.md)
