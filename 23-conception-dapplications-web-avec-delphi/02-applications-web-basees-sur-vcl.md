🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.2 Applications Web basées sur VCL

## Introduction

Si vous avez déjà développé des applications desktop avec Delphi, vous connaissez la VCL (Visual Component Library). Cette bibliothèque de composants visuels a fait la réputation de Delphi depuis les années 1990, permettant de créer des interfaces utilisateur de manière rapide et intuitive.

La bonne nouvelle ? Vous pouvez réutiliser une grande partie de vos connaissances VCL pour créer des applications web ! Cette approche est particulièrement intéressante si vous avez :

- Une application VCL existante à moderniser
- Une équipe habituée au développement VCL
- Besoin de créer rapidement une version web d'une application desktop
- Des contraintes de temps limitées

## Qu'est-ce qu'une application web basée sur VCL ?

### Le concept

Une application web basée sur VCL utilise une approche similaire au développement desktop traditionnel :

1. Vous créez des formulaires visuellement dans l'IDE
2. Vous placez des composants (boutons, champs de texte, grilles, etc.)
3. Vous écrivez du code Pascal pour la logique métier
4. Le framework se charge de traduire tout cela en HTML/CSS/JavaScript

**La différence principale :** Votre application s'exécute sur un serveur et génère dynamiquement des pages web au lieu de fenêtres Windows.

### Architecture technique

```
┌──────────────────────────────────────┐
│        Navigateur utilisateur        │
│   (Affiche HTML/CSS/JavaScript)      │
└─────────────┬────────────────────────┘
              │
              │ HTTP/HTTPS
              │ (Requêtes et réponses)
              │
┌─────────────┴────────────────────────┐
│    Serveur Application Delphi        │
│                                      │
│  ┌────────────────────────────────┐  │
│  │  Vos formulaires VCL-like      │  │
│  │  (Code Object Pascal)          │  │
│  └────────────┬───────────────────┘  │
│               │                      │
│  ┌────────────┴───────────────────┐  │
│  │  Framework de conversion       │  │
│  │  (IntraWeb, WebBroker, etc.)   │  │
│  └────────────┬───────────────────┘  │
│               │                      │
│               ↓                      │
│     Génération HTML/CSS/JS           │
└──────────────────────────────────────┘
```

## Les technologies disponibles

### IntraWeb - La solution RAD complète

**IntraWeb** est la solution la plus aboutie pour créer des applications web avec une approche VCL. Elle offre :

- Des composants visuels familiers (IWButton, IWEdit, IWGrid...)
- Un éditeur visuel dans l'IDE Delphi
- Une gestion automatique des sessions utilisateur
- Un mode standalone pour le développement rapide
- Un déploiement ISAPI/Apache pour la production

**Avantages d'IntraWeb :**
- Transition naturelle depuis VCL
- Développement rapide (RAD)
- Composants riches et éprouvés
- Communauté active

**Limitations d'IntraWeb :**
- Nécessite un serveur d'application
- Performance dépendante du réseau
- Interface moins moderne que les frameworks JavaScript natifs

### WebBroker - La technologie de base

**WebBroker** est le framework historique de Delphi pour le web. Plus bas niveau qu'IntraWeb, il offre :

- Gestion directe des requêtes HTTP
- Actions web pour différentes routes
- Génération de HTML via code ou templates
- Support CGI, ISAPI, Apache

**Quand utiliser WebBroker :**
- Pour des applications web simples
- Quand vous avez besoin de contrôle total
- Pour des services web légers
- Apprentissage des fondamentaux du web avec Delphi

### UniGUI - Alternative commerciale

**UniGUI** est une solution commerciale tierce qui offre :

- Framework similaire à IntraWeb mais plus moderne
- Interface utilisateur plus riche (style ExtJS)
- Support AJAX intégré
- Applications web responsive

## Comparaison VCL desktop vs VCL web

### Similitudes

| Aspect | VCL Desktop | VCL Web |
|--------|-------------|---------|
| **Langage** | Object Pascal | Object Pascal |
| **IDE** | Delphi IDE | Delphi IDE |
| **Conception** | Visuelle (drag & drop) | Visuelle (drag & drop) |
| **Événements** | OnClick, OnChange, etc. | OnClick, OnChange, etc. |
| **Propriétés** | Caption, Text, Enabled... | Caption, Text, Enabled... |
| **Code métier** | Procédures/fonctions | Procédures/fonctions |

### Différences clés

| Aspect | VCL Desktop | VCL Web |
|--------|-------------|---------|
| **Exécution** | Sur le PC de l'utilisateur | Sur le serveur |
| **Interface** | Fenêtres natives Windows | Pages HTML dans navigateur |
| **État** | Variables locales persistantes | Sessions côté serveur |
| **Communication** | Messages Windows | Requêtes HTTP |
| **Installation** | Exe à installer | Simple URL |
| **Mises à jour** | Sur chaque poste | Une seule fois sur serveur |

## Migration d'une application VCL vers le web

### Étape 1 : Évaluation de la faisabilité

Avant de commencer, évaluez votre application existante :

**✅ Facile à migrer :**
- Applications formulaires (saisie de données)
- Tableaux de bord et rapports
- Applications de gestion (CRM, ERP...)
- Interfaces CRUD (Create, Read, Update, Delete)

**⚠️ Plus complexe :**
- Applications utilisant massivement GDI/Graphics
- Jeux ou applications multimédia
- Applications avec beaucoup d'interactions temps réel
- Utilisation intensive de composants tiers spécifiques

**❌ Non recommandé :**
- Applications nécessitant accès matériel local (imprimantes spéciales, ports série...)
- Logiciels de CAO/DAO graphiques
- Applications nécessitant performances temps réel critiques

### Étape 2 : Architecture de la migration

Deux approches principales :

**Approche 1 : Migration directe (Big Bang)**
- Recréer l'application entièrement en version web
- Adapter tous les formulaires
- Tester exhaustivement
- Basculer en une fois

**Approche 2 : Migration progressive (Strangulation)**
- Créer d'abord le backend/services
- Migrer module par module
- Cohabitation des deux versions
- Transition en douceur

### Étape 3 : Réutilisation du code

Ce qui peut être réutilisé **tel quel** :

```pascal
// Logique métier pure - OK
function CalculerMontantTTC(MontantHT, TauxTVA: Currency): Currency;  
begin  
  Result := MontantHT * (1 + TauxTVA / 100);
end;

// Classes métier - OK
type
  TClient = class
  private
    FNom: string;
    FPrenom: string;
  public
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    function NomComplet: string;
  end;

// Accès base de données - OK (avec FireDAC)
procedure ChargerClients(Query: TFDQuery);  
begin  
  Query.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
  Query.Open;
end;
```

Ce qui doit être **adapté** :

```pascal
// VCL Desktop - NE FONCTIONNE PAS en web
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ShowMessage('Bonjour !');  // Pas de ShowMessage
  Edit1.SetFocus;            // Pas de SetFocus direct
  Label1.Font.Color := clRed; // Gestion différente des couleurs
end;

// Équivalent IntraWeb - VERSION WEB
procedure TIWForm1.IWButton1Click(Sender: TObject);  
begin  
  WebApplication.ShowMessage('Bonjour !'); // IntraWeb
  // Le focus est géré automatiquement
  IWLabel1.Font.Color := clWebRed; // Couleurs web
end;
```

### Étape 4 : Adaptation des composants

Table de correspondance des composants courants :

| VCL Desktop | IntraWeb | Fonction |
|-------------|----------|----------|
| TForm | TIWForm | Formulaire/page |
| TButton | IWButton | Bouton |
| TEdit | IWEdit | Champ texte |
| TMemo | IWMemo | Zone texte multi-lignes |
| TLabel | IWLabel | Étiquette |
| TComboBox | IWComboBox | Liste déroulante |
| TCheckBox | IWCheckBox | Case à cocher |
| TRadioButton | IWRadioButton | Bouton radio |
| TDBGrid | IWDBGrid | Grille de données |
| TPanel | IWRegion | Conteneur |
| TListBox | IWListBox | Liste |
| TImage | IWImage | Image |

### Étape 5 : Gestion de l'état et des sessions

**Différence majeure :** En VCL desktop, vos variables persistent. En web, vous devez gérer les sessions.

**VCL Desktop :**
```pascal
type
  TForm1 = class(TForm)
  private
    FUtilisateurConnecte: string;  // Persiste automatiquement
    FPanierArticles: TList;        // Reste en mémoire
  public
    // ...
  end;
```

**Version Web (IntraWeb) :**
```pascal
type
  TIWForm1 = class(TIWAppForm)
  private
    function GetUtilisateurConnecte: string;
    procedure SetUtilisateurConnecte(const Value: string);
  public
    // On stocke dans la session utilisateur
    property UtilisateurConnecte: string
      read GetUtilisateurConnecte
      write SetUtilisateurConnecte;
  end;

implementation

function TIWForm1.GetUtilisateurConnecte: string;  
begin  
  // Récupération depuis la session
  Result := WebApplication.Data.Values['UtilisateurConnecte'];
end;

procedure TIWForm1.SetUtilisateurConnecte(const Value: string);  
begin  
  // Stockage dans la session
  WebApplication.Data.Values['UtilisateurConnecte'] := Value;
end;
```

## Patterns et techniques spécifiques

### Pattern 1 : Séparation de la logique métier

**Principe :** Isoler la logique métier de l'interface utilisateur

```pascal
// Unit BusinessLogic.pas - RÉUTILISABLE
unit BusinessLogic;

interface

type
  TFactureManager = class
  public
    class function CalculerTotal(MontantHT, Remise, TVA: Currency): Currency;
    class function ValiderFacture(NumFacture: string): Boolean;
  end;

implementation

class function TFactureManager.CalculerTotal(
  MontantHT, Remise, TVA: Currency): Currency;
begin
  Result := (MontantHT - Remise) * (1 + TVA / 100);
end;

class function TFactureManager.ValiderFacture(NumFacture: string): Boolean;  
begin  
  // Logique de validation
  Result := Length(NumFacture) > 0;
end;

end.
```

Cette unité peut être utilisée dans :
- Votre application VCL desktop
- Votre application web IntraWeb
- Vos services REST
- Votre application mobile FireMonkey

### Pattern 2 : Data Module partagé

**Principe :** Centraliser l'accès aux données

```pascal
// Unit DataModule.pas
type
  TDMData = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
  public
    function GetClients: TDataSet;
    procedure SaveClient(const Nom, Prenom, Email: string);
  end;

implementation

function TDMData.GetClients: TDataSet;  
begin  
  FDQuery1.Close;
  FDQuery1.SQL.Text := 'SELECT * FROM clients';
  FDQuery1.Open;
  Result := FDQuery1;
end;

procedure TDMData.SaveClient(const Nom, Prenom, Email: string);  
begin  
  FDQuery1.Close;
  FDQuery1.SQL.Text :=
    'INSERT INTO clients (nom, prenom, email) VALUES (:nom, :prenom, :email)';
  FDQuery1.ParamByName('nom').AsString := Nom;
  FDQuery1.ParamByName('prenom').AsString := Prenom;
  FDQuery1.ParamByName('email').AsString := Email;
  FDQuery1.ExecSQL;
end;
```

### Pattern 3 : Factory pour l'interface

**Principe :** Abstraire la création des composants UI

```pascal
type
  IFormFactory = interface
    function CreateLoginForm: TComponent;
    function CreateMainForm: TComponent;
  end;

  TVCLFormFactory = class(TInterfacedObject, IFormFactory)
    function CreateLoginForm: TComponent;
    function CreateMainForm: TComponent;
  end;

  TWebFormFactory = class(TInterfacedObject, IFormFactory)
    function CreateLoginForm: TComponent;
    function CreateMainForm: TComponent;
  end;

// Usage
var
  Factory: IFormFactory;
begin
  {$IFDEF WEB}
  Factory := TWebFormFactory.Create;
  {$ELSE}
  Factory := TVCLFormFactory.Create;
  {$ENDIF}

  Factory.CreateLoginForm;
end;
```

## Gestion des différences comportementales

### Communication asynchrone

**En VCL :** Tout est synchrone et immédiat

```pascal
// VCL Desktop
Button1.Caption := 'Cliquez-moi';  
Edit1.Text := 'Nouvelle valeur';  
Label1.Caption := 'Mis à jour !';  
// Tout se passe instantanément
```

**En Web :** Il y a des allers-retours avec le serveur

```pascal
// IntraWeb
IWButton1.Caption := 'Cliquez-moi';    // Marqué comme modifié  
IWEdit1.Text := 'Nouvelle valeur';    // Marqué comme modifié  
IWLabel1.Caption := 'Mis à jour !';   // Marqué comme modifié  
// Les changements sont envoyés au navigateur
// à la fin du traitement de l'événement
```

**Conseil :** Regroupez les modifications pour minimiser les échanges réseau.

### Gestion des fichiers

**En VCL :** Accès direct au système de fichiers local

```pascal
// VCL Desktop - Accès fichier local
OpenDialog1.Execute;  
Image1.Picture.LoadFromFile(OpenDialog1.FileName);  
```

**En Web :** Upload nécessaire

```pascal
// IntraWeb - Upload fichier
procedure TIWForm1.IWFileUpload1AsyncUpload(Sender: TObject;
  const AFileName: string);
begin
  // Le fichier est uploadé sur le serveur
  IWImage1.Picture.LoadFromFile(AFileName);
end;
```

### Gestion des impressions

**En VCL :** Impression directe

```pascal
// VCL Desktop
Printer.BeginDoc;  
Printer.Canvas.TextOut(100, 100, 'Mon texte');  
Printer.EndDoc;  
```

**En Web :** Génération PDF ou impression navigateur

```pascal
// IntraWeb - Générer un PDF
procedure TIWForm1.GenererPDF;  
var  
  PDF: TPDFDocument;
begin
  PDF := TPDFDocument.Create;
  try
    PDF.AddPage;
    PDF.Canvas.TextOut(100, 100, 'Mon texte');
    PDF.SaveToFile(WebApplication.ApplicationPath + 'rapport.pdf');

    // Proposer le téléchargement
    WebApplication.SendFile('rapport.pdf');
  finally
    PDF.Free;
  end;
end;
```

## Optimisation des performances

### Principe 1 : Minimiser les allers-retours

**Mauvais :**
```pascal
// Chaque modification provoque un aller-retour
for i := 0 to 99 do  
begin  
  IWListBox1.Items.Add('Item ' + IntToStr(i)); // 100 allers-retours !
end;
```

**Bon :**
```pascal
// Construction locale puis affectation unique
var
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    for i := 0 to 99 do
      TempList.Add('Item ' + IntToStr(i));

    IWListBox1.Items.Assign(TempList); // 1 seul aller-retour
  finally
    TempList.Free;
  end;
end;
```

### Principe 2 : Utiliser le cache

```pascal
// Stocker en session les données fréquemment utilisées
procedure TIWForm1.ChargerListeClients;  
var  
  Liste: TStringList;
begin
  // Vérifier si déjà en cache
  if WebApplication.Data.Values['ListeClients'] = '' then
  begin
    // Charger depuis la base
    Liste := TStringList.Create;
    try
      // ... remplir la liste ...
      WebApplication.Data.Values['ListeClients'] := Liste.Text;
    finally
      Liste.Free;
    end;
  end;

  // Utiliser les données en cache
  IWComboBox1.Items.Text := WebApplication.Data.Values['ListeClients'];
end;
```

### Principe 3 : AJAX pour interactions légères

```pascal
// Utiliser AJAX pour des mises à jour partielles
procedure TIWForm1.IWButton1AsyncClick(Sender: TObject;
  EventParams: TStringList);
begin
  // Seul ce composant sera mis à jour (pas toute la page)
  IWLabel1.Caption := 'Mis à jour via AJAX : ' + TimeToStr(Now);
end;
```

## Avantages de l'approche VCL web

### 1. Réutilisation des compétences

Si vous maîtrisez déjà la VCL, vous êtes **immédiatement productif** :
- Même paradigme de programmation
- Composants familiers
- Patterns connus
- IDE identique

### 2. Réutilisation du code

Vous pouvez réutiliser :
- Toute votre logique métier
- Vos classes de gestion de données
- Vos algorithmes et calculs
- Vos Data Modules

**Gain de temps considérable** pour les migrations !

### 3. Développement rapide

- Conception visuelle dans l'IDE
- Pas de HTML/CSS à écrire manuellement
- Génération automatique du code web
- Tests immédiats en mode standalone

### 4. Maintenance facilitée

- Un seul langage (Object Pascal)
- Une seule équipe de développement
- Code source unifié
- Débogage dans Delphi

## Limitations et défis

### 1. Performance réseau

Chaque interaction utilisateur peut nécessiter un aller-retour serveur :
- Latence perceptible
- Dépendance à la qualité du réseau
- Moins réactif qu'une application desktop

**Solution :** Utiliser AJAX pour les interactions légères

### 2. Scalabilité

Chaque utilisateur connecté consomme des ressources serveur :
- Mémoire pour la session
- Threads de traitement
- Connexions base de données

**Solution :** Architecture distribuée, load balancing, optimisation

### 3. Interface utilisateur

L'interface générée peut paraître moins moderne :
- Style datant par défaut
- Moins fluide que les frameworks JS natifs
- Limitations des effets visuels

**Solution :** Personnalisation CSS, templates, styles modernes

### 4. Mode déconnecté

Applications web côté serveur ne fonctionnent **pas hors ligne** :
- Nécessite connexion permanente
- Pas de cache local
- Problématique pour mobilité

**Solution :** Pour le hors ligne, préférer TMS Web Core (PWA)

## Bonnes pratiques

### 1. Séparer les couches

```
Application VCL Web
├── UI Layer (Formulaires IntraWeb)
├── Business Layer (Logique métier pure)
└── Data Layer (Accès base de données)
```

### 2. Gérer correctement les sessions

```pascal
// Toujours vérifier la validité de la session
if not Assigned(WebApplication) then Exit;  
if WebApplication.Terminated then Exit;  

// Stocker les données critiques
WebApplication.Data.Values['UserID'] := UserID;
```

### 3. Sécuriser l'application

```pascal
// Valider côté serveur (jamais faire confiance au client)
procedure TIWForm1.IWButton1Click(Sender: TObject);  
begin  
  // Validation
  if Trim(IWEdit1.Text) = '' then
  begin
    WebApplication.ShowMessage('Champ obligatoire !');
    Exit;
  end;

  // Échapper les entrées utilisateur
  SafeValue := StringReplace(IWEdit1.Text, '''', '''''', [rfReplaceAll]);

  // Utiliser des requêtes paramétrées
  Query.SQL.Text := 'SELECT * FROM users WHERE login = :login';
  Query.ParamByName('login').AsString := SafeValue;
end;
```

### 4. Optimiser les requêtes

```pascal
// Limiter les données transférées
Query.SQL.Text :=
  'SELECT id, nom, prenom FROM clients ' +
  'ORDER BY nom LIMIT 100';  // Pas de SELECT *

// Pagination
Query.SQL.Text :=
  'SELECT * FROM clients ' +
  'LIMIT :limit OFFSET :offset';
Query.ParamByName('limit').AsInteger := 50;  
Query.ParamByName('offset').AsInteger := PageNumber * 50;  
```

## Conclusion

Les applications web basées sur VCL avec Delphi offrent un excellent **pont entre le monde desktop et le monde web**. Elles permettent :

✅ De capitaliser sur vos compétences VCL existantes  
✅ De migrer rapidement des applications desktop vers le web  
✅ De développer efficacement des applications intranet  
✅ De réutiliser une grande partie de votre code existant

Cette approche est particulièrement adaptée pour :
- Les applications d'entreprise internes
- Les tableaux de bord de gestion
- Les outils d'administration
- Les migrations progressives d'applications legacy

Cependant, pour des applications web modernes destinées au grand public, avec des exigences fortes en termes de réactivité et d'expérience utilisateur, vous pourriez préférer des solutions comme TMS Web Core (section 23.1) ou une architecture avec services REST (section 23.3).

Le choix de la technologie dépend toujours de votre contexte, de vos contraintes et de vos objectifs. L'approche VCL web reste une option solide et productive dans l'écosystème Delphi.

⏭️ [Création de services REST avec Delphi](/23-conception-dapplications-web-avec-delphi/03-creation-de-services-rest-avec-delphi.md)
