🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.9 Partage de code entre applications mobile et desktop

## Introduction

L'un des avantages majeurs de Delphi est sa capacité à créer des applications pour multiples plateformes à partir d'une seule base de code. Plutôt que d'écrire votre application trois fois (une fois en Java/Kotlin pour Android, une fois en Swift pour iOS, et une fois en Delphi pour Windows), vous pouvez écrire votre code une seule fois et le compiler pour toutes ces plateformes.

Cependant, partager du code entre mobile et desktop ne signifie pas que vous écrivez exactement la même application pour toutes les plateformes. Les utilisateurs mobiles et desktop ont des attentes différentes, des modes d'interaction différents, et les appareils ont des capacités différentes. Le secret est de **partager intelligemment** : partager ce qui peut l'être (logique métier, accès aux données, algorithmes) tout en adaptant ce qui doit l'être (interface utilisateur, navigation, gestion des entrées).

Dans cette section, nous allons explorer comment structurer vos projets Delphi pour maximiser le partage de code tout en offrant une expérience optimale sur chaque plateforme.

## Pourquoi partager du code ?

### Avantages du partage de code

**Réduction du temps de développement** :
- Écrire la logique métier une seule fois
- Moins de code à maintenir
- Développement plus rapide de nouvelles fonctionnalités

**Cohérence entre plateformes** :
- Même logique = même comportement
- Moins de risques d'incohérences
- Tests centralisés

**Maintenance simplifiée** :
- Correction de bug une seule fois
- Évolutions simultanées sur toutes les plateformes
- Documentation unifiée

**Expertise concentrée** :
- Une seule équipe de développement
- Un seul langage (Object Pascal)
- Courbe d'apprentissage unique

### Exemples concrets

Imaginez une application de gestion de tâches :

**Code à partager** :
- Modèle de données (classes TTache, TProjet, TUtilisateur)
- Logique métier (calcul des échéances, priorités, notifications)
- Accès à la base de données
- Synchronisation avec le serveur
- Validation des données
- Algorithmes de tri et de filtrage

**Code spécifique à chaque plateforme** :
- Interface utilisateur (VCL pour Windows, FMX pour mobile)
- Navigation (menu vs onglets)
- Gestion des gestes tactiles
- Accès aux APIs spécifiques (notifications, GPS)
- Taille et disposition des contrôles

## Architecture en couches

La clé d'un partage de code réussi est une architecture bien structurée en couches distinctes.

### Modèle à trois couches

```
┌─────────────────────────────────────┐
│   Couche Présentation (UI)          │  ← Spécifique à chaque plateforme
│   - VCL (Windows Desktop)           │
│   - FMX (Mobile, macOS, Linux)      │
└─────────────────────────────────────┘
              ↓  ↑
┌─────────────────────────────────────┐
│   Couche Logique Métier             │  ← Code partagé
│   - Règles métier                   │
│   - Traitement des données          │
│   - Validation                      │
└─────────────────────────────────────┘
              ↓  ↑
┌─────────────────────────────────────┐
│   Couche Accès aux Données          │  ← Code partagé
│   - Bases de données                │
│   - Services REST                   │
│   - Fichiers                        │
└─────────────────────────────────────┘
```

### Exemple de structure de classes

```pascal
// ========================================
// Unité partagée : Modèle de données
// Fichier: Model.Tache.pas
// ========================================
unit Model.Tache;

interface

type
  TPriorite = (prBasse, prMoyenne, prHaute, prUrgente);
  TStatut = (stAFaire, stEnCours, stTerminee, stAnnulee);

  TTache = class
  private
    FID: Integer;
    FTitre: string;
    FDescription: string;
    FDateEcheance: TDateTime;
    FPriorite: TPriorite;
    FStatut: TStatut;
    FDateCreation: TDateTime;
  public
    constructor Create;

    // Propriétés
    property ID: Integer read FID write FID;
    property Titre: string read FTitre write FTitre;
    property Description: string read FDescription write FDescription;
    property DateEcheance: TDateTime read FDateEcheance write FDateEcheance;
    property Priorite: TPriorite read FPriorite write FPriorite;
    property Statut: TStatut read FStatut write FStatut;
    property DateCreation: TDateTime read FDateCreation write FDateCreation;

    // Méthodes métier partagées
    function EstEnRetard: Boolean;
    function EstUrgente: Boolean;
    function JoursRestants: Integer;
    procedure Terminer;
    function Valider: string; // Retourne un message d'erreur ou chaîne vide
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

constructor TTache.Create;
begin
  inherited;
  FDateCreation := Now;
  FStatut := stAFaire;
  FPriorite := prMoyenne;
end;

function TTache.EstEnRetard: Boolean;
begin
  Result := (FStatut <> stTerminee) and
            (FStatut <> stAnnulee) and
            (FDateEcheance < Now);
end;

function TTache.EstUrgente: Boolean;
begin
  // Une tâche est urgente si échéance dans moins de 2 jours
  Result := (FStatut = stAFaire) and
            (JoursRestants <= 2) and
            (JoursRestants >= 0);
end;

function TTache.JoursRestants: Integer;
begin
  Result := DaysBetween(Now, FDateEcheance);
  if FDateEcheance < Now then
    Result := -Result;
end;

procedure TTache.Terminer;
begin
  FStatut := stTerminee;
end;

function TTache.Valider: string;
begin
  Result := '';

  if Trim(FTitre).IsEmpty then
    Result := 'Le titre est obligatoire'
  else if Length(FTitre) < 3 then
    Result := 'Le titre doit contenir au moins 3 caractères'
  else if FDateEcheance < Date then
    Result := 'La date d''échéance ne peut pas être dans le passé';
end;

end.
```

```pascal
// ========================================
// Unité partagée : Gestionnaire de tâches
// Fichier: Business.TacheManager.pas
// ========================================
unit Business.TacheManager;

interface

uses
  System.Generics.Collections, Model.Tache;

type
  TTacheManager = class
  private
    FTaches: TObjectList<TTache>;
    FOnTacheModifiee: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    // CRUD Operations (partagé entre toutes les plateformes)
    function AjouterTache(const Titre, Description: string;
      DateEcheance: TDateTime; Priorite: TPriorite): TTache;
    procedure SupprimerTache(TacheID: Integer);
    procedure ModifierTache(Tache: TTache);
    function ObtenirTache(TacheID: Integer): TTache;
    function ObtenirToutesLesTaches: TArray<TTache>;

    // Filtres et recherches
    function ObtenirTachesEnRetard: TArray<TTache>;
    function ObtenirTachesUrgentes: TArray<TTache>;
    function ObtenirTachesParPriorite(Priorite: TPriorite): TArray<TTache>;
    function RechercherTaches(const Texte: string): TArray<TTache>;

    // Statistiques
    function CompterTachesTerminees: Integer;
    function CompterTachesEnCours: Integer;
    function TauxCompletionPourcentage: Double;

    // Persistance (interface à implémenter spécifiquement)
    procedure Sauvegarder;
    procedure Charger;

    // Événements
    property OnTacheModifiee: TNotifyEvent read FOnTacheModifiee write FOnTacheModifiee;
  end;

implementation

uses
  System.SysUtils;

constructor TTacheManager.Create;
begin
  inherited;
  FTaches := TObjectList<TTache>.Create(True); // True = possède les objets
end;

destructor TTacheManager.Destroy;
begin
  FTaches.Free;
  inherited;
end;

function TTacheManager.AjouterTache(const Titre, Description: string;
  DateEcheance: TDateTime; Priorite: TPriorite): TTache;
begin
  Result := TTache.Create;
  Result.Titre := Titre;
  Result.Description := Description;
  Result.DateEcheance := DateEcheance;
  Result.Priorite := Priorite;

  // Valider avant d'ajouter
  var MessageErreur := Result.Valider;
  if not MessageErreur.IsEmpty then
  begin
    Result.Free;
    raise Exception.Create(MessageErreur);
  end;

  // Générer un ID unique (en production, utiliser la BD)
  Result.ID := FTaches.Count + 1;

  FTaches.Add(Result);

  if Assigned(FOnTacheModifiee) then
    FOnTacheModifiee(Self);
end;

procedure TTacheManager.SupprimerTache(TacheID: Integer);
var
  Tache: TTache;
begin
  Tache := ObtenirTache(TacheID);
  if Assigned(Tache) then
  begin
    FTaches.Remove(Tache);

    if Assigned(FOnTacheModifiee) then
      FOnTacheModifiee(Self);
  end;
end;

function TTacheManager.ObtenirTache(TacheID: Integer): TTache;
var
  Tache: TTache;
begin
  Result := nil;
  for Tache in FTaches do
  begin
    if Tache.ID = TacheID then
      Exit(Tache);
  end;
end;

function TTacheManager.ObtenirTachesEnRetard: TArray<TTache>;
var
  Liste: TList<TTache>;
  Tache: TTache;
begin
  Liste := TList<TTache>.Create;
  try
    for Tache in FTaches do
    begin
      if Tache.EstEnRetard then
        Liste.Add(Tache);
    end;

    Result := Liste.ToArray;
  finally
    Liste.Free;
  end;
end;

function TTacheManager.CompterTachesTerminees: Integer;
var
  Tache: TTache;
begin
  Result := 0;
  for Tache in FTaches do
  begin
    if Tache.Statut = stTerminee then
      Inc(Result);
  end;
end;

function TTacheManager.TauxCompletionPourcentage: Double;
begin
  if FTaches.Count = 0 then
    Result := 0
  else
    Result := (CompterTachesTerminees / FTaches.Count) * 100;
end;

procedure TTacheManager.Sauvegarder;
begin
  // Implémentation spécifique à chaque plateforme
  // Sera surchargée dans les classes dérivées
end;

procedure TTacheManager.Charger;
begin
  // Implémentation spécifique à chaque plateforme
  // Sera surchargée dans les classes dérivées
end;

end.
```

## Organisation des projets

### Structure recommandée des dossiers

```
MonProjet/
├── Shared/                      ← Code partagé
│   ├── Models/                  ← Modèles de données
│   │   ├── Model.Tache.pas
│   │   ├── Model.Utilisateur.pas
│   │   └── Model.Projet.pas
│   ├── Business/                ← Logique métier
│   │   ├── Business.TacheManager.pas
│   │   └── Business.ValidationService.pas
│   ├── Data/                    ← Accès aux données
│   │   ├── Data.Database.pas
│   │   └── Data.ApiClient.pas
│   └── Utils/                   ← Utilitaires
│       ├── Utils.DateHelper.pas
│       └── Utils.StringHelper.pas
│
├── Desktop/                     ← Application Windows (VCL)
│   ├── Forms/
│   │   ├── Main.Form.pas
│   │   └── TaskEdit.Form.pas
│   ├── DesktopApp.dpr          ← Projet principal
│   └── DesktopApp.dproj
│
├── Mobile/                      ← Application Mobile (FMX)
│   ├── Forms/
│   │   ├── Main.Form.pas
│   │   └── TaskEdit.Form.pas
│   ├── MobileApp.dpr           ← Projet principal
│   └── MobileApp.dproj
│
└── Tests/                       ← Tests unitaires
    ├── Test.Model.Tache.pas
    └── Test.Business.TacheManager.pas
```

### Configuration des chemins de recherche

Dans chaque projet (Desktop et Mobile), configurez les chemins pour accéder au code partagé :

```
Project > Options > Delphi Compiler > Search Path
Ajouter : ..\Shared\Models;..\Shared\Business;..\Shared\Data;..\Shared\Utils
```

## Directives de compilation conditionnelle

Les directives de compilation permettent d'adapter le code selon la plateforme cible.

### Directives de plateforme principales

```pascal
unit Utils.PlatformHelper;

interface

type
  TPlatformHelper = class
  public
    class function NomPlateforme: string;
    class function EstMobile: Boolean;
    class function EstDesktop: Boolean;
    class function CheminDocuments: string;
    class function SeparateurChemin: Char;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

class function TPlatformHelper.NomPlateforme: string;
begin
  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
    Result := 'iOS';
    {$ELSE}
    Result := 'macOS';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := 'Android';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}
end;

class function TPlatformHelper.EstMobile: Boolean;
begin
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TPlatformHelper.EstDesktop: Boolean;
begin
  Result := not EstMobile;
end;

class function TPlatformHelper.CheminDocuments: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.GetDocumentsPath; // C:\Users\[User]\Documents
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := TPath.GetDocumentsPath; // /data/data/[package]/files
  {$ENDIF}

  {$IFDEF IOS}
  Result := TPath.GetDocumentsPath; // /Documents
  {$ENDIF}

  {$IFDEF MACOS}
  Result := TPath.GetHomePath + '/Documents';
  {$ENDIF}
end;

class function TPlatformHelper.SeparateurChemin: Char;
begin
  {$IFDEF MSWINDOWS}
  Result := '\';
  {$ELSE}
  Result := '/';
  {$ENDIF}
end;

end.
```

### Adapter le code selon la plateforme

```pascal
// Exemple : Ouvrir un URL dans le navigateur
procedure TOuvreursURL;
var
  URL: string;
begin
  URL := 'https://www.example.com';

  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}

  {$IFDEF ANDROID}
  var Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  TAndroidHelper.Context.startActivity(Intent);
  {$ENDIF}

  {$IFDEF IOS}
  SharedApplication.openURL(StrToNSUrl(URL));
  {$ENDIF}

  {$IFDEF MACOS}
  _system(PAnsiChar('open ' + AnsiString(URL)));
  {$ENDIF}
end;
```

### Code avec alternatives selon la plateforme

```pascal
// Afficher une notification
procedure TAfficherNotification(const Titre, Message: string);
begin
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  // Sur mobile : notification système
  var NotificationCenter := TNotificationCenter.Create(nil);
  try
    var Notification := NotificationCenter.CreateNotification;
    try
      Notification.Name := 'notif_' + FormatDateTime('hhnnss', Now);
      Notification.Title := Titre;
      Notification.AlertBody := Message;
      Notification.FireDate := Now;

      NotificationCenter.PresentNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotificationCenter.Free;
  end;
  {$ELSE}
  // Sur desktop : MessageBox simple
  ShowMessage(Titre + sLineBreak + sLineBreak + Message);
  {$ENDIF}
end;
```

## Patterns d'architecture pour le partage de code

### Pattern MVVM (Model-View-ViewModel)

Le pattern MVVM sépare clairement la logique de l'interface, facilitant le partage de code.

```pascal
// ========================================
// ViewModel partagé entre mobile et desktop
// Fichier: ViewModel.TacheList.pas
// ========================================
unit ViewModel.TacheList;

interface

uses
  System.Generics.Collections, Model.Tache, Business.TacheManager;

type
  // Interface pour la Vue (implémentée différemment sur VCL et FMX)
  ITacheListView = interface
    ['{12345678-1234-1234-1234-123456789012}']
    procedure AfficherTaches(Taches: TArray<TTache>);
    procedure AfficherMessage(const Message: string);
    procedure AfficherChargement(Visible: Boolean);
  end;

  // ViewModel partagé
  TTacheListViewModel = class
  private
    FView: ITacheListView;
    FManager: TTacheManager;
    FFiltreActuel: string;
  public
    constructor Create(View: ITacheListView);
    destructor Destroy; override;

    // Commandes (appelées par la vue)
    procedure Charger;
    procedure AjouterNouvelleTache(const Titre, Description: string;
      DateEcheance: TDateTime; Priorite: TPriorite);
    procedure SupprimerTache(TacheID: Integer);
    procedure FiltrerTaches(const Texte: string);
    procedure AfficherTachesUrgentes;
    procedure AfficherTachesEnRetard;

    // Propriétés
    property Manager: TTacheManager read FManager;
  end;

implementation

uses
  System.SysUtils;

constructor TTacheListViewModel.Create(View: ITacheListView);
begin
  inherited Create;
  FView := View;
  FManager := TTacheManager.Create;
end;

destructor TTacheListViewModel.Destroy;
begin
  FManager.Free;
  inherited;
end;

procedure TTacheListViewModel.Charger;
begin
  FView.AfficherChargement(True);
  try
    // Charger les tâches depuis la source de données
    FManager.Charger;

    // Afficher dans la vue
    FView.AfficherTaches(FManager.ObtenirToutesLesTaches);
  finally
    FView.AfficherChargement(False);
  end;
end;

procedure TTacheListViewModel.AjouterNouvelleTache(const Titre, Description: string;
  DateEcheance: TDateTime; Priorite: TPriorite);
begin
  try
    FManager.AjouterTache(Titre, Description, DateEcheance, Priorite);
    FManager.Sauvegarder;

    // Rafraîchir l'affichage
    FView.AfficherTaches(FManager.ObtenirToutesLesTaches);
    FView.AfficherMessage('Tâche ajoutée avec succès');
  except
    on E: Exception do
      FView.AfficherMessage('Erreur : ' + E.Message);
  end;
end;

procedure TTacheListViewModel.FiltrerTaches(const Texte: string);
begin
  FFiltreActuel := Texte;

  if Texte.IsEmpty then
    FView.AfficherTaches(FManager.ObtenirToutesLesTaches)
  else
    FView.AfficherTaches(FManager.RechercherTaches(Texte));
end;

procedure TTacheListViewModel.AfficherTachesUrgentes;
begin
  FView.AfficherTaches(FManager.ObtenirTachesUrgentes);
end;

end.
```

```pascal
// ========================================
// Vue VCL pour Desktop
// Fichier: Desktop/Forms/Main.Form.pas
// ========================================
unit Main.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Model.Tache, ViewModel.TacheList;

type
  TFormMain = class(TForm, ITacheListView)
    ListView1: TListView;
    Panel1: TPanel;
    BtnAjouter: TButton;
    BtnSupprimer: TButton;
    EditRecherche: TEdit;
    BtnUrgentes: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnAjouterClick(Sender: TObject);
    procedure EditRechercheChange(Sender: TObject);
    procedure BtnUrgentesClick(Sender: TObject);
  private
    FViewModel: TTacheListViewModel;

    // Implémentation de ITacheListView
    procedure AfficherTaches(Taches: TArray<TTache>);
    procedure AfficherMessage(const Message: string);
    procedure AfficherChargement(Visible: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FViewModel := TTacheListViewModel.Create(Self);
  FViewModel.Charger;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FViewModel.Free;
end;

procedure TFormMain.AfficherTaches(Taches: TArray<TTache>);
var
  Tache: TTache;
  Item: TListItem;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    for Tache in Taches do
    begin
      Item := ListView1.Items.Add;
      Item.Caption := Tache.Titre;
      Item.SubItems.Add(Tache.Description);
      Item.SubItems.Add(DateToStr(Tache.DateEcheance));
      Item.Data := Pointer(Tache.ID);

      // Couleur selon l'urgence
      if Tache.EstEnRetard then
        Item.MakeVisible(False); // Ou autre style
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TFormMain.AfficherMessage(const Message: string);
begin
  StatusBar1.SimpleText := Message;
  ShowMessage(Message);
end;

procedure TFormMain.AfficherChargement(Visible: Boolean);
begin
  // Sur desktop, on pourrait afficher une barre de progression
  Screen.Cursor := IfThen(Visible, crHourGlass, crDefault);
end;

procedure TFormMain.BtnAjouterClick(Sender: TObject);
var
  Titre, Description: string;
begin
  // Dialogue d'ajout de tâche (simplifié)
  Titre := InputBox('Nouvelle tâche', 'Titre :', '');
  if not Titre.IsEmpty then
  begin
    Description := InputBox('Nouvelle tâche', 'Description :', '');
    FViewModel.AjouterNouvelleTache(Titre, Description, Now + 7, prMoyenne);
  end;
end;

procedure TFormMain.EditRechercheChange(Sender: TObject);
begin
  FViewModel.FiltrerTaches(EditRecherche.Text);
end;

procedure TFormMain.BtnUrgentesClick(Sender: TObject);
begin
  FViewModel.AfficherTachesUrgentes;
end;

end.
```

```pascal
// ========================================
// Vue FMX pour Mobile
// Fichier: Mobile/Forms/Main.Form.pas
// ========================================
unit Main.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.ListView, FMX.Layouts, FMX.Edit,
  Model.Tache, ViewModel.TacheList;

type
  TFormMain = class(TForm, ITacheListView)
    ListView1: TListView;
    ToolBar1: TToolBar;
    BtnAjouter: TSpeedButton;
    EditRecherche: TEdit;
    BtnUrgentes: TSpeedButton;
    AniIndicator1: TAniIndicator;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnAjouterClick(Sender: TObject);
    procedure EditRechercheChange(Sender: TObject);
    procedure BtnUrgentesClick(Sender: TObject);
  private
    FViewModel: TTacheListViewModel;

    // Implémentation de ITacheListView
    procedure AfficherTaches(Taches: TArray<TTache>);
    procedure AfficherMessage(const Message: string);
    procedure AfficherChargement(Visible: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FViewModel := TTacheListViewModel.Create(Self);
  FViewModel.Charger;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FViewModel.Free;
end;

procedure TFormMain.AfficherTaches(Taches: TArray<TTache>);
var
  Tache: TTache;
  Item: TListViewItem;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    for Tache in Taches do
    begin
      Item := ListView1.Items.Add;
      Item.Text := Tache.Titre;
      Item.Detail := Tache.Description;
      Item.TagString := Tache.ID.ToString;

      // Indicateur visuel pour les tâches urgentes
      if Tache.EstUrgente then
        Item.Objects.TextObject.TextColor := TAlphaColors.Red;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TFormMain.AfficherMessage(const Message: string);
begin
  ShowMessage(Message);
end;

procedure TFormMain.AfficherChargement(Visible: Boolean);
begin
  AniIndicator1.Enabled := Visible;
  AniIndicator1.Visible := Visible;
end;

procedure TFormMain.BtnAjouterClick(Sender: TObject);
var
  Titre, Description: string;
begin
  // Sur mobile, on utiliserait un dialogue ou un formulaire dédié
  // Ici simplifié avec InputQuery
  if InputQuery('Nouvelle tâche', ['Titre', 'Description'],
    [Titre, Description]) then
  begin
    if not Titre.IsEmpty then
      FViewModel.AjouterNouvelleTache(Titre, Description, Now + 7, prMoyenne);
  end;
end;

procedure TFormMain.EditRechercheChange(Sender: TObject);
begin
  FViewModel.FiltrerTaches(EditRecherche.Text);
end;

procedure TFormMain.BtnUrgentesClick(Sender: TObject);
begin
  FViewModel.AfficherTachesUrgentes;
end;

end.
```

## Gestion de la persistance multi-plateforme

### Interface abstraite pour la persistance

```pascal
// ========================================
// Interface de persistance (partagée)
// Fichier: Shared/Data/Data.Storage.Interfaces.pas
// ========================================
unit Data.Storage.Interfaces;

interface

uses
  Model.Tache, System.Generics.Collections;

type
  IDataStorage = interface
    ['{ABCDEF12-3456-7890-ABCD-EF1234567890}']
    procedure SauvegarderTaches(Taches: TObjectList<TTache>);
    function ChargerTaches: TObjectList<TTache>;
    procedure Vider;
  end;

implementation

end.
```

### Implémentation SQLite (partagée)

```pascal
// ========================================
// Implémentation SQLite (utilisable partout)
// Fichier: Shared/Data/Data.Storage.SQLite.pas
// ========================================
unit Data.Storage.SQLite;

interface

uses
  Data.Storage.Interfaces, Model.Tache, System.Generics.Collections,
  FireDAC.Comp.Client, FireDAC.Stan.Def;

type
  TSQLiteStorage = class(TInterfacedObject, IDataStorage)
  private
    FConnection: TFDConnection;
    procedure ConfigurerConnexion;
    procedure CreerTables;
  public
    constructor Create;
    destructor Destroy; override;

    // IDataStorage
    procedure SauvegarderTaches(Taches: TObjectList<TTache>);
    function ChargerTaches: TObjectList<TTache>;
    procedure Vider;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, Utils.PlatformHelper;

constructor TSQLiteStorage.Create;
begin
  inherited;
  FConnection := TFDConnection.Create(nil);
  ConfigurerConnexion;
  CreerTables;
end;

destructor TSQLiteStorage.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TSQLiteStorage.ConfigurerConnexion;
var
  CheminBD: string;
begin
  // Utiliser le bon chemin selon la plateforme
  CheminBD := TPath.Combine(TPlatformHelper.CheminDocuments, 'taches.db');

  FConnection.DriverName := 'SQLite';
  FConnection.Params.Database := CheminBD;
  FConnection.LoginPrompt := False;
  FConnection.Connected := True;
end;

procedure TSQLiteStorage.CreerTables;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS taches (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      '  titre TEXT NOT NULL, ' +
      '  description TEXT, ' +
      '  date_echeance DATETIME, ' +
      '  priorite INTEGER, ' +
      '  statut INTEGER, ' +
      '  date_creation DATETIME' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSQLiteStorage.SauvegarderTaches(Taches: TObjectList<TTache>);
var
  Query: TFDQuery;
  Tache: TTache;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Vider la table
    Query.SQL.Text := 'DELETE FROM taches';
    Query.ExecSQL;

    // Insérer toutes les tâches
    Query.SQL.Text :=
      'INSERT INTO taches (titre, description, date_echeance, ' +
      'priorite, statut, date_creation) ' +
      'VALUES (:titre, :desc, :echeance, :prio, :statut, :creation)';

    for Tache in Taches do
    begin
      Query.ParamByName('titre').AsString := Tache.Titre;
      Query.ParamByName('desc').AsString := Tache.Description;
      Query.ParamByName('echeance').AsDateTime := Tache.DateEcheance;
      Query.ParamByName('prio').AsInteger := Ord(Tache.Priorite);
      Query.ParamByName('statut').AsInteger := Ord(Tache.Statut);
      Query.ParamByName('creation').AsDateTime := Tache.DateCreation;

      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

function TSQLiteStorage.ChargerTaches: TObjectList<TTache>;
var
  Query: TFDQuery;
  Tache: TTache;
begin
  Result := TObjectList<TTache>.Create(True);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM taches ORDER BY date_echeance';
    Query.Open;

    while not Query.Eof do
    begin
      Tache := TTache.Create;
      Tache.ID := Query.FieldByName('id').AsInteger;
      Tache.Titre := Query.FieldByName('titre').AsString;
      Tache.Description := Query.FieldByName('description').AsString;
      Tache.DateEcheance := Query.FieldByName('date_echeance').AsDateTime;
      Tache.Priorite := TPriorite(Query.FieldByName('priorite').AsInteger);
      Tache.Statut := TStatut(Query.FieldByName('statut').AsInteger);
      Tache.DateCreation := Query.FieldByName('date_creation').AsDateTime;

      Result.Add(Tache);

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSQLiteStorage.Vider;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'DELETE FROM taches';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

## Fonctionnalités spécifiques à adapter

### Gestion des dialogues

```pascal
// Classe utilitaire pour les dialogues multi-plateformes
unit Utils.Dialogues;

interface

type
  TDialogueHelper = class
  public
    class procedure AfficherMessage(const Titre, Message: string);
    class function Confirmer(const Message: string): Boolean;
    class function DemanderTexte(const Prompt: string;
      var Valeur: string): Boolean;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Vcl.Dialogs,
  {$ELSE}
  FMX.Dialogs,
  {$ENDIF}
  System.UITypes;

class procedure TDialogueHelper.AfficherMessage(const Titre, Message: string);
begin
  {$IFDEF MSWINDOWS}
  MessageDlg(Message, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  {$ELSE}
  ShowMessage(Message);
  {$ENDIF}
end;

class function TDialogueHelper.Confirmer(const Message: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := MessageDlg(Message, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;
  {$ELSE}
  Result := MessageDlg(Message, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;
  {$ENDIF}
end;

class function TDialogueHelper.DemanderTexte(const Prompt: string;
  var Valeur: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := InputQuery('Saisie', Prompt, Valeur);
  {$ELSE}
  Result := InputQuery('Saisie', [Prompt], [Valeur]);
  if Result then
    Valeur := Valeur; // FMX retourne un tableau
  {$ENDIF}
end;

end.
```

## Tests unitaires du code partagé

Les tests unitaires sont cruciaux pour le code partagé.

```pascal
// ========================================
// Tests unitaires pour TTache
// Fichier: Tests/Test.Model.Tache.pas
// ========================================
unit Test.Model.Tache;

interface

uses
  DUnitX.TestFramework, Model.Tache;

type
  [TestFixture]
  TTestTache = class
  private
    FTache: TTache;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreation;

    [Test]
    procedure TestEstEnRetard;

    [Test]
    procedure TestEstUrgente;

    [Test]
    procedure TestValidation;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

procedure TTestTache.Setup;
begin
  FTache := TTache.Create;
end;

procedure TTestTache.TearDown;
begin
  FTache.Free;
end;

procedure TTestTache.TestCreation;
begin
  Assert.IsNotNull(FTache);
  Assert.AreEqual(stAFaire, FTache.Statut);
  Assert.AreEqual(prMoyenne, FTache.Priorite);
end;

procedure TTestTache.TestEstEnRetard;
begin
  // Tâche avec échéance hier
  FTache.DateEcheance := Yesterday;
  FTache.Statut := stAFaire;
  Assert.IsTrue(FTache.EstEnRetard, 'Devrait être en retard');

  // Tâche avec échéance demain
  FTache.DateEcheance := Tomorrow;
  Assert.IsFalse(FTache.EstEnRetard, 'Ne devrait pas être en retard');

  // Tâche terminée (même avec échéance passée)
  FTache.DateEcheance := Yesterday;
  FTache.Statut := stTerminee;
  Assert.IsFalse(FTache.EstEnRetard, 'Tâche terminée ne peut être en retard');
end;

procedure TTestTache.TestEstUrgente;
begin
  // Tâche dans 1 jour
  FTache.DateEcheance := Now + 1;
  FTache.Statut := stAFaire;
  Assert.IsTrue(FTache.EstUrgente, 'Devrait être urgente');

  // Tâche dans 5 jours
  FTache.DateEcheance := Now + 5;
  Assert.IsFalse(FTache.EstUrgente, 'Ne devrait pas être urgente');
end;

procedure TTestTache.TestValidation;
var
  MessageErreur: string;
begin
  // Tâche invalide (pas de titre)
  FTache.Titre := '';
  MessageErreur := FTache.Valider;
  Assert.IsFalse(MessageErreur.IsEmpty, 'Devrait avoir une erreur');

  // Tâche valide
  FTache.Titre := 'Ma tâche';
  FTache.DateEcheance := Tomorrow;
  MessageErreur := FTache.Valider;
  Assert.IsTrue(MessageErreur.IsEmpty, 'Ne devrait pas avoir d''erreur');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTache);

end.
```

## Bonnes pratiques

### 1. Séparer clairement les responsabilités

```pascal
// ✅ BON : Chaque couche a une responsabilité claire
- Models/ : Structures de données uniquement
- Business/ : Logique métier pure
- Data/ : Accès aux données
- ViewModels/ : Liaison entre modèle et vue
- Views/ : Interface utilisateur spécifique

// ❌ MAUVAIS : Tout mélangé
procedure TFormMain.BtnSaveClick(Sender: TObject);
begin
  // Validation + Logique métier + Accès BD + UI - tout dans un bouton !
end;
```

### 2. Utiliser des interfaces pour le découplage

```pascal
// ✅ BON : Dépendre d'interfaces, pas d'implémentations concrètes
type
  ILogger = interface
    procedure Log(const Message: string);
  end;

  TTacheManager = class
  private
    FLogger: ILogger; // Interface, pas classe concrète
  end;

// On peut facilement changer l'implémentation
FManager.Logger := TFileLogger.Create;  // Desktop
FManager.Logger := TCloudLogger.Create; // Mobile
```

### 3. Minimiser les dépendances de plateforme

```pascal
// ✅ BON : Code qui fonctionne partout
function CalculerMontantTotal(Prix: Double; Quantite: Integer): Double;
begin
  Result := Prix * Quantite * 1.20; // TVA 20%
end;

// ⚠️ À isoler : Code spécifique à une plateforme
{$IFDEF MSWINDOWS}
procedure EnvoyerNotificationWindows;
{$ENDIF}
```

### 4. Documenter le code partagé

```pascal
/// <summary>
/// Calcule le nombre de jours restants avant l'échéance.
/// </summary>
/// <returns>
/// Nombre positif si l'échéance est future, négatif si passée.
/// </returns>
/// <remarks>
/// Cette méthode est utilisée à la fois sur desktop et mobile.
/// </remarks>
function TTache.JoursRestants: Integer;
begin
  Result := DaysBetween(Now, FDateEcheance);
  if FDateEcheance < Now then
    Result := -Result;
end;
```

### 5. Tester sur toutes les plateformes cibles

```pascal
// Créer une routine de test multi-plateforme
procedure TesterSurToutesLesPlateformes;
begin
  // 1. Compiler pour Windows (VCL)
  // 2. Compiler pour Android
  // 3. Compiler pour iOS
  // 4. Compiler pour macOS

  // Vérifier que :
  // - Pas d'erreurs de compilation
  // - Fonctionnalités identiques
  // - Performance acceptable
  // - UI adaptée à chaque plateforme
end;
```

## Conclusion

Le partage de code entre applications mobile et desktop avec Delphi est l'un des grands atouts de la plateforme. En structurant correctement votre code et en suivant les bonnes pratiques d'architecture, vous pouvez :

**Maximiser la réutilisation** :
- 60-80% du code peut être partagé entre plateformes
- Logique métier entièrement partageable
- Accès aux données généralement partageable
- UI à adapter (20-40% du code total)

**Points clés à retenir** :

1. **Architecture en couches** : Séparez UI, logique métier et données
2. **MVVM/MVC** : Utilisez un pattern qui facilite le découplage
3. **Interfaces** : Définissez des contrats clairs entre les couches
4. **Directives conditionnelles** : Adaptez le code selon la plateforme quand nécessaire
5. **Tests** : Le code partagé doit être bien testé
6. **Organisation** : Structurez vos projets de manière claire
7. **Documentation** : Documentez le code partagé pour faciliter la maintenance

Avec une bonne architecture, développer une application Delphi multi-plateformes devient un plaisir : vous écrivez la logique une fois, vous l'adaptez visuellement pour chaque plateforme, et vous obtenez des applications natives performantes partout. C'est le meilleur des deux mondes : productivité du développement cross-platform et qualité des applications natives.

Dans les sections précédentes, nous avons exploré tous les aspects du développement mobile avec Delphi, depuis les fondamentaux jusqu'aux techniques avancées de publication et de partage de code. Vous disposez maintenant de toutes les connaissances nécessaires pour créer des applications mobiles professionnelles et robustes avec Delphi !

⏭️ [Permissions et confidentialité des données](/15-applications-mobiles-avec-delphi/10-permissions-et-confidentialite-des-donnees.md)
