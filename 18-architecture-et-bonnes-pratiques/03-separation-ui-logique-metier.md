🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.3 Séparation UI / logique métier

## Introduction

Imaginez un restaurant : vous avez la salle avec les serveurs et les clients (l'interface), et vous avez la cuisine où les chefs préparent les plats (la logique). Si le chef devait servir les clients ET cuisiner en même temps, ce serait le chaos. De même, si le serveur devait cuisiner ET servir, rien ne fonctionnerait correctement.

Dans le développement logiciel, c'est exactement le même principe : **l'interface utilisateur (UI)** et **la logique métier** doivent être séparées. Cette séparation est l'un des principes fondamentaux d'une architecture saine.

### Qu'est-ce que la logique métier ?

La **logique métier** (ou business logic) représente les règles, calculs et processus qui définissent comment votre application fonctionne, indépendamment de la manière dont elle est affichée.

**Exemples de logique métier :**
- Calculer le prix TTC à partir d'un prix HT
- Valider qu'un email est au bon format
- Déterminer si un client peut commander (crédit suffisant, stock disponible)
- Calculer une remise selon les règles de l'entreprise
- Vérifier qu'une date de livraison est valide

### Qu'est-ce que l'interface utilisateur (UI) ?

L'**interface utilisateur** est tout ce qui concerne l'affichage et l'interaction avec l'utilisateur.

**Exemples d'UI :**
- Afficher un message à l'écran
- Récupérer du texte dans un champ de saisie
- Changer la couleur d'un label
- Ouvrir une fenêtre de dialogue
- Désactiver un bouton

### Pourquoi les séparer ?

Vous pourriez vous demander : "Pourquoi ne pas tout mettre au même endroit ? C'est plus simple, non ?" En réalité, mélanger UI et logique métier crée de nombreux problèmes. Voyons pourquoi.

## Les problèmes du couplage fort

Lorsque l'UI et la logique métier sont mélangées (on parle de **couplage fort**), vous rencontrez ces problèmes :

### 1. Impossible de tester

**Exemple problématique :**

```pascal
procedure TFormCalcul.ButtonCalculerClick(Sender: TObject);
var
  PrixHT, TauxTVA, PrixTTC: Double;
begin
  // Récupération depuis l'interface
  PrixHT := StrToFloat(EditPrixHT.Text);
  TauxTVA := StrToFloat(EditTVA.Text);

  // Calcul (logique métier)
  PrixTTC := PrixHT * (1 + TauxTVA / 100);

  // Affichage (UI)
  LabelResultat.Caption := FormatFloat('#,##0.00 €', PrixTTC);
end;
```

**Problème** : Comment tester que le calcul de TVA est correct ? Vous devez :
1. Créer un formulaire
2. Remplir les champs
3. Cliquer sur le bouton
4. Lire le label

C'est lent, fastidieux et fragile. Si vous changez le nom du label, vos tests ne fonctionnent plus.

### 2. Impossible de réutiliser

Si vous voulez calculer la TVA ailleurs dans votre application (dans un rapport, dans une API, dans un batch), vous devez **copier-coller** le code du calcul. Et si la règle de calcul change, vous devez modifier à plusieurs endroits.

### 3. Difficile à maintenir

```pascal
procedure TFormCommande.ButtonValiderClick(Sender: TObject);
begin
  // 500 lignes de code mêlant tout...
  if EditNom.Text = '' then
    ShowMessage('Nom requis');

  Total := StrToFloat(EditPrix.Text) * StrToInt(EditQte.Text);

  if (Total > 1000) and (ComboClient.Text = 'Premium') then
    Total := Total * 0.9;

  if DayOfWeek(Now) = 1 then
    Total := Total * 0.95;

  Query.SQL.Text := 'INSERT INTO...';
  // etc...
end;
```

**Problème** : Dans 6 mois, personne ne comprendra ce code. Quelle est la règle métier ? Pourquoi cette remise le lundi ? Impossible à savoir sans analyser ligne par ligne.

### 4. Impossible de changer d'interface

Si demain vous voulez :
- Créer une version mobile de votre application
- Ajouter une API REST
- Faire une version web
- Automatiser via un batch

Vous devez **tout réécrire** car la logique est enfermée dans les formulaires.

### 5. Difficile à faire évoluer

Imaginez que la règle de calcul de TVA change. Au lieu de modifier une fonction, vous devez chercher dans tous les boutons de tous les formulaires où ce calcul est fait.

## Reconnaître UI vs logique métier

Voici un guide simple pour identifier ce qui est UI et ce qui est logique métier :

### C'est de l'UI si...

- ✅ Ça affiche quelque chose (Label, Message, Couleur)
- ✅ Ça récupère des données depuis des contrôles (Edit, ComboBox)
- ✅ Ça active/désactive des composants
- ✅ Ça ouvre/ferme des formulaires
- ✅ Ça dépend du type d'interface (VCL, FMX, Console)
- ✅ Ça concerne l'apparence visuelle

**Exemples :**
```pascal
// UI
ShowMessage('Erreur');
EditNom.Text := 'Jean';
ButtonSave.Enabled := False;
LabelTotal.Caption := '100 €';
if FormClient.ShowModal = mrOk then ...
```

### C'est de la logique métier si...

- ✅ Ça calcule quelque chose
- ✅ Ça valide des données selon des règles métier
- ✅ Ça transforme des données
- ✅ Ça applique des règles de gestion
- ✅ Ça pourrait être utilisé dans n'importe quelle interface
- ✅ Ça accède à la base de données

**Exemples :**
```pascal
// Logique métier
Total := Prix * Quantite * (1 - Remise/100);
if Email.Contains('@') then ...
Client.Age := YearsBetween(Now, Client.DateNaissance);
if Stock < SeuilAlerte then ...
Sauvegarder(Client);
```

### Test simple : la question "Et si ?"

Posez-vous cette question : **"Et si je voulais utiliser cette logique depuis une ligne de commande, sans interface graphique ?"**

- Si c'est possible → C'est de la logique métier
- Si c'est impossible → C'est de l'UI

**Exemple :**
```pascal
// Peut être utilisé sans interface ? OUI → Logique métier
function CalculerTotalTTC(PrixHT, TauxTVA: Double): Double;
begin
  Result := PrixHT * (1 + TauxTVA / 100);
end;

// Peut être utilisé sans interface ? NON → UI
procedure AfficherTotal(Total: Double);
begin
  LabelTotal.Caption := FormatFloat('#,##0.00 €', Total);
end;
```

## Techniques de séparation

Voyons maintenant comment séparer concrètement l'UI de la logique métier.

### Technique 1 : Extraire les fonctions

La technique la plus simple consiste à **extraire la logique dans des fonctions séparées**.

**AVANT (Tout mélangé) :**

```pascal
procedure TFormFacture.ButtonCalculerClick(Sender: TObject);
var
  PrixHT, Quantite, Remise, TauxTVA, Total: Double;
begin
  // Récupération UI
  PrixHT := StrToFloat(EditPrixHT.Text);
  Quantite := StrToFloat(EditQuantite.Text);
  Remise := StrToFloat(EditRemise.Text);
  TauxTVA := StrToFloat(EditTVA.Text);

  // Calcul mélangé avec UI
  Total := PrixHT * Quantite;
  Total := Total - (Total * Remise / 100);
  Total := Total * (1 + TauxTVA / 100);

  // Affichage
  LabelTotal.Caption := FormatFloat('#,##0.00 €', Total);
  if Total > 1000 then
    LabelTotal.Font.Color := clRed
  else
    LabelTotal.Font.Color := clBlack;
end;
```

**APRÈS (Séparé) :**

```pascal
// ========== LOGIQUE MÉTIER (dans une unité séparée) ==========
unit BusinessLogic.Facture;

interface

type
  TCalculateurFacture = class
  public
    class function CalculerSousTotal(PrixUnitaire, Quantite: Double): Double;
    class function AppliquerRemise(Montant, PourcentageRemise: Double): Double;
    class function CalculerTVA(MontantHT, TauxTVA: Double): Double;
    class function CalculerTotal(PrixUnitaire, Quantite, Remise, TauxTVA: Double): Double;
  end;

implementation

class function TCalculateurFacture.CalculerSousTotal(PrixUnitaire, Quantite: Double): Double;
begin
  Result := PrixUnitaire * Quantite;
end;

class function TCalculateurFacture.AppliquerRemise(Montant, PourcentageRemise: Double): Double;
begin
  Result := Montant - (Montant * PourcentageRemise / 100);
end;

class function TCalculateurFacture.CalculerTVA(MontantHT, TauxTVA: Double): Double;
begin
  Result := MontantHT * (1 + TauxTVA / 100);
end;

class function TCalculateurFacture.CalculerTotal(PrixUnitaire, Quantite, Remise, TauxTVA: Double): Double;
var
  SousTotal, AvecRemise: Double;
begin
  SousTotal := CalculerSousTotal(PrixUnitaire, Quantite);
  AvecRemise := AppliquerRemise(SousTotal, Remise);
  Result := CalculerTVA(AvecRemise, TauxTVA);
end;

end.

// ========== UI (dans le formulaire) ==========
procedure TFormFacture.ButtonCalculerClick(Sender: TObject);
var
  PrixHT, Quantite, Remise, TauxTVA, Total: Double;
begin
  // 1. Récupération des données de l'UI
  PrixHT := StrToFloatDef(EditPrixHT.Text, 0);
  Quantite := StrToFloatDef(EditQuantite.Text, 0);
  Remise := StrToFloatDef(EditRemise.Text, 0);
  TauxTVA := StrToFloatDef(EditTVA.Text, 20);

  // 2. Calcul via la logique métier
  Total := TCalculateurFacture.CalculerTotal(PrixHT, Quantite, Remise, TauxTVA);

  // 3. Affichage du résultat
  AfficherTotal(Total);
end;

procedure TFormFacture.AfficherTotal(Total: Double);
begin
  LabelTotal.Caption := FormatFloat('#,##0.00 €', Total);

  // Règle d'affichage (UI, pas métier)
  if Total > 1000 then
    LabelTotal.Font.Color := clRed
  else
    LabelTotal.Font.Color := clBlack;
end;
```

**Avantages :**
- La logique de calcul peut être testée indépendamment
- Elle peut être réutilisée ailleurs
- Le formulaire est beaucoup plus simple
- Les règles métier sont clairement identifiées

**Test unitaire maintenant possible :**
```pascal
procedure TestCalculTotal;
var
  Total: Double;
begin
  Total := TCalculateurFacture.CalculerTotal(100, 2, 10, 20);
  // Attendu : 100 * 2 = 200, -10% = 180, +20% TVA = 216
  Assert(Total = 216, 'Le calcul devrait donner 216');
end;
```

### Technique 2 : Créer des classes métier

Pour des opérations plus complexes, créez des **classes métier** qui encapsulent la logique.

**Exemple : Gestion d'une commande**

```pascal
// ========== LOGIQUE MÉTIER ==========
unit BusinessLogic.Commande;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  TStatutCommande = (scBrouillon, scValidee, scEnPreparation, scExpediee, scLivree, scAnnulee);

  TLigneCommande = class
  private
    FProduit: string;
    FQuantite: Integer;
    FPrixUnitaire: Currency;
  public
    constructor Create(const Produit: string; Quantite: Integer; PrixUnitaire: Currency);
    function CalculerTotal: Currency;

    property Produit: string read FProduit;
    property Quantite: Integer read FQuantite write FQuantite;
    property PrixUnitaire: Currency read FPrixUnitaire write FPrixUnitaire;
  end;

  TCommande = class
  private
    FNumero: string;
    FClient: string;
    FStatut: TStatutCommande;
    FLignes: TObjectList<TLigneCommande>;
    FRemiseClient: Double; // Pourcentage
    FDateCreation: TDateTime;
    FDateLivraison: TDateTime;
  public
    constructor Create(const NumeroCommande: string; const NomClient: string);
    destructor Destroy; override;

    // Méthodes métier
    procedure AjouterLigne(const Produit: string; Quantite: Integer; PrixUnitaire: Currency);
    procedure SupprimerLigne(Index: Integer);
    function CalculerSousTotal: Currency;
    function CalculerRemise: Currency;
    function CalculerTVA(TauxTVA: Double): Currency;
    function CalculerTotalTTC(TauxTVA: Double): Currency;

    // Validation métier
    function PeutEtreValidee(out MessageErreur: string): Boolean;
    function PeutEtreAnnulee(out MessageErreur: string): Boolean;

    // Actions métier
    function Valider(out MessageErreur: string): Boolean;
    function Annuler(out MessageErreur: string): Boolean;

    property Numero: string read FNumero;
    property Client: string read FClient;
    property Statut: TStatutCommande read FStatut;
    property Lignes: TObjectList<TLigneCommande> read FLignes;
    property RemiseClient: Double read FRemiseClient write FRemiseClient;
    property DateCreation: TDateTime read FDateCreation;
    property DateLivraison: TDateTime read FDateLivraison write FDateLivraison;
  end;

implementation

{ TLigneCommande }

constructor TLigneCommande.Create(const Produit: string; Quantite: Integer; PrixUnitaire: Currency);
begin
  inherited Create;
  FProduit := Produit;
  FQuantite := Quantite;
  FPrixUnitaire := PrixUnitaire;
end;

function TLigneCommande.CalculerTotal: Currency;
begin
  Result := FQuantite * FPrixUnitaire;
end;

{ TCommande }

constructor TCommande.Create(const NumeroCommande: string; const NomClient: string);
begin
  inherited Create;
  FNumero := NumeroCommande;
  FClient := NomClient;
  FStatut := scBrouillon;
  FLignes := TObjectList<TLigneCommande>.Create(True);
  FRemiseClient := 0;
  FDateCreation := Now;
  FDateLivraison := 0;
end;

destructor TCommande.Destroy;
begin
  FLignes.Free;
  inherited;
end;

procedure TCommande.AjouterLigne(const Produit: string; Quantite: Integer; PrixUnitaire: Currency);
var
  Ligne: TLigneCommande;
begin
  Ligne := TLigneCommande.Create(Produit, Quantite, PrixUnitaire);
  FLignes.Add(Ligne);
end;

procedure TCommande.SupprimerLigne(Index: Integer);
begin
  if (Index >= 0) and (Index < FLignes.Count) then
    FLignes.Delete(Index);
end;

function TCommande.CalculerSousTotal: Currency;
var
  Ligne: TLigneCommande;
  Total: Currency;
begin
  Total := 0;
  for Ligne in FLignes do
    Total := Total + Ligne.CalculerTotal;
  Result := Total;
end;

function TCommande.CalculerRemise: Currency;
var
  SousTotal: Currency;
begin
  SousTotal := CalculerSousTotal;
  Result := SousTotal * (FRemiseClient / 100);
end;

function TCommande.CalculerTVA(TauxTVA: Double): Currency;
var
  SousTotal, Remise, MontantHT: Currency;
begin
  SousTotal := CalculerSousTotal;
  Remise := CalculerRemise;
  MontantHT := SousTotal - Remise;
  Result := MontantHT * (TauxTVA / 100);
end;

function TCommande.CalculerTotalTTC(TauxTVA: Double): Currency;
var
  SousTotal, Remise, MontantHT, TVA: Currency;
begin
  SousTotal := CalculerSousTotal;
  Remise := CalculerRemise;
  MontantHT := SousTotal - Remise;
  TVA := CalculerTVA(TauxTVA);
  Result := MontantHT + TVA;
end;

function TCommande.PeutEtreValidee(out MessageErreur: string): Boolean;
begin
  Result := False;
  MessageErreur := '';

  // Règle métier : Doit être en brouillon
  if FStatut <> scBrouillon then
  begin
    MessageErreur := 'Seule une commande en brouillon peut être validée';
    Exit;
  end;

  // Règle métier : Doit avoir au moins une ligne
  if FLignes.Count = 0 then
  begin
    MessageErreur := 'La commande doit contenir au moins un produit';
    Exit;
  end;

  // Règle métier : Montant minimum
  if CalculerSousTotal < 10 then
  begin
    MessageErreur := 'Le montant minimum de commande est de 10 €';
    Exit;
  end;

  // Règle métier : Date de livraison obligatoire
  if FDateLivraison = 0 then
  begin
    MessageErreur := 'La date de livraison doit être définie';
    Exit;
  end;

  // Règle métier : Date de livraison dans le futur
  if FDateLivraison < Date then
  begin
    MessageErreur := 'La date de livraison doit être dans le futur';
    Exit;
  end;

  Result := True;
end;

function TCommande.Valider(out MessageErreur: string): Boolean;
begin
  Result := False;

  if not PeutEtreValidee(MessageErreur) then
    Exit;

  FStatut := scValidee;
  Result := True;
end;

function TCommande.PeutEtreAnnulee(out MessageErreur: string): Boolean;
begin
  Result := False;
  MessageErreur := '';

  // Règle métier : Ne peut pas annuler si déjà expédiée
  if FStatut in [scExpediee, scLivree] then
  begin
    MessageErreur := 'Une commande expédiée ou livrée ne peut pas être annulée';
    Exit;
  end;

  // Règle métier : Ne peut pas annuler si déjà annulée
  if FStatut = scAnnulee then
  begin
    MessageErreur := 'La commande est déjà annulée';
    Exit;
  end;

  Result := True;
end;

function TCommande.Annuler(out MessageErreur: string): Boolean;
begin
  Result := False;

  if not PeutEtreAnnulee(MessageErreur) then
    Exit;

  FStatut := scAnnulee;
  Result := True;
end;

end.
```

**Maintenant, l'UI est ultra-simple :**

```pascal
// ========== UI ==========
unit ViewCommande;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Grids, BusinessLogic.Commande;

type
  TFormCommande = class(TForm)
    EditClient: TEdit;
    GridLignes: TStringGrid;
    ButtonValider: TButton;
    ButtonAnnuler: TButton;
    LabelTotal: TLabel;
    DateTimePickerLivraison: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonValiderClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  private
    FCommande: TCommande;
    procedure AfficherCommande;
    procedure MettreAJourTotal;
    procedure AfficherMessage(const Message: string; Erreur: Boolean);
  public
  end;

implementation

uses
  System.SysUtils, Vcl.Dialogs;

procedure TFormCommande.FormCreate(Sender: TObject);
begin
  FCommande := TCommande.Create('CMD-2025-001', EditClient.Text);

  // Ajout de quelques lignes pour l'exemple
  FCommande.AjouterLigne('Produit A', 2, 50.00);
  FCommande.AjouterLigne('Produit B', 1, 30.00);
  FCommande.RemiseClient := 10;

  AfficherCommande;
end;

procedure TFormCommande.FormDestroy(Sender: TObject);
begin
  FCommande.Free;
end;

procedure TFormCommande.AfficherCommande;
var
  I: Integer;
begin
  // Affichage des lignes
  GridLignes.RowCount := FCommande.Lignes.Count + 1;
  for I := 0 to FCommande.Lignes.Count - 1 do
  begin
    GridLignes.Cells[0, I + 1] := FCommande.Lignes[I].Produit;
    GridLignes.Cells[1, I + 1] := IntToStr(FCommande.Lignes[I].Quantite);
    GridLignes.Cells[2, I + 1] := FormatFloat('#,##0.00 €', FCommande.Lignes[I].PrixUnitaire);
    GridLignes.Cells[3, I + 1] := FormatFloat('#,##0.00 €', FCommande.Lignes[I].CalculerTotal);
  end;

  MettreAJourTotal;

  // Activation des boutons selon l'état
  ButtonValider.Enabled := (FCommande.Statut = scBrouillon);
  ButtonAnnuler.Enabled := (FCommande.Statut <> scAnnulee);
end;

procedure TFormCommande.MettreAJourTotal;
var
  Total: Currency;
begin
  Total := FCommande.CalculerTotalTTC(20); // 20% de TVA
  LabelTotal.Caption := Format('Total TTC : %s', [FormatFloat('#,##0.00 €', Total)]);
end;

procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  MessageErreur: string;
begin
  // Récupération de la date de livraison depuis l'UI
  FCommande.DateLivraison := DateTimePickerLivraison.Date;

  // Tentative de validation via la logique métier
  if FCommande.Valider(MessageErreur) then
  begin
    AfficherMessage('Commande validée avec succès !', False);
    AfficherCommande; // Mise à jour de l'affichage
  end
  else
    AfficherMessage(MessageErreur, True);
end;

procedure TFormCommande.ButtonAnnulerClick(Sender: TObject);
var
  MessageErreur: string;
begin
  if MessageDlg('Êtes-vous sûr de vouloir annuler cette commande ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if FCommande.Annuler(MessageErreur) then
    begin
      AfficherMessage('Commande annulée', False);
      AfficherCommande;
    end
    else
      AfficherMessage(MessageErreur, True);
  end;
end;

procedure TFormCommande.AfficherMessage(const Message: string; Erreur: Boolean);
begin
  if Erreur then
    ShowMessage('Erreur : ' + Message)
  else
    ShowMessage(Message);
end;

end.
```

**Avantages de cette approche :**

1. **Toute la logique métier est dans la classe `TCommande`**
   - Règles de validation
   - Calculs
   - Transitions d'état

2. **L'UI ne fait que de l'affichage**
   - Pas de calculs
   - Pas de règles métier
   - Uniquement de la présentation

3. **Testable**
```pascal
procedure TestValidationCommande;
var
  Commande: TCommande;
  Erreur: string;
begin
  Commande := TCommande.Create('TEST-001', 'Client Test');
  try
    // Test : Commande vide ne peut pas être validée
    Assert(not Commande.PeutEtreValidee(Erreur), 'Devrait être invalide');
    Assert(Erreur.Contains('au moins un produit'), 'Message d''erreur incorrect');

    // Test : Avec une ligne, toujours invalide (pas de date)
    Commande.AjouterLigne('Produit', 1, 20);
    Assert(not Commande.PeutEtreValidee(Erreur), 'Devrait être invalide');

    // Test : Avec date future, devrait être valide
    Commande.DateLivraison := Date + 7;
    Assert(Commande.PeutEtreValidee(Erreur), 'Devrait être valide');
  finally
    Commande.Free;
  end;
end;
```

4. **Réutilisable**

La classe `TCommande` peut être utilisée dans :
- Un formulaire VCL (Windows)
- Une application FMX (mobile/multi-plateforme)
- Une API REST
- Un service Windows
- Un batch de traitement

### Technique 3 : Utiliser des services

Pour des opérations encore plus complexes, créez des **services** qui orchestrent plusieurs classes métier.

```pascal
// ========== SERVICE MÉTIER ==========
unit BusinessLogic.CommandeService;

interface

uses
  BusinessLogic.Commande, System.Generics.Collections;

type
  ICommandeService = interface
    ['{GUID-ICI}']
    function CreerCommande(const NomClient: string): TCommande;
    function ChargerCommande(const NumeroCommande: string): TCommande;
    function SauvegarderCommande(Commande: TCommande): Boolean;
    function ValiderCommande(Commande: TCommande; out MessageErreur: string): Boolean;
    function AnnulerCommande(Commande: TCommande; out MessageErreur: string): Boolean;
    function RechercherCommandes(const CritereRecherche: string): TObjectList<TCommande>;
  end;

  TCommandeService = class(TInterfacedObject, ICommandeService)
  private
    function GenererNumeroCommande: string;
    function EnvoyerEmailConfirmation(Commande: TCommande): Boolean;
    function MettreAJourStock(Commande: TCommande): Boolean;
  public
    function CreerCommande(const NomClient: string): TCommande;
    function ChargerCommande(const NumeroCommande: string): TCommande;
    function SauvegarderCommande(Commande: TCommande): Boolean;
    function ValiderCommande(Commande: TCommande; out MessageErreur: string): Boolean;
    function AnnulerCommande(Commande: TCommande; out MessageErreur: string): Boolean;
    function RechercherCommandes(const CritereRecherche: string): TObjectList<TCommande>;
  end;

implementation

uses
  System.SysUtils, DataModuleMain;

function TCommandeService.GenererNumeroCommande: string;
begin
  // Logique de génération de numéro
  Result := Format('CMD-%s-%d', [FormatDateTime('YYYYMMDD', Now), Random(9999)]);
end;

function TCommandeService.CreerCommande(const NomClient: string): TCommande;
var
  NumeroCommande: string;
begin
  NumeroCommande := GenererNumeroCommande;
  Result := TCommande.Create(NumeroCommande, NomClient);
end;

function TCommandeService.ChargerCommande(const NumeroCommande: string): TCommande;
begin
  // Chargement depuis la base de données
  Result := nil;

  with dmMain.QueryCommande do
  begin
    Close;
    SQL.Text := 'SELECT * FROM commandes WHERE numero = :numero';
    ParamByName('numero').AsString := NumeroCommande;
    Open;

    if not Eof then
    begin
      Result := TCommande.Create(
        FieldByName('numero').AsString,
        FieldByName('client').AsString
      );

      // Charger les lignes, etc.
      // ...
    end;
  end;
end;

function TCommandeService.SauvegarderCommande(Commande: TCommande): Boolean;
begin
  Result := False;

  try
    with dmMain.QueryCommande do
    begin
      Close;
      SQL.Text := 'INSERT INTO commandes (numero, client, statut, date_creation) ' +
                  'VALUES (:numero, :client, :statut, :date_creation)';
      ParamByName('numero').AsString := Commande.Numero;
      ParamByName('client').AsString := Commande.Client;
      ParamByName('statut').AsInteger := Ord(Commande.Statut);
      ParamByName('date_creation').AsDateTime := Commande.DateCreation;
      ExecSQL;
    end;

    // Sauvegarder les lignes
    // ...

    Result := True;
  except
    Result := False;
  end;
end;

function TCommandeService.ValiderCommande(Commande: TCommande; out MessageErreur: string): Boolean;
begin
  Result := False;

  // 1. Validation métier
  if not Commande.Valider(MessageErreur) then
    Exit;

  // 2. Mise à jour du stock
  if not MettreAJourStock(Commande) then
  begin
    MessageErreur := 'Erreur lors de la mise à jour du stock';
    Exit;
  end;

  // 3. Sauvegarde en base
  if not SauvegarderCommande(Commande) then
  begin
    MessageErreur := 'Erreur lors de la sauvegarde';
    Exit;
  end;

  // 4. Envoi email de confirmation
  if not EnvoyerEmailConfirmation(Commande) then
  begin
    // Ce n'est pas bloquant
    // Mais on pourrait logger l'erreur
  end;

  Result := True;
end;

function TCommandeService.AnnulerCommande(Commande: TCommande; out MessageErreur: string): Boolean;
begin
  Result := False;

  if not Commande.Annuler(MessageErreur) then
    Exit;

  // Restaurer le stock
  // Envoyer notification
  // etc.

  Result := SauvegarderCommande(Commande);
end;

function TCommandeService.RechercherCommandes(const CritereRecherche: string): TObjectList<TCommande>;
begin
  Result := TObjectList<TCommande>.Create(True);

  // Recherche en base de données
  with dmMain.QueryCommande do
  begin
    Close;
    SQL.Text := 'SELECT * FROM commandes WHERE client LIKE :critere OR numero LIKE :critere';
    ParamByName('critere').AsString := '%' + CritereRecherche + '%';
    Open;

    while not Eof do
    begin
      // Créer et ajouter les commandes
      // ...
      Next;
    end;
  end;
end;

function TCommandeService.MettreAJourStock(Commande: TCommande): Boolean;
begin
  // Logique de mise à jour du stock
  Result := True;
end;

function TCommandeService.EnvoyerEmailConfirmation(Commande: TCommande): Boolean;
begin
  // Logique d'envoi d'email
  Result := True;
end;

end.
```

**L'UI devient encore plus simple :**

```pascal
procedure TFormCommande.ButtonValiderClick(Sender: TObject);
var
  Service: ICommandeService;
  MessageErreur: string;
begin
  Service := TCommandeService.Create;

  // L'UI délègue tout au service
  if Service.ValiderCommande(FCommande, MessageErreur) then
    AfficherMessage('Commande validée !', False)
  else
    AfficherMessage(MessageErreur, True);
end;
```

### Technique 4 : Validation séparée

Créez des **validateurs** pour séparer la logique de validation.

```pascal
// ========== VALIDATEUR ==========
unit BusinessLogic.Validators;

interface

uses
  System.SysUtils, System.RegularExpressions;

type
  TValidationResult = record
    IsValid: Boolean;
    ErrorMessage: string;
    class function Success: TValidationResult; static;
    class function Failure(const Message: string): TValidationResult; static;
  end;

  TClientValidator = class
  public
    class function ValiderNom(const Nom: string): TValidationResult;
    class function ValiderEmail(const Email: string): TValidationResult;
    class function ValiderTelephone(const Telephone: string): TValidationResult;
    class function ValiderClient(const Nom, Email, Telephone: string): TValidationResult;
  end;

implementation

{ TValidationResult }

class function TValidationResult.Success: TValidationResult;
begin
  Result.IsValid := True;
  Result.ErrorMessage := '';
end;

class function TValidationResult.Failure(const Message: string): TValidationResult;
begin
  Result.IsValid := False;
  Result.ErrorMessage := Message;
end;

{ TClientValidator }

class function TClientValidator.ValiderNom(const Nom: string): TValidationResult;
begin
  if Trim(Nom) = '' then
    Exit(TValidationResult.Failure('Le nom est obligatoire'));

  if Nom.Length < 2 then
    Exit(TValidationResult.Failure('Le nom doit contenir au moins 2 caractères'));

  if Nom.Length > 100 then
    Exit(TValidationResult.Failure('Le nom ne peut pas dépasser 100 caractères'));

  Result := TValidationResult.Success;
end;

class function TClientValidator.ValiderEmail(const Email: string): TValidationResult;
var
  EmailRegex: TRegEx;
begin
  if Trim(Email) = '' then
    Exit(TValidationResult.Failure('L''email est obligatoire'));

  EmailRegex := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');

  if not EmailRegex.IsMatch(Email) then
    Exit(TValidationResult.Failure('L''email n''est pas valide'));

  Result := TValidationResult.Success;
end;

class function TClientValidator.ValiderTelephone(const Telephone: string): TValidationResult;
var
  TelephoneNettoye: string;
begin
  TelephoneNettoye := StringReplace(Telephone, ' ', '', [rfReplaceAll]);
  TelephoneNettoye := StringReplace(TelephoneNettoye, '.', '', [rfReplaceAll]);
  TelephoneNettoye := StringReplace(TelephoneNettoye, '-', '', [rfReplaceAll]);

  if TelephoneNettoye.Length < 10 then
    Exit(TValidationResult.Failure('Le téléphone doit contenir au moins 10 chiffres'));

  Result := TValidationResult.Success;
end;

class function TClientValidator.ValiderClient(const Nom, Email, Telephone: string): TValidationResult;
var
  ResultatNom, ResultatEmail, ResultatTelephone: TValidationResult;
begin
  ResultatNom := ValiderNom(Nom);
  if not ResultatNom.IsValid then
    Exit(ResultatNom);

  ResultatEmail := ValiderEmail(Email);
  if not ResultatEmail.IsValid then
    Exit(ResultatEmail);

  ResultatTelephone := ValiderTelephone(Telephone);
  if not ResultatTelephone.IsValid then
    Exit(ResultatTelephone);

  Result := TValidationResult.Success;
end;

end.
```

**Utilisation dans l'UI :**

```pascal
procedure TFormClient.ButtonSaveClick(Sender: TObject);
var
  ValidationResult: TValidationResult;
begin
  // Validation séparée et claire
  ValidationResult := TClientValidator.ValiderClient(
    EditNom.Text,
    EditEmail.Text,
    EditTelephone.Text
  );

  if not ValidationResult.IsValid then
  begin
    ShowMessage(ValidationResult.ErrorMessage);
    Exit;
  end;

  // Sauvegarde...
end;
```

## Règles d'or de la séparation

### 1. Les formulaires ne contiennent pas de logique métier

**Interdit :**
```pascal
procedure TFormCommande.ButtonSaveClick(Sender: TObject);
begin
  // INTERDIT : Règle métier dans le formulaire
  if (Total > 1000) and (DayOfWeek(Now) = 2) then
    Total := Total * 0.95;
end;
```

**Autorisé :**
```pascal
procedure TFormCommande.ButtonSaveClick(Sender: TObject);
begin
  // OK : Délégation à la logique métier
  Total := FCommande.CalculerTotalAvecPromotions;
end;
```

### 2. La logique métier ne connaît pas l'UI

**Interdit :**
```pascal
function TCommande.Valider: Boolean;
begin
  Result := CalculerTotal > 0;
  if Result then
    ShowMessage('Commande validée !'); // INTERDIT : Affichage dans la logique
end;
```

**Autorisé :**
```pascal
function TCommande.Valider(out MessageErreur: string): Boolean;
begin
  Result := CalculerTotal > 0;
  if not Result then
    MessageErreur := 'Le total doit être supérieur à 0';
  // L'UI décide comment afficher le message
end;
```

### 3. Un calcul = une fonction

Chaque calcul métier doit être une fonction séparée, testable et réutilisable.

**Mauvais :**
```pascal
procedure TFormFacture.ButtonCalculerClick(Sender: TObject);
begin
  LabelTotal.Caption := FloatToStr(
    StrToFloat(EditPrix.Text) *
    StrToInt(EditQte.Text) *
    (1 - StrToFloat(EditRemise.Text)/100) *
    1.20
  );
end;
```

**Bon :**
```pascal
function CalculerTotal(Prix, Quantite, Remise: Double): Double;
var
  SousTotal, AvecRemise: Double;
begin
  SousTotal := Prix * Quantite;
  AvecRemise := SousTotal * (1 - Remise/100);
  Result := AvecRemise * 1.20; // TVA
end;

procedure TFormFacture.ButtonCalculerClick(Sender: TObject);
var
  Total: Double;
begin
  Total := CalculerTotal(
    StrToFloatDef(EditPrix.Text, 0),
    StrToFloatDef(EditQte.Text, 0),
    StrToFloatDef(EditRemise.Text, 0)
  );
  LabelTotal.Caption := FormatFloat('#,##0.00 €', Total);
end;
```

### 4. Une validation = une fonction

Chaque règle de validation doit être une fonction indépendante.

**Mauvais :**
```pascal
procedure TFormClient.ButtonSaveClick(Sender: TObject);
begin
  if (EditNom.Text = '') or (Length(EditNom.Text) < 2) or
     (EditEmail.Text = '') or (not EditEmail.Text.Contains('@')) then
  begin
    ShowMessage('Données invalides');
    Exit;
  end;
end;
```

**Bon :**
```pascal
function ValiderNom(const Nom: string; out Erreur: string): Boolean;
begin
  if Trim(Nom) = '' then
  begin
    Erreur := 'Le nom est obligatoire';
    Exit(False);
  end;

  if Nom.Length < 2 then
  begin
    Erreur := 'Le nom doit faire au moins 2 caractères';
    Exit(False);
  end;

  Result := True;
end;
```

### 5. Ne jamais copier-coller de la logique métier

Si vous avez besoin de la même logique à deux endroits :
1. Créez une fonction
2. Appelez cette fonction depuis les deux endroits

## Organisation des unités

Voici une organisation recommandée pour séparer clairement UI et logique :

```
MonProjet/
├── Source/
│   ├── UI/                        ← Tout ce qui est interface
│   │   ├── Forms/
│   │   │   ├── MainForm.pas
│   │   │   ├── ClientForm.pas
│   │   │   └── CommandeForm.pas
│   │   └── Dialogs/
│   │       └── ConfirmDialog.pas
│   │
│   ├── Business/                  ← Toute la logique métier
│   │   ├── Models/
│   │   │   ├── Client.pas
│   │   │   ├── Commande.pas
│   │   │   └── Produit.pas
│   │   ├── Services/
│   │   │   ├── ClientService.pas
│   │   │   └── CommandeService.pas
│   │   ├── Validators/
│   │   │   ├── ClientValidator.pas
│   │   │   └── CommandeValidator.pas
│   │   └── Calculators/
│   │       ├── PriceCalculator.pas
│   │       └── TaxCalculator.pas
│   │
│   └── Data/                      ← Accès aux données
│       ├── DataModule.pas
│       └── Repositories/
│           ├── ClientRepository.pas
│           └── CommandeRepository.pas
```

## Bénéfices concrets

Voyons les avantages réels de cette séparation sur un exemple concret.

### Scénario : Changement de règle métier

**Situation** : La TVA passe de 20% à 21%.

**Sans séparation :**
```
Vous devez chercher dans TOUS les formulaires où le calcul est fait
Vous trouvez 15 endroits différents
Vous modifiez 14 sur 15 (vous en oubliez un)
→ Bug en production : une facture mal calculée
```

**Avec séparation :**
```
Vous modifiez UNE SEULE fonction :

function CalculerTVA(MontantHT: Currency): Currency;
begin
  Result := MontantHT * 0.21; // Était 0.20
end;

→ Tout est mis à jour automatiquement partout
```

### Scénario : Nouvelle interface

**Situation** : Vous devez créer une application mobile en plus de l'application desktop.

**Sans séparation :**
```
Toute la logique est dans les formulaires VCL
→ Vous devez TOUT réécrire pour FMX
→ Double maintenance : 2 versions du même code
```

**Avec séparation :**
```
La logique métier est réutilisable telle quelle
→ Vous ne créez que les formulaires FMX
→ Une seule logique métier pour les deux interfaces
```

### Scénario : Tests automatisés

**Situation** : Vous voulez ajouter des tests unitaires.

**Sans séparation :**
```
Impossible de tester sans créer les formulaires
Les tests sont lents et fragiles
→ Vous abandonnez les tests
```

**Avec séparation :**
```
Chaque fonction métier est testable indépendamment
Les tests sont rapides (millisecondes)
→ Vous testez tout facilement
```

## Cas pratiques de refactoring

### Cas 1 : Extraire un calcul complexe

**Avant :**
```pascal
procedure TFormDevis.ButtonCalculerClick(Sender: TObject);
var
  Montant: Double;
begin
  Montant := StrToFloat(EditMontant.Text);

  // Remise selon montant
  if Montant > 10000 then
    Montant := Montant * 0.85
  else if Montant > 5000 then
    Montant := Montant * 0.90
  else if Montant > 1000 then
    Montant := Montant * 0.95;

  // Remise fidélité
  if CheckBoxClientFidele.Checked then
    Montant := Montant * 0.97;

  // Remise saisonnière
  if (MonthOf(Now) = 1) or (MonthOf(Now) = 7) then
    Montant := Montant * 0.98;

  LabelTotal.Caption := FormatFloat('#,##0.00 €', Montant);
end;
```

**Après :**
```pascal
// ========== LOGIQUE MÉTIER ==========
unit BusinessLogic.Remises;

interface

type
  TCalculateurRemises = class
  public
    class function CalculerRemiseMontant(Montant: Currency): Currency;
    class function CalculerRemiseFidelite(Montant: Currency; EstFidele: Boolean): Currency;
    class function CalculerRemiseSaisonniere(Montant: Currency; Mois: Integer): Currency;
    class function CalculerMontantFinal(MontantInitial: Currency; EstClientFidele: Boolean): Currency;
  end;

implementation

class function TCalculateurRemises.CalculerRemiseMontant(Montant: Currency): Currency;
begin
  // Règle métier : remise par paliers
  if Montant > 10000 then
    Result := Montant * 0.85  // 15% de remise
  else if Montant > 5000 then
    Result := Montant * 0.90  // 10% de remise
  else if Montant > 1000 then
    Result := Montant * 0.95  // 5% de remise
  else
    Result := Montant;        // Pas de remise
end;

class function TCalculateurRemises.CalculerRemiseFidelite(Montant: Currency; EstFidele: Boolean): Currency;
begin
  // Règle métier : 3% pour les clients fidèles
  if EstFidele then
    Result := Montant * 0.97
  else
    Result := Montant;
end;

class function TCalculateurRemises.CalculerRemiseSaisonniere(Montant: Currency; Mois: Integer): Currency;
begin
  // Règle métier : 2% en janvier et juillet
  if (Mois = 1) or (Mois = 7) then
    Result := Montant * 0.98
  else
    Result := Montant;
end;

class function TCalculateurRemises.CalculerMontantFinal(MontantInitial: Currency; EstClientFidele: Boolean): Currency;
var
  Montant: Currency;
begin
  // Application successive des remises
  Montant := MontantInitial;
  Montant := CalculerRemiseMontant(Montant);
  Montant := CalculerRemiseFidelite(Montant, EstClientFidele);
  Montant := CalculerRemiseSaisonniere(Montant, MonthOf(Now));
  Result := Montant;
end;

end.

// ========== UI ==========
procedure TFormDevis.ButtonCalculerClick(Sender: TObject);
var
  MontantInitial, MontantFinal: Currency;
  EstFidele: Boolean;
begin
  // Récupération des données UI
  MontantInitial := StrToFloatDef(EditMontant.Text, 0);
  EstFidele := CheckBoxClientFidele.Checked;

  // Calcul via logique métier
  MontantFinal := TCalculateurRemises.CalculerMontantFinal(MontantInitial, EstFidele);

  // Affichage
  LabelTotal.Caption := FormatFloat('#,##0.00 €', MontantFinal);
end;
```

**Tests possibles :**
```pascal
procedure TestRemiseMontant;
begin
  Assert(TCalculateurRemises.CalculerRemiseMontant(500) = 500, 'Pas de remise sous 1000');
  Assert(TCalculateurRemises.CalculerRemiseMontant(2000) = 1900, 'Remise 5% entre 1000 et 5000');
  Assert(TCalculateurRemises.CalculerRemiseMontant(7000) = 6300, 'Remise 10% entre 5000 et 10000');
  Assert(TCalculateurRemises.CalculerRemiseMontant(15000) = 12750, 'Remise 15% au dessus de 10000');
end;
```

### Cas 2 : Extraire une validation complexe

**Avant :**
```pascal
procedure TFormInscription.ButtonValiderClick(Sender: TObject);
begin
  // Validation inline complexe
  if (EditNom.Text = '') or (EditPrenom.Text = '') or
     (EditEmail.Text = '') or (not EditEmail.Text.Contains('@')) or
     (EditMotDePasse.Text = '') or (Length(EditMotDePasse.Text) < 8) or
     (EditMotDePasse.Text <> EditConfirmation.Text) or
     (not CheckBoxCGU.Checked) or
     (YearsBetween(Now, DateTimePickerNaissance.Date) < 18) then
  begin
    ShowMessage('Formulaire invalide');
    Exit;
  end;

  // Sauvegarde...
end;
```

**Après :**
```pascal
// ========== VALIDATEUR ==========
unit BusinessLogic.InscriptionValidator;

interface

type
  TInscriptionData = record
    Nom: string;
    Prenom: string;
    Email: string;
    MotDePasse: string;
    ConfirmationMotDePasse: string;
    DateNaissance: TDateTime;
    AccepteCGU: Boolean;
  end;

  TInscriptionValidator = class
  private
    class function ValiderNomPrenom(const Valeur, Champ: string; out Erreur: string): Boolean;
    class function ValiderEmail(const Email: string; out Erreur: string): Boolean;
    class function ValiderMotDePasse(const MotDePasse, Confirmation: string; out Erreur: string): Boolean;
    class function ValiderAge(DateNaissance: TDateTime; out Erreur: string): Boolean;
    class function ValiderCGU(Accepte: Boolean; out Erreur: string): Boolean;
  public
    class function Valider(const Data: TInscriptionData; out Erreur: string): Boolean;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.RegularExpressions;

class function TInscriptionValidator.ValiderNomPrenom(const Valeur, Champ: string; out Erreur: string): Boolean;
begin
  if Trim(Valeur) = '' then
  begin
    Erreur := Format('Le %s est obligatoire', [Champ]);
    Exit(False);
  end;

  if Valeur.Length < 2 then
  begin
    Erreur := Format('Le %s doit contenir au moins 2 caractères', [Champ]);
    Exit(False);
  end;

  Result := True;
end;

class function TInscriptionValidator.ValiderEmail(const Email: string; out Erreur: string): Boolean;
var
  EmailRegex: TRegEx;
begin
  if Trim(Email) = '' then
  begin
    Erreur := 'L''email est obligatoire';
    Exit(False);
  end;

  EmailRegex := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
  if not EmailRegex.IsMatch(Email) then
  begin
    Erreur := 'L''email n''est pas valide';
    Exit(False);
  end;

  Result := True;
end;

class function TInscriptionValidator.ValiderMotDePasse(const MotDePasse, Confirmation: string; out Erreur: string): Boolean;
begin
  if Trim(MotDePasse) = '' then
  begin
    Erreur := 'Le mot de passe est obligatoire';
    Exit(False);
  end;

  if MotDePasse.Length < 8 then
  begin
    Erreur := 'Le mot de passe doit contenir au moins 8 caractères';
    Exit(False);
  end;

  if MotDePasse <> Confirmation then
  begin
    Erreur := 'Les mots de passe ne correspondent pas';
    Exit(False);
  end;

  Result := True;
end;

class function TInscriptionValidator.ValiderAge(DateNaissance: TDateTime; out Erreur: string): Boolean;
var
  Age: Integer;
begin
  Age := YearsBetween(Now, DateNaissance);

  if Age < 18 then
  begin
    Erreur := 'Vous devez avoir au moins 18 ans pour vous inscrire';
    Exit(False);
  end;

  if Age > 120 then
  begin
    Erreur := 'La date de naissance semble incorrecte';
    Exit(False);
  end;

  Result := True;
end;

class function TInscriptionValidator.ValiderCGU(Accepte: Boolean; out Erreur: string): Boolean;
begin
  if not Accepte then
  begin
    Erreur := 'Vous devez accepter les conditions générales d''utilisation';
    Exit(False);
  end;

  Result := True;
end;

class function TInscriptionValidator.Valider(const Data: TInscriptionData; out Erreur: string): Boolean;
begin
  // Validation dans un ordre logique
  if not ValiderNomPrenom(Data.Nom, 'nom', Erreur) then
    Exit(False);

  if not ValiderNomPrenom(Data.Prenom, 'prénom', Erreur) then
    Exit(False);

  if not ValiderEmail(Data.Email, Erreur) then
    Exit(False);

  if not ValiderMotDePasse(Data.MotDePasse, Data.ConfirmationMotDePasse, Erreur) then
    Exit(False);

  if not ValiderAge(Data.DateNaissance, Erreur) then
    Exit(False);

  if not ValiderCGU(Data.AccepteCGU, Erreur) then
    Exit(False);

  Result := True;
end;

end.

// ========== UI ==========
procedure TFormInscription.ButtonValiderClick(Sender: TObject);
var
  Data: TInscriptionData;
  Erreur: string;
begin
  // Préparation des données depuis l'UI
  Data.Nom := EditNom.Text;
  Data.Prenom := EditPrenom.Text;
  Data.Email := EditEmail.Text;
  Data.MotDePasse := EditMotDePasse.Text;
  Data.ConfirmationMotDePasse := EditConfirmation.Text;
  Data.DateNaissance := DateTimePickerNaissance.Date;
  Data.AccepteCGU := CheckBoxCGU.Checked;

  // Validation via logique métier
  if not TInscriptionValidator.Valider(Data, Erreur) then
  begin
    ShowMessage(Erreur);
    Exit;
  end;

  // Sauvegarde...
  ShowMessage('Inscription réussie !');
end;
```

## Erreurs courantes à éviter

### Erreur 1 : ShowMessage dans la logique métier

**Mauvais :**
```pascal
function SauvegarderClient(Client: TClient): Boolean;
begin
  Result := True;
  try
    // Sauvegarde...
  except
    ShowMessage('Erreur de sauvegarde'); // NON !
    Result := False;
  end;
end;
```

**Bon :**
```pascal
function SauvegarderClient(Client: TClient; out MessageErreur: string): Boolean;
begin
  Result := True;
  try
    // Sauvegarde...
  except
    on E: Exception do
    begin
      MessageErreur := 'Erreur de sauvegarde : ' + E.Message;
      Result := False;
    end;
  end;
end;

// Dans l'UI :
if not SauvegarderClient(Client, Erreur) then
  ShowMessage(Erreur);
```

### Erreur 2 : Références VCL dans la logique

**Mauvais :**
```pascal
type
  TClient = class
  private
    FFormulaire: TForm; // NON ! Référence à l'UI
  public
    procedure Afficher;
  end;
```

**Bon :**
```pascal
type
  TClient = class
  private
    FNom: string;
    FEmail: string;
  public
    // Pas de méthode d'affichage dans le modèle
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
  end;

// L'affichage est dans l'UI :
procedure TFormClient.AfficherClient(Client: TClient);
begin
  LabelNom.Caption := Client.Nom;
  LabelEmail.Caption := Client.Email;
end;
```

### Erreur 3 : Accès direct aux composants UI

**Mauvais :**
```pascal
function Valider: Boolean;
begin
  Result := (EditNom.Text <> '') and (EditEmail.Text <> ''); // NON !
end;
```

**Bon :**
```pascal
function Valider(const Nom, Email: string): Boolean;
begin
  Result := (Trim(Nom) <> '') and (Trim(Email) <> '');
end;

// Dans l'UI :
if Valider(EditNom.Text, EditEmail.Text) then
  // ...
```

### Erreur 4 : Logique dupliquée

**Mauvais :**
```pascal
// Dans FormClient
procedure ButtonCalculerClick(Sender: TObject);
begin
  Total := Prix * (1 + TVA/100); // Calcul en dur
end;

// Dans FormFacture
procedure ButtonTotalClick(Sender: TObject);
begin
  MontantTTC := MontantHT * (1 + TauxTVA/100); // Même calcul dupliqué
end;
```

**Bon :**
```pascal
// Une seule fonction réutilisable
function CalculerTTC(MontantHT, TauxTVA: Currency): Currency;
begin
  Result := MontantHT * (1 + TauxTVA / 100);
end;

// Utilisée partout
Total := CalculerTTC(Prix, TVA);
MontantTTC := CalculerTTC(MontantHT, TauxTVA);
```

## Conclusion

La séparation entre l'interface utilisateur et la logique métier est un principe fondamental pour créer des applications de qualité professionnelle. Cette séparation offre de nombreux avantages :

**Avantages immédiats :**
- Code plus clair et plus lisible
- Moins de bugs
- Développement plus rapide

**Avantages à moyen terme :**
- Facilité de maintenance
- Possibilité de tester
- Réutilisation du code

**Avantages à long terme :**
- Évolutivité de l'application
- Portabilité vers d'autres plateformes
- Pérennité du code

**Points clés à retenir :**

1. **L'UI affiche, la logique calcule** - Ne mélangez jamais les deux
2. **Pas de ShowMessage dans la logique** - Retournez des messages, l'UI décide comment les afficher
3. **Pas de référence VCL dans la logique** - La logique doit être utilisable sans interface
4. **Une fonction par calcul** - Chaque règle métier = une fonction testable
5. **Pas de duplication** - Créez des fonctions réutilisables
6. **Organisation claire** - Séparez physiquement UI et logique dans des dossiers différents

En appliquant ces principes dès le début de vos projets, vous construirez des applications solides, maintenables et évolutives. C'est un investissement qui paye rapidement et qui fait toute la différence entre un code amateur et un code professionnel.

Dans la prochaine section, nous verrons comment gérer proprement la configuration de vos applications, un autre aspect crucial de l'architecture logicielle.

⏭️ [Gestion de la configuration](/18-architecture-et-bonnes-pratiques/04-gestion-de-la-configuration.md)
