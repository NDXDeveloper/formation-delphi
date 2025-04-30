# 18.3 Séparation UI / logique métier

## Introduction

L'un des principes fondamentaux de la conception logicielle moderne est la séparation claire entre l'interface utilisateur (UI) et la logique métier. Cette séparation présente de nombreux avantages qui rendent votre application plus maintenable, plus testable et plus évolutive.

Dans ce chapitre, nous allons explorer comment mettre en œuvre cette séparation dans vos applications Delphi.

## Pourquoi séparer l'UI de la logique métier ?

Avant de plonger dans le "comment", comprenons le "pourquoi" :

1. **Maintenabilité améliorée** : Lorsque vous modifiez l'interface utilisateur, vous ne risquez pas d'affecter accidentellement la logique métier, et vice versa.

2. **Testabilité accrue** : Vous pouvez tester votre logique métier sans avoir besoin d'interagir avec l'interface utilisateur.

3. **Réutilisation du code** : La même logique métier peut être utilisée par différentes interfaces (desktop, mobile, web).

4. **Collaboration en équipe** : Les développeurs peuvent travailler en parallèle sur l'UI et la logique métier.

5. **Évolution facilitée** : Vous pouvez refondre complètement l'interface sans toucher à la logique métier.

## Le problème du code "monolithique"

Beaucoup de développeurs Delphi, surtout débutants, ont tendance à placer tout leur code directement dans les gestionnaires d'événements des formulaires :

```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  Prix, Quantite, Total: Double;
begin
  // Validation des entrées
  if not TryStrToFloat(EditPrix.Text, Prix) then
  begin
    ShowMessage('Prix invalide');
    Exit;
  end;

  if not TryStrToFloat(EditQuantite.Text, Quantite) then
  begin
    ShowMessage('Quantité invalide');
    Exit;
  end;

  // Calcul du prix
  Total := Prix * Quantite;
  if CheckBoxRemise.Checked then
    Total := Total * 0.9; // 10% de remise

  // Affichage du résultat
  LabelTotal.Caption := 'Total : ' + FormatFloat('#,##0.00 €', Total);
end;
```

Ce code mélange trois préoccupations différentes :
- Récupération et validation des données de l'interface utilisateur
- Calcul du prix (logique métier)
- Affichage du résultat

## Séparation en couches

Pour bien séparer l'UI de la logique métier, on utilise généralement une architecture en couches :

### 1. Couche Présentation (UI)
Responsable uniquement de l'interface utilisateur, de la collecte des entrées et de l'affichage des sorties.

### 2. Couche Métier (Business Logic)
Contient toutes les règles et processus métier, indépendamment de l'interface.

### 3. Couche Données (Data Access)
Gère le stockage et la récupération des données (bases de données, fichiers, services web).

## Implémentation pratique

Voici comment refactoriser l'exemple précédent :

### Étape 1 : Créer une unité pour la logique métier

```pascal
unit CalculPrixLogic;

interface

type
  TCalculateur = class
  private
    FPrix: Double;
    FQuantite: Double;
    FAppliquerRemise: Boolean;
  public
    constructor Create;
    procedure SetPrix(const Value: Double);
    procedure SetQuantite(const Value: Double);
    procedure SetAppliquerRemise(const Value: Boolean);
    function CalculerTotal: Double;
    property Prix: Double read FPrix write SetPrix;
    property Quantite: Double read FQuantite write SetQuantite;
    property AppliquerRemise: Boolean read FAppliquerRemise write SetAppliquerRemise;
  end;

implementation

constructor TCalculateur.Create;
begin
  inherited;
  FPrix := 0;
  FQuantite := 0;
  FAppliquerRemise := False;
end;

procedure TCalculateur.SetPrix(const Value: Double);
begin
  if Value < 0 then
    raise Exception.Create('Le prix ne peut pas être négatif')
  else
    FPrix := Value;
end;

procedure TCalculateur.SetQuantite(const Value: Double);
begin
  if Value < 0 then
    raise Exception.Create('La quantité ne peut pas être négative')
  else
    FQuantite := Value;
end;

procedure TCalculateur.SetAppliquerRemise(const Value: Boolean);
begin
  FAppliquerRemise := Value;
end;

function TCalculateur.CalculerTotal: Double;
var
  Total: Double;
begin
  Total := FPrix * FQuantite;
  if FAppliquerRemise then
    Total := Total * 0.9; // 10% de remise
  Result := Total;
end;

end.
```

### Étape 2 : Utiliser cette logique dans le formulaire

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CalculPrixLogic;

type
  TForm1 = class(TForm)
    EditPrix: TEdit;
    EditQuantite: TEdit;
    CheckBoxRemise: TCheckBox;
    ButtonCalculer: TButton;
    LabelTotal: TLabel;
    procedure ButtonCalculerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCalculateur: TCalculateur;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCalculateur := TCalculateur.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCalculateur.Free;
end;

procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  Prix, Quantite, Total: Double;
begin
  // Responsabilité UI : Validation des entrées
  if not TryStrToFloat(EditPrix.Text, Prix) then
  begin
    ShowMessage('Prix invalide');
    Exit;
  end;

  if not TryStrToFloat(EditQuantite.Text, Quantite) then
  begin
    ShowMessage('Quantité invalide');
    Exit;
  end;

  try
    // Transfert des données UI vers la logique métier
    FCalculateur.Prix := Prix;
    FCalculateur.Quantite := Quantite;
    FCalculateur.AppliquerRemise := CheckBoxRemise.Checked;

    // Appel de la logique métier
    Total := FCalculateur.CalculerTotal;

    // Responsabilité UI : Affichage du résultat
    LabelTotal.Caption := 'Total : ' + FormatFloat('#,##0.00 €', Total);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.
```

## Utilisation de patterns de conception

Pour les applications plus complexes, vous pouvez utiliser des patterns de conception formels :

### Pattern MVC (Model-View-Controller)

- **Modèle** : Représente les données et la logique métier
- **Vue** : Gère l'affichage et l'interface utilisateur
- **Contrôleur** : Coordonne le modèle et la vue, gère les événements

### Pattern MVVM (Model-View-ViewModel)

- **Modèle** : Représente les données et la logique métier
- **Vue** : Interface utilisateur pure
- **ViewModel** : Adapte le modèle pour la vue et gère la logique de présentation

## Exemple MVVM simplifié dans Delphi

```pascal
// Modèle - Classe de données et logique métier
unit ProductModel;

interface

type
  TProduct = class
  private
    FName: string;
    FPrice: Double;
    FQuantity: Integer;
  public
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Quantity: Integer read FQuantity write FQuantity;
    function GetTotalValue: Double;
  end;

implementation

function TProduct.GetTotalValue: Double;
begin
  Result := FPrice * FQuantity;
end;

end.

// ViewModel - Adaptateur entre UI et modèle
unit ProductViewModel;

interface

uses
  System.SysUtils, ProductModel;

type
  TProductViewModel = class
  private
    FProduct: TProduct;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetPrice: string;
    procedure SetPrice(const Value: string);
    function GetQuantity: string;
    procedure SetQuantity(const Value: string);
    function GetTotalValue: string;
  public
    constructor Create(AProduct: TProduct);
    property Name: string read GetName write SetName;
    property Price: string read GetPrice write SetPrice;
    property Quantity: string read GetQuantity write SetQuantity;
    property TotalValue: string read GetTotalValue;
  end;

implementation

constructor TProductViewModel.Create(AProduct: TProduct);
begin
  inherited Create;
  FProduct := AProduct;
end;

function TProductViewModel.GetName: string;
begin
  Result := FProduct.Name;
end;

procedure TProductViewModel.SetName(const Value: string);
begin
  FProduct.Name := Value;
end;

function TProductViewModel.GetPrice: string;
begin
  Result := FormatFloat('#,##0.00 €', FProduct.Price);
end;

procedure TProductViewModel.SetPrice(const Value: string);
var
  Price: Double;
begin
  if TryStrToFloat(Value, Price) then
    FProduct.Price := Price;
end;

function TProductViewModel.GetQuantity: string;
begin
  Result := IntToStr(FProduct.Quantity);
end;

procedure TProductViewModel.SetQuantity(const Value: string);
var
  Quantity: Integer;
begin
  if TryStrToInt(Value, Quantity) then
    FProduct.Quantity := Quantity;
end;

function TProductViewModel.GetTotalValue: string;
begin
  Result := FormatFloat('#,##0.00 €', FProduct.GetTotalValue);
end;
```

## Live Bindings pour la séparation UI/logique

Delphi offre la technologie "Live Bindings" qui permet de lier directement les propriétés de vos objets métier aux composants visuels, renforçant ainsi la séparation.

```pascal
// Dans le formulaire, après avoir créé le ViewModel
procedure TForm1.SetupBindings;
begin
  BindingsList1.AddBinding(
    EditName, 'Text',
    FViewModel, 'Name',
    True, // Bidirectionnel
    TBindings.NotifyOutputs
  );

  BindingsList1.AddBinding(
    EditPrice, 'Text',
    FViewModel, 'Price',
    True,
    TBindings.NotifyOutputs
  );

  BindingsList1.AddBinding(
    EditQuantity, 'Text',
    FViewModel, 'Quantity',
    True,
    TBindings.NotifyOutputs
  );

  BindingsList1.AddBinding(
    LabelTotal, 'Caption',
    FViewModel, 'TotalValue',
    False, // Unidirectionnel (affichage seulement)
    TBindings.NotifyOutputs
  );
end;
```

## Bonnes pratiques

1. **Commencez tôt** : Ne repoussez pas la séparation à plus tard. Dès le début du projet, structurez votre code avec cette séparation en tête.

2. **Interfaces** : Utilisez des interfaces pour définir les contrats entre les couches, ce qui renforce encore la séparation.

3. **Injection de dépendances** : N'instanciez pas directement vos classes métier, mais utilisez l'injection de dépendances.

4. **Tests unitaires** : Profitez de cette séparation pour écrire des tests unitaires pour votre logique métier.

5. **Évitez les références circulaires** : Les unités de logique métier ne devraient pas référencer les unités d'interface utilisateur.

## Conclusion

La séparation entre l'interface utilisateur et la logique métier est une pratique essentielle pour tout développeur Delphi souhaitant créer des applications robustes et maintenables. Cette approche demande un peu plus d'effort initial, mais elle vous épargnera des heures de debugging et facilitera grandement l'évolution de votre application à long terme.

En adoptant cette discipline dès le début de vos projets, vous construirez des applications plus modulaires, plus testables et plus faciles à faire évoluer au fil du temps.

Dans les prochains chapitres, nous explorerons plus en détail les patterns architecturaux comme MVC et MVVM, ainsi que les techniques avancées d'organisation de code.
