# 4.8 Développement de composants personnalisés

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'une des forces de Delphi est sa capacité à être étendu grâce à la création de composants personnalisés. Ces composants peuvent ensuite être utilisés comme n'importe quel autre composant de la palette, ce qui vous permet de réutiliser facilement votre code et d'enrichir votre environnement de développement. Dans cette section, nous allons découvrir comment créer vos propres composants personnalisés.

## Pourquoi créer des composants personnalisés ?

Avant de nous lancer dans le développement, voici quelques raisons pour lesquelles vous pourriez vouloir créer vos propres composants :

- **Réutilisation du code** : encapsulez une fonctionnalité que vous utilisez souvent
- **Simplification** : masquez la complexité derrière une interface simple
- **Cohérence** : assurez-vous que vos fonctionnalités sont utilisées de manière cohérente
- **Organisation** : regroupez des contrôles liés en un seul composant
- **Distribution** : partagez facilement vos fonctionnalités avec d'autres développeurs

## Les différents types de composants personnalisés

Il existe plusieurs approches pour créer des composants personnalisés dans Delphi :

1. **Composants non visuels** : ils n'ont pas d'interface visible mais fournissent des fonctionnalités
2. **Contrôles simples** : ils étendent les contrôles existants en ajoutant des fonctionnalités
3. **Contrôles composés** : ils combinent plusieurs contrôles existants
4. **Contrôles entièrement personnalisés** : ils dessinent leur propre interface et gèrent leurs propres événements

Commençons par comprendre la structure d'un composant et comment le créer.

## Structure de base d'un composant

Un composant Delphi est essentiellement une classe dérivée de `TComponent` ou d'une de ses classes dérivées. Voici la structure typique :

```pascal
unit MaUnitComposant;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics;

type
  TMonComposant = class(TComponent)  // Ou TGraphicControl, TWinControl, etc.
  private
    // Champs privés et méthodes
    FMaPropriete: Integer;
    procedure SetMaPropriete(const Value: Integer);
  protected
    // Méthodes protégées pour la dérivation
    procedure Paint; override;  // Pour les contrôles visuels
  public
    // Méthodes et propriétés publiques
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Propriétés visibles dans l'Inspecteur d'objets
    property MaPropriete: Integer read FMaPropriete write SetMaPropriete;
  end;

procedure Register;

implementation

{ TMonComposant }

constructor TMonComposant.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Initialisation
  FMaPropriete := 0;
end;

destructor TMonComposant.Destroy;
begin
  // Nettoyage
  inherited Destroy;
end;

procedure TMonComposant.SetMaPropriete(const Value: Integer);
begin
  if FMaPropriete <> Value then
  begin
    FMaPropriete := Value;
    // Actions suite au changement de la propriété
  end;
end;

procedure Register;
begin
  RegisterComponents('Ma Palette', [TMonComposant]);
end;

end.
```

Les sections clés sont :

- **private** : champs et méthodes internes
- **protected** : méthodes que les classes dérivées peuvent surcharger
- **public** : interface accessible à tous
- **published** : propriétés visibles dans l'Inspecteur d'objets

La procédure `Register` est particulièrement importante : elle indique à Delphi dans quelle palette placer votre composant.

## Créer un composant non visuel simple

Commençons par un exemple simple : un composant de minuterie amélioré qui offre plus de fonctionnalités que le `TTimer` standard.

### Étape 1 : Créer un paquet pour vos composants

1. Choisissez **Fichier** > **Nouveau** > **Paquet**
2. Enregistrez-le sous un nom significatif (par exemple, "MesComposants.dpk")
3. Dans l'explorateur de projets, faites un clic droit sur le nom du paquet et choisissez **Ajouter...**
4. Sélectionnez **Nouveau fichier** puis **Unité** et enregistrez-la (par exemple, "UTimerPlus.pas")

### Étape 2 : Coder le composant

```pascal
unit UTimerPlus;

interface

uses
  System.SysUtils, System.Classes;

type
  TCountDownEvent = procedure(Sender: TObject; SecondesRestantes: Integer) of object;
  TTimerCompleteEvent = procedure(Sender: TObject) of object;

  TTimerPlus = class(TComponent)
  private
    FTimer: TTimer;
    FDuree: Integer;  // Durée en secondes
    FRestant: Integer;  // Temps restant
    FPause: Boolean;
    FOnCountDown: TCountDownEvent;
    FOnComplete: TTimerCompleteEvent;
    procedure TimerTick(Sender: TObject);
    procedure SetDuree(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    property TempsRestant: Integer read FRestant;
  published
    property Duree: Integer read FDuree write SetDuree default 60;
    property Enabled: Boolean read GetEnabled write SetEnabled default False;
    property OnCountDown: TCountDownEvent read FOnCountDown write FOnCountDown;
    property OnComplete: TTimerCompleteEvent read FOnComplete write FOnComplete;
  end;

procedure Register;

implementation

{ TTimerPlus }

constructor TTimerPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;  // 1 seconde
  FTimer.OnTimer := TimerTick;
  FDuree := 60;  // Par défaut : 60 secondes
  FRestant := FDuree;
  FPause := False;
end;

destructor TTimerPlus.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

function TTimerPlus.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TTimerPlus.Pause;
begin
  if FTimer.Enabled and not FPause then
  begin
    FTimer.Enabled := False;
    FPause := True;
  end;
end;

procedure TTimerPlus.Resume;
begin
  if FPause then
  begin
    FTimer.Enabled := True;
    FPause := False;
  end;
end;

procedure TTimerPlus.SetDuree(const Value: Integer);
begin
  if Value > 0 then
  begin
    FDuree := Value;
    if not FTimer.Enabled then
      FRestant := FDuree;
  end;
end;

procedure TTimerPlus.SetEnabled(const Value: Boolean);
begin
  if Value <> FTimer.Enabled then
  begin
    FTimer.Enabled := Value;

    if Value then
    begin
      // Réinitialise le compteur lorsqu'on l'active
      FRestant := FDuree;
      FPause := False;
    end;
  end;
end;

procedure TTimerPlus.Start;
begin
  FRestant := FDuree;
  FPause := False;
  FTimer.Enabled := True;
end;

procedure TTimerPlus.Stop;
begin
  FTimer.Enabled := False;
  FPause := False;
  FRestant := FDuree;
end;

procedure TTimerPlus.TimerTick(Sender: TObject);
begin
  if FRestant > 0 then
  begin
    Dec(FRestant);

    // Déclencher l'événement de décompte
    if Assigned(FOnCountDown) then
      FOnCountDown(Self, FRestant);

    // Si terminé
    if FRestant = 0 then
    begin
      FTimer.Enabled := False;

      if Assigned(FOnComplete) then
        FOnComplete(Self);
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TTimerPlus]);
end;

end.
```

### Étape 3 : Compiler et installer le paquet

1. Assurez-vous que l'unité de votre composant est ajoutée au paquet
2. Compilez le paquet (clic droit sur le nom du paquet > **Compiler**)
3. Installez le paquet (clic droit sur le nom du paquet > **Installer**)

Après l'installation, votre composant apparaîtra dans la palette sous l'onglet "Mes Composants".

### Étape 4 : Utiliser votre composant

Vous pouvez maintenant utiliser votre composant comme n'importe quel autre :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du TimerPlus
  TimerPlus1.Duree := 30;  // 30 secondes
  TimerPlus1.OnCountDown := AfficherDecompte;
  TimerPlus1.OnComplete := FinDecompte;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  TimerPlus1.Start;
  ButtonStart.Enabled := False;
  ButtonPause.Enabled := True;
  ButtonStop.Enabled := True;
end;

procedure TForm1.AfficherDecompte(Sender: TObject; SecondesRestantes: Integer);
begin
  LabelCompte.Caption := 'Temps restant : ' + IntToStr(SecondesRestantes) + ' secondes';
end;

procedure TForm1.FinDecompte(Sender: TObject);
begin
  ShowMessage('Temps écoulé !');
  ButtonStart.Enabled := True;
  ButtonPause.Enabled := False;
  ButtonStop.Enabled := False;
end;
```

## Créer un contrôle visuel simple

Passons maintenant à un contrôle visuel simple. Nous allons créer un bouton amélioré qui change de couleur lorsqu'on le survole.

### Étape 1 : Ajouter une nouvelle unité au paquet

Créez une nouvelle unité nommée "UBoutonCouleur.pas" et ajoutez-la à votre paquet.

### Étape 2 : Coder le composant

```pascal
unit UBoutonCouleur;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

type
  TBoutonCouleur = class(TButton)
  private
    FCouleurSurvol: TColor;
    FCouleurNormale: TColor;
    FSurvole: Boolean;
    procedure SetCouleurSurvol(const Value: TColor);
    procedure SetCouleurNormale(const Value: TColor);
  protected
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CouleurSurvol: TColor read FCouleurSurvol write SetCouleurSurvol default clHighlight;
    property CouleurNormale: TColor read FCouleurNormale write SetCouleurNormale default clBtnFace;
  end;

procedure Register;

implementation

{ TBoutonCouleur }

constructor TBoutonCouleur.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCouleurNormale := clBtnFace;
  FCouleurSurvol := clHighlight;
  FSurvole := False;
  Font.Color := clWindowText;
end;

procedure TBoutonCouleur.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  // La souris entre dans la zone du bouton
  FSurvole := True;
  Color := FCouleurSurvol;

  // Si la couleur de survol est foncée, on adapte la couleur du texte
  if GetRValue(FCouleurSurvol) + GetGValue(FCouleurSurvol) + GetBValue(FCouleurSurvol) < 384 then
    Font.Color := clWhite
  else
    Font.Color := clBlack;

  Invalidate;  // Forcer le redessin
end;

procedure TBoutonCouleur.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  // La souris quitte la zone du bouton
  FSurvole := False;
  Color := FCouleurNormale;

  // Restaurer la couleur du texte par défaut
  Font.Color := clWindowText;

  Invalidate;  // Forcer le redessin
end;

procedure TBoutonCouleur.SetCouleurNormale(const Value: TColor);
begin
  if FCouleurNormale <> Value then
  begin
    FCouleurNormale := Value;
    if not FSurvole then
    begin
      Color := FCouleurNormale;
      Invalidate;
    end;
  end;
end;

procedure TBoutonCouleur.SetCouleurSurvol(const Value: TColor);
begin
  if FCouleurSurvol <> Value then
  begin
    FCouleurSurvol := Value;
    if FSurvole then
    begin
      Color := FCouleurSurvol;
      Invalidate;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TBoutonCouleur]);
end;

end.
```

### Étape 3 : Recompiler et réinstaller le paquet

Après avoir ajouté le nouveau composant, vous devez recompiler et réinstaller le paquet pour qu'il apparaisse dans la palette.

## Créer un contrôle composé

Un contrôle composé regroupe plusieurs contrôles existants en un seul. Voici un exemple de champ de recherche qui combine un `TEdit` et un `TButton`.

### Étape 1 : Ajouter une nouvelle unité

Créez une nouvelle unité "UChampRecherche.pas" et ajoutez-la à votre paquet.

### Étape 2 : Coder le composant

```pascal
unit UChampRecherche;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics,
  Vcl.ExtCtrls;

type
  TSearchEvent = procedure(Sender: TObject; const SearchText: string) of object;

  TChampRecherche = class(TWinControl)
  private
    FEdit: TEdit;
    FButton: TButton;
    FPanel: TPanel;
    FPlaceholder: string;
    FOnSearch: TSearchEvent;
    procedure SetPlaceholder(const Value: string);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure ButtonClick(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property Visible;

    property Text: string read GetText write SetText;
    property Placeholder: string read FPlaceholder write SetPlaceholder;
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
  end;

procedure Register;

implementation

{ TChampRecherche }

constructor TChampRecherche.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 200;
  Height := 25;

  // Créer le panneau conteneur
  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alClient;
  FPanel.BevelOuter := bvNone;
  FPanel.ShowCaption := False;

  // Créer le champ de texte
  FEdit := TEdit.Create(Self);
  FEdit.Parent := FPanel;
  FEdit.Align := alClient;
  FEdit.TextHint := 'Rechercher...';
  FEdit.OnKeyPress := EditKeyPress;

  // Créer le bouton
  FButton := TButton.Create(Self);
  FButton.Parent := FPanel;
  FButton.Align := alRight;
  FButton.Width := 25;
  FButton.Caption := '🔍';
  FButton.OnClick := ButtonClick;

  // Définir les valeurs par défaut
  FPlaceholder := 'Rechercher...';
end;

destructor TChampRecherche.Destroy;
begin
  // Les contrôles enfants seront automatiquement libérés
  inherited Destroy;
end;

procedure TChampRecherche.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnSearch) then
    FOnSearch(Self, FEdit.Text);
end;

procedure TChampRecherche.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then  // Touche Entrée
  begin
    Key := #0;  // Supprime le bip
    ButtonClick(Sender);  // Déclenche la recherche
  end;
end;

function TChampRecherche.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TChampRecherche.Resize;
begin
  inherited;
  FButton.Width := Height;  // Adapte la largeur du bouton à la hauteur du contrôle
end;

procedure TChampRecherche.SetPlaceholder(const Value: string);
begin
  if FPlaceholder <> Value then
  begin
    FPlaceholder := Value;
    FEdit.TextHint := FPlaceholder;
  end;
end;

procedure TChampRecherche.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TChampRecherche]);
end;

end.
```

## Créer un contrôle entièrement personnalisé

Pour finir, créons un contrôle plus avancé : une jauge de progression circulaire qui dessine son propre affichage.

### Étape 1 : Ajouter une nouvelle unité

Créez une nouvelle unité "UJaugeCirculaire.pas" et ajoutez-la à votre paquet.

### Étape 2 : Coder le composant

```pascal
unit UJaugeCirculaire;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  System.Math;

type
  TJaugeCirculaire = class(TGraphicControl)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FCouleurFond: TColor;
    FCouleurJauge: TColor;
    FCouleurTexte: TColor;
    FEpaisseur: Integer;
    FAfficherPourcentage: Boolean;
    procedure SetMin(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetCouleurFond(const Value: TColor);
    procedure SetCouleurJauge(const Value: TColor);
    procedure SetCouleurTexte(const Value: TColor);
    procedure SetEpaisseur(const Value: Integer);
    procedure SetAfficherPourcentage(const Value: Boolean);
    function GetPourcentage: Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PositionValide(APosition: Integer): Boolean;
    property Pourcentage: Integer read GetPourcentage;
  published
    property Align;
    property Anchors;
    property Cursor;
    property Visible;
    property Enabled;
    property ShowHint;
    property Hint;
    property ParentShowHint;

    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property CouleurFond: TColor read FCouleurFond write SetCouleurFond default clBtnFace;
    property CouleurJauge: TColor read FCouleurJauge write SetCouleurJauge default clHighlight;
    property CouleurTexte: TColor read FCouleurTexte write SetCouleurTexte default clWindowText;
    property Epaisseur: Integer read FEpaisseur write SetEpaisseur default 10;
    property AfficherPourcentage: Boolean read FAfficherPourcentage write SetAfficherPourcentage default True;
  end;

procedure Register;

implementation

{ TJaugeCirculaire }

constructor TJaugeCirculaire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FCouleurFond := clBtnFace;
  FCouleurJauge := clHighlight;
  FCouleurTexte := clWindowText;
  FEpaisseur := 10;
  FAfficherPourcentage := True;
end;

function TJaugeCirculaire.GetPourcentage: Integer;
begin
  if FMax <> FMin then
    Result := Round((FPosition - FMin) / (FMax - FMin) * 100)
  else
    Result := 0;
end;

procedure TJaugeCirculaire.Paint;
var
  R, RInner: TRect;
  CenterX, CenterY, Radius: Integer;
  StartAngle, SweepAngle: Integer;
  S: string;
  TextSize: TSize;
  ArcColor: TColor;
begin
  // Calcul des coordonnées
  CenterX := Width div 2;
  CenterY := Height div 2;
  Radius := Min(CenterX, CenterY) - 2;

  // Dessiner le fond
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Dessiner le cercle extérieur
  Canvas.Brush.Color := FCouleurFond;
  Canvas.Pen.Color := FCouleurFond;
  Canvas.Pen.Width := 1;
  R := Rect(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius);
  Canvas.Ellipse(R);

  // Dessiner la jauge
  if FPosition > FMin then
  begin
    StartAngle := 270;  // Commencer en haut
    SweepAngle := Round(360 * GetPourcentage / 100);

    // Convertir les angles en coordonnées logiques pour Windows
    StartAngle := StartAngle * 16;
    SweepAngle := SweepAngle * 16;

    Canvas.Brush.Color := FCouleurJauge;
    Canvas.Pen.Color := FCouleurJauge;
    Canvas.Pen.Width := FEpaisseur;

    // Dessiner l'arc
    Winapi.Windows.Arc(Canvas.Handle,
                        R.Left, R.Top, R.Right, R.Bottom,
                        CenterX + Round(Radius * Cos(DegToRad(StartAngle / 16))),
                        CenterY + Round(Radius * Sin(DegToRad(StartAngle / 16))),
                        CenterX + Round(Radius * Cos(DegToRad((StartAngle + SweepAngle) / 16))),
                        CenterY + Round(Radius * Sin(DegToRad((StartAngle + SweepAngle) / 16))));
  end;

  // Dessiner le cercle intérieur (trou)
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := 1;
  RInner := Rect(CenterX - Radius + FEpaisseur + 2,
                 CenterY - Radius + FEpaisseur + 2,
                 CenterX + Radius - FEpaisseur - 2,
                 CenterY + Radius - FEpaisseur - 2);
  Canvas.Ellipse(RInner);

  // Afficher le pourcentage
  if FAfficherPourcentage then
  begin
    S := IntToStr(GetPourcentage) + '%';
    Canvas.Font := Font;
    Canvas.Font.Color := FCouleurTexte;
    TextSize := Canvas.TextExtent(S);
    Canvas.TextOut(CenterX - TextSize.cx div 2, CenterY - TextSize.cy div 2, S);
  end;
end;

function TJaugeCirculaire.PositionValide(APosition: Integer): Boolean;
begin
  Result := (APosition >= FMin) and (APosition <= FMax);
end;

procedure TJaugeCirculaire.SetAfficherPourcentage(const Value: Boolean);
begin
  if FAfficherPourcentage <> Value then
  begin
    FAfficherPourcentage := Value;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetCouleurFond(const Value: TColor);
begin
  if FCouleurFond <> Value then
  begin
    FCouleurFond := Value;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetCouleurJauge(const Value: TColor);
begin
  if FCouleurJauge <> Value then
  begin
    FCouleurJauge := Value;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetCouleurTexte(const Value: TColor);
begin
  if FCouleurTexte <> Value then
  begin
    FCouleurTexte := Value;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetEpaisseur(const Value: Integer);
begin
  if FEpaisseur <> Value then
  begin
    FEpaisseur := Value;
    if FEpaisseur < 1 then FEpaisseur := 1;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMax <= FMin then FMax := FMin + 1;
    if not PositionValide(FPosition) then FPosition := FMin;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMin >= FMax then FMin := FMax - 1;
    if not PositionValide(FPosition) then FPosition := FMin;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    if PositionValide(Value) then
      FPosition := Value
    else if Value < FMin then
      FPosition := FMin
    else
      FPosition := FMax;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TJaugeCirculaire]);
end;

end.
```

## Éditeurs de propriétés personnalisés

Pour fournir une meilleure expérience utilisateur lors de la conception, vous pouvez créer des éditeurs de propriétés personnalisés. Cela dépasse le cadre d'une introduction, mais voici un aperçu simplifié :

```pascal
unit UEditeurCouleur;

interface

uses
  DesignIntf, DesignEditors, System.Classes, Vcl.Dialogs, Vcl.Graphics;

type
  TEditeurCouleur = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

procedure TEditeurCouleur.Edit;
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := TColor(GetOrdValue);
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
  finally
    ColorDialog.Free;
  end;
end;

function TEditeurCouleur.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paDialog];
end;

function TEditeurCouleur.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TEditeurCouleur.SetValue(const Value: string);
begin
  SetOrdValue(StringToColor(Value));
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TEditeurCouleur);
end;

end.
```

## Bonnes pratiques de développement de composants

### 1. Organisation du code

- Séparez clairement les sections private, protected, public et published
- Utilisez des noms explicites pour les propriétés et méthodes
- Commentez votre code, surtout pour les fonctionnalités complexes
- Regroupez les propriétés liées dans la section published

### 2. Gestion de la mémoire

- Libérez toujours tous les objets que vous créez dans le destructeur
- Utilisez le owner (propriétaire) pour la gestion automatique des composants enfants
- Évitez les fuites de mémoire en vérifiant avec des outils comme FastMM

```pascal
destructor TMonComposant.Destroy;
begin
  // Libérer les objets créés manuellement
  if Assigned(FListe) then
    FListe.Free;

  // Appeler le destructeur parent en dernier
  inherited Destroy;
end;
```

### 3. Propriétés et notifications

- Utilisez des méthodes Set... pour les propriétés qui nécessitent des actions lors du changement
- Appelez Invalidate pour les propriétés qui affectent l'apparence visuelle
- Utilisez la notification pour réagir aux changements des composants liés

```pascal
procedure TMonComposant.SetCouleur(const Value: TColor);
begin
  if FCouleur <> Value then
  begin
    FCouleur := Value;
    // Redessiner si la couleur change
    Invalidate;
  end;
end;

procedure TMonComposant.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // Réagir si un composant lié est supprimé
  if (Operation = opRemove) and (AComponent = FComposantLie) then
    FComposantLie := nil;
end;
```

### 4. Messages Windows

Pour les contrôles visuels, gérez correctement les messages Windows :

```pascal
procedure TMonBouton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  // Code pour gérer l'entrée de la souris
  FSurvole := True;
  Invalidate;
end;

procedure TMonBouton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  // Code pour gérer la sortie de la souris
  FSurvole := False;
  Invalidate;
end;
```

### 5. Personnalisation de l'aspect

- Utilisez des propriétés comme BorderStyle, Color, Font
- Pensez à la compatibilité avec les thèmes Windows
- Pour les contrôles entièrement personnalisés, surchargez la méthode Paint

### 6. Tests et débogage

- Testez votre composant dans différents contextes
- Vérifiez son comportement lors du redimensionnement et du changement de thème
- Utilisez des assertions pour vérifier les conditions importantes

## Organisation des paquets de composants

Pour les projets plus importants, il est recommandé d'organiser vos composants en plusieurs paquets :

### 1. Paquet d'exécution (Runtime)

Contient uniquement le code nécessaire à l'exécution. Ce paquet sera inclus dans votre application.

```delphi
package MesComposantsRuntime;

{$R *.res}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
// ... autres directives ...

requires
  rtl,
  vcl;

contains
  UTimerPlus in 'UTimerPlus.pas',
  UBoutonCouleur in 'UBoutonCouleur.pas',
  UChampRecherche in 'UChampRecherche.pas',
  UJaugeCirculaire in 'UJaugeCirculaire.pas';

end.
```

### 2. Paquet de conception (Design)

Contient le code nécessaire uniquement à l'IDE (éditeurs de propriétés, procédures d'enregistrement).

```delphi
package MesComposantsDesign;

{$R *.res}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
// ... autres directives ...

requires
  rtl,
  vcl,
  designide,
  MesComposantsRuntime;

contains
  UEditeurCouleur in 'UEditeurCouleur.pas',
  UEnregistrement in 'UEnregistrement.pas';

end.
```

L'avantage de cette séparation est que vos applications finales n'incluront que le paquet d'exécution, sans le code spécifique à la conception.

## Distribution de vos composants

Pour partager vos composants avec d'autres développeurs, suivez ces étapes :

### 1. Créer un package d'installation

Incluez tous les fichiers nécessaires :
- Fichiers source (.pas)
- Fichiers de projet de paquet (.dpk)
- Fichiers de ressources (.res, .dcr)
- Documentation

### 2. Fournir des exemples

Créez un projet de démonstration qui montre comment utiliser vos composants.

### 3. Créer une documentation

Documentez vos composants :
- Description générale
- Liste des propriétés, méthodes et événements
- Exemples d'utilisation

### 4. Gestion des versions

Utilisez un système de contrôle de version comme Git pour gérer l'évolution de vos composants.

## Exemple concret : Un composant de saisie monétaire

Voici un exemple plus complet d'un composant de saisie monétaire qui hérite de `TEdit` et qui formate automatiquement la valeur entrée comme un montant monétaire.

```pascal
unit UEditMonetaire;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls,
  System.UITypes;

type
  TDevise = (devEuro, devDollar, devLivre, devYen);

  TFormatOptions = class(TPersistent)
  private
    FDevise: TDevise;
    FDecimales: Integer;
    FSeparateurMilliers: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetDevise(const Value: TDevise);
    procedure SetDecimales(const Value: Integer);
    procedure SetSeparateurMilliers(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Devise: TDevise read FDevise write SetDevise default devEuro;
    property Decimales: Integer read FDecimales write SetDecimales default 2;
    property SeparateurMilliers: Boolean read FSeparateurMilliers
                                write SetSeparateurMilliers default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TEditMonetaire = class(TEdit)
  private
    FValeur: Currency;
    FOptions: TFormatOptions;
    FModifie: Boolean;
    procedure SetValeur(const Value: Currency);
    procedure OptionsChangeHandler(Sender: TObject);
    procedure UpdateDisplay;
    function GetSymboleDevise: string;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FormatText;
    property Valeur: Currency read FValeur write SetValeur;
  published
    property Options: TFormatOptions read FOptions write FOptions;
  end;

procedure Register;

implementation

const
  SYMBOLES_DEVISE: array[TDevise] of string = ('€', '$', '£', '¥');

{ TFormatOptions }

constructor TFormatOptions.Create;
begin
  inherited Create;
  FDevise := devEuro;
  FDecimales := 2;
  FSeparateurMilliers := True;
end;

procedure TFormatOptions.Assign(Source: TPersistent);
begin
  if Source is TFormatOptions then
  begin
    FDevise := TFormatOptions(Source).Devise;
    FDecimales := TFormatOptions(Source).Decimales;
    FSeparateurMilliers := TFormatOptions(Source).SeparateurMilliers;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TFormatOptions.SetDecimales(const Value: Integer);
begin
  if (Value >= 0) and (Value <= 4) and (FDecimales <> Value) then
  begin
    FDecimales := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFormatOptions.SetDevise(const Value: TDevise);
begin
  if FDevise <> Value then
  begin
    FDevise := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFormatOptions.SetSeparateurMilliers(const Value: Boolean);
begin
  if FSeparateurMilliers <> Value then
  begin
    FSeparateurMilliers := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TEditMonetaire }

constructor TEditMonetaire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValeur := 0;
  FModifie := False;

  // Créer et configurer les options
  FOptions := TFormatOptions.Create;
  FOptions.OnChange := OptionsChangeHandler;

  // Configuration par défaut
  TextHint := 'Entrez un montant';
  Alignment := taRightJustify;

  // Initialiser l'affichage
  UpdateDisplay;
end;

destructor TEditMonetaire.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

function TEditMonetaire.GetSymboleDevise: string;
begin
  Result := SYMBOLES_DEVISE[FOptions.Devise];
end;

procedure TEditMonetaire.Change;
begin
  inherited Change;

  // Marquer comme modifié
  FModifie := True;
end;

procedure TEditMonetaire.KeyPress(var Key: Char);
var
  DecimalSeparator: Char;
begin
  DecimalSeparator := FormatSettings.DecimalSeparator;

  // Accepter uniquement les chiffres, le séparateur décimal et le Backspace
  if not (CharInSet(Key, ['0'..'9', DecimalSeparator, #8])) then
    Key := #0;

  // Accepter le séparateur décimal seulement s'il n'est pas déjà présent
  // et si le composant est configuré pour afficher des décimales
  if (Key = DecimalSeparator) and
     ((Pos(DecimalSeparator, Text) > 0) or (FOptions.Decimales = 0)) then
    Key := #0;

  inherited KeyPress(Key);
end;

procedure TEditMonetaire.OptionsChangeHandler(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TEditMonetaire.SetValeur(const Value: Currency);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    UpdateDisplay;
  end;
end;

procedure TEditMonetaire.UpdateDisplay;
var
  FormatStr: string;
  TempText: string;
begin
  // Construire le format selon les options
  if FOptions.SeparateurMilliers then
    FormatStr := '#,##0'
  else
    FormatStr := '0';

  // Ajouter les décimales si nécessaire
  if FOptions.Decimales > 0 then
    FormatStr := FormatStr + '.' + StringOfChar('0', FOptions.Decimales);

  // Formater le montant
  TempText := FormatFloat(FormatStr, FValeur);

  // Ajouter le symbole de devise
  TempText := TempText + ' ' + GetSymboleDevise;

  // Mettre à jour le texte sans déclencher l'événement Change
  FModifie := False;
  Text := TempText;
end;

procedure TEditMonetaire.FormatText;
var
  Value: Currency;
begin
  if FModifie and (Text <> '') then
  begin
    // Enlever tout sauf les chiffres et le séparateur décimal
    try
      Value := StrToCurr(Text);
      FValeur := Value;
    except
      // En cas d'erreur, ne pas modifier la valeur
    end;
  end;

  // Mettre à jour l'affichage
  UpdateDisplay;
  FModifie := False;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TEditMonetaire]);
end;

end.
```

## Débogage des composants

Le débogage des composants peut être plus complexe que celui d'une application normale. Voici quelques techniques utiles :

### 1. Console de débogage

Utilisez `OutputDebugString` pour envoyer des messages à la console de débogage :

```pascal
OutputDebugString(PChar('Valeur : ' + IntToStr(MaValeur)));
```

### 2. Exceptions personnalisées

Lancez des exceptions avec des messages clairs pour identifier les problèmes :

```pascal
if not ConditionValide then
  raise Exception.Create('TMonComposant: Paramètre invalide');
```

### 3. Journal d'événements

Créez un journal pour suivre les événements importants :

```pascal
procedure TMonComposant.LogEvent(const EventName, Details: string);
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, 'C:\Temp\ComposantLog.txt');

  if FileExists('C:\Temp\ComposantLog.txt') then
    Append(LogFile)
  else
    Rewrite(LogFile);

  try
    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) +
                     ' - ' + EventName + ': ' + Details);
  finally
    CloseFile(LogFile);
  end;
end;
```

## Ressources pour aller plus loin

Le développement de composants est un vaste sujet. Voici quelques ressources pour approfondir vos connaissances :

1. **Documentation officielle Delphi** - Consultez la section sur le développement de composants
2. **Livres spécialisés** - "Delphi Component Design" de Danny Thorpe
3. **Forums et groupes de discussion** - DelphiPraxis, Stack Overflow, etc.
4. **Étudier le code source** - Examinez le code de composants open source pour apprendre les bonnes pratiques

## Conclusion

La création de composants personnalisés est l'une des compétences les plus puissantes que vous puissiez développer en tant que programmeur Delphi. En encapsulant des fonctionnalités réutilisables dans des composants, vous pouvez :

- Améliorer votre productivité
- Standardiser votre code
- Créer des interfaces utilisateur plus riches
- Partager votre travail avec d'autres développeurs

Commencez par des composants simples, puis progressez vers des composants plus complexes à mesure que vous gagnez en expérience.

---

*Exercice pratique : Créez un paquet de composants contenant un composant TLabeledPanel qui combine un TPanel avec un TLabel situé en haut qui sert de titre. Le composant doit avoir des propriétés pour contrôler la couleur, la police et la position du titre. Testez votre composant dans une application simple.*

⏭️ [Migration depuis des versions précédentes de Delphi](/04-conception-dinterfaces-utilisateur-avec-la-vcl/09-migration-depuis-versions-precedentes.md)
