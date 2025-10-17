🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.8 Développement de composants personnalisés

## Introduction

Le développement de composants personnalisés est l'une des fonctionnalités les plus puissantes de Delphi. Cela vous permet de créer vos propres contrôles réutilisables, d'étendre les composants existants et de construire des bibliothèques de composants adaptées à vos besoins spécifiques.

## 4.8.1 Qu'est-ce qu'un composant personnalisé ?

### Définition

Un **composant personnalisé** est un élément réutilisable que vous créez vous-même, qui peut être :
- Ajouté à la palette d'outils de Delphi
- Glissé-déposé sur un formulaire comme n'importe quel autre composant
- Configuré via l'Inspecteur d'objets
- Réutilisé dans plusieurs projets

### Pourquoi créer des composants personnalisés ?

**Réutilisabilité :**
- Éviter de dupliquer le code dans plusieurs formulaires
- Créer une bibliothèque de composants pour votre entreprise
- Partager des fonctionnalités entre projets

**Encapsulation :**
- Cacher la complexité derrière une interface simple
- Regrouper des fonctionnalités liées
- Faciliter la maintenance

**Cohérence :**
- Garantir un comportement uniforme dans toute l'application
- Standardiser l'apparence et les fonctionnalités
- Simplifier les mises à jour

**Productivité :**
- Conception visuelle plutôt que code
- Propriétés configurables dans l'Inspecteur d'objets
- Gain de temps sur les projets futurs

### Types de composants

**Composants visuels :**
- S'affichent à l'écran (boutons, zones de texte, graphiques)
- Héritent généralement de `TControl` ou `TGraphicControl`
- Exemple : Un bouton avec dégradé personnalisé

**Composants non-visuels :**
- N'ont pas d'interface visible (timers, connexions)
- Héritent de `TComponent`
- Exemple : Un gestionnaire de logs

---

## 4.8.2 Les bases de l'héritage en Delphi

### Hiérarchie des classes de composants

```
TObject (classe de base)
  └─ TPersistent
      └─ TComponent (composants non-visuels)
          └─ TControl (composants visuels de base)
              ├─ TWinControl (composants Windows natifs)
              │   ├─ TButton
              │   ├─ TEdit
              │   └─ TCustomControl (pour dessins personnalisés)
              └─ TGraphicControl (composants graphiques légers)
                  ├─ TLabel
                  ├─ TShape
                  └─ TImage
```

### Choisir la classe parent

| Classe parent | Quand l'utiliser |
|--------------|------------------|
| `TComponent` | Composant non-visuel (timer, connexion) |
| `TGraphicControl` | Composant visuel simple sans focus ni enfants |
| `TCustomControl` | Composant visuel avec dessin personnalisé complet |
| `TWinControl` | Composant pouvant contenir d'autres composants |
| Composant existant | Pour étendre un composant standard |

---

## 4.8.3 Créer un premier composant simple

### Exemple 1 : Étendre un composant existant

Créons un `TEdit` qui n'accepte que des nombres.

#### Étape 1 : Créer l'unité

1. Menu **Fichier** → **Nouveau** → **Autre**
2. Catégorie **Delphi** → **Unité**
3. Nommer le fichier : `EditNumerique.pas`

#### Étape 2 : Déclarer la classe

```pascal
unit EditNumerique;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls;

type
  TEditNumerique = class(TEdit)
  private
    FAccepterDecimales: Boolean;
    FAccepterNegatifs: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AccepterDecimales: Boolean read FAccepterDecimales
      write FAccepterDecimales default True;
    property AccepterNegatifs: Boolean read FAccepterNegatifs
      write FAccepterNegatifs default True;
  end;

procedure Register;

implementation

constructor TEditNumerique.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAccepterDecimales := True;
  FAccepterNegatifs := True;
end;

procedure TEditNumerique.KeyPress(var Key: Char);
var
  SeparateurDecimal: Char;
begin
  SeparateurDecimal := FormatSettings.DecimalSeparator;

  // Autoriser les touches de contrôle (Backspace, etc.)
  if CharInSet(Key, [#8, #13]) then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  // Autoriser les chiffres
  if CharInSet(Key, ['0'..'9']) then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  // Autoriser le signe négatif
  if FAccepterNegatifs and (Key = '-') and (SelStart = 0) and
     (Pos('-', Text) = 0) then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  // Autoriser le séparateur décimal
  if FAccepterDecimales and (Key = SeparateurDecimal) and
     (Pos(SeparateurDecimal, Text) = 0) then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  // Bloquer toutes les autres touches
  Key := #0;
  Beep;
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TEditNumerique]);
end;

end.
```

#### Étape 3 : Comprendre le code

**Sections de la classe :**

```pascal
private
  // Variables internes, non accessibles de l'extérieur
  FAccepterDecimales: Boolean;

protected
  // Méthodes que les classes dérivées peuvent redéfinir
  procedure KeyPress(var Key: Char); override;

public
  // Méthodes et propriétés accessibles par le code
  constructor Create(AOwner: TComponent); override;

published
  // Propriétés visibles dans l'Inspecteur d'objets
  property AccepterDecimales: Boolean read FAccepterDecimales
    write FAccepterDecimales default True;
```

**Le mot-clé `override` :**
- Indique que vous redéfinissez une méthode de la classe parent
- Permet d'appeler la version parent avec `inherited`

**Le constructeur :**
```pascal
constructor TEditNumerique.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // Toujours appeler le constructeur parent
  FAccepterDecimales := True; // Initialiser les valeurs par défaut
  FAccepterNegatifs := True;
end;
```

**La procédure Register :**
```pascal
procedure Register;
begin
  // Enregistre le composant dans la palette d'outils
  RegisterComponents('Mes Composants', [TEditNumerique]);
end;
```

#### Étape 4 : Installer le composant

1. Menu **Composant** → **Installer des composants**
2. Cliquez sur **Ajouter une unité**
3. Sélectionnez votre fichier `EditNumerique.pas`
4. Cliquez sur **OK**
5. Delphi compile et installe le package
6. Le composant apparaît dans la palette sous "Mes Composants"

#### Étape 5 : Utiliser le composant

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Le composant est configuré visuellement dans l'Inspecteur d'objets
  EditNumerique1.AccepterDecimales := True;
  EditNumerique1.AccepterNegatifs := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Valeur: Double;
begin
  if TryStrToFloat(EditNumerique1.Text, Valeur) then
    ShowMessage('Valeur saisie : ' + FloatToStr(Valeur))
  else
    ShowMessage('Veuillez entrer un nombre valide');
end;
```

---

## 4.8.4 Propriétés personnalisées

### Types de propriétés

#### Propriétés simples

```pascal
type
  TMonComposant = class(TCustomControl)
  private
    FCouleurFond: TColor;
    FTexte: string;
    procedure SetCouleurFond(const Value: TColor);
    procedure SetTexte(const Value: string);
  published
    property CouleurFond: TColor read FCouleurFond write SetCouleurFond
      default clWhite;
    property Texte: string read FTexte write SetTexte;
  end;

implementation

procedure TMonComposant.SetCouleurFond(const Value: TColor);
begin
  if FCouleurFond <> Value then
  begin
    FCouleurFond := Value;
    Invalidate; // Redessiner le composant
  end;
end;

procedure TMonComposant.SetTexte(const Value: string);
begin
  if FTexte <> Value then
  begin
    FTexte := Value;
    Invalidate;
  end;
end;
```

**Pourquoi utiliser des méthodes Set ?**
- Valider les valeurs avant de les accepter
- Déclencher des actions lors du changement (redessiner, recalculer)
- Maintenir la cohérence de l'état interne

#### Propriétés calculées (lecture seule)

```pascal
type
  TCompteur = class(TComponent)
  private
    FValeur: Integer;
    function GetEstPair: Boolean;
  published
    property Valeur: Integer read FValeur write FValeur;
    property EstPair: Boolean read GetEstPair; // Lecture seule
  end;

function TCompteur.GetEstPair: Boolean;
begin
  Result := (FValeur mod 2) = 0;
end;
```

#### Propriétés avec valeurs par défaut

```pascal
type
  TMonBouton = class(TButton)
  private
    FRayon: Integer;
  published
    property Rayon: Integer read FRayon write FRayon default 10;
  end;

constructor TMonBouton.Create(AOwner: TComponent);
begin
  inherited;
  FRayon := 10; // Important : initialiser avec la valeur par défaut
end;
```

**La directive `default` :**
- Indique la valeur par défaut à l'Inspecteur d'objets
- Évite de sauvegarder la propriété dans le fichier .dfm si elle a la valeur par défaut
- ATTENTION : Ne définit PAS automatiquement la valeur, vous devez l'initialiser dans le constructeur

#### Propriétés énumérées

```pascal
type
  TAlignementTexte = (atGauche, atCentre, atDroite);

  TLabelPersonnalise = class(TGraphicControl)
  private
    FAlignement: TAlignementTexte;
    procedure SetAlignement(const Value: TAlignementTexte);
  published
    property Alignement: TAlignementTexte read FAlignement
      write SetAlignement default atGauche;
  end;

implementation

constructor TLabelPersonnalise.Create(AOwner: TComponent);
begin
  inherited;
  FAlignement := atGauche;
  Width := 100;
  Height := 20;
end;

procedure TLabelPersonnalise.SetAlignement(const Value: TAlignementTexte);
begin
  if FAlignement <> Value then
  begin
    FAlignement := Value;
    Invalidate;
  end;
end;

procedure TLabelPersonnalise.Paint;
var
  R: TRect;
  Flags: Integer;
begin
  inherited;

  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);

  // Définir l'alignement
  case FAlignement of
    atGauche:  Flags := DT_LEFT;
    atCentre:  Flags := DT_CENTER;
    atDroite:  Flags := DT_RIGHT;
  end;

  Flags := Flags or DT_VCENTER or DT_SINGLELINE;
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, Flags);
end;
```

---

## 4.8.5 Événements personnalisés

### Créer un événement

```pascal
type
  // Type de l'événement
  TValeurChangeEvent = procedure(Sender: TObject; NouvelleValeur: Integer) of object;

  TCompteurPersonnalise = class(TComponent)
  private
    FValeur: Integer;
    FOnValeurChange: TValeurChangeEvent;
    procedure SetValeur(const Value: Integer);
  protected
    procedure DoValeurChange(NouvelleValeur: Integer); virtual;
  published
    property Valeur: Integer read FValeur write SetValeur;
    property OnValeurChange: TValeurChangeEvent read FOnValeurChange
      write FOnValeurChange;
  end;

implementation

procedure TCompteurPersonnalise.SetValeur(const Value: Integer);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    DoValeurChange(FValeur); // Déclencher l'événement
  end;
end;

procedure TCompteurPersonnalise.DoValeurChange(NouvelleValeur: Integer);
begin
  // Déclencher l'événement s'il est assigné
  if Assigned(FOnValeurChange) then
    FOnValeurChange(Self, NouvelleValeur);
end;
```

### Utiliser l'événement

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  CompteurPersonnalise1.OnValeurChange := GererChangementValeur;
end;

procedure TForm1.GererChangementValeur(Sender: TObject; NouvelleValeur: Integer);
begin
  Label1.Caption := 'Nouvelle valeur : ' + IntToStr(NouvelleValeur);
end;
```

---

## 4.8.6 Créer un composant visuel personnalisé

### Exemple : Jauge de progression circulaire

```pascal
unit JaugeCirculaire;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, System.Math;

type
  TJaugeCirculaire = class(TGraphicControl)
  private
    FMinimum: Integer;
    FMaximum: Integer;
    FPosition: Integer;
    FCouleurFond: TColor;
    FCouleurJauge: TColor;
    FEpaisseur: Integer;
    procedure SetMinimum(const Value: Integer);
    procedure SetMaximum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetCouleurFond(const Value: TColor);
    procedure SetCouleurJauge(const Value: TColor);
    procedure SetEpaisseur(const Value: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property CouleurFond: TColor read FCouleurFond write SetCouleurFond
      default clSilver;
    property CouleurJauge: TColor read FCouleurJauge write SetCouleurJauge
      default clGreen;
    property Epaisseur: Integer read FEpaisseur write SetEpaisseur default 10;

    // Republier les propriétés héritées utiles
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

constructor TJaugeCirculaire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Valeurs par défaut
  FMinimum := 0;
  FMaximum := 100;
  FPosition := 0;
  FCouleurFond := clSilver;
  FCouleurJauge := clGreen;
  FEpaisseur := 10;

  // Taille par défaut
  Width := 100;
  Height := 100;
end;

procedure TJaugeCirculaire.SetMinimum(const Value: Integer);
begin
  if FMinimum <> Value then
  begin
    FMinimum := Value;
    if FPosition < FMinimum then
      FPosition := FMinimum;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetMaximum(const Value: Integer);
begin
  if FMaximum <> Value then
  begin
    FMaximum := Value;
    if FPosition > FMaximum then
      FPosition := FMaximum;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    // Limiter la valeur entre Min et Max
    if Value < FMinimum then
      FPosition := FMinimum
    else if Value > FMaximum then
      FPosition := FMaximum
    else
      FPosition := Value;

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

procedure TJaugeCirculaire.SetEpaisseur(const Value: Integer);
begin
  if FEpaisseur <> Value then
  begin
    FEpaisseur := Value;
    Invalidate;
  end;
end;

procedure TJaugeCirculaire.Paint;
var
  Rect: TRect;
  CentreX, CentreY, Rayon: Integer;
  Pourcentage: Double;
  AngleFin: Integer;
  Texte: string;
  TailleTexte: TSize;
begin
  inherited Paint;

  // Fond
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Calculer les dimensions
  Rect := ClientRect;
  CentreX := Rect.Width div 2;
  CentreY := Rect.Height div 2;
  Rayon := Min(Rect.Width, Rect.Height) div 2 - FEpaisseur;

  // Dessiner le cercle de fond
  Canvas.Pen.Color := FCouleurFond;
  Canvas.Pen.Width := FEpaisseur;
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(CentreX - Rayon, CentreY - Rayon,
                 CentreX + Rayon, CentreY + Rayon);

  // Calculer le pourcentage
  if FMaximum > FMinimum then
    Pourcentage := (FPosition - FMinimum) / (FMaximum - FMinimum)
  else
    Pourcentage := 0;

  // Dessiner l'arc de progression (de -90° à AngleFin)
  AngleFin := Round(Pourcentage * 360);
  if AngleFin > 0 then
  begin
    Canvas.Pen.Color := FCouleurJauge;
    Canvas.Arc(CentreX - Rayon, CentreY - Rayon,
               CentreX + Rayon, CentreY + Rayon,
               CentreX, CentreY - Rayon,  // Point de départ (haut)
               CentreX + Round(Rayon * Cos(DegToRad(AngleFin - 90))),
               CentreY + Round(Rayon * Sin(DegToRad(AngleFin - 90))));
  end;

  // Afficher le pourcentage au centre
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Font;
  Texte := Format('%d%%', [Round(Pourcentage * 100)]);
  TailleTexte := Canvas.TextExtent(Texte);
  Canvas.TextOut(CentreX - TailleTexte.cx div 2,
                 CentreY - TailleTexte.cy div 2,
                 Texte);
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TJaugeCirculaire]);
end;

end.
```

### Utilisation du composant

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  JaugeCirculaire1.Minimum := 0;
  JaugeCirculaire1.Maximum := 100;
  JaugeCirculaire1.Position := 0;
  JaugeCirculaire1.CouleurJauge := clBlue;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Incrémenter la jauge
  if JaugeCirculaire1.Position < JaugeCirculaire1.Maximum then
    JaugeCirculaire1.Position := JaugeCirculaire1.Position + 10;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Animation automatique
  JaugeCirculaire1.Position := (JaugeCirculaire1.Position + 1) mod 101;
end;
```

---

## 4.8.7 Composants conteneurs

### Créer un composant pouvant contenir d'autres composants

```pascal
type
  TPanneauPersonnalise = class(TCustomControl)
  private
    FTitre: string;
    FCouleurTitre: TColor;
    FHauteurTitre: Integer;
    procedure SetTitre(const Value: string);
    procedure SetCouleurTitre(const Value: TColor);
    procedure SetHauteurTitre(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Titre: string read FTitre write SetTitre;
    property CouleurTitre: TColor read FCouleurTitre write SetCouleurTitre
      default clNavy;
    property HauteurTitre: Integer read FHauteurTitre write SetHauteurTitre
      default 30;

    // Republier les propriétés de TWinControl
    property Align;
    property Anchors;
    property Color;
    property ParentColor;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
  end;

implementation

constructor TPanneauPersonnalise.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTitre := 'Panneau';
  FCouleurTitre := clNavy;
  FHauteurTitre := 30;

  Width := 200;
  Height := 150;
  Color := clWhite;

  // Important pour un conteneur
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TPanneauPersonnalise.SetTitre(const Value: string);
begin
  if FTitre <> Value then
  begin
    FTitre := Value;
    Invalidate;
  end;
end;

procedure TPanneauPersonnalise.SetCouleurTitre(const Value: TColor);
begin
  if FCouleurTitre <> Value then
  begin
    FCouleurTitre := Value;
    Invalidate;
  end;
end;

procedure TPanneauPersonnalise.SetHauteurTitre(const Value: Integer);
begin
  if FHauteurTitre <> Value then
  begin
    FHauteurTitre := Value;
    Realign; // Réaligner les contrôles enfants
    Invalidate;
  end;
end;

procedure TPanneauPersonnalise.Paint;
var
  R: TRect;
begin
  inherited Paint;

  // Dessiner la zone de titre
  R := Rect(0, 0, Width, FHauteurTitre);
  Canvas.Brush.Color := FCouleurTitre;
  Canvas.FillRect(R);

  // Dessiner le texte du titre
  Canvas.Font.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, PChar(FTitre), -1, R,
           DT_CENTER or DT_VCENTER or DT_SINGLELINE);

  // Dessiner la bordure
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(0, 0, Width, Height);
end;

procedure TPanneauPersonnalise.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TPanneauPersonnalise.AlignControls(AControl: TControl; var Rect: TRect);
begin
  // Ajuster la zone disponible pour les contrôles enfants
  // en tenant compte de la barre de titre
  Rect.Top := FHauteurTitre + 5;
  Rect.Left := 5;
  Rect.Right := Rect.Right - 5;
  Rect.Bottom := Rect.Bottom - 5;

  inherited AlignControls(AControl, Rect);
end;
```

---

## 4.8.8 Composants non-visuels

### Exemple : Gestionnaire de logs

```pascal
unit GestionnaireLog;

interface

uses
  System.SysUtils, System.Classes;

type
  TNiveauLog = (nlDebug, nlInfo, nlAvertissement, nlErreur, nlCritique);

  TLogEvent = procedure(Sender: TObject; Niveau: TNiveauLog;
    const Message: string) of object;

  TGestionnaireLog = class(TComponent)
  private
    FNomFichier: string;
    FActif: Boolean;
    FNiveauMinimum: TNiveauLog;
    FOnLog: TLogEvent;
    procedure SetNomFichier(const Value: string);
  protected
    procedure DoLog(Niveau: TNiveauLog; const Message: string); virtual;
    procedure EcrireDansFichier(const Ligne: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Avertissement(const Message: string);
    procedure Erreur(const Message: string);
    procedure Critique(const Message: string);
    procedure Log(Niveau: TNiveauLog; const Message: string);
  published
    property NomFichier: string read FNomFichier write SetNomFichier;
    property Actif: Boolean read FActif write FActif default True;
    property NiveauMinimum: TNiveauLog read FNiveauMinimum
      write FNiveauMinimum default nlDebug;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

procedure Register;

implementation

uses
  System.IOUtils;

constructor TGestionnaireLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActif := True;
  FNiveauMinimum := nlDebug;
  FNomFichier := 'application.log';
end;

destructor TGestionnaireLog.Destroy;
begin
  // Nettoyage si nécessaire
  inherited Destroy;
end;

procedure TGestionnaireLog.SetNomFichier(const Value: string);
begin
  if FNomFichier <> Value then
    FNomFichier := Value;
end;

procedure TGestionnaireLog.DoLog(Niveau: TNiveauLog; const Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Niveau, Message);
end;

procedure TGestionnaireLog.EcrireDansFichier(const Ligne: string);
var
  Fichier: TextFile;
begin
  try
    AssignFile(Fichier, FNomFichier);
    if FileExists(FNomFichier) then
      Append(Fichier)
    else
      Rewrite(Fichier);
    try
      WriteLn(Fichier, Ligne);
    finally
      CloseFile(Fichier);
    end;
  except
    // Gérer silencieusement les erreurs d'écriture
  end;
end;

procedure TGestionnaireLog.Log(Niveau: TNiveauLog; const Message: string);
const
  NiveauTexte: array[TNiveauLog] of string =
    ('DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL');
var
  Ligne: string;
begin
  if not FActif then
    Exit;

  if Niveau < FNiveauMinimum then
    Exit;

  Ligne := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     NiveauTexte[Niveau],
     Message]);

  EcrireDansFichier(Ligne);
  DoLog(Niveau, Message);
end;

procedure TGestionnaireLog.Debug(const Message: string);
begin
  Log(nlDebug, Message);
end;

procedure TGestionnaireLog.Info(const Message: string);
begin
  Log(nlInfo, Message);
end;

procedure TGestionnaireLog.Avertissement(const Message: string);
begin
  Log(nlAvertissement, Message);
end;

procedure TGestionnaireLog.Erreur(const Message: string);
begin
  Log(nlErreur, Message);
end;

procedure TGestionnaireLog.Critique(const Message: string);
begin
  Log(nlCritique, Message);
end;

procedure Register;
begin
  RegisterComponents('Mes Composants', [TGestionnaireLog]);
end;

end.
```

### Utilisation

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  GestionnaireLog1.NomFichier := 'C:\Logs\monapp.log';
  GestionnaireLog1.Actif := True;
  GestionnaireLog1.NiveauMinimum := nlInfo;
  GestionnaireLog1.OnLog := AfficherLog;

  GestionnaireLog1.Info('Application démarrée');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    // Code qui peut générer une erreur
    GestionnaireLog1.Debug('Début du traitement');
    TraiterDonnees;
    GestionnaireLog1.Info('Traitement réussi');
  except
    on E: Exception do
    begin
      GestionnaireLog1.Erreur('Erreur : ' + E.Message);
      raise;
    end;
  end;
end;

procedure TForm1.AfficherLog(Sender: TObject; Niveau: TNiveauLog;
  const Message: string);
begin
  Memo1.Lines.Add(Message);
end;
```

---

## 4.8.9 Packages de composants

### Créer un package

Un **package** permet de regrouper plusieurs composants pour faciliter leur installation et distribution.

#### Étape 1 : Créer le package

1. Menu **Fichier** → **Nouveau** → **Package**
2. Sauvegarder le package : `MesComposants.dpk`

#### Étape 2 : Ajouter les unités

1. Clic droit sur le package → **Ajouter**
2. Sélectionner vos fichiers .pas
3. Compiler le package

#### Étape 3 : Installer le package

1. Clic droit sur le package → **Installer**
2. Les composants apparaissent dans la palette

### Structure d'un package

```pascal
package MesComposants;

{$R *.res}
{$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
// ... autres directives ...
{$ENDIF IMPLICITBUILDING}

requires
  rtl,
  vcl;

contains
  EditNumerique in 'EditNumerique.pas',
  JaugeCirculaire in 'JaugeCirculaire.pas',
  GestionnaireLog in 'GestionnaireLog.pas';

end.
```

---

## 4.8.10 Bonnes pratiques

### 1. Nommage des composants

```pascal
// Bon : Préfixe cohérent
type
  TMesComposantsEdit = class(TEdit)
  TMesComposantsButton = class(TButton)

// Ou utiliser un préfixe d'entreprise
type
  TACMEButton = class(TButton)
  TACMEEdit = class(TEdit)
```

### 2. Toujours appeler inherited

```pascal
constructor TMonComposant.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // TOUJOURS en premier
  // Votre code d'initialisation
end;

procedure TMonComposant.Paint;
begin
  inherited Paint; // Appeler la version parente
  // Votre code de dessin
end;
```

### 3. Utiliser Invalidate pour le redessin

```pascal
procedure TMonComposant.SetCouleur(const Value: TColor);
begin
  if FCouleur <> Value then
  begin
    FCouleur := Value;
    Invalidate; // Demander un redessin
  end;
end;
```

### 4. Valider les valeurs des propriétés

```pascal
procedure TJauge.SetMaximum(const Value: Integer);
begin
  if Value <= FMinimum then
    raise Exception.Create('Maximum doit être supérieur à Minimum');

  FMaximum := Value;

  // Ajuster Position si nécessaire
  if FPosition > FMaximum then
    SetPosition(FMaximum);

  Invalidate;
end;
```

### 5. Documenter les propriétés

```pascal
published
  /// <summary>
  /// Définit la valeur minimale de la jauge
  /// </summary>
  /// <remarks>
  /// Doit être inférieur à Maximum
  /// </remarks>
  property Minimum: Integer read FMinimum write SetMinimum default 0;
```

### 6. Gérer la persistance correctement

```pascal
type
  TMonComposant = class(TCustomControl)
  private
    FOptions: TStringList;
    function GetOptions: TStrings;
    procedure SetOptions(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Options: TStrings read GetOptions write SetOptions;
  end;

implementation

constructor TMonComposant.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TStringList.Create;
end;

destructor TMonComposant.Destroy;
begin
  FOptions.Free;
  inherited;
end;

function TMonComposant.GetOptions: TStrings;
begin
  Result := FOptions;
end;

procedure TMonComposant.SetOptions(const Value: TStrings);
begin
  FOptions.Assign(Value);
end;
```

### 7. Optimiser le redessin

```pascal
procedure TMonComposant.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMonComposant.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Invalidate;
end;

procedure TMonComposant.SetPropriete(const Value: Integer);
begin
  if FUpdateCount = 0 then
    Invalidate;
end;
```

### 8. Supporter le changement de taille

```pascal
procedure TMonComposant.Paint;
var
  R: TRect;
begin
  inherited;

  // Toujours utiliser ClientRect ou Width/Height
  R := ClientRect;

  // S'adapter à la taille actuelle
  Canvas.Rectangle(0, 0, Width, Height);
end;
```

### 9. Tests et débogage

```pascal
{$IFDEF DEBUG}
procedure TMonComposant.SetPosition(const Value: Integer);
begin
  // Vérifications supplémentaires en mode debug
  Assert(Value >= FMinimum, 'Position < Minimum');
  Assert(Value <= FMaximum, 'Position > Maximum');

  FPosition := Value;
  Invalidate;
end;
{$ENDIF}
```

### 10. Gestion des ressources

```pascal
destructor TMonComposant.Destroy;
begin
  // Libérer tous les objets créés
  FreeAndNil(FBitmap);
  FreeAndNil(FListe);

  inherited Destroy; // TOUJOURS en dernier
end;
```

---

## 4.8.11 Astuces avancées

### Éditeur de propriété personnalisé

Pour certaines propriétés complexes, vous pouvez créer un éditeur personnalisé dans l'Inspecteur d'objets :

```pascal
uses
  DesignIntf, DesignEditors;

type
  TCouleurGradientEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TCouleurGradientEditor.Edit;
begin
  // Afficher un dialogue personnalisé pour éditer la propriété
  with TFormEditeurCouleur.Create(nil) do
  try
    // Configurer et afficher
    if ShowModal = mrOk then
      SetOrdValue(CouleurSelectionnee);
  finally
    Free;
  end;
end;

function TCouleurGradientEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TCouleurGradientEditor.GetValue: string;
begin
  Result := ColorToString(GetOrdValue);
end;

// Enregistrer l'éditeur
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TColor), TMonComposant,
    'CouleurGradient', TCouleurGradientEditor);
end;
```

### Icône personnalisée dans la palette

```pascal
// Créer un fichier .dcr (ressource bitmap)
// Nommé : NomUnite.dcr
// Contenant des bitmaps 24x24 pour chaque composant

{$R EditNumerique.dcr}

procedure Register;
begin
  RegisterComponents('Mes Composants', [TEditNumerique]);
end;
```

### Composant avec streaming personnalisé

```pascal
type
  TMonComposant = class(TComponent)
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure LireData(Reader: TReader);
    procedure EcrireData(Writer: TWriter);
  end;

procedure TMonComposant.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DonneesPersonnalisees', LireData, EcrireData, True);
end;

procedure TMonComposant.LireData(Reader: TReader);
begin
  // Lire depuis le fichier .dfm
end;

procedure TMonComposant.EcrireData(Writer: TWriter);
begin
  // Écrire dans le fichier .dfm
end;
```

---

## Conclusion

Le développement de composants personnalisés est une compétence avancée qui ouvre de nombreuses possibilités dans Delphi. En créant vos propres composants, vous pouvez :

- Réutiliser efficacement votre code
- Créer des bibliothèques de composants métier
- Améliorer la productivité de votre équipe
- Construire des interfaces utilisateur uniques
- Encapsuler la complexité

### Points clés à retenir :

- **Héritage** : Choisissez la bonne classe parente (TComponent, TGraphicControl, TWinControl)
- **Propriétés** : Utilisez des méthodes Set pour la validation et le redessin
- **Événements** : Créez des événements personnalisés pour notifier les changements
- **Paint** : Redéfinissez la méthode Paint pour le dessin personnalisé
- **Invalidate** : Appelez Invalidate pour déclencher un redessin
- **inherited** : Appelez toujours les méthodes parentes
- **Packages** : Regroupez vos composants dans des packages pour faciliter la distribution
- **Documentation** : Documentez vos composants pour faciliter leur utilisation

Avec ces connaissances, vous êtes prêt à créer vos propres composants personnalisés et à étendre les capacités de Delphi selon vos besoins spécifiques !

⏭️ [Migration depuis des versions précédentes de Delphi](/04-conception-dinterfaces-utilisateur-avec-la-vcl/09-migration-depuis-versions-precedentes.md)
