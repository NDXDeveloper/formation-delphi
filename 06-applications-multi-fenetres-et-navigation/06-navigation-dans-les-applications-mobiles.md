🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.6 Navigation dans les applications mobiles

## Introduction

La navigation dans les applications mobiles est fondamentalement différente de celle des applications desktop. Les utilisateurs mobiles ont des attentes spécifiques en termes d'interface et de comportement, et Delphi avec FireMonkey (FMX) offre tous les outils nécessaires pour créer des expériences de navigation modernes et intuitives.

### Différences entre navigation desktop et mobile

**Desktop (VCL/FMX) :**
- Fenêtres multiples
- Menus traditionnels
- Souris et clavier
- Écran large

**Mobile (FMX) :**
- Application plein écran
- Navigation tactile
- Gestes (swipe, pinch, tap)
- Écran limité
- Bouton retour matériel (Android)
- Barre de navigation (iOS)

### Patterns de navigation mobile

Les applications mobiles utilisent principalement ces patterns :

1. **Navigation par onglets** : Barre d'onglets en bas (iOS) ou en haut (Android)
2. **Navigation hiérarchique** : Empilage d'écrans avec bouton retour
3. **Menu latéral (Drawer)** : Menu glissant depuis le côté
4. **Navigation modale** : Écrans temporaires par-dessus le contenu
5. **Navigation par cartes** : Cartes à défiler horizontalement

## TTabControl - Navigation par onglets

Le composant `TTabControl` est l'outil principal pour créer une navigation par onglets dans les applications mobiles FMX.

### Configuration de base

```pascal
unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.TabControl, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    TabItemAccueil: TTabItem;
    TabItemRecherche: TTabItem;
    TabItemProfil: TTabItem;
    ToolBar1: TToolBar;
    LabelTitre: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    procedure ConfigurerNavigation;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ConfigurerNavigation;
end;

procedure TFormMain.ConfigurerNavigation;
begin
  // Masquer les onglets visuels (on créera des boutons personnalisés)
  TabControl1.TabPosition := TTabPosition.None;

  // Ou afficher les onglets en bas (style iOS)
  // TabControl1.TabPosition := TTabPosition.Bottom;

  // Configuration des TabItems
  TabItemAccueil.Text := 'Accueil';
  TabItemRecherche.Text := 'Recherche';
  TabItemProfil.Text := 'Profil';

  // Page par défaut
  TabControl1.ActiveTab := TabItemAccueil;
end;

procedure TFormMain.TabControl1Change(Sender: TObject);
begin
  // Mettre à jour le titre selon l'onglet actif
  case TabControl1.TabIndex of
    0: LabelTitre.Text := 'Accueil';
    1: LabelTitre.Text := 'Recherche';
    2: LabelTitre.Text := 'Profil';
  end;
end;

end.
```

### Créer une barre de navigation personnalisée

```pascal
type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    LayoutNavigation: TLayout;
    ButtonAccueil: TSpeedButton;
    ButtonRecherche: TSpeedButton;
    ButtonProfil: TSpeedButton;
    procedure ButtonAccueilClick(Sender: TObject);
    procedure ButtonRechercheClick(Sender: TObject);
    procedure ButtonProfilClick(Sender: TObject);
  private
    procedure NaviguerVers(Index: Integer);
    procedure MettreAJourBoutons;
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Masquer les onglets par défaut
  TabControl1.TabPosition := TTabPosition.None;

  // Positionner la barre de navigation en bas
  LayoutNavigation.Align := TAlignLayout.Bottom;
  LayoutNavigation.Height := 60;

  // Configurer les boutons
  ButtonAccueil.StyleLookup := 'tabitemstyle';
  ButtonRecherche.StyleLookup := 'tabitemstyle';
  ButtonProfil.StyleLookup := 'tabitemstyle';

  MettreAJourBoutons;
end;

procedure TFormMain.NaviguerVers(Index: Integer);
begin
  TabControl1.TabIndex := Index;
  MettreAJourBoutons;
end;

procedure TFormMain.MettreAJourBoutons;
begin
  // Mettre en évidence le bouton actif
  ButtonAccueil.IsPressed := TabControl1.TabIndex = 0;
  ButtonRecherche.IsPressed := TabControl1.TabIndex = 1;
  ButtonProfil.IsPressed := TabControl1.TabIndex = 2;
end;

procedure TFormMain.ButtonAccueilClick(Sender: TObject);
begin
  NaviguerVers(0);
end;

procedure TFormMain.ButtonRechercheClick(Sender: TObject);
begin
  NaviguerVers(1);
end;

procedure TFormMain.ButtonProfilClick(Sender: TObject);
begin
  NaviguerVers(2);
end;
```

### Navigation avec icônes

```pascal
procedure TFormMain.ConfigurerIcones;
begin
  // Utiliser des caractères Unicode ou des images
  ButtonAccueil.Text := #$F015;  // Icône maison (Font Awesome)
  ButtonRecherche.Text := #$F002; // Icône recherche
  ButtonProfil.Text := #$F007;    // Icône utilisateur

  // Ou charger des images
  ButtonAccueil.StyleLookup := 'tabitemstyle';
  // Ajouter des TImage comme enfants des boutons
end;
```

## Navigation hiérarchique (Stack Navigation)

Pour une navigation en profondeur avec possibilité de revenir en arrière.

### Principe du stack de navigation

```pascal
type
  TNavigationManager = class
  private
    FTabControl: TTabControl;
    FStack: TList<TTabItem>;
  public
    constructor Create(ATabControl: TTabControl);
    destructor Destroy; override;
    procedure Push(TabItem: TTabItem);
    procedure Pop;
    function CanGoBack: Boolean;
  end;

implementation

constructor TNavigationManager.Create(ATabControl: TTabControl);
begin
  inherited Create;
  FTabControl := ATabControl;
  FStack := TList<TTabItem>.Create;
end;

destructor TNavigationManager.Destroy;
begin
  FStack.Free;
  inherited;
end;

procedure TNavigationManager.Push(TabItem: TTabItem);
begin
  // Sauvegarder l'onglet actuel dans la pile
  if Assigned(FTabControl.ActiveTab) then
    FStack.Add(FTabControl.ActiveTab);

  // Afficher le nouvel onglet
  FTabControl.ActiveTab := TabItem;
end;

procedure TNavigationManager.Pop;
var
  PreviousTab: TTabItem;
begin
  if FStack.Count > 0 then
  begin
    PreviousTab := FStack.Last;
    FStack.Delete(FStack.Count - 1);
    FTabControl.ActiveTab := PreviousTab;
  end;
end;

function TNavigationManager.CanGoBack: Boolean;
begin
  Result := FStack.Count > 0;
end;
```

### Utilisation du gestionnaire de navigation

```pascal
type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    TabItemListe: TTabItem;
    TabItemDetail: TTabItem;
    ButtonRetour: TButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(Sender: TObject; const Point: TPointF);
    procedure ButtonRetourClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FNavManager: TNavigationManager;
  public
    destructor Destroy; override;
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNavManager := TNavigationManager.Create(TabControl1);
  TabControl1.TabPosition := TTabPosition.None;
  ButtonRetour.Visible := False;
end;

destructor TFormMain.Destroy;
begin
  FNavManager.Free;
  inherited;
end;

procedure TFormMain.ListBox1ItemClick(Sender: TObject; const Point: TPointF);
begin
  // Naviguer vers le détail
  FNavManager.Push(TabItemDetail);
  ButtonRetour.Visible := True;

  // Charger les détails de l'élément sélectionné
  LabelDetail.Text := 'Détail de : ' + ListBox1.Selected.Text;
end;

procedure TFormMain.ButtonRetourClick(Sender: TObject);
begin
  FNavManager.Pop;
  ButtonRetour.Visible := FNavManager.CanGoBack;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // Gérer le bouton retour Android
  {$IFDEF ANDROID}
  if Key = vkHardwareBack then
  begin
    if FNavManager.CanGoBack then
    begin
      FNavManager.Pop;
      ButtonRetour.Visible := FNavManager.CanGoBack;
      Key := 0; // Empêcher la fermeture de l'app
    end;
  end;
  {$ENDIF}
end;
```

## Transitions et animations

FireMonkey permet d'ajouter des animations lors des changements de page.

### Transitions de base

```pascal
uses
  FMX.Ani;

procedure TFormMain.NaviguerAvecAnimation(VersDroite: Boolean);
var
  Animation: TFloatAnimation;
begin
  Animation := TFloatAnimation.Create(nil);
  try
    Animation.Parent := TabControl1;
    Animation.PropertyName := 'Position.X';
    Animation.Duration := 0.3;
    Animation.AnimationType := TAnimationType.InOut;
    Animation.Interpolation := TInterpolationType.Quadratic;

    if VersDroite then
    begin
      Animation.StartValue := -TabControl1.Width;
      Animation.StopValue := 0;
    end
    else
    begin
      Animation.StartValue := TabControl1.Width;
      Animation.StopValue := 0;
    end;

    Animation.Start;
  finally
    Animation.Free;
  end;
end;
```

### Transition slide (glissement)

```pascal
procedure TFormMain.ChangerTabAvecSlide(NouvelIndex: Integer);
var
  Direction: Integer;
begin
  Direction := NouvelIndex - TabControl1.TabIndex;

  // Configurer la transition
  TabControl1.Transition := TTabTransition.Slide;
  TabControl1.TransitionEffect := TTabTransitionEffect.Normal;

  // Changer d'onglet (l'animation se fait automatiquement)
  TabControl1.TabIndex := NouvelIndex;
end;
```

### Personnaliser les transitions

```pascal
type
  TTransitionType = (ttSlide, ttFade, ttNone);

procedure TFormMain.AppliquerTransition(TransType: TTransitionType);
begin
  case TransType of
    ttSlide:
    begin
      TabControl1.Transition := TTabTransition.Slide;
      TabControl1.TransitionDuration := 0.3;
    end;

    ttFade:
    begin
      TabControl1.Transition := TTabTransition.None;
      // Implémenter un fade manuel avec TFloatAnimation sur Opacity
    end;

    ttNone:
    begin
      TabControl1.Transition := TTabTransition.None;
    end;
  end;
end;
```

## Menu latéral avec TMultiView

`TMultiView` permet de créer un menu latéral (drawer) moderne.

### Configuration de base

```pascal
type
  TFormMain = class(TForm)
    MultiView1: TMultiView;
    ButtonMenu: TSpeedButton;
    LayoutMenu: TLayout;
    ButtonMenuItem1: TButton;
    ButtonMenuItem2: TButton;
    ButtonMenuItem3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonMenuItem1Click(Sender: TObject);
  private
    procedure ConfigurerMultiView;
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ConfigurerMultiView;
end;

procedure TFormMain.ConfigurerMultiView;
begin
  // Mode du MultiView
  MultiView1.Mode := TMultiViewMode.Drawer;  // Menu coulissant

  // Position du menu
  MultiView1.DrawerOptions.Placement := TDrawerPlacement.Left;

  // Largeur du menu
  MultiView1.Width := 250;

  // Options
  MultiView1.DrawerOptions.Mode := TDrawerMode.OverlapDetailView;
  MultiView1.DrawerOptions.DurationSliding := 0.3;

  // Bouton pour ouvrir/fermer
  ButtonMenu.Text := #$2630;  // Icône hamburger (≡)
end;

procedure TFormMain.ButtonMenuClick(Sender: TObject);
begin
  // Basculer l'état du menu
  if MultiView1.IsShowed then
    MultiView1.HideMaster
  else
    MultiView1.ShowMaster;
end;

procedure TFormMain.ButtonMenuItem1Click(Sender: TObject);
begin
  // Action du menu
  ShowMessage('Menu Item 1 cliqué');

  // Fermer le menu
  MultiView1.HideMaster;

  // Naviguer vers une page
  TabControl1.ActiveTab := TabItemPage1;
end;
```

### Modes de MultiView

```pascal
// Mode Drawer : Menu coulissant par-dessus le contenu
MultiView1.Mode := TMultiViewMode.Drawer;
MultiView1.DrawerOptions.Mode := TDrawerMode.OverlapDetailView;

// Mode Panel : Menu pousse le contenu
MultiView1.Mode := TMultiViewMode.Panel;

// Mode Popover : Menu en popup (iPad)
MultiView1.Mode := TMultiViewMode.Popover;

// Mode PlatformBehaviour : Comportement natif selon la plateforme
MultiView1.Mode := TMultiViewMode.PlatformBehaviour;
```

### Menu avec profil utilisateur

```pascal
procedure TFormMain.ConfigurerMenuAvecProfil;
var
  LayoutProfil: TLayout;
  CircleProfil: TCircle;
  LabelNom: TLabel;
begin
  // Créer une section profil en haut du menu
  LayoutProfil := TLayout.Create(MultiView1);
  LayoutProfil.Parent := MultiView1;
  LayoutProfil.Align := TAlignLayout.Top;
  LayoutProfil.Height := 150;

  // Photo de profil
  CircleProfil := TCircle.Create(LayoutProfil);
  CircleProfil.Parent := LayoutProfil;
  CircleProfil.Width := 80;
  CircleProfil.Height := 80;
  CircleProfil.Position.X := 20;
  CircleProfil.Position.Y := 20;
  CircleProfil.Fill.Color := TAlphaColors.Lightblue;

  // Nom de l'utilisateur
  LabelNom := TLabel.Create(LayoutProfil);
  LabelNom.Parent := LayoutProfil;
  LabelNom.Text := 'Jean Dupont';
  LabelNom.Position.X := 20;
  LabelNom.Position.Y := 110;
  LabelNom.Font.Size := 16;
end;
```

## Gestes de navigation

FireMonkey supporte nativement les gestes tactiles.

### Swipe pour naviguer

```pascal
type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    GestureManager1: TGestureManager;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Activer les gestes
  Touch.GestureManager := GestureManager1;
  Touch.InteractiveGestures := [TInteractiveGesture.Zoom,
                                  TInteractiveGesture.Pan];

  // Gestes standards
  Touch.StandardGestures := [TStandardGesture.Left, TStandardGesture.Right];
end;

procedure TFormMain.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiLeft then
  begin
    // Swipe vers la gauche : page suivante
    if TabControl1.TabIndex < TabControl1.TabCount - 1 then
    begin
      TabControl1.TabIndex := TabControl1.TabIndex + 1;
      Handled := True;
    end;
  end
  else if EventInfo.GestureID = igiRight then
  begin
    // Swipe vers la droite : page précédente
    if TabControl1.TabIndex > 0 then
    begin
      TabControl1.TabIndex := TabControl1.TabIndex - 1;
      Handled := True;
    end;
  end;
end;
```

### Pull to refresh (tirer pour rafraîchir)

```pascal
type
  TFormMain = class(TForm)
    VertScrollBox1: TVertScrollBox;
    ListBox1: TListBox;
    LayoutRefresh: TLayout;
    ArcRefresh: TArc;
    procedure VertScrollBox1CalcContentBounds(Sender: TObject;
      var ContentBounds: TRectF);
    procedure VertScrollBox1ViewportPositionChange(Sender: TObject;
      const OldViewportPosition, NewViewportPosition: TPointF);
  private
    FRefreshing: Boolean;
    procedure DemarrerRefresh;
    procedure TerminerRefresh;
  end;

implementation

procedure TFormMain.VertScrollBox1ViewportPositionChange(Sender: TObject;
  const OldViewportPosition, NewViewportPosition: TPointF);
begin
  // Si on tire vers le bas au-delà du haut de la liste
  if (NewViewportPosition.Y < -80) and not FRefreshing then
  begin
    DemarrerRefresh;
  end;
end;

procedure TFormMain.DemarrerRefresh;
begin
  FRefreshing := True;

  // Animation de l'indicateur
  TAnimator.AnimateFloat(ArcRefresh, 'EndAngle', 360, 1, TAnimationType.InOut);

  // Charger les nouvelles données
  TTask.Run(procedure
  begin
    Sleep(2000); // Simulation

    TThread.Synchronize(nil, procedure
    begin
      // Mettre à jour les données
      ListBox1.Items.Add('Nouvel élément');
      TerminerRefresh;
    end);
  end);
end;

procedure TFormMain.TerminerRefresh;
begin
  FRefreshing := False;
  VertScrollBox1.ViewportPosition := PointF(0, 0);
end;
```

## Bouton retour matériel (Android)

Gérer correctement le bouton retour physique d'Android.

### Implémentation de base

```pascal
uses
  FMX.Platform;

type
  TFormMain = class(TForm)
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    function GererBoutonRetour: Boolean;
  end;

implementation

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  if Key = vkHardwareBack then
  begin
    if GererBoutonRetour then
      Key := 0;  // Empêcher la fermeture de l'application
  end;
  {$ENDIF}
end;

function TFormMain.GererBoutonRetour: Boolean;
begin
  Result := False;

  // Si un menu est ouvert, le fermer
  if MultiView1.IsShowed then
  begin
    MultiView1.HideMaster;
    Result := True;
    Exit;
  end;

  // Si on n'est pas sur la page d'accueil, y retourner
  if TabControl1.TabIndex <> 0 then
  begin
    TabControl1.TabIndex := 0;
    Result := True;
    Exit;
  end;

  // Si on est sur l'accueil, demander confirmation pour quitter
  if MessageDlg('Voulez-vous quitter l''application ?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
  begin
    // Laisser l'application se fermer
    Result := False;
  end
  else
    Result := True;
end;
```

## Navigation avec paramètres

Passer des données entre les écrans.

### Méthode 1 : Propriétés publiques

```pascal
type
  TTabItemDetail = class(TTabItem)
  private
    FItemID: Integer;
    FItemNom: string;
  public
    property ItemID: Integer read FItemID write FItemID;
    property ItemNom: string read FItemNom write FItemNom;
    procedure ChargerDetails;
  end;

implementation

procedure TTabItemDetail.ChargerDetails;
begin
  LabelNom.Text := FItemNom;
  // Charger les détails depuis la base de données avec FItemID
end;

// Utilisation
procedure TFormMain.ListBox1ItemClick(Sender: TObject);
begin
  TabItemDetail.ItemID := ListBox1.ItemIndex;
  TabItemDetail.ItemNom := ListBox1.Selected.Text;
  TabItemDetail.ChargerDetails;
  TabControl1.ActiveTab := TabItemDetail;
end;
```

### Méthode 2 : Événement avec données

```pascal
type
  TNavigationData = record
    ID: Integer;
    Nom: string;
    Data: TObject;
  end;

  TNavigationEvent = procedure(Sender: TObject; const Data: TNavigationData) of object;

type
  TFormMain = class(TForm)
  private
    FOnNavigate: TNavigationEvent;
    procedure DoNavigation(const Data: TNavigationData);
  public
    property OnNavigate: TNavigationEvent read FOnNavigate write FOnNavigate;
  end;

procedure TFormMain.DoNavigation(const Data: TNavigationData);
begin
  if Assigned(FOnNavigate) then
    FOnNavigate(Self, Data);
end;

// Utilisation
procedure TFormMain.NaviguerVersDetail(ItemID: Integer);
var
  NavData: TNavigationData;
begin
  NavData.ID := ItemID;
  NavData.Nom := 'Article ' + IntToStr(ItemID);
  NavData.Data := nil;

  DoNavigation(NavData);
  TabControl1.ActiveTab := TabItemDetail;
end;
```

## Barre d'outils et actions

### Barre d'outils avec boutons

```pascal
type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    ButtonMenu: TSpeedButton;
    LabelTitre: TLabel;
    ButtonAction: TSpeedButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigurerToolBar;
  end;

procedure TFormMain.ConfigurerToolBar;
begin
  // Position de la toolbar
  ToolBar1.Align := TAlignLayout.Top;
  ToolBar1.Height := 56;

  // Bouton menu à gauche
  ButtonMenu.Align := TAlignLayout.Left;
  ButtonMenu.Width := 50;
  ButtonMenu.Text := #$2630;  // ≡

  // Titre au centre
  LabelTitre.Align := TAlignLayout.Client;
  LabelTitre.TextSettings.HorzAlign := TTextAlign.Center;
  LabelTitre.TextSettings.Font.Size := 18;

  // Bouton d'action à droite
  ButtonAction.Align := TAlignLayout.Right;
  ButtonAction.Width := 50;
  ButtonAction.Text := #$22EE;  // ⋮ (trois points verticaux)
end;
```

### Menu contextuel (ActionSheet iOS / BottomSheet Android)

```pascal
procedure TFormMain.AfficherMenuActions;
var
  ActionSheet: TCustomActionSheet;
begin
  ActionSheet := TCustomActionSheet.Create(Self);
  try
    ActionSheet.Title := 'Actions';

    ActionSheet.AddAction('Partager', procedure
    begin
      ShowMessage('Partager');
    end);

    ActionSheet.AddAction('Copier', procedure
    begin
      ShowMessage('Copier');
    end);

    ActionSheet.AddAction('Supprimer', procedure
    begin
      ShowMessage('Supprimer');
    end, TCustomActionSheetType.Destructive);

    ActionSheet.AddAction('Annuler', procedure
    begin
      // Ne rien faire
    end, TCustomActionSheetType.Cancel);

    ActionSheet.Execute;
  finally
    ActionSheet.Free;
  end;
end;
```

## Indicateurs de chargement

### ActivityIndicator

```pascal
procedure TFormMain.ChargerDonnees;
begin
  ActivityIndicator1.Visible := True;
  ActivityIndicator1.Enabled := True;

  TTask.Run(procedure
  begin
    // Charger les données
    Sleep(2000); // Simulation

    TThread.Synchronize(nil, procedure
    begin
      // Mettre à jour l'interface
      ListBox1.Items.Add('Données chargées');

      ActivityIndicator1.Enabled := False;
      ActivityIndicator1.Visible := False;
    end);
  end);
end;
```

### Skeleton screens (écrans squelettes)

```pascal
procedure TFormMain.AfficherSkeleton;
var
  i: Integer;
  Item: TListBoxItem;
  Rect: TRectangle;
begin
  ListBox1.Clear;

  for i := 0 to 5 do
  begin
    Item := TListBoxItem.Create(ListBox1);
    Item.Parent := ListBox1;
    Item.Height := 80;
    Item.Selectable := False;

    // Rectangle gris animé (placeholder)
    Rect := TRectangle.Create(Item);
    Rect.Parent := Item;
    Rect.Align := TAlignLayout.Client;
    Rect.Margins.Rect := RectF(10, 10, 10, 10);
    Rect.Fill.Color := TAlphaColorRec.Lightgray;
    Rect.Stroke.Kind := TBrushKind.None;
    Rect.XRadius := 5;
    Rect.YRadius := 5;

    // Animation de pulsation
    TAnimator.AnimateFloat(Rect, 'Opacity', 0.3, 1, TAnimationType.InOut,
      TInterpolationType.Linear);
  end;
end;
```

## Gestion de l'état de l'application

### Sauvegarder l'état de navigation

```pascal
uses
  System.IOUtils;

procedure TFormMain.SauvegarderEtat;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  {$IFDEF ANDROID}
  ConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'config.ini');
  {$ENDIF}
  {$IFDEF IOS}
  ConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'config.ini');
  {$ENDIF}

  IniFile := TIniFile.Create(ConfigPath);
  try
    IniFile.WriteInteger('Navigation', 'TabIndex', TabControl1.TabIndex);
    IniFile.WriteInteger('Navigation', 'ScrollPosition',
      Round(VertScrollBox1.ViewportPosition.Y));
  finally
    IniFile.Free;
  end;
end;

procedure TFormMain.RestaurerEtat;
var
  IniFile: TIniFile;
  ConfigPath: string;
  TabIndex: Integer;
begin
  ConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'config.ini');

  if TFile.Exists(ConfigPath) then
  begin
    IniFile := TIniFile.Create(ConfigPath);
    try
      TabIndex := IniFile.ReadInteger('Navigation', 'TabIndex', 0);
      TabControl1.TabIndex := TabIndex;

      VertScrollBox1.ViewportPosition := PointF(0,
        IniFile.ReadInteger('Navigation', 'ScrollPosition', 0));
    finally
      IniFile.Free;
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Navigation cohérente

```pascal
// Toujours utiliser le même pattern dans toute l'app
// Éviter de mélanger navigation par onglets et drawer sans raison
```

### 2. Feedback visuel

```pascal
procedure TFormMain.ButtonItemClick(Sender: TObject);
begin
  // Indiquer visuellement le clic
  TAnimator.AnimateFloat(Sender as TControl, 'Opacity', 0.5, 0.2);

  // Puis naviguer
  TTask.Run(procedure
  begin
    Sleep(200);
    TThread.Synchronize(nil, procedure
    begin
      NaviguerVersPage(1);
    end);
  end);
end;
```

### 3. Transitions rapides

```pascal
// Les animations doivent être courtes (200-300ms max)
TabControl1.TransitionDuration := 0.25;
```

### 4. Gérer les états

```pascal
type
  TPageState = (psLoading, psLoaded, psError, psEmpty);

procedure TFormMain.AfficherEtat(Etat: TPageState);
begin
  case Etat of
    psLoading: ActivityIndicator1.Visible := True;
    psLoaded:
    begin
      ActivityIndicator1.Visible := False;
      ListBox1.Visible := True;
    end;
    psError: AfficherMessageErreur;
    psEmpty: AfficherMessageVide;
  end;
end;
```

### 5. Navigation prévisible

```pascal
// Le bouton retour doit toujours faire ce que l'utilisateur attend
// Ordre de priorité :
// 1. Fermer un dialogue/popup
// 2. Fermer un menu
// 3. Revenir à la page précédente
// 4. Retour à l'accueil
// 5. Demander confirmation pour quitter
```

### 6. Tester sur vrais appareils

```pascal
// Tester particulièrement :
// - Différentes tailles d'écran
// - Rotation de l'écran
// - Bouton retour Android
// - Gestes iOS (swipe depuis le bord gauche)
// - Notch iPhone / barre de navigation Android
```

### 7. Respecter les guidelines

```pascal
// iOS : Navigation en bas, bouton retour en haut à gauche
// Android : Navigation en haut, bouton menu hamburger

{$IFDEF IOS}
TabControl1.TabPosition := TTabPosition.Bottom;
{$ENDIF}

{$IFDEF ANDROID}
TabControl1.TabPosition := TTabPosition.Top;
{$ENDIF}
```

## Patterns de navigation avancés

### Master-Detail

```pascal
type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    TabMaster: TTabItem;
    TabDetail: TTabItem;
    ListBox1: TListBox;
    procedure ListBox1ItemClick(Sender: TObject);
  private
    procedure AfficherDetail(ItemIndex: Integer);
  end;

procedure TFormMain.ListBox1ItemClick(Sender: TObject);
begin
  AfficherDetail(ListBox1.ItemIndex);
end;

procedure TFormMain.AfficherDetail(ItemIndex: Integer);
begin
  // Charger les détails
  LabelDetailTitre.Text := ListBox1.Items[ItemIndex];

  // Naviguer avec animation
  TabControl1.Transition := TTabTransition.Slide;
  TabControl1.ActiveTab := TabDetail;
end;
```

### Navigation conditionnelle

```pascal
procedure TFormMain.NaviguerSelon Authentification;
begin
  if UtilisateurConnecte then
    TabControl1.ActiveTab := TabAccueil
  else
    TabControl1.ActiveTab := TabConnexion;
end;
```

### Deep linking

```pascal
procedure TFormMain.GererLienProfond(const URL: string);
begin
  // Exemple : myapp://profile/123
  if URL.StartsWith('myapp://profile/') then
  begin
    var ID := URL.Replace('myapp://profile/', '');
    NaviguerVersProfil(StrToInt(ID));
  end
  else if URL.StartsWith('myapp://article/') then
  begin
    var ID := URL.Replace('myapp://article/', '');
    NaviguerVersArticle(StrToInt(ID));
  end;
end;
```

## Résumé

La navigation mobile nécessite une approche différente des applications desktop. Les points clés à retenir :

- **TTabControl** : Principal composant pour la navigation par onglets
- **TMultiView** : Pour les menus latéraux (drawer)
- **Stack de navigation** : Gérer l'historique et le bouton retour
- **Transitions** : Animations fluides entre les pages (200-300ms)
- **Gestes** : Swipe, pull-to-refresh, gestes natifs
- **Bouton retour Android** : Gérer correctement avec FormKeyUp
- **États** : Loading, loaded, error, empty
- **Patterns natifs** : Respecter les conventions iOS/Android
- **Performance** : Transitions rapides, chargement asynchrone
- **Tests** : Toujours tester sur vrais appareils

Une bonne navigation rend l'application intuitive et agréable à utiliser. Suivez les conventions de chaque plateforme pour une expérience native.

⏭️ [Gestion de l'état de l'application](/06-applications-multi-fenetres-et-navigation/07-gestion-de-letat-de-lapplication.md)
