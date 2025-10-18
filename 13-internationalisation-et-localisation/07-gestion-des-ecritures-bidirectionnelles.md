🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.7 Gestion des écritures bidirectionnelles (RTL)

## Introduction

Certaines langues s'écrivent de droite à gauche (Right-to-Left ou RTL) au lieu de gauche à droite (Left-to-Right ou LTR). Supporter ces langues nécessite d'adapter l'interface utilisateur pour créer une expérience naturelle et intuitive. Dans cette section, nous allons découvrir comment Delphi gère les écritures bidirectionnelles et comment adapter vos applications.

## Qu'est-ce que RTL/LTR ?

### Direction d'écriture

| Direction | Abréviation | Description | Langues |
|-----------|-------------|-------------|---------|
| **Left-to-Right** | LTR | Écriture de gauche à droite | Français, anglais, espagnol, allemand, etc. |
| **Right-to-Left** | RTL | Écriture de droite à gauche | Arabe, hébreu, persan, ourdou |

### Exemple visuel

**LTR (Français) :**
```
→ Bonjour le monde
  Le texte commence à gauche
```

**RTL (Arabe) :**
```
مرحبا بالعالم ←
         النص يبدأ من اليمين
```

### Langues RTL principales

| Langue | Nom natif | Nombre de locuteurs | Remarques |
|--------|-----------|---------------------|-----------|
| **Arabe** | العربية | ~300 millions | Très répandu |
| **Hébreu** | עברית | ~9 millions | Israël |
| **Persan** | فارسی | ~70 millions | Iran, Afghanistan |
| **Ourdou** | اردو | ~70 millions | Pakistan, Inde |
| **Yiddish** | ייִדיש | ~1 million | Communautés juives |

> 💡 **Important** : Ces langues représentent un marché important. Ne pas supporter RTL peut exclure des millions d'utilisateurs potentiels.

## Comprendre BiDiMode

### La propriété BiDiMode

Dans Delphi, la propriété `BiDiMode` contrôle la direction du texte et de la mise en page.

| Valeur | Description | Usage |
|--------|-------------|-------|
| `bdLeftToRight` | Gauche à droite (défaut) | Langues LTR (français, anglais, etc.) |
| `bdRightToLeft` | Droite à gauche | Langues RTL (arabe, hébreu) |
| `bdRightToLeftNoAlign` | RTL sans alignement automatique | RTL avec contrôle manuel |
| `bdRightToLeftReadingOnly` | RTL lecture seule | Affichage RTL, contrôles LTR |

### Effet de BiDiMode

Quand vous activez `BiDiMode = bdRightToLeft` sur un formulaire :

**Changements automatiques :**
- ✅ Le texte s'aligne à droite
- ✅ Les menus s'ouvrent de droite à gauche
- ✅ Les barres de défilement passent à gauche
- ✅ Les onglets s'inversent (dernier à gauche)
- ✅ Les boutons OK/Annuler s'inversent
- ✅ L'ordre de tabulation s'inverse

### Exemple simple

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer le mode RTL pour tout le formulaire
  Self.BiDiMode := bdRightToLeft;

  // Tous les composants enfants héritent de ce mode
end;
```

## Configuration d'une application RTL

### Détecter si RTL est nécessaire

```pascal
uses
  System.SysUtils;

function EstLangueRTL(const CodeLangue: string): Boolean;
begin
  Result := (CodeLangue = 'ar') or  // Arabe
            (CodeLangue = 'he') or  // Hébreu
            (CodeLangue = 'fa') or  // Persan
            (CodeLangue = 'ur');    // Ourdou
end;

procedure TFormPrincipal.DefinirDirectionTexte(const CodeLangue: string);
begin
  if EstLangueRTL(CodeLangue) then
    Self.BiDiMode := bdRightToLeft
  else
    Self.BiDiMode := bdLeftToRight;

  // Réappliquer la mise en page
  AppliquerMiseEnPageRTL;
end;
```

### Activer RTL globalement

Pour activer RTL sur toute l'application :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  System.SysUtils,
  FormPrincipal in 'FormPrincipal.pas' {FrmPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Activer BiDiMode globalement
  Application.BiDiMode := bdRightToLeft;

  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
```

## Adaptation des composants

### Labels et textes

```pascal
procedure ConfigurerLabelsRTL;
begin
  // En mode RTL, les labels s'alignent automatiquement à droite
  Label1.Caption := 'الاسم:'; // "Nom:" en arabe

  // L'alignement est inversé automatiquement
  // LTR : taLeftJustify  → RTL : taRightJustify
  // LTR : taRightJustify → RTL : taLeftJustify
end;
```

### Boutons et ordre

En mode RTL, l'ordre des boutons doit être inversé :

```pascal
procedure TFormDialogue.ConfigurerBoutonsRTL;
begin
  if Self.BiDiMode = bdRightToLeft then
  begin
    // RTL : Annuler à gauche, OK à droite
    BtnOK.Left := Self.ClientWidth - BtnOK.Width - 10;
    BtnAnnuler.Left := BtnOK.Left - BtnAnnuler.Width - 5;
  end
  else
  begin
    // LTR : OK à gauche, Annuler à droite
    BtnOK.Left := 10;
    BtnAnnuler.Left := BtnOK.Left + BtnOK.Width + 5;
  end;
end;
```

### Champs de saisie

```pascal
procedure ConfigurerEditsRTL;
begin
  // Les Edit héritent automatiquement de BiDiMode
  EditNom.BiDiMode := bdRightToLeft;

  // Le curseur commence à droite
  // Le texte s'écrit de droite à gauche
  EditNom.Text := 'محمد'; // "Mohamed" en arabe
end;
```

### Menus

Les menus s'adaptent automatiquement en mode RTL :

```pascal
procedure TFormPrincipal.CreerMenuRTL;
begin
  // Le menu principal s'inverse automatiquement
  Self.BiDiMode := bdRightToLeft;

  // Menu Fichier devient le plus à droite
  MenuFichier.Caption := 'ملف'; // "Fichier" en arabe
  MenuEdition.Caption := 'تحرير'; // "Édition" en arabe

  // Les sous-menus s'ouvrent vers la gauche
end;
```

### Barres d'outils

```pascal
procedure ConfigurerToolBarRTL(ToolBar: TToolBar);
var
  i: Integer;
  Bouton: TToolButton;
begin
  if ToolBar.BiDiMode = bdRightToLeft then
  begin
    // Inverser l'ordre des boutons
    for i := ToolBar.ButtonCount - 1 downto 0 do
    begin
      Bouton := ToolBar.Buttons[i];
      Bouton.Left := (ToolBar.ButtonCount - 1 - i) * (Bouton.Width + 2);
    end;
  end;
end;
```

### Onglets (PageControl)

```pascal
procedure ConfigurerOngletsRTL(PageControl: TPageControl);
begin
  PageControl.BiDiMode := bdRightToLeft;

  // Les onglets s'affichent de droite à gauche
  // Le premier onglet est à droite
  PageControl.TabSheet1.Caption := 'عام'; // "Général" en arabe
  PageControl.TabSheet2.Caption := 'متقدم'; // "Avancé" en arabe
end;
```

## Gestion des images et icônes

Certaines images doivent être inversées en mode RTL.

### Images directionnelles

Les images qui ont une direction (flèches, etc.) doivent être inversées :

```pascal
type
  TGestionnaireImagesRTL = class
  private
    FImagesFlecheGauche: TImageList;
    FImagesFlecheDroite: TImageList;
  public
    procedure ChargerImagesRTL;
    function ObtenirImageFleche(Gauche: Boolean; RTL: Boolean): TBitmap;
  end;

procedure TGestionnaireImagesRTL.ChargerImagesRTL;
begin
  // Charger les images dans les deux sens
  FImagesFlecheGauche := TImageList.Create(nil);
  FImagesFlecheDroite := TImageList.Create(nil);

  // En RTL, échanger les flèches
end;

function TGestionnaireImagesRTL.ObtenirImageFleche(Gauche: Boolean; RTL: Boolean): TBitmap;
begin
  // Si RTL, inverser la direction
  if RTL then
    Gauche := not Gauche;

  if Gauche then
    Result := ExtraireImage(FImagesFlecheGauche, 0)
  else
    Result := ExtraireImage(FImagesFlecheDroite, 0);
end;
```

### Images à inverser vs à ne pas inverser

| Type d'image | Action en RTL | Exemple |
|--------------|---------------|---------|
| **Flèches directionnelles** | Inverser | ← devient → |
| **Icônes de navigation** | Inverser | Suivant/Précédent |
| **Logos et marques** | Ne pas inverser | Logo de l'entreprise |
| **Icônes neutres** | Ne pas inverser | Engrenage, étoile |
| **Texte dans l'image** | Traduire, ne pas inverser | Capture d'écran |

### Fonction d'inversion d'image

```pascal
uses
  Vcl.Graphics;

function InverserImageHorizontalement(const ImageOriginale: TBitmap): TBitmap;
var
  x, y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := ImageOriginale.Width;
  Result.Height := ImageOriginale.Height;

  for y := 0 to ImageOriginale.Height - 1 do
    for x := 0 to ImageOriginale.Width - 1 do
      Result.Canvas.Pixels[ImageOriginale.Width - 1 - x, y] :=
        ImageOriginale.Canvas.Pixels[x, y];
end;

// Utilisation
procedure TForm1.ChargerImageRTL;
var
  ImageLTR, ImageRTL: TBitmap;
begin
  ImageLTR := TBitmap.Create;
  try
    ImageLTR.LoadFromFile('fleche_droite.bmp');

    if Self.BiDiMode = bdRightToLeft then
    begin
      ImageRTL := InverserImageHorizontalement(ImageLTR);
      try
        Image1.Picture.Bitmap := ImageRTL;
      finally
        ImageRTL.Free;
      end;
    end
    else
      Image1.Picture.Bitmap := ImageLTR;
  finally
    ImageLTR.Free;
  end;
end;
```

## Mise en page et alignement

### Utilisation des Anchors

Les Anchors fonctionnent bien avec RTL :

```pascal
procedure ConfigurerAnchorsRTL;
begin
  // Ces anchors s'adaptent automatiquement
  BtnOK.Anchors := [akRight, akBottom];     // Reste à droite en LTR, à gauche en RTL
  BtnAnnuler.Anchors := [akRight, akBottom];

  Panel1.Anchors := [akLeft, akTop, akRight, akBottom]; // S'étend correctement
end;
```

### Panels et conteneurs

```pascal
procedure ConfigurerPanelsRTL(Form: TForm);
begin
  // Panel gauche en LTR devient panel droit en RTL
  PanelMenu.BiDiMode := Form.BiDiMode;
  PanelMenu.Align := alRight; // Devient automatiquement alLeft en RTL

  // Panel principal
  PanelPrincipal.BiDiMode := Form.BiDiMode;
  PanelPrincipal.Align := alClient;
end;
```

### Grilles et listes

```pascal
procedure ConfigurerGridRTL(Grid: TStringGrid);
begin
  Grid.BiDiMode := bdRightToLeft;

  // Les colonnes s'affichent de droite à gauche
  // La première colonne est à droite

  // Inverser l'ordre des colonnes si nécessaire
  if Grid.BiDiMode = bdRightToLeft then
  begin
    Grid.ColCount := 3;
    Grid.Cells[0, 0] := 'العمود 3'; // Colonne 3
    Grid.Cells[1, 0] := 'العمود 2'; // Colonne 2
    Grid.Cells[2, 0] := 'العمود 1'; // Colonne 1
  end;
end;
```

## Texte mixte (BiDi)

Parfois, un texte RTL peut contenir des éléments LTR (nombres, mots anglais).

### Algorithme BiDi Unicode

Unicode définit un algorithme pour gérer le texte bidirectionnel :

```pascal
var
  TexteMixte: string;
begin
  // Texte arabe avec un mot anglais et un nombre
  TexteMixte := 'مرحبا Hello عالم 123';

  // Delphi gère automatiquement l'affichage :
  // Les mots arabes RTL, "Hello" LTR, "123" LTR
  Label1.Caption := TexteMixte;
end;
```

### Marques directionnelles Unicode

Pour contrôler finement la direction, utilisez des caractères de contrôle Unicode :

| Caractère | Code | Description | Usage |
|-----------|------|-------------|-------|
| LRM | U+200E | Left-to-Right Mark | Forcer LTR |
| RLM | U+200F | Right-to-Left Mark | Forcer RTL |
| LRE | U+202A | Left-to-Right Embedding | Début zone LTR |
| RLE | U+202B | Right-to-Left Embedding | Début zone RTL |
| PDF | U+202C | Pop Directional Formatting | Fin zone |

```pascal
const
  LRM = #$200E; // Left-to-Right Mark
  RLM = #$200F; // Right-to-Left Mark

procedure ExempleMarquesDirectionnelles;
var
  Texte: string;
begin
  // Forcer un nombre à rester LTR dans un texte RTL
  Texte := 'السعر:' + LRM + ' 1,234.56' + RLM + ' دولار';
  // "Prix: 1,234.56 dollars" en arabe avec le nombre correctement formaté

  Label1.Caption := Texte;
end;
```

## Gestion des raccourcis clavier

Les raccourcis clavier ne changent pas en RTL, mais leur position peut être confuse.

```pascal
procedure ConfigurerRaccourcisRTL;
begin
  // Les raccourcis restent les mêmes
  ActionFichierNouveau.ShortCut := ShortCut(Ord('N'), [ssCtrl]);

  // Mais le texte du menu change
  if Self.BiDiMode = bdRightToLeft then
    MenuNouveau.Caption := 'جديد' + #9 + 'Ctrl+N' // Arabe + raccourci
  else
    MenuNouveau.Caption := 'Nouveau' + #9 + 'Ctrl+N';
end;
```

## Problèmes courants et solutions

### Problème 1 : Barres de défilement mal positionnées

**Symptôme :** Les barres de défilement restent à droite en mode RTL

**Solution :**

```pascal
procedure CorrigerBarresDefilement(Memo: TMemo);
begin
  Memo.BiDiMode := bdRightToLeft;

  // Forcer la mise à jour
  Memo.Perform(WM_VSCROLL, SB_TOP, 0);
  Memo.ScrollBars := ssVertical;
end;
```

### Problème 2 : Composants non alignés

**Symptôme :** Certains composants ne s'alignent pas correctement

**Solution :**

```pascal
procedure RealignerComposantsRTL(Form: TForm);
var
  i: Integer;
  Composant: TComponent;
  Control: TControl;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    Composant := Form.Components[i];
    if Composant is TControl then
    begin
      Control := TControl(Composant);

      // Forcer la mise à jour
      Control.BiDiMode := Form.BiDiMode;
      Control.Invalidate;
    end;
  end;
end;
```

### Problème 3 : Texte dans les dialogues système

**Symptôme :** MessageDlg et InputBox restent en LTR

**Solution :**

```pascal
uses
  Vcl.Dialogs, Winapi.Windows;

function MessageDlgRTL(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): Integer;
var
  Dialog: TForm;
begin
  // Créer le dialogue
  Dialog := CreateMessageDialog(Msg, DlgType, Buttons);
  try
    // Configurer en RTL
    Dialog.BiDiMode := bdRightToLeft;

    // Afficher
    Result := Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

// Utilisation
procedure TForm1.AfficherMessageRTL;
begin
  MessageDlgRTL('هل أنت متأكد؟', mtConfirmation, [mbYes, mbNo]);
end;
```

### Problème 4 : Ordre de tabulation incorrect

**Symptôme :** La touche Tab ne suit pas l'ordre visuel

**Solution :**

```pascal
procedure InverserOrdreTabulation(Form: TForm);
var
  i: Integer;
  Controls: TList;
begin
  if Form.BiDiMode = bdRightToLeft then
  begin
    Controls := TList.Create;
    try
      // Collecter tous les contrôles
      for i := 0 to Form.ControlCount - 1 do
        Controls.Add(Form.Controls[i]);

      // Inverser l'ordre de tabulation
      for i := 0 to Controls.Count - 1 do
        TControl(Controls[i]).TabOrder := Controls.Count - 1 - i;
    finally
      Controls.Free;
    end;
  end;
end;
```

## Classe de gestion RTL

Créons une classe pour faciliter la gestion RTL :

```pascal
unit GestionnaireRTL;

interface

uses
  Vcl.Forms, Vcl.Controls, System.Classes, System.SysUtils;

type
  TGestionnaireRTL = class
  private
    FModeRTL: Boolean;
  public
    constructor Create;

    // Configuration
    procedure ActiverRTL(Form: TForm);
    procedure DesactiverRTL(Form: TForm);
    procedure BasculerRTL(Form: TForm);

    // Vérification
    function EstRTLActif: Boolean;
    function EstLangueRTL(const CodeLangue: string): Boolean;

    // Application
    procedure AppliquerATousLesFormulaires;
    procedure AppliquerAuxComposants(Container: TWinControl);

    // Mise en page
    procedure InverserBoutons(BtnGauche, BtnDroit: TControl);
    procedure RealignerPanels(Form: TForm);
    procedure AjusterAnchors(Control: TControl);

    property ModeRTL: Boolean read FModeRTL write FModeRTL;
  end;

var
  GestRTL: TGestionnaireRTL;

implementation

constructor TGestionnaireRTL.Create;
begin
  inherited;
  FModeRTL := False;
end;

procedure TGestionnaireRTL.ActiverRTL(Form: TForm);
begin
  FModeRTL := True;
  Form.BiDiMode := bdRightToLeft;
  AppliquerAuxComposants(Form);
end;

procedure TGestionnaireRTL.DesactiverRTL(Form: TForm);
begin
  FModeRTL := False;
  Form.BiDiMode := bdLeftToRight;
  AppliquerAuxComposants(Form);
end;

procedure TGestionnaireRTL.BasculerRTL(Form: TForm);
begin
  if FModeRTL then
    DesactiverRTL(Form)
  else
    ActiverRTL(Form);
end;

function TGestionnaireRTL.EstRTLActif: Boolean;
begin
  Result := FModeRTL;
end;

function TGestionnaireRTL.EstLangueRTL(const CodeLangue: string): Boolean;
begin
  Result := (CodeLangue = 'ar') or  // Arabe
            (CodeLangue = 'he') or  // Hébreu
            (CodeLangue = 'fa') or  // Persan
            (CodeLangue = 'ur');    // Ourdou
end;

procedure TGestionnaireRTL.AppliquerATousLesFormulaires;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    if FModeRTL then
      Screen.Forms[i].BiDiMode := bdRightToLeft
    else
      Screen.Forms[i].BiDiMode := bdLeftToRight;

    AppliquerAuxComposants(Screen.Forms[i]);
  end;
end;

procedure TGestionnaireRTL.AppliquerAuxComposants(Container: TWinControl);
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to Container.ControlCount - 1 do
  begin
    Control := Container.Controls[i];

    if FModeRTL then
      Control.BiDiMode := bdRightToLeft
    else
      Control.BiDiMode := bdLeftToRight;

    // Récursif pour les conteneurs
    if Control is TWinControl then
      AppliquerAuxComposants(TWinControl(Control));
  end;
end;

procedure TGestionnaireRTL.InverserBoutons(BtnGauche, BtnDroit: TControl);
var
  TempLeft: Integer;
begin
  if FModeRTL then
  begin
    TempLeft := BtnGauche.Left;
    BtnGauche.Left := BtnDroit.Left;
    BtnDroit.Left := TempLeft;
  end;
end;

procedure TGestionnaireRTL.RealignerPanels(Form: TForm);
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to Form.ControlCount - 1 do
  begin
    Control := Form.Controls[i];

    // Forcer le réalignement
    if Control is TPanel then
      Control.Realign;
  end;
end;

procedure TGestionnaireRTL.AjusterAnchors(Control: TControl);
begin
  // Les anchors s'inversent automatiquement avec BiDiMode
  // Cette méthode force la mise à jour
  Control.Anchors := Control.Anchors;
end;

initialization
  GestRTL := TGestionnaireRTL.Create;

finalization
  GestRTL.Free;

end.
```

### Utilisation de la classe

```pascal
uses
  GestionnaireRTL;

procedure TFormPrincipal.ChangerLangue(const CodeLangue: string);
begin
  // Charger les traductions
  GestionnaireTraduction.DefinirLangue(CodeLangue);

  // Appliquer RTL si nécessaire
  if GestRTL.EstLangueRTL(CodeLangue) then
    GestRTL.ActiverRTL(Self)
  else
    GestRTL.DesactiverRTL(Self);

  // Mettre à jour l'interface
  AppliquerTraductions;
end;

procedure TFormPrincipal.BtnTestRTLClick(Sender: TObject);
begin
  // Basculer entre RTL et LTR pour tester
  GestRTL.BasculerRTL(Self);
end;
```

## Tests spécifiques RTL

### Checklist de test RTL

```
Affichage:
  □ Le texte est aligné à droite
  □ Les menus s'ouvrent vers la gauche
  □ Les barres de défilement sont à gauche
  □ Les onglets commencent à droite
  □ L'interface est "miroir" de la version LTR

Navigation:
  □ La touche Tab suit l'ordre visuel (droite → gauche)
  □ Les flèches clavier fonctionnent correctement
  □ Les raccourcis clavier sont accessibles

Composants:
  □ Les dialogues système sont en RTL
  □ Les messages d'erreur sont alignés correctement
  □ Les tooltips s'affichent correctement
  □ Les listes déroulantes s'ouvrent vers la gauche

Mise en page:
  □ Aucun débordement de texte
  □ Les boutons sont dans le bon ordre
  □ Les icônes directionnelles sont inversées
  □ Les panels sont correctement positionnés

Texte mixte:
  □ Les nombres s'affichent correctement
  □ Les mots anglais dans le texte RTL sont lisibles
  □ Les URLs et emails sont corrects
  □ Les caractères de ponctuation sont bien placés
```

### Tests avec données réelles

```pascal
procedure TesterAffichageRTL;
const
  // Textes de test en arabe
  TEXTE_COURT = 'مرحبا';
  TEXTE_LONG = 'هذا نص طويل لاختبار التفاف النص في وضع RTL';
  TEXTE_MIXTE = 'مرحبا Hello عالم 123 www.example.com';
begin
  Label1.Caption := TEXTE_COURT;
  Memo1.Text := TEXTE_LONG;
  Edit1.Text := TEXTE_MIXTE;

  // Vérifier visuellement l'affichage
end;
```

## Bonnes pratiques RTL

### Recommandations

| Pratique | Description | Importance |
|----------|-------------|------------|
| **Tester tôt** | Tester RTL dès le début du développement | ⭐⭐⭐ |
| **Utiliser BiDiMode** | Toujours utiliser BiDiMode, pas de calculs manuels | ⭐⭐⭐ |
| **Anchors** | Privilégier Anchors aux positions fixes | ⭐⭐⭐ |
| **Locuteur natif** | Faire tester par un utilisateur RTL natif | ⭐⭐⭐ |
| **Images** | Inverser les images directionnelles | ⭐⭐ |
| **Texte mixte** | Tester avec du texte bidirectionnel réel | ⭐⭐ |
| **Ordre Tab** | Vérifier l'ordre de tabulation | ⭐⭐ |

### Ce qu'il faut éviter

```pascal
// ❌ MAUVAIS : Calculs de position manuels
if RTL then
  Button1.Left := Form.Width - Button1.Width - 10
else
  Button1.Left := 10;

// ✅ BON : Utiliser Anchors
Button1.Anchors := [akRight, akTop];  // S'adapte automatiquement


// ❌ MAUVAIS : Supposer la direction
Label1.Alignment := taLeftJustify;

// ✅ BON : Laisser BiDiMode gérer
// L'alignement s'adapte automatiquement


// ❌ MAUVAIS : Hardcoder l'ordre
Panel1.Left := 0;
Panel2.Left := 200;

// ✅ BON : Utiliser Align
Panel1.Align := alLeft;
Panel2.Align := alClient;
```

## Ressources et outils

### Polices recommandées pour RTL

| Police | Langues | Qualité | Disponibilité |
|--------|---------|---------|---------------|
| Arial | Arabe, hébreu | Moyenne | Windows |
| Tahoma | Arabe, hébreu | Bonne | Windows |
| Segoe UI | Arabe, hébreu | Excellente | Windows 7+ |
| Noto Sans Arabic | Arabe | Excellente | Google Fonts |
| Noto Sans Hebrew | Hébreu | Excellente | Google Fonts |

### Outils de test

```pascal
// Outil simple pour tester RTL/LTR
procedure CreerOutilTestRTL;
var
  Form: TForm;
  BtnBasculer: TButton;
  MemoTest: TMemo;
begin
  Form := TForm.Create(nil);
  Form.Caption := 'Test RTL/LTR';
  Form.Width := 600;
  Form.Height := 400;

  BtnBasculer := TButton.Create(Form);
  BtnBasculer.Parent := Form;
  BtnBasculer.Caption := 'Basculer RTL/LTR';
  BtnBasculer.Top := 10;
  BtnBasculer.Left := 10;
  BtnBasculer.OnClick := procedure(Sender: TObject)
    begin
      if Form.BiDiMode = bdLeftToRight then
        Form.BiDiMode := bdRightToLeft
      else
        Form.BiDiMode := bdLeftToRight;
    end;

  MemoTest := TMemo.Create(Form);
  MemoTest.Parent := Form;
  MemoTest.Top := 50;
  MemoTest.Left := 10;
  MemoTest.Width := Form.ClientWidth - 20;
  MemoTest.Height := Form.ClientHeight - 60;
  MemoTest.Anchors := [akLeft, akTop, akRight, akBottom];
  MemoTest.Text := 'مرحبا بالعالم' + #13#10 + 'Hello World' + #13#10 + '你好世界';

  Form.ShowModal;
  Form.Free;
end;
```

## Conclusion

Le support des langues RTL est essentiel pour toucher un public international de plusieurs centaines de millions de personnes. Delphi offre un excellent support pour RTL via la propriété `BiDiMode`, qui gère automatiquement la plupart des adaptations nécessaires.

**Points clés à retenir :**

- **BiDiMode** : Utiliser cette propriété pour tout le support RTL
- **Tester tôt** : Intégrer les tests RTL dès le début
- **Anchors** : Privilégier les anchors aux positions fixes
- **Images** : Inverser les images directionnelles
- **Locuteurs natifs** : Faire valider par des utilisateurs RTL
- **Texte mixte** : Tester avec du contenu bidirectionnel réel
- **Automatique** : Laisser Delphi gérer l'inversion, éviter les calculs manuels

En suivant ces recommandations et en utilisant les outils présentés dans cette section, vous pourrez créer des applications qui offrent une expérience utilisateur naturelle et professionnelle pour les utilisateurs de langues RTL.

⏭️ [Outils de traduction et flux de travail](/13-internationalisation-et-localisation/08-outils-de-traduction-et-flux-de-travail.md)
