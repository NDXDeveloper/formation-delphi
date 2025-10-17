🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.10 Styles visuels et thèmes VCL

## Introduction

Les styles visuels VCL (Visual Component Library) permettent de transformer l'apparence de votre application Windows sans modifier une seule ligne de code fonctionnel. C'est l'une des fonctionnalités les plus impressionnantes de Delphi moderne, qui permet de donner un aspect professionnel et contemporain à vos applications.

## 4.10.1 Qu'est-ce qu'un style VCL ?

### Définition

Un **style VCL** est un ensemble de paramètres visuels qui définissent l'apparence de tous les composants de votre application :
- Couleurs de fond et de texte
- Bordures et dégradés
- Polices et tailles
- Images et icônes
- Effets visuels

### Avant et après les styles VCL

**Sans style (apparence Windows classique) :**
```
┌─────────────────────────────────┐
│  Mon Application            [_][□][X]│
├─────────────────────────────────┤
│ Fichier  Edition  Affichage     │
├─────────────────────────────────┤
│                                  │
│  [ Bouton gris standard ]       │
│                                  │
│  [✓] Case à cocher              │
│                                  │
│  Apparence Windows basique      │
└─────────────────────────────────┘
```

**Avec style moderne (ex: Windows 11 Modern Dark) :**
```
┌─────────────────────────────────┐
│  Mon Application            [_][□][X]│
├─────────────────────────────────┤
│ Fichier  Edition  Affichage     │
├─────────────────────────────────┤
│                                  │
│  [ Bouton moderne arrondi  ]    │
│                                  │
│  [✓] Case moderne               │
│                                  │
│  Design contemporain et élégant │
└─────────────────────────────────┘
```

### Pourquoi utiliser des styles VCL ?

**1. Apparence moderne**
- Donnez un look contemporain à votre application
- Restez au goût du jour sans refaire l'interface
- Impressionnez vos utilisateurs

**2. Cohérence visuelle**
- Tous les composants adoptent le même style
- Harmonisation automatique des couleurs
- Aspect professionnel garanti

**3. Adaptabilité**
- Support du mode sombre (Dark Mode)
- Adaptation à Windows 11
- Respect des préférences utilisateur

**4. Personnalisation**
- Créez votre propre identité visuelle
- Démarquez-vous de la concurrence
- Alignez-vous avec votre charte graphique

**5. Facilité**
- Pas de modification du code
- Application en quelques clics
- Changement dynamique possible

---

## 4.10.2 Styles prédéfinis de Delphi 13

### Liste des styles disponibles

Delphi 13 Florence inclut de nombreux styles prêts à l'emploi :

**Styles clairs :**
- `Windows` (style par défaut)
- `Windows10`
- `Windows11 Modern Light`
- `Light`
- `Silver`
- `Sky`
- `Aqua Light Slate`

**Styles sombres :**
- `Windows11 Modern Dark`
- `Carbon`
- `Charcoal Dark Slate`
- `Obsidian`
- `Glossy`

**Styles colorés :**
- `Ruby Graphite`
- `Sapphire Kamri`
- `Emerald Light Slate`
- `Amethyst Kamri`

**Styles Windows classiques :**
- `Luna`
- `Amakrits`
- `Aqua Graphite`

### Aperçu des styles populaires

#### Windows11 Modern Light
```
Caractéristiques :
- Arrière-plan blanc/gris clair
- Bordures arrondies
- Ombres douces
- Police moderne (Segoe UI)
- Boutons avec effet hover
```

#### Windows11 Modern Dark
```
Caractéristiques :
- Arrière-plan sombre (#1E1E1E)
- Texte clair (#FFFFFF)
- Contraste élevé
- Moins de fatigue oculaire
- Aspect professionnel
```

#### Carbon
```
Caractéristiques :
- Noir profond
- Accents bleus
- Style élégant
- Idéal pour applications créatives
```

---

## 4.10.3 Appliquer un style à votre application

### Méthode 1 : Configuration du projet (Design Time)

C'est la méthode la plus simple pour appliquer un style permanent.

#### Étapes détaillées

**1. Ouvrir les options du projet**
```
Menu Projet → Options
ou
Clic droit sur le projet → Options
```

**2. Naviguer vers les styles**
```
Apparence → Styles VCL personnalisés
```

**3. Sélectionner les styles**
```
☑ Windows11 Modern Dark
☑ Windows11 Modern Light
☑ Carbon
☐ Tous les autres (décocher)
```

**4. Définir le style par défaut**
```
Dans la liste déroulante en haut :
"Style par défaut" → Sélectionner "Windows11 Modern Dark"
```

**5. Appliquer et compiler**
```
Cliquer sur OK
Compiler le projet (Ctrl + F9)
Exécuter (F9)
```

**Important :** Les styles sélectionnés seront inclus dans l'exécutable, augmentant légèrement sa taille.

### Méthode 2 : Par code (Runtime)

Pour changer le style dynamiquement pendant l'exécution.

```pascal
uses
  Vcl.Themes;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Vérifier si un style est disponible
  if TStyleManager.IsValidStyle('Windows11 Modern Dark') then
  begin
    // Appliquer le style
    TStyleManager.SetStyle('Windows11 Modern Dark');
  end;
end;
```

### Méthode 3 : Sélection par l'utilisateur

Permettre à l'utilisateur de choisir son style préféré.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Remplir une ComboBox avec les styles disponibles
  ComboBoxStyles.Items.Clear;
  for StyleName in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(StyleName);

  // Sélectionner le style actuel
  ComboBoxStyles.ItemIndex := ComboBoxStyles.Items.IndexOf(
    TStyleManager.ActiveStyle.Name
  );
end;

procedure TForm1.ComboBoxStylesChange(Sender: TObject);
begin
  // Appliquer le style sélectionné
  if ComboBoxStyles.ItemIndex <> -1 then
    TStyleManager.SetStyle(ComboBoxStyles.Items[ComboBoxStyles.ItemIndex]);
end;
```

---

## 4.10.4 Styles VCL en mode conception - Prototypage rapide

### Aperçu des styles dans l'IDE

Delphi 13 permet de visualiser les styles directement pendant la conception.

**Activer l'aperçu des styles :**

**1. Dans l'Inspecteur d'objets du formulaire**
```
Propriété : StyleElements
Valeur : [seFont, seClient, seBorder]  (par défaut, activé)
```

**2. Prévisualisation en temps réel**
```
Menu Affichage → Aperçu du style
ou
Barre d'outils → Sélecteur de style
```

**3. Changer temporairement le style de l'IDE**
```
Menu Outils → Options
Environnement → Thème de l'IDE
```

### Conception avec aperçu du style

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Le formulaire et ses composants respectent automatiquement
  // le style défini dans les options du projet

  // Tous ces composants adopteront le style actif :
  // - Boutons
  // - Zones de texte
  // - Listes
  // - Grilles
  // - Menus
  // etc.
end;
```

### StyleElements : Contrôle fin

La propriété `StyleElements` permet de contrôler quels aspects du style sont appliqués.

```pascal
// Appliquer tous les éléments de style (par défaut)
Button1.StyleElements := [seFont, seClient, seBorder];

// Désactiver le style de police uniquement
Button1.StyleElements := [seClient, seBorder];
Button1.Font.Color := clRed; // Cette couleur sera respectée

// Désactiver complètement le style pour ce composant
Button1.StyleElements := [];
Button1.Color := clYellow; // Couleur personnalisée
```

**Éléments de StyleElements :**

| Élément | Description |
|---------|-------------|
| `seFont` | Police (couleur, taille, style) |
| `seClient` | Zone cliente (arrière-plan) |
| `seBorder` | Bordures |

---

## 4.10.5 Gestion avancée des styles

### Obtenir des informations sur le style actif

```pascal
uses
  Vcl.Themes;

procedure TForm1.AfficherInfosStyle;
var
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  Memo1.Lines.Clear;
  Memo1.Lines.Add('Nom du style : ' + Style.Name);
  Memo1.Lines.Add('Fichier : ' + TStyleManager.ActiveStyle.FileName);

  // Vérifier si c'est un style sombre
  if Style.GetSystemColor(clWindow) = clBlack then
    Memo1.Lines.Add('Mode : Sombre')
  else
    Memo1.Lines.Add('Mode : Clair');
end;
```

### Lister tous les styles disponibles

```pascal
procedure TForm1.ListerStyles;
var
  StyleName: string;
begin
  ListBox1.Items.Clear;

  for StyleName in TStyleManager.StyleNames do
  begin
    ListBox1.Items.Add(StyleName);

    // Marquer le style actif
    if StyleName = TStyleManager.ActiveStyle.Name then
      ListBox1.Items[ListBox1.Items.Count - 1] :=
        '► ' + ListBox1.Items[ListBox1.Items.Count - 1];
  end;
end;
```

### Charger un style externe

```pascal
procedure TForm1.ChargerStyleExterne;
var
  NomFichier: string;
begin
  OpenDialog1.Filter := 'Styles VCL (*.vsf)|*.vsf';
  if OpenDialog1.Execute then
  begin
    NomFichier := OpenDialog1.FileName;

    try
      // Charger le style
      if TStyleManager.IsValidStyle(NomFichier) then
      begin
        TStyleManager.LoadFromFile(NomFichier);
        ShowMessage('Style chargé avec succès');
      end
      else
        ShowMessage('Fichier de style invalide');
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

### Sauvegarder les préférences utilisateur

```pascal
uses
  System.IniFiles;

procedure TForm1.SauvegarderPreferencesStyle;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteString('Apparence', 'Style', TStyleManager.ActiveStyle.Name);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.ChargerPreferencesStyle;
var
  Ini: TIniFile;
  StyleName: string;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    StyleName := Ini.ReadString('Apparence', 'Style', 'Windows11 Modern Light');

    if TStyleManager.IsValidStyle(StyleName) then
      TStyleManager.SetStyle(StyleName);
  finally
    Ini.Free;
  end;
end;
```

---

## 4.10.6 Adapter votre code aux styles

### Utiliser les couleurs du style

Au lieu de coder des couleurs en dur, utilisez les couleurs du style actif.

**Mauvaise pratique :**
```pascal
procedure TForm1.DessinerFond;
begin
  Canvas.Brush.Color := clWhite; // Toujours blanc, même en mode sombre
  Canvas.FillRect(ClientRect);
  Canvas.Font.Color := clBlack;  // Toujours noir
  Canvas.TextOut(10, 10, 'Texte');
end;
```

**Bonne pratique :**
```pascal
uses
  Vcl.Themes;

procedure TForm1.DessinerFond;
var
  StyleServices: TCustomStyleServices;
begin
  StyleServices := TStyleManager.ActiveStyle;

  // Utiliser les couleurs du style actif
  Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
  Canvas.FillRect(ClientRect);

  Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
  Canvas.TextOut(10, 10, 'Texte');
end;
```

### Couleurs système disponibles

```pascal
// Couleurs principales
clWindow        // Arrière-plan des fenêtres
clWindowText    // Texte des fenêtres
clBtnFace       // Fond des boutons
clBtnText       // Texte des boutons
clHighlight     // Sélection
clHighlightText // Texte sélectionné

// Exemple d'utilisation
procedure TForm1.AppliquerCouleurs;
var
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  Panel1.Color := Style.GetSystemColor(clBtnFace);
  Label1.Font.Color := Style.GetSystemColor(clBtnText);
  Edit1.Color := Style.GetSystemColor(clWindow);
  Edit1.Font.Color := Style.GetSystemColor(clWindowText);
end;
```

### Dessiner avec le style

```pascal
uses
  Vcl.Themes;

procedure TForm1.Image1Paint(Sender: TObject);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  R := Image1.ClientRect;

  // Dessiner un bouton avec le style actif
  Details := TStyleManager.ActiveStyle.GetElementDetails(tbPushButtonNormal);
  TStyleManager.ActiveStyle.DrawElement(Image1.Canvas.Handle, Details, R);

  // Dessiner du texte centré
  TStyleManager.ActiveStyle.DrawText(Image1.Canvas.Handle, Details,
    'Bouton stylisé', R, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;
```

---

## 4.10.7 Styles et composants tiers

### Problèmes courants

Certains composants tiers ne supportent pas automatiquement les styles VCL.

**Symptômes :**
- Apparence incohérente
- Composant reste en style Windows classique
- Couleurs inadaptées au thème

### Solution 1 : StyleElements

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Forcer l'application du style
  ComposantTiers1.StyleElements := [seFont, seClient, seBorder];
end;
```

### Solution 2 : Hooks de style

Pour des composants plus complexes :

```pascal
uses
  Vcl.Themes;

type
  TStyleHook_MonComposant = class(TScrollingStyleHook)
  protected
    procedure Paint(Canvas: TCanvas); override;
  end;

procedure TStyleHook_MonComposant.Paint(Canvas: TCanvas);
begin
  // Code de dessin personnalisé avec le style
  inherited;
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TMonComposant, TStyleHook_MonComposant);
```

### Solution 3 : Mise à jour du composant

Vérifiez si une version récente du composant supporte les styles VCL.

---

## 4.10.8 Créer un menu de sélection de style

### Interface utilisateur complète

```pascal
unit FormPreferencesStyle;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Themes;

type
  TFormPreferences = class(TForm)
    GroupBoxStyle: TGroupBox;
    RadioGroupStyles: TRadioGroup;
    ButtonAppliquer: TButton;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    PanelApercu: TPanel;
    LabelApercu: TLabel;
    ButtonApercu: TButton;
    CheckBoxApercu: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupStylesClick(Sender: TObject);
    procedure ButtonAppliquerClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FStyleInitial: string;
    procedure ChargerStyles;
    procedure AppercuStyle(const StyleName: string);
  end;

var
  FormPreferences: TFormPreferences;

implementation

{$R *.dfm}

procedure TFormPreferences.FormCreate(Sender: TObject);
begin
  // Sauvegarder le style actuel
  FStyleInitial := TStyleManager.ActiveStyle.Name;

  // Charger la liste des styles
  ChargerStyles;

  // Sélectionner le style actuel
  RadioGroupStyles.ItemIndex := RadioGroupStyles.Items.IndexOf(FStyleInitial);
end;

procedure TFormPreferences.ChargerStyles;
var
  StyleName: string;
begin
  RadioGroupStyles.Items.Clear;

  // Ajouter tous les styles disponibles
  for StyleName in TStyleManager.StyleNames do
    RadioGroupStyles.Items.Add(StyleName);
end;

procedure TFormPreferences.RadioGroupStylesClick(Sender: TObject);
begin
  if RadioGroupStyles.ItemIndex <> -1 then
    AppercuStyle(RadioGroupStyles.Items[RadioGroupStyles.ItemIndex]);
end;

procedure TFormPreferences.AppercuStyle(const StyleName: string);
begin
  // Appliquer temporairement le style pour l'aperçu
  if TStyleManager.IsValidStyle(StyleName) then
  begin
    TStyleManager.SetStyle(StyleName);

    // Mettre à jour l'aperçu
    LabelApercu.Caption := 'Aperçu du style : ' + StyleName;

    // Forcer le redessin
    PanelApercu.Invalidate;
  end;
end;

procedure TFormPreferences.ButtonAppliquerClick(Sender: TObject);
begin
  // Appliquer définitivement le style sélectionné
  if RadioGroupStyles.ItemIndex <> -1 then
  begin
    TStyleManager.SetStyle(RadioGroupStyles.Items[RadioGroupStyles.ItemIndex]);
    ShowMessage('Style appliqué avec succès');
  end;
end;

procedure TFormPreferences.ButtonOKClick(Sender: TObject);
begin
  ButtonAppliquerClick(Sender);
  ModalResult := mrOk;
end;

end.
```

### Exemple d'utilisation

```pascal
procedure TFormMain.MenuOptionsStyleClick(Sender: TObject);
begin
  with TFormPreferences.Create(Self) do
  try
    if ShowModal = mrOk then
    begin
      // Le style a été appliqué
      // Optionnel : Sauvegarder la préférence
      SauvegarderPreferencesStyle;
    end;
  finally
    Free;
  end;
end;
```

---

## 4.10.9 Mode sombre et mode clair

### Détecter le mode Windows

Windows 10/11 permet aux utilisateurs de choisir entre mode clair et sombre.

```pascal
uses
  Winapi.Windows, System.Win.Registry;

function EstModeSombreWindows: Boolean;
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

### Appliquer automatiquement le bon style

```pascal
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Appliquer le style selon les préférences Windows
  if EstModeSombreWindows then
  begin
    if TStyleManager.IsValidStyle('Windows11 Modern Dark') then
      TStyleManager.SetStyle('Windows11 Modern Dark');
  end
  else
  begin
    if TStyleManager.IsValidStyle('Windows11 Modern Light') then
      TStyleManager.SetStyle('Windows11 Modern Light');
  end;
end;
```

### Basculer entre modes

```pascal
procedure TFormMain.MenuBasculerModeClick(Sender: TObject);
var
  StyleActuel: string;
begin
  StyleActuel := TStyleManager.ActiveStyle.Name;

  // Basculer entre mode clair et sombre
  if Pos('Dark', StyleActuel) > 0 then
  begin
    // Passer au mode clair
    if TStyleManager.IsValidStyle('Windows11 Modern Light') then
      TStyleManager.SetStyle('Windows11 Modern Light');
  end
  else
  begin
    // Passer au mode sombre
    if TStyleManager.IsValidStyle('Windows11 Modern Dark') then
      TStyleManager.SetStyle('Windows11 Modern Dark');
  end;

  // Mettre à jour l'icône du menu
  ActualiserIconeMode;
end;
```

---

## 4.10.10 Personnalisation avancée

### Créer un style personnalisé

Delphi inclut un éditeur de styles visuels.

**Lancer l'éditeur de styles :**
```
Menu Outils → Bitmap Style Designer
```

**Processus de création :**

1. **Charger un style de base**
   - Ouvrir un style existant (.vsf)
   - Partir d'un modèle proche de ce que vous voulez

2. **Modifier les couleurs**
   - Onglet "Colors"
   - Changer les couleurs principales
   - Tester avec l'aperçu

3. **Personnaliser les composants**
   - Sélectionner chaque type de composant
   - Modifier les bitmaps et couleurs
   - Ajuster les marges et espacements

4. **Enregistrer le style**
   - Fichier → Enregistrer sous
   - Format .vsf
   - Donner un nom descriptif

### Appliquer votre style personnalisé

```pascal
// Méthode 1 : Inclure dans les options du projet
// Projet → Options → Apparence → Styles VCL personnalisés
// Cliquer sur "Ajouter" et sélectionner votre fichier .vsf

// Méthode 2 : Charger dynamiquement
procedure TForm1.ChargerStylePersonnalise;
var
  StylePath: string;
begin
  StylePath := ExtractFilePath(Application.ExeName) + 'Styles\MonStyle.vsf';

  if FileExists(StylePath) then
  begin
    try
      TStyleManager.LoadFromFile(StylePath);
      TStyleManager.SetStyle('MonStyle');
      ShowMessage('Style personnalisé chargé');
    except
      on E: Exception do
        ShowMessage('Erreur de chargement : ' + E.Message);
    end;
  end;
end;
```

### Modifier des couleurs spécifiques

```pascal
uses
  Vcl.Themes;

procedure TForm1.PersonnaliserCouleurs;
var
  Style: TCustomStyleServices;
begin
  Style := TStyleManager.ActiveStyle;

  // Modifier temporairement des couleurs
  // Note : Ceci affecte l'instance actuelle uniquement

  // Exemple : Forcer une couleur d'accentuation
  Panel1.Color := RGB(0, 120, 215); // Bleu Windows
  Panel1.StyleElements := [seFont, seBorder]; // Garder style sauf couleur
end;
```

---

## 4.10.11 Performance et optimisation

### Impact sur les performances

**Temps de démarrage :**
- Les styles ajoutent environ 50-200ms au démarrage
- Négligeable pour la plupart des applications
- Styles chargés une seule fois

**Utilisation mémoire :**
- Environ 2-5 Mo par style chargé
- Recommandé : Inclure seulement 2-3 styles
- Libérer les styles non utilisés si nécessaire

### Optimisations

**1. Inclure seulement les styles nécessaires**
```pascal
// Dans les options du projet, ne cocher que :
☑ Windows11 Modern Light
☑ Windows11 Modern Dark
☐ Tous les autres (décocher)
```

**2. Charger les styles à la demande**
```pascal
procedure TForm1.ChargerStyleSiNecessaire(const StyleName: string);
begin
  if not TStyleManager.IsValidStyle(StyleName) then
  begin
    // Charger depuis un fichier externe
    TStyleManager.LoadFromFile(StyleName + '.vsf');
  end;

  TStyleManager.SetStyle(StyleName);
end;
```

**3. Libérer les styles non utilisés**
```pascal
procedure TForm1.LibererStylesInutilises;
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
  begin
    if StyleName <> TStyleManager.ActiveStyle.Name then
      // Note : Il n'y a pas de méthode UnloadStyle dans Delphi
      // Les styles restent en mémoire une fois chargés
      ;
  end;
end;
```

---

## 4.10.12 Problèmes courants et solutions

### Problème 1 : Texte illisible

**Symptôme :**
Le texte n'est pas visible ou mal contrasté.

**Cause :**
Couleur de texte codée en dur.

**Solution :**
```pascal
// Mauvais
Label1.Font.Color := clBlack;

// Bon
Label1.Font.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindowText);
Label1.StyleElements := [seFont, seClient, seBorder];
```

### Problème 2 : Composant ne respecte pas le style

**Symptôme :**
Un composant garde l'apparence Windows standard.

**Solution :**
```pascal
// Vérifier StyleElements
MonComposant.StyleElements := [seFont, seClient, seBorder];

// Forcer le redessin
MonComposant.Invalidate;
MonComposant.Repaint;
```

### Problème 3 : Changement de style lent

**Symptôme :**
L'application se fige lors du changement de style.

**Solution :**
```pascal
procedure TForm1.ChangerStyleAsync(const NouveauStyle: string);
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;

    TStyleManager.SetStyle(NouveauStyle);

    // Forcer la mise à jour de tous les formulaires
    Application.ProcessMessages;
  finally
    Screen.Cursor := crDefault;
  end;
end;
```

### Problème 4 : Certaines zones ne se mettent pas à jour

**Symptôme :**
Après changement de style, certaines zones gardent l'ancien style.

**Solution :**
```pascal
procedure TForm1.ActualiserTout;
var
  i: Integer;
begin
  // Forcer le redessin du formulaire principal
  Invalidate;

  // Forcer le redessin de tous les contrôles
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TControl then
      TControl(Components[i]).Invalidate;
  end;

  // Forcer le redessin de tous les formulaires ouverts
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].Invalidate;
end;
```

### Problème 5 : Images avec fond blanc

**Symptôme :**
Les icônes avec fond blanc sont visibles en mode sombre.

**Solution :**
```pascal
// Utiliser des images avec transparence (PNG)
// Ou utiliser plusieurs ImageList selon le thème

procedure TForm1.ActualiserImages;
begin
  if Pos('Dark', TStyleManager.ActiveStyle.Name) > 0 then
  begin
    // Images pour mode sombre
    ToolBar1.Images := ImageListDark;
    MainMenu1.Images := ImageListDark;
  end
  else
  begin
    // Images pour mode clair
    ToolBar1.Images := ImageListLight;
    MainMenu1.Images := ImageListLight;
  end;
end;
```

---

## 4.10.13 Bonnes pratiques

### 1. Toujours utiliser StyleElements

```pascal
// Appliquer à tous les composants lors de la création
procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TControl then
      TControl(Components[i]).StyleElements := [seFont, seClient, seBorder];
  end;
end;
```

### 2. Tester avec plusieurs styles

```pascal
// Pendant le développement, tester régulièrement avec :
// - Un style clair (Windows11 Modern Light)
// - Un style sombre (Windows11 Modern Dark)
// - Le style Windows par défaut
```

### 3. Éviter les couleurs codées en dur

```pascal
// À éviter
Panel1.Color := clWhite;
Label1.Font.Color := clBlack;

// Préférer
Panel1.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindow);
Label1.Font.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindowText);
```

### 4. Gérer les images correctement

```pascal
// Utiliser des images vectorielles ou PNG avec transparence
// Adapter les couleurs des images selon le thème
// Prévoir deux jeux d'icônes (clair/sombre) si nécessaire
```

### 5. Documenter le style par défaut

```pascal
{
  CONFIGURATION DES STYLES
  ========================

  Style par défaut : Windows11 Modern Light
  Styles inclus :
    - Windows11 Modern Light
    - Windows11 Modern Dark

  L'utilisateur peut changer le style via :
    Menu Options → Apparence → Sélectionner un thème

  Le choix est sauvegardé dans :
    %AppData%\MonApplication\config.ini
}
```

### 6. Prévoir un mode sombre

```pascal
// Toujours inclure au minimum :
// - Un style clair
// - Un style sombre
// Et permettre à l'utilisateur de choisir
```

### 7. Tester l'accessibilité

```pascal
// Vérifier :
// - Le contraste des couleurs
// - La lisibilité des polices
// - La taille des éléments interactifs
// - Le support des lecteurs d'écran
```

---

## 4.10.14 Exemple complet : Application avec sélection de style

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.IniFiles,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Themes;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuOptions: TMenuItem;
    MenuAide: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuTheme: TMenuItem;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuThemeClick(Sender: TObject);
  private
    procedure ChargerPreferences;
    procedure SauvegarderPreferences;
    procedure CreerMenuStyles;
    procedure ChangerStyle(Sender: TObject);
    procedure MettreAJourStyleActif;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Charger les préférences sauvegardées
  ChargerPreferences;

  // Créer le menu de sélection de styles
  CreerMenuStyles;

  // Mettre à jour l'affichage
  MettreAJourStyleActif;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Sauvegarder les préférences
  SauvegarderPreferences;
end;

procedure TFormMain.ChargerPreferences;
var
  Ini: TIniFile;
  StyleName: string;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Charger le style préféré
    StyleName := Ini.ReadString('Apparence', 'Style', 'Windows11 Modern Light');

    if TStyleManager.IsValidStyle(StyleName) then
      TStyleManager.SetStyle(StyleName);
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.SauvegarderPreferences;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Sauvegarder le style actuel
    Ini.WriteString('Apparence', 'Style', TStyleManager.ActiveStyle.Name);
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.CreerMenuStyles;
var
  StyleName: string;
  MenuItem: TMenuItem;
begin
  // Vider le menu existant
  MenuTheme.Clear;

  // Ajouter un élément pour chaque style disponible
  for StyleName in TStyleManager.StyleNames do
  begin
    MenuItem := TMenuItem.Create(MenuTheme);
    MenuItem.Caption := StyleName;
    MenuItem.RadioItem := True; // Comportement bouton radio
    MenuItem.GroupIndex := 1;
    MenuItem.OnClick := ChangerStyle;

    // Cocher le style actif
    if StyleName = TStyleManager.ActiveStyle.Name then
      MenuItem.Checked := True;

    MenuTheme.Add(MenuItem);
  end;
end;

procedure TFormMain.ChangerStyle(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);

    // Changer le style
    if TStyleManager.IsValidStyle(MenuItem.Caption) then
    begin
      Screen.Cursor := crHourGlass;
      try
        TStyleManager.SetStyle(MenuItem.Caption);
        MettreAJourStyleActif;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TFormMain.MettreAJourStyleActif;
begin
  // Afficher le style actif dans la barre d'état
  StatusBar1.SimpleText := 'Style actif : ' + TStyleManager.ActiveStyle.Name;

  // Forcer le redessin
  Invalidate;
end;

procedure TFormMain.MenuThemeClick(Sender: TObject);
begin
  // Recréer le menu au cas où des styles auraient été ajoutés
  CreerMenuStyles;
end;

procedure TFormMain.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

end.
```

---

## Conclusion

Les styles visuels VCL sont un outil puissant pour moderniser vos applications Delphi. Avec Delphi 13 Florence, vous bénéficiez d'un support complet de Windows 11 et de styles modernes prêts à l'emploi.

### Points clés à retenir :

- **Facilité** : Appliquer un style en quelques clics
- **Modernité** : Aspect contemporain garanti
- **Flexibilité** : Changement dynamique possible
- **Cohérence** : Tous les composants harmonisés
- **Personnalisation** : Créez vos propres styles
- **Performance** : Impact minimal sur les performances
- **Accessibilité** : Support du mode sombre
- **Compatibilité** : Fonctionne avec la plupart des composants VCL

### Recommandations finales :

1. Incluez toujours au moins un style clair et un style sombre
2. Utilisez `StyleElements` sur tous vos composants
3. Évitez les couleurs codées en dur
4. Testez votre application avec différents styles
5. Permettez à l'utilisateur de choisir son style préféré
6. Sauvegardez les préférences utilisateur
7. Adaptez vos images aux différents thèmes
8. Documentez le style par défaut de votre application

Avec ces connaissances, vous êtes prêt à créer des applications Delphi modernes et élégantes qui s'intègrent parfaitement à Windows 11 ! 🎨

⏭️ [Styles VCL en mode conception - Prototypage rapide](/04-conception-dinterfaces-utilisateur-avec-la-vcl/10.1-styles-vcl-en-mode-conception.md)
