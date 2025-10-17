🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.13 Modernisation des applications VCL pour Windows 11

## Introduction

Windows 11 introduit une nouvelle esthétique et de nouvelles fonctionnalités qui transforment l'expérience utilisateur. Pour que vos applications Delphi restent modernes et professionnelles, il est essentiel de les adapter à ce nouvel environnement. Ce chapitre vous guidera dans la modernisation de vos applications VCL pour Windows 11.

## 4.13.1 Nouveautés de Windows 11

### Changements visuels majeurs

**Design Fluent raffiné :**
```
Windows 10                      Windows 11
┌──────────────┐               ╭──────────────╮
│  Fenêtre     │      →        │  Fenêtre     │
│              │               │              │
│  [Bouton]    │               │  [Bouton]    │
└──────────────┘               ╰──────────────╯
Coins carrés                   Coins arrondis

Boutons plats                  Boutons avec ombres
Menus simples                  Menus avec effets
```

**Éléments clés du design Windows 11 :**
- Coins arrondis partout (fenêtres, boutons, menus)
- Ombres douces et subtiles
- Effets de transparence (Acrylic, Mica)
- Animations fluides
- Espacement généreux
- Palette de couleurs raffinée
- Icônes redessinées

### Nouvelles fonctionnalités

**Snap Layouts :**
- Disposition intelligente des fenêtres
- Groupes de fenêtres mémorisés
- Accès via le bouton maximiser

**Centre de widgets :**
- Informations personnalisées
- Météo, actualités, calendrier
- API pour intégration

**Barre des tâches centrée :**
- Nouvelle position par défaut
- Menu démarrer centré
- Icônes centrées

**Mode sombre amélioré :**
- Meilleure intégration système
- Thèmes automatiques
- Support natif étendu

### Exigences techniques

**Configuration minimale :**
```
Processeur : 1 GHz dual-core 64-bit
RAM : 4 GB
Stockage : 64 GB
TPM : Version 2.0
UEFI : Secure Boot capable
```

**Pour les développeurs :**
```
Windows 11 SDK
Visual Studio 2019/2022 (optionnel)
Delphi 11+ (Delphi 13 recommandé)
```

---

## 4.13.2 Styles VCL pour Windows 11

### Styles Windows 11 intégrés

Delphi 13 Florence inclut des styles spécialement conçus pour Windows 11.

**Styles disponibles :**

```pascal
const
  STYLES_WINDOWS11: array[0..1] of string = (
    'Windows11 Modern Light',  // Thème clair
    'Windows11 Modern Dark'    // Thème sombre
  );
```

### Appliquer le style Windows 11

**Méthode 1 : Configuration du projet**

```
1. Projet → Options
2. Apparence → Styles VCL personnalisés
3. Cocher "Windows11 Modern Light"
4. Cocher "Windows11 Modern Dark"
5. Style par défaut : "Windows11 Modern Light"
6. OK
```

**Méthode 2 : Par code**

```pascal
uses
  Vcl.Themes;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Appliquer le style Windows 11
  if TStyleManager.IsValidStyle('Windows11 Modern Light') then
    TStyleManager.SetStyle('Windows11 Modern Light');
end;
```

### Adaptation automatique au thème système

```pascal
uses
  Winapi.Windows, System.Win.Registry, Vcl.Themes;

function GetWindowsTheme: string;
var
  Reg: TRegistry;
  UseLightTheme: Integer;
begin
  Result := 'Windows11 Modern Light'; // Par défaut

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(
      'SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
      begin
        UseLightTheme := Reg.ReadInteger('AppsUseLightTheme');
        if UseLightTheme = 0 then
          Result := 'Windows11 Modern Dark'
        else
          Result := 'Windows11 Modern Light';
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ThemeName: string;
begin
  // Détecter et appliquer le thème système
  ThemeName := GetWindowsTheme;

  if TStyleManager.IsValidStyle(ThemeName) then
    TStyleManager.SetStyle(ThemeName);
end;
```

### Surveiller les changements de thème

```pascal
type
  TFormMain = class(TForm)
  private
    procedure WMSettingChange(var Message: TWMSettingChange);
      message WM_SETTINGCHANGE;
  end;

procedure TFormMain.WMSettingChange(var Message: TWMSettingChange);
var
  Section: string;
begin
  inherited;

  // Vérifier si c'est un changement de thème
  Section := string(Message.Section);
  if (Section = 'ImmersiveColorSet') or (Section = 'WindowMetrics') then
  begin
    // Le thème système a changé
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(500); // Petit délai pour laisser le système se mettre à jour
        TThread.Synchronize(nil,
          procedure
          var
            NewTheme: string;
          begin
            NewTheme := GetWindowsTheme;
            if TStyleManager.IsValidStyle(NewTheme) then
              TStyleManager.SetStyle(NewTheme);
          end
        );
      end
    ).Start;
  end;
end;
```

---

## 4.13.3 Coins arrondis et bordures modernes

### Activer les coins arrondis (Windows 11 22H2+)

Windows 11 22H2 a introduit les coins arrondis pour les fenêtres des applications.

**API DWM pour coins arrondis :**

```pascal
uses
  Winapi.Windows, Winapi.Messages;

const
  DWMWA_WINDOW_CORNER_PREFERENCE = 33;

  DWMWCP_DEFAULT = 0;      // Laisser le système décider
  DWMWCP_DONOTROUND = 1;   // Coins carrés
  DWMWCP_ROUND = 2;        // Coins arrondis
  DWMWCP_ROUNDSMALL = 3;   // Coins légèrement arrondis

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD;
  pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  external 'dwmapi.dll';

procedure TFormMain.ActiverCoinsArrondis;
var
  Preference: Integer;
begin
  Preference := DWMWCP_ROUND;
  DwmSetWindowAttribute(Handle, DWMWA_WINDOW_CORNER_PREFERENCE,
    @Preference, SizeOf(Preference));
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Vérifier si Windows 11 22H2 ou supérieur
  if (TOSVersion.Major >= 10) and (TOSVersion.Build >= 22621) then
    ActiverCoinsArrondis;
end;
```

### Bordure personnalisée

```pascal
const
  DWMWA_BORDER_COLOR = 34;
  DWMWA_CAPTION_COLOR = 35;
  DWMWA_TEXT_COLOR = 36;

procedure TFormMain.PersonnaliserBordure;
var
  CouleurBordure: COLORREF;
  CouleurTitre: COLORREF;
begin
  // Couleur de bordure (accent Windows 11)
  CouleurBordure := RGB(0, 120, 215); // Bleu Windows
  DwmSetWindowAttribute(Handle, DWMWA_BORDER_COLOR,
    @CouleurBordure, SizeOf(CouleurBordure));

  // Couleur de la barre de titre
  CouleurTitre := RGB(32, 32, 32); // Gris foncé
  DwmSetWindowAttribute(Handle, DWMWA_CAPTION_COLOR,
    @CouleurTitre, SizeOf(CouleurTitre));
end;
```

### Effet Mica (arrière-plan translucide)

L'effet Mica donne un arrière-plan semi-transparent avec un léger flou.

```pascal
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_MICA_EFFECT = 1029;

procedure TFormMain.ActiverEffetMica;
var
  UseMica: Integer;
  UseDarkMode: Integer;
begin
  // Activer le mode sombre si nécessaire
  if Pos('Dark', TStyleManager.ActiveStyle.Name) > 0 then
  begin
    UseDarkMode := 1;
    DwmSetWindowAttribute(Handle, DWMWA_USE_IMMERSIVE_DARK_MODE,
      @UseDarkMode, SizeOf(UseDarkMode));
  end;

  // Activer l'effet Mica
  UseMica := 1;
  DwmSetWindowAttribute(Handle, DWMWA_MICA_EFFECT,
    @UseMica, SizeOf(UseMica));
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Vérifier Windows 11
  if TOSVersion.Major >= 10 then
    ActiverEffetMica;
end;
```

---

## 4.13.4 Icônes et ressources modernes

### Icônes Fluent Design

Windows 11 utilise les icônes Fluent Design avec un style épuré et moderne.

**Ressources pour icônes :**
```
Microsoft Fluent UI System Icons (gratuit)
https://github.com/microsoft/fluentui-system-icons

Caractéristiques :
- Format SVG et PNG
- Plusieurs tailles (16, 20, 24, 48 pixels)
- Thème clair et sombre
- Style cohérent avec Windows 11
```

**Intégration dans Delphi :**

```pascal
procedure TFormMain.ChargerIconesModernes;
begin
  // Configurer les ImageList pour différents DPI
  ImageList16.Width := ScaleValue(16);
  ImageList16.Height := ScaleValue(16);

  ImageList24.Width := ScaleValue(24);
  ImageList24.Height := ScaleValue(24);

  // Charger les icônes appropriées
  if Screen.PixelsPerInch >= 192 then
  begin
    // DPI élevé : icônes 48px
    ImageList24.LoadFromFile('icones_fluent_48.png');
  end
  else if Screen.PixelsPerInch >= 144 then
  begin
    // DPI moyen : icônes 32px
    ImageList24.LoadFromFile('icones_fluent_32.png');
  end
  else
  begin
    // DPI standard : icônes 24px
    ImageList24.LoadFromFile('icones_fluent_24.png');
  end;

  // Associer aux contrôles
  ToolBar1.Images := ImageList24;
  MainMenu1.Images := ImageList16;
end;
```

### Icône d'application moderne

```pascal
// Créer une icône moderne pour votre application
// Format : ICO avec plusieurs résolutions
// Tailles recommandées : 16, 32, 48, 256 pixels

// Dans le fichier .dpr ou .dproj
{$R *.res}
{$R 'icones_modernes.res' 'icones_modernes.rc'}

// Le fichier .rc contient :
// MAINICON ICON "app_icon.ico"
```

**Recommandations pour l'icône :**
- Design simple et reconnaissable
- Contours nets
- Bonne visibilité à petite taille
- Cohérent avec l'identité de l'application
- Format PNG 256×256 pour le Microsoft Store

---

## 4.13.5 Barre de titre personnalisée

### Étendre le contenu dans la barre de titre

Windows 11 permet d'étendre l'interface dans la zone de titre.

```pascal
type
  TFormMain = class(TForm)
  private
    procedure EtendreBarreTitre;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  end;

procedure TFormMain.EtendreBarreTitre;
const
  DWMWA_EXTENDED_FRAME_BOUNDS = 9;
var
  Marges: TMargins;
begin
  // Étendre le cadre client dans la zone non-client
  Marges.cyTopHeight := 32; // Hauteur de la barre de titre personnalisée
  Marges.cxLeftWidth := 0;
  Marges.cxRightWidth := 0;
  Marges.cyBottomHeight := 0;

  DwmExtendFrameIntoClientArea(Handle, @Marges);
end;

procedure TFormMain.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  // Ne pas réduire la zone client
  if Message.CalcValidRects then
  begin
    // Laisser la zone client occuper toute la fenêtre
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TFormMain.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;

  P := ScreenToClient(Point(Message.XPos, Message.YPos));

  // Zone de titre personnalisée (haut de la fenêtre)
  if (P.Y >= 0) and (P.Y < 32) then
  begin
    // Zone de déplacement
    if (P.X > 150) and (P.X < ClientWidth - 150) then
      Message.Result := HTCAPTION
    else
      Message.Result := HTCLIENT;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Supprimer la bordure standard
  BorderStyle := bsNone;

  // Étendre la barre de titre
  EtendreBarreTitre;

  // Ajouter un panel en haut pour la barre de titre personnalisée
  PanelTitre := TPanel.Create(Self);
  PanelTitre.Parent := Self;
  PanelTitre.Align := alTop;
  PanelTitre.Height := 32;
  PanelTitre.BevelOuter := bvNone;

  // Ajouter les boutons de contrôle
  AjouterBoutonsControle;
end;
```

### Boutons de fenêtre personnalisés

```pascal
procedure TFormMain.AjouterBoutonsControle;
var
  BtnFermer, BtnMaximiser, BtnMinimiser: TButton;
begin
  // Bouton fermer
  BtnFermer := TButton.Create(Self);
  BtnFermer.Parent := PanelTitre;
  BtnFermer.Width := 48;
  BtnFermer.Height := 32;
  BtnFermer.Left := PanelTitre.Width - 48;
  BtnFermer.Top := 0;
  BtnFermer.Anchors := [akTop, akRight];
  BtnFermer.Caption := '✕';
  BtnFermer.Flat := True;
  BtnFermer.OnClick := BtnFermerClick;

  // Bouton maximiser
  BtnMaximiser := TButton.Create(Self);
  BtnMaximiser.Parent := PanelTitre;
  BtnMaximiser.Width := 48;
  BtnMaximiser.Height := 32;
  BtnMaximiser.Left := PanelTitre.Width - 96;
  BtnMaximiser.Top := 0;
  BtnMaximiser.Anchors := [akTop, akRight];
  BtnMaximiser.Caption := '□';
  BtnMaximiser.Flat := True;
  BtnMaximiser.OnClick := BtnMaximiserClick;

  // Bouton minimiser
  BtnMinimiser := TButton.Create(Self);
  BtnMinimiser.Parent := PanelTitre;
  BtnMinimiser.Width := 48;
  BtnMinimiser.Height := 32;
  BtnMinimiser.Left := PanelTitre.Width - 144;
  BtnMinimiser.Top := 0;
  BtnMinimiser.Anchors := [akTop, akRight];
  BtnMinimiser.Caption := '─';
  BtnMinimiser.Flat := True;
  BtnMinimiser.OnClick := BtnMinimiserClick;
end;

procedure TFormMain.BtnFermerClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.BtnMaximiserClick(Sender: TObject);
begin
  if WindowState = wsMaximized then
    WindowState := wsNormal
  else
    WindowState := wsMaximized;
end;

procedure TFormMain.BtnMinimiserClick(Sender: TObject);
begin
  WindowState := wsMinimized;
end;
```

---

## 4.13.6 Animations et transitions

### Animations fluides

Windows 11 met l'accent sur les animations douces et naturelles.

```pascal
uses
  Vcl.ExtCtrls, System.Math;

type
  TAnimationType = (atFadeIn, atFadeOut, atSlideIn, atSlideOut);

procedure AnimerComposant(Composant: TControl; AnimType: TAnimationType;
  DureeMs: Integer = 200);
var
  Timer: TTimer;
  Debut: TDateTime;
  OpaciteDepart, OpaciteFin: Byte;
  PosDepart, PosFin: Integer;
begin
  Debut := Now;

  case AnimType of
    atFadeIn:
      begin
        OpaciteDepart := 0;
        OpaciteFin := 255;
        Composant.Visible := True;
      end;
    atFadeOut:
      begin
        OpaciteDepart := 255;
        OpaciteFin := 0;
      end;
    atSlideIn:
      begin
        PosDepart := -Composant.Width;
        PosFin := Composant.Left;
        Composant.Left := PosDepart;
        Composant.Visible := True;
      end;
    atSlideOut:
      begin
        PosDepart := Composant.Left;
        PosFin := -Composant.Width;
      end;
  end;

  Timer := TTimer.Create(nil);
  Timer.Interval := 16; // ~60 FPS
  Timer.OnTimer := procedure(Sender: TObject)
    var
      Progression: Double;
      Easing: Double;
    begin
      // Calculer la progression (0.0 à 1.0)
      Progression := Min(1.0, MilliSecondsBetween(Now, Debut) / DureeMs);

      // Appliquer un easing (courbe d'accélération)
      Easing := 1 - Power(1 - Progression, 3); // Ease out cubic

      case AnimType of
        atFadeIn, atFadeOut:
          begin
            Composant.AlphaBlend := True;
            Composant.AlphaBlendValue := Round(OpaciteDepart +
              (OpaciteFin - OpaciteDepart) * Easing);
          end;
        atSlideIn, atSlideOut:
          begin
            Composant.Left := Round(PosDepart +
              (PosFin - PosDepart) * Easing);
          end;
      end;

      // Terminer l'animation
      if Progression >= 1.0 then
      begin
        Timer.Enabled := False;
        Timer.Free;

        if AnimType = atFadeOut then
          Composant.Visible := False;
      end;
    end;

  Timer.Enabled := True;
end;

// Utilisation
procedure TFormMain.ButtonMontrerPanelClick(Sender: TObject);
begin
  AnimerComposant(Panel1, atFadeIn, 300);
end;

procedure TFormMain.ButtonCacherPanelClick(Sender: TObject);
begin
  AnimerComposant(Panel1, atFadeOut, 300);
end;
```

### Transition entre fenêtres

```pascal
procedure TFormMain.OuvrirFormulaireAvecAnimation(FormClass: TFormClass);
var
  Form: TForm;
begin
  Form := FormClass.Create(Application);
  try
    // Position initiale hors écran
    Form.Position := poDesigned;
    Form.Left := Screen.Width;
    Form.AlphaBlend := True;
    Form.AlphaBlendValue := 0;

    Form.Show;

    // Animer l'entrée
    TThread.CreateAnonymousThread(
      procedure
      var
        i: Integer;
      begin
        for i := 0 to 20 do
        begin
          TThread.Synchronize(nil,
            procedure
            var
              Progress: Double;
            begin
              Progress := i / 20;
              Form.Left := Screen.Width - Round(Screen.Width * Progress * 0.3);
              Form.AlphaBlendValue := Round(255 * Progress);
            end
          );
          Sleep(15);
        end;
      end
    ).Start;
  except
    Form.Free;
    raise;
  end;
end;
```

---

## 4.13.7 Notifications et toasts

### Notifications Windows 11

```pascal
uses
  Winapi.ShellAPI, System.SysUtils;

type
  TNotification = class
  public
    class procedure Afficher(const Titre, Message: string;
      IconeType: Integer = NIIF_INFO);
  end;

class procedure TNotification.Afficher(const Titre, Message: string;
  IconeType: Integer);
var
  NotifyIconData: TNotifyIconData;
begin
  FillChar(NotifyIconData, SizeOf(NotifyIconData), 0);
  NotifyIconData.cbSize := SizeOf(TNotifyIconData);
  NotifyIconData.Wnd := Application.MainForm.Handle;
  NotifyIconData.uID := 1;
  NotifyIconData.uFlags := NIF_INFO;
  NotifyIconData.dwInfoFlags := IconeType;

  StrPLCopy(NotifyIconData.szInfoTitle, Titre,
    Length(NotifyIconData.szInfoTitle) - 1);
  StrPLCopy(NotifyIconData.szInfo, Message,
    Length(NotifyIconData.szInfo) - 1);

  Shell_NotifyIcon(NIM_ADD, @NotifyIconData);
  Shell_NotifyIcon(NIM_MODIFY, @NotifyIconData);
end;

// Utilisation
procedure TFormMain.ButtonNotifierClick(Sender: TObject);
begin
  TNotification.Afficher(
    'Mise à jour disponible',
    'Une nouvelle version de l''application est disponible.',
    NIIF_INFO
  );
end;
```

### Centre de notifications Windows 11

```pascal
// Pour des notifications plus riches, utilisez l'API WinRT
// Nécessite des unités supplémentaires (WinRT, etc.)

uses
  Winapi.Windows, Winapi.WinRT;

procedure EnvoyerToastNotification(const Titre, Corps: string);
var
  ToastXml: string;
begin
  // XML pour toast moderne Windows 11
  ToastXml := Format(
    '<toast>' +
    '<visual>' +
    '<binding template="ToastGeneric">' +
    '<text>%s</text>' +
    '<text>%s</text>' +
    '</binding>' +
    '</visual>' +
    '<audio src="ms-winsoundevent:Notification.Default"/>' +
    '</toast>',
    [Titre, Corps]
  );

  // Envoyer via l'API WinRT
  // (Code simplifié - nécessite plus d'implémentation)
end;
```

---

## 4.13.8 Gestion du mode tablette

### Détecter le mode tablette

```pascal
uses
  Winapi.Windows, System.Win.Registry;

function EstEnModeTablette: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(
      'SOFTWARE\Microsoft\Windows\CurrentVersion\ImmersiveShell') then
    begin
      if Reg.ValueExists('TabletMode') then
        Result := Reg.ReadInteger('TabletMode') = 1;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.AdapterPourModeTablette;
begin
  if EstEnModeTablette then
  begin
    // Interface adaptée au tactile

    // Augmenter la taille des boutons
    Button1.Height := ScaleValue(48); // Minimum pour tactile
    Button1.Width := ScaleValue(120);

    // Augmenter l'espacement
    Panel1.Padding.SetBounds(16, 16, 16, 16);

    // Activer le mode plein écran
    WindowState := wsMaximized;

    // Désactiver certaines fonctionnalités
    MainMenu1.Visible := False; // Masquer le menu
  end
  else
  begin
    // Interface desktop standard
    Button1.Height := ScaleValue(32);
    Button1.Width := ScaleValue(100);
    Panel1.Padding.SetBounds(8, 8, 8, 8);
    MainMenu1.Visible := True;
  end;
end;
```

### Gestes tactiles

```pascal
type
  TFormMain = class(TForm)
  private
    FGestureStartX, FGestureStartY: Integer;
    procedure WMGesture(var Message: TMessage); message WM_GESTURE;
  end;

procedure TFormMain.WMGesture(var Message: TMessage);
const
  GID_BEGIN = 1;
  GID_END = 2;
  GID_ZOOM = 3;
  GID_PAN = 4;
  GID_ROTATE = 5;
var
  GestureInfo: TGestureInfo;
begin
  if GetGestureInfo(Message.LParam, GestureInfo) then
  begin
    case GestureInfo.dwID of
      GID_BEGIN:
        begin
          FGestureStartX := GestureInfo.ptsLocation.X;
          FGestureStartY := GestureInfo.ptsLocation.Y;
        end;

      GID_PAN:
        begin
          // Gérer le défilement par glissement
          ScrollBy(
            FGestureStartX - GestureInfo.ptsLocation.X,
            FGestureStartY - GestureInfo.ptsLocation.Y
          );
          FGestureStartX := GestureInfo.ptsLocation.X;
          FGestureStartY := GestureInfo.ptsLocation.Y;
        end;

      GID_ZOOM:
        begin
          // Gérer le zoom par pincement
          // (Implémentation selon vos besoins)
        end;
    end;

    CloseGestureInfoHandle(Message.LParam);
  end;

  inherited;
end;
```

---

## 4.13.9 Support du Microsoft Store

### Préparer l'application pour le Store

**1. Créer un package MSIX**

```
Le format MSIX est requis pour le Microsoft Store

Avantages :
- Installation et désinstallation propres
- Mises à jour automatiques
- Sandboxing pour la sécurité
- Distribution via le Store
```

**2. Manifeste MSIX (AppxManifest.xml)**

```xml
<?xml version="1.0" encoding="utf-8"?>
<Package xmlns="http://schemas.microsoft.com/appx/manifest/foundation/windows10"
         xmlns:uap="http://schemas.microsoft.com/appx/manifest/uap/windows10"
         xmlns:rescap="http://schemas.microsoft.com/appx/manifest/foundation/windows10/restrictedcapabilities">

  <Identity Name="VotreCompagnie.VotreApp"
            Publisher="CN=VotreCompagnie"
            Version="1.0.0.0" />

  <Properties>
    <DisplayName>Votre Application</DisplayName>
    <PublisherDisplayName>Votre Compagnie</PublisherDisplayName>
    <Logo>Assets\StoreLogo.png</Logo>
  </Properties>

  <Dependencies>
    <TargetDeviceFamily Name="Windows.Desktop"
                       MinVersion="10.0.22000.0"
                       MaxVersionTested="10.0.22621.0" />
  </Dependencies>

  <Resources>
    <Resource Language="fr-FR"/>
    <Resource Language="en-US"/>
  </Resources>

  <Applications>
    <Application Id="App"
                 Executable="VotreApp.exe"
                 EntryPoint="Windows.FullTrustApplication">

      <uap:VisualElements DisplayName="Votre Application"
                         Description="Description de votre application"
                         BackgroundColor="transparent"
                         Square150x150Logo="Assets\Square150x150Logo.png"
                         Square44x44Logo="Assets\Square44x44Logo.png">

        <uap:DefaultTile Wide310x150Logo="Assets\Wide310x150Logo.png"
                        Square310x310Logo="Assets\LargeTile.png"
                        Square71x71Logo="Assets\SmallTile.png">
        </uap:DefaultTile>

        <uap:SplashScreen Image="Assets\SplashScreen.png" />
      </uap:VisualElements>
    </Application>
  </Applications>

  <Capabilities>
    <rescap:Capability Name="runFullTrust" />
  </Capabilities>
</Package>
```

**3. Ressources graphiques requises**

```
Assets à fournir (format PNG avec transparence) :

Square44x44Logo.png        (44 × 44 px)   - Petite icône
Square150x150Logo.png      (150 × 150 px) - Tuile moyenne
Wide310x150Logo.png        (310 × 150 px) - Tuile large
Square310x310Logo.png      (310 × 310 px) - Tuile grande
StoreLogo.png              (50 × 50 px)   - Store
SplashScreen.png           (620 × 300 px) - Écran de démarrage

Échelles supplémentaires à 125%, 150%, 200%, 400%
```

### Créer le package avec MSIX Packaging Tool

```pascal
// Configuration de l'application pour MSIX

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Détecter si l'application s'exécute dans un package MSIX
  if IsAppPackaged then
  begin
    // Comportements spécifiques au Store
    // - Pas d'élévation de privilèges
    // - Fichiers dans des dossiers limités
    // - Utiliser les API modernes

    ConfigurerPourStore;
  end;
end;

function IsAppPackaged: Boolean;
var
  PackageFullName: array[0..MAX_PATH] of Char;
  Length: UINT32;
begin
  Length := MAX_PATH;
  Result := GetCurrentPackageFullName(@Length, @PackageFullName) = ERROR_SUCCESS;
end;

procedure TFormMain.ConfigurerPourStore;
begin
  // Utiliser le dossier local de l'application
  CheminDonnees := TPath.Combine(
    TPath.GetHomePath,
    'AppData\Local\Packages\VotrePackageID\LocalState'
  );

  // Ne pas essayer d'écrire dans Program Files
  // Ne pas créer de clés de registre système
  // Utiliser les API modernes pour les fichiers
end;
```

---

## 4.13.10 Checklist de modernisation

### Checklist complète

```
□ APPARENCE
  □ Style Windows 11 appliqué (Light/Dark)
  □ Coins arrondis activés
  □ Icônes Fluent Design
  □ Police Segoe UI Variable (si disponible)
  □ Espacement généreux (marges 8-16px)
  □ Ombres subtiles sur éléments flottants

□ FONCTIONNALITÉS
  □ Support du thème système (auto Light/Dark)
  □ Surveillance des changements de thème
  □ DPI awareness (PerMonitorV2)
  □ Support multi-écrans
  □ Animations fluides (200-300ms)

□ INTERFACE
  □ Barre de titre moderne
  □ Menu Snap Layouts compatible
  □ Notifications Windows 11
  □ Support mode tablette
  □ Gestes tactiles

□ TECHNIQUE
  □ Manifeste à jour (Windows 11 compatible)
  □ Pas d'API dépréciées
  □ Support 64-bit
  □ Certificat de signature de code

□ ACCESSIBILITÉ
  □ Navigation au clavier complète
  □ Contrastes suffisants
  □ Hints sur tous les boutons
  □ Support lecteurs d'écran

□ PERFORMANCE
  □ Démarrage < 2 secondes
  □ Interface réactive (< 100ms)
  □ Consommation mémoire optimisée
  □ Pas de blocage du thread UI

□ DISTRIBUTION
  □ Package MSIX créé (si Store)
  □ Installateur classique disponible
  □ Documentation utilisateur
  □ Notes de version
```

---

## 4.13.11 Exemple complet : Application modernisée

```pascal
unit FormModerne;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Imaging.pngimage, Vcl.Themes, System.Win.Registry;

type
  TFormMain = class(TForm)
    PanelTitre: TPanel;
    LabelTitre: TLabel;
    PanelContenu: TPanel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FThemeSysteme: string;
    procedure ModerniserInterface;
    procedure AppliquerStyleWindows11;
    procedure ActiverCoinsArrondis;
    procedure ActiverEffetMica;
    function GetThemeSysteme: string;
    procedure AppliquerCouleurAccent;
    procedure CreerBoutons Controls;
    procedure WMSettingChange(var Message: TWMSettingChange);
      message WM_SETTINGCHANGE;
    procedure WMDwmColorizationColorChanged(var Message: TMessage);
      message WM_DWMCOLORIZATIONCOLORCHANGED;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  DWMWA_WINDOW_CORNER_PREFERENCE = 33;
  DWMWA_MICA_EFFECT = 1029;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_BORDER_COLOR = 34;
  DWMWA_CAPTION_COLOR = 35;
  DWMWCP_ROUND = 2;

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD;
  pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  external 'dwmapi.dll';

function DwmExtendFrameIntoClientArea(hwnd: HWND; const pMarInset: PMargins): HRESULT; stdcall;
  external 'dwmapi.dll';

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Configuration de base
  Scaled := True;
  AutoScroll := True;
  DoubleBuffered := True;

  // Appliquer les modernisations
  ModerniserInterface;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  // Appliquer après que la fenêtre soit créée
  if (TOSVersion.Major >= 10) and (TOSVersion.Build >= 22000) then
  begin
    ActiverCoinsArrondis;
    ActiverEffetMica;
    AppliquerCouleurAccent;
  end;
end;

procedure TFormMain.ModerniserInterface;
begin
  // 1. Appliquer le style Windows 11
  AppliquerStyleWindows11;

  // 2. Configurer la police
  Font.Name := 'Segoe UI';
  Font.Size := 9;

  // 3. Configurer les couleurs et espacements
  PanelTitre.Height := ScaleValue(48);
  PanelTitre.Padding.SetBounds(
    ScaleValue(16),
    ScaleValue(12),
    ScaleValue(16),
    ScaleValue(12)
  );

  PanelContenu.Padding.SetBounds(
    ScaleValue(24),
    ScaleValue(24),
    ScaleValue(24),
    ScaleValue(24)
  );

  // 4. Configurer le titre
  LabelTitre.Font.Size := 14;
  LabelTitre.Font.Style := [fsBold];
  LabelTitre.Align := alClient;
  LabelTitre.Alignment := taCenter;
  LabelTitre.Layout := tlCenter;

  // 5. Créer les boutons de contrôle
  CreerBoutonsControles;

  // 6. Configurer la barre d'état
  StatusBar1.SimplePanel := True;
  StatusBar1.Height := ScaleValue(24);
end;

procedure TFormMain.AppliquerStyleWindows11;
begin
  FThemeSysteme := GetThemeSysteme;

  if TStyleManager.IsValidStyle(FThemeSysteme) then
    TStyleManager.SetStyle(FThemeSysteme);
end;

function TFormMain.GetThemeSysteme: string;
var
  Reg: TRegistry;
  UseLightTheme: Integer;
begin
  Result := 'Windows11 Modern Light'; // Par défaut

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(
      'SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
      begin
        UseLightTheme := Reg.ReadInteger('AppsUseLightTheme');
        if UseLightTheme = 0 then
          Result := 'Windows11 Modern Dark'
        else
          Result := 'Windows11 Modern Light';
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.ActiverCoinsArrondis;
var
  Preference: Integer;
begin
  Preference := DWMWCP_ROUND;
  DwmSetWindowAttribute(Handle, DWMWA_WINDOW_CORNER_PREFERENCE,
    @Preference, SizeOf(Preference));
end;

procedure TFormMain.ActiverEffetMica;
var
  UseMica: Integer;
  UseDarkMode: Integer;
begin
  // Mode sombre si nécessaire
  if Pos('Dark', FThemeSysteme) > 0 then
  begin
    UseDarkMode := 1;
    DwmSetWindowAttribute(Handle, DWMWA_USE_IMMERSIVE_DARK_MODE,
      @UseDarkMode, SizeOf(UseDarkMode));
  end;

  // Effet Mica
  UseMica := 1;
  DwmSetWindowAttribute(Handle, DWMWA_MICA_EFFECT,
    @UseMica, SizeOf(UseMica));
end;

procedure TFormMain.AppliquerCouleurAccent;
var
  Reg: TRegistry;
  CouleurAccent: DWORD;
  CouleurBordure: COLORREF;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(
      'SOFTWARE\Microsoft\Windows\DWM') then
    begin
      if Reg.ValueExists('AccentColor') then
      begin
        CouleurAccent := Reg.ReadInteger('AccentColor');

        // Convertir et appliquer
        CouleurBordure := RGB(
          (CouleurAccent and $FF),
          ((CouleurAccent shr 8) and $FF),
          ((CouleurAccent shr 16) and $FF)
        );

        DwmSetWindowAttribute(Handle, DWMWA_BORDER_COLOR,
          @CouleurBordure, SizeOf(CouleurBordure));
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.CreerBoutonsControles;
var
  BtnFermer, BtnMaximiser, BtnMinimiser: TButton;
begin
  // Créer les boutons de contrôle Windows 11 style
  // (Implémentation simplifiée - voir section 4.13.5 pour détails)

  BtnFermer := TButton.Create(Self);
  BtnFermer.Parent := PanelTitre;
  BtnFermer.Align := alRight;
  BtnFermer.Width := ScaleValue(48);
  BtnFermer.Caption := '✕';
  BtnFermer.Flat := True;
  BtnFermer.OnClick := procedure(Sender: TObject)
    begin
      Close;
    end;
end;

procedure TFormMain.WMSettingChange(var Message: TWMSettingChange);
var
  Section: string;
begin
  inherited;

  Section := string(Message.Section);
  if (Section = 'ImmersiveColorSet') then
  begin
    // Le thème a changé
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(500);
        TThread.Synchronize(nil,
          procedure
          begin
            AppliquerStyleWindows11;
          end
        );
      end
    ).Start;
  end;
end;

procedure TFormMain.WMDwmColorizationColorChanged(var Message: TMessage);
begin
  inherited;

  // La couleur d'accent a changé
  if (TOSVersion.Major >= 10) and (TOSVersion.Build >= 22000) then
    AppliquerCouleurAccent;
end;

end.
```

---

## Conclusion

La modernisation de vos applications VCL pour Windows 11 n'est pas qu'une question d'esthétique, c'est une nécessité pour rester compétitif et offrir la meilleure expérience utilisateur possible.

### Points clés à retenir

✅ **Styles Windows 11** : Utilisez les styles intégrés de Delphi 13
✅ **Coins arrondis** : Activez-les via l'API DWM
✅ **Thème système** : Détectez et suivez les préférences utilisateur
✅ **Icônes modernes** : Adoptez Fluent Design
✅ **Animations** : Rendez l'interface plus vivante
✅ **Support tactile** : Pensez aux appareils hybrides
✅ **Microsoft Store** : Préparez un package MSIX
✅ **Tests** : Vérifiez sur Windows 11 réel

### Bénéfices de la modernisation

🎨 **Apparence** : Interface élégante et contemporaine
⚡ **Performance** : Animations fluides et réactivité
📱 **Versatilité** : Fonctionne en mode desktop et tablette
🏪 **Distribution** : Présence sur le Microsoft Store possible
👥 **Satisfaction** : Utilisateurs plus heureux
🚀 **Futur** : Application prête pour les évolutions

Avec Delphi 13 Florence et ces techniques, vos applications VCL seront parfaitement intégrées à Windows 11 et offriront une expérience utilisateur moderne et professionnelle ! 🚀

⏭️ [Améliorations VCL de Delphi 13](/04-conception-dinterfaces-utilisateur-avec-la-vcl/14-ameliorations-vcl-delphi-13.md)
