# 6.6 Navigation dans les applications mobiles

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La navigation dans les applications mobiles fonctionne différemment des applications de bureau traditionnelles. Dans ce chapitre, nous allons explorer les spécificités de la navigation mobile avec Delphi et FireMonkey (FMX), en nous concentrant sur des concepts accessibles aux débutants.

## Comprendre la navigation mobile

Les applications mobiles ont généralement une interface à écran unique, où l'utilisateur navigue d'un écran à un autre, contrairement aux multiples fenêtres des applications de bureau. Cette approche est optimisée pour les écrans tactiles et les appareils à surface d'affichage limitée.

### Modèles de navigation mobile courants

1. **Navigation hiérarchique** : Organisation des écrans en arborescence
2. **Navigation par onglets** : Changement d'écran par des onglets en bas ou en haut
3. **Navigation latérale** : Menu coulissant (drawer/hamburger menu)
4. **Navigation modale** : Écrans qui apparaissent temporairement au-dessus des autres

## Préparation d'un projet mobile avec Delphi

Avant de commencer à implémenter la navigation, créons un projet mobile basique.

### Création d'un projet FireMonkey mobile

1. Lancez Delphi et sélectionnez **Fichier → Nouveau → Application multi-périphériques**
2. Choisissez **Blank Application** comme modèle
3. Dans l'**Object Inspector**, configurez le formulaire principal :
   - Définissez `Caption` à "Mon Application Mobile"
   - Ajustez `Fill.Color` pour un fond agréable (par exemple, un bleu clair)

### Ajout de styles mobile

Pour donner à votre application un aspect natif :

1. Cliquez sur **Project → Options**
2. Sélectionnez **Application → Appearance**
3. Activez l'option **Apply style** et choisissez un style correspondant à la plateforme cible (par exemple "Glacier" pour iOS ou "Material" pour Android)

```pascal
// Vous pouvez aussi définir le style par programmation
uses
  FMX.Styles;

// Dans votre FormCreate
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TStyleManager.TrySetStyleFromFile('Material.style');
end;
```

## Méthodes de navigation dans FireMonkey

### 1. Navigation entre formulaires

La méthode la plus simple consiste à utiliser plusieurs formulaires et à passer de l'un à l'autre.

#### Création des formulaires

1. Créez un nouveau formulaire : **Fichier → Nouveau → Multi-périphériques → Form**
2. Personnalisez-le et ajoutez des éléments d'interface
3. Répétez pour créer autant de formulaires que nécessaire

#### Navigation basique entre formulaires

```pascal
// Dans votre unité principale (par exemple, MainForm.pas)
uses
  SecondForm; // Assurez-vous d'inclure l'unité du second formulaire

procedure TMainForm.btnNavigateClick(Sender: TObject);
var
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Application);
  Form2.Show;
  // Attention : ceci n'est pas idéal pour les applications mobiles
  // car l'ancien formulaire reste en mémoire
end;
```

#### Gestion appropriée des formulaires pour mobile

Sur mobile, il est préférable de libérer le formulaire précédent ou d'utiliser d'autres approches :

```pascal
procedure TMainForm.btnNavigateClick(Sender: TObject);
var
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Application);
  // Passer des données si nécessaire
  Form2.UserName := edtUserName.Text;

  // Afficher le nouveau formulaire
  Form2.Show;

  // Fermer le formulaire actuel
  Close;
end;

// Dans le second formulaire, ajouter un bouton retour
procedure TForm2.btnBackClick(Sender: TObject);
var
  Form1: TForm1;
begin
  Form1 := TForm1.Create(Application);
  Form1.Show;
  Close; // Fermer ce formulaire
end;
```

**Problèmes avec cette approche** :
- Perte de l'état des formulaires lors de la navigation
- Utilisation inefficace de la mémoire
- Non-respect des conventions de navigation mobile

### 2. Utilisation de TTabControl pour la navigation

Une meilleure approche consiste à utiliser un `TTabControl` et à changer de "page" plutôt que de changer de formulaire.

#### Configuration du TTabControl

1. Glissez un `TTabControl` depuis la palette de composants sur votre formulaire
2. Définissez son alignement à `Client` pour qu'il occupe tout l'espace
3. Ajoutez plusieurs `TTabItem` (onglets) pour représenter vos différents écrans
4. Placez les contrôles appropriés sur chaque onglet

```pascal
// Dans le concepteur, ajoutez un TTabControl avec les TabItems :
// - tabHome
// - tabProfile
// - tabSettings

// Dans le code, masquez les onglets pour une navigation mobile native
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Masquer les contrôles d'onglets (important pour une UI mobile)
  TabControl1.TabPosition := TTabPosition.None;

  // Démarrer sur la page d'accueil
  TabControl1.ActiveTab := tabHome;
end;
```

#### Navigation entre les onglets

```pascal
// Bouton pour aller au profil
procedure TMainForm.btnGoToProfileClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabProfile;
end;

// Bouton pour aller aux paramètres
procedure TMainForm.btnGoToSettingsClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabSettings;
end;

// Bouton pour revenir à l'accueil
procedure TMainForm.btnBackToHomeClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabHome;
end;
```

#### Ajout d'une transition animée

FireMonkey permet d'ajouter facilement des animations de transition :

```pascal
procedure TMainForm.btnGoToProfileClick(Sender: TObject);
begin
  TabControl1.SetActiveTabWithTransition(tabProfile, TTabTransition.Slide,
                                         TTabTransitionDirection.Normal, 0.3);
end;
```

Les types de transitions disponibles sont :
- `TTabTransition.Slide` : Glissement
- `TTabTransition.Fade` : Fondu
- `TTabTransition.None` : Aucune transition

Les directions incluent :
- `TTabTransitionDirection.Normal` : Direction par défaut (droite à gauche)
- `TTabTransitionDirection.Reversed` : Direction inverse (gauche à droite)

### 3. Navigation par onglets inférieure (style natif)

Pour créer une barre d'onglets en bas comme dans de nombreuses applications mobiles :

#### Configuration de la structure

1. Utilisez un `TLayout` aligné en bas pour la barre d'onglets
2. Utilisez un `TTabControl` pour le contenu principal, aligné `Client`
3. Placez des boutons dans le `TLayout` pour représenter les onglets

```pascal
// Dans le concepteur :
// - TTabControl (aligné Client) avec plusieurs TabItems
// - TLayout (aligné Bottom, hauteur ~60) contenant plusieurs TSpeedButton

// Dans le code :
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Masquer les contrôles d'onglets
  TabControl1.TabPosition := TTabPosition.None;

  // Style initial des boutons d'onglets
  UpdateTabButtons(btnHome);
end;

// Gestionnaire de clic pour tous les boutons d'onglets
procedure TMainForm.TabButtonClick(Sender: TObject);
begin
  // Mettre à jour l'apparence des boutons
  UpdateTabButtons(TSpeedButton(Sender));

  // Changer l'onglet actif
  if Sender = btnHome then
    TabControl1.ActiveTab := tabHome
  else if Sender = btnProfile then
    TabControl1.ActiveTab := tabProfile
  else if Sender = btnSettings then
    TabControl1.ActiveTab := tabSettings;
end;

// Mise à jour de l'apparence des boutons
procedure TMainForm.UpdateTabButtons(ActiveButton: TSpeedButton);
begin
  // Réinitialiser tous les boutons
  btnHome.Opacity := 0.6;
  btnProfile.Opacity := 0.6;
  btnSettings.Opacity := 0.6;

  // Mettre en évidence le bouton actif
  ActiveButton.Opacity := 1.0;
end;
```

### 4. Navigation avec menu latéral (Drawer)

Le menu latéral (aussi appelé "drawer" ou menu hamburger) est très courant dans les applications mobiles.

#### Création d'un menu latéral simple

1. Utilisez un `TMultiView` depuis la palette de composants
2. Configurer le `TMultiView` :
   - `DrawerOptions.Mode` = `TMultiViewMode.Popover`
   - `DrawerOptions.Width` = `250` (ou selon vos besoins)
   - `MasterButton` = un bouton avec l'icône hamburger

```pascal
// Dans l'événement OnClick du bouton de menu
procedure TMainForm.btnMenuClick(Sender: TObject);
begin
  // Ouvrir/fermer le menu latéral
  MultiView1.ShowMaster;
end;

// Dans le menu, ajouter des éléments comme des TListBoxItem
procedure TMainForm.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Navigation basée sur l'élément sélectionné
  if Item = lbiHome then
    TabControl1.ActiveTab := tabHome
  else if Item = lbiProfile then
    TabControl1.ActiveTab := tabProfile
  else if Item = lbiSettings then
    TabControl1.ActiveTab := tabSettings;

  // Fermer le menu après la sélection
  MultiView1.HideMaster;
end;
```

## Exemple complet : Application mobile avec navigation combinée

Maintenant, créons une application complète combinant plusieurs techniques de navigation.

### Structure de l'application

- **Menu latéral** : Pour la navigation principale entre sections
- **Onglets inférieurs** : Pour la navigation dans une section
- **Navigation entre formulaires** : Pour des écrans spécifiques (comme un écran de détails)

### Code de l'interface principale

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.MultiView, FMX.Layouts, FMX.ListBox,
  FMX.TabControl, FMX.Objects;

type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    btnMenu: TSpeedButton;
    lblTitle: TLabel;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    lbiHome: TListBoxItem;
    lbiExplore: TListBoxItem;
    lbiProfile: TListBoxItem;
    lbiSettings: TListBoxItem;
    TabControl1: TTabControl;
    tabHome: TTabItem;
    tabExplore: TTabItem;
    tabProfile: TTabItem;
    tabSettings: TTabItem;
    LayoutBottomTabs: TLayout;
    btnTabHome: TSpeedButton;
    btnTabExplore: TSpeedButton;
    imgHome: TImage;
    imgExplore: TImage;
    imgProfile: TImage;
    imgSettings: TImage;
    btnTabProfile: TSpeedButton;
    btnTabSettings: TSpeedButton;
    ListViewHome: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure btnTabHomeClick(Sender: TObject);
    procedure btnTabExploreClick(Sender: TObject);
    procedure btnTabProfileClick(Sender: TObject);
    procedure btnTabSettingsClick(Sender: TObject);
    procedure btnMenuClick(Sender: TObject);
    procedure ListViewHomeItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    procedure UpdateTabButtons(ActiveButton: TSpeedButton);
    procedure UpdateTitle;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  DetailForm;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Configuration initiale
  TabControl1.TabPosition := TTabPosition.None;
  TabControl1.ActiveTab := tabHome;

  // Configuration du MultiView
  MultiView1.Mode := TMultiViewMode.Popover;
  MultiView1.HideMaster;

  // Remplir la ListView avec des données de test
  for I := 1 to 20 do
  begin
    with ListViewHome.Items.Add do
    begin
      Text := 'Élément ' + I.ToString;
      Detail := 'Description de l''élément ' + I.ToString;
    end;
  end;

  // Mettre à jour l'apparence des boutons d'onglets
  UpdateTabButtons(btnTabHome);

  // Définir le titre
  UpdateTitle;
end;

procedure TFormMain.UpdateTabButtons(ActiveButton: TSpeedButton);
begin
  // Réinitialiser tous les boutons
  btnTabHome.Opacity := 0.6;
  btnTabExplore.Opacity := 0.6;
  btnTabProfile.Opacity := 0.6;
  btnTabSettings.Opacity := 0.6;

  // Mettre en évidence le bouton actif
  ActiveButton.Opacity := 1.0;
end;

procedure TFormMain.UpdateTitle;
begin
  // Mettre à jour le titre en fonction de l'onglet actif
  if TabControl1.ActiveTab = tabHome then
    lblTitle.Text := 'Accueil'
  else if TabControl1.ActiveTab = tabExplore then
    lblTitle.Text := 'Explorer'
  else if TabControl1.ActiveTab = tabProfile then
    lblTitle.Text := 'Profil'
  else if TabControl1.ActiveTab = tabSettings then
    lblTitle.Text := 'Paramètres';
end;

procedure TFormMain.btnMenuClick(Sender: TObject);
begin
  // Ouvrir/fermer le menu latéral
  if MultiView1.IsShowed then
    MultiView1.HideMaster
  else
    MultiView1.ShowMaster;
end;

procedure TFormMain.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  // Navigation basée sur l'élément sélectionné dans le menu latéral
  if Item = lbiHome then
  begin
    TabControl1.ActiveTab := tabHome;
    UpdateTabButtons(btnTabHome);
  end
  else if Item = lbiExplore then
  begin
    TabControl1.ActiveTab := tabExplore;
    UpdateTabButtons(btnTabExplore);
  end
  else if Item = lbiProfile then
  begin
    TabControl1.ActiveTab := tabProfile;
    UpdateTabButtons(btnTabProfile);
  end
  else if Item = lbiSettings then
  begin
    TabControl1.ActiveTab := tabSettings;
    UpdateTabButtons(btnTabSettings);
  end;

  // Mettre à jour le titre
  UpdateTitle;

  // Fermer le menu après la sélection
  MultiView1.HideMaster;
end;

procedure TFormMain.btnTabHomeClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabHome;
  UpdateTabButtons(TSpeedButton(Sender));
  UpdateTitle;
end;

procedure TFormMain.btnTabExploreClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabExplore;
  UpdateTabButtons(TSpeedButton(Sender));
  UpdateTitle;
end;

procedure TFormMain.btnTabProfileClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabProfile;
  UpdateTabButtons(TSpeedButton(Sender));
  UpdateTitle;
end;

procedure TFormMain.btnTabSettingsClick(Sender: TObject);
begin
  TabControl1.ActiveTab := tabSettings;
  UpdateTabButtons(TSpeedButton(Sender));
  UpdateTitle;
end;

procedure TFormMain.ListViewHomeItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  DetailForm: TDetailForm;
begin
  // Naviguer vers le formulaire de détails
  DetailForm := TDetailForm.Create(Application);
  DetailForm.SetItemDetails(AItem.Text, AItem.Detail);
  DetailForm.Show;
end;

end.
```

### Création du formulaire de détails

Créez un nouveau formulaire (`DetailForm.pas`) pour l'écran de détails :

```pascal
unit DetailForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TDetailForm = class(TForm)
    ToolBar1: TToolBar;
    btnBack: TSpeedButton;
    lblTitle: TLabel;
    Layout1: TLayout;
    lblItemName: TLabel;
    lblItemDetail: TLabel;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FItemName: string;
    FItemDetail: string;
  public
    procedure SetItemDetails(const AName, ADetail: string);
  end;

implementation

{$R *.fmx}

procedure TDetailForm.FormCreate(Sender: TObject);
begin
  // Configurer le bouton de retour selon la plateforme
  {$IFDEF IOS}
  btnBack.StyleLookup := 'backtoolbutton';
  {$ENDIF}

  {$IFDEF ANDROID}
  btnBack.StyleLookup := 'arrowlefttoolbutton';
  {$ENDIF}

  btnBack.Text := '';
end;

procedure TDetailForm.SetItemDetails(const AName, ADetail: string);
begin
  FItemName := AName;
  FItemDetail := ADetail;

  // Mettre à jour l'interface
  lblTitle.Text := FItemName;
  lblItemName.Text := FItemName;
  lblItemDetail.Text := FItemDetail;
end;

procedure TDetailForm.btnBackClick(Sender: TObject);
begin
  // Retour au formulaire précédent
  Close;
end;

end.
```

## Gestion du bouton retour sur Android

Sur Android, il est important de gérer correctement le bouton retour matériel (ou virtuel) :

```pascal
// Ajouter à l'interface de TFormMain
procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

// Implémentation
procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  // Gestion du bouton retour sur Android
  if Key = vkHardwareBack then
  begin
    // Si le menu est ouvert, le fermer
    if MultiView1.IsShowed then
    begin
      MultiView1.HideMaster;
      Key := 0; // Empêcher l'action par défaut (quitter l'application)
    end
    // Sinon, comportement par défaut (quitter l'application)
  end;
  {$ENDIF}
end;
```

Pour l'écran de détails, il faut aussi gérer le bouton retour :

```pascal
// Dans DetailForm.pas, ajouter à l'interface
procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

// Implémentation
procedure TDetailForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  // Gestion du bouton retour sur Android
  if Key = vkHardwareBack then
  begin
    // Fermer ce formulaire
    Close;
    Key := 0; // Empêcher l'action par défaut
  end;
  {$ENDIF}
end;
```

## Navigation avec état persistant

Un défi courant dans les applications mobiles est de préserver l'état de l'interface lors de la navigation. Voici comment implémenter cette fonctionnalité :

```pascal
// Dans MainForm.pas, ajoutez ces variables privées
private
  FLastActiveTab: TTabItem;
  FScrollPositions: array[0..3] of Single;

// Dans la procédure où vous changez d'onglet, sauvegardez la position de défilement
procedure TFormMain.btnTabHomeClick(Sender: TObject);
begin
  // Sauvegarder la position de défilement de l'onglet actuel
  if TabControl1.ActiveTab = tabHome then
    FScrollPositions[0] := ListViewHome.ScrollViewPos
  else if TabControl1.ActiveTab = tabExplore then
    FScrollPositions[1] := // position du contrôle de défilement
  // etc.

  // Sauvegarder l'onglet actuel
  FLastActiveTab := TabControl1.ActiveTab;

  // Changer d'onglet
  TabControl1.ActiveTab := tabHome;

  // Restaurer la position de défilement
  ListViewHome.ScrollViewPos := FScrollPositions[0];

  // Mettre à jour l'UI
  UpdateTabButtons(TSpeedButton(Sender));
  UpdateTitle;
end;
```

## Gestion de l'orientation de l'écran

Les applications mobiles doivent souvent s'adapter à l'orientation de l'écran :

```pascal
procedure TFormMain.FormResize(Sender: TObject);
begin
  // Détecter l'orientation
  if Width > Height then
    HandleLandscapeOrientation
  else
    HandlePortraitOrientation;
end;

procedure TFormMain.HandlePortraitOrientation;
begin
  // Configuration pour l'orientation portrait
  LayoutBottomTabs.Align := TAlignLayout.Bottom;
  LayoutBottomTabs.Height := 60;

  // Ajuster les autres contrôles si nécessaire
end;

procedure TFormMain.HandleLandscapeOrientation;
begin
  // Configuration pour l'orientation paysage
  // Par exemple, déplacer les onglets sur le côté
  LayoutBottomTabs.Align := TAlignLayout.Right;
  LayoutBottomTabs.Width := 80;

  // Ajuster les autres contrôles si nécessaire
end;
```

## Navigation avec gestes (Swipe)

FireMonkey permet d'ajouter facilement une navigation par gestes :

```pascal
// Dans le concepteur, ajoutez un TGestureManager
// Dans FormCreate :
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // ... autre code d'initialisation

  // Configurer les gestes
  Touch.GestureManager := GestureManager1;
  Touch.StandardGestures := [TStandardGesture.sgLeft, TStandardGesture.sgRight];
end;

// Gérer les événements de geste
procedure TFormMain.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
begin
  case EventInfo.GestureID of
    // Glisser vers la gauche (naviguer vers la droite)
    sgiLeft:
      begin
        if TabControl1.ActiveTab = tabHome then
        begin
          TabControl1.ActiveTab := tabExplore;
          UpdateTabButtons(btnTabExplore);
          UpdateTitle;
          Handled := True;
        end
        else if TabControl1.ActiveTab = tabExplore then
        begin
          TabControl1.ActiveTab := tabProfile;
          UpdateTabButtons(btnTabProfile);
          UpdateTitle;
          Handled := True;
        end
        else if TabControl1.ActiveTab = tabProfile then
        begin
          TabControl1.ActiveTab := tabSettings;
          UpdateTabButtons(btnTabSettings);
          UpdateTitle;
          Handled := True;
        end;
      end;

    // Glisser vers la droite (naviguer vers la gauche)
    sgiRight:
      begin
        if TabControl1.ActiveTab = tabSettings then
        begin
          TabControl1.ActiveTab := tabProfile;
          UpdateTabButtons(btnTabProfile);
          UpdateTitle;
          Handled := True;
        end
        else if TabControl1.ActiveTab = tabProfile then
        begin
          TabControl1.ActiveTab := tabExplore;
          UpdateTabButtons(btnTabExplore);
          UpdateTitle;
          Handled := True;
        end
        else if TabControl1.ActiveTab = tabExplore then
        begin
          TabControl1.ActiveTab := tabHome;
          UpdateTabButtons(btnTabHome);
          UpdateTitle;
          Handled := True;
        end;
      end;
  end;
end;
```

## Navigation adaptative pour téléphones et tablettes

Pour les applications universelles, adaptez la navigation selon la taille de l'écran :

```pascal
procedure TFormMain.FormCreate(Sender: TObject);
var
  ScreenService: IFMXScreenService;
  ScreenSize: TSizeF;
begin
  // ... autre code d'initialisation

  // Détecter si c'est une tablette
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
  begin
    ScreenSize := ScreenService.GetScreenSize;
    if (ScreenSize.Width >= 600) or (ScreenSize.Height >= 600) then
      SetupTabletUI
    else
      SetupPhoneUI;
  end;
end;

procedure TFormMain.SetupPhoneUI;
begin
  // Interface pour téléphone
  // - Onglets en bas
  // - Navigation empilée
  LayoutBottomTabs.Visible := True;
  LayoutBottomTabs.Align := TAlignLayout.Bottom;
  LayoutBottomTabs.Height := 60;
end;

procedure TFormMain.SetupTabletUI;
begin
  // Interface pour tablette
  // - Menu permanent à gauche (comme sur iPad)
  // - Écran divisé en mode paysage
  LayoutBottomTabs.Visible := False;
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.DrawerOptions.Mode := TDrawerMode.PushingDetailView;
  MultiView1.DrawerOptions.PushingMode := TPushingMode.Overlap;
  MultiView1.Visible := True;

  // En mode paysage, garder le menu ouvert par défaut
  if Width > Height then
    MultiView1.ShowMaster;
end;
```

## Bonnes pratiques pour la navigation mobile

1. **Cohérence** : Suivez les conventions de la plateforme (iOS ou Android)
2. **Simplicité** : Limitez la profondeur de navigation à 3-4 niveaux maximum
3. **Retour évident** : Assurez-vous que l'utilisateur sait toujours comment revenir en arrière
4. **Feedback visuel** : Indiquez clairement où se trouve l'utilisateur dans l'application
5. **Efficacité** : Permettez d'accéder aux fonctions principales en 2-3 actions
6. **Orientation** : Testez votre application en mode portrait et paysage
7. **Performance** : Évitez de créer/détruire des formulaires en permanence
8. **Accessibilité** : Assurez-vous que les cibles tactiles sont assez grandes (≥ 44pt)

## Erreurs courantes à éviter

1. **Ignorer les conventions de plateforme** : Un menu hamburger n'est pas naturel sur iOS
2. **Navigation trop profonde** : L'utilisateur se perd après 3-4 niveaux
3. **Boutons trop petits** ou trop proches les uns des autres
4. **Manque d'indication** sur l'emplacement actuel dans l'application
5. **Oublier de gérer le bouton Retour** sur Android
6. **Navigation incohérente** entre différentes parties de l'application
7. **Non-persistance de l'état** lors de la navigation (perte des données saisies)
8. **Transitions trop lentes** qui frustrent l'utilisateur

## Exercices pratiques

1. **Exercice simple** : Créez une application avec 3 onglets et une barre de navigation inférieure
2. **Exercice intermédiaire** : Ajoutez un écran de détails accessible depuis une liste, avec navigation retour
3. **Exercice avancé** : Créez une application combinant menu latéral, onglets et adaptée au mode tablette

## Conclusion

La navigation est l'un des aspects les plus importants d'une application mobile. Bien conçue, elle permet à l'utilisateur de se déplacer intuitivement dans votre application. FireMonkey offre tous les outils nécessaires pour créer une navigation fluide et adaptée aux différentes plateformes mobiles.

En combinant les techniques vues dans ce chapitre – onglets, menu latéral, navigation hiérarchique – vous pouvez créer des applications mobiles Delphi offrant une excellente expérience utilisateur.


# 6.6 Navigation dans les applications mobiles

La navigation dans les applications mobiles diffère considérablement de celle des applications desktop. Alors que les applications desktop utilisent souvent plusieurs fenêtres et des interfaces MDI, les applications mobiles s'appuient sur un modèle de navigation plus fluide et adapté aux écrans tactiles. Ce chapitre vous guidera à travers les concepts et techniques de navigation pour les applications mobiles créées avec Delphi et FireMonkey (FMX).

## Comprendre les modèles de navigation mobile

Avant de plonger dans le code, il est important de comprendre les modèles de navigation courants dans les applications mobiles :

### 1. Navigation par pile (Stack Navigation)

C'est le modèle le plus courant :
- Les écrans sont empilés les uns sur les autres
- Chaque nouvel écran est placé au sommet de la pile
- Le bouton de retour permet de revenir à l'écran précédent (dépiler)
- Typique des applications iOS et Android

### 2. Navigation par onglets (Tab Navigation)

- Plusieurs écrans accessibles via des onglets en bas ou en haut
- Permet de basculer rapidement entre les fonctions principales
- Les onglets conservent généralement leur état entre les changements

### 3. Navigation par tiroir (Drawer Navigation)

- Menu latéral qui s'affiche en glissant depuis le bord de l'écran
- Souvent utilisé pour les options, paramètres ou sections principales
- Également appelé "Hamburger Menu" ou "Side Menu"

### 4. Navigation modale

- Affiche un écran qui doit être fermé avant de continuer
- Utilisé pour les tâches qui nécessitent l'attention complète de l'utilisateur
- Exemples : dialogues de confirmation, assistants, formulaires de saisie

## Implémenter la navigation par pile dans FireMonkey

La navigation par pile est la base de la plupart des applications mobiles. Voici comment l'implémenter avec Delphi et FireMonkey.

### Étape 1 : Créer un projet d'application mobile

1. Lancez Delphi et créez un nouveau projet FireMonkey Mobile
2. Sélectionnez "Multi-Device Application"
3. Choisissez "Blank Application"
4. Enregistrez le projet

### Étape 2 : Configurer l'écran principal

Commençons par configurer l'écran principal qui servira de point d'entrée :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox;

type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    lblTitle: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  DetailForm; // Nous créerons cette unité plus tard

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Configuration initiale de l'écran
  lblTitle.Text := 'Écran principal';

  // Configurer les éléments de la liste
  ListBoxItem1.Text := 'Produit 1';
  ListBoxItem2.Text := 'Produit 2';

  // Ajouter quelques éléments supplémentaires pour l'exemple
  ListBox1.Items.Add('Produit 3');
  ListBox1.Items.Add('Produit 4');
end;

procedure TFormMain.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Ouvrir l'écran de détails quand un élément est cliqué
  ShowDetailForm(Item.Text);
end;

procedure TFormMain.ShowDetailForm(const ProductName: string);
var
  DetailForm: TFormDetail;
begin
  DetailForm := TFormDetail.Create(nil);
  try
    // Passer des données à l'écran de détails
    DetailForm.SetProductName(ProductName);

    // Afficher l'écran de détails
    DetailForm.Show;
  except
    DetailForm.Free;
    raise;
  end;
end;

end.
```

Dans le Designer FireMonkey, ajoutez ces composants à votre formulaire principal :
- Un `TToolBar` en haut
- Un `TLabel` dans la barre d'outils (pour le titre)
- Un `TListBox` qui occupe la majeure partie de l'écran
- Ajoutez quelques `TListBoxItem` à la liste

### Étape 3 : Créer l'écran de détails

Ajoutez un nouveau formulaire à votre projet (File → New → Multi-Device Form) et implémentez-le comme suit :

```pascal
unit DetailForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TFormDetail = class(TForm)
    ToolBar1: TToolBar;
    btnBack: TButton;
    lblTitle: TLabel;
    lblProductName: TLabel;
    lblDescription: TLabel;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProductName: string;
  public
    procedure SetProductName(const Value: string);
  end;

implementation

{$R *.fmx}

procedure TFormDetail.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  lblTitle.Text := 'Détails du produit';
end;

procedure TFormDetail.SetProductName(const Value: string);
begin
  FProductName := Value;

  // Mettre à jour l'interface utilisateur
  lblProductName.Text := FProductName;
  lblDescription.Text := 'Description de ' + FProductName;
end;

procedure TFormDetail.btnBackClick(Sender: TObject);
begin
  // Fermer cet écran et revenir au précédent
  Close;
end;

end.
```

Dans le Designer, ajoutez ces composants à votre formulaire de détails :
- Un `TToolBar` en haut
- Un `TButton` dans la barre d'outils (pour revenir en arrière)
- Un `TLabel` dans la barre d'outils (pour le titre)
- Un `TLabel` pour afficher le nom du produit
- Un `TLabel` pour afficher la description

**Important** : Positionnez le bouton de retour à gauche de la barre d'outils et définissez son texte à "Retour" ou utilisez un caractère comme "←".

### Étape 4 : Améliorer la navigation

La navigation de base fonctionne, mais elle n'est pas très élégante. Améliorons-la avec des transitions et une meilleure gestion de l'écran :

```pascal
// Dans MainForm.pas, modifiez la méthode ShowDetailForm
procedure TFormMain.ShowDetailForm(const ProductName: string);
var
  DetailForm: TFormDetail;
begin
  DetailForm := TFormDetail.Create(Application);

  // Configurer le formulaire avant de l'afficher
  DetailForm.SetProductName(ProductName);

  // Transition vers le nouvel écran (glissement depuis la droite)
  DetailForm.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      // Code exécuté après la fermeture du formulaire
      // (nettoyage, actualisation, etc.)
    end);
end;
```

## Implémenter la navigation par onglets

La navigation par onglets est idéale pour accéder rapidement aux principales fonctionnalités. Voyons comment l'implémenter.

### Étape 1 : Créer un projet avec TabControl

1. Créez un nouveau projet FireMonkey Mobile
2. Ajoutez un `TTabControl` qui occupe la majeure partie de l'écran
3. Ajoutez un `TTabItem` pour chaque onglet (par exemple : Accueil, Produits, Panier, Profil)
4. Ajoutez un `TToolBar` en bas de l'écran pour les boutons d'onglets

```pascal
unit MainTabForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TFormTabs = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    ToolBar1: TToolBar;
    btnTab1: TButton;
    btnTab2: TButton;
    btnTab3: TButton;
    btnTab4: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    lblTab1: TLabel;
    lblTab2: TLabel;
    lblTab3: TLabel;
    lblTab4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnTab1Click(Sender: TObject);
    procedure btnTab2Click(Sender: TObject);
    procedure btnTab3Click(Sender: TObject);
    procedure btnTab4Click(Sender: TObject);
  private
    procedure UpdateTabButtons;
  public
    { Public declarations }
  end;

var
  FormTabs: TFormTabs;

implementation

{$R *.fmx}

procedure TFormTabs.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  TabControl1.ActiveTab := TabItem1;

  // Définir les titres des onglets
  TabItem1.Text := 'Accueil';
  TabItem2.Text := 'Produits';
  TabItem3.Text := 'Panier';
  TabItem4.Text := 'Profil';

  // Définir le contenu de démonstration
  lblTab1.Text := 'Écran d''accueil';
  lblTab2.Text := 'Liste des produits';
  lblTab3.Text := 'Votre panier';
  lblTab4.Text := 'Votre profil';

  // Définir les boutons d'onglets
  btnTab1.Text := 'Accueil';
  btnTab2.Text := 'Produits';
  btnTab3.Text := 'Panier';
  btnTab4.Text := 'Profil';

  // Mettre à jour l'état des boutons
  UpdateTabButtons;
end;

procedure TFormTabs.btnTab1Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  UpdateTabButtons;
end;

procedure TFormTabs.btnTab2Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem2;
  UpdateTabButtons;
end;

procedure TFormTabs.btnTab3Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem3;
  UpdateTabButtons;
end;

procedure TFormTabs.btnTab4Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem4;
  UpdateTabButtons;
end;

procedure TFormTabs.UpdateTabButtons;
begin
  // Mise en évidence du bouton actif
  btnTab1.Opacity := IfThen(TabControl1.ActiveTab = TabItem1, 1.0, 0.6);
  btnTab2.Opacity := IfThen(TabControl1.ActiveTab = TabItem2, 1.0, 0.6);
  btnTab3.Opacity := IfThen(TabControl1.ActiveTab = TabItem3, 1.0, 0.6);
  btnTab4.Opacity := IfThen(TabControl1.ActiveTab = TabItem4, 1.0, 0.6);
end;

end.
```

### Étape 2 : Améliorer la barre d'onglets

Pour une apparence plus professionnelle, ajoutez des icônes à vos boutons d'onglets :

```pascal
// Dans le Designer, ajoutez des TImage à chaque bouton
// Ou personnalisez en code

procedure TFormTabs.FormCreate(Sender: TObject);
begin
  // ... Code existant

  // Ajouter des images aux boutons d'onglets
  // (supposons que les images sont dans le projet)
  btnTab1.ImageIndex := 0; // Icône Accueil
  btnTab2.ImageIndex := 1; // Icône Produits
  btnTab3.ImageIndex := 2; // Icône Panier
  btnTab4.ImageIndex := 3; // Icône Profil

  // Style des boutons pour onglets
  btnTab1.StyleLookup := 'tabitembutton';
  btnTab2.StyleLookup := 'tabitembutton';
  btnTab3.StyleLookup := 'tabitembutton';
  btnTab4.StyleLookup := 'tabitembutton';
end;
```

### Étape 3 : Combiner navigation par onglets et par pile

Dans les applications réelles, vous combinerez souvent navigation par onglets et navigation par pile :

```pascal
// Dans l'onglet Produits, implémentez l'ouverture d'un détail de produit

procedure TFormTabs.ListBoxProductsItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  DetailForm: TFormDetail;
begin
  DetailForm := TFormDetail.Create(Application);

  // Configurer le formulaire avant de l'afficher
  DetailForm.SetProductName(Item.Text);

  // Afficher l'écran de détails en mode modal
  DetailForm.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      // Retour à l'onglet Produits
    end);
end;
```

## Implémenter un menu tiroir (Drawer Navigation)

Le menu tiroir est une autre forme populaire de navigation, particulièrement sur Android.

### Étape 1 : Créer la structure du formulaire

1. Créez un nouveau projet FireMonkey Mobile
2. Ajoutez un `TLayout` (`LayoutDrawer`) pour contenir le menu tiroir
3. Ajoutez un `TLayout` (`LayoutContent`) pour le contenu principal
4. Ajoutez un `TToolBar` avec un bouton de menu hamburger

```pascal
unit DrawerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Ani;

type
  TFormDrawer = class(TForm)
    ToolBar1: TToolBar;
    btnMenu: TButton;
    lblTitle: TLabel;
    LayoutDrawer: TLayout;
    LayoutContent: TLayout;
    ListBoxMenu: TListBox;
    LayoutOverlay: TLayout;
    FloatAnimationDrawer: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure btnMenuClick(Sender: TObject);
    procedure ListBoxMenuItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure LayoutOverlayClick(Sender: TObject);
  private
    FDrawerOpen: Boolean;
    procedure ToggleDrawer;
    procedure CloseDrawer;
    procedure OpenDrawer;
    procedure SwitchToScreen(const ScreenName: string);
  public
    { Public declarations }
  end;

var
  FormDrawer: TFormDrawer;

implementation

{$R *.fmx}

procedure TFormDrawer.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  lblTitle.Text := 'Écran d''accueil';
  FDrawerOpen := False;

  // Configurer le menu tiroir
  LayoutDrawer.Width := 250;
  LayoutDrawer.Position.X := -LayoutDrawer.Width;

  // Configurer l'overlay (couche semi-transparente)
  LayoutOverlay.Visible := False;
  LayoutOverlay.HitTest := True;
  LayoutOverlay.Opacity := 0;

  // Configurer l'animation du tiroir
  FloatAnimationDrawer.PropertyName := 'Position.X';
  FloatAnimationDrawer.Parent := LayoutDrawer;
  FloatAnimationDrawer.Duration := 0.25;

  // Remplir le menu
  ListBoxMenu.Items.Add('Accueil');
  ListBoxMenu.Items.Add('Produits');
  ListBoxMenu.Items.Add('Catégories');
  ListBoxMenu.Items.Add('Mon compte');
  ListBoxMenu.Items.Add('Paramètres');
  ListBoxMenu.Items.Add('À propos');
end;

procedure TFormDrawer.btnMenuClick(Sender: TObject);
begin
  ToggleDrawer;
end;

procedure TFormDrawer.ToggleDrawer;
begin
  if FDrawerOpen then
    CloseDrawer
  else
    OpenDrawer;
end;

procedure TFormDrawer.OpenDrawer;
begin
  // Afficher le tiroir
  FDrawerOpen := True;

  // Animer l'ouverture du tiroir
  FloatAnimationDrawer.StartValue := LayoutDrawer.Position.X;
  FloatAnimationDrawer.StopValue := 0;
  FloatAnimationDrawer.Start;

  // Afficher l'overlay
  LayoutOverlay.Visible := True;
  LayoutOverlay.BringToFront;
  LayoutDrawer.BringToFront;

  // Animer l'opacité de l'overlay
  TAnimator.AnimateFloat(LayoutOverlay, 'Opacity', 0.5, 0.25);
end;

procedure TFormDrawer.CloseDrawer;
begin
  // Cacher le tiroir
  FDrawerOpen := False;

  // Animer la fermeture du tiroir
  FloatAnimationDrawer.StartValue := LayoutDrawer.Position.X;
  FloatAnimationDrawer.StopValue := -LayoutDrawer.Width;
  FloatAnimationDrawer.Start;

  // Animer l'opacité de l'overlay puis le cacher
  TAnimator.AnimateFloat(LayoutOverlay, 'Opacity', 0, 0.25,
    TAnimationType.Out, TInterpolationType.Linear,
    procedure
    begin
      LayoutOverlay.Visible := False;
    end);
end;

procedure TFormDrawer.LayoutOverlayClick(Sender: TObject);
begin
  // Fermer le tiroir quand on clique en dehors
  CloseDrawer;
end;

procedure TFormDrawer.ListBoxMenuItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Changer d'écran quand on clique sur un élément du menu
  SwitchToScreen(Item.Text);

  // Fermer le tiroir
  CloseDrawer;
end;

procedure TFormDrawer.SwitchToScreen(const ScreenName: string);
begin
  // Mettre à jour le titre
  lblTitle.Text := ScreenName;

  // Dans une application réelle, vous changeriez de contenu ici
  ShowMessage('Navigation vers : ' + ScreenName);
end;

end.
```

## Navigation modale

La navigation modale est utile pour des interactions isolées. En FireMonkey, cela se fait facilement avec `ShowModal`.

### Exemple d'écran de connexion modal

```pascal
// Dans le formulaire principal, ajoutez un bouton de connexion
procedure TMainForm.btnLoginClick(Sender: TObject);
var
  LoginForm: TLoginForm;
begin
  LoginForm := TLoginForm.Create(nil);
  try
    // Afficher l'écran de connexion en mode modal
    if LoginForm.ShowModal = mrOk then
    begin
      // L'utilisateur s'est connecté avec succès
      lblUser.Text := 'Connecté en tant que : ' + LoginForm.Username;
    end;
  finally
    LoginForm.Free;
  end;
end;
```

## Utiliser un composant TMultiView pour une navigation avancée

FireMonkey inclut un composant `TMultiView` spécialement conçu pour la navigation par tiroir.

### Étape 1 : Configurer le TMultiView

1. Ajoutez un `TMultiView` à votre formulaire
2. Placez un `TListBox` à l'intérieur du `TMultiView.DrawerContent`
3. Placez votre contenu principal dans `TMultiView.MasterContent`

```pascal
unit MultiViewForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.MultiView;

type
  TFormMultiView = class(TForm)
    MultiView1: TMultiView;
    ToolBar1: TToolBar;
    btnMenu: TButton;
    lblTitle: TLabel;
    ListBoxMenu: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    Layout1: TLayout;
    lblContent: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxMenuItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    procedure SwitchToScreen(const ScreenName: string);
  public
    { Public declarations }
  end;

var
  FormMultiView: TFormMultiView;

implementation

{$R *.fmx}

procedure TFormMultiView.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.DrawerOptions.Mode := TDrawerMode.OverlapDetailView;
  MultiView1.DrawerOptions.Gesture := [TDrawerGesture.Edge, TDrawerGesture.Swipe];

  // Configurer le bouton de menu
  btnMenu.StyleLookup := 'drawertoolbutton';
  btnMenu.Text := '';

  // Titre initial
  lblTitle.Text := 'Écran d''accueil';
  lblContent.Text := 'Contenu de l''écran d''accueil';

  // Configurer le menu
  ListBoxItem1.Text := 'Accueil';
  ListBoxItem2.Text := 'Produits';
  ListBoxItem3.Text := 'Catégories';
  ListBoxItem4.Text := 'Mon compte';
  ListBoxItem5.Text := 'Paramètres';
end;

procedure TFormMultiView.ListBoxMenuItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Changer d'écran quand on clique sur un élément du menu
  SwitchToScreen(Item.Text);

  // Fermer le MultiView
  MultiView1.HideMaster;
end;

procedure TFormMultiView.SwitchToScreen(const ScreenName: string);
begin
  // Mettre à jour le titre
  lblTitle.Text := ScreenName;

  // Mettre à jour le contenu
  lblContent.Text := 'Contenu de l''écran : ' + ScreenName;
end;

end.
```

## Bonnes pratiques pour la navigation mobile

1. **Cohérence avec les plateformes** : Respectez les conventions de navigation d'iOS et Android
2. **Retour intuitif** : Assurez-vous que les utilisateurs peuvent toujours revenir à l'écran précédent
3. **Transitions fluides** : Ajoutez des animations pour rendre la navigation plus agréable
4. **Feedback visuel** : Indiquez clairement l'écran actif (couleur, icône, texte)
5. **Navigation adaptative** : Adaptez la navigation selon la taille d'écran et l'orientation
6. **Profondeur limitée** : Évitez d'avoir plus de 3 niveaux de profondeur dans la navigation
7. **Navigation efficace** : Permettez d'accéder aux fonctions principales en 2-3 touches maximum

## Astuces et pièges à éviter

### Gestion de la mémoire

Soyez attentif à la libération des formulaires :

```pascal
// Évitez ce code (fuite de mémoire) :
procedure TMainForm.btnShowDetailBadClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(nil);
  DetailForm.Show; // Fuite de mémoire ! Qui va libérer DetailForm ?
end;

// Préférez cette approche :
procedure TMainForm.btnShowDetailGoodClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(Application);
  DetailForm.Show;
end;

// Ou encore mieux, avec ShowModal et un gestionnaire :
procedure TMainForm.btnShowDetailBestClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(nil);
  try
    DetailForm.ShowModal(
      procedure(ModalResult: TModalResult)
      begin
        // Le formulaire se ferme ici
      end);
  except
    DetailForm.Free;
    raise;
  end;
end;
```

### Gestion du cycle de vie de l'application

N'oubliez pas que les applications mobiles peuvent être mises en arrière-plan ou terminées à tout moment :

```pascal
procedure TMainForm.FormSaveState(Sender: TObject);
begin
  // Sauvegarder l'état avant que l'application soit mise en arrière-plan
  SaveCurrentScreen;
  SaveUserProgress;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Restaurer l'état à la réouverture
  if HasSavedState then
    RestoreAppState;
end;
```

### Adaptation aux différentes plateformes

Adaptez votre navigation aux conventions de chaque plateforme :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Adapter l'interface selon la plateforme
  {$IFDEF IOS}
  // Style iOS
  btnBack.Text := '';
  btnBack.StyleLookup := 'backtoolbutton';
  {$ENDIF}

  {$IFDEF ANDROID}
  // Style Android
  btnBack.Text := '';
  btnBack.StyleLookup := 'arrowlefttoolbutton';
  {$ENDIF}
end;
```

## Exemple pratique : Application complète

Pour illustrer ces concepts, créons une petite application de recettes combinant plusieurs types de navigation :

1. Navigation par onglets pour les sections principales (Recettes, Favoris, Courses, Profil)
2. Navigation par pile pour afficher les détails des recettes
3. Menu tiroir pour les options et paramètres

Ce code est un point de départ que vous pouvez étendre :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.TabControl, FMX.MultiView;

type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    btnMenu: TButton;
    lblTitle: TLabel;
    MultiView1: TMultiView;
    ListBoxMenu: TListBox;
    TabControl1: TTabControl;
    TabItemRecipes: TTabItem;
    TabItemFavorites: TTabItem;
    TabItemShopping: TTabItem;
    TabItemProfile: TTabItem;
    Layout1: TLayout;
    btnTab1: TButton;
    btnTab2: TButton;
    btnTab3: TButton;
    btnTab4: TButton;
    ListBoxRecipes: TListBox;
    ListBoxFavorites: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnTab1Click(Sender: TObject);
    procedure btnTab2Click(Sender: TObject);
    procedure btnTab3Click(Sender: TObject);
    procedure btnTab4Click(Sender: TObject);
    procedure ListBoxRecipesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure ListBoxMenuItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    procedure UpdateTabButtons;
    procedure ShowRecipeDetail(const RecipeName: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RecipeDetailForm;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Configuration du MultiView
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.DrawerOptions.Mode := TDrawerMode.OverlapDetailView;

  // Configuration du bouton de menu
  btnMenu.StyleLookup := 'drawertoolbutton';
  btnMenu.Text := '';

  // Configurer TabControl
  TabControl1.ActiveTab := TabItemRecipes;

  // Configurer les titres
  TabItemRecipes.Text := 'Recettes';
  TabItemFavorites.Text := 'Favoris';
  TabItemShopping.Text := 'Courses';
  TabItemProfile.Text := 'Profil';

  // Configurer les boutons d'onglets
  btnTab1.Text := 'Recettes';
  btnTab2.Text := 'Favoris';
  btnTab3.Text := 'Courses';
  btnTab4.Text := 'Profil';

  // Ajouter des éléments au menu latéral
  ListBoxMenu.Items.Add('Paramètres');
  ListBoxMenu.Items.Add('Aide');
  ListBoxMenu.Items.Add('À propos');
  ListBoxMenu.Items.Add('Commentaires');
  ListBoxMenu.Items.Add('Noter l''application');

  // Ajouter des recettes de démonstration
  ListBoxRecipes.Items.Clear;
  for I := 1 to 20 do
    ListBoxRecipes.Items.Add('Recette ' + I.ToString);

  // Ajouter des favoris de démonstration
  ListBoxFavorites.Items.Clear;
  ListBoxFavorites.Items.Add('Recette favorite 1');
  ListBoxFavorites.Items.Add('Recette favorite 2');
  ListBoxFavorites.Items.Add('Recette favorite 3');

  // Mettre à jour les boutons d'onglets
  UpdateTabButtons;

  // Définir le titre initial
  lblTitle.Text := 'Recettes';
end;

procedure TFormMain.btnTab1Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemRecipes;
  lblTitle.Text := 'Recettes';
  UpdateTabButtons;
end;

procedure TFormMain.btnTab2Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemFavorites;
  lblTitle.Text := 'Favoris';
  UpdateTabButtons;
end;

procedure TFormMain.btnTab3Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemShopping;
  lblTitle.Text := 'Liste de courses';
  UpdateTabButtons;
end;

procedure TFormMain.btnTab4Click(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemProfile;
  lblTitle.Text := 'Profil';
  UpdateTabButtons;
end;

procedure TFormMain.UpdateTabButtons;
begin
  // Mise en évidence du bouton actif
  btnTab1.Opacity := IfThen(TabControl1.ActiveTab = TabItemRecipes, 1.0, 0.6);
  btnTab2.Opacity := IfThen(TabControl1.ActiveTab = TabItemFavorites, 1.0, 0.6);
  btnTab3.Opacity := IfThen(TabControl1.ActiveTab = TabItemShopping, 1.0, 0.6);
  btnTab4.Opacity := IfThen(TabControl1.ActiveTab = TabItemProfile, 1.0, 0.6);
end;

procedure TFormMain.ListBoxRecipesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Afficher le détail de la recette
  ShowRecipeDetail(Item.Text);
end;

procedure TFormMain.ShowRecipeDetail(const RecipeName: string);
var
  DetailForm: TRecipeDetailForm;
begin
  DetailForm := TRecipeDetailForm.Create(Application);
  try
    // Configurer les détails de la recette
    DetailForm.SetRecipeName(RecipeName);

    // Afficher l'écran de détails
    DetailForm.Show;
  except
    DetailForm.Free;
    raise;
  end;
end;

procedure TFormMain.ListBoxMenuItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  // Gérer les options du menu
  MultiView1.HideMaster;

  if Item.Text = 'Paramètres' then
    ShowSettings
  else if Item.Text = 'Aide' then
    ShowHelp
  else if Item.Text = 'À propos' then
    ShowAbout
  else if Item.Text = 'Noter l''application' then
    RateApp;
end;
```

Créons maintenant le formulaire de détail de recette :

```pascal
unit RecipeDetailForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects;

type
  TRecipeDetailForm = class(TForm)
    ToolBar1: TToolBar;
    btnBack: TButton;
    lblTitle: TLabel;
    btnFavorite: TButton;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    lblRecipeName: TLabel;
    lblPreparationTime: TLabel;
    lblDifficulty: TLabel;
    Layout1: TLayout;
    lblIngredientsTitle: TLabel;
    lblIngredients: TLabel;
    Layout2: TLayout;
    lblDirectionsTitle: TLabel;
    lblDirections: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnFavoriteClick(Sender: TObject);
  private
    FRecipeName: string;
    FIsFavorite: Boolean;
    procedure UpdateFavoriteButton;
  public
    procedure SetRecipeName(const Value: string);
  end;

implementation

{$R *.fmx}

procedure TRecipeDetailForm.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  FIsFavorite := False;

  // Configurer le bouton de retour selon la plateforme
  {$IFDEF IOS}
  btnBack.StyleLookup := 'backtoolbutton';
  {$ENDIF}

  {$IFDEF ANDROID}
  btnBack.StyleLookup := 'arrowlefttoolbutton';
  {$ENDIF}

  btnBack.Text := '';

  // Configuration du bouton favori
  UpdateFavoriteButton;
end;

procedure TRecipeDetailForm.SetRecipeName(const Value: string);
begin
  FRecipeName := Value;

  // Mettre à jour l'interface
  lblTitle.Text := FRecipeName;
  lblRecipeName.Text := FRecipeName;

  // Dans une application réelle, vous chargeriez les détails de la recette depuis une source de données
  lblPreparationTime.Text := 'Temps de préparation : 30 minutes';
  lblDifficulty.Text := 'Difficulté : Facile';

  lblIngredients.Text :=
    '- 200g de farine' + sLineBreak +
    '- 100g de sucre' + sLineBreak +
    '- 2 œufs' + sLineBreak +
    '- 100ml de lait' + sLineBreak +
    '- 50g de beurre' + sLineBreak +
    '- 1 sachet de levure';

  lblDirections.Text :=
    '1. Mélanger la farine et la levure.' + sLineBreak +
    '2. Ajouter le sucre et mélanger.' + sLineBreak +
    '3. Incorporer les œufs un à un.' + sLineBreak +
    '4. Ajouter le lait progressivement.' + sLineBreak +
    '5. Faire fondre le beurre et l''ajouter à la préparation.' + sLineBreak +
    '6. Cuire à 180°C pendant 25 minutes.';
end;

procedure TRecipeDetailForm.btnBackClick(Sender: TObject);
begin
  // Fermer l'écran et revenir à la liste
  Close;
end;

procedure TRecipeDetailForm.btnFavoriteClick(Sender: TObject);
begin
  // Inverser l'état favori
  FIsFavorite := not FIsFavorite;
  UpdateFavoriteButton;

  // Dans une application réelle, vous sauvegarderiez cet état
  if FIsFavorite then
    ShowMessage('Ajouté aux favoris')
  else
    ShowMessage('Retiré des favoris');
end;

procedure TRecipeDetailForm.UpdateFavoriteButton;
begin
  if FIsFavorite then
  begin
    btnFavorite.StyleLookup := 'starglyph';
    btnFavorite.Opacity := 1.0;
  end
  else
  begin
    btnFavorite.StyleLookup := 'starglyphgray';
    btnFavorite.Opacity := 0.6;
  end;
end;

end.
```

## Navigation responsive selon l'orientation de l'appareil

Une bonne application mobile doit s'adapter à l'orientation de l'appareil. Voici comment gérer cela:

```pascal
procedure TMainForm.FormResize(Sender: TObject);
begin
  // Détecter l'orientation
  if Width > Height then
    HandleLandscapeOrientation
  else
    HandlePortraitOrientation;
end;

procedure TMainForm.HandlePortraitOrientation;
begin
  // Configuration pour l'orientation portrait
  TabBtnsLayout.Align := TAlignLayout.Bottom;
  TabBtnsLayout.Height := 60;

  // Ajuster les contrôles pour le mode portrait
  ListBoxRecipes.Margins.Bottom := TabBtnsLayout.Height;
end;

procedure TMainForm.HandleLandscapeOrientation;
begin
  // Configuration pour l'orientation paysage
  TabBtnsLayout.Align := TAlignLayout.Right;
  TabBtnsLayout.Width := 120;

  // Ajuster les contrôles pour le mode paysage
  ListBoxRecipes.Margins.Bottom := 0;
  ListBoxRecipes.Margins.Right := TabBtnsLayout.Width;
end;
```

## Gestion de la barre de notification (StatusBar)

La barre de notification (en haut de l'écran) peut être personnalisée pour une meilleure intégration:

```pascal
uses
  // ... autres uses
  FMX.StatusBar;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // ... autre code d'initialisation

  // Configurer la barre de statut
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  StatusBar.Visible := True;

  // Couleur de la barre de statut
  StatusBar.BackgroundColor := TAlphaColorRec.Green; // Ou votre couleur d'application

  // Style du texte dans la barre de statut
  // (important pour la lisibilité selon la couleur de fond)
  StatusBar.ForegroundColor := TAlphaColorRec.White;
  {$ENDIF}
end;
```

## Navigation adaptative selon la taille de l'écran

Pour une application universelle (téléphone et tablette), adaptez la navigation:

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
var
  ScreenService: IFMXScreenService;
  ScreenSize: TSizeF;
begin
  // ... autre code d'initialisation

  // Obtenir le service d'écran et la taille
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
  begin
    ScreenSize := ScreenService.GetScreenSize;

    // Vérifier si c'est une tablette (critère approximatif)
    if (ScreenSize.Width >= 600) or (ScreenSize.Height >= 600) then
      SetupTabletUI
    else
      SetupPhoneUI;
  end;
end;

procedure TMainForm.SetupTabletUI;
begin
  // Interface pour tablette - navigation divisée
  // Par exemple, liste à gauche et détails à droite en mode paysage
  SplitView.Visible := True;
  RecipesList.Width := 300;
  RecipesList.Align := TAlignLayout.Left;
  RecipeDetail.Align := TAlignLayout.Client;

  // Dans cet exemple, nous supposons que vous avez des composants
  // SplitView, RecipesList et RecipeDetail sur votre formulaire
end;

procedure TMainForm.SetupPhoneUI;
begin
  // Interface pour téléphone - navigation empilée
  SplitView.Visible := False;
  RecipesList.Align := TAlignLayout.Client;
  RecipeDetail.Visible := False;
end;

// Pour gérer le clic sur un élément de la liste dans l'interface tablette
procedure TMainForm.ListBoxRecipesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  ScreenService: IFMXScreenService;
  ScreenSize: TSizeF;
  IsTablet: Boolean;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
  begin
    ScreenSize := ScreenService.GetScreenSize;
    IsTablet := (ScreenSize.Width >= 600) or (ScreenSize.Height >= 600);

    if IsTablet and (Width > Height) then
    begin
      // Mode tablette en paysage - montrer les détails dans le panneau de droite
      RecipeDetail.Visible := True;
      LoadRecipeDetails(Item.Text, RecipeDetail);
    end
    else
    begin
      // Mode téléphone ou tablette en portrait - naviguer vers un nouvel écran
      ShowRecipeDetail(Item.Text);
    end;
  end;
end;
```

## Navigation avec gestes (Swipe Navigation)

FireMonkey permet d'implémenter facilement une navigation par glissement (swipe):

```pascal
unit SwipeNavigationForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.TabControl,
  FMX.Gestures;

type
  TFormSwipe = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    GestureManager1: TGestureManager;
    ToolBar1: TToolBar;
    lblTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    procedure UpdateTitle;
  public
    { Public declarations }
  end;

var
  FormSwipe: TFormSwipe;

implementation

{$R *.fmx}

procedure TFormSwipe.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  TabControl1.ActiveTab := TabItem1;
  TabItem1.Text := 'Écran 1';
  TabItem2.Text := 'Écran 2';
  TabItem3.Text := 'Écran 3';

  // Configurer les gestes
  Touch.GestureManager := GestureManager1;
  Touch.StandardGestures := [TStandardGesture.sgLeft, TStandardGesture.sgRight];

  // Mettre à jour le titre
  UpdateTitle;
end;

procedure TFormSwipe.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Gestion des gestes de swipe
  case EventInfo.GestureID of
    // Swipe vers la gauche (aller à droite)
    sgiLeft:
      begin
        if TabControl1.ActiveTab = TabItem1 then
          TabControl1.ActiveTab := TabItem2
        else if TabControl1.ActiveTab = TabItem2 then
          TabControl1.ActiveTab := TabItem3;

        UpdateTitle;
        Handled := True;
      end;

    // Swipe vers la droite (aller à gauche)
    sgiRight:
      begin
        if TabControl1.ActiveTab = TabItem3 then
          TabControl1.ActiveTab := TabItem2
        else if TabControl1.ActiveTab = TabItem2 then
          TabControl1.ActiveTab := TabItem1;

        UpdateTitle;
        Handled := True;
      end;
  end;
end;

procedure TFormSwipe.UpdateTitle;
begin
  lblTitle.Text := TabControl1.ActiveTab.Text;
end;

end.
```

## Navigation avec les boutons système

Sur les appareils mobiles, il est important de gérer correctement le bouton Retour du système (en particulier sur Android):

```pascal
procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  // Gérer le bouton retour d'Android
  if Key = vkHardwareBack then
  begin
    // Si le menu tiroir est ouvert, le fermer
    if MultiView1.IsShowed then
    begin
      MultiView1.HideMaster;
      Key := 0; // Empêcher l'action par défaut
    end
    // Si nous sommes dans un écran de détail, revenir à la liste
    else if FCurrentDetailForm <> nil then
    begin
      FCurrentDetailForm.Close;
      Key := 0; // Empêcher l'action par défaut
    end
    // Sinon, demander confirmation pour quitter l'application
    else if MessageDlg('Voulez-vous quitter l''application ?',
                       TMsgDlgType.mtConfirmation,
                       [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      // Laisser l'action par défaut se produire (quitter l'application)
    end
    else
    begin
      Key := 0; // Empêcher l'action par défaut
    end;
  end;
  {$ENDIF}
end;
```

## Partage d'état entre écrans

Lorsque vous naviguez entre plusieurs écrans, il est souvent nécessaire de partager des données:

### 1. Via une variable globale

```pascal
// Dans une unité séparée (DataModule.pas)
unit DataModule;

interface

uses
  System.SysUtils, System.Classes;

type
  TAppData = class
  private
    FUsername: string;
    FIsLoggedIn: Boolean;
    FThemeIndex: Integer;
  public
    property Username: string read FUsername write FUsername;
    property IsLoggedIn: Boolean read FIsLoggedIn write FIsLoggedIn;
    property ThemeIndex: Integer read FThemeIndex write FThemeIndex;
  end;

var
  AppData: TAppData;

implementation

initialization
  AppData := TAppData.Create;

finalization
  AppData.Free;

end.
```

### 2. Via des paramètres

```pascal
// Dans le formulaire principal
procedure TMainForm.ShowSettingsForm;
var
  SettingsForm: TSettingsForm;
begin
  SettingsForm := TSettingsForm.Create(nil);
  try
    // Passer les paramètres actuels
    SettingsForm.SetSettings(FNotificationsEnabled, FDarkModeEnabled);

    if SettingsForm.ShowModal = mrOk then
    begin
      // Récupérer les nouveaux paramètres
      SettingsForm.GetSettings(FNotificationsEnabled, FDarkModeEnabled);

      // Appliquer les nouveaux paramètres
      ApplySettings;
    end;
  finally
    SettingsForm.Free;
  end;
end;
```

### 3. Via des notifications

```pascal
// Dans une unité séparée (NotificationService.pas)
unit NotificationService;

interface

type
  TNotificationType = (ntLogin, ntLogout, ntThemeChanged, ntDataUpdated);
  TNotificationEvent = procedure(NotificationType: TNotificationType; Data: TObject) of object;

  TNotificationCenter = class
  private
    FObservers: TList<TNotificationEvent>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Subscribe(Observer: TNotificationEvent);
    procedure Unsubscribe(Observer: TNotificationEvent);
    procedure Notify(NotificationType: TNotificationType; Data: TObject = nil);
  end;

var
  NotificationCenter: TNotificationCenter;

implementation

uses
  System.Generics.Collections;

constructor TNotificationCenter.Create;
begin
  inherited Create;
  FObservers := TList<TNotificationEvent>.Create;
end;

destructor TNotificationCenter.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TNotificationCenter.Subscribe(Observer: TNotificationEvent);
begin
  if FObservers.IndexOf(Observer) < 0 then
    FObservers.Add(Observer);
end;

procedure TNotificationCenter.Unsubscribe(Observer: TNotificationEvent);
var
  Index: Integer;
begin
  Index := FObservers.IndexOf(Observer);
  if Index >= 0 then
    FObservers.Delete(Index);
end;

procedure TNotificationCenter.Notify(NotificationType: TNotificationType; Data: TObject);
var
  Observer: TNotificationEvent;
begin
  for Observer in FObservers do
    Observer(NotificationType, Data);
end;

initialization
  NotificationCenter := TNotificationCenter.Create;

finalization
  NotificationCenter.Free;

end.
```

Utilisation dans les formulaires:

```pascal
// Dans n'importe quel formulaire
procedure TLoginForm.DoLogin;
begin
  // Effectuer la connexion
  // ...

  // Notifier les autres écrans
  NotificationCenter.Notify(ntLogin, User);

  // Fermer l'écran de connexion
  Close;
end;

// Dans le formulaire principal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // ... autre code d'initialisation

  // S'abonner aux notifications
  NotificationCenter.Subscribe(HandleNotification);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Se désabonner des notifications
  NotificationCenter.Unsubscribe(HandleNotification);
end;

procedure TMainForm.HandleNotification(NotificationType: TNotificationType; Data: TObject);
begin
  case NotificationType of
    ntLogin:
      begin
        // Mettre à jour l'interface pour un utilisateur connecté
        lblUser.Text := TUser(Data).Username;
        btnLogin.Visible := False;
        btnLogout.Visible := True;
      end;

    ntLogout:
      begin
        // Mettre à jour l'interface pour un utilisateur déconnecté
        lblUser.Text := 'Non connecté';
        btnLogin.Visible := True;
        btnLogout.Visible := False;
      end;

    // Autres types de notifications
  end;
end;
```

## Gestion de l'état avec un pattern MVVM

Pour les applications plus complexes, considérez l'utilisation du pattern MVVM (Model-View-ViewModel):

```pascal
// ViewModel pour la liste des recettes
unit RecipesViewModel;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  RecipeModel; // Contient la définition du TRecipe

type
  TRecipesViewModel = class
  private
    FRecipes: TList<TRecipe>;
    FOnRecipesChanged: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadRecipes;
    function GetRecipe(Index: Integer): TRecipe;
    function GetRecipeCount: Integer;
    function FindRecipeByName(const Name: string): TRecipe;
    procedure ToggleFavorite(RecipeId: Integer);

    property OnRecipesChanged: TNotifyEvent read FOnRecipesChanged write FOnRecipesChanged;
  end;

implementation

// ... Implémentation
```

## Conclusion et bonnes pratiques

1. **Planifiez votre navigation** avant de commencer le développement
2. **Restez cohérent** avec les conventions de chaque plateforme
3. **Testez sur de vrais appareils** pour vérifier l'expérience utilisateur
4. **Optimisez pour les différentes tailles d'écran** (téléphone vs tablette)
5. **Utilisez des animations fluides** mais pas excessives
6. **Gardez à l'esprit l'accessibilité** (taille des boutons, contraste)
7. **Assurez-vous que l'utilisateur sait toujours où il se trouve** dans l'application
8. **Gérez correctement le cycle de vie** de l'application mobile

La navigation est l'une des clés d'une bonne expérience utilisateur mobile. Prenez le temps de la concevoir soigneusement pour que vos utilisateurs puissent naviguer intuitivement dans votre application Delphi.

## Exercices pratiques

1. **Exercice simple** : Créez une application avec 3 écrans et une navigation par pile simple (Main → Detail → Edit)
2. **Exercice intermédiaire** : Implémentez une application avec navigation par onglets (4 onglets) et un écran de détail accessible depuis l'un des onglets
3. **Exercice avancé** : Créez une application combinant menu tiroir, onglets et navigation par pile, avec un comportement adaptatif selon l'orientation de l'appareil

⏭️ [Gestion de l'état de l'application](/06-applications-multi-fenetres-et-navigation/07-gestion-de-letat-de-lapplication.md)
