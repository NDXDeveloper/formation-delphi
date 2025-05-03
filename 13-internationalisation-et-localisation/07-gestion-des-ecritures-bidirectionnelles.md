# 13.7 Gestion des écritures bidirectionnelles (RTL)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Certaines langues comme l'arabe, l'hébreu, le farsi (persan) et l'ourdou s'écrivent de droite à gauche (Right-to-Left ou RTL). La prise en charge de ces langues dans vos applications Delphi nécessite une attention particulière pour offrir une expérience utilisateur intuitive et naturelle. Cette section vous explique comment adapter votre interface pour les langues RTL.

## Comprendre les écritures bidirectionnelles

### Qu'est-ce que la bidirectionnalité ?

La bidirectionnalité (souvent abrégée "BiDi") désigne la capacité d'afficher à la fois des textes qui se lisent de droite à gauche (RTL) et des textes qui se lisent de gauche à droite (LTR). Ce concept est important car même dans un texte en arabe ou en hébreu, certains éléments comme les chiffres ou les mots en langues occidentales s'écrivent de gauche à droite.

### Caractéristiques des langues RTL

- Le texte commence à droite et se poursuit vers la gauche
- Les paragraphes sont alignés à droite
- L'interface utilisateur est généralement inversée (menus, boutons, etc.)
- Les chiffres et les mots en langues occidentales restent LTR même dans un texte RTL

## Le système BiDiMode de Delphi

Delphi offre une propriété centrale appelée `BiDiMode` qui permet de gérer l'orientation des textes et des contrôles. Cette propriété est disponible pour les formulaires et pour la plupart des contrôles visuels.

### Les valeurs possibles de BiDiMode

```pascal
type
  TBiDiMode = (bdLeftToRight, bdRightToLeft, bdRightToLeftNoAlign,
                bdRightToLeftReadingOnly);
```

Voici la signification de chaque valeur :

- `bdLeftToRight` : Affichage standard de gauche à droite (par défaut)
- `bdRightToLeft` : Affichage de droite à gauche avec alignement à droite
- `bdRightToLeftNoAlign` : Texte de droite à gauche sans alignement à droite
- `bdRightToLeftReadingOnly` : Seul l'ordre de lecture est de droite à gauche, pas l'alignement

## Application de BiDiMode à un formulaire

Pour prendre en charge les langues RTL dans votre application, commencez par configurer le mode BiDi au niveau du formulaire :

```pascal
procedure TForm1.SetRTLMode(IsRTL: Boolean);
begin
  if IsRTL then
    BiDiMode := bdRightToLeft
  else
    BiDiMode := bdLeftToRight;

  // Forcer la mise à jour de l'interface
  RecreateWnd;
end;
```

> 💡 Lorsque vous définissez `BiDiMode` sur un formulaire, tous les contrôles qui ont `ParentBiDiMode` à `True` hériteront automatiquement de ce paramètre.

## Définir BiDiMode au niveau de l'application

Pour appliquer le mode RTL à toute l'application d'un coup :

```pascal
procedure TMyApplication.SetRTLMode(IsRTL: Boolean);
begin
  if IsRTL then
  begin
    Application.BiDiMode := bdRightToLeft;
    // Forcer RTL au niveau de l'application
    SetProcessDefaultLayout(1);  // Windows API
  end
  else
  begin
    Application.BiDiMode := bdLeftToRight;
    SetProcessDefaultLayout(0);  // Windows API
  end;
end;
```

> ⚠️ Attention : Il est préférable de définir le `BiDiMode` de l'application avant la création des formulaires, idéalement dans le fichier .dpr du projet.

### Exemple dans le fichier projet (.dpr)

```pascal
program MyRTLApp;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1};

{$R *.res}

begin
  // Définir le mode RTL au niveau de l'application si nécessaire
  if SomeCondition then
    Application.BiDiMode := bdRightToLeft;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

## Héritage du BiDiMode

Chaque contrôle possède une propriété `ParentBiDiMode` qui, lorsqu'elle est définie à `True`, fait que le contrôle hérite de la propriété `BiDiMode` de son parent.

```pascal
procedure TForm1.ConfigureControls;
begin
  // Faire hériter tous les contrôles du mode BiDi du formulaire
  for var I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TControl then
      TControl(Components[I]).ParentBiDiMode := True;
  end;
end;
```

## Adaptation de la disposition des contrôles

Lorsque vous passez en mode RTL, Delphi inverse automatiquement la disposition des contrôles, mais il est parfois nécessaire d'apporter des ajustements manuels.

### Alignement automatique des contrôles

Dans la plupart des cas, Delphi gère automatiquement l'alignement des contrôles en mode RTL :

- Les libellés et textes sont alignés à droite
- Les barres de défilement passent du côté gauche
- L'ordre des onglets est inversé
- Les menus sont alignés à droite

### Inverser manuellement l'alignement

Dans certains cas, vous pourriez vouloir contrôler explicitement l'alignement :

```pascal
procedure TForm1.UpdateAlignment(IsRTL: Boolean);
begin
  if IsRTL then
  begin
    Label1.Alignment := taRightJustify;
    Edit1.TextHint := 'أدخل النص هنا';  // "Entrez le texte ici" en arabe
  end
  else
  begin
    Label1.Alignment := taLeftJustify;
    Edit1.TextHint := 'Entrez le texte ici';
  end;
end;
```

## Alignement des textes avec BiDiMode

Les propriétés d'alignement des textes interagissent avec `BiDiMode` :

```pascal
procedure TForm1.ConfigureTextAlignment;
begin
  // Pour un Label
  Label1.Alignment := taLeftJustify;  // En mode RTL, s'affichera à droite

  // Pour un Edit
  Edit1.Alignment := taRightJustify;  // Alignement explicite à droite

  // Pour un Memo
  Memo1.Alignment := taCenter;        // Centré quelle que soit la direction
end;
```

## Traitement des champs numériques et des dates

Les chiffres restent toujours affichés de gauche à droite, même en mode RTL. Cela peut nécessiter des adaptations pour les champs combinant texte et chiffres :

```pascal
procedure TForm1.SetupNumericFields;
begin
  // Pour les champs numériques, on peut garder LTR même en contexte RTL
  edtAmount.BiDiMode := bdLeftToRight;
  edtAmount.ParentBiDiMode := False;  // Ne pas hériter du parent

  // Pour les dates, cela dépend du format
  if FormatSettings.ShortDateFormat = 'yyyy/MM/dd' then
    edtDate.BiDiMode := bdLeftToRight
  else
    edtDate.BiDiMode := bdRightToLeft;
end;
```

## Gestion des composants complexes

### Tableaux et grilles

Les composants comme `TStringGrid` ou `TDBGrid` nécessitent une attention particulière :

```pascal
procedure TForm1.ConfigureGrid;
begin
  // En mode RTL, la première colonne est à droite
  StringGrid1.BiDiMode := bdRightToLeft;

  // Définir les titres des colonnes
  if StringGrid1.BiDiMode = bdRightToLeft then
  begin
    StringGrid1.Cells[0, 0] := 'العمود 1';  // "Colonne 1" en arabe
    StringGrid1.Cells[1, 0] := 'العمود 2';  // "Colonne 2" en arabe
  end
  else
  begin
    StringGrid1.Cells[0, 0] := 'Colonne 1';
    StringGrid1.Cells[1, 0] := 'Colonne 2';
  end;
end;
```

### Menus et barres d'outils

Les menus et barres d'outils sont automatiquement inversés en mode RTL :

```pascal
procedure TForm1.CreateRTLMenu;
var
  MenuItem: TMenuItem;
begin
  // Créer un menu - l'alignement sera géré automatiquement
  MenuItem := TMenuItem.Create(MainMenu1);
  MenuItem.Caption := 'ملف';  // "Fichier" en arabe
  MainMenu1.Items.Add(MenuItem);

  // Ajouter un sous-menu
  with TMenuItem.Create(MenuItem) do
  begin
    Caption := 'فتح';  // "Ouvrir" en arabe
    OnClick := OpenMenuItemClick;
    MenuItem.Add(Self);
  end;
end;
```

## Prise en charge des textes bidirectionnels

Les textes bidirectionnels (qui contiennent à la fois des parties RTL et LTR) nécessitent une attention particulière.

### Contrôle de l'orientation avec des marqueurs Unicode

Unicode inclut des caractères spéciaux qui contrôlent la direction du texte :

```pascal
const
  // Marqueurs de direction Unicode
  LRM = Char($200E);  // Left-to-right Mark
  RLM = Char($200F);  // Right-to-left Mark
  LRE = Char($202A);  // Left-to-right Embedding
  RLE = Char($202B);  // Right-to-left Embedding
  PDF = Char($202C);  // Pop Directional Formatting
  LRO = Char($202D);  // Left-to-right Override
  RLO = Char($202E);  // Right-to-left Override

procedure TForm1.SetupBidirectionalText;
var
  MixedText: string;
begin
  // Exemple : texte arabe avec un mot anglais intégré
  MixedText := 'مرحبا ' + LRE + 'Hello' + PDF + ' العالم';
  Label1.Caption := MixedText;
end;
```

> 💡 Ces marqueurs sont invisibles mais influencent l'affichage du texte.

## Changer dynamiquement la direction du texte

Vous pouvez permettre à l'utilisateur de changer la direction du texte à la volée :

```pascal
procedure TForm1.btnToggleDirectionClick(Sender: TObject);
begin
  // Inverser le mode BiDi actuel
  if BiDiMode = bdLeftToRight then
    BiDiMode := bdRightToLeft
  else
    BiDiMode := bdLeftToRight;

  // Mettre à jour l'interface
  RecreateWnd;
end;
```

## Détection automatique de la direction du texte

Vous pouvez détecter automatiquement si un texte est principalement RTL :

```pascal
function IsRTLText(const AText: string): Boolean;
var
  RTLCount, LTRCount, I: Integer;
  Ch: Char;
begin
  RTLCount := 0;
  LTRCount := 0;

  for I := 1 to Length(AText) do
  begin
    Ch := AText[I];

    // Plages de caractères RTL (simplifiées)
    if (Ord(Ch) >= $0590) and (Ord(Ch) <= $08FF) then
      Inc(RTLCount)
    // Plages de caractères LTR (simplifiées)
    else if ((Ord(Ch) >= $0041) and (Ord(Ch) <= $007A)) or
            ((Ord(Ch) >= $00C0) and (Ord(Ch) <= $024F)) then
      Inc(LTRCount);
  end;

  // Si plus de caractères RTL que LTR, considérer comme RTL
  Result := RTLCount > LTRCount;
end;

procedure TForm1.AutoDetectDirection;
begin
  // Détecter et appliquer automatiquement la direction
  if IsRTLText(Memo1.Text) then
    Memo1.BiDiMode := bdRightToLeft
  else
    Memo1.BiDiMode := bdLeftToRight;

  Memo1.ParentBiDiMode := False;  // Ne pas hériter du parent
end;
```

> ⚠️ Cette fonction est simplifiée. Pour une détection précise, il faudrait une analyse plus complète des plages Unicode.

## Gestion de l'éditeur de texte en mode RTL

Pour les champs de texte multilignes comme `TMemo`, des considérations supplémentaires s'appliquent :

```pascal
procedure TForm1.ConfigureRTLMemo;
begin
  // Configurer un Memo pour le texte arabe
  Memo1.BiDiMode := bdRightToLeft;
  Memo1.Alignment := taRightJustify;

  // Définir une police qui supporte l'arabe
  Memo1.Font.Name := 'Segoe UI';
  Memo1.Font.Size := 12;

  // Texte initial en arabe
  Memo1.Text := 'مرحبا بالعالم. هذا مثال على النص العربي.';

  // Configurer la barre de défilement
  Memo1.ScrollBars := ssVertical;  // Généralement à gauche en mode RTL
end;
```

## Adapter les raccourcis clavier

En mode RTL, il peut être judicieux d'adapter certains raccourcis clavier :

```pascal
procedure TForm1.UpdateShortcuts(IsRTL: Boolean);
begin
  if IsRTL then
  begin
    // Adapter les raccourcis pour RTL
    ActionNext.ShortCut := TextToShortCut('Alt+Left');  // Contre-intuitif mais utilisé
    ActionPrevious.ShortCut := TextToShortCut('Alt+Right');
  end
  else
  begin
    // Raccourcis standards LTR
    ActionNext.ShortCut := TextToShortCut('Alt+Right');
    ActionPrevious.ShortCut := TextToShortCut('Alt+Left');
  end;
end;
```

## Adaptation des formulaires de données

Pour les applications de bases de données, des adaptations supplémentaires peuvent être nécessaires :

```pascal
procedure TForm1.ConfigureDataControls;
begin
  // Grille de données
  DBGrid1.BiDiMode := BiDiMode;

  // Champs d'édition liés à des données
  DBEdit1.BiDiMode := BiDiMode;

  // Navigation entre enregistrements
  if BiDiMode = bdRightToLeft then
  begin
    btnNext.Left := btnPrevious.Left;
    btnPrevious.Left := btnNext.Left + btnNext.Width + 10;
  end
  else
  begin
    btnPrevious.Left := btnNext.Left;
    btnNext.Left := btnPrevious.Left + btnPrevious.Width + 10;
  end;
end;
```

## Intégration avec le système de traduction

Combinez le support RTL avec votre système de traduction :

```pascal
procedure TForm1.ApplyLanguage(const LangCode: string);
begin
  // Charger les chaînes traduites
  TranslationManager.LoadLanguage(LangCode);

  // Définir RTL pour les langues qui le nécessitent
  case LangCode of
    'ar', 'he', 'fa', 'ur':
      BiDiMode := bdRightToLeft;
    else
      BiDiMode := bdLeftToRight;
  end;

  // Appliquer les traductions
  Caption := TranslationManager.GetString('MainForm.Caption');
  btnOK.Caption := TranslationManager.GetString('Common.OK');
  // etc.

  // Mettre à jour l'interface
  RecreateWnd;
end;
```

## Exemple complet : Formulaire avec support RTL

Voici un exemple complet d'un formulaire qui prend en charge la bidirectionnalité :

```pascal
unit RTLSupportForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmRTLSupport = class(TForm)
    pnlTop: TPanel;
    lblLanguage: TLabel;
    cmbLanguage: TComboBox;
    btnApply: TButton;
    memText: TMemo;
    lblName: TLabel;
    edtName: TEdit;
    lblEmail: TLabel;
    edtEmail: TEdit;
    btnSubmit: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ApplyLanguageSettings;
    procedure UpdateControlsAlignment;
  end;

var
  frmRTLSupport: TfrmRTLSupport;

implementation

{$R *.dfm}

procedure TfrmRTLSupport.FormCreate(Sender: TObject);
begin
  // Initialiser la liste des langues
  cmbLanguage.Items.Clear;
  cmbLanguage.Items.Add('Français');
  cmbLanguage.Items.Add('English');
  cmbLanguage.Items.Add('العربية');  // Arabe
  cmbLanguage.Items.Add('עברית');    // Hébreu

  // Langue par défaut
  cmbLanguage.ItemIndex := 0;
end;

procedure TfrmRTLSupport.FormShow(Sender: TObject);
begin
  // Appliquer les paramètres initiaux
  ApplyLanguageSettings;
end;

procedure TfrmRTLSupport.btnApplyClick(Sender: TObject);
begin
  ApplyLanguageSettings;
end;

procedure TfrmRTLSupport.ApplyLanguageSettings;
var
  IsRTL: Boolean;
begin
  // Déterminer si la langue sélectionnée est RTL
  IsRTL := (cmbLanguage.ItemIndex >= 2);

  // Définir le mode BiDi du formulaire
  if IsRTL then
    BiDiMode := bdRightToLeft
  else
    BiDiMode := bdLeftToRight;

  // Charger les textes selon la langue
  case cmbLanguage.ItemIndex of
    0: // Français
      begin
        Caption := 'Formulaire avec support RTL';
        lblLanguage.Caption := 'Langue:';
        btnApply.Caption := 'Appliquer';
        lblName.Caption := 'Nom:';
        lblEmail.Caption := 'Email:';
        btnSubmit.Caption := 'Envoyer';
        btnCancel.Caption := 'Annuler';
        memText.Text := 'Exemple de texte en français. Ce texte s''affiche de gauche à droite.';
      end;
    1: // Anglais
      begin
        Caption := 'Form with RTL Support';
        lblLanguage.Caption := 'Language:';
        btnApply.Caption := 'Apply';
        lblName.Caption := 'Name:';
        lblEmail.Caption := 'Email:';
        btnSubmit.Caption := 'Submit';
        btnCancel.Caption := 'Cancel';
        memText.Text := 'Example text in English. This text displays from left to right.';
      end;
    2: // Arabe
      begin
        Caption := 'نموذج مع دعم RTL';
        lblLanguage.Caption := 'اللغة:';
        btnApply.Caption := 'تطبيق';
        lblName.Caption := 'الاسم:';
        lblEmail.Caption := 'البريد الإلكتروني:';
        btnSubmit.Caption := 'إرسال';
        btnCancel.Caption := 'إلغاء';
        memText.Text := 'مثال على النص باللغة العربية. يتم عرض هذا النص من اليمين إلى اليسار.';
      end;
    3: // Hébreu
      begin
        Caption := 'טופס עם תמיכה ב-RTL';
        lblLanguage.Caption := 'שפה:';
        btnApply.Caption := 'החל';
        lblName.Caption := 'שם:';
        lblEmail.Caption := 'אימייל:';
        btnSubmit.Caption := 'שלח';
        btnCancel.Caption := 'בטל';
        memText.Text := 'טקסט לדוגמה בעברית. טקסט זה מוצג מימין לשמאל.';
      end;
  end;

  // Mettre à jour l'alignement des contrôles
  UpdateControlsAlignment;

  // Forcer la mise à jour de l'interface
  RecreateWnd;
end;

procedure TfrmRTLSupport.UpdateControlsAlignment;
var
  IsRTL: Boolean;
begin
  IsRTL := (BiDiMode = bdRightToLeft);

  // Ajuster les contrôles qui nécessitent une attention particulière

  // Email est toujours LTR
  edtEmail.BiDiMode := bdLeftToRight;
  edtEmail.ParentBiDiMode := False;

  // Mais l'étiquette suit la direction générale
  lblEmail.BiDiMode := BiDiMode;
  lblEmail.ParentBiDiMode := True;

  // Ajuster la position des boutons
  if IsRTL then
  begin
    // En RTL, le bouton Submit est à gauche et Cancel à droite
    btnSubmit.Left := ClientWidth - btnSubmit.Width - 20;
    btnCancel.Left := btnSubmit.Left - btnCancel.Width - 10;
  end
  else
  begin
    // En LTR, le bouton Submit est à droite et Cancel à gauche
    btnCancel.Left := ClientWidth - btnCancel.Width - 20;
    btnSubmit.Left := btnCancel.Left - btnSubmit.Width - 10;
  end;
end;

end.
```

## Test du support RTL

Pour tester efficacement votre application en mode RTL :

1. **Changez les paramètres régionaux** de votre système d'exploitation à une langue RTL
2. **Utilisez des données réelles** en langues RTL, pas seulement des traductions
3. **Vérifiez les alignements** de tous les contrôles dans votre application
4. **Testez les interactions utilisateur** comme le glisser-déposer ou les raccourcis clavier
5. **Vérifiez les textes mixtes** contenant à la fois des parties RTL et LTR

## Bonnes pratiques pour le support RTL

1. **Définissez le BiDiMode au niveau de l'application** si possible, plutôt que formulaire par formulaire

2. **Utilisez ParentBiDiMode à True** pour la plupart des contrôles afin d'hériter automatiquement de la direction du formulaire

3. **Faites attention aux contrôles avec disposition fixe** comme les panneaux ou les boîtes de groupe

4. **Testez avec des utilisateurs natifs** de langues RTL si possible

5. **Gardez les champs numériques en mode LTR** même dans les interfaces RTL

6. **Évitez de coder en dur les positions des contrôles**, utilisez plutôt l'alignement ou l'ancrage

7. **Adaptez les icônes directionnelles** (flèches, etc.) pour qu'elles aient du sens en mode RTL

8. **Utilisez des méthodes comme FlipChildren** pour inverser la disposition des contrôles enfants si nécessaire

## Conclusion

La prise en charge des écritures bidirectionnelles est essentielle pour créer des applications véritablement internationales. Delphi fournit un bon support de base pour les langues RTL grâce à la propriété `BiDiMode` et à ses valeurs associées.

En comprenant les principes de la bidirectionnalité et en appliquant les techniques présentées dans cette section, vous pouvez adapter vos applications Delphi pour qu'elles offrent une expérience utilisateur intuitive et naturelle aux utilisateurs de langues RTL comme l'arabe et l'hébreu.

Les points clés à retenir :

1. Utilisez la propriété `BiDiMode` pour contrôler la direction du texte et des contrôles
2. Définissez la direction au niveau du formulaire ou de l'application et utilisez `ParentBiDiMode` pour propager
3. Faites attention aux textes mixtes qui contiennent à la fois des parties RTL et LTR
4. Adaptez la disposition des contrôles pour qu'elle soit cohérente avec la direction du texte
5. Testez votre application avec de vraies données en langues RTL

---

Dans la prochaine section, nous explorerons les outils de traduction et les flux de travail pour faciliter l'internationalisation de vos applications Delphi.

⏭️ [Outils de traduction et flux de travail](13-internationalisation-et-localisation/08-outils-de-traduction-et-flux-de-travail.md)
