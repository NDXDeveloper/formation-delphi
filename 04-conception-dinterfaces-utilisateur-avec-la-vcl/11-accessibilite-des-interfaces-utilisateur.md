🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.11 Accessibilité des interfaces utilisateur

## Introduction

L'accessibilité numérique consiste à rendre vos applications utilisables par tous, y compris les personnes en situation de handicap. Une interface accessible est non seulement une obligation légale dans de nombreux pays, mais c'est aussi un gage de qualité et d'inclusion qui améliore l'expérience de tous les utilisateurs.

## 4.11.1 Qu'est-ce que l'accessibilité ?

### Définition

L'**accessibilité** (accessibility, ou a11y) est la capacité d'une application à être utilisée par le plus grand nombre de personnes possible, quelles que soient leurs capacités physiques, sensorielles ou cognitives.

### Pourquoi l'accessibilité est importante

**1. Inclusion sociale**
```
Statistiques mondiales :
- 15% de la population mondiale vit avec un handicap
- 1 personne sur 12 a une déficience visuelle
- Nombreuses personnes âgées avec handicaps liés à l'âge
```

**2. Obligations légales**
```
Réglementations :
- Europe : Directive européenne sur l'accessibilité
- France : RGAA (Référentiel Général d'Amélioration de l'Accessibilité)
- USA : ADA (Americans with Disabilities Act)
- Section 508 pour les administrations
```

**3. Bénéfices pour tous**
```
Une application accessible est :
✓ Plus facile à utiliser pour tous
✓ Plus rapide à naviguer au clavier
✓ Mieux structurée et organisée
✓ Plus robuste et maintenable
✓ Compatible avec plus de contextes d'utilisation
```

**4. Avantages business**
```
✓ Élargissement du public cible
✓ Amélioration de l'image de marque
✓ Conformité réglementaire
✓ Réduction des risques légaux
✓ Meilleure satisfaction client
```

### Types de handicaps à considérer

**Handicaps visuels :**
- Cécité totale → Utilisation de lecteurs d'écran
- Malvoyance → Besoin de fort contraste et grossissement
- Daltonisme → Problèmes avec certaines couleurs
- Sensibilité à la lumière → Préférence pour modes sombres

**Handicaps auditifs :**
- Surdité → Besoin d'alternatives visuelles aux sons
- Malentendance → Sous-titres et signaux visuels

**Handicaps moteurs :**
- Difficulté avec la souris → Navigation au clavier uniquement
- Tremblements → Besoin de grandes zones cliquables
- Mobilité réduite → Raccourcis clavier essentiels

**Handicaps cognitifs :**
- Dyslexie → Polices adaptées, espacement
- Troubles de l'attention → Interface claire et simple
- Difficultés de mémorisation → Aide contextuelle

---

## 4.11.2 Principes fondamentaux de l'accessibilité

### Les 4 piliers POUR (WCAG)

**P - Perceptible**
```
L'information doit être présentable aux utilisateurs
de manière à ce qu'ils puissent la percevoir.

Exemples :
- Alternative textuelle pour les images
- Contraste suffisant entre texte et fond
- Texte redimensionnable
- Contenu adaptable
```

**O - Opérable (Utilisable)**
```
Les composants de l'interface doivent être utilisables
par tous les moyens d'interaction.

Exemples :
- Navigation au clavier complète
- Temps suffisant pour lire et utiliser
- Pas de contenus clignotants dangereux
- Navigation claire et prévisible
```

**U - Understandable (Compréhensible)**
```
L'information et l'utilisation de l'interface
doivent être compréhensibles.

Exemples :
- Texte lisible et compréhensible
- Fonctionnement prévisible
- Aide à la saisie
- Prévention des erreurs
```

**R - Robuste**
```
Le contenu doit être robuste pour être interprété
par différentes technologies d'assistance.

Exemples :
- Code valide et standard
- Compatibilité avec lecteurs d'écran
- Support de différents navigateurs
- Adaptation aux futures technologies
```

---

## 4.11.3 Navigation au clavier

### Importance de la navigation au clavier

De nombreux utilisateurs ne peuvent pas ou préfèrent ne pas utiliser la souris :
- Utilisateurs de lecteurs d'écran
- Personnes avec handicaps moteurs
- Utilisateurs avancés recherchant l'efficacité

### TabOrder : Ordre de tabulation

**Concept :**
La touche Tab permet de naviguer entre les contrôles. L'ordre doit être logique et prévisible.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Définir l'ordre de tabulation
  EditNom.TabOrder := 0;      // Premier champ
  EditPrenom.TabOrder := 1;   // Deuxième champ
  EditEmail.TabOrder := 2;    // Troisième champ
  ButtonValider.TabOrder := 3; // Dernier (bouton)

  // S'assurer que le premier champ a le focus
  EditNom.SetFocus;
end;
```

**Vérification automatique :**
```pascal
procedure TForm1.VerifierOrdreTabulation;
var
  i: Integer;
  Controle: TWinControl;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Ordre de tabulation :');

  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TWinControl then
    begin
      Controle := TWinControl(Controls[i]);
      if Controle.TabStop then
        Memo1.Lines.Add(Format('%d. %s', [Controle.TabOrder, Controle.Name]));
    end;
  end;
end;
```

### TabStop : Inclure/exclure de la tabulation

```pascal
procedure TForm1.ConfigurerTabStop;
begin
  // Composants devant recevoir le focus
  EditNom.TabStop := True;
  ButtonValider.TabStop := True;

  // Composants à exclure de la tabulation
  LabelTitre.TabStop := False;  // Les labels ne reçoivent généralement pas le focus
  Panel1.TabStop := False;      // Les panneaux non plus

  // Composants désactivés
  ButtonSupprimer.Enabled := False; // Automatiquement exclus de la tabulation
end;
```

### Raccourcis clavier accessibles

**Touches d'accès rapide (mnémoniques) :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Utiliser & pour créer un raccourci Alt+lettre
  LabelNom.Caption := '&Nom :';         // Alt+N
  LabelPrenom.Caption := '&Prénom :';   // Alt+P
  LabelEmail.Caption := '&Email :';     // Alt+E

  ButtonValider.Caption := '&Valider';  // Alt+V
  ButtonAnnuler.Caption := '&Annuler';  // Alt+A

  // Associer le label au contrôle
  LabelNom.FocusControl := EditNom;
  LabelPrenom.FocusControl := EditPrenom;
  LabelEmail.FocusControl := EditEmail;
end;
```

**Raccourcis clavier globaux :**
```pascal
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Ctrl+S pour sauvegarder
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    SauvegarderDonnees;
    Key := 0; // Consommer l'événement
  end;

  // Ctrl+N pour nouveau
  if (ssCtrl in Shift) and (Key = Ord('N')) then
  begin
    NouveauDocument;
    Key := 0;
  end;

  // F1 pour l'aide
  if Key = VK_F1 then
  begin
    AfficherAide;
    Key := 0;
  end;

  // Échap pour annuler/fermer
  if Key = VK_ESCAPE then
  begin
    if MessageDlg('Voulez-vous vraiment quitter ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Close;
    Key := 0;
  end;
end;
```

### Navigation dans les listes

```pascal
procedure TForm1.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SPACE, VK_RETURN:
      begin
        // Activer l'élément sélectionné
        if ListBox1.ItemIndex <> -1 then
          EditerElement(ListBox1.ItemIndex);
      end;

    VK_DELETE:
      begin
        // Supprimer l'élément sélectionné
        if ListBox1.ItemIndex <> -1 then
          if MessageDlg('Supprimer cet élément ?',
                        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            ListBox1.Items.Delete(ListBox1.ItemIndex);
      end;

    VK_INSERT:
      begin
        // Insérer un nouvel élément
        AjouterNouvelElement;
      end;
  end;
end;
```

---

## 4.11.4 Lecteurs d'écran et ARIA

### Qu'est-ce qu'un lecteur d'écran ?

Un **lecteur d'écran** est un logiciel qui lit à haute voix le contenu affiché à l'écran pour les personnes aveugles ou malvoyantes.

**Lecteurs d'écran populaires :**
- NVDA (Windows, gratuit)
- JAWS (Windows, commercial)
- Narrator (Windows, intégré)
- VoiceOver (macOS/iOS, intégré)

### Propriétés importantes pour l'accessibilité

**Hint : Info-bulle**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer les hints
  ShowHint := True;

  // Définir des hints descriptifs
  ButtonNouveau.Hint := 'Créer un nouveau document (Ctrl+N)';
  ButtonOuvrir.Hint := 'Ouvrir un document existant (Ctrl+O)';
  ButtonEnregistrer.Hint := 'Enregistrer le document (Ctrl+S)';

  // Hint pour les icônes sans texte
  SpeedButtonSupprimer.Hint := 'Supprimer l''élément sélectionné (Suppr)';

  // Hints plus longs pour aide détaillée
  EditMotDePasse.Hint :=
    'Mot de passe : minimum 8 caractères,' + #13#10 +
    'incluant au moins une majuscule,' + #13#10 +
    'un chiffre et un caractère spécial';
end;
```

**Caption : Texte descriptif**
```pascal
// Mauvais : Icônes sans texte
Button1.Caption := '';
Button2.Caption := '';

// Bon : Texte descriptif
ButtonNouveau.Caption := 'Nouveau';
ButtonOuvrir.Caption := 'Ouvrir';
ButtonEnregistrer.Caption := 'Enregistrer';

// Acceptable avec hint clair si contrainte d'espace
SpeedButton1.Caption := '';
SpeedButton1.Hint := 'Nouveau document (Ctrl+N)';
SpeedButton1.ShowHint := True;
```

**AccessibleName et AccessibleDescription (Windows)**
```pascal
// Pour les composants VCL avec support MSAA
procedure TForm1.ConfigurerAccessibilite;
begin
  // Ces propriétés aident les lecteurs d'écran
  Edit1.AccessibleName := 'Nom du client';
  Edit1.AccessibleDescription := 'Saisir le nom complet du client';

  Button1.AccessibleName := 'Valider le formulaire';
  Button1.AccessibleDescription := 'Cliquer pour enregistrer les modifications';
end;
```

### Annoncer les changements dynamiques

```pascal
// Informer l'utilisateur des changements
procedure TForm1.AjouterElementAListe(const Element: string);
begin
  ListBox1.Items.Add(Element);

  // Annoncer le changement
  StatusBar1.SimpleText := Format('%s ajouté. Total : %d éléments',
    [Element, ListBox1.Items.Count]);

  // Alternative : Message vocal avec Windows
  AnnoncerMessage(Format('%s ajouté', [Element]));
end;

procedure TForm1.AnnoncerMessage(const Message: string);
begin
  // Utiliser la synthèse vocale Windows (nécessite units supplémentaires)
  // Ou mettre à jour un composant visible par les lecteurs d'écran
  LabelAnnonce.Caption := Message;
end;
```

---

## 4.11.5 Contraste et couleurs

### Ratios de contraste recommandés

**Standards WCAG :**
```
Niveau AA (minimum) :
- Texte normal : ratio de 4.5:1
- Texte large (18pt+) : ratio de 3:1

Niveau AAA (optimal) :
- Texte normal : ratio de 7:1
- Texte large : ratio de 4.5:1
```

### Vérifier le contraste

```pascal
uses
  Vcl.Graphics;

function CalculerLuminance(Couleur: TColor): Double;
var
  R, G, B: Byte;
  Rs, Gs, Bs: Double;
begin
  // Convertir TColor en RGB
  R := GetRValue(Couleur);
  G := GetGValue(Couleur);
  B := GetBValue(Couleur);

  // Normaliser et appliquer la formule de luminance
  Rs := R / 255;
  Gs := G / 255;
  Bs := B / 255;

  // Correction gamma
  if Rs <= 0.03928 then
    Rs := Rs / 12.92
  else
    Rs := Power((Rs + 0.055) / 1.055, 2.4);

  if Gs <= 0.03928 then
    Gs := Gs / 12.92
  else
    Gs := Power((Gs + 0.055) / 1.055, 2.4);

  if Bs <= 0.03928 then
    Bs := Bs / 12.92
  else
    Bs := Power((Bs + 0.055) / 1.055, 2.4);

  Result := 0.2126 * Rs + 0.7152 * Gs + 0.0722 * Bs;
end;

function CalculerRatioContraste(Couleur1, Couleur2: TColor): Double;
var
  L1, L2: Double;
begin
  L1 := CalculerLuminance(Couleur1);
  L2 := CalculerLuminance(Couleur2);

  if L1 > L2 then
    Result := (L1 + 0.05) / (L2 + 0.05)
  else
    Result := (L2 + 0.05) / (L1 + 0.05);
end;

procedure TForm1.VerifierContrasteLabel;
var
  Ratio: Double;
begin
  Ratio := CalculerRatioContraste(Label1.Font.Color, Label1.Color);

  if Ratio >= 7.0 then
    ShowMessage(Format('Excellent ! Ratio : %.2f:1 (AAA)', [Ratio]))
  else if Ratio >= 4.5 then
    ShowMessage(Format('Bon. Ratio : %.2f:1 (AA)', [Ratio]))
  else if Ratio >= 3.0 then
    ShowMessage(Format('Acceptable pour texte large. Ratio : %.2f:1', [Ratio]))
  else
    ShowMessage(Format('INSUFFISANT ! Ratio : %.2f:1', [Ratio]));
end;
```

### Ne pas utiliser uniquement la couleur

**Mauvaise pratique :**
```pascal
// Utiliser uniquement la couleur pour indiquer un état
procedure TForm1.MarquerChampInvalide(Edit: TEdit);
begin
  Edit.Color := clRed; // Seul indicateur = problème pour daltoniens
end;
```

**Bonne pratique :**
```pascal
// Combiner couleur et autre indicateur visuel
procedure TForm1.MarquerChampInvalide(Edit: TEdit; const MessageErreur: string);
begin
  // Couleur
  Edit.Color := RGB(255, 230, 230); // Rouge pâle

  // Icône d'erreur
  ImageErreur.Visible := True;
  ImageErreur.Picture.LoadFromFile('erreur.png');

  // Texte explicatif
  LabelErreur.Caption := '⚠ ' + MessageErreur;
  LabelErreur.Font.Style := [fsBold];
  LabelErreur.Visible := True;

  // Bordure
  Edit.BorderStyle := bsSingle;
end;

procedure TForm1.MarquerChampValide(Edit: TEdit);
begin
  Edit.Color := clWindow;
  ImageErreur.Visible := False;
  LabelErreur.Visible := False;
end;
```

### Palettes de couleurs accessibles

```pascal
// Palettes testées pour le daltonisme
const
  // Couleurs sûres pour tous types de daltonisme
  COULEUR_SUCCES = TColor($00AA00);    // Vert
  COULEUR_ERREUR = TColor($0000CC);    // Rouge
  COULEUR_AVERTISSEMENT = TColor($0099FF); // Orange
  COULEUR_INFO = TColor($CC6600);      // Bleu

  // Couleurs avec bon contraste
  TEXTE_SOMBRE = TColor($333333);
  TEXTE_CLAIR = TColor($FFFFFF);
  FOND_CLAIR = TColor($FFFFFF);
  FOND_SOMBRE = TColor($2B2B2B);
```

---

## 4.11.6 Taille de texte et polices

### Polices accessibles

**Polices recommandées :**
```pascal
procedure TForm1.ChoisirPoliceAccessible;
begin
  // Polices sans serif, claires et lisibles
  Font.Name := 'Segoe UI';        // Windows moderne
  // ou
  Font.Name := 'Arial';           // Universelle
  // ou
  Font.Name := 'Calibri';         // Bonne lisibilité
  // ou
  Font.Name := 'Open Sans';       // Web-friendly

  // Taille minimum recommandée
  Font.Size := 10; // Minimum 9, idéalement 10-12

  // Éviter les polices décoratives pour le texte principal
  // Font.Name := 'Comic Sans MS'; // À éviter
  // Font.Name := 'Papyrus';       // À éviter
end;
```

### Polices pour la dyslexie

```pascal
procedure TForm1.ActiverModeSymptexie;
begin
  // Polices spécialement conçues pour la dyslexie
  // (nécessite d'installer la police sur le système)

  if Screen.Fonts.IndexOf('OpenDyslexic') <> -1 then
    Font.Name := 'OpenDyslexic'
  else if Screen.Fonts.IndexOf('Lexie Readable') <> -1 then
    Font.Name := 'Lexie Readable'
  else
    Font.Name := 'Arial'; // Fallback

  // Augmenter l'espacement
  // (nécessite de créer un composant personnalisé ou d'utiliser RichEdit)
end;
```

### Texte redimensionnable

```pascal
type
  TTaillePolice = (tpPetite, tpMoyenne, tpGrande, tpTresGrande);

procedure TForm1.ChangerTaillePolice(Taille: TTaillePolice);
var
  i: Integer;
  NouvelleSize: Integer;
begin
  case Taille of
    tpPetite:      NouvelleSize := 9;
    tpMoyenne:     NouvelleSize := 11;
    tpGrande:      NouvelleSize := 14;
    tpTresGrande:  NouvelleSize := 18;
  end;

  // Appliquer à tous les composants
  Font.Size := NouvelleSize;

  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TControl then
    begin
      TControl(Components[i]).Font.Size := NouvelleSize;
    end;
  end;

  // Redimensionner le formulaire si nécessaire
  AjusterDimensionsFormulaire;
end;

procedure TForm1.AjusterDimensionsFormulaire;
begin
  // Augmenter la hauteur pour accommoder le texte plus grand
  Height := Height + (Font.Size - 11) * 10;
end;
```

### Menu de réglage de taille

```pascal
procedure TForm1.CreerMenuAccessibilite;
var
  MenuAccessibilite, SousMenuTaille: TMenuItem;
begin
  // Créer menu principal
  MenuAccessibilite := TMenuItem.Create(MainMenu1);
  MenuAccessibilite.Caption := '&Accessibilité';
  MainMenu1.Items.Add(MenuAccessibilite);

  // Sous-menu taille de police
  SousMenuTaille := TMenuItem.Create(MenuAccessibilite);
  SousMenuTaille.Caption := 'Taille du &texte';
  MenuAccessibilite.Add(SousMenuTaille);

  // Options de taille
  AjouterOptionTaille(SousMenuTaille, 'P&etite', tpPetite);
  AjouterOptionTaille(SousMenuTaille, '&Moyenne', tpMoyenne);
  AjouterOptionTaille(SousMenuTaille, '&Grande', tpGrande);
  AjouterOptionTaille(SousMenuTaille, '&Très grande', tpTresGrande);
end;

procedure TForm1.AjouterOptionTaille(Parent: TMenuItem;
  const Caption: string; Taille: TTaillePolice);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Parent);
  MenuItem.Caption := Caption;
  MenuItem.Tag := Ord(Taille);
  MenuItem.RadioItem := True;
  MenuItem.GroupIndex := 1;
  MenuItem.OnClick := MenuTailleClick;
  Parent.Add(MenuItem);
end;

procedure TForm1.MenuTailleClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    ChangerTaillePolice(TTaillePolice(TMenuItem(Sender).Tag));
end;
```

---

## 4.11.7 Focus visuel

### Importance du focus visuel

Le focus visuel indique quel élément est actuellement actif lors de la navigation au clavier. Il doit être clairement visible.

### Indicateur de focus par défaut

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // S'assurer que le focus est visible
  Button1.TabStop := True;

  // Le focus par défaut utilise une bordure en pointillés
  // Cela peut être personnalisé
end;
```

### Focus personnalisé

```pascal
type
  TEditAvecFocus = class(TEdit)
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  end;

procedure TEditAvecFocus.DoEnter;
begin
  inherited;
  // Mettre en évidence le champ actif
  Color := RGB(255, 255, 200); // Jaune pâle
  Font.Style := [fsBold];
  BorderStyle := bsSingle;
end;

procedure TEditAvecFocus.DoExit;
begin
  inherited;
  // Retour à l'apparence normale
  Color := clWindow;
  Font.Style := [];
end;
```

### Ordre de focus logique

```pascal
procedure TForm1.ConfigurerOrdreLogique;
begin
  // Ordre de lecture naturel : gauche à droite, haut en bas

  // Ligne 1
  EditNom.TabOrder := 0;
  EditPrenom.TabOrder := 1;

  // Ligne 2
  EditAdresse.TabOrder := 2;
  EditVille.TabOrder := 3;
  EditCodePostal.TabOrder := 4;

  // Ligne 3
  EditTelephone.TabOrder := 5;
  EditEmail.TabOrder := 6;

  // Boutons en bas
  ButtonValider.TabOrder := 7;
  ButtonAnnuler.TabOrder := 8;
end;
```

### Focus visible même avec la souris

```pascal
procedure TForm1.Button1Enter(Sender: TObject);
begin
  // Toujours afficher un indicateur visuel du focus
  Button1.Font.Style := [fsBold];

  // Ou utiliser une bordure
  Panel1.BorderStyle := bsSingle;
  Panel1.BorderWidth := 2;
end;

procedure TForm1.Button1Exit(Sender: TObject);
begin
  Button1.Font.Style := [];
  Panel1.BorderWidth := 1;
end;
```

---

## 4.11.8 Formulaires accessibles

### Labels associés aux champs

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // TOUJOURS associer un label à chaque champ de saisie

  LabelNom.Caption := '&Nom :';
  LabelNom.FocusControl := EditNom;

  LabelPrenom.Caption := '&Prénom :';
  LabelPrenom.FocusControl := EditPrenom;

  LabelDateNaissance.Caption := '&Date de naissance :';
  LabelDateNaissance.FocusControl := DateTimePickerNaissance;

  // Avantage : Cliquer sur le label donne le focus au champ
  // Avantage : Alt+lettre fonctionne pour naviguer
  // Avantage : Lecteurs d'écran annoncent le label
end;
```

### Messages d'erreur clairs

```pascal
procedure TForm1.ValiderFormulaire;
var
  Erreurs: TStringList;
begin
  Erreurs := TStringList.Create;
  try
    // Collecter toutes les erreurs
    if Trim(EditNom.Text) = '' then
      Erreurs.Add('Le nom est obligatoire');

    if Trim(EditEmail.Text) = '' then
      Erreurs.Add('L''email est obligatoire')
    else if not EmailValide(EditEmail.Text) then
      Erreurs.Add('L''email n''est pas au bon format');

    if EditAge.Text = '' then
      Erreurs.Add('L''âge est obligatoire')
    else if StrToIntDef(EditAge.Text, 0) < 18 then
      Erreurs.Add('Vous devez avoir au moins 18 ans');

    // Afficher toutes les erreurs ensemble
    if Erreurs.Count > 0 then
    begin
      MessageDlg(
        'Veuillez corriger les erreurs suivantes :' + #13#10#13#10 +
        Erreurs.Text,
        mtWarning, [mbOK], 0
      );

      // Mettre le focus sur le premier champ en erreur
      if Trim(EditNom.Text) = '' then
        EditNom.SetFocus
      else if Trim(EditEmail.Text) = '' then
        EditEmail.SetFocus
      else if EditAge.Text = '' then
        EditAge.SetFocus;
    end
    else
    begin
      // Validation réussie
      EnregistrerFormulaire;
    end;
  finally
    Erreurs.Free;
  end;
end;
```

### Indication des champs obligatoires

```pascal
procedure TForm1.MarquerChampsObligatoires;
begin
  // Méthode 1 : Astérisque dans le label
  LabelNom.Caption := '&Nom * :';
  LabelEmail.Caption := '&Email * :';

  // Méthode 2 : Couleur du label
  LabelNom.Font.Color := clRed;

  // Méthode 3 : Icône
  ImageObligatoire1.Picture.LoadFromFile('obligatoire.png');
  ImageObligatoire1.Hint := 'Champ obligatoire';

  // Ajouter une légende
  LabelLegende.Caption := '* Champs obligatoires';
  LabelLegende.Font.Style := [fsItalic];
end;
```

### Groupes de champs liés

```pascal
procedure TForm1.CreerGroupeAdresse;
begin
  // Utiliser un GroupBox pour regrouper les champs liés
  GroupBoxAdresse := TGroupBox.Create(Self);
  GroupBoxAdresse.Parent := Self;
  GroupBoxAdresse.Caption := 'Adresse';
  GroupBoxAdresse.Left := 10;
  GroupBoxAdresse.Top := 100;
  GroupBoxAdresse.Width := 400;
  GroupBoxAdresse.Height := 150;

  // Les champs à l'intérieur sont logiquement groupés
  EditRue.Parent := GroupBoxAdresse;
  EditVille.Parent := GroupBoxAdresse;
  EditCodePostal.Parent := GroupBoxAdresse;

  // Avantage pour les lecteurs d'écran :
  // Ils annoncent "Adresse, Rue" au lieu de juste "Rue"
end;
```

---

## 4.11.9 Gestion des erreurs accessible

### Messages d'erreur descriptifs

```pascal
// Mauvais : Message vague
procedure AfficherErreurVague;
begin
  ShowMessage('Erreur !');
end;

// Bon : Message descriptif et actionnable
procedure AfficherErreurDescriptive;
begin
  ShowMessage(
    'Erreur lors de la sauvegarde' + #13#10#13#10 +
    'Le fichier "donnees.db" est en lecture seule.' + #13#10#13#10 +
    'Solutions possibles :' + #13#10 +
    '- Vérifiez que le fichier n''est pas ouvert ailleurs' + #13#10 +
    '- Vérifiez vos permissions sur le fichier' + #13#10 +
    '- Contactez l''administrateur système'
  );
end;
```

### Prévention des erreurs

```pascal
procedure TForm1.PrevenirErreursSaisie;
begin
  // Limiter la saisie
  EditAge.NumbersOnly := True;
  EditAge.MaxLength := 3;

  // Masque de saisie
  MaskEditTelephone.EditMask := '00 00 00 00 00;0; ';

  // Validation en temps réel avec aide
  EditEmail.OnChange := ValiderEmailEnTempsReel;
end;

procedure TForm1.ValiderEmailEnTempsReel(Sender: TObject);
begin
  if Length(EditEmail.Text) = 0 then
  begin
    LabelAideEmail.Caption := 'Entrez votre adresse email';
    LabelAideEmail.Font.Color := clGray;
  end
  else if not EmailValide(EditEmail.Text) then
  begin
    LabelAideEmail.Caption := '⚠ Format invalide (exemple: nom@domaine.fr)';
    LabelAideEmail.Font.Color := clRed;
  end
  else
  begin
    LabelAideEmail.Caption := '✓ Email valide';
    LabelAideEmail.Font.Color := clGreen;
  end;
end;
```

### Confirmation des actions importantes

```pascal
procedure TForm1.SupprimerElementAvecConfirmation;
var
  NomElement: string;
begin
  if ListBox1.ItemIndex = -1 then
  begin
    ShowMessage('Veuillez sélectionner un élément à supprimer.');
    ListBox1.SetFocus;
    Exit;
  end;

  NomElement := ListBox1.Items[ListBox1.ItemIndex];

  // Confirmation claire et explicite
  if MessageDlg(
    Format('Voulez-vous vraiment supprimer "%s" ?' + #13#10#13#10 +
           'Cette action est irréversible.', [NomElement]),
    mtWarning,
    [mbYes, mbNo],
    0
  ) = mrYes then
  begin
    ListBox1.Items.Delete(ListBox1.ItemIndex);
    ShowMessage(Format('"%s" a été supprimé.', [NomElement]));
  end;
end;
```

---

## 4.11.10 Tests d'accessibilité

### Checklist de test manuel

```
□ Navigation au clavier
  □ Tab parcourt tous les contrôles interactifs
  □ Ordre de tabulation logique
  □ Focus visible sur chaque élément
  □ Échap ferme les dialogues
  □ Entrée active les boutons

□ Lecteur d'écran
  □ Tous les contrôles ont un nom
  □ Les labels sont correctement associés
  □ Les erreurs sont annoncées
  □ Les changements d'état sont signalés

□ Contraste
  □ Ratio minimum de 4.5:1 pour le texte
  □ Texte lisible sur tous les fonds
  □ Pas d'information par couleur seule

□ Taille de texte
  □ Police minimum de 10pt
  □ Texte agrandissable
  □ Interface reste utilisable à 200%

□ Aide et documentation
  □ Hints sur les boutons icônes
  □ Messages d'erreur clairs
  □ Aide contextuelle (F1)
  □ Champs obligatoires indiqués
```

### Test avec clavier seul

```pascal
// Test : Pouvez-vous faire TOUT cela sans souris ?
procedure TForm1.TesterNavigationClavier;
begin
  {
    1. Ouvrir l'application
    2. Naviguer entre tous les champs avec Tab
    3. Remplir un formulaire complet
    4. Activer tous les menus (Alt+lettre)
    5. Utiliser tous les boutons (Entrée/Espace)
    6. Ouvrir et fermer tous les dialogues (Échap)
    7. Copier/coller du texte (Ctrl+C/Ctrl+V)
    8. Fermer l'application (Alt+F4)

    Si vous êtes bloqué à une étape = problème d'accessibilité
  }
end;
```

### Test avec lecteur d'écran

```pascal
// Installer NVDA (gratuit) et tester :
procedure TesterAvecNVDA;
begin
  {
    1. Lancer NVDA (Ctrl+Alt+N)
    2. Lancer votre application
    3. Naviguer avec Tab
    4. Vérifier que chaque élément est annoncé clairement
    5. Remplir un formulaire
    6. Déclencher une erreur
    7. Vérifier que l'erreur est lue correctement
    8. Fermer l'application

    Questions à se poser :
    - Comprenez-vous ce qui est lu ?
    - Pouvez-vous accomplir toutes les tâches ?
    - Les informations sont-elles dans le bon ordre ?
  }
end;
```

### Test de contraste

```pascal
procedure TForm1.TesterContrastes;
var
  i: Integer;
  Ratio: Double;
  ProblemesTrouves: TStringList;
begin
  ProblemesTrouves := TStringList.Create;
  try
    // Vérifier tous les labels
    for i := 0 to ComponentCount - 1 do
    begin
      if Components[i] is TLabel then
      begin
        with TLabel(Components[i]) do
        begin
          Ratio := CalculerRatioContraste(Font.Color, Color);

          if Ratio < 4.5 then
            ProblemesTrouves.Add(
              Format('%s : Ratio insuffisant (%.2f:1)', [Name, Ratio])
            );
        end;
      end;
    end;

    // Afficher le rapport
    if ProblemesTrouves.Count > 0 then
    begin
      ShowMessage(
        'Problèmes de contraste trouvés :' + #13#10#13#10 +
        ProblemesTrouves.Text
      );
    end
    else
      ShowMessage('Tous les contrastes sont conformes !');
  finally
    ProblemesTrouves.Free;
  end;
end;
```

---

## 4.11.11 Normes et standards

### WCAG (Web Content Accessibility Guidelines)

Bien que conçues pour le web, les WCAG sont une excellente référence pour toute interface.

**Niveaux de conformité :**
```
Niveau A : Conformité minimale
- Navigation au clavier
- Alternative textuelle
- Contraste de base

Niveau AA : Conformité recommandée (objectif standard)
- Contraste 4.5:1 pour texte
- Taille de texte ajustable
- Pas de contenu clignotant

Niveau AAA : Conformité maximale
- Contraste 7:1 pour texte
- Langue des passages identifiée
- Aide contextuelle disponible
```

### Section 508 (États-Unis)

Standards pour les technologies accessibles dans les administrations américaines.

### RGAA (France)

Référentiel Général d'Amélioration de l'Accessibilité pour les services publics français.

### EN 301 549 (Europe)

Norme européenne d'accessibilité pour les TIC (Technologies de l'Information et de la Communication).

---

## 4.11.12 Bonnes pratiques résumées

### Do's (À faire)

```
✓ Permettre la navigation au clavier complète
✓ Fournir des alternatives textuelles
✓ Assurer un contraste suffisant (minimum 4.5:1)
✓ Utiliser des polices lisibles (>10pt)
✓ Rendre le focus clairement visible
✓ Associer les labels aux champs
✓ Fournir des messages d'erreur clairs
✓ Permettre l'agrandissement du texte
✓ Utiliser des hints descriptifs
✓ Grouper logiquement les contrôles
✓ Tester avec un lecteur d'écran
✓ Documenter l'ordre de tabulation
```

### Don'ts (À éviter)

```
✗ Utiliser uniquement la couleur pour l'information
✗ Créer des pièges au clavier
✗ Utiliser des polices décoratives
✗ Coder des tailles de texte en dur
✗ Oublier les hints sur les icônes
✗ Bloquer le redimensionnement
✗ Désactiver les indicateurs de focus
✗ Utiliser des contrastes faibles
✗ Ignorer l'ordre de tabulation
✗ Messages d'erreur vagues
✗ Forcer l'utilisation de la souris
✗ Contenus clignotants excessifs
```

---

## 4.11.13 Exemple complet : Formulaire accessible

```pascal
unit FormAccessible;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics;

type
  TFormInscription = class(TForm)
    // Composants
    GroupBoxInfosPersonnelles: TGroupBox;
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelPrenom: TLabel;
    EditPrenom: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    LabelAideEmail: TLabel;

    GroupBoxPreferences: TGroupBox;
    CheckBoxNewsletter: TCheckBox;
    CheckBoxConditions: TCheckBox;

    ButtonValider: TButton;
    ButtonAnnuler: TButton;

    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditEmailChange(Sender: TObject);
    procedure ButtonValiderClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  private
    procedure ConfigurerAccessibilite;
    procedure ConfigurerOrdreTabulation;
    procedure ConfigurerRaccourcis;
    function ValiderFormulaire: Boolean;
  public
    { Déclarations publiques }
  end;

var
  FormInscription: TFormInscription;

implementation

{$R *.dfm}

procedure TFormInscription.FormCreate(Sender: TObject);
begin
  ConfigurerAccessibilite;
  ConfigurerOrdreTabulation;
  ConfigurerRaccourcis;

  // Focus initial
  EditNom.SetFocus;

  // Message de bienvenue
  StatusBar1.SimpleText := 'Remplissez le formulaire d''inscription';
end;

procedure TFormInscription.ConfigurerAccessibilite;
begin
  // Titre du formulaire descriptif
  Caption := 'Formulaire d''inscription - Tous les champs sont obligatoires';

  // Activer les hints
  ShowHint := True;

  // Configuration des labels avec FocusControl et mnémoniques
  LabelNom.Caption := '&Nom * :';
  LabelNom.FocusControl := EditNom;

  LabelPrenom.Caption := '&Prénom * :';
  LabelPrenom.FocusControl := EditPrenom;

  LabelEmail.Caption := '&Email * :';
  LabelEmail.FocusControl := EditEmail;

  // Hints descriptifs
  EditNom.Hint := 'Entrez votre nom de famille';
  EditPrenom.Hint := 'Entrez votre prénom';
  EditEmail.Hint := 'Entrez votre adresse email (format: nom@domaine.fr)';

  ButtonValider.Hint := 'Valider et enregistrer l''inscription (Ctrl+Entrée)';
  ButtonAnnuler.Hint := 'Annuler l''inscription (Échap)';

  // Texte des checkboxes clair
  CheckBoxNewsletter.Caption := 'Je souhaite recevoir la &newsletter';
  CheckBoxConditions.Caption := 'J''accepte les &conditions d''utilisation *';

  // Police lisible et taille appropriée
  Font.Name := 'Segoe UI';
  Font.Size := 10;

  // Contraste suffisant
  Color := clWhite;
  Font.Color := RGB(51, 51, 51); // Gris foncé, meilleur que noir pur

  // Légende pour les champs obligatoires
  LabelAideEmail.Caption := '* Champs obligatoires';
  LabelAideEmail.Font.Style := [fsItalic];
  LabelAideEmail.Font.Color := clGray;
end;

procedure TFormInscription.ConfigurerOrdreTabulation;
begin
  // Ordre logique de tabulation
  EditNom.TabOrder := 0;
  EditPrenom.TabOrder := 1;
  EditEmail.TabOrder := 2;
  CheckBoxNewsletter.TabOrder := 3;
  CheckBoxConditions.TabOrder := 4;
  ButtonValider.TabOrder := 5;
  ButtonAnnuler.TabOrder := 6;

  // Bouton par défaut
  ButtonValider.Default := True;
  ButtonAnnuler.Cancel := True;
end;

procedure TFormInscription.ConfigurerRaccourcis;
begin
  // Gestion globale du clavier
  KeyPreview := True;
end;

procedure TFormInscription.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Ctrl+Entrée pour valider rapidement
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
  begin
    ButtonValiderClick(ButtonValider);
    Key := 0;
  end;

  // F1 pour l'aide
  if Key = VK_F1 then
  begin
    ShowMessage(
      'Aide - Formulaire d''inscription' + #13#10#13#10 +
      'Remplissez tous les champs obligatoires (*).' + #13#10#13#10 +
      'Raccourcis clavier :' + #13#10 +
      '- Tab : Champ suivant' + #13#10 +
      '- Maj+Tab : Champ précédent' + #13#10 +
      '- Alt+lettre : Accès rapide aux champs' + #13#10 +
      '- Ctrl+Entrée : Valider' + #13#10 +
      '- Échap : Annuler' + #13#10 +
      '- F1 : Cette aide'
    );
    Key := 0;
  end;
end;

procedure TFormInscription.EditEmailChange(Sender: TObject);
begin
  // Validation en temps réel avec feedback visuel
  if Length(EditEmail.Text) = 0 then
  begin
    LabelAideEmail.Caption := '* Champ obligatoire';
    LabelAideEmail.Font.Color := clGray;
    EditEmail.Color := clWindow;
  end
  else if Pos('@', EditEmail.Text) = 0 then
  begin
    LabelAideEmail.Caption := '⚠ Email invalide (doit contenir @)';
    LabelAideEmail.Font.Color := clRed;
    EditEmail.Color := RGB(255, 240, 240);
  end
  else
  begin
    LabelAideEmail.Caption := '✓ Email valide';
    LabelAideEmail.Font.Color := clGreen;
    EditEmail.Color := RGB(240, 255, 240);
  end;
end;

function TFormInscription.ValiderFormulaire: Boolean;
var
  Erreurs: TStringList;
begin
  Result := False;
  Erreurs := TStringList.Create;
  try
    // Collecter toutes les erreurs
    if Trim(EditNom.Text) = '' then
      Erreurs.Add('- Le nom est obligatoire');

    if Trim(EditPrenom.Text) = '' then
      Erreurs.Add('- Le prénom est obligatoire');

    if Trim(EditEmail.Text) = '' then
      Erreurs.Add('- L''email est obligatoire')
    else if Pos('@', EditEmail.Text) = 0 then
      Erreurs.Add('- L''email doit contenir un @');

    if not CheckBoxConditions.Checked then
      Erreurs.Add('- Vous devez accepter les conditions d''utilisation');

    // Afficher les erreurs ou valider
    if Erreurs.Count > 0 then
    begin
      MessageDlg(
        'Veuillez corriger les erreurs suivantes :' + #13#10#13#10 +
        Erreurs.Text,
        mtWarning,
        [mbOK],
        0
      );

      // Focus sur le premier champ en erreur
      if Trim(EditNom.Text) = '' then
        EditNom.SetFocus
      else if Trim(EditPrenom.Text) = '' then
        EditPrenom.SetFocus
      else if Trim(EditEmail.Text) = '' then
        EditEmail.SetFocus
      else if not CheckBoxConditions.Checked then
        CheckBoxConditions.SetFocus;
    end
    else
      Result := True;
  finally
    Erreurs.Free;
  end;
end;

procedure TFormInscription.ButtonValiderClick(Sender: TObject);
begin
  if ValiderFormulaire then
  begin
    MessageDlg(
      'Inscription réussie !' + #13#10#13#10 +
      'Un email de confirmation a été envoyé à :' + #13#10 +
      EditEmail.Text,
      mtInformation,
      [mbOK],
      0
    );
    ModalResult := mrOk;
  end;
end;

procedure TFormInscription.ButtonAnnulerClick(Sender: TObject);
begin
  if MessageDlg(
    'Voulez-vous vraiment annuler l''inscription ?',
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes then
    ModalResult := mrCancel;
end;

end.
```

---

## Conclusion

L'accessibilité n'est pas une contrainte, c'est une opportunité de créer de meilleures applications pour tous. En suivant les principes et pratiques présentés dans ce chapitre, vous créerez des interfaces :

### Avantages d'une application accessible

✅ **Inclusive** - Utilisable par tous
✅ **Professionnelle** - Image de qualité
✅ **Légale** - Conforme aux réglementations
✅ **Utilisable** - Plus facile pour tous
✅ **Maintenable** - Mieux structurée
✅ **Performante** - Navigation optimisée
✅ **Future-proof** - Compatible avec nouvelles technologies

### Points clés à retenir

1. **Navigation au clavier** : Tout doit être accessible sans souris
2. **Contraste** : Minimum 4.5:1 pour le texte
3. **Focus visible** : L'utilisateur doit toujours savoir où il est
4. **Alternatives textuelles** : Tous les éléments doivent avoir un nom
5. **Messages clairs** : Erreurs et aides compréhensibles
6. **Tests réguliers** : Tester avec clavier et lecteur d'écran
7. **Standards** : Suivre les WCAG niveau AA minimum

L'accessibilité commence dès la conception. En l'intégrant dès le départ, vous économiserez du temps et créerez une meilleure expérience pour tous vos utilisateurs ! ♿

⏭️ [Interfaces haute résolution et prise en charge du DPI](/04-conception-dinterfaces-utilisateur-avec-la-vcl/12-interfaces-haute-resolution-et-dpi.md)
