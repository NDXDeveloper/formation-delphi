# 9.7 Rapports interactifs

## Introduction

Les rapports interactifs constituent une évolution majeure dans le domaine de la génération de rapports. Contrairement aux rapports statiques qui présentent simplement des informations, les rapports interactifs permettent aux utilisateurs d'explorer les données, de naviguer entre différentes sections, de filtrer les informations et même d'effectuer des actions directement dans le rapport.

Dans cette section, nous découvrirons comment créer des rapports interactifs avec FastReport, offrant ainsi à vos utilisateurs une expérience plus riche et personnalisée.

## Éléments d'interactivité dans les rapports

FastReport propose plusieurs façons de rendre vos rapports interactifs :

1. **Navigation entre pages** : boutons et liens pour se déplacer
2. **Hyperliens** : liens vers des sites web ou d'autres documents
3. **Signets** : navigation rapide vers des sections spécifiques
4. **Données détaillées** : affichage de détails à la demande
5. **Formulaires de saisie** : collecte de paramètres utilisateur
6. **Contenu conditionnel** : adaptation du contenu selon les interactions

## Navigation entre pages

La navigation de base permet aux utilisateurs de se déplacer facilement dans un rapport multi-pages.

```pascal
procedure TForm1.CreateNavigationReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  Band: TfrxReportTitle;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Ajouter une bande de titre
  Band := TfrxReportTitle.Create(Report);
  Page.Bands.Add(Band);
  Band.Height := 50;

  // Ajouter un titre
  Memo := TfrxMemoView.Create(Report);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'RAPPORT INTERACTIF';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Ajouter des boutons de navigation
  Memo := TfrxMemoView.Create(Report);
  Band.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 30;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'Page suivante';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'NextPage';

  Memo := TfrxMemoView.Create(Report);
  Band.Objects.Add(Memo);
  Memo.Left := 100;
  Memo.Top := 30;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'Dernière page';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'LastPage';

  // Ajouter un script pour gérer les clics
  Report.Script.Text :=
    'procedure NextPage;' + #13#10 +
    'begin' + #13#10 +
    '  Engine.GotoNextPage;' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'procedure LastPage;' + #13#10 +
    'begin' + #13#10 +
    '  Engine.GotoLastPage;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

### Ajout d'une fonctionnalité de navigation avancée

Pour une navigation plus élaborée, vous pouvez créer une barre de navigation complète :

```pascal
// Créer une bande de pied de page pour la navigation
BandFooter := TfrxPageFooter.Create(Report);
Page.Bands.Add(BandFooter);
BandFooter.Height := 30;

// Première page
Memo := TfrxMemoView.Create(Report);
BandFooter.Objects.Add(Memo);
Memo.Left := 10;
Memo.Top := 5;
Memo.Width := 20;
Memo.Height := 20;
Memo.Text := '<<';
Memo.Font.Color := clBlue;
Memo.Cursor := crHandPoint;
Memo.OnClick := 'FirstPage';

// Page précédente
Memo := TfrxMemoView.Create(Report);
BandFooter.Objects.Add(Memo);
Memo.Left := 40;
Memo.Top := 5;
Memo.Width := 20;
Memo.Height := 20;
Memo.Text := '<';
Memo.Font.Color := clBlue;
Memo.Cursor := crHandPoint;
Memo.OnClick := 'PrevPage';

// Page actuelle / Total
Memo := TfrxMemoView.Create(Report);
BandFooter.Objects.Add(Memo);
Memo.Left := 70;
Memo.Top := 5;
Memo.Width := 100;
Memo.Height := 20;
Memo.Text := 'Page [Page#] sur [TotalPages#]';
Memo.HAlign := haCenter;

// Page suivante
Memo := TfrxMemoView.Create(Report);
BandFooter.Objects.Add(Memo);
Memo.Left := 180;
Memo.Top := 5;
Memo.Width := 20;
Memo.Height := 20;
Memo.Text := '>';
Memo.Font.Color := clBlue;
Memo.Cursor := crHandPoint;
Memo.OnClick := 'NextPage';

// Dernière page
Memo := TfrxMemoView.Create(Report);
BandFooter.Objects.Add(Memo);
Memo.Left := 210;
Memo.Top := 5;
Memo.Width := 20;
Memo.Height := 20;
Memo.Text := '>>';
Memo.Font.Color := clBlue;
Memo.Cursor := crHandPoint;
Memo.OnClick := 'LastPage';
```

Ajoutez ces fonctions au script :

```pascal
Report.Script.Text :=
  'procedure FirstPage;' + #13#10 +
  'begin' + #13#10 +
  '  Engine.GotoFirstPage;' + #13#10 +
  'end;' + #13#10 +
  #13#10 +
  'procedure PrevPage;' + #13#10 +
  'begin' + #13#10 +
  '  Engine.GotoPriorPage;' + #13#10 +
  'end;' + #13#10 +
  #13#10 +
  'procedure NextPage;' + #13#10 +
  'begin' + #13#10 +
  '  Engine.GotoNextPage;' + #13#10 +
  'end;' + #13#10 +
  #13#10 +
  'procedure LastPage;' + #13#10 +
  'begin' + #13#10 +
  '  Engine.GotoLastPage;' + #13#10 +
  'end;';
```

## Hyperliens dans les rapports

Les hyperliens permettent de connecter votre rapport à des ressources externes ou à d'autres sections.

### Lien vers un site web

```pascal
// Créer un lien vers un site web
Memo := TfrxMemoView.Create(Report);
Band.Objects.Add(Memo);
Memo.Left := 10;
Memo.Top := 10;
Memo.Width := 200;
Memo.Height := 20;
Memo.Text := 'Visiter notre site web';
Memo.Font.Color := clBlue;
Memo.Font.Style := [fsUnderline];
Memo.Cursor := crHandPoint;
Memo.URL := 'http://www.example.com';
```

### Lien vers un email

```pascal
// Créer un lien email
Memo := TfrxMemoView.Create(Report);
Band.Objects.Add(Memo);
Memo.Left := 10;
Memo.Top := 40;
Memo.Width := 200;
Memo.Height := 20;
Memo.Text := 'Contacter le support';
Memo.Font.Color := clBlue;
Memo.Font.Style := [fsUnderline];
Memo.Cursor := crHandPoint;
Memo.URL := 'mailto:support@example.com?subject=Demande d''assistance';
```

### Lien vers un fichier local

```pascal
// Créer un lien vers un fichier local
Memo := TfrxMemoView.Create(Report);
Band.Objects.Add(Memo);
Memo.Left := 10;
Memo.Top := 70;
Memo.Width := 200;
Memo.Height := 20;
Memo.Text := 'Ouvrir le manuel utilisateur';
Memo.Font.Color := clBlue;
Memo.Font.Style := [fsUnderline];
Memo.Cursor := crHandPoint;
Memo.URL := 'file://C:\Manuel\Guide.pdf';
```

## Utilisation des signets (bookmarks)

Les signets permettent de naviguer rapidement vers des sections spécifiques du rapport.

### Définir des signets

```pascal
procedure TForm1.CreateReportWithBookmarks;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  BandTitle, BandHeader, BandDetail: TfrxBand;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Bande de titre
  BandTitle := TfrxReportTitle.Create(Report);
  Page.Bands.Add(BandTitle);
  BandTitle.Height := 100;

  // Titre avec table des matières
  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'RAPPORT AVEC SIGNETS';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Liens vers les sections
  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 40;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := '1. Section Clients';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'GotoClients';

  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 60;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := '2. Section Produits';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'GotoProduits';

  // Section Clients avec signet
  BandHeader := TfrxHeader.Create(Report);
  Page.Bands.Add(BandHeader);
  BandHeader.Top := 120;
  BandHeader.Height := 30;

  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := '1. SECTION CLIENTS';
  Memo.Font.Size := 12;
  Memo.Font.Style := [fsBold];
  Memo.Bookmark := 'Clients'; // Définir un signet

  // Ajouter plus de contenu pour la section Clients...

  // Section Produits avec signet
  BandHeader := TfrxHeader.Create(Report);
  Page.Bands.Add(BandHeader);
  BandHeader.Top := 300; // Position plus bas dans le rapport
  BandHeader.Height := 30;

  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := '2. SECTION PRODUITS';
  Memo.Font.Size := 12;
  Memo.Font.Style := [fsBold];
  Memo.Bookmark := 'Produits'; // Définir un signet

  // Ajouter script pour la navigation
  Report.Script.Text :=
    'procedure GotoClients;' + #13#10 +
    'begin' + #13#10 +
    '  Engine.GotoBookmark(''Clients'');' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'procedure GotoProduits;' + #13#10 +
    'begin' + #13#10 +
    '  Engine.GotoBookmark(''Produits'');' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

### Créer une table des matières automatique

FastReport peut générer automatiquement une table des matières basée sur vos signets :

```pascal
procedure TForm1.CreateTableOfContents;
var
  Report: TfrxReport;
  Page, TOCPage: TfrxReportPage;
  BandTitle, BandContent: TfrxBand;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page pour la table des matières
  TOCPage := TfrxReportPage.Create(Report);
  Report.Pages.Add(TOCPage);

  // Titre de la table des matières
  BandTitle := TfrxReportTitle.Create(Report);
  TOCPage.Bands.Add(BandTitle);
  BandTitle.Height := 40;

  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := TOCPage.Width - TOCPage.LeftMargin - TOCPage.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'TABLE DES MATIÈRES';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Contenu de la table des matières
  BandContent := TfrxBand.Create(Report);
  TOCPage.Bands.Add(BandContent);
  BandContent.Height := 20;

  // Cette bande sera automatiquement remplie avec les signets du rapport
  BandContent.Object := 'TableOfContents';

  // Créer le reste du rapport avec des signets...

  // Dans le designer, vous pouvez aussi placer un objet TfrxTableOfContents
end;
```

## Affichage de détails à la demande (drill-down)

La fonctionnalité "drill-down" permet aux utilisateurs de voir des détails supplémentaires en cliquant sur un élément.

```pascal
procedure TForm1.CreateDrillDownReport;
var
  Report: TfrxReport;
  MasterPage, DetailPage: TfrxReportPage;
  BandHeader, BandMaster, BandDetail: TfrxBand;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // Page principale avec résumé
  MasterPage := TfrxReportPage.Create(Report);
  Report.Pages.Add(MasterPage);
  MasterPage.Name := 'MasterPage';

  // Titre
  BandHeader := TfrxReportTitle.Create(Report);
  MasterPage.Bands.Add(BandHeader);
  BandHeader.Height := 40;

  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := MasterPage.Width - MasterPage.LeftMargin - MasterPage.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'RÉSUMÉ DES VENTES PAR RÉGION';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Bande maître avec données cliquables
  BandMaster := TfrxMasterData.Create(Report);
  MasterPage.Bands.Add(BandMaster);
  BandMaster.Height := 25;
  BandMaster.DataSet := frxDBDataset1; // Dataset des régions

  // Région
  Memo := TfrxMemoView.Create(Report);
  BandMaster.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := '[frxDBDataset1."Region"]';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'ShowDetails'; // Script pour afficher les détails

  // Total des ventes
  Memo := TfrxMemoView.Create(Report);
  BandMaster.Objects.Add(Memo);
  Memo.Left := 150;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := '[frxDBDataset1."TotalVentes"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // Page de détails (cachée initialement)
  DetailPage := TfrxReportPage.Create(Report);
  Report.Pages.Add(DetailPage);
  DetailPage.Name := 'DetailPage';
  DetailPage.Visible := False; // Page cachée

  // Titre de la page de détails
  BandHeader := TfrxReportTitle.Create(Report);
  DetailPage.Bands.Add(BandHeader);
  BandHeader.Height := 40;

  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := DetailPage.Width - DetailPage.LeftMargin - DetailPage.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'DÉTAILS DES VENTES POUR LA RÉGION: [Region]';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Bouton de retour
  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 30;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'Retour au résumé';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'ReturnToMaster';

  // Détails des ventes
  BandDetail := TfrxDetailData.Create(Report);
  DetailPage.Bands.Add(BandDetail);
  BandDetail.Height := 20;
  BandDetail.DataSet := frxDBDataset2; // Dataset des ventes détaillées

  // Ajouter les champs détaillés...

  // Script pour la navigation entre pages
  Report.Script.Text :=
    'var Region: String;' + #13#10 +
    #13#10 +
    'procedure ShowDetails;' + #13#10 +
    'begin' + #13#10 +
    '  // Stocker la région sélectionnée' + #13#10 +
    '  Region := <frxDBDataset1."Region">;' + #13#10 +
    #13#10 +
    '  // Filtrer les données détaillées' + #13#10 +
    '  // (à implémenter côté Delphi)' + #13#10 +
    '  Report.Variables["Region"] := Region;' + #13#10 +
    #13#10 +
    '  // Afficher la page de détails' + #13#10 +
    '  Engine.ShowBand(FindObject("DetailPage.ReportTitle1"));' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'procedure ReturnToMaster;' + #13#10 +
    'begin' + #13#10 +
    '  // Retourner à la page principale' + #13#10 +
    '  Engine.ShowBand(FindObject("MasterPage.ReportTitle1"));' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

Pour implémenter le filtrage côté Delphi :

```pascal
procedure TForm1.frxReport1GetValue(const VarName: string; var Value: Variant);
begin
  if VarName = 'Region' then
  begin
    // Utiliser la région sélectionnée pour filtrer les données détaillées
    qryVentesDetail.Close;
    qryVentesDetail.Parameters.ParamByName('Region').Value := Value;
    qryVentesDetail.Open;
  end;
end;
```

## Formulaires de saisie pour les paramètres

Les formulaires de saisie permettent aux utilisateurs de définir des paramètres avant ou pendant l'exécution du rapport.

### Dialogue de paramètres avant le rapport

```pascal
procedure TForm1.CreateParameterizedReport;
var
  Report: TfrxReport;
  DialogPage: TfrxDialogPage;
  DateFrom, DateTo: TfrxDateEditControl;
  RegionList: TfrxComboBoxControl;
  Button: TfrxButtonControl;
  Label: TfrxLabelControl;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page de dialogue
  DialogPage := TfrxDialogPage.Create(Report);
  Report.Pages.Add(DialogPage);
  DialogPage.Name := 'DialogPage';
  DialogPage.Width := 400;
  DialogPage.Height := 300;
  DialogPage.Caption := 'Paramètres du rapport';

  // Titre
  Label := TfrxLabelControl.Create(DialogPage);
  Label.Parent := DialogPage;
  Label.Left := 100;
  Label.Top := 20;
  Label.Width := 200;
  Label.Height := 24;
  Label.Caption := 'SÉLECTION DES PARAMÈTRES';
  Label.Font.Size := 12;
  Label.Font.Style := [fsBold];

  // Paramètre de date début
  Label := TfrxLabelControl.Create(DialogPage);
  Label.Parent := DialogPage;
  Label.Left := 20;
  Label.Top := 60;
  Label.Width := 100;
  Label.Height := 24;
  Label.Caption := 'Date début:';

  DateFrom := TfrxDateEditControl.Create(DialogPage);
  DateFrom.Parent := DialogPage;
  DateFrom.Left := 140;
  DateFrom.Top := 60;
  DateFrom.Width := 150;
  DateFrom.Height := 24;
  DateFrom.Date := EncodeDate(Year(Now), Month(Now), 1); // Premier jour du mois
  DateFrom.Name := 'edDateFrom';

  // Paramètre de date fin
  Label := TfrxLabelControl.Create(DialogPage);
  Label.Parent := DialogPage;
  Label.Left := 20;
  Label.Top := 100;
  Label.Width := 100;
  Label.Height := 24;
  Label.Caption := 'Date fin:';

  DateTo := TfrxDateEditControl.Create(DialogPage);
  DateTo.Parent := DialogPage;
  DateTo.Left := 140;
  DateTo.Top := 100;
  DateTo.Width := 150;
  DateTo.Height := 24;
  DateTo.Date := Now; // Date actuelle
  DateTo.Name := 'edDateTo';

  // Liste des régions
  Label := TfrxLabelControl.Create(DialogPage);
  Label.Parent := DialogPage;
  Label.Left := 20;
  Label.Top := 140;
  Label.Width := 100;
  Label.Height := 24;
  Label.Caption := 'Région:';

  RegionList := TfrxComboBoxControl.Create(DialogPage);
  RegionList.Parent := DialogPage;
  RegionList.Left := 140;
  RegionList.Top := 140;
  RegionList.Width := 150;
  RegionList.Height := 24;
  RegionList.Items.Add('Toutes');
  RegionList.Items.Add('Nord');
  RegionList.Items.Add('Sud');
  RegionList.Items.Add('Est');
  RegionList.Items.Add('Ouest');
  RegionList.ItemIndex := 0;
  RegionList.Name := 'cbRegion';

  // Boutons
  Button := TfrxButtonControl.Create(DialogPage);
  Button.Parent := DialogPage;
  Button.Left := 100;
  Button.Top := 200;
  Button.Width := 80;
  Button.Height := 30;
  Button.Caption := 'OK';
  Button.ModalResult := mrOk;
  Button.Default := True;

  Button := TfrxButtonControl.Create(DialogPage);
  Button.Parent := DialogPage;
  Button.Left := 220;
  Button.Top := 200;
  Button.Width := 80;
  Button.Height := 30;
  Button.Caption := 'Annuler';
  Button.ModalResult := mrCancel;
  Button.Cancel := True;

  // Créer le reste du rapport qui utilisera ces paramètres...

  // Script pour récupérer les valeurs
  Report.Script.Text :=
    'var DateFrom, DateTo: TDateTime;' + #13#10 +
    'var Region: String;' + #13#10 +
    #13#10 +
    'procedure DialogPage1OnActivate(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Code exécuté quand le dialogue s''affiche' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'procedure DialogPage1OnDeactivate(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer les valeurs' + #13#10 +
    '  DateFrom := edDateFrom.Date;' + #13#10 +
    '  DateTo := edDateTo.Date;' + #13#10 +
    '  Region := cbRegion.Text;' + #13#10 +
    '  ' + #13#10 +
    '  // Les stocker comme variables' + #13#10 +
    '  Report.Variables["DateFrom"] := DateFrom;' + #13#10 +
    '  Report.Variables["DateTo"] := DateTo;' + #13#10 +
    '  Report.Variables["Region"] := Region;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

### Application des paramètres côté Delphi

Dans votre application Delphi, vous pouvez récupérer les paramètres :

```pascal
procedure TForm1.frxReport1GetValue(const VarName: string; var Value: Variant);
begin
  // Récupérer les paramètres du rapport
  if VarName = 'DateFrom' then
    Value := frxReport1.Variables['DateFrom']
  else if VarName = 'DateTo' then
    Value := frxReport1.Variables['DateTo']
  else if VarName = 'Region' then
  begin
    Value := frxReport1.Variables['Region'];

    // Filtrer les données si nécessaire
    if Value <> 'Toutes' then
    begin
      qryVentes.Close;
      qryVentes.Parameters.ParamByName('Region').Value := Value;
      qryVentes.Parameters.ParamByName('DateFrom').Value := frxReport1.Variables['DateFrom'];
      qryVentes.Parameters.ParamByName('DateTo').Value := frxReport1.Variables['DateTo'];
      qryVentes.Open;
    end;
  end;
end;
```

## Contenu conditionnel dans les rapports

Une autre fonctionnalité interactive puissante est la possibilité d'afficher ou de masquer du contenu en fonction des interactions de l'utilisateur ou des paramètres sélectionnés.

### Affichage conditionnel basé sur les paramètres

```pascal
procedure TForm1.CreateConditionalReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  BandHeader, BandDetail: TfrxBand;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;

  // Supposons que le dialogue de paramètres est déjà configuré
  // et que nous avons une variable "AfficherDetails" (booléen)

  // Ajouter une bande conditionnelle pour les détails
  BandDetail := TfrxDetailData.Create(Report);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 20;
  BandDetail.DataSet := frxDBDataset1;

  // Cette bande ne sera visible que si l'utilisateur a coché "Afficher détails"
  BandDetail.Visible := False; // Par défaut, invisible

  // Script pour contrôler la visibilité
  BandDetail.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Vérifier le paramètre' + #13#10 +
    '  if <AfficherDetails> = True then' + #13#10 +
    '    Visible := True' + #13#10 +
    '  else' + #13#10 +
    '    Visible := False;' + #13#10 +
    'end;';

  // Ou au niveau d'un objet individuel
  Memo := TfrxMemoView.Create(Report);
  BandHeader.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 30;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := 'Informations détaillées';

  Memo.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // N''afficher ce texte que pour certaines régions' + #13#10 +
    '  if <Region> = ''Nord'' then' + #13#10 +
    '    Visible := True' + #13#10 +
    '  else' + #13#10 +
    '    Visible := False;' + #13#10 +
    'end;';
end;
```

### Formatage conditionnel

Le formatage conditionnel permet de modifier l'apparence des éléments selon certaines conditions :

```pascal
// Formatage conditionnel pour mettre en évidence les valeurs importantes
Memo := TfrxMemoView.Create(Report);
BandDetail.Objects.Add(Memo);
Memo.Left := 300;
Memo.Top := 0;
Memo.Width := 100;
Memo.Height := 20;
Memo.Text := '[frxDBDataset1."Montant"]';
Memo.DisplayFormat.FormatStr := '%2.2f €';
Memo.DisplayFormat.Kind := fkNumeric;
Memo.HAlign := haRight;

// Script pour le formatage conditionnel
Memo.Script :=
  'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
  'begin' + #13#10 +
  '  // Changer la couleur selon le montant' + #13#10 +
  '  if <frxDBDataset1."Montant"> > 1000 then begin' + #13#10 +
  '    Font.Color := clRed;' + #13#10 +
  '    Font.Style := [fsBold];' + #13#10 +
  '  end else if <frxDBDataset1."Montant"> < 0 then begin' + #13#10 +
  '    Font.Color := clBlue;' + #13#10 +
  '    Font.Style := [fsBold, fsItalic];' + #13#10 +
  '  end else begin' + #13#10 +
  '    Font.Color := clBlack;' + #13#10 +
  '    Font.Style := [];' + #13#10 +
  '  end;' + #13#10 +
  'end;';
```

## Graphiques interactifs dans les rapports

Les graphiques peuvent également être rendus interactifs, permettant à l'utilisateur d'explorer les données visuellement.

### Graphique cliquable pour explorer les données

```pascal
procedure TForm1.CreateInteractiveChart;
var
  Report: TfrxReport;
  MasterPage, DetailPage: TfrxReportPage;
  BandTitle, BandChart: TfrxBand;
  Chart: TfrxChartView;
begin
  Report := frxReport1;
  Report.Clear;

  // Page principale avec graphique
  MasterPage := TfrxReportPage.Create(Report);
  Report.Pages.Add(MasterPage);
  MasterPage.Name := 'MasterPage';

  // Titre
  BandTitle := TfrxReportTitle.Create(Report);
  MasterPage.Bands.Add(BandTitle);
  BandTitle.Height := 40;

  // Configurer le titre...

  // Bande pour le graphique
  BandChart := TfrxBand.Create(Report);
  MasterPage.Bands.Add(BandChart);
  BandChart.Top := 50;
  BandChart.Height := 300;

  // Ajouter un graphique
  Chart := TfrxChartView.Create(Report);
  BandChart.Objects.Add(Chart);
  Chart.Left := 10;
  Chart.Top := 0;
  Chart.Width := MasterPage.Width - MasterPage.LeftMargin - MasterPage.RightMargin - 20;
  Chart.Height := 250;
  Chart.Title.Text.Add('Ventes par région');

  // Configurer le graphique en camembert
  Chart.Chart.SeriesType := stPie;

  // Ajouter des données
  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := True;
    XSource := 'frxDBDataset1."Region"';
    YSource := 'frxDBDataset1."TotalVentes"';
    Name := 'Ventes';
  end;

  // Rendre le graphique interactif
  Chart.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'var' + #13#10 +
    '  ClickedRegion: String;' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer la région sur laquelle l''utilisateur a cliqué' + #13#10 +
    '  if Chart.CalcClick(X, Y) then begin' + #13#10 +
    '    ClickedRegion := Chart.ClickedSeries.XValue;' + #13#10 +
    '    ShowMessage(''Vous avez sélectionné la région: '' + ClickedRegion);' + #13#10 +
    '    ' + #13#10 +
    '    // Filtrer les données et afficher les détails' + #13#10 +
    '    Report.Variables["SelectedRegion"] := ClickedRegion;' + #13#10 +
    '    // Afficher une page détaillée...' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Page de détails (similaire à l'exemple précédent)...

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

## Objets Rich Text pour le contenu interactif

FastReport prend en charge les objets Rich Text qui permettent d'intégrer du contenu formaté avec des liens.

```pascal
procedure TForm1.CreateRichTextReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  BandTitle: TfrxBand;
  RichView: TfrxRichView;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Titre
  BandTitle := TfrxReportTitle.Create(Report);
  Page.Bands.Add(BandTitle);
  BandTitle.Height := 400; // Hauteur pour le contenu rich text

  // Ajouter un objet Rich Text
  RichView := TfrxRichView.Create(Report);
  BandTitle.Objects.Add(RichView);
  RichView.Left := 10;
  RichView.Top := 10;
  RichView.Width := Page.Width - Page.LeftMargin - Page.RightMargin - 20;
  RichView.Height := 380;

  // Ajouter du contenu rich text
  RichView.RichEdit.Lines.Clear;
  RichView.RichEdit.Lines.Add('{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}}');
  RichView.RichEdit.Lines.Add('{\colortbl ;\red0\green0\blue255;}');
  RichView.RichEdit.Lines.Add('\viewkind4\uc1\pard\cf0\f0\fs28\b Guide interactif des fonctionnalités\b0\fs22\par');
  RichView.RichEdit.Lines.Add('\par');
  RichView.RichEdit.Lines.Add('Ce document contient des liens interactifs vers différentes sections:\par');
  RichView.RichEdit.Lines.Add('\par');
  RichView.RichEdit.Lines.Add('\cf1\ul\fs24 1. Présentation des produits\cf0\ulnone\fs22\par');
  RichView.RichEdit.Lines.Add('\par');
  RichView.RichEdit.Lines.Add('\cf1\ul\fs24 2. Guide d''utilisation\cf0\ulnone\fs22\par');
  RichView.RichEdit.Lines.Add('\par');
  RichView.RichEdit.Lines.Add('\cf1\ul\fs24 3. Support technique\cf0\ulnone\fs22\par');
  RichView.RichEdit.Lines.Add('\par');
  RichView.RichEdit.Lines.Add('Vous pouvez également visiter notre \cf1\ul site web\cf0\ulnone pour plus d''informations.\par');
  RichView.RichEdit.Lines.Add('}');

  // Configurer les liens
  RichView.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'var' + #13#10 +
    '  Pos: Integer;' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer la position du clic dans le texte' + #13#10 +
    '  Pos := RichEdit.SelStart;' + #13#10 +
    '  ' + #13#10 +
    '  // Déterminer quelle section a été cliquée' + #13#10 +
    '  if (Pos >= 150) and (Pos <= 180) then begin' + #13#10 +
    '    // Section 1: Présentation des produits' + #13#10 +
    '    ShowMessage(''Affichage de la section Produits'');' + #13#10 +
    '    // Navigation vers cette section...' + #13#10 +
    '  end else if (Pos >= 210) and (Pos <= 230) then begin' + #13#10 +
    '    // Section 2: Guide d''utilisation' + #13#10 +
    '    ShowMessage(''Affichage du guide d''''utilisation'');' + #13#10 +
    '  end else if (Pos >= 260) and (Pos <= 280) then begin' + #13#10 +
    '    // Section 3: Support technique' + #13#10 +
    '    ShowMessage(''Affichage du support technique'');' + #13#10 +
    '  end else if (Pos >= 350) and (Pos <= 380) then begin' + #13#10 +
    '    // Lien vers le site web' + #13#10 +
    '    ShellExecute(0, ''open'', ''http://www.example.com'', '''', '''', 0);' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

## Interface utilisateur personnalisée pour les rapports

Vous pouvez créer une interface utilisateur personnalisée pour la prévisualisation des rapports.

```pascal
procedure TForm1.CreateCustomUIReport;
begin
  // 1. Créer un formulaire de prévisualisation personnalisé
  frmCustomPreview := TfrmCustomPreview.Create(Self);
  try
    // 2. Configurer le composant TfrxPreview du formulaire
    frmCustomPreview.frxPreview1.Report := frxReport1;

    // 3. Préparer le rapport
    frxReport1.PrepareReport;

    // 4. Afficher la prévisualisation personnalisée
    frmCustomPreview.ShowModal;
  finally
    frmCustomPreview.Free;
  end;
end;
```

Voici un exemple simplifié d'un formulaire de prévisualisation personnalisé :

```pascal
unit CustomPreviewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Buttons, frxClass, frxPreview;

type
  TfrmCustomPreview = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    frxPreview1: TfrxPreview;
    btnPrint: TSpeedButton;
    btnExport: TSpeedButton;
    btnZoomIn: TSpeedButton;
    btnZoomOut: TSpeedButton;
    btnFirstPage: TSpeedButton;
    btnPrevPage: TSpeedButton;
    btnNextPage: TSpeedButton;
    btnLastPage: TSpeedButton;
    edtPage: TEdit;
    lblPageCount: TLabel;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnFirstPageClick(Sender: TObject);
    procedure btnPrevPageClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
    procedure btnLastPageClick(Sender: TObject);
    procedure edtPageChange(Sender: TObject);
    procedure frxPreview1PageChanged(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure UpdatePageInfo;
  public
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

procedure TfrmCustomPreview.FormCreate(Sender: TObject);
begin
  // Configurer la prévisualisation
  frxPreview1.Align := alClient;

  // Mettre à jour l'affichage du numéro de page
  UpdatePageInfo;
end;

procedure TfrmCustomPreview.UpdatePageInfo;
begin
  // Mettre à jour le numéro de page et le nombre total de pages
  edtPage.Text := IntToStr(frxPreview1.CurPage);
  lblPageCount.Caption := 'sur ' + IntToStr(frxPreview1.PagesCount);

  // Activer/désactiver les boutons de navigation
  btnFirstPage.Enabled := frxPreview1.CurPage > 1;
  btnPrevPage.Enabled := frxPreview1.CurPage > 1;
  btnNextPage.Enabled := frxPreview1.CurPage < frxPreview1.PagesCount;
  btnLastPage.Enabled := frxPreview1.CurPage < frxPreview1.PagesCount;
end;

procedure TfrmCustomPreview.btnPrintClick(Sender: TObject);
begin
  frxPreview1.Print;
end;

procedure TfrmCustomPreview.btnExportClick(Sender: TObject);
begin
  frxPreview1.Export;
end;

procedure TfrmCustomPreview.btnZoomInClick(Sender: TObject);
begin
  frxPreview1.Zoom := frxPreview1.Zoom + 0.25;
end;

procedure TfrmCustomPreview.btnZoomOutClick(Sender: TObject);
begin
  if frxPreview1.Zoom > 0.5 then
    frxPreview1.Zoom := frxPreview1.Zoom - 0.25;
end;

procedure TfrmCustomPreview.btnFirstPageClick(Sender: TObject);
begin
  frxPreview1.First;
end;

procedure TfrmCustomPreview.btnPrevPageClick(Sender: TObject);
begin
  frxPreview1.Prior;
end;

procedure TfrmCustomPreview.btnNextPageClick(Sender: TObject);
begin
  frxPreview1.Next;
end;

procedure TfrmCustomPreview.btnLastPageClick(Sender: TObject);
begin
  frxPreview1.Last;
end;

procedure TfrmCustomPreview.edtPageChange(Sender: TObject);
var
  Page: Integer;
begin
  // Changer la page si l'utilisateur saisit un numéro valide
  if TryStrToInt(edtPage.Text, Page) then
  begin
    if (Page >= 1) and (Page <= frxPreview1.PagesCount) then
      frxPreview1.PageNo := Page;
  end;
end;

procedure TfrmCustomPreview.frxPreview1PageChanged(Sender: TObject);
begin
  // Mettre à jour l'affichage quand la page change
  UpdatePageInfo;
end;

procedure TfrmCustomPreview.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
```

## Intégration d'actions utilisateur complexes

Les rapports interactifs peuvent également inclure des actions plus complexes comme l'enregistrement de modifications ou l'envoi d'emails.

### Exemple d'un rapport de validation avec action utilisateur

```pascal
procedure TForm1.CreateActionReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  BandDetail: TfrxBand;
  Memo, Action: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // Créer une page
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Configurer les bandes et le titre...

  // Bande de détail avec actions utilisateur
  BandDetail := TfrxDetailData.Create(Report);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 25;
  BandDetail.DataSet := frxDBDataset1; // Factures en attente de validation

  // Informations de facture
  Memo := TfrxMemoView.Create(Report);
  BandDetail.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 0;
  Memo.Width := 300;
  Memo.Height := 20;
  Memo.Text := 'Facture #[frxDBDataset1."NumeroFacture"] - [frxDBDataset1."DateFacture"] - [frxDBDataset1."MontantTotal"]€';

  // Bouton "Valider"
  Action := TfrxMemoView.Create(Report);
  BandDetail.Objects.Add(Action);
  Action.Left := 350;
  Action.Top := 0;
  Action.Width := 60;
  Action.Height := 20;
  Action.Text := 'Valider';
  Action.HAlign := haCenter;
  Action.Color := clGreen;
  Action.Font.Color := clWhite;
  Action.Font.Style := [fsBold];
  Action.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Action.Frame.Width := 1;
  Action.Cursor := crHandPoint;

  // Script pour l'action de validation
  Action.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'var' + #13#10 +
    '  FactureID: Integer;' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer l''ID de la facture à valider' + #13#10 +
    '  FactureID := <frxDBDataset1."ID">;' + #13#10 +
    '  ' + #13#10 +
    '  // Confirmer l''action' + #13#10 +
    '  if MessageDlg(''Voulez-vous vraiment valider la facture #'' + <frxDBDataset1."NumeroFacture">, mtConfirmation, mbYesNo, 0) = mrYes then' + #13#10 +
    '  begin' + #13#10 +
    '    // Stocker l''ID pour traitement côté Delphi' + #13#10 +
    '    Report.Variables["ActionType"] := ''Validate'';' + #13#10 +
    '    Report.Variables["FactureID"] := FactureID;' + #13#10 +
    '    ' + #13#10 +
    '    // Déclencher l''événement OnManualBuild pour notifier l''application' + #13#10 +
    '    Engine.OnManualBuild;' + #13#10 +
    '    ' + #13#10 +
    '    // Changer l''apparence du bouton' + #13#10 +
    '    Sender.Color := clGray;' + #13#10 +
    '    Sender.Text := ''Validé'';' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Bouton "Rejeter"
  Action := TfrxMemoView.Create(Report);
  BandDetail.Objects.Add(Action);
  Action.Left := 420;
  Action.Top := 0;
  Action.Width := 60;
  Action.Height := 20;
  Action.Text := 'Rejeter';
  Action.HAlign := haCenter;
  Action.Color := clRed;
  Action.Font.Color := clWhite;
  Action.Font.Style := [fsBold];
  Action.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Action.Frame.Width := 1;
  Action.Cursor := crHandPoint;

  // Script pour l'action de rejet
  Action.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'var' + #13#10 +
    '  FactureID: Integer;' + #13#10 +
    '  Motif: String;' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer l''ID de la facture à rejeter' + #13#10 +
    '  FactureID := <frxDBDataset1."ID">;' + #13#10 +
    '  ' + #13#10 +
    '  // Demander un motif de rejet' + #13#10 +
    '  Motif := InputBox(''Rejet de facture'', ''Veuillez indiquer le motif du rejet :'', '''');' + #13#10 +
    '  ' + #13#10 +
    '  if Motif <> '''' then' + #13#10 +
    '  begin' + #13#10 +
    '    // Stocker les informations pour traitement côté Delphi' + #13#10 +
    '    Report.Variables["ActionType"] := ''Reject'';' + #13#10 +
    '    Report.Variables["FactureID"] := FactureID;' + #13#10 +
    '    Report.Variables["Motif"] := Motif;' + #13#10 +
    '    ' + #13#10 +
    '    // Déclencher l''événement' + #13#10 +
    '    Engine.OnManualBuild;' + #13#10 +
    '    ' + #13#10 +
    '    // Changer l''apparence du bouton' + #13#10 +
    '    Sender.Color := clGray;' + #13#10 +
    '    Sender.Text := ''Rejeté'';' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

Dans votre application Delphi, gérez les actions déclenchées depuis le rapport :

```pascal
procedure TForm1.frxReport1ManualBuild(Sender: TObject);
var
  ActionType: string;
  FactureID: Integer;
  Motif: string;
begin
  // Récupérer les informations de l'action
  ActionType := frxReport1.Variables['ActionType'];
  FactureID := frxReport1.Variables['FactureID'];

  if ActionType = 'Validate' then
  begin
    // Valider la facture dans la base de données
    qryValiderFacture.Close;
    qryValiderFacture.Parameters.ParamByName('ID').Value := FactureID;
    qryValiderFacture.ExecSQL;

    // Journal d'audit
    LogAction('Validation facture #' + IntToStr(FactureID));
  end
  else if ActionType = 'Reject' then
  begin
    // Récupérer le motif de rejet
    Motif := frxReport1.Variables['Motif'];

    // Rejeter la facture dans la base de données
    qryRejeterFacture.Close;
    qryRejeterFacture.Parameters.ParamByName('ID').Value := FactureID;
    qryRejeterFacture.Parameters.ParamByName('Motif').Value := Motif;
    qryRejeterFacture.ExecSQL;

    // Journal d'audit
    LogAction('Rejet facture #' + IntToStr(FactureID) + ' - Motif: ' + Motif);
  end;
end;
```

## Conseils et bonnes pratiques

### 1. Tester l'interactivité

Les rapports interactifs nécessitent plus de tests que les rapports statiques. Assurez-vous de tester toutes les fonctionnalités interactives avec différents scénarios et données.

### 2. Gestion des erreurs

Ajoutez une gestion des erreurs appropriée, surtout pour les actions qui modifient des données.

```pascal
// Exemple d'une gestion d'erreurs robuste dans un script
'try' + #13#10 +
'  // Code qui pourrait générer une erreur' + #13#10 +
'  // ...' + #13#10 +
'except' + #13#10 +
'  on E: Exception do' + #13#10 +
'  begin' + #13#10 +
'    ShowMessage(''Erreur: '' + E.Message);' + #13#10 +
'    Report.Variables["ErrorOccurred"] := True;' + #13#10 +
'  end;' + #13#10 +
'end;'
```

### 3. Privilégier la simplicité

Ne surchargez pas vos rapports avec trop d'éléments interactifs. Gardez l'interface simple et intuitive.

### 4. Fournir des instructions

Pour les rapports complexes, ajoutez des instructions claires sur la façon d'utiliser les fonctionnalités interactives.

```pascal
// Ajouter une note d'aide
Memo := TfrxMemoView.Create(Report);
BandTitle.Objects.Add(Memo);
Memo.Left := 10;
Memo.Top := 40;
Memo.Width := 400;
Memo.Height := 20;
Memo.Text := 'Note: Cliquez sur les régions du graphique pour voir les détails correspondants.';
Memo.Font.Style := [fsItalic];
Memo.Font.Color := clGray;
```

### 5. Sécurité

Si votre rapport peut effectuer des modifications de données, assurez-vous d'implémenter les contrôles de sécurité appropriés.

```pascal
// Vérifier les droits de l'utilisateur avant une action
'if <CurrentUserHasRights> = False then' + #13#10 +
'begin' + #13#10 +
'  ShowMessage(''Vous n''''avez pas les droits nécessaires pour effectuer cette action.'');' + #13#10 +
'  Exit;' + #13#10 +
'end;'
```

## Exemple complet : Tableau de bord interactif

Voici un exemple plus complet qui combine plusieurs techniques pour créer un tableau de bord interactif :

```pascal
procedure TForm1.CreateDashboardReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  BandTitle, BandFilters, BandSummary, BandCharts, BandDetails: TfrxBand;
  Memo: TfrxMemoView;
  Chart: TfrxChartView;
  DateFrom, DateTo: TfrxDateEditControl;
  RegionList: TfrxComboBoxControl;
  DetailPage: TfrxReportPage;
begin
  Report := frxReport1;
  Report.Clear;

  // Page principale du tableau de bord
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);
  Page.Name := 'DashboardPage';
  Page.Orientation := poLandscape; // Format paysage pour le tableau de bord

  // Titre du tableau de bord
  BandTitle := TfrxReportTitle.Create(Report);
  Page.Bands.Add(BandTitle);
  BandTitle.Height := 40;

  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'TABLEAU DE BORD DES VENTES';
  Memo.Font.Size := 16;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Bande pour les filtres interactifs
  BandFilters := TfrxBand.Create(Report);
  Page.Bands.Add(BandFilters);
  BandFilters.Top := 50;
  BandFilters.Height := 60;

  // Titre des filtres
  Memo := TfrxMemoView.Create(Report);
  BandFilters.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 0;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := 'Filtres:';
  Memo.Font.Style := [fsBold];

  // Filtre de date début
  Memo := TfrxMemoView.Create(Report);
  BandFilters.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 25;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'Date début:';

  DateFrom := TfrxDateEditControl.Create(Report);
  BandFilters.Objects.Add(DateFrom);
  DateFrom.Left := 100;
  DateFrom.Top := 25;
  DateFrom.Width := 100;
  DateFrom.Height := 20;
  DateFrom.Date := EncodeDate(Year(Now), Month(Now), 1); // Premier jour du mois
  DateFrom.Name := 'edDateFrom';

  // Filtre de date fin
  Memo := TfrxMemoView.Create(Report);
  BandFilters.Objects.Add(Memo);
  Memo.Left := 220;
  Memo.Top := 25;
  Memo.Width := 70;
  Memo.Height := 20;
  Memo.Text := 'Date fin:';

  DateTo := TfrxDateEditControl.Create(Report);
  BandFilters.Objects.Add(DateTo);
  DateTo.Left := 300;
  DateTo.Top := 25;
  DateTo.Width := 100;
  DateTo.Height := 20;
  DateTo.Date := Now; // Date actuelle
  DateTo.Name := 'edDateTo';

  // Filtre de région
  Memo := TfrxMemoView.Create(Report);
  BandFilters.Objects.Add(Memo);
  Memo.Left := 420;
  Memo.Top := 25;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'Région:';

  RegionList := TfrxComboBoxControl.Create(Report);
  BandFilters.Objects.Add(RegionList);
  RegionList.Left := 490;
  RegionList.Top := 25;
  RegionList.Width := 120;
  RegionList.Height := 20;
  RegionList.Items.Add('Toutes');
  RegionList.Items.Add('Nord');
  RegionList.Items.Add('Sud');
  RegionList.Items.Add('Est');
  RegionList.Items.Add('Ouest');
  RegionList.ItemIndex := 0;
  RegionList.Name := 'cbRegion';

  // Bouton "Appliquer les filtres"
  Memo := TfrxMemoView.Create(Report);
  BandFilters.Objects.Add(Memo);
  Memo.Left := 630;
  Memo.Top := 25;
  Memo.Width := 100;
  Memo.Height := 25;
  Memo.Text := 'Appliquer';
  Memo.Color := clBtnFace;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;
  Memo.VAlign := vaCenter;
  Memo.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Memo.Cursor := crHandPoint;

  // Script pour le bouton "Appliquer"
  Memo.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer les valeurs des filtres' + #13#10 +
    '  Report.Variables["DateFrom"] := edDateFrom.Date;' + #13#10 +
    '  Report.Variables["DateTo"] := edDateTo.Date;' + #13#10 +
    '  Report.Variables["Region"] := cbRegion.Text;' + #13#10 +
    '  ' + #13#10 +
    '  // Notifier l''application pour mettre à jour les données' + #13#10 +
    '  Report.Variables["ApplyFilters"] := True;' + #13#10 +
    '  Engine.OnManualBuild;' + #13#10 +
    '  ' + #13#10 +
    '  // Actualiser l''affichage' + #13#10 +
    '  Engine.ShowBand(FindObject("DashboardPage.BandSummary"));' + #13#10 +
    '  Engine.ShowBand(FindObject("DashboardPage.BandCharts"));' + #13#10 +
    'end;';

  // Bande pour les indicateurs de synthèse
  BandSummary := TfrxBand.Create(Report);
  Page.Bands.Add(BandSummary);
  BandSummary.Name := 'BandSummary';
  BandSummary.Top := 120;
  BandSummary.Height := 80;

  // Indicateur 1: Total des ventes
  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 10;
  Memo.Width := 160;
  Memo.Height := 60;
  Memo.Color := $00E0FFE0; // Vert clair
  Memo.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Memo.Frame.Width := 1;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 15;
  Memo.Top := 15;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'VENTES TOTALES';
  Memo.Font.Style := [fsBold];
  Memo.Font.Size := 10;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 15;
  Memo.Top := 35;
  Memo.Width := 150;
  Memo.Height := 30;
  Memo.Text := '[TotalVentes]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.Cursor := crHandPoint;

  // Script pour afficher les détails des ventes
  Memo.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'begin' + #13#10 +
    '  ShowMessage(''Affichage des détails des ventes totales'');' + #13#10 +
    '  Report.Variables["DetailType"] := ''Ventes'';' + #13#10 +
    '  Engine.ShowBand(FindObject("DetailPage.ReportTitle1"));' + #13#10 +
    'end;';

  // Indicateur 2: Nombre de clients
  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 180;
  Memo.Top := 10;
  Memo.Width := 160;
  Memo.Height := 60;
  Memo.Color := $00E0E0FF; // Bleu clair
  Memo.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Memo.Frame.Width := 1;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 185;
  Memo.Top := 15;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'CLIENTS ACTIFS';
  Memo.Font.Style := [fsBold];
  Memo.Font.Size := 10;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 185;
  Memo.Top := 35;
  Memo.Width := 150;
  Memo.Height := 30;
  Memo.Text := '[NbClients]';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.Cursor := crHandPoint;

  // Script pour afficher les détails des clients
  Memo.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'begin' + #13#10 +
    '  ShowMessage(''Affichage de la liste des clients'');' + #13#10 +
    '  Report.Variables["DetailType"] := ''Clients'';' + #13#10 +
    '  Engine.ShowBand(FindObject("DetailPage.ReportTitle1"));' + #13#10 +
    'end;';

  // Indicateur 3: Produits vendus
  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 350;
  Memo.Top := 10;
  Memo.Width := 160;
  Memo.Height := 60;
  Memo.Color := $00FFE0E0; // Rouge clair
  Memo.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Memo.Frame.Width := 1;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 355;
  Memo.Top := 15;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'PRODUITS VENDUS';
  Memo.Font.Style := [fsBold];
  Memo.Font.Size := 10;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 355;
  Memo.Top := 35;
  Memo.Width := 150;
  Memo.Height := 30;
  Memo.Text := '[NbProduits]';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.Cursor := crHandPoint;

  // Script pour afficher les détails des produits
  Memo.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'begin' + #13#10 +
    '  ShowMessage(''Affichage des détails des produits'');' + #13#10 +
    '  Report.Variables["DetailType"] := ''Produits'';' + #13#10 +
    '  Engine.ShowBand(FindObject("DetailPage.ReportTitle1"));' + #13#10 +
    'end;';

  // Indicateur 4: Marge moyenne
  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 520;
  Memo.Top := 10;
  Memo.Width := 160;
  Memo.Height := 60;
  Memo.Color := $00FFFFE0; // Jaune clair
  Memo.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Memo.Frame.Width := 1;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 525;
  Memo.Top := 15;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'MARGE MOYENNE';
  Memo.Font.Style := [fsBold];
  Memo.Font.Size := 10;

  Memo := TfrxMemoView.Create(Report);
  BandSummary.Objects.Add(Memo);
  Memo.Left := 525;
  Memo.Top := 35;
  Memo.Width := 150;
  Memo.Height := 30;
  Memo.Text := '[MargeAvg]%';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.Cursor := crHandPoint;

  // Bande pour les graphiques
  BandCharts := TfrxBand.Create(Report);
  Page.Bands.Add(BandCharts);
  BandCharts.Name := 'BandCharts';
  BandCharts.Top := 210;
  BandCharts.Height := 300;

  // Graphique 1: Ventes par région (camembert)
  Chart := TfrxChartView.Create(Report);
  BandCharts.Objects.Add(Chart);
  Chart.Left := 10;
  Chart.Top := 10;
  Chart.Width := 320;
  Chart.Height := 280;
  Chart.Title.Text.Add('Ventes par région');

  // Configurer le graphique camembert
  Chart.Chart.SeriesType := stPie;

  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := True;
    XSource := 'frxDBDataset1."Region"';
    YSource := 'frxDBDataset1."TotalVentes"';
    Name := 'Ventes';
  end;

  // Rendre le graphique interactif
  Chart.Script :=
    'procedure OnClick(Sender: TfrxComponent; Button: TMouseButton; Shift: Integer; X, Y: Integer);' + #13#10 +
    'var' + #13#10 +
    '  ClickedRegion: String;' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer la région cliquée' + #13#10 +
    '  if Chart.CalcClick(X, Y) then begin' + #13#10 +
    '    ClickedRegion := Chart.ClickedSeries.XValue;' + #13#10 +
    '    ' + #13#10 +
    '    // Filtrer par cette région' + #13#10 +
    '    cbRegion.Text := ClickedRegion;' + #13#10 +
    '    ' + #13#10 +
    '    // Appliquer le filtre' + #13#10 +
    '    Report.Variables["Region"] := ClickedRegion;' + #13#10 +
    '    Report.Variables["ApplyFilters"] := True;' + #13#10 +
    '    Engine.OnManualBuild;' + #13#10 +
    '    ' + #13#10 +
    '    // Actualiser l''affichage' + #13#10 +
    '    Engine.ShowBand(FindObject("DashboardPage.BandSummary"));' + #13#10 +
    '    Engine.ShowBand(FindObject("DashboardPage.BandCharts"));' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Graphique 2: Évolution des ventes (courbe)
  Chart := TfrxChartView.Create(Report);
  BandCharts.Objects.Add(Chart);
  Chart.Left := 340;
  Chart.Top := 10;
  Chart.Width := 340;
  Chart.Height := 280;
  Chart.Title.Text.Add('Évolution des ventes');

  // Configurer le graphique en courbe
  Chart.Chart.SeriesType := stLine;

  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := False;
    Color := clRed;
    XSource := 'frxDBDataset2."Mois"';
    YSource := 'frxDBDataset2."MontantVentes"';
    Name := 'Ventes';
  end;

  // Page pour les détails (cachée initialement)
  DetailPage := TfrxReportPage.Create(Report);
  Report.Pages.Add(DetailPage);
  DetailPage.Name := 'DetailPage';
  DetailPage.Visible := False;

  // Titre de la page de détails
  BandTitle := TfrxReportTitle.Create(Report);
  DetailPage.Bands.Add(BandTitle);
  BandTitle.Height := 50;

  // Titre dynamique selon le type de détail
  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := DetailPage.Width - DetailPage.LeftMargin - DetailPage.RightMargin;
  Memo.Height := 30;
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Script pour définir le titre dynamiquement
  Memo.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  if <DetailType> = ''Ventes'' then' + #13#10 +
    '    Text := ''DÉTAIL DES VENTES'';' + #13#10 +
    '  else if <DetailType> = ''Clients'' then' + #13#10 +
    '    Text := ''LISTE DES CLIENTS'';' + #13#10 +
    '  else if <DetailType> = ''Produits'' then' + #13#10 +
    '    Text := ''DÉTAIL DES PRODUITS VENDUS'';' + #13#10 +
    'end;';

  // Bouton de retour au tableau de bord
  Memo := TfrxMemoView.Create(Report);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 10;
  Memo.Top := 30;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'Retour au tableau de bord';
  Memo.Font.Color := clBlue;
  Memo.Font.Style := [fsUnderline];
  Memo.Cursor := crHandPoint;
  Memo.OnClick := 'ReturnToDashboard';

  // Bande de détail (contenu dynamique selon le type)
  BandDetails := TfrxDetailData.Create(Report);
  DetailPage.Bands.Add(BandDetails);
  BandDetails.Height := 20;

  // Script pour définir le dataset et le contenu selon le type
  BandDetails.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  if <DetailType> = ''Ventes'' then' + #13#10 +
    '    DataSet := frxDBDataset3' + #13#10 +
    '  else if <DetailType> = ''Clients'' then' + #13#10 +
    '    DataSet := frxDBDataset4' + #13#10 +
    '  else if <DetailType> = ''Produits'' then' + #13#10 +
    '    DataSet := frxDBDataset5;' + #13#10 +
    'end;';

  // Ajouter les champs pour chaque type de détail...
  // (Code omis pour concision)

  // Script global pour gérer la navigation
  Report.Script.Text :=
    'procedure ReturnToDashboard;' + #13#10 +
    'begin' + #13#10 +
    '  Engine.ShowBand(FindObject("DashboardPage.ReportTitle1"));' + #13#10 +
    'end;';

  // Initialiser les variables
  Report.Variables['TotalVentes'] := 0;
  Report.Variables['NbClients'] := 0;
  Report.Variables['NbProduits'] := 0;
  Report.Variables['MargeAvg'] := 0;

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

Pour que ce tableau de bord fonctionne, vous devez implémenter l'événement `OnManualBuild` dans votre application Delphi :

```pascal
procedure TForm1.frxReport1ManualBuild(Sender: TObject);
begin
  if frxReport1.Variables['ApplyFilters'] = True then
  begin
    // Récupérer les paramètres de filtrage
    DateFrom := frxReport1.Variables['DateFrom'];
    DateTo := frxReport1.Variables['DateTo'];
    Region := frxReport1.Variables['Region'];

    // Mettre à jour les données
    RefreshDashboardData(DateFrom, DateTo, Region);

    // Réinitialiser le flag
    frxReport1.Variables['ApplyFilters'] := False;
  end;
end;

procedure TForm1.RefreshDashboardData(DateFrom, DateTo: TDateTime; const Region: string);
begin
  // Filtrer les requêtes selon les paramètres
  qryVentesSummary.Close;
  qryVentesSummary.Parameters.ParamByName('DateFrom').Value := DateFrom;
  qryVentesSummary.Parameters.ParamByName('DateTo').Value := DateTo;

  if Region <> 'Toutes' then
    qryVentesSummary.Parameters.ParamByName('Region').Value := Region
  else
    qryVentesSummary.Parameters.ParamByName('Region').Value := Null;

  qryVentesSummary.Open;

  // Mettre à jour les indicateurs de synthèse
  frxReport1.Variables['TotalVentes'] := qryVentesSummary.FieldByName('TotalVentes').AsFloat;
  frxReport1.Variables['NbClients'] := qryVentesSummary.FieldByName('NbClients').AsInteger;
  frxReport1.Variables['NbProduits'] := qryVentesSummary.FieldByName('NbProduits').AsInteger;
  frxReport1.Variables['MargeAvg'] := qryVentesSummary.FieldByName('MargeAvg').AsFloat;

  // Mettre à jour les autres datasets pour les graphiques
  qryVentesParRegion.Close;
  qryVentesParRegion.Parameters.ParamByName('DateFrom').Value := DateFrom;
  qryVentesParRegion.Parameters.ParamByName('DateTo').Value := DateTo;
  if Region <> 'Toutes' then
    qryVentesParRegion.Parameters.ParamByName('Region').Value := Region
  else
    qryVentesParRegion.Parameters.ParamByName('Region').Value := Null;
  qryVentesParRegion.Open;

  qryVentesEvolution.Close;
  qryVentesEvolution.Parameters.ParamByName('DateFrom').Value := DateFrom;
  qryVentesEvolution.Parameters.ParamByName('DateTo').Value := DateTo;
  if Region <> 'Toutes' then
    qryVentesEvolution.Parameters.ParamByName('Region').Value := Region
  else
    qryVentesEvolution.Parameters.ParamByName('Region').Value := Null;
  qryVentesEvolution.Open;
end;
```

## Conclusion

Les rapports interactifs représentent une avancée majeure par rapport aux rapports statiques traditionnels. Ils offrent :

1. **Exploration des données** : Les utilisateurs peuvent naviguer, filtrer et explorer les informations selon leurs besoins.

2. **Personnalisation** : Chaque utilisateur peut adapter le rapport à ses propres préférences et centres d'intérêt.

3. **Productivité** : Les actions intégrées permettent d'effectuer des opérations directement depuis le rapport.

4. **Expérience utilisateur** : Une interface plus agréable et engageante qui améliore la compréhension des données.

FastReport offre un ensemble complet d'outils pour créer des rapports interactifs, notamment :

- Navigation par liens et signets
- Dialogue de paramètres
- Affichage conditionnel
- Actions utilisateur
- Composants interactifs (contrôles)
- Graphiques cliquables

En combinant ces techniques, vous pouvez transformer vos rapports statiques en véritables applications de reporting interactives, permettant aux utilisateurs d'analyser les données plus efficacement et de prendre de meilleures décisions métier.

Dans la prochaine section, nous explorerons les graphiques et tableaux de bord avec TeeChart, pour enrichir encore davantage vos visualisations de données dans Delphi.
