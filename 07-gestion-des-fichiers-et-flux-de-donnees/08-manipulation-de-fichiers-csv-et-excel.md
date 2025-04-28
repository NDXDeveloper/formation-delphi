# 7.8 Manipulation de fichiers CSV et Excel

Les fichiers CSV (Comma-Separated Values) et Excel sont couramment utilisés pour stocker et échanger des données tabulaires. Delphi offre plusieurs méthodes pour manipuler ces formats, que ce soit avec des composants natifs ou des bibliothèques tierces. Ce chapitre vous guidera à travers les différentes approches pour travailler avec ces formats.

## 7.8.1 Manipulation de fichiers CSV

Les fichiers CSV sont simples mais puissants pour échanger des données. Ils stockent des informations tabulaires sous forme de texte, avec des valeurs séparées par des délimiteurs (généralement des virgules).

### Lecture d'un fichier CSV avec les méthodes de base

Pour les fichiers CSV simples, vous pouvez utiliser les classes de base de Delphi :

```pascal
uses
  System.Classes, System.SysUtils;

procedure LireCSVSimple(const NomFichier: string);
var
  Fichier: TStringList;
  Ligne: string;
  Colonnes: TArray<string>;
  i, j: Integer;
begin
  Fichier := TStringList.Create;
  try
    // Chargement du fichier
    Fichier.LoadFromFile(NomFichier);

    // Parcours des lignes
    for i := 0 to Fichier.Count - 1 do
    begin
      Ligne := Fichier[i];

      // Séparation des valeurs (en supposant une virgule comme séparateur)
      Colonnes := Ligne.Split([',']);

      // Affichage des valeurs
      for j := 0 to Length(Colonnes) - 1 do
      begin
        Memo1.Lines.Add(Format('Ligne %d, Colonne %d: %s', [i+1, j+1, Colonnes[j]]));
      end;
    end;
  finally
    Fichier.Free;
  end;
end;
```

### Gestion des particularités des CSV

Les fichiers CSV peuvent présenter des défis :
- Délimiteurs variables (virgule, point-virgule, tabulation...)
- Guillemets pour les champs contenant des délimiteurs
- Caractères d'échappement
- Encodages différents

Voici un exemple plus robuste :

```pascal
procedure LireCSVAvance(const NomFichier: string; const Delimiteur: Char);
var
  Fichier: TStringList;
  Ligne: string;
  Position, Debut: Integer;
  Valeur: string;
  DansChamp: Boolean;
  i: Integer;
  Valeurs: TStringList;
begin
  Fichier := TStringList.Create;
  Valeurs := TStringList.Create;
  try
    // Chargement du fichier avec encodage UTF-8
    Fichier.LoadFromFile(NomFichier, TEncoding.UTF8);

    // Parcours des lignes
    for i := 0 to Fichier.Count - 1 do
    begin
      Ligne := Fichier[i];
      Valeurs.Clear;

      // Algorithme de parsing tenant compte des guillemets
      Position := 1;
      Debut := 1;
      DansChamp := False;

      while Position <= Length(Ligne) do
      begin
        // Gestion des champs entre guillemets
        if Ligne[Position] = '"' then
        begin
          DansChamp := not DansChamp;
        end
        // Si on trouve un délimiteur et qu'on n'est pas dans un champ entre guillemets
        else if (Ligne[Position] = Delimiteur) and (not DansChamp) then
        begin
          Valeur := Copy(Ligne, Debut, Position - Debut);
          // Suppression des guillemets si présents
          if (Length(Valeur) >= 2) and (Valeur[1] = '"') and (Valeur[Length(Valeur)] = '"') then
            Valeur := Copy(Valeur, 2, Length(Valeur) - 2);
          Valeurs.Add(Valeur);
          Debut := Position + 1;
        end;

        Inc(Position);
      end;

      // Traitement de la dernière valeur
      Valeur := Copy(Ligne, Debut, Position - Debut);
      if (Length(Valeur) >= 2) and (Valeur[1] = '"') and (Valeur[Length(Valeur)] = '"') then
        Valeur := Copy(Valeur, 2, Length(Valeur) - 2);
      Valeurs.Add(Valeur);

      // Traitement de la ligne
      Memo1.Lines.Add(Format('Ligne %d: %s', [i+1, Valeurs.CommaText]));
    end;
  finally
    Valeurs.Free;
    Fichier.Free;
  end;
end;
```

### Utilisation de la bibliothèque CsvDocument

Bien que Delphi n'intègre pas nativement une bibliothèque dédiée aux CSV, plusieurs bibliothèques tierces sont disponibles. L'une des plus populaires est `CsvDocument`, qui simplifie grandement la manipulation des CSV :

```pascal
// Nécessite l'installation du composant CsvDocument
uses
  CsvDocument;

procedure UtiliserCsvDocument;
var
  CSV: TCsvDocument;
  i, j: Integer;
begin
  CSV := TCsvDocument.Create;
  try
    // Configuration (point-virgule comme séparateur)
    CSV.Delimiter := ';';

    // Chargement du fichier
    CSV.LoadFromFile('donnees.csv');

    // Accès aux données
    Memo1.Lines.Add(Format('Nombre de lignes: %d', [CSV.RowCount]));
    Memo1.Lines.Add(Format('Nombre de colonnes: %d', [CSV.ColCount]));

    // Affichage de l'en-tête (première ligne)
    Memo1.Lines.Add('En-têtes:');
    for j := 0 to CSV.ColCount - 1 do
      Memo1.Lines.Add(Format('  Colonne %d: %s', [j+1, CSV.Cells[j, 0]]));

    // Parcours et affichage des données
    for i := 1 to CSV.RowCount - 1 do
    begin
      Memo1.Lines.Add(Format('Ligne %d:', [i]));
      for j := 0 to CSV.ColCount - 1 do
        Memo1.Lines.Add(Format('  %s: %s', [CSV.Cells[j, 0], CSV.Cells[j, i]]));
    end;

    // Modification et sauvegarde
    CSV.Cells[1, 2] := 'Nouvelle valeur';
    CSV.SaveToFile('donnees_modifiees.csv');
  finally
    CSV.Free;
  end;
end;
```

### Écriture d'un fichier CSV

La création d'un fichier CSV est relativement simple :

```pascal
procedure EcrireCSV(const NomFichier: string);
var
  Fichier: TStringList;
  Ligne: string;
begin
  Fichier := TStringList.Create;
  try
    // Création de l'en-tête
    Fichier.Add('Nom,Prénom,Âge,Ville');

    // Ajout de données
    Fichier.Add('Dupont,Jean,42,Paris');
    Fichier.Add('Martin,Sophie,35,Lyon');
    Fichier.Add('"Durand","Pierre","28","Aix-en-Provence"');

    // Sauvegarde du fichier
    Fichier.SaveToFile(NomFichier, TEncoding.UTF8);

    ShowMessage('Fichier CSV créé avec succès !');
  finally
    Fichier.Free;
  end;
end;
```

### Utilisation avec les bases de données

Les fichiers CSV sont souvent utilisés pour importer ou exporter des données de bases de données :

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Param;

procedure ExporterTableVersCSV(const Table: TFDTable; const NomFichier: string);
var
  Fichier: TStringList;
  Ligne: TStringBuilder;
  i: Integer;
begin
  Fichier := TStringList.Create;
  try
    // Ouverture de la table
    Table.Open;

    // Création de l'en-tête
    Ligne := TStringBuilder.Create;
    try
      for i := 0 to Table.FieldCount - 1 do
      begin
        if i > 0 then
          Ligne.Append(',');
        // Ajouter des guillemets pour éviter les problèmes avec les noms de champs
        Ligne.Append('"').Append(Table.Fields[i].FieldName).Append('"');
      end;
      Fichier.Add(Ligne.ToString);

      // Parcours des enregistrements
      Table.First;
      while not Table.Eof do
      begin
        Ligne.Clear;
        for i := 0 to Table.FieldCount - 1 do
        begin
          if i > 0 then
            Ligne.Append(',');

          // Formatage selon le type de champ
          case Table.Fields[i].DataType of
            ftString, ftWideString, ftMemo, ftWideMemo:
              // Échapper les guillemets et entourer de guillemets
              Ligne.Append('"').Append(StringReplace(Table.Fields[i].AsString, '"', '""', [rfReplaceAll])).Append('"');
            ftDate:
              Ligne.Append('"').Append(FormatDateTime('yyyy-mm-dd', Table.Fields[i].AsDateTime)).Append('"');
            ftDateTime:
              Ligne.Append('"').Append(FormatDateTime('yyyy-mm-dd hh:nn:ss', Table.Fields[i].AsDateTime)).Append('"');
            else
              Ligne.Append(Table.Fields[i].AsString);
          end;
        end;
        Fichier.Add(Ligne.ToString);
        Table.Next;
      end;
    finally
      Ligne.Free;
    end;

    // Sauvegarde du fichier
    Fichier.SaveToFile(NomFichier, TEncoding.UTF8);

    ShowMessage('Exportation terminée avec succès !');
  finally
    Fichier.Free;
  end;
end;
```

## 7.8.2 Manipulation de fichiers Excel

Delphi offre plusieurs possibilités pour travailler avec des fichiers Excel, depuis l'automatisation OLE jusqu'aux bibliothèques dédiées.

### Utilisation de la bibliothèque XLSX (recommandée)

La bibliothèque XLSX (disponible via GetIt Package Manager) est une solution moderne et performante pour manipuler les fichiers Excel sans nécessiter Excel installé :

```pascal
// Nécessite l'installation du composant XLSX
uses
  XLSFile, XLSWorkbook, XLSSheets;

procedure ManipulerExcel;
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  Row, Col: Integer;
begin
  // Création d'un nouveau classeur
  Workbook := TXLSWorkbook.Create;
  try
    // Création d'une feuille de calcul
    Sheet := Workbook.AddSheet('Données');

    // Ajout d'en-têtes
    Sheet.WriteText(0, 0, 'Nom');
    Sheet.WriteText(0, 1, 'Prénom');
    Sheet.WriteText(0, 2, 'Âge');
    Sheet.WriteText(0, 3, 'Ville');

    // Mise en forme des en-têtes (gras, fond gris)
    for Col := 0 to 3 do
    begin
      Sheet.SetCellFontBold(0, Col, True);
      Sheet.SetCellColor(0, Col, $EEEEEE);
    end;

    // Ajout de données
    Sheet.WriteText(1, 0, 'Dupont');
    Sheet.WriteText(1, 1, 'Jean');
    Sheet.WriteNumber(1, 2, 42);
    Sheet.WriteText(1, 3, 'Paris');

    Sheet.WriteText(2, 0, 'Martin');
    Sheet.WriteText(2, 1, 'Sophie');
    Sheet.WriteNumber(2, 2, 35);
    Sheet.WriteText(2, 3, 'Lyon');

    // Ajustement automatique des colonnes
    for Col := 0 to 3 do
      Sheet.AutofitColumn(Col);

    // Sauvegarde du fichier
    Workbook.SaveAs('donnees.xlsx');

    ShowMessage('Fichier Excel créé avec succès !');
  finally
    Workbook.Free;
  end;
end;
```

### Lecture d'un fichier Excel existant

```pascal
procedure LireExcel(const NomFichier: string);
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  Row, Col: Integer;
  RowCount, ColCount: Integer;
begin
  Workbook := TXLSWorkbook.Create;
  try
    // Chargement du fichier
    Workbook.LoadFromFile(NomFichier);

    // Accès à la première feuille
    if Workbook.SheetCount > 0 then
    begin
      Sheet := Workbook.GetSheet(0);

      Memo1.Lines.Add('Nom de la feuille: ' + Sheet.Name);

      // Détermination des dimensions utilisées
      RowCount := Sheet.GetLastRow;
      ColCount := Sheet.GetLastCol;

      Memo1.Lines.Add(Format('Dimensions: %d lignes x %d colonnes', [RowCount, ColCount]));

      // Lecture des en-têtes
      Memo1.Lines.Add('En-têtes:');
      for Col := 0 to ColCount do
        Memo1.Lines.Add(Format('  Colonne %d: %s', [Col+1, Sheet.ReadText(0, Col)]));

      // Lecture des données
      for Row := 1 to RowCount do
      begin
        Memo1.Lines.Add(Format('Ligne %d:', [Row]));
        for Col := 0 to ColCount do
        begin
          // Conversion selon le type de cellule
          case Sheet.GetCellType(Row, Col) of
            cellNumber:
              Memo1.Lines.Add(Format('  %s: %f', [Sheet.ReadText(0, Col), Sheet.ReadNumber(Row, Col)]));
            cellBoolean:
              Memo1.Lines.Add(Format('  %s: %s', [Sheet.ReadText(0, Col), BoolToStr(Sheet.ReadBool(Row, Col), True)]));
            cellDate:
              Memo1.Lines.Add(Format('  %s: %s', [Sheet.ReadText(0, Col),
                              FormatDateTime('dd/mm/yyyy', Sheet.ReadDateTime(Row, Col))]));
            else // cellText, cellBlank, etc.
              Memo1.Lines.Add(Format('  %s: %s', [Sheet.ReadText(0, Col), Sheet.ReadText(Row, Col)]));
          end;
        end;
      end;
    end;
  finally
    Workbook.Free;
  end;
end;
```

### Fonctionnalités avancées

La bibliothèque XLSX offre également des fonctionnalités avancées :

```pascal
procedure FonctionsAvanceesExcel;
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
begin
  Workbook := TXLSWorkbook.Create;
  try
    Sheet := Workbook.AddSheet('Données');

    // Ajout de données
    for var Row := 0 to 9 do
      for var Col := 0 to 4 do
        Sheet.WriteNumber(Row, Col, Random(100));

    // Ajout d'une formule de somme
    Sheet.WriteFormula(10, 0, 'SUM(A1:A10)');
    Sheet.WriteText(10, 1, 'Total colonne A');

    // Création d'un graphique
    var Chart := Sheet.AddChart(TXLSChartType.ctBarClustered, 12, 0, 20, 10);
    Chart.AddSeries('Données', 'Données!A1:A10', 'Données!B1:B10');
    Chart.Title := 'Graphique des données';

    // Création d'un tableau croisé dynamique (PivotTable)
    var PivotSheet := Workbook.AddSheet('PivotTable');
    var PivotTable := PivotSheet.AddPivotTable('Données!A1:E10', 1, 1);
    PivotTable.AddRowField(0); // Première colonne comme champ de ligne
    PivotTable.AddDataField(1, 'Somme', TXLSPivotTableFunction.ptfSum); // Somme de la deuxième colonne

    // Mise en forme conditionnelle
    var Rule := Sheet.AddConditionalFormatting('A1:E10');
    Rule.AddRule(TXLSCFType.cftCellValue, TXLSCFOperator.cfoGreaterThan, 80)
        .SetFillColor($AAAAFF); // Fond bleu pour les valeurs > 80

    // Protection de certaines cellules
    Sheet.ProtectCells('A1:E10', True);
    Sheet.SetSheetProtection(True, ['sort', 'autofilter']); // Autoriser le tri et les filtres

    // Sauvegarde avec mot de passe
    Workbook.SaveAs('donnees_avancees.xlsx', 'motdepasse');

    ShowMessage('Fichier Excel avancé créé avec succès !');
  finally
    Workbook.Free;
  end;
end;
```

### Automatisation avec Microsoft Excel (COM)

Si Excel est installé sur l'ordinateur, vous pouvez également utiliser l'automatisation COM :

```pascal
uses
  ComObj, Variants;

procedure AutomatiserExcel;
var
  Excel, Workbook, Sheet, Range: Variant;
begin
  // Création d'une instance d'Excel
  Excel := CreateOleObject('Excel.Application');
  try
    // Rendre Excel visible (optionnel)
    Excel.Visible := True;

    // Création d'un nouveau classeur
    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    // Définition du nom de la feuille
    Sheet.Name := 'Données';

    // Ajout d'en-têtes
    Sheet.Range['A1'].Value := 'Nom';
    Sheet.Range['B1'].Value := 'Prénom';
    Sheet.Range['C1'].Value := 'Âge';
    Sheet.Range['D1'].Value := 'Ville';

    // Mise en forme des en-têtes
    Range := Sheet.Range['A1:D1'];
    Range.Font.Bold := True;
    Range.Interior.Color := RGB(240, 240, 240);

    // Ajout de données
    Sheet.Cells[2, 1].Value := 'Dupont';
    Sheet.Cells[2, 2].Value := 'Jean';
    Sheet.Cells[2, 3].Value := 42;
    Sheet.Cells[2, 4].Value := 'Paris';

    Sheet.Cells[3, 1].Value := 'Martin';
    Sheet.Cells[3, 2].Value := 'Sophie';
    Sheet.Cells[3, 3].Value := 35;
    Sheet.Cells[3, 4].Value := 'Lyon';

    // Ajustement automatique des colonnes
    Sheet.Columns.AutoFit;

    // Sauvegarde du fichier
    Workbook.SaveAs(GetCurrentDir + '\donnees_com.xlsx');

    ShowMessage('Fichier Excel créé avec succès via COM !');
  finally
    // Fermeture d'Excel
    if not VarIsEmpty(Excel) then
    begin
      Excel.DisplayAlerts := False; // Désactiver les alertes
      Excel.Quit;
      Excel := Unassigned;
    end;
  end;
end;
```

> ⚠️ **Attention**: L'automatisation COM nécessite que Microsoft Excel soit installé sur l'ordinateur exécutant l'application. Elle est moins portable et généralement plus lente que les bibliothèques natives.

## 7.8.3 Conversion entre CSV et Excel

Il est souvent utile de convertir entre ces deux formats :

```pascal
procedure ConvertirCSVversExcel(const FichierCSV, FichierExcel: string);
var
  CSV: TStringList;
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  Ligne: string;
  Colonnes: TArray<string>;
  Row, Col: Integer;
begin
  CSV := TStringList.Create;
  Workbook := TXLSWorkbook.Create;
  try
    // Chargement du fichier CSV
    CSV.LoadFromFile(FichierCSV);

    // Création d'une feuille
    Sheet := Workbook.AddSheet('Données CSV');

    // Conversion ligne par ligne
    for Row := 0 to CSV.Count - 1 do
    begin
      Ligne := CSV[Row];
      Colonnes := Ligne.Split([',']);

      for Col := 0 to Length(Colonnes) - 1 do
      begin
        // Tenter de convertir en nombre si possible
        var ValeurTexte := Colonnes[Col];
        var ValeurNombre: Double;
        if TryStrToFloat(ValeurTexte, ValeurNombre) then
          Sheet.WriteNumber(Row, Col, ValeurNombre)
        else
          Sheet.WriteText(Row, Col, ValeurTexte);
      end;
    end;

    // Ajustement automatique des colonnes
    for Col := 0 to Length(Colonnes) - 1 do
      Sheet.AutofitColumn(Col);

    // Sauvegarde en Excel
    Workbook.SaveAs(FichierExcel);

    ShowMessage('Conversion CSV vers Excel réussie !');
  finally
    CSV.Free;
    Workbook.Free;
  end;
end;
```

## 7.8.4 Intégration avec les interfaces utilisateur

Voici un exemple d'interface permettant de visualiser et modifier un fichier CSV :

```pascal
// Composants nécessaires sur le formulaire:
// - StringGrid1: TStringGrid
// - BtnCharger, BtnSauvegarder: TButton
// - OpenDialog1, SaveDialog1: TDialogs

procedure TForm1.BtnChargerClick(Sender: TObject);
var
  Fichier: TStringList;
  Ligne: string;
  Colonnes: TArray<string>;
  i, j, MaxCols: Integer;
begin
  if OpenDialog1.Execute then
  begin
    Fichier := TStringList.Create;
    try
      // Chargement du fichier
      Fichier.LoadFromFile(OpenDialog1.FileName);

      // Détermination du nombre maximal de colonnes
      MaxCols := 0;
      for i := 0 to Fichier.Count - 1 do
      begin
        Colonnes := Fichier[i].Split([',']);
        MaxCols := Max(MaxCols, Length(Colonnes));
      end;

      // Configuration de la grille
      StringGrid1.RowCount := Fichier.Count;
      StringGrid1.ColCount := MaxCols;

      // Remplissage de la grille
      for i := 0 to Fichier.Count - 1 do
      begin
        Colonnes := Fichier[i].Split([',']);
        for j := 0 to Length(Colonnes) - 1 do
        begin
          // Suppression des guillemets si présents
          var Valeur := Colonnes[j];
          if (Length(Valeur) >= 2) and (Valeur[1] = '"') and (Valeur[Length(Valeur)] = '"') then
            Valeur := Copy(Valeur, 2, Length(Valeur) - 2);

          StringGrid1.Cells[j, i] := Valeur;
        end;
      end;

      Caption := 'Éditeur CSV - ' + ExtractFileName(OpenDialog1.FileName);
    finally
      Fichier.Free;
    end;
  end;
end;

procedure TForm1.BtnSauvegarderClick(Sender: TObject);
var
  Fichier: TStringList;
  Ligne: TStringBuilder;
  i, j: Integer;
  Valeur: string;
begin
  if SaveDialog1.Execute then
  begin
    Fichier := TStringList.Create;
    try
      // Construction ligne par ligne
      for i := 0 to StringGrid1.RowCount - 1 do
      begin
        Ligne := TStringBuilder.Create;
        try
          for j := 0 to StringGrid1.ColCount - 1 do
          begin
            if j > 0 then
              Ligne.Append(',');

            // Ajout de guillemets si nécessaire
            Valeur := StringGrid1.Cells[j, i];
            if (Pos(',', Valeur) > 0) or (Pos('"', Valeur) > 0) or (Pos(#13, Valeur) > 0) or (Pos(#10, Valeur) > 0) then
            begin
              // Échapper les guillemets en les doublant
              Valeur := StringReplace(Valeur, '"', '""', [rfReplaceAll]);
              Ligne.Append('"').Append(Valeur).Append('"');
            end
            else
              Ligne.Append(Valeur);
          end;

          Fichier.Add(Ligne.ToString);
        finally
          Ligne.Free;
        end;
      end;

      // Sauvegarde
      Fichier.SaveToFile(SaveDialog1.FileName);
      ShowMessage('Fichier sauvegardé avec succès !');
    finally
      Fichier.Free;
    end;
  end;
end;
```

## 7.8.5 Bonnes pratiques

Voici quelques conseils pour travailler efficacement avec les fichiers CSV et Excel :

1. **Encodage correct** : Toujours spécifier l'encodage lors de la lecture/écriture de CSV (`TEncoding.UTF8` est généralement recommandé).

2. **Gestion des guillemets** : Ne pas oublier que les champs CSV contenant des délimiteurs ou des sauts de ligne doivent être entourés de guillemets.

3. **Performance** : Pour les grands fichiers, envisagez une approche de streaming plutôt que de charger tout le fichier en mémoire.

4. **Formats régionaux** : Attention aux paramètres régionaux pour les nombres (virgule vs point décimal).

5. **Validation des données** : Toujours valider les données lors de l'importation pour éviter les erreurs.

6. **Préférer les bibliothèques spécialisées** : Pour des manipulations complexes, préférez utiliser des bibliothèques dédiées plutôt que de réinventer la roue.

7. **Créer une couche d'abstraction** : Pour les projets importants, créez une couche d'abstraction qui masque les détails d'implémentation des formats.

## 7.8.6 Exemples concrets

### Application d'importation/exportation de contacts

```pascal
type
  TContact = class
    Nom: string;
    Prenom: string;
    Email: string;
    Telephone: string;
    DateNaissance: TDate;
  end;

procedure ImporterContactsCSV(const NomFichier: string; var Contacts: TObjectList<TContact>);
var
  CSV: TStringList;
  Colonnes: TArray<string>;
  Contact: TContact;
  i: Integer;
begin
  CSV := TStringList.Create;
  try
    CSV.LoadFromFile(NomFichier, TEncoding.UTF8);

    // Ignorer la première ligne (en-têtes)
    for i := 1 to CSV.Count - 1 do
    begin
      Colonnes := CSV[i].Split([',']);

      // Vérification du format
      if Length(Colonnes) >= 5 then
      begin
        Contact := TContact.Create;
        Contact.Nom := Colonnes[0].Trim(['"']);
        Contact.Prenom := Colonnes[1].Trim(['"']);
        Contact.Email := Colonnes[2].Trim(['"']);
        Contact.Telephone := Colonnes[3].Trim(['"']);

        // Conversion de la date
        try
          Contact.DateNaissance := StrToDate(Colonnes[4].Trim(['"']));
        except
          Contact.DateNaissance := 0; // Date invalide
        end;

        Contacts.Add(Contact);
      end;
    end;
  finally
    CSV.Free;
  end;
end;

procedure ExporterContactsExcel(const NomFichier: string; Contacts: TObjectList<TContact>);
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  i: Integer;
begin
  Workbook := TXLSWorkbook.Create;
  try
    Sheet := Workbook.AddSheet('Contacts');

    // Ajout des en-têtes
    Sheet.WriteText(0, 0, 'Nom');
    Sheet.WriteText(0, 1, 'Prénom');
    Sheet.WriteText(0, 2, 'Email');
    Sheet.WriteText(0, 3, 'Téléphone');
    Sheet.WriteText(0, 4, 'Date de naissance');

    // Mise en forme des en-têtes
    for i := 0 to 4 do
    begin
      Sheet.SetCellFontBold(0, i, True);
      Sheet.SetCellColor(0, i, $EEEEEE);
    end;

    // Ajout des données
    for i := 0 to Contacts.Count - 1 do
    begin
      Sheet.WriteText(i+1, 0, Contacts[i].Nom);
      Sheet.WriteText(i+1, 1, Contacts[i].Prenom);
      Sheet.WriteText(i+1, 2, Contacts[i].Email);
      Sheet.WriteText(i+1, 3, Contacts[i].Telephone);

      // Format de date personnalisé
      if Contacts[i].DateNaissance > 0 then
        Sheet.WriteDateTime(i+1, 4, Contacts[i].DateNaissance, 'dd/mm/yyyy');
    end;

    // Ajustement automatique des colonnes
    for i := 0 to 4 do
      Sheet.AutofitColumn(i);

    // Ajout d'un filtre automatique
    Sheet.SetAutoFilter(0, 0, Contacts.Count, 4);

    // Figer la première ligne (en-têtes)
    Sheet.FreezePanes(1, 0);

    // Sauvegarde du fichier
    Workbook.SaveAs(NomFichier);

    ShowMessage('Exportation terminée avec succès !');
  finally
    Workbook.Free;
  end;
end;
```

### Exemple d'application de gestion de stock

Voici un exemple plus complet d'application de gestion de stock qui utilise à la fois CSV et Excel :

```pascal
type
  TProduit = class
  private
    FCode: string;
    FNom: string;
    FPrix: Double;
    FQuantite: Integer;
    FCategorie: string;
  public
    property Code: string read FCode write FCode;
    property Nom: string read FNom write FNom;
    property Prix: Double read FPrix write FPrix;
    property Quantite: Integer read FQuantite write FQuantite;
    property Categorie: string read FCategorie write FCategorie;

    function ValeurStock: Double;
  end;

function TProduit.ValeurStock: Double;
begin
  Result := FPrix * FQuantite;
end;

procedure TFormPrincipal.ImporterProduits(Sender: TObject);
var
  Dlg: TOpenDialog;
  Extension: string;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Filter := 'Fichiers supportés (*.csv;*.xls;*.xlsx)|*.csv;*.xls;*.xlsx|' +
                 'Fichiers CSV (*.csv)|*.csv|Fichiers Excel (*.xls;*.xlsx)|*.xls;*.xlsx';

    if Dlg.Execute then
    begin
      Extension := LowerCase(ExtractFileExt(Dlg.FileName));

      // Vider la liste actuelle
      ListeProduits.Clear;

      // Importer selon le format
      if Extension = '.csv' then
        ImporterProduitsCSV(Dlg.FileName)
      else if (Extension = '.xls') or (Extension = '.xlsx') then
        ImporterProduitsExcel(Dlg.FileName);

      // Mettre à jour l'interface
      AfficherProduits;
      CalculerStatistiques;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TFormPrincipal.ImporterProduitsCSV(const NomFichier: string);
var
  CSV: TStringList;
  Colonnes: TArray<string>;
  Produit: TProduit;
  i: Integer;
begin
  CSV := TStringList.Create;
  try
    CSV.LoadFromFile(NomFichier, TEncoding.UTF8);

    // Ignorer la première ligne si c'est un en-tête
    // (vérification en regardant si c'est un nombre)
    var PremiereLigne := 0;
    if (CSV.Count > 0) then
    begin
      Colonnes := CSV[0].Split([',', ';']);
      if (Length(Colonnes) >= 3) and not TryStrToFloat(Colonnes[2], valeur) then
        PremiereLigne := 1;
    end;

    // Parcourir les lignes
    for i := PremiereLigne to CSV.Count - 1 do
    begin
      Colonnes := CSV[i].Split([',', ';']);

      // S'assurer qu'il y a assez de colonnes
      if Length(Colonnes) >= 5 then
      begin
        Produit := TProduit.Create;

        // Nettoyage et assignation des valeurs
        Produit.Code := Colonnes[0].Trim([' ', '"']);
        Produit.Nom := Colonnes[1].Trim([' ', '"']);

        // Conversion des valeurs numériques en tenant compte des formats régionaux
        try
          // Remplacement de la virgule par un point si nécessaire
          var ValeurPrix := StringReplace(Colonnes[2].Trim([' ', '"']), ',', '.', []);
          Produit.Prix := StrToFloat(ValeurPrix);
        except
          Produit.Prix := 0;
          Log('Erreur de conversion de prix à la ligne ' + IntToStr(i+1));
        end;

        try
          Produit.Quantite := StrToInt(Colonnes[3].Trim([' ', '"']));
        except
          Produit.Quantite := 0;
          Log('Erreur de conversion de quantité à la ligne ' + IntToStr(i+1));
        end;

        Produit.Categorie := Colonnes[4].Trim([' ', '"']);

        // Ajouter à la liste
        ListeProduits.Add(Produit);
      end;
    end;

    ShowMessage(Format('Importation réussie : %d produits chargés.', [ListeProduits.Count]));
  finally
    CSV.Free;
  end;
end;

procedure TFormPrincipal.ImporterProduitsExcel(const NomFichier: string);
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  Produit: TProduit;
  RowCount, ColCount: Integer;
  Row: Integer;
begin
  Workbook := TXLSWorkbook.Create;
  try
    // Charger le fichier Excel
    Workbook.LoadFromFile(NomFichier);

    if Workbook.SheetCount > 0 then
    begin
      // Utiliser la première feuille
      Sheet := Workbook.GetSheet(0);

      // Déterminer les dimensions
      RowCount := Sheet.GetLastRow;
      ColCount := Sheet.GetLastCol;

      // Vérifier qu'il y a suffisamment de colonnes
      if ColCount >= 4 then
      begin
        // Parcourir les lignes (en commençant à 1 pour ignorer l'en-tête)
        for Row := 1 to RowCount do
        begin
          Produit := TProduit.Create;

          // Lire les données selon leur type
          Produit.Code := Sheet.ReadText(Row, 0);
          Produit.Nom := Sheet.ReadText(Row, 1);

          // Pour les valeurs numériques, vérifier le type de cellule
          if Sheet.GetCellType(Row, 2) = cellNumber then
            Produit.Prix := Sheet.ReadNumber(Row, 2)
          else
          begin
            try
              Produit.Prix := StrToFloat(StringReplace(Sheet.ReadText(Row, 2), ',', '.', []));
            except
              Produit.Prix := 0;
              Log('Erreur de conversion de prix à la ligne ' + IntToStr(Row+1));
            end;
          end;

          if Sheet.GetCellType(Row, 3) = cellNumber then
            Produit.Quantite := Round(Sheet.ReadNumber(Row, 3))
          else
          begin
            try
              Produit.Quantite := StrToInt(Sheet.ReadText(Row, 3));
            except
              Produit.Quantite := 0;
              Log('Erreur de conversion de quantité à la ligne ' + IntToStr(Row+1));
            end;
          end;

          Produit.Categorie := Sheet.ReadText(Row, 4);

          // Ajouter à la liste
          ListeProduits.Add(Produit);
        end;
      end;
    end;

    ShowMessage(Format('Importation réussie : %d produits chargés.', [ListeProduits.Count]));
  finally
    Workbook.Free;
  end;
end;

procedure TFormPrincipal.ExporterRapportStock;
var
  Workbook: TXLSWorkbook;
  SheetStock, SheetResume: TXLSSheet;
  Row, i: Integer;
  Categories: TDictionary<string, TList<TProduit>>;
  Categorie: string;
  ProduitsCategorie: TList<TProduit>;
  ValeurTotale: Double;
begin
  // Création du dictionnaire pour regrouper par catégorie
  Categories := TDictionary<string, TList<TProduit>>.Create;
  try
    // Regrouper les produits par catégorie
    for i := 0 to ListeProduits.Count - 1 do
    begin
      Categorie := ListeProduits[i].Categorie;

      if not Categories.ContainsKey(Categorie) then
        Categories.Add(Categorie, TList<TProduit>.Create);

      Categories[Categorie].Add(ListeProduits[i]);
    end;

    // Créer le classeur Excel
    Workbook := TXLSWorkbook.Create;
    try
      // Feuille détaillée du stock
      SheetStock := Workbook.AddSheet('Détail du stock');

      // En-têtes
      SheetStock.WriteText(0, 0, 'Code');
      SheetStock.WriteText(0, 1, 'Nom du produit');
      SheetStock.WriteText(0, 2, 'Prix unitaire');
      SheetStock.WriteText(0, 3, 'Quantité');
      SheetStock.WriteText(0, 4, 'Valeur stock');
      SheetStock.WriteText(0, 5, 'Catégorie');

      // Mise en forme des en-têtes
      for i := 0 to 5 do
      begin
        SheetStock.SetCellFontBold(0, i, True);
        SheetStock.SetCellColor(0, i, $DDDDDD);
      end;

      // Ajouter les produits
      Row := 1;
      for i := 0 to ListeProduits.Count - 1 do
      begin
        SheetStock.WriteText(Row, 0, ListeProduits[i].Code);
        SheetStock.WriteText(Row, 1, ListeProduits[i].Nom);
        SheetStock.WriteNumber(Row, 2, ListeProduits[i].Prix);
        SheetStock.WriteNumber(Row, 3, ListeProduits[i].Quantite);
        SheetStock.WriteNumber(Row, 4, ListeProduits[i].ValeurStock);
        SheetStock.WriteText(Row, 5, ListeProduits[i].Categorie);

        // Format monétaire pour les prix et valeurs
        SheetStock.SetCellFormat(Row, 2, '#,##0.00 €');
        SheetStock.SetCellFormat(Row, 4, '#,##0.00 €');

        // Coloration conditionnelle pour les stocks faibles
        if ListeProduits[i].Quantite < 5 then
          SheetStock.SetCellColor(Row, 3, $AAAAFF); // Rouge clair

        Inc(Row);
      end;

      // Ajustement des colonnes et filtres
      for i := 0 to 5 do
        SheetStock.AutofitColumn(i);

      SheetStock.SetAutoFilter(0, 0, ListeProduits.Count, 5);
      SheetStock.FreezePanes(1, 0);

      // Feuille de résumé par catégorie
      SheetResume := Workbook.AddSheet('Résumé par catégorie');

      // En-têtes du résumé
      SheetResume.WriteText(0, 0, 'Catégorie');
      SheetResume.WriteText(0, 1, 'Nombre de produits');
      SheetResume.WriteText(0, 2, 'Valeur totale');

      // Mise en forme des en-têtes
      for i := 0 to 2 do
      begin
        SheetResume.SetCellFontBold(0, i, True);
        SheetResume.SetCellColor(0, i, $DDDDDD);
      end;

      // Ajouter les données de résumé
      Row := 1;
      ValeurTotale := 0;

      var CategoriesList := Categories.Keys.ToArray;
      for i := 0 to Length(CategoriesList) - 1 do
      begin
        Categorie := CategoriesList[i];
        ProduitsCategorie := Categories[Categorie];

        // Calculer la valeur totale de cette catégorie
        var ValeurCategorie := 0.0;
        for var j := 0 to ProduitsCategorie.Count - 1 do
          ValeurCategorie := ValeurCategorie + ProduitsCategorie[j].ValeurStock;

        // Ajouter à la valeur totale
        ValeurTotale := ValeurTotale + ValeurCategorie;

        // Écrire dans la feuille
        SheetResume.WriteText(Row, 0, Categorie);
        SheetResume.WriteNumber(Row, 1, ProduitsCategorie.Count);
        SheetResume.WriteNumber(Row, 2, ValeurCategorie);

        // Format monétaire
        SheetResume.SetCellFormat(Row, 2, '#,##0.00 €');

        Inc(Row);
      end;

      // Ligne de total
      SheetResume.WriteText(Row+1, 0, 'TOTAL');
      SheetResume.WriteNumber(Row+1, 1, ListeProduits.Count);
      SheetResume.WriteNumber(Row+1, 2, ValeurTotale);
      SheetResume.SetCellFontBold(Row+1, 0, True);
      SheetResume.SetCellFontBold(Row+1, 1, True);
      SheetResume.SetCellFontBold(Row+1, 2, True);
      SheetResume.SetCellFormat(Row+1, 2, '#,##0.00 €');

      // Ajuster les colonnes
      for i := 0 to 2 do
        SheetResume.AutofitColumn(i);

      // Ajouter un graphique en camembert pour les catégories
      var Chart := SheetResume.AddChart(TXLSChartType.ctPie, 1, 4, 15, 10);
      Chart.AddSeries('Valeur par catégorie', 'Résumé par catégorie!A2:A' + IntToStr(Row),
                     'Résumé par catégorie!C2:C' + IntToStr(Row));
      Chart.Title := 'Répartition de la valeur du stock par catégorie';

      // Sauvegarde du fichier
      if SaveDialog1.Execute then
      begin
        Workbook.SaveAs(SaveDialog1.FileName);
        ShowMessage('Rapport de stock exporté avec succès !');
      end;
    finally
      Workbook.Free;
    end;
  finally
    // Libérer le dictionnaire et les listes qu'il contient
    for var Pair in Categories do
      Pair.Value.Free;
    Categories.Free;
  end;
end;
```

## 7.8.7 Traitement de fichiers CSV et Excel volumineux

Pour les fichiers volumineux, les approches standard peuvent consommer trop de mémoire ou être trop lentes. Voici quelques techniques pour optimiser le traitement :

### Traitement en streaming de CSV

```pascal
procedure TraiterCSVVolumineux(const NomFichier: string);
var
  Fichier: TStreamReader;
  Ligne: string;
  Colonnes: TArray<string>;
  CompteurLignes: Integer;
begin
  CompteurLignes := 0;

  // Utiliser un StreamReader au lieu de charger tout le fichier
  Fichier := TStreamReader.Create(NomFichier, TEncoding.UTF8);
  try
    // Lire et ignorer l'en-tête
    Fichier.ReadLine;

    // Traiter ligne par ligne
    while not Fichier.EndOfStream do
    begin
      Ligne := Fichier.ReadLine;
      Inc(CompteurLignes);

      // Traitement par lots (exemple: chaque 1000 lignes)
      if CompteurLignes mod 1000 = 0 then
      begin
        // Mise à jour de la progression
        ProgressBar1.Position := CompteurLignes;
        Application.ProcessMessages; // Permettre à l'interface de se rafraîchir
      end;

      // Traitement de la ligne
      Colonnes := Ligne.Split([',']);

      // Faire quelque chose avec les colonnes...
      // Cette partie doit être efficace et ne pas stocker trop d'informations en mémoire
    end;

    ShowMessage(Format('Traitement terminé : %d lignes traitées.', [CompteurLignes]));
  finally
    Fichier.Free;
  end;
end;
```

### Traitement par lots d'Excel

Pour les grands fichiers Excel, le traitement par plages est souvent plus efficace :

```pascal
procedure TraiterExcelVolumineux(const NomFichier: string);
var
  Workbook: TXLSWorkbook;
  Sheet: TXLSSheet;
  RowCount, RowBatch, BatchSize: Integer;
  StartRow, EndRow: Integer;
begin
  Workbook := TXLSWorkbook.Create;
  try
    // Ouvrir le fichier avec des options optimisées
    Workbook.OpenOptions.MemoryUsage := xlsxMinimalMemory;
    Workbook.LoadFromFile(NomFichier);

    if Workbook.SheetCount > 0 then
    begin
      Sheet := Workbook.GetSheet(0);
      RowCount := Sheet.GetLastRow;

      // Configurer la barre de progression
      ProgressBar1.Max := RowCount;
      ProgressBar1.Position := 0;

      // Taille des lots (ajuster selon la mémoire disponible)
      BatchSize := 1000;

      // Traiter par lots
      for RowBatch := 0 to Ceil(RowCount / BatchSize) - 1 do
      begin
        StartRow := RowBatch * BatchSize + 1; // +1 pour ignorer l'en-tête
        EndRow := Min((RowBatch + 1) * BatchSize, RowCount);

        // Traiter les lignes du lot actuel
        for var Row := StartRow to EndRow do
        begin
          // Traiter la ligne...

          // Mise à jour de la progression
          if Row mod 100 = 0 then
          begin
            ProgressBar1.Position := Row;
            Application.ProcessMessages;
          end;
        end;

        // Libérer de la mémoire entre les lots
        if Assigned(Workbook.ClearCacheCallback) then
          Workbook.ClearCacheCallback();
      end;

      ShowMessage(Format('Traitement terminé : %d lignes traitées.', [RowCount-1]));
    end;
  finally
    Workbook.Free;
  end;
end;
```

## 7.8.8 Conclusion

La manipulation de fichiers CSV et Excel est une tâche courante dans de nombreuses applications professionnelles. Delphi offre plusieurs approches pour traiter ces formats, depuis les méthodes de base jusqu'aux bibliothèques spécialisées.

Points clés à retenir :

1. Pour les fichiers CSV simples, les classes de base comme `TStringList` peuvent suffire.
2. Pour les CSV complexes ou les traitements avancés, envisagez des bibliothèques dédiées.
3. Pour Excel, des bibliothèques comme XLSX offrent des performances supérieures et ne nécessitent pas Excel installé.
4. L'automatisation COM reste une option pour des cas spécifiques nécessitant des fonctionnalités avancées d'Excel.
5. Pour les fichiers volumineux, privilégiez les approches de streaming et le traitement par lots.
6. N'oubliez pas de gérer correctement les questions d'encodage, de formats régionaux et d'échappement des caractères spéciaux.

Ces techniques vous permettront de créer des applications robustes pour l'importation, l'exportation et l'analyse de données tabulaires.

## 7.8.9 Exercices pratiques

1. Créez un convertisseur simple qui peut transformer un fichier CSV en Excel et vice-versa.
2. Développez un éditeur de CSV avec validation des données et formatage conditionnel.
3. Réalisez une application qui peut fusionner plusieurs fichiers CSV ou feuilles Excel en un seul document.
4. Créez un générateur de rapport qui analyse un fichier CSV de ventes et produit un rapport Excel avec graphiques et tableaux croisés dynamiques.
5. Implémentez une solution pour traiter un fichier CSV de plusieurs gigaoctets en utilisant le streaming et des techniques d'optimisation de la mémoire.
