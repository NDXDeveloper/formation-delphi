🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.8 Manipulation de fichiers CSV et Excel

## Introduction

Les fichiers CSV (Comma-Separated Values) et Excel sont parmi les formats de données les plus utilisés dans le monde professionnel pour stocker et échanger des données tabulaires (tableaux). Savoir les manipuler est une compétence essentielle pour tout développeur.

**Analogie simple :** Imaginez un tableau Excel comme une grande feuille de papier quadrillée :
- **CSV** : C'est comme écrire ce tableau avec seulement du texte, en séparant les colonnes par des virgules
- **Excel** : C'est le tableau complet avec mise en forme, formules, graphiques, etc.

## Pourquoi manipuler CSV et Excel ?

### Cas d'utilisation courants

1. **Import/Export de données** : Échanger des données avec d'autres applications
2. **Rapports** : Générer des rapports pour les utilisateurs
3. **Migration de données** : Transférer des données entre systèmes
4. **Traitement par lots** : Importer de grandes quantités de données
5. **Analyses** : Exporter des données pour analyse dans Excel
6. **Sauvegardes** : Format simple et universel

---

## Partie 1 : Fichiers CSV

### Qu'est-ce qu'un fichier CSV ?

Un fichier CSV est un fichier texte où chaque ligne représente une ligne de tableau, et les colonnes sont séparées par un délimiteur (généralement une virgule ou un point-virgule).

**Exemple de fichier CSV :**
```csv
Nom,Prénom,Âge,Ville  
Dupont,Jean,30,Paris  
Martin,Marie,28,Lyon  
Dubois,Pierre,35,Marseille  
```

### Variantes de CSV

Les fichiers CSV peuvent utiliser différents délimiteurs :
- **Virgule** : `Dupont,Jean,30` (standard anglais)
- **Point-virgule** : `Dupont;Jean;30` (standard français/européen)
- **Tabulation** : `Dupont   Jean   30` (TSV - Tab-Separated Values)

---

## Lecture de fichiers CSV

### Méthode 1 : Lecture manuelle simple

```pascal
uses
  System.Classes, System.SysUtils, System.StrUtils;

procedure LireCSVSimple(const NomFichier: string; Grille: TStringGrid);  
var  
  Lignes: TStringList;
  i, j: Integer;
  Ligne: string;
  Colonnes: TArray<string>;
begin
  Lignes := TStringList.Create;
  try
    // Charger le fichier
    Lignes.LoadFromFile(NomFichier);

    if Lignes.Count = 0 then
      Exit;

    // Configurer la grille
    Grille.RowCount := Lignes.Count;

    // Première ligne pour déterminer le nombre de colonnes
    Colonnes := Lignes[0].Split([',']);
    Grille.ColCount := Length(Colonnes);

    // Remplir la grille
    for i := 0 to Lignes.Count - 1 do
    begin
      Ligne := Lignes[i];
      Colonnes := Ligne.Split([',']);

      for j := 0 to High(Colonnes) do
      begin
        if j < Grille.ColCount then
          Grille.Cells[j, i] := Trim(Colonnes[j]);
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  LireCSVSimple('donnees.csv', StringGrid1);
end;
```

### Méthode 2 : Lecture avec gestion avancée

```pascal
type
  TCSVOptions = record
    Delimiter: Char;           // Séparateur (virgule, point-virgule, etc.)
    QuoteChar: Char;          // Caractère d'encadrement des chaînes
    HasHeader: Boolean;       // Première ligne = en-têtes
    Encoding: TEncoding;      // Encodage du fichier
  end;

function ParseCSVLine(const Line: string; Delimiter, QuoteChar: Char): TArray<string>;  
var  
  i, Start: Integer;
  InQuotes: Boolean;
  CurrentField: string;
  Fields: TList<string>;
begin
  Fields := TList<string>.Create;
  try
    InQuotes := False;
    CurrentField := '';
    Start := 1;

    for i := 1 to Length(Line) do
    begin
      if Line[i] = QuoteChar then
        InQuotes := not InQuotes
      else if (Line[i] = Delimiter) and (not InQuotes) then
      begin
        // Fin de champ
        Fields.Add(Trim(CurrentField));
        CurrentField := '';
      end
      else
        CurrentField := CurrentField + Line[i];
    end;

    // Ajouter le dernier champ
    Fields.Add(Trim(CurrentField));

    Result := Fields.ToArray;
  finally
    Fields.Free;
  end;
end;

procedure LireCSVAvance(const NomFichier: string;
                        const Options: TCSVOptions;
                        Grille: TStringGrid);
var
  Lignes: TStringList;
  i, j, StartRow: Integer;
  Colonnes: TArray<string>;
begin
  Lignes := TStringList.Create;
  try
    Lignes.LoadFromFile(NomFichier, Options.Encoding);

    if Lignes.Count = 0 then
      Exit;

    StartRow := 0;

    // Traiter l'en-tête si présent
    if Options.HasHeader then
    begin
      Colonnes := ParseCSVLine(Lignes[0], Options.Delimiter, Options.QuoteChar);
      Grille.ColCount := Length(Colonnes);

      // Remplir l'en-tête (ligne 0 fixe)
      Grille.RowCount := Lignes.Count;
      Grille.FixedRows := 1;

      for j := 0 to High(Colonnes) do
        Grille.Cells[j, 0] := Colonnes[j];

      StartRow := 1;
    end
    else
    begin
      Colonnes := ParseCSVLine(Lignes[0], Options.Delimiter, Options.QuoteChar);
      Grille.ColCount := Length(Colonnes);
      Grille.RowCount := Lignes.Count;
    end;

    // Remplir les données
    for i := StartRow to Lignes.Count - 1 do
    begin
      Colonnes := ParseCSVLine(Lignes[i], Options.Delimiter, Options.QuoteChar);

      for j := 0 to High(Colonnes) do
      begin
        if j < Grille.ColCount then
          Grille.Cells[j, i] := Colonnes[j];
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.Button2Click(Sender: TObject);  
var  
  Options: TCSVOptions;
begin
  Options.Delimiter := ';';        // Point-virgule
  Options.QuoteChar := '"';        // Guillemets
  Options.HasHeader := True;       // Première ligne = en-têtes
  Options.Encoding := TEncoding.UTF8;

  LireCSVAvance('donnees.csv', Options, StringGrid1);
end;
```

### Détection automatique du délimiteur

```pascal
function DetecterDelimiteurCSV(const NomFichier: string): Char;  
var  
  Lignes: TStringList;
  PremiereLigne: string;
  NbVirgules, NbPointsVirgules, NbTabulations: Integer;
begin
  Result := ','; // Par défaut

  Lignes := TStringList.Create;
  try
    Lignes.LoadFromFile(NomFichier);

    if Lignes.Count = 0 then
      Exit;

    PremiereLigne := Lignes[0];

    // Compter les occurrences de chaque séparateur
    NbVirgules := Length(PremiereLigne) -
                  Length(StringReplace(PremiereLigne, ',', '', [rfReplaceAll]));
    NbPointsVirgules := Length(PremiereLigne) -
                        Length(StringReplace(PremiereLigne, ';', '', [rfReplaceAll]));
    NbTabulations := Length(PremiereLigne) -
                     Length(StringReplace(PremiereLigne, #9, '', [rfReplaceAll]));

    // Choisir le plus fréquent
    if (NbPointsVirgules > NbVirgules) and (NbPointsVirgules > NbTabulations) then
      Result := ';'
    else if (NbTabulations > NbVirgules) and (NbTabulations > NbPointsVirgules) then
      Result := #9
    else
      Result := ',';
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.OuvrirCSVAuto;  
var  
  Delimiter: Char;
  Options: TCSVOptions;
begin
  if OpenDialog1.Execute then
  begin
    Delimiter := DetecterDelimiteurCSV(OpenDialog1.FileName);

    Options.Delimiter := Delimiter;
    Options.QuoteChar := '"';
    Options.HasHeader := True;
    Options.Encoding := TEncoding.UTF8;

    LireCSVAvance(OpenDialog1.FileName, Options, StringGrid1);

    ShowMessage(Format('Délimiteur détecté : "%s"', [Delimiter]));
  end;
end;
```

---

## Écriture de fichiers CSV

### Écriture simple

```pascal
procedure EcrireCSVSimple(const NomFichier: string; Grille: TStringGrid);  
var  
  Lignes: TStringList;
  i, j: Integer;
  Ligne: string;
begin
  Lignes := TStringList.Create;
  try
    // Parcourir toutes les lignes
    for i := 0 to Grille.RowCount - 1 do
    begin
      Ligne := '';

      // Construire la ligne
      for j := 0 to Grille.ColCount - 1 do
      begin
        if j > 0 then
          Ligne := Ligne + ',';

        Ligne := Ligne + Grille.Cells[j, i];
      end;

      Lignes.Add(Ligne);
    end;

    // Sauvegarder
    Lignes.SaveToFile(NomFichier, TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;
```

### Écriture avec protection des données

```pascal
function EchapperChampCSV(const Champ: string; QuoteChar: Char): string;  
begin  
  Result := Champ;

  // Si le champ contient des caractères spéciaux, l'encadrer de guillemets
  if (Pos(',', Result) > 0) or
     (Pos(';', Result) > 0) or
     (Pos('"', Result) > 0) or
     (Pos(#13, Result) > 0) or
     (Pos(#10, Result) > 0) then
  begin
    // Doubler les guillemets internes
    Result := StringReplace(Result, '"', '""', [rfReplaceAll]);

    // Encadrer
    Result := QuoteChar + Result + QuoteChar;
  end;
end;

procedure EcrireCSVAvance(const NomFichier: string;
                          Grille: TStringGrid;
                          Delimiter, QuoteChar: Char);
var
  Lignes: TStringList;
  i, j: Integer;
  Ligne: string;
begin
  Lignes := TStringList.Create;
  try
    for i := 0 to Grille.RowCount - 1 do
    begin
      Ligne := '';

      for j := 0 to Grille.ColCount - 1 do
      begin
        if j > 0 then
          Ligne := Ligne + Delimiter;

        Ligne := Ligne + EchapperChampCSV(Grille.Cells[j, i], QuoteChar);
      end;

      Lignes.Add(Ligne);
    end;

    Lignes.SaveToFile(NomFichier, TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.SauvegarderCSV;  
begin  
  if SaveDialog1.Execute then
    EcrireCSVAvance(SaveDialog1.FileName, StringGrid1, ';', '"');
end;
```

---

## Classe complète pour CSV

```pascal
type
  TCSVReader = class
  private
    FFileName: string;
    FDelimiter: Char;
    FQuoteChar: Char;
    FHasHeader: Boolean;
    FHeaders: TArray<string>;
    FData: TList<TArray<string>>;

    function ParseLine(const Line: string): TArray<string>;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Load;
    procedure Save(const FileName: string = '');

    function GetValue(Row, Col: Integer): string;
    procedure SetValue(Row, Col: Integer; const Value: string);

    function GetValueByHeader(Row: Integer; const HeaderName: string): string;
    procedure AddRow(const Values: TArray<string>);

    function RowCount: Integer;
    function ColCount: Integer;

    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property HasHeader: Boolean read FHasHeader write FHasHeader;
    property Headers: TArray<string> read FHeaders;
  end;

constructor TCSVReader.Create(const FileName: string);  
begin  
  inherited Create;
  FFileName := FileName;
  FDelimiter := ',';
  FQuoteChar := '"';
  FHasHeader := True;
  FData := TList<TArray<string>>.Create;
end;

destructor TCSVReader.Destroy;  
begin  
  FData.Free;
  inherited;
end;

function TCSVReader.ParseLine(const Line: string): TArray<string>;  
var  
  i: Integer;
  InQuotes: Boolean;
  CurrentField: string;
  Fields: TList<string>;
begin
  Fields := TList<string>.Create;
  try
    InQuotes := False;
    CurrentField := '';
    i := 1;

    // Utiliser while car on doit parfois avancer de 2 positions (guillemets doublés)
    // Note : for ne permet pas de modifier la variable de boucle en Delphi
    while i <= Length(Line) do
    begin
      if Line[i] = FQuoteChar then
      begin
        // Gérer les guillemets doublés
        if (i < Length(Line)) and (Line[i + 1] = FQuoteChar) then
        begin
          CurrentField := CurrentField + FQuoteChar;
          Inc(i); // Sauter le deuxième guillemet
        end
        else
          InQuotes := not InQuotes;
      end
      else if (Line[i] = FDelimiter) and (not InQuotes) then
      begin
        Fields.Add(CurrentField);
        CurrentField := '';
      end
      else
        CurrentField := CurrentField + Line[i];

      Inc(i);
    end;

    Fields.Add(CurrentField);
    Result := Fields.ToArray;
  finally
    Fields.Free;
  end;
end;

procedure TCSVReader.Load;  
var  
  Lignes: TStringList;
  i, StartRow: Integer;
begin
  FData.Clear;

  Lignes := TStringList.Create;
  try
    Lignes.LoadFromFile(FFileName, TEncoding.UTF8);

    if Lignes.Count = 0 then
      Exit;

    StartRow := 0;

    if FHasHeader then
    begin
      FHeaders := ParseLine(Lignes[0]);
      StartRow := 1;
    end;

    for i := StartRow to Lignes.Count - 1 do
      FData.Add(ParseLine(Lignes[i]));
  finally
    Lignes.Free;
  end;
end;

procedure TCSVReader.Save(const FileName: string);  
var  
  Lignes: TStringList;
  i, j: Integer;
  Ligne: string;
  Row: TArray<string>;
  SaveName: string;
begin
  if FileName = '' then
    SaveName := FFileName
  else
    SaveName := FileName;

  Lignes := TStringList.Create;
  try
    // Écrire l'en-tête si présent
    if FHasHeader and (Length(FHeaders) > 0) then
    begin
      Ligne := '';
      for j := 0 to High(FHeaders) do
      begin
        if j > 0 then
          Ligne := Ligne + FDelimiter;
        Ligne := Ligne + EchapperChampCSV(FHeaders[j], FQuoteChar);
      end;
      Lignes.Add(Ligne);
    end;

    // Écrire les données
    for i := 0 to FData.Count - 1 do
    begin
      Row := FData[i];
      Ligne := '';

      for j := 0 to High(Row) do
      begin
        if j > 0 then
          Ligne := Ligne + FDelimiter;
        Ligne := Ligne + EchapperChampCSV(Row[j], FQuoteChar);
      end;

      Lignes.Add(Ligne);
    end;

    Lignes.SaveToFile(SaveName, TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;

function TCSVReader.GetValue(Row, Col: Integer): string;  
begin  
  if (Row >= 0) and (Row < FData.Count) and
     (Col >= 0) and (Col < Length(FData[Row])) then
    Result := FData[Row][Col]
  else
    Result := '';
end;

procedure TCSVReader.SetValue(Row, Col: Integer; const Value: string);  
begin  
  if (Row >= 0) and (Row < FData.Count) and
     (Col >= 0) and (Col < Length(FData[Row])) then
    FData[Row][Col] := Value;
end;

function TCSVReader.GetValueByHeader(Row: Integer;
                                     const HeaderName: string): string;
var
  Col, i: Integer;
begin
  Result := '';
  Col := -1;

  // Trouver la colonne
  for i := 0 to High(FHeaders) do
  begin
    if SameText(FHeaders[i], HeaderName) then
    begin
      Col := i;
      Break;
    end;
  end;

  if Col >= 0 then
    Result := GetValue(Row, Col);
end;

procedure TCSVReader.AddRow(const Values: TArray<string>);  
begin  
  FData.Add(Values);
end;

function TCSVReader.RowCount: Integer;  
begin  
  Result := FData.Count;
end;

function TCSVReader.ColCount: Integer;  
begin  
  if FHasHeader then
    Result := Length(FHeaders)
  else if FData.Count > 0 then
    Result := Length(FData[0])
  else
    Result := 0;
end;

// Utilisation
procedure TForm1.UtiliserClasseCSV;  
var  
  CSV: TCSVReader;
  i: Integer;
  Nom, Age: string;
begin
  CSV := TCSVReader.Create('personnes.csv');
  try
    CSV.Delimiter := ';';
    CSV.HasHeader := True;
    CSV.Load;

    // Lire les données
    Memo1.Lines.Add(Format('Fichier contient %d lignes', [CSV.RowCount]));

    for i := 0 to CSV.RowCount - 1 do
    begin
      Nom := CSV.GetValueByHeader(i, 'Nom');
      Age := CSV.GetValueByHeader(i, 'Âge');

      Memo1.Lines.Add(Format('%s : %s ans', [Nom, Age]));
    end;

    // Ajouter une ligne
    CSV.AddRow(['Nouveau', 'Jean', '40', 'Nice']);

    // Sauvegarder
    CSV.Save('personnes_modifiees.csv');
  finally
    CSV.Free;
  end;
end;
```

---

## Partie 2 : Fichiers Excel

### Introduction aux fichiers Excel

Les fichiers Excel (.xlsx, .xls) sont des formats binaires complexes qui peuvent contenir :
- Plusieurs feuilles (worksheets)
- Formules
- Mise en forme (couleurs, polices, bordures)
- Graphiques
- Images
- Et bien plus...

### Bibliothèques pour Excel en Delphi

Delphi n'a pas de support natif pour Excel, mais plusieurs options existent :

1. **OLE Automation** : Utilise Excel installé sur le PC
2. **SheetJS (XLSX)** : Bibliothèque tierce pure Delphi
3. **Composants tiers** : TMS, DevExpress, etc.

Nous allons voir les deux premières méthodes.

---

## Méthode 1 : OLE Automation (Excel doit être installé)

### Lecture de fichier Excel via OLE

```pascal
uses
  ComObj, Variants;

procedure LireExcelViaOLE(const NomFichier: string; Grille: TStringGrid);  
var  
  Excel, Workbook, Worksheet: Variant;
  LastRow, LastCol: Integer;
  i, j: Integer;
begin
  // Créer une instance Excel
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;
    Excel.DisplayAlerts := False;

    // Ouvrir le fichier
    Workbook := Excel.Workbooks.Open(NomFichier);
    try
      // Prendre la première feuille
      Worksheet := Workbook.Worksheets[1];

      // Trouver la dernière ligne et colonne utilisées
      LastRow := Worksheet.UsedRange.Rows.Count;
      LastCol := Worksheet.UsedRange.Columns.Count;

      // Configurer la grille
      Grille.RowCount := LastRow;
      Grille.ColCount := LastCol;

      // Lire les données
      for i := 1 to LastRow do
      begin
        for j := 1 to LastCol do
        begin
          Grille.Cells[j - 1, i - 1] :=
            VarToStr(Worksheet.Cells[i, j].Value);
        end;
      end;
    finally
      Workbook.Close(False);
    end;
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;

// Utilisation
procedure TForm1.Button3Click(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    try
      LireExcelViaOLE(OpenDialog1.FileName, StringGrid1);
      ShowMessage('Fichier Excel chargé');
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message +
                   #13#10'Excel doit être installé sur ce PC.');
    end;
  end;
end;
```

### Écriture de fichier Excel via OLE

```pascal
procedure EcrireExcelViaOLE(const NomFichier: string; Grille: TStringGrid);  
var  
  Excel, Workbook, Worksheet: Variant;
  i, j: Integer;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;
    Excel.DisplayAlerts := False;

    // Créer un nouveau classeur
    Workbook := Excel.Workbooks.Add;
    try
      Worksheet := Workbook.Worksheets[1];

      // Écrire les données
      for i := 0 to Grille.RowCount - 1 do
      begin
        for j := 0 to Grille.ColCount - 1 do
        begin
          Worksheet.Cells[i + 1, j + 1].Value := Grille.Cells[j, i];
        end;
      end;

      // Mettre en forme l'en-tête (première ligne en gras)
      Worksheet.Rows[1].Font.Bold := True;

      // Ajuster automatiquement la largeur des colonnes
      Worksheet.Columns.AutoFit;

      // Sauvegarder
      Workbook.SaveAs(NomFichier);
    finally
      Workbook.Close(False);
    end;
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;

// Utilisation
procedure TForm1.SauvegarderExcel;  
begin  
  if SaveDialog1.Execute then
  begin
    try
      EcrireExcelViaOLE(SaveDialog1.FileName, StringGrid1);
      ShowMessage('Fichier Excel créé');
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

### Mise en forme avancée avec OLE

```pascal
procedure CreerExcelAvecMiseEnForme(const NomFichier: string);  
var  
  Excel, Workbook, Worksheet, Range: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;

    Workbook := Excel.Workbooks.Add;
    try
      Worksheet := Workbook.Worksheets[1];

      // En-tête
      Worksheet.Cells[1, 1].Value := 'Nom';
      Worksheet.Cells[1, 2].Value := 'Prénom';
      Worksheet.Cells[1, 3].Value := 'Âge';
      Worksheet.Cells[1, 4].Value := 'Salaire';

      // Mise en forme de l'en-tête
      Range := Worksheet.Range['A1:D1'];
      Range.Font.Bold := True;
      Range.Font.Size := 12;
      Range.Interior.Color := $00CCFFFF; // Jaune clair
      Range.HorizontalAlignment := -4108; // xlCenter

      // Données
      Worksheet.Cells[2, 1].Value := 'Dupont';
      Worksheet.Cells[2, 2].Value := 'Jean';
      Worksheet.Cells[2, 3].Value := 30;
      Worksheet.Cells[2, 4].Value := 45000;

      Worksheet.Cells[3, 1].Value := 'Martin';
      Worksheet.Cells[3, 2].Value := 'Marie';
      Worksheet.Cells[3, 3].Value := 28;
      Worksheet.Cells[3, 4].Value := 48000;

      // Format des salaires (monétaire)
      Range := Worksheet.Range['D2:D3'];
      Range.NumberFormat := '#,##0.00 €';

      // Bordures
      Range := Worksheet.Range['A1:D3'];
      Range.Borders.LineStyle := 1; // xlContinuous

      // Ajuster les colonnes
      Worksheet.Columns.AutoFit;

      // Ajouter une formule (total des salaires)
      Worksheet.Cells[4, 3].Value := 'Total :';
      Worksheet.Cells[4, 3].Font.Bold := True;
      Worksheet.Cells[4, 4].Formula := '=SUM(D2:D3)';
      Worksheet.Cells[4, 4].Font.Bold := True;

      Workbook.SaveAs(NomFichier);
    finally
      Workbook.Close(False);
    end;
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

---

## Méthode 2 : Sans Excel installé (lecture uniquement)

Pour lire des fichiers Excel sans avoir Excel installé, vous pouvez :

### Convertir XLSX en CSV d'abord

Les fichiers .xlsx sont en fait des archives ZIP contenant des fichiers XML. On peut les extraire et lire le XML.

```pascal
uses
  System.Zip;

procedure ExtraireXLSXEnCSV(const FichierXLSX, FichierCSV: string);  
var  
  ZipFile: TZipFile;
  XMLContent: string;
  Bytes: TBytes;
begin
  // Note: Ceci est une approche simplifiée
  // En réalité, le format XLSX est très complexe

  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(FichierXLSX, zmRead);

    // Extraire le fichier sheet1.xml (première feuille)
    // TZipFile.Read retourne les données dans un TBytes
    ZipFile.Read('xl/worksheets/sheet1.xml', Bytes);
    XMLContent := TEncoding.UTF8.GetString(Bytes);

    // Parser le XML et convertir en CSV
    // (code complexe, nécessite un parser XML complet)

    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;
```

**Note :** La lecture directe de XLSX est complexe. Il est recommandé d'utiliser une bibliothèque tierce spécialisée.

---

## Exemples pratiques complets

### Exemple 1 : Importateur de contacts CSV

```pascal
type
  TContact = record
    Nom: string;
    Prenom: string;
    Email: string;
    Telephone: string;
  end;

  TContactImporter = class
  private
    FContacts: TList<TContact>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ImportFromCSV(const FileName: string);
    procedure ExportToCSV(const FileName: string);
    function GetContact(Index: Integer): TContact;
    function Count: Integer;

    property Contacts: TList<TContact> read FContacts;
  end;

constructor TContactImporter.Create;  
begin  
  inherited;
  FContacts := TList<TContact>.Create;
end;

destructor TContactImporter.Destroy;  
begin  
  FContacts.Free;
  inherited;
end;

procedure TContactImporter.ImportFromCSV(const FileName: string);  
var  
  CSV: TCSVReader;
  i: Integer;
  Contact: TContact;
begin
  FContacts.Clear;

  CSV := TCSVReader.Create(FileName);
  try
    CSV.Delimiter := ';';
    CSV.HasHeader := True;
    CSV.Load;

    for i := 0 to CSV.RowCount - 1 do
    begin
      Contact.Nom := CSV.GetValueByHeader(i, 'Nom');
      Contact.Prenom := CSV.GetValueByHeader(i, 'Prénom');
      Contact.Email := CSV.GetValueByHeader(i, 'Email');
      Contact.Telephone := CSV.GetValueByHeader(i, 'Téléphone');

      FContacts.Add(Contact);
    end;
  finally
    CSV.Free;
  end;
end;

procedure TContactImporter.ExportToCSV(const FileName: string);  
var  
  Lignes: TStringList;
  i: Integer;
  Contact: TContact;
begin
  Lignes := TStringList.Create;
  try
    // En-tête
    Lignes.Add('Nom;Prénom;Email;Téléphone');

    // Données
    for i := 0 to FContacts.Count - 1 do
    begin
      Contact := FContacts[i];
      Lignes.Add(Format('%s;%s;%s;%s',
        [Contact.Nom, Contact.Prenom, Contact.Email, Contact.Telephone]));
    end;

    Lignes.SaveToFile(FileName, TEncoding.UTF8);
  finally
    Lignes.Free;
  end;
end;

function TContactImporter.GetContact(Index: Integer): TContact;  
begin  
  if (Index >= 0) and (Index < FContacts.Count) then
    Result := FContacts[Index]
  else
    Result := Default(TContact);
end;

function TContactImporter.Count: Integer;  
begin  
  Result := FContacts.Count;
end;

// Utilisation
procedure TForm1.ImporterContacts;  
var  
  Importer: TContactImporter;
  i: Integer;
  Contact: TContact;
begin
  if OpenDialog1.Execute then
  begin
    Importer := TContactImporter.Create;
    try
      Importer.ImportFromCSV(OpenDialog1.FileName);

      ListBox1.Items.Clear;
      for i := 0 to Importer.Count - 1 do
      begin
        Contact := Importer.GetContact(i);
        ListBox1.Items.Add(Format('%s %s - %s',
          [Contact.Prenom, Contact.Nom, Contact.Email]));
      end;

      ShowMessage(Format('%d contacts importés', [Importer.Count]));
    finally
      Importer.Free;
    end;
  end;
end;
```

### Exemple 2 : Générateur de rapports Excel

```pascal
type
  TExcelReportGenerator = class
  private
    FExcel: Variant;
    FWorkbook: Variant;
    FWorksheet: Variant;
    FCurrentRow: Integer;

    procedure InitializeExcel;
    procedure FinalizeExcel;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartReport(const Title: string);
    procedure AddHeader(const Headers: TArray<string>);
    procedure AddRow(const Values: TArray<Variant>);
    procedure AddSummary(const Label_: string; const Formula: string);
    procedure SaveReport(const FileName: string);
  end;

constructor TExcelReportGenerator.Create;  
begin  
  inherited;
  FCurrentRow := 1;
  InitializeExcel;
end;

destructor TExcelReportGenerator.Destroy;  
begin  
  FinalizeExcel;
  inherited;
end;

procedure TExcelReportGenerator.InitializeExcel;  
begin  
  FExcel := CreateOleObject('Excel.Application');
  FExcel.Visible := False;
  FExcel.DisplayAlerts := False;
  FWorkbook := FExcel.Workbooks.Add;
  FWorksheet := FWorkbook.Worksheets[1];
end;

procedure TExcelReportGenerator.FinalizeExcel;  
begin  
  if not VarIsEmpty(FWorkbook) then
    FWorkbook.Close(False);

  if not VarIsEmpty(FExcel) then
  begin
    FExcel.Quit;
    FExcel := Unassigned;
  end;
end;

procedure TExcelReportGenerator.StartReport(const Title: string);  
var  
  Range: Variant;
begin
  // Titre du rapport
  FWorksheet.Cells[FCurrentRow, 1].Value := Title;

  Range := FWorksheet.Range[
    FWorksheet.Cells[FCurrentRow, 1],
    FWorksheet.Cells[FCurrentRow, 5]];
  Range.Merge;
  Range.Font.Bold := True;
  Range.Font.Size := 16;
  Range.HorizontalAlignment := -4108; // xlCenter

  Inc(FCurrentRow, 2);
end;

procedure TExcelReportGenerator.AddHeader(const Headers: TArray<string>);  
var  
  i: Integer;
  Range: Variant;
begin
  // Écrire les en-têtes
  for i := 0 to High(Headers) do
    FWorksheet.Cells[FCurrentRow, i + 1].Value := Headers[i];

  // Mise en forme
  Range := FWorksheet.Range[
    FWorksheet.Cells[FCurrentRow, 1],
    FWorksheet.Cells[FCurrentRow, Length(Headers)]];
  Range.Font.Bold := True;
  Range.Interior.Color := $00E0E0E0; // Gris clair
  Range.Borders.LineStyle := 1;

  Inc(FCurrentRow);
end;

procedure TExcelReportGenerator.AddRow(const Values: TArray<Variant>);  
var  
  i: Integer;
begin
  for i := 0 to High(Values) do
    FWorksheet.Cells[FCurrentRow, i + 1].Value := Values[i];

  Inc(FCurrentRow);
end;

procedure TExcelReportGenerator.AddSummary(const Label_: string;
                                           const Formula: string);
var
  Range: Variant;
begin
  Inc(FCurrentRow);

  FWorksheet.Cells[FCurrentRow, 1].Value := Label_;
  FWorksheet.Cells[FCurrentRow, 1].Font.Bold := True;

  FWorksheet.Cells[FCurrentRow, 2].Formula := Formula;
  FWorksheet.Cells[FCurrentRow, 2].Font.Bold := True;

  Range := FWorksheet.Range[
    FWorksheet.Cells[FCurrentRow, 1],
    FWorksheet.Cells[FCurrentRow, 2]];
  Range.Interior.Color := $0000FFFF; // Jaune
end;

procedure TExcelReportGenerator.SaveReport(const FileName: string);  
begin  
  // Ajuster les colonnes
  FWorksheet.Columns.AutoFit;

  // Sauvegarder
  FWorkbook.SaveAs(FileName);
end;

// Utilisation
procedure TForm1.GenererRapportVentes;  
var  
  Report: TExcelReportGenerator;
begin
  Report := TExcelReportGenerator.Create;
  try
    Report.StartReport('Rapport de Ventes - ' +
                      FormatDateTime('mmmm yyyy', Now));

    Report.AddHeader(['Produit', 'Quantité', 'Prix Unitaire', 'Total']);

    Report.AddRow(['Ordinateur Portable', 15, 899.99, '=B4*C4']);
    Report.AddRow(['Souris Sans Fil', 50, 29.99, '=B5*C5']);
    Report.AddRow(['Clavier Mécanique', 30, 149.99, '=B6*C6']);

    Report.AddSummary('Total Général :', '=SUM(D4:D6)');

    Report.SaveReport('C:\Rapports\ventes.xlsx');

    ShowMessage('Rapport généré avec succès');
  finally
    Report.Free;
  end;
end;
```

### Exemple 3 : Convertisseur CSV ↔ Excel

```pascal
type
  TFileConverter = class
    class procedure CSVToExcel(const CSVFile, ExcelFile: string);
    class procedure ExcelToCSV(const ExcelFile, CSVFile: string);
  end;

class procedure TFileConverter.CSVToExcel(const CSVFile, ExcelFile: string);  
var  
  CSV: TCSVReader;
  Excel, Workbook, Worksheet: Variant;
  i, j: Integer;
begin
  // Lire le CSV
  CSV := TCSVReader.Create(CSVFile);
  try
    CSV.Delimiter := ';';
    CSV.HasHeader := True;
    CSV.Load;

    // Créer Excel
    Excel := CreateOleObject('Excel.Application');
    try
      Excel.Visible := False;
      Workbook := Excel.Workbooks.Add;
      try
        Worksheet := Workbook.Worksheets[1];

        // Écrire l'en-tête
        for j := 0 to CSV.ColCount - 1 do
          Worksheet.Cells[1, j + 1].Value := CSV.Headers[j];

        Worksheet.Rows[1].Font.Bold := True;

        // Écrire les données
        for i := 0 to CSV.RowCount - 1 do
        begin
          for j := 0 to CSV.ColCount - 1 do
            Worksheet.Cells[i + 2, j + 1].Value := CSV.GetValue(i, j);
        end;

        Worksheet.Columns.AutoFit;
        Workbook.SaveAs(ExcelFile);
      finally
        Workbook.Close(False);
      end;
    finally
      Excel.Quit;
      Excel := Unassigned;
    end;
  finally
    CSV.Free;
  end;
end;

class procedure TFileConverter.ExcelToCSV(const ExcelFile, CSVFile: string);  
var  
  Excel, Workbook, Worksheet: Variant;
  LastRow, LastCol: Integer;
  i, j: Integer;
  Lignes: TStringList;
  Ligne: string;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;
    Workbook := Excel.Workbooks.Open(ExcelFile);
    try
      Worksheet := Workbook.Worksheets[1];

      LastRow := Worksheet.UsedRange.Rows.Count;
      LastCol := Worksheet.UsedRange.Columns.Count;

      Lignes := TStringList.Create;
      try
        for i := 1 to LastRow do
        begin
          Ligne := '';
          for j := 1 to LastCol do
          begin
            if j > 1 then
              Ligne := Ligne + ';';
            Ligne := Ligne + VarToStr(Worksheet.Cells[i, j].Value);
          end;
          Lignes.Add(Ligne);
        end;

        Lignes.SaveToFile(CSVFile, TEncoding.UTF8);
      finally
        Lignes.Free;
      end;
    finally
      Workbook.Close(False);
    end;
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;

// Utilisation
procedure TForm1.ConvertirFichiers;  
begin  
  // CSV vers Excel
  TFileConverter.CSVToExcel('donnees.csv', 'donnees.xlsx');
  ShowMessage('CSV converti en Excel');

  // Excel vers CSV
  TFileConverter.ExcelToCSV('rapport.xlsx', 'rapport.csv');
  ShowMessage('Excel converti en CSV');
end;
```

---

## Gestion des erreurs et validation

### Valider un fichier CSV

```pascal
function ValidateCSV(const FileName: string;
                     ExpectedColumns: Integer): Boolean;
var
  Lignes: TStringList;
  PremiereLigne: string;
  NbColonnes: Integer;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    ShowMessage('Fichier introuvable');
    Exit;
  end;

  Lignes := TStringList.Create;
  try
    try
      Lignes.LoadFromFile(FileName, TEncoding.UTF8);

      if Lignes.Count = 0 then
      begin
        ShowMessage('Fichier vide');
        Exit;
      end;

      PremiereLigne := Lignes[0];
      NbColonnes := Length(PremiereLigne.Split([';']));

      if NbColonnes <> ExpectedColumns then
      begin
        ShowMessage(Format('Nombre de colonnes incorrect. Attendu : %d, Trouvé : %d',
          [ExpectedColumns, NbColonnes]));
        Exit;
      end;

      Result := True;
    except
      on E: Exception do
        ShowMessage('Erreur de lecture : ' + E.Message);
    end;
  finally
    Lignes.Free;
  end;
end;
```

### Vérifier Excel installé

```pascal
function IsExcelInstalled: Boolean;  
var  
  Excel: Variant;
begin
  Result := False;
  try
    Excel := CreateOleObject('Excel.Application');
    Excel.Quit;
    Excel := Unassigned;
    Result := True;
  except
    Result := False;
  end;
end;

// Utilisation
procedure TForm1.Button4Click(Sender: TObject);  
begin  
  if IsExcelInstalled then
    ShowMessage('Excel est installé')
  else
    ShowMessage('Excel n''est pas installé sur ce PC.' + #13#10 +
               'Utilisez l''export CSV à la place.');
end;
```

---

## Bonnes pratiques

### 1. Toujours spécifier l'encodage

```pascal
// BON
Lignes.SaveToFile('data.csv', TEncoding.UTF8);

// MAUVAIS (peut causer des problèmes avec les accents)
Lignes.SaveToFile('data.csv');
```

### 2. Gérer les caractères spéciaux dans CSV

```pascal
// Toujours échapper les champs contenant des délimiteurs
if (Pos(',', Champ) > 0) or (Pos('"', Champ) > 0) then
  Champ := '"' + StringReplace(Champ, '"', '""', [rfReplaceAll]) + '"';
```

### 3. Fermer proprement Excel

```pascal
try
  // Utiliser Excel
finally
  if not VarIsEmpty(Workbook) then
    Workbook.Close(False);
  if not VarIsEmpty(Excel) then
  begin
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

### 4. Prévoir des limites

```pascal
const
  MAX_CSV_ROWS = 100000; // Limite pour éviter les problèmes mémoire

if Lignes.Count > MAX_CSV_ROWS then  
begin  
  ShowMessage('Fichier trop volumineux. Maximum : ' +
             IntToStr(MAX_CSV_ROWS) + ' lignes');
  Exit;
end;
```

### 5. Informer l'utilisateur

```pascal
procedure ImporterAvecProgression(const FileName: string);  
var  
  CSV: TCSVReader;
  i: Integer;
begin
  CSV := TCSVReader.Create(FileName);
  try
    CSV.Load;

    ProgressBar1.Max := CSV.RowCount;

    for i := 0 to CSV.RowCount - 1 do
    begin
      // Traiter la ligne
      ProgressBar1.Position := i + 1;
      Label1.Caption := Format('Importation... %d/%d',
        [i + 1, CSV.RowCount]);
      Application.ProcessMessages;
    end;
  finally
    CSV.Free;
  end;
end;
```

---

## Comparaison CSV vs Excel

| Critère | CSV | Excel |
|---------|-----|-------|
| **Simplicité** | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **Taille fichier** | Petit | Gros |
| **Compatibilité** | Universelle | Nécessite Excel/librairie |
| **Mise en forme** | Aucune | Complète |
| **Formules** | Non | Oui |
| **Multi-feuilles** | Non | Oui |
| **Vitesse lecture** | Très rapide | Plus lent |
| **Pour débuter** | ✅ Recommandé | ⚠️ Plus complexe |

### Quand utiliser quoi ?

**CSV - Choisir pour :**
- Import/export simple de données
- Maximum de compatibilité
- Fichiers volumineux
- Traitement automatisé
- Pas besoin de mise en forme

**Excel - Choisir pour :**
- Rapports avec mise en forme
- Graphiques et visualisations
- Formules de calcul
- Présentation aux utilisateurs
- Multi-feuilles nécessaires

---

## Résumé

Dans ce chapitre, vous avez découvert la manipulation de fichiers CSV et Excel :

**CSV (Comma-Separated Values) :**
- Format texte simple et universel
- Lecture et écriture faciles
- Gestion des délimiteurs
- Classe TCSVReader complète
- Idéal pour l'échange de données

**Excel :**
- Format riche avec mise en forme
- OLE Automation (nécessite Excel installé)
- Création de rapports professionnels
- Formules et graphiques
- Plus complexe à manipuler

**Compétences acquises :**
- Lire et écrire des CSV
- Parser avec différents délimiteurs
- Détecter automatiquement le format
- Créer et manipuler des fichiers Excel
- Générer des rapports formatés
- Convertir entre formats

**Bonnes pratiques :**
- Toujours utiliser UTF-8
- Échapper les caractères spéciaux
- Valider les fichiers
- Gérer les erreurs
- Informer l'utilisateur
- Fermer proprement les ressources

Ces formats sont essentiels pour l'import/export de données dans vos applications professionnelles Delphi !

⏭️ [Accès aux bases de données MySQL/MariaDB](/08-acces-aux-bases-de-donnees-mysql-mariadb/README.md)
