🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.4 COM et ActiveX

## Introduction à COM

### Qu'est-ce que COM ?

**COM** (Component Object Model) est une technologie Microsoft qui permet à différents logiciels de communiquer entre eux, indépendamment du langage de programmation utilisé. C'est comme une langue commune que tous les programmes Windows peuvent comprendre.

Imaginez que vous ayez besoin d'utiliser Excel depuis votre application Delphi pour créer un graphique : COM est la technologie qui rend cela possible, sans que vous ayez besoin de comprendre le code interne d'Excel.

### Qu'est-ce qu'ActiveX ?

**ActiveX** est une extension de COM qui permet d'intégrer des composants visuels (contrôles) dans vos applications. Par exemple, vous pouvez intégrer un lecteur vidéo Windows Media Player directement dans votre formulaire Delphi.

### Pourquoi utiliser COM et ActiveX ?

**Réutilisation de code** : Utiliser des fonctionnalités existantes (Excel, Word, Internet Explorer) sans les recréer.

**Interopérabilité** : Faire communiquer différentes applications entre elles.

**Composants tiers** : Intégrer des contrôles ActiveX commerciaux ou gratuits.

**Automatisation** : Contrôler d'autres applications (générer des rapports Excel, envoyer des emails avec Outlook).

**Indépendance du langage** : Les composants COM peuvent être créés en C++, C#, VB et utilisés dans Delphi (et vice-versa).

## Concepts de base

### Interfaces

En COM, tout est basé sur le concept d'**interface**. Une interface est un contrat qui définit un ensemble de méthodes que l'objet doit implémenter.

```pascal
type
  // Exemple d'interface simple
  ICalculatrice = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function Additionner(a, b: Integer): Integer;
    function Soustraire(a, b: Integer): Integer;
  end;
```

**Caractéristiques importantes :**
- Une interface définit "quoi faire" mais pas "comment le faire"
- Chaque interface a un identifiant unique (GUID)
- Les interfaces ne peuvent pas être instanciées directement
- Une classe peut implémenter plusieurs interfaces

### GUID (Globally Unique Identifier)

Chaque interface COM possède un **GUID** unique, aussi appelé **IID** (Interface Identifier). C'est comme une carte d'identité qui garantit qu'il n'y a pas de confusion entre deux interfaces différentes.

```pascal
// Générer un nouveau GUID dans Delphi : Ctrl+Shift+G
const
  IID_MonInterface: TGUID = '{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}';
```

### IUnknown : L'interface de base

Toutes les interfaces COM héritent de **IUnknown**, l'interface de base qui fournit trois méthodes essentielles :

```pascal
type
  IUnknown = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult;
    function AddRef: Integer;
    function Release: Integer;
  end;
```

- **QueryInterface** : Demande une interface spécifique à l'objet
- **AddRef** : Incrémente le compteur de références
- **Release** : Décrémente le compteur et détruit l'objet si nécessaire

**Bonne nouvelle :** Delphi gère automatiquement ces méthodes pour vous !

### Comptage de références

COM utilise un système de comptage de références pour gérer la mémoire. Quand le compteur atteint zéro, l'objet se détruit automatiquement.

```pascal
var
  Obj: IMonInterface;
begin
  Obj := CreerObjet;  // AddRef appelé automatiquement
  // Utiliser Obj...
end; // Release appelé automatiquement, objet détruit si compteur = 0
```

## Utilisation de composants COM existants

### Automation avec IDispatch

**IDispatch** est une interface spéciale qui permet l'accès tardif (late binding) aux objets COM. C'est la méthode la plus simple pour contrôler des applications Office.

```pascal
uses
  ComObj;  // Unité essentielle pour COM

procedure CreerDocumentWord;  
var  
  Word: OleVariant;
begin
  // Créer une instance de Word
  Word := CreateOleObject('Word.Application');
  try
    // Rendre Word visible
    Word.Visible := True;

    // Créer un nouveau document
    Word.Documents.Add;

    // Ajouter du texte
    Word.Selection.TypeText('Bonjour depuis Delphi !');

    // Mettre en gras
    Word.Selection.Font.Bold := True;

  finally
    // Word continue de s'exécuter après
    Word := Unassigned;
  end;
end;
```

### Vérifier si une application COM est disponible

```pascal
uses
  ComObj;

function ApplicationCOMDisponible(const ProgID: string): Boolean;  
var  
  ClassID: TGUID;
begin
  Result := Succeeded(CLSIDFromProgID(PWideChar(ProgID), ClassID));
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  if ApplicationCOMDisponible('Excel.Application') then
    ShowMessage('Excel est installé')
  else
    ShowMessage('Excel n''est pas installé');
end;
```

### Se connecter à une instance existante

```pascal
uses
  ComObj;

procedure UtiliserWordExistant;  
var  
  Word: OleVariant;
begin
  try
    // Se connecter à une instance de Word déjà ouverte
    Word := GetActiveOleObject('Word.Application');

    // Ajouter du texte au document actif
    Word.Selection.TypeText('Texte ajouté depuis Delphi');

  except
    on E: Exception do
      ShowMessage('Aucune instance de Word active : ' + E.Message);
  end;
end;
```

## Automation Microsoft Office

### Contrôle d'Excel

#### Créer et remplir une feuille Excel

```pascal
uses
  ComObj;

procedure CreerFeuilleSuiviVentes;  
var  
  Excel, Workbook, Sheet: OleVariant;
  Row: Integer;
begin
  // Créer Excel
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := True;

    // Créer un nouveau classeur
    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    // Définir les en-têtes
    Sheet.Cells[1, 1] := 'Produit';
    Sheet.Cells[1, 2] := 'Quantité';
    Sheet.Cells[1, 3] := 'Prix';
    Sheet.Cells[1, 4] := 'Total';

    // Mettre en gras les en-têtes
    Sheet.Range['A1:D1'].Font.Bold := True;

    // Ajouter des données
    Sheet.Cells[2, 1] := 'Ordinateur';
    Sheet.Cells[2, 2] := 5;
    Sheet.Cells[2, 3] := 800;
    Sheet.Cells[2, 4] := '=B2*C2';  // Formule

    Sheet.Cells[3, 1] := 'Souris';
    Sheet.Cells[3, 2] := 20;
    Sheet.Cells[3, 3] := 25;
    Sheet.Cells[3, 4] := '=B3*C3';

    // Ajuster la largeur des colonnes
    Sheet.Columns.AutoFit;

    // Sauvegarder
    Workbook.SaveAs('C:\Ventes.xlsx');

  finally
    Excel := Unassigned;
  end;
end;
```

#### Lire des données depuis Excel

```pascal
procedure LireDonneesExcel(const Fichier: string);  
var  
  Excel, Workbook, Sheet: OleVariant;
  Row, Col: Integer;
  Valeur: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;  // Mode invisible

    // Ouvrir le fichier
    Workbook := Excel.Workbooks.Open(Fichier);
    Sheet := Workbook.Worksheets[1];

    // Lire les données (lignes 1 à 10, colonnes 1 à 3)
    for Row := 1 to 10 do
    begin
      for Col := 1 to 3 do
      begin
        Valeur := Sheet.Cells[Row, Col].Value;
        if not VarIsNull(Valeur) then
          Memo1.Lines.Add(Format('Cellule [%d,%d] = %s',
            [Row, Col, VarToStr(Valeur)]));
      end;
    end;

    // Fermer sans sauvegarder
    Workbook.Close(False);

  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

#### Créer un graphique Excel

```pascal
procedure CreerGraphiqueExcel;  
var  
  Excel, Workbook, Sheet, Chart: OleVariant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := True;

    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    // Données pour le graphique
    Sheet.Cells[1, 1] := 'Mois';
    Sheet.Cells[1, 2] := 'Ventes';
    Sheet.Cells[2, 1] := 'Janvier';
    Sheet.Cells[2, 2] := 1000;
    Sheet.Cells[3, 1] := 'Février';
    Sheet.Cells[3, 2] := 1500;
    Sheet.Cells[4, 1] := 'Mars';
    Sheet.Cells[4, 2] := 1200;

    // Créer le graphique
    Chart := Workbook.Charts.Add;
    Chart.ChartType := 51;  // xlColumnClustered
    Chart.SetSourceData(Sheet.Range['A1:B4']);
    Chart.HasTitle := True;
    Chart.ChartTitle.Text := 'Ventes trimestrielles';

  finally
    Excel := Unassigned;
  end;
end;
```

### Contrôle de Word

#### Créer un document formaté

```pascal
procedure CreerRapportWord;  
var  
  Word, Document, Selection: OleVariant;
begin
  Word := CreateOleObject('Word.Application');
  try
    Word.Visible := True;

    // Nouveau document
    Document := Word.Documents.Add;
    Selection := Word.Selection;

    // Titre
    Selection.Font.Size := 18;
    Selection.Font.Bold := True;
    Selection.TypeText('Rapport mensuel');
    Selection.TypeParagraph;
    Selection.TypeParagraph;

    // Contenu normal
    Selection.Font.Size := 12;
    Selection.Font.Bold := False;
    Selection.TypeText('Ceci est le contenu du rapport.');
    Selection.TypeParagraph;

    // Liste à puces
    Selection.Range.ListFormat.ApplyBulletDefault;
    Selection.TypeText('Premier point');
    Selection.TypeParagraph;
    Selection.TypeText('Deuxième point');
    Selection.TypeParagraph;
    Selection.TypeText('Troisième point');
    Selection.TypeParagraph;

    // Sauvegarder
    Document.SaveAs2('C:\Rapport.docx');

  finally
    Word := Unassigned;
  end;
end;
```

#### Insérer un tableau Word

```pascal
procedure InsererTableauWord;  
var  
  Word, Document, Table: OleVariant;
begin
  Word := CreateOleObject('Word.Application');
  try
    Word.Visible := True;
    Document := Word.Documents.Add;

    // Insérer un tableau 3x3
    Table := Document.Tables.Add(
      Word.Selection.Range,
      3,  // Nombre de lignes
      3   // Nombre de colonnes
    );

    // Remplir le tableau
    Table.Cell(1, 1).Range.Text := 'En-tête 1';
    Table.Cell(1, 2).Range.Text := 'En-tête 2';
    Table.Cell(1, 3).Range.Text := 'En-tête 3';

    Table.Cell(2, 1).Range.Text := 'Donnée A1';
    Table.Cell(2, 2).Range.Text := 'Donnée A2';
    Table.Cell(2, 3).Range.Text := 'Donnée A3';

    // Formater la première ligne
    Table.Rows[1].Range.Font.Bold := True;
    Table.Rows[1].Shading.BackgroundPatternColor := 15790320; // Gris clair

  finally
    Word := Unassigned;
  end;
end;
```

### Contrôle d'Outlook

#### Envoyer un email

```pascal
procedure EnvoyerEmail(const Destinataire, Sujet, Corps: string);  
var  
  Outlook, Mail: OleVariant;
begin
  Outlook := CreateOleObject('Outlook.Application');
  try
    // Créer un nouveau message
    Mail := Outlook.CreateItem(0);  // 0 = olMailItem

    Mail.To := Destinataire;
    Mail.Subject := Sujet;
    Mail.Body := Corps;

    // Ajouter une pièce jointe (optionnel)
    // Mail.Attachments.Add('C:\fichier.pdf');

    // Envoyer
    Mail.Send;

    ShowMessage('Email envoyé avec succès');
  finally
    Outlook := Unassigned;
  end;
end;

// Utilisation
procedure TForm1.ButtonEnvoyerClick(Sender: TObject);  
begin  
  EnvoyerEmail(
    'utilisateur@example.com',
    'Test depuis Delphi',
    'Ceci est un email envoyé automatiquement.'
  );
end;
```

#### Créer un rendez-vous

```pascal
procedure CreerRendezVous;  
var  
  Outlook, Appointment: OleVariant;
begin
  Outlook := CreateOleObject('Outlook.Application');
  try
    // Créer un rendez-vous
    Appointment := Outlook.CreateItem(1);  // 1 = olAppointmentItem

    Appointment.Subject := 'Réunion importante';
    Appointment.Location := 'Salle de conférence';
    Appointment.Start := EncodeDate(2024, 12, 15) + EncodeTime(14, 30, 0, 0);
    Appointment.Duration := 60;  // minutes
    Appointment.Body := 'Discussion sur le nouveau projet';
    Appointment.ReminderSet := True;
    Appointment.ReminderMinutesBeforeStart := 15;

    Appointment.Save;
    ShowMessage('Rendez-vous créé');

  finally
    Outlook := Unassigned;
  end;
end;
```

## Contrôles ActiveX

### Importer un contrôle ActiveX

Pour utiliser un contrôle ActiveX dans Delphi :

1. Menu **Component** → **Import Component**
2. Sélectionner **Import ActiveX Control**
3. Choisir le contrôle dans la liste
4. Cliquer sur **Install** pour l'ajouter à la palette

Delphi génère automatiquement une unité Delphi qui encapsule le contrôle ActiveX.

### Exemple : WebBrowser (Internet Explorer)

```pascal
uses
  SHDocVw;  // Unité générée pour le contrôle WebBrowser

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    EditURL: TEdit;
    ButtonNaviguer: TButton;
    ButtonRetour: TButton;
    ButtonAvant: TButton;
    procedure ButtonNaviguerClick(Sender: TObject);
    procedure ButtonRetourClick(Sender: TObject);
    procedure ButtonAvantClick(Sender: TObject);
  end;

procedure TForm1.ButtonNaviguerClick(Sender: TObject);  
begin  
  WebBrowser1.Navigate(EditURL.Text);
end;

procedure TForm1.ButtonRetourClick(Sender: TObject);  
begin  
  WebBrowser1.GoBack;
end;

procedure TForm1.ButtonAvantClick(Sender: TObject);  
begin  
  WebBrowser1.GoForward;
end;
```

### Exemple : Windows Media Player

```pascal
uses
  WMPLib_TLB;  // Unité générée pour Windows Media Player

type
  TForm1 = class(TForm)
    WindowsMediaPlayer1: TWindowsMediaPlayer;
    procedure FormCreate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configurer le lecteur
  WindowsMediaPlayer1.settings.autoStart := True;
  WindowsMediaPlayer1.settings.volume := 50;

  // Charger et lire une vidéo
  WindowsMediaPlayer1.URL := 'C:\Videos\ma_video.mp4';
end;
```

### Manipuler le contenu HTML du WebBrowser

```pascal
procedure RemplirPageHTML;  
var  
  Document: IHTMLDocument2;
  Body: IHTMLElement;
begin
  // Attendre que le document soit chargé
  WebBrowser1.Navigate('about:blank');
  while WebBrowser1.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  // Obtenir le document
  Document := WebBrowser1.Document as IHTMLDocument2;

  // Écrire du HTML
  Document.write('<html><body>');
  Document.write('<h1>Titre depuis Delphi</h1>');
  Document.write('<p>Ceci est du contenu HTML généré dynamiquement.</p>');
  Document.write('</body></html>');
  Document.close;
end;
```

## Créer un serveur COM simple

### Définir l'interface

```pascal
unit MonServeurCOM;

interface

uses
  ComObj, ActiveX;

type
  // Définir l'interface
  ICalculatrice = interface(IDispatch)
    ['{12345678-1234-1234-1234-123456789012}']
    function Additionner(a, b: Integer): Integer; safecall;
    function Multiplier(a, b: Double): Double; safecall;
  end;

  // Implémenter la classe
  TCalculatrice = class(TAutoObject, ICalculatrice)
  protected
    function Additionner(a, b: Integer): Integer; safecall;
    function Multiplier(a, b: Double): Double; safecall;
  end;

const
  Class_Calculatrice: TGUID = '{87654321-4321-4321-4321-210987654321}';

implementation

uses
  ComServ;

function TCalculatrice.Additionner(a, b: Integer): Integer;  
begin  
  Result := a + b;
end;

function TCalculatrice.Multiplier(a, b: Double): Double;  
begin  
  Result := a * b;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCalculatrice, Class_Calculatrice,
    ciMultiInstance, tmApartment);
end.
```

### Utiliser le serveur COM

```pascal
procedure UtiliserServeurCOM;  
var  
  Calc: OleVariant;
  Resultat: Integer;
begin
  // Créer une instance du serveur
  Calc := CreateOleObject('MonServeur.Calculatrice');
  try
    // Utiliser les méthodes
    Resultat := Calc.Additionner(10, 20);
    ShowMessage('10 + 20 = ' + IntToStr(Resultat));

    ShowMessage('3.5 * 2.0 = ' + FloatToStr(Calc.Multiplier(3.5, 2.0)));
  finally
    Calc := Unassigned;
  end;
end;
```

## Gestion des erreurs

### Intercepter les erreurs COM

```pascal
uses
  ComObj;

procedure GestionErreursCOM;  
var  
  Excel: OleVariant;
begin
  try
    Excel := CreateOleObject('Excel.Application');
    try
      // Code qui pourrait générer une erreur
      Excel.Workbooks.Open('fichier_inexistant.xlsx');
    finally
      Excel.Quit;
      Excel := Unassigned;
    end;
  except
    on E: EOleSysError do
      ShowMessage('Erreur COM: ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

### Vérifier le résultat d'une opération

```pascal
function OuvrirDocumentExcel(const Fichier: string): Boolean;  
var  
  Excel, Workbook: OleVariant;
begin
  Result := False;

  if not FileExists(Fichier) then
  begin
    ShowMessage('Fichier introuvable');
    Exit;
  end;

  try
    Excel := CreateOleObject('Excel.Application');
    try
      Workbook := Excel.Workbooks.Open(Fichier);
      Result := not VarIsEmpty(Workbook);

      if Result then
        ShowMessage('Document ouvert avec succès');
    finally
      Excel := Unassigned;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ouverture: ' + E.Message);
  end;
end;
```

## Gestion de la mémoire et des ressources

### Libération correcte des objets

```pascal
procedure BonnesGestionRessources;  
var  
  Excel, Workbook, Sheet: OleVariant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Workbook := Excel.Workbooks.Add;
    try
      Sheet := Workbook.Worksheets[1];

      // Utiliser Sheet...

    finally
      // Fermer le classeur
      Workbook.Close(False);
      Workbook := Unassigned;
    end;
  finally
    // Quitter Excel
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

### Éviter les fuites mémoire

```pascal
// MAUVAIS : Ne fait pas le ménage correctement
procedure MauvaiseGestion;  
var  
  Excel: OleVariant;
begin
  Excel := CreateOleObject('Excel.Application');
  Excel.Visible := True;
  // Excel reste en mémoire après la fin de la procédure !
end;

// BON : Libération correcte
procedure BonneGestion;  
var  
  Excel: OleVariant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := True;
    // Faire quelque chose...
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

## Cas d'usage avancés

### Génération de rapports Excel automatisée

```pascal
procedure GenererRapportVentes(Donnees: TDataSet);  
var  
  Excel, Workbook, Sheet: OleVariant;
  Row: Integer;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Excel.Visible := False;

    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    // En-têtes
    Sheet.Cells[1, 1] := 'Date';
    Sheet.Cells[1, 2] := 'Client';
    Sheet.Cells[1, 3] := 'Montant';
    Sheet.Range['A1:C1'].Font.Bold := True;

    // Données depuis le dataset
    Row := 2;
    Donnees.First;
    while not Donnees.Eof do
    begin
      Sheet.Cells[Row, 1] := Donnees.FieldByName('Date').AsString;
      Sheet.Cells[Row, 2] := Donnees.FieldByName('Client').AsString;
      Sheet.Cells[Row, 3] := Donnees.FieldByName('Montant').AsFloat;
      Inc(Row);
      Donnees.Next;
    end;

    // Total
    Sheet.Cells[Row, 2] := 'TOTAL:';
    Sheet.Cells[Row, 3] := Format('=SUM(C2:C%d)', [Row - 1]);
    Sheet.Cells[Row, 3].Font.Bold := True;

    // Mise en forme
    Sheet.Columns.AutoFit;
    Sheet.Range['C2:C' + IntToStr(Row)].NumberFormat := '#,##0.00 €';

    // Sauvegarder
    Workbook.SaveAs('C:\Rapport_' +
      FormatDateTime('yyyymmdd_hhnnss', Now) + '.xlsx');

    Excel.Visible := True;

  finally
    Excel := Unassigned;
  end;
end;
```

### Fusion de documents Word

```pascal
procedure FusionnerDocumentsWord(Fichiers: TStringList; Sortie: string);  
var  
  Word, DocPrincipal, DocSource: OleVariant;
  I: Integer;
begin
  if Fichiers.Count = 0 then Exit;

  Word := CreateOleObject('Word.Application');
  try
    Word.Visible := False;

    // Ouvrir le premier document
    DocPrincipal := Word.Documents.Open(Fichiers[0]);

    // Ajouter les autres documents
    for I := 1 to Fichiers.Count - 1 do
    begin
      // Aller à la fin du document
      Word.Selection.EndKey(6);  // wdStory

      // Insérer un saut de page
      Word.Selection.InsertBreak(7);  // wdPageBreak

      // Insérer le fichier
      Word.Selection.InsertFile(Fichiers[I]);
    end;

    // Sauvegarder
    DocPrincipal.SaveAs2(Sortie);
    DocPrincipal.Close;

    ShowMessage('Fusion terminée: ' + Sortie);

  finally
    Word.Quit;
    Word := Unassigned;
  end;
end;
```

### Extraction de données PDF via COM

Si Adobe Acrobat est installé avec le SDK :

```pascal
procedure LireTextePDF(const Fichier: string);  
var  
  AcroApp, AcroDoc, Page: OleVariant;
  I, NumPages: Integer;
  Texte: string;
begin
  try
    AcroApp := CreateOleObject('AcroExch.App');
    AcroDoc := CreateOleObject('AcroExch.AVDoc');

    if AcroDoc.Open(Fichier, '') then
    begin
      NumPages := AcroDoc.GetNumPages;

      for I := 0 to NumPages - 1 do
      begin
        Page := AcroDoc.AcquirePage(I);
        // Extraction du texte (nécessite des objets supplémentaires)
        // Code simplifié ici
      end;

      AcroDoc.Close;
    end;

  finally
    AcroApp := Unassigned;
  end;
end;
```

## Bonnes pratiques

### Encapsulation dans une classe

```pascal
type
  TExcelWrapper = class
  private
    FExcel: OleVariant;
    FWorkbook: OleVariant;
    FConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    function CreateWorkbook: Boolean;
    procedure SetCellValue(Row, Col: Integer; const Value: Variant);
    function GetCellValue(Row, Col: Integer): Variant;
    procedure SaveAs(const FileName: string);
    property Connected: Boolean read FConnected;
  end;

constructor TExcelWrapper.Create;  
begin  
  inherited;
  FConnected := False;
end;

destructor TExcelWrapper.Destroy;  
begin  
  Disconnect;
  inherited;
end;

function TExcelWrapper.Connect: Boolean;  
begin  
  try
    FExcel := CreateOleObject('Excel.Application');
    FConnected := True;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TExcelWrapper.Disconnect;  
begin  
  if FConnected then
  begin
    try
      if not VarIsEmpty(FWorkbook) then
        FWorkbook.Close(False);
      FExcel.Quit;
    finally
      FWorkbook := Unassigned;
      FExcel := Unassigned;
      FConnected := False;
    end;
  end;
end;
```

### Tests de disponibilité

```pascal
function TesterDisponibilite: Boolean;  
begin  
  Result := ApplicationCOMDisponible('Excel.Application') and
            ApplicationCOMDisponible('Word.Application') and
            ApplicationCOMDisponible('Outlook.Application');

  if not Result then
    ShowMessage('Certaines applications Office ne sont pas installées');
end;
```

### Gestion des versions Office

```pascal
function ObtenirVersionExcel: string;  
var  
  Excel: OleVariant;
begin
  try
    Excel := CreateOleObject('Excel.Application');
    try
      Result := Excel.Version;
    finally
      Excel := Unassigned;
    end;
  except
    Result := 'Non installé';
  end;
end;
```

## Débogage

### Activer le mode visible

Lors du développement, rendez les applications visibles pour voir ce qui se passe :

```pascal
Excel.Visible := True;  // Pour voir Excel pendant l'exécution
```

### Utiliser des points d'arrêt

Placez des points d'arrêt pour inspecter les valeurs OleVariant :

```pascal
var
  Valeur: OleVariant;
begin
  Valeur := Sheet.Cells[1, 1].Value;
  // Point d'arrêt ici pour inspecter Valeur
  ShowMessage(VarToStr(Valeur));
end;
```

### Journalisation des erreurs

```pascal
procedure LogErreurCOM(const Operation: string; E: Exception);  
begin  
  with TStringList.Create do
  try
    Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Add('Opération: ' + Operation);
    Add('Erreur: ' + E.Message);
    Add('------');
    SaveToFile('com_errors.log');
  finally
    Free;
  end;
end;
```

## Résumé

COM et ActiveX permettent l'interopérabilité entre applications Windows.

**Points clés :**

1. **COM** = Communication entre applications
2. **ActiveX** = Composants visuels réutilisables
3. Utilisez **OleVariant** pour l'automation simple
4. Toujours utiliser **try...finally** pour libérer les ressources
5. **CreateOleObject** crée une nouvelle instance
6. **GetActiveOleObject** se connecte à une instance existante
7. Vérifiez la **disponibilité** avant utilisation
8. Gérez les **erreurs** avec try...except
9. **Encapsulez** les appels COM dans des classes
10. Office Automation : Excel, Word, Outlook sont les plus utilisés
11. **GUID** identifie uniquement chaque interface
12. Utilisez **VarIsEmpty** et **VarIsNull** pour vérifier les variables OLE

COM est une technologie puissante mais complexe. Commencez par des exemples simples (automation Office) avant d'explorer des scénarios plus avancés comme la création de serveurs COM.

⏭️ [Intégration avec des services tiers](/14-utilisation-dapi-et-bibliotheques-externes/05-integration-avec-des-services-tiers.md)
