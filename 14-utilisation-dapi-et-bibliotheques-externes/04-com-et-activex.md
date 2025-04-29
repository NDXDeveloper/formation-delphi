# 14.4 COM et ActiveX

## Introduction

COM (Component Object Model) est une technologie Microsoft qui permet à différents composants logiciels de communiquer entre eux, même s'ils sont écrits dans des langages de programmation différents. ActiveX est une extension de COM qui ajoute des fonctionnalités supplémentaires, principalement pour les composants visuels et interactifs.

Dans ce chapitre, nous allons découvrir comment utiliser des composants COM et ActiveX dans vos applications Delphi, ainsi que comment créer vos propres composants COM que d'autres applications pourront utiliser.

## Pourquoi utiliser COM et ActiveX ?

Voici quelques avantages de COM et ActiveX :

1. **Interopérabilité** : Permet à votre application Delphi d'interagir avec des composants écrits dans d'autres langages (C++, C#, Visual Basic, etc.)
2. **Réutilisation** : Utilise des composants existants plutôt que de tout recoder
3. **Intégration** : S'intègre facilement avec d'autres applications Windows comme Microsoft Office
4. **Mise à jour dynamique** : Permet de mettre à jour des composants sans recompiler l'application entière

## Bases de COM

### Concepts clés de COM

Avant de plonger dans le code, voici quelques concepts importants à comprendre :

- **Interface** : Définit les méthodes qu'un objet COM peut exposer
- **GUID/UUID** : Identifiant unique global qui identifie chaque interface et classe COM
- **CoClass** : Une classe qui implémente une ou plusieurs interfaces COM
- **Serveur COM** : Un exécutable ou une DLL qui fournit des objets COM
- **Enregistrement COM** : Processus d'inscription des composants COM dans le registre Windows

### IUnknown : L'interface de base

Toutes les interfaces COM héritent de l'interface `IUnknown`, qui définit trois méthodes essentielles :

1. **QueryInterface** : Permet de demander à un objet COM s'il supporte une interface particulière
2. **AddRef** : Incrémente le compteur de références de l'objet
3. **Release** : Décrémente le compteur de références, et détruit l'objet quand il atteint zéro

Heureusement, Delphi gère automatiquement ces détails de bas niveau pour vous.

## Utilisation de composants COM existants

### Importation d'une bibliothèque de types

Pour utiliser un composant COM dans votre application Delphi, vous devez d'abord importer sa bibliothèque de types :

1. Dans Delphi, sélectionnez **Composant** > **Importer un composant**
2. Choisissez **Importer une bibliothèque de types**
3. Sélectionnez la bibliothèque COM que vous souhaitez utiliser (par exemple, "Microsoft Excel Object Library")
4. Cliquez sur **Créer une unité** pour générer une unité Delphi avec les déclarations d'interface

Delphi va créer une nouvelle unité contenant toutes les définitions d'interfaces et de classes nécessaires pour utiliser ce composant COM.

### Exemple : Utilisation de Microsoft Excel

Voici un exemple simple qui ouvre Microsoft Excel, crée un nouveau classeur et y ajoute quelques données :

```pascal
procedure TForm1.ButtonExcelClick(Sender: TObject);
var
  Excel: OleVariant;
  Workbook: OleVariant;
  Worksheet: OleVariant;
  Range: OleVariant;
begin
  try
    // Créer une instance d'Excel
    Excel := CreateOleObject('Excel.Application');

    // Rendre Excel visible
    Excel.Visible := True;

    // Ajouter un nouveau classeur
    Workbook := Excel.Workbooks.Add;

    // Sélectionner la première feuille
    Worksheet := Workbook.Worksheets[1];

    // Ajouter des titres
    Worksheet.Cells[1, 1] := 'Produit';
    Worksheet.Cells[1, 2] := 'Quantité';
    Worksheet.Cells[1, 3] := 'Prix';

    // Ajouter des données
    Worksheet.Cells[2, 1] := 'Moniteur';
    Worksheet.Cells[2, 2] := 5;
    Worksheet.Cells[2, 3] := 200;

    Worksheet.Cells[3, 1] := 'Clavier';
    Worksheet.Cells[3, 2] := 10;
    Worksheet.Cells[3, 3] := 50;

    // Mettre en forme les titres en gras
    Range := Worksheet.Range['A1:C1'];
    Range.Font.Bold := True;

    // Ajuster la largeur des colonnes
    Worksheet.Columns['A:C'].AutoFit;

    ShowMessage('Excel a été démarré avec succès !');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

Pour que ce code fonctionne, vous devez ajouter les unités suivantes à votre clause `uses` :

```pascal
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, ComObj;
```

### Utilisation d'une interface typée plutôt que OleVariant

L'exemple précédent utilisait des variables `OleVariant`, ce qui est simple mais n'offre pas la vérification des types à la compilation. Voici comment utiliser des interfaces typées après avoir importé la bibliothèque de types Excel :

```pascal
procedure TForm1.ButtonExcelTypedClick(Sender: TObject);
var
  Excel: ExcelApplication;  // Défini dans l'unité importée
  Workbook: _Workbook;
  Worksheet: _Worksheet;
  Range: Range;
begin
  try
    // Créer une instance d'Excel
    Excel := CoExcelApplication.Create;

    // Rendre Excel visible
    Excel.Visible := True;

    // Ajouter un nouveau classeur
    Workbook := Excel.Workbooks.Add(EmptyParam);

    // Sélectionner la première feuille
    Worksheet := Workbook.Worksheets[1] as _Worksheet;

    // Ajouter des titres
    Worksheet.Cells.Item[1, 1].Value := 'Produit';
    Worksheet.Cells.Item[1, 2].Value := 'Quantité';
    Worksheet.Cells.Item[1, 3].Value := 'Prix';

    // Mettre en forme les titres en gras
    Range := Worksheet.Range['A1:C1', EmptyParam];
    Range.Font.Bold := True;

    ShowMessage('Excel a été démarré avec succès (interfaces typées) !');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Utilisation de contrôles ActiveX

Les contrôles ActiveX sont des composants visuels COM qui peuvent être intégrés dans votre application Delphi.

### Importation d'un contrôle ActiveX

Pour utiliser un contrôle ActiveX dans votre application :

1. Dans Delphi, sélectionnez **Composant** > **Importer un composant**
2. Choisissez **Importer un contrôle ActiveX**
3. Sélectionnez le contrôle que vous souhaitez utiliser (par exemple, "Windows Media Player")
4. Cliquez sur **Installer** pour créer un nouveau composant Delphi encapsulant le contrôle ActiveX

Une fois installé, le contrôle apparaîtra dans la palette des composants de Delphi, généralement dans la catégorie "ActiveX".

### Exemple : Intégration du lecteur Windows Media Player

Après avoir importé le contrôle ActiveX de Windows Media Player, vous pouvez l'utiliser ainsi :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Assurez-vous d'avoir ajouté un composant TWindowsMediaPlayer nommé WMP1 au formulaire

  // Définir l'URL du média à lire
  WMP1.URL := 'C:\exemple\video.mp4';

  // Configurer les contrôles
  WMP1.uiMode := 'full';  // Afficher tous les contrôles

  // Lancer la lecture
  WMP1.controls.play;
end;

procedure TForm1.ButtonPlayClick(Sender: TObject);
begin
  WMP1.controls.play;
end;

procedure TForm1.ButtonPauseClick(Sender: TObject);
begin
  WMP1.controls.pause;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  WMP1.controls.stop;
end;
```

## Création de composants COM avec Delphi

Delphi facilite grandement la création de vos propres composants COM que d'autres applications pourront utiliser.

### Création d'un serveur COM simple

Voici les étapes pour créer un serveur COM simple :

1. Sélectionnez **Fichier** > **Nouveau** > **Autre**
2. Dans la boîte de dialogue, choisissez **ActiveX** > **Bibliothèque ActiveX**
3. Cliquez sur **OK** pour créer un nouveau projet

Ensuite, pour ajouter un objet COM :

1. Sélectionnez **Fichier** > **Nouveau** > **Autre**
2. Choisissez **ActiveX** > **Objet COM**
3. Donnez un nom à votre classe COM (par exemple, "MathLibrary")
4. Sélectionnez **Ajouter des méthodes par défaut**
5. Cliquez sur **OK**

Delphi génère un squelette de classe et de fichiers projet pour votre composant COM.

### Exemple : Création d'une bibliothèque mathématique COM

Voici un exemple de création d'une bibliothèque mathématique simple exposée via COM :

```pascal
// Dans l'unité d'interface générée
type
  IMathLibrary = interface(IDispatch)
    ['{VOTRE-GUID-GÉNÉRÉ}']
    function Add(A, B: Integer): Integer; safecall;
    function Subtract(A, B: Integer): Integer; safecall;
    function Multiply(A, B: Integer): Integer; safecall;
    function Divide(A, B: Integer): Double; safecall;
  end;

  TMathLibrary = class(TAutoObject, IMathLibrary)
  protected
    function Add(A, B: Integer): Integer; safecall;
    function Subtract(A, B: Integer): Integer; safecall;
    function Multiply(A, B: Integer): Integer; safecall;
    function Divide(A, B: Integer): Double; safecall;
  end;

// Dans l'unité d'implémentation
function TMathLibrary.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TMathLibrary.Subtract(A, B: Integer): Integer;
begin
  Result := A - B;
end;

function TMathLibrary.Multiply(A, B: Integer): Integer;
begin
  Result := A * B;
end;

function TMathLibrary.Divide(A, B: Integer): Double;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro');
  Result := A / B;
end;
```

### Enregistrement du serveur COM

Pour que d'autres applications puissent utiliser votre composant COM, vous devez l'enregistrer dans le registre Windows :

1. Compilez votre projet
2. Exécutez la DLL avec le paramètre `/regserver` :
   ```
   YourComServer.dll /regserver
   ```

Delphi génère automatiquement le code pour gérer l'enregistrement du serveur.

### Utilisation du serveur COM depuis une autre application

Une fois votre bibliothèque COM enregistrée, vous pouvez l'utiliser depuis n'importe quelle autre application compatible COM :

```pascal
// Dans une application cliente Delphi
procedure TForm1.ButtonCalculateClick(Sender: TObject);
var
  MathLib: Variant;
  Result: Integer;
begin
  try
    MathLib := CreateOleObject('YourProject.MathLibrary');

    Result := MathLib.Add(StrToInt(EditA.Text), StrToInt(EditB.Text));

    ShowMessage('Résultat : ' + IntToStr(Result));
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Création d'un contrôle ActiveX avec Delphi

Vous pouvez également créer vos propres contrôles ActiveX qui pourront être utilisés dans d'autres applications comme Internet Explorer, Visual Basic ou même d'autres applications Delphi.

### Étapes de création d'un contrôle ActiveX

1. Créez d'abord un composant VCL standard
2. Sélectionnez **Fichier** > **Nouveau** > **Autre**
3. Choisissez **ActiveX** > **Wrapper de contrôle ActiveX**
4. Sélectionnez votre composant VCL
5. Configurez les options selon vos besoins

### Exemple : Création d'un bouton ActiveX personnalisé

D'abord, créez un composant VCL personnalisé :

```pascal
// TFancyButton - un bouton amélioré
type
  TFancyButton = class(TButton)
  private
    FGradientStart: TColor;
    FGradientEnd: TColor;
    procedure SetGradientStart(const Value: TColor);
    procedure SetGradientEnd(const Value: TColor);
  protected
    procedure Paint; override;
  published
    property GradientStart: TColor read FGradientStart write SetGradientStart default clBlue;
    property GradientEnd: TColor read FGradientEnd write SetGradientEnd default clNavy;
  end;
```

Ensuite, transformez-le en contrôle ActiveX avec l'assistant Delphi. L'assistant génère les fichiers nécessaires pour exposer votre contrôle en tant qu'ActiveX.

## Bonnes pratiques et conseils

### Gestion de la mémoire et des références

COM utilise un système de comptage de références pour gérer la mémoire. Delphi gère automatiquement cela pour les objets COM que vous utilisez, mais voici quelques conseils :

1. Utilisez des blocs `try...finally` pour vous assurer que les objets COM sont libérés
2. Affectez `nil` aux variables d'interface COM quand vous avez fini de les utiliser
3. Soyez attentif aux références circulaires qui peuvent causer des fuites de mémoire

### Débogage des composants COM

Le débogage des applications COM peut être plus complexe que celui des applications standard :

1. Utilisez les outils de débogage intégrés de Delphi
2. Activez les options de débogage COM dans les options du projet
3. Utilisez `OleView` ou `ComView` pour inspecter les serveurs COM enregistrés
4. Vérifiez les entrées du registre en cas de problèmes d'enregistrement

### Gestion des erreurs COM

Les erreurs COM sont généralement représentées par des codes HRESULT. Delphi convertit ces codes en exceptions :

```pascal
try
  // Code utilisant COM
except
  on E: EOleSysError do
    ShowMessage('Erreur COM: ' + E.Message + ' (Code: ' + IntToStr(E.ErrorCode) + ')');
  on E: Exception do
    ShowMessage('Erreur: ' + E.Message);
end;
```

## Exemples pratiques avancés

### Automatisation de Microsoft Word

Voici un exemple plus complet d'automatisation de Microsoft Word :

```pascal
procedure CreateWordDocument;
var
  WordApp: OleVariant;
  Doc: OleVariant;
begin
  try
    // Créer une instance de Word
    WordApp := CreateOleObject('Word.Application');

    // Rendre Word visible
    WordApp.Visible := True;

    // Ajouter un nouveau document
    Doc := WordApp.Documents.Add;

    // Ajouter du texte
    WordApp.Selection.TypeText('Rapport généré par Delphi');
    WordApp.Selection.TypeParagraph;

    // Mettre en forme le texte
    WordApp.Selection.Font.Bold := True;
    WordApp.Selection.Font.Size := 16;
    WordApp.Selection.TypeText('Titre du document');
    WordApp.Selection.TypeParagraph;

    // Revenir à la mise en forme normale
    WordApp.Selection.Font.Bold := False;
    WordApp.Selection.Font.Size := 12;

    // Ajouter plus de texte
    WordApp.Selection.TypeText('Ceci est un exemple d''automatisation de Word depuis Delphi.');
    WordApp.Selection.TypeParagraph;

    // Insérer un tableau
    const RowCount = 3;
    const ColCount = 2;
    var Table := Doc.Tables.Add(WordApp.Selection.Range, RowCount, ColCount);

    // Remplir le tableau
    Table.Cell(1, 1).Range.Text := 'Produit';
    Table.Cell(1, 2).Range.Text := 'Prix';
    Table.Cell(2, 1).Range.Text := 'Produit A';
    Table.Cell(2, 2).Range.Text := '100 €';
    Table.Cell(3, 1).Range.Text := 'Produit B';
    Table.Cell(3, 2).Range.Text := '200 €';

    // Enregistrer le document
    Doc.SaveAs('C:\Temp\DocumentDelphi.docx');

    ShowMessage('Document Word créé avec succès !');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Création d'un serveur COM In-Process vs Out-of-Process

Delphi vous permet de créer deux types de serveurs COM :

1. **Serveur In-Process (DLL)** : S'exécute dans le même processus que le client
   - Avantage : Plus rapide et efficace
   - Inconvénient : Un plantage peut affecter l'application cliente

2. **Serveur Out-of-Process (EXE)** : S'exécute dans un processus séparé
   - Avantage : Meilleure isolation, un plantage n'affecte pas le client
   - Inconvénient : Communication plus lente

Pour créer un serveur Out-of-Process, choisissez **Fichier** > **Nouveau** > **Autre** > **ActiveX** > **Serveur ActiveX** au lieu de **Bibliothèque ActiveX**.

### Support des événements COM

Les objets COM peuvent également déclencher des événements que les clients peuvent intercepter. Pour cela, vous devez définir une interface de "sink" (récepteur d'événements) :

```pascal
// Dans le serveur COM
type
  IMathEvents = interface(IDispatch)
    ['{VOTRE-GUID-GENERE}']
    procedure OnCalculationComplete(Result: Integer); safecall;
    procedure OnError(const ErrorMessage: WideString); safecall;
  end;

  TMathLibrary = class(TAutoObject, IMathLibrary, IConnectionPointContainer)
  private
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    // ...
```

Du côté client, vous devez implémenter l'interface de sink et vous abonner aux événements.

## Conclusion

COM et ActiveX sont des technologies puissantes qui permettent une interopérabilité significative entre différentes applications Windows. Delphi offre un excellent support pour utiliser des composants COM existants et pour créer vos propres composants COM.

Les avantages principaux incluent :
- Réutilisation de code entre différents langages de programmation
- Intégration facile avec les applications Microsoft Office
- Architecture modulaire et extensible
- Support de mise à jour dynamique des composants

Bien que ces technologies soient un peu plus anciennes, elles restent très pertinentes pour le développement Windows, en particulier pour l'intégration avec des applications existantes et l'automatisation de tâches.

### Ressources supplémentaires

Pour en apprendre davantage sur COM et ActiveX en Delphi, voici quelques ressources utiles :

1. Documentation Delphi officielle
2. MSDN (Microsoft Developer Network) pour les spécifications COM
3. Exemples fournis avec Delphi dans le dossier "Exemples"

En maîtrisant COM et ActiveX, vous ajoutez un outil précieux à votre arsenal de développement Delphi, ouvrant la porte à une intégration transparente avec un large écosystème d'applications Windows.
