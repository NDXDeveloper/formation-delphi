# 9.1 Composants d'impression natifs

L'impression est une fonctionnalité essentielle pour de nombreuses applications professionnelles. Delphi propose plusieurs composants natifs qui facilitent l'implémentation des fonctionnalités d'impression sans avoir recours à des bibliothèques tierces. Dans cette section, nous allons explorer ces composants et apprendre à les utiliser efficacement.

## Concepts de base de l'impression

Avant de plonger dans les composants spécifiques, comprenons quelques concepts fondamentaux de l'impression sous Windows :

- **Imprimante** : Périphérique physique ou virtuel capable de produire des documents imprimés
- **Spooler** : Service de Windows qui gère la file d'attente des travaux d'impression
- **Contexte d'impression** : Surface virtuelle sur laquelle on dessine le contenu à imprimer
- **Jobs d'impression** : Tâches d'impression individuelles envoyées à l'imprimante

## Les composants natifs d'impression

### TPrinter

Le composant **TPrinter** est la pierre angulaire du système d'impression de Delphi. Il n'est pas visible dans la palette de composants car c'est une classe globale accessible via la fonction `Printer` du module `Printers`.

```pascal
uses
  Printers;

procedure TForm1.ImprimerTexteSimple;
begin
  Printer.BeginDoc;
  try
    Printer.Canvas.Font.Size := 12;
    Printer.Canvas.Font.Name := 'Arial';
    Printer.Canvas.TextOut(100, 100, 'Mon premier texte imprimé avec Delphi!');
  finally
    Printer.EndDoc;
  end;
end;
```

Le composant `Printer` permet notamment :
- De sélectionner l'imprimante à utiliser
- De configurer les paramètres d'impression
- D'accéder au canevas (`Canvas`) de l'imprimante
- De démarrer et terminer des travaux d'impression

### TPrinterSetupDialog

Ce composant affiche la boîte de dialogue standard de Windows permettant à l'utilisateur de sélectionner une imprimante et ses paramètres :

```pascal
procedure TForm1.btnConfigImprimanteClick(Sender: TObject);
var
  PrinterSetupDialog: TPrinterSetupDialog;
begin
  PrinterSetupDialog := TPrinterSetupDialog.Create(nil);
  try
    if PrinterSetupDialog.Execute then
    begin
      // L'utilisateur a validé les paramètres d'impression
      ShowMessage('Imprimante sélectionnée : ' + Printer.Printers[Printer.PrinterIndex]);
    end;
  finally
    PrinterSetupDialog.Free;
  end;
end;
```

### TPrintDialog

Le composant **TPrintDialog** affiche la boîte de dialogue standard "Imprimer" de Windows. Il permet à l'utilisateur de choisir l'imprimante, le nombre de copies et les pages à imprimer :

![Boîte de dialogue Imprimer](/assets/images/print-dialog.png)

```pascal
procedure TForm1.btnImpressionClick(Sender: TObject);
var
  PrintDialog: TPrintDialog;
begin
  PrintDialog := TPrintDialog.Create(nil);
  try
    // Configuration initiale du dialogue
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := 10;
    PrintDialog.Copies := 1;

    // Affichage du dialogue
    if PrintDialog.Execute then
    begin
      // Récupération des options choisies par l'utilisateur
      if PrintDialog.PrintRange = prAllPages then
        ImprimerToutesLesPages
      else if PrintDialog.PrintRange = prSelection then
        ImprimerSelection
      else
        ImprimerPagesSpecifiques(PrintDialog.FromPage, PrintDialog.ToPage);

      // Nombre de copies
      FNombreCopies := PrintDialog.Copies;
    end;
  finally
    PrintDialog.Free;
  end;
end;
```

Si vous déposez ce composant sur un formulaire, vous pouvez le configurer via l'Object Inspector et l'utiliser directement.

## Impression directe avec Canvas

Le canevas d'impression (`Printer.Canvas`) est similaire au canevas d'un formulaire (`Form.Canvas`). Vous pouvez utiliser les mêmes méthodes de dessin pour créer votre contenu à imprimer :

```pascal
procedure TForm1.ImprimerGraphiqueSimple;
begin
  Printer.BeginDoc;
  try
    // Définir les attributs du stylo et du pinceau
    Printer.Canvas.Pen.Width := 3;
    Printer.Canvas.Pen.Color := clBlue;
    Printer.Canvas.Brush.Color := clLightBlue;

    // Dessiner un rectangle
    Printer.Canvas.Rectangle(100, 100, 400, 200);

    // Dessiner du texte
    Printer.Canvas.Font.Size := 14;
    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.TextOut(120, 130, 'Graphique imprimé depuis Delphi');

    // Dessiner une ligne
    Printer.Canvas.MoveTo(100, 250);
    Printer.Canvas.LineTo(400, 250);
  finally
    Printer.EndDoc;
  end;
end;
```

## Impression multi-pages

Pour créer des documents de plusieurs pages, utilisez `Printer.NewPage` entre chaque page :

```pascal
procedure TForm1.ImprimerDocumentMultiPages;
var
  i: Integer;
begin
  Printer.BeginDoc;
  try
    // Première page
    Printer.Canvas.TextOut(100, 100, 'Page 1');

    // Pages suivantes
    for i := 2 to 5 do
    begin
      Printer.NewPage; // Passage à une nouvelle page
      Printer.Canvas.TextOut(100, 100, 'Page ' + IntToStr(i));
    end;
  finally
    Printer.EndDoc;
  end;
end;
```

## Impression avec prévisualisation

Delphi ne fournit pas de composant natif pour la prévisualisation d'impression. Cependant, vous pouvez créer une solution simple en dessinant sur un TImage avant d'imprimer :

```pascal
procedure TForm1.AperçuAvantImpression;
var
  Ratio: Double;
begin
  // Calcul du ratio entre l'écran et l'imprimante
  Ratio := Image1.Width / Printer.PageWidth;

  // Effacer l'image
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);

  // Dessiner le contenu avec mise à l'échelle
  Image1.Canvas.Font.Size := Round(12 * Ratio);
  Image1.Canvas.TextOut(
    Round(100 * Ratio),
    Round(100 * Ratio),
    'Aperçu du texte à imprimer'
  );

  // Note: dans une application réelle, vous utiliseriez le même code
  // pour dessiner sur l'aperçu et sur l'imprimante
end;
```

Pour une prévisualisation plus avancée, vous pouvez vous tourner vers des composants tiers comme FastReport ou QuickReport (voir section 9.3).

## Gestion des erreurs d'impression

Il est important de gérer correctement les erreurs qui peuvent survenir lors de l'impression :

```pascal
procedure TForm1.ImprimerAvecGestionErreurs;
begin
  try
    Printer.BeginDoc;
    try
      Printer.Canvas.TextOut(100, 100, 'Texte à imprimer');
    finally
      Printer.EndDoc;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''impression : ' + E.Message);
      // Journaliser l'erreur, informer l'utilisateur...
    end;
  end;
end;
```

## Sélection et configuration de l'imprimante

Pour lister les imprimantes disponibles et permettre à l'utilisateur d'en choisir une :

```pascal
procedure TForm1.RemplirListeImprimantes;
var
  i: Integer;
begin
  ComboBoxImprimantes.Items.Clear;

  for i := 0 to Printer.Printers.Count - 1 do
    ComboBoxImprimantes.Items.Add(Printer.Printers[i]);

  // Sélectionner l'imprimante par défaut
  ComboBoxImprimantes.ItemIndex := Printer.PrinterIndex;
end;

procedure TForm1.ComboBoxImprimantesChange(Sender: TObject);
begin
  // Changer l'imprimante active
  Printer.PrinterIndex := ComboBoxImprimantes.ItemIndex;
end;
```

## Orientation et taille du papier

Vous pouvez également modifier l'orientation et la taille du papier :

```pascal
procedure TForm1.DefinirParametresPage;
begin
  // Changer l'orientation (portrait/paysage)
  Printer.Orientation := poLandscape; // ou poPortrait

  // Note: La modification de la taille du papier nécessite d'accéder
  // aux API Windows d'impression ou d'utiliser un dialogue
end;
```

## Cas pratique : Impression d'une facture simple

Voici un exemple plus complet pour imprimer une facture :

```pascal
procedure TForm1.ImprimerFacture;
var
  Y: Integer;
begin
  // Vérifier si une imprimante est disponible
  if Printer.Printers.Count = 0 then
  begin
    ShowMessage('Aucune imprimante n''est installée!');
    Exit;
  end;

  Printer.BeginDoc;
  try
    // En-tête
    Printer.Canvas.Font.Size := 14;
    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.TextOut(100, 100, 'FACTURE N°2023-001');

    // Informations client
    Printer.Canvas.Font.Size := 10;
    Printer.Canvas.Font.Style := [];
    Y := 150;
    Printer.Canvas.TextOut(100, Y, 'Client: Jean Dupont');
    Y := Y + 20;
    Printer.Canvas.TextOut(100, Y, 'Adresse: 123 rue des Développeurs');
    Y := Y + 20;
    Printer.Canvas.TextOut(100, Y, 'Ville: Codevillage');
    Y := Y + 40;

    // Tableau des produits
    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.TextOut(100, Y, 'Désignation');
    Printer.Canvas.TextOut(400, Y, 'Quantité');
    Printer.Canvas.TextOut(500, Y, 'Prix unitaire');
    Printer.Canvas.TextOut(600, Y, 'Total');
    Y := Y + 20;

    // Ligne de séparation
    Printer.Canvas.MoveTo(100, Y);
    Printer.Canvas.LineTo(700, Y);
    Y := Y + 20;

    // Produits
    Printer.Canvas.Font.Style := [];
    Printer.Canvas.TextOut(100, Y, 'Développement application Delphi');
    Printer.Canvas.TextOut(400, Y, '1');
    Printer.Canvas.TextOut(500, Y, '1000 €');
    Printer.Canvas.TextOut(600, Y, '1000 €');
    Y := Y + 20;

    // Plus d'articles...
    Printer.Canvas.TextOut(100, Y, 'Formation Object Pascal');
    Printer.Canvas.TextOut(400, Y, '2');
    Printer.Canvas.TextOut(500, Y, '500 €');
    Printer.Canvas.TextOut(600, Y, '1000 €');
    Y := Y + 40;

    // Total
    Printer.Canvas.MoveTo(100, Y);
    Printer.Canvas.LineTo(700, Y);
    Y := Y + 20;

    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.TextOut(500, Y, 'Total:');
    Printer.Canvas.TextOut(600, Y, '2000 €');

    // Pied de page
    Y := Y + 100;
    Printer.Canvas.Font.Style := [];
    Printer.Canvas.Font.Size := 8;
    Printer.Canvas.TextOut(100, Y, 'Merci de votre confiance!');
  finally
    Printer.EndDoc;
  end;
end;
```

## Conseils et bonnes pratiques

1. **Toujours terminer un document d'impression** : Utilisez un bloc try..finally pour garantir que `Printer.EndDoc` est appelé.

2. **Tester la disponibilité d'une imprimante** : Vérifiez que `Printer.Printers.Count > 0` avant d'imprimer.

3. **Adapter à la résolution** : Utilisez `Printer.PageWidth` et `Printer.PageHeight` pour ajuster votre mise en page.

4. **Prévoir une marge de sécurité** : Les imprimantes ont souvent une zone non imprimable sur les bords.

5. **Découper en méthodes** : Pour les documents complexes, créez des méthodes séparées pour chaque section.

## Conclusion

Les composants natifs d'impression de Delphi offrent une solution simple et efficace pour les besoins d'impression de base. Pour des fonctionnalités plus avancées comme la prévisualisation intégrée ou des rapports complexes, envisagez d'utiliser des composants spécialisés comme FastReport ou QuickReport (voir sections suivantes).

Dans la prochaine section, nous explorerons comment créer un aperçu avant impression plus sophistiqué pour offrir une meilleure expérience utilisateur.
