🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.1 Composants d'impression natifs

## Introduction

L'impression est une fonctionnalité essentielle dans de nombreuses applications de gestion. Delphi propose des composants natifs qui permettent d'imprimer facilement du contenu sans avoir besoin de bibliothèques externes. Dans cette section, nous allons découvrir ces composants et apprendre à les utiliser.

## Pourquoi utiliser les composants d'impression natifs ?

Les composants d'impression natifs de Delphi offrent plusieurs avantages :

- **Simplicité** : ils sont directement intégrés dans Delphi, pas besoin d'installation supplémentaire
- **Fiabilité** : ils utilisent l'API Windows standard pour l'impression
- **Légèreté** : ils n'alourdissent pas votre application
- **Compatibilité** : ils fonctionnent avec toutes les imprimantes installées sur le système

## Les composants principaux

### TPrinter : le composant central

`TPrinter` est l'objet principal pour gérer l'impression dans Delphi. Il n'est pas visible dans la palette de composants car il s'agit d'une classe accessible via le code.

**Accès à l'imprimante :**

```pascal
uses
  Printers;  // Unité nécessaire pour utiliser TPrinter

var
  MonImprimante: TPrinter;
begin
  MonImprimante := Printer;  // Accès à l'instance globale
end;
```

**Propriétés importantes de TPrinter :**

- **Canvas** : surface de dessin pour l'impression (comme pour un formulaire)
- **PageWidth** : largeur de la page en pixels
- **PageHeight** : hauteur de la page en pixels
- **Printers** : liste des imprimantes disponibles
- **PrinterIndex** : index de l'imprimante actuellement sélectionnée
- **Printing** : indique si une impression est en cours
- **Copies** : nombre de copies à imprimer

### TPrintDialog : dialogue de sélection d'imprimante

Le composant `TPrintDialog` affiche la boîte de dialogue standard Windows permettant à l'utilisateur de choisir une imprimante et de configurer les options d'impression.

**Emplacement dans la palette :**
Vous trouverez `TPrintDialog` dans l'onglet **Dialogs** de la palette de composants.

**Propriétés principales :**

- **MinPage** : numéro de la première page
- **MaxPage** : numéro de la dernière page
- **FromPage** : page de début sélectionnée
- **ToPage** : page de fin sélectionnée
- **Copies** : nombre de copies demandées
- **Options** : options d'affichage du dialogue

### TPrinterSetupDialog : configuration de l'imprimante

Le composant `TPrinterSetupDialog` affiche la boîte de dialogue de configuration de l'imprimante (format papier, orientation, etc.).

**Emplacement dans la palette :**
Également dans l'onglet **Dialogs**.

**Utilisation typique :**

```pascal
if PrinterSetupDialog1.Execute then
begin
  // L'utilisateur a validé, les paramètres sont appliqués
  ShowMessage('Configuration enregistrée');
end;
```

## Structure d'un processus d'impression

Une impression se déroule toujours selon le même schéma :

1. **Début** : appel de `BeginDoc`
2. **Contenu** : dessin sur le Canvas
3. **Nouvelle page** (si nécessaire) : appel de `NewPage`
4. **Fin** : appel de `EndDoc`

### Exemple de base

Voici un exemple simple d'impression d'un texte :

```pascal
procedure TForm1.btnImprimerClick(Sender: TObject);
begin
  Printer.BeginDoc;  // Début du document d'impression
  try
    // Impression du texte
    Printer.Canvas.TextOut(100, 100, 'Bonjour depuis Delphi !');
    Printer.Canvas.TextOut(100, 200, 'Ceci est une impression simple.');
  finally
    Printer.EndDoc;  // Fin du document (envoie à l'imprimante)
  end;
end;
```

**Explications :**

- `BeginDoc` initialise le document d'impression
- `Canvas.TextOut(X, Y, Texte)` écrit du texte à la position (X, Y)
- `EndDoc` finalise et envoie le document à l'imprimante
- Le bloc `try...finally` garantit que `EndDoc` sera toujours appelé

## Utilisation du TPrintDialog

Il est recommandé de toujours permettre à l'utilisateur de choisir son imprimante et ses options. Voici comment intégrer le dialogue d'impression :

```pascal
procedure TForm1.btnImprimerAvecDialogueClick(Sender: TObject);
begin
  // Configuration du dialogue
  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := 1;

  // Affichage du dialogue
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.TextOut(100, 100, 'Impression avec dialogue');
      Printer.Canvas.TextOut(100, 200, 'Imprimante : ' + Printer.Printers[Printer.PrinterIndex]);
    finally
      Printer.EndDoc;
    end;
  end
  else
  begin
    ShowMessage('Impression annulée');
  end;
end;
```

## Mise en forme du texte

Le Canvas de l'imprimante fonctionne comme celui d'un formulaire. Vous pouvez modifier la police, la taille, la couleur, etc.

### Propriétés de la police

```pascal
procedure TForm1.ImprimerAvecMiseEnForme;
begin
  Printer.BeginDoc;
  try
    // Titre en gras et grande taille
    Printer.Canvas.Font.Name := 'Arial';
    Printer.Canvas.Font.Size := 18;
    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.TextOut(100, 100, 'TITRE DU DOCUMENT');

    // Texte normal
    Printer.Canvas.Font.Size := 12;
    Printer.Canvas.Font.Style := [];
    Printer.Canvas.TextOut(100, 300, 'Contenu du document en taille normale');

    // Texte en italique
    Printer.Canvas.Font.Style := [fsItalic];
    Printer.Canvas.TextOut(100, 400, 'Texte en italique');
  finally
    Printer.EndDoc;
  end;
end;
```

### Propriétés utiles de Font

- **Name** : nom de la police (Arial, Times New Roman, etc.)
- **Size** : taille de la police
- **Style** : style (fsBold, fsItalic, fsUnderline, fsStrikeOut)
- **Color** : couleur du texte

## Dessiner sur le Canvas

En plus du texte, vous pouvez dessiner des lignes, des rectangles et d'autres formes :

### Lignes

```pascal
// Tracer une ligne horizontale
Printer.Canvas.MoveTo(100, 500);
Printer.Canvas.LineTo(500, 500);
```

### Rectangles

```pascal
// Dessiner un rectangle
Printer.Canvas.Rectangle(100, 600, 400, 800);
```

### Rectangles remplis

```pascal
// Rectangle avec fond coloré
Printer.Canvas.Brush.Color := clYellow;
Printer.Canvas.Brush.Style := bsSolid;
Printer.Canvas.Rectangle(100, 900, 400, 1100);
```

## Gestion des pages multiples

Pour les documents de plusieurs pages, utilisez la méthode `NewPage` :

```pascal
procedure TForm1.ImprimerPlusieursPages;
var
  i: Integer;
begin
  Printer.BeginDoc;
  try
    for i := 1 to 3 do
    begin
      Printer.Canvas.Font.Size := 14;
      Printer.Canvas.TextOut(100, 100, 'Page ' + IntToStr(i));

      if i < 3 then  // Ne pas créer de nouvelle page après la dernière
        Printer.NewPage;
    end;
  finally
    Printer.EndDoc;
  end;
end;
```

## Calculer les dimensions

Les coordonnées du Canvas sont en pixels, mais il est utile de connaître les dimensions réelles de la page :

```pascal
procedure TForm1.AfficherDimensionsPage;
var
  LargeurMM, HauteurMM: Double;
begin
  // Conversion pixels vers millimètres (approximation)
  LargeurMM := (Printer.PageWidth / Screen.PixelsPerInch) * 25.4;
  HauteurMM := (Printer.PageHeight / Screen.PixelsPerInch) * 25.4;

  ShowMessage(Format('Largeur : %.1f mm, Hauteur : %.1f mm', [LargeurMM, HauteurMM]));
end;
```

## Gestion de l'orientation

Vous pouvez définir l'orientation de la page (portrait ou paysage) :

```pascal
Printer.Orientation := poPortrait;   // Portrait (vertical)
// ou
Printer.Orientation := poLandscape;  // Paysage (horizontal)
```

**Important :** L'orientation doit être définie **avant** l'appel à `BeginDoc`.

## Imprimer le contenu d'un Memo

Voici un exemple pratique pour imprimer le contenu d'un composant TMemo :

```pascal
procedure TForm1.ImprimerMemo;
var
  i, Y: Integer;
  HauteurLigne: Integer;
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      Printer.Canvas.Font.Name := 'Courier New';
      Printer.Canvas.Font.Size := 10;

      HauteurLigne := Printer.Canvas.TextHeight('A') + 10;
      Y := 100;

      for i := 0 to Memo1.Lines.Count - 1 do
      begin
        // Vérifier si on doit passer à une nouvelle page
        if Y > Printer.PageHeight - 200 then
        begin
          Printer.NewPage;
          Y := 100;
        end;

        Printer.Canvas.TextOut(100, Y, Memo1.Lines[i]);
        Y := Y + HauteurLigne;
      end;
    finally
      Printer.EndDoc;
    end;
  end;
end;
```

## Gestion des erreurs d'impression

Il est important de gérer les erreurs qui peuvent survenir lors de l'impression :

```pascal
procedure TForm1.ImprimerAvecGestionErreurs;
begin
  if PrintDialog1.Execute then
  begin
    try
      Printer.BeginDoc;
      try
        Printer.Canvas.TextOut(100, 100, 'Test d''impression');
      finally
        Printer.EndDoc;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Erreur lors de l''impression : ' + E.Message);
        Printer.Abort;  // Annule l'impression en cas d'erreur
      end;
    end;
  end;
end;
```

## Annulation d'une impression

Si vous devez annuler une impression en cours :

```pascal
Printer.Abort;
```

Cette méthode peut être appelée entre `BeginDoc` et `EndDoc` pour interrompre l'impression.

## Liste des imprimantes disponibles

Vous pouvez afficher la liste de toutes les imprimantes disponibles :

```pascal
procedure TForm1.AfficherImprimantes;
var
  i: Integer;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Imprimantes disponibles :');
  Memo1.Lines.Add('');

  for i := 0 to Printer.Printers.Count - 1 do
  begin
    Memo1.Lines.Add(IntToStr(i) + ' : ' + Printer.Printers[i]);

    if i = Printer.PrinterIndex then
      Memo1.Lines.Add('  (imprimante par défaut)');
  end;
end;
```

## Sélectionner une imprimante par code

Vous pouvez changer d'imprimante par programmation :

```pascal
// Sélectionner l'imprimante à l'index 1
if Printer.Printers.Count > 1 then
  Printer.PrinterIndex := 1;
```

## Conseils et bonnes pratiques

### Toujours utiliser try...finally

Le bloc `try...finally` garantit que `EndDoc` sera toujours appelé, même en cas d'erreur :

```pascal
Printer.BeginDoc;
try
  // Votre code d'impression ici
finally
  Printer.EndDoc;
end;
```

### Tester avant d'imprimer

Vérifiez qu'une imprimante est disponible avant de tenter d'imprimer :

```pascal
if Printer.Printers.Count = 0 then
begin
  ShowMessage('Aucune imprimante installée');
  Exit;
end;
```

### Prévisualiser plutôt que d'imprimer directement

Lors du développement, il peut être utile de dessiner d'abord sur un formulaire pour visualiser le résultat avant d'imprimer :

```pascal
// Remplacer Printer.Canvas par Form1.Canvas pour tester
Form1.Canvas.TextOut(100, 100, 'Test de mise en page');
```

### Utiliser des constantes pour les marges

Pour faciliter la maintenance, définissez des constantes pour les marges :

```pascal
const
  MARGE_GAUCHE = 100;
  MARGE_HAUT = 100;
  MARGE_DROITE = 100;
  MARGE_BAS = 100;
```

## Exemple complet : impression d'une facture simple

Voici un exemple pratique qui combine plusieurs concepts :

```pascal
procedure TForm1.ImprimerFactureSimple;
var
  Y: Integer;
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      Y := 100;

      // En-tête
      Printer.Canvas.Font.Size := 16;
      Printer.Canvas.Font.Style := [fsBold];
      Printer.Canvas.TextOut(100, Y, 'FACTURE');
      Y := Y + 150;

      // Ligne de séparation
      Printer.Canvas.Pen.Width := 2;
      Printer.Canvas.MoveTo(100, Y);
      Printer.Canvas.LineTo(Printer.PageWidth - 100, Y);
      Y := Y + 100;

      // Informations client
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.TextOut(100, Y, 'Client : ' + Edit1.Text);
      Y := Y + 80;
      Printer.Canvas.TextOut(100, Y, 'Date : ' + DateToStr(Date));
      Y := Y + 150;

      // Articles
      Printer.Canvas.Font.Style := [fsBold];
      Printer.Canvas.TextOut(100, Y, 'Désignation');
      Printer.Canvas.TextOut(400, Y, 'Prix');
      Y := Y + 80;

      Printer.Canvas.Font.Style := [];
      Printer.Canvas.TextOut(100, Y, 'Article 1');
      Printer.Canvas.TextOut(400, Y, '50,00 €');
      Y := Y + 80;

      Printer.Canvas.TextOut(100, Y, 'Article 2');
      Printer.Canvas.TextOut(400, Y, '30,00 €');
      Y := Y + 150;

      // Total
      Printer.Canvas.Font.Style := [fsBold];
      Printer.Canvas.TextOut(100, Y, 'TOTAL :');
      Printer.Canvas.TextOut(400, Y, '80,00 €');

    finally
      Printer.EndDoc;
    end;
  end;
end;
```

## Limitations des composants natifs

Bien que pratiques, les composants d'impression natifs ont certaines limitations :

- **Pas d'aperçu avant impression intégré** : vous devrez créer votre propre système de prévisualisation
- **Mise en page manuelle** : vous devez calculer vous-même les positions et gérer les sauts de page
- **Pas de modèles** : chaque impression doit être codée manuellement
- **Complexité pour les documents élaborés** : pour des rapports complexes, il est préférable d'utiliser un générateur de rapports (voir sections suivantes)

## Quand utiliser les composants natifs ?

Les composants d'impression natifs sont parfaits pour :

- Des impressions simples (reçus, tickets, étiquettes)
- Des listes ou tableaux basiques
- Des documents avec peu de mise en forme
- Des applications où vous voulez éviter les dépendances externes

Pour des rapports plus complexes, il sera préférable d'utiliser des bibliothèques spécialisées comme FastReport ou QuickReport (voir sections 9.3 et 9.4).

## Résumé

Les composants d'impression natifs de Delphi offrent une solution simple et efficace pour imprimer du contenu depuis vos applications. Les points clés à retenir :

- **TPrinter** est l'objet central pour gérer l'impression
- **TPrintDialog** et **TPrinterSetupDialog** permettent à l'utilisateur de configurer l'impression
- Le processus d'impression suit toujours la structure : BeginDoc → Dessin sur Canvas → EndDoc
- Le Canvas de l'imprimante fonctionne comme celui d'un formulaire
- Toujours utiliser `try...finally` pour garantir l'appel à `EndDoc`
- Pour des documents complexes, envisagez d'utiliser un générateur de rapports dédié

Dans les prochaines sections, nous découvrirons l'aperçu avant impression et les générateurs de rapports qui offrent des fonctionnalités plus avancées.

⏭️ [Aperçu avant impression](/09-rapports-et-impressions/02-apercu-avant-impression.md)
