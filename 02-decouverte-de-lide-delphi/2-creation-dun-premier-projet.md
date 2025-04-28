# 2.2 Création d'un premier projet

Maintenant que nous sommes familiarisés avec l'interface de Delphi, passons à l'étape suivante : la création de notre premier projet. Nous allons réaliser une application simple mais fonctionnelle qui vous permettra de comprendre les bases du développement avec Delphi.

## Lancement d'un nouveau projet

Pour créer un nouveau projet dans Delphi, suivez ces étapes :

1. Lancez Delphi si ce n'est pas déjà fait
2. Dans l'écran d'accueil, cliquez sur "Nouveau" ou allez dans le menu **Fichier > Nouveau > Application VCL**
3. Si l'écran d'accueil n'apparaît pas, utilisez le raccourci **Fichier > Nouveau > Application VCL** depuis la barre de menus

> **Note :** VCL signifie "Visual Component Library", c'est la bibliothèque de composants visuels pour Windows. Pour une application multi-plateforme, vous choisiriez plutôt "Application FireMonkey" (FMX), mais nous commencerons par VCL qui est parfaite pour les débutants.

## Comprendre ce qui a été créé

Après avoir créé un nouveau projet, voici ce que vous observez :

- Un **formulaire vide** (nommé "Form1" par défaut) s'affiche dans le concepteur
- Delphi a automatiquement généré deux fichiers principaux :
  - `Unit1.pas` : fichier contenant le code Object Pascal de votre formulaire
  - `Unit1.dfm` : fichier décrivant l'interface visuelle (Design Form Module)
- Dans le gestionnaire de projet, vous verrez également le fichier projet (`.dproj`) qui coordonne tous les éléments

![Premier formulaire Delphi](https://placeholder.com/delphi-first-form)

## Enregistrement du projet

Il est important d'enregistrer votre projet dès le début :

1. Allez dans le menu **Fichier > Enregistrer tout** ou utilisez le raccourci **Ctrl+Shift+S**
2. Delphi vous demandera d'enregistrer trois éléments :
   - Le fichier projet (`.dproj`) - Donnez-lui un nom significatif comme "MonPremierProjet"
   - Le fichier unité (`.pas`) - Vous pouvez le nommer "UPrincipal" (la convention est de préfixer les noms d'unités par "U")
   - Le fichier formulaire (`.dfm`) sera enregistré automatiquement avec le même nom que le fichier unité

> **Bonne pratique :** Créez un dossier dédié pour chaque projet Delphi. Cela vous aidera à organiser votre travail et facilitera la sauvegarde ou le partage ultérieur.

## Conception de l'interface utilisateur

Maintenant, créons une interface simple avec quelques composants :

1. **Ajoutons un libellé (TLabel)** :
   - Localisez l'onglet "Standard" dans la palette d'outils
   - Cliquez sur le composant "Label"
   - Cliquez ensuite sur le formulaire pour placer le composant
   - Dans l'inspecteur d'objets, modifiez la propriété "Caption" en "Entrez votre nom :"

2. **Ajoutons un champ de saisie (TEdit)** :
   - Dans la palette d'outils, cliquez sur le composant "Edit"
   - Placez-le sur le formulaire à droite du label
   - Dans l'inspecteur d'objets, modifiez la propriété "Name" en "EditNom" (la convention est de préfixer le nom des composants par leur type)
   - Effacez le contenu de la propriété "Text" pour que le champ soit vide au démarrage

3. **Ajoutons un bouton (TButton)** :
   - Dans la palette d'outils, cliquez sur le composant "Button"
   - Placez-le sous le champ de saisie
   - Modifiez sa propriété "Caption" en "Cliquez-moi"
   - Modifiez sa propriété "Name" en "ButtonClickMe"

4. **Améliorons l'apparence du formulaire** :
   - Sélectionnez le formulaire en cliquant sur sa surface (loin des autres composants)
   - Modifiez sa propriété "Caption" en "Mon Premier Programme"
   - Ajustez sa propriété "Width" à environ 350 (largeur)
   - Ajustez sa propriété "Height" à environ 200 (hauteur)

![Interface utilisateur simple](https://placeholder.com/delphi-simple-ui)

## Ajout de comportement avec du code

Maintenant, ajoutons du code pour rendre notre application interactive :

1. **Double-cliquez sur le bouton** "Cliquez-moi" dans le formulaire
   - Cette action crée automatiquement un gestionnaire d'événement pour l'événement "OnClick" du bouton
   - Vous êtes maintenant dans l'éditeur de code, où Delphi a généré une procédure nommée `ButtonClickMeClick`

2. **Ajoutez le code suivant** entre les mots `begin` et `end` :

```pascal
if EditNom.Text = '' then
  ShowMessage('Veuillez entrer votre nom!')
else
  ShowMessage('Bonjour ' + EditNom.Text + ' ! Bienvenue dans le monde de Delphi!');
```

Votre code complet devrait ressembler à ceci :

```pascal
procedure TForm1.ButtonClickMeClick(Sender: TObject);
begin
  if EditNom.Text = '' then
    ShowMessage('Veuillez entrer votre nom!')
  else
    ShowMessage('Bonjour ' + EditNom.Text + ' ! Bienvenue dans le monde de Delphi!');
end;
```

Ce code vérifie si l'utilisateur a saisi un nom. Si le champ est vide, il affiche un message d'avertissement. Sinon, il affiche un message personnalisé avec le nom saisi.

## Exécution de votre premier programme

Pour exécuter votre application :

1. Appuyez sur la touche **F9** ou cliquez sur le bouton "Run" dans la barre d'outils (l'icône de triangle vert)
2. Delphi va compiler votre projet et lancer l'application
3. Votre formulaire apparaîtra comme une véritable application Windows
4. Testez votre application :
   - Essayez de cliquer sur le bouton sans rien saisir
   - Puis entrez votre nom et cliquez à nouveau sur le bouton

![Application en exécution](https://placeholder.com/delphi-running-app)

## Comprendre ce qui se passe

Félicitations, vous venez de créer votre première application Delphi ! Voici ce qui s'est passé :

1. Vous avez créé une interface utilisateur en plaçant des composants visuels
2. Vous avez défini leurs propriétés pour personnaliser leur apparence
3. Vous avez écrit du code pour répondre à un événement (le clic sur un bouton)
4. Delphi a compilé votre code en un exécutable Windows natif
5. Votre application s'est exécutée de manière autonome

## Exploration du code généré

Examinons brièvement le code que Delphi a généré automatiquement. Dans l'éditeur de code, vous verrez une structure comme celle-ci :

```pascal
unit UPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    EditNom: TEdit;
    ButtonClickMe: TButton;
    procedure ButtonClickMeClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonClickMeClick(Sender: TObject);
begin
  if EditNom.Text = '' then
    ShowMessage('Veuillez entrer votre nom!')
  else
    ShowMessage('Bonjour ' + EditNom.Text + ' ! Bienvenue dans le monde de Delphi!');
end;

end.
```

Ne vous inquiétez pas si tout cela semble complexe pour l'instant. Vous apprendrez progressivement la signification de chaque partie.

## Personnalisation avancée (optionnelle)

Si vous souhaitez aller plus loin, voici quelques améliorations que vous pouvez apporter :

1. **Ajouter un raccourci clavier** :
   - Sélectionnez le bouton "Cliquez-moi"
   - Dans l'inspecteur d'objets, cherchez la propriété "Default" et changez-la à "True"
   - Maintenant, appuyer sur la touche Entrée équivaudra à cliquer sur le bouton

2. **Rendre l'interface plus attrayante** :
   - Ajoutez un composant TPanel depuis la palette pour regrouper vos contrôles
   - Explorez les propriétés Font pour modifier l'apparence du texte
   - Utilisez les propriétés Color pour ajouter des couleurs

## Conclusion

Dans cette section, vous avez appris à :
- Créer un nouveau projet Delphi
- Concevoir une interface utilisateur simple
- Ajouter du comportement avec du code Object Pascal
- Exécuter et tester votre application

Ce premier projet, bien que simple, illustre le concept fondamental du développement avec Delphi : la création rapide d'applications Windows natives avec une interface visuelle et un comportement programmé.

Dans la section suivante, nous explorerons plus en détail la Palette d'outils et l'Inspecteur d'objets, deux outils essentiels pour accélérer votre travail de développement.
