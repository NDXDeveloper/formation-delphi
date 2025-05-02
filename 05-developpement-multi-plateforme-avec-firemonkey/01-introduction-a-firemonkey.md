# 5.1 Introduction à FireMonkey (FMX)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

FireMonkey (souvent abrégé en FMX) est la bibliothèque d'interface utilisateur multi-plateforme de Delphi. Contrairement à la VCL (Visual Component Library) qui est spécifique à Windows, FireMonkey permet de développer des applications qui fonctionnent sur plusieurs systèmes d'exploitation à partir d'une base de code unique.

## Qu'est-ce que FireMonkey ?

FireMonkey est une plateforme de développement d'applications modernes qui offre plusieurs avantages clés :

- **Véritable développement multi-plateforme** : Créez une seule application qui s'exécutera sur Windows, macOS, iOS, Android et Linux (support Linux disponible depuis Delphi 11).
- **Interface utilisateur riche et moderne** : Contrairement à la VCL qui utilise les contrôles natifs Windows, FireMonkey dessine ses propres contrôles, ce qui lui permet d'avoir un aspect cohérent sur toutes les plateformes.
- **Moteur graphique avancé** : FireMonkey utilise un moteur de rendu graphique qui prend en charge les effets visuels 2D et 3D, les animations, et les transformations.
- **Approche orientée styles** : Les applications FireMonkey utilisent des styles pour contrôler l'apparence, permettant de modifier facilement l'aspect visuel sans changer le code.

## Comparaison avec la VCL

| Caractéristique | FireMonkey (FMX) | VCL |
|-----------------|------------------|-----|
| Plateformes supportées | Windows, macOS, iOS, Android, Linux | Windows uniquement |
| Contrôles | Dessinés par FireMonkey | Contrôles natifs Windows |
| Performances | Bonnes, mais peuvent nécessiter plus d'optimisation sur mobile | Excellentes sur Windows |
| Aspect visuel | Cohérent sur toutes les plateformes | Look and feel Windows |
| Support 3D | Oui | Non (sauf via des composants tiers) |
| Maturité | Plus récent (introduit dans Delphi XE2) | Mature (depuis les débuts de Delphi) |

## Quand utiliser FireMonkey ?

FireMonkey est le choix idéal dans les cas suivants :

- Vous devez développer une application qui fonctionnera sur plusieurs plateformes
- Vous souhaitez créer des applications mobiles pour iOS et/ou Android
- Vous avez besoin d'interfaces utilisateur modernes avec des animations et des effets visuels
- Vous voulez un aspect visuel cohérent sur toutes les plateformes

La VCL reste préférable si :
- Vous développez exclusivement pour Windows
- Vous avez besoin des meilleures performances possibles sur Windows
- Vous devez utiliser des contrôles Windows natifs spécifiques
- Vous maintenez une application VCL existante

## Structure d'un projet FireMonkey

Un projet FireMonkey de base comprend :

1. **Un fichier .dpr** : Le point d'entrée du programme
2. **Des fichiers .fmx** : Les fichiers de formulaire FireMonkey (équivalents aux .dfm de la VCL)
3. **Des fichiers .pas** : Les unités Pascal contenant le code de l'application

L'extension `.fmx` est la principale différence visible avec un projet VCL (qui utilise `.dfm`).

## Premier exemple : Hello World avec FireMonkey

Voici comment créer une application "Hello World" simple avec FireMonkey :

1. Dans Delphi, sélectionnez **Fichier > Nouveau > Application multi-périphériques**
2. Delphi crée un nouveau projet FireMonkey avec un formulaire vide
3. Dans l'Inspecteur d'objets, définissez la propriété `Caption` du formulaire à "Hello FireMonkey"
4. Depuis la palette d'outils, faites glisser un composant `TLabel` sur le formulaire
5. Positionnez-le au centre et définissez sa propriété `Text` à "Hello World!"
6. Pour personnaliser l'apparence, vous pouvez modifier les propriétés suivantes du label :
   - `TextSettings.Font.Size` : définissez-la à 24 pour agrandir le texte
   - `TextSettings.Font.Style` : ajoutez `[fsBold]` pour mettre en gras
   - `TextSettings.FontColor` : choisissez une couleur pour le texte

```pascal
// Le code généré ressemblera à ceci :
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
```

## Notez les différences avec la VCL

Si vous êtes habitué à la VCL, vous remarquerez quelques différences importantes :

1. Les unités importées commencent par `FMX.` au lieu de `Vcl.`
2. Certaines propriétés sont organisées différemment, comme les propriétés de police qui se trouvent dans `TextSettings`
3. Le positionnement des contrôles utilise par défaut un système de coordonnées indépendant de la résolution

## Styles dans FireMonkey

Une des caractéristiques les plus puissantes de FireMonkey est son système de styles. Par défaut, l'IDE propose plusieurs styles prédéfinis :

- Windows
- macOS
- iOS
- Android
- Et d'autres styles personnalisés

Pour changer le style d'une application FireMonkey :

1. Sélectionnez le formulaire principal
2. Dans l'Inspecteur d'objets, recherchez la propriété `StyleBook`
3. Créez un nouveau TStyleBook en cliquant sur le bouton [...]
4. Dans l'éditeur de StyleBook, sélectionnez un style dans la liste déroulante

## Déploiement multi-plateforme

L'avantage principal de FireMonkey est la possibilité de cibler plusieurs plateformes :

1. **Windows** : Compilation directe sans configuration supplémentaire
2. **macOS** : Nécessite un Mac pour la compilation finale (mais vous pouvez développer sous Windows)
3. **iOS** : Nécessite un Mac avec Xcode et un certificat de développeur Apple
4. **Android** : Nécessite le SDK Android et Java JDK
5. **Linux** : Support disponible depuis Delphi 11 Alexandria

Pour changer la plateforme cible :
1. Utilisez le sélecteur de plateforme dans la barre d'outils de l'IDE
2. Ou accédez à **Projet > Options de projet > Plateformes cibles**

## Conclusion

FireMonkey représente l'avenir du développement multi-plateforme avec Delphi. Bien que la courbe d'apprentissage puisse être un peu plus raide que la VCL si vous êtes habitué à cette dernière, les avantages de FireMonkey en termes de portabilité et de fonctionnalités modernes en font un choix excellent pour les nouvelles applications.

Dans les sections suivantes, nous explorerons plus en détail les composants spécifiques de FireMonkey, comment adapter vos interfaces aux différentes plateformes, et comment tirer parti des fonctionnalités spécifiques à chaque système d'exploitation.

## Ressources supplémentaires

- Documentation officielle de FireMonkey
- Exemples d'applications FireMonkey inclus dans Delphi
- Vidéos tutorielles sur le site d'Embarcadero

⏭️ [Différences entre VCL et FMX](/05-developpement-multi-plateforme-avec-firemonkey/02-differences-entre-vcl-et-fmx.md)
