# 2.7 Structure d'un projet Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Comprendre la structure d'un projet Delphi est essentiel pour développer efficacement des applications. Dans cette section, nous allons examiner en détail les différents fichiers et dossiers qui composent un projet Delphi, ainsi que leur rôle dans l'architecture globale de votre application.

## Vue d'ensemble d'un projet Delphi

Un projet Delphi est constitué de plusieurs fichiers de différents types, chacun ayant un rôle spécifique. Ensemble, ces fichiers forment une structure cohérente qui permet à Delphi de générer une application exécutable.

![Structure d'un projet Delphi](https://placeholder.com/delphi-project-structure)

## Les fichiers principaux d'un projet

Lorsque vous créez un nouveau projet Delphi, plusieurs fichiers sont automatiquement générés. Voici les plus importants :

### Le fichier projet (.dproj)

**Nom typique** : `MonProjet.dproj`

Le fichier `.dproj` est un fichier XML qui contient les paramètres et la configuration de votre projet. Il est géré par l'IDE et vous n'avez généralement pas besoin de le modifier directement.

Ce fichier contient :
- Les options de compilation
- Les chemins de recherche
- La liste des unités incluses dans le projet
- Les plateformes cibles (Windows 32/64 bits, macOS, Android, iOS, etc.)
- Les dépendances et les packages utilisés

> **Note pour les débutants :** Ne modifiez pas ce fichier manuellement, utilisez plutôt l'option "Projet > Options" dans l'IDE.

### Le fichier projet principal (.dpr)

**Nom typique** : `MonProjet.dpr`

Le fichier `.dpr` est le point d'entrée de votre application. C'est un fichier Pascal qui contient le code minimal pour démarrer votre application. Vous pouvez l'ouvrir en double-cliquant sur "Voir source du projet" dans l'Explorateur de projets.

Voici à quoi ressemble généralement un fichier `.dpr` pour une application VCL :

```pascal
program MonProjet;

uses
  Vcl.Forms,
  UnitPrincipale in 'UnitPrincipale.pas' {FormPrincipale},
  UnitSecondaire in 'UnitSecondaire.pas' {FormSecondaire};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.CreateForm(TFormSecondaire, FormSecondaire);
  Application.Run;
end.
```

Ce code :
1. Définit le nom du programme
2. Liste toutes les unités utilisées par le projet
3. Inclut le fichier de ressources (avec la directive `{$R *.res}`)
4. Initialise l'application
5. Crée les formulaires
6. Lance la boucle principale de l'application

### Les unités (.pas)

**Nom typique** : `UnitPrincipale.pas`

Les unités sont les modules de base de votre code source Pascal. Chaque unité est un fichier `.pas` qui contient généralement :

- Des déclarations d'imports (`uses`)
- Des déclarations de types (classes, records, etc.)
- Des variables globales
- Des procédures et fonctions
- Du code d'implémentation

Une unité typique contenant un formulaire ressemble à ceci :

```pascal
unit UnitPrincipale;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TFormPrincipale = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormPrincipale: TFormPrincipale;

implementation

{$R *.dfm}

procedure TFormPrincipale.Button1Click(Sender: TObject);
begin
  ShowMessage('Bonjour ' + Edit1.Text);
end;

end.
```

La structure d'une unité comporte deux sections principales :
- **interface** : Contient les déclarations publiques accessibles aux autres unités
- **implementation** : Contient le code d'implémentation des éléments déclarés dans l'interface

> **Pour les débutants :** Pensez à l'interface comme à la "promesse" de ce que l'unité fournit, et à l'implémentation comme à la façon dont cette promesse est tenue.

### Les fichiers de formulaire (.dfm)

**Nom typique** : `UnitPrincipale.dfm`

Pour chaque formulaire dans votre application, Delphi crée un fichier `.dfm` associé à l'unité correspondante. Ce fichier stocke la définition visuelle du formulaire : positions et propriétés des composants, taille du formulaire, etc.

Le fichier `.dfm` peut être visualisé de deux façons :
1. **Vue conception** : L'éditeur visuel que vous utilisez pour placer les composants
2. **Vue texte** : Une représentation texte que vous pouvez éditer directement (Clic droit sur le formulaire > "Voir comme texte")

Voici un exemple simple de fichier `.dfm` en vue texte :

```
object FormPrincipale: TFormPrincipale
  Left = 0
  Top = 0
  Caption = 'Mon Application'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Button1: TButton
    Left = 272
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Cliquez-moi'
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 240
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
end
```

> **Astuce :** Bien que vous puissiez modifier directement le fichier `.dfm` en mode texte, il est généralement préférable d'utiliser l'éditeur visuel pour éviter les erreurs de syntaxe.

### Le fichier de ressources (.res)

**Nom typique** : `MonProjet.res`

Ce fichier binaire contient les ressources de l'application, comme les icônes, les curseurs, les images, et les chaînes de version. Il est généralement géré automatiquement par Delphi.

## Fichiers secondaires et dossiers

En plus des fichiers principaux, un projet Delphi peut comporter d'autres fichiers et dossiers :

### Fichiers de configuration

- **`.dsk`** : Stocke l'état du bureau (dispositions des fenêtres, onglets ouverts)
- **`.identcache`** : Cache pour l'auto-complétion et la navigation
- **`.local`** : Paramètres locaux spécifiques à l'ordinateur

### Dossiers générés

Delphi crée plusieurs dossiers pour organiser les fichiers générés :

- **`__history`** : Contient des sauvegardes automatiques de vos fichiers source
- **`Win32/Debug`** : Contient les fichiers compilés en mode Debug pour Windows 32 bits
- **`Win32/Release`** : Contient les fichiers compilés en mode Release pour Windows 32 bits
- **`Win64/Debug`** : Contient les fichiers compilés en mode Debug pour Windows 64 bits
- **`Win64/Release`** : Contient les fichiers compilés en mode Release pour Windows 64 bits

> **Note :** Dans Delphi 12 et 11, d'autres dossiers existent pour les plateformes supplémentaires (macOS, Android, iOS, Linux).

### Fichiers de compilation intermédiaires

Pendant la compilation, Delphi génère plusieurs types de fichiers intermédiaires :

- **`.dcu`** (Delphi Compiled Unit) : Unités compilées
- **`.dcpil`** : Fichiers intermédiaires pour la compilation
- **`.map`** : Fichiers de mappage pour le débogage
- **`.~pas`**, **`.~dfm`** : Fichiers de sauvegarde

Ces fichiers sont généralement placés dans les dossiers Debug ou Release et n'ont pas besoin d'être inclus dans le contrôle de version.

## Organisation des fichiers dans un projet réel

Pour les projets plus complexes, une bonne organisation des fichiers est essentielle. Voici une structure recommandée :

```
MonProjet/
│
├── src/                    # Code source
│   ├── forms/              # Formulaires et fiches
│   ├── modules/            # Modules fonctionnels
│   ├── utils/              # Utilitaires et fonctions communes
│   └── data/               # Modules d'accès aux données
│
├── resources/              # Ressources externes
│   ├── images/             # Images et icônes
│   ├── strings/            # Fichiers de chaînes localisées
│   └── data/               # Fichiers de données
│
├── docs/                   # Documentation
│
├── libs/                   # Bibliothèques tierces
│
└── output/                 # Dossier de sortie pour les builds
    ├── debug/
    └── release/
```

> **Bonne pratique :** Dans Delphi, vous pouvez créer des dossiers virtuels dans l'Explorateur de projets pour organiser vos fichiers logiquement, même si leur emplacement physique est différent.

## Les différentes parties d'un projet

### Unités visuelles vs non visuelles

Dans un projet Delphi, on distingue généralement deux types d'unités :

1. **Unités visuelles** : Contiennent des formulaires ou des cadres (frames)
   - Ont un fichier `.dfm` associé
   - Définissent l'interface utilisateur

2. **Unités non visuelles** : Ne contiennent que du code
   - Pas de fichier `.dfm` associé
   - Peuvent contenir des classes, des fonctions, des constantes
   - Souvent utilisées pour la logique métier, les utilitaires, l'accès aux données

### Architectures courantes

Il existe plusieurs façons d'organiser le code dans un projet Delphi :

1. **Architecture simple** :
   - Toute la logique est dans les formulaires
   - Adaptée aux petits projets ou prototypes

2. **Architecture en couches** :
   - **Couche UI** : Formulaires et composants visuels
   - **Couche métier** : Classes et logique métier
   - **Couche données** : Accès aux données (base de données, fichiers)

3. **Architecture MVC (Modèle-Vue-Contrôleur)** :
   - **Modèle** : Classes de données et logique métier
   - **Vue** : Formulaires et composants visuels
   - **Contrôleur** : Gestion des interactions entre le modèle et la vue

> **Pour les débutants :** Commencez par une architecture simple, puis évoluez vers une architecture en couches à mesure que votre projet grandit.

## Comment Delphi gère les dépendances

### La clause "uses"

En Object Pascal, les dépendances entre unités sont gérées par la clause `uses`. Cette clause liste toutes les unités dont l'unité actuelle dépend.

```pascal
unit MonUnite;

interface

uses
  System.SysUtils, // Unité standard de Delphi
  System.Classes,  // Autre unité standard
  MonAutreUnite;   // Unité personnalisée du projet

// ... reste du code
```

La clause `uses` peut apparaître dans deux sections :
- Dans la section `interface` : Les unités sont disponibles partout dans l'unité actuelle
- Dans la section `implementation` : Les unités sont disponibles uniquement dans l'implémentation

> **Bonne pratique :** Pour minimiser les dépendances, placez les unités dans la section `implementation` lorsque possible.

### Ordre de compilation

Delphi compile les unités dans l'ordre suivant :
1. Les unités listées dans le fichier `.dpr`
2. Les unités dont dépendent ces unités, de manière récursive

Cet ordre est généralement automatique, mais il peut être important pour certains projets complexes.

## Organisation des ressources

### Ressources liées au projet

Les ressources liées au projet (icônes, curseurs, etc.) sont généralement stockées dans :
- Un fichier de ressources (`.res`) pour les ressources globales
- Des fichiers image individuels pour les ressources utilisées dans l'interface

### Utilisation de paquets (packages)

Les paquets sont des bibliothèques de composants qui peuvent être partagées entre plusieurs projets :
- **Paquets d'exécution** (runtime) : Utilisés lors de l'exécution
- **Paquets de conception** (design-time) : Utilisés dans l'IDE

Delphi 12 et 11 utilisent le gestionnaire de paquets GetIt pour installer et gérer les paquets tiers.

## Les différents types de projets Delphi

Delphi permet de créer plusieurs types de projets :

1. **Application VCL** : Application Windows native
2. **Application FireMonkey** : Application multi-plateforme
3. **Service Windows** : Application qui s'exécute en arrière-plan
4. **Bibliothèque dynamique (DLL)** : Bibliothèque de fonctions partagée
5. **Paquet** : Bibliothèque de composants
6. **Console** : Application en mode texte

Chaque type de projet a une structure légèrement différente, mais les concepts de base restent les mêmes.

## Exercice pratique

Pour mieux comprendre la structure d'un projet Delphi, essayez cet exercice :

1. Créez un nouveau projet d'application VCL
2. Ajoutez une seconde unité avec un formulaire (Fichier > Nouveau > Form)
3. Ajoutez une unité non visuelle (Fichier > Nouveau > Unit)
4. Examinez les fichiers créés dans l'Explorateur de projets
5. Ouvrez le fichier `.dpr` et observez comment les unités sont listées
6. Regardez comment les unités sont liées entre elles par la clause `uses`

## Meilleures pratiques

Pour maintenir une structure de projet saine :

1. **Nommez clairement vos unités** : Utilisez un préfixe pour indiquer leur type (U pour unité, F pour frame, etc.)
2. **Organisez logiquement vos fichiers** : Utilisez des dossiers ou des dossiers virtuels
3. **Minimisez les dépendances** : Ne mettez dans la clause `uses` que ce dont vous avez besoin
4. **Séparez l'interface de l'implémentation** : Ne mettez pas toute la logique dans les formulaires
5. **Commentez votre structure** : Documentez le rôle de chaque unité
6. **Maintenez la cohérence** : Suivez les mêmes conventions dans tout le projet

## Conclusion

Comprendre la structure d'un projet Delphi est fondamental pour développer des applications bien organisées et maintenables. Les fichiers `.dpr`, `.pas` et `.dfm` constituent l'épine dorsale de votre application, tandis qu'une organisation claire des unités et des ressources contribuera à la facilité de développement et de maintenance.

Dans la prochaine section, nous explorerons le cycle de développement avec Delphi, de la conception initiale au déploiement final.

⏭️ [Introduction au cycle de développement avec Delphi](/02-decouverte-de-lide-delphi/08-introduction-au-cycle-de-developpement.md)
