🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.7 Structure d'un projet Delphi

## Introduction

Vous avez créé plusieurs projets, ajouté des composants, écrit du code... Mais vous êtes-vous déjà demandé comment tout cela s'organise en coulisse ? Comment Delphi transforme vos fiches visuelles et votre code en une application exécutable ?

Comprendre la structure d'un projet Delphi est essentiel pour bien organiser votre travail, résoudre les problèmes, et travailler efficacement sur des projets plus complexes. Dans cette section, nous allons explorer en détail tous les fichiers qui composent un projet, leur rôle, et comment ils interagissent.

Ne vous inquiétez pas si cela semble technique au début : nous allons prendre le temps de tout expliquer clairement, avec des exemples concrets.

## Vue d'ensemble d'un projet

### Qu'est-ce qu'un projet exactement ?

Un projet Delphi n'est pas un simple fichier, mais un **ensemble de fichiers** qui travaillent ensemble pour créer votre application. Imaginez un projet comme une équipe où chaque fichier a un rôle spécifique :

- Le **fichier projet** (.dpr) est le chef d'équipe qui coordonne tout
- Les **unités de code** (.pas) sont les ouvriers qui font le travail
- Les **fichiers de fiches** (.dfm) sont les designers qui définissent l'apparence
- Les **fichiers de ressources** (.res) fournissent les matériaux (icônes, images)
- Les **fichiers de configuration** (.dproj) gardent les paramètres et les réglages

Tous ces fichiers doivent être présents et correctement organisés pour que votre projet fonctionne.

### Hiérarchie d'un projet simple

Prenons l'exemple d'un projet simple appelé "MonApplication". Voici sa structure de fichiers typique :

```
MonApplication/
├── MonApplication.dpr          (Fichier projet principal)
├── MonApplication.dproj        (Configuration du projet)
├── MonApplication.res          (Ressources du projet)
├── MonApplication.identcache   (Cache d'identifiants)
├── FormPrincipale.pas          (Code de la fiche principale)
├── FormPrincipale.dfm          (Définition visuelle de la fiche)
├── FormSecondaire.pas          (Code d'une autre fiche)
├── FormSecondaire.dfm          (Définition visuelle de l'autre fiche)
├── UnitUtils.pas               (Unité de code sans interface)
├── Win32/                      (Dossier de sortie Windows 32 bits)
│   ├── Debug/
│   │   ├── MonApplication.exe  (Exécutable de débogage)
│   │   ├── FormPrincipale.dcu  (Unité compilée)
│   │   ├── FormSecondaire.dcu
│   │   └── UnitUtils.dcu
│   └── Release/
│       └── MonApplication.exe  (Exécutable de production)
├── Win64/                      (Dossier de sortie Windows 64 bits)
├── __history/                  (Historique des modifications)
└── __recovery/                 (Fichiers de récupération)
```

Ne soyez pas intimidé par tous ces fichiers ! Nous allons les explorer un par un.

## Les fichiers essentiels

### Le fichier projet (.dpr)

Le fichier .dpr (Delphi Project) est le point d'entrée de votre application. C'est le premier fichier que le compilateur lit quand vous appuyez sur F9.

#### À quoi ressemble un fichier .dpr ?

Voici un exemple de fichier .dpr pour un projet simple :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  FormPrincipale in 'FormPrincipale.pas' {Form1},
  FormSecondaire in 'FormSecondaire.pas' {Form2},
  UnitUtils in 'UnitUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
```

Analysons ce code :

**program MonApplication;** : déclare que c'est un programme et lui donne un nom.

**uses** : liste toutes les unités utilisées par le projet. Notez que chaque unité est suivie de **in 'NomFichier.pas'**, ce qui indique au compilateur où trouver le fichier.

**{$R *.res}** : directive qui inclut le fichier de ressources (.res).

**begin...end.** : c'est le code principal qui s'exécute au démarrage de l'application.

#### Que fait le code principal ?

**Application.Initialize;** : initialise l'objet Application, qui représente votre programme.

**Application.MainFormOnTaskbar := True;** : dit à Windows d'afficher l'icône de l'application dans la barre des tâches.

**Application.CreateForm(TForm1, Form1);** : crée la fiche principale. Le premier paramètre est la **classe** de la fiche (TForm1), le second est la **variable** qui contiendra l'instance de la fiche (Form1).

**Application.Run;** : lance la boucle d'événements de l'application. C'est ici que votre programme "attend" et réagit aux actions de l'utilisateur. Cette ligne ne se termine que quand l'utilisateur ferme l'application.

#### Devez-vous modifier le .dpr ?

**Rarement !** Delphi gère automatiquement ce fichier pour vous. Il ajoute ou supprime des lignes dans la clause **uses** et dans les **CreateForm** quand vous ajoutez ou supprimez des fiches.

Vous pourriez avoir besoin de le modifier dans des cas particuliers :
- Pour ajouter du code d'initialisation avant la création des fiches
- Pour changer l'ordre de création des fiches
- Pour vérifier des conditions avant de lancer l'application
- Pour configurer des paramètres globaux

Mais au début, laissez Delphi le gérer.

### Les unités de code (.pas)

Les fichiers .pas (Pascal) contiennent le code Object Pascal de votre application. C'est là que réside toute la logique de votre programme.

#### Structure d'une unité

Chaque unité suit une structure standard avec plusieurs sections. Voici un exemple d'unité complète :

```pascal
unit FormPrincipale;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    FCounter: Integer;
    procedure IncrementCounter;
  public
    { Déclarations publiques }
    function GetCounter: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  IncrementCounter;
  Label1.Caption := 'Compteur : ' + IntToStr(FCounter);
end;

procedure TForm1.IncrementCounter;
begin
  Inc(FCounter);
end;

function TForm1.GetCounter: Integer;
begin
  Result := FCounter;
end;

end.
```

Décortiquons cette structure :

#### La déclaration de l'unité

```pascal
unit FormPrincipale;
```

Cette ligne donne le nom de l'unité. Par convention, le nom de l'unité doit correspondre au nom du fichier. Si votre fichier s'appelle FormPrincipale.pas, l'unité doit s'appeler FormPrincipale.

#### La section interface

```pascal
interface

uses
  ...

type
  TForm1 = class(TForm)
    ...
  end;

var
  Form1: TForm1;
```

La section **interface** contient tout ce qui est **public**, c'est-à-dire visible par les autres unités de votre projet. Elle comprend :

**uses** : les unités dont cette unité a besoin. Ce sont les bibliothèques et les autres unités de votre projet.

**type** : les déclarations de types : classes, records, énumérations, etc. C'est ici que votre classe de fiche (TForm1) est déclarée.

**var** : les variables globales publiques. La variable Form1 représente l'instance de votre fiche.

**const** : les constantes publiques (si vous en avez).

**Méthodes et propriétés** : dans la déclaration de la classe, vous déclarez les méthodes et propriétés accessibles.

#### La section implementation

```pascal
implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ...
end;

...

end.
```

La section **implementation** contient le code réel, les implémentations des méthodes déclarées dans **interface**. C'est ici que vous écrivez le code qui fait fonctionner votre application.

**{$R *.dfm}** : cette directive lie le fichier .dfm (la définition visuelle de la fiche) à cette unité. Le compilateur charge le .dfm et crée les composants visuels automatiquement.

**Implémentations des méthodes** : chaque méthode déclarée dans **interface** doit être implémentée ici.

#### Uses dans interface vs implementation

Vous pouvez avoir une clause **uses** dans **interface** ET une autre dans **implementation** :

```pascal
interface

uses
  System.SysUtils, Vcl.Forms;  // Unités nécessaires dans l'interface

implementation

uses
  System.Math, UnitUtils;  // Unités nécessaires uniquement dans l'implémentation
```

**Pourquoi cette distinction ?**

- Les unités dans **interface** sont accessibles à toutes les autres unités qui utilisent cette unité (dépendances publiques).
- Les unités dans **implementation** ne sont visibles qu'à l'intérieur de cette unité (dépendances privées).

**Bonne pratique** : mettez dans **implementation** toutes les unités qui ne sont pas strictement nécessaires dans **interface**. Cela réduit les dépendances et accélère la compilation.

#### Les sections private et public

Dans la déclaration d'une classe, vous organisez les membres en sections :

**private** : accessible uniquement à l'intérieur de cette classe. Utilisez **private** pour les détails d'implémentation que personne d'autre n'a besoin de connaître.

**protected** : accessible dans cette classe et dans les classes dérivées.

**public** : accessible par tout le monde. Utilisez **public** pour les méthodes et propriétés que d'autres parties de votre code doivent utiliser.

**published** : comme **public**, mais les propriétés **published** apparaissent dans l'Inspecteur d'objets. C'est ce que Delphi utilise pour les composants visuels.

```pascal
type
  TForm1 = class(TForm)
    // Composants visuels (automatiquement en published)
    Button1: TButton;
  private
    // Variables privées
    FCounter: Integer;
    // Méthodes privées
    procedure IncrementCounter;
  public
    // Méthodes publiques
    function GetCounter: Integer;
  end;
```

### Les fichiers de fiches (.dfm)

Les fichiers .dfm (Delphi Form) décrivent l'apparence de vos fenêtres. Ils stockent toutes les propriétés que vous définissez dans le concepteur visuel et dans l'Inspecteur d'objets.

#### Format du fichier .dfm

Le .dfm est un fichier texte dans un format spécial. Voici un exemple simplifié :

```
object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Ma Premi'#232're Application'
  ClientHeight = 300
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []

  object Button1: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Cliquer'
    TabOrder = 0
    OnClick = Button1Click
  end

  object Label1: TLabel
    Left = 120
    Top = 30
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
end
```

Chaque composant est représenté avec toutes ses propriétés. Les composants conteneurs (comme la fiche) contiennent d'autres composants (comme les boutons et labels).

#### Voir et modifier le .dfm

Par défaut, vous voyez la fiche dans le concepteur visuel. Pour voir le .dfm en mode texte :

**Clic droit sur la fiche dans l'Explorateur de projets > Afficher en tant que texte**

Pour revenir au mode visuel :

**Clic droit > Afficher en tant que fiche**

**Attention** : vous pouvez modifier le .dfm directement en mode texte, mais c'est risqué. Une erreur de syntaxe peut rendre la fiche impossible à ouvrir. Préférez toujours utiliser le concepteur visuel et l'Inspecteur d'objets.

#### Synchronisation .pas et .dfm

Delphi maintient automatiquement une synchronisation entre le .pas et le .dfm :

- Quand vous ajoutez un bouton dans le concepteur, Delphi ajoute sa déclaration dans le .pas et sa définition dans le .dfm
- Quand vous changez une propriété dans l'Inspecteur, Delphi met à jour le .dfm
- Quand vous créez un gestionnaire d'événement, Delphi ajoute la déclaration dans le .pas et la référence dans le .dfm

Cette synchronisation magique est l'une des grandes forces de Delphi !

### Les fichiers de ressources (.res)

Les fichiers .res (Resource) contiennent des ressources binaires incorporées dans votre exécutable :

**L'icône de l'application** : l'icône qui s'affiche dans l'explorateur Windows et la barre des tâches

**Les manifestes** : informations pour Windows (niveau d'exécution, compatibilité)

**Autres ressources** : images, sons, données binaires que vous voulez incorporer

#### Voir les ressources

Pour voir et modifier les ressources de votre projet :

**Projet > Ressources et images**

Vous pouvez y ajouter, supprimer ou modifier l'icône de l'application et d'autres ressources.

#### Modifier l'icône de l'application

1. **Projet > Options > Application**
2. Cliquez sur **Charger l'icône**
3. Choisissez un fichier .ico
4. L'icône est incorporée dans le .res

Delphi recompile automatiquement le .res quand vous modifiez les ressources.

### Les fichiers de configuration (.dproj, .dof, .cfg)

Ces fichiers contiennent les paramètres et options de votre projet.

#### Le fichier .dproj

C'est le fichier de projet moderne au format XML. Il contient :

- Les configurations de compilation (Debug, Release)
- Les plateformes cibles (Win32, Win64, etc.)
- Les chemins de recherche
- Les options du compilateur et de l'éditeur de liens
- Les frameworks et bibliothèques utilisés

Vous ne devez généralement pas éditer ce fichier manuellement. Utilisez **Projet > Options** pour modifier les paramètres.

#### Autres fichiers de configuration

**.dof** : ancien format de configuration (versions antérieures à Delphi 2007)

**.cfg** : fichiers de configuration supplémentaires

**.local** : paramètres locaux spécifiques à votre machine (ne pas partager avec d'autres développeurs)

**.identcache** : cache pour accélérer l'IDE (peut être supprimé sans problème)

## Les fichiers générés par la compilation

Quand vous compilez votre projet, Delphi crée plusieurs fichiers.

### Les unités compilées (.dcu)

Les fichiers .dcu (Delphi Compiled Unit) sont la version compilée de vos fichiers .pas. Ils contiennent le code machine généré par le compilateur.

**Emplacement** : dans Win32\Debug ou Win32\Release

**Rôle** : fichiers intermédiaires de compilation. Delphi les utilise pour la compilation incrémentale (recompiler uniquement ce qui a changé).

**Suppression** : vous pouvez les supprimer sans problème via **Projet > Nettoyer**. Ils seront recréés à la prochaine compilation.

### L'exécutable (.exe)

C'est le fichier final, votre application exécutable.

**Emplacement** : dans Win32\Debug ou Win32\Release

**Rôle** : c'est ce fichier que vous distribuez et que les utilisateurs lancent.

**Taille** : en mode Debug, il est plus gros (avec les informations de débogage). En mode Release, il est optimisé et plus petit.

### Les fichiers de débogage (.map, .rsm)

Ces fichiers contiennent des informations pour le débogage :

**.map** : carte mémoire du programme (adresses des fonctions, variables)

**.rsm** : informations de débogage détaillées

Ces fichiers ne sont générés que si vous activez les options correspondantes dans **Projet > Options > Compilation > Éditeur de liens**.

**Usage** : utiles pour analyser les crashs en production, mais pas nécessaires pour le développement normal.

## Organisation du code en unités

### Pourquoi diviser le code en unités ?

Vous pourriez théoriquement mettre tout votre code dans une seule unité géante. Mais ce serait une très mauvaise idée ! Diviser votre code en unités offre plusieurs avantages :

**Modularité** : chaque unité a une responsabilité claire

**Réutilisabilité** : vous pouvez réutiliser une unité dans plusieurs projets

**Compilation plus rapide** : seules les unités modifiées sont recompilées

**Travail en équipe** : plusieurs développeurs peuvent travailler sur différentes unités sans conflit

**Maintenance** : plus facile de trouver et corriger les bugs dans des unités petites et ciblées

**Lisibilité** : un fichier de 200 lignes est plus facile à comprendre qu'un fichier de 5000 lignes

### Types d'unités

Dans un projet typique, vous aurez différents types d'unités :

#### Unités de fiches

Chaque fiche (fenêtre) a son unité associée. Ces unités contiennent :

- La déclaration de la classe de la fiche
- Les composants visuels
- Les gestionnaires d'événements
- La logique spécifique à cette fiche

**Exemple** : FormPrincipale.pas, FormLogin.pas, FormOptions.pas

#### Unités de logique métier

Ces unités contiennent la logique de votre application, sans interface visuelle :

- Calculs
- Traitement de données
- Algorithmes
- Règles métier

**Exemple** : UnitCalculs.pas, UnitValidation.pas, UnitBusiness.pas

#### Unités utilitaires

Des fonctions et procédures génériques, réutilisables :

- Manipulation de chaînes
- Conversion de données
- Fonctions mathématiques
- Helpers et extensions

**Exemple** : UnitUtils.pas, UnitHelpers.pas, UnitTools.pas

#### Unités d'accès aux données

Code lié aux bases de données et à la persistance :

- Connexions aux bases de données
- Requêtes SQL
- Objets métier (entités)

**Exemple** : UnitDatabase.pas, UnitModels.pas, UnitDAO.pas

#### Unités de constantes et types

Déclarations partagées dans tout le projet :

- Constantes globales
- Types personnalisés
- Énumérations
- Records

**Exemple** : UnitConstants.pas, UnitTypes.pas, UnitGlobals.pas

### Conventions de nommage des unités

Voici quelques conventions courantes pour nommer vos unités :

**Préfixe Form** : FormMain, FormLogin, FormSettings (pour les fiches)

**Préfixe Unit** : UnitDatabase, UnitUtils, UnitValidation (pour le code sans interface)

**Pas de préfixe** : Main, Login, Database (style plus moderne et concis)

**Préfixe du projet** : MyAppMain, MyAppLogin (pour éviter les conflits avec d'autres bibliothèques)

Choisissez une convention et restez cohérent dans tout votre projet.

## Dépendances entre unités

### Comprendre les dépendances

Quand une unité A utilise (uses) une unité B, on dit que A **dépend** de B. Les dépendances créent une hiérarchie :

```
FormPrincipale.pas
  ↓ uses
UnitDatabase.pas
  ↓ uses
UnitUtils.pas
```

### Le problème des dépendances circulaires

Une **dépendance circulaire** se produit quand deux unités dépendent l'une de l'autre :

```
UnitA uses UnitB
UnitB uses UnitA
```

C'est un problème ! Le compilateur Delphi ne peut pas compiler cette situation dans la section **interface**.

#### Solution 1 : Uses dans implementation

Si UnitA a besoin de UnitB uniquement dans son implementation, mettez le uses dans la section implementation :

```pascal
unit UnitA;

interface

uses
  UnitB;  // UnitA dépend de UnitB

implementation

// Pas de uses UnitC ici car pas besoin

end.
```

```pascal
unit UnitB;

interface

// Pas de uses UnitA dans interface

implementation

uses
  UnitA;  // UnitB dépend de UnitA uniquement ici - OK !

end.
```

#### Solution 2 : Refactoriser

Souvent, une dépendance circulaire indique un problème de conception. Réorganisez votre code :

- Créez une troisième unité pour le code partagé
- Utilisez des interfaces plutôt que des classes concrètes
- Revoyez la responsabilité de chaque unité

### Minimiser les dépendances

**Principe** : une unité devrait dépendre du minimum d'autres unités nécessaires.

**Pourquoi ?**

- Compilation plus rapide
- Moins de risques de dépendances circulaires
- Code plus modulaire et réutilisable
- Plus facile à tester

**Comment ?**

- Ne mettez dans **interface** que les uses vraiment nécessaires
- Utilisez des déclarations anticipées (forward declarations) quand possible
- Passez des paramètres plutôt que d'accéder à des variables globales

## Gestion multi-plateforme

### Structure pour Win32 et Win64

Si votre projet cible plusieurs plateformes, la structure s'enrichit :

```
MonProjet/
├── MonProjet.dpr
├── MonProjet.dproj
├── FormMain.pas
├── FormMain.dfm
├── Win32/
│   ├── Debug/
│   └── Release/
└── Win64/
    ├── Debug/
    └── Release/
```

Chaque plateforme a son propre dossier de sortie. Le même code source génère des exécutables différents pour chaque plateforme.

### Structure FireMonkey (multi-plateforme)

Pour un projet FireMonkey ciblant plusieurs systèmes d'exploitation :

```
MonProjetFMX/
├── MonProjetFMX.dpr
├── MonProjetFMX.dproj
├── FormMain.pas
├── FormMain.fmx     (au lieu de .dfm)
├── Win32/
├── Win64/
├── OSX64/
├── Android/
├── Android64/
├── iOSDevice64/
└── iOSSimulator/
```

Notez que les fiches FireMonkey utilisent **.fmx** au lieu de **.dfm**.

## Fichiers à inclure dans le contrôle de version

Si vous utilisez Git, SVN ou un autre système de contrôle de version, voici ce qu'il faut inclure et exclure :

### À INCLURE (commit)

✅ Fichiers source :
- **.pas** (code source)
- **.dfm** ou **.fmx** (fiches)
- **.dpr** (projet)
- **.dproj** (configuration du projet)
- **.res** (ressources personnalisées)

✅ Fichiers de configuration partagés :
- **.dof** (si vous utilisez d'anciennes versions)

### À EXCLURE (.gitignore)

❌ Fichiers générés :
- **.dcu** (unités compilées)
- **.exe** (exécutables)
- **.dll** (bibliothèques compilées)
- Tous les fichiers dans Win32/, Win64/, etc.

❌ Fichiers spécifiques à la machine locale :
- **.local** (paramètres locaux)
- **.identcache** (cache)
- **__history/** (historique local)
- **__recovery/** (fichiers de récupération)

❌ Fichiers temporaires :
- **.~pas**, **.~dfm** (backups)
- **.stat** (statistiques)

### Exemple de .gitignore pour Delphi

```gitignore
# Fichiers compilés
*.dcu
*.exe
*.dll
*.bpl
*.dcp
*.so
*.apk
*.dylib

# Dossiers de sortie
Win32/
Win64/
OSX32/
OSX64/
Android/
Android64/
iOSDevice/
iOSDevice64/
iOSSimulator/

# Fichiers temporaires
*.local
*.identcache
*.stat
__history/
__recovery/
*.~*

# Logs
*.log
```

## Bonnes pratiques d'organisation

### Structure recommandée pour un projet moyen

```
MonProjet/
├── Source/                  (Code source)
│   ├── Forms/              (Fiches)
│   │   ├── FormMain.pas
│   │   ├── FormMain.dfm
│   │   ├── FormLogin.pas
│   │   └── FormLogin.dfm
│   ├── Business/           (Logique métier)
│   │   ├── UnitCalculs.pas
│   │   └── UnitValidation.pas
│   ├── Data/               (Accès aux données)
│   │   └── UnitDatabase.pas
│   └── Utils/              (Utilitaires)
│       └── UnitHelpers.pas
├── Resources/              (Ressources)
│   ├── Images/
│   └── Icons/
├── Documentation/          (Documentation)
│   └── README.md
├── Tests/                  (Tests unitaires)
│   └── TestCalculs.pas
├── MonProjet.dpr
├── MonProjet.dproj
└── Win32/
```

**Note** : Delphi ne crée pas automatiquement ces sous-dossiers. Vous devez les créer manuellement et configurer les chemins dans **Projet > Options > Répertoires et conditions**.

### Conseils d'organisation

**Un fichier, une responsabilité** : chaque unité devrait avoir un objectif clair et limité.

**Nommage cohérent** : utilisez la même convention de nommage dans tout le projet.

**Grouper par fonctionnalité** : organisez vos unités par domaine fonctionnel plutôt que par type technique.

**Limiter la taille des unités** : une unité de plus de 1000 lignes devrait probablement être divisée.

**Documenter les dépendances** : si une unité dépend d'une autre de manière non évidente, documentez-le dans un commentaire.

**Éviter les variables globales** : préférez passer des paramètres ou utiliser des classes singleton.

## Erreurs courantes et solutions

### "Unit X not found"

**Cause** : le compilateur ne trouve pas le fichier .pas de l'unité X.

**Solutions** :
- Vérifiez l'orthographe dans la clause uses
- Assurez-vous que le fichier existe dans le projet
- Ajoutez le chemin du fichier dans **Projet > Options > Répertoires et conditions > Chemin de recherche**

### "Circular unit reference"

**Cause** : deux unités se référencent mutuellement dans leur section interface.

**Solutions** :
- Déplacez un des uses dans la section implementation
- Refactorisez le code pour éliminer la dépendance circulaire
- Créez une troisième unité pour le code partagé

### Fichier .dfm corrompu

**Cause** : modification manuelle incorrecte du .dfm, crash pendant la sauvegarde.

**Solutions** :
- Restaurez depuis __history/ ou depuis votre contrôle de version
- Si possible, ouvrez le .dfm en mode texte et corrigez l'erreur
- En dernier recours, recréez la fiche

### Projet qui ne s'ouvre plus

**Cause** : fichier .dproj corrompu, chemins incorrects.

**Solutions** :
- Essayez d'ouvrir le .dpr directement au lieu du .dproj
- Restaurez le .dproj depuis une sauvegarde
- Créez un nouveau projet et réimportez vos fichiers

## Conclusion

La structure d'un projet Delphi peut sembler complexe au premier abord, mais elle est logique et bien organisée. Comprendre cette structure vous permet de :

- **Naviguer efficacement** dans votre projet
- **Organiser votre code** de manière professionnelle
- **Résoudre les problèmes** de compilation et de dépendances
- **Collaborer** avec d'autres développeurs
- **Maintenir** votre code sur le long terme

Points essentiels à retenir :

- Le **.dpr** est le chef d'orchestre, les **.pas** sont les ouvriers
- Chaque **.pas** a une section **interface** (publique) et **implementation** (privée)
- Les **.dfm** décrivent l'apparence, synchronisés automatiquement avec les **.pas**
- Organisez votre code en **unités** par responsabilité
- Minimisez les **dépendances** entre unités
- Utilisez le **contrôle de version** pour protéger votre travail

Avec la pratique, cette structure deviendra naturelle. Vous créerez instinctivement des unités bien organisées, et vous saurez immédiatement où chercher quand quelque chose ne fonctionne pas.

Dans la prochaine section, nous découvrirons le cycle de développement avec Delphi, de la conception initiale jusqu'au déploiement de l'application !

⏭️ [Introduction au cycle de développement avec Delphi](/02-decouverte-de-lide-delphi/08-introduction-au-cycle-de-developpement.md)
