🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.7 Programmation orientée objet

## Introduction

La **Programmation Orientée Objet** (POO) est un paradigme de programmation qui organise le code autour du concept d'**objets** plutôt que de simples fonctions et données séparées. C'est l'une des approches les plus importantes et les plus utilisées dans le développement logiciel moderne.

Delphi, avec son langage Object Pascal, place la programmation orientée objet au cœur de sa philosophie. Que vous créiez une simple application de bureau ou un système complexe multi-plateforme, vous utiliserez constamment les principes de la POO.

## Qu'est-ce que la Programmation Orientée Objet ?

### Analogie du monde réel

Imaginez que vous devez modéliser une bibliothèque dans un programme :

**Approche procédurale (ancienne méthode)** :
- Des variables séparées : `TitreLivre1`, `AuteurLivre1`, `AnnéeLivre1`, `TitreLivre2`, `AuteurLivre2`...
- Des fonctions séparées : `AfficherTitreLivre()`, `EmprunterLivre()`, `RendreLivre()`...
- Difficile à maintenir et à organiser

**Approche orientée objet (moderne)** :
- Un objet **Livre** qui regroupe :
  - Ses données : titre, auteur, année, disponibilité
  - Ses comportements : afficher, emprunter, rendre
- Beaucoup plus naturel et organisé !

La POO nous permet de penser et de programmer de manière plus proche de la façon dont nous percevons le monde réel.

## Les fondements de la POO

La programmation orientée objet repose sur quatre piliers fondamentaux :

### 1. L'Encapsulation

L'**encapsulation** consiste à regrouper les données (attributs) et les comportements (méthodes) qui agissent sur ces données au sein d'une même entité : l'objet.

**Avantage** : On cache les détails internes et on expose uniquement ce qui est nécessaire. C'est comme une voiture : vous utilisez le volant et les pédales sans avoir besoin de comprendre comment fonctionne le moteur.

### 2. L'Héritage

L'**héritage** permet de créer de nouvelles classes basées sur des classes existantes, en réutilisant et en étendant leurs fonctionnalités.

**Exemple** :
- Une classe `Véhicule` (caractéristiques communes : marque, vitesse, démarrer, arrêter)
- Une classe `Voiture` qui hérite de `Véhicule` (ajoute : nombre de portes, klaxonner)
- Une classe `Camion` qui hérite de `Véhicule` (ajoute : capacité de chargement, décharger)

**Avantage** : Évite la duplication de code et crée une hiérarchie logique.

### 3. Le Polymorphisme

Le **polymorphisme** (du grec "plusieurs formes") permet à des objets de types différents d'être traités de manière uniforme tout en conservant leur comportement spécifique.

**Exemple** :
- Différents animaux peuvent tous "faire du bruit"
- Un chien fait "Wouf", un chat fait "Miaou", une vache fait "Meuh"
- On peut appeler la méthode `FaireDuBruit()` sans savoir précisément quel type d'animal on a

**Avantage** : Flexibilité et extensibilité du code.

### 4. L'Abstraction

L'**abstraction** consiste à représenter les concepts essentiels sans inclure les détails d'implémentation. On se concentre sur **ce que fait** un objet plutôt que sur **comment il le fait**.

**Exemple** : Une télécommande abstraite les commandes complexes de la télévision en simples boutons.

**Avantage** : Simplifie la complexité et facilite la compréhension.

## Pourquoi utiliser la POO ?

### Avantages de la POO

1. **Organisation du code**
   - Le code est structuré de manière logique et intuitive
   - Facile de retrouver où se trouve une fonctionnalité
   - Reflète mieux les problèmes du monde réel

2. **Réutilisabilité**
   - On écrit du code une fois et on le réutilise partout
   - L'héritage permet d'étendre les fonctionnalités sans réécrire
   - Les composants peuvent être utilisés dans différents projets

3. **Maintenance facilitée**
   - Les modifications sont localisées dans des classes spécifiques
   - Moins de risque de casser le code existant
   - Plus facile de comprendre et de corriger les bugs

4. **Évolutivité**
   - Facile d'ajouter de nouvelles fonctionnalités
   - On peut étendre le système sans modifier l'existant
   - Le code s'adapte mieux aux changements

5. **Travail en équipe**
   - Différents développeurs peuvent travailler sur différentes classes
   - Les interfaces clairement définies facilitent la collaboration
   - Division naturelle du travail

6. **Qualité du code**
   - Code plus lisible et compréhensible
   - Moins d'erreurs grâce à l'encapsulation
   - Tests plus faciles à écrire et à maintenir

## Comparaison : Programmation Procédurale vs POO

### Programmation Procédurale

```pascal
// Exemple de code procédural (ancien style)
var
  NomUtilisateur1: string;
  AgeUtilisateur1: Integer;
  EmailUtilisateur1: string;

  NomUtilisateur2: string;
  AgeUtilisateur2: Integer;
  EmailUtilisateur2: string;

procedure AfficherUtilisateur(Nom: string; Age: Integer; Email: string);  
begin  
  WriteLn('Nom: ' + Nom);
  WriteLn('Âge: ' + IntToStr(Age));
  WriteLn('Email: ' + Email);
end;

procedure EnvoyerEmail(Email: string; Message: string);  
begin  
  // Code pour envoyer l'email
end;
```

**Problèmes** :
- Données et comportements séparés
- Difficile de gérer plusieurs utilisateurs
- Code répétitif et difficile à maintenir

### Programmation Orientée Objet

```pascal
// Exemple de code orienté objet (style moderne)
type
  TUtilisateur = class
  private
    FNom: string;
    FAge: Integer;
    FEmail: string;
  public
    procedure Afficher;
    procedure EnvoyerEmail(const Message: string);
    property Nom: string read FNom write FNom;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

var
  Utilisateur1, Utilisateur2: TUtilisateur;
```

**Avantages** :
- Données et comportements regroupés
- Facile de créer autant d'utilisateurs que nécessaire
- Code organisé et maintenable

## Les concepts clés de la POO en Delphi

Cette section 3.7 va explorer en détail les concepts suivants :

### Classes et objets
- Qu'est-ce qu'une classe et un objet ?
- Comment définir et utiliser des classes
- La différence entre classe (modèle) et objet (instance)

### Propriétés et méthodes
- Les propriétés : accès contrôlé aux données
- Les méthodes : actions que les objets peuvent effectuer
- Encapsulation et protection des données

### Héritage et polymorphisme
- Créer des hiérarchies de classes
- Réutiliser et étendre le code existant
- Le polymorphisme pour un code flexible

### Constructeurs et destructeurs
- Initialiser correctement les objets
- Libérer la mémoire et les ressources
- Gestion du cycle de vie des objets

### Interfaces
- Définir des contrats que les classes doivent respecter
- Implémenter plusieurs interfaces
- Programmation par contrat

### Généricité
- Créer du code réutilisable pour différents types
- Les collections génériques
- Éviter la duplication de code

## La POO dans Delphi

### Delphi et la POO

Delphi a été conçu dès le départ avec la POO comme fondement :

- **Tous les composants visuels sont des objets** : boutons, zones de texte, formulaires...
- **L'IDE exploite la POO** : l'inspecteur d'objets, la palette de composants...
- **Le framework VCL/FMX est orienté objet** : toute l'architecture repose sur des classes

Même si vous ne vous en rendez pas compte au début, dès que vous créez un formulaire dans Delphi, vous créez une classe !

### Un formulaire Delphi est une classe

Quand vous créez un nouveau formulaire dans Delphi, l'IDE génère automatiquement du code orienté objet :

```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
```

Ce code définit une **classe** `TForm1` qui :
- **Hérite** de `TForm` (héritage)
- Contient des **composants** (encapsulation)
- Peut répondre à des **événements** (méthodes)

### Tout est objet en Delphi

En Delphi moderne, presque tout est un objet :
- Les formulaires (`TForm`)
- Les composants (`TButton`, `TEdit`, `TLabel`...)
- Les collections (`TList`, `TStringList`...)
- Les connexions aux bases de données (`TFDConnection`)
- Les composants réseau (`TRESTClient`)
- Et bien plus encore !

## Évolution de la POO en Delphi

### Du Pascal procédural à Object Pascal

Delphi est l'évolution du langage Pascal classique :

**Pascal classique** (Turbo Pascal) :
- Principalement procédural
- Quelques notions d'objets basiques

**Object Pascal** (Delphi) :
- POO complète et moderne
- Interfaces, généricité, attributs
- Constamment enrichi avec de nouvelles fonctionnalités

### Delphi moderne

Les versions récentes de Delphi ont ajouté :
- **Généricité** : code réutilisable pour différents types
- **Méthodes anonymes** : fonctions en ligne
- **RTTI étendue** : réflexion et introspection
- **Attributs** : métadonnées sur les classes
- **Expressions régulières** intégrées
- **Opérateur ternaire** (Delphi 13)

## Philosophie de développement en POO

### Penser en termes d'objets

Quand vous concevez une application en POO, vous devez :

1. **Identifier les entités** : Quels sont les "objets" de votre domaine ?
   - Une application de gestion : Client, Commande, Produit, Facture...
   - Un jeu : Joueur, Ennemi, Arme, Niveau...

2. **Définir les responsabilités** : Que fait chaque objet ?
   - Un Client peut passer une commande
   - Une Commande peut calculer son total
   - Un Produit peut être en stock ou en rupture

3. **Établir les relations** : Comment les objets interagissent ?
   - Un Client possède plusieurs Commandes
   - Une Commande contient plusieurs Produits
   - Une Facture est générée à partir d'une Commande

4. **Identifier les hiérarchies** : Peut-on réutiliser du code ?
   - Personne → Client, Employé
   - Document → Facture, Devis, BonDeCommande
   - Véhicule → Voiture, Camion, Moto

### Principes de conception

Les bons développeurs POO suivent des principes éprouvés :

**SOLID** (acronyme de 5 principes) :
- **S**ingle Responsibility : une classe, une responsabilité
- **O**pen/Closed : ouvert à l'extension, fermé à la modification
- **L**iskov Substitution : les classes dérivées doivent être substituables
- **I**nterface Segregation : interfaces spécifiques plutôt que générales
- **D**ependency Inversion : dépendre d'abstractions, pas de détails

**DRY** : Don't Repeat Yourself (ne vous répétez pas)
- Si vous écrivez le même code deux fois, créez une méthode ou une classe

**KISS** : Keep It Simple, Stupid (restez simple)
- Ne compliquez pas inutilement votre code

## Pour bien commencer

### État d'esprit

Pour réussir en POO :

1. **Pensez "objets"** : ne pensez plus en termes de fonctions séparées, mais en termes d'entités qui collaborent

2. **Commencez simple** : créez des classes simples avant de vous lancer dans des hiérarchies complexes

3. **Pratiquez** : la POO devient naturelle avec la pratique

4. **Lisez du code** : étudiez les classes existantes de Delphi (VCL, FMX) pour voir comment elles sont conçues

5. **Refactorisez** : améliorez continuellement votre code en appliquant les principes POO

### Ce que vous allez apprendre

Dans les sections suivantes, vous découvrirez en détail :

1. **Les classes et objets** : les briques de base de la POO
2. **Les propriétés et méthodes** : comment encapsuler données et comportements
3. **L'héritage et le polymorphisme** : réutiliser et étendre le code
4. **Les constructeurs et destructeurs** : gérer le cycle de vie des objets
5. **Les interfaces** : définir des contrats flexibles
6. **La généricité** : créer du code réutilisable et type-safe

Chaque concept sera expliqué avec des exemples concrets et progressifs, adaptés aux débutants.

## Résumé

- La **Programmation Orientée Objet** organise le code autour d'objets qui combinent données et comportements

- Les **quatre piliers de la POO** :
  - **Encapsulation** : regrouper données et méthodes
  - **Héritage** : réutiliser et étendre le code
  - **Polymorphisme** : traiter différents objets de manière uniforme
  - **Abstraction** : se concentrer sur l'essentiel

- **Avantages** : organisation, réutilisabilité, maintenance, évolutivité, qualité

- **Delphi est orienté objet** : formulaires, composants, tout est objet

- **Object Pascal** : langage moderne avec POO complète

- **État d'esprit** : penser en termes d'objets et de responsabilités

La maîtrise de la POO est essentielle pour devenir un bon développeur Delphi. Les concepts peuvent sembler abstraits au début, mais avec de la pratique, ils deviendront une seconde nature. Commençons maintenant notre voyage dans le monde de la programmation orientée objet !

⏭️ [Classes et objets](/03-langage-object-pascal/07.1-classes-objets.md)
