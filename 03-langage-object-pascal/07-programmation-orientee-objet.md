# 3.7 Programmation orientée objet

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La programmation orientée objet (POO) est un paradigme de programmation qui permet d'organiser le code de manière plus structurée et modulaire. Object Pascal, le langage utilisé dans Delphi, est un langage orienté objet très puissant. Cette section vous présente les concepts fondamentaux de la POO dans Delphi.

## Qu'est-ce que la programmation orientée objet ?

La programmation orientée objet est une approche qui permet de modéliser des problèmes en termes d'"objets", qui sont des entités combinant des données (attributs) et des comportements (méthodes). Cette approche présente plusieurs avantages par rapport à la programmation procédurale traditionnelle :

- **Modularité** : Le code est organisé en unités indépendantes (objets)
- **Réutilisabilité** : Les objets peuvent être réutilisés dans différentes parties du programme ou même dans d'autres projets
- **Extensibilité** : Vous pouvez créer de nouveaux objets basés sur des objets existants
- **Maintenance** : Le code est généralement plus facile à maintenir et à faire évoluer

## Concepts clés de la POO

Avant de plonger dans les détails, voici les concepts fondamentaux que vous devez comprendre :

### Objets et classes

- Une **classe** est un modèle qui définit les caractéristiques et comportements d'un type d'objet.
- Un **objet** est une instance concrète d'une classe.

Pour faire une analogie, une classe est comme un plan d'architecte pour une maison, tandis qu'un objet est une maison réelle construite selon ce plan. Vous pouvez créer plusieurs maisons (objets) à partir d'un même plan (classe).

### Attributs et méthodes

- Les **attributs** (ou champs) sont les données stockées dans un objet.
- Les **méthodes** sont les fonctions et procédures qui définissent le comportement d'un objet.

Par exemple, une classe `TVoiture` pourrait avoir des attributs comme `Couleur`, `Marque` et `VitesseMaximale`, et des méthodes comme `Demarrer()`, `Accelerer()` et `Freiner()`.

### Encapsulation

L'**encapsulation** est le principe qui consiste à regrouper les données et les méthodes au sein d'une classe, tout en cachant les détails internes. Cela se fait généralement en utilisant des niveaux de visibilité (`private`, `protected`, `public`, etc.).

L'encapsulation permet de :
- Protéger les données contre les modifications accidentelles
- Cacher la complexité interne
- Présenter une interface simple et claire pour utiliser l'objet

### Héritage

L'**héritage** permet de créer une nouvelle classe (classe dérivée) à partir d'une classe existante (classe de base). La classe dérivée hérite de tous les attributs et méthodes de la classe de base, et peut en ajouter de nouveaux ou modifier ceux existants.

Par exemple, à partir d'une classe `TVehicule`, vous pourriez créer des classes dérivées comme `TVoiture`, `TMoto` et `TCamion`, qui héritent des caractéristiques communes tout en ajoutant leurs spécificités.

### Polymorphisme

Le **polymorphisme** permet à des objets de classes différentes d'être traités comme des objets d'une classe commune. Cela se fait généralement via des méthodes virtuelles qui peuvent être redéfinies dans les classes dérivées.

Par exemple, différents types de véhicules peuvent avoir une méthode `Avancer()`, mais chacun l'implémente différemment.

### Abstraction

L'**abstraction** consiste à se concentrer sur les caractéristiques essentielles d'un objet tout en ignorant les détails moins importants. Cela permet de créer des modèles simplifiés mais fonctionnels de concepts complexes.

## Pourquoi utiliser la POO en Delphi ?

Delphi est construit autour de la POO. En fait, tous les composants visuels que vous placez sur un formulaire sont des objets :
- Un bouton (`TButton`) est un objet
- Un champ texte (`TEdit`) est un objet
- Le formulaire lui-même (`TForm`) est un objet

En comprenant et en utilisant la POO, vous pourrez :
- Créer vos propres composants réutilisables
- Étendre les fonctionnalités des composants existants
- Organiser votre code de manière plus logique et maintenable
- Profiter pleinement de la puissance de Delphi

## Différence avec la programmation procédurale

Jusqu'à présent dans ce tutoriel, nous avons principalement utilisé la programmation procédurale, qui organise le code en procédures et fonctions. Voici les principales différences entre ces deux approches :

| Programmation procédurale | Programmation orientée objet |
|---------------------------|------------------------------|
| Se concentre sur les procédures | Se concentre sur les objets |
| Les données et les fonctions sont séparées | Les données et les méthodes sont regroupées dans des classes |
| Approche descendante (top-down) | Approche basée sur les objets et leurs interactions |
| Plus simple pour les petits programmes | Plus adaptée aux grands projets complexes |

En Delphi, vous pouvez utiliser les deux approches, mais la POO est particulièrement avantageuse pour les projets d'envergure.

## Quand utiliser la POO ?

La POO n'est pas toujours nécessaire pour tous les aspects d'un programme. Voici quelques situations où elle est particulièrement utile :

- Quand vous modélisez des entités du monde réel (clients, produits, commandes...)
- Quand vous créez des composants graphiques personnalisés
- Quand vous avez besoin de réutiliser du code dans différents contextes
- Quand vous travaillez sur un grand projet avec plusieurs développeurs
- Quand vous voulez créer une architecture maintenable à long terme

## Exemple simple : de procédural à orienté objet

Pour illustrer la différence entre les deux approches, prenons l'exemple d'une calculatrice simple.

**Approche procédurale :**

```pascal
function Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function Soustraire(A, B: Integer): Integer;
begin
  Result := A - B;
end;

procedure UtiliserCalculatrice;
var
  X, Y, Resultat: Integer;
begin
  X := 10;
  Y := 5;

  Resultat := Additionner(X, Y);
  ShowMessage('Addition : ' + IntToStr(Resultat));

  Resultat := Soustraire(X, Y);
  ShowMessage('Soustraction : ' + IntToStr(Resultat));
end;
```

**Approche orientée objet :**

```pascal
type
  TCalculatrice = class
  private
    FDernierResultat: Integer;
  public
    function Additionner(A, B: Integer): Integer;
    function Soustraire(A, B: Integer): Integer;
    property DernierResultat: Integer read FDernierResultat;
  end;

function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  FDernierResultat := A + B;
  Result := FDernierResultat;
end;

function TCalculatrice.Soustraire(A, B: Integer): Integer;
begin
  FDernierResultat := A - B;
  Result := FDernierResultat;
end;

procedure UtiliserCalculatrice;
var
  Calc: TCalculatrice;
  X, Y: Integer;
begin
  Calc := TCalculatrice.Create;
  try
    X := 10;
    Y := 5;

    ShowMessage('Addition : ' + IntToStr(Calc.Additionner(X, Y)));
    ShowMessage('Soustraction : ' + IntToStr(Calc.Soustraire(X, Y)));
    ShowMessage('Dernier résultat : ' + IntToStr(Calc.DernierResultat));
  finally
    Calc.Free;  // Libération de la mémoire
  end;
end;
```

Dans l'approche orientée objet :
- Nous avons une classe `TCalculatrice` qui regroupe les opérations et les données
- La classe maintient un état interne (le dernier résultat)
- Nous créons un objet `Calc` à partir de cette classe
- Nous appelons les méthodes de cet objet
- Nous libérons la mémoire avec `Free` quand nous avons terminé

## Préparation aux sous-sections suivantes

Dans les sous-sections suivantes, nous explorerons en détail tous les aspects de la programmation orientée objet en Delphi :

- Classes et objets
- Propriétés et méthodes
- Héritage et polymorphisme
- Constructeurs et destructeurs
- Interfaces
- Généricité

Ces concepts vous permettront de tirer pleinement parti de la puissance de Delphi et de créer des applications bien structurées et maintenables.

---

La programmation orientée objet est un paradigme puissant qui vous permettra de mieux organiser votre code et de créer des applications plus robustes. Dans les sections suivantes, nous explorerons en détail chaque aspect de la POO en Delphi, en commençant par les classes et les objets.

⏭️ [Classes et objets](/03-langage-object-pascal/07.1-classes-objets.md)

