# 12.2 Inspection et modification des variables

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction à l'inspection des variables

Une fois que vous avez suspendu l'exécution de votre programme avec un point d'arrêt, l'une des actions les plus utiles que vous pouvez effectuer est d'examiner l'état de vos variables. Delphi offre plusieurs outils puissants pour inspecter et même modifier les variables pendant le débogage, ce qui vous permet de comprendre et de corriger efficacement les problèmes dans votre code.

## Les fenêtres d'inspection du débogueur

Delphi propose plusieurs fenêtres de débogage pour inspecter les variables, chacune ayant son propre usage et ses avantages.

### 1. Fenêtre de suivi local (Local Variables)

La fenêtre de suivi local affiche automatiquement toutes les variables disponibles dans la portée actuelle (contexte d'exécution) :

1. Pendant le débogage, allez dans le menu **View > Debug Windows > Local Variables** (Vue > Fenêtres de débogage > Variables locales)
2. Vous verrez apparaître une fenêtre listant toutes les variables locales avec leurs types et valeurs actuelles

![Fenêtre des variables locales](https://via.placeholder.com/500x200)

Cette fenêtre est particulièrement utile car elle affiche automatiquement toutes les variables de la fonction ou procédure en cours d'exécution, vous évitant d'avoir à les ajouter manuellement.

### 2. Fenêtre Watches (Surveillance)

La fenêtre Watches vous permet de suivre spécifiquement des variables ou expressions de votre choix :

1. Ouvrez-la via **View > Debug Windows > Watches** (Vue > Fenêtres de débogage > Surveillance)
2. Pour ajouter une expression à surveiller :
   - Cliquez avec le bouton droit et sélectionnez **Add Watch...** (Ajouter surveillance...)
   - Tapez le nom de la variable ou l'expression à surveiller
   - OU utilisez le raccourci Ctrl+F5 lorsque le curseur est sur une variable dans l'éditeur

![Fenêtre de surveillance](https://via.placeholder.com/500x200)

Avantages de la fenêtre Watches :
- Elle persiste entre les sessions de débogage
- Vous pouvez y ajouter des expressions complexes (comme `Total / Count` ou `Length(MaChaine)`)
- Elle permet d'organiser vos surveillances selon vos besoins

### 3. Évaluation et modification rapide (Evaluate/Modify)

Pour une inspection rapide ou une modification ponctuelle :

1. Pendant le débogage, sélectionnez une variable ou une expression dans le code
2. Appuyez sur **Ctrl+F7** ou cliquez avec le bouton droit et choisissez **Evaluate/Modify** (Évaluer/Modifier)
3. Une fenêtre s'ouvre avec la valeur actuelle et vous permet de la modifier

![Fenêtre Évaluer/Modifier](https://via.placeholder.com/400x300)

Cette méthode est idéale pour des vérifications rapides ou lorsque vous souhaitez tester comment votre programme réagirait à une valeur différente.

### 4. Info-bulles de débogage

La méthode la plus simple pour une vérification rapide :

1. Pendant le débogage, survolez simplement une variable avec votre souris
2. Une info-bulle apparaîtra avec le type et la valeur actuelle de la variable

Pour les objets ou structures complexes, vous pouvez cliquer sur le signe + dans l'info-bulle pour explorer leurs propriétés.

## Inspection de différents types de données

### Variables simples (Integer, Boolean, String, etc.)

Pour les types simples, la valeur s'affiche directement dans les fenêtres d'inspection :

```
Nom      | Type    | Valeur
---------|---------|-------
compteur | Integer | 42
actif    | Boolean | True
nom      | String  | 'Exemple'
```

### Tableaux (Arrays)

Les tableaux s'affichent avec leurs indices et valeurs correspondantes :

```
MonTableau[0] = 10
MonTableau[1] = 20
MonTableau[2] = 30
```

Pour les grands tableaux, vous pouvez visualiser un sous-ensemble spécifique en utilisant la notation d'intervalle :

```pascal
MonTableau[5..10]  // Affiche les éléments de l'indice 5 à 10
```

### Objets et classes

Les objets sont affichés sous forme hiérarchique, vous permettant de développer leurs propriétés :

```
MaPersonne : TPersonne
  + Nom = 'Dubois'
  + Prenom = 'Jean'
  + Age = 42
  + Adresse : TAdresse
    + Rue = '123 rue des Exemples'
    + Ville = 'Paris'
    ...
```

### Collections et listes

Pour les collections comme `TList`, `TStringList` ou `TObjectList`, Delphi affiche le nombre d'éléments et vous permet d'explorer chaque élément :

```
MaListe : TStringList (Count=3)
  + [0] = 'Premier élément'
  + [1] = 'Deuxième élément'
  + [2] = 'Troisième élément'
```

### Types génériques

> 💡 **Astuce**: Delphi 12 Athens améliore considérablement l'affichage des types génériques dans le débogueur, permettant une inspection plus intuitive des listes et dictionnaires génériques.
>
> **Nécessite Delphi 12 ou supérieur**

## Modification des variables pendant le débogage

Le débogueur Delphi ne se contente pas d'afficher les valeurs, il vous permet également de les modifier à la volée pour tester différents scénarios.

### Comment modifier une variable

1. **Via la fenêtre Evaluate/Modify** :
   - Sélectionnez la variable dans l'éditeur
   - Appuyez sur Ctrl+F7
   - Entrez la nouvelle valeur dans le champ "New value" et cliquez sur "Modify"

2. **Via les fenêtres d'inspection** :
   - Dans la fenêtre Local Variables ou Watches, double-cliquez sur la valeur d'une variable
   - Entrez la nouvelle valeur et appuyez sur Entrée

### Exemples de modifications selon les types

#### Types simples

- Integer : entrez simplement un nombre (ex: `42`)
- Boolean : entrez `True` ou `False`
- String : entrez le texte entre guillemets simples (ex: `'Nouveau texte'`)
- Char : entrez un caractère entre guillemets simples (ex: `'A'`)
- Float : entrez un nombre décimal avec point (ex: `3.14`)

#### Tableaux et collections

Pour modifier un élément spécifique :
```pascal
MonTableau[2] := 100;  // Dans Evaluate/Modify
```

Pour une StringList :
```pascal
MaListe[1] := 'Nouvelle valeur';
```

#### Objets

Pour modifier une propriété d'objet :
```pascal
MaPersonne.Nom := 'Durand';
MaPersonne.Age := 35;
```

### Cas d'utilisation pratiques

La modification des variables pendant le débogage est particulièrement utile dans ces situations :

1. **Contourner temporairement un bug** : Si une condition échoue à cause d'une valeur incorrecte, vous pouvez la modifier pour voir si votre hypothèse de correction est valide.

2. **Tester différents scénarios** : Modifiez une valeur pour voir comment le programme se comporterait dans une situation particulière sans avoir à redémarrer l'application.

3. **Éviter des étapes fastidieuses** : Par exemple, remplir un formulaire avec des données de test, vous pouvez simplement affecter des valeurs aux variables directement.

## Expressions avancées dans le débogueur

Le débogueur Delphi accepte des expressions complexes, pas seulement des noms de variables. Voici quelques exemples :

### Opérations mathématiques
```pascal
Total / Quantite  // Division
Prix * 1.2        // Multiplication
Montant + Taxes   // Addition
```

### Fonctions sur les chaînes
```pascal
Length(MaChaine)          // Longueur d'une chaîne
Pos('recherche', Texte)   // Position d'une sous-chaîne
```

### Navigation dans les objets
```pascal
Form1.Button1.Caption      // Propriété d'un contrôle visuel
Liste.Items[3].SubItems[2] // Élément d'une liste à plusieurs niveaux
```

## Astuces pour une inspection efficace

### 1. Format d'affichage personnalisé

Vous pouvez modifier le format d'affichage des valeurs en ajoutant des modificateurs dans la fenêtre Watches :

```pascal
MonEntier,h    // Affiche en hexadécimal
MonByte,b      // Affiche en binaire
MonFloat,e     // Notation scientifique
```

Formats disponibles :
- `,d` : décimal (par défaut pour les entiers)
- `,h` ou `,x` : hexadécimal
- `,b` : binaire
- `,c` : caractère
- `,e` : notation scientifique (pour les flottants)
- `,f` : notation à virgule fixe
- `,m` : monétaire (selon les paramètres régionaux)

### 2. Inspection des pointeurs

Pour les variables de type pointeur, vous pouvez afficher la valeur pointée en utilisant le déréférencement :

```pascal
MonPointeur^   // Valeur pointée
```

### 3. Utilisation de typecasts dans le débogueur

Il est parfois utile de convertir temporairement une variable pour l'inspecter différemment :

```pascal
TObject(MaVariable).ClassName   // Obtenir le nom de la classe
```

### 4. Organiser vos surveillances

Dans la fenêtre Watches, vous pouvez organiser vos variables en groupes :

1. Cliquez avec le bouton droit et sélectionnez **Add Group...** (Ajouter un groupe)
2. Nommez votre groupe (ex: "Variables UI", "Données métier", etc.)
3. Ajoutez des variables dans ce groupe pour une meilleure organisation

## Exemple pratique : Débogage d'une fonction de calcul

Considérons une fonction qui calcule la moyenne d'une liste de nombres :

```pascal
function CalculerMoyenne(const Valeurs: array of Double): Double;
var
  i: Integer;
  Total: Double;
begin
  Total := 0;
  for i := Low(Valeurs) to High(Valeurs) do
  begin
    Total := Total + Valeurs[i];
  end;

  if Length(Valeurs) > 0 then
    Result := Total / Length(Valeurs)
  else
    Result := 0;
end;
```

Étapes de débogage :

1. Placez un point d'arrêt sur la ligne `Total := 0;`
2. Exécutez la fonction avec des valeurs de test
3. Lorsque le point d'arrêt est atteint, ajoutez ces surveillances :
   - `Valeurs` (pour voir le tableau d'entrée)
   - `Length(Valeurs)` (pour vérifier la taille)
   - `Total` (pour suivre l'accumulation)
   - `i` (pour suivre l'itération)

4. Utilisez F8 pour avancer pas à pas et observer comment les valeurs changent
5. Si nécessaire, modifiez `Total` ou `Valeurs[i]` pour tester différents scénarios

## Déboguer les exceptions et cas limites

L'inspection des variables est particulièrement utile pour comprendre les exceptions :

1. Configurez Delphi pour s'arrêter sur les exceptions (menu **Tools > Debugger Options** > onglet **Language Exceptions**)
2. Lorsqu'une exception se produit, le débogueur s'arrête automatiquement
3. Inspectez les variables pour comprendre ce qui a causé l'exception
4. Vous pouvez modifier les variables pour contourner l'exception et poursuivre l'exécution

## Conclusion

L'inspection et la modification des variables pendant le débogage sont des compétences essentielles qui vous permettront de comprendre rapidement ce qui se passe dans votre application et de tester facilement différentes hypothèses sans avoir à modifier et recompiler votre code à chaque fois.

En maîtrisant ces techniques, vous gagnerez un temps précieux lors de la résolution de problèmes complexes et développerez une meilleure compréhension du comportement de votre code à l'exécution.

Dans la prochaine section, nous aborderons les tests unitaires avec DUnit/DUnitX, qui constituent une approche plus structurée pour vérifier le bon fonctionnement de votre code.

⏭️ [Test unitaire avec DUnit/DUnitX](12-debogage-et-tests/03-test-unitaire-avec-dunit-dunitx.md)
