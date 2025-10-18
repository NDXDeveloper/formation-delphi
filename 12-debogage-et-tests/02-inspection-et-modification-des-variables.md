🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.2 Inspection et modification des variables

## Introduction

Une fois que vous avez appris à utiliser les points d'arrêt pour suspendre l'exécution de votre programme, l'étape suivante naturelle consiste à examiner ce qui se passe réellement dans votre code à ce moment précis. C'est là qu'intervient l'inspection des variables.

L'inspection des variables vous permet de "regarder à l'intérieur" de votre programme pendant son exécution pour voir les valeurs actuelles de vos variables, comprendre l'état de vos objets, et même modifier ces valeurs à la volée pour tester différents scénarios sans avoir à recompiler votre code.

Pour un débutant, cette capacité est révolutionnaire : au lieu de deviner pourquoi votre programme ne fonctionne pas comme prévu, vous pouvez **voir exactement** ce qui se passe.

## Pourquoi inspecter et modifier les variables ?

### Comprendre l'état du programme

Lorsque votre programme est en pause sur un point d'arrêt, il est dans un état précis à un moment donné. Chaque variable contient une valeur spécifique. En inspectant ces valeurs, vous pouvez :

- Vérifier que vos calculs produisent les résultats attendus
- Identifier des valeurs incorrectes ou inattendues
- Comprendre pourquoi une condition if ne se comporte pas comme prévu
- Voir le contenu d'objets complexes (listes, tableaux, objets personnalisés)

### Tester des hypothèses rapidement

Imaginons que vous pensez qu'un bug se produit parce qu'une variable a une valeur incorrecte. Plutôt que de modifier le code, recompiler et relancer le programme, vous pouvez simplement modifier la valeur de la variable pendant le débogage et continuer l'exécution pour voir si cela résout le problème. C'est un gain de temps considérable.

### Apprendre comment fonctionne le code

L'inspection de variables est également un excellent outil d'apprentissage. En observant comment les valeurs changent au fur et à mesure de l'exécution du code, vous développez une meilleure intuition du comportement de vos programmes.

## Méthodes d'inspection des variables

Delphi offre plusieurs méthodes pour inspecter les variables pendant le débogage. Chacune a ses avantages selon la situation.

### 1. Survol avec la souris (Tooltip Evaluation)

C'est la méthode la plus simple et la plus rapide pour vérifier rapidement la valeur d'une variable.

**Comment l'utiliser :**

1. Mettez votre programme en pause sur un point d'arrêt
2. Placez simplement votre curseur de souris sur le nom d'une variable dans le code
3. Après un bref instant, une **info-bulle** apparaît affichant la valeur actuelle de la variable

**Avantages :**
- Extrêmement rapide et intuitif
- Parfait pour des vérifications ponctuelles
- Ne nécessite aucune configuration

**Limitations :**
- L'info-bulle disparaît lorsque vous déplacez la souris
- Difficile de comparer plusieurs variables en même temps
- Ne fonctionne que pour les variables simples (pas pour des expressions complexes)

**Astuce pour débutant :** Cette méthode est idéale quand vous voulez juste vérifier rapidement "Est-ce que ma variable contient bien la valeur que je pense ?"

### 2. La fenêtre Evaluateur/Modificateur (Evaluate/Modify)

Cette fenêtre vous permet d'évaluer n'importe quelle expression et même de modifier des valeurs de variables.

**Comment l'ouvrir :**

- Utilisez le raccourci **Ctrl+F7** pendant que le programme est en pause
- Ou allez dans **Run > Evaluate/Modify**
- Ou faites un clic droit sur une variable et sélectionnez **Evaluate/Modify**

**Utilisation :**

La fenêtre comporte deux zones principales :

**Expression :** C'est ici que vous tapez le nom de la variable ou l'expression que vous voulez évaluer. Par exemple :
- Un nom de variable simple : `MonCompteur`
- Une propriété d'objet : `MaListe.Count`
- Une expression : `Nombre * 2 + 10`
- Un élément de tableau : `MonTableau[5]`

**Result (Résultat) :** Après avoir cliqué sur **Evaluate** (Évaluer), le résultat s'affiche ici.

**Pour modifier une variable :**

1. Entrez le nom de la variable dans le champ **Expression**
2. Cliquez sur **Evaluate** pour voir sa valeur actuelle
3. Modifiez la valeur dans le champ **Result**
4. Cliquez sur **Modify** (Modifier)

La variable prendra immédiatement la nouvelle valeur dans votre programme en cours d'exécution.

**Exemple concret :**

Imaginez que vous avez une variable `Age` qui vaut 15, et vous voulez tester ce qui se passe si elle valait 25 :

1. Tapez `Age` dans Expression
2. Cliquez sur Evaluate → Résultat : 15
3. Changez 15 en 25 dans le champ Result
4. Cliquez sur Modify
5. Continuez l'exécution (F9) et observez le comportement avec la nouvelle valeur

### 3. La fenêtre Watch List (Liste de surveillance)

La **Watch List** est probablement l'outil le plus puissant pour surveiller plusieurs variables simultanément pendant le débogage.

**Comment l'ouvrir :**

- Menu **View > Debug Windows > Watch List**
- Ou raccourci **Ctrl+Alt+W**

**Ajouter des variables à surveiller :**

Il existe plusieurs façons d'ajouter une variable à la Watch List :

**Méthode 1 - Clic droit :**
1. Dans votre code, faites un clic droit sur le nom de la variable
2. Sélectionnez **Add Watch at Cursor** (Ajouter une surveillance au curseur)

**Méthode 2 - Depuis la fenêtre Watch List :**
1. Cliquez sur le bouton **Add Watch** dans la barre d'outils de la fenêtre Watch List
2. Tapez le nom de la variable ou l'expression
3. Cliquez sur OK

**Méthode 3 - Glisser-déposer :**
1. Sélectionnez le texte d'une variable dans l'éditeur
2. Faites-le glisser vers la fenêtre Watch List

**Ce que vous verrez :**

La fenêtre Watch List affiche un tableau avec plusieurs colonnes :

- **Expression :** Le nom de la variable ou l'expression surveillée
- **Value (Valeur) :** La valeur actuelle de la variable
- **Type :** Le type de données de la variable

**Avantages de la Watch List :**

- Vous pouvez surveiller **plusieurs variables en même temps**
- Les valeurs se mettent à jour automatiquement à chaque pas d'exécution
- Les variables qui changent de valeur sont souvent mises en surbrillance (en rouge) pour attirer votre attention
- Vous pouvez organiser vos variables en groupes
- Les watches sont conservées entre les sessions de débogage

**Modifier une variable depuis la Watch List :**

1. Double-cliquez sur la valeur de la variable dans la colonne Value
2. Tapez la nouvelle valeur
3. Appuyez sur Entrée

**Supprimer une watch :**

- Sélectionnez la ligne et appuyez sur **Delete**
- Ou clic droit > **Delete Watch**

### 4. La fenêtre Local Variables (Variables locales)

Cette fenêtre affiche automatiquement toutes les variables locales de la fonction ou procédure actuelle, sans que vous ayez besoin de les ajouter manuellement.

**Comment l'ouvrir :**

- Menu **View > Debug Windows > Local Variables**
- Ou raccourci **Ctrl+Alt+L**

**Ce qu'elle affiche :**

Lorsque vous êtes en pause sur un point d'arrêt, cette fenêtre montre automatiquement :

- Tous les paramètres de la fonction/procédure actuelle
- Toutes les variables locales déclarées dans cette fonction/procédure
- La valeur actuelle de chaque variable

**Quand l'utiliser :**

Cette fenêtre est particulièrement utile quand :
- Vous déboguez une fonction complexe avec de nombreuses variables
- Vous voulez avoir une vue d'ensemble de toutes les variables sans avoir à les ajouter manuellement à la Watch List
- Vous explorez du code que vous ne connaissez pas bien

**Limitation :**

Elle ne montre que les variables **locales** de la fonction actuelle. Pour voir des variables globales ou des propriétés d'objets, utilisez plutôt la Watch List.

### 5. Inspecter les objets complexes

Delphi permet d'inspecter non seulement les variables simples (entiers, chaînes), mais aussi les objets complexes avec toutes leurs propriétés.

**Expansion des objets :**

Lorsque vous inspectez un objet (que ce soit via le survol, la Watch List ou l'Evaluator), vous verrez souvent un **petit triangle** ou **signe plus (+)** à côté du nom de l'objet. Cliquez dessus pour **développer** l'objet et voir toutes ses propriétés internes.

**Exemple :**

Si vous avez un objet `MaListe` de type `TStringList`, en le développant vous verrez :
- `Count` : le nombre d'éléments
- `Capacity` : la capacité de la liste
- `Strings[0]`, `Strings[1]`, etc. : les éléments individuels

**Navigation en profondeur :**

Vous pouvez continuer à développer les propriétés qui sont elles-mêmes des objets, créant ainsi une arborescence de l'état complet de votre objet.

## Inspecter différents types de données

### Variables simples

Les types de base (Integer, String, Boolean, Real, etc.) s'affichent directement avec leur valeur.

```pascal
var
  MonAge: Integer;
  MonNom: String;
  EstActif: Boolean;
```

Inspection :
- `MonAge` → affiche : 25
- `MonNom` → affiche : 'Jean Dupont'
- `EstActif` → affiche : True

### Tableaux

Pour les tableaux, vous pouvez inspecter :
- Le tableau complet
- Des éléments individuels en utilisant l'indice

```pascal
var
  Nombres: array[0..4] of Integer;
```

Inspection :
- `Nombres` → affiche l'objet tableau (développable)
- `Nombres[0]` → affiche la valeur du premier élément
- `Nombres[2]` → affiche la valeur du troisième élément

### Chaînes de caractères

Les chaînes s'affichent avec leur contenu entre guillemets simples. Vous pouvez aussi voir leur longueur.

```pascal
var
  Message: String;
```

Si `Message` contient 'Bonjour', vous verrez : `'Bonjour'`

Pour connaître la longueur, ajoutez à la Watch List : `Length(Message)`

### Pointeurs

Les pointeurs affichent leur adresse mémoire (par exemple : `$00405A20`).

Pour voir la valeur pointée, vous devez **déréférencer** le pointeur en utilisant l'opérateur `^` :

```pascal
var
  P: ^Integer;
```

Inspection :
- `P` → affiche l'adresse mémoire
- `P^` → affiche la valeur pointée

### Énumérations

Les types énumérés affichent le nom de la valeur courante.

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi);
var
  JourActuel: TJour;
```

Si `JourActuel` vaut `Mercredi`, vous verrez : `Mercredi`

### Records (Enregistrements)

Les records peuvent être développés pour voir chacun de leurs champs.

```pascal
type
  TPersonne = record
    Nom: String;
    Age: Integer;
  end;
var
  Utilisateur: TPersonne;
```

En développant `Utilisateur`, vous verrez :
- `Nom` : 'Martin'
- `Age` : 30

### Objets et classes

Les objets sont développables et montrent toutes leurs propriétés publiques et publiées, ainsi que leurs champs.

**Astuce :** Pour voir les champs privés et protégés, vous devrez peut-être utiliser des techniques avancées ou les rendre publics temporairement pendant le débogage.

## Modification des variables pendant le débogage

La capacité de modifier des variables pendant l'exécution est un outil puissant pour tester différents scénarios sans recompiler votre code.

### Quand modifier des variables ?

Voici quelques situations courantes où modifier une variable est utile :

**Tester une correction :** Vous avez identifié qu'une variable a une valeur incorrecte. Au lieu de modifier le code, recompiler et relancer, vous changez la valeur et continuez pour voir si cela corrige le bug.

**Tester des conditions edge cases :** Vous voulez voir ce qui se passe si une variable a une valeur extrême (0, une valeur très grande, une valeur négative) sans avoir à créer tout un scénario pour arriver à cette situation.

**Simuler des conditions difficiles à reproduire :** Par exemple, simuler une erreur de connexion en mettant une variable de statut à False.

**Sauter une partie du code :** En modifiant une variable de condition, vous pouvez forcer votre programme à prendre un chemin différent dans une structure if/then/else.

### Comment modifier efficacement

**Méthode recommandée :**

1. **Arrêtez-vous juste avant** la ligne qui utilise la variable que vous voulez modifier
2. Modifiez la valeur via l'Evaluator, la Watch List, ou la fenêtre Local Variables
3. Exécutez la ligne suivante (F8) pour voir l'effet de votre modification
4. Observez le comportement

**Exemple pratique :**

```pascal
procedure VerifierAge;
var
  Age: Integer;
begin
  Age := 15;  // ← Point d'arrêt ici

  if Age >= 18 then
    ShowMessage('Majeur')
  else
    ShowMessage('Mineur');
end;
```

1. Le programme s'arrête après l'affectation de `Age := 15`
2. Vous modifiez `Age` pour qu'il vaille 20
3. Vous continuez l'exécution
4. Le programme affichera "Majeur" au lieu de "Mineur"

### Limitations et précautions

**Ne modifiez pas n'importe quoi :**

Certaines modifications peuvent rendre votre programme instable pendant cette session de débogage :

- Modifier des pointeurs vers des adresses invalides peut causer un crash
- Changer la taille d'une structure de données peut créer des incohérences
- Modifier des variables système peut avoir des effets imprévisibles

**Les modifications sont temporaires :**

Rappelez-vous que toute modification effectuée pendant le débogage est **temporaire** et **ne modifie pas votre code source**. Lorsque vous arrêterez et relancerez le programme, il utilisera à nouveau les valeurs d'origine.

Si une modification résout votre problème, vous devrez ensuite modifier votre code source en conséquence.

**Cohérence des données :**

Faites attention à ne pas créer des états incohérents. Par exemple, si vous avez une liste avec 5 éléments et que vous modifiez manuellement la propriété `Count` à 10, votre programme pourrait crasher en essayant d'accéder aux 5 éléments inexistants.

## Expressions avancées

Les outils d'inspection de Delphi ne se limitent pas aux noms de variables simples. Vous pouvez évaluer des **expressions complexes**.

### Expressions arithmétiques

Vous pouvez effectuer des calculs directement dans l'Evaluator ou la Watch List :

- `Nombre1 + Nombre2`
- `Prix * Quantite * 1.20` (calcul avec TVA)
- `Longueur * Largeur` (aire d'un rectangle)

### Appels de fonctions

Vous pouvez appeler des fonctions intégrées de Delphi :

- `Length(MaChaine)` : longueur d'une chaîne
- `UpperCase(Nom)` : convertir en majuscules
- `IntToStr(Nombre)` : convertir un entier en chaîne
- `Now` : date et heure actuelles

### Conditions et comparaisons

Vous pouvez même évaluer des expressions booléennes :

- `Age >= 18`
- `Nom = 'Martin'`
- `(Nombre > 0) and (Nombre < 100)`

Ces expressions retournent True ou False et sont particulièrement utiles pour comprendre pourquoi une condition if se comporte d'une certaine façon.

### Propriétés et méthodes d'objets

Pour les objets, vous pouvez accéder à leurs propriétés et même appeler certaines méthodes (à utiliser avec précaution) :

- `MaListe.Count`
- `Bouton1.Caption`
- `Edit1.Text`

## La fenêtre Call Stack (Pile d'appels)

Bien que ce ne soit pas strictement une fenêtre d'inspection de variables, la **Call Stack** est étroitement liée car elle vous aide à comprendre le contexte dans lequel vos variables existent.

**Comment l'ouvrir :**

- Menu **View > Debug Windows > Call Stack**
- Ou raccourci **Ctrl+Alt+S**

**Ce qu'elle montre :**

La Call Stack affiche la **séquence d'appels de fonctions** qui a mené à l'endroit où votre programme est actuellement en pause.

**Exemple :**

```
FonctionC (ligne 45) ← Vous êtes ici
FonctionB (ligne 30) ← Appelée par FonctionB
FonctionA (ligne 15) ← Appelée par FonctionA
FormCreate (ligne 8) ← Point d'entrée
```

**Utilité pour l'inspection :**

En double-cliquant sur une ligne de la Call Stack, vous pouvez "remonter" dans l'historique des appels et inspecter les variables locales de chaque fonction appelante. Cela vous aide à comprendre d'où viennent les valeurs passées en paramètres.

## Conseils pratiques pour débutants

### Commencez avec le survol de souris

Pour vos premiers déboguages, utilisez simplement le survol de souris. C'est la méthode la plus simple et souvent suffisante pour des vérifications rapides.

### Progressez vers la Watch List

Une fois à l'aise avec le survol, apprenez à utiliser la Watch List pour surveiller plusieurs variables en même temps. C'est particulièrement utile dans les boucles où vous voulez voir comment plusieurs variables évoluent.

### Utilisez des noms de variables descriptifs

Si vos variables s'appellent `i`, `j`, `x`, `temp`, il sera difficile de vous y retrouver dans les fenêtres d'inspection. Des noms clairs comme `CompteurClient`, `TotalHT`, `EstValide` rendent le débogage beaucoup plus facile.

### Créez des Watch List organisées

Vous pouvez organiser votre Watch List par groupes thématiques en utilisant des commentaires ou des espaces vides entre les groupes de variables liées.

### Méfiez-vous des effets de bord

Lorsque vous évaluez une expression qui appelle une fonction, cette fonction s'exécute réellement. Si cette fonction a des effets de bord (modifie l'état du programme, écrit dans un fichier, etc.), ces effets auront lieu. Soyez donc prudent avec les expressions complexes.

### Notez vos observations

Pendant le débogage, prenez des notes sur ce que vous observez. Par exemple : "La variable CompteurClient vaut 0 au lieu de 5 après la boucle". Ces notes vous aideront à comprendre et corriger le problème.

### Ne restez pas bloqué

Si vous ne comprenez pas pourquoi une variable a une certaine valeur, remontez dans le Call Stack pour voir d'où vient cette valeur, ou placez des points d'arrêt plus tôt dans le code pour observer l'évolution de la variable.

## Astuces avancées pour gagner du temps

### Inspecteur rapide (Quick Evaluate)

Vous pouvez sélectionner n'importe quelle expression dans votre code, faire un clic droit et choisir **Evaluate/Modify** pour l'évaluer immédiatement sans avoir à la retaper.

### Copier des valeurs

Dans la Watch List ou l'Evaluator, vous pouvez copier (Ctrl+C) la valeur d'une variable pour la coller ailleurs (dans un document de notes, un tableur, etc.). Cela est utile pour comparer des valeurs entre différentes exécutions.

### Formater l'affichage

Dans certains cas, vous pouvez influencer la façon dont une valeur est affichée. Par exemple, pour voir un nombre en hexadécimal, vous pouvez parfois utiliser des suffixes comme `,h` après le nom de la variable (cela dépend des versions de Delphi).

### Utiliser des expressions à valeurs booléennes

Au lieu d'ajouter `MaVariable` à la Watch List, ajoutez `MaVariable > 100`. Vous verrez directement True ou False, ce qui est parfois plus parlant qu'une valeur numérique.

## Résolution de problèmes courants

### "Identifier not found" ou "Identificateur non trouvé"

Ce message apparaît quand Delphi ne peut pas trouver la variable que vous essayez d'inspecter. Causes possibles :

- La variable n'est pas dans la portée actuelle (scope)
- Vous avez mal orthographié le nom de la variable
- Les informations de débogage n'ont pas été compilées dans le programme

**Solution :** Vérifiez l'orthographe, assurez-vous d'être dans la bonne fonction, et recompilez en mode Debug.

### La valeur affichée semble incorrecte

Si vous voyez une valeur étrange ou inattendue :

- Vérifiez le type de la variable (peut-être interprétez-vous mal le type)
- Assurez-vous que la variable a bien été initialisée
- Si c'est un pointeur, vérifiez qu'il pointe vers une adresse valide
- Les variables non initialisées peuvent contenir des valeurs aléatoires ("garbage")

### Impossible de modifier une variable

Certaines variables ne peuvent pas être modifiées :

- Les constantes (par définition)
- Les expressions calculées (vous ne pouvez pas modifier le résultat de `X + Y`, mais vous pouvez modifier `X` et `Y` séparément)
- Certaines propriétés en lecture seule

### "Expression too complex" ou "Expression trop complexe"

L'Evaluator a des limites sur la complexité des expressions qu'il peut évaluer. Si vous obtenez cette erreur, simplifiez votre expression en la décomposant en parties plus petites.

## Intégration avec les points d'arrêt

L'inspection de variables devient vraiment puissante lorsqu'elle est combinée avec les points d'arrêt :

### Points d'arrêt avec condition sur variable

Vous pouvez créer un point d'arrêt qui ne se déclenche que quand une variable atteint une valeur spécifique. Par exemple : "arrête-toi quand `Compteur = 100`".

### Watch Points

Certaines versions de Delphi permettent de créer des **Watch Points** : des points d'arrêt qui se déclenchent automatiquement quand une variable spécifique **change de valeur**. C'est extrêmement utile pour détecter des modifications inattendues de variables.

## Conclusion

L'inspection et la modification des variables sont des compétences essentielles pour tout développeur Delphi. Ces outils transforment le débogage d'un processus mystérieux en une investigation méthodique et logique.

Voici les points clés à retenir :

- **Commencez simple** avec le survol de souris pour les vérifications rapides
- **Utilisez la Watch List** pour surveiller plusieurs variables simultanément
- **L'Evaluator/Modifier** vous permet de tester des hypothèses rapidement
- **La fenêtre Local Variables** offre une vue d'ensemble automatique
- **La modification temporaire** de variables peut vous faire gagner beaucoup de temps
- **Les expressions complexes** vous permettent d'aller au-delà des simples noms de variables

Avec la pratique, l'inspection de variables deviendra un réflexe naturel. Vous développerez une intuition pour savoir quelles variables surveiller et quand le faire. Cette compétence vous accompagnera tout au long de votre carrière de développeur et vous permettra de résoudre des bugs qui sembleraient autrement impossibles à diagnostiquer.

N'hésitez pas à expérimenter avec ces outils dans un environnement sans risque (un petit programme de test) pour vous familiariser avec toutes leurs possibilités avant de les utiliser sur des projets plus complexes.

⏭️ [Test unitaire avec DUnit/DUnitX](/12-debogage-et-tests/03-test-unitaire-avec-dunit-dunitx.md)
