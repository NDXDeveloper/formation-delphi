🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.1 Utilisation des points d'arrêt

## Introduction

Les points d'arrêt (ou **breakpoints** en anglais) sont l'un des outils les plus puissants et les plus utilisés lors du débogage d'une application Delphi. Ils permettent de suspendre temporairement l'exécution de votre programme à un endroit précis du code, afin d'examiner l'état de vos variables, de comprendre le flux d'exécution et d'identifier les erreurs.

Pour un débutant, maîtriser les points d'arrêt représente une étape cruciale dans l'apprentissage du développement, car cela permet de "voir" ce qui se passe réellement à l'intérieur de votre application.

## Qu'est-ce qu'un point d'arrêt ?

Un point d'arrêt est une **marque** que vous placez sur une ligne de code spécifique dans l'éditeur de Delphi. Lorsque votre programme s'exécute en mode débogage et atteint cette ligne, il se met automatiquement en pause, vous donnant ainsi l'opportunité d'inspecter l'état actuel de votre application.

Imaginez que vous lisez un livre et que vous placez un marque-page à une page importante pour y revenir plus tard. Un point d'arrêt fonctionne de manière similaire : il "marque" un endroit dans votre code où vous voulez faire une pause pour examiner les choses.

## Pourquoi utiliser les points d'arrêt ?

Les points d'arrêt sont essentiels pour plusieurs raisons :

**Comprendre le flux d'exécution** : Ils vous permettent de vérifier que votre code s'exécute dans l'ordre que vous attendez. Parfois, certaines conditions ou boucles ne se comportent pas comme prévu.

**Inspecter les valeurs des variables** : Lorsque le programme est en pause, vous pouvez examiner le contenu exact de vos variables à ce moment précis de l'exécution. C'est particulièrement utile pour détecter des valeurs incorrectes ou inattendues.

**Identifier les erreurs logiques** : Contrairement aux erreurs de compilation qui sont détectées automatiquement, les erreurs logiques (où le code fonctionne mais produit des résultats incorrects) nécessitent une analyse pas à pas du comportement du programme.

**Gagner du temps** : Au lieu de placer des dizaines de `ShowMessage()` pour afficher des valeurs, les points d'arrêt offrent une approche plus élégante et efficace pour comprendre ce qui se passe dans votre code.

## Comment créer un point d'arrêt

### Méthode simple : clic dans la marge

La façon la plus rapide et la plus courante de créer un point d'arrêt est de cliquer dans la **marge gauche** de l'éditeur de code, juste à côté du numéro de ligne où vous souhaitez arrêter l'exécution.

Lorsque vous cliquez, un **cercle rouge** apparaît dans la marge, indiquant qu'un point d'arrêt a été placé sur cette ligne. La ligne de code peut également être mise en surbrillance avec un fond coloré (généralement rouge ou rose pâle selon votre thème).

### Utilisation du clavier

Vous pouvez également utiliser le raccourci clavier **F5** lorsque votre curseur est positionné sur la ligne où vous souhaitez placer le point d'arrêt. Appuyer à nouveau sur F5 supprimera le point d'arrêt.

### Menu contextuel

Vous pouvez faire un clic droit sur une ligne de code et sélectionner **"Toggle Breakpoint"** (Activer/Désactiver le point d'arrêt) dans le menu contextuel.

## Exécuter le programme avec des points d'arrêt

Une fois vos points d'arrêt placés, vous devez exécuter votre programme **en mode débogage** pour qu'ils soient activés. Il existe plusieurs façons de le faire :

- Appuyez sur **F9** pour lancer l'exécution
- Cliquez sur le bouton **"Run"** (lecture verte) dans la barre d'outils
- Utilisez le menu **Run > Run** (Exécuter)

Lorsque l'exécution atteint un point d'arrêt, le programme se met en pause et l'éditeur de Delphi passe au premier plan. La ligne de code où le programme s'est arrêté est mise en évidence (généralement avec une flèche verte ou bleue dans la marge).

**Important** : À ce stade, le programme n'est pas terminé, il est simplement en pause. Vous pouvez maintenant inspecter vos variables, examiner la pile d'appels, et continuer l'exécution ligne par ligne ou jusqu'au prochain point d'arrêt.

## Gérer les points d'arrêt

### Désactiver temporairement un point d'arrêt

Parfois, vous ne voulez pas supprimer complètement un point d'arrêt, mais simplement le désactiver temporairement. Pour ce faire :

- Clic droit sur le point d'arrêt (le cercle rouge)
- Sélectionnez **"Disable Breakpoint"** (Désactiver le point d'arrêt)

Le cercle rouge devient alors **grisé** ou **hachuré**, indiquant que le point d'arrêt existe toujours mais n'arrêtera pas l'exécution. Vous pouvez le réactiver de la même manière en sélectionnant **"Enable Breakpoint"**.

### Supprimer un point d'arrêt

Pour supprimer définitivement un point d'arrêt :

- Cliquez à nouveau sur le cercle rouge dans la marge
- Ou appuyez sur **F5** avec le curseur sur la ligne concernée
- Ou faites un clic droit et sélectionnez **"Delete Breakpoint"**

### Voir tous les points d'arrêt

Delphi offre une vue dédiée pour gérer tous vos points d'arrêt en un seul endroit :

- Allez dans le menu **View > Debug Windows > Breakpoints** (Affichage > Fenêtres de débogage > Points d'arrêt)
- Ou utilisez le raccourci **Ctrl+Alt+B**

Cette fenêtre affiche la liste de tous les points d'arrêt actifs dans votre projet, avec des informations sur leur emplacement (fichier et numéro de ligne). Vous pouvez :

- Activer ou désactiver des points d'arrêt individuellement
- Supprimer des points d'arrêt
- Naviguer directement vers le code correspondant en double-cliquant sur un point d'arrêt

### Supprimer tous les points d'arrêt

Si vous avez placé de nombreux points d'arrêt et que vous souhaitez tous les supprimer d'un coup :

- Dans la fenêtre **Breakpoints**, cliquez sur le bouton **"Delete All"** (Supprimer tout)
- Ou utilisez le menu **Run > Remove All Breakpoints**

## Types de points d'arrêt

### Points d'arrêt simples

C'est le type le plus courant que nous avons décrit jusqu'ici. Le programme s'arrête automatiquement lorsqu'il atteint la ligne de code marquée.

### Points d'arrêt conditionnels

Un point d'arrêt conditionnel ne suspend l'exécution que si une **condition spécifique** est remplie. Cela est extrêmement utile lorsque vous voulez examiner le code uniquement dans certaines situations.

Par exemple, si vous avez une boucle qui s'exécute 1000 fois, mais que vous voulez seulement vous arrêter quand une variable `i` est égale à 500 :

**Pour créer un point d'arrêt conditionnel :**

1. Placez d'abord un point d'arrêt normal sur la ligne souhaitée
2. Faites un clic droit sur le cercle rouge du point d'arrêt
3. Sélectionnez **"Breakpoint Properties"** (Propriétés du point d'arrêt)
4. Dans la fenêtre qui s'ouvre, cochez **"Condition"**
5. Entrez votre condition, par exemple : `i = 500`
6. Cliquez sur **OK**

Maintenant, le point d'arrêt ne se déclenchera que lorsque la condition sera vraie.

### Points d'arrêt avec compteur de passages

Ce type de point d'arrêt permet de s'arrêter uniquement après un certain nombre de passages sur la ligne. Par exemple, vous pouvez configurer un point d'arrêt pour qu'il se déclenche uniquement à la 10ème itération d'une boucle.

**Pour créer un point d'arrêt avec compteur :**

1. Accédez aux propriétés du point d'arrêt (clic droit > Breakpoint Properties)
2. Activez l'option **"Pass Count"** (Nombre de passages)
3. Entrez le nombre de fois que la ligne doit être exécutée avant que le point d'arrêt ne se déclenche
4. Validez avec **OK**

## Points d'arrêt et performances

Il est important de noter que les points d'arrêt n'ont **aucun impact** sur les performances de votre application lorsqu'elle est compilée en mode **Release** (sans informations de débogage). Ils n'existent que dans les versions de développement et de débogage.

Vous pouvez donc placer autant de points d'arrêt que nécessaire pendant le développement sans vous inquiéter qu'ils ralentissent l'application finale livrée à vos utilisateurs.

## Continuer l'exécution après un point d'arrêt

Lorsque votre programme est en pause sur un point d'arrêt, vous avez plusieurs options pour continuer :

**Continuer l'exécution normale (F9)** : Le programme reprend son exécution normale jusqu'au prochain point d'arrêt ou jusqu'à la fin du programme.

**Exécuter la ligne actuelle (F8 - Step Over)** : Le programme exécute la ligne actuelle et s'arrête à la ligne suivante. Si la ligne actuelle contient un appel de fonction ou de procédure, celle-ci est exécutée entièrement et le programme s'arrête à la ligne suivante du code actuel.

**Entrer dans une fonction/procédure (F7 - Step Into)** : Si la ligne actuelle contient un appel de fonction ou de procédure, le débogueur "entre" dans cette fonction et s'arrête à sa première ligne. Cela permet d'examiner le code de la fonction en détail.

**Sortir de la fonction actuelle (Shift+F8 - Step Out)** : Le programme termine l'exécution de la fonction/procédure actuelle et s'arrête juste après l'appel dans le code appelant.

**Exécuter jusqu'au curseur (F4)** : Le programme s'exécute jusqu'à la ligne où se trouve votre curseur, comme si vous aviez placé un point d'arrêt temporaire à cet endroit.

## Conseils pratiques pour débutants

### Commencez simple

Ne placez pas trop de points d'arrêt dès le début. Commencez par un ou deux points d'arrêt aux endroits stratégiques, comme au début d'une fonction que vous déboguez, ou juste avant une ligne qui génère une erreur.

### Utilisez les points d'arrêt pour comprendre le code

Si vous lisez du code écrit par quelqu'un d'autre (ou même votre propre code après quelque temps), placer des points d'arrêt et exécuter le programme pas à pas est un excellent moyen de comprendre comment il fonctionne.

### Combinez avec l'inspection de variables

Les points d'arrêt sont encore plus puissants lorsqu'ils sont combinés avec l'inspection de variables. Lorsque le programme est en pause, survolez une variable avec votre souris pour voir sa valeur actuelle, ou utilisez la fenêtre **Watch List** pour surveiller plusieurs variables simultanément.

### N'oubliez pas de les retirer

Avant de compiler votre version finale, pensez à retirer ou désactiver les points d'arrêt inutiles. Bien qu'ils n'affectent pas la version Release, il est plus propre de ne garder que ceux qui sont vraiment nécessaires pendant le développement.

### Utilisez les points d'arrêt conditionnels pour les boucles

Si vous déboguez une boucle qui s'exécute de nombreuses fois, utilisez un point d'arrêt conditionnel pour ne vous arrêter que sur les itérations qui vous intéressent. Cela vous évitera de devoir appuyer sur F9 des dizaines ou des centaines de fois.

## Résolution de problèmes courants

### Le point d'arrêt ne se déclenche pas

Si un point d'arrêt ne se déclenche pas, vérifiez que :

- Vous exécutez bien le programme en mode **Debug** et non en mode Release
- Le code où se trouve le point d'arrêt est effectivement exécuté (il n'est pas dans une condition qui n'est jamais vraie, par exemple)
- Le point d'arrêt est bien **activé** (cercle rouge plein, pas grisé)
- Les options de compilation incluent les informations de débogage (vérifiez dans **Project > Options > Delphi Compiler > Compiling > Debug information** qui doit être activé)

### Le point d'arrêt est représenté par un cercle avec un point d'interrogation

Cela signifie généralement que Delphi ne peut pas associer le point d'arrêt à une ligne de code exécutable. Cela arrive souvent si :

- Le point d'arrêt est placé sur une ligne de commentaire ou une ligne vide
- Le point d'arrêt est sur une déclaration de variable ou de type
- Le code n'a pas été recompilé depuis la modification

**Solution** : Recompilez votre projet (Shift+F9) ou déplacez le point d'arrêt sur une ligne de code exécutable (une instruction).

### L'exécution semble sauter des lignes

Cela peut arriver en raison de l'optimisation du compilateur. En mode Debug, l'optimisation devrait être désactivée par défaut, mais si ce n'est pas le cas :

- Allez dans **Project > Options > Delphi Compiler > Compiling**
- Assurez-vous que **Optimization** est décoché
- Vérifiez que **Debug information** est coché

## Conclusion

Les points d'arrêt sont un outil fondamental pour tout développeur Delphi. Ils transforment le débogage d'une tâche ardue en un processus structuré et efficace. En maîtrisant leur utilisation, vous gagnerez un temps considérable et développerez une meilleure compréhension de la façon dont votre code s'exécute réellement.

N'hésitez pas à expérimenter avec les différents types de points d'arrêt et à les utiliser régulièrement dans votre travail quotidien. Avec la pratique, leur utilisation deviendra une seconde nature, et vous vous demanderez comment vous avez pu développer sans eux auparavant.

⏭️ [Inspection et modification des variables](/12-debogage-et-tests/02-inspection-et-modification-des-variables.md)
