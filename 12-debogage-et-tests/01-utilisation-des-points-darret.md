# 12.1 Utilisation des points d'arrêt

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux points d'arrêt

Le débogage est une compétence essentielle pour tout développeur, et les points d'arrêt (breakpoints) constituent l'un des outils les plus puissants dans cette boîte à outils. Dans Delphi, les points d'arrêt vous permettent de suspendre l'exécution de votre application à des endroits précis pour examiner l'état du programme.

## Qu'est-ce qu'un point d'arrêt ?

Un point d'arrêt est un marqueur que vous placez dans votre code pour indiquer à Delphi de suspendre l'exécution lorsque le programme atteint cette ligne. Lorsque l'exécution est suspendue, vous pouvez :

- Examiner les valeurs des variables
- Vérifier l'état des objets
- Suivre l'exécution ligne par ligne
- Comprendre le flux d'exécution de votre application

## Placement d'un point d'arrêt

Pour placer un point d'arrêt dans Delphi :

1. Ouvrez l'éditeur de code et localisez la ligne où vous souhaitez suspendre l'exécution
2. Cliquez dans la marge gauche (la gouttière) à côté du numéro de ligne
   - OU appuyez sur F5 lorsque le curseur est positionné sur la ligne concernée
   - OU faites un clic droit sur la ligne et sélectionnez "Ajouter point d'arrêt"

Un point rouge apparaîtra dans la marge, indiquant qu'un point d'arrêt a été défini sur cette ligne :

![Point d'arrêt dans l'éditeur Delphi](https://via.placeholder.com/500x100)

## Types de points d'arrêt

Delphi propose plusieurs types de points d'arrêt :

### 1. Points d'arrêt simples
Un point d'arrêt standard qui arrête l'exécution à chaque fois.

### 2. Points d'arrêt conditionnels
Arrête l'exécution seulement si une condition est remplie. Pour définir un point d'arrêt conditionnel :

1. Faites un clic droit sur un point d'arrêt existant
2. Sélectionnez "Point d'arrêt > Propriétés..." ou "Éditer le point d'arrêt..."
3. Dans la fenêtre qui apparaît, entrez une expression conditionnelle, par exemple :
   ```pascal
   i > 10
   ```

Cette condition signifie que l'exécution s'arrêtera seulement lorsque la variable `i` sera supérieure à 10.

### 3. Points d'arrêt avec compteur
Permet d'arrêter l'exécution après un certain nombre de passages sur le point d'arrêt. Ceci est utile pour déboguer des boucles ou des fonctions récursives.

1. Accédez aux propriétés du point d'arrêt
2. Définissez le champ "Passer le compte" à la valeur souhaitée (par exemple, 5)

### 4. Points d'arrêt avec actions
Au lieu d'arrêter l'exécution, vous pouvez configurer un point d'arrêt pour exécuter une action spécifique :

- Afficher un message
- Évaluer une expression
- Journaliser une information
- Continuer l'exécution sans interruption

## Gestion des points d'arrêt

### Fenêtre des points d'arrêt

Delphi offre une vue complète de tous vos points d'arrêt via le menu "View > Debug Windows > Breakpoints" (Vue > Fenêtres de débogage > Points d'arrêt). Cette fenêtre vous permet de :

- Voir tous les points d'arrêt définis
- Activer/désactiver des points d'arrêt individuellement
- Modifier les propriétés des points d'arrêt
- Supprimer des points d'arrêt
- Créer de nouveaux points d'arrêt (y compris sur des adresses mémoire)

![Fenêtre des points d'arrêt](https://via.placeholder.com/500x200)

### Activer/désactiver les points d'arrêt

Si vous souhaitez conserver un point d'arrêt mais le désactiver temporairement :

- Cliquez avec le bouton droit sur le point d'arrêt et sélectionnez "Désactiver"
- Dans la fenêtre des points d'arrêt, décochez la case correspondant au point d'arrêt
- Utilisez le raccourci Ctrl+Alt+F5 pour activer/désactiver tous les points d'arrêt

Un point d'arrêt désactivé apparaît en gris au lieu de rouge.

### Supprimer les points d'arrêt

Pour supprimer un point d'arrêt :

- Cliquez à nouveau sur le point rouge dans la marge
- Appuyez sur F5 lorsque le curseur est sur une ligne avec un point d'arrêt
- Cliquez avec le bouton droit sur le point d'arrêt et sélectionnez "Supprimer"
- Dans la fenêtre des points d'arrêt, sélectionnez le point d'arrêt et appuyez sur Delete

Pour supprimer tous les points d'arrêt en une seule fois :
- Menu "Run > Remove All Breakpoints" (Exécuter > Supprimer tous les points d'arrêt)
- Raccourci Ctrl+Shift+F5

## Utilisation des points d'arrêt en pratique

### Exemple pratique

Considérons une fonction simple qui calcule la factorielle d'un nombre :

```pascal
function Factorielle(n: Integer): Integer;
var
  i, resultat: Integer;
begin
  resultat := 1;
  for i := 1 to n do
  begin
    resultat := resultat * i;
  end;
  Result := resultat;
end;
```

Pour déboguer cette fonction :

1. Placez un point d'arrêt sur la ligne `resultat := 1;`
2. Exécutez l'application en mode débogage (F9)
3. Lorsque l'exécution s'arrête au point d'arrêt, utilisez F8 pour exécuter le code ligne par ligne
4. Observez les valeurs de `i` et `resultat` pendant l'exécution

### Conseils pour une utilisation efficace

- **Placez les points d'arrêt stratégiquement** : Identifiez les endroits où des problèmes pourraient survenir, comme après des initialisations ou avant des opérations complexes.
- **Utilisez des points d'arrêt conditionnels** pour les boucles longues ou pour cibler des cas spécifiques.
- **Combinez avec d'autres outils de débogage** comme les watches (surveillance), l'évaluation et modification, et le pas à pas.
- **N'oubliez pas de supprimer ou désactiver les points d'arrêt** avant de distribuer votre application.

## Raccourcis clavier utiles pour le débogage

| Action | Raccourci |
|--------|-----------|
| Définir/supprimer un point d'arrêt | F5 |
| Exécuter jusqu'au curseur | F4 |
| Pas à pas (entrer dans les fonctions) | F7 |
| Pas à pas (ignorer les fonctions) | F8 |
| Continuer l'exécution | F9 |
| Évaluer/modifier | Ctrl+F7 |
| Ajouter une variable à surveiller | Ctrl+F5 |
| Activer/désactiver tous les points d'arrêt | Ctrl+Alt+F5 |
| Supprimer tous les points d'arrêt | Ctrl+Shift+F5 |

## Points d'arrêt avancés

### Points d'arrêt sur les exceptions

Delphi peut également s'arrêter automatiquement lorsqu'une exception est levée :

1. Menu "Tools > Debugger Options" (Outils > Options du débogueur)
2. Sélectionnez l'onglet "Language Exceptions" (Exceptions de langage)
3. Configurez quelles exceptions doivent interrompre l'exécution

### Points d'arrêt sur accès aux données

Dans Delphi 12 Athens, vous pouvez définir des points d'arrêt qui se déclenchent lorsqu'une variable ou une adresse mémoire spécifique est lue ou modifiée.

> 💡 **Nécessite Delphi 12 ou supérieur**

1. Menu "Run > Add Watch at Address..." (Exécuter > Ajouter une surveillance à l'adresse...)
2. Spécifiez l'adresse ou la variable et le type d'accès à surveiller

## Conclusion

Les points d'arrêt sont un outil fondamental pour le débogage efficace dans Delphi. En maîtrisant leur utilisation, vous pourrez résoudre plus rapidement les problèmes dans votre code et améliorer votre productivité en tant que développeur. N'hésitez pas à expérimenter avec les différents types de points d'arrêt pour trouver ceux qui correspondent le mieux à votre style de développement et aux besoins de vos projets.

Dans la prochaine section, nous explorerons comment inspecter et modifier les variables pendant le débogage pour une analyse encore plus approfondie de votre code.

⏭️ [Inspection et modification des variables](/12-debogage-et-tests/02-inspection-et-modification-des-variables.md)
