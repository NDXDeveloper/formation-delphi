🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.5 Compilation et exécution

## Introduction

Vous avez créé votre projet, ajouté des composants, écrit du code... Il est maintenant temps de transformer tout cela en une véritable application exécutable ! C'est là qu'intervient le processus de compilation.

La compilation peut sembler mystérieuse au début, mais c'est en réalité un processus logique et bien défini. Dans cette section, nous allons démystifier la compilation, comprendre ce qui se passe réellement quand vous appuyez sur F9, et apprendre à gérer les erreurs qui peuvent survenir.

## Qu'est-ce que la compilation ?

### Du code source au programme exécutable

Quand vous écrivez du code en Object Pascal, vous créez ce qu'on appelle du "code source" : un texte que vous pouvez lire et comprendre (plus ou moins facilement). Mais votre ordinateur ne peut pas exécuter directement ce code source. Il a besoin d'instructions en langage machine, un langage binaire que seul le processeur comprend.

La **compilation** est le processus qui transforme votre code source lisible par l'humain en code machine exécutable par l'ordinateur. C'est comme traduire un livre du français vers une langue que votre ordinateur comprend.

### Le rôle du compilateur

Le compilateur Delphi est un programme très sophistiqué qui :

1. **Lit votre code source** : il parcourt tous vos fichiers .pas
2. **Vérifie la syntaxe** : il s'assure que vous avez respecté les règles du langage Object Pascal
3. **Analyse le sens** : il vérifie que votre code a du sens (les types sont cohérents, les variables sont déclarées, etc.)
4. **Optimise** : il réorganise et améliore votre code pour qu'il s'exécute plus rapidement
5. **Génère le code machine** : il traduit votre code en instructions processeur
6. **Lie les bibliothèques** : il assemble votre code avec les bibliothèques nécessaires (VCL, RTL, etc.)
7. **Crée l'exécutable** : il génère le fichier .exe final

Tout cela se passe en quelques secondes, parfois moins !

### La différence avec l'interprétation

Certains langages (comme Python ou JavaScript) sont "interprétés" : le code est traduit ligne par ligne pendant l'exécution. Delphi, lui, est "compilé" : tout est traduit avant l'exécution.

L'avantage de la compilation :
- **Vitesse d'exécution** : votre programme tourne beaucoup plus vite
- **Détection d'erreurs** : beaucoup d'erreurs sont détectées avant même de lancer le programme
- **Distribution** : vous donnez un fichier .exe, pas le code source

L'inconvénient :
- **Temps de compilation** : vous devez attendre la compilation avant de tester

Mais avec Delphi, la compilation est généralement très rapide.

## Compiler et exécuter : les méthodes

### La méthode la plus simple : F9

La façon la plus courante de compiler et exécuter votre application est d'appuyer sur **F9** (ou cliquer sur le bouton vert "Exécuter" dans la barre d'outils).

Quand vous faites cela, Delphi :
1. Sauvegarde tous les fichiers modifiés
2. Compile votre projet
3. Si la compilation réussit, lance automatiquement votre application

Si vous avez déjà lancé votre application et que vous appuyez à nouveau sur F9, Delphi détecte que l'application tourne déjà et vous propose de la terminer avant de recompiler.

### Compiler sans exécuter : Ctrl + F9

Parfois, vous voulez juste vérifier que votre code compile correctement, sans lancer l'application. Utilisez **Ctrl + F9** (ou menu **Projet > Compiler**).

Cela compile votre projet et vous indique s'il y a des erreurs, mais ne lance pas l'exécutable.

C'est utile pour :
- Vérifier rapidement votre code
- Compiler avant de fermer Delphi
- Compiler une DLL ou une bibliothèque qui ne peut pas se lancer seule

### Tout recompiler : Shift + F9

Normalement, Delphi est intelligent : il ne recompile que les fichiers qui ont changé depuis la dernière compilation. C'est ce qu'on appelle la "compilation incrémentale", et c'est ce qui rend Delphi rapide.

Mais parfois, vous voulez forcer une recompilation complète de tous les fichiers, depuis zéro. Utilisez **Shift + F9** (ou menu **Projet > Tout construire**).

Utilisez cette option quand :
- Vous soupçonnez un problème de compilation incrémentale
- Vous avez modifié des options du projet
- Vous passez d'une configuration à une autre (Debug vers Release)
- Quelque chose semble "cassé" sans raison apparente

### Nettoyer le projet

Pour supprimer tous les fichiers générés par la compilation (fichiers .dcu, .exe, etc.) et repartir de zéro : **Projet > Nettoyer**.

Cela peut résoudre certains problèmes mystérieux de compilation.

## Le processus de compilation en détail

### Phase 1 : Analyse syntaxique

Le compilateur lit votre code et vérifie que vous respectez les règles de grammaire du langage Object Pascal :

- Les instructions se terminent-elles par un point-virgule ?
- Les blocs begin/end sont-ils équilibrés ?
- Les mots-clés sont-ils correctement utilisés ?

Si vous avez fait une faute de frappe ou oublié un point-virgule, c'est à ce stade que l'erreur sera détectée.

### Phase 2 : Analyse sémantique

Le compilateur vérifie que votre code a du sens :

- Les variables sont-elles déclarées avant d'être utilisées ?
- Les types sont-ils compatibles (vous ne pouvez pas assigner une chaîne à un entier) ?
- Les fonctions sont-elles appelées avec le bon nombre de paramètres ?
- Les propriétés que vous utilisez existent-elles ?

C'est à ce stade que les erreurs de logique de typage sont détectées.

### Phase 3 : Génération de code

Si tout est correct, le compilateur génère des fichiers intermédiaires appelés "unités compilées" (fichiers .dcu pour Delphi Compiled Unit). Ces fichiers contiennent le code machine correspondant à vos unités .pas.

Les .dcu sont stockés dans le dossier de sortie (généralement Win32\Debug ou Win64\Debug).

### Phase 4 : Liaison (Linking)

L'éditeur de liens (linker) prend tous vos fichiers .dcu et les assemble avec les bibliothèques nécessaires (VCL, RTL, bibliothèques système) pour créer le fichier .exe final.

C'est aussi à ce stade que sont incorporées les ressources (icône de l'application, fichiers .dfm, etc.).

### Phase 5 : Finalisation

Le fichier .exe est créé dans le dossier de sortie. Il est prêt à être exécuté !

### Ce qui se passe en coulisse

Pendant la compilation, vous verrez en bas de l'IDE la fenêtre **Messages** qui affiche la progression :

```
Compilation démarrée  
Compilation de MonProjet.dpr  
Compilation de FormPrincipale.pas  
...
Compilation réussie  
Temps écoulé : 00:00:01.2  
```

Cette fenêtre est importante : c'est là que s'afficheront les erreurs et avertissements.

## Comprendre les messages de compilation

### Les erreurs (Errors)

Les erreurs, affichées en rouge, sont des problèmes graves qui empêchent la compilation. Tant qu'il y a des erreurs, vous ne pouvez pas créer l'exécutable.

Exemples d'erreurs courantes :

**"Undeclared identifier 'X'"** : vous utilisez une variable ou une fonction 'X' qui n'a pas été déclarée. Vérifiez l'orthographe, et assurez-vous que la bonne unité est dans votre clause uses.

**"Incompatible types"** : vous essayez d'assigner une valeur d'un type à une variable d'un autre type incompatible. Par exemple, assigner une chaîne à un entier.

**"';' expected"** : vous avez oublié un point-virgule quelque part.

**"'BEGIN' expected"** : la structure de votre code est incorrecte, probablement un begin/end manquant ou mal placé.

**"Identifier redeclared"** : vous avez déclaré deux fois la même variable ou fonction.

### Les avertissements (Warnings)

Les avertissements, affichés en jaune, sont des problèmes potentiels qui ne bloquent pas la compilation, mais qui méritent votre attention.

Exemples d'avertissements courants :

**"Variable 'X' might not have been initialized"** : vous utilisez peut-être une variable avant de lui avoir donné une valeur. C'est dangereux car elle contiendra une valeur aléatoire.

**"Comparison always evaluates to True/False"** : vous faites une comparaison dont le résultat est toujours le même. C'est probablement une erreur de logique.

**"Return value might be undefined"** : une fonction ne retourne pas toujours une valeur dans tous les cas possibles.

**"Local variable 'X' not used"** : vous avez déclaré une variable que vous n'utilisez jamais. Ce n'est pas grave, mais c'est du code mort.

Ne négligez pas les avertissements ! Même s'ils ne bloquent pas la compilation, ils signalent souvent de vrais bugs.

### Les indications (Hints)

Les indications, affichées en bleu, sont des suggestions mineures pour améliorer votre code. Elles n'indiquent généralement pas de problème réel.

Exemples :

**"Private symbol 'X' declared but never used"** : une méthode ou variable privée que vous n'utilisez pas.

**"Parameter 'X' not used"** : un paramètre de fonction que vous ne lisez jamais.

Les indications peuvent généralement être ignorées, surtout au début. Mais dans du code professionnel, on essaie de les éliminer pour avoir un code propre.

### Naviguer dans les messages

Double-cliquez sur un message d'erreur ou d'avertissement pour naviguer directement vers la ligne de code concernée. C'est très pratique pour corriger rapidement les problèmes.

Vous pouvez aussi filtrer les messages par type (erreurs, avertissements, indications) en utilisant les boutons en haut de la fenêtre Messages.

## Corriger les erreurs de compilation

### Stratégie de correction

Quand vous avez plusieurs erreurs, ne paniquez pas ! Suivez cette stratégie :

1. **Lisez le premier message d'erreur** : souvent, une seule erreur en génère plusieurs. Corrigez d'abord celle tout en haut de la liste.

2. **Localisez le problème** : double-cliquez sur le message pour aller à la ligne concernée.

3. **Comprenez l'erreur** : lisez le message d'erreur attentivement. Que dit-il exactement ? Parfois, le message est clair ; parfois, il faut réfléchir un peu.

4. **Corrigez** : faites la correction nécessaire.

5. **Recompilez** : appuyez sur F9 ou Ctrl + F9 pour voir si l'erreur est résolue.

6. **Répétez** : s'il reste des erreurs, recommencez avec la première de la liste.

### Erreurs en cascade

Une seule erreur peut en provoquer plusieurs autres. Par exemple, si vous oubliez de déclarer une variable, toutes les lignes qui l'utilisent généreront une erreur "Undeclared identifier".

C'est pourquoi il faut toujours corriger les erreurs dans l'ordre, de haut en bas. Souvent, corriger la première erreur fait disparaître plusieurs autres.

### Utiliser l'aide

Si vous ne comprenez pas un message d'erreur, sélectionnez-le et appuyez sur **F1** pour ouvrir l'aide de Delphi. L'aide fournit souvent des explications détaillées et des exemples.

Vous pouvez aussi chercher l'erreur sur internet. La communauté Delphi est très active, et il y a de fortes chances que quelqu'un ait déjà rencontré et résolu le même problème.

### Erreurs courantes et leurs solutions

**"Unit X not found"**
- **Cause** : l'unité X n'est pas dans le chemin de recherche
- **Solution** : ajoutez l'unité au projet ou configurez le chemin de recherche dans les options du projet

**"Cannot assign to a read-only property"**
- **Cause** : vous essayez de modifier une propriété en lecture seule
- **Solution** : certaines propriétés ne peuvent être définies qu'à la création ou via des méthodes spécifiques

**"Access violation at address..."** (à l'exécution)
- **Cause** : vous accédez à une zone mémoire invalide (pointeur nil, objet non créé, etc.)
- **Solution** : vérifiez que vos objets sont bien créés avant utilisation, et que vous n'accédez pas à des indices hors limites

**"Abstract Error"** (à l'exécution)
- **Cause** : vous appelez une méthode abstraite qui devrait être implémentée dans une classe dérivée
- **Solution** : implémentez la méthode dans votre classe ou utilisez une classe non abstraite

## Les configurations de compilation

### Debug vs Release

Delphi propose deux configurations principales de compilation :

#### Configuration Debug (Débogage)

C'est la configuration par défaut pendant le développement. Caractéristiques :

**Optimisations minimales** : le compilateur ne réorganise pas trop le code, pour qu'il soit plus facile à déboguer

**Informations de débogage incluses** : des données supplémentaires sont ajoutées pour permettre le débogage (points d'arrêt, inspection de variables, etc.)

**Vérifications supplémentaires** : contrôles des débordements, assertions, etc.

**Fichier .exe plus gros** : à cause des informations supplémentaires

**Performance réduite** : le code est moins optimisé

Utilisez cette configuration pendant tout le développement.

#### Configuration Release (Production)

C'est la configuration pour la version finale de votre application. Caractéristiques :

**Optimisations maximales** : le compilateur réorganise le code pour maximiser la performance

**Pas d'informations de débogage** : l'exécutable est plus petit

**Vérifications désactivées** : pour gagner en vitesse

**Fichier .exe plus petit** : grâce aux optimisations

**Performance maximale** : le code tourne plus vite

Utilisez cette configuration uniquement pour créer la version finale à distribuer.

### Changer de configuration

Pour changer de configuration :

1. Dans la barre d'outils, trouvez le menu déroulant qui affiche "Debug" ou "Release"
2. Cliquez dessus et sélectionnez la configuration souhaitée

Ou via le menu **Projet > Configuration de build** et choisissez la configuration active.

### Plateformes cibles

Delphi permet de compiler pour différentes plateformes :

**Win32** : Windows 32 bits (compatible avec tous les Windows, de XP à Windows 11)

**Win64** : Windows 64 bits (recommandé pour les applications modernes)

**macOS** : pour créer des applications Mac (nécessite une licence appropriée)

**iOS** : pour créer des applications iPhone et iPad

**Android** : pour créer des applications Android

**Linux** : pour créer des applications Linux (avec FireMonkey)

Pour changer de plateforme :

1. Dans l'Explorateur de projets, clic droit sur le projet
2. Choisissez **Ajouter une plateforme**
3. Sélectionnez la plateforme souhaitée
4. Dans la barre d'outils, sélectionnez la plateforme active

Pour débuter, concentrez-vous sur Win32 ou Win64. Les autres plateformes nécessitent des configurations supplémentaires.

## Les fichiers générés par la compilation

### Dans le dossier du projet

Après une compilation, plusieurs fichiers et dossiers sont créés :

#### Dossier Win32 ou Win64

C'est le dossier principal de sortie. Il contient :

**Debug** ou **Release** (sous-dossier) : contient l'exécutable final et les fichiers nécessaires

À l'intérieur de Debug/Release, vous trouverez :

- **MonProjet.exe** : votre application exécutable
- **Fichiers .dcu** : les unités compilées (fichiers intermédiaires)
- **Fichiers .res** : les ressources compilées
- **Fichiers .map** : informations de débogage (si activées)

### Fichiers temporaires et intermédiaires

Ces fichiers sont générés pendant la compilation et peuvent être supprimés (via "Nettoyer le projet") :

**Fichiers .dcu** : unités compilées, générées à partir des .pas

**Fichiers .~pas, .~dfm** : fichiers de sauvegarde automatique de Delphi

**__history** (dossier) : historique des modifications de fichiers

**__recovery** (dossier) : fichiers de récupération en cas de crash

Ces fichiers ne sont pas nécessaires pour l'exécution de votre application. Seul le .exe l'est (et éventuellement des DLL si vous en utilisez).

### Distribution de votre application

Pour distribuer votre application, vous avez besoin au minimum du fichier .exe compilé en mode Release.

Attention : votre .exe peut avoir besoin de fichiers supplémentaires :

- **DLL externes** si vous en utilisez
- **Fichiers de ressources** (images, sons, etc.) si vous les chargez depuis le disque
- **Fichiers de base de données** si votre application en utilise

Nous verrons la distribution en détail dans un chapitre ultérieur.

## Options de compilation

### Accéder aux options

Menu **Projet > Options**, puis section **Compilation** (ou **Compilateur Delphi**).

Vous y trouverez de nombreuses options. Pour débuter, les valeurs par défaut sont généralement appropriées, mais voici quelques options importantes à connaître :

### Options de syntaxe

**Syntaxe étendue** : active des fonctionnalités modernes du langage. Recommandé : activé.

**Chaînes longues** : utilise les chaînes modernes (String) au lieu des anciennes ShortString. Recommandé : activé.

### Options de débogage

**Informations de débogage** : indispensable en mode Debug, désactivé en Release.

**Assertions** : active les assertions (vérifications de conditions dans le code). Utile pour détecter les bugs.

**Vérification des débordements** : vérifie que les calculs ne dépassent pas les limites des types. Ralentit un peu le code mais évite des bugs.

**Vérification des E/S** : vérifie les erreurs d'entrée/sortie fichier. Recommandé.

### Options d'optimisation

**Optimisation** : niveau d'optimisation du code (aucune, basique, complète).

**Inline** : permet au compilateur d'insérer le code de petites fonctions directement dans le code appelant, pour gagner en vitesse.

**Élimination du code mort** : supprime le code qui n'est jamais exécuté.

En mode Debug, désactivez les optimisations pour faciliter le débogage. En mode Release, activez-les toutes pour maximiser la performance.

### Options d'avertissements

Vous pouvez configurer quels avertissements et indications afficher. Par défaut, Delphi est bien configuré, mais vous pouvez :

- Transformer un avertissement en erreur (pour le forcer à être corrigé)
- Désactiver certaines indications qui vous gênent
- Activer des vérifications supplémentaires

## Optimiser le temps de compilation

### Compilation incrémentale

Par défaut, Delphi ne recompile que ce qui a changé. Pour que cela fonctionne bien :

- Sauvegardez régulièrement vos fichiers
- Ne modifiez pas manuellement les fichiers .dcu
- Utilisez "Nettoyer" si vous soupçonnez un problème

### Diviser le code en unités

Plus vos unités sont petites et bien séparées, plus la compilation incrémentale est efficace. Si vous modifiez une petite unité, seule celle-ci sera recompilée.

À l'inverse, si tout votre code est dans un seul gros fichier, tout sera recompilé à chaque modification.

### Précompiler les bibliothèques

Les bibliothèques que vous ne modifiez pas (VCL, composants tiers) sont déjà précompilées. C'est pourquoi elles ne rallongent pas le temps de compilation.

### Machine plus rapide

Évidemment, un ordinateur plus rapide compile plus vite ! Mais surtout :

- **SSD** : un disque SSD accélère grandement la compilation
- **RAM** : au moins 8 Go, idéalement 16 Go ou plus
- **Processeur** : plus de cœurs = compilation plus rapide (Delphi utilise la parallélisation)

## Résolution de problèmes

### "Cannot compile"

Si Delphi refuse de compiler sans raison apparente :

1. Fermez et relancez Delphi
2. Nettoyez le projet
3. Vérifiez que les fichiers ne sont pas en lecture seule
4. Vérifiez les droits d'accès au dossier du projet

### "Cannot create output file"

L'application est probablement encore en cours d'exécution. Fermez-la complètement, puis recompilez.

Ou un antivirus bloque la création du fichier. Ajoutez une exception pour votre dossier de projets.

### Compilation très lente

- Désactivez temporairement l'antivirus pour le dossier du projet
- Vérifiez que votre disque n'est pas plein
- Fermez les autres applications gourmandes
- Utilisez un SSD plutôt qu'un disque dur

### Messages bizarres après une mise à jour

Après avoir mis à jour Delphi ou des composants :

1. Nettoyez le projet
2. Recompilez tout (Shift + F9)
3. Si ça ne suffit pas, supprimez manuellement tous les .dcu

## Bonnes pratiques

### Compilez souvent

Ne laissez pas des heures de code s'accumuler sans compiler. Compilez régulièrement, même si votre code n'est pas terminé. Cela permet de détecter les erreurs tôt.

### Corrigez immédiatement les avertissements

Ne laissez pas les avertissements s'accumuler. Corrigez-les au fur et à mesure. Un code sans avertissements est généralement un code de meilleure qualité.

### Testez en Debug, distribuez en Release

Tout votre développement et vos tests doivent se faire en mode Debug. Ce n'est qu'à la toute fin, juste avant la distribution, que vous compilez en Release.

### Sauvegardez avant de compiler

Delphi sauvegarde automatiquement, mais prenez l'habitude de sauvegarder manuellement (**Ctrl + S**) avant de compiler. C'est un réflexe de sécurité.

### Utilisez le contrôle de version

Un système comme Git vous permet de revenir en arrière si une compilation casse tout. Commitez régulièrement votre code qui compile.

### Documentez les options spéciales

Si vous modifiez des options de compilation non standard, documentez pourquoi dans un fichier README ou dans les commentaires du projet. Votre "vous futur" (ou vos collègues) vous remerciera.

## Conclusion

La compilation est le pont entre votre code et l'application finale. Comprendre ce processus vous permet de :

- **Travailler plus efficacement** en compilant au bon moment
- **Corriger les erreurs plus rapidement** en comprenant les messages
- **Optimiser vos applications** en choisissant les bonnes options
- **Distribuer correctement** en créant les bons exécutables

Voici les points essentiels à retenir :

- **F9** compile et exécute : c'est votre raccourci principal
- **Corrigez les erreurs de haut en bas** : une erreur peut en masquer d'autres
- **Ne négligez pas les avertissements** : ils signalent souvent de vrais problèmes
- **Debug pour développer, Release pour distribuer** : utilisez la bonne configuration
- **Compilez souvent** : pour détecter les erreurs tôt

Avec la pratique, la compilation deviendra une seconde nature. Vous compilerez sans même y penser, et vous saurez instinctivement corriger les erreurs courantes.

Dans la prochaine section, nous verrons comment personnaliser l'IDE Delphi pour l'adapter à vos préférences et améliorer votre productivité !

⏭️ [Personnalisation de l'IDE](/02-decouverte-de-lide-delphi/06-personnalisation-de-lide.md)
