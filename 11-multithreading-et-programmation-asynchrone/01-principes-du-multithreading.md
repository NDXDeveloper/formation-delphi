🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.1 Principes du multithreading

## Qu'est-ce que le multithreading ?

Le **multithreading** (ou "programmation multi-fils") est une technique qui permet à une application d'exécuter plusieurs tâches simultanément. Imaginez que votre programme puisse "faire plusieurs choses en même temps", comme un chef cuisinier qui peut surveiller plusieurs plats simultanément.

### Analogie simple

Pensez à votre programme comme à une cuisine :
- **Sans multithreading** : Un seul cuisinier qui doit accomplir toutes les tâches l'une après l'autre. Si une tâche prend du temps (comme attendre que l'eau bouille), tout le reste est bloqué.
- **Avec multithreading** : Plusieurs cuisiniers qui travaillent ensemble. Pendant qu'un cuisinier attend que l'eau bouille, un autre peut préparer les légumes, et un troisième peut dresser les assiettes.

## Pourquoi utiliser le multithreading ?

### 1. Interfaces utilisateur réactives

Sans multithreading, si votre application effectue une opération longue (téléchargement d'un fichier, traitement de données), l'interface se fige et l'utilisateur ne peut plus interagir avec votre application. C'est très frustrant !

**Exemple concret** : Lors du téléchargement d'un fichier, l'utilisateur souhaite pouvoir :
- Continuer à naviguer dans l'application
- Annuler le téléchargement si nécessaire
- Voir une barre de progression qui se met à jour

### 2. Meilleures performances

Sur les ordinateurs modernes qui possèdent plusieurs cœurs de processeur, le multithreading permet d'utiliser toute la puissance disponible. C'est comme avoir plusieurs employés au lieu d'un seul pour accomplir un travail.

### 3. Traitement en arrière-plan

Certaines tâches peuvent s'exécuter en arrière-plan sans bloquer l'utilisateur :
- Sauvegarde automatique des données
- Synchronisation avec un serveur
- Compression de fichiers
- Analyse de données

## Concepts fondamentaux

### Thread (Fil d'exécution)

Un **thread** est une séquence d'instructions qui s'exécute de manière indépendante. Chaque programme Delphi possède au minimum un thread : le **thread principal** (ou thread UI).

**Le thread principal** :
- Gère l'interface utilisateur
- Reçoit les événements (clics, saisies clavier, etc.)
- Ne doit JAMAIS être bloqué par des opérations longues

**Les threads secondaires** :
- Sont créés pour exécuter des tâches longues ou complexes
- S'exécutent en parallèle du thread principal
- Ne doivent PAS modifier directement l'interface utilisateur

### Processus vs Thread

Il est important de comprendre la différence :

**Processus** :
- Un programme complet en cours d'exécution
- Possède sa propre mémoire isolée
- Exemple : Word, Excel, votre application Delphi sont des processus séparés

**Thread** :
- Une "sous-tâche" à l'intérieur d'un processus
- Partage la mémoire avec les autres threads du même processus
- Plusieurs threads existent dans un seul processus

### Concurrence vs Parallélisme

**Concurrence** : Plusieurs tâches progressent en alternance (même sur un seul cœur de processeur). Le système donne rapidement la parole à chaque tâche tour à tour, créant l'illusion qu'elles s'exécutent simultanément.

**Parallélisme** : Plusieurs tâches s'exécutent réellement en même temps sur plusieurs cœurs de processeur.

```
Concurrence (1 cœur) :  
Tâche A : ▓▓░░▓▓░░▓▓░░▓▓░░  
Tâche B : ░░▓▓░░▓▓░░▓▓░░▓▓  
          Le cœur alterne rapidement entre A et B

Parallélisme (2 cœurs) :  
Cœur 1 - Tâche A : ▓▓▓▓▓▓▓▓▓▓▓▓  
Cœur 2 - Tâche B : ▓▓▓▓▓▓▓▓▓▓▓▓  
          A et B s'exécutent vraiment en simultané
```

Dans Delphi, le multithreading permet les deux selon le matériel disponible.

## Avantages et défis du multithreading

### Avantages

1. **Réactivité** : L'application reste responsive même pendant les opérations longues
2. **Performance** : Exploitation optimale des processeurs multi-cœurs
3. **Modularité** : Séparation claire entre les différentes tâches
4. **Expérience utilisateur** : L'utilisateur n'a pas l'impression que l'application "rame"

### Défis et précautions

1. **Complexité accrue** : Le code devient plus difficile à écrire et à déboguer
2. **Problèmes de synchronisation** : Deux threads qui modifient la même donnée peuvent créer des résultats imprévisibles
3. **Conditions de concurrence** : Des bugs difficiles à reproduire peuvent apparaître
4. **Consommation de ressources** : Trop de threads peuvent ralentir le système

## Quand utiliser le multithreading ?

### Situations appropriées

- Téléchargement ou upload de fichiers
- Requêtes vers une base de données qui prennent du temps
- Traitement d'images ou de vidéos
- Calculs mathématiques complexes
- Surveillance de périphériques externes
- Communications réseau
- Compression/décompression de données
- Analyse de grands volumes de données

### Situations où ce n'est PAS nécessaire

- Opérations très rapides (quelques millisecondes)
- Code simple et séquentiel
- Lorsque la complexité ajoutée n'en vaut pas la peine
- Applications très simples avec peu d'interactions

## La règle d'or en multithreading avec Delphi

**🚨 RÈGLE ESSENTIELLE** : Les composants visuels (boutons, labels, grilles, etc.) ne doivent être modifiés QUE depuis le thread principal.

Si un thread secondaire doit mettre à jour l'interface, il doit demander au thread principal de le faire. Delphi fournit des mécanismes spécifiques pour cela (que nous verrons dans les prochaines sections).

## Types de tâches parallèles

### 1. Tâches indépendantes

Plusieurs tâches qui n'ont pas besoin de communiquer entre elles.

**Exemple** : Convertir plusieurs images simultanément. Chaque conversion est indépendante.

### 2. Tâches avec communication

Plusieurs tâches qui doivent échanger des informations.

**Exemple** : Un thread qui télécharge des données et un autre qui les traite au fur et à mesure.

### 3. Tâches maître-esclave

Un thread principal qui distribue le travail à plusieurs threads secondaires.

**Exemple** : Traiter un million de lignes d'un fichier en divisant le travail entre 4 threads.

## Considérations importantes

### Sécurité des threads (Thread-Safety)

Un code est dit "thread-safe" s'il peut être utilisé par plusieurs threads simultanément sans problème. Tous les composants Delphi ne sont pas thread-safe !

### Synchronisation

Lorsque plusieurs threads accèdent à la même ressource (variable, fichier, base de données), il faut coordonner les accès pour éviter les conflits.

### Surcharge (Overhead)

Créer et gérer des threads a un coût. Pour des tâches très courtes, le temps passé à créer le thread peut être supérieur au temps gagné !

### Débogage

Les bugs liés au multithreading sont souvent difficiles à reproduire car ils dépendent du timing d'exécution des threads.

## Conclusion

Le multithreading est un outil puissant pour créer des applications performantes et réactives. Cependant, il introduit de la complexité qu'il faut maîtriser. Dans les sections suivantes, nous verrons comment Delphi facilite la création et la gestion de threads de manière sûre et efficace.

**Points clés à retenir** :
- Le multithreading permet d'exécuter plusieurs tâches simultanément
- Il garde l'interface utilisateur réactive pendant les opérations longues
- Le thread principal gère l'interface utilisateur et ne doit jamais être bloqué
- Les threads secondaires ne doivent jamais modifier directement l'interface
- C'est un outil puissant mais qui demande de la rigueur

⏭️ [Création et gestion de threads](/11-multithreading-et-programmation-asynchrone/02-creation-et-gestion-de-threads.md)
