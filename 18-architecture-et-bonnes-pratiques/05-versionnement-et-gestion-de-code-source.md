🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.5 Versionnement et gestion de code source

## Introduction

Imaginez que vous écrivez un roman. Vous créez des chapitres, puis vous décidez de tout réécrire différemment. Quelques jours plus tard, vous regrettez : l'ancienne version était meilleure ! Mais vous l'avez écrasée... elle est perdue à jamais.

Ou pire : vous travaillez sur un document avec trois collègues. Chacun fait ses modifications de son côté. Comment fusionner tout ça sans perdre le travail de personne ? Comment savoir qui a modifié quoi et quand ?

Le **versionnement** (ou gestion de versions) résout ces problèmes en conservant **l'historique complet** de votre code : chaque modification, chaque version, avec la possibilité de revenir en arrière à tout moment.

### Qu'est-ce que le versionnement ?

Le versionnement est un système qui enregistre les changements apportés à vos fichiers au fil du temps. C'est comme une **machine à remonter le temps** pour votre code.

**Sans versionnement :**
```
MonProjet/
  - MonFichier.pas
  - MonFichier_v2.pas
  - MonFichier_v2_final.pas
  - MonFichier_v2_final_VRAI.pas
  - MonFichier_v2_final_VRAI_jeudi.pas
```

**Avec versionnement :**
```
MonProjet/
  - MonFichier.pas  (avec tout l'historique accessible)
```

### Pourquoi utiliser le versionnement ?

#### 1. Historique complet

Vous pouvez voir :
- Qui a modifié quoi
- Quand
- Pourquoi (grâce aux messages de commit)
- Revenir à n'importe quelle version antérieure

#### 2. Collaboration facilitée

Plusieurs développeurs peuvent travailler sur le même projet simultanément sans s'écraser mutuellement le travail.

#### 3. Branches et expérimentation

Vous pouvez créer des "branches" pour tester de nouvelles fonctionnalités sans affecter le code principal. Si ça ne marche pas, vous supprimez la branche. Si ça marche, vous la fusionnez.

#### 4. Sauvegarde et sécurité

Votre code est sauvegardé sur des serveurs distants. Si votre ordinateur tombe en panne, rien n'est perdu.

#### 5. Travail offline puis synchronisation

Vous pouvez travailler sans connexion Internet, puis synchroniser vos modifications plus tard.

## Git : Le standard actuel

**Git** est le système de gestion de versions le plus utilisé au monde. Il a été créé par Linus Torvalds (le créateur de Linux) en 2005.

### Pourquoi Git ?

- **Gratuit et open source**
- **Décentralisé** : Chaque développeur a une copie complète de l'historique
- **Rapide et léger**
- **Branching puissant** : Créer et fusionner des branches est facile
- **Standard de l'industrie** : Utilisé par la majorité des projets

### Les concepts fondamentaux de Git

Avant de commencer, comprenons quelques concepts clés :

#### Repository (Dépôt)

Un **repository** (ou "repo") est un dossier contenant votre projet et tout son historique. C'est comme une base de données qui stocke toutes les versions de vos fichiers.

Il existe deux types :
- **Repository local** : Sur votre machine
- **Repository distant** : Sur un serveur (GitHub, GitLab, etc.)

#### Commit

Un **commit** est un instantané (snapshot) de votre projet à un moment donné. C'est comme prendre une photo de tous vos fichiers.

Chaque commit contient :
- Les modifications apportées
- Un message décrivant les changements
- L'auteur et la date
- Un identifiant unique (hash)

```
Commit 1: "Création du projet"
   ↓
Commit 2: "Ajout du formulaire principal"
   ↓
Commit 3: "Connexion à la base de données"
   ↓
Commit 4: "Correction bug affichage"
```

#### Working Directory, Staging Area, Repository

Git a trois zones importantes :

```
┌─────────────────────┐
│  Working Directory  │  ← Vos fichiers de travail
│   (Modifications)   │
└──────────┬──────────┘
           │ git add
           ↓
┌─────────────────────┐
│   Staging Area      │  ← Zone de préparation
│    (Index)          │
└──────────┬──────────┘
           │ git commit
           ↓
┌─────────────────────┐
│    Repository       │  ← Historique permanent
│   (.git folder)     │
└─────────────────────┘
```

1. **Working Directory** : Vos fichiers actuels que vous modifiez
2. **Staging Area** : Zone où vous préparez les modifications avant de les enregistrer
3. **Repository** : L'historique complet de votre projet

#### Branch (Branche)

Une **branch** est une ligne de développement indépendante. C'est comme créer une copie parallèle de votre projet pour travailler sur une fonctionnalité sans affecter le code principal.

```
       main (branche principale)
         │
    ┌────┼────┬────┬────┐
    │    │    │    │    │
    C1   C2   C3   C4   C5
              │
              └─── feature/nouvelle-fonction
                   │
                   C6 ── C7
```

## Installation et configuration de Git

### Installation

**Windows :**
1. Téléchargez Git depuis https://git-scm.com/download/win
2. Exécutez l'installateur
3. Acceptez les options par défaut (ou personnalisez selon vos préférences)

**Vérification de l'installation :**

Ouvrez un terminal (CMD ou PowerShell) et tapez :
```bash
git --version
```

Vous devriez voir quelque chose comme : `git version 2.43.0`

### Configuration initiale

Avant d'utiliser Git, configurez votre identité :

```bash
# Votre nom (sera visible dans les commits)
git config --global user.name "Votre Nom"

# Votre email
git config --global user.email "votre.email@example.com"

# Éditeur par défaut (optionnel)
git config --global core.editor "notepad"

# Fin de ligne (recommandé pour Windows)
git config --global core.autocrlf true
```

**Vérifier votre configuration :**
```bash
git config --list
```

### Interface graphique ou ligne de commande ?

Vous avez deux options pour utiliser Git :

#### 1. Ligne de commande (Terminal)

**Avantages :**
- ✅ Contrôle total
- ✅ Fonctionne partout
- ✅ Compréhension profonde

**Inconvénients :**
- ❌ Courbe d'apprentissage
- ❌ Moins visuel

#### 2. Interface graphique (GUI)

**Outils populaires :**
- **GitKraken** : Moderne et visuellement attrayant
- **SourceTree** : Gratuit et complet
- **GitHub Desktop** : Simple et intégré à GitHub
- **TortoiseGit** : Intégré à l'explorateur Windows
- **Git Extensions** : Intégration avec Visual Studio/Delphi

**Avantages :**
- ✅ Visuel et intuitif
- ✅ Facile pour les débutants
- ✅ Vue graphique des branches

**Inconvénients :**
- ❌ Moins de contrôle
- ❌ Peut cacher des détails importants

**Recommandation :** Commencez par une GUI pour comprendre les concepts, puis apprenez progressivement la ligne de commande pour plus de puissance.

## Créer votre premier repository

### Initialiser un nouveau projet

Créons un repository pour un projet Delphi existant.

**Via la ligne de commande :**

```bash
# 1. Naviguez vers votre dossier projet
cd C:\MesProjets\MonAppliDelphi

# 2. Initialisez Git
git init

# 3. Vérifiez le statut
git status
```

Le dossier `.git` est créé. C'est là que Git stocke tout l'historique.

**Via GitHub Desktop :**
1. File → Add Local Repository
2. Sélectionnez votre dossier
3. Cliquez sur "Create Repository"

### Le fichier .gitignore

**IMPORTANT** : Avant votre premier commit, créez un fichier `.gitignore` pour exclure les fichiers générés.

Créez un fichier nommé `.gitignore` à la racine de votre projet :

```gitignore
# Fichiers compilés Delphi
*.dcu
*.exe
*.dll
*.bpl
*.dcp
*.so
*.apk
*.drc
*.map
*.dres
*.rsm
*.tds
*.dcu
*.lib
*.a
*.o
*.ocx

# Dossiers de compilation
__history/
__recovery/
Win32/  
Win64/  
Win64/  
OSX32/  
OSX64/  
iOSDevice/  
iOSDevice32/  
iOSDevice64/  
iOSSimulator/  
Android/  
Android64/  
Linux64/  

# Fichiers de sauvegarde
*.~pas
*.~dfm
*.~dpr
*.~dpk
*.~dsk
*.~ddp
*.bak
*.*~

# Fichiers locaux
*.local
*.identcache
*.projdata
*.tvsconfig
*.stat
*.dsk
Desktop.ini

# Fichiers de configuration locaux
config.local.ini  
config.prod.ini  
*.local.json

# Logs et données temporaires
*.log
*.tmp
temp/  
logs/  

# IDE
.vs/
.vscode/
*.suo
*.user

# Fichiers système
.DS_Store
Thumbs.db
```

**Pourquoi .gitignore ?**

- ❌ Les `.exe` et `.dcu` sont générés à la compilation, inutile de les versionner
- ❌ Les dossiers `Win32/`, `Win64/` contiennent des fichiers temporaires
- ❌ Les fichiers de config locaux peuvent contenir des mots de passe
- ✅ Seul le code source doit être versionné

### Premier commit

Maintenant que `.gitignore` est en place, faisons notre premier commit :

```bash
# 1. Ajouter tous les fichiers
git add .

# 2. Vérifier ce qui sera commité
git status

# 3. Créer le commit
git commit -m "Initial commit - Création du projet"
```

**Explication :**
- `git add .` : Ajoute tous les fichiers (sauf ceux dans .gitignore) à la staging area
- `git commit -m "message"` : Crée un commit avec un message descriptif

**Via GitHub Desktop :**
1. Tous les fichiers modifiés apparaissent dans la liste
2. Entrez un message de commit en bas à gauche
3. Cliquez sur "Commit to main"

### Voir l'historique

```bash
# Afficher l'historique des commits
git log

# Version plus compacte
git log --oneline

# Avec un graphique des branches
git log --oneline --graph --all
```

**Résultat :**
```
3f8a2b1 (HEAD -> main) Initial commit - Création du projet
```

## Workflow Git de base

Voici le cycle de travail typique avec Git :

### 1. Modifier des fichiers

Vous travaillez normalement sur votre code dans Delphi.

```pascal
// Vous modifiez MainForm.pas
procedure TFormMain.ButtonClickClick(Sender: TObject);  
begin  
  ShowMessage('Hello Git!');
end;
```

### 2. Vérifier l'état

```bash
git status
```

**Résultat :**
```
On branch main  
Changes not staged for commit:  
  modified:   Source/MainForm.pas
```

### 3. Voir les modifications

```bash
# Voir les modifications en détail
git diff

# Voir les modifications d'un fichier spécifique
git diff Source/MainForm.pas
```

**Résultat :**
```diff
diff --git a/Source/MainForm.pas b/Source/MainForm.pas  
index 1234567..abcdefg 100644  
--- a/Source/MainForm.pas
+++ b/Source/MainForm.pas
@@ -45,7 +45,7 @@ procedure TFormMain.ButtonClickClick(Sender: TObject);
 begin
-  ShowMessage('Hello');
+  ShowMessage('Hello Git!');
 end;
```

### 4. Ajouter à la staging area

```bash
# Ajouter un fichier spécifique
git add Source/MainForm.pas

# Ou ajouter tous les fichiers modifiés
git add .

# Ajouter seulement certains types de fichiers
git add *.pas
```

### 5. Créer un commit

```bash
git commit -m "Ajout du message dans le bouton"
```

**Messages de commit :**

✅ **Bons messages :**
- "Ajout de la fonctionnalité de connexion"
- "Correction bug d'affichage dans la grille"
- "Refactoring de la classe TClient"
- "Mise à jour de la base de données MySQL"

❌ **Mauvais messages :**
- "update"
- "fix"
- "modifs"
- "ça marche"

**Convention pour les messages :**
```bash
# Format recommandé
git commit -m "Type: Description courte

Description détaillée si nécessaire
- Point 1
- Point 2"
```

**Types courants :**
- `feat:` Nouvelle fonctionnalité
- `fix:` Correction de bug
- `refactor:` Refactoring du code
- `docs:` Documentation
- `style:` Formatage, point-virgules, etc.
- `test:` Ajout de tests
- `chore:` Tâches diverses

**Exemples :**
```bash
git commit -m "feat: Ajout du formulaire de connexion"  
git commit -m "fix: Correction du bug d'affichage des dates"  
git commit -m "refactor: Séparation UI et logique métier"  
```

### 6. Cycle continu

Répétez ce cycle : modifier → vérifier → ajouter → commiter.

```
┌─────────────────────────────────┐
│  1. Modifier les fichiers       │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│  2. git status / git diff       │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│  3. git add                     │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│  4. git commit -m "message"     │
└────────────┬────────────────────┘
             ↓
         Recommencer
```

## Les branches

Les branches sont l'une des fonctionnalités les plus puissantes de Git.

### Qu'est-ce qu'une branche ?

Une branche est une ligne de développement indépendante. Par défaut, vous êtes sur la branche `main` (anciennement `master`).

**Cas d'usage :**
- Développer une nouvelle fonctionnalité sans affecter le code principal
- Corriger un bug urgent
- Expérimenter une nouvelle approche

### Créer une branche

```bash
# Créer une nouvelle branche
git branch feature/ajout-client

# Lister les branches
git branch

# Changer de branche
git checkout feature/ajout-client

# Créer ET changer de branche en une commande
git checkout -b feature/ajout-client
```

**Résultat :**
```
* feature/ajout-client
  main
```

L'étoile (*) indique votre branche actuelle.

### Travailler sur une branche

Une fois sur votre branche, travaillez normalement :

```bash
# Modifier des fichiers
# ...

# Commiter
git add .  
git commit -m "feat: Ajout du formulaire client"  

git add .  
git commit -m "feat: Validation des champs client"  
```

Ces commits sont **uniquement** sur la branche `feature/ajout-client`. La branche `main` n'est pas affectée.

### Visualiser les branches

```bash
# Voir l'historique avec branches
git log --oneline --graph --all
```

**Résultat :**
```
* 7b8c9d2 (feature/ajout-client) feat: Validation des champs client
* 4e5f6a7 feat: Ajout du formulaire client
| * 2c3d4e5 (main) fix: Correction bug grille
|/
* 1a2b3c4 Initial commit
```

### Fusionner une branche (Merge)

Quand votre fonctionnalité est terminée, fusionnez-la dans `main` :

```bash
# 1. Retourner sur main
git checkout main

# 2. Fusionner la branche feature
git merge feature/ajout-client

# 3. Supprimer la branche (optionnel)
git branch -d feature/ajout-client
```

**Résultat :**
```
Updating 1a2b3c4..7b8c9d2  
Fast-forward  
 Source/ClientForm.pas | 150 ++++++++++++++++++++++++++
 Source/ClientForm.dfm | 85 +++++++++++++++
 2 files changed, 235 insertions(+)
```

### Stratégies de branching

#### Git Flow (pour projets structurés)

```
main
  │
  ├─── develop (branche de développement)
  │      │
  │      ├─── feature/fonction-1
  │      ├─── feature/fonction-2
  │      └─── feature/fonction-3
  │
  ├─── release/v1.0 (préparation release)
  │
  └─── hotfix/bug-urgent (corrections urgentes)
```

#### GitHub Flow (plus simple)

```
main
  │
  ├─── feature/nouvelle-fonction
  ├─── fix/correction-bug
  └─── refactor/amelioration
```

**Pour débuter, utilisez GitHub Flow :**
1. `main` est toujours stable et déployable
2. Créez une branche pour chaque fonctionnalité/correction
3. Fusionnez dans `main` quand terminé

### Résolution de conflits

Un **conflit** se produit quand deux personnes modifient la même ligne de code.

**Exemple de conflit :**

```pascal
<<<<<<< HEAD
ShowMessage('Version principale');
=======
ShowMessage('Version de la branche');
>>>>>>> feature/modification
```

**Résoudre le conflit :**

1. Ouvrez le fichier
2. Choisissez quelle version garder (ou combinez-les)
3. Supprimez les marqueurs `<<<<<<<`, `=======`, `>>>>>>>`
4. Sauvegardez le fichier

```pascal
// Après résolution
ShowMessage('Version finale combinée');
```

5. Marquez comme résolu et commitez :

```bash
git add Source/MainForm.pas  
git commit -m "Résolution du conflit dans MainForm"  
```

**Dans une GUI :**
- Les outils comme GitKraken ou SourceTree ont des interfaces visuelles pour résoudre les conflits
- Plus facile pour les débutants

## Travail avec un repository distant

Jusqu'ici, nous avons travaillé en local. Pour collaborer et sauvegarder sur le cloud, utilisons un repository distant.

### Plateformes populaires

**GitHub** (https://github.com)
- Le plus populaire
- Gratuit pour les projets publics et privés
- Excellente intégration avec les outils

**GitLab** (https://gitlab.com)
- Alternative complète à GitHub
- CI/CD intégré
- Peut être auto-hébergé

**Bitbucket** (https://bitbucket.org)
- De la société Atlassian
- Intégré avec Jira

### Créer un repository sur GitHub

1. Connectez-vous à GitHub
2. Cliquez sur "New repository"
3. Nommez votre repository (ex: "MonAppliDelphi")
4. Choisissez Public ou Private
5. N'initialisez PAS avec README (vous avez déjà un projet local)
6. Cliquez sur "Create repository"

GitHub vous donne les commandes à exécuter :

```bash
# Ajouter le remote
git remote add origin https://github.com/votre-nom/MonAppliDelphi.git

# Pousser votre code
git push -u origin main
```

### Commandes remote

#### Clone (Cloner un repository)

Pour télécharger un projet existant :

```bash
git clone https://github.com/utilisateur/projet.git
```

Cela crée un dossier avec tout le code et l'historique.

#### Push (Pousser vos modifications)

Pour envoyer vos commits locaux vers GitHub :

```bash
# Pousser la branche actuelle
git push

# Pousser une branche spécifique
git push origin feature/ma-branche

# Première fois (définir le upstream)
git push -u origin main
```

#### Pull (Récupérer les modifications)

Pour télécharger les modifications des autres :

```bash
# Récupérer et fusionner
git pull

# Équivalent à:
git fetch  # Télécharger  
git merge  # Fusionner  
```

#### Fetch (Télécharger sans fusionner)

```bash
# Télécharger les modifications sans les appliquer
git fetch

# Voir les différences
git diff origin/main

# Fusionner manuellement si désiré
git merge origin/main
```

### Workflow de collaboration

**Scénario typique :**

1. **Cloner le projet**
```bash
git clone https://github.com/equipe/projet.git  
cd projet  
```

2. **Créer une branche pour votre fonctionnalité**
```bash
git checkout -b feature/ma-fonctionnalite
```

3. **Travailler et commiter**
```bash
# Modifications...
git add .  
git commit -m "feat: Ajout de ma fonctionnalité"  
```

4. **Récupérer les dernières modifications**
```bash
git checkout main  
git pull  
git checkout feature/ma-fonctionnalite  
git merge main  # Fusionner les dernières modifs dans votre branche  
```

5. **Pousser votre branche**
```bash
git push -u origin feature/ma-fonctionnalite
```

6. **Créer une Pull Request sur GitHub**
   - Allez sur GitHub
   - Cliquez sur "Compare & pull request"
   - Décrivez vos modifications
   - Demandez une revue de code

7. **Après approbation, fusionner**
   - Cliquez sur "Merge pull request"
   - Supprimez la branche (optionnel)

8. **Nettoyer localement**
```bash
git checkout main  
git pull  
git branch -d feature/ma-fonctionnalite  
```

## Commandes Git essentielles

Voici un récapitulatif des commandes les plus utilisées :

### Configuration

```bash
git config --global user.name "Nom"  
git config --global user.email "email@example.com"  
git config --list  
```

### Créer et cloner

```bash
git init                           # Créer un nouveau repo  
git clone <url>                    # Cloner un repo existant  
```

### Changements locaux

```bash
git status                         # État des fichiers  
git diff                          # Voir les modifications  
git add <fichier>                 # Ajouter un fichier  
git add .                         # Ajouter tous les fichiers  
git commit -m "message"           # Créer un commit  
git commit -am "message"          # Ajouter et commiter en une commande  
```

### Historique

```bash
git log                           # Historique complet  
git log --oneline                 # Historique compact  
git log --oneline --graph --all   # Avec graphique des branches  
git show <commit>                 # Détails d'un commit  
```

### Branches

```bash
git branch                        # Lister les branches  
git branch <nom>                  # Créer une branche  
git checkout <branche>            # Changer de branche  
git checkout -b <branche>         # Créer et changer de branche  
git merge <branche>               # Fusionner une branche  
git branch -d <branche>           # Supprimer une branche  
```

### Remote (distant)

```bash
git remote add origin <url>       # Ajouter un remote  
git remote -v                     # Voir les remotes  
git push                          # Pousser les commits  
git push -u origin <branche>      # Pousser et définir upstream  
git pull                          # Récupérer et fusionner  
git fetch                         # Récupérer sans fusionner  
```

### Annuler des changements

```bash
git checkout -- <fichier>         # Annuler les modifs d'un fichier  
git reset HEAD <fichier>          # Retirer de la staging area  
git reset --soft HEAD~1           # Annuler le dernier commit (garde les changements)  
git reset --hard HEAD~1           # Annuler le dernier commit (perd les changements)  
git revert <commit>               # Créer un commit qui annule un commit précédent  
```

### Autres

```bash
git stash                         # Mettre de côté les modifications  
git stash pop                     # Récupérer les modifications  
git tag v1.0.0                    # Créer un tag  
git tag -a v1.0.0 -m "Version 1.0" # Tag avec annotation  
```

## Tags et Releases

Les **tags** marquent des points spécifiques dans l'historique, typiquement pour les versions.

### Créer un tag

```bash
# Tag simple
git tag v1.0.0

# Tag avec message
git tag -a v1.0.0 -m "Version 1.0 - Première release stable"

# Voir les tags
git tag

# Voir les détails d'un tag
git show v1.0.0
```

### Pousser les tags

```bash
# Pousser un tag spécifique
git push origin v1.0.0

# Pousser tous les tags
git push --tags
```

### Convention de versioning

Utilisez le **Semantic Versioning** (SemVer) : `MAJOR.MINOR.PATCH`

- **MAJOR** : Changements incompatibles (breaking changes)
- **MINOR** : Nouvelles fonctionnalités (compatibles)
- **PATCH** : Corrections de bugs

**Exemples :**
- `1.0.0` : Première version stable
- `1.1.0` : Ajout de fonctionnalités
- `1.1.1` : Correction de bug
- `2.0.0` : Changements majeurs incompatibles

### Créer une Release sur GitHub

1. Allez sur votre repository GitHub
2. Cliquez sur "Releases"
3. Cliquez sur "Create a new release"
4. Choisissez un tag (ou créez-en un)
5. Ajoutez des notes de release
6. Attachez des binaires compilés (optionnel)
7. Cliquez sur "Publish release"

**Exemple de notes de release :**

```markdown
## Version 1.2.0 - 2025-03-15

### Nouvelles fonctionnalités
- Ajout du module de gestion des clients
- Export Excel des rapports
- Thème sombre

### Améliorations
- Performance de la grille améliorée de 40%
- Interface utilisateur modernisée

### Corrections
- Correction du bug d'affichage des dates
- Correction de la sauvegarde des préférences

### Notes techniques
- Nécessite Delphi 13 ou supérieur
- Compatible Windows 10/11
```

## Intégration de Git avec Delphi

### Version Control dans l'IDE Delphi

Delphi supporte l'intégration avec Git :

1. **Tools → Options → Version Control → Git**
2. Configurez le chemin vers `git.exe`
3. Activez l'intégration

**Fonctionnalités disponibles :**
- Voir l'état des fichiers dans le Project Manager
- Commit depuis l'IDE
- Diff intégré
- Historique

### Workflow recommandé

Bien que Delphi ait une intégration Git, beaucoup de développeurs préfèrent :

1. **Utiliser l'IDE pour coder**
2. **Utiliser une GUI Git séparée** (GitKraken, SourceTree) pour le versionnement
3. **Ou utiliser la ligne de commande** dans un terminal séparé

**Avantages :**
- Plus de contrôle
- Meilleure visualisation
- Moins de risque d'erreurs

### Fichiers Delphi et Git

**Fichiers à versionner :**
- ✅ `.dpr`, `.dproj` (fichiers projet)
- ✅ `.pas`, `.dfm` (unités et formulaires)
- ✅ `.res` (ressources)
- ✅ `.dpk` (packages)
- ✅ `.groupproj` (groupe de projets)

**Fichiers à ignorer :**
- ❌ `.dcu` (unités compilées)
- ❌ `.exe`, `.dll` (exécutables)
- ❌ `__history/` (historique local Delphi)
- ❌ `Win32/`, `Win64/` (dossiers de build)

### Gestion des fichiers binaires

Les `.dfm` peuvent être stockés en texte ou binaire. Pour Git, préférez le texte.

**Dans Delphi :**
1. Tools → Options → Environment Options → VCL Designer
2. Cochez "Text DFM"

**Ou via le Project Options :**
1. Project → Options → Form
2. Choisissez "Text" pour "Form file format"

**Avantage :** Les `.dfm` en texte peuvent être diffés et mergés facilement.

### Ignorer les fichiers locaux

Certains fichiers sont spécifiques à votre machine :

```gitignore
# Configuration locale
*.local
*.identcache
*.dsk

# Chemins absolus dans les projets
*.stat
*.dsk
```

**Important :** Ne commitez jamais :
- Chemins absolus
- Mots de passe
- Configurations locales

## Bonnes pratiques

### 1. Commitez souvent

- ✅ Petits commits fréquents
- ❌ Gros commits rares

**Pourquoi ?**
- Plus facile à revoir
- Plus facile à annuler si problème
- Historique plus clair

### 2. Messages descriptifs

```bash
# Mauvais
git commit -m "fix"

# Bon
git commit -m "fix: Correction du calcul de TVA dans la facture"
```

### 3. Une fonctionnalité = Une branche

Ne mélangez pas plusieurs fonctionnalités dans une même branche.

```bash
# Mauvais
git checkout -b tout-en-vrac
# ... ajout client + correction bug + refactoring ...

# Bon
git checkout -b feature/ajout-client
# ... seulement ajout client ...

git checkout -b fix/bug-grille
# ... seulement correction du bug ...
```

### 4. Synchronisez régulièrement

```bash
# Au moins une fois par jour
git pull
```

Cela évite les gros conflits difficiles à résoudre.

### 5. Ne commitez pas de fichiers générés

Utilisez `.gitignore` dès le début. Ne commitez que le code source.

### 6. Testez avant de pousser

```bash
# Compilez et testez avant de:
git push
```

Ne poussez pas de code qui ne compile pas.

### 7. Revue de code (Pull Requests)

Avant de fusionner dans `main`, faites relire votre code par un collègue via une Pull Request.

**Avantages :**
- Détection d'erreurs
- Partage de connaissances
- Amélioration de la qualité

### 8. Protégez la branche main

Sur GitHub, configurez des protections :

1. Settings → Branches → Branch protection rules
2. Cochez :
   - "Require pull request reviews before merging"
   - "Require status checks to pass before merging"
   - "Include administrators"

Cela empêche les pushs directs sur `main`.

### 9. Documentation

Maintenez un fichier `README.md` à jour :

```markdown
# Mon Application Delphi

Description de l'application

## Installation

1. Cloner le repository
2. Ouvrir MonProjet.dpr dans Delphi
3. Compiler et exécuter

## Configuration

Copiez `config.template.ini` en `config.ini` et configurez...

## Contribution

Voir [CONTRIBUTING.md](CONTRIBUTING.md)

## Licence

MIT
```

### 10. Utilisez des issues

Pour tracker les bugs et fonctionnalités :

1. GitHub → Issues → New Issue
2. Décrivez le problème ou la fonctionnalité
3. Assignez à quelqu'un
4. Liez les commits/PR à l'issue

```bash
# Dans le message de commit
git commit -m "fix: Correction bug grille #42"
# Le #42 crée un lien vers l'issue #42
```

## Situations courantes

### J'ai modifié des fichiers par erreur

```bash
# Annuler les modifications d'un fichier
git checkout -- MonFichier.pas

# Annuler toutes les modifications
git checkout -- .
```

### J'ai commité sur la mauvaise branche

```bash
# 1. Annuler le commit (garde les changements)
git reset --soft HEAD~1

# 2. Créer/changer vers la bonne branche
git checkout -b bonne-branche

# 3. Recommiter
git commit -m "Message"
```

### J'ai commité un fichier sensible

```bash
# 1. Retirer le fichier de Git (garde le fichier local)
git rm --cached config.prod.ini

# 2. Ajouter au .gitignore
echo "config.prod.ini" >> .gitignore

# 3. Commiter
git add .gitignore  
git commit -m "Suppression fichier sensible de Git"  

# 4. Si déjà poussé, nettoyer l'historique (complexe)
# Utilisez git filter-branch ou BFG Repo-Cleaner
```

### Je veux revenir à une version antérieure

```bash
# Voir l'historique
git log --oneline

# Revenir temporairement à un commit
git checkout abc123

# Créer une branche depuis ce point
git checkout -b retour-version-anterieure

# Ou annuler les commits récents
git reset --hard abc123  # ATTENTION: Perte des changements
```

### Mon collègue a poussé, j'ai des conflits

```bash
# 1. Récupérer les changements
git pull

# 2. Si conflits, Git vous le dit
# 3. Ouvrir les fichiers en conflit
# 4. Résoudre manuellement
# 5. Marquer comme résolu
git add FichierResolu.pas

# 6. Finir le merge
git commit -m "Résolution conflits"
```

### Je veux mettre de côté mes modifications

```bash
# Mettre de côté (stash)
git stash

# Travailler sur autre chose...

# Récupérer
git stash pop

# Voir la liste des stash
git stash list

# Appliquer un stash spécifique
git stash apply stash@{0}
```

## Ressources et outils

### Apprendre Git

**Tutoriels interactifs :**
- https://learngitbranching.js.org/ - Apprendre Git visuellement
- https://try.github.io/ - Tutoriels GitHub

**Documentation :**
- https://git-scm.com/doc - Documentation officielle
- https://www.atlassian.com/git/tutorials - Tutoriels Atlassian

**Cheat sheets :**
- https://education.github.com/git-cheat-sheet-education.pdf

### Outils Git

**Clients GUI :**
- GitKraken (https://www.gitkraken.com/)
- SourceTree (https://www.sourcetreeapp.com/)
- GitHub Desktop (https://desktop.github.com/)
- TortoiseGit (https://tortoisegit.org/)

**Intégrations IDE :**
- Git Extensions for Visual Studio/Delphi
- Plugin Git pour RAD Studio

**En ligne de commande :**
- Git Bash (inclus avec Git pour Windows)
- PowerShell avec posh-git

### Services d'hébergement

- **GitHub** (https://github.com) - Le plus populaire
- **GitLab** (https://gitlab.com) - Alternative complète
- **Bitbucket** (https://bitbucket.org) - Par Atlassian
- **Azure DevOps** (https://dev.azure.com) - Par Microsoft
- **Gitea** (https://gitea.io) - Auto-hébergeable

## Exemple complet : Workflow d'une journée

Voici un scénario réaliste d'une journée de travail :

### Matin : Démarrage

```bash
# 1. Récupérer les dernières modifications
cd C:\Projets\MonAppli  
git checkout main  
git pull  

# 2. Créer une branche pour la fonctionnalité du jour
git checkout -b feature/ajout-export-excel

# 3. Travailler dans Delphi
# ... codage ...
```

### Milieu de journée : Premier commit

```bash
# 4. Vérifier ce qui a changé
git status  
git diff  

# 5. Commiter les changements
git add Source/ExportExcel.pas Source/ExportExcel.dfm  
git commit -m "feat: Création du module d'export Excel"  
```

### Après-midi : Continuation

```bash
# 6. Continuer à travailler
# ... plus de code ...

# 7. Autre commit
git add .  
git commit -m "feat: Ajout formatage des cellules Excel"  
```

### Fin de journée : Synchronisation

```bash
# 8. Pousser votre travail
git push -u origin feature/ajout-export-excel

# 9. Créer une Pull Request sur GitHub
# (Via l'interface web)

# 10. Revenir sur main pour le lendemain
git checkout main
```

### Lendemain : Fusion et suite

```bash
# 11. Pull Request approuvée et fusionnée (via GitHub)

# 12. Mettre à jour votre repo local
git checkout main  
git pull  

# 13. Supprimer la branche locale
git branch -d feature/ajout-export-excel

# 14. Nouvelle fonctionnalité
git checkout -b feature/nouvelle-fonctionnalite
```

## Conclusion

Le versionnement avec Git est une compétence essentielle pour tout développeur moderne. Bien que la courbe d'apprentissage puisse sembler raide au début, les bénéfices sont immenses :

**Avantages principaux :**

1. **Historique complet** - Jamais de perte de code
2. **Collaboration efficace** - Travaillez en équipe sans friction
3. **Branches puissantes** - Expérimentez sans risque
4. **Sauvegarde cloud** - Code sécurisé sur GitHub/GitLab
5. **Professionnel** - Standard de l'industrie

**Pour débuter :**

1. Installez Git et créez un compte GitHub
2. Créez un `.gitignore` adapté à Delphi
3. Commencez avec les commandes de base : `add`, `commit`, `push`, `pull`
4. Utilisez une GUI si la ligne de commande vous intimide
5. Pratiquez sur vos projets personnels
6. Progressez vers les branches et la collaboration

**Commandes essentielles à retenir :**

```bash
git init                    # Créer un repo  
git clone <url>            # Cloner un repo  
git status                 # État des fichiers  
git add .                  # Ajouter tout  
git commit -m "message"    # Commiter  
git push                   # Pousser  
git pull                   # Récupérer  
git checkout -b <branche>  # Créer une branche  
git merge <branche>        # Fusionner  
```

**N'ayez pas peur de faire des erreurs !** Git permet presque toujours de revenir en arrière. L'important est de commencer à l'utiliser et d'apprendre progressivement.

Avec Git, vous ne perdrez plus jamais de code, et la collaboration avec d'autres développeurs deviendra un plaisir plutôt qu'un cauchemar. C'est un investissement en temps qui rapporte très rapidement.

Dans la prochaine section, nous explorerons la documentation du code, un autre aspect crucial de la qualité logicielle.

⏭️ [Documentation du code](/18-architecture-et-bonnes-pratiques/06-documentation-du-code.md)
