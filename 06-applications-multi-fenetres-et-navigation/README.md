🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 6 : Applications multi-fenêtres et navigation

## Introduction

Vous avez maintenant acquis les bases du langage Object Pascal et de l'interface utilisateur avec un seul formulaire. Mais les applications réelles sont rarement limitées à une seule fenêtre. La plupart des logiciels professionnels nécessitent plusieurs écrans, des boîtes de dialogue, des menus de navigation et une gestion intelligente du flux de travail de l'utilisateur.

Ce chapitre vous guidera dans l'art de créer des applications complexes et ergonomiques en maîtrisant la navigation entre différentes fenêtres et la communication entre elles. Que vous développiez une application de bureau traditionnelle ou une application mobile moderne, ces compétences sont essentielles.

## Pourquoi ce chapitre est important

### Les limites d'une application à fenêtre unique

Imaginez une application de gestion de clients où tout se trouve sur un seul écran : la liste des clients, leurs détails, l'historique des commandes, les statistiques, les paramètres... L'interface deviendrait rapidement surchargée, confuse et difficile à utiliser.

En répartissant les fonctionnalités sur plusieurs fenêtres ou écrans, vous créez une expérience utilisateur :
- **Plus claire** : Chaque fenêtre a un objectif précis
- **Plus organisée** : Les informations sont structurées logiquement
- **Plus intuitive** : L'utilisateur comprend mieux le flux de travail
- **Plus professionnelle** : Votre application ressemble à un vrai logiciel

### De la simplicité à la complexité

Ce chapitre est structuré pour vous faire progresser naturellement :

1. **D'abord les bases** : Comment créer et afficher plusieurs formulaires
2. **Puis la communication** : Comment faire dialoguer ces formulaires entre eux
3. **Ensuite l'organisation** : Les patterns classiques (MDI, dialogues, assistants)
4. **Et enfin le mobile** : Adapter ces concepts aux applications mobiles

## Ce que vous allez apprendre

### 1. Gestion des formulaires multiples

Vous apprendrez à :
- Créer plusieurs formulaires dans un projet
- Afficher et masquer des fenêtres dynamiquement
- Choisir entre affichage modal et non-modal
- Gérer correctement la mémoire et le cycle de vie des formulaires
- Organiser votre code pour des projets à plusieurs fenêtres

**Exemple concret** : Une application où un formulaire principal affiche une liste de clients, et un clic sur un client ouvre un formulaire de détails.

### 2. Communication entre formulaires

Les formulaires ne sont pas des îles isolées - ils doivent pouvoir échanger des informations :
- Passer des données d'un formulaire à un autre
- Notifier un formulaire parent d'un changement dans un formulaire enfant
- Utiliser les événements pour un couplage faible
- Mettre en place des patterns de communication propres et maintenables

**Exemple concret** : Un formulaire de saisie qui envoie les données validées au formulaire principal pour mise à jour de la liste.

### 3. Formulaires MDI (Multiple Document Interface)

L'interface MDI permet d'avoir plusieurs fenêtres enfants à l'intérieur d'une fenêtre parent :
- Comprendre le pattern MDI et ses cas d'utilisation
- Créer une fenêtre parent MDI
- Gérer plusieurs documents simultanément
- Organiser automatiquement les fenêtres (cascade, mosaïque)
- Connaître les alternatives modernes au MDI

**Exemple concret** : Un éditeur de texte permettant d'ouvrir plusieurs documents en même temps dans une seule fenêtre principale.

### 4. Boîtes de dialogue standard et personnalisées

Les dialogues sont essentiels pour interagir avec l'utilisateur :
- Utiliser les boîtes de dialogue Windows standard (ouvrir fichier, enregistrer, couleurs, polices)
- Créer des messages personnalisés (confirmation, erreur, information)
- Construire vos propres boîtes de dialogue pour des besoins spécifiques
- Gérer les valeurs de retour et la validation des saisies

**Exemple concret** : Une boîte de dialogue personnalisée pour saisir les informations d'un nouveau client avec validation des données.

### 5. Assistants (Wizards)

Les assistants guident l'utilisateur à travers un processus complexe étape par étape :
- Concevoir un workflow en plusieurs étapes
- Créer une navigation intuitive (Précédent/Suivant/Terminer)
- Valider chaque étape avant de continuer
- Afficher une progression visuelle
- Gérer les cas particuliers (étapes conditionnelles)

**Exemple concret** : Un assistant de configuration initiale qui guide le nouvel utilisateur à travers les paramètres essentiels de l'application.

### 6. Navigation dans les applications mobiles

Le mobile impose ses propres règles de navigation :
- Comprendre les différences entre desktop et mobile
- Utiliser TTabControl pour la navigation par onglets
- Implémenter une navigation hiérarchique avec historique
- Gérer les gestes tactiles et les transitions animées
- Respecter les conventions iOS et Android

**Exemple concret** : Une application mobile avec une barre de navigation en bas, des pages qui glissent lors du changement d'onglet, et un bouton retour qui fonctionne correctement.

### 7. Gestion de l'état de l'application

Préserver et restaurer l'état de votre application est crucial pour l'expérience utilisateur :
- Sauvegarder la position et la taille des fenêtres
- Mémoriser les préférences utilisateur
- Détecter les modifications non sauvegardées
- Gérer les sessions utilisateur
- Gérer les interruptions sur mobile (appels, notifications)

**Exemple concret** : Une application qui se souvient de l'onglet ouvert, de la position de défilement et des filtres appliqués, même après redémarrage.

## Concepts clés à maîtriser

### Modal vs Non-Modal

**Modal** : L'utilisateur doit fermer cette fenêtre avant d'interagir avec le reste de l'application
- Utilisation : Dialogues importants, saisies obligatoires, confirmations
- Méthode : `ShowModal`

**Non-Modal** : L'utilisateur peut librement passer d'une fenêtre à l'autre
- Utilisation : Fenêtres d'outils, palettes, fenêtres secondaires
- Méthode : `Show`

### Propriétaire (Owner) et Parent

**Owner** : Responsable de la destruction de l'objet
```pascal
FormEnfant := TFormEnfant.Create(Self); // Self est le propriétaire
```

**Parent** : Responsable de l'affichage visuel (pour les composants)
```pascal
Button := TButton.Create(Self);  
Button.Parent := Panel1; // S'affiche dans le panel  
```

### Cycle de vie d'un formulaire

1. **Création** : `Create` - Le formulaire est créé en mémoire
2. **Affichage** : `Show` ou `ShowModal` - Le formulaire devient visible
3. **Événements** : `OnCreate`, `OnShow`, `OnActivate` - Initialisation
4. **Utilisation** : L'utilisateur interagit avec le formulaire
5. **Fermeture** : `Close` - L'utilisateur ferme le formulaire
6. **Événements** : `OnClose`, `OnCloseQuery` - Validation avant fermeture
7. **Destruction** : `Free` - Libération de la mémoire

### Navigation et UX

**Sur desktop** :
- Menus et barres d'outils
- Fenêtres multiples
- Raccourcis clavier
- Clics de souris

**Sur mobile** :
- Navigation tactile
- Gestes (swipe, tap, pinch)
- Boutons de navigation
- Transitions animées

## Organisation de vos projets

### Structure recommandée

```
MonProjet/
├── Forms/
│   ├── Main/
│   │   └── UnitMain.pas          (Formulaire principal)
│   ├── Dialogs/
│   │   ├── UnitLogin.pas         (Dialogue de connexion)
│   │   └── UnitOptions.pas       (Dialogue d'options)
│   ├── Wizards/
│   │   └── UnitSetupWizard.pas   (Assistant de configuration)
│   └── Details/
│       ├── UnitClientDetail.pas  (Détail client)
│       └── UnitOrderDetail.pas   (Détail commande)
├── Data/
│   └── UnitDataModule.pas        (Accès aux données)
├── Business/
│   └── UnitBusinessLogic.pas     (Logique métier)
└── Utils/
    └── UnitHelpers.pas            (Fonctions utilitaires)
```

### Bonnes pratiques dès le départ

1. **Un formulaire = une responsabilité** : Ne mettez pas tout dans le formulaire principal
2. **Nommez clairement** : `FormClientDetail` plutôt que `Form2`
3. **Séparez la logique** : L'interface ne doit pas contenir la logique métier
4. **Documentez** : Expliquez le rôle de chaque formulaire
5. **Testez** : Vérifiez tous les chemins de navigation

## Différences Desktop vs Mobile

### Applications Desktop (VCL)

**Caractéristiques** :
- Fenêtres multiples indépendantes
- Redimensionnables
- Menus traditionnels
- Souris et clavier
- Écran large

**Approche** :
- Plusieurs formulaires `TForm`
- Affichage modal et non-modal
- MDI pour documents multiples
- Dialogues Windows standard

### Applications Mobile (FireMonkey)

**Caractéristiques** :
- Application plein écran
- Pas de fenêtres multiples
- Navigation tactile
- Interface simplifiée
- Écran limité

**Approche** :
- Un formulaire principal
- Navigation par `TTabControl`
- Transitions animées
- Gestes tactiles
- États et historique

## Prérequis pour ce chapitre

Avant de commencer, assurez-vous de maîtriser :

✓ Création d'un formulaire simple avec des composants  
✓ Gestion des événements (OnClick, OnChange, etc.)  
✓ Syntaxe Object Pascal de base  
✓ Création et manipulation d'objets  
✓ Utilisation de l'IDE Delphi

Si ces concepts ne sont pas clairs, n'hésitez pas à revenir aux chapitres précédents.

## Comment aborder ce chapitre

### Pour les débutants

Suivez l'ordre des sections. Chaque concept s'appuie sur le précédent :
1. Commencez par comprendre comment gérer plusieurs formulaires
2. Apprenez ensuite à les faire communiquer
3. Explorez les patterns classiques (MDI, dialogues, assistants)
4. Si vous visez le mobile, étudiez attentivement la section navigation mobile
5. Terminez par la gestion de l'état pour consolider

### Pour les développeurs expérimentés

Vous pouvez :
- Parcourir rapidement les sections de base (6.1, 6.2)
- Vous concentrer sur les bonnes pratiques et patterns
- Explorer en profondeur la navigation mobile si c'est nouveau pour vous
- Utiliser les sections comme référence pour des cas spécifiques

### Approche pratique

**Pour chaque section** :
1. Lisez les concepts et exemples
2. Créez un petit projet de test
3. Expérimentez avec le code fourni
4. Modifiez pour tester votre compréhension
5. Notez les pièges et astuces découverts

## Ce que vous saurez faire après ce chapitre

À la fin de ce chapitre, vous serez capable de :

✓ Créer des applications multi-fenêtres complexes et organisées  
✓ Faire communiquer efficacement vos formulaires  
✓ Choisir le pattern de navigation adapté à vos besoins  
✓ Créer des boîtes de dialogue professionnelles  
✓ Guider l'utilisateur avec des assistants intuitifs  
✓ Développer des applications mobiles avec une navigation fluide  
✓ Gérer correctement l'état et les préférences de votre application  
✓ Structurer vos projets de manière professionnelle

## Un dernier conseil avant de commencer

La navigation et la gestion des fenêtres peuvent sembler complexes au début, mais c'est comme apprendre à conduire : au début on réfléchit à chaque action, puis cela devient naturel.

**Ne vous découragez pas** si certains concepts semblent abstraits. Pratiquez avec de petits exemples, et vous verrez rapidement comment tout s'articule.

**Expérimentez** : Créez de petits projets de test. C'est en manipulant concrètement les formulaires que vous comprendrez vraiment leur fonctionnement.

**Pensez UX** : Toujours vous mettre à la place de l'utilisateur. Une bonne navigation est invisible - l'utilisateur ne devrait jamais se sentir perdu ou confus.

---

Maintenant que vous comprenez l'importance et l'étendue de ce chapitre, commençons par les fondamentaux avec la gestion des formulaires multiples.

⏭️ [Gestion des formulaires multiples](/06-applications-multi-fenetres-et-navigation/01-gestion-des-formulaires-multiples.md)
