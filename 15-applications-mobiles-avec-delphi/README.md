🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15. Applications mobiles avec Delphi

## Introduction

Le monde du développement logiciel a connu une transformation majeure avec l'avènement des smartphones et tablettes. Aujourd'hui, des milliards de personnes dans le monde utilisent quotidiennement des applications mobiles pour communiquer, travailler, se divertir, gérer leurs finances, leur santé et bien plus encore. En tant que développeur, savoir créer des applications mobiles est devenu une compétence incontournable.

Delphi, avec sa longue histoire dans le développement d'applications de bureau, s'est parfaitement adapté à cette révolution mobile. Grâce à la technologie **FireMonkey (FMX)**, Delphi vous permet de développer des applications mobiles natives pour **iOS** et **Android** à partir d'une seule base de code source. Cette approche multi-plateforme vous offre un avantage considérable : vous pouvez toucher un public immense sans avoir à apprendre plusieurs langages de programmation ou à maintenir plusieurs versions de votre application.

## Pourquoi développer des applications mobiles avec Delphi ?

### Une seule base de code, plusieurs plateformes

L'un des atouts majeurs de Delphi pour le développement mobile est sa capacité à générer des applications **natives** pour iOS et Android à partir du même code Object Pascal. Contrairement aux solutions hybrides qui s'appuient sur des WebViews, Delphi compile votre code en applications véritablement natives qui offrent des performances optimales et un accès direct aux fonctionnalités du système d'exploitation.

Concrètement, cela signifie que vous écrivez votre code une seule fois et vous obtenez :
- Une application pour iPhone et iPad (iOS)
- Une application pour smartphones et tablettes Android
- Et même, si nécessaire, des versions Windows, macOS et Linux de la même application

### Réutilisation de vos compétences

Si vous avez déjà appris l'Object Pascal et développé des applications avec Delphi, vous possédez déjà l'essentiel des connaissances nécessaires pour créer des applications mobiles. Vous n'avez pas besoin d'apprendre Java/Kotlin pour Android ou Swift pour iOS. Votre expertise existante est directement transférable au monde mobile.

### Développement rapide (RAD)

Delphi conserve sa philosophie de **Rapid Application Development** (développement rapide d'applications) dans le domaine mobile. Grâce à l'éditeur visuel de FireMonkey, vous pouvez concevoir vos interfaces utilisateur de manière intuitive en glissant-déposant des composants, puis les tester immédiatement sur un émulateur ou un appareil réel.

Cette approche visuelle accélère considérablement le développement par rapport à des approches purement "code", tout en vous laissant la possibilité d'affiner chaque détail si nécessaire.

### Accès complet aux fonctionnalités natives

Delphi ne vous limite pas : vous avez accès à toutes les fonctionnalités modernes des smartphones :
- **Capteurs** : GPS, accéléromètre, gyroscope, boussole
- **Multimédia** : caméra, microphone, lecteur audio/vidéo
- **Connectivité** : WiFi, Bluetooth, NFC
- **Services** : notifications push, reconnaissance vocale, biométrie
- **Intégration** : calendrier, contacts, partage social

### Partage de code avec vos applications desktop

Un avantage unique de Delphi est la possibilité de partager une partie importante de votre code entre vos applications mobiles et desktop. La logique métier, l'accès aux données, les algorithmes peuvent être réutilisés, ce qui réduit considérablement les coûts de développement et de maintenance.

## L'écosystème mobile actuel

Avant d'entrer dans les détails techniques, il est important de comprendre le paysage du développement mobile.

### Les deux géants : iOS et Android

Le marché des applications mobiles est dominé par deux systèmes d'exploitation :

**iOS (Apple)** :
- Utilisé sur iPhone et iPad
- Système fermé et contrôlé par Apple
- Base d'utilisateurs généralement plus dépensière
- Standards de qualité élevés imposés par l'App Store

**Android (Google)** :
- Utilisé par de nombreux fabricants (Samsung, Huawei, Xiaomi, etc.)
- Système plus ouvert
- Part de marché mondiale plus importante
- Grande diversité d'appareils et de versions du système

Avec Delphi, vous ciblez ces deux plateformes simultanément, ce qui maximise votre portée potentielle.

### Le marché des applications mobiles

Le marché des applications est immense et continue de croître :
- Des millions d'applications disponibles sur l'App Store et le Play Store
- Des milliards de téléchargements chaque année
- De multiples modèles économiques : applications gratuites, payantes, freemium, abonnements, publicité

Que vous souhaitiez créer une application pour votre entreprise, lancer un produit commercial, ou simplement développer un outil personnel, Delphi vous donne les moyens d'y parvenir.

## Ce que vous apprendrez dans ce chapitre

Ce chapitre consacré aux applications mobiles avec Delphi est structuré de manière progressive pour vous accompagner depuis les concepts fondamentaux jusqu'à des techniques avancées.

### Fondamentaux du mobile

Vous découvrirez d'abord les **spécificités du développement mobile** qui le distinguent du développement desktop : les contraintes matérielles, les paradigmes d'interaction tactile, le cycle de vie des applications, et les systèmes de permissions.

### Interface utilisateur tactile

Vous apprendrez à concevoir des **interfaces adaptées au tactile** : dimensionnement approprié des contrôles, gestion des gestes naturels (swipe, pinch, tap), adaptation aux différentes orientations d'écran, et création d'expériences utilisateur fluides et intuitives.

### Fonctionnalités natives

Nous explorerons l'**accès aux capteurs** de l'appareil (GPS, accéléromètre, boussole) pour créer des applications géolocalisées ou réactives aux mouvements. Vous verrez également comment utiliser la **caméra et les médias** pour capturer des photos, enregistrer des vidéos, ou lire du contenu multimédia.

### Communication et synchronisation

Vous maîtriserez l'implémentation des **notifications** (locales et push) pour maintenir l'engagement de vos utilisateurs, ainsi que les techniques de **stockage local et synchronisation** pour que vos applications fonctionnent même hors ligne.

### Publication et distribution

Enfin, vous découvrirez les étapes nécessaires pour **publier vos applications** sur l'App Store d'Apple et le Play Store de Google, y compris la préparation des éléments marketing, les processus de validation, et les stratégies de mise à jour.

## Prérequis et outils nécessaires

Pour suivre efficacement ce chapitre, vous devriez :

### Connaissances

- Maîtriser les bases de l'Object Pascal (variables, structures de contrôle, fonctions)
- Avoir une compréhension de la programmation orientée objet
- Connaître les fondamentaux de FireMonkey (même si nous les réviserons dans un contexte mobile)

### Matériel et logiciels

**Pour développer pour Android** :
- Delphi 13 Florence (ou version récente)
- SDK Android installé via le SDK Manager de Delphi
- Un émulateur Android ou un appareil physique pour les tests

**Pour développer pour iOS** :
- Tout le nécessaire pour Android, plus :
- Un Mac (pour la compilation finale, peut être sur le même réseau)
- Xcode installé sur le Mac
- Un compte développeur Apple (gratuit pour les tests, payant pour publier sur l'App Store)
- Un iPhone ou iPad pour les tests sur appareil réel

**Note importante** : Même si vous n'avez pas accès à un Mac pour le moment, vous pouvez commencer à apprendre et développer pour Android. Les concepts sont largement transférables, et vous pourrez ajouter le support iOS plus tard.

## Structure de ce chapitre

Les sections suivantes de ce chapitre sont organisées pour construire progressivement vos compétences :

1. **Spécificités du développement mobile** — particularités du monde mobile, cycle de vie, contraintes matérielles
2. **Interface utilisateur tactile** — gestes, contrôles adaptés, orientations, ergonomie tactile
3. **Accès aux capteurs** — GPS, accéléromètre, gyroscope, boussole, luminosité
4. **Utilisation de la caméra et des médias** — photo, vidéo, son, galerie, permissions médias Android 13+
5. **Notifications** — notifications locales et notifications push (FCM HTTP v1, APNs)
6. **Stockage local et synchronisation** — préférences, JSON, SQLite via FireDAC, synchronisation client/serveur
7. **Publication sur App Store / Play Store** — signature, métadonnées, validation, ASO
8. **Mises à jour OTA (Over The Air)** — déploiement de contenu sans repasser par les stores, rollback, sécurité
9. **Partage de code entre applications mobile et desktop** — architecture en couches, MVVM, directives conditionnelles
10. **Permissions et confidentialité des données** — RGPD, App Tracking Transparency, hachage et chiffrement des secrets
11. **Intégration des services Firebase** — FCM, Analytics, Realtime Database, Authentication, Storage, Crashlytics

Chaque section inclut des explications théoriques, des exemples de code concrets, et des conseils pratiques basés sur les meilleures pratiques du développement mobile en 2026.

## Votre première application mobile

À la fin de ce chapitre, vous serez capable de créer votre propre application mobile complète avec Delphi, incluant :
- Une interface utilisateur moderne et réactive
- L'accès aux fonctionnalités natives de l'appareil
- La gestion des données locales et distantes
- Les notifications pour engager les utilisateurs
- Et la publication sur les stores officiels

Le développement mobile peut sembler intimidant au premier abord, mais avec Delphi et FireMonkey, vous disposez d'outils puissants qui simplifient grandement le processus. La courbe d'apprentissage est douce, et vous pourrez rapidement créer des applications fonctionnelles et attrayantes.

## Un mot d'encouragement

Le développement d'applications mobiles est une aventure passionnante. Chaque application que vous créerez sera potentiellement utilisée par des personnes du monde entier, directement dans leurs poches. C'est une opportunité unique de créer quelque chose de tangible et d'utile.

N'ayez pas peur de faire des erreurs ou d'expérimenter. Le développement mobile s'apprend par la pratique. Commencez par des projets simples, testez sur de vrais appareils, et augmentez progressivement la complexité de vos applications.

Delphi vous fournit un environnement robuste et professionnel pour ce voyage. Alors, sans plus attendre, plongeons dans le monde fascinant des applications mobiles !

---

Prêt à créer votre première application mobile ? Commençons par explorer les spécificités du développement mobile dans la section suivante.

⏭️ [Spécificités du développement mobile](/15-applications-mobiles-avec-delphi/01-specificites-du-developpement-mobile.md)
