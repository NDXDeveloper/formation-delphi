🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.1 Introduction à FireMonkey

## Qu'est-ce que FireMonkey ?

FireMonkey (souvent abrégé en **FMX**) est le framework de développement d'interfaces graphiques multi-plateformes de Delphi. Contrairement à la VCL (Visual Component Library) qui est spécifique à Windows, FireMonkey vous permet de créer une seule application qui peut fonctionner sur plusieurs systèmes d'exploitation différents.

### Un framework, plusieurs plateformes

Avec FireMonkey, vous pouvez développer votre application une seule fois et la déployer sur :

- **Windows** (32 bits et 64 bits)
- **macOS** (Intel et Apple Silicon)
- **iOS** (iPhone et iPad)
- **Android** (smartphones et tablettes)
- **Linux** (distributions 64 bits)

Cette approche multi-plateforme représente un gain de temps considérable : au lieu de développer cinq applications distinctes dans cinq langages différents, vous écrivez votre code une fois en Object Pascal et Delphi se charge de la compilation pour chaque plateforme cible.

## Pourquoi FireMonkey a été créé ?

Historiquement, Delphi était uniquement orienté Windows avec la VCL. Mais avec l'évolution du marché technologique et l'essor des appareils mobiles (smartphones, tablettes) ainsi que la diversification des systèmes d'exploitation, il est devenu nécessaire de proposer une solution permettant aux développeurs Delphi de cibler ces nouvelles plateformes.

FireMonkey a été conçu pour répondre à ce besoin en offrant :

1. **Un moteur de rendu graphique moderne** basé sur GPU (carte graphique)
2. **Une architecture indépendante du système d'exploitation**
3. **Une compatibilité avec les écrans tactiles**
4. **Une conception adaptative** pour différentes tailles d'écran
5. **Des performances optimisées** pour les applications mobiles et desktop

## Les principes fondamentaux de FireMonkey

### Rendu vectoriel et GPU

FireMonkey utilise un système de rendu vectoriel accéléré par GPU. Contrairement à la VCL qui s'appuie sur les API graphiques de Windows (GDI/GDI+), FireMonkey dessine lui-même tous les composants en utilisant la puissance de la carte graphique.

**Ce que cela signifie pour vous :**
- Les interfaces sont fluides et modernes
- Les animations sont naturellement supportées
- L'apparence est cohérente sur toutes les plateformes
- Les composants peuvent être facilement stylisés et personnalisés

### Abstraction des API natives

FireMonkey fait abstraction des différences entre les systèmes d'exploitation. Il traduit vos composants et votre code en appels natifs pour chaque plateforme :

- Sur Windows, il utilise DirectX ou GDI+
- Sur macOS et iOS, il utilise Metal ou OpenGL
- Sur Android, il utilise OpenGL ES
- Sur Linux, il utilise OpenGL

En tant que développeur, vous n'avez pas à vous soucier de ces détails techniques. Vous placez un bouton dans votre interface, et FireMonkey s'occupe de le rendre correctement sur chaque plateforme.

## Quand utiliser FireMonkey ?

FireMonkey est particulièrement adapté dans les situations suivantes :

### 1. Applications multi-plateformes

Si vous devez distribuer votre application sur plusieurs systèmes d'exploitation, FireMonkey est le choix évident. Un exemple typique serait une application de gestion qui doit fonctionner aussi bien sur les ordinateurs Windows du bureau que sur les tablettes Android des commerciaux sur le terrain.

### 2. Applications mobiles

Pour développer des applications iOS et Android, FireMonkey est votre seule option dans l'écosystème Delphi. Il offre l'accès aux fonctionnalités spécifiques aux mobiles comme :
- Les capteurs (GPS, accéléromètre, gyroscope)
- L'appareil photo et la galerie de photos
- Les notifications push
- Le tactile et les gestes
- Les services de localisation

### 3. Interfaces modernes et graphiques

Si votre application nécessite :
- Des animations fluides
- Des effets visuels (ombres, flous, reflets)
- Des graphiques complexes ou des visualisations de données
- Une interface hautement personnalisée

FireMonkey offre des possibilités graphiques bien supérieures à la VCL traditionnelle.

### 4. Applications nécessitant un look uniforme

Certaines entreprises souhaitent que leur application ait exactement la même apparence sur Windows, macOS et Linux, plutôt que d'adopter le style natif de chaque plateforme. FireMonkey permet cette uniformité visuelle.

## Quand privilégier la VCL plutôt que FireMonkey ?

Il est important de noter que FireMonkey n'est pas toujours le meilleur choix :

### Applications Windows exclusivement

Si votre application est destinée uniquement à Windows et n'a aucune ambition multi-plateforme, la VCL reste souvent préférable car :
- Elle s'intègre parfaitement à l'apparence native de Windows
- Elle est plus mature et dispose de plus de composants tiers
- Elle est généralement plus légère en termes de ressources
- La documentation et les exemples sont plus abondants

### Applications nécessitant des composants Windows spécifiques

Certains composants ou fonctionnalités Windows n'ont pas d'équivalent en FireMonkey. Si votre application dépend fortement de composants VCL spécialisés, la migration vers FMX peut être complexe.

## L'architecture de FireMonkey

### Les formulaires (Forms)

Comme en VCL, l'élément de base d'une application FireMonkey est le formulaire (TForm en FMX). C'est le conteneur principal de votre interface utilisateur.

### Les composants visuels

FireMonkey propose une large gamme de composants visuels :
- **Composants de base** : TButton, TEdit, TLabel, TImage, etc.
- **Composants de mise en page** : TLayout, TPanel, TScrollBox, etc.
- **Composants de liste** : TListView, TListBox, TTreeView, etc.
- **Composants multimédias** : TMediaPlayer, TCamera, etc.
- **Composants graphiques** : TRectangle, TCircle, TPath, etc.

### Les styles

Un concept central de FireMonkey est le système de **styles**. Les styles définissent l'apparence visuelle des composants. Vous pouvez :
- Utiliser les styles prédéfinis (styles Windows, macOS, Android, iOS)
- Créer vos propres styles personnalisés
- Changer dynamiquement de style en fonction de la plateforme ou des préférences utilisateur

### Les effets visuels

FireMonkey intègre de nombreux effets visuels prêts à l'emploi :
- Ombres (Shadow, GlowEffect)
- Flou (BlurEffect, GaussianBlur)
- Reflets (ReflectionEffect)
- Transitions et animations
- Transformations 3D

Ces effets peuvent être appliqués facilement à n'importe quel composant depuis l'inspecteur d'objets.

## Premier contact avec FireMonkey

### Création d'un projet FireMonkey

Dans l'IDE Delphi :

1. Allez dans **Fichier → Nouveau → Application multi-périphériques** (ou **Multi-Device Application**)
2. Choisissez un modèle d'application (Application vide, Application principale-détail, etc.)
3. Delphi crée automatiquement un projet FMX avec un formulaire de départ

### Différences visuelles avec la VCL

Lorsque vous ouvrez le concepteur de formulaires FireMonkey, vous remarquerez quelques différences :

- **Palette d'outils** : Les composants portent souvent le même nom qu'en VCL mais sont préfixés par "T" de la même manière. Cependant, leurs propriétés peuvent différer.
- **Inspecteur d'objets** : Contient des propriétés spécifiques à FMX comme StyleLookup, Effects, Margins, Padding, etc.
- **Zone de conception** : Affiche par défaut une grille et permet de visualiser différentes tailles d'écran et orientations.

### Le modèle de plateforme

FireMonkey utilise un système de **plateforme cible** qui vous permet de :
- Prévisualiser votre interface pour différentes plateformes
- Définir des vues spécifiques par plateforme si nécessaire
- Configurer les paramètres de déploiement pour chaque système d'exploitation

## Performance et optimisation

### Applications desktop

Pour les applications Windows, macOS et Linux, FireMonkey offre d'excellentes performances, notamment grâce à l'accélération GPU. Les interfaces sont fluides et réactives.

### Applications mobiles

Sur mobile, quelques considérations sont importantes :
- **Mémoire** : Les appareils mobiles ont moins de RAM que les ordinateurs
- **Batterie** : L'utilisation intensive du GPU consomme de l'énergie
- **Résolution** : Les écrans mobiles ont des résolutions et des DPI variés

FireMonkey gère automatiquement beaucoup de ces aspects, mais il est important d'optimiser votre code et vos ressources (images, médias) pour garantir une expérience utilisateur optimale.

## Compatibilité du code

### Partage de code avec la VCL

Bien que FireMonkey et la VCL soient différents frameworks, vous pouvez partager une grande partie de votre code :

**Ce qui est partageable :**
- Toute la logique métier (calculs, algorithmes, traitement de données)
- Les classes non visuelles
- L'accès aux bases de données (FireDAC)
- La communication réseau
- Le traitement de fichiers

**Ce qui n'est pas directement partageable :**
- Les composants visuels (TButton VCL ≠ TButton FMX)
- Le code spécifique à l'interface utilisateur
- Les gestionnaires d'événements liés aux composants visuels

### Migration VCL vers FMX

Si vous avez une application VCL existante et souhaitez la porter vers FireMonkey, vous devrez :
- Recréer l'interface utilisateur avec les composants FMX
- Adapter le code qui interagit avec l'interface
- Conserver toute la logique métier qui fonctionne sans changement

Cette migration peut être progressive : certaines entreprises maintiennent deux versions de leur application (une VCL pour Windows, une FMX pour le multi-plateforme) en partageant la logique métier commune.

## L'écosystème FireMonkey

### Composants tiers

De nombreux éditeurs proposent des composants FireMonkey tiers pour étendre les capacités du framework :
- TMS Software
- DevExpress
- FMXExpress
- Components4Developers

Ces composants ajoutent des fonctionnalités avancées comme des grilles de données sophistiquées, des graphiques, des composants UI modernes, etc.

### Communauté et ressources

La communauté FireMonkey est active et propose :
- Des forums d'entraide
- Des tutoriels et exemples de code
- Des composants open source
- Des outils et bibliothèques gratuites

## Conclusion

FireMonkey représente une avancée majeure pour Delphi en permettant le développement multi-plateforme moderne. C'est un framework puissant et flexible qui ouvre de nombreuses possibilités :

**Points forts :**
- Véritable multi-plateforme (desktop et mobile)
- Interface moderne avec effets et animations
- Rendu GPU performant
- Un seul code source pour plusieurs cibles
- Support tactile natif

**Points à considérer :**
- Courbe d'apprentissage si vous venez de la VCL
- Moins de composants tiers que la VCL
- Fichiers exécutables généralement plus volumineux
- Nécessite de repenser l'approche UI pour les applications mobiles

Dans les sections suivantes, nous explorerons en détail comment créer des interfaces avec FireMonkey, comment gérer les spécificités de chaque plateforme, et comment optimiser vos applications pour offrir la meilleure expérience utilisateur possible.

FireMonkey est l'outil qui permet à Delphi de rester compétitif dans le paysage technologique actuel, où la mobilité et le multi-plateforme sont devenus essentiels. En maîtrisant FireMonkey, vous serez capable de développer des applications modernes pour pratiquement tous les appareils utilisés aujourd'hui.

⏭️ [Différences entre VCL et FMX](/05-developpement-multi-plateforme-avec-firemonkey/02-differences-entre-vcl-et-fmx.md)
