🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.3 Bibliothèques et composants tiers

## Introduction

L'une des grandes forces de Delphi réside dans son écosystème riche en composants et bibliothèques développés par la communauté et des éditeurs spécialisés. Ces composants tiers peuvent considérablement accélérer votre développement en vous évitant de "réinventer la roue" et en vous donnant accès à des fonctionnalités avancées et testées par des milliers d'utilisateurs.

Dans cette section, nous allons explorer cet univers passionnant des composants tiers : où les trouver, comment les évaluer, les installer et les utiliser efficacement dans vos projets.

## Qu'est-ce qu'un composant tiers ?

### Définition simple

Un composant tiers est un élément logiciel (classe, bibliothèque, contrôle visuel) développé par une personne ou une entreprise autre qu'Embarcadero, que vous pouvez intégrer dans vos applications Delphi.

**Analogie** : Imaginez que vous construisez une maison. Delphi vous fournit les outils de base (marteau, scie, tournevis). Les composants tiers sont comme des éléments préfabriqués de qualité : une fenêtre haut de gamme, une porte blindée, un système de chauffage performant. Vous pourriez les fabriquer vous-même, mais utiliser ces éléments spécialisés vous fait gagner du temps et vous garantit souvent un meilleur résultat.

### Types de composants tiers

**Composants visuels** : Grilles de données avancées, graphiques, contrôles de saisie sophistiqués, calendriers, etc.

**Bibliothèques fonctionnelles** : Gestion de PDF, compression de fichiers, cryptographie, traitement d'images, etc.

**Frameworks complets** : Architectures pour REST, ORM (Object-Relational Mapping), injection de dépendances, etc.

**Outils de développement** : Générateurs de code, outils de refactoring, analyseurs de qualité de code, etc.

## Pourquoi utiliser des composants tiers ?

### Gain de temps considérable

Développer certaines fonctionnalités de zéro peut prendre des semaines voire des mois. Un composant tiers mature et bien conçu vous permet d'intégrer ces fonctionnalités en quelques heures.

**Exemple concret** : Créer un composant de graphique sophistiqué avec zoom, animations, export en différents formats pourrait vous prendre plusieurs semaines. Avec TeeChart (inclus dans Delphi) ou d'autres bibliothèques de graphiques, vous l'avez en quelques heures.

### Qualité et fiabilité

Les composants populaires ont été testés par des milliers d'utilisateurs dans des contextes variés. Ils sont généralement plus robustes et mieux optimisés que ce que vous pourriez développer rapidement.

### Fonctionnalités avancées

Certains composants offrent des fonctionnalités très spécialisées qui nécessiteraient une expertise pointue pour être développées en interne.

### Support et documentation

Les composants commerciaux incluent généralement un support technique et une documentation complète, ce qui peut être crucial pour des projets professionnels.

### Focus sur votre valeur ajoutée

En utilisant des composants pour les fonctionnalités standards, vous pouvez concentrer vos efforts sur la logique métier spécifique qui fait la valeur de votre application.

## Où trouver des composants tiers ?

### GetIt Package Manager (intégré dans Delphi)

**Qu'est-ce que c'est ?**
GetIt est le gestionnaire de packages officiel intégré directement dans l'IDE Delphi. C'est votre première source pour trouver des composants, bibliothèques et outils.

**Comment y accéder ?**
Dans Delphi : Menu **Tools** → **GetIt Package Manager**

**Points forts** :
- Installation en un clic
- Gestion automatique des dépendances
- Composants vérifiés par Embarcadero
- Mises à jour facilitées
- Désinstallation propre

**Contenu disponible** :
- Composants gratuits et commerciaux (versions d'essai)
- Exemples de code
- Styles visuels
- Outils de développement
- Templates de projets

**Pour les débutants** : Commencez toujours par explorer GetIt avant de chercher ailleurs. C'est la méthode la plus sûre et la plus simple.

### Sites web spécialisés

#### TorryDelphi
**URL** : http://www.torry.net/

L'une des plus anciennes et complètes collections de composants Delphi gratuits.

**Contenu** :
- Plus de 3000 composants et bibliothèques
- Classement par catégories
- Descriptions et liens de téléchargement
- Composants pour toutes les versions de Delphi

**Utilisation** : Excellent pour découvrir des solutions gratuites à des problèmes spécifiques.

#### Embarcadero Technology Partners
**URL** : https://www.embarcadero.com/partners

Le site officiel des partenaires technologiques d'Embarcadero.

**Contenu** :
- Composants commerciaux de qualité professionnelle
- Outils certifiés et validés
- Support garanti
- Informations sur les versions compatibles

#### DelphiComponents.com
**URL** : http://www.delphi-components.com/

Annuaire de composants avec filtres et recherche avancée.

### GitHub et dépôts open source

De nombreux développeurs partagent leurs bibliothèques sur GitHub.

**Comment rechercher** :
- Utilisez les mots-clés "Delphi" + nom de fonctionnalité
- Consultez les "stars" et l'activité du dépôt
- Vérifiez la date du dernier commit

**Avantages** :
- Code source accessible
- Possibilité de contribuer
- Souvent gratuit
- Communauté active dans les issues

**Exemples de recherche** :
- "Delphi REST client"
- "Delphi JSON parser"
- "Delphi SQLite wrapper"

### Forums et recommandations communautaires

Les forums Delphi que nous avons vus précédemment sont d'excellentes sources pour découvrir des composants recommandés par la communauté.

**Questions à poser** :
- "Quel composant recommandez-vous pour [fonctionnalité] ?"
- "Quelles sont les meilleures bibliothèques pour [domaine] ?"
- "Des alternatives gratuites à [composant commercial] ?"

## Composants gratuits vs composants payants

### Composants gratuits (Open Source ou Freeware)

**Avantages** :
- Pas de coût
- Code source souvent disponible
- Possibilité de modifier selon vos besoins
- Pas de restrictions de licence (selon la licence open source)

**Inconvénients possibles** :
- Support limité ou inexistant
- Documentation parfois incomplète
- Mises à jour irrégulières
- Compatibilité non garantie avec les nouvelles versions de Delphi

**Quand les utiliser ?**
- Projets personnels ou d'apprentissage
- Budgets limités
- Fonctionnalités simples et bien définies
- Quand vous êtes prêt à investir du temps pour comprendre le code

### Composants commerciaux (payants)

**Avantages** :
- Support technique professionnel
- Documentation complète
- Mises à jour régulières
- Tests approfondis
- Compatibilité garantie
- Exemples nombreux

**Inconvénients** :
- Coût d'acquisition (licence)
- Coûts de maintenance annuels parfois
- Dépendance vis-à-vis de l'éditeur
- Code source pas toujours fourni

**Quand les utiliser ?**
- Projets professionnels
- Fonctionnalités critiques
- Quand le temps de développement coûte plus cher que la licence
- Besoin de support garanti

**Fourchettes de prix** :
- Simple : 50-200 €
- Moyen : 200-800 €
- Suite complète : 800-3000 €
- Versions d'essai généralement disponibles

## Catégories de composants populaires

### Interfaces utilisateur (UI)

**Grilles de données avancées**
- DevExpress VCL / FMX
- TMS Component Pack
- EhLib (TDBGridEh)

Ces composants offrent :
- Tri et filtrage automatiques
- Groupement de données
- Édition en ligne avancée
- Export Excel/PDF
- Mise en forme conditionnelle

**Skins et thèmes**
- AlphaControls
- VCL Styles Utils
- TMS VCL UI Pack

Permettent de créer des interfaces modernes et personnalisées.

**Contrôles avancés**
- TMS Component Pack (calendriers, planners, contrôles de saisie enrichis)
- Konopka Signature VCL Controls (KSVC, anciennement Raize Components, racheté par Embarcadero en 2015)
- PngComponents (support PNG natif pour anciennes versions)

### Bases de données et accès aux données

**Connexion et composants de données**
- FireDAC (inclus dans Delphi, anciennement nommé AnyDAC avant le rachat par Embarcadero en 2013)
- UniDAC (Universal Data Access Components) - Accès universel
- ZeosLib - Open source, support multi-SGBD

**ORM (Object-Relational Mapping)**
- TMS Aurelius - ORM complet
- mORMot - Framework complet avec ORM
- InstantObjects - Open source

**Reporting dans les bases**
- DevExpress Reports
- FastReport VCL/FMX
- ReportBuilder

### Réseau et communication

**REST et Web Services**
- TMS Cloud Pack
- Indy (inclus dans Delphi)
- Synapse - Bibliothèque réseau légère

**HTTP et Internet**
- ICS (Internet Component Suite) - Open source
- TMS Cloud Pack
- Indy (inclus)

**WebSockets**
- TMS WebSockets
- SGC WebSockets

### Manipulation de fichiers et documents

**PDF**
- Gnostice PDFtoolkit
- Debenu Quick PDF Library
- Report Workshop (génération)
- Synopse PDF Engine (gratuit, open source)

**Excel**
- TMS FlexCel Studio (commercial)
- TMS XLSReadWrite (commercial)
- Excel4Delphi (open source)
- ZEXMLSS (open source, Lazarus/Delphi)

**ZIP et compression**
- TMS Pack (compression multiple formats)
- Abbrevia (open source)
- 7Zip DLL wrapper

**XML/JSON**
- TMS JSON Library
- DWS JSON (open source)
- SuperObject (open source)
- Support natif de Delphi (System.JSON)

### Cryptographie et sécurité

**Chiffrement**
- TurboPower LockBox (open source, anciennement commercial)
- SecureBlackBox (commercial)
- DCPcrypt (open source)

**Hachage et signatures**
- DCPcrypt (algorithmes de hachage : SHA, MD5, etc.)
- OpenSSL wrappers (TIdSSLIOHandlerSocketOpenSSL d'Indy, ou wrappers tiers)

### Multimédia et graphisme

**Traitement d'images**
- Graphics32 (open source)
- ImageEn
- TMS Image Components

**Audio et vidéo**
- BASS Audio Library
- DSPack
- FFmpeg wrappers

**Graphiques et charts**
- TeeChart (inclus dans certaines éditions)
- DevExpress Charts
- TMS Charts

### Tests et qualité

**Tests unitaires**
- DUnitX (inclus)
- TestInsight
- DUnit (historique)

**Analyse de code**
- Pascal Analyzer
- FixInsight

**Profilage (Profiling)**
- AQTime Pro
- Sampling Profiler (open source)

## Comment évaluer un composant tiers

### Critères essentiels

#### 1. Compatibilité de version

**Vérifiez** :
- Supporte votre version de Delphi (exemple : Delphi 13 Florence)
- Compatible avec vos plateformes cibles (Windows, macOS, iOS, Android, Linux)
- Fonctionne avec VCL et/ou FireMonkey selon vos besoins

#### 2. Documentation

**Une bonne documentation doit inclure** :
- Guide de démarrage rapide
- Référence API complète
- Exemples de code
- Tutoriels pas à pas
- FAQ

**Signal d'alarme** : Documentation absente, incomplète ou uniquement en langue que vous ne maîtrisez pas.

#### 3. Support et communauté

**Questions à se poser** :
- Y a-t-il un forum de support actif ?
- Le développeur/éditeur répond-il aux questions ?
- Quelle est la fréquence des mises à jour ?
- Y a-t-il une communauté d'utilisateurs ?

**Pour les composants open source** :
- Nombre de stars sur GitHub
- Fréquence des commits
- Nombre d'issues ouvertes vs résolues
- Dernière activité

#### 4. Exemples et démos

Un bon composant est accompagné de :
- Projets de démonstration
- Code source des exemples
- Cas d'usage variés

**Testez les démos** avant d'acheter ou d'intégrer le composant.

#### 5. Performance

Recherchez :
- Benchmarks
- Retours d'utilisateurs sur la performance
- Impact sur la taille de l'exécutable
- Consommation mémoire

#### 6. Stabilité et maturité

**Indicateurs de stabilité** :
- Nombre d'années d'existence
- Nombre de versions publiées
- Liste de clients ou projets utilisant le composant
- Retours utilisateurs

#### 7. Prix et modèle de licence

**Comprenez** :
- Prix d'achat
- Coûts de mise à jour / maintenance
- Type de licence (par développeur, par site, par application)
- Source code inclus ou non
- Redistribution autorisée

### Questions à poser avant adoption

**Pour un projet professionnel** :
- Que se passe-t-il si l'éditeur arrête le support ?
- Puis-je obtenir le code source via un dépôt fiduciaire (escrow) ?
- Quelle est la politique de mises à jour ?
- Y a-t-il un SLA (Service Level Agreement, contrat de niveau de service) ?

**Pour un projet personnel** :
- Est-ce que je comprends suffisamment le composant ?
- Puis-je me passer de support ?
- Y a-t-il des alternatives gratuites ?

## Installation et gestion des composants

### Via GetIt Package Manager

**Étapes** :
1. Ouvrez GetIt (Tools → GetIt Package Manager)
2. Recherchez le composant souhaité
3. Cliquez sur "Install"
4. Redémarrez l'IDE si demandé

**Avantages** : Installation automatique, gestion des dépendances, désinstallation propre.

### Installation manuelle

#### Composants avec installateur

Beaucoup de composants commerciaux fournissent un programme d'installation :
1. Téléchargez l'installateur
2. Fermez Delphi
3. Exécutez l'installateur
4. Suivez les instructions
5. Redémarrez Delphi

#### Composants à installer manuellement

Pour les composants open source ou sans installateur :

**Méthode 1 : Via packages (.dpk)**
1. Ouvrez le fichier .dpk dans Delphi (File → Open)
2. Clic droit sur le package → Compile
3. Clic droit → Install
4. Le composant apparaît dans la palette

**Méthode 2 : Ajout direct au projet**
1. Copiez les fichiers source dans un dossier de votre projet
2. Ajoutez le chemin dans Project → Options → Delphi Compiler → Search Path
3. Utilisez le composant via un uses dans votre code

### Organisation des composants

**Bonne pratique** :
- Créez un dossier "Components" ou "Libraries" dans votre espace de travail
- Organisez par catégorie ou par éditeur
- Documentez les versions et dépendances
- Utilisez un gestionnaire de versions (Git) pour vos composants personnalisés

### Gestion des versions

**Problème courant** : Un projet utilise une version d'un composant, un autre projet une version différente.

**Solutions** :
- Utilisez des dossiers séparés par version
- Documentez les dépendances de chaque projet
- Envisagez des machines virtuelles ou conteneurs pour des environnements isolés
- Testez toujours après une mise à jour de composant

## Bibliothèques incontournables pour débuter

### Pour tous les débutants

**1. TMS Component Pack** (commercial)
Suite complète de composants UI pour créer des interfaces modernes. Version d'essai disponible.

**2. Synopse mORMot** (gratuit, open source)
Framework complet pour REST, ORM et services. Courbe d'apprentissage, mais très puissant.

**3. FastReport** (commercial, inclus dans certaines éditions)
Générateur de rapports simple et efficace.

**4. Spring4D** (gratuit, open source)
Framework qui apporte des patterns modernes (IoC, collections enrichies).

**5. DUnitX** (gratuit, inclus)
Framework de tests unitaires moderne.

### Pour le développement web/REST

**1. Horse** (gratuit, open source)
Framework web minimaliste et élégant pour créer des API REST.

**2. Delphi MVC Framework** (gratuit, open source)
Framework complet pour applications web.

**3. mORMot** (gratuit, open source)
Framework complet avec client/serveur REST.

### Pour les bases de données

**1. FireDAC** (inclus dans Delphi)
Commencez par maîtriser FireDAC avant d'explorer d'autres solutions.

**2. UniDAC** (commercial)
Alternative à FireDAC avec certaines fonctionnalités supplémentaires.

**3. ZeosLib** (gratuit, open source)
Bibliothèque légère d'accès multi-SGBD.

### Pour les interfaces modernes

**1. VCL Styles** (inclus dans Delphi)
Système de thèmes natif de Delphi. Gratuit et efficace.

**2. TMS VCL UI Pack** (commercial)
Composants modernes pour interfaces Windows 10/11.

**3. AlphaControls** (commercial)
Skins et thèmes avancés pour VCL.

## Précautions et bonnes pratiques

### Ne tombez pas dans le piège du "syndrome du composant"

**Le syndrome** : Télécharger et installer des dizaines de composants "au cas où", alourdissant l'IDE et créant de la confusion.

**Bonne approche** :
- Installez uniquement ce dont vous avez besoin maintenant
- Testez un composant sur un projet de test avant de l'utiliser en production
- Désinstallez les composants que vous n'utilisez plus

### Lisez les licences

**Important pour les projets commerciaux** :
- Certaines licences open source (GPL) imposent de partager votre code source
- Vérifiez les droits de redistribution
- Assurez-vous de respecter les attributions demandées

### Gardez des copies locales

**Pour les composants critiques** :
- Sauvegardez une copie du composant et de sa documentation
- Ne dépendez pas uniquement du téléchargement en ligne
- Cas réel : certains sites de composants ont disparu, rendant impossible le téléchargement

### Testez avant d'acheter

**Pour les composants commerciaux** :
- Utilisez toujours la version d'essai
- Testez sur un projet réel représentatif
- Vérifiez la qualité du support en posant des questions
- Comparez avec les alternatives

### Documentez vos dépendances

**Dans chaque projet** :
- Listez les composants tiers utilisés
- Notez les versions exactes
- Conservez les liens de téléchargement
- Documentez les configurations spécifiques

**Exemple de fichier COMPONENTS.md** :
```markdown
# Composants tiers utilisés

## TMS Component Pack
- Version: 12.1.0.0
- Licence: Commerciale (1 développeur)
- URL: https://www.tmssoftware.com
- Utilisé pour: TAdvStringGrid, TWebBrowser

## mORMot
- Version: 2.1.5
- Licence: Open Source (GPL/MPL)
- URL: https://github.com/synopse/mORMot2
- Utilisé pour: Client REST, ORM
```

### Planifiez les mises à jour

**Stratégie** :
- Ne mettez pas à jour un composant en cours de développement d'une fonctionnalité critique
- Lisez toujours les Release Notes
- Testez sur une branche Git séparée
- Gardez la possibilité de revenir en arrière

### Ayez un plan B

**Question à se poser** : Que se passe-t-il si ce composant n'est plus maintenu ?

**Stratégies** :
- Pour les fonctionnalités critiques, privilégiez les composants avec code source
- Encapsulez l'utilisation du composant dans vos propres classes (pattern Adapter)
- Gardez une trace de vos recherches d'alternatives

## Créer ses propres composants

### Quand créer son propre composant ?

**Bonnes raisons** :
- Aucun composant existant ne répond exactement à votre besoin
- Vous avez un code réutilisé dans plusieurs projets
- Vous souhaitez partager avec la communauté
- Apprentissage et amélioration de vos compétences

**Mauvaises raisons** :
- "Je peux le faire mieux" sans preuve concrète
- Refus de payer pour un composant mature alors que votre temps coûte plus cher
- Syndrome du "Not Invented Here"

### Ressources pour apprendre

- Documentation Delphi sur la création de composants
- Livre : "Developing Custom Delphi Components"
- Analyse du code source de composants open source
- Tutoriels sur les forums et blogs

## Contribution à l'écosystème

### Utiliser des composants open source = responsabilité

Si vous utilisez des composants gratuits :
- Signalez les bugs proprement
- Contribuez à la documentation si possible
- Proposez des améliorations
- Faites des dons si le projet accepte
- Mentionnez le composant dans vos projets

### Partager ses propres créations

Si vous développez quelque chose d'utile :
- Envisagez de le partager sur GitHub
- Documentez correctement
- Choisissez une licence claire
- Soyez prêt à gérer les issues et questions

## Ressources pour aller plus loin

### Sites de référence
- GetIt Package Manager (dans l'IDE)
- TorryDelphi : http://www.torry.net/
- Embarcadero Technology Partners
- Awesome Delphi : https://github.com/Fr0sT-Brutal/awesome-delphi (liste GitHub)

### Groupes et forums
Les forums que nous avons vus au chapitre 20.2 sont excellents pour demander des recommandations de composants.

### Chaînes YouTube
Beaucoup de développeurs publient des critiques (reviews) de composants et des tutoriels d'intégration.

## Conclusion

Les composants tiers sont l'un des plus grands atouts de Delphi. Ils peuvent transformer votre façon de développer en vous donnant accès à des fonctionnalités professionnelles en un temps record.

**Points clés à retenir** :

- Commencez par explorer GetIt Package Manager
- Évaluez soigneusement avant d'adopter un composant
- Privilégiez la documentation et le support
- N'installez que ce dont vous avez vraiment besoin
- Documentez vos dépendances
- Testez les versions d'essai avant d'acheter
- Contribuez à l'écosystème open source quand vous le pouvez
- Gardez une approche pragmatique : utilisez ce qui existe, créez uniquement ce qui manque vraiment

**Conseil final** : Un bon développeur Delphi n'est pas celui qui code tout de zéro, mais celui qui sait identifier, évaluer et intégrer efficacement les bons composants pour créer rapidement des applications de qualité professionnelle.

L'écosystème Delphi est riche, profitez-en intelligemment et contribuez-y quand vous le pouvez. Vous ne développez jamais seul avec Delphi : derrière chaque composant que vous utilisez, il y a des développeurs passionnés qui partagent leur expertise. Respectez leur travail et, un jour, vous pourrez peut-être contribuer à votre tour à cet écosystème formidable.

⏭️ [Conférences et événements](/20-ressources-et-communaute/04-conferences-et-evenements.md)
