🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.2 Histoire et évolutions

## Les origines : Pascal et Turbo Pascal

Pour comprendre Delphi, il faut remonter à **1970**, avec la création du langage **Pascal** par le professeur suisse **Niklaus Wirth** (1934-2024) à l'École polytechnique fédérale de Zurich (ETH). Wirth, lauréat du **prix Turing** en 1984 (l'équivalent du Nobel en informatique), avait conçu Pascal comme un langage d'enseignement, mettant l'accent sur la clarté, la structure et les bonnes pratiques de programmation.

> **Anecdote francophone :** Le nom **Pascal** est un hommage au mathématicien et philosophe français **Blaise Pascal** (1623-1662), inventeur de l'une des premières machines à calculer mécaniques (la "Pascaline"). Une belle façon pour Wirth de rendre hommage à un pionnier du calcul.

Niklaus Wirth a continué à concevoir d'autres langages dans la même lignée — **Modula-2** (1978), **Oberon** (1986) — toujours dans une démarche d'enseignement et de simplicité. Il s'est éteint le 1ᵉʳ janvier 2024 à l'âge de 89 ans, laissant un héritage majeur dans l'histoire de l'informatique.

### Borland et Turbo Pascal : la révolution des années 80

Dans les années 1980, une jeune société californienne nommée **Borland**, fondée en 1983 par l'entrepreneur **Philippe Kahn** (un Français installé aux États-Unis), révolutionne le monde de la programmation en créant **Turbo Pascal**.

Ce compilateur Pascal, conçu initialement par le développeur danois **Anders Hejlsberg** (sur la base de son compilateur "Compass Pascal"), est vendu à **49,95 $** seulement — un prix révolutionnaire à l'époque où les compilateurs concurrents coûtaient plusieurs centaines de dollars. Le succès est phénoménal : Turbo Pascal devient l'un des outils de développement les plus populaires de l'époque, particulièrement apprécié des étudiants, des développeurs indépendants, et plus tard des professionnels.

Turbo Pascal apporte plusieurs innovations marquantes :
- **Compilation extrêmement rapide** (quelques secondes, là où d'autres compilateurs prenaient plusieurs minutes)
- **Éditeur de code intégré** avec coloration syntaxique
- **Système d'aide en ligne**
- **Tarif accessible**

C'est sur cette base que naîtra Delphi 12 ans plus tard.

### L'architecte derrière Turbo Pascal et Delphi : Anders Hejlsberg

Une figure majeure se cache derrière Turbo Pascal puis Delphi : **Anders Hejlsberg**, un développeur danois qui rejoint Borland en 1983 et devient l'architecte en chef de Turbo Pascal puis l'un des architectes principaux de Delphi. Ses choix de conception — syntaxe claire, compilation rapide, gestion fine des types — influenceront durablement le langage.

En 1996, **Anders Hejlsberg quitte Borland pour Microsoft**. Il commence par travailler sur **Visual J++** (la tentative Microsoft de Java), puis devient l'architecte en chef de **C#** et du framework **.NET** à partir de 2000. Plus tard, il créera également **TypeScript** (2012). On retrouve donc dans C#, TypeScript (et même J++) de nombreuses idées héritées de Delphi : propriétés (`property`), événements (`event`), types valeur (`struct`), génériques, attributs (`[Attribute]`), méthodes anonymes. Cette filiation explique pourquoi un développeur Delphi se sent rapidement à l'aise en C# (et vice-versa).

Le départ d'Hejlsberg en 1996 marque la fin d'une époque chez Borland et joue probablement un rôle dans certains errements stratégiques ultérieurs (notamment le virage hasardeux vers .NET avec Delphi 8).

> 🎓 **Pour aller plus loin :** Anders Hejlsberg a donné de nombreuses conférences passionnantes sur l'histoire de Turbo Pascal, Delphi et C#. Plusieurs sont disponibles gratuitement sur YouTube.

## 1995 : La naissance de Delphi

Le 14 février 1995, Borland lance **Delphi 1**, un événement majeur dans l'histoire du développement logiciel. Le nom "Delphi" fait référence à l'oracle de Delphes dans la mythologie grecque, mais a aussi une origine plus prosaïque : pendant le développement (nom de code interne "Delphi"), l'équipe trouvait que "*If you want to talk to Oracle, you go to Delphi*" — un jeu de mots avec Oracle Database, alors un concurrent majeur en bases de données que Delphi cherchait à concurrencer.

Delphi 1 introduit plusieurs innovations révolutionnaires :
- Un environnement de développement visuel pour Windows
- La compilation rapide en code natif
- Une bibliothèque de composants visuels réutilisables (la VCL)
- L'approche RAD (Développement Rapide d'Applications)

Cette première version cible exclusivement **Windows 3.1** et marque le début d'une longue lignée de versions successives.

## L'ère Borland (1995-2006)

### Delphi 2 (1996)
Première version 32 bits, compatible avec Windows 95 et Windows NT. Cette évolution majeure permet de créer des applications plus puissantes et modernes.

### Delphi 3 (1997)
Introduit les packages de composants, permettant une meilleure organisation et distribution des bibliothèques. Cette version renforce également le support des bases de données.

### Delphi 4 (1998)
Apporte des améliorations significatives à l'IDE et introduit de nouvelles fonctionnalités pour la programmation web et les applications serveur.

### Delphi 5 (1999)
Considérée par beaucoup comme l'une des versions les plus stables et abouties de l'ère Borland. Elle améliore les performances et ajoute de nombreux composants.

### Delphi 6 (2001)
Introduit WebSnap pour le développement web et de nombreux composants. En parallèle, Borland lance **Kylix**, un produit compagnon permettant de développer pour Linux avec un environnement similaire à Delphi — une première tentative de sortir du monde exclusivement Windows.

> **Le cas Kylix :** Kylix (1, 2, et 3, sortis entre 2001 et 2003) reposait sur la bibliothèque **CLX** (Component Library for Cross-Platform), une alternative à la VCL pour Linux. Malgré une idée prometteuse, Kylix souffrait de problèmes techniques (dépendance à Qt2 dans la première version, problèmes de licence) et de l'absence d'un marché Linux desktop suffisant à l'époque. Le projet est abandonné en 2003 et il faudra attendre **Delphi 10.2 Tokyo en 2017** pour voir le retour officiel du support Linux (cette fois côté serveur).

### Delphi 7 (2002)
Version emblématique et très populaire, encore utilisée dans certaines entreprises aujourd'hui. Elle représente l'apogée de Delphi sous Borland, avec une stabilité remarquable et un ensemble de fonctionnalités mature.

### Delphi 8 (2003)
Première version ciblant exclusivement le framework **.NET 1.1** de Microsoft, sans support Win32. Cette orientation radicale fut mal accueillie par la communauté, qui attendait avant tout un compilateur Win32 modernisé. Version peu populaire et rarement utilisée en production.

> **Le traumatisme Delphi 8 :** Cet épisode est souvent cité comme une erreur stratégique majeure de Borland. Tandis que la communauté Delphi attendait un Delphi 7 modernisé, Borland a misé sur .NET — qui était alors une plateforme nouvelle et concurrente. Beaucoup d'entreprises sont restées sur Delphi 7 pendant des années, voire ont migré vers d'autres technologies (C#, Java). Cet épisode marque durablement la perception de Delphi dans le monde du développement, même si les versions suivantes corrigeront partiellement le tir.

### Delphi 2005 et Delphi 2006
Tentative de réconciliation : ces versions supportent à la fois Win32 et .NET dans un IDE unifié appelé **BDS** (Borland Developer Studio), précurseur de l'actuel RAD Studio. La complexité de cette dualité et certains problèmes de performance les rendent moins populaires que Delphi 7.

## La transition : CodeGear (2006-2008)

En novembre 2006, Borland crée **CodeGear**, une filiale dédiée aux outils de développement, afin de séparer cette activité de son cœur de métier orienté ALM (Application Lifecycle Management). Cette période courte voit le lancement de :

### Delphi 2007 (mars 2007)
Retour aux sources avec un focus sur le développement Win32, abandonnant les ambitions .NET d'origine. Cette version corrige de nombreux problèmes et retrouve la faveur des développeurs. C'est aussi la première à introduire un IDE rebaptisé **CodeGear RAD Studio**.

### Delphi 2009 (août 2008)
Introduction du support **Unicode complet** : le type `String` devient nativement `UnicodeString` (UTF-16) au lieu d'`AnsiString`. Cette évolution majeure pour la gestion des caractères internationaux brise volontairement la compatibilité avec les versions antérieures et oblige les développeurs à adapter leur code existant — un tournant historique souvent comparé à la transition Python 2 → Python 3.

## L'ère Embarcadero (2008-aujourd'hui)

En mai 2008, **Embarcadero Technologies** rachète CodeGear à Borland pour environ 23 millions de dollars et poursuit le développement de Delphi avec une vision renouvelée. Embarcadero est elle-même rachetée en 2015 par le groupe **Idera, Inc.**, mais conserve sa marque et son équipe produit.

### Delphi 2010 (août 2009)
Améliore l'IDE et introduit de nouveaux composants (touch, gestures, Direct2D). C'est aussi la deuxième version Unicode après Delphi 2009. Anecdote : Delphi 2010 est en réalité la 14ᵉ version (le numéro 13 a été délibérément sauté à l'époque pour des raisons de superstition — ironie de l'histoire, puisque Delphi 13 Florence existe bel et bien aujourd'hui en 2025).

### Delphi XE (août 2010)
Changement de nomenclature avec le suffixe XE (pour "extended"), annonçant l'ambition multi-plateforme et professionnelle d'Embarcadero. Intégration du gestionnaire de versions **Subversion (SVN)** dans l'IDE, amélioration des outils de profilage, et premiers ajouts vers le développement web (avec le module **DataSnap** modernisé).

### Delphi XE2 (septembre 2011)
**Révolution majeure** : introduction de **FireMonkey (FMX)**, un nouveau framework permettant de créer des applications pour Windows, macOS et iOS à partir d'un code source unique. Première version supportant la compilation 64 bits sous Windows. C'est un tournant historique pour Delphi.

### Delphi XE3 à XE8 (2012-2015)
Série de versions améliorant progressivement le support multi-plateforme :

- **XE3 (sept. 2012)** : abandon temporaire du support iOS, support macOS amélioré
- **XE4 (avril 2013)** : retour du **support iOS natif** avec le compilateur ARM
- **XE5 (sept. 2013)** : ajout du **support Android** (ARM v7)
- **XE6 (avril 2014)** : améliorations FireMonkey, intégration cloud
- **XE7 (sept. 2014)** : Parallel Programming Library, Multi-Device Designer
- **XE8 (avril 2015)** : introduction du **GetIt Package Manager**, support iOS 64 bits

### Delphi 10 Seattle (septembre 2015)
Nouveau changement de nomenclature, retour à une numérotation classique avec des noms de villes. Modernisation de l'IDE, support de Windows 10, et amélioration des performances.

### Delphi 10.1 Berlin (avril 2016)
Améliorations significatives de l'IDE, support amélioré des composants visuels, et nouvelles fonctionnalités pour FireMonkey.

### Delphi 10.2 Tokyo (mars 2017)
Première version à intégrer un **compilateur Linux** (côté serveur, sans interface graphique native). Cette version est dédiée aux applications serveur et services Linux. Le support graphique Linux via **FMXLinux** (initialement un add-on tiers de KSDev) ne sera intégré officiellement via GetIt qu'à partir de Delphi 10.3.1 en 2019.

### Delphi 10.3 Rio (novembre 2018)
Focus sur l'amélioration de la qualité, la stabilité et les performances. Refonte de l'IDE avec un look moderne, meilleur support des écrans haute résolution (High DPI), et améliorations majeures de FireMonkey.

### Delphi 10.4 Sydney (mai 2020)
Améliorations significatives de l'IDE, nouveau **Code Insight basé sur LSP** (Language Server Protocol), unification des chaînes de caractères, gestion mémoire native ARC retirée au profit du modèle classique sur mobile (cohérence du langage), support des dernières versions d'Android et iOS.

> **L'épisode ARC mobile :** Entre Delphi XE4 (2013) et Delphi 10.4 (2020), les compilateurs Delphi mobile (iOS, Android) utilisaient un modèle de gestion mémoire différent du desktop : **ARC** (Automatic Reference Counting), inspiré d'Objective-C. Les objets étaient automatiquement libérés quand plus aucune référence ne les pointait. Cette différence créait deux dialectes incompatibles d'Object Pascal : le code écrit pour desktop devait souvent être adapté pour mobile, et inversement. Avec 10.4, Embarcadero revient au modèle classique (`Create`/`Free`) sur toutes les plateformes, unifiant ainsi le langage — décision controversée mais bénéfique pour la portabilité du code.

### Delphi 11 Alexandria (septembre 2021)
**Changement majeur** : IDE adapté aux écrans haute résolution avec icônes vectorielles, support de **Windows 11**, amélioration du compilateur, du débogueur et du designer de fiches. Première version intégrant FMXLinux directement dans la distribution standard.

### Delphi 12 Athens (novembre 2023)
Continuation des améliorations avec :
- **Préparation du passage en 64 bits de l'IDE** (la transition complète sera finalisée dans Delphi 13)
- Améliorations de l'éditeur : **Multi-Caret** (édition multi-curseurs) et **Wide Pages** (onglets plus larges)
- Support amélioré des dernières versions d'iOS et Android
- Refonte de plusieurs composants VCL et FMX (Skia, FMX 3D, etc.)
- Adoption de **Skia** comme moteur de rendu graphique alternatif pour FireMonkey

### Delphi 13 Florence (septembre 2025)
**Version actuelle** apportant des changements majeurs :
- **IDE 64 bits natif** (première version où l'IDE lui-même fonctionne en 64 bits)
- **Opérateur ternaire** introduit dans Object Pascal (via le mot-clé `if` utilisé en expression)
- **Composants IA intégrés** dans la VCL et FireMonkey
- **Site web companion IA** pour assistance au développement
- Support **LLDB v12** pour un débogage avancé sur les plateformes mobiles et macOS
- **Styles VCL en mode conception** pour le prototypage rapide
- Intégration **UIAutomation** Microsoft pour l'accessibilité et les tests automatisés
- **WebStencils** pour l'intégration côté serveur
- Améliorations significatives de FireMonkey (notamment FMXLinux) et FireDAC

### Delphi 13.1 Florence Update 1 (mars 2026)
Mise à jour majeure de la version actuelle :
- Nouveau compilateur natif **Windows on Arm (Arm64EC)**
- Nouveau **FireMonkey Style Designer**
- Améliorations du moteur LSP Delphi
- Support des derniers SDK Android et iOS
- HTTP Server-Sent Events (SSE) pour serveurs et clients web
- Support SAP ASE Server 16.1, IBM DB2 12.1, MariaDB Server 12.1

## Les grandes évolutions technologiques

Au fil des années, Delphi a connu plusieurs révolutions technologiques majeures :

**De mono-plateforme à multi-plateforme** : Initialement conçu uniquement pour Windows, Delphi supporte désormais Windows, macOS, iOS, Android et Linux.

**Du 16 bits au 64 bits** : Delphi est passé de Windows 16 bits à un support complet du 64 bits sur toutes les plateformes.

**De l'ANSI à Unicode** : Support complet de l'Unicode pour gérer toutes les langues du monde.

**Du desktop au mobile** : Évolution naturelle vers le développement d'applications mobiles natives.

**Vers le cloud et l'IA** : Intégration des services cloud et des technologies d'intelligence artificielle modernes.

## Tableau récapitulatif des versions majeures

À partir de **Delphi 10 Seattle**, Embarcadero a adopté une nomenclature où le "nom de ville" sert à la fois de codename pendant le développement **et** de nom commercial final. Pour les versions antérieures, le codename était un nom interne distinct, parfois rendu public a posteriori.

| Version | Année | Codename interne | Innovation majeure |
|---------|-------|------------------|--------------------|
| Delphi 1 | 1995 | (Delphi) | Premier IDE RAD pour Windows 3.1 (16 bits) |
| Delphi 2 | 1996 | — | Passage au 32 bits |
| Delphi 3 | 1997 | — | Packages, COM, ActiveX |
| Delphi 4 | 1998 | — | Dockable IDE, DCOM, MIDAS |
| Delphi 5 | 1999 | — | Stabilité légendaire, TeamSource |
| Delphi 6 | 2001 | — | Kylix (Linux), WebSnap, dbExpress |
| Delphi 7 | 2002 | — | Apogée de l'ère Borland |
| Delphi 8 | 2003 | **Octane** | Cible exclusive .NET (échec commercial) |
| Delphi 2005 | 2004 | **Diamondback** | Réintroduction de Win32 + .NET |
| Delphi 2006 | 2005 | **Dexter** | BDS unifié, AJAX |
| Delphi 2007 | 2007 | **Highlander** | Retour à Win32 (sous CodeGear) |
| Delphi 2009 | 2008 | **Tiburón** | Unicode complet (UnicodeString), génériques |
| Delphi 2010 | 2009 | **Weaver** | Touch / gestures, RTTI étendu |
| Delphi XE | 2010 | **Fulcrum** | Subversion intégré, profilage |
| Delphi XE2 | 2011 | **Pulsar** | FireMonkey + Mac + iOS + 64 bits Windows |
| Delphi XE3 | 2012 | **Waterdragon** | Support macOS amélioré |
| Delphi XE4 | 2013 | **Quintessence** | Retour iOS natif |
| Delphi XE5 | 2013 | **Zephyr** | Support Android |
| Delphi XE6 | 2014 | **Proteus** | Améliorations FireMonkey |
| Delphi XE7 | 2014 | **Carpathia** | Parallel Programming Library |
| Delphi XE8 | 2015 | **Elbrus** | GetIt Package Manager |
| Delphi 10 Seattle | 2015 | Seattle | Support Windows 10 |
| Delphi 10.1 Berlin | 2016 | Berlin | Améliorations IDE |
| Delphi 10.2 Tokyo | 2017 | Tokyo | Compilateur Linux (serveur) |
| Delphi 10.3 Rio | 2018 | Rio | Refonte IDE moderne |
| Delphi 10.4 Sydney | 2020 | Sydney | Code Insight LSP, fin de l'ARC mobile |
| Delphi 11 Alexandria | 2021 | Alexandria | Support Windows 11, IDE HiDPI |
| Delphi 12 Athens | 2023 | Athens | Refonte de l'éditeur, multi-caret, Skia |
| Delphi 13 Florence | 2025 | Florence | IDE 64 bits, IA intégrée, ternaire |
| Delphi 13.1 | 2026 | Florence Update 1 | Windows on Arm natif (LLVM 20) |

> 💡 Vous remarquerez que pour Delphi 1 à 7, les codenames internes ne sont pas largement documentés publiquement par Borland. Les codenames marqués en gras à partir de Delphi 8 sont attestés par les blogs officiels et la documentation Embarcadero.

## Une longévité exceptionnelle

Avec plus de **30 ans d'existence**, Delphi fait figure d'exception dans le monde du développement logiciel. Peu d'outils ont réussi à maintenir leur pertinence aussi longtemps, en s'adaptant continuellement aux nouvelles technologies et aux besoins changeants des développeurs.

Cette longévité témoigne de :
- La solidité de ses fondations techniques
- La fidélité de sa communauté
- L'engagement d'Embarcadero dans son développement
- La pertinence de l'approche RAD dans le monde moderne

## Quelques applications notoires développées en Delphi

Au-delà des milliers d'applications de gestion d'entreprise, certains logiciels grand public ou de niche ont été développés (parfois en partie) en Delphi. Quelques exemples historiques ou actuels :

- **Skype** (premières versions) : Le client desktop original de Skype, avant son rachat par Microsoft en 2011, était développé en Delphi par l'équipe estonienne.
- **FL Studio** (Image-Line) : Le célèbre logiciel de production musicale est développé en Delphi depuis ses débuts.
- **Total Commander** : Le gestionnaire de fichiers de référence sous Windows.
- **Embarcadero RAD Studio lui-même** : Une partie significative de l'IDE Delphi est elle-même écrite en Delphi (un cas classique de **dogfooding**).
- **TOAD** (Tool for Oracle Application Developers) : Outil d'administration de bases de données Oracle.
- **Beyond Compare** : Outil de comparaison de fichiers et dossiers.
- **WinRAR** (interface) : Le gestionnaire d'archives bien connu utilise Delphi pour son UI.
- **Nombreux ERP, CRM et logiciels métiers** : particulièrement répandus dans les pays germanophones (Allemagne, Suisse, Autriche), au Brésil, en Italie, et en France où Delphi est très implanté dans les PME.

Cela montre que Delphi est utilisé à la fois pour :
- Des applications "grand public" performantes (Skype, FL Studio)
- Des outils techniques exigeants (TOAD, Beyond Compare)
- Et surtout, l'immense majorité d'applications **métier critiques** qui font tourner les entreprises au quotidien

## Pourquoi cette histoire est importante

Comprendre l'histoire de Delphi vous aide à :
- Apprécier la maturité et la stabilité de l'outil
- Comprendre certains choix de conception
- Situer Delphi dans l'écosystème du développement logiciel
- Avoir confiance dans la pérennité de vos compétences Delphi

Delphi n'est pas un outil éphémère ou une mode passagère : c'est une plateforme de développement éprouvée qui continue d'évoluer et de s'adapter au monde moderne du développement logiciel.

## En résumé

De Turbo Pascal à Delphi 13 Florence, l'évolution a été constante et impressionnante. Chaque version a apporté son lot d'innovations tout en préservant la philosophie fondamentale : permettre aux développeurs de créer rapidement des applications de qualité professionnelle. Aujourd'hui, Delphi combine l'expérience de décennies de développement avec les technologies les plus modernes, offrant un outil unique dans le paysage du développement logiciel.

⏭️ [Versions disponibles et éditions (Community Edition incluse)](/01-introduction-a-delphi/03-versions-disponibles-et-editions.md)
