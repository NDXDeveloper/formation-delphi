🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.1 Qu'est-ce que Delphi ?

## Introduction

Delphi est un environnement de développement intégré (IDE) professionnel qui permet de créer des applications informatiques de manière rapide et efficace. Que vous souhaitiez développer une application Windows, une application mobile pour Android ou iOS, ou même un logiciel pour Linux ou macOS, Delphi vous offre les outils nécessaires pour y parvenir.

## Un outil de développement RAD

Delphi appartient à la famille des outils **RAD** (Rapid Application Development, ou Développement Rapide d'Applications). Cela signifie qu'il a été conçu pour vous permettre de créer des applications complètes en beaucoup moins de temps qu'avec des outils traditionnels.

Comment ? Grâce à une approche visuelle : au lieu d'écrire des centaines de lignes de code pour créer un simple bouton ou une fenêtre, vous pouvez les dessiner directement à l'écran, puis ajouter le code uniquement pour les fonctionnalités spécifiques.

### Illustration concrète de la philosophie RAD

Pour bien comprendre, imaginez que vous voulez créer une fenêtre avec un bouton qui affiche "Bonjour".

**Approche traditionnelle (par exemple en C avec Win32 API)** — environ 30 à 50 lignes de code juste pour la fenêtre, plus du code pour créer le bouton, gérer la boucle de messages, etc. :

```c
// Pseudo-code Win32 simplifié — beaucoup de code pour pas grand-chose
WNDCLASS wc = { 0 };
wc.lpfnWndProc = WindowProc;
RegisterClass(&wc);
HWND hwnd = CreateWindowEx(0, "MaClasse", "Ma fenêtre", WS_OVERLAPPEDWINDOW, ...);
// + gestion des messages, du bouton, des événements...
```

**Approche RAD avec Delphi :**
1. Vous **glissez-déposez** un bouton sur le formulaire (0 ligne de code)
2. Vous **double-cliquez** sur le bouton — Delphi génère automatiquement la procédure d'événement
3. Vous écrivez **une seule ligne** : `ShowMessage('Bonjour');`
4. Vous appuyez sur **F9** — votre application se compile et se lance

Total : **1 ligne de code écrite** pour un résultat équivalent. C'est cette philosophie qui fait la force de Delphi depuis 1995.

## Le langage Object Pascal

Delphi utilise le langage de programmation **Object Pascal**, une évolution moderne du langage Pascal créé dans les années 1970. Ne vous laissez pas intimider par l'âge de ses origines : Object Pascal est un langage puissant, structuré et relativement facile à apprendre pour les débutants.

Le langage Object Pascal se caractérise par :
- Une syntaxe claire et lisible
- Une programmation orientée objet complète
- Une forte vérification des types qui aide à éviter les erreurs
- Des performances excellentes pour les applications produites

### Un avant-goût de la syntaxe

Voici un exemple très simple pour vous donner une idée du style Object Pascal :

```pascal
program Bonjour;

procedure Saluer(const Nom: string);
begin
  WriteLn('Bonjour, ', Nom, ' !');
end;

begin
  Saluer('Marie');
  Saluer('Paul');
end.
```

Notez quelques caractéristiques visibles immédiatement :
- Le code est encadré par des mots-clés explicites (`begin` / `end`) au lieu d'accolades `{}` comme en C, Java, JavaScript ou C#
- Les déclarations de variables sont **typées** : `Nom: string` (et non simplement `Nom`)
- Le séparateur d'instructions est le **point-virgule** `;`
- Le code n'est **pas sensible à la casse** : `WriteLn`, `writeln`, `WRITELN` sont équivalents (à la différence de Java, C# ou Python). Par convention, on respecte cependant la casse "PascalCase" : majuscule sur la première lettre de chaque mot

C'est un avant-goût — l'apprentissage complet du langage est l'objet du chapitre 3 de cette formation.

## Un environnement complet

Delphi n'est pas simplement un éditeur de code. C'est un environnement complet qui intègre :

**Un éditeur de code avancé** avec coloration syntaxique, auto-complétion (appelée *Code Insight*), refactoring, et aide contextuelle pour vous aider à écrire votre code efficacement.

**Un concepteur visuel** (le *Form Designer*) qui vous permet de créer vos interfaces utilisateur en glissant-déposant des composants (boutons, zones de texte, images, etc.) sur vos formulaires (appelés *fiches* en français).

**Un compilateur performant** qui transforme votre code en application exécutable native, c'est-à-dire une application qui s'exécute directement sur le système d'exploitation sans nécessiter d'interpréteur ou de machine virtuelle (à la différence de Java, ou des modes traditionnels de C#). Concrètement, Delphi produit :
- Un fichier **`.exe`** autonome sous Windows
- Un **bundle `.app`** sous macOS (un dossier structuré qui contient le binaire Mach-O)
- Un **`.ipa`** sous iOS (contenant un binaire Mach-O ARM signé)
- Un **`.apk`** ou **`.aab`** sous Android (contenant des bibliothèques `.so` natives ARM et un peu de code Java de bootstrap)
- Un **ELF** binaire sous Linux

**Un débogueur intégré** pour identifier et corriger les erreurs dans votre code, avec depuis Delphi 13 un support amélioré de LLDB v12 sur les plateformes mobiles et macOS.

**Des bibliothèques de composants** contenant des milliers de composants prêts à l'emploi pour accélérer votre développement. Les deux principales sont :
- **VCL** (Visual Component Library) : la bibliothèque historique, spécifiquement optimisée pour Windows.
- **FMX** (FireMonkey) : la bibliothèque multi-plateformes, qui permet de cibler Windows, macOS, iOS, Android et Linux à partir d'un même code.

## Delphi et RAD Studio

Vous entendrez parfois parler de **RAD Studio** : il s'agit de la suite commerciale d'Embarcadero qui regroupe Delphi (basé sur Object Pascal) et C++Builder (basé sur C++). Les deux produits partagent le même IDE, les mêmes bibliothèques visuelles (VCL et FireMonkey) et la même approche RAD. Si vous achetez RAD Studio, vous obtenez à la fois Delphi et C++Builder ; si vous achetez Delphi seul, vous n'avez accès qu'à Object Pascal.

## Le développement multi-plateforme

L'une des forces majeures de Delphi moderne est sa capacité à créer des applications pour plusieurs plateformes à partir d'un seul code source :

- **Windows** : x86 (32 bits), x64 (64 bits), et depuis Delphi 13.1, **Arm64 / Arm64EC**
- **macOS** : Intel et **Apple Silicon** (ARM64)
- **iOS** : iPhone et iPad (ARM64)
- **Android** : smartphones et tablettes (ARM32 / ARM64)
- **Linux** : serveurs (x64) et desktop via FMXLinux

Cette approche **"un seul code source, plusieurs plateformes cibles"** (parfois résumée par le slogan marketing "Write Once, Compile Everywhere") vous permet d'économiser un temps considérable en développant une seule application qui se compilera nativement pour toutes ces plateformes.

> 💡 **À nuancer :** Cela ne signifie pas que tout votre code est identique sur toutes les plateformes. Les **spécificités plateforme** (par exemple : accès au GPS sous Android, intégration App Store sur iOS, services Windows…) nécessitent du code conditionnel. Mais l'**immense majorité** de votre code applicatif (logique métier, accès aux bases de données, traitement des données…) est partagée entre toutes les plateformes.

## À qui s'adresse Delphi ?

Delphi convient à différents profils de développeurs :

**Les débutants** qui découvrent la programmation apprécieront l'approche visuelle et la structure claire du langage Object Pascal.

**Les développeurs expérimentés** trouveront dans Delphi un outil puissant pour créer rapidement des applications professionnelles complexes.

**Les entreprises** utilisent Delphi pour développer des applications de gestion, des logiciels métier et des systèmes critiques grâce à sa fiabilité et ses performances.

**Les développeurs indépendants** profitent de la rapidité de développement pour créer et commercialiser leurs applications plus vite.

## Types d'applications réalisables

Avec Delphi, vous pouvez créer pratiquement n'importe quel type d'application :

- Applications de gestion d'entreprise (ERP, CRM, comptabilité, gestion de stock) avec bases de données
- Logiciels scientifiques, médicaux, et de calcul
- Applications multimédias et de traitement d'images
- Jeux (notamment 2D et casual ; pour la 3D AAA on préfèrera Unity ou Unreal)
- Applications mobiles natives pour iOS et Android
- Services web, API REST et microservices côté serveur
- Applications IoT (Internet des Objets) avec Arduino, Raspberry Pi, MQTT
- Services Windows et applications fonctionnant en arrière-plan
- Outils d'administration système et utilitaires
- Et bien plus encore...

## Quelques mots-clés à retenir

Voici un mini-glossaire des termes que vous rencontrerez tout au long de cette formation. Pas besoin de tout retenir maintenant — revenez ici si un mot vous échappe :

| Terme | Signification courte |
|-------|----------------------|
| **IDE** | Integrated Development Environment — l'environnement intégré dans lequel vous programmez |
| **RAD** | Rapid Application Development — l'approche de développement rapide par composants visuels |
| **Object Pascal** | Le langage utilisé par Delphi (évolution moderne de Pascal) |
| **VCL** | Visual Component Library — bibliothèque visuelle Windows native |
| **FMX** ou **FireMonkey** | Framework visuel multi-plateformes (Windows, macOS, iOS, Android, Linux) |
| **FireDAC** | Le framework d'accès aux bases de données de Delphi |
| **Fiche** (ou **Form**) | Une fenêtre de votre application, conçue dans le Form Designer |
| **Composant** | Élément réutilisable que vous placez sur une fiche (bouton, liste, grille…) |
| **Propriété** | Caractéristique configurable d'un composant (couleur, texte, taille…) |
| **Événement** | Réaction d'un composant à une action utilisateur (clic, survol, frappe…) |
| **Unité** (**Unit**) | Un fichier source `.pas` contenant du code Object Pascal |
| **Projet** (**Project**) | L'ensemble des fichiers (unités, ressources, configurations) qui forment une application |
| **Compilateur** | Programme qui traduit le code Object Pascal en exécutable natif |
| **Code natif** | Code machine exécutable directement par le processeur, sans intermédiaire |

## Idées reçues sur Delphi

Avant d'aller plus loin, il est utile de démentir quelques idées reçues qui circulent sur Delphi :

**❌ "Delphi est mort / obsolète"**
Faux. Delphi est activement développé par Embarcadero, avec une version majeure publiée environ chaque année (Delphi 13 en 2025, 13.1 en 2026). Le langage Object Pascal continue d'évoluer (opérateur ternaire en 13, génériques améliorés, etc.).

**❌ "Delphi ne sert qu'à faire des applications Windows"**
Faux depuis Delphi XE2 (2011) avec FireMonkey. Vous pouvez cibler Windows, macOS, iOS, Android et Linux avec le même code source.

**❌ "Delphi coûte cher, c'est pour les grandes entreprises"**
Nuance. Les éditions Pro/Enterprise sont effectivement chères, mais la **Community Edition est gratuite** pour les développeurs individuels et petites équipes (< 5 000 USD/an, ≤ 5 développeurs). De plus, l'alternative open source **Lazarus / FreePascal** offre 90 % des fonctionnalités gratuitement.

**❌ "Object Pascal, c'est dépassé"**
Faux. Le langage est régulièrement enrichi (génériques, anonymous methods, interfaces, attributs, opérateur ternaire…). Il reste l'un des langages compilés natifs les plus accessibles aux débutants tout en étant utilisé en production par de nombreuses entreprises.

**❌ "Personne n'utilise plus Delphi"**
Faux. Delphi est encore très présent dans les domaines de la gestion d'entreprise, du médical, de l'industrie, du commerce de détail et des PME. Des milliers d'entreprises maintiennent activement des applications Delphi en production.

## L'éditeur : Embarcadero

Delphi est aujourd'hui développé et maintenu par **Embarcadero Technologies**, une société spécialisée dans les outils de développement, elle-même filiale du groupe **Idera, Inc.** depuis 2015. Embarcadero continue d'investir dans Delphi en publiant régulièrement de nouvelles versions avec des améliorations et de nouvelles fonctionnalités — historiquement à un rythme d'une version majeure par an, avec des updates intermédiaires.

> **Note historique :** Delphi a été créé en 1995 par **Borland**, l'un des grands noms historiques de l'édition logicielle. La marque a ensuite été transférée à CodeGear (filiale de Borland), puis vendue à Embarcadero en 2008. Nous reviendrons sur cette histoire en détail dans la section suivante.

La version actuelle, **Delphi 13 Florence** (sortie en septembre 2025, suivie de l'update 13.1 en mars 2026), représente l'aboutissement de plus de 30 ans d'évolution et d'amélioration continue de l'outil.

## Pourquoi choisir Delphi ? (aperçu rapide)

Si vous vous demandez pourquoi apprendre Delphi plutôt qu'un autre langage ou outil, voici un aperçu rapide des raisons principales :

- **Productivité élevée** — créer des applications complètes en une fraction du temps nécessaire avec d'autres outils
- **Code natif** — vos applications sont compilées en code machine, garantissant des performances optimales
- **Stabilité et maturité** — Delphi existe depuis 1995 et a fait ses preuves dans d'innombrables projets professionnels
- **Communauté active** — forums, composants tiers, ressources francophones et internationales
- **Pérennité** — les applications Delphi peuvent être maintenues et évoluées sur de très longues périodes
- **Accès aux technologies modernes** — cloud, IA, IoT, Windows on Arm

> 📖 La section **1.4 Avantages et cas d'utilisation** détaille ces points avec des exemples chiffrés, des cas concrets et des comparaisons avec d'autres technologies.

## En résumé

Delphi est bien plus qu'un simple outil de programmation : c'est un environnement complet qui vous accompagne de la conception à la distribution de vos applications. Que vous soyez débutant ou développeur expérimenté, Delphi vous offre les moyens de concrétiser vos idées en applications fonctionnelles et professionnelles.

Dans les sections suivantes, nous explorerons l'histoire de Delphi, ses différentes versions, et nous commencerons à découvrir concrètement son environnement de développement.

⏭️ [Histoire et évolutions](/01-introduction-a-delphi/02-histoire-et-evolutions.md)
