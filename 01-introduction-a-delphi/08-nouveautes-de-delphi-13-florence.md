🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.8 Nouveautés de Delphi 13 Florence

## Introduction

**Delphi 13 Florence** a été lancé le **10 septembre 2025** par Embarcadero, suivi de l'**Update 1 (Delphi 13.1)** publié en **mars 2026**. Ces versions représentent une évolution significative de la plateforme RAD Studio, apportant des changements majeurs dans plusieurs domaines clés : un IDE désormais 64 bits, de nouveaux composants IA, des extensions du langage Object Pascal, et de nombreuses améliorations de VCL, FireMonkey et FireDAC.

Cette section présente les principales innovations de Delphi 13 (et 13.1) de manière accessible, en expliquant concrètement ce qu'elles vous apportent en tant que débutant ou développeur expérimenté.

## IDE 64 bits natif

**La nouveauté la plus structurante de Delphi 13** est sans doute le passage de l'IDE en **véritable application 64 bits**.

### Pourquoi c'est important ?

Pendant 30 ans, l'IDE Delphi est resté une application 32 bits, même quand il compilait des applications 64 bits. Cela imposait une limite mémoire d'environ 4 Go à l'IDE lui-même, ce qui devenait problématique sur de très gros projets ou avec de nombreux packages chargés simultanément.

### Ce que cela change concrètement

- **Plus de limite mémoire** : l'IDE peut maintenant utiliser toute la RAM disponible
- **Meilleure stabilité** sur les gros projets
- **Possibilité de charger plus de packages et bibliothèques** en même temps
- **Préparation à l'avenir** : Windows tend vers le tout 64 bits

### Implication pour les composants tiers

Les packages de composants doivent désormais être disponibles en version 64 bits pour être chargés dans l'IDE. La plupart des éditeurs majeurs (TMS, DevExpress, FastReport, etc.) ont publié des versions compatibles dès la sortie de Delphi 13.

## Composants IA intégrés (SmartCore AI)

L'une des grandes nouveautés de Delphi 13 est l'intégration de composants IA prêts à l'emploi, baptisés **SmartCore AI Component Pack**. Ils permettent d'ajouter des capacités d'intelligence artificielle à vos applications sans être expert en machine learning.

### Architecture

Les composants IA suivent une architecture en deux couches :

**1. Composant de connexion (TAIConnection)**
Un composant agnostique du fournisseur, qui s'appuie sur des **drivers** spécifiques pour communiquer en REST avec différents moteurs IA :
- **OpenAI** (GPT-4, GPT-5, etc.)
- **Anthropic Claude** (Sonnet, Opus, Haiku)
- **Google Gemini**
- **Ollama** (pour exécuter des LLM en local, sans cloud)
- Extension possible via API tierces

**2. Composants de requête IA**
Des composants spécialisés selon le type de données traitées :
- Texte (génération, résumé, traduction)
- Images (description, génération, analyse)
- Données structurées (extraction, classification)

Ces composants peuvent être directement **liés à des contrôles d'interface** (Memo, ListBox, Image) pour afficher les résultats sans code de plomberie.

### Mise en route

Pour utiliser l'IA dans votre application :
1. Déposez un `TAIConnection` sur votre formulaire
2. Assignez un driver (OpenAI, Claude, etc.)
3. Configurez via le **Connection Wizard** (double-clic sur le composant)
4. Ajoutez un composant de requête (texte, image, etc.) avec un prompt
5. Liez le résultat à un contrôle visuel

C'est aussi simple que d'ajouter un composant FireDAC pour se connecter à une base de données.

### Exemple de code minimal

Voici un aperçu de ce à quoi ressemble du code utilisant les composants IA (la syntaxe exacte peut varier selon les drivers utilisés, mais l'esprit reste celui-ci) :

```pascal
procedure TForm1.btnPoserQuestionClick(Sender: TObject);  
begin  
  // Préparer la requête
  AITextRequest1.Prompt := 'Résume ce texte en 3 phrases : ' + mmoTexte.Text;
  AITextRequest1.MaxTokens := 200;

  // Exécuter la requête (peut être synchrone ou asynchrone)
  AITextRequest1.Execute;

  // Le résultat apparaît dans la propriété Response
  mmoResultat.Text := AITextRequest1.Response;
end;
```

Avec **liaison de données visuelle** (LiveBindings), vous pouvez même éviter complètement le code et lier directement la propriété `Response` du composant IA à un `TMemo` ou un `TLabel`.

> ⚠️ **Important :** Les requêtes IA vers les services cloud (OpenAI, Claude, Gemini) sont **facturées à l'usage**. Si vous testez intensivement, surveillez votre consommation. Pour le développement et les tests, **Ollama** (local) est une excellente alternative gratuite, à condition d'avoir une machine assez puissante pour faire tourner les modèles.

### Site web companion IA

En complément des composants, Embarcadero a lancé un **site web companion IA** dédié à l'assistance au développement Delphi :
- Aide à comprendre les messages d'erreur Delphi
- Propose des solutions à des problèmes de code Object Pascal
- Donne des exemples de code adaptés à votre version
- Sert de mentor disponible pour les débutants

C'est un complément utile, mais ce n'est pas un assistant intégré dans l'IDE comme peut l'être GitHub Copilot dans VS Code — il s'agit plutôt d'une ressource web séparée.

## Améliorations du langage Object Pascal

Delphi 13 apporte plusieurs extensions au langage Object Pascal, dont la plus attendue depuis très longtemps.

### Opérateur ternaire (avec le mot-clé `if`)

**Qu'est-ce que c'est ?**
L'opérateur ternaire est l'extension de langage la plus demandée par la communauté depuis des années. Delphi 13 l'introduit avec une syntaxe spécifique utilisant le mot-clé `if` en expression (et non l'opérateur `? :` du C/C++/Java).

**Avant Delphi 13 :**
```pascal
var
  Statut: string;
begin
  if Age >= 18 then
    Statut := 'Majeur'
  else
    Statut := 'Mineur';
end;
```

**Avec Delphi 13 :**
```pascal
var
  Statut: string;
begin
  Statut := if Age >= 18 then 'Majeur' else 'Mineur';
end;
```

L'opérateur ternaire peut également être imbriqué et utilisé dans des expressions plus complexes :
```pascal
Mention := if Note >= 16 then 'Très bien'
           else if Note >= 14 then 'Bien'
           else if Note >= 12 then 'Assez bien'
           else 'Passable';
```

**Autres cas d'usage pratiques :**

```pascal
// Dans un appel de fonction
ShowMessage('Vous êtes ' + (if Connecte then 'connecté' else 'déconnecté'));

// Dans un calcul
PrixFinal := PrixBase * (if EstClient then 0.9 else 1.0);  // 10% de remise pour les clients

// Dans une affectation à une propriété
Label1.Caption := if Compteur = 0 then 'Aucun' else IntToStr(Compteur);

// Avec des appels de méthodes
btnEnregistrer.Enabled := if edtNom.Text = '' then False else True;
// (Note : équivalent à btnEnregistrer.Enabled := edtNom.Text <> '';)
```

**Avantage :** code plus compact et plus lisible pour les conditions simples, notamment pour les affectations conditionnelles, les paramètres de fonction, et l'initialisation de propriétés.

> ⚠️ **Bon usage :** L'opérateur ternaire est tentant à abuser. Pour les conditions complexes (plus de 2 niveaux d'imbrication), un `if/then/else` classique reste plus lisible. La règle est : **si vous deviez expliquer la condition à voix haute en une phrase, le ternaire est OK. Sinon, écrivez un `if` classique.**

### Autres améliorations du langage

Delphi 13 et 13.1 apportent également :
- Des **améliorations sur les génériques** (meilleure inférence de type dans certains contextes)
- Des **corrections du compilateur** Delphi Win64 et mobile
- Une **meilleure analyse statique** par le LSP (Language Server Protocol)

Ces améliorations sont incrémentales mais améliorent la qualité du code généré et l'expérience de l'éditeur.

## Environnement de développement modernisé

L'IDE lui-même a été considérablement amélioré dans Delphi 13.

### Support LLDB v12

**Qu'est-ce que c'est ?**
LLDB est le débogueur utilisé pour les applications mobiles (iOS et Android). La version 12 apporte des améliorations majeures.

**Ce que ça change pour vous :**
- Débogage plus rapide et plus stable
- Inspection des variables plus précise
- Moins de plantages pendant le débogage
- Meilleure intégration avec les outils natifs

**Pour les débutants :**
Si vous développez des applications mobiles, trouver et corriger les bugs devient beaucoup plus facile. Le débogueur vous aide à comprendre ce qui se passe dans votre code en temps réel.

### Gestionnaire de packages amélioré (GetIt)

**Nouveautés :**
- Interface plus intuitive et moderne
- Recherche améliorée de composants
- Gestion des versions plus claire (chapitre 2.10 de la formation détaillera ce point)
- Installation plus rapide et fiable
- Catégorisation des packages (composants UI, accès données, IA, etc.)

**Pour les débutants :**
Trouver et installer des composants supplémentaires (comme des graphiques, des outils PDF, etc.) se fait via une interface simple, similaire à un magasin d'extensions. Notez cependant que certains packages nécessitent une licence Pro ou Enterprise.

### Performances de l'IDE

**Améliorations notables :**
- Démarrage plus rapide de l'IDE
- Compilation plus rapide des projets
- Moins de consommation mémoire
- Interface plus réactive
- Gestion de gros projets optimisée

**Impact concret :**
Vous passez moins de temps à attendre et plus de temps à développer. L'IDE reste fluide même avec plusieurs projets ouverts.

## Améliorations VCL (applications Windows)

La VCL, utilisée pour créer des applications Windows, reçoit de nombreuses améliorations dans Delphi 13.

### Styles VCL en mode conception

Vous pouvez maintenant **voir les styles visuels directement pendant la conception** de votre interface, pas seulement à l'exécution.

**Qu'est-ce que cela signifie ?**
Quand vous créez votre interface, vous voyez immédiatement à quoi elle ressemblera avec le thème sombre, clair, ou tout autre style visuel choisi.

**Avantage pour le prototypage :**
- Conception visuelle plus rapide
- Moins d'allers-retours entre conception et exécution pour vérifier l'apparence
- Meilleure idée du résultat final dès le début
- Ajustements en temps réel

### Intégration de UIAutomation

**Première dans la VCL :** Delphi 13 intègre les interfaces **Microsoft UIAutomation** dans la VCL. Cette intégration sert deux objectifs majeurs :

1. **Accessibilité** : les applications VCL deviennent plus accessibles aux utilisateurs de technologies d'assistance (lecteurs d'écran comme NVDA, JAWS, Narrator)
2. **Tests UI automatisés** : possibilité de piloter une application VCL par des outils de test automatisés (TestComplete, Ranorex, ou des frameworks de test UI personnalisés)

C'est une avancée importante pour les entreprises soumises à des obligations légales d'accessibilité (RGAA, ADA, EN 301 549) et pour les équipes pratiquant les tests UI automatisés.

### TitleBar styling

Les **barres de titre** des fenêtres peuvent désormais être personnalisées avec les styles VCL, y compris des couleurs personnalisées et l'intégration de composants dans la barre de titre elle-même (comme dans les applications modernes type Office, VS Code, etc.).

### Composants VCL améliorés

**ControlList :**
- Nouveau type de contrôle hôte : **SplitButton**
- Nouvel événement `OnGetItemHint` et propriété `ShowItemHint` pour des bulles d'aide par élément (et non globales)

**ToggleSwitch :**
- L'apparence du contrôle a été modernisée pour s'aligner sur le toggle switch natif de Windows 11

**FormsTabsBar :**
- Améliorations pour les interfaces type "navigateur" avec onglets de formulaires

**Autres améliorations VCL :**
- Optimisations du moteur de rendu HiDPI
- Corrections sur les écrans haute densité (4K, 8K)
- Améliorations diverses sur Grid, TreeView, ListView

## Améliorations FireMonkey (multi-plateforme)

FireMonkey, le framework pour les applications multi-plateformes, reçoit aussi son lot d'améliorations.

### Performance graphique

**Optimisations :**
- Rendu plus rapide des interfaces
- Animations plus fluides
- Meilleure utilisation du GPU
- Moins de consommation batterie sur mobile

**Impact visible :**
Vos applications mobiles et desktop tournent plus rapidement et consomment moins de ressources.

### Support Linux amélioré

**FMXLinux évolue :**
- Meilleure compatibilité avec les distributions Linux modernes (Ubuntu, Debian, Fedora récents)
- Installation simplifiée via GetIt
- Corrections de bugs et améliorations de composants
- Meilleure intégration avec les bibliothèques GTK et les pilotes graphiques

**Pour qui ?**
Si vous ciblez Linux desktop, vos applications auront une meilleure intégration avec l'environnement système. Pour Linux serveur (sans GUI), le compilateur Delphi Linux est utilisable depuis Delphi 10.2 Tokyo.

### Nouveaux styles et effets

**Enrichissement visuel :**
- Nouveaux styles visuels modernes
- Effets visuels supplémentaires (flou, ombres, transparence)
- Animations prédéfinies
- Thèmes personnalisables

**Résultat :**
Créer des applications visuellement attrayantes devient encore plus facile.

## Améliorations FireDAC (bases de données)

FireDAC, le composant d'accès aux bases de données, est renforcé dans Delphi 13.

### Support de nouvelles versions

**Mises à jour :**
- MySQL 8.4 et MariaDB 11.x
- PostgreSQL 16
- SQLite 3.45
- MongoDB 7.x
- Autres moteurs mis à jour

**Importance :**
Vous pouvez utiliser les dernières versions de votre base de données préférée avec toutes leurs nouvelles fonctionnalités.

### Performances améliorées

**Optimisations :**
- Requêtes plus rapides
- Meilleure gestion de la mémoire
- Transactions optimisées
- Connexions pooling amélioré

**Impact :**
Vos applications manipulant des bases de données sont plus réactives, surtout avec de gros volumes de données.

### Nouvelles fonctionnalités

**Ajouts pratiques :**
- Support amélioré du JSON dans les requêtes
- Meilleure gestion des types de données modernes
- Outils de migration de schéma
- Logging et débogage SQL améliorés

**Pour les débutants :**
Travailler avec des bases de données devient encore plus intuitif avec des assistants et des outils visuels améliorés.

## Développement mobile

Les capacités mobiles de Delphi 13 sont renforcées.

### Support des dernières versions

**iOS :**
- Support des versions récentes d'iOS (mise à jour continue via les updates)
- Optimisation pour les derniers iPhone
- Support des nouvelles API Apple (App Tracking Transparency, App Store Connect, etc.)
- Conformité avec les exigences App Store

**Android :**
- Support des dernières API Android
- Optimisation pour les derniers SDK (Android SDK 25.2.5 / NDK r27b par défaut)
- Conformité avec les exigences Google Play
- Update 13.1 : mise à jour vers les SDK plus récents

**Importance :**
Vos applications peuvent utiliser les dernières fonctionnalités des systèmes mobiles et sont conformes aux exigences en cours des App Store et Google Play (qui imposent régulièrement de cibler une version minimale d'API).

### Permissions et sécurité

**Gestion modernisée :**
- Gestion plus claire des permissions Android (Runtime Permissions)
- Meilleure conformité RGPD pour la collecte de données utilisateur
- Outils de chiffrement et de stockage sécurisé
- Intégration avec les Keystore système (iOS Keychain, Android Keystore)

## Développement web

Delphi 13 étend significativement ses capacités pour le développement web côté serveur.

### WebStencils

**WebStencils** est un **moteur de templates côté serveur** qui étend les technologies web existantes de RAD Studio (WebBroker, DataSnap, RAD Server). Il transforme RAD Server, qui n'était jusqu'à présent qu'un moteur de Web Services, en un véritable outil de génération de sites web dynamiques.

**Fonctionnalités principales :**
- **Données de session** (Session data) et **variables globales**
- Nouvelle instruction **switch** dans les templates
- Système d'**authentification de session** intégré avec mécanisme d'**autorisation** associé
- **Accès direct aux propriétés de datasets** avec contrôles de sécurité
- Génération HTML dynamique simplifiée
- Intégration avec des frameworks JavaScript modernes (HTMX, Alpine.js, etc.)

**Cas d'usage typique :**
Créer un site web administratif piloté par des données Delphi (FireDAC), avec authentification, sessions utilisateurs, sans devoir écrire un backend séparé en Node.js ou ASP.NET.

**Aperçu syntaxique :**
Un template WebStencils ressemble à du HTML enrichi de directives, dans l'esprit de Razor (ASP.NET), Jinja (Python) ou Twig (PHP) :

```html
<!DOCTYPE html>
<html>
<head><title>Liste des clients</title></head>
<body>
  @if (Session.Authenticated) {
    <h1>Bonjour, @Session.Username !</h1>
    <table>
      <tr><th>ID</th><th>Nom</th><th>Email</th></tr>
      @foreach (var client in DataSets.Clients) {
        <tr>
          <td>@client.ID</td>
          <td>@client.Nom</td>
          <td>@client.Email</td>
        </tr>
      }
    </table>
  } else {
    <a href="/login">Connectez-vous</a>
  }
</body>
</html>
```

Côté Delphi, vous fournissez les données (`DataSets.Clients`) via FireDAC, et WebStencils s'occupe du rendu HTML.

## Cloud et services modernes

Delphi 13 s'intègre mieux avec les technologies cloud actuelles.

### Intégration cloud native

**Services supportés :**
- AWS (Amazon Web Services)
- Azure (Microsoft)
- Google Cloud Platform
- Services Firebase
- Stockage cloud (S3, Azure Blob, etc.)

**Composants fournis :**
Des composants prêts à l'emploi pour s'authentifier et utiliser ces services sans configuration complexe.

### Conteneurisation

**Support Docker :**
- Création d'images Docker pour vos applications Delphi
- Déploiement facilité en conteneurs
- Support Kubernetes pour l'orchestration

**Pour les débutants :**
Bien que plus avancé, Delphi 13 facilite le déploiement moderne de vos applications dans le cloud.

## Outils de productivité

Des outils qui vous font gagner du temps au quotidien.

### Refactoring amélioré

**Nouvelles capacités :**
- Renommage intelligent de variables dans tout le projet
- Extraction automatique de méthodes
- Réorganisation du code facilitée
- Suggestions d'optimisation

**Avantage :**
Nettoyer et réorganiser votre code devient simple et sûr.

### Recherche et navigation

**Améliorations :**
- Recherche plus rapide dans les projets (LSIF en 13.1)
- Filtrage des résultats par type (symbole, fichier, commande)
- Navigation par symboles améliorée
- **IDE Insight (Ctrl+.)** : recherche universelle dans tout l'IDE
- Indexation LSP plus performante sur les gros projets

**Impact :**
Trouver une fonction ou une variable dans un gros projet prend des secondes au lieu de minutes.

### Collaboration en équipe

**Nouveautés :**
- Meilleure intégration Git
- Support GitHub/GitLab amélioré
- Outils de revue de code
- Partage de configurations d'équipe

**Pour les équipes :**
Travailler à plusieurs sur un même projet Delphi devient plus fluide.

## Documentation et apprentissage

Delphi 13 améliore les ressources pour apprendre.

### Documentation enrichie

**Améliorations :**
- Documentation mise à jour pour toutes les nouveautés
- Nouveaux exemples de code pour les fonctionnalités IA, WebStencils, UIAutomation
- Site web companion IA en complément

**Accès :**
Toujours disponible via F1 dans l'IDE. La documentation officielle reste sur DocWiki en ligne ([docwiki.embarcadero.com](https://docwiki.embarcadero.com/)).

### Exemples de code modernisés

**Bibliothèque d'exemples installée localement :**
- Exemples pour les nouvelles fonctionnalités (IA, WebStencils, UIAutomation)
- Projets complets prêts à étudier
- Exemples mobile, cloud, BD
- Disponible dans `C:\Users\Public\Documents\Embarcadero\Studio\24.0\Samples`

**Source GitHub :**
Embarcadero maintient également des exemples publics sur [github.com/Embarcadero](https://github.com/Embarcadero), souvent mis à jour entre les versions.

## Performances et stabilité

Des améliorations moins visibles mais cruciales.

### Compilateur et qualité

**Améliorations :**
- Compilation plus rapide sur de nombreux projets
- Code généré plus performant sur certaines architectures (notamment Win64)
- Meilleure gestion mémoire dans le RTL
- Toolchain Windows on Arm basé sur LLVM 20 (13.1)

**Impact :**
Vos applications compilent et tournent plus rapidement, notamment sur les nouvelles architectures.

### Stabilité accrue

Embarcadero a particulièrement insisté sur la qualité dans Delphi 13 :
- Nombreuses corrections de bugs (suivi via le Quality Portal)
- IDE plus stable, notamment avec l'IDE 64 bits qui élimine les problèmes mémoire
- Améliorations du débogueur, en particulier sur mobile (LLDB v12)
- Meilleure gestion des gros projets dans l'éditeur grâce au LSP étendu

## Accessibilité

Delphi 13 améliore l'accessibilité pour tous.

### Support des lecteurs d'écran

**Améliorations :**
- Meilleure compatibilité avec JAWS et NVDA
- Support des technologies d'assistance
- Navigation clavier améliorée
- Propriétés d'accessibilité pour les composants visuels

**Importance :**
Vos applications sont accessibles aux utilisateurs ayant des besoins spécifiques.

### Interface adaptable

**Nouveautés :**
- Support de la mise à l'échelle de l'interface
- Thèmes à fort contraste
- Personnalisation de la taille de police
- Raccourcis clavier personnalisables

**Pour tous :**
Chacun peut adapter Delphi à ses besoins et préférences.

## Migration depuis les versions précédentes

Delphi 13 facilite la transition depuis les versions antérieures.

### Compatibilité de code

**En général :**
- La plupart du code **Delphi 11 / Delphi 12** compile directement dans Delphi 13 sans modification
- Le code **Delphi 10.x** nécessite généralement des ajustements mineurs
- Le code **Delphi 2009 à 7** demande plus de travail (essentiellement à cause des changements Unicode, ARC mobile, et de l'évolution des bibliothèques)

**Points d'attention courants lors d'une migration :**
- **Packages tiers** : doivent être disponibles en versions compatibles Delphi 13 (notamment 64 bits pour l'IDE)
- **Code mobile pré-10.4** : adaptation à la suppression de l'ARC (passage de `:= nil` à `Free`)
- **Composants obsolètes** : certains composants très anciens peuvent avoir été retirés

**Outils d'aide à la migration :**
- L'IDE détecte automatiquement les incompatibilités et affiche des erreurs claires
- Le **Quality Portal** d'Embarcadero documente les changements de compatibilité version par version
- Marco Cantù (le Product Manager Delphi) publie régulièrement des guides de migration sur son blog

**Rassurant :** vos compétences et projets existants restent largement valables. La rétrocompatibilité Delphi est l'une des meilleures de l'industrie.

## Éditions et licences

Delphi 13 maintient la structure des quatre éditions (Community, Professional, Enterprise, Architect).

### Community Edition mise à jour

**Bonne nouvelle :**
La Community Edition gratuite inclut la plupart des nouvelles fonctionnalités, notamment :
- L'opérateur ternaire et les améliorations du langage
- Les nouveaux styles VCL en mode conception
- L'IDE 64 bits
- Les améliorations de performance
- Le développement multi-plateformes : **Windows, macOS, Android et iOS** (contrairement à une idée reçue, la Community Edition supporte le mobile depuis longtemps)

**Vraies limitations :**
- Pas de Linux
- Pas de DataSnap / RAD Server complet
- Conditions d'éligibilité (revenus < 5 000 USD/an, équipe ≤ 5 développeurs)
- Pas de support technique officiel

## Delphi 13.1 Florence (Update 1) — mars 2026

L'update 1 de Delphi 13 (souvent simplement appelé "13.1") a été publié en **mars 2026** et apporte des nouveautés majeures supplémentaires. Embarcadero a profité de cette update pour pousser plusieurs fonctionnalités importantes au lieu d'attendre Delphi 14.

### Compilateur Windows on Arm (Arm64EC)

**La plus grosse nouveauté de 13.1** : un **nouveau compilateur natif** pour la plateforme **Windows on Arm**.

**Qu'est-ce que Arm64EC ?**
Arm64EC ("Emulation Compatible") est une ABI introduite par Microsoft qui permet à du code natif Arm64 de cohabiter et d'interopérer avec du code x64 émulé dans une même application. C'est la voie recommandée par Microsoft pour porter les applications Windows existantes sur les machines Arm (notamment les nouveaux Copilot+ PCs équipés de processeurs Snapdragon X et les machines à processeur Apple M sous Windows via Parallels).

**Ce que cela vous apporte :**
- Compiler vos applications Delphi en code **natif Arm64** depuis la même base de code
- Performance maximale sur les machines Windows Arm (pas d'émulation x86/x64)
- Le toolchain Delphi pour Windows on Arm est bâti sur **LLVM 20**
- La plateforme Win64 (Arm) est disponible dans le **Platform Manager** aux côtés de Win32 et Win64 (Intel)

### FireMonkey Style Designer

Un **nouvel outil standalone** pour concevoir les styles FireMonkey, qui sort enfin de l'édition objet-par-objet pour proposer un **workflow centré design**.

**Caractéristiques :**
- Gestion globale des couleurs, typographies, et états d'interaction (hover, pressed, etc.)
- Aperçu en temps réel sur toutes les plateformes
- Workflow inspiré des outils de design modernes (Figma, Sketch)
- Beaucoup plus rapide pour créer ou modifier un style global d'application

C'est une amélioration très attendue par les développeurs FireMonkey qui souhaitaient gagner du temps sur la personnalisation visuelle.

### HTTP Server-Sent Events (SSE)

Delphi 13.1 introduit le support des **HTTP Server-Sent Events** côté serveur **et** côté client. SSE est une alternative aux WebSockets pour les communications **serveur → client en push**, idéale pour :
- Notifications en temps réel
- Suivi de progression de tâches longues
- Flux de logs en direct
- Mise à jour d'interfaces (dashboards, monitoring)
- Streaming de réponses de LLM (chat IA en temps réel)

Nouvelles classes ajoutées :
- `TWebResponseStream`
- `THTTPEvent`
- `THTTPEventSource`

**Comparaison SSE vs WebSocket :**

| Critère | SSE | WebSocket |
|---------|-----|-----------|
| Direction | Serveur → Client (unidirectionnel) | Bidirectionnel |
| Protocole | HTTP standard | Protocole spécifique (Upgrade depuis HTTP) |
| Reconnexion automatique | ✓ Native | À gérer manuellement |
| Traversée proxy / firewall | Excellente (HTTP) | Peut poser problème |
| Complexité | Simple | Plus complexe |
| Bon pour | Notifications, streaming, dashboards | Chat, jeux temps réel, collaboratif |

**Aperçu côté client (réception d'événements) :**

```pascal
var
  EventSource: THTTPEventSource;
begin
  EventSource := THTTPEventSource.Create(Self);
  EventSource.URL := 'https://api.exemple.com/events';

  EventSource.OnMessage := procedure(const Event: THTTPEvent)
  begin
    // Appelé à chaque message reçu du serveur
    mmoLogs.Lines.Add(Event.Data);
  end;

  EventSource.OnError := procedure(const Error: string)
  begin
    ShowMessage('Erreur de connexion : ' + Error);
  end;

  EventSource.Connect;  // Connexion non-bloquante, reconnexion automatique
end;
```

Cela permet aussi d'implémenter des protocoles modernes comme **MCP** (Model Context Protocol) qu'utilisent les agents IA.

### Moteur LSP étendu (LSIF)

Le moteur **DelphiLSP** (Language Server Protocol) qui alimente le Code Insight a été étendu avec le support de **LSIF** (Language Server Index Format), un format d'index développé par Microsoft.

**Bénéfices :**
- Réduction de la dépendance du Code Insight au compilateur
- **Performance** améliorée
- **Stabilité** accrue
- **Précision** des suggestions de complétion et de navigation
- Indexation plus rapide des gros projets

### Support de nouvelles bases de données

Delphi 13.1 ajoute le support officiel de :
- **SAP ASE Server 16.1**
- **IBM DB2 12.1**
- **MariaDB Server 12.1**

### Mises à jour des plateformes mobiles

- Support des derniers SDK **iOS** et **Android**
- Améliorations du débogueur LLDB pour le mobile

## Ce que cela signifie pour vous, débutant

Si vous commencez avec Delphi 13 Florence, vous bénéficiez immédiatement de :

**1. Assistance disponible**
Le site web companion IA et la documentation enrichie facilitent l'apprentissage et la résolution de problèmes courants. C'est un complément utile (mais pas un remplacement) à la communauté et aux forums.

**2. Environnement moderne**
L'IDE 64 bits est plus stable et capable de gérer de gros projets sans limitation mémoire.

**3. Outils à jour**
Support des dernières plateformes (Windows 11, Windows on Arm, macOS récents, dernières versions iOS et Android). Les versions précises supportées évoluent à chaque update — consultez la documentation officielle pour la liste à jour.

**4. Meilleure expérience d'apprentissage**
Documentation enrichie, exemples modernisés, site web companion IA pour répondre à vos questions.

**5. Performance immédiate**
Delphi 13 produit des exécutables natifs performants **par défaut**, sans configuration complexe. Pas besoin d'optimiser manuellement pour obtenir un démarrage rapide et une faible empreinte mémoire.

**6. Vision d'avenir**
Vous apprenez sur une plateforme qui continue d'évoluer et d'intégrer les dernières tendances (IA, Arm, web moderne).

## Feuille de route future

Embarcadero communique régulièrement sur sa **roadmap** (généralement deux fois par an). À l'heure de la rédaction, les axes annoncés pour les versions à venir incluent :
- Plus de **composants IA** et d'intégrations avec les services LLM
- **Compilateurs LLVM** modernisés sur plus de plateformes
- **macOS sur Apple Silicon** : support continu et optimisations
- Plus de fonctionnalités **WebStencils** pour le web côté serveur
- Améliorations du **moteur LSP** et du Code Insight
- **Modernisation de la VCL** et de FireMonkey (composants, styles)

> 📅 Pour suivre la roadmap officielle : [embarcadero.com/products/rad-studio/product-roadmap](https://www.embarcadero.com/products/rad-studio/product-roadmap)

## Récapitulatif des nouveautés clés

| Catégorie | Nouveauté | Version |
|-----------|-----------|---------|
| IDE | **IDE 64 bits natif** (plus de limite mémoire) | 13.0 |
| Langage | **Opérateur ternaire** (`if ... then ... else` en expression) | 13.0 |
| IA | **SmartCore AI Component Pack** (TAIConnection : OpenAI, Claude, Gemini, Ollama) | 13.0 |
| IA | Site web companion IA | 13.0 |
| VCL | **UIAutomation** intégrée (accessibilité + tests UI automatisés) | 13.0 |
| VCL | TitleBar styling | 13.0 |
| VCL | Styles VCL en mode conception | 13.0 |
| VCL | Améliorations ControlList, ToggleSwitch, FormsTabsBar | 13.0 |
| Mobile | Support LLDB v12 | 13.0 |
| Web | **WebStencils** (sessions, auth, datasets) | 13.0 |
| BD | Améliorations FireDAC | 13.0 |
| Compilateur | **Windows on Arm (Arm64EC)** natif | 13.1 |
| FMX | **FireMonkey Style Designer** standalone | 13.1 |
| Réseau | HTTP **Server-Sent Events (SSE)** | 13.1 |
| LSP | Support **LSIF** (indexation rapide) | 13.1 |
| BD | Support SAP ASE 16.1, IBM DB2 12.1, MariaDB 12.1 | 13.1 |

## En résumé

Delphi 13 Florence et son update 13.1 représentent une étape majeure dans l'évolution de Delphi, apportant :

- **L'IDE 64 bits natif**, qui élimine les limites mémoire historiques
- **L'opérateur ternaire**, extension de langage réclamée depuis des années
- **Les composants IA SmartCore**, ouvrant la porte aux applications intégrant les LLM
- **Le compilateur Windows on Arm**, préparant Delphi aux machines de demain
- **WebStencils**, modernisant le développement web côté serveur
- **L'intégration UIAutomation**, alignant la VCL sur les standards modernes d'accessibilité et de test

Pour un débutant commençant aujourd'hui avec Delphi, la version 13 offre un excellent point d'entrée. Vous apprenez sur une plateforme mature qui intègre les technologies modernes (IA, ARM, Web moderne) tout en conservant la simplicité et la rapidité qui ont fait le succès de Delphi depuis 1995.

Pour un développeur expérimenté venant d'une version antérieure, la migration vers Delphi 13 vaut largement le coup pour bénéficier de l'IDE 64 bits, du nouvel opérateur ternaire, et des composants IA — particulièrement si votre application doit cibler à terme les machines Windows on Arm.

⏭️ [Découverte de l'IDE Delphi](/02-decouverte-de-lide-delphi/README.md)
