🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.1 Introduction à Intraweb et TMS Web Core

## Introduction

Le développement d'applications web avec Delphi offre plusieurs possibilités pour créer des sites web dynamiques et des applications web complètes. Deux solutions principales se démarquent dans l'écosystème Delphi : **IntraWeb** et **TMS Web Core**. Ces frameworks permettent aux développeurs Delphi de créer des applications web en utilisant leurs compétences existantes en Object Pascal.

## Qu'est-ce qu'IntraWeb ?

### Présentation générale

IntraWeb (souvent abrégé IW) est un framework RAD (Rapid Application Development) pour le développement d'applications web avec Delphi. Il permet de créer des applications web de la même manière que vous créeriez des applications desktop avec la VCL.

### Philosophie d'IntraWeb

IntraWeb adopte une approche **côté serveur** du développement web. Voici ce que cela signifie :

- Votre code Delphi s'exécute sur le serveur
- Les formulaires et composants sont convertis en HTML/JavaScript automatiquement
- La logique métier reste entièrement sur le serveur
- L'interface utilisateur est générée dynamiquement pour chaque client

### Avantages d'IntraWeb

**Pour les développeurs Delphi :**
- Utilisation de composants visuels familiers (comme dans la VCL)
- Conception drag-and-drop dans l'IDE Delphi
- Pas besoin d'apprendre HTML, CSS ou JavaScript en profondeur
- Réutilisation des connaissances VCL existantes

**Pour l'architecture :**
- Code métier sécurisé (reste sur le serveur)
- Gestion automatique des sessions utilisateur
- Support des applications multi-utilisateurs
- Déploiement centralisé

### Architecture d'IntraWeb

```
┌─────────────────┐
│   Navigateur    │ ← Client (HTML/CSS/JS généré)
└────────┬────────┘
         │ HTTP/HTTPS
         ↓
┌─────────────────┐
│  Serveur IW     │ ← Votre application Delphi
│  (Delphi/Pascal)│    s'exécute ici
└─────────────────┘
```

### Types d'applications IntraWeb

**Applications Standalone**
- Application serveur autonome
- Écoute sur un port (ex: 8080)
- Idéale pour intranet ou petites applications

**Applications ISAPI/Apache**
- S'intègre avec IIS ou Apache
- Pour déploiement production à grande échelle
- Meilleure performance et scalabilité

### Composants IntraWeb typiques

IntraWeb fournit des composants similaires à la VCL :

- **IWEdit** : champ de saisie texte (équivalent de TEdit)
- **IWButton** : bouton cliquable
- **IWLabel** : étiquette de texte
- **IWGrid** : grille pour afficher des données tabulaires
- **IWRegion** : conteneur pour organiser d'autres composants
- **IWForm** : formulaire (page web)

## Qu'est-ce que TMS Web Core ?

### Présentation générale

TMS Web Core est une solution plus moderne développée par TMS Software. Elle permet de créer des **applications web client** qui s'exécutent directement dans le navigateur.

### Philosophie de TMS Web Core

TMS Web Core adopte une approche **côté client** radicalement différente :

- Votre code Pascal est **transcompilé en JavaScript**
- L'application s'exécute entièrement dans le navigateur
- Pas besoin de serveur Delphi en production
- Architecture similaire aux applications JavaScript modernes (Angular, React, Vue)

### Avantages de TMS Web Core

**Pour les développeurs :**
- Écriture en Object Pascal (langage familier)
- Composants visuels compatibles avec l'IDE
- Accès direct aux API web modernes (HTML5, CSS3)
- Debugging dans l'IDE Delphi

**Pour l'architecture :**
- Pas de serveur d'application nécessaire
- Hébergement simple (serveur web statique suffit)
- Application web moderne et réactive
- Possibilité de créer des Progressive Web Apps (PWA)

### Architecture de TMS Web Core

```
┌─────────────────────────────────┐
│        Navigateur               │
│  ┌───────────────────────────┐  │
│  │  Votre application        │  │
│  │  (JavaScript généré       │  │
│  │   depuis Pascal)          │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
         │
         │ API REST (optionnel)
         ↓
┌─────────────────┐
│  Backend        │ ← Peut être n'importe quoi
│  (optionnel)    │    (Delphi, Node.js, PHP...)
└─────────────────┘
```

### Le processus de transcompilation

Lorsque vous compilez avec TMS Web Core :

1. Vous écrivez du code en Object Pascal
2. Le compilateur TMS Web Core le transforme en JavaScript
3. Le JavaScript généré s'exécute dans le navigateur
4. L'application fonctionne comme une application web moderne

**Exemple conceptuel :**
```pascal
// Votre code Pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bonjour !');
end;
```

Devient approximativement :
```javascript
// JavaScript généré
function TForm1_Button1Click(Sender) {
  alert('Bonjour !');
}
```

### Composants TMS Web Core

TMS Web Core offre des composants web natifs :

- **TWebEdit** : champ de saisie
- **TWebButton** : bouton
- **TWebLabel** : étiquette
- **TWebPanel** : panneau conteneur
- **TWebHttpRequest** : pour appels API REST
- **TWebDBGrid** : grille liée aux données

## Comparaison IntraWeb vs TMS Web Core

### Tableau comparatif

| Critère | IntraWeb | TMS Web Core |
|---------|----------|--------------|
| **Exécution** | Côté serveur | Côté client (navigateur) |
| **Langage final** | Reste en Pascal | Transcompilé en JavaScript |
| **Hébergement** | Nécessite serveur Delphi | Simple serveur web (Apache, nginx) |
| **Performance réseau** | Plus d'échanges serveur | Application locale dans navigateur |
| **Sécurité code** | Code protégé sur serveur | JavaScript visible |
| **Hors ligne** | Non possible | Possible (avec PWA) |
| **Coût hébergement** | Plus élevé (serveur applicatif) | Minimal (fichiers statiques) |
| **Scalabilité** | Limitée par ressources serveur | Excellente (charge sur clients) |

### Quand utiliser IntraWeb ?

IntraWeb est idéal quand :

- Vous devez protéger votre code métier
- Vous avez une application intranet d'entreprise
- Vous voulez un contrôle total côté serveur
- Vous migrez une application VCL existante
- Les utilisateurs ont une bonne connexion réseau
- Vous préférez l'approche traditionnelle serveur

### Quand utiliser TMS Web Core ?

TMS Web Core est préférable quand :

- Vous voulez créer une application web moderne
- Vous souhaitez minimiser les coûts d'hébergement
- Vous visez une excellente réactivité utilisateur
- Vous voulez créer une PWA
- Vous devez supporter le mode hors ligne
- Vous voulez une architecture découplée (frontend/backend)
- La scalabilité est importante

## Approches hybrides

Il est possible de combiner les deux approches :

- **Frontend TMS Web Core** : interface utilisateur moderne
- **Backend Delphi** : services REST créés avec Delphi (RAD Server, Horse, etc.)

Cette architecture moderne sépare clairement :
- La présentation (navigateur)
- La logique métier (serveur REST)
- Les données (base de données)

## Installation et prérequis

### IntraWeb

- Inclus dans certaines éditions de Delphi (Professional et supérieures)
- Installation via GetIt Package Manager
- Disponible aussi en version standalone payante

### TMS Web Core

- Produit commercial de TMS Software
- Version d'essai disponible
- Installation via installer dédié
- Nécessite une licence pour production

## Écosystème et support

### IntraWeb

- Existe depuis plus de 20 ans
- Large base de code existant
- Documentation étendue
- Communauté active
- Nombreux composants tiers disponibles

### TMS Web Core

- Solution plus récente (depuis 2018)
- En évolution rapide
- Documentation moderne
- Support actif de TMS Software
- Intégration avec écosystème web moderne

## Concepts clés à retenir

### Pour IntraWeb

1. **Programmation événementielle serveur** : Les événements (clics, changements) génèrent des requêtes au serveur
2. **Sessions** : Chaque utilisateur a une session maintenue par le serveur
3. **Génération HTML automatique** : Vous concevez visuellement, IntraWeb génère le HTML
4. **Déploiement serveur** : Votre application Delphi doit tourner sur un serveur

### Pour TMS Web Core

1. **Application monopage (SPA)** : L'application entière se charge une fois
2. **Transcompilation** : Votre code Pascal devient du JavaScript
3. **API REST** : Communication avec backend via services web
4. **Déploiement statique** : Fichiers HTML/JS/CSS à déployer

## Conclusion

IntraWeb et TMS Web Core représentent deux philosophies différentes du développement web avec Delphi :

- **IntraWeb** suit le modèle traditionnel serveur, idéal pour applications d'entreprise et intranet
- **TMS Web Core** embrasse l'approche moderne client-side, parfait pour applications web contemporaines

Le choix entre les deux dépend de vos besoins spécifiques, de votre infrastructure et de vos objectifs. Les deux permettent aux développeurs Delphi de créer des applications web sans abandonner l'écosystème et les compétences qu'ils maîtrisent.

Dans les sections suivantes de cette formation, nous explorerons en détail comment créer des applications concrètes avec chacune de ces technologies.

⏭️ [Applications Web basées sur VCL](/23-conception-dapplications-web-avec-delphi/02-applications-web-basees-sur-vcl.md)
