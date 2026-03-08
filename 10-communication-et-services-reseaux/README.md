🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 10 : Communication et services réseaux

## Introduction

Bienvenue dans ce chapitre consacré à la **communication réseau** et aux **services web** avec Delphi. Dans le monde moderne, presque aucune application ne fonctionne de manière isolée. Que ce soit pour consulter des données en ligne, synchroniser des informations entre différents appareils, ou permettre à plusieurs utilisateurs de collaborer en temps réel, la communication réseau est devenue essentielle.

### Pourquoi la communication réseau est-elle importante ?

**Hier : Applications isolées**
```
┌─────────────────┐
│   Application   │
│                 │
│  ┌──────────┐   │
│  │  Données │   │
│  │  locales │   │
│  └──────────┘   │
│                 │
└─────────────────┘
Tout est sur un seul ordinateur
```

**Aujourd'hui : Applications connectées**
```
┌──────────────┐      ┌──────────────┐      ┌──────────────┐
│ Application  │      │   Serveur    │      │   Services   │
│   Mobile     │◄────►│     Web      │◄────►│   Externes   │
└──────────────┘      └──────────────┘      └──────────────┘
       ▲                     ▲                     ▲
       │                     │                     │
       ▼                     ▼                     ▼
┌──────────────┐      ┌──────────────┐      ┌──────────────┐
│ Application  │      │   Base de    │      │    Cloud     │
│   Desktop    │      │   données    │      │   Storage    │
└──────────────┘      └──────────────┘      └──────────────┘

Tout est interconnecté
```

**Les avantages des applications connectées :**

1. **Accès aux données partout**
   - Vos données sur tous vos appareils
   - Synchronisation automatique
   - Travail collaboratif possible

2. **Partage de ressources**
   - Plusieurs utilisateurs simultanés
   - Données centralisées
   - Une seule source de vérité

3. **Services externes**
   - Intégration de fonctionnalités avancées (paiement, cartographie, IA)
   - Pas besoin de tout développer soi-même
   - Mise à jour sans modifier l'application

4. **Scalabilité**
   - Gérer des millions d'utilisateurs
   - Ajouter des serveurs selon les besoins
   - Distribution de la charge

5. **Temps réel**
   - Notifications instantanées
   - Mises à jour en direct
   - Collaboration simultanée

### Les différents types de communication réseau

Dans ce chapitre, vous découvrirez plusieurs façons de faire communiquer vos applications Delphi avec le monde extérieur :

#### 1. **APIs REST (Representational State Transfer)**

Le standard le plus populaire pour les services web modernes.

**Exemple concret :**
Votre application de météo interroge un serveur pour obtenir les prévisions :
```
Application → GET http://api.meteo.com/previsions/paris  
Serveur → { "ville": "Paris", "temperature": 18, "meteo": "Ensoleillé" }  
```

**Caractéristiques :**
- Simple et intuitif
- Utilise HTTP (comme les sites web)
- Format JSON pour les données
- Stateless (sans état)

**Cas d'usage :**
- Récupérer des données depuis Internet
- Interagir avec des services tiers (Google, Facebook, etc.)
- Créer sa propre API pour une application mobile

#### 2. **Sockets et TCP/IP**

Communication de bas niveau, directe entre deux programmes.

**Analogie :**
Comme un appel téléphonique : connexion directe et continue entre deux correspondants.

**Caractéristiques :**
- Connexion persistante
- Contrôle total sur les données
- Performance maximale

**Cas d'usage :**
- Jeux multijoueurs
- Systèmes de chat
- Applications temps réel
- Protocoles personnalisés

#### 3. **Services SOAP (Simple Object Access Protocol)**

Standard d'entreprise pour services web avec contrat strict.

**Analogie :**
Comme un formulaire officiel : structure rigide mais garanties formelles.

**Caractéristiques :**
- Format XML
- Contrat formel (WSDL)
- Standards de sécurité avancés
- Support des transactions

**Cas d'usage :**
- Applications d'entreprise
- Systèmes bancaires
- Interopérabilité garantie
- Services gouvernementaux

#### 4. **Architecture Client-Serveur**

Organisation structurée avec séparation des rôles.

**Schéma :**
```
┌──────────┐         ┌──────────────┐         ┌──────────────┐
│ Clients  │────────►│   Serveur    │────────►│   Base de    │
│(Interface│         │  (Logique    │         │   données    │
│  + UI)   │◄────────│   métier)    │◄────────│              │
└──────────┘         └──────────────┘         └──────────────┘
```

**Avantages :**
- Centralisation des données
- Maintenance facilitée
- Sécurité renforcée
- Scalabilité

#### 5. **Applications distribuées**

Systèmes répartis sur plusieurs serveurs.

**Exemple :**
```
┌─────────┐     ┌─────────┐     ┌─────────┐
│Serveur 1│◄───►│Serveur 2│◄───►│Serveur 3│
│(Europe) │     │ (Asie)  │     │(Amérique│
└─────────┘     └─────────┘     └─────────┘
```

**Avantages :**
- Haute disponibilité
- Résilience aux pannes
- Performance optimale (proximité géographique)
- Scalabilité horizontale

#### 6. **OAuth2 et authentification moderne**

Sécuriser l'accès à vos services et données.

**Exemple :**
```
Utilisateur → "Je veux me connecter avec Google"  
Application → Redirige vers Google  
Google → Authentifie l'utilisateur  
Google → Renvoie un token à l'application  
Application → Utilise le token pour accéder aux données  
```

**Pourquoi c'est important :**
- Sécurité des données
- Pas de partage de mot de passe
- Contrôle granulaire des permissions
- Standard de l'industrie

#### 7. **GraphQL**

Alternative moderne à REST, plus flexible.

**Différence avec REST :**
```
REST:  
GET /user/123 → Tout le profil utilisateur  
GET /user/123/posts → Tous les posts  
GET /user/123/friends → Tous les amis  
= 3 requêtes, beaucoup de données inutiles

GraphQL:  
query {  
  user(id: 123) {
    name
    posts { title }
  }
}
= 1 requête, seulement ce dont vous avez besoin
```

**Avantages :**
- Moins de requêtes
- Pas de sur-chargement de données
- Auto-documenté
- Flexible

#### 8. **Services Cloud (AWS, Azure, Google Cloud)**

Intégrer des services puissants sans infrastructure propre.

**Services typiques :**
- **Stockage** : Sauvegarder fichiers et images
- **Bases de données** : Données gérées et scalables
- **IA/ML** : Reconnaissance d'images, traduction, etc.
- **Compute** : Exécuter du code sans serveur

**Exemple :**
```pascal
// Utiliser l'IA de Google pour analyser une image
Result := GoogleVision.AnalyzeImage('photo.jpg');
// → "Chat, Animal domestique, Mignon"
```

#### 9. **WebSockets**

Communication bidirectionnelle en temps réel.

**Différence avec HTTP :**
```
HTTP (Polling):  
Client → "Nouveaux messages ?"  
Serveur → "Non"  
[pause]
Client → "Nouveaux messages ?"  
Serveur → "Non"  
[pause]
Client → "Nouveaux messages ?"  
Serveur → "Oui : 'Bonjour'"  

WebSocket:  
Client ↔ Serveur [connexion permanente]  
Serveur → "Nouveau message : 'Bonjour'" (instantané)  
```

**Cas d'usage :**
- Chat en direct
- Notifications push
- Tableaux de bord temps réel
- Jeux en ligne
- Éditeurs collaboratifs

### Les protocoles et technologies que vous allez maîtriser

**Protocoles de communication :**
- **HTTP/HTTPS** : Le web standard
- **TCP/IP** : Communication de bas niveau
- **WebSocket** : Temps réel bidirectionnel
- **SSL/TLS** : Sécurisation des communications

**Formats de données :**
- **JSON** : Léger et populaire
- **XML** : Standard d'entreprise
- **Binaire** : Performance maximale

**Architectures :**
- **Client-Serveur** : Architecture classique
- **Peer-to-Peer** : Communication directe
- **Microservices** : Services indépendants
- **Serverless** : Sans gestion de serveur

**Sécurité :**
- **OAuth2** : Authentification déléguée
- **JWT** : Tokens sécurisés
- **API Keys** : Clés d'accès
- **HTTPS** : Chiffrement des communications

### Ce que vous allez apprendre dans ce chapitre

À la fin de ce chapitre, vous serez capable de :

✅ **Consommer des APIs REST**
- Appeler n'importe quelle API web
- Traiter les données JSON et XML
- Gérer l'authentification
- Gérer les erreurs réseau

✅ **Créer des communications directes**
- Établir des connexions TCP/IP
- Créer des serveurs et clients
- Échanger des données en temps réel

✅ **Utiliser des services d'entreprise**
- Consommer des services SOAP
- Gérer des contrats WSDL
- Implémenter des standards de sécurité

✅ **Architecturer vos applications**
- Concevoir une architecture client-serveur
- Créer des systèmes distribués
- Gérer la scalabilité

✅ **Sécuriser vos communications**
- Implémenter OAuth2
- Gérer des tokens JWT
- Protéger vos données

✅ **Utiliser des technologies modernes**
- Interroger des APIs GraphQL
- Intégrer des services cloud
- Implémenter des WebSockets

### Prérequis pour ce chapitre

Avant de commencer, vous devriez être à l'aise avec :

- **Les bases de Delphi** : Variables, fonctions, classes
- **La programmation orientée objet** : Classes, héritage
- **Les exceptions** : try-except pour gérer les erreurs
- **Les chaînes et JSON** : Manipulation de texte et données

**Concepts utiles (mais pas obligatoires) :**
- HTTP : Comment fonctionne le web
- JSON : Format de données
- Bases de données : Si vous voulez faire du client-serveur

### Structure du chapitre

Ce chapitre est organisé de manière progressive :

**Niveau débutant :**
1. APIs REST - Le plus accessible
2. JSON/XML - Manipulation de données
3. OAuth2 - Sécurité moderne

**Niveau intermédiaire :**
4. Sockets TCP/IP - Communication directe
5. SOAP - Services d'entreprise
6. Architecture client-serveur

**Niveau avancé :**
7. Applications distribuées
8. GraphQL - APIs modernes
9. Services cloud - AWS, Azure, Google
10. WebSockets - Temps réel

Vous pouvez suivre l'ordre proposé ou sauter directement aux sections qui vous intéressent, mais nous recommandons de commencer par les APIs REST pour acquérir les bases.

### Outils et composants Delphi

Delphi fournit de nombreux composants pour faciliter la communication réseau :

**Composants natifs :**
- `TRESTClient`, `TRESTRequest`, `TRESTResponse` : APIs REST
- `TNetHTTPClient` : Client HTTP moderne
- `TIdTCPClient`, `TIdTCPServer` : Sockets (Indy)
- `TJSONObject`, `TJSONArray` : Manipulation JSON
- `TXMLDocument` : Manipulation XML

**Bibliothèques tierces utiles :**
- **Indy** : Suite complète de composants réseau (incluse)
- **Synapse** : Alternative légère
- **mORMot** : Framework complet client-serveur

### Conseils pour bien débuter

**1. Commencez simple**
Ne cherchez pas à tout comprendre d'un coup. Commencez par une simple requête REST, puis progressez.

**2. Testez avec des APIs publiques**
De nombreuses APIs gratuites existent pour s'entraîner :
- JSONPlaceholder : API de test
- OpenWeatherMap : Météo
- REST Countries : Informations sur les pays

**3. Utilisez des outils de test**
- **Postman** : Tester des APIs REST
- **SoapUI** : Tester des services SOAP
- **WebSocket.org** : Tester des WebSockets

**4. Gérez toujours les erreurs**
Le réseau peut échouer. Utilisez toujours try-except et gérez les timeouts.

```pascal
try
  Response := HTTPClient.Get('https://api.example.com/data');
  // Traiter la réponse
except
  on E: Exception do
    ShowMessage('Erreur réseau: ' + E.Message);
end;
```

**5. Respectez la sécurité**
- Utilisez toujours HTTPS en production
- Ne stockez jamais de mots de passe en clair
- Validez toutes les données reçues

### Ressources supplémentaires

**Documentation officielle :**
- Documentation Embarcadero Delphi
- API Documentation des services (Google, AWS, etc.)

**Outils en ligne :**
- **JSON Formatter** : Visualiser et valider du JSON
- **Reqbin** : Tester des APIs en ligne
- **Webhook.site** : Tester des webhooks

**Standards et spécifications :**
- RFC 2616 : HTTP/1.1
- RFC 6455 : WebSocket Protocol
- OAuth 2.0 : RFC 6749
- JSON : RFC 8259

### Philosophie de ce chapitre

Ce chapitre adopte une approche **pratique et progressive** :

1. **Des explications simples** : Analogies et schémas pour comprendre
2. **Du code fonctionnel** : Exemples complets et testés
3. **Des cas réels** : Applications concrètes
4. **Pas à pas** : Du simple au complexe
5. **Bonnes pratiques** : Code professionnel dès le début

**Notre objectif :** Vous rendre autonome dans la création d'applications Delphi connectées, capables de communiquer avec n'importe quel service, local ou distant, simple ou complexe.

### Commençons !

Vous êtes maintenant prêt à explorer le monde fascinant de la communication réseau avec Delphi. Chaque section vous apportera de nouvelles compétences qui s'ajouteront aux précédentes pour faire de vous un développeur complet.

Les applications modernes ne sont plus isolées : elles communiquent, partagent, collaborent. Avec ce chapitre, vos applications Delphi vont franchir un nouveau cap et rejoindre l'écosystème connecté d'aujourd'hui.

**Allons-y ! 🚀**

---

*Passons maintenant à la première section : **10.1 Appels REST et API Web (TRESTClient)**, où vous apprendrez à consommer des APIs REST, le type de service web le plus populaire aujourd'hui.*

⏭️ [Appels REST et API Web (TRESTClient)](/10-communication-et-services-reseaux/01-appels-rest-et-api-web.md)
