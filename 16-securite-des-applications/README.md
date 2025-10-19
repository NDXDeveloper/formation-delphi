🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 16 - Sécurité des applications

## Introduction

La sécurité des applications est l'un des aspects les plus critiques du développement logiciel moderne. Dans un monde de plus en plus connecté, où les données personnelles et professionnelles ont une valeur considérable, protéger vos applications contre les menaces est devenu une nécessité absolue, et non plus une option.

**Pourquoi la sécurité est-elle si importante ?**

Chaque jour, des milliers d'applications sont ciblées par des attaques informatiques. Les conséquences d'une faille de sécurité peuvent être désastreuses :
- Vol de données personnelles ou financières
- Perte de confiance des utilisateurs
- Sanctions légales et amendes (RGPD, etc.)
- Dommages à la réputation de l'entreprise
- Pertes financières importantes

En tant que développeur, vous avez la responsabilité de protéger les utilisateurs et les données qui vous sont confiées. Ce chapitre vous donnera les connaissances fondamentales pour construire des applications Delphi sécurisées.

## Qu'est-ce que la sécurité applicative ?

La sécurité applicative englobe l'ensemble des mesures, techniques et pratiques mises en œuvre pour protéger une application contre :
- Les accès non autorisés
- Le vol ou la fuite de données
- Les modifications malveillantes
- Les interruptions de service
- L'exploitation de vulnérabilités

**Analogie du monde réel** : Pensez à votre application comme à une maison. La sécurité applicative, c'est :
- Les serrures sur les portes (authentification)
- Les clés qui déterminent qui peut entrer dans quelles pièces (autorisation)
- Les coffres-forts pour les objets de valeur (chiffrement)
- Le système d'alarme (détection d'intrusions)
- Les murs solides (protection de l'infrastructure)

## Les piliers de la sécurité

La sécurité des applications repose sur plusieurs piliers fondamentaux, souvent résumés par l'acronyme **CIA** (en anglais) ou **DIC** (en français) :

### 1. Confidentialité (Confidentiality)

**Objectif** : S'assurer que seules les personnes autorisées peuvent accéder aux informations sensibles.

**Exemples** :
- Un utilisateur ne peut pas consulter les données personnelles d'un autre utilisateur
- Les mots de passe sont chiffrés et jamais stockés en clair
- Les communications entre le client et le serveur sont sécurisées

**Techniques utilisées** :
- Chiffrement des données
- Contrôle d'accès
- Authentification forte

### 2. Intégrité (Integrity)

**Objectif** : Garantir que les données n'ont pas été modifiées de manière non autorisée.

**Exemples** :
- Un attaquant ne peut pas modifier le montant d'une transaction
- Les données transmises sur le réseau arrivent intactes
- Les fichiers de configuration n'ont pas été altérés

**Techniques utilisées** :
- Sommes de contrôle (checksums)
- Signatures numériques
- Validation des entrées
- Transactions atomiques

### 3. Disponibilité (Availability)

**Objectif** : S'assurer que l'application et les données sont accessibles quand les utilisateurs en ont besoin.

**Exemples** :
- L'application résiste aux attaques par déni de service (DoS)
- Les données sont sauvegardées et peuvent être restaurées
- Le système peut gérer une charge importante

**Techniques utilisées** :
- Redondance
- Sauvegardes régulières
- Protection contre les attaques DoS
- Surveillance et alertes

## Le principe de la défense en profondeur

**Concept clé** : Ne jamais se reposer sur une seule mesure de sécurité.

La sécurité doit être pensée en **couches successives**. Si une couche est compromise, les autres continuent de protéger l'application.

```
┌─────────────────────────────────────┐
│  Sécurité périmétrique (Firewall)   │
├─────────────────────────────────────┤
│  Sécurité réseau (HTTPS, VPN)       │
├─────────────────────────────────────┤
│  Authentification utilisateur       │
├─────────────────────────────────────┤
│  Autorisation et contrôle d'accès   │
├─────────────────────────────────────┤
│  Validation des entrées             │
├─────────────────────────────────────┤
│  Chiffrement des données            │
├─────────────────────────────────────┤
│  Journalisation et surveillance     │
├─────────────────────────────────────┤
│  Sauvegardes et récupération        │
└─────────────────────────────────────┘
```

**Exemple concret** : Protection d'une application bancaire
1. **Périmètre** : Pare-feu qui filtre le trafic
2. **Réseau** : Connexion HTTPS obligatoire
3. **Authentification** : Identifiant + mot de passe + code SMS
4. **Autorisation** : L'utilisateur ne voit que ses comptes
5. **Validation** : Les montants de transaction sont validés
6. **Chiffrement** : Les données sensibles sont chiffrées en base
7. **Journalisation** : Toutes les opérations sont tracées
8. **Sauvegarde** : Sauvegardes quotidiennes des données

## Les types de menaces courantes

Pour protéger votre application, vous devez comprendre les menaces auxquelles elle est exposée.

### 1. Attaques sur les données

**Injection SQL**
L'attaquant insère du code SQL malveillant pour accéder ou modifier la base de données.

```sql
-- Requête normale
SELECT * FROM Users WHERE username = 'john' AND password = 'secret123'

-- Requête avec injection
SELECT * FROM Users WHERE username = 'admin' OR '1'='1' -- ' AND password = 'anything'
-- Résultat : tous les utilisateurs sont retournés, y compris l'admin !
```

**Vol de données (Data Breach)**
Accès non autorisé à des données sensibles suite à une faille de sécurité.

**Fuite de données (Data Leakage)**
Exposition accidentelle de données sensibles (logs, messages d'erreur, etc.).

### 2. Attaques sur l'identité

**Usurpation d'identité**
Un attaquant se fait passer pour un utilisateur légitime.

**Force brute**
Tentatives répétées de connexion avec différents mots de passe.

**Hameçonnage (Phishing)**
Tromper l'utilisateur pour qu'il révèle ses identifiants.

### 3. Attaques sur l'application

**Cross-Site Scripting (XSS)**
Injection de code JavaScript malveillant dans une page web.

**Cross-Site Request Forgery (CSRF)**
Forcer un utilisateur authentifié à exécuter des actions non désirées.

**Déni de service (DoS/DDoS)**
Surcharger l'application pour la rendre indisponible.

### 4. Attaques sur le code

**Exécution de code à distance (RCE)**
Permettre à un attaquant d'exécuter du code arbitraire sur le serveur.

**Débordement de tampon (Buffer Overflow)**
Écrire au-delà de la mémoire allouée pour corrompre le programme.

**Injection de commandes**
Exécuter des commandes système non autorisées.

## Le cycle de vie de la sécurité

La sécurité n'est pas quelque chose qu'on ajoute à la fin du développement. Elle doit être intégrée à chaque étape :

### 1. Phase de conception

**Questions à se poser** :
- Quelles données sont sensibles ?
- Qui doit avoir accès à quoi ?
- Quels sont les scénarios d'attaque possibles ?
- Quelles réglementations s'appliquent (RGPD, etc.) ?

**Actions** :
- Modélisation des menaces
- Définition de la politique de sécurité
- Choix des technologies sécurisées

### 2. Phase de développement

**Principes à appliquer** :
- Validation stricte de toutes les entrées
- Principe du moindre privilège
- Séparation des responsabilités
- Gestion sécurisée des erreurs
- Code reviews axées sécurité

**Outils** :
- Bibliothèques de chiffrement
- Frameworks d'authentification
- Analyseurs de code statique

### 3. Phase de test

**Types de tests** :
- Tests de pénétration (pentesting)
- Analyse de vulnérabilités
- Fuzzing (injection de données aléatoires)
- Tests d'intrusion

**Objectif** : Trouver les failles avant qu'un attaquant ne le fasse.

### 4. Phase de déploiement

**Mesures de sécurité** :
- Configuration sécurisée des serveurs
- Certificats SSL/TLS
- Pare-feu et filtrage
- Surveillance et alertes

### 5. Phase de maintenance

**Activités continues** :
- Mise à jour des composants de sécurité
- Surveillance des logs
- Réponse aux incidents
- Audit de sécurité régulier

## La sécurité dans le contexte de Delphi

Delphi offre de nombreux outils et bibliothèques pour développer des applications sécurisées :

### Avantages de Delphi pour la sécurité

✅ **Compilation native** : Le code compilé est plus difficile à analyser que du code interprété

✅ **Typage fort** : Réduit les erreurs de programmation qui peuvent créer des vulnérabilités

✅ **FireDAC avec requêtes paramétrées** : Protection native contre les injections SQL

✅ **Bibliothèques de chiffrement intégrées** : Support de AES, RSA, SHA, etc.

✅ **Gestion mémoire** : Moins de risques de débordement qu'en C/C++

✅ **Multi-plateforme sécurisé** : Même code base pour Windows, macOS, iOS, Android, Linux

### Composants et unités de sécurité en Delphi

**Unités cryptographiques** :
- `System.Hash` : Fonctions de hachage (MD5, SHA1, SHA2)
- `System.NetEncoding` : Encodage Base64, URL, HTML
- Indy (Internet Direct) : SSL/TLS, protocoles sécurisés

**Composants de base de données** :
- FireDAC : Requêtes paramétrées, chiffrement de connexion
- Champs chiffrés dans les datasets

**Composants réseau** :
- `TRESTClient` : Support HTTPS, authentification
- Indy SSL : Composants SSL/TLS
- WebBroker : Gestion sécurisée des sessions web

## Les réglementations à connaître

En fonction de votre domaine et de votre géographie, vous devez respecter certaines réglementations :

### RGPD (Règlement Général sur la Protection des Données)

**Applicable** : Union Européenne et données de citoyens européens

**Principes clés** :
- Consentement explicite pour la collecte de données
- Droit à l'oubli (suppression des données)
- Notification des violations de données sous 72h
- Minimisation des données collectées
- Chiffrement des données sensibles

**Impact sur votre application Delphi** :
- Implémenter la suppression complète des données utilisateur
- Créer des exports de données personnelles
- Journaliser les consentements
- Chiffrer les données à caractère personnel

### Autres réglementations importantes

**PCI DSS** : Pour les applications manipulant des cartes de crédit

**HIPAA** : Pour les applications médicales (États-Unis)

**SOX** : Pour les applications financières d'entreprises cotées

**Loi CNIL** : Pour les applications traitant des données en France

## Les erreurs courantes à éviter

### ❌ Erreur n°1 : "Ça n'arrivera pas à mon application"

**Réalité** : Toutes les applications sont des cibles potentielles, même les petites.

### ❌ Erreur n°2 : Stocker les mots de passe en clair

**Conséquence** : Si la base est compromise, tous les comptes sont instantanément accessibles.

### ❌ Erreur n°3 : Faire confiance aux données utilisateur

**Règle d'or** : Ne JAMAIS faire confiance à une donnée venant de l'utilisateur. Toujours valider et filtrer.

### ❌ Erreur n°4 : La sécurité par l'obscurité

**Mythe** : "Si personne ne connaît mon système, il est sûr"

**Réalité** : La vraie sécurité fonctionne même si l'attaquant connaît le système.

### ❌ Erreur n°5 : Négliger les mises à jour

**Danger** : Les vulnérabilités découvertes doivent être corrigées rapidement.

### ❌ Erreur n°6 : Messages d'erreur trop détaillés

**Problème** : Révéler des informations système dans les messages d'erreur.

```pascal
// ❌ MAUVAIS
ShowMessage('Erreur SQL : Table users introuvable sur serveur mysql-prod-01');

// ✅ BON
ShowMessage('Une erreur est survenue. Veuillez contacter le support.');
// Logs détaillés côté serveur uniquement
```

### ❌ Erreur n°7 : Développer son propre algorithme de chiffrement

**Règle** : Utilisez toujours des algorithmes éprouvés (AES, RSA, etc.). La cryptographie est un domaine complexe.

## Mentalité sécurité (Security Mindset)

Pour développer des applications sécurisées, vous devez adopter une nouvelle façon de penser :

### Pensez comme un attaquant

Posez-vous constamment ces questions :
- "Comment pourrais-je contourner cette protection ?"
- "Que se passe-t-il si j'envoie une valeur inattendue ?"
- "Puis-je accéder aux données d'un autre utilisateur ?"
- "Que révèlent les messages d'erreur ?"

### Le principe du moindre privilège

**Définition** : Donner uniquement les droits strictement nécessaires.

**Application** :
- Un utilisateur normal n'a pas besoin de droits administrateur
- Une base de données d'application n'a pas besoin d'accès DROP TABLE
- Un processus n'a besoin que des fichiers qu'il manipule

### Présumer la compromission

**Principe** : Supposez qu'une partie du système peut être compromise.

**Exemple** :
- Si le client est compromis, le serveur doit quand même être sûr
- Si une session est volée, les dégâts doivent être limités
- Si une base est copiée, les données sensibles doivent être chiffrées

### Échec sécurisé (Fail-Safe)

**Principe** : En cas d'erreur, le système doit rester sûr.

```pascal
// ❌ MAUVAIS - En cas d'erreur, l'accès est autorisé
function VerifierAcces: Boolean;
begin
  Result := True; // Par défaut
  try
    // Vérification...
  except
    // En cas d'erreur, Result reste True !
  end;
end;

// ✅ BON - En cas d'erreur, l'accès est refusé
function VerifierAcces: Boolean;
begin
  Result := False; // Par défaut sécurisé
  try
    // Vérification...
    if ConditionsRemplies then
      Result := True;
  except
    Result := False; // Explicitement refusé en cas d'erreur
  end;
end;
```

## Structure de ce chapitre

Ce chapitre est organisé en sections progressives qui couvrent tous les aspects de la sécurité :

**16.1 Authentification des utilisateurs**
- Comment vérifier l'identité des utilisateurs
- Gestion des mots de passe
- Authentification multifacteur
- Session et tokens

**16.2 Autorisation et contrôle d'accès**
- Qui peut faire quoi dans l'application
- Rôles et permissions
- Contrôle d'accès basé sur les rôles (RBAC)

**16.3 Chiffrement des données**
- Protection des données sensibles
- Chiffrement symétrique et asymétrique
- Certificats et PKI

**16.4 Sécurisation des connexions**
- HTTPS et SSL/TLS
- Sécurisation des API
- Protection des communications

**16.5 Protection contre les vulnérabilités courantes**
- Injection SQL
- XSS, CSRF
- Validation des entrées

**16.6 Audit de sécurité**
- Journalisation
- Détection d'intrusions
- Analyse des logs

**16.7 Stockage sécurisé des identifiants**
- Coffres-forts d'identifiants
- Gestion des secrets
- Variables d'environnement

**16.8 GDPR et confidentialité des données**
- Conformité réglementaire
- Gestion du consentement
- Droit à l'oubli

**16.9 Signature numérique et validation**
- Intégrité des fichiers
- Code signing
- Certificats numériques

**16.10 Sécurité des applications mobiles**
- Spécificités iOS/Android
- Stockage sécurisé mobile
- Authentification biométrique

## Ressources et outils

### Documentation et références

📚 **Documentation Delphi** :
- [DocWiki Embarcadero - Security](https://docwiki.embarcadero.com/)
- Exemples de code sécurisé

🌐 **Ressources en ligne** :
- OWASP (Open Web Application Security Project)
- NIST Cybersecurity Framework
- CWE (Common Weakness Enumeration)

### Outils utiles

🛠️ **Analyse de code** :
- Analyseurs statiques pour Delphi
- Outils de revue de code

🔐 **Test de sécurité** :
- Burp Suite (test d'applications web)
- OWASP ZAP
- Nmap (scan réseau)

📊 **Surveillance** :
- Outils de monitoring
- Analyseurs de logs
- Systèmes d'alerte

## Checklist de sécurité de base

Avant de déployer votre application, assurez-vous que :

- [ ] Les mots de passe sont hashés avec un salt
- [ ] Toutes les requêtes SQL sont paramétrées
- [ ] Les entrées utilisateur sont validées
- [ ] Les communications sensibles utilisent HTTPS
- [ ] Les données sensibles sont chiffrées
- [ ] Les sessions expirent après inactivité
- [ ] Les erreurs ne révèlent pas d'informations système
- [ ] Les tentatives de connexion sont limitées
- [ ] Les actions sensibles sont journalisées
- [ ] Les dépendances sont à jour
- [ ] Un plan de réponse aux incidents existe
- [ ] Les sauvegardes sont régulières et testées

## Conclusion de l'introduction

La sécurité peut sembler complexe et intimidante, mais elle devient plus accessible quand on la décompose en concepts fondamentaux et qu'on applique des bonnes pratiques éprouvées.

**Points clés à retenir** :

✅ La sécurité est l'affaire de tous, pas seulement des experts

✅ Elle doit être intégrée dès la conception, pas ajoutée à la fin

✅ La défense en profondeur est la meilleure stratégie

✅ Ne faites jamais confiance aux données utilisateur

✅ Utilisez des bibliothèques et algorithmes éprouvés

✅ Restez informé des nouvelles vulnérabilités et menaces

Dans les sections suivantes, nous allons explorer en détail chaque aspect de la sécurité avec des exemples concrets en Delphi. Vous apprendrez non seulement les concepts théoriques, mais aussi comment les implémenter concrètement dans vos applications.

**Rappelez-vous** : Une application sécurisée n'est jamais "terminée". La sécurité est un processus continu d'amélioration, de surveillance et d'adaptation aux nouvelles menaces.

Commençons maintenant notre exploration approfondie de la sécurité des applications Delphi !

⏭️ [Authentification des utilisateurs](/16-securite-des-applications/01-authentification-des-utilisateurs.md)
