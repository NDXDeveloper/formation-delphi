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
- Les mots de passe sont **hashés** (pas chiffrés) et jamais stockés en clair — voir le chapitre 16.1 pour la distinction
- Les communications entre le client et le serveur sont sécurisées (TLS)

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
3. **Authentification** : Identifiant + mot de passe + 2ᵉ facteur (TOTP, Passkey ou push notification — **pas SMS** : le NIST SP 800-63B le déconseille depuis 2017 à cause des attaques par SIM-swap)
4. **Autorisation** : L'utilisateur ne voit que ses comptes
5. **Validation** : Les montants de transaction sont validés
6. **Chiffrement** : Les données sensibles sont chiffrées en base
7. **Journalisation** : Toutes les opérations sont tracées
8. **Sauvegarde** : Sauvegardes quotidiennes des données, chiffrées et testées (un backup non testé n'est PAS un backup)

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
Exécuter des commandes système non autorisées. Exemple : un champ « nom de fichier » concaténé dans `ShellExecute('cmd.exe /c convertir ' + ANomFichier)` permet à l'utilisateur d'entrer `image.png & del /Q *.*` pour exécuter une commande supplémentaire.

### 5. Menaces opérationnelles (2024-2026)

**Ransomware**
Selon l'ANSSI, c'est **la menace numéro 1** depuis 2020 pour les organisations françaises. L'attaquant chiffre vos données et exige une rançon. Sauvegardes immuables + segmentation réseau + PRA testé sont les seules protections fiables.

**Insider threats (menaces internes)**
Un employé légitime (actuel ou ancien) qui exfiltre, sabote ou utilise abusivement ses accès. Souvent sous-estimées, elles représentent ~20 % des incidents selon le rapport Verizon DBIR. Contre-mesures : principe du moindre privilège, séparation des tâches, audit des accès, *zero standing privileges* (accès *just-in-time* uniquement).

**Side-channel attacks (canaux auxiliaires)**
L'attaquant déduit un secret en mesurant le temps de calcul, la consommation électrique, les radiations électromagnétiques, le cache CPU (Spectre/Meltdown 2018), etc. Contre-mesures : code à temps constant (cf comparaisons de hash), randomisation des opérations sensibles.

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
- **SAST** (*Static Application Security Testing*) — analyse du code source sans l'exécuter. Outils : Pascal Analyzer, Semgrep, SonarQube.
- **DAST** (*Dynamic Application Security Testing*) — test de l'application en cours d'exécution, en envoyant des requêtes malicieuses. Outils : OWASP ZAP, Burp Suite.
- **IAST** (*Interactive AST*) — combinaison : instrumente l'app pendant les tests DAST pour avoir une vue interne.
- **SCA** (*Software Composition Analysis*) — analyse des dépendances pour détecter les CVE connues. Outils : Dependabot, Snyk, OWASP Dependency-Check.
- **Tests de pénétration (pentesting)** — par des humains, simulant un vrai attaquant.
- **Fuzzing** — injection de données aléatoires pour faire crasher l'application et trouver des vulnérabilités.
- **Red Team** — exercice grandeur nature où une équipe (interne ou externe) tente de pénétrer le SI sans préavis.

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

✅ **Compilation native** : pas de bytecode intermédiaire trivialement décompilable (à la différence de Java/.NET). ⚠️ N'en faites PAS un argument de sécurité : les outils de rétro-ingénierie modernes (IDA Pro, Ghidra qui est gratuit depuis 2019, Hopper, Binary Ninja) désassemblent un binaire Delphi sans difficulté. Les chaînes constantes restent visibles avec `strings`, et la RTTI de Delphi fournit même aux attaquants la liste des classes, méthodes et propriétés publiées. La compilation native **ralentit** l'attaquant, elle ne le **bloque pas**.

✅ **Typage fort** : Réduit les erreurs de programmation qui peuvent créer des vulnérabilités

✅ **FireDAC avec requêtes paramétrées** : Protection native contre les injections SQL

✅ **Bibliothèques de chiffrement intégrées** : Support de SHA-2 et PBKDF2 dans `System.Hash` ; AES, RSA et TLS via OpenSSL (par Indy ou par des wrappers tiers comme LockBox 3, GrijjyFoundation, TMS Cryptography Pack).

✅ **Gestion mémoire** : les types managed (`string`, `TBytes`, tableaux dynamiques) éliminent la plupart des débordements typiques de C/C++. ⚠️ La sécurité n'est pas automatique : tout usage de `PChar`, `Move`, `BlockRead`, ou de pointeurs bruts peut réintroduire des dépassements. Le typage fort de Delphi est une aide, pas une garantie.

✅ **Multi-plateforme sécurisé** : Même code base pour Windows, macOS, iOS, Android, Linux

### Composants et unités de sécurité en Delphi

**Unités cryptographiques** :
- `System.Hash` : fonctions de hachage. ⚠️ N'utilisez que `THashSHA2` (SHA-256/SHA-512) et `THashPBKDF2_SHA256` pour la sécurité. `THashMD5` et `THashSHA1` sont fournies à des fins de compatibilité historique : elles ne sont plus considérées comme cryptographiquement sûres (collisions pratiques démontrées).
- `System.NetEncoding` : Encodage Base64, URL, HTML (encodages, pas du chiffrement !)
- Indy (Internet Direct) : SSL/TLS via OpenSSL, protocoles sécurisés
- `System.Security.Cryptography` (sur .NET, non disponible en Delphi VCL/FMX) : à ne pas confondre avec son équivalent .NET

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

**Applicable** : entreprises établies dans l'UE/EEE, OU traitement de données de personnes **situées** dans l'UE (peu importe leur nationalité — voir 16.8 pour les détails sur le champ d'application).

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

**PCI DSS v4.0** : Pour les applications manipulant des cartes de crédit (la v4.0.1 publiée en 2024 est obligatoire depuis le 31 mars 2025).

**HIPAA** : Pour les applications médicales (États-Unis).

**SOX** : Pour les applications financières d'entreprises cotées en bourse.

**Loi Informatique et Libertés** : Loi française de 1978, modifiée par la loi du 20 juin 2018 pour s'aligner sur le RGPD. C'est l'instrument national qui complète le RGPD en France ; la CNIL en est l'autorité de contrôle.

**NIS 2** (directive UE 2022/2555) : entrée en vigueur en application nationale depuis octobre 2024. Élargit considérablement le périmètre de la directive NIS de 2016 : les opérateurs « essentiels » et « importants » (énergie, santé, transports, services numériques, etc.) doivent appliquer des mesures de gestion des risques cyber et notifier les incidents significatifs sous 24 h (pré-notification) puis 72 h.

**DORA** (règlement UE 2022/2554) : applicable depuis le 17 janvier 2025 aux entités financières (banques, assurances, gestionnaires d'actifs, etc.) et à leurs prestataires informatiques critiques. Impose un cadre de résilience opérationnelle informatique.

**AI Act** (règlement UE 2024/1689) : entré en vigueur progressivement depuis août 2024. Si votre application Delphi embarque un modèle d'IA classé « à haut risque », des obligations de documentation, de transparence et de robustesse s'appliquent.

**Cyber Resilience Act** (règlement UE 2024/2847) : applicable progressivement à partir de 2026/2027. Impose des exigences de cybersécurité « by design » à tous les produits avec composants numériques mis sur le marché européen — y compris les applications desktop natives.

## Les erreurs courantes à éviter

### ❌ Erreur n°1 : "Ça n'arrivera pas à mon application"

**Réalité** : Toutes les applications sont des cibles potentielles, même les petites.

### ❌ Erreur n°2 : Stocker les mots de passe en clair

**Conséquence** : Si la base est compromise, tous les comptes sont instantanément accessibles.

### ❌ Erreur n°3 : Faire confiance aux données utilisateur

**Règle d'or** : Ne JAMAIS faire confiance à une donnée venant de l'utilisateur. Toujours valider et filtrer.

### ❌ Erreur n°4 : La sécurité par l'obscurité

**Mythe** : "Si personne ne connaît mon système, il est sûr"

**Réalité** : La vraie sécurité fonctionne même si l'attaquant connaît le système. C'est le **principe de Kerckhoffs** (Auguste Kerckhoffs, 1883) : *un cryptosystème doit rester sûr même si tout sauf la clé est public*. Toute la cryptographie moderne repose sur ce principe — c'est pour cela qu'on connaît les détails d'AES, de RSA ou de TLS, et qu'ils restent sûrs malgré tout.

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

### ❌ Erreur n°8 : Pousser un secret dans un dépôt Git public

**Réalité 2024-2025** : selon le rapport annuel de GitGuardian, **23 millions de secrets** ont été détectés en 2023 sur les commits publics GitHub. Une clé API exposée 30 secondes est suffisante pour être scrapée par les bots qui parcourent en continu les nouveaux commits.

**Protections** :
- Pre-commit hooks avec `gitleaks`, `trufflehog` ou GitGuardian CLI.
- Ne JAMAIS résoudre le problème en "supprimant le commit" — l'historique Git le garde, et même un `force-push` n'efface pas les forks/clones. Considérer le secret comme **compromis** et le rotater immédiatement.
- Si secret compromis : rotation + audit des logs d'accès pour détecter une utilisation malveillante.

### ❌ Erreur n°9 : Faire confiance à toutes ses dépendances

**Supply chain** : un attaquant ne cible plus directement votre app, mais une dépendance. Cas célèbres récents : `event-stream` (npm, 2018), `ua-parser-js` (npm, 2021), `xz-utils` (Linux, mars 2024 — backdoor SSH évitée de justesse), `SolarWinds Orion` (2020 — compromission du processus de build affectant ~18 000 organisations).

**Pour Delphi spécifiquement** :
- Surveiller les CVE de vos composants : **FireDAC** (drivers MySQL/PostgreSQL/SQL Server), **Indy** (qui embarque OpenSSL et dont les DLL `libcrypto`/`libssl` doivent être à jour), bibliothèques tierces (TMS, Konopka, FastReport, JEDI…).
- **GetIt Package Manager** (Embarcadero) : équivalent npm/NuGet pour Delphi. Ses packages ne sont pas signés individuellement — vérifier la source avant d'installer un package communautaire.
- *Software Bill of Materials* (SBOM, format SPDX ou CycloneDX) — déclaration de toutes les dépendances et versions. Exigé par le **Cyber Resilience Act** européen à partir de 2027.
- Verrouillage des versions dans le `.dproj` (références aux fichiers `.pas`/`.dpk` précis) et reproduction stricte des builds (même version Delphi, même patches, mêmes packages installés).
- **Reproduire les builds en CI** sur une image clean — un build artisanal sur le poste d'un développeur n'est pas auditable.

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

### Présumer la compromission (Zero Trust)

**Principe** : Supposez qu'une partie du système est déjà compromise. C'est le fondement de l'approche **Zero Trust** (« Never trust, always verify »), devenue depuis 2023-2024 le modèle de référence des grandes organisations et des autorités (NIST SP 800-207, ANSSI, CISA).

Quatre maximes :
1. **Aucune confiance implicite** : aucune connexion, machine ou utilisateur n'est de confiance par défaut, même à l'intérieur du périmètre.
2. **Vérification systématique** : chaque requête est authentifiée, autorisée et chiffrée, à chaque saut.
3. **Moindre privilège** : juste l'accès nécessaire, pour la durée nécessaire (just-in-time).
4. **Présumer la brèche** : concevoir comme si l'attaquant était déjà dans le système.

**Exemple appliqué à Delphi** :
- Si le client desktop est compromis, le serveur doit quand même être sûr (validation côté serveur, pas seulement côté client).
- Si une session est volée, les dégâts doivent être limités (jetons à courte durée de vie, refresh tokens rotatifs, MFA pour les actions sensibles).
- Si une base est copiée, les données sensibles doivent être chiffrées (chiffrement applicatif au-dessus du chiffrement disque).
- Si un binaire est extrait, il ne doit contenir aucun secret en clair (rotation par configuration, jamais en dur).

### Modéliser les menaces : STRIDE

Avant d'écrire du code, identifiez les menaces. La méthode **STRIDE** de Microsoft est un acronyme mnémonique des six familles d'attaques :

| Lettre | Menace | Propriété violée | Contre-mesure principale |
|---|---|---|---|
| **S** | **S**poofing (usurpation) | Authenticité | Authentification forte, MFA, certificats |
| **T** | **T**ampering (altération) | Intégrité | Hash, signatures, transactions |
| **R** | **R**epudiation (déni) | Non-répudiation | Journalisation signée, horodatage |
| **I** | **I**nformation disclosure (fuite) | Confidentialité | Chiffrement, contrôle d'accès |
| **D** | **D**enial of service | Disponibilité | Rate limiting, quotas, redondance |
| **E** | **E**levation of privilege | Autorisation | Moindre privilège, isolation, RBAC |

Parcourez chaque flux de données de votre application (entrée utilisateur, appel API, accès base, écriture fichier, IPC) et posez les 6 questions STRIDE. C'est l'outil que nous appliquerons implicitement tout au long des sections 16.1 à 16.10.

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
- **OWASP** (Open Web Application Security Project) — Top 10, ASVS, MASVS, Cheat Sheet Series
- **NIST Cybersecurity Framework 2.0** (publié février 2024) — version la plus récente, ajoute la fonction « Govern »
- **CWE** (Common Weakness Enumeration) — taxonomie des faiblesses logicielles, complète OWASP
- **CAPEC** (Common Attack Pattern Enumeration and Classification) — patterns d'attaque
- **MITRE ATT&CK** — base de connaissances tactiques et techniques d'attaque
- **ANSSI** (France) — guides de bonnes pratiques, particulièrement pour l'administration et les OIV

### Outils utiles

🛠️ **Analyse statique pour Delphi** :
- **Pascal Analyzer (Peganza)** — analyseur commercial spécifique Delphi/Object Pascal, détecte bug patterns et anti-patterns
- **FixInsight** — extension IDE qui pointe les *code smells* et erreurs courantes (anciennement TMS, désormais Open Source)
- **DCC32 hints/warnings** — n'ignorez pas les avertissements du compilateur (`-W` au maximum)
- **DelphiAST** — parseur Pascal pour écrire vos propres analyses
- **Semgrep** — règles personnalisables multi-langages, utilisable pour détecter des patterns dangereux dans du Pascal (concaténation SQL, secrets en dur, etc.). De plus en plus présent dans les pipelines CI 2024-2025.

🔐 **Test de sécurité** :
- **Burp Suite** — proxy HTTP pour intercepter et modifier les requêtes (test d'API REST)
- **OWASP ZAP** — alternative gratuite à Burp
- **Nmap** — scan réseau et détection de services
- **Wireshark** — capture de trafic pour vérifier qu'aucune donnée sensible ne fuite en clair
- **MobSF** — *Mobile Security Framework* pour APK/IPA

📊 **Surveillance et détection de secrets** :
- **gitleaks, trufflehog, GitGuardian CLI** — pré-commit hooks pour empêcher le push de secrets
- **Dependabot / Renovate** — notification automatique des CVE dans vos dépendances
- **SIEM** : ELK Stack, Splunk, Datadog, Wazuh (open source) pour centraliser et corréler les logs

## Checklist de sécurité de base

Avant de déployer votre application, assurez-vous que :

**Authentification & sessions**
- [ ] Les mots de passe sont hashés avec un sel CSPRNG (Argon2id, PBKDF2 ≥ 600 000 itérations, bcrypt ou scrypt)
- [ ] **MFA activé pour les comptes administrateur** (au minimum TOTP, idéalement Passkey/FIDO2)
- [ ] Les sessions expirent après inactivité (côté serveur, pas seulement client)
- [ ] Les tentatives de connexion sont limitées (par compte ET par IP)

**Entrées & sorties**
- [ ] Toutes les requêtes SQL sont paramétrées
- [ ] Les entrées utilisateur sont validées (liste blanche)
- [ ] Les sorties sont échappées selon le contexte (HTML, JS, SQL, shell)

**Transport & stockage**
- [ ] Les communications utilisent TLS 1.2 minimum (idéalement TLS 1.3)
- [ ] Les données sensibles sont chiffrées au repos (AES-256-GCM ou ChaCha20-Poly1305)
- [ ] Aucun secret en dur dans le code source ou les fichiers de configuration

**Observabilité & réaction**
- [ ] Les erreurs ne révèlent pas d'informations système à l'utilisateur
- [ ] Les actions sensibles sont journalisées (UTC, JSON Lines, intégrité protégée)
- [ ] Les dépendances sont à jour, surveillées (Dependabot/Renovate)
- [ ] **Un plan de réponse aux incidents (IRP)** existe et a été testé au moins une fois
- [ ] **Procédure de notification CNIL/utilisateurs** prête (72 h pour RGPD)
- [ ] Les sauvegardes sont régulières, chiffrées, et **leur restauration a été testée**

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
