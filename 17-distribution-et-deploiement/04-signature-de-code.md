🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.4 Signature de code

## Introduction

Imaginez que vous recevez un colis par la poste. Comment savez-vous qu'il vient vraiment de l'expéditeur indiqué et qu'il n'a pas été ouvert en chemin ? C'est exactement le même problème avec les applications logicielles : comment vos utilisateurs peuvent-ils être sûrs que votre application provient bien de vous et qu'elle n'a pas été modifiée par un tiers malveillant ?

La **signature de code** (ou *code signing*) est la solution à ce problème. C'est un processus cryptographique qui permet de prouver l'authenticité de votre application et de garantir qu'elle n'a pas été altérée depuis sa création.

## Qu'est-ce que la signature de code ?

### Définition simple

La signature de code est comme un sceau de cire numérique apposé sur votre application. Ce sceau :

1. **Prouve votre identité** : Confirme que vous êtes bien l'éditeur de l'application
2. **Garantit l'intégrité** : Assure que personne n'a modifié l'application depuis que vous l'avez signée
3. **Établit la confiance** : Aide Windows et les utilisateurs à faire confiance à votre logiciel

### Comment ça fonctionne ?

Le processus utilise la **cryptographie asymétrique** :

1. Vous obtenez un **certificat de signature de code** auprès d'une autorité de certification reconnue
2. Ce certificat contient votre **clé publique** et des informations vous identifiant
3. Vous signez votre application avec votre **clé privée** (que vous gardez secrète)
4. Quand un utilisateur télécharge votre application, Windows vérifie la signature avec votre clé publique
5. Si la signature est valide, Windows sait que l'application vient de vous et n'a pas été modifiée

**Analogie** : C'est comme signer un document avec votre signature personnelle, mais en version numérique inviolable.

## Pourquoi signer votre application ?

### 1. Éviter les avertissements Windows

Sans signature, Windows affiche des messages alarmants :

```
"Windows a protégé votre ordinateur"
"Éditeur inconnu"
"Cette application peut endommager votre ordinateur"
```

Ces messages effrayent les utilisateurs et beaucoup abandonnent l'installation.

**Avec une signature valide**, Windows affiche :
```
"Voulez-vous autoriser cette application de [Votre Nom] à apporter des modifications ?"
```

C'est beaucoup plus rassurant !

### 2. Contourner Windows SmartScreen

**Windows SmartScreen** filtre les applications non reconnues. Sans signature :
- Message "Windows SmartScreen a empêché le démarrage d'une application non reconnue"
- L'utilisateur doit cliquer sur "Informations complémentaires" puis "Exécuter quand même"
- Beaucoup d'utilisateurs abandonnent à cette étape

Avec une signature valide, SmartScreen est beaucoup plus tolérant.

### 3. Établir votre réputation

Au fil du temps, si votre application signée est téléchargée et utilisée sans problème, Windows construit une **réputation** pour votre certificat. Cela réduit progressivement les avertissements.

### 4. Exigence pour certaines plateformes

Certains environnements **exigent** la signature :
- **Microsoft Store** : signature obligatoire.
- **Entreprises** : beaucoup n'autorisent que les applications signées (politique AppLocker, Smart App Control).
- **macOS** : signature **avec un certificat Apple Developer ID** obligatoire (Apple Developer Program, 99 $/an). En plus, pour la distribution **hors App Store**, **notarisation** Apple obligatoire depuis macOS 10.15 (2019) — c'est un processus distinct de la signature qui soumet le binaire à Apple pour analyse antivirus automatique. Voir section 17.6.
- **Windows 11 22H2+ avec Smart App Control activé** : Smart App Control (mode très restrictif, activé par défaut sur certaines nouvelles installations) **refuse d'exécuter** tout binaire qui n'a pas une réputation établie OU une signature avec certificat de réputation forte. Un nouveau certificat met plusieurs mois à acquérir cette réputation.
- **Drivers Windows** : obligation EV depuis 2016 + soumission à Microsoft pour attestation (sinon le driver n'est pas chargé).

### 5. Protection contre les modifications

Si quelqu'un modifie votre application signée (pour y ajouter un virus, par exemple), la signature devient invalide. Windows alertera immédiatement l'utilisateur.

### 6. Professionnalisme

Une application signée démontre le sérieux de votre démarche. C'est un signe de qualité et de confiance.

## Types de certificats de signature de code

> 🚨 **Changement majeur depuis juin 2023** : selon les *CA/Browser Forum Baseline Requirements*, **TOUS** les certificats Code Signing (OV ET EV) doivent désormais être délivrés sur un **module cryptographique matériel** (HSM ou token USB type SafeNet eToken, YubiKey 5 FIPS). Les fichiers `.pfx` exportables ne sont **plus délivrés** par les autorités de certification reconnues. Si vous lisez encore des tutoriels qui parlent simplement de « télécharger le .pfx par email », ils datent d'avant ce changement.  
>  
> Conséquences pratiques :  
> - Le délai d'obtention inclut désormais l'envoi physique du token par courrier (1-2 semaines de plus).  
> - La signature en CI nécessite soit un HSM cloud (Azure Key Vault, AWS CloudHSM), soit un setup avec runner self-hosted sur la machine où est branché le token.  
> - Les prix ont augmenté en conséquence (le token est inclus dans le prix mais a un coût matériel).

Il existe deux types principaux de certificats :

### 1. Certificats de validation d'organisation (OV - Organization Validation)

**Caractéristiques 2026** :
- Vérifie l'existence légale de votre entreprise
- Affiche le nom de votre organisation dans Windows
- Délivré après vérification des documents officiels (1 à 4 semaines)
- **Livré sur token hardware** (depuis juin 2023, obligatoire)
- Validité : 1 à 3 ans
- Coût : **300-600 € par an** (le token est inclus, mais facturable en cas de perte)

**Adapté pour** : entreprises établies, applications commerciales.

### 2. Certificats de validation étendue (EV - Extended Validation)

**Caractéristiques 2026** :
- Vérification la plus stricte (entretien téléphonique, vérifications croisées)
- **Réputation initiale élevée** dans Windows SmartScreen (l'avantage historique « réputation immédiate » est aujourd'hui moins absolu — voir ci-dessous)
- Livré sur token hardware (déjà le cas avant 2023)
- Validité : 1 à 3 ans
- Coût : **500-1500 € par an** selon le fournisseur

**Avantage** : pas d'avertissement SmartScreen pour les utilisateurs dès les premiers téléchargements (les certificats OV doivent construire leur réputation).

**Adapté pour** : applications professionnelles largement distribuées, applications de sécurité, drivers Windows (obligation EV depuis 2016).

### 3. Et les certificats individuels (IV) ?

> ⚠️ **Quasi-disparus du marché grand public en 2026**. Depuis 2022-2023, la plupart des CA ne proposent plus de certificats à des personnes physiques sans structure juridique :  
> - **DigiCert, GlobalSign, Sectigo** : restreignent aux entreprises immatriculées.  
> - **Certum** (Pologne) : reste l'une des rares CA à proposer encore des certificats Code Signing pour personnes physiques (« Open Source Code Signing » à ~80-200 €/an), avec un processus de vérification d'identité strict.  
> - **SSL.com** : propose des EV individuels mais à prix proche des EV entreprises.  
>  
> Pour un développeur indépendant en 2026 : option la plus accessible = **immatriculation en micro-entreprise / auto-entrepreneur** pour pouvoir prendre un certificat OV au nom de la structure.

## Où obtenir un certificat ?

### Autorités de certification reconnues

Voici les principales autorités de certification (CA) qui délivrent des certificats de signature de code :

#### 1. DigiCert (anciennement Symantec/VeriSign)
- Site : https://www.digicert.com
- Leader du marché
- Excellente réputation
- Prix : 300-800€/an selon le type

#### 2. Sectigo (anciennement Comodo)
- Site : https://sectigo.com
- Bon rapport qualité/prix
- Très populaire
- Prix : 100-400€/an

#### 3. GlobalSign
- Site : https://www.globalsign.com
- Bien établi
- Service international
- Prix : 250-600€/an

#### 4. SSL.com
- Site : https://www.ssl.com
- Prix compétitifs
- Support multilingue
- Prix : 150-500€/an

#### 5. Certum (Asseco)
- Site : https://www.certum.eu
- Particulièrement accessible pour l'Europe
- Prix attractifs
- Prix : 80-350€/an

### Comparaison des fournisseurs

| Fournisseur | OV | EV | Cert. personne physique | HSM cloud | Support | Prix moyen |
|-------------|----|----|------------------------|-----------|---------|------------|
| DigiCert | ✓ | ✓ | ✗ | KeyLocker | Excellent | €€€ |
| Sectigo | ✓ | ✓ | ✗ | CodeSign HSM | Bon | €€ |
| GlobalSign | ✓ | ✓ | ✗ | AATL HSM | Bon | €€€ |
| SSL.com | ✓ | ✓ | EV individuel | eSigner | Bon | €€ |
| Certum | ✓ | ✓ | ✓ (Open Source) | ✗ | Moyen | € |
| **Azure Artifact Signing** | (n/a*) | (n/a*) | ✓ (USA/Canada) | natif Azure | Microsoft | € (9,99 USD/mois) |

*\* Azure Artifact Signing (ex-Trusted Signing) ne suit pas la classification OV/EV traditionnelle : Microsoft fournit le certificat, géré dans son propre HSM cloud. La réputation SmartScreen est gérée par Microsoft. Voir la section dédiée en fin de chapitre.*

**Recommandation pour débutants** :
- **Indépendant en USA/Canada** : **Azure Artifact Signing** (le plus économique et le plus simple, pas de token à manipuler).
- **Indépendant en Europe** : **Certum Open Source Code Signing** (en attendant qu'Azure Artifact Signing étende sa disponibilité aux particuliers européens).
- **PME avec structure juridique** : **Sectigo** ou **SSL.com** (OV traditionnel) OU **Azure Artifact Signing** (si organisation US/Canada/UE/UK).

## Processus d'obtention d'un certificat

### Étape 1 : Préparation des documents

Selon le type de certificat, vous devrez fournir :

**Pour un certificat personne physique** (rare en 2026 — typiquement Certum ou SSL.com) :
- Pièce d'identité officielle (passeport, carte d'identité).
- Justificatif de domicile récent (< 3 mois).
- Numéro de téléphone vérifiable.
- Pour Certum « Open Source Code Signing » : justification de projet open source publié (lien GitHub/GitLab ou similaire).

**Pour un certificat d'organisation (OV)** :
- Documents d'enregistrement de l'entreprise (Kbis en France, équivalent dans votre pays).
- Preuve d'adresse de l'entreprise (facture utilities, bail).
- Pièce d'identité du représentant légal.
- Numéro D-U-N-S (Dun & Bradstreet) parfois demandé pour les PME — gratuit à obtenir mais délai 1-2 semaines.

**Pour un certificat EV** :
- Tous les documents OV.
- Vérification téléphonique obligatoire via un numéro listé publiquement (Pages Jaunes, registre du commerce).
- Vérification opérationnelle : preuve que l'entreprise est active depuis au moins 3 ans **OU** preuve d'opérations commerciales (factures, contrats).

### Étape 2 : Demande en ligne

1. **Choisir le type de certificat** sur le site de l'autorité
2. **Remplir le formulaire** avec vos informations
3. **Soumettre les documents** numérisés
4. **Payer** (carte bancaire, virement)

### Étape 3 : Vérification

L'autorité de certification va :
- Vérifier vos documents (1-7 jours)
- Vous appeler pour confirmer (surtout pour EV)
- Vérifier les informations de votre entreprise dans les registres officiels

### Étape 4 : Réception du certificat

**Tous les certificats Code Signing depuis juin 2023** :
- Livrés sur **token USB cryptographique** (FIPS 140-2 niveau 2 minimum)
- Envoyés par **courrier postal** (3-10 jours selon le pays)
- Le certificat **ne peut pas être copié ni exporté** — il vit et meurt avec le token
- Le PIN est envoyé séparément (par email ou téléchargement sécurisé)

**Cas particulier — HSM cloud** :
Certaines CA proposent désormais une option « HSM-as-a-Service » (par ex. DigiCert KeyLocker, Sectigo CodeSign HSM, Azure Key Vault EV) qui évite le token physique. C'est l'option recommandée pour les pipelines CI/CD car le token physique est complexe à utiliser dans un agent de build automatisé.

### Étape 5 : Installation du token

1. Branchez le token USB
2. Installez les **pilotes** fournis par le fabricant (SafeNet Authentication Client pour eToken, ou logiciel équivalent)
3. Le certificat apparaît automatiquement dans le magasin Windows (`certmgr.msc` → Personnel)
4. Activez le PIN reçu (changement obligatoire au premier usage)
5. Le certificat est prêt à l'emploi — SignTool y accède via son empreinte (`/sha1`) ou son nom (`/n`)

> ⚠️ **Limite de signatures par PIN** : par défaut, beaucoup de tokens demandent le PIN à chaque signature, ce qui ralentit les builds CI. La plupart proposent un mode « *PIN cache* » à activer dans le logiciel client, mais il faut peser sécurité contre productivité.

### Coût total estimé (2026)

| Type | Certificat/an | Renouvellements | Total 3 ans |
|------|---------------|-----------------|-------------|
| Certum Open Source (personne physique) | 80-200 € | ~100 €/an | 350-600 € (+ ~80 € de token au 1er achat) |
| OV (entreprise, token hardware) | 300-600 € | ~400 €/an | 1200-1800 € |
| EV (entreprise, token hardware ou HSM cloud) | 500-1500 € | ~700-1200 €/an | 2500-5000 € |
| Surcoût HSM cloud (DigiCert KeyLocker, etc.) | +200-500 €/an | (idem) | + 600-1500 € sur 3 ans |

*Prix indicatifs en 2026. Le token hardware est généralement inclus dans le prix du premier certificat ; il est facturable séparément (~80-150 €) en cas de perte ou de second token.*

## Signer votre application Delphi

Une fois que vous avez votre certificat, vous pouvez signer vos exécutables.

### Méthode 1 : Avec SignTool (Windows SDK)

**SignTool** est l'outil officiel de Microsoft pour signer du code.

#### Installation de SignTool

SignTool fait partie du Windows SDK :

1. Téléchargez le **Windows SDK** depuis :
   https://developer.microsoft.com/windows/downloads/windows-sdk/

2. Pendant l'installation, sélectionnez uniquement :
   - "Windows SDK Signing Tools for Desktop Apps"

3. SignTool sera installé dans :
   `C:\Program Files (x86)\Windows Kits\10\bin\<version>\x64\signtool.exe`

#### Utilisation de SignTool — version moderne 2026

> ⚠️ **Pièges à éviter** :  
> - `/t` (sans `r`) utilise le format **Authenticode legacy SHA-1**, déprécié. Utiliser **`/tr`** pour le format **RFC 3161 SHA-256**.  
> - Préciser explicitement **`/td sha256`** pour le digest du timestamp (sinon SHA-1 par défaut).  
> - **`/fd sha256`** pour le digest de la signature elle-même.

**Commande moderne (recommandée en 2026)** — fichier .pfx (legacy, pré-juin 2023) :

```cmd
signtool sign /f "MonCertificat.pfx" /p "MotDePasse" /fd sha256 /tr http://timestamp.digicert.com /td sha256 "MonApplication.exe"
```

**Commande moderne avec token hardware (certificats OV/EV délivrés depuis 2023)** :

```cmd
signtool sign /sha1 "EMPREINTE_HEX_DU_CERTIFICAT" /fd sha256 /tr http://timestamp.digicert.com /td sha256 "MonApplication.exe"
```

> 💡 L'empreinte (*thumbprint*) du certificat se trouve dans `certmgr.msc` → propriétés du certificat → onglet *Détails* → champ *Empreinte numérique*. Notez-la sans espaces.

**Pour un certificat installé dans le magasin Windows par son nom** :

```cmd
signtool sign /n "Nom Affiché du Certificat" /fd sha256 /tr http://timestamp.digicert.com /td sha256 "MonApplication.exe"
```

**Explication des paramètres modernes** :

| Paramètre | Rôle | Valeur recommandée |
|---|---|---|
| `/f <fichier>` | Certificat depuis fichier .pfx | (legacy, plus délivré) |
| `/sha1 <empreinte>` | Certificat par empreinte (magasin) | hex en majuscules, sans espace |
| `/n <nom>` | Certificat par nom (magasin) | nom du sujet (CN) |
| `/p <password>` | Mot de passe (.pfx uniquement) | éviter en clair (cf CI) |
| **`/fd sha256`** | Algorithme de digest signature | **toujours `sha256`** |
| **`/tr <url>`** | Serveur timestamp RFC 3161 | (cf liste ci-dessous) |
| **`/td sha256`** | Algorithme de digest timestamp | **toujours `sha256`** |
| `/v` | Mode verbeux | utile pour debug |

#### L'importance du timestamp (horodatage RFC 3161)

Le timestamp est **crucial** ! Voici pourquoi :

- Sans timestamp : votre signature expire quand le certificat expire (1-3 ans). Une fois le certificat expiré, Windows considère le binaire comme non signé.
- Avec timestamp : la signature reste **valide indéfiniment** car le serveur de timestamp prouve que la signature a été créée pendant la validité du certificat.

**Serveurs de timestamp RFC 3161 recommandés** :
```
http://timestamp.digicert.com  
http://timestamp.sectigo.com  
http://timestamp.globalsign.com/tsa/r6advanced1  
http://ts.ssl.com  
http://timestamp.acs.microsoft.com  
```

Si un serveur ne répond pas (ce qui arrive régulièrement), réessayez avec un autre — le format est standardisé.

> ⚠️ **`http://timestamp.comodoca.com`** : ce serveur n'existe plus depuis le rachat de Comodo CA par Sectigo en 2018. Utiliser `http://timestamp.sectigo.com` à la place.

### Méthode 2 : Intégration dans Delphi

Vous pouvez configurer Delphi pour signer automatiquement à chaque compilation.

#### Configuration dans Delphi

1. **Ouvrir les options du projet**
   - `Projet` → `Options`

2. **Post-compilation**
   - Allez dans `Compilation` → `Événements de compilation`
   - Dans "Commandes post-compilation", ajoutez :

```cmd
REM ⚠ Le numéro de version après "Windows Kits\10\bin\" dépend de la  
REM   version du Windows SDK installée. Adaptez-le au vôtre :  
REM   - 10.0.22621.0 = SDK Windows 11 22H2  
REM   - 10.0.26100.0 = SDK Windows 11 24H2 (recommandé en 2026)  
REM   Ou utilisez `where signtool.exe` pour trouver le chemin actuel.  
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe" sign /f "$(PROJECTDIR)MonCertificat.pfx" /p "%CERT_PASSWORD%" /fd sha256 /tr http://timestamp.digicert.com /td sha256 "$(OUTPUTPATH)$(OUTPUTNAME)"
```

**Variables Delphi utiles** :
- `$(PROJECTDIR)` : Répertoire du projet
- `$(OUTPUTPATH)` : Chemin de sortie de l'exe
- `$(OUTPUTNAME)` : Nom de l'exécutable

3. **Sécurité du mot de passe**
   - Ne mettez **JAMAIS** votre mot de passe en clair dans les options du projet (commit accidentel dans Git → secret compromis publiquement).
   - Utilisez une variable d'environnement (`%CERT_PASSWORD%` ci-dessus) définie dans la session du développeur.
   - Préférez un token hardware (sans mot de passe en ligne de commande) ou un HSM cloud pour les pipelines CI.

#### Script de signature sécurisé

Créez un fichier `sign.bat` :

```batch
@echo off
REM Adapter la version du SDK (10.0.26100.0 = Windows 11 24H2).  
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"  
set CERT_FILE="C:\Certificats\MonCertificat.pfx"  
set TIMESTAMP=http://timestamp.digicert.com  

REM Demander le mot de passe (saisie masquée non native en batch — utiliser  
REM PowerShell pour un vrai masquage : Read-Host -AsSecureString)  
set /p CERT_PASS="Entrez le mot de passe du certificat : "  

REM Signer le fichier (commande moderne 2026 :  
REM   /fd sha256        = digest signature  
REM   /tr <url>         = timestamp RFC 3161  
REM   /td sha256        = digest du timestamp  
%SIGNTOOL% sign /f %CERT_FILE% /p %CERT_PASS% /fd sha256 /tr %TIMESTAMP% /td sha256 %1

REM Effacer le mot de passe de la variable (ne purge pas vraiment la RAM,  
REM mais évite la persistance dans la session shell)  
set CERT_PASS=  

echo Signature terminée !  
pause  
```

Utilisez-le ainsi :
```cmd
sign.bat "MonApplication.exe"
```

### Méthode 3 : Signer avec Inno Setup

Si vous utilisez Inno Setup pour créer un installateur, vous pouvez signer à la fois l'exécutable et l'installateur.

**Configuration dans le script .iss** :

```ini
[Setup]
; Définir l'outil de signature
SignTool=mysigntool

; Signer aussi le programme de désinstallation
SignedUninstaller=yes

; Configuration de SignTool — commande moderne 2026 (cf section "Utilisation
; de SignTool" plus haut pour le détail des options /fd, /tr, /td).
; Ajoutez ceci dans le menu Tools → Configure Sign Tools
; Name: mysigntool
; Command: "C:\Path\To\signtool.exe" sign /f "C:\Path\To\Certificate.pfx" /p "$qPassword$q" /fd sha256 /tr http://timestamp.digicert.com /td sha256 $f
```

**Note** : `$f` est remplacé par le nom du fichier à signer ; `$q` représente un guillemet double.

**Configuration de l'outil dans Inno Setup Compiler** :

1. Menu `Tools` → `Configure Sign Tools`
2. Cliquez sur `Add`
3. Nom : `mysigntool`
4. Commande (adapter la version du SDK Windows installée) :
```
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe" sign /f "C:\Certificats\MonCert.pfx" /p "MotDePasse" /fd sha256 /tr http://timestamp.digicert.com /td sha256 $f
```

### Méthode 4 : Signer avec InstallAware

InstallAware a une interface graphique pour la signature :

1. **Ouvrir les paramètres de build**
   - `Build` → `Build Settings`

2. **Onglet Code Signing**
   - Cochez "Sign setup and uninstall executables"
   - **Certificate File** : sélectionnez votre `.pfx` (legacy) OU sélectionnez un certificat du magasin Windows par empreinte (cas token hardware).
   - **Password** : entrez le mot de passe (uniquement pour .pfx legacy).
   - **Timestamp URL (RFC 3161)** : `http://timestamp.digicert.com`
   - **Hash Algorithm** : **SHA-256** (pas SHA-1).
   - **Timestamp Hash Algorithm** : **SHA-256**.

3. **Compiler**
   - InstallAware signera automatiquement l'installateur

## Vérifier une signature

### Vérifier dans l'Explorateur Windows

1. **Clic droit sur l'exécutable** → `Propriétés`
2. **Onglet "Signatures numériques"**
3. Vous devriez voir votre signature
4. Cliquez sur `Détails` pour voir les informations du certificat
5. Vérifiez :
   - Nom du signataire
   - Date de signature
   - Timestamp présent
   - "Cette signature numérique est correcte"

### Vérifier avec SignTool

```cmd
signtool verify /pa /v "MonApplication.exe"
```

**Paramètres** :
- `/pa` : Utilise la politique par défaut
- `/v` : Mode verbeux (affiche tous les détails)

**Sortie attendue** :
```
Successfully verified: MonApplication.exe

Number of signatures successfully Verified: 1
```

### Vérifier le timestamp

```cmd
signtool verify /pa /v "MonApplication.exe" | findstr "Timestamp"
```

Vous devriez voir la date et l'heure du timestamp.

## Signer plusieurs fichiers

Si votre application comprend plusieurs exécutables ou DLL, signez-les tous.
**SignTool accepte plusieurs fichiers en une seule invocation** — c'est plus
rapide (le certificat est chargé une seule fois, surtout sur token hardware) :

```batch
@echo off
REM Adapter la version du SDK (10.0.26100.0 = Windows 11 24H2).  
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"  

REM ✓ Une SEULE invocation pour tous les fichiers — gain de temps important  
REM   avec un token hardware qui demande le PIN à chaque appel sinon.  
%SIGNTOOL% sign /f "MonCert.pfx" /p "%CERT_PASSWORD%" ^
               /fd sha256 /tr http://timestamp.digicert.com /td sha256 ^
               "MonApp.exe" "MonPlugin.dll" "Assistant.exe"

echo Tous les fichiers signés !  
pause  
```

> 💡 **Avec wildcard** : `signtool sign ... *.exe *.dll` signe tous les .exe et .dll du dossier courant en une seule passe.

**Important** :
- Signez **AVANT** de créer l'installateur (l'installateur englobe les fichiers signés), puis signez aussi l'installateur lui-même.
- Vérifiez chaque signature après coup (`signtool verify /pa /v *.exe *.dll`) — il arrive qu'un fichier en cours d'utilisation par un antivirus ne soit pas signé correctement et SignTool n'émette qu'un warning.

## Renouvellement du certificat

Les certificats expirent. Planifiez le renouvellement :

### 30-60 jours avant expiration

1. **Commandez le nouveau certificat** auprès de votre CA
2. Le processus de vérification peut prendre quelques jours
3. Pour OV/EV : La vérification peut être plus rapide si vous renouvelez avec le même fournisseur

### Lors du renouvellement

**Option 1 — Nouveau certificat avec nouveau token / nouvelle clé** :
- Nouvelle clé privée générée sur un nouveau token (cas par défaut pour la plupart des CA en 2026).
- Réputation SmartScreen **repart de zéro** — le nouveau certificat doit reconstruire sa réputation (quelques semaines à quelques mois).
- Vous devrez re-signer toutes les versions courantes que vous voulez continuer à distribuer.

**Option 2 — Renouvellement « in-place » sur le même token (rare)** :
- Certaines CA (DigiCert KeyLocker, Azure Artifact Signing) permettent de **conserver la même clé** lors du renouvellement.
- Avantage : préserve la réputation construite.
- Avec un **token hardware** standard, la clé est physiquement liée au token : le renouvellement implique généralement un nouveau token + nouvelle clé.

> 💡 **Validité 2026** : la durée maximale historique de 3 ans a été progressivement réduite par le CA/Browser Forum. **Beaucoup de CA limitent désormais à 1 an** (validité Code Signing alignée sur les TLS server certs). Vérifiez la durée proposée à l'achat.

### Après renouvellement

1. **Re-signez les nouvelles versions** avec le nouveau certificat.
2. **Les anciennes versions distribuées restent valides** grâce au timestamp RFC 3161 (l'horodatage prouve que la signature a été faite pendant la validité du certificat).
3. **Mettez à jour** vos scripts de signature, CI/CD (`/sha1 "EMPREINTE_HEX"` change avec le nouveau cert).

### Transition

**Période de chevauchement recommandée** :
- Commencez à utiliser le nouveau certificat **1 mois avant expiration** de l'ancien.
- Cela évite toute interruption (test des builds, mise à jour des pipelines).
- Conservez l'ancien token jusqu'à la fin pour pouvoir re-signer en urgence un patch de l'ancienne version si besoin.

## Bonnes pratiques de sécurité

### 1. Protéger votre clé privée

**Votre certificat est précieux !** Si quelqu'un dérobe la clé privée, il peut signer des logiciels malveillants en votre nom — votre identité serait compromise sans recours rapide.

**En 2026 avec un token hardware (cas standard depuis juin 2023)** :
- Rangez le token dans un endroit sûr (coffre, tiroir verrouillé).
- Ne le laissez **jamais branché sans surveillance** (ni sur votre poste, ni sur un serveur CI).
- Utilisez le PIN fourni (à changer au premier usage) ; **ne notez pas le PIN sur le token**.
- Limitez le nombre d'essais : la plupart des tokens se verrouillent après 5-10 PIN incorrects.
- En cas de perte du token : **contactez immédiatement** la CA pour révocation + commandez un nouveau token.

**Pour les anciens certificats .pfx (avant juin 2023)** ou les certificats auto-signés de test :

- **Mot de passe fort** : minimum 16 caractères, complexe.
- **Stockage sécurisé** :
  - Utilisez un gestionnaire de mots de passe pour le mot de passe.
  - Stockez le fichier `.pfx` chiffré sur un disque externe.
  - Déconnectez le disque après usage.
- **Sauvegarde** : faites une copie sécurisée (le `.pfx` est exportable, à la différence du token).
- **Accès limité** : seules les personnes autorisées doivent y accéder.

### 2. Ne jamais partager votre certificat

- Ne l'envoyez jamais par email
- Ne le mettez jamais sur un serveur partagé non sécurisé
- Ne le commitez jamais dans un dépôt Git

### 3. Machine de signature dédiée

Pour les environnements professionnels :

- Utilisez une **machine de signature dédiée** isolée du réseau
- Transférez les fichiers à signer via un processus contrôlé
- Signez, puis transférez les fichiers signés

### 4. Audit et journalisation

Conservez un journal de toutes les signatures :

```
2025-01-15 14:30 - MonApp.exe v1.0.0 signé par Jean Dupont
2025-01-20 10:15 - MonApp.exe v1.0.1 signé par Jean Dupont
```

Cela aide à tracer les problèmes et à détecter les abus.

### 5. Révoquer si compromis

Si vous pensez que votre certificat est compromis :

1. **Contactez immédiatement** votre autorité de certification
2. **Demandez la révocation** du certificat
3. **Commandez un nouveau certificat**
4. **Re-signez** toutes vos applications

## Problèmes courants et solutions

### "Aucun certificat trouvé"

**Cause** : Certificat non installé ou chemin incorrect

**Solution** :
- Vérifiez que le certificat est bien installé
- Utilisez `certmgr.msc` pour voir les certificats installés
- Vérifiez le chemin vers le fichier `.pfx`

### "Mot de passe incorrect"

**Cause** : Mauvais mot de passe ou certificat corrompu

**Solution** :
- Vérifiez le mot de passe (attention aux majuscules/minuscules)
- Téléchargez à nouveau le certificat depuis votre CA
- Essayez d'importer le certificat manuellement dans Windows

### "Timestamp server did not respond"

**Cause** : Serveur de timestamp temporairement indisponible

**Solution** :
- Réessayez quelques minutes plus tard
- Utilisez un autre serveur de timestamp
- Vérifiez votre connexion Internet

### "This certificate is not valid for code signing"

**Cause** : Mauvais type de certificat

**Solution** :
- Vérifiez que c'est bien un certificat de **signature de code**
- Contactez votre CA si c'est le mauvais type
- Les certificats SSL/TLS ne peuvent pas signer du code

### La signature est invalide après signature

**Cause** : Fichier modifié après signature, ou erreur de signature

**Solution** :
- Vérifiez qu'aucun processus ne modifie le fichier après signature
- Désactivez temporairement l'antivirus qui pourrait analyser le fichier
- Re-signez le fichier

### SmartScreen bloque toujours l'application

**Cause** : nouvelle signature, pas encore de réputation construite.

**Solution** :
- **Patience** : la réputation se construit avec le temps et les téléchargements (compter quelques semaines à quelques mois pour un certificat OV neuf).
- **Certificat EV** : démarre avec une réputation initiale élevée mais **pas immédiate dans tous les cas** (Smart App Control sur Windows 11 22H2+ reste strict pendant les premiers mois).
- **Signalement Microsoft** : vous pouvez soumettre votre application à Microsoft pour analyse antimalware : https://www.microsoft.com/en-us/wdsi/filesubmission — cela accélère la construction de la réputation.

## Checklist de signature

Avant de distribuer votre application signée :

- [ ] Certificat valide et non expiré
- [ ] Mot de passe du certificat sécurisé
- [ ] Tous les exécutables signés (.exe, .dll)
- [ ] Installateur signé
- [ ] Programme de désinstallation signé
- [ ] Timestamp présent sur toutes les signatures
- [ ] Signatures vérifiées dans Propriétés Windows
- [ ] Test sur machine propre sans avertissement
- [ ] Nom du signataire correct
- [ ] Date de signature récente
- [ ] Certificat sauvegardé en lieu sûr
- [ ] Documentation de signature à jour

## Coûts annuels typiques en 2026

Les prix ont augmenté depuis l'obligation du token hardware (juin 2023). Voici un aperçu pour différents profils :

### Développeur indépendant / Petite application

- **Azure Artifact Signing** (USA/Canada) : **~110 €/an** (9,99 USD/mois) — pas de token à gérer, le moins cher si vous êtes éligible. Cf section dédiée.
- **Certum Open Source Code Signing** (rare CA acceptant les personnes physiques en Europe) : 80-200 €/an + token hardware (~80 € au 1er achat).
- **Alternative** : créer une auto-entreprise / micro-entreprise et prendre un certificat OV au nom de la structure.
- **Total 3 ans** : 600-1200 € (Certum) ou ~350 € (Azure Artifact Signing si éligible).

**Recommandation** :
- **USA/Canada** : Azure Artifact Signing (le moins cher, pas de hardware).
- **Europe** : Certum (en attendant la disponibilité Azure Artifact Signing pour particuliers européens) **OU** se constituer en structure pour Sectigo OV.

### PME / Application professionnelle

- **Certificat OV** (livré sur token hardware) : **300-600 €/an** selon la CA
- **Renouvellement** : ~400 €/an
- **Total 3 ans** : 1200-1800 €

**Recommandation** : Sectigo, DigiCert ou SSL.com.

### Grande distribution / Application critique / Solution cloud

- **Certificat EV** (token hardware ou HSM cloud) : **500-1500 €/an** selon la CA et l'édition
- **HSM cloud (DigiCert KeyLocker, etc.)** : surcoût ~200-500 €/an pour faciliter l'usage en CI/CD
- **Renouvellement** : ~700-1200 €/an
- **Token de remplacement** : ~80-150 € si perdu
- **Total 3 ans** : 2500-5000 €

**Recommandation** : DigiCert (meilleure réputation initiale, intégration HSM cloud mature) ou SSL.com (rapport qualité-prix).

## Alternatives et solutions gratuites

### Certificats auto-signés

Vous pouvez créer un certificat auto-signé gratuitement, **mais** :

❌ **Inconvénients majeurs** :
- Pas reconnu par Windows
- Avertissements encore plus graves
- Aucune réputation
- Inutile pour la distribution publique

✅ **Cas d'usage acceptable** :
- Tests internes
- Développement
- Distribution en entreprise (avec installation du certificat racine)

**Création d'un certificat auto-signé** (méthode actuelle, Windows 10/11 — PowerShell) :

```powershell
# ⚠ makecert est déprécié depuis 2014 — utiliser New-SelfSignedCertificate.
$cert = New-SelfSignedCertificate `
  -Type CodeSigningCert `
  -Subject "CN=Mon Nom" `
  -KeyAlgorithm RSA -KeyLength 3072 `
  -HashAlgorithm SHA256 `
  -CertStoreLocation "Cert:\CurrentUser\My" `
  -NotAfter (Get-Date).AddYears(2)

# Pour exporter en .pfx (utilisable avec SignTool /f) :
$pwd = ConvertTo-SecureString -String "MotDePasse123" -Force -AsPlainText
Export-PfxCertificate -Cert $cert -FilePath "MonAuto.pfx" -Password $pwd
```

### Certificats open source ?

Il n'existe **pas** d'équivalent à Let's Encrypt pour la signature de code grand public. Les autorités de certification doivent vérifier l'identité réelle des développeurs, ce qui a un coût.

> 💡 **Sigstore / cosign** : initiative de la Linux Foundation pour signer des artefacts (containers, modules) sans certificat permanent. Très utilisé pour Docker/Kubernetes, **mais pas reconnu par Windows Authenticode** — utile pour vos pipelines mais pas pour les .exe distribués au grand public.

### Microsoft Azure Artifact Signing (ex-Trusted Signing) — alternative moderne 2026

Microsoft propose depuis 2024 (GA en 2025-2026) un service **« signing-as-a-service »** très intéressant qui rebat les cartes pour les indépendants et PME :

**Caractéristiques** :
- **Pas de certificat à gérer** : Microsoft génère et stocke la clé dans Azure Key Vault, sur HSM.
- **Signature à l'usage** via API REST ou tâche Azure DevOps / GitHub Actions.
- **Réputation Microsoft intégrée** : SmartScreen accepte mieux ces signatures que les nouveaux certificats OV/EV.
- **Certificats à courte durée de vie** (3 jours) régénérés automatiquement — l'horodatage RFC 3161 garantit la validité long terme du binaire.

**Tarifs 2026** :
- **Basic** : **9,99 USD/mois** pour 5 000 signatures, puis 0,005 USD par signature supplémentaire.
- **Premium** : 99,99 USD/mois pour 100 000 signatures.
- C'est **5 à 10× moins cher** qu'un certificat OV/EV traditionnel sur 3 ans.

**Disponibilité (avril 2026)** :
- **Organisations** : USA, Canada, UE, Royaume-Uni.
- **Particuliers** (« self-employed individuals ») : USA et Canada uniquement pour l'instant. **Pas encore disponible aux particuliers européens** ; surveiller l'évolution.
- L'exigence historique de « 3 ans d'activité » a été supprimée lors du passage en GA.

**Workflow type** :
```yaml
# GitHub Actions — signer un .exe avec Azure Artifact Signing
- name: Sign with Azure Trusted Signing
  uses: azure/trusted-signing-action@v0
  with:
    azure-tenant-id: ${{ secrets.AZURE_TENANT_ID }}
    azure-client-id: ${{ secrets.AZURE_CLIENT_ID }}
    azure-client-secret: ${{ secrets.AZURE_CLIENT_SECRET }}
    endpoint: https://eus.codesigning.azure.net/
    trusted-signing-account-name: MyAccount
    certificate-profile-name: MyProfile
    files-folder: ${{ github.workspace }}/Win64/Release
    files-folder-filter: exe,dll
    file-digest: SHA256
    timestamp-rfc3161: http://timestamp.acs.microsoft.com
    timestamp-digest: SHA256
```

**Quand le choisir ?**
- Vous démarrez un nouveau projet en 2026 et vous êtes en Amérique du Nord ou organisation européenne → **excellent choix premier**.
- Vous voulez signer depuis un pipeline CI sans gérer de token hardware ni de HSM cloud séparé.
- Votre budget est limité (< 20 €/mois acceptables).

**Quand préférer un certificat OV/EV traditionnel ?**
- Particulier européen (Azure Artifact Signing pas encore disponible).
- Besoin de signer hors-ligne ou sans Azure.
- Politique d'entreprise imposant le contrôle complet de la clé.

**Référence** : https://azure.microsoft.com/products/artifact-signing

## Conclusion

La signature de code est un investissement qui en vaut la peine. Bien que les certificats aient un coût (80-1500 €/an selon le type et le profil — particulier ou entreprise), les bénéfices sont nombreux :

- **Confiance des utilisateurs** : Moins d'abandons à l'installation
- **Moins d'avertissements** : Expérience utilisateur améliorée
- **Protection de votre réputation** : Impossible de falsifier vos applications
- **Exigence professionnelle** : Standard dans l'industrie

**Points clés à retenir** :

1. Choisissez le bon type de certificat selon vos besoins
2. Protégez votre clé privée comme un trésor
3. Utilisez toujours un serveur de timestamp
4. Signez tous vos exécutables, pas seulement l'installateur
5. Testez les signatures avant distribution
6. Planifiez les renouvellements à l'avance

Avec une application correctement signée, vous maximisez vos chances de succès en offrant à vos utilisateurs une expérience d'installation professionnelle et sécurisée. Dans la section suivante, nous verrons comment mettre en place un système de mise à jour automatique pour maintenir votre application à jour chez vos utilisateurs.

⏭️ [Mise à jour automatique](/17-distribution-et-deploiement/05-mise-a-jour-automatique.md)
