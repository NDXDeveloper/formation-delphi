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
- **Microsoft Store** : Signature obligatoire
- **Entreprises** : Beaucoup n'autorisent que les applications signées
- **macOS** : Obligation de notarisation (équivalent de la signature)

### 5. Protection contre les modifications

Si quelqu'un modifie votre application signée (pour y ajouter un virus, par exemple), la signature devient invalide. Windows alertera immédiatement l'utilisateur.

### 6. Professionnalisme

Une application signée démontre le sérieux de votre démarche. C'est un signe de qualité et de confiance.

## Types de certificats de signature de code

Il existe deux principaux types de certificats :

### 1. Certificats de validation d'organisation (OV - Organization Validation)

**Caractéristiques** :
- Vérifie l'existence légale de votre entreprise
- Affiche le nom de votre organisation dans Windows
- Délivré après vérification des documents officiels
- Validité : 1 à 3 ans
- Coût : 100-500€ par an

**Adapté pour** : Entreprises établies, applications commerciales

### 2. Certificats de validation individuelle (IV - Individual Validation)

**Caractéristiques** :
- Pour les développeurs indépendants
- Vérifie votre identité personnelle
- Affiche votre nom dans Windows
- Délivré après vérification d'identité
- Validité : 1 à 3 ans
- Coût : 80-300€ par an

**Adapté pour** : Développeurs freelance, projets personnels, applications gratuites

### 3. Certificats de validation étendue (EV - Extended Validation)

**Caractéristiques** :
- Vérification la plus stricte
- **Réputation immédiate** dans Windows SmartScreen
- Livré sur clé USB physique (token hardware)
- Ne peut pas être exporté
- Validité : 1 à 3 ans
- Coût : 300-800€ par an

**Avantage majeur** : Pas d'avertissement SmartScreen dès le premier téléchargement !

**Adapté pour** : Applications professionnelles largement distribuées

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

| Fournisseur | OV/IV | EV | Support | Prix moyen |
|-------------|-------|-----|---------|------------|
| DigiCert | ✓ | ✓ | Excellent | €€€ |
| Sectigo | ✓ | ✓ | Bon | €€ |
| GlobalSign | ✓ | ✓ | Bon | €€€ |
| SSL.com | ✓ | ✓ | Bon | €€ |
| Certum | ✓ | ✓ | Moyen | € |

**Recommandation pour débutants** : Sectigo ou Certum offrent un bon équilibre entre prix et qualité.

## Processus d'obtention d'un certificat

### Étape 1 : Préparation des documents

Selon le type de certificat, vous devrez fournir :

**Pour un certificat individuel (IV)** :
- Pièce d'identité officielle (passeport, carte d'identité)
- Justificatif de domicile récent
- Numéro de téléphone vérifiable

**Pour un certificat d'organisation (OV)** :
- Documents d'enregistrement de l'entreprise (Kbis en France)
- Preuve d'adresse de l'entreprise
- Pièce d'identité du représentant légal
- Justificatif du nom de domaine (parfois)

**Pour un certificat EV** :
- Tous les documents OV
- Vérification téléphonique obligatoire
- Documents supplémentaires selon l'autorité

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

**Certificat OV/IV** :
- Envoyé par email au format `.pfx` ou `.p12`
- Protégé par un mot de passe
- À installer sur votre machine

**Certificat EV** :
- Livré sur clé USB cryptographique (token)
- Envoyé par courrier postal (3-7 jours)
- Le certificat ne peut pas être copié

### Étape 5 : Installation

**Pour certificat OV/IV (.pfx)** :

1. Double-cliquez sur le fichier `.pfx`
2. Suivez l'assistant d'importation de certificat Windows
3. Choisissez "Utilisateur actuel" ou "Ordinateur local"
4. Entrez le mot de passe fourni
5. Laissez Windows choisir automatiquement le magasin de certificats
6. Terminez l'importation

**Pour certificat EV (token USB)** :
- Branchez le token USB
- Installez les pilotes fournis
- Le certificat est prêt à l'emploi

### Coût total estimé

| Type | Certificat/an | Renouvellements | Total 3 ans |
|------|---------------|-----------------|-------------|
| IV | 100-300€ | ~200€/an | 500-900€ |
| OV | 150-400€ | ~300€/an | 750-1200€ |
| EV | 300-800€ | ~600€/an | 1800-2400€ |

*Les prix sont indicatifs et peuvent varier selon le fournisseur et les promotions*

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

#### Utilisation de SignTool

**Commande de base** :

```cmd
signtool sign /f "MonCertificat.pfx" /p "MotDePasse" /t http://timestamp.digicert.com /fd SHA256 "MonApplication.exe"
```

**Explication des paramètres** :

- `/f "MonCertificat.pfx"` : Chemin vers votre certificat
- `/p "MotDePasse"` : Mot de passe du certificat
- `/t http://timestamp.digicert.com` : Serveur d'horodatage (timestamp)
- `/fd SHA256` : Algorithme de hachage (SHA256 recommandé)
- `"MonApplication.exe"` : Fichier à signer

**Pour un certificat installé dans le magasin Windows** :

```cmd
signtool sign /n "Nom de votre certificat" /t http://timestamp.digicert.com /fd SHA256 "MonApplication.exe"
```

**Pour un certificat EV (token USB)** :

```cmd
signtool sign /sha1 "Empreinte_du_certificat" /t http://timestamp.digicert.com /fd SHA256 "MonApplication.exe"
```

#### L'importance du timestamp (horodatage)

Le timestamp est **crucial** ! Voici pourquoi :

- Sans timestamp : Votre signature expire quand le certificat expire (1-3 ans)
- Avec timestamp : La signature reste valide même après expiration du certificat

**Serveurs de timestamp recommandés** :
```
http://timestamp.digicert.com  
http://timestamp.sectigo.com  
http://timestamp.globalsign.com  
http://timestamp.comodoca.com  
```

Utilisez toujours un serveur de timestamp !

### Méthode 2 : Intégration dans Delphi

Vous pouvez configurer Delphi pour signer automatiquement à chaque compilation.

#### Configuration dans Delphi

1. **Ouvrir les options du projet**
   - `Projet` → `Options`

2. **Post-compilation**
   - Allez dans `Compilation` → `Événements de compilation`
   - Dans "Commandes post-compilation", ajoutez :

```cmd
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.22621.0\x64\signtool.exe" sign /f "$(PROJECTDIR)MonCertificat.pfx" /p "MotDePasse" /t http://timestamp.digicert.com /fd SHA256 "$(OUTPUTPATH)$(OUTPUTNAME)"
```

**Variables Delphi utiles** :
- `$(PROJECTDIR)` : Répertoire du projet
- `$(OUTPUTPATH)` : Chemin de sortie de l'exe
- `$(OUTPUTNAME)` : Nom de l'exécutable

3. **Sécurité du mot de passe**
   - Ne mettez jamais votre mot de passe en clair dans les options du projet
   - Utilisez un script externe ou un gestionnaire de mots de passe

#### Script de signature sécurisé

Créez un fichier `sign.bat` :

```batch
@echo off
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.22621.0\x64\signtool.exe"  
set CERT_FILE="C:\Certificats\MonCertificat.pfx"  
set TIMESTAMP=http://timestamp.digicert.com  

REM Demander le mot de passe  
set /p CERT_PASS="Entrez le mot de passe du certificat : "  

REM Signer le fichier
%SIGNTOOL% sign /f %CERT_FILE% /p %CERT_PASS% /t %TIMESTAMP% /fd SHA256 %1

REM Effacer le mot de passe de la mémoire  
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

; Configuration de SignTool
; Ajoutez ceci dans le menu Tools → Configure Sign Tools
; Name: mysigntool
; Command: "C:\Path\To\signtool.exe" sign /f "C:\Path\To\Certificate.pfx" /p "Password" /t http://timestamp.digicert.com /fd SHA256 $f
```

**Note** : `$f` est remplacé par le nom du fichier à signer.

**Configuration de l'outil dans Inno Setup Compiler** :

1. Menu `Tools` → `Configure Sign Tools`
2. Cliquez sur `Add`
3. Nom : `mysigntool`
4. Commande :
```
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.22621.0\x64\signtool.exe" sign /f "C:\Certificats\MonCert.pfx" /p "MotDePasse" /t http://timestamp.digicert.com /fd SHA256 $f
```

### Méthode 4 : Signer avec InstallAware

InstallAware a une interface graphique pour la signature :

1. **Ouvrir les paramètres de build**
   - `Build` → `Build Settings`

2. **Onglet Code Signing**
   - Cochez "Sign setup and uninstall executables"
   - **Certificate File** : Sélectionnez votre `.pfx`
   - **Password** : Entrez le mot de passe
   - **Timestamp URL** : `http://timestamp.digicert.com`
   - **Hash Algorithm** : SHA256

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

Si votre application comprend plusieurs exécutables ou DLL, signez-les tous :

```batch
@echo off
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.22621.0\x64\signtool.exe"  
set CERT=/f "MonCert.pfx" /p "MotDePasse" /t http://timestamp.digicert.com /fd SHA256  

%SIGNTOOL% sign %CERT% "MonApp.exe"
%SIGNTOOL% sign %CERT% "MonPlugin.dll"
%SIGNTOOL% sign %CERT% "Assistant.exe"

echo Tous les fichiers signés !  
pause  
```

**Important** : Signez AVANT de créer l'installateur, puis signez aussi l'installateur.

## Renouvellement du certificat

Les certificats expirent. Planifiez le renouvellement :

### 30-60 jours avant expiration

1. **Commandez le nouveau certificat** auprès de votre CA
2. Le processus de vérification peut prendre quelques jours
3. Pour OV/EV : La vérification peut être plus rapide si vous renouvelez avec le même fournisseur

### Lors du renouvellement

**Option 1 : Nouveau certificat**
- Nouveau certificat avec nouvelle clé privée
- Vous devrez re-signer toutes les applications

**Option 2 : Renouvellement avec même clé**
- Certains CA permettent de garder la même clé
- Préserve la réputation construite

### Après renouvellement

1. **Re-signez les nouvelles versions** avec le nouveau certificat
2. **Les anciennes versions** restent valides grâce au timestamp
3. **Mettez à jour** vos scripts de signature

### Transition

**Période de chevauchement recommandée** :
- Commencez à utiliser le nouveau certificat 1 mois avant expiration de l'ancien
- Cela évite toute interruption

## Bonnes pratiques de sécurité

### 1. Protéger votre clé privée

**Votre certificat est précieux !** Si quelqu'un le vole, il peut signer des logiciels en votre nom.

**Mesures de protection** :

- **Mot de passe fort** : Minimum 16 caractères, complexe
- **Stockage sécurisé** :
  - Utilisez un gestionnaire de mots de passe pour le mot de passe
  - Stockez le fichier `.pfx` chiffré sur un disque externe
  - Déconnectez le disque après usage
- **Sauvegarde** : Faites une copie sécurisée
- **Accès limité** : Seules les personnes autorisées doivent y accéder

**Pour certificats EV (token USB)** :
- Rangez le token dans un endroit sûr
- Ne le laissez jamais branché sans surveillance
- Utilisez le code PIN fourni

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

**Cause** : Nouvelle signature, pas encore de réputation

**Solution** :
- **Patience** : La réputation se construit avec le temps et les téléchargements
- **Certificat EV** : Réputation immédiate
- **Signalement Microsoft** : Vous pouvez signaler votre application à Microsoft : https://www.microsoft.com/en-us/wdsi/filesubmission

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

## Coûts annuels typiques

Voici un aperçu des coûts pour différents profils :

### Développeur indépendant / Petite application

- **Certificat IV** : 100-200€/an
- **Renouvellement** : ~150€/an
- **Total 3 ans** : 500-700€

**Recommandation** : Sectigo ou Certum

### PME / Application professionnelle

- **Certificat OV** : 200-400€/an
- **Renouvellement** : ~300€/an
- **Total 3 ans** : 900-1200€

**Recommandation** : DigiCert ou GlobalSign

### Grande distribution / Application critique

- **Certificat EV** : 400-800€/an
- **Token de remplacement** : ~50€ si perdu
- **Renouvellement** : ~600€/an
- **Total 3 ans** : 2000-2500€

**Recommandation** : DigiCert (meilleure réputation)

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

**Création d'un certificat auto-signé** :

```powershell
New-SelfSignedCertificate -Type CodeSigningCert -Subject "CN=Mon Nom" -CertStoreLocation "Cert:\CurrentUser\My"
```

### Certificats open source ?

Il n'existe **pas** d'équivalent à Let's Encrypt pour la signature de code. Les autorités de certification doivent vérifier l'identité réelle des développeurs, ce qui a un coût.

## Conclusion

La signature de code est un investissement qui en vaut la peine. Bien que les certificats aient un coût (100-800€/an selon le type), les bénéfices sont nombreux :

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
