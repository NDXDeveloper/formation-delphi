# 17.4 Signature de code

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Lorsque vous distribuez vos applications Delphi, les utilisateurs peuvent recevoir des avertissements de sécurité inquiétants indiquant que votre logiciel provient d'un "éditeur inconnu" ou qu'il n'est "pas digne de confiance". Pour éviter ces messages et renforcer la confiance des utilisateurs, la **signature de code** est essentielle.

La signature de code est un processus qui permet d'ajouter une "signature numérique" à vos applications, prouvant votre identité en tant que développeur et garantissant que le code n'a pas été modifié depuis sa création.

![Exemple d'avertissement sans signature](https://placeholder-image.com/windows-security-warning.png)

## Pourquoi signer votre code ?

La signature de code offre plusieurs avantages importants :

1. **Confiance de l'utilisateur** : Les utilisateurs voient le nom de votre entreprise au lieu de "Éditeur inconnu"
2. **Moins d'avertissements** : Réduit ou élimine les messages d'avertissement de Windows SmartScreen et des antivirus
3. **Intégrité du code** : Garantit que votre application n'a pas été modifiée par un tiers malveillant
4. **Prérequis pour certains magasins** : Nécessaire pour publier sur le Microsoft Store
5. **Compatibilité avec les environnements sécurisés** : Certaines entreprises n'autorisent que les applications signées

## Les bases de la signature de code

### 1. Comprendre les certificats de signature de code

Pour signer votre code, vous avez besoin d'un **certificat de signature de code**. C'est un fichier numérique qui contient :
- Votre identité (nom, organisation)
- Une clé publique et une clé privée
- La signature d'une autorité de certification (CA) qui vérifie votre identité

Ce certificat peut être stocké dans deux formats principaux :
- **Fichier PFX** (aussi appelé P12) : Contient à la fois la clé privée et le certificat
- **Magasin de certificats Windows** : Intégré au système Windows

### 2. Comment obtenir un certificat de signature de code

Il existe principalement deux façons d'obtenir un certificat :

#### A. Auprès d'une autorité de certification (CA) reconnue

C'est l'option recommandée pour les applications commerciales. Vous pouvez acheter un certificat auprès de fournisseurs tels que :

- DigiCert
- Sectigo (anciennement Comodo)
- GlobalSign
- Thawte

Le prix varie généralement entre 100€ et 500€ par an, selon le niveau de validation et le fournisseur.

Le processus d'obtention implique généralement :
1. La création d'une demande de certificat (CSR)
2. La vérification de votre identité par la CA (peut inclure des documents d'entreprise)
3. La réception et l'installation du certificat

#### B. Certificat auto-signé (pour les tests uniquement)

Vous pouvez créer gratuitement un certificat auto-signé pour les tests, mais il ne sera pas reconnu comme fiable par les systèmes des utilisateurs finaux.

## Processus de signature de code pour Delphi

### Étape 1 : Obtenir un certificat de signature de code

Comme expliqué précédemment, achetez un certificat auprès d'une CA reconnue en suivant leur processus d'acquisition.

### Étape 2 : Installer et configurer votre certificat

Une fois que vous avez reçu votre certificat, il doit être installé dans le magasin de certificats Windows ou conservé sous forme de fichier PFX.

#### Pour installer dans le magasin de certificats Windows :

1. Double-cliquez sur le fichier de certificat (.pfx ou .p12)
2. L'assistant d'importation de certificats s'ouvre
3. Sélectionnez "Utilisateur actuel" comme emplacement
4. Suivez les étapes et entrez le mot de passe fourni avec le certificat
5. Sélectionnez "Placer tous les certificats dans le magasin suivant" et choisissez "Personnel"
6. Terminez l'assistant

### Étape 3 : Signer votre application Delphi

Il existe plusieurs méthodes pour signer votre application Delphi :

#### Méthode 1 : Utiliser SignTool.exe (outil Microsoft)

SignTool est un outil en ligne de commande fourni avec le SDK Windows. Pour l'utiliser :

1. Installez le Windows SDK (disponible gratuitement sur le site de Microsoft)
2. Ouvrez une invite de commande et naviguez vers le dossier où se trouve votre application Delphi compilée
3. Utilisez la commande suivante pour signer votre application :

```batch
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe" sign /tr http://timestamp.digicert.com /td sha256 /fd sha256 /a "C:\Chemin\Vers\VotreApplication.exe"
```

> **Note :** Le chemin exact vers SignTool.exe peut varier selon votre version du SDK Windows.

Explication des paramètres :
- `/tr` : URL du service d'horodatage (permet à la signature de rester valide après l'expiration du certificat)
- `/td` et `/fd` : Algorithme de hachage utilisé (SHA-256 est recommandé)
- `/a` : Sélectionne automatiquement le bon certificat dans votre magasin

#### Méthode 2 : Signature post-compilation automatique dans Delphi

Vous pouvez configurer Delphi pour signer automatiquement votre application après la compilation :

1. Dans Delphi, allez dans le menu **Project** → **Options**
2. Sélectionnez **Build Events** dans le panneau de gauche
3. Dans la section **Post-Build Event Command**, ajoutez une commande de signature similaire à celle ci-dessus :

```batch
"C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe" sign /tr http://timestamp.digicert.com /td sha256 /fd sha256 /a "$(OUTPUTPATH)"
```

> **Note :** `$(OUTPUTPATH)` est une variable Delphi qui pointe vers le fichier exécutable généré.

![Configuration des événements de build](https://placeholder-image.com/delphi-build-events.png)

#### Méthode 3 : Utiliser un outil graphique

Si vous préférez une interface graphique, plusieurs outils existent :

1. **SignTool UI** : Une interface pour SignTool
2. **Advanced Installer** : Inclut des fonctions de signature de code
3. **signtool.tech** : Application gratuite avec interface simplifiée

### Étape 4 : Vérifier la signature

Pour vérifier que votre application a été correctement signée :

1. Faites un clic droit sur votre fichier .exe
2. Sélectionnez **Propriétés**
3. Allez dans l'onglet **Signatures numériques**
4. Vous devriez voir votre certificat listé

![Vérification de la signature](https://placeholder-image.com/verify-signature.png)

## Signer tous les composants de votre application

Il est important de signer non seulement l'exécutable principal, mais aussi tous les composants secondaires :

- **Fichiers DLL** : Si votre application utilise des DLL
- **Installateurs** : Fichiers .msi, .exe d'installation
- **Pilotes** : Si votre application inclut des pilotes (nécessite une signature spéciale)

Pour signer plusieurs fichiers en une fois, vous pouvez utiliser un script batch :

```batch
@echo off
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe"
set TIMESTAMP=http://timestamp.digicert.com
set FILES=MyApp.exe MyApp.dll Installer.exe

for %%f in (%FILES%) do (
    echo Signature de %%f...
    %SIGNTOOL% sign /tr %TIMESTAMP% /td sha256 /fd sha256 /a "%%f"
    if errorlevel 1 (
        echo ERREUR: Échec de la signature de %%f
        exit /b 1
    )
)

echo Tous les fichiers ont été signés avec succès.
```

## Horodatage (Timestamping)

L'horodatage est un aspect critique de la signature de code. Sans horodatage, la signature de votre application devient invalide lorsque votre certificat expire (généralement après 1-3 ans).

L'horodatage ajoute une preuve cryptographique que le code a été signé pendant que le certificat était valide, permettant à l'application de rester considérée comme signée même après l'expiration du certificat.

C'est pourquoi le paramètre `/tr` (ou `/t` pour les anciennes versions) est si important dans les commandes de signature.

## Automatisation de la signature dans vos projets Delphi

### Intégration dans un script de publication

Voici un exemple de script batch complet pour compiler et signer automatiquement votre projet Delphi :

```batch
@echo off
echo Compilation et signature du projet Delphi...

rem Définir les chemins
set DELPHI="C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\bds.exe"
set PROJECT="C:\Projects\MonProjetDelphi\MonProjet.dproj"
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe"
set OUTPUT="C:\Projects\MonProjetDelphi\Win64\Release\MonProjet.exe"

rem Compiler le projet en mode Release
echo Compilation en mode Release...
%DELPHI% -build -config=Release -platform=Win64 %PROJECT%
if errorlevel 1 (
    echo ERREUR: Échec de la compilation
    exit /b 1
)

rem Signer l'exécutable
echo Signature de l'exécutable...
%SIGNTOOL% sign /tr http://timestamp.digicert.com /td sha256 /fd sha256 /a %OUTPUT%
if errorlevel 1 (
    echo ERREUR: Échec de la signature
    exit /b 1
)

echo Compilation et signature terminées avec succès.
```

### Intégration dans un processus CI/CD

Si vous utilisez un système d'intégration continue (comme Jenkins, GitHub Actions, etc.), vous pouvez y intégrer la signature de code.

Exemple pour GitHub Actions :

```yaml
name: Build and Sign Delphi Application

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: windows-latest
    steps:
    - uses: actions/checkout@v2

    # Installation de Delphi (exemple avec un installateur silencieux)
    - name: Install Delphi
      run: |
        # ... commandes d'installation de Delphi ...

    # Compilation du projet
    - name: Build Delphi Project
      run: |
        "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\bds.exe" -build -config=Release -platform=Win64 MyProject.dproj

    # Importation du certificat (stocké en secret)
    - name: Import Code Signing Certificate
      run: |
        echo ${{ secrets.CODE_SIGNING_CERT }} | base64 --decode > certificate.pfx
        certutil -f -p ${{ secrets.CERT_PASSWORD }} -importpfx certificate.pfx

    # Signature du code
    - name: Sign Application
      run: |
        "C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe" sign /tr http://timestamp.digicert.com /td sha256 /fd sha256 /a /f certificate.pfx /p ${{ secrets.CERT_PASSWORD }} "Win64\Release\MyApplication.exe"
```

## Résolution des problèmes courants

### 1. "Certificat non trouvé"

**Problème :** SignTool ne trouve pas votre certificat.

**Solutions :**
- Vérifiez que le certificat est bien installé dans le magasin "Personnel"
- Utilisez l'option `/f` pour spécifier directement un fichier PFX : `/f "chemin\vers\certificat.pfx" /p "motdepasse"`

### 2. "Le délai d'attente de l'opération a expiré"

**Problème :** Le service d'horodatage ne répond pas.

**Solutions :**
- Essayez un autre service d'horodatage (ex: http://timestamp.sectigo.com)
- Vérifiez votre connexion Internet
- Augmentez le délai d'attente avec l'option `/tr /ts http://timestamp.example.com /td sha256 /v /d`

### 3. "La signature a échoué avec le code d'erreur 2"

**Problème :** Le fichier peut être verrouillé par un autre processus.

**Solutions :**
- Fermez toutes les instances de l'application
- Fermez Delphi ou tout débogueur
- Redémarrez l'ordinateur si nécessaire

## Signature de code dans les installateurs

Si vous utilisez un installateur comme ceux décrits dans la section précédente, vous devez également le signer :

### Pour Inno Setup

Ajoutez ces lignes à votre script Inno Setup :

```pascal
[Setup]
; Autres paramètres...
SignTool=signtool $f
SignedUninstaller=yes
```

Cela signera automatiquement l'installateur et le désinstallateur. Vous pouvez aussi spécifier une commande personnalisée :

```pascal
SignTool=signtool sign /tr http://timestamp.digicert.com /td sha256 /fd sha256 /a $f
```

### Pour InstallAware

1. Dans InstallAware, allez dans l'onglet **Digital Signatures**
2. Cochez "Sign my installation"
3. Configurez les paramètres de signature selon votre certificat

## Bonnes pratiques pour la signature de code

1. **Protégez vos clés privées** : Stockez-les dans un endroit sécurisé, idéalement sur un dispositif matériel (HSM ou token USB)

2. **Utilisez toujours l'horodatage** : Essentiel pour la validité à long terme

3. **Signez tous les fichiers exécutables** : Pas seulement l'application principale

4. **Utilisez SHA-256** : Les algorithmes plus anciens (SHA-1) ne sont plus considérés comme sécurisés

5. **Automatisez le processus** : Intégrez la signature dans votre workflow de compilation pour éviter les oublis

6. **Renouvelez votre certificat à temps** : Prévoyez le renouvellement plusieurs semaines avant l'expiration

7. **Testez sur différentes versions de Windows** : La validation des signatures peut varier entre les versions

## Vérifier la signature du point de vue de l'utilisateur

Pour comprendre ce que vos utilisateurs verront, testez votre application signée :

1. Copiez l'application signée sur un autre ordinateur (ou une machine virtuelle)
2. Désactivez temporairement tout logiciel qui contourne automatiquement les avertissements
3. Double-cliquez sur l'application pour la lancer
4. Observez les avertissements (ou leur absence)
5. Vérifiez les propriétés du fichier pour voir les informations de signature

Une application correctement signée devrait afficher votre nom d'entreprise dans les propriétés et réduire considérablement les avertissements de sécurité.

## Évolution des exigences de signature de code

Les exigences de sécurité évoluent constamment. Voici quelques tendances importantes :

1. **EV (Extended Validation)** : Certificats avec validation renforcée, recommandés pour les pilotes
2. **Signature double** : Utilisation de SHA-1 et SHA-256 pour la compatibilité avec les anciens systèmes
3. **Notarisation** : Processus supplémentaire (comme sur macOS) qui pourrait arriver sur Windows

Il est important de se tenir informé des dernières exigences, particulièrement si vous développez des logiciels qui nécessitent un haut niveau de confiance.

## Conclusion

La signature de code est une étape essentielle dans le processus de distribution d'applications Delphi professionnelles. Elle renforce la confiance des utilisateurs, réduit les avertissements de sécurité et prouve l'intégrité de votre code.

Bien que le processus initial d'obtention d'un certificat puisse sembler complexe et coûteux, les avantages en termes de confiance et d'expérience utilisateur en valent largement la peine, particulièrement pour les applications commerciales.

En suivant les étapes décrites dans ce chapitre, vous pourrez signer efficacement vos applications Delphi et offrir une expérience d'installation plus professionnelle et sécurisée à vos utilisateurs.

Dans la prochaine section, nous aborderons la mise à jour automatique, qui permet à vos utilisateurs de toujours disposer de la dernière version de votre application.

## Exercice pratique

1. Créez un certificat auto-signé à des fins de test en utilisant l'outil "makecert" du SDK Windows ou OpenSSL
2. Compilez une application Delphi simple en mode Release
3. Signez l'application avec votre certificat de test en utilisant SignTool
4. Vérifiez la signature dans les propriétés du fichier
5. Créez un script batch simple pour automatiser le processus de signature

⏭️ [Mise à jour automatique](17-distribution-et-deploiement/05-mise-a-jour-automatique.md)
