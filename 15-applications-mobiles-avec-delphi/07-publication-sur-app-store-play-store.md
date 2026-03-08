🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.7 Publication sur App Store / Play Store

## Introduction

Vous avez développé votre application mobile, l'avez testée et perfectionnée. L'étape finale, mais cruciale, consiste à la publier sur les stores officiels pour que des millions d'utilisateurs puissent la découvrir et l'installer. Cette étape peut sembler intimidante au premier abord, mais en suivant méthodiquement les différentes étapes, vous pourrez publier votre application avec succès.

La publication d'une application implique bien plus que simplement uploader un fichier. Vous devez préparer des éléments visuels (icônes, captures d'écran), rédiger une description attractive, configurer correctement votre application, et naviguer dans les processus de validation de Google et Apple, qui ont chacun leurs propres exigences et particularités.

Dans cette section, nous allons explorer en détail comment publier votre application Delphi sur le **Google Play Store** (Android) et l'**Apple App Store** (iOS), de la préparation initiale jusqu'à la mise en ligne effective.

## Préparation générale de l'application

Avant même de penser à soumettre votre application aux stores, vous devez la préparer soigneusement.

### Finalisation du code

Assurez-vous que votre application est stable et sans bugs critiques :

```pascal
// Désactiver les fonctionnalités de débogage en production
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  {$IFDEF DEBUG}
  // Code de débogage actif uniquement en mode développement
  LabelDebug.Visible := True;
  BtnTestCrash.Visible := True;
  {$ELSE}
  // En production, masquer les éléments de débogage
  LabelDebug.Visible := False;
  BtnTestCrash.Visible := False;
  {$ENDIF}
end;

// Gérer toutes les exceptions non capturées
procedure TFormMain.ConfigurerGestionErreurs;  
begin  
  Application.OnException := GererExceptionGlobale;
end;

procedure TFormMain.GererExceptionGlobale(Sender: TObject; E: Exception);  
begin  
  // En production, logger l'erreur au lieu de crasher
  {$IFNDEF DEBUG}
  LoggerErreur(E.Message);
  ShowMessage('Une erreur est survenue. Veuillez réessayer.');
  {$ELSE}
  // En développement, afficher l'erreur complète
  raise;
  {$ENDIF}
end;
```

### Configuration du projet

Dans Delphi, configurez correctement les informations de votre application :

**Project > Options > Version Info** :

```pascal
// Informations essentielles à configurer dans l'IDE :
// - Company Name : Nom de votre entreprise
// - File Description : Description courte de l'application
// - File Version : Version du fichier (ex: 1.0.0.0)
// - Product Name : Nom de votre application
// - Product Version : Version produit (ex: 1.0.0)
// - Legal Copyright : Droits d'auteur

// Ces informations seront incluses dans l'APK/IPA
```

**Project > Options > Application** :

- **Title** : Nom affiché de l'application
- **Package** : Identifiant unique (ex: com.votreentreprise.nomapp)
  - Android : com.exemple.monapp
  - iOS : com.exemple.monapp (doit correspondre à l'App ID Apple)

### Icônes de l'application

Les icônes sont cruciales car elles sont la première chose que voient les utilisateurs.

**Exigences pour Android** :
- Multiples résolutions nécessaires : 36x36, 48x48, 72x72, 96x96, 144x144, 192x192 pixels
- Format PNG avec fond transparent ou opaque
- Design simple et reconnaissable

**Exigences pour iOS** :
- Multiples tailles : 40x40, 60x60, 76x76, 120x120, 152x152, 180x180, 1024x1024 pixels
- Format PNG, pas de transparence
- Pas d'effets de brillance (iOS les applique automatiquement)

**Configuration dans Delphi** :

```
Project > Options > Application > Icons
- Ajouter chaque taille d'icône requise
- Delphi génère automatiquement les ressources appropriées
```

**Conseils pour créer de bonnes icônes** :
- Design minimaliste et clair
- Testez l'icône à différentes tailles
- Évitez le texte trop petit
- Utilisez des couleurs vives et contrastées
- Soyez cohérent avec l'identité visuelle de votre marque

### Splash Screen (Écran de démarrage)

L'écran de démarrage s'affiche pendant le chargement de l'application.

**Configuration dans Delphi** :

```
Project > Options > Application > Splash Images
- Ajouter des images pour différentes résolutions et orientations
- Portrait et Paysage
- Différentes densités d'écran (ldpi, mdpi, hdpi, xhdpi, xxhdpi, xxxhdpi)
```

**Dimensions recommandées pour Android** :
- 320x480 (mdpi)
- 480x800 (hdpi)
- 720x1280 (xhdpi)
- 1080x1920 (xxhdpi)
- 1440x2560 (xxxhdpi)

**Dimensions pour iOS** :
- iPhone : 640x1136, 750x1334, 1242x2208
- iPad : 1024x768, 2048x1536

### Permissions et configuration

Vérifiez que toutes les permissions nécessaires sont correctement déclarées :

```
Project > Options > Uses Permissions (Android)
```

**Permissions courantes** :
- INTERNET : Accès réseau
- ACCESS_FINE_LOCATION : GPS précis
- ACCESS_COARSE_LOCATION : Localisation approximative
- CAMERA : Appareil photo
- RECORD_AUDIO : Microphone
- READ_EXTERNAL_STORAGE / WRITE_EXTERNAL_STORAGE : Stockage
- VIBRATE : Vibration

**Important** : Ne demandez que les permissions réellement nécessaires. Les utilisateurs sont méfiants envers les applications qui demandent trop de permissions.

## Publication sur Google Play Store (Android)

Le Google Play Store est la boutique d'applications officielle pour Android.

### Étape 1 : Créer un compte développeur

1. Rendez-vous sur https://play.google.com/console
2. Connectez-vous avec un compte Google
3. Acceptez les conditions d'utilisation
4. Payez les frais d'inscription unique de 25$ (paiement unique, valable à vie)
5. Complétez votre profil développeur

**Informations requises** :
- Nom du développeur (sera visible publiquement)
- Adresse email de contact
- Site web (optionnel mais recommandé)
- Numéro de téléphone

### Étape 2 : Préparer l'APK/AAB

Delphi peut générer deux formats pour Android :

**APK (Android Package)** :
- Format traditionnel
- Un fichier pour toutes les architectures (ARM, x86)
- Plus gros mais simple

**AAB (Android App Bundle)** :
- Format moderne recommandé par Google
- Google génère des APK optimisés pour chaque appareil
- Fichiers plus petits pour les utilisateurs

**Compiler pour la production dans Delphi** :

1. Sélectionnez la configuration **Release** :
   ```
   Project > Build Configurations > Release
   ```

2. Sélectionnez la plateforme **Android** :
   ```
   Project > Target Platform > Android
   ```

3. Configurez la signature de l'application :
   ```
   Project > Options > Provisioning (Android)
   ```

### Étape 3 : Signer l'application

Android exige que toutes les applications soient signées numériquement.

**Créer un keystore (trousseau de clés)** :

Vous pouvez le faire directement dans Delphi :

```
Tools > Options > Environment Options > Android Keystore Manager
- Cliquer sur "New Keystore"
- Choisir un emplacement sécurisé
- Définir un mot de passe fort
- Remplir les informations (nom, organisation, ville, pays)
```

**IMPORTANT** : Conservez précieusement ce keystore et son mot de passe ! Si vous les perdez, vous ne pourrez plus jamais mettre à jour votre application.

**Configurer la signature dans le projet** :

```
Project > Options > Provisioning > Android
- Key Store : Chemin vers votre fichier .keystore
- Key Alias : Alias de la clé
- Passwords : Mots de passe du keystore et de la clé
```

### Étape 4 : Compiler l'application

```
Project > Build [Nom de votre projet]
```

Delphi génère le fichier APK ou AAB dans le dossier de sortie (généralement dans un sous-dossier `Android\Release`).

### Étape 5 : Créer l'application dans Play Console

1. Connectez-vous à la Play Console : https://play.google.com/console
2. Cliquez sur **Créer une application**
3. Renseignez les informations de base :
   - Nom de l'application
   - Langue par défaut
   - Type d'application (gratuite ou payante)
   - Catégorie

### Étape 6 : Préparer la fiche de l'application

**Description courte** (80 caractères max) :
- Phrase accrocheuse décrivant l'essence de l'application
- Exemple : "Gérez vos tâches facilement et ne ratez plus aucune échéance"

**Description complète** (4000 caractères max) :
- Présentez votre application en détail
- Listez les fonctionnalités principales
- Expliquez les avantages pour l'utilisateur
- Utilisez des paragraphes pour faciliter la lecture

**Captures d'écran** :

Vous devez fournir au minimum 2 captures (maximum 8) :

- Format : PNG ou JPEG
- Dimensions minimales : 320 pixels
- Dimensions maximales : 3840 pixels
- Ratio d'aspect entre 16:9 et 9:16

**Conseils** :
- Montrez les fonctionnalités principales de votre application
- Ajoutez du texte explicatif sur les captures (avec un outil externe)
- Utilisez des mises en situation réelles
- Montrez l'interface dans différents contextes d'utilisation

**Icône de haute résolution** :
- 512 x 512 pixels
- Format PNG, 32 bits
- Taille max : 1 Mo

**Graphic feature (optionnel mais recommandé)** :
- 1024 x 500 pixels
- Bannière promotionnelle affichée sur certains écrans
- Texte lisible et design attractif

**Vidéo de présentation** (optionnel) :
- Lien YouTube
- Durée recommandée : 30 secondes à 2 minutes
- Montrez l'application en action

### Étape 7 : Remplir les informations obligatoires

**Catégorie** :
- Choisissez la catégorie la plus appropriée (Productivité, Jeux, Finance, etc.)

**Classification du contenu** :
- Répondez au questionnaire sur le contenu de votre application
- Obtenez une classification d'âge (3+, 7+, 12+, 16+, 18+)

**Coordonnées** :
- Email de contact (visible publiquement)
- Site web (optionnel)
- Numéro de téléphone (optionnel)

**Politique de confidentialité** :
- URL vers votre politique de confidentialité
- **Obligatoire** si vous collectez des données personnelles
- De nombreux générateurs gratuits en ligne peuvent vous aider

### Étape 8 : Uploader l'APK/AAB

1. Allez dans **Production > Créer une version**
2. Uploadez votre fichier APK ou AAB
3. Google analyse automatiquement le fichier et détecte :
   - Version de l'application
   - Permissions demandées
   - Architectures supportées
   - Taille de l'application

4. Rédigez les **notes de version** :
   - Décrivez les nouveautés de cette version
   - Listez les corrections de bugs
   - Mentionnez les améliorations

### Étape 9 : Configuration des prix et de la distribution

**Prix** :
- Gratuite (vous ne pourrez plus changer en payante après publication)
- Payante (définissez le prix dans chaque devise)

**Pays disponibles** :
- Sélectionnez les pays où votre application sera disponible
- Par défaut : tous les pays

**Programme pour les enfants et les familles** :
- Indiquez si votre application cible les enfants

### Étape 10 : Soumettre pour validation

1. Vérifiez que tous les éléments obligatoires sont complétés
2. Cliquez sur **Examiner la version**
3. Vérifiez une dernière fois toutes les informations
4. Cliquez sur **Publier la version**

**Délai de validation** :
- Généralement quelques heures
- Peut prendre jusqu'à 7 jours dans certains cas
- Vous recevrez un email lorsque l'application sera publiée ou si elle est rejetée

### Motifs courants de rejet sur Play Store

- **Permissions excessives** : Demande de permissions non justifiées
- **Contenu trompeur** : Description ne correspondant pas à l'application
- **Violation de droits d'auteur** : Utilisation non autorisée de contenu protégé
- **Politique de confidentialité manquante** : Si vous collectez des données
- **Fonctionnalités cassées** : Bugs critiques ou crashs fréquents
- **Contenu inapproprié** : Non conforme aux règles de Google

## Publication sur Apple App Store (iOS)

L'App Store d'Apple a un processus plus strict et plus long que le Play Store.

### Étape 1 : Compte Apple Developer

1. Inscrivez-vous sur https://developer.apple.com
2. Choisissez le type de compte :
   - **Personnel** : 99$/an
   - **Organisation** : 99$/an (nécessite un numéro DUNS)
3. Complétez votre profil
4. Payez les frais annuels

**Important** : Le compte Apple Developer nécessite un renouvellement annuel, contrairement au compte Google Play.

### Étape 2 : Certificats et profils de provisionnement

Cette étape est la plus complexe du processus iOS.

**Certificats nécessaires** :

1. **Development Certificate** : Pour tester sur appareils réels
2. **Distribution Certificate** : Pour publier sur l'App Store

**Créer les certificats** :

1. Ouvrez **Keychain Access** sur Mac
2. Menu : Keychain Access > Certificate Assistant > Request a Certificate from a Certificate Authority
3. Entrez votre email et votre nom
4. Sélectionnez "Saved to disk"
5. Sauvegardez le fichier .certSigningRequest

6. Allez sur https://developer.apple.com/account/resources/certificates
7. Cliquez sur **+** pour créer un nouveau certificat
8. Choisissez **iOS Distribution (App Store and Ad Hoc)**
9. Uploadez le fichier .certSigningRequest
10. Téléchargez le certificat généré (.cer)
11. Double-cliquez pour l'installer dans Keychain Access

**App ID** :

1. Allez dans https://developer.apple.com/account/resources/identifiers
2. Créez un nouvel **App ID**
3. Choisissez **Explicit App ID**
4. Bundle ID : com.votreentreprise.nomapp (doit correspondre exactement à celui dans Delphi)
5. Activez les **Capabilities** nécessaires (Push Notifications, In-App Purchase, etc.)

**Provisioning Profile** :

1. Allez dans https://developer.apple.com/account/resources/profiles
2. Créez un nouveau profil **App Store**
3. Sélectionnez votre App ID
4. Sélectionnez votre Distribution Certificate
5. Téléchargez le profil (.mobileprovision)

### Étape 3 : Configurer Delphi pour iOS

**Installer PAServer sur Mac** :

1. Sur votre Mac, installez les outils Platform Assistant depuis le CD d'installation Delphi
2. Ou téléchargez depuis le site Embarcadero
3. Lancez PAServer sur le Mac

**Configurer la connexion dans Delphi** :

```
Tools > Options > Environment Options > Connection Profile Manager
- Add (Ajouter un nouveau profil)
- Platform : macOS
- Host Name : Adresse IP de votre Mac
- Port : 64211 (par défaut)
- Password : Le mot de passe affiché par PAServer
- Test Connection
```

**Configurer le provisioning** :

```
Project > Options > Provisioning (iOS)
- Profile : Sélectionnez votre provisioning profile
```

### Étape 4 : Compiler pour iOS

1. Sélectionnez **Release** configuration
2. Sélectionnez la plateforme **iOS Device - 64 bit**
3. Project > Build

Delphi compile l'application sur le Mac via PAServer et génère un fichier .IPA.

### Étape 5 : Créer l'application dans App Store Connect

1. Connectez-vous à https://appstoreconnect.apple.com
2. Cliquez sur **My Apps**
3. Cliquez sur **+** puis **New App**

**Informations requises** :
- **Platforms** : iOS
- **Name** : Nom de l'application (30 caractères max)
- **Primary Language** : Langue principale
- **Bundle ID** : Sélectionnez l'App ID créé précédemment
- **SKU** : Identifiant unique de votre choix (ex: MONAPP001)

### Étape 6 : Préparer les métadonnées

**Captures d'écran** :

Apple exige des captures pour différentes tailles d'appareils :

- **iPhone 6.7"** : 1290 x 2796 pixels (iPhone 14 Pro Max, etc.)
- **iPhone 6.5"** : 1242 x 2688 pixels (iPhone 11 Pro Max, etc.)
- **iPhone 5.5"** : 1242 x 2208 pixels (iPhone 8 Plus, etc.)
- **iPad Pro 12.9"** : 2048 x 2732 pixels
- **iPad Pro (3ème gen) 11"** : 1668 x 2388 pixels

Minimum 3 captures, maximum 10 par taille d'appareil.

**Conseils** :
- Utilisez des captures d'écran réelles de votre application
- Ajoutez du contexte avec du texte et des annotations (avec un outil externe)
- Montrez les fonctionnalités clés
- Utilisez des images de haute qualité

**Icône de l'App Store** :
- 1024 x 1024 pixels
- Format PNG, sans transparence
- Pas d'effets de bord arrondis (Apple les applique automatiquement)

**Description** :

- **Promotional Text** (170 caractères) : Texte promotionnel modifiable après publication
- **Description** (4000 caractères) : Description complète de l'application
- **Keywords** (100 caractères) : Mots-clés séparés par des virgules pour la recherche
- **Support URL** : URL d'assistance obligatoire
- **Marketing URL** : URL marketing (optionnel)

**Vidéo de prévisualisation (App Preview)** (optionnel) :
- Format MP4 ou MOV
- Durée : 15 à 30 secondes
- Dimensions spécifiques pour chaque taille d'appareil
- Montrez l'application en action

### Étape 7 : Informations sur l'application

**Catégorie** :
- Catégorie primaire (obligatoire)
- Catégorie secondaire (optionnel)

**Classification d'âge** :
- Répondez au questionnaire sur le contenu
- Apple détermine automatiquement la classification

**Prix et disponibilité** :
- Gratuite ou payante
- Prix par région
- Disponibilité géographique
- Date de publication

**App Privacy (Confidentialité)** :
- Obligatoire depuis iOS 14
- Déclarez les types de données collectées
- Indiquez si elles sont liées à l'utilisateur
- Indiquez si elles sont utilisées pour du tracking

### Étape 8 : Uploader le build avec Application Loader

**Option 1 : Via Xcode** (sur Mac)
1. Ouvrez **Xcode**
2. Window > Organizer
3. Sélectionnez votre .IPA
4. Cliquez sur **Upload to App Store**

**Option 2 : Via Transporter** (application Apple)
1. Téléchargez Transporter depuis le Mac App Store
2. Glissez-déposez votre fichier .IPA
3. Cliquez sur **Deliver**

**Option 3 : Via ligne de commande**
```bash
xcrun altool --upload-app --type ios --file "chemin/vers/votre/app.ipa" \
  --username "votre@email.com" --password "mot-de-passe-app-specific"
```

### Étape 9 : Soumettre pour validation

1. Retournez dans App Store Connect
2. Dans la section **App Store**, sélectionnez votre build uploadé
3. Remplissez les **App Review Information** :
   - Coordonnées de contact pour l'équipe de validation
   - Notes pour les testeurs (si besoin d'un compte de test, par exemple)
   - Pièce jointe (capture ou document explicatif si nécessaire)

4. Acceptez les accords de distribution
5. Cliquez sur **Submit for Review**

**Délai de validation** :
- En moyenne : 24 à 48 heures
- Peut aller jusqu'à 7 jours
- La première soumission prend généralement plus de temps
- Vous recevez des notifications par email à chaque étape

### Étape 10 : Répondre aux retours d'Apple

Si votre application est rejetée, Apple fournit une explication détaillée.

**Motifs courants de rejet sur App Store** :

1. **Interface non conforme** : Interface qui ne respecte pas les Human Interface Guidelines
2. **Bugs ou crashs** : Application instable lors des tests
3. **Métadonnées trompeuses** : Description ou captures d'écran ne reflétant pas l'application
4. **Contenu inapproprié** : Violation des règles de contenu d'Apple
5. **Fonctionnalités incomplètes** : Fonctionnalités annoncées mais non fonctionnelles
6. **Permissions non justifiées** : Demande de permissions sans explication claire
7. **Liens cassés** : Liens dans l'application qui ne fonctionnent pas
8. **Politique de confidentialité manquante** : Si vous collectez des données
9. **Utilisation des API non documentée** : Accès à des fonctions privées d'iOS
10. **Design minimal** : Application trop simple ou ressemblant à un site web encapsulé

**Comment répondre à un rejet** :
1. Lisez attentivement les raisons du rejet
2. Corrigez les problèmes identifiés
3. Testez rigoureusement
4. Soumettez à nouveau avec des notes explicatives si nécessaire

## Gestion des mises à jour

### Versionnement

Utilisez un système de versionnement cohérent :

**Format recommandé** : MAJEUR.MINEUR.CORRECTIF

- **MAJEUR** : Changements majeurs, incompatibilités possibles
- **MINEUR** : Nouvelles fonctionnalités, compatibles
- **CORRECTIF** : Corrections de bugs

Exemple : 1.2.3
- Version majeure : 1
- Version mineure : 2
- Correctif : 3

**Dans Delphi** :
```
Project > Options > Version Info
- File Version : 1.2.3.0
- Product Version : 1.2.3
```

**Important** :
- Sur Android : Incrémentez le **Version Code** (nombre entier) à chaque nouvelle version
- Sur iOS : Incrémentez le **CFBundleVersion** (Build Number)

### Publier une mise à jour

**Google Play Store** :
1. Compilez la nouvelle version avec un Version Code supérieur
2. Allez dans Play Console > Production
3. Créez une nouvelle version
4. Uploadez le nouvel APK/AAB
5. Rédigez les notes de version
6. Publiez

**Apple App Store** :
1. Compilez la nouvelle version avec un numéro de build supérieur
2. Uploadez le build via Xcode/Transporter
3. Dans App Store Connect, créez une nouvelle version
4. Sélectionnez le nouveau build
5. Renseignez "What's New in This Version"
6. Soumettez pour validation

### Déploiement progressif

**Play Store - Déploiement progressif** :
- Publiez d'abord à 10% des utilisateurs
- Surveillez les crashs et les notes
- Augmentez progressivement si tout va bien
- Rollback rapide en cas de problème

**App Store - Publication par étapes** :
- Publication automatique après validation, ou
- Publication manuelle à la date de votre choix
- Version limitée avec TestFlight pour tester en production

## Promotion et visibilité

### Optimisation de la fiche (ASO - App Store Optimization)

**Titre** :
- Incluez des mots-clés pertinents
- Soyez descriptif mais concis
- Différenciez-vous de la concurrence

**Description** :
- Commencez par les avantages principaux
- Utilisez des paragraphes courts
- Incluez des mots-clés naturellement
- Terminez par un appel à l'action

**Mots-clés** (iOS uniquement) :
- Recherchez les mots-clés populaires
- Évitez la répétition (déjà dans le titre)
- Testez et ajustez régulièrement
- Utilisez des variations et synonymes

**Captures d'écran** :
- Montrez la valeur ajoutée en premier
- Utilisez du texte superposé pour expliquer
- Créez une histoire visuelle
- Testez différentes versions (A/B testing)

### Obtenir des avis positifs

Les avis influencent fortement les téléchargements.

**Bonnes pratiques** :
- Demandez des avis au bon moment (après une action réussie)
- Ne harcelez pas les utilisateurs
- Utilisez l'API native de demande d'avis

```pascal
uses
  FMX.Platform;

// Demander un avis avec l'API native
procedure TFormMain.DemanderAvis;  
var  
  RatingService: IFMXRatingService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXRatingService, RatingService) then
  begin
    RatingService.RequestReview;
  end;
end;

// Appeler au bon moment
procedure TFormMain.TacheTermineeAvecSucces;  
begin  
  // Incrémenter un compteur
  Inc(FNombreSucces);

  // Demander un avis après 5 succès
  if FNombreSucces = 5 then
    DemanderAvis;
end;
```

- Répondez à tous les avis (positifs et négatifs)
- Corrigez les problèmes mentionnés dans les avis négatifs

### Stratégies marketing

**Avant le lancement** :
- Créez une page de destination (landing page)
- Constituez une liste d'emails de personnes intéressées
- Créez du contenu de blog/vidéo sur le sujet
- Contactez des blogueurs et influenceurs

**Au lancement** :
- Annoncez sur les réseaux sociaux
- Envoyez des communiqués de presse
- Offrez une promotion de lancement
- Demandez à vos premiers utilisateurs de laisser des avis

**Après le lancement** :
- Créez du contenu régulier (blog, vidéos)
- Restez actif sur les réseaux sociaux
- Mettez à jour fréquemment l'application
- Écoutez les retours utilisateurs
- Analysez les métriques (téléchargements, rétention, etc.)

## Monétisation

### Modèles de monétisation

**Application gratuite** :
- Acquisition facile d'utilisateurs
- Monétisation via publicité ou achats in-app

**Application payante** :
- Revenus dès le téléchargement
- Plus difficile à promouvoir
- Moins de téléchargements

**Freemium** :
- Version gratuite avec fonctionnalités limitées
- Débloquage via achat unique ou abonnement
- Bon compromis pour tester l'application

**Abonnement** :
- Revenus récurrents
- Convient aux services continus
- Nécessite de la valeur régulière

### Publicité avec AdMob

```pascal
// Exemple d'intégration publicitaire (conceptuel)
uses
  FMX.Advertising;

procedure TFormMain.AfficherBanniere;  
begin  
  BannerAd1.AdUnitID := 'ca-app-pub-XXXXXXXXX/YYYYYYYYY';
  BannerAd1.LoadAd;
  BannerAd1.Visible := True;
end;

procedure TFormMain.AfficherInterstitiel;  
begin  
  if InterstitialAd1.IsLoaded then
  begin
    InterstitialAd1.Show;
  end;
end;
```

### Achats in-app

Les achats in-app permettent de vendre du contenu ou des fonctionnalités directement dans l'application.

**Configuration Google Play** :
1. Play Console > Votre app > Produits in-app
2. Créez un nouveau produit
3. Définissez l'ID du produit (ex: premium_upgrade)
4. Fixez le prix

**Configuration App Store** :
1. App Store Connect > Fonctionnalités > Achats intégrés
2. Créez un nouveau produit
3. Remplissez les informations
4. Définissez les prix par région

## Analyse et amélioration continue

### Firebase Analytics

Intégrez Firebase pour comprendre le comportement des utilisateurs :

- Nombre d'utilisateurs actifs
- Durée des sessions
- Écrans les plus consultés
- Taux de rétention
- Conversions (achats, inscriptions)

### Suivi des crashs

Utilisez Firebase Crashlytics ou un service similaire pour :
- Détecter les crashs en production
- Prioriser les corrections
- Suivre la stabilité de l'application

### Itération et amélioration

1. **Analysez les données** : Métriques, avis, crashs
2. **Identifiez les problèmes** : Bugs, points de friction
3. **Priorisez** : Impact vs effort
4. **Développez** : Nouvelles fonctionnalités, corrections
5. **Testez** : Assurez-vous que tout fonctionne
6. **Publiez** : Nouvelle version
7. **Répétez** : C'est un cycle continu

## Conclusion

La publication d'une application mobile sur les stores officiels est une étape exigeante mais gratifiante. En suivant méthodiquement les étapes présentées dans cette section, vous maximisez vos chances de succès.

**Points clés à retenir** :

1. **Préparation** : Peaufinez votre application et créez des visuels de qualité
2. **Google Play** : Processus relativement simple et rapide
3. **App Store** : Plus strict mais garantit une certaine qualité
4. **Mises à jour** : Publiez régulièrement des améliorations
5. **Marketing** : Une bonne application ne suffit pas, faites-la connaître
6. **Écoute** : Les retours utilisateurs sont précieux
7. **Patience** : Le succès prend du temps, persévérez

N'oubliez pas que la publication n'est pas une fin en soi, mais le début d'une aventure. Votre application évoluera avec les besoins de vos utilisateurs et les nouvelles possibilités technologiques. Restez à l'écoute, soyez réactif, et améliorez continuellement votre application.

Félicitations pour avoir franchi cette étape importante dans votre parcours de développeur d'applications mobiles avec Delphi !

⏭️ [Mises à jour OTA (Over The Air)](/15-applications-mobiles-avec-delphi/08-mises-a-jour-ota.md)
