# 15.7 Publication sur App Store / Play Store

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Après avoir développé votre application mobile avec Delphi, l'étape finale consiste à la distribuer aux utilisateurs via les magasins d'applications officiels : l'App Store d'Apple pour iOS et le Google Play Store pour Android. Ce processus implique plusieurs étapes allant de la préparation de l'application à sa soumission et sa maintenance après publication.

## Préparation de votre application à la publication

Avant de soumettre votre application aux stores, vous devez vous assurer qu'elle est prête pour une utilisation publique.

### 1. Finalisez et testez votre application

Vérifiez les points suivants :

- Toutes les fonctionnalités sont complètes et fonctionnent comme prévu
- L'application est exempte de bugs majeurs
- Les performances sont optimisées (temps de chargement, utilisation de la mémoire)
- L'interface utilisateur est cohérente et responsive sur différents appareils
- Les contrôles de saisie et la validation des données fonctionnent correctement

**Conseil :** Testez votre application sur plusieurs appareils physiques, pas seulement sur des émulateurs, pour vous assurer qu'elle fonctionne correctement dans des conditions réelles.

### 2. Préparez les ressources graphiques

Les deux stores nécessitent diverses ressources graphiques :

#### Icône de l'application

Dans Delphi, vous pouvez configurer l'icône de votre application via le Project Manager :

1. Ouvrez le gestionnaire de projet
2. Développez le dossier correspondant à votre plateforme cible (Android ou iOS)
3. Cliquez sur "Application"
4. Dans l'onglet "Appearance", configurez l'icône

Pour Android, vous aurez besoin de plusieurs tailles d'icônes :
- 48x48 pixels (mdpi)
- 72x72 pixels (hdpi)
- 96x96 pixels (xhdpi)
- 144x144 pixels (xxhdpi)
- 192x192 pixels (xxxhdpi)

Pour iOS, vous aurez également besoin de multiples tailles d'icônes, notamment :
- 60x60 pixels (iPhone)
- 76x76 pixels (iPad)
- 83.5x83.5 pixels (iPad Pro)
- 1024x1024 pixels (App Store)

#### Captures d'écran

Préparez des captures d'écran de haute qualité pour différents appareils :

**Pour Android :**
- Téléphone (par exemple : 1080x1920 pixels)
- Tablette 7" (par exemple : 1280x800 pixels)
- Tablette 10" (par exemple : 1920x1200 pixels)

**Pour iOS :**
- iPhone (6.5", 5.5" et 4.7")
- iPad (12.9" et 9.7")

#### Vidéo promotionnelle (facultatif mais recommandé)

Une courte vidéo présentant les principales fonctionnalités de votre application peut augmenter considérablement les taux d'installation.

### 3. Compilez en mode Release

Pour préparer votre application à la publication, vous devez la compiler en mode Release plutôt qu'en mode Debug :

1. Dans Delphi, allez dans Project > Options
2. Sélectionnez "Building" dans la liste de gauche
3. Changez la configuration de "Debug" à "Release"
4. Vérifiez les options d'optimisation et activez-les pour de meilleures performances

### 4. Préparez les métadonnées de l'application

Les deux stores vous demanderont diverses informations sur votre application :

- **Nom de l'application** : Le nom affiché dans les stores (max. 30 caractères pour l'App Store, 50 pour le Play Store)
- **Description** : Une description courte et une description complète de votre application
- **Mots-clés** : Pour améliorer la découvrabilité de votre application
- **Catégorie** : Choisissez la catégorie qui correspond le mieux à votre application
- **Classification d'âge** : Indiquez le public cible de votre application
- **Politique de confidentialité** : URL vers votre politique de confidentialité (obligatoire)
- **Coordonnées de support** : Email et/ou site web pour le support client

## Publication sur Google Play Store (Android)

### 1. Créez un compte développeur Google Play

Avant de pouvoir publier, vous devez créer un compte développeur Google Play :

1. Visitez la [Console Google Play Developer](https://play.google.com/console/signup)
2. Connectez-vous avec un compte Google
3. Payez les frais d'inscription (25 $ USD, paiement unique)
4. Remplissez les informations requises sur votre profil développeur

### 2. Préparez votre APK ou App Bundle

Delphi permet de générer deux types de packages pour Android :

- **APK (Android Package)** : Format traditionnel
- **AAB (Android App Bundle)** : Format recommandé qui optimise la taille de l'application pour chaque appareil

Voici comment générer un App Bundle avec Delphi 11 ou supérieur :

```pascal
// Dans le menu principal de Delphi
// Project > Deployment
// Configurer les fichiers à inclure dans le package

// Ensuite, pour créer un AAB
// Project > Build App Bundle
```

Pour les versions antérieures de Delphi qui ne supportent pas directement l'AAB, vous pouvez générer un APK standard :

```pascal
// Project > Deploy
```

### 3. Signez votre application

Toutes les applications Android doivent être signées numériquement avant publication :

1. Dans Delphi, allez dans Project > Options > Building > Android
2. Dans la section "Application certificate", choisissez une des options :
   - Utiliser la clé de débogage (non recommandé pour la production)
   - Créer une nouvelle clé
   - Utiliser une clé existante (recommandé si vous avez déjà publié des versions)

Si vous créez une nouvelle clé :

```
Alias : nom-de-votre-application
Password : choisissez un mot de passe fort
Validity : 25+ ans recommandé
First and Last Name : Votre nom ou celui de votre entreprise
Organizational Unit : Département (optionnel)
Organization : Nom de votre entreprise
Locality : Votre ville
State : Votre état/province
Country Code : Code pays à deux lettres (ex: FR pour France)
```

**Important :** Conservez précieusement le fichier keystore et son mot de passe. Si vous les perdez, vous ne pourrez plus mettre à jour votre application !

### 4. Créez une nouvelle application dans la Google Play Console

1. Connectez-vous à la [Google Play Console](https://play.google.com/console)
2. Cliquez sur "Créer une application"
3. Sélectionnez la langue par défaut
4. Entrez le nom de votre application
5. Spécifiez si c'est une application ou un jeu
6. Indiquez si elle est gratuite ou payante
7. Cliquez sur "Créer"

### 5. Configurez la fiche Google Play

Après avoir créé l'application, vous devez compléter plusieurs sections :

1. **Fiche du Play Store**
   - Ajoutez les descriptions (courte et complète)
   - Téléchargez les captures d'écran et vidéos
   - Ajoutez l'icône au format correct
   - Choisissez la catégorie et les tags

2. **Classification du contenu**
   - Remplissez le questionnaire de classification

3. **Tarifs et disponibilité**
   - Indiquez si l'application est gratuite ou payante
   - Sélectionnez les pays où l'application sera disponible

4. **Configuration de l'application**
   - Ajoutez vos informations de contact
   - Configurez les liens externes (politique de confidentialité, etc.)

### 6. Téléchargez votre APK ou App Bundle

1. Dans la section "Production" du menu, cliquez sur "Créer une nouvelle version"
2. Téléchargez votre fichier APK ou AAB
3. Ajoutez les notes de version (ce qui est nouveau dans cette version)
4. Enregistrez et passez à la vérification

### 7. Vérification et publication

Une fois toutes les sections complétées :

1. Vérifiez votre application avec les outils de la Play Console
2. Corrigez les problèmes signalés
3. Soumettez pour examen

Google examinera votre application, ce qui peut prendre quelques heures à quelques jours. Une fois approuvée, votre application sera publiée sur le Play Store.

## Publication sur Apple App Store (iOS)

La publication sur l'App Store est généralement plus complexe et stricte que sur le Play Store.

### 1. Inscrivez-vous au programme développeur Apple

1. Visitez le [site du programme développeur Apple](https://developer.apple.com/programs/)
2. Cliquez sur "Enroll"
3. Connectez-vous avec votre identifiant Apple
4. Suivez les étapes et payez les frais d'adhésion (99 $ USD par an)

### 2. Configurez les certificats et les profils de provisionnement

Pour compiler et soumettre des applications iOS, vous avez besoin de :

1. **Certificat de développement** : Pour tester sur des appareils
2. **Certificat de distribution** : Pour soumettre à l'App Store
3. **Identifiant d'application** : Pour identifier votre application
4. **Profil de provisionnement** : Qui lie le certificat à l'identifiant d'application

Voici comment procéder :

1. Allez sur le [portail développeur Apple](https://developer.apple.com/account)
2. Dans la section "Certificates, IDs & Profiles" :
   - Créez un certificat de distribution
   - Enregistrez un identifiant d'application
   - Créez un profil de provisionnement de type "App Store"

3. Téléchargez et installez ces éléments sur votre Mac

### 3. Configurez votre application dans Delphi

1. Dans Delphi, ouvrez Project > Options > Building > iOS Device
2. Dans la section "Provisioning", sélectionnez votre profil de provisionnement
3. Configurez le Bundle Identifier (doit correspondre à celui créé sur le portail Apple)
4. Configurez la version et le numéro de build

### 4. Compilez pour iOS Device

Pour générer un package iOS :

1. Connectez votre Mac à votre PC Windows (nécessaire pour la compilation iOS)
2. Configurez la connexion à votre Mac dans Tools > Options > SDK Manager > macOS SDK
3. Sélectionnez "iOS Device" comme cible de déploiement
4. Choisissez Project > Build pour compiler

### 5. Créez et configurez votre application dans App Store Connect

1. Connectez-vous à [App Store Connect](https://appstoreconnect.apple.com)
2. Cliquez sur "Mon appli" puis "+"
3. Remplissez les informations de base :
   - Nom de l'application
   - Language par défaut
   - Bundle ID (sélectionnez celui que vous avez enregistré)
   - SKU (identifiant unique pour votre référence)
4. Cliquez sur "Créer"

### 6. Configurez la fiche App Store

Après avoir créé l'application, complétez les différentes sections :

1. **Informations sur l'application**
   - Version (doit correspondre à celle dans Delphi)
   - Informations de contact
   - Classification d'âge

2. **Tarifs et disponibilité**
   - Prix
   - Pays de disponibilité

3. **Métadonnées de l'App Store**
   - Description
   - Mots-clés
   - URL du support
   - URL marketing
   - Screenshots

### 7. Téléchargez votre application

Pour soumettre votre application compilée à Apple, vous pouvez utiliser deux méthodes :

#### Méthode 1 : Via Xcode (recommandée)

1. Sur votre Mac, ouvrez Xcode
2. Utilisez Window > Organizer
3. Localisez votre application compilée
4. Cliquez sur "Distribute App"
5. Sélectionnez "App Store Connect"
6. Suivez les étapes du processus

#### Méthode 2 : Via Application Loader

1. Dans Delphi, générez un fichier IPA (iOS App Store Package)
2. Transférez ce fichier sur votre Mac
3. Utilisez l'Application Loader pour soumettre le package

### 8. Soumettez pour examen

Une fois votre application téléchargée :

1. Retournez dans App Store Connect
2. Dans la section "Test Flight", vous pouvez ajouter des testeurs avant la publication
3. Dans l'onglet "App Store", cliquez sur "Soumettre pour examen"

Le processus d'examen d'Apple est généralement plus long et plus rigoureux que celui de Google, pouvant prendre de quelques jours à une semaine. Préparez-vous à d'éventuels rejets et à la nécessité d'effectuer des modifications.

## Bonnes pratiques pour la soumission

### 1. Tests rigoureux

Testez exhaustivement votre application sur différents appareils avant de la soumettre pour minimiser les risques de rejet.

### 2. Respectez les directives des plateformes

Chaque plateforme a ses propres règles et directives de conception :

- [Directives de l'App Store](https://developer.apple.com/app-store/review/guidelines/)
- [Règles de qualité du Play Store](https://developer.android.com/docs/quality-guidelines)

Familiarisez-vous avec ces documents pour éviter les rejets.

### 3. Préparez une version incrémentielle

Utilisez un système de versionnage cohérent :

- **Version majeure.mineure.correctif** (ex : 1.2.3)
- Incrémentez le numéro de build à chaque soumission

Exemple de configuration dans Delphi :

```pascal
// Dans Project > Options > Version Info
// Pour Android
Major : 1
Minor : 0
Release : 0
Build : 1  // Incrémentez à chaque nouvelle version

// Pour iOS, configurez également :
CFBundleVersion : 1.0.0  // La version visible
Build : 1  // Le numéro de build, à incrémenter à chaque soumission
```

### 4. Utilisez des métadonnées optimisées pour l'ASO

L'App Store Optimization (ASO) est l'équivalent du SEO pour les apps :

- Utilisez des mots-clés pertinents dans le titre et la description
- Écrivez une description claire qui explique les avantages de votre application
- Créez des visuels attrayants qui montrent clairement les fonctionnalités

### 5. Planifiez les mises à jour

Après publication, prévoyez un calendrier de mises à jour pour :

- Corriger les bugs signalés
- Ajouter de nouvelles fonctionnalités
- Maintenir la conformité avec les nouvelles versions des OS

## Checklist avant soumission

Pour éviter les erreurs courantes, utilisez cette checklist :

### Pour les deux plateformes

- [ ] Application testée sur plusieurs appareils réels
- [ ] Toutes les fonctionnalités fonctionnent correctement
- [ ] Performances optimisées (compilation en mode Release)
- [ ] Contenu conforme aux directives des stores
- [ ] Politique de confidentialité créée et accessible
- [ ] Métadonnées complètes et optimisées
- [ ] Captures d'écran et icônes préparées aux formats requis
- [ ] Version et numéro de build configurés correctement

### Spécifique à Android

- [ ] APK ou AAB signé avec une clé de production (pas la clé de débogage)
- [ ] Fichier keystore sauvegardé en lieu sûr
- [ ] Permissions justifiées et réduites au minimum nécessaire
- [ ] Compatibilité vérifiée avec différentes versions d'Android

### Spécifique à iOS

- [ ] Certificats et profils de provisionnement correctement configurés
- [ ] Bundle Identifier correspond à celui enregistré sur le portail Apple
- [ ] App fonctionne correctement en mode multitâche
- [ ] Supports toutes les orientations requises
- [ ] Respecte les directives d'interface d'iOS

## Gestion des révisions et mises à jour

Une fois votre application publiée, vous devrez la maintenir à jour :

### Processus de mise à jour pour Android

1. Incrémentez le numéro de version et/ou de build
2. Compilez une nouvelle version
3. Signez avec la **même clé** que la version précédente
4. Créez une nouvelle version dans la Google Play Console
5. Téléchargez le nouveau APK ou AAB
6. Ajoutez les notes de mise à jour
7. Publiez la mise à jour

### Processus de mise à jour pour iOS

1. Incrémentez le numéro de version et/ou de build
2. Compilez une nouvelle version
3. Créez une nouvelle version dans App Store Connect
4. Téléchargez le nouveau package
5. Mettez à jour les métadonnées si nécessaire
6. Ajoutez les notes de version
7. Soumettez pour examen

### Déploiement progressif (Android)

Pour Android, vous pouvez utiliser le déploiement progressif :

1. Dans la Google Play Console, lors de la création d'une nouvelle version
2. Sous "Déploiement", choisissez "Déploiement progressif"
3. Définissez un pourcentage d'utilisateurs pour recevoir la mise à jour
4. Augmentez progressivement ce pourcentage si aucun problème n'est signalé

### Utilisation de TestFlight (iOS)

Pour iOS, utilisez TestFlight pour tester les mises à jour avant publication complète :

1. Téléchargez votre nouvelle version dans App Store Connect
2. Configurez-la dans TestFlight
3. Invitez des testeurs internes et externes
4. Collectez les retours
5. Soumettez pour publication complète quand vous êtes satisfait

## Monétisation de votre application

Plusieurs modèles de monétisation sont disponibles :

### 1. Application payante

L'utilisateur paie une fois pour télécharger votre application.

**Configuration dans Delphi :**
- Aucune configuration particulière n'est nécessaire dans le code
- Configurez simplement le prix dans les consoles de développement

### 2. Achats intégrés (In-App Purchases)

Permettez aux utilisateurs d'acheter des fonctionnalités ou du contenu supplémentaire.

**Pour implémenter les achats intégrés :**

1. Créez les produits dans les consoles des stores
2. Utilisez le composant `TPurchaseManager` de Delphi :

```pascal
uses
  FMX.InAppPurchase;

procedure TMainForm.InitializePurchases;
var
  PurchManager: TPurchaseManager;
begin
  // Créer le gestionnaire d'achats
  PurchManager := TPurchaseManager.DefaultManager;

  // Définir les gestionnaires d'événements
  PurchManager.OnProductsRequestResponse := HandleProductsResponse;
  PurchManager.OnPurchaseCompleted := HandlePurchaseCompleted;

  // Récupérer les produits disponibles
  PurchManager.QueryProducts(['votre.produit.id1', 'votre.produit.id2']);
end;

procedure TMainForm.HandleProductsResponse(Sender: TObject;
  const Products: TIAPProductList; const InvalidProductIDs: TStrings);
begin
  // Traiter la liste des produits disponibles
  for var Product in Products do
  begin
    // Ajouter le produit à l'interface utilisateur
    AddProductToUI(Product.ProductID, Product.Title, Product.Price);
  end;
end;

procedure TMainForm.BuyProduct(const ProductID: string);
begin
  // Lancer l'achat
  TPurchaseManager.DefaultManager.PurchaseProduct(ProductID);
end;

procedure TMainForm.HandlePurchaseCompleted(Sender: TObject;
  const ProductID: string; const Success: Boolean);
begin
  if Success then
  begin
    // Achat réussi, débloquer la fonctionnalité
    UnlockFeature(ProductID);
  end
  else
    ShowMessage('L''achat a échoué. Veuillez réessayer.');
end;
```

### 3. Abonnements

Similaires aux achats intégrés, mais avec renouvellement automatique.

### 4. Publicités intégrées

Affichez des annonces dans votre application via des réseaux publicitaires.

Pour intégrer des publicités, vous pouvez utiliser des composants tiers comme AdMob à travers l'API FireMonkey.

## Conclusion

La publication de votre application sur l'App Store et le Play Store représente l'aboutissement de votre travail de développement, mais c'est aussi le début d'un nouveau processus : la maintenance et l'amélioration continue de votre application.

En suivant les étapes détaillées dans ce chapitre, vous pouvez naviguer avec succès dans le processus parfois complexe de soumission d'applications. N'oubliez pas que chaque plateforme a ses propres exigences et que celles-ci peuvent évoluer avec le temps. Consultez régulièrement la documentation officielle pour rester à jour.

Une fois votre application publiée, écoutez attentivement les commentaires des utilisateurs et utilisez ces retours pour orienter vos futures mises à jour. Une application bien entretenue et régulièrement mise à jour aura plus de chances de réussir à long terme.

Dans la prochaine section, nous explorerons comment mettre en place des mises à jour automatiques pour votre application afin de faciliter la distribution de nouvelles versions à vos utilisateurs existants.

⏭️ [Mises à jour OTA (Over The Air)](/15-applications-mobiles-avec-delphi/08-mises-a-jour-ota.md)
