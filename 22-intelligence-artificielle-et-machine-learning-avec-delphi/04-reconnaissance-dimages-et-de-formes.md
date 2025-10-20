🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.4 Reconnaissance d'images et de formes

## Introduction à la vision par ordinateur

### Qu'est-ce que la reconnaissance d'images ?

La reconnaissance d'images est la capacité d'un ordinateur à identifier et interpréter le contenu visuel d'une image, tout comme le ferait un être humain. C'est une branche de l'intelligence artificielle appelée **vision par ordinateur** (Computer Vision).

**Analogie simple** : Imaginez que vous montrez une photo de votre chat à un ami. Instantanément, il reconnaît qu'il s'agit d'un chat, sa couleur, sa position. La reconnaissance d'images permet à votre application Delphi de faire la même chose.

### Pourquoi c'est révolutionnaire ?

Avant l'IA moderne, faire reconnaître un simple objet par un ordinateur nécessitait des règles complexes programmées manuellement. Aujourd'hui, grâce au deep learning, les ordinateurs peuvent apprendre à "voir" en étudiant des millions d'images, comme un enfant apprend en observant le monde.

### L'évolution de la vision par ordinateur

**Années 1960-2000** : Méthodes manuelles
- Détection de contours, de couleurs
- Règles rigides et limitées
- Fonctionne mal dans des conditions variables

**Années 2010-aujourd'hui** : Deep Learning
- Apprentissage automatique à partir d'exemples
- Réseaux de neurones convolutifs (CNN)
- Performances dépassant parfois l'humain

**2024 et au-delà** : Modèles multimodaux
- Combinaison vision + texte (CLIP, GPT-4 Vision)
- Compréhension contextuelle avancée
- Génération d'images (DALL-E, Stable Diffusion)

## Tâches fondamentales de reconnaissance d'images

### 1. Classification d'images

**Qu'est-ce que c'est ?** : Déterminer la catégorie principale d'une image entière.

**Exemple** :
```
Image → Analyse → Résultat: "Chat (95% de confiance)"
```

**Applications pratiques** :
- Tri automatique de photos
- Contrôle qualité en industrie
- Détection de produits défectueux
- Classification de documents scannés
- Modération de contenu

**Cas d'usage Delphi** :
Une application de gestion de stock qui identifie automatiquement les produits photographiés par les employés.

### 2. Détection d'objets

**Qu'est-ce que c'est ?** : Localiser et identifier plusieurs objets dans une image avec des boîtes englobantes.

**Exemple** :
```
Image de rue →
  [Voiture: x=100, y=150, largeur=200, hauteur=150, confiance=0.92]
  [Piéton: x=350, y=180, largeur=80, hauteur=180, confiance=0.87]
  [Feu rouge: x=450, y=50, largeur=40, hauteur=80, confiance=0.95]
```

**Applications pratiques** :
- Surveillance vidéo intelligente
- Comptage d'objets ou de personnes
- Véhicules autonomes
- Réalité augmentée
- Analyse de scènes complexes

**Algorithmes populaires** :
- YOLO (You Only Look Once) - Rapide, temps réel
- SSD (Single Shot Detector)
- Faster R-CNN - Plus précis mais plus lent

### 3. Segmentation d'images

**Qu'est-ce que c'est ?** : Identifier chaque pixel d'une image et l'attribuer à une classe.

**Types** :

**Segmentation sémantique** : Classe chaque pixel (tous les "chats" sont une seule catégorie)

**Segmentation d'instance** : Distingue les instances individuelles (chat n°1, chat n°2)

**Applications pratiques** :
- Imagerie médicale (tumeurs, organes)
- Retouche photo avancée (changement d'arrière-plan)
- Agriculture de précision
- Cartographie satellite
- Conduite autonome

### 4. Reconnaissance faciale

**Qu'est-ce que c'est ?** : Identifier ou vérifier l'identité d'une personne à partir de son visage.

**Étapes** :
1. **Détection** : Localiser les visages dans l'image
2. **Alignement** : Normaliser l'orientation et la taille
3. **Extraction** : Créer une "signature numérique" du visage
4. **Comparaison** : Comparer avec une base de données

**Applications pratiques** :
- Systèmes de contrôle d'accès
- Pointage automatique des employés
- Déverrouillage d'applications
- Recherche de personnes disparues
- Organisation de photos personnelles

**Considérations éthiques** :
- Respect de la vie privée (RGPD)
- Consentement explicite nécessaire
- Sécurisation des données biométriques
- Transparence sur l'utilisation

### 5. OCR (Reconnaissance optique de caractères)

**Qu'est-ce que c'est ?** : Extraire le texte présent dans une image.

**Exemple** :
```
Image d'une facture → "FACTURE N° 2024-001
                       Montant TTC : 1 250,00 €
                       Date : 15/10/2024"
```

**Applications pratiques** :
- Numérisation de documents
- Lecture de plaques d'immatriculation
- Extraction de données de formulaires
- Traduction de panneaux en temps réel
- Accessibilité (lecture pour malvoyants)

**Technologies** :
- Tesseract OCR (open source, excellent)
- Google Cloud Vision API
- Azure Computer Vision OCR
- AWS Textract (spécialisé documents complexes)

### 6. Analyse de scènes

**Qu'est-ce que c'est ?** : Comprendre le contexte global d'une image : lieu, activité, ambiance.

**Exemple** :
```
Image → "Scène: plage au coucher du soleil
         Activités: personnes se promenant
         Ambiance: paisible, romantique
         Météo: ciel dégagé"
```

**Applications pratiques** :
- Organisation intelligente de photos
- Recherche d'images par description
- Analyse de contenu pour réseaux sociaux
- Recommandations de destinations touristiques

### 7. Détection d'anomalies visuelles

**Qu'est-ce que c'est ?** : Identifier des éléments inhabituels ou défectueux dans des images.

**Applications pratiques** :
- Contrôle qualité industriel
- Détection de fraudes (documents falsifiés)
- Surveillance de sécurité
- Maintenance prédictive (détection d'usure)
- Imagerie médicale (détection précoce)

### 8. Estimation de pose

**Qu'est-ce que c'est ?** : Détecter la position et l'orientation du corps humain ou d'objets.

**Applications pratiques** :
- Applications de fitness et sport
- Animation et motion capture
- Analyse ergonomique au travail
- Jeux vidéo et réalité virtuelle
- Rééducation médicale

## Bibliothèques et outils pour la reconnaissance d'images

### OpenCV (Open Source Computer Vision)

**Présentation** :
OpenCV est LA bibliothèque de référence pour la vision par ordinateur, créée en 2000 et maintenue par Intel.

**Capacités** :
- Traitement d'images (filtres, transformations)
- Détection de contours, de formes
- Reconnaissance de visages
- Détection d'objets
- Vidéo et caméra en temps réel
- Plus de 2500 algorithmes optimisés

**Intégration avec Delphi** :

**Option 1 : Delphi-OpenCV**
- Binding Delphi communautaire
- Accès direct aux fonctions OpenCV
- Nécessite compilation de DLL

**Option 2 : Appels directs à la DLL**
- Plus de contrôle mais plus complexe
- Gestion manuelle des types de données

**Avantages** :
- Gratuit et open source
- Très performant
- Fonctionne hors ligne
- Communauté massive

**Inconvénients** :
- Courbe d'apprentissage importante
- Configuration initiale complexe
- Documentation parfois technique

### TensorFlow / TensorFlow Lite

**Pour la reconnaissance d'images** :
TensorFlow excelle dans le deep learning pour la vision par ordinateur.

**Modèles pré-entraînés disponibles** :
- **MobileNet** : Classification d'images, léger
- **ResNet** : Classification haute précision
- **YOLO** : Détection d'objets temps réel
- **Mask R-CNN** : Segmentation d'instances

**Intégration avec Delphi** :
- Via ONNX Runtime (recommandé)
- Via Python4Delphi
- Via API REST locale

**TensorFlow Lite** :
Version optimisée pour mobiles, parfaite pour applications FireMonkey iOS/Android.

### PyTorch

**Présentation** :
Concurrent principal de TensorFlow, très populaire en recherche.

**Avantages** :
- Interface plus intuitive
- Excellente communauté
- Nombreux modèles sur Hugging Face

**Intégration avec Delphi** :
- Conversion en ONNX puis utilisation avec ONNX Runtime
- Via Python4Delphi
- API REST

### ONNX Runtime pour la vision

**Pourquoi ONNX est idéal** :
- Format universel pour modèles de vision
- Optimisé pour l'inférence rapide
- Facilement intégrable en Delphi
- Supporte GPU pour accélération

**Modèles ONNX disponibles** :
- ONNX Model Zoo : collection officielle
- Conversion depuis TensorFlow/PyTorch
- Modèles pré-optimisés pour production

## API Cloud de reconnaissance d'images

### Google Cloud Vision API

**Capacités** :
- Détection d'étiquettes (labels)
- Détection de visages et émotions
- OCR multilingue
- Détection de contenu inapproprié
- Détection de monuments et logos
- Propriétés d'image (couleurs dominantes)

**Tarification** :
- 1000 premières requêtes/mois gratuites
- Ensuite : ~1,50€ / 1000 images

**Intégration Delphi** :
REST API simple avec TRESTClient

```pascal
// Exemple conceptuel
procedure AnalyserImageGoogle(const CheminImage: string);
var
  ImageBase64: string;
begin
  // 1. Charger et encoder l'image en Base64
  ImageBase64 := EncoderImageBase64(CheminImage);

  // 2. Configurer la requête
  RESTClient.BaseURL := 'https://vision.googleapis.com/v1/images:annotate';
  RESTRequest.AddParameter('key', 'VOTRE_CLE_API', pkGETorPOST);

  // 3. Construire le JSON de requête
  RESTRequest.Body.Add(Format(
    '{"requests":[{"image":{"content":"%s"},"features":[{"type":"LABEL_DETECTION"}]}]}',
    [ImageBase64]
  ));

  // 4. Exécuter en arrière-plan
  TTask.Run(procedure
  begin
    RESTRequest.Execute;
    // Traiter les résultats...
  end);
end;
```

### Azure Computer Vision

**Capacités** :
- Analyse d'images (objets, couleurs, etc.)
- OCR avancé (Read API)
- Détection de visages
- Génération de miniatures intelligentes
- Modération de contenu
- Analyse spatiale

**Avantages** :
- Intégration facile avec écosystème Microsoft
- OCR excellent pour documents complexes
- Modèles personnalisables

### AWS Rekognition

**Capacités** :
- Détection et reconnaissance faciale
- Comparaison de visages
- Détection de célébrités
- Détection de contenu inapproprié
- Détection de texte
- Analyse vidéo

**Cas d'usage fort** :
Applications nécessitant reconnaissance faciale à grande échelle.

### OpenAI Vision (GPT-4 Vision)

**Révolutionnaire** :
GPT-4 Vision peut non seulement identifier des objets, mais aussi comprendre et expliquer des images complexes.

**Exemples** :
```
Image de diagramme technique →
"Ce diagramme représente une architecture client-serveur
avec trois couches : présentation, logique métier et
base de données..."
```

**Cas d'usage** :
- Analyse de graphiques et tableaux
- Compréhension de documents visuels
- Assistance visuelle pour malvoyants
- Génération de descriptions détaillées

## Intégration pratique dans les applications Delphi

### Architecture recommandée

**Pattern de conception** :

```
┌───────────────────────────────────┐
│     Interface Utilisateur         │
│  (Sélection/Affichage d'images)   │
└────────────┬──────────────────────┘
             │
┌────────────▼──────────────────────┐
│   Gestionnaire d'images Delphi    │
│  - Chargement TImage/TBitmap      │
│  - Prétraitement (redim, format)  │
│  - Cache et gestion mémoire       │
└────────────┬──────────────────────┘
             │
┌────────────▼──────────────────────┐
│   Couche d'analyse IA             │
│  - Appels API ou bibliothèques    │
│  - Gestion asynchrone             │
│  - Traitement des résultats       │
└────────────┬──────────────────────┘
             │
┌────────────▼──────────────────────┐
│   Service de reconnaissance       │
│  (Cloud API ou modèle local)      │
└───────────────────────────────────┘
```

### Prétraitement des images

**Pourquoi prétraiter ?**
- Réduire la taille pour économiser bande passante/mémoire
- Normaliser le format (certaines API acceptent seulement JPEG/PNG)
- Améliorer la qualité de reconnaissance

**Opérations courantes** :

```pascal
// Redimensionner une image
procedure RedimensionnerImage(Bitmap: TBitmap; MaxWidth, MaxHeight: Integer);
var
  Ratio: Double;
  NewWidth, NewHeight: Integer;
begin
  Ratio := Min(MaxWidth / Bitmap.Width, MaxHeight / Bitmap.Height);
  if Ratio < 1 then
  begin
    NewWidth := Round(Bitmap.Width * Ratio);
    NewHeight := Round(Bitmap.Height * Ratio);
    Bitmap.SetSize(NewWidth, NewHeight);
  end;
end;
```

**Conversion de format** :
```pascal
// Convertir TBitmap en JPEG pour envoi API
procedure ConvertirEnJPEG(Bitmap: TBitmap; Stream: TMemoryStream);
var
  JPEG: TJPEGImage;
begin
  JPEG := TJPEGImage.Create;
  try
    JPEG.Assign(Bitmap);
    JPEG.CompressionQuality := 85; // Bon compromis qualité/taille
    JPEG.SaveToStream(Stream);
  finally
    JPEG.Free;
  end;
end;
```

### Encodage Base64 pour les API

La plupart des API REST acceptent les images encodées en Base64.

```pascal
function EncoderImageBase64(const CheminFichier: string): string;
var
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  FileStream := TFileStream.Create(CheminFichier, fmOpenRead);
  try
    MemStream.CopyFrom(FileStream, FileStream.Size);
    MemStream.Position := 0;
    Result := TNetEncoding.Base64.EncodeBytesToString(
      MemStream.Memory, MemStream.Size);
  finally
    FileStream.Free;
    MemStream.Free;
  end;
end;
```

### Gestion asynchrone et performance

**Règle d'or** : Ne bloquez JAMAIS l'interface utilisateur pendant l'analyse d'image.

```pascal
procedure AnalyserImageAsync(const CheminImage: string);
begin
  // Afficher un indicateur de chargement
  ProgressBar.Visible := True;
  BtnAnalyser.Enabled := False;

  TTask.Run(procedure
  var
    Resultats: string;
  begin
    try
      // Analyse de l'image (peut prendre plusieurs secondes)
      Resultats := EffectuerReconnaissanceImage(CheminImage);

      // Retour sur le thread principal
      TThread.Synchronize(nil, procedure
      begin
        // Afficher les résultats
        MemoResultats.Text := Resultats;
        ProgressBar.Visible := False;
        BtnAnalyser.Enabled := True;
      end);
    except
      on E: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          ShowMessage('Erreur: ' + E.Message);
          ProgressBar.Visible := False;
          BtnAnalyser.Enabled := True;
        end);
      end;
    end;
  end);
end;
```

### Affichage des résultats de détection

Pour la détection d'objets, vous devez dessiner des rectangles sur l'image.

```pascal
// Dessiner les boîtes englobantes
procedure DessinerDetections(Image: TImage; Detections: TArray<TDetection>);
var
  Detection: TDetection;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(Image.Picture.Bitmap);

    // Dessiner chaque détection
    for Detection in Detections do
    begin
      Bitmap.Canvas.Pen.Color := clRed;
      Bitmap.Canvas.Pen.Width := 3;
      Bitmap.Canvas.Brush.Style := bsClear;

      // Rectangle
      Bitmap.Canvas.Rectangle(
        Detection.X,
        Detection.Y,
        Detection.X + Detection.Width,
        Detection.Y + Detection.Height
      );

      // Label
      Bitmap.Canvas.Font.Color := clRed;
      Bitmap.Canvas.Font.Size := 12;
      Bitmap.Canvas.TextOut(
        Detection.X,
        Detection.Y - 20,
        Format('%s (%.0f%%)', [Detection.Label, Detection.Confidence * 100])
      );
    end;

    Image.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
```

## Cas d'usage pratiques avec Delphi

### 1. Application de contrôle qualité industriel

**Objectif** : Détecter automatiquement les produits défectueux sur une chaîne de production.

**Architecture** :
- Caméra connectée → FireMonkey ou VCL
- Capture d'image à intervalles réguliers
- Analyse avec modèle de détection d'anomalies
- Alerte si défaut détecté

**Modèle** : Réseau de neurones entraîné sur images de produits OK vs défectueux.

**Technologies** :
- OpenCV pour capture caméra
- ONNX Runtime pour le modèle
- Base de données pour historique

### 2. Système de gestion documentaire intelligent

**Objectif** : Organiser automatiquement des documents scannés.

**Fonctionnalités** :
- OCR pour extraire le texte
- Classification automatique (facture, contrat, rapport)
- Extraction de métadonnées (date, montant, parties)
- Archivage organisé

**Technologies** :
- Google Cloud Vision ou Azure pour OCR
- Expressions régulières pour extraction
- FireDAC pour stockage en base

### 3. Application de pointage par reconnaissance faciale

**Objectif** : Enregistrer automatiquement les entrées/sorties d'employés.

**Architecture** :
- Caméra à l'entrée
- Capture du visage
- Reconnaissance et identification
- Enregistrement en base de données

**Considérations** :
- RGPD : consentement, durée de conservation
- Performance : reconnaissance rapide (< 1 seconde)
- Sécurité : chiffrement des données biométriques

**Technologies** :
- Azure Face API ou AWS Rekognition
- Base de données sécurisée
- Interface VCL/FMX

### 4. Application mobile de reconnaissance de produits

**Objectif** : Scanner un produit avec smartphone pour obtenir informations/prix.

**Fonctionnalités** :
- Capture photo avec caméra mobile
- Détection et reconnaissance du produit
- Affichage infos + prix
- Ajout au panier

**Technologies** :
- FireMonkey multi-plateforme
- TensorFlow Lite pour Android/iOS
- API REST vers serveur Delphi

### 5. Analyse automatique de radiographies

**Objectif** : Assister les médecins dans le diagnostic.

**Important** : JAMAIS en remplacement du médecin, seulement comme aide.

**Fonctionnalités** :
- Chargement de radiographies
- Détection de zones suspectes
- Suggestions pour examen approfondi
- Génération de rapports

**Technologies** :
- Modèles médicaux spécialisés (pré-entraînés)
- Interface VCL pour professionnels
- Conformité normes médicales (CE, FDA)

## Optimisation et performance

### Utilisation du GPU

Les calculs de reconnaissance d'images sont très gourmands. Les GPU accélèrent considérablement le traitement.

**Avec ONNX Runtime** :
```pascal
// Pseudo-code : Configurer ONNX pour GPU
var
  Options: TOrtSessionOptions;
begin
  Options := TOrtSessionOptions.Create;
  Options.AppendExecutionProvider_CUDA; // Utiliser GPU NVIDIA
  // ou
  Options.AppendExecutionProvider_DirectML; // GPU Windows (AMD/Intel/NVIDIA)
end;
```

**Gain de performance** : 10x à 100x plus rapide selon le modèle.

### Optimisation de modèles

**Quantification** : Réduire la précision (float32 → int8) pour modèles plus légers.
- Taille divisée par 4
- Vitesse accrue
- Légère perte de précision (souvent acceptable)

**Pruning** : Supprimer les connexions neuronales peu importantes.
- Modèles plus compacts
- Inférence plus rapide

**Modèles mobiles** : MobileNet, EfficientNet - conçus pour être légers.

### Mise en cache intelligente

```pascal
// Cache des résultats avec hash de l'image
var
  CacheReconnaissances: TDictionary<string, TResultatReconnaissance>;

function ReconnaireAvecCache(const CheminImage: string): TResultatReconnaissance;
var
  HashImage: string;
begin
  HashImage := CalculerHashMD5(CheminImage);

  if not CacheReconnaissances.TryGetValue(HashImage, Result) then
  begin
    // Pas en cache, analyser
    Result := EffectuerReconnaissance(CheminImage);
    CacheReconnaissances.Add(HashImage, Result);
  end;
  // Sinon, retourner depuis le cache
end;
```

### Traitement par lots

Si vous avez plusieurs images à analyser, groupez-les pour réduire les coûts d'API et améliorer l'efficacité.

```pascal
// API qui supporte le batch
procedure AnalyserImagesBatch(const CheminsImages: TArray<string>);
var
  JSONArray: TJSONArray;
  Chemin: string;
begin
  JSONArray := TJSONArray.Create;
  try
    for Chemin in CheminsImages do
      JSONArray.Add(PreparerImageJSON(Chemin));

    // Envoyer tout en une requête
    EnvoyerBatchAPI(JSONArray);
  finally
    JSONArray.Free;
  end;
end;
```

## Sécurité et considérations éthiques

### Protection de la vie privée

**Pour les visages** :
- Consentement explicite obligatoire
- Informer sur l'utilisation des données
- Droit à l'effacement (RGPD)
- Chiffrement des données biométriques

**Bonnes pratiques** :
- Ne stockez QUE les embeddings (signatures), pas les photos
- Durée de conservation limitée
- Logs d'accès et audit
- Anonymisation quand possible

### Biais des modèles

**Problème connu** : Les modèles d'IA peuvent avoir des biais (ethniques, de genre, etc.).

**Solutions** :
- Utiliser des modèles entraînés sur données diversifiées
- Tester sur populations variées
- Validation humaine pour décisions importantes
- Transparence sur les limitations

### Robustesse et sécurité

**Adversarial attacks** : Images modifiées pour tromper l'IA.

**Mesures** :
- Validation multiple
- Détection d'anomalies
- Limites de confiance (rejeter si < seuil)
- Logs et surveillance

## Ressources et modèles pré-entraînés

### Hubs de modèles

**ONNX Model Zoo** :
- Modèles officiels optimisés
- Classification, détection, segmentation
- Téléchargement gratuit

**TensorFlow Hub** :
- Milliers de modèles
- Convertibles en ONNX
- Documentation complète

**Hugging Face** :
- Plateforme communautaire
- Modèles state-of-the-art
- Espaces de démo interactifs

**PyTorch Hub** :
- Modèles PyTorch officiels
- Convertibles en ONNX
- Code source disponible

### Datasets pour entraînement

**ImageNet** : 14 millions d'images, 1000 catégories
**COCO** : Détection d'objets, 330K images
**Open Images** : 9 millions d'images de Google
**Pascal VOC** : Détection et segmentation

### Outils de visualisation et test

**Netron** : Visualiser l'architecture des modèles ONNX/TensorFlow
**Roboflow** : Annoter et préparer des datasets
**Labelbox** : Annotation d'images collaborative

## Tendances futures

### Modèles fondamentaux multimodaux

**Vision + Langage** : Modèles comme GPT-4 Vision qui combinent compréhension d'images et de texte.

**Applications futures** :
- Assistance visuelle conversationnelle
- Compréhension contextuelle avancée
- Génération d'images à partir de descriptions

### Edge AI

**Tendance** : Déplacer l'IA vers les appareils locaux (smartphones, IoT).

**Avantages** :
- Latence ultra-faible
- Fonctionnement hors ligne
- Confidentialité renforcée

**Technologies** :
- TensorFlow Lite
- ONNX Runtime Mobile
- Puces dédiées (NPU)

### Reconnaissance en temps réel

**Progrès** : Modèles de plus en plus rapides permettant analyse vidéo en temps réel.

**Applications Delphi** :
- Surveillance intelligente
- Réalité augmentée
- Applications industrielles

## Conclusion

La reconnaissance d'images et de formes ouvre des possibilités extraordinaires pour enrichir vos applications Delphi. Que vous choisissiez des API cloud pour leur simplicité ou des bibliothèques locales comme OpenCV et ONNX Runtime pour plus de contrôle, Delphi offre toutes les capacités nécessaires pour une intégration réussie.

**Points essentiels à retenir** :
- La vision par ordinateur couvre de nombreuses tâches : classification, détection, segmentation, OCR
- Les API cloud (Google, Azure, AWS) sont le moyen le plus simple de démarrer
- ONNX Runtime est la meilleure option pour l'intégration locale avec Delphi
- OpenCV reste incontournable pour le traitement d'images avancé
- Le multithreading est crucial pour maintenir une interface réactive
- Respectez toujours la vie privée et les considérations éthiques

**Pour commencer** :
1. Essayez d'abord une API cloud pour une tâche simple (classification d'images)
2. Intégrez progressivement dans vos applications VCL/FMX
3. Optimisez avec mise en cache et traitement asynchrone
4. Explorez ensuite les modèles locaux pour plus de contrôle

La prochaine section explorera le développement de modèles prédictifs, permettant à vos applications d'anticiper et de prendre des décisions intelligentes !

⏭️ [Développement de modèles prédictifs](/22-intelligence-artificielle-et-machine-learning-avec-delphi/05-developpement-de-modeles-predictifs.md)
