🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.3 Traitement du langage naturel (NLP)

## Qu'est-ce que le traitement du langage naturel ?

### Définition simple

Le Traitement du Langage Naturel (NLP - Natural Language Processing) est une branche de l'intelligence artificielle qui permet aux ordinateurs de comprendre, interpréter et manipuler le langage humain.

**En d'autres termes** : Le NLP permet à votre application de "lire", "comprendre" et "communiquer" en utilisant un langage humain naturel, comme le français ou l'anglais.

### Pourquoi le NLP est-il complexe ?

Le langage humain est bien plus complexe qu'il n'y paraît :
- **Ambiguïté** : "Il a vu l'homme avec le télescope" (qui a le télescope ?)
- **Contexte** : "C'est génial !" peut être sincère ou sarcastique
- **Variabilité** : Il existe mille façons de dire la même chose
- **Évolution** : De nouveaux mots et expressions apparaissent constamment
- **Subtilité** : Tons, émotions, sous-entendus

**L'exploit du NLP** : Transformer ce langage complexe et imprécis en données structurées qu'un ordinateur peut traiter.

## Cas d'utilisation du NLP dans les applications Delphi

### Applications pratiques

**Analyse de feedback clients** :
- Classifier automatiquement les commentaires (positifs/négatifs/neutres)
- Extraire les thèmes récurrents dans les retours clients
- Identifier les urgences ou réclamations prioritaires

**Automatisation du service client** :
- Chatbots intelligents intégrés dans vos applications
- Réponses automatiques aux questions fréquentes
- Routage intelligent des demandes vers les bons services

**Traitement de documents** :
- Extraction d'informations dans des contrats ou factures
- Résumé automatique de longs documents
- Classification automatique de documents

**Recherche intelligente** :
- Moteurs de recherche qui comprennent les intentions
- Suggestions de recherche pertinentes
- Recherche sémantique (par le sens, pas seulement par mots-clés)

**Analyse de données textuelles** :
- Détection de tendances dans des données textuelles
- Analyse de sentiments sur les réseaux sociaux
- Veille médiatique automatisée

## Tâches fondamentales du NLP

### 1. Tokenisation

**Qu'est-ce que c'est ?** : Découper un texte en unités plus petites (tokens) : mots, phrases, ou caractères.

**Exemple** :
```
Texte : "Bonjour, comment allez-vous ?"  
Tokens : ["Bonjour", ",", "comment", "allez-vous", "?"]  
```

**Utilité** : C'est la première étape de presque tous les traitements NLP. Impossible d'analyser un texte sans d'abord le découper.

### 2. Analyse de sentiments

**Qu'est-ce que c'est ?** : Déterminer l'émotion ou l'opinion exprimée dans un texte (positif, négatif, neutre).

**Exemples** :
- "Ce produit est excellent !" → Positif (0.95)
- "Service catastrophique, je ne recommande pas." → Négatif (-0.82)
- "Le colis est arrivé hier." → Neutre (0.05)

**Applications** :
- Surveillance de la réputation en ligne
- Analyse de satisfaction client
- Détection de crises potentielles

### 3. Classification de texte

**Qu'est-ce que c'est ?** : Attribuer automatiquement une ou plusieurs catégories à un texte.

**Exemples** :
- Emails : spam ou légitime
- Articles de presse : sport, économie, politique, culture
- Tickets de support : bug, demande de fonctionnalité, question

**Avantage** : Automatise le tri et l'organisation de grandes quantités de texte.

### 4. Extraction d'entités nommées (NER)

**Qu'est-ce que c'est ?** : Identifier et extraire des informations spécifiques dans un texte : noms de personnes, lieux, dates, organisations, montants...

**Exemple** :
```
Texte : "Jean Dupont habitait à Paris et travaillait chez Microsoft depuis 2015."

Entités extraites :
- Personne : Jean Dupont
- Lieu : Paris
- Organisation : Microsoft
- Date : 2015
```

**Applications** :
- Extraction automatique de données de contrats
- Remplissage automatique de formulaires
- Indexation de documents

### 5. Résumé automatique

**Qu'est-ce que c'est ?** : Générer une version condensée d'un texte long en conservant les informations essentielles.

**Types** :
- **Extractif** : Sélectionne les phrases les plus importantes du texte original
- **Abstractif** : Génère de nouvelles phrases qui résument le contenu (plus avancé)

**Applications** :
- Résumés de rapports longs
- Synthèses de réunions
- Prévisualisations d'articles

### 6. Traduction automatique

**Qu'est-ce que c'est ?** : Traduire automatiquement un texte d'une langue à une autre.

**Évolution** : Les traducteurs modernes (comme Google Translate ou DeepL) utilisent des réseaux de neurones et sont beaucoup plus précis que les anciennes méthodes.

**Applications** :
- Applications multilingues
- Communication internationale
- Traduction de documentation

### 7. Questions-Réponses (Q&A)

**Qu'est-ce que c'est ?** : Système qui peut répondre à des questions en langage naturel en s'appuyant sur une base de connaissances ou un texte.

**Exemple** :
```
Texte : "Delphi 13 Florence a été publié en novembre 2024."  
Question : "Quand Delphi 13 est-il sorti ?"  
Réponse : "En novembre 2024"  
```

**Applications** :
- Assistants intelligents
- Systèmes d'aide en ligne
- Chatbots de support

### 8. Reconnaissance d'intentions

**Qu'est-ce que c'est ?** : Comprendre ce que l'utilisateur veut faire à partir de sa phrase.

**Exemples** :
- "Réserve-moi un vol pour Lyon" → Intention : RÉSERVATION_VOL
- "Quel temps fera-t-il demain ?" → Intention : MÉTÉO
- "Annule ma commande" → Intention : ANNULATION

**Applications** :
- Chatbots et assistants vocaux
- Interfaces en langage naturel
- Automatisation de tâches

## Intégration du NLP dans les applications Delphi

### Approche 1 : API Cloud de NLP

**Services disponibles** :

**Google Cloud Natural Language API**
- Analyse de sentiments
- Extraction d'entités
- Analyse syntaxique
- Classification de contenu

**Azure Cognitive Services - Text Analytics**
- Analyse de sentiments
- Extraction de phrases clés
- Reconnaissance d'entités nommées
- Détection de langue

**AWS Comprehend**
- Analyse de sentiments
- Extraction d'entités
- Détection de sujets
- Classification personnalisée

**OpenAI API (GPT-4)**
- Traitement de texte avancé
- Génération de texte
- Résumés, traductions, questions-réponses
- Conversations naturelles

### Comment utiliser une API NLP avec Delphi

**Principe général** : Toutes ces API fonctionnent via REST, ce qui est parfait pour Delphi avec TRESTClient.

**Exemple de flux** :
```pascal
// 1. Préparer le texte à analyser
var
  TexteAnalyser: string;
begin
  TexteAnalyser := MemoCommentaire.Text;

  // 2. Configurer la requête REST
  RESTClient.BaseURL := 'https://api.example.com/sentiment';
  RESTRequest.Method := rmPOST;
  RESTRequest.AddParameter('text', TexteAnalyser, pkREQUESTBODY);

  // 3. Exécuter l'analyse (de préférence en arrière-plan)
  TTask.Run(procedure
  begin
    RESTRequest.Execute;

    // 4. Traiter le résultat
    TThread.Synchronize(nil, procedure
    var
      Sentiment: string;
      Score: Double;
    begin
      Sentiment := RESTResponse.JSONValue.GetValue<string>('sentiment');
      Score := RESTResponse.JSONValue.GetValue<Double>('score');

      LabelResultat.Caption := Format('Sentiment: %s (%.2f)',
        [Sentiment, Score]);
    end);
  end);
end;
```

**Avantages des API Cloud** :
- Facile à intégrer avec TRESTClient
- Modèles constamment mis à jour
- Pas besoin d'expertise ML
- Multiples langues supportées
- Résultats de haute qualité

**Inconvénients** :
- Nécessite une connexion internet
- Coûts selon le volume d'utilisation
- Données envoyées à un tiers (confidentialité)
- Latence réseau

### Approche 2 : Bibliothèques NLP locales

**Bibliothèques populaires** :

**spaCy (Python)**
- Bibliothèque NLP moderne et rapide
- Support de nombreuses langues dont le français
- Extraction d'entités, analyse syntaxique, etc.
- Utilisation via Python4Delphi

**NLTK (Python)**
- Bibliothèque académique très complète
- Excellent pour l'apprentissage
- Plus lente que spaCy
- Via Python4Delphi également

**Stanford NLP (Java)**
- Suite complète d'outils NLP
- Très précis mais gourmand en ressources
- Peut être appelé via services REST ou JNI

**Intégration avec Python4Delphi** :

```pascal
// Exemple conceptuel avec spaCy
procedure AnalyserTexteLocal(const Texte: string);  
var  
  PythonEngine: TPythonEngine;
  Script: string;
begin
  PythonEngine := TPythonEngine.Create(nil);
  try
    // Charger spaCy et analyser le texte
    Script :=
      'import spacy' + #13#10 +
      'nlp = spacy.load("fr_core_news_sm")' + #13#10 +
      'doc = nlp("' + Texte + '")' + #13#10 +
      'entities = [(ent.text, ent.label_) for ent in doc.ents]';

    PythonEngine.ExecString(Script);

    // Récupérer et traiter les résultats
    // ...
  finally
    PythonEngine.Free;
  end;
end;
```

**Avantages** :
- Fonctionne hors ligne
- Confidentialité des données
- Pas de coûts récurrents
- Contrôle total

**Inconvénients** :
- Configuration plus complexe
- Distribution avec dépendances
- Nécessite de la puissance de calcul
- Mises à jour manuelles des modèles

### Approche 3 : LLM (Large Language Models) via API

Les modèles de langage de grande taille comme GPT-4, Claude, ou Llama sont extrêmement polyvalents pour le NLP.

**Capacités** :
- Comprennent le contexte de manière sophistiquée
- Peuvent effectuer presque toutes les tâches NLP
- Génèrent du texte de qualité humaine
- S'adaptent à de nouvelles tâches sans réentraînement

**Utilisation avec Delphi** :
Exactement comme les API REST classiques, mais avec des prompts (instructions) bien formulés.

**Exemple de prompt pour analyse de sentiment** :
```
Analyse le sentiment du texte suivant et réponds uniquement  
par "positif", "négatif" ou "neutre", suivi d'un score de  
confiance entre 0 et 1.  

Texte : "Ce produit a dépassé mes attentes !"
```

**Avantages** :
- Extrêmement flexibles
- Excellents résultats même sur des tâches complexes
- Un seul modèle pour de multiples tâches
- Compréhension contextuelle avancée

**Inconvénients** :
- Coûteux pour de gros volumes
- Latence plus élevée
- Résultats parfois imprévisibles
- Nécessite des prompts bien conçus

## Gestion des langues avec le NLP

### Détection automatique de langue

**Pourquoi c'est important** : Avant de traiter un texte, vous devez souvent savoir dans quelle langue il est écrit.

**Solutions** :
- **API Cloud** : Google Cloud Translation propose une détection de langue
- **Bibliothèques** : langdetect (Python) via Python4Delphi
- **Simple** : Analyser les caractères et patterns

**Exemple d'utilisation** :
```pascal
// Pseudo-code
var
  Langue: string;
begin
  Langue := DetecterLangue(TexteUtilisateur);

  if Langue = 'fr' then
    TraiterTexteFrancais(TexteUtilisateur)
  else if Langue = 'en' then
    TraiterTexteAnglais(TexteUtilisateur)
  else if Langue = 'es' then
    TraiterTexteEspagnol(TexteUtilisateur)
  else
    ShowMessage('Langue non supportée');
end;
```

### Support du français

**Modèles disponibles** :

**Pour spaCy** :
- `fr_core_news_sm` : Modèle français léger
- `fr_core_news_md` : Modèle moyen (meilleur)
- `fr_core_news_lg` : Modèle large (le plus précis)

**Pour les API Cloud** :
- Toutes supportent nativement le français
- Qualité généralement excellente

**Spécificités du français** :
- Accords en genre et nombre
- Conjugaisons complexes
- Accents et caractères spéciaux
- Expressions idiomatiques

## Techniques avancées de NLP

### Embeddings (Plongements de mots)

**Concept** : Représenter les mots comme des vecteurs de nombres qui capturent leur signification.

**Exemple** :
```
"chat" → [0.2, -0.5, 0.8, ...]
"chien" → [0.3, -0.4, 0.7, ...]
```

**Propriété magique** : Les mots similaires ont des vecteurs proches.

**Applications** :
- Recherche sémantique (par le sens)
- Détection de documents similaires
- Recommandations de contenu

**Modèles populaires** :
- Word2Vec
- GloVe
- FastText
- BERT, GPT (contextuels)

### Transformers et BERT

**Révolution récente** : Les architectures Transformer (comme BERT, GPT) ont révolutionné le NLP.

**Pourquoi c'est important** :
- Comprennent le contexte bidirectionnel
- Pré-entraînés sur d'énormes corpus
- Adaptables à de nombreuses tâches

**Pour Delphi** : Utilisez ces modèles via :
- API Cloud (OpenAI, Hugging Face)
- ONNX Runtime pour les modèles locaux
- Python4Delphi avec bibliothèque Transformers

## Construire un système NLP dans votre application Delphi

### Architecture recommandée

**Conception en couches** :

```
┌─────────────────────────────┐
│   Interface Utilisateur     │  (VCL/FMX)
│   (Saisie/Affichage texte)  │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│   Couche de traitement NLP  │  (Delphi)
│   - Prétraitement           │
│   - Appels API/Bibliothèques│
│   - Gestion du cache        │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│   Services NLP              │  (Cloud ou Local)
│   - Modèles ML              │
│   - Traitement réel         │
└─────────────────────────────┘
```

### Stratégies d'optimisation

**1. Mise en cache** :
Ne retraitez pas le même texte plusieurs fois. Stockez les résultats.

```pascal
// Cache simple avec TDictionary
var
  CacheSentiments: TDictionary<string, Double>;

function ObtenirSentiment(const Texte: string): Double;  
begin  
  if not CacheSentiments.TryGetValue(Texte, Result) then
  begin
    // Appel API seulement si non en cache
    Result := AppelerAPISentiment(Texte);
    CacheSentiments.Add(Texte, Result);
  end;
end;
```

**2. Traitement par lots** :
Envoyez plusieurs textes en une seule requête pour réduire les coûts et la latence.

**3. Prétraitement côté client** :
- Nettoyez le texte avant de l'envoyer
- Supprimez les espaces inutiles
- Normalisez la casse si nécessaire

**4. Interface réactive** :
Utilisez toujours le multithreading pour les appels NLP afin de ne pas bloquer l'interface.

```pascal
procedure AnalyserTexteAsync(const Texte: string;
  const Callback: TProc<string>);
begin
  TTask.Run(procedure
  var
    Resultat: string;
  begin
    // Traitement NLP (lent)
    Resultat := EffectuerAnalyseNLP(Texte);

    // Retour sur le thread UI
    TThread.Queue(nil, procedure
    begin
      Callback(Resultat);
    end);
  end);
end;
```

## Cas pratique : Chatbot simple

### Architecture d'un chatbot

**Composants nécessaires** :

1. **Interface de chat** (VCL/FMX)
   - Zone de saisie utilisateur
   - Historique de conversation
   - Affichage des réponses

2. **Analyse d'intention**
   - Déterminer ce que l'utilisateur veut
   - Extraire les paramètres pertinents

3. **Gestionnaire de dialogue**
   - Maintenir le contexte de conversation
   - Générer les réponses appropriées

4. **Base de connaissances**
   - Réponses prédéfinies
   - Accès à des données métier
   - FAQ

### Flux de traitement

```
Utilisateur : "Quel est le statut de ma commande 12345 ?"
    ↓
1. Détection d'intention → VÉRIFIER_STATUT_COMMANDE
2. Extraction d'entités → NuméroCommande: "12345"
3. Requête base de données → Recherche commande 12345
4. Génération réponse → "Votre commande 12345 est en cours
                         de livraison, arrivée prévue demain."
    ↓
Affichage dans le chat
```

### Implémentation avec API Cloud

**Avec OpenAI (GPT) par exemple** :

Envoyez l'historique de conversation complet et laissez le modèle générer la réponse appropriée.

**Avantages** :
- Conversations naturelles et fluides
- Gestion automatique du contexte
- Adaptation aux questions inattendues

**Avec une approche hybride** :

Utilisez un LLM pour comprendre l'intention, puis du code Delphi classique pour exécuter les actions métier.

## Défis et bonnes pratiques

### Défis courants

**1. Gestion des ambiguïtés**
Les textes peuvent avoir plusieurs interprétations. Prévoyez des confirmations utilisateur.

**2. Erreurs de l'IA**
Les modèles NLP peuvent se tromper. Permettez toujours à l'utilisateur de corriger.

**3. Langues et dialectes**
Le français de France ≠ français du Québec. Choisissez les bons modèles.

**4. Coûts**
Les API peuvent coûter cher avec de gros volumes. Optimisez et mettez en cache.

**5. Confidentialité**
Ne envoyez jamais de données sensibles à des API tierces sans cryptage et consentement.

### Bonnes pratiques

**Validation humaine** : Pour les décisions critiques, ajoutez toujours une validation humaine.

**Feedback utilisateur** : Permettez aux utilisateurs de signaler les erreurs pour améliorer le système.

**Gestion d'erreurs robuste** : Les API peuvent échouer, prévoyez des alternatives ou messages explicites.

**Tests exhaustifs** : Testez avec des textes variés, y compris des cas limites.

**Documentation** : Documentez les limitations de votre système NLP pour les utilisateurs.

## Ressources pour aller plus loin

### Apprentissage du NLP

**Cours en ligne** :
- Coursera : "Natural Language Processing" par DeepLearning.AI
- Fast.ai : Cours gratuits sur le NLP
- Documentation officielle spaCy (excellents tutoriels)

### Jeux de données français

**Pour s'entraîner** :
- French Sentiment Analysis Dataset
- AlloCiné (critiques de films françaises)
- Wikipedia français pour l'entraînement de modèles

### Outils de test

**Postman** : Testez vos appels API NLP avant de les intégrer dans Delphi

**Jupyter Notebooks** : Expérimentez avec Python et les bibliothèques NLP avant l'intégration

## Conclusion

Le traitement du langage naturel ouvre des possibilités extraordinaires pour enrichir vos applications Delphi. Que vous choisissiez des API cloud pour leur simplicité ou des bibliothèques locales pour plus de contrôle, Delphi offre les outils nécessaires pour une intégration réussie.

**Points essentiels** :
- Le NLP permet à vos applications de comprendre le langage humain
- Les API REST cloud sont le moyen le plus simple de démarrer
- Python4Delphi donne accès à l'écosystème riche des bibliothèques Python
- Utilisez le multithreading pour garder votre interface réactive
- Le cache et l'optimisation sont cruciaux pour les performances et les coûts

**Commencez petit** : Intégrez d'abord une fonctionnalité simple comme l'analyse de sentiments, puis progressez vers des systèmes plus complexes comme des chatbots ou de l'extraction d'information avancée.

Dans la section suivante, nous explorerons la reconnaissance d'images et de formes, un autre domaine passionnant de l'intelligence artificielle !

⏭️ [Reconnaissance d'images et de formes](/22-intelligence-artificielle-et-machine-learning-avec-delphi/04-reconnaissance-dimages-et-de-formes.md)
