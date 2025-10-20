🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.5 Développement de modèles prédictifs

## Qu'est-ce qu'un modèle prédictif ?

### Définition simple

Un modèle prédictif est un système d'intelligence artificielle qui utilise des données historiques pour faire des prévisions sur l'avenir ou sur des situations inconnues. C'est comme un oracle mathématique qui apprend des patterns du passé pour anticiper ce qui va se produire.

**Analogie simple** : Imaginez un médecin expérimenté. Après avoir vu des milliers de patients, il peut prédire avec une bonne précision si un nouveau patient risque de développer une maladie en se basant sur ses symptômes et antécédents. Un modèle prédictif fait la même chose, mais de manière automatique et à grande échelle.

### Différence avec l'IA descriptive

**IA descriptive** : "Que s'est-il passé ?" (analyse du passé)
- Statistiques de ventes du dernier trimestre
- Analyse de tendances historiques
- Rapports et tableaux de bord

**IA prédictive** : "Que va-t-il se passer ?" (anticipation du futur)
- Prévision des ventes du prochain trimestre
- Estimation de la probabilité de désabonnement d'un client
- Prédiction de pannes d'équipement

### Pourquoi les modèles prédictifs sont puissants

**Automatisation des décisions** : Le système peut prendre ou suggérer des décisions en temps réel sans intervention humaine constante.

**Détection de patterns invisibles** : Les modèles peuvent identifier des corrélations complexes que l'œil humain ne verrait jamais.

**Scalabilité** : Une fois entraîné, un modèle peut traiter des millions de prédictions instantanément.

**Amélioration continue** : Les modèles peuvent être réentraînés régulièrement avec de nouvelles données pour s'améliorer.

## Types de modèles prédictifs

### 1. Régression (Prédiction de valeurs numériques)

**Qu'est-ce que c'est ?** : Prédire une valeur numérique continue.

**Exemples** :
- Prédire le prix d'une maison en fonction de sa surface, localisation, nombre de chambres
- Estimer les ventes du prochain mois
- Calculer la durée de vie restante d'un équipement
- Prévoir la température de demain

**Types de régression** :

**Régression linéaire** :
Relation simple et directe entre variables.
```
Prix maison = (Surface × 2000€) + (Chambres × 15000€) + Constante
```

**Régression polynomiale** :
Relations plus complexes avec courbes.

**Régression avec réseaux de neurones** :
Pour relations très complexes et non linéaires.

**Cas d'usage Delphi** :
Application de gestion d'inventaire qui prédit les quantités à commander en fonction des tendances de vente.

### 2. Classification (Prédiction de catégories)

**Qu'est-ce que c'est ?** : Prédire une catégorie ou une classe parmi plusieurs possibilités.

**Classification binaire** (2 catégories) :
- Client va se désabonner : OUI / NON
- Transaction frauduleuse : FRAUDE / LÉGITIME
- Email : SPAM / NON SPAM
- Patient : MALADE / SAIN

**Classification multi-classes** (3+ catégories) :
- Priorité ticket support : BASSE / MOYENNE / HAUTE / URGENTE
- Catégorie produit : ÉLECTRONIQUE / VÊTEMENTS / ALIMENTATION / AUTRES
- Sentiment : POSITIF / NEUTRE / NÉGATIF

**Algorithmes populaires** :
- Régression logistique (malgré son nom, c'est de la classification)
- Arbres de décision
- Random Forest
- Support Vector Machines (SVM)
- Réseaux de neurones

**Cas d'usage Delphi** :
Application CRM qui classe automatiquement les leads en "Haute probabilité de conversion", "Moyenne", "Faible" pour prioriser le travail des commerciaux.

### 3. Séries temporelles

**Qu'est-ce que c'est ?** : Prédire des valeurs futures en se basant sur des séquences chronologiques.

**Caractéristiques** :
- Dépendance temporelle (l'ordre compte)
- Saisonnalité possible
- Tendances à long terme

**Exemples** :
- Prévision de la demande quotidienne
- Prédiction de charge serveur
- Anticipation de trafic réseau
- Prévision météorologique

**Techniques** :
- ARIMA (AutoRegressive Integrated Moving Average)
- Prophet (Facebook)
- LSTM (Long Short-Term Memory) - réseaux de neurones récurrents

**Cas d'usage Delphi** :
Application de gestion énergétique qui prédit la consommation électrique pour optimiser les achats d'énergie.

### 4. Détection d'anomalies

**Qu'est-ce que c'est ?** : Identifier des observations qui diffèrent significativement de la normale.

**Applications** :
- Détection de fraudes bancaires
- Surveillance d'équipements industriels
- Cybersécurité (détection d'intrusions)
- Contrôle qualité

**Approches** :
- **Statistique** : Valeurs au-delà de X écarts-types
- **Machine Learning** : Isolation Forest, One-Class SVM
- **Deep Learning** : Autoencoders

**Cas d'usage Delphi** :
Système de surveillance qui détecte des comportements anormaux dans les logs d'application.

### 5. Clustering (Regroupement)

**Qu'est-ce que c'est ?** : Regrouper automatiquement des données similaires sans catégories prédéfinies.

**Différence avec classification** :
- Classification : catégories connues à l'avance
- Clustering : découvrir les groupes naturels

**Applications** :
- Segmentation de clientèle
- Organisation de produits similaires
- Analyse de comportements utilisateurs
- Compression de données

**Algorithmes** :
- K-Means (le plus populaire)
- DBSCAN
- Hierarchical Clustering

**Cas d'usage Delphi** :
Application marketing qui segmente automatiquement les clients en groupes homogènes pour des campagnes ciblées.

### 6. Systèmes de recommandation

**Qu'est-ce que c'est ?** : Suggérer des items pertinents pour un utilisateur.

**Types** :

**Filtrage collaboratif** :
"Les utilisateurs similaires à vous ont aussi aimé..."

**Filtrage basé sur le contenu** :
"Vous avez aimé X, vous aimerez probablement Y car similaire"

**Hybride** :
Combinaison des deux approches

**Applications** :
- Recommandation de produits e-commerce
- Suggestions de contenu
- Associations de services

**Cas d'usage Delphi** :
Application de vente qui suggère automatiquement des produits complémentaires lors de la saisie d'une commande.

## Le cycle de vie d'un modèle prédictif

### Vue d'ensemble du processus

```
1. Définir le problème
    ↓
2. Collecter les données
    ↓
3. Explorer et préparer les données
    ↓
4. Choisir l'algorithme
    ↓
5. Entraîner le modèle
    ↓
6. Évaluer les performances
    ↓
7. Optimiser (tuning)
    ↓
8. Déployer en production
    ↓
9. Monitorer et maintenir
    ↓
(Retour à l'étape 2 pour amélioration continue)
```

### 1. Définir le problème

**Questions clés** :
- Que voulez-vous prédire exactement ?
- Est-ce un problème de régression ou de classification ?
- Quel niveau de précision est nécessaire ?
- Quelles sont les contraintes (temps, coût, ressources) ?

**Exemple** :
"Nous voulons prédire quels clients vont se désabonner dans les 30 prochains jours avec au moins 80% de précision pour lancer des campagnes de rétention ciblées."

### 2. Collecter les données

**Sources de données** :
- Base de données d'application (FireDAC)
- Fichiers CSV/Excel
- API externes
- Logs et événements
- Capteurs et IoT
- Saisies utilisateurs

**Volume nécessaire** :
- Minimum : quelques centaines d'exemples
- Idéal : plusieurs milliers à millions
- Dépend de la complexité du problème

**Qualité > Quantité** : Mieux vaut 1000 exemples de qualité que 100000 exemples bruités.

### 3. Explorer et préparer les données

**Exploration (EDA - Exploratory Data Analysis)** :
- Statistiques descriptives
- Visualisations (histogrammes, scatter plots)
- Recherche de corrélations
- Détection de valeurs manquantes ou aberrantes

**Nettoyage** :
```pascal
// Pseudo-code : Nettoyer des données
procedure NettoyerDonnees(var Dataset: TDataSet);
begin
  // Supprimer les doublons
  SupprimerDoublons(Dataset);

  // Gérer les valeurs manquantes
  RemplirValeursMissing(Dataset, 'moyenne'); // ou 'médiane', 'mode'

  // Supprimer les outliers extrêmes
  FiltrerOutliers(Dataset, 3.0); // 3 écarts-types

  // Normaliser les formats
  NormaliserDates(Dataset);
  NormaliserTexte(Dataset); // majuscules, espaces
end;
```

**Transformation des features** :

**Normalisation** :
Mettre toutes les valeurs sur la même échelle (0-1 ou -1 à 1).
```
Valeur normalisée = (Valeur - Min) / (Max - Min)
```

**Encodage de catégories** :
Convertir texte en nombres.
```
Couleur : "Rouge" → [1, 0, 0]
          "Vert"  → [0, 1, 0]
          "Bleu"  → [0, 0, 1]
```

**Feature engineering** :
Créer de nouvelles features à partir des existantes.
```
Age + Genre + Historique achats → "Segment client"
Date → "Jour de la semaine", "Mois", "Est weekend", "Est férié"
```

### 4. Choisir l'algorithme

**Pour débuter** :
- **Régression** : Commencer avec régression linéaire
- **Classification** : Commencer avec arbre de décision ou régression logistique
- **Séries temporelles** : Prophet (simple) ou ARIMA

**Critères de choix** :
- Complexité du problème
- Volume de données
- Besoin d'interprétabilité
- Contraintes de performance
- Ressources disponibles

**Règle empirique** :
"Commencez simple, complexifiez si nécessaire."

### 5. Entraîner le modèle

**Principe** : Le modèle "apprend" en ajustant ses paramètres internes pour minimiser l'erreur sur les données d'entraînement.

**Split des données** :
```
Toutes les données (100%)
    ↓
├─ Entraînement (70-80%)  → Pour apprendre
├─ Validation (10-15%)     → Pour optimiser
└─ Test (10-15%)          → Pour évaluer en conditions réelles
```

**Important** : JAMAIS utiliser les données de test pendant l'entraînement ou l'optimisation !

**Avec Python (via Python4Delphi)** :
```python
# Entraînement d'un modèle simple
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

# Séparer données
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42
)

# Créer et entraîner le modèle
model = LinearRegression()
model.fit(X_train, y_train)

# Sauvegarder pour utilisation dans Delphi
import pickle
pickle.dump(model, open('model.pkl', 'wb'))
```

### 6. Évaluer les performances

**Métriques pour régression** :

**MAE (Mean Absolute Error)** :
Erreur moyenne absolue. Plus simple à comprendre.
```
MAE = moyenne(|Prédiction - Réalité|)
```

**RMSE (Root Mean Square Error)** :
Pénalise davantage les grandes erreurs.

**R² (Coefficient de détermination)** :
Proportion de variance expliquée (0 à 1, plus proche de 1 = meilleur).

**Métriques pour classification** :

**Accuracy (Précision globale)** :
Pourcentage de prédictions correctes.
```
Accuracy = (Vrais Positifs + Vrais Négatifs) / Total
```

**Precision (Précision)** :
Parmi les prédictions positives, combien étaient correctes ?

**Recall (Rappel)** :
Parmi tous les cas positifs réels, combien ont été détectés ?

**F1-Score** :
Moyenne harmonique de Precision et Recall.

**Matrice de confusion** :
```
                Prédiction
              Positif  Négatif
Réalité  Pos     TP       FN
         Neg     FP       TN

TP = Vrais Positifs
TN = Vrais Négatifs
FP = Faux Positifs (Erreur type I)
FN = Faux Négatifs (Erreur type II)
```

### 7. Optimiser le modèle

**Hyperparamètres** :
Réglages du modèle qu'on définit AVANT l'entraînement.

**Exemples** :
- Profondeur maximale d'un arbre de décision
- Nombre de neurones dans un réseau
- Taux d'apprentissage

**Techniques d'optimisation** :

**Grid Search** :
Tester toutes les combinaisons possibles.
```
Taux d'apprentissage: [0.01, 0.1, 0.5]
Nombre de couches: [2, 3, 4]
→ 3 × 3 = 9 combinaisons à tester
```

**Random Search** :
Tester des combinaisons aléatoires (plus efficace).

**Cross-validation** :
Validation croisée pour éviter le surapprentissage.

### 8. Déployer en production

**Options pour Delphi** :

**Option 1 : Exporter vers ONNX**
```python
# Python : Convertir en ONNX
import sklearn
from skl2onnx import convert_sklearn
from skl2onnx.common.data_types import FloatTensorType

initial_type = [('float_input', FloatTensorType([None, 4]))]
onx = convert_sklearn(model, initial_types=initial_type)

with open("model.onnx", "wb") as f:
    f.write(onx.SerializeToString())
```

Puis utiliser ONNX Runtime depuis Delphi.

**Option 2 : Service REST Python**
```python
# Python : Flask API
from flask import Flask, request, jsonify
import pickle

app = Flask(__name__)
model = pickle.load(open('model.pkl', 'rb'))

@app.route('/predict', methods=['POST'])
def predict():
    data = request.json['features']
    prediction = model.predict([data])
    return jsonify({'prediction': prediction[0]})

if __name__ == '__main__':
    app.run(port=5000)
```

```pascal
// Delphi : Appeler l'API
function FairePrediction(const Features: TArray<Double>): Double;
var
  JSONObj: TJSONObject;
  JSONArray: TJSONArray;
  Feature: Double;
begin
  JSONArray := TJSONArray.Create;
  for Feature in Features do
    JSONArray.Add(Feature);

  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('features', JSONArray);

    RESTRequest.Body.Add(JSONObj.ToString);
    RESTRequest.Execute;

    Result := RESTResponse.JSONValue.GetValue<Double>('prediction');
  finally
    JSONObj.Free;
  end;
end;
```

**Option 3 : Python4Delphi**
Charger et utiliser directement le modèle Python.

### 9. Monitorer et maintenir

**Monitoring continu** :
- Tracker les performances en production
- Détecter la dérive du modèle (model drift)
- Alertes si précision chute

**Concept drift** :
Quand les patterns des données changent avec le temps, le modèle devient moins performant.

**Solutions** :
- Réentraînement régulier (mensuel, trimestriel)
- Mise à jour avec nouvelles données
- A/B testing de nouveaux modèles

## Intégration pratique dans Delphi

### Architecture recommandée

```
┌───────────────────────────────────┐
│   Application Delphi (UI)         │
│   - Collecte des features         │
│   - Affichage des prédictions     │
└────────────┬──────────────────────┘
             │
┌────────────▼──────────────────────┐
│   Couche de prédiction Delphi     │
│   - Préparation des données       │
│   - Cache des prédictions         │
│   - Gestion des erreurs           │
└────────────┬──────────────────────┘
             │
┌────────────▼──────────────────────┐
│   Service de ML                   │
│   - ONNX Runtime (local)          │
│   - ou API REST Python            │
│   - ou Python4Delphi              │
└───────────────────────────────────┘
```

### Exemple : Prédiction de churn client

**Contexte** : Prédire si un client va se désabonner.

**Features utilisées** :
- Ancienneté (mois)
- Nombre d'achats derniers 6 mois
- Montant moyen par commande
- Nombre de contacts support
- Délai depuis dernier achat
- Score de satisfaction

**Implémentation Delphi** :

```pascal
type
  TClientFeatures = record
    Anciennete: Integer;
    NbAchats: Integer;
    MontantMoyen: Double;
    ContactsSupport: Integer;
    JoursDepuisDernierAchat: Integer;
    ScoreSatisfaction: Double;
  end;

  TPredictionChurn = record
    Probabilite: Double;
    Risque: string; // 'FAIBLE', 'MOYEN', 'ÉLEVÉ'
    ActionsRecommandees: TArray<string>;
  end;

function PredireChurn(const ClientID: Integer): TPredictionChurn;
var
  Features: TClientFeatures;
  Proba: Double;
begin
  // 1. Récupérer les données du client
  Features := ChargerFeaturesClient(ClientID);

  // 2. Normaliser les features
  Features := NormaliserFeatures(Features);

  // 3. Faire la prédiction
  Proba := AppelerModeleChurn(Features);

  // 4. Interpréter le résultat
  Result.Probabilite := Proba;

  if Proba >= 0.7 then
  begin
    Result.Risque := 'ÉLEVÉ';
    Result.ActionsRecommandees := [
      'Contacter immédiatement',
      'Offrir remise 20%',
      'Proposer support premium gratuit'
    ];
  end
  else if Proba >= 0.4 then
  begin
    Result.Risque := 'MOYEN';
    Result.ActionsRecommandees := [
      'Email personnalisé',
      'Proposition de nouveaux produits'
    ];
  end
  else
  begin
    Result.Risque := 'FAIBLE';
    Result.ActionsRecommandees := ['Suivi standard'];
  end;
end;

// Utilisation dans l'interface
procedure TFormClient.BtnAnalyserClick(Sender: TObject);
var
  Prediction: TPredictionChurn;
begin
  Prediction := PredireChurn(ClientActuelID);

  LabelRisque.Caption := Prediction.Risque;
  GaugeRisque.Progress := Round(Prediction.Probabilite * 100);

  MemoActions.Lines.Clear;
  MemoActions.Lines.AddStrings(Prediction.ActionsRecommandees);

  // Changer la couleur selon le risque
  case Prediction.Risque of
    'ÉLEVÉ': PanelRisque.Color := clRed;
    'MOYEN': PanelRisque.Color := clYellow;
    'FAIBLE': PanelRisque.Color := clGreen;
  end;
end;
```

### Traitement par batch

Pour des prédictions sur de nombreux enregistrements :

```pascal
procedure AnalyserTousClients;
var
  Query: TFDQuery;
  Prediction: TPredictionChurn;
  ClientID: Integer;
begin
  ProgressBar.Max := GetNombreClients;
  ProgressBar.Position := 0;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;
    Query.SQL.Text := 'SELECT ClientID FROM Clients WHERE Actif = 1';
    Query.Open;

    while not Query.Eof do
    begin
      ClientID := Query.FieldByName('ClientID').AsInteger;

      // Prédiction
      Prediction := PredireChurn(ClientID);

      // Sauvegarder en base
      SauvegarderPrediction(ClientID, Prediction);

      Query.Next;
      ProgressBar.Position := ProgressBar.Position + 1;
      Application.ProcessMessages; // Garder l'UI réactive
    end;
  finally
    Query.Free;
  end;

  ShowMessage('Analyse terminée !');
end;
```

### Gestion du cache

```pascal
type
  TCachePredictions = class
  private
    FCache: TDictionary<Integer, TPredictionChurn>;
    FExpiration: TDictionary<Integer, TDateTime>;
    FDureeValidite: Integer; // en heures
  public
    constructor Create(DureeValiditeHeures: Integer = 24);
    destructor Destroy; override;

    function ObtenirPrediction(ClientID: Integer): TPredictionChurn;
    procedure InvaliderCache(ClientID: Integer);
    procedure ViderCache;
  end;

function TCachePredictions.ObtenirPrediction(ClientID: Integer): TPredictionChurn;
var
  DateExpiration: TDateTime;
begin
  // Vérifier si en cache et encore valide
  if FCache.ContainsKey(ClientID) and
     FExpiration.TryGetValue(ClientID, DateExpiration) then
  begin
    if Now < DateExpiration then
    begin
      // Cache valide
      Result := FCache[ClientID];
      Exit;
    end;
  end;

  // Pas en cache ou expiré, calculer
  Result := PredireChurn(ClientID);

  // Mettre en cache
  FCache.AddOrSetValue(ClientID, Result);
  FExpiration.AddOrSetValue(ClientID, Now + (FDureeValidite / 24));
end;
```

## Cas pratiques avec Delphi

### 1. Prévision de ventes

**Objectif** : Prédire les ventes mensuelles pour optimiser les stocks.

**Features** :
- Historique des ventes (12-24 derniers mois)
- Saisonnalité (mois, jour de la semaine)
- Promotions planifiées
- Événements externes (vacances, météo)
- Tendances du marché

**Modèle** : Séries temporelles (Prophet ou ARIMA)

**Intégration** :
```pascal
function PrevoirVentesProchainMois: TArray<Double>;
var
  HistoriqueVentes: TArray<Double>;
  Previsions: TJSONArray;
  i: Integer;
begin
  // Récupérer l'historique
  HistoriqueVentes := ChargerHistoriqueVentes(24); // 24 mois

  // Appeler API Python
  RESTRequest.AddParameter('historique',
    TJSONArray.Create(HistoriqueVentes).ToString);
  RESTRequest.Execute;

  // Parser les prévisions
  Previsions := RESTResponse.JSONValue.GetValue<TJSONArray>('previsions');
  SetLength(Result, Previsions.Count);

  for i := 0 to Previsions.Count - 1 do
    Result[i] := Previsions.Items[i].AsType<Double>;
end;
```

**Affichage** : Graphique avec TeeChart montrant historique + prévisions.

### 2. Scoring de leads

**Objectif** : Évaluer automatiquement la qualité des prospects pour prioriser les actions commerciales.

**Features** :
- Taille entreprise
- Secteur d'activité
- Source du lead
- Comportement sur le site (pages vues, temps passé)
- Interactions emails
- Budget estimé

**Modèle** : Classification (Random Forest ou Gradient Boosting)

**Classes** :
- A (Excellent) : > 80% chance de conversion
- B (Bon) : 50-80%
- C (Moyen) : 20-50%
- D (Faible) : < 20%

**Interface Delphi** :
```pascal
procedure TFormLead.CalculerScore;
var
  Score: string;
  Probabilite: Double;
begin
  Probabilite := PredireConversionLead(LeadActuel);

  if Probabilite >= 0.8 then
    Score := 'A'
  else if Probabilite >= 0.5 then
    Score := 'B'
  else if Probabilite >= 0.2 then
    Score := 'C'
  else
    Score := 'D';

  LabelScore.Caption := Format('Score: %s (%d%%)',
    [Score, Round(Probabilite * 100)]);

  // Afficher actions recommandées
  AfficherActionsRecommandees(Score);
end;
```

### 3. Maintenance prédictive

**Objectif** : Anticiper les pannes d'équipements industriels.

**Features** :
- Âge de la machine
- Heures de fonctionnement
- Température
- Vibrations
- Consommation électrique
- Historique de maintenance

**Modèle** : Régression (prédire jours avant panne) ou Classification (panne imminente oui/non)

**Alerte proactive** :
```pascal
procedure VerifierEtatEquipements;
var
  Query: TFDQuery;
  EquipementID: Integer;
  JoursAvantPanne: Double;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;
    Query.SQL.Text := 'SELECT * FROM Equipements WHERE Actif = 1';
    Query.Open;

    while not Query.Eof do
    begin
      EquipementID := Query.FieldByName('ID').AsInteger;
      JoursAvantPanne := PredirePanneEquipement(EquipementID);

      if JoursAvantPanne <= 7 then
      begin
        // Alerte urgente
        CreerTicketMaintenance(EquipementID, 'URGENTE');
        EnvoyerNotification(
          Format('Équipement %d : panne prédite dans %.1f jours',
            [EquipementID, JoursAvantPanne])
        );
      end
      else if JoursAvantPanne <= 30 then
      begin
        // Alerte préventive
        PlanifierMaintenance(EquipementID, Now + JoursAvantPanne);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;
```

### 4. Détection de fraudes

**Objectif** : Identifier les transactions suspectes en temps réel.

**Features** :
- Montant de la transaction
- Localisation (différente de l'habituelle ?)
- Heure (inhabituelle ?)
- Type de marchand
- Fréquence des transactions récentes
- Historique du client

**Modèle** : Classification (Isolation Forest ou réseau de neurones)

**Traitement temps réel** :
```pascal
function VerifierTransaction(const Transaction: TTransaction): TResultatVerification;
var
  ScoreFraude: Double;
begin
  // Calculer score de fraude
  ScoreFraude := DetecterFraude(Transaction);

  Result.Suspect := ScoreFraude > 0.7;
  Result.Score := ScoreFraude;

  if ScoreFraude > 0.9 then
  begin
    Result.Action := 'BLOQUER';
    Result.Raison := 'Très haute probabilité de fraude';
  end
  else if ScoreFraude > 0.7 then
  begin
    Result.Action := 'VÉRIFICATION_MANUELLE';
    Result.Raison := 'Transaction suspecte, validation requise';
  end
  else
  begin
    Result.Action := 'APPROUVER';
    Result.Raison := 'Transaction normale';
  end;

  // Logger pour audit
  LoggerTransaction(Transaction, Result);
end;
```

### 5. Optimisation de prix dynamique

**Objectif** : Suggérer le prix optimal pour maximiser les ventes ou la marge.

**Features** :
- Coût du produit
- Prix concurrents
- Élasticité-prix historique
- Stock disponible
- Saison/période
- Segment client

**Modèle** : Régression ou optimisation (reinforcement learning avancé)

**Application** :
```pascal
function SuggererPrixOptimal(const ProduitID: Integer): TPrixOptimal;
var
  PrixMin, PrixMax, PrixSuggere: Double;
  VentesEstimees: Integer;
begin
  // Contraintes
  PrixMin := ObtenirCoutProduit(ProduitID) * 1.2; // Marge min 20%
  PrixMax := ObtenirPrixMarcheMax(ProduitID);

  // Prédiction
  PrixSuggere := PredirePrixOptimal(ProduitID);

  // S'assurer dans les contraintes
  if PrixSuggere < PrixMin then
    PrixSuggere := PrixMin;
  if PrixSuggere > PrixMax then
    PrixSuggere := PrixMax;

  Result.Prix := PrixSuggere;
  Result.VentesEstimees := PrevoirVentes(ProduitID, PrixSuggere);
  Result.MargeEstimee := CalculerMarge(ProduitID, PrixSuggere);

  Result.Confiance := ObtenirConfianceModele;
end;
```

## Outils et bibliothèques pour modèles prédictifs

### Scikit-learn (Python)

**Le couteau suisse du ML** :
- Dizaines d'algorithmes implémentés
- Interface cohérente et simple
- Excellent pour débuter
- Très bien documenté

**Usage avec Delphi** : Via Python4Delphi ou API REST

**Forces** :
- ML "classique" (non deep learning)
- Prétraitement de données
- Validation et métriques

### XGBoost / LightGBM

**Modèles d'ensemble très performants** :
- Souvent gagnants de compétitions Kaggle
- Excellents pour données tabulaires
- Très rapides

**Cas d'usage** : Scoring, prédictions de séries, classification complexe

### Prophet (Facebook)

**Spécialisé séries temporelles** :
- Très simple d'utilisation
- Gère automatiquement saisonnalité
- Robuste aux données manquantes
- Idéal pour prévisions business

### TensorFlow / Keras

**Pour deep learning** :
- Réseaux de neurones complexes
- Excellent pour grandes quantités de données
- GPU requis pour performances

### H2O.ai

**Plateforme AutoML** :
- Automatise le choix et l'optimisation des modèles
- Interface REST native
- Parfait pour Delphi

**Avantage** : Pas besoin d'être expert ML, H2O fait le travail.

## Pièges et bonnes pratiques

### Pièges courants

**1. Surapprentissage (Overfitting)**

**Symptôme** : Modèle excellent sur données d'entraînement, mauvais en production.

**Cause** : Le modèle a "mémorisé" plutôt qu'appris des patterns généraux.

**Solutions** :
- Plus de données d'entraînement
- Régularisation
- Validation croisée
- Simplifier le modèle

**2. Fuite de données (Data Leakage)**

**Erreur** : Inclure dans les features des informations qui ne seront pas disponibles au moment de la prédiction.

**Exemple** : Prédire si un client va acheter en incluant "montant total acheté" (qui n'existe que s'il a déjà acheté).

**3. Biais dans les données**

**Problème** : Données d'entraînement non représentatives de la réalité.

**Exemple** : Entraîner sur données de l'été pour prédire toute l'année.

**Solution** : Données diversifiées et représentatives.

**4. Ignorer la dérive du modèle**

**Problème** : Le monde change, les patterns aussi.

**Solution** : Monitoring continu et réentraînement régulier.

### Bonnes pratiques

**1. Commencer simple**
Testez d'abord un modèle simple (régression linéaire, arbre de décision). Complexifiez seulement si nécessaire.

**2. Feature engineering > Algorithme complexe**
De bonnes features avec un modèle simple battent souvent un modèle complexe avec features médiocres.

**3. Toujours avoir des données de test séparées**
JAMAIS toucher aux données de test avant l'évaluation finale.

**4. Documenter vos choix**
Notez pourquoi vous avez choisi tel algorithme, tels hyperparamètres.

**5. Versionner vos modèles**
Comme le code, gardez l'historique de vos modèles.

```pascal
// Structure de versioning
type
  TVersionModele = record
    Version: string;        // '1.2.3'
    DateCreation: TDateTime;
    Algorithme: string;
    Features: TArray<string>;
    Performance: Double;    // Score sur test set
    CheminFichier: string;  // 'models/churn_v1.2.3.onnx'
  end;
```

**6. Expliquez les prédictions**
Donnez du contexte à l'utilisateur, pas juste un chiffre.

```pascal
// Mauvais
ShowMessage('Probabilité: 0.73');

// Bon
ShowMessage(
  'Risque de désabonnement: ÉLEVÉ (73%)' + #13#10 +
  'Facteurs principaux:' + #13#10 +
  '- Pas d''achat depuis 90 jours' + #13#10 +
  '- 3 contacts support le mois dernier' + #13#10 +
  '- Score satisfaction en baisse'
);
```

**7. Mesurer l'impact business**
Un modèle précis mais inutilisé ne sert à rien.

**Métriques business** :
- ROI de l'implémentation
- Temps économisé
- Augmentation des ventes
- Réduction des coûts

## Conclusion

Le développement de modèles prédictifs transforme vos applications Delphi en systèmes intelligents capables d'anticiper et d'optimiser les décisions. Bien que le Machine Learning puisse sembler complexe, les outils modernes et les approches d'intégration avec Delphi rendent ces technologies accessibles.

**Points essentiels** :
- Les modèles prédictifs apprennent des patterns du passé pour prévoir l'avenir
- Régression (valeurs) et Classification (catégories) couvrent la majorité des cas d'usage
- Le cycle de vie : données → entraînement → évaluation → déploiement → monitoring
- L'intégration avec Delphi se fait via ONNX, API REST, ou Python4Delphi
- La qualité des données et le feature engineering sont cruciaux
- Commencez simple et itérez

**Pour démarrer** :
1. Identifiez un cas d'usage concret dans votre application
2. Collectez et nettoyez les données historiques
3. Commencez avec un modèle simple via API ou Python
4. Intégrez progressivement dans votre interface Delphi
5. Mesurez l'impact et améliorez

La prochaine section explorera l'intégration avec les services d'IA cloud comme Azure AI et Google AI, offrant des capacités encore plus avancées pour vos applications Delphi !

⏭️ [Intégration avec des services d'IA cloud (Azure AI, Google AI, etc.)](/22-intelligence-artificielle-et-machine-learning-avec-delphi/06-integration-avec-des-services-dia-cloud.md)
