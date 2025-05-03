# 12.7 Tests d'intégration

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux tests d'intégration

Alors que les tests unitaires (vus en section 12.3) se concentrent sur la vérification du bon fonctionnement des composants individuels de votre application, les tests d'intégration visent à valider les interactions entre ces composants. Ils constituent une étape cruciale pour s'assurer que les différentes parties de votre application fonctionnent correctement ensemble.

## Tests unitaires vs tests d'intégration

Pour bien comprendre les tests d'intégration, comparons-les aux tests unitaires :

| Tests unitaires | Tests d'intégration |
|-----------------|---------------------|
| Testent des unités individuelles (fonctions, méthodes) | Testent l'interaction entre plusieurs unités |
| Isolent le code testé (via des mocks, stubs) | Utilisent les composants réels connectés ensemble |
| Rapides à exécuter | Généralement plus lents à exécuter |
| Faciles à maintenir | Plus complexes à maintenir |
| Identifient des bugs dans des composants spécifiques | Identifient des bugs d'interaction entre composants |

## Pourquoi faire des tests d'intégration ?

Les tests d'intégration offrent plusieurs avantages importants :

1. **Détection des problèmes d'interface** : Identifient les incompatibilités entre les différents modules.

2. **Validation du flux de données** : Vérifient que les données circulent correctement d'un composant à l'autre.

3. **Contrôle des dépendances externes** : Testent les interactions avec les bases de données, services web, et autres systèmes externes.

4. **Vérification des scénarios utilisateur** : Valident les fonctionnalités du point de vue de l'utilisateur final.

5. **Filet de sécurité** : Détectent les régressions qui pourraient survenir lors de modifications du code.

## Types de tests d'intégration

### 1. Tests d'intégration ascendants (Bottom-up)

On commence par tester les modules de bas niveau, puis on remonte progressivement vers les modules de plus haut niveau.

![Approche ascendante](https://via.placeholder.com/500x300)

### 2. Tests d'intégration descendants (Top-down)

On commence par tester les modules de haut niveau en simulant les modules de bas niveau, puis on intègre progressivement les modules réels de bas niveau.

![Approche descendante](https://via.placeholder.com/500x300)

### 3. Tests d'intégration sandwich (ou hybrides)

Cette approche combine les approches ascendante et descendante pour optimiser le processus de test.

### 4. Tests d'intégration big bang

Tous les modules sont intégrés simultanément pour former le système complet, puis testés ensemble.

## Mise en place des tests d'intégration avec Delphi

### Outils disponibles

Pour les tests d'intégration dans Delphi, vous pouvez utiliser :

1. **DUnitX** : Le même framework que pour les tests unitaires, mais configuré pour les tests d'intégration.

2. **TestInsight** : Un outil visuel intégré à l'IDE pour exécuter et visualiser les tests.

3. **FinalBuilder** : Un outil d'automatisation qui peut orchestrer l'exécution des tests d'intégration.

4. **Frameworks personnalisés** : Vous pouvez créer votre propre framework adapté à vos besoins spécifiques.

### Configuration d'un projet de test d'intégration

Voici les étapes pour configurer un projet de test d'intégration avec DUnitX :

1. **Créez un nouveau projet de test** :
   - **File > New > Other**
   - Sélectionnez **DUnitX > DUnitX Test Project**
   - Nommez votre projet (ex: "MonProjetTestsIntegration")

2. **Configurez le projet pour inclure les modules à tester** :
   - Ajoutez les unités nécessaires dans la clause `uses`
   - Configurez les chemins de recherche (**Project > Options > Delphi Compiler > Search Path**)

3. **Structurez vos tests** :
   - Créez des classes de test avec l'attribut `[TestFixture]`
   - Utilisez des attributs comme `[Setup]` et `[TearDown]` pour la préparation et le nettoyage
   - Définissez vos méthodes de test avec l'attribut `[Test]`

## Exemple de test d'intégration simple

Imaginons une application de gestion d'inventaire avec deux modules principaux :

1. `GestionProduits` : Gère les informations sur les produits
2. `GestionStocks` : Gère les quantités en stock

Voici un exemple de test d'intégration pour vérifier leur interaction :

```pascal
unit TestIntegrationInventaire;

interface

uses
  DUnitX.TestFramework,
  GestionProduits,
  GestionStocks,
  System.SysUtils;

type
  [TestFixture]
  TTestIntegrationInventaire = class
  private
    FGestionnaireProduits: TGestionnaireProduits;
    FGestionnaireStocks: TGestionnaireStocks;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAjoutProduitAvecStock;
    [Test]
    procedure TestMiseAJourStockApresModificationProduit;
    [Test]
    procedure TestSuppressionProduitAvecStockZero;
  end;

implementation

procedure TTestIntegrationInventaire.Setup;
begin
  // Créer des instances réelles (pas de mocks ici)
  FGestionnaireProduits := TGestionnaireProduits.Create;
  FGestionnaireStocks := TGestionnaireStocks.Create(FGestionnaireProduits);

  // Initialiser l'environnement de test
  // (ex: configurer une base de données de test)
end;

procedure TTestIntegrationInventaire.TearDown;
begin
  // Nettoyage
  FGestionnaireStocks.Free;
  FGestionnaireProduits.Free;

  // Nettoyage supplémentaire
  // (ex: réinitialiser la base de données de test)
end;

procedure TTestIntegrationInventaire.TestAjoutProduitAvecStock;
var
  IDProduit: Integer;
  StockInitial, StockFinal: Integer;
begin
  // Test d'intégration : Ajouter un produit devrait automatiquement
  // créer une entrée de stock avec quantité zéro

  // Action : Ajouter un nouveau produit
  IDProduit := FGestionnaireProduits.AjouterProduit('Écran 24"', 299.99);

  // Vérification : Le stock initial devrait être créé automatiquement à zéro
  StockInitial := FGestionnaireStocks.ObtenirQuantiteStock(IDProduit);
  Assert.AreEqual(0, StockInitial, 'Le stock initial devrait être zéro');

  // Action : Mettre à jour le stock
  FGestionnaireStocks.AjouterStock(IDProduit, 10);

  // Vérification : Le stock devrait être mis à jour
  StockFinal := FGestionnaireStocks.ObtenirQuantiteStock(IDProduit);
  Assert.AreEqual(10, StockFinal, 'Le stock devrait être 10 après ajout');
end;

procedure TTestIntegrationInventaire.TestMiseAJourStockApresModificationProduit;
var
  IDProduit: Integer;
  StockAvant, StockApres: Integer;
begin
  // Préparer : Ajouter un produit avec du stock
  IDProduit := FGestionnaireProduits.AjouterProduit('Clavier', 49.99);
  FGestionnaireStocks.AjouterStock(IDProduit, 5);

  // Vérifier le stock avant
  StockAvant := FGestionnaireStocks.ObtenirQuantiteStock(IDProduit);
  Assert.AreEqual(5, StockAvant, 'Le stock initial devrait être 5');

  // Action : Modifier le produit (par exemple, le rendre obsolète)
  FGestionnaireProduits.ModifierProduit(IDProduit, 'Clavier (Obsolète)', 29.99);

  // Vérification : Le stock ne devrait pas être affecté par la modification du produit
  StockApres := FGestionnaireStocks.ObtenirQuantiteStock(IDProduit);
  Assert.AreEqual(StockAvant, StockApres, 'Le stock ne devrait pas changer après modification du produit');
end;

procedure TTestIntegrationInventaire.TestSuppressionProduitAvecStockZero;
var
  IDProduit: Integer;
  ProduitExiste: Boolean;
begin
  // Préparer : Ajouter un produit avec stock zéro
  IDProduit := FGestionnaireProduits.AjouterProduit('Souris', 19.99);

  // Action : Supprimer le produit
  FGestionnaireProduits.SupprimerProduit(IDProduit);

  // Vérification : Le produit ne devrait plus exister
  ProduitExiste := FGestionnaireProduits.ProduitExiste(IDProduit);
  Assert.IsFalse(ProduitExiste, 'Le produit devrait être supprimé');

  // Vérification : La tentative d'obtenir le stock devrait lever une exception
  Assert.WillRaise(
    procedure
    begin
      FGestionnaireStocks.ObtenirQuantiteStock(IDProduit);
    end,
    EProduitInexistant,
    'Une exception devrait être levée pour un produit inexistant'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationInventaire);

end.
```

Cet exemple illustre plusieurs scénarios d'intégration entre les modules de gestion des produits et des stocks.

## Tests d'intégration avec des dépendances externes

Les applications réelles interagissent souvent avec des systèmes externes comme les bases de données, les services web ou les systèmes de fichiers. Pour les tests d'intégration, nous avons plusieurs approches :

### 1. Utilisation d'environnements de test dédiés

```pascal
[Setup]
procedure TTestIntegrationBD.Setup;
var
  ConnexionChaine: string;
begin
  // Utiliser une base de données de test dédiée
  ConnexionChaine := 'Server=localhost;Database=TestInventaire;User=test;Password=test;';

  // Initialiser la connexion
  FConnexion := TConnexionBD.Create(ConnexionChaine);

  // Réinitialiser la base de données de test
  FConnexion.ExecuterScript('scripts/reinitialisation_test.sql');

  // Créer les composants avec la connexion de test
  FGestionnaireProduits := TGestionnaireProduits.Create(FConnexion);
  FGestionnaireStocks := TGestionnaireStocks.Create(FConnexion, FGestionnaireProduits);
end;
```

### 2. Utilisation de bases de données en mémoire

Pour des tests plus rapides, vous pouvez utiliser des bases de données en mémoire comme SQLite :

```pascal
[Setup]
procedure TTestIntegrationBD.Setup;
begin
  // Créer une base de données SQLite en mémoire
  FConnexion := TFDConnection.Create(nil);
  FConnexion.DriverName := 'SQLite';
  FConnexion.Params.Values['Database'] := ':memory:';
  FConnexion.Connected := True;

  // Créer la structure de la base de données
  FConnexion.ExecSQL('CREATE TABLE Produits (ID INTEGER PRIMARY KEY, Nom TEXT, Prix REAL)');
  FConnexion.ExecSQL('CREATE TABLE Stocks (ProduitID INTEGER, Quantite INTEGER)');

  // Initialiser les composants
  FGestionnaireProduits := TGestionnaireProduits.Create(FConnexion);
  FGestionnaireStocks := TGestionnaireStocks.Create(FConnexion, FGestionnaireProduits);
end;
```

### 3. Utilisation de services de test simulés

Pour les services externes, vous pouvez créer des serveurs de test légers :

```pascal
[Setup]
procedure TTestIntegrationAPI.Setup;
begin
  // Démarrer un serveur HTTP de test sur un port local
  FServeurTest := THTTPTestServer.Create(8080);
  FServeurTest.AddEndpoint('/api/produits', HandleRequeteProduits);
  FServeurTest.Start;

  // Configurer le client pour utiliser le serveur de test
  FClient := TAPIClient.Create('http://localhost:8080/api');

  // Initialiser les composants avec le client de test
  FGestionnaireProduits := TGestionnaireProduits.Create(FClient);
end;

[TearDown]
procedure TTestIntegrationAPI.TearDown;
begin
  FGestionnaireProduits.Free;
  FClient.Free;
  FServeurTest.Stop;
  FServeurTest.Free;
end;
```

## Techniques avancées pour les tests d'intégration

### 1. Tests d'intégration automatisés avec scripts

Vous pouvez créer des scripts pour automatiser l'exécution des tests d'intégration :

```pascal
procedure ExecuterTestsIntegration;
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
begin
  // Préparation de l'environnement
  PreparerEnvironnementTest;

  try
    // Création du runner de test
    Runner := TDUnitX.CreateRunner;
    Logger := TDUnitXConsoleLogger.Create(true);
    Runner.AddLogger(Logger);

    // Exécution des tests
    Results := Runner.Execute;

    // Traitement des résultats
    if not Results.AllPassed then
    begin
      WriteLn('Des tests ont échoué !');
      ExitCode := 1;
    end
    else
    begin
      WriteLn('Tous les tests ont réussi !');
      ExitCode := 0;
    end;
  finally
    // Nettoyage de l'environnement
    NettoyerEnvironnementTest;
  end;
end;
```

### 2. Tests d'intégration avec des scénarios complexes

Pour les flux de travail complexes, utilisez des scénarios complets :

```pascal
[Test]
procedure TTestIntegrationWorkflow.TestProcessusCommandeComplet;
var
  Client: TClient;
  Produit: TProduit;
  Commande: TCommande;
  Paiement: TPaiement;
  Livraison: TLivraison;
  Statut: string;
begin
  // Étape 1 : Créer un client
  Client := FGestionnaireClients.CreerClient('Jean Dupont', 'jean@exemple.com');

  // Étape 2 : Ajouter un produit avec stock
  Produit := FGestionnaireProduits.AjouterProduit('Ordinateur portable', 999.99);
  FGestionnaireStocks.AjouterStock(Produit.ID, 5);

  // Étape 3 : Créer une commande
  Commande := FGestionnaireCommandes.CreerCommande(Client.ID);
  FGestionnaireCommandes.AjouterLigneCommande(Commande.ID, Produit.ID, 1);

  // Étape 4 : Valider la commande
  FGestionnaireCommandes.ValiderCommande(Commande.ID);

  // Étape 5 : Créer un paiement
  Paiement := FGestionnairePaiements.EnregistrerPaiement(Commande.ID, 999.99);

  // Étape 6 : Créer une livraison
  Livraison := FGestionnaireLivraisons.CreerLivraison(Commande.ID, 'Adresse de livraison');

  // Vérification finale : statut de la commande
  Statut := FGestionnaireCommandes.ObtenirStatutCommande(Commande.ID);
  Assert.AreEqual('Payée', Statut, 'La commande devrait être en statut Payée');

  // Vérification du stock
  Assert.AreEqual(4, FGestionnaireStocks.ObtenirQuantiteStock(Produit.ID),
    'Le stock devrait être réduit de 1');

  // Vérification de la livraison
  Assert.AreEqual('En préparation', FGestionnaireLivraisons.ObtenirStatut(Livraison.ID),
    'La livraison devrait être en préparation');
end;
```

## Intégration continue et tests d'intégration

Les tests d'intégration s'intègrent parfaitement dans un système d'intégration continue (CI) :

### Configuration avec Jenkins ou GitLab CI

1. **Créez un script de build** qui compile votre application et exécute les tests d'intégration.

2. **Configurez votre pipeline CI** pour exécuter ce script après chaque commit ou à intervalles réguliers.

3. **Générez des rapports de test** au format XML pour une visualisation facile des résultats.

Exemple de script pour GitLab CI (.gitlab-ci.yml) :

```yaml
stages:
  - build
  - test

build:
  stage: build
  script:
    - "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
    - msbuild MonProjet.dproj /t:Build /p:Config=Release

integration_tests:
  stage: test
  script:
    - "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
    - msbuild TestsIntegration.dproj /t:Build /p:Config=Debug
    - .\bin\Debug\TestsIntegration.exe -xml=rapport_tests.xml
  artifacts:
    paths:
      - rapport_tests.xml
```

## Bonnes pratiques pour les tests d'intégration

### 1. Indépendance des tests

Chaque test d'intégration doit être indépendant des autres. Utilisez `Setup` et `TearDown` pour garantir un environnement propre.

```pascal
[Setup]
procedure TTestIntegration.Setup;
begin
  // Réinitialiser l'environnement de test
  // ...
end;

[TearDown]
procedure TTestIntegration.TearDown;
begin
  // Nettoyer après le test
  // ...
end;
```

### 2. Tests déterministes

Les tests doivent produire les mêmes résultats à chaque exécution. Évitez les dépendances aux données aléatoires ou à l'heure système.

```pascal
// Moins bon - résultat non déterministe
function GenererId: Integer;
begin
  Result := Random(1000);
end;

// Meilleur - résultat prévisible
function GenererId: Integer;
begin
  // Utiliser un compteur ou un ID prédéfini pour les tests
  Result := FCompteurId;
  Inc(FCompteurId);
end;
```

### 3. Tests lisibles et maintenables

Utilisez des noms de test descriptifs et structurez le code avec le pattern AAA (Arrange-Act-Assert) :

```pascal
[Test]
procedure TTestIntegration.TestAjoutProduitDeclencheNotificationStock;
begin
  // Arrange - Préparation
  var ObservateurMock := TMockObservateurStock.Create;
  FGestionnaireStocks.AjouterObservateur(ObservateurMock);

  // Act - Action
  FGestionnaireProduits.AjouterProduit('Nouveau Produit', 99.99);

  // Assert - Vérification
  Assert.IsTrue(ObservateurMock.AEteNotifie,
    'L''observateur de stock devrait être notifié lors de l''ajout d''un produit');
end;
```

### 4. Tests ciblés

Concentrez chaque test sur un scénario d'intégration spécifique. Évitez les tests qui tentent de tout tester en même temps.

### 5. Gestion des configurations

Utilisez des fichiers de configuration externes pour les paramètres de test :

```pascal
procedure ChargerConfigurationTest;
var
  ConfigFile: string;
  Config: TJSONObject;
begin
  ConfigFile := ExtractFilePath(ParamStr(0)) + 'config_test.json';
  if FileExists(ConfigFile) then
  begin
    Config := TJSONObject.ParseJSONValue(
      TFile.ReadAllText(ConfigFile)) as TJSONObject;
    try
      FDBHost := Config.GetValue('db_host').Value;
      FDBName := Config.GetValue('db_name').Value;
      // ...
    finally
      Config.Free;
    end;
  end;
end;
```

## Évolution vers Delphi 12 Athens

> 💡 **Nécessite Delphi 12 ou supérieur**

Delphi 12 Athens apporte plusieurs améliorations pour les tests d'intégration :

### 1. Support amélioré des conteneurs de test

```pascal
// Utilisation de conteneurs Docker pour les tests d'intégration
[TestFixture]
[DockerContainer('mysql:8.0', 'Port=3306:3306;Env=MYSQL_ROOT_PASSWORD=password;Env=MYSQL_DATABASE=testdb')]
TTestIntegrationMySQL = class
  // Tests d'intégration avec MySQL
end;
```

### 2. Mocks améliorés pour les composants d'interface utilisateur

```pascal
// Test d'intégration impliquant des composants UI
[Test]
procedure TTestIntegrationUI.TestAffichageListeProduits;
var
  FormMock: TMockForm;
  ListView: TListView;
begin
  // Créer un mock de formulaire
  FormMock := TMockForm.Create(nil);
  try
    ListView := TListView.Create(FormMock);
    ListView.Parent := FormMock;

    // Test d'intégration entre le gestionnaire de produits et l'interface
    FGestionnaireUI.AfficherProduits(ListView);

    // Vérifications
    Assert.AreEqual(3, ListView.Items.Count, 'La liste devrait contenir 3 produits');
    Assert.AreEqual('Produit A', ListView.Items[0].Caption, 'Premier produit incorrect');
  finally
    FormMock.Free;
  end;
end;
```

## Conclusion

Les tests d'intégration constituent une couche essentielle dans votre stratégie de test, complémentaire aux tests unitaires. Ils vous permettent de valider que les différents composants de votre application fonctionnent correctement ensemble et interagissent comme prévu avec les systèmes externes.

Bien que plus complexes à mettre en place que les tests unitaires, ils offrent une garantie plus forte sur la qualité globale de votre application et peuvent détecter des problèmes qui échapperaient aux tests unitaires.

En suivant les bonnes pratiques présentées dans cette section et en adaptant les techniques à votre contexte spécifique, vous pourrez mettre en place une stratégie de test d'intégration efficace qui contribuera grandement à la qualité et à la robustesse de vos applications Delphi.

Dans la prochaine section, nous explorerons le mocking et les tests avec dépendances, des techniques qui vous permettront d'isoler votre code pour des tests plus précis et plus contrôlés.

⏭️ [Mocking et tests avec dépendances](/12-debogage-et-tests/08-mocking-et-tests-avec-dependances.md)
