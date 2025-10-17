🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.14 Améliorations FireDAC de Delphi 13

## Introduction

**Delphi 13 Florence** apporte son lot d'améliorations et de nouvelles fonctionnalités à FireDAC, le framework d'accès aux données de référence d'Embarcadero. Ces améliorations visent à simplifier le développement, améliorer les performances et étendre le support des bases de données modernes.

Dans ce chapitre, nous allons explorer les nouveautés de FireDAC dans Delphi 13 et voir comment elles peuvent améliorer vos applications.

## Vue d'ensemble des nouveautés

### Principales améliorations

```
┌─────────────────────────────────────────────┐
│      AMÉLIORATIONS FireDAC DELPHI 13        │
├─────────────────────────────────────────────┤
│  ✨ Performance                             │
│  • Optimisations des requêtes               │
│  • Gestion mémoire améliorée                │
│  • Pool de connexions optimisé              │
├─────────────────────────────────────────────┤
│  🔧 Fonctionnalités                         │
│  • Support étendu JSON                      │
│  • Nouvelles options de paramètres          │
│  • Améliorations des transactions           │
├─────────────────────────────────────────────┤
│  🗄️ Support bases de données                │
│  • Drivers mis à jour                       │
│  • Support versions récentes                │
│  • Nouvelles bases supportées               │
├─────────────────────────────────────────────┤
│  🛠️ Outils de développement                 │
│  • Éditeur de requêtes amélioré             │
│  • Débogage facilité                        │
│  • Assistance IA pour FireDAC               │
└─────────────────────────────────────────────┘
```

## Performance et optimisations

### 1. Optimisation du pooling de connexions

Delphi 13 améliore la gestion du pool de connexions pour de meilleures performances dans les applications multi-utilisateurs.

```pascal
// Configuration améliorée du pool
procedure ConfigurerPoolOptimise;
begin
  FDConnection1.Params.Add('Pooled=True');
  FDConnection1.Params.Add('POOL_MaximumItems=50');  // Nouveau : limite max
  FDConnection1.Params.Add('POOL_CleanupTimeout=60000');  // Nettoyage optimisé
  FDConnection1.Params.Add('POOL_ExpireTimeout=90000');  // Expiration intelligente
end;
```

**Nouveautés :**
- ✅ Gestion automatique de la taille du pool
- ✅ Recyclage intelligent des connexions
- ✅ Monitoring intégré du pool
- ✅ Détection des connexions mortes

### Monitoring du pool

```pascal
// Nouvelle propriété : statistiques du pool
procedure AfficherStatistiquesPool;
begin
  ShowMessage(Format(
    'Connexions actives: %d' + sLineBreak +
    'Connexions disponibles: %d' + sLineBreak +
    'Connexions max: %d',
    [FDConnection1.ConnectionMetaData.ActiveConnections,
     FDConnection1.ConnectionMetaData.IdleConnections,
     FDConnection1.ConnectionMetaData.MaxConnections]
  ));
end;
```

### 2. Optimisation des requêtes préparées

```pascal
// Cache étendu des requêtes préparées
procedure TdmDatabase.ConfigurerCacheRequetes;
begin
  // Nouveau dans Delphi 13 : cache de requêtes plus intelligent
  FDConnection1.Params.Add('MetaCacheSize=1000');  // Cache augmenté
  FDConnection1.Params.Add('MetaCacheTime=300');   // Durée de cache

  FDQuery1.CachedUpdates := True;
  FDQuery1.FetchOptions.Cache := [fiBlobs, fiDetails];  // Cache enrichi
end;
```

**Amélioration :**
- Réutilisation optimale des plans d'exécution
- Cache partagé entre connexions
- Invalidation intelligente du cache

### 3. Gestion mémoire améliorée

```pascal
// Nouvelle option : gestion mémoire large dataset
procedure ConfigurerGestionMemoire;
begin
  FDQuery1.FetchOptions.Mode := fmOnDemand;  // Chargement à la demande
  FDQuery1.FetchOptions.RowsetSize := 100;   // Taille du lot optimisée
  FDQuery1.FetchOptions.RecordCountMode := cmTotal;  // Comptage efficace

  // Nouveau : libération automatique de mémoire
  FDQuery1.FetchOptions.AutoFetchAll := afDisable;
  FDQuery1.FetchOptions.Unidirectional := True;  // Mode unidirectionnel pour économie mémoire
end;
```

## Support JSON étendu

### 1. Lecture et écriture JSON native

Delphi 13 améliore significativement le support JSON dans FireDAC.

```pascal
uses
  System.JSON, FireDAC.Comp.DataSet;

// Exporter un dataset en JSON (amélioré)
function DataSetVersJSON(Query: TFDQuery): string;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONArray.Create;
  try
    // Nouvelle méthode optimisée
    Query.SaveToJSON(JSONArray, True);  // True = inclure métadonnées
    Result := JSONArray.ToJSON;
  finally
    JSONArray.Free;
  end;
end;

// Charger JSON dans un dataset (amélioré)
procedure JSONVersDataSet(const JSON: string; Query: TFDQuery);
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    Query.Close;
    Query.LoadFromJSON(JSONArray, True);  // True = charger métadonnées
  finally
    JSONArray.Free;
  end;
end;
```

### 2. Filtrage et manipulation JSON

```pascal
// Nouveau : filtrage sur colonnes JSON (PostgreSQL, MySQL 8+)
procedure FiltrerSurJSON;
begin
  FDQuery1.SQL.Text :=
    'SELECT * FROM produits ' +
    'WHERE JSON_EXTRACT(attributs, ''$.couleur'') = :couleur';

  FDQuery1.ParamByName('couleur').AsString := 'rouge';
  FDQuery1.Open;
end;

// Support amélioré des types JSON natifs
procedure ManipulerJSONNatif;
var
  JSONData: TJSONObject;
begin
  FDQuery1.Open;

  // Extraction directe en objet JSON
  JSONData := FDQuery1.FieldByName('data').AsJSON as TJSONObject;
  try
    ShowMessage(JSONData.GetValue<string>('nom'));
  finally
    JSONData.Free;
  end;
end;
```

## Transactions améliorées

### 1. Transactions imbriquées simplifiées

```pascal
// Nouveau : support amélioré des savepoints
procedure TransactionAvecSavepoint;
begin
  FDConnection1.StartTransaction;
  try
    // Opération 1
    Query1.ExecSQL;

    // Créer un savepoint
    FDConnection1.SavePoint('point1');

    try
      // Opération 2 (peut échouer)
      Query2.ExecSQL;
    except
      // Revenir au savepoint sans annuler toute la transaction
      FDConnection1.RollbackToSavePoint('point1');
      // Continuer avec la transaction principale
    end;

    // Opération 3
    Query3.ExecSQL;

    FDConnection1.Commit;
  except
    FDConnection1.Rollback;
    raise;
  end;
end;
```

### 2. Gestion automatique des deadlocks

```pascal
// Nouveau : retry automatique sur deadlock
procedure ConfigurerRetryDeadlock;
begin
  FDConnection1.Params.Add('AutoReconnect=True');
  FDConnection1.Params.Add('RetryOnDeadlock=3');  // Réessayer 3 fois
  FDConnection1.Params.Add('RetryDelay=1000');    // Attendre 1 seconde entre tentatives
end;

// Les deadlocks sont maintenant gérés automatiquement
procedure ExecuterAvecRetry;
begin
  FDConnection1.StartTransaction;
  try
    Query1.ExecSQL;  // FireDAC réessaiera automatiquement si deadlock
    FDConnection1.Commit;
  except
    FDConnection1.Rollback;
    raise;
  end;
end;
```

## Support bases de données étendu

### 1. Mises à jour des drivers

| Base de données | Versions supportées | Nouveautés Delphi 13 |
|-----------------|---------------------|----------------------|
| **MySQL** | 8.0, 8.1, 8.2 | Support protocole x |
| **PostgreSQL** | 13, 14, 15, 16 | Réplication logique |
| **SQL Server** | 2019, 2022 | Always Encrypted |
| **Oracle** | 19c, 21c, 23c | JSON étendu |
| **MongoDB** | 5.x, 6.x, 7.x | Transactions multi-documents |
| **SQLite** | 3.42+ | JSON amélioré |

### 2. Support MongoDB amélioré

```pascal
// Nouveau : support transactions MongoDB
procedure TransactionMongoDB;
var
  MongoQuery: TFDMongoQuery;
begin
  MongoQuery := TFDMongoQuery.Create(nil);
  try
    MongoQuery.Connection := FDConnection1;

    // Nouveau : support des transactions multi-documents
    FDConnection1.StartTransaction;
    try
      // Document 1
      MongoQuery.DatabaseName := 'ma_base';
      MongoQuery.CollectionName := 'clients';
      MongoQuery.QInsert := '{"nom": "Dupont", "solde": 1000}';
      MongoQuery.Execute;

      // Document 2 (autre collection)
      MongoQuery.CollectionName := 'transactions';
      MongoQuery.QInsert := '{"client": "Dupont", "montant": -100}';
      MongoQuery.Execute;

      FDConnection1.Commit;
    except
      FDConnection1.Rollback;
      raise;
    end;
  finally
    MongoQuery.Free;
  end;
end;
```

### 3. Support PostgreSQL étendu

```pascal
// Nouveau : support des fonctionnalités PostgreSQL avancées
procedure UtiliserFonctionnalitesPostgreSQL;
begin
  // Support des types personnalisés
  Query.SQL.Text :=
    'CREATE TYPE adresse AS (' +
    '  rue VARCHAR(100),' +
    '  ville VARCHAR(50),' +
    '  code_postal VARCHAR(10)' +
    ')';
  Query.ExecSQL;

  // Support JSONB amélioré
  Query.SQL.Text :=
    'SELECT * FROM produits ' +
    'WHERE specs @> ''{"couleur": "rouge"}''';  // Opérateur JSONB natif
  Query.Open;

  // Support des tableaux natifs
  Query.SQL.Text :=
    'SELECT * FROM clients ' +
    'WHERE :tag = ANY(tags)';  // Recherche dans tableau
  Query.ParamByName('tag').AsString := 'premium';
  Query.Open;
end;
```

## Nouvelles options de paramètres

### 1. Types de paramètres étendus

```pascal
// Support de nouveaux types de données
procedure NouveauxTypesParametres;
begin
  FDQuery1.SQL.Text :=
    'INSERT INTO clients (nom, donnees_json, tags, geo_point) ' +
    'VALUES (:nom, :json, :tags, :geo)';

  // Type standard
  FDQuery1.ParamByName('nom').AsString := 'Dupont';

  // JSON natif (nouveau)
  FDQuery1.ParamByName('json').AsJSON :=
    '{"age": 35, "ville": "Paris"}';

  // Tableau (nouveau pour PostgreSQL)
  FDQuery1.ParamByName('tags').AsArray := ['client', 'premium', 'actif'];

  // Point géographique (nouveau pour PostGIS)
  FDQuery1.ParamByName('geo').AsGeoPoint := PointF(48.8566, 2.3522);  // Paris

  FDQuery1.ExecSQL;
end;
```

### 2. Paramètres nommés améliorés

```pascal
// Nouveau : paramètres avec valeurs par défaut
procedure ParametresAvecDefaut;
begin
  FDQuery1.SQL.Text :=
    'SELECT * FROM clients ' +
    'WHERE (actif = :actif OR :actif IS NULL) ' +
    'AND (ville = :ville OR :ville IS NULL)';

  // Nouveau : définir des valeurs par défaut
  FDQuery1.ParamByName('actif').DefaultValue := True;
  FDQuery1.ParamByName('ville').DefaultValue := Null;

  // Si non assigné, utilise la valeur par défaut
  FDQuery1.Open;
end;
```

## Outils de développement améliorés

### 1. Éditeur de requêtes enrichi

L'éditeur SQL dans l'IDE a été amélioré :

- ✅ **Coloration syntaxique** améliorée
- ✅ **Auto-complétion** des tables et champs
- ✅ **Analyse syntaxique** en temps réel
- ✅ **Suggestions** basées sur le schéma de base
- ✅ **Formatage automatique** du code SQL

```pascal
// Dans l'IDE, l'éditeur propose maintenant :
// • Complétion automatique des noms de tables
// • Suggestions de colonnes disponibles
// • Détection des erreurs de syntaxe
// • Aperçu des résultats

// Exemple : en tapant "SELECT * FROM cl"
// L'IDE propose : clients, clients_archive, classes, etc.
```

### 2. Débogage amélioré

```pascal
// Nouveau : breakpoints conditionnels sur les données
procedure DeboguerRequetes;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients';
  FDQuery1.Open;

  // Dans l'IDE, possibilité de :
  // • Inspecter les paramètres en temps réel
  // • Voir le SQL généré avec les valeurs
  // • Tracer l'exécution des requêtes
  // • Analyser les temps d'exécution

  FDQuery1.First;
  while not FDQuery1.Eof do
  begin
    // Breakpoint conditionnel : s'arrêter si solde < 0
    if FDQuery1.FieldByName('solde').AsCurrency < 0 then
      DebugBreak;  // L'IDE peut maintenant s'arrêter ici conditionnellement

    FDQuery1.Next;
  end;
end;
```

### 3. Profiler de requêtes intégré

```pascal
// Nouveau : profiling automatique des requêtes
procedure ProfilerRequetes;
begin
  // Activer le profiling
  FDConnection1.Params.Add('MonitorBy=Flat');  // Nouveau
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE ville = :ville';
  FDQuery1.ParamByName('ville').AsString := 'Paris';

  FDQuery1.Open;

  // Statistiques automatiques
  ShowMessage(Format(
    'Temps d''exécution: %d ms' + sLineBreak +
    'Lignes retournées: %d' + sLineBreak +
    'Mémoire utilisée: %d Ko',
    [FDQuery1.ExecTime,
     FDQuery1.RecordCount,
     FDQuery1.MemoryUsage div 1024]
  ));
end;
```

## Assistance IA pour FireDAC

### 1. Génération de code assistée par IA

Delphi 13 intègre une **assistance IA** pour FireDAC via le site companion.

```pascal
// L'IA peut maintenant vous aider à :
// • Générer des requêtes SQL complexes
// • Optimiser les performances
// • Suggérer des index
// • Détecter les problèmes de sécurité (injections SQL)
// • Proposer des refactorisations

// Exemple dans l'IDE :
// Vous tapez : "// Créer une requête pour les clients parisiens actifs"
// L'IA suggère :
(*
  FDQuery1.SQL.Text :=
    'SELECT id, nom, prenom, email ' +
    'FROM clients ' +
    'WHERE ville = :ville AND actif = :actif ' +
    'ORDER BY nom';
  FDQuery1.ParamByName('ville').AsString := 'Paris';
  FDQuery1.ParamByName('actif').AsBoolean := True;
  FDQuery1.Open;
*)
```

### 2. Détection automatique des problèmes

```pascal
// L'IA détecte maintenant les problèmes potentiels
procedure CodeAvecProblemes;
begin
  // ⚠️ L'IDE avertit : "Risque d'injection SQL détecté"
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE nom = ''' + editNom.Text + '''';

  // ✅ L'IA suggère la correction :
  // FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE nom = :nom';
  // FDQuery1.ParamByName('nom').AsString := editNom.Text;
end;
```

## Batch Updates optimisés

### Support amélioré des mises à jour par lots

```pascal
// Nouveau : batch updates plus performants
procedure MajMassive;
var
  i: Integer;
begin
  FDQuery1.SQL.Text :=
    'UPDATE clients SET derniere_visite = :date WHERE id = :id';

  // Nouveau mode batch
  FDQuery1.Params.ArraySize := 1000;  // Traiter par lots de 1000

  // Préparer les données
  for i := 0 to 999 do
  begin
    FDQuery1.Params[0].AsDateTimes[i] := Now;
    FDQuery1.Params[1].AsIntegers[i] := i + 1;
  end;

  // Exécuter en un seul appel réseau
  FDQuery1.Execute(1000, 0);  // Beaucoup plus rapide !
end;
```

**Avantages :**
- ✅ Jusqu'à **10x plus rapide** pour insertions massives
- ✅ Réduction du trafic réseau
- ✅ Utilisation optimale des ressources serveur

## Améliorations Live Bindings

### 1. Performance accrue

```pascal
// Les Live Bindings sont maintenant plus rapides
// et consomment moins de mémoire

// Configuration optimisée
BindSourceDB1.DataSet := FDQuery1;
BindSourceDB1.AutoBufferCount := 50;  // Nouveau : buffer optimisé

// Liaison optimisée pour grandes listes
LinkListControlToField1.BufferCount := 100;  // Plus efficace
LinkListControlToField1.Active := True;
```

### 2. Expressions étendues

```pascal
// Nouvelles fonctions dans les expressions Live Bindings

// Formatage avancé
Expression := 'FormatCurrency(prix * quantite)';

// Fonctions de chaînes étendues
Expression := 'UpperCase(Left(nom, 1)) + LowerCase(Copy(nom, 2))';

// Dates améliorées
Expression := 'FormatDateTime(''dd/mm/yyyy'', date_creation)';

// Conditions ternaires (nouveau)
Expression := 'IIF(actif, ''Actif'', ''Inactif'')';
```

## Migration depuis Delphi 12

### Compatibilité ascendante

**Bonne nouvelle :** Le code FireDAC existant fonctionne sans modification dans Delphi 13.

```pascal
// Code Delphi 12 → fonctionne tel quel dans Delphi 13
FDConnection1.Params.Add('DriverID=MySQL');
FDConnection1.Params.Add('Server=localhost');
FDConnection1.Connected := True;

FDQuery1.SQL.Text := 'SELECT * FROM clients';
FDQuery1.Open;
```

### Nouvelles propriétés optionnelles

```pascal
// Vous pouvez adopter progressivement les nouveautés

// Ancien code (fonctionne toujours)
FDQuery1.Open;

// Nouveau code (optionnel, pour profiter des optimisations)
FDQuery1.FetchOptions.Mode := fmOnDemand;
FDQuery1.FetchOptions.Cache := [fiBlobs, fiDetails];
FDQuery1.Open;
```

### Checklist de migration

- [ ] Tester l'application sans modification
- [ ] Mettre à jour les drivers de bases de données
- [ ] Activer les nouveaux paramètres de performance
- [ ] Utiliser le profiler pour identifier les optimisations possibles
- [ ] Adopter progressivement les nouvelles fonctionnalités

## Exemple complet : Application optimisée Delphi 13

```pascal
unit uDatabaseDelphi13;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  FireDAC.Stan.Intf, FireDAC.Comp.Client, FireDAC.Phys.MySQL;

type
  TDatabaseDelphi13 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDQuery1: TFDQuery;
  public
    procedure ConfigurerAvecOptimisations;
    procedure ExempleRequeteOptimisee;
    procedure ExempleBatchUpdate;
    function ExporterJSON: string;
  end;

implementation

procedure TDatabaseDelphi13.ConfigurerAvecOptimisations;
begin
  // Utiliser les optimisations Delphi 13
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');

  // Pool optimisé
  FDConnection1.Params.Add('Pooled=True');
  FDConnection1.Params.Add('POOL_MaximumItems=50');
  FDConnection1.Params.Add('POOL_CleanupTimeout=60000');

  // Cache étendu
  FDConnection1.Params.Add('MetaCacheSize=1000');

  // Retry automatique
  FDConnection1.Params.Add('AutoReconnect=True');
  FDConnection1.Params.Add('RetryOnDeadlock=3');

  FDConnection1.Connected := True;
end;

procedure TDatabaseDelphi13.ExempleRequeteOptimisee;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE ville = :ville';

  // Options de fetch optimisées
  FDQuery1.FetchOptions.Mode := fmOnDemand;
  FDQuery1.FetchOptions.RowsetSize := 100;
  FDQuery1.FetchOptions.RecordCountMode := cmTotal;
  FDQuery1.FetchOptions.AutoFetchAll := afDisable;

  FDQuery1.ParamByName('ville').AsString := 'Paris';
  FDQuery1.Open;

  // Profiling automatique
  ShowMessage(Format('Temps: %d ms, Lignes: %d',
    [FDQuery1.ExecTime, FDQuery1.RecordCount]));
end;

procedure TDatabaseDelphi13.ExempleBatchUpdate;
var
  i: Integer;
begin
  FDQuery1.SQL.Text :=
    'UPDATE clients SET statut = :statut WHERE id = :id';

  // Batch optimisé
  FDQuery1.Params.ArraySize := 100;

  for i := 0 to 99 do
  begin
    FDQuery1.Params[0].AsStrings[i] := 'actif';
    FDQuery1.Params[1].AsIntegers[i] := i + 1;
  end;

  FDQuery1.Execute(100, 0);
end;

function TDatabaseDelphi13.ExporterJSON: string;
var
  JSONArray: TJSONArray;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients';
  FDQuery1.Open;

  JSONArray := TJSONArray.Create;
  try
    // Export JSON optimisé Delphi 13
    FDQuery1.SaveToJSON(JSONArray, True);
    Result := JSONArray.ToJSON;
  finally
    JSONArray.Free;
  end;
end;

end.
```

## Ressources et documentation

### Documentation officielle

- **DocWiki Embarcadero** : Documentation complète de FireDAC
- **Site companion IA** : Assistance pour FireDAC dans Delphi 13
- **Exemples de code** : Dossier samples\FireDAC dans l'installation Delphi

### Communauté

- **Forums Embarcadero** : Questions et réponses
- **Stack Overflow** : Tag `delphi` et `firedac`
- **Blogs Delphi** : Articles sur les nouveautés

## Résumé

### Points clés de Delphi 13

✅ **Performance** : Pool optimisé, cache étendu, batch updates
✅ **JSON** : Support natif amélioré, export/import facilité
✅ **Transactions** : Savepoints, retry automatique sur deadlock
✅ **Bases de données** : Drivers mis à jour, support versions récentes
✅ **Outils** : Éditeur SQL enrichi, profiler intégré
✅ **IA** : Assistance au développement, détection de problèmes
✅ **Compatibilité** : Code existant fonctionne sans modification

### Améliorations majeures

| Domaine | Amélioration | Impact |
|---------|-------------|--------|
| **Performance** | Pool et cache optimisés | +30% vitesse |
| **JSON** | Support natif étendu | Développement simplifié |
| **Transactions** | Retry automatique | Robustesse accrue |
| **Outils** | Profiler intégré | Optimisation facilitée |
| **IA** | Assistance code | Productivité +50% |

### Recommandations

**Pour nouveaux projets :**
- Utilisez toutes les nouvelles fonctionnalités
- Activez le pooling optimisé
- Profitez de l'assistance IA

**Pour projets existants :**
- Testez d'abord sans modification
- Adoptez progressivement les optimisations
- Utilisez le profiler pour identifier les améliorations

## Conclusion

Delphi 13 Florence apporte des améliorations significatives à FireDAC, rendant le développement d'applications de bases de données encore plus efficace et agréable. Les optimisations de performance, le support JSON étendu et l'assistance IA sont les points forts de cette version.

**Que vous débutiez avec FireDAC ou que vous soyez un développeur expérimenté**, ces améliorations vous permettront de créer des applications plus rapides, plus robustes et plus faciles à maintenir.

N'hésitez pas à explorer les nouvelles fonctionnalités progressivement et à tirer parti de l'assistance IA pour accélérer votre développement. FireDAC dans Delphi 13 représente l'état de l'art de l'accès aux données dans le monde Delphi !

**Bon développement avec Delphi 13 et FireDAC ! 🚀**

⏭️ [Rapports et impressions](/09-rapports-et-impressions/README.md)
