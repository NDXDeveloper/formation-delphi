🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.2 Optimisation du code final

## Introduction

Une fois votre application compilée en mode Release, vous pouvez encore améliorer ses performances et réduire sa taille grâce à diverses techniques d'optimisation. L'objectif est d'offrir à vos utilisateurs la meilleure expérience possible : une application rapide, légère et réactive.

L'optimisation est un art délicat qui nécessite un équilibre entre performances, maintenabilité du code et temps de développement. Ce chapitre vous guidera à travers les différentes techniques d'optimisation disponibles avec Delphi 13, en vous aidant à identifier où concentrer vos efforts pour obtenir les meilleurs résultats.

## Principe fondamental : mesurer avant d'optimiser

### La règle d'or

> "L'optimisation prématurée est la racine de tous les maux" - Donald Knuth

Avant de commencer à optimiser, suivez toujours cette règle :

1. **Mesurer** : Identifiez où se trouvent réellement les problèmes de performance
2. **Optimiser** : Appliquez des optimisations ciblées
3. **Mesurer à nouveau** : Vérifiez que vos optimisations ont eu l'effet escompté

Ne perdez pas de temps à optimiser du code qui s'exécute rapidement. Concentrez-vous sur les véritables goulots d'étranglement.

### Utiliser les outils de profilage

Delphi dispose d'outils intégrés pour mesurer les performances :

- **Sampling** : Permet de voir quelles fonctions consomment le plus de temps CPU
- **Instrumentation** : Mesure précise du temps d'exécution de chaque fonction
- **Compteurs de performances** : Pour surveiller mémoire, threads, etc.

Pour accéder au profileur dans Delphi 13 :
- Menu `Exécuter` → `Paramètres de démarrage` → Onglet `Sampling`
- Activez le profilage et exécutez votre application
- Analysez les résultats pour identifier les zones lentes

## Types d'optimisations

Il existe deux grandes catégories d'optimisations :

### 1. Optimisations du compilateur

Le compilateur Delphi peut automatiquement améliorer votre code. Ces optimisations sont activées en mode Release.

### 2. Optimisations manuelles

Ce sont les améliorations que vous apportez vous-même au code source pour le rendre plus efficace.

Nous allons explorer les deux approches.

## Optimisations du compilateur Delphi

### Paramètres de compilation avancés

Au-delà des paramètres Release de base, vous pouvez affiner les optimisations du compilateur.

#### Accéder aux options d'optimisation

1. Allez dans `Projet` → `Options`
2. Sélectionnez `Compilation` sous votre plateforme cible
3. Assurez-vous d'être en configuration `Release`

#### Options importantes

**Optimisation**
- Activez cette option pour permettre au compilateur d'optimiser votre code
- Le compilateur réorganisera les instructions pour une exécution plus rapide

**Alignement**
```
Options possibles : 1, 2, 4, 8, 16 octets
Recommandé : 8 octets (bon compromis performance/taille)
```
L'alignement affecte la façon dont les données sont organisées en mémoire. Un bon alignement améliore les performances d'accès à la mémoire.

**Vérifications à désactiver en Release**
- **Contrôle de débordement** : Désactivez pour de meilleures performances
- **Contrôle de plage** : Désactivez sauf si votre application manipule beaucoup de tableaux
- **Vérification des E/S** : Peut rester activée pour les applications critiques

### Optimisation de l'éditeur de liens

L'éditeur de liens peut aussi optimiser l'exécutable final :

**Élimination du code mort**
- Active automatiquement en mode Release
- Supprime les fonctions et unités non utilisées
- Réduit significativement la taille de l'exécutable

**Fusion de sections**
- Combine les sections similaires de l'exécutable
- Réduit la taille et améliore le chargement

## Optimisations manuelles du code

### 1. Optimisation des algorithmes

La meilleure optimisation est souvent de choisir le bon algorithme.

#### Choisir les bonnes structures de données

**Exemple : Recherche dans une liste**

❌ **Mauvais** : Liste simple (O(n))
```pascal
var
  List: TStringList;
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    if List[i] = 'valeur' then
      // Trouvé
  end;
end;
```

✅ **Meilleur** : Dictionnaire (O(1))
```pascal
var
  Dict: TDictionary<string, TObject>;
begin
  if Dict.ContainsKey('valeur') then
    // Trouvé instantanément
end;
```

Pour 1000 éléments, la version avec dictionnaire peut être 1000 fois plus rapide !

#### Éviter les calculs répétitifs

❌ **Mauvais** : Calcul dans la boucle
```pascal
for i := 0 to GetCount - 1 do  // GetCount appelé à chaque itération
begin
  ProcessItem(i);
end;
```

✅ **Meilleur** : Calcul une seule fois
```pascal
Count := GetCount;  // Appelé une seule fois
for i := 0 to Count - 1 do
begin
  ProcessItem(i);
end;
```

### 2. Optimisation de la mémoire

#### Libérer les ressources rapidement

```pascal
procedure TraiterDonnees;
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Utilisation de Liste
    Liste.LoadFromFile('data.txt');
    // Traitement...
  finally
    Liste.Free;  // Libération immédiate
  end;
end;
```

#### Utiliser des pools d'objets

Pour les objets créés/détruits fréquemment, considérez un pool d'objets :

```pascal
type
  TObjectPool<T: class, constructor> = class
  private
    FPool: TList<T>;
  public
    function Acquire: T;
    procedure Release(AObject: T);
  end;
```

Cela évite les allocations/désallocations répétées qui sont coûteuses.

#### Limiter les copies de chaînes

Les chaînes en Delphi utilisent le "copy-on-write", mais certaines opérations forcent des copies :

❌ **Mauvais** : Concaténations multiples
```pascal
Result := '';
for i := 0 to 1000 do
  Result := Result + IntToStr(i) + ',';  // Copie à chaque itération
```

✅ **Meilleur** : Utiliser TStringBuilder
```pascal
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    for i := 0 to 1000 do
      Builder.Append(IntToStr(i)).Append(',');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

### 3. Optimisation des boucles

Les boucles sont souvent des zones critiques pour les performances.

#### Minimiser le travail dans les boucles

❌ **Mauvais**
```pascal
for i := 0 to List.Count - 1 do
begin
  if Assigned(List[i]) then
  begin
    ProcessItem(List[i]);
    UpdateUI;  // Mise à jour de l'interface à chaque itération !
  end;
end;
```

✅ **Meilleur**
```pascal
for i := 0 to List.Count - 1 do
begin
  if Assigned(List[i]) then
    ProcessItem(List[i]);
end;
UpdateUI;  // Une seule mise à jour à la fin
```

#### Dérouler les boucles pour les petites itérations

Pour de très petites boucles avec un nombre connu d'itérations :

```pascal
// Au lieu de :
for i := 0 to 3 do
  Total := Total + Values[i];

// Écrivez :
Total := Values[0] + Values[1] + Values[2] + Values[3];
```

Cela élimine la gestion de la boucle, mais rendez le code moins lisible. À utiliser avec parcimonie.

### 4. Optimisation des appels de fonctions

#### Inlining

Pour les petites fonctions appelées fréquemment, utilisez la directive `inline` :

```pascal
function Carre(X: Integer): Integer; inline;
begin
  Result := X * X;
end;
```

Le compilateur remplacera l'appel de fonction par le code directement, évitant le coût de l'appel.

**Attention** : N'utilisez `inline` que pour des fonctions très courtes (1-3 lignes). Pour les fonctions plus longues, cela peut augmenter la taille du code sans gain de performance.

#### Éviter les appels virtuels inutiles

Les appels de méthodes virtuelles sont légèrement plus lents :

```pascal
type
  TBase = class
    procedure Process; virtual;  // Appel dynamique
  end;

  TConcret = class(TBase)
    procedure Process; override;
  end;

// Si vous connaissez le type exact :
var
  Obj: TConcret;  // Type précis
begin
  Obj.Process;  // Appel direct possible
end;
```

### 5. Optimisation de l'accès aux données

#### Utiliser des transactions

Pour les opérations sur bases de données :

```pascal
// Au lieu de valider chaque insertion :
for i := 0 to 1000 do
begin
  Query.SQL.Text := 'INSERT INTO ...';
  Query.ExecSQL;  // Commit automatique à chaque fois
end;

// Utilisez une transaction :
Connection.StartTransaction;
try
  for i := 0 to 1000 do
  begin
    Query.SQL.Text := 'INSERT INTO ...';
    Query.ExecSQL;
  end;
  Connection.Commit;  // Un seul commit
except
  Connection.Rollback;
  raise;
end;
```

Gain de performance : jusqu'à 100 fois plus rapide !

#### Charger uniquement les données nécessaires

```pascal
// Au lieu de :
Query.SQL.Text := 'SELECT * FROM Clients';  // Toutes les colonnes

// Chargez seulement ce dont vous avez besoin :
Query.SQL.Text := 'SELECT ID, Nom, Email FROM Clients';
```

#### Utiliser des requêtes préparées

```pascal
Query.SQL.Text := 'SELECT * FROM Clients WHERE Ville = :ville';
Query.Prepare;  // Préparation une seule fois

// Puis réutilisez :
Query.ParamByName('ville').AsString := 'Paris';
Query.Open;
// ...
Query.Close;

Query.ParamByName('ville').AsString := 'Lyon';
Query.Open;
```

### 6. Optimisation de l'interface utilisateur

#### Suspendre les mises à jour

Lors de modifications multiples de l'interface :

```pascal
ListView.Items.BeginUpdate;
try
  for i := 0 to 1000 do
    ListView.Items.Add.Caption := 'Item ' + IntToStr(i);
finally
  ListView.Items.EndUpdate;  // Rafraîchissement en une fois
end;
```

#### Utiliser des threads pour les opérations longues

Ne bloquez jamais l'interface utilisateur :

```pascal
procedure TForm1.ButtonClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      // Traitement long en arrière-plan
      ProcessData;

      // Mise à jour de l'interface dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Caption := 'Terminé';
        end
      );
    end
  );
end;
```

#### Virtualisation des listes

Pour afficher de grandes quantités de données :

```pascal
// Utilisez TListView ou TStringGrid en mode virtuel
ListView.OwnerData := True;  // Mode virtuel

procedure TForm1.ListViewData(Sender: TObject; Item: TListItem);
begin
  // Fournissez les données à la demande
  Item.Caption := GetItemCaption(Item.Index);
end;
```

Seuls les éléments visibles sont créés, économisant mémoire et temps.

## Optimisation de la taille de l'exécutable

### 1. Supprimer les unités inutilisées

Vérifiez les clauses `uses` de vos unités et supprimez celles qui ne sont pas utilisées :

```pascal
uses
  System.SysUtils,  // Nécessaire
  System.Classes,   // Nécessaire
  Vcl.Graphics,     // Supprimez si non utilisé
  Vcl.Forms;        // Nécessaire pour les formulaires
```

### 2. Utiliser l'édition de liens intelligente

Dans `Projet` → `Options` → `Édition de liens` :
- Activez **Smart linking** (liaison intelligente)
- Élimine automatiquement le code non utilisé
- Peut réduire la taille de 20-50%

### 3. Compresser l'exécutable

Utilisez des outils de compression d'exécutables :
- **UPX** (Ultimate Packer for eXecutables) : Gratuit et efficace
- Peut réduire la taille de 50-70%
- Attention : Certains antivirus peuvent le signaler comme suspect

```bash
upx --best MonApplication.exe
```

### 4. Éviter d'inclure des ressources inutiles

Supprimez les images, icônes et ressources non utilisées de votre projet.

## Optimisations spécifiques à Delphi 13

### 1. Opérateur ternaire

Delphi 13 introduit l'opérateur ternaire qui peut être plus efficace :

```pascal
// Avant :
if Condition then
  Result := ValeurSiVrai
else
  Result := ValeurSiFaux;

// Delphi 13 :
Result := if Condition then ValeurSiVrai else ValeurSiFaux;
```

### 2. Améliorations FireDAC

Delphi 13 améliore les performances de FireDAC :
- Meilleure gestion du cache
- Optimisation des requêtes asynchrones
- Compression native des données

### 3. Support LLDB v12

Le nouveau débogueur permet un meilleur profilage :
- Mesures de performance plus précises
- Identification plus rapide des goulots d'étranglement

## Bonnes pratiques d'optimisation

### 1. Optimisez les 20% qui comptent

Selon le principe de Pareto, 80% du temps d'exécution est passé dans 20% du code. Concentrez-vous sur ces 20%.

### 2. Documentez vos optimisations

Quand vous optimisez du code, ajoutez un commentaire expliquant pourquoi :

```pascal
// Optimisation : Utilisation de TStringBuilder pour éviter
// les copies répétées de chaînes dans la boucle
Builder := TStringBuilder.Create;
```

Cela aide à la maintenance future.

### 3. Ne sacrifiez pas la lisibilité sans raison

Un code lisible est plus facile à maintenir et à optimiser plus tard. N'écrivez pas de code obscur pour gagner quelques microsecondes.

### 4. Testez sur du matériel représentatif

Testez vos optimisations sur du matériel similaire à celui de vos utilisateurs cibles, pas seulement sur votre machine de développement puissante.

### 5. Considérez l'optimisation comme itérative

L'optimisation n'est pas une étape unique mais un processus continu :
1. Profiler
2. Identifier le goulot
3. Optimiser
4. Mesurer l'amélioration
5. Répéter

## Pièges à éviter

### 1. Optimisation prématurée

N'optimisez pas avant d'avoir un problème de performance réel et mesuré.

### 2. Micro-optimisations inutiles

Ne perdez pas de temps à optimiser du code qui s'exécute une seule fois ou très rarement.

### 3. Casser la fonctionnalité

Testez toujours après une optimisation. Un code rapide mais incorrect n'a aucune valeur.

### 4. Ignorer la complexité algorithmique

Parfois, réécrire une fonction avec un meilleur algorithme est plus efficace que toutes les micro-optimisations possibles.

### 5. Optimiser sans mesurer

"Je pense que ce code est lent" n'est pas une base suffisante. Mesurez avec des outils de profilage.

## Checklist d'optimisation

Avant de distribuer votre application, vérifiez :

- [ ] Mode Release activé avec optimisations du compilateur
- [ ] Profilage effectué pour identifier les goulots d'étranglement
- [ ] Algorithmes critiques optimisés
- [ ] Unités inutilisées supprimées
- [ ] Smart linking activé
- [ ] Tests de performance sur matériel représentatif
- [ ] Mémoire : Pas de fuites détectées
- [ ] Interface utilisateur : Réactive même sous charge
- [ ] Taille de l'exécutable raisonnable
- [ ] Temps de démarrage acceptable

## Outils recommandés

### Profilage

- **AQtime** : Profileur professionnel (payant)
- **Sampling Profiler** : Intégré à Delphi
- **FastMM** : Pour détecter les fuites mémoire

### Analyse de code

- **Pascal Analyzer** : Analyse statique du code
- **Delphi IDE Metrics** : Métriques de complexité

### Compression

- **UPX** : Compression d'exécutables (gratuit)

## Exemples de gains typiques

Voici des gains de performance typiques obtenus avec différentes optimisations :

| Optimisation | Gain typique | Difficulté |
|-------------|--------------|------------|
| Compiler en Release | 20-50% | Facile |
| Meilleur algorithme | 100-1000%+ | Moyenne |
| TStringBuilder pour concaténations | 50-200% | Facile |
| Transactions BD | 1000-5000% | Facile |
| Mise en cache | 50-500% | Moyenne |
| Threading pour UI | Réactivité | Moyenne-Difficile |
| Mode virtuel pour listes | 80-95% (mémoire) | Moyenne |
| Smart linking | 20-50% (taille) | Facile |

## Conclusion

L'optimisation du code final est un art qui demande de l'expérience et de la mesure. Les règles d'or sont :

1. **Mesurez d'abord** : Utilisez des outils de profilage
2. **Ciblez les vrais problèmes** : Les 20% de code qui prennent 80% du temps
3. **Choisissez les bons algorithmes** : Souvent plus efficace que les micro-optimisations
4. **Testez après chaque optimisation** : Vérifiez que vous avez vraiment amélioré les performances
5. **Maintenez la lisibilité** : Un code maintenable est un code que vous pourrez optimiser plus tard

Avec Delphi 13, vous disposez d'un excellent compilateur qui fait déjà beaucoup d'optimisations automatiquement. Concentrez-vous sur les optimisations à haut niveau (algorithmes, architecture) et laissez le compilateur gérer les détails de bas niveau.

Une application bien optimisée offrira une excellente expérience utilisateur et se démarquera de la concurrence par sa réactivité et son efficacité.

⏭️ [Création d'installateurs (Inno Setup, InstallAware)](/17-distribution-et-deploiement/03-creation-dinstallateurs.md)
