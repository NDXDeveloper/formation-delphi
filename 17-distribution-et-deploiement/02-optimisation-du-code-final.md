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

Delphi ne fournit plus de profileur natif intégré depuis la dépréciation d'AQtime (2020). Pour mesurer les performances en 2026, on combine plusieurs approches :

- **Sampling** : à intervalles réguliers, on relève la pile d'appel active pour repérer les fonctions qui consomment le plus de CPU. Faible surcoût, idéal en production.
- **Instrumentation** : on insère du code de mesure autour des fonctions (`TStopwatch`, traces, etc.) pour des mesures précises mais avec un surcoût plus élevé.
- **Compteurs système** : RAM, threads, handles, I/O — via les compteurs Windows (PerfMon), `htop`/`perf` sous Linux, Instruments sous macOS.

**Outils recommandés** (détaillés en fin de chapitre) :
- Sur **Linux/macOS/Android/iOS**, Delphi utilise LLDB et expose un onglet `Profile` dans les *Run Configuration* pour le sampling natif.
- Sur **Windows**, utiliser un outil externe : **Intel VTune Profiler** (gratuit non-commercial), **Visual Studio Profiler** (avec `.pdb` généré par `map2pdb`), ou **Nexus Quality Suite** (commercial).
- Pour mesurer manuellement dans le code Delphi : `System.Diagnostics.TStopwatch.StartNew` et `.ElapsedMilliseconds`.

```pascal
uses System.Diagnostics;

var
  SW: TStopwatch;
begin
  SW := TStopwatch.StartNew;
  TraitementLent;
  SW.Stop;
  WriteLn(Format('Durée : %d ms', [SW.ElapsedMilliseconds]));
end;
```

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

**Vérifications à l'exécution — recommandations 2026**

> ⚠️ Contrairement à un conseil historique souvent répété, **ne désactivez pas systématiquement** les vérifications à l'exécution en Release. Les gains de performance sont marginaux (généralement < 2 %) face au risque qu'elles couvrent : un débordement d'entier ou un dépassement d'indice non détecté est exactement le type de bug qui crée des vulnérabilités de sécurité (CWE-129, CWE-190) ou des comportements silencieusement faux en production.  
>  
> - **Contrôle de débordement (`{$Q+}`)** : laisser **activé** par défaut. Désactiver localement via `{$Q-}...{$Q+}` uniquement dans les hot-paths mesurés au profileur.  
> - **Contrôle de plage (`{$R+}`)** : laisser **activé** par défaut, surtout pour les apps manipulant des tableaux ou indices issus de données externes (réseau, fichiers, BD).  
> - **Vérification des E/S (`{$I+}`)** : à laisser activée (coût négligeable).  
> - **Assertions (`{$C+}`)** : à votre discrétion (désactiver si pures aides au debug, garder si elles documentent des invariants).

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

> 💡 **Mythe à dissiper** : contrairement à C/C++/C#/Java, **la borne d'une boucle `for ... to ... do` est évaluée UNE SEULE FOIS en Delphi**, au début de l'exécution. Donc `for i := 0 to GetCount - 1 do` n'appelle PAS `GetCount` à chaque itération — pas besoin de l'optimiser. C'est documenté dans la spécification du langage.  
>  
> La règle change pour les boucles `while` et `repeat`, où la condition est ré-évaluée à chaque tour.

❌ **Vraiment mauvais** : `while` avec appel coûteux dans la condition
```pascal
while i < Liste.Count do  // .Count est ré-évalué à chaque tour.  
begin  
  if SomethingCostly(Liste[i]) then  // SomethingCostly aussi.
    ProcessItem(Liste[i]);
  Inc(i);
end;
```

✅ **Meilleur** : extraire la valeur invariante avant la boucle
```pascal
N := Liste.Count;  
while i < N do  
begin  
  Item := Liste[i];     // Une seule indexation.
  if SomethingCostly(Item) then
    ProcessItem(Item);
  Inc(i);
end;
```

✅ **Encore mieux** : `for-in` quand on parcourt toute la collection
```pascal
for Item in Liste do
  if SomethingCostly(Item) then
    ProcessItem(Item);
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

> ⚠️ **Contraintes Delphi pour que l'inlining fonctionne réellement** :  
> - La fonction inline doit être **déclarée AVANT son utilisation** (Pascal classique).  
> - Si elle est dans une autre unité, cette unité doit apparaître dans la clause `uses` de la section **`interface`** (pas seulement `implementation`) — sinon le compilateur émet le warning « W1035 Cannot inline » et fait un appel classique.  
> - Certaines constructions empêchent l'inlining : `try/except`, `try/finally`, code assembleur inline, fonctions virtuelles, exceptions levées, paramètres `out`/`var` complexes.  
> - Activez `{$INLINE ON}` (par défaut) — `{$INLINE OFF}` ou `{$INLINE AUTO}` changent le comportement.

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

#### Utiliser des transactions et des requêtes paramétrées

Pour les opérations en masse sur bases de données, deux leviers se cumulent :

1. **Une seule transaction** au lieu d'un commit par insertion (FireDAC : `AutoCommit = True` par défaut → chaque `ExecSQL` valide).
2. **Une requête préparée avec paramètres** au lieu de reconstruire le SQL à chaque tour (évite le parsing + planification SQL côté serveur).

❌ **Mauvais** : SQL reconstruit + autocommit à chaque insertion :
```pascal
for i := 0 to 1000 do  
begin  
  Query.SQL.Text := Format('INSERT INTO T(v) VALUES(%d)', [i]);
  Query.ExecSQL;  // Avec AutoCommit=True : commit après chaque insertion.
end;
```

✅ **Meilleur** : requête préparée + transaction explicite :
```pascal
Query.SQL.Text := 'INSERT INTO T(v) VALUES(:v)';  
Query.Prepare;  // Plan compilé une fois côté serveur.  

Connection.StartTransaction;  
try  
  for i := 0 to 1000 do
  begin
    Query.ParamByName('v').AsInteger := i;
    Query.ExecSQL;
  end;
  Connection.Commit;  // Un seul commit pour les 1001 lignes.
except
  Connection.Rollback;
  raise;
end;
```

Gain de performance typique : **100× à 1000×** sur un serveur distant, où la latence réseau domine et où un commit force un *fsync* disque.

> 💡 **Pour FireDAC spécifiquement** : utilisez la fonctionnalité **Array DML** (propriétés `TFDCustomCommand.ArrayDMLSize` + `Params[].AsXxxs[i]`) qui envoie le batch en un seul aller-retour réseau pour des gains encore plus importants sur SQL Server, Oracle, PostgreSQL et MySQL 8.x.  
>  
> ```pascal  
> Query.SQL.Text := 'INSERT INTO T(v) VALUES(:v)';
> Query.Params.ArraySize := 1000;            // Taille du batch
> for i := 0 to 999 do
>   Query.Params[0].AsIntegers[i] := i;       // Remplir le tableau
> Query.Execute(1000, 0);                     // 1 appel réseau pour 1000 lignes
> ```

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
uses System.Threading, System.Classes;

procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  TTask.Run(
    procedure
    begin
      // Traitement long en arrière-plan
      ProcessData;

      // Mise à jour de l'interface dans le thread principal.
      // ⚠ `TThread.Synchronize` BLOQUE le thread arrière-plan jusqu'à ce
      //   que le code UI ait été exécuté. Pour un simple « notifier la
      //   fin » sans attendre, préférer `TThread.Queue` qui poste
      //   asynchroniquement (le thread arrière-plan continue immédiatement).
      TThread.Queue(nil,
        procedure
        begin
          Label1.Caption := 'Terminé';
        end
      );
    end
  );
end;
```

> 💡 **Synchronize vs Queue** :  
> - `TThread.Synchronize` : bloquant. Utilisez-le quand le thread arrière-plan **a besoin du résultat** de l'opération UI avant de continuer.  
> - `TThread.Queue` : non bloquant. Utilisez-le pour les **notifications fire-and-forget** (mises à jour de progression, fin de traitement).

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

> 🚨 **À éviter en 2026 pour les applications grand public.** L'utilisation de packers comme UPX est aujourd'hui **fortement déconseillée** :  
> - **Faux positifs antivirus** : Windows Defender, CrowdStrike, SentinelOne et la plupart des EDR considèrent les binaires packés UPX comme suspects par défaut (la majorité des malwares modernes utilisent du packing pour échapper aux signatures).  
> - **Windows SmartScreen** : un binaire packé n'acquiert quasi jamais de réputation positive, déclenchant l'avertissement « *Microsoft Defender SmartScreen a empêché le démarrage d'une application non reconnue* ».  
> - **Signature de code cassée** : packer un binaire APRÈS signature invalide la signature Authenticode ; packer AVANT signature donne un binaire dont l'identité du code packé n'est pas vérifiable.  
> - **Bandwidth marginal** : les CDN modernes utilisent déjà la compression Brotli/gzip au transport HTTP, qui compresse à peu près aussi bien qu'UPX pour les EXE Delphi.  
>  
> **Alternatives modernes** :  
> - Activer le **Smart linking** (déjà couvert plus haut) pour réduire la taille à la source.  
> - Compresser l'installateur (Inno Setup avec LZMA2 ou solid mode, cf section 17.3) : la décompression a lieu une seule fois à l'installation, pas à chaque lancement.  
> - Pour les distributions web, laisser le serveur HTTP gérer la compression de transport.  
>  
> Si vous tenez à packer (par exemple pour un usage interne sans signature) :  
>  
> ```bash  
> upx --best --lzma MonApplication.exe
> ```  
>  
> Notez aussi que beaucoup de DRM logiciels (par ex. VMProtect, Themida) résolvent à la fois le packing et la protection contre la rétro-ingénierie — mais ce sont des produits coûteux à usage très spécifique.

### 4. Éviter d'inclure des ressources inutiles

Supprimez les images, icônes et ressources non utilisées de votre projet.

## Optimisations spécifiques à Delphi 13

### 1. Fonctions `IfThen` génériques

> ⚠️ **Important** : contrairement à C/C++/Java, **Object Pascal n'a PAS d'opérateur ternaire** sous la forme `Resultat := if Condition then A else B`. Le `if` reste une *instruction*, pas une *expression*. Pour un raccourci, utilisez `IfThen` :  
>  
> ```pascal  
> uses System.SysUtils;       // IfThen(condition, vrai_str, faux_str)
> uses System.Math;           // IfThen pour Integer / Double / etc.
>
> // String :
> Result := System.SysUtils.IfThen(EstAdmin, 'Administrateur', 'Utilisateur');
>
> // Integer :
> NbItems := System.Math.IfThen(ListePresente, List.Count, 0);
> ```  
>  
> ⚠ **Piège** : `IfThen` étant une fonction normale, ses **deux branches sont évaluées** (à la différence d'un vrai opérateur ternaire). N'utilisez donc pas `IfThen(Obj <> nil, Obj.Valeur, 0)` — l'évaluation de `Obj.Valeur` quand `Obj = nil` lèvera une AV.

### 2. Améliorations FireDAC

FireDAC dans Delphi 13 (Florence) offre des optimisations notables :
- Cache de schéma plus efficace (réduit les requêtes meta-data sur PostgreSQL/SQL Server)
- Améliorations du moteur asynchrone (`TFDQuery.OpenAsync`, `ExecSQLAsync`)
- Meilleure gestion de la connexion pooling pour les apps multi-thread

### 3. Support LLDB sur les plateformes non-Windows

Le débogueur Delphi sur Windows reste le débogueur natif Embarcadero. Sur **Linux 64**, **macOS** (Intel et Apple Silicon), **iOS** et **Android**, Delphi utilise **LLDB** (le débogueur du projet LLVM). Cela apporte :
- Mesures de performance plus précises (sampling moins intrusif).
- Meilleur support des CPU ARM64 (Apple Silicon, Android arm64-v8a).
- Compatibilité avec les outils LLVM tiers pour le profilage avancé.

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

- **`TStopwatch`** (intégré à `System.Diagnostics`) : mesure manuelle simple dans le code Delphi (cf exemple en début de chapitre). Pas un vrai profileur mais utile pour cibler une section précise.
- **Sampling Profiler natif Delphi** : disponible sur **Linux/macOS/iOS/Android** via l'intégration LLDB (onglet *Profile* dans les *Run Configuration*). **Pas disponible sur Windows** depuis la dépréciation d'AQtime.
- **FastMM4 / FastMM5** : pour détecter les fuites mémoire (FastMM5 est la version moderne maintenue par Pierre le Riche).
- **MadExcept / EurekaLog** : profilage léger + capture de pile en production (cf section 17.10).
- ~~**AQtime**~~ : profileur historique de SmartBear, **déprécié depuis 2020** (n'est plus en vente ni mis à jour).
- **Nexus Quality Suite (NQS)** : alternative commerciale moderne pour Delphi, succession spirituelle d'AQtime.
- **Intel VTune Profiler** (gratuit pour usage non commercial) : excellent pour le profilage CPU au niveau micro-architectural sur x86_64.
- **Visual Studio Profiler** : utilisable sur des binaires Delphi accompagnés de leur fichier `.map` converti en `.pdb` via `map2pdb`.

### Analyse de code

- **Pascal Analyzer (Peganza)** : analyse statique commerciale spécifique Delphi.
- **FixInsight (TMS Software)** : extension IDE pour détecter les code smells. **Édition gratuite** (intégration IDE) et **édition Pro** payante (ligne de commande pour CI/CD). Pas open source.
- **DelphiAST** (open source) : parseur Pascal pour bâtir vos propres analyses.
- **Semgrep** (open source) : règles personnalisables multi-langages, supporte la syntaxe Pascal via patterns custom.

### Compression

- **LZMA2** (intégré à Inno Setup, voir section 17.3) : compression de l'installateur, à privilégier sur le packing d'EXE.
- **UPX** : à éviter pour les applications grand public en 2026 (faux positifs antivirus). Acceptable pour distribution interne contrôlée.

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
