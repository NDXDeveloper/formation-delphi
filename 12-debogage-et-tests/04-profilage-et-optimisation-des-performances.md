# 12.4 Profilage et optimisation des performances

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction au profilage

Le profilage est une technique permettant d'analyser le comportement d'un programme pendant son exécution pour identifier les goulots d'étranglement et les opportunités d'optimisation. En tant que développeur Delphi, optimiser les performances de vos applications est essentiel pour offrir une meilleure expérience utilisateur et réduire la consommation de ressources.

## Pourquoi profiler une application ?

Avant d'investir du temps dans l'optimisation, il est important de comprendre pourquoi et quand cette démarche est nécessaire :

1. **Optimisation prématurée vs ciblée** : La célèbre citation de Donald Knuth, "L'optimisation prématurée est la racine de tous les maux", reste pertinente. Il est plus efficace d'optimiser en se basant sur des données réelles plutôt que sur des suppositions.

2. **La règle des 80/20** : Généralement, 80% du temps d'exécution d'un programme est consommé par seulement 20% du code. Le profilage aide à identifier ces 20% critiques.

3. **Évaluation objective** : Le profilage fournit des mesures objectives, évitant ainsi les optimisations fondées sur des impressions subjectives qui peuvent parfois dégrader les performances.

## Outils de profilage intégrés à Delphi

Delphi propose plusieurs outils intégrés pour analyser les performances de vos applications :

### 1. Le profileur de performance de Delphi

Depuis Delphi 10.4 Sydney, un profileur de performance complet est intégré dans l'IDE :

> 💡 **Nécessite Delphi 10.4 ou supérieur**

Pour l'utiliser :

1. Ouvrez votre projet dans Delphi
2. Allez dans **View > Tools Windows > Performance Profile** (Vue > Fenêtres d'outils > Profil de performance)
3. Cliquez sur **Run With Profiling** (Exécuter avec profilage)
4. Utilisez votre application normalement pour générer des données
5. Fermez l'application pour analyser les résultats

![Interface du profileur Delphi](https://via.placeholder.com/600x300)

L'interface du profileur affiche alors :

- **Call Graph** (Graphe d'appels) : Visualisation des appels entre fonctions
- **Function List** (Liste des fonctions) : Classement des fonctions selon leur temps d'exécution
- **Time Line** (Chronologie) : Répartition temporelle des appels
- **Hot Spots** (Points chauds) : Fonctions consommant le plus de temps

### 2. CodeSite Express

CodeSite Express est inclus dans Delphi et permet de tracer l'exécution de votre code :

```pascal
uses
  CodeSiteLogging;

procedure ExempleProcedure;
begin
  CodeSite.EnterMethod('ExempleProcedure');
  try
    // Votre code ici
    CodeSite.SendNote('Étape intermédiaire');

    // Plus de code
    CodeSite.SendValue('Compteur', Compteur);
  finally
    CodeSite.ExitMethod('ExempleProcedure');
  end;
end;
```

### 3. Mesure manuelle du temps avec TStopwatch

Pour des mesures simples, la classe `TStopwatch` (depuis Delphi XE2) permet de chronométrer des portions de code :

```pascal
uses
  System.Diagnostics;

procedure MesureExecution;
var
  Chrono: TStopwatch;
  Duree: Int64;
begin
  Chrono := TStopwatch.StartNew;

  // Code à mesurer
  for var i := 1 to 1000000 do
  begin
    // Opération à tester
  end;

  Chrono.Stop;
  Duree := Chrono.ElapsedMilliseconds;

  ShowMessage(Format('Temps d''exécution : %d ms', [Duree]));
end;
```

### 4. AQTime (outil externe)

Pour un profilage plus avancé, AQTime est un outil professionnel qui s'intègre à Delphi :

1. Installez AQTime (vendu séparément ou inclus dans certaines éditions de Delphi)
2. Configurez-le via le menu **Tools > AQTime**

## Techniques de profilage de base

### 1. Profilage de la consommation CPU

Le profilage CPU identifie les fonctions qui consomment le plus de temps processeur :

1. Lancez le profileur intégré à Delphi
2. Sélectionnez le mode "Performance Profiling" (Profilage de performances)
3. Exécutez votre application et utilisez les fonctionnalités à analyser
4. Examinez le rapport pour identifier les "hot spots" (points chauds)

#### Exemple d'interprétation des résultats

Voici un exemple de ce que vous pourriez voir dans les résultats :

```
Fonction                    | Temps (ms) | % du temps total | Appels | Temps moyen/appel
---------------------------|------------|------------------|--------|------------------
TDataModule1.ChargeDonnees | 1542       | 45.2%            | 1      | 1542 ms
TForm1.RechercheTexte      | 823        | 24.1%            | 15     | 54.9 ms
TForm1.FormCreate          | 321        | 9.4%             | 1      | 321 ms
...                        | ...        | ...              | ...    | ...
```

Dans cet exemple, `TDataModule1.ChargeDonnees` consomme presque la moitié du temps d'exécution et serait une cible prioritaire pour l'optimisation.

### 2. Profilage de la mémoire

Le profilage mémoire vous aide à détecter les fuites de mémoire et à optimiser l'utilisation de la RAM :

1. Dans le profileur, sélectionnez "Memory Profiling" (Profilage mémoire)
2. Observez les allocations et libérations de mémoire
3. Identifiez les objets qui ne sont pas libérés correctement

## Optimisation des performances

Une fois les points problématiques identifiés, vous pouvez appliquer diverses techniques d'optimisation :

### 1. Optimisations au niveau du code

#### a. Boucles et itérations

```pascal
// Moins efficace
for i := 0 to Liste.Count - 1 do
begin
  // Utilisation de Liste[i]
end;

// Plus efficace
Count := Liste.Count; // Évite les calculs répétés
for i := 0 to Count - 1 do
begin
  // Utilisation de Liste[i]
end;
```

#### b. Réutilisation des objets

```pascal
// Moins efficace - crée et détruit un objet à chaque itération
for i := 1 to 1000 do
begin
  Obj := TStringList.Create;
  try
    // Utilisation de Obj
  finally
    Obj.Free;
  end;
end;

// Plus efficace - réutilise le même objet
Obj := TStringList.Create;
try
  for i := 1 to 1000 do
  begin
    Obj.Clear;
    // Utilisation de Obj
  end;
finally
  Obj.Free;
end;
```

#### c. Chaînes de caractères

```pascal
// Moins efficace - crée une nouvelle chaîne à chaque itération
ResultatTexte := '';
for i := 1 to 1000 do
  ResultatTexte := ResultatTexte + IntToStr(i) + ', ';

// Plus efficace - utilise un StringBuilder
var Builder := TStringBuilder.Create;
try
  for i := 1 to 1000 do
    Builder.Append(IntToStr(i)).Append(', ');
  ResultatTexte := Builder.ToString;
finally
  Builder.Free;
end;
```

### 2. Optimisations des accès aux données

#### a. Requêtes SQL

```pascal
// Moins efficace - charge toutes les colonnes
Query.SQL.Text := 'SELECT * FROM Clients';

// Plus efficace - ne charge que les colonnes nécessaires
Query.SQL.Text := 'SELECT ID, Nom FROM Clients WHERE DateCreation > :Date';
```

#### b. Chargement différé ou pagination

```pascal
// Charger des données par lots
Query.SQL.Text := 'SELECT * FROM GrandeTable LIMIT 100 OFFSET :Start';
Query.ParamByName('Start').AsInteger := PageActuelle * 100;
```

#### c. Utilisation d'index

Assurez-vous que vos tables de base de données ont des index appropriés pour les requêtes fréquentes.

### 3. Optimisations de l'interface utilisateur

#### a. Double buffering pour réduire le scintillement

```pascal
Form1.DoubleBuffered := True;
```

#### b. Désactivation des mises à jour pendant les opérations massives

```pascal
ListView1.Items.BeginUpdate;
try
  // Ajout multiple d'éléments
  for i := 1 to 1000 do
    ListView1.Items.Add.Caption := 'Item ' + IntToStr(i);
finally
  ListView1.Items.EndUpdate;
end;
```

#### c. Virtualisation pour les grandes listes

Utilisez des contrôles virtuels comme `TVirtualStringTree` pour gérer efficacement les grandes quantités de données.

## Exemple pratique : Optimisation d'une application de traitement de texte

Prenons un exemple concret d'optimisation d'une fonction qui cherche des occurrences dans un grand texte :

### Étape 1 : Code initial

```pascal
function CompteMots(const Texte, MotCherche: string): Integer;
var
  i, Compteur: Integer;
  MotTrouve: Boolean;
begin
  Compteur := 0;
  i := 1;

  while i <= Length(Texte) - Length(MotCherche) + 1 do
  begin
    MotTrouve := True;

    for var j := 1 to Length(MotCherche) do
    begin
      if Texte[i + j - 1] <> MotCherche[j] then
      begin
        MotTrouve := False;
        Break;
      end;
    end;

    if MotTrouve then
    begin
      Inc(Compteur);
      i := i + Length(MotCherche);
    end
    else
      Inc(i);
  end;

  Result := Compteur;
end;
```

### Étape 2 : Profilage

Après avoir profilé cette fonction avec un grand texte, nous constatons qu'elle est lente pour de grands volumes de données.

### Étape 3 : Optimisation

```pascal
function CompteMotsOptimise(const Texte, MotCherche: string): Integer;
var
  Position, Compteur: Integer;
  TexteBas, MotChercheBas: string;
begin
  // Conversion en minuscules pour recherche insensible à la casse
  TexteBas := LowerCase(Texte);
  MotChercheBas := LowerCase(MotCherche);

  Compteur := 0;
  Position := PosEx(MotChercheBas, TexteBas, 1);

  while Position > 0 do
  begin
    Inc(Compteur);
    Position := PosEx(MotChercheBas, TexteBas, Position + Length(MotChercheBas));
  end;

  Result := Compteur;
end;
```

### Étape 4 : Comparaison des performances

```pascal
procedure ComparePerformances;
var
  GrandTexte: string;
  Chrono: TStopwatch;
  TempsOriginal, TempsOptimise: Int64;
  ResultatOriginal, ResultatOptimise: Integer;
begin
  // Préparation des données de test
  GrandTexte := // Grand texte de test

  // Test de la fonction originale
  Chrono := TStopwatch.StartNew;
  ResultatOriginal := CompteMots(GrandTexte, 'Delphi');
  Chrono.Stop;
  TempsOriginal := Chrono.ElapsedMilliseconds;

  // Test de la fonction optimisée
  Chrono := TStopwatch.StartNew;
  ResultatOptimise := CompteMotsOptimise(GrandTexte, 'Delphi');
  Chrono.Stop;
  TempsOptimise := Chrono.ElapsedMilliseconds;

  // Affichage des résultats
  Memo1.Lines.Add(Format('Résultat original: %d occurrences, temps: %d ms',
                         [ResultatOriginal, TempsOriginal]));

  Memo1.Lines.Add(Format('Résultat optimisé: %d occurrences, temps: %d ms',
                         [ResultatOptimise, TempsOptimise]));

  Memo1.Lines.Add(Format('Amélioration: %.2f fois plus rapide',
                         [TempsOriginal / TempsOptimise]));
end;
```

## Optimisations avancées en Delphi

### 1. Compilateur et options de compilation

Delphi offre plusieurs options de compilation qui peuvent influencer les performances :

#### a. Optimisations du compilateur

Dans **Project > Options > Delphi Compiler > Compiling** :

- **Optimization** : Activez-la pour que le compilateur optimise le code
- **Range checking** : Désactivez-la pour les versions release (mais gardez-la en debug)
- **Overflow checking** : Désactivez-la pour les versions release

#### b. Directives de compilation

```pascal
{$OPTIMIZATION ON}    // Active les optimisations du compilateur
{$OVERFLOWCHECKS OFF} // Désactive les vérifications de dépassement
{$RANGECHECKS OFF}    // Désactive les vérifications de plage
```

### 2. Techniques multi-threading

Pour les applications qui effectuent des tâches intensives, le multithreading peut améliorer considérablement les performances :

```pascal
procedure TForm1.BoutonTraitementClick(Sender: TObject);
var
  Thread: TThread;
begin
  ProgressBar1.Visible := True;

  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      // Tâche longue ici
      TThread.Synchronize(nil,
        procedure
        begin
          // Mise à jour de l'interface utilisateur
          ProgressBar1.Visible := False;
          ShowMessage('Traitement terminé');
        end);
    end);

  Thread.Start;
end;
```

> 💡 **Astuce**: Delphi 12 Athens améliore considérablement le support du multithreading avec des primitives de synchronisation avancées et une meilleure intégration avec les interfaces utilisateur.
>
> **Nécessite Delphi 12 ou supérieur**

### 3. Optimisation des ressources graphiques

#### a. Utilisation des images adaptées

- Utilisez des formats d'image appropriés (PNG pour la transparence, JPEG pour les photos)
- Redimensionnez les images à la taille d'affichage finale
- Utilisez `TImageList` pour gérer les collections d'icônes

#### b. Dessin efficace

```pascal
// Moins efficace - redessine tout le composant
invalidate;

// Plus efficace - ne redessine qu'une zone spécifique
InvalidateRect(Handle, Rect(10, 10, 100, 100), False);
```

## Mesure de l'utilisation des ressources système

Au-delà du temps d'exécution, il est important de surveiller d'autres métriques :

### 1. Utilisation de la mémoire

```pascal
function GetMemoryUsage: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.ullTotalVirtual - MemStatus.ullAvailVirtual;
end;
```

### 2. Utilisation du processeur

Vous pouvez utiliser les API Windows pour obtenir l'utilisation du processeur par votre application.

### 3. Fichiers et E/S

Les opérations d'entrée/sortie peuvent être des goulots d'étranglement importants. Utilisez des techniques comme :

- La mise en cache des résultats
- La lecture/écriture par blocs
- Les opérations asynchrones

## Approche méthodique de l'optimisation

Pour optimiser efficacement, suivez cette approche systématique :

1. **Mesurer** : Établissez une ligne de base des performances actuelles
2. **Analyser** : Identifiez les goulots d'étranglement avec les outils de profilage
3. **Optimiser** : Modifiez le code ciblé pour améliorer les performances
4. **Mesurer à nouveau** : Vérifiez l'impact des modifications
5. **Documenter** : Notez les changements et les améliorations pour référence future

## Pièges courants à éviter

### 1. Optimisation prématurée

N'optimisez pas sans mesurer d'abord. Concentrez-vous sur les problèmes réels, pas sur les hypothétiques.

### 2. Complexité excessive

Parfois, un algorithme simple mais inefficace est préférable à un algorithme complexe difficile à maintenir.

### 3. Non-respect des compromis

L'optimisation implique souvent des compromis. Par exemple :
- Vitesse vs consommation mémoire
- Performance vs lisibilité du code
- Optimisation vs maintenabilité

## Conclusion

Le profilage et l'optimisation des performances sont des compétences essentielles pour tout développeur Delphi. En identifiant précisément les problèmes de performance et en appliquant des techniques d'optimisation ciblées, vous pouvez considérablement améliorer la réactivité et l'efficacité de vos applications.

Rappelez-vous que l'optimisation est un processus itératif qui doit être guidé par des mesures objectives. Commencez toujours par profiler votre application pour identifier les véritables goulots d'étranglement, puis appliquez les techniques d'optimisation appropriées.

Dans la prochaine section, nous examinerons la gestion des exceptions et la journalisation, des compétences essentielles pour créer des applications robustes et faciles à déboguer.

⏭️ [Gestion des exceptions et journalisation](/12-debogage-et-tests/05-gestion-des-exceptions-et-journalisation.md)
