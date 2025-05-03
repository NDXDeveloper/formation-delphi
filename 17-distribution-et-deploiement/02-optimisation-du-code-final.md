# 17.2 Optimisation du code final

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Une fois que vous avez compilé votre application Delphi en mode Release, vous pouvez aller encore plus loin pour optimiser votre code final. Cette optimisation permet d'améliorer les performances, de réduire la taille de l'exécutable et d'offrir une meilleure expérience à vos utilisateurs. Ce chapitre vous guide à travers les différentes techniques d'optimisation accessibles même aux développeurs débutants.

## Pourquoi optimiser votre code final ?

L'optimisation de votre code apporte plusieurs avantages :

- **Performance améliorée** : exécution plus rapide de votre application
- **Consommation réduite de mémoire** : utilisation plus efficace des ressources
- **Taille d'exécutable réduite** : téléchargement et installation plus rapides
- **Meilleure expérience utilisateur** : interface plus réactive et fluide

## Options d'optimisation du compilateur

### Niveau d'optimisation

Delphi offre plusieurs niveaux d'optimisation que vous pouvez configurer :

1. Ouvrez votre projet dans l'IDE Delphi
2. Allez dans **Project** → **Options du projet** (`Shift+Ctrl+F11`)
3. Sélectionnez **Delphi Compiler** → **Optimizations** dans le panneau gauche
4. Vérifiez que les options suivantes sont activées en mode Release :

![Options d'optimisation](https://placeholder-image.com/delphi-optimization-options.png)

- ✅ **Optimization** : active l'optimisation du code généré
- ✅ **Complete boolean evaluation** : optimise les expressions booléennes
- ✅ **Use inline expansion** : remplace certains appels de fonction par le code de la fonction

### Options de liaison (Linking)

Dans la même boîte de dialogue, allez dans **Delphi Compiler** → **Linking** :

- Activez **Build with runtime packages** uniquement si vous souhaitez partager des packages entre applications
- Activez **Use debug DCUs** uniquement en mode Debug, jamais en Release

## Techniques d'optimisation du code

Au-delà des options du compilateur, voici des techniques que vous pouvez appliquer directement dans votre code :

### 1. Utilisez les bonnes structures de données

Choisir la bonne structure de données peut considérablement améliorer les performances :

```pascal
// Moins efficace pour les recherches fréquentes
var
  ListeNoms: TStringList;
begin
  ListeNoms := TStringList.Create;
  try
    // Ajouter des milliers d'éléments...
    if ListeNoms.IndexOf('Dupont') > -1 then // Recherche lente
      ShowMessage('Trouvé!');
  finally
    ListeNoms.Free;
  end;
end;

// Plus efficace pour les recherches
var
  DictionnaireNoms: TDictionary<string, Boolean>;
begin
  DictionnaireNoms := TDictionary<string, Boolean>.Create;
  try
    // Ajouter des milliers d'éléments...
    if DictionnaireNoms.ContainsKey('Dupont') then // Recherche rapide
      ShowMessage('Trouvé!');
  finally
    DictionnaireNoms.Free;
  end;
end;
```

### 2. Utilisez les directives inline

La directive `inline` permet au compilateur d'insérer le code d'une fonction directement à l'endroit où elle est appelée, évitant ainsi le coût d'un appel de fonction :

```pascal
function Carre(Valeur: Integer): Integer; inline;
begin
  Result := Valeur * Valeur;
end;
```

⚠️ À utiliser avec modération : ne placez pas `inline` sur de grandes fonctions ou celles rarement appelées.

### 3. Évitez les allocations mémoire inutiles

La création et la destruction fréquentes d'objets peuvent ralentir votre application :

```pascal
// Moins efficace - Crée et détruit un objet à chaque itération
procedure MoinsBien;
var
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    var Temp := TStringList.Create;
    try
      Temp.Add('Donnée');
      // Traitement...
    finally
      Temp.Free;
    end;
  end;
end;

// Plus efficace - Réutilise le même objet
procedure Mieux;
var
  Temp: TStringList;
  i: Integer;
begin
  Temp := TStringList.Create;
  try
    for i := 1 to 1000 do
    begin
      Temp.Clear; // Réinitialise au lieu de recréer
      Temp.Add('Donnée');
      // Traitement...
    end;
  finally
    Temp.Free;
  end;
end;
```

### 4. Utilisez StringBuilder pour les manipulations de chaînes

Pour les concaténations répétées de chaînes, utilisez `TStringBuilder` au lieu de l'opérateur `+` :

```pascal
// Moins efficace
function ConcatenationInefficient: string;
var
  Resultat: string;
  i: Integer;
begin
  Resultat := '';
  for i := 1 to 1000 do
    Resultat := Resultat + 'Texte' + IntToStr(i); // Crée une nouvelle chaîne à chaque itération
  Result := Resultat;
end;

// Plus efficace
function ConcatenationEfficient: string;
var
  Builder: TStringBuilder;
  i: Integer;
begin
  Builder := TStringBuilder.Create(20000); // Préalloue la capacité estimée
  try
    for i := 1 to 1000 do
      Builder.Append('Texte').Append(i); // Ne crée pas de nouvelles chaînes
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

## Optimisation des images et ressources

### 1. Compression des images

Réduisez la taille de vos fichiers d'images avant de les inclure dans votre projet :

- Utilisez des formats appropriés : PNG pour les images avec transparence, JPEG pour les photos
- Redimensionnez les images à la taille exacte nécessaire
- Utilisez des outils de compression d'images comme PNGGauntlet ou TinyPNG

### 2. Chargement paresseux (Lazy Loading)

Chargez les ressources uniquement lorsqu'elles sont nécessaires :

```pascal
procedure TFormPrincipale.AfficherImageSiNecessaire;
begin
  // Ne chargez l'image que lorsqu'elle devient visible
  if TabSheet2.Visible and (ImageLarge.Picture.Graphic = nil) then
  begin
    ImageLarge.Picture.LoadFromFile('ressources\grande_image.jpg');
  end;
end;
```

## Mesurer les performances

Pour vérifier si vos optimisations sont efficaces, mesurez le temps d'exécution :

```pascal
procedure TesterPerformance;
var
  DebutTemps, FinTemps: TDateTime;
  Millisecondes: Integer;
begin
  DebutTemps := Now;

  // Code à tester...

  FinTemps := Now;
  Millisecondes := MilliSecondsBetween(FinTemps, DebutTemps);
  ShowMessage('Temps d'exécution : ' + IntToStr(Millisecondes) + ' ms');
end;
```

## Outils de profilage

Delphi dispose d'outils intégrés pour analyser les performances de votre application :

1. Activez le profilage : **Project** → **Options du projet** → **Delphi Compiler** → **Compiling** → cochez **Use Debug DCUs**
2. Exécutez votre application en mode Debug via **Run** → **Run with Profiling** → **Start Profiling**
3. Utilisez l'application normalement
4. Arrêtez le profilage et analysez les résultats pour identifier les goulots d'étranglement

## Liste de vérification finale

Avant de finaliser votre application, vérifiez ces points :

- [ ] Compilé en mode Release avec les optimisations activées
- [ ] Supprimé tout code de débogage inutile (`OutputDebugString`, etc.)
- [ ] Retiré les composants non utilisés
- [ ] Optimisé les requêtes de base de données
- [ ] Réduit la taille des images et ressources
- [ ] Testé les performances avec des données réalistes

## Conseils pour les débutants

- **Commencez simple** : n'essayez pas d'optimiser tout en même temps
- **Mesurez avant et après** : assurez-vous que vos optimisations apportent un réel bénéfice
- **Optimisez les parties critiques** : concentrez-vous sur le code exécuté fréquemment
- **La lisibilité reste importante** : ne sacrifiez pas la maintenance pour des gains mineurs

## Exercice pratique

1. Prenez un projet existant et compilez-le en mode Release
2. Mesurez le temps d'exécution d'une fonctionnalité importante
3. Appliquez une ou deux techniques d'optimisation décrites dans ce chapitre
4. Mesurez à nouveau et comparez les résultats

## Conclusion

L'optimisation du code final est une étape importante avant la distribution de votre application Delphi. En appliquant les techniques décrites dans ce chapitre, vous pouvez améliorer significativement les performances de votre application. Rappelez-vous que l'optimisation est un équilibre entre performances, maintenabilité et temps de développement.

Dans la prochaine section, nous verrons comment créer des installateurs professionnels pour distribuer facilement votre application optimisée.

⏭️ [Création d'installateurs (Inno Setup, InstallAware)](/17-distribution-et-deploiement/03-creation-dinstallateurs.md)
