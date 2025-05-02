# 5.8 Performances et optimisation mobile

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les appareils mobiles ont des ressources plus limitées que les ordinateurs de bureau : processeurs moins puissants, mémoire plus restreinte et autonomie de batterie à préserver. Pour offrir une expérience utilisateur fluide et réactive sur ces appareils, il est essentiel d'optimiser votre application FireMonkey. Dans cette section, nous explorerons diverses techniques pour améliorer les performances de vos applications mobiles.

## Comprendre les défis de performance sur mobile

Avant de plonger dans les optimisations, identifions les principales contraintes des appareils mobiles :

- **Puissance de calcul limitée** : Même les smartphones haut de gamme ont moins de puissance qu'un ordinateur de bureau
- **Mémoire restreinte** : La mémoire disponible est plus limitée et partagée entre toutes les applications
- **Autonomie de batterie** : Chaque opération consomme de l'énergie, affectant l'autonomie
- **Bande passante variable** : Les connexions réseau peuvent être lentes ou intermittentes
- **Expérience utilisateur** : Les utilisateurs attendent une interface fluide et réactive (60 FPS)

## Mesurer les performances

Avant d'optimiser, il faut mesurer. FireMonkey offre plusieurs outils pour évaluer les performances :

### Utiliser le moniteur de performances intégré

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer le moniteur de performances (FPS counter)
  FillRateControl.Enabled := True;
  FillRateControl.ShowInOptionsMenu := True;
end;
```

Pour activer le compteur de FPS visuellement pendant l'exécution :
1. Appuyez longuement sur l'application pendant environ 3 secondes
2. Un menu contextuel apparaîtra avec l'option "Fill Rate"
3. Activez cette option pour afficher le compteur de FPS

### Chronométrer des opérations spécifiques

```pascal
procedure ChronometrerOperation;
var
  StartTime: TDateTime;
  ElapsedMS: Int64;
begin
  StartTime := Now;

  // Opération à mesurer
  MaFonctionAChronomerer;

  // Calculer le temps écoulé en millisecondes
  ElapsedMS := MilliSecondsBetween(Now, StartTime);
  ShowMessage('Opération effectuée en ' + ElapsedMS.ToString + ' ms');
end;
```

## Optimisations générales

### 1. Désactiver les animations inutiles

Les animations consomment beaucoup de ressources. Désactivez celles qui ne sont pas essentielles :

```pascal
procedure TForm1.OptimiserAnimations;
begin
  // Désactiver les animations sur les listes longues
  ListView1.ShowAnimations := False;

  // Réduire la complexité des animations
  FloatAnimation1.Duration := 0.2; // Durée plus courte (200ms au lieu de 300ms par défaut)
  FloatAnimation1.InterpolationType := TInterpolationType.Linear; // Interpolation simple
end;
```

### 2. Gérer efficacement les ressources

Libérez les ressources dès qu'elles ne sont plus nécessaires :

```pascal
procedure TForm1.ChargerImage;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile('image.png');
    Image1.Bitmap.Assign(Bitmap);
  finally
    // Libérer la ressource dès que possible
    Bitmap.Free;
  end;
end;
```

### 3. Utiliser les opérations asynchrones

Évitez de bloquer l'interface utilisateur en effectuant les opérations longues en arrière-plan :

```pascal
procedure TForm1.ChargementAsynchrone;
begin
  // Afficher un indicateur de chargement
  ArcDial1.Visible := True;

  // Exécuter la tâche en arrière-plan
  TTask.Run(
    procedure
    var
      Data: TMemoryStream;
    begin
      Data := TMemoryStream.Create;
      try
        // Opération longue (ex: téléchargement)
        TelechargerDonnees(Data);

        // Mise à jour de l'interface sur le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            ArcDial1.Visible := False;
            TraiterDonnees(Data);
          end);
      finally
        Data.Free;
      end;
    end);
end;
```

## Optimisations spécifiques à l'interface utilisateur

### 1. Utiliser le Lazy Loading pour les listes

Pour les longues listes, chargez uniquement les éléments visibles :

```pascal
procedure TForm1.ConfigurerListeOptimisee;
begin
  // Configurer la liste avec chargement à la demande
  ListView1.ItemAppearance.ItemHeight := 60;
  ListView1.SearchVisible := False; // Désactiver la recherche si non nécessaire

  // Événement de chargement des éléments
  ListView1.OnUpdatingItemView := ChargementElementListe;
end;

procedure TForm1.ChargementElementListe(const Sender: TObject;
  const AItem: TListViewItem);
begin
  // Charger l'image seulement lorsque l'élément devient visible
  if not Assigned(AItem.Objects.ImageObject.Bitmap) or
     (AItem.Objects.ImageObject.Bitmap.Width = 0) then
  begin
    // Charger l'image pour cet élément
    AItem.Objects.ImageObject.Bitmap.LoadFromFile(
      'images/' + AItem.Tag.ToString + '.png');
  end;
end;
```

### 2. Éviter les opérations coûteuses pendant le défilement

```pascal
procedure TForm1.ListView1Scroll(Sender: TObject);
begin
  // Désactiver temporairement les opérations coûteuses pendant le défilement
  FEstEnDefilement := True;

  // Réactiver après une courte période d'inactivité
  if Assigned(FTimerDefilement) then
    FTimerDefilement.Enabled := False;

  FTimerDefilement.Interval := 200; // 200ms
  FTimerDefilement.OnTimer := FinDefilement;
  FTimerDefilement.Enabled := True;
end;

procedure TForm1.FinDefilement(Sender: TObject);
begin
  FEstEnDefilement := False;
  FTimerDefilement.Enabled := False;

  // Recharger les éléments visibles avec toutes les informations
  RechargerElementsVisibles;
end;
```

### 3. Réduire la complexité visuelle

Des interfaces visuellement plus simples sont plus performantes :

```pascal
procedure TForm1.SimplifierInterface;
begin
  // Éviter les effets de transparence complexes
  Rectangle1.Opacity := 1.0; // Opacité complète plutôt que semi-transparence

  // Éviter les dégradés complexes si non essentiels
  Rectangle1.Fill.Kind := TBrushKind.Solid;

  // Limiter les ombres portées aux éléments importants
  ShadowEffect1.Enabled := False;

  // Préférer les rectangles arrondis plutôt que des formes complexes
  Path1.Visible := False;
  RoundRect1.Visible := True;
end;
```

### 4. Utiliser des bitmaps mis en cache

```pascal
procedure TForm1.OptimiserListeAvecCache;
const
  MAX_CACHE_SIZE = 20; // Limiter la taille du cache
var
  CacheBitmaps: TDictionary<Integer, TBitmap>;
begin
  // Créer le cache d'images
  CacheBitmaps := TDictionary<Integer, TBitmap>.Create;

  // Utilisation du cache dans la gestion des éléments de liste
  ListView1.OnUpdatingItemView :=
    procedure(const Sender: TObject; const AItem: TListViewItem)
    var
      ItemID: Integer;
      Bitmap: TBitmap;
    begin
      ItemID := AItem.Tag;

      // Vérifier si l'image est dans le cache
      if not CacheBitmaps.TryGetValue(ItemID, Bitmap) then
      begin
        // Si pas dans le cache, la charger et l'ajouter
        if CacheBitmaps.Count >= MAX_CACHE_SIZE then
        begin
          // Stratégie simple : vider le cache quand plein
          for var CachedBitmap in CacheBitmaps.Values do
            CachedBitmap.Free;
          CacheBitmaps.Clear;
        end;

        Bitmap := TBitmap.Create;
        Bitmap.LoadFromFile('images/' + ItemID.ToString + '.png');
        CacheBitmaps.Add(ItemID, Bitmap);
      end;

      // Assigner l'image depuis le cache
      AItem.Objects.ImageObject.Bitmap.Assign(Bitmap);
    end;
end;
```

### 5. Utiliser des images de taille adaptée

Redimensionner les images à la taille nécessaire avant de les utiliser :

```pascal
function RedimensionnerImage(const ImageSource: TBitmap;
  const LargeurCible, HauteurCible: Integer): TBitmap;
begin
  Result := TBitmap.Create(LargeurCible, HauteurCible);
  Result.Canvas.BeginScene;
  try
    Result.Canvas.DrawBitmap(ImageSource,
      RectF(0, 0, ImageSource.Width, ImageSource.Height),
      RectF(0, 0, LargeurCible, HauteurCible),
      1.0, // Opacité
      False // Maintenir les proportions
    );
  finally
    Result.Canvas.EndScene;
  end;
end;
```

## Optimisation de la mémoire

### 1. Éviter les fuites de mémoire

```pascal
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources explicitement
  if Assigned(FImageCache) then
  begin
    // Vider le cache d'images
    for var Bitmap in FImageCache.Values do
      Bitmap.Free;
    FImageCache.Free;
  end;

  // Annuler les tâches en cours
  if Assigned(FTachesEnCours) then
  begin
    for var Tache in FTachesEnCours do
      Tache.Cancel;
  end;
end;
```

### 2. Réduire l'utilisation de mémoire

```pascal
procedure TForm1.OptimiserUtilisationMemoire;
begin
  // Utiliser des types de données appropriés
  // (par exemple, Short String au lieu de String pour les chaînes courtes)

  // Limiter le nombre d'éléments chargés
  ListView1.BeginUpdate;
  try
    // Conserver seulement les N premiers éléments
    while ListView1.Items.Count > 100 do
      ListView1.Items.Delete(ListView1.Items.Count - 1);
  finally
    ListView1.EndUpdate;
  end;

  // Déclencher manuellement le ramasse-miettes
  ReportMemoryLeaksOnShutdown := True; // En mode débogage uniquement
end;
```

### 3. Réutiliser les objets plutôt que de les recréer

```pascal
procedure TForm1.ConfigurerPoolObjets;
const
  POOL_SIZE = 20;
var
  i: Integer;
begin
  // Créer un pool d'objets réutilisables
  FBitmapPool := TObjectList<TBitmap>.Create(False); // Ne pas libérer automatiquement

  // Pré-allouer des objets
  for i := 1 to POOL_SIZE do
  begin
    var Bitmap := TBitmap.Create(100, 100);
    FBitmapPool.Add(Bitmap);
  end;
end;

function TForm1.ObtenirBitmapDuPool: TBitmap;
begin
  if FBitmapPool.Count > 0 then
  begin
    Result := FBitmapPool[0];
    FBitmapPool.Delete(0);
  end
  else
    Result := TBitmap.Create(100, 100);
end;

procedure TForm1.RetournerBitmapAuPool(Bitmap: TBitmap);
begin
  // Réinitialiser l'objet
  Bitmap.Clear(TAlphaColors.Null);

  // Le remettre dans le pool s'il n'est pas trop grand
  if FBitmapPool.Count < 30 then
    FBitmapPool.Add(Bitmap)
  else
    Bitmap.Free;
end;
```

## Optimisations des opérations réseau

### 1. Mise en cache des données

```pascal
function TForm1.ObtenirDonneesAvecCache(const URL: string;
  ExpirationEnMinutes: Integer): TStream;
var
  NomFichierCache: string;
  InfoFichier: TFileInfo;
begin
  // Créer un nom de fichier unique pour cette URL
  NomFichierCache := TPath.Combine(TPath.GetCachePath,
    THashMD5.GetHashString(URL) + '.cache');

  // Vérifier si le cache existe et est valide
  if FileExists(NomFichierCache) then
  begin
    if GetFileInfo(NomFichierCache, InfoFichier) then
    begin
      // Vérifier si le cache n'a pas expiré
      if MinutesBetween(Now, InfoFichier.FileTime) < ExpirationEnMinutes then
      begin
        Result := TFileStream.Create(NomFichierCache, fmOpenRead);
        Exit;
      end;
    end;
  end;

  // Le cache n'existe pas ou a expiré, télécharger les données
  Result := TelechargerDonneesEtMettreCacheAJour(URL, NomFichierCache);
end;

function TForm1.TelechargerDonneesEtMettreCacheAJour(const URL, NomFichierCache: string): TStream;
var
  HTTP: TNetHTTPClient;
  Response: IHTTPResponse;
  FichierCache: TFileStream;
begin
  HTTP := TNetHTTPClient.Create(nil);
  try
    // Télécharger les données
    Response := HTTP.Get(URL);

    // Sauvegarder dans le cache
    if Response.StatusCode = 200 then
    begin
      FichierCache := TFileStream.Create(NomFichierCache, fmCreate);
      try
        FichierCache.WriteBuffer(Response.ContentAsString[1],
          Length(Response.ContentAsString) * SizeOf(Char));
        // Rembobiner pour lecture
        FichierCache.Position := 0;
        Result := FichierCache;
      except
        FichierCache.Free;
        raise;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur HTTP %d', [Response.StatusCode]);
  finally
    HTTP.Free;
  end;
end;
```

### 2. Compression des données

```pascal
procedure TForm1.EnvoyerDonneesCompressees(const URL: string; Data: TStream);
var
  HTTP: TNetHTTPClient;
  CompressedStream: TMemoryStream;
  ZipFile: TZipFile;
begin
  // Compresser les données
  CompressedStream := TMemoryStream.Create;
  try
    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(CompressedStream, TZipMode.zmWrite);
      ZipFile.Add(Data, 'data.bin');
    finally
      ZipFile.Free;
    end;

    CompressedStream.Position := 0;

    // Envoyer les données compressées
    HTTP := TNetHTTPClient.Create(nil);
    try
      HTTP.ContentType := 'application/zip';
      HTTP.CustomHeaders['Content-Encoding'] := 'gzip';
      HTTP.Post(URL, CompressedStream);
    finally
      HTTP.Free;
    end;
  finally
    CompressedStream.Free;
  end;
end;
```

### 3. Limiter les transferts de données

```pascal
procedure TForm1.ChargerDonneesParPage(Page, ElementsParPage: Integer);
var
  URL: string;
begin
  // Construire l'URL avec pagination
  URL := Format('https://api.example.com/data?page=%d&limit=%d',
    [Page, ElementsParPage]);

  // Charger seulement une page de données à la fois
  TTask.Run(
    procedure
    var
      HTTP: TNetHTTPClient;
      Response: IHTTPResponse;
    begin
      HTTP := TNetHTTPClient.Create(nil);
      try
        Response := HTTP.Get(URL);
        if Response.StatusCode = 200 then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              // Traiter les données reçues et mettre à jour l'interface
              TraiterDonneesJSON(Response.ContentAsString);
            end);
        end;
      finally
        HTTP.Free;
      end;
    end);
end;
```

## Optimisations spécifiques au système d'exploitation

### Android

```pascal
{$IFDEF ANDROID}
procedure TForm1.OptimiserPourAndroid;
begin
  // Réduire la résolution des images pour les appareils à faible mémoire
  if TOSVersion.Check(5, 0) then // Android 5.0 ou supérieur
  begin
    var ActivityManager := TJActivityManager.Wrap(
      TAndroidHelper.Context.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE));
    var MemInfo := TJActivityManager_MemoryInfo.Create;
    ActivityManager.getMemoryInfo(MemInfo);

    // Si mémoire disponible faible, réduire la qualité des images
    if (MemInfo.availMem / MemInfo.totalMem) < 0.2 then
    begin
      // Moins de 20% de mémoire disponible
      Image1.DisableInterpolation := True; // Qualité inférieure mais plus rapide
      FQualiteImage := 70; // Réduire la qualité JPEG
    end;
  end;

  // Adapter les animations à la puissance de l'appareil
  var PackageManager := TAndroidHelper.Context.getPackageManager;
  if not PackageManager.hasSystemFeature(TJPackageManager.JavaClass.FEATURE_RAM_HIGH) then
  begin
    // Appareil à faible RAM
    Animation1.Enabled := False;
    Animation2.Duration := Animation2.Duration * 1.5; // Ralentir les animations
    Animation3.InterpolationType := TInterpolationType.Linear; // Interpolation simple
  end;
end;
{$ENDIF}
```

### iOS

```pascal
{$IFDEF IOS}
procedure TForm1.OptimiserPouriOS;
begin
  // Vérifier le modèle d'appareil pour adapter les performances
  var DeviceModel: NSString;
  var Size: NSUInteger;
  var Device: UIDevice := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);

  // Obtenir le modèle d'appareil
  var ModelKey := StrToNSStr('model');
  if sysctlbyname(MarshaledAString(UTF8String('hw.machine')), nil, @Size, nil, 0) = 0 then
  begin
    SetLength(DeviceModel, Size);
    sysctlbyname(MarshaledAString(UTF8String('hw.machine')), MarshaledAString(UTF8String(DeviceModel)), @Size, nil, 0);

    // Adapter selon le modèle
    if Pos('iPhone8', NSStrToStr(DeviceModel)) > 0 then
    begin
      // Modèle plus ancien - réduire les effets
      DesactiverEffetsAvances;
    end;
  end;

  // Éviter l'échantillonnage sur les petits écrans
  if Screen.Size.Width < 400 then
  begin
    Image1.WrapMode := TImageWrapMode.Original;
  end;
end;
{$ENDIF}
```

## Cas pratiques d'optimisation

### Optimisation d'une galerie d'images

```pascal
procedure TForm1.ConfigurerGalerieOptimisee;
begin
  // Utiliser un TImageViewer avec chargement optimisé
  ImageViewer1.Align := TAlignLayout.Client;

  // Mode d'interpolation optimisé
  ImageViewer1.BitmapInterpolationMode := TImageInterpolationMode.GPUOptimized;

  // Configuration des images miniatures
  FlowLayout1.Align := TAlignLayout.Bottom;
  FlowLayout1.Height := 120;
  FlowLayout1.HorizontalFlow := True;
  FlowLayout1.WrapMode := TFlowLayoutWrapMode.Wrap;

  // Charger les miniatures en tâche de fond
  ChargementMiniaturesTacheFond;
end;

procedure TForm1.ChargementMiniaturesTacheFond;
begin
  TTask.Run(
    procedure
    var
      Fichiers: TArray<string>;
      Miniatures: TArray<TBitmap>;
      i: Integer;
    begin
      // Obtenir la liste des fichiers images
      Fichiers := TDirectory.GetFiles(TPath.GetDocumentsPath, '*.jpg');
      SetLength(Miniatures, Length(Fichiers));

      // Créer des miniatures optimisées
      for i := 0 to High(Fichiers) do
      begin
        Miniatures[i] := TBitmap.Create;
        ChargerImageMiniature(Fichiers[i], Miniatures[i]);

        // Mettre à jour l'interface tous les 5 éléments ou à la fin
        if (i mod 5 = 0) or (i = High(Fichiers)) then
        begin
          TThread.Synchronize(nil,
            procedure
            var
              j: Integer;
            begin
              // Ajouter les miniatures traitées à l'interface
              for j := Max(0, i-4) to i do
              begin
                if (j <= High(Miniatures)) and Assigned(Miniatures[j]) then
                  AjouterMiniatureUI(Miniatures[j], Fichiers[j]);
              end;

              // Mise à jour de la progression
              ProgressBar1.Value := (i + 1) / Length(Fichiers);
            end);
        end;
      end;
    end);
end;

procedure TForm1.ChargerImageMiniature(const Fichier: string; Miniature: TBitmap);
var
  SourceBitmap: TBitmap;
begin
  SourceBitmap := TBitmap.Create;
  try
    // Charger l'image source
    SourceBitmap.LoadFromFile(Fichier);

    // Redimensionner pour créer une miniature (max 120x120 pixels)
    Miniature.SetSize(120, 120);
    Miniature.Canvas.BeginScene;
    try
      // Dessiner avec maintien du ratio
      var SourceRect := RectF(0, 0, SourceBitmap.Width, SourceBitmap.Height);
      var DestRect := RectF(0, 0, 120, 120);

      // Centrer l'image dans la miniature
      PreserverRatio(SourceBitmap.Width, SourceBitmap.Height, DestRect);

      Miniature.Canvas.DrawBitmap(SourceBitmap, SourceRect, DestRect, 1.0);
    finally
      Miniature.Canvas.EndScene;
    end;
  finally
    SourceBitmap.Free;
  end;
end;

procedure TForm1.PreserverRatio(SourceWidth, SourceHeight: Single; var DestRect: TRectF);
var
  Ratio, NewWidth, NewHeight: Single;
begin
  if (SourceWidth <= 0) or (SourceHeight <= 0) then
    Exit;

  Ratio := SourceWidth / SourceHeight;

  if Ratio > 1 then
  begin
    // Image plus large que haute
    NewWidth := DestRect.Width;
    NewHeight := NewWidth / Ratio;

    // Centrer verticalement
    DestRect.Top := (DestRect.Height - NewHeight) / 2;
    DestRect.Bottom := DestRect.Top + NewHeight;
  end
  else
  begin
    // Image plus haute que large
    NewHeight := DestRect.Height;
    NewWidth := NewHeight * Ratio;

    // Centrer horizontalement
    DestRect.Left := (DestRect.Width - NewWidth) / 2;
    DestRect.Right := DestRect.Left + NewWidth;
  end;
end;
```

### Optimisation d'une liste de données

```pascal
procedure TForm1.OptimiserListeDonnees;
begin
  // Utiliser un ListView optimisé
  ListView1.Align := TAlignLayout.Client;

  // Configurer le ListView pour les performances
  ListView1.ItemAppearance.ItemHeight := 60;
  ListView1.ItemAppearance.ItemEditHeight := 60;
  ListView1.SearchVisible := False; // Désactiver si non nécessaire

  // Désactiver les fonctionnalités coûteuses non utilisées
  ListView1.CanSwipeDelete := False; // Désactiver si non nécessaire
  ListView1.PullToRefresh := False;  // Désactiver si non nécessaire

  // Si le défilement est saccadé, optimiser les animations
  ListView1.SeparatorLeftOffset := 0; // Simplifier le dessin des séparateurs
  ListView1.ShowSelection := False;   // Désactiver la sélection visuelle si non nécessaire

  // Configurer le chargement optimal
  ListView1.BeginUpdate;
  try
    ChargementDonneesPagine(1); // Charger la première page
  finally
    ListView1.EndUpdate;
  end;

  // Configurer le chargement des pages suivantes lors du défilement
  ListView1.OnScrollViewChange :=
    procedure(Sender: TObject)
    begin
      if EstProcheDeLaFin(ListView1) and not FChargementEnCours then
      begin
        FChargementEnCours := True;
        ChargementDonneesPagine(FPageActuelle + 1);
      end;
    end;
end;

function TForm1.EstProcheDeLaFin(ListView: TListView): Boolean;
var
  ContentBounds, ViewportBounds: TRectF;
  Distance: Single;
begin
  ContentBounds := ListView.ContentBounds;
  ViewportBounds := ListView.ViewportBounds;

  // Distance entre la position actuelle et la fin du contenu
  Distance := ContentBounds.Bottom - ViewportBounds.Bottom;

  // Considérer "proche de la fin" si moins de 2 éléments visibles restants
  Result := Distance < (ListView.ItemAppearance.ItemHeight * 2);
end;
```

## Meilleures pratiques pour l'optimisation

1. **Mesurer avant d'optimiser** : Identifiez les vrais problèmes de performance
2. **Optimiser par ordre d'importance** : Concentrez-vous sur les opérations les plus coûteuses
3. **Tester sur des appareils réels** : Les émulateurs peuvent donner des résultats trompeurs
4. **Tester sur des appareils bas de gamme** : Si ça fonctionne bien sur un appareil bas de gamme, ça fonctionnera partout
5. **Optimiser progressivement** : Une amélioration à la fois pour identifier les gains
6. **Équilibrer performance et fonctionnalités** : Parfois, il vaut mieux simplifier que d'optimiser à l'extrême

## Liste de vérification d'optimisation mobile

Utilisez cette liste pour vérifier si votre application est optimisée pour mobile :

- [ ] Les animations sont fluides (60 FPS)
- [ ] L'application démarre en moins de 3 secondes
- [ ] Les opérations longues s'exécutent en arrière-plan
- [ ] Les images sont mises à l'échelle appropriée
- [ ] Les effets visuels complexes sont limités
- [ ] Les listes longues utilisent un chargement à la demande
- [ ] Les ressources sont libérées lorsqu'elles ne sont plus nécessaires
- [ ] Les opérations réseau sont optimisées (cache, compression)
- [ ] L'application s'adapte aux appareils de faible puissance
- [ ] La consommation de batterie est raisonnable

## Conclusion

L'optimisation des performances sur mobile est essentielle pour offrir une expérience utilisateur de qualité. En suivant les techniques présentées dans cette section, vous pouvez considérablement améliorer la réactivité et la fluidité de vos applications FireMonkey sur les appareils mobiles.

Rappelez-vous que l'optimisation est un processus continu : mesurez, améliorez, testez, puis recommencez. En prenant l'habitude d'appliquer ces bonnes pratiques dès le début de votre développement, vous créerez des applications mobiles performantes et agréables à utiliser, même sur des appareils aux ressources limitées.

Dans la section suivante, nous explorerons comment ajouter des animations et effets visuels à vos applications FireMonkey tout en maintenant de bonnes performances.

⏭️ [Animations et effets visuels](/05-developpement-multi-plateforme-avec-firemonkey/09-animations-et-effets-visuels.md)
