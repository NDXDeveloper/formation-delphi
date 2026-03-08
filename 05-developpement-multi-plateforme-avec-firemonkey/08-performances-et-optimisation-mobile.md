🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.8 Performances et optimisation mobile

## Introduction

Développer pour mobile n'est pas simplement "compiler pour iOS ou Android". Les appareils mobiles ont des contraintes très différentes des ordinateurs de bureau : batterie limitée, mémoire restreinte, puissance de calcul variable, et des utilisateurs qui attendent une réactivité parfaite. Dans cette section, nous allons explorer comment optimiser vos applications FireMonkey pour qu'elles fonctionnent de manière fluide et efficace sur smartphones et tablettes.

## 1. Comprendre les contraintes mobiles

### Les limitations physiques

**Batterie** :
- Capacité : 2000-5000 mAh (vs ordinateur branché secteur)
- Autonomie : 8-12 heures d'utilisation typique
- Sensible : GPS, écran, réseau consomment beaucoup
- Impact utilisateur : Une app qui vide la batterie sera désinstallée

**Mémoire RAM** :
- Smartphone entrée de gamme : 2-4 GB
- Smartphone milieu de gamme : 4-6 GB
- Smartphone haut de gamme : 8-12 GB
- Comparaison : PC moderne a 16-32 GB

**Processeur** :
- Architecture ARM (pas x86 comme PC)
- Plusieurs cœurs (4-8) mais moins puissants individuellement
- Throttling thermique : ralentit si surchauffe
- Comparaison : 2-3x moins puissant qu'un PC

**Stockage** :
- Plus lent que les SSD des PC
- Partagé avec le système et autres apps
- Espace limité : 32-256 GB total

**Réseau** :
- 4G/5G : rapide mais consomme de la batterie
- WiFi : plus économe mais pas toujours disponible
- Coût des données : limitation des forfaits

### Implications pour le développement

```pascal
// Ce qui fonctionne bien sur PC...
for i := 1 to 10000 do  
begin  
  Image := TImage.Create(Self);
  Image.Bitmap.LoadFromFile('photo' + i.ToString + '.jpg');
  // ... traitement
end;

// ... peut être catastrophique sur mobile :
// - Trop de mémoire utilisée
// - Trop lent
// - Batterie qui fond
```

### Le principe "Mobile First"

Développer en pensant mobile d'abord, puis enrichir pour desktop :
- ✅ Interface simple et directe
- ✅ Animations légères
- ✅ Chargement progressif
- ✅ Gestion agressive de la mémoire
- ✅ Économie de batterie

## 2. Optimisation de l'interface utilisateur

### Réduire la complexité visuelle

**Problème** : Trop d'éléments à dessiner = framerate faible

```pascal
// ❌ MAUVAIS : Trop d'éléments
procedure TForm1.CreerInterface;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    // 100 boutons avec effets = lourd
    var Button := TButton.Create(Self);
    Button.Parent := ScrollBox1;

    // Effet d'ombre sur chaque bouton
    var Shadow := TShadowEffect.Create(Button);
    Shadow.Parent := Button;

    // Effet de lueur
    var Glow := TGlowEffect.Create(Button);
    Glow.Parent := Button;
  end;
end;
```

```pascal
// ✅ BON : Simplifier
procedure TForm1.CreerInterfaceOptimisee;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    var Button := TButton.Create(Self);
    Button.Parent := ScrollBox1;

    {$IFDEF ANDROID OR IOS}
      // Pas d'effets sur mobile
    {$ELSE}
      // Effets uniquement sur desktop
      var Shadow := TShadowEffect.Create(Button);
      Shadow.Parent := Button;
    {$ENDIF}
  end;
end;
```

### Limiter les effets visuels

**Effets coûteux sur mobile** :
- Ombres (Shadow, InnerGlow)
- Flou (Blur, GaussianBlur)
- Reflets (Reflection)
- Transparence excessive

```pascal
procedure TForm1.ConfigurerEffetsSelonPlateforme;  
begin  
  {$IFDEF ANDROID OR IOS}
    // Mobile : Désactiver les effets
    BlurEffect1.Enabled := False;
    ShadowEffect1.Enabled := False;
    ReflectionEffect1.Enabled := False;
  {$ELSE}
    // Desktop : Activer
    BlurEffect1.Enabled := True;
    ShadowEffect1.Enabled := True;
    ReflectionEffect1.Enabled := True;
  {$ENDIF}
end;
```

### Optimiser les listes

**Problème** : Créer tous les éléments d'une liste = lent et gourmand

```pascal
// ❌ MAUVAIS : Créer 1000 items d'un coup
procedure TForm1.RemplirListe;  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    var Item := TListBoxItem.Create(ListBox1);
    Item.Parent := ListBox1;
    Item.Text := 'Item ' + i.ToString;
    // Crée 1000 composants en mémoire
  end;
end;
```

```pascal
// ✅ BON : Utiliser la virtualisation
procedure TForm1.RemplirListeVirtuelle;  
begin  
  // TListView avec virtualisation
  ListView1.Items.Count := 1000;  // Juste le nombre
  // Les items sont créés à la demande lors du scroll
end;

procedure TForm1.ListView1UpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
begin
  // Cette méthode est appelée pour chaque item visible
  AItem.Text := 'Item ' + AItem.Index.ToString;
  AItem.Detail := 'Détails de l''item';

  // Seuls les items visibles sont créés
end;
```

### Réduire les mises à jour de l'UI

```pascal
// ❌ MAUVAIS : Mettre à jour l'UI à chaque itération
procedure TForm1.TraiterDonnees;  
var  
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    // Traitement
    TraiterItem(i);

    // Mise à jour UI à chaque fois = très lent
    ProgressBar1.Value := i / 10000 * 100;
    Label1.Text := i.ToString + ' / 10000';
    Application.ProcessMessages;  // Force le redessinage
  end;
end;
```

```pascal
// ✅ BON : Mettre à jour par lots
procedure TForm1.TraiterDonneesOptimise;  
var  
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    TraiterItem(i);

    // Mettre à jour tous les 100 items
    if (i mod 100) = 0 then
    begin
      ProgressBar1.Value := i / 10000 * 100;
      Label1.Text := i.ToString + ' / 10000';
      Application.ProcessMessages;
    end;
  end;
end;
```

## 3. Gestion de la mémoire

### Comprendre la mémoire sur mobile

**Limite stricte** :
- Android : Le système tue les apps qui consomment trop
- iOS : idem, et avertit l'utilisateur si récurrent

**Symptômes de problèmes mémoire** :
- App qui se ferme brutalement
- Ralentissements progressifs
- Freezes et blocages

### Libérer les ressources

```pascal
// ❌ MAUVAIS : Garder tout en mémoire
var
  FImages: TArray<TBitmap>;

procedure TForm1.ChargerImages;  
var  
  i: Integer;
begin
  SetLength(FImages, 100);
  for i := 0 to 99 do
  begin
    FImages[i] := TBitmap.Create;
    FImages[i].LoadFromFile('image' + i.ToString + '.jpg');
    // 100 images en mémoire = plusieurs centaines de MB
  end;
end;
```

```pascal
// ✅ BON : Charger à la demande
procedure TForm1.ChargerImageALaDemande(Index: Integer);  
begin  
  // Libérer l'image précédente
  if Assigned(FImageActuelle) then
    FImageActuelle.Free;

  // Charger uniquement l'image nécessaire
  FImageActuelle := TBitmap.Create;
  FImageActuelle.LoadFromFile('image' + Index.ToString + '.jpg');

  Image1.Bitmap.Assign(FImageActuelle);
end;
```

### Vider les caches régulièrement

```pascal
procedure TForm1.ViderCaches;  
begin  
  // Vider les bitmaps non utilisés
  Image1.Bitmap.Clear;
  Image2.Bitmap.Clear;

  // Forcer le garbage collection (Delphi gère automatiquement,
  // mais on peut suggérer)

  // Libérer les objets temporaires
  for var Obj in FListeObjetsTemporaires do
    Obj.Free;
  FListeObjetsTemporaires.Clear;
end;

procedure TForm1.FormDeactivate(Sender: TObject);  
begin  
  // Libérer les ressources quand l'app passe en arrière-plan
  ViderCaches;
end;
```

### Détecter les fuites mémoire

```pascal
// Utiliser ReportMemoryLeaksOnShutdown en mode Debug
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end;

// Vérifier qu'on libère tout
procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Libérer explicitement les objets créés
  FreeAndNil(FMonObjet);
  FreeAndNil(FMaListe);

  // Les composants créés avec Owner sont libérés automatiquement
end;
```

## 4. Optimisation des images

### Comprendre le poids des images

**Calcul mémoire** :
```
Taille en mémoire = Largeur × Hauteur × 4 octets (RGBA)

Exemple :  
Image 1920×1080 = 1920 × 1080 × 4 = 8.3 MB en mémoire  
(même si le fichier JPG fait 500 KB sur disque !)
```

**Conséquence** : 10 photos plein écran = 80 MB de RAM sur mobile !

### Redimensionner les images

```pascal
procedure TForm1.ChargerImageRedimensionnee(const Fichier: string;
  LargeurMax, HauteurMax: Integer);
var
  BitmapTemp: TBitmap;
  Ratio: Single;
  NouvelleLargeur, NouvelleHauteur: Integer;
begin
  BitmapTemp := TBitmap.Create;
  try
    BitmapTemp.LoadFromFile(Fichier);

    // Calculer le ratio pour conserver les proportions
    if BitmapTemp.Width > BitmapTemp.Height then
      Ratio := LargeurMax / BitmapTemp.Width
    else
      Ratio := HauteurMax / BitmapTemp.Height;

    NouvelleLargeur := Trunc(BitmapTemp.Width * Ratio);
    NouvelleHauteur := Trunc(BitmapTemp.Height * Ratio);

    // Redimensionner
    Image1.Bitmap.SetSize(NouvelleLargeur, NouvelleHauteur);
    Image1.Bitmap.Canvas.BeginScene;
    try
      Image1.Bitmap.Canvas.DrawBitmap(
        BitmapTemp,
        RectF(0, 0, BitmapTemp.Width, BitmapTemp.Height),
        RectF(0, 0, NouvelleLargeur, NouvelleHauteur),
        1.0,
        True);  // High quality
    finally
      Image1.Bitmap.Canvas.EndScene;
    end;
  finally
    BitmapTemp.Free;
  end;
end;

// Utilisation
{$IFDEF ANDROID OR IOS}
  ChargerImageRedimensionnee('photo.jpg', 800, 600);  // Résolution mobile
{$ELSE}
  ChargerImageRedimensionnee('photo.jpg', 1920, 1080);  // Résolution desktop
{$ENDIF}
```

### Utiliser des formats optimisés

```pascal
// Préférer PNG pour les images avec transparence
// Préférer JPG pour les photos (plus léger)

procedure TForm1.SauvegarderImageOptimisee(Bitmap: TBitmap;
  const Fichier: string);
begin
  // Déterminer le format selon le contenu
  if ImageATransparence(Bitmap) then
  begin
    // PNG pour la transparence
    Bitmap.SaveToFile(Fichier + '.png');
  end
  else
  begin
    // JPG avec compression pour les photos
    var Surface := TBitmapSurface.Create;
    try
      Surface.Assign(Bitmap);

      // Qualité 80% = bon compromis taille/qualité
      if not TBitmapCodecManager.SaveToFile(
        Fichier + '.jpg', Surface, nil) then
        raise Exception.Create('Erreur sauvegarde');
    finally
      Surface.Free;
    end;
  end;
end;
```

### Compression et cache d'images

```pascal
type
  TImageCache = class
  private
    FCache: TDictionary<string, TBitmap>;
    FTailleMaxCache: Integer;
  public
    constructor Create(TailleMaxMB: Integer);
    destructor Destroy; override;

    function Obtenir(const Fichier: string): TBitmap;
    procedure Vider;
  end;

constructor TImageCache.Create(TailleMaxMB: Integer);  
begin  
  inherited Create;
  FCache := TDictionary<string, TBitmap>.Create;
  FTailleMaxCache := TailleMaxMB * 1024 * 1024;  // Convertir en octets
end;

destructor TImageCache.Destroy;  
begin  
  Vider;
  FCache.Free;
  inherited;
end;

function TImageCache.Obtenir(const Fichier: string): TBitmap;  
begin  
  // Chercher dans le cache
  if not FCache.TryGetValue(Fichier, Result) then
  begin
    // Pas en cache : charger
    Result := TBitmap.Create;
    Result.LoadFromFile(Fichier);

    // Ajouter au cache si pas trop gros
    if CalculerTailleCache + TailleImage(Result) < FTailleMaxCache then
      FCache.Add(Fichier, Result);
  end;
end;

procedure TImageCache.Vider;  
begin  
  for var Bitmap in FCache.Values do
    Bitmap.Free;
  FCache.Clear;
end;
```

## 5. Animations performantes

### Éviter les animations trop fréquentes

```pascal
// ❌ MAUVAIS : Animation à 60 FPS peut être trop
procedure TForm1.Timer60FPSTimer(Sender: TObject);  
begin  
  // Appelé 60 fois par seconde = beaucoup de travail
  Rectangle1.Position.X := Rectangle1.Position.X + 1;
  Rectangle1.RotationAngle := Rectangle1.RotationAngle + 1;
  // Si plusieurs animations = charge importante
end;
```

```pascal
// ✅ BON : Utiliser TAnimation intégré
procedure TForm1.AnimerAvecTAnimation;  
var  
  Anim: TFloatAnimation;
begin
  // FireMonkey optimise automatiquement
  Anim := TFloatAnimation.Create(Rectangle1);
  Anim.Parent := Rectangle1;
  Anim.PropertyName := 'Position.X';
  Anim.StartValue := 0;
  Anim.StopValue := 300;
  Anim.Duration := 1.0;  // 1 seconde
  Anim.Start;

  // L'animation est gérée par le moteur, pas par vous
end;
```

### Limiter le nombre d'animations simultanées

```pascal
// ❌ MAUVAIS : Trop d'animations simultanées
procedure TForm1.AnimerTout;  
var  
  i: Integer;
begin
  // 50 éléments animés = surcharge
  for i := 0 to 49 do
  begin
    var Anim := TFloatAnimation.Create(Rectangles[i]);
    Anim.Parent := Rectangles[i];
    Anim.PropertyName := 'Opacity';
    Anim.StartValue := 0;
    Anim.StopValue := 1;
    Anim.Duration := 2;
    Anim.Start;
  end;
end;
```

```pascal
// ✅ BON : Animer par groupes
procedure TForm1.AnimerParGroupes;  
var  
  i: Integer;
begin
  // Animer 5 par 5 avec délai
  for i := 0 to 49 do
  begin
    var Anim := TFloatAnimation.Create(Rectangles[i]);
    Anim.Parent := Rectangles[i];
    Anim.PropertyName := 'Opacity';
    Anim.StartValue := 0;
    Anim.StopValue := 1;
    Anim.Duration := 0.5;  // Plus court
    Anim.Delay := (i div 5) * 0.1;  // Délai par groupe
    Anim.Start;
  end;
end;
```

### Désactiver les animations sur appareil lent

```pascal
function TForm1.AppareilRapide: Boolean;  
begin  
  // Détecter si l'appareil est assez puissant
  {$IFDEF ANDROID}
    // Vérifier le nombre de cœurs, la RAM, etc.
    Result := GetProcessorCount > 4;
  {$ENDIF}

  {$IFDEF IOS}
    // iPhone plus récent = plus rapide
    Result := True;  // iOS généralement performant
  {$ENDIF}
end;

procedure TForm1.ConfigurerAnimations;  
begin  
  if AppareilRapide then
  begin
    // Activer toutes les animations
    AnimationsComplexes := True;
  end
  else
  begin
    // Simplifier ou désactiver
    AnimationsComplexes := False;
  end;
end;
```

## 6. Gestion du réseau et batterie

### Regrouper les requêtes réseau

```pascal
// ❌ MAUVAIS : Une requête par donnée
procedure TForm1.ChargerDonnees;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    // 100 requêtes HTTP = lent et consomme batterie
    RESTClient1.BaseURL := 'https://api.exemple.com/item/' + i.ToString;
    RESTRequest1.Execute;
    TraiterReponse(RESTRequest1.Response.Content);
  end;
end;
```

```pascal
// ✅ BON : Une seule requête
procedure TForm1.ChargerDonneesOptimise;  
begin  
  // Une seule requête pour tout
  RESTClient1.BaseURL := 'https://api.exemple.com/items?limit=100';
  RESTRequest1.Execute;

  // Traiter la réponse groupée
  var JSON := TJSONObject.ParseJSONValue(RESTRequest1.Response.Content);
  try
    TraiterTousLesItems(JSON);
  finally
    JSON.Free;
  end;
end;
```

### Cache des données réseau

```pascal
type
  TCacheReseau = class
  private
    FCacheDonnees: TDictionary<string, string>;
    FCacheExpiration: TDictionary<string, TDateTime>;
    FDureeCache: Integer;  // En secondes
  public
    constructor Create(DureeCacheSecondes: Integer = 300);
    destructor Destroy; override;

    function ObtenirDonnees(const URL: string): string;
    procedure StockerDonnees(const URL, Donnees: string);
    function EstDansCache(const URL: string): Boolean;
  end;

function TCacheReseau.ObtenirDonnees(const URL: string): string;  
begin  
  // Vérifier si en cache et pas expiré
  if EstDansCache(URL) then
  begin
    if Now - FCacheExpiration[URL] < FDureeCache / 86400 then
    begin
      // Cache valide
      Result := FCacheDonnees[URL];
      Exit;
    end
    else
    begin
      // Cache expiré : retirer
      FCacheDonnees.Remove(URL);
      FCacheExpiration.Remove(URL);
    end;
  end;

  // Pas en cache : retourner vide
  Result := '';
end;

procedure TCacheReseau.StockerDonnees(const URL, Donnees: string);  
begin  
  FCacheDonnees.AddOrSetValue(URL, Donnees);
  FCacheExpiration.AddOrSetValue(URL, Now);
end;

// Utilisation
procedure TForm1.ChargerAvecCache(const URL: string);  
var  
  Donnees: string;
begin
  // Essayer le cache d'abord
  Donnees := FCache.ObtenirDonnees(URL);

  if Donnees = '' then
  begin
    // Pas en cache : faire la requête
    RESTClient1.BaseURL := URL;
    RESTRequest1.Execute;
    Donnees := RESTRequest1.Response.Content;

    // Stocker en cache
    FCache.StockerDonnees(URL, Donnees);
  end;

  // Utiliser les données
  TraiterDonnees(Donnees);
end;
```

### Désactiver les synchronisations en arrière-plan

```pascal
procedure TForm1.GererEtatApplication;  
begin  
  // Événement quand l'app passe en arrière-plan
  var FMXApplicationEventService: IFMXApplicationEventService;
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXApplicationEventService, FMXApplicationEventService) then
  begin
    FMXApplicationEventService.SetApplicationEventHandler(
      procedure(AAppEvent: TApplicationEvent; AContext: TObject)
      begin
        case AAppEvent of
          TApplicationEvent.BecameActive:
          begin
            // App devient active : reprendre les activités
            Timer1.Enabled := True;
            ReprendreSynchronisation;
          end;

          TApplicationEvent.EnteredBackground:
          begin
            // App en arrière-plan : économiser la batterie
            Timer1.Enabled := False;
            SusprendreSynchronisation;
          end;
        end;
      end);
  end;
end;
```

## 7. Traitements asynchrones

### Déporter les calculs lourds

```pascal
// ❌ MAUVAIS : Calcul dans le thread principal
procedure TForm1.ButtonCalculClick(Sender: TObject);  
var  
  i: Integer;
  Resultat: Double;
begin
  // Calcul lourd = interface gelée
  for i := 1 to 1000000 do
    Resultat := Resultat + Sqrt(i);

  Label1.Text := Resultat.ToString;
end;
```

```pascal
// ✅ BON : Calcul asynchrone avec TTask
procedure TForm1.ButtonCalculClickOptimise(Sender: TObject);  
begin  
  // Afficher un indicateur de chargement
  ProgressCircle.Visible := True;
  ProgressCircle.Enabled := True;

  // Lancer le calcul en arrière-plan
  TTask.Run(
    procedure
    var
      i: Integer;
      Resultat: Double;
    begin
      // Calcul lourd dans un thread séparé
      Resultat := 0;
      for i := 1 to 1000000 do
        Resultat := Resultat + Sqrt(i);

      // Retour au thread principal pour mettre à jour l'UI
      TThread.Synchronize(nil,
        procedure
        begin
          Label1.Text := Resultat.ToString;
          ProgressCircle.Visible := False;
          ProgressCircle.Enabled := False;
        end);
    end);
end;
```

### Charger les images en arrière-plan

```pascal
procedure TForm1.ChargerImageAsync(const Fichier: string;
  Image: TImage);
begin
  TTask.Run(
    procedure
    var
      Bitmap: TBitmap;
    begin
      // Charger dans le thread de fond
      Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromFile(Fichier);

        // Redimensionner si nécessaire
        {$IFDEF ANDROID OR IOS}
        if Bitmap.Width > 1024 then
          RedimensionnerBitmap(Bitmap, 1024, 768);
        {$ENDIF}

        // Mettre à jour l'UI dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            Image.Bitmap.Assign(Bitmap);
          end);
      finally
        Bitmap.Free;
      end;
    end);
end;
```

## 8. Profiling et mesure des performances

### Mesurer le temps d'exécution

```pascal
uses
  System.Diagnostics;

procedure TForm1.MesurerPerformance;  
var  
  Chrono: TStopwatch;
begin
  Chrono := TStopwatch.StartNew;

  // Code à mesurer
  ChargerDonnees;
  TraiterDonnees;
  AfficherResultats;

  Chrono.Stop;

  {$IFDEF DEBUG}
  ShowMessage('Temps d''exécution : ' +
    Chrono.ElapsedMilliseconds.ToString + ' ms');
  {$ENDIF}
end;
```

### Surveiller l'utilisation mémoire

```pascal
function TForm1.ObtenirMemoireUtilisee: Int64;  
var  
  MemoryManagerState: TMemoryManagerState;
  SmallBlockTypeState: TSmallBlockTypeState;
begin
  GetMemoryManagerState(MemoryManagerState);
  Result := MemoryManagerState.TotalAllocatedMediumBlockSize +
            MemoryManagerState.TotalAllocatedLargeBlockSize;

  for SmallBlockTypeState in MemoryManagerState.SmallBlockTypeStates do
    Result := Result + SmallBlockTypeState.UseableBlockSize *
                       SmallBlockTypeState.AllocatedBlockCount;
end;

procedure TForm1.AfficherInfosMemoire;  
var  
  MemMB: Double;
begin
  MemMB := ObtenirMemoireUtilisee / (1024 * 1024);
  LabelMemoire.Text := Format('Mémoire : %.2f MB', [MemMB]);

  {$IFDEF DEBUG}
  if MemMB > 100 then
    ShowMessage('ATTENTION : Consommation mémoire élevée !');
  {$ENDIF}
end;
```

### Calculer le framerate

```pascal
var
  FFrameCount: Integer;
  FLastTime: TDateTime;
  FFPS: Single;

procedure TForm1.FormRender(Sender: TObject; Context: TContext3D;
  const ATarget: TContextTarget);
begin
  Inc(FFrameCount);

  // Calculer FPS chaque seconde
  if Now - FLastTime >= 1/86400 then  // 1 seconde
  begin
    FFPS := FFrameCount / ((Now - FLastTime) * 86400);
    FFrameCount := 0;
    FLastTime := Now;

    LabelFPS.Text := Format('FPS : %.1f', [FFPS]);

    {$IFDEF DEBUG}
    if FFPS < 30 then
      ShowMessage('ATTENTION : Framerate faible !');
    {$ENDIF}
  end;
end;
```

## 9. Optimisations spécifiques iOS

### Désactiver le motion blur

```pascal
{$IFDEF IOS}
procedure TForm1.DesactiverMotionBlur;  
begin  
  // iOS ajoute parfois un effet de flou pendant les animations
  // Désactiver pour de meilleures performances
  Quality := TCanvasQuality.SystemDefault;
end;
{$ENDIF}
```

### Optimiser pour les différents iPhones

```pascal
{$IFDEF IOS}
function TForm1.EstiPhoneRecent: Boolean;  
var  
  Device: UIDevice;
begin
  Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);

  // iPhone 12 et plus récents
  Result := Device.systemVersion.doubleValue >= 14.0;
end;

procedure TForm1.ConfigurerSeloniPhone;  
begin  
  if EstiPhoneRecent then
  begin
    // iPhone récent : activer plus de fonctionnalités
    EffetsVisuels := True;
    QualiteImages := Haute;
  end
  else
  begin
    // iPhone plus ancien : simplifier
    EffetsVisuels := False;
    QualiteImages := Moyenne;
  end;
end;
{$ENDIF}
```

## 10. Optimisations spécifiques Android

### Gérer les différentes versions Android

```pascal
{$IFDEF ANDROID}
uses
  Androidapi.Helpers;

function TForm1.VersionAndroid: Integer;  
begin  
  Result := TJBuild_VERSION.JavaClass.SDK_INT;
end;

procedure TForm1.ConfigurerSelonAndroid;  
begin  
  if VersionAndroid >= 28 then  // Android 9.0+
  begin
    // Version récente : fonctionnalités complètes
    ActiverNotificationsAvancees;
  end
  else if VersionAndroid >= 23 then  // Android 6.0+
  begin
    // Version moyenne : fonctionnalités standard
    ActiverNotificationsStandard;
  end
  else
  begin
    // Ancienne version : fonctionnalités basiques
    ActiverNotificationsBasiques;
  end;
end;
{$ENDIF}
```

### Optimiser pour différents écrans Android

```pascal
{$IFDEF ANDROID}
function TForm1.EstGrandEcran: Boolean;  
var  
  ScreenService: IFMXScreenService;
  ScreenSize: TSize;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXScreenService, ScreenService) then
  begin
    ScreenSize := ScreenService.GetScreenSize;
    // Plus de 6 pouces en diagonale
    Result := Sqrt(Sqr(ScreenSize.Width) + Sqr(ScreenSize.Height)) > 1000;
  end
  else
    Result := False;
end;

procedure TForm1.AdapterSelonEcran;  
begin  
  if EstGrandEcran then
  begin
    // Grand écran : interface enrichie
    AfficherDeuxColonnes;
    TailleTexte := 16;
  end
  else
  begin
    // Petit écran : interface simple
    AfficherUneColonne;
    TailleTexte := 14;
  end;
end;
{$ENDIF}
```

## 11. Bonnes pratiques

### ✅ À FAIRE

**1. Tester sur appareils réels**
```pascal
// Ne pas se fier uniquement à l'émulateur
// Tester sur :
// - Smartphone entrée de gamme
// - Smartphone milieu de gamme
// - Tablette
```

**2. Profiler régulièrement**
```pascal
{$IFDEF DEBUG}
  // Mesurer performances à chaque sprint
  MesurerTempsChargement;
  MesurerUtilisationMemoire;
  MesurerFramerate;
{$ENDIF}
```

**3. Charger progressivement**
```pascal
// Afficher l'interface rapidement
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Afficher l'essentiel immédiatement
  AfficherInterfaceMinimale;

  // Charger le reste en arrière-plan
  TTask.Run(procedure
  begin
    ChargerDonneesSecondaires;
    TThread.Synchronize(nil, procedure
    begin
      AfficherInterfaceComplete;
    end);
  end);
end;
```

**4. Gérer les états de l'application**
```pascal
// Économiser la batterie en arrière-plan
procedure TForm1.ApplicationEnteredBackground;  
begin  
  // Suspendre les timers
  Timer1.Enabled := False;

  // Arrêter les animations
  ArreterAnimations;

  // Fermer les connexions réseau
  FermerConnexions;
end;
```

**5. Utiliser les composants optimisés**
```pascal
// Préférer TListView à TListBox (virtualisation)
// Préférer TImageList pour multiples images
// Utiliser TBitmapListAnimation pour animations d'images
```

**6. Limiter les redessins**
```pascal
// Utiliser BeginUpdate/EndUpdate
ListBox1.BeginUpdate;  
try  
  for i := 1 to 1000 do
    ListBox1.Items.Add('Item ' + i.ToString);
finally
  ListBox1.EndUpdate;  // Un seul redessinage
end;
```

### ❌ À ÉVITER

**1. Charger toutes les données au démarrage**
```pascal
// ❌ MAUVAIS
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  ChargerToutesLesImages;  // Lourd
  ChargerToutesBDD;         // Lent
  InitialiserTout;          // Long
  // L'utilisateur attend 10 secondes...
end;
```

**2. Animations et effets excessifs**
```pascal
// ❌ MAUVAIS : Trop d'effets
for i := 0 to 50 do  
begin  
  var Shadow := TShadowEffect.Create(Buttons[i]);
  var Glow := TGlowEffect.Create(Buttons[i]);
  var Blur := TBlurEffect.Create(Buttons[i]);
  // = Framerate catastrophique
end;
```

**3. Synchronisation constante**
```pascal
// ❌ MAUVAIS
Timer1.Interval := 100;  // 10 fois par seconde

procedure TForm1.Timer1Timer(Sender: TObject);  
begin  
  // Requête réseau toutes les 100ms = batterie vide rapidement
  SynchroniserServeur;
end;
```

**4. Ignorer la mémoire**
```pascal
// ❌ MAUVAIS : Fuites mémoire
procedure TForm1.CreerObjets;  
begin  
  for i := 1 to 1000 do
  begin
    var Obj := TMonObjet.Create;  // Jamais libéré !
    Obj.Traiter;
  end;
  // = Crash après quelques minutes
end;
```

**5. Blocage du thread principal**
```pascal
// ❌ MAUVAIS
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  Sleep(5000);  // Interface gelée 5 secondes
  // L'utilisateur pense que l'app a crashé
end;
```

## 12. Checklist d'optimisation

Avant de déployer votre application mobile :

**Performance** :
- [ ] FPS > 30 sur appareil entrée de gamme
- [ ] Temps de démarrage < 3 secondes
- [ ] Scroll fluide dans les listes
- [ ] Pas de freeze/blocage

**Mémoire** :
- [ ] Utilisation < 100 MB sur appareil moyen
- [ ] Pas de fuite mémoire (vérifier avec profiler)
- [ ] Libération ressources en arrière-plan
- [ ] Images redimensionnées pour mobile

**Batterie** :
- [ ] Pas de synchronisation constante
- [ ] GPS utilisé uniquement si nécessaire
- [ ] Animations limitées et optimisées
- [ ] Activités suspendues en arrière-plan

**Réseau** :
- [ ] Requêtes regroupées
- [ ] Cache des données
- [ ] Timeout configuré
- [ ] Gestion des erreurs réseau

**Expérience utilisateur** :
- [ ] Feedback visuel immédiat
- [ ] Indicateurs de chargement
- [ ] Messages d'erreur clairs
- [ ] Pas de crash

## Conclusion

L'optimisation mobile est un aspect crucial du développement avec FireMonkey. Les points clés à retenir :

📱 **Contraintes** : Batterie, mémoire et processeur limités

📱 **Interface** : Simplifier et limiter les effets visuels

📱 **Mémoire** : Gérer activement et libérer agressivement

📱 **Images** : Redimensionner et compresser pour mobile

📱 **Animations** : Limiter et utiliser les APIs optimisées

📱 **Réseau** : Regrouper les requêtes et cacher les données

📱 **Asynchrone** : Déporter les traitements lourds

📱 **Profiling** : Mesurer et optimiser régulièrement

📱 **Tests** : Tester sur appareils réels de différentes gammes

Une application bien optimisée offre une expérience fluide, ne vide pas la batterie, et sera mieux notée par les utilisateurs. Investir du temps dans l'optimisation mobile est essentiel pour le succès de votre application.

⏭️ [Animations et effets visuels](/05-developpement-multi-plateforme-avec-firemonkey/09-animations-et-effets-visuels.md)
