# 3.8 Modèles de conception (Design Patterns)

Les modèles de conception, ou "design patterns" en anglais, sont des solutions éprouvées à des problèmes courants de conception logicielle. Ce sont des "recettes" qui décrivent comment résoudre un problème spécifique de manière efficace et réutilisable. Dans cette section, nous allons découvrir les principaux modèles de conception et voir comment les implémenter en Object Pascal.

## Pourquoi utiliser des modèles de conception ?

Les modèles de conception offrent plusieurs avantages :

1. **Solutions éprouvées** : Ce sont des solutions qui ont fait leurs preuves et sont utilisées par des développeurs expérimentés
2. **Vocabulaire commun** : Ils fournissent un langage standard pour parler de conception logicielle
3. **Réutilisabilité** : Ils favorisent des conceptions réutilisables et évolutives
4. **Qualité** : Ils contribuent à créer des logiciels plus maintenables et extensibles

## Catégories de modèles de conception

Les modèles de conception sont généralement classés en trois catégories :

1. **Modèles de création** : Comment créer des objets de manière flexible
2. **Modèles structurels** : Comment organiser les classes et les objets
3. **Modèles comportementaux** : Comment les objets interagissent et se comportent

Examinons quelques modèles courants de chaque catégorie et comment les implémenter en Delphi.

## Modèles de création

### Singleton

Le modèle Singleton garantit qu'une classe n'a qu'une seule instance et fournit un point d'accès global à cette instance.

**Cas d'utilisation** : Quand vous avez besoin d'exactement une instance d'une classe, comme un gestionnaire de configuration, un journal d'application ou une connexion à une base de données.

```pascal
unit ConfigurationManager;

interface

type
  TConfigurationManager = class
  private
    class var FInstance: TConfigurationManager;
    FConfigFile: string;

    constructor Create;
  public
    class function GetInstance: TConfigurationManager;
    class procedure ReleaseInstance;

    procedure LoadConfiguration;
    procedure SaveConfiguration;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  end;

implementation

constructor TConfigurationManager.Create;
begin
  inherited Create;
  FConfigFile := 'config.ini';
  // Initialisation
end;

class function TConfigurationManager.GetInstance: TConfigurationManager;
begin
  if FInstance = nil then
    FInstance := TConfigurationManager.Create;
  Result := FInstance;
end;

class procedure TConfigurationManager.ReleaseInstance;
begin
  FInstance.Free;
  FInstance := nil;
end;

// Autres méthodes d'implémentation...

end.
```

Utilisation :

```pascal
var
  Config: TConfigurationManager;
begin
  Config := TConfigurationManager.GetInstance;
  Config.LoadConfiguration;

  ShowMessage('Serveur : ' + Config.GetValue('ServerName'));

  Config.SetValue('Port', '8080');
  Config.SaveConfiguration;

  // Ne pas libérer Config avec Free!
  // À la fin de l'application :
  // TConfigurationManager.ReleaseInstance;
end;
```

### Fabrique (Factory)

Le modèle Fabrique fournit une interface pour créer des objets sans spécifier leurs classes concrètes.

**Cas d'utilisation** : Quand vous voulez créer des objets sans connaître leur type exact à l'avance, ou quand vous voulez déléguer la création d'objets à des sous-classes.

```pascal
unit AnimalFactory;

interface

type
  TAnimal = class
  public
    procedure Manger; virtual; abstract;
    procedure Parler; virtual; abstract;
  end;

  TChien = class(TAnimal)
  public
    procedure Manger; override;
    procedure Parler; override;
  end;

  TChat = class(TAnimal)
  public
    procedure Manger; override;
    procedure Parler; override;
  end;

  TTypeAnimal = (taChien, taChat);

  TAnimalFactory = class
  public
    function CreerAnimal(TypeAnimal: TTypeAnimal): TAnimal;
  end;

implementation

procedure TChien.Manger;
begin
  ShowMessage('Le chien mange sa nourriture.');
end;

procedure TChien.Parler;
begin
  ShowMessage('Wouf!');
end;

procedure TChat.Manger;
begin
  ShowMessage('Le chat mange des croquettes.');
end;

procedure TChat.Parler;
begin
  ShowMessage('Miaou!');
end;

function TAnimalFactory.CreerAnimal(TypeAnimal: TTypeAnimal): TAnimal;
begin
  case TypeAnimal of
    taChien: Result := TChien.Create;
    taChat: Result := TChat.Create;
    else
      Result := nil;
  end;
end;

end.
```

Utilisation :

```pascal
var
  Factory: TAnimalFactory;
  Animal: TAnimal;
begin
  Factory := TAnimalFactory.Create;
  try
    // Créer un chien
    Animal := Factory.CreerAnimal(taChien);
    try
      Animal.Parler;  // Affiche "Wouf!"
      Animal.Manger;  // Affiche "Le chien mange sa nourriture."
    finally
      Animal.Free;
    end;

    // Créer un chat
    Animal := Factory.CreerAnimal(taChat);
    try
      Animal.Parler;  // Affiche "Miaou!"
      Animal.Manger;  // Affiche "Le chat mange des croquettes."
    finally
      Animal.Free;
    end;
  finally
    Factory.Free;
  end;
end;
```

## Modèles structurels

### Adaptateur (Adapter)

Le modèle Adaptateur permet à des interfaces incompatibles de travailler ensemble en convertissant l'interface d'une classe en une autre interface attendue par le client.

**Cas d'utilisation** : Quand vous devez utiliser une classe existante mais que son interface ne correspond pas à celle dont vous avez besoin.

```pascal
unit LegacySystemAdapter;

interface

type
  // Interface attendue par notre application
  IModernLogger = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure LogInfo(const Message: string);
    procedure LogError(const ErrorMessage: string);
  end;

  // Classe hérité d'un ancien système que nous ne pouvons pas modifier
  TOldLogger = class
  public
    procedure WriteLog(const Text: string);
    procedure WriteError(const ErrorCode: Integer; const Text: string);
  end;

  // Adaptateur qui fait le pont entre les deux
  TLoggerAdapter = class(TInterfacedObject, IModernLogger)
  private
    FOldLogger: TOldLogger;
  public
    constructor Create(OldLogger: TOldLogger);
    destructor Destroy; override;

    // Implémentation de IModernLogger
    procedure LogInfo(const Message: string);
    procedure LogError(const ErrorMessage: string);
  end;

implementation

// Implémentation de TOldLogger (simulation)
procedure TOldLogger.WriteLog(const Text: string);
begin
  // Écrit dans un fichier journal au format ancien
  ShowMessage('ANCIEN SYSTÈME : ' + Text);
end;

procedure TOldLogger.WriteError(const ErrorCode: Integer; const Text: string);
begin
  // Écrit une erreur au format ancien
  ShowMessage('ANCIEN SYSTÈME ERREUR #' + IntToStr(ErrorCode) + ' : ' + Text);
end;

// Implémentation de l'adaptateur
constructor TLoggerAdapter.Create(OldLogger: TOldLogger);
begin
  inherited Create;
  FOldLogger := OldLogger;
end;

destructor TLoggerAdapter.Destroy;
begin
  // Ne pas libérer FOldLogger ici
  inherited;
end;

procedure TLoggerAdapter.LogInfo(const Message: string);
begin
  FOldLogger.WriteLog(Message);
end;

procedure TLoggerAdapter.LogError(const ErrorMessage: string);
begin
  FOldLogger.WriteError(1, ErrorMessage);  // Code d'erreur arbitraire
end;

end.
```

Utilisation :

```pascal
var
  OldLogger: TOldLogger;
  Logger: IModernLogger;
begin
  OldLogger := TOldLogger.Create;
  try
    // Créer un adaptateur qui implémente notre interface moderne
    Logger := TLoggerAdapter.Create(OldLogger);

    // Utiliser l'interface moderne
    Logger.LogInfo('Test d''information');
    Logger.LogError('Quelque chose a mal tourné');

    // Pas besoin de libérer Logger (interface)
  finally
    OldLogger.Free;
  end;
end;
```

### Décorateur (Decorator)

Le modèle Décorateur permet d'ajouter des comportements à des objets de manière dynamique sans modifier leur structure.

**Cas d'utilisation** : Quand vous voulez ajouter des responsabilités à des objets individuels de manière dynamique et transparente, sans affecter d'autres objets.

```pascal
unit TextProcessor;

interface

type
  ITextProcessor = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function ProcessText(const Text: string): string;
  end;

  // Classe de base qui traite le texte
  TBaseTextProcessor = class(TInterfacedObject, ITextProcessor)
  public
    function ProcessText(const Text: string): string; virtual;
  end;

  // Décorateur abstrait
  TTextProcessorDecorator = class(TInterfacedObject, ITextProcessor)
  private
    FWrapped: ITextProcessor;
  public
    constructor Create(Processor: ITextProcessor);
    function ProcessText(const Text: string): string; virtual;
  end;

  // Décorateurs concrets
  TUpperCaseDecorator = class(TTextProcessorDecorator)
  public
    function ProcessText(const Text: string): string; override;
  end;

  TTrimDecorator = class(TTextProcessorDecorator)
  public
    function ProcessText(const Text: string): string; override;
  end;

  TBracketDecorator = class(TTextProcessorDecorator)
  public
    function ProcessText(const Text: string): string; override;
  end;

implementation

// Implémentation du processeur de base
function TBaseTextProcessor.ProcessText(const Text: string): string;
begin
  Result := Text;  // Passe simplement le texte tel quel
end;

// Implémentation du décorateur abstrait
constructor TTextProcessorDecorator.Create(Processor: ITextProcessor);
begin
  inherited Create;
  FWrapped := Processor;
end;

function TTextProcessorDecorator.ProcessText(const Text: string): string;
begin
  Result := FWrapped.ProcessText(Text);  // Délègue au processeur enveloppé
end;

// Implémentations des décorateurs concrets
function TUpperCaseDecorator.ProcessText(const Text: string): string;
begin
  Result := UpperCase(inherited ProcessText(Text));
end;

function TTrimDecorator.ProcessText(const Text: string): string;
begin
  Result := Trim(inherited ProcessText(Text));
end;

function TBracketDecorator.ProcessText(const Text: string): string;
begin
  Result := '[' + inherited ProcessText(Text) + ']';
end;

end.
```

Utilisation :

```pascal
var
  Processor: ITextProcessor;
begin
  // Créer un processeur de base
  Processor := TBaseTextProcessor.Create;

  // Décorer le processeur dans n'importe quel ordre
  Processor := TTrimDecorator.Create(Processor);
  Processor := TUpperCaseDecorator.Create(Processor);
  Processor := TBracketDecorator.Create(Processor);

  // Utiliser le processeur décoré
  ShowMessage(Processor.ProcessText('  hello world  '));  // Affiche "[HELLO WORLD]"

  // Pas besoin de libérer Processor (interface)
end;
```

## Modèles comportementaux

### Observateur (Observer)

Le modèle Observateur permet de définir une dépendance un-à-plusieurs entre des objets, de sorte que lorsqu'un objet change d'état, tous ses observateurs sont notifiés et mis à jour automatiquement.

**Cas d'utilisation** : Quand vous avez des objets qui doivent être informés des changements d'état d'un autre objet, comme une interface utilisateur qui doit refléter les changements dans les données.

```pascal
unit WeatherStation;

interface

uses
  System.Generics.Collections;

type
  // Interface pour les observateurs
  IWeatherObserver = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Update(const Temperature, Humidity, Pressure: Double);
  end;

  // Le sujet qui est observé
  TWeatherStation = class
  private
    FObservers: TList<IWeatherObserver>;
    FTemperature: Double;
    FHumidity: Double;
    FPressure: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterObserver(Observer: IWeatherObserver);
    procedure RemoveObserver(Observer: IWeatherObserver);
    procedure NotifyObservers;

    procedure SetMeasurements(Temperature, Humidity, Pressure: Double);

    property Temperature: Double read FTemperature;
    property Humidity: Double read FHumidity;
    property Pressure: Double read FPressure;
  end;

  // Observateurs concrets
  TCurrentConditionsDisplay = class(TInterfacedObject, IWeatherObserver)
  private
    FTemperature: Double;
    FHumidity: Double;
  public
    procedure Update(const Temperature, Humidity, Pressure: Double);
    procedure Display;
  end;

  TStatisticsDisplay = class(TInterfacedObject, IWeatherObserver)
  private
    FTemperatureSum: Double;
    FReadingsCount: Integer;
    FMaxTemperature: Double;
    FMinTemperature: Double;
  public
    constructor Create;
    procedure Update(const Temperature, Humidity, Pressure: Double);
    procedure Display;
  end;

implementation

uses
  System.SysUtils;

{ TWeatherStation }

constructor TWeatherStation.Create;
begin
  inherited Create;
  FObservers := TList<IWeatherObserver>.Create;
end;

destructor TWeatherStation.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TWeatherStation.RegisterObserver(Observer: IWeatherObserver);
begin
  FObservers.Add(Observer);
end;

procedure TWeatherStation.RemoveObserver(Observer: IWeatherObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TWeatherStation.NotifyObservers;
var
  Observer: IWeatherObserver;
begin
  for Observer in FObservers do
    Observer.Update(FTemperature, FHumidity, FPressure);
end;

procedure TWeatherStation.SetMeasurements(Temperature, Humidity, Pressure: Double);
begin
  FTemperature := Temperature;
  FHumidity := Humidity;
  FPressure := Pressure;
  NotifyObservers;
end;

{ TCurrentConditionsDisplay }

procedure TCurrentConditionsDisplay.Update(const Temperature, Humidity, Pressure: Double);
begin
  FTemperature := Temperature;
  FHumidity := Humidity;
  Display;
end;

procedure TCurrentConditionsDisplay.Display;
begin
  ShowMessage(Format('Conditions actuelles : %.1f°C et %.1f%% d''humidité',
                    [FTemperature, FHumidity]));
end;

{ TStatisticsDisplay }

constructor TStatisticsDisplay.Create;
begin
  inherited;
  FTemperatureSum := 0;
  FReadingsCount := 0;
  FMaxTemperature := -1000;
  FMinTemperature := 1000;
end;

procedure TStatisticsDisplay.Update(const Temperature, Humidity, Pressure: Double);
begin
  FTemperatureSum := FTemperatureSum + Temperature;
  Inc(FReadingsCount);

  if Temperature > FMaxTemperature then
    FMaxTemperature := Temperature;

  if Temperature < FMinTemperature then
    FMinTemperature := Temperature;

  Display;
end;

procedure TStatisticsDisplay.Display;
begin
  if FReadingsCount > 0 then
    ShowMessage(Format('Statistiques de température : Moy=%.1f, Max=%.1f, Min=%.1f',
                      [FTemperatureSum / FReadingsCount, FMaxTemperature, FMinTemperature]));
end;

end.
```

Utilisation :

```pascal
var
  WeatherStation: TWeatherStation;
  CurrentDisplay: IWeatherObserver;
  StatisticsDisplay: IWeatherObserver;
begin
  WeatherStation := TWeatherStation.Create;
  try
    // Créer des observateurs
    CurrentDisplay := TCurrentConditionsDisplay.Create;
    StatisticsDisplay := TStatisticsDisplay.Create;

    // Enregistrer les observateurs
    WeatherStation.RegisterObserver(CurrentDisplay);
    WeatherStation.RegisterObserver(StatisticsDisplay);

    // Simuler des changements météo
    WeatherStation.SetMeasurements(27.5, 65, 1013.1);
    WeatherStation.SetMeasurements(28.2, 70, 1012.5);
    WeatherStation.SetMeasurements(26.8, 75, 1010.2);

    // Retirer un observateur
    WeatherStation.RemoveObserver(CurrentDisplay);

    WeatherStation.SetMeasurements(25.9, 80, 1009.7);

    // Pas besoin de libérer CurrentDisplay ou StatisticsDisplay (interfaces)
  finally
    WeatherStation.Free;
  end;
end;
```

### Stratégie (Strategy)

Le modèle Stratégie définit une famille d'algorithmes, encapsule chacun d'eux et les rend interchangeables. Il permet de changer l'algorithme indépendamment des clients qui l'utilisent.

**Cas d'utilisation** : Quand vous avez plusieurs manières de faire quelque chose et que vous voulez changer l'algorithme à l'exécution.

```pascal
unit SortingStrategies;

interface

type
  // Interface pour les stratégies de tri
  ISortStrategy = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Sort(var Items: array of Integer);
  end;

  // Contexte qui utilise la stratégie
  TSorter = class
  private
    FSortStrategy: ISortStrategy;
  public
    procedure SetStrategy(Strategy: ISortStrategy);
    procedure SortItems(var Items: array of Integer);
  end;

  // Stratégies concrètes
  TBubbleSortStrategy = class(TInterfacedObject, ISortStrategy)
  public
    procedure Sort(var Items: array of Integer);
  end;

  TQuickSortStrategy = class(TInterfacedObject, ISortStrategy)
  public
    procedure Sort(var Items: array of Integer);
  private
    procedure QuickSort(var Items: array of Integer; Left, Right: Integer);
    function Partition(var Items: array of Integer; Left, Right: Integer): Integer;
  end;

  TInsertionSortStrategy = class(TInterfacedObject, ISortStrategy)
  public
    procedure Sort(var Items: array of Integer);
  end;

implementation

{ TSorter }

procedure TSorter.SetStrategy(Strategy: ISortStrategy);
begin
  FSortStrategy := Strategy;
end;

procedure TSorter.SortItems(var Items: array of Integer);
begin
  if Assigned(FSortStrategy) then
    FSortStrategy.Sort(Items);
end;

{ TBubbleSortStrategy }

procedure TBubbleSortStrategy.Sort(var Items: array of Integer);
var
  I, J, Temp: Integer;
  Swapped: Boolean;
begin
  for I := High(Items) downto Low(Items) do
  begin
    Swapped := False;
    for J := Low(Items) to I - 1 do
    begin
      if Items[J] > Items[J + 1] then
      begin
        Temp := Items[J];
        Items[J] := Items[J + 1];
        Items[J + 1] := Temp;
        Swapped := True;
      end;
    end;
    if not Swapped then Break;
  end;
end;

{ TQuickSortStrategy }

procedure TQuickSortStrategy.Sort(var Items: array of Integer);
begin
  if Length(Items) > 1 then
    QuickSort(Items, Low(Items), High(Items));
end;

procedure TQuickSortStrategy.QuickSort(var Items: array of Integer; Left, Right: Integer);
var
  PivotIndex: Integer;
begin
  if Left < Right then
  begin
    PivotIndex := Partition(Items, Left, Right);
    QuickSort(Items, Left, PivotIndex - 1);
    QuickSort(Items, PivotIndex + 1, Right);
  end;
end;

function TQuickSortStrategy.Partition(var Items: array of Integer; Left, Right: Integer): Integer;
var
  Pivot, Temp, I, J: Integer;
begin
  Pivot := Items[Right];
  I := Left - 1;

  for J := Left to Right - 1 do
  begin
    if Items[J] <= Pivot then
    begin
      Inc(I);
      Temp := Items[I];
      Items[I] := Items[J];
      Items[J] := Temp;
    end;
  end;

  Temp := Items[I + 1];
  Items[I + 1] := Items[Right];
  Items[Right] := Temp;

  Result := I + 1;
end;

{ TInsertionSortStrategy }

procedure TInsertionSortStrategy.Sort(var Items: array of Integer);
var
  I, J, Temp: Integer;
begin
  for I := Low(Items) + 1 to High(Items) do
  begin
    Temp := Items[I];
    J := I - 1;

    while (J >= Low(Items)) and (Items[J] > Temp) do
    begin
      Items[J + 1] := Items[J];
      Dec(J);
    end;

    Items[J + 1] := Temp;
  end;
end;

end.
```

Utilisation :

```pascal
var
  Sorter: TSorter;
  Items: array of Integer;
  I: Integer;
  StartTime, EndTime: Cardinal;

  procedure PrintArray;
  var
    S: string;
    I: Integer;
  begin
    S := 'Items: ';
    for I := Low(Items) to High(Items) do
      S := S + IntToStr(Items[I]) + ' ';
    ShowMessage(S);
  end;

begin
  // Création du tableau à trier
  SetLength(Items, 10);
  Items[0] := 64; Items[1] := 34; Items[2] := 25; Items[3] := 12; Items[4] := 22;
  Items[5] := 11; Items[6] := 90; Items[7] := 87; Items[8] := 45; Items[9] := 56;

  Sorter := TSorter.Create;
  try
    // Utiliser la stratégie de tri à bulles
    PrintArray;
    Sorter.SetStrategy(TBubbleSortStrategy.Create);
    StartTime := GetTickCount;
    Sorter.SortItems(Items);
    EndTime := GetTickCount;
    PrintArray;
    ShowMessage('Tri à bulles : ' + IntToStr(EndTime - StartTime) + ' ms');

    // Remélanger le tableau
    Items[0] := 64; Items[1] := 34; Items[2] := 25; Items[3] := 12; Items[4] := 22;
    Items[5] := 11; Items[6] := 90; Items[7] := 87; Items[8] := 45; Items[9] := 56;

    // Utiliser la stratégie de tri rapide
    PrintArray;
    Sorter.SetStrategy(TQuickSortStrategy.Create);
    StartTime := GetTickCount;
    Sorter.SortItems(Items);
    EndTime := GetTickCount;
    PrintArray;
    ShowMessage('Tri rapide : ' + IntToStr(EndTime - StartTime) + ' ms');

    // Remélanger le tableau
    Items[0] := 64; Items[1] := 34; Items[2] := 25; Items[3] := 12; Items[4] := 22;
    Items[5] := 11; Items[6] := 90; Items[7] := 87; Items[8] := 45; Items[9] := 56;

    // Utiliser la stratégie de tri par insertion
    PrintArray;
    Sorter.SetStrategy(TInsertionSortStrategy.Create);
    StartTime := GetTickCount;
    Sorter.SortItems(Items);
    EndTime := GetTickCount;
    PrintArray;
    ShowMessage('Tri par insertion : ' + IntToStr(EndTime - StartTime) + ' ms');
  finally
    Sorter.Free;
  end;
end;
```

## Utilisation des modèles de conception dans Delphi

Delphi utilise déjà de nombreux modèles de conception dans son architecture :

- **Singleton** : `Application`, `Screen`, et `DataModule` sont des singletons
- **Fabrique** : `TComponent.Create` agit comme une fabrique pour créer des composants
- **Décorateur** : Les filtres de flux (`TStream`) comme `TBufferedFileStream`
- **Observateur** : Le système d'événements (comme `OnClick`, `OnChange`) implémente le modèle observateur
- **Stratégie** : Les adaptateurs de données (`TDataSet` avec différentes implémentations)

## Comment choisir le bon modèle de conception ?

Voici quelques conseils pour choisir le modèle approprié :

1. **Identifiez le problème** : Quel problème essayez-vous de résoudre ?
2. **Connaissez les modèles** : Familiarisez-vous avec les modèles courants
3. **Considérez la simplicité** : Le modèle le plus simple qui répond au besoin est souvent le meilleur
4. **Pensez à l'évolution** : Comment votre application pourrait-elle évoluer ?
5. **Évitez la sur-ingénierie** : N'utilisez pas un modèle juste parce qu'il est "cool"

## Bonnes pratiques

1. **Apprenez progressivement** : Commencez par les modèles les plus simples et les plus courants
2. **Pratiquez** : Implémentez des modèles dans des petits projets pour les comprendre
3. **Documentez** : Indiquez quels modèles vous utilisez et pourquoi
4. **Ne réinventez pas la roue** : Utilisez les bibliothèques existantes quand elles implémentent le modèle dont vous avez besoin
5. **Adaptez** : Les modèles sont des guides, pas des règles rigides

## Modèles supplémentaires courants

Voici quelques autres modèles que vous rencontrerez souvent :

### 1. Builder (Constructeur)

Sépare la construction d'un objet complexe de sa représentation.

### 2. Prototype

Crée de nouveaux objets en clonant un objet existant.

### 3. Composite

Compose des objets en structures arborescentes pour représenter des hiérarchies partie-tout.

### 4. Façade (Facade)

Fournit une interface unifiée à un ensemble d'interfaces dans un sous-système.

### 5. Commande (Command)

Encapsule une demande sous forme d'objet, permettant de paramétrer des clients avec différentes demandes.

### 6. État (State)

Permet à un objet de modifier son comportement quand son état interne change.

### 7. Chaîne de responsabilité (Chain of Responsibility)

Passe une requête le long d'une chaîne de gestionnaires.

### 8. Visiteur (Visitor)

Représente une opération à effectuer sur les éléments d'une structure d'objets.

---

Les modèles de conception sont des outils puissants dans votre boîte à outils de développeur. Ils vous aident à structurer votre code de manière élégante et à résoudre des problèmes courants. En comprenant et en appliquant ces modèles dans vos applications Delphi, vous créerez un code plus maintenable, plus flexible et plus robuste.

Dans la prochaine section, nous explorerons comment organiser votre code source de manière efficace et favoriser la modularité dans vos applications Delphi.
