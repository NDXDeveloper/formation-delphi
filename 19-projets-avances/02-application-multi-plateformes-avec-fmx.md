# 19.2 Application multi-plateformes avec FireMonkey (FMX)

## Introduction

FireMonkey (FMX) est le framework multi-plateformes de Delphi qui vous permet de développer une application unique pouvant s'exécuter sur Windows, macOS, iOS, Android et Linux. Dans ce tutoriel, nous allons créer une application simple de gestion de tâches qui fonctionnera sur toutes ces plateformes avec un minimum d'adaptations.

## Prérequis

- Delphi 11 Alexandria ou Delphi 12 Athens
- Connaissance de base de l'interface Delphi et du langage Object Pascal
- Les SDK installés pour les plateformes cibles (pour Android/iOS si vous souhaitez déployer sur mobile)

## Objectifs du projet

Notre application "TaskMaster" offrira les fonctionnalités suivantes :
- Liste des tâches à faire
- Ajout/modification/suppression de tâches
- Marquage des tâches comme terminées
- Sauvegarde locale des données
- Interface adaptative selon la taille d'écran

## 1. Création du projet

Commençons par créer un nouveau projet FireMonkey multi-plateformes :

1. Lancez Delphi et sélectionnez **Fichier > Nouveau > Application Multi-périphériques**.
2. Dans la boîte de dialogue qui s'affiche, sélectionnez **Formulaire vide**.
3. Cliquez sur **OK** pour créer le projet.

## 2. Configuration du projet

Avant de commencer à concevoir l'interface, configurons notre projet :

1. Dans le **Project Manager** (généralement à droite), cliquez-droit sur le nom du projet et sélectionnez **Options du projet**.
2. Allez dans **Application > Apparence** et définissez :
   - Titre de l'application : "TaskMaster"
   - Icône : ajoutez une icône si vous en avez une
3. Dans l'onglet **Version Info**, ajoutez les informations de version.
4. Dans **Build Configurations**, vérifiez les plateformes cibles (Windows 32-bit, Windows 64-bit, Android, iOS, macOS, Linux).

## 3. Conception de l'interface utilisateur

### Structure de base

1. Sélectionnez la fiche principale (Form1) dans l'éditeur.
2. Dans l'**Inspecteur d'objets** (généralement à droite), modifiez les propriétés :
   - **Name** : `MainForm`
   - **Caption** : `TaskMaster`
   - **Fill** : définissez une couleur de fond claire

3. Ajoutez un `TLayout` depuis la palette et configurez-le :
   - **Name** : `HeaderLayout`
   - **Align** : `Top`
   - **Height** : `60`
   - **Padding** : `10,5,10,5` (gauche, haut, droite, bas)
   - **Fill** : définissez une couleur plus foncée pour l'en-tête

4. Ajoutez un `TLabel` dans le HeaderLayout :
   - **Name** : `TitleLabel`
   - **Text** : `TaskMaster`
   - **Align** : `Client`
   - **TextSettings.Font.Size** : `18`
   - **TextSettings.Font.Style** : `[fsBold]`
   - **StyledSettings** : désactivez `[Family, Size, Style]`

5. Ajoutez un `TButton` dans le HeaderLayout :
   - **Name** : `AddButton`
   - **Text** : `+`
   - **Align** : `Right`
   - **Width** : `40`
   - **TextSettings.Font.Size** : `20`

### Liste des tâches

1. Ajoutez un `TListView` sous le HeaderLayout :
   - **Name** : `TaskListView`
   - **Align** : `Client`
   - **SearchVisible** : `True` (pour permettre la recherche)
   - **ShowSelection** : `True`
   - **ItemAppearance.ItemAppearance** : `ImageListText` (pour avoir du texte et une case à cocher)

2. Ajoutez un `TRectangle` pour afficher quand la liste est vide :
   - **Name** : `EmptyNotification`
   - **Align** : `Client`
   - **Fill.Color** : `claWhite`
   - **Opacity** : `0.8`
   - **Visible** : `False`

3. Ajoutez un `TLabel` dans le EmptyNotification :
   - **Name** : `EmptyLabel`
   - **Align** : `Center`
   - **Text** : `Aucune tâche. Appuyez sur + pour ajouter.`
   - **TextSettings.Font.Size** : `14`
   - **TextSettings.FontColor** : `#707070`

## 4. Création du formulaire d'ajout de tâche

1. Ajoutez un nouveau formulaire : **Fichier > Nouveau > Multi-périphériques > Formulaire vide**.
2. Configurez ses propriétés :
   - **Name** : `TaskFormUnit` (dans le fichier source)
   - Pour la Form : **Name** : `TaskForm`
   - **Caption** : `Nouvelle tâche`

3. Créez l'interface d'ajout :
   - Ajoutez un `TLayout` pour l'en-tête avec un label et un bouton retour
   - Ajoutez un `TEdit` pour le titre de la tâche
   - Ajoutez un `TMemo` pour la description
   - Ajoutez un `TDateEdit` pour la date limite
   - Ajoutez un `TButton` pour sauvegarder

## 5. Définition de la structure de données

Créons une unité pour définir notre structure de tâche :

1. **Fichier > Nouveau > Unité - Delphi**.
2. Enregistrez-la sous le nom `TaskDataUnit.pas`.
3. Ajoutez le code suivant :

```pascal
unit TaskDataUnit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TTaskPriority = (tpLow, tpNormal, tpHigh);

  TTaskItem = class
  private
    FID: TGUID;
    FTitle: string;
    FDescription: string;
    FDueDate: TDateTime;
    FCompleted: Boolean;
    FPriority: TTaskPriority;
    FCreationDate: TDateTime;
  public
    constructor Create;
    function Clone: TTaskItem;

    property ID: TGUID read FID write FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property Completed: Boolean read FCompleted write FCompleted;
    property Priority: TTaskPriority read FPriority write FPriority;
    property CreationDate: TDateTime read FCreationDate;
  end;

  TTaskList = class(TObjectList<TTaskItem>)
  private
    FSaveFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile;
    procedure SaveToFile;
    function AddTask(const Title, Description: string; DueDate: TDateTime;
      Priority: TTaskPriority): TTaskItem;
    function FindTaskByID(const ID: TGUID): TTaskItem;
  end;

implementation

uses
  System.IOUtils, System.JSON;

{ TTaskItem }

constructor TTaskItem.Create;
begin
  inherited;
  CreateGUID(FID);
  FCreationDate := Now;
  FCompleted := False;
  FPriority := tpNormal;
end;

function TTaskItem.Clone: TTaskItem;
begin
  Result := TTaskItem.Create;
  Result.FID := FID;
  Result.FTitle := FTitle;
  Result.FDescription := FDescription;
  Result.FDueDate := FDueDate;
  Result.FCompleted := FCompleted;
  Result.FPriority := FPriority;
  Result.FCreationDate := FCreationDate;
end;

{ TTaskList }

constructor TTaskList.Create;
begin
  inherited Create(True); // Own objects

  // Détermine le chemin de sauvegarde selon la plateforme
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  FSaveFileName := TPath.Combine(TPath.GetDocumentsPath, 'tasks.json');
  {$ELSE}
  FSaveFileName := TPath.Combine(TPath.GetHomePath, 'TaskMaster', 'tasks.json');
  // S'assurer que le répertoire existe
  if not TDirectory.Exists(TPath.GetDirectoryName(FSaveFileName)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(FSaveFileName));
  {$ENDIF}

  LoadFromFile;
end;

destructor TTaskList.Destroy;
begin
  SaveToFile;
  inherited;
end;

function TTaskList.AddTask(const Title, Description: string; DueDate: TDateTime;
  Priority: TTaskPriority): TTaskItem;
begin
  Result := TTaskItem.Create;
  Result.Title := Title;
  Result.Description := Description;
  Result.DueDate := DueDate;
  Result.Priority := Priority;
  Add(Result);
  SaveToFile;
end;

function TTaskList.FindTaskByID(const ID: TGUID): TTaskItem;
var
  Task: TTaskItem;
begin
  Result := nil;
  for Task in Self do
    if IsEqualGUID(Task.ID, ID) then
      Exit(Task);
end;

procedure TTaskList.LoadFromFile;
var
  JsonArray: TJSONArray;
  JsonValue: TJSONValue;
  JsonObject: TJSONObject;
  Task: TTaskItem;
  JSONString: string;
begin
  Clear;

  if not TFile.Exists(FSaveFileName) then
    Exit;

  try
    JSONString := TFile.ReadAllText(FSaveFileName);
    JsonArray := TJSONObject.ParseJSONValue(JSONString) as TJSONArray;

    if Assigned(JsonArray) then
    begin
      try
        for JsonValue in JsonArray do
        begin
          JsonObject := JsonValue as TJSONObject;
          Task := TTaskItem.Create;

          Task.FID := StringToGUID(JsonObject.GetValue<string>('ID'));
          Task.FTitle := JsonObject.GetValue<string>('Title');
          Task.FDescription := JsonObject.GetValue<string>('Description');
          Task.FDueDate := JsonObject.GetValue<TDateTime>('DueDate');
          Task.FCompleted := JsonObject.GetValue<Boolean>('Completed');
          Task.FPriority := TTaskPriority(JsonObject.GetValue<Integer>('Priority'));
          Task.FCreationDate := JsonObject.GetValue<TDateTime>('CreationDate');

          Add(Task);
        end;
      finally
        JsonArray.Free;
      end;
    end;
  except
    // Gérer les erreurs silencieusement pour les premiers lancements
  end;
end;

procedure TTaskList.SaveToFile;
var
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
  Task: TTaskItem;
begin
  JsonArray := TJSONArray.Create;
  try
    for Task in Self do
    begin
      JsonObject := TJSONObject.Create;
      JsonObject.AddPair('ID', GUIDToString(Task.ID));
      JsonObject.AddPair('Title', Task.Title);
      JsonObject.AddPair('Description', Task.Description);
      JsonObject.AddPair('DueDate', TJSONNumber.Create(Task.DueDate));
      JsonObject.AddPair('Completed', TJSONBool.Create(Task.Completed));
      JsonObject.AddPair('Priority', TJSONNumber.Create(Ord(Task.Priority)));
      JsonObject.AddPair('CreationDate', TJSONNumber.Create(Task.CreationDate));

      JsonArray.AddElement(JsonObject);
    end;

    TFile.WriteAllText(FSaveFileName, JsonArray.ToJSON);
  finally
    JsonArray.Free;
  end;
end;

end.
```

## 6. Implémentation du gestionnaire de tâches

Maintenant, implémentons la logique principale de notre application :

### 6.1 Liaison entre l'interface et les données

Modifier le fichier de l'unité principale (`MainFormUnit.pas`) :

```pascal
unit MainFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  TaskDataUnit, FMX.Effects;

type
  TMainForm = class(TForm)
    HeaderLayout: TLayout;
    TitleLabel: TLabel;
    AddButton: TButton;
    TaskListView: TListView;
    EmptyNotification: TRectangle;
    EmptyLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure TaskListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure TaskListViewDeleteItem(Sender: TObject; AIndex: Integer);
  private
    FTaskList: TTaskList;
    procedure RefreshTaskList;
    procedure UpdateEmptyState;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  TaskFormUnit, System.DateUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FTaskList := TTaskList.Create;
  RefreshTaskList;
  UpdateEmptyState;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTaskList.Free;
end;

procedure TMainForm.RefreshTaskList;
var
  Task: TTaskItem;
  Item: TListViewItem;
  DueText: string;
begin
  TaskListView.BeginUpdate;
  try
    TaskListView.Items.Clear;

    for Task in FTaskList do
    begin
      Item := TaskListView.Items.Add;
      Item.TagObject := Task;
      Item.Text := Task.Title;

      // Formatage de la date d'échéance
      if Task.DueDate = 0 then
        DueText := 'Sans date limite'
      else if DaysBetween(Date, Task.DueDate) = 0 then
        DueText := 'Aujourd''hui'
      else if DaysBetween(Date, Task.DueDate) = 1 then
        DueText := 'Demain'
      else if Task.DueDate < Date then
        DueText := 'En retard - ' + FormatDateTime('dd/mm/yyyy', Task.DueDate)
      else
        DueText := FormatDateTime('dd/mm/yyyy', Task.DueDate);

      Item.Detail := DueText;

      // Status de la tâche (terminée ou non)
      if Task.Completed then
      begin
        Item.Objects.AccessoryObject.Visible := True;
        Item.Objects.TextObject.TextColor := TAlphaColorRec.Gray;
        Item.Objects.DetailObject.TextColor := TAlphaColorRec.Gray;
      end
      else
      begin
        Item.Objects.AccessoryObject.Visible := False;

        // Priorité par couleur
        case Task.Priority of
          tpLow: Item.Objects.TextObject.TextColor := TAlphaColorRec.Navy;
          tpNormal: Item.Objects.TextObject.TextColor := TAlphaColorRec.Black;
          tpHigh: Item.Objects.TextObject.TextColor := TAlphaColorRec.Crimson;
        end;
      end;
    end;
  finally
    TaskListView.EndUpdate;
  end;

  UpdateEmptyState;
end;

procedure TMainForm.UpdateEmptyState;
begin
  EmptyNotification.Visible := TaskListView.Items.Count = 0;
end;

procedure TMainForm.AddButtonClick(Sender: TObject);
var
  Task: TTaskItem;
begin
  Task := nil; // Nouvelle tâche
  TaskForm.ShowTaskDialog(Task);

  if TaskForm.ModalResult = mrOK then
  begin
    FTaskList.AddTask(
      TaskForm.TitleEdit.Text,
      TaskForm.DescriptionMemo.Text,
      TaskForm.DueDateEdit.Date,
      TTaskPriority(TaskForm.PriorityComboBox.ItemIndex)
    );
    RefreshTaskList;
  end;
end;

procedure TMainForm.TaskListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  Task, EditTask: TTaskItem;
begin
  if AItem = nil then
    Exit;

  Task := AItem.TagObject as TTaskItem;

  // Double-tap pour terminer/réactiver une tâche
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  if TaskListView.Selected = AItem then
  {$ENDIF}
  begin
    Task.Completed := not Task.Completed;
    FTaskList.SaveToFile;
    RefreshTaskList;
    Exit;
  end;

  // Édition de la tâche
  EditTask := Task.Clone;
  TaskForm.ShowTaskDialog(EditTask);

  if TaskForm.ModalResult = mrOK then
  begin
    Task.Title := EditTask.Title;
    Task.Description := EditTask.Description;
    Task.DueDate := EditTask.DueDate;
    Task.Priority := EditTask.Priority;

    FTaskList.SaveToFile;
    RefreshTaskList;
  end;

  EditTask.Free;
end;

procedure TMainForm.TaskListViewDeleteItem(Sender: TObject; AIndex: Integer);
var
  Task: TTaskItem;
begin
  if (AIndex >= 0) and (AIndex < TaskListView.Items.Count) then
  begin
    Task := TaskListView.Items[AIndex].TagObject as TTaskItem;
    FTaskList.Remove(Task);
    FTaskList.SaveToFile;
    UpdateEmptyState;
  end;
end;

end.
```

### 6.2 Implémentation du formulaire d'ajout/édition

Maintenant, complétons le formulaire d'ajout/édition de tâche (`TaskFormUnit.pas`) :

```pascal
unit TaskFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox,
  FMX.Controls.Presentation, FMX.Objects, TaskDataUnit;

type
  TTaskForm = class(TForm)
    HeaderLayout: TLayout;
    TitleLabel: TLabel;
    CancelButton: TButton;
    SaveButton: TButton;
    ContentLayout: TLayout;
    TitleEdit: TEdit;
    TitleLabel2: TLabel;
    DescriptionLabel: TLabel;
    DescriptionMemo: TMemo;
    DueDateLabel: TLabel;
    DueDateEdit: TDateEdit;
    PriorityLabel: TLabel;
    PriorityComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FTask: TTaskItem;
    FIsNew: Boolean;
  public
    function ShowTaskDialog(ATask: TTaskItem): TModalResult;
  end;

var
  TaskForm: TTaskForm;

implementation

{$R *.fmx}

procedure TTaskForm.FormCreate(Sender: TObject);
begin
  // Initialisation des contrôles
  PriorityComboBox.Items.Clear;
  PriorityComboBox.Items.Add('Basse');
  PriorityComboBox.Items.Add('Normale');
  PriorityComboBox.Items.Add('Haute');
end;

function TTaskForm.ShowTaskDialog(ATask: TTaskItem): TModalResult;
begin
  // Nouvelle tâche ou édition
  FIsNew := ATask = nil;

  if FIsNew then
  begin
    TitleLabel.Text := 'Nouvelle tâche';
    TitleEdit.Text := '';
    DescriptionMemo.Text := '';
    DueDateEdit.Date := Date + 1; // Demain par défaut
    PriorityComboBox.ItemIndex := 1; // Normale par défaut
  end
  else
  begin
    FTask := ATask;
    TitleLabel.Text := 'Modifier la tâche';
    TitleEdit.Text := FTask.Title;
    DescriptionMemo.Text := FTask.Description;
    DueDateEdit.Date := FTask.DueDate;
    PriorityComboBox.ItemIndex := Ord(FTask.Priority);
  end;

  // Affichage modal
  Result := ShowModal;
end;

procedure TTaskForm.SaveButtonClick(Sender: TObject);
begin
  if Trim(TitleEdit.Text) = '' then
  begin
    ShowMessage('Veuillez entrer un titre pour cette tâche');
    TitleEdit.SetFocus;
    Exit;
  end;

  if not FIsNew then
  begin
    FTask.Title := TitleEdit.Text;
    FTask.Description := DescriptionMemo.Text;
    FTask.DueDate := DueDateEdit.Date;
    FTask.Priority := TTaskPriority(PriorityComboBox.ItemIndex);
  end;

  ModalResult := mrOK;
end;

procedure TTaskForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
```

## 7. Adaptations spécifiques aux plateformes

Pour que notre application fonctionne au mieux sur différentes plateformes, ajoutons quelques adaptations :

### 7.1 Gestion des retours arrière sur Android

Ajoutez à l'unité principale (`MainFormUnit.pas`) :

```pascal
procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  if Key = vkHardwareBack then
  begin
    Key := 0;
    // Si une boîte de dialogue est ouverte, ne rien faire
    // Sinon, demander confirmation pour quitter
    if not (TDialog.Current <> nil) then
    begin
      TDialogService.MessageDialog('Voulez-vous quitter l''application ?',
        TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
        TMsgDlgBtn.mbNo, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrYes then
            Application.Terminate;
        end);
    end;
  end;
  {$ENDIF}
end;
```

### 7.2 Adaptation à l'orientation et aux différentes tailles d'écran

Modifiez la méthode `FormResize` de l'unité principale pour gérer différentes tailles d'écran :

```pascal
procedure TMainForm.FormResize(Sender: TObject);
begin
  // Ajustement selon la largeur d'écran
  if Width > 600 then
  begin
    // Mode tablette/bureau
    TaskListView.ItemAppearance.ItemHeight := 60;
    TaskListView.ItemAppearance.ItemEditHeight := 60;
    TitleLabel.TextSettings.Font.Size := 22;
    AddButton.Width := 50;
    HeaderLayout.Height := 70;
  end
  else
  begin
    // Mode téléphone
    TaskListView.ItemAppearance.ItemHeight := 50;
    TaskListView.ItemAppearance.ItemEditHeight := 50;
    TitleLabel.TextSettings.Font.Size := 18;
    AddButton.Width := 40;
    HeaderLayout.Height := 60;
  end;
end;
```

### 7.3 Localisation et internationalisation

Pour une application vraiment multi-plateformes, la localisation est importante :

1. Créez un répertoire `Localization` dans votre projet.
2. Ajoutez un nouveau fichier `LocalizationUnit.pas` :

```pascal
unit LocalizationUnit;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles;

type
  TLocalization = class
  private
    FLanguageFile: TIniFile;
    FCurrentLanguage: string;
    class var FInstance: TLocalization;
    constructor Create;
  public
    destructor Destroy; override;
    function GetString(const Section, Ident, Default: string): string;
    procedure SetLanguage(const LanguageCode: string);
    class function GetInstance: TLocalization;
    class procedure FreeInstance;
  end;

function L(const Section, Ident, Default: string): string;

implementation

uses
  System.IOUtils;

{ TLocalization }

constructor TLocalization.Create;
begin
  inherited;
  FCurrentLanguage := 'fr'; // Langue par défaut
  SetLanguage(FCurrentLanguage);
end;

destructor TLocalization.Destroy;
begin
  FLanguageFile.Free;
  inherited;
end;

class function TLocalization.GetInstance: TLocalization;
begin
  if FInstance = nil then
    FInstance := TLocalization.Create;
  Result := FInstance;
end;

class procedure TLocalization.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

function TLocalization.GetString(const Section, Ident, Default: string): string;
begin
  if FLanguageFile <> nil then
    Result := FLanguageFile.ReadString(Section, Ident, Default)
  else
    Result := Default;
end;

procedure TLocalization.SetLanguage(const LanguageCode: string);
var
  LangFilePath: string;
begin
  FreeAndNil(FLanguageFile);
  FCurrentLanguage := LanguageCode;

  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  LangFilePath := TPath.Combine(TPath.GetDocumentsPath, 'Languages');
  {$ELSE}
  LangFilePath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Languages');
  {$ENDIF}

  if not TDirectory.Exists(LangFilePath) then
    TDirectory.CreateDirectory(LangFilePath);

  LangFilePath := TPath.Combine(LangFilePath, LanguageCode + '.lng');

  if not TFile.Exists(LangFilePath) then
    // Créer un fichier de langue par défaut si inexistant
    CreateDefaultLanguageFile(LangFilePath);

  FLanguageFile := TIniFile.Create(LangFilePath);
end;

procedure TLocalization.CreateDefaultLanguageFile(const FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    // Section principale
    Ini.WriteString('Main', 'AppTitle', 'TaskMaster');
    Ini.WriteString('Main', 'EmptyList', 'Aucune tâche. Appuyez sur + pour ajouter.');

    // Formulaire de tâche
    Ini.WriteString('TaskForm', 'NewTask', 'Nouvelle tâche');
    Ini.WriteString('TaskForm', 'EditTask', 'Modifier la tâche');
    Ini.WriteString('TaskForm', 'Title', 'Titre');
    Ini.WriteString('TaskForm', 'Description', 'Description');
    Ini.WriteString('TaskForm', 'DueDate', 'Date limite');
    Ini.WriteString('TaskForm', 'Priority', 'Priorité');
    Ini.WriteString('TaskForm', 'Save', 'Enregistrer');
    Ini.WriteString('TaskForm', 'Cancel', 'Annuler');

    // Priorités
    Ini.WriteString('Priority', 'Low', 'Basse');
    Ini.WriteString('Priority', 'Normal', 'Normale');
    Ini.WriteString('Priority', 'High', 'Haute');

    // Dates
    Ini.WriteString('Dates', 'Today', 'Aujourd''hui');
    Ini.WriteString('Dates', 'Tomorrow', 'Demain');
    Ini.WriteString('Dates', 'Overdue', 'En retard - ');
    Ini.WriteString('Dates', 'NoDate', 'Sans date limite');
  finally
    Ini.Free;
  end;
end;

// Fonction helper pour accéder rapidement aux chaînes localisées
function L(const Section, Ident, Default: string): string;
begin
  Result := TLocalization.GetInstance.GetString(Section, Ident, Default);
end;

initialization
  TLocalization.FInstance := nil;

finalization
  TLocalization.FreeInstance;

end.
```

## 8. Compilation et tests

### 8.1 Test sur Windows

1. Assurez-vous que la plateforme cible est définie sur **Windows 32-bit** ou **Windows 64-bit** dans le Project Manager.
2. Appuyez sur **F9** ou cliquez sur **Exécuter** pour compiler et lancer l'application.
3. Testez toutes les fonctionnalités : ajout, modification, suppression et marquage des tâches.

### 8.2 Test sur Android

1. Connectez un appareil Android ou lancez un émulateur.
2. Dans le Project Manager, changez la plateforme cible pour **Android**.
3. Si c'est la première fois que vous déployez sur Android :
   - Assurez-vous que le SDK Android est correctement configuré dans **Outils > Options > Déploiement > Android**
   - Vérifiez que votre appareil est en mode Développeur et que le débogage USB est activé
4. Appuyez sur **F9** ou cliquez sur **Exécuter** pour compiler et déployer l'application sur l'appareil Android.
5. Testez particulièrement les fonctionnalités tactiles et l'adaptation de l'interface.

### 8.3 Test sur iOS

1. **Remarque importante** : Pour déployer sur iOS, vous avez besoin d'un Mac avec Xcode installé et configuré.
2. Connectez votre Mac au réseau local et configurez le PAServer.
3. Dans Delphi, configurez la connexion au PAServer dans **Outils > Options > SDK Manager**.
4. Changez la plateforme cible pour **iOS Device** ou **iOS Simulator**.
5. Appuyez sur **F9** pour déployer l'application.
6. Testez les spécificités iOS comme les gestes et les transitions.

### 8.4 Test sur macOS

1. Similaire au déploiement iOS, vous avez besoin d'un Mac connecté avec PAServer.
2. Changez la plateforme cible pour **macOS 64-bit**.
3. Déployez l'application et testez les fonctionnalités spécifiques à macOS.

### 8.5 Test sur Linux

1. Pour Linux (disponible depuis Delphi 11), configurez l'environnement Linux.
2. Changez la plateforme cible pour **Linux 64-bit**.
3. Déployez et testez l'application.

> **Note** : Les déploiements sur macOS, iOS et Linux nécessitent Delphi 11 Alexandria ou supérieur. Linux est particulièrement bien supporté à partir de Delphi 12 Athens.

## 9. Ajout de fonctionnalités spécifiques aux plateformes

Pour rendre notre application plus native sur chaque plateforme, nous allons ajouter des fonctionnalités spécifiques.

### 9.1 Notifications sur mobile

Ajoutons des notifications pour les tâches à échéance proche :

```pascal
// Ajoutez cette unité aux uses de votre unité principale
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
, FMX.PushNotification
{$ENDIF}

// Dans la déclaration de TMainForm, ajoutez :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
private
  FPushService: TPushService;
  FServiceConnection: TPushServiceConnection;
  procedure SetupPushNotification;
  procedure ScheduleTaskReminders;
{$ENDIF}

// Implémentez les méthodes :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
procedure TMainForm.SetupPushNotification;
begin
  FPushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
  if FPushService <> nil then
  begin
    FServiceConnection := TPushServiceConnection.Create(FPushService);
    FServiceConnection.Active := True;
  end;
end;

procedure TMainForm.ScheduleTaskReminders;
var
  Task: TTaskItem;
  Notification: TPushNotification;
  NotificationTime: TDateTime;
begin
  if (FPushService = nil) or (not FServiceConnection.Active) then
    Exit;

  // Annuler les notifications existantes
  FServiceConnection.Service.CancelAllLocalNotifications;

  for Task in FTaskList do
  begin
    // Ne pas notifier pour les tâches déjà terminées
    if Task.Completed then
      Continue;

    // Ne pas notifier pour les tâches sans date d'échéance
    if Task.DueDate = 0 then
      Continue;

    // Programmer une notification 1 heure avant l'échéance
    NotificationTime := Task.DueDate - (1/24);

    // Ne pas notifier pour les tâches déjà en retard
    if NotificationTime < Now then
      Continue;

    Notification := FServiceConnection.Service.CreateLocalNotification;
    try
      Notification.Title := 'Rappel : ' + Task.Title;
      Notification.AlertBody := 'Échéance dans 1 heure';
      Notification.FireDate := NotificationTime;
      Notification.Number := 1;

      FServiceConnection.Service.ScheduleLocalNotification(Notification);
    finally
      Notification.Free;
    end;
  end;
end;
{$ENDIF}

// Dans la méthode FormCreate, ajoutez :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
SetupPushNotification;
ScheduleTaskReminders;
{$ENDIF}

// Dans RefreshTaskList, appelez ScheduleTaskReminders après avoir mis à jour la liste :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
ScheduleTaskReminders;
{$ENDIF}
```

### 9.2 Intégration avec les services natifs

#### Partage sur mobile

```pascal
// Dans uses :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
, FMX.ShareSheet
{$ENDIF}

// Ajoutez une méthode pour partager une tâche :
procedure TMainForm.ShareTask(const Task: TTaskItem);
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
var
  ShareService: IFMXShareSheetService;
  Text: string;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetService, ShareService) then
  begin
    Text := 'Tâche: ' + Task.Title + #13#10;
    if Task.Description <> '' then
      Text := Text + 'Description: ' + Task.Description + #13#10;
    if Task.DueDate > 0 then
      Text := Text + 'Échéance: ' + FormatDateTime('dd/mm/yyyy', Task.DueDate) + #13#10;

    ShareService.Share(Text, nil);
  end;
{$ELSE}
begin
  // Pour les plateformes qui ne supportent pas le partage natif
  // vous pourriez implémenter un export vers un fichier
{$ENDIF}
end;

// Ajoutez un bouton de partage dans la ListView et appelez cette méthode
```

#### Intégration du calendrier

Sur les plateformes mobiles, vous pouvez permettre à l'utilisateur d'ajouter une tâche au calendrier de l'appareil :

```pascal
// Dans uses :
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
, FMX.Platform
{$ENDIF}

procedure TMainForm.AddTaskToCalendar(const Task: TTaskItem);
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
var
  CalendarService: IFMXCalendarService;
  EventID: string;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCalendarService, CalendarService) then
  begin
    EventID := CalendarService.AddEvent(
      Task.Title,                // Titre
      Task.Description,          // Description
      '',                        // Lieu
      Task.DueDate - (1/24),     // Début (1 heure avant l'échéance)
      Task.DueDate,              // Fin
      True                       // Alerte
    );

    if EventID <> '' then
      ShowMessage('Événement ajouté au calendrier');
  end;
{$ELSE}
begin
  // Non disponible sur desktop
  ShowMessage('Fonctionnalité disponible uniquement sur mobile');
{$ENDIF}
end;
```

## 10. Optimisation des performances

### 10.1 Liste virtuelle pour grands ensembles de données

Pour améliorer les performances avec de grandes listes de tâches, utilisons l'approche de chargement virtuel :

```pascal
// Dans TMainForm, modifiez TaskListView pour utiliser un adaptateur personnalisé
private
  FTaskListAdapter: TTaskListAdapter; // Classe que nous allons créer

// Créez une nouvelle unité TaskListAdapterUnit.pas :
unit TaskListAdapterUnit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  TaskDataUnit;

type
  TTaskListAdapter = class(TListViewAdapter)
  private
    FTaskList: TTaskList;
    FOwnsTaskList: Boolean;
  protected
    function GetCount: Integer; override;
    function GetItem(const Index: Integer): TListViewItem; override;
    function GetItemText(const Index: Integer): string; override;
    function GetItemDetail(const Index: Integer): string; override;
  public
    constructor Create(AOwner: TComponent; ATaskList: TTaskList;
      AOwnsTaskList: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure SetTaskList(ATaskList: TTaskList; AOwnsTaskList: Boolean = False);
    function TaskAtIndex(Index: Integer): TTaskItem;
  end;

implementation

constructor TTaskListAdapter.Create(AOwner: TComponent; ATaskList: TTaskList;
  AOwnsTaskList: Boolean);
begin
  inherited Create(AOwner);
  SetTaskList(ATaskList, AOwnsTaskList);
end;

destructor TTaskListAdapter.Destroy;
begin
  if FOwnsTaskList then
    FTaskList.Free;
  inherited;
end;

function TTaskListAdapter.GetCount: Integer;
begin
  if FTaskList <> nil then
    Result := FTaskList.Count
  else
    Result := 0;
end;

function TTaskListAdapter.GetItem(const Index: Integer): TListViewItem;
var
  Task: TTaskItem;
  LItem: TListViewItem;
  DueText: string;
begin
  // Créer un item "virtuel" basé sur les données du TTaskList
  if (Index >= 0) and (Index < FTaskList.Count) then
  begin
    LItem := TListViewItem.Create(nil);
    Task := FTaskList[Index];

    LItem.Text := Task.Title;

    // Même logique que précédemment pour DueText...
    if Task.DueDate = 0 then
      DueText := 'Sans date limite'
    else if DaysBetween(Date, Task.DueDate) = 0 then
      DueText := 'Aujourd''hui'
    else if DaysBetween(Date, Task.DueDate) = 1 then
      DueText := 'Demain'
    else if Task.DueDate < Date then
      DueText := 'En retard - ' + FormatDateTime('dd/mm/yyyy', Task.DueDate)
    else
      DueText := FormatDateTime('dd/mm/yyyy', Task.DueDate);

    LItem.Detail := DueText;

    // Configurer l'apparence selon l'état et la priorité...
    if Task.Completed then
    begin
      LItem.Objects.AccessoryObject.Visible := True;
      LItem.Objects.TextObject.TextColor := TAlphaColorRec.Gray;
      LItem.Objects.DetailObject.TextColor := TAlphaColorRec.Gray;
    end
    else
    begin
      LItem.Objects.AccessoryObject.Visible := False;

      case Task.Priority of
        tpLow: LItem.Objects.TextObject.TextColor := TAlphaColorRec.Navy;
        tpNormal: LItem.Objects.TextObject.TextColor := TAlphaColorRec.Black;
        tpHigh: LItem.Objects.TextObject.TextColor := TAlphaColorRec.Crimson;
      end;
    end;

    Result := LItem;
  end
  else
    Result := nil;
end;

function TTaskListAdapter.GetItemText(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < FTaskList.Count) then
    Result := FTaskList[Index].Title
  else
    Result := '';
end;

function TTaskListAdapter.GetItemDetail(const Index: Integer): string;
var
  Task: TTaskItem;
begin
  if (Index >= 0) and (Index < FTaskList.Count) then
  begin
    Task := FTaskList[Index];

    // Même logique de formatage que précédemment
    if Task.DueDate = 0 then
      Result := 'Sans date limite'
    else if DaysBetween(Date, Task.DueDate) = 0 then
      Result := 'Aujourd''hui'
    else if DaysBetween(Date, Task.DueDate) = 1 then
      Result := 'Demain'
    else if Task.DueDate < Date then
      Result := 'En retard - ' + FormatDateTime('dd/mm/yyyy', Task.DueDate)
    else
      Result := FormatDateTime('dd/mm/yyyy', Task.DueDate);
  end
  else
    Result := '';
end;

procedure TTaskListAdapter.SetTaskList(ATaskList: TTaskList; AOwnsTaskList: Boolean);
begin
  if FOwnsTaskList then
    FTaskList.Free;

  FTaskList := ATaskList;
  FOwnsTaskList := AOwnsTaskList;

  ResetView(True);
end;

function TTaskListAdapter.TaskAtIndex(Index: Integer): TTaskItem;
begin
  if (Index >= 0) and (Index < FTaskList.Count) then
    Result := FTaskList[Index]
  else
    Result := nil;
end;

end.
```

### 10.2 Chargement asynchrone des données

Pour une application plus réactive, chargeons les données en arrière-plan :

```pascal
// Dans TaskDataUnit.pas, ajoutez :
TTaskLoadCallback = reference to procedure(const Success: Boolean);

class procedure TTaskList.LoadFromFileAsync(const Callback: TTaskLoadCallback);
begin
  TTask.Run(
    procedure
    var
      TaskList: TTaskList;
    begin
      TaskList := TTaskList.Create;
      try
        TaskList.LoadFromFile;

        TThread.Synchronize(nil,
          procedure
          begin
            // Copier les tâches dans la liste principale (this)
            Self.Clear;
            for var Task in TaskList do
              Self.Add(Task.Clone);

            if Assigned(Callback) then
              Callback(True);
          end);
      except
        TThread.Synchronize(nil,
          procedure
          begin
            if Assigned(Callback) then
              Callback(False);
          end);
      end;
      TaskList.Free;
    end);
end;
```

## 11. Ajouts spécifiques à chaque plateforme

FireMonkey permet d'ajouter des comportements spécifiques à chaque plateforme tout en maintenant une base de code commune.

### 11.1 Styles visuels spécifiques aux plateformes

```pascal
procedure TMainForm.ApplyPlatformStyle;
begin
  {$IFDEF MSWINDOWS}
  // Style Windows
  TStyleManager.TrySetStyleFromFile('Windows11.vsf');
  {$ENDIF}

  {$IFDEF MACOS}
  // Style macOS
  TStyleManager.TrySetStyleFromFile('macOS.vsf');
  {$ENDIF}

  {$IFDEF IOS}
  // Style iOS
  TStyleManager.TrySetStyleFromFile('iOS.vsf');
  {$ENDIF}

  {$IFDEF ANDROID}
  // Style Material Design
  TStyleManager.TrySetStyleFromFile('Material.vsf');
  {$ENDIF}

  // Ajustements supplémentaires selon la plateforme
  {$IFDEF MSWINDOWS}
  HeaderLayout.Padding.Top := 5;
  HeaderLayout.Padding.Bottom := 5;
  {$ENDIF}

  {$IFDEF ANDROID}
  HeaderLayout.Padding.Top := 10;
  HeaderLayout.Padding.Bottom := 10;
  {$ENDIF}

  {$IFDEF IOS}
  // Tenir compte de la "notch" et de la barre d'état
  HeaderLayout.Padding.Top := 20;
  {$ENDIF}
end;
```

### 11.2 Comportements spécifiques

```pascal
// Dans FormCreate, ajoutez des comportements spécifiques à la plateforme
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Code commun
  FTaskList := TTaskList.Create;
  ApplyPlatformStyle;

  // Comportements spécifiques
  {$IFDEF ANDROID}
  // Sur Android, activons le glissement pour supprimer
  TaskListView.DeleteButtonText := 'Supprimer';
  TaskListView.ShowDeleteButton := True;
  {$ENDIF}

  {$IFDEF IOS}
  // Sur iOS, utilisons un style différent pour les suppressions
  TaskListView.EditMode := True;
  TaskListView.CanSwipeDelete := True;
  {$ENDIF}

  RefreshTaskList;
  UpdateEmptyState;
end;
```

## 12. Analyse des performances et débogage multi-plateformes

### 12.1 Profilage de l'application

Delphi offre des outils intégrés pour analyser les performances :

1. Cliquez sur **Exécuter > Profileur** pour activer le profilage.
2. Exécutez votre application normalement.
3. Analysez les résultats pour identifier les goulots d'étranglement.

### 12.2 Débogage à distance

Pour déboguer sur des appareils physiques :

1. Pour Android/iOS/macOS :
   - Utilisez **Exécuter > Attachement au processus** pour vous connecter à l'application en cours d'exécution.
   - Utilisez les points d'arrêt comme d'habitude.

2. Pour Linux :
   - Configurez le PAServer sur la machine Linux.
   - Connectez-vous au PAServer depuis Delphi.
   - Déployez et déboguez comme d'habitude.

## 13. Préparation au déploiement

### 13.1 Configuration des options de déploiement

Pour chaque plateforme, configurez les options de déploiement :

1. Dans le Project Manager, cliquez-droit sur le nom du projet et sélectionnez **Options du projet**.
2. Allez dans **Déploiement** et configurez pour chaque plateforme cible :
   - Mode de déploiement (déboguer/release)
   - Fichiers à inclure
   - Paramètres de l'application (icônes, versions, etc.)

### 13.2 Préparation des icônes et écrans de démarrage

Pour une application professionnelle, préparez des ressources graphiques adaptées à chaque plateforme :

```pascal
// Ajoutez au projet
// - Un fichier .icns pour macOS
// - Des fichiers .ico pour Windows
// - Un ensemble d'icônes PNG pour Android/iOS
// - Un splash screen pour chaque plateforme

// Dans le projet (.dproj), assurez-vous que ces ressources sont correctement liées
```

## 14. Améliorations possibles

Pour aller plus loin avec ce projet, vous pourriez explorer :

1. **Synchronisation cloud** : Ajoutez une synchronisation avec Firebase ou un autre service cloud.
2. **Widgets** : Créez des widgets pour Android/iOS affichant les tâches à venir.
3. **Thèmes sombres/clairs** : Ajoutez une option pour changer les thèmes selon les préférences utilisateur ou le thème système.
4. **Catégorisation** : Permettez aux utilisateurs de classer les tâches par catégories.
5. **Rappels multiples** : Ajoutez la possibilité de définir plusieurs rappels pour une même tâche.

## Conclusion

Dans ce tutoriel, nous avons créé une application de gestion de tâches complète qui fonctionne sur plusieurs plateformes. FireMonkey nous a permis de maintenir une base de code commune tout en adaptant l'expérience utilisateur aux spécificités de chaque plateforme.

Les principes clés à retenir :

1. **Une base commune** : La majorité du code est partagée entre toutes les plateformes.
2. **Adaptations ciblées** : Utilisez des directives de compilation (`{$IFDEF}`) pour ajouter des comportements spécifiques.
3. **Services natifs** : Tirez parti des services propres à chaque plateforme quand c'est pertinent.
4. **UI adaptative** : Concevez des interfaces qui s'adaptent à différentes tailles d'écran et orientations.
5. **Persistance des données** : Utilisez des mécanismes de stockage qui fonctionnent sur toutes les plateformes.

FireMonkey et Delphi vous permettent de créer rapidement des applications multi-plateformes de qualité professionnelle, tout en minimisant la duplication de code et d'effort.

> **Note** : Ce tutoriel utilise Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria. Les fonctionnalités spécifiques à Delphi 12 sont marquées comme telles.
