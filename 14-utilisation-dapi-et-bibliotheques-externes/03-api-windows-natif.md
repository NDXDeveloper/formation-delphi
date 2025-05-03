# 14.3 API Windows natif

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'API Windows (Application Programming Interface) est un ensemble de fonctions, structures et constantes qui permettent à votre application Delphi d'interagir directement avec le système d'exploitation Windows. Si vous imaginez Windows comme une maison, l'API Windows représente toutes les portes, fenêtres et interrupteurs que vous pouvez utiliser pour contrôler cette maison.

Bien que Delphi offre déjà de nombreuses fonctionnalités intégrées via sa bibliothèque VCL (Visual Component Library), il existe plusieurs raisons d'apprendre à utiliser l'API Windows native :

- Accéder à des fonctionnalités avancées non disponibles dans la VCL
- Personnaliser le comportement de votre application
- Optimiser certaines opérations
- Utiliser des fonctionnalités récentes de Windows

Dans ce chapitre, nous allons explorer les bases de l'API Windows et apprendre à l'utiliser dans vos applications Delphi avec des exemples pratiques et accessibles.

## Les bases de l'API Windows dans Delphi

### Comment accéder à l'API Windows

Delphi facilite l'accès à l'API Windows en incluant déjà les déclarations nécessaires dans plusieurs unités :

- `Winapi.Windows` : Contient la plupart des fonctions Windows courantes
- `Winapi.Messages` : Définit les messages Windows (communication entre fenêtres)
- `Winapi.ShellAPI` : Pour interagir avec l'Explorateur Windows
- `Winapi.CommCtrl` : Pour les contrôles communs Windows (comme les barres d'outils)

Pour utiliser ces fonctions, il suffit d'ajouter les unités correspondantes à la clause `uses` de votre projet :

```pascal
uses
  System.SysUtils, System.Classes, Vcl.Forms,
  Winapi.Windows, Winapi.Messages;
```

### Types de données Windows vs Delphi

L'API Windows utilise des types de données spécifiques. Delphi les a adaptés pour faciliter leur utilisation :

| Type Windows | Type Delphi     | Description                               |
|--------------|----------------|-------------------------------------------|
| HANDLE       | THandle        | Identificateur d'objet Windows            |
| HWND         | HWND           | Handle de fenêtre                         |
| BOOL         | BOOL/Boolean   | Valeur booléenne                          |
| LPSTR        | PAnsiChar      | Pointeur vers chaîne ANSI                 |
| LPWSTR       | PWideChar      | Pointeur vers chaîne Unicode              |
| DWORD        | DWORD/Cardinal | Entier 32 bits non signé                  |
| WORD         | WORD/Word      | Entier 16 bits non signé                  |
| INT          | Integer        | Entier 32 bits signé                      |

## Premiers exemples avec l'API Windows

### Exemple 1 : Afficher un message Windows

Commençons par un exemple simple : afficher une boîte de dialogue Windows personnalisée.

```pascal
procedure TForm1.ButtonMessageBoxClick(Sender: TObject);
begin
  MessageBox(
    Handle,                     // Handle de la fenêtre parente
    'Ceci est un message via l''API Windows',  // Texte du message
    'Message API Windows',      // Titre de la fenêtre
    MB_ICONINFORMATION or MB_OKCANCEL // Options (icône + boutons)
  );
end;
```

La fonction `MessageBox` offre plus d'options que la fonction `ShowMessage` standard de Delphi :

- Vous pouvez définir différentes icônes (MB_ICONINFORMATION, MB_ICONWARNING, MB_ICONERROR...)
- Vous pouvez choisir différentes combinaisons de boutons (MB_OK, MB_OKCANCEL, MB_YESNO...)
- Vous pouvez récupérer le bouton cliqué par l'utilisateur

```pascal
var
  Reponse: Integer;
begin
  Reponse := MessageBox(Handle, 'Confirmer l''action ?', 'Confirmation',
                        MB_YESNOCANCEL or MB_ICONQUESTION);

  case Reponse of
    IDYES: ShowMessage('Vous avez cliqué sur Oui');
    IDNO: ShowMessage('Vous avez cliqué sur Non');
    IDCANCEL: ShowMessage('Vous avez cliqué sur Annuler');
  end;
end;
```

### Exemple 2 : Obtenir des informations système

Voici comment récupérer des informations sur le système d'exploitation :

```pascal
procedure TForm1.ButtonSystemInfoClick(Sender: TObject);
var
  SysInfo: TSystemInfo;
  VersionInfo: TOSVersionInfo;
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
  Msg: string;
begin
  // Obtenir les informations système
  GetSystemInfo(SysInfo);

  // Obtenir les informations de version
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VersionInfo);

  // Obtenir le nom de l'ordinateur
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(ComputerName, Size);

  // Afficher les informations
  Msg := Format(
    'Informations système:' + #13#10 +
    'Processeurs: %d' + #13#10 +
    'Type de processeur: %d' + #13#10 +
    'Version Windows: %d.%d' + #13#10 +
    'Nom de l''ordinateur: %s',
    [SysInfo.dwNumberOfProcessors, SysInfo.dwProcessorType,
     VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion,
     ComputerName]
  );

  ShowMessage(Msg);
end;
```

> **Note** : La fonction `GetVersionEx` est considérée comme obsolète dans les versions récentes de Windows. Pour les applications modernes, utilisez les fonctions comme `GetProductInfo` ou consultez les informations via WMI (Windows Management Instrumentation).

## Manipulation des fenêtres Windows

L'API Windows permet de manipuler n'importe quelle fenêtre dans le système.

### Trouver et manipuler des fenêtres

Dans Windows, chaque fenêtre possède un identifiant unique appelé "handle". Vous pouvez utiliser ces handles pour interagir avec n'importe quelle fenêtre de votre système :

```pascal
procedure TForm1.ButtonFindNotepadClick(Sender: TObject);
var
  NotepadHandle: HWND;
begin
  // Rechercher une fenêtre du Bloc-notes par son titre
  NotepadHandle := FindWindow('Notepad', nil);

  if NotepadHandle <> 0 then
  begin
    // Mettre le Bloc-notes au premier plan
    SetForegroundWindow(NotepadHandle);

    // Envoyer un message pour changer le titre
    SendMessage(NotepadHandle, WM_SETTEXT, 0,
                LPARAM(PChar('Titre modifié par Delphi')));

    ShowMessage('Bloc-notes trouvé et modifié !');
  end
  else
    ShowMessage('Bloc-notes non trouvé. Veuillez l''ouvrir d''abord.');
end;
```

### Énumérer les fenêtres ouvertes

Pour trouver toutes les fenêtres ouvertes sur le système, vous pouvez utiliser `EnumWindows` avec une fonction de callback :

```pascal
// Fonction de callback qui sera appelée pour chaque fenêtre
function EnumWindowsProc(WndHandle: HWND; List: TStringList): BOOL; stdcall;
var
  Title: array[0..255] of Char;
  TitleLength: Integer;
begin
  // Récupérer le titre de la fenêtre
  TitleLength := GetWindowText(WndHandle, Title, 256);

  // Ajouter à la liste si la fenêtre a un titre
  if (TitleLength > 0) and IsWindowVisible(WndHandle) then
    List.Add(Format('0x%.8x - %s', [WndHandle, Title]));

  // Continuer l'énumération
  Result := True;
end;

procedure TForm1.ButtonEnumWindowsClick(Sender: TObject);
var
  WindowsList: TStringList;
begin
  WindowsList := TStringList.Create;
  try
    // Énumérer toutes les fenêtres
    EnumWindows(@EnumWindowsProc, LPARAM(WindowsList));

    // Afficher la liste des fenêtres
    Memo1.Lines.Clear;
    Memo1.Lines.AddStrings(WindowsList);
  finally
    WindowsList.Free;
  end;
end;
```

## Manipulation du registre Windows

Le registre Windows est une base de données hiérarchique qui stocke les paramètres et options pour le système d'exploitation et les applications.

```pascal
procedure TForm1.ButtonRegistryClick(Sender: TObject);
var
  RegKey: HKEY;
  Value: array[0..255] of Char;
  ValueSize: DWORD;
  ValueType: DWORD;
  Disposition: DWORD;
begin
  // Ouvrir/créer une clé de registre
  if RegCreateKeyEx(
    HKEY_CURRENT_USER,             // Ruche principale
    'Software\MonApplication',      // Chemin de la clé
    0,                             // Réservé
    nil,                           // Classe (peut être nil)
    REG_OPTION_NON_VOLATILE,       // Options
    KEY_ALL_ACCESS,                // Droits d'accès
    nil,                           // Attributs de sécurité
    RegKey,                        // Handle de clé résultant
    @Disposition                   // Indique si la clé a été créée ou ouverte
  ) = ERROR_SUCCESS then
  begin
    try
      // Écrire une valeur
      RegSetValueEx(
        RegKey,                // Handle de clé
        'MaValeur',            // Nom de la valeur
        0,                     // Réservé
        REG_SZ,                // Type de données (chaîne)
        PByte(PChar('Test')),  // Données
        Length('Test') + 1     // Taille des données (y compris le zéro final)
      );

      // Lire la valeur que nous venons d'écrire
      ValueSize := SizeOf(Value);
      if RegQueryValueEx(
        RegKey,          // Handle de clé
        'MaValeur',      // Nom de la valeur
        nil,             // Réservé
        @ValueType,      // Type de données
        @Value[0],       // Buffer de données
        @ValueSize       // Taille du buffer
      ) = ERROR_SUCCESS then
        ShowMessage('Valeur lue : ' + Value)
      else
        ShowMessage('Erreur lors de la lecture');
    finally
      // Fermer la clé (important !)
      RegCloseKey(RegKey);
    end;
  end
  else
    ShowMessage('Impossible d''ouvrir/créer la clé de registre');
end;
```

> **Important** : L'accès au registre Windows peut nécessiter des privilèges élevés, surtout pour les clés système. Soyez prudent lors de la modification du registre.

## Opérations sur les fichiers

L'API Windows offre de nombreuses fonctions pour manipuler les fichiers, souvent avec plus d'options que les fonctions standard de Delphi.

### Copier, déplacer et supprimer des fichiers

```pascal
procedure TForm1.ButtonFilesClick(Sender: TObject);
var
  SourceFile, DestFile: string;
  FileOperation: TSHFileOpStruct;
begin
  SourceFile := 'C:\Temp\source.txt';
  DestFile := 'C:\Temp\destination.txt';

  // Créer un fichier de test si nécessaire
  if not FileExists(SourceFile) then
  begin
    with TStringList.Create do
    try
      Text := 'Fichier de test pour l''API Windows';
      SaveToFile(SourceFile);
    finally
      Free;
    end;
  end;

  // Copier un fichier
  if CopyFile(PChar(SourceFile), PChar(DestFile), False) then
    ShowMessage('Fichier copié avec succès!')
  else
    ShowMessage('Échec de la copie : ' + SysErrorMessage(GetLastError));

  // Définir des attributs de fichier
  SetFileAttributes(PChar(DestFile), FILE_ATTRIBUTE_READONLY);

  // Supprimer un fichier (avec interface utilisateur)
  FillChar(FileOperation, SizeOf(FileOperation), 0);
  FileOperation.wFunc := FO_DELETE;
  FileOperation.pFrom := PChar(DestFile + #0); // Doit se terminer par double null
  FileOperation.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;

  if SHFileOperation(FileOperation) = 0 then
    ShowMessage('Fichier supprimé avec succès!')
  else
    ShowMessage('Échec de la suppression');
end;
```

### Énumérer les fichiers d'un dossier

Voici comment parcourir les fichiers d'un répertoire avec l'API Windows :

```pascal
procedure TForm1.ButtonListFilesClick(Sender: TObject);
var
  SearchRec: TWin32FindData;
  SearchHandle: THandle;
  Directory: string;
begin
  Directory := 'C:\Windows\*.*';
  Memo1.Lines.Clear;

  // Démarrer la recherche
  SearchHandle := FindFirstFile(PChar(Directory), SearchRec);

  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      repeat
        // Vérifier si c'est un dossier
        if (SearchRec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
          Memo1.Lines.Add('[DIR] ' + SearchRec.cFileName)
        else
          Memo1.Lines.Add(Format('%s (%d octets)',
            [SearchRec.cFileName, SearchRec.nFileSizeLow]));
      until not FindNextFile(SearchHandle, SearchRec);
    finally
      // Fermer le handle de recherche
      Windows.FindClose(SearchHandle);
    end;
  end
  else
    ShowMessage('Erreur lors de la recherche : ' + SysErrorMessage(GetLastError));
end;
```

## Gestion des processus et des threads

L'API Windows permet de lancer et de contrôler d'autres programmes.

### Démarrer un nouveau processus

```pascal
procedure TForm1.ButtonStartProcessClick(Sender: TObject);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: string;
begin
  // Préparer la ligne de commande
  CommandLine := 'notepad.exe C:\temp\exemple.txt';

  // Initialiser les structures
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);

  // Créer le processus
  if CreateProcess(
    nil,                    // Nom de l'application (utilise CommandLine)
    PChar(CommandLine),     // Ligne de commande
    nil,                    // Attributs de sécurité du processus
    nil,                    // Attributs de sécurité du thread
    False,                  // Héritage des handles
    0,                      // Flags de création
    nil,                    // Environnement (utilise celui du parent)
    nil,                    // Répertoire courant (utilise celui du parent)
    StartupInfo,            // Informations de démarrage
    ProcessInfo             // Informations sur le processus créé
  ) then
  begin
    ShowMessage('Processus démarré avec succès!');

    // Fermer les handles (important pour éviter les fuites de ressources)
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
    ShowMessage('Échec du démarrage du processus : ' +
                SysErrorMessage(GetLastError));
end;
```

### Énumérer les processus en cours

Pour lister tous les processus en cours d'exécution (nécessite l'unité `TlHelp32`) :

```pascal
procedure TForm1.ButtonListProcessesClick(Sender: TObject);
var
  SnapProcHandle: THandle;
  ProcEntry: TProcessEntry32;
  NextProc: Boolean;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('PID    | Nom du processus');
  Memo1.Lines.Add('---------------------');

  // Prendre un instantané des processus
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if SnapProcHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      // Initialiser la structure
      ProcEntry.dwSize := SizeOf(TProcessEntry32);

      // Obtenir le premier processus
      NextProc := Process32First(SnapProcHandle, ProcEntry);

      // Parcourir tous les processus
      while NextProc do
      begin
        Memo1.Lines.Add(Format('%5d | %s',
          [ProcEntry.th32ProcessID, ProcEntry.szExeFile]));

        NextProc := Process32Next(SnapProcHandle, ProcEntry);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;
end;
```

## Gestion du son

Windows permet de jouer des sons facilement.

### Jouer un son système

```pascal
procedure TForm1.ButtonPlaySoundClick(Sender: TObject);
begin
  // Jouer un son système
  PlaySound('SystemAsterisk', 0, SND_ALIAS or SND_ASYNC);

  // Autres sons système : SystemExclamation, SystemHand,
  // SystemQuestion, SystemDefault
end;
```

### Jouer un fichier audio

```pascal
procedure TForm1.ButtonPlayWavClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := 'C:\Windows\Media\chimes.wav';

  if FileExists(FileName) then
    PlaySound(PChar(FileName), 0, SND_FILENAME or SND_ASYNC)
  else
    ShowMessage('Fichier audio non trouvé');
end;
```

## Gestion du presse-papiers

L'API Windows permet d'interagir avec le presse-papiers du système.

### Copier du texte vers le presse-papiers

```pascal
procedure TForm1.ButtonCopyToClipboardClick(Sender: TObject);
var
  TextToCopy: string;
begin
  TextToCopy := 'Texte copié via l''API Windows';

  // Approche simplifiée avec Delphi
  Clipboard.AsText := TextToCopy;
  ShowMessage('Texte copié dans le presse-papiers');

  // Alternative avec l'API Windows directe (plus complexe)
  // Voir documentation avancée pour cette approche
end;
```

### Récupérer du texte du presse-papiers

```pascal
procedure TForm1.ButtonPasteFromClipboardClick(Sender: TObject);
begin
  // Approche simplifiée avec Delphi
  if Clipboard.HasFormat(CF_TEXT) then
    ShowMessage('Texte du presse-papiers: ' + Clipboard.AsText)
  else
    ShowMessage('Le presse-papiers ne contient pas de texte');
end;
```

## Gestion des erreurs avec l'API Windows

La plupart des fonctions de l'API Windows renvoient une valeur indiquant si l'opération a réussi ou échoué. En cas d'échec, vous pouvez obtenir plus d'informations avec `GetLastError` :

```pascal
function TForm1.TryWindowsOperation: Boolean;
var
  ErrorCode: DWORD;
  ErrorMessage: string;
begin
  // Tentative d'opération Windows (exemple)
  Result := SomeWindowsFunction();

  if not Result then
  begin
    // Obtenir le code d'erreur
    ErrorCode := GetLastError();

    // Convertir le code en message
    ErrorMessage := SysErrorMessage(ErrorCode);

    // Afficher le message d'erreur
    ShowMessage('Erreur Windows: ' + ErrorMessage + ' (Code: ' + IntToStr(ErrorCode) + ')');
  end;
end;
```

## Comment manipuler les chemins de fichiers Windows

L'API Windows fournit des fonctions utiles pour manipuler les chemins de fichiers :

```pascal
procedure TForm1.ButtonPathOperationsClick(Sender: TObject);
var
  FullPath, FileName, Extension: string;
  Buffer: array[0..MAX_PATH] of Char;
begin
  FullPath := 'C:\Program Files\Mon Application\exemple.txt';

  // Extraire le nom du fichier
  FileName := ExtractFileName(FullPath);

  // Extraire l'extension
  Extension := ExtractFileExt(FullPath);

  // Obtenir le dossier Temp
  GetTempPath(MAX_PATH, Buffer);

  // Obtenir le dossier Windows
  GetWindowsDirectory(Buffer, MAX_PATH);

  ShowMessage('Nom du fichier: ' + FileName + #13#10 +
              'Extension: ' + Extension + #13#10 +
              'Dossier Temp: ' + Buffer + #13#10 +
              'Dossier Windows: ' + Buffer);
end;
```

## Exemple pratique : un explorateur de fichiers simple

Voici un exemple plus complet qui combine plusieurs concepts de l'API Windows pour créer un petit explorateur de fichiers :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser l'interface utilisateur
  DriveComboBox.Items.Clear;

  // Obtenir les lecteurs disponibles
  var Drives: DWORD := GetLogicalDrives;
  var Drive: Char;

  // Ajouter les lecteurs à la liste déroulante
  for Drive := 'A' to 'Z' do
  begin
    if (Drives and 1) = 1 then
      DriveComboBox.Items.Add(Drive + ':');
    Drives := Drives shr 1;
  end;

  if DriveComboBox.Items.Count > 0 then
    DriveComboBox.ItemIndex := 0;

  // Afficher le contenu du lecteur sélectionné
  BrowseDirectory(DriveComboBox.Text);
end;

procedure TForm1.BrowseDirectory(const Directory: string);
var
  SearchRec: TWin32FindData;
  SearchHandle: THandle;
  SearchPath: string;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
  FileDateTime: TDateTime;
  FileSizeStr: string;
begin
  // Effacer la liste actuelle
  FileListBox.Items.Clear;

  // Préparer le chemin de recherche
  SearchPath := IncludeTrailingPathDelimiter(Directory) + '*.*';

  // Démarrer la recherche
  SearchHandle := FindFirstFile(PChar(SearchPath), SearchRec);

  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      repeat
        // Ignorer les entrées "." et ".."
        if (SearchRec.cFileName <> '.') and (SearchRec.cFileName <> '..') then
        begin
          // Convertir la taille du fichier en format lisible
          if (SearchRec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
            FileSizeStr := '<DIR>'
          else
          begin
            if SearchRec.nFileSizeHigh = 0 then
            begin
              if SearchRec.nFileSizeLow < 1024 then
                FileSizeStr := Format('%d octets', [SearchRec.nFileSizeLow])
              else if SearchRec.nFileSizeLow < 1024*1024 then
                FileSizeStr := Format('%.1f Ko', [SearchRec.nFileSizeLow / 1024])
              else
                FileSizeStr := Format('%.1f Mo', [SearchRec.nFileSizeLow / (1024*1024)]);
            end
            else
              FileSizeStr := 'Très grand';
          end;

          // Convertir la date du fichier
          FileTime := SearchRec.ftLastWriteTime;
          FileTimeToLocalFileTime(FileTime, FileTime);
          FileTimeToSystemTime(FileTime, SystemTime);

          FileDateTime := SystemTimeToDateTime(SystemTime);

          // Ajouter à la liste
          FileListBox.Items.Add(
            Format('%-30s %-15s %s',
            [SearchRec.cFileName, FileSizeStr, DateTimeToStr(FileDateTime)])
          );
        end;
      until not FindNextFile(SearchHandle, SearchRec);
    finally
      Windows.FindClose(SearchHandle);
    end;
  end
  else
    ShowMessage('Erreur lors de la recherche : ' + SysErrorMessage(GetLastError));

  // Mettre à jour le titre
  CurrentDirectoryLabel.Caption := 'Répertoire : ' + Directory;
end;

procedure TForm1.DriveComboBoxChange(Sender: TObject);
begin
  // Afficher le contenu du lecteur sélectionné
  BrowseDirectory(DriveComboBox.Text);
end;

procedure TForm1.FileListBoxDblClick(Sender: TObject);
var
  SelectedItem, FileName, FilePath: string;
  IsDirectory: Boolean;
begin
  if FileListBox.ItemIndex >= 0 then
  begin
    // Extraire le nom du fichier (première partie de la chaîne)
    SelectedItem := FileListBox.Items[FileListBox.ItemIndex];
    FileName := Trim(Copy(SelectedItem, 1, 30));

    // Déterminer s'il s'agit d'un répertoire
    IsDirectory := Pos('<DIR>', SelectedItem) > 0;

    // Construire le chemin complet
    FilePath := IncludeTrailingPathDelimiter(CurrentDirectoryLabel.Caption) + FileName;

    if IsDirectory then
      // Naviguer dans le répertoire
      BrowseDirectory(FilePath)
    else
      // Ouvrir le fichier avec l'application associée
      ShellExecute(Handle, 'open', PChar(FilePath), nil, nil, SW_SHOWNORMAL);
  end;
end;
```

## Bonnes pratiques avec l'API Windows

1. **Toujours vérifier les valeurs de retour** - Les fonctions Windows renvoient souvent des codes d'erreur qu'il faut vérifier.
2. **Libérer les ressources** - Les handles Windows doivent être fermés avec `CloseHandle` lorsqu'ils ne sont plus nécessaires.
3. **Gérer les erreurs** - Utilisez `GetLastError` et `SysErrorMessage` pour obtenir des informations sur les erreurs.
4. **Attention à l'Unicode** - Les versions modernes de Windows utilisent Unicode. Utilisez les versions W des fonctions (comme `CreateFileW`) ou laissez Delphi gérer cela automatiquement.
5. **Documentation** - Consultez la documentation Microsoft pour les détails complets sur les fonctions de l'API Windows.

## Conclusion

L'API Windows offre une puissance considérable aux développeurs Delphi, permettant d'accéder à pratiquement toutes les fonctionnalités du système d'exploitation. Bien que la VCL offre déjà beaucoup de fonctionnalités encapsulées, la connaissance de l'API Windows vous donne un contrôle plus précis et l'accès à des fonctionnalités avancées.

Les exemples présentés dans ce chapitre ne sont qu'un aperçu des possibilités. À mesure que vous développerez vos compétences, vous pourrez explorer d'autres domaines comme :

- La manipulation avancée de l'interface utilisateur Windows
- L'accès aux services système
- Les notifications système
- La communication réseau de bas niveau
- Les graphiques et l'accès matériel

N'hésitez pas à consulter la documentation Microsoft et les exemples Delphi pour approfondir vos connaissances de l'API Windows et enrichir vos applications.

⏭️ [COM et ActiveX](14-utilisation-dapi-et-bibliotheques-externes/04-com-et-activex.md)
