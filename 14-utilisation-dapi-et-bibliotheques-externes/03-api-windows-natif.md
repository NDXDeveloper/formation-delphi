🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.3 API Windows natif

## Introduction à l'API Windows

### Qu'est-ce que l'API Windows ?

L'**API Windows** (Application Programming Interface) est un ensemble de fonctions, structures et constantes fournies par Microsoft pour interagir directement avec le système d'exploitation Windows. C'est le socle sur lequel sont construites toutes les applications Windows, y compris Delphi lui-même.

### Pourquoi utiliser l'API Windows ?

Bien que Delphi fournisse des composants et classes de haut niveau (VCL, RTL), l'API Windows permet de :

- Accéder à des fonctionnalités non exposées par Delphi
- Obtenir un contrôle plus fin sur le système
- Implémenter des comportements spécifiques avancés
- Améliorer les performances dans certains cas
- Comprendre le fonctionnement interne de Windows
- Créer des applications système

### Les principales DLLs Windows

L'API Windows est répartie dans plusieurs DLLs :

**user32.dll** : Interface utilisateur (fenêtres, messages, menus, icônes)

**kernel32.dll** : Fonctions système de base (processus, threads, mémoire, fichiers)

**gdi32.dll** : Graphics Device Interface (dessin, polices, images)

**advapi32.dll** : Services avancés (registre, sécurité, services Windows)

**shell32.dll** : Shell Windows (explorateur, raccourcis, icônes système)

**comctl32.dll** : Contrôles communs Windows

### Unité Windows

Delphi fournit l'unité `Winapi.Windows` (ou simplement `Windows` dans les anciennes versions) qui contient la plupart des déclarations d'API Windows :

```pascal
uses
  Winapi.Windows,  // API Windows principales
  Winapi.Messages, // Constantes de messages
  Winapi.ShellAPI; // API du Shell

// Ou dans les versions antérieures :
uses
  Windows, Messages, ShellAPI;
```

## Gestion des fenêtres

### Obtenir des informations sur une fenêtre

```pascal
uses
  Winapi.Windows;

procedure ObtenirInfoFenetre(Handle: HWND);  
var  
  texte: array[0..255] of Char;
  rect: TRect;
begin
  // Obtenir le titre de la fenêtre
  GetWindowText(Handle, texte, Length(texte));
  ShowMessage('Titre: ' + string(texte));

  // Obtenir les dimensions de la fenêtre
  GetWindowRect(Handle, rect);
  ShowMessage(Format('Position: %d,%d Taille: %dx%d',
    [rect.Left, rect.Top, rect.Width, rect.Height]));

  // Vérifier si la fenêtre est visible
  if IsWindowVisible(Handle) then
    ShowMessage('La fenêtre est visible')
  else
    ShowMessage('La fenêtre est cachée');
end;
```

### Énumérer toutes les fenêtres

```pascal
function EnumWindowsProc(Wnd: HWND; LParam: LPARAM): BOOL; stdcall;  
var  
  titre: array[0..255] of Char;
  liste: TStrings;
begin
  liste := TStrings(LParam);

  if IsWindowVisible(Wnd) then
  begin
    GetWindowText(Wnd, titre, Length(titre));
    if titre[0] <> #0 then  // Si le titre n'est pas vide
      liste.Add(string(titre));
  end;

  Result := True; // Continuer l'énumération
end;

procedure ListerFenetres(Liste: TStrings);  
begin  
  Liste.Clear;
  EnumWindows(@EnumWindowsProc, LPARAM(Liste));
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ListerFenetres(Memo1.Lines);
end;
```

### Manipulation de fenêtres

```pascal
uses
  Winapi.Windows;

// Déplacer une fenêtre
procedure DeplacerFenetre(Handle: HWND; X, Y: Integer);  
begin  
  SetWindowPos(Handle, 0, X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOZORDER);
end;

// Redimensionner une fenêtre
procedure RedimensionnerFenetre(Handle: HWND; Largeur, Hauteur: Integer);  
begin  
  SetWindowPos(Handle, 0, 0, 0, Largeur, Hauteur,
    SWP_NOMOVE or SWP_NOZORDER);
end;

// Rendre une fenêtre toujours au premier plan
procedure AlwaysOnTop(Handle: HWND; OnTop: Boolean);  
const  
  Flags: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, Flags[OnTop], 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE);
end;

// Minimiser/Maximiser/Restaurer
procedure ChangerEtatFenetre(Handle: HWND; Etat: Integer);  
begin  
  ShowWindow(Handle, Etat);
  // Etat peut être: SW_MINIMIZE, SW_MAXIMIZE, SW_RESTORE
end;
```

### Trouver une fenêtre

```pascal
// Trouver une fenêtre par son titre
function TrouverFenetreParTitre(const Titre: string): HWND;  
begin  
  Result := FindWindow(nil, PChar(Titre));
  if Result = 0 then
    ShowMessage('Fenêtre non trouvée');
end;

// Trouver une fenêtre par son nom de classe
function TrouverFenetreParClasse(const Classe: string): HWND;  
begin  
  Result := FindWindow(PChar(Classe), nil);
end;

// Exemple d'utilisation
procedure TForm1.Button2Click(Sender: TObject);  
var  
  hWnd: HWND;
begin
  // Trouver le Bloc-notes
  hWnd := TrouverFenetreParTitre('Sans titre - Bloc-notes');
  if hWnd <> 0 then
  begin
    // Mettre le focus sur cette fenêtre
    SetForegroundWindow(hWnd);
  end;
end;
```

## Messages Windows

### Principe des messages

Windows fonctionne avec un système de messages. Chaque événement (clic, frappe clavier, etc.) génère un message envoyé à la fenêtre concernée.

### Envoyer un message

```pascal
uses
  Winapi.Windows, Winapi.Messages;

// Fermer une application
procedure FermerApplication(Handle: HWND);  
begin  
  SendMessage(Handle, WM_CLOSE, 0, 0);
end;

// Envoyer du texte à une fenêtre
procedure EnvoyerTexte(Handle: HWND; const Texte: string);  
var  
  I: Integer;
begin
  for I := 1 to Length(Texte) do
  begin
    SendMessage(Handle, WM_CHAR, Ord(Texte[I]), 0);
  end;
end;

// Simuler un clic sur un bouton
procedure CliquerBouton(HandleBouton: HWND);  
begin  
  SendMessage(HandleBouton, BM_CLICK, 0, 0);
end;
```

### Intercepter des messages

Dans une application Delphi, vous pouvez intercepter les messages Windows :

```pascal
type
  TForm1 = class(TForm)
  protected
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
  end;

// Intercepter un raccourci clavier global
procedure TForm1.WMHotKey(var Msg: TWMHotKey);  
begin  
  case Msg.HotKey of
    1: ShowMessage('Ctrl+Alt+A pressé');
    2: ShowMessage('Ctrl+Alt+B pressé');
  end;
end;

// Intercepter les changements de périphériques
procedure TForm1.WMDeviceChange(var Msg: TMessage);  
begin  
  if Msg.WParam = DBT_DEVICEARRIVAL then
    ShowMessage('Un périphérique a été connecté')
  else if Msg.WParam = DBT_DEVICEREMOVECOMPLETE then
    ShowMessage('Un périphérique a été déconnecté');
end;
```

### Enregistrer un raccourci clavier global

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Enregistrer Ctrl+Alt+A (ID 1)
  RegisterHotKey(Handle, 1, MOD_CONTROL or MOD_ALT, Ord('A'));

  // Enregistrer Ctrl+Alt+B (ID 2)
  RegisterHotKey(Handle, 2, MOD_CONTROL or MOD_ALT, Ord('B'));
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Désenregistrer les raccourcis
  UnregisterHotKey(Handle, 1);
  UnregisterHotKey(Handle, 2);
end;
```

## Gestion des processus

### Obtenir des informations système

```pascal
uses
  Winapi.Windows;

// Obtenir le nom de l'ordinateur
function ObtenirNomOrdinateur: string;  
var  
  buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  taille: DWORD;
begin
  taille := Length(buffer);
  if GetComputerName(buffer, taille) then
    Result := string(buffer)
  else
    Result := '';
end;

// Obtenir le nom d'utilisateur
function ObtenirNomUtilisateur: string;  
var  
  buffer: array[0..255] of Char;
  taille: DWORD;
begin
  taille := Length(buffer);
  if GetUserName(buffer, taille) then
    Result := string(buffer)
  else
    Result := '';
end;

// Obtenir le répertoire Windows
function ObtenirRepWindows: string;  
var  
  buffer: array[0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(buffer, Length(buffer));
  Result := string(buffer);
end;

// Obtenir le répertoire System32
function ObtenirRepSystem: string;  
var  
  buffer: array[0..MAX_PATH] of Char;
begin
  GetSystemDirectory(buffer, Length(buffer));
  Result := string(buffer);
end;
```

### Informations sur le système

```pascal
// Obtenir la version de Windows
function ObtenirVersionWindows: string;  
var  
  Version: TOSVersionInfo;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(Version) then
  begin
    Result := Format('Windows %d.%d (Build %d)',
      [Version.dwMajorVersion, Version.dwMinorVersion, Version.dwBuildNumber]);
  end
  else
    Result := 'Version inconnue';
end;

// Obtenir la quantité de mémoire
function ObtenirMemoireDisponible: string;  
var  
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(TMemoryStatusEx);
  if GlobalMemoryStatusEx(MemStatus) then
  begin
    Result := Format('Mémoire: %d MB / %d MB',
      [MemStatus.ullAvailPhys div (1024*1024),
       MemStatus.ullTotalPhys div (1024*1024)]);
  end
  else
    Result := 'Information non disponible';
end;
```

### Lancer un processus

```pascal
// Méthode simple avec ShellExecute
procedure LancerApplication(const Fichier: string);  
begin  
  ShellExecute(0, 'open', PChar(Fichier), nil, nil, SW_SHOWNORMAL);
end;

// Méthode avancée avec CreateProcess
function LancerProcessus(const Application, Parametres: string): Boolean;  
var  
  SI: TStartupInfo;
  PI: TProcessInformation;
  CommandLine: string;
begin
  // Initialiser les structures
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);

  // Construire la ligne de commande
  CommandLine := '"' + Application + '" ' + Parametres;

  // Créer le processus
  Result := CreateProcess(
    nil,
    PChar(CommandLine),
    nil,
    nil,
    False,
    0,
    nil,
    nil,
    SI,
    PI
  );

  if Result then
  begin
    // Fermer les handles (le processus continue à s'exécuter)
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

// Lancer et attendre la fin du processus
function LancerEtAttendre(const Application: string): DWORD;  
var  
  SI: TStartupInfo;
  PI: TProcessInformation;
  ExitCode: DWORD;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);

  if CreateProcess(nil, PChar(Application), nil, nil, False, 0,
     nil, nil, SI, PI) then
  begin
    // Attendre que le processus se termine
    WaitForSingleObject(PI.hProcess, INFINITE);

    // Récupérer le code de sortie
    GetExitCodeProcess(PI.hProcess, ExitCode);
    Result := ExitCode;

    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end
  else
    Result := MAXDWORD; // Erreur
end;
```

### Terminer un processus

```pascal
// Terminer un processus par son handle
procedure TerminerProcessus(ProcessHandle: THandle);  
begin  
  TerminateProcess(ProcessHandle, 0);
end;

// Terminer un processus par son ID
procedure TerminerProcessusParID(ProcessID: DWORD);  
var  
  hProcess: THandle;
begin
  hProcess := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
  if hProcess <> 0 then
  begin
    try
      TerminateProcess(hProcess, 0);
    finally
      CloseHandle(hProcess);
    end;
  end;
end;
```

## Gestion du système de fichiers

### Attributs de fichiers

```pascal
uses
  Winapi.Windows;

// Vérifier si un fichier est en lecture seule
function EstLectureSeule(const Fichier: string): Boolean;  
var  
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(Fichier));
  Result := (Attr <> INVALID_FILE_ATTRIBUTES) and
            ((Attr and FILE_ATTRIBUTE_READONLY) <> 0);
end;

// Mettre/Enlever l'attribut lecture seule
procedure DefinirLectureSeule(const Fichier: string; LectureSeule: Boolean);  
var  
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(Fichier));
  if Attr = INVALID_FILE_ATTRIBUTES then Exit;

  if LectureSeule then
    Attr := Attr or FILE_ATTRIBUTE_READONLY
  else
    Attr := Attr and not FILE_ATTRIBUTE_READONLY;

  SetFileAttributes(PChar(Fichier), Attr);
end;

// Vérifier si un fichier est caché
function EstCache(const Fichier: string): Boolean;  
var  
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(Fichier));
  Result := (Attr <> INVALID_FILE_ATTRIBUTES) and
            ((Attr and FILE_ATTRIBUTE_HIDDEN) <> 0);
end;

// Cacher/Montrer un fichier
procedure DefinirCache(const Fichier: string; Cache: Boolean);  
var  
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(Fichier));
  if Attr = INVALID_FILE_ATTRIBUTES then Exit;

  if Cache then
    Attr := Attr or FILE_ATTRIBUTE_HIDDEN
  else
    Attr := Attr and not FILE_ATTRIBUTE_HIDDEN;

  SetFileAttributes(PChar(Fichier), Attr);
end;
```

### Informations sur les disques

```pascal
// Obtenir le type de lecteur
function ObtenirTypeLecteur(Lecteur: Char): string;  
var  
  TypeLecteur: UINT;
begin
  TypeLecteur := GetDriveType(PChar(Lecteur + ':\'));
  case TypeLecteur of
    DRIVE_REMOVABLE: Result := 'Amovible';
    DRIVE_FIXED: Result := 'Disque fixe';
    DRIVE_REMOTE: Result := 'Réseau';
    DRIVE_CDROM: Result := 'CD-ROM';
    DRIVE_RAMDISK: Result := 'RAM Disk';
  else
    Result := 'Inconnu';
  end;
end;

// Obtenir l'espace disque disponible
function ObtenirEspaceDisque(const Lecteur: string): string;  
var  
  Disponible, Total, Libre: Int64;
begin
  if GetDiskFreeSpaceEx(PChar(Lecteur), Disponible, Total, @Libre) then
  begin
    Result := Format('Libre: %.2f GB / Total: %.2f GB',
      [Libre / (1024*1024*1024), Total / (1024*1024*1024)]);
  end
  else
    Result := 'Information non disponible';
end;

// Lister tous les lecteurs
function ListerLecteurs: TStringList;  
var  
  Lecteurs: DWORD;
  I: Integer;
  Lettre: Char;
begin
  Result := TStringList.Create;
  Lecteurs := GetLogicalDrives;

  for I := 0 to 25 do
  begin
    if (Lecteurs and (1 shl I)) <> 0 then
    begin
      Lettre := Chr(Ord('A') + I);
      Result.Add(Lettre + ':\');
    end;
  end;
end;
```

### Copie et déplacement de fichiers

```pascal
// Copier un fichier avec barre de progression
function CopierFichierAvecProgres(const Source, Dest: string;
  ProgressProc: TFNProgressRoutine): Boolean;
begin
  Result := CopyFileEx(
    PChar(Source),
    PChar(Dest),
    ProgressProc,
    nil,
    nil,
    0
  );
end;

// Fonction de rappel pour la progression
function ProgressCallback(TotalFileSize, TotalBytesTransferred: Int64;
  StreamSize, StreamBytesTransferred: Int64; dwStreamNumber: DWORD;
  dwCallbackReason: DWORD; hSourceFile, hDestinationFile: THandle;
  lpData: Pointer): DWORD; stdcall;
var
  Pourcentage: Integer;
begin
  if TotalFileSize > 0 then
  begin
    Pourcentage := Round((TotalBytesTransferred / TotalFileSize) * 100);
    // Mettre à jour une barre de progression ici
  end;
  Result := PROGRESS_CONTINUE;
end;
```

## Registre Windows

### Lecture du registre

```pascal
uses
  Winapi.Windows, System.Win.Registry;

// Lire une valeur chaîne du registre
function LireRegistreChaîne(Cle: HKEY; const SousCle, Nom: string): string;  
var  
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := Cle;
    if Reg.OpenKeyReadOnly(SousCle) then
    begin
      try
        if Reg.ValueExists(Nom) then
          Result := Reg.ReadString(Nom);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

// Exemple d'utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Version: string;
begin
  // Lire la version de Windows dans le registre
  Version := LireRegistreChaîne(
    HKEY_LOCAL_MACHINE,
    'SOFTWARE\Microsoft\Windows NT\CurrentVersion',
    'ProductName'
  );
  ShowMessage('Windows: ' + Version);
end;
```

### Écriture dans le registre

```pascal
// Écrire une valeur dans le registre
function EcrireRegistre(Cle: HKEY; const SousCle, Nom, Valeur: string): Boolean;  
var  
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := Cle;
    if Reg.OpenKey(SousCle, True) then
    begin
      try
        Reg.WriteString(Nom, Valeur);
        Result := True;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

// Exemple : Sauvegarder des paramètres
procedure SauvegarderParametres;  
begin  
  EcrireRegistre(
    HKEY_CURRENT_USER,
    'SOFTWARE\MonApplication',
    'Username',
    'JohnDoe'
  );
end;
```

### Lancer une application au démarrage

```pascal
procedure AjouterDemarrage(const NomApp, CheminExe: string);  
var  
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', False) then
    begin
      try
        Reg.WriteString(NomApp, CheminExe);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure RetirerDemarrage(const NomApp: string);  
var  
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', False) then
    begin
      try
        if Reg.ValueExists(NomApp) then
          Reg.DeleteValue(NomApp);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;
```

## Presse-papiers

### Gestion du presse-papiers

```pascal
uses
  Winapi.Windows, Vcl.Clipbrd;

// Copier du texte dans le presse-papiers
procedure CopierTextePressePapiers(const Texte: string);  
begin  
  Clipboard.AsText := Texte;
end;

// Lire le texte du presse-papiers
function LireTextePressePapiers: string;  
begin  
  if Clipboard.HasFormat(CF_TEXT) then
    Result := Clipboard.AsText
  else
    Result := '';
end;

// Vider le presse-papiers
procedure ViderPressePapiers;  
begin  
  Clipboard.Clear;
end;

// Vérifier si le presse-papiers contient du texte
function PressePapiersContientTexte: Boolean;  
begin  
  Result := Clipboard.HasFormat(CF_TEXT);
end;
```

### Surveillance du presse-papiers

```pascal
type
  TForm1 = class(TForm)
  private
    FNextClipboardViewer: HWND;
  protected
    procedure WMChangeCBChain(var Msg: TWMChangeCBChain);
      message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg: TMessage);
      message WM_DRAWCLIPBOARD;
  public
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // S'enregistrer comme observateur du presse-papiers
  FNextClipboardViewer := SetClipboardViewer(Handle);
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Se désenregistrer
  ChangeClipboardChain(Handle, FNextClipboardViewer);
end;

procedure TForm1.WMChangeCBChain(var Msg: TWMChangeCBChain);  
begin  
  if Msg.Remove = FNextClipboardViewer then
    FNextClipboardViewer := Msg.Next
  else if FNextClipboardViewer <> 0 then
    SendMessage(FNextClipboardViewer, WM_CHANGECBCHAIN,
      Msg.Remove, Msg.Next);
end;

procedure TForm1.WMDrawClipboard(var Msg: TMessage);  
begin  
  // Le presse-papiers a changé
  Memo1.Lines.Add('Presse-papiers modifié: ' + Clipboard.AsText);

  // Passer le message au suivant
  if FNextClipboardViewer <> 0 then
    SendMessage(FNextClipboardViewer, WM_DRAWCLIPBOARD, 0, 0);
end;
```

## Graphiques avec GDI

### Dessin de base

```pascal
uses
  Winapi.Windows, Vcl.Graphics;

procedure DessinerSurCanvas(Canvas: TCanvas);  
var  
  OldPen: HPEN;
  DC: HDC;
begin
  DC := Canvas.Handle;

  // Créer un stylo rouge de 2 pixels
  OldPen := SelectObject(DC, CreatePen(PS_SOLID, 2, RGB(255, 0, 0)));

  try
    // Dessiner une ligne
    MoveToEx(DC, 10, 10, nil);
    LineTo(DC, 100, 100);

    // Dessiner un rectangle
    Rectangle(DC, 50, 50, 150, 100);

    // Dessiner un cercle
    Ellipse(DC, 200, 50, 300, 150);
  finally
    // Restaurer et supprimer le stylo
    DeleteObject(SelectObject(DC, OldPen));
  end;
end;
```

### Capture d'écran

```pascal
function CapturerEcran: TBitmap;  
var  
  DC: HDC;
  Bitmap: TBitmap;
  Width, Height: Integer;
begin
  // Obtenir les dimensions de l'écran
  Width := GetSystemMetrics(SM_CXSCREEN);
  Height := GetSystemMetrics(SM_CYSCREEN);

  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    // Obtenir le DC de l'écran
    DC := GetDC(0);
    try
      // Copier l'écran dans le bitmap
      BitBlt(Bitmap.Canvas.Handle, 0, 0, Width, Height,
        DC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(0, DC);
    end;

    Result := Bitmap;
  except
    Bitmap.Free;
    raise;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Screenshot: TBitmap;
begin
  Screenshot := CapturerEcran;
  try
    Screenshot.SaveToFile('capture.bmp');
    ShowMessage('Capture sauvegardée');
  finally
    Screenshot.Free;
  end;
end;
```

### Capture d'une fenêtre spécifique

```pascal
function CapturerFenetre(WindowHandle: HWND): TBitmap;  
var  
  DC: HDC;
  Bitmap: TBitmap;
  Rect: TRect;
begin
  GetWindowRect(WindowHandle, Rect);

  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Rect.Width;
    Bitmap.Height := Rect.Height;

    DC := GetWindowDC(WindowHandle);
    try
      BitBlt(Bitmap.Canvas.Handle, 0, 0, Rect.Width, Rect.Height,
        DC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(WindowHandle, DC);
    end;

    Result := Bitmap;
  except
    Bitmap.Free;
    raise;
  end;
end;
```

## Son système

### Jouer des sons

```pascal
uses
  Winapi.Windows, Winapi.MMSystem;

// Jouer un son système
procedure JouerSonSysteme(Son: Integer);  
begin  
  MessageBeep(Son);
  // Son peut être: MB_OK, MB_ICONERROR, MB_ICONWARNING, MB_ICONINFORMATION
end;

// Jouer un fichier WAV
procedure JouerFichierWAV(const Fichier: string);  
begin  
  PlaySound(PChar(Fichier), 0, SND_FILENAME or SND_ASYNC);
end;

// Jouer un son en boucle
procedure JouerEnBoucle(const Fichier: string);  
begin  
  PlaySound(PChar(Fichier), 0, SND_FILENAME or SND_ASYNC or SND_LOOP);
end;

// Arrêter tous les sons
procedure ArreterSons;  
begin  
  PlaySound(nil, 0, 0);
end;
```

## Gestion de l'alimentation

### État de la batterie

```pascal
uses
  Winapi.Windows;

function ObtenirEtatBatterie: string;  
var  
  Status: TSystemPowerStatus;
begin
  if GetSystemPowerStatus(Status) then
  begin
    if Status.ACLineStatus = 1 then
      Result := 'Sur secteur'
    else
      Result := 'Sur batterie';

    if Status.BatteryLifePercent <> 255 then
      Result := Result + Format(' - Niveau: %d%%', [Status.BatteryLifePercent]);
  end
  else
    Result := 'Information non disponible';
end;
```

### Empêcher la mise en veille

```pascal
procedure EmpecherMiseEnVeille(Empecher: Boolean);  
begin  
  if Empecher then
    SetThreadExecutionState(ES_CONTINUOUS or ES_SYSTEM_REQUIRED or ES_DISPLAY_REQUIRED)
  else
    SetThreadExecutionState(ES_CONTINUOUS);
end;
```

## Bonnes pratiques

### Vérification des erreurs

Toujours vérifier les valeurs de retour des fonctions API :

```pascal
function ExempleBonnesPratiques: Boolean;  
var  
  Handle: THandle;
begin
  Result := False;

  Handle := CreateFile(
    PChar('test.txt'),
    GENERIC_READ,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );

  if Handle = INVALID_HANDLE_VALUE then
  begin
    ShowMessage('Erreur: ' + SysErrorMessage(GetLastError));
    Exit;
  end;

  try
    // Utiliser le fichier...
    Result := True;
  finally
    CloseHandle(Handle);
  end;
end;
```

### Gestion des ressources

Toujours libérer les ressources allouées :

```pascal
procedure GestionRessources;  
var  
  DC: HDC;
  Bitmap: HBITMAP;
  OldBitmap: HBITMAP;
begin
  DC := CreateCompatibleDC(0);
  if DC = 0 then Exit;

  try
    Bitmap := CreateCompatibleBitmap(GetDC(0), 100, 100);
    if Bitmap = 0 then Exit;

    try
      OldBitmap := SelectObject(DC, Bitmap);
      try
        // Utiliser le DC et le bitmap...
      finally
        SelectObject(DC, OldBitmap);
      end;
    finally
      DeleteObject(Bitmap);
    end;
  finally
    DeleteDC(DC);
  end;
end;
```

### Documentation

Documentez vos appels API, surtout les complexes :

```pascal
/// <summary>
/// Énumère toutes les fenêtres visibles et retourne leurs titres
/// </summary>
/// <param name="Liste">Liste où seront ajoutés les titres</param>
/// <remarks>
/// Utilise l'API Windows EnumWindows avec un callback
/// Seules les fenêtres avec un titre non vide sont incluses
/// </remarks>
procedure ListerFenetres(Liste: TStrings);
```

## Résumé

L'API Windows native offre un contrôle complet sur le système d'exploitation.

**Points clés :**

1. Utilisez l'unité `Winapi.Windows` pour les déclarations
2. Les DLLs principales sont **user32.dll**, **kernel32.dll**, **gdi32.dll**
3. Toujours **vérifier les erreurs** avec `GetLastError`
4. **Libérer les ressources** (handles, DC, objets GDI)
5. Les **messages Windows** permettent la communication entre fenêtres
6. Le **registre** stocke les paramètres système et applications
7. Utilisez `try...finally` pour garantir le nettoyage
8. Attention aux **conventions d'appel** (stdcall pour Windows API)
9. Documentez les appels complexes
10. Testez sur différentes versions de Windows si possible

L'API Windows est vaste et puissante. Cette section couvre les fonctionnalités les plus courantes, mais il existe des milliers d'autres fonctions pour des besoins spécifiques. La documentation MSDN (Microsoft Developer Network) est votre meilleure ressource pour explorer davantage.

⏭️ [COM et ActiveX](/14-utilisation-dapi-et-bibliotheques-externes/04-com-et-activex.md)
