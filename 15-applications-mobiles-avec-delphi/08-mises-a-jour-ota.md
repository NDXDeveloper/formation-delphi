🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.8 Mises à jour OTA (Over The Air)

## Introduction

Les mises à jour OTA (Over The Air) permettent à votre application de se mettre à jour automatiquement sans nécessiter que l'utilisateur se rende sur le Google Play Store ou l'Apple App Store. Cette technologie, couramment utilisée dans les applications d'entreprise et certaines applications grand public, offre plusieurs avantages : déploiement rapide de correctifs, contrôle total du processus de mise à jour, et possibilité de mettre à jour uniquement certains composants de l'application.

Cependant, les mises à jour OTA comportent également des défis et des limitations, notamment sur iOS où Apple impose des restrictions strictes. Il est crucial de comprendre ce qui peut et ne peut pas être fait avec les mises à jour OTA avant de les implémenter dans votre application.

Dans cette section, nous allons explorer comment mettre en place un système de mise à jour OTA avec Delphi, tout en respectant les règles des plateformes mobiles et en garantissant une expérience utilisateur optimale.

## Qu'est-ce qu'une mise à jour OTA ?

### Définition

Une mise à jour OTA (Over The Air) est une mise à jour logicielle qui est téléchargée et installée directement par l'application elle-même, sans passer par les stores officiels. Le terme "Over The Air" signifie littéralement "par les airs", faisant référence à la transmission sans fil des données.

**Exemple concret** : Imaginez que vous découvrez un bug critique dans votre application un vendredi soir. Avec une mise à jour traditionnelle via les stores, vous devez attendre la validation (24-48h minimum pour l'App Store). Avec une mise à jour OTA, vous pouvez déployer le correctif immédiatement à tous vos utilisateurs.

### Différence avec les mises à jour via stores

**Mise à jour via Store** :
- Nécessite validation par Google/Apple (délai de quelques heures à plusieurs jours)
- Peut mettre à jour le code natif et la structure de l'application
- L'utilisateur doit accepter et installer la mise à jour
- Processus sécurisé et contrôlé par la plateforme

**Mise à jour OTA** :
- Déploiement immédiat, sans validation
- Généralement limitée au contenu et aux ressources (pas le code natif)
- Peut être automatique et transparente pour l'utilisateur
- Nécessite une infrastructure serveur propre

### Ce qui peut être mis à jour via OTA

**✅ Autorisé et recommandé** :
- Textes et traductions
- Images et ressources graphiques
- Fichiers de données (JSON, XML, bases de données)
- Configurations et paramètres
- Contenus dynamiques (articles, produits, etc.)
- Scripts ou code interprété (avec limitations iOS)

**❌ Non autorisé ou très limité** :
- Code natif compilé (binaires)
- Bibliothèques natives (.so, .dylib)
- Structure fondamentale de l'application
- Permissions système
- Icône de l'application et métadonnées

**⚠️ Important pour iOS** : Apple interdit strictement le téléchargement et l'exécution de code natif non approuvé. Votre application peut être rejetée ou retirée du store si vous enfreignez cette règle.

## Architecture d'un système OTA

### Composants nécessaires

Un système de mise à jour OTA complet comprend plusieurs composants :

**Côté serveur** :
1. **Serveur de fichiers** : Stocke les fichiers de mise à jour
2. **API de vérification de version** : Informe l'application des versions disponibles
3. **Base de données** : Enregistre les versions, métadonnées et statistiques
4. **Système de déploiement** : Gère la publication des mises à jour

**Côté application** :
1. **Module de vérification** : Vérifie périodiquement les mises à jour
2. **Module de téléchargement** : Récupère les fichiers
3. **Module d'installation** : Applique les mises à jour
4. **Système de rollback** : Permet de revenir en arrière en cas d'erreur

### Flux de mise à jour typique

```
1. Application vérifie la version actuelle
2. Requête au serveur : "Quelle est la dernière version ?"
3. Serveur répond avec les informations de version
4. Si nouvelle version disponible :
   a. Téléchargement des fichiers
   b. Vérification d'intégrité (checksum)
   c. Backup de l'ancienne version
   d. Installation de la nouvelle version
   e. Redémarrage ou rechargement de l'application
5. Sinon : Continuer normalement
```

## Implémentation d'un système OTA basique

### Structure de données pour les versions

Commençons par définir une structure pour gérer les informations de version.

```pascal
uses
  System.JSON;

type
  TVersionInfo = record
    VersionActuelle: string;
    VersionDisponible: string;
    URLTelechargement: string;
    TailleFichier: Int64;
    Checksum: string;
    EstObligatoire: Boolean;
    NotesVersion: string;
    DatePublication: TDateTime;

    function DepuisJSON(JSON: TJSONObject): TVersionInfo;
    function VersJSON: TJSONObject;
    function NouvelleVersionDisponible: Boolean;
  end;

function TVersionInfo.DepuisJSON(JSON: TJSONObject): TVersionInfo;  
begin  
  Result.VersionDisponible := JSON.GetValue<string>('version');
  Result.URLTelechargement := JSON.GetValue<string>('download_url');
  Result.TailleFichier := JSON.GetValue<Int64>('file_size');
  Result.Checksum := JSON.GetValue<string>('checksum');
  Result.EstObligatoire := JSON.GetValue<Boolean>('mandatory');
  Result.NotesVersion := JSON.GetValue<string>('release_notes');
  Result.DatePublication := ISO8601ToDate(JSON.GetValue<string>('published_at'));
end;

function TVersionInfo.VersJSON: TJSONObject;  
begin  
  Result := TJSONObject.Create;
  Result.AddPair('version', VersionDisponible);
  Result.AddPair('download_url', URLTelechargement);
  Result.AddPair('file_size', TJSONNumber.Create(TailleFichier));
  Result.AddPair('checksum', Checksum);
  Result.AddPair('mandatory', TJSONBool.Create(EstObligatoire));
  Result.AddPair('release_notes', NotesVersion);
  Result.AddPair('published_at', DateToISO8601(DatePublication));
end;

function TVersionInfo.NouvelleVersionDisponible: Boolean;  
begin  
  // Comparer les versions (simple comparaison de chaînes)
  Result := CompareVersion(VersionDisponible, VersionActuelle) > 0;
end;

// Fonction utilitaire pour comparer les versions
function CompareVersion(Version1, Version2: string): Integer;  
var  
  V1Parts, V2Parts: TArray<string>;
  i, Num1, Num2: Integer;
begin
  Result := 0;

  V1Parts := Version1.Split(['.']);
  V2Parts := Version2.Split(['.']);

  for i := 0 to Max(Length(V1Parts), Length(V2Parts)) - 1 do
  begin
    if i < Length(V1Parts) then
      Num1 := StrToIntDef(V1Parts[i], 0)
    else
      Num1 := 0;

    if i < Length(V2Parts) then
      Num2 := StrToIntDef(V2Parts[i], 0)
    else
      Num2 := 0;

    if Num1 > Num2 then
      Exit(1)
    else if Num1 < Num2 then
      Exit(-1);
  end;
end;
```

### Vérifier les mises à jour disponibles

```pascal
uses
  System.Net.HttpClient, System.IOUtils;

// Vérifier si une mise à jour est disponible
procedure TFormMain.VerifierMiseAJour;  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  VersionInfo: TVersionInfo;
  JSONResponse: TJSONObject;
begin
  HttpClient := THTTPClient.Create;
  try
    // Obtenir la version actuelle de l'application
    VersionInfo.VersionActuelle := GetVersionActuelle;

    // Requête au serveur
    Response := HttpClient.Get('https://votreserveur.com/api/version/check?version=' +
      VersionInfo.VersionActuelle);

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        VersionInfo := VersionInfo.DepuisJSON(JSONResponse);

        if VersionInfo.NouvelleVersionDisponible then
        begin
          // Une nouvelle version est disponible
          AfficherDialogueMiseAJour(VersionInfo);
        end
        else
        begin
          ShowMessage('Votre application est à jour !');
        end;
      finally
        JSONResponse.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Obtenir la version actuelle de l'application
function TFormMain.GetVersionActuelle: string;
{$IFDEF ANDROID}
var
  PackageInfo: JPackageInfo;
begin
  PackageInfo := TAndroidHelper.Context.getPackageManager.getPackageInfo(
    TAndroidHelper.Context.getPackageName, 0);
  Result := JStringToString(PackageInfo.versionName);
end;
{$ENDIF}

{$IFDEF IOS}
begin
  Result := TNSString.Wrap(
    TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).infoDictionary.objectForKey(
      NSSTR('CFBundleShortVersionString'))).UTF8String;
end;
{$ENDIF}

{$IFNDEF ANDROID}
{$IFNDEF IOS}
begin
  // Version par défaut pour Windows/autre
  Result := '1.0.0';
end;
{$ENDIF}
{$ENDIF}
```

### Afficher le dialogue de mise à jour

```pascal
// Proposer à l'utilisateur d'installer la mise à jour
procedure TFormMain.AfficherDialogueMiseAJour(VersionInfo: TVersionInfo);  
var  
  Message: string;
begin
  Message := Format('Une nouvelle version (%s) est disponible !' + sLineBreak + sLineBreak +
    'Taille : %s' + sLineBreak +
    'Nouveautés :' + sLineBreak + '%s',
    [VersionInfo.VersionDisponible,
     FormatTaille(VersionInfo.TailleFichier),
     VersionInfo.NotesVersion]);

  if VersionInfo.EstObligatoire then
  begin
    // Mise à jour obligatoire : pas de bouton annuler
    TDialogService.MessageDialog(
      'Mise à jour obligatoire' + sLineBreak + sLineBreak + Message,
      TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK],
      TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        TelevergerEtInstallerMiseAJour(VersionInfo);
      end);
  end
  else
  begin
    // Mise à jour optionnelle : l'utilisateur peut refuser
    TDialogService.MessageDialog(Message,
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
      TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          TelevergerEtInstallerMiseAJour(VersionInfo)
        else
          EnregistrerRefusMiseAJour(VersionInfo.VersionDisponible);
      end,
      'Installer', 'Plus tard');
  end;
end;

// Formater la taille du fichier de manière lisible
function TFormMain.FormatTaille(Octets: Int64): string;  
begin  
  if Octets < 1024 then
    Result := Format('%d octets', [Octets])
  else if Octets < 1024 * 1024 then
    Result := Format('%.2f Ko', [Octets / 1024])
  else if Octets < 1024 * 1024 * 1024 then
    Result := Format('%.2f Mo', [Octets / (1024 * 1024)])
  else
    Result := Format('%.2f Go', [Octets / (1024 * 1024 * 1024)]);
end;
```

### Télécharger la mise à jour

```pascal
uses
  System.Threading;

// Télécharger et installer la mise à jour
procedure TFormMain.TelevergerEtInstallerMiseAJour(VersionInfo: TVersionInfo);  
begin  
  // Afficher un indicateur de progression
  LayoutProgression.Visible := True;
  ProgressBar1.Value := 0;
  LabelProgression.Text := 'Préparation du téléchargement...';

  // Télécharger en arrière-plan
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      Response: IHTTPResponse;
      CheminFichier: string;
      Stream: TFileStream;
    begin
      HttpClient := THTTPClient.Create;
      try
        // Configurer le callback de progression
        HttpClient.OnReceiveData := procedure(const Sender: TObject;
          AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              if AContentLength > 0 then
              begin
                ProgressBar1.Max := AContentLength;
                ProgressBar1.Value := AReadCount;

                var Pourcentage := (AReadCount * 100) div AContentLength;
                LabelProgression.Text := Format('Téléchargement... %d%%', [Pourcentage]);
              end;
            end);
        end;

        // Télécharger le fichier
        CheminFichier := TPath.Combine(TPath.GetDocumentsPath,
          'update_' + VersionInfo.VersionDisponible + '.zip');

        Stream := TFileStream.Create(CheminFichier, fmCreate);
        try
          Response := HttpClient.Get(VersionInfo.URLTelechargement, Stream);

          if Response.StatusCode = 200 then
          begin
            // Vérifier l'intégrité du fichier
            TThread.Synchronize(nil,
              procedure
              begin
                LabelProgression.Text := 'Vérification du fichier...';
              end);

            if VerifierChecksum(CheminFichier, VersionInfo.Checksum) then
            begin
              // Installation
              TThread.Synchronize(nil,
                procedure
                begin
                  LabelProgression.Text := 'Installation en cours...';
                  InstallerMiseAJour(CheminFichier, VersionInfo);
                end);
            end
            else
            begin
              TThread.Synchronize(nil,
                procedure
                begin
                  LayoutProgression.Visible := False;
                  ShowMessage('Erreur : Le fichier téléchargé est corrompu.');
                  TFile.Delete(CheminFichier);
                end);
            end;
          end
          else
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                LayoutProgression.Visible := False;
                ShowMessage('Erreur de téléchargement : ' + Response.StatusCode.ToString);
              end);
          end;
        finally
          Stream.Free;
        end;
      finally
        HttpClient.Free;
      end;
    end);
end;

// Vérifier l'intégrité du fichier avec un checksum MD5
function TFormMain.VerifierChecksum(CheminFichier, ChecksumAttendu: string): Boolean;  
var  
  Checksum: string;
begin
  Checksum := CalculerMD5(CheminFichier);
  Result := SameText(Checksum, ChecksumAttendu);
end;

// Calculer le MD5 d'un fichier
function TFormMain.CalculerMD5(CheminFichier: string): string;  
var  
  MD5: THashMD5;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(CheminFichier, fmOpenRead);
  try
    Result := MD5.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;
end;
```

### Installer la mise à jour

```pascal
uses
  System.Zip;

// Installer la mise à jour téléchargée
procedure TFormMain.InstallerMiseAJour(CheminFichierZip: string;
  VersionInfo: TVersionInfo);
var
  DossierExtraction: string;
  DossierBackup: string;
begin
  try
    // Créer un backup de la version actuelle
    DossierBackup := TPath.Combine(TPath.GetDocumentsPath, 'backup');
    if TDirectory.Exists(DossierBackup) then
      TDirectory.Delete(DossierBackup, True);

    TDirectory.CreateDirectory(DossierBackup);
    SauvegarderVersionActuelle(DossierBackup);

    // Extraire la mise à jour
    DossierExtraction := TPath.Combine(TPath.GetDocumentsPath, 'update_temp');
    if TDirectory.Exists(DossierExtraction) then
      TDirectory.Delete(DossierExtraction, True);

    TDirectory.CreateDirectory(DossierExtraction);
    TZipFile.ExtractZipFile(CheminFichierZip, DossierExtraction);

    // Appliquer les mises à jour
    AppliquerFichiersMAJ(DossierExtraction);

    // Mettre à jour la version dans les préférences
    SauvegarderVersionInstalle(VersionInfo.VersionDisponible);

    // Nettoyer
    TFile.Delete(CheminFichierZip);
    TDirectory.Delete(DossierExtraction, True);

    LayoutProgression.Visible := False;

    // Proposer de redémarrer l'application
    TDialogService.MessageDialog(
      'Mise à jour installée avec succès !' + sLineBreak +
      'Redémarrer l''application pour appliquer les changements ?',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
      TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          RedemarrerApplication;
      end);

  except
    on E: Exception do
    begin
      // En cas d'erreur, restaurer le backup
      LayoutProgression.Visible := False;
      RestaurerBackup(DossierBackup);
      ShowMessage('Erreur lors de l''installation : ' + E.Message);
    end;
  end;
end;

// Appliquer les fichiers de mise à jour
procedure TFormMain.AppliquerFichiersMAJ(DossierSource: string);  
var  
  Fichier: string;
  CheminDestination: string;
begin
  // Copier tous les fichiers du dossier de mise à jour vers l'application
  for Fichier in TDirectory.GetFiles(DossierSource, '*.*',
    TSearchOption.soAllDirectories) do
  begin
    CheminDestination := Fichier.Replace(DossierSource, TPath.GetDocumentsPath);

    // Créer le dossier de destination si nécessaire
    TDirectory.CreateDirectory(TPath.GetDirectoryName(CheminDestination));

    // Copier le fichier
    TFile.Copy(Fichier, CheminDestination, True);
  end;
end;

// Sauvegarder la version actuelle avant la mise à jour
procedure TFormMain.SauvegarderVersionActuelle(DossierBackup: string);  
var  
  DossierApp: string;
  Fichier: string;
  CheminDestination: string;
begin
  DossierApp := TPath.GetDocumentsPath;

  // Copier tous les fichiers importants
  for Fichier in TDirectory.GetFiles(DossierApp, '*.*',
    TSearchOption.soAllDirectories) do
  begin
    // Ignorer les fichiers temporaires et le dossier backup lui-même
    if not Fichier.Contains('temp') and not Fichier.Contains('backup') then
    begin
      CheminDestination := Fichier.Replace(DossierApp, DossierBackup);
      TDirectory.CreateDirectory(TPath.GetDirectoryName(CheminDestination));
      TFile.Copy(Fichier, CheminDestination, True);
    end;
  end;
end;

// Restaurer le backup en cas d'échec
procedure TFormMain.RestaurerBackup(DossierBackup: string);  
var  
  Fichier: string;
  CheminDestination: string;
begin
  if not TDirectory.Exists(DossierBackup) then
    Exit;

  for Fichier in TDirectory.GetFiles(DossierBackup, '*.*',
    TSearchOption.soAllDirectories) do
  begin
    CheminDestination := Fichier.Replace(DossierBackup, TPath.GetDocumentsPath);
    TDirectory.CreateDirectory(TPath.GetDirectoryName(CheminDestination));
    TFile.Copy(Fichier, CheminDestination, True);
  end;

  ShowMessage('Backup restauré. L''application est revenue à l''état précédent.');
end;
```

### Redémarrer l'application

```pascal
// Redémarrer l'application pour appliquer les changements
procedure TFormMain.RedemarrerApplication;  
begin  
  {$IFDEF ANDROID}
  // Sur Android, relancer l'activité principale
  var Intent := TJIntent.Create;
  Intent.setClassName(TAndroidHelper.Context.getPackageName,
    StringToJString('com.embarcadero.firemonkey.FMXNativeActivity'));
  Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or
    TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TASK);
  TAndroidHelper.Context.startActivity(Intent);

  // Terminer l'activité actuelle
  TAndroidHelper.Activity.finish;
  {$ENDIF}

  {$IFDEF IOS}
  // Sur iOS, impossible de redémarrer programmatiquement
  // L'utilisateur doit fermer et rouvrir l'application manuellement
  ShowMessage('Veuillez fermer et rouvrir l''application pour appliquer les changements.');
  {$ENDIF}
end;
```

## Vérification automatique des mises à jour

### Au démarrage de l'application

```pascal
// Vérifier les mises à jour au démarrage
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Vérifier seulement si connecté à Internet
  if EstConnecteInternet then
  begin
    // Ne pas bloquer l'interface, vérifier en arrière-plan
    TTask.Run(
      procedure
      begin
        Sleep(2000); // Attendre 2 secondes après le démarrage

        TThread.Synchronize(nil,
          procedure
          begin
            VerifierMiseAJour;
          end);
      end);
  end;
end;
```

### Vérification périodique

```pascal
// Timer pour vérifier périodiquement les mises à jour
procedure TFormMain.ConfigurerVerificationPeriodique;  
begin  
  TimerMiseAJour := TTimer.Create(Self);
  TimerMiseAJour.Interval := 3600000; // 1 heure
  TimerMiseAJour.OnTimer := TimerMiseAJourTimer;
  TimerMiseAJour.Enabled := True;
end;

procedure TFormMain.TimerMiseAJourTimer(Sender: TObject);  
begin  
  if EstConnecteInternet then
    VerifierMiseAJour;
end;
```

### Vérification en fonction de la dernière vérification

```pascal
// Vérifier seulement après un certain délai
procedure TFormMain.VerifierSiNecessaire;  
var  
  DerniereVerif: TDateTime;
  DelaiHeures: Integer;
begin
  DelaiHeures := 24; // Vérifier max une fois par jour

  // Lire la date de dernière vérification
  DerniereVerif := StrToDateTimeDef(
    LirePreference('DerniereVerifMAJ', ''),
    Now - 365);

  // Vérifier si assez de temps s'est écoulé
  if HoursBetween(Now, DerniereVerif) >= DelaiHeures then
  begin
    VerifierMiseAJour;
    SauvegarderPreference('DerniereVerifMAJ', DateTimeToStr(Now));
  end;
end;
```

## Gestion des erreurs et rollback

### Stratégie de rollback

```pascal
// Classe pour gérer le rollback automatique
type
  TRollbackManager = class
  private
    FDossierBackup: string;
    FVersionBackup: string;
  public
    constructor Create;
    procedure CreerBackup;
    procedure Rollback;
    procedure SupprimerBackup;
    function BackupExiste: Boolean;
  end;

constructor TRollbackManager.Create;  
begin  
  FDossierBackup := TPath.Combine(TPath.GetDocumentsPath, 'app_backup');
end;

procedure TRollbackManager.CreerBackup;  
begin  
  // Créer un backup avant la mise à jour
  if TDirectory.Exists(FDossierBackup) then
    TDirectory.Delete(FDossierBackup, True);

  TDirectory.CreateDirectory(FDossierBackup);
  FVersionBackup := GetVersionActuelle;

  // Sauvegarder les fichiers critiques
  // ... (code de sauvegarde)
end;

procedure TRollbackManager.Rollback;  
begin  
  if not BackupExiste then
    raise Exception.Create('Aucun backup disponible');

  // Restaurer les fichiers depuis le backup
  // ... (code de restauration)

  ShowMessage('Application restaurée à la version ' + FVersionBackup);
end;

function TRollbackManager.BackupExiste: Boolean;  
begin  
  Result := TDirectory.Exists(FDossierBackup);
end;
```

### Détection de problèmes après mise à jour

```pascal
// Vérifier si l'application fonctionne correctement après la mise à jour
procedure TFormMain.VerifierSanteApplication;  
var  
  NombreCrashs: Integer;
  DerniereMiseAJour: TDateTime;
begin
  NombreCrashs := LirePreference('NombreCrashsDepuisMAJ', 0);
  DerniereMiseAJour := StrToDateTimeDef(
    LirePreference('DateDerniereMiseAJour', ''), Now);

  // Si plus de 3 crashs dans les 24h suivant la mise à jour
  if (NombreCrashs >= 3) and
     (HoursBetween(Now, DerniereMiseAJour) < 24) then
  begin
    TDialogService.MessageDialog(
      'L''application semble instable depuis la dernière mise à jour. ' +
      'Voulez-vous revenir à la version précédente ?',
      TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
      TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
        begin
          var RollbackMgr := TRollbackManager.Create;
          try
            RollbackMgr.Rollback;
            SauvegarderPreference('NombreCrashsDepuisMAJ', 0);
          finally
            RollbackMgr.Free;
          end;
        end;
      end);
  end;
end;

// Enregistrer un crash
procedure TFormMain.EnregistrerCrash;  
var  
  Compteur: Integer;
begin
  Compteur := LirePreference('NombreCrashsDepuisMAJ', 0);
  Inc(Compteur);
  SauvegarderPreference('NombreCrashsDepuisMAJ', Compteur);
end;
```

## Mises à jour partielles (Delta Updates)

Pour économiser la bande passante et accélérer les mises à jour, vous pouvez n'envoyer que les fichiers modifiés.

```pascal
type
  TDeltaUpdate = record
    FichiersAjoutes: TArray<string>;
    FichiersModifies: TArray<string>;
    FichiersSupprimes: TArray<string>;
  end;

// Appliquer une mise à jour delta
procedure TFormMain.AppliquerMiseAJourDelta(CheminManifeste: string);  
var  
  Manifeste: TJSONObject;
  DeltaInfo: TDeltaUpdate;
  i: Integer;
begin
  // Charger le manifeste
  Manifeste := TJSONObject.ParseJSONValue(
    TFile.ReadAllText(CheminManifeste)) as TJSONObject;
  try
    // Lire les fichiers ajoutés
    var JSONAjoutes := Manifeste.GetValue<TJSONArray>('files_added');
    SetLength(DeltaInfo.FichiersAjoutes, JSONAjoutes.Count);
    for i := 0 to JSONAjoutes.Count - 1 do
      DeltaInfo.FichiersAjoutes[i] := JSONAjoutes.Items[i].Value;

    // Lire les fichiers modifiés
    var JSONModifies := Manifeste.GetValue<TJSONArray>('files_modified');
    SetLength(DeltaInfo.FichiersModifies, JSONModifies.Count);
    for i := 0 to JSONModifies.Count - 1 do
      DeltaInfo.FichiersModifies[i] := JSONModifies.Items[i].Value;

    // Lire les fichiers à supprimer
    var JSONSupprimes := Manifeste.GetValue<TJSONArray>('files_deleted');
    SetLength(DeltaInfo.FichiersSupprimes, JSONSupprimes.Count);
    for i := 0 to JSONSupprimes.Count - 1 do
      DeltaInfo.FichiersSupprimes[i] := JSONSupprimes.Items[i].Value;

    // Appliquer les changements
    for var Fichier in DeltaInfo.FichiersSupprimes do
      TFile.Delete(TPath.Combine(TPath.GetDocumentsPath, Fichier));

    // Les fichiers ajoutés et modifiés sont dans l'archive
    // et seront extraits normalement

  finally
    Manifeste.Free;
  end;
end;
```

## Considérations de sécurité

### Signature et vérification des mises à jour

```pascal
uses
  System.Hash;

// Vérifier la signature numérique d'une mise à jour
function TFormMain.VerifierSignature(CheminFichier, Signature,
  ClePublique: string): Boolean;
var
  Hash: string;
  SignatureAttendue: string;
begin
  // Calculer le hash du fichier
  Hash := CalculerSHA256(CheminFichier);

  // Vérifier avec la signature (implémentation simplifiée)
  // En production, utilisez une véritable cryptographie asymétrique
  SignatureAttendue := CalculerSignature(Hash, ClePublique);

  Result := SameText(Signature, SignatureAttendue);
end;

// Calculer le SHA256 d'un fichier
function TFormMain.CalculerSHA256(CheminFichier: string): string;  
var  
  SHA256: THashSHA2;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(CheminFichier, fmOpenRead);
  try
    Result := SHA256.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;
end;
```

### Communication sécurisée (HTTPS)

```pascal
// Toujours utiliser HTTPS pour les mises à jour
procedure TFormMain.VerifierURLSecurisee(URL: string);  
begin  
  if not URL.StartsWith('https://', True) then
    raise Exception.Create('Les mises à jour doivent utiliser HTTPS');
end;

// Configurer le client HTTP pour la sécurité
procedure TFormMain.ConfigurerHTTPSecurise(HttpClient: THTTPClient);  
begin  
  // Valider les certificats SSL
  HttpClient.SecureProtocols := [THTTPSecureProtocol.SSL3,
                                   THTTPSecureProtocol.TLS12];

  // Timeout raisonnable
  HttpClient.ConnectionTimeout := 30000; // 30 secondes
  HttpClient.ResponseTimeout := 60000;   // 60 secondes
end;
```

## Limitations et restrictions par plateforme

### Restrictions iOS

Apple impose des règles strictes concernant les mises à jour OTA :

**❌ Interdit** :
- Télécharger et exécuter du code natif (Objective-C, Swift compilé)
- Modifier le comportement fondamental de l'application
- Contourner le processus de validation de l'App Store
- Utiliser des API privées non documentées

**✅ Autorisé** :
- Mettre à jour le contenu (textes, images, données)
- Modifier les ressources graphiques
- Télécharger de nouvelles données depuis un serveur
- Interpréter du JavaScript (via WKWebView) dans certaines limites

**Conséquence** : Violation des règles = Rejet de l'application ou retrait du store

### Possibilités Android

Android est plus permissif mais nécessite des précautions :

**✅ Plus de liberté** :
- Installation d'APK depuis des sources tierces (avec permission)
- Plus de flexibilité dans les mises à jour
- Accès à des API système étendues

**⚠️ Attention** :
- Les utilisateurs doivent autoriser les installations de sources inconnues
- Google Play peut rejeter les applications qui contournent le store
- Risques de sécurité si mal implémenté

### Approche recommandée multi-plateforme

```pascal
// Adapter la stratégie OTA selon la plateforme
procedure TFormMain.ConfigurerStrategieOTA;  
begin  
  {$IFDEF IOS}
  // Sur iOS : Mises à jour de contenu uniquement
  FTypeMAJAutorisees := [tmajContenu, tmajRessources, tmajDonnees];
  {$ENDIF}

  {$IFDEF ANDROID}
  // Sur Android : Plus de possibilités
  FTypeMAJAutorisees := [tmajContenu, tmajRessources, tmajDonnees, tmajModules];
  {$ENDIF}
end;
```

## Meilleures pratiques

### 1. Tests approfondis avant déploiement

```pascal
// Déployer d'abord en test sur un groupe restreint
procedure TFormMain.DeployerEnBeta(VersionInfo: TVersionInfo);  
begin  
  // Marquer comme version beta
  VersionInfo.IsBeta := True;
  VersionInfo.GroupeCible := 'beta_testers';

  // Publier seulement pour le groupe beta
  PublierMiseAJour(VersionInfo);

  // Surveiller pendant 24-48h avant déploiement complet
end;
```

### 2. Déploiement progressif

```pascal
// Déployer progressivement à un pourcentage croissant d'utilisateurs
procedure TFormMain.DeployerProgressivement(VersionInfo: TVersionInfo);  
begin  
  // Jour 1 : 10% des utilisateurs
  VersionInfo.PourcentageDeploiement := 10;
  PublierMiseAJour(VersionInfo);

  // Jour 2 : 25% si pas de problèmes
  // Jour 3 : 50%
  // Jour 4 : 100%
end;
```

### 3. Logging et télémétrie

```pascal
// Logger tous les événements importants
procedure TFormMain.LoggerEvenementMAJ(Evenement, Details: string);  
var  
  LogEntry: TJSONObject;
begin
  LogEntry := TJSONObject.Create;
  try
    LogEntry.AddPair('timestamp', DateTimeToStr(Now));
    LogEntry.AddPair('event', Evenement);
    LogEntry.AddPair('details', Details);
    LogEntry.AddPair('version', GetVersionActuelle);
    LogEntry.AddPair('device_id', GetDeviceID);

    // Envoyer au serveur d'analytics
    EnvoyerLogServeur(LogEntry);
  finally
    LogEntry.Free;
  end;
end;

// Utilisation
procedure TFormMain.TelevergerEtInstallerMiseAJour(VersionInfo: TVersionInfo);  
begin  
  LoggerEvenementMAJ('update_started',
    'Version: ' + VersionInfo.VersionDisponible);

  // ... code de téléchargement ...

  if Success then
    LoggerEvenementMAJ('update_success', 'Installation réussie')
  else
    LoggerEvenementMAJ('update_failed', 'Erreur: ' + ErrorMessage);
end;
```

### 4. Gestion des échecs réseau

```pascal
// Reprendre le téléchargement en cas d'interruption
type
  TResumableDownload = class
  private
    FURL: string;
    FCheminLocal: string;
    FOctetsTelecharges: Int64;
  public
    procedure TelevergerAvecReprise;
    function PeutReprendre: Boolean;
  end;

procedure TResumableDownload.TelevergerAvecReprise;  
var  
  HttpClient: THTTPClient;
  Headers: TNetHeaders;
begin
  HttpClient := THTTPClient.Create;
  try
    // Si on a déjà téléchargé une partie
    if TFile.Exists(FCheminLocal) then
    begin
      FOctetsTelecharges := TFile.GetSize(FCheminLocal);

      // Demander la suite du fichier avec Range header
      SetLength(Headers, 1);
      Headers[0].Name := 'Range';
      Headers[0].Value := Format('bytes=%d-', [FOctetsTelecharges]);

      // Télécharger la suite
      HttpClient.CustomHeaders := Headers;
    end;

    // Télécharger...
  finally
    HttpClient.Free;
  end;
end;
```

### 5. Interface utilisateur claire

```pascal
// Afficher clairement l'état de la mise à jour
procedure TFormMain.AfficherEtatMAJ(Etat: string; Progression: Integer);  
begin  
  LabelEtat.Text := Etat;
  ProgressBar1.Value := Progression;

  // Permettre d'annuler si pas encore installé
  if (Etat = 'Téléchargement...') and (Progression < 100) then
    BtnAnnuler.Visible := True
  else
    BtnAnnuler.Visible := False;
end;
```

## Conclusion

Les mises à jour OTA sont un outil puissant pour maintenir votre application à jour rapidement et efficacement. Cependant, elles doivent être utilisées avec précaution et en respectant les règles des plateformes.

**Points clés à retenir** :

1. **Contenu seulement sur iOS** : Respectez strictement les règles d'Apple
2. **Sécurité** : Utilisez HTTPS, vérifiez l'intégrité des fichiers, signez vos mises à jour
3. **Backup** : Toujours créer un backup avant une mise à jour
4. **Tests** : Testez exhaustivement avant de déployer
5. **Progressif** : Déployez graduellement pour détecter les problèmes
6. **Rollback** : Ayez un plan pour revenir en arrière en cas de problème
7. **Communication** : Informez clairement l'utilisateur de ce qui se passe
8. **Logs** : Surveillez les mises à jour pour détecter les problèmes rapidement

Les mises à jour OTA complètent mais ne remplacent pas les mises à jour via les stores officiels. Utilisez-les pour du contenu et des corrections urgentes, mais continuez à publier des versions majeures via les stores pour bénéficier de leur validation et de leur visibilité.

En combinant intelligemment mises à jour OTA et mises à jour traditionnelles, vous offrirez à vos utilisateurs la meilleure expérience possible : une application toujours à jour, stable et conforme aux standards des plateformes mobiles.

⏭️ [Partage de code entre applications mobile et desktop](/15-applications-mobiles-avec-delphi/09-partage-de-code-entre-applications.md)
