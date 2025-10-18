🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.6 Stockage local et synchronisation

## Introduction

Les applications mobiles modernes doivent souvent fonctionner même sans connexion Internet. Que ce soit dans le métro, en avion, ou dans une zone sans couverture réseau, vos utilisateurs s'attendent à pouvoir continuer à utiliser votre application. C'est là qu'interviennent le stockage local et la synchronisation.

Le **stockage local** permet de sauvegarder des données directement sur l'appareil de l'utilisateur : paramètres de l'application, contenu consulté, brouillons de messages, ou toute autre information nécessaire au fonctionnement de l'application. La **synchronisation** garantit que ces données locales restent cohérentes avec celles stockées sur un serveur distant, permettant ainsi une expérience fluide entre plusieurs appareils et modes (en ligne/hors ligne).

Dans cette section, nous allons explorer les différentes méthodes de stockage disponibles sur mobile avec Delphi, et apprendre à créer des stratégies de synchronisation efficaces.

## Options de stockage local

Les appareils mobiles offrent plusieurs méthodes pour stocker des données localement, chacune adaptée à des besoins spécifiques.

### Vue d'ensemble des options

**Fichiers de préférences** :
- Pour les paramètres simples (clé-valeur)
- Très rapide et simple à utiliser
- Idéal pour les configurations de l'application

**Fichiers texte et JSON** :
- Pour des données structurées simples
- Facilement lisibles et modifiables
- Parfait pour des exports ou des données temporaires

**Base de données SQLite** :
- Pour des données structurées complexes
- Requêtes SQL puissantes
- Idéal pour de grandes quantités de données

**Fichiers binaires** :
- Pour stocker des objets complexes
- Sérialisation personnalisée
- Utile pour la mise en cache

## Stockage de préférences simples

Pour les paramètres de configuration de votre application, Delphi offre plusieurs solutions simples.

### Utilisation de TIniFile

Le format INI est simple et lisible, parfait pour les préférences utilisateur.

```pascal
uses
  System.IniFiles, System.IOUtils;

// Sauvegarder des préférences
procedure TFormMain.SauvegarderPreferences;
var
  IniFile: TIniFile;
  CheminIni: string;
begin
  // Obtenir le chemin du dossier Documents
  CheminIni := TPath.Combine(TPath.GetDocumentsPath, 'preferences.ini');

  IniFile := TIniFile.Create(CheminIni);
  try
    // Sauvegarder différents types de données
    IniFile.WriteString('Utilisateur', 'Nom', EditNom.Text);
    IniFile.WriteString('Utilisateur', 'Email', EditEmail.Text);
    IniFile.WriteBool('Parametres', 'NotificationsActives', SwitchNotif.IsChecked);
    IniFile.WriteInteger('Parametres', 'ThemeIndex', ComboTheme.ItemIndex);
    IniFile.WriteFloat('Parametres', 'VolumeMusique', TrackBarVolume.Value);

    ShowMessage('Préférences sauvegardées');
  finally
    IniFile.Free;
  end;
end;

// Charger des préférences
procedure TFormMain.ChargerPreferences;
var
  IniFile: TIniFile;
  CheminIni: string;
begin
  CheminIni := TPath.Combine(TPath.GetDocumentsPath, 'preferences.ini');

  // Vérifier si le fichier existe
  if not TFile.Exists(CheminIni) then
  begin
    // Utiliser les valeurs par défaut
    Exit;
  end;

  IniFile := TIniFile.Create(CheminIni);
  try
    // Charger les données avec valeurs par défaut si non présentes
    EditNom.Text := IniFile.ReadString('Utilisateur', 'Nom', '');
    EditEmail.Text := IniFile.ReadString('Utilisateur', 'Email', '');
    SwitchNotif.IsChecked := IniFile.ReadBool('Parametres', 'NotificationsActives', True);
    ComboTheme.ItemIndex := IniFile.ReadInteger('Parametres', 'ThemeIndex', 0);
    TrackBarVolume.Value := IniFile.ReadFloat('Parametres', 'VolumeMusique', 50.0);
  finally
    IniFile.Free;
  end;
end;
```

### Classe utilitaire pour les préférences

Pour faciliter la gestion des préférences, créons une classe utilitaire :

```pascal
type
  TPreferences = class
  private
    FIniFile: TIniFile;
  public
    constructor Create;
    destructor Destroy; override;

    // Méthodes génériques
    procedure SauvegarderString(Section, Cle, Valeur: string);
    function ChargerString(Section, Cle, ValeurDefaut: string): string;
    procedure SauvegarderBool(Section, Cle: string; Valeur: Boolean);
    function ChargerBool(Section, Cle: string; ValeurDefaut: Boolean): Boolean;
    procedure SauvegarderInteger(Section, Cle: string; Valeur: Integer);
    function ChargerInteger(Section, Cle: string; ValeurDefaut: Integer): Integer;

    procedure Effacer;
  end;

constructor TPreferences.Create;
var
  CheminIni: string;
begin
  CheminIni := TPath.Combine(TPath.GetDocumentsPath, 'app_preferences.ini');
  FIniFile := TIniFile.Create(CheminIni);
end;

destructor TPreferences.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TPreferences.SauvegarderString(Section, Cle, Valeur: string);
begin
  FIniFile.WriteString(Section, Cle, Valeur);
end;

function TPreferences.ChargerString(Section, Cle, ValeurDefaut: string): string;
begin
  Result := FIniFile.ReadString(Section, Cle, ValeurDefaut);
end;

procedure TPreferences.SauvegarderBool(Section, Cle: string; Valeur: Boolean);
begin
  FIniFile.WriteBool(Section, Cle, Valeur);
end;

function TPreferences.ChargerBool(Section, Cle: string; ValeurDefaut: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(Section, Cle, ValeurDefaut);
end;

procedure TPreferences.Effacer;
begin
  TFile.Delete(FIniFile.FileName);
end;

// Utilisation
var
  Prefs: TPreferences;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Prefs := TPreferences.Create;

  // Charger les préférences
  EditNom.Text := Prefs.ChargerString('User', 'Name', '');
  SwitchMode.IsChecked := Prefs.ChargerBool('Settings', 'DarkMode', False);
end;

procedure TFormMain.BtnSauvegarderClick(Sender: TObject);
begin
  Prefs.SauvegarderString('User', 'Name', EditNom.Text);
  Prefs.SauvegarderBool('Settings', 'DarkMode', SwitchMode.IsChecked);
end;
```

## Stockage de données structurées avec JSON

JSON est un format populaire pour stocker et échanger des données structurées.

### Sauvegarder et charger des objets en JSON

```pascal
uses
  System.JSON, System.IOUtils;

type
  TUtilisateur = class
  public
    Nom: string;
    Email: string;
    Age: Integer;
    Preferences: TStringList;

    constructor Create;
    destructor Destroy; override;

    function VersJSON: TJSONObject;
    procedure DepuisJSON(JSON: TJSONObject);
  end;

constructor TUtilisateur.Create;
begin
  Preferences := TStringList.Create;
end;

destructor TUtilisateur.Destroy;
begin
  Preferences.Free;
  inherited;
end;

// Convertir l'utilisateur en JSON
function TUtilisateur.VersJSON: TJSONObject;
var
  PrefArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;

  Result.AddPair('nom', Nom);
  Result.AddPair('email', Email);
  Result.AddPair('age', TJSONNumber.Create(Age));

  // Tableau de préférences
  PrefArray := TJSONArray.Create;
  for i := 0 to Preferences.Count - 1 do
    PrefArray.Add(Preferences[i]);

  Result.AddPair('preferences', PrefArray);
end;

// Charger depuis JSON
procedure TUtilisateur.DepuisJSON(JSON: TJSONObject);
var
  PrefArray: TJSONArray;
  i: Integer;
begin
  Nom := JSON.GetValue<string>('nom');
  Email := JSON.GetValue<string>('email');
  Age := JSON.GetValue<Integer>('age');

  Preferences.Clear;
  PrefArray := JSON.GetValue<TJSONArray>('preferences');
  if Assigned(PrefArray) then
  begin
    for i := 0 to PrefArray.Count - 1 do
      Preferences.Add(PrefArray.Items[i].Value);
  end;
end;

// Sauvegarder dans un fichier
procedure TFormMain.SauvegarderUtilisateurJSON;
var
  Utilisateur: TUtilisateur;
  JSON: TJSONObject;
  CheminFichier: string;
begin
  Utilisateur := TUtilisateur.Create;
  try
    // Remplir les données
    Utilisateur.Nom := EditNom.Text;
    Utilisateur.Email := EditEmail.Text;
    Utilisateur.Age := StrToIntDef(EditAge.Text, 0);

    // Convertir en JSON
    JSON := Utilisateur.VersJSON;
    try
      // Sauvegarder dans un fichier
      CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'utilisateur.json');
      TFile.WriteAllText(CheminFichier, JSON.ToString, TEncoding.UTF8);

      ShowMessage('Utilisateur sauvegardé');
    finally
      JSON.Free;
    end;
  finally
    Utilisateur.Free;
  end;
end;

// Charger depuis un fichier
procedure TFormMain.ChargerUtilisateurJSON;
var
  Utilisateur: TUtilisateur;
  JSON: TJSONObject;
  CheminFichier: string;
  Contenu: string;
begin
  CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'utilisateur.json');

  if not TFile.Exists(CheminFichier) then
  begin
    ShowMessage('Aucun utilisateur sauvegardé');
    Exit;
  end;

  // Lire le fichier
  Contenu := TFile.ReadAllText(CheminFichier, TEncoding.UTF8);

  JSON := TJSONObject.ParseJSONValue(Contenu) as TJSONObject;
  try
    Utilisateur := TUtilisateur.Create;
    try
      // Charger les données
      Utilisateur.DepuisJSON(JSON);

      // Afficher dans l'interface
      EditNom.Text := Utilisateur.Nom;
      EditEmail.Text := Utilisateur.Email;
      EditAge.Text := Utilisateur.Age.ToString;
    finally
      Utilisateur.Free;
    end;
  finally
    JSON.Free;
  end;
end;
```

### Sauvegarder une liste d'objets

```pascal
// Sauvegarder une liste de tâches en JSON
type
  TTache = record
    ID: Integer;
    Titre: string;
    Description: string;
    Terminee: Boolean;
    DateCreation: TDateTime;
  end;

procedure TFormMain.SauvegarderListeTaches(Taches: TList<TTache>);
var
  JSONArray: TJSONArray;
  JSONTache: TJSONObject;
  Tache: TTache;
  CheminFichier: string;
begin
  JSONArray := TJSONArray.Create;
  try
    // Convertir chaque tâche en JSON
    for Tache in Taches do
    begin
      JSONTache := TJSONObject.Create;
      JSONTache.AddPair('id', TJSONNumber.Create(Tache.ID));
      JSONTache.AddPair('titre', Tache.Titre);
      JSONTache.AddPair('description', Tache.Description);
      JSONTache.AddPair('terminee', TJSONBool.Create(Tache.Terminee));
      JSONTache.AddPair('dateCreation', DateToISO8601(Tache.DateCreation));

      JSONArray.Add(JSONTache);
    end;

    // Sauvegarder
    CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'taches.json');
    TFile.WriteAllText(CheminFichier, JSONArray.ToString, TEncoding.UTF8);

    ShowMessage(Taches.Count.ToString + ' tâches sauvegardées');
  finally
    JSONArray.Free;
  end;
end;

procedure TFormMain.ChargerListeTaches(Taches: TList<TTache>);
var
  JSONArray: TJSONArray;
  JSONTache: TJSONObject;
  Tache: TTache;
  CheminFichier: string;
  Contenu: string;
  i: Integer;
begin
  CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'taches.json');

  if not TFile.Exists(CheminFichier) then
    Exit;

  Contenu := TFile.ReadAllText(CheminFichier, TEncoding.UTF8);
  JSONArray := TJSONObject.ParseJSONValue(Contenu) as TJSONArray;
  try
    Taches.Clear;

    for i := 0 to JSONArray.Count - 1 do
    begin
      JSONTache := JSONArray.Items[i] as TJSONObject;

      Tache.ID := JSONTache.GetValue<Integer>('id');
      Tache.Titre := JSONTache.GetValue<string>('titre');
      Tache.Description := JSONTache.GetValue<string>('description');
      Tache.Terminee := JSONTache.GetValue<Boolean>('terminee');
      Tache.DateCreation := ISO8601ToDate(JSONTache.GetValue<string>('dateCreation'));

      Taches.Add(Tache);
    end;

    ShowMessage(Taches.Count.ToString + ' tâches chargées');
  finally
    JSONArray.Free;
  end;
end;
```

## Base de données SQLite locale

Pour des données plus complexes nécessitant des requêtes et des relations, SQLite est la solution idéale.

### Configuration de FireDAC avec SQLite

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Phys.SQLite,
  FireDAC.Stan.Async, System.IOUtils;

var
  Connexion: TFDConnection;

// Configurer la connexion SQLite
procedure TFormMain.ConfigurerBDD;
var
  CheminBDD: string;
begin
  // Créer le chemin de la base de données
  CheminBDD := TPath.Combine(TPath.GetDocumentsPath, 'mabase.db');

  Connexion := TFDConnection.Create(Self);

  // Configuration
  Connexion.DriverName := 'SQLite';
  Connexion.Params.Database := CheminBDD;
  Connexion.Params.Add('LockingMode=Normal');

  try
    Connexion.Connected := True;
    ShowMessage('Base de données connectée');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

### Créer les tables

```pascal
// Créer la structure de la base de données
procedure TFormMain.CreerTables;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    // Table des utilisateurs
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Utilisateurs (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      '  nom TEXT NOT NULL, ' +
      '  email TEXT UNIQUE, ' +
      '  date_inscription DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
      '  actif BOOLEAN DEFAULT 1' +
      ')';
    Query.ExecSQL;

    // Table des tâches
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Taches (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      '  utilisateur_id INTEGER, ' +
      '  titre TEXT NOT NULL, ' +
      '  description TEXT, ' +
      '  terminee BOOLEAN DEFAULT 0, ' +
      '  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
      '  date_modification DATETIME, ' +
      '  FOREIGN KEY (utilisateur_id) REFERENCES Utilisateurs(id)' +
      ')';
    Query.ExecSQL;

    // Index pour améliorer les performances
    Query.SQL.Text :=
      'CREATE INDEX IF NOT EXISTS idx_taches_utilisateur ' +
      'ON Taches(utilisateur_id)';
    Query.ExecSQL;

    ShowMessage('Tables créées avec succès');
  finally
    Query.Free;
  end;
end;
```

### Opérations CRUD

```pascal
// Créer (Insérer) une tâche
procedure TFormMain.AjouterTache(Titre, Description: string;
  UtilisateurID: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;
    Query.SQL.Text :=
      'INSERT INTO Taches (utilisateur_id, titre, description) ' +
      'VALUES (:userid, :titre, :desc)';

    Query.ParamByName('userid').AsInteger := UtilisateurID;
    Query.ParamByName('titre').AsString := Titre;
    Query.ParamByName('desc').AsString := Description;

    Query.ExecSQL;

    ShowMessage('Tâche ajoutée avec succès');
  finally
    Query.Free;
  end;
end;

// Lire (Récupérer) les tâches
procedure TFormMain.ChargerTachesUtilisateur(UtilisateurID: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;
    Query.SQL.Text :=
      'SELECT * FROM Taches ' +
      'WHERE utilisateur_id = :userid ' +
      'ORDER BY date_creation DESC';

    Query.ParamByName('userid').AsInteger := UtilisateurID;
    Query.Open;

    ListView1.Items.Clear;

    while not Query.Eof do
    begin
      var Item := ListView1.Items.Add;
      Item.Text := Query.FieldByName('titre').AsString;
      Item.Detail := Query.FieldByName('description').AsString;
      Item.TagString := Query.FieldByName('id').AsString;

      Query.Next;
    end;

    LabelNbTaches.Text := ListView1.Items.Count.ToString + ' tâches';
  finally
    Query.Free;
  end;
end;

// Mettre à jour une tâche
procedure TFormMain.MarquerTacheTerminee(TacheID: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;
    Query.SQL.Text :=
      'UPDATE Taches ' +
      'SET terminee = 1, date_modification = CURRENT_TIMESTAMP ' +
      'WHERE id = :id';

    Query.ParamByName('id').AsInteger := TacheID;
    Query.ExecSQL;

    ShowMessage('Tâche marquée comme terminée');
  finally
    Query.Free;
  end;
end;

// Supprimer une tâche
procedure TFormMain.SupprimerTache(TacheID: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;
    Query.SQL.Text := 'DELETE FROM Taches WHERE id = :id';

    Query.ParamByName('id').AsInteger := TacheID;
    Query.ExecSQL;

    ShowMessage('Tâche supprimée');
  finally
    Query.Free;
  end;
end;
```

### Requêtes complexes

```pascal
// Obtenir des statistiques
procedure TFormMain.AfficherStatistiques(UtilisateurID: Integer);
var
  Query: TFDQuery;
  TotalTaches, TachesTerminees, TachesEnCours: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    // Compter le total de tâches
    Query.SQL.Text :=
      'SELECT COUNT(*) as total FROM Taches WHERE utilisateur_id = :userid';
    Query.ParamByName('userid').AsInteger := UtilisateurID;
    Query.Open;
    TotalTaches := Query.FieldByName('total').AsInteger;
    Query.Close;

    // Compter les tâches terminées
    Query.SQL.Text :=
      'SELECT COUNT(*) as total FROM Taches ' +
      'WHERE utilisateur_id = :userid AND terminee = 1';
    Query.ParamByName('userid').AsInteger := UtilisateurID;
    Query.Open;
    TachesTerminees := Query.FieldByName('total').AsInteger;
    Query.Close;

    TachesEnCours := TotalTaches - TachesTerminees;

    // Afficher les statistiques
    LabelStats.Text := Format('Total: %d | Terminées: %d | En cours: %d',
      [TotalTaches, TachesTerminees, TachesEnCours]);
  finally
    Query.Free;
  end;
end;

// Recherche de tâches
procedure TFormMain.RechercherTaches(TexteRecherche: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;
    Query.SQL.Text :=
      'SELECT * FROM Taches ' +
      'WHERE titre LIKE :recherche OR description LIKE :recherche ' +
      'ORDER BY date_creation DESC';

    Query.ParamByName('recherche').AsString := '%' + TexteRecherche + '%';
    Query.Open;

    ListView1.Items.Clear;

    while not Query.Eof do
    begin
      var Item := ListView1.Items.Add;
      Item.Text := Query.FieldByName('titre').AsString;
      Item.Detail := Query.FieldByName('description').AsString;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;
```

## Synchronisation avec un serveur

La synchronisation permet de garder les données cohérentes entre l'appareil local et un serveur distant.

### Architecture de synchronisation

Il existe plusieurs stratégies de synchronisation :

**Synchronisation complète** : Remplacer toutes les données locales par celles du serveur
**Synchronisation incrémentielle** : Ne synchroniser que les changements depuis la dernière sync
**Synchronisation bidirectionnelle** : Envoyer les modifications locales et recevoir celles du serveur

### Marquage des données pour la synchronisation

```pascal
// Ajouter des champs de synchronisation aux tables
procedure TFormMain.AjouterChampsSynchronisation;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    // Ajouter des colonnes pour la synchronisation
    Query.SQL.Text :=
      'ALTER TABLE Taches ADD COLUMN synced BOOLEAN DEFAULT 0';
    try
      Query.ExecSQL;
    except
      // Colonne existe déjà
    end;

    Query.SQL.Text :=
      'ALTER TABLE Taches ADD COLUMN server_id INTEGER';
    try
      Query.ExecSQL;
    except
      // Colonne existe déjà
    end;

    Query.SQL.Text :=
      'ALTER TABLE Taches ADD COLUMN last_sync DATETIME';
    try
      Query.ExecSQL;
    except
      // Colonne existe déjà
    end;
  finally
    Query.Free;
  end;
end;
```

### Envoyer les modifications locales au serveur

```pascal
uses
  System.Net.HttpClient, System.JSON;

// Synchroniser les tâches non synchronisées vers le serveur
procedure TFormMain.EnvoyerModificationsLocales;
var
  Query: TFDQuery;
  HttpClient: THTTPClient;
  JSONArray: TJSONArray;
  JSONTache: TJSONObject;
  Response: IHTTPResponse;
begin
  Query := TFDQuery.Create(nil);
  HttpClient := THTTPClient.Create;
  try
    Query.Connection := Connexion;

    // Récupérer les tâches non synchronisées
    Query.SQL.Text :=
      'SELECT * FROM Taches WHERE synced = 0';
    Query.Open;

    if Query.RecordCount = 0 then
    begin
      ShowMessage('Aucune modification à synchroniser');
      Exit;
    end;

    // Créer le JSON avec toutes les tâches à synchroniser
    JSONArray := TJSONArray.Create;
    try
      while not Query.Eof do
      begin
        JSONTache := TJSONObject.Create;
        JSONTache.AddPair('local_id', Query.FieldByName('id').AsString);
        JSONTache.AddPair('titre', Query.FieldByName('titre').AsString);
        JSONTache.AddPair('description', Query.FieldByName('description').AsString);
        JSONTache.AddPair('terminee', TJSONBool.Create(
          Query.FieldByName('terminee').AsBoolean));

        JSONArray.Add(JSONTache);
        Query.Next;
      end;

      // Envoyer au serveur
      Response := HttpClient.Post(
        'https://votreserveur.com/api/sync/taches',
        TStringStream.Create(JSONArray.ToString, TEncoding.UTF8),
        nil);

      if Response.StatusCode = 200 then
      begin
        // Marquer comme synchronisé
        MarquerCommeSynchronise(JSONArray);
        ShowMessage('Synchronisation réussie');
      end
      else
        ShowMessage('Erreur de synchronisation : ' + Response.StatusCode.ToString);
    finally
      JSONArray.Free;
    end;
  finally
    Query.Free;
    HttpClient.Free;
  end;
end;

// Marquer les tâches comme synchronisées
procedure TFormMain.MarquerCommeSynchronise(TachesJSON: TJSONArray);
var
  Query: TFDQuery;
  i: Integer;
  LocalID: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    Connexion.StartTransaction;
    try
      for i := 0 to TachesJSON.Count - 1 do
      begin
        LocalID := (TachesJSON.Items[i] as TJSONObject).GetValue<Integer>('local_id');

        Query.SQL.Text :=
          'UPDATE Taches SET synced = 1, last_sync = CURRENT_TIMESTAMP ' +
          'WHERE id = :id';
        Query.ParamByName('id').AsInteger := LocalID;
        Query.ExecSQL;
      end;

      Connexion.Commit;
    except
      Connexion.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;
```

### Recevoir les modifications du serveur

```pascal
// Télécharger les nouvelles données du serveur
procedure TFormMain.RecevoirModificationsServeur;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JSONArray: TJSONArray;
  JSONTache: TJSONObject;
  Query: TFDQuery;
  i: Integer;
begin
  HttpClient := THTTPClient.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    // Obtenir la dernière date de synchronisation
    Query.SQL.Text := 'SELECT MAX(last_sync) as derniere_sync FROM Taches';
    Query.Open;
    var DerniereSync := Query.FieldByName('derniere_sync').AsDateTime;
    Query.Close;

    // Demander les modifications depuis cette date
    var URL := Format('https://votreserveur.com/api/sync/taches?since=%s',
      [DateToISO8601(DerniereSync)]);

    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSONArray := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONArray;
      try
        Connexion.StartTransaction;
        try
          for i := 0 to JSONArray.Count - 1 do
          begin
            JSONTache := JSONArray.Items[i] as TJSONObject;

            // Insérer ou mettre à jour
            InsererOuMettreAJourTache(JSONTache, Query);
          end;

          Connexion.Commit;
          ShowMessage(JSONArray.Count.ToString + ' tâches synchronisées');
        except
          Connexion.Rollback;
          raise;
        end;
      finally
        JSONArray.Free;
      end;
    end;
  finally
    Query.Free;
    HttpClient.Free;
  end;
end;

// Insérer ou mettre à jour une tâche depuis le serveur
procedure TFormMain.InsererOuMettreAJourTache(JSONTache: TJSONObject;
  Query: TFDQuery);
var
  ServerID: Integer;
begin
  ServerID := JSONTache.GetValue<Integer>('id');

  // Vérifier si la tâche existe déjà
  Query.SQL.Text := 'SELECT id FROM Taches WHERE server_id = :serverid';
  Query.ParamByName('serverid').AsInteger := ServerID;
  Query.Open;

  if Query.RecordCount > 0 then
  begin
    // Mise à jour
    Query.Close;
    Query.SQL.Text :=
      'UPDATE Taches SET ' +
      '  titre = :titre, ' +
      '  description = :desc, ' +
      '  terminee = :terminee, ' +
      '  synced = 1, ' +
      '  last_sync = CURRENT_TIMESTAMP ' +
      'WHERE server_id = :serverid';
  end
  else
  begin
    // Insertion
    Query.Close;
    Query.SQL.Text :=
      'INSERT INTO Taches (server_id, titre, description, terminee, ' +
      '  synced, last_sync) ' +
      'VALUES (:serverid, :titre, :desc, :terminee, 1, CURRENT_TIMESTAMP)';
  end;

  Query.ParamByName('serverid').AsInteger := ServerID;
  Query.ParamByName('titre').AsString := JSONTache.GetValue<string>('titre');
  Query.ParamByName('desc').AsString := JSONTache.GetValue<string>('description');
  Query.ParamByName('terminee').AsBoolean := JSONTache.GetValue<Boolean>('terminee');

  Query.ExecSQL;
end;
```

### Gestion des conflits

Lorsque les données sont modifiées à la fois localement et sur le serveur, des conflits peuvent survenir.

```pascal
type
  TStrategieConflit = (scServeurGagne, scClientGagne, scPlusRecent, scDemanderUtilisateur);

// Résoudre un conflit de synchronisation
procedure TFormMain.ResoudreConflit(TacheLocale, TacheServeur: TJSONObject;
  Strategie: TStrategieConflit);
begin
  case Strategie of
    scServeurGagne:
      // Toujours prendre la version du serveur
      AppliquerVersionServeur(TacheServeur);

    scClientGagne:
      // Toujours garder la version locale et l'envoyer au serveur
      EnvoyerVersionLocale(TacheLocale);

    scPlusRecent:
      // Comparer les dates de modification
      begin
        var DateLocale := ISO8601ToDate(TacheLocale.GetValue<string>('date_modification'));
        var DateServeur := ISO8601ToDate(TacheServeur.GetValue<string>('date_modification'));

        if DateServeur > DateLocale then
          AppliquerVersionServeur(TacheServeur)
        else
          EnvoyerVersionLocale(TacheLocale);
      end;

    scDemanderUtilisateur:
      // Afficher un dialogue pour que l'utilisateur choisisse
      DemanderResolutionUtilisateur(TacheLocale, TacheServeur);
  end;
end;

// Demander à l'utilisateur de résoudre le conflit
procedure TFormMain.DemanderResolutionUtilisateur(TacheLocale,
  TacheServeur: TJSONObject);
begin
  var Message := 'Conflit détecté sur la tâche "' +
    TacheLocale.GetValue<string>('titre') + '"' + sLineBreak + sLineBreak +
    'Version locale: ' + TacheLocale.GetValue<string>('description') + sLineBreak +
    'Version serveur: ' + TacheServeur.GetValue<string>('description') + sLineBreak + sLineBreak +
    'Quelle version conserver ?';

  TDialogService.MessageDialog(Message, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
        EnvoyerVersionLocale(TacheLocale)
      else
        AppliquerVersionServeur(TacheServeur);
    end,
    'Garder local', 'Garder serveur');
end;
```

## Synchronisation automatique

```pascal
// Synchroniser automatiquement en arrière-plan
procedure TFormMain.ConfigurerSyncAuto;
begin
  // Timer pour synchroniser périodiquement
  TimerSync := TTimer.Create(Self);
  TimerSync.Interval := 300000; // 5 minutes
  TimerSync.OnTimer := TimerSyncTimer;
  TimerSync.Enabled := True;
end;

procedure TFormMain.TimerSyncTimer(Sender: TObject);
begin
  // Ne synchroniser que si connecté
  if EstConnecteInternet then
  begin
    SynchroniserDonnees;
  end;
end;

// Vérifier la connexion Internet
function TFormMain.EstConnecteInternet: Boolean;
var
  HttpClient: THTTPClient;
begin
  HttpClient := THTTPClient.Create;
  try
    try
      var Response := HttpClient.Get('https://www.google.com');
      Result := Response.StatusCode = 200;
    except
      Result := False;
    end;
  finally
    HttpClient.Free;
  end;
end;

// Processus complet de synchronisation
procedure TFormMain.SynchroniserDonnees;
begin
  if FSyncEnCours then
  begin
    ShowMessage('Synchronisation déjà en cours');
    Exit;
  end;

  FSyncEnCours := True;
  AniIndicator1.Enabled := True;
  AniIndicator1.Visible := True;

  TTask.Run(
    procedure
    begin
      try
        // 1. Envoyer les modifications locales
        EnvoyerModificationsLocales;

        // 2. Recevoir les modifications du serveur
        RecevoirModificationsServeur;

        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicator1.Enabled := False;
            AniIndicator1.Visible := False;
            LabelDerniereSync.Text := 'Dernière sync : ' + TimeToStr(Now);
            FSyncEnCours := False;
          end);
      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur de synchronisation : ' + E.Message);
              AniIndicator1.Enabled := False;
              AniIndicator1.Visible := False;
              FSyncEnCours := False;
            end);
        end;
      end;
    end);
end;
```

## Optimisation et bonnes pratiques

### Compression des données

```pascal
uses
  System.ZLib;

// Compresser les données avant envoi
function CompresserJSON(JSON: string): TBytes;
var
  Input, Output: TBytes;
begin
  Input := TEncoding.UTF8.GetBytes(JSON);

  // Compresser avec ZLib
  ZCompress(Input, Output);

  Result := Output;
end;

// Décompresser les données reçues
function DecompresserJSON(Data: TBytes): string;
var
  Output: TBytes;
begin
  ZDecompress(Data, Output);
  Result := TEncoding.UTF8.GetString(Output);
end;
```

### Cache intelligent

```pascal
// Mettre en cache les données fréquemment consultées
type
  TCacheManager = class
  private
    FCache: TDictionary<string, TJSONObject>;
    FDureeCacheSecondes: Integer;
  public
    constructor Create(DureeCacheSecondes: Integer = 300);
    destructor Destroy; override;

    procedure Ajouter(Cle: string; Valeur: TJSONObject);
    function Obtenir(Cle: string): TJSONObject;
    function Existe(Cle: string): Boolean;
    procedure Vider;
  end;

constructor TCacheManager.Create(DureeCacheSecondes: Integer);
begin
  FCache := TDictionary<string, TJSONObject>.Create;
  FDureeCacheSecondes := DureeCacheSecondes;
end;

destructor TCacheManager.Destroy;
begin
  Vider;
  FCache.Free;
  inherited;
end;

procedure TCacheManager.Ajouter(Cle: string; Valeur: TJSONObject);
begin
  if FCache.ContainsKey(Cle) then
    FCache[Cle].Free;

  FCache.AddOrSetValue(Cle, Valeur.Clone as TJSONObject);
end;

function TCacheManager.Obtenir(Cle: string): TJSONObject;
begin
  if FCache.ContainsKey(Cle) then
    Result := FCache[Cle]
  else
    Result := nil;
end;
```

### Nettoyage des anciennes données

```pascal
// Supprimer les anciennes données synchronisées
procedure TFormMain.NettoyerAnciennesDonnees;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connexion;

    // Supprimer les tâches terminées depuis plus de 30 jours
    Query.SQL.Text :=
      'DELETE FROM Taches ' +
      'WHERE terminee = 1 AND synced = 1 ' +
      'AND date_modification < datetime("now", "-30 days")';

    Query.ExecSQL;

    ShowMessage('Anciennes données nettoyées');
  finally
    Query.Free;
  end;
end;
```

## Conclusion

Le stockage local et la synchronisation sont des éléments essentiels pour créer des applications mobiles robustes qui fonctionnent dans toutes les conditions.

Les points clés à retenir :

1. **Préférences** : Utilisez TIniFile pour les paramètres simples de l'application
2. **JSON** : Format idéal pour les données structurées légères
3. **SQLite** : Base de données puissante pour les données complexes et volumineuses
4. **Synchronisation** : Permettez à vos utilisateurs d'accéder à leurs données partout
5. **Hors ligne d'abord** : Concevez votre application pour fonctionner sans connexion
6. **Conflits** : Gérez intelligemment les conflits de synchronisation
7. **Performance** : Utilisez le cache et optimisez les requêtes
8. **Sécurité** : Chiffrez les données sensibles stockées localement

Une stratégie de stockage et de synchronisation bien pensée assure une expérience utilisateur fluide, que l'utilisateur soit en ligne ou hors ligne, sur un seul appareil ou sur plusieurs. Dans la section suivante, nous verrons comment préparer et publier vos applications sur les stores officiels.

⏭️ [Publication sur App Store / Play Store](/15-applications-mobiles-avec-delphi/07-publication-sur-app-store-play-store.md)
