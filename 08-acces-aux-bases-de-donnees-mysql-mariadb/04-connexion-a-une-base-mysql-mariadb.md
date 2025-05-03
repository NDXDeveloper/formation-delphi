# 8.4 Connexion à une base MySQL/MariaDB

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans cette section, nous allons apprendre à établir une connexion entre votre application Delphi et une base de données MySQL/MariaDB. Nous verrons différentes approches, de la plus simple à la plus avancée, et nous examinerons les options qui vous permettront d'optimiser cette connexion.

## Préparation

Avant de commencer, assurez-vous que :
- MySQL/MariaDB est correctement installé (voir section 8.2)
- Les bibliothèques clientes sont disponibles
- Vous avez créé une base de données et un utilisateur avec les privilèges appropriés

## Connexion simple avec l'Éditeur de Connexion FireDAC

Delphi offre un éditeur visuel qui facilite la configuration de vos connexions. C'est la méthode recommandée pour les débutants.

### Étape 1 : Ajouter un composant TFDConnection

1. Créez un nouveau projet VCL ou ouvrez un projet existant
2. Dans la palette de composants, cliquez sur l'onglet "FireDAC"
3. Déposez un composant `TFDConnection` sur votre formulaire

![Ajout du composant TFDConnection](https://placeholder.pics/svg/400x200/DEDEDE/555555/Ajout%20TFDConnection)

### Étape 2 : Configurer la connexion avec l'éditeur visuel

1. Sélectionnez le composant `TFDConnection` sur votre formulaire
2. Dans l'Object Inspector (Inspecteur d'Objets), localisez la propriété `ConnectionDefName`
3. Cliquez sur les points de suspension [...] à droite de cette propriété

![Ouverture de l'éditeur de connexion](https://placeholder.pics/svg/400x150/DEDEDE/555555/Ouverture%20Editeur%20Connexion)

4. Dans la fenêtre qui apparaît, cliquez sur le bouton "+" pour ajouter une nouvelle définition
5. Donnez un nom significatif à votre connexion (ex: "MySQL_MaBase")
6. Sélectionnez "MySQL" dans la liste déroulante "Driver ID"

![Configuration de la connexion](https://placeholder.pics/svg/500x350/DEDEDE/555555/Configuration%20Connexion%20MySQL)

7. Dans l'onglet "Connection", renseignez les paramètres suivants :
   - **Server** : l'adresse du serveur (souvent "localhost")
   - **Database** : le nom de votre base de données
   - **User_Name** : votre nom d'utilisateur MySQL
   - **Password** : votre mot de passe
   - **CharacterSet** : "utf8mb4" (recommandé pour le support Unicode complet)

8. Cliquez sur "Test" pour vérifier que la connexion fonctionne
9. Si le test réussit, cliquez sur "OK" pour sauvegarder cette configuration

10. De retour sur le formulaire, définissez la propriété `Connected` à `True` pour établir la connexion au démarrage de l'application

### Étape 3 : Gestion des erreurs de connexion

Il est important de gérer les erreurs qui pourraient survenir lors de la connexion. Ajoutez ce code à votre formulaire :

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    // Tentative de connexion
    FDConnection1.Connected := True;
    StatusBar1.SimpleText := 'Connecté à la base de données';
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;
```

## Connexion par code

Si vous préférez configurer la connexion entièrement par code (par exemple pour changer dynamiquement de base de données), voici comment procéder :

```delphi
procedure TForm1.ConnecterBaseDeDonnees;
begin
  // S'assurer que la connexion est fermée
  FDConnection1.Connected := False;

  // Configurer la connexion
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=mon_utilisateur');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');
  FDConnection1.Params.Add('CharacterSet=utf8mb4');

  try
    // Ouvrir la connexion
    FDConnection1.Connected := True;
    StatusBar1.SimpleText := 'Connecté à ' + FDConnection1.Params.Values['Database'];
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;
```

## Stocker les paramètres de connexion de façon sécurisée

Pour une application en production, il est essentiel de ne pas coder en dur les identifiants de connexion dans votre application. Voici quelques approches :

### 1. Utiliser un fichier de configuration

Créez un fichier de configuration (par exemple, `config.ini`) :

```ini
[Database]
Server=localhost
Database=ma_base
User=mon_utilisateur
Password=mon_mot_de_passe
```

Puis lisez ce fichier dans votre application :

```delphi
procedure TForm1.ChargerConfigurationBD;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := ExtractFilePath(Application.ExeName) + 'config.ini';

  if FileExists(ConfigPath) then
  begin
    IniFile := TIniFile.Create(ConfigPath);
    try
      FDConnection1.Params.Clear;
      FDConnection1.DriverName := 'MySQL';
      FDConnection1.Params.Add('Server=' + IniFile.ReadString('Database', 'Server', 'localhost'));
      FDConnection1.Params.Add('Database=' + IniFile.ReadString('Database', 'Database', ''));
      FDConnection1.Params.Add('User_Name=' + IniFile.ReadString('Database', 'User', ''));
      FDConnection1.Params.Add('Password=' + IniFile.ReadString('Database', 'Password', ''));
      FDConnection1.Params.Add('CharacterSet=utf8mb4');
    finally
      IniFile.Free;
    end;
  end
  else
    ShowMessage('Fichier de configuration non trouvé : ' + ConfigPath);
end;
```

**Note de sécurité** : Ce fichier de configuration doit être stocké de manière sécurisée et non distribué avec l'application.

### 2. Demander les identifiants à l'utilisateur

Pour une meilleure sécurité, vous pouvez demander les identifiants à l'utilisateur lors du démarrage de l'application :

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.LoginPrompt := True;  // Active la demande d'identifiants
end;
```

Avec `LoginPrompt := True`, FireDAC affichera automatiquement une boîte de dialogue demandant le nom d'utilisateur et le mot de passe.

## Options avancées de connexion

FireDAC offre de nombreuses options pour personnaliser votre connexion MySQL/MariaDB. Voici les plus utiles :

### Timeout de connexion

```delphi
FDConnection1.Params.Add('ConnectionTimeout=30');  // En secondes
```

### Réessayer automatiquement en cas d'échec

```delphi
FDConnection1.ConnectionOptions.ConnectRetryCount := 3;        // Nombre de tentatives
FDConnection1.ConnectionOptions.ConnectRetryInterval := 2000;  // Intervalle en ms
```

### Compression des données

Pour les connexions à distance avec beaucoup de données :

```delphi
FDConnection1.Params.Add('Compress=True');
```

### SSL/TLS pour connexion sécurisée

```delphi
FDConnection1.Params.Add('SSL=True');
FDConnection1.Params.Add('SSLKey=/chemin/vers/client-key.pem');
FDConnection1.Params.Add('SSLCert=/chemin/vers/client-cert.pem');
FDConnection1.Params.Add('SSLCACert=/chemin/vers/ca-cert.pem');
```

### Optimisation du fetch des résultats

```delphi
// Récupérer 100 enregistrements à la fois pour de meilleures performances
FDConnection1.FetchOptions.RowsetSize := 100;

// Mode d'accès aux résultats
FDConnection1.ResourceOptions.DirectExecute := False;
FDConnection1.ResourceOptions.ServerOutput := False;
```

## Connexion à plusieurs bases de données

Vous pouvez avoir plusieurs connexions dans une même application :

```delphi
// Première connexion - Base de données principale
FDConnection1.DriverName := 'MySQL';
FDConnection1.Params.Clear;
FDConnection1.Params.Add('Server=localhost');
FDConnection1.Params.Add('Database=base_principale');
FDConnection1.Params.Add('User_Name=utilisateur1');
FDConnection1.Params.Add('Password=motdepasse1');

// Deuxième connexion - Base de données d'archives
FDConnection2.DriverName := 'MySQL';
FDConnection2.Params.Clear;
FDConnection2.Params.Add('Server=serveur_archives.mondomaine.com');
FDConnection2.Params.Add('Database=base_archives');
FDConnection2.Params.Add('User_Name=utilisateur2');
FDConnection2.Params.Add('Password=motdepasse2');
```

Chaque connexion est indépendante et peut être utilisée avec ses propres composants `TFDQuery`, `TFDTable`, etc.

## Exemple complet : Formulaire de connexion personnalisé

Voici un exemple plus élaboré qui crée un formulaire de connexion personnalisé :

```delphi
unit UnitConnexion;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, FireDAC.Comp.Client;

type
  TFormConnexion = class(TForm)
    EditServeur: TEdit;
    EditBaseDeDonnees: TEdit;
    EditUtilisateur: TEdit;
    EditMotDePasse: TEdit;
    ButtonConnecter: TButton;
    ButtonAnnuler: TButton;
    LabelServeur: TLabel;
    LabelBaseDeDonnees: TLabel;
    LabelUtilisateur: TLabel;
    LabelMotDePasse: TLabel;
    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FConnection: TFDConnection;
  public
    constructor Create(AOwner: TComponent; AConnection: TFDConnection); reintroduce;
  end;

implementation

{$R *.dfm}

constructor TFormConnexion.Create(AOwner: TComponent; AConnection: TFDConnection);
begin
  inherited Create(AOwner);
  FConnection := AConnection;
end;

procedure TFormConnexion.FormCreate(Sender: TObject);
begin
  // Remplir avec les valeurs actuelles si la connexion est déjà configurée
  if FConnection.Params.Count > 0 then
  begin
    EditServeur.Text := FConnection.Params.Values['Server'];
    EditBaseDeDonnees.Text := FConnection.Params.Values['Database'];
    EditUtilisateur.Text := FConnection.Params.Values['User_Name'];
    EditMotDePasse.Text := FConnection.Params.Values['Password'];
  end
  else
  begin
    // Valeurs par défaut
    EditServeur.Text := 'localhost';
    EditBaseDeDonnees.Text := 'ma_base';
    EditUtilisateur.Text := 'utilisateur';
    EditMotDePasse.Text := '';
  end;
end;

procedure TFormConnexion.ButtonConnecterClick(Sender: TObject);
begin
  // Fermer la connexion existante
  if FConnection.Connected then
    FConnection.Connected := False;

  // Configurer la connexion
  FConnection.DriverName := 'MySQL';
  FConnection.Params.Clear;
  FConnection.Params.Add('Server=' + EditServeur.Text);
  FConnection.Params.Add('Database=' + EditBaseDeDonnees.Text);
  FConnection.Params.Add('User_Name=' + EditUtilisateur.Text);
  FConnection.Params.Add('Password=' + EditMotDePasse.Text);
  FConnection.Params.Add('CharacterSet=utf8mb4');

  try
    // Tester la connexion
    FConnection.Connected := True;
    ShowMessage('Connexion réussie à la base de données ' + EditBaseDeDonnees.Text);
    ModalResult := mrOk;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      ModalResult := mrNone;  // Rester sur le formulaire
    end;
  end;
end;

procedure TFormConnexion.ButtonAnnulerClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
```

Et voici comment utiliser ce formulaire :

```delphi
procedure TFormPrincipal.ActionConnexionExecute(Sender: TObject);
var
  FormConnexion: TFormConnexion;
begin
  FormConnexion := TFormConnexion.Create(Self, FDConnection1);
  try
    if FormConnexion.ShowModal = mrOk then
    begin
      // La connexion a été configurée et testée dans le formulaire
      // Mettre à jour l'interface utilisateur en conséquence
      ActualiserInterface;
    end;
  finally
    FormConnexion.Free;
  end;
end;
```

## Vérifier l'état de la connexion

Il est important de vérifier l'état de la connexion avant d'exécuter des opérations sur la base de données :

```delphi
procedure TForm1.ExecuterRequete;
begin
  if not FDConnection1.Connected then
  begin
    try
      FDConnection1.Connected := True;
    except
      on E: Exception do
      begin
        ShowMessage('Impossible de se connecter à la base de données : ' + E.Message);
        Exit;  // Sortir de la procédure si la connexion échoue
      end;
    end;
  end;

  // Maintenant que la connexion est établie, exécuter la requête
  FDQuery1.Open;
end;
```

## Déconnexion propre

N'oubliez pas de fermer proprement la connexion lorsque vous n'en avez plus besoin :

```delphi
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Fermer toutes les requêtes actives
  if FDQuery1.Active then
    FDQuery1.Close;

  // Fermer la connexion
  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;
```

## Diagnostiquer les problèmes de connexion

Si vous rencontrez des problèmes de connexion, FireDAC propose un système de trace :

```delphi
procedure TForm1.ActiverTrace;
begin
  // Créer et configurer le composant de trace
  FDMonitor := TFDMonitor.Create(nil);
  FDMonitor.Tracing := True;
  FDMonitor.Enabled := True;

  // Configurer les options de trace
  FDConnection1.Params.Add('MonitorBy=Remote');  // Activer le monitoring
  FDConnection1.FetchOptions.RecsMax := -1;       // Tracer tous les enregistrements

  // Spécifier le fichier de trace
  FDMonitor.FileName := 'C:\Temp\FireDAC_Trace.txt';
  FDMonitor.OutputOptions := [moFileName];

  ShowMessage('Trace activée : ' + FDMonitor.FileName);
end;
```

## Conclusion

La connexion à une base de données MySQL/MariaDB avec FireDAC est relativement simple mais offre de nombreuses options de personnalisation. Vous pouvez choisir entre une configuration visuelle via l'éditeur de connexion ou une configuration par code selon vos besoins.

Dans tous les cas, n'oubliez pas de :
- Gérer correctement les erreurs de connexion
- Sécuriser les identifiants de connexion
- Fermer proprement les connexions lorsqu'elles ne sont plus nécessaires

Dans la prochaine section, nous verrons comment manipuler les données une fois la connexion établie.

---

**À suivre :** 8.5 Manipulation des données

⏭️ [Manipulation des données](/08-acces-aux-bases-de-donnees-mysql-mariadb/05-manipulation-des-donnees.md)
