# 8.5 Manipulation des données

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Maintenant que nous avons établi une connexion à notre base de données MySQL/MariaDB, nous allons explorer comment manipuler les données : récupérer des informations, ajouter de nouveaux enregistrements, modifier des données existantes et supprimer des enregistrements. Ces opérations, souvent désignées par l'acronyme CRUD (Create, Read, Update, Delete), constituent la base de toute application de gestion de données.

Dans cette section, nous allons présenter les principes fondamentaux de la manipulation des données avec FireDAC. Les sections suivantes approfondiront chaque aspect avec des exemples plus détaillés.

## Vue d'ensemble de la manipulation des données

La manipulation des données avec FireDAC dans Delphi suit généralement ce flux :

1. **Créer une requête SQL** ou utiliser une table directement
2. **Configurer les paramètres** si la requête en contient
3. **Exécuter la requête** pour récupérer, ajouter, modifier ou supprimer des données
4. **Parcourir les résultats** si la requête retourne des données
5. **Gérer les exceptions** en cas d'erreur

FireDAC propose principalement deux composants pour manipuler les données :

- `TFDQuery` : pour exécuter des requêtes SQL personnalisées
- `TFDTable` : pour manipuler directement une table entière

Pour les débutants, nous recommandons de commencer avec `TFDQuery` car il offre plus de flexibilité et vous permet d'apprendre les bases du SQL.

## Les bases des requêtes SQL

Avant de plonger dans le code Delphi, rappelons les quatre opérations fondamentales en SQL :

1. **SELECT** : Récupérer des données
   ```sql
   SELECT * FROM clients WHERE ville = 'Paris'
   ```

2. **INSERT** : Ajouter des données
   ```sql
   INSERT INTO clients (nom, prenom, email) VALUES ('Dupont', 'Jean', 'jean.dupont@exemple.com')
   ```

3. **UPDATE** : Modifier des données
   ```sql
   UPDATE clients SET telephone = '0102030405' WHERE id = 42
   ```

4. **DELETE** : Supprimer des données
   ```sql
   DELETE FROM clients WHERE id = 42
   ```

## Configuration du projet pour les exemples

Pour illustrer la manipulation des données, créons un formulaire simple qui nous permettra d'interagir avec une table `clients`. Si vous n'avez pas encore de table `clients`, voici le script SQL pour en créer une :

```sql
CREATE TABLE clients (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(100) NOT NULL,
  prenom VARCHAR(100),
  email VARCHAR(100) NOT NULL,
  telephone VARCHAR(20),
  adresse TEXT,
  ville VARCHAR(100),
  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

Maintenant, créons un nouveau projet VCL et ajoutons les composants suivants :

1. Un `TFDConnection` pour la connexion à la base de données
2. Un `TFDQuery` pour exécuter nos requêtes
3. Un `TDataSource` pour lier les données aux contrôles visuels
4. Un `TDBGrid` pour afficher les résultats sous forme de tableau
5. Des boutons pour effectuer les opérations CRUD

Voici à quoi pourrait ressembler notre formulaire :

![Exemple de formulaire pour manipuler des données](https://placeholder.pics/svg/800x500/DEDEDE/555555/Formulaire%20manipulation%20données)

## Connexion et initialisation

Dans l'événement `FormCreate`, nous allons établir la connexion à la base de données et configurer notre requête initiale :

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurer la connexion
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=mon_utilisateur');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');

  try
    // Ouvrir la connexion
    FDConnection1.Connected := True;

    // Configurer la requête initiale
    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
    FDQuery1.Open;

    // Lier la source de données
    DataSource1.DataSet := FDQuery1;

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

## Premier aperçu des opérations CRUD

### Lire des données (READ)

La lecture de données est déjà initiée dans notre `FormCreate` avec `FDQuery1.Open`. Cette méthode exécute la requête SQL et charge les données pour les afficher dans notre `TDBGrid`.

Pour rafraîchir les données à tout moment, nous pouvons ajouter un bouton "Actualiser" :

```delphi
procedure TForm1.ButtonRafraichirClick(Sender: TObject);
begin
  FDQuery1.Close;  // Ferme le dataset
  FDQuery1.Open;   // Réexécute la requête et charge les données fraîches
  // ou simplement :
  // FDQuery1.Refresh;
end;
```

Pour filtrer les données, nous pouvons modifier la requête SQL :

```delphi
procedure TForm1.ButtonRechercherClick(Sender: TObject);
var
  Recherche: string;
begin
  Recherche := EditRecherche.Text;

  if Recherche <> '' then
  begin
    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE nom LIKE :recherche OR prenom LIKE :recherche';
    FDQuery1.ParamByName('recherche').AsString := '%' + Recherche + '%';
    FDQuery1.Open;
  end
  else
  begin
    // Si le champ de recherche est vide, afficher tous les clients
    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
    FDQuery1.Open;
  end;
end;
```

### Créer des données (CREATE)

Pour ajouter un nouveau client, nous allons utiliser un formulaire de saisie. Créons d'abord un nouveau formulaire (`TFormClient`) avec des champs pour saisir les informations du client.

Dans le formulaire principal, ajoutons un bouton "Nouveau client" :

```delphi
procedure TForm1.ButtonNouveauClick(Sender: TObject);
var
  FormClient: TFormClient;
begin
  FormClient := TFormClient.Create(Self);
  try
    FormClient.Caption := 'Nouveau client';
    FormClient.FDConnection := FDConnection1;  // Passer la connexion au formulaire

    if FormClient.ShowModal = mrOk then
    begin
      // Le client a été ajouté, rafraîchir la liste
      FDQuery1.Refresh;
    end;
  finally
    FormClient.Free;
  end;
end;
```

Dans le formulaire client, nous ajouterons le code pour insérer un nouveau client :

```delphi
// Dans le formulaire TFormClient
procedure TFormClient.ButtonEnregistrerClick(Sender: TObject);
var
  Query: TFDQuery;
begin
  // Validation basique
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    Exit;
  end;

  if Trim(EditEmail.Text) = '' then
  begin
    ShowMessage('L''email est obligatoire');
    EditEmail.SetFocus;
    Exit;
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;

    // Préparer la requête d'insertion
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email, telephone, adresse, ville) ' +
      'VALUES (:nom, :prenom, :email, :telephone, :adresse, :ville)';

    // Définir les paramètres
    Query.ParamByName('nom').AsString := Trim(EditNom.Text);
    Query.ParamByName('prenom').AsString := Trim(EditPrenom.Text);
    Query.ParamByName('email').AsString := Trim(EditEmail.Text);
    Query.ParamByName('telephone').AsString := Trim(EditTelephone.Text);
    Query.ParamByName('adresse').AsString := Trim(MemoAdresse.Text);
    Query.ParamByName('ville').AsString := Trim(EditVille.Text);

    // Exécuter la requête
    Query.ExecSQL;

    ShowMessage('Client ajouté avec succès !');
    ModalResult := mrOk;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ajout du client : ' + E.Message);
  finally
    Query.Free;
  end;
end;
```

### Mettre à jour des données (UPDATE)

Pour modifier un client existant, nous réutiliserons le même formulaire, mais cette fois en le pré-remplissant avec les données du client sélectionné :

```delphi
procedure TForm1.ButtonModifierClick(Sender: TObject);
var
  FormClient: TFormClient;
  ClientID: Integer;
begin
  // Vérifier qu'un client est sélectionné
  if FDQuery1.IsEmpty then
  begin
    ShowMessage('Veuillez sélectionner un client à modifier');
    Exit;
  end;

  // Récupérer l'ID du client sélectionné
  ClientID := FDQuery1.FieldByName('id').AsInteger;

  FormClient := TFormClient.Create(Self);
  try
    FormClient.Caption := 'Modifier client';
    FormClient.FDConnection := FDConnection1;
    FormClient.ClientID := ClientID;  // Passer l'ID du client à modifier

    // Pré-remplir les champs avec les données du client
    FormClient.EditNom.Text := FDQuery1.FieldByName('nom').AsString;
    FormClient.EditPrenom.Text := FDQuery1.FieldByName('prenom').AsString;
    FormClient.EditEmail.Text := FDQuery1.FieldByName('email').AsString;
    FormClient.EditTelephone.Text := FDQuery1.FieldByName('telephone').AsString;
    FormClient.MemoAdresse.Text := FDQuery1.FieldByName('adresse').AsString;
    FormClient.EditVille.Text := FDQuery1.FieldByName('ville').AsString;

    if FormClient.ShowModal = mrOk then
    begin
      // Le client a été modifié, rafraîchir la liste
      FDQuery1.Refresh;
    end;
  finally
    FormClient.Free;
  end;
end;
```

Dans le formulaire client, nous ajouterons le code pour la mise à jour :

```delphi
// Dans le formulaire TFormClient
// Ajout d'une propriété ClientID
private
  FClientID: Integer;
public
  property ClientID: Integer read FClientID write FClientID;

// Modification de ButtonEnregistrerClick pour gérer aussi les mises à jour
procedure TFormClient.ButtonEnregistrerClick(Sender: TObject);
var
  Query: TFDQuery;
begin
  // Validation basique (comme précédemment)...

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;

    if FClientID > 0 then
    begin
      // Mise à jour d'un client existant
      Query.SQL.Text :=
        'UPDATE clients SET ' +
        'nom = :nom, prenom = :prenom, email = :email, ' +
        'telephone = :telephone, adresse = :adresse, ville = :ville ' +
        'WHERE id = :id';

      Query.ParamByName('id').AsInteger := FClientID;
    end
    else
    begin
      // Insertion d'un nouveau client
      Query.SQL.Text :=
        'INSERT INTO clients (nom, prenom, email, telephone, adresse, ville) ' +
        'VALUES (:nom, :prenom, :email, :telephone, :adresse, :ville)';
    end;

    // Définir les paramètres communs
    Query.ParamByName('nom').AsString := Trim(EditNom.Text);
    Query.ParamByName('prenom').AsString := Trim(EditPrenom.Text);
    Query.ParamByName('email').AsString := Trim(EditEmail.Text);
    Query.ParamByName('telephone').AsString := Trim(EditTelephone.Text);
    Query.ParamByName('adresse').AsString := Trim(MemoAdresse.Text);
    Query.ParamByName('ville').AsString := Trim(EditVille.Text);

    // Exécuter la requête
    Query.ExecSQL;

    if FClientID > 0 then
      ShowMessage('Client modifié avec succès !')
    else
      ShowMessage('Client ajouté avec succès !');

    ModalResult := mrOk;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  finally
    Query.Free;
  end;
end;
```

### Supprimer des données (DELETE)

Pour supprimer un client, nous ajouterons un bouton "Supprimer" :

```delphi
procedure TForm1.ButtonSupprimerClick(Sender: TObject);
var
  Query: TFDQuery;
  ClientID: Integer;
begin
  // Vérifier qu'un client est sélectionné
  if FDQuery1.IsEmpty then
  begin
    ShowMessage('Veuillez sélectionner un client à supprimer');
    Exit;
  end;

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce client ?',
     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Récupérer l'ID du client sélectionné
  ClientID := FDQuery1.FieldByName('id').AsInteger;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Préparer la requête de suppression
    Query.SQL.Text := 'DELETE FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ClientID;

    // Exécuter la requête
    Query.ExecSQL;

    ShowMessage('Client supprimé avec succès !');

    // Rafraîchir la liste
    FDQuery1.Refresh;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la suppression : ' + E.Message);
  finally
    Query.Free;
  end;
end;
```

## Navigation dans les enregistrements

FireDAC fournit des méthodes simples pour naviguer dans les enregistrements :

```delphi
// Aller au premier enregistrement
procedure TForm1.ButtonPremierClick(Sender: TObject);
begin
  FDQuery1.First;
end;

// Aller à l'enregistrement précédent
procedure TForm1.ButtonPrecedentClick(Sender: TObject);
begin
  FDQuery1.Prior;
end;

// Aller à l'enregistrement suivant
procedure TForm1.ButtonSuivantClick(Sender: TObject);
begin
  FDQuery1.Next;
end;

// Aller au dernier enregistrement
procedure TForm1.ButtonDernierClick(Sender: TObject);
begin
  FDQuery1.Last;
end;
```

## Gestion des erreurs

La gestion des erreurs est cruciale lors de la manipulation des données. Voici quelques bonnes pratiques :

1. **Toujours utiliser un bloc try-except** autour des opérations de base de données
2. **Afficher des messages d'erreur clairs** pour aider les utilisateurs
3. **Journaliser les erreurs graves** pour le débogage ultérieur

```delphi
procedure TForm1.ExecuterRequeteSecurisee(SQL: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := SQL;

    try
      Query.ExecSQL;
      ShowMessage('Opération réussie !');
    except
      on E: Exception do
      begin
        ShowMessage('Erreur : ' + E.Message);

        // Journaliser l'erreur (dans un vrai projet)
        // LogError('Erreur SQL', E.Message, SQL);
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

## Résumé

Dans cette section, nous avons vu les bases de la manipulation des données avec FireDAC :

- **Lecture de données** avec les méthodes `Open` et `Refresh`
- **Création de données** en utilisant des requêtes d'insertion paramétrées
- **Mise à jour de données** avec des requêtes de modification
- **Suppression de données** après confirmation de l'utilisateur
- **Navigation** dans les enregistrements
- **Gestion des erreurs** avec try-except

Ces concepts fondamentaux sont la base de toute application de gestion de données. Dans les sections suivantes, nous approfondirons chaque aspect avec plus de détails et d'exemples pratiques.

---

Dans les prochaines sections, nous explorerons plus en détail :
- Les requêtes SQL et paramétrées
- Les opérations CRUD avancées
- Les transactions et l'intégrité des données

⏭️ [Requêtes SQL et paramétrées](/08-acces-aux-bases-de-donnees-mysql-mariadb/05.1-requetes-sql-parametrees.md)
