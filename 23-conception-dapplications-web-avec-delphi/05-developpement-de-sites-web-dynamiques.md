# 23.5 Développement de sites Web dynamiques

## Introduction aux sites Web dynamiques avec Delphi

Jusqu'à présent, nous avons exploré comment créer des services REST et des applications Web basées sur VCL avec Delphi. Dans cette section, nous allons voir comment développer des sites Web véritablement dynamiques - des sites qui génèrent du contenu HTML à la volée en fonction des actions de l'utilisateur, des données de base de données, et d'autres facteurs.

Les sites Web dynamiques se distinguent des sites statiques par leur capacité à produire un contenu personnalisé pour chaque visiteur et à interagir avec des bases de données. Avec Delphi, vous pouvez créer ces sites en utilisant différentes approches que nous allons explorer.

## Les différentes approches pour créer des sites Web dynamiques avec Delphi

Delphi offre plusieurs façons de développer des sites Web dynamiques :

1. **WebBroker** : L'approche traditionnelle, idéale pour générer du HTML dynamique
2. **IntraWeb** : Une approche plus visuelle, similaire au développement VCL
3. **TMS Web Core** : Développement côté client avec du code Pascal transpilé en JavaScript
4. **Uniserver/UniGUI** : Une solution tierce populaire avec une approche similaire à VCL
5. **Approche hybride** : Combinaison d'un backend Delphi avec un frontend utilisant des frameworks web modernes

Pour ce tutoriel, nous allons principalement nous concentrer sur WebBroker pour comprendre les fondamentaux, puis explorer brièvement les autres options.

## Création d'un site Web dynamique avec WebBroker

### Étape 1 : Créer un nouveau projet WebBroker

1. Lancez Delphi et sélectionnez **Fichier** > **Nouveau** > **Autres**
2. Naviguez vers **Delphi Projects** > **Web Server Application**
3. Choisissez **WebBroker Application** et **Stand Alone Application** comme type de serveur
4. Cliquez sur **OK** pour créer le projet

### Étape 2 : Comprendre la structure du projet

Ouvrez le fichier `WebModuleUnit1.pas`. Vous y trouverez un module Web qui contient déjà un gestionnaire d'action par défaut. C'est ici que nous allons ajouter notre logique pour générer des pages Web dynamiques.

```delphi
type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
```

### Étape 3 : Créer un modèle de page

Pour commencer, créons un modèle de page simple que nous pourrons réutiliser. Ajoutez cette méthode à la classe `TWebModule1` :

```delphi
function TWebModule1.GeneratePageHTML(const Title, Content: string): string;
begin
  Result :=
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head>' + #13#10 +
    '  <meta charset="UTF-8">' + #13#10 +
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' + #13#10 +
    '  <title>' + Title + '</title>' + #13#10 +
    '  <style>' + #13#10 +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' + #13#10 +
    '    header { background-color: #4a6da7; color: white; padding: 10px 20px; margin-bottom: 20px; }' + #13#10 +
    '    nav { margin-bottom: 20px; }' + #13#10 +
    '    nav a { margin-right: 15px; color: #4a6da7; text-decoration: none; }' + #13#10 +
    '    nav a:hover { text-decoration: underline; }' + #13#10 +
    '    .content { padding: 20px; background-color: #f5f5f5; border-radius: 5px; }' + #13#10 +
    '    footer { margin-top: 20px; text-align: center; color: #666; font-size: 0.8em; }' + #13#10 +
    '  </style>' + #13#10 +
    '</head>' + #13#10 +
    '<body>' + #13#10 +
    '  <header>' + #13#10 +
    '    <h1>' + Title + '</h1>' + #13#10 +
    '  </header>' + #13#10 +
    '  <nav>' + #13#10 +
    '    <a href="/">Accueil</a>' + #13#10 +
    '    <a href="/produits">Produits</a>' + #13#10 +
    '    <a href="/contact">Contact</a>' + #13#10 +
    '    <a href="/apropos">À propos</a>' + #13#10 +
    '  </nav>' + #13#10 +
    '  <div class="content">' + #13#10 +
    Content + #13#10 +
    '  </div>' + #13#10 +
    '  <footer>' + #13#10 +
    '    <p>&copy; ' + FormatDateTime('yyyy', Now) + ' - Mon Site Web Dynamique avec Delphi</p>' + #13#10 +
    '  </footer>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;
```

Cette fonction nous permettra de générer facilement des pages HTML avec un en-tête, un pied de page et une navigation cohérents.

### Étape 4 : Créer les différentes pages du site

Maintenant, créons les différentes pages de notre site. D'abord, ajoutons les actions correspondantes :

1. Double-cliquez sur le fichier `WebModuleUnit1.dfm` pour ouvrir le concepteur
2. Faites un clic droit sur le module et sélectionnez **Nouveau WebAction**
3. Configurez les actions suivantes :
   - **Action Accueil** : PathInfo = `/` ou `/accueil`, MethodType = `mtGet`
   - **Action Produits** : PathInfo = `/produits`, MethodType = `mtGet`
   - **Action Contact** : PathInfo = `/contact`, MethodType = `mtGet`
   - **Action À propos** : PathInfo = `/apropos`, MethodType = `mtGet`

Ensuite, implémentons les gestionnaires pour ces actions :

```delphi
procedure TWebModule1.AccueilActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content := GeneratePageHTML('Accueil',
    '<h2>Bienvenue sur notre site !</h2>' +
    '<p>Ceci est un exemple de site Web dynamique créé avec Delphi.</p>' +
    '<p>Explorez les différentes sections en utilisant la navigation ci-dessus.</p>');
  Handled := True;
end;

procedure TWebModule1.ProduitsActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  ProductList: string;
begin
  // Nous simulons ici une liste de produits qui pourrait venir d'une base de données
  ProductList :=
    '<div class="product">' +
    '  <h3>Produit 1</h3>' +
    '  <p>Description du produit 1. Un produit vraiment fantastique.</p>' +
    '  <p><strong>Prix : 99,99 €</strong></p>' +
    '</div>' +
    '<div class="product">' +
    '  <h3>Produit 2</h3>' +
    '  <p>Description du produit 2. Encore mieux que le premier.</p>' +
    '  <p><strong>Prix : 149,99 €</strong></p>' +
    '</div>' +
    '<div class="product">' +
    '  <h3>Produit 3</h3>' +
    '  <p>Description du produit 3. Le nec plus ultra.</p>' +
    '  <p><strong>Prix : 199,99 €</strong></p>' +
    '</div>';

  Response.ContentType := 'text/html';
  Response.Content := GeneratePageHTML('Nos Produits',
    '<h2>Catalogue de produits</h2>' +
    '<p>Voici nos produits vedettes :</p>' +
    ProductList);
  Handled := True;
end;

procedure TWebModule1.ContactActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content := GeneratePageHTML('Contact',
    '<h2>Contactez-nous</h2>' +
    '<form method="post" action="/envoi-contact">' +
    '  <div style="margin-bottom: 15px;">' +
    '    <label for="nom">Nom :</label><br>' +
    '    <input type="text" id="nom" name="nom" style="width: 300px; padding: 5px;">' +
    '  </div>' +
    '  <div style="margin-bottom: 15px;">' +
    '    <label for="email">Email :</label><br>' +
    '    <input type="email" id="email" name="email" style="width: 300px; padding: 5px;">' +
    '  </div>' +
    '  <div style="margin-bottom: 15px;">' +
    '    <label for="message">Message :</label><br>' +
    '    <textarea id="message" name="message" rows="5" style="width: 300px; padding: 5px;"></textarea>' +
    '  </div>' +
    '  <div>' +
    '    <button type="submit" style="padding: 8px 15px; background-color: #4a6da7; color: white; border: none; cursor: pointer;">Envoyer</button>' +
    '  </div>' +
    '</form>');
  Handled := True;
end;

procedure TWebModule1.AProposActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content := GeneratePageHTML('À propos',
    '<h2>À propos de nous</h2>' +
    '<p>Notre entreprise a été fondée en 2023 avec une mission simple : créer des sites web dynamiques avec Delphi.</p>' +
    '<p>Nous sommes une équipe passionnée qui aime explorer les possibilités offertes par ce langage puissant.</p>' +
    '<h3>Notre équipe</h3>' +
    '<ul>' +
    '  <li><strong>Jean Dupont</strong> - Fondateur &amp; Développeur principal</li>' +
    '  <li><strong>Marie Martin</strong> - Designer UI/UX</li>' +
    '  <li><strong>Pierre Dubois</strong> - Expert Base de données</li>' +
    '</ul>');
  Handled := True;
end;
```

### Étape 5 : Traiter les formulaires

Maintenant, ajoutons une action pour traiter le formulaire de contact :

```delphi
procedure TWebModule1.EnvoiContactActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Nom, Email, Message: string;
begin
  // Récupérer les données du formulaire
  if Request.MethodType = mtPost then
  begin
    Nom := Request.ContentFields.Values['nom'];
    Email := Request.ContentFields.Values['email'];
    Message := Request.ContentFields.Values['message'];

    // Ici, vous pourriez enregistrer ces informations dans une base de données
    // ou envoyer un email, etc.

    // Pour cet exemple, nous affichons simplement une confirmation
    Response.ContentType := 'text/html';
    Response.Content := GeneratePageHTML('Message envoyé',
      '<h2>Merci pour votre message!</h2>' +
      '<p>Nous avons bien reçu vos informations :</p>' +
      '<ul>' +
      '  <li><strong>Nom :</strong> ' + Nom + '</li>' +
      '  <li><strong>Email :</strong> ' + Email + '</li>' +
      '  <li><strong>Message :</strong> ' + Message + '</li>' +
      '</ul>' +
      '<p>Nous vous répondrons dans les plus brefs délais.</p>' +
      '<p><a href="/">Retour à l''accueil</a></p>');
  end
  else
  begin
    // Rediriger vers le formulaire si quelqu'un accède directement à cette URL
    Response.StatusCode := 302; // Found / Redirection
    Response.Location := '/contact';
  end;

  Handled := True;
end;
```

N'oubliez pas d'ajouter cette action dans le concepteur WebModule avec PathInfo = `/envoi-contact` et MethodType = `mtPost`.

### Étape 6 : Configurer le gestionnaire par défaut

Modifions le gestionnaire par défaut pour rediriger vers la page d'accueil si une URL inconnue est demandée :

```delphi
procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content := GeneratePageHTML('Page non trouvée',
    '<h2>Erreur 404 - Page non trouvée</h2>' +
    '<p>Désolé, la page que vous recherchez n''existe pas.</p>' +
    '<p><a href="/">Retour à l''accueil</a></p>');
  Response.StatusCode := 404;
  Handled := True;
end;
```

### Étape 7 : Compiler et tester

1. Appuyez sur F9 pour compiler et exécuter votre application
2. Ouvrez votre navigateur et accédez à `http://localhost:8080/`
3. Explorez les différentes sections de votre site
4. Testez le formulaire de contact en soumettant des données

Félicitations ! Vous venez de créer un site Web dynamique avec Delphi en utilisant WebBroker.

## Ajouter une base de données à votre site

Un site Web dynamique n'est pas vraiment complet sans une base de données. Ajoutons une connexion à MySQL/MariaDB pour afficher des produits réels.

### Étape 1 : Configurer la connexion à la base de données

Ajoutez les unités nécessaires au début de votre fichier :

```delphi
uses
  // ... autres unités existantes ...
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;
```

Ajoutez un champ à la classe `TWebModule1` :

```delphi
private
  FDConnection: TFDConnection;
```

Initialisez la connexion dans la méthode `WebModuleCreate` :

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FDConnection := TFDConnection.Create(Self);
  FDConnection.DriverName := 'MySQL';
  FDConnection.Params.Values['Server'] := 'localhost';
  FDConnection.Params.Values['Database'] := 'monsite';
  FDConnection.Params.Values['User_Name'] := 'root';
  FDConnection.Params.Values['Password'] := 'motdepasse';
  FDConnection.LoginPrompt := False;

  try
    FDConnection.Connected := True;
  except
    on E: Exception do
      // Gérer l'erreur de connexion (log, etc.)
  end;
end;
```

### Étape 2 : Créer la table et ajouter des données

Vous devrez exécuter ce script SQL dans votre base de données MySQL/MariaDB :

```sql
CREATE DATABASE IF NOT EXISTS monsite;
USE monsite;

CREATE TABLE IF NOT EXISTS produits (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(100) NOT NULL,
  description TEXT,
  prix DECIMAL(10, 2) NOT NULL,
  image VARCHAR(255),
  categorie VARCHAR(50)
);

INSERT INTO produits (nom, description, prix, image, categorie)
VALUES
('Écran 24 pouces', 'Écran HD avec une excellente qualité d''image.', 199.99, 'ecran.jpg', 'Informatique'),
('Clavier mécanique', 'Clavier gaming avec rétroéclairage RGB.', 89.99, 'clavier.jpg', 'Informatique'),
('Souris sans fil', 'Souris ergonomique avec une excellente précision.', 49.99, 'souris.jpg', 'Informatique'),
('Casque audio', 'Casque avec réduction de bruit active.', 149.99, 'casque.jpg', 'Audio'),
('Enceinte Bluetooth', 'Enceinte portable avec une autonomie de 12 heures.', 79.99, 'enceinte.jpg', 'Audio');
```

### Étape 3 : Modifier la page des produits pour utiliser la base de données

```delphi
procedure TWebModule1.ProduitsActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  ProductList: string;
  Categorie: string;
begin
  if not FDConnection.Connected then
  begin
    Response.ContentType := 'text/html';
    Response.Content := GeneratePageHTML('Erreur',
      '<h2>Erreur de connexion à la base de données</h2>' +
      '<p>Impossible de se connecter à la base de données. Veuillez réessayer ultérieurement.</p>');
    Handled := True;
    Exit;
  end;

  // Récupérer le paramètre de catégorie s'il existe
  Categorie := Request.QueryFields.Values['categorie'];

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;

    // Construire la requête SQL
    if Categorie <> '' then
    begin
      Query.SQL.Text := 'SELECT * FROM produits WHERE categorie = :categorie ORDER BY nom';
      Query.ParamByName('categorie').AsString := Categorie;
    end
    else
      Query.SQL.Text := 'SELECT * FROM produits ORDER BY nom';

    Query.Open;

    // Générer le HTML pour les filtres de catégorie
    var CategorieFilter :=
      '<div style="margin-bottom: 20px;">' +
      '  <strong>Filtrer par catégorie : </strong>' +
      '  <a href="/produits">Tous</a> | ' +
      '  <a href="/produits?categorie=Informatique">Informatique</a> | ' +
      '  <a href="/produits?categorie=Audio">Audio</a>' +
      '</div>';

    // Générer le HTML pour les produits
    ProductList := '';
    while not Query.Eof do
    begin
      ProductList := ProductList +
        '<div class="product" style="border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; border-radius: 5px;">' +
        '  <h3>' + Query.FieldByName('nom').AsString + '</h3>' +
        '  <p>' + Query.FieldByName('description').AsString + '</p>' +
        '  <p><strong>Prix : ' + FormatFloat('#,##0.00 €', Query.FieldByName('prix').AsFloat) + '</strong></p>' +
        '  <p><em>Catégorie : ' + Query.FieldByName('categorie').AsString + '</em></p>' +
        '  <a href="/produit-details?id=' + Query.FieldByName('id').AsString + '" ' +
        '     style="display: inline-block; padding: 5px 10px; background-color: #4a6da7; color: white; ' +
        '            text-decoration: none; border-radius: 3px;">Voir détails</a>' +
        '</div>';
      Query.Next;
    end;

    if ProductList = '' then
      ProductList := '<p>Aucun produit trouvé.</p>';

    // Titre de la page en fonction du filtre
    var PageTitle := 'Nos Produits';
    if Categorie <> '' then
      PageTitle := 'Produits - ' + Categorie;

    Response.ContentType := 'text/html';
    Response.Content := GeneratePageHTML(PageTitle,
      '<h2>' + PageTitle + '</h2>' +
      CategorieFilter +
      ProductList);
  finally
    Query.Free;
  end;

  Handled := True;
end;
```

### Étape 4 : Ajouter une page de détails de produit

Ajoutez une nouvelle action pour afficher les détails d'un produit :

```delphi
procedure TWebModule1.ProduitDetailsActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  ProductId: string;
  Content: string;
begin
  ProductId := Request.QueryFields.Values['id'];

  if ProductId = '' then
  begin
    Response.StatusCode := 302; // Found / Redirection
    Response.Location := '/produits';
    Handled := True;
    Exit;
  end;

  if not FDConnection.Connected then
  begin
    Response.ContentType := 'text/html';
    Response.Content := GeneratePageHTML('Erreur',
      '<h2>Erreur de connexion à la base de données</h2>' +
      '<p>Impossible de se connecter à la base de données. Veuillez réessayer ultérieurement.</p>');
    Handled := True;
    Exit;
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;
    Query.SQL.Text := 'SELECT * FROM produits WHERE id = :id';
    Query.ParamByName('id').AsString := ProductId;
    Query.Open;

    if Query.IsEmpty then
    begin
      Response.ContentType := 'text/html';
      Response.Content := GeneratePageHTML('Produit non trouvé',
        '<h2>Erreur - Produit non trouvé</h2>' +
        '<p>Le produit demandé n''existe pas.</p>' +
        '<p><a href="/produits">Retour aux produits</a></p>');
    end
    else
    begin
      Content :=
        '<h2>' + Query.FieldByName('nom').AsString + '</h2>' +
        '<div style="display: flex; flex-wrap: wrap;">' +
        '  <div style="flex: 1; min-width: 300px; margin-right: 20px;">' +
        '    <img src="/images/' + Query.FieldByName('image').AsString + '" ' +
        '         alt="' + Query.FieldByName('nom').AsString + '" ' +
        '         style="max-width: 100%; height: auto; border: 1px solid #ddd;">' +
        '  </div>' +
        '  <div style="flex: 2; min-width: 300px;">' +
        '    <p style="font-size: 1.2em;">' + Query.FieldByName('description').AsString + '</p>' +
        '    <p style="font-size: 1.5em; color: #4a6da7;"><strong>Prix : ' +
               FormatFloat('#,##0.00 €', Query.FieldByName('prix').AsFloat) + '</strong></p>' +
        '    <p><em>Catégorie : ' + Query.FieldByName('categorie').AsString + '</em></p>' +
        '    <button style="padding: 10px 20px; background-color: #4a6da7; color: white; ' +
        '               border: none; border-radius: 5px; cursor: pointer; font-size: 1.1em;">' +
        '      Ajouter au panier' +
        '    </button>' +
        '  </div>' +
        '</div>' +
        '<div style="margin-top: 30px;">' +
        '  <a href="/produits" style="color: #4a6da7;">← Retour aux produits</a>' +
        '</div>';

      Response.ContentType := 'text/html';
      Response.Content := GeneratePageHTML(Query.FieldByName('nom').AsString, Content);
    end;
  finally
    Query.Free;
  end;

  Handled := True;
end;
```

N'oubliez pas d'ajouter cette action dans le concepteur avec PathInfo = `/produit-details` et MethodType = `mtGet`.

### Étape 5 : Servir des images statiques

Pour que les images fonctionnent, nous devons ajouter un gestionnaire pour servir des fichiers statiques :

```delphi
procedure TWebModule1.ImagesActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  FileName, FilePath, FileExt: string;
begin
  // Extraire le nom du fichier de l'URL
  FileName := StringReplace(Request.PathInfo, '/images/', '', [rfIgnoreCase]);

  // Vérifier que le nom de fichier est valide (éviter les attaques par traversée de répertoire)
  if (Pos('..', FileName) > 0) or (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0) then
  begin
    Response.StatusCode := 403; // Forbidden
    Response.ContentType := 'text/plain';
    Response.Content := 'Accès refusé';
    Handled := True;
    Exit;
  end;

  // Chemin complet vers le fichier
  FilePath := ExtractFilePath(ParamStr(0)) + 'images\' + FileName;

  if not FileExists(FilePath) then
  begin
    Response.StatusCode := 404; // Not Found
    Response.ContentType := 'text/plain';
    Response.Content := 'Fichier non trouvé';
    Handled := True;
    Exit;
  end;

  // Déterminer le type MIME en fonction de l'extension
  FileExt := LowerCase(ExtractFileExt(FileName));
  if FileExt = '.jpg' or FileExt = '.jpeg' then
    Response.ContentType := 'image/jpeg'
  else if FileExt = '.png' then
    Response.ContentType := 'image/png'
  else if FileExt = '.gif' then
    Response.ContentType := 'image/gif'
  else if FileExt = '.css' then
    Response.ContentType := 'text/css'
  else if FileExt = '.js' then
    Response.ContentType := 'application/javascript'
  else
    Response.ContentType := 'application/octet-stream';

  // Lire le fichier et l'envoyer comme réponse
  try
    Response.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    Response.FreeContentStream := True; // WebBroker libérera le stream
  except
    on E: Exception do
    begin
      Response.StatusCode := 500; // Internal Server Error
      Response.ContentType := 'text/plain';
      Response.Content := 'Erreur lors de la lecture du fichier';
    end;
  end;

  Handled := True;
end;
```

Ajoutez cette action dans le concepteur avec PathInfo = `/images/*` et MethodType = `mtGet`.

Créez un dossier `images` dans le même répertoire que votre exécutable et placez-y quelques images de produits.

## Autres techniques pour des sites Web dynamiques

### Utilisation des templates

L'approche que nous avons utilisée jusqu'à présent (concaténation de chaînes) peut devenir difficile à maintenir pour des pages complexes. Une meilleure approche consiste à utiliser un système de templates. Voici un exemple simple :

```delphi
function TWebModule1.LoadTemplate(const TemplateName: string): string;
var
  TemplateFile: TextFile;
  FilePath: string;
  Line: string;
begin
  Result := '';
  FilePath := ExtractFilePath(ParamStr(0)) + 'templates\' + TemplateName + '.html';

  if not FileExists(FilePath) then
    Exit;

  AssignFile(TemplateFile, FilePath);
  try
    Reset(TemplateFile);
    while not Eof(TemplateFile) do
    begin
      ReadLn(TemplateFile, Line);
      Result := Result + Line + #13#10;
    end;
  finally
    CloseFile(TemplateFile);
  end;
end;

function TWebModule1.ProcessTemplate(const Template: string; const Params: array of string): string;
var
  Result: string;
  I: Integer;
begin
  Result := Template;

  // Remplacer les paramètres (format : {PARAM1}, {PARAM2}, etc.)
  for I := 0 to High(Params) div 2 do
  begin
    Result := StringReplace(Result,
                           '{' + Params[I*2] + '}',
                           Params[I*2+1],
                           [rfReplaceAll, rfIgnoreCase]);
  end;
end;
```

Vous pourriez alors créer un fichier template comme `templates\produit_detail.html` :

```html
<h2>{TITRE}</h2>
<div style="display: flex; flex-wrap: wrap;">
  <div style="flex: 1; min-width: 300px; margin-right: 20px;">
    <img src="/images/{IMAGE}" alt="{TITRE}" style="max-width: 100%; height: auto; border: 1px solid #ddd;">
  </div>
  <div style="flex: 2; min-width: 300px;">
    <p style="font-size: 1.2em;">{DESCRIPTION}</p>
    <p style="font-size: 1.5em; color: #4a6da7;"><strong>Prix : {PRIX}</strong></p>
    <p><em>Catégorie : {CATEGORIE}</em></p>
    <button style="padding: 10px 20px; background-color: #4a6da7; color: white;
                 border: none; border-radius: 5px; cursor: pointer; font-size: 1.1em;">
      Ajouter au panier
    </button>
  </div>
</div>
<div style="margin-top: 30px;">
  <a href="/produits" style="color: #4a6da7;">← Retour aux produits</a>
</div>
```

Et l'utiliser ainsi :

```delphi
procedure TWebModule1.ProduitDetailsActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  ProductId: string;
  Template, Content: string;
begin
  // ... code initial identique ...

  if Query.IsEmpty then
  begin
    // ... gestion d'erreur identique ...
  end
  else
  begin
    // Charger le template
    Template := LoadTemplate('produit_detail');

    // Traiter le template avec les données du produit
    Content := ProcessTemplate(Template, [
      'TITRE', Query.FieldByName('nom').AsString,
      'IMAGE', Query.FieldByName('image').AsString,
      'DESCRIPTION', Query.FieldByName('description').AsString,
      'PRIX', FormatFloat('#,##0.00 €', Query.FieldByName('prix').AsFloat),
      'CATEGORIE', Query.FieldByName('categorie').AsString
    ]);

    Response.ContentType := 'text/html';
    Response.Content := GeneratePageHTML(Query.FieldByName('nom').AsString, Content);
  end;
  // ... suite du code identique ...
end;
```

Cette approche rend le code beaucoup plus maintenable et permet à un designer de travailler sur les templates HTML sans avoir à modifier le code Delphi.

### Utilisation de technologies côté client

Pour rendre votre site web plus interactif, vous pouvez intégrer JavaScript. Ajoutons par exemple un panier d'achat simple fonctionnant côté client :

1. Créez un fichier `cart.js` dans un dossier `scripts` :

```javascript
// cart.js
let cart = [];

function addToCart(productId, productName, price) {
    // Vérifier si le produit est déjà dans le panier
    const existingItem = cart.find(item => item.id === productId);

    if (existingItem) {
        existingItem.quantity += 1;
    } else {
        cart.push({
            id: productId,
            name: productName,
            price: price,
            quantity: 1
        });
    }

    updateCartDisplay();
    saveCart();

    // Montrer une notification
    const notification = document.getElementById('notification');
    notification.textContent = `${productName} ajouté au panier`;
    notification.style.display = 'block';

    setTimeout(() => {
        notification.style.display = 'none';
    }, 2000);
}

function updateCartDisplay() {
    const cartElement = document.getElementById('cart-items');
    const cartCountElement = document.getElementById('cart-count');
    const cartTotalElement = document.getElementById('cart-total');

    if (!cartElement) return;

    // Mettre à jour le compteur
    const itemCount = cart.reduce((total, item) => total + item.quantity, 0);
    cartCountElement.textContent = itemCount;

    // Calculer le total
    const total = cart.reduce((sum, item) => sum + (item.price * item.quantity), 0);
    cartTotalElement.textContent = total.toFixed(2) + ' €';

    // Mettre à jour le contenu du panier
    cartElement.innerHTML = '';

    cart.forEach(item => {
        const itemElement = document.createElement('div');
        itemElement.className = 'cart-item';
        itemElement.innerHTML = `
            <span>${item.name} x ${item.quantity}</span>
            <span>${(item.price * item.quantity).toFixed(2)} €</span>
            <button onclick="removeFromCart('${item.id}')">×</button>
        `;
        cartElement.appendChild(itemElement);
    });
}

function removeFromCart(productId) {
    const index = cart.findIndex(item => item.id === productId);

    if (index !== -1) {
        if (cart[index].quantity > 1) {
            cart[index].quantity -= 1;
        } else {
            cart.splice(index, 1);
        }

        updateCartDisplay();
        saveCart();
    }
}

function saveCart() {
    localStorage.setItem('shopping-cart', JSON.stringify(cart));
}

function loadCart() {
    const savedCart = localStorage.getItem('shopping-cart');
    if (savedCart) {
        cart = JSON.parse(savedCart);
        updateCartDisplay();
    }
}

// Charger le panier au chargement de la page
document.addEventListener('DOMContentLoaded', loadCart);
```

2. Modifiez votre fonction `GeneratePageHTML` pour inclure les éléments du panier et le script :

```delphi
function TWebModule1.GeneratePageHTML(const Title, Content: string): string;
begin
  Result :=
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head>' + #13#10 +
    '  <meta charset="UTF-8">' + #13#10 +
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' + #13#10 +
    '  <title>' + Title + '</title>' + #13#10 +
    '  <style>' + #13#10 +
    '    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }' + #13#10 +
    '    header { background-color: #4a6da7; color: white; padding: 10px 20px; margin-bottom: 20px; display: flex; justify-content: space-between; align-items: center; }' + #13#10 +
    '    nav { margin-bottom: 20px; }' + #13#10 +
    '    nav a { margin-right: 15px; color: #4a6da7; text-decoration: none; }' + #13#10 +
    '    nav a:hover { text-decoration: underline; }' + #13#10 +
    '    .content { padding: 20px; background-color: #f5f5f5; border-radius: 5px; }' + #13#10 +
    '    footer { margin-top: 20px; text-align: center; color: #666; font-size: 0.8em; }' + #13#10 +
    '    #cart-icon { position: relative; cursor: pointer; }' + #13#10 +
    '    #cart-count { position: absolute; top: -10px; right: -10px; background-color: red; color: white; border-radius: 50%; width: 20px; height: 20px; display: flex; justify-content: center; align-items: center; font-size: 12px; }' + #13#10 +
    '    #cart-panel { display: none; position: absolute; top: 40px; right: 0; background-color: white; border: 1px solid #ddd; border-radius: 5px; padding: 15px; width: 300px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }' + #13#10 +
    '    .cart-item { display: flex; justify-content: space-between; padding: 5px 0; border-bottom: 1px solid #eee; }' + #13#10 +
    '    #notification { display: none; position: fixed; top: 20px; right: 20px; background-color: #4a6da7; color: white; padding: 10px 20px; border-radius: 5px; z-index: 1000; }' + #13#10 +
    '  </style>' + #13#10 +
    '</head>' + #13#10 +
    '<body>' + #13#10 +
    '  <div id="notification"></div>' + #13#10 +
    '  <header>' + #13#10 +
    '    <h1>' + Title + '</h1>' + #13#10 +
    '    <div id="cart-icon" onclick="document.getElementById(''cart-panel'').style.display = document.getElementById(''cart-panel'').style.display === ''none'' ? ''block'' : ''none'';">' + #13#10 +
    '      🛒 <span id="cart-count">0</span>' + #13#10 +
    '      <div id="cart-panel">' + #13#10 +
    '        <h3>Votre panier</h3>' + #13#10 +
    '        <div id="cart-items"></div>' + #13#10 +
    '        <div style="margin-top: 10px; font-weight: bold; text-align: right;">' + #13#10 +
    '          Total: <span id="cart-total">0.00 €</span>' + #13#10 +
    '        </div>' + #13#10 +
    '        <button style="width: 100%; margin-top: 10px; padding: 8px; background-color: #4a6da7; color: white; border: none; border-radius: 3px; cursor: pointer;">Commander</button>' + #13#10 +
    '      </div>' + #13#10 +
    '    </div>' + #13#10 +
    '  </header>' + #13#10 +
    '  <nav>' + #13#10 +
    '    <a href="/">Accueil</a>' + #13#10 +
    '    <a href="/produits">Produits</a>' + #13#10 +
    '    <a href="/contact">Contact</a>' + #13#10 +
    '    <a href="/apropos">À propos</a>' + #13#10 +
    '  </nav>' + #13#10 +
    '  <div class="content">' + #13#10 +
    Content + #13#10 +
    '  </div>' + #13#10 +
    '  <footer>' + #13#10 +
    '    <p>&copy; ' + FormatDateTime('yyyy', Now) + ' - Mon Site Web Dynamique avec Delphi</p>' + #13#10 +
    '  </footer>' + #13#10 +
    '  <script src="/scripts/cart.js"></script>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;
```

3. Créez une action pour servir les scripts JavaScript :

```delphi
procedure TWebModule1.ScriptsActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  FileName, FilePath: string;
begin
  // Extraire le nom du fichier de l'URL
  FileName := StringReplace(Request.PathInfo, '/scripts/', '', [rfIgnoreCase]);

  // Vérifier que le nom de fichier est valide
  if (Pos('..', FileName) > 0) or (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0) then
  begin
    Response.StatusCode := 403; // Forbidden
    Response.ContentType := 'text/plain';
    Response.Content := 'Accès refusé';
    Handled := True;
    Exit;
  end;

  // Chemin complet vers le fichier
  FilePath := ExtractFilePath(ParamStr(0)) + 'scripts\' + FileName;

  if not FileExists(FilePath) then
  begin
    Response.StatusCode := 404; // Not Found
    Response.ContentType := 'text/plain';
    Response.Content := 'Fichier non trouvé';
    Handled := True;
    Exit;
  end;

  // Servir le fichier JavaScript
  Response.ContentType := 'application/javascript';
  Response.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  Response.FreeContentStream := True;
  Handled := True;
end;
```

4. Modifiez la page détail du produit pour ajouter la fonctionnalité d'ajout au panier :

```delphi
// Dans la méthode ProduitDetailsActionAction, remplacez le bouton par :
'    <button onclick="addToCart(''' + Query.FieldByName('id').AsString + ''', ''' +
               HTMLEscape(Query.FieldByName('nom').AsString) + ''', ' +
               FormatFloat('0.00', Query.FieldByName('prix').AsFloat) + ')" ' +
'            style="padding: 10px 20px; background-color: #4a6da7; color: white; ' +
'               border: none; border-radius: 5px; cursor: pointer; font-size: 1.1em;">' +
'      Ajouter au panier' +
'    </button>' +
```

N'oubliez pas de créer la fonction `HTMLEscape` pour échapper les caractères spéciaux :

```delphi
function TWebModule1.HTMLEscape(const S: string): string;
begin
  Result := StringReplace(S, '''', '\''\''', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;
```

## Comparaison des approches alternatives

Vous connaissez maintenant l'approche WebBroker, mais il existe d'autres façons de développer des sites Web dynamiques avec Delphi. Voici une comparaison des principales approches :

### IntraWeb

**Avantages :**
- Approche visuelle similaire au développement VCL
- Gestion automatique des états de session
- Abstractions qui simplifient la programmation Web
- Génération automatique du HTML, CSS et JavaScript

**Inconvénients :**
- Moins de contrôle sur le HTML généré
- Peut être moins performant pour les sites à fort trafic
- Dépendance à une bibliothèque tierce

**Exemple minimal :**

```delphi
unit Unit1;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWCompEdit, IWCompLabel;

type
  TForm1 = class(TIWAppForm)
    IWLabel1: TIWLabel;
    IWEdit1: TIWEdit;
    IWButton1: TIWButton;
    procedure IWButton1Click(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TForm1.IWButton1Click(Sender: TObject);
begin
  IWLabel1.Caption := 'Bonjour ' + IWEdit1.Text + '!';
end;

end.
```

### TMS Web Core

**Avantages :**
- Code Pascal transpilé en JavaScript exécuté côté client
- Utilise des composants visuels similaires à VCL/FMX
- Compatible avec les frameworks web modernes
- Réutilisation possible du code avec les applications bureau

**Inconvénients :**
- Requiert une compréhension du fonctionnement des applications Web côté client
- Nécessite un backend séparé pour les opérations côté serveur

**Exemple minimal :**

```delphi
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.StdCtrls;

type
  TForm1 = class(TWebForm)
    WebLabel1: TWebLabel;
    WebEdit1: TWebEdit;
    WebButton1: TWebButton;
    procedure WebButton1Click(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  WebLabel1.Caption := 'Bonjour ' + WebEdit1.Text + '!';
end;

end.
```

### Approche hybride : Backend Delphi + Frontend moderne

Une approche populaire consiste à utiliser Delphi pour créer un backend RESTful, puis à développer le frontend avec des frameworks web modernes comme React, Angular ou Vue.js.

**Avantages :**
- Utilise les forces de Delphi pour le backend
- Interface utilisateur moderne et réactive
- Séparation claire entre frontend et backend
- Possibilité d'avoir des équipes spécialisées pour chaque partie

**Inconvénients :**
- Nécessite de maîtriser plusieurs technologies
- Peut être plus complexe à mettre en place initialement

## Bonnes pratiques pour le développement Web avec Delphi

1. **Séparez la logique métier de la présentation**
   - Utilisez des classes séparées pour gérer les données et la logique métier
   - Évitez de mélanger SQL et génération HTML

2. **Sécurisez vos applications Web**
   - Protégez-vous contre les injections SQL avec des requêtes paramétrées
   - Échappez toujours les données utilisateur affichées dans le HTML
   - Validez les entrées utilisateur côté serveur
   - Utilisez HTTPS pour les données sensibles

3. **Optimisez les performances**
   - Minimisez le nombre de requêtes à la base de données
   - Utilisez le cache lorsque c'est possible
   - Compressez les réponses (gzip)
   - Optimisez les images et autres ressources statiques

4. **Pensez à la maintenabilité**
   - Utilisez des templates pour séparer HTML et code
   - Structurez votre projet de manière logique
   - Documentez votre code et votre API
   - Implémentez des tests unitaires

5. **Créez des interfaces responsives**
   - Utilisez des techniques CSS modernes (flexbox, grid)
   - Testez sur différentes tailles d'écran
   - Assurez-vous que votre site fonctionne sur mobile

## Conclusion

Le développement de sites Web dynamiques avec Delphi offre plusieurs options, chacune avec ses avantages et ses inconvénients. WebBroker offre un contrôle total sur le HTML généré, tandis qu'IntraWeb et TMS Web Core offrent une approche plus visuelle et plus proche du développement d'applications bureau.

Quelle que soit l'approche choisie, Delphi vous permet de créer des sites Web dynamiques performants et bien structurés, tout en bénéficiant de la puissance du langage Object Pascal et de l'écosystème Delphi.

Pour aller plus loin, explorez les frameworks et bibliothèques complémentaires comme :
- **mORMot** : Un framework complet pour créer des services REST avec Delphi
- **Sparkle** : Une alternative légère pour les services REST
- **DWS (Delphi Web Script)** : Pour exécuter des scripts côté serveur
- **HTMLComponents** : Pour un parsing HTML plus avancé

N'hésitez pas à combiner Delphi avec des technologies web modernes pour tirer le meilleur des deux mondes !
