# 16. Sécurité des applications
## 16.5 Protection contre les vulnérabilités courantes

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Même avec une authentification solide, des autorisations strictes, un chiffrement robuste et des connexions sécurisées, votre application peut rester vulnérable à diverses attaques si vous ne prenez pas en compte les risques de sécurité courants. Dans ce chapitre, nous allons explorer les vulnérabilités les plus fréquentes et comment les éviter dans vos applications Delphi.

### Injection SQL

L'injection SQL reste l'une des vulnérabilités les plus répandues. Elle permet à un attaquant d'insérer des commandes SQL malveillantes dans votre application.

#### Le problème

Voici un exemple de code vulnérable :

```pas
procedure TUserManager.AuthenticateUser(const Username, Password: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // DANGEREUX : Construction directe de requête SQL avec des entrées utilisateur
    Query.SQL.Text := 'SELECT * FROM users WHERE username = ''' + Username +
                      ''' AND password = ''' + Password + '''';
    Query.Open;

    if Query.RecordCount > 0 then
      // Authentification réussie
    else
      // Échec de l'authentification
  finally
    Query.Free;
  end;
end;
```

Un attaquant pourrait entrer `admin' --` comme nom d'utilisateur, ce qui transformerait la requête en :

```sql
SELECT * FROM users WHERE username = 'admin' --' AND password = '...'
```

Le `--` est un commentaire en SQL, ce qui signifie que la vérification du mot de passe est ignorée.

#### La solution : requêtes paramétrées

Utilisez toujours des requêtes paramétrées :

```pas
procedure TUserManager.AuthenticateUser(const Username, Password: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // SÉCURISÉ : Utilisation de paramètres nommés
    Query.SQL.Text := 'SELECT * FROM users WHERE username = :username AND password_hash = :password';
    Query.ParamByName('username').AsString := Username;
    Query.ParamByName('password').AsString := Password;
    Query.Open;

    if Query.RecordCount > 0 then
      // Authentification réussie
    else
      // Échec de l'authentification
  finally
    Query.Free;
  end;
end;
```

#### Bonnes pratiques pour prévenir l'injection SQL

1. **Utilisez toujours des requêtes paramétrées** avec FireDAC, DBX ou tout autre accès aux données.

2. **Validez les entrées utilisateur** avant de les utiliser dans des requêtes.

3. **Utilisez des privilèges minimaux** pour le compte de base de données utilisé par votre application.

4. **N'affichez jamais les erreurs de base de données** directement aux utilisateurs.

```pas
// Exemple de traitement sécurisé des erreurs de base de données
procedure TDataModule1.ExecuteSafeQuery(const SQL: string; Params: array of Variant);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := SQL;

    // Assigner les paramètres
    for var I := 0 to High(Params) div 2 do
      Query.ParamByName(string(Params[I*2])).Value := Params[I*2+1];

    try
      Query.ExecSQL;
    except
      on E: Exception do
      begin
        // Journaliser l'erreur réelle pour le débogage
        LogError('Erreur SQL: ' + E.Message);

        // Afficher un message générique à l'utilisateur
        raise Exception.Create('Une erreur est survenue lors de l''accès à la base de données. ' +
                              'Veuillez contacter le support technique.');
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

### Cross-Site Scripting (XSS)

Si votre application Delphi génère du contenu HTML (comme une application web ou un composant WebView), elle peut être vulnérable aux attaques XSS.

#### Le problème

Imaginons une application de chat où les messages sont affichés dans un composant TWebBrowser :

```pas
procedure TChatForm.AddMessage(const Username, Message: string);
var
  HTML: string;
begin
  // DANGEREUX : Insertion directe du texte dans le HTML
  HTML := WebBrowser1.OleObject.Document.body.innerHTML;
  HTML := HTML + '<div><strong>' + Username + ': </strong>' + Message + '</div>';
  WebBrowser1.OleObject.Document.body.innerHTML := HTML;
end;
```

Un attaquant pourrait envoyer un message comme `<script>alert('Hacked!');</script>` qui serait exécuté dans le navigateur.

#### La solution : échappement HTML

Échappez toujours le contenu généré par l'utilisateur avant de l'insérer dans du HTML :

```pas
function EscapeHTML(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

procedure TChatForm.AddMessage(const Username, Message: string);
var
  HTML: string;
begin
  // SÉCURISÉ : Échapper le contenu généré par l'utilisateur
  HTML := WebBrowser1.OleObject.Document.body.innerHTML;
  HTML := HTML + '<div><strong>' + EscapeHTML(Username) + ': </strong>' +
          EscapeHTML(Message) + '</div>';
  WebBrowser1.OleObject.Document.body.innerHTML := HTML;
end;
```

#### Bonnes pratiques pour prévenir le XSS

1. **Échappez toujours les données utilisateur** affichées dans du HTML.

2. **Utilisez des bibliothèques d'échappement HTML** éprouvées plutôt que d'écrire votre propre fonction.

3. **Implémentez une politique de sécurité du contenu (CSP)** si vous développez une application web.

4. **Validez les entrées utilisateur** avant de les afficher.

### Injection de commandes

L'injection de commandes se produit lorsque votre application exécute des commandes système avec des entrées utilisateur non validées.

#### Le problème

```pas
procedure TUtilityForm.ExecuteCommand(const Command: string);
var
  Output: string;
begin
  // DANGEREUX : Exécution de commandes avec entrée utilisateur non validée
  Output := RunDOSCommand(Command);
  MemoResult.Text := Output;
end;
```

Un attaquant pourrait entrer une commande comme `dir & del /F /S /Q C:\ImportantFiles`.

#### La solution : validation et sanitization

```pas
procedure TUtilityForm.ExecuteCommand(const Command: string);
var
  Output: string;
  SafeCommand: string;
begin
  // Vérifier si la commande est dans une liste d'autorisations
  if not IsCommandAllowed(Command) then
  begin
    ShowMessage('Commande non autorisée');
    Exit;
  end;

  // Supprimer les caractères dangereux
  SafeCommand := SanitizeCommand(Command);

  // Exécuter la commande sécurisée
  Output := RunDOSCommand(SafeCommand);
  MemoResult.Text := Output;
end;

function IsCommandAllowed(const Command: string): Boolean;
const
  AllowedCommands: array[0..2] of string = ('dir', 'type', 'echo');
var
  CleanCommand: string;
  SpacePos: Integer;
begin
  // Extraire la commande principale (avant le premier espace)
  SpacePos := Pos(' ', Command);
  if SpacePos > 0 then
    CleanCommand := LowerCase(Trim(Copy(Command, 1, SpacePos - 1)))
  else
    CleanCommand := LowerCase(Trim(Command));

  // Vérifier si la commande est dans la liste d'autorisations
  for var I := 0 to High(AllowedCommands) do
    if CleanCommand = AllowedCommands[I] then
      Exit(True);

  Result := False;
end;

function SanitizeCommand(const Command: string): string;
begin
  Result := Command;

  // Supprimer les caractères dangereux
  Result := StringReplace(Result, '&', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '', [rfReplaceAll]);
end;
```

#### Bonnes pratiques pour prévenir l'injection de commandes

1. **Évitez d'exécuter des commandes système** si possible.

2. **Utilisez une liste d'autorisations** pour limiter les commandes autorisées.

3. **Sanitisez les entrées utilisateur** en supprimant les caractères dangereux.

4. **Utilisez des API natives** plutôt que des commandes système.

### Attaques par chemin traversant (Path Traversal)

Les attaques par chemin traversant permettent à un attaquant d'accéder à des fichiers en dehors du répertoire prévu.

#### Le problème

```pas
procedure TFileViewer.ShowFile(const Filename: string);
var
  Content: string;
begin
  // DANGEREUX : Accès direct à un fichier avec entrée utilisateur
  Content := TFile.ReadAllText(ExtractFilePath(Application.ExeName) + 'files\' + Filename);
  MemoContent.Text := Content;
end;
```

Un attaquant pourrait entrer `..\..\Windows\System32\drivers\etc\hosts` pour accéder à des fichiers système.

#### La solution : validation et canonicalisation

```pas
procedure TFileViewer.ShowFile(const Filename: string);
var
  Content: string;
  SafePath, FullPath, BasePath: string;
begin
  // Vérifier les caractères interdits
  if ContainsPathTraversalChars(Filename) then
  begin
    ShowMessage('Nom de fichier invalide');
    Exit;
  end;

  // Définir le chemin de base autorisé
  BasePath := ExtractFilePath(Application.ExeName) + 'files\';

  // Construire le chemin complet
  FullPath := BasePath + Filename;

  // Obtenir le chemin canonique (résout les ..\ et .\)
  SafePath := ExpandFileName(FullPath);

  // Vérifier que le chemin canonique est dans le dossier autorisé
  if not StartsText(BasePath, SafePath) then
  begin
    ShowMessage('Accès non autorisé');
    Exit;
  end;

  // Accéder au fichier en toute sécurité
  if FileExists(SafePath) then
  begin
    Content := TFile.ReadAllText(SafePath);
    MemoContent.Text := Content;
  end
  else
    ShowMessage('Fichier introuvable');
end;

function ContainsPathTraversalChars(const Path: string): Boolean;
begin
  Result := (Pos('..', Path) > 0) or
            (Pos('/', Path) > 0) or
            (Pos('\', Path) > 0) or
            (Pos(':', Path) > 0);
end;
```

#### Bonnes pratiques pour prévenir les attaques par chemin traversant

1. **Validez tous les noms de fichiers** fournis par l'utilisateur.

2. **Utilisez des chemins canoniques** pour vérifier l'accès aux fichiers.

3. **Limitez l'accès à un répertoire spécifique** en vérifiant que le chemin final est bien dans ce répertoire.

4. **Utilisez des listes d'autorisations** pour limiter les fichiers accessibles.

### Stockage non sécurisé des données sensibles

Le stockage inapproprié des données sensibles est une vulnérabilité courante.

#### Le problème

```pas
procedure TSettingsManager.SaveConnectionString(const ConnectionString: string);
var
  IniFile: TIniFile;
begin
  // DANGEREUX : Stockage en texte clair des données sensibles
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    IniFile.WriteString('Database', 'ConnectionString', ConnectionString);
  finally
    IniFile.Free;
  end;
end;
```

#### La solution : chiffrement et stockage sécurisé

```pas
// Nécessite Delphi 11 ou supérieur pour certaines fonctionnalités
procedure TSettingsManager.SaveConnectionString(const ConnectionString: string);
var
  IniFile: TIniFile;
  EncryptedString: string;
begin
  // Chiffrer la chaîne de connexion avant de la stocker
  EncryptedString := TModernCrypto.EncryptString(ConnectionString, GetEncryptionKey);

  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    IniFile.WriteString('Database', 'EncryptedConnectionString', EncryptedString);
  finally
    IniFile.Free;
  end;
end;

function TSettingsManager.GetConnectionString: string;
var
  IniFile: TIniFile;
  EncryptedString: string;
begin
  Result := '';

  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    EncryptedString := IniFile.ReadString('Database', 'EncryptedConnectionString', '');

    if EncryptedString <> '' then
      Result := TModernCrypto.DecryptString(EncryptedString, GetEncryptionKey);
  finally
    IniFile.Free;
  end;
end;

function TSettingsManager.GetEncryptionKey: string;
begin
  // En production, utilisez des méthodes plus sécurisées pour stocker/récupérer la clé
  // comme le Windows Data Protection API (DPAPI)
  Result := 'CleSup3rS3cr3teP0urLeCh1ffr3ment';  // Exemple simplifié
end;
```

#### Bonnes pratiques pour le stockage sécurisé

1. **Ne stockez jamais des données sensibles en texte clair**.

2. **Utilisez le chiffrement** pour protéger les données sensibles.

3. **Stockez les clés de chiffrement de manière sécurisée**, idéalement à l'aide d'API système comme DPAPI.

4. **Minimisez le stockage des données sensibles** : ne stockez que ce qui est absolument nécessaire.

### Exposition d'informations sensibles dans les journaux et messages d'erreur

Les journaux et messages d'erreur peuvent révéler des informations sensibles.

#### Le problème

```pas
procedure TDataModule1.ConnectToDatabase;
begin
  try
    FDConnection1.Connected := True;
  except
    on E: Exception do
    begin
      // DANGEREUX : Afficher les détails d'erreur à l'utilisateur
      ShowMessage('Erreur de connexion: ' + E.Message);

      // DANGEREUX : Journaliser des informations sensibles
      LogError('Échec de connexion à ' + FDConnection1.Params.Values['Server'] +
              ' avec l''utilisateur ' + FDConnection1.Params.Values['User_Name']);
    end;
  end;
end;
```

#### La solution : messages génériques et journalisation sécurisée

```pas
procedure TDataModule1.ConnectToDatabase;
begin
  try
    FDConnection1.Connected := True;
  except
    on E: Exception do
    begin
      // SÉCURISÉ : Message d'erreur générique pour l'utilisateur
      ShowMessage('Impossible de se connecter à la base de données. ' +
                 'Veuillez contacter votre administrateur.');

      // SÉCURISÉ : Journaliser sans informations sensibles ou masquer partiellement
      LogError('Échec de connexion à la base de données. Code: ' +
               GetErrorCode(E) + '. Utilisateur: ' +
               MaskSensitiveData(FDConnection1.Params.Values['User_Name']));

      // Journaliser les détails complets dans un journal sécurisé avec accès restreint
      LogSecureError('Détails de connexion: ' + E.Message, True);
    end;
  end;
end;

function MaskSensitiveData(const Data: string): string;
begin
  if Length(Data) <= 2 then
    Result := '***'
  else
    Result := Copy(Data, 1, 1) + StringOfChar('*', Length(Data) - 2) +
              Copy(Data, Length(Data), 1);
end;
```

#### Bonnes pratiques pour la journalisation et les messages d'erreur

1. **Affichez des messages d'erreur génériques** aux utilisateurs.

2. **Ne journalisez jamais des données sensibles** comme les mots de passe, jetons, etc.

3. **Masquez partiellement les informations sensibles** si vous devez les journaliser.

4. **Utilisez différents niveaux de journalisation** avec des contrôles d'accès appropriés.

5. **Sécurisez l'accès aux fichiers journaux** et chiffrez-les si nécessaire.

### Désérialisation non sécurisée

La désérialisation de données non fiables peut conduire à des vulnérabilités.

#### Le problème

```pas
procedure TDataProcessor.LoadSettings(const FileName: string);
var
  FileStream: TFileStream;
  Settings: TSettings;
begin
  // DANGEREUX : Désérialisation sans validation
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Settings := TSettings.Create;
    try
      Settings.LoadFromStream(FileStream);
      ApplySettings(Settings);
    finally
      Settings.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

#### La solution : validation et désérialisation sécurisée

```pas
procedure TDataProcessor.LoadSettings(const FileName: string);
var
  FileStream: TFileStream;
  Settings: TSettings;
  Validator: TSettingsValidator;
begin
  if not FileExists(FileName) then
    Exit;

  // Vérifier d'abord si le fichier a une taille raisonnable
  if GetFileSize(FileName) > MAX_SETTINGS_SIZE then
  begin
    LogSecurityWarning('Tentative de chargement d''un fichier de paramètres trop grand: ' + FileName);
    Exit;
  end;

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Settings := TSettings.Create;
    try
      try
        // Désérialiser les données
        Settings.LoadFromStream(FileStream);

        // Valider les paramètres
        Validator := TSettingsValidator.Create;
        try
          if Validator.Validate(Settings) then
            ApplySettings(Settings)
          else
            LogSecurityWarning('Fichier de paramètres invalide: ' + FileName);
        finally
          Validator.Free;
        end;
      except
        on E: Exception do
        begin
          LogError('Erreur lors du chargement des paramètres: ' + E.Message);
          // Ne pas propager l'exception, utiliser des valeurs par défaut
          Settings.ResetToDefault;
          ApplySettings(Settings);
        end;
      end;
    finally
      Settings.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

// Classe de validation
type
  TSettingsValidator = class
  public
    function Validate(Settings: TSettings): Boolean;
  private
    function IsValidFontSize(Size: Integer): Boolean;
    function IsValidColorValue(Value: Integer): Boolean;
    function IsValidLanguage(const Lang: string): Boolean;
    // Autres méthodes de validation...
  end;

function TSettingsValidator.Validate(Settings: TSettings): Boolean;
begin
  Result := IsValidFontSize(Settings.FontSize) and
            IsValidColorValue(Settings.BackColor) and
            IsValidColorValue(Settings.TextColor) and
            IsValidLanguage(Settings.Language);
  // Autres validations...
end;
```

#### Bonnes pratiques pour la désérialisation sécurisée

1. **Validez toutes les données désérialisées** avant de les utiliser.

2. **Limitez la taille des données** à désérialiser.

3. **Utilisez des formats de sérialisation sécurisés** et des bibliothèques à jour.

4. **Ne désérialisez pas des données provenant de sources non fiables** sans validation approfondie.

### Buffer Overflow et défauts de gestion de la mémoire

Bien que Delphi offre une protection contre de nombreux problèmes de gestion de mémoire, des vulnérabilités peuvent toujours survenir.

#### Le problème

```pas
procedure TBufferHandler.ProcessBuffer(const Buffer: PChar; Size: Integer);
var
  TempBuffer: array[0..255] of Char;
begin
  // DANGEREUX : Pas de vérification de la taille du buffer
  StrCopy(TempBuffer, Buffer);
  // Traitement...
end;
```

Si `Buffer` contient plus de 256 caractères, cela causera un dépassement de tampon.

#### La solution : vérification des limites

```pas
procedure TBufferHandler.ProcessBuffer(const Buffer: PChar; Size: Integer);
var
  TempBuffer: array[0..255] of Char;
begin
  // SÉCURISÉ : Vérifier la taille avant de copier
  if Size > SizeOf(TempBuffer) - 1 then
  begin
    LogSecurityWarning('Tentative de dépassement de tampon détectée');
    Exit;
  end;

  // Utiliser StrLCopy qui respecte la taille limite
  StrLCopy(TempBuffer, Buffer, SizeOf(TempBuffer) - 1);

  // Assurer la terminaison par zéro
  TempBuffer[SizeOf(TempBuffer) - 1] := #0;

  // Traitement...
end;
```

#### Bonnes pratiques pour éviter les problèmes de mémoire

1. **Utilisez les classes et types sécurisés** de Delphi (String, TStringList, etc.) au lieu de manipuler directement la mémoire.

2. **Vérifiez toujours les limites** avant d'accéder aux tableaux ou d'effectuer des opérations sur des tampons.

3. **Utilisez les fonctions sécurisées** comme `StrLCopy` au lieu de `StrCopy`.

4. **Libérez correctement la mémoire allouée** avec les instructions try-finally.

### Failles CORS (Cross-Origin Resource Sharing)

Si votre application Delphi inclut un serveur web ou interagit avec des API web, elle peut être vulnérable aux problèmes CORS.

#### Le problème

```pas
procedure TWebServerModule.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // DANGEREUX : Configuration CORS trop permissive
  Response.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  Response.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'GET, POST, PUT, DELETE, OPTIONS';
  Response.CustomHeaders.Values['Access-Control-Allow-Headers'] := '*';

  // Traitement de la requête...
end;
```

#### La solution : configuration CORS stricte

```pas
procedure TWebServerModule.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Origin: string;
begin
  // Récupérer l'origine de la requête
  Origin := Request.GetFieldByName('Origin');

  // Vérifier si l'origine est autorisée
  if IsAllowedOrigin(Origin) then
  begin
    // SÉCURISÉ : Spécifier exactement l'origine autorisée
    Response.CustomHeaders.Values['Access-Control-Allow-Origin'] := Origin;

    // Spécifier précisément les méthodes et en-têtes autorisés
    Response.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'GET, POST';
    Response.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, Authorization';

    // Pas de cookies ou d'authentification en cross-origin
    Response.CustomHeaders.Values['Access-Control-Allow-Credentials'] := 'false';
  end;

  // Traitement de la requête...
end;

function IsAllowedOrigin(const Origin: string): Boolean;
const
  AllowedOrigins: array[0..1] of string = (
    'https://app.example.com',
    'https://admin.example.com'
  );
begin
  Result := False;

  for var I := 0 to High(AllowedOrigins) do
    if SameText(Origin, AllowedOrigins[I]) then
      Exit(True);
end;
```

#### Bonnes pratiques pour CORS

1. **Ne pas utiliser `*` pour Access-Control-Allow-Origin** en production.

2. **Spécifiez précisément les origines, méthodes et en-têtes autorisés**.

3. **Limitez l'utilisation de Access-Control-Allow-Credentials** (cookies, authentification).

4. **Implémentez une liste d'autorisations** pour les origines autorisées.

### Protection contre la manipulation des paramètres côté client

Les entrées côté client peuvent être facilement manipulées par les utilisateurs malveillants.

#### Le problème

```pas
procedure TOrderProcessor.ProcessOrder(Request: TWebRequest);
var
  ProductID, Quantity, Price: Integer;
begin
  // DANGEREUX : Faire confiance aux paramètres côté client
  ProductID := StrToIntDef(Request.QueryFields.Values['productId'], 0);
  Quantity := StrToIntDef(Request.QueryFields.Values['quantity'], 0);
  Price := StrToIntDef(Request.QueryFields.Values['price'], 0);

  // Calculer le total
  var Total := Price * Quantity;

  // Créer la commande...
end;
```

Un attaquant pourrait modifier le paramètre `price` pour réduire le coût total.

#### La solution : validation côté serveur

```pas
procedure TOrderProcessor.ProcessOrder(Request: TWebRequest);
var
  ProductID, Quantity: Integer;
  Price: Currency;
begin
  // SÉCURISÉ : Valider et récupérer les informations côté serveur
  ProductID := StrToIntDef(Request.QueryFields.Values['productId'], 0);
  Quantity := StrToIntDef(Request.QueryFields.Values['quantity'], 0);

  // Valider les paramètres
  if (ProductID <= 0) or (Quantity <= 0) then
  begin
    SendErrorResponse('Paramètres invalides');
    Exit;
  end;

  // Récupérer le prix depuis la base de données, pas depuis la requête
  Price := GetProductPrice(ProductID);

  if Price <= 0 then
  begin
    SendErrorResponse('Produit non trouvé');
    Exit;
  end;

  // Limiter la quantité
  if Quantity > 10 then
  begin
    SendErrorResponse('Quantité maximum dépassée');
    Exit;
  end;

  // Calculer le total
  var Total := Price * Quantity;

  // Créer la commande...
end;

function TOrderProcessor.GetProductPrice(ProductID: Integer): Currency;
var
  Query: TFDQuery;
begin
  Result := 0;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT price FROM products WHERE id = :id';
    Query.ParamByName('id').AsInteger := ProductID;
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('price').AsCurrency;
  finally
    Query.Free;
  end;
end;
```

#### Bonnes pratiques pour la validation des paramètres

1. **Ne faites jamais confiance aux données côté client**.

2. **Validez toutes les entrées utilisateur** côté serveur.

3. **Récupérez les données sensibles** (comme les prix) depuis la base de données.

4. **Imposez des limites sur les quantités, montants, etc.**

5. **Vérifiez toujours les autorisations** pour chaque action.

### CSRF (Cross-Site Request Forgery)

Les attaques CSRF permettent d'exécuter des actions non autorisées au nom d'un utilisateur authentifié.

#### Le problème

```pas
procedure TUserSettingsController.UpdateEmailHandler(Request: TWebRequest;
  Response: TWebResponse);
var
  NewEmail: string;
  UserID: Integer;
begin
  // DANGEREUX : Pas de protection contre CSRF
  UserID := GetUserIDFromSession(Request);
  NewEmail := Request.ContentFields.Values['email'];

  if UserID > 0 then
    UpdateUserEmail(UserID, NewEmail);

  // Répondre à la requête...
end;
```

#### La solution : jetons anti-CSRF

```pas
procedure TUserSettingsController.GetUpdateProfileFormHandler(Request: TWebRequest;
  Response: TWebResponse);
var
  CSRFToken: string;
  UserID: Integer;
  OutputHTML: string;
begin
  UserID := GetUserIDFromSession(Request);

  if UserID <= 0 then
  begin
    Response.StatusCode := 401; // Non autorisé
    Exit;
  end;

  // Générer un jeton CSRF unique pour cette session
  CSRFToken := GenerateCSRFToken(UserID);

  // Stocker le jeton CSRF dans la session
  StoreCSRFToken(UserID, CSRFToken);

  // Générer le formulaire HTML avec le jeton CSRF caché
  OutputHTML := '<form method="post" action="/update_email">' +
                '<input type="hidden" name="csrf_token" value="' + CSRFToken + '">' +
                '<label for="email">Nouvel email :</label>' +
                '<input type="email" id="email" name="email">' +
                '<button type="submit">Mettre à jour</button>' +
                '</form>';

  Response.ContentType := 'text/html';
  Response.Content := OutputHTML;
end;

procedure TUserSettingsController.UpdateEmailHandler(Request: TWebRequest;
  Response: TWebResponse);
var
  NewEmail, CSRFToken, StoredToken: string;
  UserID: Integer;
begin
  // SÉCURISÉ : Vérifier le jeton anti-CSRF
  UserID := GetUserIDFromSession(Request);

  if UserID <= 0 then
  begin
    Response.StatusCode := 401; // Non autorisé
    Exit;
  end;

  // Récupérer le jeton soumis et le jeton stocké
  CSRFToken := Request.ContentFields.Values['csrf_token'];
  StoredToken := GetStoredCSRFToken(UserID);

  // Vérifier que le jeton est valide
  if (CSRFToken = '') or (StoredToken = '') or (CSRFToken <> StoredToken) then
  begin
    Response.StatusCode := 403; // Interdit
    Response.Content := 'Jeton CSRF invalide';
    LogSecurityEvent(slWarning, 'Tentative CSRF potentielle détectée', 'UpdateEmail');
    Exit;
  end;

  // Le jeton est valide, procéder à la mise à jour
  NewEmail := Request.ContentFields.Values['email'];

  // Valider l'email
  if not IsValidEmail(NewEmail) then
  begin
    Response.StatusCode := 400; // Requête incorrecte
    Response.Content := 'Format d''email invalide';
    Exit;
  end;

  // Mettre à jour l'email
  UpdateUserEmail(UserID, NewEmail);

  // Générer un nouveau jeton pour la prochaine requête
  RegenerateCSRFToken(UserID);

  // Répondre à la requête...
end;

function GenerateCSRFToken(UserID: Integer): string;
var
  TokenData: TBytes;
  HashBytes: TBytes;
begin
  // Créer des données uniques basées sur l'ID utilisateur et un timestamp
  TokenData := TEncoding.UTF8.GetBytes(
    Format('%d:%s:%s', [
      UserID,
      FormatDateTime('yyyymmddhhnnsszzz', Now),
      GetSecretKey  // Une clé secrète stockée côté serveur
    ])
  );

  // Hacher les données pour créer un jeton
  HashBytes := THashSHA2.GetHashBytes(TokenData);

  // Convertir en chaîne Base64 pour faciliter la transmission
  Result := TNetEncoding.Base64.EncodeBytesToString(HashBytes);
end;
```

#### Bonnes pratiques pour prévenir le CSRF

1. **Utilisez des jetons anti-CSRF** dans tous les formulaires et requêtes AJAX.

2. **Vérifiez l'en-tête Origin ou Referer** pour les requêtes sensibles.

3. **Utilisez le header SameSite=Strict** pour les cookies d'authentification.

4. **Implémentez la double soumission de cookies** pour une sécurité renforcée.

5. **Limitez la durée de validité** des jetons CSRF.

### Attaques par force brute et limitation de débit

Les attaques par force brute tentent de deviner des identifiants ou des mots de passe par essais répétés.

#### Le problème

```pas
procedure TAuthController.LoginHandler(Request: TWebRequest; Response: TWebResponse);
var
  Username, Password: string;
  Authenticated: Boolean;
begin
  // DANGEREUX : Pas de protection contre les attaques par force brute
  Username := Request.ContentFields.Values['username'];
  Password := Request.ContentFields.Values['password'];

  Authenticated := AuthenticateUser(Username, Password);

  if Authenticated then
  begin
    // Créer la session, rediriger, etc.
  end
  else
  begin
    Response.StatusCode := 401;
    Response.Content := 'Identifiants invalides';
  end;
end;
```

#### La solution : limitation de débit et verrouillage de compte

```pas
type
  TLoginAttempt = record
    Username: string;
    IPAddress: string;
    Timestamp: TDateTime;
    Success: Boolean;
  end;

var
  LoginAttempts: TThreadList<TLoginAttempt>;  // Liste thread-safe

procedure TAuthController.LoginHandler(Request: TWebRequest; Response: TWebResponse);
var
  Username, Password, IPAddress: string;
  Authenticated: Boolean;
begin
  Username := Request.ContentFields.Values['username'];
  Password := Request.ContentFields.Values['password'];
  IPAddress := GetClientIPAddress(Request);

  // SÉCURISÉ : Vérifier si l'adresse IP ou le compte est verrouillé
  if IsIPBlocked(IPAddress) then
  begin
    Response.StatusCode := 429; // Trop de requêtes
    Response.Content := 'Trop de tentatives de connexion. Veuillez réessayer plus tard.';
    Exit;
  end;

  if IsAccountLocked(Username) then
  begin
    Response.StatusCode := 403; // Interdit
    Response.Content := 'Ce compte est temporairement verrouillé suite à de multiples échecs de connexion.';
    Exit;
  end;

  // Ajouter un léger délai aléatoire pour ralentir les attaques
  Sleep(Random(500) + 100);

  // Tenter l'authentification
  Authenticated := AuthenticateUser(Username, Password);

  // Enregistrer la tentative
  RecordLoginAttempt(Username, IPAddress, Authenticated);

  if Authenticated then
  begin
    // Réinitialiser le compteur d'échecs
    ResetFailedLoginCounter(Username);

    // Créer la session, rediriger, etc.
  end
  else
  begin
    Response.StatusCode := 401;
    Response.Content := 'Identifiants invalides';

    // Incrémenter le compteur d'échecs
    IncrementFailedLoginCounter(Username, IPAddress);
  end;
end;

procedure RecordLoginAttempt(const Username, IPAddress: string; Success: Boolean);
var
  Attempt: TLoginAttempt;
  List: TList<TLoginAttempt>;
begin
  // Créer un enregistrement de tentative
  Attempt.Username := Username;
  Attempt.IPAddress := IPAddress;
  Attempt.Timestamp := Now;
  Attempt.Success := Success;

  // Ajouter à la liste des tentatives
  List := LoginAttempts.LockList;
  try
    List.Add(Attempt);

    // Optionnel : nettoyer les anciennes tentatives
    CleanupOldAttempts(List);
  finally
    LoginAttempts.UnlockList;
  end;

  // Journaliser la tentative
  if not Success then
    LogSecurityEvent(slWarning,
                    Format('Échec de connexion pour %s depuis %s', [Username, IPAddress]),
                    'Authentication');
end;

function IsIPBlocked(const IPAddress: string): Boolean;
var
  List: TList<TLoginAttempt>;
  FailedAttempts, I: Integer;
  OneHourAgo: TDateTime;
begin
  Result := False;
  FailedAttempts := 0;
  OneHourAgo := Now - (1 / 24); // 1 heure

  List := LoginAttempts.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      if (List[I].IPAddress = IPAddress) and
         (List[I].Timestamp > OneHourAgo) and
         (not List[I].Success) then
        Inc(FailedAttempts);
    end;

    // Bloquer après 10 échecs en une heure depuis la même IP
    Result := FailedAttempts >= 10;
  finally
    LoginAttempts.UnlockList;
  end;
end;

function IsAccountLocked(const Username: string): Boolean;
var
  List: TList<TLoginAttempt>;
  FailedAttempts, I: Integer;
  ThirtyMinutesAgo: TDateTime;
begin
  Result := False;
  FailedAttempts := 0;
  ThirtyMinutesAgo := Now - (30 / 1440); // 30 minutes

  List := LoginAttempts.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      if (List[I].Username = Username) and
         (List[I].Timestamp > ThirtyMinutesAgo) and
         (not List[I].Success) then
        Inc(FailedAttempts);
    end;

    // Verrouiller le compte après 5 échecs en 30 minutes
    Result := FailedAttempts >= 5;
  finally
    LoginAttempts.UnlockList;
  end;
end;
```

#### Bonnes pratiques contre les attaques par force brute

1. **Implémentez une limitation de débit** basée sur l'adresse IP et le nom d'utilisateur.

2. **Verrouillez temporairement les comptes** après plusieurs échecs consécutifs.

3. **Utilisez des CAPTCHA** après quelques tentatives infructueuses.

4. **Introduisez un délai progressif** entre les tentatives.

5. **Journalisez toutes les tentatives d'authentification** et alertez sur les comportements suspects.

6. **Utilisez des mots de passe forts** et encouragez l'authentification à deux facteurs.

### Exposition de données sensibles dans les URL

Les données sensibles exposées dans les URL peuvent être interceptées ou stockées dans des historiques de navigation, journaux de serveur, etc.

#### Le problème

```pas
procedure TReportController.GenerateReportHandler(Request: TWebRequest;
  Response: TWebResponse);
var
  ReportID, APIKey: string;
begin
  // DANGEREUX : Données sensibles exposées dans l'URL
  ReportID := Request.QueryFields.Values['report_id'];
  APIKey := Request.QueryFields.Values['api_key'];

  if IsValidAPIKey(APIKey) then
  begin
    GenerateAndSendReport(ReportID, Response);
  end
  else
  begin
    Response.StatusCode := 403;
    Response.Content := 'Clé API invalide';
  end;
end;
```

#### La solution : données sensibles dans le corps ou les en-têtes

```pas
procedure TReportController.GenerateReportHandler(Request: TWebRequest;
  Response: TWebResponse);
var
  ReportID: string;
  APIKey: string;
begin
  // SÉCURISÉ : Récupérer la clé API depuis l'en-tête Authorization
  APIKey := Request.GetFieldByName('Authorization');

  // Supprimer le préfixe "Bearer " si présent
  if StartsText('Bearer ', APIKey) then
    APIKey := Copy(APIKey, 8, Length(APIKey));

  // Récupérer l'ID du rapport depuis le chemin URL ou le corps
  if Request.MethodType = mtPOST then
    ReportID := Request.ContentFields.Values['report_id']
  else
    ReportID := Request.PathInfo.Substring(Request.PathInfo.LastIndexOf('/') + 1);

  if IsValidAPIKey(APIKey) then
  begin
    GenerateAndSendReport(ReportID, Response);
  end
  else
  begin
    Response.StatusCode := 403;
    Response.Content := 'Clé API invalide';
  end;
end;
```

#### Bonnes pratiques pour la manipulation des données sensibles

1. **Ne placez jamais de données sensibles dans les URL**.

2. **Utilisez des en-têtes HTTP** pour les jetons d'authentification.

3. **Utilisez POST au lieu de GET** pour les opérations impliquant des données sensibles.

4. **Implémentez HTTPS** pour chiffrer toutes les communications.

5. **Utilisez des identifiants aléatoires** plutôt que des identifiants séquentiels prévisibles.

### Absence de protection contre le détournement de clics (Clickjacking)

Le clickjacking permet à un attaquant de tromper un utilisateur en lui faisant cliquer sur quelque chose de différent de ce qu'il croit.

#### Le problème

```pas
procedure TWebServer.SendResponse(Response: TIdHTTPResponseInfo);
begin
  // DANGEREUX : Pas de protection contre le clickjacking
  Response.ContentText := '<html><body><h1>Mon contenu sensible</h1>...</body></html>';
  Response.ContentType := 'text/html';
  Response.ResponseNo := 200;
end;
```

#### La solution : en-têtes de sécurité

```pas
procedure TWebServer.SendResponse(Response: TIdHTTPResponseInfo);
begin
  // SÉCURISÉ : Ajouter des en-têtes de protection

  // Empêcher le contenu d'être affiché dans un iframe
  Response.CustomHeaders.Values['X-Frame-Options'] := 'DENY';

  // Protection moderne contre le clickjacking et autres
  Response.CustomHeaders.Values['Content-Security-Policy'] :=
    'frame-ancestors ''none''; default-src ''self''; script-src ''self'' ''unsafe-inline''';

  // Autres en-têtes de sécurité utiles
  Response.CustomHeaders.Values['X-Content-Type-Options'] := 'nosniff';
  Response.CustomHeaders.Values['X-XSS-Protection'] := '1; mode=block';

  // Le contenu de la réponse
  Response.ContentText := '<html><body><h1>Mon contenu sensible</h1>...</body></html>';
  Response.ContentType := 'text/html';
  Response.ResponseNo := 200;
end;
```

#### Bonnes pratiques pour prévenir le clickjacking

1. **Utilisez l'en-tête X-Frame-Options** pour contrôler si votre page peut être affichée dans un iframe.

2. **Implémentez Content-Security-Policy** avec la directive frame-ancestors pour un contrôle plus précis.

3. **Utilisez un middleware de sécurité** pour appliquer ces en-têtes à toutes les réponses.

### Mauvaise gestion des sessions

Une gestion de session incorrecte peut conduire à de nombreuses vulnérabilités, comme le vol de session.

#### Le problème

```pas
procedure TSessionManager.CreateSession(const Username: string): string;
begin
  // DANGEREUX : Identifiant de session prévisible
  Result := 'SESSION_' + Username + '_' + FormatDateTime('yyyymmdd', Date);

  // Stocker la session...
end;

procedure TSessionManager.ValidateSession(const SessionID: string);
begin
  // DANGEREUX : Pas de vérification d'expiration
  if SessionExists(SessionID) then
    ExtendSession(SessionID)  // Prolonger indéfiniment
  else
    RaiseException('Session invalide');
end;
```

#### La solution : gestion de session sécurisée

```pas
type
  TSession = record
    ID: string;
    UserID: Integer;
    Username: string;
    IPAddress: string;
    UserAgent: string;
    CreatedAt: TDateTime;
    LastActivity: TDateTime;
    ExpiresAt: TDateTime;
  end;

procedure TSessionManager.CreateSession(const Username: string;
  Request: TWebRequest): string;
var
  Session: TSession;
  UserID: Integer;
  SessionData: TBytes;
begin
  // Générer un ID de session aléatoire et cryptographiquement sûr
  SessionData := TEncoding.UTF8.GetBytes(
    Username + '|' +
    Request.RemoteAddr + '|' +
    Request.UserAgent + '|' +
    FormatDateTime('yyyymmddhhnnsszzz', Now) + '|' +
    IntToStr(Random(1000000))
  );

  Result := THashSHA2.GetHashString(SessionData);

  // Récupérer l'ID de l'utilisateur
  UserID := GetUserID(Username);

  // Créer l'objet session
  Session.ID := Result;
  Session.UserID := UserID;
  Session.Username := Username;
  Session.IPAddress := Request.RemoteAddr;
  Session.UserAgent := Request.UserAgent;
  Session.CreatedAt := Now;
  Session.LastActivity := Now;
  Session.ExpiresAt := Now + (30 / 1440); // 30 minutes

  // Stocker la session
  StoreSession(Session);

  // Supprimer les anciennes sessions de cet utilisateur (optionnel)
  CleanupOldSessions(UserID);

  // Journaliser la création de session
  LogSecurityEvent(slInfo,
                  Format('Session créée pour %s depuis %s', [Username, Session.IPAddress]),
                  'SessionManagement');
end;

function TSessionManager.ValidateSession(const SessionID: string;
  Request: TWebRequest): Boolean;
var
  Session: TSession;
begin
  Result := False;

  if not GetSession(SessionID, Session) then
    Exit;

  // Vérifier si la session a expiré
  if Now > Session.ExpiresAt then
  begin
    DestroySession(SessionID);
    LogSecurityEvent(slInfo, 'Session expirée', 'SessionManagement');
    Exit;
  end;

  // Vérification de liaison de session (optionnel mais recommandé)
  // Vérifier que la session provient du même navigateur/appareil
  if (Session.IPAddress <> Request.RemoteAddr) or
     (Session.UserAgent <> Request.UserAgent) then
  begin
    LogSecurityEvent(slWarning,
                    'Possible détournement de session détecté',
                    'SessionManagement');
    DestroySession(SessionID);
    Exit;
  end;

  // Mettre à jour la dernière activité et prolonger la session
  Session.LastActivity := Now;
  Session.ExpiresAt := Now + (30 / 1440); // 30 minutes
  UpdateSession(Session);

  Result := True;
end;

procedure TSessionManager.DestroySession(const SessionID: string);
begin
  // Supprimer la session de la base de données ou du stockage
  if DeleteSession(SessionID) then
    LogSecurityEvent(slInfo, 'Session détruite: ' + SessionID, 'SessionManagement');
end;
```

#### Bonnes pratiques pour la gestion des sessions

1. **Utilisez des identifiants de session aléatoires** et cryptographiquement sûrs.

2. **Régénérez les identifiants de session** après l'authentification (rotation des sessions).

3. **Définissez une durée d'expiration** pour toutes les sessions.

4. **Utilisez des cookies sécurisés** avec les flags HttpOnly, Secure et SameSite.

5. **Vérifiez l'adresse IP et l'agent utilisateur** pour détecter un possible vol de session.

6. **Permettez aux utilisateurs de voir et terminer leurs sessions actives**.

7. **Implémentez une fonctionnalité de déconnexion** qui détruit correctement la session.

### Mise en œuvre d'une politique de sécurité centralisée

Pour une approche cohérente de la sécurité, créez un module centralisé qui applique les meilleures pratiques à toute votre application :

```pas
unit SecurityPolicy;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.NetEncoding,
  IdHTTPServer, IdCustomHTTPServer;

type
  TSecurityHeaders = record
    XFrameOptions: string;
    ContentSecurityPolicy: string;
    XContentTypeOptions: string;
    XSSProtection: string;
    ReferrerPolicy: string;
    StrictTransportSecurity: string;
  end;

  TSecurityPolicy = class
  private
    FSecurityHeaders: TSecurityHeaders;

    procedure InitDefaultHeaders;
  public
    constructor Create;

    // Appliquer des en-têtes de sécurité
    procedure ApplySecurityHeaders(Response: TWebResponse); overload;
    procedure ApplySecurityHeaders(Response: TIdHTTPResponseInfo); overload;

    // Validation d'entrée
    function SanitizeHTML(const Input: string): string;
    function ValidateInput(const Input: string; InputType: string): Boolean;

    // Journalisation de sécurité
    procedure LogSecurityEvent(Level: string; const Message, Source: string);

    // Propriétés pour configurer la politique
    property SecurityHeaders: TSecurityHeaders read FSecurityHeaders write FSecurityHeaders;
  end;

var
  SecurityPolicy: TSecurityPolicy;

implementation

constructor TSecurityPolicy.Create;
begin
  inherited Create;
  InitDefaultHeaders;
end;

procedure TSecurityPolicy.InitDefaultHeaders;
begin
  // Définir les en-têtes de sécurité par défaut
  FSecurityHeaders.XFrameOptions := 'DENY';
  FSecurityHeaders.ContentSecurityPolicy :=
    'default-src ''self''; script-src ''self'' ''unsafe-inline''; frame-ancestors ''none''';
  FSecurityHeaders.XContentTypeOptions := 'nosniff';
  FSecurityHeaders.XSSProtection := '1; mode=block';
  FSecurityHeaders.ReferrerPolicy := 'strict-origin-when-cross-origin';
  FSecurityHeaders.StrictTransportSecurity := 'max-age=31536000; includeSubDomains';
end;

procedure TSecurityPolicy.ApplySecurityHeaders(Response: TWebResponse);
begin
  Response.CustomHeaders.Values['X-Frame-Options'] := FSecurityHeaders.XFrameOptions;
  Response.CustomHeaders.Values['Content-Security-Policy'] := FSecurityHeaders.ContentSecurityPolicy;
  Response.CustomHeaders.Values['X-Content-Type-Options'] := FSecurityHeaders.XContentTypeOptions;
  Response.CustomHeaders.Values['X-XSS-Protection'] := FSecurityHeaders.XSSProtection;
  Response.CustomHeaders.Values['Referrer-Policy'] := FSecurityHeaders.ReferrerPolicy;
  Response.CustomHeaders.Values['Strict-Transport-Security'] := FSecurityHeaders.StrictTransportSecurity;
end;

procedure TSecurityPolicy.ApplySecurityHeaders(Response: TIdHTTPResponseInfo);
begin
  Response.CustomHeaders.Values['X-Frame-Options'] := FSecurityHeaders.XFrameOptions;
  Response.CustomHeaders.Values['Content-Security-Policy'] := FSecurityHeaders.ContentSecurityPolicy;
  Response.CustomHeaders.Values['X-Content-Type-Options'] := FSecurityHeaders.XContentTypeOptions;
  Response.CustomHeaders.Values['X-XSS-Protection'] := FSecurityHeaders.XSSProtection;
  Response.CustomHeaders.Values['Referrer-Policy'] := FSecurityHeaders.ReferrerPolicy;
  Response.CustomHeaders.Values['Strict-Transport-Security'] := FSecurityHeaders.StrictTransportSecurity;
end;

function TSecurityPolicy.SanitizeHTML(const Input: string): string;
begin
  // Implémenter un sanitizer HTML complet
  // Ceci est une version simplifiée
  Result := StringReplace(Input, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function TSecurityPolicy.ValidateInput(const Input: string; InputType: string): Boolean;
begin
  Result := False;

  if Input = '' then
    Exit;

  // Valider selon le type d'entrée
  if InputType = 'email' then
    Result := TRegEx.IsMatch(Input, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$')
  else if InputType = 'username' then
    Result := TRegEx.IsMatch(Input, '^[a-zA-Z0-9_-]{3,20}$')
  else if InputType = 'number' then
    Result := TRegEx.IsMatch(Input, '^[0-9]+$')
  else if InputType = 'date' then
    Result := TRegEx.IsMatch(Input, '^\d{4}-\d{2}-\d{2}$')
  else if InputType = 'phone' then
    Result := TRegEx.IsMatch(Input, '^\+?[0-9]{10,15}$')
  else
    // Type inconnu, considérer valide mais loguer un avertissement
    begin
      LogSecurityEvent('WARNING', 'Type de validation inconnu: ' + InputType, 'InputValidation');
      Result := True;
    end;
end;

procedure TSecurityPolicy.LogSecurityEvent(Level: string; const Message, Source: string);
var
  LogFile: TextFile;
  LogFileName, LogMessage: string;
  TimeStamp: string;
begin
  // Générer un nom de fichier basé sur la date
  LogFileName := FormatDateTime('yyyy-mm-dd', Date) + '_security.log';

  // Préparer le message de journal
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);

  LogMessage := Format('[%s] [%s] [%s] %s',
                       [TimeStamp, Level, Source, Message]);

  // Écrire dans le fichier de journal
  AssignFile(LogFile, LogFileName);
  try
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, LogMessage);
  finally
    CloseFile(LogFile);
  end;
end;

initialization
  SecurityPolicy := TSecurityPolicy.Create;

finalization
  SecurityPolicy.Free;

end.
```

### Utilisation de la politique de sécurité

```pas
procedure TWebModule.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Username, Email: string;
begin
  // Appliquer les en-têtes de sécurité à toutes les réponses
  SecurityPolicy.ApplySecurityHeaders(Response);

  // Valider et sanitiser les entrées
  Username := Request.ContentFields.Values['username'];
  Email := Request.ContentFields.Values['email'];

  if not SecurityPolicy.ValidateInput(Username, 'username') then
  begin
    Response.StatusCode := 400;
    Response.Content := 'Nom d''utilisateur invalide';
    Exit;
  end;

  if not SecurityPolicy.ValidateInput(Email, 'email') then
  begin
    Response.StatusCode := 400;
    Response.Content := 'Email invalide';
    Exit;
  end;

  // Journaliser l'événement
  SecurityPolicy.LogSecurityEvent('INFO',
                                 Format('Traitement de la requête pour %s', [Username]),
                                 'WebModule');

  // Traitement de la requête...
end;
```

### Conclusion

La protection contre les vulnérabilités courantes est un aspect fondamental de la sécurité des applications. En suivant les bonnes pratiques présentées dans ce chapitre, vous pouvez considérablement réduire les risques de sécurité dans vos applications Delphi.

Récapitulons les points clés :

1. **Ne faites jamais confiance aux entrées utilisateur** : validez, sanitisez et échappez toutes les entrées.

2. **Utilisez des requêtes paramétrées** pour prévenir les injections SQL.

3. **Protégez vos sessions** avec des identifiants sécurisés et des délais d'expiration.

4. **Implémentez des jetons anti-CSRF** pour les opérations sensibles.

5. **Appliquez des limites de débit** pour prévenir les attaques par force brute.

6. **Utilisez des en-têtes de sécurité** pour protéger contre le clickjacking et autres attaques.

7. **Chiffrez les données sensibles** en transit et au repos.

8. **Centralisez votre politique de sécurité** pour une application cohérente.

9. **Journalisez les événements de sécurité** pour détecter et analyser les incidents.

10. **Testez régulièrement** la sécurité de votre application.

Dans le prochain chapitre, nous aborderons l'audit de sécurité, qui vous permettra de vérifier systématiquement que vos applications sont correctement protégées contre les vulnérabilités que nous avons décrites.

### Mise en place d'une liste de contrôle de sécurité

Pour vous aider à vérifier que vous avez bien mis en œuvre toutes les protections nécessaires, voici une liste de contrôle que vous pouvez utiliser pour évaluer la sécurité de vos applications Delphi :

#### Liste de contrôle de sécurité pour les applications Delphi

**Protection contre l'injection SQL :**
- [ ] Toutes les requêtes SQL utilisent des paramètres nommés
- [ ] Les entrées utilisateur sont validées avant utilisation
- [ ] Les erreurs de base de données sont gérées proprement sans divulguer d'informations sensibles
- [ ] Les privilèges de base de données sont limités au minimum nécessaire

**Protection contre le XSS :**
- [ ] Toutes les données affichées dans du HTML sont échappées
- [ ] Des en-têtes de sécurité appropriés sont utilisés (Content-Security-Policy)
- [ ] Les entrées utilisateur sont validées et sanitisées

**Protection contre l'injection de commandes :**
- [ ] L'utilisation de commandes système est évitée si possible
- [ ] Une liste d'autorisations stricte est utilisée pour les commandes permises
- [ ] Les entrées utilisateur sont sanitisées avant utilisation dans des commandes

**Protection contre les attaques par chemin traversant :**
- [ ] Les noms de fichiers sont validés
- [ ] Les chemins sont canonicalisés et vérifiés
- [ ] L'accès aux fichiers est limité à des répertoires spécifiques

**Stockage sécurisé des données sensibles :**
- [ ] Les données sensibles sont chiffrées avant stockage
- [ ] Les clés de chiffrement sont stockées de manière sécurisée
- [ ] Les mots de passe sont hachés avec sel et non chiffrés

**Journalisation et gestion des erreurs :**
- [ ] Les messages d'erreur affichés aux utilisateurs ne révèlent pas d'informations sensibles
- [ ] Les journaux ne contiennent pas de données sensibles
- [ ] Les événements de sécurité importants sont journalisés

**Désérialisation sécurisée :**
- [ ] Les données désérialisées sont validées avant utilisation
- [ ] La taille des données à désérialiser est limitée
- [ ] Des formats de sérialisation sécurisés sont utilisés

**Gestion de la mémoire :**
- [ ] Les limites des tampons sont vérifiées
- [ ] Les classes et types sécurisés de Delphi sont utilisés
- [ ] Les fonctions sécurisées pour la manipulation de chaînes sont utilisées

**Configuration CORS sécurisée :**
- [ ] Une liste d'autorisations stricte est utilisée pour Access-Control-Allow-Origin
- [ ] Les méthodes et en-têtes autorisés sont spécifiés précisément
- [ ] L'utilisation de Access-Control-Allow-Credentials est limitée

**Validation des paramètres côté serveur :**
- [ ] Tous les paramètres sont validés côté serveur
- [ ] Les données sensibles comme les prix sont récupérées depuis la base de données
- [ ] Des limites sont imposées sur les quantités, montants, etc.

**Protection contre CSRF :**
- [ ] Des jetons anti-CSRF sont utilisés pour les opérations sensibles
- [ ] Les jetons sont vérifiés avant l'exécution des actions
- [ ] Les cookies d'authentification utilisent SameSite=Strict

**Protection contre les attaques par force brute :**
- [ ] Une limitation de débit est implémentée
- [ ] Les comptes sont verrouillés temporairement après plusieurs échecs
- [ ] Les tentatives d'authentification sont journalisées

**Gestion des données sensibles dans les URL :**
- [ ] Aucune donnée sensible n'est placée dans les URL
- [ ] Les jetons d'authentification sont transmis via des en-têtes
- [ ] POST est utilisé au lieu de GET pour les opérations sensibles

**Protection contre le clickjacking :**
- [ ] L'en-tête X-Frame-Options est utilisé
- [ ] Content-Security-Policy avec frame-ancestors est configuré
- [ ] D'autres en-têtes de sécurité sont implémentés

**Gestion sécurisée des sessions :**
- [ ] Les identifiants de session sont aléatoires et sécurisés
- [ ] Les sessions ont une durée d'expiration
- [ ] Les cookies de session utilisent HttpOnly, Secure et SameSite
- [ ] Une fonctionnalité de déconnexion est implémentée

### Exemple d'outil d'auto-évaluation de sécurité

Voici un exemple simple d'outil d'auto-évaluation que vous pouvez intégrer à votre projet pour vérifier certaines bonnes pratiques de sécurité :

```pas
unit SecurityAssessment;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils, FireDAC.Comp.Client;

type
  TSecurityCheck = record
    Category: string;
    Name: string;
    Description: string;
    IsPassed: Boolean;
    Recommendation: string;
  end;

  TSecurityAssessment = class
  private
    FChecks: TArray<TSecurityCheck>;
    FDatabase: TFDConnection;

    procedure AddCheck(const Category, Name, Description, Recommendation: string; IsPassed: Boolean);
    function CheckFDQueryParameters: Boolean;
    function CheckPasswordStorage: Boolean;
    function CheckSessionManagement: Boolean;
    function CheckSecureFileHandling: Boolean;
    function CheckInputValidation: Boolean;
    function CheckErrorHandling: Boolean;
    function CheckSensitiveDataExposure: Boolean;
  public
    constructor Create(Database: TFDConnection);

    procedure RunAssessment;
    procedure SaveReport(const FileName: string);
    procedure DisplayReport;

    property Checks: TArray<TSecurityCheck> read FChecks;
  end;

implementation

constructor TSecurityAssessment.Create(Database: TFDConnection);
begin
  inherited Create;
  FDatabase := Database;
  SetLength(FChecks, 0);
end;

procedure TSecurityAssessment.AddCheck(const Category, Name, Description,
  Recommendation: string; IsPassed: Boolean);
var
  Check: TSecurityCheck;
begin
  Check.Category := Category;
  Check.Name := Name;
  Check.Description := Description;
  Check.Recommendation := Recommendation;
  Check.IsPassed := IsPassed;

  SetLength(FChecks, Length(FChecks) + 1);
  FChecks[High(FChecks)] := Check;
end;

procedure TSecurityAssessment.RunAssessment;
begin
  // Réinitialiser les vérifications
  SetLength(FChecks, 0);

  // Exécuter les différentes vérifications
  var HasParamQueries := CheckFDQueryParameters;
  var HasSecurePasswords := CheckPasswordStorage;
  var HasSecureSessions := CheckSessionManagement;
  var HasSecureFiles := CheckSecureFileHandling;
  var HasInputValidation := CheckInputValidation;
  var HasSecureErrorHandling := CheckErrorHandling;
  var HasProtectedSensitiveData := CheckSensitiveDataExposure;

  // Ajouter les résultats
  AddCheck('Injection SQL', 'Requêtes paramétrées',
          'Vérification de l''utilisation de requêtes paramétrées',
          'Utilisez des requêtes paramétrées pour toutes les requêtes SQL',
          HasParamQueries);

  AddCheck('Authentification', 'Stockage des mots de passe',
          'Vérification du hachage des mots de passe',
          'Utilisez des algorithmes de hachage sécurisés avec sel pour les mots de passe',
          HasSecurePasswords);

  AddCheck('Gestion de session', 'Sécurité des sessions',
          'Vérification de la sécurité des sessions',
          'Utilisez des ID de session aléatoires et une expiration de session',
          HasSecureSessions);

  AddCheck('Accès aux fichiers', 'Manipulation sécurisée des fichiers',
          'Vérification de la sécurité des opérations sur les fichiers',
          'Validez les chemins et noms de fichiers pour éviter les attaques par chemin traversant',
          HasSecureFiles);

  AddCheck('Validation d''entrée', 'Validation des entrées utilisateur',
          'Vérification de la validation des entrées',
          'Validez toutes les entrées utilisateur avant utilisation',
          HasInputValidation);

  AddCheck('Gestion des erreurs', 'Traitement sécurisé des erreurs',
          'Vérification de la gestion des erreurs',
          'Ne divulguez pas d''informations sensibles dans les messages d''erreur',
          HasSecureErrorHandling);

  AddCheck('Protection des données', 'Protection des données sensibles',
          'Vérification de la protection des données sensibles',
          'Chiffrez toutes les données sensibles au repos et en transit',
          HasProtectedSensitiveData);

  // D'autres vérifications peuvent être ajoutées...
end;

function TSecurityAssessment.CheckFDQueryParameters: Boolean;
var
  Queries: TArray<TFDQuery>;
  NonParamCount, ParamCount, I: Integer;
begin
  // Cette fonction est un exemple et devrait être adaptée à votre code réel
  // Dans une implémentation réelle, vous pourriez scanner votre code source
  // ou examiner les requêtes en cours d'exécution

  NonParamCount := 0;
  ParamCount := 0;

  // Simuler la recherche de requêtes dans l'application
  // Dans une application réelle, vous pourriez examiner les composants
  // ou scanner le code source
  SetLength(Queries, 0);
  // Obtenez les queries de votre application...

  for I := 0 to High(Queries) do
  begin
    var SQL := Queries[I].SQL.Text;

    // Rechercher des signes de concaténation de chaînes (dangereux)
    if (Pos(''' +', SQL) > 0) or (Pos('+ ''', SQL) > 0) then
      Inc(NonParamCount)
    // Rechercher des signes de paramètres (sécurisé)
    else if (Pos(':param', SQL) > 0) or (Pos('?', SQL) > 0) then
      Inc(ParamCount);
  end;

  // Si nous avons trouvé des requêtes et que plus de 90% utilisent des paramètres,
  // considérer que le test est réussi
  Result := (ParamCount + NonParamCount > 0) and
            (ParamCount / (ParamCount + NonParamCount) >= 0.9);
end;

function TSecurityAssessment.CheckPasswordStorage: Boolean;
var
  TableList: TStringList;
  Query: TFDQuery;
begin
  Result := False;

  if not Assigned(FDatabase) or not FDatabase.Connected then
    Exit;

  TableList := TStringList.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDatabase;

    // Obtenir la liste des tables
    FDatabase.GetTableNames('', '', '', TableList);

    // Chercher une table d'utilisateurs
    for var Table in TableList do
    begin
      if (LowerCase(Table) = 'users') or
         (LowerCase(Table) = 'utilisateurs') or
         (LowerCase(Table) = 'user') then
      begin
        // Vérifier la structure de la table
        Query.SQL.Text := 'SELECT * FROM ' + Table + ' LIMIT 1';
        try
          Query.Open;

          // Vérifier si nous avons un champ de mot de passe qui semble être haché
          for var I := 0 to Query.FieldCount - 1 do
          begin
            var FieldName := LowerCase(Query.Fields[I].FieldName);

            if (FieldName = 'password') or (FieldName = 'mot_de_passe') then
            begin
              Result := False; // Un champ 'password' simple est suspect
              Exit;
            end
            else if (FieldName = 'password_hash') or
                    (FieldName = 'hashed_password') or
                    (FieldName = 'mot_de_passe_hash') then
            begin
              Result := True; // Un champ avec 'hash' semble plus sécurisé
              Exit;
            end;
          end;
        except
          // Ignorer les erreurs
        end;
      end;
    end;
  finally
    Query.Free;
    TableList.Free;
  end;
end;

// Implémentez les autres méthodes de vérification similaires...

procedure TSecurityAssessment.SaveReport(const FileName: string);
var
  Report: TJSONArray;
  Check: TJSONObject;
  I: Integer;
begin
  Report := TJSONArray.Create;
  try
    for I := 0 to High(FChecks) do
    begin
      Check := TJSONObject.Create;
      Check.AddPair('category', FChecks[I].Category);
      Check.AddPair('name', FChecks[I].Name);
      Check.AddPair('description', FChecks[I].Description);
      Check.AddPair('passed', TJSONBool.Create(FChecks[I].IsPassed));
      Check.AddPair('recommendation', FChecks[I].Recommendation);

      Report.Add(Check);
    end;

    TFile.WriteAllText(FileName, Report.ToString);
  finally
    Report.Free;
  end;
end;

procedure TSecurityAssessment.DisplayReport;
var
  TotalChecks, PassedChecks, I: Integer;
begin
  TotalChecks := Length(FChecks);
  PassedChecks := 0;

  WriteLn('=== RAPPORT D''ÉVALUATION DE SÉCURITÉ ===');
  WriteLn('');

  for I := 0 to High(FChecks) do
  begin
    if FChecks[I].IsPassed then
    begin
      WriteLn('[SUCCÈS] ', FChecks[I].Category, ' - ', FChecks[I].Name);
      Inc(PassedChecks);
    end
    else
    begin
      WriteLn('[ÉCHEC] ', FChecks[I].Category, ' - ', FChecks[I].Name);
      WriteLn('  Description: ', FChecks[I].Description);
      WriteLn('  Recommandation: ', FChecks[I].Recommendation);
    end;

    WriteLn('');
  end;

  WriteLn('Résumé: ', PassedChecks, ' succès sur ', TotalChecks, ' vérifications');
  WriteLn('Score de sécurité: ', Round(PassedChecks / TotalChecks * 100), '%');

  if PassedChecks = TotalChecks then
    WriteLn('Toutes les vérifications ont été passées avec succès !')
  else
    WriteLn('Des problèmes de sécurité ont été détectés. Veuillez consulter les recommandations.');
end;

end.
```

### Utilisation de l'outil d'auto-évaluation

```pas
procedure TMainForm.ButtonSecurityAssessmentClick(Sender: TObject);
var
  Assessment: TSecurityAssessment;
begin
  Assessment := TSecurityAssessment.Create(DataModule1.FDConnection1);
  try
    // Exécuter l'évaluation
    Assessment.RunAssessment;

    // Afficher les résultats dans un mémo
    MemoResult.Clear;

    for var Check in Assessment.Checks do
    begin
      if Check.IsPassed then
        MemoResult.Lines.Add('[SUCCÈS] ' + Check.Category + ' - ' + Check.Name)
      else
      begin
        MemoResult.Lines.Add('[ÉCHEC] ' + Check.Category + ' - ' + Check.Name);
        MemoResult.Lines.Add('  Description: ' + Check.Description);
        MemoResult.Lines.Add('  Recommandation: ' + Check.Recommendation);
      end;

      MemoResult.Lines.Add('');
    end;

    // Sauvegarder le rapport
    Assessment.SaveReport(ChangeFileExt(Application.ExeName, '_security_report.json'));

    ShowMessage('Évaluation de sécurité terminée. Un rapport détaillé a été sauvegardé.');
  finally
    Assessment.Free;
  end;
end;
```

### Outils tiers pour la sécurité

En plus des pratiques de programmation sécurisée, vous pouvez utiliser des outils tiers pour renforcer la sécurité de vos applications Delphi :

#### 1. Analyseurs statiques de code

Ces outils analysent votre code sans l'exécuter pour identifier les vulnérabilités potentielles :

- **Sonar** : Peut analyser le code Delphi pour identifier les problèmes de sécurité.
- **RAD Auditor** : Spécifiquement conçu pour les applications Delphi.
- **CodeSonar** : Un analyseur de code statique avancé qui prend en charge plusieurs langages, dont Delphi.

#### 2. Outils de protection binaire

Ces outils protègent votre application une fois compilée :

- **VMProtect** : Protège le code par virtualisation pour empêcher le reverse engineering.
- **Themida** : Offre une protection avancée contre la rétro-ingénierie.
- **ASProtect** : Protège contre la décompilation et le débogage non autorisé.

#### 3. Scanners de vulnérabilités

Pour tester votre application déployée :

- **OWASP ZAP** : Un scanner de vulnérabilités open source pour les applications web.
- **Nessus** : Un scanner de vulnérabilités qui peut tester les applications desktop et serveur.
- **Acunetix** : Particulièrement utile pour tester les applications web créées avec Delphi.

#### 4. Bibliothèques de sécurité pour Delphi

Des composants spécialisés pour renforcer la sécurité :

- **TurboPower LockBox** : Une bibliothèque de cryptographie pour Delphi.
- **SecureBlackbox** : Fournit des composants pour SSL/TLS, SFTP, chiffrement, et plus.
- **Spring4D Cryptography** : Partie de la bibliothèque Spring4D, offre des fonctionnalités cryptographiques modernes.

### Exemple : Intégration d'un analyseur statique dans votre processus de développement

Voici un exemple de script batch qui peut être exécuté comme une étape de pré-compilation pour analyser votre code Delphi :

```batch
@echo off
echo Exécution de l'analyse de sécurité...

set PROJ_DIR=%1
set SOURCE_DIR=%PROJ_DIR%\src
set REPORT_DIR=%PROJ_DIR%\security_reports

if not exist "%REPORT_DIR%" mkdir "%REPORT_DIR%"

echo Analysing du code dans %SOURCE_DIR%...

rem Exemple avec un analyseur de code fictif
SecurityAnalyzer.exe --src="%SOURCE_DIR%" --report="%REPORT_DIR%\security_report.html" --rules=injection,xss,path_traversal

if %ERRORLEVEL% GEQ 1 (
    echo Des problèmes de sécurité ont été détectés! Veuillez consulter le rapport.
    start "" "%REPORT_DIR%\security_report.html"
    exit /b 1
) else (
    echo Aucun problème de sécurité critique détecté.
    exit /b 0
)
```

### Recommandations finales pour la sécurité des applications Delphi

1. **Adoptez une approche de "sécurité dès la conception"** : Intégrez la sécurité dès le début du cycle de développement, pas comme une réflexion après coup.

2. **Formez votre équipe** : Assurez-vous que tous les développeurs comprennent les principes de base de la sécurité des applications.

3. **Testez régulièrement** : Effectuez des tests de sécurité réguliers, y compris des tests de pénétration par des experts externes.

4. **Restez informé** : Suivez les dernières tendances et menaces en matière de sécurité, comme les bulletins OWASP Top 10.

5. **Adoptez le principe du moindre privilège** : Accordez le minimum de droits nécessaires à tous les niveaux de votre application.

6. **Mettez en place un processus de gestion des vulnérabilités** : Disposez d'un système pour traiter les vulnérabilités découvertes dans votre application.

7. **Documentation** : Documentez vos pratiques de sécurité et créez des guides pour les futurs développeurs.

8. **Automatisez les tests de sécurité** : Intégrez les tests de sécurité automatisés dans votre pipeline de CI/CD.

### Exercices pratiques

1. Créez une fonction de validation qui vérifie si une entrée utilisateur contient des caractères dangereux pour SQL, HTML et les chemins de fichier.

2. Modifiez une application existante pour implémenter des jetons anti-CSRF pour tous les formulaires sensibles.

3. Créez une classe de session sécurisée qui génère des identifiants aléatoires et gère l'expiration des sessions.

4. Ajoutez une limitation de débit à une fonction d'authentification pour prévenir les attaques par force brute.

5. Écrivez un outil simple qui scanne votre code Delphi à la recherche de vulnérabilités potentielles, comme des requêtes SQL non paramétrées.

6. Implémentez une gestion sécurisée des erreurs qui journalise les détails pour les développeurs mais affiche des messages génériques aux utilisateurs.

7. Pour les plus avancés : Créez un middleware de sécurité qui applique automatiquement des en-têtes de sécurité à toutes les réponses HTTP de votre application.

En mettant en œuvre ces recommandations et en utilisant les outils appropriés, vous pouvez considérablement renforcer la sécurité de vos applications Delphi contre les vulnérabilités courantes.

⏭️ [Audit de sécurité](/16-securite-des-applications/06-audit-de-securite.md)
