🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.5 Protection contre les vulnérabilités courantes

## Introduction

Les vulnérabilités sont des failles de sécurité dans votre application que des attaquants peuvent exploiter pour voler des données, prendre le contrôle du système ou causer des dommages. Même les développeurs expérimentés peuvent introduire des vulnérabilités sans s'en rendre compte.

**Analogie du monde réel** : Une vulnérabilité, c'est comme laisser une fenêtre ouverte dans une maison bien fermée. Même si vous avez verrouillé toutes les portes (authentification, chiffrement), cette petite fenêtre ouverte suffit à un cambrioleur pour entrer.

### Pourquoi ces vulnérabilités existent-elles ?

Les vulnérabilités apparaissent généralement pour ces raisons :
- **Faire confiance aux données utilisateur** : Croire que l'utilisateur va toujours entrer des données valides
- **Manque de validation** : Ne pas vérifier les entrées avant de les utiliser
- **Mauvaise gestion des erreurs** : Révéler trop d'informations dans les messages d'erreur
- **Code hérité** : Anciennes pratiques qui n'étaient pas sécurisées
- **Pression temporelle** : Rush pour terminer sans penser à la sécurité

## Le OWASP Top 10

L'OWASP (Open Web Application Security Project) publie régulièrement une liste des 10 vulnérabilités les plus critiques. Voici celles que nous allons aborder :

1. **Injection** (SQL, commandes, etc.)
2. **Broken Authentication** (authentification cassée)
3. **Sensitive Data Exposure** (exposition de données sensibles)
4. **XML External Entities (XXE)**
5. **Broken Access Control** (contrôle d'accès cassé)
6. **Security Misconfiguration** (mauvaise configuration)
7. **Cross-Site Scripting (XSS)**
8. **Insecure Deserialization** (désérialisation non sécurisée)
9. **Using Components with Known Vulnerabilities** (composants vulnérables)
10. **Insufficient Logging & Monitoring** (journalisation insuffisante)

## 1. Injection SQL

### Qu'est-ce qu'une injection SQL ?

C'est la vulnérabilité **N°1** la plus dangereuse. Un attaquant insère du code SQL malveillant dans vos requêtes pour accéder, modifier ou détruire vos données.

**Exemple de scénario** :
```
Utilisateur entre : admin  
Mot de passe : ' OR '1'='1  

Requête générée :  
SELECT * FROM Users WHERE username = 'admin' AND password = '' OR '1'='1'  

Résultat : La condition '1'='1' est toujours vraie, donc l'attaquant est connecté !
```

### Code vulnérable (à NE JAMAIS FAIRE)

```pascal
// ❌ TRÈS DANGEREUX - Vulnérable aux injections SQL
procedure TFormLogin.BtnConnexionClick(Sender: TObject);  
var  
  Query: TFDQuery;
  SQL: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Construction directe de la requête avec les entrées utilisateur
    SQL := 'SELECT * FROM Users WHERE Username = ''' + EditUsername.Text +
           ''' AND Password = ''' + EditPassword.Text + '''';

    Query.SQL.Text := SQL;
    Query.Open;

    if not Query.IsEmpty then
      ShowMessage('Connexion réussie')
    else
      ShowMessage('Identifiants incorrects');
  finally
    Query.Free;
  end;
end;
```

**Pourquoi c'est dangereux ?**

Si l'utilisateur entre : `'; DROP TABLE Users; --`

La requête devient :
```sql
SELECT * FROM Users WHERE Username = ''; DROP TABLE Users; --' AND Password = ''
```

Résultat : **Toute la table Users est supprimée !**

### Solution : Requêtes paramétrées

```pascal
// ✅ SÉCURISÉ - Utilise des paramètres
procedure TFormLogin.BtnConnexionClickSecurise(Sender: TObject);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Utilisation de paramètres nommés
    Query.SQL.Text := 'SELECT * FROM Users WHERE Username = :Username AND Password = :Password';

    // Les paramètres sont automatiquement échappés
    Query.ParamByName('Username').AsString := EditUsername.Text;
    Query.ParamByName('Password').AsString := HashMotDePasse(EditPassword.Text);

    Query.Open;

    if not Query.IsEmpty then
      ShowMessage('Connexion réussie')
    else
      ShowMessage('Identifiants incorrects');
  finally
    Query.Free;
  end;
end;
```

**Pourquoi c'est sécurisé ?**

Les paramètres sont traités comme des **données**, jamais comme du **code SQL**. Même si l'utilisateur entre du SQL malveillant, il sera simplement cherché comme une chaîne de caractères.

### Autres exemples de requêtes paramétrées

```pascal
// INSERT sécurisé
procedure AjouterUtilisateurSecurise(const ANom, AEmail: string);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'INSERT INTO Users (Nom, Email) VALUES (:Nom, :Email)';
    Query.ParamByName('Nom').AsString := ANom;
    Query.ParamByName('Email').AsString := AEmail;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// UPDATE sécurisé
procedure ModifierUtilisateurSecurise(AID: Integer; const ANom: string);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'UPDATE Users SET Nom = :Nom WHERE ID = :ID';
    Query.ParamByName('Nom').AsString := ANom;
    Query.ParamByName('ID').AsInteger := AID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// DELETE sécurisé
procedure SupprimerUtilisateurSecurise(AID: Integer);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'DELETE FROM Users WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := AID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// SELECT avec clause IN sécurisée
procedure ChercherUtilisateursParIDs(const AIDs: array of Integer);  
var  
  Query: TFDQuery;
  i: Integer;
  Params: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Construire les paramètres dynamiquement
    Params := '';
    for i := Low(AIDs) to High(AIDs) do
    begin
      if i > Low(AIDs) then
        Params := Params + ', ';
      Params := Params + ':ID' + IntToStr(i);
    end;

    Query.SQL.Text := 'SELECT * FROM Users WHERE ID IN (' + Params + ')';

    // Assigner les valeurs
    for i := Low(AIDs) to High(AIDs) do
      Query.ParamByName('ID' + IntToStr(i)).AsInteger := AIDs[i];

    Query.Open;

    // Traiter les résultats
    while not Query.Eof do
    begin
      // ...
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;
```

### Validation supplémentaire

En plus des paramètres, validez toujours les entrées :

```pascal
function ValiderNomUtilisateur(const AUsername: string): Boolean;  
begin  
  Result := False;

  // Vérifier la longueur
  if (Length(AUsername) < 3) or (Length(AUsername) > 50) then
    Exit;

  // Vérifier les caractères autorisés (lettres, chiffres, underscore)
  if not TRegEx.IsMatch(AUsername, '^[a-zA-Z0-9_]+$') then
    Exit;

  Result := True;
end;

procedure TFormInscription.BtnInscrireClick(Sender: TObject);  
begin  
  if not ValiderNomUtilisateur(EditUsername.Text) then
  begin
    ShowMessage('Nom d''utilisateur invalide. ' +
                'Utilisez uniquement des lettres, chiffres et underscores (3-50 caractères).');
    Exit;
  end;

  // Continuer l'inscription...
end;
```

## 2. Cross-Site Scripting (XSS)

### Qu'est-ce que le XSS ?

Le XSS permet à un attaquant d'injecter du code JavaScript malveillant dans votre application web, qui sera exécuté dans le navigateur d'autres utilisateurs.

**Scénario** : Un forum où les utilisateurs peuvent poster des messages.

```pascal
// ❌ VULNÉRABLE
procedure AfficherMessage(const AMessage: string);  
begin  
  // Affiche directement le HTML
  WebBrowser1.Navigate('about:blank');
  (WebBrowser1.Document as IHTMLDocument2).write(
    '<html><body>' + AMessage + '</body></html>'
  );
end;

// Si un utilisateur poste :
// <script>alert('XSS!');</script>
// Le JavaScript sera exécuté !
```

### Types de XSS

**1. XSS Stocké (Stored)** : Le code malveillant est stocké en base de données
```
Attaquant poste : <script>volerCookies();</script>
→ Stocké en base
→ Tous les visiteurs exécutent ce code
```

**2. XSS Réfléchi (Reflected)** : Le code malveillant est dans l'URL
```
URL : http://site.com/search?q=<script>alert('XSS')</script>
→ Le script est affiché et exécuté immédiatement
```

**3. XSS DOM** : Le code malveillant manipule le DOM directement

### Protection contre le XSS

**Solution 1 : Échapper le HTML**

```pascal
uses
  System.NetEncoding;

function EchapperHTML(const ATexte: string): string;  
begin  
  Result := ATexte;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#x27;', [rfReplaceAll]);
end;

// ✅ SÉCURISÉ
procedure AfficherMessageSecurise(const AMessage: string);  
var  
  MessageEchappe: string;
begin
  MessageEchappe := EchapperHTML(AMessage);

  WebBrowser1.Navigate('about:blank');
  (WebBrowser1.Document as IHTMLDocument2).write(
    '<html><body>' + MessageEchappe + '</body></html>'
  );
end;

// Maintenant, <script>alert('XSS');</script>
// devient : &lt;script&gt;alert('XSS');&lt;/script&gt;
// et s'affiche comme texte au lieu d'être exécuté
```

**Solution 2 : Utiliser TNetEncoding**

```pascal
uses
  System.NetEncoding;

function EncoderPourHTML(const ATexte: string): string;  
begin  
  Result := TNetEncoding.HTML.Encode(ATexte);
end;
```

**Solution 3 : Content Security Policy (CSP)**

Pour les applications web servies par Delphi :

```pascal
procedure ConfigurerHeadersSecurite;  
begin  
  // Ajouter un header CSP qui interdit les scripts inline
  Response.SetCustomHeader('Content-Security-Policy',
    'default-src ''self''; script-src ''self'' https://cdnjs.cloudflare.com');
end;
```

### Validation des entrées

```pascal
function FiltrerBalises(const ATexte: string): string;  
var  
  BaliseAutorisees: array of string;
begin
  // Liste blanche de balises autorisées
  BaliseAutorisees := ['<b>', '</b>', '<i>', '</i>', '<br>'];

  // Supprimer toutes les balises sauf celles autorisées
  Result := ATexte;

  // Supprimer d'abord tous les scripts
  Result := TRegEx.Replace(Result, '<script[^>]*>.*?</script>', '', [roIgnoreCase]);
  Result := TRegEx.Replace(Result, '<iframe[^>]*>.*?</iframe>', '', [roIgnoreCase]);

  // Pour une protection maximale, utiliser une bibliothèque de sanitisation HTML
end;
```

## 3. Cross-Site Request Forgery (CSRF)

### Qu'est-ce que le CSRF ?

Le CSRF force un utilisateur authentifié à exécuter des actions non désirées sur une application web.

**Scénario** :
```
1. Vous êtes connecté à votre banque (cookie de session valide)
2. Un attaquant vous envoie un email avec un lien
3. Le lien pointe vers : http://votre-banque.com/transfert?montant=1000&vers=attaquant
4. Si vous cliquez, le transfert est exécuté car vous êtes authentifié !
```

### Protection contre le CSRF

**Solution : Tokens CSRF**

```pascal
type
  TCSRFManager = class
  private
    class var FTokens: TDictionary<string, TDateTime>;
  public
    class constructor Create;
    class destructor Destroy;
    class function GenererToken(const ASessionID: string): string;
    class function ValiderToken(const ASessionID, AToken: string): Boolean;
  end;

class constructor TCSRFManager.Create;  
begin  
  FTokens := TDictionary<string, TDateTime>.Create;
end;

class destructor TCSRFManager.Destroy;  
begin  
  FTokens.Free;
end;

class function TCSRFManager.GenererToken(const ASessionID: string): string;  
var  
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);

  // Stocker le token avec timestamp
  FTokens.AddOrSetValue(ASessionID + '_' + Result, Now);
end;

class function TCSRFManager.ValiderToken(const ASessionID, AToken: string): Boolean;  
var  
  Cle: string;
  DateCreation: TDateTime;
begin
  Result := False;
  Cle := ASessionID + '_' + AToken;

  if FTokens.TryGetValue(Cle, DateCreation) then
  begin
    // Le token est valide pendant 1 heure
    Result := MinutesBetween(Now, DateCreation) < 60;

    // Utiliser une seule fois (supprimer après validation)
    if Result then
      FTokens.Remove(Cle);
  end;
end;

// Utilisation dans un formulaire
procedure TFormAction.FormCreate(Sender: TObject);  
begin  
  // Générer un token CSRF pour ce formulaire
  FCSRFToken := TCSRFManager.GenererToken(SessionID);

  // L'inclure dans un champ caché
  HiddenCSRFToken.Value := FCSRFToken;
end;

procedure TFormAction.BtnSoumettreClick(Sender: TObject);  
var  
  TokenRecu: string;
begin
  TokenRecu := HiddenCSRFToken.Value;

  // Vérifier le token avant d'exécuter l'action
  if not TCSRFManager.ValiderToken(SessionID, TokenRecu) then
  begin
    ShowMessage('Erreur : Token CSRF invalide. Action refusée.');
    Exit;
  end;

  // Token valide, exécuter l'action
  ExecuterActionSensible;
end;
```

**Autres protections CSRF** :

1. **Vérifier le referer**
```pascal
function VerifierReferer(const ARefererAttendu: string): Boolean;  
var  
  Referer: string;
begin
  Referer := Request.GetFieldByName('Referer');
  Result := Pos(ARefererAttendu, Referer) > 0;
end;
```

2. **Double Submit Cookie**
```pascal
// Stocker le token dans un cookie ET dans le formulaire
// Les comparer lors de la soumission
```

3. **SameSite Cookie**
```pascal
// Configurer les cookies avec l'attribut SameSite
Response.SetCookie('session_id', SessionID, 0, '/', '', True, True, 'Strict');
```

## 4. Validation des entrées

### Règle d'or : Ne JAMAIS faire confiance aux entrées utilisateur

**Principe de la liste blanche** : N'accepter que ce qui est explicitement autorisé.

```pascal
// ❌ MAUVAIS - Liste noire (interdire certains caractères)
function ValiderNomListeNoire(const ANom: string): Boolean;  
begin  
  // Trop facile à contourner
  Result := (Pos('<', ANom) = 0) and (Pos('>', ANom) = 0);
end;

// ✅ BON - Liste blanche (autoriser seulement certains caractères)
function ValiderNomListeBlanche(const ANom: string): Boolean;  
begin  
  // Seulement lettres, espaces, tirets et apostrophes
  Result := TRegEx.IsMatch(ANom, '^[a-zA-ZÀ-ÿ \-'']+$');
end;
```

### Types de validation

**1. Validation de format**

```pascal
uses
  System.RegularExpressions;

function ValiderEmail(const AEmail: string): Boolean;  
const  
  REGEX_EMAIL = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
begin
  Result := TRegEx.IsMatch(AEmail, REGEX_EMAIL);
end;

function ValiderTelephone(const ATel: string): Boolean;  
const  
  REGEX_TEL_FR = '^0[1-9](\d{2}){4}$'; // Format français
var
  TelNettoye: string;
begin
  // Supprimer les espaces et tirets
  TelNettoye := StringReplace(ATel, ' ', '', [rfReplaceAll]);
  TelNettoye := StringReplace(TelNettoye, '-', '', [rfReplaceAll]);
  Result := TRegEx.IsMatch(TelNettoye, REGEX_TEL_FR);
end;

function ValiderCodePostal(const ACodePostal: string): Boolean;  
const  
  REGEX_CP_FR = '^\d{5}$';
begin
  Result := TRegEx.IsMatch(ACodePostal, REGEX_CP_FR);
end;

function ValiderURL(const AURL: string): Boolean;  
const  
  REGEX_URL = '^https?://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,}(/.*)?$';
begin
  Result := TRegEx.IsMatch(AURL, REGEX_URL);
end;
```

**2. Validation de plage**

```pascal
function ValiderAge(AAge: Integer): Boolean;  
begin  
  Result := (AAge >= 0) and (AAge <= 150);
end;

function ValiderMontant(AMontant: Currency): Boolean;  
begin  
  Result := (AMontant >= 0) and (AMontant <= 1000000);
end;

function ValiderDate(ADate: TDate): Boolean;  
begin  
  // Date entre 1900 et aujourd'hui
  Result := (ADate >= EncodeDate(1900, 1, 1)) and (ADate <= Date);
end;
```

**3. Validation de longueur**

```pascal
function ValiderLongueurTexte(const ATexte: string; AMin, AMax: Integer): Boolean;  
begin  
  Result := (Length(ATexte) >= AMin) and (Length(ATexte) <= AMax);
end;

function ValiderCommentaire(const ACommentaire: string): Boolean;  
begin  
  // Entre 10 et 1000 caractères
  Result := ValiderLongueurTexte(ACommentaire, 10, 1000);
end;
```

**4. Validation de type**

```pascal
function EstUnEntier(const ATexte: string): Boolean;  
var  
  Valeur: Integer;
begin
  Result := TryStrToInt(ATexte, Valeur);
end;

function EstUnDecimal(const ATexte: string): Boolean;  
var  
  Valeur: Double;
begin
  Result := TryStrToFloat(ATexte, Valeur);
end;

function EstUneDate(const ATexte: string): Boolean;  
var  
  Valeur: TDate;
begin
  Result := TryStrToDate(ATexte, Valeur);
end;
```

### Classe de validation réutilisable

```pascal
type
  TValidateur = class
  public
    class function Email(const AEmail: string): Boolean;
    class function Telephone(const ATel: string): Boolean;
    class function CodePostal(const ACP: string): Boolean;
    class function NomUtilisateur(const AUsername: string): Boolean;
    class function MotDePasse(const APassword: string): Boolean;
    class function URL(const AURL: string): Boolean;
    class function Plage(AValeur, AMin, AMax: Integer): Boolean;
  end;

class function TValidateur.Email(const AEmail: string): Boolean;  
begin  
  Result := TRegEx.IsMatch(AEmail, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

class function TValidateur.NomUtilisateur(const AUsername: string): Boolean;  
begin  
  // 3-20 caractères, lettres, chiffres et underscore uniquement
  Result := TRegEx.IsMatch(AUsername, '^[a-zA-Z0-9_]{3,20}$');
end;

class function TValidateur.MotDePasse(const APassword: string): Boolean;  
var  
  AMajuscule, AMinuscule, AChiffre, ASpecial: Boolean;
  i: Integer;
begin
  Result := False;

  // Minimum 8 caractères
  if Length(APassword) < 8 then
    Exit;

  AMajuscule := False;
  AMinuscule := False;
  AChiffre := False;
  ASpecial := False;

  for i := 1 to Length(APassword) do
  begin
    if CharInSet(APassword[i], ['A'..'Z']) then
      AMajuscule := True
    else if CharInSet(APassword[i], ['a'..'z']) then
      AMinuscule := True
    else if CharInSet(APassword[i], ['0'..'9']) then
      AChiffre := True
    else
      ASpecial := True;
  end;

  // Doit contenir au moins 3 types de caractères sur 4
  Result := (Ord(AMajuscule) + Ord(AMinuscule) + Ord(AChiffre) + Ord(ASpecial)) >= 3;
end;

class function TValidateur.Plage(AValeur, AMin, AMax: Integer): Boolean;  
begin  
  Result := (AValeur >= AMin) and (AValeur <= AMax);
end;

// Utilisation
procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  if not TValidateur.Email(EditEmail.Text) then
  begin
    ShowMessage('Email invalide');
    Exit;
  end;

  if not TValidateur.NomUtilisateur(EditUsername.Text) then
  begin
    ShowMessage('Nom d''utilisateur invalide (3-20 caractères alphanumériques)');
    Exit;
  end;

  if not TValidateur.MotDePasse(EditPassword.Text) then
  begin
    ShowMessage('Mot de passe trop faible (minimum 8 caractères avec majuscules, minuscules, chiffres)');
    Exit;
  end;

  // Toutes les validations passées
  InscrireUtilisateur;
end;
```

## 5. Gestion sécurisée des erreurs

### Le problème

Les messages d'erreur trop détaillés peuvent révéler des informations sensibles aux attaquants.

```pascal
// ❌ DANGEREUX - Révèle trop d'informations
try
  Query.SQL.Text := 'SELECT * FROM Users WHERE ID = :ID';
  Query.ParamByName('ID').AsInteger := StrToInt(EditID.Text);
  Query.Open;
except
  on E: Exception do
    ShowMessage('Erreur : ' + E.Message);
    // Affiche : "Table 'mydb.Users' doesn't exist"
    // L'attaquant sait maintenant le nom de la base et de la table !
end;
```

### Solution : Messages génériques + Logs détaillés

```pascal
type
  TLoggerSecurite = class
  public
    class procedure LoggerErreur(const AMessage, ADetails: string);
  end;

class procedure TLoggerSecurite.LoggerErreur(const AMessage, ADetails: string);  
var  
  Fichier: TextFile;
  Ligne: string;
begin
  AssignFile(Fichier, 'logs\errors.log');
  try
    if FileExists('logs\errors.log') then
      Append(Fichier)
    else
      Rewrite(Fichier);

    Ligne := Format('[%s] %s - %s', [DateTimeToStr(Now), AMessage, ADetails]);
    WriteLn(Fichier, Ligne);
  finally
    CloseFile(Fichier);
  end;
end;

// ✅ SÉCURISÉ - Message générique à l'utilisateur, log détaillé
procedure ExecuterRequeteSecurisee;  
begin  
  try
    Query.SQL.Text := 'SELECT * FROM Users WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := StrToInt(EditID.Text);
    Query.Open;
  except
    on E: Exception do
    begin
      // Message générique à l'utilisateur
      ShowMessage('Une erreur est survenue. Veuillez réessayer ou contacter le support.');

      // Log détaillé pour les développeurs (jamais montré à l'utilisateur)
      TLoggerSecurite.LoggerErreur('Erreur base de données',
        Format('Classe: %s, Message: %s, Query: %s',
               [E.ClassName, E.Message, Query.SQL.Text]));
    end;
  end;
end;
```

### Codes d'erreur vs messages détaillés

```pascal
const
  ERR_DB_CONNECTION = 1001;
  ERR_DB_QUERY = 1002;
  ERR_VALIDATION = 2001;
  ERR_AUTH = 3001;

function ObtenirMessageErreur(ACode: Integer): string;  
begin  
  case ACode of
    ERR_DB_CONNECTION: Result := 'Impossible de se connecter au serveur';
    ERR_DB_QUERY: Result := 'Erreur lors de la récupération des données';
    ERR_VALIDATION: Result := 'Les données saisies sont invalides';
    ERR_AUTH: Result := 'Échec de l''authentification';
  else
    Result := 'Une erreur inconnue est survenue';
  end;
end;

procedure TraiterErreur(ACode: Integer; const ADetailsInternes: string);  
begin  
  // Message utilisateur
  ShowMessage(ObtenirMessageErreur(ACode));

  // Log détaillé
  TLoggerSecurite.LoggerErreur(
    Format('Code erreur: %d', [ACode]),
    ADetailsInternes
  );
end;
```

## 6. Upload de fichiers sécurisé

### Vulnérabilités liées aux uploads

- Upload de fichiers malveillants (virus, malware)
- Upload de scripts exécutables (.php, .exe)
- Déni de service (fichiers énormes)
- Path traversal (../../system32)

### Protection de l'upload

```pascal
type
  TUploadSecurise = class
  private
    const
      TAILLE_MAX_FICHIER = 10 * 1024 * 1024; // 10 Mo
      EXTENSIONS_AUTORISEES: array[0..4] of string =
        ('.jpg', '.jpeg', '.png', '.gif', '.pdf');
  public
    class function ValiderFichier(const ANomFichier: string; ATaille: Int64): Boolean;
    class function GenererNomSecurise: string;
    class function ScannerVirus(const ACheminFichier: string): Boolean;
  end;

class function TUploadSecurise.ValiderFichier(const ANomFichier: string; ATaille: Int64): Boolean;  
var  
  Extension: string;
  i: Integer;
  ExtensionAutorisee: Boolean;
begin
  Result := False;

  // Vérifier la taille
  if ATaille > TAILLE_MAX_FICHIER then
  begin
    ShowMessage('Fichier trop volumineux (maximum 10 Mo)');
    Exit;
  end;

  if ATaille = 0 then
  begin
    ShowMessage('Fichier vide');
    Exit;
  end;

  // Vérifier l'extension
  Extension := LowerCase(ExtractFileExt(ANomFichier));
  ExtensionAutorisee := False;

  for i := Low(EXTENSIONS_AUTORISEES) to High(EXTENSIONS_AUTORISEES) do
  begin
    if Extension = EXTENSIONS_AUTORISEES[i] then
    begin
      ExtensionAutorisee := True;
      Break;
    end;
  end;

  if not ExtensionAutorisee then
  begin
    ShowMessage('Type de fichier non autorisé. Extensions acceptées : jpg, jpeg, png, gif, pdf');
    Exit;
  end;

  // Vérifier qu'il n'y a pas de path traversal
  if (Pos('..', ANomFichier) > 0) or (Pos('/', ANomFichier) > 0) or (Pos('\', ANomFichier) > 0) then
  begin
    ShowMessage('Nom de fichier invalide');
    Exit;
  end;

  Result := True;
end;

class function TUploadSecurise.GenererNomSecurise: string;  
var  
  GUID: TGUID;
begin
  // Générer un nom unique pour éviter les collisions et les attaques
  CreateGUID(GUID);
  Result := StringReplace(GUIDToString(GUID), '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

class function TUploadSecurise.ScannerVirus(const ACheminFichier: string): Boolean;  
begin  
  // Intégrer avec un antivirus (ClamAV, Windows Defender, etc.)
  // Pour l'exemple, on suppose que c'est propre
  Result := True;

  // Implémentation réelle nécessiterait une API antivirus
end;

// Utilisation
procedure TForm1.UploadFichier;  
var  
  NomOriginal: string;
  NomSecurise: string;
  CheminDestination: string;
  Extension: string;
begin
  if OpenDialog1.Execute then
  begin
    NomOriginal := ExtractFileName(OpenDialog1.FileName);

    // Valider le fichier
    if not TUploadSecurise.ValiderFichier(NomOriginal,
                                           GetFileSize(OpenDialog1.FileName)) then
      Exit;

    // Générer un nom sécurisé
    Extension := ExtractFileExt(NomOriginal);
    NomSecurise := TUploadSecurise.GenererNomSecurise + Extension;

    // Définir le chemin de destination (hors de la racine web si possible)
    CheminDestination := TPath.Combine(CheminUploads, NomSecurise);

    // Copier le fichier
    TFile.Copy(OpenDialog1.FileName, CheminDestination);

    // Scanner pour les virus
    if not TUploadSecurise.ScannerVirus(CheminDestination) then
    begin
      TFile.Delete(CheminDestination);
      ShowMessage('Fichier suspect détecté et supprimé');
      Exit;
    end;

    // Enregistrer en base avec le nom original et le nom sécurisé
    EnregistrerFichierEnBase(NomOriginal, NomSecurise);

    ShowMessage('Fichier uploadé avec succès');
  end;
end;

function GetFileSize(const AFileName: string): Int64;  
var  
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FileStream.Size;
  finally
    FileStream.Free;
  end;
end;
```

## 7. Protection contre les attaques par force brute

### Le problème

Un attaquant essaie de nombreuses combinaisons de mots de passe pour trouver le bon.

### Solution : Rate limiting et blocage temporaire

```pascal
type
  TProtectionForceBrute = class
  private
    class var FTentatives: TDictionary<string, TList<TDateTime>>;
  public
    class constructor Create;
    class destructor Destroy;
    class function PeutTenterConnexion(const AUsername: string): Boolean;
    class procedure EnregistrerTentativeEchouee(const AUsername: string);
    class procedure ReinitialiserTentatives(const AUsername: string);
  end;

class constructor TProtectionForceBrute.Create;  
begin  
  FTentatives := TDictionary<string, TList<TDateTime>>.Create;
end;

class destructor TProtectionForceBrute.Destroy;  
var  
  Liste: TList<TDateTime>;
begin
  for Liste in FTentatives.Values do
    Liste.Free;
  FTentatives.Free;
end;

class function TProtectionForceBrute.PeutTenterConnexion(const AUsername: string): Boolean;  
const  
  MAX_TENTATIVES = 5;
  FENETRE_MINUTES = 15;
var
  Liste: TList<TDateTime>;
  i: Integer;
  TentativesRecentes: Integer;
begin
  Result := True;

  if not FTentatives.TryGetValue(AUsername, Liste) then
    Exit; // Première tentative

  // Compter les tentatives dans les dernières FENETRE_MINUTES minutes
  TentativesRecentes := 0;
  for i := Liste.Count - 1 downto 0 do
  begin
    if MinutesBetween(Now, Liste[i]) <= FENETRE_MINUTES then
      Inc(TentativesRecentes)
    else
      Break; // Les tentatives plus anciennes ne comptent plus
  end;

  Result := TentativesRecentes < MAX_TENTATIVES;
end;

class procedure TProtectionForceBrute.EnregistrerTentativeEchouee(const AUsername: string);  
var  
  Liste: TList<TDateTime>;
begin
  if not FTentatives.TryGetValue(AUsername, Liste) then
  begin
    Liste := TList<TDateTime>.Create;
    FTentatives.Add(AUsername, Liste);
  end;

  Liste.Add(Now);
end;

class procedure TProtectionForceBrute.ReinitialiserTentatives(const AUsername: string);  
var  
  Liste: TList<TDateTime>;
begin
  if FTentatives.TryGetValue(AUsername, Liste) then
  begin
    Liste.Free;
    FTentatives.Remove(AUsername);
  end;
end;

// Utilisation
procedure TFormLogin.BtnConnexionClick(Sender: TObject);  
var  
  Username: string;
begin
  Username := EditUsername.Text;

  // Vérifier si l'utilisateur peut tenter une connexion
  if not TProtectionForceBrute.PeutTenterConnexion(Username) then
  begin
    ShowMessage('Trop de tentatives échouées. Veuillez réessayer dans 15 minutes.');
    Exit;
  end;

  // Tenter la connexion
  if VerifierIdentifiants(Username, EditPassword.Text) then
  begin
    // Connexion réussie
    TProtectionForceBrute.ReinitialiserTentatives(Username);
    ShowMessage('Connexion réussie');
  end
  else
  begin
    // Connexion échouée
    TProtectionForceBrute.EnregistrerTentativeEchouee(Username);
    ShowMessage('Identifiants incorrects');
  end;
end;
```

### CAPTCHA

Pour les tentatives répétées, ajouter un CAPTCHA :

```pascal
procedure TFormLogin.AfficherCaptchaSiNecessaire;  
const  
  SEUIL_CAPTCHA = 3;
var
  NbTentatives: Integer;
begin
  NbTentatives := ObtenirNombreTentatives(EditUsername.Text);

  if NbTentatives >= SEUIL_CAPTCHA then
  begin
    PanelCaptcha.Visible := True;
    GenererNouveauCaptcha;
  end;
end;

function ValiderCaptcha(const AReponse: string): Boolean;  
begin  
  // Vérifier la réponse du CAPTCHA
  Result := AReponse = FReponseCaptchaAttendue;
end;
```

## 8. Protection contre le déni de service (DoS)

### Limitation du taux de requêtes

```pascal
type
  TRateLimiter = class
  private
    class var FRequetes: TDictionary<string, TList<TDateTime>>;
  public
    class constructor Create;
    class destructor Destroy;
    class function PeutExecuterRequete(const AClientID: string): Boolean;
  end;

class constructor TRateLimiter.Create;  
begin  
  FRequetes := TDictionary<string, TList<TDateTime>>.Create;
end;

class destructor TRateLimiter.Destroy;  
var  
  Liste: TList<TDateTime>;
begin
  for Liste in FRequetes.Values do
    Liste.Free;
  FRequetes.Free;
end;

class function TRateLimiter.PeutExecuterRequete(const AClientID: string): Boolean;  
const  
  MAX_REQUETES_PAR_MINUTE = 60;
var
  Liste: TList<TDateTime>;
  i: Integer;
  RequetesRecentes: Integer;
begin
  if not FRequetes.TryGetValue(AClientID, Liste) then
  begin
    Liste := TList<TDateTime>.Create;
    FRequetes.Add(AClientID, Liste);
  end;

  // Compter les requêtes dans la dernière minute
  RequetesRecentes := 0;
  for i := Liste.Count - 1 downto 0 do
  begin
    if SecondsBetween(Now, Liste[i]) <= 60 then
      Inc(RequetesRecentes)
    else
    begin
      // Nettoyer les anciennes entrées
      Liste.Delete(i);
    end;
  end;

  Result := RequetesRecentes < MAX_REQUETES_PAR_MINUTE;

  if Result then
    Liste.Add(Now);
end;

// Utilisation
procedure TraiterRequeteAPI(const AClientID: string);  
begin  
  if not TRateLimiter.PeutExecuterRequete(AClientID) then
  begin
    // 429 Too Many Requests
    Response.StatusCode := 429;
    Response.Content := 'Trop de requêtes. Veuillez réessayer plus tard.';
    Exit;
  end;

  // Traiter la requête normalement
  TraiterRequete;
end;
```

## 9. Path Traversal

### Le problème

Un attaquant tente d'accéder à des fichiers en dehors du répertoire autorisé.

```
Demande : /download?file=../../etc/passwd
```

### Protection

```pascal
function CheminSecurise(const ACheminBase, AFichierDemande: string): string;  
var  
  CheminComplet: string;
  CheminCanonique: string;
begin
  // Construire le chemin complet
  CheminComplet := TPath.Combine(ACheminBase, AFichierDemande);

  // Obtenir le chemin canonique (résolu, sans ..)
  CheminCanonique := ExpandFileName(CheminComplet);

  // Vérifier que le chemin final est bien dans le répertoire de base
  if not CheminCanonique.StartsWith(ACheminBase) then
    raise Exception.Create('Accès refusé : tentative de path traversal');

  Result := CheminCanonique;
end;

// Utilisation
procedure TelechargerFichier(const ANomFichier: string);  
var  
  CheminBase: string;
  CheminFichier: string;
begin
  CheminBase := TPath.Combine(ExtractFilePath(ParamStr(0)), 'downloads');

  try
    CheminFichier := CheminSecurise(CheminBase, ANomFichier);

    if FileExists(CheminFichier) then
      EnvoyerFichier(CheminFichier)
    else
      ShowMessage('Fichier introuvable');
  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      TLoggerSecurite.LoggerErreur('Tentative path traversal', ANomFichier);
    end;
  end;
end;
```

## Checklist de sécurité

Avant de déployer votre application :

### Bases de données
- [ ] Toutes les requêtes utilisent des paramètres
- [ ] Aucune construction dynamique de SQL avec concat
- [ ] Les erreurs SQL ne sont pas affichées aux utilisateurs
- [ ] Privilèges minimaux pour l'utilisateur de la base

### Validation des entrées
- [ ] Toutes les entrées utilisateur sont validées
- [ ] Validation côté client ET serveur
- [ ] Liste blanche plutôt que liste noire
- [ ] Longueurs maximales définies

### Gestion des erreurs
- [ ] Messages d'erreur génériques pour les utilisateurs
- [ ] Logging détaillé pour les développeurs
- [ ] Pas d'informations techniques dans les erreurs

### Upload de fichiers
- [ ] Taille maximale définie
- [ ] Extensions autorisées (liste blanche)
- [ ] Noms de fichiers générés automatiquement
- [ ] Stockage hors de la racine web
- [ ] Scan antivirus si possible

### Protection des accès
- [ ] Rate limiting implémenté
- [ ] Protection contre la force brute
- [ ] CAPTCHA après plusieurs échecs
- [ ] Tokens CSRF pour les actions sensibles

### Sessions et authentification
- [ ] Sessions avec timeout
- [ ] Tokens JWT avec expiration
- [ ] Pas de données sensibles dans les tokens
- [ ] Déconnexion propre (invalidation token)

## Résumé des points essentiels

✅ **Règles d'or de la sécurité** :
- Ne JAMAIS faire confiance aux entrées utilisateur
- Toujours valider et filtrer les données
- Utiliser des requêtes paramétrées SYSTÉMATIQUEMENT
- Messages d'erreur génériques + logs détaillés
- Principe de la liste blanche (autoriser explicitement)
- Défense en profondeur (plusieurs couches de protection)

❌ **Vulnérabilités critiques à éviter absolument** :
- Injection SQL par concaténation
- Affichage direct de HTML non échappé (XSS)
- Actions sensibles sans token CSRF
- Upload de fichiers sans validation
- Pas de limite sur les tentatives de connexion
- Révélation d'informations dans les erreurs
- Path traversal non protégé

🛡️ **Protection minimale obligatoire** :
- Requêtes SQL paramétrées partout
- Échappement HTML pour tout affichage dynamique
- Validation de toutes les entrées (format, type, longueur)
- Rate limiting sur les endpoints sensibles
- Gestion d'erreurs sécurisée
- Logging des événements de sécurité

## Aller plus loin

**Sections complémentaires du chapitre 16** :
- **16.6** : Audit de sécurité et journalisation
- **16.7** : Stockage sécurisé des identifiants
- **16.8** : GDPR et confidentialité

**Outils recommandés** :
- OWASP ZAP : Scanner de vulnérabilités
- Burp Suite : Tests d'intrusion
- SonarQube : Analyse de code statique

**Ressources** :
- OWASP Top 10 : https://owasp.org/www-project-top-ten/
- CWE : Liste des faiblesses communes
- SANS Top 25 : Erreurs logicielles les plus dangereuses

La protection contre les vulnérabilités est un processus continu. Restez informé des nouvelles menaces, testez régulièrement votre application et adoptez toujours une approche défensive dans votre code.

⏭️ [Audit de sécurité](/16-securite-des-applications/06-audit-de-securite.md)
