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

L'OWASP (Open Web Application Security Project) publie régulièrement une liste des 10 vulnérabilités les plus critiques. Voici la version **OWASP Top 10 — édition 2021** (toujours d'actualité en 2026, la prochaine révision est attendue) :

1. **A01 — Broken Access Control** (contrôle d'accès cassé) — *passé en n°1 depuis 2021*
2. **A02 — Cryptographic Failures** (échecs cryptographiques, ex *Sensitive Data Exposure*)
3. **A03 — Injection** (SQL, commandes, LDAP, NoSQL… inclut désormais XSS)
4. **A04 — Insecure Design** (failles de conception, *nouvelle catégorie 2021*)
5. **A05 — Security Misconfiguration** (mauvaise configuration, inclut désormais XXE)
6. **A06 — Vulnerable and Outdated Components** (composants vulnérables ou obsolètes)
7. **A07 — Identification and Authentication Failures** (anciennement *Broken Authentication*)
8. **A08 — Software and Data Integrity Failures** (*nouvelle catégorie 2021*, ex CI/CD compromis)
9. **A09 — Security Logging and Monitoring Failures** (journalisation insuffisante)
10. **A10 — Server-Side Request Forgery (SSRF)** (*nouvelle catégorie 2021*)

> 💡 Ce chapitre couvre les catégories les plus pertinentes pour un développeur Delphi : injection (A03), contrôle d'accès (A01), échecs cryptographiques (A02, voir chapitre 16.3), authentification (A07, voir chapitre 16.1), et journalisation (A09, voir chapitre 16.6).

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
// ⚠ Cet exemple se concentre uniquement sur la protection anti-injection.
//   Pour le flux d'authentification complet (récupération du sel, calcul
//   PBKDF2, comparaison à temps constant), voir la section 16.1.
procedure TFormLogin.BtnConnexionClickSecurise(Sender: TObject);  
var  
  Query: TFDQuery;
  HashStocke, SaltB64, HashCalcule: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // 1. Récupérer le sel et le hash de l'utilisateur via requête paramétrée
    Query.SQL.Text :=
      'SELECT MotDePasseHash, Salt FROM Users WHERE Username = :Username';
    Query.ParamByName('Username').AsString := EditUsername.Text;
    Query.Open;

    if Query.IsEmpty then
    begin
      ShowMessage('Identifiants incorrects');  // message identique = on ne révèle pas
      Exit;                                    // si l'utilisateur existe
    end;

    SaltB64 := Query.FieldByName('Salt').AsString;
    HashStocke := Query.FieldByName('MotDePasseHash').AsString;

    // 2. Hasher le mot de passe saisi avec le sel récupéré (PBKDF2, voir 16.1)
    HashCalcule := HasherMotDePasse(EditPassword.Text, SaltB64);

    // 3. Comparaison à temps constant pour empêcher les attaques temporelles
    if HashEgalTempsConstant(HashCalcule, HashStocke) then
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
  // ⚠ Cas particulier : tableau vide → `WHERE ID IN ()` est un SQL invalide
  //   sur la plupart des SGBD. À court-circuiter explicitement :
  if Length(AIDs) = 0 then
    Exit;

  // ⚠ Limite à ne pas oublier : la plupart des SGBD imposent un maximum
  //   de paramètres par requête (~65 535 pour MySQL, 32 767 pour Oracle,
  //   2 100 pour SQL Server). Pour de longues listes d'IDs, préférez
  //   un INSERT temporaire ou un JOIN avec une table-valued parameter.

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

**Solution 3 : Content Security Policy (CSP) et autres en-têtes de sécurité**

Pour les applications web servies par Delphi (WebBroker, Horse, mORMot…), configurer une suite d'en-têtes de sécurité — c'est presque gratuit, et très efficace pour la défense en profondeur :

```pascal
procedure ConfigurerHeadersSecurite(AResponse: TWebResponse);  
begin  
  // CSP : empêche l'exécution de scripts non whitelistés et limite les origines.
  //   - default-src 'self'    : par défaut, ressources depuis le même origine
  //   - script-src 'self' ... : scripts uniquement depuis notre site + CDN whitelisté
  //   - style-src             : pareil pour CSS
  //   - frame-ancestors       : remplace X-Frame-Options, empêche le clickjacking
  //   - base-uri              : empêche les attaques par <base> tag
  //   - form-action           : limite les destinations de formulaires
  //   - upgrade-insecure-requests : convertit automatiquement http:// → https://
  AResponse.SetCustomHeader('Content-Security-Policy',
    'default-src ''self''; ' +
    'script-src ''self'' https://cdnjs.cloudflare.com; ' +
    'style-src ''self'' ''unsafe-inline''; ' +
    'img-src ''self'' data: https:; ' +
    'frame-ancestors ''none''; ' +
    'base-uri ''self''; ' +
    'form-action ''self''; ' +
    'upgrade-insecure-requests');

  // Empêche le sniffing MIME → réduit XSS via fichiers servis avec un type incorrect.
  AResponse.SetCustomHeader('X-Content-Type-Options', 'nosniff');

  // Anti-clickjacking pour les vieux navigateurs (CSP frame-ancestors prend le relais).
  AResponse.SetCustomHeader('X-Frame-Options', 'DENY');

  // Strict-Transport-Security : impose HTTPS pour 1 an (cf section HSTS plus loin).
  AResponse.SetCustomHeader('Strict-Transport-Security',
    'max-age=31536000; includeSubDomains');

  // Referrer-Policy : limite l'information envoyée à des sites tiers.
  AResponse.SetCustomHeader('Referrer-Policy', 'strict-origin-when-cross-origin');

  // Permissions-Policy (anciennement Feature-Policy) : désactive APIs sensibles
  //   non utilisées (caméra, micro, géolocalisation, etc.).
  AResponse.SetCustomHeader('Permissions-Policy',
    'camera=(), microphone=(), geolocation=(), payment=()');
end;
```

> 💡 **Tester votre configuration** : [securityheaders.com](https://securityheaders.com) note de A+ à F la suite d'entêtes que vous renvoyez. Un objectif raisonnable : A. Le rapport pointe aussi les directives CSP suspectes (`'unsafe-inline'`, `'unsafe-eval'`) à éviter.

> 💡 **Inline scripts et nonces** : si vous devez vraiment injecter du JS inline (analytics, etc.), utilisez `script-src 'self' 'nonce-<aléatoire>'` et générez un nonce CSPRNG différent à chaque requête, à mettre dans l'attribut `nonce="..."` du `<script>`. Plus sûr que `'unsafe-inline'`.

### Validation des entrées

> 🚨 **NE PAS écrire votre propre filtreur HTML par regex.** Le HTML est un langage **non régulier** ; un regex ne peut pas le filtrer correctement. Tous les contournements suivants ont été utilisés dans la nature :  
> ```html  
> <ScRiPt>alert(1)</sCrIpT>         <!-- mixed case -->  
> <script\n>alert(1)</script>        <!-- newline -->  
> <img src=x onerror=alert(1)>      <!-- attribut JS, pas de balise script -->  
> <a href="javascript:alert(1)">    <!-- protocole javascript: -->  
> <svg onload=alert(1)>             <!-- événement sur SVG -->  
> &lt;script&gt;alert(1)&lt;/script&gt; <!-- double encodage -->  
> ```  
> Pour assainir du HTML utilisateur (champ riche, commentaires Markdown rendus en HTML…), utilisez impérativement une **bibliothèque éprouvée** :  
> - **DOMPurify** côté navigateur (référence, maintenu par Mario Heiderich).  
> - **Bleach** (Python), **HtmlSanitizer** (.NET) si vous avez un microservice de rendu.  
> - Côté Delphi pur, aucune bibliothèque mature équivalente — déléguer à un service externe ou imposer un format **Markdown** strict (qui rend en HTML sans accepter de HTML brut).

```pascal
function FiltrerBalises(const ATexte: string): string;  
var  
  BaliseAutorisees: array of string;
begin
  // ⚠ Exemple SIMPLIFIÉ — ne couvre PAS tous les contournements.
  //   Ne pas utiliser en production sans bibliothèque de sanitisation.
  BaliseAutorisees := ['<b>', '</b>', '<i>', '</i>', '<br>'];
  Result := ATexte;

  // Supprimer scripts et iframes — ne couvre PAS les attributs JS,
  // les protocoles javascript:, le double encodage, etc.
  Result := TRegEx.Replace(Result, '<script[^>]*>.*?</script>', '', [roIgnoreCase]);
  Result := TRegEx.Replace(Result, '<iframe[^>]*>.*?</iframe>', '', [roIgnoreCase]);
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
  Octets: TBytes;
begin
  // ⚠ Ne PAS utiliser CreateGUID : un GUID v4 contient certes des bits
  //   aléatoires, mais son entropie réelle dépend de l'implémentation
  //   système et n'est pas garantie cryptographique. Un token CSRF est
  //   un secret, il doit provenir d'un CSPRNG.
  SetLength(Octets, 32);                  // 256 bits
  RemplirOctetsCSPRNG(Octets);            // voir section 16.1
  Result := TNetEncoding.Base64.EncodeBytesToString(Octets);

  // Stocker le token avec timestamp
  // ⚠ TDictionary n'est PAS thread-safe : un serveur HTTP qui traite
  //   plusieurs requêtes en parallèle peut corrompre la structure.
  //   En production, encapsuler dans TThreadedDictionary, ou protéger
  //   les accès par un TCriticalSection / TMonitor.Enter(FTokens).
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

1. **Vérifier le `Origin` / `Referer`**
```pascal
uses
  System.NetEncoding;

function VerifierOrigine(const AOrigineAttendue: string): Boolean;  
var  
  Origine, URI_Host: string;
  URI: TURI;
begin
  // ⚠ Préférer le header `Origin` au `Referer` : `Origin` ne contient
  //   PAS le path complet et est défini pour les requêtes cross-origin
  //   par tous les navigateurs modernes. `Referer` peut être supprimé
  //   par le navigateur (Referrer-Policy) ou falsifié par des proxies.
  Origine := Request.GetFieldByName('Origin');
  if Origine = '' then
    Origine := Request.GetFieldByName('Referer');
  if Origine = '' then
    Exit(False);                  // Pas d'origine → refuser par défaut

  // ⚠ Comparer le HOST exact, PAS un substring : un `Pos('monsite.com', ...)`
  //   accepterait `attaquant-monsite.com.evil.com` comme origine valide !
  try
    URI := TURI.Create(Origine);
    URI_Host := URI.Host;
  except
    Exit(False);                  // URL malformée → refuser
  end;

  // Comparer avec le host attendu (insensible à la casse, pas de port).
  Result := SameText(URI_Host, AOrigineAttendue);
end;
```

2. **Double Submit Cookie**
```pascal
// Stocker le token dans un cookie ET dans le formulaire
// Les comparer lors de la soumission
```

3. **SameSite Cookie**
```pascal
// ⚠ La signature exacte de SetCookie varie selon le framework :
//   - WebBroker (TWebResponse.SetCookieField) : signature à 5 arguments
//     (Values, Domain, Path, Expires, Secure)
//   - mORMot, Horse, et d'autres frameworks tiers : signatures différentes.
//
// L'attribut `SameSite` n'est PAS directement exposé partout — il faut
// souvent l'ajouter au header `Set-Cookie` brut. Par exemple :
Response.SetCustomHeader('Set-Cookie',
  Format('session_id=%s; Path=/; Secure; HttpOnly; SameSite=Strict',
         [SessionID]));

// Niveaux SameSite (RFC 6265bis) :
//   - Strict : le cookie n'est PAS envoyé pour les requêtes cross-origin
//     (même un lien depuis Google n'inclura PAS le cookie).
//   - Lax (par défaut depuis Chrome 80, fév. 2020) : envoyé pour les
//     navigations top-level GET, pas pour les POST cross-origin.
//   - None : envoyé partout (nécessite `Secure` en complément).
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
  // ⚠ La classe `À-ÿ` couvre le Latin-1 Supplement Unicode ($C0-$FF)
  //   et inclut donc é, à, ñ, ö, etc. Mais elle EXCLUT :
  //   - les autres alphabets (cyrillique, grec, chinois, arabe, hébreu...)
  //   - certaines extensions latines (ą, č, ę, ł, ř, ş, ž des langues d'Europe centrale)
  //   - les caractères francophones comme œ et Œ qui ne sont pas dans Latin-1.
  //
  //   Pour une vraie internationalisation, utiliser la propriété Unicode
  //   `\p{L}` (Letter) supportée par les regex Delphi 11+ :
  //     '^[\p{L}\p{M}'' \-]+$'
  //   `\p{L}` = toute lettre Unicode, `\p{M}` = marques combinantes (accents).
  Result := TRegEx.IsMatch(ANom, '^[\p{L}\p{M}'' \-]+$');
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
  // ⚠ Cette implémentation suit l'ancien pattern « N types de caractères ».
  //   Les recommandations modernes (NIST SP 800-63B Rév. 4, OWASP ASVS 4)
  //   privilégient la LONGUEUR et le REJET DES MOTS DE PASSE COMPROMIS
  //   plutôt que des contraintes de composition.
  //   Voir section 16.1 (`MotDePasseValide` + `EstMotDePasseCompromis` via
  //   l'API HIBP k-anonyme) pour la version recommandée 2026.
  Result := False;

  // Minimum 8 caractères (12+ recommandés)
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
  Ligne, CheminFichier: string;
begin
  // ⚠ Utiliser `TPath.Combine` pour rester portable (Windows utilise `\`,
  //   Linux/macOS utilisent `/`). Un chemin codé en dur comme
  //   `'logs\errors.log'` ne fonctionne que sur Windows.
  // ⚠ Pour une vraie implémentation, voir la classe TLogger du chapitre 16.6
  //   avec verrou thread-safe, rotation, UTC et JSON Lines.
  CheminFichier := TPath.Combine(
    TPath.Combine(ExtractFilePath(ParamStr(0)), 'logs'),
    'errors.log');
  ForceDirectories(ExtractFilePath(CheminFichier));

  AssignFile(Fichier, CheminFichier);
  try
    if FileExists(CheminFichier) then
      Append(Fichier)
    else
      Rewrite(Fichier);

    Ligne := Format('[%s] %s - %s',
      [FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                      TTimeZone.Local.ToUniversalTime(Now)),
       AMessage, ADetails]);
    WriteLn(Fichier, Ligne);
  finally
    CloseFile(Fichier);
  end;
end;

// ✅ SÉCURISÉ - Message générique à l'utilisateur, log détaillé
procedure ExecuterRequeteSecurisee;  
var  
  IDValeur: Integer;
begin
  // ⚠ Valider les entrées AVANT d'exécuter la requête.
  //   `StrToInt` lèverait `EConvertError` sur "abc" — attraper l'exception
  //   et l'afficher en générique fonctionne, mais c'est mieux d'éviter
  //   l'exception en validant explicitement via `TryStrToInt` ou `TEdit`
  //   avec un masque numérique.
  if not TryStrToInt(EditID.Text, IDValeur) then
  begin
    ShowMessage('Identifiant invalide.');
    Exit;
  end;

  try
    Query.SQL.Text := 'SELECT * FROM Users WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := IDValeur;
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

// ⚠ La vérification d'extension est INSUFFISANTE. Un attaquant renomme
//   `payload.exe` en `image.jpg` et passe la validation. Pour une vraie
//   sécurité, vérifier la **signature binaire** du fichier (magic bytes) :
//
//   PNG  : 89 50 4E 47 0D 0A 1A 0A
//   JPEG : FF D8 FF
//   GIF  : 47 49 46 38 (37|39) 61
//   PDF  : 25 50 44 46 2D
//   ZIP  : 50 4B 03 04        (donc aussi DOCX, XLSX, JAR, APK...)
//
class function TUploadSecurise.VerifierMagicBytes(const ACheminFichier,
                                                   AExtensionAttendue: string): Boolean;
var
  Stream: TFileStream;
  Buffer: array[0..7] of Byte;
  Lus: Integer;
begin
  Result := False;
  Stream := TFileStream.Create(ACheminFichier, fmOpenRead);
  try
    Lus := Stream.Read(Buffer, SizeOf(Buffer));
    if Lus < 4 then Exit;

    if SameText(AExtensionAttendue, '.png') then
      Result := (Buffer[0] = $89) and (Buffer[1] = $50) and
                (Buffer[2] = $4E) and (Buffer[3] = $47)
    else if SameText(AExtensionAttendue, '.jpg') or
            SameText(AExtensionAttendue, '.jpeg') then
      Result := (Buffer[0] = $FF) and (Buffer[1] = $D8) and (Buffer[2] = $FF)
    else if SameText(AExtensionAttendue, '.gif') then
      Result := (Buffer[0] = $47) and (Buffer[1] = $49) and (Buffer[2] = $46)
    else if SameText(AExtensionAttendue, '.pdf') then
      Result := (Buffer[0] = $25) and (Buffer[1] = $50) and
                (Buffer[2] = $44) and (Buffer[3] = $46);
  finally
    Stream.Free;
  end;
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
  BaseCanonique: string;
begin
  // ⚠ Plusieurs pièges classiques à éviter ici :
  //
  // 1. Si `AFichierDemande` commence par '/' ou 'C:\', `TPath.Combine`
  //    RETOURNE `AFichierDemande` tel quel (ignore `ACheminBase`).
  //    Refuser explicitement les chemins absolus :
  if TPath.IsPathRooted(AFichierDemande) then
    raise Exception.Create('Accès refusé : chemin absolu interdit');

  // 2. Refuser explicitement la présence de '..' avant même la résolution.
  //    Belt and suspenders.
  if (Pos('..', AFichierDemande) > 0) then
    raise Exception.Create('Accès refusé : séquence ".." interdite');

  // 3. Normaliser les DEUX chemins (base et résultat) pour une comparaison
  //    fiable. Sans cela, `'/srv/downloads-public'.StartsWith('/srv/downloads')`
  //    matche, ce qui est une vulnérabilité.
  CheminComplet := TPath.Combine(ACheminBase, AFichierDemande);
  CheminCanonique := ExpandFileName(CheminComplet);
  BaseCanonique := IncludeTrailingPathDelimiter(ExpandFileName(ACheminBase));

  // 4. Comparer en s'assurant que le séparateur de fin force la frontière.
  //    `downloads/` ne matche PAS `downloads-public/...`.
  if not CheminCanonique.StartsWith(BaseCanonique,
       {$IFDEF MSWINDOWS} True {$ELSE} False {$ENDIF}) then  // case-insensitive sur Windows
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

## 10. Mass Assignment (OWASP API6:2023)

### Le problème

Lorsqu'une API accepte un objet JSON pour mettre à jour un enregistrement, un attaquant peut **ajouter des champs non prévus** que le développeur n'a pas pensé à filtrer :

```pascal
// ❌ VULNÉRABLE — l'attaquant peut injecter "IsAdmin": true dans le JSON
procedure ModifierProfil(const ABody: string);  
var  
  JSON: TJSONObject;
  Champ: TJSONPair;
  Update: string;
begin
  JSON := TJSONObject.ParseJSONValue(ABody) as TJSONObject;
  try
    // Construire dynamiquement UPDATE avec TOUS les champs reçus.
    // Si le client envoie { "Nom": "X", "IsAdmin": true, "Solde": 999999 }
    // → tous ces champs sont mis à jour !
    Update := 'UPDATE Users SET ';
    for Champ in JSON do
      Update := Update + Champ.JsonString.Value + ' = ' +
                Champ.JsonValue.ToJSON + ', ';
    // ...
  finally
    JSON.Free;
  end;
end;
```

### Protection : liste blanche explicite des champs autorisés

```pascal
const
  CHAMPS_AUTORISES: array[0..3] of string =
    ('Nom', 'Prenom', 'Email', 'Telephone');

function EstChampAutorise(const ANom: string): Boolean;  
var  
  Champ: string;
begin
  for Champ in CHAMPS_AUTORISES do
    if SameText(Champ, ANom) then
      Exit(True);
  Result := False;
end;

procedure ModifierProfilSecurise(AIDUtilisateur: Integer; const ABody: string);  
var  
  ValeurParsee: TJSONValue;
  JSON: TJSONObject;
  Pair: TJSONPair;
  Query: TFDQuery;
begin
  // ⚠ `as TJSONObject` lève une `EInvalidCast` si la valeur n'est pas un
  //   objet (par ex. `[1,2,3]` ou `"hello"`). Utiliser `ParseJSONValue`
  //   et vérifier le type explicitement avant de caster.
  ValeurParsee := TJSONObject.ParseJSONValue(ABody);
  if not (ValeurParsee is TJSONObject) then
  begin
    ValeurParsee.Free;
    raise Exception.Create('Le body doit être un objet JSON.');
  end;
  JSON := TJSONObject(ValeurParsee);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    for Pair in JSON do
    begin
      // ✅ Ne traiter QUE les champs explicitement autorisés
      if EstChampAutorise(Pair.JsonString.Value) then
      begin
        // ⚠ Le nom de colonne est concaténé (pas paramétrable en SQL DDL).
        //   La sécurité repose ENTIÈREMENT sur la whitelist : si elle est
        //   cassée, l'injection SQL est directe. Vérifier doublement la
        //   whitelist en cas de doute (par exemple en l'écrivant à deux
        //   endroits différents) et tester les cas limites en CI.
        Query.SQL.Text := Format(
          'UPDATE Users SET %s = :Valeur WHERE ID = :ID',
          [Pair.JsonString.Value]);
        Query.ParamByName('Valeur').AsString := Pair.JsonValue.Value;
        Query.ParamByName('ID').AsInteger := AIDUtilisateur;
        Query.ExecSQL;
      end;
      // Les autres champs (IsAdmin, Solde, MotDePasseHash...) sont IGNORÉS.
    end;
  finally
    Query.Free;
    JSON.Free;
  end;
end;
```

> 💡 **Pattern DTO** : la pratique recommandée est de définir un **DTO** (Data Transfer Object) dédié à la mise à jour, qui n'expose QUE les champs modifiables. Ainsi le code de mapping ne peut PAS toucher aux autres champs, même par accident.

## 11. SSRF (Server-Side Request Forgery)

### Le problème (OWASP A10:2021)

Si votre application Delphi côté serveur récupère des ressources depuis une URL fournie par l'utilisateur, un attaquant peut la forcer à appeler des URLs **internes** auxquelles il n'a normalement pas accès :

```pascal
// ❌ VULNÉRABLE SSRF
procedure TWebModule.HandlerPreview(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse);
var
  URL: string;
  HTTP: TIdHTTP;
begin
  URL := Request.QueryFields.Values['url'];  // ex: ?url=https://example.com/og:image
  HTTP := TIdHTTP.Create;
  try
    // Le serveur récupère l'URL → l'attaquant fait passer :
    //   ?url=http://169.254.169.254/latest/meta-data/iam/security-credentials/
    //   (endpoint des métadonnées AWS — donne les credentials du serveur)
    // ou :
    //   ?url=http://localhost:6379/  (Redis interne, sans authentification)
    //   ?url=file:///etc/passwd      (système de fichiers local)
    Response.Content := HTTP.Get(URL);
  finally
    HTTP.Free;
  end;
end;
```

### Protection : liste blanche + résolution DNS

```pascal
uses
  IdGlobal, IdStack, System.RegularExpressions;

function URLSecuriseePourFetch(const AURL: string): Boolean;  
const  
  DOMAINES_AUTORISES: array[0..2] of string =
    ('api.partenaire1.com', 'cdn.partenaire2.com', 'images.unsplash.com');
var
  URI: TIdURI;
  IP: string;
  i: Integer;
begin
  Result := False;
  URI := TIdURI.Create(AURL);
  try
    // 1. Refuser tout protocole autre que HTTPS
    if not SameText(URI.Protocol, 'https') then Exit;

    // 2. Liste blanche stricte de domaines
    for i := 0 to High(DOMAINES_AUTORISES) do
      if SameText(URI.Host, DOMAINES_AUTORISES[i]) then
        Break
      else if i = High(DOMAINES_AUTORISES) then
        Exit;

    // 3. Résoudre le DNS et REJETER les plages IP privées / locales.
    //    Sinon, un attaquant peut posséder un domaine du whitelist qui
    //    pointe (via DNS) vers 127.0.0.1 ou 169.254.169.254.
    IP := GStack.ResolveHost(URI.Host);
    if EstIPPrivee(IP) or EstIPLocale(IP) or EstIPMetadata(IP) then
      Exit;

    Result := True;
  finally
    URI.Free;
  end;
end;

function EstIPPrivee(const AIP: string): Boolean;  
begin  
  Result := AIP.StartsWith('10.') or
            AIP.StartsWith('192.168.') or
            TRegEx.IsMatch(AIP, '^172\.(1[6-9]|2\d|3[01])\.') or
            AIP.StartsWith('169.254.') or  // link-local
            AIP.StartsWith('127.') or       // loopback
            (AIP = '::1') or                // IPv6 loopback
            AIP.StartsWith('fc') or         // IPv6 unique local
            AIP.StartsWith('fe80:');        // IPv6 link-local
end;

function EstIPMetadata(const AIP: string): Boolean;  
begin  
  // Endpoints de métadonnées des clouds — accès = compromission totale
  Result := (AIP = '169.254.169.254') or    // AWS, Azure, GCP, OpenStack
            (AIP = '100.100.100.200');      // Alibaba Cloud
end;
```

> ⚠️ **Attention à la "TOCTOU" (Time-Of-Check to Time-Of-Use)** : valider l'URL, puis la passer à `TIdHTTP.Get` rouvre une seconde résolution DNS — l'attaquant peut servir une IP différente entre les deux. La protection robuste implique de **résoudre le DNS une fois**, valider l'IP obtenue, puis appeler **par IP** (avec l'entête `Host` réécrit).

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
