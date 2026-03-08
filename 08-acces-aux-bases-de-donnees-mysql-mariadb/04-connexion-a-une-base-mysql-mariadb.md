🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.4 Connexion à une base MySQL/MariaDB

## Introduction

Maintenant que vous connaissez l'architecture de FireDAC et que MySQL/MariaDB est installé et configuré, il est temps de créer votre première connexion ! Dans ce chapitre, nous allons connecter pas à pas une application Delphi à votre base de données.

## Création d'un projet de test

### Étape 1 : Nouveau projet VCL

1. Lancez Delphi
2. Menu **Fichier** → **Nouveau** → **Application VCL - Delphi**
3. Sauvegardez immédiatement votre projet :
   - **Fichier** → **Enregistrer tout**
   - Créez un dossier pour votre projet (par exemple : `C:\MesProjets\TestMySQL`)
   - Nommez l'unité : `uMain.pas`
   - Nommez le projet : `TestMySQL.dproj`

**Pourquoi sauvegarder tout de suite ?**
Cela évite les problèmes de chemins relatifs avec les composants FireDAC.

### Étape 2 : Préparer le formulaire

1. Redimensionnez le formulaire à une taille confortable (environ 600x400)
2. Définissez les propriétés du formulaire :
   - `Name` : `FormMain`
   - `Caption` : `Test de connexion MySQL`
   - `Position` : `poScreenCenter`

## Ajout des composants FireDAC

### Les composants nécessaires

Pour établir une connexion basique, vous avez besoin de **3 composants minimum** :

1. **TFDConnection** - pour la connexion
2. **TFDPhysMySQLDriverLink** - pour le pilote MySQL (optionnel mais recommandé)
3. **TFDGUIxWaitCursor** - pour le curseur d'attente (optionnel)

### Placement des composants

#### 1. Ajouter TFDConnection

1. Dans la palette de composants, onglet **FireDAC**
2. Double-cliquez sur **TFDConnection** (ou glissez-le sur le formulaire)
3. Un composant non-visuel apparaît sur votre formulaire
4. Dans l'inspecteur d'objets, modifiez la propriété `Name` : `FDConnection1` (déjà le nom par défaut)

#### 2. Ajouter TFDPhysMySQLDriverLink

1. Même onglet **FireDAC**
2. Cherchez **TFDPhysMySQLDriverLink** (ou utilisez **FireDAC Links** → **TFDPhysMySQLDriverLink**)
3. Double-cliquez pour l'ajouter
4. Laissez le nom par défaut : `FDPhysMySQLDriverLink1`

#### 3. Ajouter TFDGUIxWaitCursor

1. Onglet **FireDAC**
2. Cherchez **TFDGUIxWaitCursor**
3. Double-cliquez pour l'ajouter
4. Laissez le nom par défaut : `FDGUIxWaitCursor1`

**Résultat :** Vous devriez voir 3 icônes non-visuelles sur votre formulaire.

### Composants visuels pour tester

Ajoutons maintenant des éléments visuels pour tester la connexion :

#### 1. Ajouter un bouton de connexion

1. Palette **Standard** → **TButton**
2. Placez-le sur le formulaire
3. Propriétés :
   - `Name` : `btnConnecter`
   - `Caption` : `Se connecter`
   - `Left` : `20`
   - `Top` : `20`

#### 2. Ajouter un bouton de déconnexion

1. Ajoutez un second **TButton**
2. Propriétés :
   - `Name` : `btnDeconnecter`
   - `Caption` : `Se déconnecter`
   - `Left` : `120`
   - `Top` : `20`
   - `Enabled` : `False` (désactivé au départ)

#### 3. Ajouter un Memo pour les messages

1. Palette **Standard** → **TMemo**
2. Placez-le sur le formulaire
3. Propriétés :
   - `Name` : `memoLog`
   - `Align` : `alBottom`
   - `Height` : `200`
   - `ReadOnly` : `True`
   - `ScrollBars` : `ssVertical`

**Votre formulaire devrait maintenant ressembler à ceci :**

```
┌─────────────────────────────────────┐
│ Test de connexion MySQL    [_][□][X]│
├─────────────────────────────────────┤
│  [Se connecter] [Se déconnecter]    │
│                                     │
│                                     │
│                                     │
├─────────────────────────────────────┤
│ ┌─────────────────────────────────┐ │
│ │ Memo pour les messages          │ │
│ │                                 │ │
│ │                                 │ │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

## Configuration de la connexion (Design Time)

Nous allons d'abord configurer la connexion directement dans l'IDE (au moment de la conception).

### Méthode 1 : Éditeur de connexion graphique

#### Étape 1 : Ouvrir l'éditeur

1. Sélectionnez le composant `FDConnection1` sur le formulaire
2. Dans l'inspecteur d'objets, cherchez la propriété `ConnectionDefName`
3. Cliquez sur le bouton **[...]** à côté de `Params`
4. L'**Éditeur de définition de connexion** s'ouvre

Ou plus simplement :
- Double-cliquez directement sur le composant `FDConnection1`

#### Étape 2 : Choisir le pilote

Dans l'éditeur de définition :

1. Dans la liste déroulante **Driver ID**, sélectionnez : `MySQL`
2. L'interface affiche maintenant les paramètres spécifiques à MySQL

#### Étape 3 : Configurer les paramètres

Remplissez les champs suivants :

| Paramètre | Valeur à saisir | Explication |
|-----------|-----------------|-------------|
| **Server** | `localhost` ou `127.0.0.1` | Adresse du serveur MySQL |
| **Port** | `3306` | Port par défaut MySQL (laissez vide pour utiliser par défaut) |
| **Database** | `ma_gestion` | Nom de votre base de données |
| **User_Name** | `delphi_user` | Utilisateur créé précédemment |
| **Password** | `votre_mot_de_passe` | Mot de passe de l'utilisateur |
| **CharacterSet** | `utf8mb4` | Important pour les accents ! |

**Paramètres optionnels mais recommandés :**

| Paramètre | Valeur | Pourquoi ? |
|-----------|--------|------------|
| **LoginTimeout** | `10` | Délai max pour la connexion (secondes) |
| **ConnectionTimeout** | `30` | Délai max pour garder la connexion |

#### Étape 4 : Tester la connexion

1. Cliquez sur le bouton **Test** en bas de la fenêtre
2. Si tout est correct, vous verrez : **"Connection test successful"**
3. Si erreur, consultez la section "Résolution de problèmes" plus bas
4. Cliquez sur **OK** pour fermer l'éditeur

#### Étape 5 : Propriétés importantes

De retour dans l'inspecteur d'objets du `FDConnection1` :

| Propriété | Valeur recommandée | Explication |
|-----------|-------------------|-------------|
| `LoginPrompt` | `False` | Ne pas demander les identifiants à l'utilisateur |
| `Connected` | `False` | Ne pas connecter automatiquement au démarrage |

**Pourquoi `Connected = False` ?**
Il est préférable de gérer la connexion par code pour mieux contrôler les erreurs.

### Méthode 2 : Configuration manuelle des paramètres

Vous pouvez aussi configurer les paramètres directement dans l'inspecteur d'objets :

1. Sélectionnez `FDConnection1`
2. Cherchez la propriété `Params`
3. Double-cliquez sur `Params` : une fenêtre s'ouvre
4. Ajoutez les lignes suivantes (une par ligne) :

```
DriverID=MySQL  
Server=localhost  
Port=3306  
Database=ma_gestion  
User_Name=delphi_user  
Password=votre_mot_de_passe  
CharacterSet=utf8mb4  
```

**Attention :** Mettre le mot de passe dans les propriétés n'est pas sécurisé ! C'est acceptable pour le développement, mais pas pour la production.

## Configuration de la bibliothèque client MySQL

### Spécifier le chemin de libmysql.dll

Le composant `TFDPhysMySQLDriverLink` permet de spécifier où se trouve la bibliothèque client MySQL.

1. Sélectionnez `FDPhysMySQLDriverLink1`
2. Dans l'inspecteur d'objets, cherchez la propriété `VendorLib`
3. Spécifiez le chemin complet vers la DLL :

**Exemples de chemins :**

```
// MariaDB par défaut (64 bits)
C:\Program Files\MariaDB 11.5\lib\libmariadb.dll

// MySQL par défaut (64 bits)
C:\Program Files\MySQL\MySQL Server 8.0\lib\libmysql.dll

// Dans le dossier de votre application (recommandé pour distribution)
libmysql.dll
```

**Astuce :** Si vous mettez juste le nom du fichier (`libmysql.dll`), Delphi cherchera dans :
- Le dossier de l'application
- Les dossiers du PATH système

**Recommandation pour distribution :**
Copiez `libmysql.dll` dans le même dossier que votre .exe, et utilisez juste le nom de fichier sans chemin.

### Laisser vide (détection automatique)

Vous pouvez aussi laisser `VendorLib` vide. FireDAC essaiera de trouver automatiquement la bibliothèque. Cela fonctionne généralement sous Windows si MySQL/MariaDB est installé correctement.

## Connexion par code (Runtime)

Maintenant, programmons la connexion avec du code !

### Code du bouton "Se connecter"

Double-cliquez sur le bouton `btnConnecter` pour créer son événement `OnClick` :

```pascal
procedure TFormMain.btnConnecterClick(Sender: TObject);  
begin  
  // Effacer le log
  memoLog.Clear;

  try
    // Tentative de connexion
    memoLog.Lines.Add('Tentative de connexion à MySQL...');
    FDConnection1.Connected := True;

    // Si on arrive ici, la connexion a réussi
    memoLog.Lines.Add('✓ Connexion réussie !');
    memoLog.Lines.Add('Serveur : ' + FDConnection1.Params.Values['Server']);
    memoLog.Lines.Add('Base de données : ' + FDConnection1.Params.Values['Database']);

    // Activer/désactiver les boutons
    btnConnecter.Enabled := False;
    btnDeconnecter.Enabled := True;

  except
    on E: Exception do
    begin
      memoLog.Lines.Add('✗ Erreur de connexion :');
      memoLog.Lines.Add(E.Message);

      // S'assurer que la connexion est fermée
      FDConnection1.Connected := False;
    end;
  end;
end;
```

**Explications du code :**

- `memoLog.Clear` : efface les anciens messages
- `try...except` : capture les erreurs de connexion
- `FDConnection1.Connected := True` : établit la connexion
- Si succès : affiche les infos et change l'état des boutons
- Si échec : affiche l'erreur dans le memo

### Code du bouton "Se déconnecter"

Double-cliquez sur le bouton `btnDeconnecter` :

```pascal
procedure TFormMain.btnDeconnecterClick(Sender: TObject);  
begin  
  try
    // Fermer la connexion
    FDConnection1.Connected := False;

    memoLog.Lines.Add('─────────────────────────');
    memoLog.Lines.Add('✓ Déconnexion réussie');

    // Activer/désactiver les boutons
    btnConnecter.Enabled := True;
    btnDeconnecter.Enabled := False;

  except
    on E: Exception do
    begin
      memoLog.Lines.Add('✗ Erreur lors de la déconnexion :');
      memoLog.Lines.Add(E.Message);
    end;
  end;
end;
```

### Code de fermeture du formulaire

Il est important de fermer la connexion quand l'application se termine.

1. Sélectionnez le formulaire `FormMain`
2. Dans l'inspecteur d'objets, onglet **Événements**
3. Double-cliquez sur `OnClose`
4. Ajoutez ce code :

```pascal
procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  // Fermer la connexion si elle est ouverte
  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;
```

## Configuration par code complet

Pour un contrôle total, vous pouvez configurer tous les paramètres par code :

```pascal
procedure TFormMain.ConnecterAvecCode;  
begin  
  // Configurer les paramètres de connexion
  FDConnection1.Params.Clear;  // Effacer les paramètres existants

  // Paramètres de base
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Port=3306');
  FDConnection1.Params.Add('Database=ma_gestion');
  FDConnection1.Params.Add('User_Name=delphi_user');
  FDConnection1.Params.Add('Password=VotreMotDePasse');
  FDConnection1.Params.Add('CharacterSet=utf8mb4');

  // Paramètres optionnels
  FDConnection1.Params.Add('LoginTimeout=10');

  // Désactiver le prompt de connexion
  FDConnection1.LoginPrompt := False;

  try
    // Se connecter
    FDConnection1.Connected := True;
    ShowMessage('Connexion réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

**Avantages de la configuration par code :**
- Peut lire les paramètres depuis un fichier de configuration
- Plus facile à changer en production
- Meilleure sécurité (pas de mot de passe visible dans le .dfm)

## Lecture des paramètres depuis un fichier INI

Une approche professionnelle consiste à stocker les paramètres dans un fichier externe.

### Créer un fichier config.ini

Créez un fichier texte `config.ini` dans le dossier de votre application :

```ini
[Database]
Server=localhost  
Port=3306  
Database=ma_gestion  
Username=delphi_user  
Password=VotreMotDePasse  
CharacterSet=utf8mb4  
```

### Code pour lire le fichier INI

Ajoutez `IniFiles` dans la clause `uses` de votre unité :

```pascal
uses
  System.SysUtils, System.Classes, IniFiles;
```

Créez une méthode pour charger la configuration :

```pascal
procedure TFormMain.ChargerConfiguration;  
var  
  IniFile: TIniFile;
  CheminConfig: string;
begin
  // Chemin du fichier de configuration
  CheminConfig := ExtractFilePath(Application.ExeName) + 'config.ini';

  // Vérifier que le fichier existe
  if not FileExists(CheminConfig) then
  begin
    ShowMessage('Fichier config.ini introuvable !');
    Exit;
  end;

  // Créer l'objet IniFile
  IniFile := TIniFile.Create(CheminConfig);
  try
    // Lire les paramètres
    FDConnection1.Params.Clear;
    FDConnection1.Params.Add('DriverID=MySQL');
    FDConnection1.Params.Add('Server=' + IniFile.ReadString('Database', 'Server', 'localhost'));
    FDConnection1.Params.Add('Port=' + IniFile.ReadString('Database', 'Port', '3306'));
    FDConnection1.Params.Add('Database=' + IniFile.ReadString('Database', 'Database', ''));
    FDConnection1.Params.Add('User_Name=' + IniFile.ReadString('Database', 'Username', ''));
    FDConnection1.Params.Add('Password=' + IniFile.ReadString('Database', 'Password', ''));
    FDConnection1.Params.Add('CharacterSet=' + IniFile.ReadString('Database', 'CharacterSet', 'utf8mb4'));

    FDConnection1.LoginPrompt := False;

  finally
    IniFile.Free;  // Libérer l'objet
  end;
end;
```

**Utilisation :**

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Charger la configuration au démarrage
  ChargerConfiguration;
end;
```

## Gestion avancée des erreurs

Voici un exemple de gestion d'erreurs plus détaillée :

```pascal
procedure TFormMain.btnConnecterClick(Sender: TObject);  
begin  
  memoLog.Clear;

  try
    memoLog.Lines.Add('Connexion en cours...');
    FDConnection1.Connected := True;

    memoLog.Lines.Add('');
    memoLog.Lines.Add('═══════════════════════════════');
    memoLog.Lines.Add('✓ CONNEXION RÉUSSIE');
    memoLog.Lines.Add('═══════════════════════════════');
    memoLog.Lines.Add('Serveur     : ' + FDConnection1.Params.Values['Server']);
    memoLog.Lines.Add('Base        : ' + FDConnection1.Params.Values['Database']);
    memoLog.Lines.Add('Utilisateur : ' + FDConnection1.Params.Values['User_Name']);
    memoLog.Lines.Add('Encodage    : ' + FDConnection1.Params.Values['CharacterSet']);

    btnConnecter.Enabled := False;
    btnDeconnecter.Enabled := True;

  except
    on E: EFDDBEngineException do
    begin
      memoLog.Lines.Add('');
      memoLog.Lines.Add('═══════════════════════════════');
      memoLog.Lines.Add('✗ ERREUR DE CONNEXION');
      memoLog.Lines.Add('═══════════════════════════════');
      memoLog.Lines.Add('Type    : Erreur FireDAC/MySQL');
      memoLog.Lines.Add('Message : ' + E.Message);

      // Analyser l'erreur
      if Pos('Can''t connect', E.Message) > 0 then
        memoLog.Lines.Add('→ Le serveur MySQL n''est peut-être pas démarré')
      else if Pos('Access denied', E.Message) > 0 then
        memoLog.Lines.Add('→ Vérifiez le nom d''utilisateur et le mot de passe')
      else if Pos('Unknown database', E.Message) > 0 then
        memoLog.Lines.Add('→ La base de données n''existe pas');

      FDConnection1.Connected := False;
    end;

    on E: Exception do
    begin
      memoLog.Lines.Add('');
      memoLog.Lines.Add('✗ ERREUR GÉNÉRALE');
      memoLog.Lines.Add(E.ClassName + ': ' + E.Message);
      FDConnection1.Connected := False;
    end;
  end;
end;
```

## Test de votre application

### Compilation et exécution

1. Appuyez sur **F9** ou cliquez sur le bouton **Exécuter**
2. Votre application se lance
3. Cliquez sur **Se connecter**
4. Si tout va bien, vous devriez voir le message de succès dans le memo

### Vérifications

✅ Le message "Connexion réussie" apparaît  
✅ Les informations de connexion sont affichées  
✅ Le bouton "Se connecter" est désactivé  
✅ Le bouton "Se déconnecter" est activé  
✅ En cliquant sur "Se déconnecter", la connexion se ferme

## Résolution de problèmes courants

### Erreur : "Cannot load vendor library"

**Cause :** FireDAC ne trouve pas `libmysql.dll` ou `libmariadb.dll`

**Solutions :**
1. Vérifiez la propriété `VendorLib` de `TFDPhysMySQLDriverLink`
2. Copiez la DLL dans le dossier de votre .exe
3. Vérifiez que la DLL est de la même architecture (32/64 bits) que votre application

### Erreur : "Can't connect to MySQL server"

**Cause :** Le serveur MySQL n'est pas accessible

**Solutions :**
1. Vérifiez que le service MySQL/MariaDB est démarré
2. Testez la connexion avec HeidiSQL ou MySQL Workbench
3. Vérifiez le nom du serveur et le port (localhost, 3306)
4. Vérifiez le pare-feu

### Erreur : "Access denied for user"

**Cause :** Nom d'utilisateur ou mot de passe incorrect

**Solutions :**
1. Vérifiez les identifiants
2. Vérifiez que l'utilisateur existe dans MySQL
3. Vérifiez les permissions de l'utilisateur
4. Testez la connexion avec HeidiSQL

### Erreur : "Unknown database"

**Cause :** La base de données spécifiée n'existe pas

**Solutions :**
1. Vérifiez le nom de la base de données (sensible à la casse sous Linux)
2. Créez la base de données si elle n'existe pas
3. Vérifiez que l'utilisateur a accès à cette base

### Problème : Caractères accentués mal affichés

**Cause :** Encodage incorrect

**Solutions :**
1. Ajoutez `CharacterSet=utf8mb4` dans les paramètres
2. Vérifiez que la base de données utilise utf8mb4
3. Vérifiez l'encodage de vos fichiers source Delphi (UTF-8)

## Bonnes pratiques

### ✅ À faire

- **Toujours** utiliser try...except pour gérer les erreurs de connexion
- **Fermer** la connexion quand elle n'est plus nécessaire
- **Utiliser** un fichier de configuration pour les paramètres de connexion
- **Tester** la connexion avant de déployer l'application
- **Utiliser** utf8mb4 comme encodage de caractères
- **Créer** un utilisateur dédié pour l'application (ne pas utiliser root)

### ❌ À éviter

- Ne **jamais** laisser un mot de passe en clair dans le code source
- Ne **pas** ignorer les exceptions de connexion
- Ne **pas** garder la connexion ouverte inutilement
- Ne **pas** utiliser le compte root pour les applications
- Ne **pas** oublier de fermer la connexion à la fermeture de l'application

## Méthodes utiles de TFDConnection

| Méthode | Description | Exemple |
|---------|-------------|---------|
| `Open` | Ouvre la connexion | `FDConnection1.Open;` |
| `Close` | Ferme la connexion | `FDConnection1.Close;` |
| `ExecSQL` | Exécute une commande SQL | `FDConnection1.ExecSQL('CREATE TABLE...');` |
| `StartTransaction` | Démarre une transaction | `FDConnection1.StartTransaction;` |
| `Commit` | Valide une transaction | `FDConnection1.Commit;` |
| `Rollback` | Annule une transaction | `FDConnection1.Rollback;` |

## Propriétés utiles de TFDConnection

| Propriété | Type | Description |
|-----------|------|-------------|
| `Connected` | Boolean | État de la connexion |
| `ConnectionName` | String | Nom de la connexion |
| `DriverName` | String | Nom du pilote (MySQL) |
| `Params` | TStrings | Paramètres de connexion |
| `LoginPrompt` | Boolean | Demander les identifiants |
| `InTransaction` | Boolean | Transaction en cours ? |

## Vérifier l'état de la connexion

Vous pouvez vérifier l'état de la connexion à tout moment :

```pascal
if FDConnection1.Connected then  
begin  
  ShowMessage('Connexion active');
  // Informations sur la connexion
  ShowMessage('Serveur: ' + FDConnection1.Params.Values['Server']);
end  
else  
  ShowMessage('Pas de connexion');
```

## Événements de TFDConnection

Vous pouvez réagir aux événements de connexion :

### OnBeforeConnect

Se produit juste avant la tentative de connexion :

```pascal
procedure TFormMain.FDConnection1BeforeConnect(Sender: TObject);  
begin  
  memoLog.Lines.Add('Tentative de connexion...');
end;
```

### OnAfterConnect

Se produit après une connexion réussie :

```pascal
procedure TFormMain.FDConnection1AfterConnect(Sender: TObject);  
begin  
  memoLog.Lines.Add('Connecté avec succès !');
end;
```

### OnBeforeDisconnect

Se produit avant la déconnexion :

```pascal
procedure TFormMain.FDConnection1BeforeDisconnect(Sender: TObject);  
begin  
  memoLog.Lines.Add('Fermeture de la connexion...');
end;
```

## Résumé

Vous savez maintenant comment :

✅ Placer les composants FireDAC sur un formulaire  
✅ Configurer une connexion MySQL/MariaDB au design time  
✅ Établir une connexion par code  
✅ Gérer les erreurs de connexion  
✅ Charger les paramètres depuis un fichier INI  
✅ Fermer proprement une connexion  
✅ Diagnostiquer les problèmes courants

## Prochaines étapes

Maintenant que votre application peut se connecter à MySQL/MariaDB, vous êtes prêt à :

1. **Exécuter des requêtes SELECT** pour lire des données
2. **Afficher les données** dans des grilles et des contrôles
3. **Modifier les données** (INSERT, UPDATE, DELETE)
4. **Utiliser des requêtes paramétrées** pour la sécurité

Dans la section suivante, nous verrons comment manipuler concrètement les données de votre base MySQL/MariaDB !

⏭️ [Manipulation des données](/08-acces-aux-bases-de-donnees-mysql-mariadb/05-manipulation-des-donnees.md)
