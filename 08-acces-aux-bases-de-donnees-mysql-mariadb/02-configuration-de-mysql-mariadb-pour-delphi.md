# 8.2 Configuration de MySQL/MariaDB pour Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans cette section, nous allons voir comment configurer une base de données MySQL ou MariaDB pour l'utiliser avec Delphi. Nous couvrirons l'installation du serveur de base de données, la création d'une base et les configurations nécessaires pour que Delphi puisse s'y connecter efficacement.

## Qu'est-ce que MySQL et MariaDB ?

**MySQL** est l'un des systèmes de gestion de bases de données relationnelles (SGBDR) les plus populaires au monde. Il est open source et particulièrement utilisé pour les applications web, mais il convient parfaitement aux applications Delphi.

**MariaDB** est un fork de MySQL créé par les développeurs originaux de MySQL après son acquisition par Oracle. MariaDB est totalement compatible avec MySQL et offre des fonctionnalités supplémentaires.

Pour Delphi, les deux sont pratiquement identiques en termes d'utilisation. Les instructions ci-dessous fonctionnent pour les deux systèmes.

## Étape 1 : Installation du serveur MySQL/MariaDB

### Pour Windows

1. **MySQL** :
   - Téléchargez l'installateur depuis [le site officiel de MySQL](https://dev.mysql.com/downloads/installer/)
   - Choisissez "MySQL Installer" qui installera tout ce dont vous avez besoin
   - Lancez l'installateur et suivez les instructions
   - Sélectionnez au minimum : "MySQL Server" et "Connector/ODBC"
   - Lors de la configuration, définissez un mot de passe pour l'utilisateur root

2. **MariaDB** :
   - Téléchargez l'installateur depuis [le site officiel de MariaDB](https://mariadb.org/download/)
   - Lancez l'installateur et suivez les instructions
   - Définissez un mot de passe pour l'utilisateur root
   - Assurez-vous que le service démarre automatiquement

### Pour macOS

1. **MySQL** :
   - Téléchargez l'installateur DMG depuis [le site de MySQL](https://dev.mysql.com/downloads/mysql/)
   - Installez le package
   - Notez le mot de passe temporaire qui vous est fourni (uniquement pour MySQL)

2. **MariaDB** :
   - Utilisez Homebrew : `brew install mariadb`
   - Démarrez le service : `brew services start mariadb`
   - Sécurisez l'installation : `sudo mysql_secure_installation`

### Pour Linux

1. **MySQL** :
   ```bash
   sudo apt-get update
   sudo apt-get install mysql-server
   sudo mysql_secure_installation
   ```

2. **MariaDB** :
   ```bash
   sudo apt-get update
   sudo apt-get install mariadb-server
   sudo mysql_secure_installation
   ```

## Étape 2 : Installation des bibliothèques clientes

Pour que Delphi puisse communiquer avec MySQL/MariaDB, vous avez besoin des bibliothèques clientes appropriées.

### Pour Windows

1. Si vous avez installé MySQL avec l'installateur, les bibliothèques clientes sont déjà installées.
2. Si vous utilisez MariaDB, vérifiez que les fichiers `libmysql.dll` ou `libmariadb.dll` sont dans le répertoire `C:\Program Files\MariaDB\MariaDB Server\lib`.

### Pour macOS et Linux

Les bibliothèques clientes sont généralement installées avec le serveur. Les fichiers importants sont `libmysqlclient.dylib` (macOS) ou `libmysqlclient.so` (Linux).

## Étape 3 : Création d'une base de données et d'un utilisateur

Il est fortement recommandé de créer une base de données dédiée à votre application et un utilisateur spécifique avec des privilèges limités (plutôt que d'utiliser l'utilisateur root).

Voici comment procéder avec un outil de ligne de commande :

1. Connectez-vous à MySQL/MariaDB en tant qu'administrateur :
   ```
   mysql -u root -p
   ```
   Entrez votre mot de passe root quand il est demandé.

2. Créez une nouvelle base de données :
   ```sql
   CREATE DATABASE ma_base_delphi CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
   ```

3. Créez un nouvel utilisateur avec un mot de passe :
   ```sql
   CREATE USER 'user_delphi'@'localhost' IDENTIFIED BY 'mot_de_passe_securise';
   ```

4. Accordez les privilèges à cet utilisateur sur la base de données :
   ```sql
   GRANT ALL PRIVILEGES ON ma_base_delphi.* TO 'user_delphi'@'localhost';
   FLUSH PRIVILEGES;
   ```

5. Quittez l'outil en ligne de commande :
   ```sql
   EXIT;
   ```

## Étape 4 : Utilisation d'un outil graphique (facultatif mais recommandé)

Pour faciliter la gestion de votre base de données, vous pouvez utiliser un outil graphique :

- **MySQL Workbench** : Outil officiel de MySQL, très complet
- **HeidiSQL** : Outil léger et convivial, fonctionne avec MySQL et MariaDB
- **phpMyAdmin** : Interface web populaire, nécessite un serveur web

Ces outils vous permettront de créer des tables, d'exécuter des requêtes SQL et de gérer votre base de données plus facilement.

## Étape 5 : Configuration de Delphi pour MySQL/MariaDB

Maintenant que le serveur est prêt, configurons Delphi pour s'y connecter :

1. **Ajoutez les composants FireDAC à votre palette** :
   - Dans Delphi, ils se trouvent généralement dans l'onglet "FireDAC"
   - Si vous ne les voyez pas, cliquez-droit sur la palette → "Components" → "Install Packages" et vérifiez que les packages FireDAC sont bien installés.

2. **Configuration du pilote FireDAC pour MySQL** :

   Delphi a besoin de savoir où trouver les bibliothèques clientes MySQL. Deux approches sont possibles :

   **Approche 1 : Configuration globale** (recommandée)

   Configurez le fichier `FireDAC.ini` qui se trouve généralement dans :
   - Windows : `C:\Users\[Utilisateur]\AppData\Roaming\Embarcadero\[Version_Delphi]\FireDAC.ini`
   - macOS : `/Users/[Utilisateur]/Library/Application Support/Embarcadero/[Version_Delphi]/FireDAC.ini`

   Si le fichier n'existe pas, créez-le avec ce contenu :

   ```ini
   [MySQL]
   Drivers=MySQL

   [MySQL@Win32]
   VendorLib=C:\Program Files\MySQL\MySQL Server 8.0\lib\libmysql.dll

   [MySQL@Win64]
   VendorLib=C:\Program Files\MySQL\MySQL Server 8.0\lib\libmysql.dll

   [MySQL@OSX64]
   VendorLib=/usr/local/mysql/lib/libmysqlclient.dylib

   [MySQL@Linux64]
   VendorLib=/usr/lib/x86_64-linux-gnu/libmysqlclient.so
   ```

   Adaptez les chemins en fonction de votre installation.

   **Approche 2 : Configuration par application**

   Dans le code de votre application, avant d'établir la connexion :

   ```delphi
   FDManager.AddConnectionDef('MySQL_App', 'MySQL', '');
   FDManager.ConnectionDefs.ConnectionDefByName('MySQL_App').Params.DriverID := 'MySQL';

   // Pour Windows 32 bits
   FDManager.DriverDefs.DriverDefByName('MySQL').VendorLib := 'C:\Program Files\MySQL\MySQL Server 8.0\lib\libmysql.dll';

   // Pour Windows 64 bits
   // FDManager.DriverDefs.DriverDefByName('MySQL').VendorLib := 'C:\Program Files\MySQL\MySQL Server 8.0\lib\libmysql.dll';
   ```

## Test de la connexion

Voici un exemple simple pour tester la connexion à votre base de données :

1. Créez un nouveau projet d'application VCL
2. Placez les composants suivants sur le formulaire :
   - Un `TFDConnection` (nommez-le `FDConnection1`)
   - Un `TButton` (nommez-le `btnTest`)
   - Un `TMemo` (nommez-le `Memo1`)

3. Double-cliquez sur `btnTest` et ajoutez ce code :

```delphi
procedure TForm1.btnTestClick(Sender: TObject);
begin
  try
    // Configuration de la connexion
    FDConnection1.Params.Clear;
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Add('Server=localhost');
    FDConnection1.Params.Add('Port=3306');  // Port par défaut
    FDConnection1.Params.Add('Database=ma_base_delphi');
    FDConnection1.Params.Add('User_Name=user_delphi');
    FDConnection1.Params.Add('Password=mot_de_passe_securise');
    FDConnection1.Params.Add('CharacterSet=utf8mb4'); // Recommandé pour le support Unicode complet

    // Ouvrir la connexion
    FDConnection1.Connected := True;

    // Afficher un message de succès
    Memo1.Lines.Add('Connexion réussie à MySQL !');
    Memo1.Lines.Add('Version du serveur : ' + FDConnection1.ExecSQLScalar('SELECT VERSION()'));

    // Fermer la connexion
    FDConnection1.Connected := False;
  except
    on E: Exception do
    begin
      // Afficher l'erreur
      Memo1.Lines.Add('Erreur de connexion : ' + E.Message);
    end;
  end;
end;
```

4. Exécutez l'application et cliquez sur le bouton de test. Si tout est correctement configuré, vous devriez voir un message de succès et la version de votre serveur MySQL/MariaDB.

## Dépannage

Si vous rencontrez des problèmes de connexion, voici quelques points à vérifier :

1. **Service MySQL/MariaDB** : Assurez-vous que le service est en cours d'exécution.
2. **Pare-feu** : Vérifiez que le pare-feu ne bloque pas les connexions (généralement sur le port 3306).
3. **Bibliothèque cliente** : Vérifiez que le chemin vers `libmysql.dll` ou `libmariadb.dll` est correct.
4. **Identifiants** : Vérifiez que le nom d'utilisateur et le mot de passe sont corrects.
5. **Privilèges** : Assurez-vous que l'utilisateur a les droits nécessaires sur la base de données.
6. **Encodage** : Pour éviter les problèmes d'encodage, utilisez toujours `CharacterSet=utf8mb4`.

## Conclusion

Vous avez maintenant configuré MySQL/MariaDB et Delphi pour travailler ensemble. Dans les prochaines sections, nous verrons comment exploiter cette connexion pour créer, lire, mettre à jour et supprimer des données en utilisant FireDAC.

---

**À suivre :** 8.3 FireDAC : architecture et composants

⏭️ [FireDAC : architecture et composants](/08-acces-aux-bases-de-donnees-mysql-mariadb/03-firedac-architecture-et-composants.md)
