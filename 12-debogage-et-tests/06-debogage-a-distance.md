🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.6 Débogage à distance

## Introduction

Imaginez que vous avez développé une application qui fonctionne parfaitement sur votre ordinateur de développement, mais qui rencontre des problèmes sur l'ordinateur d'un client, sur un serveur en production, ou sur une machine virtuelle. Comment déboguer ce problème sans avoir à installer tout l'environnement de développement Delphi sur cette machine distante ?

C'est exactement ce que permet le **débogage à distance** : exécuter votre application sur un ordinateur (la machine cible) tout en la déboguant depuis votre environnement de développement Delphi sur un autre ordinateur (la machine de développement).

Pour un débutant, le débogage à distance peut sembler complexe, mais c'est simplement une extension des techniques de débogage locales que vous connaissez déjà. La différence principale est que l'application s'exécute ailleurs, souvent sur une machine avec un système d'exploitation différent ou une configuration particulière.

## Qu'est-ce que le débogage à distance ?

### Définition simple

Le débogage à distance permet à un développeur de :
- Exécuter une application sur une machine distante (la **cible**)
- Contrôler cette exécution depuis Delphi sur sa machine locale (l'**hôte**)
- Utiliser tous les outils de débogage habituels (points d'arrêt, inspection de variables, etc.)

### Analogie pour comprendre

Pensez au débogage à distance comme à une opération chirurgicale assistée par robot :
- Le patient (l'application) est dans une salle d'opération (la machine distante)
- Le chirurgien (vous, le développeur) est dans une autre salle de contrôle (votre machine de développement)
- Le chirurgien manipule les instruments à distance mais voit tout ce qui se passe comme s'il était sur place

### Architecture du débogage à distance

Le débogage à distance repose sur une architecture client-serveur :

```
┌──────────────────────────┐          ┌─────────────────────────┐
│  Machine de développement│          │   Machine cible         │
│  (votre PC)              │          │   (serveur, client, VM) │
│                          │          │                         │
│  ┌────────────────────┐  │          │  ┌──────────────────┐   │
│  │   IDE Delphi       │  │          │  │  Débogueur       │   │
│  │                    │ ◄├──────────┤─►│  distant         │   │
│  │  - Points d'arrêt  │  │  Réseau  │  │  (PAServer)      │   │
│  │  - Inspection vars │  │          │  │                  │   │
│  │  - Contrôle exec.  │  │          │  │  ┌────────────┐  │   │
│  └────────────────────┘  │          │  │  │ Votre App  │  │   │
│                          │          │  │  └────────────┘  │   │
└──────────────────────────┘          └─────────────────────────┘
```

**Composants principaux :**

1. **IDE Delphi** : Sur votre machine, c'est l'interface que vous utilisez
2. **Débogueur distant (PAServer)** : Un programme spécial qui tourne sur la machine cible
3. **Connexion réseau** : Permet la communication entre les deux machines
4. **Application cible** : Votre programme s'exécutant sur la machine distante

## Pourquoi utiliser le débogage à distance ?

### Scénarios courants

**Problèmes spécifiques à un environnement**

Votre application peut avoir un comportement différent selon :
- La version du système d'exploitation
- La configuration matérielle
- Les logiciels installés
- Les paramètres régionaux

Le débogage à distance vous permet de déboguer directement dans l'environnement problématique.

**Développement multi-plateformes**

Si vous développez une application FireMonkey pour macOS, iOS, Android ou Linux, vous devez tester et déboguer sur ces plateformes. Le débogage à distance est essentiel car :
- Vous développez sous Windows
- Mais l'application doit tourner sur macOS, Linux, Android, ou iOS

**Débogage de serveurs**

Pour les applications serveur ou les services Windows tournant sur des machines distantes, le débogage à distance permet d'analyser les problèmes sans perturber l'environnement de production.

**Débogage sur des machines virtuelles**

Vous pouvez avoir plusieurs VM avec différentes configurations (Windows 10, Windows 11, Windows Server) et déboguer sur chacune sans installer Delphi partout.

**Tests sur du matériel spécifique**

Pour des applications IoT ou embarquées, vous devez déboguer directement sur le matériel cible (Raspberry Pi, dispositifs industriels, etc.).

**Problèmes de performance**

Un problème peut apparaître seulement sur une machine moins puissante. Le débogage à distance vous permet d'analyser le comportement dans ces conditions.

### Avantages

- **Pas besoin d'installer Delphi partout** : Seul un petit programme (PAServer) est nécessaire sur la cible
- **Débogage dans l'environnement réel** : Vous testez dans les conditions exactes où le problème se produit
- **Économie de temps** : Pas besoin de reproduire localement un environnement complexe
- **Support de multiples plateformes** : Déboguez sur Windows, macOS, iOS, Android, Linux depuis un seul poste

### Limitations

- **Nécessite une connexion réseau** : Peut être lente selon la bande passante
- **Configuration initiale** : Peut être complexe la première fois
- **Sécurité** : Nécessite d'ouvrir des ports réseau (voir section sécurité)
- **Performances** : Le débogage peut être plus lent qu'en local

## PAServer : Le serveur de débogage distant

### Qu'est-ce que PAServer ?

**PAServer** (Platform Assistant Server) est un programme fourni avec Delphi qui :
- S'installe sur la machine cible
- Attend les connexions depuis l'IDE Delphi
- Permet le déploiement et le débogage de votre application
- Gère la communication entre l'IDE et l'application en cours d'exécution

**Important** : PAServer est nécessaire pour les plateformes non-Windows (macOS, iOS, Android, Linux). Pour Windows, le débogage distant peut utiliser d'autres mécanismes mais PAServer fonctionne aussi.

### Plateformes supportées

PAServer existe pour :
- **macOS** : Pour les applications macOS et iOS
- **Linux** : Pour les applications Linux (serveurs, desktop)
- **Android** : Via une connexion USB ou réseau
- **Windows** : Bien que moins courant (débogage Windows distant direct existe)

### Où trouver PAServer ?

PAServer est inclus avec Delphi :
- Chemin typique : `C:\Program Files (x86)\Embarcadero\Studio\[version]\PAServer\`
- Pour macOS : Téléchargeable via le site Embarcadero ou inclus dans le package d'installation
- Pour Linux : Package disponible en téléchargement

## Configuration du débogage à distance

Voyons comment configurer le débogage à distance étape par étape. Nous prendrons l'exemple d'une connexion vers une machine macOS, mais les principes sont similaires pour les autres plateformes.

### Étape 1 : Préparer la machine cible

**Installer PAServer sur la machine cible**

Pour **macOS** :
1. Téléchargez le package PAServer pour macOS depuis le site Embarcadero
2. Copiez-le sur votre Mac
3. Ouvrez le Terminal sur le Mac
4. Naviguez vers le dossier où se trouve PAServer
5. Lancez PAServer : `./paserver`

Pour **Linux** :
1. Téléchargez le package PAServer pour Linux
2. Extrayez l'archive
3. Donnez les droits d'exécution : `chmod +x paserver`
4. Lancez : `./paserver`

**Configuration réseau**

Assurez-vous que :
- La machine cible et la machine de développement sont sur le même réseau (ou accessible via VPN)
- Le pare-feu autorise les connexions sur le port utilisé par PAServer (par défaut : 64211)
- Vous connaissez l'adresse IP de la machine cible

**Obtenir l'adresse IP de la machine cible :**

Sur **macOS** :
```bash
ifconfig | grep "inet "
```

Sur **Linux** :
```bash
ip addr show
```

Sur **Windows** :
```cmd
ipconfig
```

Notez l'adresse IP (par exemple : `192.168.1.100`)

### Étape 2 : Configurer Delphi pour se connecter

**Créer une connexion dans Delphi**

1. Dans Delphi, allez dans **Tools > Options**
2. Naviguez vers **Connection Profile Manager**
3. Cliquez sur **Add** (Ajouter) pour créer un nouveau profil
4. Donnez un nom au profil (ex: "Mac Studio Bureau")
5. Configurez les paramètres :
   - **Platform** : Sélectionnez la plateforme (macOS 64-bit, Linux 64-bit, etc.)
   - **Host name** : Entrez l'adresse IP de la machine cible (ex: `192.168.1.100`)
   - **Port** : Généralement 64211 (valeur par défaut)
   - **Password** : Le mot de passe configuré sur PAServer (optionnel mais recommandé)
6. Cliquez sur **Test Connection** pour vérifier
7. Si la connexion réussit, cliquez sur **OK**

**Configuration du mot de passe PAServer :**

Lorsque vous lancez PAServer pour la première fois, il peut vous demander de définir un mot de passe :

```bash
./paserver -password=VotreMotDePasseSecurise
```

Notez ce mot de passe, vous en aurez besoin dans Delphi.

### Étape 3 : Configurer votre projet

**Ajouter une plateforme cible**

1. Ouvrez votre projet dans Delphi
2. Dans le **Project Manager** (Gestionnaire de projet), faites un clic droit sur le nom du projet
3. Sélectionnez **Add Platform**
4. Choisissez la plateforme cible (ex: macOS 64-bit)
5. Une nouvelle configuration de plateforme apparaît dans le Project Manager

**Configurer les options de déploiement**

1. Sélectionnez la plateforme cible dans le Project Manager
2. Faites un clic droit > **Options**
3. Dans **Deployment**, vérifiez les fichiers qui seront déployés
4. Delphi configure automatiquement les fichiers essentiels

**Associer le profil de connexion**

1. Dans **Project > Options > Connection**
2. Pour la plateforme cible, sélectionnez le profil de connexion que vous avez créé
3. Cliquez sur **OK**

### Étape 4 : Compiler et déployer

**Compiler pour la plateforme cible**

1. Dans la barre d'outils, sélectionnez la plateforme cible (ex: macOS 64-bit)
2. Compilez le projet : **Project > Compile** ou Ctrl+F9
3. Delphi compile le code pour la plateforme sélectionnée

**Déployer l'application**

Le déploiement copie automatiquement l'application et ses dépendances sur la machine cible.

1. Vérifiez que PAServer est en cours d'exécution sur la machine cible
2. Dans Delphi : **Project > Deploy [PlatformName]**
3. L'IDE se connecte à PAServer et transfère les fichiers
4. Vous verrez la progression dans la fenêtre Messages

**Où sont déployés les fichiers ?**

Sur la machine cible, les fichiers sont copiés dans un dossier temporaire, généralement :
- **macOS** : `~/PAServer/scratch-dir/[UserName]-[MachineName]/[ProjectName]/`
- **Linux** : `~/PAServer/scratch-dir/[UserName]-[MachineName]/[ProjectName]/`

### Étape 5 : Déboguer à distance

**Lancer le débogage**

1. Placez des points d'arrêt dans votre code comme d'habitude (clic dans la marge ou F5)
2. Assurez-vous que la plateforme cible est sélectionnée
3. Appuyez sur **F9** (Run) ou cliquez sur le bouton Run

**Ce qui se passe :**

1. Delphi compile le projet pour la plateforme cible
2. L'application est déployée sur la machine distante via PAServer
3. PAServer lance l'application en mode débogage
4. L'IDE Delphi se connecte au débogueur distant
5. Votre application s'exécute sur la machine distante mais vous contrôlez tout depuis Delphi

**Utilisation du débogueur**

Une fois connecté, vous utilisez le débogueur exactement comme en local :

- **Points d'arrêt** : L'exécution s'arrête quand un point d'arrêt est atteint
- **Inspection de variables** : Survolez une variable avec la souris pour voir sa valeur
- **Watch List** : Ajoutez des variables à surveiller
- **Call Stack** : Voyez la pile d'appels
- **Step Into / Step Over** : Exécutez le code ligne par ligne (F7 / F8)
- **Continue** : Reprenez l'exécution normale (F9)

**Important** : Il peut y avoir un léger délai entre vos actions et la réponse, dû à la communication réseau.

## Débogage à distance sous Windows

### Débogage Windows-vers-Windows

Pour déboguer une application Windows sur une autre machine Windows, vous avez plusieurs options :

**Option 1 : Débogueur intégré (sans PAServer)**

Pour les applications Windows, vous pouvez utiliser le débogueur distant natif :

1. Sur la machine cible, activez le débogage distant dans les paramètres Windows
2. Configurez les permissions réseau
3. Dans Delphi, configurez la connexion réseau vers la machine distante
4. Lancez l'application avec **Run > Run Without Debugging** puis attachez le débogueur

**Option 2 : Utiliser PAServer**

Vous pouvez aussi utiliser PAServer sur Windows pour une expérience cohérente avec les autres plateformes :

1. Copiez PAServer sur la machine Windows cible
2. Lancez `PAServer.exe`
3. Configurez le profil de connexion dans Delphi
4. Déboguez comme pour les autres plateformes

**Option 3 : Débogage via Remote Desktop**

Pour des tests rapides, vous pouvez :
1. Connectez-vous à la machine distante via Remote Desktop
2. Copiez l'exécutable compilé
3. Lancez-le directement sur la machine distante
4. Utilisez des logs pour diagnostiquer les problèmes

Cette approche est moins sophistiquée mais rapide à mettre en place.

### Déploiement d'une application pour test distant

Si le débogage complet n'est pas nécessaire, vous pouvez simplement déployer l'exécutable :

```pascal
// Ajouter du logging pour le diagnostic à distance
procedure TForm1.FormCreate(Sender: TObject);
begin
  Logger.Info('Application démarrée sur : ' + GetComputerName);
  Logger.Info('Version OS : ' + TOSVersion.ToString);
end;
```

Puis récupérez le fichier de log pour analyser les problèmes.

## Débogage avancé avec LLDB v12

### Qu'est-ce que LLDB ?

**LLDB** (Low Level Debugger) est le débogueur moderne utilisé par Delphi pour les plateformes Apple (macOS, iOS) et certaines versions de Linux. Delphi 13 intègre la **version 12 de LLDB**, qui apporte des améliorations significatives.

### Nouveautés de LLDB v12

**Performances améliorées**
- Démarrage plus rapide du débogueur
- Réponse plus rapide lors des inspections de variables
- Meilleure gestion de la mémoire

**Support amélioré des types Delphi**
- Meilleure visualisation des types Object Pascal
- Support amélioré des génériques
- Inspection plus précise des chaînes et tableaux dynamiques

**Stabilité accrue**
- Moins de crashs lors du débogage de code complexe
- Meilleure gestion des threads multiples
- Récupération plus fiable après des erreurs

### Utiliser LLDB pour le débogage avancé

LLDB offre des commandes avancées accessibles via la console de débogage.

**Ouvrir la console LLDB**

Pendant une session de débogage :
1. Allez dans **View > Debug Windows > LLDB Console**
2. Vous verrez une console où vous pouvez entrer des commandes LLDB

**Commandes LLDB utiles**

**Afficher les threads :**
```
thread list
```

**Afficher la pile d'appels complète :**
```
bt all
```

**Inspecter une adresse mémoire :**
```
memory read 0x7ffeefbff000
```

**Afficher les registres du processeur :**
```
register read
```

**Évaluer une expression :**
```
expr MonObjet.MaPropriete
```

**Afficher les variables locales :**
```
frame variable
```

**Breakpoint conditionnel avancé :**
```
breakpoint set -f MainUnit.pas -l 42 -c "Age > 18"
```

### Configuration de LLDB dans Delphi

**Paramètres LLDB**

1. Allez dans **Tools > Options**
2. Naviguez vers **Debugger Options**
3. Section **LLDB** :
   - Vous pouvez activer/désactiver certaines fonctionnalités
   - Configurer le timeout de connexion
   - Ajuster les paramètres de logging

**Debugging plus verbeux**

Pour diagnostiquer des problèmes de connexion ou de débogage, activez le logging LLDB :

1. Dans **Tools > Options > Debugger Options**
2. Cochez **Enable LLDB logging**
3. Spécifiez un fichier de log
4. Les détails complets de la communication LLDB seront enregistrés

### Déboguer des crashs difficiles

**Capturer un crash dump**

Quand une application plante sur la machine distante :

1. LLDB peut capturer un crash dump
2. Dans Delphi, allez dans **View > Debug Windows > Modules**
3. Vous pouvez voir l'état de tous les modules chargés
4. Le crash dump contient la pile d'appels au moment du crash

**Analyser post-mortem**

Même si l'application a planté, LLDB conserve l'état :
```
bt  # Voir la stack trace du crash
frame select 0  # Sélectionner le frame où le crash s'est produit
frame variable  # Voir les variables locales à ce moment
```

## Débogage mobile (iOS et Android)

### Débogage iOS

Le débogage d'applications iOS nécessite :

**Prérequis :**
- Un Mac avec Xcode installé
- PAServer en cours d'exécution sur le Mac
- Un iPhone/iPad connecté au Mac (pour tester sur device réel)
- Un certificat de développeur Apple (pour déployer sur device)

**Configuration :**

1. Configurez le profil de connexion vers votre Mac
2. Sélectionnez la plateforme **iOS Device 64-bit** ou **iOS Simulator**
3. Dans **Project > Options > Provisioning**, configurez votre certificat et profil de provisioning
4. Déployez et déboguez comme une application macOS

**Particularités iOS :**
- Le déploiement initial peut être plus long (code signing)
- Les permissions iOS doivent être configurées (Info.plist)
- Certaines fonctionnalités nécessitent des autorisations spécifiques

### Débogage Android

Pour Android, le processus est similaire mais utilise ADB (Android Debug Bridge) :

**Prérequis :**
- SDK Android installé et configuré dans Delphi
- Un device Android en mode développeur
- Connexion USB ou connexion réseau (ADB over network)

**Configuration :**

1. Activez le **Mode développeur** sur votre appareil Android :
   - Paramètres > À propos du téléphone
   - Tapotez 7 fois sur "Numéro de build"
   - Activez "Débogage USB" dans les options développeur

2. Connectez votre appareil via USB
3. Dans Delphi, sélectionnez **Android 32-bit** ou **Android 64-bit**
4. Le device devrait apparaître dans la liste des cibles
5. Déployez et déboguez

**Débogage via WiFi (Android) :**

Vous pouvez déboguer sans câble USB :

```bash
# Sur votre PC, avec le device connecté en USB initialement
adb tcpip 5555
adb connect [IP_DU_DEVICE]:5555

# Maintenant vous pouvez débrancher l'USB
```

Dans Delphi, le device apparaîtra comme cible réseau.

## Résolution de problèmes courants

### Impossible de se connecter à PAServer

**Vérifications :**

1. **PAServer est-il lancé ?**
   - Sur la machine cible, vérifiez que PAServer est bien en cours d'exécution
   - Vous devriez voir un message "Waiting for connection on port 64211"

2. **Pare-feu :**
   - Vérifiez que le port 64211 n'est pas bloqué
   - Sur macOS : **Préférences Système > Sécurité > Pare-feu > Options du pare-feu**
   - Sur Windows : **Pare-feu Windows Defender > Paramètres avancés > Règles entrantes**
   - Sur Linux : `sudo ufw allow 64211` ou configurez iptables

3. **Adresse IP correcte :**
   - Vérifiez que vous utilisez la bonne adresse IP
   - Essayez de pinguer la machine cible : `ping [IP]`

4. **Mot de passe :**
   - Vérifiez que le mot de passe dans Delphi correspond à celui de PAServer
   - PAServer affiche "Password: ..." au démarrage si un mot de passe est configuré

5. **Réseau :**
   - Assurez-vous que les deux machines sont sur le même réseau
   - Vérifiez qu'aucun VPN ne bloque la connexion

**Test de connectivité :**

Sur votre machine de développement :
```bash
telnet [IP_DE_LA_CIBLE] 64211
```

Si vous obtenez une connexion, le réseau fonctionne.

### L'application ne démarre pas sur la cible

**Causes possibles :**

1. **Dépendances manquantes :**
   - Vérifiez dans **Project > Deployment** que tous les fichiers nécessaires sont déployés
   - DLLs, ressources, frameworks sur macOS, etc.

2. **Permissions :**
   - Sur macOS/Linux, vérifiez que l'exécutable a les droits d'exécution
   - PAServer devrait gérer cela, mais vérifiez manuellement si nécessaire

3. **Architecture incorrecte :**
   - Assurez-vous de compiler pour la bonne architecture (32-bit vs 64-bit)
   - Sur macOS moderne, utilisez 64-bit

4. **Console de sortie :**
   - Regardez la console PAServer sur la machine cible
   - Elle affiche souvent des messages d'erreur utiles

### Points d'arrêt non atteints

**Vérifications :**

1. **Informations de débogage :**
   - **Project > Options > Delphi Compiler > Compiling**
   - Assurez-vous que **Debug Information** est coché

2. **Code mort :**
   - Le code avec le point d'arrêt est-il réellement exécuté ?
   - Ajoutez un log juste avant pour vérifier

3. **Optimisation :**
   - Désactivez l'optimisation du compilateur en mode Debug
   - **Project > Options > Delphi Compiler > Compiling > Optimization** = décoché

4. **Chemin des sources :**
   - Vérifiez que l'IDE trouve bien les fichiers sources
   - **Tools > Options > Delphi Options > Library > Library Path**

### Débogage lent

**Causes et solutions :**

1. **Latence réseau :**
   - Vérifiez votre connexion réseau
   - Utilisez une connexion filaire plutôt que WiFi si possible
   - Si vous déboguez via Internet, considérez un VPN optimisé

2. **Trop de points d'arrêt :**
   - Limitez le nombre de points d'arrêt actifs
   - Désactivez ceux qui ne sont pas nécessaires

3. **Watch expressions complexes :**
   - Les expressions complexes dans la Watch List sont évaluées à chaque pause
   - Simplifiez ou retirez-les temporairement

4. **Bande passante :**
   - Le transfert de gros fichiers durant le déploiement peut être lent
   - Déployez une première fois, puis utilisez **Build** au lieu de **Clean + Build**

### "Symbol file not found"

Ce message apparaît quand le débogueur ne trouve pas les symboles de débogage.

**Solution :**

1. Recompilez le projet complètement : **Project > Build**
2. Vérifiez que les informations de débogage sont activées
3. Redéployez l'application : **Project > Deploy**

## Sécurité du débogage à distance

### Risques de sécurité

Le débogage à distance ouvre des ports réseau et expose votre application. Soyez conscient des risques :

**Accès non autorisé :**
- Sans mot de passe, n'importe qui sur le réseau peut se connecter à PAServer
- Un attaquant pourrait déployer du code malveillant

**Interception :**
- La communication entre l'IDE et PAServer n'est pas chiffrée par défaut
- Des données sensibles (code source, données applicatives) transitent sur le réseau

**Porte dérobée :**
- Laisser PAServer en cours d'exécution permanent crée une vulnérabilité

### Bonnes pratiques de sécurité

**1. Toujours utiliser un mot de passe fort**

```bash
./paserver -password=UnMotDePasseComplexe123!@#
```

Ne laissez JAMAIS PAServer sans mot de passe sur un réseau partagé.

**2. Limiter l'accès réseau**

Configurez le pare-feu pour n'autoriser que votre machine de développement :

Sur **macOS** :
```bash
# Autoriser uniquement depuis une IP spécifique
sudo pfctl -e
sudo pfctl -f /etc/pf.conf
```

Sur **Linux** (avec UFW) :
```bash
# Autoriser uniquement depuis 192.168.1.50
sudo ufw allow from 192.168.1.50 to any port 64211
```

**3. Utiliser un réseau privé**

- Déboguez sur un réseau local privé, pas sur Internet directement
- Utilisez un VPN si vous devez déboguer via Internet

**4. Fermer PAServer après usage**

Ne laissez pas PAServer tourner en permanence. Lancez-le seulement quand nécessaire :

```bash
./paserver
# Utilisez Ctrl+C pour arrêter proprement quand vous avez terminé
```

**5. Surveiller les connexions**

PAServer affiche les connexions entrantes. Surveillez ces messages pour détecter des accès suspects :

```
PAServer version 22.0.47991.6037 Ready.
Accepted connection from 192.168.1.50:54321
```

Si vous voyez une IP inconnue, arrêtez PAServer immédiatement.

**6. Ne pas utiliser en production**

Ne déployez JAMAIS PAServer sur un serveur de production. C'est un outil de développement uniquement.

**7. Considérer un tunnel SSH**

Pour une sécurité maximale, créez un tunnel SSH chiffré :

```bash
# Sur votre machine de développement
ssh -L 64211:localhost:64211 user@machine-cible

# Puis configurez Delphi pour se connecter à localhost:64211
```

Le trafic sera chiffré via SSH.

## Alternatives au débogage à distance

Selon votre situation, d'autres approches peuvent être plus appropriées :

### Logging intensif

Pour des problèmes difficiles à reproduire :

```pascal
procedure OperationComplexe;
begin
  Logger.Debug('Début OperationComplexe');
  Logger.Debug('Paramètre A = ' + IntToStr(A));

  try
    TraiterA;
    Logger.Debug('TraiterA terminé avec succès');

    TraiterB;
    Logger.Debug('TraiterB terminé avec succès');
  except
    on E: Exception do
    begin
      Logger.Error('Exception dans OperationComplexe', E);
      raise;
    end;
  end;

  Logger.Debug('Fin OperationComplexe');
end;
```

Récupérez ensuite le fichier de log depuis la machine distante.

### Rapports d'erreur automatiques

Utilisez des outils comme **EurekaLog** ou **madExcept** qui capturent automatiquement les crashs et génèrent des rapports détaillés.

```pascal
// Configuration EurekaLog
procedure ConfigurerEurekaLog;
begin
  ExceptionLog1.OnException := GererException;
  ExceptionLog1.Active := True;
end;

procedure GererException(Sender: TObject; E: Exception; var Handled: Boolean);
begin
  // EurekaLog capture automatiquement la stack trace
  // et peut envoyer un rapport par email
  Handled := True;
end;
```

### Machines virtuelles locales

Créez des VMs en local avec différentes configurations :

**Avantages :**
- Pas de latence réseau
- Débogage aussi rapide qu'en local
- Contrôle total de l'environnement
- Pas de problèmes de sécurité

**Outils :**
- VirtualBox (gratuit)
- VMware Workstation
- Hyper-V (inclus dans Windows Pro)
- Parallels (macOS)

### TeamViewer / Remote Desktop

Pour des tests ponctuels, connectez-vous simplement à la machine distante et exécutez l'application localement :

1. Connectez-vous via Remote Desktop
2. Copiez l'exécutable compilé
3. Lancez l'application
4. Utilisez les logs pour diagnostiquer

Moins sophistiqué que le débogage à distance, mais rapide à mettre en place.

## Conseils pour débutants

### Commencez par maîtriser le débogage local

Avant de vous lancer dans le débogage à distance, assurez-vous de bien maîtriser :
- Les points d'arrêt
- L'inspection de variables
- La navigation dans le code (Step Into, Step Over)
- La Watch List

Le débogage à distance utilise exactement les mêmes concepts.

### Testez d'abord la connexion

Avant de déboguer, vérifiez que la connexion fonctionne :
1. Lancez PAServer sur la cible
2. Dans Delphi, testez la connexion (**Test Connection**)
3. Si ça fonctionne, alors tentez un déploiement simple

Résolvez les problèmes de connexion avant d'essayer de déboguer.

### Commencez avec une application simple

Ne testez pas le débogage à distance avec votre application complexe de 50 000 lignes. Créez une petite application de test :

```pascal
program TestDistant;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

var
  i: Integer;
begin
  WriteLn('Application de test distant');

  for i := 1 to 10 do
  begin
    WriteLn('Compteur : ', i);  // Mettez un point d'arrêt ici
    Sleep(1000);
  end;

  WriteLn('Terminé. Appuyez sur Entrée.');
  ReadLn;
end.
```

Si vous arrivez à déboguer cela, vous pouvez passer à des applications plus complexes.

### Documentez votre configuration

La première configuration prend du temps. Documentez les étapes :
- L'adresse IP de la machine cible
- Le mot de passe PAServer
- Les paramètres réseau spécifiques
- Les problèmes rencontrés et leurs solutions

Cela vous fera gagner du temps lors des prochaines sessions.

### Utilisez les logs en complément

Le débogage à distance peut être lent. Combinez-le avec des logs pour être plus efficace :

1. Ajoutez des logs à votre code
2. Déployez et exécutez
3. Consultez les logs pour avoir une vue d'ensemble
4. Utilisez le débogueur uniquement pour les zones problématiques spécifiques

### Patience avec la latence

Le débogage à distance est plus lent que le débogage local. C'est normal. Soyez patient et anticipez les délais lors de :
- Le déploiement initial
- L'atteinte d'un point d'arrêt
- L'inspection de variables complexes

### Gardez PAServer à jour

Utilisez toujours la version de PAServer qui correspond à votre version de Delphi. Les incompatibilités de version peuvent causer des problèmes mystérieux.

### Familiarisez-vous avec la ligne de commande

Beaucoup d'opérations de débogage à distance nécessitent un minimum de connaissances en ligne de commande :
- Lancer PAServer
- Vérifier les connexions réseau
- Gérer les permissions de fichiers

Prenez le temps d'apprendre les bases du Terminal (macOS/Linux) ou de l'Invite de commandes (Windows).

## Workflow typique de débogage à distance

Voici un processus typique pour déboguer un problème sur une machine distante :

### Phase 1 : Préparation

1. Identifiez le problème et sa plateforme
2. Configurez la machine cible avec PAServer
3. Créez/vérifiez le profil de connexion dans Delphi
4. Testez la connexion

### Phase 2 : Investigation initiale

1. Ajoutez des logs au code pour avoir une vue d'ensemble
2. Compilez et déployez l'application
3. Exécutez sans débogueur et consultez les logs
4. Identifiez la zone problématique

### Phase 3 : Débogage ciblé

1. Placez des points d'arrêt dans la zone identifiée
2. Lancez le débogueur distant (F9)
3. Reproduisez le problème
4. Inspectez les variables et la pile d'appels
5. Identifiez la cause

### Phase 4 : Correction et validation

1. Corrigez le code
2. Recompilez et redéployez
3. Testez la correction
4. Vérifiez avec les logs que tout fonctionne
5. Fermez PAServer

## Conclusion

Le débogage à distance est une compétence avancée mais essentielle pour tout développeur Delphi travaillant sur des applications multi-plateformes ou destinées à des environnements spécifiques.

**Points clés à retenir :**

**Architecture :** Le débogage à distance repose sur PAServer qui s'exécute sur la machine cible et communique avec l'IDE Delphi via le réseau.

**Configuration :** Nécessite une configuration initiale (PAServer, profil de connexion, réseau) mais ensuite le processus est fluide.

**Utilisation :** Une fois configuré, le débogage à distance fonctionne comme le débogage local avec les mêmes outils et techniques.

**Plateformes :** Essentiel pour macOS, iOS, Android et Linux. Optionnel mais utile pour Windows distant.

**LLDB v12 :** Offre des capacités avancées pour le débogage sur plateformes Apple et Linux avec de meilleures performances et stabilité.

**Sécurité :** Utilisez toujours un mot de passe fort, limitez l'accès réseau, et n'exécutez PAServer que pendant les sessions de développement.

**Alternatives :** Selon la situation, considérez les logs, les rapports d'erreur automatiques, ou les VMs locales.

**Pratique :** Commencez avec des applications simples pour vous familiariser avant de déboguer des projets complexes.

Le débogage à distance peut sembler intimidant au début, mais avec de la pratique, il devient un outil naturel et indispensable dans votre arsenal de développeur Delphi. Il vous permet de diagnostiquer et résoudre des problèmes qui seraient autrement impossibles à corriger, et ouvre la porte au développement véritablement multi-plateformes.

N'hésitez pas à expérimenter avec une configuration simple, et progressivement vous développerez l'expertise nécessaire pour déboguer efficacement sur n'importe quelle plateforme supportée par Delphi.

⏭️ [Débogage avancé avec LLDB v12](/12-debogage-et-tests/06.1-debogage-avance-avec-lldb-v12.md)
