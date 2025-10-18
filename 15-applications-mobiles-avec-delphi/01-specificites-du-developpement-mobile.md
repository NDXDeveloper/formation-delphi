🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.1 Spécificités du développement mobile avec Delphi

## Introduction

Le développement d'applications mobiles avec Delphi présente de nombreuses particularités qui le distinguent du développement d'applications de bureau (desktop). Comprendre ces spécificités est essentiel pour créer des applications mobiles performantes et conformes aux attentes des utilisateurs modernes.

Avec Delphi, vous pouvez développer des applications natives pour iOS et Android à partir d'une seule base de code, grâce à la technologie FireMonkey (FMX). Cette approche multi-plateforme vous permet de gagner du temps tout en atteignant un large public.

## Les différences fondamentales

### Architecture matérielle

Les appareils mobiles présentent des contraintes matérielles significatives par rapport aux ordinateurs de bureau :

**Processeur** : Les smartphones et tablettes utilisent des processeurs ARM optimisés pour la faible consommation d'énergie plutôt que pour la performance brute. Votre code doit être efficace et éviter les calculs intensifs inutiles.

**Mémoire limitée** : Contrairement aux PC modernes qui disposent de plusieurs gigaoctets de RAM, les applications mobiles doivent fonctionner avec des ressources mémoire limitées. Une application qui consomme trop de mémoire sera fermée automatiquement par le système d'exploitation.

**Stockage** : L'espace de stockage est précieux sur mobile. Vos applications doivent être légères et gérer intelligemment les données locales.

**Batterie** : La consommation électrique est un critère crucial. Les opérations réseau fréquentes, le GPS constant ou les animations intensives peuvent rapidement épuiser la batterie de l'utilisateur.

### Paradigme d'interaction

L'interface utilisateur mobile repose sur des principes différents du desktop :

**Écran tactile** : L'absence de souris et de clavier physique change complètement la façon dont l'utilisateur interagit avec l'application. Les boutons doivent être suffisamment grands pour être touchés avec un doigt (minimum 44x44 pixels recommandé).

**Gestes** : Les utilisateurs s'attendent à pouvoir utiliser des gestes naturels comme le glissement (swipe), le pincement (pinch) pour zoomer, ou le balayage (scroll).

**Orientation de l'écran** : Votre application doit généralement supporter à la fois le mode portrait et paysage, et s'adapter dynamiquement lors du changement d'orientation.

**Taille d'écran variable** : Contrairement au desktop où vous pouvez contrôler la taille de fenêtre, sur mobile vous devez concevoir pour une multitude de résolutions et de formats d'écran.

## Cycle de vie d'une application mobile

Les applications mobiles ont un cycle de vie spécifique que vous devez comprendre et gérer correctement dans Delphi :

### Les états de l'application

Une application mobile peut se trouver dans différents états :

**Active** : L'application est au premier plan et l'utilisateur interagit avec elle.

**En arrière-plan** : L'utilisateur a quitté l'application (pour répondre à un appel, consulter une autre app, etc.) mais elle n'est pas fermée. Sur iOS et Android, les applications en arrière-plan sont fortement limitées dans leurs actions.

**Suspendue** : Le système a mis l'application en pause pour économiser les ressources. Elle peut être réactivée instantanément ou fermée par le système si nécessaire.

**Fermée** : L'application a été complètement arrêtée.

### Gestion des événements de cycle de vie

Dans Delphi, vous devez intercepter les événements de cycle de vie pour sauvegarder l'état de votre application :

```pascal
// L'application passe en arrière-plan
procedure TForm1.FormDeactivate(Sender: TObject);
begin
  // Sauvegarder l'état actuel
  // Arrêter les animations
  // Fermer les connexions réseau non essentielles
end;

// L'application revient au premier plan
procedure TForm1.FormActivate(Sender: TObject);
begin
  // Restaurer l'état
  // Relancer les animations
  // Reconnecter si nécessaire
end;
```

**Point important** : L'utilisateur peut quitter votre application à tout moment, et celle-ci peut être fermée par le système sans avertissement. Vous devez sauvegarder régulièrement l'état de l'application.

## Permissions et sécurité

Les systèmes d'exploitation mobiles modernes protègent la vie privée des utilisateurs en imposant un système de permissions strict.

### Types de permissions

Votre application doit demander explicitement l'autorisation d'accéder à certaines fonctionnalités :

- Localisation GPS
- Appareil photo et photothèque
- Microphone
- Contacts
- Calendrier
- Notifications push
- Bluetooth
- Accès réseau

### Demande de permissions dans Delphi

Vous devez configurer les permissions dans les paramètres du projet et les demander au moment opportun dans votre code :

```pascal
uses
  FMX.MediaLibrary;

procedure TForm1.BtnPhotoClick(Sender: TObject);
begin
  // Vérifier et demander la permission
  PermissionsService.RequestPermissions(
    [JStringToString(TJManifest_permission.JavaClass.CAMERA)],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        // Permission accordée, utiliser la caméra
        PrendrePhoto;
      end
      else
        ShowMessage('Permission refusée');
    end
  );
end;
```

**Bonne pratique** : Expliquez toujours à l'utilisateur pourquoi vous avez besoin d'une permission avant de la demander.

## Contraintes de plateformes

Chaque système d'exploitation mobile a ses propres règles et conventions.

### iOS (Apple)

- Interface utilisateur suivant les Human Interface Guidelines d'Apple
- Processus de validation strict avant publication sur l'App Store
- Obligation d'utiliser un Mac pour la compilation finale
- Gestion des certificats et profils de provisionnement
- Restrictions sur les processus en arrière-plan
- Pas d'accès au système de fichiers hors du sandbox de l'application

### Android (Google)

- Interface utilisateur suivant les Material Design Guidelines
- Processus de publication plus souple sur le Play Store
- Grande diversité de fabricants et de versions d'Android
- Permissions plus granulaires
- Plus de liberté dans les tâches en arrière-plan
- Fragmentation : votre application doit fonctionner sur de nombreux appareils différents

### Compilation et déploiement dans Delphi

Delphi gère ces différences à travers le PAServer (Platform Assistant Server) pour iOS et macOS, et le SDK Manager pour Android. Vous pouvez compiler pour différentes plateformes depuis le même IDE en sélectionnant simplement la cible de compilation.

## Considérations de performance

Les performances sont critiques sur mobile où les ressources sont limitées.

### Optimisation de l'interface

- **Utilisez des contrôles natifs** : FireMonkey offre des contrôles natifs qui sont plus performants que les contrôles stylisés
- **Limitez les animations complexes** : Les effets visuels consomment beaucoup de ressources
- **Optimisez les images** : Réduisez leur taille et résolution au strict nécessaire
- **Évitez les mises à jour UI trop fréquentes** : Rafraîchir l'écran 60 fois par seconde n'est généralement pas nécessaire

### Gestion de la mémoire

```pascal
// Libérer les ressources non utilisées
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Libérer explicitement les objets
  MonObjet.Free;
  // Vider les caches
  ImageCache.Clear;
end;
```

### Opérations réseau

- **Travaillez en asynchrone** : Ne bloquez jamais l'interface utilisateur lors d'un appel réseau
- **Gérez les connexions intermittentes** : L'utilisateur peut perdre la connexion à tout moment
- **Optimisez la taille des données échangées** : La bande passante mobile est limitée et parfois coûteuse

## Mode de connexion

Les applications mobiles doivent gérer intelligemment les différents types de connexion :

### Types de connexion

- **WiFi** : Connexion rapide et généralement illimitée
- **4G/5G** : Rapide mais peut être limitée en volume de données
- **3G** : Plus lente et limitée
- **Hors ligne** : Aucune connexion

### Détection du type de connexion dans Delphi

```pascal
uses
  System.Net.HttpClient;

function EstConnecteEnWiFi: Boolean;
begin
  // Logique pour détecter le type de connexion
  // Adapter le comportement de l'application en conséquence
end;
```

**Stratégie recommandée** : Votre application doit idéalement fonctionner hors ligne avec une synchronisation des données lorsque la connexion est rétablie.

## Notifications

Les notifications sont un canal de communication essentiel avec vos utilisateurs.

### Notifications locales

Déclenchées directement par l'application sans passer par un serveur :

```pascal
uses
  FMX.Notification;

procedure TForm1.EnvoyerNotification;
var
  NotifCenter: TNotificationCenter;
  Notification: TNotification;
begin
  NotifCenter := TNotificationCenter.Create(nil);
  try
    Notification := NotifCenter.CreateNotification;
    try
      Notification.Name := 'MaNotification';
      Notification.Title := 'Rappel';
      Notification.AlertBody := 'N''oubliez pas votre rendez-vous !';
      Notification.FireDate := Now + EncodeTime(1, 0, 0, 0); // Dans 1 heure

      NotifCenter.ScheduleNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotifCenter.Free;
  end;
end;
```

### Notifications push

Envoyées depuis un serveur, elles nécessitent une configuration plus complexe avec Firebase Cloud Messaging (FCM) pour Android et Apple Push Notification Service (APNs) pour iOS.

## Stockage local

Les applications mobiles doivent souvent stocker des données localement.

### Options de stockage

**Fichiers locaux** : Pour les configurations, paramètres et données simples

```pascal
uses
  System.IOUtils;

procedure SauvegarderParametres;
var
  CheminFichier: string;
begin
  // Obtenir le chemin du dossier Documents de l'application
  CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'parametres.ini');
  // Sauvegarder vos données
end;
```

**Base de données SQLite** : Pour des données structurées plus complexes, SQLite est intégré dans Delphi via FireDAC

**Préférences système** : Pour des paramètres simples clé-valeur

## Conclusion

Le développement mobile avec Delphi requiert une compréhension de ces spécificités pour créer des applications performantes et conformes aux attentes des utilisateurs. Les principes clés à retenir sont :

- Optimiser pour des ressources limitées (mémoire, batterie, processeur)
- Concevoir pour le tactile et les différentes tailles d'écran
- Gérer correctement le cycle de vie de l'application
- Respecter les permissions et la vie privée des utilisateurs
- S'adapter aux contraintes de chaque plateforme (iOS et Android)
- Fonctionner de manière fiable même avec une connexion intermittente

Dans les prochaines sections, nous explorerons en détail comment implémenter ces concepts dans vos applications Delphi mobiles avec FireMonkey.

⏭️ [Interface utilisateur tactile](/15-applications-mobiles-avec-delphi/02-interface-utilisateur-tactile.md)
