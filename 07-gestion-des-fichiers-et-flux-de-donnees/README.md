🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 7 : Gestion des fichiers et flux de données

## Introduction

Bienvenue dans ce chapitre essentiel consacré à la gestion des fichiers et des flux de données en Delphi. La capacité à lire, écrire, manipuler et gérer des fichiers est l'une des compétences fondamentales de tout développeur.

**Analogie simple :** Pensez à votre application comme à une personne qui a besoin de communiquer avec le monde extérieur :
- **Lire un fichier** = écouter ce que quelqu'un vous dit
- **Écrire un fichier** = écrire une lettre ou un message
- **Manipuler des données** = organiser et traiter les informations reçues
- **Persister des données** = garder une trace écrite pour plus tard

Sans la capacité de lire et écrire des fichiers, votre application serait isolée, incapable de sauvegarder des données, d'échanger des informations avec d'autres programmes, ou de conserver l'état entre deux sessions.

---

## Pourquoi la gestion de fichiers est-elle importante ?

### Dans la vie quotidienne d'une application

Presque toutes les applications que vous utilisez quotidiennement manipulent des fichiers :

1. **Microsoft Word** : Ouvre et sauvegarde des documents `.docx`
2. **Spotify** : Lit des fichiers audio, stocke les paramètres
3. **Photoshop** : Ouvre des images, exporte dans différents formats
4. **Votre navigateur** : Télécharge des fichiers, sauvegarde l'historique
5. **Un jeu vidéo** : Sauvegarde votre progression, charge des ressources

Sans gestion de fichiers, aucune de ces applications ne pourrait fonctionner !

### Les besoins typiques en entreprise

Dans un contexte professionnel, la gestion de fichiers est encore plus cruciale :

- **Import/Export de données** : Échanger des informations avec d'autres systèmes
- **Configuration** : Stocker les paramètres de l'application
- **Logs** : Enregistrer l'activité pour le débogage et l'audit
- **Sauvegardes** : Préserver les données critiques
- **Rapports** : Générer des documents pour les utilisateurs
- **Communication** : Échanger des fichiers entre applications
- **Persistance** : Conserver l'état de l'application entre les sessions

---

## Vue d'ensemble du chapitre

Ce chapitre est organisé en huit sections progressives qui vous emmèneront des bases jusqu'aux techniques avancées :

### 📄 7.1 - Lecture/écriture de fichiers texte
Vous découvrirez comment manipuler les fichiers texte, le format le plus simple et universel. C'est le point de départ idéal pour comprendre les concepts fondamentaux.

**Ce que vous apprendrez :**
- Lire le contenu d'un fichier texte
- Écrire et modifier des fichiers
- Gérer les encodages (UTF-8, ANSI)
- Traiter les fichiers ligne par ligne

**Exemples pratiques :**
- Créer un système de logs
- Lire des fichiers de configuration
- Compter les occurrences de mots

### 🔢 7.2 - Manipulation de fichiers binaires
Les fichiers binaires stockent les données dans leur format natif, ce qui les rend plus compacts et rapides. Vous apprendrez à travailler avec ces formats.

**Ce que vous apprendrez :**
- Comprendre la différence entre texte et binaire
- Lire et écrire des données structurées
- Créer vos propres formats de fichiers
- Gérer des records et structures complexes

**Exemples pratiques :**
- Mini base de données personnelle
- Format de sauvegarde de jeu
- Système de configuration binaire

### 🌊 7.3 - Utilisation des TStream et classes dérivées
Les streams (flux) sont l'abstraction qui unifie toutes les opérations sur les données. C'est un concept puissant à maîtriser.

**Ce que vous apprendrez :**
- Le concept de stream (flux de données)
- TFileStream, TMemoryStream, TStringStream
- Copier, transformer et manipuler des streams
- Créer vos propres classes de stream

**Exemples pratiques :**
- Buffer de données en mémoire
- Copie efficace de fichiers
- Traitement par flux

### 💾 7.4 - Sérialisation et persistance d'objets
La sérialisation permet de sauvegarder des objets complexes et de les recharger plus tard. C'est essentiel pour la sauvegarde de l'état.

**Ce que vous apprendrez :**
- Transformer des objets en données stockables
- Différentes méthodes de sérialisation
- Gestion des versions de format
- Sauvegarder et restaurer l'état d'une application

**Exemples pratiques :**
- Système de configuration complet
- Sauvegarde de personnages de jeu
- Persistance de l'état de l'interface

### 📦 7.5 - Compression et décompression
La compression réduit la taille des fichiers et accélère les transferts. Vous découvrirez comment l'utiliser efficacement.

**Ce que vous apprendrez :**
- Principes de la compression
- Utiliser ZLib et les archives ZIP
- Choisir le bon niveau de compression
- Créer des archives protégées

**Exemples pratiques :**
- Système de sauvegarde compressée
- Archivage automatique de fichiers
- Réduction de la taille des logs

### ⚙️ 7.6 - Traitement par lots (Batch)
Automatiser le traitement de nombreux fichiers en une seule opération pour gagner un temps précieux.

**Ce que vous apprendrez :**
- Lister et filtrer des fichiers
- Renommer, copier, déplacer en masse
- Créer des systèmes de traitement automatique
- Gérer la progression et les erreurs

**Exemples pratiques :**
- Convertisseur d'images par lots
- Organisateur de fichiers automatique
- Système de nettoyage planifié

### 📊 7.7 - Utilisation de formats modernes (JSON, XML, YAML)
Les formats structurés sont le standard actuel pour l'échange de données. Indispensable dans le développement moderne.

**Ce que vous apprendrez :**
- Lire et écrire du JSON
- Manipuler des documents XML
- Comprendre YAML
- Communiquer avec des API REST

**Exemples pratiques :**
- Configuration d'application en JSON
- Client API REST
- Export de données structurées

### 📈 7.8 - Manipulation de fichiers CSV et Excel
Les formats tabulaires sont omniprésents dans le monde professionnel. Vous apprendrez à les maîtriser.

**Ce que vous apprendrez :**
- Lire et écrire des fichiers CSV
- Manipuler Excel (avec ou sans l'application)
- Convertir entre formats
- Générer des rapports

**Exemples pratiques :**
- Importateur de contacts
- Générateur de rapports Excel
- Convertisseur CSV/Excel

---

## Concepts fondamentaux à connaître

Avant de plonger dans les détails, voici quelques concepts essentiels qui reviendront tout au long de ce chapitre.

### Fichiers et chemins

Un **fichier** est une unité de stockage sur le disque. En Delphi, vous manipulez les fichiers en utilisant leur **chemin** (path).

```pascal
// Chemin absolu (complet)
'C:\Documents\MonFichier.txt'

// Chemin relatif (par rapport au dossier de l'application)
'.\Data\Config.ini'

// Obtenir le chemin de l'application
ExtractFilePath(ParamStr(0))
```

### Types de données

Les fichiers peuvent contenir différents types de données :

1. **Texte** : Lisible par un humain (`.txt`, `.log`, `.csv`)
2. **Binaire** : Format brut, plus compact (`.dat`, `.bin`, `.exe`)
3. **Structuré** : Données organisées (`.json`, `.xml`, `.yaml`)
4. **Propriétaire** : Format spécifique (`.docx`, `.xlsx`, `.pdf`)

### Opérations de base

Toutes les manipulations de fichiers se résument à quelques opérations fondamentales :

```pascal
// Vérifier l'existence
if FileExists('fichier.txt') then
  ShowMessage('Le fichier existe');

// Créer un fichier
// (différentes méthodes selon le type)

// Lire un fichier
var Contenu := TFile.ReadAllText('fichier.txt');

// Écrire un fichier
TFile.WriteAllText('fichier.txt', 'Contenu');

// Copier un fichier
TFile.Copy('source.txt', 'destination.txt');

// Déplacer un fichier
TFile.Move('ancien.txt', 'nouveau.txt');

// Supprimer un fichier
TFile.Delete('fichier.txt');
```

### Gestion des erreurs

La manipulation de fichiers peut échouer pour de nombreuses raisons. Il est crucial de gérer les erreurs :

```pascal
try
  // Opération sur fichier
  var Contenu := TFile.ReadAllText('fichier.txt');
except
  on E: EFileNotFoundException do
    ShowMessage('Fichier introuvable');
  on E: EInOutError do
    ShowMessage('Erreur d''accès au fichier');
  on E: Exception do
    ShowMessage('Erreur inattendue : ' + E.Message);
end;
```

**Erreurs courantes :**
- Fichier introuvable
- Permissions insuffisantes
- Disque plein
- Fichier en cours d'utilisation
- Chemin invalide

### Encodage de caractères

Les fichiers texte utilisent différents encodages pour représenter les caractères, notamment les accents :

```pascal
// UTF-8 (recommandé, universel)
TFile.WriteAllText('fichier.txt', 'Contenu avec accents éàç', TEncoding.UTF8);

// ANSI (ancien standard Windows)
TFile.WriteAllText('fichier.txt', 'Contenu', TEncoding.ANSI);

// Unicode (UTF-16)
TFile.WriteAllText('fichier.txt', 'Contenu', TEncoding.Unicode);
```

**Règle d'or :** Utilisez toujours **UTF-8** pour les nouveaux fichiers. C'est le standard universel qui gère tous les caractères internationaux.

---

## Philosophie de ce chapitre

### Approche progressive

Ce chapitre est conçu pour vous accompagner progressivement :

1. **Commencer simple** : Fichiers texte basiques
2. **Comprendre les concepts** : Binaire, streams
3. **Maîtriser les techniques** : Sérialisation, compression
4. **Appliquer en pratique** : Batch, formats modernes
5. **Résoudre des problèmes réels** : CSV, Excel

### Apprentissage par l'exemple

Chaque section contient de nombreux exemples pratiques et complets que vous pourrez :
- **Copier-coller** directement dans vos projets
- **Adapter** à vos besoins spécifiques
- **Comprendre** grâce aux commentaires détaillés
- **Étendre** pour créer vos propres solutions

### Bonnes pratiques intégrées

Tout au long du chapitre, vous découvrirez :
- ✅ Les façons recommandées de faire
- ❌ Les erreurs courantes à éviter
- 💡 Les astuces pour optimiser
- ⚠️ Les pièges à connaître

---

## Outils et unités Delphi

Voici les principales unités que nous utiliserons dans ce chapitre :

```pascal
uses
  // Manipulation de fichiers moderne
  System.IOUtils,          // TFile, TDirectory, TPath

  // Streams et flux
  System.Classes,          // TStream, TFileStream, TMemoryStream
  System.SysUtils,         // Fonctions utilitaires

  // Formats structurés
  System.JSON,             // Manipulation JSON
  Xml.XMLIntf,             // Manipulation XML
  Xml.XMLDoc,

  // Compression
  System.Zip,              // Archives ZIP
  System.ZLib,             // Compression ZLib

  // Encodage
  System.NetEncoding;      // Encodage de données
```

Ces unités font partie de Delphi et ne nécessitent aucune installation supplémentaire.

---

## Prérequis

Pour tirer le meilleur parti de ce chapitre, vous devriez être à l'aise avec :

✅ **Bases de Delphi**
- Créer un projet simple
- Utiliser des variables et types de base
- Comprendre les procédures et fonctions

✅ **Concepts de programmation**
- Structures de contrôle (if, for, while)
- Gestion des exceptions (try/except)
- Classes et objets (notions de base)

✅ **Interface utilisateur**
- Placer des composants sur un formulaire
- Gérer des événements simples

Si vous avez des lacunes sur ces points, n'hésitez pas à réviser les chapitres précédents. Cependant, ce chapitre reste accessible aux débutants motivés !

---

## Conseils pour réussir

### 1. Pratiquez avec de vrais fichiers

Créez de petits fichiers de test et expérimentez avec le code fourni. N'ayez pas peur de faire des erreurs !

### 2. Testez chaque exemple

Copiez les exemples dans Delphi et exécutez-les. Modifiez-les, cassez-les, corrigez-les. C'est en expérimentant qu'on apprend le mieux.

### 3. Créez vos propres projets

Après chaque section, essayez de créer un petit projet personnel qui utilise les concepts appris. Par exemple :
- Un gestionnaire de notes (section 7.1)
- Un carnet d'adresses (section 7.4)
- Un organiseur de photos (section 7.6)

### 4. Gardez les exemples sous la main

Ce chapitre contient de nombreux exemples réutilisables. Créez-vous une bibliothèque de code que vous pourrez réutiliser dans vos futurs projets.

### 5. Commencez simple, complexifiez ensuite

Ne cherchez pas à tout comprendre d'un coup. Maîtrisez d'abord les bases (sections 7.1 et 7.2), puis progressez vers les concepts plus avancés.

---

## Structure des sections

Chaque section de ce chapitre suit la même structure pour faciliter votre apprentissage :

1. **Introduction** : Présentation du concept et de son utilité
2. **Concepts fondamentaux** : Théorie de base expliquée simplement
3. **Exemples simples** : Code de base pour démarrer
4. **Techniques avancées** : Aller plus loin
5. **Exemples pratiques complets** : Applications réelles et utilisables
6. **Bonnes pratiques** : Recommandations et pièges à éviter
7. **Résumé** : Récapitulatif des points clés

---

## À quoi ressemblera votre code

Voici un aperçu du style de code que vous allez apprendre :

### Avant ce chapitre (débutant)
```pascal
// Code basique et peu robuste
procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile('fichier.txt');
end;
```

### Après ce chapitre (professionnel)
```pascal
// Code robuste, réutilisable et bien structuré
procedure TForm1.ChargerFichierSecurise(const NomFichier: string);
begin
  if not FileExists(NomFichier) then
  begin
    ShowMessage('Fichier introuvable : ' + NomFichier);
    Exit;
  end;

  try
    Memo1.Lines.LoadFromFile(NomFichier, TEncoding.UTF8);
    StatusBar1.SimpleText := 'Fichier chargé : ' + ExtractFileName(NomFichier);
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors du chargement : ' + E.Message);
      LogErreur('ChargerFichier', E);
    end;
  end;
end;
```

---

## Ressources complémentaires

### Documentation officielle Delphi

Pour aller plus loin, consultez la documentation officielle :
- [System.IOUtils](https://docwiki.embarcadero.com/Libraries/en/System.IOUtils)
- [System.Classes.TStream](https://docwiki.embarcadero.com/Libraries/en/System.Classes.TStream)
- [System.JSON](https://docwiki.embarcadero.com/Libraries/en/System.JSON)

### Unités de référence

Gardez ces références sous la main :
- `System.IOUtils.TFile` : Opérations sur fichiers
- `System.IOUtils.TDirectory` : Opérations sur dossiers
- `System.IOUtils.TPath` : Manipulation de chemins
- `System.Classes.TStringList` : Listes de chaînes

---

## Objectifs d'apprentissage

À la fin de ce chapitre, vous serez capable de :

✅ Lire et écrire tous types de fichiers (texte, binaire, structuré)
✅ Choisir le format approprié selon vos besoins
✅ Manipuler des streams de données efficacement
✅ Sérialiser et persister des objets complexes
✅ Compresser des données pour économiser de l'espace
✅ Automatiser le traitement de fichiers par lots
✅ Échanger des données avec d'autres applications (JSON, XML)
✅ Importer et exporter des données CSV/Excel
✅ Gérer les erreurs de manière professionnelle
✅ Créer des applications robustes et fiables

---

## Motivation

La gestion de fichiers peut sembler technique au premier abord, mais c'est une compétence qui vous ouvrira d'innombrables possibilités :

- **Créez des applications qui se souviennent** : Sauvegardez l'état entre les sessions
- **Échangez avec le monde** : Importez et exportez des données
- **Automatisez des tâches** : Traitez des centaines de fichiers en quelques secondes
- **Professionnalisez vos projets** : Ajoutez la persistance et la configuration
- **Communiquez avec d'autres systèmes** : Intégrez-vous dans des écosystèmes existants

Chaque section vous rapprochera de votre objectif : créer des applications complètes et professionnelles.

---

## Prêt à commencer ?

Vous avez maintenant une vue d'ensemble complète de ce qui vous attend. Ce chapitre est dense mais structuré pour vous accompagner progressivement.

**Commençons par les bases avec la section 7.1 : Lecture/écriture de fichiers texte !**

N'oubliez pas : **la pratique est la clé**. Lisez, testez, expérimentez, créez. C'est en manipulant réellement des fichiers que vous maîtriserez ces concepts.

Bon apprentissage ! 🚀

---

## Aide et support

Si vous rencontrez des difficultés :

1. **Relisez la section** : La réponse est souvent dans les détails
2. **Testez les exemples fournis** : Assurez-vous qu'ils fonctionnent chez vous
3. **Simplifiez le problème** : Revenez à un exemple minimal
4. **Consultez la documentation** : Les liens sont fournis dans chaque section
5. **Expérimentez** : Modifiez le code pour comprendre son comportement

La gestion de fichiers est une compétence fondamentale qui vous servira tout au long de votre carrière de développeur. Investissez du temps pour bien la maîtriser, cela en vaut la peine !

---

**Tournons maintenant la page vers la section 7.1 et découvrons ensemble la manipulation de fichiers texte...**

⏭️ [Lecture/écriture de fichiers texte](/07-gestion-des-fichiers-et-flux-de-donnees/01-lecture-ecriture-de-fichiers-texte.md)
