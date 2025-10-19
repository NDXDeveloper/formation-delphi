🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.6 Documentation du code

## Introduction

Imaginez que vous trouvez un appareil électronique complexe sans mode d'emploi. Vous appuyez sur des boutons au hasard, espérant comprendre comment ça fonctionne. Frustrant, n'est-ce pas ?

Le code sans documentation, c'est pareil. Vous (ou un collègue) ouvrez un fichier six mois après l'avoir écrit et vous vous demandez : "Mais qu'est-ce que j'ai voulu faire ici ?"

La **documentation** est l'art d'expliquer votre code : ce qu'il fait, pourquoi il le fait, et comment l'utiliser. C'est un cadeau que vous faites à votre futur vous-même et à vos collègues.

### Pourquoi documenter ?

#### 1. Pour votre futur vous

**Situation réelle :**
```pascal
// Code que vous avez écrit il y a 6 mois
function Process(X: Integer): Integer;
begin
  Result := X * 3 + 7 - (X div 2);
end;
```

**Vous aujourd'hui :** "Euh... pourquoi * 3 + 7 - (X div 2) ? C'était quoi cette formule déjà ?"

**Avec documentation :**
```pascal
/// <summary>
///   Calcule le coefficient d'ajustement selon la formule métier définie
///   dans le cahier des charges section 4.2
/// </summary>
/// <remarks>
///   Formule : (quantité × 3) + bonus_fixe - réduction_volume
///   où réduction_volume = quantité ÷ 2
/// </remarks>
function CalculerCoefficientAjustement(Quantite: Integer): Integer;
begin
  Result := Quantite * 3 + 7 - (Quantite div 2);
end;
```

**Vous aujourd'hui :** "Ah oui, c'est la formule du cahier des charges !"

#### 2. Pour vos collègues

Votre code sera lu par d'autres développeurs. La documentation les aide à :
- Comprendre rapidement ce que fait votre code
- Utiliser vos fonctions sans regarder l'implémentation
- Modifier le code en toute confiance
- Éviter de vous poser 50 questions par jour

#### 3. Pour les nouveaux arrivants

Un nouveau développeur rejoint l'équipe. Avec une bonne documentation, il est productif en quelques jours. Sans documentation, il passe des semaines à comprendre.

#### 4. Pour la maintenabilité

**Statistique importante :** Le code est lu 10 fois plus souvent qu'il n'est écrit. Investir dans la documentation fait gagner un temps considérable.

### Les mythes sur la documentation

❌ **Mythe 1 : "Le code se suffit à lui-même"**
- Réalité : Le code montre CE QUE ça fait, pas POURQUOI

❌ **Mythe 2 : "Je n'ai pas le temps"**
- Réalité : Vous perdrez 10 fois plus de temps à expliquer oralement

❌ **Mythe 3 : "Personne ne lit la documentation"**
- Réalité : Si elle n'existe pas, personne ne peut la lire !

❌ **Mythe 4 : "Mon code est simple, pas besoin"**
- Réalité : Ce qui est simple pour vous aujourd'hui ne le sera pas dans 6 mois

✅ **Réalité :** La documentation fait partie intégrante du code professionnel

## Les types de documentation

Il existe plusieurs niveaux de documentation, chacun ayant son utilité.

### 1. Commentaires inline

Les commentaires directs dans le code, pour expliquer des lignes complexes.

```pascal
procedure TFacture.Calculer;
var
  Total: Currency;
begin
  Total := SousTotal;

  // Appliquer la remise client si > 1000€
  if Total > 1000 then
    Total := Total * 0.9;

  // TVA à 20% sauf pour les livres (5.5%)
  if TypeProduit = tpLivre then
    Total := Total * 1.055
  else
    Total := Total * 1.20;

  FTotal := Total;
end;
```

### 2. Documentation XML

Documentation structurée qui peut être extraite automatiquement pour générer de la documentation HTML.

```pascal
/// <summary>
///   Calcule le montant total TTC d'une facture
/// </summary>
/// <remarks>
///   Applique la remise client si le montant dépasse 1000€
///   Applique le taux de TVA selon le type de produit
/// </remarks>
procedure TFacture.Calculer;
```

### 3. Documentation technique

Documents expliquant l'architecture, les choix techniques, les patterns utilisés.

**Exemples :**
- Architecture.md
- TechnicalDecisions.md
- DatabaseSchema.md

### 4. Documentation utilisateur

Guides pour les utilisateurs finaux de l'application.

**Exemples :**
- Manuel utilisateur
- Guides de démarrage rapide
- FAQ
- Tutoriels vidéo

### 5. README

Le fichier d'accueil du projet, expliquant ce que c'est et comment l'utiliser.

## Les commentaires en Delphi

### Syntaxes de commentaires

Delphi supporte trois types de commentaires :

```pascal
// Commentaire sur une ligne

{ Commentaire
  sur plusieurs
  lignes }

(* Autre syntaxe pour
   commentaires multi-lignes *)
```

**Convention :** Utilisez `//` pour les commentaires courts, et `{ }` pour les longs commentaires.

### Commentaires de documentation XML

Delphi supporte une syntaxe XML pour la documentation structurée :

```pascal
/// <summary>
///   Description courte de la fonction
/// </summary>
/// <param name="NomParam">Description du paramètre</param>
/// <returns>Description de ce qui est retourné</returns>
/// <remarks>
///   Remarques additionnelles
/// </remarks>
/// <exception cref="EException">Quand cette exception est levée</exception>
function MaFonction(NomParam: Integer): string;
```

**Balises XML principales :**

| Balise | Usage |
|--------|-------|
| `<summary>` | Description courte |
| `<param>` | Description d'un paramètre |
| `<returns>` | Ce qui est retourné |
| `<remarks>` | Remarques détaillées |
| `<exception>` | Exceptions possibles |
| `<example>` | Exemple d'utilisation |
| `<see>` | Référence à un autre élément |

### Exemples de bonne documentation

#### Fonction simple

```pascal
/// <summary>
///   Calcule la TVA sur un montant HT
/// </summary>
/// <param name="MontantHT">Montant hors taxes en euros</param>
/// <param name="TauxTVA">Taux de TVA en pourcentage (ex: 20 pour 20%)</param>
/// <returns>Montant de la TVA en euros</returns>
function CalculerTVA(MontantHT: Currency; TauxTVA: Double): Currency;
begin
  Result := MontantHT * (TauxTVA / 100);
end;
```

#### Procédure avec exceptions

```pascal
/// <summary>
///   Sauvegarde les données client dans la base de données
/// </summary>
/// <param name="Client">Objet client à sauvegarder</param>
/// <exception cref="EClientInvalid">
///   Si les données du client ne sont pas valides
/// </exception>
/// <exception cref="EDatabaseError">
///   Si la connexion à la base échoue
/// </exception>
/// <remarks>
///   Cette méthode valide automatiquement les données avant sauvegarde
///   Si le client existe déjà (même ID), il sera mis à jour
/// </remarks>
procedure TClientManager.Sauvegarder(Client: TClient);
begin
  if not Client.EstValide then
    raise EClientInvalid.Create('Données client invalides');

  try
    if ClientExiste(Client.ID) then
      MettreAJour(Client)
    else
      Inserer(Client);
  except
    on E: Exception do
      raise EDatabaseError.Create('Erreur de sauvegarde : ' + E.Message);
  end;
end;
```

#### Classe complète

```pascal
/// <summary>
///   Gestionnaire de calculs de prix avec remises et taxes
/// </summary>
/// <remarks>
///   Cette classe centralise tous les calculs de prix de l'application.
///   Elle applique les règles métier suivantes :
///   - Remise par paliers selon le montant
///   - Remise fidélité pour les clients premium
///   - TVA variable selon le type de produit
/// </remarks>
type
  TCalculateurPrix = class
  private
    FTauxTVAStandard: Double;
    FTauxTVAReduit: Double;

    /// <summary>
    ///   Détermine le taux de remise selon le montant
    /// </summary>
    function ObtenirTauxRemise(Montant: Currency): Double;
  public
    /// <summary>
    ///   Crée une nouvelle instance du calculateur
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Calcule le prix TTC final avec toutes les remises et taxes
    /// </summary>
    /// <param name="MontantHT">Prix de base hors taxes</param>
    /// <param name="EstClientPremium">True si le client a un compte premium</param>
    /// <param name="TypeProduit">Type de produit (influence le taux de TVA)</param>
    /// <returns>Prix final TTC incluant remises et taxes</returns>
    function CalculerPrixFinal(MontantHT: Currency;
      EstClientPremium: Boolean;
      TypeProduit: TTypeProduit): Currency;

    /// <summary>
    ///   Taux de TVA standard (20% par défaut)
    /// </summary>
    property TauxTVAStandard: Double read FTauxTVAStandard write FTauxTVAStandard;

    /// <summary>
    ///   Taux de TVA réduit (5.5% par défaut)
    /// </summary>
    property TauxTVAReduit: Double read FTauxTVAReduit write FTauxTVAReduit;
  end;
```

## Que documenter et que ne pas documenter

### ✅ À DOCUMENTER

#### 1. Le "POURQUOI"

Expliquez toujours POURQUOI vous faites quelque chose, surtout si ce n'est pas évident.

```pascal
// ✅ BON : Explique pourquoi
// On attend 100ms avant de réessayer pour éviter de surcharger le serveur
Sleep(100);

// ❌ MAUVAIS : Répète juste le code
// Attendre 100 millisecondes
Sleep(100);
```

#### 2. Les algorithmes complexes

```pascal
/// <summary>
///   Implémente l'algorithme de Luhn pour valider un numéro de carte bancaire
/// </summary>
/// <remarks>
///   L'algorithme de Luhn (aussi appelé "modulo 10") est utilisé pour
///   détecter les erreurs de saisie dans les numéros de carte.
///   Voir : https://fr.wikipedia.org/wiki/Formule_de_Luhn
/// </remarks>
function ValiderNumeroCarte(const Numero: string): Boolean;
var
  Somme, Chiffre, I: Integer;
begin
  Somme := 0;

  // Parcourir de droite à gauche
  for I := Length(Numero) downto 1 do
  begin
    Chiffre := StrToInt(Numero[I]);

    // Doubler un chiffre sur deux
    if (Length(Numero) - I) mod 2 = 1 then
    begin
      Chiffre := Chiffre * 2;
      // Si > 9, soustraire 9
      if Chiffre > 9 then
        Chiffre := Chiffre - 9;
    end;

    Somme := Somme + Chiffre;
  end;

  // Valide si la somme est divisible par 10
  Result := (Somme mod 10) = 0;
end;
```

#### 3. Les workarounds et solutions temporaires

```pascal
// WORKAROUND: Bug dans la version 10.4 de FireDAC avec MySQL 8.0
// Forcer le charset à utf8mb4 explicitement
// TODO: Retirer quand le bug sera corrigé dans FireDAC
FDConnection.Params.Add('CharacterSet=utf8mb4');

// HACK: Contourner un bug de Windows 11 avec les DPI
// Voir ticket #1234
if TOSVersion.Major >= 11 then
  ScaleFactor := ScaleFactor * 1.1;
```

#### 4. Les paramètres non évidents

```pascal
/// <param name="Timeout">
///   Délai d'attente en millisecondes (0 = attente infinie,
///   -1 = utiliser le timeout par défaut de 30 secondes)
/// </param>
procedure EnvoyerRequete(const URL: string; Timeout: Integer);
```

#### 5. Les effets de bord

```pascal
/// <summary>
///   Charge les données client depuis la base
/// </summary>
/// <remarks>
///   ATTENTION: Cette méthode modifie aussi les données en cache.
///   Appeler RefreshCache() après si vous voulez synchroniser.
/// </remarks>
procedure ChargerClient(ID: Integer);
```

#### 6. Les interfaces publiques

Toute classe, fonction, procédure qui sera utilisée par d'autres développeurs doit être documentée.

```pascal
type
  /// <summary>
  ///   Interface pour les gestionnaires de données clients
  /// </summary>
  IClientManager = interface
    ['{GUID-ICI}']

    /// <summary>
    ///   Récupère un client par son identifiant
    /// </summary>
    /// <param name="ID">Identifiant unique du client</param>
    /// <returns>
    ///   Objet TClient si trouvé, nil sinon
    /// </returns>
    function GetClient(ID: Integer): TClient;

    /// <summary>
    ///   Sauvegarde ou met à jour un client
    /// </summary>
    /// <param name="Client">Client à sauvegarder</param>
    /// <returns>True si succès, False sinon</returns>
    function SaveClient(Client: TClient): Boolean;
  end;
```

### ❌ À NE PAS DOCUMENTER

#### 1. L'évidence

```pascal
// ❌ MAUVAIS : C'est évident
// Incrémenter I
Inc(I);

// ❌ MAUVAIS : Le nom de la fonction le dit déjà
/// <summary>
///   Calcule le total
/// </summary>
function CalculerTotal: Currency;

// ✅ BON : Pas de commentaire inutile
function CalculerTotal: Currency;
```

#### 2. La paraphrase du code

```pascal
// ❌ MAUVAIS : Répète juste le code
// Si X est supérieur à 10
if X > 10 then
  // Mettre Y à 5
  Y := 5;

// ✅ BON : Explique le business
// Appliquer la remise standard pour les commandes importantes
if Montant > 1000 then
  Remise := 0.05;
```

#### 3. Les détails d'implémentation évidents

```pascal
// ❌ MAUVAIS
/// <summary>
///   Cette fonction prend un string en paramètre et retourne un Integer
/// </summary>
function ConvertirEnEntier(const Texte: string): Integer;

// ✅ BON : Focus sur l'objectif, pas la mécanique
/// <summary>
///   Convertit une chaîne en entier avec gestion des erreurs
/// </summary>
/// <returns>0 si la conversion échoue</returns>
function ConvertirEnEntier(const Texte: string): Integer;
```

## Styles de documentation

### Style descriptif

Décrit ce que fait le code de manière neutre.

```pascal
/// <summary>
///   Valide les données du formulaire client
/// </summary>
/// <returns>
///   True si toutes les données sont valides, False sinon
/// </returns>
function ValiderFormulaire: Boolean;
```

### Style impératif

Utilise des verbes à l'impératif, comme si vous donniez un ordre.

```pascal
/// <summary>
///   Valide les données du formulaire client
/// </summary>
/// <returns>
///   Retourne True si valide, False sinon
/// </returns>
function ValiderFormulaire: Boolean;
```

**Recommandation :** Choisissez un style et soyez cohérent dans tout le projet. Le style descriptif est plus courant en français.

### Exemples d'utilisation

Incluez des exemples pour les fonctions complexes :

```pascal
/// <summary>
///   Formate un numéro de téléphone français
/// </summary>
/// <param name="Numero">Numéro brut (10 chiffres)</param>
/// <returns>Numéro formaté avec espaces</returns>
/// <example>
///   <code>
///     Resultat := FormaterTelephone('0123456789');
///     // Resultat = '01 23 45 67 89'
///   </code>
/// </example>
function FormaterTelephone(const Numero: string): string;
begin
  if Length(Numero) <> 10 then
    Exit(Numero);

  Result := Format('%s %s %s %s %s', [
    Copy(Numero, 1, 2),
    Copy(Numero, 3, 2),
    Copy(Numero, 5, 2),
    Copy(Numero, 7, 2),
    Copy(Numero, 9, 2)
  ]);
end;
```

## Organisation de la documentation dans les fichiers

### Structure d'un fichier bien documenté

```pascal
unit GestionClients;

{$IFDEF DOCUMENTATION}
///
/// <summary>
///   Unité de gestion des clients de l'application
/// </summary>
/// <remarks>
///   Cette unité fournit toutes les fonctionnalités nécessaires pour :
///   - Créer, modifier et supprimer des clients
///   - Valider les données client
///   - Calculer les remises selon les règles métier
///
///   Classes principales :
///   - TClient : Représente un client
///   - TClientManager : Gestionnaire de clients
///   - TClientValidator : Validateur de données
///
///   Auteur: Jean Dupont
///   Date: 2025-01-15
///   Version: 1.2
/// </remarks>
///
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  /// <summary>
  ///   Représente un client de l'application
  /// </summary>
  /// <remarks>
  ///   Cette classe contient toutes les informations d'un client
  ///   ainsi que les méthodes de validation
  /// </remarks>
  TClient = class
  private
    FID: Integer;
    FNom: string;
    FEmail: string;
    FDateCreation: TDateTime;
    FEstPremium: Boolean;

    /// <summary>
    ///   Définit le nom du client
    /// </summary>
    /// <remarks>
    ///   Nettoie automatiquement les espaces en début et fin
    /// </remarks>
    procedure SetNom(const Value: string);
  public
    /// <summary>
    ///   Crée une nouvelle instance de TClient
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Valide que les données du client sont correctes
    /// </summary>
    /// <param name="MessageErreur">
    ///   Reçoit le message d'erreur si validation échoue
    /// </param>
    /// <returns>True si valide, False sinon</returns>
    function Valider(out MessageErreur: string): Boolean;

    // Propriétés

    /// <summary>
    ///   Identifiant unique du client
    /// </summary>
    property ID: Integer read FID write FID;

    /// <summary>
    ///   Nom complet du client (minimum 2 caractères)
    /// </summary>
    property Nom: string read FNom write SetNom;

    /// <summary>
    ///   Adresse email (doit être valide)
    /// </summary>
    property Email: string read FEmail write FEmail;

    /// <summary>
    ///   Date de création du compte client
    /// </summary>
    property DateCreation: TDateTime read FDateCreation write FDateCreation;

    /// <summary>
    ///   Indique si le client a un compte premium
    /// </summary>
    property EstPremium: Boolean read FEstPremium write FEstPremium;
  end;

implementation

{ TClient }

constructor TClient.Create;
begin
  inherited;
  FID := 0;
  FNom := '';
  FEmail := '';
  FDateCreation := Now;
  FEstPremium := False;
end;

procedure TClient.SetNom(const Value: string);
begin
  FNom := Trim(Value);
end;

function TClient.Valider(out MessageErreur: string): Boolean;
begin
  Result := False;
  MessageErreur := '';

  // Validation du nom
  if Length(FNom) < 2 then
  begin
    MessageErreur := 'Le nom doit contenir au moins 2 caractères';
    Exit;
  end;

  // Validation de l'email
  if not FEmail.Contains('@') then
  begin
    MessageErreur := 'L''adresse email n''est pas valide';
    Exit;
  end;

  Result := True;
end;

end.
```

### En-tête de fichier

Ajoutez un en-tête informatif au début de chaque unité :

```pascal
unit MonUnite;

{*******************************************************************************
  Nom: MonUnite
  Description: Gestion des opérations sur les commandes
  Auteur: Jean Dupont <jean.dupont@company.com>
  Date de création: 2025-01-15
  Dernière modification: 2025-02-20
  Version: 2.1

  Historique des modifications:
    v2.1 (2025-02-20): Ajout de la gestion des remises saisonnières
    v2.0 (2025-02-01): Refactoring complet de l'architecture
    v1.5 (2025-01-20): Correction bug calcul TVA
    v1.0 (2025-01-15): Version initiale

  Dépendances:
    - FireDAC pour l'accès base de données
    - System.JSON pour l'export JSON

  TODO:
    - Ajouter support des codes promo
    - Optimiser la requête GetCommandesByDate
    - Ajouter tests unitaires
*******************************************************************************}

interface
```

## Documentation au niveau du projet

### Le fichier README.md

Le README est la porte d'entrée de votre projet. Il doit contenir :

```markdown
# Mon Application de Gestion

[![Version](https://img.shields.io/badge/version-2.1.0-blue.svg)]()
[![Delphi](https://img.shields.io/badge/Delphi-13%20Florence-red.svg)]()
[![Licence](https://img.shields.io/badge/licence-MIT-green.svg)]()

Application de gestion commerciale développée en Delphi pour Windows.

## 📋 Table des matières

- [Fonctionnalités](#fonctionnalités)
- [Prérequis](#prérequis)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
- [Architecture](#architecture)
- [Contribution](#contribution)
- [Licence](#licence)

## ✨ Fonctionnalités

- Gestion des clients et fournisseurs
- Création et suivi des commandes
- Génération de factures PDF
- Statistiques et tableaux de bord
- Synchronisation cloud
- Export Excel des données

## 🔧 Prérequis

- Windows 10 ou supérieur
- Delphi 13 Florence (ou version supérieure)
- MySQL 8.0+
- 4 GB RAM minimum
- 500 MB d'espace disque

## 📥 Installation

### Installation pour développeurs

1. Clonez le repository :
```bash
git clone https://github.com/societe/mon-application.git
cd mon-application
```

2. Restaurez les dépendances :
   - Ouvrez `GetIt Package Manager` dans Delphi
   - Installez les packages listés dans `packages.txt`

3. Configurez la base de données :
```bash
mysql -u root -p < database/schema.sql
```

4. Copiez le fichier de configuration :
```bash
copy config.template.ini config.local.ini
```

5. Éditez `config.local.ini` avec vos paramètres :
```ini
[Database]
Server=localhost
Database=gestion_commerciale
Username=root
Password=votre_mot_de_passe
```

6. Ouvrez `GestionCommerciale.dpr` dans Delphi

7. Compilez et exécutez (F9)

### Installation pour utilisateurs finaux

Téléchargez l'installateur depuis la [page Releases](https://github.com/societe/mon-application/releases) et suivez les instructions.

## ⚙️ Configuration

### Base de données

Le fichier `config.ini` contient les paramètres de connexion :

```ini
[Database]
Server=localhost
Port=3306
Database=gestion_commerciale
Username=root
Password=
```

### Personnalisation

L'interface peut être personnalisée via le menu **Paramètres > Préférences**.

## 🚀 Usage

### Démarrage rapide

1. Lancez l'application
2. Connectez-vous avec vos identifiants
3. Créez votre premier client via **Clients > Nouveau**
4. Créez une commande via **Commandes > Nouvelle**

### Exemples

**Créer un client par code :**

```pascal
var
  Client: TClient;
  Manager: TClientManager;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Dupont SA';
    Client.Email := 'contact@dupont.fr';

    Manager := TClientManager.Create;
    try
      if Manager.Sauvegarder(Client) then
        ShowMessage('Client créé avec succès');
    finally
      Manager.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

## 🏗️ Architecture

Le projet suit une architecture en couches :

```
GestionCommerciale/
├── Source/
│   ├── UI/              # Interface utilisateur (VCL)
│   ├── Business/        # Logique métier
│   ├── DataAccess/      # Accès aux données
│   └── Models/          # Modèles de données
├── Database/            # Scripts SQL
├── Tests/               # Tests unitaires
└── Docs/                # Documentation
```

Pour plus de détails, voir [ARCHITECTURE.md](docs/ARCHITECTURE.md).

## 🤝 Contribution

Les contributions sont les bienvenues ! Voir [CONTRIBUTING.md](CONTRIBUTING.md) pour les guidelines.

### Processus

1. Forkez le projet
2. Créez une branche (`git checkout -b feature/AmazingFeature`)
3. Commitez vos changements (`git commit -m 'feat: Add AmazingFeature'`)
4. Pushez vers la branche (`git push origin feature/AmazingFeature`)
5. Ouvrez une Pull Request

## 📝 Licence

Ce projet est sous licence MIT. Voir [LICENSE](LICENSE) pour plus d'informations.

## 👥 Auteurs

- **Jean Dupont** - *Développeur principal* - [@jeandupont](https://github.com/jeandupont)
- **Marie Martin** - *Développeuse* - [@mariemartin](https://github.com/mariemartin)

Voir aussi la liste des [contributeurs](https://github.com/societe/mon-application/contributors).

## 🙏 Remerciements

- L'équipe Embarcadero pour Delphi
- La communauté Delphi francophone
- Tous les contributeurs

## 📞 Support

- Documentation : https://docs.example.com
- Issues : https://github.com/societe/mon-application/issues
- Email : support@example.com

## 📈 Roadmap

- [x] Version 1.0 - Fonctionnalités de base
- [x] Version 2.0 - Interface modernisée
- [ ] Version 2.5 - Support multi-devises
- [ ] Version 3.0 - Application mobile (FMX)
```

### CHANGELOG.md

Documentez l'évolution du projet :

```markdown
# Changelog

Toutes les modifications notables de ce projet sont documentées dans ce fichier.

Le format est basé sur [Keep a Changelog](https://keepachangelog.com/fr/1.0.0/),
et ce projet adhère au [Semantic Versioning](https://semver.org/lang/fr/).

## [Non publié]

### Ajouté
- Support des codes promotionnels

### En cours
- Optimisation des performances des requêtes

## [2.1.0] - 2025-02-20

### Ajouté
- Gestion des remises saisonnières
- Export des statistiques en PDF
- Nouveau thème sombre

### Modifié
- Amélioration de la performance de la grille (+40%)
- Interface utilisateur modernisée

### Corrigé
- Bug d'affichage des dates en format américain
- Crash lors de l'export Excel avec plus de 10000 lignes
- Problème de mémoire avec les grandes images

### Sécurité
- Correction de la vulnérabilité XSS dans les commentaires

## [2.0.0] - 2025-02-01

### Ajouté
- Architecture complètement refactorisée (MVC)
- Tests unitaires (couverture 80%)
- Documentation API complète

### Modifié
- BREAKING: API complètement redessinée
- Migration vers FireDAC (abandonné dbExpress)

### Supprimé
- Support de Windows 7

## [1.5.0] - 2025-01-20

### Corrigé
- Correction du calcul de TVA pour les livres

## [1.0.0] - 2025-01-15

### Ajouté
- Version initiale
- Gestion des clients
- Gestion des commandes
- Génération de factures

[Non publié]: https://github.com/user/repo/compare/v2.1.0...HEAD
[2.1.0]: https://github.com/user/repo/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/user/repo/compare/v1.5.0...v2.0.0
[1.5.0]: https://github.com/user/repo/compare/v1.0.0...v1.5.0
[1.0.0]: https://github.com/user/repo/releases/tag/v1.0.0
```

### CONTRIBUTING.md

Guide pour les contributeurs :

```markdown
# Guide de contribution

Merci de votre intérêt pour contribuer à ce projet !

## Code de conduite

Ce projet adhère au [Code de conduite Contributor Covenant](CODE_OF_CONDUCT.md).
En participant, vous vous engagez à respecter ce code.

## Comment contribuer

### Signaler un bug

Avant de créer un rapport de bug :
- Vérifiez que le bug n'a pas déjà été signalé
- Vérifiez que vous utilisez la dernière version
- Collectez les informations nécessaires

Créez une issue avec ces informations :
- Description claire du problème
- Étapes pour reproduire
- Comportement attendu vs comportement actuel
- Captures d'écran si pertinent
- Version de Delphi et Windows

### Proposer une fonctionnalité

1. Créez une issue décrivant la fonctionnalité
2. Expliquez pourquoi elle est utile
3. Attendez les retours avant de commencer le code

### Soumettre une Pull Request

1. Forkez le repository
2. Créez une branche depuis `main`
3. Écrivez votre code
4. Ajoutez des tests
5. Mettez à jour la documentation
6. Assurez-vous que tout compile sans warning
7. Commitez avec des messages clairs
8. Pushez vers votre fork
9. Ouvrez une Pull Request

## Standards de code

### Style de code

- Indentation : 2 espaces
- Noms de classes : TPascalCase
- Noms de méthodes : PascalCase
- Noms de variables : camelCase
- Constantes : UPPER_SNAKE_CASE

### Documentation

- Toutes les classes publiques doivent être documentées
- Toutes les méthodes publiques doivent avoir un XML doc
- Les algorithmes complexes doivent être commentés

### Tests

- Ajoutez des tests pour toute nouvelle fonctionnalité
- Assurez-vous que tous les tests passent
- Visez une couverture de code de 80%+

## Processus de revue

1. Soumission de la PR
2. Revue automatique (CI/CD)
3. Revue par un mainteneur
4. Corrections si nécessaires
5. Approbation et merge

## Questions ?

N'hésitez pas à :
- Ouvrir une issue pour toute question
- Rejoindre notre Discord : https://discord.gg/...
- Envoyer un email : dev@example.com
```

## Outils de génération de documentation

### PasDoc

**PasDoc** est un outil open source qui génère de la documentation HTML à partir des commentaires de votre code Delphi.

**Installation :**
1. Téléchargez depuis https://github.com/pasdoc/pasdoc
2. Extrayez dans un dossier
3. Ajoutez au PATH

**Utilisation :**

```bash
# Générer la documentation
pasdoc --source=Source --output=Docs\html --format=html --title="Mon Application"

# Avec options avancées
pasdoc ^
  --source=Source\*.pas ^
  --output=Docs\html ^
  --format=html ^
  --title="Gestion Commerciale" ^
  --introduction=Docs\intro.txt ^
  --footer=Docs\footer.html ^
  --verbosity=3
```

**Résultat :** Documentation HTML complète navigable.

### Documentation intégrée Delphi

Delphi peut extraire la documentation XML :

1. Project → Options → Delphi Compiler
2. Cochez "Generate XML documentation"
3. Compilez le projet

Un fichier `.xml` est généré avec toute la documentation.

### Doc-O-Matic

Outil commercial puissant pour générer de la documentation professionnelle.

**Fonctionnalités :**
- Support Delphi natif
- Multiples formats de sortie (HTML, PDF, CHM)
- Templates personnalisables
- Intégration CI/CD

### Autres outils

- **Doxygen** : Supporte Delphi via un filtre
- **NaturalDocs** : Documentation naturelle
- **Sphinx** : Avec extension pour Delphi

## Bonnes pratiques

### 1. Documentez au fur et à mesure

❌ **Mauvaise approche :**
```
Écrire 1000 lignes de code
→ "Je documenterai plus tard"
→ Plus tard n'arrive jamais
```

✅ **Bonne approche :**
```
Écrire une fonction
→ La documenter immédiatement
→ Passer à la suivante
```

### 2. Maintenez la documentation à jour

La documentation obsolète est pire que pas de documentation.

```pascal
// ❌ Documentation obsolète (dangereuse !)
/// <returns>Retourne toujours True</returns>
function Valider: Boolean;
begin
  // Le code a changé mais pas la doc !
  Result := False;
end;

// ✅ Documentation mise à jour
/// <returns>
///   True si les données sont valides, False sinon
/// </returns>
function Valider: Boolean;
begin
  Result := FNom <> '';
end;
```

**Règle d'or :** Quand vous modifiez du code, mettez à jour la documentation en même temps.

### 3. Utilisez des TODO et FIXME

Marquez les problèmes et améliorations futures :

```pascal
// TODO: Ajouter support des formats de date internationaux
// FIXME: Bug avec les dates avant 1900
// HACK: Contournement temporaire du bug #123
// NOTE: Cette fonction est appelée très souvent, optimiser si possible
// WARNING: Ne pas appeler dans un thread

function FormatDate(Date: TDateTime): string;
begin
  // Implémentation...
end;
```

**Dans Delphi :** Ces commentaires apparaissent dans la fenêtre "To-Do List".

### 4. Documentation progressive

Commencez simple et enrichissez progressivement :

**Niveau 1 - Minimum :**
```pascal
/// <summary>Valide les données</summary>
function Valider: Boolean;
```

**Niveau 2 - Standard :**
```pascal
/// <summary>
///   Valide que les données du formulaire sont correctes
/// </summary>
/// <returns>True si valide, False sinon</returns>
function Valider: Boolean;
```

**Niveau 3 - Complet :**
```pascal
/// <summary>
///   Valide que les données du formulaire sont correctes selon les règles métier
/// </summary>
/// <returns>True si toutes les validations passent, False sinon</returns>
/// <remarks>
///   Vérifie :
///   - Nom : minimum 2 caractères
///   - Email : format valide
///   - Téléphone : 10 chiffres
///   - Date de naissance : entre 1900 et aujourd'hui
/// </remarks>
/// <example>
///   <code>
///     if not Valider then
///     begin
///       ShowMessage('Formulaire invalide');
///       Exit;
///     end;
///   </code>
/// </example>
function Valider: Boolean;
```

### 5. Revue de documentation

Lors des revues de code, vérifiez aussi la documentation :

✅ **Checklist de revue :**
- [ ] Toutes les fonctions publiques sont documentées
- [ ] Les paramètres sont expliqués
- [ ] Les valeurs de retour sont décrites
- [ ] Les exceptions possibles sont listées
- [ ] Les effets de bord sont mentionnés
- [ ] Les exemples sont corrects
- [ ] Pas de documentation obsolète

### 6. Templates de documentation

Créez des templates pour standardiser :

```pascal
// Template pour une fonction
/// <summary>
///   [Description courte de ce que fait la fonction]
/// </summary>
/// <param name="Param1">[Description du paramètre]</param>
/// <returns>[Ce qui est retourné]</returns>
/// <remarks>
///   [Détails supplémentaires si nécessaires]
/// </remarks>
function MaFonction(Param1: Integer): string;

// Template pour une classe
/// <summary>
///   [Description courte de la classe]
/// </summary>
/// <remarks>
///   [Responsabilités de la classe]
///
///   Utilisation typique :
///   [Exemple de code]
/// </remarks>
type
  TMaClasse = class
```

### 7. Documentation vs nommage

Un bon nommage réduit le besoin de documentation :

```pascal
// ❌ Mauvais nommage nécessite documentation
/// <summary>
///   Calcule le total TTC en ajoutant la TVA au montant HT
/// </summary>
function Calc(X: Currency): Currency;

// ✅ Bon nommage rend la documentation optionnelle
function CalculerTotalTTC(MontantHT: Currency): Currency;
```

**Principe :** Code explicite > Documentation > Code obscur

### 8. Évitez la sur-documentation

```pascal
// ❌ Trop de documentation tue la documentation
/// <summary>
///   Cette fonction prend un paramètre de type Integer nommé X
///   et retourne un Integer qui est le résultat de X multiplié par 2
/// </summary>
/// <param name="X">
///   Un nombre entier qui sera multiplié par 2
/// </param>
/// <returns>
///   Retourne X * 2, c'est à dire X multiplié par 2
/// </returns>
/// <remarks>
///   Cette fonction est utilisée pour doubler un nombre
/// </remarks>
/// <example>
///   Si X = 5, le résultat sera 10
/// </example>
function Doubler(X: Integer): Integer;
begin
  Result := X * 2;
end;

// ✅ Documentation concise et utile
/// <summary>Retourne le double de la valeur</summary>
function Doubler(X: Integer): Integer;
begin
  Result := X * 2;
end;
```

## Documentation pour les différents publics

### Pour les développeurs (documentation technique)

Focus sur l'implémentation, les détails techniques, les algorithmes.

```pascal
/// <summary>
///   Implémente un cache LRU (Least Recently Used) thread-safe
/// </summary>
/// <remarks>
///   Utilise un TDictionary pour O(1) lookup et une liste doublement
///   chaînée pour O(1) éviction. Thread-safety via TCriticalSection.
///
///   Complexité :
///   - Get : O(1)
///   - Put : O(1)
///   - Espace : O(n) où n = capacité
/// </remarks>
type
  TLRUCache<TKey, TValue> = class
```

### Pour les utilisateurs de l'API (documentation fonctionnelle)

Focus sur ce que fait la fonction, pas comment.

```pascal
/// <summary>
///   Envoie un email avec des pièces jointes
/// </summary>
/// <param name="Destinataire">Adresse email du destinataire</param>
/// <param name="Sujet">Sujet de l'email</param>
/// <param name="Message">Corps du message</param>
/// <param name="PiecesJointes">
///   Liste des chemins complets des fichiers à joindre (optionnel)
/// </param>
/// <returns>True si l'envoi a réussi</returns>
/// <exception cref="EEmailInvalid">
///   Si l'adresse email n'est pas valide
/// </exception>
/// <exception cref="EFichierNonTrouve">
///   Si une pièce jointe n'existe pas
/// </exception>
function EnvoyerEmail(const Destinataire, Sujet, Message: string;
  PiecesJointes: TStringList = nil): Boolean;
```

### Pour les utilisateurs finaux (manuel utilisateur)

Focus sur comment utiliser l'application, avec captures d'écran.

```markdown
## Envoyer un email

Pour envoyer un email depuis l'application :

1. Cliquez sur le bouton **Nouveau message** dans la barre d'outils
2. Remplissez les champs :
   - **À** : Adresse email du destinataire
   - **Sujet** : Objet du message
   - **Message** : Votre texte
3. (Optionnel) Cliquez sur **Joindre** pour ajouter des fichiers
4. Cliquez sur **Envoyer**

![Capture d'écran de la fenêtre d'envoi](images/envoi-email.png)

**Note :** Les pièces jointes ne peuvent pas dépasser 25 MB au total.
```

## Checklist de documentation

Avant de considérer votre code comme "terminé", vérifiez :

### Au niveau du code

- [ ] Toutes les classes publiques ont une documentation `<summary>`
- [ ] Toutes les méthodes publiques sont documentées
- [ ] Les paramètres non évidents sont expliqués
- [ ] Les valeurs de retour sont décrites
- [ ] Les exceptions possibles sont listées
- [ ] Les algorithmes complexes ont des commentaires explicatifs
- [ ] Les workarounds et hacks sont marqués comme tels
- [ ] Les TODO sont documentés

### Au niveau du projet

- [ ] Le README.md existe et est complet
- [ ] Le CHANGELOG.md est à jour
- [ ] Le fichier LICENSE existe
- [ ] Un CONTRIBUTING.md existe si c'est open source
- [ ] Les exemples de code fonctionnent
- [ ] La documentation de l'architecture existe
- [ ] Les diagrammes sont à jour

### Au niveau utilisateur

- [ ] Un manuel utilisateur existe
- [ ] Des tutoriels de démarrage rapide existent
- [ ] Une FAQ couvre les questions courantes
- [ ] Des captures d'écran illustrent les fonctionnalités
- [ ] Les messages d'erreur sont explicites
- [ ] Une aide contextuelle est disponible

## Erreurs courantes

### 1. Documentation copier-coller

```pascal
// ❌ Documentation copiée sans modification
/// <summary>Traite les données</summary>
procedure TraiterClients;

/// <summary>Traite les données</summary>
procedure TraiterCommandes;

/// <summary>Traite les données</summary>
procedure TraiterFactures;
```

### 2. Documentation contradictoire

```pascal
// ❌ Le code et la doc ne correspondent pas
/// <returns>Retourne le nombre de clients</returns>
function GetClients: TObjectList<TClient>;  // Retourne une liste !
```

### 3. Documentation dans le mauvais format

```pascal
// ❌ Commentaire simple au lieu de XML doc
// Cette fonction calcule le total
function CalculerTotal: Currency;

// ✅ XML doc correct
/// <summary>Calcule le montant total TTC</summary>
function CalculerTotal: Currency;
```

### 4. Trop de détails inutiles

```pascal
// ❌ Information évidente et verbeuse
/// <summary>
///   Cette méthode est appelée lorsque l'utilisateur clique sur le bouton
///   et elle va afficher un message à l'écran en utilisant la fonction
///   ShowMessage de Delphi qui ouvre une boîte de dialogue modale
/// </summary>
procedure ButtonClick(Sender: TObject);
begin
  ShowMessage('Bonjour');
end;

// ✅ Concis et utile
/// <summary>Affiche le message de bienvenue</summary>
procedure ButtonClick(Sender: TObject);
begin
  ShowMessage('Bonjour');
end;
```

### 5. Documentation obsolète non maintenue

```pascal
// ❌ La doc parle de CustomerID mais le paramètre s'appelle ClientID
/// <param name="CustomerID">Identifiant du client</param>
procedure LoadClient(ClientID: Integer);
```

## Conclusion

La documentation est un investissement qui rapporte rapidement. Elle permet de :

**Pour vous-même :**
- Comprendre votre propre code des mois plus tard
- Éviter de refaire les mêmes erreurs
- Progresser plus vite sur de nouvelles fonctionnalités

**Pour votre équipe :**
- Faciliter la collaboration
- Réduire les questions répétitives
- Accélérer l'intégration des nouveaux membres

**Pour les utilisateurs :**
- Utiliser efficacement votre application/bibliothèque
- Réduire le besoin de support
- Augmenter la satisfaction

**Points clés à retenir :**

1. **Documentez au fur et à mesure** - Pas "plus tard"
2. **Focus sur le POURQUOI** - Le code montre le comment
3. **Gardez-la à jour** - Documentation obsolète = danger
4. **Soyez concis** - Trop de doc tue la doc
5. **Plusieurs niveaux** - Code, projet, utilisateurs
6. **Utilisez des outils** - PasDoc, générateurs automatiques
7. **Standards cohérents** - Même style dans tout le projet
8. **Relisez** - La doc fait partie du code à revoir

**Citation importante :**
> "Le code raconte comment. Les commentaires racontent pourquoi."
> — Jeff Atwood

Commencez dès aujourd'hui à documenter votre code. Votre futur vous-même vous remerciera !

Dans la prochaine section, nous explorerons la revue de code et le refactoring, deux pratiques essentielles pour maintenir la qualité du code dans la durée.

⏭️ [Revue de code et refactoring](/18-architecture-et-bonnes-pratiques/07-revue-de-code-et-refactoring.md)
