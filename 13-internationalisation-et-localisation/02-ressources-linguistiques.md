🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.2 Ressources linguistiques

## Introduction

Les ressources linguistiques permettent de gérer les textes et éléments textuels d'une application de manière centralisée. Cela facilite grandement la traduction de votre application dans différentes langues sans avoir à modifier le code source. Dans cette section, nous allons découvrir comment Delphi gère ces ressources et comment les exploiter efficacement.

## Pourquoi utiliser des ressources linguistiques ?

Imaginez que vous ayez développé une application avec tous les textes écrits directement dans le code :

```pascal
Button1.Caption := 'Valider';  
ShowMessage('Fichier sauvegardé avec succès');  
Label1.Caption := 'Nom de l''utilisateur :';  
```

Si vous souhaitez traduire votre application en anglais, espagnol ou toute autre langue, vous devriez modifier chaque ligne de code contenant du texte. C'est fastidieux, source d'erreurs, et difficile à maintenir.

**Avec les ressources linguistiques**, vous centralisez tous les textes, et il devient très simple de basculer d'une langue à l'autre.

## Les différents types de ressources en Delphi

| Type de ressource | Description | Format | Usage |
|-------------------|-------------|--------|-------|
| **ResourceString** | Constantes de chaînes dans le code | `.pas` | Messages, textes constants |
| **Fichiers DFM** | Propriétés visuelles des formulaires | `.dfm` | Captions, libellés des composants |
| **Fichiers de ressources** | Ressources compilées | `.rc`, `.res` | Textes, images, sons |
| **Fichiers externes** | Fichiers de traduction | `.ini`, `.xml`, `.json` | Dictionnaires de traduction |

## ResourceString : Les chaînes de ressources

`ResourceString` est la méthode la plus simple et la plus élégante pour gérer les chaînes de caractères localisables directement dans votre code.

### Déclaration des ResourceString

Au lieu de déclarer des constantes normales, vous utilisez la section `resourcestring` :

```pascal
unit MesMessages;

interface

resourcestring
  // Messages de l'application
  MSG_BIENVENUE = 'Bienvenue dans l''application';
  MSG_CONFIRMATION = 'Êtes-vous sûr de vouloir continuer ?';
  MSG_ENREGISTREMENT_OK = 'Données enregistrées avec succès';
  MSG_ERREUR_CHARGEMENT = 'Erreur lors du chargement du fichier';

  // Libellés d'interface
  LBL_NOM = 'Nom :';
  LBL_PRENOM = 'Prénom :';
  LBL_EMAIL = 'Adresse e-mail :';

  // Boutons
  BTN_VALIDER = 'Valider';
  BTN_ANNULER = 'Annuler';
  BTN_FERMER = 'Fermer';

implementation

end.
```

### Utilisation des ResourceString

Une fois déclarées, vous utilisez ces ressources comme des constantes normales :

```pascal
uses
  MesMessages;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ShowMessage(MSG_BIENVENUE);
  Button1.Caption := BTN_VALIDER;
  Label1.Caption := LBL_NOM;
end;
```

### Avantages des ResourceString

- ✅ Centralisation de tous les textes
- ✅ Facilite la traduction (fichiers de traduction séparés)
- ✅ Pas de modification du code pour changer la langue
- ✅ Support natif de Delphi pour l'extraction et la traduction
- ✅ Type-safe (vérification à la compilation)

## Organisation des ResourceString

Il est recommandé de bien organiser vos ressources pour faciliter la maintenance.

### Structure par modules

```pascal
unit MessagesAuthentification;

interface

resourcestring
  // Connexion
  MSG_AUTH_CONNEXION = 'Connexion';
  MSG_AUTH_IDENTIFIANT = 'Identifiant';
  MSG_AUTH_MOT_DE_PASSE = 'Mot de passe';
  MSG_AUTH_SE_CONNECTER = 'Se connecter';
  MSG_AUTH_ERREUR = 'Identifiant ou mot de passe incorrect';
  MSG_AUTH_SUCCES = 'Connexion réussie';

implementation

end.
```

```pascal
unit MessagesDocument;

interface

resourcestring
  // Gestion de documents
  MSG_DOC_NOUVEAU = 'Nouveau document';
  MSG_DOC_OUVRIR = 'Ouvrir un document';
  MSG_DOC_ENREGISTRER = 'Enregistrer';
  MSG_DOC_ENREGISTRER_SOUS = 'Enregistrer sous...';
  MSG_DOC_FERMER = 'Fermer le document';
  MSG_DOC_MODIFIE = 'Le document a été modifié. Voulez-vous l''enregistrer ?';

implementation

end.
```

### Convention de nommage

Une bonne pratique consiste à utiliser des préfixes pour identifier le type de texte :

| Préfixe | Utilisation | Exemple |
|---------|-------------|---------|
| `MSG_` | Messages d'information ou d'erreur | `MSG_ERREUR_FICHIER` |
| `LBL_` | Libellés de composants | `LBL_NOM_UTILISATEUR` |
| `BTN_` | Textes de boutons | `BTN_VALIDER` |
| `TITLE_` | Titres de fenêtres | `TITLE_PARAMÈTRES` |
| `HINT_` | Bulles d'aide (hints) | `HINT_BOUTON_SAUVEGARDER` |
| `ERR_` | Messages d'erreur spécifiques | `ERR_CONNEXION_BDD` |
| `CONFIRM_` | Messages de confirmation | `CONFIRM_SUPPRESSION` |

## Localisation des formulaires (fichiers DFM)

Les formulaires Delphi stockent leurs propriétés visuelles dans des fichiers `.dfm`. Ces fichiers contiennent les textes des composants (captions, hints, etc.).

### Structure d'un fichier DFM

Un fichier DFM ressemble à ceci :

```
object Form1: TForm1
  Caption = 'Ma Première Application'

  object Button1: TButton
    Caption = 'Cliquez ici'
    Hint = 'Cliquez pour valider'
  end

  object Label1: TLabel
    Caption = 'Nom :'
  end
end
```

### Activation de la localisation

Pour rendre un formulaire localisable, vous devez activer la propriété `Localizable` du formulaire :

1. Sélectionnez le formulaire dans l'IDE
2. Dans l'Inspecteur d'objets, trouvez la propriété `Localizable`
3. Changez sa valeur à `True`

> 💡 **Important** : Une fois `Localizable` activé, Delphi créera automatiquement des fichiers de ressources pour chaque langue.

### Création de versions linguistiques

Une fois la localisation activée :

1. Sélectionnez le formulaire
2. Dans l'Inspecteur d'objets, modifiez la propriété `Language`
3. Choisissez une langue (par exemple, `English`)
4. Modifiez les textes des composants dans cette langue

Delphi créera automatiquement un fichier `.dfm` séparé pour chaque langue.

**Exemple de structure de fichiers :**

```
MonFormulaire.pas          // Code source  
MonFormulaire.dfm          // Version française (par défaut)  
MonFormulaire.en.dfm       // Version anglaise  
MonFormulaire.es.dfm       // Version espagnole  
MonFormulaire.de.dfm       // Version allemande  
```

### Changement de langue à l'exécution

Pour changer la langue de votre application à l'exécution, utilisez l'unit `System.SysUtils` :

```pascal
uses
  System.SysUtils;

procedure TForm1.ChangerLangue(const Langue: string);  
begin  
  // 'fr' pour français, 'en' pour anglais, 'es' pour espagnol, etc.
  SetCurrentLanguage(Langue);

  // Recharger les formulaires pour appliquer la nouvelle langue
  // (nécessite généralement un redémarrage de l'application
  // ou une recréation des formulaires)
end;
```

## Fichiers de ressources RC/RES

Les fichiers de ressources permettent d'embarquer du contenu (textes, images, sons) directement dans l'exécutable.

### Création d'un fichier de ressources (.RC)

Créez un fichier texte avec l'extension `.rc` :

```
// Fichier : MesRessources.rc

STRINGTABLE  
BEGIN  
  1, "Bienvenue"
  2, "Au revoir"
  3, "Erreur"
  4, "Succès"
END
```

### Compilation du fichier RC

Delphi compile automatiquement les fichiers `.rc` en fichiers `.res` lors de la compilation du projet.

### Utilisation des ressources dans le code

```pascal
uses
  Winapi.Windows;

function ChargerChaineRessource(ID: Integer): string;  
var  
  Buffer: array[0..255] of Char;
begin
  LoadString(HInstance, ID, Buffer, Length(Buffer));
  Result := Buffer;
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Message: string;
begin
  Message := ChargerChaineRessource(1); // Charge "Bienvenue"
  ShowMessage(Message);
end;
```

## Fichiers externes de traduction

Pour plus de flexibilité, vous pouvez stocker vos traductions dans des fichiers externes.

### Format INI

```ini
; Fichier : Traduction_FR.ini

[Messages]
Bienvenue=Bienvenue dans l'application  
AuRevoir=Au revoir et à bientôt  
ErreurFichier=Impossible de charger le fichier  

[Boutons]
Valider=Valider  
Annuler=Annuler  
Fermer=Fermer  

[Libelles]
Nom=Nom :  
Prenom=Prénom :  
Email=E-mail :  
```

### Chargement d'un fichier INI

```pascal
uses
  System.IniFiles, System.SysUtils;

type
  TTraducteur = class
  private
    FIniFile: TIniFile;
  public
    constructor Create(const CheminFichier: string);
    destructor Destroy; override;
    function Traduire(const Section, Cle: string): string;
  end;

constructor TTraducteur.Create(const CheminFichier: string);  
begin  
  inherited Create;
  FIniFile := TIniFile.Create(CheminFichier);
end;

destructor TTraducteur.Destroy;  
begin  
  FIniFile.Free;
  inherited;
end;

function TTraducteur.Traduire(const Section, Cle: string): string;  
begin  
  Result := FIniFile.ReadString(Section, Cle, Cle); // Retourne la clé si non trouvée
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);  
var  
  Traducteur: TTraducteur;
begin
  Traducteur := TTraducteur.Create('Traduction_FR.ini');
  try
    Button1.Caption := Traducteur.Traduire('Boutons', 'Valider');
    Label1.Caption := Traducteur.Traduire('Libelles', 'Nom');
    ShowMessage(Traducteur.Traduire('Messages', 'Bienvenue'));
  finally
    Traducteur.Free;
  end;
end;
```

### Format JSON

Les fichiers JSON sont de plus en plus populaires pour stocker les traductions.

```json
{
  "messages": {
    "bienvenue": "Bienvenue dans l'application",
    "auRevoir": "Au revoir et à bientôt",
    "erreurFichier": "Impossible de charger le fichier"
  },
  "boutons": {
    "valider": "Valider",
    "annuler": "Annuler",
    "fermer": "Fermer"
  },
  "libelles": {
    "nom": "Nom :",
    "prenom": "Prénom :",
    "email": "E-mail :"
  }
}
```

### Chargement d'un fichier JSON

```pascal
uses
  System.JSON, System.IOUtils;

function ChargerTraductionJSON(const CheminFichier: string): TJSONObject;  
var  
  ContenuJSON: string;
begin
  ContenuJSON := TFile.ReadAllText(CheminFichier, TEncoding.UTF8);
  Result := TJSONObject.ParseJSONValue(ContenuJSON) as TJSONObject;
end;

function ObtenirTraduction(JSON: TJSONObject; const Categorie, Cle: string): string;  
var  
  Section: TJSONObject;
begin
  Result := Cle; // Valeur par défaut

  Section := JSON.GetValue<TJSONObject>(Categorie);
  if Assigned(Section) then
    Result := Section.GetValue<string>(Cle);
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);  
var  
  Traductions: TJSONObject;
begin
  Traductions := ChargerTraductionJSON('Traduction_FR.json');
  try
    Button1.Caption := ObtenirTraduction(Traductions, 'boutons', 'valider');
    Label1.Caption := ObtenirTraduction(Traductions, 'libelles', 'nom');
    ShowMessage(ObtenirTraduction(Traductions, 'messages', 'bienvenue'));
  finally
    Traductions.Free;
  end;
end;
```

## Système de traduction centralisé

Pour gérer efficacement les traductions dans une grande application, il est recommandé de créer un gestionnaire de traduction centralisé.

### Classe de gestion des traductions

```pascal
unit GestionnaireTraduction;

interface

uses
  System.SysUtils, System.Generics.Collections, System.IniFiles;

type
  TGestionnaireTraduction = class
  private
    FLangueActive: string;
    FTraductions: TDictionary<string, string>;
    procedure ChargerLangue(const CodeLangue: string);
  public
    constructor Create;
    destructor Destroy; override;

    function T(const Cle: string): string; // T = Traduire
    procedure DefinirLangue(const CodeLangue: string);
    function LangueActive: string;
  end;

var
  Traduction: TGestionnaireTraduction; // Instance globale

implementation

constructor TGestionnaireTraduction.Create;  
begin  
  inherited;
  FTraductions := TDictionary<string, string>.Create;
  FLangueActive := 'fr'; // Langue par défaut
  ChargerLangue(FLangueActive);
end;

destructor TGestionnaireTraduction.Destroy;  
begin  
  FTraductions.Free;
  inherited;
end;

procedure TGestionnaireTraduction.ChargerLangue(const CodeLangue: string);  
var  
  IniFile: TIniFile;
  Sections, Cles: TStringList;
  i, j: Integer;
  Cle, Valeur: string;
begin
  FTraductions.Clear;

  IniFile := TIniFile.Create(Format('Lang\%s.ini', [CodeLangue]));
  Sections := TStringList.Create;
  Cles := TStringList.Create;
  try
    // Charger toutes les sections
    IniFile.ReadSections(Sections);

    // Pour chaque section
    for i := 0 to Sections.Count - 1 do
    begin
      Cles.Clear;
      IniFile.ReadSection(Sections[i], Cles);

      // Pour chaque clé dans la section
      for j := 0 to Cles.Count - 1 do
      begin
        Cle := Sections[i] + '.' + Cles[j];
        Valeur := IniFile.ReadString(Sections[i], Cles[j], '');
        FTraductions.Add(Cle, Valeur);
      end;
    end;
  finally
    Cles.Free;
    Sections.Free;
    IniFile.Free;
  end;
end;

function TGestionnaireTraduction.T(const Cle: string): string;  
begin  
  if not FTraductions.TryGetValue(Cle, Result) then
    Result := Cle; // Retourne la clé si la traduction n'existe pas
end;

procedure TGestionnaireTraduction.DefinirLangue(const CodeLangue: string);  
begin  
  FLangueActive := CodeLangue;
  ChargerLangue(CodeLangue);
end;

function TGestionnaireTraduction.LangueActive: string;  
begin  
  Result := FLangueActive;
end;

initialization
  Traduction := TGestionnaireTraduction.Create;

finalization
  Traduction.Free;

end.
```

### Utilisation du gestionnaire

```pascal
uses
  GestionnaireTraduction;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Utilisation simple avec la fonction T()
  Button1.Caption := Traduction.T('Boutons.Valider');
  Button2.Caption := Traduction.T('Boutons.Annuler');
  Label1.Caption := Traduction.T('Libelles.Nom');
  ShowMessage(Traduction.T('Messages.Bienvenue'));
end;

procedure TForm1.ChangerLangueEnAnglais;  
begin  
  Traduction.DefinirLangue('en');

  // Mettre à jour l'interface
  Button1.Caption := Traduction.T('Boutons.Valider');
  Button2.Caption := Traduction.T('Boutons.Annuler');
  Label1.Caption := Traduction.T('Libelles.Nom');
end;
```

## Structure de fichiers recommandée

Pour un projet multilingue professionnel, voici une organisation recommandée :

```
MonProjet/
│
├── Lang/                    # Dossier des traductions
│   ├── fr.ini              # Traduction française
│   ├── en.ini              # Traduction anglaise
│   ├── es.ini              # Traduction espagnole
│   └── de.ini              # Traduction allemande
│
├── Resources/              # Ressources compilées
│   ├── Strings.rc          # Chaînes de ressources
│   └── Strings.res         # Ressources compilées
│
└── Source/
    ├── GestionnaireTraduction.pas
    └── Messages.pas         # ResourceStrings
```

## Outils de traduction

### Extraction des textes à traduire

Delphi propose des outils pour extraire automatiquement tous les textes de votre application :

1. **Gestionnaire de traduction intégré** (Integrated Translation Manager)
2. **Outils tiers** comme :
   - Sisulizer
   - SDL Passolo
   - LocFactory

### Processus de traduction typique

```
1. Développement en langue source (français)
   ↓
2. Extraction des textes à traduire
   ↓
3. Envoi aux traducteurs
   ↓
4. Réception des traductions
   ↓
5. Intégration dans l'application
   ↓
6. Tests dans chaque langue
```

## Bonnes pratiques

### 1. Utiliser des clés descriptives

**Mauvais :**
```pascal
resourcestring
  STR001 = 'Enregistrer';
  STR002 = 'Annuler';
```

**Bon :**
```pascal
resourcestring
  BTN_ENREGISTRER = 'Enregistrer';
  BTN_ANNULER = 'Annuler';
```

### 2. Éviter les concaténations

**Mauvais :**
```pascal
Message := 'Bonjour ' + Nom + ', vous avez ' + IntToStr(Age) + ' ans';
```

Les traducteurs ne pourront pas réorganiser les mots selon la grammaire de leur langue.

**Bon :**
```pascal
resourcestring
  MSG_ACCUEIL = 'Bonjour %s, vous avez %d ans';

// Utilisation
Message := Format(MSG_ACCUEIL, [Nom, Age]);
```

### 3. Prévoir de l'espace pour les traductions

Certaines langues (comme l'allemand) produisent des textes plus longs. Prévoyez de l'espace supplémentaire dans votre interface.

```
Français : "Enregistrer"    → 11 caractères  
Anglais  : "Save"           →  4 caractères  
Allemand : "Speichern"      → 10 caractères  
Italien  : "Salva"          →  5 caractères  
```

### 4. Documenter le contexte

Ajoutez des commentaires pour aider les traducteurs :

```pascal
resourcestring
  // Bouton pour valider un formulaire de saisie
  BTN_VALIDER = 'Valider';

  // Message affiché après un enregistrement réussi
  MSG_ENREGISTREMENT_OK = 'Vos données ont été enregistrées avec succès';

  // Titre de la fenêtre de paramètres (max 30 caractères)
  TITLE_PARAMETRES = 'Paramètres de l''application';
```

### 5. Tester dans toutes les langues

Créez une checklist de vérification pour chaque langue :

- [ ] Tous les textes sont traduits
- [ ] Aucun texte ne dépasse les limites de l'interface
- [ ] Les raccourcis clavier sont cohérents
- [ ] Les formats de date/heure sont corrects
- [ ] Les symboles monétaires sont appropriés
- [ ] Les caractères spéciaux s'affichent correctement

## Gestion des pluriels

Certaines langues ont des règles de pluriel complexes. Voici comment les gérer :

```pascal
resourcestring
  MSG_FICHIER_SINGULIER = '%d fichier sélectionné';
  MSG_FICHIER_PLURIEL = '%d fichiers sélectionnés';

function MessageFichiers(Nombre: Integer): string;  
begin  
  if Nombre <= 1 then
    Result := Format(MSG_FICHIER_SINGULIER, [Nombre])
  else
    Result := Format(MSG_FICHIER_PLURIEL, [Nombre]);
end;
```

> ⚠️ **Attention** : Les règles de pluriel varient selon les langues. Le russe, par exemple, a trois formes de pluriel !

## Fallback (retour à une langue par défaut)

Il est important de gérer les cas où une traduction est manquante :

```pascal
function TGestionnaireTraduction.T(const Cle: string): string;  
begin  
  // Essayer dans la langue active
  if not FTraductions.TryGetValue(Cle, Result) then
  begin
    // Si non trouvé, essayer en français (langue par défaut)
    if FLangueActive <> 'fr' then
    begin
      ChargerLangue('fr');
      if not FTraductions.TryGetValue(Cle, Result) then
        Result := Cle; // En dernier recours, retourner la clé
      ChargerLangue(FLangueActive); // Recharger la langue active
    end
    else
      Result := Cle;
  end;
end;
```

## Conclusion

Les ressources linguistiques sont essentielles pour créer des applications internationales de qualité. Delphi offre plusieurs approches, de la plus simple (`ResourceString`) à la plus sophistiquée (systèmes de traduction externes).

**Recommandations selon la taille du projet :**

| Taille du projet | Approche recommandée |
|------------------|---------------------|
| Petite application | `ResourceString` uniquement |
| Application moyenne | `ResourceString` + Localisation des formulaires |
| Grande application | Gestionnaire de traduction centralisé + Fichiers externes |
| Application d'entreprise | Système complet avec outils de traduction professionnels |

Dans la prochaine section, nous verrons comment adapter concrètement votre application à différentes langues et cultures, en exploitant ces ressources linguistiques.

⏭️ [Adaptation à différentes langues](/13-internationalisation-et-localisation/03-adaptation-a-differentes-langues.md)
