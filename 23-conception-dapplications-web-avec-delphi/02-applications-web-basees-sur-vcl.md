# 23.2 Applications Web basées sur VCL

## Introduction aux applications Web basées sur VCL

Si vous êtes familier avec le développement d'applications desktop en Delphi, vous apprécierez la possibilité de réutiliser vos connaissances VCL (Visual Component Library) pour créer des applications web. Cette approche vous permet de faire la transition vers le développement web tout en conservant la puissance et la familiarité de l'environnement Delphi.

Dans cette section, nous allons explorer comment adapter vos compétences VCL pour le développement web, en nous concentrant sur les technologies qui permettent d'utiliser des concepts similaires à la VCL dans un contexte web.

## Comprendre le concept d'applications Web basées sur VCL

Avant de plonger dans les détails techniques, clarifions ce que nous entendons par "applications Web basées sur VCL":

- Ce ne sont **pas** des applications VCL traditionnelles qui fonctionnent dans un navigateur
- Ce sont des applications web qui utilisent une approche de développement similaire à celle de la VCL
- Elles permettent de réutiliser certains concepts, méthodes et parfois même du code de vos applications VCL existantes

## Les technologies principales

Delphi propose plusieurs approches pour créer des applications web avec une expérience de développement proche de la VCL:

### 1. Intraweb avec composants VCL-like

Comme nous l'avons vu dans la section précédente, IntraWeb offre une approche de développement visuel similaire à la VCL:

- Vous concevez votre interface en déposant des composants sur une fiche
- Les composants IntraWeb ressemblent à leurs équivalents VCL (`IWButton` ≈ `TButton`, `IWEdit` ≈ `TEdit`, etc.)
- Les événements fonctionnent de manière similaire (OnClick, OnChange, etc.)

### 2. UniGUI

UniGUI est une solution tierce populaire qui permet de développer des applications web avec une expérience VCL:

- Interface de conception visuelle similaire à la VCL
- Composants qui correspondent aux contrôles VCL
- Gestion automatique de l'état de l'application
- Support AJAX pour une expérience utilisateur fluide

### 3. TMS Web Core avec composants FNC

TMS Web Core, mentionné précédemment, propose des composants FNC (Framework Neutral Components) qui peuvent être utilisés à la fois dans des applications VCL et web:

- Réutilisation de code entre applications desktop et web
- Composants visuellement similaires aux contrôles VCL
- Logique d'affaires partagée entre différentes plateformes

## Différences clés avec les applications VCL traditionnelles

Bien que ces technologies offrent une expérience similaire à la VCL, il est important de comprendre les différences fondamentales:

| Aspect | VCL traditionnelle | Applications Web basées sur VCL |
|--------|-------------------|--------------------------------|
| Exécution | Sur le poste client | Sur serveur ou compilé en JavaScript |
| Rafraîchissement | Mise à jour immédiate | Peut nécessiter des requêtes au serveur |
| État | Persistant pendant l'exécution | Doit être géré explicitement ou automatiquement |
| Ressources | Accès direct au système local | Limité par le contexte du navigateur |
| Rendu | Natif Windows | HTML, CSS et JavaScript |
| Événements | Synchrones | Souvent asynchrones |

## Création d'une application Web simple avec approche VCL

Pour illustrer cette approche, créons une application simple qui montre comment la philosophie VCL s'applique au développement web. Nous utiliserons IntraWeb comme exemple.

### Étape 1: Création du projet

1. Dans Delphi, sélectionnez **Fichier** > **Nouveau** > **Autres** > **IntraWeb** > **Application**
2. Choisissez "Application Standalone" pour créer une application autonome
3. Une fiche web vide apparaît, similaire à une fiche VCL

### Étape 2: Conception de l'interface

Disposez les composants suivants sur la fiche:

- Un `IWLabel` avec le texte "Nom:"
- Un `IWEdit` nommé `edtNom`
- Un `IWButton` nommé `btnSaluer` avec le texte "Saluer"
- Un `IWLabel` nommé `lblResultat` (vide initialement)

L'interface ressemblera beaucoup à ce que vous feriez dans une application VCL standard.

### Étape 3: Implémentation du code

Double-cliquez sur le bouton `btnSaluer` pour créer son gestionnaire d'événement et ajoutez le code suivant:

```delphi
procedure TIWForm1.btnSaluerClick(Sender: TObject);
var
  Nom: string;
begin
  Nom := edtNom.Text;
  if Nom <> '' then
    lblResultat.Caption := 'Bonjour, ' + Nom + ' ! Bienvenue sur votre première application web basée sur VCL!'
  else
    lblResultat.Caption := 'Veuillez entrer votre nom.';
end;
```

Ce code est pratiquement identique à ce que vous écririez dans une application VCL traditionnelle!

## Adaptation des concepts VCL pour le Web

### Formulaires et navigation

Dans les applications VCL, vous utilisez souvent plusieurs formulaires et naviguez entre eux. Dans le contexte web, cela fonctionne différemment:

#### Avec IntraWeb:

```delphi
// Pour afficher un autre formulaire
IWForm2.Show;

// Ou avec paramètres
with TIWForm2.Create(WebApplication) do
begin
  NomUtilisateur := edtNom.Text;
  Show;
end;
```

### Gestion des données

La liaison aux données dans les applications web basées sur VCL ressemble beaucoup à ce que vous connaissez:

```delphi
// Exemple avec IntraWeb
procedure TIWForm1.IWAppFormCreate(Sender: TObject);
begin
  // Connexion à la base de données
  FDConnection1.Connected := True;

  // Ouverture du dataset
  FDQuery1.Open;

  // Liaison d'un DBGrid (similaire à la VCL)
  IWDBGrid1.DataSource := DataSource1;
end;
```

## Exemple pratique: Liste de tâches (Todo List)

Voyons comment créer une simple liste de tâches avec une approche VCL sur le web.

### Interface utilisateur:

- `IWEdit` pour saisir une nouvelle tâche
- `IWButton` pour ajouter la tâche
- `IWListBox` pour afficher les tâches
- `IWButton` pour supprimer une tâche sélectionnée

### Code d'implémentation:

```delphi
// Ajout d'une tâche
procedure TIWForm1.btnAjouterClick(Sender: TObject);
var
  NouveauTache: string;
begin
  NouveauTache := edtNouvelleTache.Text;
  if NouveauTache <> '' then
  begin
    lstTaches.Items.Add(NouveauTache);
    edtNouvelleTache.Text := '';
    lblStatut.Caption := 'Tâche ajoutée!';
  end
  else
    lblStatut.Caption := 'Veuillez entrer une tâche.';
end;

// Suppression d'une tâche
procedure TIWForm1.btnSupprimerClick(Sender: TObject);
begin
  if lstTaches.ItemIndex >= 0 then
  begin
    lstTaches.Items.Delete(lstTaches.ItemIndex);
    lblStatut.Caption := 'Tâche supprimée!';
  end
  else
    lblStatut.Caption := 'Veuillez sélectionner une tâche à supprimer.';
end;
```

Ce code est quasi identique à ce que vous écririez dans une application VCL desktop!

## Adaptation de techniques VCL avancées pour le Web

### Création de composants personnalisés

Tout comme avec la VCL, vous pouvez créer vos propres composants pour réutilisation:

```delphi
// Exemple de composant personnalisé IntraWeb
unit MyCustomIWEdit;

interface

uses
  Classes, SysUtils, IWCompEdit;

type
  TMyCustomIWEdit = class(TIWEdit)
  private
    FValidPattern: string;
    function IsValid: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property IsValidInput: Boolean read IsValid;
  published
    property ValidationPattern: string read FValidPattern write FValidPattern;
  end;

implementation

constructor TMyCustomIWEdit.Create(AOwner: TComponent);
begin
  inherited;
  FValidPattern := '';
end;

function TMyCustomIWEdit.IsValid: Boolean;
begin
  // Implémenter la logique de validation ici
  Result := True;
end;

end.
```

### Actions et menus

Les actions fonctionnent de manière similaire dans les applications web basées sur VCL:

```delphi
// Configuration d'une action
procedure TIWForm1.IWAppFormCreate(Sender: TObject);
begin
  actSave.Caption := 'Enregistrer';
  actSave.OnExecute := actSaveExecute;

  // Association avec un bouton
  IWButton1.Action := actSave;
end;

procedure TIWForm1.actSaveExecute(Sender: TObject);
begin
  // Logique pour sauvegarder
  lblStatus.Caption := 'Données enregistrées!';
end;
```

## Considérations pour le déploiement

Les applications web basées sur VCL nécessitent généralement un serveur web pour fonctionner:

1. **Serveur intégré**: Solutions comme IntraWeb et UniGUI incluent souvent un serveur HTTP intégré pour le développement et les petits déploiements
2. **Serveur web standard**: Déployez sous forme de DLL (ISAPI) sur IIS ou en tant que CGI sur d'autres serveurs web
3. **Serveurs d'applications**: Certaines solutions offrent leurs propres serveurs d'applications pour le déploiement en production

## Avantages et inconvénients des applications Web basées sur VCL

### Avantages:
- Réutilisation des compétences VCL existantes
- Développement rapide avec conception visuelle
- Moins de connaissances web (HTML, CSS, JavaScript) requises
- Code métier potentiellement partageable entre desktop et web
- Gestion automatique de nombreux aspects web complexes

### Inconvénients:
- Moins de contrôle direct sur le HTML/CSS/JavaScript généré
- Parfois moins optimisé qu'un développement web natif
- Peut être limité pour des interfaces utilisateur web très avancées
- Dépendance envers des technologies spécifiques (IntraWeb, UniGUI, etc.)

## Quand utiliser des applications Web basées sur VCL?

Cette approche est particulièrement adaptée pour:

- Les développeurs Delphi voulant passer au web sans tout réapprendre
- La migration progressive d'applications VCL existantes vers le web
- Des applications d'entreprise internes où l'expérience utilisateur web sophistiquée n'est pas prioritaire
- Des projets nécessitant un développement rapide avec des ressources limitées

## Bonnes pratiques

1. **Comprendre les différences**: Bien que similaire à la VCL, le contexte web a ses propres contraintes
2. **État de l'application**: Soyez conscient de la gestion d'état dans un environnement web
3. **Tester dans différents navigateurs**: Assurez-vous que votre application fonctionne dans tous les navigateurs cibles
4. **Ne pas surcharger l'interface**: Les applications web ont des attentes différentes en termes d'UX
5. **Sécurité web**: Appliquer les bonnes pratiques de sécurité spécifiques au web

## Migration d'une application VCL existante vers le web

Si vous souhaitez migrer une application VCL existante vers le web, voici les étapes générales:

1. **Analyser l'architecture**: Identifiez les composants qui peuvent être directement transposés
2. **Séparer la logique métier**: Isolez le code métier qui peut être réutilisé
3. **Remplacer les composants**: Substituez les composants VCL par leurs équivalents web
4. **Adapter les interactions**: Modifiez les interactions pour tenir compte du contexte web
5. **Tester et optimiser**: Vérifiez les performances et l'expérience utilisateur

### Exemple de migration de code:

Code VCL original:
```delphi
procedure TForm1.ButtonClick(Sender: TObject);
begin
  Label1.Caption := 'Bonjour ' + Edit1.Text;
end;
```

Code équivalent avec IntraWeb:
```delphi
procedure TIWForm1.IWButtonClick(Sender: TObject);
begin
  IWLabel1.Caption := 'Bonjour ' + IWEdit1.Text;
end;
```

## Exemple concret: Formulaire de contact

Pour conclure cette section, implémentons un formulaire de contact complet en utilisant IntraWeb:

```delphi
procedure TContactForm.IWAppFormCreate(Sender: TObject);
begin
  // Initialisation du formulaire
  edtNom.Text := '';
  edtEmail.Text := '';
  mmoMessage.Lines.Clear;

  // Configuration des validations
  edtEmail.InputType := 'email'; // HTML5 validation (nécessite Delphi 12 ou supérieur)
end;

procedure TContactForm.btnEnvoyerClick(Sender: TObject);
var
  IsValid: Boolean;
begin
  // Validation simple
  IsValid := (edtNom.Text <> '') and (edtEmail.Text <> '') and (mmoMessage.Lines.Text <> '');

  if IsValid then
  begin
    // Dans une application réelle, vous enverriez l'email ici
    // SendEmail(edtNom.Text, edtEmail.Text, mmoMessage.Lines.Text);

    // Feedback à l'utilisateur
    lblStatus.Caption := 'Merci! Votre message a été envoyé.';
    lblStatus.Font.Color := clGreen;

    // Réinitialisation du formulaire
    edtNom.Text := '';
    edtEmail.Text := '';
    mmoMessage.Lines.Clear;
  end
  else
  begin
    lblStatus.Caption := 'Veuillez remplir tous les champs.';
    lblStatus.Font.Color := clRed;
  end;
end;
```

## Conclusion

Les applications web basées sur VCL offrent une transition en douceur pour les développeurs Delphi souhaitant créer des applications web. Bien que cette approche ait ses limites par rapport au développement web natif, elle présente des avantages significatifs en termes de vitesse de développement et de réutilisation des compétences.

Dans la prochaine section, nous explorerons comment créer des services REST avec Delphi pour développer des API web robustes qui peuvent être consommées par différents types de clients.

## Exercices pratiques

1. Créez une application IntraWeb avec un formulaire comprenant des champs pour le nom, l'adresse, le téléphone et l'email, ainsi qu'un bouton pour enregistrer les données.

2. Ajoutez une validation pour s'assurer que l'email est au bon format et que le numéro de téléphone contient uniquement des chiffres.

3. Bonus: Ajoutez un second formulaire qui affiche les données saisies et permet de les modifier.
