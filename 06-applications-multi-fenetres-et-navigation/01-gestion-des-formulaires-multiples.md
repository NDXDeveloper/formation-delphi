🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.1 Gestion des formulaires multiples

## Introduction

Dans les applications Delphi, un formulaire (ou fiche) représente une fenêtre de votre application. Jusqu'à présent, vous avez peut-être travaillé avec un seul formulaire principal, mais la plupart des applications réelles nécessitent plusieurs fenêtres pour organiser les fonctionnalités de manière claire et intuitive.

La gestion des formulaires multiples est une compétence essentielle pour créer des applications professionnelles avec Delphi. Elle vous permet de :

- Séparer les différentes fonctionnalités de votre application
- Créer des boîtes de dialogue pour recueillir des informations
- Afficher des fenêtres d'options ou de paramètres
- Organiser votre interface utilisateur de manière modulaire

## Comprendre les formulaires dans Delphi

### Qu'est-ce qu'un formulaire ?

Un formulaire dans Delphi est une classe qui hérite de `TForm`. Chaque formulaire possède :

- Une partie visuelle (l'interface utilisateur)
- Une partie code (la logique de l'application)
- Des propriétés configurables (titre, taille, position, etc.)
- Des événements (OnCreate, OnShow, OnClose, etc.)

### Le formulaire principal

Lorsque vous créez un nouveau projet Delphi, un formulaire principal est automatiquement créé. C'est le point d'entrée de votre application. Lorsque ce formulaire se ferme, l'application se termine généralement.

## Créer un nouveau formulaire

### Étapes de création

1. Dans l'IDE Delphi, allez dans le menu **Fichier** → **Nouveau** → **Fiche VCL** (pour une application VCL) ou **Fiche FireMonkey** (pour une application multi-plateforme)

2. Un nouveau formulaire vierge apparaît dans l'éditeur

3. Deux fichiers sont créés automatiquement :
   - Un fichier `.pas` (le code Object Pascal)
   - Un fichier `.dfm` ou `.fmx` (la description visuelle du formulaire)

### Structure d'un formulaire

Voici à quoi ressemble le code d'un formulaire nouvellement créé :

```pascal
unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm2 = class(TForm)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
```

**Points importants :**

- `TForm2` est le nom de la classe du formulaire
- `Form2` est une variable globale qui représente l'instance du formulaire
- La section `interface` contient les déclarations visibles par les autres unités
- La section `implementation` contient le code de mise en œuvre

## Afficher un formulaire secondaire

### Méthode 1 : Show (affichage non-modal)

Un formulaire non-modal permet à l'utilisateur d'interagir avec d'autres fenêtres de l'application pendant qu'il est affiché.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.Show;
end;
```

**Caractéristiques de Show :**
- L'utilisateur peut passer d'une fenêtre à l'autre librement
- Le code continue son exécution immédiatement après l'appel
- Idéal pour des fenêtres d'outils ou de palettes

### Méthode 2 : ShowModal (affichage modal)

Un formulaire modal bloque l'interaction avec les autres fenêtres jusqu'à ce qu'il soit fermé.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.ShowModal;
  // Le code ici ne s'exécute qu'après la fermeture de Form2
end;
```

**Caractéristiques de ShowModal :**
- L'utilisateur doit fermer cette fenêtre avant de continuer
- Le code s'arrête à cet appel jusqu'à la fermeture du formulaire
- Idéal pour les boîtes de dialogue et les formulaires de saisie
- Retourne une valeur (ModalResult) qui indique comment le formulaire a été fermé

### Récupérer le résultat d'un formulaire modal

`ShowModal` est une **fonction** qui retourne la valeur de `ModalResult` au moment de la fermeture. Vous pouvez donc utiliser directement son résultat dans une comparaison ou un `case` :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  case Form2.ShowModal of
    mrOk:     ShowMessage('L''utilisateur a cliqué sur OK');
    mrCancel: ShowMessage('L''utilisateur a annulé');
    mrYes:    ShowMessage('L''utilisateur a répondu Oui');
    mrNo:     ShowMessage('L''utilisateur a répondu Non');
  end;
end;
```

> 💡 **Pourquoi `case` plutôt que des `if/else if` enchaînés ?** Le `case` est plus lisible quand on a plus de deux valeurs à tester, et le compilateur l'optimise en table de saut. Réservez `if/else if` aux cas où chaque branche teste une condition différente.

Les valeurs `ModalResult` courantes :
- `mrNone` (0) : Aucun résultat — utilisé pour **empêcher la fermeture** depuis `OnClick` d'un bouton de validation
- `mrOk` (1) : Validation (bouton OK)
- `mrCancel` (2) : Annulation (bouton Annuler ou touche Échap)
- `mrAbort`, `mrRetry`, `mrIgnore` : Abandonner / Réessayer / Ignorer
- `mrYes` / `mrNo` : Réponses Oui/Non
- `mrAll`, `mrYesToAll`, `mrNoToAll` : Variantes « pour tout »
- `mrClose`, `mrHelp` : Fermer, Aide

> 💡 **Astuce `mrNone`** : si vous voulez **annuler** la fermeture d'un dialogue modal depuis le clic sur un bouton (par exemple parce que la validation a échoué), faites `ModalResult := mrNone;` dans le gestionnaire `OnClick` — le formulaire reste alors ouvert.

## Gestion de la création et destruction des formulaires

### Auto-création des formulaires

Par défaut, Delphi crée automatiquement tous les formulaires au démarrage de l'application. Vous pouvez voir et modifier cette liste dans **Projet** → **Options** → **Formulaires**.

**Avantages :**
- Simple à utiliser
- Les formulaires sont toujours disponibles

**Inconvénients :**
- Consomme de la mémoire dès le démarrage
- Ralentit le démarrage de l'application si vous avez beaucoup de formulaires

### Création manuelle des formulaires

Pour une meilleure gestion de la mémoire, vous pouvez créer les formulaires à la demande :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  MonFormulaire: TForm2;
begin
  MonFormulaire := TForm2.Create(Self);
  try
    MonFormulaire.ShowModal;
  finally
    MonFormulaire.Free;
  end;
end;
```

**Explications :**
- `TForm2.Create(Self)` crée une nouvelle instance du formulaire
- `Self` indique que Form1 est le propriétaire (owner)
- Le bloc `try...finally` garantit que le formulaire est libéré même en cas d'erreur
- `Free` libère la mémoire occupée par le formulaire

### Vérifier si un formulaire existe

Avant d'utiliser un formulaire, il est prudent de vérifier s'il existe :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  if not Assigned(Form2) then
    Form2 := TForm2.Create(Application);

  Form2.Show;
end;
```

`Assigned()` retourne `True` si la variable contient une référence valide à un objet.

> ⚠️ **Piège du pointeur invalide** : si `Form2` est fermé avec `Action := caFree` dans son `OnClose`, l'objet est libéré mais la **variable globale `Form2` n'est pas remise à `nil` automatiquement**. Un nouvel appel à `Assigned(Form2)` retournera donc `True` et vous accéderez à un objet détruit → violation d'accès.  
>  
> **Solution** : dans `Form2.OnClose`, remettez explicitement la variable à `nil` :  
>  
> ```pascal  
> procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
> begin
>   Action := caFree;
>   Form2 := nil;  // ← important : invalide la variable globale
> end;
> ```

## Masquer et réafficher un formulaire

Plutôt que de créer et détruire un formulaire à chaque utilisation, vous pouvez le masquer et le réafficher :

```pascal
// Masquer un formulaire
Form2.Hide;

// ou
Form2.Visible := False;

// Réafficher un formulaire
Form2.Show;

// ou
Form2.Visible := True;
```

**Avantage :** Le formulaire conserve son état (valeurs des champs, position, etc.)

**Inconvénient :** Le formulaire reste en mémoire

## Fermer un formulaire

### Depuis le formulaire lui-même

```pascal
procedure TForm2.Button1Click(Sender: TObject);  
begin  
  Close;  // Ferme le formulaire
end;
```

### Pour un formulaire modal, définir le ModalResult

```pascal
procedure TForm2.ButtonOKClick(Sender: TObject);  
begin  
  ModalResult := mrOk;  // Ferme automatiquement le formulaire
end;

procedure TForm2.ButtonCancelClick(Sender: TObject);  
begin  
  ModalResult := mrCancel;
end;
```

**Astuce :** Vous pouvez définir la propriété `ModalResult` d'un bouton directement dans l'Inspecteur d'objets, ce qui évite d'écrire du code.

## Passer des données entre formulaires

### Méthode 1 : Utiliser les propriétés publiques

Dans Form2, ajoutez une propriété publique :

```pascal
type
  TForm2 = class(TForm)
    Edit1: TEdit;
  private
    FNomUtilisateur: string;
  public
    property NomUtilisateur: string read FNomUtilisateur write FNomUtilisateur;
  end;
```

Depuis Form1, accédez à cette propriété :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.NomUtilisateur := 'Jean Dupont';
  Form2.ShowModal;
end;
```

### Méthode 2 : Accéder directement aux composants

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.Edit1.Text := 'Valeur initiale';
  if Form2.ShowModal = mrOk then
  begin
    ShowMessage('Valeur saisie : ' + Form2.Edit1.Text);
  end;
end;
```

**Note :** Cette méthode couple fortement les formulaires. La première méthode est préférable pour un code plus maintenable.

### Méthode 3 : Constructeur personnalisé

Créez un constructeur qui accepte des paramètres :

```pascal
type
  TForm2 = class(TForm)
  private
    FNomUtilisateur: string;
  public
    constructor Create(AOwner: TComponent; const ANom: string); reintroduce;
  end;

implementation

constructor TForm2.Create(AOwner: TComponent; const ANom: string);  
begin  
  inherited Create(AOwner);
  FNomUtilisateur := ANom;
end;
```

Utilisation :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  MonForm: TForm2;
begin
  MonForm := TForm2.Create(Self, 'Jean Dupont');
  try
    MonForm.ShowModal;
  finally
    MonForm.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Gérer la mémoire correctement

Toujours libérer les formulaires créés manuellement :

```pascal
// BON
MonForm := TForm2.Create(Self);  
try  
  MonForm.ShowModal;
finally
  MonForm.Free;
end;

// MAUVAIS - Fuite mémoire
MonForm := TForm2.Create(Self);  
MonForm.ShowModal;  
// Le formulaire n'est jamais libéré !
```

### 2. Utiliser des variables locales pour les formulaires temporaires

```pascal
// BON - Variable locale
procedure TForm1.AfficherOptions;  
var  
  FormOptions: TFormOptions;
begin
  FormOptions := TFormOptions.Create(Self);
  try
    FormOptions.ShowModal;
  finally
    FormOptions.Free;
  end;
end;
```

### 3. Éviter les références circulaires

Si Form1 fait référence à Form2 et que Form2 fait référence à Form1, cela peut créer des problèmes. Utilisez plutôt des événements ou des interfaces pour communiquer.

### 4. Ne pas accéder directement aux variables globales des formulaires

```pascal
// MAUVAIS
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.Edit1.Text := 'test';  // Couplage fort
end;

// BON
procedure TForm1.Button1Click(Sender: TObject);  
var  
  F: TForm2;
begin
  F := TForm2.Create(Self);
  try
    F.ConfigurerAvec('test');  // Méthode encapsulée
    F.ShowModal;
  finally
    F.Free;
  end;
end;
```

### 5. Gérer l'événement OnClose correctement

```pascal
procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  // Valeurs possibles de Action :
  //   caHide     : (défaut) le formulaire est seulement caché — il reste en mémoire
  //   caFree     : le formulaire est libéré automatiquement après la fermeture
  //   caMinimize : le formulaire est minimisé au lieu d'être fermé
  //   caNone     : la fermeture est annulée

  // Cas 1 : formulaire modal créé/Free manuellement avec try/finally
  //         → laisser caHide (défaut), votre code Free s'en charge après ShowModal

  // Cas 2 : formulaire non-modal créé dynamiquement et que vous ne référencez plus
  //         → Action := caFree;
end;
```

> ⚠️ **Ne combinez jamais `caFree` avec un `Free` manuel** : vous libéreriez deux fois le même objet, ce qui provoque une violation d'accès. Choisissez **un seul** mode de gestion de la durée de vie.

### 6. Empêcher ou confirmer la fermeture avec `OnCloseQuery`

L'événement `OnCloseQuery` est déclenché **avant** `OnClose` et permet de **bloquer** la fermeture du formulaire en passant `CanClose` à `False` :

```pascal
procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  if FModifie then
  begin
    case MessageDlg('Des modifications n''ont pas été sauvegardées.' + sLineBreak +
                    'Voulez-vous les enregistrer ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:    Enregistrer;            // Sauvegarde puis ferme
      mrNo:     ;                       // Ferme sans sauvegarder
      mrCancel: CanClose := False;      // Annule la fermeture
    end;
  end;
end;
```

**Différence avec `OnClose` :**

| Événement       | Rôle                                                   | Variable de contrôle |
|-----------------|--------------------------------------------------------|----------------------|
| `OnCloseQuery`  | Décide **si** le formulaire peut être fermé           | `CanClose: Boolean`  |
| `OnClose`       | Décide **comment** la fermeture sera effectuée        | `Action: TCloseAction` |

> 💡 `OnCloseQuery` est l'endroit idéal pour valider une saisie obligatoire ou prévenir une perte de données ; `OnClose` sert à gérer le devenir du formulaire (libération, masquage…).

## Position et taille des formulaires

### Définir la position d'affichage

```pascal
// Au centre de l'écran
Form2.Position := poScreenCenter;

// Au centre du formulaire parent
Form2.Position := poMainFormCenter;

// Position personnalisée
Form2.Position := poDesigned;  // Utilise les coordonnées définies  
Form2.Left := 100;  
Form2.Top := 100;  
```

### Adapter la taille

```pascal
// Taille fixe
Form2.BorderStyle := bsDialog;  // L'utilisateur ne peut pas redimensionner

// Taille ajustable
Form2.BorderStyle := bsSizeable;

// Définir les contraintes
Form2.Constraints.MinWidth := 400;  
Form2.Constraints.MinHeight := 300;  
```

## Ordre d'affichage (Z-Order)

Pour mettre un formulaire au premier plan :

```pascal
Form2.BringToFront;
```

Pour l'envoyer à l'arrière-plan :

```pascal
Form2.SendToBack;
```

### Maintenir un formulaire au-dessus des autres

Pour qu'une fenêtre d'outils ou de palette **reste toujours au-dessus** des autres fenêtres (même quand elle perd le focus), utilisez `FormStyle := fsStayOnTop` :

```pascal
// Dans l'OnCreate ou directement dans l'Inspecteur d'objets
Form2.FormStyle := fsStayOnTop;
```

> 💡 **Cas d'usage** : palettes d'outils, mini-lecteurs, fenêtres de surveillance en temps réel. À utiliser avec parcimonie — un formulaire `fsStayOnTop` est intrusif si l'utilisateur travaille avec d'autres applications.

### Choisir le parent visuel d'un formulaire non-modal

Par défaut, un formulaire non-modal créé avec `Show` peut « disparaître » derrière le formulaire principal quand celui-ci reprend le focus. La propriété `PopupMode` règle ce comportement :

```pascal
// pmNone     : comportement « Win32 classique » (pré-Delphi 8) — peut passer derrière
// pmAuto     : le PopupParent devient automatiquement Screen.ActiveForm.
//              C'est ce que ShowModal fait implicitement depuis Delphi 8+.
// pmExplicit : vous désignez vous-même PopupParent.
//              Si PopupParent est nil, Application.MainForm est utilisé.
Form2.PopupMode := pmExplicit;  
Form2.PopupParent := Self;  // Self = le formulaire qui crée Form2  
Form2.Show;  
```

> 💡 **Astuce VCL** : pour qu'une **fenêtre d'outils flottante** (tool window) reste toujours au-dessus de votre formulaire principal sans bloquer son interaction, utilisez `PopupMode := pmExplicit` et `PopupParent := MonFormulairePrincipal`. C'est la méthode recommandée par Embarcadero, plus prévisible que `fsStayOnTop` (qui place la fenêtre au-dessus de **toutes** les autres applications).

## Résumé

La gestion des formulaires multiples dans Delphi offre une grande flexibilité pour organiser votre application. Les points clés à retenir :

- **Show** pour les fenêtres non-modales (interaction libre)
- **ShowModal** pour les boîtes de dialogue (bloque l'interaction)
- Toujours gérer la création et la destruction des formulaires
- Utiliser des propriétés et méthodes pour passer des données entre formulaires
- Respecter les bonnes pratiques pour éviter les fuites mémoire
- Définir le `ModalResult` pour faciliter la communication avec les formulaires modaux
- **`OnCloseQuery`** pour décider **si** un formulaire peut être fermé, **`OnClose`** pour décider **comment** (libération, masquage, minimisation)
- Ne jamais combiner `Action := caFree` avec un `Free` manuel (double libération)

La maîtrise de ces concepts vous permettra de créer des applications Delphi avec des interfaces utilisateur riches et bien organisées.

⏭️ [Communication entre formulaires](/06-applications-multi-fenetres-et-navigation/02-communication-entre-formulaires.md)
