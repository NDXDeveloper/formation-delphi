# 4.9 Migration depuis des versions précédentes de Delphi

La migration d'un projet depuis une version antérieure de Delphi vers Delphi 12 Athens (ou Delphi 11 Alexandria) peut sembler intimidante au premier abord. Cependant, avec une approche méthodique et quelques précautions, ce processus peut se dérouler sans difficultés majeures. Cette section vous guidera à travers les étapes essentielles et les considérations importantes pour réussir votre migration.

## Pourquoi migrer vers une version plus récente ?

Avant de nous plonger dans le "comment", il est utile de comprendre le "pourquoi" :

- **Nouvelles fonctionnalités** : Les versions récentes offrent de nombreuses améliorations pour la VCL
- **Support des technologies modernes** : Meilleure prise en charge des systèmes d'exploitation récents
- **Corrections de bugs** : Les versions plus récentes corrigent des problèmes des versions antérieures
- **Performances améliorées** : Optimisations du compilateur et des bibliothèques
- **Support à long terme** : Les anciennes versions ne reçoivent plus de mises à jour

## Préparation avant la migration

### 1. Sauvegarde de votre projet

C'est l'étape la plus importante. Avant toute migration, assurez-vous de :

- Créer une copie complète de votre projet
- Utiliser un système de contrôle de version (Git, SVN, etc.) si possible
- Sauvegarder non seulement le code, mais aussi les configurations, ressources et bibliothèques

```
📂 Projet_Original                   📂 Projet_Migration
 ┣ 📂 Source                          ┣ 📂 Source
 ┣ 📂 Resources                       ┣ 📂 Resources
 ┣ 📂 Libs                            ┣ 📂 Libs
 ┣ 📄 Project1.dproj                  ┣ 📄 Project1.dproj
 ┗ 📄 Project1.dpr                    ┗ 📄 Project1.dpr
```

### 2. Inventaire des dépendances

Listez tous les éléments externes dont dépend votre projet :

- Composants tiers
- Bibliothèques DLL/BPL
- Frameworks (par exemple, DevExpress, TMS, etc.)
- Outils spécifiques

Vérifiez si des versions compatibles avec Delphi 12 Athens (ou Delphi 11) sont disponibles pour chacun.

### 3. Vérification de la compatibilité

Consultez la documentation de Delphi pour identifier :

- Les fonctionnalités obsolètes ou supprimées
- Les changements dans les API que vous utilisez
- Les nouvelles directives de compilation nécessaires

## Processus de migration étape par étape

### Étape 1 : Installation des composants tiers

Avant d'ouvrir votre projet dans la nouvelle version :

1. Installez les versions compatibles de tous vos composants tiers
2. Vérifiez qu'ils apparaissent correctement dans l'IDE
3. Testez-les avec un petit projet si possible

### Étape 2 : Ouverture initiale du projet

1. Lancez la nouvelle version de Delphi
2. Ouvrez votre projet (.dproj ou .dpr)
3. Delphi vous proposera généralement de migrer le projet automatiquement

```
┌────────────────────────────────────────────────┐
│ Confirmation                                   │
│                                                │
│ Ce projet a été créé avec une version          │
│ antérieure de Delphi. Voulez-vous le           │
│ convertir pour cette version ?                 │
│                                                │
│         [Oui]    [Non]    [Annuler]           │
└────────────────────────────────────────────────┘
```

4. Acceptez la conversion et notez tous les avertissements qui apparaissent

### Étape 3 : Résolution des problèmes de compilation

Après la conversion, tentez une première compilation (Shift+F9) pour identifier les erreurs. Les problèmes courants incluent :

#### Changements de noms d'unités

Dans les versions récentes, les préfixes des unités ont été réorganisés :

| Ancien nom | Nouveau nom |
|------------|-------------|
| Windows    | Winapi.Windows |
| Messages   | Winapi.Messages |
| Classes    | System.Classes |
| SysUtils   | System.SysUtils |
| Graphics   | Vcl.Graphics |
| Controls   | Vcl.Controls |
| Forms      | Vcl.Forms |

Solution : Mettez à jour les clauses `uses` avec les nouveaux noms. Delphi peut souvent suggérer les corrections.

```pascal
// Ancien code
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

// Nouveau code
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms;
```

#### Fonctions et procédures obsolètes

Certaines fonctions ont été remplacées par de nouvelles alternatives.

Solution : Consultez la documentation pour trouver les équivalents modernes.

```pascal
// Ancien code
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buffer);
  // Utiliser Buffer...
end;

// Nouveau code
var
  TempPath: string;
begin
  TempPath := TPath.GetTempPath;
  // Utiliser TempPath...
end;
```

#### Modifications des types de données

Certains types ont changé ou nécessitent des ajustements.

Solution : Mettez à jour les déclarations et les conversions.

```pascal
// Ancien code (32 bits)
var
  Handle: Integer;

// Nouveau code (compatible 32/64 bits)
var
  Handle: THandle;
```

### Étape 4 : Vérification des avertissements

Après avoir résolu les erreurs, examinez les avertissements :

1. Activez "Afficher tous les avertissements" dans les options du projet
2. Compilez à nouveau et examinez attentivement chaque avertissement
3. Corrigez-les autant que possible pour améliorer la qualité et la compatibilité du code

### Étape 5 : Test de l'application

Une fois le projet compilé avec succès :

1. Testez toutes les fonctionnalités de l'application
2. Vérifiez particulièrement les zones qui utilisaient des fonctionnalités obsolètes
3. Testez sur différentes configurations (Windows 10, 11, etc.)
4. Soyez attentif aux problèmes d'affichage ou de performance

## Problèmes spécifiques à la VCL et leurs solutions

### Adaptation à la haute résolution (DPI)

Les écrans haute résolution sont plus courants aujourd'hui. Les versions récentes de Delphi améliorent la prise en charge du DPI.

```pascal
// Ajoutez dans le fichier .dpr avant l'Application.Initialize
Application.MainFormOnTaskbar := True;
Application.ScaleForCurrentDpi := True;
```

Pour vos formulaires existants, vérifiez :

- La propriété `Scaled`
- L'événement `OnAfterMonitorDpiChanged`
- La position et la taille des contrôles

### Thèmes et styles VCL

Delphi 11 et 12 offrent des styles visuels améliorés. Pour les utiliser :

```pascal
// Ajouter dans uses
Vcl.Themes, Vcl.Styles;

// Dans l'initialisation
TStyleManager.TrySetStyle('Windows11 Modern Light');
```

Considérations :
- Certains contrôles personnalisés peuvent nécessiter des ajustements
- Testez avec différents styles pour assurer la compatibilité

### Support Unicode

Si vous migrez depuis une très ancienne version (Delphi 2007 ou antérieure), le passage à Unicode est une considération majeure.

```pascal
// Ancien code (Delphi 2007 ou antérieur)
var
  S: string;  // AnsiString en Delphi 2007

// Nouveau code
var
  S: string;  // UnicodeString en Delphi moderne
```

Solutions pour les problèmes d'Unicode :
- Utilisez les fonctions `AnsiString` et `UnicodeString` explicitement si nécessaire
- Révisez le code qui manipule directement des octets dans les chaînes
- Vérifiez les interactions avec les DLL et API externes

## Tirer parti des nouvelles fonctionnalités

Après une migration réussie, profitez des nouvelles fonctionnalités :

### 1. Améliorations de la VCL

```pascal
// Support des contrôles tactiles
Button1.Touch.InteractiveGestures := [igPressAndTap, igLongTap];
Button1.Touch.OnGesture := GestureHandler;

// TEdgeBrowser (Delphi 10.4 et supérieur)
EdgeBrowser1.Navigate('https://www.example.com');
```

### 2. Classes d'aide système

```pascal
// TPath pour les manipulations de chemin
var
  DocPath: string;
begin
  DocPath := TPath.Combine(TPath.GetDocumentsPath, 'MesDocuments');
end;

// TFile pour les opérations sur les fichiers
if TFile.Exists(FileName) then
  Content := TFile.ReadAllText(FileName, TEncoding.UTF8);
```

### 3. Fonctionnalités de langage modernes

```pascal
// Opérateur de navigation sécurisée (?.)
if Assigned(Customer) and Assigned(Customer.Address) then
  ZipCode := Customer.Address.ZipCode;

// Avec l'opérateur ?. (Delphi 10.4 et supérieur)
ZipCode := Customer?.Address?.ZipCode;

// Inférence de type avec var (Delphi 10.3 et supérieur)
var Counter := 0;
var FilePath := TPath.Combine(TPath.GetDocumentsPath, 'data.txt');
```

## Astuces pour une migration en douceur

### Approche progressive pour les gros projets

Si votre projet est volumineux, envisagez une approche progressive :

1. Migrez d'abord les bibliothèques partagées ou les unités de base
2. Puis les couches intermédiaires
3. Enfin les formulaires et l'interface utilisateur

### Utiliser la directive IFDEF pour la compatibilité multiversion

Si vous devez maintenir la compatibilité avec plusieurs versions de Delphi :

```pascal
{$IFDEF VER330} // Delphi 10.3
  // Code spécifique à Delphi 10.3
{$ELSE}
  {$IFDEF VER340} // Delphi 10.4
    // Code spécifique à Delphi 10.4
  {$ELSE}
    // Code pour autres versions
  {$ENDIF}
{$ENDIF}
```

Pour une meilleure lisibilité, définissez vos propres constantes :

```pascal
{$IFDEF VER350} // Delphi 11 Alexandria
  {$DEFINE DELPHI11_UP}
{$ENDIF}

{$IFDEF VER360} // Delphi 12 Athens
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
{$ENDIF}

{$IFDEF DELPHI11_UP}
  // Code pour Delphi 11 et supérieur
{$ENDIF}
```

### Documentation des changements

Tenez un journal des modifications effectuées pendant la migration :

```
Changements effectués pendant la migration vers Delphi 12 Athens :

1. UnitMain.pas - Ligne 125 :
   - Ancien code : GetTempPath(MAX_PATH, Buffer);
   - Nouveau code : TempPath := TPath.GetTempPath;
   - Raison : Fonction obsolète

2. FormCustomers.pas - Ligne 236-240 :
   - Ajout de la prise en charge du DPI
   - Raison : Support écrans haute résolution
```

Ce journal sera précieux pour :
- Identifier la source des problèmes qui pourraient apparaître
- Documenter les décisions techniques
- Former d'autres développeurs

## Problèmes courants et solutions

### Erreur "Cannot find unit X"

**Cause** : L'unité n'est pas dans le chemin de recherche ou le nom a changé.

**Solution** :
1. Vérifiez le chemin dans les options du projet (Library Path)
2. Vérifiez si le nom de l'unité a changé avec un préfixe
3. Vérifiez si le composant est installé dans la nouvelle version

### Erreur "Method X not found"

**Cause** : La méthode a été renommée ou déplacée dans les nouvelles versions.

**Solution** :
1. Consultez la documentation pour voir l'équivalent moderne
2. Utilisez l'autocomplétion de l'IDE pour explorer les méthodes disponibles
3. Recherchez dans le code source de la RTL si accessible

### Problèmes de thèmes visuels

**Cause** : Les composants personnalisés ou anciens ne prennent pas en charge les styles VCL.

**Solution** :
1. Mettez à jour la méthode Paint des composants pour utiliser `StyleServices`
2. Ajoutez `TStyleManager.IsCustomStyleActive` pour adapter le comportement
3. Dans certains cas, désactivez les styles pour des contrôles spécifiques

```pascal
TStyleManager.Engine.RegisterStyleHook(TMaClasseSpeciale, TStyleHook);
// ou pour désactiver le style sur un contrôle
TCustomStyleEngine.UnRegisterStyleHook(TMaClasseSpeciale, TStyleHook);
```

## Conclusion

La migration vers une version plus récente de Delphi représente un investissement important, mais les avantages en valent la peine. Vous bénéficierez de fonctionnalités modernes, d'une meilleure compatibilité avec les systèmes actuels et d'améliorations de performances.

Clés du succès :
- Prenez votre temps et procédez méthodiquement
- Testez abondamment à chaque étape
- Documentez vos changements
- N'hésitez pas à consulter la documentation et les forums communautaires

Avec ces conseils, vous devriez pouvoir migrer vos projets VCL vers Delphi 12 Athens ou Delphi 11 Alexandria avec un minimum de difficultés.

---

*Exercice pratique : Prenez un petit projet développé avec une ancienne version de Delphi (Delphi 7, Delphi 2007 ou même Delphi XE) et suivez les étapes décrites dans cette section pour le migrer vers votre version actuelle de Delphi. Notez les problèmes rencontrés et comment vous les avez résolus.*
