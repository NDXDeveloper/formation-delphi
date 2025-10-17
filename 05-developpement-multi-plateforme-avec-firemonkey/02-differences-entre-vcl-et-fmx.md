🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.2 Différences entre VCL et FMX

## Introduction

Maintenant que vous avez découvert FireMonkey, il est essentiel de bien comprendre les différences entre VCL (Visual Component Library) et FMX (FireMonkey). Si vous avez déjà développé avec la VCL, certaines habitudes devront être ajustées. Si vous débutez directement avec FMX, ces explications vous aideront à comprendre les choix de conception de Delphi.

Cette section n'a pas pour but de dire qu'un framework est meilleur que l'autre, mais plutôt de vous aider à choisir le bon outil selon vos besoins et à comprendre comment travailler avec chacun.

## 1. Philosophie et objectif principal

### VCL (Visual Component Library)

**Philosophie** : Encapsuler les contrôles natifs de Windows

La VCL a été conçue dès l'origine de Delphi (1995) pour faciliter le développement d'applications Windows. Elle fonctionne comme une "surcouche élégante" au-dessus de l'API Windows :

- Chaque composant VCL (TButton, TEdit, TLabel) correspond à un contrôle Windows natif
- L'apparence est gérée par Windows lui-même
- Le comportement suit les standards de Windows

**Métaphore simple** : La VCL est comme un traducteur qui vous permet de parler à Windows dans un langage plus simple (Object Pascal) plutôt que d'utiliser directement l'API Windows complexe.

**Avantage** :
- Les applications VCL ont automatiquement le look and feel de Windows
- Elles s'intègrent parfaitement au système
- Les utilisateurs Windows reconnaissent immédiatement les contrôles

**Limitation** :
- Fonctionne uniquement sur Windows
- Dépend de l'API Windows

### FMX (FireMonkey)

**Philosophie** : Créer ses propres contrôles indépendants du système

FireMonkey adopte une approche radicalement différente :

- Les composants FMX sont dessinés par Delphi lui-même, pas par le système d'exploitation
- FireMonkey utilise la carte graphique (GPU) pour rendre l'interface
- L'apparence est définie par des styles, pas par le système

**Métaphore simple** : FireMonkey est comme un peintre qui dessine lui-même chaque bouton, chaque champ de texte, en utilisant ses propres pinceaux (le GPU), au lieu de demander au système de les dessiner.

**Avantage** :
- Fonctionne sur Windows, macOS, iOS, Android, Linux
- Apparence cohérente sur toutes les plateformes
- Personnalisation complète de l'interface
- Effets visuels avancés

**Limitation** :
- L'apparence peut ne pas être 100% native sur chaque plateforme
- Taille des exécutables généralement plus importante

## 2. Architecture et rendu graphique

### Comment VCL dessine l'interface

```
Application VCL → API Windows GDI/GDI+ → Contrôles natifs Windows → Écran
```

**Processus** :
1. Vous placez un TButton VCL sur votre formulaire
2. Delphi demande à Windows de créer un bouton natif
3. Windows gère l'affichage, le comportement, et les interactions
4. Votre code Delphi réagit aux événements (OnClick, etc.)

**Résultat** : Si vous regardez une application VCL avec un outil système, vous verrez de vrais contrôles Windows (HWND), comme n'importe quelle application Windows classique.

### Comment FMX dessine l'interface

```
Application FMX → Moteur de rendu FMX → GPU (DirectX/OpenGL/Metal) → Écran
```

**Processus** :
1. Vous placez un TButton FMX sur votre formulaire
2. FireMonkey calcule comment dessiner ce bouton (forme, couleur, texte, ombre, etc.)
3. Le GPU exécute les instructions de dessin
4. Le résultat est affiché à l'écran
5. FireMonkey gère les interactions utilisateur

**Résultat** : Une application FMX ne contient pas de contrôles système natifs. Tout est dessiné par FireMonkey, comme dans un jeu vidéo.

### Implications concrètes

**Pour VCL** :
- Les composants suivent automatiquement le thème Windows
- Si l'utilisateur change le thème Windows, l'application suit
- Moins de contrôle sur l'apparence détaillée

**Pour FMX** :
- Vous contrôlez totalement l'apparence
- Le changement de thème Windows n'affecte pas l'application (sauf si vous le décidez)
- Plus de flexibilité créative

## 3. Les composants : similitudes et différences

### Nomenclature similaire mais implémentation différente

La bonne nouvelle : beaucoup de composants portent le même nom dans VCL et FMX.

**Exemples** :
- VCL : TButton → FMX : TButton
- VCL : TEdit → FMX : TEdit
- VCL : TLabel → FMX : TLabel
- VCL : TPanel → FMX : TPanel

**Attention** : Même s'ils portent le même nom, leur fonctionnement interne est totalement différent !

### Propriétés communes

Heureusement, de nombreuses propriétés sont similaires :

```pascal
// VCL
Button1.Caption := 'Cliquez-moi';
Button1.Enabled := True;
Button1.Visible := True;

// FMX
Button1.Text := 'Cliquez-moi';  // Notez : Text au lieu de Caption
Button1.Enabled := True;        // Identique
Button1.Visible := True;        // Identique
```

### Différences de propriétés importantes

**VCL utilise** :
- `Caption` pour le texte des boutons et labels
- `Left`, `Top`, `Width`, `Height` pour le positionnement
- `Color` pour la couleur de fond
- `Font` pour la police

**FMX utilise** :
- `Text` pour le texte (plus cohérent sémantiquement)
- `Position.X`, `Position.Y`, `Width`, `Height` pour le positionnement
- `Fill.Color` pour la couleur de remplissage
- `TextSettings` pour les paramètres de texte (plus complet que Font)
- `Stroke` pour les bordures

### Composants spécifiques à chaque framework

**Uniquement en VCL** :
- TRichEdit (édition de texte riche façon Windows)
- TStatusBar (barre d'état native Windows)
- Composants Windows spécifiques (TPageSetupDialog, etc.)

**Uniquement en FMX** :
- TRectangle (forme rectangulaire dessinable)
- TCircle (forme circulaire)
- TPath (formes vectorielles personnalisées)
- Composants avec effets visuels intégrés
- Composants tactiles (TGestureManager)

**En FMX, vous avez accès à des formes géométriques** comme composants de base, ce qui n'existe pas en VCL. Cela ouvre de nouvelles possibilités créatives.

## 4. Système de positionnement et de mise en page

### VCL : positionnement absolu principalement

En VCL, le positionnement classique est absolu :

```pascal
Button1.Left := 10;
Button1.Top := 20;
Button1.Width := 100;
Button1.Height := 25;
```

Vous pouvez utiliser des ancrages (Anchors) et des alignements (Align) pour rendre l'interface adaptative, mais l'approche par défaut est positionnelle.

**Conséquence** : Les interfaces VCL sont souvent conçues pour une résolution d'écran spécifique.

### FMX : approche layout et positionnement relatif

FireMonkey encourage une approche plus moderne avec des layouts :

**Composants de layout** :
- `TLayout` : Conteneur invisible pour organiser d'autres composants
- `TFlowLayout` : Disposition en flux (comme du texte)
- `TGridLayout` : Disposition en grille
- `TScrollBox` : Zone défilante

**Propriétés de positionnement avancées** :
- `Align` : AlignTop, AlignBottom, AlignClient, AlignLeft, AlignRight, etc.
- `Anchors` : Ancrer les bords par rapport au parent
- `Margins` : Marges externes
- `Padding` : Marges internes

**Exemple conceptuel** :
```pascal
// Créer une barre d'outils en haut qui s'étend sur toute la largeur
Toolbar.Align := TAlignLayout.Top;
Toolbar.Height := 50;

// Zone de contenu qui occupe tout l'espace restant
ContentArea.Align := TAlignLayout.Client;
```

**Avantage** : L'interface s'adapte automatiquement à différentes tailles d'écran.

## 5. Gestion des événements

### Similarités

Les événements principaux fonctionnent de manière similaire :

**VCL** :
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué!');
end;
```

**FMX** :
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué!');
end;
```

### Différences pour les événements tactiles

FMX ajoute des événements spécifiques pour le tactile :

```pascal
// Événements tactiles en FMX
procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);
procedure TForm1.Button1Swipe(Sender: TObject; const Direction: TSwipeDirection);
procedure TForm1.Button1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo);
```

Ces événements n'existent pas en VCL car ils sont spécifiques aux interfaces tactiles.

### Événements souris vs tactile

**VCL** : Principalement orienté souris
- OnMouseDown, OnMouseUp, OnMouseMove

**FMX** : Souris et tactile
- OnMouseDown, OnMouseUp (compatibilité souris)
- OnTap, OnSwipe, OnGesture (interactions tactiles)

Sur desktop, les clics de souris déclenchent aussi les événements tactiles en FMX, assurant une compatibilité complète.

## 6. Styles et apparence

### VCL : thèmes Windows

En VCL, l'apparence est principalement gérée par :

**1. Le système Windows** :
- Les contrôles suivent le thème Windows actif
- Vous avez peu de contrôle sur l'apparence détaillée

**2. VCL Styles** (depuis Delphi XE2) :
- Permet d'appliquer des thèmes personnalisés
- Modifie l'apparence sans changer le code
- Plus limité que FMX en termes de personnalisation

### FMX : système de styles complet

FireMonkey utilise un système de styles très puissant :

**Styles intégrés** :
- Styles par plateforme (Windows, macOS, iOS, Android)
- Votre application peut ressembler au style natif de chaque système

**Styles personnalisés** :
- Créez vos propres thèmes de A à Z
- Contrôle total sur chaque pixel
- Un seul style peut être appliqué à toute l'application

**StyleBook** :
- Composant TStyleBook pour charger un style
- Changement dynamique de style en cours d'exécution

**Exemple** :
```pascal
// Charger un style iOS sur Windows pour tester
StyleBook1.LoadFromFile('iOS_Style.style');
Form1.StyleBook := StyleBook1;
```

### Comparaison visuelle

**VCL** :
- Look Windows classique ou thèmes VCL prédéfinis
- Boutons, champs, listes ressemblent à Windows
- Cohérent avec le système, mais moins moderne

**FMX** :
- Look moderne et personnalisable
- Peut ressembler à iOS, Android, ou être complètement unique
- Effets visuels (ombres, transparence, animations)

## 7. Effets visuels et animations

### VCL : limité

En VCL, les effets visuels sont limités :
- Pas d'effets intégrés (ombres, flous, reflets)
- Animations complexes nécessitent du code personnalisé
- Graphismes avancés via GDI+ mais peu pratique

### FMX : riche en effets

FireMonkey offre de nombreux effets prêts à l'emploi :

**Effets disponibles** :
- `TShadowEffect` : Ombres portées
- `TGlowEffect` : Effet de lueur
- `TBlurEffect` : Flou
- `TReflectionEffect` : Reflets
- `TBevelEffect` : Effet de relief
- Et bien d'autres...

**Animations** :
- `TFloatAnimation` : Animer des valeurs numériques
- `TColorAnimation` : Animer des couleurs
- `TBitmapAnimation` : Animer des images
- Système d'interpolation sophistiqué

**Exemple conceptuel** :
```pascal
// Ajouter une ombre à un bouton (design time ou code)
var Shadow: TShadowEffect;
Shadow := TShadowEffect.Create(Button1);
Shadow.Parent := Button1;
Shadow.Distance := 3;
Shadow.Softness := 0.3;
```

## 8. Performance et utilisation des ressources

### VCL : léger et rapide sur Windows

**Points forts** :
- Exécutables relativement petits
- Démarrage rapide
- Faible consommation mémoire
- Pas de dépendance GPU

**Utilisation** : Idéal pour des applications de gestion classiques sur Windows

### FMX : plus gourmand mais plus riche visuellement

**Points forts** :
- Rendu GPU accéléré
- Animations fluides
- Graphismes modernes

**Points à considérer** :
- Exécutables plus volumineux (inclut le moteur de rendu)
- Utilisation GPU (peut consommer plus de batterie sur mobile)
- Légèrement plus lent au démarrage

**Important** : Sur des ordinateurs modernes, ces différences sont généralement imperceptibles pour l'utilisateur. Sur mobile, l'optimisation devient plus importante.

## 9. Déploiement et compilation

### VCL : Windows uniquement

**Cibles de compilation** :
- Windows 32 bits
- Windows 64 bits

**Résultat** :
- Un fichier .exe pour Windows
- Déploiement simple (copier l'exe et éventuelles DLL)

### FMX : multi-plateforme

**Cibles de compilation** :
- Windows 32 bits et 64 bits
- macOS (Intel et Apple Silicon)
- iOS (iPhone et iPad)
- Android (ARM et x86)
- Linux 64 bits

**Résultat** :
- Un package d'application pour chaque plateforme
- Processus de déploiement spécifique à chaque système
- Nécessite parfois des outils supplémentaires (SDK Android, Xcode pour iOS)

## 10. Accès aux fonctionnalités système

### VCL : API Windows directement accessible

En VCL, vous pouvez facilement appeler l'API Windows :

```pascal
uses WinAPI.Windows;

// Appel direct à l'API Windows
MessageBeep(MB_ICONEXCLAMATION);
```

**Avantage** : Accès complet à toutes les fonctionnalités Windows

### FMX : abstraction multi-plateforme

FireMonkey fournit des abstractions pour les fonctionnalités communes :

```pascal
// Services FMX multi-plateformes
TDialogService.ShowMessage('Message');
```

**Pour des fonctionnalités spécifiques** :
- Utilisation de directives de compilation conditionnelles
- Accès aux API natives via des classes spécialisées

```pascal
{$IFDEF MSWINDOWS}
  // Code spécifique Windows
{$ENDIF}

{$IFDEF ANDROID}
  // Code spécifique Android
{$ENDIF}
```

## 11. Composants liés aux données

### Similarités

Les deux frameworks utilisent FireDAC pour l'accès aux bases de données, donc cette partie est identique :
- Même composants de connexion
- Mêmes datasets
- Même logique SQL

### Différences dans les contrôles visuels

**VCL** :
- DBGrid (grille de données mature et riche)
- DBEdit, DBMemo, DBComboBox (contrôles liés)
- DBNavigator (navigation dans les données)

**FMX** :
- TGrid avec LiveBindings (approche différente)
- Composants DB (similaires mais propriétés différentes)
- LiveBindings Designer (outil visuel de liaison)

Le système LiveBindings de FMX est plus moderne mais nécessite un apprentissage différent de l'approche VCL classique.

## 12. Développement et productivité

### VCL : mature et documentée

**Avantages** :
- Documentation abondante (20+ ans d'existence)
- Énorme quantité d'exemples en ligne
- Composants tiers nombreux et matures
- Communauté vaste

**Productivité** :
- Développement très rapide pour applications Windows
- Peu de surprises, tout est bien rodé

### FMX : moderne mais moins de ressources

**Avantages** :
- Technologie moderne
- Approche contemporaine du développement
- Capacités multi-plateformes

**Défis** :
- Moins d'exemples que VCL
- Composants tiers moins nombreux
- Documentation parfois moins détaillée
- Courbe d'apprentissage si on vient de VCL

**Productivité** :
- Plus lent au début (apprentissage)
- Très productif une fois maîtrisé
- Gain énorme pour le multi-plateforme

## 13. Tableau comparatif récapitulatif

| Critère | VCL | FMX |
|---------|-----|-----|
| **Plateformes** | Windows uniquement | Windows, macOS, iOS, Android, Linux |
| **Rendu graphique** | API Windows (GDI/GDI+) | GPU (DirectX/OpenGL/Metal) |
| **Apparence** | Native Windows | Personnalisable, styles |
| **Effets visuels** | Limités | Nombreux et intégrés |
| **Animations** | Manuelles | Système intégré |
| **Taille exécutable** | Petit | Plus important |
| **Performance** | Excellente sur Windows | Très bonne, dépend du GPU |
| **Tactile** | Non natif | Support complet |
| **Composants tiers** | Très nombreux | En croissance |
| **Documentation** | Abondante | Suffisante mais moins complète |
| **Courbe d'apprentissage** | Facile (si Windows) | Moyenne |
| **Maintenance** | Stable, mature | Évolutif, améliorations continues |

## 14. Quand choisir VCL ou FMX ?

### Choisissez VCL si :

✅ Votre application est **exclusivement pour Windows**

✅ Vous avez besoin d'une **intégration Windows parfaite**

✅ Vous utilisez des **composants VCL tiers spécifiques**

✅ La **taille de l'exécutable** est critique

✅ Vous développez des applications de **gestion classiques**

✅ Vous avez une **expertise VCL** et pas de besoin multi-plateforme

### Choisissez FMX si :

✅ Vous visez **plusieurs plateformes** (desktop + mobile)

✅ Vous développez pour **iOS ou Android**

✅ Vous voulez des **interfaces modernes** avec effets visuels

✅ Votre application nécessite une **interface tactile**

✅ Vous voulez une **apparence cohérente** sur toutes les plateformes

✅ Les **animations et transitions** sont importantes

✅ Vous démarrez un **nouveau projet** sans héritage VCL

### Peut-on combiner les deux ?

Oui, dans une certaine mesure :

**Partage de code** :
- Logique métier commune (classes non visuelles)
- Accès aux données (FireDAC)
- Utilitaires et bibliothèques

**Projets séparés** :
- Interface VCL pour Windows
- Interface FMX pour mobile/multi-plateforme
- Code métier partagé via unités communes

Cette approche est utilisée par certaines entreprises qui veulent le meilleur des deux mondes.

## 15. Migration VCL vers FMX

### Ce qui se transfère facilement

**Code métier** :
- Algorithmes et calculs
- Classes non visuelles
- Gestion de données
- Logique de validation

**Concepts** :
- Événements
- Propriétés
- Orienté objet
- Structure des projets

### Ce qui doit être refait

**Interface utilisateur** :
- Tous les formulaires
- Placement des composants
- Code d'interaction UI
- Gestionnaires d'événements visuels

### Stratégie de migration

1. **Séparer la logique métier de l'UI** dans le projet VCL
2. **Créer un nouveau projet FMX**
3. **Réutiliser les unités de logique métier**
4. **Reconstruire l'interface** en FMX
5. **Reconnecter** l'UI aux classes métier
6. **Tester sur les plateformes cibles**

**Important** : Ne vous attendez pas à une migration automatique. C'est essentiellement une reconstruction de l'interface, mais avec la logique réutilisée.

## Conclusion

VCL et FMX sont deux frameworks excellents, chacun avec ses forces :

**VCL** reste le choix optimal pour les applications Windows pures, offrant performance, maturité, et intégration parfaite à Windows.

**FMX** est le framework du futur multi-plateforme, permettant de toucher un public beaucoup plus large avec un seul code source, tout en offrant des capacités graphiques modernes.

Votre choix dépendra de :
- Vos plateformes cibles
- Vos besoins visuels
- Votre expertise existante
- Les contraintes du projet

La bonne nouvelle est que Delphi vous permet d'utiliser les deux, selon les besoins spécifiques de chaque projet. Vous n'êtes pas obligé de choisir un camp : vous pouvez être expert en VCL ET en FMX.

Dans les sections suivantes, nous approfondirons les aspects pratiques du développement avec FireMonkey, maintenant que vous comprenez ses différences fondamentales avec la VCL.

⏭️ [Création d'interfaces multi-plateformes](/05-developpement-multi-plateforme-avec-firemonkey/03-creation-dinterfaces-multi-plateformes.md)
