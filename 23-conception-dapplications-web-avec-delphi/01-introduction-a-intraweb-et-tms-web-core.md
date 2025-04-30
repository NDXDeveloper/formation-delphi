# 23.1 Introduction à Intraweb et TMS Web Core

## Développement web avec Delphi : les fondamentaux

Vous connaissez déjà Delphi pour le développement d'applications desktop avec VCL ou multi-plateformes avec FireMonkey (FMX), mais saviez-vous que Delphi permet également de créer des applications web complètes ? Dans cette section, nous allons découvrir deux solutions majeures pour le développement web avec Delphi : IntraWeb et TMS Web Core.

## Qu'est-ce qu'IntraWeb ?

IntraWeb est une technologie intégrée à Delphi qui permet de développer des applications web en utilisant une approche similaire à celle du développement d'applications desktop. C'est une solution côté serveur où le code s'exécute sur un serveur web.

### Caractéristiques principales d'IntraWeb :

- **Développement visuel** : Vous créez votre interface utilisateur en plaçant des composants sur une fiche, comme pour une application VCL
- **État de session** : IntraWeb gère automatiquement l'état de la session utilisateur
- **Composants web** : Utilise des contrôles spécifiques adaptés au web
- **Événements côté serveur** : Le code des événements s'exécute sur le serveur

### Avantages d'IntraWeb :

- Courbe d'apprentissage réduite pour les développeurs Delphi
- Pas besoin d'apprendre HTML, CSS et JavaScript en profondeur
- Développement rapide d'applications web d'entreprise
- Intégration facile avec les données

### Exemple simple d'application IntraWeb :

```delphi
// Création d'un bouton avec gestion d'événement
procedure TForm1.IWButton1Click(Sender: TObject);
begin
  IWLabel1.Caption := 'Bonjour depuis IntraWeb !';
end;
```

## Qu'est-ce que TMS Web Core ?

TMS Web Core est une approche différente du développement web avec Delphi. Contrairement à IntraWeb qui s'exécute côté serveur, TMS Web Core compile votre code Pascal en JavaScript qui s'exécute directement dans le navigateur web du client.

### Caractéristiques principales de TMS Web Core :

- **Code Pascal côté client** : Votre code Object Pascal est transpilé en JavaScript
- **Applications SPA** : Crée des Single Page Applications modernes
- **Compatibilité VCL** : Utilise des composants visuels similaires à la VCL
- **Frameworks web** : S'intègre avec Bootstrap, Material UI et autres frameworks web modernes

### Avantages de TMS Web Core :

- Applications web réactives sans rechargement de page
- Utilisation des compétences Delphi existantes pour le web
- Réduction du trafic serveur (exécution côté client)
- Possibilité de créer des PWA (Progressive Web Apps)

### Exemple simple d'application TMS Web Core :

```delphi
// Création d'un bouton avec gestion d'événement
procedure TForm1.WebButton1Click(Sender: TObject);
begin
  WebLabel1.Caption := 'Bonjour depuis TMS Web Core !';
end;
```

## Comparaison entre IntraWeb et TMS Web Core

| Caractéristique | IntraWeb | TMS Web Core |
|----------------|----------|--------------|
| Exécution | Côté serveur | Côté client (navigateur) |
| Type d'application | Applications web traditionnelles | SPA (Single Page Applications) |
| Connaissance web requise | Minimale | Moyenne (utile de comprendre HTML/CSS) |
| Composants | Composants IntraWeb spécifiques | Composants TMS FNC et Web |
| Gestion d'état | Automatique par le serveur | Manuelle ou avec frameworks |
| Déploiement | Serveur web requis | Fichiers statiques possibles |
| Performance | Dépend du serveur | Dépend du navigateur client |
| Licence | Commerciale | Commerciale |

## Démarrer avec IntraWeb

### Création d'un projet IntraWeb :

1. Dans Delphi, sélectionnez **Fichier** > **Nouveau** > **Autres** > **IntraWeb** > **Application**
2. Choisissez le type d'application IntraWeb (standalone ou ISAPI)
3. Une fiche web vide apparaît, similaire à une fiche VCL
4. Placez des composants depuis la palette (section **IntraWeb**)
5. Double-cliquez sur les composants pour créer des gestionnaires d'événements

### Exemple de formulaire simple avec IntraWeb :

```delphi
procedure TIWForm1.IWButton1Click(Sender: TObject);
var
  Nom: string;
begin
  Nom := IWEdit1.Text;
  if Nom <> '' then
    IWLabel1.Caption := 'Bonjour, ' + Nom + ' !';
  else
    IWLabel1.Caption := 'Veuillez entrer votre nom.';
end;
```

## Démarrer avec TMS Web Core

### Installation de TMS Web Core :

1. Téléchargez et installez TMS Web Core depuis le site TMS Software
2. Redémarrez Delphi pour finaliser l'installation

### Création d'un projet TMS Web Core :

1. Dans Delphi, sélectionnez **Fichier** > **Nouveau** > **TMS Web** > **TMS Web Application**
2. Choisissez un modèle de projet (Standard, Bootstrap, etc.)
3. Une fiche web vide apparaît dans l'IDE
4. Placez des composants depuis la palette (section **TMS Web Core**)
5. Double-cliquez sur les composants pour créer des gestionnaires d'événements

### Exemple de formulaire simple avec TMS Web Core :

```delphi
procedure TForm1.WebButton1Click(Sender: TObject);
var
  Nom: string;
begin
  Nom := WebEdit1.Text;
  if Nom <> '' then
    WebLabel1.Caption := 'Bonjour, ' + Nom + ' !';
  else
    WebLabel1.Caption := 'Veuillez entrer votre nom.';
end;
```

## Considérations pour choisir entre IntraWeb et TMS Web Core

### Choisissez IntraWeb si :

- Vous préférez le modèle de développement côté serveur traditionnel
- Vous avez besoin d'une gestion d'état de session automatique
- Votre application nécessite un traitement intensif côté serveur
- Vous souhaitez une transition plus simple depuis le développement VCL

### Choisissez TMS Web Core si :

- Vous voulez créer des applications web modernes et réactives
- Vous préférez l'exécution côté client pour réduire la charge serveur
- Vous souhaitez développer des PWA (Progressive Web Apps)
- Vous voulez intégrer des frameworks web modernes

## Mini-projet : Créer une calculatrice web simple

Pour illustrer les concepts, nous allons créer une calculatrice simple dans les deux technologies.

### Avec IntraWeb :

```delphi
procedure TIWForm1.IWButton1Click(Sender: TObject); // Bouton "+"
var
  Num1, Num2, Resultat: Double;
begin
  Num1 := StrToFloatDef(IWEdit1.Text, 0);
  Num2 := StrToFloatDef(IWEdit2.Text, 0);
  Resultat := Num1 + Num2;
  IWLabel1.Caption := 'Résultat : ' + FloatToStr(Resultat);
end;
```

### Avec TMS Web Core :

```delphi
procedure TForm1.WebButton1Click(Sender: TObject); // Bouton "+"
var
  Num1, Num2, Resultat: Double;
begin
  Num1 := StrToFloatDef(WebEdit1.Text, 0);
  Num2 := StrToFloatDef(WebEdit2.Text, 0);
  Resultat := Num1 + Num2;
  WebLabel1.Caption := 'Résultat : ' + FloatToStr(Resultat);
end;
```

## Conclusion

IntraWeb et TMS Web Core sont deux solutions puissantes qui permettent aux développeurs Delphi de créer des applications web en utilisant leurs compétences Object Pascal existantes. Chacune a ses forces et ses cas d'utilisation :

- **IntraWeb** offre une approche traditionnelle côté serveur, idéale pour les applications d'entreprise avec traitement intensif.
- **TMS Web Core** propose une approche moderne avec exécution côté client, parfaite pour les applications web réactives et les PWA.

Dans les prochaines sections, nous approfondirons chacune de ces technologies et explorerons leurs fonctionnalités avancées.

## Ressources complémentaires

- Documentation officielle d'IntraWeb : [https://www.atozed.com/intraweb/docs/](https://www.atozed.com/intraweb/docs/)
- Documentation TMS Web Core : [https://www.tmssoftware.com/site/tmswebcore.asp](https://www.tmssoftware.com/site/tmswebcore.asp)
- Forums communautaires Delphi sur le développement web

## Exercices pratiques

1. Créez une page de connexion simple avec IntraWeb ou TMS Web Core
2. Ajoutez une validation des champs (nom d'utilisateur non vide, mot de passe d'au moins 6 caractères)
3. Affichez un message de bienvenue personnalisé après la connexion
