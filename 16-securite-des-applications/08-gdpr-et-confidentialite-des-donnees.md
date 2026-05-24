🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.8 GDPR et confidentialité des données

## Introduction

Le RGPD (Règlement Général sur la Protection des Données), ou GDPR en anglais (General Data Protection Regulation), est une réglementation européenne entrée en vigueur le 25 mai 2018. Elle définit comment les données personnelles doivent être collectées, traitées, stockées et protégées.

**Analogie du monde réel** : Le RGPD, c'est comme un contrat de respect entre vous et vos utilisateurs. Vous vous engagez à traiter leurs données avec le plus grand soin, comme vous aimeriez que vos propres données soient traitées.

### Pourquoi le RGPD est important ?

**Pour les utilisateurs** :
- Protection de leur vie privée
- Contrôle sur leurs données
- Transparence sur l'utilisation
- Droit de rectifier ou supprimer

**Pour votre application** :
- Conformité légale obligatoire
- Confiance des utilisateurs renforcée
- Amendes évitées. **Deux niveaux** (article 83) :
  - Jusqu'à **10 millions € ou 2 % du CA mondial** (le plus élevé) pour les manquements moins graves (registre des traitements, AIPD, notification de violation…).
  - Jusqu'à **20 millions € ou 4 % du CA mondial** pour les manquements les plus graves (consentement, droits des personnes, transferts internationaux…).
- Bonne réputation

### À qui s'applique le RGPD ?

Le RGPD s'applique si :
- Votre entreprise est établie dans l'UE (article 3§1) — peu importe que le traitement ait lieu dans ou hors UE ;
- Ou votre entreprise est hors UE mais traite les données de **personnes situées dans l'UE** (article 3§2) :
  - en leur offrant des biens ou des services (gratuits ou payants),
  - ou en surveillant leur comportement dans l'UE.

> ⚠ **Erreur fréquente** : « citoyens européens ». Le critère du RGPD n'est PAS la nationalité, c'est la **localisation au moment du traitement**. Un touriste américain à Paris qui utilise votre app a les mêmes droits qu'un résident français. Un Français installé à New York n'est pas couvert par le RGPD (mais peut l'être par le CCPA californien, ou la LGPD brésilienne, etc.).

**Même si votre serveur est aux USA**, si vous avez des utilisateurs en Europe, vous devez respecter le RGPD.

## Qu'est-ce qu'une donnée personnelle ?

Une donnée personnelle est toute information se rapportant à une personne physique identifiée ou identifiable.

### Données directement identifiantes

```
Nom, Prénom  
Adresse email  
Numéro de téléphone  
Adresse postale  
Numéro de sécurité sociale  
Plaque d'immatriculation  
Photo ou vidéo du visage  
```

### Données indirectement identifiantes

```
Adresse IP  
Cookie identifiant  
Numéro de client  
Données de géolocalisation  
Données biométriques  
```

### Données sensibles (protection renforcée)

```
Origine ethnique ou raciale  
Opinions politiques  
Convictions religieuses  
Appartenance syndicale  
Données génétiques ou biométriques  
Données de santé  
Orientation sexuelle  
Casier judiciaire  
```

### Pseudonymisation vs anonymisation : ne pas confondre

| | Pseudonymisation | Anonymisation |
|---|---|---|
| **Définition** | Remplacer l'identifiant par un pseudonyme, mais conserver une table de correspondance | Suppression *irréversible* de tout lien avec l'identité |
| **Réversibilité** | Réversible (avec la clé) | Irréversible |
| **Statut RGPD** | Reste une donnée personnelle | N'est plus une donnée personnelle |
| **Exemple** | `user_id = 'a3f5b...'` + table `(a3f5b... → jean@dupont.fr)` | Agrégat statistique « 42 hommes 30-40 ans ont acheté X » |

Le RGPD **encourage** la pseudonymisation comme mesure de sécurité (article 32), mais l'**anonymisation complète** est plus rare en pratique qu'on ne le croit : il suffit souvent de croiser quelques attributs (code postal + date de naissance + sexe) pour ré-identifier une personne avec haute probabilité. La CNIL et le G29/EDPB ont publié des guides sur les critères d'anonymisation valable (test de singularisation, de corrélation, d'inférence).

### Privacy by Design et by Default (article 25)

Deux obligations distinctes du responsable de traitement :

- **Privacy by Design** : intégrer la protection des données *dès la conception* de l'application — chiffrement, minimisation, contrôle d'accès. Pas comme une couche ajoutée après coup.
- **Privacy by Default** : les paramètres par défaut doivent être les plus protecteurs (ex : profil privé par défaut, notifications opt-in, partage désactivé).

### Responsable de traitement vs Sous-traitant

Le RGPD distingue deux rôles aux obligations très différentes :

- **Responsable de traitement** (*controller*) : décide des finalités et moyens du traitement. C'est lui qui répond devant la CNIL.
- **Sous-traitant** (*processor*) : traite les données *pour le compte du* responsable. Doit être lié par un contrat (article 28) listant ses obligations.

Si vous éditez un logiciel SaaS qui traite les données de vos clients, vous êtes **sous-traitant** de vos clients. Si vous éditez un logiciel installé chez le client, vous n'êtes ni l'un ni l'autre (vous fournissez l'outil, le client est seul responsable). Cette distinction conditionne quelles obligations s'appliquent à vous.

## Les 7 principes du RGPD

### 1. Licéité, loyauté et transparence

**Principe** : Traiter les données de manière légale, équitable et transparente.

```pascal
// ✅ BON - Informer clairement l'utilisateur
procedure AfficherPolitiqueConfidentialite;  
begin  
  ShowMessage('Vos données (nom, email, adresse) seront utilisées uniquement pour :' + sLineBreak +
              '- Créer votre compte' + sLineBreak +
              '- Vous envoyer des notifications importantes' + sLineBreak +
              '- Améliorer nos services' + sLineBreak + sLineBreak +
              'Vous pouvez à tout moment demander leur suppression.');
end;

// ❌ MAUVAIS - Collecte cachée
procedure CollecterDonneesCachees;  
begin  
  // Collecte silencieuse sans informer l'utilisateur
  EnvoyerVersServeur(ListeContacts, HistoriqueNavigation, Localisation);
end;
```

### 2. Limitation des finalités

**Principe** : Collecter les données pour des objectifs précis et ne pas les utiliser à d'autres fins.

```pascal
type
  TFinaliteTraitement = (
    ftCreationCompte,
    ftGestionCommandes,
    ftEnvoiNewsletter,
    ftAnalyseStatistique,
    ftSupport
  );

procedure CollecterDonneesAvecFinalite(AFinalite: TFinaliteTraitement; const AEmail: string);  
begin  
  case AFinalite of
    ftCreationCompte:
      // ✅ Base légale : exécution du contrat (art. 6§1b RGPD)
      CreerCompte(AEmail);

    ftGestionCommandes:
      // ✅ Base légale : exécution du contrat
      AjouterDansHistoriqueCommandes(AEmail);

    ftEnvoiNewsletter:
    begin
      // ⚠ Base légale : consentement (art. 6§1a). Doit être libre,
      //   spécifique, éclairé et univoque (recital 32).
      if not ConsentementNewsletter(AEmail) then
      begin
        ShowMessage('Vous devez consentir à recevoir la newsletter');
        Exit;
      end;
      InscrireNewsletter(AEmail);
    end;

    ftAnalyseStatistique:
      // ⚠ Intérêt légitime (art. 6§1f), à condition que les données
      //   soient pseudonymisées et qu'un test de balance des intérêts
      //   ait été documenté. Sinon, consentement nécessaire.
      EnregistrerStatistiquePseudonymisee(AEmail);

    ftSupport:
      // ✅ Base légale : exécution du contrat / intérêt légitime
      OuvrirTicketSupport(AEmail);
  else
    // ⚠ Toute finalité non listée doit être REFUSÉE explicitement —
    //   éviter qu'une nouvelle valeur d'enum soit silencieusement
    //   traitée sans base légale documentée.
    raise Exception.CreateFmt('Finalité non prévue : %d', [Ord(AFinalite)]);
  end;
end;

// ❌ MAUVAIS - Utilisation détournée
procedure UtilisationDetournee;  
var  
  EmailsClients: TStringList;
begin
  EmailsClients := ChargerEmailsClients;

  // Email collecté pour les commandes
  // mais utilisé pour du marketing non consenti
  EnvoyerPublicite(EmailsClients); // INTERDIT !
end;
```

### 3. Minimisation des données

**Principe** : Ne collecter que les données strictement nécessaires.

```pascal
// ❌ MAUVAIS - Trop de données collectées
type
  TInscriptionComplete = record
    Nom: string;
    Prenom: string;
    Email: string;
    Telephone: string;
    Adresse: string;
    CodePostal: string;
    Ville: string;
    DateNaissance: TDate;
    Profession: string;
    Salaire: Currency;           // Nécessaire ?
    SituationMatrimoniale: string; // Nécessaire ?
    NombreEnfants: Integer;       // Nécessaire ?
  end;

// ✅ BON - Uniquement le nécessaire
type
  TInscriptionMinimale = record
    Email: string;        // Nécessaire pour la connexion
    MotDePasse: string;   // Nécessaire pour la sécurité
  end;

// Si vous avez besoin de plus, demandez au moment voulu
type
  TAdresseLivraison = record
    Nom: string;
    Adresse: string;
    CodePostal: string;
    Ville: string;
  end;

procedure DemanderAdresseAuMomentCommande;  
begin  
  // Demander l'adresse uniquement quand l'utilisateur commande
  if UtilisateurCommandeQuelqueChose then
    AdresseLivraison := SaisirAdresseLivraison;
end;
```

### 4. Exactitude

**Principe** : Les données doivent être exactes et mises à jour.

```pascal
type
  TGestionDonneesUtilisateur = class
  public
    procedure PermettreModification(AIDUtilisateur: Integer);
    procedure VerifierEtNettoyer;
    procedure SupprimerDonneesObsoletes;
  end;

procedure TGestionDonneesUtilisateur.PermettreModification(AIDUtilisateur: Integer);  
begin  
  // Permettre à l'utilisateur de modifier ses données
  FormModificationProfil := TFormModificationProfil.Create(nil);
  try
    FormModificationProfil.ChargerDonnees(AIDUtilisateur);
    if FormModificationProfil.ShowModal = mrOk then
      FormModificationProfil.Enregistrer;
  finally
    FormModificationProfil.Free;
  end;
end;

procedure TGestionDonneesUtilisateur.VerifierEtNettoyer;  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // ⚠ Syntaxe `DELETE u1 FROM ... INNER JOIN ...` propre à MySQL/MariaDB.
    //   Équivalent portable (ANSI SQL) :
    //     DELETE FROM Utilisateurs
    //     WHERE ID NOT IN (SELECT MIN(ID) FROM Utilisateurs GROUP BY Email);
    //   (Attention : sur MySQL, on ne peut pas faire DELETE depuis une
    //    table puis SELECT depuis la même table dans la même requête —
    //    il faut une sous-requête imbriquée `SELECT ... FROM (SELECT...)`.)
    Query.SQL.Text :=
      'DELETE u1 FROM Utilisateurs u1 ' +
      'INNER JOIN Utilisateurs u2 ' +
      'WHERE u1.ID > u2.ID AND u1.Email = u2.Email';
    Query.ExecSQL;

    // Normaliser les formats.
    // ⚠ Si la colonne Email a une contrainte UNIQUE et que deux comptes
    //   existaient avec « Jean@Dupont.FR » et « jean@dupont.fr », le UPDATE
    //   en masse échouera (violation d'unicité). Exécuter D'ABORD la
    //   dédoublication ci-dessus.
    Query.SQL.Text :=
      'UPDATE Utilisateurs SET Email = LOWER(TRIM(Email))';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

### 5. Limitation de la conservation

**Principe** : Conserver les données uniquement le temps nécessaire.

```pascal
type
  TDureeConservation = record
    const
      COMPTES_INACTIFS = 36; // 3 ans en mois
      COMMANDES = 60;        // 5 ans (obligation légale)
      LOGS = 12;             // 1 an
      NEWSLETTER = 36;       // 3 ans
  end;

procedure SupprimerDonneesExpirees;  
var  
  Query: TFDQuery;
begin
  // ⚠ La suppression automatique de comptes inactifs doit être PRÉCÉDÉE :
  //   1. d'un email d'avertissement (typiquement 30 jours avant) ;
  //   2. d'un mécanisme simple permettant à l'utilisateur de réactiver
  //      son compte (un clic dans l'email) ;
  //   3. d'une journalisation pour pouvoir répondre à toute réclamation.
  //   La CNIL a déjà sanctionné des entreprises ayant supprimé des comptes
  //   sans préavis (« durée de conservation » ≠ « suppression brutale »).
  //
  // ⚠ Durées de conservation à adapter à votre métier :
  //   - Comptabilité (France) : 10 ans (Code de commerce)
  //   - Marketing : 3 ans après dernier contact (CNIL)
  //   - Logs sécurité : 6 mois (recommandation CNIL)
  //   - Données médicales : 20-30 ans selon les cas

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Supprimer les comptes inactifs depuis 3 ans
    Query.SQL.Text :=
      'DELETE FROM Utilisateurs ' +
      'WHERE DerniereConnexion < DATE_SUB(NOW(), INTERVAL :Mois MONTH)';
    Query.ParamByName('Mois').AsInteger := TDureeConservation.COMPTES_INACTIFS;
    Query.ExecSQL;

    TLogger.Instance.Info('Comptes inactifs supprimés',
                          IntToStr(Query.RowsAffected) + ' comptes');

    // Anonymiser les anciennes commandes (après 5 ans)
    Query.SQL.Text :=
      'UPDATE Commandes SET ' +
      '  NomClient = ''Anonymisé'', ' +
      '  EmailClient = ''anonyme@example.com'', ' +
      '  AdresseLivraison = ''Anonymisée'' ' +
      'WHERE DateCommande < DATE_SUB(NOW(), INTERVAL :Mois MONTH)';
    Query.ParamByName('Mois').AsInteger := TDureeConservation.COMMANDES;
    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

// Tâche planifiée (à exécuter quotidiennement ou hebdomadairement)
procedure TFormPrincipal.TimerNettoyageTimer(Sender: TObject);  
begin  
  SupprimerDonneesExpirees;
end;
```

### 6. Intégrité et confidentialité

**Principe** : Protéger les données contre la perte, la destruction ou les dommages.

```pascal
// Voir sections précédentes :
// - 16.3 Chiffrement des données
// - 16.4 Sécurisation des connexions
// - 16.5 Protection contre les vulnérabilités

procedure ProtegerDonneesPersonnelles;  
begin  
  // 1. Chiffrement
  ChiffrerDonneesAuRepos;
  UtiliserHTTPS;

  // 2. Contrôle d'accès
  VerifierPermissions;

  // 3. Sauvegardes
  SauvegarderRegulierement;

  // 4. Journalisation
  LoggerAccesDonnees;
end;
```

### 7. Responsabilité (Accountability)

**Principe** : Être capable de démontrer la conformité.

```pascal
type
  TRegistreConformite = class
  public
    procedure DocumenterTraitement;
    procedure GenererRapportConformite;
    procedure ConserverPreuves;
  end;
```

### Analyse d'Impact relative à la Protection des Données (AIPD / DPIA)

**Article 35 du RGPD** : pour les traitements **à risque élevé** pour les droits et libertés des personnes, une AIPD est **obligatoire** avant la mise en œuvre.

Cas où l'AIPD est obligatoire (liste indicative CNIL) :
- Traitements à grande échelle de données sensibles ou de mineurs.
- Surveillance systématique d'une zone accessible au public.
- Profilage avec décision automatisée ayant un effet juridique.
- Croisement de données venant de plusieurs sources avec finalités différentes.
- Données biométriques pour identifier une personne.

Une AIPD documente :
- La description du traitement et ses finalités.
- La nécessité et la proportionnalité.
- Les risques pour les personnes concernées.
- Les mesures pour traiter ces risques.

La CNIL fournit un outil gratuit (PIA) pour réaliser une AIPD.

### Transferts internationaux de données (chapitre 5 du RGPD)

Tout transfert de données personnelles **hors de l'UE/EEE** est soumis à conditions strictes :

| Pays / Cadre | Statut 2026 |
|---|---|
| Pays « adéquats » (UK, Suisse, Japon, Corée du Sud…) | Transfert libre, comme dans l'UE |
| **États-Unis** | **Data Privacy Framework** (juillet 2023) — décision d'adéquation pour les entreprises certifiées DPF. Couvre la plupart des géants tech (Google, Microsoft, Salesforce…). Risque : recours européens en cours (Schrems III ?). |
| Autres pays | Clauses Contractuelles Types (CCT) de la Commission ou règles d'entreprise contraignantes (BCR) |

> ⚠️ **Conséquences pratiques pour une app Delphi** :  
> - Stockage AWS/Azure/GCP : vérifier la région — `eu-west-3` (Paris), `eu-central-1` (Francfort) restent en UE. `us-east-1` (Virginie) = transfert.  
> - Service de mail transactionnel (SendGrid, Mailgun) basé aux US : vérifier qu'ils sont certifiés DPF.  
> - Outil de support tiers (Intercom, Zendesk) : idem.  
> - **Conseil de la CNIL en 2024-2025** : privilégier les hébergeurs européens (souveraineté), surtout pour les données sensibles ou les administrations.

## Les droits des utilisateurs (et comment les implémenter)

### 1. Droit d'accès

L'utilisateur peut demander une copie de toutes ses données.

```pascal
type
  TExportDonneesPersonnelles = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    function ExporterDonneesUtilisateur(AIDUtilisateur: Integer): string; // JSON
    procedure GenererRapportPDF(AIDUtilisateur: Integer; const ANomFichier: string);
  end;

constructor TExportDonneesPersonnelles.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

function TExportDonneesPersonnelles.ExporterDonneesUtilisateur(AIDUtilisateur: Integer): string;  
var  
  Query: TFDQuery;
  JSONRoot: TJSONObject;
  JSONUtilisateur: TJSONObject;
  JSONCommandes: TJSONArray;
begin
  JSONRoot := TJSONObject.Create;
  try
    // Informations utilisateur
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := 'SELECT * FROM Utilisateurs WHERE ID = :ID';
      Query.ParamByName('ID').AsInteger := AIDUtilisateur;
      Query.Open;

      if not Query.IsEmpty then
      begin
        JSONUtilisateur := TJSONObject.Create;
        JSONUtilisateur.AddPair('nom', Query.FieldByName('Nom').AsString);
        JSONUtilisateur.AddPair('email', Query.FieldByName('Email').AsString);
        JSONUtilisateur.AddPair('telephone', Query.FieldByName('Telephone').AsString);
        JSONUtilisateur.AddPair('date_creation',
          DateToStr(Query.FieldByName('DateCreation').AsDateTime));
        JSONRoot.AddPair('utilisateur', JSONUtilisateur);
      end;
      Query.Close;

      // Commandes
      Query.SQL.Text := 'SELECT * FROM Commandes WHERE IDUtilisateur = :ID';
      Query.ParamByName('ID').AsInteger := AIDUtilisateur;
      Query.Open;

      JSONCommandes := TJSONArray.Create;
      while not Query.Eof do
      begin
        // ⚠ Utiliser un format DATE et un format NUMÉRIQUE invariants
        //   pour l'export : un Belge qui exporte et un Américain qui
        //   relit ne doivent pas avoir d'ambiguïté `,` vs `.`. ISO 8601
        //   pour les dates, `.` pour les nombres.
        JSONCommandes.Add(TJSONObject.Create
          .AddPair('numero', Query.FieldByName('Numero').AsString)
          .AddPair('date', FormatDateTime('yyyy-mm-dd',
                                          Query.FieldByName('DateCommande').AsDateTime))
          .AddPair('montant', FormatFloat('0.00',
                                          Query.FieldByName('Montant').AsFloat,
                                          TFormatSettings.Invariant))
        );
        Query.Next;
      end;
      JSONRoot.AddPair('commandes', JSONCommandes);

      // Ajouter d'autres données (préférences, historique, etc.)

    finally
      Query.Free;
    end;

    Result := JSONRoot.ToString;
  finally
    JSONRoot.Free;
  end;
end;

procedure TExportDonneesPersonnelles.GenererRapportPDF(AIDUtilisateur: Integer;
                                                        const ANomFichier: string);
var
  JSON: string;
  Lignes: TStringList;
begin
  JSON := ExporterDonneesUtilisateur(AIDUtilisateur);

  // Convertir en format lisible
  Lignes := TStringList.Create;
  try
    Lignes.Add('RAPPORT DE VOS DONNÉES PERSONNELLES');
    Lignes.Add('===================================');
    Lignes.Add('');
    Lignes.Add('Généré le : ' + DateTimeToStr(Now));
    Lignes.Add('');
    Lignes.Add(JSON); // En production, formatter joliment

    // ⚠ Spécifier UTF-8 explicitement pour préserver les caractères
    //   accentués (é, ç, à...) à travers tous les systèmes — sinon
    //   Delphi utilise l'ANSI Windows par défaut.
    Lignes.SaveToFile(ANomFichier, TEncoding.UTF8);

    // En production : convertir en PDF avec un outil comme FastReport
  finally
    Lignes.Free;
  end;
end;

// Interface utilisateur
procedure TFormProfil.BtnExporterDonneesClick(Sender: TObject);  
var  
  Exportation: TExportDonneesPersonnelles;
  CheminFichier: string;
begin
  Exportation := TExportDonneesPersonnelles.Create(FDConnection1);
  try
    CheminFichier := TPath.Combine(TPath.GetDocumentsPath,
                                    'mes_donnees_' + FormatDateTime('yyyymmdd', Now) + '.json');

    TFile.WriteAllText(CheminFichier,
                       Exportation.ExporterDonneesUtilisateur(UtilisateurConnecteID),
                       TEncoding.UTF8);

    ShowMessage('Vos données ont été exportées vers :' + sLineBreak + CheminFichier);
  finally
    Exportation.Free;
  end;
end;
```

### 2. Droit de rectification

L'utilisateur peut corriger ses données inexactes.

```pascal
procedure TFormMonProfil.BtnModifierClick(Sender: TObject);  
var  
  Query: TFDQuery;
begin
  // Validation
  if not ValiderEmail(EditEmail.Text) then
  begin
    ShowMessage('Email invalide');
    Exit;
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'UPDATE Utilisateurs SET ' +
      '  Nom = :Nom, ' +
      '  Email = :Email, ' +
      '  Telephone = :Tel, ' +
      '  Adresse = :Adresse ' +
      'WHERE ID = :ID';
    Query.ParamByName('Nom').AsString := EditNom.Text;
    Query.ParamByName('Email').AsString := EditEmail.Text;
    Query.ParamByName('Tel').AsString := EditTelephone.Text;
    Query.ParamByName('Adresse').AsString := EditAdresse.Text;
    Query.ParamByName('ID').AsInteger := UtilisateurConnecteID;
    Query.ExecSQL;

    // Logger la modification
    TLogger.Instance.Info('Profil modifié',
                          Format('User ID: %d', [UtilisateurConnecteID]));

    ShowMessage('Profil mis à jour');
  finally
    Query.Free;
  end;
end;
```

### 3. Droit à l'effacement (droit à l'oubli)

L'utilisateur peut demander la suppression de ses données.

```pascal
type
  TSuppressionCompte = class
  private
    FConnection: TFDConnection;
    procedure SupprimerDonneesUtilisateur(AIDUtilisateur: Integer);
    procedure AnonymiserDonneesConservees(AIDUtilisateur: Integer);
    procedure JournaliserSuppression(AIDUtilisateur: Integer);
  public
    constructor Create(AConnection: TFDConnection);
    procedure SupprimerCompte(AIDUtilisateur: Integer; const ARaison: string);
  end;

constructor TSuppressionCompte.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TSuppressionCompte.SupprimerDonneesUtilisateur(AIDUtilisateur: Integer);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Supprimer les données non nécessaires légalement
    Query.SQL.Text := 'DELETE FROM Preferences WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;

    Query.SQL.Text := 'DELETE FROM HistoriqueNavigation WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;

    Query.SQL.Text := 'DELETE FROM Newsletter WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;

    // Supprimer le compte utilisateur
    Query.SQL.Text := 'DELETE FROM Utilisateurs WHERE ID = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSuppressionCompte.AnonymiserDonneesConservees(AIDUtilisateur: Integer);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Anonymiser les commandes (obligation légale de garder 5 ans).
    //
    // ⚠ Si une table archive des commandes a une CONTRAINTE UNIQUE sur
    //   EmailClient, écraser plusieurs lignes avec « anonyme@deleted.com »
    //   violera cette contrainte. Solutions :
    //   - retirer la contrainte UNIQUE sur les tables historiques ;
    //   - ou anonymiser avec une valeur dérivée non collisionnante,
    //     par exemple Format('anonyme+%d@deleted.local', [IDCommande]).
    Query.SQL.Text :=
      'UPDATE Commandes SET ' +
      '  NomClient = ''Utilisateur supprimé'', ' +
      '  EmailClient = ''anonyme@deleted.com'', ' +
      '  TelephoneClient = NULL, ' +
      '  AdresseLivraison = ''Anonymisée'' ' +
      'WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;

    // Anonymiser les avis/commentaires (option : supprimer ou anonymiser)
    Query.SQL.Text :=
      'UPDATE Avis SET ' +
      '  NomAuteur = ''Utilisateur'', ' +
      '  IDUtilisateur = NULL ' +
      'WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSuppressionCompte.JournaliserSuppression(AIDUtilisateur: Integer);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO LogsSuppressionComptes (IDUtilisateur, DateSuppression) ' +
      'VALUES (:ID, NOW())';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSuppressionCompte.SupprimerCompte(AIDUtilisateur: Integer; const ARaison: string);  
begin  
  // ⚠️ La suppression RGPD doit être atomique : si l'une des étapes échoue,
  // il ne faut pas se retrouver avec un utilisateur supprimé mais des commandes
  // non anonymisées (fuite RGPD) ou des préférences orphelines.
  FConnection.StartTransaction;
  try
    // 1. Journaliser la demande (la trace de la demande doit survivre au rollback :
    //    on peut la dupliquer hors transaction si le rollback échoue)
    JournaliserSuppression(AIDUtilisateur);

    // 2. Anonymiser les données à conserver (commandes pour obligations fiscales)
    AnonymiserDonneesConservees(AIDUtilisateur);

    // 3. Supprimer toutes les autres données
    SupprimerDonneesUtilisateur(AIDUtilisateur);

    FConnection.Commit;
  except
    on E: Exception do
    begin
      FConnection.Rollback;
      TLogger.Instance.Error('Suppression RGPD échouée',
        Format('User ID: %d, Erreur: %s', [AIDUtilisateur, E.Message]));
      raise;
    end;
  end;

  TLogger.Instance.Info('Compte supprimé',
                        Format('User ID: %d, Raison: %s', [AIDUtilisateur, ARaison]));
end;

// Interface utilisateur
procedure TFormParametres.BtnSupprimerCompteClick(Sender: TObject);  
var  
  Suppression: TSuppressionCompte;
  Confirmation: string;
begin
  // Double confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer votre compte ?' + sLineBreak +
                'Cette action est IRRÉVERSIBLE.',
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Confirmation := '';
  if not InputQuery('Confirmation', 'Tapez SUPPRIMER pour confirmer', Confirmation) then
    Exit;

  if Confirmation <> 'SUPPRIMER' then
  begin
    ShowMessage('Confirmation incorrecte');
    Exit;
  end;

  Suppression := TSuppressionCompte.Create(FDConnection1);
  try
    Suppression.SupprimerCompte(UtilisateurConnecteID, 'Demande utilisateur');
    ShowMessage('Votre compte a été supprimé. Au revoir.');
    Application.Terminate;
  finally
    Suppression.Free;
  end;
end;
```

### 4. Droit à la portabilité

L'utilisateur peut récupérer ses données dans un format structuré.

```pascal
procedure TFormProfil.BtnExporterDonneesPortablesClick(Sender: TObject);  
var  
  Exportation: TExportDonneesPersonnelles;
  JSON: string;
  CheminJSON, CheminCSV: string;
begin
  Exportation := TExportDonneesPersonnelles.Create(FDConnection1);
  try
    // Export JSON
    JSON := Exportation.ExporterDonneesUtilisateur(UtilisateurConnecteID);
    CheminJSON := TPath.Combine(TPath.GetDocumentsPath, 'mes_donnees.json');
    TFile.WriteAllText(CheminJSON, JSON, TEncoding.UTF8);

    // Export CSV (pour Excel)
    CheminCSV := TPath.Combine(TPath.GetDocumentsPath, 'mes_donnees.csv');
    ExporterVersCSV(UtilisateurConnecteID, CheminCSV);

    ShowMessage('Vos données ont été exportées :' + sLineBreak +
                '- JSON : ' + CheminJSON + sLineBreak +
                '- CSV : ' + CheminCSV);
  finally
    Exportation.Free;
  end;
end;

function EchapperChampCSV(const AValeur: string): string;  
begin  
  // RFC 4180 : si le champ contient `;`, `"`, `,` ou un retour ligne,
  // il faut le entourer de guillemets doubles ; les guillemets internes
  // sont doublés.
  if AValeur.IndexOfAny([';', '"', ',', #10, #13]) >= 0 then
    Result := '"' + StringReplace(AValeur, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AValeur;
end;

procedure ExporterVersCSV(AIDUtilisateur: Integer; const ANomFichier: string);  
var  
  Query: TFDQuery;
  CSV: TStringList;
  Settings: TFormatSettings;
begin
  // Format invariant pour les dates et nombres — sinon un Belge et un
  // Américain obtiendront des CSV incompatibles (`,` vs `.` comme décimal).
  Settings := TFormatSettings.Invariant;

  Query := TFDQuery.Create(nil);
  CSV := TStringList.Create;
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT * FROM Commandes WHERE IDUtilisateur = :ID';
    Query.ParamByName('ID').AsInteger := AIDUtilisateur;
    Query.Open;

    // En-tête
    CSV.Add('Numero;Date;Montant;Statut');

    // Données
    while not Query.Eof do
    begin
      CSV.Add(
        EchapperChampCSV(Query.FieldByName('Numero').AsString) + ';' +
        FormatDateTime('yyyy-mm-dd', Query.FieldByName('DateCommande').AsDateTime) + ';' +
        FormatFloat('0.00', Query.FieldByName('Montant').AsFloat, Settings) + ';' +
        EchapperChampCSV(Query.FieldByName('Statut').AsString));
      Query.Next;
    end;

    // Préfixer d'un BOM UTF-8 pour qu'Excel détecte correctement l'encodage
    CSV.WriteBOM := True;
    CSV.SaveToFile(ANomFichier, TEncoding.UTF8);
  finally
    CSV.Free;
    Query.Free;
  end;
end;
```

> 💡 **Sauvegardes et droit à l'oubli** : la suppression dans la base **principale** ne suffit pas — vos sauvegardes contiennent encore les données. La CNIL accepte généralement le principe de la « purge cyclique » : la donnée disparaît au prochain cycle de rotation des sauvegardes, à condition que :  
> - votre **politique de rétention** soit documentée et raisonnable (typiquement 30 à 90 jours) ;  
> - les sauvegardes soient **isolées** (chiffrées + accès restreint) jusqu'à leur rotation ;  
> - si l'utilisateur demande explicitement la suppression, vous l'**informez** du délai de purge des sauvegardes.

### 5. Droit d'opposition

L'utilisateur peut s'opposer à certains traitements.

```pascal
type
  TGestionConsentements = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure DefinirConsentement(AIDUtilisateur: Integer;
                                   const ATypeTraitement: string;
                                   AConsenti: Boolean);
    function AConsenti(AIDUtilisateur: Integer; const ATypeTraitement: string): Boolean;
  end;

constructor TGestionConsentements.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TGestionConsentements.DefinirConsentement(AIDUtilisateur: Integer;
                                                     const ATypeTraitement: string;
                                                     AConsenti: Boolean);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Enregistrer ou mettre à jour le consentement
    // ⚠️ `ON DUPLICATE KEY UPDATE` est une extension MySQL/MariaDB.
    //   - PostgreSQL : `ON CONFLICT (IDUtilisateur, TypeTraitement) DO UPDATE ...`
    //   - SQL Server : `MERGE ... WHEN MATCHED THEN UPDATE WHEN NOT MATCHED THEN INSERT`
    //   - Firebird   : `UPDATE OR INSERT INTO ... MATCHING (...)`
    //   - SQLite     : `ON CONFLICT (...) DO UPDATE SET ...` (≥ 3.24)
    Query.SQL.Text :=
      'INSERT INTO Consentements (IDUtilisateur, TypeTraitement, Consenti, DateModification) ' +
      'VALUES (:IDUser, :Type, :Consenti, NOW()) ' +
      'ON DUPLICATE KEY UPDATE Consenti = :Consenti, DateModification = NOW()';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('Type').AsString := ATypeTraitement;
    Query.ParamByName('Consenti').AsBoolean := AConsenti;
    Query.ExecSQL;

    // Logger le changement
    TLogger.Instance.Info('Consentement modifié',
      Format('User: %d, Type: %s, Consenti: %s',
             [AIDUtilisateur, ATypeTraitement, BoolToStr(AConsenti, True)]));
  finally
    Query.Free;
  end;
end;

function TGestionConsentements.AConsenti(AIDUtilisateur: Integer;
                                          const ATypeTraitement: string): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT Consenti FROM Consentements ' +
      'WHERE IDUtilisateur = :IDUser AND TypeTraitement = :Type';
    Query.ParamByName('IDUser').AsInteger := AIDUtilisateur;
    Query.ParamByName('Type').AsString := ATypeTraitement;
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('Consenti').AsBoolean;
  finally
    Query.Free;
  end;
end;

// Interface de gestion des consentements
procedure TFormParametresViePrivee.ChargerConsentements;  
begin  
  CheckBoxNewsletter.Checked :=
    GestionConsentements.AConsenti(UtilisateurConnecteID, 'newsletter');

  CheckBoxAnalyse.Checked :=
    GestionConsentements.AConsenti(UtilisateurConnecteID, 'analyse_statistique');

  CheckBoxPartage.Checked :=
    GestionConsentements.AConsenti(UtilisateurConnecteID, 'partage_partenaires');
end;

procedure TFormParametresViePrivee.BtnEnregistrerClick(Sender: TObject);  
begin  
  GestionConsentements.DefinirConsentement(
    UtilisateurConnecteID, 'newsletter', CheckBoxNewsletter.Checked);

  GestionConsentements.DefinirConsentement(
    UtilisateurConnecteID, 'analyse_statistique', CheckBoxAnalyse.Checked);

  GestionConsentements.DefinirConsentement(
    UtilisateurConnecteID, 'partage_partenaires', CheckBoxPartage.Checked);

  ShowMessage('Vos préférences ont été enregistrées');
end;
```

## Gestion du consentement

### Structure de base de données

```sql
-- Table « état courant » : un seul consentement actif par (utilisateur, type)
CREATE TABLE Consentements (
    ID INT PRIMARY KEY AUTO_INCREMENT,
    IDUtilisateur INT NOT NULL,
    TypeTraitement VARCHAR(50) NOT NULL,
    Consenti BOOLEAN NOT NULL,
    DateModification DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    AdresseIP VARCHAR(45),
    UserAgent VARCHAR(255),
    UNIQUE KEY (IDUtilisateur, TypeTraitement),
    FOREIGN KEY (IDUtilisateur) REFERENCES Utilisateurs(ID) ON DELETE CASCADE
);

-- Index pour recherche rapide
CREATE INDEX idx_utilisateur_type ON Consentements(IDUtilisateur, TypeTraitement);

-- ⚠ Le RGPD exige de POUVOIR PROUVER le consentement (article 7§1). En cas
--   de litige avec un utilisateur qui prétend ne pas avoir consenti, vous
--   devez démontrer quand et comment il l'a fait. Un simple UPSERT écrase
--   l'historique — il faut donc une table d'historique en plus :
CREATE TABLE HistoriqueConsentements (
    ID BIGINT PRIMARY KEY AUTO_INCREMENT,
    IDUtilisateur INT NOT NULL,
    TypeTraitement VARCHAR(50) NOT NULL,
    Consenti BOOLEAN NOT NULL,
    DateChangement DATETIME DEFAULT CURRENT_TIMESTAMP,
    AdresseIP VARCHAR(45),
    UserAgent VARCHAR(255),
    Source VARCHAR(50),         -- 'inscription', 'paramètres', 'banniere_cookie'…
    INDEX idx_user_date (IDUtilisateur, DateChangement DESC)
);

-- ⚠ `ON DELETE CASCADE` supprime les consentements quand l'utilisateur est
--   supprimé. Si vous voulez conserver une preuve de consentement APRÈS
--   suppression du compte (par exemple en cas de litige), utilisez plutôt
--   `ON DELETE SET NULL` sur HistoriqueConsentements (avec IDUtilisateur
--   nullable) ou conservez les lignes anonymisées.
```

### Demande de consentement explicite

```pascal
procedure TFormInscription.BtnInscrireClick(Sender: TObject);  
begin  
  // Validation des champs...

  // Consentement EXPLICITE obligatoire
  if not CheckBoxCGU.Checked then
  begin
    ShowMessage('Vous devez accepter les conditions générales d''utilisation');
    Exit;
  end;

  // Créer le compte
  CreerCompte(EditEmail.Text, HashMotDePasse(EditPassword.Text));

  // Enregistrer les consentements
  GestionConsentements.DefinirConsentement(NouvelIDUtilisateur, 'cgu', True);

  // Consentement OPTIONNEL pour newsletter (case décochée par défaut)
  if CheckBoxNewsletter.Checked then
    GestionConsentements.DefinirConsentement(NouvelIDUtilisateur, 'newsletter', True);

  ShowMessage('Compte créé avec succès');
end;
```

### Interface de consentement granulaire

```pascal
procedure TFormConsentements.AfficherOptionsConsentement;  
begin  
  // Consentement obligatoire (non modifiable)
  CheckBoxCGU.Checked := True;
  CheckBoxCGU.Enabled := False;
  LabelCGU.Caption := 'Conditions générales d''utilisation (Obligatoire)';

  // Consentements optionnels (modifiables)
  CheckBoxNewsletter.Caption := 'Je souhaite recevoir la newsletter hebdomadaire';
  CheckBoxNewsletter.Checked := False; // Décochée par défaut (opt-in)

  CheckBoxAnalyse.Caption := 'J''accepte l''analyse de mon utilisation pour améliorer le service';
  CheckBoxAnalyse.Checked := False;

  CheckBoxPartage.Caption := 'J''accepte le partage de mes données avec des partenaires';
  CheckBoxPartage.Checked := False;

  // Ajouter des liens vers les détails
  LinkPolitique.Caption := 'Voir la politique de confidentialité';
end;
```

## Notification des violations de données

### Obligation légale

Une « violation de données » au sens du RGPD (article 4§12) est **toute violation de la sécurité entraînant** la destruction, la perte, l'altération, la divulgation non autorisée ou l'accès à des données personnelles. Cela couvre bien plus que les piratages : perte d'un ordinateur portable non chiffré, envoi par erreur d'un email à la mauvaise personne, suppression accidentelle… sont aussi des violations.

En cas de violation, le responsable de traitement doit :

1. **Notifier l'autorité de contrôle** (CNIL en France) **dans les 72 h** après en avoir pris connaissance (article 33). ⚠ Si vous ne respectez pas ce délai, vous devez justifier ce retard. Si le risque pour les droits et libertés est **improbable** (par ex. : données déjà chiffrées avec une clé non compromise, perte rapidement contenue), la notification n'est pas obligatoire — mais documentez votre raisonnement.

2. **Informer les utilisateurs concernés sans délai** uniquement si le risque pour leurs droits et libertés est **ÉLEVÉ** (article 34). Pas systématiquement.

3. **Tenir un registre interne** de TOUTES les violations (même celles non notifiées à la CNIL), avec : nature, catégories et nombre approximatif de personnes concernées, conséquences probables, mesures prises (article 33§5).

```pascal
type
  TViolationDonnees = class
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure EnregistrerViolation(const ADescription, AGravite: string;
                                     AUtilisateursConcernes: TArray<Integer>);
    procedure NotifierCNIL(const ADetails: string);
    procedure NotifierUtilisateurs(AUtilisateursConcernes: TArray<Integer>);
  end;

constructor TViolationDonnees.Create(AConnection: TFDConnection);  
begin  
  inherited Create;
  FConnection := AConnection;
end;

procedure TViolationDonnees.EnregistrerViolation(const ADescription, AGravite: string;
                                                   AUtilisateursConcernes: TArray<Integer>);
var
  Query: TFDQuery;
  IDViolation: Integer;
  IDUser: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // ⚠ Idéalement, encapsuler tout le bloc dans une transaction
    //   `FConnection.StartTransaction` / `Commit` / `Rollback` : si
    //   l'INSERT principal réussit mais que le `for` qui suit échoue
    //   à mi-chemin (perte de connexion, contrainte), on aurait une
    //   violation enregistrée avec un nombre d'utilisateurs partiel.

    // Enregistrer la violation
    Query.SQL.Text :=
      'INSERT INTO ViolationsDonnees (DateDetection, Description, Gravite, NbUtilisateurs) ' +
      'VALUES (NOW(), :Description, :Gravite, :NbUsers)';
    Query.ParamByName('Description').AsString := ADescription;
    Query.ParamByName('Gravite').AsString := AGravite;
    Query.ParamByName('NbUsers').AsInteger := Length(AUtilisateursConcernes);
    Query.ExecSQL;

    // Récupérer l'ID de la violation.
    // ⚠ `GetLastAutoGenValue` fonctionne sur MySQL/MariaDB. Pour les autres SGBD :
    //   - PostgreSQL : utiliser `INSERT ... RETURNING ID` puis `Query.Open`
    //   - SQL Server : `SELECT SCOPE_IDENTITY()` après l'INSERT
    //   - Oracle/Firebird : `INSERT ... RETURNING ID INTO :ID`
    //   - SQLite : `Query.Connection.GetLastAutoGenValue` fonctionne aussi.
    IDViolation := Query.Connection.GetLastAutoGenValue;

    // Enregistrer les utilisateurs concernés
    for IDUser in AUtilisateursConcernes do
    begin
      Query.SQL.Text :=
        'INSERT INTO UtilisateursViolation (IDViolation, IDUtilisateur) ' +
        'VALUES (:IDViolation, :IDUser)';
      Query.ParamByName('IDViolation').AsInteger := IDViolation;
      Query.ParamByName('IDUser').AsInteger := IDUser;
      Query.ExecSQL;
    end;

    // Logger
    TLogger.Instance.Critical('Violation de données détectée',
      Format('Description: %s, Utilisateurs: %d',
             [ADescription, Length(AUtilisateursConcernes)]));
  finally
    Query.Free;
  end;
end;

procedure TViolationDonnees.NotifierCNIL(const ADetails: string);  
begin  
  // En production : envoyer via le portail de notification de la CNIL
  // https://www.cnil.fr/fr/notifier-une-violation-de-donnees-personnelles

  TLogger.Instance.Critical('Notification CNIL requise', ADetails);

  // Envoyer un email à l'équipe de conformité
  EnvoyerEmail('dpo@monentreprise.com',
               'URGENT : Violation de données - Notification CNIL requise',
               ADetails);
end;

procedure TViolationDonnees.NotifierUtilisateurs(AUtilisateursConcernes: TArray<Integer>);  
var  
  Query: TFDQuery;
  IDUser: Integer;
  Email: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    for IDUser in AUtilisateursConcernes do
    begin
      Query.SQL.Text := 'SELECT Email FROM Utilisateurs WHERE ID = :ID';
      Query.ParamByName('ID').AsInteger := IDUser;
      Query.Open;

      if not Query.IsEmpty then
      begin
        Email := Query.FieldByName('Email').AsString;

        EnvoyerEmail(Email,
          'IMPORTANT : Information sur la sécurité de vos données',
          'Nous vous informons qu''une violation de données a été détectée. ' +
          'Vos données ont pu être exposées. ' +
          'Par précaution, nous vous recommandons de changer immédiatement votre mot de passe. ' +
          'Détails complets : [lien vers page d''information]');
      end;

      Query.Close;
    end;
  finally
    Query.Free;
  end;
end;

// En cas de violation détectée
procedure GererViolationDonnees;  
var  
  Violation: TViolationDonnees;
  UtilisateursConcernes: TArray<Integer>;
begin
  Violation := TViolationDonnees.Create(FDConnection1);
  try
    // Identifier les utilisateurs concernés
    UtilisateursConcernes := IdentifierUtilisateursConcernes;

    // Enregistrer la violation
    Violation.EnregistrerViolation(
      'Accès non autorisé à la base de données clients',
      'ÉLEVÉE',
      UtilisateursConcernes
    );

    // Notifier la CNIL dans les 72h
    Violation.NotifierCNIL('Détails de la violation...');

    // Notifier les utilisateurs concernés
    Violation.NotifierUtilisateurs(UtilisateursConcernes);
  finally
    Violation.Free;
  end;
end;
```

## Registre des traitements

Le RGPD impose de tenir un registre de tous les traitements de données.

> ⚠️ **Anti-pattern Pascal — record avec objets gérés manuellement** :  
> le `TTraitement` ci-dessous contient des `TStringList` comme champs. Or  
> un `record` est copié **par valeur** lors d'une affectation ou d'un  
> `TList.Add` — mais ses pointeurs internes (TStringList) ne sont PAS  
> dupliqués : on obtient deux records partageant les mêmes objets. Si l'un  
> libère le TStringList, l'autre devient invalide. La libération manuelle  
> dans `TRegistreTraitements.Destroy` est fragile.  
>  
> **Recommandation** : transformer `TTraitement` en **classe** (ownership clair  
> via destructeur) ou n'utiliser que des types managés (`TArray<string>` au  
> lieu de `TStringList`).

```pascal
type
  TRegistreTraitements = class
  private
    FTraitements: TList<TTraitement>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterTraitement(const ATraitement: TTraitement);
    procedure GenererRapport(const ANomFichier: string);
  end;

type
  // ⚠ Voir l'avertissement ci-dessus : ce record est conservé à des fins
  //   pédagogiques mais sa gestion mémoire est délicate. Préférer une
  //   classe TTraitement avec destructeur, ou des `TArray<string>`.
  TTraitement = record
    Nom: string;
    Finalite: string;
    BaseJuridique: string;  // Consentement, Contrat, Obligation légale, etc.
    DonneesCollectees: TStringList;
    Destinataires: TStringList;
    DureeConservation: string;
    MesuresSecurite: TStringList;
    TransfertHorsUE: Boolean;
    PaysDestination: string;
  end;

constructor TRegistreTraitements.Create;  
begin  
  inherited Create;
  FTraitements := TList<TTraitement>.Create;

  // Définir tous les traitements
  DefinirTraitements;
end;

procedure DefinirTraitements;  
var  
  Traitement: TTraitement;
begin
  // Traitement 1 : Gestion des comptes clients
  Traitement.Nom := 'Gestion des comptes clients';
  Traitement.Finalite := 'Permettre aux utilisateurs de créer un compte et utiliser nos services';
  Traitement.BaseJuridique := 'Exécution du contrat';
  Traitement.DonneesCollectees := TStringList.Create;
  Traitement.DonneesCollectees.Add('Nom');
  Traitement.DonneesCollectees.Add('Prénom');
  Traitement.DonneesCollectees.Add('Email');
  Traitement.DonneesCollectees.Add('Mot de passe (hashé)');
  Traitement.Destinataires := TStringList.Create;
  Traitement.Destinataires.Add('Service informatique');
  Traitement.Destinataires.Add('Service client');
  Traitement.DureeConservation := '3 ans après dernière connexion';
  Traitement.MesuresSecurite := TStringList.Create;
  Traitement.MesuresSecurite.Add('Chiffrement des données sensibles');
  Traitement.MesuresSecurite.Add('Connexions HTTPS');
  Traitement.MesuresSecurite.Add('Contrôle d''accès');
  Traitement.TransfertHorsUE := False;

  FTraitements.Add(Traitement);

  // Traitement 2 : Newsletter
  Traitement.Nom := 'Envoi de newsletter';
  Traitement.Finalite := 'Informer les abonnés des nouveautés';
  Traitement.BaseJuridique := 'Consentement';  // IMPORTANT : consentement explicite
  Traitement.DonneesCollectees := TStringList.Create;
  Traitement.DonneesCollectees.Add('Email');
  Traitement.DonneesCollectees.Add('Prénom');
  Traitement.Destinataires := TStringList.Create;
  Traitement.Destinataires.Add('Service marketing');
  Traitement.Destinataires.Add('Prestataire emailing (Mailchimp)');
  Traitement.DureeConservation := '3 ans ou jusqu''à désinscription';
  Traitement.MesuresSecurite := TStringList.Create;
  Traitement.MesuresSecurite.Add('API sécurisée avec le prestataire');
  Traitement.TransfertHorsUE := True;
  Traitement.PaysDestination := 'États-Unis (garanties appropriées)';

  FTraitements.Add(Traitement);

  // Ajouter tous les autres traitements...
end;

procedure TRegistreTraitements.GenererRapport(const ANomFichier: string);  
var  
  Fichier: TextFile;
  Traitement: TTraitement;
  Donnee: string;
begin
  AssignFile(Fichier, ANomFichier);
  Rewrite(Fichier);
  try
    WriteLn(Fichier, 'REGISTRE DES TRAITEMENTS');
    WriteLn(Fichier, 'Conformité RGPD - Article 30');
    WriteLn(Fichier, 'Date : ' + DateToStr(Date));
    WriteLn(Fichier, StringOfChar('=', 80));
    WriteLn(Fichier, '');

    for Traitement in FTraitements do
    begin
      WriteLn(Fichier, 'TRAITEMENT : ' + Traitement.Nom);
      WriteLn(Fichier, '  Finalité : ' + Traitement.Finalite);
      WriteLn(Fichier, '  Base juridique : ' + Traitement.BaseJuridique);
      WriteLn(Fichier, '  Données collectées :');
      for Donnee in Traitement.DonneesCollectees do
        WriteLn(Fichier, '    - ' + Donnee);
      WriteLn(Fichier, '  Durée de conservation : ' + Traitement.DureeConservation);
      WriteLn(Fichier, '  Transfert hors UE : ' + BoolToStr(Traitement.TransfertHorsUE, True));
      if Traitement.TransfertHorsUE then
        WriteLn(Fichier, '  Pays : ' + Traitement.PaysDestination);
      WriteLn(Fichier, '');
    end;
  finally
    CloseFile(Fichier);
  end;
end;
```

## Politique de confidentialité

Vous devez fournir une politique de confidentialité claire et accessible.

```pascal
procedure AfficherPolitiqueConfidentialite;  
var  
  Politique: TStringList;
begin
  Politique := TStringList.Create;
  try
    Politique.Add('POLITIQUE DE CONFIDENTIALITÉ');
    Politique.Add('');
    Politique.Add('Dernière mise à jour : ' + DateToStr(Date));
    Politique.Add('');
    Politique.Add('1. QUI COLLECTE VOS DONNÉES ?');
    Politique.Add('   [Nom de votre société], [adresse]');
    Politique.Add('');
    Politique.Add('2. QUELLES DONNÉES COLLECTONS-NOUS ?');
    Politique.Add('   - Données d''identification : nom, prénom, email');
    Politique.Add('   - Données de connexion : adresse IP, cookies');
    Politique.Add('   - Données de commande : adresse de livraison, historique');
    Politique.Add('');
    Politique.Add('3. POURQUOI COLLECTONS-NOUS CES DONNÉES ?');
    Politique.Add('   - Créer et gérer votre compte');
    Politique.Add('   - Traiter vos commandes');
    Politique.Add('   - Vous envoyer des communications (avec votre consentement)');
    Politique.Add('');
    Politique.Add('4. COMBIEN DE TEMPS CONSERVONS-NOUS VOS DONNÉES ?');
    Politique.Add('   - Comptes inactifs : 3 ans');
    Politique.Add('   - Commandes : 5 ans (obligation légale)');
    Politique.Add('');
    Politique.Add('5. VOS DROITS');
    Politique.Add('   - Droit d''accès à vos données (art. 15)');
    Politique.Add('   - Droit de rectification (art. 16)');
    Politique.Add('   - Droit à l''effacement / droit à l''oubli (art. 17)');
    Politique.Add('   - Droit à la limitation du traitement (art. 18)');
    Politique.Add('   - Droit à la portabilité (art. 20)');
    Politique.Add('   - Droit d''opposition (art. 21)');
    Politique.Add('   - Droit de ne pas faire l''objet d''une décision');
    Politique.Add('     fondée exclusivement sur un traitement automatisé (art. 22)');
    Politique.Add('   - Droit de retirer votre consentement à tout moment (art. 7§3)');
    Politique.Add('   - Droit d''introduire une réclamation auprès de la CNIL (art. 77)');
    Politique.Add('');
    Politique.Add('   Pour exercer vos droits : dpo@monentreprise.com');
    Politique.Add('');
    Politique.Add('6. SÉCURITÉ');
    Politique.Add('   Nous mettons en œuvre des mesures techniques et organisationnelles');
    Politique.Add('   pour protéger vos données : chiffrement, HTTPS, contrôles d''accès.');

    // Afficher dans une fenêtre ou un navigateur web
    ShowMessage(Politique.Text);
  finally
    Politique.Free;
  end;
end;
```

## Checklist de conformité RGPD

### Avant le lancement

- [ ] Politique de confidentialité rédigée et accessible
- [ ] Consentement explicite pour toute collecte de données
- [ ] Processus de gestion des droits des utilisateurs
  - [ ] Droit d'accès (export de données)
  - [ ] Droit de rectification (modification)
  - [ ] Droit à l'effacement (suppression)
  - [ ] Droit à la portabilité (export structuré)
  - [ ] Droit d'opposition (gestion consentements)
- [ ] Registre des traitements complété
- [ ] Mesures de sécurité implémentées
- [ ] Durées de conservation définies et appliquées
- [ ] Procédure de notification des violations
- [ ] DPO (Délégué à la Protection des Données) désigné si nécessaire

### Pendant l'exploitation

- [ ] Nettoyage régulier des données obsolètes
- [ ] Mise à jour de la politique de confidentialité si changements
- [ ] Traitement des demandes d'exercice de droits dans les délais
- [ ] Surveillance des violations de données
- [ ] Formation continue de l'équipe
- [ ] Audits de conformité réguliers

## Bonnes pratiques

### ✅ À faire

**1. Privacy by Design**
```pascal
// Concevoir l'application avec la vie privée au cœur
// Chiffrer dès la conception, minimiser les données
```

**2. Transparence**
```pascal
// Expliquer clairement ce que vous faites des données
// Avant de collecter, informer l'utilisateur
```

**3. Minimisation**
```pascal
// Ne demander que le strict nécessaire
// Pseudonymiser quand c'est possible
```

**4. Documentation**
```pascal
// Tenir un registre des traitements
// Documenter les mesures de sécurité
```

**5. Formation**
```pascal
// Former l'équipe au RGPD
// Sensibiliser aux risques
```

### ❌ À éviter

**1. Collecte excessive**
```pascal
// ❌ Demander la date de naissance pour une newsletter
```

**2. Consentement pré-coché**
```pascal
// ❌ Case newsletter cochée par défaut
// ✅ Case décochée, l'utilisateur doit cocher activement
```

**3. Durée illimitée**
```pascal
// ❌ Garder les données indéfiniment
// ✅ Définir et respecter des durées de conservation
```

**4. Pas de procédure d'exercice des droits**
```pascal
// ❌ Ignorer les demandes de suppression
// ✅ Article 12 §3 : 1 mois maximum, prolongeable de 2 mois pour les demandes
//    complexes ou nombreuses (le responsable doit alors notifier l'utilisateur
//    de la prolongation et de ses motifs dans le délai initial d'1 mois).
```

## Résumé des points essentiels

✅ **Obligations RGPD essentielles** :
- Recueillir le consentement explicite
- Informer clairement les utilisateurs
- Permettre l'exercice des droits (accès, rectification, effacement, portabilité, opposition)
- Sécuriser les données
- Limiter la conservation
- Notifier les violations (CNIL + utilisateurs)
- Tenir un registre des traitements

❌ **Erreurs à éviter absolument** :
- Collecter sans consentement
- Conserver indéfiniment
- Ignorer les demandes d'exercice de droits
- Ne pas sécuriser les données
- Utiliser les données pour d'autres finalités
- Ne pas notifier une violation
- Transférer hors UE sans garanties

🎯 **Impact du non-respect** :
- Amendes jusqu'à 20 millions € ou 4% du CA mondial
- Perte de confiance des utilisateurs
- Dommages réputationnels
- Actions en justice possibles

## Ressources utiles

**Officielles** :
- CNIL : https://www.cnil.fr
- Texte du RGPD : https://www.cnil.fr/fr/reglement-europeen-protection-donnees
- Guide du développeur CNIL : https://www.cnil.fr/fr/guide-rgpd-du-developpeur

**Outils** :
- Modèles de politique de confidentialité
- Registre des traitements (template)
- Guide de notification de violation

Le RGPD n'est pas qu'une contrainte légale, c'est aussi une opportunité de construire une relation de confiance avec vos utilisateurs en respectant leur vie privée. Intégrez ces principes dès la conception de votre application.

⏭️ [Signature numérique et validation](/16-securite-des-applications/09-signature-numerique-et-validation.md)
