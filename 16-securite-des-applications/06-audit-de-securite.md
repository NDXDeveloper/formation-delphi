# 16. Sécurité des applications
## 16.6 Audit de sécurité

Une fois que vous avez mis en place les différentes protections pour sécuriser votre application Delphi, il est essentiel de vérifier leur efficacité par le biais d'un audit de sécurité. Ce processus vous permet d'identifier les vulnérabilités potentielles avant qu'elles ne soient exploitées par des attaquants.

Dans ce chapitre, nous allons découvrir comment réaliser un audit de sécurité de base pour vos applications Delphi, même si vous n'êtes pas un expert en sécurité informatique.

### Qu'est-ce qu'un audit de sécurité ?

Un audit de sécurité est un examen systématique de votre application pour identifier les faiblesses potentielles, vérifier l'efficacité des mesures de sécurité mises en place, et s'assurer que les bonnes pratiques sont suivies. Il peut être réalisé à différents niveaux :

1. **Revue de code** : Analyse manuelle ou automatisée du code source
2. **Tests de sécurité dynamiques** : Tests de l'application en cours d'exécution
3. **Analyse de la configuration** : Vérification des paramètres de déploiement et d'exécution
4. **Test de pénétration** : Simulation d'attaques réelles sur l'application

### Préparation à l'audit de sécurité

Avant de commencer l'audit, il est important de définir clairement son périmètre :

```pas
procedure TPrepareSecurityAudit.PrepareAuditScope;
begin
  // Définir le périmètre de l'audit
  AuditScope.AppName := 'GestionStock';
  AuditScope.Version := '2.3.1';
  AuditScope.ModulesToAudit := ['Authentication', 'UserManagement',
                               'ProductDatabase', 'ReportGeneration'];
  AuditScope.ExcludedModules := ['Help', 'About'];
  AuditScope.SecurityRequirements := [
    'Protection contre injection SQL',
    'Stockage sécurisé des mots de passe',
    'Contrôle d''accès basé sur les rôles',
    'Protection des données sensibles',
    'Journalisation de sécurité'
  ];

  // Rassembler la documentation pertinente
  CollectDocumentation('architecture_diagram.pdf');
  CollectDocumentation('database_schema.pdf');
  CollectDocumentation('previous_audit_report.pdf');

  // Créer un environnement de test isolé
  PrepareAuditEnvironment;
end;
```

#### Liste de vérification préalable à l'audit

Avant de commencer l'audit, utilisez cette liste de vérification pour vous assurer que vous êtes bien préparé :

1. ✅ Avez-vous une version complète et récente du code source ?
2. ✅ Disposez-vous de la documentation de l'architecture de l'application ?
3. ✅ Connaissez-vous les flux de données sensibles dans l'application ?
4. ✅ Avez-vous un environnement de test représentatif de la production ?
5. ✅ Avez-vous identifié les exigences réglementaires (RGPD, etc.) ?
6. ✅ Disposez-vous des outils nécessaires pour l'audit ?

### Les étapes d'un audit de sécurité

#### 1. Revue de code manuelle

La revue de code manuelle consiste à examiner votre code source à la recherche de vulnérabilités potentielles. C'est un processus qui demande du temps mais qui peut identifier des problèmes que les outils automatisés pourraient manquer.

Voici quelques points clés à rechercher lors d'une revue de code manuelle :

```pas
procedure TSecurityAudit.PerformManualCodeReview(const SourcePath: string);
var
  Files: TStringList;
  CurrentFile: string;
begin
  Files := FindDelphiSourceFiles(SourcePath);
  try
    for CurrentFile in Files do
    begin
      // Vérifier les problèmes d'injection SQL
      CheckForSQLInjection(CurrentFile);

      // Vérifier le stockage des mots de passe
      CheckForInsecurePasswordStorage(CurrentFile);

      // Vérifier la validation des entrées
      CheckForInputValidation(CurrentFile);

      // Vérifier la gestion des erreurs et exceptions
      CheckForErrorHandling(CurrentFile);

      // Vérifier l'exposition de données sensibles
      CheckForSensitiveDataExposure(CurrentFile);

      // Journaliser les problèmes trouvés
      LogFindings(CurrentFile);
    end;
  finally
    Files.Free;
  end;
end;
```

##### Exemple : Recherche d'injections SQL potentielles

```pas
procedure TSecurityAudit.CheckForSQLInjection(const SourceFile: string);
var
  FileContent: TStringList;
  LineNumber: Integer;
  CurrentLine: string;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(SourceFile);

    for LineNumber := 0 to FileContent.Count - 1 do
    begin
      CurrentLine := FileContent[LineNumber];

      // Rechercher les constructions de requêtes SQL par concaténation
      if (Pos('SELECT', UpperCase(CurrentLine)) > 0) or
         (Pos('INSERT', UpperCase(CurrentLine)) > 0) or
         (Pos('UPDATE', UpperCase(CurrentLine)) > 0) or
         (Pos('DELETE', UpperCase(CurrentLine)) > 0) then
      begin
        // Rechercher les signes de concaténation de chaînes
        if (Pos(' + ', CurrentLine) > 0) or
           (Pos(''' +', CurrentLine) > 0) or
           (Pos('+ ''', CurrentLine) > 0) then
        begin
          // Potentielle injection SQL trouvée
          RecordVulnerability(
            'Injection SQL potentielle',
            SourceFile,
            LineNumber + 1,
            CurrentLine,
            'Utilisez des requêtes paramétrées au lieu de concaténer des chaînes.'
          );
        end;
      end;
    end;
  finally
    FileContent.Free;
  end;
end;
```

#### 2. Analyse de code automatisée

L'analyse automatisée permet de scanner rapidement l'ensemble de votre base de code pour identifier les problèmes courants. Voici comment intégrer cela dans votre processus d'audit :

```pas
procedure TSecurityAudit.PerformAutomatedCodeAnalysis(const ProjectPath: string);
var
  OutputLog: TStringList;
  Result: Integer;
begin
  OutputLog := TStringList.Create;
  try
    // Exemple d'utilisation d'un outil de ligne de commande fictif "DelphiSecScan"
    Result := ExecuteCommand(
      'DelphiSecScan.exe',
      ['--project', ProjectPath, '--output', 'security_scan_results.json', '--format', 'json'],
      OutputLog
    );

    if Result <> 0 then
    begin
      // L'outil a trouvé des problèmes
      LogMessage('L''analyse de code a détecté des problèmes potentiels.');

      // Analyser les résultats de l'outil
      ParseSecurityScanResults('security_scan_results.json');
    end
    else
      LogMessage('Aucun problème détecté par l''analyse automatisée.');
  finally
    OutputLog.Free;
  end;
end;
```

##### Outils d'analyse de code pour Delphi

Voici quelques outils que vous pouvez utiliser pour l'analyse automatisée de code Delphi :

1. **Sonar** avec un plugin pour Delphi
2. **Pascal Analyzer**
3. **Peganza Pascal Analyzer**
4. **DelphiAST** (pour créer vos propres analyses)

#### 3. Tests de sécurité dynamiques

Les tests dynamiques consistent à exécuter votre application et à vérifier son comportement face à diverses attaques. Voici un exemple simple de test d'injection SQL :

```pas
procedure TSecurityAudit.PerformSQLInjectionTest(ALoginForm: TLoginForm);
const
  SqlInjectionPayloads: array[0..4] of string = (
    ''' OR 1=1 --',
    'admin'' --',
    ''' UNION SELECT username, password FROM users --',
    ''' DROP TABLE users --',
    'admin''; EXEC sp_MSforeachtable @command1=''DROP TABLE ?''; --'
  );
var
  I: Integer;
  Result: Boolean;
begin
  for I := 0 to High(SqlInjectionPayloads) do
  begin
    // Tester chaque charge utile
    LogMessage('Test d''injection SQL #' + IntToStr(I+1) + ': ' + SqlInjectionPayloads[I]);

    // Essayer de se connecter avec la charge utile
    ALoginForm.EditUsername.Text := 'admin';
    ALoginForm.EditPassword.Text := SqlInjectionPayloads[I];
    ALoginForm.ButtonLogin.Click;

    // Vérifier si l'authentification a réussi (ce qui indiquerait une vulnérabilité)
    Result := IsUserAuthenticated;

    if Result then
    begin
      // Vulnérabilité détectée
      RecordVulnerability(
        'Vulnérabilité d''injection SQL confirmée',
        'LoginForm',
        0,
        'La charge utile "' + SqlInjectionPayloads[I] + '" a permis de contourner l''authentification',
        'Utilisez des requêtes paramétrées et validez toutes les entrées utilisateur.'
      );
    end;

    // Réinitialiser l'application pour le prochain test
    ResetApplicationState;
  end;
end;
```

#### 4. Vérification des configurations

Les paramètres de configuration peuvent introduire des vulnérabilités. Vérifiez-les soigneusement :

```pas
procedure TSecurityAudit.AuditAppConfigurations;
var
  IniFile: TIniFile;
  ConnectionString: string;
  LogPath: string;
  TempPath: string;
  DebugMode: Boolean;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Vérifier si la chaîne de connexion est en clair
    ConnectionString := IniFile.ReadString('Database', 'ConnectionString', '');
    if (ConnectionString <> '') and (Pos('Encrypt=', ConnectionString) <= 0) then
    begin
      RecordVulnerability(
        'Chaîne de connexion non chiffrée',
        'Configuration',
        0,
        'La chaîne de connexion à la base de données est stockée en texte clair',
        'Chiffrez les chaînes de connexion et autres informations sensibles.'
      );
    end;

    // Vérifier les chemins de journalisation
    LogPath := IniFile.ReadString('Logging', 'Path', '');
    if (LogPath <> '') and not DirectoryIsWriteProtected(LogPath) then
    begin
      RecordVulnerability(
        'Dossier de journaux non sécurisé',
        'Configuration',
        0,
        'Le dossier des journaux n''est pas protégé contre les écritures non autorisées',
        'Appliquez des restrictions d''accès appropriées aux dossiers de journalisation.'
      );
    end;

    // Vérifier si le mode débogage est activé en production
    DebugMode := IniFile.ReadBool('General', 'DebugMode', False);
    if DebugMode then
    begin
      RecordVulnerability(
        'Mode débogage activé',
        'Configuration',
        0,
        'L''application est configurée en mode débogage',
        'Désactivez le mode débogage en environnement de production.'
      );
    end;

    // D'autres vérifications de configuration...
  finally
    IniFile.Free;
  end;
end;
```

#### 5. Test de pénétration simplifié

Le test de pénétration consiste à simuler des attaques réelles. Voici un exemple simple de script pour tester la protection contre les tentatives de force brute :

```pas
procedure TSecurityAudit.TestBruteForceProtection(const URL: string);
var
  HTTP: TIdHTTP;
  Params: TStringList;
  Response: string;
  I: Integer;
  StartTime, EndTime: TDateTime;
  BlockDetected: Boolean;
begin
  HTTP := TIdHTTP.Create(nil);
  Params := TStringList.Create;
  try
    // Configurer les paramètres de la requête
    Params.Add('username=admin');

    BlockDetected := False;
    StartTime := Now;

    // Essayer de se connecter plusieurs fois avec un mot de passe incorrect
    for I := 1 to 20 do
    begin
      Params.Values['password'] := 'wrong_password_' + IntToStr(I);

      try
        Response := HTTP.Post(URL + '/login', Params);
        LogMessage('Tentative ' + IntToStr(I) + ': Réponse reçue, longueur = ' +
                  IntToStr(Length(Response)));

        // Vérifier si nous sommes bloqués (par ex. détection de CAPTCHA ou message d'erreur)
        if (Pos('too many attempts', LowerCase(Response)) > 0) or
           (Pos('captcha', LowerCase(Response)) > 0) or
           (Pos('temporarily blocked', LowerCase(Response)) > 0) then
        begin
          BlockDetected := True;
          LogMessage('Protection contre force brute détectée après ' + IntToStr(I) + ' tentatives');
          Break;
        end;
      except
        on E: EIdHTTPProtocolException do
        begin
          // HTTP 429 Too Many Requests ou autre code d'erreur indiquant une limitation
          if E.ErrorCode = 429 then
          begin
            BlockDetected := True;
            LogMessage('Protection contre force brute détectée (code 429) après ' +
                      IntToStr(I) + ' tentatives');
            Break;
          end;

          LogMessage('Erreur HTTP ' + IntToStr(E.ErrorCode) + ': ' + E.Message);
        end;
      end;

      // Ajouter un délai pour éviter d'être bloqué par des règles de pare-feu
      Sleep(500);
    end;

    EndTime := Now;

    if not BlockDetected then
    begin
      RecordVulnerability(
        'Absence de protection contre les attaques par force brute',
        'Authentication',
        0,
        '20 tentatives de connexion échouées n''ont pas déclenché de mécanisme de protection',
        'Implémentez une limitation de débit après plusieurs échecs d''authentification.'
      );
    end
    else
    begin
      // Calculer après combien de temps la protection s'est déclenchée
      LogMessage('La protection s''est déclenchée après ' +
                FormatDateTime('n:ss', EndTime - StartTime));
    end;
  finally
    Params.Free;
    HTTP.Free;
  end;
end;
```

### Documentation des résultats d'audit

Il est essentiel de bien documenter les résultats de votre audit. Voici un exemple de classe pour gérer les résultats :

```pas
unit SecurityAuditReport;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils;

type
  TVulnerabilitySeverity = (vsLow, vsMedium, vsHigh, vsCritical);

  TVulnerability = record
    ID: string;
    Title: string;
    SourceFile: string;
    LineNumber: Integer;
    Description: string;
    Recommendation: string;
    Severity: TVulnerabilitySeverity;
    DateFound: TDateTime;
  end;

  TSecurityAuditReport = class
  private
    FVulnerabilities: TArray<TVulnerability>;
    FProjectName: string;
    FAuditDate: TDateTime;
    FAuditorName: string;

    function GetVulnerabilitiesCount: Integer;
    function GetCriticalVulnerabilitiesCount: Integer;
  public
    constructor Create(const ProjectName, AuditorName: string);

    procedure AddVulnerability(const Title, SourceFile: string;
                              LineNumber: Integer;
                              const Description, Recommendation: string;
                              Severity: TVulnerabilitySeverity = vsMedium);

    procedure SaveToJSON(const FileName: string);
    procedure SaveToHTML(const FileName: string);
    procedure SaveToText(const FileName: string);

    property VulnerabilitiesCount: Integer read GetVulnerabilitiesCount;
    property CriticalVulnerabilitiesCount: Integer read GetCriticalVulnerabilitiesCount;
    property ProjectName: string read FProjectName;
    property AuditDate: TDateTime read FAuditDate;
    property AuditorName: string read FAuditorName;
  end;

implementation

constructor TSecurityAuditReport.Create(const ProjectName, AuditorName: string);
begin
  inherited Create;
  FProjectName := ProjectName;
  FAuditorName := AuditorName;
  FAuditDate := Now;
  SetLength(FVulnerabilities, 0);
end;

function TSecurityAuditReport.GetVulnerabilitiesCount: Integer;
begin
  Result := Length(FVulnerabilities);
end;

function TSecurityAuditReport.GetCriticalVulnerabilitiesCount: Integer;
var
  Vulnerability: TVulnerability;
begin
  Result := 0;

  for Vulnerability in FVulnerabilities do
    if Vulnerability.Severity = vsCritical then
      Inc(Result);
end;

procedure TSecurityAuditReport.AddVulnerability(const Title, SourceFile: string;
                                              LineNumber: Integer;
                                              const Description, Recommendation: string;
                                              Severity: TVulnerabilitySeverity);
var
  Vulnerability: TVulnerability;
begin
  Vulnerability.ID := 'VUL-' + FormatDateTime('yyyymmdd', FAuditDate) + '-' +
                     IntToStr(Length(FVulnerabilities) + 1);
  Vulnerability.Title := Title;
  Vulnerability.SourceFile := SourceFile;
  Vulnerability.LineNumber := LineNumber;
  Vulnerability.Description := Description;
  Vulnerability.Recommendation := Recommendation;
  Vulnerability.Severity := Severity;
  Vulnerability.DateFound := Now;

  SetLength(FVulnerabilities, Length(FVulnerabilities) + 1);
  FVulnerabilities[High(FVulnerabilities)] := Vulnerability;
end;

procedure TSecurityAuditReport.SaveToJSON(const FileName: string);
var
  JSONObject: TJSONObject;
  VulnerabilitiesArray: TJSONArray;
  I: Integer;
  SeverityStr: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('project_name', FProjectName);
    JSONObject.AddPair('audit_date', FormatDateTime('yyyy-mm-dd', FAuditDate));
    JSONObject.AddPair('auditor_name', FAuditorName);

    VulnerabilitiesArray := TJSONArray.Create;
    JSONObject.AddPair('vulnerabilities', VulnerabilitiesArray);

    for I := 0 to High(FVulnerabilities) do
    begin
      var VulnObj := TJSONObject.Create;

      VulnObj.AddPair('id', FVulnerabilities[I].ID);
      VulnObj.AddPair('title', FVulnerabilities[I].Title);
      VulnObj.AddPair('source_file', FVulnerabilities[I].SourceFile);
      VulnObj.AddPair('line_number', TJSONNumber.Create(FVulnerabilities[I].LineNumber));
      VulnObj.AddPair('description', FVulnerabilities[I].Description);
      VulnObj.AddPair('recommendation', FVulnerabilities[I].Recommendation);

      case FVulnerabilities[I].Severity of
        vsLow: SeverityStr := 'low';
        vsMedium: SeverityStr := 'medium';
        vsHigh: SeverityStr := 'high';
        vsCritical: SeverityStr := 'critical';
      end;

      VulnObj.AddPair('severity', SeverityStr);
      VulnObj.AddPair('date_found', FormatDateTime('yyyy-mm-dd hh:nn:ss', FVulnerabilities[I].DateFound));

      VulnerabilitiesArray.Add(VulnObj);
    end;

    TFile.WriteAllText(FileName, JSONObject.ToString);
  finally
    JSONObject.Free;
  end;
end;

procedure TSecurityAuditReport.SaveToHTML(const FileName: string);
var
  HTML: TStringList;
  I: Integer;
  SeverityClass: string;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('  <title>Rapport d''audit de sécurité - ' + FProjectName + '</title>');
    HTML.Add('  <style>');
    HTML.Add('    body { font-family: Arial, sans-serif; margin: 40px; }');
    HTML.Add('    h1 { color: #2c3e50; }');
    HTML.Add('    .summary { background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; }');
    HTML.Add('    table { width: 100%; border-collapse: collapse; margin-top: 20px; }');
    HTML.Add('    th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }');
    HTML.Add('    th { background-color: #f2f2f2; }');
    HTML.Add('    .severity-low { background-color: #d4edda; }');
    HTML.Add('    .severity-medium { background-color: #fff3cd; }');
    HTML.Add('    .severity-high { background-color: #f8d7da; }');
    HTML.Add('    .severity-critical { background-color: #dc3545; color: white; }');
    HTML.Add('  </style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('  <h1>Rapport d''audit de sécurité</h1>');
    HTML.Add('  <div class="summary">');
    HTML.Add('    <p><strong>Projet :</strong> ' + FProjectName + '</p>');
    HTML.Add('    <p><strong>Date de l''audit :</strong> ' + FormatDateTime('dd/mm/yyyy', FAuditDate) + '</p>');
    HTML.Add('    <p><strong>Auditeur :</strong> ' + FAuditorName + '</p>');
    HTML.Add('    <p><strong>Nombre total de vulnérabilités :</strong> ' + IntToStr(VulnerabilitiesCount) + '</p>');
    HTML.Add('    <p><strong>Vulnérabilités critiques :</strong> ' + IntToStr(CriticalVulnerabilitiesCount) + '</p>');
    HTML.Add('  </div>');

    if VulnerabilitiesCount > 0 then
    begin
      HTML.Add('  <h2>Vulnérabilités identifiées</h2>');
      HTML.Add('  <table>');
      HTML.Add('    <tr>');
      HTML.Add('      <th>ID</th>');
      HTML.Add('      <th>Titre</th>');
      HTML.Add('      <th>Sévérité</th>');
      HTML.Add('      <th>Fichier source</th>');
      HTML.Add('      <th>Ligne</th>');
      HTML.Add('    </tr>');

      for I := 0 to High(FVulnerabilities) do
      begin
        case FVulnerabilities[I].Severity of
          vsLow: SeverityClass := 'severity-low';
          vsMedium: SeverityClass := 'severity-medium';
          vsHigh: SeverityClass := 'severity-high';
          vsCritical: SeverityClass := 'severity-critical';
        end;

        HTML.Add('    <tr class="' + SeverityClass + '">');
        HTML.Add('      <td>' + FVulnerabilities[I].ID + '</td>');
        HTML.Add('      <td>' + FVulnerabilities[I].Title + '</td>');
        HTML.Add('      <td>' + SeverityClass.Substring(9) + '</td>');
        HTML.Add('      <td>' + FVulnerabilities[I].SourceFile + '</td>');
        HTML.Add('      <td>' + IntToStr(FVulnerabilities[I].LineNumber) + '</td>');
        HTML.Add('    </tr>');
      end;

      HTML.Add('  </table>');

      // Détails des vulnérabilités
      HTML.Add('  <h2>Détails des vulnérabilités</h2>');

      for I := 0 to High(FVulnerabilities) do
      begin
        case FVulnerabilities[I].Severity of
          vsLow: SeverityClass := 'severity-low';
          vsMedium: SeverityClass := 'severity-medium';
          vsHigh: SeverityClass := 'severity-high';
          vsCritical: SeverityClass := 'severity-critical';
        end;

        HTML.Add('  <div class="' + SeverityClass + '" style="padding: 15px; margin-bottom: 15px; border-radius: 5px;">');
        HTML.Add('    <h3>' + FVulnerabilities[I].ID + ': ' + FVulnerabilities[I].Title + '</h3>');
        HTML.Add('    <p><strong>Emplacement :</strong> ' + FVulnerabilities[I].SourceFile +
                ' (ligne ' + IntToStr(FVulnerabilities[I].LineNumber) + ')</p>');
        HTML.Add('    <p><strong>Description :</strong> ' + FVulnerabilities[I].Description + '</p>');
        HTML.Add('    <p><strong>Recommandation :</strong> ' + FVulnerabilities[I].Recommendation + '</p>');
        HTML.Add('  </div>');
      end;
    end
    else
    begin
      HTML.Add('  <h2>Aucune vulnérabilité détectée</h2>');
      HTML.Add('  <p>L''audit n''a relevé aucune vulnérabilité de sécurité dans le code examiné.</p>');
    end;

    HTML.Add('</body>');
    HTML.Add('</html>');

    HTML.SaveToFile(FileName);
  finally
    HTML.Free;
  end;
end;

procedure TSecurityAuditReport.SaveToText(const FileName: string);
var
  Text: TStringList;
  I: Integer;
  SeverityStr: string;
begin
  Text := TStringList.Create;
  try
    Text.Add('RAPPORT D''AUDIT DE SÉCURITÉ');
    Text.Add('==========================');
    Text.Add('');
    Text.Add('Projet: ' + FProjectName);
    Text.Add('Date de l''audit: ' + FormatDateTime('dd/mm/yyyy', FAuditDate));
    Text.Add('Auditeur: ' + FAuditorName);
    Text.Add('');
    Text.Add('RÉSUMÉ');
    Text.Add('------');
    Text.Add('Nombre total de vulnérabilités: ' + IntToStr(VulnerabilitiesCount));
    Text.Add('Vulnérabilités critiques: ' + IntToStr(CriticalVulnerabilitiesCount));
    Text.Add('');

    if VulnerabilitiesCount > 0 then
    begin
      Text.Add('VULNÉRABILITÉS IDENTIFIÉES');
      Text.Add('--------------------------');
      Text.Add('');

      for I := 0 to High(FVulnerabilities) do
      begin
        case FVulnerabilities[I].Severity of
          vsLow: SeverityStr := 'FAIBLE';
          vsMedium: SeverityStr := 'MOYENNE';
          vsHigh: SeverityStr := 'HAUTE';
          vsCritical: SeverityStr := 'CRITIQUE';
        end;

        Text.Add(FVulnerabilities[I].ID + ': ' + FVulnerabilities[I].Title);
        Text.Add('Sévérité: ' + SeverityStr);
        Text.Add('Emplacement: ' + FVulnerabilities[I].SourceFile +
                ' (ligne ' + IntToStr(FVulnerabilities[I].LineNumber) + ')');
        Text.Add('Description: ' + FVulnerabilities[I].Description);
        Text.Add('Recommandation: ' + FVulnerabilities[I].Recommendation);
        Text.Add('Date de découverte: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', FVulnerabilities[I].DateFound));
        Text.Add('');
        Text.Add('--------------------------------------------------');
        Text.Add('');
      end;
    end
    else
    begin
      Text.Add('Aucune vulnérabilité détectée.');
      Text.Add('');
    end;

    Text.SaveToFile(FileName);
  finally
    Text.Free;
  end;
end;

end.
```

### Création d'un outil d'audit simple

Maintenant que nous avons créé une classe pour gérer les rapports d'audit, nous pouvons l'intégrer dans un outil d'audit simple. Voici un exemple d'application qui combine toutes les techniques présentées :

```pas
program SecurityAuditor;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.RegularExpressions,
  SecurityAuditReport in 'SecurityAuditReport.pas';

type
  TSecurityAuditor = class
  private
    FProjectPath: string;
    FReport: TSecurityAuditReport;

    procedure ScanFiles;
    procedure CheckForSQLInjection(const FileName: string);
    procedure CheckForPasswordStorage(const FileName: string);
    procedure CheckForPathTraversal(const FileName: string);
    procedure CheckForHardcodedSecrets(const FileName: string);
    procedure CheckForInsecureRandomness(const FileName: string);
    procedure CheckForInsecureDirectObjectReference(const FileName: string);
    procedure CheckConfigurationFiles;
  public
    constructor Create(const ProjectPath, ProjectName, AuditorName: string);
    destructor Destroy; override;

    procedure RunAudit;
    procedure SaveReports(const OutputPath: string);
  end;

constructor TSecurityAuditor.Create(const ProjectPath, ProjectName, AuditorName: string);
begin
  inherited Create;
  FProjectPath := ProjectPath;
  FReport := TSecurityAuditReport.Create(ProjectName, AuditorName);
end;

destructor TSecurityAuditor.Destroy;
begin
  FReport.Free;
  inherited;
end;

procedure TSecurityAuditor.RunAudit;
begin
  WriteLn('Début de l''audit de sécurité...');
  WriteLn('Examen du projet dans : ' + FProjectPath);
  WriteLn;

  // Scanner les fichiers source
  ScanFiles;

  // Vérifier les fichiers de configuration
  CheckConfigurationFiles;

  WriteLn;
  WriteLn('Audit terminé !');
  WriteLn('Nombre total de vulnérabilités trouvées : ' + IntToStr(FReport.VulnerabilitiesCount));
  WriteLn('Vulnérabilités critiques : ' + IntToStr(FReport.CriticalVulnerabilitiesCount));
end;

procedure TSecurityAuditor.ScanFiles;
var
  Files: TStringDynArray;
  FileName: string;
  FileExt: string;
  ProcessedCount: Integer;
begin
  WriteLn('Recherche des fichiers source...');

  Files := TDirectory.GetFiles(FProjectPath, '*.pas', TSearchOption.soAllDirectories);
  WriteLn('Nombre de fichiers à examiner : ' + IntToStr(Length(Files)));

  ProcessedCount := 0;
  for FileName in Files do
  begin
    Inc(ProcessedCount);
    if ProcessedCount mod 10 = 0 then
      Write('Progression : ' + IntToStr(Round(ProcessedCount / Length(Files) * 100)) + '%'#13);

    FileExt := ExtractFileExt(FileName).ToLower;

    if FileExt = '.pas' then
    begin
      // Effectuer les vérifications de sécurité
      CheckForSQLInjection(FileName);
      CheckForPasswordStorage(FileName);
      CheckForPathTraversal(FileName);
      CheckForHardcodedSecrets(FileName);
      CheckForInsecureRandomness(FileName);
      CheckForInsecureDirectObjectReference(FileName);
    end;
  end;

  WriteLn('Progression : 100%   ');
  WriteLn(IntToStr(Length(Files)) + ' fichiers analysés.');
end;

procedure TSecurityAuditor.CheckForSQLInjection(const FileName: string);
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  RelativePath: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];

      // Rechercher des signes d'injection SQL potentielle
      if (Pos('SQL.Text', Line) > 0) or
         (Pos('SQL.Add', Line) > 0) or
         (Pos('CommandText', Line) > 0) then
      begin
        if (Pos(' + ', Line) > 0) or
           (Pos(''' +', Line) > 0) or
           (Pos('+ ''', Line) > 0) then
        begin
          // Obtenir le chemin relatif pour un affichage plus propre
          RelativePath := StringReplace(FileName, FProjectPath, '', [rfIgnoreCase]);
          if RelativePath.StartsWith('\') or RelativePath.StartsWith('/') then
            RelativePath := RelativePath.Substring(1);

          FReport.AddVulnerability(
            'Injection SQL potentielle',
            RelativePath,
            I + 1,
            'Concaténation de chaînes dans une requête SQL : ' + Trim(Line),
            'Utilisez des requêtes paramétrées avec ParamByName() ou Parameters[] au lieu de concaténer des chaînes.',
            vsHigh
          );
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSecurityAuditor.CheckForPasswordStorage(const FileName: string);
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  RelativePath: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];

      // Rechercher des signes de stockage de mot de passe en clair
      if (Pos('password', LowerCase(Line)) > 0) or
         (Pos('mot de passe', LowerCase(Line)) > 0) or
         (Pos('passwd', LowerCase(Line)) > 0) then
      begin
        // Vérifier s'il ne s'agit pas d'un mot de passe haché
        if (Pos('hash', LowerCase(Line)) <= 0) and
           (Pos('crypt', LowerCase(Line)) <= 0) and
           (Pos('sha', LowerCase(Line)) <= 0) and
           (Pos('pbkdf2', LowerCase(Line)) <= 0) and
           (Pos('bcrypt', LowerCase(Line)) <= 0) then
        begin
          RelativePath := StringReplace(FileName, FProjectPath, '', [rfIgnoreCase]);
          if RelativePath.StartsWith('\') or RelativePath.StartsWith('/') then
            RelativePath := RelativePath.Substring(1);

          FReport.AddVulnerability(
            'Stockage de mot de passe potentiellement non sécurisé',
            RelativePath,
            I + 1,
            'Possible stockage de mot de passe en clair ou avec un algorithme faible : ' + Trim(Line),
            'Utilisez des algorithmes de hachage sécurisés (PBKDF2, bcrypt, scrypt) avec un sel aléatoire pour stocker les mots de passe.',
            vsHigh
          );
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

// Implémentez les autres méthodes de vérification...

procedure TSecurityAuditor.CheckConfigurationFiles;
var
  ConfigFiles: TStringDynArray;
  FileName: string;
  Lines: TStringList;
  I: Integer;
  Line: string;
  RelativePath: string;
begin
  WriteLn('Vérification des fichiers de configuration...');

  // Rechercher différents types de fichiers de configuration
  ConfigFiles := TDirectory.GetFiles(FProjectPath, '*.ini', TSearchOption.soAllDirectories);

  for FileName in ConfigFiles do
  begin
    RelativePath := StringReplace(FileName, FProjectPath, '', [rfIgnoreCase]);
    if RelativePath.StartsWith('\') or RelativePath.StartsWith('/') then
      RelativePath := RelativePath.Substring(1);

    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(FileName);

      for I := 0 to Lines.Count - 1 do
      begin
        Line := Lines[I];

        // Rechercher des informations sensibles dans les fichiers de configuration
        if (Pos('password=', LowerCase(Line)) > 0) or
           (Pos('pwd=', LowerCase(Line)) > 0) or
           (Pos('connectionstring=', LowerCase(Line)) > 0) or
           (Pos('apikey=', LowerCase(Line)) > 0) or
           (Pos('secret=', LowerCase(Line)) > 0) then
        begin
          FReport.AddVulnerability(
            'Information sensible dans un fichier de configuration',
            RelativePath,
            I + 1,
            'Donnée sensible potentiellement stockée en clair : ' + Trim(Line),
            'Chiffrez les informations sensibles dans les fichiers de configuration ou utilisez un stockage sécurisé comme le Credential Manager de Windows.',
            vsCritical
          );
        end;
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TSecurityAuditor.SaveReports(const OutputPath: string);
var
  BaseName: string;
begin
  if not DirectoryExists(OutputPath) then
    ForceDirectories(OutputPath);

  BaseName := OutputPath + PathDelim + 'security_audit_' + FormatDateTime('yyyymmdd_hhnnss', Now);

  // Sauvegarder dans différents formats
  FReport.SaveToJSON(BaseName + '.json');
  FReport.SaveToHTML(BaseName + '.html');
  FReport.SaveToText(BaseName + '.txt');

  WriteLn('Rapports sauvegardés dans : ' + OutputPath);
end;

// Programme principal
var
  Auditor: TSecurityAuditor;
  ProjectPath, ProjectName, AuditorName, OutputPath: string;
begin
  try
    WriteLn('===== AUDITEUR DE SÉCURITÉ DELPHI =====');
    WriteLn;

    // En production, vous pourriez utiliser des arguments de ligne de commande
    // ou une interface graphique pour ces entrées
    Write('Chemin du projet à auditer : ');
    ReadLn(ProjectPath);

    if not DirectoryExists(ProjectPath) then
    begin
      WriteLn('Erreur : Le répertoire spécifié n''existe pas.');
      Exit;
    end;

    Write('Nom du projet : ');
    ReadLn(ProjectName);

    Write('Nom de l''auditeur : ');
    ReadLn(AuditorName);

    Write('Chemin de sortie pour les rapports : ');
    ReadLn(OutputPath);

    WriteLn;

    Auditor := TSecurityAuditor.Create(ProjectPath, ProjectName, AuditorName);
    try
      Auditor.RunAudit;
      Auditor.SaveReports(OutputPath);
    finally
      Auditor.Free;
    end;

    WriteLn;
    WriteLn('Audit terminé. Appuyez sur Entrée pour quitter...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Erreur : ' + E.Message);
      ReadLn;
    end;
  end;
end.
```

### Interprétation des résultats d'audit

Une fois votre audit terminé, il est important de savoir comment interpréter les résultats et définir les priorités pour les corrections. Voici quelques conseils :

#### 1. Comprendre les niveaux de sévérité

- **Critique** : Vulnérabilités qui permettent une compromission complète du système ou un accès non autorisé à des données sensibles. Elles doivent être corrigées immédiatement.

- **Haute** : Vulnérabilités qui peuvent conduire à une compromission partielle du système ou à une fuite d'informations sensibles. Elles doivent être corrigées rapidement.

- **Moyenne** : Vulnérabilités qui pourraient être exploitées dans certaines circonstances, mais qui nécessitent généralement des conditions spécifiques. Elles doivent être planifiées pour correction.

- **Faible** : Vulnérabilités mineures qui présentent un risque limité ou qui sont difficiles à exploiter. Elles peuvent être abordées lors des cycles de maintenance réguliers.

#### 2. Prioriser les corrections

Voici un exemple de matrice d'évaluation des risques pour prioriser les corrections :

```
Sévérité / Probabilité | Faible    | Moyenne   | Élevée
-----------------------|-----------|-----------|-----------
Critique               | Priorité 2| Priorité 1| Priorité 1
Haute                  | Priorité 3| Priorité 2| Priorité 1
Moyenne                | Priorité 4| Priorité 3| Priorité 2
Faible                 | Priorité 5| Priorité 4| Priorité 3
```

Où :
- **Priorité 1** : Correction immédiate requise
- **Priorité 2** : Correction requise dans les 15 jours
- **Priorité 3** : Correction requise dans les 30 jours
- **Priorité 4** : Correction à planifier dans le prochain cycle
- **Priorité 5** : Correction à considérer lors de la prochaine refonte

#### 3. Plan de correction

Pour chaque vulnérabilité identifiée, établissez un plan de correction qui inclut :

1. **Description du problème** : Comprendre précisément le problème
2. **Impact potentiel** : Évaluer les conséquences possibles
3. **Actions correctives** : Définir les changements nécessaires
4. **Responsable** : Attribuer la tâche à un développeur
5. **Échéance** : Fixer une date limite en fonction de la priorité
6. **Tests de validation** : Définir comment vérifier que la correction est efficace

### Intégration de l'audit de sécurité dans votre processus de développement

Pour maximiser l'efficacité des audits de sécurité, intégrez-les dans votre cycle de développement :

#### 1. Audit lors du cycle de développement

```pas
procedure TDevelopmentProcess.OnPreCommitHook;
var
  SecurityScan: TSecurityAuditor;
  CurrentBranch, DeveloperName: string;
begin
  // Obtenir les informations du contexte actuel
  CurrentBranch := GetCurrentGitBranch;
  DeveloperName := GetCurrentDeveloper;

  // Exécuter un scan de sécurité rapide avant le commit
  SecurityScan := TSecurityAuditor.Create(GetProjectPath, ProjectName, DeveloperName);
  try
    SecurityScan.SetScanMode(smQuick); // Mode rapide pour les commits
    SecurityScan.RunAudit;

    // Si des problèmes critiques sont trouvés, empêcher le commit
    if SecurityScan.Report.CriticalVulnerabilitiesCount > 0 then
    begin
      ShowMessage('Le commit a été bloqué car des vulnérabilités critiques ont été détectées.' +
                  'Veuillez corriger ces problèmes avant de réessayer.');
      Abort;
    end;

    // Sauvegarder le rapport pour référence
    SecurityScan.SaveReports(GetReportsDirectory + CurrentBranch);
  finally
    SecurityScan.Free;
  end;
end;
```

#### 2. Intégration continue

Ajoutez des vérifications de sécurité automatisées à votre pipeline CI/CD :

```yaml
# Exemple de configuration pour un pipeline CI/CD fictif
stages:
  - build
  - test
  - security_audit
  - deploy

security_audit:
  stage: security_audit
  script:
    - echo "Exécution de l'audit de sécurité..."
    - SecurityAuditor.exe --project $CI_PROJECT_DIR --output $CI_PROJECT_DIR/security_reports --mode full
  artifacts:
    paths:
      - security_reports/
    expire_in: 1 week
  rules:
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"
    - if: $CI_COMMIT_BRANCH == "main"
    - if: $CI_COMMIT_BRANCH == "develop"
```

#### 3. Audits de sécurité périodiques

Planifiez des audits de sécurité complets à intervalles réguliers :

```pas
procedure TProjectScheduler.ScheduleSecurityAudits;
var
  Task: TScheduledTask;
begin
  // Créer une tâche hebdomadaire pour les audits de sécurité
  Task := TScheduledTask.Create;
  Task.Name := 'AuditSécuritéHebdomadaire';
  Task.Description := 'Exécute un audit de sécurité complet du projet';
  Task.Frequency := tfWeekly;
  Task.DayOfWeek := 1; // Lundi
  Task.StartTime := EncodeTime(2, 0, 0, 0); // 2h00 du matin

  Task.Command := 'SecurityAuditor.exe';
  Task.Parameters := '--project "' + GetProjectPath + '" ' +
                    '--output "' + GetReportsDirectory + '\weekly" ' +
                    '--mode full ' +
                    '--notify security_team@entreprise.com';

  ScheduleManager.AddTask(Task);

  // Créer une tâche mensuelle pour un audit plus approfondi
  Task := TScheduledTask.Create;
  Task.Name := 'AuditSécuritéMensuel';
  Task.Description := 'Exécute un audit de sécurité approfondi avec tests dynamiques';
  Task.Frequency := tfMonthly;
  Task.DayOfMonth := 1; // 1er du mois
  Task.StartTime := EncodeTime(1, 0, 0, 0); // 1h00 du matin

  Task.Command := 'SecurityAuditor.exe';
  Task.Parameters := '--project "' + GetProjectPath + '" ' +
                    '--output "' + GetReportsDirectory + '\monthly" ' +
                    '--mode deep ' +
                    '--dynamic_tests ' +
                    '--notify security_team@entreprise.com,management@entreprise.com';

  ScheduleManager.AddTask(Task);
end;
```

### Outils d'audit de sécurité pour Delphi

En plus de l'outil simple que nous avons créé, voici quelques solutions plus complètes pour l'audit de sécurité de vos applications Delphi :

#### 1. CodeGuard

Un outil fictif qui illustre ce à quoi pourrait ressembler un outil commercial :

- Analyse statique du code Delphi
- Détection des vulnérabilités courantes
- Intégration avec l'IDE Delphi
- Génération de rapports détaillés
- Suggestions de correction automatisées

#### 2. DelphiScan

Un autre outil fictif pour scanner les applications Delphi compilées :

- Analyse des binaires Delphi
- Détection des bibliothèques tierces vulnérables
- Audit des configurations de déploiement
- Vérification des droits d'accès
- Analyse des communications réseau

#### 3. Outils génériques adaptables

Des outils réels qui, bien que non spécifiques à Delphi, peuvent être adaptés :

- **SonarQube** : Un outil d'analyse de code statique qui peut être configuré pour analyser le code Delphi
- **OWASP ZAP** : Pour tester les applications web créées avec Delphi
- **Dependency-Check** : Pour vérifier les vulnérabilités dans les bibliothèques tierces
- **BurpSuite** : Pour tester la sécurité des communications réseau

### Bonnes pratiques pour l'audit de sécurité

1. **Combinez analyses automatiques et manuelles** : Les outils automatisés ne détectent pas toutes les vulnérabilités. Une revue manuelle est souvent nécessaire pour les problèmes complexes.

2. **Documentez tout** : Chaque vulnérabilité, test et correction doit être documenté pour référence future.

3. **Testez dans un environnement représentatif** : Assurez-vous que votre environnement de test reflète fidèlement l'environnement de production.

4. **Revérifiez après les corrections** : Testez à nouveau après avoir corrigé une vulnérabilité pour s'assurer que la correction est efficace.

5. **Impliquez l'équipe de développement** : Les audits ne doivent pas être perçus comme punitifs, mais comme éducatifs. Impliquez les développeurs dans le processus.

6. **Restez à jour** : Les vulnérabilités évoluent constamment. Mettez régulièrement à jour vos connaissances et outils.

7. **Adoptez une approche d'amélioration continue** : Utilisez les résultats d'audit pour améliorer continuellement vos pratiques de développement.

### Exemple de rapport d'audit

Voici un exemple de structure pour un rapport d'audit de sécurité complet :

1. **Résumé exécutif**
   - Aperçu des résultats
   - Évaluation globale du risque
   - Recommandations principales

2. **Méthodologie**
   - Outils utilisés
   - Techniques appliquées
   - Périmètre de l'audit

3. **Résultats détaillés**
   - Vulnérabilités par catégorie
   - Description technique
   - Impact potentiel
   - Étapes de reproduction
   - Recommandations de correction

4. **Plan de remédiation**
   - Priorisation des corrections
   - Assignation des responsabilités
   - Échéancier

5. **Annexes**
   - Captures d'écran et preuves
   - Détails techniques supplémentaires
   - Références aux bonnes pratiques et standards

### Conclusion

L'audit de sécurité est une composante essentielle du cycle de développement d'applications sécurisées. En intégrant des audits réguliers dans votre processus de développement Delphi, vous pouvez identifier et corriger les vulnérabilités avant qu'elles ne soient exploitées.

L'outil simple présenté dans ce chapitre vous offre un point de départ pour mettre en place vos propres audits. Pour les applications critiques ou sensibles, envisagez de compléter cette approche par des audits professionnels externes.

Rappelez-vous que la sécurité est un processus continu, pas un état final. Les menaces évoluent constamment, et votre approche de la sécurité doit évoluer en conséquence.

### Exercices pratiques

1. Créez une version simplifiée de l'outil d'audit présenté dans ce chapitre et exécutez-le sur l'un de vos projets Delphi.

2. Développez un module de détection spécifique pour une vulnérabilité qui vous préoccupe particulièrement dans vos applications.

3. Configurez un hook de pré-commit Git qui vérifie les problèmes de sécurité courants avant chaque commit.

4. Modifiez l'outil pour générer des rapports dans un format spécifique requis par votre organisation.

5. Pour les plus avancés : Intégrez l'outil d'audit dans votre IDE Delphi en créant un expert ou un plugin.
