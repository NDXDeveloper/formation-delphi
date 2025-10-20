🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.6 Intégration avec des frameworks JavaScript

## Introduction

Les frameworks JavaScript modernes comme **React**, **Vue.js** et **Angular** ont révolutionné le développement d'interfaces web. Ils permettent de créer des applications web interactives, réactives et performantes. La bonne nouvelle ? Vous pouvez combiner la puissance de Delphi pour le backend avec ces frameworks pour le frontend !

**L'idée centrale :** Delphi gère les données et la logique métier (backend), tandis qu'un framework JavaScript crée une interface utilisateur moderne (frontend).

```
┌─────────────────────────────────┐
│  Frontend (Navigateur)          │
│  ┌───────────────────────────┐  │
│  │  React / Vue / Angular    │  │
│  │  (Interface utilisateur)  │  │
│  └──────────┬────────────────┘  │
└─────────────┼───────────────────┘
              │
              │ API REST (JSON)
              │ HTTP/HTTPS
              │
┌─────────────┴───────────────────┐
│  Backend (Serveur)              │
│  ┌───────────────────────────┐  │
│  │  Delphi / Object Pascal   │  │
│  │  (Logique métier + BDD)   │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
```

## Pourquoi cette combinaison ?

### Avantages de Delphi côté backend

✅ **Performance native** - Code compilé rapide et efficace
✅ **Accès base de données** - FireDAC puissant et mature
✅ **Logique métier sécurisée** - Code protégé côté serveur
✅ **Expertise existante** - Capitaliser sur vos compétences
✅ **Stabilité éprouvée** - Plateforme fiable pour applications critiques

### Avantages des frameworks JavaScript côté frontend

✅ **Interface moderne** - UX/UI à la pointe
✅ **Réactivité** - Mises à jour instantanées sans rechargement
✅ **Écosystème riche** - Milliers de composants prêts à l'emploi
✅ **Communauté massive** - Support et ressources abondantes
✅ **Standards web** - Technologies répandues et bien documentées

### Le meilleur des deux mondes

Cette architecture vous permet de :
- Utiliser Delphi pour ce qu'il fait de mieux : données et logique métier
- Profiter des frameworks JavaScript pour créer des interfaces exceptionnelles
- Évoluer indépendamment frontend et backend
- Recruter facilement des développeurs frontend JavaScript

## Architectures possibles

### Architecture 1 : Séparation complète (recommandée)

```
┌──────────────────┐
│  Application JS  │ (React/Vue/Angular)
│  (Frontend)      │ Port 3000 ou 4200
└────────┬─────────┘
         │
         │ Appels API REST
         │
┌────────┴─────────┐
│  API Delphi      │ (Horse, RAD Server)
│  (Backend)       │ Port 9000
└────────┬─────────┘
         │
┌────────┴─────────┐
│  Base de données │
└──────────────────┘
```

**Avantages :**
- Séparation claire des responsabilités
- Développement parallèle possible
- Déploiement indépendant
- Scaling horizontal facilité

### Architecture 2 : Serveur unique avec fichiers statiques

```
┌─────────────────────────────────┐
│  Serveur Web (Delphi)           │
│  ┌───────────────────────────┐  │
│  │  API REST    /api/*       │  │
│  ├───────────────────────────┤  │
│  │  Fichiers JS  /           │  │
│  │  (build React/Vue)        │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
```

**Avantages :**
- Déploiement simplifié
- Un seul serveur à gérer
- Pas de problème CORS
- Configuration réseau simplifiée

## Les frameworks JavaScript populaires

### React

**Créé par :** Facebook/Meta
**Type :** Bibliothèque UI (pas un framework complet)
**Philosophie :** Composants réutilisables

**Exemple simple React :**
```javascript
// Composant React qui affiche une liste de clients
import React, { useState, useEffect } from 'react';

function ClientsList() {
  const [clients, setClients] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    // Appel à l'API Delphi
    fetch('http://localhost:9000/api/clients')
      .then(response => response.json())
      .then(data => {
        setClients(data);
        setLoading(false);
      })
      .catch(error => {
        console.error('Erreur:', error);
        setLoading(false);
      });
  }, []);

  if (loading) return <div>Chargement...</div>;

  return (
    <div>
      <h1>Liste des clients</h1>
      <table>
        <thead>
          <tr>
            <th>Nom</th>
            <th>Prénom</th>
            <th>Email</th>
          </tr>
        </thead>
        <tbody>
          {clients.map(client => (
            <tr key={client.id}>
              <td>{client.nom}</td>
              <td>{client.prenom}</td>
              <td>{client.email}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export default ClientsList;
```

**Quand utiliser React :**
- Interface complexe avec beaucoup d'interactions
- Besoin de flexibilité maximale
- Grande communauté et écosystème
- Nombreux développeurs disponibles sur le marché

### Vue.js

**Créé par :** Evan You
**Type :** Framework progressif
**Philosophie :** Simple et intuitif

**Exemple simple Vue.js :**
```vue
<template>
  <div>
    <h1>Liste des clients</h1>
    <div v-if="loading">Chargement...</div>
    <table v-else>
      <thead>
        <tr>
          <th>Nom</th>
          <th>Prénom</th>
          <th>Email</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="client in clients" :key="client.id">
          <td>{{ client.nom }}</td>
          <td>{{ client.prenom }}</td>
          <td>{{ client.email }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
export default {
  name: 'ClientsList',
  data() {
    return {
      clients: [],
      loading: true
    };
  },
  mounted() {
    // Appel à l'API Delphi
    fetch('http://localhost:9000/api/clients')
      .then(response => response.json())
      .then(data => {
        this.clients = data;
        this.loading = false;
      })
      .catch(error => {
        console.error('Erreur:', error);
        this.loading = false;
      });
  }
};
</script>
```

**Quand utiliser Vue.js :**
- Courbe d'apprentissage douce
- Migration progressive d'application existante
- Documentation excellente en français
- Bonne productivité dès le départ

### Angular

**Créé par :** Google
**Type :** Framework complet
**Philosophie :** Structure et conventions strictes

**Exemple simple Angular :**
```typescript
// clients.component.ts
import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';

interface Client {
  id: number;
  nom: string;
  prenom: string;
  email: string;
}

@Component({
  selector: 'app-clients',
  templateUrl: './clients.component.html'
})
export class ClientsComponent implements OnInit {
  clients: Client[] = [];
  loading = true;

  constructor(private http: HttpClient) {}

  ngOnInit() {
    // Appel à l'API Delphi
    this.http.get<Client[]>('http://localhost:9000/api/clients')
      .subscribe({
        next: (data) => {
          this.clients = data;
          this.loading = false;
        },
        error: (error) => {
          console.error('Erreur:', error);
          this.loading = false;
        }
      });
  }
}
```

```html
<!-- clients.component.html -->
<div>
  <h1>Liste des clients</h1>
  <div *ngIf="loading">Chargement...</div>
  <table *ngIf="!loading">
    <thead>
      <tr>
        <th>Nom</th>
        <th>Prénom</th>
        <th>Email</th>
      </tr>
    </thead>
    <tbody>
      <tr *ngFor="let client of clients">
        <td>{{ client.nom }}</td>
        <td>{{ client.prenom }}</td>
        <td>{{ client.email }}</td>
      </tr>
    </tbody>
  </table>
</div>
```

**Quand utiliser Angular :**
- Applications d'entreprise complexes
- Équipe habituée à TypeScript
- Besoin de structure stricte
- Applications à grande échelle

## Configuration de l'API Delphi

### Serveur Horse avec CORS

Pour que JavaScript puisse communiquer avec votre API Delphi, vous devez configurer CORS (Cross-Origin Resource Sharing).

```pascal
program APIServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Jhonson,  // Pour JSON
  Horse.CORS,     // Pour CORS
  Horse.HandleException, // Gestion erreurs
  System.JSON,
  FireDAC.Comp.Client,
  DataModuleUnit in 'DataModuleUnit.pas';

var
  App: THorse;

// Route GET /api/clients
procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMData.Connection;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients ORDER BY nom';
    Query.Open;

    JSONArray := TJSONArray.Create;
    try
      while not Query.Eof do
      begin
        JSONObject := TJSONObject.Create;
        JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
        JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
        JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
        JSONObject.AddPair('email', Query.FieldByName('email').AsString);
        JSONArray.Add(JSONObject);
        Query.Next;
      end;

      Res.Send<TJSONArray>(JSONArray);
    finally
      // JSONArray sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

// Route GET /api/clients/:id
procedure GetClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  JSONObject: TJSONObject;
  ClientID: Integer;
begin
  ClientID := StrToInt(Req.Params['id']);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMData.Connection;
    Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ClientID;
    Query.Open;

    if Query.IsEmpty then
    begin
      Res.Status(404).Send('Client non trouvé');
      Exit;
    end;

    JSONObject := TJSONObject.Create;
    try
      JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
      JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
      JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
      JSONObject.AddPair('email', Query.FieldByName('email').AsString);
      JSONObject.AddPair('telephone', Query.FieldByName('telephone').AsString);

      Res.Send<TJSONObject>(JSONObject);
    finally
      // JSONObject sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

// Route POST /api/clients
procedure CreateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  Body: TJSONObject;
  Response: TJSONObject;
  NewID: Integer;
begin
  Body := Req.Body<TJSONObject>;

  // Validation basique
  if not Body.TryGetValue<string>('nom').Trim.IsEmpty and
     not Body.TryGetValue<string>('prenom').Trim.IsEmpty and
     not Body.TryGetValue<string>('email').Trim.IsEmpty then
  begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DMData.Connection;
      Query.SQL.Text :=
        'INSERT INTO clients (nom, prenom, email, telephone) ' +
        'VALUES (:nom, :prenom, :email, :telephone)';
      Query.ParamByName('nom').AsString := Body.GetValue<string>('nom');
      Query.ParamByName('prenom').AsString := Body.GetValue<string>('prenom');
      Query.ParamByName('email').AsString := Body.GetValue<string>('email');
      Query.ParamByName('telephone').AsString := Body.GetValue<string>('telephone', '');
      Query.ExecSQL;

      // Récupérer l'ID du nouveau client
      Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
      Query.Open;
      NewID := Query.FieldByName('id').AsInteger;

      Response := TJSONObject.Create;
      try
        Response.AddPair('success', TJSONBool.Create(True));
        Response.AddPair('id', TJSONNumber.Create(NewID));
        Response.AddPair('message', 'Client créé avec succès');

        Res.Status(201).Send<TJSONObject>(Response);
      finally
        // Response sera libéré automatiquement
      end;
    finally
      Query.Free;
    end;
  end
  else
  begin
    Response := TJSONObject.Create;
    Response.AddPair('success', TJSONBool.Create(False));
    Response.AddPair('message', 'Données invalides');
    Res.Status(400).Send<TJSONObject>(Response);
  end;
end;

// Route PUT /api/clients/:id
procedure UpdateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  Body: TJSONObject;
  Response: TJSONObject;
  ClientID: Integer;
begin
  ClientID := StrToInt(Req.Params['id']);
  Body := Req.Body<TJSONObject>;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMData.Connection;
    Query.SQL.Text :=
      'UPDATE clients SET nom = :nom, prenom = :prenom, ' +
      'email = :email, telephone = :telephone WHERE id = :id';
    Query.ParamByName('id').AsInteger := ClientID;
    Query.ParamByName('nom').AsString := Body.GetValue<string>('nom');
    Query.ParamByName('prenom').AsString := Body.GetValue<string>('prenom');
    Query.ParamByName('email').AsString := Body.GetValue<string>('email');
    Query.ParamByName('telephone').AsString := Body.GetValue<string>('telephone', '');
    Query.ExecSQL;

    Response := TJSONObject.Create;
    try
      Response.AddPair('success', TJSONBool.Create(True));
      Response.AddPair('message', 'Client modifié avec succès');

      Res.Send<TJSONObject>(Response);
    finally
      // Response sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

// Route DELETE /api/clients/:id
procedure DeleteClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  Response: TJSONObject;
  ClientID: Integer;
begin
  ClientID := StrToInt(Req.Params['id']);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMData.Connection;
    Query.SQL.Text := 'DELETE FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ClientID;
    Query.ExecSQL;

    Response := TJSONObject.Create;
    try
      Response.AddPair('success', TJSONBool.Create(True));
      Response.AddPair('message', 'Client supprimé avec succès');

      Res.Status(200).Send<TJSONObject>(Response);
    finally
      // Response sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

begin
  App := THorse.Create;

  // Middlewares
  App.Use(Jhonson);           // Support JSON
  App.Use(CORS);              // Support CORS
  App.Use(HandleException);   // Gestion des erreurs

  // Routes API
  App.Get('/api/clients', GetClients);
  App.Get('/api/clients/:id', GetClient);
  App.Post('/api/clients', CreateClient);
  App.Put('/api/clients/:id', UpdateClient);
  App.Delete('/api/clients/:id', DeleteClient);

  App.Listen(9000);

  Writeln('API Delphi démarrée sur http://localhost:9000');
  Writeln('Endpoints disponibles :');
  Writeln('  GET    /api/clients');
  Writeln('  GET    /api/clients/:id');
  Writeln('  POST   /api/clients');
  Writeln('  PUT    /api/clients/:id');
  Writeln('  DELETE /api/clients/:id');
  Writeln('');
  Writeln('Appuyez sur Entrée pour arrêter...');

  Readln;
end.
```

## Application React complète avec API Delphi

### Structure du projet React

```
client-app/
├── public/
│   └── index.html
├── src/
│   ├── components/
│   │   ├── ClientsList.js
│   │   ├── ClientForm.js
│   │   └── ClientDetails.js
│   ├── services/
│   │   └── api.js
│   ├── App.js
│   └── index.js
└── package.json
```

### Service API (api.js)

```javascript
// src/services/api.js
const API_BASE_URL = 'http://localhost:9000/api';

// Configuration de base pour fetch
const defaultOptions = {
  headers: {
    'Content-Type': 'application/json',
  },
};

// Récupérer tous les clients
export async function getClients() {
  try {
    const response = await fetch(`${API_BASE_URL}/clients`, defaultOptions);
    if (!response.ok) {
      throw new Error('Erreur lors de la récupération des clients');
    }
    return await response.json();
  } catch (error) {
    console.error('Erreur:', error);
    throw error;
  }
}

// Récupérer un client par ID
export async function getClient(id) {
  try {
    const response = await fetch(`${API_BASE_URL}/clients/${id}`, defaultOptions);
    if (!response.ok) {
      throw new Error('Client non trouvé');
    }
    return await response.json();
  } catch (error) {
    console.error('Erreur:', error);
    throw error;
  }
}

// Créer un nouveau client
export async function createClient(clientData) {
  try {
    const response = await fetch(`${API_BASE_URL}/clients`, {
      ...defaultOptions,
      method: 'POST',
      body: JSON.stringify(clientData),
    });
    if (!response.ok) {
      throw new Error('Erreur lors de la création du client');
    }
    return await response.json();
  } catch (error) {
    console.error('Erreur:', error);
    throw error;
  }
}

// Modifier un client
export async function updateClient(id, clientData) {
  try {
    const response = await fetch(`${API_BASE_URL}/clients/${id}`, {
      ...defaultOptions,
      method: 'PUT',
      body: JSON.stringify(clientData),
    });
    if (!response.ok) {
      throw new Error('Erreur lors de la modification du client');
    }
    return await response.json();
  } catch (error) {
    console.error('Erreur:', error);
    throw error;
  }
}

// Supprimer un client
export async function deleteClient(id) {
  try {
    const response = await fetch(`${API_BASE_URL}/clients/${id}`, {
      ...defaultOptions,
      method: 'DELETE',
    });
    if (!response.ok) {
      throw new Error('Erreur lors de la suppression du client');
    }
    return await response.json();
  } catch (error) {
    console.error('Erreur:', error);
    throw error;
  }
}
```

### Composant Liste des clients

```javascript
// src/components/ClientsList.js
import React, { useState, useEffect } from 'react';
import { getClients, deleteClient } from '../services/api';

function ClientsList({ onEdit }) {
  const [clients, setClients] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  // Charger les clients au montage du composant
  useEffect(() => {
    loadClients();
  }, []);

  async function loadClients() {
    try {
      setLoading(true);
      const data = await getClients();
      setClients(data);
      setError(null);
    } catch (err) {
      setError('Impossible de charger les clients');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }

  async function handleDelete(id) {
    if (window.confirm('Êtes-vous sûr de vouloir supprimer ce client ?')) {
      try {
        await deleteClient(id);
        // Recharger la liste après suppression
        loadClients();
      } catch (err) {
        alert('Erreur lors de la suppression');
        console.error(err);
      }
    }
  }

  if (loading) {
    return <div className="loading">Chargement des clients...</div>;
  }

  if (error) {
    return <div className="error">{error}</div>;
  }

  return (
    <div className="clients-list">
      <h2>Liste des clients ({clients.length})</h2>

      {clients.length === 0 ? (
        <p>Aucun client trouvé.</p>
      ) : (
        <table>
          <thead>
            <tr>
              <th>Nom</th>
              <th>Prénom</th>
              <th>Email</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody>
            {clients.map(client => (
              <tr key={client.id}>
                <td>{client.nom}</td>
                <td>{client.prenom}</td>
                <td>{client.email}</td>
                <td>
                  <button onClick={() => onEdit(client)}>
                    ✏️ Modifier
                  </button>
                  <button onClick={() => handleDelete(client.id)}>
                    🗑️ Supprimer
                  </button>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  );
}

export default ClientsList;
```

### Composant Formulaire client

```javascript
// src/components/ClientForm.js
import React, { useState, useEffect } from 'react';
import { createClient, updateClient } from '../services/api';

function ClientForm({ client, onSave, onCancel }) {
  const [formData, setFormData] = useState({
    nom: '',
    prenom: '',
    email: '',
    telephone: '',
  });
  const [errors, setErrors] = useState({});
  const [saving, setSaving] = useState(false);

  // Pré-remplir le formulaire si on modifie un client existant
  useEffect(() => {
    if (client) {
      setFormData({
        nom: client.nom || '',
        prenom: client.prenom || '',
        email: client.email || '',
        telephone: client.telephone || '',
      });
    }
  }, [client]);

  function handleChange(e) {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
    // Effacer l'erreur du champ modifié
    if (errors[name]) {
      setErrors(prev => ({
        ...prev,
        [name]: null
      }));
    }
  }

  function validate() {
    const newErrors = {};

    if (!formData.nom.trim()) {
      newErrors.nom = 'Le nom est obligatoire';
    }
    if (!formData.prenom.trim()) {
      newErrors.prenom = 'Le prénom est obligatoire';
    }
    if (!formData.email.trim()) {
      newErrors.email = 'L\'email est obligatoire';
    } else if (!/\S+@\S+\.\S+/.test(formData.email)) {
      newErrors.email = 'Email invalide';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }

  async function handleSubmit(e) {
    e.preventDefault();

    if (!validate()) {
      return;
    }

    try {
      setSaving(true);

      if (client) {
        // Modification
        await updateClient(client.id, formData);
      } else {
        // Création
        await createClient(formData);
      }

      onSave();
    } catch (err) {
      alert('Erreur lors de l\'enregistrement');
      console.error(err);
    } finally {
      setSaving(false);
    }
  }

  return (
    <div className="client-form">
      <h2>{client ? 'Modifier le client' : 'Nouveau client'}</h2>

      <form onSubmit={handleSubmit}>
        <div className="form-group">
          <label htmlFor="nom">Nom *</label>
          <input
            type="text"
            id="nom"
            name="nom"
            value={formData.nom}
            onChange={handleChange}
            className={errors.nom ? 'error' : ''}
          />
          {errors.nom && <span className="error-message">{errors.nom}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="prenom">Prénom *</label>
          <input
            type="text"
            id="prenom"
            name="prenom"
            value={formData.prenom}
            onChange={handleChange}
            className={errors.prenom ? 'error' : ''}
          />
          {errors.prenom && <span className="error-message">{errors.prenom}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="email">Email *</label>
          <input
            type="email"
            id="email"
            name="email"
            value={formData.email}
            onChange={handleChange}
            className={errors.email ? 'error' : ''}
          />
          {errors.email && <span className="error-message">{errors.email}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="telephone">Téléphone</label>
          <input
            type="tel"
            id="telephone"
            name="telephone"
            value={formData.telephone}
            onChange={handleChange}
          />
        </div>

        <div className="form-actions">
          <button type="submit" disabled={saving}>
            {saving ? 'Enregistrement...' : 'Enregistrer'}
          </button>
          <button type="button" onClick={onCancel}>
            Annuler
          </button>
        </div>
      </form>
    </div>
  );
}

export default ClientForm;
```

### Application principale

```javascript
// src/App.js
import React, { useState } from 'react';
import ClientsList from './components/ClientsList';
import ClientForm from './components/ClientForm';
import './App.css';

function App() {
  const [view, setView] = useState('list'); // 'list' ou 'form'
  const [selectedClient, setSelectedClient] = useState(null);

  function handleNewClient() {
    setSelectedClient(null);
    setView('form');
  }

  function handleEditClient(client) {
    setSelectedClient(client);
    setView('form');
  }

  function handleSaveComplete() {
    setView('list');
    setSelectedClient(null);
  }

  function handleCancel() {
    setView('list');
    setSelectedClient(null);
  }

  return (
    <div className="App">
      <header>
        <h1>Gestion des Clients</h1>
        {view === 'list' && (
          <button onClick={handleNewClient}>
            ➕ Nouveau client
          </button>
        )}
      </header>

      <main>
        {view === 'list' ? (
          <ClientsList onEdit={handleEditClient} />
        ) : (
          <ClientForm
            client={selectedClient}
            onSave={handleSaveComplete}
            onCancel={handleCancel}
          />
        )}
      </main>

      <footer>
        <p>Application React + API Delphi</p>
      </footer>
    </div>
  );
}

export default App;
```

## Authentification JWT

### Côté Delphi - Génération du token

```pascal
uses
  Horse, Horse.JWT, JOSE.Core.JWT, JOSE.Core.Builder, System.DateUtils;

procedure Login(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Body: TJSONObject;
  Username, Password: string;
  JWT: TJWT;
  Token: string;
  Response: TJSONObject;
begin
  Body := Req.Body<TJSONObject>;
  Username := Body.GetValue<string>('username');
  Password := Body.GetValue<string>('password');

  // Vérifier les identifiants (à implémenter selon votre logique)
  if VerifyCredentials(Username, Password) then
  begin
    JWT := TJWT.Create;
    try
      JWT.Claims.Subject := Username;
      JWT.Claims.Expiration := IncHour(Now, 24); // Expire dans 24h
      JWT.Claims.SetClaimOfType<string>('role', GetUserRole(Username));

      Token := TJOSE.SHA256CompactToken('SECRET_KEY_CHANGE_ME', JWT);

      Response := TJSONObject.Create;
      try
        Response.AddPair('success', TJSONBool.Create(True));
        Response.AddPair('token', Token);
        Response.AddPair('username', Username);
        Response.AddPair('expiresIn', '86400'); // 24h en secondes

        Res.Send<TJSONObject>(Response);
      finally
        // Response sera libéré automatiquement
      end;
    finally
      JWT.Free;
    end;
  end
  else
  begin
    Response := TJSONObject.Create;
    Response.AddPair('success', TJSONBool.Create(False));
    Response.AddPair('message', 'Identifiants invalides');
    Res.Status(401).Send<TJSONObject>(Response);
  end;
end;

// Middleware d'authentification
procedure AuthMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Token: string;
  JWT: TJWT;
begin
  Token := Req.Headers['Authorization'];

  if Token.IsEmpty then
  begin
    Res.Status(401).Send('Token manquant');
    Exit;
  end;

  // Retirer "Bearer " du token
  if Token.StartsWith('Bearer ') then
    Token := Token.Substring(7);

  try
    JWT := TJOSE.Verify('SECRET_KEY_CHANGE_ME', Token);
    try
      // Token valide, continuer
      Next;
    finally
      JWT.Free;
    end;
  except
    Res.Status(401).Send('Token invalide ou expiré');
  end;
end;

// Configuration des routes
begin
  // Route de login (publique)
  THorse.Post('/api/login', Login);

  // Routes protégées
  THorse.AddCallback(AuthMiddleware)
    .Get('/api/clients', GetClients)
    .Post('/api/clients', CreateClient);
end;
```

### Côté React - Gestion du token

```javascript
// src/services/auth.js
const TOKEN_KEY = 'auth_token';

export function saveToken(token) {
  localStorage.setItem(TOKEN_KEY, token);
}

export function getToken() {
  return localStorage.getItem(TOKEN_KEY);
}

export function removeToken() {
  localStorage.removeItem(TOKEN_KEY);
}

export function isAuthenticated() {
  return !!getToken();
}

// Service API modifié avec authentification
// src/services/api.js
import { getToken } from './auth';

function getHeaders() {
  const headers = {
    'Content-Type': 'application/json',
  };

  const token = getToken();
  if (token) {
    headers['Authorization'] = `Bearer ${token}`;
  }

  return headers;
}

export async function login(username, password) {
  const response = await fetch(`${API_BASE_URL}/login`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ username, password }),
  });

  if (!response.ok) {
    throw new Error('Échec de la connexion');
  }

  return await response.json();
}

export async function getClients() {
  const response = await fetch(`${API_BASE_URL}/clients`, {
    headers: getHeaders(),
  });

  if (response.status === 401) {
    // Token expiré ou invalide
    throw new Error('NON_AUTHENTIFIE');
  }

  if (!response.ok) {
    throw new Error('Erreur lors de la récupération des clients');
  }

  return await response.json();
}

// Composant Login
// src/components/Login.js
import React, { useState } from 'react';
import { login } from '../services/api';
import { saveToken } from '../services/auth';

function Login({ onLoginSuccess }) {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [error, setError] = useState('');
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e) {
    e.preventDefault();
    setError('');
    setLoading(true);

    try {
      const response = await login(username, password);

      if (response.success) {
        saveToken(response.token);
        onLoginSuccess(response.username);
      } else {
        setError(response.message || 'Échec de la connexion');
      }
    } catch (err) {
      setError('Erreur de connexion');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="login-container">
      <h2>Connexion</h2>
      <form onSubmit={handleSubmit}>
        {error && <div className="error-message">{error}</div>}

        <div className="form-group">
          <label>Nom d'utilisateur</label>
          <input
            type="text"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            required
          />
        </div>

        <div className="form-group">
          <label>Mot de passe</label>
          <input
            type="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            required
          />
        </div>

        <button type="submit" disabled={loading}>
          {loading ? 'Connexion...' : 'Se connecter'}
        </button>
      </form>
    </div>
  );
}

export default Login;
```

## Gestion des erreurs réseau

### Intercepteur d'erreurs côté React

```javascript
// src/services/api.js
async function handleResponse(response) {
  if (!response.ok) {
    // Tenter de lire le message d'erreur du serveur
    let errorMessage = 'Une erreur est survenue';

    try {
      const errorData = await response.json();
      errorMessage = errorData.message || errorMessage;
    } catch {
      // Le serveur n'a pas renvoyé de JSON
    }

    const error = new Error(errorMessage);
    error.status = response.status;
    throw error;
  }

  return response.json();
}

export async function getClients() {
  try {
    const response = await fetch(`${API_BASE_URL}/clients`, {
      headers: getHeaders(),
    });
    return await handleResponse(response);
  } catch (error) {
    if (error.status === 401) {
      // Rediriger vers login
      window.location.href = '/login';
    }
    throw error;
  }
}

// Hook personnalisé pour gérer les erreurs
// src/hooks/useApiError.js
import { useState } from 'react';

export function useApiError() {
  const [error, setError] = useState(null);

  function handleError(err) {
    console.error('Erreur API:', err);

    let message = 'Une erreur est survenue';

    if (err.status === 401) {
      message = 'Vous devez vous connecter';
    } else if (err.status === 403) {
      message = 'Accès refusé';
    } else if (err.status === 404) {
      message = 'Ressource non trouvée';
    } else if (err.status >= 500) {
      message = 'Erreur serveur. Veuillez réessayer plus tard.';
    } else if (err.message) {
      message = err.message;
    }

    setError(message);
  }

  function clearError() {
    setError(null);
  }

  return { error, handleError, clearError };
}

// Utilisation
function ClientsList() {
  const [clients, setClients] = useState([]);
  const { error, handleError, clearError } = useApiError();

  async function loadClients() {
    try {
      clearError();
      const data = await getClients();
      setClients(data);
    } catch (err) {
      handleError(err);
    }
  }

  return (
    <div>
      {error && <div className="error-banner">{error}</div>}
      {/* ... reste du composant */}
    </div>
  );
}
```

## Déploiement

### Option 1 : Déploiement séparé

**Backend Delphi :**
- Serveur dédié (VPS, cloud)
- URL : `https://api.monapp.com`
- Port 9000 ou 443 (HTTPS)

**Frontend React :**
- Hébergement statique (Netlify, Vercel, GitHub Pages)
- URL : `https://app.monapp.com`
- Appels API vers le backend

### Option 2 : Déploiement combiné

**Serveur unique Delphi servant :**
- API REST : `/api/*`
- Fichiers statiques React : `/*`

```pascal
// Servir les fichiers statiques React
procedure ServeStaticFiles(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  FileName: string;
  FilePath: string;
begin
  FileName := Req.PathInfo;
  if FileName = '/' then
    FileName := '/index.html';

  FilePath := TPath.Combine('public', FileName);

  if TFile.Exists(FilePath) then
    Res.SendFile(FilePath)
  else
    Next; // Passer au handler suivant
end;

begin
  // Routes API
  THorse.Get('/api/clients', GetClients);

  // Fichiers statiques (après les routes API)
  THorse.Get('/*', ServeStaticFiles);

  THorse.Listen(9000);
end;
```

## Bonnes pratiques

### 1. Variables d'environnement

**React (.env) :**
```
REACT_APP_API_URL=http://localhost:9000/api
```

```javascript
const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:9000/api';
```

### 2. Gestion du loading

```javascript
function ClientsList() {
  const [loading, setLoading] = useState(false);
  const [clients, setClients] = useState([]);

  async function loadClients() {
    setLoading(true);
    try {
      const data = await getClients();
      setClients(data);
    } finally {
      setLoading(false); // Toujours arrêter le loading
    }
  }

  if (loading) {
    return <div className="spinner">Chargement...</div>;
  }

  return <div>{/* contenu */}</div>;
}
```

### 3. Debouncing pour recherche

```javascript
import { useState, useEffect } from 'react';

function useDebounce(value, delay) {
  const [debouncedValue, setDebouncedValue] = useState(value);

  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);

    return () => clearTimeout(handler);
  }, [value, delay]);

  return debouncedValue;
}

// Utilisation
function SearchClients() {
  const [searchTerm, setSearchTerm] = useState('');
  const debouncedSearch = useDebounce(searchTerm, 500);

  useEffect(() => {
    if (debouncedSearch) {
      // Effectuer la recherche seulement après 500ms d'inactivité
      searchClients(debouncedSearch);
    }
  }, [debouncedSearch]);

  return (
    <input
      type="text"
      value={searchTerm}
      onChange={(e) => setSearchTerm(e.target.value)}
      placeholder="Rechercher..."
    />
  );
}
```

### 4. Optimistic UI Updates

```javascript
async function handleDelete(id) {
  // Supprimer immédiatement de l'UI
  setClients(prev => prev.filter(c => c.id !== id));

  try {
    await deleteClient(id);
  } catch (error) {
    // En cas d'erreur, restaurer
    loadClients();
    alert('Erreur lors de la suppression');
  }
}
```

## Conclusion

L'intégration de Delphi avec des frameworks JavaScript modernes offre le meilleur des deux mondes :

✅ **Backend solide** avec Delphi - Performance, fiabilité, accès données
✅ **Frontend moderne** avec React/Vue/Angular - UX exceptionnelle
✅ **Architecture découplée** - Évolution indépendante
✅ **Scalabilité** - Chaque partie peut être optimisée séparément
✅ **Écosystème riche** - Profiter des deux communautés

Cette approche est idéale pour :
- Moderniser des applications Delphi existantes
- Créer de nouvelles applications avec interface web moderne
- Permettre à des développeurs JavaScript de travailler sur le frontend
- Conserver l'expertise Delphi pour le backend critique

Dans la section suivante, nous explorerons les Progressive Web Apps (PWA), qui permettent de transformer vos applications web en applications installables fonctionnant hors ligne.

⏭️ [Progressive Web Apps (PWA)](/23-conception-dapplications-web-avec-delphi/07-progressive-web-apps.md)
