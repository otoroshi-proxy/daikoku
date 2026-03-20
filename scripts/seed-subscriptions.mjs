#!/usr/bin/env node

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

/////////////////////////////////////////////
// CONFIG — calibré sur les stats prod
/////////////////////////////////////////////

const BASE_URL = "http://localhost:5173";
const ADMIN_USER = "admin_key_client_id";
const ADMIN_PASS = "admin_key_client_secret";

const OTOROSHI_URL = "http://otoroshi-api.oto.tools:8080";
const OTOROSHI_USER = "admin-api-apikey-id";
const OTOROSHI_PASS = "admin-api-apikey-secret";

const TENANT_ID = "default";
const OTOROSHI_SETTINGS_ID = "seed-otoroshi";
let USER_ID = "";

// --- Dimensions ---
const NB_TEAMS = 660;
const NB_APIS = 220;                // ~3 teams par API
const PLANS_PER_API_MIN = 5;
const PLANS_PER_API_MAX = 30;       // avg ~10

// --- Souscriptions ---
const TARGET_PARENTS = 3900;
const PARALLEL = 120;

// --- Distribution des enfants par parent (bimodale, calquée sur prod) ---
// 75% petites agrégations (1-15), 15% moyennes (16-43), 10% grosses (66-106)
const CHILDREN_DISTRIBUTION = [
  { weight: 12, min: 1, max: 1 },
  { weight: 10, min: 2, max: 3 },
  { weight: 12, min: 4, max: 5 },
  { weight: 8, min: 6, max: 7 },
  { weight: 10, min: 8, max: 10 },
  { weight: 8, min: 11, max: 13 },
  { weight: 5, min: 14, max: 15 },
  { weight: 5, min: 16, max: 20 },
  { weight: 5, min: 21, max: 25 },
  { weight: 3, min: 26, max: 30 },
  { weight: 3, min: 31, max: 43 },
  { weight: 5, min: 66, max: 80 },
  { weight: 2, min: 80, max: 106 },
];

// --- Mega teams ---
const NB_MEGA_TEAMS = 3;
const MEGA_TEAM_PARENTS_MIN = 200;
const MEGA_TEAM_PARENTS_MAX = 443;

/////////////////////////////////////////////

const PLAN_NAMES = [
  "dev", "staging", "prod", "sandbox", "beta",
  "free", "basic", "standard", "premium", "enterprise",
  "internal", "partner", "trial", "preprod", "demo",
  "gold", "silver", "bronze", "platinum", "custom",
  "test", "uat", "perf", "backup", "legacy",
  "v1", "v2", "v3", "public", "private"
];

const AUTH = `Basic ${btoa(ADMIN_USER + ":" + ADMIN_PASS)}`;
const OTO_AUTH = `Basic ${btoa(OTOROSHI_USER + ":" + OTOROSHI_PASS)}`;

/////////////////////////////////////////////
// HTTP
/////////////////////////////////////////////

async function apiCall(method, path, body) {
  const res = await fetch(`${BASE_URL}${path}`, {
    method,
    headers: {
      Authorization: AUTH,
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: body ? JSON.stringify(body) : undefined
  });

  if (!res.ok) {
    const txt = await res.text();
    throw new Error(`[daikoku] ${method} ${path} → ${res.status} ${txt}`);
  }

  return res.json().catch(() => null);
}

async function otoCall(method, path, body) {
  const res = await fetch(`${OTOROSHI_URL}${path}`, {
    method,
    headers: {
      Authorization: OTO_AUTH,
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: body ? JSON.stringify(body) : undefined
  });

  if (!res.ok) {
    const txt = await res.text();
    throw new Error(`[otoroshi] ${method} ${path} → ${res.status} ${txt}`);
  }

  return res.json().catch(() => null);
}

/////////////////////////////////////////////
// WORKER POOL
/////////////////////////////////////////////

class WorkerPool {
  constructor(size) {
    this.size = size;
    this.queue = [];
    this.running = 0;
  }

  push(task) {
    return new Promise((resolve, reject) => {
      this.queue.push({ task, resolve, reject });
      this.next();
    });
  }

  next() {
    while (this.running < this.size && this.queue.length) {
      const { task, resolve, reject } = this.queue.shift();
      this.running++;
      task()
        .then(resolve)
        .catch(reject)
        .finally(() => {
          this.running--;
          this.next();
        });
    }
  }
}

async function drain(pool) {
  while (pool.running > 0 || pool.queue.length > 0) {
    await new Promise(r => setTimeout(r, 200));
  }
}

/////////////////////////////////////////////
// PROGRESS BAR
/////////////////////////////////////////////

function createProgress(name, total) {
  const start = Date.now();
  let current = 0;

  function render() {
    const width = 30;
    const percent = current / total;
    const filled = Math.round(percent * width);
    const bar = "█".repeat(filled) + "░".repeat(width - filled);
    const elapsed = (Date.now() - start) / 1000;
    const rate = Math.round(current / (elapsed || 1));
    const eta = Math.round((total - current) / (rate || 1));
    process.stdout.write(
      `\r${name.padEnd(18)} [${bar}] ${current}/${total}  ${rate}/s ETA ${eta}s`
    );
  }

  const timer = setInterval(render, 300);

  return {
    inc() { current++; },
    stop() { clearInterval(timer); render(); process.stdout.write("\n"); }
  };
}

/////////////////////////////////////////////
// RANDOM HELPERS
/////////////////////////////////////////////

function randInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function pickChildrenCount() {
  const totalWeight = CHILDREN_DISTRIBUTION.reduce((s, b) => s + b.weight, 0);
  let r = Math.random() * totalWeight;
  for (const bucket of CHILDREN_DISTRIBUTION) {
    r -= bucket.weight;
    if (r <= 0) return randInt(bucket.min, bucket.max);
  }
  return CHILDREN_DISTRIBUTION[0].min;
}

/////////////////////////////////////////////
// HELPERS
/////////////////////////////////////////////

let subCounter = 0;

function nextSubId() {
  subCounter++;
  return `seed-sub-${subCounter}`;
}

function makeSubPayload(id, teamId, apiId, planId, parentId) {
  return {
    _id: id,
    _tenant: TENANT_ID,
    plan: planId,
    team: teamId,
    api: apiId,
    enabled: true,
    by: USER_ID,
    createdAt: Date.now(),
    integrationToken: `integ-${id}`,
    customName: parentId ? `Child ${id}` : `Parent ${id}`,
    apiKey: {
      clientName: `Key ${id}`,
      clientId: `cid-${id}`,
      clientSecret: `sec-${id}`
    },
    ...(parentId ? { parent: parentId } : {})
  };
}

function makeOtoRoutePayload(routeId) {
  return {
    id: routeId,
    name: `Seed route ${routeId}`,
    frontend: {
      domains: [`${routeId}.oto.tools`]
    },
    backend: {
      targets: [{ hostname: "mirror.otoroshi.io", port: 443, tls: true }]
    },
    plugins: []
  };
}

function makeOtoApikeyPayload(clientId, clientSecret, clientName, routeIds) {
  return {
    clientId,
    clientSecret,
    clientName,
    authorizedEntities: routeIds.map(r => `route_${r}`),
    enabled: true,
    throttlingQuota: 10000,
    dailyQuota: 10000000,
    monthlyQuota: 100000000
  };
}

/////////////////////////////////////////////
// START
/////////////////////////////////////////////

console.log("\n=== Daikoku + Otoroshi massive seed (prod-like) ===\n");

const pool = new WorkerPool(PARALLEL);

/////////////////////////////////////////////
// 0. CHECK OTOROSHI CONNECTIVITY
/////////////////////////////////////////////

try {
  await otoCall("GET", "/api/live");
  console.log("✓ Otoroshi is reachable\n");
} catch (e) {
  console.error(`✗ Cannot reach Otoroshi at ${OTOROSHI_URL}: ${e.message}`);
  process.exit(1);
}

/////////////////////////////////////////////
// 0.5 RESET DAIKOKU + OTOROSHI
/////////////////////////////////////////////

console.log("Resetting Daikoku...");
await apiCall("POST", "/admin-api/state/reset");
console.log("✓ Daikoku reset\n");

console.log("Resetting Otoroshi...");
const __dirname = dirname(fileURLToPath(import.meta.url));
const otoState = JSON.parse(readFileSync(join(__dirname, "otoroshi-state.json"), "utf-8"));
await otoCall("POST", "/api/import", otoState);
console.log("✓ Otoroshi reset\n");

/////////////////////////////////////////////
// 1. USER
/////////////////////////////////////////////

const users = await apiCall("GET", "/admin-api/users");
USER_ID = users[0]._id;

/////////////////////////////////////////////
// 2. TEMPLATES
/////////////////////////////////////////////

const [teamTemplate, planTemplate, apiTemplate] = await Promise.all([
  apiCall("GET", "/api/entities/team"),
  apiCall("GET", "/api/entities/plan"),
  apiCall("GET", "/api/entities/api"),
]);

/////////////////////////////////////////////
// 3. REGISTER OTOROSHI SETTINGS IN DAIKOKU
/////////////////////////////////////////////

console.log("Registering OtoroshiSettings in tenant...");

const tenant = await apiCall("GET", `/admin-api/tenants/${TENANT_ID}`);

const alreadyExists = (tenant.otoroshiSettings || []).some(s => s._id === OTOROSHI_SETTINGS_ID);

if (!alreadyExists) {
  const otoSettings = {
    _id: OTOROSHI_SETTINGS_ID,
    url: OTOROSHI_URL,
    host: new URL(OTOROSHI_URL).host,
    clientId: OTOROSHI_USER,
    clientSecret: OTOROSHI_PASS,
  };

  await apiCall("PATCH", `/admin-api/tenants/${TENANT_ID}`, [
    { op: "add", path: "/otoroshiSettings/-", value: otoSettings }
  ]);
  console.log("✓ OtoroshiSettings added\n");
} else {
  console.log("✓ OtoroshiSettings already exists\n");
}

/////////////////////////////////////////////
// 4. TEAMS
/////////////////////////////////////////////

const teamIds = [];
const teamBar = createProgress("Teams", NB_TEAMS);

for (let i = 0; i < NB_TEAMS; i++) {
  const id = `seed-team-${i}`;
  const payload = {
    ...teamTemplate,
    _id: id,
    _tenant: TENANT_ID,
    name: `Seed Team ${i}`,
    users: [{ userId: USER_ID, teamPermission: "Administrator" }]
  };

  await apiCall("POST", "/admin-api/teams", payload);
  teamIds.push(id);
  teamBar.inc();
}

teamBar.stop();

/////////////////////////////////////////////
// 5. OTOROSHI ROUTES + DAIKOKU PLANS + APIS
/////////////////////////////////////////////

const apiData = [];         // { apiId, planIds: [{ planId, routeId }] }
const allRouteIds = [];     // pour le résumé

// Créer routes Otoroshi en parallèle, puis plans Daikoku, puis APIs
const apiBar = createProgress("APIs", NB_APIS);
const routeBar = createProgress("Oto Routes", 0); // on mettra à jour le total après
const planBar = createProgress("Plans", 0);

// Pré-calculer le nombre de plans par API
const plansPerApi = Array.from({ length: NB_APIS }, () =>
  randInt(PLANS_PER_API_MIN, PLANS_PER_API_MAX)
);
const totalPlans = plansPerApi.reduce((a, b) => a + b, 0);

// Mettre à jour les totaux des barres
routeBar.stop();
planBar.stop();
const routeBar2 = createProgress("Oto Routes", totalPlans);
const planBar2 = createProgress("Dk Plans", totalPlans);

for (let i = 0; i < NB_APIS; i++) {
  const apiId = `seed-api-${i}`;
  const nbPlans = plansPerApi[i];
  const planEntries = [];

  // Créer routes Otoroshi + plans Daikoku en parallèle par API
  await Promise.all(
    Array.from({ length: nbPlans }).map(async (_, p) => {
      const planId = `seed-plan-${i}-${p}`;
      const routeId = `seed-route-${i}-${p}`;

      // 1. Créer la route dans Otoroshi
      try {
        await otoCall("POST", "/apis/proxy.otoroshi.io/v1/routes", makeOtoRoutePayload(routeId));
      } catch (e) {
        // La route existe peut-être déjà (re-run)
        if (!e.message.includes("409")) throw e;
      }
      allRouteIds.push(routeId);
      routeBar2.inc();

      // 2. Créer le plan dans Daikoku avec otoroshiTarget
      const payload = {
        ...planTemplate,
        _id: planId,
        _tenant: TENANT_ID,
        customName: PLAN_NAMES[p % PLAN_NAMES.length],
        otoroshiTarget: {
          otoroshiSettings: OTOROSHI_SETTINGS_ID,
          authorizedEntities: {
            groups: [],
            services: [],
            routes: [routeId]
          },
          apikeyCustomization: {
            clientIdOnly: false,
            constrainedServicesOnly: false,
            readOnly: false,
            metadata: {},
            tags: [],
            restrictions: {
              enabled: false,
              allowLast: true,
              allowed: [],
              forbidden: [],
              notFound: []
            }
          }
        }
      };

      await apiCall("POST", "/admin-api/usage-plans", payload);
      planEntries[p] = { planId, routeId };
      planBar2.inc();
    })
  );

  // Créer l'API dans Daikoku
  const apiPayload = {
    ...apiTemplate,
    _id: apiId,
    _tenant: TENANT_ID,
    team: teamIds[i % NB_TEAMS],
    name: `Seed API ${i}`,
    possibleUsagePlans: planEntries.map(e => e.planId),
    defaultUsagePlan: planEntries[0].planId,
    state: "published"
  };

  await apiCall("POST", "/admin-api/apis", apiPayload);
  apiData.push({ apiId, planEntries });
  apiBar.inc();
}

routeBar2.stop();
planBar2.stop();
apiBar.stop();

/////////////////////////////////////////////
// 6. SUBSCRIPTIONS + OTOROSHI API KEYS
/////////////////////////////////////////////

// Distribuer les parents par team
const parentsByTeam = new Array(NB_TEAMS).fill(0);

const megaTeamIndices = [];
for (let m = 0; m < NB_MEGA_TEAMS; m++) {
  const idx = Math.floor((m + 1) * NB_TEAMS / (NB_MEGA_TEAMS + 1));
  megaTeamIndices.push(idx);
  parentsByTeam[idx] = randInt(MEGA_TEAM_PARENTS_MIN, MEGA_TEAM_PARENTS_MAX);
}

const megaTotal = megaTeamIndices.reduce((s, i) => s + parentsByTeam[i], 0);
const remaining = TARGET_PARENTS - megaTotal;
const normalTeams = NB_TEAMS - NB_MEGA_TEAMS;
const avgPerNormal = Math.floor(remaining / normalTeams);

for (let t = 0; t < NB_TEAMS; t++) {
  if (megaTeamIndices.includes(t)) continue;
  parentsByTeam[t] = Math.max(1, avgPerNormal + randInt(-Math.floor(avgPerNormal / 2), Math.floor(avgPerNormal / 2)));
}

const actualTotalParents = parentsByTeam.reduce((a, b) => a + b, 0);

// Pré-calculer les enfants
let estimatedChildren = 0;
const parentChildrenCounts = [];
for (let t = 0; t < NB_TEAMS; t++) {
  for (let p = 0; p < parentsByTeam[t]; p++) {
    const cc = pickChildrenCount();
    parentChildrenCounts.push(cc);
    estimatedChildren += cc;
  }
}

const TOTAL_SUBS = actualTotalParents + estimatedChildren;

console.log(`\nPlanned: ${actualTotalParents} parents + ~${estimatedChildren} children = ~${TOTAL_SUBS} subs\n`);

const subBar = createProgress("Dk Subscriptions", TOTAL_SUBS);
const apikeyBar = createProgress("Oto ApiKeys", actualTotalParents);
let errors = 0;
let childCountIdx = 0;

async function createSubscription(payload) {
  try {
    await apiCall("POST", "/admin-api/subscriptions", payload);
    subBar.inc();
  } catch {
    errors++;
    subBar.inc();
  }
}

async function createOtoApiKey(clientId, clientSecret, clientName, routeIds) {
  try {
    await otoCall(
      "POST",
      "/apis/apim.otoroshi.io/v1/apikeys",
      makeOtoApikeyPayload(clientId, clientSecret, clientName, routeIds)
    );
    apikeyBar.inc();
  } catch (e) {
    // Ignore 409 (already exists on re-run)
    if (!e.message.includes("409")) errors++;
    apikeyBar.inc();
  }
}

for (let t = 0; t < NB_TEAMS; t++) {
  const teamId = teamIds[t];

  for (let p = 0; p < parentsByTeam[t]; p++) {
    const apiIdx = randInt(0, apiData.length - 1);
    const planIdx = randInt(0, apiData[apiIdx].planEntries.length - 1);
    const parentId = nextSubId();

    const parentPlanEntry = apiData[apiIdx].planEntries[planIdx];

    // Collecter les routeIds pour l'apikey : la route du parent
    const apikeyRouteIds = [parentPlanEntry.routeId];

    // Enfants
    const nbChildren = parentChildrenCounts[childCountIdx++];
    const usedApis = new Set([apiIdx]);
    const childPayloads = [];

    for (let c = 0; c < nbChildren; c++) {
      let childApiIdx;
      if (usedApis.size < apiData.length) {
        do { childApiIdx = randInt(0, apiData.length - 1); } while (usedApis.has(childApiIdx));
        usedApis.add(childApiIdx);
      } else {
        childApiIdx = randInt(0, apiData.length - 1);
      }

      const childPlanIdx = randInt(0, apiData[childApiIdx].planEntries.length - 1);
      const childPlanEntry = apiData[childApiIdx].planEntries[childPlanIdx];
      const childId = nextSubId();

      // La route de l'enfant est aussi autorisée sur l'apikey du parent
      apikeyRouteIds.push(childPlanEntry.routeId);

      childPayloads.push(makeSubPayload(
        childId,
        teamId,
        apiData[childApiIdx].apiId,
        childPlanEntry.planId,
        parentId
      ));
    }

    // Créer le parent sub dans Daikoku + l'API key dans Otoroshi en parallèle
    await pool.push(async () => {
      await Promise.all([
        createSubscription(makeSubPayload(
          parentId,
          teamId,
          apiData[apiIdx].apiId,
          parentPlanEntry.planId
        )),
        createOtoApiKey(
          `cid-${parentId}`,
          `sec-${parentId}`,
          `Key ${parentId}`,
          apikeyRouteIds
        )
      ]);
    });

    // Créer les enfants dans Daikoku (pas de clé Otoroshi, ils partagent celle du parent)
    for (const childPayload of childPayloads) {
      pool.push(() => createSubscription(childPayload));
    }
  }
}

await drain(pool);

subBar.stop();
apikeyBar.stop();

/////////////////////////////////////////////

console.log("\n=== Seed completed ===");
console.log(`Teams:          ${NB_TEAMS}`);
console.log(`APIs:           ${NB_APIS}`);
console.log(`Oto Routes:     ${allRouteIds.length}`);
console.log(`Oto ApiKeys:    ${actualTotalParents}`);
console.log(`Dk Parents:     ${actualTotalParents}`);
console.log(`Dk Total subs:  ${TOTAL_SUBS}`);
console.log(`Errors:         ${errors}`);
console.log();
