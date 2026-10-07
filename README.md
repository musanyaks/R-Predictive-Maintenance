<!-- ============================================================
     File: README.md
     Project: R-Predictive-Maintenance (rpredictmaint)
     ============================================================ -->

<div align="center">

# 🏭 R Predictive Maintenance

**An industrial-grade, end-to-end predictive maintenance platform built in R —
multi-model ML, PostgreSQL/TimescaleDB, Plumber REST API, Shiny dashboard,
explainable AI, drift monitoring, automated `targets` pipelines, and Docker deployment.**

[![R](https://img.shields.io/badge/R-4.2%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![tests](https://github.com/your-org/R-Predictive-Maintenance/actions/workflows/tests.yml/badge.svg)](https://github.com/your-org/R-Predictive-Maintenance/actions/workflows/tests.yml)
[![Docker](https://img.shields.io/badge/docker-compose-2496ED?logo=docker&logoColor=white)](docker-compose.yml)
[![targets](https://img.shields.io/badge/pipeline-targets-35729E)](https://books.ropensci.org/targets/)
[![tidymodels](https://img.shields.io/badge/ml-tidymodels-E1E8ED?logo=r)](https://www.tidymodels.org/)

</div>



---

## Table of Contents

- [Overview](#overview)
- [Features at a Glance](#features-at-a-glance)
- [Architecture](#architecture)
- [The Machine Learning System](#the-machine-learning-system)
- [Tech Stack](#tech-stack)
- [Repository Structure](#repository-structure)
- [Quick Start](#quick-start)
- [Local Development Setup](#local-development-setup)
- [Configuration](#configuration)
- [Usage Guide](#usage-guide)
- [API Reference](#api-reference)
- [The Shiny Dashboard](#the-shiny-dashboard)
- [Database Schema](#database-schema)
- [Testing](#testing)
- [Make Targets](#make-targets)
- [Deployment](#deployment)
- [Monitoring & Drift Detection](#monitoring--drift-detection)
- [Performance & Scaling Notes](#performance--scaling-notes)
- [Production Readiness Checklist](#production-readiness-checklist)
- [Troubleshooting](#troubleshooting)
- [FAQ](#faq)
- [Roadmap](#roadmap)
- [Project Conventions](#project-conventions)
- [Documentation Map](#documentation-map)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgments](#acknowledgments)

---

## Overview

Most R "dashboards" stop at a plot. This project is the other end of the spectrum:
a **complete predictive maintenance system** that a reliability engineering team
could actually run.

It continuously ingests sensor readings from an industrial fleet, engineers
time-series features, and runs **four complementary models** (plus a tuned
ensemble) to answer the three questions that matter in maintenance:

| Question | Model | Output |
|---|---|---|
| *Will this machine fail soon?* | XGBoost failure classifier | `failure_prob` (0–1) |
| *How much life is left?* | Random forest RUL regressor | `rul_days` |
| *Is it behaving abnormally right now?* | Isolation forest | `anomaly_score` (0–1) |
| *What's the long-term hazard?* | Weibull AFT survival model | `hazard_30d` |

These feed a **risk engine** that produces a single 0–100 score and a
categorical level (`low / medium / high / critical`), which drive
**rule-based maintenance recommendations**, **automatic alerts**, a live
**Shiny dashboard**, and a versioned **REST API**.

The project ships with a **synthetic data seeder** (a 10-machine fleet, 90 days
of hourly readings, injected degradation ramps) so the entire system — training
through dashboard — runs out of the box with zero external dependencies.

### Key invariants (design promises)

1. **One feature function.** Training and inference both call
   `compute_features()` — training/serving skew is structurally impossible.
2. **No temporal leakage.** Splits are time-based, never shuffled.
3. **The database is the registry.** Model versions, metrics, and artifact
   paths live in `model_registry`; the CSV mirror is read-only convenience.
4. **Migrations are idempotent.** Every `database/tables/*.sql` uses
   `IF NOT EXISTS`; running them twice is always safe.
5. **Everything is reproducible.** `renv` pins packages, `targets` orchestrates
   the pipeline, and every model version is timestamped in the registry.

---

## Features at a Glance

- 🤖 **5 models** — failure classification, RUL regression, anomaly detection,
  survival analysis, and a Brier-optimized weighted ensemble
- 🧬 **~50 engineered features** per machine-hour — rolling means/sds,
  lag/delta features, deviation-from-baseline, range ratios — auto-derived
  from any number of sensor types
- 🗄️ **PostgreSQL + TimescaleDB** — 6 tables, indexed time-series storage,
  hypertable-ready, idempotent SQL migrations
- 🔌 **Plumber REST API** — 12 versioned endpoints (`/api/v1/...`), bearer-token
  auth middleware, structured logging, error handling, Swagger UI
- 📊 **7-tab Shiny dashboard** — fleet KPIs, per-machine risk cards, gauges,
  72h risk projection, anomaly leaderboard, model registry browser
- 🔍 **Explainability** — SHAP (`shapviz` + `kernelshap`) and XGBoost gain
  importance, plus plain-English prediction rationales
- 📉 **Monitoring** — PSI/Kolmogorov–Smirnov drift detection on predictions,
  backtested precision/recall against actual maintenance events, drift alerts
- ⚙️ **`targets` pipeline** — full DAG from raw CSVs to registered models,
  incremental rebuilds, parquet storage
- 🧪 **Tested** — testthat (edition 3) unit tests, DB-integration tests, and an
  end-to-end smoke test script
- 🐳 **Deployment-ready** — Docker Compose (5 services incl. Nginx reverse
  proxy), per-service Dockerfiles, Kubernetes manifest, GitHub Actions CI

---

## Architecture

### System diagram

```text
                 ┌─────────────────────┐
                 │  Industrial Sensors │
                 └──────────┬──────────┘
                            ↓
                 ┌─────────────────────┐
                 │   Data Ingestion    │
                 │  R seeder / API /   │
                 │  future OPC-UA/MQTT │
                 └──────────┬──────────┘
                            ↓
                 ┌─────────────────────┐
                 │  PostgreSQL /       │
                 │  TimescaleDB        │
                 │  (6 tables)         │
                 └──────────┬──────────┘
                            ↓
                 ┌─────────────────────┐
                 │ Feature Engineering │
                 │  compute_features() │◀── single source of truth
                 └──────────┬──────────┘   (training AND inference)
                            ↓
          ┌─────────────────┼─────────────────┐
          ↓                 ↓                 ↓
   ┌─────────────┐   ┌─────────────┐   ┌─────────────┐
   │   XGBoost   │   │   Random    │   │ Isolation   │      ┌─────────────┐
   │  Failure    │   │  Forest RUL │   │  Forest     │      │   Weibull   │
   │Prediction   │   │ Regression  │   │  Anomaly    │      │  Survival   │
   └──────┬──────┘   └──────┬──────┘   └──────┬──────┘      └──────┬──────┘
          └─────────────────┼─────────────────┘                    │
                            ↓                                      │
                 ┌─────────────────────┐                          │
                 │  Ensemble + Risk    │◀─────────────────────────┘
                 │  Engine (0–100)     │
                 └──────────┬──────────┘
                            ↓
                 ┌─────────────────────┐
                 │  Maintenance Advice │
                 │  + Alerting         │
                 └──────────┬──────────┘
                            ↓
             ┌──────────────┴──────────────┐
             ↓                             ↓
    ┌─────────────────┐          ┌─────────────────┐
    │  R Shiny        │          │  Plumber REST   │
    │  Dashboard      │          │  API (/api/v1)  │
    │  :3838          │          │  :8000          │
    └─────────────────┘          └─────────────────┘
             └──────────────┬──────────────┘
                            ↓
                 ┌─────────────────────┐
                 │  Nginx reverse      │
                 │  proxy (:80)        │
                 └─────────────────────┘
```

### How data flows

1. **Ingest** — `database/seed/seed_database.R` generates a synthetic fleet and
   writes it to `data/raw/*.csv` **and** PostgreSQL. In production, readings
   arrive via an API writer, batch loads, or a streaming collector.
2. **Validate → Clean** — `R/data_pipeline/validation.R` enforces schema,
   ranges, duplicates, and category rules; `cleaning.R` winsorizes outliers
   (IQR fences) and interpolates gaps.
3. **Featurize** — `compute_features()` pivots long readings wide, adds time,
   rolling, lag, and statistical features, and drops the warm-up period.
4. **Label** — `add_failure_label()` marks rows with a failure within 48h;
   `add_rul_label()` computes days-to-next-maintenance.
5. **Train** — the `targets` DAG tunes, fits, evaluates, and registers all
   models; artifacts land in `models/`, metadata in `model_registry`.
6. **Predict** — `run_batch_predictions()` loads active artifacts, scores the
   latest row per machine, computes risk, persists predictions, and raises
   alerts for high/critical machines.
7. **Serve** — the Shiny dashboard reads from Postgres; the Plumber API
   exposes predictions, forecasts, machine data, and alert management.

### `targets` DAG (simplified)

```text
raw_data → validated → cleaned → features_raw ─┬→ features_parquet
                                               ↓
                                          labels_data → feature_cols → splits
                                                                        │
              tuned_classifier → fit_classifier ────────────────────────┤
              fit_rul ──────────────────────────────────────────────────┤
              fit_anomaly ──────────────────────────────────────────────┤
              fit_survival ─────────────────────────────────────────────┤
                                                                        ↓
                                                                val_predictions
                                              ┌───────────────────────────┤
                                              ↓                           ↓
                                     ensemble_weights              model_metrics
                                              └────────────┬──────────────┘
                                                           ↓
                                                   registry_update
                                                           ↓
                                    batch_run (always re-runs) → rul_predictions_parquet
                                    monitor_results (always re-runs)
```

---

## The Machine Learning System

### Prediction tasks

| # | Task | Algorithm | Engine | Key hyperparameters | Registered as |
|---|---|---|---|---|---|
| 1 | Failure classification | Gradient-boosted trees | `xgboost` | 400 trees, depth 4, lr 0.05 (tuned via 5-fold CV grid) | `failure_classifier` |
| 2 | RUL regression | Random forest | `ranger` | 300 trees, min_n 10 | `rul_model` |
| 3 | Anomaly detection | Isolation forest | `isotree` | ndim 2, sample ≤ 1024 | `anomaly_detector` |
| 4 | Survival analysis | Weibull AFT | `survival::survreg` | — | `survival_model` |
| 5 | Risk fusion | Weighted linear ensemble | base R | weights grid-searched by Brier score | `ensemble` |

### Labeling strategy

- **Failure label** — a row is positive if *any* `maintenance_type == 'failure'`
  event occurs within the next `failure_horizon_hours` (default **48h**).
- **RUL label** — days from the feature timestamp to the *next maintenance
  event of any type*; rows after the final event are `NA` and excluded.

### Feature engineering (`compute_features()`)

From `machine_id, sensor_type, value, reading_time` (long format), for **each**
sensor column:

| Family | Features | Windows / lags |
|---|---|---|
| Rolling | `roll_mean_`, `roll_sd_` | 6h, 24h |
| Lag / delta | `lag{n}_`, `delta{n}_` | 1h, 6h, 12h |
| Statistical | `dev_from_mean_`, `range_ratio_` | 24h window |
| Time | `hour_of_day`, `day_of_week` | — |

→ 12 features × 4 sensor types + 2 time features = **50 raw features**.
Selection: Pearson correlation filter (|ρ| > **0.95**, first-occurrence keep)
→ XGBoost gain top-**25**. The first `min_history_hours` (**48h**) of each
machine is discarded so no rolling window is ever `NA` at training time.

### Risk scoring

```text
rul_urgency = 1 / (1 + max(rul_days, 0) / 14)          # 14-day half-life

risk = (0.5·failure_prob + 0.3·rul_urgency + 0.2·anomaly_score) / 1.0

risk_score = 100 × risk        # 0–100
```

| Level | Range | Automatic action |
|---|---|---|
| `low` | 0 – 39 | Routine monitoring (next window 30d) |
| `medium` | 40 – 64 | Increase monitoring frequency; plan window (7d) |
| `high` | 65 – 84 | Schedule maintenance within 3 days + alert |
| `critical` | 85 – 100 | Immediate inspection / shutdown within 24h + alert |

Ensemble weights are grid-searched on the validation set to minimize Brier
score against the realized failure outcome.

### Explainability

- **Global** — `get_feature_importance()` (XGBoost gain), rendered in the
  Models tab.
- **Local** — `compute_shap()` via `kernelshap` + `shapviz`
  (importance + waterfall plots); `explain_machine_prediction()` turns the
  top drivers into a one-line plain-English rationale attached to each
  recommendation.

### Monitoring

See [Monitoring & Drift Detection](#monitoring--drift-detection) below.

---

## Tech Stack

| Layer | Technology | Why |
|---|---|---|
| Language | R ≥ 4.2 | Ecosystem for stats + ML + dashboards |
| Package structure | Real R package (`DESCRIPTION`) | `load_all()`, `R CMD check`, clean namespacing |
| ML framework | `tidymodels` (`parsnip`, `workflows`, `tune`, `rsample`, `dials`, `yardstick`) | Consistent modeling interface |
| Specialized models | `xgboost`, `ranger`, `isotree`, `survival` | Best-in-class per task |
| Explainability | `shapviz`, `kernelshap` | Modern, fast SHAP for XGBoost |
| Pipeline orchestration | `targets` (+ `tarchetypes` semantics) | Incremental, reproducible DAGs |
| Database | PostgreSQL 16 / TimescaleDB, `DBI` + `RPostgres` + `pool` | Time-series optimized, pooled connections |
| Data handling | `dplyr`, `tidyr`, `purrr`, `lubridate`, `zoo`, `arrow` | Tidyverse + columnar parquet |
| Validation | Hand-rolled rule engine (`validation.R`) | Zero-dependency, testable |
| API | `plumber` | Native R REST with Swagger |
| Dashboard | `shiny`, `shinydashboard`, `plotly`, `DT` | Production Shiny stack |
| Config | `config`, `yaml` | Environment-aware settings |
| Logging | `logger` | Leveled, tee-to-file |
| Reproducibility | `renv` | Pinned lockfile (`renv.lock`) |
| Containers | Docker + Docker Compose + Nginx | Parity across dev/prod |
| CI | GitHub Actions (Postgres service container) | `R CMD check` on every PR |

---

## Repository Structure

```text
R-Predictive-Maintenance/
├── DESCRIPTION               # Package definition + all dependencies
├── _targets.R                # Pipeline entrypoint (MUST be at root)
├── Makefile                  # One-command interface to everything
├── Dockerfile                # Base/trainer image
├── docker-compose.yml        # db + api + shiny + trainer + nginx
├── .Renviron.example         # Template for secrets (never commit .Renviron)
│
├── config/                   # Environment-aware configuration
│   ├── config.yml            #   app paths, ports, logging
│   ├── database.yml          #   DB credentials (env-var interpolated)
│   └── model_config.yml      #   features, labels, training, risk, monitoring
│
├── database/
│   ├── schema.sql            # psql convenience wrapper
│   ├── tables/               # 6 idempotent migrations (.sql)
│   └── seed/seed_database.R  # Synthetic fleet generator + loader
│
├── R/                        # The package source (rpredictmaint)
│   ├── utilities/            #   logging, metrics, helpers, config readers
│   ├── database/             #   pool, migrations, all queries
│   ├── data_pipeline/        #   ingestion, validation, cleaning, transforms
│   ├── feature_engineering/  #   compute_features() + feature families + selection
│   ├── models/               #   classifier, RUL, anomaly, survival, ensemble, tuning
│   ├── explainability/       #   SHAP, importance, plain-English explanations
│   ├── forecasting/          #   sensor ARIMA forecasts → 72h risk projection
│   ├── prediction/           #   risk scoring, recommendations, batch runner
│   └── monitoring/           #   PSI/KS drift, prediction drift, backtests
│
├── pipelines/                # targets plans (sourced by _targets.R)
│   ├── data_pipeline.R
│   ├── training_pipeline.R
│   ├── prediction_pipeline.R
│   └── monitoring_pipeline.R
│
├── shiny/                    # Dashboard (app.R + ui/ + server/ + modules/ + www/)
├── api/                      # plumber.R + endpoints/ + middleware/
├── models/                   # Saved artifacts (model.rds) + registry mirror
├── data/                     # raw/ (seeded CSVs), processed/ (parquet)
├── notebooks/                # 7 numbered analysis Rmds
├── reports/                  # final_report.Rmd + generated outputs
├── tests/                    # testthat unit tests + integration smoke test
├── deployment/               # per-service Dockerfiles, nginx.conf, k8s manifest
├── .github/workflows/        # tests.yml, build.yml, deploy.yml
├── docs/                     # 7 deep-dive documents (see Documentation Map)
└── logs/                     # Runtime logs (gitignored)
```

---

## Quick Start

**Prerequisites:** R ≥ 4.2, Git, Make, Docker (for the database — or any
PostgreSQL 14+ you can reach).

```bash
# 1. Clone and configure
git clone https://github.com/your-org/R-Predictive-Maintenance.git
cd R-Predictive-Maintenance
cp .Renviron.example .Renviron        # edit DB_PASSWORD etc.

# 2. Install dependencies (creates renv/ and renv.lock on first run)
make setup
make install

# 3. Start PostgreSQL (TimescaleDB) in Docker
docker compose up -d db

# 4. Create schema and seed a synthetic 10-machine, 90-day fleet
make db-migrate
make db-seed

# 5. Train everything (targets: features → labels → 5 models → registry)
make train

# 6. Score the fleet and raise alerts
make predict

# 7. Serve
make api        # terminal 1 → http://localhost:8000/__docs__
make shiny      # terminal 2 → http://localhost:3838
```

**That's it.** You now have a trained, registered, monitored ML system with a
live dashboard and a REST API — running entirely on synthetic data.

To verify the API:

```bash
curl http://localhost:8000/api/v1/health
# {"status":"ok","time":"2025-01-15T14:32:07Z"}
```

---

## Local Development Setup

### Prerequisites

| Tool | Version | Notes |
|---|---|---|
| R | ≥ 4.2 | Required by `DESCRIPTION` |
| PostgreSQL / TimescaleDB | 14+ / latest-pg16 | Timescale recommended for production |
| Docker + Compose | any recent | Only if using containerized DB/services |
| Make | any | Optional but recommended |
| C toolchain | — | Needed to compile `isotree`, `arrow`, etc. (`r-base-dev` on Linux, Rtools on Windows, Xcode CLT on macOS) |

### Step by step

```bash
# 0. Clone
git clone https://github.com/your-org/R-Predictive-Maintenance.git
cd R-Predictive-Maintenance

# 1. Secrets — copy the template, then edit real values
cp .Renviron.example .Renviron

# 2. renv: initialize + restore exact package versions
Rscript -e 'renv::init(bare = TRUE)'
Rscript -e 'renv::install(); renv::snapshot(force = TRUE)'

# 3. Install the project package (rpredictmaint) in dev mode
Rscript -e "install.packages('.', repos = NULL, type = 'source')"
# or during development:  pkgload::load_all()

# 4. Database
docker compose up -d db                      # or point .Renviron at your own PG
Rscript -e 'rpredictmaint::run_migrations()' # idempotent, safe to re-run
Rscript database/seed/seed_database.R        # ~86,400 sensor rows, 30 maintenance events

# 5. Train (first run: full DAG; later runs: incremental)
Rscript -e 'targets::tar_make()'

# 6. Inspect the pipeline
Rscript -e 'targets::tar_visnetwork()'
```

### Interactive development

```r
pkgload::load_all(".")                    # loads the package + all R/ functions
rpredictmaint::setup_logging("dev")

# Pull latest predictions straight into your session
preds <- get_latest_predictions()
head(preds)

# Load the active classifier and inspect it
models <- load_active_models()
get_feature_importance(models$failure_classifier)
```

---

## Configuration

### Environment variables (`.Renviron`)

| Variable | Default | Purpose |
|---|---|---|
| `DB_HOST` | `localhost` | PostgreSQL host |
| `DB_PORT` | `5432` | PostgreSQL port |
| `DB_NAME` | `predictmaint` | Database name |
| `DB_USER` | `postgres` | Database user |
| `DB_PASSWORD` | *(required)* | Database password — **never commit the real one** |
| `API_TOKEN` | *(empty = open)* | Bearer token for the API; empty disables auth in dev |
| `R_CONFIG_ACTIVE` | `default` | Selects `default` vs `production` config blocks |
| `LOG_LEVEL` | `INFO` | `DEBUG / INFO / WARN / ERROR` |

### Config files

| File | Controls | Key sections |
|---|---|---|
| `config/config.yml` | App-wide paths, ports, logging | `paths`, `api.port`, `shiny.port` |
| `config/database.yml` | DB connection | env-var interpolated per environment |
| `config/model_config.yml` | **The entire ML behavior** | see below |

### `model_config.yml` — the ML tuning surface

```yaml
features:
  rolling_windows: [6, 24]     # hours — rolling mean/sd windows
  lag_offsets: [1, 6, 12]      # hours — lag & delta features
  stat_window: 24              # hours — dev_from_mean / range_ratio
  min_history_hours: 48        # warm-up rows discarded per machine
labels:
  failure_horizon_hours: 48    # look-ahead for the failure label
training:
  test_prop: 0.2               # time-based holdout (most recent 20%)
  xgboost: {trees: 400, tree_depth: 4, learn_rate: 0.05}
  corr_threshold: 0.95         # correlation filter
  top_k_features: 25           # keep top-k by xgboost gain
risk:
  weights: {failure: 0.5, rul: 0.3, anomaly: 0.2}
  rul_half_life_days: 14
  thresholds: {medium: 40, high: 65, critical: 85}
monitoring:
  psi_threshold: 0.2           # PSI above this = drift alert
```

> Changing `model_config.yml` changes feature construction — which changes
> `compute_features()` output — which `targets` detects and propagates through
> the entire DAG on the next `tar_make()`.

---

## Usage Guide

### 1. Seed data

```bash
Rscript database/seed/seed_database.R
```

Generates: 10 machines (`PUMP-001`…`PUMP-010`, 3 machine types, 2 plants),
90 days × hourly × 4 sensors (temperature ~70, vibration ~3, pressure ~100,
rpm ~1750) = **86,400 readings**, ~30 maintenance events, and an injected
degradation ramp on `PUMP-003`, `PUMP-007`, `PUMP-010` during the final 10
days — so the trained models have real signal to find.

### 2. Train

```bash
make train
```

Runs the `targets` DAG. Because `targets` caches, subsequent runs only rebuild
what changed. Useful inspection commands:

```r
targets::tar_manifest()   # what will run
targets::tar_visnetwork() # dependency graph
targets::tar_meta()       # timings, errors, sizes
targets::tar_read(model_metrics)
```

Every successful run registers a **new timestamped version** of all five
models and deactivates the previous ones — predictions always record which
`model_version` produced them.

### 3. Batch predictions

```bash
make predict
# or: Rscript -e 'rpredictmaint::run_batch_predictions()'
```

Looks back **96 hours**, rebuilds features via the *same* `compute_features()`,
scores the latest row per machine with all active models, computes risk,
writes to the `predictions` table, and inserts an `alerts` row for every
`high`/`critical` machine. The API's `POST /api/v1/predictions/run` triggers
the identical code path.

### 4. Monitoring

```bash
make monitor
# or: Rscript -e 'targets::tar_make(names = starts_with(c("drift_","perf_","monitor_")))'
```

See [Monitoring & Drift Detection](#monitoring--drift-detection).

### 5. Scheduled operation (cron example)

```cron
# Hourly batch scoring
0 * * * * cd /opt/rpredictmaint && Rscript -e 'rpredictmaint::run_batch_predictions()'
# Daily drift check + performance backtest at 06:00
0 6 * * * cd /opt/rpredictmaint && Rscript -e 'rpredictmaint::run_model_monitoring()'
# Weekly retrain Sunday 02:00
0 2 * * 0 cd /opt/rpredictmaint && Rscript -e 'targets::tar_make()'
```

---

## API Reference

Base URL: `http://localhost:8000/api/v1` · Swagger UI: `http://localhost:8000/__docs__`

**Authentication** — all routes except `/health` require:

```text
Authorization: Bearer <API_TOKEN>
```

If `API_TOKEN` is unset, the API runs open (development mode only).

### Endpoints

| Method | Path | Description |
|---|---|---|
| GET | `/health` | Liveness probe (no auth) |
| GET | `/predictions/latest?limit=500` | Most recent prediction per machine |
| POST | `/predictions/run` | Trigger a full batch scoring run |
| GET | `/predictions/forecast/{machine_name}?horizon=72` | Projected failure risk, hour by hour |
| GET | `/machines` | Fleet list (active machines) |
| GET | `/machines/{machine_id}` | Single machine detail |
| GET | `/sensors/{machine_id}/readings?hours=24` | Raw readings window |
| GET | `/sensors/{machine_id}/summary` | 24h per-sensor stats (mean/sd/min/max) |
| GET | `/alerts/open` | All unresolved alerts |
| POST | `/alerts/{alert_id}/resolve` | Mark an alert resolved |
| GET | `/maintenance/schedule` | Current recommended actions per machine |
| GET | `/maintenance/history` | Maintenance event history |

### Examples

```bash
export API_TOKEN="your-token"
BASE=http://localhost:8000/api/v1

# Health (no auth)
curl $BASE/health

# Latest predictions
curl -H "Authorization: Bearer $API_TOKEN" "$BASE/predictions/latest?limit=10"
```

Sample response — `GET /predictions/latest`:

```json
[
  {
    "prediction_id": 1042,
    "machine_id": 3,
    "model_version": "20250115.061203",
    "failure_prob": 0.812,
    "rul_days": 4.3,
    "anomaly_score": 0.94,
    "risk_score": 82.6,
    "risk_level": "high",
    "predicted_at": "2025-01-15T06:14:00Z"
  }
]
```

```bash
# Trigger scoring now
curl -X POST -H "Authorization: Bearer $API_TOKEN" $BASE/predictions/run
# → {"status":"ok","machines":10,"critical":1,"high":2}

# 72-hour risk projection for a degrading pump
curl -H "Authorization: Bearer $API_TOKEN" "$BASE/predictions/forecast/PUMP-003?horizon=72"

# Open alerts
curl -H "Authorization: Bearer $API_TOKEN" $BASE/alerts/open

# Resolve alert 17
curl -X POST -H "Authorization: Bearer $API_TOKEN" $BASE/alerts/17/resolve
```

Error shape (all 5xx): `{"error": "internal_error", "message": "..."}` ·
401 on bad/missing token · 404 on unknown machine/alert.

---

## The Shiny Dashboard

Run with `make shiny` (or `Rscript shiny/app.R`) → **http://localhost:3838**

| Tab | What you see |
|---|---|
| **Dashboard** | 4 KPI value boxes (critical / high / medium / fleet size), one card per machine with risk score, RUL, anomaly score — auto-refresh every 60s |
| **Monitoring** | PSI drift bar chart by output metric, backtest precision/recall table, open alerts |
| **Predictions** | Machine selector → risk gauge (0–100 with colored bands), prediction detail table, **72h failure-risk projection curve** (loads models on demand) |
| **Anomalies** | Anomaly leaderboard (DT) + anomaly-vs-risk scatter colored by level |
| **Models** | Model registry (versions, types, active flags), XGBoost feature importance, latest metrics from the registry JSON |
| **Maintenance** | Rule-based recommended actions with priority + due date + rationale; full maintenance history |
| **Reports** | One-click download of a rendered HTML maintenance report (`reports/final_report.Rmd`) with any report date |

The dashboard is read-mostly by design — operators *observe*, the API and
pipelines *mutate*.

---

## Database Schema

| Table | Grain | Key columns | Notes |
|---|---|---|---|
| `machines` | one per machine | `machine_id` (PK), `machine_name` (UNIQUE), `machine_type`, `location`, `status` | Fleet registry |
| `sensor_readings` | one per machine × sensor × timestamp | `machine_id` (FK), `sensor_type`, `value`, `reading_time` | Unique index `(machine_id, sensor_type, reading_time)`; descending index for window queries; TimescaleDB hypertable-ready |
| `maintenance` | one per event | `machine_id` (FK), `maintenance_type`, `downtime_hours`, `performed_at` | Source of all labels; types: `preventive` \| `corrective` \| `failure` |
| `predictions` | one per machine per batch run | `failure_prob`, `rul_days`, `anomaly_score`, `risk_score`, `risk_level`, `model_version` | Append-only audit trail |
| `alerts` | one per raised alert | `alert_level`, `message`, `created_at`, `resolved_at` | Partial index on open alerts |
| `model_registry` | one per model version | `model_name`, `version` (UNIQUE together), `metrics` (JSONB), `artifact_path`, `active` | DB is the source of truth; CSV mirror written on every registration |

Apply / re-apply at any time:

```r
rpredictmaint::run_migrations()   # idempotent
```

---

## Testing

```bash
make test          # unit tests (testthat, edition 3)
Rscript tests/integration/test_pipeline.R   # end-to-end smoke test (no DB needed)
```

| Suite | File | Covers |
|---|---|---|
| Validation | `tests/testthat/test-validation.R` | Duplicate/NA/range/missing-column detection, category rules |
| Features | `tests/testthat/test-features.R` | `compute_features()` determinism, column contract, no residual NAs |
| Models | `tests/testthat/test-models.R` | Classifier trains + probabilities in [0,1]; ensemble weights sum to 1 |
| Predictions | `tests/testthat/test-predictions.R` | Risk thresholds; recommendation escalation |
| Database | `tests/testthat/test-database.R` | Pool connectivity; **migration idempotency** (auto-skips without creds) |
| API | `tests/testthat/test-api.R` | Router builds; auth filter present |
| Integration | `tests/integration/test_pipeline.R` | raw → features → labels → 3 models → risk, with assertions |

CI (`.github/workflows/tests.yml`) runs `R CMD check` on every push/PR with a
real PostgreSQL 16 service container.

---

## Make Targets

| Target | Command it wraps | Purpose |
|---|---|---|
| `make setup` | `renv::init(bare = TRUE)` | Initialize renv |
| `make install` | `renv::install()` + package install | Restore deps, install `rpredictmaint` |
| `make renv-lock` | `renv::snapshot(force = TRUE)` | Regenerate the lockfile |
| `make db-migrate` | `run_migrations()` | Apply all schema migrations |
| `make db-seed` | `seed_database.R` | Generate + load synthetic fleet |
| `make train` | `tar_make(names = ...)` | Full training DAG → registry |
| `make predict` | `run_batch_predictions()` | Score the fleet now |
| `make monitor` | `tar_make(names = ...)` | Drift + performance pass |
| `make test` | `devtools::test()` | Unit tests |
| `make api` | `run_api(port = 8000)` | Start the REST API |
| `make shiny` | `shiny/app.R` | Start the dashboard |
| `make docker-build` | `docker compose build` | Build all images |
| `make docker-up` | `docker compose up -d` | Start the full stack |

---

## Deployment

### Docker Compose (recommended for single-host / staging)

```bash
cp .Renviron.example .Renviron   # set DB_PASSWORD, API_TOKEN
docker compose build
docker compose up -d db          # wait for healthy
docker compose up -d api shiny nginx
docker compose --profile train run trainer   # one-off full training
```

| Service | Image | Port | Notes |
|---|---|---|---|
| `db` | TimescaleDB pg16 | 5432 | Health-checked; volume `pgdata` |
| `api` | project API image | 8000 | Waits for `db` healthy |
| `shiny` | project Shiny image | 3838 | Waits for `db` healthy |
| `trainer` | project base image | — | Profile `train`; runs `tar_make()` |
| `nginx` | nginx 1.25 | **80** | `/api/` → api; `/` → shiny (websocket-upgraded) |

All services share env vars for DB connection; secrets come from your shell
or `.env` (gitignored).

### Kubernetes

`deployment/production/deployment.yml` ships a 2-replica API Deployment +
Service with a readiness probe on `/api/v1/health`:

```bash
kubectl create secret generic rpredictmaint-secrets \
  --from-literal=DB_HOST=... --from-literal=DB_PASSWORD=... \
  --from-literal=API_TOKEN=...
kubectl apply -f deployment/production/deployment.yml
```

### CI/CD

- `tests.yml` — `R CMD check` + Postgres service on every PR
- `build.yml` — builds both Docker images on push to `main`
- `deploy.yml` — builds/pushes `ghcr.io/.../api:<tag>` on `v*` tags;
  wire your `kubectl apply` into the marked step

---

## Monitoring & Drift Detection

`run_model_monitoring()` (triggered by `make monitor`, the monitoring tab, or
cron) performs three checks:

1. **Prediction drift** — PSI between predictions older than 30 days and the
   last 14 days, per output metric (`failure_prob`, `risk_score`, `rul_days`).
   PSI > **0.2** ⇒ drift + `drift` alert row.
2. **Feature drift** — `detect_data_drift()` compares any reference frame
   (e.g., the training snapshot in `data/processed/features_train.parquet`)
   against current features using PSI **and** a two-sample KS test per feature.
3. **Performance backtest** — `evaluate_predictions()` checks whether
   high/critical predictions were followed by an actual `failure`/`corrective`
   event within 30 days, yielding backtested precision/recall written to
   `reports/model_performance/performance_YYYYMMDD.csv`.

**Interpretation guide:** PSI < 0.1 stable · 0.1–0.2 watch · > 0.2 drift
(retrain recommended). Rising drift across runs = systematic distribution
change (new operating regime, sensor replacement, seasonality) — retrain via
`make train` and compare versions in the registry.

---

## Performance & Scaling Notes

- **Inserts** are chunked (5,000 rows/transaction) — safe for the ~100k-row
  seed scale and beyond.
- **Connections** are pooled (`pool`, 1–10 connections) — never open per-request.
- **Features** are computed vectorized per machine group; the current
  bottleneck at ~1M+ readings is `group_modify` rolling windows — migrate to
  TimescaleDB **continuous aggregates** (precomputing `roll_mean_24h`, etc.)
  when you get there.
- **targets** stores targets as parquet (`format = "parquet"`) — fast, compact,
  language-agnostic.
- **API** — for heavy forecast endpoints, wrap handlers in `promises` +
  `future` to keep the event loop free (roadmap item).
- **Scale path**: Postgres → Timescale hypertables → continuous aggregates →
  read replica for the dashboard → separate scoring worker consuming a queue.

---

## Production Readiness Checklist

What's built vs. what *you* must add before a real plant:

**Done in this repo**

- [x] Bearer-token API auth + structured error handling
- [x] Idempotent, re-runnable migrations
- [x] Versioned model registry with automatic deactivation of old versions
- [x] Training/serving skew prevention via single feature function
- [x] Drift monitoring + alerting + backtesting
- [x] Unit + integration tests in CI with a real database
- [x] Secrets kept out of git (`.Renviron.example` pattern)

**Your responsibility before real deployment**

- [ ] TLS everywhere (terminate at Nginx or your ingress)
- [ ] Real ingestion path (OPC-UA / MQTT / historian → `sensor_readings`)
- [ ] Real labeled failures (synthetic seeder is for demo only)
- [ ] Database backups + retention policy (predictions grow unbounded — add
      partitioning/rotation)
- [ ] Secrets manager (Vault / AWS SM) instead of `.Renviron`
- [ ] RBAC / per-client API keys if multi-team
- [ ] Alert delivery (email / Slack / PagerDuty) beyond DB rows
- [ ] Load-test the API; add `promises`/`future` async to heavy endpoints
- [ ] Model governance review (the survival + SHAP outputs help here)

---

## Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `could not connect to server` | DB down or wrong `.Renviron` | `docker compose up -d db`; verify `DB_*` vars |
| `relation "machines" does not exist` | Migrations not run | `make db-migrate` |
| `No active models registered. Train first.` | API/dashboard started before training | `make train` (registers 5 models) |
| Dashboard loads but all cards say "No data" | `make predict` never run | `make predict` |
| `renv` package errors after pulling | Lockfile out of sync | `renv::restore()`; after dep changes: `make renv-lock` |
| `targets` won't rebuild something | Stale cache after manual edits | `targets::tar_invalidate(<name>)` or `targets::tar_destroy()` (nuclear) |
| Port 8000/3838/5432 in use | Other service bound | Change in `config/config.yml` / compose file |
| SHAP notebook fails | Models not trained / too few rows | `make train` first; check `nrow(feats)` |
| Tests skip database tests | `DB_PASSWORD` unset in test env | Export it (CI does this via the service container) |
| Forecasts look flat | Series < 48 points → naive-drift fallback | Expected behavior; seed 90 days to get ARIMA |
| `isotree`/`arrow` install fails | Missing C toolchain | Install `r-base-dev` / Rtools / Xcode CLT |

---

## FAQ

**Can I use real sensor data?**
Yes. Three paths: (a) write rows to `sensor_readings` via your own loader,
(b) replace the CSVs in `data/raw/` and re-run `make train` (offline pipeline
path), or (c) add a POST ingestion endpoint (see `api/endpoints/sensors.R`).
Any set of `sensor_type` names works — `compute_features()` pivots whatever
types it finds.

**Why a time-based split instead of cross-validation?**
Shuffled CV on time-series leaks the future into training (rolling/lag
features at time *t* correlate with rows after *t*). The 80/20 chronological
split respects causality; CV is only used *within* the training window for
hyperparameter tuning.

**How do I add a new model?**
Create `R/models/my_model.R` with `train_*()` / `predict_*()` functions,
register it in `training_pipeline.R` (one `tar_target` + one
`register_model()` call), and optionally fold it into `predict_ensemble()`.

**How do I retrain?**
`make train`. A new timestamped version is registered; the old one is
deactivated but kept for audit. Predictions record which version scored them.

**Is the ensemble weight tuning automatic?**
Yes — `fit_ensemble_weights()` grid-searches weights on the validation set
each training run and stores the result in the registry as the `ensemble`
model.

**Where do predictions go after the table gets huge?**
Plan partitioning or a retention job (e.g., keep 180 days). The dashboard and
API only ever read the latest per machine (`DISTINCT ON`), so pruning old rows
is safe once your backtests are done.

**Does this run on Windows/macOS?**
Development: yes (R + Docker Desktop). The Dockerfiles target Linux
(`rocker/r-ver`), which is what you should ship.

---

## Roadmap

- [ ] Migrate model serving/registry to **vetiver + pins** (standard ecosystem)
- [ ] Refactor Shiny app to **golem** + `shinytest2` browser tests
- [ ] Async API handlers (`promises` + `future`) for forecast endpoints
- [ ] Real streaming ingestion (MQTT/OPC-UA collector writing to Timescale)
- [ ] Timescale continuous aggregates replacing in-R rolling windows at scale
- [ ] Alert delivery integrations (Slack, email, PagerDuty)
- [ ] JWT/OAuth + RBAC for the API
- [ ] Model A/B shadow scoring (score with candidate + active, compare)
- [ ] Scheduled CI training runs with automatic promotion on metric improvement

---

## Project Conventions

- **R style**: tidyverse style guide; 2-space indent; native pipe `|>`
- **Docs**: every exported function has roxygen comments; rebuild with `devtools::document()`
- **Naming**: `snake_case` functions; `value_<sensor>` for raw pivots;
  fixed prefixes for feature families (`roll_mean_`, `lag1_`, `delta6_`, `dev_from_mean_`)
- **SQL**: all schema changes are new idempotent files in `database/tables/` — never edit applied migrations
- **Git**: feature branches (`feat/`, `fix/`, `docs/`), conventional-commit-style messages; PRs must pass CI
- **Config**: anything tunable lives in `config/*.yml` or env vars — never hardcoded

---

## Documentation Map

| Document | Contents |
|---|---|
| `docs/architecture.md` | System diagram + key invariant (single feature function) |
| `docs/database_design.md` | Table-by-table design rationale |
| `docs/ml_pipeline.md` | Labels, splits, selection, models, registry, monitoring in detail |
| `docs/api_documentation.md` | Endpoint reference (mirrors [API Reference](#api-reference)) |
| `docs/dashboard_guide.md` | Tab-by-tab operator walkthrough |
| `docs/deployment_guide.md` | Local / Docker / Kubernetes runbooks |
| `docs/user_manual.md` | Role-based guide (operator, reliability engineer, data scientist, API consumer) |
| `data/external/dataset_documentation.md` | Schema of every seeded/generated file |
| `notebooks/01…07_*.Rmd` | Executable analysis walkthroughs (EDA → evaluation) |

---

## Contributing

1. Fork / branch from `main` (`feat/my-change`)
2. `renv::restore()` to get the pinned environment
3. Add/adjust tests in `tests/testthat/` for any behavior change
4. `devtools::document()` if you touched roxygen
5. `make test` + `Rscript tests/integration/test_pipeline.R` locally
6. Open a PR — CI runs `R CMD check` against a real Postgres

Bug reports and architecture proposals via GitHub Issues are welcome.

---

## License

MIT — see [LICENSE](LICENSE).

---

## Acknowledgments

Built on the shoulders of:

- [tidymodels](https://www.tidymodels.org/) — modeling framework
- [targets](https://books.ropensci.org/targets/) — pipeline orchestration
- [plumber](https://www.rplumber.io/) — REST APIs in R
- [Shiny](https://shiny.posit.co/) — interactive dashboards
- [TimescaleDB](https://www.timescale.com/) — time-series Postgres
- `shapviz` / `kernelshap` — fast SHAP
- `isotree` — isolation forests in R

<div align="center">
<sub>Built with R. Run by reliability engineers.</sub>
</div>
