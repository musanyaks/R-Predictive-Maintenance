-- File: database/tables/model_registry.sql
CREATE TABLE IF NOT EXISTS model_registry (
    model_id      SERIAL PRIMARY KEY,
    model_name    TEXT NOT NULL,
    version       TEXT NOT NULL,
    model_type    TEXT NOT NULL,  -- 'classification'|'rul'|'anomaly'|'survival'|'ensemble'
    metrics       JSONB,
    artifact_path TEXT NOT NULL,
    active        BOOLEAN NOT NULL DEFAULT TRUE,
    trained_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX IF NOT EXISTS uq_model_version
    ON model_registry (model_name, version);