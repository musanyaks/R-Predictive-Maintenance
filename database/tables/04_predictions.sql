-- File: database/tables/predictions.sql
CREATE TABLE IF NOT EXISTS predictions (
    prediction_id  BIGSERIAL PRIMARY KEY,
    machine_id     INTEGER NOT NULL REFERENCES machines(machine_id),
    model_version  TEXT,
    failure_prob   DOUBLE PRECISION,
    rul_days       DOUBLE PRECISION,
    anomaly_score  DOUBLE PRECISION,
    risk_score     DOUBLE PRECISION,
    risk_level     TEXT,
    recommendation TEXT,
    predicted_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_pred_machine_time
    ON predictions (machine_id, predicted_at DESC);