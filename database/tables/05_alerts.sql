-- File: database/tables/alerts.sql
CREATE TABLE IF NOT EXISTS alerts (
    alert_id    BIGSERIAL PRIMARY KEY,
    machine_id  INTEGER NOT NULL REFERENCES machines(machine_id),
    alert_level TEXT NOT NULL,  -- 'medium' | 'high' | 'critical' | 'drift'
    message     TEXT NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    resolved_at TIMESTAMPTZ
);
CREATE INDEX IF NOT EXISTS idx_alerts_open
    ON alerts (machine_id) WHERE resolved_at IS NULL;