-- File: database/tables/maintenance.sql
CREATE TABLE IF NOT EXISTS maintenance (
    maintenance_id   SERIAL PRIMARY KEY,
    machine_id       INTEGER NOT NULL REFERENCES machines(machine_id),
    maintenance_type TEXT NOT NULL,  -- 'preventive' | 'corrective' | 'failure'
    description      TEXT,
    downtime_hours   DOUBLE PRECISION DEFAULT 0,
    performed_at     TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_maint_machine_time
    ON maintenance (machine_id, performed_at DESC);