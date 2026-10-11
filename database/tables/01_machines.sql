-- File: database/tables/machines.sql
CREATE TABLE IF NOT EXISTS machines (
    machine_id     SERIAL PRIMARY KEY,
    machine_name   TEXT NOT NULL UNIQUE,
    machine_type   TEXT NOT NULL,
    location       TEXT,
    installed_date DATE,
    status         TEXT NOT NULL DEFAULT 'active',
    created_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);