-- File: database/tables/sensors.sql
-- In production, convert to a TimescaleDB hypertable:
--   SELECT create_hypertable('sensor_readings', 'reading_time');
CREATE TABLE IF NOT EXISTS sensor_readings (
    reading_id   BIGSERIAL PRIMARY KEY,
    machine_id   INTEGER NOT NULL REFERENCES machines(machine_id),
    sensor_type  TEXT NOT NULL,
    value        DOUBLE PRECISION NOT NULL,
    reading_time TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_readings_machine_time
    ON sensor_readings (machine_id, reading_time DESC);
CREATE UNIQUE INDEX IF NOT EXISTS uq_readings
    ON sensor_readings (machine_id, sensor_type, reading_time);