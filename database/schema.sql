-- File: database/schema.sql
-- Convenience wrapper for psql. Each table file is idempotent.
\i database/tables/machines.sql
\i database/tables/sensors.sql
\i database/tables/maintenance.sql
\i database/tables/predictions.sql
\i database/tables/alerts.sql
\i database/tables/model_registry.sql