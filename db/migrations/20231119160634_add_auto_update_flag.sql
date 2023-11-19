-- migrate:up
ALTER TABLE project_scan_config
  ADD COLUMN auto_update INTEGER NOT NULL DEFAULT 0;

-- migrate:down
ALTER TABLE project_scan_config
  DROP COLUMN auto_update;

