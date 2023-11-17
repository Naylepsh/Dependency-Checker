-- migrate:up
PRAGMA foreign_keys=off;

CREATE TABLE project_scan_config_new (
  id UUID PRIMARY KEY,
  gitlab_id INTEGER NOT NULL,
  enabled INTEGER NOT NULL,
  branch TEXT NOT NULL,
  project_id UUID NOT NULL,

  CONSTRAINT fk_project_id
    FOREIGN KEY (project_id)
    REFERENCES project (id)
    ON DELETE CASCADE
);
INSERT INTO project_scan_config_new SELECT * FROM project_scan_config;
DROP TABLE project_scan_config;
ALTER TABLE project_scan_config_new RENAME TO project_scan_config;

PRAGMA foreign_keys=on;

-- migrate:down
PRAGMA foreign_keys=off;

CREATE TABLE project_scan_config (
  id UUID PRIMARY KEY,
  gitlab_id INTEGER NOT NULL UNIQUE,
  enabled INTEGER NOT NULL,
  branch TEXT NOT NULL,
  project_id UUID NOT NULL,

  CONSTRAINT fk_project_id
    FOREIGN KEY (project_id)
    REFERENCES project (id)
    ON DELETE CASCADE
);
INSERT INTO project_scan_config_new SELECT * FROM project_scan_config;
DROP TABLE project_scan_config;
ALTER TABLE project_scan_config_new RENAME TO project_scan_config;

PRAGMA foreign_keys=on;
