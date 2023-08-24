-- migrate:up
CREATE TABLE project (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

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

CREATE TABLE txt_source (
  id UUID PRIMARY KEY,
  config_id UUID NOT NULL,
  path TEXT NOT NULL,

  CONSTRAINT fk_config_id
    FOREIGN KEY (config_id)
    REFERENCES project_scan_config (id)
    ON DELETE cascade
);

CREATE TABLE toml_source (
  id UUID PRIMARY KEY,
  config_id UUID NOT NULL,
  path TEXT NOT NULL,
  target_group TEXT,

  CONSTRAINT fk_config_id
    FOREIGN KEY (config_id)
    REFERENCES project_scan_config (id)
    ON DELETE cascade
);

CREATE TABLE dependency (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE dependency_scan
(
  id UUID PRIMARY KEY,
  timestamp TIMESTAMP NOT NULL,
  current_version TEXT,
  latest_version TEXT NOT NULL,
  latest_release_date TIMESTAMP,
  notes TEXT,
  dependency_id UUID NOT NULL,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependency_id)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);

CREATE TABLE project_dependency
(
  group_name TEXT NOT NULL,
  timestamp TIMESTAMP NOT NULL,
  dependency_id UUID NOT NULL,
  project_id UUID NOT NULL,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependency_id)
    REFERENCES dependency (id)
    ON DELETE CASCADE,
  CONSTRAINT fk_project_id
    FOREIGN KEY (project_id)
    REFERENCES project (id)
    ON DELETE CASCADE
);

CREATE TABLE vulnerability
(
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  dependency_scan_id UUID NOT NULL,
  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependency_scan_id)
    REFERENCES dependency_scan (id)
    ON DELETE CASCADE
);

CREATE TABLE upkeep_request (
  id UUID PRIMARY KEY,
  project_id UUID NOT NULL,
  dependency_name TEXT NOT NULL,
  update_to_version TEXT NOT NULL,
  url TEXT NOT NULL,

  CONSTRAINT fk_project_id
    FOREIGN KEY (project_id)
    REFERENCES project (id)
    ON DELETE CASCADE
);

-- migrate:down
DROP TABLE dependency
DROP TABLE dependency_scan
DROP TABLE project_dependency
DROP TABLE vulnerability
DROP TABLE upkeep_request
DROP TABLE project_scan_config
DROP TABLE txt_source
DROP TABLE toml_source
