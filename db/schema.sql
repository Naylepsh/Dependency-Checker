CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
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
  name TEXT NOT NULL,
  version TEXT,
  release_date TIMESTAMP,
  notes TEXT,

  UNIQUE (name, version)
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
CREATE TABLE IF NOT EXISTS "vulnerability"
(
  id UUID PRIMARY KEY,
  date_created TIMESTAMP NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  name TEXT NOT NULL,
  dependency_id UUID NOT NULL,
  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependency_id)
    REFERENCES dependency (id)
    ON DELETE CASCADE,
  UNIQUE (name, dependency_id)
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230824143342'),
  ('20230831175728');
