CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE dependency (
  id uuid primary key,
  timestamp timestamp not null,
  name text not null
);
CREATE TABLE dependencyScan
(
  id uuid primary key,
  timestamp timestamp not null,
  currentVersion text,
  latestVersion text not null,
  latestReleaseDate timestamp,
  notes text,
  dependencyId uuid not null,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyId)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);
CREATE TABLE projectDependency
(
  projectName text not null,
  groupName text not null,
  timestamp timestamp not null,
  dependencyId uuid not null,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyId)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);
CREATE TABLE vulnerability
(
  id uuid primary key,
  name text not null,
  dependencyScanId uuid not null,
  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyScanId)
    REFERENCES dependencyScan (id)
    ON DELETE CASCADE
);
CREATE TABLE upkeepRequest (
  id uuid primary key,
  projectId text not null,
  dependencyName text not null,
  updateToVersion text not null,
  url text not null
);
CREATE TABLE projectScanConfig (
  id uuid primary key,
  projectName text not null unique,
  gitlabId integer not null unique,
  enabled integer not null,
  branch text not null
);
CREATE TABLE txtSource (
  id uuid primary key,
  configId uuid not null,
  path text not null,

  constraint fk_config_id
    foreign key (configId)
    references projectScanConfig (id)
    on delete cascade
);
CREATE TABLE tomlSource (
  id uuid primary key,
  configId uuid not null,
  path text not null,
  targetGroup text,

  constraint fk_config_id
    foreign key (configId)
    references projectScanConfig (id)
    on delete cascade
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230504143135'),
  ('20230514114834'),
  ('20230530194352'),
  ('20230815133917');
