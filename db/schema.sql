CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE dependency (
  id uuid primary key,
  timestamp timestamp not null,
  name text not null
);
CREATE TABLE dependencyScan (
  id uuid primary key,
  timestamp timestamp not null,
  currentVersion text,
  latestVersion text not null,
  latestReleaseDate timestamp,
  notes text,
  dependencyId uuid not null,
  foreign key (dependencyId) references dependency (id)
);
CREATE TABLE projectDependency (
  projectName text not null,
  groupName text not null,
  timestamp timestamp not null,
  dependencyId uuid not null,
  foreign key (dependencyId) references dependency (id)
);
CREATE TABLE vulnerability (
  id uuid primary key,
  name text not null,
  dependencyScanId uuid not null,
  foreign key (dependencyScanId) references dependencyScan (id)
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230504143135');
