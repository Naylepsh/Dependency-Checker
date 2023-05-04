CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE dependency (
  id uuid primary key,
  timestamp timestamp not null,
  name text not null,
  currentVersion text,
  latestVersion text not null,
  latestReleaseDate timestamp,
  notes text
);
CREATE TABLE vulnerability (
  id uuid primary key,
  name text not null,
  dependencyId uuid not null,

  foreign key (dependencyId) references dependency (id)
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230504143135');
