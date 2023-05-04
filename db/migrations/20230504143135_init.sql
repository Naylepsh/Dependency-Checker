-- migrate:up
create table dependency (
  id uuid primary key,
  timestamp timestamp not null,
  name text not null
);

create table dependencyScan (
  id uuid primary key,
  timestamp timestamp not null,
  currentVersion text,
  latestVersion text not null,
  latestReleaseDate timestamp,
  notes text,
  dependencyId uuid not null,
  foreign key (dependencyId) references dependency (id)
);

create table projectDependency (
  projectName text not null,
  groupName text not null,
  timestamp timestamp not null,
  dependencyId uuid not null,
  foreign key (dependencyId) references dependency (id)
);

create table vulnerability (
  id uuid primary key,
  name text not null,
  dependencyScanId uuid not null,
  foreign key (dependencyScanId) references dependencyScan (id)
);

-- migrate:down
drop table if exists projectDependency;
drop table if exists dependencyScan;
drop table if exists vulnerability;
drop table if exists dependency;