-- migrate:up
create table dependency (
  id uuid primary key,
  timestamp timestamp not null,
  name text not null,
  currentVersion text,
  latestVersion text not null,
  latestReleaseDate timestamp,
  notes text
);

create table vulnerability (
  id uuid primary key,
  name text not null,
  dependencyId uuid not null,

  foreign key (dependencyId) references dependency (id)
);

-- migrate:down
drop table dependency;
drop table vulnerability;
