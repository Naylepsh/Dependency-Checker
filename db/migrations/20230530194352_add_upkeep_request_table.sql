-- migrate:up
create table upkeepRequest (
  id uuid primary key,
  projectId text not null,
  dependencyName text not null,
  updateToVersion text not null,
  url text not null
);

-- migrate:down
drop table upkeepRequest;
