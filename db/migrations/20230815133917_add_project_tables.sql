-- migrate:up
create table projectScanConfig (
  id uuid primary key,
  projectName text not null unique,
  gitlabId integer not null unique,
  enabled integer not null,
  branch text not null,

  constraint fk_project_id
    foreign key (projectId)
    references project (id)
    on delete cascade
);

create table txtSource (
  id uuid primary key,
  configId uuid not null,
  path text not null,

  constraint fk_config_id
    foreign key (configId)
    references projectScanConfig (id)
    on delete cascade
);

create table tomlSource (
  id uuid primary key,
  configId uuid not null,
  path text not null,
  group text,

  constraint fk_config_id
    foreign key (configId)
    references projectScanConfig (id)
    on delete cascade
);

-- migrate:down
drop table tomlSource;
drop table txtSource;
drop table projectScanConfig;

