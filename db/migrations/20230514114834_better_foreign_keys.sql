-- migrate:up
PRAGMA foreign_keys=off;

-- migrate dependencyScan table
BEGIN TRANSACTION;
ALTER TABLE dependencyScan RENAME TO dependencyScanOld;
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
    FOREIGN KEY (dependencyId) references dependency (id)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);
INSERT INTO dependencyScan SELECT * FROM dependencyScanOld;
COMMIT;

-- migrate projectDependency table
BEGIN TRANSACTION;
ALTER TABLE projectDependency RENAME TO projectDependencyOld;
CREATE TABLE projectDependency
(
  projectName text not null,
  groupName text not null,
  timestamp timestamp not null,
  dependencyId uuid not null,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyId) references dependency (id)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);
INSERT INTO projectDependency SELECT * FROM projectDependencyOld;
COMMIT;

-- migrate vulnerabity table
BEGIN TRANSACTION;
ALTER TABLE vulnerability RENAME TO vulnerabilityOld;
CREATE TABLE vulnerability
(
  id uuid primary key,
  name text not null,
  dependencyScanId uuid not null,
  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyScanId) references dependencyScan (id)
    REFERENCES dependency (id)
    ON DELETE CASCADE
);
INSERT INTO vulnerability SELECT * FROM vulnerabilityOld;
COMMIT;

-- cleanup
DROP TABLE dependencyScanOld;
DROP TABLE projectDependencyOld;
DROP TABLE vulnerabilityOld;

PRAGMA foreign_keys=on;

-- migrate:down
PRAGMA foreign_keys=off;

-- migrate dependencyScan table
BEGIN TRANSACTION;
ALTER TABLE dependencyScan RENAME TO dependencyScanOld;
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
    FOREIGN KEY (dependencyId) references dependency (id)
    REFERENCES dependency (id)
);
INSERT INTO dependencyScan SELECT * FROM dependencyScanOld;
COMMIT;

-- migrate projectDependency table
BEGIN TRANSACTION;
ALTER TABLE projectDependency RENAME TO projectDependencyOld;
CREATE TABLE projectDependency
(
  projectName text not null,
  groupName text not null,
  timestamp timestamp not null,
  dependencyId uuid not null,

  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyId) references dependency (id)
    REFERENCES dependency (id)
);
INSERT INTO projectDependency SELECT * FROM projectDependencyOld;
COMMIT;

-- migrate vulnerabity table
BEGIN TRANSACTION;
ALTER TABLE vulnerability RENAME TO vulnerabilityOld;
CREATE TABLE vulnerability
(
  id uuid primary key,
  name text not null,
  dependencyScanId uuid not null,
  CONSTRAINT fk_dependency_id
    FOREIGN KEY (dependencyScanId) references dependencyScan (id)
    REFERENCES dependency (id)
);
INSERT INTO vulnerability SELECT * FROM vulnerabilityOld;
COMMIT;

-- cleanup
DROP TABLE dependencyScanOld;
DROP TABLE projectDependencyOld;
DROP TABLE vulnerabilityOld;

PRAGMA foreign_keys=off;