-- migrate:up
ALTER TABLE upkeep_request RENAME TO update_request;


-- migrate:down
ALTER TABLE update_request RENAME TO upkeep_request;
