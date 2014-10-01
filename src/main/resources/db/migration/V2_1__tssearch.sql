alter table "patients" add column "name_ts" tsvector;

create index "patients_name_ts_gin" on "patients" using gin("name_ts");

create trigger "patients_name_ts_trigger"
  before insert or update on "patients"
  for each row execute procedure
    tsvector_update_trigger("name_ts", 'pg_catalog.english', 'name');

alter table "studies" add column "description_ts" tsvector;

create index "studies_description_ts_gin" on "studies" using gin("description_ts");

create trigger "studies_description_ts_trigger"
  before insert or update on "studies"
  for each row execute procedure
    tsvector_update_trigger("description_ts", 'pg_catalog.english', 'description');

