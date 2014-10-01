create table "users" ("email" VARCHAR(254) NOT NULL,"password" VARCHAR(254) NOT NULL,"first" VARCHAR(254) NOT NULL,"last" VARCHAR(254) NOT NULL,"is_active" BOOLEAN NOT NULL,"created_at" TIMESTAMP WITH TIME ZONE NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create unique index "user_email__idx" on "users" ("email");

create table "user_tokens" ("user_id" BIGINT NOT NULL,"token_type" VARCHAR(254) NOT NULL,"is_active" BOOLEAN NOT NULL,"token" VARCHAR(254) NOT NULL,"created_at" TIMESTAMP WITH TIME ZONE NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create index "user_tokens_user_id__idx" on "user_tokens" ("user_id");

create index "user_tokens_token__idx" on "user_tokens" ("token");

create table "ext_sessions" ("cookie" VARCHAR(254) NOT NULL,"user_id" BIGINT NOT NULL,"expires_at" TIMESTAMP WITH TIME ZONE NOT NULL,"is_active" BOOLEAN NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create index "ext_sessions_user_id__idx" on "ext_sessions" ("user_id");

create unique index "ext_sessions_cookie__idx" on "ext_sessions" ("cookie");

create table "files" ("user_id" BIGINT NOT NULL,"name" VARCHAR(254) NOT NULL,"size" BIGINT NOT NULL,"key" VARCHAR(254) NOT NULL,"parent_id" BIGINT,"is_removed" BOOLEAN NOT NULL,"created_at" TIMESTAMP WITH TIME ZONE NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "patients" ("user_id" BIGINT NOT NULL,"name" VARCHAR(254) NOT NULL,"identifier" VARCHAR(254) NOT NULL,"identifier_issuer" VARCHAR(254) NOT NULL,"other_identifiers" VARCHAR(254) NOT NULL,"birth_date" TIMESTAMP WITH TIME ZONE NOT NULL,"address" VARCHAR(254) NOT NULL,"phone" VARCHAR(254) NOT NULL,"sex" VARCHAR(254) NOT NULL,"is_removed" BOOLEAN NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "studies" ("patient_id" BIGINT NOT NULL,"instance_uid" VARCHAR(254) NOT NULL,"description" VARCHAR(254) NOT NULL,"date" TIMESTAMP WITH TIME ZONE NOT NULL,"modality" VARCHAR(254) NOT NULL,"is_removed" BOOLEAN NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "series" ("study_id" BIGINT NOT NULL,"instance_uid" VARCHAR(254) NOT NULL,"description" VARCHAR(254) NOT NULL,"date" TIMESTAMP WITH TIME ZONE NOT NULL,"number" INTEGER NOT NULL,"modality" VARCHAR(254) NOT NULL,"is_removed" BOOLEAN NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "images" ("series_id" BIGINT NOT NULL,"file_id" BIGINT NOT NULL,"sop_instance_uid" VARCHAR(254) NOT NULL,"transfer_syntax" VARCHAR(254) NOT NULL,"instance_number" INTEGER NOT NULL,"rows" INTEGER NOT NULL,"columns" INTEGER NOT NULL,"is_removed" BOOLEAN NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "thumbnails" ("dimension" INTEGER NOT NULL,"size" BIGINT NOT NULL,"key" VARCHAR(254) NOT NULL,"image_id" BIGINT NOT NULL,"created_at" TIMESTAMP WITH TIME ZONE NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create table "import_results" ("file_id" BIGINT NOT NULL,"kind" VARCHAR(254) NOT NULL,"version" INTEGER NOT NULL,"error" TEXT,"patient_id" BIGINT,"study_id" BIGINT,"series_id" BIGINT,"image_id" BIGINT,"created_at" TIMESTAMP WITH TIME ZONE NOT NULL,"id" SERIAL NOT NULL PRIMARY KEY);

create index "import_results_file_id__idx" on "import_results" ("file_id");

alter table "user_tokens" add constraint "user_tokens_user_id__fk" foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

alter table "ext_sessions" add constraint "ext_sessions_user_id__fk" foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

alter table "files" add constraint "files_user_id__fk" foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

alter table "patients" add constraint "patients_user_id__fk" foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

alter table "studies" add constraint "studies_patient_id__fk" foreign key("patient_id") references "patients"("id") on update RESTRICT on delete CASCADE;

alter table "series" add constraint "series_study_id__fk" foreign key("study_id") references "studies"("id") on update RESTRICT on delete CASCADE;

alter table "images" add constraint "images_file_id__fk" foreign key("file_id") references "files"("id") on update RESTRICT on delete CASCADE;

alter table "images" add constraint "images_series_id__fk" foreign key("series_id") references "series"("id") on update RESTRICT on delete CASCADE;

alter table "thumbnails" add constraint "thumbnails_image_id__fk" foreign key("image_id") references "images"("id") on update RESTRICT on delete CASCADE;

alter table "import_results" add constraint "import_results_file_id__fk" foreign key("file_id") references "files"("id") on update RESTRICT on delete CASCADE;
