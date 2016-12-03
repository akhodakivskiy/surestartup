create table "ideas" (
    "title" VARCHAR(254) NOT NULL,
    "description" VARCHAR(254) NOT NULL,
    "template" VARCHAR(254) NOT NULL,
    "local_id" VARCHAR(254) NOT NULL,
    "domain" VARCHAR(254),
    "ga" VARCHAR(254),
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "is_published" BOOLEAN NOT NULL,
    "is_removed" BOOLEAN NOT NULL,
    "user_id" BIGINT NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create unique index "idx__ideas__domain" on "ideas" ("domain");

create unique index "idx__ideas__local_id" on "ideas" ("local_id");

create index "idx__ideas__user_id" on "ideas" ("user_id");


create table "idea_texts" (
    "name" VARCHAR(254) NOT NULL,
    "text" VARCHAR(254) NOT NULL,
    "idea_id" BIGINT NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "is_removed" BOOLEAN NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create index "idx__idea_texts__idea_id" on "idea_texts" ("idea_id");


create table "idea_images" (
    "name" VARCHAR(254) NOT NULL,
    "path" VARCHAR(254) NOT NULL,
    "height" INTEGER NOT NULL,
    "width" INTEGER NOT NULL,
    "idea_id" BIGINT NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "is_removed" BOOLEAN NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create index "idx__idea_images__idea_id" on "idea_images" ("idea_id");


create table "idea_signups" (
    "email" VARCHAR(254) NOT NULL,
    "name" VARCHAR(254) NOT NULL,
    "message" TEXT NOT NULL,
    "idea_id" BIGINT NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "is_removed" BOOLEAN NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create index "idx__idea_signups__idea_id" on "idea_signups" ("idea_id");


create table "users" (
    "email" VARCHAR(254) NOT NULL,
    "password" VARCHAR(254) NOT NULL,
    "first" VARCHAR(254) NOT NULL,
    "last" VARCHAR(254) NOT NULL,
    "created_at" TIMESTAMP NOT NULL,
    "updated_at" TIMESTAMP NOT NULL,
    "is_active" BOOLEAN NOT NULL,
    "is_removed" BOOLEAN NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create unique index "idx__users__email" on "users" ("email");


create table "user_tokens" (
    "user_id" BIGINT NOT NULL,
    "token_type" VARCHAR(254) NOT NULL,
    "is_active" BOOLEAN NOT NULL,
    "token" VARCHAR(254) NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY);

create index "idx__user_tokens__token" on "user_tokens" ("token");

create index "idx__user_tokens__user_id" on "user_tokens" ("user_id");


alter table "ideas" add constraint "fk__ideas__user_id"
    foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

alter table "idea_texts" add constraint "fk__idea_texts__idea_id"
    foreign key("idea_id") references "ideas"("id") on update RESTRICT on delete CASCADE;

alter table "idea_images" add constraint "fk__idea_images__idea_id"
    foreign key("idea_id") references "ideas"("id") on update RESTRICT on delete CASCADE;

alter table "idea_signups" add constraint "fk__idea_signups__idea_id"
    foreign key("idea_id") references "ideas"("id") on update RESTRICT on delete CASCADE;

alter table "user_tokens" add constraint "fk__user_tokens__user_id"
    foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;

