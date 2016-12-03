create table "user_auth_infos" (
    "provider" VARCHAR(32) NOT NULL,
    "uid" VARCHAR(128) NOT NULL,
    "nickname" VARCHAR(254),
    "user_id" BIGINT NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL,
    "id" BIGSERIAL NOT NULL PRIMARY KEY
);

create index "idx__user_auth_infos__user_id" on "user_auth_infos" ("user_id");

alter table "user_auth_infos" add constraint "fk__user_auth_infos__user_id"
    foreign key("user_id") references "users"("id") on update RESTRICT on delete CASCADE;