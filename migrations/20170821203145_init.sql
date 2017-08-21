BEGIN;

CREATE TABLE IF NOT EXISTS items
(
    item_id int4 PRIMARY KEY,
    item_name text
);

CREATE TABLE IF NOT EXISTS bid
(
    bid_date timestamptz NOT NULL,
    server_slug text NOT NULL,
    item_id int4 NOT NULL REFERENCES items,
    item_count int4 NOT NULL,
    min_w int8 NOT NULL,
    bot_w int8 NOT NULL,
    p_25 int8 NOT NULL,
    p_50 int8 NOT NULL,
    p_75 int8 NOT NULL,
    top_w int8 NOT NULL,
    max_w int8 NOT NULL,
    UNIQUE (bid_date, server_slug, item_id)
);

CREATE TABLE IF NOT EXISTS buyout
(
    buyout_date timestamptz NOT NULL,
    server_slug text NOT NULL,
    item_id int4 NOT NULL REFERENCES items,
    item_count int4 NOT NULL,
    min_w int8 NOT NULL,
    bot_w int8 NOT NULL,
    p_25 int8 NOT NULL,
    p_50 int8 NOT NULL,
    p_75 int8 NOT NULL,
    top_w int8 NOT NULL,
    max_w int8 NOT NULL,
    UNIQUE (buyout_date, server_slug, item_id)
);
    


CREATE TABLE IF NOT EXISTS recipes
(
    recipe_id int4 PRIMARY KEY,
    item_id int4 REFERENCES items,
    recipe_name text
);


CREATE TABLE IF NOT EXISTS recipe_items
(
    item_id int4 REFERENCES items,
    recipe_id int4 REFERENCES recipes ON DELETE CASCADE,
    quantity int4,
    PRIMARY KEY (item_id, recipe_id)
);

CREATE TABLE IF NOT EXISTS last_modified
(
    upd_date timestamptz,
    server_slug text PRIMARY KEY
);

COMMIT;