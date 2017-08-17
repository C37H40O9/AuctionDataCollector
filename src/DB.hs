{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DB 
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- TODO read all configuration and credentials from file
connInfo = ConnectInfo {
    connectHost = "127.0.0.1"
  , connectPort = 5432
  , connectUser = "wow_adp"
  , connectPassword = "pass"
  , connectDatabase = "wowadp_db"
  }

--conn = connect connInfo

initTableBid conn =execute_ conn [sql|
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
            )
                |]
{-
wowadp_db
wow_adp@pass

DB scheme

CREATE TABLE bid
(
    bid_date timestamptz NOT NULL,
    server_slug text NOT NULL,
    item_id integer NOT NULL REFERENCES items,
    item_count integer NOT NULL,
    min_w bigint NOT NULL,
    bot_w bigint NOT NULL,
    p_25 bigint NOT NULL,
    p_50 bigint NOT NULL,
    p_75 bigint NOT NULL,
    top_w bigint NOT NULL,
    max_w bigint NOT NULL,
    UNIQUE (bid_date, server_slug, item_id)
);

CREATE TABLE buyout
(
    buyout_date timestamptz NOT NULL,
    server_slug text NOT NULL,
    item_id integer NOT NULL REFERENCES items,
    item_count integer NOT NULL,
    min_w bigint NOT NULL,
    bot_w bigint NOT NULL,
    p_25 bigint NOT NULL,
    p_50 bigint NOT NULL,
    p_75 bigint NOT NULL,
    top_w bigint NOT NULL,
    max_w bigint NOT NULL,
    UNIQUE (buyout_date, server_slug, item_id)
);
    
CEATE TABLE recipes
(
    recipe_id integer PRIMARY KEY,
    item_id integer REFERENCE items,
    recipe_name text
);

CREATE TABLE items
(
    item_id integer PRIMARY KEY
);

CREATE TABLE recipe_items
(
    item_id integer REFERENCES items,
    recipe_id integer REFERENCES recipes ON DELETE CASCADE,
    quantity integer,
    PRIMARY KEY (item_id, recipe_id)
);

CREATE TABLE last_modified
(
    upd_date timestamptz,
    server_slug text PRIMARY KEY
)

-}