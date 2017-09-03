BEGIN;

CREATE TYPE profession AS ENUM 
(
    'alchemy',
    'engineering',
    'leatherworking',
    'blacksmith',
    'enchanting',
    'inscription',
    'tailoring',
    'skinning',
    'jewelcrafting',
    'herbalism',
    'mining',
    'world'
);


ALTER TABLE items ADD COLUMN from_profession profession;
ALTER TABLE items ALTER COLUMN from_profession SET NOT NULL;

COMMIT;