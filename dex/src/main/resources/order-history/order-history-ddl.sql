CREATE TABLE IF NOT EXISTS orders (
  id                VARCHAR (44) PRIMARY KEY,
  "type"            SMALLINT,
  sender_address    VARCHAR (44),
  sender_public_key VARCHAR (44),
  amount_asset_id   VARCHAR (44),
  price_asset_id    VARCHAR (44),
  fee_asset_id      VARCHAR (44),
  side              SMALLINT,
  price             NUMERIC (27, 8),
  amount            NUMERIC (27, 8),
  "timestamp"       TIMESTAMP(3) WITH TIME ZONE,
  expiration        TIMESTAMP(3) WITH TIME ZONE,
  fee               NUMERIC (27, 8),
  created           TIMESTAMP(3) WITH TIME ZONE,
  closed_at         TIMESTAMP(3) WITH TIME ZONE
);

CREATE TABLE IF NOT EXISTS events (
  order_id         VARCHAR(44),
  event_type       SMALLINT,
  "timestamp"      TIMESTAMP(3) WITH TIME ZONE,
  price            NUMERIC (27, 8),
  filled           NUMERIC (27, 8),
  total_filled     NUMERIC (27, 8),
  fee_filled       NUMERIC (27, 8),
  fee_total_filled NUMERIC (27, 8),
  status           SMALLINT,
  reason           SMALLINT NOT NULL DEFAULT 0,
  PRIMARY KEY (order_id, total_filled, status)
);
