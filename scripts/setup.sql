CREATE OR REPLACE FUNCTION update_timestamps()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- TODO: may need to separate types and tables
DROP TYPE IF EXISTS streaming_platform;
CREATE TYPE streaming_platform AS ENUM ('Twitch', 'Youtube');

DROP TABLE IF EXISTS streamers;
CREATE TABLE IF NOT EXISTS streamers
  ( id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    handle TEXT NOT NULL,
    platform streaming_platform NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );

CREATE OR REPLACE TRIGGER update_streamer_timestamps
BEFORE UPDATE ON streamers
FOR EACH ROW
EXECUTE PROCEDURE update_timestamps();
