CREATE TABLE IF NOT EXISTS followings (
  user_id VARCHAR NOT NULL,
  following_id VARCHAR NOT NULL,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user_id, following_id)
);