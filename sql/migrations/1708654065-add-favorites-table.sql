CREATE TABLE IF NOT EXISTS favorites (
  user_id VARCHAR NOT NULL,
  article_id VARCHAR NOT NULL,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user_id, article_id)
);