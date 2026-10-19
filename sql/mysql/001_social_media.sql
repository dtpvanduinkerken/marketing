CREATE TABLE IF NOT EXISTS social_accounts (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  platform VARCHAR(32) NOT NULL,
  external_account_id VARCHAR(128) NOT NULL,
  account_name VARCHAR(255) NOT NULL,
  account_timezone VARCHAR(64) NOT NULL DEFAULT 'Europe/Amsterdam',
  is_active TINYINT(1) NOT NULL DEFAULT 1,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (id),
  UNIQUE KEY uq_social_account (platform, external_account_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- statement-break
CREATE TABLE IF NOT EXISTS social_daily_stats (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  account_id BIGINT UNSIGNED NOT NULL,
  metric_date DATE NOT NULL,
  followers_count BIGINT NULL,
  fan_count BIGINT NULL,
  media_views BIGINT NULL,
  post_engagements BIGINT NULL,
  metric_period VARCHAR(16) NOT NULL DEFAULT 'day',
  source_end_time DATETIME NULL,
  collected_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id),
  UNIQUE KEY uq_social_daily (account_id, metric_date),
  KEY idx_social_daily_date (metric_date),
  CONSTRAINT fk_social_daily_account FOREIGN KEY (account_id)
    REFERENCES social_accounts(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- statement-break
CREATE TABLE IF NOT EXISTS social_posts (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  account_id BIGINT UNSIGNED NOT NULL,
  external_post_id VARCHAR(191) NOT NULL,
  post_type VARCHAR(64) NULL,
  message TEXT NULL,
  permalink_url TEXT NULL,
  picture_url TEXT NULL,
  published_at DATETIME NOT NULL,
  first_seen_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (id),
  UNIQUE KEY uq_social_post (account_id, external_post_id),
  KEY idx_social_post_published (published_at),
  CONSTRAINT fk_social_post_account FOREIGN KEY (account_id)
    REFERENCES social_accounts(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- statement-break
CREATE TABLE IF NOT EXISTS social_post_stats (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  post_id BIGINT UNSIGNED NOT NULL,
  captured_date DATE NOT NULL,
  media_views BIGINT NULL,
  likes BIGINT NULL,
  reactions_total BIGINT NULL,
  comments BIGINT NULL,
  shares BIGINT NULL,
  engagement_total BIGINT NULL,
  collected_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id),
  UNIQUE KEY uq_social_post_stats (post_id, captured_date),
  KEY idx_social_post_stats_date (captured_date),
  CONSTRAINT fk_social_post_stats_post FOREIGN KEY (post_id)
    REFERENCES social_posts(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- statement-break
CREATE TABLE IF NOT EXISTS social_sync_logs (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  platform VARCHAR(32) NOT NULL,
  account_external_id VARCHAR(128) NOT NULL,
  started_at DATETIME NOT NULL,
  finished_at DATETIME NULL,
  status VARCHAR(16) NOT NULL,
  daily_rows INT NOT NULL DEFAULT 0,
  post_rows INT NOT NULL DEFAULT 0,
  error_code VARCHAR(64) NULL,
  error_message VARCHAR(1000) NULL,
  PRIMARY KEY (id),
  KEY idx_social_sync_latest (platform, account_external_id, started_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

