-- Update plugging_history table to include "Surprising Plug!!" status
-- This script updates the CHECK constraint for the plugging_status column

-- First, backup existing data
CREATE TABLE IF NOT EXISTS plugging_history_backup AS SELECT * FROM plugging_history;

-- Drop the existing table
DROP TABLE plugging_history;

-- Recreate the table with the new status in the CHECK constraint
CREATE TABLE plugging_history (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  male_id TEXT NOT NULL,
  female_id TEXT NOT NULL,
  cage_id TEXT,
  pairing_start_date DATE,
  pairing_end_date DATE,
  plug_observed_date TEXT,
  plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)', 'Surprising Plug!!')),
  notes TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  expected_age_for_harvesting TEXT
);

-- Restore the data
INSERT INTO plugging_history SELECT * FROM plugging_history_backup;

-- Drop the backup table
DROP TABLE plugging_history_backup;

-- Verify the update
SELECT DISTINCT plugging_status FROM plugging_history ORDER BY plugging_status; 