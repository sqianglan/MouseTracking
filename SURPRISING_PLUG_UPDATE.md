# Surprising Plug!! Status Update

## Overview
This update adds a new plugging status "Surprising Plug!!" to the Mouse Management System. This status is designed for cases where a plug is discovered unexpectedly, and the exact plug observed date is unknown.

## New Status: "Surprising Plug!!"

### Characteristics:
- **Active Status**: Counts as an active plugging record (prevents new plugging events)
- **Plugged Status**: Considered as a "plugged" status for validation purposes
- **Cannot Mark Empty**: This status cannot be marked as "Empty"
- **Unknown Plug Date**: Sets `plug_observed_date` to "Unknown"
- **Calendar Estimation**: Uses `pairing_start_date` for calendar purposes

### Use Case:
When a female mouse is found to be pregnant but the exact date when the plug was observed is unknown. The system will estimate the plug date using the pairing start date for calendar and harvesting calculations.

## Implementation Details

### Database Changes:
1. **Updated CHECK constraint** in `plugging_history` table to include "Surprising Plug!!"
2. **New status added** to `PLUGGING_STATUSES` constant
3. **Active statuses updated** to include "Surprising Plug!!"

### UI Changes:
1. **New button** "🎉 Surprising Plug!!" in detailed plugging view
2. **Available only for "Not Observed (Waiting for confirmation)" status** records
3. **Distinct styling** with red background to highlight the special nature

### Logic Changes:
1. **Validation functions updated** to include new status in active statuses
2. **Action buttons updated** to show/hide appropriate buttons
3. **Cannot be marked as "Empty"** - excluded from `can_mark_empty()` function
4. **Cannot be deleted** - excluded from delete button logic

## Files Modified:

### Core Files:
- `Modules/tab_plugging.R` - Main plugging logic and UI
- `Modules/modal_add_plugging_event.R` - Modal for adding plugging events
- `Modules/tab_all_mice.R` - All mice tab validation
- `Modules/db_check.R` - Database schema definition

### New Files:
- `update_database_for_surprising_plug.R` - Database migration script
- `update_plugging_status.sql` - SQL migration script
- `SURPRISING_PLUG_UPDATE.md` - This documentation

## Installation Instructions:

### For New Installations:
1. The new status is automatically included in fresh database installations
2. No additional steps required

### For Existing Installations:
1. **Backup your database** before proceeding
2. Run the migration script:
   ```r
   source("update_database_for_surprising_plug.R")
   ```
3. Or manually execute the SQL script:
   ```sql
   -- Run the contents of update_plugging_status.sql
   ```

## Usage:

### Marking a Plug as "Surprising Plug!!":
1. Navigate to the Plugging tab
2. Double-click on a "Not Observed (Waiting for confirmation)" plugging record
3. Click the "🎉 Surprising Plug!!" button
4. Enter expected age for harvesting (optional)
5. Add notes (optional)
6. Click "Confirm Surprising Plug!!"

### Status Flow:
```
Ongoing → Plugged (Manual)
Ongoing → Not Observed (Waiting for confirmation) (Auto)
Not Observed (Waiting for confirmation) → Surprising Plug!! (Manual)
```

### Validation Rules:
- **Males**: Can have up to 2 active plugging records (including "Surprising Plug!!")
- **Females**: Can have up to 1 active plugging record (including "Surprising Plug!!")
- **Cannot be marked as "Empty"**: This status is excluded from empty marking
- **Cannot be deleted**: This status is excluded from deletion

## Audit Trail:
All "Surprising Plug!!" status changes are logged to the audit trail with:
- Old and new values
- Timestamp
- User ID
- Operation details

## Calendar Integration:
When a plug is marked as "Surprising Plug!!":
- `plug_observed_date` is set to "Unknown"
- Calendar calculations use `pairing_start_date` as the reference point
- Expected harvesting dates are calculated based on the pairing start date

## Testing:
After installation, test the new functionality:
1. Create a new plugging event with "Ongoing" status
2. Wait for it to auto-transition to "Not Observed (Waiting for confirmation)" or manually change it
3. Mark it as "Surprising Plug!!"
4. Verify the status change in the audit trail
5. Check that the plug observed date shows as "Unknown"
6. Verify that the record cannot be marked as "Empty"

## Rollback:
If you need to rollback this change:
1. Restore your database backup
2. The system will continue to work with the previous status set
3. No data loss will occur

## Support:
For issues or questions regarding this update, please refer to the main application documentation or contact the development team. 