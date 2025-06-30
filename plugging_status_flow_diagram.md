# Plugging Status Logic Flow Diagram

## Status Definitions

### Active Statuses (Can be updated via Update button)
- **Ongoing**: Initial status when plugging event is created
- **Plugged**: Plug was observed
- **Plug Confirmed**: Plug confirmed by staff
- **Not Observed (Waiting for confirmation)**: Auto-updated when pairing period ends
- **Surprising Plug!!**: Special status with unknown plug date

### Final Statuses (Cannot be updated, only deleted)
- **Empty**: Plug was empty
- **Not Pregnant**: Mouse is not pregnant
- **Not Observed (Confirmed)**: Confirmed no plug was observed
- **Deleted**: Record marked as deleted

## Status Transition Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           PLUGGING STATUS FLOW                              │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────┐
│   START     │
│  (New Pair) │
└─────┬───────┘
      │
      ▼
┌─────────────┐
│   Ongoing   │ ◄─── Initial status when plugging event is created
│             │
│ Update to:  │
│ • Not Observed (Waiting for confirmation) │
│ • Plugged                                 │
│ • Plug Confirmed                          │
│ • Not Pregnant                            │
│ • Empty                                   │
│ • Surprising Plug!!                       │
└─────┬───────┘
      │
      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    AUTO-UPDATE TRIGGER                                     │
│  When pairing_end_date < today AND status = "Ongoing"                     │
│  → Auto-update to "Not Observed (Waiting for confirmation)"               │
└─────────────────────────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────┐
│   Plugged   │ ◄─── Plug was observed
│             │
│ Update to:  │
│ • Plug Confirmed                          │
│ • Not Pregnant                            │
│ • Empty                                   │
│ • Surprising Plug!!                       │
└─────┬───────┘
      │
      ▼
┌─────────────┐
│Plug Confirmed│ ◄─── Plug confirmed by staff
│             │
│ Update to:  │
│ • Not Pregnant                            │
│ • Empty                                   │
│ • Surprising Plug!!                       │
└─────┬───────┘
      │
      ▼
┌─────────────┐
│Surprising   │ ◄─── Special status (unknown plug date)
│Plug!!       │
│             │
│ Update to:  │
│ • Plug Confirmed                          │
│ • Not Pregnant                            │
│ • Not Observed (Confirmed)                │
└─────┬───────┘
      │
      ▼
┌─────────────┐
│Not Observed │ ◄─── Auto-updated from Ongoing
│(Waiting for │
│confirmation)│
│             │
│ Update to:  │
│ • Not Observed (Confirmed)                │
│ • Plug Confirmed                          │
│ • Not Pregnant                            │
│ • Surprising Plug!!                       │
└─────┬───────┘
      │
      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FINAL STATUSES                                   │
│                    (Cannot be updated, only deleted)                       │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Empty     │    │Not Pregnant │    │Not Observed │    │  Deleted    │
│             │    │             │    │(Confirmed)  │    │             │
│ • Plug was  │    │ • Mouse is  │    │ • Confirmed │    │ • Record    │
│   empty     │    │   not preg. │    │   no plug   │    │   marked    │
│ • Can only  │    │ • Can only  │    │   observed  │    │   deleted   │
│   be deleted│    │   be deleted│    │ • Can only  │    │ • Hidden    │
└─────────────┘    └─────────────┘    │   be deleted│    │   from view │
                                      └─────────────┘    └─────────────┘
```

## Button Logic

### Update Button (Green)
- **Shows for**: All active statuses
- **Function**: Opens modal with status transition options
- **Active statuses**: `["Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!"]`

### Delete Button (Red)
- **Shows for**: Non-active statuses (except "Deleted")
- **Function**: Marks record as "Deleted"
- **Non-active statuses**: `["Empty", "Not Pregnant", "Not Observed (Confirmed)"]`

### Surprising Plug!! Button (Special)
- **Shows for**: Only "Not Observed (Waiting for confirmation)" status
- **Function**: Directly sets status to "Surprising Plug!!" with "Unknown" plug date

## Status Transition Rules

### From "Ongoing"
```
Ongoing → Not Observed (Waiting for confirmation)  # Auto-update when pairing ends
Ongoing → Plugged                                  # Manual update
Ongoing → Plug Confirmed                           # Manual update
Ongoing → Not Pregnant                             # Manual update
Ongoing → Empty                                    # Manual update
Ongoing → Surprising Plug!!                        # Manual update
```

### From "Plugged"
```
Plugged → Plug Confirmed                           # Manual update
Plugged → Not Pregnant                             # Manual update
Plugged → Empty                                    # Manual update
Plugged → Surprising Plug!!                        # Manual update
```

### From "Plug Confirmed"
```
Plug Confirmed → Not Pregnant                      # Manual update
Plug Confirmed → Empty                             # Manual update
Plug Confirmed → Surprising Plug!!                 # Manual update
```

### From "Not Observed (Waiting for confirmation)"
```
Not Observed (Waiting for confirmation) → Not Observed (Confirmed)  # Manual update
Not Observed (Waiting for confirmation) → Plug Confirmed            # Manual update
Not Observed (Waiting for confirmation) → Not Pregnant              # Manual update
Not Observed (Waiting for confirmation) → Surprising Plug!!         # Manual update
```

### From "Surprising Plug!!"
```
Surprising Plug!! → Plug Confirmed                 # Manual update
Surprising Plug!! → Not Pregnant                   # Manual update
Surprising Plug!! → Not Observed (Confirmed)       # Manual update
```

## Special Behaviors

### Auto-Update Logic
```javascript
// Triggered daily
if (status === "Ongoing" && pairing_end_date < today) {
    status = "Not Observed (Waiting for confirmation)"
    notes += "Auto-update: pairing period ended"
}
```

### Surprising Plug!! Logic
```javascript
// When setting to Surprising Plug!!
if (new_status === "Surprising Plug!!") {
    plug_observed_date = "Unknown"
    notes += "Status updated to Surprising Plug!! - plug_observed_date set to Unknown"
}
```

### Empty Status Logic
```javascript
// When setting to Empty
if (new_status === "Empty") {
    plug_observed_date = current_date
    notes += "Status updated to Empty"
}
```

## Calendar Events

### Statuses that generate calendar events:
- **Plugged**: Uses plug_observed_date
- **Plug Confirmed**: Uses plug_observed_date
- **Not Observed (Waiting for confirmation)**: Uses pairing_start_date + 1
- **Surprising Plug!!**: Uses pairing_start_date + 1

### Statuses that do NOT generate calendar events:
- **Ongoing**: No plug date yet
- **Empty**: Final status
- **Not Pregnant**: Final status
- **Not Observed (Confirmed)**: Final status
- **Deleted**: Hidden from system

## Busy Status Indicator

### Females
- **Busy**: 1+ active plugging records
- **Free**: 0 active plugging records

### Males
- **Busy**: 2+ active plugging records
- **Free**: 0-1 active plugging records

### Active records include:
`["Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!"]` 