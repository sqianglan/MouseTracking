# Mouse Management System

A comprehensive web-based application for tracking and managing mouse colony plugging records in animal research facilities. Built with R Shiny, this system provides a dedicated platform for breeding pair management and plugging event tracking.

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Data Structure](#data-structure)
- [User Guide](#user-guide)
- [Tutorial](#tutorial)
- [Technical Documentation](#technical-documentation)
- [Troubleshooting](#troubleshooting)
- [Contact](#contact)

## 🎯 Overview

The Mouse Management System addresses the critical gap in current webtools used by Animal Facilities by providing a dedicated platform for:
- **Mouse Colony Management**: Track individual mice with comprehensive metadata
- **Breeding Pair Management**: Create and manage breeding pairs or trios
- **Plugging Event Tracking**: Monitor plugging events through multiple status stages
- **Audit Trail**: Complete tracking of all database operations
- **Data Import/Export**: Bulk operations with Excel integration

## ✨ Features

### 🏠 Home Dashboard
- Modern welcome interface with visual plugging status workflow
- Quick access to search animals and add new animals
- Embedded SVG diagram illustrating standard plugging procedures
- Responsive design with professional styling

### 🐭 All Mice Management
- **Advanced Search & Filter**: Wildcard support (* and ?) for flexible searching
- **Real-time Filtering**: Filter by ASU ID, Animal ID, Gender, Breeding Line, Responsible Person, Stock Category, and Status
- **Interactive DataTable**: Sortable, searchable table with pagination
- **Bulk Operations**: Edit multiple selected mice simultaneously
- **Mouse History**: Double-click to view detailed modification history
- **Status Indicators**: Visual status lights for live/deceased mice

### 🐭⚤🐭 Plugging Management
- **Breeding Pair Management**: Create and manage breeding pairs or trios
- **Plugging Event Tracking**: Record and track events through status stages:
  - Ongoing
  - Plugged
  - Plug Confirmed
  - Not Pregnant
  - Not Observed (Waiting for confirmation)
  - Empty
  - Not Observed (Confirmed)
  - Surprising Plug!!
- **Status Workflow**: Visual progression through plugging stages
- **Historical Records**: View archived and deleted plugging records

### 📅 Event Calendar
- Interactive monthly calendar visualization
- Event color coding for different plugging statuses
- Navigation controls with modern design
- Real-time statistics for current month/year
- Export functionality for calendar data

### 🛡️ Security & Data Integrity
- **Global Lock System**: Protection against accidental deletions
- **Comprehensive Audit Trail**: Detailed logging of all operations
- **Data Validation**: Multi-level validation for all input data
- **SQL Injection Protection**: Input sanitization and validation

## 🚀 Installation

### Prerequisites
- R 4.0 or higher
- RStudio (recommended)
- Modern web browser with JavaScript enabled

### Setup Instructions

1. **Clone or Download the Repository**
   ```bash
   git clone [repository-url]
   cd MouseManagement2
   ```

2. **Install Required R Packages**
   ```r
   # Run the installation script
   source("install_packages.R")
   ```

3. **Configure Database Settings** (Optional)
   ```r
   # Set environment variables for custom database location
   Sys.setenv("MOUSE_DB_DIR" = "/path/to/database/directory")
   Sys.setenv("MOUSE_DB_NAME" = "your_database_name.db")
   ```

4. **Launch the Application**
   ```r
   # Method 1: Run directly in R
   source("app.R")
   
   # Method 2: Use the shell script (Unix/Linux/macOS)
   ./run_shiny.sh
   
   # Method 3: Open app.R in RStudio and click "Run App"
   ```

## 📊 Data Structure

### Database Schema

#### Mice Stock Table (`mice_stock`)
| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| `asu_id` | TEXT | Primary key - unique identifier | PRIMARY KEY |
| `animal_id` | TEXT | Alternative animal identifier | |
| `ear_mark` | TEXT | Ear marking information | |
| `gender` | TEXT | Mouse gender | CHECK (Male/Female) |
| `dob` | DATE | Date of birth | NOT NULL |
| `genotype` | TEXT | Genetic information | |
| `transgenes` | TEXT | Transgenic information | |
| `strain` | TEXT | Mouse strain | DEFAULT 'C57BL/6J' |
| `breeding_line` | TEXT | Breeding line designation | |
| `dam` | TEXT | Mother's ASU ID | |
| `sire` | TEXT | Father's ASU ID | |
| `cage_id` | TEXT | Cage identifier | |
| `room` | TEXT | Room location | |
| `project_code` | TEXT | Research project code | |
| `responsible_person` | TEXT | Person responsible for the mouse | |
| `protocol` | TEXT | Research protocol | |
| `study_plan` | TEXT | Study plan reference | CHECK (SP2500090, SP2500083, SP2500082, SP2500081) |
| `stock_category` | TEXT | Stock classification | CHECK (Experiment, Breeding, Charles River) |
| `status` | TEXT | Current status | CHECK (Alive, Deceased, Deleted) |
| `date_of_death` | DATE | Date of death (if applicable) | |
| `age_at_death_weeks` | REAL | Age at death in weeks | |
| `max_severity` | TEXT | Maximum severity level | |
| `procedure` | TEXT | Procedure information | |
| `stage` | TEXT | Current stage | |
| `deceased_timestamp` | TIMESTAMP | Death timestamp | |
| `notes` | TEXT | Additional notes | |
| `imported_from_excel` | BOOLEAN | Import source flag | DEFAULT 0 |
| `date_created` | TIMESTAMP | Record creation time | DEFAULT CURRENT_TIMESTAMP |
| `last_updated` | TIMESTAMP | Last update time | DEFAULT CURRENT_TIMESTAMP |

#### Plugging History Table (`plugging_history`)
| Field | Type | Description |
|-------|------|-------------|
| `id` | INTEGER | Primary key - auto-increment |
| `male_id` | TEXT | Male mouse ASU ID |
| `female_id` | TEXT | Female mouse ASU ID |
| `plugging_date` | DATE | Date of plugging event |
| `status` | TEXT | Current plugging status |
| `notes` | TEXT | Additional notes |
| `created_at` | TIMESTAMP | Record creation time |
| `updated_at` | TIMESTAMP | Last update time |

#### Audit Trail Table (`audit_trail`)
| Field | Type | Description |
|-------|------|-------------|
| `id` | INTEGER | Primary key - auto-increment |
| `table_name` | TEXT | Affected table name |
| `record_id` | TEXT | Affected record identifier |
| `action` | TEXT | Operation type (INSERT, UPDATE, DELETE) |
| `field_name` | TEXT | Specific field changed (for updates) |
| `old_value` | TEXT | Previous value |
| `new_value` | TEXT | New value |
| `old_values` | TEXT | JSON of all old values |
| `new_values` | TEXT | JSON of all new values |
| `timestamp` | TIMESTAMP | Operation timestamp |
| `user_id` | TEXT | User identifier |
| `session_id` | TEXT | Session identifier |
| `ip_address` | TEXT | Source IP address |
| `user_agent` | TEXT | Browser/client information |
| `operation_details` | TEXT | Additional operation context |

### Plugging Status Workflow

```
Ongoing → Plugged → Plug Confirmed → [Multiple Outcomes]
                                    ├── Not Pregnant
                                    ├── Not Observed (Waiting)
                                    ├── Empty
                                    ├── Not Observed (Confirmed)
                                    ├── Surprising Plug!!
                                    └── Collected
```

## 📖 User Guide

### Getting Started

1. **Launch the Application**
   - Open the application in your web browser
   - You'll see the Home dashboard with quick action buttons

2. **System Lock Status**
   - The system starts in "Locked" mode for safety
   - Click "🔓 Unlock System" to enable deletion functions
   - A warning dialog will appear - confirm to proceed

3. **Timezone Configuration**
   - Click "🌍 Set Timezone" to configure your local timezone
   - This ensures all timestamps are displayed correctly

### Managing Mice

#### Adding Individual Mice
1. Navigate to "🐭 All Mice" tab
2. Click "➕ Add Animals" button
3. Fill in the required fields:
   - **ASU ID**: Unique identifier (required)
   - **Gender**: Male or Female (required)
   - **Date of Birth**: Required
   - **Breeding Line**: Optional but recommended
   - **Genotype**: Genetic information
   - **Responsible Person**: Person in charge
   - **Stock Category**: Experiment, Breeding, or Charles River
4. Click "Add Mouse" to save

#### Bulk Import from Excel
1. Click "📁 Import from Excel" button
2. Select your Excel file
3. Map columns to database fields
4. Choose duplicate handling strategy:
   - Skip duplicates
   - Modify (generate new ASU ID)
   - Overwrite existing records
   - Keep both records
5. Review and confirm import

#### Searching and Filtering
1. Use the search panel on the left side
2. Enter search terms with wildcards:
   - `*` for multiple characters
   - `?` for single character
3. Select filters from dropdown menus
4. Click "🔍 Search" to apply filters
5. Use "🗑️ Clear Search" to reset

#### Viewing Mouse History
1. Double-click any row in the mice table
2. A modal will show the complete modification history
3. View all changes with timestamps and details

### Managing Plugging Events

#### Creating a Plugging Event
1. Navigate to "🐭⚤🐭 Plugging" tab
2. Click "➕ Add Plugging Event" button
3. Select male and female mice from dropdown lists
4. Enter plugging date and notes
5. Click "Add Plugging Event" to save

#### Updating Plugging Status
1. Find the plugging event in the table
2. Click the status button in the "Actions" column
3. Select the new status from the dropdown
4. Add any relevant notes
5. Click "Update Status" to confirm

#### Status Progression
- **Ongoing**: Initial status when breeding pair is set up
- **Plugged**: Male has been introduced to female
- **Plug Confirmed**: Plug has been observed
- **Not Pregnant**: Female is not pregnant
- **Empty**: Female was pregnant but no longer is
- **Not Observed**: No plug was observed
- **Surprising Plug!!**: Unexpected plug observation
- **Collected**: Pups have been collected

#### Viewing Historical Records
1. Check "Show Archived Records" to see completed events
2. Check "Show Deleted Records" to see removed events
3. Use "🔄 Refresh Table" to update the view

### Calendar View

#### Accessing the Calendar
1. Click "📅 Event Calendar" button in the Plugging tab
2. Navigate between months using arrow buttons
3. Click on dates to view events for that day

#### Calendar Features
- Color-coded events by status
- Monthly statistics panel
- Export functionality for calendar data
- Responsive design for different screen sizes

## 🎓 Tutorial

### Tutorial 1: Setting Up Your First Breeding Pair

**Objective**: Create a breeding pair and track their plugging events

**Steps**:
1. **Add Mice to the System**
   - Go to "🐭 All Mice" tab
   - Click "➕ Add Animals"
   - Add a male mouse:
     - ASU ID: `M001`
     - Gender: Male
     - Date of Birth: `2024-01-15`
     - Breeding Line: `Control`
     - Genotype: `WT`
     - Responsible Person: `Your Name`
   - Add a female mouse:
     - ASU ID: `F001`
     - Gender: Female
     - Date of Birth: `2024-01-20`
     - Breeding Line: `Control`
     - Genotype: `WT`
     - Responsible Person: `Your Name`

2. **Create a Plugging Event**
   - Go to "🐭⚤🐭 Plugging" tab
   - Click "➕ Add Plugging Event"
   - Select Male: `M001`
   - Select Female: `F001`
   - Plugging Date: Today's date
   - Notes: `Initial breeding setup`
   - Click "Add Plugging Event"

3. **Track Status Progression**
   - The event starts with "Ongoing" status
   - After introducing the male, update to "Plugged"
   - When you observe a plug, update to "Plug Confirmed"
   - Based on outcome, update to appropriate final status

### Tutorial 2: Bulk Import and Management

**Objective**: Import multiple mice from Excel and manage them efficiently

**Steps**:
1. **Prepare Excel File**
   - Create Excel file with columns: ASU_ID, Gender, DOB, Breeding_Line, Genotype, Responsible_Person
   - Add sample data for 10-20 mice

2. **Import Data**
   - Go to "🐭 All Mice" tab
   - Click "📁 Import from Excel"
   - Select your Excel file
   - Map columns to database fields
   - Choose duplicate handling strategy
   - Review and confirm import

3. **Search and Filter**
   - Use search panel to find specific mice
   - Filter by breeding line or responsible person
   - Use wildcards for flexible searching

4. **Bulk Operations**
   - Select multiple mice using checkboxes
   - Use "✏️ Edit Selected" for bulk updates
   - Update responsible person or other fields for all selected mice

### Tutorial 3: Advanced Plugging Management

**Objective**: Manage complex breeding scenarios with multiple females

**Steps**:
1. **Create Breeding Trio**
   - Add a male and two females to the system
   - Create plugging events for male + female1 and male + female2
   - Track both events independently

2. **Status Management**
   - Update statuses based on observations
   - Use notes to record specific details
   - Monitor both events in the plugging history table

3. **Calendar Integration**
   - Click "📅 Event Calendar" to view all events
   - Navigate between months to see event distribution
   - Export calendar data for reporting

## 🔧 Technical Documentation

### Architecture Overview

The application follows a modular architecture with the following components:

```
app.R (Main Application)
├── Modules/
│   ├── audit_trail.R (Audit logging system)
│   ├── db_check.R (Database initialization and management)
│   ├── modal_add_plugging_event.R (Plugging event modal)
│   ├── modal_mice_history.R (Mouse history modal)
│   ├── tab_all_mice.R (Mice management interface)
│   ├── tab_plugging.R (Plugging management interface)
│   ├── tab_calendar_events.R (Calendar view)
│   └── validation.R (Data validation functions)
├── www/ (Static assets)
└── install_packages.R (Dependency installation)
```

### Key Functions

#### Database Management
- `initialize_db()`: Creates database and tables
- `add_plugging_tables()`: Adds plugging-related tables
- `create_enhanced_audit_trail_table()`: Sets up audit trail

#### Audit Trail
- `log_audit_action()`: Logs database operations
- `log_field_change()`: Tracks field-level changes
- `get_mouse_modification_history()`: Retrieves mouse history

#### Data Validation
- `validate_asu_id()`: Validates ASU ID format
- `validate_date()`: Validates date inputs
- `check_duplicates()`: Identifies duplicate records

### Configuration Options

#### Environment Variables
```r
# Database location
Sys.setenv("MOUSE_DB_DIR" = "/path/to/database")
Sys.setenv("MOUSE_DB_NAME" = "database_name.db")
```

#### Database Settings
- Default database: `mice_colony_test.db`
- Table name: `mice_stock`
- Automatic schema updates
- SQLite with full ACID compliance

### Performance Considerations

- **Indexing**: Database tables are indexed for optimal query performance
- **Reactive Programming**: Efficient data updates using Shiny's reactive framework
- **Lazy Loading**: Modules are loaded on-demand
- **Connection Pooling**: Database connections are managed efficiently

## 🛠️ Troubleshooting

### Common Issues

#### Application Won't Start
1. **Check R Version**: Ensure R 4.0+ is installed
2. **Install Dependencies**: Run `source("install_packages.R")`
3. **Check File Permissions**: Ensure write access to database directory
4. **Port Conflicts**: Try different port if 3838 is busy

#### Database Errors
1. **Corrupted Database**: Delete database file and restart (data will be lost)
2. **Permission Issues**: Check write permissions for database directory
3. **Schema Issues**: Run database initialization manually

#### Import Problems
1. **Excel Format**: Ensure Excel file has proper column headers
2. **Data Types**: Check that dates are in YYYY-MM-DD format
3. **Duplicate ASU IDs**: Use duplicate handling options in import dialog

#### Performance Issues
1. **Large Datasets**: Use filters to reduce data load
2. **Browser Issues**: Clear browser cache and refresh
3. **Memory Usage**: Restart R session if memory usage is high

### Error Messages

| Error | Cause | Solution |
|-------|-------|----------|
| "Database connection failed" | Database file not accessible | Check file permissions and path |
| "ASU ID already exists" | Duplicate ASU ID | Use different ASU ID or handle duplicates |
| "Invalid date format" | Date not in YYYY-MM-DD format | Correct date format in input |
| "Required field missing" | Missing required data | Fill in all required fields |

### Getting Help

1. **Check Logs**: Look for error messages in R console
2. **Database Inspection**: Use SQLite browser to inspect database directly
3. **Contact Support**: Email qiang.lan@bristol.ac.uk for technical support

## 📞 Contact

- **Developer**: Qiang Lan
- **Institution**: University of Bristol
- **Email**: qiang.lan@bristol.ac.uk
- **Purpose**: Mouse colony management for Animal Facility operations

## 📄 License

**Copyright (c) 2025 Qiang Lan. All rights reserved.**

This software is proprietary and confidential. Unauthorized copying, distribution, or use of this software is strictly prohibited. For inquiries regarding licensing or usage rights, please contact qiang.lan@bristol.ac.uk.

---

*This system represents a significant advancement in animal research facility management, providing a dedicated solution for the critical but previously underserved area of mouse breeding and plugging record management.* 