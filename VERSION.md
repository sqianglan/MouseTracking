# Version: beta 1.41 - Update Summary

## 🎉 Latest Updates (beta 1.41)

### 🗃️ Database Management System Overhaul
- **Simplified naming**: Always use `mice_colony.db` as main database name (eliminates "0.db" issues)
- **Smart backup system**: Automatic backups with intelligent naming (`mice_colony_YYYYMMDD_HHMMSS.db`)
- **Pre-import backups**: Automatic backup creation before database imports (`mice_colony_YYYYMMDD_HHMMSS_preimport.db`)
- **Session-only switching**: Temporary database viewing without permanent changes
- **Unified database list**: View main database and all backups in single interface
- **Real-time updates**: Database list refreshes after backup/import operations
- **Cross-platform paths**: Proper database location handling for development and packaged apps

### 🔄 Import & Backup Improvements
- **Streamlined imports**: Replace main database with automatic backup of previous version
- **Automatic startup backups**: Create backups when app initializes (6-hour intervals)
- **Cleanup functionality**: Remove old backups with refresh capability
- **Clear user feedback**: Improved messaging for database operations
- **Safe packaging**: Database locations optimized for app distribution

### 🏗️ Infrastructure Enhancements
- **Cleaner directory structure**: Removed unused `databases` folder
- **Configuration management**: Session-based database switching via config files
- **Development workflow**: Separate database locations for development vs production
- **Error handling**: Robust validation and fallback mechanisms

---

# Version: beta 1.40 - Update Summary

## 🎉 Previous Updates (beta 1.40)

### 📊 Body Weight Chart Enhancements
- **Y-axis optimization**: Body weight charts now always start from 0 for better visual comparison
- **Enhanced chart integration**: Added body weight charts to plugging event details modals
- **Interactive "Add Body Weight" buttons**: Direct access to weight management from plugging details
- **Chart height improvements**: Fixed x-axis label display issues with increased container heights
- **Multi-location integration**: Body weight charts available in both plugging tab and calendar tab modals

### 🎨 Modal Design Improvements
- **Optimized layout**: Redesigned plugging details modal with left-right column structure (info panels vs. chart)
- **Clean styling**: Removed background gradients from calendar tab body weight containers
- **Better space utilization**: Improved modal sizing and component arrangement
- **Consistent UI**: Unified body weight chart appearance across all modules

---

# Version: beta 1.31 - Update Summary

## 🎉 Previous Updates (beta 1.31)

### 📅 Calendar & Surprising Plug Improvements
- Fixed surprising plug text visibility in calendar (white text on gray background)
- Enhanced date calculation for surprising plugs to use actual plug dates when available
- Added surprising emoji (😱) to calendar events and legend for better visual identification
- Fixed date parsing error for records with "Unknown" dates in edit modal
- Improved robust date validation in plugging event editing

---

# Version: beta 1.30 - Update Summary

## 🎉 Previous Updates (beta 1.30)

### 📅 Plugging Events Calendar Function
- Separated the category for Plugged and Plug Confirmed
- Increased the modal size for better visibility
- Added direct link to show plugging details from calendar legends
- Added clickable filter buttons for event categories and stages
- Enhanced export functionality to include only filtered events

---

# Version: beta 1.20 - Update Summary

## 🎉 Previous Updates (beta 1.20)

### 🗑️ Enhanced Deleted Record Management
- Added distinctive trash bin symbol (🗑️) for deleted mice records
- Improved visual differentiation between deceased and deleted statuses
- Fixed ASU ID extraction for deleted records in double-click functionality
- Conditional deleted status legend (only shown when system is unlocked)

### 🔧 System Lock & Security Improvements
- Refined system lock functionality with proper authorization warnings
- Deleted status indicators only visible when system is unlocked and "All" selected
- Enhanced security controls for deletion operations
- Improved user interface for system lock/unlock operations

### 🎨 User Interface Enhancements
- Changed "Both" to "All" in status search dropdown for better clarity
- Dynamic legend display based on system lock state
- Enhanced JavaScript for robust ASU ID extraction from complex HTML
- Improved status indicator styling and alignment

### 📝 Add Animals Modal System
- Comprehensive modal system for adding single animals
- Excel import functionality with duplicate detection
- Custom ASU ID handling for duplicate records
- Advanced validation for animal data entry

### 🔍 Search & Filter Improvements
- Updated status filtering with clearer naming conventions
- Enhanced search functionality with wildcard support
- Better user feedback for search operations
- Improved data table state management

---


## 🎉 Previous Updates (beta 1.20)

### 🗑️ Enhanced Deleted Record Management
- Added distinctive trash bin symbol (🗑️) for deleted mice records
- Improved visual differentiation between deceased and deleted statuses
- Fixed ASU ID extraction for deleted records in double-click functionality
- Conditional deleted status legend (only shown when system is unlocked)

### 🔧 System Lock & Security Improvements
- Refined system lock functionality with proper authorization warnings
- Deleted status indicators only visible when system is unlocked and "All" selected
- Enhanced security controls for deletion operations
- Improved user interface for system lock/unlock operations

### 🎨 User Interface Enhancements
- Changed "Both" to "All" in status search dropdown for better clarity
- Dynamic legend display based on system lock state
- Enhanced JavaScript for robust ASU ID extraction from complex HTML
- Improved status indicator styling and alignment

### 📝 Add Animals Modal System
- Comprehensive modal system for adding single animals
- Excel import functionality with duplicate detection
- Custom ASU ID handling for duplicate records
- Advanced validation for animal data entry

### 🔍 Search & Filter Improvements
- Updated status filtering with clearer naming conventions
- Enhanced search functionality with wildcard support
- Better user feedback for search operations
- Improved data table state management

---

## 🎉 Previous Updates (beta 1.10)

### ✨ Body Weight Tracking System
- Added comprehensive body weight tracking for individual mice
- Interactive weight charts with trend visualization
- Body weight indicators in All Mice table with SVG icons
- Integrated weight management within mouse history modal
- Data validation for weight measurements and dates

### 🎨 Visual Improvements
- Enhanced status indicators with proper alignment
- SVG-based body weight icons for better visibility
- Improved column layout and spacing in data tables
- Professional visual design with consistent styling

### 🔧 Technical Enhancements
- Fixed database table references for body weight functions
- Optimized SQL queries for better performance
- Enhanced data validation and error handling
- Improved audit trail for body weight operations

### 🐭 Mouse Management Features
- Body weight status indicators show at-a-glance tracking status
- Clickable weight history with detailed measurement records
- Easy add/edit/delete functionality for weight data
- Seamless integration with existing mouse management workflow

---

🚀 This version focuses on enhanced data tracking and improved user experience.

**Contact:** qiang.lan@bristol.ac.uk for feedback or feature requests.