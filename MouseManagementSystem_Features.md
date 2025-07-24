# Mouse Management System - Feature Summary

## Overview
The Mouse Management System is a comprehensive web-based application built with R Shiny, designed specifically for tracking and managing mouse colony plugging records used. **This is not a replacement of facility mouse managment syste, rather then for personal or small group using.** This system addresses the critical gap in current webtools used by Animal Facilities by providing a clear tracking recordes for breeding or plugging mice and provide a summarizing table to highlight the successful rate of a given mouse.

### 🏠 Home Dashboard
- **Global Lock Functionality**: The system features a  global lock mechanism to prevent accidental deletions and modifications. Unlocking requires explicit user action, ensuring data safety and auditability. Meanwhile, all the deleting actions are performed as soft-delete. It only set the status as delet, without removing it from database.
- **Time Zone Awareness**: All date and time records are automatically adjusted to the user's local time zone, providing accurate event tracking and consistency across different users and locations. It is possible to set the timezone during travelling to match the working place.
- **Single Entry Form**: Add individual mice with comprehensive validation
- **Excel Import**: Bulk import from Excel files with column mapping with manul confirmation


### 🐭 All Mice Management
- **Comprehensive Search & Filter**: Advanced search capabilities with wildcard support (* and ?)
- **Real-time Filtering**: Filter by ASU ID, Animal ID, Gender, Breeding Line, Responsible Person, Stock Category, and Status. ASU ID was automatically extracted from Animal ID by detecting the longest number separted by "/", or " ".
- **Interactive DataTable**: Sortable, searchable table with pagination and row selection
- **Bulk Operations**: Edit multiple selected mice simultaneously
- **Mouse History**: Double-click functionality to view summary of past or ongoing plugging events and final stages. This would be helpful to decide the fertility of the mice. 
- **Status Indicators**: Visual status lights for live/deceased mice
- **Add Plugging Pairs/Trios** By selecting maxium 3 mice and at least one male and one female, and clicking "Add Plugging Event" button, will generate plugging events for each female. The sytem will check the status of each mice, and pop out warning message if the mice is not alive, busy (two or more active plugging event for male, and one for female). 

### 🐭⚤🐭 Plugging Management
- **Mice Pairing Management**: Create and manage Pairing pairs or trios
- **Plugging Event Tracking**: Record and track plugging events through multiple status stages (🔵 = active plugging event):
  - Ongoing 🔵
  - Plugged 🔵
  - Plug Confirmed 🔵
  - Not Pregnant
  - Not Observed (Waiting for confirmation) 🔵
  - Empty
  - Not Observed (Confirmed)
  - Surprising Plug!! 🔵
- **Status Workflow**: Visual progression through plugging stages with automatic date tracking
- **Show Archived/Deleted Records**: View archived and deleted plugging records
- **Cross-module Integration**: Seamless integration with All Mice tab for data consistency

### 📅 Event Calendar
- **Interactive Calendar View**: Monthly calendar visualization summarizing the plugging events
- **Event Color Coding**: Different colors for various plugging statuses
- **Statistics Panel**: Real-time statistics for current month/year
- **Export Functionality**: Export calendar data into your own

### 🔍 Advanced Search & Import
- **Duplicate Detection**: Intelligent duplicate checking with conflict resolution options:
  - Skip duplicates
  - Modify (generate new ASU ID)
  - Overwrite existing records
  - Keep both records
- **Data Validation**: Comprehensive validation for all input fields
- **Audit Trail**: Complete tracking of all import operations

### 🛡️ Security & Data Integrity
- **Global Lock System**: Protection against accidental deletions with unlock mechanism
- **Comprehensive Audit Trail**: Detailed logging of all database operations including:
  - Field-level change tracking
  - User session tracking
  - IP address logging
  - Operation details
  - Timestamp recording
- **Data Validation**: Multi-level validation for all input data
- **SQL Injection Protection**: Input sanitization and validation
- **Backup & Recovery**: Database integrity checks and recovery mechanisms


## Database Schema

### Mice Stock Table
- **Primary Key**: ASU ID (unique identifier)
- **Core Fields**: Animal ID, Gender, Date of Birth, Genotype, Breeding Line
- **Management Fields**: Responsible Person, Project Code, Protocol, Study Plan
- **Status Tracking**: Live/Deceased/Deleted status with timestamps
- **Metadata**: Creation and update timestamps, import source tracking

### Plugging History Table
- **Breeding Pair Tracking**: Male and Female ASU IDs
- **Event Management**: Plugging dates, status progression, notes
- **Audit Fields**: Creation and modification timestamps
- **Status Workflow**: Complete plugging lifecycle tracking

### Audit Trail Table
- **Comprehensive Logging**: All database operations tracked
- **Field-level Changes**: Detailed change tracking for individual fields
- **User Tracking**: Session ID, IP address, user agent information
- **Operation Details**: Contextual information about each operation

## System Requirements
- **R Environment**: R 4.0+ with Shiny framework
- **Dependencies**: tidyverse, DT, calendR, ggsci, DBI, RSQLite
- **Storage**: SQLite database with automatic backup capabilities

## Copyright Notice
**Copyright (c) 2025 Qiang Lan. This project is licensed under the BSD 3-Clause License.**

This software is proprietary and confidential. Unauthorized copying, distribution, or use of this software is strictly prohibited. For inquiries regarding licensing or usage rights, please contact qiang.lan@bristol.ac.uk.

## Contact Information
- **Developer**: Qiang Lan
- **Institution**: University of Bristol
- **Email**: qiang.lan@bristol.ac.uk
- **Purpose**: Mouse colony management for Animal Facility operations

---

*This system represents a significant advancement in animal research facility management, providing a dedicated solution for the critical but previously underserved area of mouse  plugging record tracking.* 