# Mouse Management System - Feature Summary

## Overview
The Mouse Management System is a comprehensive web-based application built with R Shiny, designed specifically for tracking and managing mouse colony plugging records in animal research facilities. This system addresses the critical gap in current webtools used by Animal Facilities by providing a dedicated platform for breeding pair management and plugging event tracking.

## Core Features

### 🏠 Home Dashboard
- **Welcome Interface**: Modern, intuitive landing page with visual plugging status workflow diagram
- **Quick Actions**: Direct access to search animals and add new animals functionality
- **Visual Workflow**: Embedded SVG diagram illustrating the standard plugging procedure workflow
- **Responsive Design**: Modern UI with gradient backgrounds and professional styling

### 🐭 All Mice Management
- **Comprehensive Search & Filter**: Advanced search capabilities with wildcard support (* and ?)
- **Real-time Filtering**: Filter by ASU ID, Animal ID, Gender, Breeding Line, Responsible Person, Stock Category, and Status
- **Interactive DataTable**: Sortable, searchable table with pagination and row selection
- **Bulk Operations**: Edit multiple selected mice simultaneously
- **Mouse History**: Double-click functionality to view detailed modification history
- **Status Indicators**: Visual status lights for live/deceased mice
- **Export Capabilities**: Download filtered data in various formats

### 🐭⚤🐭 Plugging Management
- **Breeding Pair Management**: Create and manage breeding pairs or trios
- **Plugging Event Tracking**: Record and track plugging events through multiple status stages:
  - Ongoing
  - Plugged
  - Plug Confirmed
  - Not Pregnant
  - Not Observed (Waiting for confirmation)
  - Empty
  - Not Observed (Confirmed)
  - Surprising Plug!!
- **Status Workflow**: Visual progression through plugging stages with automatic date tracking
- **Historical Records**: View archived and deleted plugging records
- **Cross-module Integration**: Seamless integration with All Mice tab for data consistency

### 📅 Event Calendar
- **Interactive Calendar View**: Monthly calendar visualization of plugging events
- **Event Color Coding**: Different colors for various plugging statuses
- **Navigation Controls**: Easy month/year navigation with Apple-style design
- **Statistics Panel**: Real-time statistics for current month/year
- **Export Functionality**: Export calendar data and statistics
- **Responsive Design**: Modern UI with backdrop blur effects and gradient styling

### 🔍 Advanced Search & Import
- **Single Entry Form**: Add individual mice with comprehensive validation
- **Excel Import**: Bulk import from Excel files with column mapping
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

### 📊 Data Management
- **SQLite Database**: Lightweight, reliable database with full ACID compliance
- **Schema Management**: Automatic database initialization and schema updates
- **Data Consistency**: Referential integrity and constraint enforcement
- **Performance Optimization**: Indexed queries and efficient data retrieval
- **Timezone Support**: User-configurable timezone settings

### 🎨 User Experience
- **Modern UI Design**: Professional interface with gradient backgrounds and modern styling
- **Responsive Layout**: Works seamlessly on different screen sizes
- **Intuitive Navigation**: Clear tab-based navigation with emoji icons
- **Loading States**: Visual feedback during data operations
- **Error Handling**: Comprehensive error messages and validation feedback
- **Accessibility**: High contrast design and keyboard navigation support

### 🔧 Technical Features
- **Modular Architecture**: Clean separation of concerns with dedicated modules
- **Reactive Programming**: Real-time data updates across all components
- **Cross-module Communication**: Shared state management for consistent data
- **Hot-reloading Support**: Development-friendly with automatic module reloading
- **Performance Monitoring**: Efficient data handling for large datasets

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
- **Browser Support**: Modern web browsers with JavaScript enabled
- **Storage**: SQLite database with automatic backup capabilities

## Copyright Notice
**Copyright (c) 2025 Qiang Lan. All rights reserved.**

This software is proprietary and confidential. Unauthorized copying, distribution, or use of this software is strictly prohibited. For inquiries regarding licensing or usage rights, please contact qiang.lan@bristol.ac.uk.

## Contact Information
- **Developer**: Qiang Lan
- **Institution**: University of Bristol
- **Email**: qiang.lan@bristol.ac.uk
- **Purpose**: Mouse colony management for Animal Facility operations

---

*This system represents a significant advancement in animal research facility management, providing a dedicated solution for the critical but previously underserved area of mouse breeding and plugging record management.* 