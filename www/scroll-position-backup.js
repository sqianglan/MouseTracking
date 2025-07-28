// Global object to store scroll positions for different elements
window.scrollPositions = window.scrollPositions || {};

// Suppress bootstrap-datepicker deprecation warnings to prevent console noise
(function() {
  // Store original console methods
  var originalWarn = console.warn;
  var originalLog = console.log;
  var originalError = console.error;
  
  // Override console.warn to filter out datepicker warnings
  console.warn = function() {
    var message = Array.prototype.slice.call(arguments).join(' ');
    if (typeof message === 'string' && 
        (message.includes('DEPRECATED') || 
         message.includes('datepicker') ||
         message.includes('language code') ||
         message.includes('filename doesn\'t follow the convention'))) {
      // Suppress these warnings
      return;
    }
    originalWarn.apply(console, arguments);
  };
  
  // Also override console.log in case warnings come through there
  console.log = function() {
    var message = Array.prototype.slice.call(arguments).join(' ');
    if (typeof message === 'string' && 
        (message.includes('DEPRECATED') || 
         message.includes('datepicker'))) {
      return;
    }
    originalLog.apply(console, arguments);
  };
  
  console.info('Bootstrap-datepicker warnings suppressed for cleaner debugging');
})();

$(document).ready(function() {
  console.log('Document ready - scroll position system initialized');
});

/**
 * Saves scroll position for a DataTable - handles tables with or without IDs
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 */
function saveScrollPosition(tableId) {
  try {
    console.log('=== SAVE SCROLL: Starting for table:', tableId);
    
    var savedTop = 0;
    var savedLeft = 0;
    var containerFound = false;
    
    // For fallback IDs (datatable_0, datatable_1, etc.), find the table by index
    var isIndexBased = tableId.startsWith('datatable_');
    var targetWrapper = null;
    
    if (isIndexBased) {
      var index = parseInt(tableId.replace('datatable_', ''));
      targetWrapper = $('.dataTables_wrapper').eq(index);
      console.log('Using index-based lookup for wrapper #' + index);
    } else {
      // Try to find by actual table ID
      targetWrapper = $('#' + tableId).closest('.dataTables_wrapper');
    }
    
    if (targetWrapper && targetWrapper.length > 0) {
      // Try the scroll body first (this is where vertical scrolling happens)
      var scrollBody = targetWrapper.find('.dataTables_scrollBody');
      if (scrollBody.length > 0) {
        savedTop = scrollBody.scrollTop();
        savedLeft = scrollBody.scrollLeft();
        containerFound = true;
        console.log('Found scrollBody with position:', savedTop, savedLeft);
      } else {
        // Fallback to wrapper itself
        savedTop = targetWrapper.scrollTop();
        savedLeft = targetWrapper.scrollLeft();
        containerFound = true;
        console.log('Found wrapper with position:', savedTop, savedLeft);
      }
    }
    
    // If still no container found, try generic selectors
    if (!containerFound) {
      var containers = [
        '.dataTables_wrapper .dataTables_scrollBody',  // Any DataTables scroll body
        '.dataTables_wrapper',                         // Any DataTables wrapper
      ];
      
      for (var i = 0; i < containers.length; i++) {
        var container = $(containers[i]).first();
        console.log('Trying fallback container:', containers[i], 'Found:', container.length);
        
        if (container.length > 0) {
          savedTop = container.scrollTop();
          savedLeft = container.scrollLeft();
          console.log('Container scroll position:', savedTop, savedLeft);
          
          if (savedTop > 0 || savedLeft > 0 || container.is('.dataTables_scrollBody')) {
            containerFound = true;
            console.log('Found active scroll in:', containers[i]);
            break;
          }
        }
      }
    }
    
    // If no container scroll found, try window scroll
    if (!containerFound) {
      savedTop = $(window).scrollTop();
      savedLeft = $(window).scrollLeft();
      console.log('Using window scroll:', savedTop, savedLeft);
    }
    
    // Save the position
    window.scrollPositions[tableId] = {
      top: savedTop,
      left: savedLeft,
      timestamp: Date.now(),
      source: containerFound ? 'container' : 'window',
      isIndexBased: isIndexBased
    };
    
    console.log('=== SAVE SCROLL: Completed for', tableId + ':', window.scrollPositions[tableId]);
    
  } catch (err) {
    console.error('=== SAVE SCROLL: Error for', tableId + ':', err);
  }
}

/**
 * Restores scroll position for a DataTable with a delay
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 * @param {number} delay - Delay in milliseconds before restoring position
 */
function restoreScrollPosition(tableId, delay) {
  setTimeout(function() {
    try {
      if (!window.scrollPositions || !window.scrollPositions[tableId]) {
        console.log('No saved scroll position found for:', tableId);
        return;
      }
      
      var savedPosition = window.scrollPositions[tableId];
      console.log('=== RESTORE SCROLL: Starting for ' + tableId + ':', savedPosition);
      
      var restored = false;
      var targetWrapper = null;
      
      // Find the target wrapper the same way as in save
      if (savedPosition.isIndexBased) {
        var index = parseInt(tableId.replace('datatable_', ''));
        targetWrapper = $('.dataTables_wrapper').eq(index);
        console.log('Using index-based lookup for restore, wrapper #' + index);
      } else {
        targetWrapper = $('#' + tableId).closest('.dataTables_wrapper');
      }
      
      // Method 1: Try dataTables_scrollBody (when scrollY is enabled)
      if (targetWrapper && targetWrapper.length > 0) {
        var scrollBody = targetWrapper.find('.dataTables_scrollBody');
        if (scrollBody.length > 0) {
          console.log('Found scrollBody element, restoring position');
          scrollBody.scrollTop(savedPosition.top);
          scrollBody.scrollLeft(savedPosition.left);
          
          // Verify the scroll was applied
          var newTop = scrollBody.scrollTop();
          var newLeft = scrollBody.scrollLeft();
          console.log('Applied scroll - Expected:', savedPosition.top, savedPosition.left, 'Actual:', newTop, newLeft);
          
          restored = true;
        } else {
          // Try wrapper itself
          console.log('Found wrapper element, restoring position');
          targetWrapper.scrollTop(savedPosition.top);
          targetWrapper.scrollLeft(savedPosition.left);
          restored = true;
        }
      }
      
      // Method 2: Fallback to any scroll body if target not found
      if (!restored) {
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0) {
          console.log('Using fallback scrollBody element');
          scrollBody.scrollTop(savedPosition.top);
          scrollBody.scrollLeft(savedPosition.left);
          restored = true;
        }
      }
      
      // Method 3: Use window scroll as fallback
      if (!restored && savedPosition.source === 'window') {
        console.log('Using window scroll fallback');
        $(window).scrollTop(savedPosition.top);
        $(window).scrollLeft(savedPosition.left);
        restored = true;
      }
      
      if (restored) {
        console.log('=== RESTORE SCROLL: Successfully restored for ' + tableId);
      } else {
        console.log('=== RESTORE SCROLL: Failed to restore for ' + tableId);
        console.log('Available containers:');
        console.log('- targetWrapper:', targetWrapper ? targetWrapper.length : 0);
        console.log('- scrollBody:', $('.dataTables_scrollBody').length);
      }
      
    } catch (err) {
      console.error('=== RESTORE SCROLL: Error for', tableId + ':', err);
    }
  }, delay || 300);
}

// Register event handler for when DataTables are fully initialized
$(document).on('init.dt', function(e, settings) {
  try {
    var tableId = $(settings.nTable).attr('id');
    if (tableId && window.scrollPositions && window.scrollPositions[tableId]) {
      // Restore position after initialization
      restoreScrollPosition(tableId, 100);
    }
  } catch (err) {
    console.error('Error in init.dt handler:', err);
  }
});

// For DataTables draw events
$(document).on('draw.dt', function(e, settings) {
  try {
    var tableId = $(settings.nTable).attr('id');
    console.log('=== DRAW EVENT for table:', tableId);
    
    // Don't restore automatically on draw - only when explicitly called
    // This prevents conflicts with the manual restore calls
  } catch (err) {
    console.error('Error in draw.dt handler:', err);
  }
});

// Add a specific function to handle DataTable refresh with scroll preservation
function refreshTableWithScrollPreservation(tableId) {
  console.log('=== REFRESH WITH SCROLL PRESERVATION:', tableId);
  
  // Save current position immediately
  var currentPosition = null;
  
  // Try to get current scroll position synchronously
  var containers = [
    '#' + tableId + '_wrapper .dataTables_scrollBody',
    '#' + tableId + '_wrapper',
    '.dataTables_wrapper:has(#' + tableId + ')',
  ];
  
  for (var i = 0; i < containers.length; i++) {
    var container = $(containers[i]);
    if (container.length > 0) {
      var top = container.scrollTop();
      var left = container.scrollLeft();
      if (top > 0 || left > 0 || i === 0) {
        currentPosition = { top: top, left: left, container: containers[i] };
        console.log('Captured position immediately:', currentPosition);
        break;
      }
    }
  }
  
  // Also save using our standard function
  saveScrollPosition(tableId);
  
  // Return an object with a then method for chaining
  return {
    then: function(callback) {
      // Use multiple attempts to restore position
      var attempts = 0;
      var maxAttempts = 5;
      
      function attemptRestore() {
        attempts++;
        console.log('Restore attempt', attempts, 'of', maxAttempts);
        
        if (currentPosition) {
          var container = $(currentPosition.container);
          if (container.length > 0) {
            container.scrollTop(currentPosition.top);
            container.scrollLeft(currentPosition.left);
            console.log('Restored using saved position:', currentPosition);
            if (callback) callback();
            return;
          }
        }
        
        // Fallback to standard restore
        if (window.scrollPositions && window.scrollPositions[tableId]) {
          restoreScrollPosition(tableId, 0);
          if (callback) callback();
          return;
        }
        
        // Try again if we haven't reached max attempts
        if (attempts < maxAttempts) {
          setTimeout(attemptRestore, 200);
        } else {
          console.log('Failed to restore scroll position after', maxAttempts, 'attempts');
          if (callback) callback();
        }
      }
      
      // Start first attempt after a short delay
      setTimeout(attemptRestore, 300);
    }
  };
}

// Register Shiny message handlers with improved error handling
$(document).on('shiny:connected', function() {
  // Custom handler for evaluating JavaScript code
  Shiny.addCustomMessageHandler('eval', function(code) {
    try {
      eval(code);
    } catch (error) {
      console.error('Error evaluating JavaScript:', error);
      console.error('Code that failed:', code);
    }
  });
  
  console.log('Scroll position handlers registered successfully');
  console.log('Checking for existing DataTables...');
  
  // Debug any existing tables
  setTimeout(function() {
    if (typeof debugScrollState === 'function') {
      debugScrollState();
    }
  }, 1000);
});

// Global functions to save/restore scroll for any DataTable on the page
function saveScrollForAllTables() {
  try {
    console.log('=== SAVING SCROLL FOR ALL DATATABLES ===');
    
    if (typeof $ === 'undefined') {
      console.error('jQuery not available');
      return;
    }
    
    var tablesFound = 0;
    $('.dataTables_wrapper').each(function(index) {
      try {
        var wrapper = $(this);
        var table = wrapper.find('table').first();
        var tableId = table.attr('id');
        
        // If no ID, create a fallback ID based on index
        if (!tableId) {
          tableId = 'datatable_' + index;
          console.log('Table has no ID, using fallback:', tableId);
        }
        
        tablesFound++;
        console.log('Found DataTable #' + tablesFound + ':', tableId);
        saveScrollPosition(tableId);
      } catch (err) {
        console.error('Error processing table:', err);
      }
    });
    console.log('Total DataTables processed:', tablesFound);
  } catch (err) {
    console.error('Error in saveScrollForAllTables:', err);
  }
}

function restoreScrollForAllTables() {
  try {
    console.log('=== RESTORING SCROLL FOR ALL DATATABLES ===');
    
    if (typeof $ === 'undefined') {
      console.error('jQuery not available');
      return;
    }
    
    var tablesFound = 0;
    var tablesRestored = 0;
    $('.dataTables_wrapper').each(function(index) {
      try {
        var wrapper = $(this);
        var table = wrapper.find('table').first();
        var tableId = table.attr('id');
        
        // If no ID, use the same fallback ID as in save
        if (!tableId) {
          tableId = 'datatable_' + index;
          console.log('Table has no ID, using fallback:', tableId);
        }
        
        tablesFound++;
        console.log('Found DataTable #' + tablesFound + ':', tableId);
        
        if (window.scrollPositions && window.scrollPositions[tableId]) {
          console.log('Has saved position:', window.scrollPositions[tableId]);
          restoreScrollPosition(tableId, 50);
          tablesRestored++;
        } else {
          console.log('No saved position for:', tableId);
        }
      } catch (err) {
        console.error('Error processing table for restore:', err);
      }
    });
    console.log('Total DataTables found:', tablesFound, 'Tables with saved positions:', tablesRestored);
  } catch (err) {
    console.error('Error in restoreScrollForAllTables:', err);
  }
}

// Debug function to inspect current state
function debugScrollState() {
  console.log('=== CURRENT SCROLL STATE DEBUG ===');
  console.log('Available DataTables:');
  
  $('.dataTables_wrapper').each(function(index) {
    var wrapper = $(this);
    var table = wrapper.find('table').first();
    var tableId = table.attr('id');
    var scrollBody = wrapper.find('.dataTables_scrollBody');
    
    console.log('Table #' + (index + 1) + ':');
    console.log('  ID:', tableId);
    console.log('  Has scrollBody:', scrollBody.length > 0);
    
    if (scrollBody.length > 0) {
      console.log('  ScrollBody position:', {
        top: scrollBody.scrollTop(),
        left: scrollBody.scrollLeft(),
        height: scrollBody.height(),
        scrollHeight: scrollBody.prop('scrollHeight')
      });
    }
    
    console.log('  Wrapper position:', {
      top: wrapper.scrollTop(),
      left: wrapper.scrollLeft()
    });
    
    if (window.scrollPositions && window.scrollPositions[tableId]) {
      console.log('  Saved position:', window.scrollPositions[tableId]);
    } else {
      console.log('  No saved position');
    }
  });
  
  console.log('Window scroll:', {
    top: $(window).scrollTop(),
    left: $(window).scrollLeft()
  });
}
