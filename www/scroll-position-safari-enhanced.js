// Enhanced scroll-position.js with Safari-specific fixes
// Global object to store scroll positions for different elements
window.scrollPositions = window.scrollPositions || {};

// Detect Safari browser
function isSafari() {
  const ua = navigator.userAgent;
  return /^((?!chrome|android).)*safari/i.test(ua) || /iPad|iPhone|iPod/.test(ua);
}

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
  console.log('Browser detected as Safari:', isSafari());
});

/**
 * Saves scroll position for a DataTable - handles tables with or without IDs
 * Enhanced with Safari-specific handling
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 */
function saveScrollPosition(tableId) {
  try {
    console.log('=== SAVE SCROLL: Starting for table:', tableId);
    
    var savedTop = 0;
    var savedLeft = 0;
    var containerFound = false;
    var method = 'none';
    
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
        method = 'scrollBody';
        console.log('Found scrollBody with position:', savedTop, savedLeft);
        
        // Safari-specific: Additional scroll detection methods
        if (isSafari() && savedTop === 0 && savedLeft === 0) {
          // Method 1: Try the native DOM element
          var nativeElement = scrollBody[0];
          if (nativeElement) {
            var nativeTop = nativeElement.scrollTop;
            var nativeLeft = nativeElement.scrollLeft;
            console.log('Safari native DOM scroll:', nativeTop, nativeLeft);
            if (nativeTop > 0 || nativeLeft > 0) {
              savedTop = nativeTop;
              savedLeft = nativeLeft;
              method = 'scrollBody-native';
            }
          }
          
          // Method 2: Try checking all possible scroll containers
          var allScrollContainers = scrollBody.find('*').addBack();
          allScrollContainers.each(function() {
            var elem = $(this);
            var elemTop = elem.scrollTop();
            var elemLeft = elem.scrollLeft();
            if (elemTop > 0 || elemLeft > 0) {
              console.log('Safari found scroll in element:', this.tagName, this.className, 'position:', elemTop, elemLeft);
              if (savedTop === 0) savedTop = elemTop;
              if (savedLeft === 0) savedLeft = elemLeft;
              method = 'scrollBody-deep';
              return false; // break
            }
          });
          
          // Method 3: Check for webkit-specific scroll properties
          if (savedTop === 0 && nativeElement && nativeElement.webkitScrollTop !== undefined) {
            savedTop = nativeElement.webkitScrollTop;
            console.log('Safari webkit scroll top:', savedTop);
            method = 'scrollBody-webkit';
          }
        }
      } else {
        // Fallback to wrapper itself
        savedTop = targetWrapper.scrollTop();
        savedLeft = targetWrapper.scrollLeft();
        containerFound = true;
        method = 'wrapper';
        console.log('Found wrapper with position:', savedTop, savedLeft);
        
        // Safari-specific wrapper checks
        if (isSafari() && savedTop === 0 && savedLeft === 0) {
          var nativeWrapper = targetWrapper[0];
          if (nativeWrapper) {
            savedTop = nativeWrapper.scrollTop || 0;
            savedLeft = nativeWrapper.scrollLeft || 0;
            console.log('Safari native wrapper scroll:', savedTop, savedLeft);
            method = 'wrapper-native';
          }
        }
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
          
          // Safari-specific: Try native DOM access if jQuery returns 0
          if (isSafari() && savedTop === 0 && savedLeft === 0) {
            var nativeContainer = container[0];
            if (nativeContainer) {
              var nativeTop = nativeContainer.scrollTop;
              var nativeLeft = nativeContainer.scrollLeft;
              console.log('Safari native container scroll:', nativeTop, nativeLeft);
              if (nativeTop > 0 || nativeLeft > 0) {
                savedTop = nativeTop;
                savedLeft = nativeLeft;
              }
            }
            
            // Also try all child elements that might have scroll
            container.find('*').each(function() {
              var elem = $(this);
              var elemTop = elem.scrollTop();
              var elemLeft = elem.scrollLeft();
              if (elemTop > 0 || elemLeft > 0) {
                console.log('Safari found child scroll:', this.tagName, this.className, elemTop, elemLeft);
                if (savedTop === 0) savedTop = elemTop;
                if (savedLeft === 0) savedLeft = elemLeft;
                return false; // break
              }
            });
          }
          
          if (savedTop > 0 || savedLeft > 0 || container.is('.dataTables_scrollBody')) {
            containerFound = true;
            method = containers[i];
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
      method = 'window';
      console.log('Using window scroll:', savedTop, savedLeft);
    }
    
    // Safari-specific: Last resort scroll detection
    if (isSafari() && savedTop === 0 && savedLeft === 0) {
      console.log('Safari last resort: searching all scrollable elements');
      
      // Find all elements with scroll
      $('*').each(function() {
        var elem = $(this);
        var elemTop = elem.scrollTop();
        var elemLeft = elem.scrollLeft();
        
        if (elemTop > 0 || elemLeft > 0) {
          console.log('Safari found ANY scroll element:', this.tagName, this.className || this.id, 'position:', elemTop, elemLeft);
          if (savedTop === 0) savedTop = elemTop;
          if (savedLeft === 0) savedLeft = elemLeft;
          method = 'safari-any-element';
          containerFound = true;
          return false; // break from each
        }
      });
      
      // Try document.documentElement and document.body
      if (savedTop === 0 && savedLeft === 0) {
        var docElem = document.documentElement;
        var bodyElem = document.body;
        
        console.log('Safari checking document elements:');
        console.log('  documentElement scroll:', docElem.scrollTop, docElem.scrollLeft);
        console.log('  body scroll:', bodyElem.scrollTop, bodyElem.scrollLeft);
        
        if (docElem.scrollTop > 0 || docElem.scrollLeft > 0) {
          savedTop = docElem.scrollTop;
          savedLeft = docElem.scrollLeft;
          method = 'safari-documentElement';
        } else if (bodyElem.scrollTop > 0 || bodyElem.scrollLeft > 0) {
          savedTop = bodyElem.scrollTop;
          savedLeft = bodyElem.scrollLeft;
          method = 'safari-body';
        }
      }
    }
    
    // Save the position with additional Safari metadata
    window.scrollPositions[tableId] = {
      top: savedTop,
      left: savedLeft,
      timestamp: Date.now(),
      source: containerFound ? 'container' : 'window',
      isIndexBased: isIndexBased,
      method: method,
      browser: isSafari() ? 'safari' : 'other'
    };
    
    console.log('=== SAVE SCROLL: Completed for', tableId + ':', window.scrollPositions[tableId]);
    
  } catch (err) {
    console.error('=== SAVE SCROLL: Error for', tableId + ':', err);
  }
}

/**
 * Restores scroll position for a DataTable with a delay
 * Enhanced with Safari-specific handling including requestAnimationFrame
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 * @param {number} delay - Delay in milliseconds before restoring position
 */
function restoreScrollPosition(tableId, delay) {
  var actualDelay = delay || (isSafari() ? 500 : 300); // Longer delay for Safari
  
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
      
      // Safari-specific restoration function
      function applySafariScroll(element, top, left) {
        if (isSafari()) {
          // For Safari, try multiple methods
          
          // Method 1: Direct scroll
          element.scrollTop(top);
          element.scrollLeft(left);
          
          // Method 2: Using requestAnimationFrame (Safari sometimes needs this)
          if (typeof requestAnimationFrame !== 'undefined') {
            requestAnimationFrame(function() {
              element.scrollTop(top);
              element.scrollLeft(left);
            });
          }
          
          // Method 3: Delayed second attempt (Safari rendering quirk)
          setTimeout(function() {
            element.scrollTop(top);
            element.scrollLeft(left);
          }, 50);
          
        } else {
          // Standard scroll for other browsers
          element.scrollTop(top);
          element.scrollLeft(left);
        }
      }
      
      // Method 1: Try dataTables_scrollBody (when scrollY is enabled)
      if (targetWrapper && targetWrapper.length > 0) {
        var scrollBody = targetWrapper.find('.dataTables_scrollBody');
        if (scrollBody.length > 0) {
          console.log('Found scrollBody element, restoring position');
          applySafariScroll(scrollBody, savedPosition.top, savedPosition.left);
          
          // Verify the scroll was applied (with tolerance for Safari)
          setTimeout(function() {
            var newTop = scrollBody.scrollTop();
            var newLeft = scrollBody.scrollLeft();
            console.log('Applied scroll - Expected:', savedPosition.top, savedPosition.left, 'Actual:', newTop, newLeft);
            
            // Safari tolerance check
            var tolerance = isSafari() ? 10 : 2;
            if (Math.abs(newTop - savedPosition.top) <= tolerance || savedPosition.top === 0) {
              restored = true;
              console.log('✓ Successfully restored via scrollBody');
            }
          }, 100);
          
          restored = true; // Assume success for now
        } else {
          // Try wrapper itself
          console.log('Found wrapper element, restoring position');
          applySafariScroll(targetWrapper, savedPosition.top, savedPosition.left);
          restored = true;
        }
      }
      
      // Method 2: Fallback to any scroll body if target not found
      if (!restored) {
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0) {
          console.log('Using fallback scrollBody element');
          applySafariScroll(scrollBody, savedPosition.top, savedPosition.left);
          restored = true;
        }
      }
      
      // Method 3: Use window scroll as fallback
      if (!restored && savedPosition.source === 'window') {
        console.log('Using window scroll fallback');
        applySafariScroll($(window), savedPosition.top, savedPosition.left);
        restored = true;
      }
      
      // Safari-specific: Additional verification and retry
      if (isSafari() && restored) {
        setTimeout(function() {
          var scrollBody = $('.dataTables_scrollBody').first();
          if (scrollBody.length > 0) {
            var currentTop = scrollBody.scrollTop();
            if (Math.abs(currentTop - savedPosition.top) > 10 && savedPosition.top > 0) {
              console.log('Safari retry: Current position', currentTop, 'differs from expected', savedPosition.top);
              applySafariScroll(scrollBody, savedPosition.top, savedPosition.left);
            }
          }
        }, 200);
      }
      
      if (restored) {
        console.log('=== RESTORE SCROLL: Successfully initiated for ' + tableId);
      } else {
        console.log('=== RESTORE SCROLL: Failed to restore for ' + tableId);
        console.log('Available containers:');
        console.log('- targetWrapper:', targetWrapper ? targetWrapper.length : 0);
        console.log('- scrollBody:', $('.dataTables_scrollBody').length);
      }
      
    } catch (err) {
      console.error('=== RESTORE SCROLL: Error for', tableId + ':', err);
    }
  }, actualDelay);
}

// Register event handler for when DataTables are fully initialized
$(document).on('init.dt', function(e, settings) {
  try {
    var tableId = $(settings.nTable).attr('id');
    if (tableId && window.scrollPositions && window.scrollPositions[tableId]) {
      // Restore position after initialization with Safari-appropriate delay
      var delay = isSafari() ? 200 : 100;
      restoreScrollPosition(tableId, delay);
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

// Enhanced functions with Safari support
function refreshTableWithScrollPreservation(tableId) {
  console.log('=== REFRESH WITH SCROLL PRESERVATION:', tableId);
  
  // Save current position immediately
  var currentPosition = null;
  
  try {
    // Get current scroll position before refresh
    var scrollBody = $('.dataTables_scrollBody').first();
    if (scrollBody.length > 0) {
      currentPosition = {
        top: scrollBody.scrollTop(),
        left: scrollBody.scrollLeft()
      };
    }
    
    console.log('Current position before refresh:', currentPosition);
    
    // Save the position using our function
    saveScrollPosition(tableId);
    
    // Return a function that can restore the position
    return function(callback) {
      var maxAttempts = isSafari() ? 5 : 3; // More attempts for Safari
      var attempts = 0;
      
      function attemptRestore() {
        attempts++;
        console.log('Restore attempt', attempts, 'of', maxAttempts);
        
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0 && currentPosition) {
          // For Safari, use enhanced scroll function
          if (isSafari()) {
            scrollBody.scrollTop(currentPosition.top);
            scrollBody.scrollLeft(currentPosition.left);
            
            requestAnimationFrame(function() {
              scrollBody.scrollTop(currentPosition.top);
              scrollBody.scrollLeft(currentPosition.left);
            });
          } else {
            scrollBody.scrollTop(currentPosition.top);
            scrollBody.scrollLeft(currentPosition.left);
          }
          
          console.log('Restored to position:', currentPosition);
          if (callback) callback();
          return;
        }
        
        // Try again if we haven't reached max attempts
        if (attempts < maxAttempts) {
          var retryDelay = isSafari() ? 300 : 200;
          setTimeout(attemptRestore, retryDelay);
        } else {
          console.log('Failed to restore scroll position after', maxAttempts, 'attempts');
          if (callback) callback();
        }
      }
      
      // Start first attempt after a delay appropriate for the browser
      var initialDelay = isSafari() ? 400 : 300;
      setTimeout(attemptRestore, initialDelay);
    };
  } catch (err) {
    console.error('Error in refreshTableWithScrollPreservation:', err);
    return function(callback) {
      if (callback) callback();
    };
  }
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
  console.log('Safari mode:', isSafari() ? 'enabled' : 'disabled');
  
  // Debug any existing tables
  setTimeout(function() {
    if (typeof debugScrollState === 'function') {
      debugScrollState();
    }
  }, 1000);
});

// Global functions to save/restore scroll for any DataTable on the page
// Enhanced with Safari-specific handling
function saveScrollForAllTables() {
  try {
    console.log('=== SAVING SCROLL FOR ALL DATATABLES ===');
    console.log('Safari mode:', isSafari());
    
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
    console.log('Safari mode:', isSafari());
    
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
          var delay = isSafari() ? 100 : 50; // Adjusted delay for Safari
          restoreScrollPosition(tableId, delay);
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
  console.log('=== SCROLL STATE DEBUG ===');
  console.log('Browser: Safari =', isSafari());
  console.log('jQuery available:', typeof $ !== 'undefined');
  console.log('DataTables available:', typeof $.fn.DataTable !== 'undefined');
  console.log('Saved positions:', window.scrollPositions);
  
  $('.dataTables_wrapper').each(function(index) {
    var wrapper = $(this);
    var table = wrapper.find('table').first();
    var tableId = table.attr('id') || 'datatable_' + index;
    var scrollBody = wrapper.find('.dataTables_scrollBody');
    
    console.log('Table #' + index + ':');
    console.log('  ID:', tableId);
    console.log('  Has scrollBody:', scrollBody.length > 0);
    
    if (scrollBody.length > 0) {
      console.log('  Current scroll - top:', scrollBody.scrollTop(), 'left:', scrollBody.scrollLeft());
    }
    
    if (window.scrollPositions && window.scrollPositions[tableId]) {
      console.log('  Saved position:', window.scrollPositions[tableId]);
    }
  });
}

// Export debug function to global scope
window.debugScrollState = debugScrollState;
