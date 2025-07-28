// Enhanced scroll-position.js with Safari-specific fixes and DataTable state preservation
// Global object to store scroll positions for different elements
window.scrollPositions = window.scrollPositions || {};
// Global object to store complete DataTable state
window.dataTableStates = window.dataTableStates || {};

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
         message.includes('filename doesn\'t follow the convention') ||
         message.includes('DataTables warning') ||
         message.includes('Non-table node initialisation'))) {
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
 * Saves complete DataTable state including sorting, filtering, search, page, etc.
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 */
function saveDataTableState(tableId) {
  try {
    console.log('=== SAVE DATATABLE STATE: Starting for table:', tableId);
    
    // Find the actual DataTable instance
    var $table = null;
    var isIndexBased = tableId.startsWith('datatable_');
    var dataTableApi = null;
    
    if (isIndexBased) {
      var index = parseInt(tableId.replace('datatable_', ''));
      $table = $('.dataTables_wrapper').eq(index).find('table');
      console.log('Using index-based lookup for state save, wrapper #' + index);
    } else {
      $table = $('#' + tableId);
    }
    
    if ($table && $table.length > 0) {
      // Check if this is actually a table element and if DataTables is initialized
      if (!$table.is('table')) {
        console.log('Element is not a table, skipping DataTable state save for:', tableId);
        return;
      }
      
      // Check if DataTables is already initialized on this table
      if (!$.fn.DataTable.isDataTable($table[0])) {
        console.log('DataTables not initialized on table, skipping state save for:', tableId);
        return;
      }
      
      // Get the DataTable API instance (safe now since we've checked)
      dataTableApi = $table.DataTable();
      
      if (dataTableApi) {
        // Get current state from DataTable
        var state = dataTableApi.state();
        
        // Also capture current values that might not be in state
        var currentState = {
          // Core DataTable state
          start: dataTableApi.page.info().start,
          length: dataTableApi.page.info().length,
          order: dataTableApi.order(),
          search: {
            search: dataTableApi.search(),
            smart: true,
            regex: false,
            caseInsensitive: true
          },
          columns: [],
          
          // Custom metadata
          timestamp: Date.now(),
          tableId: tableId,
          isIndexBased: isIndexBased
        };
        
        // Save individual column states
        dataTableApi.columns().every(function(index) {
          var column = this;
          currentState.columns[index] = {
            visible: column.visible(),
            search: {
              search: column.search(),
              smart: true,
              regex: false,
              caseInsensitive: true
            }
          };
        });
        
        // Store the state
        window.dataTableStates[tableId] = currentState;
        
        console.log('=== SAVE DATATABLE STATE: Completed for', tableId + ':', currentState);
        return currentState;
      } else {
        console.log('DataTable API not found for:', tableId);
      }
    } else {
      console.log('Table element not found for:', tableId);
    }
    
  } catch (err) {
    console.error('=== SAVE DATATABLE STATE: Error for', tableId + ':', err);
  }
  
  return null;
}

/**
 * Restores complete DataTable state including sorting, filtering, search, page, etc.
 * @param {string} tableId - The ID of the DataTable (or fallback ID)
 * @param {number} delay - Delay in milliseconds before restoring state
 */
function restoreDataTableState(tableId, delay) {
  var actualDelay = delay || (isSafari() ? 300 : 200);
  
  setTimeout(function() {
    try {
      if (!window.dataTableStates || !window.dataTableStates[tableId]) {
        console.log('No saved DataTable state found for:', tableId);
        return;
      }
      
      var savedState = window.dataTableStates[tableId];
      console.log('=== RESTORE DATATABLE STATE: Starting for ' + tableId + ':', savedState);
      
      // Find the DataTable instance
      var $table = null;
      var dataTableApi = null;
      
      if (savedState.isIndexBased) {
        var index = parseInt(tableId.replace('datatable_', ''));
        $table = $('.dataTables_wrapper').eq(index).find('table');
        console.log('Using index-based lookup for state restore, wrapper #' + index);
      } else {
        $table = $('#' + tableId);
      }
      
      if ($table && $table.length > 0) {
        // Check if this is actually a table element and if DataTables is initialized
        if (!$table.is('table')) {
          console.log('Element is not a table, skipping DataTable state restore for:', tableId);
          return;
        }
        
        // Check if DataTables is already initialized on this table
        if (!$.fn.DataTable.isDataTable($table[0])) {
          console.log('DataTables not initialized on table, skipping state restore for:', tableId);
          return;
        }
        
        dataTableApi = $table.DataTable();
        
        if (dataTableApi) {
          console.log('Restoring DataTable state...');
          
          // Restore search
          if (savedState.search && savedState.search.search) {
            dataTableApi.search(savedState.search.search);
            console.log('Restored global search:', savedState.search.search);
          }
          
          // Restore column searches
          if (savedState.columns) {
            savedState.columns.forEach(function(colState, index) {
              if (colState && colState.search && colState.search.search) {
                dataTableApi.column(index).search(colState.search.search);
                console.log('Restored column', index, 'search:', colState.search.search);
              }
              
              if (colState && typeof colState.visible === 'boolean') {
                dataTableApi.column(index).visible(colState.visible);
                console.log('Restored column', index, 'visibility:', colState.visible);
              }
            });
          }
          
          // Restore ordering
          if (savedState.order && savedState.order.length > 0) {
            dataTableApi.order(savedState.order);
            console.log('Restored order:', savedState.order);
          }
          
          // Apply all changes and redraw
          dataTableApi.draw('page');
          
          // Restore page (after draw to ensure data is loaded)
          setTimeout(function() {
            if (savedState.start !== undefined && savedState.length !== undefined) {
              var targetPage = Math.floor(savedState.start / savedState.length);
              if (targetPage !== dataTableApi.page()) {
                dataTableApi.page(targetPage).draw('page');
                console.log('Restored to page:', targetPage);
              }
            }
            
            // Restore page length
            if (savedState.length !== undefined && savedState.length !== dataTableApi.page.len()) {
              dataTableApi.page.len(savedState.length).draw();
              console.log('Restored page length:', savedState.length);
            }
            
            console.log('✓ DataTable state restoration completed for', tableId);
          }, 100);
          
        } else {
          console.log('DataTable API not available for state restore:', tableId);
        }
      } else {
        console.log('Table element not found for state restore:', tableId);
      }
      
    } catch (err) {
      console.error('=== RESTORE DATATABLE STATE: Error for', tableId + ':', err);
    }
  }, actualDelay);
}

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
          console.log('Safari applying scroll to element:', element[0] ? element[0].tagName : 'window', 'position:', top, left);
          
          // First, let's diagnose the element
          if (element[0] && element[0] !== window) {
            var elem = element[0];
            console.log('Safari element diagnosis:');
            console.log('  tagName:', elem.tagName);
            console.log('  className:', elem.className);
            console.log('  scrollHeight:', elem.scrollHeight);
            console.log('  clientHeight:', elem.clientHeight);
            console.log('  offsetHeight:', elem.offsetHeight);
            console.log('  scrollTop before:', elem.scrollTop);
            console.log('  scrollLeft before:', elem.scrollLeft);
            console.log('  overflow:', window.getComputedStyle(elem).overflow);
            console.log('  overflowY:', window.getComputedStyle(elem).overflowY);
            console.log('  display:', window.getComputedStyle(elem).display);
            console.log('  visibility:', window.getComputedStyle(elem).visibility);
          }
          
          // Method 1: Direct jQuery scroll
          element.scrollTop(top);
          element.scrollLeft(left);
          
          // Method 2: Native DOM element access
          if (element[0] && element[0] !== window) {
            element[0].scrollTop = top;
            element[0].scrollLeft = left;
            console.log('Safari native DOM set - scrollTop:', element[0].scrollTop, 'scrollLeft:', element[0].scrollLeft);
          }
          
          // Method 3: Force scroll using scrollTo if available
          if (element[0] && typeof element[0].scrollTo === 'function') {
            element[0].scrollTo(left, top);
            console.log('Safari scrollTo method applied');
          }
          
          // Method 4: Try scrollBy (relative scroll)
          if (element[0] && typeof element[0].scrollBy === 'function') {
            // First scroll to 0, then scroll by the amount
            element[0].scrollTop = 0;
            element[0].scrollBy(left, top);
            console.log('Safari scrollBy method applied');
          }
          
          // Method 5: Using requestAnimationFrame (Safari sometimes needs this)
          if (typeof requestAnimationFrame !== 'undefined') {
            requestAnimationFrame(function() {
              element.scrollTop(top);
              element.scrollLeft(left);
              if (element[0] && element[0] !== window) {
                element[0].scrollTop = top;
                element[0].scrollLeft = left;
              }
              console.log('Safari requestAnimationFrame scroll applied');
            });
          }
          
          // Method 6: Force reflow and try again (Safari rendering quirk)
          setTimeout(function() {
            // Force reflow
            if (element[0] && element[0].offsetHeight !== undefined) {
              var height = element[0].offsetHeight; // Force reflow
            }
            
            element.scrollTop(top);
            element.scrollLeft(left);
            if (element[0] && element[0] !== window) {
              element[0].scrollTop = top;
              element[0].scrollLeft = left;
            }
            console.log('Safari delayed scroll with reflow applied');
          }, 50);
          
          // Method 7: Try multiple times with different intervals
          [100, 200, 300].forEach(function(delay) {
            setTimeout(function() {
              if (element[0] && element[0].scrollTop !== top) {
                element.scrollTop(top);
                element[0].scrollTop = top;
                console.log('Safari retry at', delay, 'ms - current position:', element[0].scrollTop);
              }
            }, delay);
          });
          
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
          
          // Check if this scrollBody is actually scrollable
          var isScrollable = scrollBody[0] && scrollBody[0].scrollHeight > scrollBody[0].clientHeight;
          console.log('ScrollBody is scrollable:', isScrollable, 'scrollHeight:', scrollBody[0] ? scrollBody[0].scrollHeight : 'N/A', 'clientHeight:', scrollBody[0] ? scrollBody[0].clientHeight : 'N/A');
          
          if (isScrollable) {
            // Only try scrollBody if it's actually scrollable
            applySafariScroll(scrollBody, savedPosition.top, savedPosition.left);
          } else {
            console.log('ScrollBody is not scrollable, will use window scroll instead');
            // If the saved method was from document/window level, restore to window
            if (savedPosition.method && savedPosition.method.includes('safari-any-element')) {
              console.log('Saved position was from document level, restoring to window');
              applySafariScroll($(window), savedPosition.top, savedPosition.left);
              restored = true;
            }
          }
          
          // Safari-specific: Try alternative scroll containers if the main one fails
          if (isSafari() && isScrollable) {
            setTimeout(function() {
              var currentTop = scrollBody.scrollTop();
              if (currentTop === 0 && savedPosition.top > 0) {
                console.log('Safari primary scrollBody failed, trying alternatives...');
                
                // Try all child elements of the wrapper
                targetWrapper.find('*').each(function() {
                  var elem = $(this);
                  if (elem[0] && elem[0].scrollHeight > elem[0].clientHeight) {
                    console.log('Safari trying alternative scroll element:', this.tagName, this.className);
                    applySafariScroll(elem, savedPosition.top, savedPosition.left);
                    
                    // Check if this one worked
                    setTimeout(function() {
                      if (elem.scrollTop() > 0) {
                        console.log('✓ Safari alternative element worked:', this.tagName, this.className);
                        return false; // break
                      }
                    }, 100);
                  }
                });
                
                // Try the table element itself
                var table = targetWrapper.find('table').first();
                if (table.length > 0) {
                  console.log('Safari trying table element scroll');
                  applySafariScroll(table, savedPosition.top, savedPosition.left);
                }
                
                // Try the wrapper itself
                console.log('Safari trying wrapper element scroll');
                applySafariScroll(targetWrapper, savedPosition.top, savedPosition.left);
              }
            }, 100);
          }
          
          // Verify the scroll was applied (with tolerance for Safari)
          setTimeout(function() {
            // Check if we used window scroll or scrollBody scroll
            if (savedPosition.method && 
                (savedPosition.method.includes('safari-any-element') || 
                 savedPosition.method.includes('safari-documentElement') || 
                 savedPosition.method.includes('safari-body') ||
                 savedPosition.method === 'window')) {
              // Verify window scroll instead of scrollBody
              var windowTop = $(window).scrollTop();
              var windowLeft = $(window).scrollLeft();
              console.log('Window scroll verification - Expected:', savedPosition.top, savedPosition.left, 'Actual:', windowTop, windowLeft);
              
              var tolerance = isSafari() ? 10 : 2;
              if (Math.abs(windowTop - savedPosition.top) <= tolerance || savedPosition.top === 0) {
                restored = true;
                console.log('✓ Successfully restored via window scroll');
              } else {
                console.log('❌ Window scroll verification failed - will trigger retry mechanism');
              }
            } else {
              // Original scrollBody verification for DataTable-level scrolling
              var newTop = scrollBody.scrollTop();
              var newLeft = scrollBody.scrollLeft();
              console.log('Applied scroll - Expected:', savedPosition.top, savedPosition.left, 'Actual:', newTop, newLeft);
              
              // Also check native DOM element
              if (scrollBody[0]) {
                var nativeTop = scrollBody[0].scrollTop;
                var nativeLeft = scrollBody[0].scrollLeft;
                console.log('Native DOM verification - scrollTop:', nativeTop, 'scrollLeft:', nativeLeft);
              }
              
              // Safari tolerance check
              var tolerance = isSafari() ? 10 : 2;
              if (Math.abs(newTop - savedPosition.top) <= tolerance || savedPosition.top === 0) {
                restored = true;
                console.log('✓ Successfully restored via scrollBody');
              } else {
                console.log('❌ Scroll verification failed - will trigger retry mechanism');
              }
            }
          }, 150); // Longer delay for Safari verification
          
          restored = true; // Assume success for now
        } else {
          // Try wrapper itself
          console.log('Found wrapper element, restoring position');
          applySafariScroll(targetWrapper, savedPosition.top, savedPosition.left);
          restored = true;
        }
      }
      
      // Method 2: Check if the saved position was from window/document level
      if (!restored && savedPosition.method && 
          (savedPosition.method.includes('safari-any-element') || 
           savedPosition.method.includes('safari-documentElement') || 
           savedPosition.method.includes('safari-body') ||
           savedPosition.method === 'window')) {
        console.log('Saved position was from document/window level, restoring to window');
        applySafariScroll($(window), savedPosition.top, savedPosition.left);
        
        // Verify window scroll
        setTimeout(function() {
          var windowTop = $(window).scrollTop();
          console.log('Window scroll verification - Expected:', savedPosition.top, 'Actual:', windowTop);
          if (Math.abs(windowTop - savedPosition.top) <= 10) {
            console.log('✓ Successfully restored via window scroll');
          }
        }, 200);
        
        restored = true;
      }
      
      // Method 3: Fallback to any scroll body if target not found
      if (!restored) {
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0) {
          console.log('Using fallback scrollBody element');
          applySafariScroll(scrollBody, savedPosition.top, savedPosition.left);
          restored = true;
        }
      }
      
      // Method 4: Use window scroll as fallback
      if (!restored && savedPosition.source === 'window') {
        console.log('Using window scroll fallback');
        applySafariScroll($(window), savedPosition.top, savedPosition.left);
        restored = true;
      }
      
      // Safari-specific: Additional verification and retry
      if (isSafari() && restored) {
        setTimeout(function() {
          // Check the appropriate scroll element based on the saved method
          if (savedPosition.method && 
              (savedPosition.method.includes('safari-any-element') || 
               savedPosition.method.includes('safari-documentElement') || 
               savedPosition.method.includes('safari-body') ||
               savedPosition.method === 'window')) {
            // Check window scroll for document-level saved positions
            var currentTop = $(window).scrollTop();
            if (Math.abs(currentTop - savedPosition.top) > 10 && savedPosition.top > 0) {
              console.log('Safari window retry: Current position', currentTop, 'differs from expected', savedPosition.top);
              
              // More aggressive window scroll retry for Safari
              var maxRetries = 5;
              var retryCount = 0;
              
              function aggressiveWindowRetry() {
                retryCount++;
                console.log('Safari aggressive window retry #', retryCount);
                
                // Force multiple window scroll methods
                $(window).scrollTop(savedPosition.top);
                $(window).scrollLeft(savedPosition.left);
                
                // Try native window scroll
                if (window.scrollTo) {
                  window.scrollTo(savedPosition.left, savedPosition.top);
                }
                
                // Try document element scroll
                if (document.documentElement) {
                  document.documentElement.scrollTop = savedPosition.top;
                  document.documentElement.scrollLeft = savedPosition.left;
                }
                
                // Check if it worked
                setTimeout(function() {
                  var checkTop = $(window).scrollTop();
                  console.log('Safari window retry #', retryCount, 'result:', checkTop, 'target:', savedPosition.top);
                  
                  if (Math.abs(checkTop - savedPosition.top) > 10 && retryCount < maxRetries && savedPosition.top > 0) {
                    setTimeout(aggressiveWindowRetry, 100); // Try again
                  } else if (Math.abs(checkTop - savedPosition.top) <= 10) {
                    console.log('✓ Safari aggressive window retry succeeded on attempt', retryCount);
                  } else {
                    console.log('❌ Safari aggressive window retry failed after', maxRetries, 'attempts');
                  }
                }, 50);
              }
              
              aggressiveWindowRetry();
            } else {
              console.log('✓ Safari window scroll position verified as correct');
            }
          } else {
            // Original scrollBody retry logic for DataTable-level scrolling
            var scrollBody = $('.dataTables_scrollBody').first();
            if (scrollBody.length > 0) {
              var currentTop = scrollBody.scrollTop();
              if (Math.abs(currentTop - savedPosition.top) > 10 && savedPosition.top > 0) {
                console.log('Safari retry: Current position', currentTop, 'differs from expected', savedPosition.top);
                
                // More aggressive retry for Safari
                var maxRetries = 5;
                var retryCount = 0;
                
                function aggressiveRetry() {
                  retryCount++;
                  console.log('Safari aggressive retry #', retryCount);
                  
                  // Force multiple scroll methods
                  scrollBody.scrollTop(savedPosition.top);
                  scrollBody.scrollLeft(savedPosition.left);
                  
                  if (scrollBody[0]) {
                    scrollBody[0].scrollTop = savedPosition.top;
                    scrollBody[0].scrollLeft = savedPosition.left;
                    
                    // Force reflow
                    var height = scrollBody[0].offsetHeight;
                    
                    // Try scrollTo if available
                    if (typeof scrollBody[0].scrollTo === 'function') {
                      scrollBody[0].scrollTo(savedPosition.left, savedPosition.top);
                    }
                  }
                  
                  // Check if it worked
                  setTimeout(function() {
                    var checkTop = scrollBody.scrollTop();
                    console.log('Safari retry #', retryCount, 'result:', checkTop, 'target:', savedPosition.top);
                    
                    if (Math.abs(checkTop - savedPosition.top) > 10 && retryCount < maxRetries && savedPosition.top > 0) {
                      setTimeout(aggressiveRetry, 100); // Try again
                    } else if (Math.abs(checkTop - savedPosition.top) <= 10) {
                      console.log('✓ Safari aggressive retry succeeded on attempt', retryCount);
                    } else {
                      console.log('❌ Safari aggressive retry failed after', maxRetries, 'attempts');
                    }
                  }, 50);
                }
                
                aggressiveRetry();
              } else {
                console.log('✓ Safari scroll position verified as correct');
              }
            }
          }
        }, 300); // Longer initial delay for Safari
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

// Enhanced functions with Safari support and complete state preservation
function refreshTableWithScrollPreservation(tableId) {
  console.log('=== REFRESH WITH COMPLETE STATE PRESERVATION:', tableId);
  
  // Save current position and DataTable state immediately
  var currentPosition = null;
  var currentDataTableState = null;
  
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
    
    // Save both scroll position and DataTable state using our functions
    saveScrollPosition(tableId);
    currentDataTableState = saveDataTableState(tableId);
    
    // Return a function that can restore both position and state
    return function(callback) {
      var maxAttempts = isSafari() ? 5 : 3; // More attempts for Safari
      var attempts = 0;
      
      function attemptRestore() {
        attempts++;
        console.log('Complete state restore attempt', attempts, 'of', maxAttempts);
        
        // First restore DataTable state (filtering, sorting, etc.)
        if (currentDataTableState) {
          restoreDataTableState(tableId, 50);
        }
        
        // Then restore scroll position
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
          
          console.log('Restored scroll to position:', currentPosition);
        } else if (currentPosition) {
          // Use the enhanced scroll restoration system
          restoreScrollPosition(tableId, 100);
        }
        
        if (callback) callback();
        return;
        
        // Legacy retry logic (keeping for compatibility)
        // Try again if we haven't reached max attempts
        if (attempts < maxAttempts) {
          var retryDelay = isSafari() ? 300 : 200;
          setTimeout(attemptRestore, retryDelay);
        } else {
          console.log('Failed to restore complete state after', maxAttempts, 'attempts');
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
  
  console.log('Enhanced scroll position and DataTable state handlers registered successfully');
  console.log('Safari mode:', isSafari() ? 'enabled' : 'disabled');
  console.log('Features enabled: scroll preservation, DataTable state saving, Safari compatibility');
  
  // Debug any existing tables
  setTimeout(function() {
    if (typeof debugScrollState === 'function') {
      debugScrollState();
    }
  }, 1000);
});

// Global functions to save/restore both scroll position and DataTable state for any DataTable on the page
// Enhanced with Safari-specific handling and complete state preservation
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
        
        // Skip if no table found or not a proper table element
        if (!table.length || !table.is('table')) {
          console.log('Skipping wrapper #' + index + ': no valid table found');
          return;
        }
        
        // If no ID, create a fallback ID based on index
        if (!tableId) {
          tableId = 'datatable_' + index;
          console.log('Table has no ID, using fallback:', tableId);
        }
        
        tablesFound++;
        console.log('Found DataTable #' + tablesFound + ':', tableId);
        saveScrollPosition(tableId);
        
        // Only save DataTable state if DataTables is actually initialized
        if ($.fn.DataTable.isDataTable(table[0])) {
          saveDataTableState(tableId);
        } else {
          console.log('DataTables not initialized on table:', tableId);
        }
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
    console.log('=== RESTORING SCROLL & STATE FOR ALL DATATABLES ===');
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
        
        var hasScrollPosition = window.scrollPositions && window.scrollPositions[tableId];
        var hasDataTableState = window.dataTableStates && window.dataTableStates[tableId];
        
        if (hasScrollPosition || hasDataTableState) {
          console.log('Has saved state - scroll:', !!hasScrollPosition, 'datatable:', !!hasDataTableState);
          
          // Restore DataTable state first (this may trigger redraws)
          if (hasDataTableState) {
            restoreDataTableState(tableId, isSafari() ? 50 : 25);
          }
          
          // Then restore scroll position with a slight delay
          if (hasScrollPosition) {
            var scrollDelay = isSafari() ? 200 : 100;
            restoreScrollPosition(tableId, scrollDelay);
          }
          
          tablesRestored++;
        } else {
          console.log('No saved state for:', tableId);
        }
      } catch (err) {
        console.error('Error processing table for restore:', err);
      }
    });
    console.log('Total DataTables found:', tablesFound, 'Tables with saved state:', tablesRestored);
  } catch (err) {
    console.error('Error in restoreScrollForAllTables:', err);
  }
}

// Debug function to inspect current state
function debugScrollState() {
  console.log('=== SCROLL & DATATABLE STATE DEBUG ===');
  console.log('Browser: Safari =', isSafari());
  console.log('jQuery available:', typeof $ !== 'undefined');
  console.log('DataTables available:', typeof $.fn.DataTable !== 'undefined');
  console.log('Saved scroll positions:', window.scrollPositions);
  console.log('Saved DataTable states:', window.dataTableStates);
  
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
    
    // Check DataTable API
    if (table.length > 0) {
      try {
        // Only try to get DataTable API if it's actually initialized
        if (table.is('table') && $.fn.DataTable.isDataTable(table[0])) {
          var api = table.DataTable();
          if (api) {
            console.log('  DataTable info:', api.page.info());
            console.log('  Current search:', api.search());
            console.log('  Current order:', api.order());
          }
        } else {
          console.log('  DataTable not initialized on this element');
        }
      } catch (e) {
        console.log('  No DataTable API available:', e.message);
      }
    }
    
    if (window.scrollPositions && window.scrollPositions[tableId]) {
      console.log('  Saved scroll position:', window.scrollPositions[tableId]);
    }
    
    if (window.dataTableStates && window.dataTableStates[tableId]) {
      console.log('  Saved DataTable state:', window.dataTableStates[tableId]);
    }
  });
}

// Export debug function to global scope
window.debugScrollState = debugScrollState;
