// Ultra-enhanced scroll-position.js with aggressive Safari fixes
// Global object to store scroll positions for different elements
window.scrollPositions = window.scrollPositions || {};

// Enhanced Safari detection including iOS Safari
function isSafari() {
  const ua = navigator.userAgent;
  const isSafariDesktop = /^((?!chrome|android).)*safari/i.test(ua);
  const isSafariMobile = /iPad|iPhone|iPod/.test(ua);
  const isWebKit = /WebKit/.test(ua) && !/Chrome/.test(ua);
  return isSafariDesktop || isSafariMobile || isWebKit;
}

// Aggressive Safari scroll position utilities
const SafariScrollUtils = {
  // Force Safari to acknowledge scroll changes
  forceScrollUpdate: function(element) {
    if (isSafari()) {
      // Method 1: Trigger reflow
      element[0].offsetHeight;
      
      // Method 2: Force repaint
      var originalTransform = element.css('transform');
      element.css('transform', 'translateZ(0)');
      setTimeout(function() {
        element.css('transform', originalTransform);
      }, 0);
    }
  },
  
  // Safari-specific scroll setting with multiple fallbacks
  setScroll: function(element, top, left) {
    console.log('SafariScrollUtils.setScroll - Setting scroll to:', top, left);
    
    if (!isSafari()) {
      // Standard method for non-Safari browsers
      element.scrollTop(top);
      element.scrollLeft(left);
      return;
    }
    
    // Safari aggressive approach
    var attempts = 0;
    var maxAttempts = 10;
    
    function attemptScroll() {
      attempts++;
      console.log('Safari scroll attempt', attempts, 'of', maxAttempts);
      
      // Method 1: Direct jQuery scroll
      element.scrollTop(top);
      element.scrollLeft(left);
      
      // Method 2: Native DOM scroll
      if (element[0]) {
        element[0].scrollTop = top;
        element[0].scrollLeft = left;
      }
      
      // Method 3: Using scrollTo if available
      if (element[0] && element[0].scrollTo) {
        element[0].scrollTo(left, top);
      }
      
      // Method 4: Force reflow
      SafariScrollUtils.forceScrollUpdate(element);
      
      // Verify scroll position after a short delay
      setTimeout(function() {
        var currentTop = element.scrollTop();
        var currentLeft = element.scrollLeft();
        console.log('Scroll verification - Expected:', top, left, 'Actual:', currentTop, currentLeft);
        
        var tolerance = 20; // Increased tolerance for Safari
        var topOk = Math.abs(currentTop - top) <= tolerance || top === 0;
        var leftOk = Math.abs(currentLeft - left) <= tolerance || left === 0;
        
        if (!topOk || !leftOk) {
          if (attempts < maxAttempts) {
            console.log('Safari scroll verification failed, retrying...');
            setTimeout(attemptScroll, 100);
          } else {
            console.log('Safari scroll failed after', maxAttempts, 'attempts');
          }
        } else {
          console.log('✅ Safari scroll successful after', attempts, 'attempts');
        }
      }, 50);
    }
    
    // Start attempts immediately and with requestAnimationFrame
    attemptScroll();
    
    if (typeof requestAnimationFrame !== 'undefined') {
      requestAnimationFrame(attemptScroll);
    }
    
    // Also try with longer delays
    setTimeout(attemptScroll, 100);
    setTimeout(attemptScroll, 300);
  },
  
  // Get scroll position with Safari-specific handling
  getScroll: function(element) {
    if (!isSafari()) {
      return {
        top: element.scrollTop(),
        left: element.scrollLeft()
      };
    }
    
    // Safari might need multiple reads
    var top1 = element.scrollTop();
    var left1 = element.scrollLeft();
    
    // Force reflow and read again
    SafariScrollUtils.forceScrollUpdate(element);
    
    var top2 = element.scrollTop();
    var left2 = element.scrollLeft();
    
    // Use the most recent values
    return {
      top: top2 !== undefined ? top2 : top1,
      left: left2 !== undefined ? left2 : left1
    };
  }
};

// Suppress bootstrap-datepicker deprecation warnings
(function() {
  var originalWarn = console.warn;
  var originalLog = console.log;
  
  console.warn = function() {
    var message = Array.prototype.slice.call(arguments).join(' ');
    if (typeof message === 'string' && 
        (message.includes('DEPRECATED') || 
         message.includes('datepicker') ||
         message.includes('language code') ||
         message.includes('filename doesn\'t follow the convention'))) {
      return;
    }
    originalWarn.apply(console, arguments);
  };
  
  console.log = function() {
    var message = Array.prototype.slice.call(arguments).join(' ');
    if (typeof message === 'string' && 
        (message.includes('DEPRECATED') || 
         message.includes('datepicker'))) {
      return;
    }
    originalLog.apply(console, arguments);
  };
  
  console.info('Bootstrap-datepicker warnings suppressed');
})();

$(document).ready(function() {
  console.log('🔧 Ultra-enhanced scroll system initialized');
  console.log('🍎 Safari detected:', isSafari());
  console.log('📱 User Agent:', navigator.userAgent);
});

/**
 * Enhanced save scroll function with aggressive Safari handling
 */
function saveScrollPosition(tableId) {
  try {
    console.log('🔄 SAVE SCROLL: Starting for table:', tableId);
    
    var savedTop = 0;
    var savedLeft = 0;
    var containerFound = false;
    var method = 'none';
    var element = null;
    
    // Enhanced element finding with multiple strategies
    var strategies = [
      // Strategy 1: Index-based lookup for fallback IDs
      function() {
        if (tableId.startsWith('datatable_')) {
          var index = parseInt(tableId.replace('datatable_', ''));
          var wrapper = $('.dataTables_wrapper').eq(index);
          if (wrapper.length > 0) {
            var scrollBody = wrapper.find('.dataTables_scrollBody');
            if (scrollBody.length > 0) {
              return { element: scrollBody, method: 'index-scrollBody' };
            }
            return { element: wrapper, method: 'index-wrapper' };
          }
        }
        return null;
      },
      
      // Strategy 2: Direct ID lookup
      function() {
        var table = $('#' + tableId);
        if (table.length > 0) {
          var wrapper = table.closest('.dataTables_wrapper');
          if (wrapper.length > 0) {
            var scrollBody = wrapper.find('.dataTables_scrollBody');
            if (scrollBody.length > 0) {
              return { element: scrollBody, method: 'direct-scrollBody' };
            }
            return { element: wrapper, method: 'direct-wrapper' };
          }
        }
        return null;
      },
      
      // Strategy 3: Generic fallback
      function() {
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0) {
          return { element: scrollBody, method: 'generic-scrollBody' };
        }
        var wrapper = $('.dataTables_wrapper').first();
        if (wrapper.length > 0) {
          return { element: wrapper, method: 'generic-wrapper' };
        }
        return null;
      }
    ];
    
    // Try each strategy
    for (var i = 0; i < strategies.length; i++) {
      var result = strategies[i]();
      if (result) {
        element = result.element;
        method = result.method;
        containerFound = true;
        console.log('📍 Found element using strategy:', method);
        break;
      }
    }
    
    if (containerFound && element) {
      var scrollData = SafariScrollUtils.getScroll(element);
      savedTop = scrollData.top;
      savedLeft = scrollData.left;
      console.log('📏 Scroll position captured:', savedTop, savedLeft);
    } else {
      // Window fallback
      savedTop = $(window).scrollTop();
      savedLeft = $(window).scrollLeft();
      method = 'window';
      console.log('🪟 Using window scroll:', savedTop, savedLeft);
    }
    
    // Enhanced save with Safari metadata
    window.scrollPositions[tableId] = {
      top: savedTop,
      left: savedLeft,
      timestamp: Date.now(),
      source: containerFound ? 'container' : 'window',
      method: method,
      browser: isSafari() ? 'safari' : 'other',
      userAgent: navigator.userAgent.substring(0, 50) + '...',
      elementSnapshot: element ? {
        hasScrollBody: element.hasClass('dataTables_scrollBody'),
        isWrapper: element.hasClass('dataTables_wrapper'),
        height: element.height(),
        scrollHeight: element[0] ? element[0].scrollHeight : 0
      } : null
    };
    
    console.log('✅ SAVE SCROLL: Completed for', tableId, window.scrollPositions[tableId]);
    
  } catch (err) {
    console.error('❌ SAVE SCROLL: Error for', tableId, err);
  }
}

/**
 * Ultra-enhanced restore scroll function
 */
function restoreScrollPosition(tableId, delay) {
  var actualDelay = delay || (isSafari() ? 800 : 300); // Even longer delay for Safari
  
  console.log('⏱️ RESTORE SCROLL: Scheduling restore in', actualDelay, 'ms for', tableId);
  
  setTimeout(function() {
    try {
      if (!window.scrollPositions || !window.scrollPositions[tableId]) {
        console.log('❌ No saved scroll position found for:', tableId);
        return;
      }
      
      var savedPosition = window.scrollPositions[tableId];
      console.log('🔄 RESTORE SCROLL: Starting for', tableId, savedPosition);
      
      // Find the element using the same strategies as save
      var element = null;
      var method = 'none';
      
      // Try to find element based on saved method
      if (savedPosition.method && savedPosition.method.includes('index')) {
        var index = parseInt(tableId.replace('datatable_', ''));
        var wrapper = $('.dataTables_wrapper').eq(index);
        if (wrapper.length > 0) {
          var scrollBody = wrapper.find('.dataTables_scrollBody');
          element = scrollBody.length > 0 ? scrollBody : wrapper;
          method = 'index-recovery';
        }
      }
      
      // Fallback to generic search
      if (!element) {
        var scrollBody = $('.dataTables_scrollBody').first();
        if (scrollBody.length > 0) {
          element = scrollBody;
          method = 'generic-scrollBody';
        } else {
          var wrapper = $('.dataTables_wrapper').first();
          if (wrapper.length > 0) {
            element = wrapper;
            method = 'generic-wrapper';
          }
        }
      }
      
      if (element && element.length > 0) {
        console.log('🎯 Found restore target using:', method);
        console.log('📐 Restoring to position:', savedPosition.top, savedPosition.left);
        
        // Use Safari-specific scroll setting
        SafariScrollUtils.setScroll(element, savedPosition.top, savedPosition.left);
        
        // Additional Safari verification with extended delay
        if (isSafari()) {
          setTimeout(function() {
            var currentScroll = SafariScrollUtils.getScroll(element);
            console.log('🔍 Final Safari verification - Expected:', savedPosition.top, savedPosition.left, 
                       'Actual:', currentScroll.top, currentScroll.left);
            
            var tolerance = 25;
            if (Math.abs(currentScroll.top - savedPosition.top) > tolerance && savedPosition.top > 0) {
              console.log('⚠️ Safari final attempt - large discrepancy detected');
              SafariScrollUtils.setScroll(element, savedPosition.top, savedPosition.left);
            }
          }, 500);
        }
        
      } else {
        console.log('❌ No suitable element found for restore');
        // Window fallback
        if (savedPosition.source === 'window') {
          $(window).scrollTop(savedPosition.top);
          $(window).scrollLeft(savedPosition.left);
          console.log('🪟 Applied window scroll fallback');
        }
      }
      
    } catch (err) {
      console.error('❌ RESTORE SCROLL: Error for', tableId, err);
    }
  }, actualDelay);
}

// Enhanced global functions
function saveScrollForAllTables() {
  try {
    console.log('🔄 SAVING SCROLL FOR ALL DATATABLES');
    console.log('🍎 Safari mode:', isSafari());
    
    if (typeof $ === 'undefined') {
      console.error('❌ jQuery not available');
      return;
    }
    
    var tablesFound = 0;
    $('.dataTables_wrapper').each(function(index) {
      try {
        var wrapper = $(this);
        var table = wrapper.find('table').first();
        var tableId = table.attr('id') || 'datatable_' + index;
        
        tablesFound++;
        console.log('📋 Processing DataTable #' + tablesFound + ':', tableId);
        saveScrollPosition(tableId);
      } catch (err) {
        console.error('❌ Error processing table:', err);
      }
    });
    console.log('📊 Total DataTables processed:', tablesFound);
  } catch (err) {
    console.error('❌ Error in saveScrollForAllTables:', err);
  }
}

function restoreScrollForAllTables() {
  try {
    console.log('🔄 RESTORING SCROLL FOR ALL DATATABLES');
    console.log('🍎 Safari mode:', isSafari());
    
    if (typeof $ === 'undefined') {
      console.error('❌ jQuery not available');
      return;
    }
    
    var tablesFound = 0;
    var tablesRestored = 0;
    $('.dataTables_wrapper').each(function(index) {
      try {
        var wrapper = $(this);
        var table = wrapper.find('table').first();
        var tableId = table.attr('id') || 'datatable_' + index;
        
        tablesFound++;
        console.log('📋 Processing DataTable #' + tablesFound + ':', tableId);
        
        if (window.scrollPositions && window.scrollPositions[tableId]) {
          console.log('💾 Has saved position:', window.scrollPositions[tableId]);
          var delay = isSafari() ? 200 : 100;
          restoreScrollPosition(tableId, delay);
          tablesRestored++;
        } else {
          console.log('📭 No saved position for:', tableId);
        }
      } catch (err) {
        console.error('❌ Error processing table for restore:', err);
      }
    });
    console.log('📊 Total DataTables found:', tablesFound, 'Tables restored:', tablesRestored);
  } catch (err) {
    console.error('❌ Error in restoreScrollForAllTables:', err);
  }
}

// Enhanced Shiny integration
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('eval', function(code) {
    try {
      eval(code);
    } catch (error) {
      console.error('❌ Error evaluating JavaScript:', error);
    }
  });
  
  console.log('✅ Shiny scroll handlers registered');
  console.log('🍎 Safari mode:', isSafari() ? 'enabled' : 'disabled');
});

// DataTables event handlers
$(document).on('init.dt', function(e, settings) {
  try {
    var tableId = $(settings.nTable).attr('id');
    if (tableId && window.scrollPositions && window.scrollPositions[tableId]) {
      var delay = isSafari() ? 300 : 150;
      console.log('🔄 Auto-restoring scroll after DataTable init for:', tableId);
      restoreScrollPosition(tableId, delay);
    }
  } catch (err) {
    console.error('❌ Error in init.dt handler:', err);
  }
});

$(document).on('draw.dt', function(e, settings) {
  try {
    var tableId = $(settings.nTable).attr('id');
    console.log('🎨 DataTable draw event for:', tableId);
  } catch (err) {
    console.error('❌ Error in draw.dt handler:', err);
  }
});

// Debug function
function debugScrollState() {
  console.log('🔍 SCROLL STATE DEBUG');
  console.log('🍎 Safari:', isSafari());
  console.log('📱 User Agent:', navigator.userAgent);
  console.log('💾 Saved positions:', window.scrollPositions);
  
  $('.dataTables_wrapper').each(function(index) {
    var wrapper = $(this);
    var table = wrapper.find('table').first();
    var tableId = table.attr('id') || 'datatable_' + index;
    var scrollBody = wrapper.find('.dataTables_scrollBody');
    
    console.log('📋 Table #' + index + ':', tableId);
    console.log('  📏 Has scrollBody:', scrollBody.length > 0);
    
    if (scrollBody.length > 0) {
      var scroll = SafariScrollUtils.getScroll(scrollBody);
      console.log('  📍 Current scroll:', scroll.top, scroll.left);
    }
    
    if (window.scrollPositions && window.scrollPositions[tableId]) {
      console.log('  💾 Saved position:', window.scrollPositions[tableId]);
    }
  });
}

window.debugScrollState = debugScrollState;
