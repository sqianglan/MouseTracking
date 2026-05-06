(function() {
  window.savedScrollState = window.savedScrollState || {
    windowX: 0,
    windowY: 0,
    tables: {}
  };

  window.savedDataTableStates = window.savedDataTableStates || {};

  function getTableKey(element, index) {
    var table = element.closest('.dataTables_wrapper');
    if (table) {
      var tableNode = table.querySelector('table');
      if (tableNode && tableNode.id) {
        return tableNode.id;
      }
    }
    return 'datatable-' + index;
  }

  window.saveScrollForAllTables = function() {
    window.savedScrollState.windowX = window.scrollX || window.pageXOffset || 0;
    window.savedScrollState.windowY = window.scrollY || window.pageYOffset || 0;
    window.savedScrollState.tables = {};

    document.querySelectorAll('.dataTables_scrollBody').forEach(function(element, index) {
      window.savedScrollState.tables[getTableKey(element, index)] = {
        left: element.scrollLeft,
        top: element.scrollTop
      };
    });
  };

  window.restoreScrollForAllTables = function() {
    window.scrollTo(window.savedScrollState.windowX || 0, window.savedScrollState.windowY || 0);

    document.querySelectorAll('.dataTables_scrollBody').forEach(function(element, index) {
      var saved = window.savedScrollState.tables[getTableKey(element, index)];
      if (!saved) {
        return;
      }
      element.scrollLeft = saved.left || 0;
      element.scrollTop = saved.top || 0;
    });
  };

  function getDataTableApi(tableId) {
    if (!window.jQuery || !window.jQuery.fn || !window.jQuery.fn.DataTable) {
      return null;
    }

    if (!window.jQuery.fn.DataTable.isDataTable('#' + tableId)) {
      return null;
    }

    return window.jQuery('#' + tableId).DataTable();
  }

  window.saveDataTableState = function(tableId) {
    var table = getDataTableApi(tableId);
    if (!table) {
      return;
    }

    window.savedDataTableStates[tableId] = {
      order: table.order(),
      search: table.search(),
      page: table.page(),
      length: table.page.len()
    };
  };

  window.restoreDataTableState = function(tableId) {
    var table = getDataTableApi(tableId);
    var saved = window.savedDataTableStates[tableId];

    if (!table || !saved) {
      return;
    }

    if (saved.length) {
      table.page.len(saved.length);
    }
    if (saved.order) {
      table.order(saved.order);
    }
    if (typeof saved.search === 'string') {
      table.search(saved.search);
    }

    table.draw(false);

    if (typeof saved.page === 'number') {
      table.page(saved.page).draw('page');
    }
  };

  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
    window.Shiny.addCustomMessageHandler('eval', function(message) {
      try {
        window.eval(message);
      } catch (error) {
        console.error('Failed to evaluate custom message', error);
      }
    });
  }
})();
