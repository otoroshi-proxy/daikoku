import React from 'react';
import { Sliders } from 'react-feather';

import { SwitchButton } from './Switch';

// Define a default UI for filtering
export const DefaultColumnFilter = ({ column: { filterValue, preFilteredRows, setFilter } }) => {
  return (
    <div className="form-group pb-0">
      <div className="input-group input-group-sm mb-2 pr-2">
        <div className="input-group-prepend">
          <div className="input-group-text">
            <i className="fas fa-search" />
          </div>
        </div>
        <input
          value={filterValue || ''}
          onChange={(e) => {
            setFilter(e.target.value || undefined); // Set undefined to remove the filter entirely
          }}
          placeholder={'Search'}
          className="form-control form-control-sm mr-2"
          style={{ borderColor: '#9ab0c5' }}
        />
      </div>
    </div>
  );
};

// This is a custom filter UI that uses a
// slider to set the filter value between a column's
// min and max values
export const BooleanColumnFilter = ({
  column: { filterValue, setFilter, preFilteredRows, id },
}) => {
  return (
    <span className="d-flex justify-content-around">
      <SwitchButton
        onSwitch={(enabled) => setFilter(enabled)}
        checked={filterValue || false}
        large
        noText
      />
      {filterValue !== undefined && (
        <span className="btn btn-sm btn-danger-negative table-filter-suppress">
          <Sliders onClick={() => setFilter(undefined)} />
        </span>
      )}
    </span>
  );
};
