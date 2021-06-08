import React from 'react';
import { t } from '../../../locales';

const StarsButton = ({ stars, toggleStar, starred, currentLanguage }) => (
  <div
    className="d-flex flex-row"
    style={{
      borderRadius: '4px',
      border: '1px solid',
      overflow: 'hidden',
      boxSizing: 'content-box',
      borderColor: 'var(--btn-border-color, #97b0c7)',
      backgroundColor: 'var(--btn-border-color, #fff)',
      fontSize: '18px',
    }}>
    <button
      className="btn flex-row align-items-center"
      style={{ color: 'var(--btn-bg-color, "#000")', padding: '0' }}
      onClick={toggleStar}>
      <i className={`${starred ? 'fas' : 'far'} fa-star pl-2`} />
      <span className="px-2">{starred ? t('unstar', currentLanguage) : t('star', currentLanguage)}</span>
    </button>
    <div className="px-2 d-flex align-items-center" style={{ backgroundColor: '#fff' }}>
      <span>{stars}</span>
    </div>
  </div>
);

export default StarsButton;