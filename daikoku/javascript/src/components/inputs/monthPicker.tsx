import React, { useContext } from 'react';

import DatePicker from 'antd/lib/date-picker';
import { I18nContext } from '../../core';

const getDateFormat = (language: any) => {
  switch (language.toUpperCase()) {
    case 'FR':
      return {
        format: 'MMM. YYYY',
      };
    case 'EN':
    default:
      return {
        format: 'MMM., YYYY',
      };
  }
};

export const MonthPicker = ({
  updateDate,
  value
}: any) => {
    const { language } = useContext(I18nContext);

  const dateFormat = getDateFormat(language);

  const onChange = (newMonth: any) => {
    if (newMonth && updateDate && !value.isSame(newMonth)) updateDate(newMonth.endOf('month'));
  };

  return (
        <DatePicker.MonthPicker
      defaultValue={value}
      onChange={onChange}
      format={dateFormat.format}
      onOk={(value) => value}
    />
  );
};