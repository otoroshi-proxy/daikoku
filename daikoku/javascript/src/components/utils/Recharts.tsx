import sortBy from 'lodash/sortBy';

import {
  AreaChart,
  Area,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  PieChart,
  Pie,
  Cell,
  Legend,
} from 'recharts';
import { formatDate } from './formatters';
import { I18nContext } from '../../contexts';
import { useContext } from 'react';

export function Histogram(props: any) {
  const { translate } = useContext(I18nContext)

  const colors = [
    '#027cc3',
    '#95cf3d',
    '#ff8900',
    '#7cb5ec',
    '#8085c9',
    '#ffeb3b',
    '#8a2be2',
    '#deb887',
    '#d50200',
    '#a52a2a',
  ];

  const formatTick = (v: any) => {
    if (v > 999999) {
      return (v / 1000000).toFixed(0) + ' M';
    }
    if (v > 999) {
      return (v / 1000).toFixed(0) + ' k';
    }
    return v;
  };

  let data: Array<any> = [];
  let seriesName: Array<any> = [];

  // console.log(props.title, props.series);

  if (props.series && props.series[0]) {
    seriesName = props.series.map((s: any) => s.name);
    const values: Array<any> = [];
    const size = props.series[0].data.length;
    for (let i = 0; i < size; i++) {
      let finalItem = {};
      props.series.forEach((serie: any) => {
        const item = serie.data[i];
        if (item) {
          finalItem = {
            ...finalItem,
            ...{
              name: formatDate(item[0], translate('date.locale'), translate('date.format')),
              [serie.name]: item[1],
            },
          };
        }
      });
      values.push(finalItem);
    }
    data = values;
  }

  return (
    <div
      style={{
        backgroundColor: '#fff',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      <h4 className="recharts">{props.title}</h4>
      <ResponsiveContainer height={props.height || 200}>
        <AreaChart data={data} margin={{ top: 10, right: 30, left: 0, bottom: 0 }}>
          <XAxis dataKey="name" />
          <YAxis tickFormatter={formatTick} />
          <CartesianGrid strokeDasharray="3 3" />
          <Tooltip />
          {sortBy(seriesName, (sn) => sn).map((sn, idx) => (
            <Area
              key={sn}
              type="monotone"
              name={sn}
              unit={props.unit}
              dataKey={sn}
              stroke={colors[idx]}
              fillOpacity={0.6}
              fill={colors[idx]}
            />
          ))}
        </AreaChart>
      </ResponsiveContainer>
    </div>
  );
}

export function RoundChart(props: any) {
  const colors = [
    '#95cf3d',
    '#027cc3',
    '#ff8900',
    '#7cb5ec',
    '#8085c9',
    '#ffeb3b',
    '#8a2be2',
    '#deb887',
    '#d50200',
    '#a52a2a',
  ];

  const renderCustomizedLabel = (props: any) => {
    const { x, y, cx } = props;
    return (
      <text
        x={x}
        y={y}
        fill="var(--level2_text-color)"
        textAnchor={x > cx ? 'start' : 'end'}
        dominantBaseline="central"
        style={{ padding: 5 }}
      >
        {props.name.replace(/"/g, '')}: {(props.percent * 100).toFixed(0)}% ({props.value})
      </text>
    );
  };

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
      }}
      className="recharts"
    >
      <h4 className="recharts">{props.title}</h4>
      <ResponsiveContainer height={props.size ? props.size + 150 : 200}>
        <PieChart>
          <Pie
            data={props.series}
            fill="#8884d8"
            outerRadius={props.size ? props.size / 2 : 100}
            dataKey={props.dataKey || 'value'}
            label={!props.series2 && !props.noLabel && renderCustomizedLabel}
          >
            {props.series.map((entry: any, index: any) => (
              <Cell key={entry.name} fill={colors[index % colors.length]} />
            ))}
          </Pie>
          {props.series2 && (
            <Pie
              data={props.series2}
              fill="#8884d8"
              innerRadius={props.size ? props.size / 2 + 10 : 110}
              outerRadius={props.size ? props.size / 2 + 30 : 130}
              dataKey={props.dataKey || 'value'}
              label={renderCustomizedLabel}
            >
              {props.series2.map((entry: any) => {
                const parentIdx = [...new Set(props.series.map((item: any) => item.name))].indexOf(
                  entry[props.parentKey]
                );
                return <Cell key={entry.name} fill={colors[parentIdx % colors.length]} />;
              })}
            </Pie>
          )}
          <Tooltip />
          {props.legend && <Legend verticalAlign="top" height={36} content={props.legend} />}
        </PieChart>
      </ResponsiveContainer>
    </div>
  );
}
