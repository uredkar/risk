import React from 'react';

const GridComponent: React.FC = () => {
  const data = [
    { stock: 'AAPL', risk: 'Low', weight: 25 },
    { stock: 'GOOG', risk: 'High', weight: 35 },
    { stock: 'TSLA', risk: 'Medium', weight: 40 },
  ];

  return (
    <div style={{ padding: '10px' }}>
      <h4>Portfolio Grid</h4>
      <table>
        <thead>
          <tr>
            <th>Stock</th>
            <th>Risk Level</th>
            <th>Weight (%)</th>
          </tr>
        </thead>
        <tbody>
          {data.map((item, idx) => (
            <tr key={idx}>
              <td>{item.stock}</td>
              <td>{item.risk}</td>
              <td>{item.weight}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default GridComponent;
