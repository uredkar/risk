"use client"; // This is a client component
import * as React from 'react';
import dynamic from 'next/dynamic';
import { LineChart } from '@mui/x-charts/LineChart';
import { BarChart } from '@mui/x-charts/BarChart';
import { Grid2, Paper, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography,Box } from '@mui/material';
import { Accordion, AccordionDetails, AccordionSummary} from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import { useState,useEffect } from 'react';
//const Grid2 = dynamic(() => import('@mui/material/Unstable_Grid2'), { ssr: false });
// Sample stock data
const stockData = [
  { symbol: 'AAPL', name: 'Apple Inc.', price: 150.12 },
  { symbol: 'GOOGL', name: 'Alphabet Inc.', price: 2800.22 },
  { symbol: 'MSFT', name: 'Microsoft Corp.', price: 299.56 },
  { symbol: 'AMZN', name: 'Amazon.com Inc.', price: 3450.88 },
  { symbol: 'TSLA', name: 'Tesla Inc.', price: 790.54 },
];

const quarteryData = [
  { data: [35, 44, 24, 34] },
  { data: [51, 6, 49, 30] },
  { data: [15, 25, 30, 50] },
  { data: [60, 50, 15, 25] },
];
// Define types for stock and account data
interface Stock {
  symbol: string;
  name: string;
  price: number;
  quantity: number;
}

interface Account {
  accountName: string;
  stocks: Stock[];
}

export function ChartsOverviewDemo({ dataSeries }) {
  return (
    <BarChart
      series= {dataSeries}
      height={290}
      xAxis={[{ data: ['Q1', 'Q2', 'Q3', 'Q4'], scaleType: 'band' }]}
      margin={{ top: 10, bottom: 30, left: 40, right: 10 }}
    />
  );
}

/// Sample accounts and stock data
const accountData: Account[] = [
  {
    accountName: 'Account 1',
    stocks: [
      { symbol: 'AAPL', name: 'Apple Inc.', price: 150.12, quantity: 10 },
      { symbol: 'GOOGL', name: 'Alphabet Inc.', price: 2800.22, quantity: 5 },
    ],
  },
  {
    accountName: 'Account 2',
    stocks: [
      { symbol: 'MSFT', name: 'Microsoft Corp.', price: 299.56, quantity: 20 },
      { symbol: 'AMZN', name: 'Amazon.com Inc.', price: 3450.88, quantity: 2 },
      { symbol: 'TSLA', name: 'Tesla Inc.', price: 790.54, quantity: 15 },
    ],
  },
];

const StockTable: React.FC<{ stocks: Stock[] }> = ({ stocks }) => {
  const total = stocks.reduce((acc, stock) => acc + stock.price * stock.quantity, 0);

  return (
    <>
      <TableContainer>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Stock Symbol</TableCell>
              <TableCell>Company Name</TableCell>
              <TableCell align="right">Price ($)</TableCell>
              <TableCell align="right">Quantity</TableCell>
              <TableCell align="right">Total Value ($)</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {stocks.map((stock) => (
              <TableRow key={stock.symbol}>
                <TableCell>{stock.symbol}</TableCell>
                <TableCell>{stock.name}</TableCell>
                <TableCell align="right">{stock.price.toFixed(2)}</TableCell>
                <TableCell align="right">{stock.quantity}</TableCell>
                <TableCell align="right">{(stock.price * stock.quantity).toFixed(2)}</TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
      <Typography variant="h6" align="right" sx={{ padding: 2 }}>
        Account Total: ${total.toFixed(2)}
      </Typography>
      <ChartsOverviewDemo dataSeries={quarteryData}></ChartsOverviewDemo>
      <LineChart
      xAxis={[{ data: [1, 2, 3, 5, 8, 10] }]}
      series={[
        {
          data: [2, 5.5, 2, 8.5, 1.5, 5],
          area: true,
        },
      ]}
      width={500}
      height={300}
    />
    </>
  );
};

const AccountAccordion: React.FC<{ account: Account }> = ({ account }) => {
  const [expanded, setExpanded] = useState<boolean>(false);
  const total = account.stocks.reduce((acc, stock) => acc + stock.price * stock.quantity, 0);

  return (
    <Accordion expanded={expanded} onChange={() => setExpanded(!expanded)}>
      <AccordionSummary expandIcon={<ExpandMoreIcon />}>
        <Typography>{account.accountName}</Typography>
        {!expanded && (
          <Typography sx={{ marginLeft: 'auto' }}>
            Total: ${total.toFixed(2)}
          </Typography>
        )}
      </AccordionSummary>
      <AccordionDetails>
        <StockTable stocks={account.stocks} />
      </AccordionDetails>
    </Accordion>
  );
};

export default function PortfolioPage() {

  const [expanded, setExpanded] = useState<boolean>(false);
  const [isClient, setIsClient] = useState(false);

  useEffect(() => {
    setIsClient(true);
  }, [])

  const grandTotal = accountData.reduce(
    (acc, account) => acc + account.stocks.reduce((acc, stock) => acc + stock.price * stock.quantity, 0),
    0
  );

   // Render only after confirming it's running on the client
  if (!isClient) return null;
  return (
    <Grid2 container spacing={2} justifyContent="center" sx={{ padding: 2 }}>
      <Grid2 xs={12} md={8}>
        <Typography variant="h4" gutterBottom>
          Portfolio
        </Typography>

        {accountData.map((account) => (
          <AccountAccordion key={account.accountName} account={account} />
        ))}

        <Box sx={{ marginTop: 4, textAlign: 'right' }}>
          <Typography variant="h5">
            Grand Total: ${grandTotal.toFixed(2)}
          </Typography>
        </Box>
      </Grid2>

    </Grid2>
  );
};

