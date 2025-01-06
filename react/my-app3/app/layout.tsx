import * as React from 'react';
import { AppProps } from 'next/app';
import { AppProvider } from '@toolpad/core/nextjs';
import { AppRouterCacheProvider } from '@mui/material-nextjs/v14-appRouter';
import DashboardIcon from '@mui/icons-material/Dashboard';
import ListIcon from '@mui/icons-material/List';
import ViewListIcon from '@mui/icons-material/ViewList';
import type { Navigation } from '@toolpad/core';
import { DndProvider } from "react-dnd";
import { HTML5Backend } from "react-dnd-html5-backend";
import theme from '../theme';

const NAVIGATION: Navigation = [
  {
    kind: 'header',
    title: 'Main items',
  },
  {
    segment: '',
    title: 'Dashboard',
    icon: <DashboardIcon />,
  },
  {
    segment: 'portfolio',
    title: 'Portfolio',
    icon: <ListIcon />,
  },
  {
    segment: 'accounts',
    title: 'Accounts',
    icon: <ViewListIcon />,
  },
  {
    segment: 'risk',
    title: 'Risk',
    icon: <ViewListIcon />,
  },
  {
    segment: 'd3jscharts',
    title: 'D3 JS Charts',
    icon: <ViewListIcon />,
  },
  {
    segment: 'playground1',
    title: 'Playground1',
    icon: <ViewListIcon />,
  },
  {
    segment: 'playground2',
    title: 'Playground2',
    icon: <ViewListIcon />,
  },
  {
    segment: 'playground3',
    title: 'Playground3',
    icon: <ViewListIcon />,
  },
];

const BRANDING = {
  title: 'My Toolpad Core Next.js App',
};



export default function RootLayout(props: { children: React.ReactNode }) {
  

  return (
    <html lang="en" data-toolpad-color-scheme="light" suppressHydrationWarning>
      <body>
          <AppRouterCacheProvider options={{ enableCssLayer: true }}>
          
            <AppProvider
              navigation={NAVIGATION}
              branding={BRANDING}
              
              theme={theme}
            >
             
              {props.children}
              
            </AppProvider>
        
          </AppRouterCacheProvider>

      </body>
    </html>
  );
}
