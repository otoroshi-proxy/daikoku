import { ApolloClient, ApolloProvider, InMemoryCache } from '@apollo/client';
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import jQuery from 'jquery';
import { useContext } from 'react';
import { createRoot } from 'react-dom/client';
import { toast, Toaster } from 'sonner';

import { DaikokuApp } from './apps';
import { GlobalContext, GlobalContextProvider } from './contexts/globalContext';
import { I18nProvider } from './contexts/i18n-context';

import '@maif/react-forms/lib/index.css';
import 'bootstrap/dist/css/bootstrap.css';
import 'react-tooltip/dist/react-tooltip.css';
import '@fortawesome/fontawesome-free/css/all.min.css'
import './style/main.scss';

import 'bootstrap';


const client = new ApolloClient({
  uri: '/api/search',
  cache: new InMemoryCache(),
  defaultOptions: {
    query: {
      fetchPolicy: 'network-only',
    },
  },
});

(window as any).$ = jQuery;
(window as any).jQuery = jQuery;

const container = document.getElementById('app');
const root = createRoot(container!)

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false, // TODO for dev only
      refetchOnWindowFocus: false, // TODO for dev only
    },
  },
});

const ToasterComponent = () => {
  const { theme } = useContext(GlobalContext)

  return (
    <Toaster richColors position="top-right" theme={theme.toLocaleLowerCase() as 'light' | 'dark'} containerAriaLabel='notifications'/>
  )
}


root.render(
  <ApolloProvider client={client}>
    <QueryClientProvider client={queryClient}>
      <GlobalContextProvider>
        <I18nProvider>
          <ToasterComponent />
          <DaikokuApp />
        </I18nProvider>
      </GlobalContextProvider>
    </QueryClientProvider>
  </ApolloProvider>

);

// export function init(
//   user: any,
//   tenant: any,
//   session: any,
//   loginCallback: any,
// ) {


//   const container = document.getElementById('app');
//   const root = createRoot(container!)

//   const queryClient = new QueryClient({
//     defaultOptions: {
//       queries: {
//         retry: false, // TODO for dev only
//         refetchOnWindowFocus: false, // TODO for dev only
//       },
//     },
//   });


//   root.render(
//     <ApolloProvider client={client}>
//       <QueryClientProvider client={queryClient}>
//         <GlobalContextProvider>
//           <I18nProvider tenant={tenant} user={user}>
//             <ToasterComponent />
//             <DaikokuApp />
//           </I18nProvider>
//         </GlobalContextProvider>
//       </QueryClientProvider>
//     </ApolloProvider>

//   );
// }



// export function login(provider: any, callback: any, tenant: any, redirect?: string) {
//   ReactDOM.render(
//     <I18nProvider tenant={tenant}>
//       <LoginPage provider={provider} action={callback} tenant={tenant} redirect={redirect} method="post" />
//     </I18nProvider>,
//     document.getElementById('app')
//   );
// }

// export function initNotLogged(tenant: any) {

//   const container = document.getElementById('app');
//   const root = createRoot(container!)

//   root.render(
//     <I18nProvider tenant={tenant}>
//       <DaikokuHomeApp tenant={tenant} />
//     </I18nProvider>
//   );
// }
