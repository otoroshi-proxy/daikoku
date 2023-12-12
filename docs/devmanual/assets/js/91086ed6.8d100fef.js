"use strict";(self.webpackChunkdaikoku_documentation=self.webpackChunkdaikoku_documentation||[]).push([[9902],{1462:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>o,default:()=>u,frontMatter:()=>a,metadata:()=>r,toc:()=>l});var i=n(5893),s=n(1151);const a={},o="Managing tenants",r={id:"usages/adminusage/tenants",title:"Managing tenants",description:"Go to settings/Organizations settings",source:"@site/docs/02-usages/07-adminusage/1-tenants.md",sourceDirName:"02-usages/07-adminusage",slug:"/usages/adminusage/tenants",permalink:"/docs/usages/adminusage/tenants",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Using Daikoku as Daikoku admin",permalink:"/docs/usages/adminusage/"},next:{title:"Managing users",permalink:"/docs/usages/adminusage/users"}},c={},l=[{value:"Create a tenant",id:"create-a-tenant",level:2},{value:"General",id:"general",level:3},{value:"Customization",id:"customization",level:3},{value:"Footer",id:"footer",level:4},{value:"Unlogged home description",id:"unlogged-home-description",level:4},{value:"Audit",id:"audit",level:3},{value:"Mail",id:"mail",level:3},{value:"Authentication",id:"authentication",level:3},{value:"Bucket",id:"bucket",level:3},{value:"Payment",id:"payment",level:3},{value:"Configuring Stripe",id:"configuring-stripe",level:4},{value:"Security",id:"security",level:3},{value:"Display mode",id:"display-mode",level:3}];function d(e){const t={a:"a",blockquote:"blockquote",code:"code",h1:"h1",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",strong:"strong",ul:"ul",...(0,s.a)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(t.h1,{id:"managing-tenants",children:"Managing tenants"}),"\n",(0,i.jsxs)(t.p,{children:["Go to ",(0,i.jsx)(t.code,{children:"settings/Organizations settings"})]}),"\n",(0,i.jsxs)(t.p,{children:["And you should see the list of the existing ",(0,i.jsx)(t.code,{children:"tenants"}),"."]}),"\n",(0,i.jsx)(t.h2,{id:"create-a-tenant",children:"Create a tenant"}),"\n",(0,i.jsxs)(t.p,{children:["To create a new tenant, just click on the ",(0,i.jsx)(t.code,{children:"create a new tenant"})," button."]}),"\n",(0,i.jsx)(t.h3,{id:"general",children:"General"}),"\n",(0,i.jsx)(t.p,{children:"Modify the name and the domain name of the current tenant.\nIt's possible to set the default language of the tenant.\nMaintenance Mode and construction mode can be activated here (only super and tenant admins can login)."}),"\n",(0,i.jsx)(t.p,{children:"A robot.txt file is serve on the path /robot.txt. by default, the file is empty but, you can fill it like you want."}),"\n",(0,i.jsx)(t.h3,{id:"customization",children:"Customization"}),"\n",(0,i.jsxs)(t.p,{children:["Tenants could be customized in Daikoku.\nLogo, title, description can be changed.\nThe client side can be customized with some css code or js code, with overwriting css variables or with a css/js file.\nDaikoku can be customized just on passing a new css color theme (ths theme can be rewritten by using the dedicated page by clicking on the ",(0,i.jsx)(t.code,{children:"Set color theme from UI"})," button)"]}),"\n",(0,i.jsx)(t.p,{children:"You can set a default message, visible for all user clicking on the message button, in the top of conversation modal."}),"\n",(0,i.jsxs)(t.p,{children:["To go further, it is possible to create new pages with the embedded ",(0,i.jsx)(t.a,{href:"/docs/usages/tenantusage/cms",children:"CMS"})]}),"\n",(0,i.jsx)(t.h4,{id:"footer",children:"Footer"}),"\n",(0,i.jsx)(t.p,{children:"A footer can be drawn by daikoku an all frontend page. Just fill code input with HTML code"}),"\n",(0,i.jsx)(t.h4,{id:"unlogged-home-description",children:"Unlogged home description"}),"\n",(0,i.jsx)(t.p,{children:"The unlogged home description is the first content shown to user for private tenant, before the login page.\nIt can also be enabled for public tenant."}),"\n",(0,i.jsx)(t.h3,{id:"audit",children:"Audit"}),"\n",(0,i.jsx)(t.p,{children:"The settings for the output of the audit trail can be set on this page.\nThe output of it, in addition to writing in database, can be an elastic, a kafka or webhooks."}),"\n",(0,i.jsx)(t.p,{children:"Here you can set the email adresses to report some Daikoku alert."}),"\n",(0,i.jsx)(t.h3,{id:"mail",children:"Mail"}),"\n",(0,i.jsx)(t.p,{children:"The mailer type, by default just the standard output.\nMailgun, Mailjet and Sendgrid can be configured as an sass solution.\nOtherwise, a smtp client can be configured."}),"\n",(0,i.jsxs)(t.blockquote,{children:["\n",(0,i.jsxs)(t.p,{children:["The mail templates (one by supported languages) can be edited on the ",(0,i.jsx)(t.code,{children:"internationalization page"})," accessible by clicking the ",(0,i.jsx)(t.code,{children:"edit mail template"})," button."]}),"\n"]}),"\n",(0,i.jsx)(t.h3,{id:"authentication",children:"Authentication"}),"\n",(0,i.jsx)(t.p,{children:"Authentication can be set by choosen Local auth. or a tiers auth. like LDAP, OAuth2 or Otoroshi.\nThe user session duration can be set here, by default it last 24h."}),"\n",(0,i.jsx)(t.h3,{id:"bucket",children:"Bucket"}),"\n",(0,i.jsx)(t.p,{children:"An object storage can be set to store tenant and team assets."}),"\n",(0,i.jsx)(t.h3,{id:"payment",children:"Payment"}),"\n",(0,i.jsx)(t.p,{children:"Daikoku offers full integration with online payment service, allowing users to easily manage their financial transactions as part of the subscription process."}),"\n",(0,i.jsxs)(t.blockquote,{children:["\n",(0,i.jsxs)(t.p,{children:["Currently, only integration with ",(0,i.jsx)(t.a,{href:"https://stripe.com",children:"Stripe"})," is implemented, but our project is open to integrating with other payment platforms. Daikoku's code is designed to be modular, making it easier to add support for additional payment platforms. We welcome contributions to enhance our product in this area."]}),"\n"]}),"\n",(0,i.jsx)(t.h4,{id:"configuring-stripe",children:"Configuring Stripe"}),"\n",(0,i.jsx)(t.p,{children:"Before using Stripe with Daikoku, you need to complete the following steps:"}),"\n",(0,i.jsxs)(t.ul,{children:["\n",(0,i.jsxs)(t.li,{children:["\n",(0,i.jsxs)(t.p,{children:[(0,i.jsx)(t.strong,{children:"Create a Stripe Account"}),": Visit the Stripe website (stripe.com) to create an account. Follow Stripe's instructions to verify your identity and set up your payment account."]}),"\n"]}),"\n",(0,i.jsxs)(t.li,{children:["\n",(0,i.jsxs)(t.p,{children:[(0,i.jsx)(t.strong,{children:"Obtain Stripe API Keys"}),": Once your Stripe account is set up, you need to obtain the necessary API keys to communicate with the Stripe API from Daikoku. These keys include the secret API key and the public API key."]}),"\n"]}),"\n",(0,i.jsxs)(t.li,{children:["\n",(0,i.jsxs)(t.p,{children:[(0,i.jsx)(t.strong,{children:"Configure API Keys in Daikoku"}),": Access the Daikoku configuration settings (in the tenant configuration) and provide the Stripe API keys in the appropriate fields. This allows Daikoku to communicate with Stripe for payment handling."]}),"\n"]}),"\n"]}),"\n",(0,i.jsxs)(t.p,{children:["the next step is to configure your API to accept payment. see this ",(0,i.jsx)(t.a,{href:"/docs/usages/producerusage/apis",children:"part"})," of the doc to learn more"]}),"\n",(0,i.jsx)(t.h3,{id:"security",children:"Security"}),"\n",(0,i.jsx)(t.p,{children:"A tenant can be private or public. In the last case, just public api can be accessed by unauthenticated users."}),"\n",(0,i.jsx)(t.p,{children:"A creation security option can be activated to forbid all unauthorized teams to create API. You just have to authorize the team from the team edit page."}),"\n",(0,i.jsx)(t.p,{children:"A subscription security option can be activated to forbid all personal team to subscribe to an API."}),"\n",(0,i.jsx)(t.p,{children:"An API keys aggregation security can be activated to allow admin API to activate the possibility to aggregate the generated key with an existing API key. It must be activated in every plan"}),"\n",(0,i.jsxs)(t.blockquote,{children:["\n",(0,i.jsx)(t.p,{children:"Beware the risk of override metadata between key using multiple plans."}),"\n"]}),"\n",(0,i.jsx)(t.p,{children:"It's possible to hide API Reference tab (for all APIs) to unlogged user. This is a feature primally intended for public tenants."}),"\n",(0,i.jsx)(t.p,{children:"It's possible to hide teams page (/teams) to prevent everyone for knowing all teams in the organisation. This is a feature primally intended for public tenants."}),"\n",(0,i.jsx)(t.h3,{id:"display-mode",children:"Display mode"}),"\n",(0,i.jsxs)(t.p,{children:["It's possible to switch tenant display mode from ",(0,i.jsx)(t.code,{children:"default"})," to ",(0,i.jsx)(t.code,{children:"environment"}),".\nsee this ",(0,i.jsx)(t.a,{href:"/docs/usages/tenantusage/5.5-display",children:"part"})," of the doc to learn more"]})]})}function u(e={}){const{wrapper:t}={...(0,s.a)(),...e.components};return t?(0,i.jsx)(t,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},1151:(e,t,n)=>{n.d(t,{Z:()=>r,a:()=>o});var i=n(7294);const s={},a=i.createContext(s);function o(e){const t=i.useContext(a);return i.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function r(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:o(e.components),i.createElement(a.Provider,{value:t},e.children)}}}]);