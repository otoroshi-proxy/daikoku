"use strict";(self.webpackChunkdaikoku_documentation=self.webpackChunkdaikoku_documentation||[]).push([[7192],{94821:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>s,metadata:()=>t,toc:()=>c});const t=JSON.parse('{"id":"usages/tenantusage/translations","title":"Translate mail content and front office","description":"Daikoku supports two languages///settings/internationalization/mail).","source":"@site/docs/02-usages/08-tenantusage/5-translations.md","sourceDirName":"02-usages/08-tenantusage","slug":"/usages/tenantusage/translations","permalink":"/daikoku/docs/usages/tenantusage/translations","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":5,"frontMatter":{},"sidebar":"tutorialSidebar","previous":{"title":"User messages","permalink":"/daikoku/docs/usages/tenantusage/messages"},"next":{"title":"Exploring the CMS","permalink":"/daikoku/docs/usages/tenantusage/cms"}}');var i=a(74848),o=a(28453);const s={},r="Translate mail content and front office",l={},c=[{value:"Mail",id:"mail",level:3},{value:"Mail template",id:"mail-template",level:3},{value:"Front office",id:"front-office",level:3}];function d(e){const n={admonition:"admonition",blockquote:"blockquote",code:"code",h1:"h1",h3:"h3",header:"header",li:"li",ol:"ol",p:"p",pre:"pre",...(0,o.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"translate-mail-content-and-front-office",children:"Translate mail content and front office"})}),"\n",(0,i.jsxs)(n.p,{children:["Daikoku supports two languages : French and English (If your language is missing, you can contribute, do not hesitate). All translations can be override when navigating on the ",(0,i.jsx)(n.code,{children:"internationalization"})," page from a tenant settings page (the full route is ",(0,i.jsx)(n.code,{children:"https://<your-domain-name>/settings/internationalization/mail"}),")."]}),"\n",(0,i.jsxs)(n.p,{children:["On this page, you have 3 tabs : ",(0,i.jsx)(n.code,{children:"Mail"}),", ",(0,i.jsx)(n.code,{children:"Mail Template"})," and ",(0,i.jsx)(n.code,{children:"Front office"}),"."]}),"\n",(0,i.jsx)(n.h3,{id:"mail",children:"Mail"}),"\n",(0,i.jsxs)(n.p,{children:["The mail tab manages all the translations of the mails content. For each translation, you have a list of ",(0,i.jsx)(n.code,{children:"required variables"})," which will be replaced by Daikoku when an email is send"]}),"\n",(0,i.jsxs)(n.blockquote,{children:["\n",(0,i.jsx)(n.p,{children:"Beware of surround your variable with square brackets if you want it to be replaced."}),"\n"]}),"\n",(0,i.jsx)(n.p,{children:"For example, the message"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"Your apikey for api [apiName] and plan [planName] have been updated.\n"})}),"\n",(0,i.jsxs)(n.p,{children:["Has by default two variable ",(0,i.jsx)(n.code,{children:"apiName"})," and ",(0,i.jsx)(n.code,{children:"planName"})," which will be replace by the name of the API and the name of the plan respectively."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"Your apikey for api My First API and plan My first plan have been updated. \n"})}),"\n",(0,i.jsx)(n.p,{children:"Each translation has one field by language. Once a translation is overloaded, a reset button will appear to retrieve the original translation."}),"\n",(0,i.jsx)(n.h3,{id:"mail-template",children:"Mail template"}),"\n",(0,i.jsx)(n.p,{children:"All sent mails are composed of the subject of the mail as body, incorporate in a template. This template can be change and translate in the supported languages."}),"\n",(0,i.jsxs)(n.p,{children:["When you want to change one of these fields, we have to include one ",(0,i.jsx)(n.code,{children:"required variable"})," which is the ",(0,i.jsx)(n.code,{children:"email"})," variable. That variable is replaced by Daikoku, depending on the subject of the email."]}),"\n",(0,i.jsx)(n.p,{children:"If we take the previous example, without any changes, you should have for an update of API :"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"Your apikey for api My First API and plan My first plan have been updated. \n"})}),"\n",(0,i.jsxs)(n.p,{children:["Now if you want to add a header and a footer to your email, you have to write in the first field ",(0,i.jsx)(n.code,{children:"Default mail template"})]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"Hello,\n\n{{email}}\n\nSend from Daikoku.\n"})}),"\n",(0,i.jsx)(n.p,{children:"The mail will be sent with the following content:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"Hello,\n\nYour apikey for api My First API and plan My first plan have been updated. \n\nSend from Daikoku.\n"})}),"\n",(0,i.jsx)(n.h3,{id:"front-office",children:"Front office"}),"\n",(0,i.jsx)(n.p,{children:"Daikoku is a fully customizable user interface that includes the ability to translate the front office (which represents a list of one thousand words)."}),"\n",(0,i.jsx)(n.p,{children:"To change one expression:"}),"\n",(0,i.jsxs)(n.ol,{children:["\n",(0,i.jsx)(n.li,{children:"Search the expression to change."}),"\n",(0,i.jsx)(n.li,{children:"Replace the value for your current language."}),"\n",(0,i.jsx)(n.li,{children:"Click on the save button on your right."}),"\n",(0,i.jsx)(n.li,{children:"Refresh your page (Daikoku is caching the translations to avoid fetching the translations in the back office each time they are needed)."}),"\n"]}),"\n",(0,i.jsx)(n.admonition,{type:"warning",children:(0,i.jsx)(n.p,{children:"Some expressions are used in different pages, beware of breaking changes."})})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},28453:(e,n,a)=>{a.d(n,{R:()=>s,x:()=>r});var t=a(96540);const i={},o=t.createContext(i);function s(e){const n=t.useContext(o);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:s(e.components),t.createElement(o.Provider,{value:n},e.children)}}}]);