"use strict";(self.webpackChunkdaikoku_documentation=self.webpackChunkdaikoku_documentation||[]).push([[2237],{3214:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>c,contentTitle:()=>o,default:()=>h,frontMatter:()=>a,metadata:()=>r,toc:()=>l});var t=i(5893),s=i(1151);const a={},o="Managing APIs",r={id:"usages/producerusage/apis",title:"Managing APIs",description:"Create a new API",source:"@site/docs/02-usages/09-producerusage/1-apis.md",sourceDirName:"02-usages/09-producerusage",slug:"/usages/producerusage/apis",permalink:"/docs/usages/producerusage/apis",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Using Daikoku as api producer",permalink:"/docs/usages/producerusage/"},next:{title:"Managing teams",permalink:"/docs/usages/producerusage/members"}},c={},l=[{value:"Create a new API",id:"create-a-new-api",level:2},{value:"API informations",id:"api-informations",level:3},{value:"Versions and tags",id:"versions-and-tags",level:3},{value:"Visibililty",id:"visibililty",level:3},{value:"Authorizations",id:"authorizations",level:3},{value:"Description",id:"description",level:3},{value:"Plans",id:"plans",level:3},{value:"Otoroshi, billing and security",id:"otoroshi-billing-and-security",level:4},{value:"OpenAPI definition",id:"openapi-definition",level:3},{value:"Testing",id:"testing",level:3},{value:"Configure your openAPI",id:"configure-your-openapi",level:4},{value:"Documentation",id:"documentation",level:3},{value:"Manage subscription",id:"manage-subscription",level:2},{value:"Api consumptions",id:"api-consumptions",level:2}];function d(e){const n={a:"a",admonition:"admonition",blockquote:"blockquote",code:"code",h1:"h1",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,s.a)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.h1,{id:"managing-apis",children:"Managing APIs"}),"\n",(0,t.jsx)(n.h2,{id:"create-a-new-api",children:"Create a new API"}),"\n",(0,t.jsxs)(n.p,{children:["To create a new API, you can click on the ",(0,t.jsx)(n.code,{children:"+ API"})," button in the catalog page or the ",(0,t.jsx)(n.code,{children:"Create new API"})," button in the Team's APIs page in your back offices' team.\nAfter clicking on the button localized in the catalog page, you need to choose a team which is the owner."]}),"\n",(0,t.jsx)(n.h3,{id:"api-informations",children:"API informations"}),"\n",(0,t.jsx)(n.p,{children:'An API needs a name to be created.\nThen, you can add a small description, which will be displayed in the corresponding catalog\'s page.\nThe API can be published or not. In the latter case, you can consider this as a draft.\nAn image can be provide for the "card view" of the Api list page.\nto go further in customization of Daikoku, the header of frontebd page of all APIs can be customized by HTML'}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsxs)(n.p,{children:["To keep the title and the description use title and description surrended by double brace in your template. You can add a button with the ",(0,t.jsx)(n.code,{children:"btn-edit"})," classname to add a direct link to API backoffice for your team members."]}),"\n"]}),"\n",(0,t.jsx)(n.h3,{id:"versions-and-tags",children:"Versions and tags"}),"\n",(0,t.jsx)(n.p,{children:"Versionning your API is supported. You can update the current version of your API."}),"\n",(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsxs)(n.p,{children:["Make sure that your main version is toggle as ",(0,t.jsx)(n.code,{children:"use as last version"})," to be sure the link in the API list redirect to the correct version of your API."]})}),"\n",(0,t.jsx)(n.p,{children:"Supported versions are pure information for your users.\nTags and categories are array of item, mostly used to filter APIs."}),"\n",(0,t.jsx)(n.h3,{id:"visibililty",children:"Visibililty"}),"\n",(0,t.jsx)(n.p,{children:"Visibility can be:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"public: everybody can see the complete API."}),"\n",(0,t.jsx)(n.li,{children:"public with authorization: everybody sees just a part of the API, on the catalog's page (name, tags, categories and small desc.). Everybody can ask access to an admin of owner team."}),"\n",(0,t.jsx)(n.li,{children:"private: Just authorized teams have access to the API."}),"\n"]}),"\n",(0,t.jsx)(n.h3,{id:"authorizations",children:"Authorizations"}),"\n",(0,t.jsx)(n.p,{children:"The teams which have access to the API, in the case of visibility is private."}),"\n",(0,t.jsx)(n.h3,{id:"description",children:"Description"}),"\n",(0,t.jsx)(n.p,{children:"API description. Basically it can be written in markdown but you can use HTML.\nThe description can be set from team asset."}),"\n",(0,t.jsx)(n.h3,{id:"plans",children:"Plans"}),"\n",(0,t.jsxs)(n.p,{children:["An API needs a plan to be subscribed.\nPlan needs a name, possibly a description and an ",(0,t.jsx)(n.a,{href:"/docs/usages/tenantusage/otoroshi",children:"otoroshi instance"}),"."]}),"\n",(0,t.jsxs)(n.p,{children:["You can define a plan as ",(0,t.jsx)(n.strong,{children:"default plan"})," as it's possible to ",(0,t.jsx)(n.strong,{children:"make it private"})," (only accessible by the producer team)\nIt's possible to ",(0,t.jsx)(n.strong,{children:"allow multiple API keys"})," for a plan (by default a team can only have one API key).\nIt's possible to ",(0,t.jsx)(n.strong,{children:"allow API keys aggregation"})]}),"\n",(0,t.jsx)(n.p,{children:"It's important to choose a type of plan :"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"free without quotas"}),": a plan with an unlimited number of calls per day and per month."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"free with quotas"}),": a plan with a limited number of calls per day and per month. Quotas will be set by default and can be overwritten."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"quotas with limit"}),": a priced plan with a limited number of calls per day and per month. Quotas will be set by default but can be overwritten. A fixed cost by month can be set."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"quotas without limit"}),": a priced plan with unlimited number of calls per day and per month. Quotas will be set by default but can be overwritten. A fixed cost by month can be set. The cost per additional requests can be set."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"pay per use"}),": a plan priced on usage. A fixed cost by month can be set. The cost per additional requests can be set."]}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:"The subscription process in Daikoku offers flexibility, allowing users to customize their validation steps. There are three possible types of steps:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"Admin Validation"}),": This step requires an administrator from the AP I's owning team to validate the subscription request. When a request is submitted, all administrators in the team receive a Daikoku notification and an email containing the request details. The first administrator who validates the notification approves the step."]}),"\n"]}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsxs)(n.p,{children:["it's possible to configure this step to display a specific form when user ask subscription.\nto parameter the form, we use the json language use by ",(0,t.jsx)(n.a,{href:"https://github.com/MAIF/react-forms#maifreact-forms",children:"@maif/react-forms"}),".\nYou can write a formatter to format the form response as a notification sended to admins. it's just a string with a replacement pattern like this ",(0,t.jsx)(n.code,{children:"[[label]]"}),"\nthe result of the form is used to automatically generate metadata which will be deleteable or modifiable before validation by the admin"]}),"\n"]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"HTTP Request"}),": This step is a semi-automatic step. A http POST is sended to an API with the whole context of the subscription demand\nA json response is awaited with a precise format."]}),"\n"]}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsxs)(n.p,{children:["a property ",(0,t.jsx)(n.code,{children:"accept"})," as boolean to accept or reject the subscription demand\nit totally possible to calculate and give some metadata or custom property for the subscription to created"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-json",children:'{\n  "accept": true,\n  "customMaxPerDay": 42,\n  "customMaxPerSecond": 42,\n  "customMaxPerMonth": 42,\n  "customMetadata": {\n    "value1": 2,\n    "value2": true,\n    "value3": "foo"\n  },\n  "adminCustomName": "foo-bar-apikey",\n  "customReadOnly": true\n}\n'})}),"\n",(0,t.jsx)(n.p,{children:"the body of the call contains lot of data as the context:  the subscription demand with the other step information, the api, the usage plan, the team and the aggragation in case of an aggergated apikey"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-json",children:'{\n  "demand": {},\n  "api": {},\n  "plan": {},\n  "team": {},\n  "aggregate": {\n    "parent": {\n      "api": {},\n      "plan": {},\n      "subscription": {} \n    },\n    "subscriptions": [\n      {\n        "api": {},\n        "plan": {},\n        "subscription": {} \n      }\n    ]\n  } \n}\n'})}),"\n"]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.strong,{children:"Email Validation"}),": This step involves a third-party person validating the subscription request by clicking on a validation link sent to the provided email address. Once the person clicks on the link, the request is validated, and the user is redirected to a public acknowledgment interface."]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.strong,{children:"Payment Validation"}),": This step requires the requester to make a payment through a payment gateway. The user is redirected to a payment page provided by the payment gateway where they need to enter their payment information. Once the payment is validated, the subscription is approved, and the user is redirected to Daikoku's home page."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsx)(n.p,{children:"When a consumption plan for an API is priced, after its creation, one or more Stripe products are created based on the plan type:"}),"\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.strong,{children:"Quotas Only"}),": A fixed-price subscription product is created."]}),"\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.strong,{children:"Quotas / Pay Per Use"}),": Two products are created: a fixed-price subscription and a variable component linked to the user's consumption, which is synchronized with Stripe at the end of each day."]}),"\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.strong,{children:"Pay Per Use"}),": Similarly, two products are created: a fixed-price subscription and a variable component linked to the user's consumption."]}),"\n"]}),"\n",(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsx)(n.p,{children:"Please note that the email validation step can be used multiple times, while the other steps can only be completed once in the process."})}),"\n",(0,t.jsx)(n.p,{children:"The subscription process can be customized by adding new steps based on specific requirements."}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsxs)(n.p,{children:["Daikoku is an open-source project, and we encourage community initiatives to contribute new step types. We invite contributors to submit Pull Requests on the ",(0,t.jsx)(n.a,{href:"https://github.com/MAIF/daikoku",children:"project's GitHub repository"})," to propose new features."]}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:'To visualize and customize the subscription process, you can access the "Process" tab from the plan modification page. From this tab, you can view the entire process, add new steps, modify existing ones, or delete steps.'}),"\n",(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsx)(n.p,{children:"Please be aware that modifying or deleting a step immediately affects ongoing requests, either modifying them or validating them if necessary."})}),"\n",(0,t.jsx)(n.p,{children:"When submitting a subscription request, the only required data is the team for which you want to subscribe. The subscription process may also prompt you to provide a motivation, particularly if the admin validation step is included. In such cases, a modal window appears to allow for detailed motivation for the request."}),"\n",(0,t.jsx)(n.p,{children:"Once the subscription request is validated or declined, notifications and emails are generated to inform the user of the request's status."}),"\n",(0,t.jsx)(n.p,{children:"If your subscription plan is associated with consumption-based pricing, you can also modify certain billing information, such as your name, address, and payment details. The link to the billing information modification page, stored by the payment gateway, can be found on Daikoku's billing page."}),"\n",(0,t.jsx)(n.h4,{id:"otoroshi-billing-and-security",children:"Otoroshi, billing and security"}),"\n",(0,t.jsx)(n.p,{children:"Depending on chosen plan type, certain custom properties may be accessibles."}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Plan needs an Otoroshi instance to allow users to subscribe. After choosing an instance, a list of Otoroshi service groups and services is accessible to link daikoku api/plan with one or more Otoroshi service group or services."}),"\n",(0,t.jsx)(n.li,{children:"As it's Otoroshi which manages apis, apikey quotas can be define."}),"\n",(0,t.jsxs)(n.li,{children:["Daikoku provide, like ",(0,t.jsx)(n.a,{href:"https://maif.github.io/otoroshi/manual/entities/apikeys.html",children:"Otoroshi"}),", some apikey parameters."]}),"\n",(0,t.jsx)(n.li,{children:"Daikoku side, billing informations can be filled (billing period, cost per month, currency ...)"}),"\n",(0,t.jsx)(n.li,{children:"For security, you can force apikey rotation. it's an Otoroshi feature that will reset clientSecret every month with a grace period of 1 week (during this week both secrets works)"}),"\n",(0,t.jsxs)(n.li,{children:["You can force the integration process :","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"ApiKey: subscribers have access to an apikey to call api"}),"\n",(0,t.jsx)(n.li,{children:"Automatic: Subscribers have just access to a token, which link to a real apikey, accessible by admin api. It's a perfect solution to integrate automatically your apikey in your prod environment if rotation is activated."}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.admonition,{type:"note",children:(0,t.jsxs)(n.p,{children:["As Otoroshi does, it's possible to add metadata on API keys. ",(0,t.jsx)(n.strong,{children:"Automatic metadata"})," will be calculated and added after subscription validation. ",(0,t.jsx)(n.strong,{children:"Asked metadata"})," will switch the plan subscription mode to manual then, on susbcription acceptation, a team admin will have to add the metadata manually."]})}),"\n",(0,t.jsx)(n.h3,{id:"openapi-definition",children:"OpenAPI definition"}),"\n",(0,t.jsx)(n.p,{children:"The OpenAPI definition can be provided as a url or just some content paste on the UI.\nan additional configuration allow to"}),"\n",(0,t.jsx)(n.h3,{id:"testing",children:"Testing"}),"\n",(0,t.jsx)(n.p,{children:"You can enable the testing for your API."}),"\n",(0,t.jsxs)(n.blockquote,{children:["\n",(0,t.jsx)(n.p,{children:"The testing is based on the openAPI definition of your API. Beware of set up the right host of your testing service."}),"\n"]}),"\n",(0,t.jsxs)(n.p,{children:["Click on the ",(0,t.jsx)(n.code,{children:"Generate a dedicated testing key in Otoroshi"})," to choose an otoroshi instance and and service group or service which is used to receive the testing APIkey. Then, just follow the instruction display on UI?"]}),"\n",(0,t.jsx)(n.h4,{id:"configure-your-openapi",children:"Configure your openAPI"}),"\n",(0,t.jsxs)(n.p,{children:["Your openAPI have to be configured to accept apikey from Basic authentication header or from the ",(0,t.jsx)(n.code,{children:"Otoroshi-Client-Id"})," and ",(0,t.jsx)(n.code,{children:"Otoroshi-Client-Secret"})," headers."]}),"\n",(0,t.jsx)(n.p,{children:"If you had changed the Otoroshi headers to pass the apikey don't forget to apply the changes on your openAPI."}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-json",children:"...\npaths:\n  /api/_verify:\n    get:\n      summary: Verification using query params\n      operationId: getVerify\n      ...\n      security:\n      - basicAuth: []\n      - otoClientId: []\n        otoClientSecret: []\ncomponents:\n  schemas:\n  securitySchemes:\n    basicAuth:\n      type: http\n      scheme: basic\n    otoClientId:\n      type: apiKey\n      name: Otoroshi-Client-Id\n      in: header\n    otoClientSecret:\n      type: apiKey\n      name: Otoroshi-Client-Secret\n      in: header\n"})}),"\n",(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsx)(n.p,{children:"Make sure this service descriptor is the right one for testing and not your production system !"})}),"\n",(0,t.jsx)(n.h3,{id:"documentation",children:"Documentation"}),"\n",(0,t.jsx)(n.p,{children:"The documentation tabs allows you to create a paginated documentation. Like description every pages can be written with markdown or set from asset."}),"\n",(0,t.jsx)(n.h2,{id:"manage-subscription",children:"Manage subscription"}),"\n",(0,t.jsxs)(n.p,{children:["On the team APIs screen on your team back office, it's possible to manage for every APIs its subscriptions by clicking on the ",(0,t.jsx)(n.code,{children:"key"})," button.\nYou can activate/deactivate API keys or update metadata."]}),"\n",(0,t.jsx)(n.h2,{id:"api-consumptions",children:"Api consumptions"}),"\n",(0,t.jsxs)(n.p,{children:["On the team APIs screen on your team back office, it's possible to see for every APIs consumptions by clicking on the ",(0,t.jsx)(n.code,{children:"stats"})," button. Global stats by default but visible by API key or usage plan."]})]})}function h(e={}){const{wrapper:n}={...(0,s.a)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(d,{...e})}):d(e)}},1151:(e,n,i)=>{i.d(n,{Z:()=>r,a:()=>o});var t=i(7294);const s={},a=t.createContext(s);function o(e){const n=t.useContext(a);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:o(e.components),t.createElement(a.Provider,{value:n},e.children)}}}]);