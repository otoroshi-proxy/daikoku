"use strict";(self.webpackChunkdaikoku_documentation=self.webpackChunkdaikoku_documentation||[]).push([[9235],{5277:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>d,contentTitle:()=>l,default:()=>p,frontMatter:()=>s,metadata:()=>c,toc:()=>h});var t=i(5893),o=i(1151),a=i(4866),r=i(5162);const s={},l="Authentication",c={id:"guides/authentication",title:"Authentication",description:"Authentication in daikoku can be tricky. It's a tenant configuration, we are here to setup some examples.",source:"@site/docs/03-guides/14-authentication.mdx",sourceDirName:"03-guides",slug:"/guides/authentication",permalink:"/daikoku/docs/guides/authentication",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:14,frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Deploy to production",permalink:"/daikoku/docs/guides/deploy"},next:{title:"CLI",permalink:"/daikoku/docs/cli/"}},d={},h=[{value:"Before starting",id:"before-starting",level:2},{value:"Running an openldap server",id:"running-an-openldap-server",level:2},{value:"Create an Authentication configuration",id:"create-an-authentication-configuration",level:2},{value:"Testing your configuration",id:"testing-your-configuration",level:2},{value:"Before starting",id:"before-starting-1",level:2},{value:"Running an otoroshi server",id:"running-an-otoroshi-server",level:2},{value:"Create an authentication module",id:"create-an-authentication-module",level:2},{value:"Expose your daikoku by Otoroshi",id:"expose-your-daikoku-by-otoroshi",level:2},{value:"Create an Authentication configuration",id:"create-an-authentication-configuration-1",level:2},{value:"Testing your configuration",id:"testing-your-configuration-1",level:2},{value:"Before starting",id:"before-starting-2",level:2},{value:"Configure an Auth0 client",id:"configure-an-auth0-client",level:2},{value:"Create authentication configuration",id:"create-authentication-configuration",level:2},{value:"Testing your configuration",id:"testing-your-configuration-2",level:2}];function u(e){const n={a:"a",admonition:"admonition",code:"code",em:"em",h1:"h1",h2:"h2",li:"li",mdxAdmonitionTitle:"mdxAdmonitionTitle",p:"p",pre:"pre",ul:"ul",...(0,o.a)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.h1,{id:"authentication",children:"Authentication"}),"\n",(0,t.jsx)(n.p,{children:"Authentication in daikoku can be tricky. It's a tenant configuration, we are here to setup some examples."}),"\n",(0,t.jsxs)(a.Z,{children:[(0,t.jsx)(r.Z,{value:"local",label:"Local",default:!0,children:(0,t.jsx)(n.p,{children:"this is the default authentication mode. Every user can create an account in your Daikoku instance, the user profil is saved in database.\nThere is no needed configuration for this mode."})}),(0,t.jsxs)(r.Z,{value:"ldap",label:"LDAP",children:[(0,t.jsx)(n.h2,{id:"before-starting",children:"Before starting"}),(0,t.jsx)(n.p,{children:"If you already have an up and running Daikoku instance, you can skip the following instructions and log in to your instance."}),(0,t.jsxs)(n.p,{children:["Let\u2019s start by ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/getdaikoku/",children:"downloading the latest Daikoku"})," and ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/firstrun/run",children:"run it"})]}),(0,t.jsx)(n.p,{children:"Once Daikoku is started you can log in to your brand new instance."}),(0,t.jsx)(n.h2,{id:"running-an-openldap-server",children:"Running an openldap server"}),(0,t.jsxs)(n.p,{children:["Run  ghcr.io/rroemhild/docker-test-openldap",":master"," docker Image"]}),(0,t.jsxs)(n.admonition,{type:"note",children:[(0,t.jsx)(n.mdxAdmonitionTitle,{}),(0,t.jsx)(n.p,{children:(0,t.jsxs)(n.em,{children:["you can find all documention on the ",(0,t.jsx)(n.a,{href:"https://github.com/rroemhild/docker-test-openldap",children:"github repo"})]})})]}),(0,t.jsx)(n.p,{children:"first, pull and run your ldap. This openldap is already initialized with data based on futurama tv show."}),(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-bash",children:"docker pull ghcr.io/rroemhild/docker-test-openldap:master\ndocker run --rm -p 10389:10389 -p 10636:10636 ghcr.io/rroemhild/docker-test-openldap:master\n\n"})}),(0,t.jsx)(n.p,{children:"Let\u2019s make the first search in our LDAP container :"}),(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-bash",children:'# List all Users\nldapsearch -H ldap://localhost:10389 -x -b "ou=people,dc=planetexpress,dc=com" -D "cn=admin,dc=planetexpress,dc=com" -w GoodNewsEveryone "(objectClass=inetOrgPerson)"\n'})}),(0,t.jsx)(n.p,{children:"the response is very long due to image but at the end you shoulf have the following output:"}),(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-Bash",children:"  ...\n  ...\n  # admin_staff, people, planetexpress.com\n  dn: cn=admin_staff,ou=people,dc=planetexpress,dc=com\n  objectClass: Group\n  objectClass: top\n  groupType: 2147483650\n  cn: admin_staff\n  member: cn=Hubert J. Farnsworth,ou=people,dc=planetexpress,dc=com\n  member: cn=Hermes Conrad,ou=people,dc=planetexpress,dc=com\n\n  # ship_crew, people, planetexpress.com\n  dn: cn=ship_crew,ou=people,dc=planetexpress,dc=com\n  objectClass: Group\n  objectClass: top\n  groupType: 2147483650\n  cn: ship_crew\n  member: cn=Philip J. Fry,ou=people,dc=planetexpress,dc=com\n  member: cn=Turanga Leela,ou=people,dc=planetexpress,dc=com\n  member:: Y249QmVuZGVyIEJlbmRpbmcgUm9kcsOtZ3VleixvdT1wZW9wbGUsZGM9cGxhbmV0ZXhwc\n  mVzcyxkYz1jb20=\n\n  # search result\n  search: 2\n  result: 0 Success\n\n  # numResponses: 11\n  # numEntries: 10\n"})}),(0,t.jsx)(n.h2,{id:"create-an-authentication-configuration",children:"Create an Authentication configuration"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Go ahead and navigate to your Daikoku home page"}),"\n",(0,t.jsx)(n.li,{children:"Click on the your avatar on bottom left of the screen"}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"<your-tenant-name> settings"})]}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"Authentication"})," on left panel"]}),"\n",(0,t.jsxs)(n.li,{children:["You must be on ",(0,t.jsx)(n.code,{children:"<your-domain>/settings/settings/authentication"})]}),"\n",(0,t.jsxs)(n.li,{children:["This page show the settings for the current authentication mode (by default ",(0,t.jsx)(n.code,{children:"Local"}),")"]}),"\n",(0,t.jsxs)(n.li,{children:["Click on ",(0,t.jsx)(n.code,{children:"LDAP"})," at the top of the form"]}),"\n",(0,t.jsxs)(n.li,{children:["Add a ",(0,t.jsx)(n.code,{children:"LDAP Server URL"})," with value ",(0,t.jsx)(n.code,{children:"ldap://localhost:10389"})," and ",(0,t.jsx)(n.code,{children:"dc=planetexpress,dc=com"})," as ",(0,t.jsx)(n.code,{children:"Search base"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"ou=people"})," as ",(0,t.jsx)(n.code,{children:"Users search base"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"cn=ship_crew"})," as ",(0,t.jsx)(n.code,{children:"Simple user filter"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"cn=admin_staff"})," as ",(0,t.jsx)(n.code,{children:"Daikoku admin filter"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"(mail=${username})"})," as ",(0,t.jsx)(n.code,{children:"Search filter"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"cn=admin,dc=planetexpress,dc=com"})," as ",(0,t.jsx)(n.code,{children:"Admin username (bind DN)"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"GoodNewsEveryone"})," as ",(0,t.jsx)(n.code,{children:"Admin password"})]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"Name fields name"})," can be ",(0,t.jsx)(n.code,{children:"givenName"})," and ",(0,t.jsx)(n.code,{children:"sn"})]}),"\n",(0,t.jsxs)(n.li,{children:["Set ",(0,t.jsx)(n.code,{children:"mail"})," as ",(0,t.jsx)(n.code,{children:"Email field name"})]}),"\n"]}),(0,t.jsxs)(n.p,{children:["With this configuration, all ldap users with cn ",(0,t.jsx)(n.code,{children:"admin_staff"})," will be Daikoku admin otherwise, with cn ",(0,t.jsx)(n.code,{children:"ship_crew"})," he will be a simple user."]}),(0,t.jsx)(n.h2,{id:"testing-your-configuration",children:"Testing your configuration"}),(0,t.jsxs)(n.p,{children:["Disconnect from your instance\nThen click on the Login button (or navigate to ",(0,t.jsx)(n.code,{children:"<your-daikoku-domain>/login"}),")\nSet ",(0,t.jsx)(n.a,{href:"mailto:fry@planetexpress.com",children:"fry@planetexpress.com"}),"/fry as credentials for a simple user and ",(0,t.jsx)(n.a,{href:"mailto:professor@planetexpress.com",children:"professor@planetexpress.com"}),"/professor for a Daikoku admin."]}),(0,t.jsxs)(n.p,{children:["A fallback solution is always available in the event of a bad authentication configuration.\nBy going to ",(0,t.jsx)(n.code,{children:"<your-daikoku-domain>.login"}),", the previous local administrators will be able to login."]}),(0,t.jsxs)(n.admonition,{type:"warning",children:[(0,t.jsx)(n.p,{children:"In one case, your search filter can be annoying.\nif instead of mail, you've choose to log user by uid, when team admins will wants to invite a collaborator, they will be required to know the user uid."}),(0,t.jsxs)(n.p,{children:["In this case you can replace ",(0,t.jsx)(n.code,{children:"Search filter"})," by ",(0,t.jsx)(n.code,{children:"(|(uid=${username})(mail=${username}))"})," both ",(0,t.jsx)(n.code,{children:"uid"})," and ",(0,t.jsx)(n.code,{children:"mail"})," will be auhtorized to log in (and search collaborator)"]})]})]}),(0,t.jsxs)(r.Z,{value:"otoroshi",label:"Otoroshi",default:!0,children:[(0,t.jsx)(n.h2,{id:"before-starting-1",children:"Before starting"}),(0,t.jsx)(n.p,{children:"If you already have an up and running Daikoku instance, you can skip the following instructions and log in to your instance."}),(0,t.jsxs)(n.p,{children:["Let\u2019s start by ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/getdaikoku/",children:"downloading the latest Daikoku"})," and ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/firstrun/run",children:"run it"})]}),(0,t.jsx)(n.p,{children:"Once Daikoku is started you can log in to your brand new instance."}),(0,t.jsx)(n.h2,{id:"running-an-otoroshi-server",children:"Running an otoroshi server"}),(0,t.jsxs)(n.p,{children:["Otoroshi have his own manual to this part let's ",(0,t.jsx)(n.a,{href:"https://maif.github.io/otoroshi/manual/install/get-otoroshi.html",children:"get"})," as ",(0,t.jsx)(n.a,{href:"https://maif.github.io/otoroshi/manual/install/run-otoroshi.html",children:"run"})," your Otoroshi."]}),(0,t.jsx)(n.h2,{id:"create-an-authentication-module",children:"Create an authentication module"}),(0,t.jsxs)(n.p,{children:["Like the previous section, you can follow instructions in ",(0,t.jsx)(n.a,{href:"https://maif.github.io/otoroshi/manual/how-to-s/secure-app-with-auth0.html",children:"here"})," to create a new Auth plugin in Otoroshi.\nYou can find many possibility to secure Daikoku with Otoroshi in the documentation."]}),(0,t.jsx)(n.h2,{id:"expose-your-daikoku-by-otoroshi",children:"Expose your daikoku by Otoroshi"}),(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsx)(n.p,{children:"before exposing your daikoku instance with otoroshi, you need to setup exposing mode to Otoroshi."})}),(0,t.jsx)(n.p,{children:"You ultimately have to add some plugins along the route to make this work :"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Authentication, select the created Auth module in configuration to use it"}),"\n",(0,t.jsx)(n.li,{children:"Otoroshi info. token, you can configure the name of the header in which the authentication token will be passed and the secert to signe this token."}),"\n"]}),(0,t.jsx)(n.h2,{id:"create-an-authentication-configuration-1",children:"Create an Authentication configuration"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Go ahead and navigate to your Daikoku home page"}),"\n",(0,t.jsx)(n.li,{children:"Click on the your avatar on bottom left of the screen"}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"<your-tenant-name> settings"})]}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"Authentication"})," on left panel"]}),"\n",(0,t.jsxs)(n.li,{children:["You must be on ",(0,t.jsx)(n.code,{children:"<your-domain>/settings/settings/authentication"})]}),"\n",(0,t.jsxs)(n.li,{children:["This page show the settings for the current authentication mode (by default ",(0,t.jsx)(n.code,{children:"Local"}),")"]}),"\n",(0,t.jsxs)(n.li,{children:["Click on ",(0,t.jsx)(n.code,{children:"Otoroshi"})," at the top of the form"]}),"\n",(0,t.jsx)(n.li,{children:"Fill the header name previously setup"}),"\n",(0,t.jsx)(n.li,{children:"Fill the secret previously setup"}),"\n"]}),(0,t.jsx)(n.h2,{id:"testing-your-configuration-1",children:"Testing your configuration"}),(0,t.jsxs)(n.p,{children:["Disconnect from your instance\nThen click on the Login button (or navigate to ",(0,t.jsx)(n.code,{children:"<your-daikoku-domain>/login"}),")\nYou can now login with a user setup with the auth module."]}),(0,t.jsxs)(n.admonition,{type:"warning",children:[(0,t.jsxs)(n.p,{children:["To be daikoku admin, a user need to have a ",(0,t.jsx)(n.code,{children:"daikokuAdmin"})," property in his definition setup to true.\nYou can cerate a Daikoku admin by adding a metadata in the user definition in the Auth. plugin :"]}),(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-json",children:"{daikokuAdmin: true}\n"})})]})]}),(0,t.jsxs)(r.Z,{value:"oauth2",label:"OAuth2",default:!0,children:[(0,t.jsx)(n.admonition,{type:"info",children:(0,t.jsxs)(n.p,{children:["For this tutorial, we'll use ",(0,t.jsx)(n.a,{href:"https://manage.auth0.com",children:"Auth0"})]})}),(0,t.jsx)(n.h2,{id:"before-starting-2",children:"Before starting"}),(0,t.jsx)(n.p,{children:"If you already have an up and running Daikoku instance, you can skip the following instructions and log in to your instance."}),(0,t.jsxs)(n.p,{children:["Let\u2019s start by ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/getdaikoku/",children:"downloading the latest Daikoku"})," and ",(0,t.jsx)(n.a,{href:"/daikoku/docs/getstarted/firstrun/run",children:"run it"})]}),(0,t.jsx)(n.p,{children:"Once Daikoku is started you can log in to your brand new instance."}),(0,t.jsx)(n.h2,{id:"configure-an-auth0-client",children:"Configure an Auth0 client"}),(0,t.jsx)(n.p,{children:"The first step of this tutorial is to setup an Auth0 application with the information of the instance of our Otoroshi."}),(0,t.jsxs)(n.p,{children:["Navigate to ",(0,t.jsx)(n.a,{href:"https://manage.auth0.com",children:"https://manage.auth0.com"})," (create an account if it\u2019s not already done)."]}),(0,t.jsx)(n.p,{children:"Let\u2019s create an application when clicking on the Applications button on the sidebar. Then click on the Create application button on the top right."}),(0,t.jsxs)(n.p,{children:["Choose ",(0,t.jsx)(n.code,{children:"Regular Web Applications"})," as Application type\nThen set for example ",(0,t.jsx)(n.code,{children:"daikoku-client"})," as Name, and confirm the creation\nJump to the Settings tab\nScroll to the Application URLs section and add the following urls"]}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.a,{href:"http://localhost:9000/auth/oauth2/callback",children:"http://localhost:9000/auth/oauth2/callback"})," as Allowed Callback URLs"]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.a,{href:"http://localhost:9000",children:"http://localhost:9000"})," as Allowed Logout URLs"]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.a,{href:"http://localhost:9000",children:"http://localhost:9000"})," as Allowed Web Origins\nSave changes at the bottom of the page."]}),"\n"]}),(0,t.jsx)(n.p,{children:"Once done, we have a full setup, with a client ID and secret at the top of the page, which authorizes our Daikoku and redirects the user to the callback url when they log into Auth0."}),(0,t.jsxs)(n.admonition,{type:"warning",children:[(0,t.jsxs)(n.p,{children:["To be daikoku admin, a user need to have a ",(0,t.jsx)(n.code,{children:"daikokuAdmin"})," property in his definition setup to true. Daikoku get this information in the metadata of user."]}),(0,t.jsx)(n.p,{children:"In the Auth0 case, here are the steps to follow :"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["add app metadata to a user ",(0,t.jsx)(n.code,{children:"daikokuAdmin"})," with ",(0,t.jsx)(n.code,{children:"true"})," as value"]}),"\n",(0,t.jsx)(n.li,{children:"create a new custom action in auth0 :"}),"\n"]}),(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-js",children:"exports.onExecutePostLogin = async (event, api) => {\n  const { daikokuAdmin } = event.user.app_metadata;\n\n  if (event.authorization) {\n    api.idToken.setCustomClaim(`daikokuAdmin`, daikokuAdmin);\n  }\n};\n"})}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"deploy it"}),"\n"]})]}),(0,t.jsx)(n.h2,{id:"create-authentication-configuration",children:"Create authentication configuration"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Go ahead and navigate to your Daikoku home page"}),"\n",(0,t.jsx)(n.li,{children:"Click on the your avatar on bottom left of the screen"}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"<your-tenant-name> settings"})]}),"\n",(0,t.jsxs)(n.li,{children:["Then ",(0,t.jsx)(n.code,{children:"Authentication"})," on left panel"]}),"\n",(0,t.jsxs)(n.li,{children:["You must be on ",(0,t.jsx)(n.code,{children:"<your-domain>/settings/settings/authentication"})]}),"\n",(0,t.jsxs)(n.li,{children:["This page show the settings for the current authentication mode (by default ",(0,t.jsx)(n.code,{children:"Local"}),")"]}),"\n",(0,t.jsxs)(n.li,{children:["Click on ",(0,t.jsx)(n.code,{children:"OAuth2"})," at the top of the form"]}),"\n",(0,t.jsxs)(n.li,{children:["Enable ",(0,t.jsx)(n.code,{children:"Read profile from JWT token"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Token scope"})," with ",(0,t.jsx)(n.code,{children:"openid profile name email picture"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Client Id"})," with the client ID provided by Auth0"]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Client Secret"})," with the client secret provided by Auth0"]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Authorize URL"})," with ",(0,t.jsx)(n.code,{children:"https://<your-auth0-tenant>.eu.auth0.com/authorize"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Token URL"})," with ",(0,t.jsx)(n.code,{children:"https://<your-auth0-tenant>.eu.auth0.com/oauth/token"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Userinfo URL"})," with ",(0,t.jsx)(n.code,{children:"https://<your-auth0-tenant>.eu.auth0.com/userinfo"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Login URL"})," with ",(0,t.jsx)(n.code,{children:"https://<your-auth0-tenant>.eu.auth0.com/authorize"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Logout URL"})," with ",(0,t.jsx)(n.code,{children:"https://<your-auth0-tenant>.eu.auth0.com/oidc/logout?redirectTo=${redirect}&client_id=${clientId}"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Callback URL"})," with ",(0,t.jsx)(n.code,{children:"http://localhost:9000/auth/oauth2/callback"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Access token field name"})," with ",(0,t.jsx)(n.code,{children:"access_token"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Name field name"})," with ",(0,t.jsx)(n.code,{children:"name"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Email field name"})," with ",(0,t.jsx)(n.code,{children:"email"})]}),"\n",(0,t.jsxs)(n.li,{children:["Fill ",(0,t.jsx)(n.code,{children:"Picture field name"})," with ",(0,t.jsx)(n.code,{children:"picture"})]}),"\n"]}),(0,t.jsx)(n.p,{children:"Two optional fields can be filled :"}),(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.code,{children:"Email of Daikoku Admins"})," is a white list of Daikoku admins if you don't want use user metadata to setup admins"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.code,{children:"jwt verifier"})," can be use to verify the JWT token received by Daikoku. According to the selected algorithm, the validation form will change."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"mac + SHA"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"SHA Size: Word size for the SHA-2 hash function used"}),"\n",(0,t.jsx)(n.li,{children:"Hmac secret: used to verify the token"}),"\n",(0,t.jsx)(n.li,{children:"Base64 encoded secret: if enabled, the extracted token will be base64 decoded before it is verifier"}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"RSASSA-PKCS1 + SHA"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"SHA Size: Word size for the SHA-2 hash function used"}),"\n",(0,t.jsx)(n.li,{children:"Public key: the RSA public key"}),"\n",(0,t.jsx)(n.li,{children:"Private key: the RSA private key that can be empty if not used for JWT token signing"}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"JWK Set (only for verification)"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"URL: the JWK set URL where the public keys are exposed"}),"\n",(0,t.jsx)(n.li,{children:"HTTP call timeout: timeout for fetching the keyset"}),"\n",(0,t.jsx)(n.li,{children:"TTL: cache TTL for the keyset"}),"\n",(0,t.jsx)(n.li,{children:"HTTP Headers: the HTTP headers passed"}),"\n",(0,t.jsx)(n.li,{children:"Key type: type of the key searched in the jwks"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"testing-your-configuration-2",children:"Testing your configuration"}),"\n",(0,t.jsxs)(n.p,{children:["Disconnect from your instance\nThen click on the Login button (or navigate to ",(0,t.jsx)(n.code,{children:"<your-daikoku-domain>/login"}),")\nYou can now login with a user setup with the auth module."]}),"\n"]}),"\n"]})]})]})]})}function p(e={}){const{wrapper:n}={...(0,o.a)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(u,{...e})}):u(e)}},5162:(e,n,i)=>{i.d(n,{Z:()=>r});i(7294);var t=i(6905);const o={tabItem:"tabItem_Ymn6"};var a=i(5893);function r(e){let{children:n,hidden:i,className:r}=e;return(0,a.jsx)("div",{role:"tabpanel",className:(0,t.Z)(o.tabItem,r),hidden:i,children:n})}},4866:(e,n,i)=>{i.d(n,{Z:()=>v});var t=i(7294),o=i(6905),a=i(2466),r=i(6550),s=i(469),l=i(1980),c=i(7392),d=i(12);function h(e){return t.Children.toArray(e).filter((e=>"\n"!==e)).map((e=>{if(!e||(0,t.isValidElement)(e)&&function(e){const{props:n}=e;return!!n&&"object"==typeof n&&"value"in n}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}function u(e){const{values:n,children:i}=e;return(0,t.useMemo)((()=>{const e=n??function(e){return h(e).map((e=>{let{props:{value:n,label:i,attributes:t,default:o}}=e;return{value:n,label:i,attributes:t,default:o}}))}(i);return function(e){const n=(0,c.l)(e,((e,n)=>e.value===n.value));if(n.length>0)throw new Error(`Docusaurus error: Duplicate values "${n.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[n,i])}function p(e){let{value:n,tabValues:i}=e;return i.some((e=>e.value===n))}function x(e){let{queryString:n=!1,groupId:i}=e;const o=(0,r.k6)(),a=function(e){let{queryString:n=!1,groupId:i}=e;if("string"==typeof n)return n;if(!1===n)return null;if(!0===n&&!i)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return i??null}({queryString:n,groupId:i});return[(0,l._X)(a),(0,t.useCallback)((e=>{if(!a)return;const n=new URLSearchParams(o.location.search);n.set(a,e),o.replace({...o.location,search:n.toString()})}),[a,o])]}function m(e){const{defaultValue:n,queryString:i=!1,groupId:o}=e,a=u(e),[r,l]=(0,t.useState)((()=>function(e){let{defaultValue:n,tabValues:i}=e;if(0===i.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(n){if(!p({value:n,tabValues:i}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${n}" but none of its children has the corresponding value. Available values are: ${i.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return n}const t=i.find((e=>e.default))??i[0];if(!t)throw new Error("Unexpected error: 0 tabValues");return t.value}({defaultValue:n,tabValues:a}))),[c,h]=x({queryString:i,groupId:o}),[m,j]=function(e){let{groupId:n}=e;const i=function(e){return e?`docusaurus.tab.${e}`:null}(n),[o,a]=(0,d.Nk)(i);return[o,(0,t.useCallback)((e=>{i&&a.set(e)}),[i,a])]}({groupId:o}),f=(()=>{const e=c??m;return p({value:e,tabValues:a})?e:null})();(0,s.Z)((()=>{f&&l(f)}),[f]);return{selectedValue:r,selectValue:(0,t.useCallback)((e=>{if(!p({value:e,tabValues:a}))throw new Error(`Can't select invalid tab value=${e}`);l(e),h(e),j(e)}),[h,j,a]),tabValues:a}}var j=i(2389);const f={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};var g=i(5893);function k(e){let{className:n,block:i,selectedValue:t,selectValue:r,tabValues:s}=e;const l=[],{blockElementScrollPositionUntilNextRender:c}=(0,a.o5)(),d=e=>{const n=e.currentTarget,i=l.indexOf(n),o=s[i].value;o!==t&&(c(n),r(o))},h=e=>{let n=null;switch(e.key){case"Enter":d(e);break;case"ArrowRight":{const i=l.indexOf(e.currentTarget)+1;n=l[i]??l[0];break}case"ArrowLeft":{const i=l.indexOf(e.currentTarget)-1;n=l[i]??l[l.length-1];break}}n?.focus()};return(0,g.jsx)("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,o.Z)("tabs",{"tabs--block":i},n),children:s.map((e=>{let{value:n,label:i,attributes:a}=e;return(0,g.jsx)("li",{role:"tab",tabIndex:t===n?0:-1,"aria-selected":t===n,ref:e=>l.push(e),onKeyDown:h,onClick:d,...a,className:(0,o.Z)("tabs__item",f.tabItem,a?.className,{"tabs__item--active":t===n}),children:i??n},n)}))})}function b(e){let{lazy:n,children:i,selectedValue:o}=e;const a=(Array.isArray(i)?i:[i]).filter(Boolean);if(n){const e=a.find((e=>e.props.value===o));return e?(0,t.cloneElement)(e,{className:"margin-top--md"}):null}return(0,g.jsx)("div",{className:"margin-top--md",children:a.map(((e,n)=>(0,t.cloneElement)(e,{key:n,hidden:e.props.value!==o})))})}function y(e){const n=m(e);return(0,g.jsxs)("div",{className:(0,o.Z)("tabs-container",f.tabList),children:[(0,g.jsx)(k,{...e,...n}),(0,g.jsx)(b,{...e,...n})]})}function v(e){const n=(0,j.Z)();return(0,g.jsx)(y,{...e,children:h(e.children)},String(n))}},1151:(e,n,i)=>{i.d(n,{Z:()=>s,a:()=>r});var t=i(7294);const o={},a=t.createContext(o);function r(e){const n=t.useContext(a);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:r(e.components),t.createElement(a.Provider,{value:n},e.children)}}}]);