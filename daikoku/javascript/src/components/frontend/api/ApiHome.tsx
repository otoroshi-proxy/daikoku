import { getApolloContext } from '@apollo/client';
import hljs from 'highlight.js';
import { useContext, useEffect, useState } from 'react';
import Navigation from 'react-feather/dist/icons/navigation';
import { useMatch, useNavigate, useParams } from 'react-router-dom';
import Select from 'react-select';
import { toast } from 'sonner';

import { ApiDocumentation, ApiIssue, ApiPost, ApiPricing, ApiRedoc, ApiSwagger } from '.';
import { I18nContext, ModalContext, useApiFrontOffice } from '../../../contexts';
import * as Services from '../../../services';
import { converter } from '../../../services/showdown';
import { IApi, ISubscription, ISubscriptionDemand, ITeamSimple, IUsagePlan, isError } from '../../../types';
import { ActionWithTeamSelector, Can, CanIDoAction, Option, apikey, manage } from '../../utils';
import { formatPlanType } from '../../utils/formatters';
import StarsButton from './StarsButton';

import classNames from 'classnames';
import 'highlight.js/styles/monokai.css';
import { GlobalContext } from '../../../contexts/globalContext';
import { CmsViewer } from '../CmsViewer';
import { SimpleApiKeyCard } from '../../backoffice/apikeys/TeamApiKeysForApi';

(window as any).hljs = hljs;

export const ApiDescription = ({
  api
}: { api: IApi }) => {
  useEffect(() => {
    (window as any).$('pre code').each((i: any, block: any) => {
      hljs.highlightElement(block);
    });
  }, []);

  return (
    <div className="d-flex col flex-column p-3 section">
      <div
        className="api-description"
        dangerouslySetInnerHTML={{ __html: converter.makeHtml(api.description) }}
      />
    </div>
  );
};

type Version = {
  label: string,
  value: string
}

export const ApiHeader = ({
  api,
  ownerTeam,
  connectedUser,
  toggleStar,
  tab
}: any) => {
  const navigate = useNavigate();
  const params = useParams();

  const [versions, setApiVersions] = useState<Array<Version>>([]);

  useEffect(() => {
    Services.getAllApiVersions(ownerTeam._id, params.apiId! || params.apiGroupId!)
      .then((versions) =>
        setApiVersions(versions.map((v) => ({
          label: v,
          value: v
        })))
      );
  }, []);

  if (api.header) {
    const apiHeader = api.header
      .replace('{{title}}', api.name)
      .replace('{{description}}', api.smallDescription);

    return (
      <section className="api__header col-12 mb-4">
        <div
          className="api-description"
          dangerouslySetInnerHTML={{ __html: converter.makeHtml(apiHeader) }}
        />
      </section>
    );
  } else {
    return (
      <section className="api__header col-12 mb-4 p-3">
        <div className="container-fluid">
          <h1 className="jumbotron-heading" style={{ position: 'relative' }}>
            {api.name}
            <div
              style={{ position: 'absolute', right: 0, bottom: 0 }}
              className="d-flex align-items-center"
            >
              {versions.length > 1 && tab !== 'issues' && (
                <div style={{ minWidth: '125px', fontSize: 'initial' }}>
                  <Select
                    name="versions-selector"
                    value={{ label: params.versionId, value: params.versionId }}
                    options={versions}
                    onChange={(e) =>
                      navigate(`/${params.teamId}/${params.apiId}/${e?.value}/${tab}`)
                    }
                    classNamePrefix="reactSelect"
                    className="me-2"
                    menuPlacement="auto"
                    menuPosition="fixed"
                  />
                </div>
              )}
              <StarsButton
                stars={api.stars}
                starred={connectedUser.starredApis.includes(api._id)}
                toggleStar={toggleStar}
              />
            </div>
          </h1>
          <p className="lead">{api.smallDescription}</p>
        </div>
      </section>
    );
  }
};

type ApiHomeProps = {
  groupView?: boolean
}
export const ApiHome = ({
  groupView
}: ApiHomeProps) => {
  const [api, setApi] = useState<IApi>();
  const [subscriptions, setSubscriptions] = useState<Array<ISubscription>>([]);
  const [pendingSubscriptions, setPendingSubscriptions] = useState<Array<ISubscriptionDemand>>([]);
  const [ownerTeam, setOwnerTeam] = useState<ITeamSimple>();
  const [myTeams, setMyTeams] = useState<Array<any>>([]);
  const [showAccessModal, setAccessModalError] = useState<any>();
  const [showGuestModal, setGuestModal] = useState(false);

  const { connectedUser, tenant, reloadContext } = useContext(GlobalContext);
  const { openCustomModal, openRightPanel } = useContext(ModalContext);

  const navigate = useNavigate();
  const defaultParams = useParams();
  const apiGroupMatch = useMatch('/:teamId/apigroups/:apiGroupId/apis/:apiId/:versionId/:tab*');
  const params = Option(apiGroupMatch)
    .map((match: any) => match.params)
    .getOrElse(defaultParams);

  const { translate, Translation } = useContext(I18nContext);
  const { openLoginOrRegisterModal } = useContext(ModalContext);
  const { client } = useContext(getApolloContext());

  const { addMenu } = groupView ? { addMenu: () => { } } : useApiFrontOffice(api, ownerTeam);

  useEffect(() => {
    updateSubscriptions(params.apiId);
  }, [params.apiId, params.versionId]);

  useEffect(() => {
    if (api) {
      Services.team(api.team)
        .then((ownerTeam) => {
          if (!isError(ownerTeam)) {
            setOwnerTeam(ownerTeam)
          }
        });
    }
  }, [api, params.versionId]);

  useEffect(() => {
    if (myTeams && subscriptions && !groupView) {
      const subscribingTeams = myTeams
        .filter((team) => subscriptions.some((sub) => sub.team === team._id));
      const viewApiKeyLink = (
        <Can I={manage} a={apikey} teams={subscribingTeams}>
          <ActionWithTeamSelector
            title={translate('teamapi.select.title')}
            teams={subscribingTeams.filter((t) => CanIDoAction(connectedUser, manage, apikey, t))}
            action={(teams) => {
              const team = myTeams.find((t) => teams.includes(t._id));
              if (!team) {
                return;
              }
              navigate(
                `/${team._humanReadableId}/settings/apikeys/${api?._humanReadableId}/${api?.currentVersion}`
              );
            }}
            actionLabel={translate('View your api keys')}
            allTeamSelector={false}
          >
            <span className="block__entry__link">
              <Translation i18nkey="View your api keys">View your api keys</Translation>
            </span>
          </ActionWithTeamSelector>
        </Can>
      );

      addMenu({
        blocks: {
          actions: { links: { viewApiKey: { label: 'view apikey', component: viewApiKeyLink } } },
        },
      });
    }
  }, [subscriptions, myTeams]);

  const updateSubscriptions = (apiId: string) => {
    //FIXME: handle case if appolo client is not setted
    if (!client) {
      return;
    }
    Promise.all([
      Services.getVisibleApi(apiId, params.versionId),
      Services.getMySubscriptions(apiId, params.versionId),
      client.query({
        query: Services.graphql.myTeams,
      }),
    ]).then(
      ([
        api,
        { subscriptions, requests },
        {
          data: { myTeams },
        },
      ]) => {
        if (isError(api)) {
          toast.error(api.error) //FIXME [#609] better error management
        } else {
          setApi(api);
          setSubscriptions(subscriptions);
          setPendingSubscriptions(requests);
          setMyTeams(
            myTeams.map((team) => ({
              ...team,

              users: team.users.map((us) => ({
                ...us,
                ...us.user
              }))
            }))
          );
        }
      }
    );
  };

  const askForApikeys = ({ team, plan, apiKey, motivation }: { team: string, plan: IUsagePlan, apiKey?: ISubscription, motivation?: object }) => {
    const planName = formatPlanType(plan, translate);

    if (api) {
      return (
        apiKey
          ? Services.extendApiKey(api._id, apiKey._id, team, plan._id, motivation)
          : Services.askForApiKey(api._id, team, plan._id, motivation)
      ).then((result) => {

        if (isError(result)) {
          return toast.error(result.error);
        } else if (Services.isCheckoutUrl(result)) {
          window.location.href = result.checkoutUrl
        } else if (Services.isCreationDone(result)) {
          openRightPanel({
            title: translate('api.pricing.created.subscription.panel.title'),
            content: <SimpleApiKeyCard
              api={api!}
              plan={plan!}
              apiTeam={ownerTeam!}
              subscription={result.subscription}
            />
          })
        } else if (result.creation === 'waiting') {
          const teamName = myTeams.find((t) => t._id === team)!.name;
          return toast.info(translate({ key: 'subscription.plan.waiting', replacements: [planName, teamName] }));
        }

      })
        .then(() => updateSubscriptions(api._id));
    } else {
      return Promise.reject(false)
    }
  };

  const toggleStar = () => {
    if (api) {
      Services.toggleStar(api._id)
        .then(() => reloadContext());
    }
  };

  if (showGuestModal) {
    openLoginOrRegisterModal({
      tenant,
      showOnlyMessage: true,
      message: translate('guest_user_not_allowed')
    })
  }

  if (showAccessModal) {
    const teams = showAccessModal.api.myTeams.filter((t: any) => t.type !== 'Admin');
    const pendingTeams = showAccessModal.api.authorizations
      .filter((auth: any) => auth.pending)
      .map((auth: any) => auth.team);
    const authorizedTeams = showAccessModal.api.authorizations
      .filter((auth: any) => auth.authorized)
      .map((auth: any) => auth.team);

    return (<div className="mx-auto mt-3 d-flex flex-column justify-content-center">
      <h1 style={{ margin: 0 }}>{translate(showAccessModal.error)}</h1>
      {(teams.length === 1 &&
        (pendingTeams.includes(teams[0]._id) || authorizedTeams.includes(teams[0]._id))) ||
        showAccessModal.api.authorizations.every((auth: any) => auth.pending && !auth.authorized) ? (<>
          <h2 className="text-center my-3">{translate('request_already_pending')}</h2>
          <button className="btn btn-outline-info mx-auto" style={{ width: 'fit-content' }} onClick={() => navigate(-1)}>
            {translate('go_back')}
          </button>
        </>) : (<>
          <span className="text-center my-3">{translate('request_api_access')}</span>
          <ActionWithTeamSelector
            title={translate("api.access.modal.title")}
            description={translate({ key: 'api.access.request', replacements: [params.apIid] })}
            pendingTeams={pendingTeams}
            acceptedTeams={authorizedTeams}
            teams={teams}
            actionLabel={translate('Ask access to API')}
            action={(teams) => {
              Services.askForApiAccess(teams, showAccessModal.api._id).then((_) => {
                toast.info(translate({ key: 'ask.api.access.info', replacements: showAccessModal.api.name }));
                updateSubscriptions(showAccessModal.api._id);
              });
            }}>
            <button className="btn btn-outline-success mx-auto" style={{ width: 'fit-content' }}>
              {translate({ key: 'notif.api.access', replacements: [params.apiId] })}
            </button>
          </ActionWithTeamSelector>
        </>)}
    </div>);
  }

  if (!api || !ownerTeam) {
    return null;
  }
  const teamId = params.teamId;

  document.title = `${tenant.title} - ${api ? api.name : 'API'}`;

  return (<main role="main">

    {api.customHeaderCmsPage ?
      <CmsViewer pageId={api.customHeaderCmsPage} fields={{ api }} /> :
      <ApiHeader api={api} ownerTeam={ownerTeam} connectedUser={connectedUser} toggleStar={toggleStar} tab={params.tab} />}

    <div className="album py-2 me-4 min-vh-100">
      <div className={classNames({
        'container-fluid': params.tab === 'swagger',
        container: params.tab !== 'swagger'
      })}>
        <div className="row pt-3">
          {params.tab === 'description' &&
            (api.descriptionCmsPage ? <CmsViewer pageId={api.descriptionCmsPage} fields={{ api }} /> : <ApiDescription api={api} />)}
          {params.tab === 'pricing' && (<ApiPricing api={api} myTeams={myTeams} ownerTeam={ownerTeam} subscriptions={subscriptions} askForApikeys={askForApikeys} inProgressDemands={pendingSubscriptions} />)}
          {params.tab === 'documentation' && <ApiDocumentation api={api} documentation={api.documentation} getDocPage={(pageId) => Services.getApiDocPage(api._id, pageId)} />}
          {params.tab === 'testing' && (<ApiSwagger
            _id={api._id}
            testing={api.testing}
            swagger={api.swagger}
            swaggerUrl={`/api/teams/${params.teamId}/apis/${params.apiId}/${params.versionId}/swagger`}
            callUrl={`/api/teams/${ownerTeam._id}/testing/${api._id}/call`}
          />)}
          {params.tab === 'swagger' && (<ApiRedoc
            swaggerUrl={`/api/teams/${api.team}/apis/${api._id}/${api.currentVersion}/swagger`} swaggerConf={api.swagger} />)}
          {params.tab === 'news' && (<ApiPost api={api} ownerTeam={ownerTeam} versionId={params.versionId} />)}
          {(params.tab === 'issues' || params.tab === 'labels') && (<ApiIssue api={api} onChange={(editedApi: any) => setApi(editedApi)} ownerTeam={ownerTeam} connectedUser={connectedUser} />)}
        </div>
      </div>
    </div>
  </main>);
};
