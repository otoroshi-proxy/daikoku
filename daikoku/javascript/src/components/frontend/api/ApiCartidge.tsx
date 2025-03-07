import React, { useContext } from 'react';
import moment from 'moment';

import { Link } from 'react-router-dom';
import { ActionWithTeamSelector, Can, manage, apikey, CanIDoAction } from '../../utils';
import { I18nContext } from '../../../contexts';

const Separator = () => <hr className="hr-apidescription" />;

export function ApiCartidge(props: any) {
  const { translate, Translation } = useContext(I18nContext);

  const { api, ownerTeam } = props;
  const defaultPlan = api.possibleUsagePlans.filter((p: any) => p._id === api.defaultUsagePlan)[0];
  const pricing = defaultPlan ? defaultPlan.type : 'None';

  const subscribingTeams = props.myTeams
    // .filter((t) => t.type !== 'Admin')
    .filter((team: any) => props.subscriptions.some((sub: any) => sub.team === team._id));

  return (
    <div className="d-flex col-12 col-sm-3 col-md-2 text-muted flex-column p-3 album">
      <span>
        <Translation i18nkey="API by">API by</Translation>
      </span>
      <small className="word-break">
        <Link to={`/${ownerTeam._humanReadableId}`}>{ownerTeam.name}</Link>
      </small>
      <div>
        <button className="btn btn-xs btn-outline-primary" onClick={props.openContactModal}>
          <i className="far fa-envelope me-1" />
          <Translation i18nkey="Contact us">Contact us</Translation>
        </button>
      </div>
      <Separator />
      <span>
        <Translation i18nkey="Version">Version</Translation>
        <span className="badge bg-info ms-1">{api.currentVersion}</span>
      </span>
      <Separator />
      <span>
        <Translation i18nkey="Supported versions">Supported versions</Translation>
        {(api.supportedVersions || []).map((v: any, idx: any) => (
          <span key={idx} className="badge bg-info ms-1">
            {v}
          </span>
        ))}
      </span>
      <Separator />
      <span>
        <Translation i18nkey="Tags">Tags</Translation>
        {(api.tags || []).map((a: any, idx: any) => (
          <span key={idx} className="badge badge-custom ms-1">
            {a}
          </span>
        ))}
      </span>
      <Separator />
      <span>
        <Translation i18nkey="Visibility">Visibility</Translation>
        <span className={`badge ms-1 ${api.visibility === 'Public' ? 'bg-success' : 'bg-danger'}`}>
          {translate(api.visibility)}
        </span>
      </span>
      <Separator />
      {defaultPlan && (
        <>
          <span>
            <Translation i18nkey="Default plan">Default plan</Translation>
            <span className="badge bg-primary word-break ms-1" style={{ whiteSpace: 'normal' }}>
              {defaultPlan.customName || translate(pricing)}
            </span>
          </span>
          <Separator />
        </>
      )}
      <span>
        <Translation i18nkey="Last modification">Last modification</Translation>
      </span>
      <small>{moment(api.lastUpdate).format(translate('moment.date.format.short'))}</small>

      {!!subscribingTeams.length && (
        <Can I={manage} a={apikey} teams={subscribingTeams}>
          <ActionWithTeamSelector
            title={translate('teamapi.select.title')}
            teams={subscribingTeams.filter((t: any) => CanIDoAction(props.connectedUser, manage, apikey, t)
            )}
            action={(teams) => {
              props.redirectToApiKeysPage(props.myTeams.find((t: any) => teams.includes(t._id)));
            }}
            actionLabel={translate('View your api keys')}
            allTeamSelector={false}
          >
            <button className="btn btn-sm btn-outline-primary mt-2">
              <Translation i18nkey="View your api keys">View your api keys</Translation>
            </button>
          </ActionWithTeamSelector>
        </Can>
      )}

      {defaultPlan && !defaultPlan.otoroshiTarget && (
        <small className="mt-5">
          <Translation i18nkey="api not linked">
            This api is not linked to an actual Otoroshi service yet
          </Translation>
        </small>
      )}
    </div>
  );
}
