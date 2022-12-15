import React, { useState, useEffect, useContext } from 'react';

import { TeamEditForm } from '../../backoffice/teams/TeamEdit';
import * as Services from '../../../services';
import { I18nContext } from '../../../core';
import { useNavigate } from 'react-router-dom';

type Props = {
    closeModal: (...args: any[]) => any;
    team: any;
};

export const TeamCreationModal = (props: Props) => {
  const [team, setTeam] = useState(props.team);
  const [created, setCreated] = useState(false);
  const [error, setError] = useState(undefined);
  const navigate = useNavigate();

    const { translate, Translation } = useContext(I18nContext);

  useEffect(() => {
    if (created) {
      props.closeModal();
      navigate(`/${team._humanReadableId}/settings/members`);
    }
  }, [created]);

  return (
        <div className="modal-content">
            <div className="modal-header">
                <h5 className="modal-title">
                    <Translation i18nkey="New team">New team</Translation>
        </h5>
                <button type="button" className="btn-close" aria-label="Close" onClick={props.closeModal} />
      </div>
            <div className="modal-body">
        {!!error && (
                    <div className="alert alert-danger" role="alert">
            {translate(error)}
          </div>
        )}
                <TeamEditForm team={team} updateTeam={setTeam} />
      </div>
            <div className="modal-footer">
                <button type="button" className="btn btn-outline-danger" onClick={props.closeModal}>
                    <Translation i18nkey="Close">Close</Translation>
        </button>
        {!created && (
                    <button
            type="button"
            className="btn btn-outline-success"
            onClick={() =>
              Services.createTeam(team)
                .then((r) => {
                  if (r.error) {
                    return Promise.reject(r);
                  } else {
                    return r;
                  }
                })
                .then((newteam) => setTeam(newteam))
                .then(() => setCreated(true))
                .catch((e) => {
                  setError(e.error);
                })
            }
          >
                        <Translation i18nkey="Create">Create</Translation>
          </button>
        )}
      </div>
    </div>
  );
};