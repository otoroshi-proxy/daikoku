import React, { useContext, useEffect, useState } from 'react';
import { connect } from 'react-redux';
import Pagination from 'react-paginate';
import { useNavigate } from 'react-router-dom';
import * as Services from '../../../services';

import { TeamCard } from '.';
import { updateTeamPromise } from '../../../core';
import { I18nContext } from '../../../contexts/i18n-context';

function TeamChooserComponent(props: any) {
    const { translate, Translation } = useContext(I18nContext);
  const navigate = useNavigate();

  const [state, setState] = useState({
    teams: [],
    searched: '',
    offset: 0,
    pageNumber: 10,
    selectedPage: 0,
  });

  useEffect(() => {
    Services.allJoinableTeams().then((teams) => setState({ ...state, teams }));
  }, []);

  const askToJoin = (e: any, team: any) => {
    Services.askToJoinTeam(team._id)
      .then(() => Services.allJoinableTeams())
      .then((teams) => setState({ ...state, teams }));
  };

  const redirectToTeamSettings = (team: any) => {
    props.updateTeam(team).then(() => navigate(`/${team._humanReadableId}/settings`));
  };

  const handlePageClick = (data: any) => {
    setState({ ...state, offset: data.selected * state.pageNumber, selectedPage: data.selected });
  };

  const teams = state.teams;
  const searched = state.searched.trim().toLowerCase();
  const filteredTeams =
    searched === ''
      ? teams
      : teams.filter((team) => {
          if ((team as any).name.toLowerCase().indexOf(searched) > -1) {
            return true;
          } else return (team as any).description.toLowerCase().indexOf(searched) > -1;
        });
  const paginateTeams = filteredTeams.slice(state.offset, state.offset + state.pageNumber);

    return (<main role="main">
            <section className="organisation__header col-12 mb-4 p-3">
                <div className="container">
                    <div className="row text-center">
                        <div className="col-sm-4">
                            <img className="organisation__avatar" src={props.tenant ? props.tenant.logo : '/assets/images/daikoku.svg'} alt="avatar"/>
            </div>
                        <div className="col-sm-8 d-flex flex-column justify-content-center">
                            <h1 className="jumbotron-heading">
                                <Translation i18nkey="All teams">All teams</Translation>
              </h1>
            </div>
          </div>
        </div>
      </section>
            <section className="container">
                <div className="row mb-2">
                    <div className="col-12 col-sm mb-2">
                        <input type="text" className="form-control" placeholder={translate('Search a team')} aria-label="Search a team" value={state.searched} onChange={(e) => setState({ ...state, searched: e.target.value })}/>
          </div>
        </div>
                <div className="row">
                    <div className="d-flex col flex-column p-3">
                        {paginateTeams.map((team) => (<TeamCard key={(team as any)._id} user={props.connectedUser} team={team} askToJoin={(e) => askToJoin(e, team)} redirectToTeamPage={() => navigate(`/${(team as any)._humanReadableId}`)} redirectToTeamSettings={() => redirectToTeamSettings(team)}/>))}
                        <div className="apis__pagination">
                            <Pagination previousLabel={translate('Previous')} nextLabel={translate('Next')} breakLabel="..." breakClassName={'break'} pageCount={Math.ceil(filteredTeams.length / state.pageNumber)} marginPagesDisplayed={1} pageRangeDisplayed={5} onPageChange={handlePageClick} containerClassName={'pagination'} pageClassName={'page-selector'} forcePage={state.selectedPage} activeClassName={'active'}/>
            </div>
          </div>
        </div>
      </section>
    </main>);
}

const mapStateToProps = (state: any) => ({
  ...state.context
});

const mapDispatchToProps = {
  updateTeam: (team: any) => updateTeamPromise(team),
};

export const TeamChooser = connect(mapStateToProps, mapDispatchToProps)(TeamChooserComponent);