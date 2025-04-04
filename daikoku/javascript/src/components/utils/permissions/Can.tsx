import { ReactNode, useContext } from 'react';

import { toast } from 'sonner';
import { Option } from '..';
import { I18nContext } from '../../../contexts';
import { GlobalContext } from '../../../contexts/globalContext';
import {
  ITeamSimple,
  ITenant,
  IUserSimple,
  TeamPermission,
  TeamUser
} from '../../../types';
import { doNothing } from './actions';
import { TPermission, TPermissions, permissions } from './permissions';
import { tenant } from './subjects';

export const CanIDoAction = (
  user: IUserSimple,
  action: number,
  what: string,
  team?: ITeamSimple,
  isTenantAdmin?: boolean,
  whichOne?: any,
  currentTenant?: any
) => {
  if (what === tenant) {
    return (isTenantAdmin && whichOne._id === currentTenant._id) || user.isDaikokuAdmin;
  }
  // else if (what === api && !apiCreationPermitted)
  //   return false
  else {
    const realPerm: number = Option(team)
      .map((t) => t.users)
      .flatMap((users: TeamUser[]) => Option(users.find((u) => u.userId === user._id)))
      .map((userWithPermission: TeamUser) => userWithPermission.teamPermission)
      .map((ability: TeamPermission) => permissions[ability])
      .flatMap((perms: TPermissions) => Option(perms.find((p: any) => p.what === what)))
      .map((perm: TPermission) =>
        Option(perm.condition).fold(
          () => perm.action,
          (condition: (t) => boolean) => (condition(team) ? perm.action : doNothing)
        )
      )
      .fold(
        () => doNothing,
        (perm: number) => perm
      );

    return action <= realPerm || user.isDaikokuAdmin;
  }
};

export const CanIDoActionForOneOfTeams = (user: any, action: any, what: any, teams: any) => {
  return teams.some((team: any) => CanIDoAction(user, action, what, team, false));
};

export const Can = ({
  I,
  a,
  team,
  teams,
  dispatchError,
  children,
  orElse = <></>,
  whichOne,
}: {
  I: number;
  a: string;
  team?: ITeamSimple;
  teams?: Array<ITeamSimple>;
  dispatchError?: boolean;
  children: ReactNode;
  orElse?: JSX.Element;
  whichOne?: ITenant;
}): JSX.Element => {
  const { connectedUser, isTenantAdmin, tenant } = useContext(GlobalContext);
  const { translate } = useContext(I18nContext);

  const authorized = teams
    ? CanIDoActionForOneOfTeams(connectedUser, I, a, teams)
    : CanIDoAction(connectedUser, I, a, team, isTenantAdmin, whichOne || tenant, tenant);
  if (!authorized) {
    if (dispatchError) {
      toast.error(translate('Unauthorized'))
      return orElse; //FIXME [#609]
    }
    return orElse;
  }

  return <>{children}</>;
};