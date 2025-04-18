import sortBy from 'lodash/sortBy';
import { useContext, useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { toast } from 'sonner';

import { ModalContext, useDaikokuBackOffice } from '../../../contexts';
import { I18nContext } from '../../../contexts/i18n-context';
import { GlobalContext } from '../../../contexts/globalContext';
import * as Services from '../../../services';
import { IUserSimple, isError } from '../../../types';
import { AvatarWithAction, Can, PaginatedComponent, daikoku, manage } from '../../utils';
import { constraints, type } from '@maif/react-forms';

export const UserList = () => {
  const { connectedUser } = useContext(GlobalContext);
  useDaikokuBackOffice();

  const { alert, confirm, openFormModal } = useContext(ModalContext);

  const [users, setUsers] = useState<Array<IUserSimple>>([]);
  const [search, setSearch] = useState<string>();
  const navigate = useNavigate();

  useEffect(() => {
    updateUsers();
  }, []);

  const { translate } = useContext(I18nContext);

  const updateUsers = () => {
    Services.fetchAllUsers()
      .then((users) => {
        if (!isError(users)) {
          setUsers(users)
        }
      });
  };

  const removeUser = (user: IUserSimple) => {
    openFormModal({
      title: translate('Confirm'),
      description: <div className="alert alert-danger" role="alert">
        <h4 className="alert-heading">{translate('Warning')}</h4>
        <p>{translate("delete.user.confirm.modal.description.1")}</p>
        <ul>
          <li>{translate("delete.user.confirm.modal.description.2")}</li>
          <li>{translate("delete.user.confirm.modal.description.3")}</li>
          <li>{translate("delete.user.confirm.modal.description.4")}</li>
        </ul>
      </div>,
      schema: {
        confirm: {
          type: type.string,
          label: translate({ key: 'delete.item.confirm.modal.confirm.label', replacements: [user.name] }),
          constraints: [
            constraints.oneOf(
              [user.name],
              translate({ key: 'constraints.type.api.name', replacements: [user.name] })
            ),
          ],
        },
      },
      onSubmit: () => Services.deleteUserById(user._id)
        .then((r) => {
          if (isError(r)) {
            toast.error(r.error)
          } else {
            toast.success(translate({ key: 'remove.user.success', replacements: [user.name] }));
            updateUsers();
          }
        }),
      actionLabel: translate('Confirm')
    })
  };

  const toggleAdmin = (member: IUserSimple) => {
    if (member._id === connectedUser._id) {
      alert({ message: translate('toggle.admin.alert') });
    } else {
      Services.setAdminStatus(member, !member.isDaikokuAdmin).then(() => updateUsers());
    }
  };

  const filteredUsers = search
    ? users.filter(({ name, email }) => [name, email].some((item) => item.toLowerCase().includes(search)))
    : users;
  return (<Can I={manage} a={daikoku} dispatchError>
    <div className="row">
      <div className="col">
        <div className="d-flex justify-content-between align-items-center">
          <h1>
            {translate('Users')}
          </h1>
          <div className="col-5">
            <input placeholder={translate('Find a user')} className="form-control" onChange={(e) => {
              setSearch(e.target.value);
            }} />
          </div>
        </div>
        <PaginatedComponent
          items={sortBy(filteredUsers, [(user) => user.name.toLowerCase()])}
          count={15}
          formatter={(user: IUserSimple) => {
            return (<AvatarWithAction
              key={user._id}
              avatar={user.picture}
              infos={<>
                {user.isDaikokuAdmin && (<i className="fas fa-shield-alt" style={{ marginRight: '10px' }} />)}
                <span className="team__name text-truncate">{user.name}</span>
              </>} actions={[
                {
                  action: () => removeUser(user),
                  iconClass: 'fas fa-trash delete-icon',
                  tooltip: translate('Remove user'),
                },
                {
                  redirect: () => navigate(`/settings/users/${user._humanReadableId}`),
                  iconClass: 'fas fa-pen',
                  tooltip: translate('Edit user'),
                },
                {
                  link: `/api/admin/users/${user._id}/_impersonate`,
                  iconClass: 'fas fa-user-ninja',
                  tooltip: translate('Impersonate this user'),
                },
                {
                  action: () => toggleAdmin(user),
                  iconClass: `fas fa-shield-alt ${user.isDaikokuAdmin ? 'admin-active' : 'admin-inactive'}`,
                  tooltip: translate('toggle admin status'),
                },
              ]} />);
          }} />
      </div>
    </div>
  </Can>);
};
