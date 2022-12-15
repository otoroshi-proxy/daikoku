import React, { useContext, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { I18nContext } from '../../../contexts/i18n-context';

export const SessionModal = ({
  session
}: any) => {
  const { translate } = useContext(I18nContext);
  const navigate = useNavigate();

  useEffect(() => {
    const sessionExpires = translate('session.expire.info');
    const extendMySession = translate('session.extend');

    if (session) {
      let reloadTimeout: any = null;

      const extendSession = (close: any) => {
        return fetch('/api/session/_renew', {
          method: 'POST',
          credentials: 'include',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
          },
          body: '',
        })
          .then((r) => r.json())
          .then((sess) => {
            clearTimeout(reloadTimeout);
            setupTimeouts(sess);
            close();
          });
      };

      const setupTimeouts = (_session: any) => {
        const firstPing = _session.expires - Date.now() - 2 * 60 * 1000;
        const secondPing = _session.expires - Date.now() + 2000;
        setTimeout(() => {
          alert({
            message: (close: any) => <div style={{ width: '100%' }}>
              <p>{sessionExpires}</p>
              <div
                style={{
                  width: '100%',
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                }}
              >
                <button
                  type="button"
                  className="btn btn-success"
                  onClick={() => extendSession(close)}
                >
                  {extendMySession}
                </button>
              </div>
            </div>,
            title: translate('modal.session.expire.title')
          });
        }, firstPing);
        reloadTimeout = setTimeout(() => { navigate('/') }, secondPing);
      };
      setupTimeouts(session);
    }
  }, []);

  return <></>;
};