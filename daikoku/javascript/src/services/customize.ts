import { toast } from 'sonner';

export function customizeFetch(store: any) {
  let willRedirect = false;
  (window as any).old_fetch = window.fetch;
  window.fetch = (...args) => {
    // const dispatchError = (response: any) =>
    //   response.json()
    //     .then((error: any) => {
    //       store.dispatch({
    //         type: SET_ERROR,
    //         error: { status: response.status, message: error.error, args, response: error },
    //       });
    //       return Promise.reject(error);
    //     });

    const query = new URLSearchParams(window.location.search);
    const url = args[0];

    let newUrl = url;
    const sessionId = query.get('sessionId');
    if (!!sessionId && (url as any).indexOf('?') > -1) {
      newUrl = newUrl + '&sessionId=' + sessionId;
    }
    if (!!sessionId && (url as any).indexOf('?') < 0) {
      newUrl = newUrl + '?sessionId=' + sessionId;
    }
    let newArgs = [...args];
    newArgs.shift();
    newArgs = [newUrl, ...newArgs];

    return (window as any).old_fetch(...newArgs).then((r: any) => {
      const status = r.status;
      if (r.redirected && r.url.indexOf('/auth/') > -1) {
        if (willRedirect === false) {
          willRedirect = true;
          // redirect();
        }
      } else if (status > 199 && status < 300) {
        // nothing to do yet
      } else if (status > 299 && status < 400) {
        // nothing to do yet
      } else if (status === 409) {
        return r.json().then((error) => toast.error(error.message));
      } else if (status === 404) {
        // nothing to do yet
      } else if (status >= 500 && status < 600) {
        toast.error(r.error); //TODO [#609]
        return r;
      } else {
        // nothing to do yet
      }
      return r;
    });
  };
}
