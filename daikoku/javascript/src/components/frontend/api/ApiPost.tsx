import React, { useState, useEffect, useContext } from 'react';
import { I18nContext } from '../../../core';
import * as Services from '../../../services/index';
import { converter } from '../../../services/showdown';

type Pagination = {
  limit: number,
  offset: number,
  total: number
}

export function ApiPost({
  api,
  versionId
}: any) {
  const [posts, setPosts] = useState([]);

  const { translate } = useContext(I18nContext);

  const [pagination, setPagination] = useState<Pagination>({
    limit: 1,
    offset: 0,
    total: 0,
  });

  useEffect(() => {
    Services.getAPIPosts(api._humanReadableId, versionId, pagination.offset, pagination.limit).then(
      (data) => {
        setPosts(
          [...posts, ...data.posts].reduce((acc, post) => {
            if (!acc.find((p: any) => p._id === post._id)) acc.push(post);
            return acc;
          }, [])
        );
        setPagination({
          ...pagination,
          total: data.total,
        });
      }
    );
  }, [pagination.offset, pagination.limit]);

  function formatDate(lastModificationAt: any) {
    const date = new Date(lastModificationAt);
    return new Intl.DateTimeFormat('fr-FR', { dateStyle: 'full', timeStyle: 'short' }).format(date);
  }

  return (<div className="container-fluid">
    {posts.map((post, i) => (<div key={i} className="jumbotron">
      <div className="d-flex justify-content-between align-items-center">
        <h1>{(post as any).title}</h1>
        <span>{formatDate((post as any).lastModificationAt)}</span>
      </div>
      <div className="api-post" dangerouslySetInnerHTML={{ __html: converter.makeHtml((post as any).content) }} />
    </div>))}
    {posts.length < pagination.total && (<button className="btn btn-outline-info" onClick={() => {
      setPagination({
        limit: 1,
        offset: pagination.offset + 1,
        total:pagination.total
      });
    }}>
      {translate('Load older posts')}
    </button>)}
  </div>);
}