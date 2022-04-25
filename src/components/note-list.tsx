import React from 'react'
import { Link, navigate } from 'gatsby'
import '../styles/note.css'
import moment from 'moment';

export default function NoteList({ notes }) {
  return (
    <div className="block note-cards note-list">
      {notes.map((data, index) => (
        <div
          className="note-area box-feed"
          key={index}
          role="button"
          tabIndex={index}
          onClick={() => navigate(data.node.fields.slug)}
          onKeyDown={event => { if (event.keyCode === 13) navigate(data.node.fields.slug) }}
        >
          <h4 className="note-title">
            <Link to={data.node.fields.slug}>{data.node.fields.title}</Link>
          </h4>
          <p className="note-excerpt">
            {data.node.fields?.excerpt || data.node.excerpt}
          </p>

          {data.node.frontmatter.tags?.length ? (
            <p className="note-tag-list">
              Tagged with:{' '}
              {data.node.frontmatter.tags.map((tag, index) => (
                <span key={index}>
                  <Link to={`/tags/${tag}`}>#{tag}</Link>
                  {index < data.node.frontmatter.tags.length - 1 ? ', ' : ''}
                </span>
              ))}
            </p>
          ) : null}
          <p className="note-date">
            Published on {moment(new Date(data.node.fields.date)).fromNow()}
          </p>
        </div>
      ))}
    </div>
  )
}
