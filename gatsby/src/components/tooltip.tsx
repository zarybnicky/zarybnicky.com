import React from 'react'
import { Link } from 'gatsby'
import Tippy from '@tippyjs/react'
import 'tippy.js/dist/tippy.css'
import '../styles/tooltip.css'

export default function Tooltip({ children, content }) {
  return (
    <Tippy
      content={<div dangerouslySetInnerHTML={{ __html: content }} />}
      interactive="true" allowHTML="true" placement="bottom-start" delay="300"
    >
      {children}
    </Tippy>
  );
}

// This is to disable the tooltips with the tooltip.
const DefaultLink = (props) => {
  if (props.href.includes("http")) { // External link
    // eslint-disable-next-line
    return <a {...props} />
  } else {
    return <Link {...props} to={`/${props.href}`} />
  }
}
